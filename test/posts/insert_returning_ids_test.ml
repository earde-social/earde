(* Step-3 RETURNING-id changes: Comment_store.create_comment and
   Pending_signup_store.confirm must return the real inserted ids, with the
   failure variants unchanged. Same EARDE_TEST_DATABASE_URL opt-in gate as
   Mod_scope. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_insert_community =
  (Caqti_type.unit ->! Caqti_type.int)
  "INSERT INTO communities (slug, name) VALUES ('step3ret-c', 'step3ret-c') RETURNING id"

let q_insert_post =
  (Caqti_type.(t2 int int) ->! Caqti_type.int)
  "INSERT INTO posts (title, content, community_id, user_id)
   VALUES ('step3ret post', 'step3ret post body', $1, $2) RETURNING id"

(* Full row lookup by the returned id: proves the id is the real insert. *)
let q_comment_by_id =
  (Caqti_type.int ->? Caqti_type.(t4 string int int (option int)))
  "SELECT content, post_id, user_id, parent_id FROM comments WHERE id = $1"

let q_insert_pending =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
  "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
   VALUES ($1, $1 || '@test.invalid', 'x', $2, NOW() + INTERVAL '1 hour')"

let q_insert_expired_pending =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
  "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
   VALUES ($1, $1 || '@test.invalid', 'x', $2, NOW() - INTERVAL '1 hour')"

let q_user_id_by_name =
  (Caqti_type.string ->? Caqti_type.int)
  "SELECT id FROM users WHERE username = $1"

let comment_returning_case =
  Db_fixture.returning_ids_db_case "create_comment returns the real inserted id" (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* community = C.find q_insert_community () in
      let* community = Db_fixture.or_fail "community" community in
      let* author = C.find Db_fixture.q_insert_user "step3ret_author" in
      let* author = Db_fixture.or_fail "author" author in
      let* post = C.find q_insert_post (community, author) in
      let* post = Db_fixture.or_fail "post" post in
      let* top =
        Earde.Comment_store.create_comment conn "step3ret top comment" post author None
      in
      let top = match top with
        | Ok (`Created id) -> id
        | Ok `Invalid_parent ->
            Alcotest.fail "create_comment top: parentless insert refused"
        | Error e -> Alcotest.failf "create_comment top: %s" e
      in
      let* row = C.find_opt q_comment_by_id top in
      let* row = Db_fixture.or_fail "top row" row in
      (match row with
       | Some (content, p, u, parent) ->
           Alcotest.(check string) "top content" "step3ret top comment" content;
           Alcotest.(check int) "top post" post p;
           Alcotest.(check int) "top author" author u;
           Alcotest.(check (option int)) "top has no parent" None parent
       | None -> Alcotest.fail "returned top id matches no comment row");
      let* reply =
        Earde.Comment_store.create_comment conn "step3ret reply comment" post author
          (Some top)
      in
      let reply = match reply with
        | Ok (`Created id) -> id
        | Ok `Invalid_parent ->
            Alcotest.fail "create_comment reply: same-post parent refused"
        | Error e -> Alcotest.failf "create_comment reply: %s" e
      in
      Alcotest.(check bool) "reply id differs from top id" true (reply <> top);
      let* row = C.find_opt q_comment_by_id reply in
      let* row = Db_fixture.or_fail "reply row" row in
      (match row with
       | Some (content, _, _, parent) ->
           Alcotest.(check string) "reply content" "step3ret reply comment"
             content;
           Alcotest.(check (option int)) "reply parent is top id" (Some top)
             parent
       | None -> Alcotest.fail "returned reply id matches no comment row");
      Lwt.return_unit)

let confirm_str = function
  | Ok (`Confirmed (id, username, _, _, _)) ->
      Printf.sprintf "confirmed:%d:%s" id username
  | Ok `Invalid -> "invalid"
  | Ok `Conflict -> "conflict"
  | Error e -> "error:" ^ e

let signup_confirm_returning_case =
  Db_fixture.returning_ids_db_case "pending_signup_confirm returns the real inserted user id"
    (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* r = C.exec q_insert_pending ("step3ret_confirmed", "step3ret_tok_1") in
      let* () = Db_fixture.or_fail "insert pending" r in
      let* confirmed = Earde.Pending_signup_store.confirm conn "step3ret_tok_1" in
      let user_id, username =
        match confirmed with
        | Ok (`Confirmed (id, name, email, created_at, is_admin)) ->
            (* The confirmation's RETURNING now also carries the closed
               person properties (step 6). *)
            Alcotest.(check string) "email preserved"
              "step3ret_confirmed@test.invalid" email;
            Alcotest.(check bool) "created_at non-empty" true (created_at <> "");
            Alcotest.(check bool) "not admin" false is_admin;
            (id, name)
        | other -> Alcotest.failf "expected `Confirmed, got %s" (confirm_str other)
      in
      Alcotest.(check string) "username preserved" "step3ret_confirmed" username;
      let* looked_up = C.find_opt q_user_id_by_name "step3ret_confirmed" in
      let* looked_up = Db_fixture.or_fail "user lookup" looked_up in
      Alcotest.(check (option int)) "returned id is the real users.id"
        (Some user_id) looked_up;
      (* Replay of the same token: consumed_at excludes it -> `Invalid. *)
      let* replay = Earde.Pending_signup_store.confirm conn "step3ret_tok_1" in
      Alcotest.(check string) "replayed token invalid" "invalid"
        (confirm_str replay);
      (* Unknown token stays `Invalid. *)
      let* unknown = Earde.Pending_signup_store.confirm conn "step3ret_tok_none" in
      Alcotest.(check string) "unknown token invalid" "invalid"
        (confirm_str unknown);
      Lwt.return_unit)

let signup_confirm_failures_case =
  Db_fixture.returning_ids_db_case "expired and conflicting pendings behave as before" (fun conn c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      (* Expired token -> `Invalid, no user row created. *)
      let* r =
        C.exec q_insert_expired_pending ("step3ret_expired", "step3ret_tok_2")
      in
      let* () = Db_fixture.or_fail "insert expired pending" r in
      let* expired = Earde.Pending_signup_store.confirm conn "step3ret_tok_2" in
      Alcotest.(check string) "expired token invalid" "invalid"
        (confirm_str expired);
      let* none = C.find_opt q_user_id_by_name "step3ret_expired" in
      let* none = Db_fixture.or_fail "no expired user" none in
      Alcotest.(check (option int)) "expired pending created no user" None none;
      (* Username already in users -> `Conflict, token not consumed into a user. *)
      let* taken = C.find Db_fixture.q_insert_user "step3ret_taken" in
      let* taken_id = Db_fixture.or_fail "existing user" taken in
      let* r = C.exec q_insert_pending ("step3ret_taken", "step3ret_tok_3") in
      let* () = Db_fixture.or_fail "insert conflicting pending" r in
      let* conflict = Earde.Pending_signup_store.confirm conn "step3ret_tok_3" in
      Alcotest.(check string) "conflicting pending" "conflict"
        (confirm_str conflict);
      let* still = C.find_opt q_user_id_by_name "step3ret_taken" in
      let* still = Db_fixture.or_fail "conflict user unchanged" still in
      Alcotest.(check (option int)) "existing user row unchanged"
        (Some taken_id) still;
      Lwt.return_unit)

let suite =
  [ comment_returning_case
  ; signup_confirm_returning_case
  ; signup_confirm_failures_case
  ]

let suites =
  [ ( "db_returning_ids", suite )
  ]
