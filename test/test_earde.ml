let check_parse name expected body =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (Earde.Turnstile.parse_siteverify body))

let check_random name expected username =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (Earde.Pages.looks_random_username username))

(* "Start thread from chat" pure helpers — title/body prefill, checkbox-id parsing,
   server-side selection guard. No DB, no request. *)
module ST = Earde.Pages.Start_thread

let check_title name expected content =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (ST.derive_title content))

let check_ids name expected form =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list int64)) name expected (ST.parse_selected_ids form))

let check_norm name expected ~seed ~max_total ~valid selected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list int64)) name expected
        (ST.normalize_selection ~seed ~max_total ~valid selected))

(* Render the marker variant to a stable string so we can assert without an Alcotest
   testable for the variant. *)
let marker_str = function
  | ST.Mk_seed (pid, t) -> Printf.sprintf "seed:%d:%s" pid t
  | ST.Mk_referenced (pid, t, n) -> Printf.sprintf "ref:%d:%s:%d" pid t n
  | ST.Mk_no_link -> "none"

let check_marker name expected links =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (marker_str (ST.classify_message_links links)))

(* Reverse-navigation query-parameter parse: strict positive ints only. *)
let check_src_thread name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (option int)) name expected (ST.parse_source_thread raw))

(* data-source-highlight-ids serialization: digits and commas only. *)
let check_hl name expected ids =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (ST.highlight_ids_attr ids))

(* Compact timestamp truncations of Postgres timestamp text. *)
let check_ts name expected f raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (f raw))

(* Promoted-conversation provenance summary — a source-row constructor keeps the cases
   readable; summaries render as a stable string for a single assertion per case. *)
let sm ?(seed = false) ?(deleted = false) ~id ~author ~at content : Earde.Db.thread_source_msg =
  { Earde.Db.sm_id = Int64.of_int id; sm_author = author; sm_content = content;
    sm_created_at = at; sm_is_seed = seed; sm_deleted = deleted }

let summary_str (s : ST.source_summary) =
  Printf.sprintf "avail:%d unavail:%d parts:%d range:%s"
    s.ST.ss_available s.ST.ss_unavailable s.ST.ss_participants s.ST.ss_date_range

let check_summary name expected msgs =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (summary_str (ST.summarize_source msgs)))

(* Image src gate — pure, no DB. Local upload paths and http(s) pass; everything dangerous
   collapses to "#"; passed values are always html-escaped so they can't break the attribute. *)
let check_img name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (Earde.Components.safe_img_src raw))

(* Report enum conversions (Slice A): pure, no DB. Closed variant -> string -> variant must
   round-trip, and any off-enum string must be rejected with None. [to_s]/[of_s] are the
   per-enum helpers; polymorphic [=] compares the variant options directly. *)
let check_round_trip name to_s of_s v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (of_s (to_s v) = Some v))

let check_none name of_s s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (of_s s = None))

module D = Earde.Db

(* Community visibility / effective-indexability / read-predicate helpers (Slice B): all pure,
   no DB. Privacy is the stronger property — private is always effectively non-indexable. *)
let check_vis_round_trip name v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (D.community_visibility_of_string (D.community_visibility_to_string v) = Some v))

let check_vis_none name s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (D.community_visibility_of_string s = None))

let check_idx_community name expected vis ~community_indexable =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (D.effective_indexable_community vis ~community_indexable))

let check_idx_child name expected vis ~community_indexable ~child_indexable =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (D.effective_indexable_child vis ~community_indexable ~child_indexable))

let check_can_read name expected vis ~is_member ~is_mod ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (D.can_read_community vis ~is_member ~is_mod ~is_admin))

(* Realtime token: pure signing/claims tests, no DB and no network. The signing
   secret comes from the environment, so each case pins it explicitly. The
   endpoint handler reuses create_for_topic verbatim, so topic binding, expiry
   and signature format proven here hold for freshly refreshed tokens too. *)
module RT = Earde.Realtime_token

let with_secret secret f =
  Unix.putenv RT.secret_env secret;
  Fun.protect ~finally:(fun () -> Unix.putenv RT.secret_env "") f

let mint ?(shared_cursors = false) () =
  match
    RT.create_for_topic ~user_id:7 ~username:"alice" ~topic:"chan:8"
      ~shared_cursors
  with
  | Some token -> token
  | None -> Alcotest.fail "expected a token when the secret is set"

let claims_field token name =
  match RT.decode_payload token with
  | Some (`Assoc fields) -> List.assoc_opt name fields
  | _ -> Alcotest.fail "token payload did not decode to a JSON object"

(* Community feature gating (shared cursors): pure allow-list parsing and
   membership only — the env-reading wrapper is a trivial composition. *)
let check_slugs name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string)) name expected (Earde.Features.slugs_of_string raw))

let check_enabled name expected slugs community_slug =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Earde.Features.enabled_in ~slugs ~community_slug))

let rt_case name f = Alcotest.test_case name `Quick f

(* Chat composer JSON contract (Chat_api): pure negotiation/validation/shape
   helpers behind POST /messages. The form path (wants_json = false) keeps the
   legacy redirect/HTML contract, which is what the "browser accept" cases pin. *)
module CA = Earde.Handlers.Chat_api

let check_wants_json name expected accept =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (CA.wants_json accept))

let content_result_str = function
  | Ok content -> "ok:" ^ content
  | Error `Empty -> "empty"
  | Error `Too_long -> "too_long"

let check_content name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (content_result_str (CA.validate_content raw)))

let check_error_json name expected ~code ~message =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (CA.error_json ~code ~message))

(* JSON success shape: the composer response is the same canonical row as a
   catch-up entry / realtime new_msg payload, minute-precision timestamp
   included. Built from a plain Db.chat_message record — no DB. *)
let chat_row ?(deleted = false) ~id ~content ~created_at () : Earde.Db.chat_message =
  { Earde.Db.id = Int64.of_int id; channel_id = 14; user_id = Some 7;
    content; created_at; edited_at = None;
    deleted_at = (if deleted then Some created_at else None) }

let check_msg_json name expected ?thread_id row author =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (Yojson.Safe.to_string
           (Earde.Handlers.chat_message_json ~channel_id:14 ~community_id:24
              ?thread_id (row, author))))

(* The shared_cursors claim: derived from the community capability at mint
   time. The refresh endpoint calls the same create_for_topic with a freshly
   recomputed Features value, so these cover refreshed tokens too. *)
let rt_capability_claim =
  rt_case "shared_cursors claim mirrors the community capability" (fun () ->
      with_secret "test-secret" (fun () ->
          let claim_for community_slug =
            let shared_cursors =
              Earde.Features.enabled_in ~slugs:[ "beryl" ] ~community_slug
            in
            let token =
              match
                RT.create_for_topic ~user_id:7 ~username:"alice"
                  ~topic:"chan:8" ~shared_cursors
              with
              | Some token -> token
              | None -> Alcotest.fail "expected a token"
            in
            claims_field token "shared_cursors"
          in
          Alcotest.(check bool) "enabled community mints true" true
            (claim_for "beryl" = Some (`Bool true));
          Alcotest.(check bool) "ordinary community mints false" true
            (claim_for "earde" = Some (`Bool false))))

let rt_capability_recomputed =
  rt_case "each mint recomputes the capability it is given" (fun () ->
      with_secret "test-secret" (fun () ->
          (* Same identity/topic, different capability inputs — as when a
             refresh happens after a community's capability changed. *)
          Alcotest.(check bool) "true stays true" true
            (claims_field (mint ~shared_cursors:true ()) "shared_cursors"
            = Some (`Bool true));
          Alcotest.(check bool) "false stays false" true
            (claims_field (mint ~shared_cursors:false ()) "shared_cursors"
            = Some (`Bool false))))

let rt_no_secret =
  rt_case "no secret => no token" (fun () ->
      Unix.putenv RT.secret_env "";
      Alcotest.(check bool) "None without secret" true
        (RT.create_for_topic ~user_id:7 ~username:"alice" ~topic:"chan:8"
           ~shared_cursors:false
        = None))

let rt_format =
  rt_case "token is base64url payload dot signature" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              let is_b64url s =
                s <> ""
                && String.for_all
                     (function
                       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true
                       | _ -> false)
                     s
              in
              Alcotest.(check bool) "payload base64url" true (is_b64url payload);
              Alcotest.(check bool) "signature base64url" true (is_b64url signature)
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_topic_binding =
  rt_case "claims carry the exact topic, identity and version" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          Alcotest.(check bool) "topic" true
            (claims_field token "topic" = Some (`String "chan:8"));
          Alcotest.(check bool) "user_id" true
            (claims_field token "user_id" = Some (`Int 7));
          Alcotest.(check bool) "username" true
            (claims_field token "username" = Some (`String "alice"));
          Alcotest.(check bool) "version" true
            (claims_field token "v" = Some (`Int 1))))

let rt_expiry =
  rt_case "expiry is now + default ttl" (fun () ->
      with_secret "test-secret" (fun () ->
          let before = RT.unix_now () in
          let token = mint () in
          let after = RT.unix_now () in
          match claims_field token "exp" with
          | Some (`Int exp) ->
              Alcotest.(check bool) "exp lower bound" true
                (exp >= before + RT.default_ttl_seconds);
              Alcotest.(check bool) "exp upper bound" true
                (exp <= after + RT.default_ttl_seconds)
          | _ -> Alcotest.fail "exp claim missing or not an int"))

let rt_signature =
  rt_case "signature is HMAC-SHA256 of the payload part" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              Alcotest.(check string) "recomputed signature matches"
                (RT.hmac_sha256_base64url ~secret:"test-secret" payload)
                signature
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_tamper =
  rt_case "tampered payload no longer matches the signature" (fun () ->
      with_secret "test-secret" (fun () ->
          let token = mint () in
          match String.split_on_char '.' token with
          | [ payload; signature ] ->
              let tampered = payload ^ "A" in
              Alcotest.(check bool) "signature differs" true
                (RT.hmac_sha256_base64url ~secret:"test-secret" tampered
                <> signature)
          | _ -> Alcotest.fail "expected exactly one dot"))

let rt_decode_garbage =
  rt_case "decode_payload rejects garbage" (fun () ->
      Alcotest.(check bool) "no dot" true (RT.decode_payload "nodot" = None);
      Alcotest.(check bool) "two dots" true (RT.decode_payload "a.b.c" = None);
      Alcotest.(check bool) "non-json payload" true
        (RT.decode_payload "!!!.sig" = None))

(* /delete-comment authorization matrix — pure. The decision function takes no
   community id at all: the old handler trusted a hidden community_id form field
   for its moderator check, which is exactly what allowed a moderator of
   community A to delete community B's comment. Only the session role and the
   server-resolved owner may matter, so a forged community field cannot affect
   authorization by construction. *)
module CD = Earde.Handlers.Comment_delete

let cd_str = function
  | CD.Admin_delete -> "admin_delete"
  | CD.Author_delete -> "author_delete"
  | CD.Forbidden -> "forbidden"

let check_cd name expected ~is_admin ~requester_id ~owner_id =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (cd_str (CD.decide ~is_admin ~requester_id ~owner_id)))

(* === Cross-community moderation scoping (security regression) ===
   A moderator of community A must not be able to tombstone content in community
   B by forging the numeric id on an A-scoped route. The enforcement lives in the
   SQL of Db.mod_delete_post / Db.mod_delete_comment (id AND community scope,
   RETURNING as match proof), so only a DB-backed test can catch a regression —
   a pure helper test cannot see a missing `AND community_id`.

   Opt-in: set EARDE_TEST_DATABASE_URL to a scratch/dev Postgres to run these;
   without it each case skips, keeping the default `dune test` DB-free. Fixture
   rows use fixed modscope_* names and are removed before and after each case, so
   reruns are idempotent even after a crash. *)
module Mod_scope = struct
  let ( let* ) = Lwt.bind

  open Caqti_request.Infix

  (* Fixtures — fixed names so cleanup is targeted and idempotent. One request
     per statement: postgres prepared statements reject multi-command strings. *)
  let q_cleanup =
    List.map
      (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
      [ "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE title LIKE 'modscope %')"
      ; "DELETE FROM posts WHERE title LIKE 'modscope %'"
      ; "DELETE FROM communities WHERE slug IN ('modscope-a', 'modscope-b')"
      ; "DELETE FROM users WHERE username IN ('modscope_author', 'modscope_other')"
      ]

  let q_insert_user =
    (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ('modscope_author', 'modscope_author@test.invalid', 'x', TRUE) RETURNING id"

  (* A second, non-author user: stands in for a moderator of community A
     attacking through the general /delete-comment path. *)
  let q_insert_other_user =
    (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ('modscope_other', 'modscope_other@test.invalid', 'x', TRUE) RETURNING id"

  let q_insert_community =
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO communities (slug, name) VALUES ($1, $1) RETURNING id"

  let q_insert_post =
    (Caqti_type.(t4 string (option string) int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, image_url, community_id, user_id)
     VALUES ($1, 'modscope original post', $2, $3, $4) RETURNING id"

  let q_insert_comment =
    (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id)
     VALUES ('modscope original comment', $1, $2) RETURNING id"

  let q_post_state =
    (Caqti_type.int ->! Caqti_type.(t2 (option string) (option string)))
    "SELECT content, image_url FROM posts WHERE id = $1"

  let q_comment_content =
    (Caqti_type.int ->! Caqti_type.string)
    "SELECT content FROM comments WHERE id = $1"

  let or_fail label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

  (* Each case gets a fresh connection and a clean fixture slate; cleanup runs
     again afterwards even when an assertion fails mid-way. *)
  let db_case name f =
    Alcotest.test_case name `Quick (fun () ->
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | None | Some "" -> Alcotest.skip ()
        | Some url ->
            Lwt_main.run
              (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
               let* conn = or_fail "connect" conn in
               let (module C : Caqti_lwt.CONNECTION) = conn in
               let cleanup () =
                 Lwt_list.iter_s
                   (fun q ->
                     let* r = C.exec q () in
                     let* _ = or_fail "cleanup" r in
                     Lwt.return_unit)
                   q_cleanup
               in
               let* () = cleanup () in
               Lwt.finalize (fun () -> f conn (module C : Caqti_lwt.CONNECTION)) cleanup))

  (* Two communities, one author, a post (with image) in each. *)
  let setup_posts (module C : Caqti_lwt.CONNECTION) =
    let* a = C.find q_insert_community "modscope-a" in
    let* a = or_fail "community a" a in
    let* b = C.find q_insert_community "modscope-b" in
    let* b = or_fail "community b" b in
    let* author = C.find q_insert_user () in
    let* author = or_fail "author" author in
    let* post_a = C.find q_insert_post ("modscope post a", Some "/static/uploads/modscope_a.webp", a, author) in
    let* post_a = or_fail "post a" post_a in
    let* post_b = C.find q_insert_post ("modscope post b", Some "/static/uploads/modscope_b.webp", b, author) in
    let* post_b = or_fail "post b" post_b in
    Lwt.return (a, b, author, post_a, post_b)

  let check_post_state (module C : Caqti_lwt.CONNECTION) label post_id expected =
    let* st = C.find q_post_state post_id in
    let* st = or_fail label st in
    Alcotest.(check (pair (option string) (option string))) label expected st;
    Lwt.return_unit

  (* Posts: forging a B post id on an A-scoped delete must not match — the same
     DB call serves moderators and admins on community routes, so this also pins
     the admin-on-wrong-route rejection. A genuine A deletion still works and
     reports the match that entitles the handler to exactly one modlog entry. *)
  let posts_case =
    db_case "cross-community post delete refused; local delete works" (fun conn c ->
        let* (a, _b, _author, post_a, post_b) = setup_posts c in
        let* cross = Earde.Db.mod_delete_post conn ~community_id:a post_b in
        Alcotest.(check (result bool string)) "A-scoped delete of B post matches nothing" (Ok false) cross;
        let* () = check_post_state c "B post untouched, image_url intact" post_b
            (Some "modscope original post", Some "/static/uploads/modscope_b.webp") in
        let* local = Earde.Db.mod_delete_post conn ~community_id:a post_a in
        Alcotest.(check (result bool string)) "A-scoped delete of A post matches" (Ok true) local;
        check_post_state c "A post tombstoned, image_url cleared" post_a
          (Some "[removed by moderator]", None))

  (* Comments: ownership is comment -> post -> community; the scoped UPDATE joins
     posts, so a comment under a B post must not match on an A-scoped call. *)
  let comments_case =
    db_case "cross-community comment delete refused; local delete works" (fun conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* (a, _b, author, post_a, post_b) = setup_posts c in
        let* comment_a = C.find q_insert_comment (post_a, author) in
        let* comment_a = or_fail "comment a" comment_a in
        let* comment_b = C.find q_insert_comment (post_b, author) in
        let* comment_b = or_fail "comment b" comment_b in
        let* cross = Earde.Db.mod_delete_comment conn ~community_id:a comment_b in
        Alcotest.(check (result (option int) string)) "A-scoped delete of B comment matches nothing" (Ok None) cross;
        let* content = C.find q_comment_content comment_b in
        let* content = or_fail "B comment state" content in
        Alcotest.(check string) "B comment untouched" "modscope original comment" content;
        let* local = Earde.Db.mod_delete_comment conn ~community_id:a comment_a in
        Alcotest.(check (result (option int) string)) "A-scoped delete of A comment matches, returns parent post"
          (Ok (Some post_a)) local;
        let* content = C.find q_comment_content comment_a in
        let* content = or_fail "A comment state" content in
        Alcotest.(check string) "A comment tombstoned" "[removed by moderator]" content;
        Lwt.return_unit)

  (* /delete-comment data path: the general endpoint is author-only for
     non-admins, and its SQL is ownership-scoped (id AND user_id) — so a
     moderator of community A attacking B's comment reaches (at most)
     soft_delete_comment with a requester id that is not the owner, which must
     match nothing regardless of any community value the client sends. The
     admin path resolves the target server-side and tombstones with the admin
     label. *)
  let delete_comment_case =
    db_case "delete-comment: ownership-scoped soft delete; admin path tombstones" (fun conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* (_a, _b, author, post_a, post_b) = setup_posts c in
        let* other = C.find q_insert_other_user () in
        let* other = or_fail "other user" other in
        let* comment_b = C.find q_insert_comment (post_b, author) in
        let* comment_b = or_fail "comment b" comment_b in
        (* Non-author (e.g. a mod of A) against B's comment: zero rows match.
           soft_delete_comment reports Ok () either way — the ownership scope in
           the SQL is what this pins down. *)
        let* r = Earde.Db.soft_delete_comment conn comment_b other in
        Alcotest.(check (result unit string)) "non-author soft delete does not error" (Ok ()) r;
        let* content = C.find q_comment_content comment_b in
        let* content = or_fail "B comment state" content in
        Alcotest.(check string) "non-author soft delete matches nothing" "modscope original comment" content;
        (* The author still deletes their own comment. *)
        let* comment_a = C.find q_insert_comment (post_a, author) in
        let* comment_a = or_fail "comment a" comment_a in
        let* r = Earde.Db.soft_delete_comment conn comment_a author in
        Alcotest.(check (result unit string)) "author soft delete ok" (Ok ()) r;
        let* content = C.find q_comment_content comment_a in
        let* content = or_fail "A comment state" content in
        Alcotest.(check string) "author soft delete tombstones" "[deleted]" content;
        (* Global admin: deletes regardless of author, with the admin label. *)
        let* r = Earde.Db.admin_delete_comment conn ~label:"[removed by admin]" comment_b in
        Alcotest.(check (result unit string)) "admin delete ok" (Ok ()) r;
        let* content = C.find q_comment_content comment_b in
        let* content = or_fail "B comment after admin" content in
        Alcotest.(check string) "admin delete tombstones with admin label" "[removed by admin]" content;
        Lwt.return_unit)

  let suite = [ posts_case; comments_case; delete_comment_case ]
end

(* Step-3 RETURNING-id changes: Db.create_comment and
   Db.pending_signup_confirm must return the real inserted ids, with the
   failure variants unchanged. Same EARDE_TEST_DATABASE_URL opt-in gate as
   Mod_scope. *)
module Returning_ids = struct
  let ( let* ) = Lwt.bind

  open Caqti_request.Infix

  let q_cleanup =
    List.map
      (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
      [ "DELETE FROM comments WHERE content LIKE 'step3ret %'"
      ; "DELETE FROM posts WHERE title LIKE 'step3ret %'"
      ; "DELETE FROM communities WHERE slug = 'step3ret-c'"
      ; "DELETE FROM users WHERE username IN ('step3ret_author', 'step3ret_confirmed', 'step3ret_taken')"
      ; "DELETE FROM pending_signups WHERE username IN ('step3ret_confirmed', 'step3ret_taken', 'step3ret_expired')"
      ]

  let q_insert_user =
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

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

  let or_fail label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

  let db_case name f =
    Alcotest.test_case name `Quick (fun () ->
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | None | Some "" -> Alcotest.skip ()
        | Some url ->
            Lwt_main.run
              (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
               let* conn = or_fail "connect" conn in
               let (module C : Caqti_lwt.CONNECTION) = conn in
               let cleanup () =
                 Lwt_list.iter_s
                   (fun q ->
                     let* r = C.exec q () in
                     let* _ = or_fail "cleanup" r in
                     Lwt.return_unit)
                   q_cleanup
               in
               let* () = cleanup () in
               Lwt.finalize (fun () -> f conn (module C : Caqti_lwt.CONNECTION)) cleanup))

  let comment_returning_case =
    db_case "create_comment returns the real inserted id" (fun conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* community = C.find q_insert_community () in
        let* community = or_fail "community" community in
        let* author = C.find q_insert_user "step3ret_author" in
        let* author = or_fail "author" author in
        let* post = C.find q_insert_post (community, author) in
        let* post = or_fail "post" post in
        let* top =
          Earde.Db.create_comment conn "step3ret top comment" post author None
        in
        let top = match top with
          | Ok id -> id
          | Error e -> Alcotest.failf "create_comment top: %s" e
        in
        let* row = C.find_opt q_comment_by_id top in
        let* row = or_fail "top row" row in
        (match row with
         | Some (content, p, u, parent) ->
             Alcotest.(check string) "top content" "step3ret top comment" content;
             Alcotest.(check int) "top post" post p;
             Alcotest.(check int) "top author" author u;
             Alcotest.(check (option int)) "top has no parent" None parent
         | None -> Alcotest.fail "returned top id matches no comment row");
        let* reply =
          Earde.Db.create_comment conn "step3ret reply comment" post author
            (Some top)
        in
        let reply = match reply with
          | Ok id -> id
          | Error e -> Alcotest.failf "create_comment reply: %s" e
        in
        Alcotest.(check bool) "reply id differs from top id" true (reply <> top);
        let* row = C.find_opt q_comment_by_id reply in
        let* row = or_fail "reply row" row in
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
    db_case "pending_signup_confirm returns the real inserted user id"
      (fun conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* r = C.exec q_insert_pending ("step3ret_confirmed", "step3ret_tok_1") in
        let* () = or_fail "insert pending" r in
        let* confirmed = Earde.Db.pending_signup_confirm conn "step3ret_tok_1" in
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
        let* looked_up = or_fail "user lookup" looked_up in
        Alcotest.(check (option int)) "returned id is the real users.id"
          (Some user_id) looked_up;
        (* Replay of the same token: consumed_at excludes it -> `Invalid. *)
        let* replay = Earde.Db.pending_signup_confirm conn "step3ret_tok_1" in
        Alcotest.(check string) "replayed token invalid" "invalid"
          (confirm_str replay);
        (* Unknown token stays `Invalid. *)
        let* unknown = Earde.Db.pending_signup_confirm conn "step3ret_tok_none" in
        Alcotest.(check string) "unknown token invalid" "invalid"
          (confirm_str unknown);
        Lwt.return_unit)

  let signup_confirm_failures_case =
    db_case "expired and conflicting pendings behave as before" (fun conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        (* Expired token -> `Invalid, no user row created. *)
        let* r =
          C.exec q_insert_expired_pending ("step3ret_expired", "step3ret_tok_2")
        in
        let* () = or_fail "insert expired pending" r in
        let* expired = Earde.Db.pending_signup_confirm conn "step3ret_tok_2" in
        Alcotest.(check string) "expired token invalid" "invalid"
          (confirm_str expired);
        let* none = C.find_opt q_user_id_by_name "step3ret_expired" in
        let* none = or_fail "no expired user" none in
        Alcotest.(check (option int)) "expired pending created no user" None none;
        (* Username already in users -> `Conflict, token not consumed into a user. *)
        let* taken = C.find q_insert_user "step3ret_taken" in
        let* taken_id = or_fail "existing user" taken in
        let* r = C.exec q_insert_pending ("step3ret_taken", "step3ret_tok_3") in
        let* () = or_fail "insert conflicting pending" r in
        let* conflict = Earde.Db.pending_signup_confirm conn "step3ret_tok_3" in
        Alcotest.(check string) "conflicting pending" "conflict"
          (confirm_str conflict);
        let* still = C.find_opt q_user_id_by_name "step3ret_taken" in
        let* still = or_fail "conflict user unchanged" still in
        Alcotest.(check (option int)) "existing user row unchanged"
          (Some taken_id) still;
        Lwt.return_unit)

  let suite =
    [ comment_returning_case
    ; signup_confirm_returning_case
    ; signup_confirm_failures_case
    ]
end

(* PostHog analytics module (lib/analytics.ml): pure payload builders, exact
   consent-cookie parsing, and the consent gate exercised through the test
   capture sink. No network, no DB, no real PostHog project — the sink
   replaces the HTTP transport entirely. *)
module An = Earde.Analytics
module AnT = Earde.Analytics.For_testing

let yojson =
  Alcotest.testable
    (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
    Yojson.Safe.equal

let an_case name f = Alcotest.test_case name `Quick f
let payload_member name = function `Assoc l -> List.assoc_opt name l | _ -> None

let payload_props payload =
  match payload_member "properties" payload with
  | Some (`Assoc l) -> l
  | _ -> []

let prop_keys payload = List.map fst (payload_props payload)

let consent_str = function
  | `Granted -> "granted"
  | `Denied -> "denied"
  | `Unknown -> "unknown"

let check_consent name expected header =
  an_case name (fun () ->
      Alcotest.(check string)
        name expected
        (consent_str (AnT.consent_of_cookie_header header)))

let check_distinct name expected id =
  an_case name (fun () ->
      Alcotest.(check string) name expected (An.distinct_id_of_user_id id))

(* Runs f with a test configuration and a collecting sink; always restores the
   module's global state, and returns the captured payloads in order. *)
let with_sink ~enabled f =
  let captured = ref [] in
  (if enabled then AnT.use_enabled_test_configuration ()
   else AnT.use_disabled_test_configuration ());
  AnT.set_capture_sink (fun p -> captured := p :: !captured);
  Fun.protect
    ~finally:(fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ())
    f;
  List.rev !captured

let consent_request = function
  | None -> Dream.request ""
  | Some cookie -> Dream.request ~headers:[ ("Cookie", cookie) ] ""

let an_person =
  { An.username = "alice"; email = "alice@example.com";
    signup_date = "2026-01-01T00:00:00Z"; is_admin = false }

let an_login = An.Login_succeeded { user_id = 7; person = an_person }

(* The exact closed $set object an_person must serialize to. *)
let an_person_set : Yojson.Safe.t =
  `Assoc
    [ ("username", `String "alice")
    ; ("email", `String "alice@example.com")
    ; ("signup_date", `String "2026-01-01T00:00:00Z")
    ; ("is_admin", `Bool false)
    ]

let check_gate name expected_count cookie =
  an_case name (fun () ->
      let captured =
        with_sink ~enabled:true (fun () ->
            An.capture_if_consented (consent_request cookie)
              ~distinct_id:"user:7" an_login)
      in
      Alcotest.(check int) name expected_count (List.length captured))

(* One instance of every event constructor, used for payload/allowlist tests. *)
let an_all_events =
  [
    ("signup_confirmed", An.Signup_confirmed { user_id = 1; person = an_person });
    ("login_succeeded", An.Login_succeeded { user_id = 1; person = an_person });
    ( "community_joined",
      An.Community_joined
        { user_id = 1; community_id = 7; community_slug = "ocaml";
          community_visibility = "public" } );
    ("community_left", An.Community_left { user_id = 1; community_id = 7 });
    ( "chat_message_sent",
      An.Chat_message_sent
        { user_id = 1; community_id = 7; community_slug = "ocaml";
          channel_id = 3; channel_slug = "general"; message_id = 91L;
          content_length = 42; response_mode = An.Response_json } );
    ( "post_created",
      An.Post_created
        { user_id = 1; community_id = 7; section_id = Some 2; post_id = 10;
          content_length = 100; has_link = true; has_mention = false } );
    ( "comment_created",
      An.Comment_created
        { user_id = 1; community_id = 7; post_id = 10; comment_id = 55;
          parent_comment_id = None; content_length = 9; has_mention = true } );
    ( "thread_promoted",
      An.Thread_promoted
        { user_id = 1; community_id = 7; community_slug = "ocaml";
          channel_id = 3; channel_slug = "general"; section_id = None;
          post_id = 11; message_id = 91L; promoted_message_count = 4;
          promoted_participant_count = Some 2 } );
    ("account_deleted", An.Account_deleted { user_id = 1 });
  ]

let an_payload event =
  AnT.event_payload ~api_key:"phc_test" ~distinct_id:"user:1" event

let check_keys name event expected =
  an_case name (fun () ->
      Alcotest.(check (slist string compare))
        name expected
        (prop_keys (an_payload event)))

let check_event_name name expected event =
  an_case name (fun () ->
      match payload_member "event" (an_payload event) with
      | Some (`String n) -> Alcotest.(check string) name expected n
      | _ -> Alcotest.fail "payload has no event name")

let an_group_key payload =
  match List.assoc_opt "$groups" (payload_props payload) with
  | Some (`Assoc [ ("community", `String key) ]) -> Some key
  | _ -> None

let check_group name expected event =
  an_case name (fun () ->
      Alcotest.(check (option string))
        name expected
        (an_group_key (an_payload event)))

(* --- Step-4: consent endpoint, cookie contract, browser config ----------- *)

let contains haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  if nl = 0 then true
  else
    let rec loop i =
      if i > hl - nl then false
      else if String.sub haystack i nl = needle then true
      else loop (i + 1)
    in
    loop 0

let read_analytics_js () =
  (* dune test runs in test/, dune exec from the project root. *)
  let path =
    if Sys.file_exists "../static/js/analytics.js" then
      "../static/js/analytics.js"
    else "static/js/analytics.js"
  in
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let validate_result_str = function
  | Ok `Granted -> "granted"
  | Ok `Denied -> "denied"
  | Error (`Bad_request _) -> "bad_request"
  | Error (`Forbidden _) -> "forbidden"

(* The dummy origin baked into For_testing.use_enabled_test_configuration. *)
let test_origin = "http://earde.test"

let check_validate name expected ~content_type ~origin ~sec_fetch_site body =
  an_case name (fun () ->
      AnT.use_enabled_test_configuration ();
      Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
          Alcotest.(check string) name expected
            (validate_result_str
               (An.validate_consent_request ~content_type ~origin
                  ~sec_fetch_site ~body))))

let consent_good_headers =
  [ ("Content-Type", "application/json")
  ; ("Origin", test_origin)
  ; ("Sec-Fetch-Site", "same-origin")
  ]

(* Runs the real handler on a mock request (deliberately WITHOUT session or
   sql middleware: a first-time visitor has neither) and returns
   (status, set-cookie header, sync payloads observed by the sink). *)
let run_consent ?(headers = consent_good_headers) body =
  let payloads = ref [] in
  AnT.use_enabled_test_configuration ();
  AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
  Fun.protect
    ~finally:(fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ())
    (fun () ->
      let request =
        Dream.request ~method_:`POST ~target:"/analytics/consent" ~headers body
      in
      let response =
        Lwt_main.run (Earde.Handlers.analytics_consent_handler request)
      in
      ( Dream.status_to_int (Dream.status response),
        Dream.header response "Set-Cookie",
        List.rev !payloads ))

let check_consent_reject name expected_status ?headers body =
  an_case name (fun () ->
      let status, cookie, payloads = run_consent ?headers body in
      Alcotest.(check int) (name ^ " status") expected_status status;
      Alcotest.(check (option string)) (name ^ " no cookie") None cookie;
      Alcotest.(check int) (name ^ " no sync") 0 (List.length payloads))

(* Gated DB case: a real user row + sql_pool + memory sessions, so a granted
   authenticated request performs exactly one closed person-property sync. *)
let consent_sync_db_case =
  Returning_ids.db_case "granted consent syncs person props once (authed)"
    (fun _conn c ->
      let ( let* ) = Lwt.bind in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* author = C.find Returning_ids.q_insert_user "step3ret_author" in
      let* author = Returning_ids.or_fail "author" author in
      let url =
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | Some u -> u
        | None -> Alcotest.fail "gate env var vanished"
      in
      let payloads = ref [] in
      AnT.use_enabled_test_configuration ();
      AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
      Lwt.finalize
        (fun () ->
          let pipeline =
            Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
            let* () =
              Dream.set_session_field req "user_id" (string_of_int author)
            in
            Earde.Handlers.analytics_consent_handler req
          in
          let request =
            Dream.request ~method_:`POST ~target:"/analytics/consent"
              ~headers:consent_good_headers {|{"state":"granted"}|}
          in
          let* response = pipeline request in
          Alcotest.(check int) "status" 204
            (Dream.status_to_int (Dream.status response));
          (match !payloads with
           | [ payload ] ->
               (match payload_member "event" payload with
                | Some (`String e) ->
                    Alcotest.(check string) "event" "$identify" e
                | _ -> Alcotest.fail "sync payload has no event");
               (match payload_member "distinct_id" payload with
                | Some (`String d) ->
                    Alcotest.(check string) "distinct id"
                      ("user:" ^ string_of_int author)
                      d
                | _ -> Alcotest.fail "sync payload has no distinct_id");
               (match List.assoc_opt "$set" (payload_props payload) with
                | Some (`Assoc set) ->
                    Alcotest.(check (option string)) "username"
                      (Some "step3ret_author")
                      (match List.assoc_opt "username" set with
                       | Some (`String u) -> Some u
                       | _ -> None);
                    Alcotest.(check bool) "email present" true
                      (List.mem_assoc "email" set)
                | _ -> Alcotest.fail "sync payload has no $set")
           | l ->
               Alcotest.failf "expected exactly 1 sync, got %d" (List.length l));
          Lwt.return_unit)
        (fun () ->
          AnT.clear_capture_sink ();
          AnT.clear_configuration_override ();
          Lwt.return_unit))

(* --- Step-6: consent-gated $groupidentify API + domain-event wiring ------- *)

let an_group ?created_at () : An.community_group =
  { An.community_id = 7; community_slug = "ocaml"; community_name = "OCaml";
    community_visibility = "public"; created_at }

let run_group_identify ~enabled cookie =
  with_sink ~enabled (fun () ->
      An.identify_community_if_consented (consent_request cookie)
        ~distinct_id:"user:7" (an_group ()))

let check_group_identify_gate name expected ~enabled cookie =
  an_case name (fun () ->
      Alcotest.(check int)
        name expected
        (List.length (run_group_identify ~enabled cookie)))

let event_of payload =
  match payload_member "event" payload with
  | Some (`String e) -> e
  | _ -> "<no event>"

let distinct_of payload =
  match payload_member "distinct_id" payload with
  | Some (`String d) -> d
  | _ -> "<no distinct_id>"

let group_set_of payload =
  match List.assoc_opt "$group_set" (payload_props payload) with
  | Some (`Assoc set) -> set
  | _ -> []

let group_key_prop_of payload =
  match List.assoc_opt "$group_key" (payload_props payload) with
  | Some (`String k) -> Some k
  | _ -> None

let is_redirect status = status / 100 = 3

(* Real handlers over a real DB (EARDE_TEST_DATABASE_URL gate): each case runs
   the actual Dream handler behind sql_pool + memory_sessions with a valid
   CSRF token injected into the body, asserting the events the success path
   emits through the sink and the silence of every failure path. *)
module Step6_events = struct
  let ( let* ) = Lwt.bind

  open Caqti_request.Infix

  let q_cleanup =
    List.map
      (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
      [ "DELETE FROM thread_source_messages WHERE post_id IN (SELECT id FROM posts WHERE title LIKE 'step6 %')"
      ; "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'step6_%')"
      ; "DELETE FROM comments WHERE content LIKE 'step6 %'"
      ; "DELETE FROM posts WHERE title LIKE 'step6 %'"
      ; "DELETE FROM chat_messages WHERE content LIKE 'step6 %'"
      ; "DELETE FROM community_user_stats WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'step6_%')"
      ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
      ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
      ; "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
      ; "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'step6-%')"
      ; "DELETE FROM communities WHERE slug LIKE 'step6-%'"
      ; "DELETE FROM pending_signups WHERE username LIKE 'step6_%'"
      ; "DELETE FROM users WHERE username LIKE 'step6_%'"
      ]

  let or_fail label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

  let or_fail_s label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label e

  let db_case name f =
    Alcotest.test_case name `Quick (fun () ->
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | None | Some "" -> Alcotest.skip ()
        | Some url ->
            Lwt_main.run
              (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
               let* conn = or_fail "connect" conn in
               let (module C : Caqti_lwt.CONNECTION) = conn in
               let cleanup () =
                 Lwt_list.iter_s
                   (fun q ->
                     let* r = C.exec q () in
                     let* _ = or_fail "cleanup" r in
                     Lwt.return_unit)
                   q_cleanup
               in
               let* () = cleanup () in
               Lwt.finalize
                 (fun () -> f ~url conn (module C : Caqti_lwt.CONNECTION))
                 cleanup))

  let q_insert_user =
    (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ($1, $1 || '@test.invalid', $2, TRUE) RETURNING id"

  let q_insert_community =
    (Caqti_type.(t3 string bool string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, sections_enabled, visibility)
     VALUES ($1, $1, $2, $3) RETURNING id"

  let q_insert_post =
    (Caqti_type.(t2 int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)
     VALUES ('step6 post', 'step6 post body', $1, $2) RETURNING id"

  let q_insert_pending =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "INSERT INTO pending_signups (username, email, password_hash, token_hash, expires_at)
     VALUES ($1, $1 || '@test.invalid', 'x', $2, NOW() + INTERVAL '1 hour')"

  let q_comment_content =
    (Caqti_type.int ->? Caqti_type.string)
    "SELECT content FROM comments WHERE id = $1"

  let q_username_by_id =
    (Caqti_type.int ->? Caqti_type.string)
    "SELECT username FROM users WHERE id = $1"

  let q_community_id_by_slug =
    (Caqti_type.string ->? Caqti_type.int)
    "SELECT id FROM communities WHERE slug = $1"

  let q_visibility_by_id =
    (Caqti_type.int ->? Caqti_type.string)
    "SELECT visibility FROM communities WHERE id = $1"

  let q_delete_user =
    (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

  let form_body fields =
    String.concat "&"
      (List.map
         (fun (k, v) ->
           Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
         fields)

  let multipart_boundary = "step6boundary"

  let multipart_body fields =
    String.concat ""
      (List.map
         (fun (k, v) ->
           Printf.sprintf
             "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
             multipart_boundary k v)
         fields)
    ^ Printf.sprintf "--%s--\r\n" multipart_boundary

  let consent_header = function
    | Some v -> [ ("Cookie", An.consent_cookie_name ^ "=" ^ v) ]
    | None -> []

  (* POST runner: presets session fields, injects a valid dream.csrf into the
     (urlencoded or multipart) body, and returns (status, sink payloads). *)
  let run_handler ~url ?(consent = Some "granted") ?(session = [])
      ?(multipart = false) ?accept ~target ~form handler =
    let payloads = ref [] in
    AnT.use_enabled_test_configuration ();
    AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
    Lwt.finalize
      (fun () ->
        let pipeline =
          Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
          let* () =
            Lwt_list.iter_s
              (fun (k, v) -> Dream.set_session_field req k v)
              session
          in
          let csrf = Dream.csrf_token req in
          let fields = ("dream.csrf", csrf) :: form in
          Dream.set_body req
            (if multipart then multipart_body fields else form_body fields);
          handler req
        in
        let headers =
          [ ( "Content-Type",
              if multipart then
                "multipart/form-data; boundary=" ^ multipart_boundary
              else "application/x-www-form-urlencoded" ) ]
          @ (match accept with Some a -> [ ("Accept", a) ] | None -> [])
          @ consent_header consent
        in
        let request = Dream.request ~method_:`POST ~target ~headers "" in
        let* response = pipeline request in
        Lwt.return
          (Dream.status_to_int (Dream.status response), List.rev !payloads))
      (fun () ->
        AnT.clear_capture_sink ();
        AnT.clear_configuration_override ();
        Lwt.return_unit)

  (* GET runner (confirm-email): anonymous — no CSRF, no preset session fields
     (the session middleware itself is part of the real app pipeline). *)
  let run_get_handler ~url ?(consent = Some "granted") ~target handler =
    let payloads = ref [] in
    AnT.use_enabled_test_configuration ();
    AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
    Lwt.finalize
      (fun () ->
        let pipeline = Dream.sql_pool url @@ Dream.memory_sessions @@ handler in
        let request =
          Dream.request ~method_:`GET ~target ~headers:(consent_header consent)
            ""
        in
        let* response = pipeline request in
        Lwt.return
          (Dream.status_to_int (Dream.status response), List.rev !payloads))
      (fun () ->
        AnT.clear_capture_sink ();
        AnT.clear_configuration_override ();
        Lwt.return_unit)

  let check_set_keys name expected payload =
    match List.assoc_opt "$set" (payload_props payload) with
    | Some (`Assoc set) ->
        Alcotest.(check (slist string compare))
          name expected (List.map fst set)
    | _ -> Alcotest.failf "%s: payload has no $set" name

  let signup_case =
    db_case "signup_confirmed once with closed $set; invalid/unconsented silent"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let hash tok = Earde.Db.pending_signup_hash_token tok in
        let* r = C.exec q_insert_pending ("step6_signup", hash "step6_tok_1") in
        let* () = or_fail "pending" r in
        let* status, payloads =
          run_get_handler ~url ~target:"/confirm?token=step6_tok_1"
            Earde.Handlers.confirm_email_handler
        in
        Alcotest.(check int) "confirm status" 200 status;
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "signup_confirmed" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys" [ "user_id"; "$set" ] (prop_keys p);
             check_set_keys "closed $set"
               [ "username"; "email"; "signup_date"; "is_admin" ] p;
             (match List.assoc_opt "$set" (payload_props p) with
              | Some (`Assoc set) ->
                  Alcotest.(check (option string)) "$set username"
                    (Some "step6_signup")
                    (match List.assoc_opt "username" set with
                     | Some (`String u) -> Some u
                     | _ -> None)
              | _ -> Alcotest.fail "no $set");
             (match List.assoc_opt "user_id" (payload_props p) with
              | Some (`Int uid) ->
                  Alcotest.(check string) "distinct id"
                    ("user:" ^ string_of_int uid) (distinct_of p)
              | _ -> Alcotest.fail "no user_id prop")
         | l -> Alcotest.failf "expected 1 signup event, got %d" (List.length l));
        (* Replay: token consumed -> `Invalid -> no event. *)
        let* status, payloads =
          run_get_handler ~url ~target:"/confirm?token=step6_tok_1"
            Earde.Handlers.confirm_email_handler
        in
        Alcotest.(check int) "replay status" 200 status;
        Alcotest.(check int) "replay emits none" 0 (List.length payloads);
        (* Fresh pending confirmed WITHOUT consent: user still created, no
           event. *)
        let* r = C.exec q_insert_pending ("step6_signup2", hash "step6_tok_2") in
        let* () = or_fail "pending 2" r in
        let* status, payloads =
          run_get_handler ~url ~consent:None ~target:"/confirm?token=step6_tok_2"
            Earde.Handlers.confirm_email_handler
        in
        Alcotest.(check int) "unconsented confirm status" 200 status;
        Alcotest.(check int) "unconsented emits none" 0 (List.length payloads);
        let* created = C.find_opt q_username_by_id 0 in
        let* _ = or_fail "noop lookup" created in
        Lwt.return_unit)

  let login_case =
    db_case "login_succeeded once with closed $set; bad password silent"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* hash = Earde.Auth.hash_password "step6 password" in
        let* hash = or_fail_s "hash" hash in
        let* uid = C.find q_insert_user ("step6_login", hash) in
        let* uid = or_fail "user" uid in
        let* status, payloads =
          run_handler ~url ~target:"/login"
            ~form:
              [ ("identifier", "step6_login"); ("password", "step6 password") ]
            Earde.Handlers.login_handler
        in
        Alcotest.(check bool) "login redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "login_succeeded" (event_of p);
             Alcotest.(check string) "distinct id"
               ("user:" ^ string_of_int uid) (distinct_of p);
             Alcotest.(check (slist string compare))
               "person fields only inside $set" [ "user_id"; "$set" ]
               (prop_keys p);
             check_set_keys "closed $set"
               [ "username"; "email"; "signup_date"; "is_admin" ] p
         | l -> Alcotest.failf "expected 1 login event, got %d" (List.length l));
        let* status, payloads =
          run_handler ~url ~target:"/login"
            ~form:[ ("identifier", "step6_login"); ("password", "wrong") ]
            Earde.Handlers.login_handler
        in
        Alcotest.(check int) "failed login status" 200 status;
        Alcotest.(check int) "failed login emits none" 0 (List.length payloads);
        Lwt.return_unit)

  let join_case =
    db_case
      "join emits community_joined + $groupidentify; private/denied silent"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        ignore conn;
        let* uid = C.find q_insert_user ("step6_joiner", "x") in
        let* uid = or_fail "user" uid in
        let* pub = C.find q_insert_community ("step6-pub", true, "public") in
        let* pub = or_fail "public community" pub in
        let* priv = C.find q_insert_community ("step6-priv", true, "private") in
        let* priv = or_fail "private community" priv in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_joiner") ]
        in
        let* status, payloads =
          run_handler ~url ~session ~target:"/join"
            ~form:
              [ ("community_id", string_of_int pub); ("redirect_to", "/feed") ]
            Earde.Handlers.join_community_handler
        in
        Alcotest.(check bool) "join redirects" true (is_redirect status);
        (match payloads with
         | [ joined; gi ] ->
             Alcotest.(check string) "event" "community_joined"
               (event_of joined);
             Alcotest.(check string) "joined distinct"
               ("user:" ^ string_of_int uid) (distinct_of joined);
             Alcotest.(check (option string)) "$groups key"
               (Some ("community:" ^ string_of_int pub))
               (an_group_key joined);
             Alcotest.(check (slist string compare))
               "joined props keys"
               [ "user_id"; "community_id"; "community_slug";
                 "community_visibility"; "$groups" ]
               (prop_keys joined);
             Alcotest.(check string) "groupidentify event" "$groupidentify"
               (event_of gi);
             Alcotest.(check string) "groupidentify distinct is the USER"
               ("user:" ^ string_of_int uid) (distinct_of gi);
             Alcotest.(check (option string)) "group key"
               (Some ("community:" ^ string_of_int pub))
               (group_key_prop_of gi);
             Alcotest.(check (slist string compare))
               "closed group props (no created_at on the record)"
               [ "community_id"; "community_slug"; "community_name";
                 "community_visibility" ]
               (List.map fst (group_set_of gi))
         | l ->
             Alcotest.failf "expected joined+groupidentify, got %d payloads"
               (List.length l));
        (* Private community: same 404 as missing; nothing emitted. *)
        let* status, payloads =
          run_handler ~url ~session ~target:"/join"
            ~form:
              [ ("community_id", string_of_int priv); ("redirect_to", "/feed") ]
            Earde.Handlers.join_community_handler
        in
        Alcotest.(check int) "private join 404" 404 status;
        Alcotest.(check int) "private join emits none" 0 (List.length payloads);
        (* Denied consent: the join itself still succeeds, zero events. *)
        let* status, payloads =
          run_handler ~url ~session ~consent:(Some "denied") ~target:"/join"
            ~form:
              [ ("community_id", string_of_int pub); ("redirect_to", "/feed") ]
            Earde.Handlers.join_community_handler
        in
        Alcotest.(check bool) "denied join still redirects" true
          (is_redirect status);
        Alcotest.(check int) "denied join emits none" 0 (List.length payloads);
        Lwt.return_unit)

  let leave_case =
    db_case "leave emits community_left only when a row was really deleted"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_leaver", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-leave", true, "public") in
        let* cid = or_fail "community" cid in
        let* r = Earde.Db.join_community conn uid cid in
        let* () = or_fail_s "join fixture" r in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_leaver") ]
        in
        let leave () =
          run_handler ~url ~session ~target:"/leave"
            ~form:
              [ ("community_id", string_of_int cid); ("redirect_to", "/feed") ]
            Earde.Handlers.leave_community_handler
        in
        let* status, payloads = leave () in
        Alcotest.(check bool) "leave redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "community_left" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys" [ "user_id"; "community_id"; "$groups" ]
               (prop_keys p);
             Alcotest.(check (option string)) "$groups key"
               (Some ("community:" ^ string_of_int cid))
               (an_group_key p)
         | l -> Alcotest.failf "expected 1 leave event, got %d" (List.length l));
        (* Leaving again as a non-member: identical product response (the
           DELETE matches zero rows), but no community_left event. *)
        let* status, payloads = leave () in
        Alcotest.(check bool) "no-op leave still redirects" true
          (is_redirect status);
        Alcotest.(check int) "no-op leave emits none" 0 (List.length payloads);
        Lwt.return_unit)

  let visibility_case =
    db_case "visibility change emits one $groupidentify with the new value"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_visadmin", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-vis", true, "public") in
        let* cid = or_fail "community" cid in
        let router =
          Dream.router
            [ Dream.post "/c/:slug/settings/visibility"
                Earde.Handlers.update_community_visibility_handler
            ]
        in
        let submit ?consent ~session value =
          run_handler ~url ?consent ~session
            ~target:"/c/step6-vis/settings/visibility"
            ~form:[ ("visibility", value) ]
            router
        in
        let admin_session =
          [ ("user_id", string_of_int uid); ("username", "step6_visadmin");
            ("is_admin", "true") ]
        in
        let* status, payloads = submit ~session:admin_session "private" in
        Alcotest.(check bool) "visibility change redirects" true
          (is_redirect status);
        (match payloads with
         | [ gi ] ->
             Alcotest.(check string) "event" "$groupidentify" (event_of gi);
             Alcotest.(check string) "distinct is the acting user, not synthetic"
               ("user:" ^ string_of_int uid) (distinct_of gi);
             Alcotest.(check (option string)) "group key"
               (Some ("community:" ^ string_of_int cid))
               (group_key_prop_of gi);
             Alcotest.(check (option string)) "NEW visibility in $group_set"
               (Some "private")
               (match List.assoc_opt "community_visibility" (group_set_of gi) with
                | Some (`String v) -> Some v
                | _ -> None);
             Alcotest.(check (slist string compare))
               "closed group props only (no indexability)"
               [ "community_id"; "community_slug"; "community_name";
                 "community_visibility" ]
               (List.map fst (group_set_of gi))
         | l ->
             Alcotest.failf "expected 1 groupidentify, got %d" (List.length l));
        (* Forbidden: neither admin nor top mod -> 403, silent, value kept. *)
        let* nobody = C.find q_insert_user ("step6_visnobody", "x") in
        let* nobody = or_fail "nobody" nobody in
        let* status, payloads =
          submit
            ~session:
              [ ("user_id", string_of_int nobody);
                ("username", "step6_visnobody") ]
            "public"
        in
        Alcotest.(check int) "forbidden status" 403 status;
        Alcotest.(check int) "forbidden emits none" 0 (List.length payloads);
        (* Invalid value: validation error, silent. *)
        let* status, payloads = submit ~session:admin_session "friends-only" in
        Alcotest.(check int) "invalid value status" 400 status;
        Alcotest.(check int) "invalid value emits none" 0 (List.length payloads);
        (* Denied consent: the mutation still succeeds, zero events. *)
        let* status, payloads =
          submit ~consent:(Some "denied") ~session:admin_session "public"
        in
        Alcotest.(check bool) "denied consent still redirects" true
          (is_redirect status);
        Alcotest.(check int) "denied consent emits none" 0 (List.length payloads);
        let* stored = C.find_opt q_visibility_by_id cid in
        let* stored = or_fail "stored visibility" stored in
        Alcotest.(check (option string))
          "denied-consent mutation really applied" (Some "public") stored;
        Lwt.return_unit)

  let chat_case =
    db_case "chat_message_sent: json and redirect modes each emit exactly once"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_chatter", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-chat", true, "public") in
        let* cid = or_fail "community" cid in
        let* r = Earde.Db.join_community conn uid cid in
        let* () = or_fail_s "membership" r in
        let* chslug = Earde.Db.create_channel conn cid "general" None 0 in
        let* chslug = or_fail_s "channel" chslug in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_chatter") ]
        in
        let send ?accept content =
          run_handler ~url ~session ?accept ~target:"/send-message"
            ~form:
              [ ("community_slug", "step6-chat"); ("channel_slug", chslug);
                ("content", content) ]
            Earde.Handlers.send_message_handler
        in
        let* status, payloads = send "step6 hello redirect" in
        Alcotest.(check bool) "redirect mode redirects" true
          (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "chat_message_sent" (event_of p);
             Alcotest.(check (option string)) "response_mode"
               (Some "redirect")
               (match List.assoc_opt "response_mode" (payload_props p) with
                | Some (`String m) -> Some m
                | _ -> None);
             Alcotest.(check (option string)) "$groups key"
               (Some ("community:" ^ string_of_int cid))
               (an_group_key p);
             Alcotest.(check (slist string compare))
               "props keys"
               [ "user_id"; "community_id"; "community_slug"; "channel_id";
                 "channel_slug"; "message_id"; "content_length";
                 "response_mode"; "$groups" ]
               (prop_keys p);
             (* Length only — never the message text. *)
             Alcotest.(check (option int)) "content_length"
               (Some (String.length "step6 hello redirect"))
               (match List.assoc_opt "content_length" (payload_props p) with
                | Some (`Int n) -> Some n
                | _ -> None)
         | l -> Alcotest.failf "redirect mode: expected 1, got %d" (List.length l));
        let* status, payloads =
          send ~accept:"application/json" "step6 hello json"
        in
        Alcotest.(check int) "json mode 200" 200 status;
        (match payloads with
         | [ p ] ->
             Alcotest.(check (option string)) "response_mode json"
               (Some "json")
               (match List.assoc_opt "response_mode" (payload_props p) with
                | Some (`String m) -> Some m
                | _ -> None)
         | l -> Alcotest.failf "json mode: expected 1, got %d" (List.length l));
        Lwt.return_unit)

  let post_case =
    db_case "post_created once on success; non-member silent"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_poster", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-post", false, "public") in
        let* cid = or_fail "community" cid in
        let* r = Earde.Db.join_community conn uid cid in
        let* () = or_fail_s "membership" r in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_poster") ]
        in
        let form =
          [ ("title", "step6 post title"); ("content", "step6 body");
            ("community_id", string_of_int cid) ]
        in
        let* status, payloads =
          run_handler ~url ~session ~multipart:true ~target:"/create-post"
            ~form Earde.Handlers.create_post_handler
        in
        Alcotest.(check bool) "post redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "post_created" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys (no title/body/url)"
               [ "user_id"; "community_id"; "post_id"; "content_length";
                 "has_link"; "has_mention"; "$groups" ]
               (prop_keys p);
             Alcotest.(check (option string)) "$groups key"
               (Some ("community:" ^ string_of_int cid))
               (an_group_key p)
         | l -> Alcotest.failf "expected 1 post event, got %d" (List.length l));
        (* Non-member: refused, silent. *)
        let* other = C.find q_insert_user ("step6_stranger", "x") in
        let* other = or_fail "other" other in
        let* status, payloads =
          run_handler ~url
            ~session:
              [ ("user_id", string_of_int other);
                ("username", "step6_stranger") ]
            ~multipart:true ~target:"/create-post" ~form
            Earde.Handlers.create_post_handler
        in
        Alcotest.(check int) "non-member status" 200 status;
        Alcotest.(check int) "non-member emits none" 0 (List.length payloads);
        Lwt.return_unit)

  let comment_case =
    db_case "comment_created carries the real RETURNING comment id"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_commenter", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-comm", false, "public") in
        let* cid = or_fail "community" cid in
        let* pid = C.find q_insert_post (cid, uid) in
        let* pid = or_fail "post" pid in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_commenter") ]
        in
        let* status, payloads =
          run_handler ~url ~session ~target:"/create-comment"
            ~form:
              [ ("content", "step6 comment"); ("post_id", string_of_int pid) ]
            Earde.Handlers.create_comment_handler
        in
        Alcotest.(check bool) "comment redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "comment_created" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys (top-level comment: no parent_comment_id)"
               [ "user_id"; "community_id"; "post_id"; "comment_id";
                 "content_length"; "has_mention"; "$groups" ]
               (prop_keys p);
             (match List.assoc_opt "comment_id" (payload_props p) with
              | Some (`Int comment_id) ->
                  let* row = C.find_opt q_comment_content comment_id in
                  let* row = or_fail "comment row" row in
                  Alcotest.(check (option string))
                    "payload comment_id is the real inserted row"
                    (Some "step6 comment") row;
                  Lwt.return_unit
              | _ -> Alcotest.fail "payload has no int comment_id")
         | l ->
             Alcotest.failf "expected 1 comment event, got %d" (List.length l))
        )

  let promote_case =
    db_case "thread_promoted once with counts and group key"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_promoter", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-thr", false, "public") in
        let* cid = or_fail "community" cid in
        let* r = Earde.Db.join_community conn uid cid in
        let* () = or_fail_s "membership" r in
        let* chslug = Earde.Db.create_channel conn cid "general" None 0 in
        let* chslug = or_fail_s "channel" chslug in
        let* channel = Earde.Db.get_channel_by_slug conn chslug cid in
        let* channel = or_fail_s "channel row" channel in
        let channel =
          match channel with
          | Some ch -> ch
          | None -> Alcotest.fail "channel vanished"
        in
        let* seed = Earde.Db.send_message conn channel.Earde.Db.id uid "step6 seed" in
        let* seed = or_fail_s "seed message" seed in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_promoter") ]
        in
        let router =
          Dream.router
            [ Dream.post
                "/c/:slug/ch/:channel_slug/messages/:message_id/start-thread"
                Earde.Handlers.start_thread_create_handler
            ]
        in
        let* status, payloads =
          run_handler ~url ~session
            ~target:
              (Printf.sprintf "/c/step6-thr/ch/%s/messages/%Ld/start-thread"
                 chslug seed.Earde.Db.id)
            ~form:[ ("title", "step6 thread"); ("content", "") ]
            router
        in
        Alcotest.(check bool) "promotion redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "thread_promoted" (event_of p);
             Alcotest.(check string) "distinct"
               ("user:" ^ string_of_int uid) (distinct_of p);
             Alcotest.(check (slist string compare))
               "props keys (sectionless community)"
               [ "user_id"; "community_id"; "community_slug"; "channel_id";
                 "channel_slug"; "post_id"; "message_id";
                 "promoted_message_count"; "promoted_participant_count";
                 "$groups" ]
               (prop_keys p);
             Alcotest.(check (option string)) "$groups key"
               (Some ("community:" ^ string_of_int cid))
               (an_group_key p);
             Alcotest.(check (option int)) "seed-only message count" (Some 1)
               (match
                  List.assoc_opt "promoted_message_count" (payload_props p)
                with
                | Some (`Int n) -> Some n
                | _ -> None);
             Alcotest.(check (option int)) "participant count" (Some 1)
               (match
                  List.assoc_opt "promoted_participant_count" (payload_props p)
                with
                | Some (`Int n) -> Some n
                | _ -> None)
         | l ->
             Alcotest.failf "expected 1 promotion event, got %d" (List.length l));
        Lwt.return_unit)

  let create_community_case =
    db_case "community creation emits exactly one $groupidentify (no event)"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_founder", "x") in
        let* uid = or_fail "user" uid in
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_founder") ]
        in
        let* status, payloads =
          run_handler ~url ~session ~target:"/create-community"
            ~form:
              [ ("name", "step6-created"); ("slug", "step6-created");
                ("section_count", "0") ]
            Earde.Handlers.create_community_handler
        in
        Alcotest.(check bool) "creation redirects" true (is_redirect status);
        let* cid = C.find_opt q_community_id_by_slug "step6-created" in
        let* cid = or_fail "created community" cid in
        let cid =
          match cid with Some id -> id | None -> Alcotest.fail "no community"
        in
        (match payloads with
         | [ gi ] ->
             Alcotest.(check string) "only $groupidentify" "$groupidentify"
               (event_of gi);
             Alcotest.(check string) "distinct is the creator"
               ("user:" ^ string_of_int uid) (distinct_of gi);
             Alcotest.(check (option string)) "group key"
               (Some ("community:" ^ string_of_int cid))
               (group_key_prop_of gi)
         | l ->
             Alcotest.failf "expected exactly 1 groupidentify, got %d: %s"
               (List.length l)
               (String.concat ", " (List.map event_of l)));
        Lwt.return_unit)

  let update_settings_case =
    db_case "settings update emits $groupidentify; forbidden silent"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_moddy", "x") in
        let* uid = or_fail "user" uid in
        let* cid = C.find q_insert_community ("step6-upd", true, "public") in
        let* cid = or_fail "community" cid in
        let form =
          [ ("community_id", string_of_int cid);
            ("community_slug", "step6-upd");
            ("description", "step6 new description"); ("rules", "");
            ("avatar_url", ""); ("banner_url", "");
            ("existing_avatar_url", ""); ("existing_banner_url", "") ]
        in
        let admin_session =
          [ ("user_id", string_of_int uid); ("username", "step6_moddy");
            ("is_admin", "true") ]
        in
        let* status, payloads =
          run_handler ~url ~session:admin_session ~multipart:true
            ~target:"/update-community" ~form
            Earde.Handlers.update_community_handler
        in
        Alcotest.(check bool) "update redirects" true (is_redirect status);
        (match payloads with
         | [ gi ] ->
             Alcotest.(check string) "event" "$groupidentify" (event_of gi);
             Alcotest.(check string) "distinct is the acting admin"
               ("user:" ^ string_of_int uid) (distinct_of gi);
             Alcotest.(check (option string)) "group key"
               (Some ("community:" ^ string_of_int cid))
               (group_key_prop_of gi);
             Alcotest.(check (slist string compare))
               "closed group props"
               [ "community_id"; "community_slug"; "community_name";
                 "community_visibility" ]
               (List.map fst (group_set_of gi))
         | l ->
             Alcotest.failf "expected 1 groupidentify, got %d" (List.length l));
        (* Unauthorized (neither admin nor moderator): 403, silent. *)
        let* other = C.find q_insert_user ("step6_nobody", "x") in
        let* other = or_fail "other" other in
        let* status, payloads =
          run_handler ~url
            ~session:
              [ ("user_id", string_of_int other); ("username", "step6_nobody") ]
            ~multipart:true ~target:"/update-community" ~form
            Earde.Handlers.update_community_handler
        in
        Alcotest.(check int) "forbidden status" 403 status;
        Alcotest.(check int) "forbidden emits none" 0 (List.length payloads);
        Lwt.return_unit)

  let delete_account_case =
    db_case "account_deleted once with the pre-anonymization id"
      (fun ~url _conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_deleteme", "x") in
        let* uid = or_fail "user" uid in
        let* status, payloads =
          run_handler ~url
            ~session:
              [ ("user_id", string_of_int uid); ("username", "step6_deleteme") ]
            ~target:"/delete-account" ~form:[]
            Earde.Handlers.delete_account_handler
        in
        Alcotest.(check bool) "deletion redirects" true (is_redirect status);
        (match payloads with
         | [ p ] ->
             Alcotest.(check string) "event" "account_deleted" (event_of p);
             Alcotest.(check string) "pre-anonymization distinct id"
               ("user:" ^ string_of_int uid) (distinct_of p);
             Alcotest.(check (slist string compare))
               "user_id only — no person data" [ "user_id" ] (prop_keys p)
         | l ->
             Alcotest.failf "expected 1 deletion event, got %d" (List.length l));
        let* name = C.find_opt q_username_by_id uid in
        let* name = or_fail "anonymized row" name in
        Alcotest.(check (option string)) "row anonymized"
          (Some (Printf.sprintf "[deleted_%d]" uid))
          name;
        (* The anonymized username no longer matches the step6_ cleanup
           pattern — drop the row here. *)
        let* r = C.exec q_delete_user uid in
        let* () = or_fail "drop anonymized user" r in
        Lwt.return_unit)

  let suite =
    [ signup_case; login_case; join_case; leave_case; chat_case; post_case
    ; comment_case; promote_case; create_community_case; update_settings_case
    ; visibility_case; delete_account_case
    ]
end

(* --- Step-5: identity/group attributes + browser reconciliation ---------- *)

let index_of haystack needle =
  let hl = String.length haystack and nl = String.length needle in
  let rec loop i =
    if nl = 0 || i > hl - nl then None
    else if String.sub haystack i nl = needle then Some i
    else loop (i + 1)
  in
  loop 0

(* Extracts the single-quoted value of [name='value'] from rendered HTML. *)
let attr_value html name =
  match index_of html (name ^ "='") with
  | None -> None
  | Some i -> (
      let start = i + String.length name + 2 in
      match String.index_from_opt html start '\'' with
      | None -> None
      | Some j -> Some (String.sub html start (j - start)))

(* Counts occurrences of a substring — used to prove no unexpected
   data-analytics-* attribute sneaks in. *)
let count_sub haystack needle =
  let nl = String.length needle in
  let rec loop i acc =
    match index_of (String.sub haystack i (String.length haystack - i)) needle with
    | None -> acc
    | Some j -> loop (i + j + nl) (acc + 1)
  in
  if nl = 0 then 0 else loop 0 0

(* Renders the shared layout through real session middleware, optionally with
   a session user_id value, under the enabled test analytics config. *)
let render_layout ?session_user ?analytics_community_id () =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           Lwt.bind
             (match session_user with
             | Some v -> Dream.set_session_field req "user_id" v
             | None -> Lwt.return_unit)
             (fun () ->
               rendered :=
                 Earde.Components.layout ~request:req ?analytics_community_id
                   ~title:"T" "<p>body</p>";
               Dream.html ""))
         (Dream.request ~method_:`GET ~target:"/" ""))
  in
  !rendered

let with_enabled_config f =
  AnT.use_enabled_test_configuration ();
  Fun.protect ~finally:AnT.clear_configuration_override f

let test_community ~id ~visibility : Earde.Db.community =
  { Earde.Db.id; slug = "testc"; name = "Test Community"; description = None;
    rules = None; avatar_url = None; banner_url = None; allow_downvotes = true;
    sections_enabled = true; visibility; indexable = true }

let () =
  Alcotest.run "earde"
    [ ( "smoke"
      , [ Alcotest.test_case "true is true" `Quick (fun () ->
              Alcotest.(check bool) "same bool" true true) ] )
      (* Turnstile siteverify response parsing. Pure, no network — fail closed on
         anything that is not an explicit {"success": true}. *)
    ; ( "turnstile_parse"
      , [ check_parse "success true" true {|{"success": true}|}
        ; check_parse "success true with extras" true
            {|{"success": true, "challenge_ts": "2026-06-16T00:00:00Z", "hostname": "earde.com"}|}
        ; check_parse "success false" false
            {|{"success": false, "error-codes": ["invalid-input-response"]}|}
        ; check_parse "success missing" false {|{"hostname": "earde.com"}|}
        ; check_parse "success non-bool" false {|{"success": "true"}|}
        ; check_parse "empty object" false {|{}|}
        ; check_parse "non-object body" false {|"success"|}
        ; check_parse "malformed json" false {|not json at all|}
        ; check_parse "empty string" false ""
        ] )
      (* Suspicious-username heuristic — pure, display-only. Human-looking handles must
         not be flagged; bot-like ones should. No DB, no network. *)
    ; ( "looks_random_username"
      , [ check_random "human alice" false "alice"
        ; check_random "human damiano" false "damiano"
        ; check_random "human snake_case" false "john_doe"
        ; check_random "human with digits" false "kevin99"
        ; check_random "human long" false "mariarossi"
        ; check_random "short ignored" false "xkq"
        ; check_random "digit heavy" true "a8f3k9d2"
        ; check_random "no vowels" true "xkqjwzbf"
        ; check_random "long consonant run" true "bcdfghjk"
        ; check_random "mixed bot" true "tbvkxwlqz"
        ] )
      (* Title prefill: first sentence / newline, trimmed. *)
    ; ( "start_thread_title"
      , [ check_title "empty" "" ""
        ; check_title "first sentence" "Hello there" "Hello there. More text here."
        ; check_title "newline cut" "First line" "First line\nsecond line"
        ; check_title "no boundary" "just a phrase" "just a phrase"
        ; check_title "trimmed" "spaced" "   spaced.   "
        ] )
      (* Checkbox field parsing: msg_<id> only, deduped + sorted, bad ids dropped. *)
    ; ( "start_thread_parse_ids"
      , [ check_ids "basic" [10L; 20L] [("msg_10", "on"); ("msg_20", "on")]
        ; check_ids "ignores others" [5L] [("dream.csrf", "x"); ("title", "t"); ("msg_5", "on")]
        ; check_ids "dedup and sort" [3L; 7L] [("msg_7", "on"); ("msg_3", "on"); ("msg_7", "on")]
        ; check_ids "bad ids ignored" [] [("msg_abc", "on"); ("msg_", "on")]
        ; check_ids "none" [] [("title", "x")]
        ] )
      (* Selection guard: seed forced out, invalids dropped, chronological, capped to max-1. *)
    ; ( "start_thread_normalize"
      , [ check_norm "drops seed and sorts" [2L; 4L] ~seed:3L ~max_total:10 ~valid:[2L; 3L; 4L] [4L; 2L; 3L]
        ; check_norm "drops invalid" [2L] ~seed:3L ~max_total:10 ~valid:[2L; 3L] [2L; 9L; 100L]
        ; check_norm "caps to max-1" [1L; 2L; 3L; 4L] ~seed:99L ~max_total:5 ~valid:[1L; 2L; 3L; 4L; 5L; 6L] [6L; 5L; 4L; 3L; 2L; 1L]
        ; check_norm "dedup" [2L] ~seed:1L ~max_total:10 ~valid:[2L] [2L; 2L; 2L]
        ] )
      (* Channel-row marker: seed wins; else most-recent reference is target with count.
         (The visible copy — "Started thread →" / "Included in thread →" — lives in the
         channel renderer; classification is what's pure and covered here.) *)
    ; ( "start_thread_marker"
      , [ check_marker "no links" "none" []
        ; check_marker "seed wins over refs" "seed:7:Seed thread" [(3, "Ctx thread", false); (7, "Seed thread", true)]
        ; check_marker "single reference" "ref:5:Only ref:1" [(5, "Only ref", false)]
        ; check_marker "multi reference picks highest post id" "ref:9:Recent:3" [(4, "Old", false); (9, "Recent", false); (6, "Mid", false)]
        ] )
      (* ?source_thread= parse: strict positive int; anything else means "no focus". *)
    ; ( "source_thread_param"
      , [ check_src_thread "absent" None None
        ; check_src_thread "valid" (Some 42) (Some "42")
        ; check_src_thread "trimmed" (Some 7) (Some " 7 ")
        ; check_src_thread "zero rejected" None (Some "0")
        ; check_src_thread "negative rejected" None (Some "-3")
        ; check_src_thread "junk rejected" None (Some "abc")
        ; check_src_thread "injection rejected" None (Some "42'/><script>")
        ; check_src_thread "overflow rejected" None (Some "99999999999999999999999")
        ] )
      (* Highlight-id serialization: attribute-safe digits + commas, order preserved. *)
    ; ( "highlight_ids_attr"
      , [ check_hl "empty" "" []
        ; check_hl "single" "12" [12L]
        ; check_hl "ordered many" "3,7,20" [3L; 7L; 20L]
        ] )
      (* Timestamp truncations: date / minute prefixes; short input passes through. *)
    ; ( "source_timestamps"
      , [ check_ts "date" "2026-06-12" ST.date_of_ts "2026-06-12 10:04:56"
        ; check_ts "minute" "2026-06-12 10:04" ST.minute_of_ts "2026-06-12 10:04:56.123"
        ; check_ts "short date passthrough" "2026" ST.date_of_ts "2026"
        ; check_ts "short minute passthrough" "2026-06-12" ST.minute_of_ts "2026-06-12"
        ] )
      (* Provenance summary: counts, distinct participants, tombstone/deleted handling,
         same-day vs cross-day range. Derived from source rows only. *)
    ; ( "source_summary"
      , [ check_summary "empty" "avail:0 unavail:0 parts:0 range:"
            []
        ; check_summary "single message" "avail:1 unavail:0 parts:1 range:2026-06-12"
            [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "hi" ~seed:true ]
        ; check_summary "distinct participants, same day"
            "avail:3 unavail:0 parts:2 range:2026-06-12"
            [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
            ; sm ~id:2 ~author:"bob" ~at:"2026-06-12 10:01:00" "b"
            ; sm ~id:3 ~author:"alice" ~at:"2026-06-12 10:02:00" "c" ]
        ; check_summary "date range across days"
            "avail:2 unavail:0 parts:2 range:2026-06-12 \xe2\x80\x93 2026-06-13"
            [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 23:59:00" "a"
            ; sm ~id:2 ~author:"bob" ~at:"2026-06-13 00:01:00" "b" ]
        ; check_summary "deleted rows counted unavailable, excluded from participants"
            "avail:1 unavail:2 parts:1 range:2026-06-12"
            [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
            ; sm ~id:2 ~author:"" ~at:"2026-06-12 10:01:00" "" ~deleted:true
            ; sm ~id:3 ~author:"" ~at:"2026-06-12 10:02:00" "" ~deleted:true ]
        ; check_summary "tombstoned author available but not a participant"
            "avail:2 unavail:0 parts:1 range:2026-06-12"
            [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
            ; sm ~id:2 ~author:"" ~at:"2026-06-12 10:01:00" "ghost message" ]
        ] )
      (* Image src gate: local uploads + http(s) pass (html-escaped); javascript:/data:/
         protocol-relative/injection/empty collapse to "#". *)
    ; ( "safe_img_src"
      , [ check_img "local upload" "/static/uploads/foo.webp" "/static/uploads/foo.webp"
        ; check_img "https" "https://example.com/a.webp" "https://example.com/a.webp"
        ; check_img "http" "http://example.com/a.webp" "http://example.com/a.webp"
        ; check_img "javascript" "#" "javascript:alert(1)"
        ; check_img "data uri" "#" "data:image/svg+xml,<svg onload=alert(1)>"
        ; check_img "protocol-relative" "#" "//evil.com/x.webp"
        ; check_img "backslash protocol-relative" "#" "/\\evil.com/x.webp"
        ; check_img "attribute injection (no leading slash)" "#" "x' onerror='alert(1)"
        ; check_img "whitespace only" "#" "   "
        ; check_img "empty" "#" ""
        ; check_img "uppercase scheme rejected as non-local" "#" "JAVASCRIPT:alert(1)"
          (* Quote / whitespace anywhere in the candidate is refused outright, not escaped. *)
        ; check_img "local path with quote returns #" "#" "/static/uploads/foo' onerror='alert(1).webp"
        ; check_img "local path with whitespace returns #" "#" "/static/uploads/foo bar.webp"
        ; check_img "valid generated upload path passes"
            "/static/uploads/earde_123_456.webp" "/static/uploads/earde_123_456.webp"
        ] )
      (* Report target enum: every constructor round-trips; off-enum strings rejected. *)
    ; ( "report_target_roundtrip"
      , [ check_round_trip "post" D.report_target_to_string D.report_target_of_string D.Report_post
        ; check_round_trip "comment" D.report_target_to_string D.report_target_of_string D.Report_comment
        ; check_round_trip "chat_message" D.report_target_to_string D.report_target_of_string D.Report_chat_message
        ; check_none "empty" D.report_target_of_string ""
        ; check_none "unknown" D.report_target_of_string "user"
        ; check_none "case-sensitive" D.report_target_of_string "Post"
        ; check_none "cross-enum status" D.report_target_of_string "open"
        ] )
      (* Report reason enum. *)
    ; ( "report_reason_roundtrip"
      , [ check_round_trip "spam" D.report_reason_to_string D.report_reason_of_string D.Report_spam
        ; check_round_trip "abuse" D.report_reason_to_string D.report_reason_of_string D.Report_abuse
        ; check_round_trip "off_topic" D.report_reason_to_string D.report_reason_of_string D.Report_off_topic
        ; check_round_trip "illegal" D.report_reason_to_string D.report_reason_of_string D.Report_illegal
        ; check_round_trip "other" D.report_reason_to_string D.report_reason_of_string D.Report_other
        ; check_none "empty" D.report_reason_of_string ""
        ; check_none "unknown" D.report_reason_of_string "harassment"
        ; check_none "off-topic dash variant" D.report_reason_of_string "off-topic"
        ] )
      (* Report status enum. *)
    ; ( "report_status_roundtrip"
      , [ check_round_trip "open" D.report_status_to_string D.report_status_of_string D.Report_open
        ; check_round_trip "dismissed" D.report_status_to_string D.report_status_of_string D.Report_dismissed
        ; check_round_trip "action_taken" D.report_status_to_string D.report_status_of_string D.Report_action_taken
        ; check_none "empty" D.report_status_of_string ""
        ; check_none "unknown" D.report_status_of_string "resolved"
        ; check_none "cross-enum target" D.report_status_of_string "post"
        ] )
      (* Report action_kind enum. Report_other_action serializes to the bare "other". *)
    ; ( "report_action_kind_roundtrip"
      , [ check_round_trip "removed_content" D.report_action_kind_to_string D.report_action_kind_of_string D.Report_removed_content
        ; check_round_trip "banned_author" D.report_action_kind_to_string D.report_action_kind_of_string D.Report_banned_author
        ; check_round_trip "other" D.report_action_kind_to_string D.report_action_kind_of_string D.Report_other_action
        ; check_none "empty" D.report_action_kind_of_string ""
        ; check_none "unknown" D.report_action_kind_of_string "deleted"
        ; check_none "removed variant" D.report_action_kind_of_string "removed"
        ] )
      (* Community visibility enum: round-trips; off-enum strings rejected. *)
    ; ( "community_visibility_roundtrip"
      , [ check_vis_round_trip "public" D.Community_public
        ; check_vis_round_trip "private" D.Community_private
        ; check_vis_none "empty" ""
        ; check_vis_none "unknown" "secret"
        ; check_vis_none "unlisted not a value yet" "unlisted"
        ; check_vis_none "case-sensitive" "Public"
        ] )
      (* Effective COMMUNITY indexability: private is always non-indexable; public follows the flag. *)
    ; ( "effective_indexable_community"
      , [ check_idx_community "public + indexable => true" true D.Community_public ~community_indexable:true
        ; check_idx_community "public + non-indexable => false" false D.Community_public ~community_indexable:false
        ; check_idx_community "private + indexable flag still false" false D.Community_private ~community_indexable:true
        ; check_idx_community "private + non-indexable => false" false D.Community_private ~community_indexable:false
        ] )
      (* Effective CHILD (channel/section) indexability: needs community AND child opt-in; private kills all. *)
    ; ( "effective_indexable_child"
      , [ check_idx_child "public, community idx, child idx => true" true
            D.Community_public ~community_indexable:true ~child_indexable:true
        ; check_idx_child "public, community idx, child non-idx => false" false
            D.Community_public ~community_indexable:true ~child_indexable:false
        ; check_idx_child "public, community non-idx overrides child idx => false" false
            D.Community_public ~community_indexable:false ~child_indexable:true
        ; check_idx_child "private overrides everything => false" false
            D.Community_private ~community_indexable:true ~child_indexable:true
        ] )
      (* Private read predicate: public readable by anyone; private only by member/mod/admin. *)
    ; ( "can_read_community"
      , [ check_can_read "public readable for logged-out/non-member" true
            D.Community_public ~is_member:false ~is_mod:false ~is_admin:false
        ; check_can_read "private unreadable for logged-out/non-member" false
            D.Community_private ~is_member:false ~is_mod:false ~is_admin:false
        ; check_can_read "private readable for member" true
            D.Community_private ~is_member:true ~is_mod:false ~is_admin:false
        ; check_can_read "private readable for mod" true
            D.Community_private ~is_member:false ~is_mod:true ~is_admin:false
        ; check_can_read "private readable for admin" true
            D.Community_private ~is_member:false ~is_mod:false ~is_admin:true
        ] )
    ; ( "realtime_token"
      , [ rt_no_secret
        ; rt_format
        ; rt_topic_binding
        ; rt_expiry
        ; rt_signature
        ; rt_tamper
        ; rt_decode_garbage
        ; rt_capability_claim
        ; rt_capability_recomputed
        ] )
    ; ( "chat_api_negotiation"
      , [ check_wants_json "no accept header (legacy form post) stays redirect" false None
        ; check_wants_json "browser navigation accept stays redirect" false
            (Some "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
        ; check_wants_json "fetch json accept" true (Some "application/json")
        ; check_wants_json "json among other ranges" true
            (Some "application/json, text/plain, */*")
        ; check_wants_json "case-insensitive" true (Some "Application/JSON")
        ; check_wants_json "empty string" false (Some "")
        ] )
    ; ( "chat_api_content"
      , [ check_content "plain message" "ok:hello" "hello"
        ; check_content "trimmed" "ok:hi" "  hi  "
        ; check_content "empty" "empty" ""
        ; check_content "whitespace-only" "empty" "  \n \t "
        ; check_content "exactly 4000 accepted" ("ok:" ^ String.make 4000 'a')
            (String.make 4000 'a')
        ; check_content "4001 rejected" "too_long" (String.make 4001 'a')
        ; check_content "4001 with surrounding spaces still 4001" "too_long"
            (" " ^ String.make 4001 'a' ^ " ")
        ; check_content "4000 after trim accepted" ("ok:" ^ String.make 4000 'b')
            (" " ^ String.make 4000 'b' ^ " ")
        ] )
    ; ( "chat_api_error_shape"
      , [ check_error_json "code and copy only"
            {|{"error":"too_long","message":"Messages cannot exceed 4000 characters."}|}
            ~code:"too_long" ~message:"Messages cannot exceed 4000 characters."
        ; Alcotest.test_case "internal error body carries no detail" `Quick (fun () ->
              Alcotest.(check string) "fixed body"
                {|{"error":"internal","message":"Something went wrong. Please try again."}|}
                CA.internal_error_json)
        ] )
    ; ( "chat_send_success_row"
        (* The composer's JSON success body is chat_message_json applied to the
           INSERT ... RETURNING row: created_at comes from Postgres (NOT NULL
           default, '::text' cast), so the response always carries a canonical,
           renderable, minute-precision timestamp — never blank, never invented. *)
      , [ Alcotest.test_case "postgres microsecond timestamp renders non-empty minute" `Quick
            (fun () ->
              let row =
                chat_row ~id:2001 ~content:"hello"
                  ~created_at:"2026-07-20 15:01:25.066287" ()
              in
              let json =
                Yojson.Safe.to_string
                  (Earde.Handlers.chat_message_json ~channel_id:14 ~community_id:24
                     (row, Some "alice"))
              in
              Alcotest.(check bool) "created_at present, minute precision" true
                (let open Yojson.Safe.Util in
                 member "created_at" (Yojson.Safe.from_string json)
                 = `String "2026-07-20 15:01"))
        ; Alcotest.test_case "second-precision timestamp also renders non-empty minute" `Quick
            (fun () ->
              Alcotest.(check string) "minute truncation" "2026-07-20 15:01"
                (ST.minute_of_ts "2026-07-20 15:01:25"))
        ; check_msg_json
            "success body equals the realtime/catch-up serialization of the same row"
            {|{"v":1,"type":"chat_message_created","id":2002,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"same shape","created_at":"2026-07-20 15:01","deleted":false,"thread_id":null}|}
            (chat_row ~id:2002 ~content:"same shape"
               ~created_at:"2026-07-20 15:01:25.066287" ())
            (Some "alice")
        ; check_wants_json "curl/form default Accept */* stays redirect (no-JS contract)"
            false (Some "*/*")
        ; Alcotest.test_case "insert failure body is the fixed safe error, never a success row" `Quick
            (fun () ->
              let body = CA.internal_error_json in
              let json = Yojson.Safe.from_string body in
              let open Yojson.Safe.Util in
              Alcotest.(check bool) "has error code" true
                (member "error" json = `String "internal");
              Alcotest.(check bool) "carries no message id" true
                (member "id" json = `Null);
              Alcotest.(check bool) "carries no created_at" true
                (member "created_at" json = `Null))
        ] )
    ; ( "chat_message_json_shape"
      , [ check_msg_json "success row is new_msg-shaped with minute timestamp"
            {|{"v":1,"type":"chat_message_created","id":1765,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"hi there","created_at":"2026-07-20 15:01","deleted":false,"thread_id":null}|}
            (chat_row ~id:1765 ~content:"hi there"
               ~created_at:"2026-07-20 15:01:25.066287" ())
            (Some "alice")
        ; check_msg_json "deleted row masks content"
            {|{"v":1,"type":"chat_message_created","id":9,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"[message deleted]","created_at":"2026-07-20 15:01","deleted":true,"thread_id":null}|}
            (chat_row ~deleted:true ~id:9 ~content:"secret"
               ~created_at:"2026-07-20 15:01:25" ())
            (Some "alice")
        ; check_msg_json "tombstoned author and seed thread id"
            {|{"v":1,"type":"chat_message_created","id":3,"channel_id":14,"community_id":24,"user_id":7,"username":"[deleted]","content":"x","created_at":"2026-06-12 10:04","deleted":false,"thread_id":72}|}
            ~thread_id:72
            (chat_row ~id:3 ~content:"x" ~created_at:"2026-06-12 10:04:56" ())
            None
        ] )
    ; ( "features_shared_cursors"
      , [ check_slugs "single" [ "beryl" ] "beryl"
        ; check_slugs "trims and lowercases" [ "beryl"; "earde" ] " Beryl , EARDE "
        ; check_slugs "drops empties" [ "a"; "b" ] ",a,,b,"
        ; check_slugs "empty string" [] ""
        ; check_enabled "member enabled" true [ "beryl" ] "beryl"
        ; check_enabled "non-member disabled" false [ "beryl" ] "earde"
        ; check_enabled "case-insensitive community" true [ "beryl" ] "Beryl"
        ; check_enabled "empty list disables" false [] "beryl"
        ; check_enabled "default list has beryl" true
            Earde.Features.default_shared_cursor_slugs "beryl"
        ; check_enabled "default list is only beryl" false
            Earde.Features.default_shared_cursor_slugs "earde"
        ] )
      (* /delete-comment matrix: author-only for non-admins; admins delete with
         the admin label; moderators are refused (the mod_delete flow is the only
         community-removal path). decide has no community parameter, so there is
         nothing a forged hidden community_id could influence. *)
    ; ( "delete_comment_authorization"
      , [ check_cd "author deletes own comment" "author_delete"
            ~is_admin:false ~requester_id:7 ~owner_id:7
        ; check_cd "non-author (incl. any moderator) refused" "forbidden"
            ~is_admin:false ~requester_id:7 ~owner_id:8
        ; check_cd "admin deletes any comment" "admin_delete"
            ~is_admin:true ~requester_id:7 ~owner_id:8
        ; check_cd "admin deleting own comment stays on the admin path" "admin_delete"
            ~is_admin:true ~requester_id:7 ~owner_id:7
        ] )
      (* PostHog distinct-ID scheme: exactly user:<database_id>. *)
    ; ( "analytics_distinct_id"
      , [ check_distinct "user 42" "user:42" 42
        ; check_distinct "user 7" "user:7" 7
        ] )
      (* Exact consent-cookie parsing: only the exact values granted/denied
         count; everything else (missing, malformed, wrong case, extra text)
         is unknown and must produce no capture. *)
    ; ( "analytics_consent_parse"
      , [ check_consent "granted" "granted"
            (Some "earde_analytics_consent=granted")
        ; check_consent "denied" "denied" (Some "earde_analytics_consent=denied")
        ; check_consent "missing header" "unknown" None
        ; check_consent "empty header" "unknown" (Some "")
        ; check_consent "other cookies only" "unknown" (Some "session=abc; theme=dark")
        ; check_consent "granted among other cookies" "granted"
            (Some "session=abc; earde_analytics_consent=granted; theme=dark")
        ; check_consent "wrong case" "unknown" (Some "earde_analytics_consent=Granted")
        ; check_consent "trailing junk in value" "unknown"
            (Some "earde_analytics_consent=granted-ish")
        ; check_consent "space inside value" "unknown"
            (Some "earde_analytics_consent= granted")
        ; check_consent "name prefix mismatch" "unknown"
            (Some "xearde_analytics_consent=granted")
        ; check_consent "no equals sign" "unknown" (Some "earde_analytics_consent")
        ] )
      (* Consent gate end-to-end through the sink: one capture for granted,
         none otherwise. *)
    ; ( "analytics_consent_gate"
      , [ check_gate "granted captures once" 1
            (Some "earde_analytics_consent=granted")
        ; check_gate "denied captures nothing" 0
            (Some "earde_analytics_consent=denied")
        ; check_gate "missing cookie captures nothing" 0 None
        ; check_gate "malformed value captures nothing" 0
            (Some "earde_analytics_consent=yes")
        ; an_case "granted payload carries event name and distinct id" (fun () ->
              let captured =
                with_sink ~enabled:true (fun () ->
                    An.capture_if_consented
                      (consent_request (Some "earde_analytics_consent=granted"))
                      ~distinct_id:"user:7" an_login)
              in
              match captured with
              | [ payload ] ->
                  Alcotest.(check (option string)) "event"
                    (Some "login_succeeded")
                    (match payload_member "event" payload with
                     | Some (`String s) -> Some s
                     | _ -> None);
                  Alcotest.(check (option string)) "distinct_id"
                    (Some "user:7")
                    (match payload_member "distinct_id" payload with
                     | Some (`String s) -> Some s
                     | _ -> None)
              | l -> Alcotest.failf "expected 1 capture, got %d" (List.length l))
        ] )
      (* Per-constructor payloads: event names and the exact property key
         sets of the closed allowlist; optional fields omitted when None. *)
    ; ( "analytics_event_payloads"
      , List.map
          (fun (name, event) -> check_event_name ("name " ^ name) name event)
          an_all_events
        @ [ check_keys "signup_confirmed keys (incl. $set)"
              (List.assoc "signup_confirmed" an_all_events)
              [ "user_id"; "$set" ]
          ; check_keys "login_succeeded keys (incl. $set)"
              (List.assoc "login_succeeded" an_all_events)
              [ "user_id"; "$set" ]
          ; check_keys "community_joined keys"
              (List.assoc "community_joined" an_all_events)
              [ "user_id"; "community_id"; "community_slug";
                "community_visibility"; "$groups" ]
          ; check_keys "community_left keys"
              (List.assoc "community_left" an_all_events)
              [ "user_id"; "community_id"; "$groups" ]
          ; check_keys "chat_message_sent keys"
              (List.assoc "chat_message_sent" an_all_events)
              [ "user_id"; "community_id"; "community_slug"; "channel_id";
                "channel_slug"; "message_id"; "content_length";
                "response_mode"; "$groups" ]
          ; check_keys "post_created keys"
              (List.assoc "post_created" an_all_events)
              [ "user_id"; "community_id"; "section_id"; "post_id";
                "content_length"; "has_link"; "has_mention"; "$groups" ]
          ; check_keys "comment_created keys (no parent -> omitted)"
              (List.assoc "comment_created" an_all_events)
              [ "user_id"; "community_id"; "post_id"; "comment_id";
                "content_length"; "has_mention"; "$groups" ]
          ; check_keys "thread_promoted keys (no section -> omitted)"
              (List.assoc "thread_promoted" an_all_events)
              [ "user_id"; "community_id"; "community_slug"; "channel_id";
                "channel_slug"; "post_id"; "message_id";
                "promoted_message_count"; "promoted_participant_count";
                "$groups" ]
          ; check_keys "account_deleted keys"
              (An.Account_deleted { user_id = 1 })
              [ "user_id" ]
          ; an_case "community_joined full payload" (fun () ->
                let expected : Yojson.Safe.t =
                  `Assoc
                    [ ("api_key", `String "phc_test")
                    ; ("event", `String "community_joined")
                    ; ("distinct_id", `String "user:1")
                    ; ( "properties"
                      , `Assoc
                          [ ("user_id", `Int 1)
                          ; ("community_id", `Int 7)
                          ; ("community_slug", `String "ocaml")
                          ; ("community_visibility", `String "public")
                          ; ( "$groups"
                            , `Assoc [ ("community", `String "community:7") ] )
                          ] )
                    ]
                in
                Alcotest.check yojson "full payload" expected
                  (an_payload (List.assoc "community_joined" an_all_events)))
          ] )
      (* Hard rule of §5.1/§4.3: no bodies, titles, or tokens anywhere;
         person properties never as ordinary top-level event properties —
         only inside $set, and $set only on the two identity events. *)
    ; ( "analytics_property_allowlist"
      , [ an_case "no forbidden ordinary property on any event" (fun () ->
              let forbidden =
                [ "email"; "username"; "signup_date"; "is_admin"; "content";
                  "body"; "title"; "query"; "token" ]
              in
              List.iter
                (fun (name, event) ->
                  let keys = prop_keys (an_payload event) in
                  List.iter
                    (fun bad ->
                      if List.mem bad keys then
                        Alcotest.failf "%s carries forbidden property %s" name
                          bad)
                    forbidden)
                an_all_events)
        ; an_case "$set only on signup_confirmed and login_succeeded" (fun () ->
              List.iter
                (fun (name, event) ->
                  let has_set =
                    List.mem_assoc "$set" (payload_props (an_payload event))
                  in
                  let expected =
                    name = "signup_confirmed" || name = "login_succeeded"
                  in
                  if has_set <> expected then
                    Alcotest.failf "%s: unexpected $set presence (%b)" name
                      has_set)
                an_all_events)
        ; an_case "signup_confirmed $set is exactly the closed person record"
            (fun () ->
              match
                List.assoc_opt "$set"
                  (payload_props
                     (an_payload (List.assoc "signup_confirmed" an_all_events)))
              with
              | Some set -> Alcotest.check yojson "signup $set" an_person_set set
              | None -> Alcotest.fail "signup_confirmed has no $set")
        ; an_case "login_succeeded $set is exactly the closed person record"
            (fun () ->
              match
                List.assoc_opt "$set"
                  (payload_props
                     (an_payload (List.assoc "login_succeeded" an_all_events)))
              with
              | Some set -> Alcotest.check yojson "login $set" an_person_set set
              | None -> Alcotest.fail "login_succeeded has no $set")
        ] )
      (* $groups.community rides on community-scoped events only (§5.3). *)
    ; ( "analytics_groups"
      , [ check_group "joined has group" (Some "community:7")
            (List.assoc "community_joined" an_all_events)
        ; check_group "left has group" (Some "community:7")
            (List.assoc "community_left" an_all_events)
        ; check_group "chat has group" (Some "community:7")
            (List.assoc "chat_message_sent" an_all_events)
        ; check_group "post has group" (Some "community:7")
            (List.assoc "post_created" an_all_events)
        ; check_group "comment has group" (Some "community:7")
            (List.assoc "comment_created" an_all_events)
        ; check_group "promoted has group" (Some "community:7")
            (List.assoc "thread_promoted" an_all_events)
        ; check_group "signup has no group" None
            (List.assoc "signup_confirmed" an_all_events)
        ; check_group "login has no group" None
            (List.assoc "login_succeeded" an_all_events)
        ; check_group "deletion has no group" None
            (An.Account_deleted { user_id = 1 })
        ] )
      (* Consent-transition sync: the dedicated $identify payload with the
         same closed $set object as the identity events. *)
    ; ( "analytics_person_sync"
      , [ an_case "sync payload is a dedicated $identify" (fun () ->
              let expected : Yojson.Safe.t =
                `Assoc
                  [ ("api_key", `String "phc_test")
                  ; ("event", `String "$identify")
                  ; ("distinct_id", `String "user:9")
                  ; ("properties", `Assoc [ ("$set", an_person_set) ])
                  ]
              in
              let actual =
                AnT.person_sync_payload ~api_key:"phc_test"
                  ~distinct_id:"user:9" an_person
              in
              Alcotest.check yojson "sync payload" expected actual)
        ; an_case "sync_person_after_consent_grant emits exactly one $identify"
            (fun () ->
              let captured =
                with_sink ~enabled:true (fun () ->
                    An.sync_person_after_consent_grant ~distinct_id:"user:9"
                      an_person)
              in
              match captured with
              | [ payload ] ->
                  Alcotest.(check (option string)) "event" (Some "$identify")
                    (match payload_member "event" payload with
                     | Some (`String s) -> Some s
                     | _ -> None)
              | l -> Alcotest.failf "expected 1 capture, got %d" (List.length l))
        ] )
      (* $groupidentify: community group type/key plus only the five
         allowlisted group properties; created_at omitted when absent. *)
    ; ( "analytics_group_identify"
      , [ an_case "full payload with created_at" (fun () ->
              let expected : Yojson.Safe.t =
                `Assoc
                  [ ("api_key", `String "phc_test")
                  ; ("event", `String "$groupidentify")
                  ; ("distinct_id", `String "user:1")
                  ; ( "properties"
                    , `Assoc
                        [ ("$group_type", `String "community")
                        ; ("$group_key", `String "community:7")
                        ; ( "$group_set"
                          , `Assoc
                              [ ("community_id", `Int 7)
                              ; ("community_slug", `String "ocaml")
                              ; ("community_name", `String "OCaml")
                              ; ("community_visibility", `String "public")
                              ; ("created_at", `String "2026-01-01T00:00:00Z")
                              ] )
                        ] )
                  ]
              in
              let actual =
                AnT.group_identify_payload ~api_key:"phc_test"
                  ~distinct_id:"user:1"
                  { An.community_id = 7; community_slug = "ocaml";
                    community_name = "OCaml"; community_visibility = "public";
                    created_at = Some "2026-01-01T00:00:00Z" }
              in
              Alcotest.check yojson "group identify payload" expected actual)
        ; an_case "created_at omitted when absent" (fun () ->
              let actual =
                AnT.group_identify_payload ~api_key:"phc_test"
                  ~distinct_id:"user:1"
                  { An.community_id = 7; community_slug = "ocaml";
                    community_name = "OCaml"; community_visibility = "public";
                    created_at = None }
              in
              let set_keys =
                match List.assoc_opt "$group_set" (payload_props actual) with
                | Some (`Assoc l) -> List.map fst l
                | _ -> []
              in
              Alcotest.(check (slist string compare))
                "group set keys"
                [ "community_id"; "community_slug"; "community_name";
                  "community_visibility" ]
                set_keys)
        ] )
      (* Disabled configuration and transport failures: never a capture, never
         an exception into the caller. *)
    ; ( "analytics_disabled_and_failures"
      , [ an_case "disabled config captures nothing even when granted"
            (fun () ->
              let captured =
                with_sink ~enabled:false (fun () ->
                    An.capture_if_consented
                      (consent_request (Some "earde_analytics_consent=granted"))
                      ~distinct_id:"user:7" an_login;
                    An.sync_person_after_consent_grant ~distinct_id:"user:7"
                      { An.username = "a"; email = "a@a"; signup_date = "";
                        is_admin = false })
              in
              Alcotest.(check int) "no captures" 0 (List.length captured))
        ; an_case "raising sink does not escape into the caller" (fun () ->
              AnT.use_enabled_test_configuration ();
              AnT.set_capture_sink (fun _ -> failwith "sink boom");
              Fun.protect
                ~finally:(fun () ->
                  AnT.clear_capture_sink ();
                  AnT.clear_configuration_override ())
                (fun () ->
                  An.capture_if_consented
                    (consent_request (Some "earde_analytics_consent=granted"))
                    ~distinct_id:"user:7" an_login;
                  An.sync_person_after_consent_grant ~distinct_id:"user:7"
                    { An.username = "a"; email = "a@a"; signup_date = "";
                      is_admin = false });
              Alcotest.(check bool) "no exception escaped" true true)
        ; an_case "test config report exposes presence booleans only" (fun () ->
              AnT.use_enabled_test_configuration ();
              Fun.protect
                ~finally:(fun () -> AnT.clear_configuration_override ())
                (fun () ->
                  let report = AnT.config_report () in
                  Alcotest.(check (option bool)) "enabled" (Some true)
                    (List.assoc_opt "POSTHOG_ENABLED" report);
                  Alcotest.(check (option bool)) "token set" (Some true)
                    (List.assoc_opt "POSTHOG_PROJECT_TOKEN" report);
                  Alcotest.(check (option bool)) "personal key unset"
                    (Some false)
                    (List.assoc_opt "POSTHOG_PERSONAL_API_KEY" report);
                  Alcotest.(check (option bool)) "project id unset" (Some false)
                    (List.assoc_opt "POSTHOG_PROJECT_ID" report)))
        ] )
      (* §9 request validation matrix: JSON-only, exactly-one-field body,
         exact Origin, same-origin/same-site Sec-Fetch-Site. *)
    ; ( "analytics_consent_validate"
      , [ check_validate "granted ok" "granted"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
        ; check_validate "denied ok" "denied"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-site") {|{"state":"denied"}|}
        ; check_validate "json with charset ok" "granted"
            ~content_type:(Some "application/json; charset=utf-8")
            ~origin:(Some test_origin) ~sec_fetch_site:(Some "same-origin")
            {|{"state":"granted"}|}
        ; check_validate "form content-type rejected" "bad_request"
            ~content_type:(Some "application/x-www-form-urlencoded")
            ~origin:(Some test_origin) ~sec_fetch_site:(Some "same-origin")
            "state=granted"
        ; check_validate "missing content-type rejected" "bad_request"
            ~content_type:None ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
        ; check_validate "malformed json rejected" "bad_request"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") "{state:"
        ; check_validate "additional field rejected" "bad_request"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin")
            {|{"state":"granted","extra":1}|}
        ; check_validate "invalid state rejected" "bad_request"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") {|{"state":"yes"}|}
        ; check_validate "missing state rejected" "bad_request"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") {|{}|}
        ; check_validate "non-object body rejected" "bad_request"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "same-origin") {|"granted"|}
        ; check_validate "wrong origin rejected" "forbidden"
            ~content_type:(Some "application/json")
            ~origin:(Some "https://evil.example") ~sec_fetch_site:(Some "same-origin")
            {|{"state":"granted"}|}
        ; check_validate "missing origin rejected" "forbidden"
            ~content_type:(Some "application/json") ~origin:None
            ~sec_fetch_site:(Some "same-origin") {|{"state":"granted"}|}
        ; check_validate "cross-site fetch rejected" "forbidden"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:(Some "cross-site") {|{"state":"granted"}|}
        ; check_validate "missing sec-fetch-site rejected" "forbidden"
            ~content_type:(Some "application/json") ~origin:(Some test_origin)
            ~sec_fetch_site:None {|{"state":"granted"}|}
        ] )
      (* The real handler on mock requests: cookie contract, controlled JSON
         errors, no session required, failures isolated. *)
    ; ( "analytics_consent_endpoint"
      , [ an_case "granted: 204 + exact cookie, no session needed, no sync"
            (fun () ->
              let status, cookie, payloads =
                run_consent {|{"state":"granted"}|}
              in
              Alcotest.(check int) "status" 204 status;
              let cookie = Option.value ~default:"" cookie in
              Alcotest.(check bool) "value" true
                (contains cookie "earde_analytics_consent=granted");
              Alcotest.(check bool) "path" true (contains cookie "Path=/");
              Alcotest.(check bool) "max-age" true
                (contains cookie "Max-Age=15552000");
              Alcotest.(check bool) "samesite lax" true
                (contains cookie "SameSite=Lax");
              Alcotest.(check bool) "no httponly" false
                (contains cookie "HttpOnly");
              Alcotest.(check bool) "no secure on http origin" false
                (contains cookie "Secure");
              Alcotest.(check int) "anonymous grant syncs nothing" 0
                (List.length payloads))
        ; an_case "denied: 204 + denied cookie, no sync" (fun () ->
              let status, cookie, payloads = run_consent {|{"state":"denied"}|} in
              Alcotest.(check int) "status" 204 status;
              Alcotest.(check bool) "value" true
                (contains (Option.value ~default:"" cookie)
                   "earde_analytics_consent=denied");
              Alcotest.(check int) "no sync" 0 (List.length payloads))
        ; check_consent_reject "form body -> 400" 400
            ~headers:
              [ ("Content-Type", "application/x-www-form-urlencoded")
              ; ("Origin", test_origin)
              ; ("Sec-Fetch-Site", "same-origin")
              ]
            "state=granted"
        ; check_consent_reject "extra field -> 400" 400
            {|{"state":"granted","x":1}|}
        ; check_consent_reject "invalid state -> 400" 400 {|{"state":"maybe"}|}
        ; check_consent_reject "malformed json -> 400" 400 "{"
        ; check_consent_reject "bad origin -> 403" 403
            ~headers:
              [ ("Content-Type", "application/json")
              ; ("Origin", "https://evil.example")
              ; ("Sec-Fetch-Site", "same-origin")
              ]
            {|{"state":"granted"}|}
        ; check_consent_reject "no origin metadata -> 403" 403
            ~headers:[ ("Content-Type", "application/json") ]
            {|{"state":"granted"}|}
        ; an_case "unsupported method -> controlled 405 JSON" (fun () ->
              let response =
                Lwt_main.run
                  (Earde.Handlers.analytics_consent_method_not_allowed
                     (Dream.request ~method_:`GET ~target:"/analytics/consent"
                        ""))
              in
              Alcotest.(check int) "status" 405
                (Dream.status_to_int (Dream.status response));
              Alcotest.(check (option string)) "allow" (Some "POST")
                (Dream.header response "Allow"))
        ; an_case "person-lookup failure never blocks the consent response"
            (fun () ->
              (* Session present but no sql pool: the lookup raises, is
                 swallowed, and the cookie is still set. *)
              let payloads = ref [] in
              AnT.use_enabled_test_configuration ();
              AnT.set_capture_sink (fun p -> payloads := p :: !payloads);
              Fun.protect
                ~finally:(fun () ->
                  AnT.clear_capture_sink ();
                  AnT.clear_configuration_override ())
                (fun () ->
                  let pipeline =
                    Dream.memory_sessions (fun req ->
                        Lwt.bind
                          (Dream.set_session_field req "user_id" "12345")
                          (fun () ->
                            Earde.Handlers.analytics_consent_handler req))
                  in
                  let response =
                    Lwt_main.run
                      (pipeline
                         (Dream.request ~method_:`POST
                            ~target:"/analytics/consent"
                            ~headers:consent_good_headers
                            {|{"state":"granted"}|}))
                  in
                  Alcotest.(check int) "status" 204
                    (Dream.status_to_int (Dream.status response));
                  Alcotest.(check bool) "cookie still set" true
                    (contains
                       (Option.value ~default:""
                          (Dream.header response "Set-Cookie"))
                       "earde_analytics_consent=granted");
                  Alcotest.(check int) "no sync happened" 0
                    (List.length !payloads)))
        ] )
      (* Layout emission: banner + strictly public config when enabled;
         nothing at all when disabled; never a server-only secret. *)
    ; ( "analytics_layout"
      , [ an_case "enabled: banner, script, public attrs only" (fun () ->
              AnT.use_enabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "banner" true
                    (contains html "id='analytics-consent'");
                  Alcotest.(check bool) "script" true
                    (contains html "/static/js/analytics.js");
                  Alcotest.(check bool) "token attr" true
                    (contains html "data-ph-token='phc_test_token'");
                  Alcotest.(check bool) "api host attr" true
                    (contains html "data-ph-api-host='https://eu.i.posthog.com'");
                  Alcotest.(check bool) "banner ships hidden" true
                    (contains html "id='analytics-consent' hidden");
                  Alcotest.(check bool) "no personal key marker" false
                    (contains html "phx_");
                  Alcotest.(check bool) "no project id leak" false
                    (contains html "229260")))
        ; an_case "disabled: no banner, no script, no config" (fun () ->
              AnT.use_disabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "no banner" false
                    (contains html "analytics-consent");
                  Alcotest.(check bool) "no script" false
                    (contains html "/static/js/analytics.js");
                  Alcotest.(check bool) "no token attr" false
                    (contains html "data-ph-token")))
        ] )
      (* §2.3 URL rule, reference implementation. *)
    ; ( "analytics_url_sanitizer"
      , [ an_case "token query stripped" (fun () ->
              Alcotest.(check string) "confirm-email" "/confirm-email"
                (An.sanitize_url_for_analytics "/confirm-email?token=abc"))
        ; an_case "search query stripped" (fun () ->
              Alcotest.(check string) "search" "/search"
                (An.sanitize_url_for_analytics "/search?q=x&page=2"))
        ; an_case "fragment stripped" (fun () ->
              Alcotest.(check string) "fragment" "https://earde.com/p/1"
                (An.sanitize_url_for_analytics "https://earde.com/p/1#frag"))
        ; an_case "query and fragment stripped" (fun () ->
              Alcotest.(check string) "both" "https://earde.com/feed"
                (An.sanitize_url_for_analytics "https://earde.com/feed?a=1#b"))
        ; an_case "clean url unchanged" (fun () ->
              Alcotest.(check string) "clean" "https://earde.com/feed"
                (An.sanitize_url_for_analytics "https://earde.com/feed"))
        ] )
      (* Replay masking config coverage: every §6 selector/setting must be in
         the shipped analytics.js, plus the document-title protections. *)
    ; ( "analytics_replay_masking"
      , [ an_case "analytics.js covers all §6 masking targets" (fun () ->
              let js = read_analytics_js () in
              List.iter
                (fun needle ->
                  if not (contains js needle) then
                    Alcotest.failf "analytics.js is missing %S" needle)
                [ ".ph-mask"; ".cs-msg-text"; ".cs-msg-author"; "#chat-typing"
                ; ".cs-presence-name"; ".ft-title"; ".ft-preview"; ".th-title"
                ; ".th-body"; ".sr-row-title"; ".sr-row-excerpt"; ".ctext"
                ; "comment-content-"; ".account-notif-msg"; ".cm-table-reason"
                ; ".admin-cell-muted"; ".account-bio"; "maskAllInputs: true"
                ; "recordHeaders: false"; "recordBody: false"
                ; "capture_pageview: false"; "capture_pageleave: true"
                ])
        ; an_case "document title is masked in replay and stripped from events"
            (fun () ->
              let js = read_analytics_js () in
              (* "title" element selector in the mask list (verified against
                 the rrweb source posthog-js bundles: text-node masking checks
                 the parent element, only STYLE/SCRIPT excluded). *)
              Alcotest.(check bool) "title selector present" true
                (contains js "\"title\",");
              (* $title removed from every outbound event... *)
              Alcotest.(check bool) "$title deletion present" true
                (contains js "delete props.$title");
              (* ...and never replaced by document.title or any other
                 user-controlled value; no search-query handling either. *)
              Alcotest.(check bool) "no document.title reference" false
                (contains js "document.title");
              Alcotest.(check bool) "no query-string handling" false
                (contains js "location.search"))
        ] )
      (* Document-title leakage: the search term must never enter <title>. *)
    ; ( "analytics_title_leakage"
      , [ an_case "/search?q=secret renders a generic document title" (fun () ->
              let rendered = ref "" in
              let (_ : Dream.response) =
                Lwt_main.run
                  (Dream.memory_sessions
                     (fun req ->
                       rendered :=
                         Earde.Pages.search_results_page ~admin_usernames:[]
                           [] 1 "all" "secret" [] [] [] [] req;
                       Dream.html !rendered)
                     (Dream.request ~method_:`GET
                        ~target:"/search?q=secret" ""))
              in
              let html = !rendered in
              let title_tag =
                (* Extract exactly the <title>…</title> element. *)
                let start = ref 0 and stop = ref 0 in
                String.iteri
                  (fun i _ ->
                    if
                      i + 7 <= String.length html
                      && String.sub html i 7 = "<title>"
                    then start := i
                    else if
                      i + 8 <= String.length html
                      && String.sub html i 8 = "</title>"
                    then if !stop = 0 then stop := i)
                  html;
                if !stop > !start then String.sub html !start (!stop - !start)
                else Alcotest.fail "no <title> found"
              in
              Alcotest.(check bool) "title has no query" false
                (contains title_tag "secret");
              Alcotest.(check bool) "title is the generic label" true
                (contains title_tag "Search");
              (* The visible UI still echoes the query (input value, masked by
                 maskAllInputs) — only the title had to change. *)
              Alcotest.(check bool) "page body still echoes query in input" true
                (contains html "value='secret'"))
        ] )
      (* §4.2 identity attribute: exactly user:<id> on authenticated pages,
         nothing anywhere else, and no person data in analytics attributes. *)
    ; ( "analytics_identity_attrs"
      , [ an_case "authenticated layout emits exactly user:42" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_layout ~session_user:"42" () in
                  Alcotest.(check (option string)) "identity attr"
                    (Some "user:42")
                    (attr_value html "data-analytics-user")))
        ; an_case "anonymous layout emits no identity attribute" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_layout () in
                  Alcotest.(check (option string)) "no identity" None
                    (attr_value html "data-analytics-user")))
        ; an_case "malformed session value emits no identity" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_layout ~session_user:"not-a-number" () in
                  Alcotest.(check (option string)) "no identity" None
                    (attr_value html "data-analytics-user")))
        ; an_case "disabled analytics emits neither identity nor group"
            (fun () ->
              AnT.use_disabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    render_layout ~session_user:"42" ~analytics_community_id:7
                      ()
                  in
                  Alcotest.(check bool) "no identity attr" false
                    (contains html "data-analytics-user");
                  Alcotest.(check bool) "no group attr" false
                    (contains html "data-analytics-group")))
        ; an_case "only the two analytics attributes exist; no person data"
            (fun () ->
              with_enabled_config (fun () ->
                  let html =
                    render_layout ~session_user:"42" ~analytics_community_id:7
                      ()
                  in
                  (* exactly one identity and one group attribute (the other
                     data-analytics-* hits are the banner's own accept /
                     refuse / error control hooks, which carry no values) *)
                  Alcotest.(check int) "one identity attr" 1
                    (count_sub html "data-analytics-user='");
                  Alcotest.(check int) "one group attr" 1
                    (count_sub html "data-analytics-group='");
                  Alcotest.(check bool) "no username attr" false
                    (contains html "data-analytics-username");
                  Alcotest.(check bool) "no email anywhere in analytics root"
                    false
                    (contains html "data-analytics-email")))
        ] )
      (* §5.3 group attribute: exactly community:<id> on community-scoped
         pages, actively absent on global pages, across all wrappers. *)
    ; ( "analytics_group_attrs"
      , [ an_case "community layout emits exactly community:7" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_layout ~analytics_community_id:7 () in
                  Alcotest.(check (option string)) "group attr"
                    (Some "community:7")
                    (attr_value html "data-analytics-group")))
        ; an_case "global layout emits no group attribute" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_layout () in
                  Alcotest.(check (option string)) "no group" None
                    (attr_value html "data-analytics-group")))
        ; an_case "community_shell (public) carries the group key" (fun () ->
              with_enabled_config (fun () ->
                  let community =
                    test_community ~id:9 ~visibility:Earde.Db.Community_public
                  in
                  let html =
                    Earde.Components.community_shell ~title:"T" ~community
                      ~nav_groups:[] ~main:"MAIN" ()
                  in
                  Alcotest.(check (option string)) "group attr"
                    (Some "community:9")
                    (attr_value html "data-analytics-group");
                  Alcotest.(check bool) "public: no replay block" false
                    (contains html "ph-no-capture")))
        ; an_case
            "community_shell (private) keeps group key + ph-no-capture only"
            (fun () ->
              with_enabled_config (fun () ->
                  let community =
                    test_community ~id:9 ~visibility:Earde.Db.Community_private
                  in
                  let html =
                    Earde.Components.community_shell ~title:"T" ~community
                      ~nav_groups:[] ~main:"MAIN" ()
                  in
                  Alcotest.(check (option string)) "group attr"
                    (Some "community:9")
                    (attr_value html "data-analytics-group");
                  Alcotest.(check bool) "content replay-blocked" true
                    (contains html "class='ph-no-capture'>MAIN");
                  (* only the group attribute — no identity (no request) and
                     no visibility/name leak *)
                  Alcotest.(check int) "one group attr" 1
                    (count_sub html "data-analytics-group='");
                  Alcotest.(check int) "no identity attr" 0
                    (count_sub html "data-analytics-user='");
                  Alcotest.(check bool) "no visibility leak" false
                    (contains html "data-analytics-visibility")))
        ; an_case "community wrappers all pass the key through" (fun () ->
              with_enabled_config (fun () ->
                  let check_attr label expected html =
                    Alcotest.(check (option string)) label expected
                      (attr_value html "data-analytics-group")
                  in
                  check_attr "community_home_page" (Some "community:5")
                    (Earde.Components.community_home_page
                       ~analytics_community_id:5 ~title:"T" ~body:"B" ());
                  check_attr "community_manage_page" (Some "community:6")
                    (Earde.Components.community_manage_page
                       ~analytics_community_id:6 ~title:"T" ~body:"B" ());
                  check_attr "create_page (join/post/report/start-thread)"
                    (Some "community:8")
                    (Earde.Components.create_page ~analytics_community_id:8
                       ~title:"T" ~body:"B" ())))
        ; an_case "global wrappers emit no group" (fun () ->
              with_enabled_config (fun () ->
                  List.iter
                    (fun (label, html) ->
                      Alcotest.(check (option string)) label None
                        (attr_value html "data-analytics-group"))
                    [ ( "account_page",
                        Earde.Components.account_page ~title:"T" ~body:"B" () )
                    ; ( "admin_page",
                        Earde.Components.admin_page ~title:"T" ~body:"B" () )
                    ; ( "search_page",
                        Earde.Components.search_page ~title:"T" ~body:"B" () )
                    ; ( "feed_shell",
                        Earde.Components.feed_shell ~title:"T" ~main:"M" () )
                    ; ( "create_page without community",
                        Earde.Components.create_page ~title:"T" ~body:"B" () )
                    ]))
        ] )
      (* Shipped analytics.js: reconciliation contract + strict ordering. *)
    ; ( "analytics_js_reconciliation"
      , [ an_case "identity → group → pageview ordering" (fun () ->
              let js = read_analytics_js () in
              let pos needle =
                match index_of js needle with
                | Some i -> i
                | None -> Alcotest.failf "analytics.js is missing %S" needle
              in
              let identity_call = pos "reconcileIdentity();" in
              let group_call = pos "reconcileGroup();" in
              let pageview = pos "pageviewSent = true" in
              Alcotest.(check bool) "identify before group" true
                (identity_call < group_call);
              Alcotest.(check bool) "group before pageview" true
                (group_call < pageview))
        ; an_case "identity contract guards" (fun () ->
              let js = read_analytics_js () in
              (* identify only when the persisted id differs, key only *)
              Alcotest.(check bool) "differs guard" true
                (contains js "current !== identityAttr");
              Alcotest.(check bool) "identify carries no properties" true
                (contains js "window.posthog.identify(identityAttr);");
              (* reset only a user:-prefixed id; anonymous ids preserved *)
              Alcotest.(check bool) "reset guard" true
                (contains js
                   "else if (typeof current === \"string\" && current.indexOf(\"user:\") === 0)");
              Alcotest.(check bool) "reset present" true
                (contains js "window.posthog.reset();"))
        ; an_case "group contract guards" (fun () ->
              let js = read_analytics_js () in
              (* key only — exactly two arguments, no properties object *)
              Alcotest.(check bool) "group key only" true
                (contains js "window.posthog.group(\"community\", groupAttr);");
              Alcotest.(check bool) "global pages clear sticky group" true
                (contains js "window.posthog.resetGroups();");
              Alcotest.(check bool) "reads the layout attributes" true
                (contains js "data-analytics-user"
                && contains js "data-analytics-group"))
        ; an_case "no duplicate init or pageview" (fun () ->
              let js = read_analytics_js () in
              Alcotest.(check bool) "shared init promise" true
                (contains js "if (initPromise) return initPromise;");
              Alcotest.(check bool) "single pageview guard" true
                (contains js "if (!pageviewSent)");
              Alcotest.(check int) "exactly one $pageview capture" 1
                (count_sub js "posthog.capture(\"$pageview\""))
        ] )
    ; ( "analytics_group_identify_api"
      , [ check_group_identify_gate "granted emits one" 1 ~enabled:true
            (Some "earde_analytics_consent=granted")
        ; check_group_identify_gate "denied emits none" 0 ~enabled:true
            (Some "earde_analytics_consent=denied")
        ; check_group_identify_gate "missing cookie emits none" 0 ~enabled:true
            None
        ; check_group_identify_gate "malformed value emits none" 0
            ~enabled:true
            (Some "earde_analytics_consent=maybe")
        ; check_group_identify_gate "disabled analytics emits none" 0
            ~enabled:false
            (Some "earde_analytics_consent=granted")
        ; an_case "granted payload is a closed $groupidentify" (fun () ->
              match
                run_group_identify ~enabled:true
                  (Some "earde_analytics_consent=granted")
              with
              | [ p ] ->
                  Alcotest.(check string) "event" "$groupidentify" (event_of p);
                  Alcotest.(check string) "caller-supplied user distinct id"
                    "user:7" (distinct_of p);
                  Alcotest.(check (slist string compare))
                    "props keys"
                    [ "$group_type"; "$group_key"; "$group_set" ]
                    (prop_keys p);
                  Alcotest.(check (option string)) "group key"
                    (Some "community:7") (group_key_prop_of p);
                  Alcotest.(check (slist string compare))
                    "closed group set"
                    [ "community_id"; "community_slug"; "community_name";
                      "community_visibility" ]
                    (List.map fst (group_set_of p))
              | l -> Alcotest.failf "expected 1 payload, got %d" (List.length l))
        ; an_case "raising sink never escapes to the caller" (fun () ->
              AnT.use_enabled_test_configuration ();
              AnT.set_capture_sink (fun _ -> failwith "sink boom");
              Fun.protect
                ~finally:(fun () ->
                  AnT.clear_capture_sink ();
                  AnT.clear_configuration_override ())
                (fun () ->
                  An.identify_community_if_consented
                    (consent_request (Some "earde_analytics_consent=granted"))
                    ~distinct_id:"user:7" (an_group ());
                  An.capture_if_consented
                    (consent_request (Some "earde_analytics_consent=granted"))
                    ~distinct_id:"user:7" an_login))
        ] )
    ; ( "mod_delete_community_scope", Mod_scope.suite )
    ; ( "db_returning_ids", Returning_ids.suite )
    ; ( "analytics_consent_db", [ consent_sync_db_case ] )
    ; ( "analytics_step6_events", Step6_events.suite )
    ]
