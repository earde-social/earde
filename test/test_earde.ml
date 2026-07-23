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

(* Onboarding-state enum (network-community lifecycle foundation): result-based
   decode — off-enum values are an explicit Error, never a silent published. *)
let check_onb_decodes name s v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (D.community_onboarding_state_of_string s = Ok v))

let check_onb_serializes name v s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name s (D.string_of_community_onboarding_state v))

let check_onb_error name s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true
        (match D.community_onboarding_state_of_string s with
         | Error _ -> true
         | Ok _ -> false))

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
  { An.username = "alice";
    signup_date = "2026-01-01T00:00:00Z"; is_admin = false }

let an_login = An.Account_logged_in { user_id = 7; person = an_person }

(* The exact closed $set object an_person must serialize to — email-free by
   contract: the stable identity is user:<id> and email never reaches
   PostHog. *)
let an_person_set : Yojson.Safe.t =
  `Assoc
    [ ("username", `String "alice")
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
    ("account_signed_up", An.Account_signed_up { user_id = 1; person = an_person });
    ("account_logged_in", An.Account_logged_in { user_id = 1; person = an_person });
    ( "community_joined",
      An.Community_joined
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          community_visibility = "public" } );
    ("community_left", An.Community_left { user_id = 1; community_id = 7 });
    ( "chat_message_sent",
      An.Chat_message_sent
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          channel_id = 3; channel_slug = Some "general"; message_id = 91L;
          content_length = 42; response_mode = An.Response_json } );
    ( "forum_thread_created",
      An.Forum_thread_created
        { user_id = 1; community_id = 7; section_id = Some 2; post_id = 10;
          content_length = 100; has_link = true; has_mention = false } );
    ( "forum_comment_created",
      An.Forum_comment_created
        { user_id = 1; community_id = 7; post_id = 10; comment_id = 55;
          parent_comment_id = None; content_length = 9; has_mention = true } );
    ( "conversation_promoted",
      An.Conversation_promoted
        { user_id = 1; community_id = 7; community_slug = Some "ocaml";
          channel_id = 3; channel_slug = Some "general"; section_id = None;
          post_id = 11; message_id = 91L; promoted_message_count = 4;
          promoted_participant_count = Some 2 } );
    ("account_deleted", An.Account_deleted);
  ]

(* Test payloads are built for the Development environment; the envelope
   value is asserted separately in the analytics_envelope suite. *)
let an_payload event =
  AnT.event_payload ~api_key:"phc_test" ~environment:An.Development
    ~distinct_id:"user:1" event

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
                    Alcotest.(check bool) "email absent" false
                      (List.mem_assoc "email" set);
                    Alcotest.(check (slist string compare)) "sync $set keys"
                      [ "username"; "signup_date"; "is_admin" ]
                      (List.map fst set)
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
  { An.community_id = 7; community_slug = Some "ocaml";
    community_name = Some "OCaml"; community_visibility = "public";
    created_at }

(* The §13 private shape: numeric id and closed visibility only — no
   readable identifiers. *)
let an_private_group : An.community_group =
  { An.community_id = 9; community_slug = None; community_name = None;
    community_visibility = "private"; created_at = None }

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

(* Polls an Lwt predicate until true, failing the test after [timeout]
   seconds — used to await post-response async analytics/deletion chains. *)
let wait_until ~label ?(timeout = 5.0) predicate =
  let ( let* ) = Lwt.bind in
  let rec loop remaining =
    let* ok = predicate () in
    if ok then Lwt.return_unit
    else if remaining <= 0.0 then Alcotest.failf "%s: timed out waiting" label
    else
      let* () = Lwt_unix.sleep 0.05 in
      loop (remaining -. 0.05)
  in
  loop timeout

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
      ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || c.id::text FROM communities c WHERE c.slug LIKE 'step6-%')"
      ; "DELETE FROM communities WHERE slug LIKE 'step6-%'"
      ; "DELETE FROM posthog_group_cleanup_jobs WHERE NOT EXISTS (SELECT 1 FROM communities c WHERE 'community:' || c.id::text = posthog_group_cleanup_jobs.group_key)"
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

  let q_delete_job =
    (Caqti_type.int ->. Caqti_type.unit)
    "DELETE FROM posthog_person_deletion_jobs WHERE id = $1"

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
    db_case "account_signed_up once with closed $set; invalid/unconsented silent"
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
             Alcotest.(check string) "event" "account_signed_up" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys" [ "user_id"; "$set"; "deployment_environment" ]
               (prop_keys p);
             check_set_keys "closed $set"
               [ "username"; "signup_date"; "is_admin" ] p;
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
    db_case "account_logged_in once with closed $set; bad password silent"
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
             Alcotest.(check string) "event" "account_logged_in" (event_of p);
             Alcotest.(check string) "distinct id"
               ("user:" ^ string_of_int uid) (distinct_of p);
             Alcotest.(check (slist string compare))
               "person fields only inside $set"
               [ "user_id"; "$set"; "deployment_environment" ]
               (prop_keys p);
             check_set_keys "closed $set"
               [ "username"; "signup_date"; "is_admin" ] p
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
                 "community_visibility"; "$groups"; "deployment_environment" ]
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
               "props keys"
               [ "user_id"; "community_id"; "$groups";
                 "deployment_environment" ]
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
             (* The switch is TO private, so the §13 redaction applies to
                this very $groupidentify: no readable identifiers, no
                indexability — only the numeric id and the new closed
                visibility value. *)
             Alcotest.(check (slist string compare))
               "closed group props only (private: no slug/name/indexability)"
               [ "community_id"; "community_visibility" ]
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
                 "response_mode"; "$groups"; "deployment_environment" ]
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
    db_case "forum_thread_created once on success; non-member silent"
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
             Alcotest.(check string) "event" "forum_thread_created" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys (no title/body/url)"
               [ "user_id"; "community_id"; "post_id"; "content_length";
                 "has_link"; "has_mention"; "$groups";
                 "deployment_environment" ]
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
    db_case "forum_comment_created carries the real RETURNING comment id"
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
             Alcotest.(check string) "event" "forum_comment_created" (event_of p);
             Alcotest.(check (slist string compare))
               "props keys (top-level comment: no parent_comment_id)"
               [ "user_id"; "community_id"; "post_id"; "comment_id";
                 "content_length"; "has_mention"; "$groups";
                 "deployment_environment" ]
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
    db_case "conversation_promoted once with counts and group key"
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
             Alcotest.(check string) "event" "conversation_promoted" (event_of p);
             Alcotest.(check string) "distinct"
               ("user:" ^ string_of_int uid) (distinct_of p);
             Alcotest.(check (slist string compare))
               "props keys (sectionless community)"
               [ "user_id"; "community_id"; "community_slug"; "channel_id";
                 "channel_slug"; "post_id"; "message_id";
                 "promoted_message_count"; "promoted_participant_count";
                 "$groups"; "deployment_environment" ]
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
        (* Legacy community creation is admin-gated (server-side session
           check); the founder must carry the authoritative admin field to
           reach the analytics behavior under test. *)
        let session =
          [ ("user_id", string_of_int uid); ("username", "step6_founder");
            ("is_admin", "true") ]
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
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find q_insert_user ("step6_deleteme", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        (* Step 7 moved the capture into the post-response async cleanup chain
           (capture → claim → deletion attempt), so this runner keeps the sink
           and configuration installed until the chain has finished — tracked
           by the durable job row acquiring its safe last_error (no deletion
           credentials are configured here, so the attempt must leave the job
           pending with missing_configuration). *)
        let payloads = ref [] in
        AnT.use_enabled_test_configuration ();
        AnT.set_capture_sink (fun p -> payloads := !payloads @ [ p ]);
        Lwt.finalize
          (fun () ->
            let pipeline =
              Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
              let* () =
                Dream.set_session_field req "user_id" (string_of_int uid)
              in
              let* () =
                Dream.set_session_field req "username" "step6_deleteme"
              in
              let csrf = Dream.csrf_token req in
              Dream.set_body req (form_body [ ("dream.csrf", csrf) ]);
              Earde.Handlers.delete_account_handler req
            in
            let request =
              Dream.request ~method_:`POST ~target:"/delete-account"
                ~headers:
                  ([ ("Content-Type", "application/x-www-form-urlencoded") ]
                  @ consent_header (Some "granted"))
                ""
            in
            let* response = pipeline request in
            Alcotest.(check bool) "deletion redirects" true
              (is_redirect (Dream.status_to_int (Dream.status response)));
            let* () =
              wait_until ~label:"post-deletion cleanup chain" (fun () ->
                  let* job = Earde.Db.get_posthog_deletion_job conn did in
                  match job with
                  | Ok (Some (_, _, _, Some _)) -> Lwt.return true
                  | _ -> Lwt.return false)
            in
            (match !payloads with
             | [ p ] ->
                 Alcotest.(check string) "event" "account_deleted" (event_of p);
                 Alcotest.(check string) "constant non-user distinct id"
                   Earde.Analytics.account_deletion_distinct_id (distinct_of p);
                 Alcotest.(check (slist string compare))
                   "personless: person processing off, plus the envelope"
                   [ "$process_person_profile"; "deployment_environment" ]
                   (prop_keys p);
                 Alcotest.(check bool) "no user identity in the metric" false
                   (contains (Yojson.Safe.to_string p) did)
             | l ->
                 Alcotest.failf "expected 1 deletion metric, got %d"
                   (List.length l));
            let* job = Earde.Db.get_posthog_deletion_job conn did in
            let* job = or_fail_s "job row" job in
            let* job_id =
              match job with
              | Some (job_id, status, attempts, last_error) ->
                  Alcotest.(check string) "job stays durably pending" "pending"
                    status;
                  Alcotest.(check int) "one immediate attempt" 1 attempts;
                  Alcotest.(check (option string)) "safe config marker"
                    (Some "missing_configuration") last_error;
                  Lwt.return job_id
              | None -> Alcotest.fail "no durable deletion job"
            in
            let* name = C.find_opt q_username_by_id uid in
            let* name = or_fail "anonymized row" name in
            Alcotest.(check (option string)) "row anonymized"
              (Some (Printf.sprintf "[deleted_%d]" uid))
              name;
            (* The anonymized username no longer matches the step6_ cleanup
               pattern — drop the job and the row here. *)
            let* r = C.exec q_delete_job job_id in
            let* () = or_fail "drop job" r in
            let* r = C.exec q_delete_user uid in
            let* () = or_fail "drop anonymized user" r in
            Lwt.return_unit)
          (fun () ->
            AnT.clear_capture_sink ();
            AnT.clear_configuration_override ();
            Lwt.return_unit))

  let suite =
    [ signup_case; login_case; join_case; leave_case; chat_case; post_case
    ; comment_case; promote_case; create_community_case; update_settings_case
    ; visibility_case; delete_account_case
    ]
end

(* --- Step-7: durable PostHog person deletion ------------------------------ *)

(* Local Persons-API stub: an in-process cohttp server on an ephemeral
   127.0.0.1 port — no real PostHog, no internet. [handler] maps a recorded
   request to (status code, body). *)
module Api_stub = struct
  type req = {
    meth : string;
    path : string;
    query : (string * string list) list;
    auth : string option;
    body : string;
  }

  let free_port () =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let port =
      match Unix.getsockname sock with
      | Unix.ADDR_INET (_, p) -> p
      | _ -> assert false
    in
    Unix.close sock;
    port

  let start handler =
    let ( let* ) = Lwt.bind in
    let seen = ref [] in
    let callback _conn request body =
      let uri = Cohttp.Request.uri request in
      let path = Uri.path uri in
      if path = "/__ready" then
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"ok" ()
      else begin
        let ( let* ) = Lwt.bind in
        let* body_string = Cohttp_lwt.Body.to_string body in
        let req =
          {
            meth = Cohttp.Code.string_of_method (Cohttp.Request.meth request);
            path;
            query = Uri.query uri;
            auth =
              Cohttp.Header.get (Cohttp.Request.headers request) "authorization";
            body = body_string;
          }
        in
        seen := !seen @ [ req ];
        let status, body = handler req in
        Cohttp_lwt_unix.Server.respond_string
          ~status:(Cohttp.Code.status_of_code status)
          ~body ()
      end
    in
    let port = free_port () in
    let stop_promise, stop_resolver = Lwt.wait () in
    Lwt.async (fun () ->
        Cohttp_lwt_unix.Server.create ~stop:stop_promise
          ~mode:(`TCP (`Port port))
          (Cohttp_lwt_unix.Server.make ~callback ()));
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    let rec wait_ready retries =
      Lwt.catch
        (fun () ->
          let* _resp, body =
            Cohttp_lwt_unix.Client.get (Uri.of_string (base_url ^ "/__ready"))
          in
          Cohttp_lwt.Body.drain_body body)
        (fun exn ->
          if retries <= 0 then Lwt.reraise exn
          else
            let* () = Lwt_unix.sleep 0.02 in
            wait_ready (retries - 1))
    in
    let* () = wait_ready 100 in
    Lwt.return (base_url, seen, fun () -> Lwt.wakeup_later stop_resolver ())
end

(* Dummy credential values only — never real ones. *)
let deletion_test_key = "phx_test_dummy"
let stub_uuid = "11111111-2222-3333-4444-555555555555"
let other_uuid = "99999999-8888-7777-6666-555555555555"

let person_json uuid = Printf.sprintf {|{"id": %S, "properties": {}}|} uuid

let results_body persons =
  Printf.sprintf {|{"results": [%s]}|} (String.concat ", " persons)

(* Configures the deletion client against the stub, runs [f], restores. *)
let with_persons_stub handler f =
  Lwt_main.run
    (let ( let* ) = Lwt.bind in
     let* base_url, seen, stop = Api_stub.start handler in
     AnT.use_deletion_test_configuration ~ui_host:base_url
       ~project_id:(Some "42") ~personal_api_key:(Some deletion_test_key) ();
     Lwt.finalize
       (fun () -> f ~seen)
       (fun () ->
         AnT.clear_configuration_override ();
         stop ();
         Lwt.return_unit))

(* Reference stub behavior: bearer-authenticated lookup of user:314 returns
   [persons]; DELETE of stub_uuid returns [delete_status]. *)
let persons_handler ?(lookup_status = 200) ?(persons = [ person_json stub_uuid ])
    ?lookup_body_override ?(delete_status = 204) () (req : Api_stub.req) =
  if req.auth <> Some ("Bearer " ^ deletion_test_key) then
    (401, {|{"type":"authentication_error"}|})
  else if req.meth = "GET" && req.path = "/api/projects/42/persons/" then
    match lookup_body_override with
    | Some body -> (lookup_status, body)
    | None ->
        if List.assoc_opt "distinct_id" req.query = Some [ "user:314" ] then
          (lookup_status, results_body persons)
        else (200, results_body [])
  else if
    req.meth = "DELETE" && req.path = "/api/projects/42/persons/" ^ stub_uuid ^ "/"
  then (delete_status, "")
  else (404, {|{"detail":"not found"}|})

let attempt_result = Alcotest.(result unit string)

let check_attempt name handler expected ~expect_delete =
  an_case name (fun () ->
      with_persons_stub handler (fun ~seen ->
          let ( let* ) = Lwt.bind in
          let* r =
            Earde.Posthog_deletion.attempt_person_deletion
              ~distinct_id:"user:314"
          in
          Alcotest.(check attempt_result) name expected r;
          let deletes =
            List.filter (fun (q : Api_stub.req) -> q.meth = "DELETE") !seen
          in
          Alcotest.(check int)
            (name ^ ": DELETE requests")
            (if expect_delete then 1 else 0)
            (List.length deletes);
          Lwt.return_unit))

(* Real handlers + real DB + the HTTP stub, EARDE_TEST_DATABASE_URL-gated. *)
module Step7_deletion = struct
  let ( let* ) = Lwt.bind

  open Caqti_request.Infix

  let q_cleanup =
    List.map
      (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
      [ "DROP TRIGGER IF EXISTS step7_fail_insert ON posthog_person_deletion_jobs"
      ; "DROP FUNCTION IF EXISTS step7_fail_insert_fn()"
      ; "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id IN (SELECT 'user:' || u.id::text FROM users u WHERE u.username LIKE 'step7_%')"
      ; "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id LIKE 'user:9700%'"
        (* Orphaned jobs whose fixture user was hard-deleted by a test. *)
      ; "DELETE FROM posthog_person_deletion_jobs WHERE NOT EXISTS (SELECT 1 FROM users u WHERE 'user:' || u.id::text = posthog_person_deletion_jobs.distinct_id)"
        (* Anonymized fixture leftovers from interrupted runs: their usernames
           no longer match step7_/step6_, so match the anonymization rewrite. *)
      ; "DELETE FROM posthog_person_deletion_jobs WHERE distinct_id IN (SELECT 'user:' || u.id::text FROM users u WHERE u.password_hash = '' AND u.email LIKE 'deleted\\_%@earde.local')"
      ; "DELETE FROM users WHERE password_hash = '' AND email LIKE 'deleted\\_%@earde.local'"
      ; "DELETE FROM users WHERE username LIKE 'step7_%'"
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

  let q_insert_job =
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id) VALUES ($1) RETURNING id"

  let q_insert_job_aged =
    (Caqti_type.(t2 string int) ->! Caqti_type.int)
    "INSERT INTO posthog_person_deletion_jobs (distinct_id, created_at)
     VALUES ($1, NOW() - ($2 * INTERVAL '1 hour')) RETURNING id"

  let q_backdate_attempt =
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE posthog_person_deletion_jobs
     SET last_attempt_at = NOW() - INTERVAL '2 hours' WHERE id = $1"

  (* (status, attempts, last_error) by job id. *)
  let q_job_state =
    (Caqti_type.int ->? Caqti_type.(t3 string int (option string)))
    "SELECT status, attempts, last_error FROM posthog_person_deletion_jobs WHERE id = $1"

  let q_count_jobs_for =
    (Caqti_type.string ->! Caqti_type.int)
    "SELECT COUNT(*) FROM posthog_person_deletion_jobs WHERE distinct_id = $1"

  (* plpgsql failure trigger: a DB-only mechanism to force the job INSERT to
     fail inside the transaction — no production test hook. Quoted-body form
     avoids $$, which the Caqti query parser reserves. *)
  let q_create_fail_fn =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE OR REPLACE FUNCTION step7_fail_insert_fn() RETURNS trigger AS 'BEGIN RAISE EXCEPTION ''step7 forced failure''; END' LANGUAGE plpgsql"

  let q_create_fail_trigger =
    (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER step7_fail_insert BEFORE INSERT ON posthog_person_deletion_jobs FOR EACH ROW EXECUTE FUNCTION step7_fail_insert_fn()"

  let q_drop_fail_trigger =
    (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS step7_fail_insert ON posthog_person_deletion_jobs"

  let q_drop_fail_fn =
    (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS step7_fail_insert_fn()"

  let job_state c job_id =
    let (module C : Caqti_lwt.CONNECTION) = c in
    let* row = C.find_opt q_job_state job_id in
    or_fail "job state" row

  let atomic_case =
    db_case "atomic anonymize+enqueue: one idempotent job, user anonymized"
      (fun ~url:_ conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_atomic", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let* r = Earde.Db.anonymize_user_and_enqueue_posthog_deletion conn uid in
        let* job_id, distinct_id = or_fail_s "anonymize+enqueue" r in
        Alcotest.(check string) "immutable distinct id" did distinct_id;
        let* name = C.find_opt Step6_events.q_username_by_id uid in
        let* name = or_fail "row" name in
        Alcotest.(check (option string)) "anonymized"
          (Some (Printf.sprintf "[deleted_%d]" uid))
          name;
        let* state = job_state c job_id in
        (match state with
         | Some (status, attempts, last_error) ->
             Alcotest.(check string) "pending" "pending" status;
             Alcotest.(check int) "no attempts yet" 0 attempts;
             Alcotest.(check (option string)) "no error" None last_error
         | None -> Alcotest.fail "job row missing");
        (* Duplicate call converges on the SAME job — no competitors. *)
        let* r2 = Earde.Db.anonymize_user_and_enqueue_posthog_deletion conn uid in
        let* job_id2, _ = or_fail_s "second call" r2 in
        Alcotest.(check int) "same job id" job_id job_id2;
        let* count = C.find q_count_jobs_for did in
        let* count = or_fail "count" count in
        Alcotest.(check int) "exactly one job" 1 count;
        Lwt.return_unit)

  let rollback_case =
    db_case "forced job-insert failure rolls back the anonymization too"
      (fun ~url:_ conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_rollback", "x") in
        let* uid = or_fail "user" uid in
        let* r = C.exec q_create_fail_fn () in
        let* () = or_fail "create fn" r in
        let* r = C.exec q_create_fail_trigger () in
        let* () = or_fail "create trigger" r in
        let* result =
          Earde.Db.anonymize_user_and_enqueue_posthog_deletion conn uid
        in
        (match result with
         | Error _ -> ()
         | Ok _ -> Alcotest.fail "expected forced failure");
        let* r = C.exec q_drop_fail_trigger () in
        let* () = or_fail "drop trigger" r in
        let* r = C.exec q_drop_fail_fn () in
        let* () = or_fail "drop fn" r in
        (* BOTH changes rolled back: username untouched, no job row. *)
        let* name = C.find_opt Step6_events.q_username_by_id uid in
        let* name = or_fail "row" name in
        Alcotest.(check (option string)) "anonymization rolled back"
          (Some "step7_rollback") name;
        let* count = C.find q_count_jobs_for ("user:" ^ string_of_int uid) in
        let* count = or_fail "count" count in
        Alcotest.(check int) "no job row" 0 count;
        Lwt.return_unit)

  let claim_case =
    db_case "claim: attempts once, lease blocks, stale lease re-eligible"
      (fun ~url:_ conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* job_id = C.find q_insert_job "user:9700001" in
        let* job_id = or_fail "job" job_id in
        let* claimed = Earde.Db.claim_posthog_deletion_job conn job_id in
        let* claimed = or_fail_s "claim" claimed in
        Alcotest.(check (option string)) "claim returns the distinct id"
          (Some "user:9700001") claimed;
        let* state = job_state c job_id in
        (match state with
         | Some (_, attempts, _) ->
             Alcotest.(check int) "attempts incremented exactly once" 1 attempts
         | None -> Alcotest.fail "job vanished");
        (* Fresh lease: a concurrent attempt cannot claim it. *)
        let* again = Earde.Db.claim_posthog_deletion_job conn job_id in
        let* again = or_fail_s "concurrent claim" again in
        Alcotest.(check (option string)) "lease blocks reclaim" None again;
        (* Failure records the safe class; the stale lease re-opens the job. *)
        let* r = Earde.Db.fail_posthog_deletion_job conn job_id "timeout" in
        let* () = or_fail_s "mark failed" r in
        let* r = C.exec q_backdate_attempt job_id in
        let* () = or_fail "backdate" r in
        let* reclaimed = Earde.Db.claim_posthog_deletion_job conn job_id in
        let* reclaimed = or_fail_s "stale reclaim" reclaimed in
        Alcotest.(check (option string)) "stale lease eligible again"
          (Some "user:9700001") reclaimed;
        (* Completion clears the error and closes the job for good. *)
        let* r = Earde.Db.complete_posthog_deletion_job conn job_id in
        let* () = or_fail_s "complete" r in
        let* r = C.exec q_backdate_attempt job_id in
        let* () = or_fail "backdate completed" r in
        let* never = Earde.Db.claim_posthog_deletion_job conn job_id in
        let* never = or_fail_s "claim completed" never in
        Alcotest.(check (option string)) "completed jobs never claimed" None
          never;
        let* state = job_state c job_id in
        (match state with
         | Some (status, attempts, last_error) ->
             Alcotest.(check string) "completed" "completed" status;
             Alcotest.(check int) "two attempts total" 2 attempts;
             Alcotest.(check (option string)) "last_error cleared" None
               last_error
         | None -> Alcotest.fail "job vanished");
        Lwt.return_unit)

  let batch_case =
    db_case "batch claim: bounded and oldest-first" (fun ~url:_ conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* oldest = C.find q_insert_job_aged ("user:9700011", 3) in
        let* oldest = or_fail "oldest" oldest in
        let* middle = C.find q_insert_job_aged ("user:9700012", 2) in
        let* middle = or_fail "middle" middle in
        let* newest = C.find q_insert_job_aged ("user:9700013", 1) in
        let* newest = or_fail "newest" newest in
        let* claimed = Earde.Db.claim_posthog_deletion_batch conn ~limit:2 () in
        let* claimed = or_fail_s "batch claim" claimed in
        Alcotest.(check (list (pair int string)))
          "bound respected; oldest two, oldest first"
          [ (oldest, "user:9700011"); (middle, "user:9700012") ]
          claimed;
        let* state = job_state c newest in
        (match state with
         | Some (_, attempts, _) ->
             Alcotest.(check int) "unclaimed job untouched" 0 attempts
         | None -> Alcotest.fail "newest vanished");
        Lwt.return_unit)

  let batch_worker_case =
    db_case "process_batch: deleted+absent complete, failure stays pending"
      (fun ~url:_ conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* j_found = C.find q_insert_job_aged ("user:9700021", 3) in
        let* j_found = or_fail "found job" j_found in
        let* j_absent = C.find q_insert_job_aged ("user:9700022", 2) in
        let* j_absent = or_fail "absent job" j_absent in
        let* j_fail = C.find q_insert_job_aged ("user:9700023", 1) in
        let* j_fail = or_fail "fail job" j_fail in
        let handler (req : Api_stub.req) =
          if req.meth = "DELETE" then (204, "")
          else
            match List.assoc_opt "distinct_id" req.query with
            | Some [ "user:9700021" ] -> (200, results_body [ person_json stub_uuid ])
            | Some [ "user:9700022" ] -> (200, results_body [])
            | Some [ "user:9700023" ] -> (500, {|{"detail":"server error"}|})
            | _ -> (404, "")
        in
        let* base_url, _seen, stop = Api_stub.start handler in
        AnT.use_deletion_test_configuration ~ui_host:base_url
          ~project_id:(Some "42") ~personal_api_key:(Some deletion_test_key) ();
        Lwt.finalize
          (fun () ->
            (* Same worker the maintenance executable runs. *)
            let* summary =
              Earde.Posthog_deletion.process_batch
                ~claim:(fun () ->
                  Earde.Db.claim_posthog_deletion_batch conn ~limit:25 ())
                ~mark_completed:(fun job_id ->
                  Earde.Db.complete_posthog_deletion_job conn job_id)
                ~mark_failed:(fun job_id err ->
                  Earde.Db.fail_posthog_deletion_job conn job_id err)
                ()
            in
            let* summary = or_fail_s "process_batch" summary in
            Alcotest.(check int) "claimed" 3
              summary.Earde.Posthog_deletion.claimed;
            Alcotest.(check int) "completed" 2
              summary.Earde.Posthog_deletion.completed;
            Alcotest.(check int) "left pending" 1
              summary.Earde.Posthog_deletion.left_pending;
            let expect label job expected_status expected_error =
              let* state = job_state c job in
              match state with
              | Some (status, _, last_error) ->
                  Alcotest.(check string) (label ^ " status") expected_status
                    status;
                  Alcotest.(check (option string))
                    (label ^ " error") expected_error last_error;
                  Lwt.return_unit
              | None -> Alcotest.failf "%s vanished" label
            in
            let* () = expect "deleted person" j_found "completed" None in
            let* () = expect "already-absent person" j_absent "completed" None in
            let* () =
              expect "failed lookup" j_fail "pending" (Some "lookup_http_500")
            in
            Lwt.return_unit)
          (fun () ->
            AnT.clear_configuration_override ();
            stop ();
            Lwt.return_unit))

  (* Runs the real delete_account_handler, keeping sink + configuration
     installed until the post-response cleanup chain finishes ([done_pred]
     polls the durable job through the case's own connection). *)
  let run_delete_account ~url ~configure ?(consent = Some "granted")
      ?(on_capture = fun () -> ()) ~uid ~done_pred () =
    let payloads = ref [] in
    configure ();
    AnT.set_capture_sink (fun p ->
        payloads := !payloads @ [ p ];
        on_capture ());
    Lwt.finalize
      (fun () ->
        let pipeline =
          Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
          let* () = Dream.set_session_field req "user_id" (string_of_int uid) in
          let* () = Dream.set_session_field req "username" "step7_deleting" in
          let csrf = Dream.csrf_token req in
          Dream.set_body req (Step6_events.form_body [ ("dream.csrf", csrf) ]);
          Earde.Handlers.delete_account_handler req
        in
        let request =
          Dream.request ~method_:`POST ~target:"/delete-account"
            ~headers:
              ([ ("Content-Type", "application/x-www-form-urlencoded") ]
              @ Step6_events.consent_header consent)
            ""
        in
        let* response = pipeline request in
        let* () = wait_until ~label:"deletion cleanup chain" done_pred in
        Lwt.return (Dream.status_to_int (Dream.status response), !payloads))
      (fun () ->
        AnT.clear_capture_sink ();
        AnT.clear_configuration_override ();
        Lwt.return_unit)

  let job_completed conn did () =
    let* job = Earde.Db.get_posthog_deletion_job conn did in
    match job with
    | Ok (Some (_, "completed", _, _)) -> Lwt.return true
    | _ -> Lwt.return false

  let job_has_error conn did () =
    let* job = Earde.Db.get_posthog_deletion_job conn did in
    match job with
    | Ok (Some (_, _, _, Some _)) -> Lwt.return true
    | _ -> Lwt.return false

  let drop_job_and_user c ~did ~uid =
    let (module C : Caqti_lwt.CONNECTION) = c in
    let* job = Earde.Db.get_posthog_deletion_job c did in
    let* () =
      match job with
      | Ok (Some (job_id, _, _, _)) ->
          let* r = C.exec Step6_events.q_delete_job job_id in
          or_fail "drop job" r
      | _ -> Lwt.return_unit
    in
    let* r = C.exec Step6_events.q_delete_user uid in
    or_fail "drop user" r

  let stub_config base_url () =
    AnT.use_deletion_test_configuration ~ui_host:base_url
      ~project_id:(Some "42") ~personal_api_key:(Some deletion_test_key) ()

  let consented_flow_case =
    db_case "handler: personless metric + real-identity deletion job completes"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_flow", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let order = ref [] in
        let* base_url, seen, stop =
          Api_stub.start (fun req ->
              order := !order @ [ req.Api_stub.meth ];
              if req.Api_stub.meth = "GET" then
                (200, results_body [ person_json stub_uuid ])
              else (204, ""))
        in
        Lwt.finalize
          (fun () ->
            let* status, payloads =
              run_delete_account ~url ~configure:(stub_config base_url) ~uid
                ~on_capture:(fun () -> order := !order @ [ "capture" ])
                ~done_pred:(job_completed conn did) ()
            in
            Alcotest.(check bool) "response preserved (redirect)" true
              (is_redirect status);
            (match payloads with
             | [ p ] ->
                 Alcotest.(check string) "event" "account_deleted" (event_of p);
                 Alcotest.(check string) "constant non-user distinct id"
                   Earde.Analytics.account_deletion_distinct_id (distinct_of p);
                 Alcotest.(check bool)
                   "metric never mentions the deleted user" false
                   (contains (Yojson.Safe.to_string p) did)
             | l -> Alcotest.failf "expected 1 metric, got %d" (List.length l));
            (* HTTP request sequence only — the metric request happens to run
               first, but person-safety comes from the metric being personless
               by construction, NOT from this ordering (which proves nothing
               about PostHog's ingestion pipeline). *)
            Alcotest.(check (list string))
              "HTTP request sequence: metric, lookup, delete"
              [ "capture"; "GET"; "DELETE" ] !order;
            (* The Persons lookup and the durable job keep the REAL
               user:<database_id>. *)
            (match
               List.find_opt (fun (q : Api_stub.req) -> q.meth = "GET") !seen
             with
             | Some lookup ->
                 Alcotest.(check (option (list string)))
                   "Persons lookup uses the real user distinct id"
                   (Some [ did ])
                   (List.assoc_opt "distinct_id" lookup.Api_stub.query)
             | None -> Alcotest.fail "no Persons lookup recorded");
            let* job = Earde.Db.get_posthog_deletion_job conn did in
            let* job = or_fail_s "job" job in
            (match job with
             | Some (_, status, attempts, last_error) ->
                 Alcotest.(check string) "completed" "completed" status;
                 Alcotest.(check int) "one attempt" 1 attempts;
                 Alcotest.(check (option string)) "no error" None last_error
             | None -> Alcotest.fail "no job");
            let* name = C.find_opt Step6_events.q_username_by_id uid in
            let* name = or_fail "row" name in
            Alcotest.(check (option string)) "anonymized"
              (Some (Printf.sprintf "[deleted_%d]" uid))
              name;
            drop_job_and_user c ~did ~uid)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let denied_consent_case =
    db_case "handler: denied consent skips capture, still deletes remotely"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_denied", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let* base_url, seen, stop =
          Api_stub.start (fun req ->
              if req.Api_stub.meth = "GET" then (200, results_body [])
              else (404, ""))
        in
        Lwt.finalize
          (fun () ->
            let* status, payloads =
              run_delete_account ~url ~configure:(stub_config base_url)
                ~consent:(Some "denied") ~uid
                ~done_pred:(job_completed conn did) ()
            in
            Alcotest.(check bool) "redirects" true (is_redirect status);
            Alcotest.(check int) "no capture without consent" 0
              (List.length payloads);
            Alcotest.(check bool) "deletion still attempted" true
              (List.length !seen >= 1);
            drop_job_and_user c ~did ~uid)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let capture_failure_case =
    db_case "handler: capture transport failure still proceeds to deletion"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_capfail", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let* base_url, seen, stop =
          Api_stub.start (fun req ->
              if req.Api_stub.meth = "GET" then (200, results_body [])
              else (404, ""))
        in
        Lwt.finalize
          (fun () ->
            let* status, _payloads =
              run_delete_account ~url ~configure:(stub_config base_url) ~uid
                ~on_capture:(fun () -> failwith "capture transport down")
                ~done_pred:(job_completed conn did) ()
            in
            Alcotest.(check bool) "redirects" true (is_redirect status);
            Alcotest.(check bool) "deletion still attempted" true
              (List.length !seen >= 1);
            let* job = Earde.Db.get_posthog_deletion_job conn did in
            let* job = or_fail_s "job" job in
            (match job with
             | Some (_, status, _, _) ->
                 Alcotest.(check string) "completed despite capture failure"
                   "completed" status
             | None -> Alcotest.fail "no job");
            drop_job_and_user c ~did ~uid)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let missing_config_case =
    db_case "handler: missing configuration leaves a durable pending job"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_noconf", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let* status, payloads =
          run_delete_account ~url
            ~configure:AnT.use_enabled_test_configuration ~uid
            ~done_pred:(job_has_error conn did) ()
        in
        Alcotest.(check bool) "local deletion still succeeds" true
          (is_redirect status);
        Alcotest.(check int) "capture still emitted (consented)" 1
          (List.length payloads);
        let* job = Earde.Db.get_posthog_deletion_job conn did in
        let* job = or_fail_s "job" job in
        (match job with
         | Some (_, status, attempts, last_error) ->
             Alcotest.(check string) "pending" "pending" status;
             Alcotest.(check int) "one attempt" 1 attempts;
             Alcotest.(check (option string)) "safe marker"
               (Some "missing_configuration") last_error
         | None -> Alcotest.fail "no durable job");
        drop_job_and_user c ~did ~uid)

  let posthog_down_case =
    db_case "handler: PostHog failure never changes the product response"
      (fun ~url conn c ->
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("step7_phdown", "x") in
        let* uid = or_fail "user" uid in
        let did = "user:" ^ string_of_int uid in
        let* base_url, _seen, stop =
          Api_stub.start (fun _req -> (500, {|{"detail":"server error"}|}))
        in
        Lwt.finalize
          (fun () ->
            let* status, _payloads =
              run_delete_account ~url ~configure:(stub_config base_url) ~uid
                ~done_pred:(job_has_error conn did) ()
            in
            Alcotest.(check bool) "redirects" true (is_redirect status);
            let* job = Earde.Db.get_posthog_deletion_job conn did in
            let* job = or_fail_s "job" job in
            (match job with
             | Some (_, status, _, last_error) ->
                 Alcotest.(check string) "pending" "pending" status;
                 Alcotest.(check (option string)) "safe status class"
                   (Some "lookup_http_500") last_error
             | None -> Alcotest.fail "no job");
            let* name = C.find_opt Step6_events.q_username_by_id uid in
            let* name = or_fail "row" name in
            Alcotest.(check (option string)) "local deletion applied anyway"
              (Some (Printf.sprintf "[deleted_%d]" uid))
              name;
            drop_job_and_user c ~did ~uid)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let suite =
    [ atomic_case; rollback_case; claim_case; batch_case; batch_worker_case
    ; consented_flow_case; denied_consent_case; capture_failure_case
    ; missing_config_case; posthog_down_case
    ]
end

(* --- §13: durable private-community group-profile scrub ------------------ *)

(* When a community turns fully private, its previously sent community_name /
   community_slug must actually be REMOVED from the PostHog group profile via
   the documented private Groups API (find + delete_property {"$unset": …}).
   Stub-only cases run always; handler/job cases sit behind the DB gate. *)
module Group_cleanup = struct
  open Caqti_request.Infix

  let or_fail label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

  let or_fail_s label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label e

  let group_find_path = "/api/projects/42/groups/find/"
  let group_delete_path = "/api/projects/42/groups/delete_property/"

  (* Human-readable stand-ins that must NEVER appear in requests (beyond the
     find response we serve), job rows, or error strings. *)
  let secret_name = "Secret Club"
  let secret_slug = "secret-club"

  let group_props_body props =
    Printf.sprintf {|{"group_type_index": 0, "group_key": "k", "group_properties": {%s}}|}
      (String.concat ", "
         (List.map (fun (k, v) -> Printf.sprintf "%S: %S" k v) props))

  let unset_of_body body =
    match Yojson.Safe.from_string body with
    | exception _ -> None
    | `Assoc l -> (
        match List.assoc_opt "$unset" l with
        | Some (`String k) -> Some k
        | _ -> None)
    | _ -> None

  (* Stateful stub: find serves the current [props]; a successful
     delete_property removes the named key, mirroring real PostHog (which
     400s on an absent key — exactly why the client re-reads before
     deleting). *)
  let groups_handler ?(find_status = 200) ?(delete_status = 200) props
      (req : Api_stub.req) =
    if req.auth <> Some ("Bearer " ^ deletion_test_key) then
      (401, {|{"type":"authentication_error"}|})
    else if req.meth = "GET" && req.path = group_find_path then
      if find_status <> 200 then (find_status, {|{"detail":"not found"}|})
      else (200, group_props_body !props)
    else if req.meth = "POST" && req.path = group_delete_path then (
      match unset_of_body req.body with
      | Some key when delete_status = 200 && List.mem_assoc key !props ->
          props := List.remove_assoc key !props;
          (200, "{}")
      | Some key when delete_status <> 200 ->
          ignore key;
          (delete_status, "")
      | _ -> (400, {|{"attr":"$unset"}|}))
    else (404, {|{"detail":"not found"}|})

  let with_groups_stub handler f =
    Lwt_main.run
      (let ( let* ) = Lwt.bind in
       let* base_url, seen, stop = Api_stub.start handler in
       AnT.use_deletion_test_configuration ~ui_host:base_url
         ~project_id:(Some "42") ~personal_api_key:(Some deletion_test_key) ();
       Lwt.finalize
         (fun () -> f ~seen)
         (fun () ->
           AnT.clear_configuration_override ();
           stop ();
           Lwt.return_unit))

  let no_leak_in_requests (seen : Api_stub.req list) =
    List.iter
      (fun (r : Api_stub.req) ->
        let surface =
          r.path ^ " " ^ r.body ^ " "
          ^ String.concat " " (List.concat_map snd r.query)
        in
        if contains surface secret_name || contains surface secret_slug then
          Alcotest.failf "request leaked a community name/slug: %s" r.path)
      seen

  let full_props () =
    ref
      [ ("community_id", "9"); ("community_name", secret_name);
        ("community_slug", secret_slug); ("community_visibility", "private")
      ]

  let scrub_case =
    an_case "attempt: deletes exactly name+slug via $unset; rerun is a no-op"
      (fun () ->
        let props = full_props () in
        with_groups_stub (groups_handler props) (fun ~seen ->
            let ( let* ) = Lwt.bind in
            let* first =
              Earde.Posthog_deletion.attempt_group_cleanup
                ~group_key:"community:9"
            in
            Alcotest.(check (result unit string)) "first run" (Ok ()) first;
            let deletes =
              List.filter
                (fun (r : Api_stub.req) -> r.meth = "POST")
                !seen
            in
            Alcotest.(check (slist (option string) compare))
              "exactly the two closed scrub targets"
              [ Some "community_name"; Some "community_slug" ]
              (List.map (fun (r : Api_stub.req) -> unset_of_body r.body) deletes);
            List.iter
              (fun (r : Api_stub.req) ->
                Alcotest.(check (option (list string)))
                  "delete addressed by numeric key" (Some [ "community:9" ])
                  (List.assoc_opt "group_key" r.query);
                Alcotest.(check (option (list string)))
                  "community group type index" (Some [ "0" ])
                  (List.assoc_opt "group_type_index" r.query))
              deletes;
            (* Non-target properties survive; the group key itself stays. *)
            Alcotest.(check (list string)) "untouched non-targets"
              [ "community_id"; "community_visibility" ]
              (List.map fst !props);
            (* Idempotent rerun: nothing left to delete → find only. *)
            let before = List.length !seen in
            let* second =
              Earde.Posthog_deletion.attempt_group_cleanup
                ~group_key:"community:9"
            in
            Alcotest.(check (result unit string)) "rerun" (Ok ()) second;
            let extra =
              List.filteri (fun i _ -> i >= before) !seen
            in
            Alcotest.(check (list string)) "rerun performs find only"
              [ "GET" ]
              (List.map (fun (r : Api_stub.req) -> r.meth) extra);
            no_leak_in_requests deletes;
            Lwt.return_unit))

  let absent_case =
    an_case "attempt: group PostHog never saw (find 404) completes" (fun () ->
        let props = full_props () in
        with_groups_stub (groups_handler ~find_status:404 props) (fun ~seen ->
            let ( let* ) = Lwt.bind in
            let* r =
              Earde.Posthog_deletion.attempt_group_cleanup
                ~group_key:"community:9"
            in
            Alcotest.(check (result unit string)) "absent completes" (Ok ()) r;
            Alcotest.(check (list string)) "no delete attempted" [ "GET" ]
              (List.map (fun (q : Api_stub.req) -> q.meth) !seen);
            Lwt.return_unit))

  let failure_case =
    an_case "attempt: delete failure is a bounded class, never a name/slug"
      (fun () ->
        let props = full_props () in
        with_groups_stub (groups_handler ~delete_status:500 props)
          (fun ~seen ->
            let ( let* ) = Lwt.bind in
            let* r =
              Earde.Posthog_deletion.attempt_group_cleanup
                ~group_key:"community:9"
            in
            (match r with
            | Error cls ->
                Alcotest.(check string) "closed class" "group_delete_http_500"
                  cls
            | Ok () -> Alcotest.fail "expected failure");
            ignore seen;
            Lwt.return_unit))

  let missing_config_case =
    an_case "attempt: no private credentials -> bounded missing_configuration"
      (fun () ->
        AnT.use_enabled_test_configuration ();
        Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
            Alcotest.(check (result unit string))
              "missing configuration"
              (Error "missing_configuration")
              (Lwt_main.run
                 (Earde.Posthog_deletion.attempt_group_cleanup
                    ~group_key:"community:9"))))

  (* ---- DB-gated: transaction coupling, durability, retry, restore ------- *)

  let q_cleanup =
    List.map
      (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
      [ "DELETE FROM posthog_group_cleanup_jobs WHERE group_key IN (SELECT 'community:' || c.id::text FROM communities c WHERE c.slug LIKE 'grpclean-%')"
      ; "DELETE FROM posthog_group_cleanup_jobs WHERE group_key LIKE 'community:99912%'"
      ; "DELETE FROM communities WHERE slug LIKE 'grpclean-%'"
      ; "DELETE FROM posthog_group_cleanup_jobs WHERE NOT EXISTS (SELECT 1 FROM communities c WHERE 'community:' || c.id::text = posthog_group_cleanup_jobs.group_key)"
      ; "DELETE FROM users WHERE username LIKE 'grpclean_%'"
      ]

  let db_case name f =
    Alcotest.test_case name `Quick (fun () ->
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | None | Some "" -> Alcotest.skip ()
        | Some url ->
            Lwt_main.run
              (let ( let* ) = Lwt.bind in
               let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
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

  let q_insert_community =
    (Caqti_type.(t3 string string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility) VALUES ($1, $2, $3) RETURNING id"

  let q_visibility_of =
    (Caqti_type.int ->! Caqti_type.string)
    "SELECT visibility FROM communities WHERE id = $1"

  let q_insert_fake_job =
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO posthog_group_cleanup_jobs (group_key) VALUES ($1) RETURNING id"

  (* Runs update_community_visibility_handler like Step-7 runs the deletion
     handler: caller-chosen analytics configuration (so the async cleanup can
     reach a stub), completion awaited through [done_pred]. *)
  let run_visibility ~url ~configure ~uid ~slug ~value ~done_pred () =
    let ( let* ) = Lwt.bind in
    let payloads = ref [] in
    configure ();
    AnT.set_capture_sink (fun p -> payloads := !payloads @ [ p ]);
    Lwt.finalize
      (fun () ->
        let router =
          Dream.router
            [ Dream.post "/c/:slug/settings/visibility"
                Earde.Handlers.update_community_visibility_handler
            ]
        in
        let pipeline =
          Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
          let* () = Dream.set_session_field req "user_id" (string_of_int uid) in
          let* () = Dream.set_session_field req "username" "grpclean_admin" in
          let* () = Dream.set_session_field req "is_admin" "true" in
          let csrf = Dream.csrf_token req in
          Dream.set_body req
            (Step6_events.form_body
               [ ("dream.csrf", csrf); ("visibility", value) ]);
          router req
        in
        let request =
          Dream.request ~method_:`POST
            ~target:("/c/" ^ slug ^ "/settings/visibility")
            ~headers:
              ([ ("Content-Type", "application/x-www-form-urlencoded") ]
              @ Step6_events.consent_header (Some "granted"))
            ""
        in
        let* response = pipeline request in
        let* () = wait_until ~label:"group cleanup chain" done_pred in
        Lwt.return (Dream.status_to_int (Dream.status response), !payloads))
      (fun () ->
        AnT.clear_capture_sink ();
        AnT.clear_configuration_override ();
        Lwt.return_unit)

  let job_state conn key =
    let ( let* ) = Lwt.bind in
    let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
    or_fail_s "job state" job

  let job_completed conn key () =
    let ( let* ) = Lwt.bind in
    let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
    match job with
    | Ok (Some (_, "completed", _, _)) -> Lwt.return true
    | _ -> Lwt.return false

  let job_has_error conn key () =
    let ( let* ) = Lwt.bind in
    let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
    match job with
    | Ok (Some (_, _, _, Some _)) -> Lwt.return true
    | _ -> Lwt.return false

  let handler_flow_case =
    db_case "handler: public->private commits change + durable job; scrub runs"
      (fun ~url conn c ->
        let ( let* ) = Lwt.bind in
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("grpclean_admin", "x") in
        let* uid = or_fail "user" uid in
        let* cid =
          C.find q_insert_community ("grpclean-flow", secret_name, "public")
        in
        let* cid = or_fail "community" cid in
        let key = "community:" ^ string_of_int cid in
        let props =
          ref
            [ ("community_id", string_of_int cid);
              ("community_name", secret_name);
              ("community_slug", "grpclean-flow")
            ]
        in
        let* base_url, seen, stop = Api_stub.start (groups_handler props) in
        Lwt.finalize
          (fun () ->
            let* status, payloads =
              run_visibility ~url
                ~configure:(fun () ->
                  AnT.use_deletion_test_configuration ~ui_host:base_url
                    ~project_id:(Some "42")
                    ~personal_api_key:(Some deletion_test_key) ())
                ~uid ~slug:"grpclean-flow" ~value:"private"
                ~done_pred:(job_completed conn key) ()
            in
            Alcotest.(check bool) "redirects" true (status / 100 = 3);
            let* visibility = C.find q_visibility_of cid in
            let* visibility = or_fail "visibility" visibility in
            Alcotest.(check string) "visibility committed" "private" visibility;
            let* state = job_state conn key in
            (match state with
            | Some (_, s, attempts, last_error) ->
                Alcotest.(check string) "job completed" "completed" s;
                Alcotest.(check bool) "attempted" true (attempts >= 1);
                Alcotest.(check (option string)) "no error" None last_error
            | None -> Alcotest.fail "job row missing");
            (* Both stored identifiers really were unset over the API. *)
            Alcotest.(check (list string)) "profile scrubbed"
              [ "community_id" ]
              (List.map fst !props);
            no_leak_in_requests
              (List.filter (fun (r : Api_stub.req) -> r.meth = "POST") !seen);
            (* The consent-gated $groupidentify that rode along is already
               the private shape. *)
            (match
               List.find_opt
                 (fun p -> event_of p = "$groupidentify")
                 payloads
             with
            | Some gi ->
                Alcotest.(check (slist string compare)) "private group set"
                  [ "community_id"; "community_visibility" ]
                  (List.map fst (group_set_of gi))
            | None -> Alcotest.fail "no $groupidentify captured");
            Lwt.return_unit)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let handler_failure_case =
    db_case "handler: PostHog failure keeps the committed change, job pending"
      (fun ~url conn c ->
        let ( let* ) = Lwt.bind in
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("grpclean_admin2", "x") in
        let* uid = or_fail "user" uid in
        let* cid =
          C.find q_insert_community ("grpclean-down", secret_name, "public")
        in
        let* cid = or_fail "community" cid in
        let key = "community:" ^ string_of_int cid in
        let props =
          ref
            [ ("community_name", secret_name);
              ("community_slug", "grpclean-down")
            ]
        in
        let* base_url, _seen, stop =
          Api_stub.start (groups_handler ~delete_status:500 props)
        in
        Lwt.finalize
          (fun () ->
            let* status, _payloads =
              run_visibility ~url
                ~configure:(fun () ->
                  AnT.use_deletion_test_configuration ~ui_host:base_url
                    ~project_id:(Some "42")
                    ~personal_api_key:(Some deletion_test_key) ())
                ~uid ~slug:"grpclean-down" ~value:"private"
                ~done_pred:(job_has_error conn key) ()
            in
            Alcotest.(check bool) "product response preserved" true
              (status / 100 = 3);
            (* The transient PostHog failure did NOT roll anything back. *)
            let* visibility = C.find q_visibility_of cid in
            let* visibility = or_fail "visibility" visibility in
            Alcotest.(check string) "visibility still private" "private"
              visibility;
            let* state = job_state conn key in
            (match state with
            | Some (_, s, attempts, Some err) ->
                Alcotest.(check string) "durably pending" "pending" s;
                Alcotest.(check bool) "attempted" true (attempts >= 1);
                Alcotest.(check string) "bounded class only"
                  "group_delete_http_500" err;
                if contains err secret_name || contains err "grpclean-down"
                then Alcotest.fail "error leaked a name/slug"
            | _ -> Alcotest.fail "expected pending job with error");
            Lwt.return_unit)
          (fun () ->
            stop ();
            Lwt.return_unit))

  let enqueue_semantics_case =
    db_case "enqueue: converges while pending, re-arms after completion"
      (fun ~url:_ conn c ->
        let ( let* ) = Lwt.bind in
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* cid =
          C.find q_insert_community ("grpclean-rearm", secret_name, "public")
        in
        let* cid = or_fail "community" cid in
        let key = "community:" ^ string_of_int cid in
        let* r =
          Earde.Db.update_community_visibility_and_enqueue_group_cleanup conn
            cid Earde.Db.Community_private
        in
        let* updated, job1 = or_fail_s "first transition" r in
        Alcotest.(check bool) "community returned" true (updated <> None);
        let job1 = Option.get job1 in
        (* Duplicate transition converges on the SAME pending job. *)
        let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
            conn cid Earde.Db.Community_private
        in
        let* _, job2 = or_fail_s "duplicate transition" r in
        Alcotest.(check int) "same job" job1 (Option.get job2);
        (* ->public enqueues nothing (restore rides $groupidentify). *)
        let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
            conn cid Earde.Db.Community_public
        in
        let* _, job3 = or_fail_s "back to public" r in
        Alcotest.(check bool) "no job on ->public" true (job3 = None);
        (* Complete, then a NEW ->private transition re-arms it. *)
        let* m = Earde.Db.complete_posthog_group_cleanup_job conn job1 in
        let* () = or_fail_s "complete" m in
        let* r = Earde.Db.update_community_visibility_and_enqueue_group_cleanup
            conn cid Earde.Db.Community_private
        in
        let* _, job4 = or_fail_s "re-arm" r in
        Alcotest.(check int) "same row re-armed" job1 (Option.get job4);
        let* state = job_state conn key in
        (match state with
        | Some (_, s, attempts, last_error) ->
            Alcotest.(check string) "pending again" "pending" s;
            Alcotest.(check int) "counters reset" 0 attempts;
            Alcotest.(check (option string)) "diagnostics reset" None last_error
        | None -> Alcotest.fail "job row missing");
        Lwt.return_unit)

  let restore_case =
    db_case "handler: private->public restores name+slug, enqueues nothing"
      (fun ~url conn c ->
        let ( let* ) = Lwt.bind in
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* uid = C.find Step6_events.q_insert_user ("grpclean_admin3", "x") in
        let* uid = or_fail "user" uid in
        let* cid =
          C.find q_insert_community ("grpclean-back", secret_name, "private")
        in
        let* cid = or_fail "community" cid in
        let key = "community:" ^ string_of_int cid in
        let* status, payloads =
          run_visibility ~url
            ~configure:(fun () -> AnT.use_enabled_test_configuration ())
            ~uid ~slug:"grpclean-back" ~value:"public"
            ~done_pred:(fun () -> Lwt.return true)
            ()
        in
        Alcotest.(check bool) "redirects" true (status / 100 = 3);
        (match
           List.find_opt (fun p -> event_of p = "$groupidentify") payloads
         with
        | Some gi ->
            let set = group_set_of gi in
            Alcotest.(check (slist string compare))
              "public shape restored"
              [ "community_id"; "community_slug"; "community_name";
                "community_visibility" ]
              (List.map fst set);
            Alcotest.(check (option string)) "name restored"
              (Some secret_name)
              (match List.assoc_opt "community_name" set with
               | Some (`String v) -> Some v
               | _ -> None)
        | None -> Alcotest.fail "no $groupidentify captured");
        let* job = Earde.Db.get_posthog_group_cleanup_job conn key in
        let* job = or_fail_s "job lookup" job in
        Alcotest.(check bool) "no cleanup job for ->public" true (job = None);
        Lwt.return_unit)

  let batch_case =
    db_case "batch: bounded claim + shared maintenance worker completes"
      (fun ~url:_ conn c ->
        let ( let* ) = Lwt.bind in
        let (module C : Caqti_lwt.CONNECTION) = c in
        let* j1 = C.find q_insert_fake_job "community:999121" in
        let* j1 = or_fail "job1" j1 in
        let* j2 = C.find q_insert_fake_job "community:999122" in
        let* j2 = or_fail "job2" j2 in
        let* j3 = C.find q_insert_fake_job "community:999123" in
        let* j3 = or_fail "job3" j3 in
        let* claimed =
          Earde.Db.claim_posthog_group_cleanup_batch conn ~limit:2 ()
        in
        let* claimed = or_fail_s "bounded claim" claimed in
        Alcotest.(check (list int)) "bounded, oldest first" [ j1; j2 ]
          (List.map fst claimed);
        (* The third job processes through the SAME worker; a 404 find (group
           never existed) completes it. *)
        let props = ref [] in
        let handler = groups_handler ~find_status:404 props in
        let* base_url, _seen, stop = Api_stub.start handler in
        AnT.use_deletion_test_configuration ~ui_host:base_url
          ~project_id:(Some "42") ~personal_api_key:(Some deletion_test_key) ();
        Lwt.finalize
          (fun () ->
            let* summary =
              Earde.Posthog_deletion.process_group_batch
                ~claim:(fun () ->
                  Earde.Db.claim_posthog_group_cleanup_batch conn
                    ~lease_minutes:0 ~limit:10 ())
                ~mark_completed:(fun job_id ->
                  Earde.Db.complete_posthog_group_cleanup_job conn job_id)
                ~mark_failed:(fun job_id err ->
                  Earde.Db.fail_posthog_group_cleanup_job conn job_id err)
                ()
            in
            let* summary = or_fail_s "batch" summary in
            Alcotest.(check int) "all pending processed" 3
              summary.Earde.Posthog_deletion.claimed;
            Alcotest.(check int) "all completed" 3
              summary.Earde.Posthog_deletion.completed;
            let* state = job_state conn "community:999123" in
            (match state with
            | Some (id, s, _, _) ->
                Alcotest.(check int) "third job" j3 id;
                Alcotest.(check string) "completed" "completed" s
            | None -> Alcotest.fail "job row missing");
            Lwt.return_unit)
          (fun () ->
            AnT.clear_configuration_override ();
            stop ();
            Lwt.return_unit))

  let suite =
    [ scrub_case; absent_case; failure_case; missing_config_case
    ; handler_flow_case; handler_failure_case; enqueue_semantics_case
    ; restore_case; batch_case
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
let render_layout ?session_user ?analytics_community () =
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
                 Earde.Components.layout ~request:req ?analytics_community
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
    sections_enabled = true; visibility; indexable = true;
    is_network_community = false; onboarding_state = Earde.Db.Community_published;
    discoverable = true }

(* Renders the search results page through real session middleware (the page
   embeds a CSRF tag). Analytics configuration is whatever the caller
   installed. *)
let render_search ?(page = 1) ?(tab = "posts") ?(communities = [])
    ?(users = []) ?(posts = []) ?(comments = []) query =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           rendered :=
             Earde.Pages.search_results_page ~admin_usernames:[] [] page tab
               query communities users posts comments req;
           Dream.html "")
         (Dream.request ~method_:`GET ~target:"/search" ""))
  in
  !rendered

(* The full opening tag of the search analytics container, so leak assertions
   can look at exactly the markup that feeds analytics.js — the page itself
   legitimately echoes the query elsewhere (input value, pager hrefs). *)
let sr_analytics_tag html =
  match index_of html "<div id='sr-analytics'" with
  | None -> None
  | Some i -> (
      match String.index_from_opt html i '>' with
      | None -> None
      | Some j -> Some (String.sub html i (j - i + 1)))

(* --- Deployment environments: closed parsing, activation rules, envelope,
   and the credential/project preflight (local stub only, never PostHog). --- *)

(* Drives the pure validator with a COMPLETE dummy configuration by default,
   so each case flips exactly the value under test. Dummy credentials only. *)
let venv ?(enabled = "true") ?environment ?allow_development
    ?(project_token = "phc_test_token")
    ?(api_host = "https://eu.i.posthog.com")
    ?(ui_host = "https://eu.posthog.com") ?(project_id = "42")
    ?(personal_api_key = "phx_test_dummy") ?public_origin () =
  AnT.validate_environment_configuration ~enabled ?environment
    ?allow_development ~project_token ~api_host ~ui_host ~project_id
    ~personal_api_key ?public_origin ()

let check_env name ~expect_enabled ?expect_environment result =
  an_case name (fun () ->
      let enabled, environment, _diags = result in
      Alcotest.(check bool) (name ^ " enabled") expect_enabled enabled;
      match expect_environment with
      | None -> ()
      | Some expected ->
          Alcotest.(check (option string)) (name ^ " environment") expected
            environment)

(* Full production-shaped raw values (dummy secrets), installable so
   layout/browser-config tests exercise a validated production config. *)
let install_production_config () =
  AnT.install_validated_configuration ~enabled:"true"
    ~environment:"production" ~project_token:"phc_test_token"
    ~api_host:"https://eu.i.posthog.com" ~ui_host:"https://eu.posthog.com"
    ~project_id:"654321" ~personal_api_key:"phx_secret_test_value"
    ~public_origin:"https://earde.com" ()

(* Preflight stub plumbing (Api_stub, same as the deletion-client tests): the
   two documented private metadata endpoints, dummy values only. *)
let preflight_org = "0196aaaa-bbbb-cccc-dddd-eeeeffff0001"
let preflight_token = "phc_preflight_dummy_token"

let preflight_orgs_body ids =
  Printf.sprintf {|{"count": %d, "next": null, "results": [%s]}|}
    (List.length ids)
    (String.concat ", "
       (List.map (fun id -> Printf.sprintf {|{"id": %S, "name": "o"}|} id) ids))

let preflight_project_body ~id ~token =
  Printf.sprintf {|{"id": %d, "name": "p", "api_token": %S}|} id token

let preflight_handler ?(orgs_status = 200) ?orgs_override
    ?(project_status = 200) ?project_override () (req : Api_stub.req) =
  if req.Api_stub.path = "/api/organizations/" then
    ( orgs_status,
      match orgs_override with
      | Some body -> body
      | None -> preflight_orgs_body [ preflight_org ] )
  else if
    req.Api_stub.path
    = Printf.sprintf "/api/organizations/%s/projects/42/" preflight_org
  then
    ( project_status,
      match project_override with
      | Some body -> body
      | None -> preflight_project_body ~id:42 ~token:preflight_token )
  else (404, "{}")

(* Runs the real preflight against a local stub and returns
   (result, requests the stub saw). *)
let run_preflight ?(environment = An.Staging) handler =
  let ( let* ) = Lwt.bind in
  Lwt_main.run
    (let* base_url, seen, stop = Api_stub.start handler in
     AnT.use_preflight_test_configuration ~environment ~ui_host:base_url
       ~project_id:"42" ~personal_api_key:deletion_test_key
       ~project_token:preflight_token ();
     Lwt.finalize
       (fun () ->
         let* result = An.Preflight.run () in
         Lwt.return (result, !seen))
       (fun () ->
         AnT.clear_configuration_override ();
         stop ();
         Lwt.return_unit))

let preflight_class = function
  | Ok _ -> "verified"
  | Error (failure_class, _) -> failure_class

(* No request may leave the documented private metadata surface — above all,
   nothing may reach an event-ingestion path. *)
let check_preflight_requests_safe requests =
  List.iter
    (fun (req : Api_stub.req) ->
      Alcotest.(check bool)
        ("request stays on /api/organizations: " ^ req.Api_stub.path)
        true
        (String.length req.Api_stub.path >= 19
        && String.sub req.Api_stub.path 0 19 = "/api/organizations/");
      Alcotest.(check bool) "no ingestion path" false
        (contains req.Api_stub.path "/i/v0/e"
        || contains req.Api_stub.path "/capture"
        || contains req.Api_stub.path "/batch"))
    requests

let check_preflight name ?environment handler expected_class =
  an_case name (fun () ->
      let result, requests = run_preflight ?environment handler in
      Alcotest.(check string) name expected_class (preflight_class result);
      check_preflight_requests_safe requests;
      (* Neither credential may appear in any output, verified or failed. *)
      let rendered =
        match result with
        | Ok r ->
            String.concat "\n"
              (r.An.Preflight.report_project_id
               :: r.An.Preflight.report_token_fingerprint
               :: r.An.Preflight.report_notes)
        | Error (failure_class, detail) -> failure_class ^ "\n" ^ detail
      in
      Alcotest.(check bool) "output never contains the project token" false
        (contains rendered preflight_token);
      Alcotest.(check bool) "output never contains the personal key" false
        (contains rendered deletion_test_key))

(* GitHub onboarding configuration mode — pure parsing/serialization/policy.
   The parser is tested directly on string options; the process environment is
   never mutated. *)
module Ob = Earde.Project_onboarding

let ob_mode_str = function
  | Ob.Off -> "off"
  | Ob.Admins -> "admins"
  | Ob.Public -> "public"

let check_ob_parse name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name (ob_mode_str expected)
        (ob_mode_str (Ob.mode_of_string raw)))

let check_ob_string name expected mode =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (Ob.mode_to_string mode))

let check_ob_avail name expected mode ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (Ob.onboarding_available mode ~is_admin))

(* Legacy community-creation gate — pure request decisions, exercised directly
   so no Dream server or session middleware is needed. Anonymous visitors and
   authenticated non-admins both carry is_admin:false (the session flag is only
   ever set to "true" for authenticated global admins). *)
let ob_decision_str = function
  | Ob.Show_form -> "show_form"
  | Ob.Redirect_to_bring -> "redirect_to_bring"
  | Ob.Forbid -> "forbid"

let check_ob_legacy name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected
        (Ob.can_use_legacy_community_creation ~is_admin))

let check_ob_legacy_get name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name (ob_decision_str expected)
        (ob_decision_str (Ob.legacy_creation_get_decision ~is_admin)))

let check_ob_legacy_post name expected ~is_admin =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name (ob_decision_str expected)
        (ob_decision_str (Ob.legacy_creation_post_decision ~is_admin)))

(* /bring page renderer — pure (no server, no session middleware): the mode
   and viewer context are plain arguments and the layout renders without a
   request. Assertions are substring checks on stable copy/markup. *)
let render_bring ?user ~is_admin mode =
  Earde.Project_onboarding_pages.bring_page ?user ~is_admin ~mode ()

let bring_case name f = Alcotest.test_case name `Quick f

let bring_viewers =
  [ ("anonymous", None, false)
  ; ("member", Some "alice", false)
  ; ("admin", Some "root", true)
  ]

let bring_modes = [ ("off", Ob.Off); ("admins", Ob.Admins); ("public", Ob.Public) ]

(* Shared invariants: every mode × viewer explains that members do not need
   GitHub, describes both destinations for a verified project, stays noindex,
   and never emits the forbidden "Official ..." claims or links to routes
   outside this slice (GitHub install/callback, /new-community, /projects/new). *)
let bring_shared_case (mode_name, mode) (viewer_name, user, is_admin) =
  bring_case
    (Printf.sprintf "%s mode, %s viewer" mode_name viewer_name)
    (fun () ->
      let html = render_bring ?user ~is_admin mode in
      let must s = Alcotest.(check bool) ("contains: " ^ s) true (contains html s) in
      let must_not s =
        Alcotest.(check bool) ("must not contain: " ^ s) false (contains html s)
      in
      must "do not need a GitHub account";
      must "create its own Earde community";
      must "existing broader Earde community";
      must "noindex";
      must_not "Official community";
      must_not "Official home";
      must_not "/integrations/github";
      must_not "/new-community";
      must_not "/projects/new")

let bring_shared_cases =
  List.concat_map
    (fun mode -> List.map (bring_shared_case mode) bring_viewers)
    bring_modes

(* Ordinary navigation entry points — pure renderers (no server, no session
   middleware). Generic community creation is admin-only and reachable only by
   typing /new-community manually; normal navigation (topbar, rail, sidebars,
   creation-flow fallbacks) must advertise the /bring onboarding entry point
   instead — including for global admins. *)
let nav_case name f = Alcotest.test_case name `Quick f

let nav_check html =
  let must s = Alcotest.(check bool) ("contains: " ^ s) true (contains html s) in
  let must_not s =
    Alcotest.(check bool) ("must not contain: " ^ s) false (contains html s)
  in
  (must, must_not)

let nav_test_community : Earde.Db.community =
  { id = 1; slug = "ocaml"; name = "OCaml"; description = None; rules = None
  ; avatar_url = None; banner_url = None; allow_downvotes = true
  ; sections_enabled = true; visibility = Earde.Db.Community_public
  ; indexable = true; is_network_community = false
  ; onboarding_state = Earde.Db.Community_published; discoverable = true }

let nav_entry_cases =
  [ nav_case "logged-in topbar advertises /bring" (fun () ->
        let html =
          Earde.Components.render_app_topbar ~user:"alice" ~is_admin:false () in
        let must, must_not = nav_check html in
        must "href='/bring'";
        must "Connect a project";
        must_not "Start community";
        must_not "/new-community")
  ; nav_case "logged-in topbar keeps unrelated actions" (fun () ->
        let html =
          Earde.Components.render_app_topbar ~user:"alice" ~is_admin:false () in
        let must, _ = nav_check html in
        must "href='/notifications'";
        must "notif-badge";
        must "href='/settings'";
        must "Log out")
  ; nav_case "admin topbar offers /bring, not the legacy route" (fun () ->
        let html =
          Earde.Components.render_app_topbar ~user:"root" ~is_admin:true () in
        let must, must_not = nav_check html in
        must "href='/admin'";
        must "href='/bring'";
        must_not "/new-community")
  ; nav_case "anonymous topbar unchanged" (fun () ->
        let html = Earde.Components.render_app_topbar ~is_admin:false () in
        let must, must_not = nav_check html in
        must "href='/login'";
        must "href='/signup'";
        must_not "/new-community";
        must_not "/bring")
  ; nav_case "empty communities sidebar links to /bring" (fun () ->
        let html =
          Earde.Components.left_sidebar ~user:"alice" ~moderated_communities:[] [] in
        let must, must_not = nav_check html in
        must "href='/bring'";
        must "Connect a project";
        must_not "/new-community")
  ; nav_case "anonymous sidebar unchanged" (fun () ->
        let html = Earde.Components.left_sidebar ~moderated_communities:[] [] in
        let must, must_not = nav_check html in
        must "href='/signup'";
        must_not "/new-community")
  ; nav_case "app shell rail add-tile targets /bring" (fun () ->
        let html =
          Earde.Components.feed_shell ~user:"alice" ~title:"Feed" ~main:"" () in
        let must, must_not = nav_check html in
        must "cs-rail-add' href='/bring'";
        must_not "/new-community")
  ; nav_case "choose-community fallback connects a project" (fun () ->
        let html =
          Earde.Pages.choose_community_page ~user:"alice" [ nav_test_community ] in
        let must, must_not = nav_check html in
        must "href='/bring'";
        must "Connect a project";
        must "Post here";
        must_not "/new-community";
        must_not "Start a community")
  ; nav_case "admin creation form renderer retained" (fun () ->
        (* Compile-time retention check: the admin-only legacy form (and its
           request-taking signature) must not be removed by this de-linking. *)
        ignore (Earde.Pages.new_community_form : ?user:string -> Dream.request -> string))
  ]

(* === Network-community lifecycle: pure domain rules (no DB, no IO) ===
   Publication mode parsing is exact-match (no trim/casefold); publication
   yields a closed configuration record; drafts must not leak through
   indexing/discovery; a published network community can never go private. *)
module NC = Earde.Network_communities

let nc_case name f = Alcotest.test_case name `Quick f

let nc_mode_str = function NC.Public -> "Public" | NC.Unlisted -> "Unlisted"

let nc_parse_ok name expected input =
  nc_case name (fun () ->
      match NC.publication_mode_of_string input with
      | Ok m ->
          Alcotest.(check string) "parsed mode" (nc_mode_str expected)
            (nc_mode_str m)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let nc_parse_err name input =
  nc_case name (fun () ->
      match NC.publication_mode_of_string input with
      | Ok m -> Alcotest.failf "expected Error, got Ok %s" (nc_mode_str m)
      | Error _ -> ())

(* Field-by-field configuration check; onboarding_state is always Published. *)
let nc_check_config ~visibility ~indexable ~discoverable
    (c : NC.publication_configuration) =
  Alcotest.(check string) "visibility"
    (Earde.Db.community_visibility_to_string visibility)
    (Earde.Db.community_visibility_to_string c.visibility);
  Alcotest.(check bool) "indexable" indexable c.indexable;
  Alcotest.(check bool) "discoverable" discoverable c.discoverable;
  Alcotest.(check string) "onboarding_state" "published"
    (Earde.Db.string_of_community_onboarding_state c.onboarding_state)

let nc_publish_err name expected ~is_network_community ~onboarding_state mode =
  nc_case name (fun () ->
      match NC.publish ~is_network_community ~onboarding_state mode with
      | Ok _ -> Alcotest.fail "expected publication to be rejected"
      | Error e ->
          Alcotest.(check string) "error"
            (NC.string_of_publication_error expected)
            (NC.string_of_publication_error e))

let nc_vis_case name expected ~is_network_community ~onboarding_state
    ~requested_visibility =
  nc_case name (fun () ->
      Alcotest.(check bool) "allowed" expected
        (NC.visibility_change_allowed ~is_network_community ~onboarding_state
           ~requested_visibility))

let nc_valid_case name expected ~is_network_community ~onboarding_state
    ~visibility ~indexable ~discoverable =
  nc_case name (fun () ->
      Alcotest.(check bool) "valid" expected
        (NC.lifecycle_state_valid ~is_network_community ~onboarding_state
           ~visibility ~indexable ~discoverable))

(* === Settings-flow lifecycle gate (Handlers.visibility_update_rejection) ===
   The exact decision function update_community_visibility_handler consults on
   the loaded record before writing. These prove the settings flow invokes the
   lifecycle rule; the exhaustive matrix lives in network_visibility_change. *)
let vis_gate_community ~is_network_community ~onboarding_state ~visibility :
    Earde.Db.community =
  { id = 7; slug = "gate"; name = "Gate"; description = None; rules = None
  ; avatar_url = None; banner_url = None; allow_downvotes = true
  ; sections_enabled = false; visibility; indexable = true
  ; is_network_community; onboarding_state; discoverable = true }

let vis_gate_allowed name ~is_network_community ~onboarding_state ~visibility
    ~requested_visibility =
  nc_case name (fun () ->
      let community =
        vis_gate_community ~is_network_community ~onboarding_state ~visibility in
      match
        Earde.Handlers.visibility_update_rejection community
          ~requested_visibility
      with
      | None -> ()
      | Some m -> Alcotest.failf "expected update to proceed, got %S" m)

let vis_gate_rejected name ~is_network_community ~onboarding_state ~visibility
    ~requested_visibility =
  nc_case name (fun () ->
      let community =
        vis_gate_community ~is_network_community ~onboarding_state ~visibility in
      match
        Earde.Handlers.visibility_update_rejection community
          ~requested_visibility
      with
      | Some message ->
          Alcotest.(check string) "rejection copy"
            "Published network communities must remain public. Private \
             channels and sections may still be used."
            message
      | None -> Alcotest.fail "expected rejection, update was allowed")

(* === GitHub onboarding domain: pure closed types (no DB, no IO) ===
   Exact-match parsing mirrors the schema enums; Revoked is a terminal
   installation status; revoked_at coherence mirrors the schema contract
   (forbidden on non-revoked rows, optional on revoked ones). *)
module GO = Earde.Github_onboarding

let go_case name f = Alcotest.test_case name `Quick f

let go_account_str = function
  | GO.User -> "User"
  | GO.Organization -> "Organization"

let go_account_ok name expected input =
  go_case name (fun () ->
      match GO.account_type_of_string input with
      | Ok a ->
          Alcotest.(check string) "parsed account type"
            (go_account_str expected) (go_account_str a)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_account_err name input =
  go_case name (fun () ->
      match GO.account_type_of_string input with
      | Ok a -> Alcotest.failf "expected Error, got Ok %s" (go_account_str a)
      | Error _ -> ())

let go_status_str = function
  | GO.Active -> "Active"
  | GO.Revoked -> "Revoked"
  | GO.Inaccessible -> "Inaccessible"

let go_status_ok name expected input =
  go_case name (fun () ->
      match GO.installation_status_of_string input with
      | Ok s ->
          Alcotest.(check string) "parsed status" (go_status_str expected)
            (go_status_str s)
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_status_err name input =
  go_case name (fun () ->
      match GO.installation_status_of_string input with
      | Ok s -> Alcotest.failf "expected Error, got Ok %s" (go_status_str s)
      | Error _ -> ())

let go_flow_ok name input =
  go_case name (fun () ->
      match GO.flow_of_string input with
      | Ok GO.Project_onboarding -> ()
      | Error e -> Alcotest.failf "expected Ok, got Error %S" e)

let go_flow_err name input =
  go_case name (fun () ->
      match GO.flow_of_string input with
      | Ok GO.Project_onboarding ->
          Alcotest.fail "expected Error, got Ok Project_onboarding"
      | Error _ -> ())

let go_transition name expected ~from_ ~to_ =
  go_case name (fun () ->
      Alcotest.(check bool) "allowed" expected
        (GO.installation_transition_allowed ~from_ ~to_))

let go_revoked_at name expected ~status ~has_revoked_at =
  go_case name (fun () ->
      Alcotest.(check bool) "allowed" expected
        (GO.revoked_at_allowed ~status ~has_revoked_at))

(* === GitHub onboarding crypto: canonical tokens and lookup hashes ===
   Structural assertions only — generated raw state/binding values are never
   printed, and rejection cases match the payload-free [Invalid_format]
   without echoing the rejected input. *)
module GOC = Earde.Github_onboarding_crypto

let goc_case = go_case

(* Canonical means: decodes, is exactly 32 bytes, and re-encodes to the
   same string (so unpadded, no whitespace, zero trailing bits). *)
let goc_canonical_32 encoded =
  match Dream.from_base64url encoded with
  | None -> false
  | Some raw ->
      String.length raw = 32 && String.equal (Dream.to_base64url raw) encoded

(* Deterministic 32-byte fixture token; safe to print, unlike generated
   material. *)
let goc_fixture byte = Dream.to_base64url (String.make 32 byte)

let goc_state_ok name input =
  goc_case name (fun () ->
      match GOC.state_of_callback input with
      | Ok state ->
          Alcotest.(check string) "round-trips" input
            (GOC.state_to_string state)
      | Error GOC.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let goc_state_err name input =
  goc_case name (fun () ->
      match GOC.state_of_callback input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GOC.Invalid_format -> ())

let goc_binding_ok name input =
  goc_case name (fun () ->
      match GOC.session_binding_of_string input with
      | Ok binding ->
          Alcotest.(check string) "round-trips" input
            (GOC.session_binding_to_string binding)
      | Error GOC.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let goc_binding_err name input =
  goc_case name (fun () ->
      match GOC.session_binding_of_string input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GOC.Invalid_format -> ())

let goc_state_exn input =
  match GOC.state_of_callback input with
  | Ok state -> state
  | Error GOC.Invalid_format -> Alcotest.fail "fixture did not parse as state"

let goc_binding_exn input =
  match GOC.session_binding_of_string input with
  | Ok binding -> binding
  | Error GOC.Invalid_format ->
      Alcotest.fail "fixture did not parse as session binding"

let goc_state_hash input =
  GOC.state_hash_to_string (GOC.hash_state (goc_state_exn input))

let goc_binding_hash input =
  GOC.session_binding_hash_to_string
    (GOC.hash_session_binding (goc_binding_exn input))

let goc_is_hex64 s =
  String.length s = 64
  && String.for_all (function '0' .. '9' | 'a' .. 'f' -> true | _ -> false) s

let goc_contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec at i =
    i + n <= h && (String.equal (String.sub haystack i n) needle || at (i + 1))
  in
  n > 0 && at 0

(* === GitHub onboarding PKCE: verifier and S256 challenge ===
   Same discipline as the GOC suites: structural assertions for generated
   material (never printed), fixed printable fixtures for parse/challenge
   cases, payload-free Invalid_format on rejection. Challenge equality
   checks use fixed verifiers, so determinism is proven directly rather
   than probabilistically. *)
module GPK = Earde.Github_onboarding_pkce

let gpk_case = go_case

let gpk_verifier_exn input =
  match GPK.verifier_of_string input with
  | Ok verifier -> verifier
  | Error GPK.Invalid_format ->
      Alcotest.fail "fixture did not parse as verifier"

let gpk_verifier_ok name input =
  gpk_case name (fun () ->
      match GPK.verifier_of_string input with
      | Ok verifier ->
          Alcotest.(check string) "round-trips" input
            (GPK.verifier_to_string verifier)
      | Error GPK.Invalid_format -> Alcotest.fail "expected Ok, got Error")

let gpk_verifier_err name input =
  gpk_case name (fun () ->
      match GPK.verifier_of_string input with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error GPK.Invalid_format -> ())

let gpk_challenge input =
  GPK.challenge_to_string (GPK.challenge_of_verifier (gpk_verifier_exn input))

(* === GitHub App public configuration (Github_app_config) ===
   Pure [of_values] coverage only — the real process environment is never
   mutated. Rejection assertions compare the closed error variants, and the
   privacy cases pin that diagnostics never echo a supplied value. *)
module GAC = Earde.Github_app_config

let gac_case = go_case

let gac_error =
  Alcotest.testable
    (fun ppf e -> Format.pp_print_string ppf (GAC.string_of_error e))
    ( = )

(* Baseline valid production configuration; each case overrides one field. *)
let gac_of_values ?(origin = Some "https://earde.com")
    ?(slug = Some "earde-connect") ?(client = Some "Iv1.8a61f9b3a7aba766")
    ?(setup = Some "https://earde.com/integrations/github/install/return")
    ?(callback =
      Some "https://earde.com/integrations/github/authorize/callback") () =
  GAC.of_values ~public_origin:origin ~app_slug:slug ~client_id:client
    ~setup_url:setup ~callback_url:callback

let gac_ok_exn label result =
  match result with
  | Ok config -> config
  | Error e ->
      Alcotest.failf "%s: rejected with %s" label (GAC.string_of_error e)

let gac_err name expected result =
  gac_case name (fun () ->
      match result with
      | Ok _ -> Alcotest.fail "expected Error, got Ok"
      | Error e -> Alcotest.check gac_error "error" expected e)

(* === GitHub onboarding URLs (Github_onboarding_urls) ===
   Structural assertions over [Uri.of_string]-parsed output. The fixtures
   are deterministic printable values, but state and challenge comparisons
   still use boolean checks so no raw token material reaches test output. *)
module GOU = Earde.Github_onboarding_urls

let gou_case = go_case

let gou_config () = gac_ok_exn "url fixture config" (gac_of_values ())

let gou_state_string = goc_fixture 'S'

let gou_state () = goc_state_exn gou_state_string

(* The verifier string is retained so leakage tests can assert it never
   appears in a URL; only its derived challenge may. *)
let gou_verifier_string = goc_fixture 'V'

let gou_challenge () =
  GPK.challenge_of_verifier (gpk_verifier_exn gou_verifier_string)

let gou_installation () =
  GOU.installation_url (gou_config ()) ~state:(gou_state ())

let gou_authorization ?config () =
  let config = match config with Some c -> c | None -> gou_config () in
  GOU.authorization_url config ~state:(gou_state ())
    ~code_challenge:(gou_challenge ())

let gou_keys uri = List.map fst (Uri.query uri)

let gou_entries key uri =
  List.filter (fun (k, _) -> String.equal k key) (Uri.query uri)

(* The decoded value of [key], requiring the key to appear exactly once
   with exactly one value. *)
let gou_single label key uri =
  match gou_entries key uri with
  | [ (_, [ v ]) ] -> v
  | _ -> Alcotest.failf "%s: expected exactly one %s value" label key

let gou_authorization_keys =
  [ "client_id"; "redirect_uri"; "state"; "code_challenge";
    "code_challenge_method" ]

(* === GitHub onboarding state issuance (Github_onboarding_state_store) ===
   The security contract lives in the SQL — only hashes reach the table,
   expiry comes from Postgres NOW(), issuance never disturbs earlier states —
   so only a DB-backed check can pin it down. Same EARDE_TEST_DATABASE_URL
   opt-in gate as Mod_scope; each fixture-writing case runs inside a
   transaction that is rolled back (the FK-failure case relies on the failed
   INSERT's own atomicity instead), so no rows outlive a run. Raw generated
   state/binding values never reach assertion messages or test output. *)
module Gh_state_store = struct
  let ( let* ) = Lwt.bind

  open Caqti_request.Infix

  module Store = Earde.Github_onboarding_state_store

  (* attach_error shares constructor names with issue_error, so both
     stringifiers need explicit domains. *)
  let issue_error_str : Store.issue_error -> string = function
    | Store.Invalid_user_id -> "Invalid_user_id"
    | Store.Storage_error -> "Storage_error"

  let attach_error_str : Store.attach_error -> string = function
    | Store.Invalid_user_id -> "Invalid_user_id"
    | Store.Invalid_pending_installation_id ->
        "Invalid_pending_installation_id"
    | Store.State_unavailable -> "State_unavailable"
    | Store.Storage_error -> "Storage_error"

  let or_fail label = function
    | Ok v -> Lwt.return v
    | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

  let q_insert_user =
    (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ('ghstate_user', 'ghstate_user@test.invalid', 'x', TRUE) RETURNING id"

  (* Everything stored for a user's states: the three text columns (no other
     column in the table can hold token material), NULL-ness of the two
     lifecycle columns, and the Postgres-computed expiry delta. *)
  let q_rows_for_user =
    (Caqti_type.(int ->* t2 (t3 string string string) (t3 bool bool float)))
    "SELECT state_hash, session_binding_hash, flow,
            pending_github_installation_id IS NULL,
            consumed_at IS NULL,
            EXTRACT(EPOCH FROM (expires_at - created_at))::float8
     FROM github_onboarding_states WHERE user_id = $1 ORDER BY id"

  let q_count_for_user =
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM github_onboarding_states WHERE user_id = $1"

  (* A positive user id guaranteed absent from users, for the FK-failure
     case. *)
  let q_absent_user_id =
    (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COALESCE(MAX(id), 0) + 1000000 FROM users"

  let db_case name f =
    Alcotest.test_case name `Quick (fun () ->
        match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
        | None | Some "" -> Alcotest.skip ()
        | Some url ->
            Lwt_main.run
              (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
               let* conn = or_fail "connect" conn in
               f conn))

  (* Fixture user and issued rows live only inside this transaction; the
     rollback runs even when an assertion fails mid-case. *)
  let tx_case name f =
    db_case name (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* r = C.start () in
        let* () = or_fail "begin" r in
        Lwt.finalize
          (fun () -> f conn)
          (fun () ->
            let* r = C.rollback () in
            let* () = or_fail "rollback" r in
            Lwt.return_unit))

  let issue_ok conn ~user_id ~session_binding_hash =
    let* r =
      Store.issue conn ~user_id ~session_binding_hash
        ~flow:GO.Project_onboarding
    in
    match r with
    | Ok state -> Lwt.return state
    | Error e -> Alcotest.failf "issue: %s" (issue_error_str e)

  let single_issue_case =
    tx_case "issue: one row, hashes only, Postgres 15-minute expiry"
      (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid = C.find q_insert_user () in
        let* uid = or_fail "user" uid in
        let binding = GOC.generate_session_binding () in
        let binding_hash = GOC.hash_session_binding binding in
        let* state =
          issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
        in
        let raw_state = GOC.state_to_string state in
        (match GOC.state_of_callback raw_state with
        | Ok _ -> ()
        | Error GOC.Invalid_format ->
            Alcotest.fail "issued state does not pass state_of_callback");
        let* rows = C.collect_list q_rows_for_user uid in
        let* rows = or_fail "rows" rows in
        (match rows with
        | [ ( (state_hash, stored_binding_hash, flow),
              (pending_null, consumed_null, ttl) ) ] ->
            Alcotest.(check string) "stored state_hash is the derived hash"
              (GOC.state_hash_to_string (GOC.hash_state state))
              state_hash;
            Alcotest.(check string) "stored binding hash is the supplied hash"
              (GOC.session_binding_hash_to_string binding_hash)
              stored_binding_hash;
            Alcotest.(check string) "flow" "project_onboarding" flow;
            let text_columns =
              String.concat "|" [ state_hash; stored_binding_hash; flow ]
            in
            Alcotest.(check bool) "raw state absent from text columns" false
              (goc_contains ~needle:raw_state text_columns);
            Alcotest.(check bool) "raw binding absent from text columns" false
              (goc_contains
                 ~needle:(GOC.session_binding_to_string binding)
                 text_columns);
            Alcotest.(check bool) "pending installation id NULL" true
              pending_null;
            Alcotest.(check bool) "consumed_at NULL" true consumed_null;
            Alcotest.(check bool) "expiry ~15 minutes after creation" true
              (Float.abs (ttl -. 900.) <= 5.)
        | rows ->
            Alcotest.failf "expected exactly one row, found %d"
              (List.length rows));
        Lwt.return_unit)

  let multiplicity_case =
    tx_case "issue: repeat issuance leaves independent unconsumed rows"
      (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid = C.find q_insert_user () in
        let* uid = or_fail "user" uid in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let* first =
          issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
        in
        let* second =
          issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
        in
        let hash_of s = GOC.state_hash_to_string (GOC.hash_state s) in
        let* rows = C.collect_list q_rows_for_user uid in
        let* rows = or_fail "rows" rows in
        (match rows with
        | [ ((hash_a, _, _), (_, consumed_a, _));
            ((hash_b, _, _), (_, consumed_b, _)) ] ->
            Alcotest.(check bool) "first row unconsumed" true consumed_a;
            Alcotest.(check bool) "second row unconsumed" true consumed_b;
            Alcotest.(check bool) "distinct state hashes" false
              (String.equal hash_a hash_b);
            (* Rows are id-ordered, so they pair with issuance order. *)
            Alcotest.(check string) "first row is the first state"
              (hash_of first) hash_a;
            Alcotest.(check string) "second row is the second state"
              (hash_of second) hash_b
        | rows ->
            Alcotest.failf "expected exactly two rows, found %d"
              (List.length rows));
        Lwt.return_unit)

  let invalid_user_case =
    db_case "issue: non-positive user ids rejected before SQL" (fun conn ->
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let check_rejected uid =
          let* r =
            Store.issue conn ~user_id:uid ~session_binding_hash:binding_hash
              ~flow:GO.Project_onboarding
          in
          match r with
          | Error Store.Invalid_user_id -> Lwt.return_unit
          | Error Store.Storage_error ->
              Alcotest.failf "user id %d: expected Invalid_user_id, got \
                             Storage_error" uid
          | Ok _ ->
              Alcotest.failf "user id %d: expected Invalid_user_id, got Ok"
                uid
        in
        let* () = check_rejected 0 in
        check_rejected (-1))

  (* Autocommit on purpose: the failed INSERT is atomic on its own, which is
     exactly the "no partial row" contract; a wrapping transaction would be
     aborted by the FK error and block the follow-up count. *)
  let missing_user_case =
    db_case "issue: nonexistent user id is Storage_error, no partial row"
      (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* ghost = C.find q_absent_user_id () in
        let* ghost = or_fail "absent user id" ghost in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let* r =
          Store.issue conn ~user_id:ghost ~session_binding_hash:binding_hash
            ~flow:GO.Project_onboarding
        in
        (match r with
        | Error Store.Storage_error -> ()
        | Error Store.Invalid_user_id ->
            Alcotest.fail "expected Storage_error, got Invalid_user_id"
        | Ok _ -> Alcotest.fail "expected Storage_error, got Ok");
        let* count = C.find q_count_for_user ghost in
        let* count = or_fail "count" count in
        Alcotest.(check int) "no partial row" 0 count;
        Lwt.return_unit)

  (* === attach_pending_installation ===
     Same gate and rollback discipline as issuance. Flow mismatch has no
     dedicated case: the model has a single flow variant and the schema
     CHECK forbids any other flow string, so a mismatched-flow row cannot
     exist even as a SQL fixture; the flow comparison sits in the same
     conjunctive WHERE as the user/binding columns that are tested. *)

  let q_insert_second_user =
    (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ('ghstate_user_b', 'ghstate_user_b@test.invalid', 'x', TRUE) RETURNING id"

  (* Full persisted row for attach assertions: the three text columns, the
     actual pending id, consumed_at NULL-ness, and the creation/expiry
     epochs — pinning that attachment changes nothing else. *)
  let q_attach_rows_for_user =
    (Caqti_type.(int ->* t2 (t3 string string string)
                          (t3 (option int64) bool (t2 float float))))
    "SELECT state_hash, session_binding_hash, flow,
            pending_github_installation_id,
            consumed_at IS NULL,
            EXTRACT(EPOCH FROM created_at)::float8,
            EXTRACT(EPOCH FROM expires_at)::float8
     FROM github_onboarding_states WHERE user_id = $1 ORDER BY id"

  (* Fixture: push a user's states into the past. created_at moves too,
     both because expires_at > created_at is a CHECK and because NOW() is
     frozen for the whole rolled-back transaction — merely shrinking the
     TTL could never make a row expired inside the test. *)
  let q_expire_states_for_user =
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE github_onboarding_states
     SET created_at = NOW() - INTERVAL '1 hour',
         expires_at = NOW() - INTERVAL '30 minutes'
     WHERE user_id = $1"

  let q_consume_states_for_user =
    (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE github_onboarding_states SET consumed_at = NOW()
     WHERE user_id = $1"

  let attach conn ~user_id ~state ~binding_hash id =
    Store.attach_pending_installation conn ~user_id ~state
      ~session_binding_hash:binding_hash ~flow:GO.Project_onboarding
      ~pending_github_installation_id:id

  let attach_ok conn ~user_id ~state ~binding_hash id =
    let* r = attach conn ~user_id ~state ~binding_hash id in
    match r with
    | Ok () -> Lwt.return_unit
    | Error e -> Alcotest.failf "attach: %s" (attach_error_str e)

  let attach_unavailable label conn ~user_id ~state ~binding_hash id =
    let* r = attach conn ~user_id ~state ~binding_hash id in
    match r with
    | Error Store.State_unavailable -> Lwt.return_unit
    | Error e ->
        Alcotest.failf "%s: expected State_unavailable, got %s" label
          (attach_error_str e)
    | Ok () -> Alcotest.failf "%s: expected State_unavailable, got Ok" label

  (* Standard fixture: one user holding one freshly issued state. *)
  let issued_fixture conn =
    let (module C : Caqti_lwt.CONNECTION) = conn in
    let* uid = C.find q_insert_user () in
    let* uid = or_fail "user" uid in
    let binding = GOC.generate_session_binding () in
    let binding_hash = GOC.hash_session_binding binding in
    let* state =
      issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
    in
    Lwt.return (uid, state, binding, binding_hash)

  let single_row conn uid =
    let (module C : Caqti_lwt.CONNECTION) = conn in
    let* rows = C.collect_list q_attach_rows_for_user uid in
    let* rows = or_fail "rows" rows in
    match rows with
    | [ row ] -> Lwt.return row
    | rows ->
        Alcotest.failf "expected exactly one row, found %d"
          (List.length rows)

  let check_untouched_unconsumed label (_, (pending, consumed_null, _)) =
    Alcotest.(check (option int64)) (label ^ ": pending still NULL") None
      pending;
    Alcotest.(check bool) (label ^ ": still unconsumed") true consumed_null

  let attach_validation_case =
    db_case "attach: invalid inputs rejected before SQL" (fun conn ->
        let state = GOC.generate_state () in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let check_rejected label expected ~user_id id =
          let* r = attach conn ~user_id ~state ~binding_hash id in
          match r with
          | Error e ->
              Alcotest.(check string) label expected (attach_error_str e);
              Lwt.return_unit
          | Ok () -> Alcotest.failf "%s: expected %s, got Ok" label expected
        in
        let* () = check_rejected "user id 0" "Invalid_user_id" ~user_id:0 1L in
        let* () =
          check_rejected "user id -1" "Invalid_user_id" ~user_id:(-1) 1L
        in
        let* () =
          check_rejected "installation id 0"
            "Invalid_pending_installation_id" ~user_id:1 0L
        in
        check_rejected "negative installation id"
          "Invalid_pending_installation_id" ~user_id:1 (-42L))

  let attach_success_case =
    tx_case "attach: first attachment sets only the pending id" (fun conn ->
        let* uid, state, binding, binding_hash = issued_fixture conn in
        let* before = single_row conn uid in
        let ((state_hash0, binding_hash0, flow0), (pending0, _, times0)) =
          before
        in
        Alcotest.(check (option int64)) "pending NULL before attach" None
          pending0;
        let* () =
          attach_ok conn ~user_id:uid ~state ~binding_hash 123456789L
        in
        let* row = single_row conn uid in
        let ( (state_hash, stored_binding_hash, flow),
              (pending, consumed_null, (created, expires)) ) =
          row
        in
        Alcotest.(check (option int64)) "attached id" (Some 123456789L)
          pending;
        Alcotest.(check bool) "consumed_at still NULL" true consumed_null;
        Alcotest.(check string) "state_hash unchanged" state_hash0 state_hash;
        Alcotest.(check string) "binding hash unchanged" binding_hash0
          stored_binding_hash;
        Alcotest.(check string) "flow unchanged" flow0 flow;
        let created0, expires0 = times0 in
        Alcotest.(check (float 0.001)) "created_at unchanged" created0 created;
        Alcotest.(check (float 0.001)) "expires_at unchanged" expires0 expires;
        (* Attachment sends only hashes over SQL; the text columns must
           still hold no raw token material. *)
        let text =
          String.concat "|" [ state_hash; stored_binding_hash; flow ]
        in
        Alcotest.(check bool) "raw state absent from text columns" false
          (goc_contains ~needle:(GOC.state_to_string state) text);
        Alcotest.(check bool) "raw binding absent from text columns" false
          (goc_contains ~needle:(GOC.session_binding_to_string binding) text);
        Lwt.return_unit)

  let attach_idempotent_case =
    tx_case "attach: identical retry succeeds on the same single row"
      (fun conn ->
        let* uid, state, _, binding_hash = issued_fixture conn in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        (* single_row also proves the retry created no extra row. *)
        let* _, (pending, consumed_null, _) = single_row conn uid in
        Alcotest.(check (option int64)) "still the same id" (Some 42L)
          pending;
        Alcotest.(check bool) "still unconsumed" true consumed_null;
        Lwt.return_unit)

  let attach_conflict_case =
    tx_case "attach: a different id never overwrites the first" (fun conn ->
        let* uid, state, _, binding_hash = issued_fixture conn in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let* () =
          attach_unavailable "conflicting id" conn ~user_id:uid ~state
            ~binding_hash 43L
        in
        let* _, (pending, consumed_null, _) = single_row conn uid in
        Alcotest.(check (option int64)) "first id retained" (Some 42L)
          pending;
        Alcotest.(check bool) "conflict did not consume" true consumed_null;
        Lwt.return_unit)

  let attach_missing_state_case =
    tx_case "attach: unknown state is State_unavailable, creates no row"
      (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid = C.find q_insert_user () in
        let* uid = or_fail "user" uid in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        (* Generated but never issued: no row anywhere carries its hash. *)
        let state = GOC.generate_state () in
        let* () =
          attach_unavailable "unknown state" conn ~user_id:uid ~state
            ~binding_hash 42L
        in
        let* count = C.find q_count_for_user uid in
        let* count = or_fail "count" count in
        Alcotest.(check int) "no row created" 0 count;
        Lwt.return_unit)

  let attach_wrong_user_case =
    tx_case "attach: another user cannot attach to the state" (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash = issued_fixture conn in
        let* other = C.find q_insert_second_user () in
        let* other = or_fail "second user" other in
        let* () =
          attach_unavailable "wrong user" conn ~user_id:other ~state
            ~binding_hash 42L
        in
        let* row = single_row conn uid in
        check_untouched_unconsumed "wrong user" row;
        Lwt.return_unit)

  let attach_wrong_binding_case =
    tx_case "attach: session-binding mismatch does not attach" (fun conn ->
        let* uid, state, _, _ = issued_fixture conn in
        let other_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let* () =
          attach_unavailable "wrong binding" conn ~user_id:uid ~state
            ~binding_hash:other_hash 42L
        in
        let* row = single_row conn uid in
        check_untouched_unconsumed "wrong binding" row;
        Lwt.return_unit)

  let attach_expired_case =
    tx_case "attach: expired state does not attach" (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash = issued_fixture conn in
        let* r = C.exec q_expire_states_for_user uid in
        let* () = or_fail "expire fixture" r in
        let* () =
          attach_unavailable "expired" conn ~user_id:uid ~state ~binding_hash
            42L
        in
        let* row = single_row conn uid in
        check_untouched_unconsumed "expired" row;
        Lwt.return_unit)

  let attach_consumed_case =
    tx_case "attach: consumed state does not attach" (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash = issued_fixture conn in
        let* r = C.exec q_consume_states_for_user uid in
        let* () = or_fail "consume fixture" r in
        let* () =
          attach_unavailable "consumed" conn ~user_id:uid ~state
            ~binding_hash 42L
        in
        let* _, (pending, consumed_null, _) = single_row conn uid in
        Alcotest.(check (option int64)) "pending still NULL" None pending;
        Alcotest.(check bool) "row stayed consumed" false consumed_null;
        Lwt.return_unit)

  (* === consume ===
     consume starts (and commits) its own transaction, so these cases cannot
     run inside the rolled-back tx_case wrapper: a nested BEGIN would make
     consume's COMMIT commit the outer fixture transaction. Instead each case
     runs in autocommit with per-case usernames, deleting its fixture users
     both before (stale rows from a crashed run) and after — the users FK is
     ON DELETE CASCADE, so the state rows die with them. As above, raw
     states/bindings never reach assertion messages; hash comparisons are
     boolean so hash values cannot appear in failure output either.

     Flow mismatch has no case for the same reason as attach: the branch is
     structurally implemented, but with a single domain variant and the
     schema CHECK admitting only 'project_onboarding', no valid current data
     can produce it. *)

  let consume_error_str : Store.consume_error -> string = function
    | Store.Invalid_user_id -> "Invalid_user_id"
    | Store.State_not_found -> "State_not_found"
    | Store.State_expired -> "State_expired"
    | Store.State_already_consumed -> "State_already_consumed"
    | Store.User_mismatch -> "User_mismatch"
    | Store.Session_binding_mismatch -> "Session_binding_mismatch"
    | Store.Flow_mismatch -> "Flow_mismatch"
    | Store.Missing_pending_installation -> "Missing_pending_installation"
    | Store.Storage_error -> "Storage_error"

  let q_insert_named_user =
    (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

  let q_delete_named_user =
    (Caqti_type.string ->. Caqti_type.unit)
    "DELETE FROM users WHERE username = $1"

  let q_consumed_epoch_for_user =
    (Caqti_type.(int ->! option float))
    "SELECT EXTRACT(EPOCH FROM consumed_at)::float8
     FROM github_onboarding_states WHERE user_id = $1"

  let q_consumed_count_for_user =
    (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*) FROM github_onboarding_states
     WHERE user_id = $1 AND consumed_at IS NOT NULL"

  let consume_case name ~users f =
    db_case name (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let delete_users () =
          Lwt_list.iter_s
            (fun u ->
              let* r = C.exec q_delete_named_user u in
              or_fail "cleanup" r)
            users
        in
        let* () = delete_users () in
        Lwt.finalize (fun () -> f conn) delete_users)

  (* Fixture: a named committed user holding one freshly issued state. *)
  let consume_fixture conn username =
    let (module C : Caqti_lwt.CONNECTION) = conn in
    let* uid = C.find q_insert_named_user username in
    let* uid = or_fail "user" uid in
    let binding = GOC.generate_session_binding () in
    let binding_hash = GOC.hash_session_binding binding in
    let* state =
      issue_ok conn ~user_id:uid ~session_binding_hash:binding_hash
    in
    Lwt.return (uid, state, binding, binding_hash)

  let do_consume conn ~user_id ~state ~binding_hash =
    Store.consume conn ~user_id ~state ~session_binding_hash:binding_hash
      ~flow:GO.Project_onboarding

  let consume_ok conn ~user_id ~state ~binding_hash =
    let* r = do_consume conn ~user_id ~state ~binding_hash in
    match r with
    | Ok consumed -> Lwt.return consumed
    | Error e -> Alcotest.failf "consume: %s" (consume_error_str e)

  let consume_expect label expected conn ~user_id ~state ~binding_hash =
    let* r = do_consume conn ~user_id ~state ~binding_hash in
    match r with
    | Error e ->
        Alcotest.(check string) label expected (consume_error_str e);
        Lwt.return_unit
    | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" label expected

  let consume_success_case =
    consume_case "consume: valid state returns stored row, burns once"
      ~users:[ "ghconsume_ok" ] (fun conn ->
        let* uid, state, binding, binding_hash =
          consume_fixture conn "ghconsume_ok"
        in
        let* () =
          attach_ok conn ~user_id:uid ~state ~binding_hash 123456789L
        in
        let* before = single_row conn uid in
        let ((state_hash0, binding_hash0, flow0), (_, _, times0)) = before in
        let* consumed =
          consume_ok conn ~user_id:uid ~state ~binding_hash
        in
        Alcotest.(check int) "returned user_id is the stored one" uid
          consumed.Store.user_id;
        (match consumed.Store.flow with
        | GO.Project_onboarding -> ());
        Alcotest.(check int64) "returned pending id is the stored one"
          123456789L consumed.Store.pending_github_installation_id;
        let* row = single_row conn uid in
        let ( (state_hash, stored_binding_hash, flow),
              (pending, consumed_null, (created, expires)) ) =
          row
        in
        Alcotest.(check bool) "consumed_at now set" false consumed_null;
        Alcotest.(check bool) "state_hash unchanged" true
          (String.equal state_hash0 state_hash);
        Alcotest.(check bool) "binding hash unchanged" true
          (String.equal binding_hash0 stored_binding_hash);
        Alcotest.(check string) "flow unchanged" flow0 flow;
        Alcotest.(check (option int64)) "pending id unchanged"
          (Some 123456789L) pending;
        let created0, expires0 = times0 in
        Alcotest.(check (float 0.001)) "created_at unchanged" created0 created;
        Alcotest.(check (float 0.001)) "expires_at unchanged" expires0 expires;
        (* Consumption sends only hashes over SQL; the text columns must
           still hold no raw token material. *)
        let text =
          String.concat "|" [ state_hash; stored_binding_hash; flow ]
        in
        Alcotest.(check bool) "raw state absent from text columns" false
          (goc_contains ~needle:(GOC.state_to_string state) text);
        Alcotest.(check bool) "raw binding absent from text columns" false
          (goc_contains ~needle:(GOC.session_binding_to_string binding) text);
        Lwt.return_unit)

  let consume_replay_case =
    consume_case "consume: replay preserves the original consumed_at"
      ~users:[ "ghconsume_replay" ] (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_replay"
        in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let* _ = consume_ok conn ~user_id:uid ~state ~binding_hash in
        let* first = C.find q_consumed_epoch_for_user uid in
        let* first = or_fail "consumed_at" first in
        let first =
          match first with
          | Some epoch -> epoch
          | None -> Alcotest.fail "first consume left consumed_at NULL"
        in
        let* () =
          consume_expect "replay" "State_already_consumed" conn ~user_id:uid
            ~state ~binding_hash
        in
        let* second = C.find q_consumed_epoch_for_user uid in
        let* second = or_fail "consumed_at after replay" second in
        (* Exact float equality: the stored microsecond timestamp must
           round-trip untouched — any rewrite by the replay would differ. *)
        Alcotest.(check (option (float 0.))) "original consumed_at kept"
          (Some first) second;
        Lwt.return_unit)

  let consume_unknown_state_case =
    consume_case "consume: unknown state is State_not_found, creates no row"
      ~users:[ "ghconsume_unknown" ] (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid = C.find q_insert_named_user "ghconsume_unknown" in
        let* uid = or_fail "user" uid in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        (* Generated but never issued: no row anywhere carries its hash. *)
        let state = GOC.generate_state () in
        let* () =
          consume_expect "unknown state" "State_not_found" conn ~user_id:uid
            ~state ~binding_hash
        in
        let* count = C.find q_count_for_user uid in
        let* count = or_fail "count" count in
        Alcotest.(check int) "no row created" 0 count;
        Lwt.return_unit)

  let consume_expired_case =
    consume_case "consume: expired state stays unconsumed for cleanup"
      ~users:[ "ghconsume_expired" ] (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_expired"
        in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let* r = C.exec q_expire_states_for_user uid in
        let* () = or_fail "expire fixture" r in
        let* () =
          consume_expect "expired" "State_expired" conn ~user_id:uid ~state
            ~binding_hash
        in
        let* _, (_, consumed_null, _) = single_row conn uid in
        Alcotest.(check bool) "consumed_at still NULL" true consumed_null;
        Lwt.return_unit)

  let consume_wrong_user_case =
    consume_case "consume: wrong user burns the state"
      ~users:[ "ghconsume_wu_a"; "ghconsume_wu_b" ] (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_wu_a"
        in
        let* other = C.find q_insert_named_user "ghconsume_wu_b" in
        let* other = or_fail "second user" other in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let* () =
          consume_expect "wrong user" "User_mismatch" conn ~user_id:other
            ~state ~binding_hash
        in
        let* _, (pending, consumed_null, _) = single_row conn uid in
        Alcotest.(check bool) "state burned" false consumed_null;
        Alcotest.(check (option int64)) "pending id untouched by burn"
          (Some 42L) pending;
        (* The burn is durable: even the rightful caller is locked out. *)
        consume_expect "correct retry after burn" "State_already_consumed"
          conn ~user_id:uid ~state ~binding_hash)

  let consume_wrong_binding_case =
    consume_case "consume: session-binding mismatch burns the state"
      ~users:[ "ghconsume_wb" ] (fun conn ->
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_wb"
        in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let other_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let* () =
          consume_expect "wrong binding" "Session_binding_mismatch" conn
            ~user_id:uid ~state ~binding_hash:other_hash
        in
        let* _, (_, consumed_null, _) = single_row conn uid in
        Alcotest.(check bool) "state burned" false consumed_null;
        consume_expect "correct retry after burn" "State_already_consumed"
          conn ~user_id:uid ~state ~binding_hash)

  let consume_missing_pending_case =
    consume_case "consume: missing installation id burns, blocks attach"
      ~users:[ "ghconsume_mp" ] (fun conn ->
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_mp"
        in
        (* Deliberately no attach: the flow never reached the setup return. *)
        let* () =
          consume_expect "missing pending" "Missing_pending_installation"
            conn ~user_id:uid ~state ~binding_hash
        in
        let* _, (pending, consumed_null, _) = single_row conn uid in
        Alcotest.(check bool) "state burned" false consumed_null;
        Alcotest.(check (option int64)) "pending still NULL" None pending;
        attach_unavailable "attach after burn" conn ~user_id:uid ~state
          ~binding_hash 42L)

  let consume_invalid_user_case =
    db_case "consume: non-positive user ids rejected before SQL" (fun conn ->
        let state = GOC.generate_state () in
        let binding_hash =
          GOC.hash_session_binding (GOC.generate_session_binding ())
        in
        let* () =
          consume_expect "user id 0" "Invalid_user_id" conn ~user_id:0 ~state
            ~binding_hash
        in
        consume_expect "user id -1" "Invalid_user_id" conn ~user_id:(-1)
          ~state ~binding_hash)

  (* Two independent connections race on one state: the second blocks on the
     first's FOR UPDATE row lock, then re-reads the committed row and must
     see it consumed. Exactly one winner, one burn. *)
  let consume_concurrent_case =
    consume_case "consume: concurrent consumers serialize on the row lock"
      ~users:[ "ghconsume_race" ] (fun conn ->
        let (module C : Caqti_lwt.CONNECTION) = conn in
        let* uid, state, _, binding_hash =
          consume_fixture conn "ghconsume_race"
        in
        let* () = attach_ok conn ~user_id:uid ~state ~binding_hash 42L in
        let url =
          (* db_case only runs under the gate, so the URL is present. *)
          match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
          | Some url -> url
          | None -> Alcotest.fail "EARDE_TEST_DATABASE_URL vanished mid-run"
        in
        let* conn2 = Caqti_lwt_unix.connect (Uri.of_string url) in
        let* conn2 = or_fail "second connect" conn2 in
        let (module C2 : Caqti_lwt.CONNECTION) = conn2 in
        Lwt.finalize
          (fun () ->
            let* r1, r2 =
              Lwt.both
                (do_consume conn ~user_id:uid ~state ~binding_hash)
                (do_consume conn2 ~user_id:uid ~state ~binding_hash)
            in
            let classify label = function
              | Ok consumed ->
                  Alcotest.(check int64)
                    (label ^ ": winner sees stored pending id") 42L
                    consumed.Store.pending_github_installation_id;
                  `Won
              | Error Store.State_already_consumed -> `Lost
              | Error e ->
                  Alcotest.failf "%s: unexpected %s" label
                    (consume_error_str e)
            in
            (match (classify "first" r1, classify "second" r2) with
            | `Won, `Lost | `Lost, `Won -> ()
            | `Won, `Won -> Alcotest.fail "both consumers won"
            | `Lost, `Lost -> Alcotest.fail "no consumer won");
            let* burned = C.find q_consumed_count_for_user uid in
            let* burned = or_fail "burn count" burned in
            Alcotest.(check int) "exactly one non-null consumed_at" 1 burned;
            Lwt.return_unit)
          (fun () -> C2.disconnect ()))

  let suite =
    [ single_issue_case; multiplicity_case; invalid_user_case;
      missing_user_case; attach_validation_case; attach_success_case;
      attach_idempotent_case; attach_conflict_case;
      attach_missing_state_case; attach_wrong_user_case;
      attach_wrong_binding_case; attach_expired_case; attach_consumed_case;
      consume_success_case; consume_replay_case; consume_unknown_state_case;
      consume_expired_case; consume_wrong_user_case;
      consume_wrong_binding_case; consume_missing_pending_case;
      consume_invalid_user_case; consume_concurrent_case ]
end

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
      (* Onboarding-state enum: canonical decode + serialize; off-enum is an explicit Error. *)
    ; ( "community_onboarding_state"
      , [ check_onb_decodes "draft decodes" "draft" D.Community_draft
        ; check_onb_decodes "published decodes" "published" D.Community_published
        ; check_onb_serializes "draft serializes" D.Community_draft "draft"
        ; check_onb_serializes "published serializes" D.Community_published "published"
        ; check_onb_error "unknown is an error" "archived"
        ; check_onb_error "blank is an error" ""
        ; check_onb_error "case-sensitive: Draft rejected" "Draft"
        ; check_onb_error "case-sensitive: PUBLISHED rejected" "PUBLISHED"
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
                    (Some "account_logged_in")
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
        @ [ check_keys "account_signed_up keys (incl. $set)"
              (List.assoc "account_signed_up" an_all_events)
              [ "user_id"; "$set"; "deployment_environment" ]
          ; check_keys "account_logged_in keys (incl. $set)"
              (List.assoc "account_logged_in" an_all_events)
              [ "user_id"; "$set"; "deployment_environment" ]
          ; check_keys "community_joined keys"
              (List.assoc "community_joined" an_all_events)
              [ "user_id"; "community_id"; "community_slug";
                "community_visibility"; "$groups"; "deployment_environment" ]
          ; check_keys "community_left keys"
              (List.assoc "community_left" an_all_events)
              [ "user_id"; "community_id"; "$groups"; "deployment_environment" ]
          ; check_keys "chat_message_sent keys"
              (List.assoc "chat_message_sent" an_all_events)
              [ "user_id"; "community_id"; "community_slug"; "channel_id";
                "channel_slug"; "message_id"; "content_length";
                "response_mode"; "$groups"; "deployment_environment" ]
          ; check_keys "forum_thread_created keys"
              (List.assoc "forum_thread_created" an_all_events)
              [ "user_id"; "community_id"; "section_id"; "post_id";
                "content_length"; "has_link"; "has_mention"; "$groups";
                "deployment_environment" ]
          ; check_keys "forum_comment_created keys (no parent -> omitted)"
              (List.assoc "forum_comment_created" an_all_events)
              [ "user_id"; "community_id"; "post_id"; "comment_id";
                "content_length"; "has_mention"; "$groups";
                "deployment_environment" ]
          ; check_keys "conversation_promoted keys (no section -> omitted)"
              (List.assoc "conversation_promoted" an_all_events)
              [ "user_id"; "community_id"; "community_slug"; "channel_id";
                "channel_slug"; "post_id"; "message_id";
                "promoted_message_count"; "promoted_participant_count";
                "$groups"; "deployment_environment" ]
          ; check_keys "account_deleted keys (personless: no user_id)"
              An.Account_deleted
              [ "$process_person_profile"; "deployment_environment" ]
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
                          ; ("deployment_environment", `String "development")
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
        ; an_case "$set only on account_signed_up and account_logged_in" (fun () ->
              List.iter
                (fun (name, event) ->
                  let has_set =
                    List.mem_assoc "$set" (payload_props (an_payload event))
                  in
                  let expected =
                    name = "account_signed_up" || name = "account_logged_in"
                  in
                  if has_set <> expected then
                    Alcotest.failf "%s: unexpected $set presence (%b)" name
                      has_set)
                an_all_events)
        ; an_case "account_signed_up $set is exactly the closed person record"
            (fun () ->
              match
                List.assoc_opt "$set"
                  (payload_props
                     (an_payload (List.assoc "account_signed_up" an_all_events)))
              with
              | Some set -> Alcotest.check yojson "signup $set" an_person_set set
              | None -> Alcotest.fail "account_signed_up has no $set")
        ; an_case "account_logged_in $set is exactly the closed person record"
            (fun () ->
              match
                List.assoc_opt "$set"
                  (payload_props
                     (an_payload (List.assoc "account_logged_in" an_all_events)))
              with
              | Some set -> Alcotest.check yojson "login $set" an_person_set set
              | None -> Alcotest.fail "account_logged_in has no $set")
          (* Pivot taxonomy invariants: the canonical closed name set (no
             pre-rename name, no not-yet-implemented GitHub onboarding
             event) and the email-free person contract on every $set-bearing
             payload, including the consent-transition sync. *)
        ; an_case "server taxonomy is exactly the canonical closed set"
            (fun () ->
              Alcotest.(check (slist string compare))
                "emitted event names"
                [ "account_signed_up"; "account_logged_in"; "community_joined";
                  "community_left"; "chat_message_sent";
                  "forum_thread_created"; "forum_comment_created";
                  "conversation_promoted"; "account_deleted" ]
                (List.map
                   (fun (_, event) ->
                     match payload_member "event" (an_payload event) with
                     | Some (`String name) -> name
                     | _ -> "<missing>")
                   an_all_events))
        ; an_case "no obsolete pre-pivot event name is ever emitted" (fun () ->
              let obsolete =
                [ "signup_confirmed"; "login_succeeded"; "post_created";
                  "comment_created"; "thread_promoted" ]
              in
              List.iter
                (fun (label, event) ->
                  match payload_member "event" (an_payload event) with
                  | Some (`String name) ->
                      if List.mem name obsolete then
                        Alcotest.failf "%s emits obsolete name %s" label name
                  | _ -> Alcotest.failf "%s: payload has no event name" label)
                an_all_events)
        ; an_case "person $set never contains email" (fun () ->
              let check_set label payload =
                match List.assoc_opt "$set" (payload_props payload) with
                | Some (`Assoc set) ->
                    if List.mem_assoc "email" set then
                      Alcotest.failf "%s: $set contains email" label
                | Some _ -> Alcotest.failf "%s: $set is not an object" label
                | None -> Alcotest.failf "%s: no $set" label
              in
              check_set "account_signed_up"
                (an_payload (List.assoc "account_signed_up" an_all_events));
              check_set "account_logged_in"
                (an_payload (List.assoc "account_logged_in" an_all_events));
              check_set "consent sync"
                (AnT.person_sync_payload ~api_key:"phc_test" ~environment:An.Development
                   ~distinct_id:"user:1" an_person))
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
            (List.assoc "forum_thread_created" an_all_events)
        ; check_group "comment has group" (Some "community:7")
            (List.assoc "forum_comment_created" an_all_events)
        ; check_group "promoted has group" (Some "community:7")
            (List.assoc "conversation_promoted" an_all_events)
        ; check_group "signup has no group" None
            (List.assoc "account_signed_up" an_all_events)
        ; check_group "login has no group" None
            (List.assoc "account_logged_in" an_all_events)
        ; check_group "deletion has no group" None An.Account_deleted
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
                  ; ( "properties"
                    , `Assoc
                        [ ("$set", an_person_set)
                        ; ("deployment_environment", `String "development")
                        ] )
                  ]
              in
              let actual =
                AnT.person_sync_payload ~api_key:"phc_test" ~environment:An.Development
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
                        ; ("deployment_environment", `String "development")
                        ] )
                  ]
              in
              let actual =
                AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                  ~distinct_id:"user:1"
                  { An.community_id = 7; community_slug = Some "ocaml";
                    community_name = Some "OCaml";
                    community_visibility = "public";
                    created_at = Some "2026-01-01T00:00:00Z" }
              in
              Alcotest.check yojson "group identify payload" expected actual)
        ; an_case "created_at omitted when absent" (fun () ->
              let actual =
                AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                  ~distinct_id:"user:1"
                  { An.community_id = 7; community_slug = Some "ocaml";
                    community_name = Some "OCaml";
                    community_visibility = "public"; created_at = None }
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
          (* §13: fully private communities. Group identity stays
             community:<id>; readable identifiers never appear. *)
        ; an_case "private $groupidentify has no name or slug" (fun () ->
              let actual =
                AnT.group_identify_payload ~api_key:"phc_test" ~environment:An.Development
                  ~distinct_id:"user:1" an_private_group
              in
              Alcotest.(check (option string)) "group key stays numeric"
                (Some "community:9") (group_key_prop_of actual);
              let set = group_set_of actual in
              Alcotest.(check (slist string compare))
                "private group set keys"
                [ "community_id"; "community_visibility" ]
                (List.map fst set);
              Alcotest.(check bool) "no community_name" false
                (List.mem_assoc "community_name" set);
              Alcotest.(check bool) "no community_slug" false
                (List.mem_assoc "community_slug" set))
        ; an_case "private domain events omit slugs, keep ids/counts/group"
            (fun () ->
              let chat =
                an_payload
                  (An.Chat_message_sent
                     { user_id = 1; community_id = 9; community_slug = None;
                       channel_id = 3; channel_slug = None; message_id = 91L;
                       content_length = 42; response_mode = An.Response_json })
              in
              Alcotest.(check (slist string compare))
                "private chat keys"
                [ "user_id"; "community_id"; "channel_id"; "message_id";
                  "content_length"; "response_mode"; "$groups";
                  "deployment_environment" ]
                (prop_keys chat);
              Alcotest.(check (option string)) "private chat group"
                (Some "community:9") (an_group_key chat);
              let joined =
                an_payload
                  (An.Community_joined
                     { user_id = 1; community_id = 9; community_slug = None;
                       community_visibility = "private" })
              in
              Alcotest.(check (slist string compare))
                "private join keys"
                [ "user_id"; "community_id"; "community_visibility";
                  "$groups"; "deployment_environment" ]
                (prop_keys joined);
              let promoted =
                an_payload
                  (An.Conversation_promoted
                     { user_id = 1; community_id = 9; community_slug = None;
                       channel_id = 3; channel_slug = None; section_id = None;
                       post_id = 11; message_id = 91L;
                       promoted_message_count = 4;
                       promoted_participant_count = Some 2 })
              in
              Alcotest.(check (slist string compare))
                "private promoted keys"
                [ "user_id"; "community_id"; "channel_id"; "post_id";
                  "message_id"; "promoted_message_count";
                  "promoted_participant_count"; "$groups";
                  "deployment_environment" ]
                (prop_keys promoted))
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
                      { An.username = "a"; signup_date = "";
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
                    { An.username = "a"; signup_date = "";
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
        ; an_case "production https: exact cookie name, no __Host- prefix"
            (fun () ->
              (* Dream infers a __Host- prefix for a Secure + Path=/ cookie
                 unless ~prefix:None is passed explicitly; the http-origin
                 cases above never set Secure, so only this validated
                 production (https) configuration can catch the regression. *)
              install_production_config ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let request =
                    Dream.request ~method_:`POST ~target:"/analytics/consent"
                      ~headers:
                        [ ("Content-Type", "application/json")
                        ; ("Origin", "https://earde.com")
                        ; ("Sec-Fetch-Site", "same-origin")
                        ]
                      {|{"state":"granted"}|}
                  in
                  let response =
                    Lwt_main.run
                      (Earde.Handlers.analytics_consent_handler request)
                  in
                  Alcotest.(check int) "status" 204
                    (Dream.status_to_int (Dream.status response));
                  let cookie =
                    Option.value ~default:""
                      (Dream.header response "Set-Cookie")
                  in
                  Alcotest.(check bool) "exact name=value" true
                    (contains cookie "earde_analytics_consent=granted");
                  Alcotest.(check bool) "no __Host- prefix" false
                    (contains cookie "__Host-earde_analytics_consent");
                  Alcotest.(check bool) "secure" true (contains cookie "Secure");
                  Alcotest.(check bool) "path" true (contains cookie "Path=/");
                  Alcotest.(check bool) "samesite lax" true
                    (contains cookie "SameSite=Lax");
                  Alcotest.(check string) "parser recognizes returned cookie"
                    "granted"
                    (consent_str
                       (AnT.consent_of_cookie_header (Some cookie)))))
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
          (* §13 private-community marker: derived from the authoritative
             visibility passed with the community id, value "true" only. *)
        ; an_case "private community page carries the private marker" (fun () ->
              AnT.use_enabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout
                      ~analytics_community:(9, Earde.Db.Community_private)
                      ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "private marker" true
                    (contains html "data-analytics-private-community='true'");
                  Alcotest.(check bool) "group attr stays numeric" true
                    (contains html "data-analytics-group='community:9'")))
        ; an_case "public community page has no private marker" (fun () ->
              AnT.use_enabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout
                      ~analytics_community:(7, Earde.Db.Community_public)
                      ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "no private marker" false
                    (contains html "data-analytics-private-community");
                  Alcotest.(check bool) "group attr present" true
                    (contains html "data-analytics-group='community:7'")))
        ; an_case "global page has neither group nor private marker" (fun () ->
              AnT.use_enabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "no group attr" false
                    (contains html "data-analytics-group");
                  Alcotest.(check bool) "no private marker" false
                    (contains html "data-analytics-private-community")))
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
      (* §13 privacy hardening: autocapture is structural-only, exception
         autocapture is off, and fully private community documents never
         initialize the SDK. Static contract checks against the shipped
         analytics.js (same technique as the replay-masking coverage). *)
    ; ( "analytics_autocapture_protection"
      , [ an_case "autocapture text masking and scrubbers present" (fun () ->
              let js = read_analytics_js () in
              List.iter
                (fun needle ->
                  if not (contains js needle) then
                    Alcotest.failf "analytics.js is missing %S" needle)
                [ (* documented SDK option: element text never captured *)
                  "mask_all_text: true"
                  (* defense-in-depth scrubbers in the sanitize hook *)
                ; "delete props.$el_text"
                ; "sanitizeElement"
                ; "sanitizeElementsChain"
                ; "$elements_chain"
                ; "attr__title"
                ; "attr__aria-label"
                ; "attr__value"
                ; "attr__href"
                ; "attr__src"
                ; "attr__action"
                ])
        ; an_case "chain scrubber removes text and URL query/fragment"
            (fun () ->
              let js = read_analytics_js () in
              (* The regexes that make nested element text and full URLs
                 unrepresentable in $elements_chain payloads. *)
              Alcotest.(check bool) "text= scrub regex" true
                (contains js {|text="[^"]*"|});
              Alcotest.(check bool) "url attr scrub regex" true
                (contains js {|(?:attr__)?(?:href|src|action)="|});
              Alcotest.(check bool) "query/fragment stripper" true
                (contains js {|split("?")[0].split("#")[0]|}))
        ; an_case "manual closed events keep their own schemas" (fun () ->
              let js = read_analytics_js () in
              (* search_performed: exactly the existing closed properties. *)
              Alcotest.(check bool) "search_performed present" true
                (contains js "search_performed");
              List.iter
                (fun needle ->
                  if not (contains js needle) then
                    Alcotest.failf "search_performed lost %S" needle)
                [ "result_count: resultCount"; "active_tab: tab"; "page: page" ])
        ; an_case "automatic exception capture is disabled" (fun () ->
              let js = read_analytics_js () in
              Alcotest.(check bool) "capture_exceptions: false" true
                (contains js "capture_exceptions: false");
              Alcotest.(check bool) "no capture_exceptions: true" false
                (contains js "capture_exceptions: true");
              Alcotest.(check bool) "no window.onerror forwarding" false
                (contains js "window.onerror");
              Alcotest.(check bool) "no unhandledrejection forwarding" false
                (contains js "unhandledrejection");
              Alcotest.(check bool) "no raw error-message property" false
                (contains js "error_message"))
        ; an_case "private documents never reach the SDK" (fun () ->
              let js = read_analytics_js () in
              Alcotest.(check bool) "reads the private marker" true
                (contains js "data-analytics-private-community");
              (* The single centralized gate: initAnalytics resolves inert
                 before loadSdk on private documents, so pageview, group,
                 search_performed and the SDK download are all unreachable. *)
              Alcotest.(check bool) "privateCommunity gate" true
                (contains js "if (privateCommunity)");
              Alcotest.(check bool) "gate precedes SDK load" true
                (match
                   ( index_of js "if (privateCommunity)",
                     index_of js "initPromise = loadSdk()" )
                 with
                | Some gate, Some load -> gate < load
                | _ -> false))
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
                    render_layout ~session_user:"42" ~analytics_community:(7, Earde.Db.Community_public)
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
                    render_layout ~session_user:"42" ~analytics_community:(7, Earde.Db.Community_public)
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
                  let html = render_layout ~analytics_community:(7, Earde.Db.Community_public) () in
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
                    (contains html "ph-no-capture");
                  Alcotest.(check bool) "main content is a direct child of cs-main" true
                    (contains html "<main class='cs-main'>MAIN</main>")))
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
                  (* Replay-blocked via the class on <main class='cs-main'>
                     itself. A wrapper div here is a layout regression: it
                     detaches the chat head/scroller/composer from the
                     .cs-main flex column and collapses the message pane. *)
                  Alcotest.(check bool) "content replay-blocked" true
                    (contains html "<main class='cs-main ph-no-capture'>MAIN</main>");
                  Alcotest.(check bool) "no wrapper div between cs-main and content" false
                    (contains html "<div class='ph-no-capture'>");
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
                       ~analytics_community:(5, Earde.Db.Community_public)
                       ~title:"T" ~body:"B" ());
                  check_attr "community_manage_page" (Some "community:6")
                    (Earde.Components.community_manage_page
                       ~analytics_community:(6, Earde.Db.Community_public)
                       ~title:"T" ~body:"B" ());
                  check_attr "create_page (join/post/report/start-thread)"
                    (Some "community:8")
                    (Earde.Components.create_page
                       ~analytics_community:(8, Earde.Db.Community_public)
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
      (* §2.4 search metadata container: closed values only, emitted only for
         an executed non-empty search, never carrying the query. *)
    ; ( "analytics_search_metadata"
      , [ an_case "executed search emits the closed container" (fun () ->
              with_enabled_config (fun () ->
                  let html =
                    render_search ~page:3 ~tab:"communities"
                      ~communities:
                        [ test_community ~id:1
                            ~visibility:Earde.Db.Community_public
                        ; test_community ~id:2
                            ~visibility:Earde.Db.Community_public
                        ]
                      "ocaml"
                  in
                  Alcotest.(check (option string)) "tab" (Some "communities")
                    (attr_value html "data-analytics-search-tab");
                  Alcotest.(check (option string)) "count is rows rendered"
                    (Some "2")
                    (attr_value html "data-analytics-search-result-count");
                  Alcotest.(check (option string)) "page" (Some "3")
                    (attr_value html "data-analytics-search-page");
                  (* exactly the three closed attributes, nothing else *)
                  Alcotest.(check int) "three attributes" 3
                    (count_sub html "data-analytics-search-")))
        ; an_case "empty search emits no search analytics metadata" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_search "" in
                  Alcotest.(check bool) "no container" false
                    (contains html "sr-analytics");
                  Alcotest.(check bool) "no attributes" false
                    (contains html "data-analytics-search-")))
        ; an_case "disabled analytics emits no container" (fun () ->
              AnT.use_disabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html = render_search "ocaml" in
                  Alcotest.(check bool) "no container" false
                    (contains html "sr-analytics")))
        ; an_case "every supported tab value is emitted verbatim" (fun () ->
              with_enabled_config (fun () ->
                  List.iter
                    (fun tab ->
                      Alcotest.(check (option string)) tab (Some tab)
                        (attr_value (render_search ~tab "x")
                           "data-analytics-search-tab"))
                    [ "posts"; "communities"; "comments"; "people" ]))
        ; an_case "arbitrary tab values are normalized, never echoed" (fun () ->
              with_enabled_config (fun () ->
                  let html = render_search ~tab:"weird-tab" "x" in
                  (* the renderer's catch-all shows the Threads tab, so the
                     authoritative reported tab is posts *)
                  Alcotest.(check (option string)) "normalized" (Some "posts")
                    (attr_value html "data-analytics-search-tab");
                  match sr_analytics_tag html with
                  | None -> Alcotest.fail "container missing"
                  | Some tag ->
                      Alcotest.(check bool) "raw tab not in container" false
                        (contains tag "weird-tab")))
        ; an_case "the query never appears in the analytics container"
            (fun () ->
              with_enabled_config (fun () ->
                  let html = render_search ~tab:"posts" "sekret-term" in
                  (* the page legitimately echoes the query (input value,
                     pager hrefs — masked/stripped by other layers); the
                     analytics container must not *)
                  Alcotest.(check bool) "page echoes query" true
                    (contains html "sekret-term");
                  match sr_analytics_tag html with
                  | None -> Alcotest.fail "container missing"
                  | Some tag ->
                      Alcotest.(check bool) "no query in container" false
                        (contains tag "sekret-term")))
        ; an_case "non-positive page is clamped to the effective page 1"
            (fun () ->
              with_enabled_config (fun () ->
                  Alcotest.(check (option string)) "page 0 -> 1" (Some "1")
                    (attr_value (render_search ~page:0 "x")
                       "data-analytics-search-page")))
        ; an_case "result_count is authoritative for the active tab" (fun () ->
              with_enabled_config (fun () ->
                  (* a community row exists, but the active tab is people →
                     the count reflects the rendered people rows: 0 *)
                  let html =
                    render_search ~tab:"people"
                      ~communities:
                        [ test_community ~id:1
                            ~visibility:Earde.Db.Community_public
                        ]
                      "x"
                  in
                  Alcotest.(check (option string)) "count 0" (Some "0")
                    (attr_value html "data-analytics-search-result-count")))
        ] )
      (* Shipped analytics.js: search_performed capture contract. *)
    ; ( "analytics_js_search_event"
      , [ an_case "search_performed follows the manual $pageview, once"
            (fun () ->
              let js = read_analytics_js () in
              let pos needle =
                match index_of js needle with
                | Some i -> i
                | None -> Alcotest.failf "analytics.js is missing %S" needle
              in
              Alcotest.(check int) "exactly one capture call" 1
                (count_sub js "posthog.capture(\"search_performed\"");
              Alcotest.(check int) "exactly one call site" 1
                (count_sub js "captureSearchPerformed();");
              Alcotest.(check bool) "pageview precedes search event" true
                (pos "pageviewSent = true" < pos "captureSearchPerformed();");
              (* the single call site lives inside the consent-gated init
                 chain (initAnalytics only runs after granted consent and a
                 successful SDK load), before the next top-level function *)
              Alcotest.(check bool) "inside initAnalytics" true
                (pos "function initAnalytics"
                   < pos "captureSearchPerformed();"
                && pos "captureSearchPerformed();"
                   < pos "function clearPosthogPersistence"))
        ; an_case "once per document load, even across repeated init"
            (fun () ->
              let js = read_analytics_js () in
              Alcotest.(check bool) "module-owned guard" true
                (contains js "var searchPerformedSent = false;");
              Alcotest.(check bool) "re-entry returns" true
                (contains js "if (searchPerformedSent) return;");
              Alcotest.(check bool) "flag set before capture" true
                (contains js "searchPerformedSent = true;"))
        ; an_case "strict allowlist: closed tabs, integer count, positive page"
            (fun () ->
              let js = read_analytics_js () in
              Alcotest.(check bool) "exact tab set" true
                (contains js
                   "[\"posts\", \"communities\", \"comments\", \"people\"]");
              Alcotest.(check bool) "tab membership required" true
                (contains js "SEARCH_TABS.indexOf(tab) === -1) return;");
              Alcotest.(check bool) "integer-only parse" true
                (contains js "/^[0-9]+$/.test");
              Alcotest.(check bool) "positive page required" true
                (contains js "if (page < 1) return;"))
        ; an_case "captures a fresh closed object, never the raw dataset"
            (fun () ->
              let js = read_analytics_js () in
              (* reads exactly the three closed attributes... *)
              Alcotest.(check int) "three attribute reads" 3
                (count_sub js "getAttribute(\"data-analytics-search-");
              (* ...and never passes an attribute bag through *)
              Alcotest.(check bool) "no dataset access" false
                (contains js ".dataset");
              Alcotest.(check bool) "closed property object" true
                (contains js "result_count: resultCount,");
              (* no query-named property can exist in the file *)
              Alcotest.(check bool) "no query property" false
                (contains js "query:"))
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
                    [ "$group_type"; "$group_key"; "$group_set";
                      "deployment_environment" ]
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
    ; ( "posthog_deletion_api"
      , [ check_attempt "exact match: delete accepted -> completed"
            (persons_handler ())
            (Ok ()) ~expect_delete:true
        ; an_case "delete carries delete_events=true for the looked-up uuid"
            (fun () ->
              with_persons_stub (persons_handler ()) (fun ~seen ->
                  let ( let* ) = Lwt.bind in
                  let* r =
                    Earde.Posthog_deletion.attempt_person_deletion
                      ~distinct_id:"user:314"
                  in
                  Alcotest.(check attempt_result) "completed" (Ok ()) r;
                  (match !seen with
                   | [ lookup; delete ] ->
                       Alcotest.(check string) "lookup meth" "GET"
                         lookup.Api_stub.meth;
                       Alcotest.(check (option (list string)))
                         "exact URL-encoded distinct id" (Some [ "user:314" ])
                         (List.assoc_opt "distinct_id" lookup.Api_stub.query);
                       Alcotest.(check string) "delete path"
                         ("/api/projects/42/persons/" ^ stub_uuid ^ "/")
                         delete.Api_stub.path;
                       Alcotest.(check (option (list string)))
                         "delete_events=true" (Some [ "true" ])
                         (List.assoc_opt "delete_events" delete.Api_stub.query)
                   | l ->
                       Alcotest.failf "expected lookup+delete, saw %d"
                         (List.length l));
                  Lwt.return_unit))
        ; check_attempt "absent person -> completed without DELETE"
            (persons_handler ~persons:[] ())
            (Ok ()) ~expect_delete:false
        ; check_attempt "ambiguous lookup -> pending, nobody deleted"
            (persons_handler
               ~persons:[ person_json stub_uuid; person_json other_uuid ]
               ())
            (Error "ambiguous_person_match") ~expect_delete:false
        ; check_attempt "malformed lookup JSON -> pending"
            (persons_handler ~lookup_body_override:"not json at all" ())
            (Error "malformed_lookup_response") ~expect_delete:false
        ; check_attempt "lookup 401 -> pending with safe class"
            (persons_handler ~lookup_status:401
               ~lookup_body_override:{|{"type":"authentication_error"}|} ())
            (Error "lookup_http_401") ~expect_delete:false
        ; check_attempt "lookup 500 -> pending with safe class"
            (persons_handler ~lookup_status:500
               ~lookup_body_override:{|{"detail":"boom"}|} ())
            (Error "lookup_http_500") ~expect_delete:false
        ; check_attempt "delete 403 -> pending with safe class"
            (persons_handler ~delete_status:403 ())
            (Error "delete_http_403") ~expect_delete:true
        ; an_case "unreachable host -> network_error, never a raw exception"
            (fun () ->
              AnT.use_deletion_test_configuration
                ~ui_host:"http://127.0.0.1:9" ~project_id:(Some "42")
                ~personal_api_key:(Some deletion_test_key) ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let r =
                    Lwt_main.run
                      (Earde.Posthog_deletion.attempt_person_deletion
                         ~distinct_id:"user:314")
                  in
                  Alcotest.(check attempt_result) "network error class"
                    (Error "network_error") r))
        ; an_case "missing configuration -> safe pending marker" (fun () ->
              AnT.use_deletion_test_configuration ~ui_host:"http://earde.test"
                ~project_id:None ~personal_api_key:(Some deletion_test_key) ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let r =
                    Lwt_main.run
                      (Earde.Posthog_deletion.attempt_person_deletion
                         ~distinct_id:"user:314")
                  in
                  Alcotest.(check attempt_result) "missing configuration"
                    (Error "missing_configuration") r))
        ; an_case "account_deleted metric is personless by construction"
            (fun () ->
              let captured =
                with_sink ~enabled:true (fun () ->
                    Lwt_main.run
                      (An.capture_account_deleted_sequenced
                         (consent_request
                            (Some "earde_analytics_consent=granted"))))
              in
              match captured with
              | [ p ] ->
                  Alcotest.(check string) "event" "account_deleted"
                    (event_of p);
                  Alcotest.(check string) "constant non-user distinct id"
                    An.account_deletion_distinct_id (distinct_of p);
                  Alcotest.(check (slist string compare))
                    "person processing disabled, plus only the envelope"
                    [ "$process_person_profile"; "deployment_environment" ]
                    (prop_keys p);
                  (match
                     List.assoc_opt "$process_person_profile" (payload_props p)
                   with
                   | Some (`Bool false) -> ()
                   | _ ->
                       Alcotest.fail "$process_person_profile must be false");
                  let raw = Yojson.Safe.to_string p in
                  Alcotest.(check bool) "no user identity anywhere" false
                    (contains raw "user:");
                  Alcotest.(check bool) "no person $set" false
                    (contains raw "$set");
                  Alcotest.(check bool) "no community group" false
                    (contains raw "$groups")
              | l -> Alcotest.failf "expected 1 metric, got %d" (List.length l))
        ; an_case "denied/missing consent emits no deletion metric" (fun () ->
              let denied =
                with_sink ~enabled:true (fun () ->
                    Lwt_main.run
                      (An.capture_account_deleted_sequenced
                         (consent_request
                            (Some "earde_analytics_consent=denied"))))
              in
              Alcotest.(check int) "denied emits none" 0 (List.length denied);
              let missing =
                with_sink ~enabled:true (fun () ->
                    Lwt_main.run
                      (An.capture_account_deleted_sequenced
                         (consent_request None)))
              in
              Alcotest.(check int) "missing emits none" 0
                (List.length missing))
        ] )
      (* Closed deployment-environment parsing: exact values only; unknown,
         missing, blank and differently-cased values fail closed; disabled
         analytics needs no environment at all. *)
    ; ( "analytics_environment_parsing"
      , [ check_env "production parses exactly" ~expect_enabled:true
            ~expect_environment:(Some "production")
            (venv ~environment:"production" ~public_origin:"https://earde.com"
               ())
        ; check_env "staging parses exactly" ~expect_enabled:true
            ~expect_environment:(Some "staging")
            (venv ~environment:"staging"
               ~public_origin:"https://staging.example.com" ())
        ; check_env "development parses with explicit opt-in"
            ~expect_enabled:true ~expect_environment:(Some "development")
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"http://localhost:8080" ())
        ; check_env "missing environment fails closed" ~expect_enabled:false
            ~expect_environment:None
            (venv ~public_origin:"https://earde.com" ())
        ; check_env "blank environment fails closed" ~expect_enabled:false
            ~expect_environment:None
            (venv ~environment:"   " ~public_origin:"https://earde.com" ())
        ; check_env "cased Production fails closed" ~expect_enabled:false
            (venv ~environment:"Production" ~public_origin:"https://earde.com"
               ())
        ; check_env "cased STAGING fails closed" ~expect_enabled:false
            (venv ~environment:"STAGING"
               ~public_origin:"https://staging.example.com" ())
        ; check_env "abbreviation prod fails closed" ~expect_enabled:false
            (venv ~environment:"prod" ~public_origin:"https://earde.com" ())
        ; check_env "arbitrary value fails closed" ~expect_enabled:false
            (venv ~environment:"qa" ~public_origin:"https://earde.com" ())
        ; an_case "missing environment produces a diagnostic" (fun () ->
              let _, _, diags =
                venv ~public_origin:"https://earde.com" ()
              in
              Alcotest.(check int) "one diagnostic" 1 (List.length diags))
        ; an_case "disabled analytics permits an absent environment" (fun () ->
              let enabled, environment, diags =
                AnT.validate_environment_configuration ()
              in
              Alcotest.(check bool) "disabled" false enabled;
              Alcotest.(check (option string)) "no environment" None
                environment;
              Alcotest.(check int) "no diagnostics" 0 (List.length diags))
        ; check_env "POSTHOG_ENABLED must be exactly true" ~expect_enabled:false
            (venv ~enabled:"TRUE" ~environment:"production"
               ~public_origin:"https://earde.com" ())
        ; check_env "development without opt-in stays disabled"
            ~expect_enabled:false
            (venv ~environment:"development"
               ~public_origin:"http://localhost:8080" ())
        ; an_case "development without opt-in explains itself" (fun () ->
              let _, _, diags =
                venv ~environment:"development"
                  ~public_origin:"http://localhost:8080" ()
              in
              Alcotest.(check int) "one diagnostic" 1 (List.length diags))
        ; check_env "opt-in must be exactly true (not TRUE)"
            ~expect_enabled:false
            (venv ~environment:"development" ~allow_development:"TRUE"
               ~public_origin:"http://localhost:8080" ())
        ; check_env "opt-in must be exactly true (not 1)" ~expect_enabled:false
            (venv ~environment:"development" ~allow_development:"1"
               ~public_origin:"http://localhost:8080" ())
        ; an_case "explicit development enablement prints a diagnostic"
            (fun () ->
              let enabled, _, diags =
                venv ~environment:"development" ~allow_development:"true"
                  ~public_origin:"http://localhost:8080" ()
              in
              Alcotest.(check bool) "enabled" true enabled;
              Alcotest.(check int) "one notice" 1 (List.length diags))
        ] )
      (* Origin/environment binding: production is bound exactly to
         https://earde.com; staging to any OTHER https origin; the
         development opt-in to loopback only. *)
    ; ( "analytics_environment_origin"
      , [ check_env "production accepts exactly https://earde.com"
            ~expect_enabled:true
            (venv ~environment:"production" ~public_origin:"https://earde.com"
               ())
        ; check_env "production rejects http" ~expect_enabled:false
            (venv ~environment:"production" ~public_origin:"http://earde.com"
               ())
        ; check_env "production rejects www" ~expect_enabled:false
            (venv ~environment:"production"
               ~public_origin:"https://www.earde.com" ())
        ; check_env "production rejects a staging origin" ~expect_enabled:false
            (venv ~environment:"production"
               ~public_origin:"https://staging.earde.com" ())
        ; check_env "production rejects localhost" ~expect_enabled:false
            (venv ~environment:"production"
               ~public_origin:"http://localhost:8080" ())
        ; check_env "production rejects a trailing slash" ~expect_enabled:false
            (venv ~environment:"production"
               ~public_origin:"https://earde.com/" ())
        ; check_env "staging accepts a non-production https origin"
            ~expect_enabled:true
            (venv ~environment:"staging"
               ~public_origin:"https://staging.example.com" ())
        ; check_env "staging rejects the production origin"
            ~expect_enabled:false
            (venv ~environment:"staging" ~public_origin:"https://earde.com" ())
        ; check_env "staging rejects the www production origin"
            ~expect_enabled:false
            (venv ~environment:"staging" ~public_origin:"https://www.earde.com"
               ())
        ; check_env "staging rejects http" ~expect_enabled:false
            (venv ~environment:"staging"
               ~public_origin:"http://staging.example.com" ())
        ; check_env "development accepts http://localhost with port"
            ~expect_enabled:true
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"http://localhost:8080" ())
        ; check_env "development accepts https 127.0.0.1" ~expect_enabled:true
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"https://127.0.0.1:8443" ())
        ; check_env "development accepts ::1" ~expect_enabled:true
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"http://[::1]:8080" ())
        ; check_env "development rejects the production origin"
            ~expect_enabled:false
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"https://earde.com" ())
        ; check_env "development rejects arbitrary remote origins"
            ~expect_enabled:false
            (venv ~environment:"development" ~allow_development:"true"
               ~public_origin:"https://remote.example.com" ())
        ] )
      (* Complete-configuration requirements and the public browser config:
         incomplete token/host/project/key combinations disable analytics;
         the browser sees only the public token, ingest host and normalized
         environment — never the personal key or project id. *)
    ; ( "analytics_environment_config"
      , [ check_env "missing token disables" ~expect_enabled:false
            (AnT.validate_environment_configuration ~enabled:"true"
               ~environment:"production" ~api_host:"https://eu.i.posthog.com"
               ~ui_host:"https://eu.posthog.com" ~project_id:"42"
               ~personal_api_key:"phx_test_dummy"
               ~public_origin:"https://earde.com" ())
        ; check_env "missing project id disables" ~expect_enabled:false
            (AnT.validate_environment_configuration ~enabled:"true"
               ~environment:"production" ~project_token:"phc_test_token"
               ~api_host:"https://eu.i.posthog.com"
               ~ui_host:"https://eu.posthog.com"
               ~personal_api_key:"phx_test_dummy"
               ~public_origin:"https://earde.com" ())
        ; check_env "zero project id disables" ~expect_enabled:false
            (venv ~environment:"production" ~project_id:"0"
               ~public_origin:"https://earde.com" ())
        ; check_env "negative project id disables" ~expect_enabled:false
            (venv ~environment:"production" ~project_id:"-3"
               ~public_origin:"https://earde.com" ())
        ; check_env "non-numeric project id disables" ~expect_enabled:false
            (venv ~environment:"production" ~project_id:"abc"
               ~public_origin:"https://earde.com" ())
        ; check_env "missing personal key disables" ~expect_enabled:false
            (AnT.validate_environment_configuration ~enabled:"true"
               ~environment:"production" ~project_token:"phc_test_token"
               ~api_host:"https://eu.i.posthog.com"
               ~ui_host:"https://eu.posthog.com" ~project_id:"42"
               ~public_origin:"https://earde.com" ())
        ; check_env "http api host disables" ~expect_enabled:false
            (venv ~environment:"production"
               ~api_host:"http://eu.i.posthog.com"
               ~public_origin:"https://earde.com" ())
        ; check_env "http ui host disables" ~expect_enabled:false
            (venv ~environment:"production" ~ui_host:"http://eu.posthog.com"
               ~public_origin:"https://earde.com" ())
        ; an_case "staging requires the complete configuration too" (fun () ->
              let enabled, _, _ =
                AnT.validate_environment_configuration ~enabled:"true"
                  ~environment:"staging" ~project_token:"phc_test_token"
                  ~api_host:"https://eu.i.posthog.com"
                  ~ui_host:"https://eu.posthog.com"
                  ~public_origin:"https://staging.example.com" ()
              in
              Alcotest.(check bool) "disabled without id+key" false enabled)
        ; an_case "browser config carries only public values + environment"
            (fun () ->
              install_production_config ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  match An.browser_config () with
                  | None -> Alcotest.fail "expected browser config"
                  | Some cfg ->
                      Alcotest.(check string) "token" "phc_test_token"
                        cfg.An.browser_token;
                      Alcotest.(check string) "ingest host"
                        "https://eu.i.posthog.com" cfg.An.browser_api_host;
                      Alcotest.(check string) "normalized environment"
                        "production" cfg.An.browser_deployment_environment))
        ; an_case "invalid configuration yields no browser config" (fun () ->
              AnT.install_validated_configuration ~enabled:"true"
                ~environment:"production" ~project_token:"phc_test_token"
                ~public_origin:"https://earde.com" ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  Alcotest.(check bool) "no browser config" true
                    (An.browser_config () = None)))
        ; an_case "layout renders the environment attr and no secret" (fun () ->
              install_production_config ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  let html =
                    Earde.Components.layout ~title:"T" "<p>body</p>"
                  in
                  Alcotest.(check bool) "environment attr" true
                    (contains html
                       "data-ph-deployment-environment='production'");
                  Alcotest.(check bool) "no personal key" false
                    (contains html "phx_secret_test_value");
                  Alcotest.(check bool) "no project id" false
                    (contains html "654321")))
        ] )
      (* The common event envelope: deployment_environment appears exactly
         once on every server payload, follows the closed value, and the
         browser adds it centrally in the sanitizer. *)
    ; ( "analytics_envelope"
      , [ an_case "every domain event carries the envelope exactly once"
            (fun () ->
              List.iter
                (fun (name, event) ->
                  let occurrences =
                    List.length
                      (List.filter
                         (fun k -> k = "deployment_environment")
                         (prop_keys (an_payload event)))
                  in
                  if occurrences <> 1 then
                    Alcotest.failf "%s carries the envelope %d times" name
                      occurrences)
                an_all_events)
        ; an_case "envelope value follows the closed environment" (fun () ->
              List.iter
                (fun (environment, expected) ->
                  let payload =
                    AnT.event_payload ~api_key:"phc_test" ~environment
                      ~distinct_id:"user:1" an_login
                  in
                  Alcotest.(check (option string)) expected (Some expected)
                    (match
                       List.assoc_opt "deployment_environment"
                         (payload_props payload)
                     with
                    | Some (`String v) -> Some v
                    | _ -> None))
                [ (An.Production, "production"); (An.Staging, "staging");
                  (An.Development, "development") ])
        ; an_case "$groupidentify carries the envelope exactly once" (fun () ->
              let payload =
                AnT.group_identify_payload ~api_key:"phc_test"
                  ~environment:An.Production ~distinct_id:"user:1"
                  (an_group ())
              in
              Alcotest.(check int) "once" 1
                (List.length
                   (List.filter
                      (fun k -> k = "deployment_environment")
                      (prop_keys payload)));
              Alcotest.(check bool) "not inside $group_set" false
                (List.mem_assoc "deployment_environment"
                   (group_set_of payload)))
        ; an_case "person sync carries the envelope exactly once" (fun () ->
              let payload =
                AnT.person_sync_payload ~api_key:"phc_test"
                  ~environment:An.Staging ~distinct_id:"user:1" an_person
              in
              Alcotest.(check int) "once" 1
                (List.length
                   (List.filter
                      (fun k -> k = "deployment_environment")
                      (prop_keys payload)));
              (match List.assoc_opt "$set" (payload_props payload) with
              | Some (`Assoc set) ->
                  Alcotest.(check bool) "not inside $set" false
                    (List.mem_assoc "deployment_environment" set)
              | _ -> Alcotest.fail "no $set"))
        ; an_case "browser sanitizer adds the closed value centrally" (fun () ->
              let js = read_analytics_js () in
              (* the exact closed set, validated before any SDK work *)
              Alcotest.(check bool) "closed set" true
                (contains js
                   "[\"production\", \"staging\", \"development\"]");
              Alcotest.(check bool) "invalid environment bails out" true
                (contains js
                   "DEPLOYMENT_ENVIRONMENTS.indexOf(deploymentEnvironment) \
                    === -1) return;");
              (* one central assignment, inside sanitizeProperties *)
              Alcotest.(check int) "single central assignment" 1
                (count_sub js "props.deployment_environment =");
              (match
                 ( index_of js "function sanitizeProperties",
                   index_of js "props.deployment_environment =",
                   index_of js "MASK_TEXT_SELECTOR" )
               with
              | Some sanitize, Some assign, Some after ->
                  Alcotest.(check bool) "assignment inside the sanitizer" true
                    (sanitize < assign && assign < after)
              | _ -> Alcotest.fail "sanitizer markers missing");
              (* never added at capture call sites *)
              Alcotest.(check int) "no per-capture property" 0
                (count_sub js "deployment_environment:");
              (* the gate precedes SDK loading, so an invalid environment can
                 produce no PostHog request at all *)
              match
                (index_of js "DEPLOYMENT_ENVIRONMENTS.indexOf",
                 index_of js "function loadSdk")
              with
              | Some gate, Some load ->
                  Alcotest.(check bool) "gate precedes SDK load" true
                    (gate < load)
              | _ -> Alcotest.fail "gate markers missing")
        ] )
      (* Credential/project preflight against a LOCAL stub: verified only
         when the configured project id exists and its live api_token equals
         the configured token; every failure is a bounded class; no request
         ever reaches an ingestion path; no credential is ever printed. *)
    ; ( "analytics_preflight"
      , [ check_preflight "matching project and token verifies"
            (preflight_handler ()) "verified"
        ; an_case "verified report carries the safe fields" (fun () ->
              let result, requests = run_preflight (preflight_handler ()) in
              (match result with
              | Ok r ->
                  Alcotest.(check string) "project id" "42"
                    r.An.Preflight.report_project_id;
                  Alcotest.(check bool) "staging environment" true
                    (r.An.Preflight.report_environment = An.Staging);
                  Alcotest.(check int) "short fingerprint" 12
                    (String.length r.An.Preflight.report_token_fingerprint);
                  Alcotest.(check bool) "fingerprint is not the token" false
                    (contains preflight_token
                       r.An.Preflight.report_token_fingerprint)
              | Error (c, d) -> Alcotest.failf "expected Ok, got %s: %s" c d);
              Alcotest.(check int) "exactly two metadata requests" 2
                (List.length requests);
              (match requests with
              | [ orgs; project ] ->
                  Alcotest.(check string) "orgs listing first"
                    "/api/organizations/" orgs.Api_stub.path;
                  Alcotest.(check string) "documented project retrieve"
                    (Printf.sprintf "/api/organizations/%s/projects/42/"
                       preflight_org)
                    project.Api_stub.path
              | _ -> Alcotest.fail "unexpected request sequence"))
        ; check_preflight "wrong token fails"
            (preflight_handler
               ~project_override:
                 (preflight_project_body ~id:42 ~token:"phc_other_token") ())
            "token_mismatch"
        ; check_preflight "missing project fails"
            (preflight_handler ~project_status:404 ()) "project_not_found"
        ; check_preflight "unauthorized key fails"
            (preflight_handler ~orgs_status:401 ()) "unauthorized"
        ; check_preflight "missing scope fails"
            (preflight_handler ~orgs_status:403 ()) "missing_scope"
        ; check_preflight "malformed organization listing fails"
            (preflight_handler ~orgs_override:"not json" ())
            "malformed_response"
        ; check_preflight "malformed project metadata fails"
            (preflight_handler ~project_override:{|{"unexpected": true}|} ())
            "malformed_response"
        ; check_preflight "different project id in the response fails"
            (preflight_handler
               ~project_override:
                 (preflight_project_body ~id:99 ~token:preflight_token) ())
            "project_id_mismatch"
        ; check_preflight "server error on project retrieve fails"
            (preflight_handler ~project_status:500 ()) "http_500"
        ; check_preflight "redirect behavior fails safely"
            (preflight_handler ~orgs_status:302 ()) "unexpected_redirect"
        ; check_preflight "redirect on project retrieve fails safely"
            (preflight_handler ~project_status:301 ()) "unexpected_redirect"
        ; an_case "unreachable host fails as network_failure" (fun () ->
              AnT.use_preflight_test_configuration ~environment:An.Staging
                ~ui_host:"http://127.0.0.1:9" ~project_id:"42"
                ~personal_api_key:deletion_test_key
                ~project_token:preflight_token ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  match Lwt_main.run (An.Preflight.run ()) with
                  | Error ("network_failure", detail) ->
                      Alcotest.(check bool) "no credential in detail" false
                        (contains detail preflight_token
                        || contains detail deletion_test_key)
                  | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                  | Ok _ -> Alcotest.fail "must not verify"))
        ; an_case "invalid configuration fails before any request" (fun () ->
              AnT.use_disabled_test_configuration ();
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  match Lwt_main.run (An.Preflight.run ()) with
                  | Error ("configuration_invalid", _) -> ()
                  | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                  | Ok _ -> Alcotest.fail "must not verify"))
        ; an_case "mismatched cloud regions fail statically" (fun () ->
              (* eu ingest host against the us private host: fails before any
                 network request is attempted. *)
              AnT.install_validated_configuration ~enabled:"true"
                ~environment:"staging" ~project_token:"phc_test_token"
                ~api_host:"https://eu.i.posthog.com"
                ~ui_host:"https://us.posthog.com" ~project_id:"42"
                ~personal_api_key:"phx_test_dummy"
                ~public_origin:"https://staging.example.com" ()
              ;
              Fun.protect ~finally:AnT.clear_configuration_override (fun () ->
                  match Lwt_main.run (An.Preflight.run ()) with
                  | Error ("region_mismatch", _) -> ()
                  | Error (c, _) -> Alcotest.failf "unexpected class %s" c
                  | Ok _ -> Alcotest.fail "must not verify"))
        ] )
    ; ( "mod_delete_community_scope", Mod_scope.suite )
    ; ( "db_returning_ids", Returning_ids.suite )
    ; ( "analytics_consent_db", [ consent_sync_db_case ] )
    ; ( "analytics_step6_events", Step6_events.suite )
    ; ( "posthog_deletion_jobs", Step7_deletion.suite )
    ; ( "posthog_group_cleanup", Group_cleanup.suite )
      (* GitHub onboarding mode parsing: exact canonical values after trimming,
         everything else fails closed to Off. *)
    ; ( "project_onboarding_parse"
      , [ check_ob_parse "none" Ob.Off None
        ; check_ob_parse "empty string" Ob.Off (Some "")
        ; check_ob_parse "whitespace only" Ob.Off (Some "   \t ")
        ; check_ob_parse "off" Ob.Off (Some "off")
        ; check_ob_parse "admins" Ob.Admins (Some "admins")
        ; check_ob_parse "public" Ob.Public (Some "public")
        ; check_ob_parse "trims surrounding whitespace" Ob.Admins (Some "  admins ")
        ; check_ob_parse "trims around public" Ob.Public (Some "\tpublic\n")
        ; check_ob_parse "uppercase PUBLIC rejected" Ob.Off (Some "PUBLIC")
        ; check_ob_parse "mixed case Admin rejected" Ob.Off (Some "Admin")
        ; check_ob_parse "unknown value" Ob.Off (Some "on")
        ; check_ob_parse "unknown word" Ob.Off (Some "everyone")
        ] )
    ; ( "project_onboarding_to_string"
      , [ check_ob_string "off" "off" Ob.Off
        ; check_ob_string "admins" "admins" Ob.Admins
        ; check_ob_string "public" "public" Ob.Public
        ] )
    ; ( "project_onboarding_available"
      , [ check_ob_avail "off rejects admin" false Ob.Off ~is_admin:true
        ; check_ob_avail "off rejects non-admin" false Ob.Off ~is_admin:false
        ; check_ob_avail "admins accepts admin" true Ob.Admins ~is_admin:true
        ; check_ob_avail "admins rejects non-admin" false Ob.Admins ~is_admin:false
        ; check_ob_avail "public accepts admin" true Ob.Public ~is_admin:true
        ; check_ob_avail "public accepts non-admin" true Ob.Public ~is_admin:false
        ] )
      (* Legacy generic community creation is global-admin only, and the
         policy is independent of the GitHub onboarding mode: Public must
         never reopen arbitrary community creation. *)
    ; ( "legacy_community_creation"
      , [ check_ob_legacy "admin allowed" true ~is_admin:true
        ; check_ob_legacy "non-admin denied" false ~is_admin:false
        ; check_ob_legacy_get "admin GET shows form" Ob.Show_form ~is_admin:true
        ; check_ob_legacy_get "authenticated non-admin GET resolves to /bring"
            Ob.Redirect_to_bring ~is_admin:false
        ; check_ob_legacy_get "anonymous GET resolves to /bring"
            Ob.Redirect_to_bring ~is_admin:false
        ; check_ob_legacy_post "admin POST proceeds" Ob.Show_form ~is_admin:true
        ; check_ob_legacy_post "authenticated non-admin POST forbidden"
            Ob.Forbid ~is_admin:false
        ; check_ob_legacy_post "anonymous POST forbidden" Ob.Forbid
            ~is_admin:false
        ; Alcotest.test_case "Public onboarding mode does not reopen legacy creation"
            `Quick (fun () ->
              Alcotest.(check bool) "onboarding itself is open to non-admins"
                true
                (Ob.onboarding_available Ob.Public ~is_admin:false);
              Alcotest.(check bool) "legacy creation still denied" false
                (Ob.can_use_legacy_community_creation ~is_admin:false);
              Alcotest.(check string) "legacy POST still forbidden"
                (ob_decision_str Ob.Forbid)
                (ob_decision_str
                   (Ob.legacy_creation_post_decision ~is_admin:false)))
        ] )
      (* /bring renderer: shared copy/link invariants across every mode ×
         viewer combination (see bring_shared_case). *)
    ; ( "bring_page_shared", bring_shared_cases )
      (* Mode-specific states: each closed mode renders its own controlled
         status, and only the Admins-mode admin view carries the (disabled,
         informational) action-shaped element. *)
    ; ( "bring_page_modes"
      , [ bring_case "off: clear not-enabled state" (fun () ->
              let html = render_bring ~is_admin:false Ob.Off in
              Alcotest.(check bool) "unavailable state" true
                (contains html "not currently enabled"))
        ; bring_case "off: no onboarding start action" (fun () ->
              let html = render_bring ~is_admin:false Ob.Off in
              Alcotest.(check bool) "no connect action" false
                (contains html "Connect a repository");
              Alcotest.(check bool) "no button at all" false
                (contains html "auth-btn"))
        ; bring_case "admins: admin sees private testing state" (fun () ->
              let html = render_bring ~user:"root" ~is_admin:true Ob.Admins in
              Alcotest.(check bool) "admin testing state" true
                (contains html "Private administrator testing is enabled");
              (* Status element exists but is inert — no install route to link. *)
              Alcotest.(check bool) "status element is disabled" true
                (contains html "class='auth-btn' disabled"))
        ; bring_case "admins: non-admin sees limited state only" (fun () ->
              let html = render_bring ~user:"alice" ~is_admin:false Ob.Admins in
              Alcotest.(check bool) "limited/private state" true
                (contains html "limited to a small private group");
              Alcotest.(check bool) "no admin-testing claim" false
                (contains html "Private administrator testing");
              Alcotest.(check bool) "no connect action" false
                (contains html "Connect a repository"))
        ; bring_case "admins: anonymous sees limited state only" (fun () ->
              let html = render_bring ~is_admin:false Ob.Admins in
              Alcotest.(check bool) "limited/private state" true
                (contains html "limited to a small private group");
              Alcotest.(check bool) "no admin-testing claim" false
                (contains html "Private administrator testing"))
        ; bring_case "public: config-enabled but flow in preparation" (fun () ->
              let html = render_bring ~is_admin:false Ob.Public in
              Alcotest.(check bool) "config-level enablement" true
                (contains html "enabled at the configuration level");
              Alcotest.(check bool) "flow still being prepared" true
                (contains html "still being prepared"))
        ; bring_case "public: no dead start link" (fun () ->
              let html = render_bring ~is_admin:false Ob.Public in
              Alcotest.(check bool) "no connect action" false
                (contains html "Connect a repository");
              Alcotest.(check bool) "no button at all" false
                (contains html "auth-btn"))
        ] )
      (* Viewer-aware navigation: anonymous gets real login/signup links;
         authenticated viewers are never offered signup. *)
    ; ( "bring_page_nav"
      , [ bring_case "anonymous: login and signup links" (fun () ->
              let html = render_bring ~is_admin:false Ob.Off in
              Alcotest.(check bool) "login link" true
                (contains html "href='/login'");
              Alcotest.(check bool) "signup link" true
                (contains html "href='/signup'"))
        ; bring_case "authenticated: no signup, feed link stays" (fun () ->
              let html = render_bring ~user:"alice" ~is_admin:false Ob.Off in
              Alcotest.(check bool) "no signup anywhere" false
                (contains html "/signup");
              Alcotest.(check bool) "no login link" false
                (contains html "href='/login'");
              Alcotest.(check bool) "feed link" true
                (contains html "href='/feed'"))
        ] )
      (* Ordinary navigation must advertise /bring, never the admin-only
         /new-community flow (see nav_entry_cases). *)
    ; ( "app_nav_entry_points", nav_entry_cases )
      (* Publication mode strings are exact-match: no trimming, no case
         folding — off-enum values are explicit errors. *)
    ; ( "network_publication_mode"
      , [ nc_parse_ok "public parses" NC.Public "public"
        ; nc_parse_ok "unlisted parses" NC.Unlisted "unlisted"
        ; nc_parse_err "blank rejected" ""
        ; nc_parse_err "unknown rejected" "private"
        ; nc_parse_err "capitalized rejected" "Public"
        ; nc_parse_err "leading whitespace rejected" " public"
        ; nc_parse_err "trailing whitespace rejected" "unlisted "
        ; nc_case "Public serializes" (fun () ->
              Alcotest.(check string) "canonical" "public"
                (NC.string_of_publication_mode NC.Public))
        ; nc_case "Unlisted serializes" (fun () ->
              Alcotest.(check string) "canonical" "unlisted"
                (NC.string_of_publication_mode NC.Unlisted))
        ] )
      (* Every field of both publication configurations. Unlisted stays
         publicly accessible — it only opts out of indexing and discovery. *)
    ; ( "network_publication_config"
      , [ nc_case "Public: public + indexable + discoverable + published"
            (fun () ->
              nc_check_config ~visibility:Earde.Db.Community_public
                ~indexable:true ~discoverable:true
                (NC.configuration_for_publication NC.Public))
        ; nc_case "Unlisted: public, not indexable, not discoverable, published"
            (fun () ->
              nc_check_config ~visibility:Earde.Db.Community_public
                ~indexable:false ~discoverable:false
                (NC.configuration_for_publication NC.Unlisted))
        ] )
      (* Publication decision: only a network draft may publish. *)
    ; ( "network_publish_decision"
      , [ nc_publish_err "legacy draft rejected" NC.Not_a_network_community
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_draft NC.Public
        ; nc_publish_err "legacy published rejected" NC.Not_a_network_community
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published NC.Public
        ; nc_publish_err "network published rejected"
            NC.Community_already_published ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published NC.Unlisted
        ; nc_case "network draft + Public succeeds" (fun () ->
              match
                NC.publish ~is_network_community:true
                  ~onboarding_state:Earde.Db.Community_draft NC.Public
              with
              | Error e ->
                  Alcotest.failf "expected Ok, got %s"
                    (NC.string_of_publication_error e)
              | Ok c ->
                  nc_check_config ~visibility:Earde.Db.Community_public
                    ~indexable:true ~discoverable:true c)
        ; nc_case "network draft + Unlisted succeeds" (fun () ->
              match
                NC.publish ~is_network_community:true
                  ~onboarding_state:Earde.Db.Community_draft NC.Unlisted
              with
              | Error e ->
                  Alcotest.failf "expected Ok, got %s"
                    (NC.string_of_publication_error e)
              | Ok c ->
                  nc_check_config ~visibility:Earde.Db.Community_public
                    ~indexable:false ~discoverable:false c)
        ] )
      (* Full matrix: the only forbidden transition is published network
         community -> private. *)
    ; ( "network_visibility_change"
      , [ nc_vis_case "legacy draft -> public" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_draft
            ~requested_visibility:Earde.Db.Community_public
        ; nc_vis_case "legacy draft -> private" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_draft
            ~requested_visibility:Earde.Db.Community_private
        ; nc_vis_case "legacy published -> public" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~requested_visibility:Earde.Db.Community_public
        ; nc_vis_case "legacy published -> private" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~requested_visibility:Earde.Db.Community_private
        ; nc_vis_case "network draft -> public" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~requested_visibility:Earde.Db.Community_public
        ; nc_vis_case "network draft -> private" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~requested_visibility:Earde.Db.Community_private
        ; nc_vis_case "network published -> public" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~requested_visibility:Earde.Db.Community_public
        ; nc_vis_case "network published -> private forbidden" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~requested_visibility:Earde.Db.Community_private
        ] )
      (* Settings-flow gate: the decision the visibility POST handler acts on.
         None = the existing update flow continues; Some = `Conflict before any
         DB write. Legacy behavior (including existing private communities) is
         untouched; only published network communities refuse -> private. *)
    ; ( "settings_visibility_gate"
      , [ vis_gate_allowed "legacy published -> private still allowed"
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public
            ~requested_visibility:Earde.Db.Community_private
        ; vis_gate_allowed "legacy draft-shaped -> private still allowed"
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_public
            ~requested_visibility:Earde.Db.Community_private
        ; vis_gate_allowed "legacy existing private -> public unaffected"
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_private
            ~requested_visibility:Earde.Db.Community_public
        ; vis_gate_allowed "legacy existing private -> private unaffected"
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_private
            ~requested_visibility:Earde.Db.Community_private
        ; vis_gate_allowed "network draft -> private allowed"
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_private
            ~requested_visibility:Earde.Db.Community_private
        ; vis_gate_allowed "network published -> public allowed"
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public
            ~requested_visibility:Earde.Db.Community_public
        ; vis_gate_rejected "network published -> private rejected"
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public
            ~requested_visibility:Earde.Db.Community_private
        ] )
      (* Whole-state validity: legacy always passes; drafts must be fully
         hidden; published states are exactly Public or Unlisted shaped. *)
    ; ( "network_lifecycle_validity"
      , [ nc_valid_case "legacy public indexable accepted" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public ~indexable:true
            ~discoverable:false
        ; nc_valid_case "legacy private draft accepted" true
            ~is_network_community:false
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_private ~indexable:true
            ~discoverable:true
        ; nc_valid_case "canonical private draft accepted" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_private ~indexable:false
            ~discoverable:false
        ; nc_valid_case "public draft rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_public ~indexable:false
            ~discoverable:false
        ; nc_valid_case "indexable draft rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_private ~indexable:true
            ~discoverable:false
        ; nc_valid_case "discoverable draft rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_draft
            ~visibility:Earde.Db.Community_private ~indexable:false
            ~discoverable:true
        ; nc_valid_case "canonical published Public accepted" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public ~indexable:true
            ~discoverable:true
        ; nc_valid_case "canonical published Unlisted accepted" true
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public ~indexable:false
            ~discoverable:false
        ; nc_valid_case "published private rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_private ~indexable:true
            ~discoverable:true
        ; nc_valid_case "published indexable-only rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public ~indexable:true
            ~discoverable:false
        ; nc_valid_case "published discoverable-only rejected" false
            ~is_network_community:true
            ~onboarding_state:Earde.Db.Community_published
            ~visibility:Earde.Db.Community_public ~indexable:false
            ~discoverable:true
        ] )
      (* GitHub account type strings are exact-match: no trimming, no case
         folding — off-enum values are explicit errors. *)
    ; ( "github_account_type"
      , [ go_account_ok "user parses" GO.User "user"
        ; go_account_ok "organization parses" GO.Organization "organization"
        ; go_account_err "blank rejected" ""
        ; go_account_err "unknown rejected" "bot"
        ; go_account_err "capitalized User rejected" "User"
        ; go_account_err "capitalized Organization rejected" "Organization"
        ; go_account_err "leading whitespace rejected" " user"
        ; go_account_err "trailing whitespace rejected" "organization "
        ; go_case "User serializes" (fun () ->
              Alcotest.(check string) "canonical" "user"
                (GO.string_of_account_type GO.User))
        ; go_case "Organization serializes" (fun () ->
              Alcotest.(check string) "canonical" "organization"
                (GO.string_of_account_type GO.Organization))
        ] )
      (* Installation status parsing follows the same exact-match rules. *)
    ; ( "github_installation_status"
      , [ go_status_ok "active parses" GO.Active "active"
        ; go_status_ok "revoked parses" GO.Revoked "revoked"
        ; go_status_ok "inaccessible parses" GO.Inaccessible "inaccessible"
        ; go_status_err "blank rejected" ""
        ; go_status_err "unknown rejected" "suspended"
        ; go_status_err "capitalized rejected" "Active"
        ; go_status_err "uppercase rejected" "REVOKED"
        ; go_status_err "leading whitespace rejected" " active"
        ; go_status_err "trailing whitespace rejected" "inaccessible "
        ; go_case "Active serializes" (fun () ->
              Alcotest.(check string) "canonical" "active"
                (GO.string_of_installation_status GO.Active))
        ; go_case "Revoked serializes" (fun () ->
              Alcotest.(check string) "canonical" "revoked"
                (GO.string_of_installation_status GO.Revoked))
        ; go_case "Inaccessible serializes" (fun () ->
              Alcotest.(check string) "canonical" "inaccessible"
                (GO.string_of_installation_status GO.Inaccessible))
        ] )
      (* Full 3x3 matrix: Active and Inaccessible move freely (including the
         Inaccessible -> Active recovery); Revoked is terminal. *)
    ; ( "github_installation_transitions"
      , [ go_transition "active -> active" true ~from_:GO.Active ~to_:GO.Active
        ; go_transition "active -> inaccessible" true ~from_:GO.Active
            ~to_:GO.Inaccessible
        ; go_transition "active -> revoked" true ~from_:GO.Active
            ~to_:GO.Revoked
        ; go_transition "inaccessible -> active recovers" true
            ~from_:GO.Inaccessible ~to_:GO.Active
        ; go_transition "inaccessible -> inaccessible" true
            ~from_:GO.Inaccessible ~to_:GO.Inaccessible
        ; go_transition "inaccessible -> revoked" true ~from_:GO.Inaccessible
            ~to_:GO.Revoked
        ; go_transition "revoked -> active forbidden" false ~from_:GO.Revoked
            ~to_:GO.Active
        ; go_transition "revoked -> inaccessible forbidden" false
            ~from_:GO.Revoked ~to_:GO.Inaccessible
        ; go_transition "revoked -> revoked" true ~from_:GO.Revoked
            ~to_:GO.Revoked
        ] )
      (* Onboarding flow: single closed value, exact-match, no fallback. *)
    ; ( "github_onboarding_flow"
      , [ go_flow_ok "project_onboarding parses" "project_onboarding"
        ; go_flow_err "blank rejected" ""
        ; go_flow_err "unknown rejected" "repo_onboarding"
        ; go_flow_err "case variant rejected" "Project_onboarding"
        ; go_flow_err "leading whitespace rejected" " project_onboarding"
        ; go_flow_err "trailing whitespace rejected" "project_onboarding "
        ; go_case "Project_onboarding serializes" (fun () ->
              Alcotest.(check string) "canonical" "project_onboarding"
                (GO.string_of_flow GO.Project_onboarding))
        ] )
      (* Full status x has_revoked_at matrix: revoked_at is forbidden on
         non-revoked rows; a revoked row may have it or still lack it. *)
    ; ( "github_revoked_at_coherence"
      , [ go_revoked_at "active without revoked_at" true ~status:GO.Active
            ~has_revoked_at:false
        ; go_revoked_at "active with revoked_at forbidden" false
            ~status:GO.Active ~has_revoked_at:true
        ; go_revoked_at "inaccessible without revoked_at" true
            ~status:GO.Inaccessible ~has_revoked_at:false
        ; go_revoked_at "inaccessible with revoked_at forbidden" false
            ~status:GO.Inaccessible ~has_revoked_at:true
        ; go_revoked_at "revoked without revoked_at (newly marked)" true
            ~status:GO.Revoked ~has_revoked_at:false
        ; go_revoked_at "revoked with revoked_at" true ~status:GO.Revoked
            ~has_revoked_at:true
        ] )
      (* Generated tokens must already be in the canonical wire form the
         parser accepts; assertions are structural so no raw generated
         value can reach test output. *)
    ; ( "github_crypto_generation"
      , [ goc_case "generated state is canonical 32 bytes" (fun () ->
              let encoded = GOC.state_to_string (GOC.generate_state ()) in
              Alcotest.(check bool) "canonical" true (goc_canonical_32 encoded))
        ; goc_case "generated state re-parses" (fun () ->
              let encoded = GOC.state_to_string (GOC.generate_state ()) in
              match GOC.state_of_callback encoded with
              | Ok _ -> ()
              | Error GOC.Invalid_format ->
                  Alcotest.fail "generated state rejected by parser")
        ; goc_case "generated binding is canonical 32 bytes" (fun () ->
              let encoded =
                GOC.session_binding_to_string (GOC.generate_session_binding ())
              in
              Alcotest.(check bool) "canonical" true (goc_canonical_32 encoded))
        ; goc_case "generated binding re-parses" (fun () ->
              let encoded =
                GOC.session_binding_to_string (GOC.generate_session_binding ())
              in
              match GOC.session_binding_of_string encoded with
              | Ok _ -> ()
              | Error GOC.Invalid_format ->
                  Alcotest.fail "generated binding rejected by parser")
        ] )
      (* Only the canonical encoding of exactly 32 bytes parses; every other
         spelling — padded, whitespace-wrapped, malformed, wrong length,
         non-zero trailing bits — is the payload-free Invalid_format. *)
    ; ( "github_crypto_state_parse"
      , [ goc_state_ok "canonical 32-byte fixture parses" (goc_fixture 'A')
        ; goc_state_ok "all-zero canonical fixture parses"
            (Dream.to_base64url (String.make 32 '\000'))
        ; goc_state_err "blank rejected" ""
        ; goc_state_err "padded rejected" (goc_fixture 'A' ^ "=")
        ; goc_state_err "malformed rejected" "not/base64url+data!"
        ; goc_state_err "leading whitespace rejected" (" " ^ goc_fixture 'A')
        ; goc_state_err "trailing newline rejected" (goc_fixture 'A' ^ "\n")
        ; goc_state_err "31-byte value rejected"
            (Dream.to_base64url (String.make 31 'A'))
        ; goc_state_err "33-byte value rejected"
            (Dream.to_base64url (String.make 33 'A'))
        ; goc_state_err "non-canonical trailing bits rejected"
            (String.make 42 'A' ^ "B")
        ] )
    ; ( "github_crypto_binding_parse"
      , [ goc_binding_ok "canonical 32-byte fixture parses" (goc_fixture 'B')
        ; goc_binding_err "blank rejected" ""
        ; goc_binding_err "padded rejected" (goc_fixture 'B' ^ "==")
        ; goc_binding_err "malformed rejected" "\xff\xfe not a token"
        ; goc_binding_err "leading whitespace rejected" (" " ^ goc_fixture 'B')
        ; goc_binding_err "trailing whitespace rejected" (goc_fixture 'B' ^ " ")
        ; goc_binding_err "31-byte value rejected"
            (Dream.to_base64url (String.make 31 'B'))
        ; goc_binding_err "33-byte value rejected"
            (Dream.to_base64url (String.make 33 'B'))
        ; goc_binding_err "non-canonical trailing bits rejected"
            (String.make 42 'A' ^ "C")
        ] )
      (* Hashes are deterministic 64-char lowercase hex, domain-separated
         between state and session binding, and free of raw token
         material. *)
    ; ( "github_crypto_hashing"
      , [ goc_case "state hashing is deterministic" (fun () ->
              Alcotest.(check string) "same hash"
                (goc_state_hash (goc_fixture 'C'))
                (goc_state_hash (goc_fixture 'C')))
        ; goc_case "binding hashing is deterministic" (fun () ->
              Alcotest.(check string) "same hash"
                (goc_binding_hash (goc_fixture 'C'))
                (goc_binding_hash (goc_fixture 'C')))
        ; goc_case "state hash is 64-char lowercase hex" (fun () ->
              Alcotest.(check bool) "hex64" true
                (goc_is_hex64 (goc_state_hash (goc_fixture 'D'))))
        ; goc_case "binding hash is 64-char lowercase hex" (fun () ->
              Alcotest.(check bool) "hex64" true
                (goc_is_hex64 (goc_binding_hash (goc_fixture 'D'))))
        ; goc_case "domains separate identical token material" (fun () ->
              Alcotest.(check bool) "hashes differ" false
                (String.equal
                   (goc_state_hash (goc_fixture 'E'))
                   (goc_binding_hash (goc_fixture 'E'))))
        ; goc_case "state hash does not contain the raw token" (fun () ->
              let token = goc_fixture 'F' in
              Alcotest.(check bool) "no token material" false
                (goc_contains ~needle:token (goc_state_hash token)))
        ; goc_case "binding hash does not contain the raw token" (fun () ->
              let token = goc_fixture 'F' in
              Alcotest.(check bool) "no token material" false
                (goc_contains ~needle:token (goc_binding_hash token)))
        ] )
      (* Generated verifiers must already be in the canonical wire form the
         parser accepts; assertions are structural so no generated value can
         reach test output. *)
    ; ( "github_pkce_generation"
      , [ gpk_case "generated verifier is 43 chars" (fun () ->
              let encoded =
                GPK.verifier_to_string (GPK.generate_verifier ())
              in
              Alcotest.(check int) "length" 43 (String.length encoded))
        ; gpk_case "generated verifier decodes to 32 bytes" (fun () ->
              let encoded =
                GPK.verifier_to_string (GPK.generate_verifier ())
              in
              match Dream.from_base64url encoded with
              | Some raw ->
                  Alcotest.(check int) "decoded bytes" 32 (String.length raw)
              | None -> Alcotest.fail "generated verifier does not decode")
        ; gpk_case "generated verifier has no padding" (fun () ->
              let encoded =
                GPK.verifier_to_string (GPK.generate_verifier ())
              in
              Alcotest.(check bool) "no '='" false
                (String.contains encoded '='))
        ; gpk_case "generated verifier re-parses and round-trips" (fun () ->
              let encoded =
                GPK.verifier_to_string (GPK.generate_verifier ())
              in
              match GPK.verifier_of_string encoded with
              | Ok reparsed ->
                  Alcotest.(check bool) "round-trips exactly" true
                    (String.equal encoded (GPK.verifier_to_string reparsed))
              | Error GPK.Invalid_format ->
                  Alcotest.fail "generated verifier rejected by parser")
        ] )
      (* Only the canonical encoding of exactly 32 bytes parses; every other
         spelling — padded, whitespace-wrapped, malformed, wrong length,
         non-zero trailing bits — is the payload-free Invalid_format. *)
    ; ( "github_pkce_verifier_parse"
      , [ gpk_verifier_ok "canonical 32-byte fixture parses" (goc_fixture 'P')
        ; gpk_verifier_err "blank rejected" ""
        ; gpk_verifier_err "malformed rejected" "not/base64url+data!"
        ; gpk_verifier_err "padded rejected" (goc_fixture 'P' ^ "=")
        ; gpk_verifier_err "leading whitespace rejected"
            (" " ^ goc_fixture 'P')
        ; gpk_verifier_err "trailing whitespace rejected"
            (goc_fixture 'P' ^ " ")
        ; gpk_verifier_err "31-byte value rejected"
            (Dream.to_base64url (String.make 31 'P'))
        ; gpk_verifier_err "33-byte value rejected"
            (Dream.to_base64url (String.make 33 'P'))
        ; gpk_verifier_err "non-canonical trailing bits rejected"
            (String.make 42 'A' ^ "B")
        ] )
      (* S256 challenges: deterministic canonical 43-char Base64url of the
         raw 32-byte digest, pinned to the RFC 7636 appendix-B vector so the
         exact construction (string hashed, no hex, no separator) cannot
         silently drift. *)
    ; ( "github_pkce_challenge"
      , [ gpk_case "same verifier gives the same challenge" (fun () ->
              Alcotest.(check string) "deterministic"
                (gpk_challenge (goc_fixture 'Q'))
                (gpk_challenge (goc_fixture 'Q')))
        ; gpk_case "challenge is 43 chars" (fun () ->
              Alcotest.(check int) "length" 43
                (String.length (gpk_challenge (goc_fixture 'Q'))))
        ; gpk_case "challenge has no padding" (fun () ->
              Alcotest.(check bool) "no '='" false
                (String.contains (gpk_challenge (goc_fixture 'Q')) '='))
        ; gpk_case "challenge is canonical Base64url of 32 bytes" (fun () ->
              Alcotest.(check bool) "canonical" true
                (goc_canonical_32 (gpk_challenge (goc_fixture 'Q'))))
        ; gpk_case "different verifiers give different challenges" (fun () ->
              Alcotest.(check bool) "challenges differ" false
                (String.equal
                   (gpk_challenge (goc_fixture 'Q'))
                   (gpk_challenge (goc_fixture 'R'))))
        ; gpk_case "RFC 7636 S256 reference vector" (fun () ->
              Alcotest.(check string) "challenge"
                "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
                (gpk_challenge
                   "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))
        ] )
      (* State issuance: the TTL constant is checkable DB-free; the SQL
         contract needs Postgres and follows the EARDE_TEST_DATABASE_URL
         gate (each DB case skips without it). *)
    ; ( "github_state_store"
      , go_case "ttl_seconds is 900" (fun () ->
            Alcotest.(check int) "ttl" 900
              Earde.Github_onboarding_state_store.ttl_seconds)
        :: Gh_state_store.suite )
      (* Valid shapes and canonicalization: accessors must return the stored
         canonical values (trimmed, lowercase scheme/host, no trailing slash,
         no default port), not the raw environment spellings. *)
    ; ( "github_app_config_valid"
      , [ gac_case "production https configuration" (fun () ->
              let c = gac_ok_exn "production" (gac_of_values ()) in
              Alcotest.(check string) "origin" "https://earde.com"
                (GAC.public_origin c);
              Alcotest.(check string) "slug" "earde-connect" (GAC.app_slug c);
              Alcotest.(check string) "client id" "Iv1.8a61f9b3a7aba766"
                (GAC.client_id c);
              Alcotest.(check string) "setup url"
                "https://earde.com/integrations/github/install/return"
                (GAC.setup_url c);
              Alcotest.(check string) "callback url"
                "https://earde.com/integrations/github/authorize/callback"
                (GAC.callback_url c))
        ; gac_case "localhost http with port" (fun () ->
              let c =
                gac_ok_exn "localhost"
                  (gac_of_values ~origin:(Some "http://localhost:8080")
                     ~setup:
                       (Some
                          "http://localhost:8080/integrations/github/install/return")
                     ~callback:
                       (Some
                          "http://localhost:8080/integrations/github/authorize/callback")
                     ())
              in
              Alcotest.(check string) "origin" "http://localhost:8080"
                (GAC.public_origin c);
              Alcotest.(check string) "setup url"
                "http://localhost:8080/integrations/github/install/return"
                (GAC.setup_url c))
        ; gac_case "ipv6 loopback http with port" (fun () ->
              let c =
                gac_ok_exn "ipv6 loopback"
                  (gac_of_values ~origin:(Some "http://[::1]:8080")
                     ~setup:
                       (Some
                          "http://[::1]:8080/integrations/github/install/return")
                     ~callback:
                       (Some
                          "http://[::1]:8080/integrations/github/authorize/callback")
                     ())
              in
              Alcotest.(check string) "origin" "http://[::1]:8080"
                (GAC.public_origin c))
        ; gac_case "surrounding whitespace is trimmed" (fun () ->
              let c =
                gac_ok_exn "whitespace"
                  (gac_of_values ~origin:(Some "  https://earde.com\n")
                     ~slug:(Some "\tearde-connect ")
                     ~client:(Some " Iv1.8a61f9b3a7aba766 ")
                     ~setup:
                       (Some
                          " https://earde.com/integrations/github/install/return ")
                     ~callback:
                       (Some
                          "\thttps://earde.com/integrations/github/authorize/callback\n")
                     ())
              in
              Alcotest.(check string) "origin" "https://earde.com"
                (GAC.public_origin c);
              Alcotest.(check string) "slug" "earde-connect" (GAC.app_slug c);
              Alcotest.(check string) "client id" "Iv1.8a61f9b3a7aba766"
                (GAC.client_id c);
              Alcotest.(check string) "setup url"
                "https://earde.com/integrations/github/install/return"
                (GAC.setup_url c))
        ; gac_case "origin and urls are canonicalized" (fun () ->
              let c =
                gac_ok_exn "canonicalization"
                  (gac_of_values ~origin:(Some "HTTPS://Earde.COM:443/")
                     ~setup:
                       (Some
                          "https://earde.com:443/integrations/github/install/return")
                     ())
              in
              Alcotest.(check string) "origin" "https://earde.com"
                (GAC.public_origin c);
              Alcotest.(check string) "setup url"
                "https://earde.com/integrations/github/install/return"
                (GAC.setup_url c);
              Alcotest.(check string) "callback url"
                "https://earde.com/integrations/github/authorize/callback"
                (GAC.callback_url c))
        ] )
    ; ( "github_app_config_missing"
      , [ gac_err "missing public origin" (GAC.Missing GAC.Public_origin)
            (gac_of_values ~origin:None ())
        ; gac_err "missing app slug" (GAC.Missing GAC.App_slug)
            (gac_of_values ~slug:None ())
        ; gac_err "missing client id" (GAC.Missing GAC.Client_id)
            (gac_of_values ~client:None ())
        ; gac_err "missing setup url" (GAC.Missing GAC.Setup_url)
            (gac_of_values ~setup:None ())
        ; gac_err "missing callback url" (GAC.Missing GAC.Callback_url)
            (gac_of_values ~callback:None ())
        ] )
      (* The origin must be an absolute https (or loopback http) origin with
         nothing else on it — no userinfo, query, fragment or path. *)
    ; ( "github_app_config_origin"
      , [ gac_err "blank rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "   ") ())
        ; gac_err "relative url rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "/app") ())
        ; gac_err "ftp scheme rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "ftp://earde.com") ())
        ; gac_err "non-loopback http rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "http://earde.com") ())
        ; gac_err "userinfo rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "https://user:pw@earde.com") ())
        ; gac_err "query rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "https://earde.com?x=1") ())
        ; gac_err "fragment rejected" (GAC.Invalid GAC.Public_origin)
            (gac_of_values ~origin:(Some "https://earde.com#frag") ())
        ; gac_err "non-root path rejected"
            (GAC.Unexpected_path GAC.Public_origin)
            (gac_of_values ~origin:(Some "https://earde.com/app") ())
        ] )
    ; ( "github_app_config_slug"
      , [ gac_case "representative slug accepted" (fun () ->
              let c =
                gac_ok_exn "slug" (gac_of_values ~slug:(Some "my-app-01") ())
              in
              Alcotest.(check string) "slug" "my-app-01" (GAC.app_slug c))
        ; gac_err "uppercase rejected (not silently lowercased)"
            (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "My-App") ())
        ; gac_err "leading dash rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "-app") ())
        ; gac_err "trailing dash rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "app-") ())
        ; gac_err "slash rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "my/app") ())
        ; gac_err "dot rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "my.app") ())
        ; gac_err "interior whitespace rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "my app") ())
        ; gac_err "empty rejected" (GAC.Invalid GAC.App_slug)
            (gac_of_values ~slug:(Some "") ())
        ] )
      (* Opaque identifier: no GitHub-specific prefix or length is imposed,
         but whitespace and control bytes are rejected. *)
    ; ( "github_app_config_client_id"
      , [ gac_case "unprefixed opaque id accepted" (fun () ->
              let c =
                gac_ok_exn "client id"
                  (gac_of_values ~client:(Some "0123456789abcdef") ())
              in
              Alcotest.(check string) "client id" "0123456789abcdef"
                (GAC.client_id c))
        ; gac_err "blank rejected" (GAC.Invalid GAC.Client_id)
            (gac_of_values ~client:(Some "  ") ())
        ; gac_err "interior space rejected" (GAC.Invalid GAC.Client_id)
            (gac_of_values ~client:(Some "Iv1. abc") ())
        ; gac_err "interior tab rejected" (GAC.Invalid GAC.Client_id)
            (gac_of_values ~client:(Some "Iv1.\tabc") ())
        ; gac_err "interior newline rejected" (GAC.Invalid GAC.Client_id)
            (gac_of_values ~client:(Some "Iv1.\nabc") ())
        ; gac_err "control character rejected" (GAC.Invalid GAC.Client_id)
            (gac_of_values ~client:(Some "Iv1.\x01abc") ())
        ] )
      (* The registered setup URL must sit exactly on the public origin
         (scheme, host, effective port) and use exactly the registered
         install-return path. *)
    ; ( "github_app_config_setup_url"
      , [ gac_err "wrong host" (GAC.Origin_mismatch GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://evil.example/integrations/github/install/return")
               ())
        ; gac_err "wrong scheme" (GAC.Origin_mismatch GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some "http://earde.com/integrations/github/install/return")
               ())
        ; gac_err "wrong effective port" (GAC.Origin_mismatch GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://earde.com:8443/integrations/github/install/return")
               ())
        ; gac_err "wrong path" (GAC.Unexpected_path GAC.Setup_url)
            (gac_of_values
               ~setup:(Some "https://earde.com/integrations/github/install")
               ())
        ; gac_err "trailing slash on path" (GAC.Unexpected_path GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://earde.com/integrations/github/install/return/")
               ())
        ; gac_err "query rejected" (GAC.Invalid GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://earde.com/integrations/github/install/return?ok=1")
               ())
        ; gac_err "fragment rejected" (GAC.Invalid GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://earde.com/integrations/github/install/return#done")
               ())
        ; gac_err "userinfo rejected" (GAC.Invalid GAC.Setup_url)
            (gac_of_values
               ~setup:
                 (Some
                    "https://u:p@earde.com/integrations/github/install/return")
               ())
        ] )
    ; ( "github_app_config_callback_url"
      , [ gac_err "wrong host" (GAC.Origin_mismatch GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://evil.example/integrations/github/authorize/callback")
               ())
        ; gac_err "wrong scheme" (GAC.Origin_mismatch GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "http://earde.com/integrations/github/authorize/callback")
               ())
        ; gac_err "wrong effective port"
            (GAC.Origin_mismatch GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://earde.com:8443/integrations/github/authorize/callback")
               ())
        ; gac_err "setup path in callback slot"
            (GAC.Unexpected_path GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://earde.com/integrations/github/install/return")
               ())
        ; gac_err "query rejected" (GAC.Invalid GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://earde.com/integrations/github/authorize/callback?a=b")
               ())
        ; gac_err "fragment rejected" (GAC.Invalid GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://earde.com/integrations/github/authorize/callback#x")
               ())
        ; gac_err "userinfo rejected" (GAC.Invalid GAC.Callback_url)
            (gac_of_values
               ~callback:
                 (Some
                    "https://u:p@earde.com/integrations/github/authorize/callback")
               ())
        ] )
      (* Diagnostics name the field (its env var) and reason only; the
         supplied value must never appear, so errors stay safe to log. *)
    ; ( "github_app_config_error_privacy"
      , [ gac_case "origin error does not echo the value" (fun () ->
              match
                gac_of_values ~origin:(Some "ftp://private-host.internal") ()
              with
              | Ok _ -> Alcotest.fail "expected Error, got Ok"
              | Error e ->
                  let msg = GAC.string_of_error e in
                  Alcotest.(check bool) "no supplied value" false
                    (goc_contains ~needle:"private-host" msg);
                  Alcotest.(check bool) "names the field" true
                    (goc_contains ~needle:"EARDE_PUBLIC_ORIGIN" msg))
        ; gac_case "setup url error does not echo the value" (fun () ->
              match
                gac_of_values
                  ~setup:
                    (Some
                       "https://evil.attacker.example/integrations/github/install/return")
                  ()
              with
              | Ok _ -> Alcotest.fail "expected Error, got Ok"
              | Error e ->
                  let msg = GAC.string_of_error e in
                  Alcotest.(check bool) "no supplied value" false
                    (goc_contains ~needle:"evil.attacker.example" msg);
                  Alcotest.(check bool) "names the field" true
                    (goc_contains ~needle:"GITHUB_APP_SETUP_URL" msg))
        ; gac_case "client id error does not echo the value" (fun () ->
              match
                gac_of_values ~client:(Some "SECRET VALUE 123") ()
              with
              | Ok _ -> Alcotest.fail "expected Error, got Ok"
              | Error e ->
                  let msg = GAC.string_of_error e in
                  Alcotest.(check bool) "no supplied value" false
                    (goc_contains ~needle:"SECRET" msg);
                  Alcotest.(check bool) "names the field" true
                    (goc_contains ~needle:"GITHUB_APP_CLIENT_ID" msg))
        ] )
      (* Installation URL: fixed GitHub origin, slug-derived path, and
         exactly one query parameter — the raw one-time state. *)
    ; ( "github_onboarding_urls_installation"
      , [ gou_case "scheme and host are fixed" (fun () ->
              let uri = Uri.of_string (gou_installation ()) in
              Alcotest.(check (option string)) "scheme" (Some "https")
                (Uri.scheme uri);
              Alcotest.(check (option string)) "host" (Some "github.com")
                (Uri.host uri))
        ; gou_case "path is the slug installation path" (fun () ->
              let uri = Uri.of_string (gou_installation ()) in
              Alcotest.(check string) "path"
                "/apps/earde-connect/installations/new" (Uri.path uri))
        ; gou_case "query is exactly one state" (fun () ->
              let uri = Uri.of_string (gou_installation ()) in
              Alcotest.(check (list string)) "keys" [ "state" ]
                (gou_keys uri);
              Alcotest.(check bool) "state round-trips" true
                (String.equal gou_state_string
                   (gou_single "installation" "state" uri)))
        ; gou_case "no fragment or userinfo" (fun () ->
              let uri = Uri.of_string (gou_installation ()) in
              Alcotest.(check (option string)) "fragment" None
                (Uri.fragment uri);
              Alcotest.(check (option string)) "userinfo" None
                (Uri.userinfo uri))
        ; gou_case "state is not in the path" (fun () ->
              let uri = Uri.of_string (gou_installation ()) in
              Alcotest.(check bool) "path free of state" false
                (goc_contains ~needle:gou_state_string (Uri.path uri)))
        ; gou_case "construction is deterministic" (fun () ->
              Alcotest.(check bool) "equal urls" true
                (String.equal (gou_installation ()) (gou_installation ())))
        ] )
      (* Authorization URL: fixed GitHub origin, the exact five OAuth+PKCE
         parameters once each, and the registered callback emitted
         verbatim. *)
    ; ( "github_onboarding_urls_authorization"
      , [ gou_case "scheme and host are fixed" (fun () ->
              let uri = Uri.of_string (gou_authorization ()) in
              Alcotest.(check (option string)) "scheme" (Some "https")
                (Uri.scheme uri);
              Alcotest.(check (option string)) "host" (Some "github.com")
                (Uri.host uri))
        ; gou_case "path is the authorize endpoint" (fun () ->
              let uri = Uri.of_string (gou_authorization ()) in
              Alcotest.(check string) "path" "/login/oauth/authorize"
                (Uri.path uri))
        ; gou_case "query keys are exactly the five, once each" (fun () ->
              let uri = Uri.of_string (gou_authorization ()) in
              Alcotest.(check (list string)) "keys" gou_authorization_keys
                (gou_keys uri))
        ; gou_case "values match their sources" (fun () ->
              let config = gou_config () in
              let uri = Uri.of_string (gou_authorization ~config ()) in
              Alcotest.(check string) "client_id" (GAC.client_id config)
                (gou_single "authorization" "client_id" uri);
              Alcotest.(check string) "redirect_uri"
                "https://earde.com/integrations/github/authorize/callback"
                (gou_single "authorization" "redirect_uri" uri);
              Alcotest.(check bool) "state round-trips" true
                (String.equal gou_state_string
                   (gou_single "authorization" "state" uri));
              Alcotest.(check bool) "challenge round-trips" true
                (String.equal
                   (GPK.challenge_to_string (gou_challenge ()))
                   (gou_single "authorization" "code_challenge" uri));
              Alcotest.(check string) "method" "S256"
                (gou_single "authorization" "code_challenge_method" uri))
        ; gou_case "no forbidden parameters" (fun () ->
              let uri = Uri.of_string (gou_authorization ()) in
              List.iter
                (fun key ->
                  Alcotest.(check int) key 0
                    (List.length (gou_entries key uri)))
                [ "scope"; "client_secret"; "code_verifier";
                  "installation_id"; "allow_signup"; "login" ])
        ; gou_case "no setup URL" (fun () ->
              let config = gou_config () in
              let url = gou_authorization ~config () in
              let values =
                List.concat_map snd (Uri.query (Uri.of_string url))
              in
              Alcotest.(check bool) "no setup value" false
                (List.exists (String.equal (GAC.setup_url config)) values);
              Alcotest.(check bool) "no setup path" false
                (goc_contains ~needle:"install/return" url))
        ; gou_case "no fragment or userinfo" (fun () ->
              let uri = Uri.of_string (gou_authorization ()) in
              Alcotest.(check (option string)) "fragment" None
                (Uri.fragment uri);
              Alcotest.(check (option string)) "userinfo" None
                (Uri.userinfo uri))
        ; gou_case "construction is deterministic" (fun () ->
              Alcotest.(check bool) "equal urls" true
                (String.equal (gou_authorization ()) (gou_authorization ())))
        ] )
      (* Reserved characters in opaque values must be query-encoded, so a
         hostile-looking client id can neither split the query nor smuggle
         extra parameters. *)
    ; ( "github_onboarding_urls_encoding"
      , [ gou_case "client id with reserved characters round-trips" (fun () ->
              let client = "Iv1.a&b=c+d?e" in
              let config =
                gac_ok_exn "reserved client id"
                  (gac_of_values ~client:(Some client) ())
              in
              let uri = Uri.of_string (gou_authorization ~config ()) in
              Alcotest.(check string) "client_id" client
                (gou_single "encoding" "client_id" uri);
              Alcotest.(check (list string)) "keys unchanged"
                gou_authorization_keys (gou_keys uri))
        ] )
      (* Nothing secret-shaped may appear in either URL: no verifier, no
         secret-marker parameter names, and the installation URL carries no
         OAuth material at all. *)
    ; ( "github_onboarding_urls_no_leakage"
      , [ gou_case "verifier absent from authorization URL" (fun () ->
              Alcotest.(check bool) "verifier absent" false
                (goc_contains ~needle:gou_verifier_string
                   (gou_authorization ())))
        ; gou_case "no secret-marker names in either URL" (fun () ->
              List.iter
                (fun url ->
                  List.iter
                    (fun needle ->
                      Alcotest.(check bool) needle false
                        (goc_contains ~needle url))
                    [ "client_secret"; "code_verifier"; "private_key";
                      "access_token" ])
                [ gou_installation (); gou_authorization () ])
        ; gou_case "installation URL has no client id or callback" (fun () ->
              let config = gou_config () in
              let url = gou_installation () in
              Alcotest.(check bool) "no client id" false
                (goc_contains ~needle:(GAC.client_id config) url);
              Alcotest.(check bool) "no callback url" false
                (goc_contains ~needle:(GAC.callback_url config) url);
              Alcotest.(check bool) "no callback path" false
                (goc_contains ~needle:"authorize/callback" url))
        ] )
    ]
