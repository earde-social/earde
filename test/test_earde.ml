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
      ; "DELETE FROM users WHERE username = 'modscope_author'"
      ]

  let q_insert_user =
    (Caqti_type.unit ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ('modscope_author', 'modscope_author@test.invalid', 'x', TRUE) RETURNING id"

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

  let suite = [ posts_case; comments_case ]
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
    ; ( "mod_delete_community_scope", Mod_scope.suite )
    ]
