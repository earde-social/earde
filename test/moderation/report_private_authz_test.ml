(* === Report flow private-community authorization ==========================
   Regression suite for the private-target exposure in GET /c/:slug/report and
   POST /c/:slug/reports: both handlers must run can_view_community BEFORE any
   ban check or target resolution, and deny an outsider with the canonical
   community_not_found 404 — byte-identical to a missing community, a missing
   target and a wrong-typed target, leaking neither community existence nor
   target existence. Exercises the REAL handlers over a real Dream pipeline
   (router + sql_pool + memory_sessions), never the access helper alone.
   Database-gated (EARDE_TEST_DATABASE_URL). *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM reports WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM reports WHERE reporter_user_id IN (SELECT id FROM users WHERE username LIKE 'rpta_%')"
    ; "DELETE FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'rpta_%')"
    ; "DELETE FROM mod_actions WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM comments WHERE post_id IN (SELECT id FROM posts WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%'))"
    ; "DELETE FROM posts WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM community_bans WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'rpta-%'"
    ; "DELETE FROM users WHERE username LIKE 'rpta_%'"
    ]

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
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)
     VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

let q_insert_community =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, sections_enabled, visibility)
     VALUES ($1, $1, FALSE, $2) RETURNING id"

let q_insert_post =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO posts (title, content, community_id, user_id)
     VALUES ($1, $1 || ' body', $2, $3) RETURNING id"

let q_insert_comment =
  (Caqti_type.(t3 string int int) ->! Caqti_type.int)
    "INSERT INTO comments (content, post_id, user_id)
     VALUES ($1, $2, $3) RETURNING id"

let q_add_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_add_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES ($1, $2, 'mod')"

(* The admin override is decided on the DURABLE users.is_admin row, which
   the session claim only enables the lookup for. *)
let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

let q_add_community_ban =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_bans (user_id, community_id) VALUES ($1, $2)"

let q_set_globally_banned =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_banned = TRUE WHERE id = $1"

let q_count_reports =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*) FROM reports WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"

let q_count_mod_actions =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*) FROM mod_actions WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'rpta-%')"

let q_count_notifications =
  (Caqti_type.unit ->! Caqti_type.int)
    "SELECT COUNT(*) FROM notifications WHERE user_id IN (SELECT id FROM users WHERE username LIKE 'rpta_%')"

(* Distinctive markers that must NEVER appear in a denied response. *)
let secret_post_title = "RPTA-PRIV-SECRET post title"

let secret_comment_body = "RPTA-PRIV-SECRET comment body"

type fx = {
  outsider : int;
  member : int;
  moderator : int;
  admin : int;
  pub_author : int;
  priv_author : int;
  pub : int;
  pub_post : int;
  priv_post : int;
  pub_comment : int;
  priv_comment : int;
}

let make_fixtures (module C : Caqti_lwt.CONNECTION) =
  let user name =
    let* id = C.find q_insert_user name in
    or_fail name id
  in
  let* outsider = user "rpta_outsider" in
  let* member = user "rpta_member" in
  let* moderator = user "rpta_moderator" in
  let* admin = user "rpta_admin" in
  let* r = C.exec q_make_admin admin in
  let* () = or_fail "durable admin" r in
  let* pub_author = user "rpta_pubauthor" in
  let* priv_author = user "rpta_privauthor" in
  let* pub = C.find q_insert_community ("rpta-pub", "public") in
  let* pub = or_fail "pub community" pub in
  let* priv = C.find q_insert_community ("rpta-priv", "private") in
  let* priv = or_fail "priv community" priv in
  (* member and priv_author are members of the private community; the
     moderator has ONLY a moderator row (no membership) — mod access must
     not depend on a membership row. *)
  let* r = C.exec q_add_member (member, priv) in
  let* () = or_fail "member row" r in
  let* r = C.exec q_add_member (priv_author, priv) in
  let* () = or_fail "priv author member row" r in
  let* r = C.exec q_add_moderator (moderator, priv) in
  let* () = or_fail "moderator row" r in
  let* pub_post = C.find q_insert_post ("rpta public post", pub, pub_author) in
  let* pub_post = or_fail "pub post" pub_post in
  let* priv_post = C.find q_insert_post (secret_post_title, priv, priv_author) in
  let* priv_post = or_fail "priv post" priv_post in
  let* pub_comment = C.find q_insert_comment ("rpta public comment", pub_post, pub_author) in
  let* pub_comment = or_fail "pub comment" pub_comment in
  let* priv_comment = C.find q_insert_comment (secret_comment_body, priv_post, priv_author) in
  let* priv_comment = or_fail "priv comment" priv_comment in
  Lwt.return
    { outsider; member; moderator; admin; pub_author; priv_author;
      pub; pub_post; priv_post; pub_comment; priv_comment }

let form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let router =
  Dream.router
    [ Dream.get "/c/:slug/report" Earde.Handlers.report_form_handler
    ; Dream.post "/c/:slug/reports" Earde.Handlers.create_report_handler
    ]

(* One request against the real pipeline. A [form] makes it a POST and
   injects a fresh, VALID dream.csrf minted under the same session — so a
   denied POST is denied by authorization, never by CSRF. *)
let run ~url ?(session = []) ?form target =
  let pipeline =
    Dream.sql_pool url @@ Dream.memory_sessions @@ fun req ->
    let* () =
      Lwt_list.iter_s
        (fun (k, v) -> Dream.set_session_field req k v)
        session
    in
    (match form with
     | None -> ()
     | Some fields ->
         let csrf = Dream.csrf_token req in
         Dream.set_body req (form_body (("dream.csrf", csrf) :: fields)));
    router req
  in
  let method_ = match form with Some _ -> `POST | None -> `GET in
  let headers =
    match form with
    | Some _ -> [ ("Content-Type", "application/x-www-form-urlencoded") ]
    | None -> []
  in
  let request = Dream.request ~method_ ~target ~headers "" in
  let* response = pipeline request in
  let* body = Dream.body response in
  Lwt.return
    ( Dream.status_to_int (Dream.status response),
      Dream.header response "Location",
      body )

let session_of ?(admin = false) uid name =
  [ ("user_id", string_of_int uid); ("username", name) ]
  @ if admin then [ ("is_admin", "true") ] else []

let find_sub hay needle from =
  let nl = String.length needle and hl = String.length hay in
  let rec go i =
    if i + nl > hl then None
    else if String.sub hay i nl = needle then Some i
    else go (i + 1)
  in
  if nl = 0 then Some from else go from

let has_sub hay needle = find_sub hay needle 0 <> None

(* Blank the one volatile value in an authorized form document: the
   dream.csrf hidden input's value. Everything else must be stable. *)
let mask_csrf body =
  match find_sub body "dream.csrf" 0 with
  | None -> body
  | Some i ->
      (match find_sub body "value=\"" i with
       | None -> body
       | Some j ->
           let vstart = j + String.length "value=\"" in
           (match String.index_from_opt body vstart '"' with
            | None -> body
            | Some vend ->
                String.sub body 0 vstart
                ^ "CSRF"
                ^ String.sub body vend (String.length body - vend)))

let check_no_leak label body =
  Alcotest.(check bool) (label ^ ": no post excerpt") false
    (has_sub body secret_post_title);
  Alcotest.(check bool) (label ^ ": no comment excerpt") false
    (has_sub body secret_comment_body);
  Alcotest.(check bool) (label ^ ": no private community name") false
    (has_sub body "rpta-priv")

let get_target slug ty id =
  Printf.sprintf "/c/%s/report?type=%s&id=%d" slug ty id

let post_form ty id =
  [ ("target_type", ty); ("target_id", string_of_int id);
    ("reason", "spam"); ("details", "rpta details") ]

let is_redirect s = s = 301 || s = 302 || s = 303 || s = 307 || s = 308

let submit_ok ~url ~label ~session ~slug ~ty ~id () =
  let* status, _, body =
    run ~url ~session ~form:(post_form ty id) ("/c/" ^ slug ^ "/reports")
  in
  Alcotest.(check int) (label ^ ": POST status") 200 status;
  Alcotest.(check bool) (label ^ ": submitted") true
    (has_sub body "Report submitted");
  Lwt.return_unit

let open_form_ok ~url ~label ~session ~slug ~ty ~id () =
  let* status, _, body = run ~url ~session (get_target slug ty id) in
  Alcotest.(check int) (label ^ ": GET status") 200 status;
  Alcotest.(check bool) (label ^ ": form present") true
    (has_sub body ("<form action='/c/" ^ slug ^ "/reports' method='POST'"));
  Lwt.return body

(* 1: anonymous access keeps its existing contract (redirect to /login). *)
let anonymous_case =
  db_case "anonymous GET and POST redirect to /login, nothing rendered"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let* status, location, body =
        run ~url (get_target "rpta-priv" "post" fx.priv_post)
      in
      Alcotest.(check bool) "GET redirects" true (is_redirect status);
      Alcotest.(check (option string)) "GET location" (Some "/login") location;
      check_no_leak "anonymous GET" body;
      let* status, location, _ =
        run ~url ~form:(post_form "post" fx.priv_post) "/c/rpta-priv/reports"
      in
      Alcotest.(check bool) "POST redirects" true (is_redirect status);
      Alcotest.(check (option string)) "POST location" (Some "/login") location;
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* n = C.find q_count_reports () in
      let* n = or_fail "count" n in
      Alcotest.(check int) "no report row" 0 n;
      Lwt.return_unit)

(* 2+3: public-community reporting still works for members AND non-members. *)
let public_reporting_case =
  db_case "public community: member and non-member can open and submit"
    (fun ~url c ->
      let* fx = make_fixtures c in
      (* rpta_member is a member of the PRIVATE community only, so against
         the public community it is a plain authenticated non-member. *)
      let outsider = session_of fx.outsider "rpta_outsider" in
      let member = session_of fx.member "rpta_member" in
      let* _ =
        open_form_ok ~url ~label:"non-member post form" ~session:outsider
          ~slug:"rpta-pub" ~ty:"post" ~id:fx.pub_post ()
      in
      let* _ =
        open_form_ok ~url ~label:"non-member comment form" ~session:outsider
          ~slug:"rpta-pub" ~ty:"comment" ~id:fx.pub_comment ()
      in
      let* () =
        submit_ok ~url ~label:"non-member post" ~session:outsider
          ~slug:"rpta-pub" ~ty:"post" ~id:fx.pub_post ()
      in
      let* () =
        submit_ok ~url ~label:"non-member comment" ~session:outsider
          ~slug:"rpta-pub" ~ty:"comment" ~id:fx.pub_comment ()
      in
      let* _ =
        open_form_ok ~url ~label:"member form" ~session:member
          ~slug:"rpta-pub" ~ty:"post" ~id:fx.pub_post ()
      in
      let* () =
        submit_ok ~url ~label:"member post" ~session:member ~slug:"rpta-pub"
          ~ty:"post" ~id:fx.pub_post ()
      in
      (* Duplicate contract unchanged: a second identical open report by the
         same reporter inserts nothing and says so. *)
      let* status, _, body =
        run ~url ~session:member ~form:(post_form "post" fx.pub_post)
          "/c/rpta-pub/reports"
      in
      Alcotest.(check int) "duplicate status" 200 status;
      Alcotest.(check bool) "duplicate copy" true
        (has_sub body "Already reported");
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* n = C.find q_count_reports () in
      let* n = or_fail "count" n in
      Alcotest.(check int) "three rows (dup inserted nothing)" 3 n;
      Lwt.return_unit)

(* 4+5+6: authorized private viewers (member, moderator-without-membership,
   admin) keep full access. *)
let private_authorized_case =
  db_case "private community: member, moderator and admin open and submit"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let member = session_of fx.member "rpta_member" in
      let moderator = session_of fx.moderator "rpta_moderator" in
      let admin = session_of ~admin:true fx.admin "rpta_admin" in
      let* body =
        open_form_ok ~url ~label:"member post form" ~session:member
          ~slug:"rpta-priv" ~ty:"post" ~id:fx.priv_post ()
      in
      Alcotest.(check bool) "member sees the excerpt" true
        (has_sub body secret_post_title);
      let* _ =
        open_form_ok ~url ~label:"member comment form" ~session:member
          ~slug:"rpta-priv" ~ty:"comment" ~id:fx.priv_comment ()
      in
      let* () =
        submit_ok ~url ~label:"member post" ~session:member
          ~slug:"rpta-priv" ~ty:"post" ~id:fx.priv_post ()
      in
      let* () =
        submit_ok ~url ~label:"member comment" ~session:member
          ~slug:"rpta-priv" ~ty:"comment" ~id:fx.priv_comment ()
      in
      let* _ =
        open_form_ok ~url ~label:"moderator form (no member row)"
          ~session:moderator ~slug:"rpta-priv" ~ty:"post" ~id:fx.priv_post ()
      in
      let* () =
        submit_ok ~url ~label:"moderator post" ~session:moderator
          ~slug:"rpta-priv" ~ty:"post" ~id:fx.priv_post ()
      in
      let* _ =
        open_form_ok ~url ~label:"admin form" ~session:admin
          ~slug:"rpta-priv" ~ty:"post" ~id:fx.priv_post ()
      in
      let* () =
        submit_ok ~url ~label:"admin post" ~session:admin ~slug:"rpta-priv"
          ~ty:"post" ~id:fx.priv_post ()
      in
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* n = C.find q_count_reports () in
      let* n = or_fail "count" n in
      Alcotest.(check int) "four rows" 4 n;
      Lwt.return_unit)

(* 7+8+11+12: outsider GETs — valid post, valid comment, nonexistent id and
   missing community are ALL the same canonical 404, byte for byte. *)
let outsider_get_case =
  db_case
    "outsider GET: private targets, bogus ids and missing communities are \
     one indistinguishable 404"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "rpta_outsider" in
      let get t = run ~url ~session:outsider t in
      let* s1, _, valid_post = get (get_target "rpta-priv" "post" fx.priv_post) in
      let* s2, _, valid_comment =
        get (get_target "rpta-priv" "comment" fx.priv_comment)
      in
      let* s3, _, bogus_id = get (get_target "rpta-priv" "post" 999999999) in
      let* s4, _, missing_community =
        get (get_target "rpta-missing" "post" fx.priv_post)
      in
      let* s5, _, wrong_type = get (get_target "rpta-priv" "comment" fx.priv_post) in
      Alcotest.(check (list int)) "all 404"
        [ 404; 404; 404; 404; 404 ] [ s1; s2; s3; s4; s5 ];
      Alcotest.(check string) "valid post = valid comment" valid_post valid_comment;
      Alcotest.(check string) "valid = nonexistent id" valid_post bogus_id;
      Alcotest.(check string) "existing private = missing community"
        valid_post missing_community;
      Alcotest.(check string) "valid = wrong-typed id" valid_post wrong_type;
      check_no_leak "outsider GET" valid_post;
      Lwt.return_unit)

(* 9+10+11+14+15: outsider POSTs — same canonical 404 for every private
   variant, with a fresh valid CSRF token, and ZERO side effects. *)
let outsider_post_case =
  db_case
    "outsider POST: valid CSRF still denies, indistinguishably, with no \
     report/modlog/notification row"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "rpta_outsider" in
      let post ~slug form = run ~url ~session:outsider ~form ("/c/" ^ slug ^ "/reports") in
      let* s1, _, valid_post = post ~slug:"rpta-priv" (post_form "post" fx.priv_post) in
      let* s2, _, valid_comment =
        post ~slug:"rpta-priv" (post_form "comment" fx.priv_comment)
      in
      let* s3, _, bogus_id = post ~slug:"rpta-priv" (post_form "post" 999999999) in
      let* s4, _, missing_community =
        post ~slug:"rpta-missing" (post_form "post" fx.priv_post)
      in
      Alcotest.(check (list int)) "all 404" [ 404; 404; 404; 404 ]
        [ s1; s2; s3; s4 ];
      Alcotest.(check string) "valid post = valid comment" valid_post valid_comment;
      Alcotest.(check string) "valid = nonexistent id" valid_post bogus_id;
      Alcotest.(check string) "existing private = missing community"
        valid_post missing_community;
      check_no_leak "outsider POST" valid_post;
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* n = C.find q_count_reports () in
      let* n = or_fail "reports" n in
      Alcotest.(check int) "no report row" 0 n;
      let* n = C.find q_count_mod_actions () in
      let* n = or_fail "mod_actions" n in
      Alcotest.(check int) "no modlog row" 0 n;
      let* n = C.find q_count_notifications () in
      let* n = or_fail "notifications" n in
      Alcotest.(check int) "no notification" 0 n;
      Lwt.return_unit)

(* 13: cross-community tampering under a PUBLIC slug must not reveal that
   the private target id exists — identical to a nonexistent id. *)
let cross_community_case =
  db_case
    "cross-community tampering: private id under a public slug behaves \
     exactly like a nonexistent id"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "rpta_outsider" in
      let* s1, _, tampered_get =
        run ~url ~session:outsider (get_target "rpta-pub" "post" fx.priv_post)
      in
      let* s2, _, bogus_get =
        run ~url ~session:outsider (get_target "rpta-pub" "post" 999999999)
      in
      Alcotest.(check (list int)) "GET statuses" [ 400; 400 ] [ s1; s2 ];
      Alcotest.(check string) "tampered GET = bogus GET" bogus_get tampered_get;
      check_no_leak "tampered GET" tampered_get;
      let* s3, _, tampered_post =
        run ~url ~session:outsider ~form:(post_form "post" fx.priv_post)
          "/c/rpta-pub/reports"
      in
      let* s4, _, bogus_post =
        run ~url ~session:outsider ~form:(post_form "post" 999999999)
          "/c/rpta-pub/reports"
      in
      Alcotest.(check (list int)) "POST statuses" [ 404; 404 ] [ s3; s4 ];
      Alcotest.(check string) "tampered POST = bogus POST" bogus_post tampered_post;
      check_no_leak "tampered POST" tampered_post;
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* n = C.find q_count_reports () in
      let* n = or_fail "count" n in
      Alcotest.(check int) "no report row" 0 n;
      Lwt.return_unit)

(* 16: global-ban, community-ban and self-report restrictions unchanged for
   viewers the community gate admits; an unauthorized outsider's global ban
   no longer confirms private-community existence. *)
let restrictions_case =
  db_case "ban and self-report gates unchanged; ban page never leaks privates"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let (module C : Caqti_lwt.CONNECTION) = c in
      (* Globally banned user on a PUBLIC community: same Account Banned page. *)
      let* r = C.exec q_set_globally_banned fx.outsider in
      let* () = or_fail "gban" r in
      let outsider = session_of fx.outsider "rpta_outsider" in
      let* status, _, body =
        run ~url ~session:outsider (get_target "rpta-pub" "post" fx.pub_post)
      in
      Alcotest.(check int) "gban GET public 403" 403 status;
      Alcotest.(check bool) "gban copy" true (has_sub body "Account Banned");
      let* status, _, _ =
        run ~url ~session:outsider ~form:(post_form "post" fx.pub_post)
          "/c/rpta-pub/reports"
      in
      Alcotest.(check int) "gban POST public 403" 403 status;
      (* The same banned OUTSIDER against the PRIVATE community gets the
         canonical 404 — the ban page must not confirm the community exists. *)
      let* status, _, body =
        run ~url ~session:outsider (get_target "rpta-priv" "post" fx.priv_post)
      in
      Alcotest.(check int) "gban outsider private 404" 404 status;
      check_no_leak "gban outsider" body;
      (* A globally banned MEMBER of the private community still sees the
         ban page: the view gate admits them, then the ban gate fires. *)
      let* r = C.exec q_set_globally_banned fx.member in
      let* () = or_fail "gban member" r in
      let member = session_of fx.member "rpta_member" in
      let* status, _, body =
        run ~url ~session:member (get_target "rpta-priv" "post" fx.priv_post)
      in
      Alcotest.(check int) "gban member private 403" 403 status;
      Alcotest.(check bool) "gban member copy" true
        (has_sub body "Account Banned");
      (* Community ban on the public community. *)
      let* r = C.exec q_add_community_ban (fx.priv_author, fx.pub) in
      let* () = or_fail "cban" r in
      let cbanned = session_of fx.priv_author "rpta_privauthor" in
      let* status, _, body =
        run ~url ~session:cbanned (get_target "rpta-pub" "post" fx.pub_post)
      in
      Alcotest.(check int) "cban GET 403" 403 status;
      Alcotest.(check bool) "cban copy" true
        (has_sub body "Banned from Community");
      let* status, _, _ =
        run ~url ~session:cbanned ~form:(post_form "post" fx.pub_post)
          "/c/rpta-pub/reports"
      in
      Alcotest.(check int) "cban POST 403" 403 status;
      (* Self-report still refused. *)
      let author = session_of fx.pub_author "rpta_pubauthor" in
      let* status, _, body =
        run ~url ~session:author (get_target "rpta-pub" "post" fx.pub_post)
      in
      Alcotest.(check int) "self GET 403" 403 status;
      Alcotest.(check bool) "self copy" true (has_sub body "Cannot Report");
      let* status, _, _ =
        run ~url ~session:author ~form:(post_form "post" fx.pub_post)
          "/c/rpta-pub/reports"
      in
      Alcotest.(check int) "self POST 403" 403 status;
      let* n = C.find q_count_reports () in
      let* n = or_fail "count" n in
      Alcotest.(check int) "nothing inserted" 0 n;
      Lwt.return_unit)

(* 17: the authorized Cartographic form document is stable — two renders
   differ only in the dream.csrf value — and the pinned form contract
   (action, hidden fields, reasons, details cap) is intact. *)
let form_contract_case =
  db_case "authorized form: byte-stable after CSRF masking, contract pinned"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let member = session_of fx.member "rpta_member" in
      let target = get_target "rpta-priv" "post" fx.priv_post in
      let* s1, _, first = run ~url ~session:member target in
      let* s2, _, second = run ~url ~session:member target in
      Alcotest.(check (list int)) "both 200" [ 200; 200 ] [ s1; s2 ];
      Alcotest.(check bool) "csrf token present" true
        (has_sub first "dream.csrf");
      Alcotest.(check string) "byte-identical after csrf masking"
        (mask_csrf first) (mask_csrf second);
      List.iter
        (fun needle ->
          Alcotest.(check bool) ("contract: " ^ needle) true
            (has_sub first needle))
        [ "<form action='/c/rpta-priv/reports' method='POST'"
        ; "<input type='hidden' name='target_type' value='post'>"
        ; Printf.sprintf "<input type='hidden' name='target_id' value='%d'>"
            fx.priv_post
        ; "<option value='spam'>"
        ; "<option value='abuse'>"
        ; "<option value='off_topic'>"
        ; "<option value='illegal'>"
        ; "<option value='other'>"
        ; "maxlength='1000'"
        ];
      Lwt.return_unit)

let suite =
  [ anonymous_case; public_reporting_case; private_authorized_case;
    outsider_get_case; outsider_post_case; cross_community_case;
    restrictions_case; form_contract_case
  ]

let suites =
    (* Report-flow private-community authorization: GET /c/:slug/report and
       POST /c/:slug/reports run can_view_community before any ban check or
       target resolution; an outsider gets the canonical community_not_found
       404, byte-identical across valid/bogus/missing targets and missing
       communities, with zero side effects — while public reporting,
       authorized private reporting, ban gates, self-report and the pinned
       Cartographic form contract are unchanged. Database-gated. *)
  [ ("report_private_community_authorization", suite)
  ]
