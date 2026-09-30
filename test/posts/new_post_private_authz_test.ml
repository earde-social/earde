(* ===== /new-post private-community authorization (pass 15B) =====
   Pins the two disclosure fixes in Post_handlers.new_post_page:
   1. GET /new-post?community=:slug runs can_view_community before rendering
      the creation form or join gate — a private-community outsider gets the
      canonical community_not_found 404, byte-identical to a missing slug,
      instead of learning the community's existence and name from the join
      gate and document title.
   2. The no-parameter chooser filters Community_store.get_all_communities through the
      same predicate — private communities are listed only for members,
      moderators, and admins.
   Exercises the REAL handler through the production-like Dream router,
   SQL pool and memory-session pipeline (same harness as the report-flow
   authorization suite). Product behavior deliberately pinned as-is: the
   creation form itself requires an ordinary membership row, so an
   authorized private moderator-without-membership or a non-member admin
   receives the (named, authorized) join gate, never the canonical 404. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM community_sections WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'nppa-%')"
    ; "DELETE FROM community_members WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'nppa-%')"
    ; "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM communities WHERE slug LIKE 'nppa-%')"
    ; "DELETE FROM communities WHERE slug LIKE 'nppa-%'"
    ; "DELETE FROM users WHERE username LIKE 'nppa_%'"
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
  (Caqti_type.(t4 string string bool string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, sections_enabled, visibility)
     VALUES ($1, $2, $3, $4) RETURNING id"

let q_insert_section =
  (Caqti_type.(t3 int string string) ->! Caqti_type.int)
    "INSERT INTO community_sections (community_id, name, slug)
     VALUES ($1, $2, $3) RETURNING id"

let q_add_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_add_moderator =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES ($1, $2, 'mod')"

let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
    "UPDATE users SET is_admin = TRUE WHERE id = $1"

(* Distinctive markers that must NEVER appear in a denied or filtered
   response: the private community's display name is deliberately distinct
   from its slug so both leak channels are asserted independently. *)
let priv_slug = "nppa-priv"

let priv_name = "NPPA Private Secret"

type fx = {
  outsider : int;
  member : int;        (* member of nppa-priv and nppa-struct *)
  pmember : int;       (* member of nppa-pub only *)
  smember : int;       (* member of nppa-struct ONLY — no private rows *)
  moderator : int;     (* moderator row ONLY on nppa-priv, no membership *)
  admin : int;         (* no community rows; durable users.is_admin + claim *)
  pub : int;
  struct_ : int;
  priv : int;
  sec_a : int;
  sec_b : int;
  priv_sec : int;
}

let make_fixtures (module C : Caqti_lwt.CONNECTION) =
  let user name =
    let* id = C.find q_insert_user name in
    or_fail name id
  in
  let* outsider = user "nppa_outsider" in
  let* member = user "nppa_member" in
  let* pmember = user "nppa_pmember" in
  let* smember = user "nppa_smember" in
  let* moderator = user "nppa_moderator" in
  let* admin = user "nppa_admin" in
  (* The admin override is decided on the DURABLE users.is_admin row, which
     the session claim only enables the lookup for — so the admin fixture
     carries the real flag as well as the claim. *)
  let* r = C.exec q_make_admin admin in
  let* () = or_fail "durable admin" r in
  (* Insertion order pins the chooser's pass-through ordering below. *)
  let* pub = C.find q_insert_community ("nppa-pub", "NPPA Public Flat", false, "public") in
  let* pub = or_fail "pub" pub in
  let* struct_ = C.find q_insert_community ("nppa-struct", "NPPA Public Structured", true, "public") in
  let* struct_ = or_fail "struct" struct_ in
  let* priv = C.find q_insert_community (priv_slug, priv_name, true, "private") in
  let* priv = or_fail "priv" priv in
  let* sec_a = C.find q_insert_section (struct_, "NPPA Section A", "nppa-sec-a") in
  let* sec_a = or_fail "sec a" sec_a in
  let* sec_b = C.find q_insert_section (struct_, "NPPA Section B", "nppa-sec-b") in
  let* sec_b = or_fail "sec b" sec_b in
  let* priv_sec = C.find q_insert_section (priv, "NPPA Hidden Section", "nppa-hidden-sec") in
  let* priv_sec = or_fail "priv sec" priv_sec in
  let* r = C.exec q_add_member (pmember, pub) in
  let* () = or_fail "pmember row" r in
  let* r = C.exec q_add_member (member, struct_) in
  let* () = or_fail "member struct row" r in
  let* r = C.exec q_add_member (member, priv) in
  let* () = or_fail "member priv row" r in
  let* r = C.exec q_add_member (smember, struct_) in
  let* () = or_fail "smember struct row" r in
  let* r = C.exec q_add_moderator (moderator, priv) in
  let* () = or_fail "moderator row" r in
  Lwt.return
    { outsider; member; pmember; smember; moderator; admin;
      pub; struct_; priv; sec_a; sec_b; priv_sec }

let router = Dream.router [ Dream.get "/new-post" Earde.Post_handlers.new_post_page ]

(* One sql_pool for the whole suite. Nothing ever closes a Dream.sql_pool,
   and its pool lives inside the middleware closure, so reusing that one
   value is what shares the pool — a fresh [Dream.sql_pool url] per request
   leaks a connection each time, and the whole gated run sits close enough
   to Postgres max_connections that those leaks decide whether it passes.
   Each pipeline still gets its own session store, so nothing else is
   shared between requests. *)
let shared_sql_pool : Dream.middleware option ref = ref None

let sql_pool url =
  match !shared_sql_pool with
  | Some middleware -> middleware
  | None ->
      let middleware = Dream.sql_pool ~size:2 url in
      shared_sql_pool := Some middleware;
      middleware

(* One GET against the real pipeline: sql_pool + memory_sessions + router,
   with the session fields written under the same session the handler
   reads. *)
let run ~url ?(session = []) target =
  let pipeline =
    sql_pool url @@ Dream.memory_sessions @@ fun req ->
    let* () =
      Lwt_list.iter_s
        (fun (k, v) -> Dream.set_session_field req k v)
        session
    in
    router req
  in
  let request = Dream.request ~method_:`GET ~target "" in
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

(* Blank the one volatile value in an authorized document: the dream.csrf
   hidden input's value (each of the three authorized states renders
   exactly one). Everything else must be byte-stable. *)
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

let is_redirect s = s = 301 || s = 302 || s = 303 || s = 307 || s = 308

(* Every marker a denied response must not carry: the private identifiers
   themselves, and any launch-shell / creation-surface rendering (the
   denial is the shared legacy 404 — no rail, no sidebar, no create
   fragment, no launch scope class, no private analytics marker). *)
let check_denied_clean label body =
  Alcotest.(check bool) (label ^ ": no private name") false
    (has_sub body priv_name);
  Alcotest.(check bool) (label ^ ": no private slug") false
    (has_sub body priv_slug);
  Alcotest.(check bool) (label ^ ": no create-shell") false
    (has_sub body "create-shell");
  Alcotest.(check bool) (label ^ ": no join gate") false
    (has_sub body "Members only");
  Alcotest.(check bool) (label ^ ": no launch rail") false
    (has_sub body "rail__item");
  Alcotest.(check bool) (label ^ ": no launch scope class") false
    (has_sub body "launch-post-creation");
  Alcotest.(check bool) (label ^ ": no private analytics marker") false
    (has_sub body "data-analytics-private-community")

let creation_form_marker =
  "<form action='/posts' method='POST' enctype='multipart/form-data' class='create-form'>"

let join_form_marker = "<form action='/join' method='POST' class='create-form'>"

let community_id_hidden id =
  Printf.sprintf "<input type='hidden' name='community_id' value='%d'>" id

let chooser_row slug =
  Printf.sprintf "<a href='/new-post?community=%s' class='create-comm'>" slug

(* 1: anonymous requests keep the existing login redirect, and render no
   creation surface, rail data or notification wiring at all. *)
let anonymous_case =
  db_case "anonymous /new-post keeps the login redirect, renders nothing"
    (fun ~url c ->
      let* _fx = make_fixtures c in
      let* s1, l1, b1 = run ~url "/new-post" in
      Alcotest.(check bool) "chooser redirects" true (is_redirect s1);
      Alcotest.(check (option string)) "chooser location" (Some "/login") l1;
      let* s2, l2, b2 = run ~url ("/new-post?community=" ^ priv_slug) in
      Alcotest.(check bool) "private target redirects" true (is_redirect s2);
      Alcotest.(check (option string)) "private target location" (Some "/login") l2;
      List.iter
        (fun b ->
          check_denied_clean "anonymous" b;
          Alcotest.(check bool) "no notification fetch" false
            (has_sub b "unread-notifs"))
        [ b1; b2 ];
      Lwt.return_unit)

(* 2: public community, authenticated non-member — the Members-only join
   gate survives, with the community named and the real join round-trip. *)
let public_join_gate_case =
  db_case "public community non-member still gets the named join gate"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "nppa_outsider" in
      let* status, _, body = run ~url ~session:outsider "/new-post?community=nppa-pub" in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check bool) "gate copy" true (has_sub body "Members only");
      Alcotest.(check bool) "community named" true
        (has_sub body "<span class='create-gate-name'>/c/nppa-pub</span>");
      Alcotest.(check bool) "join form" true (has_sub body join_form_marker);
      Alcotest.(check bool) "join target id" true
        (has_sub body (community_id_hidden fx.pub));
      Alcotest.(check bool) "join redirect destination" true
        (has_sub body
           "<input type='hidden' name='redirect_to' value='/new-post?community=nppa-pub'>");
      Alcotest.(check bool) "create-shell marker" true (has_sub body "create-shell");
      Lwt.return_unit)

(* 3: public flat member — the real creation form, and no section field on
   a flat community. *)
let public_member_form_case =
  db_case "public flat member gets the creation form without a section field"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let pmember = session_of fx.pmember "nppa_pmember" in
      let* status, _, body = run ~url ~session:pmember "/new-post?community=nppa-pub" in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check bool) "creation form" true (has_sub body creation_form_marker);
      Alcotest.(check bool) "community bound" true
        (has_sub body (community_id_hidden fx.pub));
      Alcotest.(check bool) "no section field on flat" false
        (has_sub body "name='section_id'");
      List.iter
        (fun marker ->
          Alcotest.(check bool) ("field: " ^ marker) true (has_sub body marker))
        [ "<input type='text' name='title' required class='create-input'"
        ; "<input type='url' name='url' class='create-input'"
        ; "<input type='file' name='image' accept='image/*' class='create-file'"
        ; "<textarea name='content' class='create-textarea'" ];
      Lwt.return_unit)

(* 4+5+6: authorized private viewers. The MEMBER gets the creation form;
   the moderator-without-membership and the non-member admin are authorized
   past the privacy gate and receive the (named) join gate — the form
   itself has always required an ordinary membership row. All three see
   the private community in the chooser. *)
let private_authorized_case =
  db_case "private community: member form; moderator and admin authorized; all three choosers list it"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let member = session_of fx.member "nppa_member" in
      let moderator = session_of fx.moderator "nppa_moderator" in
      let admin = session_of ~admin:true fx.admin "nppa_admin" in
      let target = "/new-post?community=" ^ priv_slug in
      let* status, _, body = run ~url ~session:member target in
      Alcotest.(check int) "member status" 200 status;
      Alcotest.(check bool) "member creation form" true
        (has_sub body creation_form_marker);
      Alcotest.(check bool) "member form bound to private id" true
        (has_sub body (community_id_hidden fx.priv));
      Alcotest.(check bool) "member sees private section option" true
        (has_sub body (Printf.sprintf "<option value='%d'" fx.priv_sec));
      let* status, _, body = run ~url ~session:moderator target in
      Alcotest.(check int) "moderator status (never 404)" 200 status;
      Alcotest.(check bool) "moderator sees the named gate" true
        (has_sub body ("<span class='create-gate-name'>/c/" ^ priv_slug ^ "</span>"));
      let* status, _, body = run ~url ~session:admin target in
      Alcotest.(check int) "admin status (never 404)" 200 status;
      Alcotest.(check bool) "admin sees the named gate" true
        (has_sub body ("<span class='create-gate-name'>/c/" ^ priv_slug ^ "</span>"));
      let* () =
        Lwt_list.iter_s
          (fun (label, session) ->
            let* status, _, chooser = run ~url ~session "/new-post" in
            Alcotest.(check int) (label ^ " chooser status") 200 status;
            Alcotest.(check bool) (label ^ " chooser lists private name") true
              (has_sub chooser priv_name);
            Alcotest.(check bool) (label ^ " chooser links private slug") true
              (has_sub chooser (chooser_row priv_slug));
            Lwt.return_unit)
          [ ("member", member); ("moderator", moderator); ("admin", admin) ]
      in
      Lwt.return_unit)

(* 7+9+10: the outsider's direct request — the canonical anti-enumeration
   404. An existing private slug, a missing slug and an empty slug are one
   indistinguishable response, byte for byte, with no private identifier
   and no creation/launch rendering. *)
let outsider_direct_case =
  db_case "outsider direct: private, missing and empty slugs are one indistinguishable 404"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "nppa_outsider" in
      let get t = run ~url ~session:outsider t in
      let* s1, _, denied = get ("/new-post?community=" ^ priv_slug) in
      let* s2, _, missing = get "/new-post?community=nppa-missing" in
      let* s3, _, empty = get "/new-post?community=" in
      Alcotest.(check (list int)) "all 404" [ 404; 404; 404 ] [ s1; s2; s3 ];
      Alcotest.(check bool) "denied == missing byte-identical" true
        (String.equal denied missing);
      Alcotest.(check bool) "denied == empty byte-identical" true
        (String.equal denied empty);
      Alcotest.(check bool) "generic copy" true
        (has_sub denied "This community does not exist.");
      check_denied_clean "outsider direct" denied;
      ignore fx.priv;
      Lwt.return_unit)

(* 8+13: the outsider's chooser — publics listed in pass-through order with
   the /bring fallback, the private community entirely absent: no name, no
   slug, no destination URL. *)
let outsider_chooser_case =
  db_case "outsider chooser: publics in order with /bring fallback, private fully absent"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let outsider = session_of fx.outsider "nppa_outsider" in
      let* status, _, body = run ~url ~session:outsider "/new-post" in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check bool) "public flat listed" true
        (has_sub body "NPPA Public Flat");
      Alcotest.(check bool) "public structured listed" true
        (has_sub body "NPPA Public Structured");
      Alcotest.(check bool) "public rows link the composer" true
        (has_sub body (chooser_row "nppa-pub"));
      Alcotest.(check bool) "unjoined public still offered" true
        (has_sub body (chooser_row "nppa-struct"));
      (* get_all_communities has no ORDER BY: the chooser passes rows
         through, so freshly-inserted fixtures keep insertion order. *)
      (match
         ( find_sub body (chooser_row "nppa-pub") 0,
           find_sub body (chooser_row "nppa-struct") 0 )
       with
      | Some i, Some j ->
          Alcotest.(check bool) "insertion order preserved" true (i < j)
      | _ -> Alcotest.fail "expected both public chooser rows");
      Alcotest.(check bool) "no private name" false (has_sub body priv_name);
      Alcotest.(check bool) "no private slug anywhere" false
        (has_sub body priv_slug);
      Alcotest.(check bool) "no private destination URL" false
        (has_sub body (chooser_row priv_slug));
      Alcotest.(check bool) "bring fallback" true
        (has_sub body "href='/bring' class='create-link'>Connect a project</a>");
      Alcotest.(check bool) "post-here affordance" true (has_sub body "Post here");
      ignore fx.outsider;
      Lwt.return_unit)

(* 13 (empty world): with no viewable community at all, the chooser renders
   no nppa row and keeps the /bring fallback. Scoped to this suite's
   fixtures so parallel suites' data cannot flake it. *)
let empty_chooser_case =
  db_case "chooser with no eligible nppa community keeps the /bring fallback"
    (fun ~url c ->
      let (module C : Caqti_lwt.CONNECTION) = c in
      let* viewer = C.find q_insert_user "nppa_lonely" in
      let* viewer = or_fail "viewer" viewer in
      let* priv = C.find q_insert_community (priv_slug, priv_name, false, "private") in
      let* _ = or_fail "priv" priv in
      let* status, _, body =
        run ~url ~session:(session_of viewer "nppa_lonely") "/new-post"
      in
      Alcotest.(check int) "status" 200 status;
      Alcotest.(check bool) "no nppa row" false (has_sub body "nppa-");
      Alcotest.(check bool) "no private name" false (has_sub body priv_name);
      Alcotest.(check bool) "bring fallback" true
        (has_sub body "href='/bring' class='create-link'>Connect a project</a>");
      Lwt.return_unit)

(* 11+12: authorized section preselection — the real slug preselects its
   option; a bogus slug and a PRIVATE community's section slug preselect
   nothing and reveal nothing. *)
let section_preselect_case =
  db_case "section preselection: own slug selects; bogus and foreign-private slugs select and reveal nothing"
    (fun ~url c ->
      let* fx = make_fixtures c in
      (* smember belongs to the structured community ONLY — so its joined
         rail carries no private tile, and any private name/slug in this
         document would be a real leak, not the viewer's own membership. *)
      let member = session_of fx.smember "nppa_smember" in
      let* status, _, body =
        run ~url ~session:member "/new-post?community=nppa-struct&section=nppa-sec-b"
      in
      Alcotest.(check int) "preselect status" 200 status;
      Alcotest.(check bool) "structured section field" true
        (has_sub body "<select name='section_id' required class='create-select'>");
      Alcotest.(check bool) "section B preselected" true
        (has_sub body (Printf.sprintf "<option value='%d' selected>" fx.sec_b));
      Alcotest.(check bool) "section A not preselected" true
        (has_sub body (Printf.sprintf "<option value='%d'>" fx.sec_a));
      let expect_no_preselect label target =
        let* status, _, body = run ~url ~session:member target in
        Alcotest.(check int) (label ^ " status") 200 status;
        Alcotest.(check bool) (label ^ " renders the form") true
          (has_sub body creation_form_marker);
        Alcotest.(check bool) (label ^ " preselects nothing") false
          (has_sub body "' selected>");
        Lwt.return body
      in
      let* _ =
        expect_no_preselect "bogus slug"
          "/new-post?community=nppa-struct&section=nppa-nope"
      in
      let* body =
        expect_no_preselect "private community's section slug"
          "/new-post?community=nppa-struct&section=nppa-hidden-sec"
      in
      Alcotest.(check bool) "no private name leaked" false
        (has_sub body priv_name);
      Alcotest.(check bool) "no private slug leaked" false
        (has_sub body priv_slug);
      Alcotest.(check bool) "no private section option" false
        (has_sub body (Printf.sprintf "<option value='%d'" fx.priv_sec));
      Lwt.return_unit)

(* 15: the three authorized create-shell states are byte-stable across
   independent requests once the single CSRF value is masked — the pinned
   form contract cannot drift request-to-request. *)
let form_stability_case =
  db_case "authorized flat form, structured form and join gate are byte-stable after CSRF masking"
    (fun ~url c ->
      let* fx = make_fixtures c in
      let stable label session target expected_markers =
        let* s1, _, one = run ~url ~session target in
        let* s2, _, two = run ~url ~session target in
        Alcotest.(check (list int)) (label ^ " statuses") [ 200; 200 ] [ s1; s2 ];
        Alcotest.(check bool) (label ^ " byte-stable") true
          (String.equal (mask_csrf one) (mask_csrf two));
        List.iter
          (fun m ->
            Alcotest.(check bool) (label ^ " pins: " ^ m) true (has_sub one m))
          expected_markers;
        Lwt.return_unit
      in
      let* () =
        stable "flat form"
          (session_of fx.pmember "nppa_pmember")
          "/new-post?community=nppa-pub"
          [ creation_form_marker; community_id_hidden fx.pub;
            "<div class='create-shell'>" ]
      in
      let* () =
        stable "structured form"
          (session_of fx.member "nppa_member")
          "/new-post?community=nppa-struct"
          [ creation_form_marker; community_id_hidden fx.struct_;
            "<select name='section_id' required class='create-select'>";
            "<div class='create-shell'>" ]
      in
      let* () =
        stable "join gate"
          (session_of fx.outsider "nppa_outsider")
          "/new-post?community=nppa-pub"
          [ join_form_marker; community_id_hidden fx.pub;
            "<div class='create-shell'>" ]
      in
      Lwt.return_unit)

let suite =
  [ anonymous_case; public_join_gate_case; public_member_form_case;
    private_authorized_case; outsider_direct_case; outsider_chooser_case;
    empty_chooser_case; section_preselect_case; form_stability_case ]

let suites =
    (* /new-post private-community authorization (pass 15B): the direct
       ?community= route runs can_view_community — an outsider gets the
       canonical 404, byte-identical across existing-private, missing and
       empty slugs, with no name/slug/create-shell/launch rendering — and
       the chooser filters get_all_communities through the same predicate
       (members, moderators and admins keep the private entry; outsiders
       never see name, slug or destination URL). Public join gate, member
       forms, section preselection and the byte-stable authorized form
       contracts are pinned unchanged. Database-gated. *)
  [ ("new_post_private_authorization", suite)
  ]
