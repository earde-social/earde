module GUI = Earde.Github_user_installations

(* === Project-onboarding draft read model
   (Project_onboarding_draft_read_model) ===
   Owner-authorized listing/loading with the anti-oracle collapse and the
   durable-data validation live in SQL plus row checks over real Postgres
   rows, so only a DB-backed suite can pin them down. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope. Reuses Pod_store's
   real-client fixture chain (token exchange → verify → list_public →
   refresh_verified) — no test-only constructor exists — with its own
   reserved external-installation-id range 939000001..939000999 and
   podread_% usernames so the two suites never share fixtures. The
   invalid-input cases also live here rather than DB-free: the functions
   take a real connection module (which cannot be faked without a fake
   database layer), even though they must return before any SQL.
   Credential assertions are boolean, so no token bytes reach test output
   on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

module Read = Earde.Project_onboarding_draft_read_model

module Store = Earde.Project_onboarding_draft_store

let error_str : Read.error -> string = function
  | Read.Invalid_user_id -> "Invalid_user_id"
  | Read.Invalid_draft_id -> "Invalid_draft_id"
  | Read.Inconsistent_data -> "Inconsistent_data"
  | Read.Storage_error -> "Storage_error"

let account_type_str : GUI.account_type -> string = function
  | GUI.User -> "user"
  | GUI.Organization -> "organization"

let or_fail = Db_fixture.or_fail

let insert_user = Db_fixture.insert_user

(* Fixtures — reserved external-installation-id range
   939000001..939000999 and podread_% usernames so cleanup is targeted
   and idempotent. Drafts go first (installations are RESTRICT-protected
   while referenced); snapshots cascade from drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM project_onboarding_drafts \
       WHERE github_installation_record_id IN \
         (SELECT id FROM github_installations \
          WHERE github_installation_id BETWEEN 939000001 AND 939000999)"
    ; "DELETE FROM users WHERE username LIKE 'podread_%'"
    ; "DELETE FROM github_installations \
       WHERE github_installation_id BETWEEN 939000001 AND 939000999"
    ]

(* One statement, one NOW(): both rows get the database-identical
   updated_at, forcing the id DESC tiebreak to decide. *)
let q_equalize_updated =
  (Caqti_type.(t2 int64 int64) ->. Caqti_type.unit)
  "UPDATE project_onboarding_drafts SET updated_at = NOW()
   WHERE id IN ($1, $2)"

let q_select_up_to =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories
   SET is_selected = TRUE WHERE draft_id = $1 AND position <= $2"

let q_mark_primary =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories
   SET is_primary = TRUE WHERE draft_id = $1 AND position = $2"

let q_poison_description =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "UPDATE project_onboarding_draft_repositories
   SET description = 'bad' || CHR(1) || 'description'
   WHERE draft_id = $1 AND position = $2"

(* Oversize fixture: canonical filler rows at positions 2..$2, pushing a
   seeded one-repository draft past the documented 2,000 maximum. *)
let q_bulk_repos =
  (Caqti_type.(t2 int64 int) ->. Caqti_type.unit)
  "INSERT INTO project_onboarding_draft_repositories
     (draft_id, position, github_repository_id, github_owner_id,
      owner_login, name, full_name, html_url, description,
      default_branch, is_archived)
   SELECT $1, g, 939500000 + g, 939100071,
          'podread-owner', 'bulk-' || g, 'podread-owner/bulk-' || g,
          'https://github.com/podread-owner/bulk-' || g,
          NULL, 'main', FALSE
   FROM generate_series(2, $2) AS g"

(* Each case gets a fresh connection and a clean fixture slate; cleanup
   runs again afterwards even when an assertion fails mid-way. *)
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
               (fun () -> f conn)
               (fun () ->
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

(* Draft fixtures go through the real store against the real client
   chain, exactly as production writes them. *)
let refresh_draft ?(login = "podread-owner") ?(target = "User") conn ~user
    ~ext_id ~account_id repos =
  let* v =
    Project_fixture.verified ~installation_id:ext_id ~account_id ~login ~target ()
  in
  let* set = Project_fixture.repo_set ~installation:v repos in
  let* draft = Project_fixture.refresh_ok "fixture refresh" conn ~user v set in
  Lwt.return (Store.draft_id draft)

let make_draft ?login ?target ?installation_login ?installation_type
    ?connected_by conn ~user ~ext_id ~account_id repos =
  let* inst =
    Project_fixture.insert_installation
      ?login:installation_login ?account_type:installation_type
      ?connected_by conn ~ext_id ~account_id
  in
  let* draft =
    refresh_draft ?login ?target conn ~user ~ext_id ~account_id repos
  in
  Lwt.return (inst, draft)

(* === read-model call helpers === *)

let list_ok label conn ~user =
  let* r = Read.list_available conn ~user_id:user in
  match r with
  | Ok summaries -> Lwt.return summaries
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let list_expect label expected conn ~user =
  let* r = Read.list_available conn ~user_id:user in
  match r with
  | Ok _ ->
      Alcotest.failf "%s: expected %s, got Ok" label (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let load_view label conn ~user ~draft =
  let* r = Read.load_available conn ~user_id:user ~draft_id:draft in
  match r with
  | Ok (Some view) -> Lwt.return view
  | Ok None -> Alcotest.failf "%s: unexpectedly absent" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_none label conn ~user ~draft =
  let* r = Read.load_available conn ~user_id:user ~draft_id:draft in
  match r with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Alcotest.failf "%s: unexpectedly present" label
  | Error e -> Alcotest.failf "%s: %s" label (error_str e)

let load_expect label expected conn ~user ~draft =
  let* r = Read.load_available conn ~user_id:user ~draft_id:draft in
  match r with
  | Ok None ->
      Alcotest.failf "%s: expected %s, got Ok None" label
        (error_str expected)
  | Ok (Some _) ->
      Alcotest.failf "%s: expected %s, got Ok Some" label
        (error_str expected)
  | Error e ->
      Alcotest.(check string) label (error_str expected) (error_str e);
      Lwt.return_unit

let check_summary label ~draft ~login ~account_type ~count ~selected
    ~primary s =
  Alcotest.(check int64) (label ^ ": draft id") draft (Read.draft_id s);
  Alcotest.(check string) (label ^ ": account login") login
    (Read.account_login s);
  Alcotest.(check string) (label ^ ": account type") account_type
    (account_type_str (Read.account_type s));
  Alcotest.(check int) (label ^ ": repository count") count
    (Read.repository_count s);
  Alcotest.(check int) (label ^ ": selected count") selected
    (Read.selected_repository_count s);
  Alcotest.(check bool) (label ^ ": has primary") primary
    (Read.has_primary_repository s)

(* One text signature per returned repository (mirroring the expected_sig
   builder below): pins position, both GitHub ids, every metadata field,
   the derived full name and canonical URL, and all three flags in a
   single comparison. *)
let view_sig r =
  Printf.sprintf "%d|%Ld|%Ld|%s|%s|%s|%s|%s|%s|%b|%b|%b" (Read.position r)
    (Read.github_repository_id r)
    (Read.github_owner_id r) (Read.owner_login r) (Read.name r)
    (Read.full_name r) (Read.html_url r)
    (match Read.description r with None -> "<null>" | Some d -> d)
    (Read.default_branch r) (Read.is_archived r) (Read.is_selected r)
    (Read.is_primary r)

let expected_sig ~position ~id ~account_id ~login
    ?(description = "<null>") ?(branch = "main") ?(archived = false)
    ?(selected = false) ?(primary = false) name =
  Printf.sprintf
    "%d|%Ld|%Ld|%s|%s|%s/%s|https://github.com/%s/%s|%s|%s|%b|%b|%b"
    position id account_id login name login name login name description
    branch archived selected primary

(* === invalid input === *)

let invalid_input_case =
  db_case "read: non-positive ids rejected before SQL" (fun conn ->
      let* () = list_expect "list user 0" Read.Invalid_user_id conn ~user:0 in
      let* () =
        list_expect "list negative user" Read.Invalid_user_id conn
          ~user:(-4)
      in
      let* () =
        load_expect "load user 0" Read.Invalid_user_id conn ~user:0
          ~draft:1L
      in
      let* () =
        load_expect "load negative user" Read.Invalid_user_id conn
          ~user:(-1) ~draft:1L
      in
      let* () =
        load_expect "load draft 0" Read.Invalid_draft_id conn ~user:1
          ~draft:0L
      in
      load_expect "load negative draft" Read.Invalid_draft_id conn ~user:1
        ~draft:(-9L))

(* === empty list and plain absence === *)

let empty_case =
  db_case "read: no usable draft is Ok []; bare installation unlisted"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* before = list_ok "draftless user" conn ~user:uid in
      Alcotest.(check int) "no drafts listed" 0 (List.length before);
      (* An active installation with no draft must not surface. *)
      let* _ =
        Project_fixture.insert_installation conn ~ext_id:939000001L
          ~account_id:939100001L
      in
      let* after = list_ok "installation without draft" conn ~user:uid in
      Alcotest.(check int) "still no drafts listed" 0 (List.length after);
      let* absent = Db_fixture.find conn "absent draft id" Project_fixture.q_absent_draft_id () in
      load_none "nonexistent draft collapses to absent" conn ~user:uid
        ~draft:absent)

(* === owner isolation === *)

let owner_isolation_case =
  db_case "read: owners see only their own drafts; probes collapse"
    (fun conn ->
      let* a = insert_user conn "podread_a" in
      let* b = insert_user conn "podread_b" in
      (* Provenance deliberately points at B from the start. *)
      let* inst, da =
        make_draft ~connected_by:b conn ~user:a ~ext_id:939000011L
          ~account_id:939100011L
          [ Github_fixture.gur_repo ~owner_id:939100011L ~owner_login:"podread-owner"
              ~id:939600111L ~name:"alpha" () ]
      in
      let* db_draft =
        refresh_draft conn ~user:b ~ext_id:939000011L
          ~account_id:939100011L
          [ Github_fixture.gur_repo ~owner_id:939100011L ~owner_login:"podread-owner"
              ~id:939600112L ~name:"beta" () ]
      in
      let check_visibility label =
        let* la = list_ok (label ^ ": list A") conn ~user:a in
        Alcotest.(check (list int64)) (label ^ ": A lists only A's draft")
          [ da ] (List.map Read.draft_id la);
        let* lb = list_ok (label ^ ": list B") conn ~user:b in
        Alcotest.(check (list int64)) (label ^ ": B lists only B's draft")
          [ db_draft ] (List.map Read.draft_id lb);
        let* () =
          load_none (label ^ ": A probing B's draft") conn ~user:a
            ~draft:db_draft
        in
        let* () =
          load_none (label ^ ": B probing A's draft") conn ~user:b
            ~draft:da
        in
        let* va = load_view (label ^ ": A loads A's") conn ~user:a ~draft:da in
        Alcotest.(check int64) (label ^ ": A's view identity") da
          (Read.draft_id (Read.summary va));
        let* vb =
          load_view (label ^ ": B loads B's") conn ~user:b ~draft:db_draft
        in
        Alcotest.(check int64) (label ^ ": B's view identity") db_draft
          (Read.draft_id (Read.summary vb));
        Lwt.return_unit
      in
      let* () = check_visibility "provenance B" in
      (* Rewriting provenance must change nothing: it is not ownership. *)
      let* () =
        Db_fixture.exec conn "provenance to A" Project_fixture.q_set_provenance (inst, Some a)
      in
      let* () = check_visibility "provenance A" in
      let* () =
        Db_fixture.exec conn "provenance to NULL" Project_fixture.q_set_provenance (inst, None)
      in
      check_visibility "provenance NULL")

(* === multiple drafts and ordering === *)

let multiple_drafts_case =
  db_case "read: multiple drafts, updated_at DESC then id DESC" (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _, d1 =
        make_draft ~installation_login:"podread-one" conn ~user:uid
          ~ext_id:939000021L ~account_id:939100021L
          [ Github_fixture.gur_repo ~owner_id:939100021L ~owner_login:"podread-owner"
              ~id:939600211L ~name:"alpha" ()
          ; Github_fixture.gur_repo ~owner_id:939100021L ~owner_login:"podread-owner"
              ~id:939600212L ~name:"beta" ()
          ]
      in
      let* _, d2 =
        make_draft ~installation_login:"podread-two"
          ~installation_type:"organization" ~target:"Organization" conn
          ~user:uid ~ext_id:939000022L ~account_id:939100022L
          [ Github_fixture.gur_repo ~owner_id:939100022L ~owner_login:"podread-org"
              ~id:939600221L ~name:"gamma" ()
          ]
      in
      (* d2 was created later, so it holds the higher id; push its
         activity into the past so updated_at DESC must override id
         order. *)
      Alcotest.(check bool) "fixture: d2 has the higher id" true
        (Int64.compare d2 d1 > 0);
      let* () = Db_fixture.exec conn "backdate d2" Project_fixture.q_backdate_updated (d2, 2) in
      let* by_updated = list_ok "updated_at ordering" conn ~user:uid in
      Alcotest.(check (list int64))
        "updated_at DESC dominates id order" [ d1; d2 ]
        (List.map Read.draft_id by_updated);
      (* Equal updated_at: the deterministic id DESC tiebreak decides. *)
      let* () = Db_fixture.exec conn "equalize" q_equalize_updated (d1, d2) in
      let* by_id = list_ok "id tiebreak" conn ~user:uid in
      Alcotest.(check (list int64)) "id DESC tiebreak" [ d2; d1 ]
        (List.map Read.draft_id by_id);
      (* No automatic selection: both drafts are returned as data; each
         summary carries its own installation's current login and type
         and exact counts. *)
      let summary_for label draft summaries =
        match
          List.find_opt
            (fun s -> Int64.equal (Read.draft_id s) draft)
            summaries
        with
        | Some s -> s
        | None -> Alcotest.failf "%s: draft missing from list" label
      in
      check_summary "first installation" ~draft:d1 ~login:"podread-one"
        ~account_type:"user" ~count:2 ~selected:0 ~primary:false
        (summary_for "first installation" d1 by_id);
      check_summary "second installation" ~draft:d2 ~login:"podread-two"
        ~account_type:"organization" ~count:1 ~selected:0 ~primary:false
        (summary_for "second installation" d2 by_id);
      Lwt.return_unit)

(* === availability filtering === *)

(* One hidden-state scaffold: the draft must be usable before the
   mutation, and invisible — list and owner load alike — after it. *)
let hidden_case name ~ext_id mutate =
  db_case name (fun conn ->
      let account_id = Int64.add ext_id 100000L in
      let* uid = insert_user conn "podread_a" in
      let* inst, draft =
        make_draft conn ~user:uid ~ext_id ~account_id
          [ Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"podread-owner"
              ~id:(Int64.add ext_id 600000L) ~name:"alpha" () ]
      in
      let* before = list_ok "pre-mutation list" conn ~user:uid in
      Alcotest.(check (list int64)) "usable before mutation" [ draft ]
        (List.map Read.draft_id before);
      let* _ = load_view "pre-mutation load" conn ~user:uid ~draft in
      let* () = mutate conn ~inst ~draft in
      let* after = list_ok "post-mutation list" conn ~user:uid in
      Alcotest.(check int) "hidden from list" 0 (List.length after);
      load_none "owner load collapses to absent" conn ~user:uid ~draft)

let hidden_expired_case =
  hidden_case "read: expired active draft is hidden" ~ext_id:939000031L
    (fun conn ~inst:_ ~draft ->
      Db_fixture.exec conn "expire" Project_fixture.q_backdate_draft draft)

let hidden_completed_case =
  hidden_case "read: completed draft is hidden" ~ext_id:939000032L
    (fun conn ~inst:_ ~draft ->
      Db_fixture.exec conn "complete" Project_fixture.q_complete_draft draft)

let hidden_cancelled_case =
  hidden_case "read: cancelled draft is hidden" ~ext_id:939000033L
    (fun conn ~inst:_ ~draft ->
      Db_fixture.exec conn "cancel" Project_fixture.q_cancel_draft draft)

let hidden_inaccessible_case =
  hidden_case "read: inaccessible installation hides the draft"
    ~ext_id:939000034L (fun conn ~inst ~draft:_ ->
      Db_fixture.exec conn "inaccessible" Project_fixture.q_set_installation_status
        (inst, "inaccessible", false))

let hidden_revoked_case =
  hidden_case "read: revoked installation status hides the draft"
    ~ext_id:939000035L (fun conn ~inst ~draft:_ ->
      Db_fixture.exec conn "revoked" Project_fixture.q_set_installation_status
        (inst, "revoked", false))

let hidden_revoked_at_case =
  hidden_case "read: non-NULL revoked_at hides the draft"
    ~ext_id:939000036L (fun conn ~inst ~draft:_ ->
      Db_fixture.exec conn "revoked with timestamp" Project_fixture.q_set_installation_status
        (inst, "revoked", true))

(* === complete detail === *)

let complete_detail_case =
  db_case "read: full detail — identity, order, ids, metadata, flags"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _, draft =
        make_draft ~installation_login:"podread-account" conn ~user:uid
          ~ext_id:939000041L ~account_id:939100041L
          [ Github_fixture.gur_repo ~owner_id:939100041L ~owner_login:"podread-owner"
              ~id:939600411L ~name:"alpha"
              ~description:{|"Descrizione — été 🚀"|} ()
          ; Github_fixture.gur_repo ~owner_id:939100041L ~owner_login:"podread-owner"
              ~id:939600412L ~name:"beta" ~default_branch:"release/v1"
              ~archived:true ()
          ; Github_fixture.gur_repo ~owner_id:939100041L ~owner_login:"podread-owner"
              ~id:939600413L ~name:"gamma" ()
          ]
      in
      let* view = load_view "load" conn ~user:uid ~draft in
      check_summary "summary" ~draft ~login:"podread-account"
        ~account_type:"user" ~count:3 ~selected:0 ~primary:false
        (Read.summary view);
      let repos = Read.repositories view in
      Alcotest.(check (list string))
        "complete snapshot in position order, metadata byte-exact"
        [ expected_sig ~position:1 ~id:939600411L ~account_id:939100041L
            ~login:"podread-owner"
            ~description:"Descrizione — été 🚀" "alpha"
        ; expected_sig ~position:2 ~id:939600412L ~account_id:939100041L
            ~login:"podread-owner" ~branch:"release/v1" ~archived:true
            "beta"
        ; expected_sig ~position:3 ~id:939600413L ~account_id:939100041L
            ~login:"podread-owner" "gamma"
        ]
        (List.map view_sig repos);
      (* The exposed snapshot ids are the real local row ids, in the same
         order. *)
      let* stored_ids = Db_fixture.collect conn "snapshot ids" Project_fixture.q_snapshot_ids draft in
      Alcotest.(check (list int64)) "snapshot ids are the local row ids"
        stored_ids
        (List.map Read.snapshot_id repos);
      Lwt.return_unit)

(* === selection summary === *)

let selection_summary_case =
  db_case "read: selection and primary states summarize exactly"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _, draft =
        make_draft ~installation_login:"podread-account" conn ~user:uid
          ~ext_id:939000051L ~account_id:939100051L
          [ Github_fixture.gur_repo ~owner_id:939100051L ~owner_login:"podread-owner"
              ~id:939600511L ~name:"alpha" ()
          ; Github_fixture.gur_repo ~owner_id:939100051L ~owner_login:"podread-owner"
              ~id:939600512L ~name:"beta" ()
          ; Github_fixture.gur_repo ~owner_id:939100051L ~owner_login:"podread-owner"
              ~id:939600513L ~name:"gamma" ()
          ]
      in
      (* Both entry points must agree at every step. *)
      let check_counts label ~selected ~primary =
        let* summaries = list_ok (label ^ ": list") conn ~user:uid in
        let* view = load_view (label ^ ": load") conn ~user:uid ~draft in
        List.iter
          (check_summary label ~draft ~login:"podread-account"
             ~account_type:"user" ~count:3 ~selected ~primary)
          (Read.summary view :: summaries);
        Lwt.return_unit
      in
      let* () = check_counts "nothing selected" ~selected:0 ~primary:false in
      let* () = Db_fixture.exec conn "select two" q_select_up_to (draft, 2) in
      let* () = check_counts "two selected" ~selected:2 ~primary:false in
      let* () = Db_fixture.exec conn "mark primary" q_mark_primary (draft, 1) in
      let* () = check_counts "primary selected" ~selected:2 ~primary:true in
      let* view = load_view "final load" conn ~user:uid ~draft in
      Alcotest.(check (list (pair bool bool)))
        "per-repository selected/primary flags in position order"
        [ (true, true); (true, false); (false, false) ]
        (List.map
           (fun r -> (Read.is_selected r, Read.is_primary r))
           (Read.repositories view));
      Lwt.return_unit)

(* === snapshot refresh and stale ids === *)

let stale_snapshot_case =
  db_case "read: a refresh retires old snapshot ids and selection"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:939000061L
          ~account_id:939100061L
          [ Github_fixture.gur_repo ~owner_id:939100061L ~owner_login:"podread-owner"
              ~id:939600611L ~name:"alpha" ()
          ; Github_fixture.gur_repo ~owner_id:939100061L ~owner_login:"podread-owner"
              ~id:939600612L ~name:"beta" ()
          ]
      in
      let* () = Db_fixture.exec conn "select all" q_select_up_to (draft, 2) in
      let* view1 = load_view "first load" conn ~user:uid ~draft in
      let old_ids = List.map Read.snapshot_id (Read.repositories view1) in
      Alcotest.(check int) "two original snapshot rows" 2
        (List.length old_ids);
      (* The refresh keeps one repository id but changes the set — the
         stale browser-form scenario. *)
      let* refreshed =
        refresh_draft conn ~user:uid ~ext_id:939000061L
          ~account_id:939100061L
          [ Github_fixture.gur_repo ~owner_id:939100061L ~owner_login:"podread-owner"
              ~id:939600613L ~name:"gamma" ()
          ; Github_fixture.gur_repo ~owner_id:939100061L ~owner_login:"podread-owner"
              ~id:939600611L ~name:"alpha" ()
          ]
      in
      Alcotest.(check int64) "draft id unchanged" draft refreshed;
      let* view2 = load_view "reload" conn ~user:uid ~draft in
      let repos = Read.repositories view2 in
      Alcotest.(check (list string))
        "only the new snapshot, positions restarted at 1, selection reset"
        [ expected_sig ~position:1 ~id:939600613L ~account_id:939100061L
            ~login:"podread-owner" "gamma"
        ; expected_sig ~position:2 ~id:939600611L ~account_id:939100061L
            ~login:"podread-owner" "alpha"
        ]
        (List.map view_sig repos);
      let new_ids = List.map Read.snapshot_id repos in
      List.iter
        (fun old_id ->
          Alcotest.(check bool) "old snapshot id retired" false
            (List.exists (Int64.equal old_id) new_ids))
        old_ids;
      Lwt.return_unit)

(* === account-login source === *)

let login_source_case =
  db_case "read: summary login is the installation's, rows keep their own"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      (* Stored installation login and snapshot owner login deliberately
         differ; the stable account id is the same. *)
      let* inst, draft =
        make_draft ~installation_login:"installation-login"
          ~login:"snapshot-login" conn ~user:uid ~ext_id:939000071L
          ~account_id:939100071L
          [ Github_fixture.gur_repo ~owner_id:939100071L ~owner_login:"snapshot-login"
              ~id:939600711L ~name:"alpha" ()
          ]
      in
      let check_logins label ~account =
        let* summaries = list_ok (label ^ ": list") conn ~user:uid in
        let* view = load_view (label ^ ": load") conn ~user:uid ~draft in
        List.iter
          (fun s ->
            Alcotest.(check string)
              (label ^ ": summary login is the installation's") account
              (Read.account_login s))
          (Read.summary view :: summaries);
        Alcotest.(check (list string))
          (label ^ ": rows keep their snapshotted owner login")
          [ "snapshot-login" ]
          (List.map Read.owner_login (Read.repositories view));
        Lwt.return_unit
      in
      let* () = check_logins "as stored" ~account:"installation-login" in
      (* A GitHub rename updates the installation row; the read model
         must follow it without touching the snapshot. *)
      let* () =
        Db_fixture.exec conn "rename account" Project_fixture.q_set_installation_login
          (inst, "renamed-login")
      in
      check_logins "after rename" ~account:"renamed-login")

(* === zero-repository corruption === *)

let zero_repo_case =
  db_case "read: zero-snapshot draft is unlisted, owner-only error"
    (fun conn ->
      let* a = insert_user conn "podread_a" in
      let* b = insert_user conn "podread_b" in
      let* _, healthy =
        make_draft conn ~user:a ~ext_id:939000081L ~account_id:939100081L
          [ Github_fixture.gur_repo ~owner_id:939100081L ~owner_login:"podread-owner"
              ~id:939600811L ~name:"alpha" ()
          ]
      in
      let* inst2 =
        Project_fixture.insert_installation conn ~ext_id:939000082L
          ~account_id:939100082L
      in
      let* bare = Db_fixture.find conn "bare draft" Project_fixture.q_insert_bare_draft (a, inst2) in
      let* summaries = list_ok "list" conn ~user:a in
      Alcotest.(check (list int64))
        "corrupted draft omitted, healthy draft intact" [ healthy ]
        (List.map Read.draft_id summaries);
      let* () =
        load_expect "owner sees the corruption" Read.Inconsistent_data
          conn ~user:a ~draft:bare
      in
      (* The corruption signal is owner-only: to anyone else the id
         stays indistinguishable from a nonexistent draft. *)
      load_none "other user still gets plain absence" conn ~user:b
        ~draft:bare)

(* === inconsistent durable data ===
   Only invariants the schema deliberately leaves to the domain are
   corruptible here. The remaining required states are prevented
   conclusively by immediate constraints, so they cannot be fixtured
   without weakening production DDL and are documented as covered by
   the schema suite instead: primary-not-selected (CHECK is_selected OR
   NOT is_primary), duplicate snapshot ids (primary key), duplicate
   positions / repository ids / full names per draft (UNIQUE), and
   off-enum account types (CHECK on github_installations). *)

let corruption_case name ~ext_id corrupt =
  db_case name (fun conn ->
      let account_id = Int64.add ext_id 100000L in
      let base = Int64.add ext_id 600000L in
      let* uid = insert_user conn "podread_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id ~account_id
          [ Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"podread-owner"
              ~id:base ~name:"alpha" ()
          ; Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"podread-owner"
              ~id:(Int64.add base 1L) ~name:"beta" ()
          ; Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"podread-owner"
              ~id:(Int64.add base 2L) ~name:"gamma" ()
          ]
      in
      let* _ = load_view "valid before corruption" conn ~user:uid ~draft in
      let* () = corrupt conn ~draft in
      load_expect "corrupted detail rejected" Read.Inconsistent_data conn
        ~user:uid ~draft)

let corrupt_positions_case =
  corruption_case "read: non-contiguous positions are inconsistent"
    ~ext_id:939000091L (fun conn ~draft ->
      Db_fixture.exec conn "gap positions" Project_fixture.q_delete_position (draft, 2))

let corrupt_full_name_case =
  corruption_case "read: full name diverging from its parts is rejected"
    ~ext_id:939000092L (fun conn ~draft ->
      Db_fixture.exec conn "malform full name" Project_fixture.q_set_full_name
        (draft, 1, "podread-owner/other"))

let corrupt_html_url_case =
  corruption_case "read: non-canonical html url is rejected"
    ~ext_id:939000093L (fun conn ~draft ->
      Db_fixture.exec conn "malform html url" Project_fixture.q_set_html_url
        (draft, 1, "https://evil.example/podread-owner/alpha"))

let corrupt_description_case =
  corruption_case "read: control bytes in a description are rejected"
    ~ext_id:939000094L (fun conn ~draft ->
      Db_fixture.exec conn "poison description" q_poison_description (draft, 1))

let oversize_snapshot_case =
  db_case "read: a snapshot beyond 2000 rows is inconsistent everywhere"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _, draft =
        make_draft conn ~user:uid ~ext_id:939000095L
          ~account_id:939100095L
          [ Github_fixture.gur_repo ~owner_id:939100095L ~owner_login:"podread-owner"
              ~id:939600951L ~name:"seed" ()
          ]
      in
      (* 2,000 filler rows on top of the seed: 2,001 total. *)
      let* () = Db_fixture.exec conn "bulk rows" q_bulk_repos (draft, 2001) in
      let* () =
        list_expect "oversize poisons the list" Read.Inconsistent_data
          conn ~user:uid
      in
      load_expect "oversize poisons the detail" Read.Inconsistent_data
        conn ~user:uid ~draft)

(* === credential prohibition === *)

let privacy_case =
  db_case "read: no credential material reaches any returned field"
    (fun conn ->
      let* uid = insert_user conn "podread_a" in
      let* _ =
        Project_fixture.insert_installation ~login:"podread-account" conn
          ~ext_id:939000099L ~account_id:939100099L
      in
      (* The whole fixture chain rides the refresh-token exchange, so an
         access token, refresh token, code, verifier, and secret all
         exist to leak — and must not. *)
      let* v =
        Project_fixture.verified ~token_body:Project_fixture.refresh_token_body
          ~installation_id:939000099L ~account_id:939100099L
          ~login:"podread-owner" ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~token_body:Project_fixture.refresh_token_body
          ~installation:v
          [ Github_fixture.gur_repo ~owner_id:939100099L ~owner_login:"podread-owner"
              ~id:939600991L ~name:"alpha"
              ~description:{|"benign description"|} ()
          ]
      in
      let* stored = Project_fixture.refresh_ok "refresh" conn ~user:uid v set in
      let draft = Store.draft_id stored in
      let* summaries = list_ok "list" conn ~user:uid in
      let* view = load_view "load" conn ~user:uid ~draft in
      (* Every text a caller can ever render from the read model. *)
      let rendered =
        String.concat "|"
          (List.concat
             [ List.map Read.account_login
                 (Read.summary view :: summaries)
             ; List.concat_map
                 (fun r ->
                   [ Read.owner_login r; Read.name r; Read.full_name r
                   ; Read.html_url r; Read.default_branch r
                   ; (match Read.description r with
                     | None -> ""
                     | Some d -> d)
                   ])
                 (Read.repositories view)
             ])
      in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool) (label ^ " absent from returned fields")
            false
            (Html_assert.contains_nonempty ~needle rendered))
        [ ("access token", Project_fixture.pods_access_fixture)
        ; ("refresh token", Project_fixture.pods_refresh_fixture)
        ; ("authorization code", Github_fixture.gte_code_string)
        ; ("PKCE verifier", Github_fixture.gte_verifier_string)
        ; ("client secret", Github_fixture.gte_client_secret)
        ];
      Lwt.return_unit)

let suite =
  [ invalid_input_case; empty_case; owner_isolation_case;
    multiple_drafts_case; hidden_expired_case; hidden_completed_case;
    hidden_cancelled_case; hidden_inaccessible_case; hidden_revoked_case;
    hidden_revoked_at_case; complete_detail_case; selection_summary_case;
    stale_snapshot_case; login_source_case; zero_repo_case;
    corrupt_positions_case; corrupt_full_name_case;
    corrupt_html_url_case; corrupt_description_case;
    oversize_snapshot_case; privacy_case ]

let suites =
    (* Draft read model: owner-authorized listing/loading, the
       anti-oracle collapse, and durable-data validation all read real
       Postgres rows; same EARDE_TEST_DATABASE_URL gate (each case skips
       without it). *)
  [ ( "project_onboarding_draft_read_model", suite )
  ]
