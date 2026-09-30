(* === Project-onboarding draft selection store
   (Project_onboarding_draft_selection_store) ===
   Owner-authorized transactional selection replacement over local
   snapshot-row ids: authorization plus draft lock, snapshot validation,
   stale detection, and complete replacement all live in one Postgres
   transaction, so only a DB-backed suite can pin them down. Same
   EARDE_TEST_DATABASE_URL opt-in gate as Mod_scope. Reuses Pod_store's
   real-client fixture chain (token exchange → verify → list_public →
   refresh_verified) — no test-only constructor exists — with its own
   reserved external-installation-id range 940000001..940000999 and
   podsel_% usernames so the suites never share fixtures. Pure-invalid
   inputs follow the established rejected-before-SQL convention, tightened
   into an ordering proof: they run against ids that WOULD change the
   outcome had any SQL executed (an absent draft id must still yield the
   validation error, never Draft_unavailable), and existing selection
   state is proven untouched afterwards. Credential assertions are
   boolean, so no token bytes reach test output on failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module Sel = Earde.Project_onboarding_draft_selection_store
module Store = Earde.Project_onboarding_draft_store

let or_fail = Db_fixture.or_fail
let insert_user = Db_fixture.insert_user
let exec = Db_fixture.exec
let find = Db_fixture.find
let collect = Db_fixture.collect

(* Fixtures — reserved external-installation-id range
   940000001..940000999 and podsel_% usernames so cleanup is targeted
   and idempotent. Drafts go first (installations are RESTRICT-protected
   while referenced); snapshots cascade from drafts. *)
let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM project_onboarding_drafts WHERE \
       github_installation_record_id IN (SELECT id FROM github_installations \
       WHERE github_installation_id BETWEEN 940000001 AND 940000999)";
      "DELETE FROM users WHERE username LIKE 'podsel_%'";
      "DELETE FROM github_installations WHERE github_installation_id BETWEEN \
       940000001 AND 940000999";
    ]

(* Identity and metadata only, no flags: a replacement must leave this
   projection byte-identical, so it is captured before and compared
   after rather than reconstructed. *)
let q_meta_sigs =
  (Caqti_type.int64 ->* Caqti_type.string)
    "SELECT position::text || '|' || github_repository_id::text || '|' ||\n\
    \          github_owner_id::text || '|' || owner_login || '|' || name\n\
    \          || '|' || full_name || '|' || html_url || '|' ||\n\
    \          COALESCE(description, '<null>') || '|' || default_branch\n\
    \          || '|' || is_archived::text\n\
    \   FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

let q_repo_epochs =
  (Caqti_type.int64 ->* Caqti_type.(t3 int64 float float))
    "SELECT id, EXTRACT(EPOCH FROM created_at)::float8,\n\
    \          EXTRACT(EPOCH FROM updated_at)::float8\n\
    \   FROM project_onboarding_draft_repositories\n\
    \   WHERE draft_id = $1 ORDER BY position"

let q_updated_not_before_created =
  (Caqti_type.int64 ->! Caqti_type.bool)
    "SELECT updated_at >= created_at\n\
    \   FROM project_onboarding_drafts WHERE id = $1"

(* Deterministic reproduction of the lock-wait skew: a replacement's
   transaction NOW() is frozen at its BEGIN, so a draft created or
   refreshed by a concurrent transaction it then waited on can carry a
   created_at AHEAD of that NOW(). Racing two live transactions to that
   microsecond window needs timing luck; moving created_at ahead of the
   clock reproduces the exact CHECK-violating state without sleeps. The
   lifecycle CHECKs still hold: expires_at > created_at and
   updated_at >= created_at. *)
let q_future_date_draft =
  (Caqti_type.int64 ->. Caqti_type.unit)
    "UPDATE project_onboarding_drafts\n\
    \   SET created_at = NOW() + INTERVAL '1 hour',\n\
    \       updated_at = NOW() + INTERVAL '1 hour',\n\
    \       expires_at = NOW() + INTERVAL '25 hours'\n\
    \   WHERE id = $1"

(* Test-only failure injection for the rollback case: a trigger scoped
   to one reserved fixture repository id, firing only when that row is
   being SELECTED — so the reset pass and other rows' updates succeed
   first and a genuinely partial replacement exists to roll back.
   Installed and dropped inside that case alone (IF EXISTS drops keep
   the cleanup idempotent even after a mid-case failure); production
   migrations are untouched. The function body uses plain string
   quoting — Caqti templates reserve '$'. *)
let podsel_poison_repo_id = 940999999L

let q_create_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE FUNCTION podsel_fail_update_fn() RETURNS trigger\n\
    \   LANGUAGE plpgsql\n\
    \   AS 'BEGIN RAISE EXCEPTION ''podsel fixture failure''; END'"

let q_create_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "CREATE TRIGGER podsel_fail_update\n\
    \   BEFORE UPDATE ON project_onboarding_draft_repositories\n\
    \   FOR EACH ROW\n\
    \   WHEN (NEW.is_selected AND NEW.github_repository_id = 940999999)\n\
    \   EXECUTE FUNCTION podsel_fail_update_fn()"

let q_drop_fail_trigger =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP TRIGGER IF EXISTS podsel_fail_update\n\
    \   ON project_onboarding_draft_repositories"

let q_drop_fail_fn =
  (Caqti_type.unit ->. Caqti_type.unit)
    "DROP FUNCTION IF EXISTS podsel_fail_update_fn()"

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
               (fun () -> Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let repo ~account_id ?description ?default_branch ?archived ~id name =
  Github_fixture.gur_repo ~owner_id:account_id ~owner_login:"podsel-owner"
    ?description ?default_branch ?archived ~id ~name ()

(* Draft fixtures go through the real store against the real client
   chain, exactly as production writes them; the verified installation
   is returned so refresh races can rebuild the snapshot later. *)
let make_draft ?connected_by conn ~user ~ext_id repos =
  let account_id = Int64.add ext_id 100000L in
  let* inst =
    Project_fixture.insert_installation ?connected_by conn ~ext_id ~account_id
  in
  let* v =
    Project_fixture.verified ~installation_id:ext_id ~account_id
      ~login:"podsel-owner" ~target:"User" ()
  in
  let* set = Project_fixture.repo_set ~installation:v (repos account_id) in
  let* draft = Project_fixture.refresh_ok "fixture refresh" conn ~user v set in
  Lwt.return (inst, Store.draft_id draft, v)

let snapshot_ids conn draft =
  collect conn "snapshot ids" Project_fixture.q_snapshot_ids draft

let flags conn draft = collect conn "flags" Project_fixture.q_flags draft
let meta conn draft = collect conn "metadata" q_meta_sigs draft

let replace_expect label expected conn ~user ~draft ?primary selected =
  let* r = Project_fixture.replace conn ~user ~draft ?primary selected in
  match r with
  | Ok () ->
      Alcotest.failf "%s: expected %s, got Ok" label
        (Project_fixture.selection_error_str expected)
  | Error e ->
      Alcotest.(check string)
        label
        (Project_fixture.selection_error_str expected)
        (Project_fixture.selection_error_str e);
      Lwt.return_unit

let check_flags label expected conn draft =
  let* stored = flags conn draft in
  Alcotest.(check (list string)) label expected stored;
  Lwt.return_unit

let nth ids n = List.nth ids n

(* === input validation === *)

let invalid_input_case =
  db_case "replace: invalid inputs rejected before SQL, state untouched"
    (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000001L (fun account_id ->
            [
              repo ~account_id ~id:940600011L "alpha";
              repo ~account_id ~id:940600012L "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      (* Existing selection every rejected call must leave untouched. *)
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* before = flags conn draft in
      let* draft_before = Project_fixture.draft_row conn draft in
      let expect label e ~user ~draft ?primary selected =
        replace_expect label e conn ~user ~draft ?primary selected
      in
      (* User and draft ids, each independently; user id precedes. *)
      let* () = expect "user id 0" Sel.Invalid_user_id ~user:0 ~draft [ s1 ] in
      let* () =
        expect "negative user id" Sel.Invalid_user_id ~user:(-7) ~draft [ s1 ]
      in
      let* () =
        expect "user checked before draft" Sel.Invalid_user_id ~user:0 ~draft:0L
          []
      in
      let* () =
        expect "draft id 0" Sel.Invalid_draft_id ~user:uid ~draft:0L [ s1 ]
      in
      let* () =
        expect "negative draft id" Sel.Invalid_draft_id ~user:uid ~draft:(-9L)
          [ s1 ]
      in
      (* Selected-id shape, each violation on its own. *)
      let* () =
        expect "selected id 0" Sel.Invalid_selection ~user:uid ~draft [ 0L ]
      in
      let* () =
        expect "negative selected id" Sel.Invalid_selection ~user:uid ~draft
          [ -3L ]
      in
      let* () =
        expect "duplicate selected id" Sel.Invalid_selection ~user:uid ~draft
          [ s1; s1 ]
      in
      let* () =
        expect "over 2000 selected ids" Sel.Invalid_selection ~user:uid ~draft
          (List.init 2001 (fun i -> Int64.of_int (i + 1)))
      in
      (* Primary shape. *)
      let* () =
        expect "primary id 0" Sel.Invalid_selection ~user:uid ~draft ~primary:0L
          [ s1 ]
      in
      let* () =
        expect "negative primary id" Sel.Invalid_selection ~user:uid ~draft
          ~primary:(-2L) [ s1 ]
      in
      let* () =
        expect "primary absent from selected" Sel.Invalid_selection ~user:uid
          ~draft ~primary:s2 [ s1 ]
      in
      let* () =
        expect "primary with empty selection" Sel.Invalid_selection ~user:uid
          ~draft ~primary:s1 []
      in
      (* Ordering proof that validation precedes SQL: an absent draft
         would be Draft_unavailable had authorization run. *)
      let* absent =
        find conn "absent draft id" Project_fixture.q_absent_draft_id ()
      in
      let* () =
        expect "validation precedes authorization" Sel.Invalid_selection
          ~user:uid ~draft:absent [ 0L ]
      in
      let* draft_after = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" draft_before
        draft_after;
      check_flags "selection untouched by rejected inputs" before conn draft)

(* === basic replacement === *)

let basic_replacement_case =
  db_case "replace: subset plus primary, everything else untouched" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000002L (fun account_id ->
            [
              repo ~account_id ~id:940600021L
                ~description:{|"Selezione — byte exact"|} "alpha";
              repo ~account_id ~id:940600022L "beta";
              repo ~account_id ~id:940600023L ~default_branch:"release/v1"
                ~archived:true "gamma";
              repo ~account_id ~id:940600024L "delta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let s3 = nth ids 2 and s4 = nth ids 3 in
      let* meta_before = meta conn draft in
      let* epochs_before = collect conn "repo epochs" q_repo_epochs draft in
      let* _, (_, (created_b, updated_b, expires_b)) =
        Project_fixture.draft_row conn draft
      in
      let* () =
        Project_fixture.replace_ok "replace" conn ~user:uid ~draft ~primary:s2
          [ s2; s4 ]
      in
      let* () =
        check_flags "exactly the subset selected, one primary"
          [
            Project_fixture.flag_sig s1 ~selected:false ~primary:false;
            Project_fixture.flag_sig s2 ~selected:true ~primary:true;
            Project_fixture.flag_sig s3 ~selected:false ~primary:false;
            Project_fixture.flag_sig s4 ~selected:true ~primary:false;
          ]
          conn draft
      in
      let* meta_after = meta conn draft in
      Alcotest.(check (list string))
        "metadata and ordering byte-identical" meta_before meta_after;
      let* ( (_, _, status),
             ((no_completed, no_cancelled), (created_a, updated_a, expires_a)) )
          =
        Project_fixture.draft_row conn draft
      in
      Alcotest.(check string) "still active" "active" status;
      Alcotest.(check bool) "completed_at still NULL" true no_completed;
      Alcotest.(check bool) "cancelled_at still NULL" true no_cancelled;
      Alcotest.(check (float 0.))
        "draft created_at unchanged" created_b created_a;
      Alcotest.(check (float 0.)) "expires_at NOT renewed" expires_b expires_a;
      Alcotest.(check bool)
        "draft updated_at advances or stays equal" true (updated_a >= updated_b);
      let* epochs_after = collect conn "repo epochs" q_repo_epochs draft in
      List.iter2
        (fun (id_b, created_rb, updated_rb) (id_a, created_ra, updated_ra) ->
          Alcotest.(check int64) "same row" id_b id_a;
          Alcotest.(check (float 0.))
            "repo created_at unchanged" created_rb created_ra;
          Alcotest.(check bool)
            "repo updated_at advances or stays equal" true
            (updated_ra >= updated_rb))
        epochs_before epochs_after;
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "no draft created" 1 n;
      Lwt.return_unit)

(* === empty selection === *)

let empty_selection_case =
  db_case "replace: empty selection clears every flag" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000003L (fun account_id ->
            [
              repo ~account_id ~id:940600031L "alpha";
              repo ~account_id ~id:940600032L "beta";
              repo ~account_id ~id:940600033L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "seed selected and primary" conn ~user:uid
          ~draft ~primary:s2 [ s1; s2 ]
      in
      let* () = Project_fixture.replace_ok "clear" conn ~user:uid ~draft [] in
      check_flags "all rows unselected and non-primary"
        [
          Project_fixture.flag_sig s1 ~selected:false ~primary:false;
          Project_fixture.flag_sig s2 ~selected:false ~primary:false;
          Project_fixture.flag_sig s3 ~selected:false ~primary:false;
        ]
        conn draft)

(* === replacement of previous state === *)

let previous_state_case =
  db_case "replace: selection B retains nothing from selection A" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000004L (fun account_id ->
            [
              repo ~account_id ~id:940600041L "alpha";
              repo ~account_id ~id:940600042L "beta";
              repo ~account_id ~id:940600043L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "selection A" conn ~user:uid ~draft
          ~primary:s2 [ s1; s2 ]
      in
      let* () =
        Project_fixture.replace_ok "selection B" conn ~user:uid ~draft
          ~primary:s3 [ s2; s3 ]
      in
      check_flags "final state is exactly B"
        [
          Project_fixture.flag_sig s1 ~selected:false ~primary:false;
          Project_fixture.flag_sig s2 ~selected:true ~primary:false;
          Project_fixture.flag_sig s3 ~selected:true ~primary:true;
        ]
        conn draft)

(* === no-primary selection === *)

let no_primary_case =
  db_case "replace: multiple selected with no primary" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000005L (fun account_id ->
            [
              repo ~account_id ~id:940600051L "alpha";
              repo ~account_id ~id:940600052L "beta";
              repo ~account_id ~id:940600053L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "no primary" conn ~user:uid ~draft [ s1; s3 ]
      in
      check_flags "selected rows selected, every row non-primary"
        [
          Project_fixture.flag_sig s1 ~selected:true ~primary:false;
          Project_fixture.flag_sig s2 ~selected:false ~primary:false;
          Project_fixture.flag_sig s3 ~selected:true ~primary:false;
        ]
        conn draft)

(* === ownership isolation === *)

let ownership_case =
  db_case "replace: strict owner isolation, provenance changes nothing"
    (fun conn ->
      let* a = insert_user conn "podsel_a" in
      let* b = insert_user conn "podsel_b" in
      (* One shared installation: both users hold a draft on it, so the
         cross-user probe presents ids that DO exist — just not on a
         draft the caller owns. *)
      let ext_id = 940000006L in
      let account_id = Int64.add ext_id 100000L in
      let* inst =
        Project_fixture.insert_installation conn ~ext_id ~account_id
      in
      let* v =
        Project_fixture.verified ~installation_id:ext_id ~account_id
          ~login:"podsel-owner" ~target:"User" ()
      in
      let* set_a =
        Project_fixture.repo_set ~installation:v
          [
            repo ~account_id ~id:940600061L "alpha";
            repo ~account_id ~id:940600062L "beta";
          ]
      in
      let* da = Project_fixture.refresh_ok "A's draft" conn ~user:a v set_a in
      let da = Store.draft_id da in
      let* set_b =
        Project_fixture.repo_set ~installation:v
          [
            repo ~account_id ~id:940600063L "gamma";
            repo ~account_id ~id:940600064L "delta";
          ]
      in
      let* db_ = Project_fixture.refresh_ok "B's draft" conn ~user:b v set_b in
      let db_ = Store.draft_id db_ in
      let* ids_a = snapshot_ids conn da in
      let* ids_b = snapshot_ids conn db_ in
      let a1 = nth ids_a 0 and a2 = nth ids_a 1 in
      let b1 = nth ids_b 0 and b2 = nth ids_b 1 in
      let* () =
        Project_fixture.replace_ok "A modifies A's draft" conn ~user:a ~draft:da
          ~primary:a1 [ a1 ]
      in
      let* () =
        Project_fixture.replace_ok "B modifies B's draft" conn ~user:b
          ~draft:db_ ~primary:b2 [ b1; b2 ]
      in
      let b_flags =
        [
          Project_fixture.flag_sig b1 ~selected:true ~primary:false;
          Project_fixture.flag_sig b2 ~selected:true ~primary:true;
        ]
      in
      (* A on B's draft: authorization collapses before any stale
         check — the supplied ids are B's real snapshot ids. *)
      let* () =
        replace_expect "A attempting B's draft" Sel.Draft_unavailable conn
          ~user:a ~draft:db_ [ b1 ]
      in
      let* () = check_flags "B's selection unchanged" b_flags conn db_ in
      (* Provenance is never ownership: pointing it at A changes
         nothing, in either direction. *)
      let* () =
        exec conn "provenance to A" Project_fixture.q_set_provenance
          (inst, Some a)
      in
      let* () =
        replace_expect "A still cannot touch B's draft" Sel.Draft_unavailable
          conn ~user:a ~draft:db_ [ b1 ]
      in
      let* () =
        Project_fixture.replace_ok "B still can" conn ~user:b ~draft:db_
          ~primary:b2 [ b1; b2 ]
      in
      let* () =
        exec conn "provenance to NULL" Project_fixture.q_set_provenance
          (inst, None)
      in
      let* () =
        Project_fixture.replace_ok "A on A's draft regardless of provenance"
          conn ~user:a ~draft:da ~primary:a2 [ a2 ]
      in
      check_flags "B's selection still intact" b_flags conn db_)

(* === availability filtering === *)

(* One unavailable-state scaffold: a seeded selection, the mutation,
   then a rejected replacement that must leave both the draft row (as
   mutated) and the selection flags untouched. *)
let unavailable_case name ~ext_id mutate =
  db_case name (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* inst, draft, _ =
        make_draft conn ~user:uid ~ext_id (fun account_id ->
            [
              repo ~account_id ~id:(Int64.add ext_id 600000L) "alpha";
              repo ~account_id ~id:(Int64.add ext_id 600001L) "beta";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 in
      let* () =
        Project_fixture.replace_ok "seed selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* () = mutate conn ~inst ~draft in
      let* before_flags = flags conn draft in
      let* before_row = Project_fixture.draft_row conn draft in
      let* () =
        replace_expect name Sel.Draft_unavailable conn ~user:uid ~draft [ s2 ]
      in
      let* after_row = Project_fixture.draft_row conn draft in
      Project_fixture.check_same_draft_row "draft row untouched" before_row
        after_row;
      check_flags "selection untouched" before_flags conn draft)

let unavailable_expired_case =
  unavailable_case "replace: expired draft is unavailable, not revived"
    ~ext_id:940000031L (fun conn ~inst:_ ~draft ->
      let* () = exec conn "expire" Project_fixture.q_backdate_draft draft in
      Lwt.return_unit)

let unavailable_completed_case =
  unavailable_case "replace: completed draft is unavailable" ~ext_id:940000032L
    (fun conn ~inst:_ ~draft ->
      exec conn "complete" Project_fixture.q_complete_draft draft)

let unavailable_cancelled_case =
  unavailable_case "replace: cancelled draft is unavailable" ~ext_id:940000033L
    (fun conn ~inst:_ ~draft ->
      exec conn "cancel" Project_fixture.q_cancel_draft draft)

let unavailable_inaccessible_case =
  unavailable_case "replace: inaccessible installation blocks the draft"
    ~ext_id:940000034L (fun conn ~inst ~draft:_ ->
      exec conn "inaccessible" Project_fixture.q_set_installation_status
        (inst, "inaccessible", false))

let unavailable_revoked_case =
  unavailable_case "replace: revoked installation status blocks the draft"
    ~ext_id:940000035L (fun conn ~inst ~draft:_ ->
      exec conn "revoked" Project_fixture.q_set_installation_status
        (inst, "revoked", false))

let unavailable_revoked_at_case =
  unavailable_case "replace: non-NULL revoked_at blocks the draft"
    ~ext_id:940000036L (fun conn ~inst ~draft:_ ->
      exec conn "revoked with timestamp"
        Project_fixture.q_set_installation_status (inst, "revoked", true))

(* === expiry invariant === *)

let expiry_invariant_case =
  db_case "replace: expiry is never renewed and never revived" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000037L (fun account_id ->
            [ repo ~account_id ~id:940600371L "alpha" ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 in
      let* _, (_, (_, _, expires_b)) = Project_fixture.draft_row conn draft in
      let* () =
        Project_fixture.replace_ok "replace" conn ~user:uid ~draft ~primary:s1
          [ s1 ]
      in
      let* _, (_, (_, _, expires_a)) = Project_fixture.draft_row conn draft in
      Alcotest.(check (float 0.))
        "expires_at byte-identical" expires_b expires_a;
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "no draft created" 1 n;
      (* Once expired, replacement neither succeeds nor revives. *)
      let* () = exec conn "expire" Project_fixture.q_backdate_draft draft in
      let* () =
        replace_expect "expired stays unavailable" Sel.Draft_unavailable conn
          ~user:uid ~draft [ s1 ]
      in
      let* expired =
        find conn "expiry probe" Project_fixture.q_is_expired draft
      in
      Alcotest.(check bool) "still expired" true expired;
      let* n = Project_fixture.count_for_user conn uid in
      Alcotest.(check int) "still exactly one draft" 1 n;
      Lwt.return_unit)

(* === stale snapshot ids === *)

let stale_after_refresh_case =
  db_case "replace: ids from a replaced snapshot are stale" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, v =
        make_draft conn ~user:uid ~ext_id:940000041L (fun account_id ->
            [
              repo ~account_id ~id:940600411L "alpha";
              repo ~account_id ~id:940600412L "beta";
            ])
      in
      let* old_ids = snapshot_ids conn draft in
      (* A new verification replaces the snapshot: new rows, new ids. *)
      let account_id = Int64.add 940000041L 100000L in
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [
            repo ~account_id ~id:940600413L "gamma";
            repo ~account_id ~id:940600414L "delta";
          ]
      in
      let* d2 = Project_fixture.refresh_ok "refresh" conn ~user:uid v set2 in
      Alcotest.(check int64) "same draft refreshed" draft (Store.draft_id d2);
      let* new_ids = snapshot_ids conn draft in
      Alcotest.(check bool)
        "fixture: all snapshot ids replaced" true
        (List.for_all
           (fun old_id -> not (List.exists (Int64.equal old_id) new_ids))
           old_ids);
      let n1 = nth new_ids 0 and n2 = nth new_ids 1 in
      let unselected =
        [
          Project_fixture.flag_sig n1 ~selected:false ~primary:false;
          Project_fixture.flag_sig n2 ~selected:false ~primary:false;
        ]
      in
      let* () =
        replace_expect "old ids are stale" Sel.Selection_stale conn ~user:uid
          ~draft ~primary:(nth old_ids 0) old_ids
      in
      let* () =
        check_flags "refreshed snapshot entirely unselected" unselected conn
          draft
      in
      (* Arbitrary positive ids collapse the same way, alone or mixed
         with genuine current ids. *)
      let* () =
        replace_expect "arbitrary positive id" Sel.Selection_stale conn
          ~user:uid ~draft [ 999999999999L ]
      in
      let* () =
        replace_expect "mixture of current and stale" Sel.Selection_stale conn
          ~user:uid ~draft
          [ n1; nth old_ids 0 ]
      in
      check_flags "no accidental selection applied" unselected conn draft)

(* === cross-draft ids === *)

let cross_draft_case =
  db_case "replace: another draft's ids are stale after authorization"
    (fun conn ->
      let* a = insert_user conn "podsel_a" in
      let* b = insert_user conn "podsel_b" in
      let* _, target, _ =
        make_draft conn ~user:a ~ext_id:940000042L (fun account_id ->
            [ repo ~account_id ~id:940600421L "alpha" ])
      in
      (* Same user, different draft. *)
      let* _, same_user_draft, _ =
        make_draft conn ~user:a ~ext_id:940000043L (fun account_id ->
            [ repo ~account_id ~id:940600431L "beta" ])
      in
      (* Another user's draft. *)
      let* _, other_user_draft, _ =
        make_draft conn ~user:b ~ext_id:940000044L (fun account_id ->
            [ repo ~account_id ~id:940600441L "gamma" ])
      in
      let* target_ids = snapshot_ids conn target in
      let* same_ids = snapshot_ids conn same_user_draft in
      let* other_ids = snapshot_ids conn other_user_draft in
      let t1 = nth target_ids 0 in
      let s1 = nth same_ids 0 in
      let o1 = nth other_ids 0 in
      let* target_before = flags conn target in
      let* same_before = flags conn same_user_draft in
      let* other_before = flags conn other_user_draft in
      (* The target draft IS owner-authorized; only then does the
         foreign id collapse to stale — selected and primary alike. *)
      let* () =
        replace_expect "same user's other draft" Sel.Selection_stale conn
          ~user:a ~draft:target [ s1 ]
      in
      let* () =
        replace_expect "another user's draft" Sel.Selection_stale conn ~user:a
          ~draft:target [ o1 ]
      in
      let* () =
        replace_expect "foreign primary" Sel.Selection_stale conn ~user:a
          ~draft:target ~primary:s1 [ t1; s1 ]
      in
      let* () = check_flags "target unmodified" target_before conn target in
      let* () =
        check_flags "same-user donor unmodified" same_before conn
          same_user_draft
      in
      check_flags "other-user donor unmodified" other_before conn
        other_user_draft)

(* === atomic rollback === *)

let rollback_case =
  db_case "replace: a failed row update rolls back everything" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000051L (fun account_id ->
            [
              repo ~account_id ~id:940600511L "alpha";
              repo ~account_id ~id:940600512L "beta";
              repo ~account_id ~id:podsel_poison_repo_id "poison";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let* () =
        Project_fixture.replace_ok "previous selection" conn ~user:uid ~draft
          ~primary:s1 [ s1 ]
      in
      let* before_flags = flags conn draft in
      let* before_row = Project_fixture.draft_row conn draft in
      let exec_ddl label q =
        let* r = C.exec q () in
        let* () = or_fail label r in
        Lwt.return_unit
      in
      let* () = exec_ddl "pre-drop trigger" q_drop_fail_trigger in
      let* () = exec_ddl "pre-drop function" q_drop_fail_fn in
      let* () = exec_ddl "create function" q_create_fail_fn in
      let* () = exec_ddl "create trigger" q_create_fail_trigger in
      Lwt.finalize
        (fun () ->
          (* The poison row is ordered last, so the reset and the s2
             update have already succeeded inside the transaction when
             the failure fires — a genuinely partial replacement must
             vanish. *)
          let* () =
            replace_expect "poisoned replacement" Sel.Storage_error conn
              ~user:uid ~draft ~primary:s2 [ s2; s3 ]
          in
          let* after_row = Project_fixture.draft_row conn draft in
          Project_fixture.check_same_draft_row
            "draft row (incl. updated_at) unchanged" before_row after_row;
          check_flags "previous selection fully intact, nothing partial"
            before_flags conn draft)
        (fun () ->
          let* () = exec_ddl "drop trigger" q_drop_fail_trigger in
          exec_ddl "drop function" q_drop_fail_fn))

(* === concurrent replacements === *)

let concurrent_replacements_case =
  db_case "replace: concurrent selections leave one complete winner"
    (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000052L (fun account_id ->
            [
              repo ~account_id ~id:940600521L "alpha";
              repo ~account_id ~id:940600522L "beta";
              repo ~account_id ~id:940600523L "gamma";
            ])
      in
      let* ids = snapshot_ids conn draft in
      let s1 = nth ids 0 and s2 = nth ids 1 and s3 = nth ids 2 in
      let flags_a =
        [
          Project_fixture.flag_sig s1 ~selected:true ~primary:true;
          Project_fixture.flag_sig s2 ~selected:false ~primary:false;
          Project_fixture.flag_sig s3 ~selected:false ~primary:false;
        ]
      in
      let flags_b =
        [
          Project_fixture.flag_sig s1 ~selected:false ~primary:false;
          Project_fixture.flag_sig s2 ~selected:true ~primary:false;
          Project_fixture.flag_sig s3 ~selected:true ~primary:true;
        ]
      in
      Db_fixture.with_second_connection (fun conn2 ->
          let* r1, r2 =
            Lwt.both
              (Project_fixture.replace conn ~user:uid ~draft ~primary:s1 [ s1 ])
              (Project_fixture.replace conn2 ~user:uid ~draft ~primary:s3
                 [ s2; s3 ])
          in
          let check_ok label = function
            | Ok () -> ()
            | Error e ->
                Alcotest.failf "%s: %s" label
                  (Project_fixture.selection_error_str e)
          in
          check_ok "selection A" r1;
          check_ok "selection B" r2;
          let* stored = flags conn draft in
          (* Which submission wins is not asserted; the final state must
             be one COMPLETE submission — the exact-signature comparison
             rules out mixtures and duplicate primaries at once. *)
          Alcotest.(check bool)
            "final state equals all of A or all of B" true
            (stored = flags_a || stored = flags_b);
          Lwt.return_unit))

(* === concurrent refresh === *)

let concurrent_refresh_case =
  db_case "replace: racing a snapshot refresh serializes cleanly" (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let ext_id = 940000053L in
      let account_id = Int64.add ext_id 100000L in
      let* _, draft, v =
        make_draft conn ~user:uid ~ext_id (fun account_id ->
            [
              repo ~account_id ~id:940600531L "alpha";
              repo ~account_id ~id:940600532L "beta";
            ])
      in
      let* old_ids = snapshot_ids conn draft in
      let* set2 =
        Project_fixture.repo_set ~installation:v
          [ repo ~account_id ~id:940600533L "gamma" ]
      in
      Db_fixture.with_second_connection (fun conn2 ->
          (* Both take the draft row lock first (SELECT FOR UPDATE vs
             the upsert), so PostgreSQL serializes them without
             deadlock — no sleeps needed. *)
          let* r_sel, r_ref =
            Lwt.both
              (Project_fixture.replace conn ~user:uid ~draft
                 ~primary:(nth old_ids 0) old_ids)
              (Project_fixture.refresh conn2 ~user:uid v set2)
          in
          (match r_ref with
          | Ok d ->
              Alcotest.(check int64)
                "refresh kept the draft id" draft (Store.draft_id d)
          | Error e ->
              Alcotest.failf "refresh: %s" (Project_fixture.draft_error_str e));
          (* The two serialized outcomes: selection first (then wiped by
             the refresh) or refresh first (selection sees only new
             ids). Either way no old id may select a new row. *)
          (match r_sel with
          | Ok () | Error Sel.Selection_stale -> ()
          | Error e ->
              Alcotest.failf "selection: unexpected %s"
                (Project_fixture.selection_error_str e));
          let* stored_sigs = Project_fixture.sigs conn draft in
          Alcotest.(check (list string))
            "one complete refreshed snapshot, entirely unselected"
            [
              Project_fixture.sig_of ~position:1 ~id:940600533L ~account_id
                ~login:"podsel-owner" "gamma";
            ]
            stored_sigs;
          let* new_ids = snapshot_ids conn draft in
          Alcotest.(check bool)
            "no old snapshot id survives" true
            (List.for_all
               (fun old_id -> not (List.exists (Int64.equal old_id) new_ids))
               old_ids);
          Lwt.return_unit))

(* === timestamp race regression === *)

let timestamp_clamp_case =
  db_case "replace: updated_at clamps to a created_at ahead of NOW()"
    (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let* _, draft, _ =
        make_draft conn ~user:uid ~ext_id:940000054L (fun account_id ->
            [ repo ~account_id ~id:940600541L "alpha" ])
      in
      let* ids = snapshot_ids conn draft in
      (* See q_future_date_draft: the draft's created_at now sits ahead
         of any NOW() the replacement's transaction can freeze — exactly
         the state a lock-wait behind a concurrent create/refresh
         produces. Plain NOW() would trip the updated_at >= created_at
         CHECK here; GREATEST(NOW(), created_at) must not. *)
      let* () = exec conn "future-date" q_future_date_draft draft in
      let* _, (_, (created_b, _, _)) = Project_fixture.draft_row conn draft in
      let* () =
        Project_fixture.replace_ok "replacement under skew" conn ~user:uid
          ~draft ~primary:(nth ids 0)
          [ nth ids 0 ]
      in
      let* invariant =
        find conn "invariant" q_updated_not_before_created draft
      in
      Alcotest.(check bool) "updated_at >= created_at" true invariant;
      let* _, (_, (created_a, updated_a, _)) =
        Project_fixture.draft_row conn draft
      in
      Alcotest.(check (float 0.)) "created_at untouched" created_b created_a;
      Alcotest.(check (float 0.))
        "updated_at clamped to created_at" created_a updated_a;
      check_flags "selection applied despite the skew"
        [ Project_fixture.flag_sig (nth ids 0) ~selected:true ~primary:true ]
        conn draft)

(* === credential prohibition === *)

let privacy_case =
  db_case "replace: no credential material in any touched stored field"
    (fun conn ->
      let* uid = insert_user conn "podsel_a" in
      let ext_id = 940000099L in
      let account_id = Int64.add ext_id 100000L in
      let* _ = Project_fixture.insert_installation conn ~ext_id ~account_id in
      (* The whole fixture chain rides the refresh-token exchange, so an
         access token, refresh token, code, verifier, and secret all
         exist to leak — and must not. *)
      let* v =
        Project_fixture.verified ~token_body:Project_fixture.refresh_token_body
          ~installation_id:ext_id ~account_id ~login:"podsel-owner"
          ~target:"User" ()
      in
      let* set =
        Project_fixture.repo_set ~token_body:Project_fixture.refresh_token_body
          ~installation:v
          [
            repo ~account_id ~id:940600991L
              ~description:{|"benign description"|} "alpha";
          ]
      in
      let* draft =
        Project_fixture.refresh_ok "fixture refresh" conn ~user:uid v set
      in
      let draft = Store.draft_id draft in
      let* ids = snapshot_ids conn draft in
      let* () =
        Project_fixture.replace_ok "replace" conn ~user:uid ~draft
          ~primary:(nth ids 0)
          [ nth ids 0 ]
      in
      let* (_, _, status), _ = Project_fixture.draft_row conn draft in
      let* stored_sigs = Project_fixture.sigs conn draft in
      (* Every text value either table stores for this draft after the
         replacement touched its rows. *)
      let blob = String.concat "|" (status :: stored_sigs) in
      List.iter
        (fun (label, needle) ->
          Alcotest.(check bool)
            (label ^ " absent from stored values")
            false
            (Html_assert.contains_nonempty ~needle blob))
        [
          ("access token", Project_fixture.pods_access_fixture);
          ("refresh token", Project_fixture.pods_refresh_fixture);
          ("authorization code", Github_fixture.gte_code_string);
          ("PKCE verifier", Github_fixture.gte_verifier_string);
          ("client secret", Github_fixture.gte_client_secret);
        ];
      Lwt.return_unit)

let suite =
  [
    invalid_input_case;
    basic_replacement_case;
    empty_selection_case;
    previous_state_case;
    no_primary_case;
    ownership_case;
    unavailable_expired_case;
    unavailable_completed_case;
    unavailable_cancelled_case;
    unavailable_inaccessible_case;
    unavailable_revoked_case;
    unavailable_revoked_at_case;
    expiry_invariant_case;
    stale_after_refresh_case;
    cross_draft_case;
    rollback_case;
    concurrent_replacements_case;
    concurrent_refresh_case;
    timestamp_clamp_case;
    privacy_case;
  ]

let suites =
  (* Draft selection store: owner-authorized transactional selection
       replacement — availability lock, stale-snapshot detection, atomic
       complete replacement, concurrency with refreshes; same
       EARDE_TEST_DATABASE_URL gate (each case skips without it). *)
  [ ("project_onboarding_draft_selection_store", suite) ]
