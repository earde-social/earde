(* Scoped network-community lifecycle constraint (migration 20260726130000):
   the same drop/restore pattern as Ncid_relax, for the fixtures that
   deliberately write lifecycle shapes the CHECK now forbids at the database
   — a published network community reverted to private, a leaking or public
   draft, mixed published flags. Those durable shapes are no longer
   producible in production, but the defensive branches that tolerate or
   reject them are still worth exercising, so the affected fixtures relax
   the constraint for the length of one case and restore it (validated)
   after their rows are re-normalized or deleted. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let drop_statements =
  [ ddl
      "ALTER TABLE communities \
       DROP CONSTRAINT IF EXISTS communities_network_lifecycle_check"
  ]

let add_statements =
  [ ddl
      "ALTER TABLE communities \
       ADD CONSTRAINT communities_network_lifecycle_check CHECK ( \
         NOT is_network_community OR ( \
           (onboarding_state = 'draft' \
            AND visibility = 'private' \
            AND NOT indexable \
            AND NOT discoverable) \
           OR \
           (onboarding_state = 'published' \
            AND visibility = 'public' \
            AND indexable = discoverable)))"
  ]

let run conn statements =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  Lwt_list.iter_s
    (fun q ->
      let* r = C.exec q () in
      let* _ = Db_fixture.or_fail "lifecycle constraint DDL" r in
      Lwt.return_unit)
    statements

let drop conn = run conn drop_statements

let restore conn = run conn add_statements

let run_cleanup conn queries =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  Lwt_list.iter_s
    (fun q ->
      let* r = C.exec q () in
      let* _ = Db_fixture.or_fail "relaxed cleanup" r in
      Lwt.return_unit)
    queries

(* Wrap one case body: the constraint is dropped before it runs, and the
   given cleanup (normally the suite's own fixture deletes) removes every
   drift row before the constraint is restored, validated, under
   Lwt.finalize — even when an assertion fails mid-case. *)
let around conn ~cleanup f =
  let* () = drop conn in
  Lwt.finalize f (fun () -> Lwt.finalize cleanup (fun () -> restore conn))
