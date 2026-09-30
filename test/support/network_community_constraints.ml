(* === Scoped network-community identity constraints (shared helpers) ===
   Migration 20260726120000 bars noncanonical identity values on
   is_network_community rows at the database boundary, which also bars the
   corruption several gated cases plant on purpose to exercise
   application-level Inconsistent_data defenses. Those cases drop the
   three named constraints for one probe and restore them under
   Lwt.finalize once the corrupt rows are canonical or gone; the schema
   suite reuses the exact same statements for its down/up round-trip
   proof. The ADD statements are byte-for-byte the migration's, so a
   drifted migration fails these tests rather than silently diverging. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let ddl sql = (Caqti_type.unit ->. Caqti_type.unit) sql

let drop_statements =
  [
    ddl
      "ALTER TABLE communities DROP CONSTRAINT IF EXISTS \
       communities_network_name_check";
    ddl
      "ALTER TABLE communities DROP CONSTRAINT IF EXISTS \
       communities_network_slug_check";
    ddl
      "ALTER TABLE communities DROP CONSTRAINT IF EXISTS \
       communities_network_description_check";
  ]

let add_statements =
  [
    ddl
      "ALTER TABLE communities ADD CONSTRAINT communities_network_name_check \
       CHECK ( NOT is_network_community OR ( char_length(name) >= 1 AND \
       char_length(name) <= 120 AND name !~ '[\\x01-\\x1f\\x7f]' AND name !~ \
       '^ ' AND name !~ ' $'))";
    ddl
      "ALTER TABLE communities ADD CONSTRAINT communities_network_slug_check \
       CHECK ( NOT is_network_community OR ( slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$' \
       AND char_length(slug) <= 80))";
    ddl
      "ALTER TABLE communities ADD CONSTRAINT \
       communities_network_description_check CHECK ( NOT is_network_community \
       OR description IS NULL OR ( char_length(description) >= 1 AND \
       char_length(description) <= 2000 AND description !~ \
       '[\\x01-\\x08\\x0b-\\x1f\\x7f]' AND description !~ '^[ \\t\\n]' AND \
       description !~ '[ \\t\\n]$'))";
  ]

let run conn statements =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  Lwt_list.iter_s
    (fun q ->
      let* r = C.exec q () in
      let* _ = Db_fixture.or_fail "identity constraint DDL" r in
      Lwt.return_unit)
    statements

let drop conn = run conn drop_statements
let restore conn = run conn add_statements

(* One canonicalizing restore for finalize blocks: whatever a probe left
   behind, the row is valid again before the constraints return. *)
let q_recanonicalize =
  (Caqti_type.(t3 int string string) ->. Caqti_type.unit)
    "UPDATE communities SET slug = $2, name = $3, description = NULL WHERE id \
     = $1"
