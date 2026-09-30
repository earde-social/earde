(* === Network-community identity constraints (migration 20260726120000) ===
   The three scoped CHECK constraints on communities: canonical name, slug,
   and description whenever is_network_community is TRUE, with legacy rows
   exempt. Probes run autocommit against the real table with ncid% slugs;
   the down/up round-trip runs inside one rolled-back transaction using
   Ncid_relax's byte-identical statements, so the production constraints
   are never left missing even on assertion failure. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM communities WHERE slug LIKE 'ncid%'" ]

let q_insert_network =
  (Caqti_type.(t3 string string (option string)) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, description, is_network_community) \
     VALUES ($1, $2, $3, TRUE) RETURNING id"

let q_insert_legacy =
  (Caqti_type.(t3 string string (option string)) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, description, is_network_community) \
     VALUES ($1, $2, $3, FALSE) RETURNING id"

let q_identity =
  (Caqti_type.int ->! Caqti_type.(t2 (t2 string string) (option string)))
    "SELECT slug, name, description FROM communities WHERE id = $1"

(* Exactly the three identity constraints: the sibling scoped lifecycle
   CHECK (communities_network_lifecycle_check, migration 20260726130000)
   shares the prefix and has its own suite. *)
let q_constraints =
  (Caqti_type.unit ->* Caqti_type.(t2 string bool))
    "SELECT conname, convalidated FROM pg_constraint WHERE conrelid = \
     'communities'::regclass AND contype = 'c' AND conname LIKE \
     'communities_network_%' AND conname <> \
     'communities_network_lifecycle_check' ORDER BY conname"

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

let accept label conn ?description ~slug ~name () =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* id = C.find q_insert_network (slug, name, description) in
  or_fail label id

let refuse label conn ?description ~slug ~name () =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* r = C.find q_insert_network (slug, name, description) in
  reject label r

let constraint_rows conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rows = C.collect_list q_constraints () in
  or_fail "constraint rows" rows

(* === canonical acceptance === *)

let valid_case =
  db_case "identity: canonical network identities are accepted byte-exactly"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      let name = "Ncid Community \xc3\xa8" in
      let description = "Prima riga.\nSeconda con\ttab \xe2\x98\x95" in
      let* id =
        accept "canonical" conn ~slug:"ncid-valid" ~name ~description ()
      in
      let* stored = C.find q_identity id in
      let* (slug_back, name_back), description_back =
        or_fail "readback" stored
      in
      Alcotest.(check string) "slug byte-exact" "ncid-valid" slug_back;
      Alcotest.(check string) "name byte-exact" name name_back;
      Alcotest.(check (option string))
        "description byte-exact" (Some description) description_back;
      (* NULL description is a first-class canonical value. *)
      let* _ =
        accept "no description" conn ~slug:"ncid-nodesc" ~name:"Ncid Nodesc" ()
      in
      Lwt.return_unit)

(* === slug grammar === *)

let slug_case =
  db_case "identity: the network slug grammar binds at the database"
    (fun conn ->
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            refuse
              ("slug " ^ String.escaped bad)
              conn ~slug:bad ~name:"Ncid Name" ())
          [
            "ncid/slash";
            "Ncid-Upper";
            "ncid slug";
            "ncid_underscore";
            " ncid-pad";
            "-ncid";
            "ncid-";
            "ncid--a";
            "ncid-" ^ String.make 76 'a' (* 81 characters *);
          ]
      in
      (* The exact 80-character boundary is legal. *)
      let* _ =
        accept "80-character slug" conn
          ~slug:("ncid-" ^ String.make 75 'a')
          ~name:"Ncid Eighty" ()
      in
      Lwt.return_unit)

(* === name policy === *)

let name_case =
  db_case "identity: the network name policy binds at the database" (fun conn ->
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            refuse
              ("name " ^ String.escaped bad)
              conn ~slug:"ncid-name-probe" ~name:bad ())
          [
            "";
            " padded";
            "padded ";
            "\tpadded";
            "padded\t";
            "\npadded";
            "padded\x0b";
            "padded\x0c";
            "padded\r";
            "in\x01side";
            "in\x1fside";
            "in\x7fside";
            Home_provisioning_fixture.phvf_repeat
              Home_provisioning_fixture.phvf_scalar 121;
          ]
      in
      (* 120 Unicode scalars — counted per character, not per byte. *)
      let* _ =
        accept "120-scalar name" conn ~slug:"ncid-name-120"
          ~name:
            (Home_provisioning_fixture.phvf_repeat
               Home_provisioning_fixture.phvf_scalar 120)
          ()
      in
      Lwt.return_unit)

(* === description policy === *)

let description_case =
  db_case "identity: the network description policy binds at the database"
    (fun conn ->
      let* () =
        Lwt_list.iter_s
          (fun bad ->
            refuse
              ("description " ^ String.escaped bad)
              conn ~slug:"ncid-desc-probe" ~name:"Ncid Desc" ~description:bad ())
          [
            "" (* empty canonical descriptions must be NULL, never '' *);
            " padded";
            "padded ";
            "\npadded";
            "padded\n";
            "\tpadded";
            "padded\t";
            "with\rreturn";
            "with\x01control";
            "with\x0bcontrol";
            "with\x7fdel";
            Home_provisioning_fixture.phvf_repeat
              Home_provisioning_fixture.phvf_scalar 2001;
          ]
      in
      (* LF and tab survive as content; 2,000 scalars is the boundary. *)
      let* _ =
        accept "multiline description" conn ~slug:"ncid-desc-multi"
          ~name:"Ncid Multi" ~description:"Line one.\nLine\ttwo." ()
      in
      let* _ =
        accept "2000-scalar description" conn ~slug:"ncid-desc-2000"
          ~name:"Ncid Bound"
          ~description:
            (Home_provisioning_fixture.phvf_repeat
               Home_provisioning_fixture.phvf_scalar 2000)
          ()
      in
      Lwt.return_unit)

(* === legacy exemption === *)

let legacy_case =
  db_case "identity: legacy rows stay accepted with noncanonical values"
    (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      (* Every value below violates the network policy — noncanonical
         slug, padded control-bearing name, empty-string description —
         and all of it stays legal while is_network_community is
         FALSE. *)
      let* id =
        C.find q_insert_legacy
          ("ncid LEGACY_slug//", "  padded \x01 name  ", Some "")
      in
      let* _ = or_fail "legacy row" id in
      Lwt.return_unit)

(* === presence and validation === *)

let presence_case =
  db_case "identity: all three constraints are present and validated"
    (fun conn ->
      let* rows = constraint_rows conn in
      Alcotest.(check (list (pair string bool)))
        "present and validated"
        [
          ("communities_network_description_check", true);
          ("communities_network_name_check", true);
          ("communities_network_slug_check", true);
        ]
        rows;
      Lwt.return_unit)

(* === down/up round-trip === *)

let roundtrip_case =
  db_case "identity: the down/up statement pair round-trips" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      (* The whole round-trip runs inside one transaction that is always
         rolled back, so the live constraints cannot be lost even if an
         assertion fails between the drop and the re-add. *)
      let* r = C.start () in
      let* () = or_fail "begin" r in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = Network_community_constraints.drop conn in
            let* rows = constraint_rows conn in
            Alcotest.(check int) "down removes all three" 0 (List.length rows);
            let* () = Network_community_constraints.restore conn in
            let* rows = constraint_rows conn in
            Alcotest.(check (list (pair string bool)))
              "up restores all three, validated"
              [
                ("communities_network_description_check", true);
                ("communities_network_name_check", true);
                ("communities_network_slug_check", true);
              ]
              rows;
            Lwt.return_unit)
          (fun () ->
            let* _ = C.rollback () in
            Lwt.return_unit)
      in
      let* rows = constraint_rows conn in
      Alcotest.(check int) "live constraints untouched" 3 (List.length rows);
      Lwt.return_unit)

let suite =
  [
    valid_case;
    slug_case;
    name_case;
    description_case;
    legacy_case;
    presence_case;
    roundtrip_case;
  ]

let suites =
  (* Scoped network-community identity constraints (migration
       20260726120000): canonical acceptance, the exact slug/name/
       description boundaries, legacy exemption, validated presence, and
       the transactional down/up round-trip. Database-gated. *)
  [ ("network_community_identity_schema", suite) ]
