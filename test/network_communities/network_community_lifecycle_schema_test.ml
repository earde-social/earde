(* === Scoped network-community lifecycle constraint (migration
   20260726130000) ===
   The durable whole-state defense behind Network_communities.
   lifecycle_state_valid: the exact draft and two published network tuples
   are accepted, every other network combination (public/leaking draft,
   private published, both mixed publication-flag shapes) is rejected at
   the database, legacy rows stay exempt, the constraint is present and
   validated, and the down/up statement pair round-trips. Database-gated
   (EARDE_TEST_DATABASE_URL, same opt-in as Mod_scope); nclc-% community
   slugs so no suite shares fixtures. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let or_fail = Db_fixture.or_fail
let reject = Db_fixture.reject

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [ "DELETE FROM communities WHERE slug LIKE 'nclc%'" ]

(* Full lifecycle control on one row; identity stays canonical so only
   the lifecycle constraint can decide the outcome. The network marker is
   a parameter so the same shapes prove the legacy exemption. *)
let q_insert =
  (Caqti_type.(t2 (t3 string string bool) (t3 string string (t2 bool bool)))
  ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, is_network_community, \
     onboarding_state, visibility, indexable, discoverable) VALUES ($1, $2, \
     $3, $4, $5, $6, $7) RETURNING id"

let q_constraint =
  (Caqti_type.unit ->* Caqti_type.(t2 string bool))
    "SELECT conname, convalidated FROM pg_constraint WHERE conrelid = \
     'communities'::regclass AND contype = 'c' AND conname = \
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

let insert conn ?(network = true) ~onboarding ~visibility ~indexable
    ~discoverable slug =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  C.find q_insert
    ( (slug, "Nclc Community", network),
      (onboarding, visibility, (indexable, discoverable)) )

let accept label conn ?network ~onboarding ~visibility ~indexable ~discoverable
    slug =
  let* id =
    insert conn ?network ~onboarding ~visibility ~indexable ~discoverable slug
  in
  let* id = or_fail label id in
  Alcotest.(check bool) (label ^ ": positive id") true (id > 0);
  Lwt.return_unit

let refuse label conn ?network ~onboarding ~visibility ~indexable ~discoverable
    slug =
  let* r =
    insert conn ?network ~onboarding ~visibility ~indexable ~discoverable slug
  in
  reject label r

let constraint_rows conn =
  let (module C : Caqti_lwt.CONNECTION) = conn in
  let* rows = C.collect_list q_constraint () in
  or_fail "constraint rows" rows

(* === the three durable network shapes === *)

let accepted_case =
  db_case
    "lifecycle: exactly the draft, published Public, and published Unlisted \
     network tuples are accepted" (fun conn ->
      let* () =
        accept "private draft" conn ~onboarding:"draft" ~visibility:"private"
          ~indexable:false ~discoverable:false "nclc-draft"
      in
      let* () =
        accept "published Public" conn ~onboarding:"published"
          ~visibility:"public" ~indexable:true ~discoverable:true "nclc-public"
      in
      accept "published Unlisted" conn ~onboarding:"published"
        ~visibility:"public" ~indexable:false ~discoverable:false
        "nclc-unlisted")

(* === every other network tuple === *)

let rejected_case =
  db_case
    "lifecycle: leaking drafts, private published, and mixed publication flags \
     are rejected at the database" (fun conn ->
      Lwt_list.iter_s
        (fun (label, onboarding, visibility, indexable, discoverable) ->
          refuse label conn ~onboarding ~visibility ~indexable ~discoverable
            "nclc-reject")
        [
          ("public draft", "draft", "public", false, false);
          ("indexable draft", "draft", "private", true, false);
          ("discoverable draft", "draft", "private", false, true);
          ("leaking public draft", "draft", "public", true, true);
          ("private published", "published", "private", false, false);
          ("private listed published", "published", "private", true, true);
          ("published indexable only", "published", "public", true, false);
          ("published discoverable only", "published", "public", false, true);
        ])

(* === legacy exemption === *)

let legacy_case =
  db_case
    "lifecycle: legacy rows stay accepted in every combination the network \
     constraint rejects" (fun conn ->
      Lwt_list.iter_s
        (fun (label, slug, onboarding, visibility, indexable, discoverable) ->
          accept label conn ~network:false ~onboarding ~visibility ~indexable
            ~discoverable slug)
        [
          ("legacy public draft", "nclc-l1", "draft", "public", false, false);
          ( "legacy private published",
            "nclc-l2",
            "published",
            "private",
            false,
            false );
          ( "legacy mixed indexable",
            "nclc-l3",
            "published",
            "public",
            true,
            false );
          ( "legacy mixed discoverable",
            "nclc-l4",
            "published",
            "public",
            false,
            true );
          ("legacy leaking draft", "nclc-l5", "draft", "private", true, true);
        ])

(* === presence and validation === *)

let presence_case =
  db_case "lifecycle: the constraint is present and validated" (fun conn ->
      let* rows = constraint_rows conn in
      Alcotest.(check (list (pair string bool)))
        "present and validated"
        [ ("communities_network_lifecycle_check", true) ]
        rows;
      Lwt.return_unit)

(* === down/up round-trip === *)

let roundtrip_case =
  db_case "lifecycle: the down/up statement pair round-trips" (fun conn ->
      let (module C : Caqti_lwt.CONNECTION) = conn in
      (* The whole round-trip runs inside one transaction that is always
         rolled back, so the live constraint cannot be lost even if an
         assertion fails between the drop and the re-add. *)
      let* r = C.start () in
      let* () = or_fail "begin" r in
      let* () =
        Lwt.finalize
          (fun () ->
            let* () = Network_community_lifecycle_constraint.drop conn in
            let* rows = constraint_rows conn in
            Alcotest.(check int)
              "down removes the constraint" 0 (List.length rows);
            let* () = Network_community_lifecycle_constraint.restore conn in
            let* rows = constraint_rows conn in
            Alcotest.(check (list (pair string bool)))
              "up restores the constraint, validated"
              [ ("communities_network_lifecycle_check", true) ]
              rows;
            Lwt.return_unit)
          (fun () ->
            let* _ = C.rollback () in
            Lwt.return_unit)
      in
      let* rows = constraint_rows conn in
      Alcotest.(check int) "live constraint untouched" 1 (List.length rows);
      Lwt.return_unit)

let suite =
  [ accepted_case; rejected_case; legacy_case; presence_case; roundtrip_case ]

let suites =
  (* Scoped network-community lifecycle constraint (migration
       20260726130000): the exact draft and two published tuples accepted,
       every other network combination rejected, legacy exemption,
       validated presence, and the transactional down/up round-trip.
       Database-gated. *)
  [ ("network_community_lifecycle_schema", suite) ]
