module Pi = Earde.Project_identity
module Pif = Earde.Project_identity_form

(* ===== Project identity form: strict pure parser =====
   DB-free. Only field grammar and scalar identifiers are asserted here —
   content rules stay with Project_identity, and the delegation cases prove
   the domain errors originate from create_identity, not from the parser.
   Rejection fixtures are distinctive and only ever asserted through
   booleans against the nullary Invalid_form (no public printer exists —
   a compile-time property of the mli), so no submitted byte can reach
   test output. *)

let pif_case name f = Alcotest.test_case name `Quick f

(* Structurally valid baseline; every case overrides only what it probes.
   The default kind is organization so a blank primary stays domain-valid
   in delegation cases. *)
let pif_fields ?(draft = "7") ?(kind = "organization") ?(name = "Example")
    ?(slug = "example") ?(description = "") ?(website = "") ?(primary = "") () =
  [
    ("draft_id", draft);
    ("kind", kind);
    ("name", name);
    ("slug", slug);
    ("description", description);
    ("website_url", website);
    ("primary_snapshot_id", primary);
  ]

let pif_expect_ok fields =
  match Pif.of_fields fields with
  | Ok t -> t
  | Error Pif.Invalid_form -> Alcotest.fail "expected a valid form"

let pif_ok ?draft ?kind ?name ?slug ?description ?website ?primary () =
  pif_expect_ok
    (pif_fields ?draft ?kind ?name ?slug ?description ?website ?primary ())

let pif_reject name fields =
  pif_case name (fun () ->
      Alcotest.(check bool)
        "rejected" true
        (match Pif.of_fields fields with
        | Error Pif.Invalid_form -> true
        | Ok _ -> false))

let pif_recognized_fields =
  [
    "draft_id";
    "kind";
    "name";
    "slug";
    "description";
    "website_url";
    "primary_snapshot_id";
  ]

let pif_valid_cases =
  [
    pif_case "every canonical kind parses to its variant" (fun () ->
        List.iter
          (fun (raw, expected) ->
            Alcotest.(check bool)
              ("kind " ^ raw) true
              (Pif.kind (pif_ok ~kind:raw ()) = expected))
          [
            ("project", Pi.Project);
            ("organization", Pi.Organization);
            ("ecosystem", Pi.Ecosystem);
            ("foundation", Pi.Foundation);
            ("working_group", Pi.Working_group);
            ("other", Pi.Other);
          ]);
    pif_case "text fields preserved byte-exactly, never canonicalized"
      (fun () ->
        let t =
          pif_ok ~name:"  Progetto Citt\xc3\xa0  " ~slug:"  OCaml-Platform  "
            ~description:"line one\r\nline two"
            ~website:" HTTPS://Example.com/caff\xc3\xa8 " ()
        in
        Alcotest.(check string)
          "name untouched" "  Progetto Citt\xc3\xa0  " (Pif.name t);
        Alcotest.(check string)
          "slug untouched" "  OCaml-Platform  " (Pif.slug t);
        Alcotest.(check string)
          "description untouched" "line one\r\nline two" (Pif.description t);
        Alcotest.(check string)
          "website untouched" " HTTPS://Example.com/caff\xc3\xa8 "
          (Pif.website_url t));
    pif_case "empty description and website are retained as empty strings"
      (fun () ->
        let t = pif_ok ~description:"" ~website:"" () in
        Alcotest.(check string) "description" "" (Pif.description t);
        Alcotest.(check string) "website" "" (Pif.website_url t));
    pif_case "blank primary parses to None" (fun () ->
        Alcotest.(check (option int64))
          "primary" None
          (Pif.primary_snapshot_id (pif_ok ~primary:"" ())));
    pif_case "positive primary parses exactly" (fun () ->
        Alcotest.(check (option int64))
          "primary" (Some 31L)
          (Pif.primary_snapshot_id (pif_ok ~primary:"31" ())));
    pif_case "leading zeroes accepted for draft and primary ids" (fun () ->
        let t = pif_ok ~draft:"007" ~primary:"0042" () in
        Alcotest.(check int64) "draft id" 7L (Pif.draft_id t);
        Alcotest.(check (option int64))
          "primary" (Some 42L)
          (Pif.primary_snapshot_id t));
    pif_case "largest representable int64 accepted for both ids" (fun () ->
        let big = Int64.to_string Int64.max_int in
        let t = pif_ok ~draft:big ~primary:big () in
        Alcotest.(check int64) "draft id" Int64.max_int (Pif.draft_id t);
        Alcotest.(check (option int64))
          "primary" (Some Int64.max_int)
          (Pif.primary_snapshot_id t));
    pif_case "field order does not matter" (fun () ->
        let t = pif_expect_ok (List.rev (pif_fields ~primary:"5" ())) in
        Alcotest.(check int64) "draft id" 7L (Pif.draft_id t);
        Alcotest.(check (option int64))
          "primary" (Some 5L)
          (Pif.primary_snapshot_id t));
  ]

let pif_missing_cases =
  List.map
    (fun field ->
      pif_reject ("missing " ^ field)
        (List.filter (fun (k, _) -> k <> field) (pif_fields ())))
    pif_recognized_fields

let pif_duplicate_cases =
  List.map
    (fun field ->
      pif_reject ("duplicate " ^ field)
        (let fields = pif_fields ~primary:"5" () in
         fields @ List.filter (fun (k, _) -> k = field) fields))
    pif_recognized_fields

(* Renames one recognized field, producing an unknown name (and a missing
   recognized one) in a single otherwise-valid submission. *)
let pif_rename field replacement =
  List.map
    (fun (k, v) -> if k = field then (replacement, v) else (k, v))
    (pif_fields ())

let pif_grammar_cases =
  pif_missing_cases @ pif_duplicate_cases
  @ [
      pif_reject "empty field set" [];
      pif_reject "unknown repository field"
        (pif_fields () @ [ ("repository", "1") ]);
      pif_reject "unknown submit-style field"
        (pif_fields () @ [ ("submit", "create") ]);
      pif_reject "dream.csrf is not recognized by the pure parser"
        (pif_fields () @ [ ("dream.csrf", "token") ]);
      pif_reject "capitalized draft_id name" (pif_rename "draft_id" "Draft_id");
      pif_reject "capitalized kind name" (pif_rename "kind" "Kind");
      pif_reject "uppercase name field" (pif_rename "name" "NAME");
      pif_reject "capitalized slug name" (pif_rename "slug" "Slug");
      pif_reject "capitalized description name"
        (pif_rename "description" "Description");
      pif_reject "capitalized website name"
        (pif_rename "website_url" "Website_url");
      pif_reject "capitalized primary name"
        (pif_rename "primary_snapshot_id" "Primary_snapshot_id");
      pif_reject "field name with trailing space" (pif_rename "name" "name ");
      pif_reject "field name with leading space" (pif_rename "slug" " slug");
    ]

let pif_draft_cases =
  List.map
    (fun (label, raw) ->
      pif_reject ("draft id: " ^ label) (pif_fields ~draft:raw ()))
    [
      ("blank", "");
      ("zero", "0");
      ("all zeroes", "000");
      ("negative", "-5");
      ("plus-signed", "+5");
      ("leading whitespace", " 5");
      ("trailing whitespace", "5 ");
      ("newline-suffixed", "5\n");
      ("decimal point", "5.0");
      ("hexadecimal", "0x10");
      ("underscore separator", "1_000");
      ("int64 overflow", "9223372036854775808");
    ]

let pif_kind_cases =
  List.map
    (fun (label, raw) ->
      pif_reject ("kind: " ^ label) (pif_fields ~kind:raw ()))
    [
      ("blank", "");
      ("capitalized", "Project");
      ("uppercase", "PROJECT");
      ("leading whitespace", " project");
      ("trailing whitespace", "project ");
      ("hyphenated working group", "working-group");
      ("collapsed working group", "workinggroup");
      ("alias org", "org");
      ("alias github_organization", "github_organization");
      ("alias initiative", "initiative");
      ("unknown value", "unknown");
    ]

let pif_primary_cases =
  List.map
    (fun (label, raw) ->
      pif_reject ("primary id: " ^ label) (pif_fields ~primary:raw ()))
    [
      ("zero", "0");
      ("all zeroes", "000");
      ("negative", "-3");
      ("plus-signed", "+3");
      ("leading whitespace", " 3");
      ("trailing whitespace", "3 ");
      ("decimal point", "3.0");
      ("hexadecimal", "0x3");
      ("underscore separator", "1_0");
      ("int64 overflow", "9223372036854775808");
    ]

(* Exact propagation: structurally valid forms whose content the domain
   rejects. The parser accepted every one of these submissions, so the
   errors demonstrably come from Project_identity.create. *)
let pif_domain_error name expected result =
  pif_case name (fun () ->
      Alcotest.(check bool)
        "exact domain error" true
        (match result with Error e -> e = expected | Ok _ -> false))

let pif_delegation_cases =
  [
    pif_case "create_identity canonicalizes through the real domain" (fun () ->
        let t =
          pif_ok ~kind:"project" ~name:"  OCaml Platform  " ~slug:" OCaml "
            ~description:"  " ~website:"  " ~primary:"20" ()
        in
        match Pif.create_identity t ~selected_snapshot_ids:[ 10L; 20L ] with
        | Ok identity ->
            Alcotest.(check bool)
              "kind survives" true
              (Pi.kind identity = Pi.Project);
            Alcotest.(check string)
              "canonical name" "OCaml Platform" (Pi.name identity);
            Alcotest.(check string) "canonical slug" "ocaml" (Pi.slug identity);
            Alcotest.(check (option string))
              "empty description collapses to None" None
              (Pi.description identity);
            Alcotest.(check (option string))
              "empty website collapses to None" None (Pi.website_url identity);
            Alcotest.(check (option int64))
              "primary" (Some 20L)
              (Pi.primary_snapshot_id identity)
        | Error _ -> Alcotest.fail "expected a valid identity");
    pif_case "selected context is call context, never parser state" (fun () ->
        (* One parsed value, two verdicts: membership of the same primary
           depends only on the supplied authoritative list, so no selection
           can have been retained in t. *)
        let t = pif_ok ~primary:"20" () in
        (match Pif.create_identity t ~selected_snapshot_ids:[ 20L ] with
        | Ok _ -> ()
        | Error _ -> Alcotest.fail "expected a valid identity");
        Alcotest.(check bool)
          "different context rejects the same t" true
          (match Pif.create_identity t ~selected_snapshot_ids:[ 21L ] with
          | Error Pi.Invalid_primary_repository -> true
          | Ok _ | Error _ -> false));
    pif_domain_error "invalid selected context propagates"
      Pi.Invalid_repository_selection
      (Pif.create_identity (pif_ok ()) ~selected_snapshot_ids:[ 0L ]);
    pif_domain_error "empty selected context propagates"
      Pi.No_repositories_selected
      (Pif.create_identity (pif_ok ()) ~selected_snapshot_ids:[]);
    pif_domain_error "invalid name propagates" Pi.Invalid_name
      (Pif.create_identity (pif_ok ~name:"   " ()) ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "invalid slug propagates" Pi.Invalid_slug
      (Pif.create_identity
         (pif_ok ~slug:"bad slug" ())
         ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "reserved slug propagates" Pi.Reserved_slug
      (Pif.create_identity (pif_ok ~slug:"new" ()) ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "invalid description propagates" Pi.Invalid_description
      (Pif.create_identity
         (pif_ok ~description:(String.make 2001 'a') ())
         ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "invalid website propagates" Pi.Invalid_website_url
      (Pif.create_identity
         (pif_ok ~website:"example.com" ())
         ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "non-member primary propagates"
      Pi.Invalid_primary_repository
      (Pif.create_identity (pif_ok ~primary:"31" ())
         ~selected_snapshot_ids:[ 1L ]);
    pif_domain_error "missing project primary propagates"
      Pi.Primary_repository_required
      (Pif.create_identity
         (pif_ok ~kind:"project" ~primary:"" ())
         ~selected_snapshot_ids:[ 1L ]);
  ]

let pif_privacy_cases =
  [
    pif_case "every rejection is the same payload-free error" (fun () ->
        (* Distinctive fixture markers: were Invalid_form to carry any
           payload, these equalities could not all hold. *)
        let rejections =
          [
            Pif.of_fields (pif_fields ~draft:"pif-fixture-draft-zz1" ());
            Pif.of_fields (pif_fields ~kind:"pif-fixture-kind-zz2" ());
            Pif.of_fields (pif_fields ~primary:"pif-fixture-primary-zz3" ());
            Pif.of_fields (pif_fields () @ [ ("pif-fixture-field-zz4", "zz5") ]);
            Pif.of_fields
              (List.filter (fun (k, _) -> k <> "name") (pif_fields ()));
            Pif.of_fields [];
          ]
        in
        List.iter
          (fun r ->
            Alcotest.(check bool)
              "Invalid_form" true
              (r = Error Pif.Invalid_form))
          rejections);
  ]

let suites =
  (* Project-identity form parser: closed seven-field grammar, strict
       scalar identifiers, byte-exact text passthrough, exact domain
       delegation through Project_identity.create, payload-free
       rejection. *)
  [
    ("project_identity_form_valid", pif_valid_cases);
    ("project_identity_form_grammar", pif_grammar_cases);
    ("project_identity_form_draft", pif_draft_cases);
    ("project_identity_form_kind", pif_kind_cases);
    ("project_identity_form_primary", pif_primary_cases);
    ("project_identity_form_delegation", pif_delegation_cases);
    ("project_identity_form_privacy", pif_privacy_cases);
  ]
