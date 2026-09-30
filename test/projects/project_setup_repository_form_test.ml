module Psf = Earde.Project_setup_repository_form

(* ===== Project setup: repository-selection form parser + page renderer =====
   Both pure and DB-free. Parser rejection labels name only the malformed
   category — never the fixture value — and rejection is asserted as a bare
   boolean, so no form value can reach Alcotest output; the error type itself
   is payload-free by construction. *)

let psf_case name f = Alcotest.test_case name `Quick f

let psf_ok name fields ~draft ~repos =
  psf_case name (fun () ->
      match Psf.of_fields fields with
      | Ok t ->
          Alcotest.(check int64) "draft id" draft (Psf.draft_id t);
          Alcotest.(check (list int64))
            "snapshot ids" repos
            (Psf.selected_snapshot_ids t)
      | Error Psf.Invalid_form -> Alcotest.fail "expected a valid form")

let psf_reject name fields =
  psf_case name (fun () ->
      Alcotest.(check bool)
        "rejected" true
        (match Psf.of_fields fields with
        | Error Psf.Invalid_form -> true
        | Ok _ -> false))

let psf_repo_fields values = List.map (fun v -> ("repository", v)) values

let psf_fields_of_size n =
  ("draft_id", "1")
  :: psf_repo_fields (List.init n (fun i -> string_of_int (i + 1)))

let psf_cases =
  [
    psf_ok "draft id alone: empty selection is valid"
      [ ("draft_id", "7") ]
      ~draft:7L ~repos:[];
    psf_ok "one repository"
      [ ("draft_id", "7"); ("repository", "31") ]
      ~draft:7L ~repos:[ 31L ];
    psf_ok "several repositories preserve form order"
      (("draft_id", "8") :: psf_repo_fields [ "5"; "3"; "9"; "4" ])
      ~draft:8L ~repos:[ 5L; 3L; 9L; 4L ];
    psf_ok "draft id position between repositories does not matter"
      [ ("repository", "2"); ("draft_id", "7"); ("repository", "1") ]
      ~draft:7L ~repos:[ 2L; 1L ];
    psf_case "exactly 2000 repositories accepted, order intact" (fun () ->
        match Psf.of_fields (psf_fields_of_size 2000) with
        | Ok t ->
            let ids = Psf.selected_snapshot_ids t in
            Alcotest.(check int) "count" 2000 (List.length ids);
            Alcotest.(check int64) "first preserved" 1L (List.hd ids);
            Alcotest.(check int64) "last preserved" 2000L (List.nth ids 1999)
        | Error Psf.Invalid_form -> Alcotest.fail "expected a valid form");
    psf_ok "leading zeroes accepted when the value is positive"
      [ ("draft_id", "007"); ("repository", "0042") ]
      ~draft:7L ~repos:[ 42L ];
    psf_case "largest representable int64 accepted" (fun () ->
        let fields =
          [
            ("draft_id", Int64.to_string Int64.max_int);
            ("repository", Int64.to_string Int64.max_int);
          ]
        in
        match Psf.of_fields fields with
        | Ok t ->
            Alcotest.(check int64) "draft id" Int64.max_int (Psf.draft_id t);
            Alcotest.(check (list int64))
              "snapshot ids" [ Int64.max_int ]
              (Psf.selected_snapshot_ids t)
        | Error Psf.Invalid_form -> Alcotest.fail "expected a valid form")
    (* Draft id: exactly one, strict positive decimal int64. *);
    psf_reject "empty field set" [];
    psf_reject "missing draft id" (psf_repo_fields [ "1" ]);
    psf_reject "duplicate draft id field"
      [ ("draft_id", "1"); ("draft_id", "1") ];
    psf_reject "blank draft id" [ ("draft_id", "") ];
    psf_reject "zero draft id" [ ("draft_id", "0") ];
    psf_reject "all-zero draft id" [ ("draft_id", "000") ];
    psf_reject "negative draft id" [ ("draft_id", "-5") ];
    psf_reject "plus-signed draft id" [ ("draft_id", "+5") ];
    psf_reject "leading-whitespace draft id" [ ("draft_id", " 5") ];
    psf_reject "trailing-whitespace draft id" [ ("draft_id", "5 ") ];
    psf_reject "newline-suffixed draft id" [ ("draft_id", "5\n") ];
    psf_reject "decimal-point draft id" [ ("draft_id", "5.0") ];
    psf_reject "hexadecimal draft id" [ ("draft_id", "0x10") ];
    psf_reject "underscore-separated draft id" [ ("draft_id", "1_000") ];
    psf_reject "int64-overflow draft id" [ ("draft_id", "9223372036854775808") ]
    (* Repository ids: same strict grammar; one bad value rejects the whole
       form. *);
    psf_reject "blank repository id" [ ("draft_id", "1"); ("repository", "") ];
    psf_reject "zero repository id" [ ("draft_id", "1"); ("repository", "0") ];
    psf_reject "negative repository id"
      [ ("draft_id", "1"); ("repository", "-3") ];
    psf_reject "plus-signed repository id"
      [ ("draft_id", "1"); ("repository", "+3") ];
    psf_reject "leading-whitespace repository id"
      [ ("draft_id", "1"); ("repository", " 3") ];
    psf_reject "trailing-whitespace repository id"
      [ ("draft_id", "1"); ("repository", "3 ") ];
    psf_reject "decimal-point repository id"
      [ ("draft_id", "1"); ("repository", "3.0") ];
    psf_reject "hexadecimal repository id"
      [ ("draft_id", "1"); ("repository", "0x3") ];
    psf_reject "int64-overflow repository id"
      [ ("draft_id", "1"); ("repository", "9223372036854775808") ];
    psf_reject "one invalid among valid repositories rejects the whole form"
      (("draft_id", "1") :: psf_repo_fields [ "4"; "x"; "6" ]);
    psf_reject "duplicate repository ids"
      (("draft_id", "1") :: psf_repo_fields [ "4"; "4" ]);
    psf_reject "duplicate repository ids across leading-zero spellings"
      (("draft_id", "1") :: psf_repo_fields [ "7"; "007" ]);
    psf_reject "2001 repository fields" (psf_fields_of_size 2001)
    (* Closed field set: everything but draft_id/repository rejects,
       byte-exactly. *);
    psf_reject "unknown field" [ ("draft_id", "1"); ("primary", "2") ];
    psf_reject "unknown submit-style field"
      [ ("draft_id", "1"); ("submit", "save") ];
    psf_reject "capitalized field name" [ ("Draft_id", "1") ];
    psf_reject "field name with trailing space"
      [ ("draft_id", "1"); ("repository ", "2") ];
    psf_case "every rejection is the same payload-free error" (fun () ->
        let rejections =
          [
            Psf.of_fields [ ("draft_id", "0") ];
            Psf.of_fields [ ("draft_id", "1"); ("unknown", "field") ];
            Psf.of_fields (("draft_id", "1") :: psf_repo_fields [ "4"; "4" ]);
          ]
        in
        List.iter
          (fun r ->
            Alcotest.(check bool)
              "Invalid_form" true
              (r = Error Psf.Invalid_form))
          rejections);
  ]

let suites =
  (* Repository-selection form parser: closed grammar, strict positive
       decimal int64 ids, order preservation, payload-free rejection. *)
  [ ("project_setup_form", psf_cases) ]
