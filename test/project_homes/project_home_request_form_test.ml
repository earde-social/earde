module Phr = Earde.Project_home_relation
module Phrf = Earde.Project_home_request_form

(* ===== Existing-community home request form (Project_home_request_form) =====
   Pure strict parser: exact two-field grammar, strict positive OCaml-int
   community id, byte-exact note passthrough, and delegation to the real
   relation domain. DB-free. Rejection labels name only the malformed
   category, and every rejection is asserted against the nullary
   constructor, so no fixture value reaches test output. *)

let phrf_case name f = Alcotest.test_case name `Quick f

let phrf_fields ?(target = "42") ?(note = "Please make this our home") () =
  [ ("target_community_id", target); ("request_note", note) ]

let phrf_expect_ok fields =
  match Phrf.of_fields fields with
  | Ok t -> t
  | Error Phrf.Invalid_form -> Alcotest.fail "expected a valid form"

let phrf_ok ?target ?note () = phrf_expect_ok (phrf_fields ?target ?note ())

let phrf_reject name fields =
  phrf_case name (fun () ->
      Alcotest.(check bool) "rejected" true
        (match Phrf.of_fields fields with
        | Error Phrf.Invalid_form -> true
        | Ok _ -> false))

let phrf_rename field replacement =
  List.map
    (fun (k, v) -> if k = field then (replacement, v) else (k, v))
    (phrf_fields ())

let phrf_valid_cases =
  [ phrf_case "ordinary submission parses exactly" (fun () ->
        let t = phrf_ok () in
        Alcotest.(check int) "target id" 42 (Phrf.target_community_id t);
        Alcotest.(check string) "note" "Please make this our home"
          (Phrf.request_note t))
  ; phrf_case "field order does not matter" (fun () ->
        let t = phrf_expect_ok (List.rev (phrf_fields ())) in
        Alcotest.(check int) "target id" 42 (Phrf.target_community_id t))
  ; phrf_case "empty note is preserved as the empty string" (fun () ->
        Alcotest.(check string) "empty note" ""
          (Phrf.request_note (phrf_ok ~note:"" ())))
  ; phrf_case "blank note is preserved verbatim before domain creation"
      (fun () ->
        Alcotest.(check string) "blank note" " \t\r\n "
          (Phrf.request_note (phrf_ok ~note:" \t\r\n " ())))
  ; phrf_case "note preserved byte-exactly, never canonicalized" (fun () ->
        let raw = "  Prima riga \xc3\xa8\r\nseconda\ttab \xe2\x98\x95  " in
        Alcotest.(check string) "note untouched" raw
          (Phrf.request_note (phrf_ok ~note:raw ())))
  ; phrf_case "leading zeroes accepted for the community id" (fun () ->
        Alcotest.(check int) "target id" 7
          (Phrf.target_community_id (phrf_ok ~target:"007" ())))
  ; phrf_case "largest representable int accepted" (fun () ->
        Alcotest.(check int) "target id" max_int
          (Phrf.target_community_id
             (phrf_ok ~target:(string_of_int max_int) ())))
  ]

let phrf_grammar_cases =
  [ phrf_reject "empty field set" []
  ; phrf_reject "missing target_community_id"
      [ ("request_note", "only a note") ]
  ; phrf_reject "missing request_note" [ ("target_community_id", "42") ]
  ; phrf_reject "duplicate target_community_id"
      (("target_community_id", "42") :: phrf_fields ())
  ; phrf_reject "duplicate request_note"
      (("request_note", "again") :: phrf_fields ())
  ; phrf_reject "unknown extra field"
      (phrf_fields () @ [ ("community_slug", "alpine") ])
  ; phrf_reject "unknown submit-style field"
      (phrf_fields () @ [ ("submit", "send") ])
  ; phrf_reject "project slug is never a form field"
      (phrf_fields () @ [ ("project_slug", "widget-kit") ])
  ; phrf_reject "dream.csrf is not recognized by the pure parser"
      (phrf_fields () @ [ ("dream.csrf", "token") ])
  ; phrf_reject "capitalized target name"
      (phrf_rename "target_community_id" "Target_community_id")
  ; phrf_reject "uppercase note name"
      (phrf_rename "request_note" "REQUEST_NOTE")
  ; phrf_reject "target name with leading space"
      (phrf_rename "target_community_id" " target_community_id")
  ; phrf_reject "note name with trailing space"
      (phrf_rename "request_note" "request_note ")
  ]

let phrf_id_cases =
  List.map
    (fun (label, raw) ->
      phrf_reject ("community id: " ^ label) (phrf_fields ~target:raw ()))
    [ ("blank", "")
    ; ("zero", "0")
    ; ("all zeroes", "000")
    ; ("negative", "-5")
    ; ("plus-signed", "+5")
    ; ("leading whitespace", " 5")
    ; ("trailing whitespace", "5 ")
    ; ("newline-suffixed", "5\n")
    ; ("decimal point", "5.0")
    ; ("hexadecimal", "0x10")
    ; ("underscore separator", "1_000")
    ; ("int64 overflow", "9223372036854775808")
    ; ("ocaml int overflow", string_of_int max_int ^ "0")
    ]

let phrf_delegation_cases =
  [ phrf_case "create_relation canonicalizes through the real domain"
      (fun () ->
        match
          Phrf.create_relation (phrf_ok ~note:"  ciao\r\nmondo\t \r\n" ())
        with
        | Ok relation ->
            Alcotest.(check bool) "pending" true
              (Phr.status relation = Phr.Pending);
            Alcotest.(check (option string)) "canonical note"
              (Some "ciao\nmondo")
              (Phr.request_note relation)
        | Error _ -> Alcotest.fail "expected a valid relation")
  ; phrf_case "blank note collapses to None only in the domain" (fun () ->
        let t = phrf_ok ~note:" \t\r\n " () in
        Alcotest.(check string) "parser preserves" " \t\r\n "
          (Phrf.request_note t);
        match Phrf.create_relation t with
        | Ok relation ->
            Alcotest.(check (option string)) "domain collapses" None
              (Phr.request_note relation)
        | Error _ -> Alcotest.fail "expected a valid relation")
  ; phrf_case "invalid notes propagate the exact domain error" (fun () ->
        List.iter
          (fun raw ->
            Alcotest.(check bool) "Invalid_request_note" true
              (match Phrf.create_relation (phrf_ok ~note:raw ()) with
              | Error Phr.Invalid_request_note -> true
              | Ok _ | Error _ -> false))
          [ "nul\x00byte"; "esc\x1bcontrol"; "\xff\xfe not utf-8";
            String.make 2001 'a' ])
  ]

let phrf_privacy_cases =
  [ phrf_case "every rejection is the same payload-free error" (fun () ->
        (* Distinctive fixture markers: were Invalid_form to carry any
           payload, these equalities could not all hold. *)
        let rejections =
          [ Phrf.of_fields []
          ; Phrf.of_fields (phrf_fields ~target:"phrf-fixture-zz1" ())
          ; Phrf.of_fields (phrf_fields () @ [ ("phrf-fixture-zz2", "z") ])
          ; Phrf.of_fields [ ("request_note", "phrf-fixture-zz3") ]
          ; Phrf.of_fields (phrf_fields () @ phrf_fields ())
          ]
        in
        List.iter
          (fun r ->
            Alcotest.(check bool) "Invalid_form" true
              (r = Error Phrf.Invalid_form))
          rejections)
  ]

let suites =
    (* Existing-community home request form: pure strict two-field parser
       (target_community_id + request_note) delegating every note rule to
       the relation domain. *)
  [ ("project_home_request_form_valid", phrf_valid_cases)
  ; ("project_home_request_form_grammar", phrf_grammar_cases)
  ; ("project_home_request_form_id", phrf_id_cases)
  ; ("project_home_request_form_delegation", phrf_delegation_cases)
  ; ("project_home_request_form_privacy", phrf_privacy_cases)
  ]
