module Phr = Earde.Project_home_relation

(* ===== Project home relation: pure lifecycle domain =====
   DB-free. Closed status vocabulary, exact database conversion, canonical
   optional request note, explicit pending/provisioned constructors, and the
   closed MVP transition matrix. Rejection fixtures are distinctive and only
   ever asserted through booleans against the nullary errors (no public
   printer exists — a compile-time property of the mli), so no submitted
   byte can reach test output. The absence of ID, timestamp, and mutation
   accessors is likewise a compile-time property of the mli, not a runtime
   assertion. *)

let phr_case name f = Alcotest.test_case name `Quick f

let phr_status_string t = Phr.string_of_status (Phr.status t)

let phr_note_ok label ~raw ~expect =
  phr_case label (fun () ->
      Alcotest.(check (option string)) "canonical note" expect
        (Phr.request_note (Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ~note:raw ()))))

let phr_note_reject label raw =
  phr_case label (fun () ->
      Alcotest.(check bool) "rejected as invalid note" true
        (match Home_request_fixture.phr_pending ~note:raw () with
        | Error Phr.Invalid_request_note -> true
        | Ok _ | Error Phr.Invalid_transition -> false))

let phr_all_statuses = [ Phr.Pending; Phr.Accepted; Phr.Rejected; Phr.Removed ]

let phr_status_cases =
  [ phr_case "every canonical string parses and serializes back" (fun () ->
        List.iter
          (fun (s, st) ->
            (match Phr.status_of_string s with
            | Some parsed ->
                Alcotest.(check bool) "parses to the right variant" true
                  (parsed = st)
            | None -> Alcotest.fail "canonical status string did not parse");
            Alcotest.(check string) "serializes to the database value" s
              (Phr.string_of_status st))
          [ ("pending", Phr.Pending)
          ; ("accepted", Phr.Accepted)
          ; ("rejected", Phr.Rejected)
          ; ("removed", Phr.Removed)
          ])
  ; phr_case "every status round-trips" (fun () ->
        List.iter
          (fun st ->
            Alcotest.(check bool) "round trip" true
              (Phr.status_of_string (Phr.string_of_status st) = Some st))
          phr_all_statuses)
  ; phr_case "capitalization, padding, and unknown variants reject" (fun () ->
        List.iter
          (fun s ->
            Alcotest.(check bool) "rejected" true
              (Phr.status_of_string s = None))
          [ "Pending"
          ; "PENDING"
          ; "Accepted"
          ; " pending"
          ; "pending "
          ; "accepted\n"
          ; "\trejected"
          ; "reject"
          ; "rej"
          ; "approved"
          ; "cancelled"
          ; "active"
          ; "home"
          ; ""
          ; "unknown"
          ])
  ]

let phr_pending_cases =
  [ phr_case "absent note stays absent" (fun () ->
        Alcotest.(check (option string)) "no note" None
          (Phr.request_note (Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ()))))
  ; phr_note_ok "empty note collapses to none" ~raw:"" ~expect:None
  ; phr_note_ok "ascii-whitespace-only note collapses to none"
      ~raw:" \t\r\n\x0c\x0b " ~expect:None
  ; phr_note_ok "ordinary note accepted"
      ~raw:"We already discuss releases in this community."
      ~expect:(Some "We already discuss releases in this community.")
  ; phr_note_ok "outer ascii whitespace trims" ~raw:"  needs moderator eyes \t "
      ~expect:(Some "needs moderator eyes")
  ; phr_note_ok "internal ordinary spaces preserved"
      ~raw:"two  spaces   survive" ~expect:(Some "two  spaces   survive")
  ; phr_note_ok "crlf becomes lf" ~raw:"line one\r\nline two"
      ~expect:(Some "line one\nline two")
  ; phr_note_ok "lone cr becomes lf" ~raw:"line one\rline two"
      ~expect:(Some "line one\nline two")
  ; phr_note_ok "multiline utf-8 note accepted"
      ~raw:"Progetto Citt\xc3\xa0\nseconda riga \xe2\x98\x95"
      ~expect:(Some "Progetto Citt\xc3\xa0\nseconda riga \xe2\x98\x95")
  ; phr_note_ok "internal horizontal tab accepted" ~raw:"col a\tcol b"
      ~expect:(Some "col a\tcol b")
  ; phr_case "pending constructor yields status pending" (fun () ->
        Alcotest.(check string) "status" "pending"
          (phr_status_string (Home_request_fixture.phr_fresh_pending ())))
  ]

let phr_note_boundary_cases =
  [ phr_case "exactly 2000 ascii scalars accepted without truncation"
      (fun () ->
        let note = String.make 2000 'a' in
        Alcotest.(check (option string)) "byte-exact note" (Some note)
          (Phr.request_note (Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ~note ()))))
  ; phr_note_reject "2001 ascii scalars reject rather than truncate"
      (String.make 2001 'a')
  ; phr_case "exactly 2000 two-byte scalars accepted, counted per scalar"
      (fun () ->
        (* 2000 two-byte scalars: byte counting would see 4000 and reject. *)
        let note = String.concat "" (List.init 2000 (fun _ -> "\xc3\xa9")) in
        Alcotest.(check (option string)) "byte-exact note" (Some note)
          (Phr.request_note (Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ~note ()))))
  ; phr_note_reject "2001 two-byte scalars reject"
      (String.concat "" (List.init 2001 (fun _ -> "\xc3\xa9")))
  ; phr_note_reject "malformed utf-8: stray continuation" "a\xffb"
  ; phr_note_reject "malformed utf-8: truncated sequence" "a\xc3"
  ; phr_note_reject "malformed utf-8: overlong encoding" "a\xc0\xafb"
  ; phr_note_reject "malformed utf-8: utf-16 surrogate" "a\xed\xa0\x80b"
  ; phr_note_reject "malformed utf-8: above U+10FFFF" "a\xf4\x90\x80\x80b"
  ; phr_note_reject "embedded nul" "a\x00b"
  ; phr_note_reject "control byte 0x01" "a\x01b"
  ; phr_note_reject "internal vertical tab" "a\x0bb"
  ; phr_note_reject "internal form feed" "a\x0cb"
  ; phr_note_reject "escape byte" "a\x1bb"
  ; phr_note_reject "del byte" "a\x7fb"
  ]

let phr_provisioned_cases =
  [ phr_case "provisioned home is accepted with no note" (fun () ->
        (* The constructor returns the accepted value directly — no pending
           intermediate state ever exists or is observable. *)
        let t = Phr.create_provisioned_home () in
        Alcotest.(check string) "status" "accepted" (phr_status_string t);
        Alcotest.(check (option string)) "note" None (Phr.request_note t))
  ]

let phr_valid_transition_cases =
  [ phr_case "pending + accept becomes accepted" (fun () ->
        Alcotest.(check string) "status" "accepted"
          (phr_status_string
             (Home_request_fixture.phr_expect_ok (Phr.apply (Home_request_fixture.phr_fresh_pending ()) Phr.Accept))))
  ; phr_case "pending + reject becomes rejected" (fun () ->
        Alcotest.(check string) "status" "rejected"
          (phr_status_string
             (Home_request_fixture.phr_expect_ok (Phr.apply (Home_request_fixture.phr_fresh_pending ()) Phr.Reject))))
  ; phr_case "accepted + remove becomes removed" (fun () ->
        Alcotest.(check string) "status" "removed"
          (phr_status_string
             (Home_request_fixture.phr_expect_ok (Phr.apply (Home_request_fixture.phr_fresh_accepted ()) Phr.Remove))))
  ]

let phr_invalid label make_t action =
  phr_case label (fun () ->
      Alcotest.(check bool) "invalid transition" true
        (match Phr.apply (make_t ()) action with
        | Error Phr.Invalid_transition -> true
        | Ok _ | Error Phr.Invalid_request_note -> false))

let phr_invalid_transition_cases =
  [ phr_invalid "pending + remove" Home_request_fixture.phr_fresh_pending Phr.Remove
  ; phr_invalid "accepted + accept" Home_request_fixture.phr_fresh_accepted Phr.Accept
  ; phr_invalid "accepted + reject" Home_request_fixture.phr_fresh_accepted Phr.Reject
  ; phr_invalid "rejected + accept" Home_request_fixture.phr_fresh_rejected Phr.Accept
  ; phr_invalid "rejected + reject" Home_request_fixture.phr_fresh_rejected Phr.Reject
  ; phr_invalid "rejected + remove" Home_request_fixture.phr_fresh_rejected Phr.Remove
  ; phr_invalid "removed + accept" Home_request_fixture.phr_fresh_removed Phr.Accept
  ; phr_invalid "removed + reject" Home_request_fixture.phr_fresh_removed Phr.Reject
  ; phr_invalid "removed + remove" Home_request_fixture.phr_fresh_removed Phr.Remove
  ]

let phr_preservation_cases =
  [ phr_case "note is byte-exact through pending, accepted, removed" (fun () ->
        let note =
          "First line\nseconda riga \xc3\xa9\n\tindented tail"
        in
        let pending = Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ~note ()) in
        Alcotest.(check (option string)) "pending note" (Some note)
          (Phr.request_note pending);
        let accepted = Home_request_fixture.phr_expect_ok (Phr.apply pending Phr.Accept) in
        Alcotest.(check (option string)) "accepted note" (Some note)
          (Phr.request_note accepted);
        let removed = Home_request_fixture.phr_expect_ok (Phr.apply accepted Phr.Remove) in
        Alcotest.(check (option string)) "removed note" (Some note)
          (Phr.request_note removed))
  ; phr_case "note is byte-exact through pending, rejected" (fun () ->
        let note = "Historical context\nfor the moderators" in
        let pending = Home_request_fixture.phr_expect_ok (Home_request_fixture.phr_pending ~note ()) in
        let rejected = Home_request_fixture.phr_expect_ok (Phr.apply pending Phr.Reject) in
        Alcotest.(check (option string)) "rejected note" (Some note)
          (Phr.request_note rejected))
  ; phr_case "provisioned home stays note-free after removal" (fun () ->
        let removed =
          Home_request_fixture.phr_expect_ok (Phr.apply (Phr.create_provisioned_home ()) Phr.Remove)
        in
        Alcotest.(check (option string)) "note" None (Phr.request_note removed))
  ]

(* Terminal history is never reopened: a later attempt is a brand-new
   community_projects row represented by a brand-new create_pending value.
   No relation IDs exist in the domain, so "new" means a fresh value. *)
let phr_fresh_row_cases =
  [ phr_case "rejected value cannot be reopened; a new pending value can"
      (fun () ->
        let rejected = Home_request_fixture.phr_fresh_rejected () in
        List.iter
          (fun action ->
            Alcotest.(check bool) "terminal" true
              (Phr.apply rejected action = Error Phr.Invalid_transition))
          [ Phr.Accept; Phr.Reject; Phr.Remove ];
        Alcotest.(check string) "separate new request" "pending"
          (phr_status_string (Home_request_fixture.phr_fresh_pending ())))
  ; phr_case "removed value cannot be reopened; a new pending value can"
      (fun () ->
        let removed = Home_request_fixture.phr_fresh_removed () in
        List.iter
          (fun action ->
            Alcotest.(check bool) "terminal" true
              (Phr.apply removed action = Error Phr.Invalid_transition))
          [ Phr.Accept; Phr.Reject; Phr.Remove ];
        Alcotest.(check string) "separate new request" "pending"
          (phr_status_string (Home_request_fixture.phr_fresh_pending ())))
  ]

let phr_ordering_cases =
  [ phr_case "invalid note fails before any pending value exists" (fun () ->
        Alcotest.(check bool) "note rejection from the constructor" true
          (Home_request_fixture.phr_pending ~note:"phr-fixture-\x01-bad" ()
          = Error Phr.Invalid_request_note))
  ; phr_case "a valid value reports only invalid transition afterwards"
      (fun () ->
        Alcotest.(check bool) "transition rejection from apply" true
          (Phr.apply (Home_request_fixture.phr_fresh_accepted ()) Phr.Accept
          = Error Phr.Invalid_transition))
  ]

let phr_privacy_cases =
  [ phr_case "every rejection is a payload-free constructor" (fun () ->
        let all = [ Phr.Invalid_request_note; Phr.Invalid_transition ] in
        (* Distinctive fixture values: were any error to carry its input, the
           closed-membership check below could not hold for all of them. No
           public printer or serializer exists (compile-time mli property),
           so no rejected byte can reach output through this module. *)
        let rejections =
          [ Home_request_fixture.phr_pending ~note:"phr-fixture-note-\x00-secret" ()
          ; Home_request_fixture.phr_pending ~note:"phr-fixture-note-\x7f-secret" ()
          ; Home_request_fixture.phr_pending ~note:("phr-" ^ String.make 2100 'z') ()
          ; Phr.apply (Home_request_fixture.phr_fresh_rejected ()) Phr.Accept
          ]
        in
        List.iter
          (fun outcome ->
            match outcome with
            | Ok _ -> Alcotest.fail "expected a rejection"
            | Error e ->
                Alcotest.(check bool) "nullary error" true (List.mem e all))
          rejections)
  ]

let suites =
    (* Project home relation: pure lifecycle domain shared by existing-
       community home requests and provisioned dedicated-community homes.
       Closed status conversion, canonical optional note, explicit
       constructors, closed transition matrix, payload-free errors. *)
  [ ("project_home_relation_status", phr_status_cases)
  ; ("project_home_relation_pending", phr_pending_cases)
  ; ("project_home_relation_note_bounds", phr_note_boundary_cases)
  ; ("project_home_relation_provisioned", phr_provisioned_cases)
  ; ("project_home_relation_transitions", phr_valid_transition_cases)
  ; ("project_home_relation_invalid", phr_invalid_transition_cases)
  ; ("project_home_relation_note_history", phr_preservation_cases)
  ; ("project_home_relation_fresh_rows", phr_fresh_row_cases)
  ; ("project_home_relation_ordering", phr_ordering_cases)
  ; ("project_home_relation_privacy", phr_privacy_cases)
  ]
