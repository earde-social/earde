(* ===================== community connections (issue #30) =====================
   The mutual-connection storage/domain slice: the pure lifecycle domain, the
   two new tables' constraints, and the transactional store with its
   append-only audit trail. Pure cases first, then the database-gated ones. *)

(* The pure domain: closed status vocabulary, the three legal transitions and
   every illegal one, note canonicalization, the self-connection rule, and the
   symmetry helpers an accepted connection is read through. DB-free. *)

module Cc = Earde.Community_connections

let error_str : Cc.error -> string = function
  | Cc.Invalid_community_id -> "Invalid_community_id"
  | Cc.Self_connection -> "Self_connection"
  | Cc.Invalid_request_note -> "Invalid_request_note"
  | Cc.Invalid_transition -> "Invalid_transition"

let status_str = Cc.string_of_status

let make ?note ?(requester = 11) ?(recipient = 22) () =
  Cc.create_pending ~requester_community_id:requester
    ~recipient_community_id:recipient ~request_note:note

let ok label = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: unexpected %s" label (error_str e)

let pending ?note () = ok "pending fixture" (make ?note ())

let value_of_status status =
  let p = pending () in
  match status with
  | Cc.Pending -> p
  | Cc.Accepted -> ok "accept" (Cc.apply p Cc.Accept)
  | Cc.Rejected -> ok "reject" (Cc.apply p Cc.Reject)
  | Cc.Removed ->
      ok "remove" (Cc.apply (ok "accept" (Cc.apply p Cc.Accept)) Cc.Remove)

let all_statuses = [ Cc.Pending; Cc.Accepted; Cc.Rejected; Cc.Removed ]
let all_actions = [ Cc.Accept; Cc.Reject; Cc.Remove ]

let action_str = function
  | Cc.Accept -> "accept"
  | Cc.Reject -> "reject"
  | Cc.Remove -> "remove"

(* === status vocabulary === *)

let status_round_trip_case =
  Alcotest.test_case "status: exact database spellings round-trip" `Quick
    (fun () ->
      Alcotest.(check (list string))
        "serialized vocabulary"
        [ "pending"; "accepted"; "rejected"; "removed" ]
        (List.map status_str all_statuses);
      List.iter
        (fun s ->
          match Cc.status_of_string (status_str s) with
          | Some parsed ->
              Alcotest.(check string)
                ("round-trip " ^ status_str s)
                (status_str s) (status_str parsed)
          | None ->
              Alcotest.failf "round-trip %s: rejected its own spelling"
                (status_str s))
        all_statuses)

let status_drift_case =
  Alcotest.test_case "status: unknown, padded and case-drifted values reject"
    `Quick (fun () ->
      List.iter
        (fun raw ->
          match Cc.status_of_string raw with
          | None -> ()
          | Some s -> Alcotest.failf "%S accepted as %s" raw (status_str s))
        [
          "";
          " ";
          "Pending";
          "PENDING";
          "pending ";
          " pending";
          "\tpending";
          "pending\n";
          "pendinG";
          "Accepted";
          "ACCEPTED";
          " accepted ";
          "Rejected";
          "REJECTED";
          "rejected\r";
          "Removed";
          "REMOVED";
          "remove";
          "removing";
          "deleted";
          "cancelled";
          "expired";
          "active";
          "connected";
          "mutual";
          "approved";
          "declined";
          "null";
          "0";
          "1";
        ])

(* === transitions === *)

let legal_transitions_case =
  Alcotest.test_case "transitions: exactly the three legal ones succeed" `Quick
    (fun () ->
      let check label from action expected =
        match Cc.apply (value_of_status from) action with
        | Ok next ->
            Alcotest.(check string)
              label (status_str expected)
              (status_str (Cc.status next))
        | Error e -> Alcotest.failf "%s: unexpected %s" label (error_str e)
      in
      check "pending accepts" Cc.Pending Cc.Accept Cc.Accepted;
      check "pending rejects" Cc.Pending Cc.Reject Cc.Rejected;
      check "accepted removes" Cc.Accepted Cc.Remove Cc.Removed)

let illegal_transitions_case =
  Alcotest.test_case "transitions: every other status/action pair refuses"
    `Quick (fun () ->
      let legal = function
        | Cc.Pending, Cc.Accept | Cc.Pending, Cc.Reject | Cc.Accepted, Cc.Remove
          ->
            true
        | _ -> false
      in
      List.iter
        (fun from ->
          List.iter
            (fun action ->
              let label =
                Printf.sprintf "%s + %s" (status_str from) (action_str action)
              in
              match
                (Cc.apply (value_of_status from) action, legal (from, action))
              with
              | Ok _, true -> ()
              | Error Cc.Invalid_transition, false -> ()
              | Ok next, false ->
                  Alcotest.failf "%s: illegally reached %s" label
                    (status_str (Cc.status next))
              | Error e, true ->
                  Alcotest.failf "%s: legal pair refused with %s" label
                    (error_str e)
              | Error e, false ->
                  Alcotest.failf "%s: wrong error %s" label (error_str e))
            all_actions)
        all_statuses)

let terminal_case =
  Alcotest.test_case "transitions: rejected and removed are terminal" `Quick
    (fun () ->
      List.iter
        (fun terminal ->
          List.iter
            (fun action ->
              match Cc.apply (value_of_status terminal) action with
              | Error Cc.Invalid_transition -> ()
              | Ok _ ->
                  Alcotest.failf "%s reopened by %s" (status_str terminal)
                    (action_str action)
              | Error e ->
                  Alcotest.failf "%s + %s: %s" (status_str terminal)
                    (action_str action) (error_str e))
            all_actions)
        [ Cc.Rejected; Cc.Removed ])

let note_survives_transitions_case =
  Alcotest.test_case "transitions: pair and note carry through unchanged" `Quick
    (fun () ->
      let value = pending ~note:"  keep me\r\nverbatim  " () in
      let accepted = ok "accept" (Cc.apply value Cc.Accept) in
      let removed = ok "remove" (Cc.apply accepted Cc.Remove) in
      List.iter
        (fun (label, v) ->
          Alcotest.(check (option string))
            (label ^ ": note") (Some "keep me\nverbatim") (Cc.request_note v);
          Alcotest.(check int)
            (label ^ ": requester") 11
            (Cc.requester_community_id v);
          Alcotest.(check int)
            (label ^ ": recipient") 22
            (Cc.recipient_community_id v))
        [ ("pending", value); ("accepted", accepted); ("removed", removed) ])

(* === the pair === *)

let self_connection_case =
  Alcotest.test_case "pair: a community can never connect to itself" `Quick
    (fun () ->
      List.iter
        (fun id ->
          match make ~requester:id ~recipient:id () with
          | Error Cc.Self_connection -> ()
          | Error e -> Alcotest.failf "self %d: wrong error %s" id (error_str e)
          | Ok _ -> Alcotest.failf "self %d accepted" id)
        [ 1; 7; 4242 ])

let invalid_community_id_case =
  Alcotest.test_case "pair: non-positive community ids reject first" `Quick
    (fun () ->
      List.iter
        (fun (requester, recipient) ->
          match make ~requester ~recipient () with
          | Error Cc.Invalid_community_id -> ()
          | Error e ->
              Alcotest.failf "(%d,%d): wrong error %s" requester recipient
                (error_str e)
          | Ok _ -> Alcotest.failf "(%d,%d) accepted" requester recipient)
        [ (0, 5); (5, 0); (-1, 5); (5, -1); (0, 0); (-3, -3); (min_int, 1) ])

let symmetry_case =
  Alcotest.test_case "pair: symmetry helpers agree in both directions" `Quick
    (fun () ->
      let forward = ok "forward" (make ~requester:3 ~recipient:9 ()) in
      let mirror = ok "mirror" (make ~requester:9 ~recipient:3 ()) in
      let pair_str v =
        let a, b = Cc.unordered_pair v in
        Printf.sprintf "%d-%d" a b
      in
      Alcotest.(check string) "forward pair ascending" "3-9" (pair_str forward);
      Alcotest.(check string) "mirror pair ascending" "3-9" (pair_str mirror);
      List.iter
        (fun (label, v) ->
          Alcotest.(check bool)
            (label ^ ": involves 3") true
            (Cc.involves v ~community_id:3);
          Alcotest.(check bool)
            (label ^ ": involves 9") true
            (Cc.involves v ~community_id:9);
          Alcotest.(check bool)
            (label ^ ": ignores a stranger")
            false
            (Cc.involves v ~community_id:4);
          Alcotest.(check (option int))
            (label ^ ": counterpart of 3")
            (Some 9)
            (Cc.counterpart v ~community_id:3);
          Alcotest.(check (option int))
            (label ^ ": counterpart of 9")
            (Some 3)
            (Cc.counterpart v ~community_id:9);
          Alcotest.(check (option int))
            (label ^ ": stranger has none")
            None
            (Cc.counterpart v ~community_id:4))
        [ ("forward", forward); ("mirror", mirror) ];
      (* Direction survives only in the provenance accessors. *)
      Alcotest.(check int)
        "forward requester" 3
        (Cc.requester_community_id forward);
      Alcotest.(check int)
        "forward recipient" 9
        (Cc.recipient_community_id forward);
      Alcotest.(check int)
        "mirror requester" 9
        (Cc.requester_community_id mirror);
      Alcotest.(check int)
        "mirror recipient" 3
        (Cc.recipient_community_id mirror))

(* === note canonicalization === *)

let note_of label raw =
  match make ~note:raw () with
  | Ok v -> Cc.request_note v
  | Error e -> Alcotest.failf "%s: unexpected %s" label (error_str e)

let note_canonicalization_case =
  Alcotest.test_case "note: blank collapses, edges trim, line endings unify"
    `Quick (fun () ->
      Alcotest.(check (option string))
        "absent stays absent" None
        (match make () with
        | Ok v -> Cc.request_note v
        | Error e -> Alcotest.failf "absent: %s" (error_str e));
      List.iter
        (fun raw ->
          Alcotest.(check (option string))
            (Printf.sprintf "blank %S collapses" raw)
            None (note_of "blank" raw))
        [ ""; " "; "   "; "\t"; "\n"; "\r\n"; " \t\r\n\x0b\x0c " ];
      Alcotest.(check (option string))
        "outer whitespace trimmed" (Some "hello")
        (note_of "trim" "  \t\n hello \n\t  ");
      Alcotest.(check (option string))
        "CRLF becomes LF" (Some "a\nb") (note_of "crlf" "a\r\nb");
      Alcotest.(check (option string))
        "lone CR becomes LF" (Some "a\nb") (note_of "cr" "a\rb");
      Alcotest.(check (option string))
        "internal spacing preserved byte-for-byte"
        (Some "one  two\n\n\tthree — quatre ✓")
        (note_of "inner" "\n one  two\n\n\tthree — quatre ✓  \n");
      Alcotest.(check (option string))
        "no markdown or HTML parsing" (Some "<b>**x**</b> & <script>")
        (note_of "markup" " <b>**x**</b> & <script> "))

let note_rejection_case =
  Alcotest.test_case "note: control bytes and invalid UTF-8 reject" `Quick
    (fun () ->
      List.iter
        (fun (label, raw) ->
          match make ~note:raw () with
          | Error Cc.Invalid_request_note -> ()
          | Error e -> Alcotest.failf "%s: wrong error %s" label (error_str e)
          | Ok _ -> Alcotest.failf "%s accepted" label)
        [
          ("NUL", "a\x00b");
          ("SOH", "a\x01b");
          ("ESC", "a\x1bb");
          ("BEL", "a\x07b");
          ("DEL", "a\x7fb");
          ("vertical tab", "a\x0bb");
          ("form feed", "a\x0cb");
          ("lone continuation", "a\x80b");
          ("truncated sequence", "a\xc3");
          ("bare FF", "a\xffb");
          ("surrogate", "a\xed\xa0\x80b");
          ("overlong", "a\xc0\xafb");
        ];
      (* Tab and LF are content, not control noise. *)
      Alcotest.(check (option string))
        "tab and LF survive inside" (Some "a\tb\nc")
        (note_of "inner controls" "a\tb\nc"))

let note_length_case =
  Alcotest.test_case "note: 2000 scalars pass, 2001 reject, nothing truncates"
    `Quick (fun () ->
      let repeat s n = String.concat "" (List.init n (fun _ -> s)) in
      let ascii_max = repeat "x" 2000 in
      Alcotest.(check (option string))
        "2000 ASCII scalars accepted whole" (Some ascii_max)
        (note_of "ascii max" ascii_max);
      (* Counted per Unicode scalar, never per byte: 2000 two-byte scalars
         are 4000 bytes and must still pass. *)
      let wide_max = repeat "é" 2000 in
      Alcotest.(check int)
        "wide fixture really is 4000 bytes" 4000 (String.length wide_max);
      Alcotest.(check (option string))
        "2000 wide scalars accepted whole" (Some wide_max)
        (note_of "wide max" wide_max);
      List.iter
        (fun (label, raw) ->
          match make ~note:raw () with
          | Error Cc.Invalid_request_note -> ()
          | Error e -> Alcotest.failf "%s: wrong error %s" label (error_str e)
          | Ok v ->
              Alcotest.failf "%s: silently kept %d bytes" label
                (match Cc.request_note v with
                | Some s -> String.length s
                | None -> 0))
        [
          ("2001 ASCII", repeat "x" 2001);
          ("2001 wide", repeat "é" 2001);
          ("far over", repeat "x" 12000);
        ];
      (* Trimming happens before counting, so padding cannot push a legal
         note over the edge. *)
      Alcotest.(check (option string))
        "outer padding does not count" (Some ascii_max)
        (note_of "padded max" ("   " ^ ascii_max ^ "\n\n")))

let note_precedence_case =
  Alcotest.test_case "note: pair validation precedes note validation" `Quick
    (fun () ->
      match make ~requester:5 ~recipient:5 ~note:"a\x00b" () with
      | Error Cc.Self_connection -> ()
      | Error e -> Alcotest.failf "wrong error %s" (error_str e)
      | Ok _ -> Alcotest.fail "accepted a self-connection")

let suite =
  [
    status_round_trip_case;
    status_drift_case;
    legal_transitions_case;
    illegal_transitions_case;
    terminal_case;
    note_survives_transitions_case;
    self_connection_case;
    invalid_community_id_case;
    symmetry_case;
    note_canonicalization_case;
    note_rejection_case;
    note_length_case;
    note_precedence_case;
  ]

let suites =
  (* Mutual connections between communities (issue #30), storage/domain
       slice: the pure lifecycle domain and note canonicalization are
       DB-free; the two tables' constraints, the transactional store with
       its one-audit-event-per-mutation rule, the unordered-pair
       arbitration under real concurrency, and the symmetric/queue reads
       are database-gated. *)
  [ ("community_connections_domain", suite) ]
