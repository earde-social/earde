(* ===================== shared threads, slice 1 =====================
   The shared-thread placement storage/domain foundation: the pure
   placement lifecycle domain, the placement and audit tables'
   constraints, and the transactional store with its append-only audit
   trail and structured notifications. Pure cases first, then the
   database-gated ones. Fixtures live in the stp-% slug / stp_% username
   namespace so cleanup is targeted and idempotent; no external-id range
   is needed because no GitHub fixtures take part. *)

(* The pure domain: closed status vocabulary, the four legal transitions
   and every illegal one, note canonicalization, the same-community rule,
   the tombstone rule, and the accessors. DB-free. *)

module P = Earde.Shared_thread_placements

let error_str : P.error -> string = function
  | P.Invalid_post_id -> "Invalid_post_id"
  | P.Invalid_community_id -> "Invalid_community_id"
  | P.Same_community -> "Same_community"
  | P.Invalid_request_note -> "Invalid_request_note"
  | P.Invalid_transition -> "Invalid_transition"

let status_str = P.string_of_status

let make ?note ?(post = 7) ?(origin = 11) ?(destination = 22) () =
  P.create_pending ~post_id:post ~origin_community_id:origin
    ~destination_community_id:destination ~request_note:note

let ok label = function
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: unexpected %s" label (error_str e)

let pending ?note () = ok "pending fixture" (make ?note ())
let all_statuses = [ P.Pending; P.Accepted; P.Rejected; P.Removed; P.Withdrawn ]
let all_actions = [ P.Accept; P.Reject; P.Withdraw; P.Remove ]

let action_str = function
  | P.Accept -> "accept"
  | P.Reject -> "reject"
  | P.Withdraw -> "withdraw"
  | P.Remove -> "remove"

let value_of_status = function
  | P.Pending -> pending ()
  | P.Accepted -> ok "accept" (P.apply (pending ()) P.Accept)
  | P.Rejected -> ok "reject" (P.apply (pending ()) P.Reject)
  | P.Withdrawn -> ok "withdraw" (P.apply (pending ()) P.Withdraw)
  | P.Removed ->
      ok "remove"
        (P.apply (ok "accept" (P.apply (pending ()) P.Accept)) P.Remove)

let status_round_trip_case =
  Alcotest.test_case "status: exact database spellings round-trip" `Quick
    (fun () ->
      Alcotest.(check (list string))
        "serialized vocabulary"
        [ "pending"; "accepted"; "rejected"; "removed"; "withdrawn" ]
        (List.map status_str all_statuses);
      List.iter
        (fun s ->
          match P.status_of_string (status_str s) with
          | Some parsed ->
              Alcotest.(check string)
                ("round-trip " ^ status_str s)
                (status_str s) (status_str parsed)
          | None ->
              Alcotest.failf "round-trip %s: rejected its own spelling"
                (status_str s))
        all_statuses)

let unknown_status_case =
  Alcotest.test_case "status: unknown database strings fail closed" `Quick
    (fun () ->
      List.iter
        (fun raw ->
          match P.status_of_string raw with
          | None -> ()
          | Some s -> Alcotest.failf "decoded %S as %s" raw (status_str s))
        [
          "Pending";
          "PENDING";
          "active";
          "cancelled";
          "withdraw";
          "deleted";
          " pending";
          "pending ";
          "";
        ])

let transition_matrix_case =
  Alcotest.test_case
    "lifecycle: exactly four transitions are legal, all others refuse" `Quick
    (fun () ->
      let legal = function
        | P.Pending, P.Accept -> Some P.Accepted
        | P.Pending, P.Reject -> Some P.Rejected
        | P.Pending, P.Withdraw -> Some P.Withdrawn
        | P.Accepted, P.Remove -> Some P.Removed
        | _ -> None
      in
      List.iter
        (fun status ->
          List.iter
            (fun action ->
              let value = value_of_status status in
              let label =
                Printf.sprintf "%s + %s" (status_str status) (action_str action)
              in
              match (P.apply value action, legal (status, action)) with
              | Ok next, Some expected ->
                  Alcotest.(check string)
                    label (status_str expected)
                    (status_str (P.status next))
              | Error P.Invalid_transition, None -> ()
              | Ok next, None ->
                  Alcotest.failf "%s: unexpectedly reached %s" label
                    (status_str (P.status next))
              | Error e, Some _ ->
                  Alcotest.failf "%s: unexpected %s" label (error_str e)
              | Error e, None ->
                  Alcotest.failf "%s: wrong error %s" label (error_str e))
            all_actions)
        all_statuses)

let withdrawn_distinct_case =
  Alcotest.test_case
    "lifecycle: withdrawn is a distinct terminal, never removed" `Quick
    (fun () ->
      let withdrawn = value_of_status P.Withdrawn in
      Alcotest.(check string)
        "spelling" "withdrawn"
        (status_str (P.status withdrawn));
      Alcotest.(check bool)
        "distinct from removed" false
        (status_str (P.status withdrawn)
        = status_str (P.status (value_of_status P.Removed)));
      (* A pending value cannot be removed, and a withdrawn one cannot be
         reviewed or removed. *)
      (match P.apply (pending ()) P.Remove with
      | Error P.Invalid_transition -> ()
      | Ok _ -> Alcotest.fail "pending + remove was allowed"
      | Error e -> Alcotest.failf "pending + remove: %s" (error_str e));
      List.iter
        (fun action ->
          match P.apply withdrawn action with
          | Error P.Invalid_transition -> ()
          | Ok _ ->
              Alcotest.failf "withdrawn + %s was allowed" (action_str action)
          | Error e ->
              Alcotest.failf "withdrawn + %s: %s" (action_str action)
                (error_str e))
        all_actions)

let create_errors_case =
  Alcotest.test_case "create: id validation and the same-community rule" `Quick
    (fun () ->
      let expect label expected result =
        match result with
        | Ok _ -> Alcotest.failf "%s: unexpectedly accepted" label
        | Error e ->
            Alcotest.(check string) label (error_str expected) (error_str e)
      in
      expect "zero post" P.Invalid_post_id (make ~post:0 ());
      expect "negative post" P.Invalid_post_id (make ~post:(-3) ());
      expect "zero origin" P.Invalid_community_id (make ~origin:0 ());
      expect "zero destination" P.Invalid_community_id (make ~destination:0 ());
      expect "same community" P.Same_community
        (make ~origin:9 ~destination:9 ());
      let v = pending () in
      Alcotest.(check int) "post accessor" 7 (P.post_id v);
      Alcotest.(check int) "origin accessor" 11 (P.origin_community_id v);
      Alcotest.(check int)
        "destination accessor" 22
        (P.destination_community_id v);
      Alcotest.(check bool)
        "involves origin" true
        (P.involves v ~community_id:11);
      Alcotest.(check bool)
        "involves destination" true
        (P.involves v ~community_id:22);
      Alcotest.(check bool)
        "involves stranger" false
        (P.involves v ~community_id:33))

let note_canonicalization_case =
  Alcotest.test_case "note: canonicalization and blank collapse" `Quick
    (fun () ->
      let note_of result = P.request_note (ok "note fixture" result) in
      Alcotest.(check (option string))
        "absent stays absent" None
        (note_of (make ()));
      Alcotest.(check (option string))
        "blank collapses" None
        (note_of (make ~note:"  \r\n\t " ()));
      Alcotest.(check (option string))
        "CRLF and trim" (Some "please\nshare")
        (note_of (make ~note:"  please\r\nshare\r " ()));
      Alcotest.(check (option string))
        "tabs survive" (Some "a\tb")
        (note_of (make ~note:"a\tb" ())))

let note_limit_case =
  Alcotest.test_case "note: the 2000-scalar cap counts scalars, not bytes"
    `Quick (fun () ->
      let ascii_max = String.make 2000 'a' in
      (match make ~note:ascii_max () with
      | Ok v ->
          Alcotest.(check (option string))
            "2000 ASCII fits" (Some ascii_max) (P.request_note v)
      | Error e -> Alcotest.failf "2000 ASCII: %s" (error_str e));
      (match make ~note:(String.make 2001 'a') () with
      | Error P.Invalid_request_note -> ()
      | Ok _ -> Alcotest.fail "2001 ASCII was accepted"
      | Error e -> Alcotest.failf "2001 ASCII: %s" (error_str e));
      let two_byte = String.concat "" (List.init 2000 (fun _ -> "\xc3\xa9")) in
      (match make ~note:two_byte () with
      | Ok _ -> ()
      | Error e -> Alcotest.failf "2000 two-byte scalars: %s" (error_str e));
      (match make ~note:(two_byte ^ "\xc3\xa9") () with
      | Error P.Invalid_request_note -> ()
      | Ok _ -> Alcotest.fail "2001 scalars were accepted"
      | Error e -> Alcotest.failf "2001 scalars: %s" (error_str e));
      (match make ~note:"nul\x00byte" () with
      | Error P.Invalid_request_note -> ()
      | Ok _ -> Alcotest.fail "a NUL byte was accepted"
      | Error e -> Alcotest.failf "NUL byte: %s" (error_str e));
      match make ~note:"broken \xff utf8" () with
      | Error P.Invalid_request_note -> ()
      | Ok _ -> Alcotest.fail "invalid UTF-8 was accepted"
      | Error e -> Alcotest.failf "invalid UTF-8: %s" (error_str e))

let tombstone_case =
  Alcotest.test_case
    "tombstone: exactly the three deletion labels, and nothing else" `Quick
    (fun () ->
      List.iter
        (fun label ->
          Alcotest.(check bool)
            label true
            (P.post_content_tombstoned (Some label)))
        [ "[deleted]"; "[removed by admin]"; "[removed by moderator]" ];
      List.iter
        (fun (label, content) ->
          Alcotest.(check bool) label false (P.post_content_tombstoned content))
        [
          ("a link post", None);
          ("ordinary text", Some "hello");
          ("prefixed", Some " [deleted]");
          ("cased", Some "[Deleted]");
          ("empty", Some "");
        ])

let suite =
  [
    status_round_trip_case;
    unknown_status_case;
    transition_matrix_case;
    withdrawn_distinct_case;
    create_errors_case;
    note_canonicalization_case;
    note_limit_case;
    tombstone_case;
  ]

let suites =
  (* Shared threads, slice 1 (storage/domain foundation): the pure
       placement lifecycle and note canonicalization are DB-free; the
       placement and audit tables' constraints, the transactional store
       with its one-audit-event-per-mutation rule and structured
       notifications, the active-(post, destination) arbitration under
       real concurrency, and the recipient policy are database-gated. *)
  [ ("shared_thread_placements_domain", suite) ]
