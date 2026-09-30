module ST = Earde.Chat_pages.Start_thread
(* "Start thread from chat" pure helpers — title/body prefill, checkbox-id parsing,
   server-side selection guard. No DB, no request. *)

let check_title name expected content =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (ST.derive_title content))

let check_ids name expected form =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list int64)) name expected (ST.parse_selected_ids form))

let check_norm name expected ~seed ~max_total ~valid selected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list int64)) name expected
        (ST.normalize_selection ~seed ~max_total ~valid selected))

(* Render the marker variant to a stable string so we can assert without an Alcotest
   testable for the variant. *)
let marker_str = function
  | ST.Mk_seed (pid, t) -> Printf.sprintf "seed:%d:%s" pid t
  | ST.Mk_referenced (pid, t, n) -> Printf.sprintf "ref:%d:%s:%d" pid t n
  | ST.Mk_no_link -> "none"

let check_marker name expected links =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (marker_str (ST.classify_message_links links)))

(* Reverse-navigation query-parameter parse: strict positive ints only. *)
let check_src_thread name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (option int)) name expected (ST.parse_source_thread raw))

(* data-source-highlight-ids serialization: digits and commas only. *)
let check_hl name expected ids =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (ST.highlight_ids_attr ids))

(* Compact timestamp truncations of Postgres timestamp text. *)
let check_ts name expected f raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (f raw))

(* Promoted-conversation provenance summary — a source-row constructor keeps the cases
   readable; summaries render as a stable string for a single assertion per case. *)
let sm ?(seed = false) ?(deleted = false) ~id ~author ~at content : Earde.Thread_source_store.thread_source_msg =
  { Earde.Thread_source_store.sm_id = Int64.of_int id; sm_author = author; sm_content = content;
    sm_created_at = at; sm_is_seed = seed; sm_deleted = deleted }

let summary_str (s : ST.source_summary) =
  Printf.sprintf "avail:%d unavail:%d parts:%d range:%s"
    s.ST.ss_available s.ST.ss_unavailable s.ST.ss_participants s.ST.ss_date_range

let check_summary name expected msgs =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (summary_str (ST.summarize_source msgs)))

(* Image src gate — pure, no DB. Local upload paths and http(s) pass; everything dangerous
   collapses to "#"; passed values are always html-escaped so they can't break the attribute. *)
let check_img name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (Earde.Html.to_string (Earde.Html.image_src raw)))

(* Report enum conversions (Slice A): pure, no DB. Closed variant -> string -> variant must
   round-trip, and any off-enum string must be rejected with None. [to_s]/[of_s] are the
   per-enum helpers; polymorphic [=] compares the variant options directly. *)
let check_round_trip name to_s of_s v =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (of_s (to_s v) = Some v))

let check_none name of_s s =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name true (of_s s = None))

let suites =
    (* Title prefill: first sentence / newline, trimmed. *)
  [ ( "start_thread_title"
    , [ check_title "empty" "" ""
      ; check_title "first sentence" "Hello there" "Hello there. More text here."
      ; check_title "newline cut" "First line" "First line\nsecond line"
      ; check_title "no boundary" "just a phrase" "just a phrase"
      ; check_title "trimmed" "spaced" "   spaced.   "
      ] )
    (* Checkbox field parsing: msg_<id> only, deduped + sorted, bad ids dropped. *)
  ; ( "start_thread_parse_ids"
    , [ check_ids "basic" [10L; 20L] [("msg_10", "on"); ("msg_20", "on")]
      ; check_ids "ignores others" [5L] [("dream.csrf", "x"); ("title", "t"); ("msg_5", "on")]
      ; check_ids "dedup and sort" [3L; 7L] [("msg_7", "on"); ("msg_3", "on"); ("msg_7", "on")]
      ; check_ids "bad ids ignored" [] [("msg_abc", "on"); ("msg_", "on")]
      ; check_ids "none" [] [("title", "x")]
      ] )
    (* Selection guard: seed forced out, invalids dropped, chronological, capped to max-1. *)
  ; ( "start_thread_normalize"
    , [ check_norm "drops seed and sorts" [2L; 4L] ~seed:3L ~max_total:10 ~valid:[2L; 3L; 4L] [4L; 2L; 3L]
      ; check_norm "drops invalid" [2L] ~seed:3L ~max_total:10 ~valid:[2L; 3L] [2L; 9L; 100L]
      ; check_norm "caps to max-1" [1L; 2L; 3L; 4L] ~seed:99L ~max_total:5 ~valid:[1L; 2L; 3L; 4L; 5L; 6L] [6L; 5L; 4L; 3L; 2L; 1L]
      ; check_norm "dedup" [2L] ~seed:1L ~max_total:10 ~valid:[2L] [2L; 2L; 2L]
      ] )
    (* Channel-row marker: seed wins; else most-recent reference is target with count.
       (The visible copy — "Started thread →" / "Included in thread →" — lives in the
       channel renderer; classification is what's pure and covered here.) *)
  ; ( "start_thread_marker"
    , [ check_marker "no links" "none" []
      ; check_marker "seed wins over refs" "seed:7:Seed thread" [(3, "Ctx thread", false); (7, "Seed thread", true)]
      ; check_marker "single reference" "ref:5:Only ref:1" [(5, "Only ref", false)]
      ; check_marker "multi reference picks highest post id" "ref:9:Recent:3" [(4, "Old", false); (9, "Recent", false); (6, "Mid", false)]
      ] )
    (* ?source_thread= parse: strict positive int; anything else means "no focus". *)
  ; ( "source_thread_param"
    , [ check_src_thread "absent" None None
      ; check_src_thread "valid" (Some 42) (Some "42")
      ; check_src_thread "trimmed" (Some 7) (Some " 7 ")
      ; check_src_thread "zero rejected" None (Some "0")
      ; check_src_thread "negative rejected" None (Some "-3")
      ; check_src_thread "junk rejected" None (Some "abc")
      ; check_src_thread "injection rejected" None (Some "42'/><script>")
      ; check_src_thread "overflow rejected" None (Some "99999999999999999999999")
      ] )
    (* Highlight-id serialization: attribute-safe digits + commas, order preserved. *)
  ; ( "highlight_ids_attr"
    , [ check_hl "empty" "" []
      ; check_hl "single" "12" [12L]
      ; check_hl "ordered many" "3,7,20" [3L; 7L; 20L]
      ] )
    (* Timestamp truncations: date / minute prefixes; short input passes through. *)
  ; ( "source_timestamps"
    , [ check_ts "date" "2026-06-12" ST.date_of_ts "2026-06-12 10:04:56"
      ; check_ts "minute" "2026-06-12 10:04" ST.minute_of_ts "2026-06-12 10:04:56.123"
      ; check_ts "short date passthrough" "2026" ST.date_of_ts "2026"
      ; check_ts "short minute passthrough" "2026-06-12" ST.minute_of_ts "2026-06-12"
      ] )
    (* Provenance summary: counts, distinct participants, tombstone/deleted handling,
       same-day vs cross-day range. Derived from source rows only. *)
  ; ( "source_summary"
    , [ check_summary "empty" "avail:0 unavail:0 parts:0 range:"
          []
      ; check_summary "single message" "avail:1 unavail:0 parts:1 range:2026-06-12"
          [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "hi" ~seed:true ]
      ; check_summary "distinct participants, same day"
          "avail:3 unavail:0 parts:2 range:2026-06-12"
          [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
          ; sm ~id:2 ~author:"bob" ~at:"2026-06-12 10:01:00" "b"
          ; sm ~id:3 ~author:"alice" ~at:"2026-06-12 10:02:00" "c" ]
      ; check_summary "date range across days"
          "avail:2 unavail:0 parts:2 range:2026-06-12 \xe2\x80\x93 2026-06-13"
          [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 23:59:00" "a"
          ; sm ~id:2 ~author:"bob" ~at:"2026-06-13 00:01:00" "b" ]
      ; check_summary "deleted rows counted unavailable, excluded from participants"
          "avail:1 unavail:2 parts:1 range:2026-06-12"
          [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
          ; sm ~id:2 ~author:"" ~at:"2026-06-12 10:01:00" "" ~deleted:true
          ; sm ~id:3 ~author:"" ~at:"2026-06-12 10:02:00" "" ~deleted:true ]
      ; check_summary "tombstoned author available but not a participant"
          "avail:2 unavail:0 parts:1 range:2026-06-12"
          [ sm ~id:1 ~author:"alice" ~at:"2026-06-12 10:00:00" "a"
          ; sm ~id:2 ~author:"" ~at:"2026-06-12 10:01:00" "ghost message" ]
      ] )
    (* Image src gate: local uploads + http(s) pass (html-escaped); javascript:/data:/
       protocol-relative/injection/empty collapse to "#". *)
  ; ( "safe_img_src"
    , [ check_img "local upload" "/static/uploads/foo.webp" "/static/uploads/foo.webp"
      ; check_img "https" "https://example.com/a.webp" "https://example.com/a.webp"
      ; check_img "http" "http://example.com/a.webp" "http://example.com/a.webp"
      ; check_img "javascript" "#" "javascript:alert(1)"
      ; check_img "data uri" "#" "data:image/svg+xml,<svg onload=alert(1)>"
      ; check_img "protocol-relative" "#" "//evil.com/x.webp"
      ; check_img "backslash protocol-relative" "#" "/\\evil.com/x.webp"
      ; check_img "attribute injection (no leading slash)" "#" "x' onerror='alert(1)"
      ; check_img "whitespace only" "#" "   "
      ; check_img "empty" "#" ""
      ; check_img "uppercase scheme rejected as non-local" "#" "JAVASCRIPT:alert(1)"
        (* Quote / whitespace anywhere in the candidate is refused outright, not escaped. *)
      ; check_img "local path with quote returns #" "#" "/static/uploads/foo' onerror='alert(1).webp"
      ; check_img "local path with whitespace returns #" "#" "/static/uploads/foo bar.webp"
      ; check_img "valid generated upload path passes"
          "/static/uploads/earde_123_456.webp" "/static/uploads/earde_123_456.webp"
      ] )
    (* Report target enum: every constructor round-trips; off-enum strings rejected. *)
  ; ( "report_target_roundtrip"
    , [ check_round_trip "post" Earde.Report_store.report_target_to_string Earde.Report_store.report_target_of_string Earde.Report_store.Report_post
      ; check_round_trip "comment" Earde.Report_store.report_target_to_string Earde.Report_store.report_target_of_string Earde.Report_store.Report_comment
      ; check_round_trip "chat_message" Earde.Report_store.report_target_to_string Earde.Report_store.report_target_of_string Earde.Report_store.Report_chat_message
      ; check_none "empty" Earde.Report_store.report_target_of_string ""
      ; check_none "unknown" Earde.Report_store.report_target_of_string "user"
      ; check_none "case-sensitive" Earde.Report_store.report_target_of_string "Post"
      ; check_none "cross-enum status" Earde.Report_store.report_target_of_string "open"
      ] )
    (* Report reason enum. *)
  ; ( "report_reason_roundtrip"
    , [ check_round_trip "spam" Earde.Report_store.report_reason_to_string Earde.Report_store.report_reason_of_string Earde.Report_store.Report_spam
      ; check_round_trip "abuse" Earde.Report_store.report_reason_to_string Earde.Report_store.report_reason_of_string Earde.Report_store.Report_abuse
      ; check_round_trip "off_topic" Earde.Report_store.report_reason_to_string Earde.Report_store.report_reason_of_string Earde.Report_store.Report_off_topic
      ; check_round_trip "illegal" Earde.Report_store.report_reason_to_string Earde.Report_store.report_reason_of_string Earde.Report_store.Report_illegal
      ; check_round_trip "other" Earde.Report_store.report_reason_to_string Earde.Report_store.report_reason_of_string Earde.Report_store.Report_other
      ; check_none "empty" Earde.Report_store.report_reason_of_string ""
      ; check_none "unknown" Earde.Report_store.report_reason_of_string "harassment"
      ; check_none "off-topic dash variant" Earde.Report_store.report_reason_of_string "off-topic"
      ] )
    (* Report status enum. *)
  ; ( "report_status_roundtrip"
    , [ check_round_trip "open" Earde.Report_store.report_status_to_string Earde.Report_store.report_status_of_string Earde.Report_store.Report_open
      ; check_round_trip "dismissed" Earde.Report_store.report_status_to_string Earde.Report_store.report_status_of_string Earde.Report_store.Report_dismissed
      ; check_round_trip "action_taken" Earde.Report_store.report_status_to_string Earde.Report_store.report_status_of_string Earde.Report_store.Report_action_taken
      ; check_none "empty" Earde.Report_store.report_status_of_string ""
      ; check_none "unknown" Earde.Report_store.report_status_of_string "resolved"
      ; check_none "cross-enum target" Earde.Report_store.report_status_of_string "post"
      ] )
    (* Report action_kind enum. Report_other_action serializes to the bare "other". *)
  ; ( "report_action_kind_roundtrip"
    , [ check_round_trip "removed_content" Earde.Report_store.report_action_kind_to_string Earde.Report_store.report_action_kind_of_string Earde.Report_store.Report_removed_content
      ; check_round_trip "banned_author" Earde.Report_store.report_action_kind_to_string Earde.Report_store.report_action_kind_of_string Earde.Report_store.Report_banned_author
      ; check_round_trip "other" Earde.Report_store.report_action_kind_to_string Earde.Report_store.report_action_kind_of_string Earde.Report_store.Report_other_action
      ; check_none "empty" Earde.Report_store.report_action_kind_of_string ""
      ; check_none "unknown" Earde.Report_store.report_action_kind_of_string "deleted"
      ; check_none "removed variant" Earde.Report_store.report_action_kind_of_string "removed"
      ] )
  ]
