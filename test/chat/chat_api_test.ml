module ST = Earde.Pages.Start_thread
module CA = Earde.Handlers.Chat_api

(* Chat composer JSON contract (Chat_api): pure negotiation/validation/shape
   helpers behind POST /messages. The form path (wants_json = false) keeps the
   legacy redirect/HTML contract, which is what the "browser accept" cases pin. *)

let check_wants_json name expected accept =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (CA.wants_json accept))

let content_result_str = function
  | Ok content -> "ok:" ^ content
  | Error `Empty -> "empty"
  | Error `Too_long -> "too_long"

let check_content name expected raw =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (content_result_str (CA.validate_content raw)))

let check_error_json name expected ~code ~message =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (CA.error_json ~code ~message))

(* JSON success shape: the composer response is the same canonical row as a
   catch-up entry / realtime new_msg payload, minute-precision timestamp
   included. Built from a plain Db.chat_message record — no DB. *)
let chat_row ?(deleted = false) ~id ~content ~created_at () : Earde.Db.chat_message =
  { Earde.Db.id = Int64.of_int id; channel_id = 14; user_id = Some 7;
    content; created_at; edited_at = None;
    deleted_at = (if deleted then Some created_at else None) }

let check_msg_json name expected ?thread_id row author =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected
        (Yojson.Safe.to_string
           (Earde.Handlers.chat_message_json ~channel_id:14 ~community_id:24
              ?thread_id (row, author))))

let suites =
  [ ( "chat_api_negotiation"
    , [ check_wants_json "no accept header (legacy form post) stays redirect" false None
      ; check_wants_json "browser navigation accept stays redirect" false
          (Some "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      ; check_wants_json "fetch json accept" true (Some "application/json")
      ; check_wants_json "json among other ranges" true
          (Some "application/json, text/plain, */*")
      ; check_wants_json "case-insensitive" true (Some "Application/JSON")
      ; check_wants_json "empty string" false (Some "")
      ] )
  ; ( "chat_api_content"
    , [ check_content "plain message" "ok:hello" "hello"
      ; check_content "trimmed" "ok:hi" "  hi  "
      ; check_content "empty" "empty" ""
      ; check_content "whitespace-only" "empty" "  \n \t "
      ; check_content "exactly 4000 accepted" ("ok:" ^ String.make 4000 'a')
          (String.make 4000 'a')
      ; check_content "4001 rejected" "too_long" (String.make 4001 'a')
      ; check_content "4001 with surrounding spaces still 4001" "too_long"
          (" " ^ String.make 4001 'a' ^ " ")
      ; check_content "4000 after trim accepted" ("ok:" ^ String.make 4000 'b')
          (" " ^ String.make 4000 'b' ^ " ")
      ] )
  ; ( "chat_api_error_shape"
    , [ check_error_json "code and copy only"
          {|{"error":"too_long","message":"Messages cannot exceed 4000 characters."}|}
          ~code:"too_long" ~message:"Messages cannot exceed 4000 characters."
      ; Alcotest.test_case "internal error body carries no detail" `Quick (fun () ->
            Alcotest.(check string) "fixed body"
              {|{"error":"internal","message":"Something went wrong. Please try again."}|}
              CA.internal_error_json)
      ] )
  ; ( "chat_send_success_row"
      (* The composer's JSON success body is chat_message_json applied to the
         INSERT ... RETURNING row: created_at comes from Postgres (NOT NULL
         default, '::text' cast), so the response always carries a canonical,
         renderable, minute-precision timestamp — never blank, never invented. *)
    , [ Alcotest.test_case "postgres microsecond timestamp renders non-empty minute" `Quick
          (fun () ->
            let row =
              chat_row ~id:2001 ~content:"hello"
                ~created_at:"2026-07-20 15:01:25.066287" ()
            in
            let json =
              Yojson.Safe.to_string
                (Earde.Handlers.chat_message_json ~channel_id:14 ~community_id:24
                   (row, Some "alice"))
            in
            Alcotest.(check bool) "created_at present, minute precision" true
              (let open Yojson.Safe.Util in
               member "created_at" (Yojson.Safe.from_string json)
               = `String "2026-07-20 15:01"))
      ; Alcotest.test_case "second-precision timestamp also renders non-empty minute" `Quick
          (fun () ->
            Alcotest.(check string) "minute truncation" "2026-07-20 15:01"
              (ST.minute_of_ts "2026-07-20 15:01:25"))
      ; check_msg_json
          "success body equals the realtime/catch-up serialization of the same row"
          {|{"v":1,"type":"chat_message_created","id":2002,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"same shape","created_at":"2026-07-20 15:01","deleted":false,"thread_id":null}|}
          (chat_row ~id:2002 ~content:"same shape"
             ~created_at:"2026-07-20 15:01:25.066287" ())
          (Some "alice")
      ; check_wants_json "curl/form default Accept */* stays redirect (no-JS contract)"
          false (Some "*/*")
      ; Alcotest.test_case "insert failure body is the fixed safe error, never a success row" `Quick
          (fun () ->
            let body = CA.internal_error_json in
            let json = Yojson.Safe.from_string body in
            let open Yojson.Safe.Util in
            Alcotest.(check bool) "has error code" true
              (member "error" json = `String "internal");
            Alcotest.(check bool) "carries no message id" true
              (member "id" json = `Null);
            Alcotest.(check bool) "carries no created_at" true
              (member "created_at" json = `Null))
      ] )
  ; ( "chat_message_json_shape"
    , [ check_msg_json "success row is new_msg-shaped with minute timestamp"
          {|{"v":1,"type":"chat_message_created","id":1765,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"hi there","created_at":"2026-07-20 15:01","deleted":false,"thread_id":null}|}
          (chat_row ~id:1765 ~content:"hi there"
             ~created_at:"2026-07-20 15:01:25.066287" ())
          (Some "alice")
      ; check_msg_json "deleted row masks content"
          {|{"v":1,"type":"chat_message_created","id":9,"channel_id":14,"community_id":24,"user_id":7,"username":"alice","content":"[message deleted]","created_at":"2026-07-20 15:01","deleted":true,"thread_id":null}|}
          (chat_row ~deleted:true ~id:9 ~content:"secret"
             ~created_at:"2026-07-20 15:01:25" ())
          (Some "alice")
      ; check_msg_json "tombstoned author and seed thread id"
          {|{"v":1,"type":"chat_message_created","id":3,"channel_id":14,"community_id":24,"user_id":7,"username":"[deleted]","content":"x","created_at":"2026-06-12 10:04","deleted":false,"thread_id":72}|}
          ~thread_id:72
          (chat_row ~id:3 ~content:"x" ~created_at:"2026-06-12 10:04:56" ())
          None
      ] )
  ]
