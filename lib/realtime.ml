open Lwt.Infix

let gateway_url_env = "REALTIME_GATEWAY_URL"
let internal_secret_env = "REALTIME_INTERNAL_SECRET"
let publish_timeout_seconds = 0.3

let getenv_nonempty name =
  match Sys.getenv_opt name with
  | None -> None
  | Some value ->
      let value = String.trim value in
      if value = "" then None else Some value

let strip_trailing_slash value =
  let len = String.length value in
  if len > 0 && value.[len - 1] = '/' then
    String.sub value 0 (len - 1)
  else
    value

let endpoint_uri gateway_url =
  Uri.of_string (strip_trailing_slash gateway_url ^ "/internal/publish")

let json_int64 value =
  `Intlit (Int64.to_string value)

let with_timeout seconds promise =
  Lwt.pick
    [ promise
    ; (Lwt_unix.sleep seconds >>= fun () ->
       Logs.warn (fun m ->
           m "Realtime publish timed out after %.3fs" seconds);
       Lwt.return_unit)
    ]

let publish_body
    ~channel_id
    ~community_id
    ~message_id
    ~user_id
    ~username
    ~content
    ~created_at =
  let topic = Printf.sprintf "chan:%d" channel_id in
  let payload =
    `Assoc
      [ ("v", `Int 1)
      ; ("type", `String "chat_message_created")
      ; ("id", json_int64 message_id)
      ; ("channel_id", `Int channel_id)
      ; ("community_id", `Int community_id)
      ; ("user_id", `Int user_id)
      ; ("username", `String username)
      ; ("content", `String content)
      ; ("created_at", `String created_at)
      ]
  in
  `Assoc
    [ ("topic", `String topic)
    ; ("event", `String "new_msg")
    ; ("payload", payload)
    ]

let post_json ~gateway_url ~internal_secret body_json =
  let uri = endpoint_uri gateway_url in
  let body =
    body_json
    |> Yojson.Safe.to_string
    |> Cohttp_lwt.Body.of_string
  in
  let headers =
    Cohttp.Header.init ()
    |> fun h -> Cohttp.Header.add h "content-type" "application/json"
    |> fun h -> Cohttp.Header.add h "x-earde-internal" internal_secret
  in
  Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (response, response_body) ->
  Cohttp_lwt.Body.to_string response_body >|= fun response_text ->
  let status = Cohttp.Response.status response |> Cohttp.Code.code_of_status in
  if status < 200 || status >= 300 then
    Logs.warn (fun m ->
        m "Realtime publish failed: status=%d body=%S" status response_text)

let publish_chat_message
    ~channel_id
    ~community_id
    ~message_id
    ~user_id
    ~username
    ~content
    ~created_at =
  match getenv_nonempty gateway_url_env, getenv_nonempty internal_secret_env with
  | Some gateway_url, Some internal_secret ->
      let body_json =
        publish_body
          ~channel_id
          ~community_id
          ~message_id
          ~user_id
          ~username
          ~content
          ~created_at
      in
      Lwt.catch
        (fun () -> 
          with_timeout publish_timeout_seconds
            (post_json ~gateway_url ~internal_secret body_json))
        (fun exn ->
          Logs.warn (fun m ->
              m "Realtime publish exception: %s" (Printexc.to_string exn));
          Lwt.return_unit)
  | _ ->
      Lwt.return_unit
