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
  if len > 0 && value.[len - 1] = '/' then String.sub value 0 (len - 1)
  else value

let endpoint_uri gateway_url =
  Uri.of_string (strip_trailing_slash gateway_url ^ "/internal/publish")

let json_int64 value = `Intlit (Int64.to_string value)

(* [topic] carries the community's access generation (Realtime_generation),
   read by the caller after the message committed. *)
let publish_body ~topic ~channel_id ~community_id ~message_id ~user_id ~username
    ~content ~created_at =
  let payload =
    `Assoc
      [
        ("v", `Int 1);
        ("type", `String "chat_message_created");
        ("id", json_int64 message_id);
        ("channel_id", `Int channel_id);
        ("community_id", `Int community_id);
        ("user_id", `Int user_id);
        ("username", `String username);
        ("content", `String content);
        ("created_at", `String created_at);
      ]
  in
  `Assoc
    [
      ("topic", `String topic);
      ("event", `String "new_msg");
      ("payload", payload);
    ]

(* The publish owns its socket. It used to go through Cohttp_lwt_unix.Client
   under an Lwt.pick timeout: the abandoned call's connection was closed only
   on a clean EOF, so a gateway that accepted, never answered and was then
   killed (a reset, not an EOF) left one descriptor behind per message, and a
   long enough hang could exhaust the process's descriptors and take the whole
   server down. Here the descriptor is created and closed by this function:
   it is closed when the status line arrives, on any error, or at the deadline,
   whichever comes first, whatever the peer does. The gateway is a loopback
   plain-HTTP service (docs/deployment.md), so only http:// URLs are used. *)
let status_of_line line =
  match String.split_on_char ' ' line with
  | version :: code :: _ when String.starts_with ~prefix:"HTTP/1." version ->
      int_of_string_opt code
  | _ -> None

let request_text ~host_header ~path ~internal_secret body =
  Printf.sprintf
    "POST %s HTTP/1.1\r\n\
     Host: %s\r\n\
     Content-Type: application/json\r\n\
     X-Earde-Internal: %s\r\n\
     Content-Length: %d\r\n\
     Connection: close\r\n\
     \r\n\
     %s"
    path host_header internal_secret (String.length body) body

let rec write_all fd text offset =
  if offset >= String.length text then Lwt.return_unit
  else
    Lwt_unix.write_string fd text offset (String.length text - offset)
    >>= fun written -> write_all fd text (offset + written)

let post_json ?(timeout = publish_timeout_seconds) ~gateway_url ~internal_secret
    body_json =
  let uri = endpoint_uri gateway_url in
  match (Uri.scheme uri, Uri.host uri) with
  | Some "http", Some host -> (
      let port = Option.value (Uri.port uri) ~default:80 in
      let host_header =
        (* An IPv6 literal needs its brackets back in a Host header. *)
        let host =
          if String.contains host ':' then "[" ^ host ^ "]" else host
        in
        match Uri.port uri with
        | Some p -> Printf.sprintf "%s:%d" host p
        | None -> host
      in
      let request =
        request_text ~host_header ~path:(Uri.path_and_query uri)
          ~internal_secret
          (Yojson.Safe.to_string body_json)
      in
      (* [finished] is set before the descriptor is closed: a socket created
         by a step that outlived the deadline is closed at once instead of
         being recorded after the cleanup already ran. *)
      let finished = ref false in
      let owned = ref None in
      let close_owned () =
        match !owned with
        | None -> Lwt.return_unit
        | Some fd ->
            owned := None;
            Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
      in
      let exchange () =
        Lwt_unix.getaddrinfo host (string_of_int port)
          [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
        >>= function
        | [] -> Lwt.return (Error (`Failed "gateway address did not resolve"))
        | address :: _ ->
            let fd =
              Lwt_unix.socket address.Unix.ai_family Unix.SOCK_STREAM 0
            in
            if !finished then
              Lwt_unix.close fd >>= fun () ->
              Lwt.return (Error (`Failed "late socket"))
            else begin
              owned := Some fd;
              Lwt_unix.connect fd address.Unix.ai_addr >>= fun () ->
              write_all fd request 0 >>= fun () ->
              let input =
                Lwt_io.of_fd ~mode:Lwt_io.input
                  ~close:(fun () -> Lwt.return_unit)
                  fd
              in
              Lwt_io.read_line input >|= fun line ->
              match status_of_line line with
              | Some status -> Ok status
              | None -> Error (`Failed "malformed status line")
            end
      in
      Lwt.finalize
        (fun () ->
          Lwt.pick
            [
              Lwt.catch exchange (fun exn ->
                  Lwt.return (Error (`Failed (Printexc.to_string exn))));
              (Lwt_unix.sleep timeout >|= fun () -> Error `Timed_out);
            ])
        (fun () ->
          finished := true;
          close_owned ())
      >|= function
      | Ok status when status >= 200 && status < 300 -> ()
      | Ok status ->
          Logs.warn (fun m -> m "Realtime publish failed: status=%d" status)
      | Error `Timed_out ->
          Logs.warn (fun m ->
              m "Realtime publish timed out after %.3fs" timeout)
      | Error (`Failed reason) ->
          Logs.warn (fun m -> m "Realtime publish failed: %s" reason))
  | _ ->
      Logs.warn (fun m ->
          m "Realtime publish skipped: REALTIME_GATEWAY_URL must be http://");
      Lwt.return_unit

let publish_chat_message ~topic ~channel_id ~community_id ~message_id ~user_id
    ~username ~content ~created_at =
  match
    (getenv_nonempty gateway_url_env, getenv_nonempty internal_secret_env)
  with
  | Some gateway_url, Some internal_secret ->
      let body_json =
        publish_body ~topic ~channel_id ~community_id ~message_id ~user_id
          ~username ~content ~created_at
      in
      Lwt.catch
        (fun () -> post_json ~gateway_url ~internal_secret body_json)
        (fun exn ->
          Logs.warn (fun m ->
              m "Realtime publish exception: %s" (Printexc.to_string exn));
          Lwt.return_unit)
  | _ -> Lwt.return_unit
