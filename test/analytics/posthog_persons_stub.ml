(* In-process PostHog Persons API stub on an ephemeral 127.0.0.1 port,
   plus the fixed identifiers its cases use. No real PostHog access. *)

let ( let* ) = Lwt.bind

(* --- Step-7: durable PostHog person deletion ------------------------------ *)

(* Local Persons-API stub: an in-process cohttp server on an ephemeral
   127.0.0.1 port — no real PostHog, no internet. [handler] maps a recorded
   request to (status code, body). *)

type req = {
  meth : string;
  path : string;
  query : (string * string list) list;
  auth : string option;
  body : string;
}

let free_port () =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in
  Unix.close sock;
  port

let start handler =
  let ( let* ) = Lwt.bind in
  let seen = ref [] in
  let callback _conn request body =
    let uri = Cohttp.Request.uri request in
    let path = Uri.path uri in
    if path = "/__ready" then
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"ok" ()
    else begin
      let ( let* ) = Lwt.bind in
      let* body_string = Cohttp_lwt.Body.to_string body in
      let req =
        {
          meth = Cohttp.Code.string_of_method (Cohttp.Request.meth request);
          path;
          query = Uri.query uri;
          auth =
            Cohttp.Header.get (Cohttp.Request.headers request) "authorization";
          body = body_string;
        }
      in
      seen := !seen @ [ req ];
      let status, body = handler req in
      Cohttp_lwt_unix.Server.respond_string
        ~status:(Cohttp.Code.status_of_code status)
        ~body ()
    end
  in
  let port = free_port () in
  let stop_promise, stop_resolver = Lwt.wait () in
  Lwt.async (fun () ->
      Cohttp_lwt_unix.Server.create ~stop:stop_promise
        ~mode:(`TCP (`Port port))
        (Cohttp_lwt_unix.Server.make ~callback ()));
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  let rec wait_ready retries =
    Lwt.catch
      (fun () ->
        let* _resp, body =
          Cohttp_lwt_unix.Client.get (Uri.of_string (base_url ^ "/__ready"))
        in
        Cohttp_lwt.Body.drain_body body)
      (fun exn ->
        if retries <= 0 then Lwt.reraise exn
        else
          let* () = Lwt_unix.sleep 0.02 in
          wait_ready (retries - 1))
  in
  let* () = wait_ready 100 in
  Lwt.return (base_url, seen, fun () -> Lwt.wakeup_later stop_resolver ())

(* Dummy credential values only — never real ones. *)
let deletion_test_key = "phx_test_dummy"
let stub_uuid = "11111111-2222-3333-4444-555555555555"
let person_json uuid = Printf.sprintf {|{"id": %S, "properties": {}}|} uuid

let results_body persons =
  Printf.sprintf {|{"results": [%s]}|} (String.concat ", " persons)
