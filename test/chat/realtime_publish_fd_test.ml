(* === REALTIME PUBLISH DESCRIPTOR BOUND ===
   DB-free. A publish to a gateway that accepted the connection, never
   answered and was then killed used to leave one descriptor open in the
   server per message: the HTTP client closed an abandoned connection only on
   a clean EOF, and a killed peer resets instead. A long enough hang could
   exhaust the server's descriptors. These cases drive Realtime.post_json
   against loopback peers that misbehave in exactly those ways, repeatedly,
   and require this process's open descriptors to return to the baseline
   every cycle. The peers live in this process, so their own sockets are
   counted too and closed before each measurement. *)

let ( let* ) = Lwt.bind
let fd_dir = "/proc/self/fd"
let open_fds () = Array.length (Sys.readdir fd_dir)

type peer = Silent_then_reset | Reset_on_request | Accepted | Garbage

(* A listener on an ephemeral loopback port. Every accepted socket is kept
   so the case decides when (and how) the peer goes away. *)
let with_peer behaviour f =
  let listener = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let* () =
    Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0))
  in
  Lwt_unix.listen listener 256;
  let port =
    match Lwt_unix.getsockname listener with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in
  let accepted = ref [] in
  let reset fd =
    (* SO_LINGER 0: close sends RST, as a killed process's kernel does for
       a connection with unread data. *)
    Lwt_unix.setsockopt_optint fd Unix.SO_LINGER (Some 0);
    Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)
  in
  let read_request fd =
    let buf = Bytes.create 4096 in
    Lwt.catch
      (fun () ->
        let* _ = Lwt_unix.read fd buf 0 4096 in
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  in
  let respond fd text =
    Lwt.catch
      (fun () ->
        let* _ = Lwt_unix.write_string fd text 0 (String.length text) in
        Lwt.return_unit)
      (fun _ -> Lwt.return_unit)
  in
  let serve fd =
    match behaviour with
    | Silent_then_reset -> Lwt.return_unit
    | Reset_on_request ->
        let* () = read_request fd in
        accepted := List.filter (( != ) fd) !accepted;
        reset fd
    | Accepted ->
        let* () = read_request fd in
        respond fd
          "HTTP/1.1 202 Accepted\r\n\
           Content-Length: 9\r\n\
           Connection: close\r\n\
           \r\n\
           published"
    | Garbage ->
        let* () = read_request fd in
        respond fd "nonsense\r\n"
  in
  let rec accept_loop () =
    let* fd, _ = Lwt_unix.accept listener in
    accepted := fd :: !accepted;
    Lwt.async (fun () -> serve fd);
    accept_loop ()
  in
  let acceptor = accept_loop () in
  let kill () =
    let fds = !accepted in
    accepted := [];
    Lwt_list.iter_s reset fds
  in
  Lwt.finalize
    (fun () -> f ~url:(Printf.sprintf "http://127.0.0.1:%d" port) ~kill)
    (fun () ->
      Lwt.cancel acceptor;
      let* () = kill () in
      Lwt_unix.close listener)

let publish_many ~url n =
  Lwt.join
    (List.init n (fun i ->
         Earde.Realtime.post_json ~timeout:0.15 ~gateway_url:url
           ~internal_secret:"fd-test"
           (Earde.Realtime.publish_body ~topic:"chan:1:0" ~channel_id:1
              ~community_id:1 ~message_id:(Int64.of_int i) ~user_id:1
              ~username:"u" ~content:"x" ~created_at:"t")))

let settle () = Lwt_unix.sleep 0.2

(* A leak shows as one descriptor per publish. The full suite runs other
   cases' background work in this process, so the count may move by a few
   descriptors of theirs either way; it must never grow by anything like a
   cycle's worth. *)
let publishes_per_cycle = 60
let tolerance = 5

(* Lwt opens its job-notification descriptor on first use (the address
   lookup), so one throwaway publish comes before any baseline. *)
let warm_up () = publish_many ~url:"http://127.0.0.1:9" 1

let cycles behaviour ~label =
  Alcotest.test_case label `Quick (fun () ->
      if not (Sys.file_exists fd_dir) then Alcotest.skip ()
      else
        Lwt_main.run
          (let* () = warm_up () in
           let* () = settle () in
           let baseline = open_fds () in
           let rec cycle k =
             if k = 0 then Lwt.return_unit
             else
               let* () =
                 with_peer behaviour (fun ~url ~kill ->
                     let* () = publish_many ~url publishes_per_cycle in
                     (* The gateway goes away only after every publish has
                        given up on it, as in the hang-then-kill reproduction. *)
                     let* () = kill () in
                     settle ())
               in
               let* () = settle () in
               let now = open_fds () in
               if now > baseline + tolerance then
                 Alcotest.failf
                   "%s: %d descriptors after cycle %d, baseline %d (a leak \
                    adds one per publish, %d per cycle)"
                   label now k baseline publishes_per_cycle;
               cycle (k - 1)
           in
           cycle 5))

(* A gateway that is not listening at all costs nothing either. *)
let refused_case =
  Alcotest.test_case "connection refused leaves no descriptor" `Quick (fun () ->
      if not (Sys.file_exists fd_dir) then Alcotest.skip ()
      else
        Lwt_main.run
          (let* () = warm_up () in
           let* () = settle () in
           let baseline = open_fds () in
           let* () = publish_many ~url:"http://127.0.0.1:9" 50 in
           let* () = publish_many ~url:"https://127.0.0.1:9" 5 in
           let* () = settle () in
           let now = open_fds () in
           if now > baseline + tolerance then
             Alcotest.failf "%d descriptors, baseline %d" now baseline;
           Lwt.return_unit))

let suites =
  [
    ( "realtime_publish_descriptors",
      [
        cycles Silent_then_reset
          ~label:"a gateway that hangs and is then killed leaks nothing";
        cycles Reset_on_request
          ~label:"a gateway that resets mid-request leaks nothing";
        cycles Accepted ~label:"normal publishes leak nothing";
        cycles Garbage ~label:"a malformed reply leaks nothing";
        refused_case;
      ] );
  ]
