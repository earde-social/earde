(* Realtime access generations: losing read access takes effect on chat
   sockets that are already connected.

   The gated suites (EARDE_TEST_DATABASE_URL) pin the database triggers that
   bump a community's generation whenever read access can shrink, and drive
   the real channel-page, token-refresh, send-message and leave handlers over
   a routed pipeline, with a local stub standing in for the gateway's
   internal publish endpoint. The gateway-failure cases point Dream at a
   gateway that is down, failing or hanging: a message still commits and
   catch-up serves it. Fixture names use the rtg_ / rtg- prefixes; every
   gated case cleans before and after. *)

let ( let* ) = Lwt.bind

open Caqti_request.Infix
module G = Earde.Realtime_generation

let contains hay needle =
  let n = String.length needle and h = String.length hay in
  let rec go i = i + n <= h && (String.sub hay i n = needle || go (i + 1)) in
  n = 0 || go 0

let pure_case name f = Alcotest.test_case name `Quick f

(* === DB-free === *)

let topic_case =
  pure_case "topic: the generation is the last colon segment" (fun () ->
      Alcotest.(check string)
        "first generation" "chan:8:0"
        (G.topic ~channel_id:8 ~generation:0L);
      Alcotest.(check string)
        "later generation" "chan:12:41"
        (G.topic ~channel_id:12 ~generation:41L))

let publish_body_case =
  pure_case "publish: the body carries exactly the topic the caller read"
    (fun () ->
      let body =
        Earde.Realtime.publish_body ~topic:"chan:3:7" ~channel_id:3
          ~community_id:9 ~message_id:5L ~user_id:1 ~username:"u" ~content:"c"
          ~created_at:"t"
      in
      match body with
      | `Assoc fields ->
          Alcotest.(check bool)
            "topic" true
            (List.assoc_opt "topic" fields = Some (`String "chan:3:7"))
      | _ -> Alcotest.fail "not an object")

let pure_suite = [ topic_case; publish_body_case ]

(* === gated === *)

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let q_cleanup =
  List.map
    (fun sql -> (Caqti_type.unit ->. Caqti_type.unit) sql)
    [
      "DELETE FROM chat_messages WHERE channel_id IN (SELECT ch.id FROM \
       channels ch JOIN communities c ON c.id = ch.community_id WHERE c.slug \
       LIKE 'rtg-%')";
      "DELETE FROM channels WHERE community_id IN (SELECT id FROM communities \
       WHERE slug LIKE 'rtg-%')";
      "DELETE FROM community_members WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'rtg-%')";
      "DELETE FROM community_moderators WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'rtg-%')";
      "DELETE FROM mod_actions WHERE community_id IN (SELECT id FROM \
       communities WHERE slug LIKE 'rtg-%')";
      "DELETE FROM communities WHERE slug LIKE 'rtg-%'";
      "DELETE FROM users WHERE username LIKE 'rtg\\_%' OR email LIKE \
       '%@rtg.invalid'";
    ]

let env_keys =
  [
    "REALTIME_TOKEN_SECRET";
    "REALTIME_GATEWAY_URL";
    "REALTIME_INTERNAL_SECRET";
    "REALTIME_SOCKET_URL";
  ]

let db_case name f =
  Alcotest.test_case name `Quick (fun () ->
      match Sys.getenv_opt "EARDE_TEST_DATABASE_URL" with
      | None | Some "" -> Alcotest.skip ()
      | Some url ->
          Lwt_main.run
            (let* conn = Caqti_lwt_unix.connect (Uri.of_string url) in
             let* conn = or_fail "connect" conn in
             let (module C : Caqti_lwt.CONNECTION) = conn in
             let cleanup () =
               Lwt_list.iter_s
                 (fun q ->
                   let* r = C.exec q () in
                   or_fail "cleanup" r)
                 q_cleanup
             in
             let* () = cleanup () in
             Lwt.finalize
               (fun () -> f ~url (module C : Caqti_lwt.CONNECTION))
               (fun () ->
                 List.iter (fun k -> Unix.putenv k "") env_keys;
                 Lwt.finalize cleanup (fun () -> C.disconnect ()))))

let q_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@rtg.invalid', 'x', TRUE) RETURNING id"

let q_community =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
    "INSERT INTO communities (slug, name, visibility) VALUES ($1, $1, $2) \
     RETURNING id"

let q_channel =
  (Caqti_type.(t2 int string) ->! Caqti_type.int)
    "INSERT INTO channels (community_id, slug, name) VALUES ($1, $2, $2) \
     RETURNING id"

let q_member =
  (Caqti_type.(t2 int int) ->. Caqti_type.unit)
    "INSERT INTO community_members (user_id, community_id) VALUES ($1, $2)"

let q_moderator =
  (Caqti_type.(t3 int int string) ->. Caqti_type.unit)
    "INSERT INTO community_moderators (user_id, community_id, role) VALUES \
     ($1, $2, $3)"

let q_generation =
  (Caqti_type.int ->! Caqti_type.int64)
    "SELECT COALESCE((SELECT generation FROM community_realtime_generations \
     WHERE community_id = $1), 0)"

let q_generation_rows =
  (Caqti_type.int ->! Caqti_type.int)
    "SELECT COUNT(*)::int FROM community_realtime_generations WHERE \
     community_id = $1"

let exec (module C : Caqti_lwt.CONNECTION) label q v =
  let* r = C.exec q v in
  or_fail label r

let find (module C : Caqti_lwt.CONNECTION) label q v =
  let* r = C.find q v in
  or_fail label r

let sql (module C : Caqti_lwt.CONNECTION) label (text : string) id =
  let* r = C.exec ((Caqti_type.int ->. Caqti_type.unit) text) id in
  or_fail label r

(* Every change that can take read access from someone bumps; changes that
   only widen access (or change nothing) do not, so ordinary traffic does not
   churn connected clients. *)
let trigger_matrix_case =
  db_case
    "generations: every access-narrowing change bumps, widening changes do not"
    (fun ~url:_ c ->
      let* priv = find c "community" q_community ("rtg-private", "private") in
      let* publ = find c "community" q_community ("rtg-public", "public") in
      let* other = find c "community" q_community ("rtg-other", "private") in
      let* member = find c "user" q_user "rtg_member" in
      let* moderator = find c "user" q_user "rtg_mod" in
      let* stranger = find c "user" q_user "rtg_stranger" in
      let gen id = find c "generation" q_generation id in
      let expect label id want =
        let* g = gen id in
        Alcotest.(check int64) label want g;
        Lwt.return_unit
      in
      let* () = expect "a community starts at generation 0" priv 0L in
      (* Joining and promotion widen access. *)
      let* () = exec c "join" q_member (member, priv) in
      let* () = exec c "join other" q_member (member, other) in
      let* () = exec c "mod" q_moderator (moderator, priv, "mod") in
      let* () = expect "joining and promotion do not bump" priv 0L in
      (* Leaving (or being removed) narrows it. *)
      let* () =
        sql c "leave" "DELETE FROM community_members WHERE community_id = $1"
          priv
      in
      let* () = expect "a membership removal bumps" priv 1L in
      let* () =
        sql c "demote"
          "DELETE FROM community_moderators WHERE community_id = $1" priv
      in
      let* () = expect "a moderator removal bumps" priv 2L in
      (* Visibility: only public -> private narrows. *)
      let* () =
        sql c "same"
          "UPDATE communities SET visibility = 'public' WHERE id = $1" publ
      in
      let* () = expect "rewriting the same visibility does not bump" publ 0L in
      let* () =
        sql c "narrow"
          "UPDATE communities SET visibility = 'private' WHERE id = $1" publ
      in
      let* () = expect "public to private bumps" publ 1L in
      let* () =
        sql c "widen"
          "UPDATE communities SET visibility = 'public' WHERE id = $1" publ
      in
      let* () = expect "private to public does not bump" publ 1L in
      (* Account-level losses bump the communities the account could read. *)
      let* () = exec c "rejoin" q_member (member, priv) in
      let* () =
        sql c "ban" "UPDATE users SET is_banned = TRUE WHERE id = $1" member
      in
      let* () = expect "a global ban bumps the member's community" priv 3L in
      let* () = expect "and every other community it belongs to" other 1L in
      let* () =
        sql c "unban" "UPDATE users SET is_banned = FALSE WHERE id = $1" member
      in
      let* () = expect "an unban does not bump" priv 3L in
      let* () =
        sql c "promote admin" "UPDATE users SET is_admin = TRUE WHERE id = $1"
          stranger
      in
      let* () = expect "granting admin does not bump" priv 3L in
      let* () =
        sql c "demote admin" "UPDATE users SET is_admin = FALSE WHERE id = $1"
          stranger
      in
      let* () = expect "losing admin bumps every private community" priv 4L in
      let* () = expect "including one the admin never joined" other 2L in
      let* () = expect "but not a public one" publ 1L in
      let* () =
        sql c "rename"
          "UPDATE users SET username = 'rtg_member_renamed' WHERE id = $1"
          member
      in
      let* () = expect "an ordinary rename does not bump" priv 4L in
      let* () =
        sql c "anonymize"
          "UPDATE users SET username = '[deleted_' || id || ']', email = \
           'deleted_' || id || '@rtg.invalid', password_hash = '' WHERE id = \
           $1"
          member
      in
      let* () =
        expect "account deletion bumps the account's communities" priv 5L
      in
      let* () = expect "all of them" other 3L in
      Lwt.return_unit)

(* The cascaded membership deletes of a community being deleted still fire
   the trigger; it must not try to reference the vanishing row. *)
let community_deletion_case =
  db_case
    "generations: deleting a community with members and moderators still works"
    (fun ~url:_ c ->
      let* cid = find c "community" q_community ("rtg-doomed", "private") in
      let* u = find c "user" q_user "rtg_doomed_member" in
      let* () = exec c "member" q_member (u, cid) in
      let* () = exec c "mod" q_moderator (u, cid, "top_mod") in
      let* () =
        sql c "bump first"
          "DELETE FROM community_moderators WHERE community_id = $1" cid
      in
      let* () = exec c "mod again" q_moderator (u, cid, "top_mod") in
      let* () = sql c "delete" "DELETE FROM communities WHERE id = $1" cid in
      let* rows = find c "rows" q_generation_rows cid in
      Alcotest.(check int) "no generation row survives its community" 0 rows;
      Lwt.return_unit)

(* === routed pipeline with a stub gateway === *)

let next_identity : (int * string) option ref = ref None

let build_pipeline url =
  Dream.sql_pool ~size:4 url
  @@ Dream.set_secret "rtg-test-secret-value"
  @@ Dream.memory_sessions
  @@ Dream.router
       [
         Dream.get "/session" (fun req ->
             let* () =
               match !next_identity with
               | None -> Lwt.return_unit
               | Some (uid, username) ->
                   let* () =
                     Dream.set_session_field req "user_id" (string_of_int uid)
                   in
                   Dream.set_session_field req "username" username
             in
             Dream.respond (Dream.csrf_token req));
         Dream.get "/c/:slug/ch/:channel_slug"
           Earde.Chat_handlers.community_channel_handler;
         Dream.get "/c/:slug/ch/:channel_slug/realtime-token"
           Earde.Chat_handlers.realtime_token_handler;
         Dream.get "/c/:slug/ch/:channel_slug/messages.json"
           Earde.Chat_handlers.channel_messages_json_handler;
         Dream.post "/messages" Earde.Chat_handlers.send_message_handler;
         Dream.post "/leave" Earde.Membership_handlers.leave_community_handler;
       ]

let pipelines : (string, Dream.handler) Hashtbl.t = Hashtbl.create 2

let pipeline url =
  match Hashtbl.find_opt pipelines url with
  | Some p -> p
  | None ->
      let p = build_pipeline url in
      Hashtbl.replace pipelines url p;
      p

let session_cookie response =
  match
    List.find_opt
      (fun v -> contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | Some v -> (
      match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)
  | None -> Alcotest.fail "no session cookie"

let login ~url uid username =
  next_identity := Some (uid, username);
  let* response =
    (pipeline url) (Dream.request ~method_:`GET ~target:"/session" "")
  in
  next_identity := None;
  let cookie = session_cookie response in
  let* csrf = Dream.body response in
  Lwt.return (cookie, csrf)

let get ~url ~cookie target =
  let* response =
    (pipeline url)
      (Dream.request ~method_:`GET ~target ~headers:[ ("Cookie", cookie) ] "")
  in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

let post ~url ~cookie ~csrf ?(headers = []) target fields =
  let form =
    String.concat "&"
      (List.map
         (fun (k, v) ->
           Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
         (("dream.csrf", csrf) :: fields))
  in
  let request =
    Dream.request ~method_:`POST ~target
      ~headers:
        ([
           ("Content-Type", "application/x-www-form-urlencoded");
           ("Cookie", cookie);
         ]
        @ headers)
      form
  in
  let* response = (pipeline url) request in
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), body)

let token_topic token =
  match Earde.Realtime_token.decode_payload token with
  | Some (`Assoc fields) -> (
      match List.assoc_opt "topic" fields with
      | Some (`String t) -> t
      | _ -> Alcotest.fail "no topic")
  | _ -> Alcotest.fail "undecodable token"

let page_token page =
  let key = "data-signed-token=" in
  let n = String.length page and k = String.length key in
  let rec find i =
    if i + k > n then Alcotest.fail "no signed token on the page"
    else if String.sub page i k = key then i + k
    else find (i + 1)
  in
  let start = find 0 in
  let quote = page.[start] in
  let stop = String.index_from page (start + 1) quote in
  String.sub page (start + 1) (stop - start - 1)

let refreshed_topic ~url ~cookie target =
  let* status, body = get ~url ~cookie target in
  if status <> 200 then Lwt.return (Error status)
  else
    match Yojson.Safe.from_string body with
    | `Assoc fields -> (
        match List.assoc_opt "token" fields with
        | Some (`String t) -> Lwt.return (Ok (token_topic t))
        | _ -> Alcotest.fail "no token in refresh body")
    | _ -> Alcotest.fail "refresh body is not an object"

(* The gateway's internal publish endpoint, reduced to a recorder of the
   topics Dream publishes to. *)
let with_stub_gateway f =
  let topics = ref [] in
  let callback _conn _request body =
    let* text = Cohttp_lwt.Body.to_string body in
    (match Yojson.Safe.from_string text with
    | `Assoc fields -> (
        match List.assoc_opt "topic" fields with
        | Some (`String t) -> topics := !topics @ [ t ]
        | _ -> ())
    | _ -> ());
    Cohttp_lwt_unix.Server.respond_string ~status:`Accepted ~body:"published" ()
  in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname sock with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> assert false
  in
  Unix.close sock;
  let stop, stopper = Lwt.wait () in
  Lwt.async (fun () ->
      Cohttp_lwt_unix.Server.create ~stop
        ~mode:(`TCP (`Port port))
        (Cohttp_lwt_unix.Server.make ~callback ()));
  let* () = Lwt_unix.sleep 0.05 in
  Unix.putenv "REALTIME_GATEWAY_URL" (Printf.sprintf "http://127.0.0.1:%d" port);
  Unix.putenv "REALTIME_INTERNAL_SECRET" "rtg-internal-secret";
  Unix.putenv "REALTIME_TOKEN_SECRET" "rtg-token-secret";
  Unix.putenv "REALTIME_SOCKET_URL" "ws://127.0.0.1:9/socket";
  Lwt.finalize
    (fun () -> f topics)
    (fun () ->
      Lwt.wakeup_later stopper ();
      Lwt.return_unit)

(* The publish runs after the response; wait until it has been recorded. *)
let next_publish topics before =
  let rec wait n =
    if List.length !topics > before then Lwt.return (List.nth !topics before)
    else if n > 200 then Alcotest.fail "no publish reached the stub gateway"
    else
      let* () = Lwt_unix.sleep 0.01 in
      wait (n + 1)
  in
  wait 0

let send ~url ~cookie ~csrf topics content =
  let before = List.length !topics in
  let* status, _ =
    post ~url ~cookie ~csrf
      ~headers:[ ("Accept", "application/json") ]
      "/messages"
      [
        ("community_slug", "rtg-lab");
        ("channel_slug", "rtg-room");
        ("content", content);
      ]
  in
  Alcotest.(check int) ("send " ^ content) 200 status;
  next_publish topics before

(* The reproduction: a member's socket is authorized by a token for the
   channel. The member leaves. Before this change the next message was still
   published to the topic that token names, so the open socket kept
   receiving the private community's messages until the token expired. *)
let leave_case =
  db_case
    "revocation: a connected member who leaves a private community is not sent \
     later messages" (fun ~url c ->
      with_stub_gateway (fun topics ->
          let* cid = find c "community" q_community ("rtg-lab", "private") in
          let* chan = find c "channel" q_channel (cid, "rtg-room") in
          let* leaver = find c "user" q_user "rtg_leaver" in
          let* sender = find c "user" q_user "rtg_sender" in
          let* stayer = find c "user" q_user "rtg_stayer" in
          let* () = exec c "m1" q_member (leaver, cid) in
          let* () = exec c "m2" q_member (sender, cid) in
          let* () = exec c "m3" q_member (stayer, cid) in
          let* leaver_cookie, leaver_csrf = login ~url leaver "rtg_leaver" in
          let* sender_cookie, sender_csrf = login ~url sender "rtg_sender" in
          let* stayer_cookie, _ = login ~url stayer "rtg_stayer" in
          let refresh = "/c/rtg-lab/ch/rtg-room/realtime-token" in
          (* The page itself mints the token the socket connects with. *)
          let* status, page =
            get ~url ~cookie:leaver_cookie "/c/rtg-lab/ch/rtg-room"
          in
          Alcotest.(check int) "channel page" 200 status;
          (* The topic the member's open socket is subscribed to. *)
          let initial = token_topic (page_token page) in
          let* leaver_topic =
            refreshed_topic ~url ~cookie:leaver_cookie refresh
          in
          Alcotest.(check (result string int))
            "a refresh names the same topic" (Ok initial) leaver_topic;
          (* Control: while a member, the leaver's topic receives messages. *)
          let* t1 =
            send ~url ~cookie:sender_cookie ~csrf:sender_csrf topics
              "rtg before"
          in
          Alcotest.(check string)
            "published where the member listens" initial t1;
          (* The routed leave handler. *)
          let* status, _ =
            post ~url ~cookie:leaver_cookie ~csrf:leaver_csrf "/leave"
              [ ("community_id", string_of_int cid); ("redirect_to", "/") ]
          in
          Alcotest.(check bool) "left" true (status = 303 || status = 200);
          let* t2 =
            send ~url ~cookie:sender_cookie ~csrf:sender_csrf topics "rtg after"
          in
          Alcotest.(check bool)
            "a later message is not published to the leaver's topic" true
            (t2 <> initial);
          Alcotest.(check string)
            "the page named generation 0"
            (Printf.sprintf "chan:%d:0" chan)
            initial;
          Alcotest.(check string)
            "it goes to the next generation"
            (Printf.sprintf "chan:%d:1" chan)
            t2;
          (* The leaver cannot follow: refresh re-checks access. *)
          let* again = refreshed_topic ~url ~cookie:leaver_cookie refresh in
          Alcotest.(check bool)
            "the leaver's refresh is refused" true
            (match again with Error s -> s = 404 | Ok _ -> false);
          (* A remaining member follows the move with an ordinary refresh. *)
          let* stayer_topic =
            refreshed_topic ~url ~cookie:stayer_cookie refresh
          in
          Alcotest.(check (result string int))
            "the remaining member's refreshed topic" (Ok t2) stayer_topic;
          Lwt.return_unit))

(* A viewer of a public community holds a token for it; the community turns
   private. Their open socket must not get the now-private messages. *)
let visibility_case =
  db_case
    "revocation: a non-member's socket gets nothing once a public community \
     turns private" (fun ~url c ->
      with_stub_gateway (fun topics ->
          let* cid = find c "community" q_community ("rtg-lab", "public") in
          let* chan = find c "channel" q_channel (cid, "rtg-room") in
          let* viewer = find c "user" q_user "rtg_viewer" in
          let* sender = find c "user" q_user "rtg_sender" in
          let* () = exec c "member" q_member (sender, cid) in
          let* viewer_cookie, _ = login ~url viewer "rtg_viewer" in
          let* sender_cookie, sender_csrf = login ~url sender "rtg_sender" in
          let refresh = "/c/rtg-lab/ch/rtg-room/realtime-token" in
          let* viewer_topic =
            refreshed_topic ~url ~cookie:viewer_cookie refresh
          in
          let initial =
            match viewer_topic with
            | Ok t -> t
            | Error s -> Alcotest.failf "refresh %d" s
          in
          let* () =
            sql c "narrow"
              "UPDATE communities SET visibility = 'private' WHERE id = $1" cid
          in
          let* t =
            send ~url ~cookie:sender_cookie ~csrf:sender_csrf topics
              "rtg now private"
          in
          Alcotest.(check bool)
            "not published to the viewer's topic" true (t <> initial);
          Alcotest.(check string)
            "the viewer held generation 0"
            (Printf.sprintf "chan:%d:0" chan)
            initial;
          let* again = refreshed_topic ~url ~cookie:viewer_cookie refresh in
          Alcotest.(check bool)
            "the viewer's refresh is refused" true
            (match again with Error s -> s = 404 | Ok _ -> false);
          Lwt.return_unit))

(* === gateway failure === *)

(* A gateway Dream cannot use: nothing listening, a listener that answers
   every publish with 500, or one that accepts and never answers. *)
type broken_gateway = Down | Failing | Hanging

let with_broken_gateway kind f =
  let listen () =
    let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
    let* () =
      Lwt_unix.bind sock (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 0))
    in
    Lwt_unix.listen sock 16;
    match Lwt_unix.getsockname sock with
    | Unix.ADDR_INET (_, port) -> Lwt.return (sock, port)
    | _ -> assert false
  in
  let rec serve sock reply =
    let* client, _ = Lwt_unix.accept sock in
    Lwt.async (fun () -> reply client);
    serve sock reply
  in
  let* port, stop =
    match kind with
    | Down ->
        (* Bound to learn a free port, then closed: connections are refused. *)
        let* sock, port = listen () in
        let* () = Lwt_unix.close sock in
        Lwt.return (port, fun () -> Lwt.return_unit)
    | Failing | Hanging ->
        let* sock, port = listen () in
        let reply client =
          match kind with
          | Failing ->
              let* _ = Lwt_unix.read client (Bytes.create 65536) 0 65536 in
              let answer =
                "HTTP/1.1 500 Internal Server Error\r\n\
                 Content-Length: 0\r\n\
                 Connection: close\r\n\
                 \r\n"
              in
              let* _ =
                Lwt_unix.write_string client answer 0 (String.length answer)
              in
              Lwt_unix.close client
          | _ -> fst (Lwt.wait ())
        in
        let server = serve sock reply in
        Lwt.return
          ( port,
            fun () ->
              Lwt.cancel server;
              Lwt_unix.close sock )
  in
  Unix.putenv "REALTIME_GATEWAY_URL" (Printf.sprintf "http://127.0.0.1:%d" port);
  Unix.putenv "REALTIME_INTERNAL_SECRET" "rtg-internal-secret";
  Unix.putenv "REALTIME_TOKEN_SECRET" "rtg-token-secret";
  Unix.putenv "REALTIME_SOCKET_URL" "ws://127.0.0.1:9/socket";
  Lwt.finalize f stop

let q_message_count =
  (Caqti_type.(t2 int string) ->! Caqti_type.int)
    "SELECT count(*)::int FROM chat_messages WHERE channel_id = $1 AND content \
     = $2"

let json_message_contents body =
  match Yojson.Safe.from_string body with
  | `List items | `Assoc [ (_, `List items) ] ->
      List.filter_map
        (function
          | `Assoc fields -> (
              match List.assoc_opt "content" fields with
              | Some (`String c) -> Some c
              | _ -> None)
          | _ -> None)
        items
  | _ -> Alcotest.failf "unexpected catch-up body: %s" body

let gateway_failure_case kind label =
  db_case
    (Printf.sprintf "gateway %s: a sent message commits and catch-up serves it"
       label) (fun ~url c ->
      with_broken_gateway kind (fun () ->
          let* cid = find c "community" q_community ("rtg-lab", "private") in
          let* chan = find c "channel" q_channel (cid, "rtg-room") in
          let* sender = find c "user" q_user "rtg_sender" in
          let* reader = find c "user" q_user "rtg_reader" in
          let* () = exec c "m1" q_member (sender, cid) in
          let* () = exec c "m2" q_member (reader, cid) in
          let* cookie, csrf = login ~url sender "rtg_sender" in
          let* reader_cookie, _ = login ~url reader "rtg_reader" in
          let content = "rtg while the gateway is " ^ label in
          let* status, _ =
            post ~url ~cookie ~csrf
              ~headers:[ ("Accept", "application/json") ]
              "/messages"
              [
                ("community_slug", "rtg-lab");
                ("channel_slug", "rtg-room");
                ("content", content);
              ]
          in
          Alcotest.(check int) "the send succeeds" 200 status;
          let* stored = find c "stored" q_message_count (chan, content) in
          Alcotest.(check int) "the message is committed once" 1 stored;
          (* What a reconnecting client fetches. *)
          let* status, body =
            get ~url ~cookie:reader_cookie
              "/c/rtg-lab/ch/rtg-room/messages.json?after_id=0"
          in
          Alcotest.(check int) "catch-up" 200 status;
          Alcotest.(check (list string))
            "catch-up serves the message" [ content ]
            (json_message_contents body);
          Lwt.return_unit))

let db_suite =
  [ trigger_matrix_case; community_deletion_case; leave_case; visibility_case ]

let gateway_failure_suite =
  [
    gateway_failure_case Down "down";
    gateway_failure_case Failing "failing";
    gateway_failure_case Hanging "hanging";
  ]

let suites =
  [
    ("realtime_generations", pure_suite);
    ("realtime_generations_db", db_suite);
    ("realtime_gateway_failure_db", gateway_failure_suite);
  ]
