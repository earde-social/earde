(* Requests against real handlers and routed pipelines: session and CSRF
   material, form bodies, redirect checks and configuration loaders. *)

let ( let* ) = Lwt.bind

module Pc = Earde.Project_creation_handlers
module Store = Earde.Community_connections_store
module Cc = Earde.Community_connections

let is_redirect status = status / 100 = 3

let encoded_form_body fields =
  String.concat "&"
    (List.map
       (fun (k, v) ->
         Dream.to_percent_encoded k ^ "=" ^ Dream.to_percent_encoded v)
       fields)

let multipart_boundary = "step6boundary"

let multipart_body fields =
  String.concat ""
    (List.map
       (fun (k, v) ->
         Printf.sprintf
           "--%s\r\nContent-Disposition: form-data; name=\"%s\"\r\n\r\n%s\r\n"
           multipart_boundary k v)
       fields)
  ^ Printf.sprintf "--%s--\r\n" multipart_boundary

(* Renders a page renderer that needs a request (CSRF tag / session reads)
   through real session middleware. *)
let with_session_request ?(target = "/") f =
  let rendered = ref "" in
  let (_ : Dream.response) =
    Lwt_main.run
      (Dream.memory_sessions
         (fun req ->
           Lwt.bind (Dream.set_session_field req "user_id" "42") (fun () ->
               rendered := f req;
               Dream.html ""))
         (Dream.request ~method_:`GET ~target ""))
  in
  !rendered

(* Loader that records whether the handler ever asked for configuration. *)
let counting_loader result =
  let calls = ref 0 in
  ( (fun () ->
      incr calls;
      result),
    calls )

let ok_loader () = Github_fixture.gac_of_values ()
let status_of response = Dream.status_to_int (Dream.status response)
let make_setup ~mode = Pc.make_project_home_setup_handler ~mode

let gate_run ?session ?(headers = []) ~method_ ~target handler =
  let pipeline =
    match session with
    | None -> handler
    | Some fields ->
        Dream.memory_sessions (fun req ->
            let* () =
              Lwt_list.iter_s
                (fun (k, v) -> Dream.set_session_field req k v)
                fields
            in
            handler req)
  in
  let request = Dream.request ~method_ ~target ~headers "" in
  match Lwt_main.run (pipeline request) with
  | response -> `Response response
  | exception _ -> `Db_boundary

let gate_response label = function
  | `Response response -> response
  | `Db_boundary -> Alcotest.failf "%s: unexpectedly reached the DB" label

let check_db_boundary label = function
  | `Db_boundary -> ()
  | `Response response ->
      Alcotest.failf "%s: gate rejected with status %d" label
        (status_of response)

let logged_in = [ ("user_id", "42") ]
let admin_session = [ ("user_id", "42"); ("is_admin", "true") ]

let check_clean_redirect label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string))
    (label ^ ": no-referrer") (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  Alcotest.(check string)
    (label ^ ": empty body") ""
    (Lwt_main.run (Dream.body response))

(* Values are percent-encoded, so fixture text with spaces, controls, or
   markup survives the URL-encoded body byte-exactly. *)
let urlencode value =
  let buffer = Buffer.create (String.length value * 2) in
  String.iter
    (fun c ->
      match c with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
          Buffer.add_char buffer c
      | c -> Buffer.add_string buffer (Printf.sprintf "%%%02X" (Char.code c)))
    value;
  Buffer.contents buffer

let form_body fields =
  String.concat "&" (List.map (fun (k, v) -> k ^ "=" ^ urlencode v) fields)

let session_cookie label response =
  match
    List.find_opt
      (fun v -> Html_assert.contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | None -> Alcotest.fail (label ^ ": no session cookie")
  | Some v -> (
      match String.index_opt v ';' with Some i -> String.sub v 0 i | None -> v)

let identity_fields ?(kind = "project") ?(name = "Fixture Project")
    ?(slug = "pch-fixture") ?(description = "") ?(website = "") ?(primary = "")
    ~draft () =
  [
    ("draft_id", draft);
    ("kind", kind);
    ("name", name);
    ("slug", slug);
    ("description", description);
    ("website_url", website);
    ("primary_snapshot_id", primary);
  ]

let mint_tokens label pipeline =
  let response =
    Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target:"/mint" ""))
  in
  let cookie = session_cookie label response in
  match String.split_on_char '\n' (Lwt_main.run (Dream.body response)) with
  | [ fresh; expired ] -> (cookie, fresh, expired)
  | _ -> Alcotest.fail (label ^ ": unexpected mint body")

let do_get ?cookie ~target pipeline =
  let headers = match cookie with Some c -> [ ("Cookie", c) ] | None -> [] in
  let* response = pipeline (Dream.request ~method_:`GET ~target ~headers "") in
  let* body = Dream.body response in
  Lwt.return (response, body)

let check_page label response =
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy")

let csrf_field_marker = "name=\"dream.csrf\" type=\"hidden\" value=\""

let csrf_of_page label html =
  match Html_assert.index_from html csrf_field_marker 0 with
  | None -> Alcotest.fail (label ^ ": no framework CSRF field")
  | Some i -> (
      let start = i + String.length csrf_field_marker in
      match String.index_from_opt html start '"' with
      | None -> Alcotest.fail (label ^ ": unterminated CSRF value")
      | Some e -> String.sub html start (e - start))

let mint_token label ~cookie pipeline =
  let* response, body = do_get ~cookie ~target:"/mint" pipeline in
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  Lwt.return body

let check_redirect_lwt label expected response =
  Alcotest.(check int) (label ^ ": 303") 303 (status_of response);
  Alcotest.(check (option string))
    (label ^ ": Location") (Some expected)
    (Dream.header response "Location");
  Alcotest.(check (option string))
    (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": no-cache") (Some "no-cache")
    (Dream.header response "Pragma");
  Alcotest.(check (option string))
    (label ^ ": no-referrer") (Some "no-referrer")
    (Dream.header response "Referrer-Policy");
  let* body = Dream.body response in
  Alcotest.(check string) (label ^ ": empty body") "" body;
  Lwt.return_unit

(* Durable fixtures go through the real store, exactly as production
   writes them. *)
let request_direct conn ~actor ~requester ~recipient ?note () =
  match
    Cc.create_pending ~requester_community_id:requester
      ~recipient_community_id:recipient ~request_note:note
  with
  | Error _ -> Alcotest.fail "fixture: pure pending value refused"
  | Ok connection -> (
      let* r = Store.request conn ~actor_user_id:actor ~connection in
      match r with
      | Ok created -> Lwt.return (Store.created_connection_id created)
      | Error _ -> Alcotest.fail "fixture: request refused")
