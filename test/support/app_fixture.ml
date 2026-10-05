(* The production route table behind the parts of the bin/main.ml middleware
   stack that routed regressions depend on: client address, target redaction,
   the SQL pool, real SQL sessions and the router itself. Logging, analytics,
   presence and the notification badge are left out; none of them takes part
   in a routing, limiting, session or authorization decision.

   [client] stands in for the address the proxy middleware would resolve, so
   each suite owns its own rate-limit buckets. A browser is a mutable cookie
   jar that follows the session cookie Dream sets or rotates. *)

let ( let* ) = Lwt.bind
let secret = "app-fixture-secret-value"

let app ~url ~client =
  (fun handler request ->
    Dream.set_client request client;
    handler request)
  @@ Earde.Request_target_redaction.redact_middleware
  @@ Dream.sql_pool ~size:4 url @@ Dream.set_secret secret @@ Dream.sql_sessions
  @@ Earde.Request_target_redaction.restore_middleware
  @@ Earde.App_routes.router

type browser = { mutable cookie : string option }

let browser () = { cookie = None }

let remember browser response =
  match
    List.find_opt
      (fun v -> Html_assert.contains v "dream.session")
      (Dream.headers response "Set-Cookie")
  with
  | None -> ()
  | Some v ->
      browser.cookie <-
        Some
          (match String.index_opt v ';' with
          | Some i -> String.sub v 0 i
          | None -> v)

let send app browser request =
  let* response = app request in
  remember browser response;
  let* body = Dream.body response in
  Lwt.return (Dream.status_to_int (Dream.status response), response, body)

let cookie_header browser =
  match browser.cookie with Some c -> [ ("Cookie", c) ] | None -> []

let get app browser target =
  send app browser
    (Dream.request ~method_:`GET ~target ~headers:(cookie_header browser) "")

let post app browser target fields =
  let request =
    Dream.request ~method_:`POST ~target
      ~headers:
        (("Content-Type", "application/x-www-form-urlencoded")
        :: cookie_header browser)
      ""
  in
  Dream.set_body request (Http_fixture.form_body fields);
  send app browser request

(* A CSRF token minted inside the browser's session by a real form page. *)
let csrf_from app browser target =
  let* status, _, body = get app browser target in
  if status <> 200 then Alcotest.failf "%s: status %d" target status;
  Lwt.return (Http_fixture.csrf_of_page target body)

let hash_password password =
  let* h = Earde.Auth.hash_password password in
  match h with
  | Error e -> Alcotest.failf "hash: %s" e
  | Ok h ->
      (* Argon2 output can carry NUL padding the stored column never sees. *)
      Lwt.return
        (match String.index_opt h '\000' with
        | Some i -> String.sub h 0 i
        | None -> h)
