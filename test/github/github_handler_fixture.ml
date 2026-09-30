(* GitHub onboarding handlers under test: the start and setup-return
   factories, their pipelines and the state they leave behind. *)

module Ob = Earde.Project_onboarding
module GOC = Earde.Github_onboarding_crypto
module GCK = Earde.Github_onboarding_cookie

let ( let* ) = Lwt.bind

open Caqti_request.Infix

let target = "/integrations/github/install/start"

let make_start_handler ~mode ~load_config =
  Earde.Github_onboarding_handlers.make_start_installation_handler ~mode
    ~load_config

(* Only the per-flow cookies; memory_sessions appends its own session
   Set-Cookie, which is not under test here. *)
let flow_cookies response =
  List.filter
    (fun header ->
      Html_assert.contains_nonempty ~needle:Github_fixture.gsd_cookie_prefix
        header)
    (Dream.headers response "Set-Cookie")

(* DB-free run. [session = None] means no session middleware at all — the
   handler must read that as anonymous, exactly like production before
   login. *)
let gate_run ?session ?(headers = []) ~mode ~load_config () =
  let handler = make_start_handler ~mode ~load_config in
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
  let request = Dream.request ~method_:`POST ~target ~headers "" in
  match Lwt_main.run (pipeline request) with
  | response -> `Response response
  | exception _ -> `Db_boundary

let or_fail label = function
  | Ok v -> Lwt.return v
  | Error e -> Alcotest.failf "%s: %s" label (Caqti_error.show e)

let q_insert_user =
  (Caqti_type.string ->! Caqti_type.int)
    "INSERT INTO users (username, email, password_hash, is_email_verified)\n\
    \   VALUES ($1, $1 || '@test.invalid', 'x', TRUE) RETURNING id"

(* Lwt variant of gck_loaded: db cases already run inside Lwt_main.run,
   so the browser simulation must compose instead of nesting run. *)
let load_cookie config state jar =
  let result = ref None in
  let* (_ : Dream.response) =
    Dream.set_secret Github_fixture.cookie_secret
      (fun request ->
        result := Some (GCK.load config ~request ~state);
        Dream.respond "")
      (Dream.request
         ~headers:[ ("Cookie", Github_fixture.gck_cookie_header jar) ]
         "")
  in
  match !result with
  | Some (Ok data) -> Lwt.return data
  | Some (Error GCK.Missing) -> Alcotest.fail "cookie load: Missing"
  | Some (Error GCK.Invalid) -> Alcotest.fail "cookie load: Invalid"
  | None -> Alcotest.fail "secret middleware did not run the loader"

(* Real pipeline for one authenticated same-origin POST. gck_secret keeps
   the encrypted cookie interoperable with the load_cookie simulation. *)
let run_start ~url ~session_user_id =
  let handler =
    make_start_handler ~mode:Ob.Public ~load_config:(fun () ->
        Http_fixture.ok_loader ())
  in
  let pipeline =
    Dream.sql_pool url
    @@ Dream.set_secret Github_fixture.cookie_secret
    @@ Dream.memory_sessions
    @@ fun req ->
    let* () =
      Dream.set_session_field req "user_id" (string_of_int session_user_id)
    in
    handler req
  in
  pipeline
    (Dream.request ~method_:`POST ~target
       ~headers:[ ("Origin", "https://earde.com") ]
       "")

(* Parses one successful start: exact 303, the state from the Location
   query, and the single per-flow Set-Cookie pair. *)
let successful_start label response =
  Alcotest.(check int)
    (label ^ ": exactly 303") 303
    (Http_fixture.status_of response);
  let location =
    match Dream.header response "Location" with
    | Some l -> l
    | None -> Alcotest.fail (label ^ ": no Location header")
  in
  let uri = Uri.of_string location in
  let raw_state =
    match Uri.get_query_param uri "state" with
    | Some s -> s
    | None -> Alcotest.fail (label ^ ": no state parameter")
  in
  let state =
    match GOC.state_of_callback raw_state with
    | Ok s -> s
    | Error GOC.Invalid_format ->
        Alcotest.fail (label ^ ": state parameter is not canonical")
  in
  let name, value, _ =
    Github_fixture.gck_single_set_cookie
      (label ^ ": per-flow cookie")
      (flow_cookies response)
  in
  (location, state, name, value)

let path = "/integrations/github/install/return"

let make_setup_return_handler ~mode ~load_config =
  Earde.Github_onboarding_handlers.make_setup_return_handler ~mode ~load_config

let target_of params = path ^ "?" ^ String.concat "&" params

let check_safety_headers label response =
  List.iter
    (fun (name, expected) ->
      Alcotest.(check (option string))
        (label ^ ": " ^ name)
        (Some expected)
        (Dream.header response name))
    [
      ("Cache-Control", "no-store");
      ("Pragma", "no-cache");
      ("Referrer-Policy", "no-referrer");
    ]

(* One deletion Set-Cookie under exactly the flow's browser-visible name,
   already expired, carrying no material. *)
let check_deletion label ~cookie_name ~stored_value response =
  let name, value, attributes =
    Github_fixture.gck_single_set_cookie
      (label ^ ": deletion cookie")
      (Dream.headers response "Set-Cookie")
  in
  Alcotest.(check string)
    (label ^ ": deletion targets the flow cookie")
    cookie_name name;
  Alcotest.(check bool)
    (label ^ ": expired") true
    (match List.assoc_opt "expires" attributes with
    | Some date -> Html_assert.contains_nonempty ~needle:"1970" date
    | None -> List.assoc_opt "max-age" attributes = Some "0");
  Alcotest.(check bool)
    (label ^ ": no material in the deletion")
    false
    (Html_assert.contains_nonempty ~needle:stored_value value)

(* The schema CHECKs expires_at > created_at, so an expired fixture must
   backdate both. *)
let q_expire =
  (Caqti_type.string ->. Caqti_type.unit)
    "UPDATE github_onboarding_states\n\
    \   SET created_at = NOW() - INTERVAL '16 minutes',\n\
    \       expires_at = NOW() - INTERVAL '1 minute'\n\
    \   WHERE state_hash = $1"

let q_consume_now =
  (Caqti_type.string ->. Caqti_type.unit)
    "UPDATE github_onboarding_states SET consumed_at = NOW()\n\
    \   WHERE state_hash = $1"

let state_hash_of state = GOC.state_hash_to_string (GOC.hash_state state)
