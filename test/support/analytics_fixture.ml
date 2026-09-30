(* Captured PostHog payloads: sinks, consent requests and property
   accessors for the analytics cases. *)

module An = Earde.Analytics
module AnT = Earde.Analytics.For_testing
let ( let* ) = Lwt.bind
open Caqti_request.Infix

let yojson =
  Alcotest.testable
    (fun fmt j -> Format.pp_print_string fmt (Yojson.Safe.to_string j))
    Yojson.Safe.equal

let an_case name f = Alcotest.test_case name `Quick f

let payload_member name = function `Assoc l -> List.assoc_opt name l | _ -> None

let payload_props payload =
  match payload_member "properties" payload with
  | Some (`Assoc l) -> l
  | _ -> []

let prop_keys payload = List.map fst (payload_props payload)

(* Runs f with a test configuration and a collecting sink; always restores the
   module's global state, and returns the captured payloads in order. *)
let with_sink ~enabled f =
  let captured = ref [] in
  (if enabled then AnT.use_enabled_test_configuration ()
   else AnT.use_disabled_test_configuration ());
  AnT.set_capture_sink (fun p -> captured := p :: !captured);
  Fun.protect
    ~finally:(fun () ->
      AnT.clear_capture_sink ();
      AnT.clear_configuration_override ())
    f;
  List.rev !captured

let consent_request = function
  | None -> Dream.request ""
  | Some cookie -> Dream.request ~headers:[ ("Cookie", cookie) ] ""

(* The plaintext consent cookie pair, for the database-gated handler suites
   that drive real requests through a real pipeline. *)
let an_consent_granted = (An.consent_cookie_name, "granted")

let an_consent_denied = (An.consent_cookie_name, "denied")

(* Lwt variant of [with_sink] for the database-gated suites: installs the
   enabled (or disabled) test configuration and a collecting sink around one
   awaited computation, always restores the module's global state, and
   returns the computation's value with the payloads captured in order. The
   sink replaces the HTTP transport entirely, so no request can reach a real
   PostHog project, and capture becomes synchronous — no Lwt.async race
   between a captured event and the assertion that reads it. *)
let with_sink_lwt ?(enabled = true) f =
  let captured = ref [] in
  (if enabled then AnT.use_enabled_test_configuration ()
   else AnT.use_disabled_test_configuration ());
  AnT.set_capture_sink (fun p -> captured := p :: !captured);
  Lwt.map
    (fun value -> (value, List.rev !captured))
    (Lwt.finalize
       (fun () -> f ())
       (fun () ->
         AnT.clear_capture_sink ();
         AnT.clear_configuration_override ();
         Lwt.return_unit))

let an_event_names payloads =
  List.filter_map
    (fun p ->
      match payload_member "event" p with
      | Some (`String n) -> Some n
      | _ -> None)
    payloads

(* Exactly one captured payload, with the exact stable event name, the exact
   distinct id, and exactly [props] on top of the centrally injected
   environment property. Failures report event NAMES only — never a captured
   body, which would print whatever fixture material the payload was built
   next to. *)
let check_single_capture label ~name ~distinct_id ~props payloads =
  match payloads with
  | [ payload ] ->
      Alcotest.(check (option yojson))
        (label ^ ": stable event name")
        (Some (`String name))
        (payload_member "event" payload);
      Alcotest.(check (option yojson))
        (label ^ ": distinct id")
        (Some (`String distinct_id))
        (payload_member "distinct_id" payload);
      Alcotest.(check (option yojson))
        (label ^ ": exact closed properties")
        (Some
           (`Assoc
             (props @ [ ("deployment_environment", `String "development") ])))
        (payload_member "properties" payload)
  | payloads ->
      Alcotest.failf "%s: expected exactly one capture, got %d [%s]" label
        (List.length payloads)
        (String.concat "," (an_event_names payloads))

let check_no_capture label payloads =
  match payloads with
  | [] -> ()
  | payloads ->
      Alcotest.failf "%s: expected no capture, got %d [%s]" label
        (List.length payloads)
        (String.concat "," (an_event_names payloads))

let an_group_key payload =
  match List.assoc_opt "$groups" (payload_props payload) with
  | Some (`Assoc [ ("community", `String key) ]) -> Some key
  | _ -> None

let event_of payload =
  match payload_member "event" payload with
  | Some (`String e) -> e
  | _ -> "<no event>"

let distinct_of payload =
  match payload_member "distinct_id" payload with
  | Some (`String d) -> d
  | _ -> "<no distinct_id>"

let group_set_of payload =
  match List.assoc_opt "$group_set" (payload_props payload) with
  | Some (`Assoc set) -> set
  | _ -> []

let group_key_prop_of payload =
  match List.assoc_opt "$group_key" (payload_props payload) with
  | Some (`String k) -> Some k
  | _ -> None

(* Polls an Lwt predicate until true, failing the test after [timeout]
   seconds — used to await post-response async analytics/deletion chains. *)
let wait_until ~label ?(timeout = 5.0) predicate =
  let ( let* ) = Lwt.bind in
  let rec loop remaining =
    let* ok = predicate () in
    if ok then Lwt.return_unit
    else if remaining <= 0.0 then Alcotest.failf "%s: timed out waiting" label
    else
      let* () = Lwt_unix.sleep 0.05 in
      loop (remaining -. 0.05)
  in
  loop timeout

let q_insert_user =
  (Caqti_type.(t2 string string) ->! Caqti_type.int)
  "INSERT INTO users (username, email, password_hash, is_email_verified)
   VALUES ($1, $1 || '@test.invalid', $2, TRUE) RETURNING id"

(* The admin-gated handlers below authorize on the DURABLE users.is_admin
   row, so a session that merely claims is_admin is not enough to reach the
   analytics behaviour under test: the fixture user has to really be one. *)
let q_make_admin =
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE users SET is_admin = TRUE WHERE id = $1"

let q_username_by_id =
  (Caqti_type.int ->? Caqti_type.string)
  "SELECT username FROM users WHERE id = $1"

let q_delete_user =
  (Caqti_type.int ->. Caqti_type.unit) "DELETE FROM users WHERE id = $1"

let q_delete_job =
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM posthog_person_deletion_jobs WHERE id = $1"

let consent_header = function
  | Some v -> [ ("Cookie", An.consent_cookie_name ^ "=" ^ v) ]
  | None -> []

(* Request-free launch document: the same shared analytics assets without any
   session, for the emission/config cases that must not depend on middleware. *)
let launch_doc ?analytics_community () =
  Earde.Page_shell.launch_app_page ?analytics_community
    ~page_class:"launch-feed" ~title:"T" ~content:(Earde.Html.static "<p>body</p>") ()

(* Full production-shaped raw values (dummy secrets), installable so
   layout/browser-config tests exercise a validated production config. *)
let install_production_config () =
  AnT.install_validated_configuration ~enabled:"true"
    ~environment:"production" ~project_token:"phc_test_token"
    ~api_host:"https://eu.i.posthog.com" ~ui_host:"https://eu.posthog.com"
    ~project_id:"654321" ~personal_api_key:"phx_secret_test_value"
    ~public_origin:"https://earde.com" ()
