(* Running the /bring handler and reading its page. *)

module Ob = Earde.Project_onboarding
let ( let* ) = Lwt.bind

let bring_case name f = Alcotest.test_case name `Quick f

let run ?(session = []) ?(mode = Ob.Public) ?(target = "/bring") () =
  let handler = Earde.Github_onboarding_handlers.make_bring_handler ~mode in
  let pipeline =
    Dream.memory_sessions (fun req ->
        let* () =
          Lwt_list.iter_s
            (fun (k, v) -> Dream.set_session_field req k v)
            session
        in
        handler req)
  in
  Lwt_main.run (pipeline (Dream.request ~method_:`GET ~target ""))

let body_of response = Lwt_main.run (Dream.body response)

let status_of response = Dream.status_to_int (Dream.status response)

let member = [ ("user_id", "42"); ("username", "alice") ]

let admin = member @ [ ("is_admin", "true") ]

let login_copy = "An Earde account is required to connect a project"

let start_action = "action='/integrations/github/install/start'"

let button_copy = "Connect a GitHub project"

let count_occurrences haystack needle =
  let nl = String.length needle in
  let rec loop from acc =
    if from + nl > String.length haystack then acc
    else if String.sub haystack from nl = needle then loop (from + nl) (acc + 1)
    else loop (from + 1) acc
  in
  loop 0 0

(* Every /bring state is a normal, uncacheable 200 carrying the shared
   form-page referrer policy — the page hosts the start form, so it may
   never be "no-referrer" (see Browser_form_post below). *)
let check_page label response =
  Alcotest.(check int) (label ^ ": 200") 200 (status_of response);
  Alcotest.(check (option string)) (label ^ ": no-store") (Some "no-store")
    (Dream.header response "Cache-Control");
  Alcotest.(check (option string))
    (label ^ ": referrer policy")
    (Some Earde.Request_origin.referrer_policy)
    (Dream.header response "Referrer-Policy")

(* One representative session/mode pair per user-facing state. *)
let all_states =
  [ ("off", admin, Ob.Off)
  ; ("anonymous", [], Ob.Public)
  ; ("rollout-limited", member, Ob.Admins)
  ; ("ready", member, Ob.Public)
  ; ("admin ready", admin, Ob.Admins)
  ]
