(* Dream cookie adapter for one GitHub onboarding flow's private browser
   material: one dedicated encrypted, HttpOnly cookie per onboarding state,
   never the Dream SQL-session dictionary. The name is the state-hash-derived
   [Github_onboarding_session_data.cookie_name] — distinct per state, so
   concurrent flows in one browser stay independent — and the value is the
   encoded session material handed to Dream's encrypted-cookie API as
   plaintext. Scope is pinned to /integrations/github with SameSite=Lax so
   the cookie still returns on GitHub's top-level redirects to the
   install-return and authorize-callback paths but travels nowhere else.
   Every security-relevant attribute is supplied explicitly rather than left
   to Dream's request-based inference, and the Secure/prefix policy is
   derived from the validated [Github_app_config.public_origin] — never from
   forwarding headers — so a TLS-terminating proxy in front of a plain-HTTP
   Dream process still yields production attributes. Nothing here logs
   cookie names, values, or any onboarding material. *)

type load_error = Missing | Invalid

(* Both registered GitHub return paths sit under this prefix; anything
   broader would send the material to unrelated pages. *)
let cookie_path = Some "/integrations/github"

(* Matches the server-side onboarding state TTL: the browser material is
   useless once the state row expires. *)
let max_age = 900.

(* The validated configuration admits only https, or http on approved
   loopback development hosts. Any other scheme means validation was
   bypassed, and the only safe response is to refuse outright rather than
   fall back to a non-Secure production cookie. *)
exception Invalid_public_origin_scheme

type policy = { prefix : [ `Host | `Secure ] option; secure : bool }

(* __Secure-, not __Host-: the __Host- prefix requires Path=/, which would
   conflict with the deliberately narrow integration path. *)
let policy config =
  let origin = Github_app_config.public_origin config in
  if String.starts_with ~prefix:"https://" origin then
    { prefix = Some `Secure; secure = true }
  else if String.starts_with ~prefix:"http://" origin then
    { prefix = None; secure = false }
  else raise Invalid_public_origin_scheme

(* The raw name as the browser sees it — used only to tell a missing cookie
   apart from one that failed decryption, never to read values. *)
let browser_visible_name { prefix; _ } name =
  match prefix with
  | Some `Secure -> "__Secure-" ^ name
  | Some `Host -> "__Host-" ^ name
  | None -> name

let store config ~request ~response ~state data =
  let { prefix; secure } = policy config in
  Dream.set_cookie ~prefix ~encrypt:true ~max_age ~path:cookie_path ~secure
    ~http_only:true ~same_site:(Some `Lax) response request
    (Github_onboarding_session_data.cookie_name state)
    (Github_onboarding_session_data.encode data)

let load config ~request ~state =
  let policy = policy config in
  let name = Github_onboarding_session_data.cookie_name state in
  match
    Dream.cookie ~prefix:policy.prefix ~decrypt:true ~path:cookie_path
      ~secure:policy.secure request name
  with
  | Some plaintext -> (
      match Github_onboarding_session_data.decode plaintext with
      | Ok data -> Ok data
      | Error Github_onboarding_session_data.Invalid_format -> Error Invalid)
  | None ->
      (* Dream collapses "no such cookie" and "cookie failed authenticated
         decryption" into [None]; the raw cookie name list separates the two
         without ever touching a value. *)
      if
        List.mem_assoc
          (browser_visible_name policy name)
          (Dream.all_cookies request)
      then Error Invalid
      else Error Missing

let drop config ~request ~response ~state =
  let { prefix; secure } = policy config in
  Dream.drop_cookie ~prefix ~path:cookie_path ~secure ~http_only:true
    ~same_site:(Some `Lax) response request
    (Github_onboarding_session_data.cookie_name state)
