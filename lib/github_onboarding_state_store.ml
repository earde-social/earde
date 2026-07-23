(* Issuing GitHub onboarding callback states. The insert receives only hash
   material and the closed flow string; expiry authority is Postgres NOW()
   (parameterized by ttl_seconds), so state lifetime cannot drift with the
   application clock. Issuance never touches earlier states: multiple tabs
   may each hold their own single-use state, and consumption/cleanup are
   separate slices. *)

open Lwt.Infix

type issue_error =
  | Invalid_user_id
  | Storage_error

let ttl_seconds = 900

(* Pure precheck, before any SQL: onboarding states only exist for
   authenticated users with real ids. *)
let valid_user_id user_id = user_id > 0

let insert_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t4 string int string string) int) ->. Caqti_type.unit)
  "INSERT INTO github_onboarding_states \
     (state_hash, user_id, session_binding_hash, flow, expires_at) \
   VALUES ($1, $2, $3, $4, NOW() + $5 * INTERVAL '1 second')"

let issue (module C : Caqti_lwt.CONNECTION) ~user_id ~session_binding_hash
    ~flow =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else
    let state = Github_onboarding_crypto.generate_state () in
    let state_hash =
      Github_onboarding_crypto.state_hash_to_string
        (Github_onboarding_crypto.hash_state state)
    in
    let session_binding_hash =
      Github_onboarding_crypto.session_binding_hash_to_string
        session_binding_hash
    in
    let flow = Github_onboarding.string_of_flow flow in
    C.exec insert_state_query
      ((state_hash, user_id, session_binding_hash, flow), ttl_seconds)
    >>= function
    | Ok () -> Lwt.return (Ok state)
    | Error _ ->
        (* Caqti error payloads can echo SQL parameters (the hashes);
           dropping them keeps that material out of callers and logs. *)
        Lwt.return (Error Storage_error)
