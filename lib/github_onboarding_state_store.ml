(* Issuing GitHub onboarding callback states and attaching the untrusted
   mid-flow installation id. SQL only ever receives hash material and the
   closed flow string; expiry authority is Postgres NOW() (parameterized by
   ttl_seconds at issuance), so state lifetime cannot drift with the
   application clock. Issuance never touches earlier states: multiple tabs
   may each hold their own single-use state, and consumption/cleanup are
   separate slices.

   Constructor ordering: attach_error shares constructor names with
   issue_error, so each operation is defined immediately after its own error
   type to keep bare constructors unambiguous. *)

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

type attach_error =
  | Invalid_user_id
  | Invalid_pending_installation_id
  | State_unavailable
  | Storage_error

(* GitHub installation ids are positive; the schema CHECK agrees, so reject
   junk before it reaches SQL. *)
let valid_pending_installation_id id = Int64.compare id 0L > 0

(* One atomic UPDATE carries the whole contract: only a live (unconsumed,
   unexpired) row under the caller's full identity (state, user, binding,
   flow) is touched, and the NULL-or-equal arm makes an identical retry
   (browser refresh of the setup return) idempotent while never letting a
   different installation id overwrite the first. state_hash is UNIQUE, so
   at most one row can match. Only pending_github_installation_id changes;
   consumption is a separate slice. *)
let attach_pending_installation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t4 string int string string) int64) ->? Caqti_type.bool)
  "UPDATE github_onboarding_states \
     SET pending_github_installation_id = $5 \
   WHERE state_hash = $1 \
     AND user_id = $2 \
     AND session_binding_hash = $3 \
     AND flow = $4 \
     AND consumed_at IS NULL \
     AND expires_at > NOW() \
     AND (pending_github_installation_id IS NULL \
          OR pending_github_installation_id = $5) \
   RETURNING TRUE"

let attach_pending_installation (module C : Caqti_lwt.CONNECTION) ~user_id
    ~state ~session_binding_hash ~flow ~pending_github_installation_id =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else if not (valid_pending_installation_id pending_github_installation_id)
  then Lwt.return (Error Invalid_pending_installation_id)
  else
    let state_hash =
      Github_onboarding_crypto.state_hash_to_string
        (Github_onboarding_crypto.hash_state state)
    in
    let session_binding_hash =
      Github_onboarding_crypto.session_binding_hash_to_string
        session_binding_hash
    in
    let flow = Github_onboarding.string_of_flow flow in
    C.find_opt attach_pending_installation_query
      ( (state_hash, user_id, session_binding_hash, flow),
        pending_github_installation_id )
    >>= function
    | Ok (Some _) -> Lwt.return (Ok ())
    | Ok None ->
        (* Every zero-row cause (missing, expired, consumed, user/binding/
           flow mismatch, different id already attached) collapses into one
           error so the store cannot be used as a state-probing oracle. *)
        Lwt.return (Error State_unavailable)
    | Error _ ->
        (* Same rationale as issuance: never surface raw Caqti errors. *)
        Lwt.return (Error Storage_error)
