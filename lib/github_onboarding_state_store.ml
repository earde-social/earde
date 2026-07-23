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
  | Invalid_pending_installation_id
  | State_unavailable
  | Storage_error

(* GitHub installation ids are positive; the schema CHECK agrees, so reject
   junk before it reaches SQL. *)
let valid_pending_installation_id id = Int64.compare id 0L > 0

(* One atomic UPDATE carries the whole contract: only a live (unconsumed,
   unexpired) row matching the caller's proof of possession (state, binding,
   flow) is touched, and the NULL-or-equal arm makes an identical retry
   (browser refresh of the setup return) idempotent while never letting a
   different installation id overwrite the first. state_hash is UNIQUE, so
   at most one row can match. There is deliberately no user_id predicate:
   the cross-site setup return from GitHub cannot rely on the SameSite=Strict
   login-session cookie, so authorization rests on the state hash plus the
   session-binding hash proven by the SameSite=Lax per-flow cookie — the
   row's user_id was authoritatively written at issuance and stays immutable.
   Only pending_github_installation_id changes; consumption is a separate
   slice. *)
let attach_pending_installation_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 (t3 string string string) int64) ->? Caqti_type.bool)
  "UPDATE github_onboarding_states \
     SET pending_github_installation_id = $4 \
   WHERE state_hash = $1 \
     AND session_binding_hash = $2 \
     AND flow = $3 \
     AND consumed_at IS NULL \
     AND expires_at > NOW() \
     AND (pending_github_installation_id IS NULL \
          OR pending_github_installation_id = $4) \
   RETURNING TRUE"

let attach_pending_installation (module C : Caqti_lwt.CONNECTION) ~state
    ~session_binding_hash ~flow ~pending_github_installation_id =
  if not (valid_pending_installation_id pending_github_installation_id)
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
      ( (state_hash, session_binding_hash, flow),
        pending_github_installation_id )
    >>= function
    | Ok (Some _) -> Lwt.return (Ok ())
    | Ok None ->
        (* Every zero-row cause (missing, expired, consumed, binding/flow
           mismatch, different id already attached) collapses into one
           error so the store cannot be used as a state-probing oracle. *)
        Lwt.return (Error State_unavailable)
    | Error _ ->
        (* Same rationale as issuance: never surface raw Caqti errors. *)
        Lwt.return (Error Storage_error)

type consumed_state = {
  user_id : int;
  flow : Github_onboarding.flow;
  pending_github_installation_id : int64;
}

type consume_error =
  | Invalid_user_id
  | State_not_found
  | State_expired
  | State_already_consumed
  | User_mismatch
  | Session_binding_mismatch
  | Flow_mismatch
  | Missing_pending_installation
  | Storage_error

(* Locks the single row (state_hash is UNIQUE) for the whole classification,
   so no concurrent consumer can race between reading and burning it. Expiry
   and consumption are computed by Postgres against its own clock — the
   application never compares timestamps. *)
let lock_state_query =
  let open Caqti_request.Infix in
  (Caqti_type.string
   ->? Caqti_type.(t2 (t4 int64 int string string)
                      (t3 (option int64) bool bool)))
  "SELECT id, user_id, session_binding_hash, flow, \
          pending_github_installation_id, \
          expires_at <= NOW() AS expired, \
          consumed_at IS NOT NULL AS consumed \
   FROM github_onboarding_states \
   WHERE state_hash = $1 \
   FOR UPDATE"

(* Keyed on the locked row's primary key and touching only consumed_at: the
   identity/lifecycle columns of a burned row stay intact for audit. *)
let mark_consumed_query =
  let open Caqti_request.Infix in
  (Caqti_type.int64 ->. Caqti_type.unit)
  "UPDATE github_onboarding_states SET consumed_at = NOW() WHERE id = $1"

(* Single-use consumption. The whole operation runs under one transaction
   with the row locked FOR UPDATE; concurrent consumers of the same state
   serialize on that lock, so exactly one can ever see it unconsumed.

   Mismatch classifications (wrong user, wrong binding, wrong flow, missing
   installation id) burn the state before reporting: presenting a real state
   in the wrong context destroys it and forces onboarding to restart, and
   the mismatch error is only returned once that burn has committed. Dead
   states (not found / expired / already consumed) are reported without
   writing anything — an expired row stays for cleanup/audit and a replayed
   row keeps its original consumed_at. *)
let consume (module C : Caqti_lwt.CONNECTION) ~user_id ~state
    ~session_binding_hash ~flow =
  if not (valid_user_id user_id) then Lwt.return (Error Invalid_user_id)
  else
    let state_hash =
      Github_onboarding_crypto.state_hash_to_string
        (Github_onboarding_crypto.hash_state state)
    in
    let session_binding_hash =
      Github_onboarding_crypto.session_binding_hash_to_string
        session_binding_hash
    in
    (* As elsewhere in this module every Caqti error is dropped payload-free;
       rollback failure adds nothing a caller may act on either. *)
    let rollback_to err = C.rollback () >>= fun _ -> Lwt.return (Error err) in
    let burn row_id err =
      C.exec mark_consumed_query row_id >>= function
      | Error _ -> rollback_to Storage_error
      | Ok () -> (
          C.commit () >>= function
          | Error _ -> Lwt.return (Error Storage_error)
          | Ok () -> Lwt.return (Error err))
    in
    C.start () >>= function
    | Error _ -> Lwt.return (Error Storage_error)
    | Ok () -> (
        C.find_opt lock_state_query state_hash >>= function
        | Error _ -> rollback_to Storage_error
        | Ok None -> rollback_to State_not_found
        | Ok
            (Some
              ( (row_id, stored_user_id, stored_binding_hash, stored_flow),
                (pending, expired, consumed) )) -> (
            if consumed then rollback_to State_already_consumed
            else if expired then rollback_to State_expired
            else if stored_user_id <> user_id then burn row_id User_mismatch
            else if not (String.equal stored_binding_hash session_binding_hash)
            then burn row_id Session_binding_mismatch
            else
              (* The CHECK constraint should make a parse failure impossible;
                 if it happens anyway, that is storage corruption, never a
                 reason to substitute a flow. *)
              match Github_onboarding.flow_of_string stored_flow with
              | Error _ -> rollback_to Storage_error
              | Ok parsed_flow -> (
                  if parsed_flow <> flow then burn row_id Flow_mismatch
                  else
                    match pending with
                    | None -> burn row_id Missing_pending_installation
                    | Some pending_id
                      when not (valid_pending_installation_id pending_id) ->
                        (* The schema forbids non-positive ids; a row holding
                           one is corrupt and must not be trusted or burned
                           as a mere mismatch. *)
                        rollback_to Storage_error
                    | Some pending_id -> (
                        C.exec mark_consumed_query row_id >>= function
                        | Error _ -> rollback_to Storage_error
                        | Ok () -> (
                            C.commit () >>= function
                            | Error _ -> Lwt.return (Error Storage_error)
                            | Ok () ->
                                Lwt.return
                                  (Ok
                                     {
                                       user_id = stored_user_id;
                                       flow = parsed_flow;
                                       pending_github_installation_id =
                                         pending_id;
                                     }))))))
