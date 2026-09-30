(* === CURRENT GLOBAL-ADMIN AUTHORITY ===

   Dream's session caches [is_admin] at login and is never refreshed, so an
   operator demoted in [users.is_admin] kept every legacy admin power for the
   remaining life of an open session. Durable authority is the [users] row, and
   that is what every admin boundary below now asks.

   The resolution is deliberately asymmetric, and the asymmetry is the whole
   point of it being affordable: a session that does NOT claim admin cannot be
   an admin under the current claim-writing rules, so ordinary traffic performs
   no extra query at all. Only a request that already claims admin pays one
   primary-key read, and only where admin authority is actually about to be
   used. The accepted cost is the other direction: a freshly PROMOTED admin
   whose session still says [is_admin=false] must log in again before the claim
   opens the durable check. That is the documented behaviour, not an oversight.

   The durable answer is never written back into the session — the goal is
   fresh authority on each privileged request, not a second cache to go
   stale. *)
type current_admin =
  | Current_admin
  | Current_non_admin
  | Current_admin_storage_error of string

(* Session read that also behaves where no session middleware is installed:
   no middleware simply means no authenticated session, never an exception
   escaping an authorization decision. *)
let session_field_safe request name =
  match Dream.session_field request name with
  | exception _ -> None
  | value -> value

(* The session user the durable lookup would be about, and [None] whenever
   there is nothing worth looking up: no admin claim, or a claim carried by a
   session with no valid positive user id — which identifies nobody, so it can
   authorize nobody, and must not become a lookup against some coincidental
   row id either. [None] is therefore also what keeps ordinary traffic free of
   any new query. *)
let admin_claimant request =
  if session_field_safe request "is_admin" <> Some "true" then None
  else
    match
      Option.bind (session_field_safe request "user_id") int_of_string_opt
    with
    | Some uid when uid > 0 -> Some uid
    | _ -> None

let current_admin db request =
  match admin_claimant request with
  | None -> Lwt.return Current_non_admin
  | Some uid -> (
      match%lwt User_store.is_user_admin db uid with
      | Ok true -> Lwt.return Current_admin
      | Ok false -> Lwt.return Current_non_admin
      | Error e -> Lwt.return (Current_admin_storage_error e))

(* The settled answer, for boundaries whose remaining code only needs the
   boolean. [Error] carries the generic-message payload only: the caller owns
   the safe failure response and must grant nothing on that path. *)
let current_admin_bool db request =
  match%lwt current_admin db request with
  | Current_admin -> Lwt.return (Ok true)
  | Current_non_admin -> Lwt.return (Ok false)
  | Current_admin_storage_error e -> Lwt.return (Error e)

(* READ gates only. An unanswerable admin lookup is not admin authority, so it
   collapses to "not an admin" and the caller's own member/moderator policy
   still decides — deliberately NOT a distinguishable failure response, which
   on a private community would be a brand-new existence oracle. A stale
   demoted admin who is also a legitimate member or moderator still passes,
   through that independent policy. *)
let current_admin_read_override db request =
  match%lwt current_admin db request with
  | Current_admin -> Lwt.return true
  | Current_non_admin -> Lwt.return false
  | Current_admin_storage_error e ->
      (* Silent by design towards the client — and therefore invisible to the
         operator unless it is logged here. [db_error_message] is the canonical
         server-side log; the generic string it returns has no reader on this
         path. The [current_admin_bool] callers log the same way through their
         own response, which is why the log does not live in [current_admin]
         itself: that would report every failure twice. *)
      ignore (Handler_support.db_error_message e : string);
      Lwt.return false

(* Admin-only boundaries that decide before they would otherwise touch the
   database at all: a session that does not claim admin is refused without so
   much as a pool checkout.

   These callers answer from [generic_db_error] (or a fixed JSON/redirect)
   rather than from the error payload, so the log belongs here for the same
   reason as above. *)
let current_admin_of_request request =
  match admin_claimant request with
  | None -> Lwt.return Current_non_admin
  | Some _ ->
      let%lwt state = Dream.sql request (fun db -> current_admin db request) in
      (match state with
      | Current_admin_storage_error e ->
          ignore (Handler_support.db_error_message e : string)
      | Current_admin | Current_non_admin -> ());
      Lwt.return state
