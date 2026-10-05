(** Credential lifecycle: session revocation, authenticated password change,
    e-mail verification, and password-reset links. Every operation that changes
    how an account authenticates revokes the account's other sessions in the
    same transaction. *)

val delete_for_user :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
(** Durably revokes EVERY Dream SQL-backed session belonging to [user_id], by
    matching the user id inside Dream's own serialized session payload — never
    the username or email, both of which account deletion rewrites. Sessions of
    other users are untouched. [Dream.invalidate_session] only ends the session
    on the current request; this is what ends the others.

    Already called inside [Posthog_deletion_job_store.anonymize_and_enqueue],
    [reset_password_atomically], [update_password_revoking_sessions] and
    [Admin_store.ban_user]; exposed for tests and for any future flow that must
    end a user's sessions. *)

val revoke_reset_links :
  (module Caqti_lwt.CONNECTION) -> int -> (unit, string) result Lwt.t
(** Deletes every outstanding password-reset link of [user_id]. Account deletion
    calls it inside its own transaction, after locking the user row. *)

(* Writes the new hash AND revokes every Dream session of that user in one
   transaction (same invariant as password_reset_atomically) — a password
   change must end any session an attacker may already hold. Hash before
   calling; the handler must still explicitly log the changing browser out. *)
val update_password_revoking_sessions :
  (module Caqti_lwt.CONNECTION) -> int -> string -> (unit, string) result Lwt.t
(** [Error] without any write when the account is deleted, including when the
    deletion committed while this change was in flight. *)

val verify_email :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  (string option, string) result Lwt.t

val create_token :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  string ->
  (bool, string) result Lwt.t

val validate_token :
  (module Caqti_lwt.CONNECTION) -> string -> (int option, string) result Lwt.t

(* Ok true = password updated; Ok false = token expired/invalid, or its account
   deleted; Error = DB error.
   Consuming the token, deleting the account's other reset links, writing the
   new hash and revoking that user's Dream sessions all happen in ONE
   transaction: a password reset is the remedy for a compromised account, so
   it must also end whoever already holds a cookie. *)
val reset_password_atomically :
  (module Caqti_lwt.CONNECTION) ->
  string ->
  string ->
  (bool, string) result Lwt.t
