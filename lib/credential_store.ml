open Lwt.Infix

(* Durable revocation of Dream's SQL-backed sessions.

   Dream.invalidate_session only ends the session carrying the CURRENT
   request, so "delete my account" and "reset my password" both left every
   other logged-in browser (or a stolen cookie) fully authenticated: the
   session row survived, and every authorization path in the app keys off
   Dream.session_field "user_id", which still resolved. A deleted account
   could go on commenting, voting, chatting and moderating for the remaining
   session lifetime, under a tombstoned name.

   The row shape is Dream's own (see the init migration, which mirrors it
   deliberately): payload is the JSON object Dream serializes from the
   session dictionary, so the user id is payload->>'user_id' — a STRING,
   because Dream's payload is (string * string) list. Matching on that field
   rather than on a username or email is what makes this survive
   anonymization, which rewrites both of those. *)
(* Compared as text on both sides: casting the column to int would fail the
   whole statement on any session whose payload holds a non-numeric
   user_id, and there is no such row today only by convention. *)
let delete_for_user_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->. Caqti_type.unit)
  "DELETE FROM dream_session WHERE payload::jsonb ->> 'user_id' = $1"

let delete_for_user (module C : Caqti_lwt.CONNECTION) user_id =
  C.exec delete_for_user_query (string_of_int user_id) >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error err -> Lwt.return (Error (Caqti_error.show err))

let update_password_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->. Caqti_type.unit)
  "UPDATE users SET password_hash = $1 WHERE id = $2"

(* An authenticated password change carries the same invariant as
   reset_password_atomically: the new password must end every session the
   user already has — otherwise a hijacked browser survives the change.
   Hash write and session revocation commit together or not at all, so
   there is no window with a new password but a live stolen session (nor
   the reverse). Argon2 hashing must be done BEFORE calling this so no CPU
   work stalls the transaction. *)
(* A reset link issued before the password changed must not be able to
   change it again: the new password is the account's recovery point. *)
let delete_reset_tokens_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM password_resets WHERE user_id = $1"

let update_password_revoking_sessions (module C: Caqti_lwt.CONNECTION) user_id new_hash =
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () ->
    (C.exec update_password_query (new_hash, user_id) >>= function
    | Error e ->
        C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
    | Ok () ->
        (C.exec delete_reset_tokens_query user_id >>= function
        | Error e ->
            C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
        | Ok () ->
        (delete_for_user (module C) user_id >>= function
         | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
         | Ok () ->
             C.commit () >>= function
             | Error e -> Lwt.return (Error (Caqti_error.show e))
             | Ok () -> Lwt.return (Ok ()))))

(* UPDATE+RETURNING atomically consumes the token — avoids TOCTOU race of a separate
   SELECT then UPDATE, and prevents replay on concurrent verification attempts. *)
let verify_email_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.string)
  "UPDATE users SET is_email_verified = TRUE, verification_token = NULL WHERE verification_token = $1 RETURNING username"

let verify_email (module C: Caqti_lwt.CONNECTION) token =
  C.find_opt verify_email_query token >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

(* DB stores only the SHA-256 hex digest of the raw token — a stolen
   password_resets row cannot be used directly; the attacker also needs the
   raw token that was emailed. Same principle as password hashing. *)
let hash_token raw = Digestif.SHA256.(digest_string raw |> to_hex)

(* INSERT...SELECT atomically creates the token iff the email maps to a live user.
   RETURNING distinguishes "email not found" from "inserted" without a second SELECT,
   avoiding TOCTOU between the lookup and the insert. *)
let create_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string string) ->? Caqti_type.int)
  "INSERT INTO password_resets (token, user_id, expires_at)
   SELECT $1, id, NOW() + INTERVAL '2 hours' FROM users WHERE email = $2
   RETURNING user_id"

let create_token (module C : Caqti_lwt.CONNECTION) email raw_token =
  let token_hash = hash_token raw_token in
  C.find_opt create_query (token_hash, email) >>= function
  | Ok res -> Lwt.return (Ok (Option.is_some res))
  | Error e -> Lwt.return (Error (Caqti_error.show e))

(* expires_at > NOW() makes tokens inert after 2h with no background job required. *)
let validate_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.int)
  "SELECT user_id FROM password_resets WHERE token = $1 AND expires_at > NOW()"

let validate_token (module C : Caqti_lwt.CONNECTION) raw_token =
  let token_hash = hash_token raw_token in
  C.find_opt validate_query token_hash >>= function
  | Ok res -> Lwt.return (Ok res)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let consume_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.int)
  "DELETE FROM password_resets WHERE token = $1 AND expires_at > NOW() RETURNING user_id"

(* Every other outstanding link for the account dies with the one used: a
   reset is the remedy for a compromised account, and an older link (say,
   one an attacker read from the mailbox) must not be able to undo it. *)
let consume_others_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "DELETE FROM password_resets WHERE user_id = $1"

(* Separate query to avoid a cross-module reference to Security.update_password_query. *)
let update_pw_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string int) ->. Caqti_type.unit)
  "UPDATE users SET password_hash = $1 WHERE id = $2"

(* Wraps DELETE+UPDATE in a single transaction so that if the UPDATE fails the
   token is rolled back — user retains the reset link rather than being locked out.
   Argon2 hashing must be done BEFORE calling this so no CPU work stalls the txn. *)
let reset_password_atomically (module C : Caqti_lwt.CONNECTION) raw_token new_hash =
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () ->
    (C.find_opt consume_query (hash_token raw_token) >>= function
    | Error e ->
        C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
    | Ok None ->
        (* Token not found or expired — nothing to roll back. *)
        C.rollback () >>= fun _ -> Lwt.return (Ok false)
    | Ok (Some user_id) ->
        (C.exec update_pw_query (new_hash, user_id) >>= function
        | Error e ->
            C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
        | Ok () ->
        (C.exec consume_others_query user_id >>= function
        | Error e ->
            C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
        | Ok () ->
            (* A password reset is the remedy for a compromised account, so
               it must also end the attacker's sessions — otherwise the new
               password changes nothing for whoever already holds a cookie.
               Same transaction as the token consumption and the password
               write: either all three land or none does. *)
            (delete_for_user (module C) user_id >>= function
             | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error e)
             | Ok () ->
                 C.commit () >>= function
                 | Error e -> Lwt.return (Error (Caqti_error.show e))
                 | Ok () -> Lwt.return (Ok true)))))
