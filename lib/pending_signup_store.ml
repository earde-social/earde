open Lwt.Infix

(* Holds unconfirmed signups so a bot/abandoned signup never reaches the users table.
   Only a confirmed (token-clicked) pending becomes a real user. token_hash is the
   SHA-256 of the emailed token — same don't-store-the-raw-credential principle as
   password_resets and argon2 password hashing. *)
let hash_token raw = Digestif.SHA256.(digest_string raw |> to_hex)

(* Best-effort secondary cleanup only — correctness never depends on it, since
   Signup_submission_store.submit already removes collisions. Bounded by the expires_at index. *)
let sweep_expired_query =
  let open Caqti_request.Infix in
  (Caqti_type.unit ->. Caqti_type.unit)
  "DELETE FROM pending_signups WHERE expires_at < NOW() - INTERVAL '1 day'"

let sweep_expired (module C : Caqti_lwt.CONNECTION) =
  C.exec sweep_expired_query () >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error (Caqti_error.show e))

(* FOR UPDATE locks the matched row so two concurrent confirmations of the same
   token can't both insert a user. consumed_at IS NULL excludes replays; expires_at
   > NOW() excludes expired tokens — both surface as `Invalid. *)
let select_pending_query =
  let open Caqti_request.Infix in
  (Caqti_type.string ->? Caqti_type.(t4 int string string string))
  "SELECT id, username, email, password_hash FROM pending_signups
     WHERE token_hash = $1 AND consumed_at IS NULL AND expires_at > NOW()
     FOR UPDATE"

let user_conflict_query =
  let open Caqti_request.Infix in
  (Caqti_type.(t2 string string) ->! Caqti_type.bool)
  "SELECT EXISTS (SELECT 1 FROM users WHERE username = $1 OR email = $2)"

(* is_email_verified TRUE: clicking the link proves the address, so the new user is
   created already-verified; verification_token stays NULL (the legacy /verify column
   is irrelevant to pending-signup users). *)
let insert_user_query =
  let open Caqti_request.Infix in
  (* created_at/is_admin come back from the same RETURNING so the caller has
     the authoritative closed person properties (analytics §4.3) without a
     post-transaction lookup. *)
  (Caqti_type.(t3 string string string ->! t3 int string bool))
  "INSERT INTO users (username, email, password_hash, is_email_verified) VALUES ($1, $2, $3, TRUE) RETURNING id, created_at::text, is_admin"

let mark_consumed_query =
  let open Caqti_request.Infix in
  (Caqti_type.int ->. Caqti_type.unit)
  "UPDATE pending_signups SET consumed_at = NOW() WHERE id = $1"

(* Confirms a pending in one transaction so a token is consumed exactly once:
   find -> re-check users -> insert user -> mark consumed -> commit. Any failure
   rolls the whole thing back. Returns the new user id (from the insert's
   RETURNING, no later lookup) and the username for the success page. *)
let confirm (module C : Caqti_lwt.CONNECTION) token_hash =
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () ->
    (C.find_opt select_pending_query token_hash >>= function
     | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
     | Ok None -> C.rollback () >>= fun _ -> Lwt.return (Ok `Invalid)
     | Ok (Some (id, username, email, password_hash)) ->
       (C.find user_conflict_query (username, email) >>= function
        | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
        | Ok true -> C.rollback () >>= fun _ -> Lwt.return (Ok `Conflict)
        | Ok false ->
          (C.find insert_user_query (username, email, password_hash) >>= function
           | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
           | Ok (user_id, created_at, is_admin) ->
             (C.exec mark_consumed_query id >>= function
              | Error e -> C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
              | Ok () ->
                (C.commit () >>= function
                 | Error e -> Lwt.return (Error (Caqti_error.show e))
                 | Ok () ->
                   Lwt.return
                     (Ok (`Confirmed (user_id, username, email, created_at, is_admin))))))))
