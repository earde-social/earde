open Caqti_request.Infix

let ( >>= ) = Lwt.bind

type outcome = Username_taken | Pending_created | Not_created

let username_registered_q =
  (Caqti_type.string ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM users WHERE username = $1)"

let email_registered_q =
  (Caqti_type.string ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM users WHERE email = $1)"

(* Only a NON-expired, unconsumed row is a live claim on a username: an
   expired one is a squatter that clear_collisions_q removes, else an
   abandoned signup would block the name forever. A row for the SAME email is
   the owner resubmitting, which is a replace, not a conflict. *)
let foreign_reservation_q =
  (Caqti_type.(t2 string string) ->! Caqti_type.bool)
    "SELECT EXISTS (SELECT 1 FROM pending_signups\n\
    \     WHERE consumed_at IS NULL AND expires_at > NOW()\n\
    \       AND LOWER(username) = LOWER($1) AND LOWER(email) <> LOWER($2))"

(* The partial unique indexes (LOWER(email)/LOWER(username) WHERE consumed_at
   IS NULL) still include expired-but-unconsumed rows, so before inserting,
   clear any row that would collide: this email's prior attempt (active OR
   expired) — a clean resend/replace — and any EXPIRED row squatting the
   username. *)
let clear_collisions_q =
  (Caqti_type.(t2 string string) ->. Caqti_type.unit)
    "DELETE FROM pending_signups\n\
    \     WHERE consumed_at IS NULL\n\
    \       AND ( LOWER(email) = LOWER($1)\n\
    \          OR ( LOWER(username) = LOWER($2) AND expires_at <= NOW() ) )"

(* DO NOTHING (no arbiter) covers every unique index: a concurrent submission
   that committed the same email or username first makes this insert a no-op
   instead of an error, and the caller rolls back its DELETE with it. 24h
   window, as before. *)
let insert_q =
  (Caqti_type.(
     t2 (t4 string string string string) (t2 (option string) (option string)))
  ->? Caqti_type.int64)
    "INSERT INTO pending_signups\n\
    \     (username, email, password_hash, token_hash, expires_at, ip_address, \
     user_agent)\n\
    \   VALUES ($1, $2, $3, $4, NOW() + INTERVAL '24 hours', $5, $6)\n\
    \   ON CONFLICT DO NOTHING\n\
    \   RETURNING id"

let username_registered (module C : Caqti_lwt.CONNECTION) username =
  C.find username_registered_q username >>= function
  | Ok b -> Lwt.return (Ok b)
  | Error e -> Lwt.return (Error (Caqti_error.show e))

let submit (module C : Caqti_lwt.CONNECTION) ~username ~email ~password_hash
    ~token_hash ~ip ~user_agent =
  let fail e =
    C.rollback () >>= fun _ -> Lwt.return (Error (Caqti_error.show e))
  in
  let stop outcome = C.rollback () >>= fun _ -> Lwt.return (Ok outcome) in
  C.start () >>= function
  | Error e -> Lwt.return (Error (Caqti_error.show e))
  | Ok () -> (
      (* Re-checked inside the transaction: a confirmation may have created
         the account since the handler's public pre-check. *)
      C.find username_registered_q username
      >>= function
      | Error e -> fail e
      | Ok true -> stop Username_taken
      | Ok false -> (
          C.find email_registered_q email >>= function
          | Error e -> fail e
          | Ok true -> stop Not_created
          | Ok false -> (
              C.find foreign_reservation_q (username, email) >>= function
              | Error e -> fail e
              | Ok true -> stop Not_created
              | Ok false -> (
                  C.exec clear_collisions_q (email, username) >>= function
                  | Error e -> fail e
                  | Ok () -> (
                      C.find_opt insert_q
                        ( (username, email, password_hash, token_hash),
                          (ip, user_agent) )
                      >>= function
                      | Error e -> fail e
                      | Ok None -> stop Not_created
                      | Ok (Some _) -> (
                          C.commit () >>= function
                          | Error e -> Lwt.return (Error (Caqti_error.show e))
                          | Ok () -> Lwt.return (Ok Pending_created)))))))
