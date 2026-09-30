(** Argon2id password hashing with the production cost parameters. Both
    calls run the hash synchronously on the calling thread. *)

val t_cost : int
val m_cost : int
val parallelism : int
(** The production cost, exposed so the login dummy-hash fixture can be
    checked against exactly these values: a cheaper dummy would make a
    missing account measurably faster to reject than a wrong password. *)

val hash_password : string -> (Argon2.encoded, string) result Lwt.t
(** A salted Argon2id encoding of the password. *)

val verify_password :
  password:string -> hash:Argon2.encoded -> (bool, string) result Lwt.t
