(** Persistence for POST /signup, shaped so the handler can answer without
    revealing private account state.

    The only public signup outcome here is a username that already belongs to a
    real account. Whether the email is registered, whether the username is
    reserved by someone's unconfirmed signup, and whether a concurrent
    submission won a uniqueness race all collapse into [Not_created] — the
    handler renders it exactly like [Pending_created]. *)

type outcome =
  | Username_taken
      (** A real account holds this username (exact match, the users table's own
          uniqueness rule). Depends on the username alone. *)
  | Pending_created
      (** A fresh pending row carrying the given token hash is committed. Any
          earlier unconfirmed signup for the same email, and any expired
          reservation of the username, was replaced in the same transaction.
          Only this outcome may lead to a confirmation email. *)
  | Not_created
      (** Nothing was written: the email belongs to a real account, another
          email holds a live reservation of the username, or a concurrent
          submission claimed the same email/username first. *)

val username_registered :
  (module Caqti_lwt.CONNECTION) -> string -> (bool, string) result Lwt.t
(** The cheap public pre-check, run before any hashing. *)

val submit :
  (module Caqti_lwt.CONNECTION) ->
  username:string ->
  email:string ->
  password_hash:string ->
  token_hash:string ->
  ip:string option ->
  user_agent:string option ->
  (outcome, string) result Lwt.t
(** One transaction. The password hash must already be computed: no CPU work
    runs while the transaction is open. Every non-[Pending_created] path rolls
    back, so no partial state survives. [Error] is a storage failure, also with
    nothing committed. *)
