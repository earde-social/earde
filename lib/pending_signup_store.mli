val hash_token : string -> string
(* Pending-signup creation lives in Signup_submission_store, which decides
   every private outcome inside one transaction. *)
val sweep_expired : (module Caqti_lwt.CONNECTION) -> (unit, string) result Lwt.t
(* `Confirmed (user_id, username, email, created_at, is_admin) = user row
   created — id/created_at/is_admin from the insert's RETURNING, so the
   caller holds the closed analytics person properties (§4.3) without a
   post-transaction lookup; `Invalid = token missing/expired/already used;
   `Conflict = username/email taken in users since signup. *)
val confirm :
  (module Caqti_lwt.CONNECTION) -> string ->
  ([ `Confirmed of int * string * string * string * bool
   | `Invalid | `Conflict ], string) result Lwt.t
