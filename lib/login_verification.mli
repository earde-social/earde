(** Credential check for POST /login with equal work on every outcome.

    A missing account used to skip Argon2 entirely, so "no such user" answered
    measurably faster than "wrong password" — an account enumeration oracle
    despite the identical response text. Every attempt now runs exactly one
    verification: against the account's stored hash, or against a fixed public
    dummy hash with the production cost. *)

type verifier = password:string -> hash:string -> bool Lwt.t
(** [true] only for a positive verification. *)

val argon2_verifier : verifier
(** The production verifier ({!Auth.verify_password}); a malformed hash or any
    verification error is [false]. *)

val dummy_password : string

val dummy_hash : string
(** A valid Argon2id encoding of [dummy_password], produced once with the
    production parameters ({!Auth.m_cost}, {!Auth.t_cost}, {!Auth.parallelism})
    and a random salt. Both are public on purpose: the dummy belongs to no
    account and its result is discarded, so knowing the password grants nothing.
    A test pins that it verifies with full work, since a malformed dummy would
    be rejected instantly. *)

val authenticate :
  verify:verifier ->
  password:string ->
  (string * 'account) option ->
  'account option Lwt.t
(** [authenticate ~verify ~password candidate] where [candidate] is the
    looked-up [(stored_hash, account)]. Calls [verify] exactly once in every
    case. Returns [Some account] only when an account was found AND its own hash
    verified; a missing account is always [None], even if [password] is
    [dummy_password]. A raising verifier counts as a failed check. *)
