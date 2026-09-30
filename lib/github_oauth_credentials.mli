(** Validated server-only secret credential for the GitHub App OAuth
    code-for-token exchange.

    This module is deliberately separate from {!Github_app_config}: that module
    holds public browser-facing configuration, while this one holds secret
    material that must never reach a browser, a log line or a URL.

    Only GITHUB_APP_CLIENT_SECRET is read here. The App ID, private key and
    webhook secret belong to later server-to-server slices and are deliberately
    not read; the public client id stays in {!Github_app_config}. *)

type t
(** A validated client secret. The representation is abstract and there is no
    serializer or pretty-printer, so the secret cannot be exposed by accident.
*)

type field = Client_secret

type error =
  | Missing of field  (** the variable is not set at all *)
  | Invalid of field  (** empty, or contains whitespace or control bytes *)

val string_of_field : field -> string
(** The environment variable name for the field. *)

val string_of_error : error -> string
(** Stable diagnostic naming the field and reason only — never the supplied
    value, its length, or any derivative of it. *)

val of_values : client_secret:string option -> (t, error) result
(** Pure validation of the raw value. The secret is treated as an opaque,
    byte-exact credential: any non-empty value free of whitespace and control
    bytes (including DEL) is accepted and stored byte-for-byte. No trimming,
    normalization or decoding is performed; no format, prefix or length is
    imposed. *)

val from_env : unit -> (t, error) result
(** Reads GITHUB_APP_CLIENT_SECRET, then delegates to {!of_values}. No caching,
    no logging. *)

val client_secret : t -> string
(** The exact secret bytes as supplied.

    This accessor exists only so the OAuth exchange client can place the
    credential into the outbound token request. Callers must not log it, include
    it in errors, persist it, place it in URLs returned to browsers, or include
    it in metrics or analytics. *)
