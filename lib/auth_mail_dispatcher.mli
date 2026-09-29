(** Bounded, in-process, best-effort scheduler for authentication mail
    (signup confirmations and password resets).

    Why it exists: the signup and password-reset handlers used to await the
    email provider before answering, so provider latency sat on the request
    path, and only requests for real accounts paid it. That was a timing
    oracle for account existence. The handlers now admit each request here
    {e before} any private account/email decision, persist durable state,
    and settle the admission into one scheduling entry without awaiting the
    provider.

    {b Why every admission gets a fixed service slot.} The capacity is
    shared by every client. If a request that sends no mail gave its place
    back sooner than one that does, another client's next admission would
    reveal the private outcome: a 503 or a 200 at the capacity edge, or a
    different release time. So every admitted request, whatever its private
    outcome (including work that fails or is cancelled), settles into
    exactly one FIFO entry. An entry is either a real message or a no-send
    entry. Entries are serviced in the order they settle, by at most
    [concurrency] workers. Each service slot lasts exactly
    [timeout_seconds] on a monotonic clock, for both kinds, and only the end
    of the slot releases the worker and the admission place. A real message
    is attempted when its slot begins. The attempt may finish early, but the
    slot does not. An attempt still running at the deadline is cancelled.
    A no-send entry holds no recipient, token or password and never
    reaches the transport.

    Cost, accepted deliberately: at saturation two 15 s slots serve at most
    8 entries per minute, no-send entries included. A full backlog of 64
    ready entries takes about 8 minutes to drain. Delivery is not
    guaranteed.

    What this does {e not} claim: constant-time SQL, OS scheduling or HTTP
    execution. An entry joins the queue when its private work settles, so
    database timing differences carry over into when it is queued. The
    guarantee is that the job/no-job decision, provider behaviour, DNS
    availability and transport failures do not change occupancy or slot
    duration.

    Deliberately volatile: entries live only in this process. A restart
    drops them. The pending-signup row or reset token stays in PostgreSQL
    (only its hash), and the user recovers by asking again: a signup resend
    replaces that pending signup's token, and a new reset request issues
    another reset token (earlier unexpired reset tokens stay valid until
    used or expired). Delivery is neither durable nor exactly-once. *)

type config = {
  capacity : int;
      (** Admitted outstanding requests: open reservations + queued entries
          + entries in service. *)
  concurrency : int;  (** Simultaneous service slots (workers). *)
  timeout_seconds : float;
      (** Fixed length of every service slot, and so the most time one
          provider attempt may take. *)
}

val default_config : config
(** [{ capacity = 64; concurrency = 2; timeout_seconds = 15.0 }]. *)

type 'job t

type 'job transport = 'job -> (unit, string) result Lwt.t
(** One delivery attempt. [Error] carries a short, secret-free failure class
    for the log line: never a recipient, token, payload or provider body.
    The returned promise must be cancelable, and cancelling it must release
    every resource the attempt holds (sockets included). The dispatcher
    cancels it at the slot deadline. A transport may also raise. That is
    contained and counted as a failure. *)

val monotonic_sleep : float -> unit Lwt.t
(** Resolves once at least the given number of seconds have passed on the
    monotonic clock, so wall-clock steps can neither shorten nor stretch a
    slot. The production slot timer. *)

val create :
  ?config:config ->
  ?sleep:(float -> unit Lwt.t) ->
  label:('job -> string) ->
  transport:'job transport ->
  unit ->
  'job t
(** [sleep] defaults to {!monotonic_sleep}. Tests inject a controlled clock.
    The dispatcher never cancels a slot timer. [label] names a job in
    diagnostics and must return a fixed, non-personal category (e.g.
    ["password_reset"]). Creating a dispatcher starts no promise. Raises
    [Invalid_argument] on a non-positive bound. *)

val admit :
  'job t ->
  (unit -> ('a * 'job option) Lwt.t) ->
  [ `Admitted of 'a | `Refused ] Lwt.t
(** [admit t work] is the one nonblocking admission path. When [t] is at
    capacity it answers [`Refused] at once, without calling [work], so a
    refused request does no lookup, write or hashing. Otherwise it holds a
    reservation while [work] runs, then settles it exactly once into a
    queue entry: [Some job] becomes a real entry, [None] a no-send entry.
    If [work] raises or is cancelled, the reservation becomes a no-send
    entry and the exception propagates. Either way the admission place is
    held until that entry's service slot ends. The returned promise
    resolves right after settlement. It never waits for a slot or the
    provider. [work] must return [Some job] only after the state the
    message refers to is durably committed. *)

type stats = { outstanding : int; queued : int; running : int }

val stats : 'job t -> stats
(** Current occupancy. [outstanding] counts open reservations, queued
    entries and entries in service. [running] counts service slots in use,
    real or no-send. *)
