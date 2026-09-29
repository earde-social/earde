(** Bounded, in-process, best-effort dispatcher for authentication mail
    (signup confirmations and password resets).

    Why it exists: the signup and password-reset handlers used to await the
    email provider before answering, so provider latency sat on the request
    path, and only requests for real accounts paid it — a timing oracle for
    account existence. The handlers now admit the request here {e before}
    any private account/email decision, persist durable state, and hand the
    transient message to a small worker pool that runs after the response.

    Bounds (see {!default_config}): at most [capacity] admitted outstanding
    requests in total — open reservations, queued jobs and running
    deliveries together — at most [concurrency] deliveries at once, and a
    [timeout_seconds] budget covering the whole provider operation.

    Deliberately volatile: queued jobs live only in this process. A restart
    drops them; the pending-signup row or reset token stays in PostgreSQL
    (only its hash), and the user recovers by requesting a fresh
    confirmation or reset, which replaces or supersedes the earlier token.
    Delivery is neither durable nor exactly-once. *)

type config = {
  capacity : int;
      (** Admitted outstanding requests: reservations + queued + running. *)
  concurrency : int;  (** Simultaneous provider deliveries. *)
  timeout_seconds : float;
      (** Budget for one whole provider operation (resolve, connect, request,
          response body). *)
}

val default_config : config
(** [{ capacity = 64; concurrency = 2; timeout_seconds = 15.0 }]. *)

type 'job t

type 'job transport = 'job -> (unit, string) result Lwt.t
(** One delivery attempt. [Error] carries a short, secret-free failure class
    for the log line — never a recipient, token, payload or provider body.
    The returned promise must be cancelable, and cancelling it must release
    every resource the attempt holds (sockets included): the dispatcher
    cancels it when the timeout expires. A transport may also raise; that is
    contained and counted as a failure. *)

val create :
  ?config:config ->
  ?sleep:(float -> unit Lwt.t) ->
  label:('job -> string) ->
  transport:'job transport ->
  unit ->
  'job t
(** [sleep] defaults to [Lwt_unix.sleep]; tests inject a controlled clock.
    [label] names a job in diagnostics and must return a fixed, non-personal
    category (e.g. ["password_reset"]). Creating a dispatcher starts no
    promise. Raises [Invalid_argument] on a non-positive bound. *)

val admit :
  'job t ->
  (unit -> ('a * 'job option) Lwt.t) ->
  [ `Admitted of 'a | `Refused ] Lwt.t
(** [admit t work] is the one nonblocking admission path. When [t] is at
    capacity it answers [`Refused] at once, without calling [work] — so no
    lookup, write or hashing happens for a refused request. Otherwise it
    holds a reservation while [work] runs, and settles it exactly once:
    [Some job] turns the reservation into a queued delivery, [None] releases
    it. If [work] raises or is cancelled, the reservation is released and
    the exception propagates. [work] must return [Some job] only after the
    state the message refers to is durably committed. *)

val run_bounded :
  ?sleep:(float -> unit Lwt.t) ->
  timeout_seconds:float ->
  'job transport ->
  'job ->
  [ `Delivered | `Failed of string | `Timed_out ] Lwt.t
(** One delivery attempt under the timeout, exactly as a worker runs it:
    never raises, cancels (and so cleans up) the attempt on timeout, and
    cancels the timer once the attempt settles. Also used by the legacy
    awaited send functions in {!Email}. *)

type stats = { outstanding : int; queued : int; running : int }

val stats : 'job t -> stats
(** Current occupancy. [outstanding] includes open reservations. *)
