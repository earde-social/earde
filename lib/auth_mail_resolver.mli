(** Ownership-bounded name resolution for authentication mail.

    Why it exists: the system resolver ([getaddrinfo] in Lwt's thread pool)
    cannot be interrupted. When a delivery attempt is abandoned at its deadline,
    the lookup it started keeps running inside the operating system. Counting
    only the lookups someone is still waiting for would let every abandoned
    attempt start another one. This gate counts the {e physical} operations
    instead: a permit is taken when an operation starts and is returned only
    when that operation itself finishes, whether or not anyone is still waiting
    for it.

    There is no waiting queue. When every permit is held, {!resolve} answers
    [Error `Busy] at once and starts nothing, so the number of outstanding
    resolver operations (queued or running in the thread pool) never exceeds the
    permit count. *)

type 'endp t

val default_permits : int
(** [2]: the dispatcher's worker count, so a resolver outage can hold at most
    that many threads for authentication mail. *)

val create : permits:int -> resolve:(Uri.t -> 'endp Lwt.t) -> 'endp t
(** [resolve] is the physical operation. Its promise must settle only when the
    underlying work has actually finished. It is never cancelled by the gate.
    Raises [Invalid_argument] when [permits] is not positive. *)

val resolve : 'endp t -> Uri.t -> ('endp, [ `Busy ]) result Lwt.t
(** Starts one physical resolution when a permit is free, else answers
    [Error `Busy] without starting anything. The returned promise is the
    caller's own view: cancelling it stops the caller waiting, drops the
    caller's continuation and keeps the permit held until the physical operation
    finishes. A failing operation releases its permit and rejects the caller's
    promise with the same exception. *)

val outstanding : 'endp t -> int
(** Physical operations started and not yet finished, including ones whose
    callers have given up. *)
