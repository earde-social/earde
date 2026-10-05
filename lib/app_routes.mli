val router : Dream.handler
(** Every application route, including the per-operation rate-limit wrapping of
    sensitive POSTs. Expects the middleware stack of bin/main.ml around it
    (client address, target redaction, SQL pool, sessions). *)
