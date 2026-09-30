(** Post and comment votes, gated on global and community bans resolved from the
    target itself. *)

type vote_gate =
  | Vote_allowed
  | Vote_target_missing
  | Vote_globally_banned
  | Vote_community_banned
  | Vote_gate_error of string

val vote_handler : Dream.handler
val vote_comment_handler : Dream.handler
