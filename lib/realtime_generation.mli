(** Realtime access generations: the part of a chat topic that makes a lost
    read permission take effect on sockets that are already connected.

    Why it exists: a socket is authorized once, when its token is signed,
    and the gateway checks only the token's signature, topic and expiry. So
    every chat topic carries its community's current generation, and the
    database bumps that generation in the same transaction as any change
    that can take read access away (see the migration
    [20260929120000_add_community_realtime_generations]). Messages committed
    after such a change are published to a topic that no previously
    authorized socket has joined.

    Ordering rules for callers:
    - when signing a token, read the generation {e before} checking access,
      so a revocation that commits in between either fails the check or
      leaves the token on the old topic;
    - when publishing, read it {e after} the message has committed, so a
      message created after a revocation can only reach the new topic. *)

val current :
  (module Caqti_lwt.CONNECTION) -> community_id:int -> (int64, string) result Lwt.t
(** The community's current generation ([0] before its first bump). The
    error is a Caqti message for the log only. *)

val topic : channel_id:int -> generation:int64 -> string
(** [chan:<channel_id>:<generation>]. The gateway's [chan:*] pattern is a
    prefix match, so it routes these unchanged. *)
