(** The "Connected communities" block of the existing community pages — a pure
    fragment over a handler-supplied page model. No Caqti, no read-model
    dependency, no session access, no request, no JavaScript.

    The model is deliberately just an identity: a name and a slug. Nothing about
    the connection itself is representable here — no status vocabulary, no
    direction, no connection id, no request note, no requester, reviewer, or
    remover, no timestamps, and no management action. The block states that two
    communities are connected, and says nothing about what that means: there is
    no "depends on", "used by", "related ecosystem", or any other relationship
    kind.

    Every string is escaped at the template boundary, and a counterpart whose
    slug is not a single URL path segment renders as inert text rather than
    becoming an actionable link. Nothing here raises on a malformed model. *)

type connected_community = {
  name : string;
  slug : string;
      (** Used only to build [/c/<slug>], and only when it is a single non-empty
          URL path segment. *)
}

val connected_communities_section :
  communities:connected_community list -> Html.t
(** The rendered block, or [""] for an empty list — no empty card, no
    placeholder, and no "none yet" copy: a community with nothing publicly
    connected shows nothing at all. The supplied order is preserved exactly;
    nothing is re-sorted here. *)

val empty_communities_section : Html.t
(** The same block with a quiet "No connected communities yet." line in place of
    the list, for the dedicated Network page — which names both destinations
    whether or not either holds anything. Shares the heading constant with
    {!connected_communities_section}, so the two surfaces cannot drift apart;
    states no lifecycle, actor, or note, exactly like the populated block. Not
    for the community page, whose contract stays "nothing connected, nothing
    rendered". *)
