-- migrate:up

-- community_connections: the mutual connection between two Earde
-- communities. One generic relation — there is deliberately no
-- relationship kind, tier, or direction-of-meaning: the requester/
-- recipient split records only who asked and who reviewed, and an
-- accepted connection is symmetric in every reading surface.
-- One table carries the whole lifecycle: a pending request from the
-- requesting community, the accepted mutual connection, and the
-- historical rejected/removed rows that stay behind for provenance.
-- Both FK ends are provenance-neutral on authorization: community-side
-- authority stays in community_moderators (and the durable users.is_admin
-- flag) — nothing here grants a role, membership, or moderation power.
-- requested_by/reviewed_by/removed_by are provenance only (SET NULL so
-- account deletion never erases a connection or its history); the shape
-- CHECK therefore cannot require them non-NULL, only that an actor
-- column is absent in the statuses where no such act took place.
-- Valid lifecycle *transitions* (pending→accepted/rejected,
-- accepted→removed) are owned by the store, not triggers; the shape
-- check only guarantees each stored row is structurally coherent.
-- request_note is stored verbatim (the pure Community_connections domain
-- owns blank→NULL canonicalization, UTF-8/control validation, and the
-- same 2000-char limit the sibling relation tables use); it is private
-- workflow data, never public content. No tokens, secrets, OAuth
-- material, session bindings, GitHub identifiers, or raw GitHub JSON.
CREATE TABLE community_connections (
  id                     BIGSERIAL   PRIMARY KEY,
  requester_community_id INTEGER     NOT NULL
                                     REFERENCES communities(id)
                                     ON DELETE CASCADE,
  recipient_community_id INTEGER     NOT NULL
                                     REFERENCES communities(id)
                                     ON DELETE CASCADE,
  status                 TEXT        NOT NULL
                                     CHECK (status IN ('pending', 'accepted',
                                                       'rejected', 'removed')),
  requested_by_user_id   INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  reviewed_by_user_id    INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  removed_by_user_id     INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  request_note           TEXT        CHECK (request_note IS NULL
                                        OR char_length(request_note) <= 2000),
  created_at             TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at             TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewed_at            TIMESTAMPTZ,
  removed_at             TIMESTAMPTZ,
  -- A community can never connect to itself: the unordered-pair index
  -- below would collapse such a row to a single-element pair, and no
  -- product surface has a meaning for it.
  CONSTRAINT community_connections_distinct_communities_check
    CHECK (requester_community_id <> recipient_community_id),
  CONSTRAINT community_connections_updated_after_created_check
    CHECK (updated_at >= created_at),
  CONSTRAINT community_connections_reviewed_after_created_check
    CHECK (reviewed_at IS NULL OR reviewed_at >= created_at),
  CONSTRAINT community_connections_removed_after_created_check
    CHECK (removed_at IS NULL OR removed_at >= created_at),
  CONSTRAINT community_connections_removed_after_reviewed_check
    CHECK (removed_at IS NULL OR reviewed_at IS NULL
        OR removed_at >= reviewed_at),
  -- Removal is reachable only through acceptance, so a removed row
  -- always carries the review that preceded it: no pending row can jump
  -- straight to removed even if a future writer tried.
  CONSTRAINT community_connections_status_shape_check
    CHECK (
      (status = 'pending'  AND reviewed_at IS NULL AND removed_at IS NULL
                           AND reviewed_by_user_id IS NULL
                           AND removed_by_user_id IS NULL)
      OR (status = 'accepted' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL
                              AND removed_by_user_id IS NULL)
      OR (status = 'rejected' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL
                              AND removed_by_user_id IS NULL)
      OR (status = 'removed'  AND reviewed_at IS NOT NULL
                              AND removed_at IS NOT NULL)
    )
);

-- Load-bearing: at most one ACTIVE connection per unordered community
-- pair — never two pendings, two accepteds, or a pending alongside an
-- accepted, and never one in each direction. The LEAST/GREATEST
-- expression pair is what makes the slot direction-blind, so B→A loses
-- against a live A→B exactly as a second A→B would. This index (not
-- application read-before-write checks) arbitrates every concurrent
-- request and review. Historical rejected/removed rows fall outside the
-- predicate and never occupy the slot; after a rejection or a removal a
-- fresh request may be created from either side.
CREATE UNIQUE INDEX community_connections_one_active_pair_idx
  ON community_connections (LEAST(requester_community_id,
                                  recipient_community_id),
                            GREATEST(requester_community_id,
                                     recipient_community_id))
  WHERE status IN ('pending', 'accepted');

-- The incoming moderation queue of one community (pending requests
-- addressed to it), and the symmetric half of that community's accepted
-- connections, in arrival order. Also the ON DELETE CASCADE scan from
-- communities on the recipient side.
CREATE INDEX idx_community_connections_recipient_status
  ON community_connections (recipient_community_id, status, created_at);

-- The outgoing queue of one community (pending requests it sent), and
-- the other symmetric half of its accepted connections. A symmetric
-- accepted listing is one predicate over both indexes
-- (requester = $1 OR recipient = $1), so no third "either side" index
-- exists. Also the CASCADE scan on the requester side.
CREATE INDEX idx_community_connections_requester_status
  ON community_connections (requester_community_id, status, created_at);

-- One unordered pair's full relation history — active and historical
-- rows alike, newest first. The partial unique index above covers only
-- the single active row, and the two per-community indexes need a
-- status/direction filter plus a sort to answer "everything that ever
-- happened between these two"; the moderation surfaces that justify a
-- fresh request after a rejection or removal read exactly that.
CREATE INDEX idx_community_connections_pair_history
  ON community_connections (LEAST(requester_community_id,
                                  recipient_community_id),
                            GREATEST(requester_community_id,
                                     recipient_community_id),
                            created_at DESC);

-- community_connection_audit_events: the append-only audit history of
-- the community-connection lifecycle. One row per successfully committed
-- durable transition, written inside the same transaction as the
-- transition itself, so no committed lifecycle mutation can exist
-- without its event and no event can describe a rolled-back mutation.
-- mod_actions is deliberately not reused, for the same reasons as
-- project_home_audit_events: it is a public per-community moderation log
-- with moderator-only semantics (moderator_id NOT NULL, required public
-- reason, INTEGER target_id that cannot address BIGINT connection rows)
-- and ON DELETE CASCADE on both community and moderator — deleting an
-- actor there erases history, which audit must survive.
-- The event body is foreign keys plus a closed action vocabulary and a
-- timestamp; nothing else. No request note, submitted text, community
-- names or slugs, moderator roles, authorization provenance, tokens,
-- session values, or SQL diagnostics are stored. Both communities are
-- recorded so an event is readable from either side without joining the
-- connection row, which a later deletion path might have to remove.
-- actor_user_id follows the established provenance convention: SET NULL
-- so account deletion never erases the historical event. The subject FKs
-- (connection, both communities) deliberately carry no ON DELETE action:
-- nothing in the application deletes those rows today, and if a deletion
-- path ever appears it must confront the audit history explicitly rather
-- than silently cascading it away — including through the CASCADE that
-- communities have onto community_connections.
CREATE TABLE community_connection_audit_events (
  id                     BIGSERIAL   PRIMARY KEY,
  action                 TEXT        NOT NULL
                                     CHECK (action IN
                                       ('community_connection_requested',
                                        'community_connection_accepted',
                                        'community_connection_rejected',
                                        'community_connection_removed')),
  actor_user_id          INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  connection_id          BIGINT      NOT NULL
                                     REFERENCES community_connections(id),
  requester_community_id INTEGER     NOT NULL REFERENCES communities(id),
  recipient_community_id INTEGER     NOT NULL REFERENCES communities(id),
  created_at             TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Per-subject histories in reverse chronological order — one index per
-- future listing surface (one connection's trail, each community's
-- trail from either side, one actor's trail). Each also serves its FK's
-- referential scans (SET NULL on user deletion, the NO ACTION checks on
-- the subject side). The BIGSERIAL primary key already covers plain id
-- lookups, so no further index exists.
CREATE INDEX idx_community_connection_audit_events_connection
  ON community_connection_audit_events (connection_id, created_at DESC);

CREATE INDEX idx_community_connection_audit_events_requester
  ON community_connection_audit_events (requester_community_id,
                                        created_at DESC);

CREATE INDEX idx_community_connection_audit_events_recipient
  ON community_connection_audit_events (recipient_community_id,
                                        created_at DESC);

CREATE INDEX idx_community_connection_audit_events_actor
  ON community_connection_audit_events (actor_user_id, created_at DESC);

-- migrate:down

DROP TABLE community_connection_audit_events;

DROP TABLE community_connections;
