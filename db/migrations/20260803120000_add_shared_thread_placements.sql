-- migrate:up

-- shared_thread_placements: a secondary community placement of one canonical
-- durable discussion (a posts row). The canonical thread stays exactly one
-- row in posts — one title, one body, one author, one comment tree — and
-- posts.community_id / posts.section_id remain its immutable origin
-- placement. A row here carries only destination-local state: which
-- connected community received the thread, which of that community's forum
-- sections it was accepted into, the lifecycle status, and the
-- requester/reviewer/remover/withdrawer provenance. Nothing here copies a
-- post, duplicates a comment tree, or grants a role, membership, or
-- moderation power — community-side authority stays in community_moderators
-- (and the durable users.is_admin flag).
--
-- origin_community_id is a denormalized copy of the canonical post's
-- posts.community_id: the application never updates that column (the only
-- UPDATE posts statements are content tombstones and last_activity_at), so
-- the copy cannot drift. It exists so the destination<>origin CHECK below
-- can live in the table and so origin-side listings and the audit trail
-- read without joining posts. The transactional store derives it from the
-- locked post row on every insert and never trusts a caller to supply it.
--
-- One canonical post may hold placements in several destination communities
-- over time — multiple rows — but never more than one ACTIVE (pending or
-- accepted) placement per (post, destination): the partial unique index
-- below arbitrates that, not application read-before-write checks.
--
-- Actor columns (requested_by/reviewed_by/removed_by/withdrawn_by) are
-- provenance only, SET NULL so account deletion never erases a placement or
-- its history; the shape CHECK therefore cannot require them non-NULL, only
-- that an actor column is absent in the statuses where no such act took
-- place. Valid lifecycle *transitions* (pending→accepted/rejected/withdrawn,
-- accepted→removed) are owned by the store, not triggers; the shape check
-- only guarantees each stored row is structurally coherent.
--
-- destination_section_id may be NULL on an accepted row: a sectionless
-- (sections_enabled = FALSE) destination accepts into its flat surface, and
-- deleting a destination section later releases its placements through
-- ON DELETE SET NULL — mirroring posts.section_id — without invalidating
-- the acceptance. The store, not this CHECK, enforces that a currently
-- sectioned destination receives a valid section at accept time.
--
-- request_note is stored verbatim (the pure Shared_thread_placements domain
-- owns blank→NULL canonicalization, UTF-8/control validation, and the same
-- 2000-scalar limit the sibling relation tables use); it is private
-- workflow data, never public content. No tokens, secrets, session
-- bindings, or SQL diagnostics.
CREATE TABLE shared_thread_placements (
  id                       BIGSERIAL   PRIMARY KEY,
  post_id                  INTEGER     NOT NULL
                                       REFERENCES posts(id)
                                       ON DELETE CASCADE,
  origin_community_id      INTEGER     NOT NULL
                                       REFERENCES communities(id)
                                       ON DELETE CASCADE,
  destination_community_id INTEGER     NOT NULL
                                       REFERENCES communities(id)
                                       ON DELETE CASCADE,
  destination_section_id   INTEGER     REFERENCES community_sections(id)
                                       ON DELETE SET NULL,
  status                   TEXT        NOT NULL
                                       CHECK (status IN ('pending', 'accepted',
                                                         'rejected', 'removed',
                                                         'withdrawn')),
  requested_by_user_id     INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  reviewed_by_user_id      INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  removed_by_user_id       INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  withdrawn_by_user_id     INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  request_note             TEXT        CHECK (request_note IS NULL
                                          OR char_length(request_note) <= 2000),
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at               TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewed_at              TIMESTAMPTZ,
  removed_at               TIMESTAMPTZ,
  withdrawn_at             TIMESTAMPTZ,
  -- A thread is never "shared" into the community it already lives in: the
  -- origin placement is posts.community_id itself, and no product surface
  -- has a meaning for a duplicate of it here.
  CONSTRAINT shared_thread_placements_distinct_communities_check
    CHECK (destination_community_id <> origin_community_id),
  CONSTRAINT shared_thread_placements_updated_after_created_check
    CHECK (updated_at >= created_at),
  CONSTRAINT shared_thread_placements_reviewed_after_created_check
    CHECK (reviewed_at IS NULL OR reviewed_at >= created_at),
  CONSTRAINT shared_thread_placements_removed_after_created_check
    CHECK (removed_at IS NULL OR removed_at >= created_at),
  CONSTRAINT shared_thread_placements_removed_after_reviewed_check
    CHECK (removed_at IS NULL OR reviewed_at IS NULL
        OR removed_at >= reviewed_at),
  CONSTRAINT shared_thread_placements_withdrawn_after_created_check
    CHECK (withdrawn_at IS NULL OR withdrawn_at >= created_at),
  -- Removal is reachable only through acceptance, so a removed row always
  -- carries the review that preceded it; withdrawal is reachable only from
  -- pending, so a withdrawn row never carries a review or a removal, and
  -- the two terminal cancellations stay structurally distinct. The section
  -- is chosen by the destination reviewer at accept time, so it is absent
  -- on every row that was never accepted.
  CONSTRAINT shared_thread_placements_status_shape_check
    CHECK (
      (status = 'pending'   AND reviewed_at IS NULL AND removed_at IS NULL
                            AND withdrawn_at IS NULL
                            AND reviewed_by_user_id IS NULL
                            AND removed_by_user_id IS NULL
                            AND withdrawn_by_user_id IS NULL
                            AND destination_section_id IS NULL)
      OR (status = 'accepted' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL
                              AND withdrawn_at IS NULL
                              AND removed_by_user_id IS NULL
                              AND withdrawn_by_user_id IS NULL)
      OR (status = 'rejected' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL
                              AND withdrawn_at IS NULL
                              AND removed_by_user_id IS NULL
                              AND withdrawn_by_user_id IS NULL
                              AND destination_section_id IS NULL)
      OR (status = 'removed'  AND reviewed_at IS NOT NULL
                              AND removed_at IS NOT NULL
                              AND withdrawn_at IS NULL
                              AND withdrawn_by_user_id IS NULL)
      OR (status = 'withdrawn' AND withdrawn_at IS NOT NULL
                               AND reviewed_at IS NULL
                               AND removed_at IS NULL
                               AND reviewed_by_user_id IS NULL
                               AND removed_by_user_id IS NULL
                               AND destination_section_id IS NULL)
    )
);

-- Load-bearing: at most one ACTIVE placement per (post, destination
-- community) — never two pendings, two accepteds, or a pending alongside an
-- accepted. This index (not application read-before-write checks)
-- arbitrates every concurrent request through ON CONFLICT DO NOTHING.
-- Historical rejected/removed/withdrawn rows fall outside the predicate and
-- never occupy the slot; after any terminal state a fresh request may be
-- created as a new row.
CREATE UNIQUE INDEX shared_thread_placements_one_active_destination_idx
  ON shared_thread_placements (post_id, destination_community_id)
  WHERE status IN ('pending', 'accepted');

-- The destination side's review queue (pending, in arrival order) and its
-- accepted-placement management list, plus the destination community feed
-- arm a later slice will read (accepted placements of one community). Also
-- the ON DELETE CASCADE scan from communities on the destination side.
CREATE INDEX idx_shared_thread_placements_destination_status
  ON shared_thread_placements (destination_community_id, status, created_at);

-- The origin side's management lists (outgoing pending, accepted
-- elsewhere), and the CASCADE scan on the origin side.
CREATE INDEX idx_shared_thread_placements_origin_status
  ON shared_thread_placements (origin_community_id, status, created_at);

-- Placement lookup and provenance by canonical post — the thread page's
-- "where else does this discussion live", and the CASCADE scan from posts.
-- The active (post, destination) point lookup is covered by the partial
-- unique index above.
CREATE INDEX idx_shared_thread_placements_post
  ON shared_thread_placements (post_id, status);

-- The destination forum-section arm of a later feed slice, and the
-- ON DELETE SET NULL scan from community_sections. Partial: only accepted
-- rows can carry a section that a feed would read, but SET NULL must find
-- historical removed rows too, so the predicate is on the column, not the
-- status.
CREATE INDEX idx_shared_thread_placements_destination_section
  ON shared_thread_placements (destination_section_id)
  WHERE destination_section_id IS NOT NULL;

-- shared_thread_placement_audit_events: the append-only audit history of
-- the shared-thread placement lifecycle. One row per successfully committed
-- durable transition, written inside the same transaction as the
-- transition itself, so no committed lifecycle mutation can exist without
-- its event and no event can describe a rolled-back mutation.
-- mod_actions is deliberately not reused, for the same reasons as the
-- sibling audit tables: it is a public per-community moderation log with
-- moderator-only semantics (moderator_id NOT NULL, required public reason,
-- INTEGER target_id that cannot address BIGINT placement rows) and
-- ON DELETE CASCADE on both community and moderator — deleting an actor
-- there erases history, which audit must survive.
-- The event body is foreign keys plus a closed action vocabulary and a
-- timestamp; nothing else. No request note, submitted text, community
-- names or slugs, post titles, moderator roles, authorization provenance,
-- tokens, session values, or SQL diagnostics are stored. Both communities
-- and the post are recorded so an event is readable from any side without
-- joining the placement row, which a later deletion path might have to
-- remove.
-- actor_user_id follows the established provenance convention: SET NULL so
-- account deletion never erases the historical event. The subject FKs
-- (placement, post, both communities) deliberately carry no ON DELETE
-- action: nothing in the application deletes those rows today (posts are
-- tombstoned, never hard-deleted), and if a deletion path ever appears it
-- must confront the audit history explicitly rather than silently
-- cascading it away — including through the CASCADEs that communities and
-- posts have onto shared_thread_placements.
CREATE TABLE shared_thread_placement_audit_events (
  id                       BIGSERIAL   PRIMARY KEY,
  action                   TEXT        NOT NULL
                                       CHECK (action IN
                                         ('shared_thread_requested',
                                          'shared_thread_accepted',
                                          'shared_thread_rejected',
                                          'shared_thread_removed',
                                          'shared_thread_withdrawn')),
  actor_user_id            INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  placement_id             BIGINT      NOT NULL
                                       REFERENCES shared_thread_placements(id),
  post_id                  INTEGER     NOT NULL REFERENCES posts(id),
  origin_community_id      INTEGER     NOT NULL REFERENCES communities(id),
  destination_community_id INTEGER     NOT NULL REFERENCES communities(id),
  created_at               TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Per-subject histories in reverse chronological order — one index per
-- future listing surface (one placement's trail, one post's trail, each
-- community's trail from either side, one actor's trail). Each also serves
-- its FK's referential scans (SET NULL on user deletion, the NO ACTION
-- checks on the subject side). The BIGSERIAL primary key already covers
-- plain id lookups, so no further index exists.
CREATE INDEX idx_shared_thread_placement_audit_events_placement
  ON shared_thread_placement_audit_events (placement_id, created_at DESC);

CREATE INDEX idx_shared_thread_placement_audit_events_post
  ON shared_thread_placement_audit_events (post_id, created_at DESC);

CREATE INDEX idx_shared_thread_placement_audit_events_origin
  ON shared_thread_placement_audit_events (origin_community_id,
                                           created_at DESC);

CREATE INDEX idx_shared_thread_placement_audit_events_destination
  ON shared_thread_placement_audit_events (destination_community_id,
                                           created_at DESC);

CREATE INDEX idx_shared_thread_placement_audit_events_actor
  ON shared_thread_placement_audit_events (actor_user_id, created_at DESC);

-- Shared-thread placement lifecycle notifications extend the same
-- notifications table the project-home and community-connection kinds
-- already extended, rather than adding a parallel inbox or parallel prose
-- rows. A placement notification is structured exactly like its siblings:
-- durable subject references plus a closed kind, rendered from current
-- entity state at read time. No request note, decision text, actor
-- username, copied community name or slug, post title, rendered prose,
-- URL, authorization provenance, or SQL diagnostic is ever stored.
--
-- The subject reference is a NEW column rather than a reuse of relation_id
-- or connection_id: each of those carries a foreign key to a different
-- table (community_projects, community_connections), and a single column
-- cannot reference both its table and shared_thread_placements; dropping a
-- foreign key to overload a column would trade real referential integrity
-- (and its ON DELETE CASCADE cleanup) for a name.
-- shared_thread_placement_id therefore joins project_id / community_id /
-- relation_id / connection_id as one more nullable structured-subject
-- column, and the shape CHECK below is what keeps the four row shapes
-- unambiguous.
--
-- Deletion semantics repeat the established conventions:
--   * recipient (user_id): ON DELETE CASCADE, already on the table;
--   * actor: SET NULL, already on the table;
--   * subject (placement): CASCADE, matching relation_id and connection_id
--     — notifications are user-facing surface, not audit history, so they
--     follow their subject. shared_thread_placement_audit_events
--     independently protects the placement row with a no-action foreign
--     key, so this cascade can only ever fire behind an explicit audit
--     purge.
ALTER TABLE notifications
  ADD COLUMN shared_thread_placement_id BIGINT
    CONSTRAINT notifications_shared_thread_placement_id_fkey
    REFERENCES shared_thread_placements(id) ON DELETE CASCADE;

-- The vocabulary, extended with the five closed shared-thread kinds.
-- Rewritten whole rather than relaxed: a CHECK cannot be extended in
-- place, and the enumeration is the durable guarantee that no
-- caller-supplied string ever lands in notif_type whatever row shape it
-- arrives in.
ALTER TABLE notifications
  DROP CONSTRAINT notifications_notif_type_check;

ALTER TABLE notifications
  ADD CONSTRAINT notifications_notif_type_check CHECK (
    notif_type IN ('comment_reply',
                   'mention',
                   'mod_action',
                   'project_home_requested',
                   'project_home_accepted',
                   'project_home_rejected',
                   'project_home_removed',
                   'community_connection_requested',
                   'community_connection_accepted',
                   'community_connection_rejected',
                   'community_connection_removed',
                   'shared_thread_requested',
                   'shared_thread_accepted',
                   'shared_thread_rejected',
                   'shared_thread_removed',
                   'shared_thread_withdrawn')
  );

-- The shapes. Four of them now, each fully determined by its kind and each
-- exclusive of the other three, so a row can satisfy exactly one branch:
--
--   * project-home: project_id, community_id, relation_id, and no
--     connection, no placement, no prose, no post link (unchanged from the
--     previous constraint except for the added
--     shared_thread_placement_id IS NULL);
--   * community-connection: community_id and connection_id, and nothing
--     else (unchanged except the added placement exclusion);
--   * shared-thread: community_id (the recipient's own community context —
--     origin-side recipients carry the origin, destination-side recipients
--     the destination) and shared_thread_placement_id, and nothing else —
--     no project, no relation, no connection, no prose, no post link (the
--     canonical post is derived from the placement at read time, never
--     stored twice);
--   * legacy prose: a message, and none of the structured columns.
--
-- Every existing row keeps satisfying its branch unchanged: the new column
-- is NULL on all of them, and each pre-existing branch gains only a
-- shared_thread_placement_id IS NULL conjunct.
--
-- The old constraint is replaced rather than supplemented: two overlapping
-- shape CHECKs would each have to know about the other's columns.
ALTER TABLE notifications
  DROP CONSTRAINT notifications_shape_check;

ALTER TABLE notifications
  ADD CONSTRAINT notifications_shape_check CHECK (
    (notif_type IN ('project_home_requested',
                    'project_home_accepted',
                    'project_home_rejected',
                    'project_home_removed')
      AND project_id IS NOT NULL
      AND community_id IS NOT NULL
      AND relation_id IS NOT NULL
      AND connection_id IS NULL
      AND shared_thread_placement_id IS NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type IN ('community_connection_requested',
                    'community_connection_accepted',
                    'community_connection_rejected',
                    'community_connection_removed')
      AND community_id IS NOT NULL
      AND connection_id IS NOT NULL
      AND project_id IS NULL
      AND relation_id IS NULL
      AND shared_thread_placement_id IS NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type IN ('shared_thread_requested',
                    'shared_thread_accepted',
                    'shared_thread_rejected',
                    'shared_thread_removed',
                    'shared_thread_withdrawn')
      AND community_id IS NOT NULL
      AND shared_thread_placement_id IS NOT NULL
      AND project_id IS NULL
      AND relation_id IS NULL
      AND connection_id IS NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type NOT IN ('project_home_requested',
                        'project_home_accepted',
                        'project_home_rejected',
                        'project_home_removed',
                        'community_connection_requested',
                        'community_connection_accepted',
                        'community_connection_rejected',
                        'community_connection_removed',
                        'shared_thread_requested',
                        'shared_thread_accepted',
                        'shared_thread_rejected',
                        'shared_thread_removed',
                        'shared_thread_withdrawn')
      AND project_id IS NULL
      AND community_id IS NULL
      AND relation_id IS NULL
      AND connection_id IS NULL
      AND shared_thread_placement_id IS NULL
      AND actor_user_id IS NULL
      AND message IS NOT NULL)
  );

-- Durable deduplication, mirroring the relation and connection indexes: at
-- most one notification per recipient, kind, and placement. A placement
-- row is reused across its own lifecycle, but each transition writes a
-- different kind, so the lifecycle notifications coexist while a replayed
-- mutation cannot double-deliver — and a recipient reachable through both
-- communities (a top moderator of both sides, or a requester who also
-- moderates the destination) is inserted exactly once per transition,
-- which this index durably enforces. A fresh request after a rejection,
-- removal, or withdrawal creates a new placement row — the active
-- (post, destination) index guarantees the old one is no longer active —
-- so it notifies again.
CREATE UNIQUE INDEX uq_notifications_recipient_kind_placement
  ON notifications (user_id, notif_type, shared_thread_placement_id)
  WHERE shared_thread_placement_id IS NOT NULL;

-- Referential scan for the new FK (CASCADE), partial because every other
-- row carries NULL here.
CREATE INDEX idx_notifications_shared_thread_placement
  ON notifications (shared_thread_placement_id)
  WHERE shared_thread_placement_id IS NOT NULL;

-- migrate:down

DELETE FROM notifications
 WHERE notif_type IN ('shared_thread_requested',
                      'shared_thread_accepted',
                      'shared_thread_rejected',
                      'shared_thread_removed',
                      'shared_thread_withdrawn');

DROP INDEX idx_notifications_shared_thread_placement;
DROP INDEX uq_notifications_recipient_kind_placement;

ALTER TABLE notifications
  DROP CONSTRAINT notifications_shape_check;

ALTER TABLE notifications
  ADD CONSTRAINT notifications_shape_check CHECK (
    (notif_type IN ('project_home_requested',
                    'project_home_accepted',
                    'project_home_rejected',
                    'project_home_removed')
      AND project_id IS NOT NULL
      AND community_id IS NOT NULL
      AND relation_id IS NOT NULL
      AND connection_id IS NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type IN ('community_connection_requested',
                    'community_connection_accepted',
                    'community_connection_rejected',
                    'community_connection_removed')
      AND community_id IS NOT NULL
      AND connection_id IS NOT NULL
      AND project_id IS NULL
      AND relation_id IS NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type NOT IN ('project_home_requested',
                        'project_home_accepted',
                        'project_home_rejected',
                        'project_home_removed',
                        'community_connection_requested',
                        'community_connection_accepted',
                        'community_connection_rejected',
                        'community_connection_removed')
      AND project_id IS NULL
      AND community_id IS NULL
      AND relation_id IS NULL
      AND connection_id IS NULL
      AND actor_user_id IS NULL
      AND message IS NOT NULL)
  );

ALTER TABLE notifications
  DROP CONSTRAINT notifications_notif_type_check;

ALTER TABLE notifications
  ADD CONSTRAINT notifications_notif_type_check CHECK (
    notif_type IN ('comment_reply',
                   'mention',
                   'mod_action',
                   'project_home_requested',
                   'project_home_accepted',
                   'project_home_rejected',
                   'project_home_removed',
                   'community_connection_requested',
                   'community_connection_accepted',
                   'community_connection_rejected',
                   'community_connection_removed')
  );

ALTER TABLE notifications
  DROP COLUMN shared_thread_placement_id;

DROP TABLE shared_thread_placement_audit_events;

DROP TABLE shared_thread_placements;
