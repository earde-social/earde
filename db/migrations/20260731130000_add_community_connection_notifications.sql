-- migrate:up

-- Community↔community connection lifecycle notifications extend the same
-- notifications table the project-home kinds already extended, rather than
-- adding a parallel inbox or parallel prose rows. A connection notification
-- is structured exactly like a project-home one: durable subject references
-- plus a closed kind, rendered from current entity state at read time. No
-- request note, decision text, actor username, copied community name or
-- slug, rendered prose, URL, authorization provenance, or SQL diagnostic is
-- ever stored.
--
-- The subject reference is a NEW column rather than a reuse of relation_id.
-- relation_id carries a foreign key to community_projects, and a connection
-- id is a community_connections id: a single column cannot reference both
-- tables, and dropping that foreign key to overload the column would trade
-- real referential integrity (and its ON DELETE CASCADE cleanup) for a name.
-- connection_id therefore joins project_id / community_id / relation_id as
-- one more nullable structured-subject column, and the shape CHECK below is
-- what keeps the three row shapes unambiguous.
--
-- Deletion semantics repeat the conventions the project-home migration
-- established:
--   * recipient (user_id): ON DELETE CASCADE, already on the table;
--   * actor: SET NULL, already on the table — a deleted actor never erases
--     the recipient's notification, and the rendered copy never names an
--     actor anyway;
--   * subject (connection): CASCADE, matching relation_id — notifications
--     are user-facing surface, not audit history, so they follow their
--     subject. community_connection_audit_events independently protects the
--     connection row with a no-action foreign key, so this cascade can only
--     ever fire behind an explicit audit purge.
ALTER TABLE notifications
  ADD COLUMN connection_id BIGINT
    CONSTRAINT notifications_connection_id_fkey
    REFERENCES community_connections(id) ON DELETE CASCADE;

-- The vocabulary, extended with the four closed connection kinds. Rewritten
-- whole rather than relaxed: a CHECK cannot be extended in place, and the
-- enumeration is the durable guarantee that no caller-supplied string ever
-- lands in notif_type whatever row shape it arrives in.
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

-- The shapes. Three of them now, each fully determined by its kind and each
-- exclusive of the other two, so a row can satisfy exactly one branch:
--
--   * project-home: all three of project_id, community_id, relation_id, and
--     no connection_id, no prose, no post link (unchanged from the previous
--     constraint except for the added connection_id IS NULL);
--   * community-connection: community_id (the recipient's own management
--     context) and connection_id, and nothing else — no project, no
--     relation, no prose, no post link;
--   * legacy prose: a message, and none of the structured columns.
--
-- A legacy kind can therefore never reference a connection, a connection
-- kind can never smuggle prose or a project, a project-home kind can never
-- carry a connection id, and the legacy message NOT NULL invariant survives
-- the column-level DROP NOT NULL the previous migration performed.
--
-- The old single-purpose constraint is replaced rather than supplemented:
-- two overlapping shape CHECKs would each have to know about the other's
-- columns, and the name would stop describing what it enforces.
ALTER TABLE notifications
  DROP CONSTRAINT notifications_project_home_shape_check;

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

-- Durable deduplication, mirroring the relation index: at most one
-- notification per recipient, kind, and connection. A connection row is
-- reused across its own lifecycle (pending → accepted/rejected, accepted →
-- removed), but each transition writes a different kind, so the three
-- lifecycle notifications coexist while a replayed mutation cannot
-- double-deliver. A fresh request after a rejection or removal creates a new
-- connection row — the active-pair index guarantees the old one is no longer
-- active — so it notifies again.
CREATE UNIQUE INDEX uq_notifications_recipient_kind_connection
  ON notifications (user_id, notif_type, connection_id)
  WHERE connection_id IS NOT NULL;

-- Referential scan for the new FK (CASCADE), partial because every legacy
-- and project-home row carries NULL here.
CREATE INDEX idx_notifications_connection
  ON notifications (connection_id) WHERE connection_id IS NOT NULL;

-- migrate:down

DELETE FROM notifications
 WHERE notif_type IN ('community_connection_requested',
                      'community_connection_accepted',
                      'community_connection_rejected',
                      'community_connection_removed');

DROP INDEX idx_notifications_connection;
DROP INDEX uq_notifications_recipient_kind_connection;

ALTER TABLE notifications
  DROP CONSTRAINT notifications_shape_check;

ALTER TABLE notifications
  ADD CONSTRAINT notifications_project_home_shape_check CHECK (
    (notif_type IN ('project_home_requested',
                    'project_home_accepted',
                    'project_home_rejected',
                    'project_home_removed')
      AND project_id IS NOT NULL
      AND community_id IS NOT NULL
      AND relation_id IS NOT NULL
      AND message IS NULL
      AND post_id IS NULL)
    OR
    (notif_type NOT IN ('project_home_requested',
                        'project_home_accepted',
                        'project_home_rejected',
                        'project_home_removed')
      AND project_id IS NULL
      AND community_id IS NULL
      AND relation_id IS NULL
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
                   'project_home_removed')
  );

ALTER TABLE notifications
  DROP COLUMN connection_id;
