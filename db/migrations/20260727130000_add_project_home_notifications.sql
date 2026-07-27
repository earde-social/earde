-- migrate:up

-- Project/community-home lifecycle notifications extend the existing
-- notifications table (the one notification center users already have)
-- instead of adding a parallel inbox. Legacy rows (comment_reply,
-- mention, mod_action) keep their prose message and optional post link;
-- the four project-home kinds are structured instead: durable subject
-- references (project, community, home relation) plus a closed kind,
-- rendered from current entity state at read time. No request note,
-- decision text, authorization provenance, GitHub identifiers, tokens,
-- OAuth/PKCE material, emails, usernames, or browser-supplied URLs are
-- ever stored.
--
-- Deletion semantics follow each side's established convention:
--   * recipient (user_id): ON DELETE CASCADE already on the table — a
--     deleted user keeps no inbox;
--   * actor: SET NULL, matching community_projects.requested_by/
--     reviewed_by and project_home_audit_events.actor_user_id — a
--     deleted actor never erases the recipient's notification;
--   * subjects (project, community, relation): CASCADE, matching the
--     table's own post_id convention — notifications are user-facing
--     surface, not audit history, so they follow their subject. The
--     append-only audit table independently RESTRICT-protects those
--     subject rows, so these cascades only ever fire behind an explicit
--     audit purge.
ALTER TABLE notifications
  ALTER COLUMN message DROP NOT NULL;

ALTER TABLE notifications
  ADD COLUMN actor_user_id INTEGER
    CONSTRAINT notifications_actor_user_id_fkey
    REFERENCES users(id) ON DELETE SET NULL,
  ADD COLUMN project_id BIGINT
    CONSTRAINT notifications_project_id_fkey
    REFERENCES open_source_projects(id) ON DELETE CASCADE,
  ADD COLUMN community_id INTEGER
    CONSTRAINT notifications_community_id_fkey
    REFERENCES communities(id) ON DELETE CASCADE,
  ADD COLUMN relation_id BIGINT
    CONSTRAINT notifications_relation_id_fkey
    REFERENCES community_projects(id) ON DELETE CASCADE;

-- Closing the vocabulary and closing the shapes are two separate
-- responsibilities, enforced by two separately named constraints.
--
-- First the vocabulary: notif_type has never had a database-side
-- vocabulary before this migration, so this CHECK enumerates every
-- kind the application has ever durably written — the three legacy
-- kinds create_notif emits and pages.ml renders (comment_reply, the
-- column default; mention; mod_action) — plus the four new closed
-- project-home kinds. Anything else is rejected durably, whatever row
-- shape it arrives in; the OCaml variants alone cannot guarantee that.
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

-- Then the shapes: a project-home row must carry exactly one of the
-- four closed kinds, all three subject references, and no prose
-- message or post link; any other (known-legacy) kind must keep the
-- legacy shape (prose message, no subject or actor columns). A legacy
-- kind can therefore never reference a home relation, a project-home
-- kind can never smuggle prose, and the legacy message NOT NULL
-- invariant survives the column-level DROP NOT NULL above.
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

-- Durable deduplication: at most one notification per recipient, kind,
-- and home relation. Relations are historical and never reused, so a
-- future replacement relation notifies again while a replayed mutation
-- cannot double-deliver.
CREATE UNIQUE INDEX uq_notifications_recipient_kind_relation
  ON notifications (user_id, notif_type, relation_id)
  WHERE relation_id IS NOT NULL;

-- Referential scans for the new FKs (CASCADE/SET NULL), partial because
-- legacy rows carry NULLs in all four columns.
CREATE INDEX idx_notifications_project
  ON notifications (project_id) WHERE project_id IS NOT NULL;

CREATE INDEX idx_notifications_community
  ON notifications (community_id) WHERE community_id IS NOT NULL;

CREATE INDEX idx_notifications_relation
  ON notifications (relation_id) WHERE relation_id IS NOT NULL;

CREATE INDEX idx_notifications_actor
  ON notifications (actor_user_id) WHERE actor_user_id IS NOT NULL;

-- migrate:down

DELETE FROM notifications
 WHERE notif_type IN ('project_home_requested',
                      'project_home_accepted',
                      'project_home_rejected',
                      'project_home_removed');

DROP INDEX idx_notifications_actor;
DROP INDEX idx_notifications_relation;
DROP INDEX idx_notifications_community;
DROP INDEX idx_notifications_project;
DROP INDEX uq_notifications_recipient_kind_relation;

ALTER TABLE notifications
  DROP CONSTRAINT notifications_project_home_shape_check;

ALTER TABLE notifications
  DROP CONSTRAINT notifications_notif_type_check;

ALTER TABLE notifications
  DROP COLUMN relation_id,
  DROP COLUMN community_id,
  DROP COLUMN project_id,
  DROP COLUMN actor_user_id;

ALTER TABLE notifications
  ALTER COLUMN message SET NOT NULL;
