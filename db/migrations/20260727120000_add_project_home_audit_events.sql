-- migrate:up

-- project_home_audit_events: the append-only audit history of the
-- GitHub-project/community-home lifecycle. One row per successfully
-- committed durable transition, written inside the same transaction as
-- the transition itself, so no committed lifecycle mutation can exist
-- without its event and no event can describe a rolled-back mutation.
-- mod_actions is deliberately not reused: it is a public community
-- moderation log with moderator-only semantics (moderator_id NOT NULL,
-- required public reason, INTEGER target_id that cannot address BIGINT
-- project/relation rows) and ON DELETE CASCADE on both community and
-- moderator — deleting an actor there erases history, which audit must
-- survive.
-- The event body is foreign keys plus a closed action vocabulary and a
-- timestamp; nothing else. No request note, submitted text, GitHub
-- namespace/repository names, tokens, OAuth/PKCE material, session
-- values, authorization provenance, or SQL diagnostics are stored.
-- actor_user_id follows the established provenance convention
-- (community_projects.requested_by/reviewed_by): SET NULL so account
-- deletion never erases the historical event. The subject FKs
-- (project, community, relation) deliberately carry no ON DELETE
-- action: nothing in the application deletes those rows today, and if
-- a deletion path ever appears it must confront the audit history
-- explicitly rather than silently cascading it away.
CREATE TABLE project_home_audit_events (
  id            BIGSERIAL   PRIMARY KEY,
  action        TEXT        NOT NULL
                            CHECK (action IN ('project_home_requested',
                                              'project_home_accepted',
                                              'project_home_rejected',
                                              'project_home_removed',
                                              'dedicated_home_provisioned',
                                              'network_community_published')),
  actor_user_id INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  project_id    BIGINT      NOT NULL REFERENCES open_source_projects(id),
  community_id  INTEGER     NOT NULL REFERENCES communities(id),
  relation_id   BIGINT      NOT NULL REFERENCES community_projects(id),
  created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Per-subject histories in reverse chronological order — one index per
-- future listing surface (project page, community modlog-style surface,
-- one relation's trail, one actor's trail). Each also serves its FK's
-- referential scans (SET NULL on user deletion, the NO ACTION checks on
-- the subject side). The BIGSERIAL primary key already covers plain id
-- lookups, so no further index exists.
CREATE INDEX idx_project_home_audit_events_project
  ON project_home_audit_events (project_id, created_at DESC);

CREATE INDEX idx_project_home_audit_events_community
  ON project_home_audit_events (community_id, created_at DESC);

CREATE INDEX idx_project_home_audit_events_relation
  ON project_home_audit_events (relation_id, created_at DESC);

CREATE INDEX idx_project_home_audit_events_actor
  ON project_home_audit_events (actor_user_id, created_at DESC);

-- migrate:down

DROP TABLE project_home_audit_events;
