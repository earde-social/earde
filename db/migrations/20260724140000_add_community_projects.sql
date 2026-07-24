-- migrate:up

-- community_projects: the relation between a permanent verified project
-- and its Earde home community. One table carries the whole lifecycle:
-- a pending request against an existing community, the accepted home
-- (created either directly by the future dedicated-community
-- provisioning transaction or by a target-moderator acceptance), and
-- the historical rejected/removed rows that stay behind for provenance.
-- Both FK ends are provenance-neutral on authorization: project-side
-- authority stays in project_stewards and community-side authority in
-- the moderator tables — nothing here grants a role.
-- requested_by/reviewed_by are provenance only (SET NULL so account
-- deletion never erases a home or its history); an auto-provisioned
-- accepted home legitimately has reviewed_by NULL while reviewed_at
-- still records when the home was established — no fake review is
-- invented. relation_type is closed to 'home' so a future non-home
-- project/community relation widens the vocabulary deliberately instead
-- of overloading rows. Valid lifecycle *transitions*
-- (pending→accepted/rejected, accepted→removed) are owned by the future
-- stores, not triggers; the shape check only guarantees each stored row
-- is structurally coherent. request_note is stored verbatim (the future
-- pure domain layer owns blank→NULL canonicalization, UTF-8/control
-- validation, and the same 2000-char limit); it is private workflow
-- data, never public content. No tokens, secrets, OAuth material,
-- session bindings, GitHub identifiers, or raw GitHub JSON.
CREATE TABLE community_projects (
  id                   BIGSERIAL   PRIMARY KEY,
  project_id           BIGINT      NOT NULL
                                   REFERENCES open_source_projects(id)
                                   ON DELETE CASCADE,
  community_id         INTEGER     NOT NULL
                                   REFERENCES communities(id)
                                   ON DELETE CASCADE,
  relation_type        TEXT        NOT NULL DEFAULT 'home'
                                   CHECK (relation_type = 'home'),
  status               TEXT        NOT NULL
                                   CHECK (status IN ('pending', 'accepted',
                                                     'rejected', 'removed')),
  requested_by_user_id INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  reviewed_by_user_id  INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  request_note         TEXT        CHECK (request_note IS NULL
                                      OR char_length(request_note) <= 2000),
  created_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  reviewed_at          TIMESTAMPTZ,
  removed_at           TIMESTAMPTZ,
  CONSTRAINT community_projects_updated_after_created_check
    CHECK (updated_at >= created_at),
  CONSTRAINT community_projects_reviewed_after_created_check
    CHECK (reviewed_at IS NULL OR reviewed_at >= created_at),
  CONSTRAINT community_projects_removed_after_created_check
    CHECK (removed_at IS NULL OR removed_at >= created_at),
  CONSTRAINT community_projects_removed_after_reviewed_check
    CHECK (removed_at IS NULL OR reviewed_at IS NULL
        OR removed_at >= reviewed_at),
  CONSTRAINT community_projects_status_shape_check
    CHECK (
      (status = 'pending' AND reviewed_at IS NULL AND removed_at IS NULL
                          AND reviewed_by_user_id IS NULL)
      OR (status = 'accepted' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL)
      OR (status = 'rejected' AND reviewed_at IS NOT NULL
                              AND removed_at IS NULL)
      OR (status = 'removed'  AND reviewed_at IS NOT NULL
                              AND removed_at IS NOT NULL)
    )
);

-- Load-bearing: at most one ACTIVE home relation per project, regardless
-- of target community — never two pendings, two accepteds, or a pending
-- alongside an accepted. This index (not application read-before-write
-- checks) arbitrates races between provisioning a dedicated home,
-- requesting an existing one, and concurrent submissions/acceptances.
-- It is deliberately stronger than an active (project_id, community_id)
-- pair rule — it subsumes it, so no such second index exists. Historical
-- rejected/removed rows fall outside the predicate and never occupy the
-- slot; after rejection or removal a fresh request may be created.
CREATE UNIQUE INDEX community_projects_one_active_home_idx
  ON community_projects (project_id)
  WHERE relation_type = 'home' AND status IN ('pending', 'accepted');

-- Target-community moderation queue: pending (and any status) listings
-- for one community, in arrival order.
CREATE INDEX idx_community_projects_community_status
  ON community_projects (community_id, status, created_at);

-- Full per-project relation history — active and historical rows alike;
-- the partial active-home index cannot serve this. Also the ON DELETE
-- CASCADE scan from open_source_projects.
CREATE INDEX idx_community_projects_project_id
  ON community_projects (project_id);

-- migrate:down

DROP TABLE community_projects;
