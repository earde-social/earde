-- migrate:up

-- project_onboarding_drafts: a user-owned working draft of "bring your
-- project to Earde", created only after a GitHub installation has been
-- verified. user_id is the explicit draft owner — deliberately NOT
-- github_installations.connected_by_user_id, which stays installation
-- provenance (audit) only. github_installation_record_id is the LOCAL
-- github_installations primary key, never GitHub's external installation
-- id (that lives on github_installations.github_installation_id, hence
-- the "record" in the name). RESTRICT: an installation row must not be
-- deleted from under a draft.
-- There is deliberately no 'expired' status: expiry is derived
-- (status = 'active' AND expires_at <= NOW()), so cleanup needs no
-- state-transition job and an expired-but-active row remains the single
-- refreshable draft for its slot. No default for expires_at: the future
-- store supplies an explicit lifetime (initially planned as 24 hours).
-- No public token, resume token, UUID, or bearer secret: drafts are
-- addressed by internal id plus strict owner authorization.
CREATE TABLE project_onboarding_drafts (
  id                            BIGSERIAL   PRIMARY KEY,
  user_id                       INTEGER     NOT NULL
                                            REFERENCES users(id) ON DELETE CASCADE,
  github_installation_record_id BIGINT      NOT NULL
                                            REFERENCES github_installations(id) ON DELETE RESTRICT,
  status                        TEXT        NOT NULL DEFAULT 'active'
                                            CHECK (status IN ('active', 'completed', 'cancelled')),
  expires_at                    TIMESTAMPTZ NOT NULL,
  completed_at                  TIMESTAMPTZ,
  cancelled_at                  TIMESTAMPTZ,
  created_at                    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at                    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- Exactly one lifecycle-timestamp shape per status; every inconsistent
  -- combination is rejected.
  CONSTRAINT project_onboarding_drafts_lifecycle_check
    CHECK ((status = 'active'    AND completed_at IS NULL     AND cancelled_at IS NULL)
        OR (status = 'completed' AND completed_at IS NOT NULL AND cancelled_at IS NULL)
        OR (status = 'cancelled' AND completed_at IS NULL     AND cancelled_at IS NOT NULL)),
  CONSTRAINT project_onboarding_drafts_expires_after_created_check
    CHECK (expires_at > created_at),
  CONSTRAINT project_onboarding_drafts_updated_after_created_check
    CHECK (updated_at >= created_at)
);

-- At most one ACTIVE draft per (owner, installation record). Completed and
-- cancelled drafts leave the index, so a fresh active draft is allowed
-- after either terminal state; different users may each hold an active
-- draft for the same installation, and one user may hold active drafts
-- for different installations. An expired-but-still-active row keeps
-- occupying the slot on purpose — the store refreshes it in place rather
-- than inserting a sibling.
CREATE UNIQUE INDEX uniq_project_onboarding_drafts_active_per_user_installation
  ON project_onboarding_drafts (user_id, github_installation_record_id)
  WHERE status = 'active';

-- "my active drafts" lookups and the ON DELETE CASCADE scan from users.
CREATE INDEX idx_project_onboarding_drafts_user_id_active
  ON project_onboarding_drafts (user_id)
  WHERE status = 'active';
-- Serves the ON DELETE RESTRICT check from github_installations and
-- drafts-per-installation lookups.
CREATE INDEX idx_project_onboarding_drafts_installation_record_id
  ON project_onboarding_drafts (github_installation_record_id);

-- project_onboarding_draft_repositories: the verified PUBLIC-repository
-- snapshot belonging to one draft, in original GitHub response order
-- (position, from 1). github_repository_id / github_owner_id are GitHub's
-- external ids (BIGINT: full int64 range, never narrowed to the local
-- integer users.id type). The GitHub client remains the byte-level
-- gatekeeper — owner/name structure, control-byte rejection, canonical
-- full name and URL, public-only filtering, verified owner id — so SQL
-- only protects durable structural invariants. A snapshot row is NOT a
-- project claim: the same GitHub repository may legitimately appear in
-- drafts of different users and in successive drafts; global repository
-- uniqueness belongs to the future project_repositories table, hence
-- only per-draft uniqueness here. Archived public repositories stay in
-- the snapshot and stay selectable — any warning is UI policy, not
-- schema. No tokens, secrets, raw response blobs, or metadata JSON.
CREATE TABLE project_onboarding_draft_repositories (
  id                   BIGSERIAL   PRIMARY KEY,
  draft_id             BIGINT      NOT NULL
                                   REFERENCES project_onboarding_drafts(id) ON DELETE CASCADE,
  position             INTEGER     NOT NULL CHECK (position > 0),
  -- Named: the generated default would exceed the 63-byte identifier
  -- limit and truncate.
  github_repository_id BIGINT      NOT NULL
                                   CONSTRAINT project_onboarding_draft_repos_github_repository_id_check
                                   CHECK (github_repository_id > 0),
  github_owner_id      BIGINT      NOT NULL CHECK (github_owner_id > 0),
  owner_login          TEXT        NOT NULL CHECK (btrim(owner_login) <> ''),
  name                 TEXT        NOT NULL CHECK (btrim(name) <> ''),
  full_name            TEXT        NOT NULL CHECK (btrim(full_name) <> ''),
  html_url             TEXT        NOT NULL CHECK (btrim(html_url) <> ''),
  description          TEXT,
  default_branch       TEXT        NOT NULL CHECK (btrim(default_branch) <> ''),
  is_archived          BOOLEAN     NOT NULL,
  is_selected          BOOLEAN     NOT NULL DEFAULT FALSE,
  is_primary           BOOLEAN     NOT NULL DEFAULT FALSE,
  created_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  -- A primary repository must also be selected. "At least one selected"
  -- is a cross-row rule owned by the domain operation that completes
  -- project setup, and whether a primary is required depends on project
  -- kind — neither belongs in the schema.
  CONSTRAINT project_onboarding_draft_repositories_primary_selected_check
    CHECK (is_selected OR NOT is_primary),
  -- Named for the same 63-byte reason as above.
  CONSTRAINT project_onboarding_draft_repos_draft_position_key
    UNIQUE (draft_id, position),
  CONSTRAINT project_onboarding_draft_repos_draft_repository_id_key
    UNIQUE (draft_id, github_repository_id),
  CONSTRAINT project_onboarding_draft_repos_draft_full_name_key
    UNIQUE (draft_id, full_name)
);

-- At most one primary repository per draft.
CREATE UNIQUE INDEX uniq_project_onboarding_draft_repos_primary_per_draft
  ON project_onboarding_draft_repositories (draft_id)
  WHERE is_primary;

-- migrate:down

DROP TABLE project_onboarding_draft_repositories;
DROP TABLE project_onboarding_drafts;
