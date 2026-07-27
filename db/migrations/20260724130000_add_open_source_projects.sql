-- migrate:up

-- open_source_projects: the permanent verified-project record produced by
-- completing a project-onboarding draft. Unlike drafts it survives user
-- and draft deletion: source_onboarding_draft_id is provenance only
-- (SET NULL — user deletion cascades drafts, and admin-imported projects
-- may never have had one), and created_by_user_id likewise records who
-- created the project, never who administers it — administration is
-- derived from project_stewards. The forge_namespace_* triple is the
-- GitHub account verified during onboarding; the numeric namespace id is
-- the stable identity (logins are renameable, so authorization must
-- never be derived from the login). forge is closed to 'github' for the
-- MVP on purpose — widening it is a deliberate later migration, not a
-- default. Reserved slugs are a route concern owned by the future pure
-- domain parser, not SQL. name/description/website_url store what the
-- future parser hands over verbatim (no SQL trimming or normalizing);
-- SQL only pins the durable structural bounds. No tokens, secrets,
-- OAuth material, session bindings, private-repository metadata, or raw
-- GitHub JSON.
CREATE TABLE open_source_projects (
  id                         BIGSERIAL   PRIMARY KEY,
  source_onboarding_draft_id BIGINT      REFERENCES project_onboarding_drafts(id)
                                         ON DELETE SET NULL,
  name                       TEXT        NOT NULL
                                         CHECK (name = btrim(name)
                                            AND char_length(name) BETWEEN 1 AND 120),
  slug                       TEXT        NOT NULL
                                         CHECK (slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'
                                            AND char_length(slug) <= 80),
  description                TEXT        CHECK (description IS NULL
                                            OR char_length(description) <= 2000),
  website_url                TEXT        CHECK (website_url IS NULL
                                            OR (website_url = btrim(website_url)
                                                AND website_url <> ''
                                                AND char_length(website_url) <= 2048)),
  kind                       TEXT        NOT NULL
                                         CHECK (kind IN ('project', 'organization',
                                                         'ecosystem', 'foundation',
                                                         'working_group', 'other')),
  forge                      TEXT        NOT NULL DEFAULT 'github'
                                         CHECK (forge = 'github'),
  forge_namespace_id         BIGINT      NOT NULL CHECK (forge_namespace_id > 0),
  forge_namespace_login      TEXT        NOT NULL
                                         CHECK (forge_namespace_login = btrim(forge_namespace_login)
                                            AND forge_namespace_login <> ''
                                            AND char_length(forge_namespace_login) <= 255),
  forge_namespace_type       TEXT        NOT NULL
                                         CHECK (forge_namespace_type IN ('user', 'organization')),
  verification_status        TEXT        NOT NULL DEFAULT 'verified'
                                         CHECK (verification_status IN ('verified', 'stale',
                                                                        'revoked')),
  created_by_user_id         INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  created_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at                 TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT open_source_projects_slug_key UNIQUE (slug),
  CONSTRAINT open_source_projects_updated_after_created_check
    CHECK (updated_at >= created_at)
);

-- While its source draft still exists it can have produced at most one
-- permanent project; once the draft is deleted the reference goes NULL
-- and leaves the index. NULLs are deliberately unconstrained (draftless
-- projects are legitimate).
CREATE UNIQUE INDEX uniq_open_source_projects_source_draft
  ON open_source_projects (source_onboarding_draft_id)
  WHERE source_onboarding_draft_id IS NOT NULL;

-- The future refresh/revocation jobs scan by status.
CREATE INDEX idx_open_source_projects_verification_status
  ON open_source_projects (verification_status);
-- "projects verified for this GitHub account" lookups.
CREATE INDEX idx_open_source_projects_namespace
  ON open_source_projects (forge_namespace_id, forge_namespace_type);

-- project_stewards: who administers a permanent project. The steward
-- link is the authorization source — never created_by_user_id and never
-- github_installations.connected_by_user_id (both are provenance).
-- github_installation_record_id is the LOCAL github_installations
-- primary key (never GitHub's external installation id, hence "record"
-- in the name): the verified installation through which this steward
-- initially established authority. RESTRICT: an installation row must
-- not be deleted from under a steward's proof. Deleting the Earde user
-- removes only their stewardship; the project remains. One row per
-- (project, user); the MVP creates a single steward but the shape
-- already admits more.
CREATE TABLE project_stewards (
  project_id                    BIGINT      NOT NULL
                                            REFERENCES open_source_projects(id) ON DELETE CASCADE,
  user_id                       INTEGER     NOT NULL
                                            REFERENCES users(id) ON DELETE CASCADE,
  github_installation_record_id BIGINT      NOT NULL
                                            REFERENCES github_installations(id) ON DELETE RESTRICT,
  role                          TEXT        NOT NULL DEFAULT 'steward'
                                            CHECK (role = 'steward'),
  created_at                    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  PRIMARY KEY (project_id, user_id)
);

-- "projects this user stewards" and the ON DELETE CASCADE scan from
-- users. (project_id lookups ride the primary key.)
CREATE INDEX idx_project_stewards_user_id
  ON project_stewards (user_id);
-- Serves the ON DELETE RESTRICT check from github_installations.
CREATE INDEX idx_project_stewards_installation_record_id
  ON project_stewards (github_installation_record_id);

-- project_repositories: the finalized repository set of a permanent
-- project, copied from the selected draft snapshot by the future atomic
-- finalization store in contiguous position order. Unlike the draft
-- snapshot, github_repository_id is GLOBALLY unique — one GitHub
-- repository belongs to at most one permanent project; this single
-- constraint is the duplicate-claim rule and the arbiter of concurrent
-- finalization races (draft snapshots stay exempt: competing drafts may
-- hold the same repository until one wins). full_name is unique only
-- per project — display identity, not the claim. At most one primary
-- per project lives here; "at least one repository" and
-- "kind = project requires exactly one primary" are cross-row rules
-- owned by the finalization store, deliberately not schema (and not
-- triggers). default_branch may contain '/'; archived public
-- repositories remain valid members. The GitHub client and draft read
-- model stay the byte-level gatekeepers of full_name/html_url. No
-- tokens, secrets, or raw GitHub JSON.
CREATE TABLE project_repositories (
  id                   BIGSERIAL   PRIMARY KEY,
  project_id           BIGINT      NOT NULL
                                   REFERENCES open_source_projects(id) ON DELETE CASCADE,
  position             INTEGER     NOT NULL CHECK (position > 0),
  github_repository_id BIGINT      NOT NULL CHECK (github_repository_id > 0),
  full_name            TEXT        NOT NULL CHECK (btrim(full_name) <> ''),
  html_url             TEXT        NOT NULL CHECK (btrim(html_url) <> ''),
  description          TEXT,
  default_branch       TEXT        NOT NULL CHECK (btrim(default_branch) <> ''),
  is_primary           BOOLEAN     NOT NULL DEFAULT FALSE,
  is_archived          BOOLEAN     NOT NULL,
  created_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at           TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT project_repositories_github_repository_id_key
    UNIQUE (github_repository_id),
  CONSTRAINT project_repositories_project_position_key
    UNIQUE (project_id, position),
  CONSTRAINT project_repositories_project_full_name_key
    UNIQUE (project_id, full_name),
  CONSTRAINT project_repositories_updated_after_created_check
    CHECK (updated_at >= created_at)
);

-- At most one primary repository per project.
CREATE UNIQUE INDEX uniq_project_repositories_primary_per_project
  ON project_repositories (project_id)
  WHERE is_primary;

-- Repository listing for a project page and the ON DELETE CASCADE scan
-- from open_source_projects.
CREATE INDEX idx_project_repositories_project_id
  ON project_repositories (project_id);

-- migrate:down

DROP TABLE project_repositories;
DROP TABLE project_stewards;
DROP TABLE open_source_projects;
