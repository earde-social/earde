-- migrate:up

-- github_installations: durable identity of a VERIFIED GitHub App installation.
-- Rows exist only after the installation has been verified against an
-- authenticated GitHub user — there is deliberately no 'pending' status; an
-- unverified installation lives only as an untrusted id on an onboarding
-- state row. github_installation_id / github_account_id are GitHub's external
-- ids (BIGINT: GitHub ids exceed INTEGER range), never Earde database ids.
-- The GitHub account id is the authoritative account identity; the login is
-- display-only and may change, so it is neither unique nor an identity key.
-- No tokens or secrets of any kind are stored here.
CREATE TABLE github_installations (
  id                     BIGSERIAL   PRIMARY KEY,
  github_installation_id BIGINT      NOT NULL UNIQUE
                                     CHECK (github_installation_id > 0),
  github_account_id      BIGINT      NOT NULL CHECK (github_account_id > 0),
  github_account_login   TEXT        NOT NULL
                                     CHECK (btrim(github_account_login) <> ''),
  github_account_type    TEXT        NOT NULL
                                     CHECK (github_account_type IN ('user', 'organization')),
  -- Audit fact, not ownership: the installation belongs to the GitHub
  -- account, so it must outlive the Earde user who happened to connect it
  -- (SET NULL mirrors reports.resolved_by_user_id, the audit convention).
  connected_by_user_id   INTEGER     REFERENCES users(id) ON DELETE SET NULL,
  status                 TEXT        NOT NULL DEFAULT 'active'
                                     CHECK (status IN ('active', 'revoked', 'inaccessible')),
  created_at             TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at             TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  revoked_at             TIMESTAMPTZ,
  CONSTRAINT github_installations_revoked_at_check
    CHECK (revoked_at IS NULL OR status = 'revoked')
);

-- github_onboarding_states: short-lived, single-use GitHub callback state.
-- Only server-side hashes are stored — never the raw callback state or a raw
-- session id — so a database leak cannot be replayed against the callback.
-- pending_github_installation_id is the UNTRUSTED installation id echoed back
-- by GitHub mid-flow; it is not verified yet, hence no FK to
-- github_installations and a name that says so.
CREATE TABLE github_onboarding_states (
  id                             BIGSERIAL   PRIMARY KEY,
  state_hash                     TEXT        NOT NULL UNIQUE
                                             CHECK (btrim(state_hash) <> ''),
  -- Ephemeral per-user secret material: must die with the user (CASCADE,
  -- like password_resets).
  user_id                        INTEGER     NOT NULL
                                             REFERENCES users(id) ON DELETE CASCADE,
  session_binding_hash           TEXT        NOT NULL
                                             CHECK (btrim(session_binding_hash) <> ''),
  flow                           TEXT        NOT NULL
                                             CHECK (flow IN ('project_onboarding')),
  pending_github_installation_id BIGINT      CHECK (pending_github_installation_id IS NULL
                                                    OR pending_github_installation_id > 0),
  expires_at                     TIMESTAMPTZ NOT NULL,
  consumed_at                    TIMESTAMPTZ,
  created_at                     TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  CONSTRAINT github_onboarding_states_expires_after_created_check
    CHECK (expires_at > created_at),
  CONSTRAINT github_onboarding_states_consumed_after_created_check
    CHECK (consumed_at IS NULL OR consumed_at >= created_at)
);

-- user_id: serves "states for this user" and the ON DELETE CASCADE scan.
-- expires_at: serves the eventual expired-state cleanup sweep.
CREATE INDEX idx_github_onboarding_states_user_id
  ON github_onboarding_states (user_id);
CREATE INDEX idx_github_onboarding_states_expires_at
  ON github_onboarding_states (expires_at);

-- migrate:down

DROP TABLE github_onboarding_states;
DROP TABLE github_installations;
