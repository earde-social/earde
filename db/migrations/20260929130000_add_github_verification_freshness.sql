-- migrate:up

-- GitHub verification freshness, access loss and repository-claim release.
-- See docs/features/github-verification-lifecycle.md.
--
-- Before this, GitHub access was checked once, at onboarding, and the stored
-- proof then granted authority for ever: a steward who had since lost access
-- to the repositories could still request a community home or create one,
-- the project kept its "verified" label, and its exclusive repository claim
-- could never be released, so it could block the repository's current owner
-- permanently.

-- The one freshness window. Evidence is fresh for 30 days after the last
-- successful GitHub verification that covered it.
CREATE FUNCTION github_evidence_is_fresh(verified_at timestamptz) RETURNS boolean
LANGUAGE sql STABLE AS $$
    SELECT verified_at > NOW() - INTERVAL '30 days';
$$;

-- When the GitHub verification behind a draft's snapshot happened. The
-- callback sets it on every successful verification; selection never does.
ALTER TABLE project_onboarding_drafts ADD COLUMN verified_at timestamptz;
-- Drafts were always refreshed with a 24-hour expiry.
UPDATE project_onboarding_drafts SET verified_at = expires_at - INTERVAL '24 hours';
ALTER TABLE project_onboarding_drafts
    ALTER COLUMN verified_at SET DEFAULT NOW(),
    ALTER COLUMN verified_at SET NOT NULL;

-- Actor-bound evidence: when this steward last proved, through GitHub, access
-- to every repository the project currently claims.
ALTER TABLE project_stewards ADD COLUMN github_verified_at timestamptz;
-- Existing rows get the earliest time their proof can date from: the draft
-- behind them was verified at most 24 hours before finalization.
UPDATE project_stewards SET github_verified_at = created_at - INTERVAL '24 hours';
ALTER TABLE project_stewards
    ALTER COLUMN github_verified_at SET DEFAULT NOW(),
    ALTER COLUMN github_verified_at SET NOT NULL;

-- The status shown and relied on: a stored 'verified' only holds while some
-- steward's evidence is fresh. 'stale' and 'revoked' pass through.
CREATE FUNCTION project_github_verification(target bigint, stored text) RETURNS text
LANGUAGE sql STABLE AS $$
    SELECT CASE
             WHEN stored = 'verified' AND NOT EXISTS (
                    SELECT 1 FROM project_stewards s
                    WHERE s.project_id = target
                      AND github_evidence_is_fresh(s.github_verified_at))
             THEN 'stale'
             ELSE stored
           END;
$$;

-- A repository claim can be released. The row is kept as history, and only
-- unreleased rows are exclusive.
ALTER TABLE project_repositories ADD COLUMN released_at timestamptz;
ALTER TABLE project_repositories
    ADD CONSTRAINT project_repositories_released_after_created_check
    CHECK (released_at IS NULL OR released_at >= created_at);
ALTER TABLE project_repositories
    DROP CONSTRAINT project_repositories_github_repository_id_key;
CREATE UNIQUE INDEX uniq_project_repositories_active_claim
    ON project_repositories (github_repository_id)
    WHERE released_at IS NULL;

-- migrate:down

DROP INDEX IF EXISTS uniq_project_repositories_active_claim;
ALTER TABLE project_repositories
    ADD CONSTRAINT project_repositories_github_repository_id_key UNIQUE (github_repository_id);
ALTER TABLE project_repositories
    DROP CONSTRAINT IF EXISTS project_repositories_released_after_created_check;
ALTER TABLE project_repositories DROP COLUMN IF EXISTS released_at;
DROP FUNCTION IF EXISTS project_github_verification(bigint, text);
ALTER TABLE project_stewards DROP COLUMN IF EXISTS github_verified_at;
ALTER TABLE project_onboarding_drafts DROP COLUMN IF EXISTS verified_at;
DROP FUNCTION IF EXISTS github_evidence_is_fresh(timestamptz);
