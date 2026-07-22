-- migrate:up

-- posthog_person_deletion_jobs: durable GDPR person-deletion requests against
-- PostHog (analytics spec §3.3). A deletion request must survive PostHog
-- outages and process crashes, so it is a database row, not a best-effort
-- HTTP call. Deliberately NO foreign key to users: the local row is
-- anonymized in the same transaction that enqueues this job, and the job must
-- stay independently actionable (and never block local deletion) afterwards.
-- distinct_id is the immutable "user:<database_user_id>" — never email,
-- username, a PostHog person UUID, or any credential.
CREATE TABLE posthog_person_deletion_jobs (
  id              BIGSERIAL   PRIMARY KEY,
  distinct_id     TEXT        NOT NULL UNIQUE,
  status          TEXT        NOT NULL DEFAULT 'pending'
                              CHECK (status IN ('pending', 'completed')),
  attempts        INT         NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  -- Bounded safe diagnostic class only (e.g. lookup_http_401, timeout) —
  -- never a response body, URL, token, or personal data.
  last_error      TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  last_attempt_at TIMESTAMPTZ,
  completed_at    TIMESTAMPTZ
);

-- UNIQUE (distinct_id) makes enqueueing idempotent: duplicate or concurrent
-- account-deletion requests for the same immutable distinct id converge on
-- ONE job (INSERT ... ON CONFLICT) instead of competing rows. The partial
-- index serves the retry path's oldest-pending scan and stays small as
-- completed rows accumulate.
CREATE INDEX idx_posthog_deletion_jobs_pending
  ON posthog_person_deletion_jobs (created_at)
  WHERE status = 'pending';

-- migrate:down
DROP INDEX idx_posthog_deletion_jobs_pending;
DROP TABLE posthog_person_deletion_jobs;
