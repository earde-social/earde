-- migrate:up

-- posthog_group_cleanup_jobs: durable privacy scrubs of a PostHog community
-- group profile (analytics spec §13). When a community turns fully private,
-- its previously sent human-readable group properties (community_name,
-- community_slug) must be removed from PostHog via the documented private
-- Groups API (POST /groups/delete_property/, body {"$unset": <key>}). That
-- removal must survive PostHog outages and process crashes, so it is a
-- database row enqueued in the SAME transaction as the visibility change —
-- never a best-effort HTTP call. group_key is the immutable
-- "community:<database_community_id>" — never a name, slug, or credential;
-- last_error holds a bounded diagnostic class only.
CREATE TABLE posthog_group_cleanup_jobs (
  id              BIGSERIAL   PRIMARY KEY,
  group_key       TEXT        NOT NULL UNIQUE,
  status          TEXT        NOT NULL DEFAULT 'pending'
                              CHECK (status IN ('pending', 'completed')),
  attempts        INT         NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  last_error      TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  last_attempt_at TIMESTAMPTZ,
  completed_at    TIMESTAMPTZ
);

-- UNIQUE (group_key): concurrent/duplicate transitions converge on ONE job
-- (INSERT ... ON CONFLICT); a later public->private transition re-arms a
-- completed job to pending instead of creating competitors. The partial
-- index serves the retry path's oldest-pending scan.
CREATE INDEX idx_posthog_group_cleanup_pending
  ON posthog_group_cleanup_jobs (created_at)
  WHERE status = 'pending';

-- migrate:down
DROP INDEX idx_posthog_group_cleanup_pending;
DROP TABLE posthog_group_cleanup_jobs;
