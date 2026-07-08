-- migrate:up

-- community_user_stats: per-user reputation scoped to a single community.
-- Lifetime counters only — decrements on deletion are out of MVP scope.
-- ON DELETE CASCADE on both FKs: no orphan rows when users or communities are removed.
CREATE TABLE community_user_stats (
  id                  SERIAL PRIMARY KEY,
  user_id             INT       NOT NULL REFERENCES users(id)       ON DELETE CASCADE,
  community_id        INT       NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  local_karma         INT       NOT NULL DEFAULT 0,
  local_post_count    INT       NOT NULL DEFAULT 0,
  local_comment_count INT       NOT NULL DEFAULT 0,
  first_active_at     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (user_id, community_id)
);

-- Leaderboard query: rank members by local_karma within a community.
CREATE INDEX idx_community_user_stats_community_karma
  ON community_user_stats (community_id, local_karma);

-- Top contributors by post volume.
CREATE INDEX idx_community_user_stats_community_post_count
  ON community_user_stats (community_id, local_post_count);

-- migrate:down
DROP INDEX idx_community_user_stats_community_post_count;
DROP INDEX idx_community_user_stats_community_karma;
DROP TABLE community_user_stats;
