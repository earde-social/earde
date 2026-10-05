-- migrate:up

-- Every post listing (community home and sections, /feed, /search, profiles)
-- reads each listed post's comment count with a correlated
-- "SELECT COUNT(*) FROM comments WHERE post_id = p.id", and several read its
-- score from post_votes the same way. With only primary keys, each of those
-- was a sequential scan per post row, and an offset page computes them for
-- every skipped row too: a 20k-post / 60k-comment community home took
-- minutes. post_votes' primary key leads with user_id, so it cannot serve a
-- lookup by post. Created without CONCURRENTLY: upgrades stop every writer
-- before migrating (docs/deployment.md).
CREATE INDEX idx_comments_post_id ON comments (post_id);
CREATE INDEX idx_post_votes_post_id ON post_votes (post_id);

-- migrate:down

DROP INDEX idx_post_votes_post_id;
DROP INDEX idx_comments_post_id;
