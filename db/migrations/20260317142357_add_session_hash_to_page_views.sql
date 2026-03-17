-- migrate:up
ALTER TABLE page_views ADD COLUMN session_hash TEXT NOT NULL DEFAULT '';

-- migrate:down
ALTER TABLE page_views DROP COLUMN session_hash;
