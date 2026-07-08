-- migrate:up

-- A text channel within a community ("server"). Mirrors community_sections'
-- slug/position conventions, but kept as its own table so forum sections and
-- chat channels never have to share a schema. ON DELETE CASCADE mirrors
-- posts→communities.
CREATE TABLE channels (
  id           SERIAL PRIMARY KEY,
  community_id INT  NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  slug         TEXT NOT NULL,
  name         TEXT NOT NULL,
  topic        TEXT,
  position     INT  NOT NULL DEFAULT 0,
  is_archived  BOOLEAN NOT NULL DEFAULT FALSE,
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE (community_id, slug)
);

-- Durable chat. Postgres is the source of truth; a later Redis bus only fans
-- out copies for live delivery. BIGSERIAL because chat out-volumes posts.
-- user_id is nullable so anonymize_user (GDPR) can tombstone the author while
-- the message row survives. Soft-delete (deleted_at) mirrors the posts/comments
-- tombstone pattern. search_tsv is maintained later by the Search_indexer
-- (Step 8); left NULL for now.
CREATE TABLE chat_messages (
  id         BIGSERIAL PRIMARY KEY,
  channel_id INT  NOT NULL REFERENCES channels(id) ON DELETE CASCADE,
  user_id    INT       REFERENCES users(id),
  content    TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  edited_at  TIMESTAMP,
  deleted_at TIMESTAMP,
  search_tsv tsvector
);

-- Archive paging and SSE Last-Event-ID resume both read by (channel_id, id).
CREATE INDEX idx_chat_messages_channel_id ON chat_messages (channel_id, id);

-- Full-text search over chat archive (populated in Step 8).
CREATE INDEX idx_chat_messages_search_tsv ON chat_messages USING GIN (search_tsv);

-- Promotion provenance: which channel a thread was promoted from.
-- ON DELETE SET NULL so deleting a channel never destroys a durable thread.
ALTER TABLE posts ADD COLUMN promoted_from_channel_id INT
  REFERENCES channels(id) ON DELETE SET NULL;

-- Which chat messages seeded which durable thread, in display order.
-- Both FKs CASCADE: the link is meaningless once either side is hard-deleted.
CREATE TABLE thread_source_messages (
  post_id    INT    NOT NULL REFERENCES posts(id)         ON DELETE CASCADE,
  message_id BIGINT NOT NULL REFERENCES chat_messages(id) ON DELETE CASCADE,
  position   INT    NOT NULL DEFAULT 0,
  PRIMARY KEY (post_id, message_id)
);

-- migrate:down
DROP TABLE IF EXISTS thread_source_messages;
ALTER TABLE posts DROP COLUMN IF EXISTS promoted_from_channel_id;
DROP TABLE IF EXISTS chat_messages;
DROP TABLE IF EXISTS channels;
