-- migrate:up

-- "Start thread from chat" provenance: which source row is the conversation's seed.
-- The seed is the one chat message a thread was crystallized from; context rows are
-- nearby messages attached for durable knowledge. Reuses the existing
-- thread_source_messages shape (post_id, message_id, position) — no id/created_at.
ALTER TABLE thread_source_messages
  ADD COLUMN is_seed BOOLEAN NOT NULL DEFAULT FALSE;

-- Product invariant (rule D): one chat message seeds AT MOST one canonical thread.
-- Partial unique index gives us the guard race-safely at the DB level, so a double
-- submit fails on INSERT rather than silently forking a duplicate thread. Non-seed
-- inclusion stays unconstrained: a message may be context in several threads.
CREATE UNIQUE INDEX uniq_thread_source_seed_message
  ON thread_source_messages (message_id) WHERE is_seed;

-- migrate:down
DROP INDEX IF EXISTS uniq_thread_source_seed_message;
ALTER TABLE thread_source_messages DROP COLUMN IF EXISTS is_seed;
