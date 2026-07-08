-- migrate:up

-- User content reports → community mod queue. Postgres is the source of truth
-- (durable knowledge rule). target_id is BIGINT, not INTEGER, because chat_messages.id
-- is BIGSERIAL; post/comment ids (INTEGER) widen into it losslessly. No FK on target_id:
-- it is polymorphic across posts/comments/chat_messages (mirrors mod_actions.target_id),
-- and a report must survive hard-deletion of its target (e.g. a channel cascade).
-- community_id and target_author_user_id are denormalized at insert time so the queue
-- never has to multi-hop join (comment→post→community) and so a deleted target still
-- shows who/what was reported.
CREATE TABLE reports (
  id                    SERIAL  PRIMARY KEY,
  community_id          INTEGER NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  reporter_user_id      INTEGER NOT NULL REFERENCES users(id)       ON DELETE CASCADE,
  target_type           TEXT    NOT NULL
                          CHECK (target_type IN ('post', 'comment', 'chat_message')),
  target_id             BIGINT  NOT NULL,
  target_author_user_id INTEGER REFERENCES users(id) ON DELETE SET NULL,
  reason                TEXT    NOT NULL
                          CHECK (reason IN ('spam', 'abuse', 'off_topic', 'illegal', 'other')),
  details               TEXT,
  status                TEXT    NOT NULL DEFAULT 'open'
                          CHECK (status IN ('open', 'dismissed', 'action_taken')),
  action_kind           TEXT
                          CHECK (action_kind IS NULL
                                 OR action_kind IN ('removed_content', 'banned_author', 'other')),
  resolution_note       TEXT,
  resolved_by_user_id   INTEGER REFERENCES users(id) ON DELETE SET NULL,
  resolved_at           TIMESTAMP,
  created_at            TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Hot path: "open reports for this community, newest first".
CREATE INDEX idx_reports_community_status
  ON reports (community_id, status, created_at DESC);

-- Dedup / anti-spam: at most one OPEN report per (reporter, target). Partial unique so a
-- reporter may re-report after their prior report is resolved (status leaves 'open', the
-- predicate no longer matches). A double-submit fails the INSERT instead of forking a row.
CREATE UNIQUE INDEX uniq_reports_open_per_reporter_target
  ON reports (reporter_user_id, target_type, target_id) WHERE status = 'open';

-- migrate:down

DROP TABLE IF EXISTS reports;
