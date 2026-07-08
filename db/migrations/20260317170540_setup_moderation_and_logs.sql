-- migrate:up

ALTER TABLE community_moderators ADD COLUMN role VARCHAR(50) NOT NULL DEFAULT 'mod';

CREATE TABLE mod_actions (
  id            SERIAL PRIMARY KEY,
  community_id  INTEGER NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  moderator_id  INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  action_type   VARCHAR(50) NOT NULL,
  target_id     INTEGER,
  reason        TEXT NOT NULL,
  created_at    TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- migrate:down

DROP TABLE mod_actions;
ALTER TABLE community_moderators DROP COLUMN role;
