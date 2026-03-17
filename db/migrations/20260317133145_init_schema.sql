-- migrate:up

-- rate_limits has no FK — tracks anonymous IPs before any user context exists.
CREATE TABLE IF NOT EXISTS rate_limits (
  ip_address   TEXT NOT NULL,
  endpoint     TEXT NOT NULL,
  attempts     INT  NOT NULL DEFAULT 1,
  window_start REAL NOT NULL,
  PRIMARY KEY (ip_address, endpoint)
);

-- dream_session schema mirrors Dream's internal expectation exactly.
-- Defined explicitly to avoid coupling to Dream's private module hierarchy.
CREATE TABLE IF NOT EXISTS dream_session (
  id         TEXT NOT NULL PRIMARY KEY,
  label      TEXT NOT NULL,
  expires_at REAL NOT NULL,
  payload    TEXT NOT NULL
);

-- Tables created in FK-safe order: independent tables first, then dependents.
CREATE TABLE IF NOT EXISTS users (
  id                 SERIAL PRIMARY KEY,
  username           TEXT NOT NULL UNIQUE,
  email              TEXT NOT NULL UNIQUE,
  password_hash      TEXT NOT NULL,
  verification_token TEXT,
  is_email_verified  BOOLEAN NOT NULL DEFAULT FALSE,
  is_admin           BOOLEAN NOT NULL DEFAULT FALSE,
  is_banned          BOOLEAN NOT NULL DEFAULT FALSE,
  bio                TEXT,
  avatar_url         TEXT,
  created_at         TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  last_active_at     TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS communities (
  id          SERIAL PRIMARY KEY,
  slug        TEXT NOT NULL UNIQUE,
  name        TEXT NOT NULL,
  description TEXT,
  rules       TEXT,
  avatar_url  TEXT,
  banner_url  TEXT
);

-- posts.user_id omits ON DELETE CASCADE — accounts are tombstoned, not hard-deleted.
CREATE TABLE IF NOT EXISTS posts (
  id           SERIAL PRIMARY KEY,
  title        TEXT NOT NULL,
  url          TEXT,
  content      TEXT,
  community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  user_id      INT NOT NULL REFERENCES users(id),
  created_at   TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS comments (
  id         SERIAL PRIMARY KEY,
  content    TEXT NOT NULL,
  post_id    INT NOT NULL REFERENCES posts(id)    ON DELETE CASCADE,
  user_id    INT NOT NULL REFERENCES users(id),
  parent_id  INT          REFERENCES comments(id) ON DELETE CASCADE,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS post_votes (
  user_id   INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  post_id   INT NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
  direction INT NOT NULL,
  PRIMARY KEY (user_id, post_id)
);

CREATE TABLE IF NOT EXISTS comment_votes (
  user_id    INT NOT NULL REFERENCES users(id)    ON DELETE CASCADE,
  comment_id INT NOT NULL REFERENCES comments(id) ON DELETE CASCADE,
  direction  INT NOT NULL,
  PRIMARY KEY (user_id, comment_id)
);

CREATE TABLE IF NOT EXISTS community_members (
  user_id      INT NOT NULL REFERENCES users(id)       ON DELETE CASCADE,
  community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  PRIMARY KEY (user_id, community_id)
);

CREATE TABLE IF NOT EXISTS notifications (
  id         SERIAL PRIMARY KEY,
  user_id    INT NOT NULL REFERENCES users(id)  ON DELETE CASCADE,
  post_id    INT NOT NULL REFERENCES posts(id)  ON DELETE CASCADE,
  message    TEXT NOT NULL,
  is_read    BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- page_views has no FK — anonymous views tracked without a user reference.
CREATE TABLE IF NOT EXISTS page_views (
  id         SERIAL PRIMARY KEY,
  path       TEXT NOT NULL,
  referer    TEXT,
  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- community_moderators depends on users + communities, created after both.
CREATE TABLE IF NOT EXISTS community_moderators (
  user_id      INT NOT NULL REFERENCES users(id)       ON DELETE CASCADE,
  community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  promoted_at  TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, community_id)
);

-- community_bans: community-scoped exile; does not affect global login.
CREATE TABLE IF NOT EXISTS community_bans (
  user_id      INT NOT NULL REFERENCES users(id)       ON DELETE CASCADE,
  community_id INT NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  banned_at    TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (user_id, community_id)
);

-- password_resets FK-references users — cleared via CASCADE on user delete.
CREATE TABLE IF NOT EXISTS password_resets (
  id         SERIAL PRIMARY KEY,
  user_id    INT  NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  token      TEXT NOT NULL UNIQUE,
  expires_at TIMESTAMPTZ NOT NULL
);

-- migrate:down
DROP TABLE IF EXISTS password_resets;
DROP TABLE IF EXISTS community_bans;
DROP TABLE IF EXISTS community_moderators;
DROP TABLE IF EXISTS page_views;
DROP TABLE IF EXISTS notifications;
DROP TABLE IF EXISTS community_members;
DROP TABLE IF EXISTS comment_votes;
DROP TABLE IF EXISTS post_votes;
DROP TABLE IF EXISTS comments;
DROP TABLE IF EXISTS posts;
DROP TABLE IF EXISTS communities;
DROP TABLE IF EXISTS users;
DROP TABLE IF EXISTS dream_session;
DROP TABLE IF EXISTS rate_limits;
