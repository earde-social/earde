-- migrate:up

-- pending_signups: unconfirmed signups. A bot or abandoned signup lands here and
-- NEVER creates a users row until the email confirmation link is clicked, so the
-- users table stays clean. BIGSERIAL because this is a high-churn, bot-exposed table.
-- Raw password/token are never stored: password_hash is argon2 (as in users);
-- token_hash is the SHA-256 of the token that was emailed (same scheme as password_resets).
CREATE TABLE pending_signups (
  id            BIGSERIAL   PRIMARY KEY,
  username      TEXT        NOT NULL,
  email         TEXT        NOT NULL,
  password_hash TEXT        NOT NULL,
  token_hash    TEXT        NOT NULL UNIQUE,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  expires_at    TIMESTAMPTZ NOT NULL,
  consumed_at   TIMESTAMPTZ,
  ip_address    TEXT,
  user_agent    TEXT
);

-- At most ONE active (unconsumed) pending per email / per username, case-insensitive
-- (humans treat usernames/emails case-insensitively). Consumed rows are excluded so a
-- past confirmation never blocks a later re-signup. NOTE: an expired-but-unconsumed row
-- still participates in these indexes, so the signup upsert clears expired collisions
-- before inserting rather than relying on a delayed sweep.
CREATE UNIQUE INDEX idx_pending_signups_email_active
  ON pending_signups (LOWER(email)) WHERE consumed_at IS NULL;
CREATE UNIQUE INDEX idx_pending_signups_username_active
  ON pending_signups (LOWER(username)) WHERE consumed_at IS NULL;

-- Supports expiry checks and the opportunistic cleanup sweep.
CREATE INDEX idx_pending_signups_expires_at ON pending_signups (expires_at);

-- migrate:down
DROP INDEX idx_pending_signups_expires_at;
DROP INDEX idx_pending_signups_username_active;
DROP INDEX idx_pending_signups_email_active;
DROP TABLE pending_signups;
