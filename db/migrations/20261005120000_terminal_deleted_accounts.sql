-- migrate:up

-- A deleted account is terminal. Deletion rewrites the row into the
-- tombstone '[deleted_<id>]' and blanks its password, but before this release
-- it left the account's password-reset links and its is_admin flag in place:
-- an old link could set a new password on the tombstone, log back in as
-- '[deleted_<id>]', and keep any admin authority. Repair every tombstone
-- already written, then make the terminal shape a constraint, so no later
-- write (a reset or password change racing a deletion, an old link, a future
-- code path) can give a tombstone a credential or authority again.

DELETE FROM password_resets
 WHERE user_id IN (SELECT id FROM users
                    WHERE username = '[deleted_' || id::text || ']');

-- Sessions a revived tombstone may have opened. Same match as the
-- application's session revocation: the user id inside Dream's payload.
DELETE FROM dream_session
 WHERE payload::jsonb ->> 'user_id' IN
       (SELECT id::text FROM users
         WHERE username = '[deleted_' || id::text || ']');

-- Clearing is_admin fires users_realtime_access, which moves every private
-- community to a new realtime generation.
UPDATE users
   SET password_hash = '', is_admin = FALSE
 WHERE username = '[deleted_' || id::text || ']'
   AND (password_hash <> '' OR is_admin);

ALTER TABLE users ADD CONSTRAINT users_deleted_account_terminal
  CHECK (username <> '[deleted_' || id::text || ']'
         OR (password_hash = '' AND NOT is_admin));

-- migrate:down

-- The repaired rows stay repaired; only the constraint is removed.
ALTER TABLE users DROP CONSTRAINT users_deleted_account_terminal;
