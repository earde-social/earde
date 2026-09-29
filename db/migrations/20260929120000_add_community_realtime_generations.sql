-- migrate:up

-- Realtime access generations.
--
-- A live chat socket is authorized once, when Dream signs its token, and the
-- gateway only checks the token's signature, topic and expiry. Without this,
-- a socket opened while its user could read a private community kept
-- receiving that community's new messages until the token expired (up to an
-- hour) after the user lost access.
--
-- Dream now names every chat topic after the community's current generation
-- (chan:<channel_id>:<generation>) when it signs a token and when it
-- publishes a message. Any change that can take read access away from
-- someone bumps the generation in the same transaction as the change, so
-- every message committed afterwards goes to a topic that no socket
-- authorized before the change has joined. Users who still have access move
-- to the new topic by refreshing their token over HTTP, where Dream checks
-- access again. The gateway keeps no state for this.
--
-- The triggers are deliberately broad: a bump only costs connected clients
-- a token refresh and an HTTP catch-up, while a missed bump would leak.
CREATE TABLE community_realtime_generations (
    community_id integer PRIMARY KEY REFERENCES communities(id) ON DELETE CASCADE,
    generation bigint NOT NULL DEFAULT 0 CHECK (generation >= 0),
    updated_at timestamptz NOT NULL DEFAULT NOW()
);

-- INSERT ... SELECT from communities: when a community is being deleted,
-- the cascaded membership deletes still fire their triggers, and there is
-- no row left to bump (or to reference).
CREATE FUNCTION bump_community_realtime_generation(target integer) RETURNS void
LANGUAGE sql AS $$
    INSERT INTO community_realtime_generations (community_id, generation, updated_at)
    SELECT id, 1, NOW() FROM communities WHERE id = target
    ON CONFLICT (community_id) DO UPDATE
       SET generation = community_realtime_generations.generation + 1,
           updated_at = NOW();
$$;

-- Membership and moderator rows grant read access to private communities.
CREATE FUNCTION community_realtime_access_row_removed() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    PERFORM bump_community_realtime_generation(OLD.community_id);
    RETURN NULL;
END;
$$;

CREATE TRIGGER community_members_realtime_access
    AFTER DELETE ON community_members
    FOR EACH ROW EXECUTE FUNCTION community_realtime_access_row_removed();

CREATE TRIGGER community_moderators_realtime_access
    AFTER DELETE ON community_moderators
    FOR EACH ROW EXECUTE FUNCTION community_realtime_access_row_removed();

-- A public community turning private takes access from every non-member.
CREATE FUNCTION community_realtime_visibility_narrowed() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    PERFORM bump_community_realtime_generation(NEW.id);
    RETURN NULL;
END;
$$;

CREATE TRIGGER communities_realtime_access
    AFTER UPDATE OF visibility ON communities
    FOR EACH ROW
    WHEN (OLD.visibility IS DISTINCT FROM NEW.visibility AND NEW.visibility = 'private')
    EXECUTE FUNCTION community_realtime_visibility_narrowed();

-- Account-level losses:
--   * a global ban (the account's sessions are revoked with it);
--   * losing durable admin authority, which reads every private community;
--   * account deletion, which anonymizes the row to the '[deleted_<id>]'
--     tombstone (Db.User.anonymize_user) and keeps its membership rows.
CREATE FUNCTION user_realtime_access_narrowed() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    IF OLD.is_admin AND NOT NEW.is_admin THEN
        PERFORM bump_community_realtime_generation(c.id)
           FROM communities c WHERE c.visibility = 'private';
    END IF;
    IF (NEW.is_banned AND NOT OLD.is_banned)
       OR (NEW.username LIKE '[deleted\_%' AND OLD.username NOT LIKE '[deleted\_%') THEN
        PERFORM bump_community_realtime_generation(x.community_id)
           FROM (SELECT community_id FROM community_members WHERE user_id = NEW.id
                 UNION
                 SELECT community_id FROM community_moderators WHERE user_id = NEW.id) x;
    END IF;
    RETURN NULL;
END;
$$;

CREATE TRIGGER users_realtime_access
    AFTER UPDATE OF is_admin, is_banned, username ON users
    FOR EACH ROW EXECUTE FUNCTION user_realtime_access_narrowed();

-- migrate:down

DROP TRIGGER IF EXISTS users_realtime_access ON users;
DROP FUNCTION IF EXISTS user_realtime_access_narrowed();
DROP TRIGGER IF EXISTS communities_realtime_access ON communities;
DROP FUNCTION IF EXISTS community_realtime_visibility_narrowed();
DROP TRIGGER IF EXISTS community_moderators_realtime_access ON community_moderators;
DROP TRIGGER IF EXISTS community_members_realtime_access ON community_members;
DROP FUNCTION IF EXISTS community_realtime_access_row_removed();
DROP FUNCTION IF EXISTS bump_community_realtime_generation(integer);
DROP TABLE IF EXISTS community_realtime_generations;
