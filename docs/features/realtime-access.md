# Realtime access after a permission change

Live chat runs over the realtime gateway, which only fans messages out.
Dream decides who may read a channel. It does so when it signs a gateway token
(on the channel page and on `GET /c/:slug/ch/:channel_slug/realtime-token`).
The gateway checks only the token's signature, its topic and its expiry.

## The problem this solves

A socket opened while its user could read a private community stayed
subscribed after that user lost access, until the token expired (tokens last
an hour). Every message posted in the meantime reached that socket.

## How it works

- Each community has an access **generation** (`community_realtime_generations`,
  0 until first bumped). Chat topics are `chan:<channel_id>:<generation>`.
- Database triggers bump the generation in the same transaction as any change
  that can take read access away from someone:
  - a membership or moderator row is deleted (leaving, removal);
  - a public community becomes private;
  - an account is globally banned, loses durable admin authority (every
    private community), or is deleted (anonymized).
  Changes that only widen access (joining, promotion, private to public,
  unban) do not bump.
- Dream reads the generation **after** a message commits and publishes it to
  that generation's topic. A message created after a revocation therefore
  never goes to a topic that a socket authorized before the revocation joined.
- When signing a token, Dream reads the generation **before** it checks
  access. A revocation that commits in between either fails the check or
  leaves the token on the old topic.
- The gateway binds each connection to its token's topic, so an old token
  cannot join the new topic.
- Clients refresh their token at least every 3 minutes. When the refreshed
  token names a new topic, the page reconnects with it, joins, and fetches
  what it missed over HTTP. A refresh answered with 401, 403 or 404 stops live
  updates on that page.

The gateway keeps no revocation state. Postgres stays the source of truth, and
HTTP catch-up is unchanged.

## Limits

- Messages committed **before** a revocation may still reach the old socket.
  The user could read them at that time.
- After a bump, users who still have access get no live messages until their
  next refresh, at most about 3 minutes. The HTTP catch-up then fills the gap.
- Until connected clients move to the new topic, the old topic still carries
  presence, typing and cursor events. A revoked socket can see those
  (usernames, not message content) for the same short window.
- A community ban does not remove membership, so it does not change read
  access and does not bump.
