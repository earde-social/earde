# Account privacy and fail-closed authentication boundaries

This document is the contract for how login, signup and password recovery
avoid revealing which email addresses have accounts, how authentication mail
is sent, and what the shared rate limiter does when it cannot decide.

## What is public and what is not

- **Public:** whether a username already belongs to a real account. Signup
  says "That username is already taken." for such a handle. The answer
  depends on the username alone (exact match, the users table's own
  uniqueness rule) and is the same whatever email is submitted. Usernames
  are public handles; this is not a promise that they are secret.
- **Private:** whether an email address is registered, whether a username
  is held by someone's unconfirmed signup, and whose signup that is. No
  response status, body, redirect, header, cookie or session change depends
  on them. Neither does the expensive work (one Argon2 verification per
  login, one Argon2 hash per eligible signup), nor the shared mail
  capacity: whether a later request is admitted, and when a place frees
  up, is the same whichever private outcome an earlier request had (see
  "Auth mail scheduling"). This is not a claim of constant-time execution.
  SQL, operating-system scheduling and HTTP handling still vary with load
  and data.

## Login

A missing account and a wrong password get the same "Invalid username or
password." response after exactly one Argon2id verification each. A missing
account is verified against a fixed dummy hash
(`Login_verification.dummy_hash`) produced with the production parameters
(m=65536, t=2, p=1). Its password is public on purpose: the result of a
dummy verification is discarded, so it can never log anyone in. A ban is
disclosed only after valid credentials. Verification runs after the lookup's
database connection is returned to the pool, on the request's own Lwt thread
as before, so no new cryptographic concurrency is introduced.

## Signup

After the closed-signup, honeypot, Turnstile and syntax gates (all cheap and
unchanged), and the public username check, every eligible submission:

1. is admitted to the auth mail dispatcher (below) — a full dispatcher
   answers a generic 503 before anything is hashed or written;
2. hashes the password with Argon2id;
3. runs one transaction (`Signup_submission_store.submit`) that decides the
   private outcome;
4. settles into one fixed-length service slot, with a confirmation email
   or without one.

Every private outcome gets the same neutral page ("If everything checks out,
we'll email you a confirmation link…"): a new email, a registered email, a
pending email, the submitter's own reservation, someone else's live
reservation, a lost uniqueness race, or a storage failure. Only a freshly
committed pending row leads to a confirmation email, sent to the address that
row names. Resubmitting with the same email replaces that email's earlier
unconfirmed signup (the old link stops working); another email's live
reservation is left untouched; an expired reservation is replaced. A signup
submission never changes an existing account. Confirmation still requires
the emailed token, still creates the account atomically, and still does not
log the user in.

## Password reset requests

Admission first (a full dispatcher answers the same generic 503 for every
address), then the existing single-statement token write, then the same
neutral page for every address. A reset email is queued only when a token
row for a real account was written. Unknown addresses get no email, but
they occupy the same fixed service slot. Each request issues a new token.
Requesting a new link does not cancel earlier unexpired ones, but using
any link, or changing the password while logged in, deletes every
outstanding link for the account in the same transaction as the password
write.

## Auth mail scheduling

`Auth_mail_dispatcher` is a small in-process scheduler shared by signup
confirmation and password-reset mail. The request handler never awaits the
email provider.

| Bound | Value | Meaning |
|---|---|---|
| capacity | 64 | admitted requests outstanding: open reservations + queued entries + entries in service |
| slots | 2 | service slots in use at once, whatever an entry holds |
| slot length | 15 s | fixed length of every service slot on a monotonic clock, and so the longest one provider attempt may run |

**Why fixed slots.** The capacity is shared by every client. If a request
that sends no mail gave its place back sooner than one that does, another
client's next request would reveal the difference: 503 or 200 at the
capacity edge, or a place freeing up at a different time. So:

- Every admitted request settles exactly once into one entry in a single
  FIFO queue (in the order the private work finishes). The entry is either
  a real message or a no-send entry. A no-send entry covers an unknown
  address, a registered or reserved email, a lost race, a storage error,
  and work that raised or was cancelled. It holds no recipient, token or
  password and never reaches the transport or the resolver.
- Entries are serviced in queue order, at most two at a time. A service
  slot lasts exactly 15 s for both kinds. Only the end of the slot releases
  the slot and the admission place. The provider's outcome (success,
  refusal, error, exception, a hang) does not shorten or lengthen it, and
  neither do DNS availability or transport failures.
- A real message is attempted when its slot begins, after its token row or
  pending signup has committed. The attempt may finish early. Its
  connection is closed and the message is dropped at once, and only the
  account-independent slot remains until the deadline. An attempt still
  running at the deadline is cancelled.
- HTTP responses never wait for a slot. The mail attempt is not awaited,
  but a slot that is free when the request settles starts the attempt right
  away, so it can begin before the response has been written.

Regression tests replay the independent review's reproduction as paired
sequences: two requests with different private outcomes, from the same
near-full state, followed by probes through the real routed handlers,
CSRF and the production limiter. The probes' answers and the exact release
schedule match under a stalled, slow, fast-succeeding and fast-failing
provider.

**Cost, accepted deliberately.** At saturation two 15 s slots serve at most
8 entries per minute, no-send entries included. 64 ready entries take about
8 minutes to drain, not counting database time before an entry is queued or
runtime scheduling delays. Requests for unknown addresses use capacity like
real ones. The first request on an idle scheduler still starts its mail at
once. When the scheduler is full, both routes answer 503 "Temporarily
unavailable" rather than queue without bound. Delivery is not guaranteed.

**Provider deadline versus name resolution.** The 15 s deadline ends the
provider attempt: it cancels a pending connect or TLS handshake, closes an
established connection, or drops the wait on a pending name lookup. The
lookup itself (the system resolver, run in Lwt's thread pool) cannot be
interrupted, and the operating system may keep running it after the
deadline. A separate bound covers that remainder:
`Auth_mail_resolver` allows at most 2 outstanding authentication-mail
lookups per process. That count includes lookups whose attempts were
abandoned. A lookup's permit is returned only when the lookup itself
finishes. There is no waiting queue. While both permits are held, a new
real attempt fails at once (`resolver_busy`) without starting another
lookup, and its slot still runs its full length. An abandoned lookup keeps
no message state (payload, recipient, token or key) and never goes on to
connect. The endpoint, TLS certificate and hostname validation, SNI and
request are unchanged from cohttp's own client path.

A resolver outage can therefore drop best-effort mail after the token or
pending signup has been committed. The remedy is the same as for any lost
mail: the user asks again. There is no automatic retry queue.

**Other invariants.**

- No database connection is held while the provider is contacted.
- Raw passwords never enter an entry. Raw tokens exist only in the
  in-memory message, and the database keeps only their hashes.
- Delivery-failure diagnostics name only the message kind and a failure
  class (`auth mail <kind>: delivery failed (<class>)`,
  `auth mail <kind>: delivery timed out`). Other log lines are not all
  recipient-free. Without `BREVO_API_KEY` (development) the mail path logs
  the recipient, and with the explicit `EARDE_LOG_TOKENS=1` opt-in it also
  logs the link. A failed reset-token write logs the database error text.

**Volatility (deliberate).** Queued entries live only in the running
process. A restart or crash drops them. Delivery is best effort: neither
durable nor exactly-once. The pending signup or reset token remains in
PostgreSQL (as a hash, with its normal expiry). The user recovers by
signing up again, which replaces that pending signup's token, or by
requesting another reset link.

## Shared rate limiter

`Rate_limit_middleware.middleware` fronts every sensitive POST in
`lib/app_routes.ml` (login, signup, reset request, and the project/community
management mutations). It now fails closed: only a positive Allowed decision
reaches the wrapped handler. A lookup error, a rejected promise or a pool
that cannot provide a connection returns a generic 503 and runs nothing
behind it. Budgets, the 60 s window and the trusted client-address rule are
unchanged. Each route names its operation, and the bucket is that operation
rather than the request path, so percent-encoded, repeated-slash and
parameterized spellings of one route share a bucket; the opportunistic expiry cleanup remains
best effort and cannot change a decision. An exception raised by an allowed
handler is that handler's own and is not relabelled as a limiter outage.

The separate image-upload budget (`Rate_limit_store.check_upload`) is not this
middleware and deliberately stays fail-open on storage errors.

## Operations

- No new configuration, service or migration. `mtime` (already linked
  through Caqti) is now a direct dependency, for the monotonic slot timer.
- During a database outage, rate-limited POSTs now answer 503 instead of
  running without a limit.
- If mail seems not to arrive, look for `auth mail <kind>: delivery failed`
  (the class `resolver_busy` means both lookup permits were held by hung
  name resolutions) or `delivery timed out` in the logs. The user's remedy
  is to request the email again.
- Under sustained load the auth mail routes serve at most 8 admitted
  requests per minute per process, and answer 503 beyond the 64 outstanding.
