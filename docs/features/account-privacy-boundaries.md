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
  response status, body, redirect, header, cookie, or expensive-work path
  depends on them.

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
   private outcome.

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
row for a real account was written; unknown addresses get no email.

## Auth mail dispatcher

`Auth_mail_dispatcher` is a small in-process queue shared by signup
confirmation and password-reset mail. The request handler never waits for
the email provider.

| Bound | Value | Meaning |
|---|---|---|
| capacity | 64 | admitted outstanding requests: open reservations + queued jobs + running deliveries |
| concurrency | 2 | simultaneous provider deliveries |
| timeout | 15 s | one whole provider operation: resolve, connect, request, response body |

- A job is queued only after the state it refers to is committed, and no
  database connection is held while the provider is contacted.
- On timeout the delivery is cancelled and its HTTP connection is closed
  (`Email.deliver` owns its connection rather than using cohttp's shared
  client, which would keep an unanswered socket open). A failed or timed-out
  delivery frees its slot and never affects later jobs.
- Diagnostics name only the message kind and a failure class — never the
  recipient, the token or the provider response.
- Raw passwords never enter a job; raw tokens exist only in the in-memory
  message, and the database keeps only their hashes, exactly as before.

**Volatility (deliberate).** Queued mail lives only in the running process.
A restart or crash can drop it. Delivery is best effort: neither durable nor
exactly-once. The pending signup or reset token remains in PostgreSQL (as a
hash, with its normal expiry), and the user recovers by signing up again or
requesting a new reset link, which issues a fresh token. Under sustained
overload the routes answer 503 "Temporarily unavailable" rather than queue
without bound.

## Shared rate limiter

`Handlers.Rate_limit.middleware` fronts every sensitive POST in
`bin/main.ml` (login, signup, reset request, and the project/community
management mutations). It now fails closed: only a positive Allowed decision
reaches the wrapped handler. A lookup error, a rejected promise or a pool
that cannot provide a connection returns a generic 503 and runs nothing
behind it. Budgets, the 60 s window, path-only bucket keys and the trusted
client-address rule are unchanged; the opportunistic expiry cleanup remains
best effort and cannot change a decision. An exception raised by an allowed
handler is that handler's own and is not relabelled as a limiter outage.

The separate image-upload budget (`Db.Rate_limit.check_upload`) is not this
middleware and deliberately stays fail-open on storage errors.

## Operations

- No new configuration, service, dependency or migration.
- During a database outage, rate-limited POSTs now answer 503 instead of
  running without a limit.
- If mail seems not to arrive, look for `auth mail <kind>: delivery failed`
  or `delivery timed out` in the logs; the user's remedy is to request the
  email again.
