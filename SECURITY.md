# Security policy

## Reporting a vulnerability

Please report vulnerabilities privately through GitHub:
**<https://github.com/earde-social/earde/security/advisories/new>**
(Security tab → "Report a vulnerability"). Only the maintainers can see the
report, and the discussion and any fix are coordinated in that advisory.

Please do not open a public issue or pull request for a vulnerability, and do not
test against other people's accounts or data on a live deployment.

A useful report includes:
- the affected route, module or commit;
- steps to reproduce;
- what an attacker gains.

A proof of concept against a local instance (see [docs/setup.md](docs/setup.md))
is ideal.

## Supported versions

Only the current `main` branch is supported. There are no release branches.

## Scope

In scope:
- this repository's application code, including the realtime gateway;
- its default configuration.

Out of scope:
- vulnerabilities in dependencies, unless Earde uses them unsafely (report those
  upstream);
- findings that need a compromised server or database;
- missing hardening that is not a vulnerability on its own.

## Known limitations

These are deliberate, documented behaviours rather than open vulnerabilities.
Reports that show they are more severe than described are welcome.

- **Realtime access loss is not instantaneous** for already-connected sockets.
  - After access to a private community is revoked, the socket stops receiving new
    messages.
  - Presence, typing and cursor metadata on the old topic can continue for up to
    about three minutes.
  - Messages sent before the revocation remain visible to the client that
    received them.

  See [docs/features/realtime-access.md](docs/features/realtime-access.md).
- **Password-reset links.** Issuing a new link does not cancel earlier unexpired
  links. Using any link, or changing the password, invalidates all of them.
- **Upload limiter.** The per-user image-upload limiter allows the upload if its
  own storage check fails. It only meters image processing for authenticated
  members.
- **Search** uses unindexed substring matching and has no rate limit.
  Deployments with large archives should put request limits in front of
  `/search`.
- **Moderation semantics.**
  - A community ban does not remove membership, so a banned member of a private
    community can still read it.
  - The moderation log is public.
  - Deleting a chat message hides it without erasing the stored text.
- **Expired rows are not purged.** Password-reset tokens and page-view records stay
  in the database after they expire. They are inert, but operators may want to
  purge them periodically.
