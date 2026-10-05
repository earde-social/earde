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

Fixes land on `main` and in the next release. Only the latest release and the
current `main` branch are supported; there are no release branches.

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
  links. Using any link, changing the password, or deleting the account
  invalidates all of them.
- **Rate limits** count attempts per client address and per operation (five a
  minute for login, signup, reset requests and the other limited forms). There
  is no per-account limit, and the authenticated password change and the reset
  form itself are not limited. The client address is only as trustworthy as the
  proxy in front of Earde; see "Client addresses" in
  [docs/deployment.md](docs/deployment.md).
- **Upload limiter.** The image-upload limiter counts per client address and
  allows the upload if its own storage check fails. It only meters image
  processing for authenticated members.
- **Public listing cost.** Community homes and sections, `/feed` and `/search`
  page with LIMIT/OFFSET and compute each candidate post's score and comment
  count before sorting. Indexed lookups keep this cheap per post, and pages
  above 1,000 are refused before any database work, but the cost of a page still
  grows with the number of posts in the community (or, for `/feed` and
  `/search`, in the whole public archive). On a test community of 20,000 posts
  and 60,000 comments every permitted page took under about a second; we have
  not measured larger archives. These pages are public and not rate-limited.
  Profile pages (`/u/:username`) list all of one author's posts and comments
  without paging; their cost is bounded by that author's own content.
- **Search** is substring matching over titles, post bodies and comments, with
  no full-text index; each request runs one tab's query. Deployments with large
  archives should rate-limit anonymous `GET` requests to `/search`, `/feed` and
  `/c/` at the proxy.
- **Moderation semantics.**
  - A community ban does not remove membership, so a banned member of a private
    community can still read it.
  - The moderation log is public.
  - Deleting a chat message hides it without erasing the stored text.
- **Expired rows are not purged.** Password-reset tokens and page-view records stay
  in the database after they expire. They are inert, but operators may want to
  purge them periodically.
