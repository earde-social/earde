# GitHub community-home — production launch smoke test

Manual, human-executable launch verification for the two GitHub community-home
journeys. It requires no source reading: every route, form field, query
parameter and event name below was taken from the shipped router
(`bin/main.ml`), the shipped page modules, and `Analytics.event_name`.

**Scope.** Two MVP journeys:

```text
Existing-community path
GitHub project → request community home → moderator accept/reject
→ notification → connected-project display → removal

Dedicated-community path
GitHub project → provision private community draft → complete setup
→ publish Public or Unlisted → connected-project display → removal
```

**How to use this document.** Work top to bottom. Every `[ ]` is a check a
human performs against the deployed production system. Record evidence in the
gate table at the end. Automated test results never make a manual gate PASS.

> **Never paste a secret into this file, a ticket, a chat message, or a
> screenshot.** Where a value must be confirmed, confirm only that it is *set*
> and that its *shape* is right — never its content.

---

## 0. Known operational constraints (read before Phase 1)

These are current, intended behaviours that will otherwise look like bugs
mid-test.

1. **Rollout mode gates the moderator too.** `EARDE_GITHUB_ONBOARDING_ENABLED`
   is read per request and gates *every* community-home route — including
   `GET /c/:slug/project-home-requests` and the accept/reject POSTs. With
   `admins`, a top moderator who is **not** a global admin is redirected to
   `/bring` and cannot review a request, so the existing-community journey
   cannot complete. **For this test the mode must be `public`, or User B must
   be a global admin.** With `off`, every route in this document redirects to
   `/bring`.
2. **There is no public project page.** A project is visible publicly only
   through the *Connected projects* section of a community page. The two
   project routes (`/projects/:slug/setup`, `/projects/:slug/request-home`)
   are steward-only and return a generic 404 to everyone else.
3. **All "unavailable" states collapse to one generic 404** by design
   (nonexistent slug, foreign project, lost stewardship, revoked project).
   A 404 during the test is not automatically a bug — re-check the actor.
4. **Removal notifications go to both sides.** A removal notifies every
   project steward *and* every community top moderator, minus the actor. Its
   destination is the community page `/c/:slug` — the one surface both roles
   can open.
5. **`DREAM_SECRET` encrypts the onboarding cookie.** Restarting with a
   changed `DREAM_SECRET` invalidates every in-flight GitHub onboarding flow
   (and every session). Do not rotate it during this test.

---

## Phase 1 — Environment and consent

### 1.1 Deployed revision

- [ ] Record the exact commit SHA running in production: `________________`
- [ ] Confirm it contains the nine branch migrations listed in §1.4.

### 1.2 Rollout mode

- [ ] `EARDE_GITHUB_ONBOARDING_ENABLED` is exactly `public` (lowercase).
      Any unknown, blank or differently-cased value silently means `off`.
- [ ] UI check: as User A, open `/bring`. The page must offer the GitHub
      installation action, not an "onboarding is unavailable" state.
- [ ] If the mode must stay `admins` for launch, record here that User B is a
      global admin: `________________` (otherwise Phase 5 cannot pass).

### 1.3 GitHub App configuration

All five must be set; the app refuses to start an installation otherwise.
Confirm presence and shape only.

- [ ] `EARDE_PUBLIC_ORIGIN` — exactly `https://earde.com` (lowercase scheme
      and host, **no** trailing slash, no port).
- [ ] `GITHUB_APP_SLUG` — the app's slug as it appears in its GitHub URL.
- [ ] `GITHUB_APP_CLIENT_ID` — set.
- [ ] `GITHUB_APP_CLIENT_SECRET` — set (server-only; never rendered).
- [ ] `GITHUB_APP_SETUP_URL` — exactly
      `https://earde.com/integrations/github/install/return`
- [ ] `GITHUB_APP_CALLBACK_URL` — exactly
      `https://earde.com/integrations/github/authorize/callback`

Both URLs are validated at request time: they must sit on
`EARDE_PUBLIC_ORIGIN`, carry exactly those paths, and carry no userinfo,
query or fragment. A mismatch makes `POST /integrations/github/install/start`
fail before any GitHub redirect.

On **github.com → your App → General**, confirm the registered values match
byte-for-byte:

- [ ] *Setup URL* = the `GITHUB_APP_SETUP_URL` above, **"Redirect on update"
      enabled**.
- [ ] *Callback URL* = the `GITHUB_APP_CALLBACK_URL` above.
- [ ] *Request user authorization (OAuth) during installation* — enabled.
- [ ] Repository permissions include **Metadata: Read-only** (required to list
      repositories).

### 1.4 Database migrations

The branch adds nine forward-only migrations. Confirm all are applied:

- [ ] `20260723120000_add_network_community_lifecycle.sql`
- [ ] `20260723150000_add_github_installations_and_onboarding_states.sql`
- [ ] `20260724120000_add_project_onboarding_drafts.sql`
- [ ] `20260724130000_add_open_source_projects.sql`
- [ ] `20260724140000_add_community_projects.sql`
- [ ] `20260726120000_add_network_community_identity_constraints.sql`
- [ ] `20260726130000_add_network_community_lifecycle_constraint.sql`
- [ ] `20260727120000_add_project_home_audit_events.sql`
- [ ] `20260727130000_add_project_home_notifications.sql`

Read-only confirmation (safe, no writes):

```sql
SELECT to_regclass('open_source_projects'),
       to_regclass('project_stewards'),
       to_regclass('project_repositories'),
       to_regclass('community_projects'),
       to_regclass('project_home_audit_events'),
       to_regclass('github_installations'),
       to_regclass('project_onboarding_drafts');
```

- [ ] All seven return a non-NULL name.

### 1.5 Session and cookie behaviour

- [ ] `DREAM_SECRET` is set and stable (sessions survive restarts).
- [ ] Log in as User A, restart nothing, reload `/feed` — still logged in.
- [ ] In devtools, the onboarding cookie created in Phase 2 must be
      `__Secure-` prefixed, `HttpOnly`, `SameSite=Lax`, `Secure`, and scoped
      to path `/integrations/github`. (Check during Phase 2.2.)

### 1.6 PostHog and deployment environment

- [ ] `POSTHOG_ENABLED` is exactly `true`.
- [ ] `EARDE_DEPLOYMENT_ENVIRONMENT` is exactly `production`.
- [ ] `POSTHOG_PROJECT_TOKEN`, `POSTHOG_API_HOST`, `POSTHOG_UI_HOST`,
      `POSTHOG_PROJECT_ID`, `POSTHOG_PERSONAL_API_KEY` — set.
- [ ] `POSTHOG_ALLOW_DEVELOPMENT` is **not** set (or not `true`).
- [ ] Run the repository's own preflight, which verifies the token/project
      binding without ingesting anything and prints no secret:

      dune exec bin/check_posthog_config.exe

      Exit code 0 and `PostHog configuration preflight: VERIFIED`.
- [ ] Recorded environment line from that output: `production`.

Activation fails closed: with `production`, analytics stays off unless
`EARDE_PUBLIC_ORIGIN` is exactly `https://earde.com` and the PostHog
configuration is complete.

### 1.7 Analytics consent — grant it before anything else

Every event in this document is consent-gated server-side. **No consent, no
events, and Phases 2–10 will produce nothing in PostHog.**

For each browser profile used in the test:

- [ ] Accept the analytics banner, **or** open `/settings` → *Analytics* →
      **Enable analytics**.
- [ ] Devtools → Application → Cookies: `earde_analytics_consent` = exactly
      `granted`, `Path=/`, `SameSite=Lax`, `Secure`, not `HttpOnly`.
- [ ] Re-grant after any profile reset — consent is per browser profile.

### 1.8 Test isolation

- [ ] Two separate browser profiles (or one normal + one private window) so
      User A and User B never share a session.
- [ ] A third profile for User C (unauthenticated-adjacent privacy checks).
- [ ] No production credentials written into this document or its evidence.

### 1.9 Test fixtures

| Fixture | Value used |
| --- | --- |
| User A (project steward) | `________________` |
| User B (top mod of Community Existing) | `________________` |
| User C (unrelated authenticated user) | `________________` |
| Personal GitHub account | `________________` |
| GitHub organization | `________________` |
| Personal repository | `________________` |
| Organization repository | `________________` |
| Community Existing (already published, User B is `top_mod`) | `/c/________` |
| Community Dedicated (created during Phase 7) | `/c/________` |

- [ ] Community Existing is **published, network, public, indexable and
      discoverable** — only such communities appear as eligible targets.
- [ ] Prefix every test project and community name with `LAUNCHTEST` so
      Phase 12 cleanup can find them.

---

## Phase 2 — Personal GitHub installation (User A)

### 2.1 Start

- [ ] Open `/bring` while logged in as User A.
- [ ] Submit the installation form (`POST /integrations/github/install/start`).
- [ ] The response is a redirect to `github.com` — not an HTML error page.

Expected event: **`github_app_install_started`**.

### 2.2 Complete the callback

- [ ] On GitHub, install the App on your **personal account**, selecting at
      least one repository.
- [ ] GitHub returns to `/integrations/github/install/return`, which redirects
      onward to the OAuth authorization step.
- [ ] Authorize; GitHub returns to
      `/integrations/github/authorize/callback`.
- [ ] The browser lands on `/bring?github=connected` with a success banner.
      A failure lands on `/bring?github=failed`.
- [ ] Check the onboarding cookie attributes listed in §1.5 now (it is
      dropped after the flow completes).
- [ ] Address bar and browser history contain no `code=` or `state=` value
      after the final redirect.

Expected event: **`github_app_installed`**.

### 2.3 Repositories appear

- [ ] Open `/projects/new`.
- [ ] A draft appears for the personal account, labelled **Personal**, with
      its repository count.
- [ ] The repositories you granted are listed.

### 2.4 Select repositories

- [ ] Tick at least one repository (`repository` checkboxes) and choose a
      **primary** (`primary_snapshot_id`), then submit
      (`POST /projects/new/repositories`).
- [ ] You are redirected back to `/projects/new?draft=<id>&step=details` on
      success, or `...&selection=required` when no repository was selected.

Expected event: **`github_repositories_selected`**
(`repository_count` = number selected; `0` is a real, valid event).

### 2.5 Create the permanent project

On the details step, fill in:

| Field | Note |
| --- | --- |
| `name` | Display name — prefix `LAUNCHTEST` |
| `slug` | Canonical `^[a-z0-9]+(-[a-z0-9]+)*$`, 1–80 chars. `new` is reserved |
| `kind` | One of `project`, `organization`, `ecosystem`, `foundation`, `working_group`, `other` |
| `description` | Optional |
| `website_url` | Optional |
| `draft_id` | Hidden, pre-filled |

- [ ] Submit (`POST /projects`).
- [ ] You are redirected to `/projects/<slug>/setup`.

Expected event: **`github_project_created`**
(`project_id` positive, `project_kind` = the chosen kind, `repository_count`).

### 2.6 Verify project identity and setup page

- [ ] `/projects/<slug>/setup` shows the project name, namespace login and
      verification state.
- [ ] It offers both paths:
      *Connect to an existing community* → `/projects/<slug>/request-home`
      *Create a community home* → `/projects/<slug>/community-home/new`
- [ ] As User C, open `/projects/<slug>/setup` → generic **404**.

### 2.7 Expected closed properties (no sensitive identifiers)

| Event | Allowed properties besides `deployment_environment` |
| --- | --- |
| `github_app_install_started` | `user_id` |
| `github_app_installed` | `user_id` |
| `github_repositories_selected` | `user_id`, `repository_count` |
| `github_project_created` | `user_id`, `project_id`, `project_kind`, `repository_count` |

- [ ] None of them carries a GitHub login, installation id, repository name or
      URL, project slug or name, OAuth state, or PKCE material.

---

## Phase 3 — GitHub organization repository (User A)

Installing on an organization is a **separate installation** of the same App.

### 3.1 Organization access

- [ ] If User A does not own the organization, a GitHub **owner must approve**
      the installation request. Record who approved: `____________`
- [ ] Repeat Phase 2.1–2.2 and choose the **organization** as the install
      target, granting at least one org-owned repository.
- [ ] Return lands on `/bring?github=connected`.

### 3.2 Select an organization repository

- [ ] `/projects/new` now lists a second draft, labelled **Organization**,
      with the org login.
- [ ] Select at least one org repository and a primary; submit.

### 3.3 Create the organization project

- [ ] Create a second permanent project (`LAUNCHTEST` prefix, a distinct
      slug) with `kind = organization`.
- [ ] Redirect to `/projects/<org-slug>/setup`.

### 3.4 Namespace and kind

- [ ] The setup page shows the **organization login** as the namespace, not
      the personal account.
- [ ] The kind renders as *Organization*.

### 3.5 Nothing private renders

- [ ] View source on `/projects/<org-slug>/setup`: no installation id, no
      GitHub account id, no repository id, no token, no client secret.
- [ ] Repository links point only at public `https://github.com/...` URLs.
- [ ] If you granted a **private** repository, confirm nothing about it
      appears on any *public* surface later (Phase 6/8).

---

## Phase 4 — Existing-community request (User A)

Use the **personal** project from Phase 2.

### 4.1 Choose the path

- [ ] `/projects/<slug>/setup` → *Connect to an existing community*
      → `/projects/<slug>/request-home`.
- [ ] The page lists eligible target communities. **Community Existing must
      appear.** If it does not, it is not published/network/public — fix the
      fixture, not the code.

### 4.2 Submit the request

| Field | Note |
| --- | --- |
| `target_community_id` | The chosen community |
| `request_note` | Optional, private — must never surface publicly |

- [ ] Submit (`POST /projects/<slug>/request-home`).
- [ ] You are redirected back to `/projects/<slug>/request-home`.

Expected event: **`project_home_request_submitted`** (`user_id` only).

### 4.3 Pending state

- [ ] The page now shows the active relation with status **Pending** and the
      target community.
- [ ] The eligible-community list is **empty** — a second request cannot be
      solicited.
- [ ] No removal control is offered for a pending relation.

### 4.4 Replay cannot create a second pending request

- [ ] Press the browser Back button and re-submit the same form.
- [ ] The page re-renders with a conflict message; it does **not** create a
      second request.
- [ ] Read-only confirmation:

      SELECT status, count(*) FROM community_projects
      WHERE project_id = (SELECT id FROM open_source_projects WHERE slug = '<slug>')
      GROUP BY status;

      exactly one row, `pending`, count 1.

### 4.5 Exactly one notification, to the right people

- [ ] As User B: the bell badge (`/api/unread-notifs`) increments by 1.
- [ ] `/notifications` shows exactly one 🏠 line:
      *"<Project> requested <Community> as its community home"*.
- [ ] Its link is `/c/<community-slug>/project-home-requests`.
- [ ] As User C (an ordinary member of Community Existing): **no** such
      notification.
- [ ] As an ordinary (non-top) moderator of Community Existing, if one
      exists: **no** such notification. Only `top_mod` users are notified.
- [ ] As User A (the actor): **no** self-notification.

---

## Phase 5 — Moderator review (User B)

### 5.1 Open the notification

- [ ] Click the notification. It opens
      `/c/<community-slug>/project-home-requests` and returns **200**.
- [ ] The URL is the canonical route (no query parameters, no redirect chain).

### 5.2 The destination is authorized, not merely reachable

- [ ] As User A (steward, not a moderator), open the same URL → generic
      **404**.
- [ ] As User C, open the same URL → generic **404**.

### 5.3 Inspect the queue

- [ ] The pending request shows project name, kind, namespace login,
      verification state and its repositories.
- [ ] The **request note is not shown** anywhere on this page.
- [ ] *Accept* is offered only when the project is Verified, the community
      Eligible, and the project has at least one repository. *Reject* is
      always available.

### 5.4 Accept

- [ ] Press **Accept** (`POST /c/<slug>/projects/<project-slug>/accept`).
- [ ] Redirect back to the queue; the request is gone from Pending.

Expected event: **`project_home_request_reviewed`**, `decision = accepted`.

### 5.5 Reject (a separate project/relation)

- [ ] Using the **organization** project from Phase 3, repeat Phase 4 as
      User A against Community Existing.
- [ ] As User B, press **Reject**
      (`POST /c/<slug>/projects/<org-project-slug>/reject`).

Expected event: **`project_home_request_reviewed`**, `decision = rejected`.

> Do not try to review the same relation twice. A replay re-renders the queue
> with **409** and emits nothing.

### 5.6 Exactly one decision notification to User A

- [ ] User A's inbox gains exactly one line per decision:
      *"<Community> accepted the community-home request for <Project>"*
      *"<Community> rejected the community-home request for <Project>"*
- [ ] Each links to `/projects/<project-slug>/request-home` and returns 200
      for User A.
- [ ] User B (the actor) receives **no** self-notification.
- [ ] Neither notification contains the request note, the reviewer's name, or
      any moderation provenance.

### 5.7 Notes stay private

- [ ] `Ctrl-F` the note text on `/notifications`, on the queue page, and on
      the public community page → **zero** matches.

---

## Phase 6 — Accepted existing-community state

### 6.1 Project side

- [ ] As User A, `/projects/<slug>/request-home` shows the active relation as
      **Accepted** with Community Existing.
- [ ] A **Remove home** form is offered (the community is published).

### 6.2 Community public page

- [ ] Open `/c/<community-slug>` as User A → the **Connected projects**
      section lists the project.
- [ ] Open it **logged out** → the same section is present.

### 6.3 Crawlable public HTML

- [ ] `curl -s https://earde.com/c/<community-slug> | grep -i "<Project name>"`
      returns the project name from the server-rendered HTML (no JavaScript
      needed).
- [ ] The same HTML contains no `<meta name='robots' content='noindex'>`
      (Community Existing is public and indexable).

### 6.4 No internal identifiers or provenance

In the public HTML of `/c/<community-slug>`, confirm **absent**:

- [ ] internal project id / community id / relation id
- [ ] the requester's or reviewer's username
- [ ] the request note
- [ ] any GitHub installation or account id
- [ ] any private repository name

### 6.5 Removal controls are role-correct

- [ ] User A on `/projects/<slug>/request-home` → *Remove home* present.
- [ ] User B on `/c/<community-slug>/settings?panel=projects` →
      *Connected projects* panel with a *Remove home* form.
- [ ] User C on `/c/<community-slug>/settings` → no access (or no
      *Connected projects* panel; the panel is `top_mod`/admin only).
- [ ] User C on `/c/<community-slug>` → the project is listed, but **no**
      removal control of any kind.

---

## Phase 7 — Dedicated-community provisioning (User A)

Use a **third** verified project (repeat Phase 2.3–2.5 with another
repository), so Phases 4–6 fixtures stay intact.

### 7.1 Choose the path

- [ ] `/projects/<slug3>/setup` → *Create a community home*
      → `/projects/<slug3>/community-home/new`.

### 7.2 Fill the form

| Field | Note |
| --- | --- |
| `community_name` | Prefix `LAUNCHTEST` |
| `community_slug` | Canonical slug, must be globally free |
| `community_description` | Optional |

- [ ] Submit (`POST /projects/<slug3>/community-home`).

### 7.3 Redirect

- [ ] You land on `/c/<new-slug>/settings` — the **private** settings page.

Expected event: **`dedicated_home_provisioned`**
(`user_id` only; this is **not** a publication).

### 7.4 The community is a private draft

Read-only confirmation:

```sql
SELECT is_network_community, onboarding_state, visibility, indexable, discoverable
FROM communities WHERE slug = '<new-slug>';
```

- [ ] `is_network_community = t`
- [ ] `onboarding_state = draft`
- [ ] `visibility = private`
- [ ] `indexable = f`
- [ ] `discoverable = f`

### 7.5 Initial membership and role

- [ ] User A is a member.
- [ ] User A is `top_mod`.

```sql
SELECT role FROM community_moderators
WHERE community_id = (SELECT id FROM communities WHERE slug = '<new-slug>');
```

### 7.6 Initial shell

- [ ] A forum section **General** (slug `general`) exists.
- [ ] A live channel **general** (slug `general`) exists.
- [ ] Both are visible on `/c/<new-slug>` as User A.

### 7.7 Accepted provisioned home relation

```sql
SELECT status FROM community_projects
WHERE community_id = (SELECT id FROM communities WHERE slug = '<new-slug>');
```

- [ ] Exactly one row, `accepted`.

### 7.8 The draft is genuinely private

- [ ] As User C: `/c/<new-slug>` → not found / not permitted.
- [ ] Logged out: `/c/<new-slug>` → not found / not permitted.
- [ ] `curl -s https://earde.com/c/<new-slug>` returns no community content.
- [ ] The draft does **not** appear in `/search`, `/feed`, or any discovery
      listing for User C.

### 7.9 Home removal is blocked while draft

- [ ] As User A, `/projects/<slug3>/request-home` shows the accepted home
      **without** a *Remove home* button, and says setup must be completed
      first.
- [ ] `/c/<new-slug>/settings?panel=projects` likewise offers no removal for
      the draft.

---

## Phase 8 — Public publication (User A)

### 8.1 Complete setup

- [ ] `/c/<new-slug>/settings` → **Complete setup and publish**
      → `/c/<new-slug>/setup`.

| Field | Note |
| --- | --- |
| `community_name` | Final display name |
| `community_slug` | Final canonical slug — **may differ** from the draft slug |
| `community_description` | Optional |
| `publication_visibility` | `public` |

- [ ] **Deliberately change the slug** here so the slug-move path is tested.
      Record: draft `________` → final `________`.
- [ ] Confirm the form offers exactly two visibility options, `public` and
      `unlisted`. **There must be no Private option.**
- [ ] Submit (`POST /c/<draft-slug>/publish`).

Expected event: **`network_community_published`**,
`publication_visibility = public`.

### 8.2 Final identity and redirect

- [ ] You are redirected to `/c/<final-slug>` (the **new** slug).
- [ ] The page shows the final name and description.

### 8.3 Old slug is gone

- [ ] `/c/<draft-slug>` → not found (no redirect, no alias — aliases are
      explicitly out of scope).

### 8.4 Public and discoverable

- [ ] Log out: `/c/<final-slug>` loads.
- [ ] `curl -s https://earde.com/c/<final-slug>` returns the community HTML.
- [ ] That HTML contains **no** `<meta name='robots' content='noindex'>`.
- [ ] The community appears in `/search` and in discovery listings for a
      logged-out visitor.

```sql
SELECT onboarding_state, visibility, indexable, discoverable
FROM communities WHERE slug = '<final-slug>';
```

- [ ] `published`, `public`, `t`, `t`.

### 8.5 Connection and shell survive publication

- [ ] The **Connected projects** section on `/c/<final-slug>` lists the
      project (visible logged out).
- [ ] The **General** section and **general** channel still exist.
- [ ] Exactly one community row and exactly one `accepted` relation:

```sql
SELECT count(*) FROM communities WHERE slug = '<final-slug>';
SELECT status, count(*) FROM community_projects
WHERE project_id = (SELECT id FROM open_source_projects WHERE slug = '<slug3>')
GROUP BY status;
```

- [ ] `1`, and a single `accepted` row.

### 8.6 Replayed publication

- [ ] Press Back and re-submit the publish form → conflict re-render, no
      second community, no second event.

---

## Phase 9 — Unlisted publication (User A)

Provision a **fourth** project + draft (repeat Phase 2.3–2.5 and Phase 7), so
this is a fresh relation rather than a re-publication.

- [ ] On `/c/<draft2-slug>/setup`, choose `publication_visibility = unlisted`
      and submit.

Expected event: **`network_community_published`**,
`publication_visibility = unlisted`.

Verify:

- [ ] Logged out, `/c/<final2-slug>` loads directly (unlisted is reachable).
- [ ] Its HTML **does** contain `<meta name='robots' content='noindex'>`.
- [ ] It does **not** appear in `/search` results or discovery listings for a
      logged-out visitor.
- [ ] The **Connected projects** section lists the project.
- [ ] **General** section and **general** channel still exist.

```sql
SELECT onboarding_state, visibility, indexable, discoverable
FROM communities WHERE slug = '<final2-slug>';
```

- [ ] `published`, `public`, `f`, `f` — unlisted withholds indexing and
      discovery; it is not a private community.
- [ ] The publish form offered only `public` and `unlisted`. **No Private
      option exists anywhere.**

---

## Phase 10 — Removal

Two surfaces, two **separate** relations. Never remove the same relation
twice — a replay is a silent no-op that emits nothing.

### 10.1 Project-side removal

Use the Phase 8 (Public) dedicated community.

- [ ] As User A: `/projects/<slug3>/request-home` → **Remove home**
      (`POST /projects/<slug3>/community-home/<final-slug>/remove`).
- [ ] Redirect back to `/projects/<slug3>/request-home`.

Expected event: **`project_home_removed`**, `removal_surface = project`.

### 10.2 Community-side removal

Use the Phase 4–6 (existing-community) accepted relation.

- [ ] As User B: `/c/<community-slug>/settings?panel=projects` →
      **Remove home**
      (`POST /c/<community-slug>/projects/<project-slug>/remove-home`).
- [ ] Redirect back to `/c/<community-slug>/settings`.

Expected event: **`project_home_removed`**, `removal_surface = community`.

### 10.3 Relation state

For each removal:

```sql
SELECT status FROM community_projects
WHERE project_id = (SELECT id FROM open_source_projects WHERE slug = '<slug>');
```

- [ ] `removed`. No row was deleted.

### 10.4 Community and content survive

- [ ] The community still loads at its URL.
- [ ] Its sections, channels, posts and messages are intact.

### 10.5 Notifications are correct and deduplicated

- [ ] After the **project-side** removal (actor User A), User A's own
      co-stewards and **all top moderators** of the community are notified —
      User A is not.
- [ ] After the **community-side** removal (actor User B), User A is notified
      — User B is not.
- [ ] Each recipient gets **exactly one** removal notification per relation.
- [ ] The removal notification reads
      *"<Project> is no longer connected to <Community> as its home"* and
      links to `/c/<community-slug>`.
- [ ] **Open that link as each recipient** — steward and top moderator alike
      must get **200**, not 404. *(This is the one behaviour corrected during
      the launch audit; verify it explicitly on both sides.)*

### 10.6 Public surfaces stop showing the home

- [ ] `/c/<community-slug>` no longer lists the project in *Connected
      projects* (logged out too).
- [ ] `/projects/<slug>/request-home` shows no active home.

### 10.7 History does not block a future home

- [ ] As User A, `/projects/<slug>/request-home` again offers the eligible
      community list — a `removed` (or `rejected`) history does not suppress
      it.
- [ ] Submit a fresh request against Community Existing → a new `pending`
      relation is created.
- [ ] Have User B **reject** it, to leave the fixtures tidy.

---

## Phase 11 — Privacy and negative paths

No destructive SQL against production is required or permitted here.

### 11.1 CSRF

- [ ] Save a copy of the request-home page HTML, edit the `dream.csrf` hidden
      field to a wrong value, and submit → rejected (not accepted, no state
      change).
- [ ] Same for `POST /c/<slug>/publish` and for one removal form.

### 11.2 Wrong origin

- [ ] From a different origin (or with a forged `Origin` header), POST to
      `/projects/<slug>/request-home` → rejected. No relation created.

### 11.3 Unauthorized setup access

- [ ] User C → `/projects/<slug>/setup` → 404.
- [ ] User C → `/projects/<slug>/request-home` → 404.
- [ ] User C → `/projects/<slug>/community-home/new` → 404.
- [ ] User C → `/c/<draft-slug>/setup` → not permitted.

### 11.4 Unauthorized review queue

- [ ] User A → `/c/<community-slug>/project-home-requests` → 404.
- [ ] User C → same → 404.
- [ ] Logged out → same → redirected to `/login`.

### 11.5 Stale or revoked project

- [ ] If the GitHub App is uninstalled from the personal account, confirm the
      project's verification renders as *Stale* or *Revoked* on
      `/c/<community-slug>` rather than disappearing or erroring.
- [ ] A revoked project cannot be **accepted** into a new home (the Accept
      control is withheld in the queue).
- [ ] Reinstall afterwards if you want the fixtures usable.

### 11.6 Conflicting community slug

- [ ] Provision a new draft and try to publish it onto the slug of an
      existing community → conflict re-render, no second community, no event.

### 11.7 Replays

- [ ] Replayed request → conflict, one relation (Phase 4.4).
- [ ] Replayed review → 409, no second event (Phase 5.5).
- [ ] Replayed publication → conflict, one community (Phase 8.6).
- [ ] Replayed removal → same redirect as success, **no** second
      `project_home_removed` event. Verify in PostHog Live Events.

### 11.8 Protected draft removal

- [ ] Provision a fresh draft; craft a `POST` to
      `/projects/<slug>/community-home/<draft-slug>/remove` (the form is not
      rendered, so build it from the known route with a valid CSRF token from
      another form on that page).
- [ ] The relation stays `accepted`; **no** `project_home_removed` event.

### 11.9 Deleted notification actor

- [ ] If a disposable test account can be deleted safely, delete a
      notification's actor and reload the recipient's `/notifications`.
- [ ] The line still renders, names no deleted user, and does not error.
- [ ] If no disposable account is available, mark this **NOT TESTED** rather
      than deleting a real one.

### 11.10 Nothing sensitive anywhere

Search the full HTML of `/projects/<slug>/setup`,
`/projects/<slug>/request-home`, `/c/<slug>/project-home-requests`,
`/c/<slug>/settings?panel=projects`, `/c/<slug>` and `/notifications` for:

- [ ] the client secret, any token, any `state=`/`code=` value → **zero**
      matches.
- [ ] the request note on any surface other than nowhere at all → **zero**
      matches.
- [ ] internal numeric ids in visible copy → none.
- [ ] In PostHog Live Events, no property contains a slug, a GitHub login, a
      repository name, or note text (see the ingestion checklist below).

Also confirm on the new routes:

- [ ] Response headers include `Cache-Control: no-store` on the private
      project/community-home pages and on their error responses.
- [ ] Those **pages** carry `Referrer-Policy: same-origin` — *not*
      `no-referrer`. This is deliberate and must not be "tightened": a
      document served `no-referrer` makes the browser attach `Origin: null`
      to any form it posts, and the same-origin gate on every one of these
      POST routes rejects a null origin, so every button on the page would
      answer `403 Not Allowed`. `same-origin` still sends no `Referer` at
      all to any cross-origin destination, so nothing leaks off-site.
- [ ] The **redirects** away from the two state-bearing callback URLs
      (`/integrations/github/install/return`,
      `/integrations/github/authorize/callback`) still carry
      `Referrer-Policy: no-referrer` alongside `Cache-Control: no-store` and
      `Pragma: no-cache`. Those render no form, so nothing depends on their
      `Origin`, and the `state=`/`code=` value must never travel onward as a
      `Referer`.

---

## Phase 12 — Cleanup

Audit and notification rows deliberately **RESTRICT-protect** their subject
projects and communities. Do **not** attempt to delete a project or community
that has audit history — there is no supported deletion workflow yet, and
forcing one would destroy audit evidence.

Preferred, in order:

- [ ] **Remove the test home relations** (Phase 10) so no live connection
      remains on a real community. This is the only cleanup that must happen.
- [ ] Leave test projects and communities in place, clearly marked with the
      `LAUNCHTEST` prefix.
- [ ] For test *communities* you do not want visible: set them to
      **private** and **non-discoverable** through
      `/c/<slug>/settings` rather than deleting them.
- [ ] Record the surviving fixtures here so a later supported deletion
      workflow can find them:

      Projects:    ________________________________
      Communities: ________________________________

- [ ] Do **not** run `DELETE`/`TRUNCATE` against `open_source_projects`,
      `communities`, `community_projects`, `project_home_audit_events`, or
      `notifications` in production.

---

## PostHog live production-ingestion checklist

### Required first checks

1. - [ ] Analytics consent is **granted** in the testing browser profile
        (`earde_analytics_consent=granted`).
2. - [ ] `POSTHOG_ENABLED=true`.
3. - [ ] `EARDE_DEPLOYMENT_ENVIRONMENT=production`, and
        `dune exec bin/check_posthog_config.exe` exits 0 with
        `VERIFIED` / `production`.
4. - [ ] Perform **one installation-start action**: `/bring` → start
        installation.
5. - [ ] Perform **one community-home success action**: submit a request, or
        provision a dedicated home, or publish one.
6. - [ ] Open **PostHog → Activity → Live Events** for project *Earde
        Production* (EU, project 229260).

### Confirm, per event

- [ ] The **exact** event name, from this closed list — nothing else counts:

      github_app_install_started
      github_app_installed
      github_repositories_selected
      github_project_created
      dedicated_home_provisioned
      network_community_published
      project_home_request_submitted
      project_home_request_reviewed
      project_home_removed

- [ ] `distinct_id` is exactly `user:<positive internal id>` — never an
      email, never a username, never a slug.
- [ ] `deployment_environment` = `production`.
- [ ] Only the allowed closed properties are present:

| Event | Allowed properties |
| --- | --- |
| `github_app_install_started` | `user_id` |
| `github_app_installed` | `user_id` |
| `github_repositories_selected` | `user_id`, `repository_count` |
| `github_project_created` | `user_id`, `project_id`, `project_kind`, `repository_count` |
| `dedicated_home_provisioned` | `user_id` |
| `network_community_published` | `user_id`, `publication_visibility` |
| `project_home_request_submitted` | `user_id` |
| `project_home_request_reviewed` | `user_id`, `decision` |
| `project_home_removed` | `user_id`, `removal_surface` |

- [ ] Closed value spellings only: `project_kind` ∈ {`project`,
      `organization`, `ecosystem`, `foundation`, `working_group`, `other`};
      `publication_visibility` ∈ {`public`, `unlisted`};
      `decision` ∈ {`accepted`, `rejected`};
      `removal_surface` ∈ {`project`, `community`}.
- [ ] **No project slug and no community slug** on any of these events.
- [ ] **No GitHub identity** — no login, namespace, installation id, account
      id, repository id, repository name or URL.
- [ ] **No OAuth/PKCE/request-note content** — no `state`, `code`, verifier,
      challenge, or note text.
- [ ] No `$groups` on any of the nine events (`community` grouping is
      deliberately absent here).
- [ ] No `$set` person properties on any of the nine events.

### Recommended dashboard components

Build these **in the PostHog UI**. Do not create dashboards through an API.

**1. GitHub project funnel** (person funnel):

```text
github_app_install_started
→ github_app_installed
→ github_repositories_selected
→ github_project_created
```

- [ ] Filter the repository-selection step on `repository_count > 0`
      (a committed `0` is a real event but not funnel progress).
- [ ] Useful breakdowns on the last step: `project_kind`, `repository_count`.

**2. Dedicated funnel** (person funnel):

```text
github_project_created
→ dedicated_home_provisioned
→ network_community_published
```

- [ ] Break the final step down by `publication_visibility`.

**3. Existing-community funnel** (person funnel, two steps only):

```text
github_project_created
→ project_home_request_submitted
```

- [ ] Show `project_home_request_reviewed` as a **separate** trend, broken
      down by `decision`.
- [ ] **Do not** build a three-step person funnel across the request and the
      review: they are two different people (steward and moderator), and no
      `project` group type exists to join them. See
      `docs/features/posthog-analytics.md` § Grouping.

**4. Lifecycle metric:**

```text
project_home_removed   — breakdown: removal_surface
```

- [ ] Added as a standalone trend, not a funnel step.

---

## Deployment and runbook notes

**Limitation.** This repository contains **no deployment runbook**. The only
operational document in-tree is `docs/features/posthog-analytics.md` (the
event contract). Nginx configuration, systemd unit definitions, release
procedure and rollback procedure live outside this repository. This section
therefore records only the **launch-critical deltas the new flow introduces**;
it deliberately does not restate or duplicate the external runbook.

### New environment variables required by this launch

| Variable | Required | Notes |
| --- | --- | --- |
| `EARDE_GITHUB_ONBOARDING_ENABLED` | yes | `off` / `admins` / `public`, exact lowercase. Anything else means `off`. Read per request — changing it takes effect on the next request, no restart needed. See §0.1 for the `admins` moderator constraint. |
| `EARDE_PUBLIC_ORIGIN` | yes | Exactly `https://earde.com`. Both GitHub URLs are validated against it, and PostHog production activation requires it. |
| `GITHUB_APP_SLUG` | yes | Public app slug. |
| `GITHUB_APP_CLIENT_ID` | yes | Public. |
| `GITHUB_APP_CLIENT_SECRET` | yes | **Secret** — server-only, never rendered or logged. |
| `GITHUB_APP_SETUP_URL` | yes | Must be `<origin>/integrations/github/install/return`. |
| `GITHUB_APP_CALLBACK_URL` | yes | Must be `<origin>/integrations/github/authorize/callback`. |
| `DREAM_SECRET` | yes (already) | Now also encrypts the per-flow GitHub onboarding cookie. Rotating it invalidates in-flight onboarding flows as well as sessions. |
| `POSTHOG_ENABLED` | for analytics | Exactly `true`. |
| `EARDE_DEPLOYMENT_ENVIRONMENT` | for analytics | Exactly `production`. Required whenever PostHog is enabled. |
| `POSTHOG_PROJECT_TOKEN` / `POSTHOG_API_HOST` / `POSTHOG_UI_HOST` / `POSTHOG_PROJECT_ID` / `POSTHOG_PERSONAL_API_KEY` | for analytics | Unchanged from the analytics release; the last two are **secret**. |

### Order of operations for this release

1. Set the new GitHub App variables with
   `EARDE_GITHUB_ONBOARDING_ENABLED=off` — the feature stays dark.
2. Run `dune exec bin/check_posthog_config.exe` **before** migrating or
   restarting. Exit 0 required.
3. Apply the nine migrations (§1.4). All are additive; none drops or rewrites
   an existing column.
4. Restart the Dream application service.
5. The Gleam realtime gateway is **untouched** by this feature and does not
   need to be restarted or ordered relative to the app.
6. Flip `EARDE_GITHUB_ONBOARDING_ENABLED` to `public` (or `admins`) to open
   the flow. This is the kill switch — flip it back to `off` to close the
   feature instantly without a rollback.

### Rollback implications

- **Preferred rollback is the kill switch**, not a migration rollback: set
  `EARDE_GITHUB_ONBOARDING_ENABLED=off`. Every route in this document then
  redirects to `/bring`, leaving durable data intact.
- The nine migrations are **forward-only**. There are no down-migrations, and
  none should be written.
- Once production has produced `project_home_audit_events` or project-home
  `notifications` rows, **no destructive rollback is acceptable**:
  `project_home_audit_events` RESTRICT-protects its subject project and
  community rows precisely so audit history cannot be silently erased, and
  the notification rows reference the same subjects. Dropping those tables
  would destroy moderation-relevant audit evidence.
- Rolling the **application binary** back to a pre-feature revision is safe
  with the new tables in place: the older code simply never reads them.
  Combine that with the kill switch rather than touching the schema.

---

## Launch gate summary

Fill in as you go. `READY TO TEST` means automated coverage is green but the
human check has not been performed. **Automated tests never make a gate PASS.**

| Gate | Status | Evidence | Blocker owner |
| --- | --- | --- | --- |
| GitHub installation | READY TO TEST | Phase 2.1–2.2; gated suite `github_start_handler_db`, `github_oauth_callback_db` green | |
| Repository selection | READY TO TEST | Phase 2.3–2.4; `analytics_funnel_handlers_db` 0–1 green | |
| Project creation | READY TO TEST | Phase 2.5–2.6, Phase 3; `analytics_funnel_handlers_db` 2–3 green | |
| Existing-community request | READY TO TEST | Phase 4; `analytics_funnel_handlers_db` 4–6 green | |
| Review and notification | READY TO TEST | Phase 5; `analytics_funnel_handlers_db` 7–9, `project_home_notifications_ui` green | |
| Dedicated provisioning | READY TO TEST | Phase 7; `analytics_funnel_handlers_db` 10–12 green | |
| Public publication | READY TO TEST | Phase 8; `analytics_funnel_handlers_db` 13–15 green | |
| Unlisted publication | READY TO TEST | Phase 9; same suite, `publication_visibility` cases green | |
| Removal | READY TO TEST | Phase 10; `analytics_funnel_handlers_db` 16–18, `project_home_notifications_ui` removal-destination case green | |
| Privacy / authorization | READY TO TEST | Phase 6.4, Phase 11; `project_home_notifications_privacy` and per-suite privacy sweeps green | |
| PostHog ingestion | NOT TESTED | Requires live production ingestion — no automated evidence possible | |
| Deploy rollback readiness | NOT TESTED | Requires confirming the external runbook against the deltas above | |

### Sign-off

| | |
| --- | --- |
| Tester | ________________ |
| Date | ________________ |
| Revision tested | ________________ |
| Decision | GO / NO-GO |
| Outstanding blockers | ________________ |
