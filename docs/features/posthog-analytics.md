# PostHog analytics — GitHub project and community-home funnels

This document is the contract for the server-side events that measure the
GitHub-anchored project journey and the two community-home journeys. It
describes what is emitted, exactly when, with which properties, and which
funnels those events are meant to support.

Everything here runs through the existing consent-aware analytics module
(`lib/analytics.ml` / `.mli`). No second analytics client exists, and none of
these events introduce browser instrumentation.

---

## Ground rules (unchanged by this slice)

- **Consent.** The only capture path for domain events is
  `Analytics.capture_if_consented`, which emits only when the request carries
  `earde_analytics_consent=granted` **and** analytics is enabled and validly
  configured. Missing, denied, or malformed consent produces no event. These
  are server-side handlers; that changes nothing about the policy.
- **Environment envelope.** Every payload carries exactly one
  `deployment_environment` property (`production` / `staging` /
  `development`), added once by the shared payload envelope. Event
  constructors are closed variants with no field for it, so a caller can
  neither duplicate nor override it.
- **Identity.** The distinct id is always `user:<positive internal user id>`.
  Never an email. No person properties (`$set`) are attached to any event
  below.
- **Best effort.** Capture returns `unit` immediately; the HTTP attempt is
  bounded and fire-and-forget, and every failure is swallowed. A PostHog
  outage cannot roll back business state, change an HTTP status, change a
  redirect destination, surface an error, or retry a mutation.
- **Post-commit only.** Every event below is captured by exactly one handler
  branch, after the relevant store returned `Ok` — never between a business
  mutation, its audit insertion, its notification insertion, and the commit.
  The stores themselves never call PostHog.
- **Private surfaces stay silent.** No inline browser tracking was added to
  the project setup pages, private community setup, the moderator review
  queue, or notification pages. The server-side success events are the only
  measurement of those flows.

---

## Event contract

| Event | Properties (besides `deployment_environment`) |
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

Closed value vocabularies — a third spelling is unrepresentable in the type:

- `project_kind` — the exact `Project_identity.kind` database spellings:
  `project`, `organization`, `ecosystem`, `foundation`, `working_group`,
  `other`.
- `publication_visibility` — `public` or `unlisted`. There is deliberately no
  `private`: a published network community is always reachable.
- `decision` — `accepted` or `rejected`.
- `removal_surface` — `project` or `community`. This is the **route** that
  emitted the form, not the authorization source (see below).

Numeric rules:

- `repository_count` is exported only within `0..2000`, the entire
  legitimate range of a draft snapshot selection. `0` is a real committed
  transition (a deliberately cleared selection). Anything outside the range
  is durable corruption and the property is omitted rather than exported.
- `project_id` is exported only when positive, as an exact bigint literal.

---

## Exact success boundaries

| Event | Captured at |
| --- | --- |
| `github_app_install_started` | `Github_onboarding_handlers.make_start_installation_handler`, after rollout/auth/origin gates pass, the onboarding state row commits, and the per-flow cookie is stored — i.e. the valid GitHub redirect is the response being returned. |
| `github_app_installed` | `Github_onboarding_handlers.finish_authorization`, after state consumption, GitHub verification, and **both** persistence steps (installation record + refreshed verified draft) commit. The owning user comes from the consumed state row; this leg is sessionless. |
| `github_repositories_selected` | `Project_setup_handlers.make_repository_selection_handler`, on `Project_onboarding_draft_selection_store.replace` returning `Ok` — one boundary shared by both continuations (details step and "selection required"). |
| `github_project_created` | `Project_creation_handlers.handle_finalization_result`, on `Project_finalization_store.finalize` returning `Ok`. |
| `dedicated_home_provisioned` | `Project_home_provisioning_handlers.handle_store_result`, on `Project_home_provisioning_store.provision` returning `Ok` with status `Accepted`. |
| `network_community_published` | `Network_community_publication_handlers.handle_store_result`, on `Network_community_publication_store.publish` returning `Ok`. |
| `project_home_request_submitted` | `Project_home_request_handlers.handle_store_result`, on `Project_home_request_store.create` returning `Ok` (pending relation + audit event + notifications committed). |
| `project_home_request_reviewed` | `Project_home_review_handlers.handle_review_result`, on `Project_home_review_store.review` returning `Ok` whose status matches the route's decision. |
| `project_home_removed` | `Project_home_removal_handlers.handle_removal_result`, on `Project_home_removal_store.remove` returning `Ok` with status `Removed`. Both route surfaces reach this through one private helper, so a removal can never be counted twice. |

**No event** is captured for: invalid forms, rollout or authentication gate
failures, origin/CSRF/configuration failures, unavailable projects,
communities or drafts, unauthorized actors, slug conflicts, duplicate
repository claims, active-home conflicts, GitHub callback/state failures,
replayed requests, reviews, publications and removals, protected
draft-removal attempts, losing concurrent transactions, `Inconsistent_data`,
and `Storage_error`.

Two collapses are worth noting because success and failure share a response:

- A removal that finds nothing to remove (`Removal_unavailable`) redirects to
  the same destination as a success. It emits nothing — the asymmetry is
  invisible to the client.
- A `Github_repositories_selected` with `repository_count = 0` is a real
  committed transition, not a failure.

---

## Grouping: what was decided and why

The hosted-project funnel crosses two people: a steward submits the request,
a moderator reviews it. A person funnel therefore cannot represent the
complete lifecycle.

**The preferred approach — a second `project` PostHog group type — was not
implemented.** The reason is availability, not architecture:

- `Project_finalization_store` exposes `project_id` publicly, so
  `github_project_created` can and does carry it.
- The five community-home lifecycle stores deliberately return narrow,
  payload-free results: `Project_home_request_store` yields a relation id,
  the review and removal stores yield only a resulting status, and the
  provisioning and publication stores yield only a canonical community slug.
  None of their handlers can reach a project id through an existing safe
  accessor.
- Recovering one would mean either reopening those store representations or
  running handler-side SQL purely for analytics. Both are out of scope for
  an instrumentation slice that must not touch business stores.

The documented fallback therefore applies: a typed opaque numeric
`project_id` property is carried where existing policy already permits it and
the value is already in hand (`github_project_created` only), and no group is
attached to any of the funnel events. `community` remains the project's only
PostHog group type.

**Consequence to keep in mind when building the dashboard:** the review step
cannot be joined to the rest of the lifecycle as a person funnel. Report
`project_home_request_reviewed` as its own metric, broken down by `decision`,
rather than as the terminal step of a person funnel. Everything else in the
GitHub and dedicated-community funnels is the same actor and works as an
ordinary person funnel.

If cross-actor project correlation later becomes worth it, the smallest
enabling change is narrow, server-side-only `project_id` accessors on the
five store result types — a store change, and therefore its own slice.

---

## Intended funnels

### GitHub project funnel (person funnel)

```
github_app_install_started
→ github_app_installed
→ github_repositories_selected
→ github_project_created
```

All four are the same person. `github_project_created` is the anchor of both
community-home funnels below. Useful breakdowns: `project_kind`,
`repository_count`.

### Dedicated community funnel (person funnel)

```
github_project_created
→ dedicated_home_provisioned
→ network_community_published
```

Break the final step down by `publication_visibility` (`public` /
`unlisted`).

Note the naming: `dedicated_home_provisioned` is **not** a publication. At
that point the community is still a private setup draft.

### Existing-community funnel

```
github_project_created
→ project_home_request_submitted
→ project_home_request_reviewed
```

The first two steps are the steward and work as a person funnel. The third is
the moderator; report it separately (see "Grouping" above), filtered by
`decision = accepted`, and report `decision = rejected` as its own number.

### Lifecycle metric

```
project_home_removed
```

A standalone lifecycle counter, optionally broken down by `removal_surface`
to see which surface people actually use.

---

## Events deliberately not in this slice

- `github_repositories_loaded` — **omitted**. Repository loading has no
  single durable, user-meaningful production boundary: the listing happens
  inside the OAuth callback, at the very same commit as
  `github_app_installed`, and `GET /projects/new` re-reads the stored
  snapshot on every ordinary page refresh. An event there would either
  duplicate `github_app_installed` or fire repeatedly on refresh.
- `github_onboarding_failed`, `github_repository_already_connected`,
  `project_home_choice_viewed`, `bring_community_started` — out of scope.
  The first two have no single explicit user action with a stable closed
  failure contract at one boundary; the last two are GET page impressions
  already covered by the manual sanitized `$pageview`.
- `community_published` as a name for provisioning — explicitly rejected, and
  asserted absent in the tests.

---

## Privacy

None of these events can carry — the closed variants make it unrepresentable
— a user email or username, a project or community name or slug, a GitHub
login or namespace, a repository name, full name or URL, a GitHub
installation/account/repository id, an OAuth state or code, PKCE material, a
request or review note, a description, a form value, a raw error or exception
message, a request URL or query string, an internal authorization source, or
a session value.

In particular, `removal_surface` reports the route, never which of steward /
top moderator / durable admin authorized the removal — the removal store
admits all three from either route, and exporting which applied would leak
durable role state.

---

## Tests

- DB-free contract: `analytics_funnel_contract`, `analytics_funnel_consent`
  (stable names, closed property allowlists, closed value spellings, central
  environment injection, group absence, id/count omission rules, consent and
  configuration gating, transport-failure swallowing).
- Gated end-to-end over the real handlers, real stores and a real Dream
  pipeline: `analytics_funnel_handlers_db`, plus the installation events in
  `github_start_handler_db` and `github_oauth_callback_db`.
- No test contacts a real PostHog project: the module's `For_testing` capture
  sink replaces the HTTP transport entirely for every analytics assertion.
