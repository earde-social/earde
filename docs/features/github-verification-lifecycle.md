# GitHub verification lifecycle

This is how Earde decides that a project is "verified through GitHub", how long
that proof counts, what happens when access to GitHub is lost, and when a
repository claim is released.

## Evidence

- **Onboarding** (`/bring` → GitHub App install → OAuth callback) is the only
  place GitHub is asked. The callback lists the user's installations and the
  installation's public repositories with the user's own token. Identities
  are numeric and come from those server-side calls, never from submitted ids
  or URLs. No GitHub token is stored.
- A successful callback writes a **draft** with a snapshot of the listed
  repositories and `verified_at = NOW()`. The draft can be finalized for 24
  hours.
- **Finalization** creates the project and one `project_stewards` row for the
  finalizing user. The row's `github_verified_at` is the draft's
  `verified_at`: the time GitHub last confirmed this user's access.
- **Renewal** uses the same flow: the steward connects the project again. On
  a successful callback, the draft store renews `github_verified_at` for that
  user's own steward rows. It does so only for projects in the verified
  GitHub account whose claimed repositories all appear in the new listing.

## Freshness

Evidence is **fresh for 30 days** after `github_verified_at`
(`github_evidence_is_fresh` in the database, the only place this interval
exists). At exactly 30 days it is no longer fresh.

| Action | Requires |
|---|---|
| Request a home in an existing community | the acting steward's own evidence is fresh |
| Create (provision) a community home | the acting steward's own evidence is fresh |
| Moderator accepts a pending home request | the project is currently verified (some steward is fresh) |
| Moderator rejects a request | nothing more (allowed in every state) |
| Remove a home (steward, top moderator, admin) | unchanged: allowed in every state |
| Publish a provisioned community | unchanged: top moderator or admin |

The label a project shows is its **effective** status
(`project_github_verification`). A stored `verified` reads `stale` once no
steward has fresh evidence. A stored `stale` or `revoked` passes through. A
request or provisioning attempt with stale evidence gets the same "not
available" answer as any other missing authority. The setup page tells
stewards how to renew.

## Access loss

- If GitHub no longer lists a claimed repository for the steward, or no
  longer lists the installation for them, the callback renews nothing. The
  evidence goes stale when the window passes.
- The callback does not tell a revoked installation apart from one the caller
  cannot see, or from a GitHub outage: all three are one generic failure that
  writes nothing. So one user's failed verification never changes another
  user's evidence, and there is no background re-check and no webhook.
- Nothing is deleted or rewritten when evidence goes stale. Accepted homes,
  community roles gained at provisioning (top moderator), memberships, posts
  and communities stay as they are. GitHub drift does not change community
  governance. Removing a home remains the governance action for a stale
  project.

## Repository claims

A repository (by numeric GitHub id) can be claimed by one project at a time.
The partial unique index `uniq_project_repositories_active_claim` covers
unreleased claims.

- While the claiming project has **any steward with fresh evidence**, a new
  finalization for that repository is refused (`Repository_already_connected`).
- Once **no steward** of the claiming project has fresh evidence, the claim no
  longer excludes. The next finalization of that repository by a user whose
  draft GitHub verified within the last 24 hours, for the repository's own
  account, releases all of the stale project's claims in the same
  transaction and takes the repository.
  - A user without current GitHub access cannot take a repository: they
    cannot produce such a draft.
  - The released rows stay as history (`released_at`). The old project keeps
    its row, its homes and its label, which reads `stale`. Its steward can no
    longer renew evidence over released claims.
- If the finalization fails for any other reason, nothing is released.
- There is no manual release action and no transfer of an existing project
  to another user.

## Deployment note

The migration `20260929130000_add_github_verification_freshness` and the
application change must be deployed together. The old finalization query
depends on the unique constraint that the migration replaces.

After the migration, existing steward evidence is dated 24 hours before the
steward row was created. So projects finalized more than about 29 days
earlier read `stale` until a steward connects the project again.
