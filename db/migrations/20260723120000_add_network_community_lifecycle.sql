-- migrate:up

-- Storage-only foundation for the network-community (GitHub-provisioned) lifecycle.
-- Three additive columns, orthogonal to the existing pair:
--   * visibility   (existing) = server-side ACCESS control (who may read at all).
--   * indexable    (existing) = SEO behavior (should external crawlers index it).
--   * discoverable (new)      = whether the community may appear in Earde's own
--                               discovery surfaces (directory/feeds/search listings).
--   * onboarding_state (new)  = network-community setup lifecycle: 'draft' while a
--                               provisioned community is being configured, 'published'
--                               once live. Every existing community is already live.
--   * is_network_community (new) = marks GitHub-provisioned communities, whose
--                               invariants (e.g. never fully private after publish)
--                               will be enforced in later slices.
-- Defaults make every existing row a legacy community: not network, published,
-- discoverable. Existing visibility/indexable values are untouched — no backfill.
-- This slice adds storage only; no access/discovery/publication behavior yet.

-- text + CHECK (not boolean) mirrors the communities_visibility_check style and
-- leaves room for future states without a type change. App side decodes via a
-- closed variant.
ALTER TABLE communities
  ADD COLUMN is_network_community boolean NOT NULL DEFAULT false;

ALTER TABLE communities
  ADD COLUMN onboarding_state text NOT NULL DEFAULT 'published'
    CONSTRAINT communities_onboarding_state_check CHECK (onboarding_state IN ('draft', 'published'));

ALTER TABLE communities
  ADD COLUMN discoverable boolean NOT NULL DEFAULT true;

-- migrate:down

ALTER TABLE communities DROP COLUMN discoverable;
ALTER TABLE communities DROP COLUMN onboarding_state;
ALTER TABLE communities DROP COLUMN is_network_community;
