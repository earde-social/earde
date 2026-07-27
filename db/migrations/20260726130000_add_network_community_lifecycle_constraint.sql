-- migrate:up

-- Durable lifecycle policy for network communities, scoped to
-- is_network_community = TRUE so historical legacy rows keep working
-- untouched. Until now the whole-state invariant lived only in
-- Network_communities.lifecycle_state_valid, and the legacy settings
-- handlers demonstrated that malformed combinations could otherwise be
-- written; this CHECK is the final database defense, so its shape mirrors
-- the pure predicate exactly:
--
--   draft      -> private and neither indexable nor discoverable (a draft
--                 must not leak through indexing or discovery);
--   published  -> public, in exactly one of the two supported shapes:
--                 fully listed (indexable + discoverable) or fully unlisted
--                 (neither) - spelled indexable = discoverable, which also
--                 rejects both mixed flag combinations. A published network
--                 community can never be fully private.
--
-- The onboarding_state and visibility enum CHECKs already close the two
-- text vocabularies, so this constraint only has to pair them. Both the
-- dev and scratch databases were inspected before this migration: they
-- contain zero is_network_community rows, so the constraint is added
-- fully validated with no backfill.
ALTER TABLE communities
  ADD CONSTRAINT communities_network_lifecycle_check CHECK (
    NOT is_network_community OR (
      (onboarding_state = 'draft'
       AND visibility = 'private'
       AND NOT indexable
       AND NOT discoverable)
      OR
      (onboarding_state = 'published'
       AND visibility = 'public'
       AND indexable = discoverable)
    )
  );

-- migrate:down

-- Remove only the lifecycle constraint this migration added; the enum and
-- scoped identity constraints, communities_slug_key, and every index stay
-- untouched.
ALTER TABLE communities DROP CONSTRAINT communities_network_lifecycle_check;
