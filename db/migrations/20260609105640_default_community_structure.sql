-- migrate:up

-- Post-pivot invariant: every community is a shell container that always has a
-- default forum section (slug 'general') and a default chat channel (slug 'general'),
-- like Discord auto-creating #general. The simple-vs-structured split is retired for
-- new data; sections_enabled is kept (not dropped) but flipped on everywhere.

-- 1. Every existing community becomes structured.
UPDATE communities SET sections_enabled = true;

-- 2. Cement the invariant at the schema level so future raw inserts default on.
ALTER TABLE communities ALTER COLUMN sections_enabled SET DEFAULT true;

-- 3. Create a "General" forum section for any community lacking a 'general'-slug
--    section. NOT EXISTS reuses a pre-existing 'general' section and avoids the
--    UNIQUE (community_id, slug) conflict.
INSERT INTO community_sections
  (community_id, name, slug, description, position, default_sort, is_introduction_section)
SELECT c.id, 'General', 'general', 'General discussion', 0, 'new', false
FROM communities c
WHERE NOT EXISTS (
  SELECT 1 FROM community_sections s
  WHERE s.community_id = c.id AND s.slug = 'general'
);

-- 4. Create a 'general' chat channel for any community lacking one. Same reuse guard.
INSERT INTO channels (community_id, slug, name, topic, position)
SELECT c.id, 'general', 'general', 'General chat', 0
FROM communities c
WHERE NOT EXISTS (
  SELECT 1 FROM channels ch
  WHERE ch.community_id = c.id AND ch.slug = 'general'
);

-- 5. Move orphaned posts (section_id IS NULL) into their community's General section.
--    Runs after step 3 so the target section always exists.
UPDATE posts p
SET section_id = s.id
FROM community_sections s
WHERE p.section_id IS NULL
  AND s.community_id = p.community_id
  AND s.slug = 'general';

-- migrate:down
ALTER TABLE communities ALTER COLUMN sections_enabled SET DEFAULT false;
-- Data backfill is intentionally irreversible (forward-only convention):
--   * generated "General" sections are NOT deleted
--   * generated "general" channels are NOT deleted
--   * posts moved into General are NOT moved back to section_id NULL
-- A precise data inverse is impossible (we cannot know which sections/channels
-- pre-existed nor which posts were originally NULL), and a destructive teardown
-- would lose real community content.
