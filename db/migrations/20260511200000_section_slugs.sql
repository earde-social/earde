-- migrate:up
ALTER TABLE community_sections ADD COLUMN slug TEXT NOT NULL DEFAULT '';
ALTER TABLE community_sections ADD CONSTRAINT community_sections_community_id_slug_key UNIQUE (community_id, slug);

-- migrate:down
ALTER TABLE community_sections DROP CONSTRAINT community_sections_community_id_slug_key;
ALTER TABLE community_sections DROP COLUMN slug;
