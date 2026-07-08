-- migrate:up
-- NULL = link/text-only post; avoids a nullable join table just for optional images.
ALTER TABLE posts ADD COLUMN image_url TEXT;

-- migrate:down
ALTER TABLE posts DROP COLUMN image_url;
