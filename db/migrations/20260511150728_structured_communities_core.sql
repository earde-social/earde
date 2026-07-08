-- migrate:up
ALTER TABLE communities ADD COLUMN sections_enabled BOOLEAN NOT NULL DEFAULT false;

-- community_sections ordered by position ASC; ON DELETE CASCADE mirrors posts→communities.
CREATE TABLE community_sections (
  id                      SERIAL PRIMARY KEY,
  community_id            INT  NOT NULL REFERENCES communities(id) ON DELETE CASCADE,
  name                    TEXT NOT NULL,
  description             TEXT,
  position                INT  NOT NULL DEFAULT 0,
  default_sort            TEXT NOT NULL DEFAULT 'new',
  is_introduction_section BOOLEAN NOT NULL DEFAULT false
);

-- NULL section_id = post belongs to the community root feed, not any sub-section.
-- ON DELETE SET NULL: deleting a section releases its posts back to the root feed.
ALTER TABLE posts ADD COLUMN section_id INT REFERENCES community_sections(id) ON DELETE SET NULL;

-- last_activity_at drives the "active" sort — bumped on every new comment.
-- Separate from created_at so activity-ordered feeds don't mix creation and engagement signals.
ALTER TABLE posts ADD COLUMN last_activity_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP;

-- Backfill from created_at so existing posts start with a sensible activity timestamp.
UPDATE posts SET last_activity_at = created_at;

-- migrate:down
ALTER TABLE posts DROP COLUMN last_activity_at;
ALTER TABLE posts DROP COLUMN section_id;
DROP TABLE community_sections;
ALTER TABLE communities DROP COLUMN sections_enabled;
