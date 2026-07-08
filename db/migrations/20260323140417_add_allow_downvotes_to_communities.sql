-- migrate:up
ALTER TABLE communities ADD COLUMN allow_downvotes BOOLEAN NOT NULL DEFAULT true;

-- migrate:down
ALTER TABLE communities DROP COLUMN allow_downvotes;

