-- migrate:up
-- notif_type distinguishes comment_reply / mention / mod_action so pages.ml
-- can render distinct icons and messages without parsing message text.
-- post_id becomes nullable to support ban notifications that have no post.
ALTER TABLE notifications ADD COLUMN notif_type VARCHAR(50) NOT NULL DEFAULT 'comment_reply';
ALTER TABLE notifications ALTER COLUMN post_id DROP NOT NULL;

-- migrate:down
ALTER TABLE notifications ALTER COLUMN post_id SET NOT NULL;
ALTER TABLE notifications DROP COLUMN notif_type;
