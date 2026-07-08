-- migrate:up

-- Private/public access control + SEO indexability. Two orthogonal concerns:
--   * visibility = server-side ACCESS control (who may read at all). Private => gated.
--   * indexable  = SEO/DISCOVERY behavior (should crawlers + our public feed/search list it).
-- Noindex is NOT privacy: a public+indexable=false community stays readable by anyone with the
-- URL, it is only excluded from discovery. A private community is always effectively
-- non-indexable regardless of this flag (enforced in the app's effective-rules helpers, not here).
-- All columns are additive with safe defaults so every existing row stays public + indexable;
-- this slice adds storage + types only, no access/discovery behavior yet.

-- text + CHECK (not boolean) leaves room for a future 'unlisted' value without a type change,
-- mirroring the reports.status / reason CHECK style. App side decodes via a closed variant.
ALTER TABLE communities
  ADD COLUMN visibility text NOT NULL DEFAULT 'public'
    CONSTRAINT communities_visibility_check CHECK (visibility IN ('public', 'private'));

ALTER TABLE communities       ADD COLUMN indexable boolean NOT NULL DEFAULT true;

-- Per-child indexability is SEO-only and never implies privacy on its own.
ALTER TABLE channels          ADD COLUMN indexable boolean NOT NULL DEFAULT true;
ALTER TABLE community_sections ADD COLUMN indexable boolean NOT NULL DEFAULT true;

-- migrate:down

ALTER TABLE community_sections DROP COLUMN indexable;
ALTER TABLE channels          DROP COLUMN indexable;
ALTER TABLE communities       DROP COLUMN indexable;
ALTER TABLE communities       DROP COLUMN visibility;
