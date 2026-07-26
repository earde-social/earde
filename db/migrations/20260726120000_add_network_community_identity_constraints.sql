-- migrate:up

-- Durable identity policy for network communities, scoped to
-- is_network_community = TRUE so historical legacy rows (whose identities
-- were never canonicalized beyond a trim) keep working untouched. The
-- provisioning form is the write-side gatekeeper; these constraints are the
-- final database defense, so their rules mirror the form's canonical policy
-- exactly. Both the dev and scratch databases were inspected before this
-- migration: they contain zero is_network_community rows, so the
-- constraints are added fully validated with no backfill.

-- Name: 1..120 Unicode characters (char_length counts characters, never
-- bytes), no ASCII control byte and no DEL anywhere. PostgreSQL text is
-- already valid in the database encoding and cannot contain NUL, so the
-- control-byte class starts at \x01. Of the six ASCII whitespace bytes the
-- form trims at the edges (space, tab, LF, CR, FF, VT), the last five are
-- control bytes banned everywhere by the class below, so the edge rule only
-- has to name the space.
ALTER TABLE communities
  ADD CONSTRAINT communities_network_name_check CHECK (
    NOT is_network_community OR (
      char_length(name) >= 1
      AND char_length(name) <= 120
      AND name !~ '[\x01-\x1f\x7f]'
      AND name !~ '^ '
      AND name !~ ' $'
    )
  );

-- Slug: the same permanent grammar open_source_projects.slug carries —
-- lowercase alphanumeric runs joined by single hyphens, at most 80
-- characters. The regex alone already forbids emptiness, uppercase,
-- underscores, slashes, spaces, and leading/trailing/consecutive hyphens.
ALTER TABLE communities
  ADD CONSTRAINT communities_network_slug_check CHECK (
    NOT is_network_community OR (
      slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'
      AND char_length(slug) <= 80
    )
  );

-- Description: NULL, or 1..2000 Unicode characters where LF and tab are the
-- only permitted control bytes (multiline text survives; CR, every other
-- control, and DEL do not) and no space, tab, or LF sits at either edge —
-- the canonical form collapses an empty description to NULL, so '' is
-- rejected by the >= 1 bound rather than stored.
ALTER TABLE communities
  ADD CONSTRAINT communities_network_description_check CHECK (
    NOT is_network_community OR description IS NULL OR (
      char_length(description) >= 1
      AND char_length(description) <= 2000
      AND description !~ '[\x01-\x08\x0b-\x1f\x7f]'
      AND description !~ '^[ \t\n]'
      AND description !~ '[ \t\n]$'
    )
  );

-- migrate:down

-- Remove only the three constraints this migration added; communities_slug_key,
-- the lifecycle/visibility CHECKs, and every index stay untouched.
ALTER TABLE communities DROP CONSTRAINT communities_network_description_check;
ALTER TABLE communities DROP CONSTRAINT communities_network_slug_check;
ALTER TABLE communities DROP CONSTRAINT communities_network_name_check;
