---
name: Earde
description: A focused technical commons where live community conversation becomes durable knowledge.
colors:
  signal-red: "oklch(0.555 0.155 27)"
  signal-red-strong: "oklch(0.490 0.165 27)"
  workshop-paper: "oklch(0.948 0.004 250)"
  panel: "oklch(0.995 0.002 250)"
  panel-subtle: "oklch(0.972 0.004 250)"
  panel-muted: "oklch(0.958 0.005 250)"
  ink: "oklch(0.255 0.012 262)"
  ink-secondary: "oklch(0.430 0.011 262)"
  ink-muted: "oklch(0.510 0.010 262)"
  line: "oklch(0.866 0.006 255)"
  line-strong: "oklch(0.800 0.007 255)"
  frame: "oklch(0.300 0.012 262)"
  frame-ink: "oklch(0.965 0.003 250)"
  success: "oklch(0.620 0.130 150)"
  success-ink: "oklch(0.400 0.110 150)"
  success-tint: "oklch(0.950 0.030 150)"
  warning: "oklch(0.680 0.140 70)"
  warning-ink: "oklch(0.450 0.120 70)"
  warning-tint: "oklch(0.960 0.040 70)"
typography:
  headline:
    fontFamily: "IBM Plex Mono, ui-monospace, SFMono-Regular, Menlo, monospace"
    fontSize: "21px"
    fontWeight: 700
    lineHeight: 1.2
    letterSpacing: "-0.01em"
  title:
    fontFamily: "IBM Plex Mono, ui-monospace, SFMono-Regular, Menlo, monospace"
    fontSize: "14px"
    fontWeight: 600
    lineHeight: 1.35
    letterSpacing: "-0.01em"
  row-title:
    fontFamily: "IBM Plex Sans, system-ui, -apple-system, sans-serif"
    fontSize: "15.5px"
    fontWeight: 600
    lineHeight: 1.35
    letterSpacing: "normal"
  body:
    fontFamily: "IBM Plex Sans, system-ui, -apple-system, sans-serif"
    fontSize: "14px"
    fontWeight: 400
    lineHeight: 1.5
    letterSpacing: "normal"
  support:
    fontFamily: "IBM Plex Sans, system-ui, -apple-system, sans-serif"
    fontSize: "13px"
    fontWeight: 400
    lineHeight: 1.55
    letterSpacing: "normal"
  meta:
    fontFamily: "IBM Plex Mono, ui-monospace, SFMono-Regular, Menlo, monospace"
    fontSize: "12.5px"
    fontWeight: 500
    lineHeight: 1.4
    letterSpacing: "normal"
  label:
    fontFamily: "IBM Plex Mono, ui-monospace, SFMono-Regular, Menlo, monospace"
    fontSize: "11.5px"
    fontWeight: 600
    lineHeight: 1.3
    letterSpacing: "0.04em"
rounded:
  control: "2px"
  media: "4px"
spacing:
  xs: "4px"
  sm: "8px"
  md: "12px"
  lg: "16px"
  xl: "24px"
  2xl: "28px"
components:
  button-primary:
    backgroundColor: "{colors.signal-red}"
    textColor: "{colors.frame-ink}"
    typography: "{typography.label}"
    rounded: "{rounded.control}"
    padding: "9px 16px"
  button-primary-hover:
    backgroundColor: "{colors.signal-red-strong}"
    textColor: "{colors.frame-ink}"
  button-secondary:
    backgroundColor: "{colors.panel-subtle}"
    textColor: "{colors.ink}"
    typography: "{typography.label}"
    rounded: "{rounded.control}"
    padding: "8px 14px"
  input:
    backgroundColor: "{colors.panel-subtle}"
    textColor: "{colors.ink}"
    typography: "{typography.body}"
    rounded: "{rounded.control}"
    padding: "9px 11px"
  panel:
    backgroundColor: "{colors.panel}"
    textColor: "{colors.ink}"
    rounded: "{rounded.control}"
    padding: "24px"
---

# Design System: Earde

## 1. Overview

**Creative North Star: "The Technical Commons"**

Earde is a dependable shared place where active technical exchange becomes lasting knowledge. The interface is calm, compact, and infrastructural: flat cool-grey surfaces, strong information structure, and restrained moments of red make it feel deliberately engineered rather than decorated.

The system serves sustained reading, navigation, discussion, and moderation. Dense multi-pane layouts are appropriate when they preserve context; focused single-column layouts are appropriate for forms and account tasks. Core app surfaces are desktop-first today, while public and authentication surfaces remain usable at narrower widths.

It explicitly rejects the noisy consumer social feed, the playful gamified chat app, and the generic rounded-card SaaS dashboard. It also rejects engagement bait, decorative gradients, excessive motion, and any treatment that makes valuable discussion feel transient.

**Key Characteristics:**

- Cool, low-chroma workshop surfaces with one restrained action color
- IBM Plex Sans for readable content and IBM Plex Mono for structure and controls
- Compact density, sharp corners, visible boundaries, and minimal elevation
- Server-rendered clarity with page-scoped enhancement
- Familiar interaction patterns that keep attention on community knowledge

## 2. Colors

Workshop Grey forms a quiet, durable neutral field; Signal Red is the sole primary accent and appears only where action, selection, identity, or orientation requires it.

### Primary

- **Signal Red:** Primary actions, current selections, community sigils, focused borders, and high-value links.
- **Strong Signal Red:** Hover and pressed states for primary controls; never a decorative fill.

### Secondary

- **Success Green:** Confirmed, healthy, or moderator-positive states.
- **Warning Ochre:** Cautionary system states that require attention without implying destructive action.

### Neutral

- **Workshop Paper:** The app canvas and graph-paper field behind focused panels.
- **Panel White:** Primary content surfaces and controls that need maximum clarity.
- **Subtle Panel and Muted Panel:** Secondary navigation, grouped fields, alternate rows, and hover states.
- **Primary Ink, Secondary Ink, and Muted Ink:** A three-step hierarchy for content, support text, and metadata.
- **Line and Strong Line:** Structural separators and interactive boundaries.
- **Frame and Frame Ink:** High-contrast structural bars and text placed on them.

### Named Rules

**The Signal Rule.** Signal Red is semantic, not decorative. It marks actions, active state, focus, or community identity and should remain visually scarce.

**The Native Neutral Rule.** Grey surfaces stay subtly tinted toward Earde's cool hue. Do not replace them with warm cream, beige, or generic Tailwind grey.

**The Contrast Rule.** Body copy must meet WCAG 2.2 AA, placeholders must remain readable, and color must never carry meaning alone.

## 3. Typography

**Display Font:** IBM Plex Mono (with system monospace fallbacks)  
**Body Font:** IBM Plex Sans (with system sans-serif fallbacks)  
**Label/Mono Font:** IBM Plex Mono

**Character:** The pairing is technically literate without becoming terminal cosplay. Sans-serif text supports sustained reading; monospace text identifies headings, labels, paths, metadata, and compact controls.

### Hierarchy

- **Headline** (700, 21px, 1.2): Page titles and focused workflow headings — every page's single `<h1>` renders at this size regardless of surface.
- **Row Title** (600, 15.5px, 1.35, sans): Thread titles in feed rows, search-result titles, and other list-item headings.
- **Title** (600, 14px, 1.35): Panel titles, section labels, and compact structural headings.
- **Body** (400, 14px, 1.5): Discussion content, descriptions, forms, and explanatory copy; prose should remain within 65–75ch.
- **Support** (400, 13px, 1.55, sans): Secondary explanatory copy — panel descriptions, empty states, help text under controls.
- **Meta** (500, 12.5px, mono): Timestamps, counts, paths, and other row metadata; sits between Support and Label.
- **Label** (600, 11.5px, 0.04em): Form labels, metadata, status, and compact controls. Uppercase is reserved for genuine labels and state badges, never used as a universal section eyebrow.

### Named Exceptions

- **Thread document title** (`.th-title`, 24px sans): the one long-form document heading in the product; sans at a larger size is deliberate for sustained reading of the thread page.
- **Tile and crest glyphs** (18px / 26px): single-letter identity tiles are display glyphs, not text, and size to their tile.
- **HQ dashboard** (`hq_dashboard_page`): standalone internal operator tool outside the shared chrome; not held to the product ramp.
- **Prose line-heights** (1.6–1.72): long-form legal/privacy prose may exceed the 1.5 body cadence.
- **Narrow-viewport downscales**: a media query may step a Headline down one notch (e.g. 21px → 19px) where the full size would wrap badly; it may not introduce new resting sizes.

### Named Rules

**The Structural Mono Rule.** Use IBM Plex Mono to expose hierarchy and system structure, not for long-form prose.

**The Compact Scale Rule.** Product typography uses fixed sizes and a tight ratio — the roles above are the whole ramp. Never introduce oversized fluid headings into task-focused screens, and pick the nearest role instead of minting a new size.

## 4. Elevation

The system is structural and flat. Depth comes from tonal surface changes, one-pixel borders, and pane geometry. Shadows are reserved for elements that truly leave the document plane: menus, dialogs, and the mobile gate.

### Shadow Vocabulary

- **Topbar Separator** (`0 1px 0 oklch(0.30 0.01 262 / 0.06)`): A restrained edge beneath sticky app chrome.
- **Floating Menu** (`0 6px 20px oklch(0.30 0.01 262 / 0.16)`): Dropdowns and other temporary floating surfaces.
- **Gate Panel** (`0 6px 24px oklch(0.30 0.01 262 / 0.12)`): The desktop-only notice on narrow screens.

### Named Rules

**The Structural Flat Rule.** If a border or tonal layer can establish hierarchy, a shadow is forbidden.

**The Floating Exception Rule.** Shadows indicate actual overlap or interruption; they never decorate resting cards or panels.

## 5. Components

Components are precise and restrained: compact controls, sharp corners, visible boundaries, and familiar states without decorative flourish.

### Buttons

- **Shape:** Sharp rectangular controls with a slight corner softening (2px).
- **Primary:** Signal Red with light text, a Strong Signal Red border, and compact horizontal padding.
- **Hover / Focus:** Hover deepens to Strong Signal Red. Focus uses a visible Signal Red boundary; `:focus-visible` must remain obvious without relying on color alone.
- **Secondary:** Subtle Panel fill, Primary Ink text, and a Strong Line border. Hover increases tonal and border contrast.

### Chips

- **Style:** Compact monospace labels with a 2px radius, one-pixel border, and Subtle Panel fill.
- **State:** Selected or semantic chips may use Signal Red, Success Green, Warning Ochre, or Frame, but text and border treatment must reinforce the state.

### Cards / Containers

- **Corner Style:** Sharp panels (2px); image crops may use the Media radius (4px).
- **Background:** Panel White over Workshop Paper, with Subtle Panel for nested groups.
- **Shadow Strategy:** Flat by default; use the floating exceptions from Elevation only.
- **Border:** One-pixel Line or Strong Line boundaries define grouping.
- **Internal Padding:** Compact panels use 16px; focused forms and account panels use 24–28px.

### Inputs / Fields

- **Style:** Subtle Panel fill, Strong Line border, 2px radius, IBM Plex Sans input text, and IBM Plex Mono labels.
- **Focus:** The field becomes Panel White and the border shifts to Signal Red.
- **Error / Disabled:** Errors use a pale red-tinted surface plus Strong Signal Red text and border. Disabled states reduce emphasis but must retain readable text and a visible control boundary.

### Navigation

- **Style:** The 54px app command bar uses compact mono controls, a centered search field, and a restrained right-side action cluster. Default links use Secondary Ink; hover uses a tonal surface; active state adds a stronger border or Signal Red indicator.
- **Structure:** Community workspaces use a persistent icon rail, navigation rail, main content pane, and optional context pane. Narrow app viewports currently receive an explicit desktop-only gate rather than a broken compressed shell.

### Discussion Rows

Discussion and chat rows are linear, compact, and content-led. Metadata uses monospace and Muted Ink; titles and message content use stronger ink. Hover reveals secondary actions without shifting layout, and durable thread actions remain visually distinct from transient chat controls.

## 6. Do's and Don'ts

### Do:

- **Do** use Workshop Paper, Panel White, and the two secondary panel tones to create hierarchy before adding elevation.
- **Do** reserve Signal Red for primary actions, selected state, focus, community identity, and high-value links.
- **Do** keep product controls compact, rectangular, and consistent at a 2px radius.
- **Do** preserve semantic server-rendered HTML, keyboard operation, visible focus, reduced-motion preferences, and WCAG 2.2 AA contrast.
- **Do** use IBM Plex Sans for readable content and IBM Plex Mono for structure, labels, paths, and metadata.
- **Do** design dense panes and lists around comprehension, retrieval, and moderation rather than visual novelty.

### Don't:

- **Don't** make Earde look like a noisy consumer social feed, a playful gamified chat app, or a generic rounded-card SaaS dashboard.
- **Don't** add engagement bait, decorative gradients, excessive motion, or transient-feeling UI.
- **Don't** introduce warm cream, sand, beige, glassmorphism, gradient text, or decorative glow as a replacement for the canonical cool-grey system.
- **Don't** place colored side-stripe borders on cards, callouts, alerts, or list items.
- **Don't** create grids of identical rounded cards when a linear list, pane, section divider, or table communicates structure more directly.
- **Don't** use oversized display type, universal uppercase eyebrows, or numbered section scaffolding on product screens.
- **Don't** use shadows on resting panels or animate layout properties; state transitions should stay near the existing 120ms cadence and respect reduced motion.
