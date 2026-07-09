# The Orrery in Emoji — Design

**Date:** 2026-07-09
**Status:** Approved (brainstorming session)
**Kind:** Increment spec (a follow-up to The Orrery, Campaign 23 — not a new
campaign; it adds a render mode and a gallery artifact, and folds into The
Orrery's story).
**Parent specs:** `2026-07-09-the-orrery-design.md` and
`2026-07-09-rendering-strategy-design.md` (Rings 0–1; decision 0022 governs).
**Provenance:** The single-width Unicode orrery reads well, but emoji are the
semantically ideal glyphs — the phase set `🌑🌒🌓🌔🌕🌖🌗🌘` *is* an orrery's
moons. Emoji are also unconditionally **two terminal columns**, which breaks the
orrery's fixed-column grid: a 2-wide glyph in a 1-wide cell smears the row. This
increment is the experiment that meets that fragility with a grid built for it.

---

## 1. Goal

An **emoji rendering mode** for `hornvale orrery`, on a grid where every cell is
two display columns, so emoji align by construction. It is **additive**: the
single-width Unicode orrery stays the default and stays byte-identical; emoji is
a mode you select. A committed emoji `.cast` joins the Unicode one in the book,
stacked, so the two can be compared — and so the fragility is on the record,
watchable.

## 2. Design

**A semantic cell grid.** The orrery today builds a grid of `(char, color)` and
emits it directly. Emoji need *strings* (some are multi-scalar) and *width*, so
this increment refactors the orrery to a grid of **semantic cells** — an enum
`Cell { Empty, Ring, Star, World, Moon(phase_bucket) }` — and a **glyph set**
that renders a cell to a display string of a known width. Two glyph sets:

- **`Unicode`** (default): `★` star, `●` world, `·` ring, `○◐●◑` moons — each one
  display column, exactly as today. **The refactor must leave the Unicode
  orrery's bytes unchanged** (asserted against the committed `.cast`), so the
  existing artifact does not drift.
- **`Emoji`**: `🌞` star, `🌍` world, `🌑🌓🌕🌗` moons (the four phase buckets),
  and a **two-column** ring/blank — every cell renders to exactly two columns
  (an emoji, or `·` padded with a space, or two spaces). Single-scalar emoji are
  preferred (`🌞`/`🌍`/`🌑…` are one code point each) to keep the strings simple.

**The width invariant is the whole point.** Under `Emoji`, every cell is two
display columns, so a row is exactly `2 × ORRERY_WIDTH` columns and nothing can
smear regardless of the font — the same grid-integrity property the Unicode
orrery has at one column, doubled. The `.cast` header's `width` doubles to match.

**The flag.** `hornvale orrery --glyphs <unicode|emoji>` (default `unicode`).
The single-frame and `--cast` paths both honor it; `--fps` is unchanged.

## 3. Artifacts and the book

- **Two committed `.cast`s:** the existing `orrery-seed-42.cast` (Unicode) and a
  new `orrery-emoji-seed-42.cast`, both drift-checked, both regenerated in CI's
  "Artifacts are current" step.
- **One gallery page, stacked.** The Orrery gallery page carries **both**
  players top-by-bottom (they are wide — 61 vs 122 columns — so stacking reads
  better than side-by-side), each with a one-line caption. The emoji player is
  labeled as the experiment it is.
- **Book DoD (the lesson from The Orrery's retro):** this is an *increment*, not
  a campaign, so it takes **no new chronicle or retrospective**. It **does**
  update the astronomy chapter's orrery paragraph to name the emoji mode, and it
  is captured here in a spec. (If the reviewer or owner judges it campaign-sized,
  the fallback is a short chronicle note — flagged, not assumed.)

## 4. Determinism, fragility, and verification

- **Deterministic:** same seed + span + glyph set → byte-identical `.cast`
  (synthetic timing, no wall clock); both artifacts join the drift check.
- **The honest fragility:** emoji render at two columns *if* the player's font
  and width table say so — the standard for asciinema-player, but not universal.
  The 2-wide grid makes the workspace's side correct; the client's side is the
  client's. **Display width cannot be measured in a test** — that needs a
  Unicode-width table, and the workspace is `serde`-only (decision 0004). So
  verification splits: a test asserts the *construction* (every cell renders to a
  two-column-*designed* unit drawn from the known glyph set, so a row is
  `2 × ORRERY_WIDTH` columns by construction), and the *rendered display* is
  checked the way The Orrery's bugs taught — **build the book and watch the
  player**.

## 5. Testing

- Unicode orrery output is **byte-unchanged** by the refactor (a test compares a
  frame to the pre-refactor render, or the committed `.cast` frame).
- Emoji orrery: every row is composed of exactly `ORRERY_WIDTH` cells, each
  rendered to a two-column-designed unit from the known glyph set (construction
  check, not a display-width measurement — see §4); determinism; the
  star/world/a-moon-phase emoji are present; the world moves over time.
- The `--glyphs emoji` CLI path produces a valid, deterministic `.cast` whose
  header width is `2 × ORRERY_WIDTH`.

## 6. Non-goals

- **No emoji in the star map or any other render** — this increment is the
  orrery alone.
- **No per-terminal width negotiation or fallback logic** — the workspace emits
  a correct 2-wide grid; a client that renders emoji at the wrong width is out of
  scope (that is the client's font problem, decision 0022).
- **No new glyph sets beyond `unicode`/`emoji`**, and no runtime palette
  configuration — two named sets, chosen by a flag.
- **Emoji stays the orrery's mode, not its default** — the Unicode orrery
  remains what the book leads with and what other tooling assumes.

## 7. Consequences for existing documents

- `book/src/domains/astronomy.md` — the orrery paragraph names the emoji mode.
- The Orrery gallery page — gains the second, stacked player.
- CI "Artifacts are current" — regenerates the second `.cast`.
- No decision record, no registry row: this is a render mode, concrete work in
  the vein of the star chart and the orrery, not a new speculative direction.
