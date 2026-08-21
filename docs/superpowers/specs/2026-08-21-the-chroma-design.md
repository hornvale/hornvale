# The Chroma — design

**Date:** 2026-08-21
**Status:** draft (G3 review pending)
**Predecessor:** The Stylus (2026-08-20)
**Campaign worktree:** `.claude/worktrees/the-chroma` (`campaign/the-chroma`)

## 0. Summary

The TUI game (`clients/game`) renders entirely in one ink. This campaign
gives it color: the chamber plan takes its palette colors and the walk-band
chart takes the scene's per-cell colors, both of which are **already on the
wire with zero readers**. No workspace change is required for the core
campaign; every edit lands in `clients/game`, outside the cargo workspace
and outside determinism (decision 0055).

The governing channel discipline is `CLIENT-four-channels`: glyph carries
identity, the grid position, **colour carries substance**, weight carries
attention. Color is the channel that may fail — monochrome remains the
floor, and every colored render must degrade to today's byte-identical
output.

## 1. Current state (verified on the tree)

- `core/src/cell.rs` defines `Ink` with the single variant `Plain`; its doc
  reads "Monochrome for this campaign; colour is deferred." This is the
  designed-in extension point.
- `core/src/schema.rs`'s `PaletteEntry.color: Option<[u8; 3]>` is mirrored
  from the wire but read by nothing ("monochrome is the whole visual
  channel in this campaign").
- The producer (`windows/vessel/src/session.rs`) already emits plan colors
  through a lens, with two deliberate withholdings: the you-mark (`@`) is
  never tinted, and an absent palette color means "no colour is claimed
  here," never black.
- The snapshot's chart is built through `surrounds_scene_colored_in`
  (`windows/vessel/src/purview.rs`) whenever there is an observing eye, so
  `SurroundsCell.color` arrives tinted through the possessed creature's
  actual `Sight`, with the honest `sight` block riding along (The
  Beholding).
- `bin/src/term.rs` draws through crossterm and already applies Bold/Dim
  attributes.

## 2. Design

### 2.1 Ink

```rust
pub enum Ink {
    /// The default ink.
    Plain,
    /// A truecolor foreground claim carried off the wire.
    Rgb([u8; 3]),
}
```

`Cell` gains no new field: ink joins `Weight` and `Source` as the third
attribute of the existing cell struct.

**Foreground = cover; background = substrate.** A future background
channel would carry the substance *under* (soil), foreground the cover
(grass) — both claims of substance, consistent with four-channels. The
producer already computes chart color as "surface cover over the mineral
blend," so that reading is native to the sim. A later campaign would add
`Ink::Duo { fg, bg }` (client-internal enum, additive variant, no wire
break; wire path: an additive `ground_color` field under the scene
schemas' additive-or-versioned rule). **Not in scope for The Chroma**, but
`Ink`'s doc comment must state the fg=cover / bg=substrate reading now so
the future variant inherits it rather than re-arguing it.

### 2.2 Plan pane

`plan.rs` already indexes `palette[cells[i]]` for glyphs; it now resolves
`entry.color` → `Ink::Rgb` when present, `Ink::Plain` when absent or when
drawing the you-mark or any mark. The withholding set mirrors the
producer's `tint()` exactly (#4 in the ledger): marks are identity, and
identity belongs to glyph.

### 2.3 Chart pane

`chart.rs` reads each `ChartCell.color` the same way. With color comes the
caption obligation (RENDER-9 / The Lens: *the caption, not the picture, is
the load-bearing honesty*): when the pane renders colored cells it shows a
one-line disclosure naming the eye and what its projection preserves, read
off the document's `sight` block — the same role as vessel browser's
color-disclosure sentence. When the scene arrived uncolored (observer
declined sight), cells carry no `color` key, the pane renders monochrome,
and no caption appears: absence is legible, not faked.

**`NO_COLOR` suppresses the caption too.** A caption naming an eye above a
monochrome render would claim a fidelity the pixels did not have.

### 2.4 Rendering and degradation

`term.rs`'s draw loop adds `SetForegroundColor` from `Ink`, composing with
existing Bold/Dim attributes. Truecolor escape (`38;2;r;g;b`) is emitted
unconditionally — the producer's own documented rationale: *"a terminal
that does not understand truecolor degrades to an uncoloured glyph rather
than to a wrong one."* No 256-cube quantization exists to get wrong.

`NO_COLOR` set → every `Rgb` maps to `Plain` at cell-build time, so the
buffer itself is monochrome while the provenance trace still names what
*would* have been colored. Degradation is observable, not silent.

## 3. Failure modes

| failure | answer |
|---|---|
| terminal cannot show truecolor | plain ink; unconditional truecolor emit, no quantizer |
| chart arrives uncolored | cells lack the key → `Plain`, no caption |
| malformed color on the wire | impossible by type: `Option<[u8; 3]>`; serde rejects at parse — vessel's "malformed → uncoloured" rule holds by construction |

## 4. Testing

1. **Monochrome floor acceptance test:** with `NO_COLOR` set, and with all
   colors absent, the composed screen is byte-identical to pre-Chroma
   output. Pins "colour may fail."
2. **Provenance trace:** every colored cell's `Source` tag still names its
   channel; colored plan cells trace to the palette, colored chart cells to
   the chart document.
3. **Withholding tests:** you-mark and marks untinted even standing on a
   colored cell.
4. **Fixture-driven:** assertions extend the committed session fixtures in
   `core/tests/fixtures/` (The Quire discipline: real-world assertions read
   fixtures, never genesis).

## 5. Gate reality

`make gate-commit` compiles none of `clients/`. Every commit in this
campaign runs `make game-check`. (The Stylus retrospective: a green commit
gate said nothing about the client, every commit.)

## 6. Out of scope

- Background color / `Ink::Duo` (recorded at §2.1 for a successor).
- Prose/transcript tinting (rejected: duplicates weight/glyph channels).
- Any workspace-side change; any new seed draw, epoch, or schema version
  bump.
- Lens switching in the TUI (the client renders the sight it is given;
  a lens picker is unprecented territory deferred without a row).

## 7. Decisions carried from the ledger

| # | decision |
|---|---|
| 2 | scope: plan pane primary, chart pane stretch (both data on-wire) |
| 3 | always truecolor; `NO_COLOR` kill-switch; absent → Plain, never black |
| 4 | withholding mirrors producer `tint()`: you-mark, marks, unclaimed cells |
| 5 | `Ink` admits future `Duo { fg, bg }`; fg=cover, bg=substrate documented now |
| 6 | `NO_COLOR` suppresses the sight-disclosure caption too |
