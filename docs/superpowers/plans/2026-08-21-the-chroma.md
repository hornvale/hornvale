# The Chroma Implementation Plan

> **REQUIRED SUB-SKILL:** Use the executing-plans skill to implement this plan task-by-task.

**Goal:** Give `clients/game`'s TUI color — the chamber plan takes its palette colors and the walk-band chart takes the scene's per-cell colors, both already on the wire — under the monochrome-floor discipline.

**Architecture:** `Ink` (currently `Plain`-only) gains an `Rgb([u8; 3])` variant; pane drawers resolve wire colors into ink with the producer's withholding rules; `term.rs` emits truecolor foreground escapes composed with existing Bold/Dim attributes; `NO_COLOR` maps every `Rgb` to `Plain` at cell-build time so degradation is observable in the buffer itself.

**Tech Stack:** Rust 2024, crossterm 0.29, serde. Everything lives in `clients/game` (outside the cargo workspace). **`make gate-commit` compiles none of this — run `make game-check` before every commit** (The Stylus retro).

**Spec:** `docs/superpowers/specs/2026-08-21-the-chroma-design.md`

---

## Ground rules for every task

- Work in `.claude/worktrees/the-chroma` on `campaign/the-chroma`. All paths below are relative to `clients/game/`.
- Tests run with `cargo test -p hornvale-game-core` (unit tests live in-module) from the worktree root; full gate is `make game-check`.
- **Never prescribe a mutation you haven't verified compiles** — if a code sketch below doesn't match the tree when you read it, trust the tree, note the delta in your task report, and adapt.
- Every commit: `git add <files> && git commit`, then `make game-check` must be green before you push or stack further work.

---

### Task 1: `Ink::Rgb` and the wire→ink gate

**TDD scenario:** New feature — full TDD cycle.

**Files:**
- Modify: `core/src/cell.rs` (the `Ink` enum, ~line 22)
- Test: in-module `#[cfg(test)]` in `core/src/cell.rs`

**Step 1: Write the failing tests**

Add to `cell.rs`'s test module:

```rust
/// Absent colour means "no colour claimed here", never black — the
/// producer's own rule (windows/vessel/src/session.rs `tint`).
#[test]
fn absent_wire_colour_is_plain_ink() {
    assert_eq!(Ink::from_wire(None), Ink::Plain);
}

/// NO_COLOR maps every Rgb to Plain at cell-build time, so the buffer
/// itself is monochrome and degradation is observable, not silent.
#[test]
fn no_color_env_forces_plain_ink() {
    // SAFETY: tests run process-per-test under nextest; no other test
    // reads this var concurrently.
    std::env::set_var("NO_COLOR", "1");
    assert_eq!(Ink::from_wire(Some([36, 36, 1])), Ink::Plain);
    std::env::remove_var("NO_COLOR");
    assert_eq!(Ink::from_wire(Some([36, 36, 1])), Ink::Rgb([36, 36, 1]));
}
```

(If nextest runs these in one process and the env var leaks, hoist both into one test or use a serial guard — match whatever the crate already does for env-sensitive tests, if anything.)

**Step 2: Run to verify failure**

Run: `cargo test -p hornvale-game-core from_wire`
Expected: FAIL — `from_wire` does not exist.

**Step 3: Implement**

```rust
/// What a thing IS. `Plain` is the floor; `Rgb` carries substance off the
/// wire. **Foreground = cover, background = substrate**: if a future
/// campaign adds background colour (`Ink::Duo { fg, bg }`), fg carries what
/// grows/sits on a cell and bg the material under it — both claims of
/// substance per CLIENT-four-channels, never identity or attention.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Ink {
    /// The default ink.
    #[default]
    Plain,
    /// A truecolor foreground claim carried off the wire.
    Rgb([u8; 3]),
}

impl Ink {
    /// Resolve a wire colour claim to ink. `None` (no colour claimed) and
    /// `NO_COLOR` set (the reader declined colour) both yield [`Ink::Plain`]
    /// — absence is legible, never faked as black.
    pub fn from_wire(color: Option<[u8; 3]>) -> Ink {
        if std::env::var_os("NO_COLOR").is_some_and(|v| !v.is_empty()) {
            return Ink::Plain;
        }
        color.map(Ink::Rgb).unwrap_or(Ink::Plain)
    }
}
```

Update `Cell::glyph`'s callers stay untouched: add a second constructor

```rust
/// A cell with a colour claim resolved through [`Ink::from_wire`].
pub fn inked(glyph: char, weight: Weight, source: Source, color: Option<[u8; 3]>) -> Cell {
    Cell { glyph: Some(glyph), weight, ink: Ink::from_wire(color), source }
}
```

and keep `Cell::glyph` as the `Ink::Plain` shorthand (it must remain byte-for-byte today's behaviour — Task 5's monochrome-floor test pins this).

**Step 4: Run to verify pass**

Run: `cargo test -p hornvale-game-core`
Expected: PASS (all existing tests still green).

**Step 5: Commit**

```bash
git add clients/game/core/src/cell.rs
git commit -m "feat(game): Ink::Rgb and the wire-to-ink gate (NO_COLOR honoured)"
```

---

### Task 2: `term.rs` renders ink

**TDD scenario:** Modifying tested code — the terminal backend has no in-process test harness; verify by the crate's existing suite plus a manual pty smoke (below).

**Files:**
- Modify: `bin/src/term.rs` (`Term::draw`, ~line 105)

**Step 1: Extend the draw loop**

Track current foreground alongside current weight, emitting escapes only on change (same run-length discipline the weight attribute already uses):

```rust
let mut current_weight = Weight::Normal;
let mut current_ink = Ink::Plain;
// ...
let weight = cell.map(|c| c.weight).unwrap_or_default();
let ink = cell.map(|c| c.ink).unwrap_or_default();
if ink != current_ink {
    match ink {
        Ink::Plain => queue!(out, SetForegroundColor(Color::Reset))?,
        Ink::Rgb([r, g, b]) => queue!(out, SetForegroundColor(Color::Rgb { r, g, b }))?,
    }
    current_ink = ink;
}
```

(`Color::Reset` restores the terminal's default foreground — "uncoloured", never black.) Import `crossterm::style::{Color, SetForegroundColor}`. Reset both `current_ink` and attributes at the loop's trailing `Attribute::Reset` (queue a final `SetForegroundColor(Color::Reset)`).

Truecolor is emitted unconditionally — the producer's documented rationale: *"a terminal that does not understand truecolor degrades to an uncoloured glyph rather than to a wrong one."* No capability sniffing, no quantizer.

**Step 2: Verify**

Run: `cargo test -p hornvale-game` (bin crate's tests) then `make game-check`.
Expected: PASS. Then a manual smoke: `cargo run -p hornvale-game -- possess --seed 42` in a truecolor terminal shows tinted cells; `NO_COLOR=1 cargo run -p hornvale-game -- possess --seed 42` is visually identical to pre-Chroma main. Record both observations in the task report.

**Step 3: Commit**

```bash
git add clients/game/bin/src/term.rs
git commit -m "feat(game): term renders Ink as truecolor foreground, Reset for Plain"
```

---

### Task 3: Plan pane takes palette colors

**TDD scenario:** Modifying tested code — run existing plan tests first; they must stay green unchanged (they construct expected cells via `Cell::glyph`, i.e. Plain).

**Files:**
- Modify: `core/src/plan.rs` (`draw`, `draw_mark`, ~lines 137–180)
- Test: in-module tests in `core/src/plan.rs`

**Step 1: Write the failing tests**

```rust
/// A palette entry's colour rides its glyph; an entry claiming no colour
/// draws Plain. Fixture-driven: real seed-42 palette values.
#[test]
fn palette_colour_reaches_the_cell() {
    // Build a minimal Plan whose palette[0] claims [8, 8, 0]; assert the
    // drawn cell's ink is Rgb([8, 8, 0]) — construct the Plan struct
    // literally (extent/palette/cells/you/marks) rather than parsing JSON.
}

/// The you-mark is never tinted, even standing on a coloured cell —
/// identity belongs to glyph (ledger #4; mirrors producer tint()).
#[test]
fn the_you_mark_stays_plain_over_a_coloured_cell() { /* ... */ }

/// Marks re-draw their cell's glyph untinted, same rule.
#[test]
fn marks_draw_untinted() { /* ... */ }
```

**Step 2: Run to verify failure**

Run: `cargo test -p hornvale-game-core plan`
Expected: FAIL — cells carry `Ink::Plain`.

**Step 3: Implement**

In `draw`'s cells pass, replace `Cell::glyph(glyph, Weight::Normal, Source::Plan)` with a lookup that resolves the entry once:

```rust
fn cell_entry(plan: &Plan, i: usize) -> Option<&PaletteEntry> {
    let ix = *plan.cells.get(i)?;
    plan.palette.get(ix as usize)
}
```

then `Cell::inked(glyph, Weight::Normal, Source::Plan, entry.color)`. The `you` pass and `draw_mark` keep `Cell::glyph` (Plain) — that IS the withholding rule; state it in a comment citing spec §2.2.

**Step 4: Run to verify pass**

Run: `cargo test -p hornvale-game-core`
Expected: PASS — including the golden shape test against the sim's `map` verb output (unchanged: glyphs don't move).

**Step 5: Commit**

```bash
git add clients/game/core/src/plan.rs
git commit -m "feat(game): plan pane tints cells from palette colours, marks stay plain"
```

---

### Task 4: Chart pane takes scene colors

**TDD scenario:** Modifying tested code — same shape as Task 3.

**Files:**
- Modify: `core/src/chart.rs` (`draw`, ~line 274)
- Test: in-module tests in `core/src/chart.rs`

**Step 1: Write the failing test**

```rust
/// A chart cell's colour rides its glyph; a cell without a colour key
/// (scene built uncolored — observer declined sight) draws Plain.
#[test]
fn chart_cell_colour_reaches_the_cell() { /* build a Chart literal with one colored cell */ }
```

Also extend any fixture-parsing test to assert at least one drawn cell carries `Ink::Rgb` from `session-seed-42-turn-0.json` (its 31 cells all carry color — verified: `[36, 36, 1]` et al.).

**Step 2: Run to verify failure**

Run: `cargo test -p hornvale-game-core chart`
Expected: FAIL.

**Step 3: Implement**

In `draw`, `boxes_of(chart)` yields `(pos, &SurroundsCell)`-shaped items — resolve `Cell::inked(glyph_of(&cell.state), weight_of(&cell.state), Source::Chart, cell.color)`. Note `schema::ChartCell` already mirrors `color` (unread until now); no schema change needed here.

**Step 4: Verify + commit**

Run: `cargo test -p hornvale-game-core && make game-check`. Expected: PASS.

```bash
git add clients/game/core/src/chart.rs
git commit -m "feat(game): chart pane tints cells from scene colours"
```

---

### Task 5: The sight-disclosure caption

**TDD scenario:** New feature — full TDD cycle.

**Files:**
- Modify: `core/src/schema.rs` (add a `Sight` mirror — the block exists on the wire, verified in `session-seed-42-turn-0.json`: observer/channels/chromatic/projection/preserves/channel_roles/projection_slots/projection_norms/sun_altitude_deg; mirror only fields the caption reads plus enough to be a faithful record — follow the module doc's channel-vs-field rule)
- Modify: `core/src/chart.rs` (a pure `disclosure(sight: &Sight) -> String`)
- Modify: wherever the map strip row is composed (`core/src/lib.rs` compose path / `strip.rs`) to draw the caption when the chart band is active AND any chart cell carries ink; attribute `Source::Chart`
- Test: in-module

**Step 1: Write failing tests**

- `disclosure` output names the observer species, the projection, and what is NOT preserved, e.g. for the seed-42 fixture: something like `"seen through a bugbear's eyes — yellow-blue projection; the red-green axis is not carried"`. Assert on substrings, not the exact sentence (wording is product voice; Nathan reviews rendered output).
- With `NO_COLOR` set, the caption is suppressed entirely (ledger #6: a fidelity claim above a monochrome render is dishonest).
- An uncolored chart (no sight block / no colored cells) produces no caption.

**Step 2–4:** Red → implement `disclosure` as a pure string function (no env read inside it; the caller gates on `NO_COLOR`/ink presence) → green.

**Step 5: Commit**

```bash
git add clients/game/core/src/schema.rs clients/game/core/src/chart.rs clients/game/core/src/lib.rs
git commit -m "feat(game): sight-disclosure caption, suppressed under NO_COLOR"
```

---

### Task 6: Monochrome floor acceptance + full gate

**TDD scenario:** Acceptance — the campaign's pinning test.

**Files:**
- Test: `core/tests/monochrome_floor.rs` (new integration test behind the crate's `tests/suite.rs` roster — check how existing integration files register)

**Step 1: Write the acceptance test**

Render the full composed screen twice from each committed fixture — once normally, once with `NO_COLOR=1` — and assert the two grids are identical AND equal to expectations built entirely from `Cell::glyph` (Plain) constructors. This pins: colour may fail, and when it fails nothing else moves.

**Step 2: Run the whole client gate**

Run: `make game-check`
Expected: PASS, including the wasm/build drift checks.

**Step 3: Manual visual pass** (The Idioms' lesson: shader/render failures can sit invisible to green tests)

Screenshot or capture `possess --seed 42` in both bands (walk + chamber), day and night turns, with and without `NO_COLOR`. Confirm: walls/floors tinted, `@` plain, chart tinted through the bugbear's dichromat eye, caption present and honest. Attach captures to the task report.

**Step 4: Commit**

```bash
git add clients/game/core/tests/
git commit -m "test(game): monochrome-floor acceptance test"
```

---

## Checkpoint

After Task 6: submit `make sluice-stage BRANCH=campaign/the-chroma REF=<full-sha>` per `submitting-to-the-sluice` (durable before doorbell). Stage-boundary absorption cadence applies at every subsequent stage boundary.
