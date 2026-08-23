# The Portolan, part II — the world map: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

> **SUPERSEDED (2026-08-23)** by
> `docs/superpowers/plans/2026-08-23-the-portolan-world-map.md`.
>
> This plan was written before the discovery layer existed. Its five tasks
> are substantially correct and are carried forward as Tasks 1-4 and 6 of the
> replacement; what it cannot know is Amendment 1 — the discovery layer
> (a new Task 5), colour (The Chroma landed the day after it was written),
> the `Focus`/zoom-key restatement, and H1's restatement as H1'.
> **Execute the replacement, not this file.**

**Goal:** A Mercator chart of the whole planet in the terminal client's plate — scrolled, zoomed, and pointed at by the cursor the campaign already built.

**Architecture:** `clients/game/bin` renders the world plate into a `Grid` and hands it to `core`, which composes it exactly as it composes the chart. `core` keeps its no-hornvale-crate purity and the session schema gains nothing — a world view is a **lens**, not a band. The projection is **oblique**: its central line is derived from the world's own physics, so a tidally locked world holds its terminator true instead of an equator nobody lives on.

**Tech Stack:** Rust 2024, `crossterm 0.29`. `clients/game` is **outside** the cargo workspace with its own toolchain. No new dependency.

**Spec:** `docs/superpowers/specs/2026-08-20-the-portolan-world-map-design.md`
**Predecessor:** `docs/superpowers/specs/2026-08-19-the-portolan-design.md` (parts I's mechanism: look mode, cursor, strip, salience, index, resolution — all shipped and reviewed)

## Global Constraints

- **`hornvale-game-core` depends on NO hornvale crate.** It is a pure `vessel/session/v2` → grid renderer. Do not add one.
- **No schema change.** `Spatial` gains no variant; `vessel/session/v2` must not move. **If a task believes it must, that is a STOP** — a cross-repo contract, additive-or-versioned only.
- **80×24 is the floor.** `render` refuses smaller rather than degrading. If the world plate does not fit at the floor, **the plate changes, not the floor.**
- **Ornament may never occupy a cell that carries information.** The cursor is the terminal's hardware cursor and occupies no grid cell.
- **No second projection.** Reuse the spike's; move it if both callers need it. Two copies of a projection is how a chart and a cursor come to disagree — this campaign has already fixed that exact defect once (`chart.rs`'s `boxes_of`).
- **No new dependency**, no new `unsafe`, `#![warn(missing_docs)]`, no `Instant`/wall-clock anywhere.
- **`make gate-commit` compiles NONE of `clients/game`.** Most of this plan lives there. Run the client's own checks explicitly and paste them:
  `cd clients/game && cargo test -p hornvale-game-core -p hornvale-game`, `cargo clippy --all-targets -- -D warnings`, `cargo fmt --check`.
- **Run the suite ONCE, inspect many.** Capture to a file and grep it.
- **A test fixed at one configuration is blind to every defect that depends on it.** This campaign has hit that twice — a test pinned at the 24-row floor missed a height bug, and a spinning-world fixture cannot see H4 at all. Vary the thing under test.

---

### Task 1: The oblique projection and the central line

**Files:**
- Create: `clients/game/bin/src/mercator.rs`
- Modify: `clients/game/bin/src/lib.rs` (add `pub mod mercator;`)
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `pub struct Frame { /* the rotation taking world coords into projection coords */ }`
  - `pub fn frame_for(locked: bool) -> Frame`
  - `pub fn project(frame: &Frame, lat_deg: f64, lon_deg: f64, w: u32, h: u32) -> Option<(u32, u32)>` — `None` above the clamp.
  - `pub fn unproject(frame: &Frame, col: u32, row: u32, w: u32, h: u32) -> (f64, f64)`
  - `pub const LAT_CLAMP_DEG: f64 = 85.0;`

**Why `bin` and not the sim:** a projection is *rendering*, and decision 0022 puts rendering in the client. `bin` already depends on `hornvale-astronomy`, so it can read the rotation regime itself. The sim gains nothing.

- [ ] **Step 1: Read the spike before writing anything**

```bash
sed -n '1,110p' windows/worldgen/examples/portolan_spike.rs
```

It already has `mercator_y`, `mercator_y_max`, `height_for_width`, `project`, **and `unproject`** — the inverse the cursor needs — plus `GLYPH_ASPECT = 2.0` (terminal characters are about twice as tall as wide). It routes through `hornvale_kernel::math`'s `ln`/`tan`, the libm-backed path, so it is cross-platform deterministic. **Port it; do not rewrite it.** Say in your report what you changed and why.

- [ ] **Step 2: Derive the central line, and check whether it is transcendental-free**

Spec §3.1 fixes the requirement and deliberately not the matrix:

- **spinning** → the projection's pole is the geographic pole (identity).
- **locked** → the projection's pole is the **substellar point**.

The reasoning to verify, not inherit: `kernel/src/geosphere.rs:235-236` gives `latitude = asin(z)`, `longitude = atan2(y, x)`, so `+x` is exactly (0°, 0°); `domains/climate/src/temperature.rs` puts a locked world's substellar point at `+x`. A great circle's pole is 90° from every point on it, so the terminator's pole *is* the substellar point.

**Therefore the locked frame should be an exact axis swap taking `+x` to `+z`, needing no `sin`/`cos` at all.** Derive it, confirm or refute that, and **state which in your report** — a rotation that needs no transcendentals is one fewer cross-platform surface. If it is not exact, say so rather than forcing it.

- [ ] **Step 3: Write the failing tests**

```rust
    /// The spinning frame is the identity: a spinning world holds its
    /// geographic equator true, which is where its climate band is.
    #[test]
    fn the_spinning_frame_holds_the_geographic_equator() {
        let f = frame_for(false);
        let (lat, _lon) = unproject(&f, 0, 0, 80, 40);
        // Row 0 is the top of the drawn area: the clamp latitude, not a pole.
        assert!((lat - LAT_CLAMP_DEG).abs() < 1e-9, "got {lat}");
        // A point on the geographic equator projects to the vertical centre.
        let (_c, r) = project(&f, 0.0, 0.0, 80, 40).expect("the equator is drawn");
        assert_eq!(r, 20, "the equator must land on the central row");
    }

    /// THE POINT OF THE WHOLE SECTION. On a locked world the habitable band
    /// is the terminator — the great circle 90 degrees from the substellar
    /// point, running pole to pole. It must land on the central row, and the
    /// substellar point itself must fall in the clamp.
    #[test]
    fn the_locked_frame_holds_the_terminator_and_clamps_the_substellar_point() {
        let f = frame_for(true);
        // The terminator passes through the geographic poles at lon +/-90.
        for (lat, lon) in [(0.0, 90.0), (0.0, -90.0), (89.0, 0.0), (-89.0, 0.0)] {
            let (_c, r) = project(&f, lat, lon, 80, 40)
                .unwrap_or_else(|| panic!("terminator point ({lat},{lon}) was clipped"));
            assert_eq!(r, 20, "terminator point ({lat},{lon}) left the central row");
        }
        // The substellar point is the projection's pole: clipped, by design.
        assert!(project(&f, 0.0, 0.0, 80, 40).is_none(), "the substellar point must clamp");
    }

    /// project and unproject are inverses within a character's resolution.
    /// Checked across the whole drawn area, both frames — not sampled.
    #[test]
    fn project_and_unproject_round_trip_in_both_frames() {
        for locked in [false, true] {
            let f = frame_for(locked);
            for row in 0..40u32 {
                for col in 0..80u32 {
                    let (lat, lon) = unproject(&f, col, row, 80, 40);
                    let (c2, r2) = project(&f, lat, lon, 80, 40)
                        .expect("a point taken from the drawn area must project back into it");
                    assert_eq!((c2, r2), (col, row), "locked={locked} at ({col},{row})");
                }
            }
        }
    }
```

- [ ] **Step 4: Run to verify they fail, then implement, then run again**

```bash
cd clients/game && cargo test -p hornvale-game --lib mercator:: > /tmp/hv-w1.log 2>&1; echo "exit=$?"
grep -E "^test result|cannot find|error\[" /tmp/hv-w1.log | head
```

- [ ] **Step 5: Measure the zoom ladder (F1)**

§4.3 *estimates* ~364 columns at the ceiling from ~110 km cells. **Measure it instead.** Build seed 42, count the terrain cells along the equator (or derive it from `geo.cell_count()` and state the derivation), and report the real ladder: how many columns the whole planet needs at one character per terrain cell, and the rungs between that and 40.

**Every zoom constant in Task 3 comes from this number, not from the spec's estimate.**

- [ ] **Step 6: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git add clients/game/bin/src/mercator.rs
git commit -m "feat(game): an oblique Mercator whose central line comes from the world" -- clients/game/
```

---

### Task 2: The world plate

**Files:**
- Create: `clients/game/bin/src/worldplate.rs`
- Modify: `clients/game/core/src/spread.rs` (accept a caller-supplied plate)
- Modify: `clients/game/core/src/lib.rs` (`render_with`'s signature)
- Modify: `clients/game/bin/src/main.rs`, `driver.rs`
- Test: in-module `#[cfg(test)]` both sides

**Interfaces:**
- Consumes: `Frame`, `project`, `frame_for` (Task 1); `Cursor`, `render_with` (part I Task 2).
- Produces:
  - `pub fn draw_world(terrain: &GeneratedTerrain, frame: &Frame, w: u16, h: u16) -> Grid` (in `bin`)
  - `render_with` gains a `world_plate: Option<&Grid>` parameter which, when present, **replaces** the `Spatial` dispatch.

- [ ] **Step 1: Write the failing tests**

```rust
    /// A caller-supplied plate REPLACES the band dispatch. The world view is
    /// a lens, not a band — `Spatial` gains no variant and the snapshot is
    /// untouched.
    #[test]
    fn a_supplied_world_plate_replaces_the_band_plate() {
        let json = WALK_FIXTURE;
        let mut world = Grid::new(crate::spread::PLATE_WIDTH, 20);
        world.set(0, 0, Cell::glyph('#', Weight::Normal, Source::Look));
        let (with, _) = render_with(json, 80, 24, None, None, Some(&world)).expect("renders");
        assert_eq!(with.get(0, 0).and_then(|c| c.glyph), Some('#'),
                   "the supplied plate did not reach the page");
    }

    /// F4: the world plate fits the inherited floor, and the floor does not
    /// move to accommodate it.
    #[test]
    fn the_world_plate_fits_eighty_by_twentyfour_and_the_floor_holds() {
        let world = Grid::new(crate::spread::PLATE_WIDTH, 20);
        assert!(render_with(WALK_FIXTURE, 80, 24, None, None, Some(&world)).is_ok());
        assert!(matches!(
            render_with(WALK_FIXTURE, 79, 24, None, None, Some(&world)),
            Err(crate::Error::TooSmall { .. })
        ), "79 columns must still be refused");
    }
```

And in `bin`, over a real seed-42 terrain:

```rust
    /// H1: the planet is legible at the floor — land and water are both
    /// present and neither swamps the other. A 40-column planet that is all
    /// one glyph is not a view.
    #[test]
    fn the_planet_is_legible_at_forty_columns() {
        let (terrain, _geo) = seed_42_terrain();
        let grid = draw_world(&terrain, &frame_for(false), 40, 20);
        let mut land = 0usize;
        let mut water = 0usize;
        for y in 0..20 { for x in 0..40 {
            match grid.get(x, y).and_then(|c| c.glyph) {
                Some('~') => water += 1,
                Some(_) => land += 1,
                None => {}
            }
        }}
        assert!(land >= 40, "only {land} land cells drawn — the planet is unreadable");
        assert!(water >= 40, "only {water} water cells drawn");
    }
```

- [ ] **Step 2: Choose the `Source` (F2) and say why**

A caller-supplied plate is content `core` cannot verify — the same category `Source::Look` established, whose doc already says its honesty is a caller discipline. Choose the variant and document it in that same register.

**`Chrome` is wrong**: the plate is world-derived, and `Chrome` means *declared inert*. If you reach for it, re-read `cell.rs`'s `Source` doc first.

- [ ] **Step 3: Run, implement, run**

```bash
cd clients/game && cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-w2.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED" /tmp/hv-w2.log
```

- [ ] **Step 4: Report H1 with the picture, not just the counts**

Paste the actual 40×20 rendered planet into your report. **A human must be able to look at it and see a world.** If it reads as noise, H1 is falsified and that is the finding — say so; do not raise the minimum zoom to make it look better.

- [ ] **Step 5: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git add clients/game/bin/src/worldplate.rs
git commit -m "feat(game): the world plate, composed as a lens rather than a band" -- clients/game/
```

---

### Task 3: Zoom, scroll, and re-centre

**Files:**
- Modify: `clients/game/bin/src/driver.rs`, `input.rs`, `main.rs`
- Test: in-module and `clients/game/bin/tests/driver.rs`

**Interfaces:**
- Consumes: everything above; `Mode`/`Action` (part I Task 1).
- Produces: zoom level and scroll offset on `Driver`; `Action::ZoomIn`/`ZoomOut`/`Recentre`.

- [ ] **Step 1: Bind the keys**

`+`/`-` for zoom and `c` for re-centre are **candidates, not decisions** — the taken set is `? . < > 1-9 b h j k l m n Q u y x`. **Verify each is free before binding it**, and report what you chose. Look mode's own bindings are `hjkl`/arrows/`Esc`.

- [ ] **Step 2: Scroll follows the cursor (§4.2)**

No new keys. When the cursor reaches an edge, the **window** moves and the cursor stays. Longitude **wraps**; latitude stops at the clamp.

- [ ] **Step 3: Write the failing tests**

```rust
    /// H3, AND THE DEFECT CLASS THIS CAMPAIGN HAS HIT TWICE. After scrolling,
    /// the cell under the cursor must be the cell the plate draws there.
    /// Both prior instances were "a wrong name indistinguishable from a right
    /// one", and both came from an offset the resolver did not model. Test at
    /// several offsets, not one.
    #[test]
    fn the_cursor_resolves_what_the_plate_draws_at_every_scroll_offset() {
        // for each of several offsets: the glyph the plate draws at the
        // cursor must come from the same cell the resolver names.
    }

    /// H2: pointing at a feature, zooming in, and re-pointing at it resolves
    /// the same feature.
    #[test]
    fn a_feature_resolves_the_same_across_zoom_levels() { }

    /// H4: a locked world's habitable band is drawn, not clamped. A
    /// spinning-world fixture is STRUCTURALLY BLIND to this.
    #[test]
    fn a_locked_worlds_terminator_band_is_drawn() {
        // Build with RotationPin::Locked. Confirm cells along the terminator
        // (lon +/-90) are drawn, and that the substellar point is clamped.
    }
```

**Fill these in — the bodies are yours.** The doc comments state the property; write the code that would fail without it. `domains/astronomy`'s `RotationPin::Locked` is how you get a locked world; find how the existing tests pin it rather than inventing a path.

- [ ] **Step 4: Longitude wrap is a seam — test it**

Scrolling east past 180° must arrive at −180° with no discontinuity in what the cursor resolves. A wrap that is off by one column is exactly the "wrong name looks right" shape.

- [ ] **Step 5: F5 — what does a coarse character resolve to?**

At minimum zoom one character covers ~9 terrain cells. **State which one the cursor names and why that is honest rather than arbitrary** — the centre of the character's footprint is defensible; "whichever the projection rounds to" is not, unless you say so.

- [ ] **Step 6: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git commit -m "feat(game): zoom, cursor-driven scroll, and an explicit re-centre" -- clients/game/
```

---

### Task 4: The strip carries the chain, and scrolls

**Files:**
- Modify: `clients/game/core/src/strip.rs`, `clients/game/bin/src/driver.rs`
- Test: in-module

- [ ] **Step 1: The chain, most specific first**

```
Vngashngatva (a volcano), on Kxsokxkxzhakx (a landmass)
```

Every feature in the containment chain, ordered by declared salience (part I §3.2), each with its class named in prose. **The one-name cut from part I is withdrawn** — Nathan reversed it.

- [ ] **Step 2: F3 — scrolling must not introduce a clock**

The content may exceed the strip's width and scroll. **It must be driven by the existing redraw**, not a timer. `Instant` is banned workspace-wide and `clients/game` has no animation loop.

**If it needs a timer, STOP and report** — adding an animation loop is a design decision, not an implementation detail. A defensible alternative that needs no clock: advance the scroll on each keypress, so it moves when the player does.

- [ ] **Step 3: Write the failing tests**

```rust
    /// Content longer than the strip is not truncated and not wrapped: it
    /// scrolls. Truncation loses the container; wrapping steals a row from
    /// the plate.
    #[test]
    fn overlong_chain_text_scrolls_rather_than_truncating_or_wrapping() { }

    /// The chain is ordered most-specific-first, so the cursor names the
    /// volcano before the continent it stands on.
    #[test]
    fn the_chain_reads_most_specific_first() { }
```

- [ ] **Step 4: Commit**

```bash
cd clients/game && cargo fmt && cargo clippy --all-targets -- -D warnings
cd ../.. && git commit -m "feat(game): the strip carries the whole chain and scrolls" -- clients/game/
```

---

### Task 5: Close the whole campaign

This closes **parts I and II together** — part I's Task 5 was never run.

- [ ] **Step 1: Delete all three spike instruments**

```bash
git rm windows/worldgen/examples/portolan_spike.rs \
       windows/worldgen/examples/portolan_resolution_spike.rs \
       windows/worldgen/examples/portolan_measure.rs
```

- [ ] **Step 2: Absorb main and regenerate — and do this at every stage boundary, not only here**

The Gazetteer ran ten tasks 71 commits behind and found `docs/audits/type-audit-report.md` had auto-merged **cleanly and wrong**:

```bash
git merge origin/main
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

**Never infer freshness from a clean merge.**

- [ ] **Step 3: Chronicle, decision, registry rows**

The chronicle must carry:
- **The first spike was refuted by the second** — a measurement inherits its question's framing.
- **The walk band is ~3.92 km against ~110 km feature spacing**, which is why part I shipped a mechanism with nothing to point at. State the derivation (`distance_rad` × radius); an earlier draft had this wrong by two orders of magnitude.
- **The projection's central line comes from the world's physics** — and that a locked world's habitable band is the terminator, which a geographic Mercator would have clamped away.
- **Three preregistered hypotheses across two campaigns could not have failed** (the Gazetteer's H2, part I's H1, part I's H2 as first frozen). All three were caught by asking "name the defect this would catch," then mutating.

Decision record for spec §11, numbered **contiguously from main at the time of writing**. Check `make board` first.

- [ ] **Step 4: Retrospective — write it LAST**

After the final whole-branch review, not before. The Gazetteer's was authored at its close task and under-reported itself by three findings, costing a second merge to repair.

- [ ] **Step 5: Stage, then merge**

```bash
git push -u origin campaign/the-portolan
make sluice-stage BRANCH=campaign/the-portolan REF=$(git rev-parse HEAD)
```

The merge is a **hard stop for Nathan** and needs a `Sluice-Headline:` trailer sharing the `Claude-Session` trailer block with **no blank line between them**.
