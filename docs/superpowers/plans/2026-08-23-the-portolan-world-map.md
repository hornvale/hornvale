# The Portolan, part II — the world map and the discovery layer: Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A Mercator chart of the whole planet in the terminal client's plate — complete in its geometry, scrolled and zoomed, pointed at by the cursor part I already built — on which a feature's NAME appears only once the possession has encountered it.

**Architecture:** `clients/game/bin` renders the world plate into a `Grid` and hands it to `core`, which composes it exactly as it composes the chart. `core` keeps its no-hornvale-crate purity and the session schema gains nothing — a world view is a **lens**, not a band. The projection is **oblique**: its central line derives from the world's own rotation regime, so a tidally locked world holds its terminator true. Discovery is two independent mechanisms that are never wired together: cell **visitedness** (upward-propagating, the shipped `purview.rs` predicate) and feature **discovery** (per-feature, by encounter).

**Tech Stack:** Rust 2024, `crossterm 0.29`. `clients/game` is **outside** the cargo workspace with its own toolchain. No new dependency.

**Spec:** `docs/superpowers/specs/2026-08-20-the-portolan-world-map-design.md` — body **and Amendment 1**. Read both; the amendment changes what is *labelled*, not what is *drawn*, and §A4 is the heart of Task 5.

**Supersedes:** `docs/superpowers/plans/2026-08-20-the-portolan-world-map.md` (same campaign, written before the discovery layer existed).

## Global Constraints

- **`hornvale-game-core` depends on NO hornvale crate.** It is a pure `vessel/session/v2` → grid renderer. Its `Cargo.toml` says "DELIBERATELY EMPTY OF HORNVALE CRATES". Do not add one.
- **No schema change.** `Spatial` gains no variant; `vessel/session/v2` must not move. **If a task believes it must, that is a STOP** — a cross-repo contract, additive-or-versioned only.
- **80×24 is a DEGRADATION FLOOR, NOT THE DESIGN TARGET** (Nathan, 2026-08-23): *"we should suffer an 80x25 screen but by no means optimize with that in mind."* `render_with` still refuses anything smaller, and the plate still changes rather than the floor — but the size to design and judge against is a terminal filling a 1080p-or-larger screen, **~200×50 or more**. A criterion met only at 80×24, or a design choice that makes sense only there, is aimed at the wrong configuration.
- **The plate does not currently grow with the terminal, and this campaign fixes that for the world view.** `spread.rs` has `pub const PLATE_WIDTH: u16 = 40` and `plate_width = PLATE_WIDTH.min(w)` — width only ever *shrinks*, while `content_height(h) = h - 4` grows without bound. On a 210×56 terminal the plate is 40×52: a Mercator, whose natural shape is width about 2x height, handed the one axis it cannot use. **Task 3 expands the plate to the terminal's width while map focus is active** (Nathan's ruling); the walk chart and chamber plan keep the two-pane split untouched.
- **Ornament may never occupy a cell that carries information.** The cursor is the terminal's hardware cursor and occupies no grid cell.
- **No second projection.** Reuse the spike's (`windows/worldgen/examples/portolan_spike.rs`); move it if both callers need it. Two copies of a projection is how a chart and a cursor come to disagree — this campaign has already fixed that exact defect once (`chart.rs`'s `boxes_of`).
- **CO-LOCATION IS NOT DISCOVERY** (spec §A4, §A7). Visitedness is a fact about cells and may never be wired to a feature's label.
- **No third copy of the delve roster** (spec §A7). `hornvale_terrain::delve::DelveRung` owns the ladder and derivation; `hornvale_climate::underworld::DelveZone` mirrors the roster; `cli/tests/suite/delve_roster_mirror.rs` guards the pair (decision 0094). **A third mirror is a STOP.**
- **Colour: monochrome is the floor.** Every coloured render must degrade to byte-identical monochrome output under `NO_COLOR`. Reuse `Ink::from_wire`/`Ink::resolve` and `Cell::inked`; do not build a second colour path. **Colour may not carry the epistemic channel** — decision 0142 assigns epistemic to WEIGHT.
- **No new dependency**, no new `unsafe`, `#![warn(missing_docs)]`, no `Instant`/`SystemTime`/wall-clock anywhere.
- **`make gate-commit` compiles NONE of `clients/game`.** Most of this plan lives there. Run the client's own checks explicitly and paste them:
  `cd clients/game && cargo test -p hornvale-game-core -p hornvale-game`, `cargo clippy --all-targets -- -D warnings`, `cargo fmt --check`.
- **Run the suite ONCE, inspect many.** Capture to a file and grep it. Never re-run to grep a second line.
- **Test helpers the later tasks assume do not exist yet — build them, do not hallucinate them.** There is no `test_driver()` and no `test_world()` anywhere in `clients/game`. A driver is constructed as `Driver::start(42, hornvale_vessel::PossessTarget::Flagship).unwrap()` (see `clients/game/bin/tests/driver.rs`). `hornvale-game-core`'s tests are separate files (`cell.rs`, `chart.rs`, `plan.rs`, `schema.rs`, `spread.rs`, `provenance.rs`, `monochrome_floor.rs`) — **this crate has no `tests/suite.rs`**, because the workspace's test-binary consolidation does not reach `clients/game`. Committed fixtures are pulled in with `include_str!("fixtures/session-seed-42-turn-0.json")` (walk band) and `…-chamber.json` (chamber band).
- **A test fixed at one configuration is blind to every defect that depends on it.** This campaign has hit that twice already — a test pinned at the 24-row floor missed a height bug, and a spinning-world fixture cannot see H4 at all. Vary the thing under test.

---

## File Structure

| file | responsibility |
|---|---|
| `clients/game/bin/src/mercator.rs` | **create** — the oblique Mercator: `Frame`, `frame_for`, `project`, `unproject`, the clamp. Pure math, no rendering. |
| `clients/game/bin/src/plate.rs` | **create** — renders a `Frame` + terrain + a zoom/scroll window into a `Grid`. Owns glyph and colour choice. |
| `clients/game/bin/src/discovery.rs` | **create** — visitedness (cells) and discovery (features), as two separate types that never call each other. |
| `clients/game/core/src/lib.rs` | **modify** — `render_with` gains a `world_plate: Option<&Grid>` parameter. |
| `clients/game/core/src/spread.rs` | **modify** — `compose` draws the supplied plate in the plate region when present. |
| `clients/game/core/src/cell.rs` | **modify** — `Source::World`, the world plate's provenance (F2). |
| `clients/game/bin/src/driver.rs` | **modify** — holds the frame, zoom, scroll offset and discovery state; implements `Action::Zoom` at the arm that currently ignores it (line ~548). |
| `clients/game/bin/src/main.rs` | **modify** — passes the plate through to `render_with`. |

---

### Task 1: The oblique projection and the central line

**Files:**
- Create: `clients/game/bin/src/mercator.rs`
- Modify: `clients/game/bin/src/lib.rs` (add `pub mod mercator;`)
- Test: in-module `#[cfg(test)]`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces:
  - `pub struct Frame { pole_lat_deg: f64, pole_lon_deg: f64 }`
  - `pub fn frame_for(locked: bool) -> Frame`
  - `pub fn project(f: &Frame, lat_deg: f64, lon_deg: f64, w: u32, h: u32) -> Option<(u32, u32)>` — **returns `(row, col)`, in that order**, matching the spike's own convention; `None` above the clamp
  - `pub fn unproject(f: &Frame, row: u32, col: u32, w: u32, h: u32) -> (f64, f64)` — **`(row, col)` argument order**, matching `project`'s return
  - `pub const LAT_CLAMP_DEG: f64 = 85.0;`

**Why `bin` and not the sim:** a projection is *rendering*, and decision 0022 puts rendering in the client. `bin` already depends on `hornvale-astronomy`, so it can read the rotation regime itself. The sim gains nothing.

- [ ] **Step 1: Write the failing test for the spinning frame**

```rust
#[test]
fn a_spinning_world_holds_the_geographic_equator() {
    let f = frame_for(false);
    // The projection's pole IS the geographic pole, so the equator is the
    // line held true: lat 0 lands on the vertical centre of the plate.
    // `project` returns (ROW, COL) — the spike's own order; see Interfaces.
    let (row, _col) = project(&f, 0.0, 0.0, 80, 40).expect("the equator is inside the clamp");
    assert_eq!(row, 20, "lat 0 sits on the plate's horizontal midline");
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib mercator > /tmp/hv-t1.log 2>&1; echo "exit=$?"`
Expected: FAIL — `mercator` does not exist (a compile error). **A compile-error red proves nothing about an assertion**, so the behavioural reds below (Steps 6, 8) are the ones that count.

- [ ] **Step 3: Implement `Frame`, `frame_for`, and the Mercator core**

Port `mercator_y`, `mercator_y_max`, `project` and `unproject` from `windows/worldgen/examples/portolan_spike.rs` (lines 51–104). They route through `hornvale_kernel::math::{ln, tan, atan, exp}`, the libm-backed path, so they are cross-platform deterministic — **keep that routing exactly**.

**Port the MATH, not the clamp POLICY — this is the one deliberate divergence.** The spike's `project` clamps latitude and always returns a cell, by design: its doc says a feature past the clamp "still lands at the map's top/bottom edge rather than being dropped", which is right for placing a LABEL. It is wrong for drawing TERRAIN: spec §6 refuses polar fabrication, so a point above the clamp must yield `None` and its cell must be left blank. Keep `mercator_y`'s internal clamp (it is what makes the projection finite); add the out-of-band test that returns `None` before projecting. **If you find yourself making the locked-world test pass by clamping instead, stop — that is the defect this paragraph exists to prevent.**

```rust
/// The rotation taking world coordinates into projection coordinates,
/// expressed as the geographic position of the PROJECTION's north pole.
///
/// A great circle's pole is 90° from every point on it, so choosing which
/// line the projection holds true is the same act as choosing this pole.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Frame {
    /// Geographic latitude of the projection's pole, in degrees.
    pub pole_lat_deg: f64,
    /// Geographic longitude of the projection's pole, in degrees.
    pub pole_lon_deg: f64,
}

/// The frame a world's own physics chooses (spec §3.1).
pub fn frame_for(locked: bool) -> Frame {
    if locked {
        // The habitable band on a locked world is the TERMINATOR, the great
        // circle 90° from the substellar point. Its pole therefore IS the
        // substellar point, which `kernel/src/geosphere.rs` puts at
        // (lat 0, lon 0): `latitude = asin(z)`, `longitude = atan2(y, x)`,
        // so `+x` is exactly (0, 0), matching the convention
        // `domains/astronomy` states twice.
        Frame { pole_lat_deg: 0.0, pole_lon_deg: 0.0 }
    } else {
        Frame { pole_lat_deg: 90.0, pole_lon_deg: 0.0 }
    }
}
```

- [ ] **Step 4: Run the spinning test and confirm it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib mercator > /tmp/hv-t1.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 5: Write the failing test for the locked frame — H4's core**

```rust
#[test]
fn a_locked_world_holds_the_terminator_and_clamps_the_substellar_point() {
    let f = frame_for(true);
    // The terminator runs pole-to-pole through longitudes ±90°. Every point
    // on it must be INSIDE the drawn area.
    for lat in [-80.0, -40.0, 0.0, 40.0, 80.0] {
        assert!(
            project(&f, lat, 90.0, 80, 40).is_some(),
            "the terminator at lat {lat} must be drawn, not clamped"
        );
    }
    // And the substellar point — where nobody lives — is what the clamp eats.
    assert!(
        project(&f, 0.0, 0.0, 80, 40).is_none(),
        "the substellar point is the projection's pole and falls in the clamp"
    );
}
```

- [ ] **Step 6: Run it and confirm it fails BEHAVIOURALLY**

Run: `cd clients/game && cargo test -p hornvale-game --lib mercator > /tmp/hv-t1.log 2>&1; echo "exit=$?"`
Expected: FAIL on the assertion (not on compilation) — `frame_for(true)` returns a `Frame` but `project` still ignores it. **Capture this red; it is the one that proves the rotation is real.**

- [ ] **Step 7: Implement the frame rotation inside `project`/`unproject`**

Rotate the geographic (lat, lon) into the frame before applying Mercator, and back again in `unproject`. **State in the doc comment whether this is an exact axis swap** (spec F1b): if `pole_lat_deg` is exactly 0 or 90 the rotation is a coordinate permutation with no transcendental at all, which is one fewer cross-platform surface. If it is not, say so and route the trig through `hornvale_kernel::math`.

- [ ] **Step 8: Run both tests and confirm they pass**

Run: `cd clients/game && cargo test -p hornvale-game --lib mercator > /tmp/hv-t1.log 2>&1; echo "exit=$?"`
Expected: PASS, both.

- [ ] **Step 9: Write the round-trip property test**

```rust
#[test]
fn unproject_inverts_project_in_both_frames() {
    for locked in [false, true] {
        let f = frame_for(locked);
        for &(lat, lon) in &[(0.0, 0.0), (31.5, -117.25), (-64.0, 88.0), (12.0, 179.0)] {
            if let Some((row, col)) = project(&f, lat, lon, 360, 180) {
                let (rlat, rlon) = unproject(&f, row, col, 360, 180);
                // One cell of tolerance: project quantizes to a character.
                assert!((rlat - lat).abs() < 2.0, "lat {lat} -> {rlat} (locked={locked})");
                assert!(((rlon - lon + 540.0) % 360.0 - 180.0).abs() < 2.0, "lon {lon} -> {rlon}");
            }
        }
    }
}
```

- [ ] **Step 10: Measure the zoom ladder (F1) and report it**

Write a throwaway probe (do NOT commit it) that loads seed 42's terrain at `hornvale_terrain::GLOBE_LEVEL` (the spike does exactly this at line 107) and counts the equatorial cell run, then report in the task report:
- the real equatorial cell count at level 6,
- the resulting ladder: the virtual chart width at "whole planet in 40 columns" and at the one-character-per-terrain-cell ceiling,
- **the actual numbers, not §4.3's ~364 estimate.** Every zoom constant in Task 3 comes from this measurement.

- [ ] **Step 11: Run fmt, clippy and the full client suite**

```bash
cd clients/game
cargo fmt --check && cargo clippy --all-targets -- -D warnings
cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t1-all.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-t1-all.log
```

- [ ] **Step 12: Commit**

```bash
git add clients/game/bin/src/mercator.rs clients/game/bin/src/lib.rs
git commit -m "feat(game): the oblique Mercator, framed by the world's rotation regime"
```

---

### Task 2: The world plate, drawn and composed

**Files:**
- Create: `clients/game/bin/src/plate.rs`
- Modify: `clients/game/core/src/cell.rs` (add `Source::World`)
- Modify: `clients/game/core/src/lib.rs` (`render_with` gains `world_plate`)
- Modify: `clients/game/core/src/spread.rs` (`compose` draws it)
- Modify: `clients/game/bin/src/main.rs`, `clients/game/bin/src/driver.rs`
- Test: in-module `#[cfg(test)]` in `plate.rs`; `clients/game/core/tests/spread.rs` for the compose half; `clients/game/core/tests/monochrome_floor.rs` for the colour floor

**Interfaces:**
- Consumes: `mercator::{Frame, project, unproject}` (Task 1).
- Produces:
  - `pub struct Window { pub zoom: u8, pub origin_col: u32, pub origin_row: u32 }`
  - `pub fn draw(terrain: &GeneratedTerrain, geo: &Geosphere, f: &Frame, win: &Window, w: u16, h: u16) -> Grid`
  - `hornvale_game_core::Source::World`
  - `render_with(json, w, h, focus, map_cursor, line, strip, echo, world_plate: Option<&Grid>)`

- [ ] **Step 1: Write the failing test for `Source::World` (F2)**

```rust
// clients/game/core/src/cell.rs, in-module
#[test]
fn the_world_plate_has_its_own_provenance_and_it_is_not_chrome() {
    // The plate is world-derived, so `Chrome` (declared-inert) would be a
    // false claim. It is also not a snapshot channel — `core` cannot point
    // at a `Snapshot` field that justifies it — so it sits with `Look`,
    // `Typed` and `Echo` in the caller-discipline family.
    let c = Cell::glyph('^', Weight::Normal, Source::World);
    assert_eq!(c.source, Source::World);
    assert_ne!(c.source, Source::Chrome);
}
```

- [ ] **Step 2: Run it and confirm it fails**

Run: `cd clients/game && cargo test -p hornvale-game-core cell > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Expected: FAIL — no `Source::World` variant.

- [ ] **Step 3: Add `Source::World` with the doc that states the discipline**

```rust
    /// The whole-world Mercator plate, rendered by `bin` from
    /// `hornvale-terrain` and handed to [`render_with`] as a `Grid`.
    ///
    /// **Not verifiable by construction**, exactly like [`Source::Look`]:
    /// `Chart`, `Plan`, `Prose` and `Identity` each trace to a field this
    /// crate parsed off `Snapshot`, and this one does not — the planet is
    /// not on `vessel/session/v2` and must not be (a world-terrain channel
    /// would put a planet in every per-turn document). So its honesty is a
    /// **caller discipline**: `bin` must pass a genuinely terrain-derived
    /// plate, never a placeholder.
    ///
    /// **Deliberately not [`Source::Chrome`]**: the plate is world-derived
    /// data, and `Chrome` means declared-inert.
    World,
```

- [ ] **Step 4: Run and confirm it passes**

Run: `cd clients/game && cargo test -p hornvale-game-core cell > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 5: Write the failing test for the plate's geometry at the floor (F4)**

```rust
// clients/game/bin/src/plate.rs, in-module
#[test]
fn the_plate_fits_the_eighty_by_twenty_four_floor_exactly() {
    let (terrain, geo) = test_world();               // seed 42, GLOBE_LEVEL
    let f = crate::mercator::frame_for(false);
    let win = Window { zoom: 0, origin_col: 0, origin_row: 0 };
    // At the floor the plate is 40 columns; its content height is what
    // `spread::content_height(24)` leaves after the strip row.
    let g = draw(&terrain, &geo, &f, &win, 40, 20);
    assert_eq!(g.width(), 40);
    assert_eq!(g.height(), 20);
    assert!(
        g.provenance().get(&Source::World).copied().unwrap_or(0) > 0,
        "the plate must actually draw cells, not return an empty grid"
    );
}
```

- [ ] **Step 6: Run it and confirm it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib plate > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Expected: FAIL — `plate` does not exist.

- [ ] **Step 7: Implement `Window` and `draw`, monochrome first**

For each grid cell, `unproject` to (lat, lon), find the terrain cell, and choose a glyph. Reuse the spike's `glyph_for` (line 171) rather than inventing a second glyph vocabulary. Above the clamp, leave the cell blank — **no polar fabrication** (spec §6).

- [ ] **Step 8: Run and confirm it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib plate > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 9: Write the failing colour test with its `NO_COLOR` twin**

Colour tests in this crate must be hermetic — The Chroma's fix round found threaded tests mutating the environment under each other (`389d498de`). Call `Ink::resolve(color, colour_allowed)` directly rather than touching `NO_COLOR`.

**`clients/game/core/tests/monochrome_floor.rs` already exists and is the model.** It renders each committed fixture twice, once per colour regime, compares cell by cell, and carries a companion assertion walking the COLOURED render for at least one non-`Plain` ink — *"so a world where colour never resolves cannot pass vacuously by producing two identical Plain screens."* It also owns an `ENV_LOCK` and a `with_no_color_removed` helper. **Extend that file's discipline; do not invent a second colour-test idiom.**

```rust
#[test]
fn a_land_cell_tints_when_colour_is_allowed_and_is_plain_when_it_is_not() {
    let (terrain, geo) = test_world();
    let f = crate::mercator::frame_for(false);
    let win = Window { zoom: 0, origin_col: 0, origin_row: 0 };

    let lit = draw_with(&terrain, &geo, &f, &win, 40, 20, true);
    let mono = draw_with(&terrain, &geo, &f, &win, 40, 20, false);

    // Monochrome is the FLOOR: same glyphs, only the ink differs.
    assert_eq!(lit.to_plain_text(), mono.to_plain_text(),
        "colour may not change which glyph is drawn");
    // `Grid` has NO `cells()`/`iter()` — walk it with `get(x, y)`.
    let inks = |g: &Grid| -> Vec<Ink> {
        (0..g.height()).flat_map(|y| (0..g.width()).filter_map(move |x| g.get(x, y).map(|c| c.ink))).collect()
    };
    assert!(inks(&mono).iter().all(|i| *i == Ink::Plain));
    assert!(inks(&lit).iter().any(|i| matches!(i, Ink::Rgb(_))),
        "with colour allowed at least one cell must claim one — this is the VACUITY guard: \
         two identical all-Plain grids would otherwise pass the equality above");
}
```

- [ ] **Step 10: Run it and confirm it fails, then implement the tint**

Run: `cd clients/game && cargo test -p hornvale-game --lib plate > /tmp/hv-t2.log 2>&1; echo "exit=$?"`
Expected: FAIL. Then implement using `Cell::inked(glyph, weight, Source::World, color)` with the colour resolved through `Ink::resolve`. Re-run; expected PASS.

- [ ] **Step 11: Thread `world_plate` through `render_with` and `compose`**

`render_with` already carries an `#[allow(clippy::too_many_arguments)]`; this makes it 9. Extend the existing allow's comment rather than adding a second. In `compose`, when `world_plate` is `Some`, draw it into the plate region **instead of** the chart or plan — the world view is a lens over whichever band the character occupies, and the character stays where they are.

- [ ] **Step 12: Write the compose test**

```rust
// clients/game/core/tests/spread.rs  (compose lives in spread.rs; this crate has
// NO tests/suite.rs — the workspace's test-binary consolidation does not reach
// clients/game, which is outside the cargo workspace)
const WALK: &str = include_str!("fixtures/session-seed-42-turn-0.json");

#[test]
fn a_supplied_world_plate_replaces_the_band_view_and_nothing_else() {
    let plate = { let mut g = Grid::new(40, 20); g.set(0, 0, Cell::glyph('#', Weight::Normal, Source::World)); g };
    let (with, _)    = render_with(WALK, 80, 24, Focus::Map, None, CommandLine::default(), None, None, Some(&plate)).unwrap();
    let (without, _) = render_with(WALK, 80, 24, Focus::Map, None, CommandLine::default(), None, None, None).unwrap();
    assert_eq!(with.get(0, 0).unwrap().source, Source::World);
    assert_eq!(without.get(0, 0).unwrap().source, Source::Chart);
    // The entry pane is untouched by the lens.
    for y in 0..24 { for x in 40..80 {
        assert_eq!(with.get(x, y), without.get(x, y), "the lens must not reach past the plate at ({x},{y})");
    }}
}
```

- [ ] **Step 13: Render at exactly 80×24 and paste it in the task report (F4, H1')**

Run the client with the world plate at the floor and **paste both renderings** — coloured and `NO_COLOR` — into the report, next to the Gazetteer's committed `elevation_ascii` of the same world. H1' is judged on the monochrome arm; say plainly whether the largest landmass is recognisable. **A falsified H1' is a finding, not a failure** — report it and stop rather than widening the floor.

- [ ] **Step 15 (added 2026-08-23, after H1' was falsified): area-majority sampling**

**Nathan's ruling.** H1' failed at 40 columns because the plate point-samples:
800 screen cells over a ~40,962-cell mesh means ~51 terrain cells behind each
character, and taking the single NEAREST one makes a character straddling a
coastline land-or-water essentially at random. The fix is to sample the area, not
the point.

Each screen cell takes the **majority** of the terrain cells its own footprint
covers, rather than the nearest cell to its centre. At the ceiling (one character
per terrain cell) the footprint is 1 and the behaviour is unchanged, so this is a
generalisation of today's code, not a replacement.

**This is NOT forbidden by decision 0123.** 0123 governs a view *finer* than its
model and forbids the field "inventing detail" below the model's resolution. This
is the opposite direction — a view *coarser* than its model, summarising detail it
actually has. Summarising what you have and inventing what you do not are
different acts, and only the second is refused.

**It also resolves the deferred Minor from Task 2's review.** `draw_with` rebuilds
`NearestCellIndex::new(geo)` on every call (~200 ms measured). Point sampling made
that merely wasteful; area sampling multiplies the per-cell work by ~51 and Task 3
wires this into the redraw path, so build the index ONCE and hand it in (the idiom
`driver.rs` already uses for its own `NearestCellIndex`). Change the signature
rather than caching inside.

- [ ] **Step 15a: re-measure, and report BOTH renderings side by side**

Render seed 42 at 40×20 under both samplings and paste both into the report.

- [ ] **Step 15b: state the post-unblinding change explicitly**

**The original H1' result stands and is not overwritten.** It was falsified against
nearest-cell sampling, and that is a real finding about point sampling at this
ratio. What you are measuring now is a *different method*, chosen *after* seeing
that result — so report it as **H1'' (area-majority sampling)**, name it as a
post-unblinding method change, and say who decided it and why. Do not report a
passing H1' as though the original prediction had held. If H1'' also fails, say so
plainly; a second falsification is a finding about the 40-column rung itself and
Task 3's ladder floor becomes the open question.

- [ ] **Step 14: Run fmt, clippy, the full client suite, and commit**

```bash
cd clients/game
cargo fmt --check && cargo clippy --all-targets -- -D warnings
cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t2-all.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-t2-all.log
git add clients/game/bin/src/plate.rs clients/game/core/src/cell.rs \
        clients/game/core/src/lib.rs clients/game/core/src/spread.rs \
        clients/game/core/tests/suite.rs clients/game/bin/src/main.rs clients/game/bin/src/driver.rs
git commit -m "feat(game): the world plate, drawn in bin and composed by core"
```

---

### Task 3: Zoom, scroll, and the explicit re-centre

**Files:**
- Modify: `clients/game/bin/src/driver.rs` (the `Action::Zoom(_) => false` arm, ~line 548)
- Modify: `clients/game/bin/src/plate.rs`
- Test: in-module in both

**Interfaces:**
- Consumes: `plate::{Window, draw}`, `mercator::{Frame, project, unproject}`.
- Produces: `Driver::window(&self) -> &Window`; the `recentre` command.

- [ ] **Step 1: Write the failing zoom test**

```rust
#[test]
fn zoom_keys_move_the_window_and_stop_at_both_ends() {
    let mut d = test_driver();
    d.enter_map();
    let base = d.window().zoom;
    d.apply(Action::Zoom(-1));
    assert_eq!(d.window().zoom, base, "minimum zoom is the whole planet; there is nothing further out");
    d.apply(Action::Zoom(1));
    assert_eq!(d.window().zoom, base + 1);
    for _ in 0..64 { d.apply(Action::Zoom(1)); }
    assert_eq!(d.window().zoom, MAX_ZOOM,
        "maximum zoom is one character per terrain cell (decision 0123: disclose a resolution, never invent detail below it)");
}
```

`MAX_ZOOM` comes from **Task 1 Step 10's measurement**, not from §4.3's estimate.

- [ ] **Step 2: Run it, confirm it fails behaviourally**

Run: `cd clients/game && cargo test -p hornvale-game --lib driver > /tmp/hv-t3.log 2>&1; echo "exit=$?"`
Expected: FAIL — the zoom arm currently returns `false` and changes nothing. This is a real behavioural red.

- [ ] **Step 3: Implement the zoom arm**

Replace `Action::Zoom(_) => false` with a clamping implementation, and update the doc above `apply` that currently says "`Zoom` is accepted and ignored — zoom itself belongs to The Portolan part II, a paused follow-on campaign; this arm is where it will be implemented." **That sentence is now false; rewrite it rather than leaving it.**

- [ ] **Step 4: Run and confirm it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib driver > /tmp/hv-t3.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 5: Write the failing scroll test at MORE THAN ONE offset (H3)**

H3 is the hypothesis at real risk: this campaign has twice shipped "a wrong name indistinguishable from a right one", both times introduced by a fix that changed what a value depended on. **A test that only runs at one scroll offset cannot see it.**

```rust
#[test]
fn the_cell_under_the_cursor_is_the_cell_the_plate_draws_there_at_every_offset() {
    let mut d = test_driver();
    d.enter_map();
    for zoom in [0, 1, 3] {
        for _ in 0..zoom { d.apply(Action::Zoom(1)); }
        for &(dx, dy) in &[(0, 0), (7, 0), (0, 5), (39, 19), (-7, 3)] {
            d.apply(Action::CursorBy(dx, dy));
            let drawn = d.plate_cell_at(d.cursor());       // what the plate rendered there
            let resolved = d.cell_under_cursor();          // what the resolver answers for
            assert_eq!(drawn, resolved,
                "plate and resolver disagree at zoom {zoom}, cursor {:?}", d.cursor());
        }
    }
}
```

- [ ] **Step 6: Run it, confirm it fails, implement cursor-driven scroll**

Scrolling is what happens when the cursor reaches an edge: the window moves, the cursor stays (spec §4.2). Longitude wraps; latitude stops at the clamp. **One source of truth for the screen→cell mapping** — the resolver must call the same function the plate draws from, not a parallel copy.

- [ ] **Step 7: Run and confirm it passes at every offset**

Run: `cd clients/game && cargo test -p hornvale-game --lib driver > /tmp/hv-t3.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 8: Write the failing re-centre test**

**Re-centre is a KEY in map focus — `.` — not a typed command.** The controller
ruled this before dispatch; the reasoning is load-bearing, so it is here rather
than only in the ledger:

- **Letters are not available.** `input.rs`'s `Focus::Map` arm routes
  `KeyCode::Char(c) => Action::FocusAndType(c)` — every printable key that is not
  `-`/`+`/`=` bounces to the CLI and types itself. That totality is The Stride's
  convention, extended to the third focus state deliberately. Carving a letter out
  of it would make the routing table non-total and unpredictable from the screen,
  which decision 0159 exists to prevent.
- **Punctuation is the established exception**: `-`, `+` and `=` are already map
  verbs, so `.` joins a pattern rather than starting one. `Tab` stays reserved
  (`CLIENT-tab-completion`).
- **A typed `recentre` would need a reply channel that does not exist.** The entry
  pane's prose is `narration.prose`, carried from the wire; there is no client-side
  prose channel, and adding one to `vessel/session/v2` is the schema change this
  plan refuses. A key needs no reply: **the acknowledgement is the map redrawing.**
- `.` is mnemonic for the cursor's own point, and §3.2 centres on the CURSOR.

```rust
#[test]
fn recentre_rolls_the_projection_to_the_cursor_and_only_on_command() {
    let mut d = test_driver();          // Driver::start(42, PossessTarget::Flagship)
    d.enter_map();
    let before = *d.frame();
    d.apply(Action::CursorBy(12, 4));
    assert_eq!(*d.frame(), before, "the map holds still while the cursor moves");
    d.apply(Action::Recentre);
    assert_ne!(*d.frame(), before, "an explicit gesture rolls the projection");
}

#[test]
fn the_map_routing_table_stays_total() {
    // Every printable key that is not a map verb must still bounce to the CLI.
    // A new exception that swallowed a letter would be invisible to the test
    // above and would break decision 0159's predictability rule.
    for c in ['a', 'z', 'Q', '9', ' '] {
        assert!(matches!(action_for(press(c), Focus::Map), Action::FocusAndType(_)),
            "{c:?} must still type itself");
    }
    assert!(matches!(action_for(press('.'), Focus::Map), Action::Recentre));
}
```

- [ ] **Step 9: Run, implement, re-run**

Add `Action::Recentre` and bind `KeyCode::Char('.')` in the `Focus::Map` arm of
`input.rs`, beside the existing `-`/`+`/`=` bindings. Re-centring is a gesture,
never a behaviour (spec §3.2): the map would otherwise reflow every keypress, and
`RENDER-three-channels-three-clocks` records that the glyph layer is the most
cacheable thing on screen.

- [ ] **Step 10: Test H4 on a LOCKED world, not seed 42 alone**

```rust
#[test]
fn a_locked_worlds_habitable_band_is_drawn_not_clamped() {
    let d = test_driver_locked();          // --rotation locked
    // Settlements on a locked world sit near the terminator (median 19.9–27.1°
    // from ±90 across seeds 42/7/1337, vs 48.0–51.7° spinning). Every one of
    // them must land inside the drawn area.
    for (lat, lon) in d.settlement_coords() {
        assert!(d.projects_inside_clamp(lat, lon),
            "a settlement at ({lat}, {lon}) fell in the clamp on a locked world");
    }
}
```

- [ ] **Step 12: The map-focused full-width plate (Nathan's ruling, 2026-08-23)**

While `Focus::Map` is active **and a world plate is being drawn**, the plate uses
the terminal's full width instead of `PLATE_WIDTH`. Leaving map focus restores the
two-pane split. The walk chart and chamber plan are **untouched at every focus** —
this is the world view only, justified by the spec's own §11 decision that a world
view is a **lens, not a band**: something consulted, not lived in.

**One width, computed once.** `compose` already takes `focus` and `world_plate`;
derive the plate's width there and expose it the way `content_height` already is,
because `bin`'s cursor resolver needs the SAME number `compose` draws into.
`content_height`'s own doc records why: *"Two callers computing 'the plate's
content height' independently is exactly the shape that let a fixed-height
assumption silently name the wrong cell at any non-floor terminal size."* A second
copy of the width reproduces that defect on the other axis.

**The Mercator's aspect decides the height, not the terminal.** The projection is
naturally square in Mercator units, so at `GLYPH_ASPECT = 2` its natural shape is
width about 2x height. Given `w` columns and `content_height(h)` rows, fit the
largest Mercator that fits BOTH — do not stretch to fill, and do not letterbox
silently without saying so in the report.

```rust
#[test]
fn the_world_plate_uses_the_width_only_while_the_map_is_focused() {
    // Walk focus: the two-pane split is untouched.
    let (walk, _) = render_with(WALK, 210, 56, Focus::Walk, None, CommandLine::default(), None, None, Some(&plate)).unwrap();
    assert!(walk.get(PLATE_WIDTH, 0).is_some_and(|c| c.source != Source::World),
        "the walk view keeps its 40-column plate");

    // Map focus: the plate reaches past PLATE_WIDTH.
    let (map, _) = render_with(WALK, 210, 56, Focus::Map, None, CommandLine::default(), None, None, Some(&plate)).unwrap();
    assert!((PLATE_WIDTH..120).any(|x| map.get(x, 10).is_some_and(|c| c.source == Source::World)),
        "the map view draws world cells past the old fixed plate width");

    // And the floor still works, degraded rather than refused.
    let (floor, _) = render_with(WALK, 80, 24, Focus::Map, None, CommandLine::default(), None, None, Some(&plate)).unwrap();
    assert_eq!(floor.width(), 80, "80x24 degrades, never refuses");
}
```

- [ ] **Step 13: H1''' — re-measure at a REALISTIC terminal, and at the floor**

**F10 — does the expanded plate resolve the landmass?** Render seed 42 with
area-majority sampling at **210x56** (a full-screen 1080p terminal) and paste it,
next to the same world at 80x24. Report:

- whether the largest landmass reads as one coherent shape **at 210x56** — this is
  the criterion that counts, per Nathan's ruling that the floor is not the target;
- whether the 80x24 case remains *honest* (land and water distinguishable, nothing
  fabricated), which is all the floor is required to be;
- the sample count at each size, and the terrain-cells-per-sample ratio.

**Label it H1''' and say what changed since H1''.** This is the THIRD framing of
one hypothesis: H1' (nearest-cell, 40x20) falsified; H1'' (area-majority, 40x20)
measured in Task 2's fix round; H1''' (area-majority, expanded plate). Each change
was made after seeing the previous result, and the chronicle must show the whole
sequence rather than only the framing that finally passed. **If H1''' fails too,
that is a finding about the whole-planet rung and the ladder's floor becomes an
open question for Nathan — do not rescue it.**

- [ ] **Step 11: Run fmt, clippy, the full client suite, and commit**

```bash
cd clients/game
cargo fmt --check && cargo clippy --all-targets -- -D warnings
cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t3-all.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-t3-all.log
git add clients/game/bin/src/driver.rs clients/game/bin/src/plate.rs
git commit -m "feat(game): zoom, cursor-driven scroll, and the explicit re-centre"
```

---

### Task 4: The strip carries the chain and scrolls

**Files:**
- Modify: `clients/game/core/src/strip.rs`
- Modify: `clients/game/bin/src/driver.rs` (`refresh_strip`)
- Test: in-module in both

**Interfaces:**
- Consumes: `plate::Window`, the containment chain from `CellFeatureIndex::at`.
- Produces: `strip::draw(text, into, origin, width, offset: u16)`.

- [ ] **Step 1: Write the failing test for the chain**

```rust
#[test]
fn the_strip_carries_every_member_of_the_chain_most_specific_first() {
    let mut d = test_driver();
    d.enter_map();
    d.point_at_a_volcano_on_a_landmass();
    let s = d.strip().expect("the map focus resolves a strip");
    let volcano = s.find("a volcano").expect("the volcano is named");
    let landmass = s.find("a landmass").expect("the landmass is named");
    assert!(volcano < landmass, "most specific first (part I spec §3.2): got {s:?}");
}
```

The first spec cut the strip to one name because the chain overflowed 40 columns 12.02% of the time. **That cut is withdrawn** (spec §5): H1 measured max name length at 9 characters, and Nathan ruled the strip's content may exceed its width.

- [ ] **Step 2: Run, confirm it fails, implement the chain**

Run: `cd clients/game && cargo test -p hornvale-game-core strip > /tmp/hv-t4.log 2>&1; echo "exit=$?"`

- [ ] **Step 3: Write the failing scroll test — and settle F3 in the same step**

```rust
#[test]
fn a_chain_longer_than_the_plate_scrolls_without_a_clock() {
    let mut g = Grid::new(80, 24);
    let long = "Vngashngatva (a volcano), on Kxsokxkxzhakx (a landmass), in Zherqvadvoshao (a sea)";
    draw(long, &mut g, (0, 21), 40, 0);
    let head: String = row_text(&g, 21).chars().take(40).collect();
    draw(long, &mut g, (0, 21), 40, 12);
    let scrolled: String = row_text(&g, 21).chars().take(40).collect();
    assert_ne!(head, scrolled, "a different offset shows a different window on the text");
    assert!(long.contains(scrolled.trim_end()), "the window is a slice of the text, never a rewrite");
}
```

**F3 is settled by what drives `offset`, and it must not be a timer.** `Instant` and `SystemTime` are banned workspace-wide (decision 0001) and `clients/game` has no animation loop. The available driver is the one `CLIENT-animation-without-a-clock` already records: a counter incremented when `crossterm::event::poll` times out. **State in the task report which driver was used. If it needs a timer, that is a STOP.**

- [ ] **Step 4: Run, implement, re-run**

Run: `cd clients/game && cargo test -p hornvale-game-core strip > /tmp/hv-t4.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 5: Settle F8 — the chain under the discovery gate**

Task 5 gates names. State, in the report, what the strip shows when an **outer** member of the chain is undiscovered and an inner one is not — standing in a named valley inside an unnamed landmass. Write the test for whichever answer you defend; do not leave it implicit.

- [ ] **Step 6: Run fmt, clippy, the full client suite, and commit**

```bash
cd clients/game
cargo fmt --check && cargo clippy --all-targets -- -D warnings
cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t4-all.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-t4-all.log
git add clients/game/core/src/strip.rs clients/game/bin/src/driver.rs
git commit -m "feat(game): the strip carries the whole containment chain, and scrolls"
```

---

### Task 5: Discovery — two mechanisms that are never wired together

**Files:**
- Create: `clients/game/bin/src/discovery.rs`
- Modify: `clients/game/bin/src/plate.rs`, `clients/game/bin/src/driver.rs`
- Test: in-module in `discovery.rs`

**Interfaces:**
- Consumes: `Session::knowledge()` (public, `windows/vessel/src/session.rs:951`), `NearestCellIndex`, `CellFeatureIndex`, `Feature::extent`.
- Produces:
  - `pub struct Visited(BTreeSet<CellId>)` with `pub fn contains_at_rung(&self, cell: CellId, rung: u32) -> bool`
  - `pub struct Discovered(BTreeSet<FeatureId>)` with `pub fn contains(&self, id: FeatureId) -> bool`

**Read spec §A4 before writing a line of this.** The two types must not call each other, and neither may take the other as a parameter. That is the whole design.

- [ ] **Step 1: Write the failing test for visitedness (§A4a)**

```rust
#[test]
fn visitedness_propagates_upward_only() {
    let v = Visited::from_rooms(&["room/1234"], &nearest, &geo);
    let fine = cell_of("room/1234");
    assert!(v.contains_at_rung(fine, 12), "the cell walked is visited");
    assert!(v.contains_at_rung(ancestor_of(fine, 6), 6), "and every coarser cell containing it");
    assert!(!v.contains_at_rung(sibling_of(fine), 12), "but no sibling — zooming in must resolve the TRUE set");
}
```

This is the shipped `purview.rs` predicate (`w.path[..addr.path.len()] == addr.path[..]`, an ancestor test), reused rather than reinvented. Nathan, on F6: *"consider the cell visited but not the village. As zoom levels increase, you should be able to pick out what you have and have not visited accurately."*

- [ ] **Step 2: Run, confirm it fails, implement `Visited`**

Run: `cd clients/game && cargo test -p hornvale-game --lib discovery > /tmp/hv-t5.log 2>&1; echo "exit=$?"`

- [ ] **Step 3: Write THE test this whole task exists for — H6b, co-location does not disclose**

```rust
#[test]
fn walking_past_a_settlement_visits_the_cell_and_discovers_nothing() {
    // The fixture must walk PAST something. A test that only ever walks
    // straight INTO things cannot see this defect at all.
    let mut d = test_driver();
    let (cell, site) = d.a_cell_containing_a_settlement_we_will_not_enter();
    d.walk_through(cell);                      // in and out; never `enter`

    assert!(d.visited().contains_at_rung(cell, 12), "we were there, so the cell is visited");
    assert!(!d.discovered().contains(site),        "but we never met the settlement");
    assert_eq!(d.name_of(site), None,              "so it has no name on the map");
    assert!(!d.plate_draws_point_site(site),       "and it is not drawn at all — nothing is drawn and then hidden");

    // At EVERY rung, because coarse zoom is where the temptation to leak lives.
    for rung in [12, 10, 8, 6, 4] {
        assert!(!d.discovered_at_rung(site, rung), "co-location leaked at rung {rung}");
    }
}
```

- [ ] **Step 4: Run it and confirm it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib discovery > /tmp/hv-t5.log 2>&1; echo "exit=$?"`
Expected: FAIL.

- [ ] **Step 5: Implement `Discovered` with the encounter rule (§A4b)**

| kind | encountered by |
|---|---|
| **extent features** (landmass, sea, river, salt lake, volcano) | the possession has walked any cell in `Feature::extent` — the feature IS the ground, so co-location and encounter coincide |
| **point sites** (settlement, cave mouth) | the possession encountered the thing, not the cell |

**F9 — reuse an existing arrival predicate; minting a fourth is a STOP.** For a cave the answer is already owned: the delve ladder leads with `Surface` and `Undercroft` is documented as "cave mouths and the first few tens of metres of worked rock", so entering a cave is the `Surface → Undercroft` transition `Session::delve` already performs. For a settlement, choose between `structure_at`'s existence predicate and the `enter` verb, **name which you took, and cite it.**

- [ ] **Step 6: Run and confirm both discovery tests pass**

Run: `cd clients/game && cargo test -p hornvale-game --lib discovery > /tmp/hv-t5.log 2>&1; echo "exit=$?"`
Expected: PASS.

- [ ] **Step 7: Write the failing test for H6 — monotonic, never retroactive**

```rust
#[test]
fn discovery_is_monotonic() {
    let mut d = test_driver();
    let site = d.a_settlement();
    assert_eq!(d.name_of(site), None, "nothing is named before it is encountered");
    d.enter(site);
    let named = d.name_of(site).expect("entering names it");
    d.walk_far_away();
    assert_eq!(d.name_of(site), Some(named), "and it stays named for the session");
}
```

- [ ] **Step 8: Run, implement, re-run**

- [ ] **Step 9: Implement the two visibility classes in `plate.rs` (§A3)**

- **Terrain-borne landmarks** draw as terrain whether or not they are known; only the name is gated.
- **Point sites** are not drawn until discovered — they are *not yet drawn*, never *drawn and hidden*.

- [ ] **Step 10: Test H5 — the map is useful before it is complete**

```rust
#[test]
fn a_plate_with_zero_names_is_still_navigable() {
    let d = test_driver();                       // cold start: nothing discovered
    let g = d.world_plate(40, 20);
    assert_eq!(d.discovered().len(), 0);
    assert!(g.provenance().get(&Source::World).copied().unwrap_or(0) > 0);
    // Coastlines and the largest landmass must be legible with no labels at all.
    assert!(distinct_glyphs(&g) >= 2, "land and water must still be distinguishable");
}
```

Paste the zero-discovery rendering into the task report. **If an unlabelled plate is unreadable, H5 is falsified** — that would mean labels were carrying the legibility §A3 assumes terrain carries. Report it; do not patch it by drawing undiscovered names.

- [ ] **Step 11: Verify H7 — the gate costs nothing in the ledger**

```rust
#[test]
fn opening_the_world_map_leaves_the_ledger_byte_identical() {
    let a = { let mut d = test_driver(); d.walk_a_fixed_route(); d.ledger_json() };
    let b = { let mut d = test_driver(); d.enter_map(); d.world_plate(40, 20); d.walk_a_fixed_route(); d.ledger_json() };
    assert_eq!(a, b, "the map is a reader; a difference means it became a writer");
}
```

This is the same guarantee `purview.rs`'s overlay states ("WRITES NOTHING"). **A difference here is a STOP, not a tuning problem.**

- [ ] **Step 12: Run fmt, clippy, the full client suite, and commit**

```bash
cd clients/game
cargo fmt --check && cargo clippy --all-targets -- -D warnings
cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t5-all.log 2>&1; echo "exit=$?"
grep -E "^test result|FAILED|panicked" /tmp/hv-t5-all.log
git add clients/game/bin/src/discovery.rs clients/game/bin/src/plate.rs clients/game/bin/src/driver.rs
git commit -m "feat(game): discovery — visited cells and encountered features, kept apart"
```

---

### Task 6: Close

**Files:**
- Create: `book/src/chronicle/the-portolan-world-map.md`, `docs/retrospectives/the-portolan-world-map.md`
- Modify: `book/src/frontier/idea-registry.md`, `book/src/open-questions.md`, `docs/decisions/`
- Delete: `windows/worldgen/examples/portolan_spike.rs`, `portolan_measure.rs`, `portolan_resolution_spike.rs`

- [ ] **Step 1: Promote the decision from spec §11**

Two clauses, both already drafted: *a map's frame is a fact about the world, not about the graticule*; and *a view of the world is a lens, not a band*. Add the amendment's third: **co-location is not discovery**. Use the next free number **against the merged tree**, not against a board claim — a board claim tells you a number is taken, never how many are.

- [ ] **Step 2: Delete all three spike instruments and confirm nothing references them**

```bash
git rm windows/worldgen/examples/portolan_spike.rs windows/worldgen/examples/portolan_measure.rs windows/worldgen/examples/portolan_resolution_spike.rs
grep -rn 'portolan_spike\|portolan_measure\|portolan_resolution' --include='*.rs' --include='*.md' --include='*.toml' . | grep -v '^./target'
```
Expected: no hits outside the chronicle/retrospective narrative.

- [ ] **Step 3: Update the registry rows this campaign resolves**

`CLIENT-snapshot-chart-cannot-zoom` stays open (out of scope, and say so). Mark shipped: the world-map row, and any row Task 5 closed. **`PLAY-ruins-have-no-artifact` and `PLAY-site-kinds-are-constant` stay open** — they are the next campaign's brief, not this one's residue to tidy away.

- [ ] **Step 4: Write the chronicle entry**

Product story, book altitude — technical and mathematical, comprehensible without reading the code. The narrative spine: the map is complete and the *knowing* is not; a projection framed by the world's own physics; and the rule that a place you stood on is not a place you found.

- [ ] **Step 5: Write the retrospective LAST**

The Gazetteer's was authored early and under-reported itself by three findings. Include, at minimum: that the controller's one-axis fidelity spectrum could not represent a two-axis decision and the owner's answer fell between its lines; that a scoping claim (the 43 ruins as "the best discoverables") was made before its enabling mechanism was checked, and the owner's own rule then removed it; and that seed 42 alone would have put an 11.3× effect size in the spec that seeds 7 and 1337 cut to 1.2×.

- [ ] **Step 6: Freshness sweep**

Re-read every chapter this touches. If the campaign moved a bet on the Confidence Gradient (`book/src/open-questions.md`), re-score that chapter — decision 0030.

- [ ] **Step 7: Regenerate artifacts and confirm the tree is clean**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```
`docs/audits/` drifts on any pub-boundary change and `docs/digest/` on any decision record — both are common misses.

- [ ] **Step 8: Absorb main, then submit to the sluice**

```bash
make sluice-stage BRANCH=campaign/the-portolan REF=$(git rev-parse HEAD)
```
Absorb main and regenerate **at every stage boundary, not only here.** The merge needs a `Sluice-Headline` trailer in the same block as `Claude-Session`, with no blank line between them.

---

## Self-Review

**Spec coverage.** Body §1 → Tasks 2–4; §2 → Task 2 (Steps 11–12); §3/§3.1/§3.3 → Task 1; §3.2 → Task 3 (Steps 8–9); §4.1/4.2 → Task 3; §4.3 → Task 1 Step 10 + Task 3 Step 1; §5 → Task 4; §6 refusals → Global Constraints; §7 F1/F1b → Task 1, F2 → Task 2 Steps 1–3, F3 → Task 4 Step 3, F4 → Task 2 Steps 5/13, F5 → Task 3 Step 5; §8 H1' → Task 2 Step 13, H2/H3 → Task 3 Step 5, H4 → Task 3 Step 10; §11 → Task 6 Step 1; §12/§A9 → this plan. Amendment §A3 → Task 5 Step 9; §A4a → Task 5 Step 1; §A4b → Task 5 Step 5; §A5 → Task 2 Steps 9–10; §A6 → Task 3 Steps 1–3; §A7 → Global Constraints; §A8 → no task, by design (ruins are out); §A10 F6'/F7' resolved, F8 → Task 4 Step 5, F9 → Task 5 Step 5; §A11 H5 → Task 5 Step 10, H6 → Step 7, H6b → Step 3, H7 → Step 11; §A11b → Task 6 Step 5.

**Gaps found and closed:** F8 had no task until this pass; it is now Task 4 Step 5.

**Type consistency.** `Frame`, `Window`, `Visited`, `Discovered`, `Source::World`, `plate::draw`, `mercator::{project, unproject, frame_for}` are each defined once and used under the same names throughout. `MAX_ZOOM` is declared in Task 3 as deriving from Task 1 Step 10's measurement, not from the spec's estimate.

**Placeholder scan:** clean — every code step carries real code, and every "state it in the report" step names exactly what must be stated.
