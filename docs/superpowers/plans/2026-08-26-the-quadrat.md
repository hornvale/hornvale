# The Quadrat Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the client's map a square-tile raster whose zoom rungs are mesh
depths, serving bands B–E from one renderer, fast enough to draw 200x200 tiles,
and fix the three reported defects (cursor-anchored zoom, the prose pane, pane
width).

**Architecture:** A zoom rung becomes a *mesh depth* rather than a power of two
(decision 0077 applied to the view). The virtual chart's dimensions are then
derived from the mesh at that depth instead of from the plate's own width,
which is what lets a caller render a sub-rectangle — the prerequisite for
tiling. Terrain, features and perception become three layers with distinct
cache keys. Band A (the chamber band) is already a square lattice with real fog
of war and is not touched.

**Tech Stack:** Rust 2024, `clients/game` (its own workspace, outside the cargo
workspace — crossterm/signal-hook allowed there, decision 0055),
`windows/scene`, `kernel/src/room.rs`. `cargo nextest` for tests.

**Spec:** `docs/superpowers/specs/2026-08-26-the-quadrat-design.md`

## Global Constraints

- **No new workspace dependency.** `serde`, `serde_json`, `libm` only
  (decision 0004). `clients/` is exempt but adds nothing here.
- **No `HashMap`/`HashSet`** anywhere — `BTreeMap`/`BTreeSet`/`Vec` only,
  enforced by `clippy.toml` `disallowed-types`.
- **Every crate sets `#![warn(missing_docs)]`** — every new `pub` item, field
  and variant gets a one-line doc comment.
- **Every primitive at a `pub` boundary carries a `type-audit:` tag**
  (`bare-ok(<class>)` / `waiver(<reason>)` / `pending(wave-N)`) on the item's
  own doc comment, never on a field's. The check is default-deny.
- **`cargo fmt --all` is the final step before every commit.** fmt-gate skips
  are the most common review finding in this project.
- **`make gate-commit` before every commit** that touches Rust.
- **No committed byte may move.** Nothing here is in the determinism path: no
  seed label, no stream order, no serialized float. If `make rebaseline` moves
  anything outside `docs/audits/`, `docs/digest/` and this campaign's own
  fixtures, STOP — that is a finding, not a rebaseline.
- **Band A, `PURVIEW_RADIUS`, and `scene/surrounds/v2`'s key set are OUT OF
  SCOPE.** If a task appears to need a wire key that does not exist, STOP and
  report — that is an epoch question for Nathan, not a task decision.
- **Run the suite ONCE and inspect many.** Capture to a file and grep it; never
  re-run to ask a second question.

## Known limitation, stated once so no task rediscovers it

At band B (mesh depth 12) the terrain **fields** are decided at the canonical
grid level (6). `windows/scene/src/surrounds.rs:333` states it: *"4^6 rooms
share one grid cell."* So the terrain layer at band B is an interpolation
between vertices ~120 km apart, and a 200x200 tile view spans ~374 km — about
three grid cells. **Expect a smooth gradient, not varied ground.** This is the
data's resolution, and decision 0196 forbids inventing below it. What carries
band B visually is the FEATURE layer (Task 4): rivers, relief banding (ordinal
fields band a blend and so do vary sub-cell), settlements, caves. Do not
"fix" the flatness.

---

### Task 1: Decouple the virtual chart from the plate width

Closes `CLIENT-draw-with-cannot-render-a-subrect`. Nothing else in this plan can
proceed until a caller can render part of a chart.

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (`Window`, `virtual_dims`, `draw_with`, `area_majority`)
- Modify: `clients/game/bin/src/driver.rs` (every `Window` construction and `virtual_dims` call)
- Test: `clients/game/bin/src/plate.rs` (in-module `#[cfg(test)]`)

**Interfaces:**
- Consumes: `mercator::{Frame, project, unproject}`, `hornvale_kernel::Geosphere`.
- Produces:
  - `pub struct Window { pub depth: u32, pub origin_col: u32, pub origin_row: u32 }`
    — `zoom: u8` is REPLACED, not supplemented.
  - `pub fn virtual_dims(depth: u32) -> (u32, u32)` — the plate width is gone
    from the signature. That removal IS the decoupling.
  - `pub const GLOBE_RUNG: u32 = 6;` and `pub const BAND_B_RUNG: u32 = 12;`

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn virtual_dims_come_from_the_mesh_not_the_plate() {
    // The virtual chart's size is a property of the RUNG alone. Two callers
    // drawing different-sized plates at the same rung must agree about the
    // chart they are windows onto -- that agreement is what makes a subrect
    // meaningful, and deriving it from the plate width is what made it
    // impossible before this task.
    let (w_a, h_a) = virtual_dims(BAND_B_RUNG);
    let (w_b, h_b) = virtual_dims(BAND_B_RUNG);
    assert_eq!((w_a, h_a), (w_b, h_b));

    // Coarser rung => half the tiles. Each mesh level halves the edge length.
    // Tolerance is +/-1 IN EITHER DIRECTION because each rung rounds
    // independently: at the real numbers, rung 11 gives 11,623 and rung 12
    // gives 23,245, so doubling the coarse rung OVERSHOOTS by one. A
    // one-sided tolerance fails here, which is what the first draft of this
    // assertion did.
    let (w_coarse, _) = virtual_dims(BAND_B_RUNG - 1);
    assert!(
        (w_coarse * 2).abs_diff(w_a) <= 1,
        "rung {} gave {w_coarse} and rung {} gave {w_a}; expected a halving within 1",
        BAND_B_RUNG - 1, BAND_B_RUNG
    );
}

#[test]
fn the_clamped_mercator_is_nearly_square_in_tiles() {
    // MAP-vertical-axis-undersamples-the-mesh: the old code set
    // `virtual_h = virtual_w / GLYPH_ASPECT`, conflating the GLYPH aspect (a
    // screen property, 2 columns per row) with the PROJECTION aspect (a
    // geometry property). A +/-85-clamped Mercator is nearly square in
    // projected coordinates, so the vertical axis was sampled ~2x too
    // coarsely. Pin the geometry, not the glyph.
    let (w, h) = virtual_dims(BAND_B_RUNG);
    let ratio = f64::from(h) / f64::from(w);
    assert!(
        (ratio - 0.9967).abs() < 0.01,
        "clamped-Mercator aspect came out {ratio}, expected ~0.9967"
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib -- virtual_dims_come_from_the_mesh the_clamped_mercator_is_nearly_square`
Expected: FAIL — `virtual_dims` takes two arguments; `BAND_B_RUNG` undefined.

- [ ] **Step 3: Implement**

```rust
/// The mesh depth band B is drawn at: the walk band, one tile per facet.
/// Matches `hornvale_vessel::agent::walk_depth` (`globe_level + 6`) for the
/// canonical globe level of 6; a world pinned to another globe level moves
/// both together.
/// type-audit: bare-ok(count)
pub const BAND_B_RUNG: u32 = 12;

/// The coarsest rung: the canonical grid level, below which the terrain
/// fields have no resolution to disclose (decision 0196).
/// type-audit: bare-ok(count)
pub const GLOBE_RUNG: u32 = 6;

/// The virtual chart's size in TILES at `depth`, derived from the mesh alone.
///
/// **The plate's own width is deliberately not a parameter.** It used to be,
/// and that is precisely why no caller could render part of a chart
/// (`CLIENT-draw-with-cannot-render-a-subrect`): `draw_with(w = 1, ..)` drew a
/// one-column-wide whole planet rather than one column of a wide one. The
/// chart is a property of the rung; the plate is a window onto it.
///
/// Width is how many facet edges fit around a great circle at `depth`. Height
/// follows from the CLAMPED MERCATOR's own aspect -- `2*mercator_y_max()/2pi`,
/// about 0.9967, i.e. nearly square -- and NOT from
/// `hornvale_game_core::spread::GLYPH_ASPECT`. Conflating the two is
/// `MAP-vertical-axis-undersamples-the-mesh`: the glyph aspect says a tile is
/// two columns wide on a terminal, which is a fact about terminals.
/// type-audit: bare-ok(count: depth), bare-ok(count: return)
pub fn virtual_dims(depth: u32) -> (u32, u32) {
    let w = tiles_around_a_great_circle(depth);
    let aspect = (2.0 * mercator::mercator_y_max()) / (2.0 * std::f64::consts::PI);
    let h = ((f64::from(w) * aspect).round() as u32).max(1);
    (w, h)
}
```

Derive `tiles_around_a_great_circle(depth)` from the mesh — facet count at
`depth` is `20 << (2 * depth)`, and the count around a great circle follows from
that and the icosahedron's own geometry. **Derive it; do not hardcode a table.**
A hardcoded ladder becomes a tuned number the first time the globe level moves,
which is the mistake `hornvale_vessel::course::step_length_rad`'s own doc
records avoiding.

`mercator::mercator_y_max()` is currently private (`mercator.rs:145`); make it
`pub(crate)` with a doc comment.

- [ ] **Step 4: Run to verify it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib -- virtual_dims the_clamped_mercator`
Expected: PASS

- [ ] **Step 5: Migrate every `Window` construction and `virtual_dims` call site**

`rg 'Window \{|virtual_dims\(' clients/game/bin/src/` lists them. `zoom: 0`
becomes `depth: GLOBE_RUNG`; the finest rung becomes `depth: BAND_B_RUNG`.
**The ladder's direction inverts** — zooming IN raises `depth`, zooming OUT
lowers it. `MAX_ZOOM` and `MAX_VIRTUAL_WIDTH` are DELETED, not repointed;
leaving them as aliases would keep the old ladder quietly alive.

- [ ] **Step 6: Pin the subrect property**

```rust
#[test]
fn a_narrow_plate_draws_a_subrect_of_the_wide_one() {
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);
    let win = Window { depth: GLOBE_RUNG, origin_col: 40, origin_row: 20 };
    let wide = draw_with(&terrain, &geo, &index, &f, &win, 32, 8, false,
                         &BTreeSet::new(), &BTreeSet::new(), &Discovered::default());
    let narrow = draw_with(&terrain, &geo, &index, &f, &win, 8, 8, false,
                           &BTreeSet::new(), &BTreeSet::new(), &Discovered::default());
    for y in 0..8u16 {
        for x in 0..8u16 {
            assert_eq!(narrow.get(x, y).map(|c| c.glyph), wide.get(x, y).map(|c| c.glyph),
                "column {x} row {y} disagreed between an 8-wide and a 32-wide plate");
        }
    }
}
```

This assertion is the task's whole point and could not have been written before
it. Adapt the constructor names to what `discovery.rs` actually exposes — **do
not invent an API to make a test compile.**

- [ ] **Step 7: Gate and commit**

```bash
cargo fmt --all
make gate-commit
git add clients/game/bin/src/plate.rs clients/game/bin/src/driver.rs
git commit -m "feat(plate): a rung is a mesh depth, and the chart is no longer derived from the plate width"
```

---

### Task 2: The rung ladder's ends, and its refusals

**Files:**
- Modify: `clients/game/bin/src/driver.rs` (`apply_zoom`, `reclamp_window`, `active_plate_dims`, `world_plate_for_redraw`)
- Test: `clients/game/bin/src/driver.rs` (in-module `#[cfg(test)]`)

**Interfaces:**
- Consumes: `plate::{Window, virtual_dims, BAND_B_RUNG, GLOBE_RUNG}` (Task 1).
- Produces: `Driver::apply_zoom` operating on `depth`. (The strip's rung line is Task 7's, not this task's — see Task 7 Step 5.)

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_ladder_runs_from_the_globe_rung_to_band_b_and_refuses_past_both() {
    let mut d = test_driver();
    d.enter_map();
    for _ in 0..20 { d.apply(Action::Zoom(1)); }
    assert_eq!(d.window().depth, plate::BAND_B_RUNG, "zoomed in past band B");
    for _ in 0..20 { d.apply(Action::Zoom(-1)); }
    assert_eq!(d.window().depth, plate::GLOBE_RUNG, "zoomed out past the globe");
}

#[test]
fn every_rung_of_the_ladder_is_reachable_and_distinct() {
    // Seven rungs, SIX zoom-out steps -- the bound Session::map already
    // enforces as `depth - globe_level`. Stated as both numbers because they
    // differ by one and this is where that discrepancy would be minted.
    let mut d = test_driver();
    d.enter_map();
    for _ in 0..20 { d.apply(Action::Zoom(1)); }
    let mut seen = std::collections::BTreeSet::new();
    seen.insert(d.window().depth);
    for _ in 0..6 {
        d.apply(Action::Zoom(-1));
        assert!(seen.insert(d.window().depth), "a rung repeated");
    }
    assert_eq!(seen.len(), 7, "expected 7 rungs, saw {seen:?}");
    assert_eq!(*seen.iter().next().unwrap(), plate::GLOBE_RUNG);
    assert_eq!(*seen.iter().next_back().unwrap(), plate::BAND_B_RUNG);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib -- the_ladder_runs_from every_rung_of_the_ladder`
Expected: FAIL — `apply_zoom` still manipulates `world_view` and `zoom`.

- [ ] **Step 3: Implement**

`delta > 0` raises `depth` toward `BAND_B_RUNG`; `delta < 0` lowers it toward
`GLOBE_RUNG`; both saturate. **Delete the two `Window { zoom: 0, .. }`
mode-flip resets.**

**CONTROLLER RULING (pre-dispatch, binding): do NOT delete `self.world_view`.
DERIVE it.** An earlier draft of this step said to delete the field outright.
That is wrong at this point in the plan, and the reason is worth stating
because it would have broken the walk band with no test to catch it.

`world_view` is not merely a mode flag. Re-derived from the observable — `rg
'world_view' clients/game/bin/src/driver.rs` — it has **ten** sites, not the
three this step implied, and two of them pick between genuinely different
renderers:

| site | what it selects |
|---|---|
| `active_plate_dims` (~:1218) | the world plate's fitted size **vs** `PLATE_WIDTH` (40), the walk chart |
| `resolve` (~:1493) | `resolve_world_view` (terrain, from the mesh) **vs** `resolve_walk_band` (the wire packet's chart) |
| `resize` (~:762), `move_cursor` (~:1273), `recentre` (~:1370) | whether the scroll/anchor math applies at all |
| `world_plate_for_redraw` (~:879) | the redraw gate |
| the module doc (~:32), the field (~:315), the init (~:711) | declarations |

Band B does not *become* a rung of the raster ladder until **Task 6** moves
`spread::compose` and `plate.rs` to draw it. Deleting the distinction now would
leave `active_plate_dims` and `resolve` unable to tell the two renderers apart,
and the walk band would render as a mis-sized world plate.

**So:** replace the stored `bool` with a derived method —

```rust
/// Whether the world plate, rather than the walk band's own chart, is what
/// the current rung draws.
///
/// **Derived, never stored.** It was a `bool` set by a mode gesture, which
/// is what made the zoom keys mean two different things at the ladder's
/// ends. The rung alone decides now: every rung coarser than
/// [`plate::BAND_B_RUNG`] is the raster; band B itself is still the walk
/// band's chart until Task 6 moves it onto the raster too, at which point
/// this method has no referent and goes away.
fn world_view(&self) -> bool {
    self.window.depth < plate::BAND_B_RUNG
}
```

Every one of the ten sites reads `self.world_view()`. `world_plate_for_redraw`'s
gate becomes `self.focus == Focus::Map && self.world_view()` — the gate stays,
its SOURCE changes.

This is what removes the reported "zooms based on criteria I have not
identified": the two keys now change exactly one number, and which renderer
appears is a function of that number rather than of hidden state.

- [ ] **Step 4: Run to verify it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib > /tmp/hv-t2.log 2>&1; echo "exit=$?"` then grep the log.
Expected: PASS. Existing `world_view` tests will fail to COMPILE — correct;
update them to assert on `depth`. **Do not delete a test to make it compile.**
If a test's intent no longer has a referent, say so in the report.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit**

```bash
git commit -m "feat(driver): band B is a rung, not a mode

world_view was a boolean beside a zoom integer, so the same two keys meant
'change rung' mid-ladder and 'change mode' at its ends -- the reported
'zooms based on criteria I have not identified'. With rungs as mesh depths,
band B is simply the finest rung and the boolean has no referent."
```

---

### Task 3: Mesh-aligned terrain lookup — and MEASURE H1

**This task carries the campaign's preregistered hypothesis. Measure BEFORE the
tile cache exists, so the two effects cannot confound.**

**CONTROLLER RULING (pre-dispatch, binding): do NOT add `probe_count` to
`NearestVertexIndex`. The kernel already has this instrument.**

An earlier draft of this task said to add a `Cell<u64>` probe counter to
`NearestVertexIndex` in `kernel/src/geosphere.rs`. Do not. Two reasons, the
second decisive:

1. It puts mutable state into the determinism substrate for a client test, and
   `Cell` would make the type `!Sync`. I checked: no parallel closure currently
   captures a `NearestVertexIndex` by reference (`windows/lab`'s `map_seeds`
   requires `F: Fn(u64) -> T + Sync`, and `windows/worldgen`'s `thread::scope`
   suites do not touch it), so it would *probably* compile — "probably" is not
   a reason to modify the kernel.
2. **`RoomMeshMemo` already carries exactly this instrument, publicly.**
   `kernel/src/room.rs:753-774`: `corner_weights_hits()`,
   `corner_weights_misses()`, `neighbors_hits()`, `neighbors_misses()` — all
   `pub`, documented, `type-audit`-tagged, and the doc says in as many words:
   *"this campaign's instrument for whether the memo's reuse is real (the-forebay
   Task 1)."* A prior campaign built this for the same question.

`corner_weights_memo` calls `NearestVertexIndex::nearest_to_position` **three
times on a miss and zero times on a hit**. So a memo hit *is* the absence of a
vertex search, and `corner_weights_misses()` is the probe counter this task
wanted — already public, already tested, no kernel edit.

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (`area_majority` → `terrain_at_tile`)
- Create: `clients/game/bin/examples/rung_bench.rs` (kept)
- Test: `clients/game/bin/src/plate.rs`
- **`kernel/` is NOT modified by this task.**

**Interfaces:**
- Consumes: `hornvale_kernel::{Facet, RoomMeshMemo}`, `Facet::containing`,
  `Facet::corner_weights_memo`, `GeneratedTerrain::{is_ocean, elevation_at}`.
- Produces: `pub(crate) fn terrain_at_tile(terrain, geo, index, memo, f, win, virtual_w, virtual_h, row, col) -> TileTerrain`
  where `TileTerrain { pub ocean: bool, pub facet: Facet, pub vertex: Vertex }`.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_tile_resolves_to_the_facet_that_contains_it() {
    // The old path ran index.nearest(geo, lat, lon) -- a spatial SEARCH -- 49
    // times per cell. The new path asks the MESH which facet contains the
    // tile's centre and reads that facet's corner vertices by direct
    // addressing. Observable difference: the answer must agree with
    // Facet::containing exactly, for every tile.
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let mut memo = RoomMeshMemo::default();
    let f = mercator::frame_for(false);
    let win = Window { depth: GLOBE_RUNG, origin_col: 0, origin_row: 0 };
    let (vw, vh) = virtual_dims(win.depth);
    for row in 0..4u32 {
        for col in 0..4u32 {
            let got = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col);
            let (lat, lon) = mercator::unproject(&f, row, col, vw, vh);
            let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
            assert_eq!(got.facet, hornvale_kernel::Facet::containing(pos, win.depth),
                "tile ({row},{col}) resolved to the wrong facet");
        }
    }
}

#[test]
fn the_finest_rung_needs_no_subsampling() {
    // At band B a tile IS a facet, so there is nothing inside it to take a
    // majority over. Subsampling exists only because a COARSE tile spans many
    // facets. This is the mechanism H1 predicts the speedup from, asserted
    // rather than timed -- a timing assertion is a flake on a contended box.
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let mut memo = RoomMeshMemo::default();
    let f = mercator::frame_for(false);
    let win = Window { depth: BAND_B_RUNG, origin_col: 0, origin_row: 0 };
    let (vw, vh) = virtual_dims(win.depth);
    // The probe counter is `RoomMeshMemo::corner_weights_misses()`, which the
    // kernel already exposes (room.rs:753-774, built by The Forebay for this
    // exact question). `corner_weights_memo` runs three
    // `nearest_to_position` scans on a MISS and none on a HIT, so a miss count
    // IS a search count, divided by three.
    //
    // Warm the memo on the tile's own globe-level ancestor first.
    let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 0);
    let before = memo.corner_weights_misses();
    let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 1);
    // BOUNDED, not zero: tiles (0,0) and (0,1) are not guaranteed to share a
    // globe-level ancestor, and a cold ancestor costs exactly one miss. An
    // exact-zero assertion would flake on an ancestor boundary rather than
    // fail on a real regression. One miss is three vertex scans; the OLD path
    // ran 49 unmemoised ones per tile, so this still discriminates by more
    // than an order of magnitude.
    assert!(memo.corner_weights_misses() - before <= 1,
        "the finest rung took {} memo misses for one tile; direct addressing costs at most 1",
        memo.corner_weights_misses() - before);
}
```

No new instrument is needed. `RoomMeshMemo::corner_weights_misses()` is already
`pub` and already tagged (`kernel/src/room.rs:761`). **Do not add one.**

- [ ] **Step 2: Run to verify it fails**

Run: `cd clients/game && cargo test -p hornvale-game --lib -- a_tile_resolves_to_the_facet the_finest_rung_needs_no_subsampling`
Expected: FAIL — `terrain_at_tile` and `probe_count` do not exist.

- [ ] **Step 3: Implement**

Tile centre → `unproject` → unit vector → `Facet::containing(pos, win.depth)` →
`facet.corner_weights_memo(geo, index, memo)` → three `(Vertex, weight)` pairs →
blend. For a rung coarser than `BAND_B_RUNG`, take the majority over the facet's
own CHILDREN rather than over `SUBSAMPLES_PER_AXIS` squared unprojected points —
the children ARE the sub-resolution, which makes a coarse rung a reduction over
the mesh instead of a resample of the screen.

- [ ] **Step 4: Run to verify it passes**

Run: `cd clients/game && cargo test -p hornvale-game --lib > /tmp/hv-t3.log 2>&1; echo "exit=$?"` then grep.

- [ ] **Step 5: Write the harness and MEASURE H1**

```bash
cd clients/game
cargo build --release --example rung_bench
./target/release/examples/rung_bench --tiles 200x200 --rung 12 --runs 5
./target/release/examples/rung_bench --tiles 104x52  --rung 6  --runs 5
```

The harness prints wall time and `probe_count` per draw, at the old 104x52
reference size and at the 200x200 bar. **Record both in the task report and in
`docs/timings.md`.** Then dispose H1 by decision rule, not by prediction:

- 200x200 uncached **under 50 ms** → H1 SUPPORTED. State the number.
- 200x200 uncached **50 ms or more** → **H1 IS NULL.** That is a result, not a
  failure. Report it plainly, retune nothing to rescue it, and note that Task
  5's cache now carries the budget alone. The spec preregistered this branch
  (§5, §9); taking it is compliance.

**Do not skip this because the code looks fast.** Measuring here is the whole
reason this task precedes Task 5.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

Commit message must carry the actual measured numbers and the H1 disposition.
**Do not commit with a placeholder in the message.**

---

### Task 4: The layer split

Closes `CLIENT-tiles-need-the-overlay-split`, which Task 5 depends on.

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (split `draw_with`'s single pass)
- Modify: `clients/game/bin/src/driver.rs` (`PlateKey`)
- Test: `clients/game/bin/src/plate.rs`

**Interfaces:**
- Produces:
  - `pub(crate) fn draw_terrain_layer(..) -> Grid` — depends ONLY on
    `(frame, win.depth, win.origin_col, win.origin_row, w, h, colour_allowed)`.
  - `pub(crate) fn draw_feature_layer(dst: &mut Grid, ..)` — settlements, caves, discovery-gated.
  - `pub(crate) fn draw_perception_layer(dst: &mut Grid, ..)` — marks from the per-turn snapshot.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_discovery_does_not_change_the_terrain_layer() {
    // The invalidation argument as an assertion. Terrain and point sites drew
    // in ONE pass, so a tile keyed on the discovery version was invalidated by
    // every discovery -- the whole pyramid, for one settlement. This property
    // is what makes a tile cache viable at all.
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let mut memo = RoomMeshMemo::default();
    let f = mercator::frame_for(false);
    let win = Window { depth: GLOBE_RUNG, origin_col: 0, origin_row: 0 };
    let bare = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, 32, 16, false);
    let after = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, 32, 16, false);
    assert_eq!(bare.to_plain_text(), after.to_plain_text());
    // and it must not even TAKE a discovery argument -- check the signature
    // compiles without one, which this call already does.
}

#[test]
fn the_feature_layer_draws_only_discovered_sites() {
    // Decision 0197 / spec Amendment 1 A7: nothing is drawn and then hidden.
    // An undiscovered site's glyph is never chosen; there is no suppression
    // pass over an already-painted grid, because a site never drawn cannot leak.
    let (terrain, geo) = test_world();
    let index = NearestVertexIndex::new(&geo);
    let mut memo = RoomMeshMemo::default();
    let f = mercator::frame_for(false);
    let win = Window { depth: GLOBE_RUNG, origin_col: 0, origin_row: 0 };
    let site = some_settlement_vertex(&geo);
    let sites: BTreeSet<Vertex> = [site].into_iter().collect();

    let mut g = draw_terrain_layer(&terrain, &geo, &index, &mut memo, &f, &win, 32, 16, false);
    let before = g.to_plain_text();
    draw_feature_layer(&mut g, &geo, &f, &win, 32, 16, false, &sites, &BTreeSet::new(), &Discovered::default());
    assert_eq!(g.to_plain_text(), before, "an UNdiscovered site was drawn");

    let discovered = discovered_containing(site);
    draw_feature_layer(&mut g, &geo, &f, &win, 32, 16, false, &sites, &BTreeSet::new(), &discovered);
    assert!(g.to_plain_text().contains(SETTLEMENT_GLYPH), "a DISCOVERED site was not drawn");
}
```

`some_settlement_vertex` / `discovered_containing` may not exist under those
names — read `clients/game/bin/src/discovery.rs` and use the real API. **Adapt
the test to the code that is there; do not invent an API.**

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL, the three layer functions do not exist.

- [ ] **Step 3: Implement** — split `draw_with`'s body into the three functions;
`draw_with` becomes their composition, so no existing caller changes.

- [ ] **Step 4: Run to verify it passes** — capture to a log, grep it.

- [ ] **Step 5: Narrow `PlateKey`** so the terrain half no longer carries
`discovered_len`. The feature layer redraws every frame (it is a handful of
sites); only terrain is keyed and cached.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

---

### Task 5: The tile cache

Closes `CLIENT-render-tile-cache`.

**Files:**
- Create: `clients/game/bin/src/tiles.rs`
- Modify: `clients/game/bin/src/driver.rs`, `clients/game/bin/src/lib.rs`
- Test: `clients/game/bin/src/tiles.rs`

**Interfaces:**
- Consumes: `plate::draw_terrain_layer` (Task 4).
- Produces: `pub struct TileCache` with `pub fn terrain(&mut self, ..) -> &Grid`
  keyed `(Frame, depth, tile_x, tile_y)`, plus `pub fn hits(&self) -> u64` and
  `pub fn misses(&self) -> u64`.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn scrolling_one_column_reuses_every_tile_but_the_new_edge() {
    // Measured before this campaign: a full 104x52 plate is 130 ms; ONE column
    // is 1.208 ms -- ~108x cheaper. Asserted with hit/miss counts rather than
    // timings, because a timing assertion is a flake on a contended box.
    let mut cache = TileCache::default();
    let mut win = Window { depth: GLOBE_RUNG, origin_col: 0, origin_row: 0 };
    let _ = render_through(&mut cache, &win, 64, 32);
    let first = cache.misses();
    assert!(first > 0, "the first draw hit an empty cache and should have missed");
    win.origin_col += 1;
    let _ = render_through(&mut cache, &win, 64, 32);
    let scroll = cache.misses() - first;
    assert!(scroll * 4 < first,
        "a one-column scroll cost {scroll} misses against {first} for a full draw");
}

#[test]
fn a_rung_change_does_not_serve_stale_tiles() {
    // The key carries the rung. Two rungs are two pictures of the same ground,
    // and serving one for the other is the failure mode a (frame, tile) key
    // without the depth would have.
    let mut cache = TileCache::default();
    let a = render_through(&mut cache, &Window { depth: GLOBE_RUNG, origin_col: 0, origin_row: 0 }, 32, 16);
    let b = render_through(&mut cache, &Window { depth: GLOBE_RUNG + 1, origin_col: 0, origin_row: 0 }, 32, 16);
    assert_ne!(a.to_plain_text(), b.to_plain_text(), "two rungs served the same tiles");
}
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL, `tiles` module does not exist.

- [ ] **Step 3: Implement** — a `BTreeMap`-keyed cache (NOT a `HashMap` —
`clippy.toml` bans it, and the ban reaches `clients/`). Bound the cache and
evict by distance from the current window: the two-radius hot/warm policy MAP-70
already established.

**CONTROLLER RULING (pre-dispatch, binding): key the frame on `f64::to_bits()`,
NOT on a quantized form.** `Frame` (`clients/game/bin/src/mercator.rs:99`)
derives only `Debug, Clone, Copy, PartialEq` — it holds two `f64`, so it has no
`Eq`/`Ord` and cannot be a `BTreeMap` key as-is. An earlier draft of this step
said to "key on its quantized form", and that is a **correctness** mistake
rather than a style one:

- A **quantized** key can map two genuinely different frames onto one entry, and
  the cache would then serve a tile drawn under the *wrong projection* — a
  silently wrong picture.
- A **bit** key (`pole_lat_deg.to_bits()`, `pole_lon_deg.to_bits()`) is exact.
  Two frames that are bit-identical render identically; two that differ by an
  ULP get separate entries. The only failure mode is an extra miss, which costs
  time and never correctness.

The asymmetry is the whole argument: over-missing is a performance bug,
over-hitting is a wrong answer. Note also that `Frame` changes rarely — it is
set once at load by `frame_for` and only moved by the explicit `recentre`
gesture — so the extra-miss risk is close to hypothetical anyway.

Do **not** reach for `hornvale_kernel::quantize` here. That function exists for
the emit boundary (decision 0033) and this is a cache key, not an emitted value.

- [ ] **Step 4: Run to verify it passes**

- [ ] **Step 5: Re-run `rung_bench` WITH the cache** and record both numbers in
`docs/timings.md`.

**Also MEASURE the feature layer's per-redraw cost, and report the cave
roster's cardinality.** Task 4 made the feature layer unconditional work over
the whole site roster — one `geo.coord` plus one `mercator::project`
(transcendentals) per site, on every keystroke, including cursor moves that
previously cost exactly zero on a cache hit. The cave roster is built by
scanning all 40,962 vertices (`driver.rs:648`) and **its cardinality is recorded
nowhere in the tree.** Print it, measure the composition cost, and say whether a
window pre-filter is needed — do not inherit "the rosters are small".

**BUDGET AGAINST 91.6 ms, NOT 31.3 ms (controller ruling, from Task 3's
measurement).** H1 came back SUPPORTED but **rung-conditional**: 200x200 costs
31.3 ms at band B, 42.2 ms at rung 8, and **91.6 ms at `GLOBE_RUNG` — 1.8x over
the 50 ms bar.** The coarsest rung is where the memo has almost no reuse
(29,662 misses for 40,000 tiles), and it is a rung a player reaches by holding
`-`. **`GLOBE_RUNG` is therefore this task's hard case and its acceptance
criterion** — a cache tuned on band B would be tuned on the easiest rung.

Measure at rungs 6, 8 and 12. **If rung 6 at 200x200 still misses 50 ms with
the cache warm, STOP and report** rather than proceeding to Task 6.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

---

### Task 6: Band B joins the raster ladder — the overlay is drawn in `bin`

**THIS TASK WAS RE-PLANNED 2026-08-27.** Its first version told you to reproject
`core/chart.rs` and `windows/scene/src/surrounds_ascii.rs` onto the square grid.
An implementer measured that design **false** rather than building it, and the
measurement is in spec §5a. The short version: `core` works from the wire's
*relative* `(bearing_deg, distance_rad)`; the raster works from *absolute*
Mercator tiles via `floor`; bridging them depends on the observer's **sub-tile
phase**, which the wire does not carry. Across 200 phases, **best 0 of 31 marks
misplaced, worst 24, mean 11.5, only 2 of 200 exact.**

**The design that works reuses a path that already exists.** Each wire chart
cell carries a `room` — a packed `FacetId`. `core` cannot use it (no mesh
access); `bin` can, and **already does**: `driver.rs:1695-1696` is
`FacetId(real_cell.room).unpack()` then `room.coord()`. So `bin` draws the
band-B perception layer itself, projecting each cell's facet through the **same**
`mercator::project` the raster uses — exact by construction, one projection
rather than two agreeing by coincidence.

**Files:**
- Modify: `clients/game/bin/src/plate.rs` — add `draw_perception_layer`
- Modify: `clients/game/bin/src/driver.rs` — band B gets a plate; the overlay is
  composed into it; the window is centred (see the ruling below)
- Modify: `clients/game/core/src/spread.rs` — only if band B needs a `world_plate`
  routed to it; **`compose` must NOT learn about rungs**
- Test: `clients/game/bin/src/plate.rs`, `clients/game/bin/src/driver.rs`
- **NOT MODIFIED: `windows/scene/`, `clients/game/core/src/chart.rs`,
  `clients/game/core/tests/chart.rs`, `clients/vessel/`.** If you find yourself
  editing any of those, stop — the whole point of this design is that they do
  not move.

**Interfaces:**
- Consumes: `plate::{draw_terrain_layer, draw_feature_layer}` (Task 4),
  `TileCache` (Task 5), `hornvale_kernel::FacetId`, `Facet::coord`,
  `mercator::project`.
- Produces: `pub(crate) fn draw_perception_layer(dst: &mut Grid, ..)` — takes its
  bounds from `dst`, like `draw_feature_layer` does, for the reason Task 4's fix
  round established.

**CONTROLLER RULING — the band-B window origin (the ruling the block asked for).**
Band B's window origin is `(0, 0)`, which at rung 12 is ~11,800 rows and ~2,200
columns from the fixture's observer: the arctic corner. A band-B view that shows
arctic ocean while you stand in a rainforest is not shippable, so **Task 6
centres band B's window on the observer's own facet.** That is the minimal
obviously-correct behaviour — the walk view has always had you in the middle of
it — and `centre_window_on_the_player` already exists as a test helper
(`driver.rs`), which Task 1's review flagged as needing a size guard when
promoted to production. Promote it, with the guard.

**Task 7 still owns the general policy**: cursor-anchored zoom, and where
entering a *coarse* rung lands. This ruling is deliberately narrow — centre band
B, nothing else.

**RULING 19 IS RETRACTED.** It said `chart::draw` should become a
here-plus-marks overlay. It cannot: the pin at `core/tests/chart.rs:180` compares
`chart::draw`'s 31-cell placement against the sim's render, so `draw` cannot
simultaneously be that placement and a 1–2 glyph overlay. Under this design
`chart::draw` is untouched and the contradiction never arises. **But its
underlying concern stands and is now this task's:** the observer's `'@'` and the
marks must still appear at band B, and `draw_perception_layer` is what paints
them.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_perception_overlay_lands_exactly_where_the_raster_puts_that_facet() {
    // The whole reason this task was re-planned. The overlay resolves each
    // wire cell's `room` FacetId to a coordinate and projects it through the
    // SAME mercator::project the raster uses, so agreement is by construction
    // rather than by arithmetic coincidence. The measured alternative -- a
    // relative polar offset rounded onto absolute floor'd tiles -- misplaced up
    // to 24 of 31 marks depending on the observer's sub-tile phase.
    //
    // EXACT equality, on every cell. If you find yourself weakening this to
    // "within one tile", the design has regressed to the one that was rejected.
    let (drv, cells) = driver_with_a_walk_band_chart();
    let win = Window { depth: plate::BAND_B_RUNG, .. };
    let (vw, vh) = plate::virtual_dims(win.depth);
    let mut checked = 0;
    for cell in &cells {
        let facet = FacetId(cell.room).unpack().expect("a wire cell carries a real facet");
        let coord = facet.coord();
        let expected = mercator::project(&drv.frame(), coord.latitude, coord.longitude, vw, vh)
            .expect("a walk-band facet is inside the clamp");
        let got = drv.perception_square_for(cell).expect("the overlay places every cell");
        assert_eq!(got, expected, "cell at facet {facet:?} landed off its own terrain");
        checked += 1;
    }
    // Non-vacuity: a band with no cells, or an overlay that placed none, would
    // pass an empty loop. The fixture's band is 31 cells.
    assert_eq!(checked, 31, "expected the fixture's full 31-cell band");
}

#[test]
fn band_b_still_shows_the_observer_and_its_marks() {
    // Ruling 19's surviving concern. compose's plate selection was either/or,
    // so handing band B a raster silently removed the ONLY thing that paints
    // the observer's '@' and the creatures. This is the assertion that would
    // have caught that.
    let mut d = test_driver();
    d.enter_map();
    let (w, h) = (120u16, 40u16);
    let plate = d.world_plate_for_redraw(w, h).expect("band B draws a plate");
    let text = plate.to_plain_text();
    assert!(text.contains('@'), "band B lost the observer's own position marker");
    // and the terrain is still under it
    assert!(text.contains('~') || text.contains('.'), "band B lost its terrain raster");
}

#[test]
fn band_b_centres_on_the_observer_not_the_arctic_corner() {
    // The window origin ruling. Origin (0,0) at rung 12 is ~11,800 rows from
    // the fixture's observer. Assert the observer's own facet is INSIDE the
    // drawn window, which is the property that matters and which a hardcoded
    // expected origin would not survive a mesh change.
    let mut d = test_driver();
    d.enter_map();
    let (w, h) = (120u16, 40u16);
    let plate = d.world_plate_for_redraw(w, h).expect("band B draws a plate");
    assert!(plate.to_plain_text().contains('@'),
        "the observer is outside the band-B window; it was not centred");
}
```

Adapt the helper names to the real API — `driver_with_a_walk_band_chart`,
`perception_square_for` and `frame()` may need writing or may exist under other
names. **Do not invent an API to make a test compile.**

- [ ] **Step 2: Run to verify each fails.** Expected: FAIL —
  `draw_perception_layer` does not exist, band B has no plate.

- [ ] **Step 3: Implement.**
  - `draw_perception_layer(dst, ..)` in `plate.rs`: for each wire chart cell,
    unpack `room` → `Facet::coord()` → `mercator::project` → `dst.set`. Paint
    the observer's `'@'` and the dominant mark's glyph. **Do not paint a glyph
    for an ordinary terrain cell** — the raster underneath already shows the
    ground, and painting over it would obliterate the layer this campaign built.
  - `driver.rs`: band B gets a plate (terrain + features + perception), and the
    window is centred on the observer's facet.
  - **`world_view()` is NOT deleted.** Its referent survives, changed: band B now
    has a plate AND a perception overlay AND a sight caption, while coarse rungs
    have a plate and no overlay. Redefine it honestly — a predicate meaning "is
    this the walk band" — or split it. Deleting it also deletes
    `resolve_walk_band` and the walk band's sight caption (7 gate sites, 26
    `enter_world_view` call sites), which is not this task's business.

- [ ] **Step 4: Run to verify they pass.** Capture to a file, grep it.

- [ ] **Step 5: Regenerate and check drift**

```bash
make rebaseline
git diff --stat -- clients/game/core/tests/fixtures/ book/ docs/
```

Branch table, not a prediction:
- **Nothing moves** → the expected outcome, because no renderer changed. Say so.
- **`chart-reference-seed-42.txt` moves** → **STOP.** That fixture pins
  `core/chart.rs`, which this task does not touch. Movement means something
  reached it that should not have.
- **Anything under `book/src/domesday/`, `book/src/gallery/`, or an almanac
  moves** → **STOP.** A determinism finding, not a rebaseline.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

### Task 7: Cursor-anchored zoom, and the rung indicator

**Files:**
- Modify: `clients/game/bin/src/driver.rs` (`apply_zoom`, `recentre`, new shared helper)
- Modify: `clients/game/bin/src/driver.rs` strip composition (`strip_text`)
- Test: `clients/game/bin/src/driver.rs`

**Interfaces:**
- Produces: `fn anchor_geographic_point(&mut self, lat: f64, lon: f64)` — the
  shared tail of `recentre` and `apply_zoom`;
  `pub fn geographic_point_under_cursor(&self) -> (f64, f64)`; and
  `fn rung_caption(&self) -> String`, the strip's rung line.

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn a_zoom_step_keeps_the_same_geographic_point_under_the_cursor() {
    // The invariant. Modelled on the EXISTING
    // recentre_keeps_the_same_geographic_point_under_the_cursor
    // (driver.rs:2293), with a zoom step substituted for the recentre.
    let mut d = test_driver();
    d.enter_map();
    d.apply(Action::CursorRight);   // off-centre; use the real Action variant
    d.apply(Action::CursorDown);
    let before = d.geographic_point_under_cursor();
    d.apply(Action::Zoom(1));
    let after = d.geographic_point_under_cursor();
    assert!((before.0 - after.0).abs() < 0.5 && (before.1 - after.1).abs() < 0.5,
        "the point under the cursor moved from {before:?} to {after:?} across one zoom step");
}

#[test]
fn at_the_coarsest_rung_the_cursor_moves_because_the_window_cannot() {
    // Anchoring has two knobs. Normally the window origin moves; at the
    // coarsest rung the whole planet fits the plate and the origin is pinned
    // to (0,0), so the CURSOR moves instead. A test that only exercised a
    // mid-ladder rung would never reach this branch.
    let mut d = test_driver();
    d.enter_map();
    for _ in 0..20 { d.apply(Action::Zoom(-1)); }
    assert_eq!(d.window().depth, plate::GLOBE_RUNG);
    let point_before = d.geographic_point_under_cursor();
    d.apply(Action::Zoom(1));
    d.apply(Action::Zoom(-1));
    assert_eq!(d.window().origin_col, 0, "the coarsest rung's origin must stay pinned");
    let point_after = d.geographic_point_under_cursor();
    assert!((point_before.0 - point_after.0).abs() < 1.0,
        "the point moved; cursor is now {:?}", d.cursor());
}

#[test]
fn the_strip_names_the_current_rung() {
    // Part of "zooms based on criteria I have not identified" is that nothing
    // on screen said which rung was showing.
    let mut d = test_driver();
    d.enter_map();
    let a = d.strip_text().map(str::to_string);
    d.apply(Action::Zoom(1));
    let b = d.strip_text().map(str::to_string);
    assert_ne!(a, b, "the strip did not change across a zoom step");
}
```

Use the REAL `Action` variants for cursor movement — read `input.rs` first.

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL.

- [ ] **Step 3: Implement** — extract `recentre`'s unproject/re-project/re-anchor
tail into `anchor_geographic_point` and call it from both. **Declare the two
limits in its doc comment**: longitude holds exactly (it wraps); latitude holds
except where Mercator's polar clamp binds — the shape decision 0142 set for a
lost axis.

- [ ] **Step 4: Run to verify it passes. Dispose H3 in the report.**

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit**

---

### Task 8: The prose pane

Three independent changes, one commit each.

**Files:**
- Modify: `clients/game/core/src/entry.rs:125` (`wrap`)
- Modify: `clients/game/bin/src/driver.rs:1075` (the bare-`map` gesture)
- Modify: `clients/game/bin/src/driver.rs` (the lens the client requests)
- Test: `clients/game/core/src/entry.rs`, `clients/game/bin/src/driver.rs`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn wrap_preserves_a_line_that_fits_verbatim() {
    // The reported defect: wrap split paragraphs on '\n' and rejoined on
    // split_whitespace(), collapsing every run of spaces, so a fixed-width
    // picture came out left-flushed.
    let picture = "      +\n   + +   +\n+ +   @ +   +";
    assert_eq!(wrap(picture, 40), vec!["      +", "   + +   +", "+ +   @ +   +"]);
}

#[test]
fn wrap_clips_an_over_wide_line_and_never_reflows_it() {
    // Prose may grow downward; a picture may not grow at all. Re-flowing an
    // over-wide picture line reproduces the defect at a wider band, which is
    // exactly what a naive "wrap what does not fit" rule would do.
    let wide = "+ + + + + + + + + + + + + + + + + + + +";
    let out = wrap(wide, 10);
    assert_eq!(out.len(), 1, "an over-wide line was re-flowed onto {} lines", out.len());
    assert_eq!(out[0].chars().count(), 10);
}

#[test]
fn wrap_still_wraps_ordinary_prose() {
    // The other direction. A rule that preserved everything would stop
    // wrapping narration, which is the pane's actual job.
    let prose = "The sky above: Night. The vast moon is a smear of light.";
    let out = wrap(prose, 20);
    assert!(out.len() > 1, "ordinary prose stopped wrapping");
    assert!(out.iter().all(|l| l.chars().count() <= 20));
}

#[test]
fn a_bare_map_gesture_does_not_send_the_verb() {
    // driver.rs's own comment already argues map is a MODE GESTURE and not a
    // fetch. The code called self.handle("map") first anyway, so the sim
    // answered a mode gesture with a picture the client already draws.
    let mut d = test_driver();
    let before = d.snapshot();
    for ch in "map".chars() { d.apply(Action::FocusAndType(ch)); }
    d.apply(Action::Submit);
    assert_eq!(d.focus(), Focus::Map, "the gesture did not focus the map");
    assert_eq!(d.snapshot(), before, "the gesture sent a verb and advanced the session");
}

#[test]
fn map_out_n_still_returns_the_sims_own_picture() {
    // Only the BARE form changes, and that is load-bearing: `map out N` is the
    // diagnostic path that caught The Quire's wrong projection, where client
    // and sim were rendered side by side over the identical thirty-one cells
    // and only one was right. It guards H2.
    let mut d = test_driver();
    for ch in "map out 1".chars() { d.apply(Action::FocusAndType(ch)); }
    d.apply(Action::Submit);
    let v: serde_json::Value = serde_json::from_str(&d.snapshot()).unwrap();
    let prose = v["narration"]["prose"].as_str().unwrap();
    assert!(prose.contains('\n'), "map out 1 did not return a multi-line picture");
    assert_ne!(d.focus(), Focus::Map, "an argument form moved the plate");
}

#[test]
fn the_client_requests_the_escape_free_lens() {
    // surrounds_ascii.rs:224 emits \x1b[38;2;r;g;bm per TINTED glyph, and Cell
    // holds a single char -- so the first tinted chart renders escape bytes as
    // literal glyphs. NOT reproducible at seed 42 turn 0 ("0 tinted, 31
    // withheld"), which is why it stayed latent, so this test CONSTRUCTS a
    // tinted chart rather than relying on the fixture.
    let scene = tinted_chart_fixture();
    let out = render_for_client(&scene);
    assert!(!out.contains('\u{1b}'), "an escape sequence reached the prose channel");
}
```

- [ ] **Step 2: Run to verify each fails** — Expected: FAIL on all six.

- [ ] **Step 3: Implement `wrap`, then the gesture, then the lens** — three
commits, each gated.

- [ ] **Step 4: Run to verify they pass**

Run: `cargo test -p hornvale-game-core -p hornvale-game > /tmp/hv-t8.log 2>&1; echo "exit=$?"` then grep.

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit each**

---

### Task 9: Pane width

**Files:**
- Modify: `clients/game/core/src/spread.rs:43` (`PLATE_WIDTH`, `world_plate_width`, `compose`)
- Test: `clients/game/core/tests/spread.rs`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn the_plate_claims_at_least_half_the_terminal() {
    for (w, h) in [(80u16, 24u16), (120, 40), (200, 50), (300, 80)] {
        let (g, _) = compose_at(w, h, Focus::Walk);
        let cols = plate_column_count(&g);
        assert!(cols * 2 >= w, "at {w}x{h} the plate claimed {cols} of {w} columns, under half");
    }
}

#[test]
fn the_entry_pane_keeps_a_legible_minimum() {
    // The direction a bare "at least half" rule breaks: at the 80x24 floor,
    // half is 40 columns and the entry pane must still be readable. A rule
    // with one bound squeezes prose to nothing on a narrow terminal.
    for (w, h) in [(80u16, 24u16), (120, 40), (200, 50)] {
        let (g, _) = compose_at(w, h, Focus::Walk);
        let entry = w - plate_column_count(&g);
        assert!(entry >= MIN_ENTRY_WIDTH,
            "at {w}x{h} the entry pane got {entry} columns, under the legible minimum");
    }
}
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL, `PLATE_WIDTH` is a fixed 40.

- [ ] **Step 3: Implement** — `max(PLATE_WIDTH, w / 2)`, clamped so
`w - plate_width >= MIN_ENTRY_WIDTH`. Add `MIN_ENTRY_WIDTH` with a doc comment
saying what "legible" means and why that number.

- [ ] **Step 4: Run to verify it passes**

- [ ] **Step 5: `cargo fmt --all`, `make gate-commit`, commit**

---

### Task 10: Artifacts, book, chronicle, retrospective

**Definition of Done for every merged plan in this project** — not optional.

**Files:**
- Modify: `book/src/frontier/idea-registry.md`
- Create: `book/src/chronicle/the-quadrat.md`
- Create: `docs/retrospectives/the-quadrat.md`
- Create: `docs/decisions/0286-*.md` … `0291-*.md`
- Modify: any book chapter describing the client's map

- [ ] **Step 1: Regenerate and check drift**

```bash
make rebaseline
git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')
```

`docs/audits/` and `docs/digest/` moving is EXPECTED (the type-audit report
drifts on any `pub` boundary change; the decision index drifts when a decision
lands). Anything else moving needs a stated reason in the report.

- [ ] **Step 2: Write the six decision records** — 0286 through 0291, exactly as
listed in spec §12. Append-only; supersede, never edit.

- [ ] **Step 3: Mark the registry rows** — `CLIENT-draw-with-cannot-render-a-subrect`,
`CLIENT-tiles-need-the-overlay-split`, `CLIENT-render-tile-cache` shipped, and
`CLIENT-snapshot-chart-cannot-zoom` shipped as a consequence. Add the rows from
`.superpowers/sdd/followups.md`. **Re-measure**
`MAP-vertical-axis-undersamples-the-mesh` and
`MAP-settlement-glyph-may-be-unreachable-at-any-shipped-zoom` — do NOT assume
rungs-as-depths dissolved them; if they did, say so with the number.

- [ ] **Step 4: The chronicle** — technical/mathematical altitude, comprehensible
without reading the code. Lead with what H1 actually MEASURED, not what it
predicted. If H1 was null, that is the headline.

- [ ] **Step 5: The retrospective** — process lessons, not product. The one that
must be in it: this spec was drafted twice, and the first draft invented an
epistemic model for band B and built a cost carve-out on top of it. Nothing
mechanical caught that; the owner restating what the game *is* caught it.

- [ ] **Step 6: Freshness sweep** — any book chapter describing the client's map
or the zoom ladder. If a Confidence Gradient bet in `book/src/open-questions.md`
moved, re-score that chapter (decision 0030).

- [ ] **Step 7: `make gate-commit`, commit, then submit to the sluice**

```bash
make sluice BRANCH=campaign/the-quadrat REF=$(git rev-parse HEAD)
```
