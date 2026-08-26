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

**Files:**
- Modify: `clients/game/bin/src/plate.rs` (`area_majority` → `terrain_at_tile`)
- Modify: `kernel/src/geosphere.rs` (add `probe_count`, a test instrument)
- Create: `clients/game/bin/examples/rung_bench.rs` (kept)
- Test: `clients/game/bin/src/plate.rs`

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
    // Warm the memo so a warm ancestor is the common case.
    let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 0);
    let before = index.probe_count();
    let _ = terrain_at_tile(&terrain, &geo, &index, &mut memo, &f, &win, vw, vh, 0, 1);
    // BOUNDED, not zero: tiles (0,0) and (0,1) are not guaranteed to share a
    // globe-level ancestor, and a cold ancestor costs three corner probes. An
    // exact-zero assertion would flake on an ancestor boundary rather than
    // fail on a real regression. Three still discriminates the new path from
    // the old one by more than an order of magnitude -- the old path was 49.
    assert!(index.probe_count() - before <= 3,
        "the finest rung ran {} vertex searches; direct addressing costs at most 3",
        index.probe_count() - before);
}
```

Add `probe_count` to `NearestVertexIndex`: a `Cell<u64>` incremented in
`nearest`/`nearest_to_position`, exposed as `pub fn probe_count(&self) -> u64`,
documented as **a test instrument, not a behaviour** — it must not affect any
output. `Cell` is fine here: no threading, no determinism surface.

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

- [ ] **Step 3: Implement** — `BTreeMap<(Frame, u32, u32, u32), Grid>` (NOT a
`HashMap` — `clippy.toml` bans it). Bound the cache and evict by distance from
the current window: the two-radius hot/warm policy MAP-70 already established.
`Frame` must be `Ord`; if it holds `f64`, key on its quantized form and document
why in the key type's doc comment.

- [ ] **Step 4: Run to verify it passes**

- [ ] **Step 5: Re-run `rung_bench` WITH the cache** and record both numbers in
`docs/timings.md`. If H1 was null in Task 3, this is where the budget is met or
is not. **If 200x200 still misses the bar, STOP and report** rather than
proceeding to Task 6.

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

---

### Task 6: Band B joins the raster ladder, and the sim's renderer moves with it

**The pin at `clients/game/core/tests/chart.rs:180` is KEPT, not retired.** It is
the only thing that has ever caught a wrong projection here (The Quire:
"geometrically wrong under seventeen green tests"). Spec H2 is this task.

**CONTROLLER RULING (pre-flight, binding on this task).** An earlier draft of
this task said `clients/game/core/src/chart.rs` reprojects terrain onto the
raster. **It cannot.** `hornvale-game-core` carries NO hornvale crate in its
dependency graph by design — its own `chart.rs` module doc says so — so it has
no access to `Facet::containing`, the geosphere or terrain. The task is
restructured along what each crate can actually reach:

- **`bin/src/plate.rs` draws the band-B TERRAIN raster**, exactly as it already
  draws C/D/E. `spread::compose` receives a `world_plate` at band B too — the
  existing `world_plate: Option<&Grid>` parameter already supports this, so no
  new seam is needed.
- **`core/src/chart.rs` keeps the PERCEPTION layer** — the 31 packet cells,
  `here`/marks/NPCs — placed by `bearing_deg`/`distance_rad`, which needs no
  mesh and is exactly why it works in `core`. Its PROJECTION changes; its data
  source does not.
- **`windows/scene/src/surrounds_ascii.rs` makes the SAME projection change**,
  so the byte pin survives.
- **The pin at `chart.rs:180` is kept and its subject is UNCHANGED.** It was
  never a terrain comparison. The Quire's account is that "the simulation drew
  five dense rows (5 + 7 + 9 + 7 + 3 = 31) and the client drew nine sparse
  sheared rows" — it pins the PLACEMENT of the packet's 31 cells. Both sides
  keep placing 31 cells; only the projection moves, and it must move
  identically on both.

**Files:**
- Modify: `windows/scene/src/surrounds_ascii.rs` (projection only)
- Modify: `clients/game/core/src/chart.rs` (projection only — perception layer)
- Modify: `clients/game/core/src/spread.rs` (band B takes a `world_plate`)
- Modify: `clients/game/bin/src/driver.rs` (`world_plate_for_redraw` supplies band B)
- Modify: `clients/game/core/tests/fixtures/chart-reference-seed-42.txt` (REGENERATED, never hand-edited)
- Test: `clients/game/core/tests/chart.rs`, `clients/game/bin/src/driver.rs`

- [ ] **Step 1: Write the failing test** — the existing
`the_shape_matches_the_sims_own_ascii_render` IS the byte pin and will fail once
either side moves. Add one that pins the PROPERTY the stagger was a symptom of:

```rust
#[test]
fn the_raster_has_no_holes_inside_the_band() {
    // The old projection was not surjective onto the character grid: one glyph
    // per mesh cell, gaps between them. A raster asks each CHARACTER cell which
    // facet contains it, so every cell inside the band's extent is drawn.
    let c = walk_chart();
    let mut g = Grid::new(40, 20);
    chart::draw(&c, &mut g, (0, 0));
    assert!(cells_within_band_extent(&g).iter().all(|cell| !cell.is_blank()),
        "the raster left a hole inside the band's extent");
}
```

Add the assertion the controller ruling requires — that the two layers agree
about which square a bearing/distance lands in. Without it, marks sit one cell
off their ground and nothing objects:

```rust
#[test]
fn the_perception_overlay_lands_on_the_same_squares_as_the_terrain_raster() {
    // The terrain raster (bin/plate.rs, mesh-aware) and the perception overlay
    // (core/chart.rs, wire-only) are drawn by two crates that cannot share
    // code -- core carries no hornvale crate by design. So the ONE thing that
    // must agree between them is the projection, and nothing else in the build
    // checks it: a disagreement puts every mark one square off its ground and
    // leaves both suites green.
    let (lat, lon) = a_known_offset_from_the_observer();
    let from_raster = plate_square_for(lat, lon);
    let from_overlay = chart_square_for_bearing_distance(bearing_of(lat, lon), distance_of(lat, lon));
    assert_eq!(from_raster, from_overlay,
        "the terrain raster and the perception overlay disagree about which square holds {lat},{lon}");
}
```

- [ ] **Step 2: Run to verify it fails** — Expected: FAIL, holes present and the two layers disagree.

- [ ] **Step 3: Implement.** `render_surrounds_ascii` and `chart::draw` change
PROJECTION ONLY (both keep placing the packet's 31 cells); `plate.rs` gains
band B as a rung it already knows how to draw; `spread::compose` and
`world_plate_for_redraw` supply the plate at band B. **`surrounds_ascii.rs` and
`chart.rs` must move in the SAME commit** — a commit where only one has moved
leaves the pin red and is not a valid stopping point for review.

- [ ] **Step 4: Regenerate the reference fixture**

```bash
make rebaseline
git diff --stat -- clients/game/core/tests/fixtures/ book/ docs/
```

Branch table, not a prediction:
- **Only `chart-reference-seed-42.txt` and `docs/audits/` moved** → expected;
  commit with the code.
- **A `session-*.json` fixture moved** → expected only if the snapshot's chart
  changed; inspect and state why in the report.
- **Anything under `book/src/domesday/`, `book/src/gallery/`, or an almanac
  moved** → **STOP.** That is a determinism finding, not a rebaseline.

- [ ] **Step 5: Run the full pin**

Run: `cargo test -p hornvale-game-core --test suite > /tmp/hv-t6.log 2>&1; echo "exit=$?"` then grep.
Expected: PASS including `the_shape_matches_the_sims_own_ascii_render`.
**Dispose H2 in the report.**

- [ ] **Step 6: `cargo fmt --all`, `make gate-commit`, commit**

---

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
