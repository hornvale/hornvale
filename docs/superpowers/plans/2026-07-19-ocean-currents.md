# The Gyre (Ocean Currents) Implementation Plan

> **Status: COMPLETE (2026-07-19).** All 6 tasks shipped + final-review fixes (animated advection per Nathan's G-call, re-seed opacity, legibility retune). See [chronicle](../../../book/src/chronicle/the-gyre.md) and [retro](../../retrospectives/the-gyre.md).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a derived ocean surface-current vector field (gyres from band winds × Coriolis × coastline) as an additive `scene/tiles/v1` layer, advected on the orrery's Living Globe.

**Architecture:** A pure `currents` module in `domains/climate` (Ekman-deflect the existing band wind, project along the coast, zero on land); the field precomputed in the provider and emitted as two additive per-cell scene components; the orrery reads them and advects particles over the ocean. Level 0 — the current touches no temperature, so worlds/census are byte-identical.

**Tech Stack:** Rust (edition 2024, `serde`+`serde_json`, `kernel::math` for trig), TypeScript/vite/three.js (orrery), cargo-nextest.

## Global Constraints

- **Byte-identical** worlds/ledger/census: the current field is a pure derived read that touches NO temperature (level 0). `mean_temperature`, biomes, census unchanged. No epoch, no new stream draws, no reordered labels.
- **Zero on land; requires bands.** Land cells → `[0,0,0]`. A locked world (no circulation bands) has a zero current field.
- **All transcendentals via `hornvale_kernel::math`** (0041) — never raw `f64::sin`/`cos`/`sqrt`... (`.sqrt()` on f64 is allowed — it is not a libm transcendental banned by clippy.toml; mirror `wind_east_tangent`'s existing `.sqrt()`).
- **Quantize at emit only** (0033): new scene floats use `quantize_serde::vec_f64_field`.
- **`scene/tiles/v1` stays v1** (additive fields); its byte-pin `windows/scene/tests/fixtures/tiles-seed-1-w16.json` **re-pins in the same commit** (`REBASELINE=1 cargo test -p hornvale-scene --test golden`).
- **No `HashMap`/`HashSet`**; `#![warn(missing_docs)]`; type-audit tags on new pub-boundary primitives; `cargo fmt` before every commit.
- **No golden** for the client: it PARSES the current vectors (recomputes nothing), so the vendored-binary fixture test is the whole contract (the parse-vs-recompute rule).
- **No `CLIM-operators`/Field-trait refactor, no SST feedback, no seasonal currents, no winds change** (spec §6 non-goals).

---

### Task 1: The `currents` module (pure model)

**Files:**
- Create: `domains/climate/src/currents.rs`
- Modify: `domains/climate/src/lib.rs` (`mod currents;` + re-exports)

**Interfaces:**
- Consumes: `circulation::prevailing_wind(geo, cell, bands)`, `circulation::wind_east_tangent(geo, cell)`, `geo.position(cell)`, `geo.coord(cell).latitude`, `geo.neighbors(cell) -> &[CellId]`.
- Produces:
  - `pub fn ocean_current(geo: &Geosphere, is_ocean: &dyn Fn(CellId) -> bool, cell: CellId, bands: u32) -> [f64; 3]` — the tangent current vector at an ocean cell; `[0,0,0]` if `!is_ocean(cell)`.
  - `pub fn ocean_current_field(geo: &Geosphere, is_ocean: &dyn Fn(CellId) -> bool, bands: Option<u32>) -> CellMap<[f64; 3]>` — the whole field; all-zero when `bands` is `None` (locked).

**The model (implement exactly):**
```
// ocean_current(geo, is_ocean, cell, bands):
//   if !is_ocean(cell) { return [0,0,0]; }
//   let wind = prevailing_wind(geo, cell, bands);           // tangent band wind
//   if wind == [0,0,0] { return [0,0,0]; }                  // poles: east undefined
//   let up = normalize(geo.position(cell));                 // outward radial
//   // Ekman deflection: rotate `wind` about `up` by a hemisphere-signed angle.
//   // Northern hemisphere deflects RIGHT (toward the equator for an easterly),
//   // southern deflects LEFT. Right = clockwise viewed from outside = NEGATIVE
//   // rotation about the outward `up`. Rodrigues for v ⊥ up:
//   //   rot(v, θ) = v*cosθ + (up × v)*sinθ
//   let hemi = geo.coord(cell).latitude;                    // >0 N, <0 S
//   let sign = if hemi >= 0.0 { -1.0 } else { 1.0 };        // N: right (−), S: left (+)
//   let theta = sign * DEFLECT_RAD;                         // DEFLECT_RAD = 45° in radians
//   let deflected = add( scale(wind, cos(theta)), scale(cross(up, wind), sin(theta)) );
//   // Coastline projection: remove the component pointing INTO land.
//   //   toward_land = normalize( tangent-projected sum of (neighbor.pos − pos)
//   //                            over land neighbors );  (zero if no land neighbor)
//   //   if toward_land != 0: deflected -= max(0, dot(deflected, toward_land)) * toward_land
//   // (keeps along-shore + away-from-land flow, suppresses into-land → boundary currents)
//   return deflected;
// constant: DEFLECT_RAD = std::f64::consts::FRAC_PI_4 (45°).
// helpers: normalize, add, scale, dot, cross (mirror circulation.rs's `cross`;
//   tangent-project a vector w at cell: w - dot(w, up)*up).
```

- [ ] **Step 1: Write the failing tests** in `currents.rs` (`#[cfg(test)] mod tests`):

```rust
use super::*;
use hornvale_kernel::{CellId, Geosphere};

fn all_ocean(_: CellId) -> bool { true }
fn no_ocean(_: CellId) -> bool { false }

// Land cells carry no current.
#[test]
fn land_cells_are_zero() {
    let geo = Geosphere::new(4);
    for cell in geo.cells() {
        let c = ocean_current(&geo, &no_ocean, cell, 3);
        assert_eq!(c, [0.0, 0.0, 0.0], "land current must be zero");
    }
}

// The current is a tangent field (orthogonal to the radial) and nonzero over
// open ocean away from the poles.
#[test]
fn current_is_tangent_and_nonzero_over_ocean() {
    let geo = Geosphere::new(5);
    let mut saw_nonzero = false;
    for cell in geo.cells() {
        let p = geo.position(cell);
        let c = ocean_current(&geo, &all_ocean, cell, 3);
        let radial = c[0]*p[0] + c[1]*p[1] + c[2]*p[2];
        assert!(radial.abs() < 1e-9, "current must be tangent; radial={radial}");
        if c[0]*c[0]+c[1]*c[1]+c[2]*c[2] > 1e-6 { saw_nonzero = true; }
    }
    assert!(saw_nonzero, "open ocean should carry a current somewhere");
}

// Hemisphere-correct rotation: the current is deflected to the RIGHT of the
// wind in the N hemisphere (and left in S). "Right" = the deflected vector has
// a negative component along (up × wind) in the north. This is the
// gyres-spin-the-right-way guard. (Mutation: flip `sign` → this fails.)
#[test]
fn deflection_is_rightward_in_the_north() {
    let geo = Geosphere::new(5);
    // pick an ocean cell at clearly-northern mid-latitude with a nonzero wind
    let cell = geo.cells().min_by(|a, b| {
        (geo.coord(*a).latitude - 40.0).abs().total_cmp(&(geo.coord(*b).latitude - 40.0).abs())
    }).unwrap();
    let wind = crate::prevailing_wind(&geo, cell, 3);
    let up = { let p = geo.position(cell); let l=(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]).sqrt(); [p[0]/l,p[1]/l,p[2]/l] };
    let left = [ up[1]*wind[2]-up[2]*wind[1], up[2]*wind[0]-up[0]*wind[2], up[0]*wind[1]-up[1]*wind[0] ]; // up × wind = "left"
    let c = ocean_current(&geo, &all_ocean, cell, 3);
    let along_left = c[0]*left[0] + c[1]*left[1] + c[2]*left[2];
    assert!(along_left < 0.0, "N-hemisphere current must deflect RIGHT (negative along up×wind); got {along_left}");
}

// A whole-locked world (no bands) has an empty field.
#[test]
fn locked_field_is_all_zero() {
    let geo = Geosphere::new(4);
    let field = ocean_current_field(&geo, &all_ocean, None);
    for cell in geo.cells() {
        assert_eq!(*field.get(cell), [0.0, 0.0, 0.0]);
    }
}
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-climate currents` → FAIL (module missing).
- [ ] **Step 3: Implement** `currents.rs` per the model block (module doc; one-line doc on each pub fn; type-audit tags mirroring `circulation.rs`'s `bare-ok(...)` style: `is_ocean` is a predicate — tag the numeric params/return `bare-ok(ratio)`/`bare-ok(count: bands)`). Add `mod currents; pub use currents::{ocean_current, ocean_current_field};` to `lib.rs`.
- [ ] **Step 4: Run to verify pass** — `cargo test -p hornvale-climate currents` → PASS (4 tests).
- [ ] **Step 5: Mutation-verify** the hemisphere guard — flip `sign` (or its comparison) so N deflects left; confirm `deflection_is_rightward_in_the_north` FAILS; revert.
- [ ] **Step 6: fmt + commit** — `feat(the-gyre): ocean-current module — Ekman-deflected, coast-projected, zero on land`.

> **Coast tangency** (spec §5) is exercised at the field level in Task 2's provider test (it needs a real terrain ocean mask); Task 1's pure module tests the deflection + zero-on-land + tangent + locked invariants.

---

### Task 2: Provider precompute + `current_at`

**Files:**
- Modify: `domains/climate/src/provider.rs` (precompute the field + a `current_at(cell)` accessor on the climate field object, mirroring `diurnal_amp_at`)

**Interfaces:**
- Consumes: Task 1's `ocean_current_field`; the terrain ocean predicate the provider already has access to at construction; `band_count()`.
- Produces: `pub fn current_at(&self, cell: CellId) -> [f64; 3]` on the climate field object.

- [ ] **Step 1: Write the failing test** (in `provider.rs` tests or the crate's integration tests — match where `diurnal_amp_at`/climate provider tests live):

```rust
// Coast tangency: an ocean cell adjacent to land has its into-land component
// suppressed — the current runs ALONG the coast, not into it. Build a real
// seed world, find an ocean cell with a land neighbor, and assert the current's
// component toward the mean land-neighbor direction is <= a small tolerance.
#[test]
fn coastal_current_does_not_flow_into_land() {
    // (Mirror an existing provider test's world build. Find a cell where
    //  is_ocean(cell) && some neighbor is land; compute toward_land as the
    //  tangent-projected, normalized mean of (neighbor.pos − pos) over land
    //  neighbors; assert dot(current_at(cell), toward_land) <= 1e-6.)
}
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-climate current` → FAIL.
- [ ] **Step 3: Implement** — precompute `current: CellMap<[f64;3]>` once in the provider via `ocean_current_field(geo, &|c| terrain.is_ocean(c), band_count_option)` (use the same `Option<u32>` band count the moisture/circulation path uses; `None` on locked). Store it; add `current_at`.
- [ ] **Step 4: Run to verify pass** — `cargo test -p hornvale-climate` → PASS.
- [ ] **Step 5: Byte-identity** — `cargo test -p hornvale --test lens_purity` (or the frozen seed-42 world test) passes UNCHANGED; `git status` shows no `cli/tests/fixtures/` diff. Currents touch no temperature, so the world is byte-identical. If a world fixture reddens, STOP — something coupled currents into the ledger.
- [ ] **Step 6: fmt + commit** — `feat(the-gyre): provider precomputes the current field + current_at`.

---

### Task 3: Scene fields + byte-pin re-pin + almanac line

**Files:**
- Modify: `windows/scene/src/lib.rs` (`current_east`/`current_north` + emit + type-audit tag + quantize serde)
- Modify: `windows/scene/tests/fixtures/tiles-seed-1-w16.json` (re-pin)
- Modify: the almanac window (a "the seas" line) + its test
- Modify: `windows/scene/src/lib.rs` tests (`.len()` asserts)

**Interfaces:**
- Consumes: Task 2's `climate.current_at(cell)`; the existing `wind_east_tangent` for the tangent frame.
- Produces: `TilesScene.current_east: Vec<f64>` + `current_north: Vec<f64>` — the tangent-frame components (`dot(current, east)`, `dot(current, north)`), quantized, zero on land, same tile order as `t_mean_c`.

- [ ] **Step 1: Write the failing test** in `windows/scene/src/lib.rs` tests (mirror the `t_diurnal_amp_c.len()` assert):

```rust
assert_eq!(scene.current_east.len(), tiles);
assert_eq!(scene.current_north.len(), tiles);
// finite everywhere; exactly zero on land tiles
for i in 0..tiles {
    assert!(scene.current_east[i].is_finite() && scene.current_north[i].is_finite());
    if !scene.ocean[i] {
        assert_eq!(scene.current_east[i], 0.0);
        assert_eq!(scene.current_north[i], 0.0);
    }
}
```

- [ ] **Step 2: Run to verify fail** — `cargo test -p hornvale-scene` → FAIL.
- [ ] **Step 3: Implement.** Add both `Vec<f64>` fields after `t_diurnal_amp_c` (each with the `vec_f64_field` serde attr + one-line doc). Append `bare-ok(diagnostic-value: current_east), bare-ok(diagnostic-value: current_north)` to the struct's `type-audit:` tag. In the builder, per tile compute the tangent frame — `east = wind_east_tangent(geo, c_cell)`, `north = cross(position, east)` normalized (or `cross(up, east)`) — and push `dot(current_at(c_cell), east)` / `dot(..., north)`; add both to the finite-check `.chain(...)` and the struct literal. (On land `current_at` is `[0,0,0]` → both components 0.)
- [ ] **Step 4: Run** — `cargo test -p hornvale-scene` (the `v1_bytes_are_pinned` golden WILL fail — expected).
- [ ] **Step 5: Re-pin** — `REBASELINE=1 cargo test -p hornvale-scene --test golden` rewrites `tiles-seed-1-w16.json`; inspect the diff — ONLY `current_east`/`current_north` arrays added, every other field byte-identical (if anything else moved, STOP). Re-run → PASS. Re-pin lands in THIS commit.
- [ ] **Step 6: Almanac "the seas" line** — at a coastal ocean sample site, a one-line note naming the dominant offshore current direction (cardinal from the `current_east`/`current_north` sign, e.g. "a current runs north along the western shore"). A unit test asserts the line is present on a spinning world with ocean and absent on a locked world (no current field).
- [ ] **Step 7: Regenerate artifacts** — `bash scripts/regenerate-artifacts.sh`; `git add` the changed `book/src/gallery/` scene/almanac pages; `git status` clean.
- [ ] **Step 8: fmt + type-audit + commit** — run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` (exit 0) AND regenerate its report if pub items changed (`... -- report > docs/audits/type-audit-report.md`); commit `feat(the-gyre): scene/tiles/v1 current_east/current_north + almanac the-seas (re-pin)`.

---

### Task 4: world-wasm binary handoff (no golden — client parses)

**Files:** (handoff only — build + copy)

- [ ] **Step 1** Build the release wasm from this worktree:
  `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release`.
- [ ] **Step 2** Copy it to the orrery worktree (dev-loop handoff; the v9 RELEASE is a G6 action — do NOT touch package.json/tags/URLs):
  `cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm /Users/nathan/.config/superpowers/worktrees/orrery/the-gyre/public/hornvale_world.wasm` (mkdir the `public/` dir if absent). Confirm `ls -la` (paste the timestamp) and `strings <dest> | grep -c current_east` ≥ 1.
- [ ] **Step 3** No hornvale commit for this task (build artifact only). If the orrery worktree does not exist yet, STOP and tell the controller (it is created before Task 5).

> **No golden** this campaign — the client reads `current_east`/`current_north` and advects them; it recomputes nothing, so the vendored-binary fixture test (Task 5) is the whole contract.

---

### Task 5: Orrery — parse currents + advection overlay + HUD toggle

**Files (orrery worktree `~/.config/superpowers/worktrees/orrery/the-gyre`):**
- Modify: `src/sim/scene.ts` (`TilesScene` interface + `parseTiles`: `currentEast`/`currentNorth`)
- Create: `src/views/currents.ts` (the ocean-current advection overlay, sibling to `src/views/winds.ts`) + `src/views/currents.test.ts`
- Modify: `src/ui/hud.ts` (a `currents` toggle beside `winds`), `src/main.ts` / `src/views/globe.ts` (mount the overlay, wire the toggle)
- Modify: `src/sim/scene.test.ts`, `src/sim/catalogFixture.test.ts`

**Interfaces:**
- Consumes: the Task-4 binary at `public/hornvale_world.wasm` (verify `ls -la` is today; if not, STOP — Task 4 first).
- Produces: `parseTiles` result gains `currentEast: number[]`, `currentNorth: number[]`; a `createCurrents(tiles, radius)` overlay returning `null` on a locked world (no current field / all-zero).

- [ ] **Step 1: Failing tests.** `scene.test.ts`: a tiles doc with `current_east`/`current_north` round-trips to `currentEast`/`currentNorth` (length = width·height; wrong length throws). `currents.test.ts`: the pure geometry — a helper that maps a tile's `(currentEast, currentNorth)` + lat/lon into a world-space tangent advection vector returns zero on a land/zero tile and nonzero on an ocean tile (mirror how `winds.ts`/`winds.test.ts` unit-test the arrow geometry without WebGL). `catalogFixture.test.ts`: the vendored binary's tiles carry `currentEast.length > 0` with at least one nonzero.
- [ ] **Step 2: Red** — `npx vitest run src/sim/scene.test.ts src/views/currents.test.ts`.
- [ ] **Step 3: Implement** the parser fields (strict, quantized-number, mirroring `t_diurnal_amp_c`) + `createCurrents` (study `src/views/winds.ts` and the globe's particle/overlay machinery; the current overlay seeds particles over OCEAN tiles only — use `tiles.ocean[i]` — and advects them along the tangent current; non-deterministic client eyecandy, decision 0022, so `Math.random` seeding is fine). Add the HUD `currents` toggle mirroring `winds`; mount+show/hide in globe/main like the winds overlay; `null` on locked worlds.
- [ ] **Step 4: Green** — `npm test` + `npx tsc --noEmit` (whole suite; the new binary feeds every fixture test — a red elsewhere is a real regression).
- [ ] **Step 5: Commit** — `feat(the-gyre): parse current field + Living-Globe current advection + toggle`.

---

### Task 6: Assembly + visual verification + G6 package (controller-run)

- [ ] Hornvale worktree: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean; `mdbook build book`.
- [ ] Orrery worktree: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e — all green with the Task-4 binary in `public/`.
- [ ] **Visual verification (The Lens rule — it caught a physics bug last campaign):** serve the built orrery, seed 42, toggle **currents**, and screenshot — confirm the flow forms **gyres rotating clockwise in the north / counter-clockwise in the south**, **hugs coasts** (boundary currents), stays **on the water**, and **vanishes on a locked seed**. Open the PNGs yourself — a hemisphere-sign error passes every unit test and only shows as backwards gyres.
- [ ] Absorb main on both repos if it moved (regenerate `type-audit-report.md` on conflict; hand-resolve semantic collisions); conflict-marker sweep; followup register updated; G6 package assembled (ledger digest with the save-format entry leading, screenshots, gate evidence, world-wasm-v9 release + orrery re-pin + push plan for Nathan's carve-out approvals).
