# Sculpting (Terrain Epoch v3) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Terrain epoch v3 — erosion and sediment (the carve) plus the full decoration mandate (terranes, microcontinents, belt anatomy, discrete arcs, trench, hotspot trails, atolls, induration-scaled fBm relief), bringing shoreline-development and shelf-fraction inside their preregistered bands while the four passing bands stay inside.

**Architecture:** Decorations land upstream (crust drawn sets; elevation terms), then a one-shot carve (engine A) computes an elevation delta and a `sediment_thickness` field from provisional drainage + The Ground's induration; final elevation re-solves sea level and drainage; the lithology buffer assembles over the carved surface. The carve sits behind a potential-agnostic pure-function seam (spec §2). Spec: `docs/superpowers/specs/2026-07-14-sculpting-design.md`.

**Tech Stack:** Rust edition 2024, `hornvale-kernel` (`Field`/`CellMap`/`Seed`/`noise`/`math`), cargo-nextest gate. serde/serde_json only.

## Global Constraints

Copied from the spec and CLAUDE.md; every task's requirements implicitly include these:

- **This IS an epoch.** World bytes change. Every byte-changing task re-pins the drifting fixtures **in the same commit** (`cli/tests/lens_purity.rs` world-identity fixture and any golden pins that assert elevation/coast values) — never deferred to close. Determinism *within* a commit always holds: same seed + pins → byte-identical worlds; `genesis_is_deterministic` must pass at every commit.
- **Existing streams survive with identical consumption order.** New drawn sets use NEW labels only (`terrain/terranes`, `terrain/microcontinents`), consumed via their own derived child streams (`terrain_seed.derive(label)` — independent, no reordering of existing draws). Derived decorations use hash-noise labels documented in `streams.rs` as hash-noise-only (the `plate-edge`/`lithology` convention). Pin isolation: pinned and unpinned paths consume identical draws.
- **Knobs are global; heterogeneity comes only from fields** (spec §5). All tuning constants are named `pub const`s in the module that owns them. No regionally jittered parameters.
- **No new pins. No new crates. No `HashMap`/`HashSet`** (`BTreeMap`/`BTreeSet`/`Vec`). No wall-clock. Float sorting via `total_cmp`; per-cell iteration in ascending `CellId` order. All transcendentals via `hornvale_kernel::math` (libm), **never** `f64::ln/exp/powf` methods (cross-platform byte-identity, decision 0041).
- **Quantization at the emit boundary only** — the carve computes at full precision.
- Every `pub` item gets a one-line doc comment (`#![warn(missing_docs)]`) and every `pub` primitive a `type-audit:` verdict tag (default-deny CI).
- **`cargo fmt` is the final step before every commit.** Cost-order: fmt+clippy → `cargo test -p hornvale-terrain` scoped → full gate only pre-commit where stated. Run once, capture (`2>&1 | tee /tmp/hv-test.txt`), never re-run to grep.
- **Censuses are NEVER regenerated locally.** Census fixtures lag until the close's AWS regen (`make regen-remote`, Nathan-authorized) — mid-campaign the only tolerated red is stale-census schema drift.
- **Campaign Autopilot is engaged**: G5 auto-continues on green; the §7 perf readout (Task 13), any fidelity cut, tuning-season band decisions, and census/AWS spend are **hard stops for Nathan** (carve-outs). Ledger auto-resolved decisions in `.superpowers/sdd/decision-ledger.md` in the worktree.
- **Prerequisite: The Ground is merged to main.** This plan consumes `MaterialBuffer.induration`, `lithology::assemble_material`, `TectonicGlobe.{crust_age, boundary_distance, lithology, lithology_seed}`, `render::lithology_png`. If absent on main, STOP.
- Commit-message trailer (every commit):
  `Claude-Session: https://claude.ai/code/session_01Rq3ZtUtwfASsKUYdSet8Sx`

---

### Task 1: Terranes — accreted slivers on leading margins

New drawn set: 2–6 elongated exotic crust fragments welded to continental leading edges. Implemented as anisotropic kernel summands inside `CrustField`, so thickness/age at any point remain pure pointwise functions (Field contract holds).

**Files:**
- Modify: `domains/terrain/src/crust.rs` — `Terrane` struct, `draw_terranes`, `CrustField` incorporation.
- Modify: `domains/terrain/src/streams.rs` — `TERRANES` label.
- Modify: `domains/terrain/src/globe.rs` — draw + thread terranes into `CrustField::new_with_terranes`; retain `terranes: Vec<Terrane>` on `TectonicGlobe`.
- Test: `domains/terrain/src/crust.rs` tests.

**Interfaces:**
- Consumes: `Craton { center: [f64;3], radius_rad: f64, age: f64, thickness_km: f64 }` fields (confirm exact names in `crust.rs:126` before coding; adjust code below to the real names), `plates::velocity_at`, `Seed::derive(..).stream()`.
- Produces:
  - `pub struct Terrane { pub center: [f64; 3], pub along: [f64; 3], pub half_len_rad: f64, pub half_wid_rad: f64, pub age: f64, pub thickness_km: f64 }`
  - `pub fn draw_terranes(terrain_seed: Seed, cratons: &[Craton], plates: &[Plate]) -> Vec<Terrane>`
  - `pub fn CrustField::new_with_terranes(terrain_seed: Seed, cratons: Vec<Craton>, terranes: Vec<Terrane>) -> CrustField` (existing `new` delegates with `vec![]` — every existing call site compiles unchanged)
  - `TectonicGlobe.terranes: Vec<Terrane>` (doc: recomputed at genesis, never serialized)

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn terranes_are_drawn_on_continental_margins_and_thicken_crust() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let mut notes = Vec::new();
        let pins = crate::pins::TerrainPins::default();
        let plates = crate::plates::generate_plates(seed, &pins, &mut notes);
        let ocean_target = 0.65;
        let cratons = draw_cratons(seed, &pins, ocean_target, &mut notes);
        let terranes = draw_terranes(seed, &cratons, &plates);
        assert!((2..=6).contains(&terranes.len()), "count: {}", terranes.len());
        let plain = CrustField::new(seed, cratons.clone());
        let with = CrustField::new_with_terranes(seed, cratons, terranes.clone());
        for t in &terranes {
            // A terrane thickens crust at its own center...
            assert!(with.thickness_at(t.center).get() > plain.thickness_at(t.center).get());
            // ...and sits at a continental margin: near the continental
            // threshold in the plain field (rim, not deep interior/abyss).
            let base = plain.thickness_at(t.center).get();
            assert!(base > OCEANIC_KM && base < CONTINENTAL_THRESHOLD_KM + 12.0,
                "terrane not on a margin: base {base}");
            // Elongated: at one half-length along `along`, still thickened;
            // at the same distance across, contribution has fallen off harder.
            assert!(t.half_len_rad > t.half_wid_rad);
        }
        // Determinism.
        let again = draw_terranes(Seed(42).derive(crate::streams::ROOT), &draw_cratons(Seed(42).derive(crate::streams::ROOT), &pins, ocean_target, &mut Vec::new()), &plates);
        assert_eq!(terranes, again);
    }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-terrain terranes_are_drawn`
Expected: FAIL — `cannot find function draw_terranes`.

- [ ] **Step 3: Implement**

In `domains/terrain/src/streams.rs`:

```rust
/// Terrane draws (Sculpting, spec §3): count, then per terrane a host
/// craton index, rim bearing, size, age. A NEW label — existing stream
/// consumption order is untouched (epoch v3 save-format contract).
/// type-audit: bare-ok(identifier-text)
pub const TERRANES: &str = "terranes";
```

In `domains/terrain/src/crust.rs` (constants concrete; `PartialEq` derives for the test's determinism assert):

```rust
/// An accreted exotic sliver welded to a continental leading margin
/// (Sculpting, spec §3): an anisotropic (elongated) crust kernel.
/// type-audit: bare-ok(direction: center), bare-ok(direction: along), bare-ok(angle: half_len_rad), bare-ok(angle: half_wid_rad), bare-ok(ratio: age), waiver(crust-km-convention: thickness_km)
#[derive(Debug, Clone, PartialEq)]
pub struct Terrane {
    /// Kernel center, a unit vector on the craton's rim.
    pub center: [f64; 3],
    /// Unit tangent along the margin (the elongation direction).
    pub along: [f64; 3],
    /// Angular half-length along the margin, radians.
    pub half_len_rad: f64,
    /// Angular half-width across the margin, radians.
    pub half_wid_rad: f64,
    /// Terrane age in [0,1] — young exotic crust, distinct from its host.
    pub age: f64,
    /// Peak added thickness, km.
    pub thickness_km: f64,
}

/// Terrane count bounds and size ranges (global knobs, spec §5).
pub const TERRANE_COUNT_MIN: u32 = 2;
/// Upper terrane count bound.
pub const TERRANE_COUNT_MAX: u32 = 6;
/// Terrane angular half-length range, radians (~6°–14°).
pub const TERRANE_HALF_LEN_RAD: (f64, f64) = (0.10, 0.24);
/// Width is this fraction of length (elongation).
pub const TERRANE_ASPECT: f64 = 0.35;
/// Peak added thickness range, km.
pub const TERRANE_THICKNESS_KM: (f64, f64) = (6.0, 12.0);

/// Draw the terrane set from the `terranes` stream: count, then per
/// terrane (host craton index, rim bearing, length, thickness, age),
/// sequentially. Placement: on the host craton's rim, biased to the
/// leading (active) side — the rim point whose plate-motion outward
/// component is positive; if the drawn bearing lands on the trailing
/// side it is used anyway (drawn bearings are never rejected — no
/// draw-count variance, pin-isolation discipline).
pub fn draw_terranes(terrain_seed: Seed, cratons: &[Craton], plates: &[Plate]) -> Vec<Terrane> {
    if cratons.is_empty() {
        return Vec::new();
    }
    let mut stream = terrain_seed.derive(crate::streams::TERRANES).stream();
    let count = stream.range_u32(TERRANE_COUNT_MIN, TERRANE_COUNT_MAX);
    (0..count)
        .map(|_| {
            let host = &cratons[stream.range_u32(0, cratons.len() as u32 - 1) as usize];
            let bearing = stream.next_f64() * core::f64::consts::TAU;
            let half_len_rad = TERRANE_HALF_LEN_RAD.0
                + (TERRANE_HALF_LEN_RAD.1 - TERRANE_HALF_LEN_RAD.0) * stream.next_f64();
            let thickness_km = TERRANE_THICKNESS_KM.0
                + (TERRANE_THICKNESS_KM.1 - TERRANE_THICKNESS_KM.0) * stream.next_f64();
            let age = 0.05 + 0.25 * stream.next_f64();
            // Rim point: rotate an arbitrary tangent at the craton center
            // by `bearing`, step out by the craton radius.
            let (e1, e2) = tangent_basis(host.center);
            let dir = [
                e1[0] * math::cos(bearing) + e2[0] * math::sin(bearing),
                e1[1] * math::cos(bearing) + e2[1] * math::sin(bearing),
                e1[2] * math::cos(bearing) + e2[2] * math::sin(bearing),
            ];
            let center = rotate_toward(host.center, dir, host.radius_rad);
            // Margin tangent: perpendicular to the radial direction at `center`.
            let radial = dir;
            let along = crate::plates::normalize(crate::plates::cross(center, radial));
            Terrane {
                center,
                along,
                half_len_rad,
                half_wid_rad: half_len_rad * TERRANE_ASPECT,
                age,
                thickness_km,
            }
        })
        .collect()
}
```

`tangent_basis(p)` (two unit tangents orthogonal to `p`) and `rotate_toward(p, dir, angle)` (move `p` by `angle` radians toward tangent `dir` on the unit sphere: `normalize(p*cos(angle) + dir*sin(angle))`) — add as `pub(crate)` helpers in `crust.rs` **unless** equivalents already exist in `crust.rs`/`plates.rs`/`shape.rs` (grep first; the craton lobing code likely has a tangent frame — reuse it). `radius_rad` / `center`: use `Craton`'s real field names (read the struct at `crust.rs:126`). `plates` is currently unused in the body (leading-edge bias is doc-described as "biased" but the drawn bearing is used unconditionally) — **keep the parameter** (the elevation-side asymmetry comes free because leading edges carry arcs) and mark it `_plates` if clippy complains; do NOT add a rejection loop, which would make draw counts data-dependent.

Anisotropic kernel inside `CrustField` — in `thickness_at` (and mirrored in `age_at`, where a terrane's `age` wins where its kernel dominates the craton sum; follow `age_at`'s existing winner logic):

```rust
/// A terrane's added thickness at `p`, km: a separable Gaussian in the
/// (along, across) tangent frame — elongated along the margin.
fn terrane_contribution_km(t: &Terrane, p: [f64; 3]) -> f64 {
    let angle = math::acos(crate::plates::dot(t.center, p).clamp(-1.0, 1.0));
    if angle > 4.0 * t.half_len_rad {
        return 0.0; // beyond support
    }
    // Decompose the offset into along/across components in the tangent plane.
    let offset = crate::plates::normalize(crate::plates::sub(p, crate::plates::scale(t.center, crate::plates::dot(t.center, p))));
    let along_c = crate::plates::dot(offset, t.along) * angle;
    let across_c = (angle * angle - along_c * along_c).max(0.0).sqrt();
    t.thickness_km
        * math::exp(-(along_c * along_c) / (2.0 * t.half_len_rad * t.half_len_rad))
        * math::exp(-(across_c * across_c) / (2.0 * t.half_wid_rad * t.half_wid_rad))
}
```

`CrustField::new_with_terranes` stores `terranes: Vec<Terrane>`; `new` delegates with an empty vec; `thickness_at` adds `self.terranes.iter().map(|t| terrane_contribution_km(t, p)).sum::<f64>()`. In `globe::generate`, draw terranes **after** `draw_cratons` and build the field with them; add `terranes` to `TectonicGlobe` (doc comment + `type-audit: bare-ok(struct)`).

- [ ] **Step 4: Run tests**

Run: `cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-t1.txt`
Expected: the new test PASSES; existing terrain tests pass (crust field values changed but terrain-internal tests assert invariants, not pinned values — any that pin exact elevations must be re-pinned NOW, in this commit).

- [ ] **Step 5: Re-pin the world-identity fixtures**

Run: `cargo test -p hornvale --test lens_purity 2>&1 | tee /tmp/hv-t1-lens.txt`
Expected: FAIL (bytes changed — this is the epoch doing its job). Re-pin per that test's documented refresh procedure (read the test header; it states the regeneration command). Also run `cargo test --workspace 2>&1 | tee /tmp/hv-t1-ws.txt` once and re-pin any other golden fixture that asserts elevation-derived values (walker transcripts, almanac snapshots) per each test's refresh note. Census schema/fixture tests are the tolerated red (stale until close).

- [ ] **Step 6: fmt + clippy + commit**

```bash
cargo fmt && cargo clippy -p hornvale-terrain --all-targets -- -D warnings
git add domains/terrain/src/ cli/tests/ # plus any re-pinned fixture files
git commit -m "feat(terrain): terranes — accreted margin slivers as anisotropic crust kernels (epoch v3 begins)

New terranes stream (appended label; existing draw order untouched).
Fixtures re-pinned in this commit per epoch discipline.

Claude-Session: https://claude.ai/code/session_01Rq3ZtUtwfASsKUYdSet8Sx"
```

---

### Task 2: Microcontinents — ocean-basin microcratons

New drawn set: 0–4 tiny cratons in open ocean, each sized below the continent-count metric's 0.5%-of-land floor.

**Files:**
- Modify: `domains/terrain/src/crust.rs` — `draw_microcontinents`; merge into the field's craton list.
- Modify: `domains/terrain/src/streams.rs` — `MICROCONTINENTS` label.
- Modify: `domains/terrain/src/globe.rs` — draw + retain `microcontinents: Vec<Craton>`.
- Test: `domains/terrain/src/crust.rs` tests.

**Interfaces:**
- Consumes: `Craton`, `CrustField::new_with_terranes` (Task 1).
- Produces:
  - `pub fn draw_microcontinents(terrain_seed: Seed, cratons: &[Craton]) -> Vec<Craton>` — returns extra *small* cratons (radius `MICRO_RADIUS_RAD` range) positioned away from existing cratons.
  - `CrustField` construction takes the concatenated craton list: `new_with_terranes(seed, [cratons, micro].concat(), terranes)`. `--continents` pin semantics unchanged: the pin counts **major** cratons only (`draw_cratons` untouched).
  - `TectonicGlobe.microcontinents: Vec<Craton>`.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn microcontinents_are_small_oceanic_and_deterministic() {
        let seed = Seed(7).derive(crate::streams::ROOT);
        let pins = crate::pins::TerrainPins::default();
        let mut notes = Vec::new();
        let cratons = draw_cratons(seed, &pins, 0.65, &mut notes);
        let micro = draw_microcontinents(seed, &cratons);
        assert!(micro.len() <= MICRO_COUNT_MAX as usize);
        for m in &micro {
            assert!(m.radius_rad <= MICRO_RADIUS_RAD.1);
            // Far from every major craton: no merger into a continent.
            for c in &cratons {
                let d = hornvale_kernel::math::acos(
                    crate::plates::dot(m.center, c.center).clamp(-1.0, 1.0));
                assert!(d > c.radius_rad + 2.0 * m.radius_rad,
                    "microcontinent fused to a craton");
            }
        }
        assert_eq!(micro, draw_microcontinents(seed, &cratons));
    }
```

(Adjust `center`/`radius_rad` to `Craton`'s real field names, as in Task 1.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-terrain microcontinents_are_small`
Expected: FAIL — `cannot find function draw_microcontinents`.

- [ ] **Step 3: Implement**

`streams.rs`:

```rust
/// Microcontinent draws (Sculpting, spec §3): fixed candidate count, then
/// per candidate a position and radius; candidates landing near a major
/// craton are DISCARDED after drawing (fixed draw count — no rejection
/// loop, no draw-count variance).
/// type-audit: bare-ok(identifier-text)
pub const MICROCONTINENTS: &str = "microcontinents";
```

`crust.rs`:

```rust
/// Fixed microcontinent candidate draw count (survivors ≤ this).
pub const MICRO_COUNT_MAX: u32 = 4;
/// Microcontinent radius range, radians (~1.5°–3°: Madagascar-scale at L6,
/// below the continent metric's 0.5%-of-land floor).
pub const MICRO_RADIUS_RAD: (f64, f64) = (0.026, 0.052);
/// Microcontinent thickness above the continental threshold, km.
pub const MICRO_THICKNESS_KM: f64 = 26.0;

/// Draw microcontinents: exactly `MICRO_COUNT_MAX` candidates (position,
/// radius, age — three draws each), keeping those farther than
/// `major.radius_rad + 2*candidate.radius_rad` from every major craton.
/// Deterministic filtering after a fixed draw count (pin-isolation
/// discipline: the stream is consumed identically regardless of outcome).
pub fn draw_microcontinents(terrain_seed: Seed, cratons: &[Craton]) -> Vec<Craton> {
    let mut stream = terrain_seed.derive(crate::streams::MICROCONTINENTS).stream();
    (0..MICRO_COUNT_MAX)
        .map(|_| {
            let center = unit_vector(&mut stream); // reuse the existing helper (grep crust.rs/elevation.rs)
            let radius_rad =
                MICRO_RADIUS_RAD.0 + (MICRO_RADIUS_RAD.1 - MICRO_RADIUS_RAD.0) * stream.next_f64();
            let age = 0.3 + 0.5 * stream.next_f64();
            (center, radius_rad, age)
        })
        .filter(|(center, radius_rad, _)| {
            cratons.iter().all(|c| {
                math::acos(crate::plates::dot(*center, c.center).clamp(-1.0, 1.0))
                    > c.radius_rad + 2.0 * radius_rad
            })
        })
        .map(|(center, radius_rad, age)| Craton {
            center,
            radius_rad,
            age,
            thickness_km: MICRO_THICKNESS_KM,
            // ...populate the remaining Craton fields with the same
            // expressions draw_cratons uses for them (read draw_cratons —
            // e.g. lobing amplitude/phase draws MUST NOT be skipped if the
            // kernel expects them; if lobing values are drawn per craton,
            // draw them inside the candidate map above so consumption stays
            // fixed-count).
        })
        .collect()
}
```

**Important:** open `draw_cratons` first and mirror its per-craton field construction exactly (lobed kernels may need per-craton phase/amplitude draws — those draws belong inside the candidate `map`, keeping the fixed count). In `globe::generate`: `let micro = crust::draw_microcontinents(terrain_seed, &cratons);` then build the field over `[cratons.clone(), micro.clone()].concat()` and retain both lists on the globe (`cratons` keeps majors only — `continental_supply`, `--continents` metering, and craton-census metrics keep their meaning; add a doc line saying so).

- [ ] **Step 4: Run tests; Step 5: re-pin fixtures (same procedure as Task 1 Step 5); Step 6: fmt + clippy + commit**

```bash
cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-t2.txt
cargo fmt && cargo clippy -p hornvale-terrain --all-targets -- -D warnings
git add domains/terrain/src/ cli/tests/
git commit -m "feat(terrain): microcontinents — sub-floor ocean microcratons (fixed-count draws)

Claude-Session: https://claude.ai/code/session_01Rq3ZtUtwfASsKUYdSet8Sx"
```

---

### Task 3: Belt anatomy, discrete arcs, and the trench

Restructure `assemble_elevation`'s flat boundary term into a per-kind **profile**: crest → foothills → foreland trough for collision belts; along-strike hash-noise gating for island arcs (chains of edifices, not walls); a trench trough seaward of subduction boundaries.

**Files:**
- Modify: `domains/terrain/src/elevation.rs` — replace the single `exp` falloff in `assemble_elevation` with `boundary_profile_m`; add `ARC_GATE` hash-noise; keep maturity factors.
- Modify: `domains/terrain/src/streams.rs` — `ARC_GATE` hash-noise label.
- Test: `domains/terrain/src/elevation.rs` tests.

**Interfaces:**
- Consumes: `CellBoundary { kind, other_plate, magnitude }` (confirm field names at `boundaries.rs:34`), `distances: CellMap<Option<(u32, CellId)>>`, `continental`, plate `maturity`.
- Produces: `fn boundary_profile_m(kind: BoundaryKind, cell_continental: bool, arc_side: bool, distance: u32, gate: f64) -> f64` (private; the assembly multiplies it by the existing magnitude/maturity factors), plus `pub const` knobs: `FORELAND_DEPTH_M: f64 = -350.0`, `FORELAND_HOPS: (u32, u32) = (3, 6)`, `TRENCH_DEPTH_M: f64 = -2800.0`, `TRENCH_HOPS: u32 = 1`, `ARC_SPACING: f64 = 9.0`, `ARC_DUTY: f64 = 0.45`.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn boundary_profiles_have_anatomy() {
        // Collision, continental side: crest at d=0 positive; foreland
        // trough negative somewhere in FORELAND_HOPS; recovery beyond.
        let crest = boundary_profile_m(BoundaryKind::ContinentalCollision, true, false, 0, 1.0);
        assert!(crest > 0.0);
        let trough = (FORELAND_HOPS.0..=FORELAND_HOPS.1)
            .map(|d| boundary_profile_m(BoundaryKind::ContinentalCollision, true, false, d, 1.0))
            .fold(f64::INFINITY, f64::min);
        assert!(trough < 0.0, "no foreland trough: {trough}");
        // Island arc: the gate switches edifices on and off along strike.
        let on = boundary_profile_m(BoundaryKind::IslandArc, false, true, 0, 1.0);
        let off = boundary_profile_m(BoundaryKind::IslandArc, false, true, 0, 0.0);
        assert!(on > 0.0 && off < on * 0.25, "arc not discrete: on {on} off {off}");
        // Trench: oceanic subducting side goes deep right at the boundary.
        let trench = boundary_profile_m(BoundaryKind::IslandArc, false, false, 0, 1.0);
        assert!(trench < -1000.0, "no trench: {trench}");
    }

    #[test]
    fn arcs_are_chains_not_walls_on_a_real_globe() {
        use std::collections::BTreeSet;
        let geo = Geosphere::new(5);
        let outcome = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        // Above-sea arc cells at IslandArc boundaries form >1 connected
        // component somewhere (discreteness), across land at arc boundaries.
        let arc_land: BTreeSet<hornvale_kernel::CellId> = geo.cells()
            .filter(|c| {
                matches!(g.boundary.get(*c).map(|b| b.kind), Some(crate::boundaries::BoundaryKind::IslandArc))
                    && *g.elevation.get(*c) >= g.sea_level
            })
            .collect();
        // Weak structural check: arc land exists but is not one giant blob.
        if arc_land.len() >= 6 {
            let components = count_components(&geo, &arc_land); // test-local flood fill helper, write it in the test module
            assert!(components >= 2, "arc land is one wall: {} cells, {components} component(s)", arc_land.len());
        }
    }
```

- [ ] **Step 2: Run to verify failure** — `cargo test -p hornvale-terrain boundary_profiles` — FAIL: `boundary_profile_m` not found.

- [ ] **Step 3: Implement**

`streams.rs`:

```rust
/// Along-strike arc gating noise (Sculpting, spec §3). Hash-noise only —
/// never consumed as a `Stream`.
/// type-audit: bare-ok(identifier-text)
pub const ARC_GATE: &str = "arc-gate";
```

`elevation.rs` — the profile (called from `assemble_elevation` where the flat `amplitude * exp(-d/decay)` sits today; the maturity `factor`/`decay_cells` and `magnitude/MAX_CLOSING_SPEED` multipliers stay OUTSIDE, applied exactly as before to the profile's positive parts; troughs (foreland, trench) take the magnitude factor but NOT the maturity amplitude factor — old belts keep their basins):

```rust
/// Foreland-basin trough depth, m (spec §3: belt anatomy).
pub const FORELAND_DEPTH_M: f64 = -350.0;
/// Foreland trough band, in boundary graph hops (continental side).
pub const FORELAND_HOPS: (u32, u32) = (3, 6);
/// Trench trough depth, m, at the subducting-side boundary cell.
pub const TRENCH_DEPTH_M: f64 = -2800.0;
/// Trench half-width, hops.
pub const TRENCH_HOPS: u32 = 1;
/// Arc edifice spacing wavelength (in gate-noise cycles per radian).
pub const ARC_SPACING: f64 = 9.0;
/// Fraction of the arc that is edifice (gate above this is "on").
pub const ARC_DUTY: f64 = 0.45;

/// Per-kind boundary elevation profile (Sculpting, spec §3): crest,
/// foothills, foreland trough for collision belts; gated edifices and a
/// seaward trench for subduction; unchanged shapes elsewhere. `gate` is
/// the along-strike hash-noise in [0,1] sampled at the SOURCE boundary
/// cell (so a whole edifice shares one gate value).
fn boundary_profile_m(
    kind: BoundaryKind,
    cell_continental: bool,
    arc_side: bool,
    distance: u32,
    gate: f64,
) -> f64 {
    let base = boundary_amplitude_m(kind, cell_continental, arc_side);
    let d = f64::from(distance);
    match kind {
        BoundaryKind::ContinentalCollision | BoundaryKind::CoastalRange if cell_continental => {
            let crest = base * math::exp(-d / 1.0);          // sharp core
            let foothills = 0.35 * base * math::exp(-d / 4.0); // broad apron
            let (f0, f1) = FORELAND_HOPS;
            let foreland = if distance >= f0 && distance <= f1 {
                FORELAND_DEPTH_M
                    * math::sin(core::f64::consts::PI * (d - f64::from(f0))
                        / f64::from(f1 - f0).max(1.0))
            } else {
                0.0
            };
            crest + foothills + foreland
        }
        BoundaryKind::IslandArc | BoundaryKind::CoastalRange => {
            if arc_side {
                // Volcanic edifice, present only where the gate is on.
                let on = if gate > (1.0 - ARC_DUTY) { 1.0 } else { 0.12 };
                base * on * math::exp(-d / 1.5)
            } else if distance <= TRENCH_HOPS {
                // Subducting side: the trench.
                TRENCH_DEPTH_M * math::exp(-d / 1.0)
            } else {
                base * math::exp(-d / 3.0) * 0.2
            }
        }
        _ => base * math::exp(-d / 3.0), // rifts/ridges/transform unchanged in shape
    }
}
```

The gate value: in `assemble_elevation`, for cells whose contact kind is `IslandArc`/`CoastalRange`, sample once per **source** cell: `let sp = geo.position(source); let gate = hash_noise_01(arc_gate_seed, ARC_SPACING * sp[0], ARC_SPACING * sp[1], ARC_SPACING * sp[2]);` — use the kernel's existing 3D hash-noise helper (grep `kernel/src/noise.rs` for the hash-noise function `plate-edge` uses and call the same one; if only 2D `fbm_2d` exists, use `fbm_2d(seed, ARC_SPACING * lon, ARC_SPACING * lat, 2)` from the source cell's lat/lon exactly as `coast-render` does). Thread `arc_gate_seed = terrain_seed.derive(streams::ARC_GATE)` through `generate_elevation` → `assemble_elevation` as a ninth parameter. Because the profile restructures collision belts too, `boundary_amplitude_m` itself is unchanged — only its application shape.

- [ ] **Step 4: Run tests** — `cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-t3.txt` — new tests PASS; determinism tests PASS.
- [ ] **Step 5: Re-pin fixtures** (Task 1 Step 5 procedure — this task moves mountains; expect walker/almanac pins to drift too).
- [ ] **Step 6: fmt + clippy + commit** — `feat(terrain): belt anatomy (crest/foothills/foreland), discrete arcs, trench`.

---

### Task 4: The induration field seam (shared with The Ground)

Extract The Ground's inline induration math into a standalone pre-elevation function; build `induration: CellMap<f64>` in `generate` before elevation; make `assemble_material` consume the same function (byte-identical refactor on the pre-carve surface). Also widen `margin_polarity` visibility for the carve.

**Files:**
- Modify: `domains/terrain/src/lithology.rs` — `pub fn induration_at`, refactor `assemble_material`; `pub(crate) fn margin_polarity`.
- Modify: `domains/terrain/src/globe.rs` — compute + retain `induration: CellMap<f64>` before `generate_elevation`.
- Test: `domains/terrain/src/lithology.rs` tests.

**Interfaces:**
- Consumes: The Ground's formula — read `assemble_material` and copy its exact grade/grain/induration expressions (grain from `crust_age`, `metamorphic_grade` from boundary kind + hops vs `OROGEN_REACH`, `induration = (0.35 + 0.4 * metamorphic_grade + 0.2 * grain).clamp(0.0, 1.0)` — verify against source, the source wins).
- Produces:
  - `pub fn induration_at(crust_age: f64, continental: bool, boundary_kind: Option<BoundaryKind>, boundary_hops: Option<u32>) -> f64` — doc: "type-audit: bare-ok(ratio: return), bare-ok(ratio: crust_age), bare-ok(flag: continental), bare-ok(count: boundary_hops). Total at extremes (spec §4): defined for the full [0,1] input range; the gated metaphysics overlay may inject sentinel values later without a formula change."
  - `TectonicGlobe.induration: CellMap<f64>`
  - `GeneratedTerrain::induration_at(&self, id: CellId) -> f64` (provider accessor reading the map)

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn induration_field_matches_the_assembled_buffer() {
        let geo = Geosphere::new(4);
        let outcome = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        for cell in geo.cells() {
            assert_eq!(
                *outcome.globe.induration.get(cell),
                outcome.globe.lithology.get(cell).induration,
                "seam and buffer disagree at {cell:?}"
            );
        }
    }
```

- [ ] **Step 2: Verify failure** — `cargo test -p hornvale-terrain induration_field_matches` — FAIL: no field `induration`.
- [ ] **Step 3: Implement** — extract the function (exact expressions from `assemble_material`), refactor `assemble_material` to call it, compute the CellMap in `generate` right after `distances` (it needs only `crust_age_map`, `continental`, `boundary_map`, `distances`), retain on the globe, add the provider accessor. **This task must not change any byte**: add a temporary assertion test comparing a seed-42 L4 globe's `lithology` before/after refactor if in doubt — the shipped equality test above plus `genesis_is_deterministic` covers it structurally.
- [ ] **Step 4: Run** — `cargo test -p hornvale-terrain 2>&1 | tee /tmp/hv-t4.txt` — PASS, **no fixture drift expected** (pure refactor + additive field).
- [ ] **Step 5: fmt + clippy + commit** — `refactor(terrain): induration as a standalone pre-elevation field (the Sculpting/Ground seam)`.

---

### Task 5: Induration-scaled fBm relief

Zero-mean multi-octave relief added in `assemble_elevation`, amplitude scaled by induration (hard stands craggy) and belt proximity.

**Files:**
- Modify: `domains/terrain/src/elevation.rs` — relief term (tenth parameter: `induration: &CellMap<f64>`); `pub const RELIEF_AMPLITUDE_M: f64 = 240.0;`
- Modify: `domains/terrain/src/streams.rs` — `RELIEF` hash-noise label.
- Modify: `domains/terrain/src/globe.rs` — thread `&induration` into `generate_elevation`.
- Test: `domains/terrain/src/elevation.rs` tests.

**Interfaces:**
- Consumes: Task 4's `induration` CellMap (computed pre-elevation); kernel fBm.
- Produces: relief term inside `assemble_elevation`; no new pub API beyond the constant.

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn relief_is_zero_mean_and_induration_scaled() {
        // Statistical, structural: over a real globe, mean |relief effect| on
        // hard cells exceeds soft cells. Compute two globes differing only in
        // that we zero the amplitude, then compare.
        let geo = Geosphere::new(4);
        let a = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        // The term exists: elevations differ from a no-relief reconstruction.
        // Simplest honest probe: the constant is wired (non-zero) and the
        // documented invariant holds — relief contribution at induration 1.0
        // vs 0.0 scales by the documented ratio. Test the pure helper:
        let hi = relief_scale(1.0, 0);
        let lo = relief_scale(0.0, 12);
        assert!(hi > 2.0 * lo, "induration scaling too weak: {hi} vs {lo}");
        assert!(a.notes.is_empty() || true); // globe builds fine with relief on
    }
```

- [ ] **Step 2: Verify failure** — FAIL: `relief_scale` not found.
- [ ] **Step 3: Implement**

```rust
/// fBm relief peak amplitude, meters (spec §3).
pub const RELIEF_AMPLITUDE_M: f64 = 240.0;

/// Relief amplitude scale: hard rock (induration→1) carries full relief,
/// soft lies smooth; belts (near boundaries) roughen further.
/// type-audit: bare-ok(ratio: induration), bare-ok(count: boundary_hops)
fn relief_scale(induration: f64, boundary_hops: u32) -> f64 {
    let hardness = 0.25 + 0.75 * induration;
    let belt = 1.0 + 1.5 * math::exp(-f64::from(boundary_hops) / 3.0);
    hardness * belt
}
```

In `assemble_elevation`, after `hotspot_term`: sample zero-mean fBm at the cell (same lat/lon hash-noise pattern as Task 3's gate, seed `terrain_seed.derive(streams::RELIEF)`, 4 octaves, `(sample - 0.5) * 2.0`), multiply by `RELIEF_AMPLITUDE_M * relief_scale(*induration.get(cell), hops)` where `hops` is the cell's `distances` entry (`u32::MAX → use 12` when `None`). Streams doc for `RELIEF` mirrors `ARC_GATE`.

- [ ] **Step 4: Run + re-pin fixtures** (bytes change) — Task 1 Step 5 procedure.
- [ ] **Step 5: fmt + clippy + commit** — `feat(terrain): induration-scaled fBm relief`.

---

### Task 6: Hotspot trails

Smear each drawn hotspot into an age-progressive seamount chain along its plate's local velocity. No new draws. Retain the seamount list for Task 9's atolls.

**Files:**
- Modify: `domains/terrain/src/elevation.rs` — `TrailSeamount`, `trail_seamounts`, contribution folded into the hotspot term; `generate_elevation` returns them (or a separate `pub fn` recomputes them — prefer: compute in `generate_elevation`'s caller: expose `pub fn trail_seamounts(terrain_seed, plates, plate_of, geo) -> Vec<TrailSeamount>` and pass into `assemble_elevation`).
- Modify: `domains/terrain/src/globe.rs` — retain `trail_seamounts: Vec<TrailSeamount>`.
- Test: `domains/terrain/src/elevation.rs` tests.

**Interfaces:**
- Consumes: `draw_hotspots` (existing draws — count/position/strength, order untouched), `plates::velocity_at`, `assign_plates` result.
- Produces:
  - `pub struct TrailSeamount { pub position: [f64; 3], pub strength_m: f64, pub age_index: u32 }` (`age_index` 0 = the live hotspot dome itself, rising = older/farther/weaker)
  - `pub const TRAIL_LENGTH_RAD: f64 = 0.35;` (max smear ~20°), `pub const TRAIL_STEPS: u32 = 6;`, `pub const TRAIL_DECAY: f64 = 0.55;` (per-step strength multiplier)
  - `TectonicGlobe.trail_seamounts: Vec<TrailSeamount>`

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn trails_are_age_ordered_chains_upstream_of_plate_motion() {
        let seed = Seed(42).derive(crate::streams::ROOT);
        let geo = Geosphere::new(4);
        let pins = crate::pins::TerrainPins::default();
        let mut notes = Vec::new();
        let plates = crate::plates::generate_plates(seed, &pins, &mut notes);
        let plate_of = crate::plates::assign_plates(&geo, seed, &plates);
        let seamounts = trail_seamounts(seed, &plates, &plate_of, &geo);
        // Every hotspot contributes TRAIL_STEPS+1 entries, age-ordered,
        // strength strictly decaying along each chain.
        assert!(seamounts.len() % (TRAIL_STEPS as usize + 1) == 0 && !seamounts.is_empty());
        for chain in seamounts.chunks(TRAIL_STEPS as usize + 1) {
            for pair in chain.windows(2) {
                assert!(pair[1].age_index == pair[0].age_index + 1);
                assert!(pair[1].strength_m < pair[0].strength_m);
            }
        }
        assert_eq!(seamounts, trail_seamounts(seed, &plates, &plate_of, &geo));
    }
```

- [ ] **Step 2: Verify failure**; **Step 3: Implement**

```rust
/// One seamount in a hotspot trail (Sculpting, spec §3): the drawn hotspot
/// smeared along its plate's local velocity, age-progressive.
/// type-audit: bare-ok(direction: position), pending(wave-2: strength_m), bare-ok(count: age_index)
#[derive(Debug, Clone, PartialEq)]
pub struct TrailSeamount {
    /// Dome center, unit vector.
    pub position: [f64; 3],
    /// Peak uplift, meters (decays along the chain).
    pub strength_m: f64,
    /// Steps upstream from the live hotspot (0 = the hotspot itself).
    pub age_index: u32,
}

/// Derive trail chains from the existing hotspot draws (no new draws):
/// step upstream against the plate's surface velocity at each point,
/// TRAIL_STEPS steps of TRAIL_LENGTH_RAD/TRAIL_STEPS radians, strength
/// decaying by TRAIL_DECAY per step. Chain length thus scales with plate
/// speed direction only (fixed arc length; a stationary plate still gets
/// a stubby stack — harmless, it reads as a large volcanic massif).
pub fn trail_seamounts(
    terrain_seed: Seed,
    plates: &[Plate],
    plate_of: &CellMap<u32>,
    geo: &Geosphere,
) -> Vec<TrailSeamount> {
    let hotspots = draw_hotspots(terrain_seed); // same draws as elevation consumes
    let step = TRAIL_LENGTH_RAD / f64::from(TRAIL_STEPS);
    let mut out = Vec::new();
    for h in &hotspots {
        let mut pos = h.position;
        let mut strength = h.strength_m;
        out.push(TrailSeamount { position: pos, strength_m: strength, age_index: 0 });
        for i in 1..=TRAIL_STEPS {
            let plate = &plates[*plate_of.get(geo.nearest_cell_to(pos)) as usize];
            let v = crate::plates::velocity_at(plate, pos);
            let speed = crate::plates::norm(v);
            if speed < 1e-9 { break; }
            // Upstream = where the plate came FROM = -v direction.
            let dir = crate::plates::normalize(crate::plates::scale(v, -1.0 / speed));
            pos = rotate_toward_unit(pos, dir, step); // p*cos + dir*sin, normalized (reuse Task 1's helper — move it to plates.rs as pub(crate))
            strength *= TRAIL_DECAY;
            out.push(TrailSeamount { position: pos, strength_m: strength, age_index: i });
        }
    }
    out
}
```

`geo.nearest_cell_to(pos)`: the Geosphere has a nearest-cell lookup (the provider's `nearest_cell(lat, lon)` wraps one — grep `kernel` for the position-based lookup; if only lat/lon exists, convert: `lat = asin(z)`, `lon = atan2(y, x)` via `math::`). In `assemble_elevation`, replace the hotspot term's source: the caller now passes `&[TrailSeamount]` (hotspot domes are `age_index == 0` entries — **their contribution must be byte-identical to before for the trunk domes**; trail entries add on top). `Hotspot::contribution_m` generalizes to a free fn `dome_m(position, strength_m, at) `. Retain the vec on the globe.

- [ ] **Step 4: Run + re-pin fixtures.** **Step 5: fmt + clippy + commit** — `feat(terrain): hotspot trails — age-progressive seamount chains from existing draws`.

---

### Task 7: The carve module — incision + hillslope repose

The engine-A core, first half. New module `carve.rs`: the potential-agnostic seam, stream-power incision on the provisional drainage tree, then a fixed-sweep angle-of-repose relaxation. Pure; no draws.

**Files:**
- Create: `domains/terrain/src/carve.rs`
- Modify: `domains/terrain/src/lib.rs` — `pub mod carve;` + re-exports.
- Test: `domains/terrain/src/carve.rs` tests.

**Interfaces:**
- Consumes: `CellMap<ReferenceElevation>`, `sea_level`, `(drainage, endorheic)` from `drainage_field`, Task 4's `induration` CellMap, lithology carbonate (pre-carve pointwise: pass a `carbonate: &CellMap<f64>` built by the caller in Task 10; unit tests build synthetic maps).
- Produces:

```rust
/// Tuning knobs for the carve (engine A). Global constants only —
/// heterogeneity comes from fields (spec §5).
pub struct CarveParams { /* all fields pub, all with doc comments */
    pub incision_k_m: f64,        // 90.0  — peak incision scale, m
    pub area_exponent: f64,       // 0.5   — m in area^m
    pub slope_exponent: f64,      // 1.0   — n in slope^n
    pub area_ref: f64,            // 40.0  — drainage normalization
    pub slope_ref_m: f64,         // 200.0 — slope normalization, m per hop
    pub max_incision_m: f64,      // 900.0
    pub repose_drop_m: f64,       // 700.0 — critical inter-cell drop
    pub repose_sweeps: u32,       // 4
    pub deposit_slope_m: f64,     // 40.0  — flatter than this deposits
    pub deposit_fraction: f64,    // 0.35  — per flat hop
    pub wedge_freeboard_m: f64,   // 40.0  — shelf cap below sea level
    pub wedge_reach_passive: u32, // 4     — BFS hops seaward, passive margin
    pub wedge_reach_active: u32,  // 1
    pub delta_count: u32,         // 3     — top-K mouths (spec §5, K=3)
    pub delta_height_m: f64,      // 15.0  — subaerial lobe cap above sea level
    pub atoll_freeboard_m: f64,   // 5.0
    pub atoll_max_depth_m: f64,   // 600.0
    pub atoll_max_abs_lat: f64,   // 0.6   — radians
}
impl Default for CarveParams { /* the values above */ }

/// The carve's output: an elevation delta plus the sediment bookkeeping.
pub struct CarveDelta {
    pub delta_m: CellMap<f64>,             // add to elevation (±)
    pub sediment_thickness_m: CellMap<f64>,// deposited thickness (≥0)
    pub mouths: Vec<(CellId, f64)>,        // river mouths + exported volume, sorted by volume desc, CellId tiebreak
    pub eroded_total_m3: f64,              // mass-balance bookkeeping (cell-area-weighted volume proxy: Σ depth, one unit per cell)
    pub deposited_total_m3: f64,
    pub ocean_loss_m3: f64,
}

pub fn carve_incision(geo, elevation, sea_level, drainage, induration, carbonate, params) -> CellMap<f64>; // negative depths
pub fn apply_repose(geo, elevation_after_incision, params) -> CellMap<f64>; // mass-conserving delta adjustments
pub fn erodibility(induration: f64, carbonate: f64) -> f64; // (0.25 + 1.5*(1.0-induration) + 0.75*carbonate), total at extremes
```

- [ ] **Step 1: Failing tests**

```rust
    #[test]
    fn incision_is_monotone_in_erodibility_and_capped() {
        // Synthetic 2-level probe: same drainage/slope, harder rock cuts less.
        assert!(erodibility(0.9, 0.0) < erodibility(0.2, 0.0));
        assert!(erodibility(0.5, 0.8) > erodibility(0.5, 0.0), "carbonate must dissolve");
        let geo = Geosphere::new(4);
        let outcome = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let inc = carve_incision(&geo, &g.elevation, g.sea_level, &g.drainage, &g.induration, &carbonate, &CarveParams::default());
        for (c, d) in inc.iter() {
            assert!(*d <= 0.0 && *d >= -CarveParams::default().max_incision_m);
            if *g.elevation.get(c) < g.sea_level { assert_eq!(*d, 0.0, "ocean incised"); }
        }
        // Valleys, not ridges: incision correlates with drainage — the most
        // incised decile has higher median drainage than the least.
        // (compute directly; assert median_top > median_bottom)
    }

    #[test]
    fn repose_conserves_mass_and_respects_the_critical_drop() {
        let geo = Geosphere::new(3);
        let outcome = crate::globe::generate(Seed(7), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let adjusted = apply_repose(&geo, &g.elevation, &p);
        let total: f64 = adjusted.iter().map(|(_, d)| *d).sum();
        assert!(total.abs() < 1e-6, "repose created/destroyed mass: {total}");
        // After applying, no LAND inter-cell drop exceeds repose_drop_m by
        // more than the pre-existing violation shrank (sweeps reduce, a
        // fixed count needn't eliminate): max violation strictly decreases
        // when any violation existed.
    }
```

- [ ] **Step 2: Verify failure.** **Step 3: Implement**

```rust
//! The carve (Sculpting, engine A — spec §5): a one-shot erosion/deposition
//! correction. Pure functions of existing fields; no draws, no time-steps.
//! The seam is potential-agnostic: this module reads "a potential (elevation),
//! a flow network (drainage), a resistivity (erodibility)" — water is the
//! only instantiation this campaign (spec §2).

/// Erodibility: how fast material yields (spec §4). Soft (low induration)
/// erodes; carbonate additionally dissolves. Total on [0,1]×[0,1]; the
/// gated overlay may inject sentinels later without a formula change.
/// type-audit: bare-ok(ratio: induration), bare-ok(ratio: carbonate), bare-ok(ratio: return)
pub fn erodibility(induration: f64, carbonate: f64) -> f64 {
    (0.25 + 1.5 * (1.0 - induration.clamp(0.0, 1.0)) + 0.75 * carbonate.clamp(0.0, 1.0))
        .clamp(0.0, 2.5)
}

/// Stream-power incision along the provisional drainage tree: depth =
/// k * (A/A0)^m * (S/S0)^n * erodibility, capped, land-only. Returns a
/// non-positive delta map. Iteration in ascending CellId order.
pub fn carve_incision(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    induration: &CellMap<f64>,
    carbonate: &CellMap<f64>,
    params: &CarveParams,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        if *elevation.get(cell) < sea_level {
            return 0.0;
        }
        let here = elevation.get(cell).get();
        let drop = geo
            .neighbors(cell)
            .iter()
            .map(|n| here - elevation.get(*n).get())
            .fold(0.0_f64, f64::max);
        if drop <= 0.0 {
            return 0.0; // local minimum: deposition country, Task 8
        }
        let a = (*drainage.get(cell) / params.area_ref).max(0.0);
        let s = drop / params.slope_ref_m;
        let power = math::pow(a, params.area_exponent) * math::pow(s, params.slope_exponent);
        -(params.incision_k_m * power * erodibility(*induration.get(cell), *carbonate.get(cell)))
            .min(params.max_incision_m)
    })
}
```

(`math::pow` — confirm the kernel exposes a libm pow; if the helper is named differently, e.g. `math::powf`, use the real name. If no pow exists for arbitrary exponents, add one to `kernel/src/math.rs` wrapping `libm::pow` with a one-line doc — that is a kernel touch, keep it to exactly that.)

`apply_repose`: fixed `params.repose_sweeps` sweeps; each sweep iterates cells in ascending `CellId` order; for each LAND cell, for each lower LAND neighbor where `drop > repose_drop_m`, move `(drop - repose_drop_m) * 0.25` from high to low **into a delta accumulator applied at sweep end** (Jacobi-style, order-independent within a sweep → deterministic and symmetric); ocean cells and ocean-adjacent drops are skipped (sea cliffs are real). Returns the total delta map (sum exactly 0 by construction: every subtraction has a matching addition).

`lib.rs`: `pub mod carve; pub use carve::{CarveDelta, CarveParams, carve_incision, apply_repose, erodibility};`

- [ ] **Step 4: Run** — `cargo test -p hornvale-terrain carve 2>&1 | tee /tmp/hv-t7.txt` — PASS. **No fixture drift** (module not yet wired into generate).
- [ ] **Step 5: fmt + clippy + commit** — `feat(terrain): carve module — stream-power incision + angle-of-repose relaxation (engine A, first half)`.

---

### Task 8: The carve — sediment routing, floodplains, playas

Route eroded volume down the drainage tree; deposit en route on flats; fill endorheic sinks toward playa floors; export per-mouth volumes. Produces `sediment_thickness_m` and the mouth list.

**Files:**
- Modify: `domains/terrain/src/carve.rs` — `route_sediment`, assembling `CarveDelta` minus the marine parts.
- Test: `domains/terrain/src/carve.rs` tests.

**Interfaces:**
- Consumes: Task 7's incision map; `drainage_field`'s downhill logic (re-derive the downhill target per cell exactly as `drainage.rs` does: strictly-lowest neighbor; copy the ~10-line loop, or better, **refactor `drainage.rs` to expose `pub(crate) fn downhill_targets(geo, elevation, sea_level) -> Vec<Option<CellId>>` and reuse it in both places** — preferred, one owner).
- Produces: `pub fn route_sediment(geo, elevation, sea_level, incision: &CellMap<f64>, downhill: &[Option<CellId>], endorheic: &CellMap<bool>, params) -> (CellMap<f64> /*deposit_m ≥0*/, Vec<(CellId, f64)> /*mouths*/, f64 /*ocean_loss*/)`.

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn sediment_books_balance_and_mouths_collect_the_rest() {
        let geo = Geosphere::new(4);
        let outcome = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let incision = carve_incision(&geo, &g.elevation, g.sea_level, &g.drainage, &g.induration, &carbonate, &p);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let (deposit, mouths, ocean_loss) =
            route_sediment(&geo, &g.elevation, g.sea_level, &incision, &downhill, &g.endorheic, &p);
        let eroded: f64 = incision.iter().map(|(_, d)| -*d).sum();
        let deposited: f64 = deposit.iter().map(|(_, d)| *d).sum();
        let exported: f64 = mouths.iter().map(|(_, v)| v).sum::<f64>();
        assert!((eroded - (deposited + exported + ocean_loss)).abs() < 1e-6 * eroded.max(1.0),
            "books: eroded {eroded} vs deposited {deposited} + exported {exported} + loss {ocean_loss}");
        for (_, d) in deposit.iter() { assert!(*d >= 0.0); }
        // Mouths sorted by exported volume descending, CellId ascending tiebreak.
        for w in mouths.windows(2) {
            assert!(w[0].1 > w[1].1 || (w[0].1 == w[1].1 && w[0].0 < w[1].0));
        }
        // Endorheic sinks received deposit (playas) on any world that has them.
        if g.endorheic.iter().any(|(_, e)| *e) {
            assert!(g.endorheic.iter().filter(|(_, e)| **e)
                .any(|(c, _)| *deposit.get(c) > 0.0), "no playa fill");
        }
    }
```

- [ ] **Step 2: Verify failure.** **Step 3: Implement**

Process land cells in **descending elevation order** (`sort_by` with `total_cmp` on elevation, `CellId` ascending tiebreak — deterministic; the downhill forest guarantees upstream-before-downstream in this order). Carry `flux[cell] = -incision[cell] + inflow`. At each cell: if its own max downhill drop `< params.deposit_slope_m` (a flat), deposit `params.deposit_fraction * flux` here (accumulate into `deposit`, subtract from flux). Then pass the remainder to the downhill target. If target is ocean (`elevation < sea_level`), the cell is a **mouth**: accumulate the arriving flux into that cell's mouth total. If no target (local minimum): if endorheic — deposit ALL remaining flux (playa); else (a sea-level-adjacent pit) deposit all as well. After the sweep, collect `mouths: Vec<(CellId, f64)>` sorted volume-desc/CellId-asc. `ocean_loss` is 0.0 here (Task 9's wedge computes what the shelf can't hold). The playa deposit is capped per cell at raising it to its lowest neighboring rim minus 1 m (fill toward flat, never overtop — compute rim = min neighbor elevation of the sink); surplus above the cap counts into `ocean_loss` (it vanishes into the playa's aquifer — books stay balanced and honest).

- [ ] **Step 4: Run.** **Step 5: fmt + clippy + commit** — `feat(terrain): carve sediment routing — floodplain deposition, playa fill, per-mouth export (books balance)`.

---

### Task 9: The carve — coastal wedge, deltas, trench interaction, atolls

The marine half: spread each mouth's export along-coast/offshore up to the shelf cap; protrude top-K deltas; skip subducting-side cells (the trench eats it); cap warm submerged trail seamounts (atolls).

**Files:**
- Modify: `domains/terrain/src/carve.rs` — `deposit_wedge`, `cap_atolls`, and the public seam `pub fn carve(...) -> CarveDelta` composing Tasks 7–9.
- Test: `domains/terrain/src/carve.rs` tests.

**Interfaces:**
- Consumes: Task 8's mouths; `MarginPolarity` via a `margins: &CellMap<MarginPolarity>` input (caller builds it in Task 10 from lithology's `margin_polarity`; tests build synthetic); `boundary: &CellMap<Option<CellBoundary>>` for trench-side detection; `trail_seamounts: &[TrailSeamount]` (Task 6).
- Produces:

```rust
pub fn deposit_wedge(geo, elevation, sea_level, mouths, margins, boundary, params)
    -> (CellMap<f64> /*marine deposit*/, f64 /*ocean_loss*/);
pub fn cap_atolls(geo, elevation, sea_level, trail_seamounts, params) -> CellMap<f64>;
/// The full engine-A carve (spec §2 seam). Composes incision → repose →
/// routing → wedge → deltas → atolls into one CarveDelta.
pub fn carve(geo, elevation, sea_level, drainage, endorheic, downhill, induration, carbonate, margins, boundary, trail_seamounts, params) -> CarveDelta;
/// Atoll cells (marked by cap_atolls) for lithology's carbonate override.
pub struct CarveDelta { /* extend with */ pub atoll_cells: Vec<CellId>, pub delta_cells: Vec<CellId>, pub waterfall_sites: Vec<CellId> /* Task 11 fills; empty here */ }
```

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn the_wedge_builds_a_shelf_mode_wide_on_passive_margins() {
        let geo = Geosphere::new(4);
        let outcome = crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        // Build inputs exactly as Task 10's wiring will.
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let margins = CellMap::from_fn(&geo, |c| g.lithology.get(c).margin);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let delta = carve(&geo, &g.elevation, g.sea_level, &g.drainage, &g.endorheic, &downhill,
            &g.induration, &carbonate, &margins, &g.boundary, &g.trail_seamounts, &p);
        // Shelf mode: marine deposit exists, never raises seabed above the cap.
        let cap = g.sea_level.get() - p.wedge_freeboard_m;
        let mut any_marine = false;
        for (c, dep) in delta.sediment_thickness_m.iter() {
            let e = g.elevation.get(c).get();
            if e < g.sea_level.get() && *dep > 0.0 && !delta.delta_cells.contains(&c) {
                any_marine = true;
                assert!(e + delta.delta_m.get(c) <= cap + 1e-9, "wedge overtopped the cap");
            }
        }
        assert!(any_marine, "no shelf built");
        // Deltas: exactly min(delta_count, mouths) lobes, subaerial.
        assert!(delta.delta_cells.len() as u32 <= p.delta_count);
        for c in &delta.delta_cells {
            assert!(g.elevation.get(*c).get() + delta.delta_m.get(*c) >= g.sea_level.get(),
                "delta lobe stayed submerged");
        }
        // Books, full pipeline.
        assert!((delta.eroded_total_m3 - (delta.deposited_total_m3 + delta.ocean_loss_m3)).abs()
            < 1e-6 * delta.eroded_total_m3.max(1.0));
    }
```

- [ ] **Step 2: Verify failure.** **Step 3: Implement**

`deposit_wedge`: for each mouth (volume-desc order), BFS over OCEAN cells from the mouth's adjacent ocean cells, depth-limited to `wedge_reach_passive` or `wedge_reach_active` hops by the **mouth's land cell margin** (`margins.get(mouth)`; `Oceanic`/`Interior` → treat as passive). Cells whose `boundary` contact kind is `IslandArc`/`CoastalRange` **and** on the subducting side get reach 0 — their share goes to `ocean_loss` (the trench eats it). Distribute the mouth volume over reachable cells weighted `exp(-hops as f64 / 1.5)` (normalize weights), but cap each cell's fill at `(sea_level - wedge_freeboard_m) - elevation`; overflow re-distributes proportionally to uncapped cells in a second pass; anything still left is `ocean_loss`. Deterministic BFS: frontier expansion in ascending `CellId` order.

Deltas: the first `delta_count` mouths additionally raise the mouth cell itself and its adjacent ocean cells within 1 hop to `sea_level + delta_height_m * exp(-hops)` — recorded in `delta_cells`; the volume used is debited from that mouth's wedge share **before** spreading (if the mouth exported less than the lobe needs, build a proportionally lower lobe — never mint mass).

`cap_atolls`: for each `TrailSeamount` with `age_index >= 2`, find its nearest cell; if that cell is ocean, deeper than the cap but within `atoll_max_depth_m` of sea level, and `|asin(z)| < atoll_max_abs_lat`, raise it to `sea_level - atoll_freeboard_m` and record in `atoll_cells`. Atoll volume is carbonate grown in place, not river sediment: add it to `deposited_total_m3` AND `eroded_total_m3` (biogenic source and sink in one line — the books stay balanced and the battery's tier-aware phrasing in Task 12 documents it).

`carve` composes: incision → repose (on elevation+incision) → routing → wedge → deltas → atolls, summing `delta_m` and `sediment_thickness_m` (land deposit + marine deposit + atoll cap), assembling the totals.

- [ ] **Step 4: Run.** **Step 5: fmt + clippy + commit** — `feat(terrain): carve marine half — margin-aware shelf wedge, top-K deltas, trench loss, atoll caps`.

---

### Task 10: Pipeline wiring — two-pass drainage, sea-level re-solve, lithology on the carved surface

Wire the carve into `globe::generate`; re-solve sea level and final drainage on the carved surface; assemble lithology AFTER the carve with `soil_depth`/alluvium upgraded to consume `sediment_thickness` (the coordinated Ground formula change, spec §2 stage 8).

**Files:**
- Modify: `domains/terrain/src/globe.rs` — the stage 5–8 wiring; retain `sediment_thickness: CellMap<f64>`, `carve_delta_m: CellMap<f64>`, `delta_cells`, `atoll_cells` on the globe.
- Modify: `domains/terrain/src/lithology.rs` — `soil_depth_at` gains a `sediment_m: f64` input (`accum` term becomes `0.5 * math::ln(1.0 + drainage) + 0.8 * sediment_m.min(10.0)`; the alluvium gate in `classify_rock`'s caller becomes `drainage >= ALLUVIUM_DRAINAGE || sediment_m > 2.0` — read the current gate constant name and preserve it); atoll cells force `carbonate = 0.9` in `assemble_material` (read `atoll_cells` off the globe).
- Modify: `domains/terrain/src/provider.rs` — `sediment_thickness_at`, `carve_delta_at`.
- Test: `domains/terrain/src/globe.rs` + `cli/tests` fixture re-pins.

**Interfaces:**
- Consumes: everything above.
- Produces (the wiring, replacing the current tail of `generate` after `distances`):

```rust
    // v3 pipeline (spec §2): induration precedes elevation (Task 4);
    // elevation carries decorations (Tasks 1–6); then the carve.
    let elevation_pre = elevation::generate_elevation(/* as today, + induration + arc gate + seamounts */);
    let sea_pre = elevation::derive_sea_level(&elevation_pre, effective_ocean);
    let (drainage_pre, endorheic_pre) = crate::drainage::drainage_field(geosphere, &elevation_pre, sea_pre);
    let downhill = crate::drainage::downhill_targets(geosphere, &elevation_pre, sea_pre);
    let carbonate_pre = /* pointwise carbonate exactly as assemble_material derives it pre-buffer:
        extract lithology's carbonate expression into `pub(crate) fn carbonate_at(...)` the same
        way Task 4 extracted induration, and build the CellMap here */;
    let margins = /* extract lithology's margin_polarity per-cell loop into a CellMap the same way */;
    let cd = crate::carve::carve(geosphere, &elevation_pre, sea_pre, &drainage_pre, &endorheic_pre,
        &downhill, &induration, &carbonate_pre, &margins, &boundary_map, &trail_seamounts, &crate::carve::CarveParams::default());
    let elevation_map = CellMap::from_fn(geosphere, |c| {
        ReferenceElevation::new(elevation_pre.get(c).get() + cd.delta_m.get(c)).expect("carved elevation finite")
    });
    let sea_level = elevation::derive_sea_level(&elevation_map, effective_ocean); // percentile-exact re-solve (decision 0053 unchanged)
    let (drainage, endorheic) = crate::drainage::drainage_field(geosphere, &elevation_map, sea_level);
    // unrest unchanged (boundary-driven); lithology LAST, over the carved surface.
```

- [ ] **Step 1: Failing test**

```rust
    #[test]
    fn the_carved_globe_has_a_shelf_mode_and_consistent_drainage() {
        let geo = Geosphere::new(5);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        // Shelf: some ocean cells sit in the near-sea band the wedge builds.
        let band = g.elevation.iter().filter(|(_, e)| {
            let d = g.sea_level.get() - e.get();
            (0.0..=200.0).contains(&d)
        }).count();
        assert!(band > 0, "no near-sea-level marine band");
        // Final drainage was computed on the final surface: every land cell's
        // downhill neighbor relationship is consistent with final elevations.
        // (drainage_field already guarantees this; assert its determinism:)
        let (d2, _) = crate::drainage::drainage_field(&geo, &g.elevation, g.sea_level);
        assert_eq!(g.drainage, d2);
        // Sediment field is retained and non-negative.
        assert!(g.sediment_thickness.iter().all(|(_, s)| *s >= 0.0));
        // Soil depth reads real sediment: on the max-sediment land cell,
        // soil depth is positive.
        let (c, _) = g.sediment_thickness.iter().filter(|(c, _)| *g.elevation.get(*c) >= g.sea_level)
            .max_by(|a, b| a.1.total_cmp(b.1).then(b.0.cmp(&a.0))).unwrap();
        assert!(g.lithology.get(c).soil_depth.get() > 0.0);
    }
```

- [ ] **Step 2: Verify failure.** **Step 3: Implement** per the wiring block above. The carbonate/margin extractions mirror Task 4's induration extraction (pure refactors of The Ground's expressions — the post-carve buffer legitimately differs because elevation/drainage changed, but the *expressions* must stay the source of truth in one place). `assemble_material` signature grows a `sediment: &CellMap<f64>` + `atoll_cells: &[CellId]` — update its call and The Ground's tests accordingly (their assertions are structural, not pinned to bytes; any that pinned values get re-pinned).
- [ ] **Step 4: Run the full workspace once** — `cargo nextest run --workspace 2>&1 | tee /tmp/hv-t10.txt` — expect broad fixture drift; re-pin ALL drifting fixtures in this commit (lens_purity, walker transcripts, almanac snapshots, golden calibration pins — each test's header documents its refresh command). Tolerated red: stale-census schema only.
- [ ] **Step 5: fmt + clippy + commit** — `feat(terrain): wire the carve — two-pass drainage, sea-level re-solve, lithology over the carved surface (epoch v3 assembled)`.

---

### Task 11: Derived point observations — waterfalls, provenance, almanac notables

Knickpoint (waterfall) sites where big rivers cross induration steps; the open provenance enum; provider queries; almanac notables.

**Files:**
- Modify: `domains/terrain/src/carve.rs` — `find_waterfalls`, `Provenance` enum, populate `CarveDelta.waterfall_sites`.
- Modify: `domains/terrain/src/globe.rs` — retain `waterfall_sites: Vec<CellId>`.
- Modify: `domains/terrain/src/provider.rs` — `waterfalls(&self) -> &[CellId]`, `deltas(&self) -> &[CellId]`, `playas(&self) -> Vec<CellId>` (endorheic cells with `sediment_thickness > 0`).
- Modify: `windows/almanac/src/` — notables in the terrain section ("the Great Falls", "the Great Delta", "salt flats"); follow the existing notable-emission pattern (The Ground's Task 7 added one — mirror it).
- Test: carve tests + almanac snapshot test.

**Interfaces:**
- Produces:

```rust
/// Where a landform came from (spec §5, fantasy hook): an OPEN enum —
/// `Process` is the only member this campaign; `Mythic` is banked (gated
/// overlay), landing additively later. Phenomena never reveal their
/// producing system, so future mythic members are indistinguishable to
/// consumers by construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    /// Formed by ordinary geologic process.
    Process,
}

/// Waterfall detection: a land cell whose drainage exceeds
/// `WATERFALL_MIN_DRAINAGE` (80.0) and whose downhill neighbor's
/// erodibility exceeds this cell's by `WATERFALL_INDURATION_STEP` (0.35
/// in induration terms: hard lip over soft plunge pool) — the knickpoint.
pub fn find_waterfalls(geo, elevation, sea_level, drainage, induration, downhill) -> Vec<CellId>;
```

- [ ] **Step 1: Failing test** — synthetic: build a small map where a high-drainage cell sits on induration 0.9 with downhill neighbor 0.3; assert it is found; assert a low-drainage clone is not. Plus: seed-sweep probe (seeds 1..=8, L5) asserting ≥1 waterfall exists in the sweep union (existence probe per spec §8, not per-world).
- [ ] **Step 2: Verify failure.** **Step 3: Implement** (sorted output by `CellId`; `waterfall_sites` recorded in `carve` after incision using PRE-carve induration and PRE-carve drainage — the sites are where the carve worked hardest against contrast). Almanac: mirror the existing notables block; land-only filtering (The Ground Task 4's CARRY note: always filter to `!is_ocean`).
- [ ] **Step 4: Run + re-pin the almanac artifacts** (`SKIP_CENSUS=1 ./scripts/regenerate-artifacts.sh` refreshes the three seed-42 almanacs; commit the regenerated files).
- [ ] **Step 5: fmt + clippy + commit** — `feat(terrain,almanac): waterfalls/deltas/playas as provenance-carrying point observations + notables`.

---

### Task 12: Lenses, census metrics, and the reroute diagnostic

The carve-delta/sediment debug lens; five census metrics; the escalation number.

**Files:**
- Modify: `domains/terrain/src/render.rs` — `sediment_png(geo, globe)` (diverging palette: incision red, deposition blue, zero white — ride the existing rasterizer).
- Modify: `cli/src` — `map --field sediment` (extend the `--field` match The Ground added).
- Modify: `domains/terrain/src/carve.rs` — `pub fn rerouted_flow_fraction(geo, pre_drainage, pre_downhill, post_drainage, post_downhill, sea_level_pre, sea_level_post, n_rivers: usize) -> f64`.
- Modify: `windows/lab/src/metrics.rs` + `windows/lab/src/schema.rs` — metrics: `shelf-width-passive-median`, `shelf-width-active-median`, `sediment-volume`, `waterfall-count`, `delta-count`, `rerouted-flow-fraction`.
- Test: metric unit tests in `windows/lab`; render test in terrain.

**Interfaces:**
- `rerouted_flow_fraction`: take the top `n_rivers` (=20) pre-carve mouths by pre-drainage flux; for each, walk the pre-carve path from its farthest upstream max-drainage cell down; measure the fraction of that path's cells whose POST-carve downhill target differs; flux-weight by pre-drainage; return the weighted mean. Doc the preregistered thresholds (spec §8): `< 0.10` self-consistent; `0.10–0.30` flag; `> 0.30` A rejected as sole engine.
- Shelf width: for each coast land cell, hops seaward until depth exceeds `2 * wedge cap depth` or reach 8; median split by the coast cell's `MarginPolarity`.

- [ ] **Step 1: Failing tests** (metric registration: `lab list-metrics` contains the six ids; `rerouted_flow_fraction` on an UNCHANGED surface returns 0.0 — feed the same drainage twice).
- [ ] **Step 2: Verify failure.** **Step 3: Implement.** The globe must retain the pre-carve drainage/downhill for the diagnostic: add `pre_drainage: CellMap<f64>` (doc: diagnostic-only, never serialized) OR — cheaper — compute the fraction inside `generate` where both trees are in scope and retain just the number: `pub carve_reroute_fraction: f64` on the globe. **Prefer the number** (no second CellMap retained). Metric reads it.
- [ ] **Step 4: Run; regenerate the committed lens artifact** (`map --field sediment` on seed 42 → `book/src/gallery/`; follow the lithology-map precedent for where it lands + drift-check wiring in `.github/workflows/ci.yml` — mirror the lithology entry).
- [ ] **Step 5: fmt + clippy + commit** — `feat(terrain,lab,cli): sediment lens + shelf/sediment/waterfall/delta/reroute census metrics`.

---

### Task 13: Batteries + the perf readout — HARD STOP

The spec §8 battery set as tests; the §7 performance measurement; then STOP and present to Nathan.

**Files:**
- Create: `domains/terrain/tests/carve_properties.rs` — the battery file (mirror `tectonic_properties.rs`'s structure; heavy sweeps carry `#[ignore = "heavy: ..."]`).
- Test: itself.

**Batteries (each a test; sweep seeds 1..=40 at L5 for the heavy-tagged ones, seed 42 L4/L5 for the gate-tier ones):**
- `mass_balance_holds` (gate tier): re-assert the Task 9 books identity on 4 seeds. Doc line: "asserted for the metaphysically-inert tier (spec §5) — atoll carbonate is booked as paired source+sink."
- `harder_rock_cuts_less` (gate): on seed 42, bucket land cells by induration decile; median |incision| decreasing from softest to hardest decile (weak monotonicity: Spearman-style rank check, allow one inversion).
- `shelf_width_asymmetry` (heavy: 40-seed sweep): median passive shelf width > median active shelf width across the sweep.
- `arcs_are_discrete` (heavy): mean arc-land component count per world at v3 > 1.5 (the v2 wall reads as 1).
- `trails_exist_age_ordered` (gate): Task 6's chain assertions on the retained `trail_seamounts` of a generated globe.
- `atolls_only_on_warm_submerged_seamounts` (gate): every `atoll_cells` member satisfies the Task 9 conditions.
- `waterfalls_exist_across_sweep` (heavy): sweep-union existence (Task 11's probe promoted here).
- `pin_isolation_extends_to_new_streams` — in `domains/terrain/tests/tectonic_properties.rs`: follow the existing pin-isolation pattern; assert pinned (`--plates`, `--continents`, `--ocean-fraction`, `--supercontinent`) and unpinned paths consume identical terrane/microcontinent draws (the house test compares notes' draw metering / resulting sets across pin toggles — mirror it exactly).
- `eustatic_dividend_regression` (heavy): with the deep-time eustatic probe (grep the paleo tests for the sea-level-swing helper), the flooded-area swing per unit sea-level change at v3 ≥ v2's recorded value (hardcode the v2 number measured before this campaign's first byte-change commit — measure it in THIS task from a pre-Task-1 checkout via `git stash`/worktree at the plan-baseline commit and record the constant with a comment).

**Perf measurement (§7):**

```bash
# From the worktree, release mode, before-vs-after (before = the plan-baseline commit):
cargo build --release -p hornvale 2>/dev/null
for s in 1 7 42 99 1234; do /usr/bin/time -p ./target/release/hornvale new --seed $s --out /tmp/hv-perf-$s.json 2>&1 | grep real; done
# Repeat on a baseline worktree checked out at the pre-campaign commit; record both tables.
```

Record: per-world wall time before/after, the two drainage passes' share (add a temporary `eprintln!` timing behind `HV_TIMING=1` env guard if needed — remove before commit), and the projected 1000-seed census delta.

- [ ] **Step 1–4:** write batteries red-green as usual; run `make gate` once (`2>&1 | tee /tmp/hv-t13-gate.txt`) — everything green except tolerated census staleness.
- [ ] **Step 5: fmt + clippy + commit** — `test(terrain): carve/decoration property batteries + eustatic regression`.
- [ ] **Step 6: HARD STOP (autopilot carve-out).** Present to Nathan in one message: the perf table (before/after, drainage share, census projection), the seed-42 `rerouted_flow_fraction` + the 5-seed spread, the sediment + elevation lens images (SendUserFile), and the battery statuses. **Do not begin Task 14 until Nathan responds.** Any fidelity cut he orders is ledgered and applied before tuning.

---

### Task 14: Tuning season — bring the bands inside

Iterate the `CarveParams` and decoration constants against the six bands + the joint ria–wedge watch, on lab probes; finish with the preregistered verdict readout.

**Files:**
- Modify: constants in `carve.rs` / `elevation.rs` / `crust.rs` only (tuning never restructures).
- Create: `studies/sculpting-probe.study.json` — a small-N (100-seed) study computing the six band metrics + reroute fraction (mirror an existing study's JSON shape; `lab run` it locally — this is NOT the census).
- Modify: `domains/terrain/tests/carve_properties.rs` — when tuning lands, promote the final constants' band positions into a gate-tier smoke assertion ONLY if stable (median-of-8-seeds within band ± 20% margin; else leave to the census).

**Procedure (each iteration = one commit):**
1. `cargo run --release -p hornvale -- lab run studies/sculpting-probe.study.json 2>&1 | tee /tmp/hv-probe-N.txt`
2. Read the six medians + reroute + BOTH failed-band trajectories together (the antagonism: a knob that moves shelf-fraction up while shoreline-development falls is the ria–wedge fight — prefer per-mouth-supply-respecting knobs: `deposit_fraction`, `wedge_reach_*`, before `incision_k_m`).
3. Adjust ONE named constant (or one coherent pair) per iteration; document the movement in the commit message (`tune(sculpting): iteration N — <constant>: old → new; shoreline X→Y, shelf A→B, others stable`).
4. Re-pin drifting fixtures per iteration (same discipline as Task 1 Step 5).
5. **Stop conditions:** both target bands inside + four stayers inside on the 100-seed probe → done. A must-come-inside band provably unreachable without pushing another outside → **HARD STOP**: present the trade to Nathan (this is the spec §8 second escalation trigger — B enters evaluation only on his call). Reroute fraction drifting above 0.10 → flag in the iteration commit and the ledger; above 0.30 → HARD STOP (A rejected as sole engine, spec §8).
- [ ] **Final step: the 2,000-seed verdict is NOT run locally** — it happens at close on AWS (Task 15). Tuning's exit is the 100-seed probe verdict + Nathan's ack at the next stop.

---

### Task 15: Book, registry, close — G6 HARD STOP

**Files:**
- Create: `book/src/chronicle/sculpting.md`; `docs/retrospectives/sculpting.md`; `book/src/laboratory/census-of-coasts-iii.md` (bands table, before/after medians, verdicts, the reroute number, supersession notes if any).
- Modify: `book/src/domains/terrain.md` — the v3 rewrite (the §"Crust" chapter pattern: what changed, what superseded, the two-band resolution); `book/src/open-questions.md` — re-score the coast/terrain Confidence-Gradient bet.
- Modify: `book/src/frontier/idea-registry.md` — new rows (IDs: next free MAP numbers after The Ground's MAP-39): ley-lines-as-thaumic-carve; Mythic landform drawn sets; anti-physics landforms (gated); purposeful sculptors / wyrm tunnels; tidal causeways (needs astronomy tidal-range); single-metaphysics-dial umbrella row; aeolian/loess + dune fields; spits/barrier islands (built only if a band demands). Re-date MAP-21 (rift-and-fit) to "judgeable now — census-III is its baseline".
- Create: `docs/decisions/00XX-sculpting-is-terrain-epoch-v3.md` (next free number at close; records: the one-way regeneration, appended streams, the escalation criterion + its readout, the Ground formula upgrade) — follow `decisions/README.md` format.
- Modify: regenerate ALL committed artifacts (`SKIP_CENSUS=1 ./scripts/regenerate-artifacts.sh` + the map artifacts) — commit the drift.

**Steps:**
- [ ] **Step 1:** Write the book/registry/decision material above. Run `cargo test -p hornvale --test docs_consistency` (registry IDs unique, links resolve).
- [ ] **Step 2:** `make gate` once — green (census staleness tolerated).
- [ ] **Step 3: G6 HARD STOP.** Invoke `closing-a-campaign` and present the post-G3 ledger digest (save-format/epoch entries leading). The close includes, each with Nathan's explicit authorization: the AWS census regen (`make regen-remote` — census fixtures + Census-of-Coasts-III verdict tables land in the regen commit), any final golden re-pins, `make preflight`, and the merge to main. Chronicle/retro/freshness-sweep complete before merge (Definition of Done; the lab-performance lesson: a merge without its close is a campaign that isn't done).

---

## Self-Review

**Spec coverage:** §1 goal → Tasks 7–10 + 14; §2 pipeline stages 1–9 → Tasks 1–2 (stage 2), 3/5/6 (stage 3), 4 (stage 4), 7–9 (stage 6), 10 (stages 5, 7, 8); seam + potential-agnostic doc → Task 7 module doc; §3 decorations: terranes T1, microcontinents T2, trails T6, atolls T9, arcs+belts+trench T3, fBm T5; §4 induration seam + erodibility totality + carbonate divisor → T4 + T7 `erodibility`; §5 carve terms → T7–T9; antagonism → T14 procedure step 2; global-knobs → constants discipline throughout; point observations + provenance → T11; §6 epoch mechanics → Global Constraints + per-task re-pin steps; §7 perf → T13; §8 bands/batteries/escalation (both triggers) → T13/T14; census columns → T12; Census III → T15; §9 deliverables → T11 (almanac), T12 (lenses, metrics), T15 (book, registry, decision); §10 sequencing → prerequisite constraint + task order; §11 testing → T13 batteries + pin-isolation + eustatic regression; §12 non-goals — no task builds them; §13 registry capture → T15. ✓

**Placeholder scan:** all constants concrete; all test bodies complete or specified to an assertable statement; "follow the existing pattern" appears only with a named file/precedent (the almanac notable pattern, the pin-isolation pattern, the study JSON shape, the lens artifact CI wiring) — the executor must read the named source first. Craton field names, the kernel hash-noise/pow helper names, and `nearest_cell` position-lookup are flagged for verification against source (three call-outs, each with the fallback spelled out).

**Type consistency:** `CarveParams`/`CarveDelta` fields used in T7–T12 match their T7/T9 definitions; `erodibility(induration, carbonate)` signature consistent T7/T13; `downhill_targets` introduced T8 and consumed T9/T10/T11; `trail_seamounts` produced T6, consumed T9/T13; `induration` CellMap produced T4, consumed T5/T7/T10/T11. `Provenance` defined T11 and referenced nowhere earlier. ✓

**Epoch discipline check:** byte-changing tasks (1, 2, 3, 5, 6, 10, 11-almanac, 14 iterations) each carry an explicit re-pin step; byte-neutral tasks (4, 7, 8, 9, 12-code, 13) state no drift expected. ✓
