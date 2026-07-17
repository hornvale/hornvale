# The Real Sky Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Export the sim's real night sky (`scene/neighbors/v1`: 2–5 notable
neighbors + 100–300 background field stars) plus each moon's orbital
inclination/node, and make the Orrery render the actual starfield and
retrograde moon orbits instead of cosmetic fakes.

**Architecture:** Producer-first: `windows/scene` gains a fourth scene
document mirroring `moons_scene`, the CLI/wasm/book surfaces follow, then
the freshly built wasm binary feeds the Orrery consumer tasks
(parser → starfield view → moon orbits/cards). Spec:
`docs/superpowers/specs/2026-07-17-the-real-sky-design.md`.

**Tech Stack:** Rust (workspace crates `hornvale-scene`, `hornvale`,
standalone `clients/world-wasm`), TypeScript + three.js + vitest (orrery).

## Global Constraints

- Hornvale worktree: `~/.config/superpowers/worktrees/hornvale/the-real-sky`, branch `the-real-sky`. Orrery worktree: `~/.config/superpowers/worktrees/orrery/the-real-sky`, branch `the-real-sky`. Never commit elsewhere.
- Dependencies: `serde`/`serde_json` only (Rust); no new npm packages.
- No `HashMap`/`HashSet`; `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock time.
- Every serialized f64 quantizes at emit: `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]` (decision 0033). Quantize at the emit boundary ONLY, never in compute.
- Every new `pub` item gets a one-line doc comment (`#![warn(missing_docs)]`) and the struct doc carries a `type-audit:` verdict line for each primitive field (mirror `MoonSurface`'s line; use `pending(wave-1: <field>)` for degrees/ly, `bare-ok(ratio|count|identifier-text|constructor-edge: <field>)` per neighboring precedent). After adding pub fields run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` and, if it flags the report, `-- report > docs/audits/type-audit-report.md` (commit the report with the same commit).
- Schema stability: new JSON fields are APPENDED after every existing field; existing field order never changes. `scene/neighbors/v1` field order once merged is contract.
- `cargo fmt` before every commit; the hornvale commit gate is `make gate` (run once per task, at the end, with Bash `timeout: 3600000`). Orrery gate: `npm test` + `npx tsc --noEmit`.
- No new draws, no ledger writes, no stream-label changes anywhere in this plan. If you find yourself touching `streams.rs` or `Ledger`, stop — you've left the plan.
- Commit messages end with:
  `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: `neighbors_scene` producer in `windows/scene`

**Files:**
- Modify: `windows/scene/src/lib.rs` (new section after the moons-scene block, ~line 715; new tests beside `moons_scene_has_schema_indices_and_is_deterministic`, ~line 975)
- Maybe modify: `domains/astronomy/src/lib.rs` (only to re-export `class_name` / `starfield` / `FieldStar` at crate root if not already — check `grep "pub use" domains/astronomy/src/lib.rs` first; follow how `is_icy`/`radius_km` are exported, since lib.rs already re-exports those for this crate)

**Interfaces:**
- Consumes: `hornvale_worldgen::sky_of`, `hornvale_worldgen::ASTRONOMY_STREAM_ROOT` (already re-exported, see `windows/worldgen/src/lib.rs:12`), `hornvale_astronomy::{class_name, starfield}` (`neighborhood.rs` / `starfield.rs`), `system.neighbors: Vec<Neighbor>` (fields: `class`, `distance: LightYears` — tuple newtype, read with `.0` per `neighborhood.rs`, or `.get()` if it exists — `apparent_brightness`, `color: String`, `declination`, `right_ascension`).
- Produces: `NEIGHBORS_SCHEMA: &str = "scene/neighbors/v1"`, `pub struct NeighborElem`, `pub struct FieldStarElem`, `pub struct NeighborsScene`, `pub fn neighbors_scene(&World) -> Result<NeighborsScene, SceneError>`, `pub fn neighbors_json(&NeighborsScene) -> String`. Tasks 2/5 call exactly these.

- [ ] **Step 1: Write the failing tests** (inside the existing `#[cfg(test)] mod`, reusing the existing `mooned_world()` helper):

```rust
#[test]
fn neighbors_scene_has_schema_populations_and_is_deterministic() {
    let a = neighbors_scene(&mooned_world()).expect("generated world has a sky");
    assert_eq!(a.schema, "scene/neighbors/v1");
    assert_eq!(a.seed, mooned_world().seed.0);
    assert!((2..=5).contains(&a.neighbors.len()), "2-5 notable neighbors");
    assert!((100..=300).contains(&a.stars.len()), "100-300 field stars");
    for (i, n) in a.neighbors.iter().enumerate() {
        assert_eq!(n.index, i);
    }
    // Brightest first — the generator's own ordering, preserved.
    for w in a.neighbors.windows(2) {
        assert!(w[0].brightness_rel >= w[1].brightness_rel);
    }
    // Byte-identical on rebuild (determinism from the seed alone).
    assert_eq!(
        neighbors_json(&a),
        neighbors_json(&neighbors_scene(&mooned_world()).unwrap())
    );
}

#[test]
fn neighbors_scene_fields_are_in_range() {
    let a = neighbors_scene(&mooned_world()).unwrap();
    for n in &a.neighbors {
        assert!((-90.0..=90.0).contains(&n.dec_deg));
        assert!((0.0..360.0).contains(&n.ra_deg));
        assert!(n.brightness_rel > 0.0);
        assert!((4.0..=80.0).contains(&n.distance_ly));
        assert!(!n.class_name.is_empty() && !n.color.is_empty());
    }
    for s in &a.stars {
        assert!((-90.0..=90.0).contains(&s.dec_deg));
        assert!((0.0..360.0).contains(&s.ra_deg));
        assert!((1..=5).contains(&s.magnitude_class));
    }
}
```

Also mirror the existing constant-sun refusal test if one exists for
`moons_scene` (grep `no generated sky` in the test mod; copy it for
`neighbors_scene`).

- [ ] **Step 2: Run to verify failure**: `cargo test -p hornvale-scene neighbors_scene` — expect compile FAIL (`neighbors_scene` not found).

- [ ] **Step 3: Implement** (mirror the `moons_scene` block exactly — schema const + structs + builder + json fn, with doc comments and type-audit lines):

```rust
/// The `scene/neighbors/v1` schema tag.
pub const NEIGHBORS_SCHEMA: &str = "scene/neighbors/v1";

/// One notable neighbor star as drawn by the generator, brightest first.
/// type-audit: bare-ok(count: index), bare-ok(identifier-text: class_name), bare-ok(identifier-text: color), pending(wave-1: distance_ly), bare-ok(ratio: brightness_rel), pending(wave-1: ra_deg), pending(wave-1: dec_deg)
#[derive(Debug, Serialize)]
pub struct NeighborElem {
    /// Generation index (stable identity; matches the ledger entity order).
    pub index: usize,
    /// Prose spectral class ("red giant", "hard blue-white" is `color`, not this).
    pub class_name: String,
    /// The producer's color word (e.g. "smoldering red").
    pub color: String,
    /// Distance in light-years (drawn, 4-80).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_ly: f64,
    /// Apparent brightness, relative units (derived L/d²).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub brightness_rel: f64,
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
}

/// One anonymous background field star (texture, not a ledger entity).
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg), bare-ok(count: magnitude_class)
#[derive(Debug, Serialize)]
pub struct FieldStarElem {
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
    /// Brightness class, 1 (brightest) ..= 5 (faintest).
    pub magnitude_class: u8,
}

/// One `scene/neighbors/v1` document: the night sky's two populations.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct NeighborsScene {
    /// Always `scene/neighbors/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The notable neighbors, generation order (brightest first).
    pub neighbors: Vec<NeighborElem>,
    /// The background starfield, derivation order.
    pub stars: Vec<FieldStarElem>,
}

/// Build the `scene/neighbors/v1` scene for `world`. Errors when the world
/// has no generated sky — mirrors [`moons_scene`]. Pure reads: consumes no
/// genesis draws (the starfield derives on demand from the astronomy seed,
/// exactly as the almanac's figures path does).
pub fn neighbors_scene(world: &World) -> Result<NeighborsScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let neighbors = system
        .neighbors
        .iter()
        .enumerate()
        .map(|(index, n)| NeighborElem {
            index,
            class_name: hornvale_astronomy::class_name(n.class).to_string(),
            color: n.color.clone(),
            distance_ly: n.distance.0,
            brightness_rel: n.apparent_brightness,
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        })
        .collect();
    let astronomy_seed = world.seed.derive(hornvale_worldgen::ASTRONOMY_STREAM_ROOT);
    let stars = hornvale_astronomy::starfield(astronomy_seed)
        .into_iter()
        .map(|s| FieldStarElem {
            ra_deg: s.ra_deg,
            dec_deg: s.dec_deg,
            magnitude_class: s.magnitude_class,
        })
        .collect();
    Ok(NeighborsScene {
        schema: NEIGHBORS_SCHEMA.to_string(),
        seed: world.seed.0,
        neighbors,
        stars,
    })
}

/// Serialize a `NeighborsScene` to compact JSON (mirrors [`moons_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn neighbors_json(scene: &NeighborsScene) -> String {
    serde_json::to_string(scene).expect("a NeighborsScene always serializes")
}
```

Adjust import paths to what `hornvale_astronomy` actually exports (check
its `lib.rs`; add root re-exports of `class_name`, `starfield`, `FieldStar`
beside the existing `is_icy`/`radius_km` re-exports if needed). If
`LightYears` has `.get()`, prefer it over `.0`.

- [ ] **Step 4: Verify green**: `cargo test -p hornvale-scene` — all pass.
- [ ] **Step 5: type-audit**: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`; if the committed report is stale, regenerate it (see Global Constraints) and include it in the commit.
- [ ] **Step 6: fmt + clippy**: `cargo fmt && cargo clippy -p hornvale-scene --all-targets -- -D warnings`.
- [ ] **Step 7: Commit**: `feat(the-real-sky): scene/neighbors/v1 — the night sky's two populations`.

---

### Task 2: CLI `scene neighbors` + committed gallery artifact

**Files:**
- Modify: `cli/src/main.rs` (usage block ~line 47-51: add `hornvale scene neighbors [--world <PATH>]   emit scene/neighbors/v1 JSON to stdout`; `cmd_scene` ~line 756: new match arm)
- Modify: `scripts/regenerate-artifacts.sh` (~line 100, beside the `scene moons` line)
- Check/modify: `.github/workflows/ci.yml` — if the "Artifacts are current" step enumerates scene commands individually, add the neighbors line there too; if it calls the script, nothing to do.
- Create (generated): `book/src/gallery/scene-neighbors-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_scene::{neighbors_scene, neighbors_json}` (Task 1).
- Produces: the committed seed-42 gallery JSON that CI drift-checks; Task 3's reference page links it.

- [ ] **Step 1: Add the CLI arm** (mirror the `moons` arm exactly):

```rust
Some("neighbors") => {
    let world = load_world(args)?;
    let scene = hornvale_scene::neighbors_scene(&world).map_err(|e| e.to_string())?;
    println!("{}", hornvale_scene::neighbors_json(&scene));
    Ok(())
}
```

- [ ] **Step 2: Add the regen line** after the `scene moons` line in `scripts/regenerate-artifacts.sh`:

```bash
run -p hornvale -- scene neighbors --world "$wsky" > book/src/gallery/scene-neighbors-seed-42.json
```

- [ ] **Step 3: Smoke + generate**: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json && cargo run -p hornvale -- scene neighbors --world /tmp/hv42.json | head -c 300` (expect the schema tag and populated arrays), then `bash scripts/regenerate-artifacts.sh` (foreground, `timeout: 3600000`; it skips censuses by default — NEVER set HV_CENSUS) and confirm `git status` shows ONLY `book/src/gallery/scene-neighbors-seed-42.json` as new with no other drift.
- [ ] **Step 4: ci.yml check** as described under Files.
- [ ] **Step 5: Gate**: `make gate` (timeout 3600000). Expect green.
- [ ] **Step 6: Commit**: `feat(the-real-sky): CLI scene neighbors + seed-42 gallery artifact`.

---

### Task 3: Reference page `scene-neighbors-v1.md`

**Files:**
- Create: `book/src/reference/scene-neighbors-v1.md`
- Modify: `book/src/SUMMARY.md` (line ~147, after the moons v1 entry: `- [Scene Schema: neighbors v1](./reference/scene-neighbors-v1.md)`)

**Interfaces:**
- Consumes: the Task-1 schema and the spec's §2 frame convention (copy its normative language).
- Produces: the normative consumer-transform documentation Task 7's implementation must match.

- [ ] **Step 1: Write the page**, following `book/src/reference/scene-moons-v1.md`'s structure and altitude (open with what it is and why it exists; a field table in contract order; a "derived vs drawn" boundary section; consumer notes). It MUST contain, verbatim in substance:
  - Both populations and their epistemic difference: neighbors are drawn ledger entities (each backed by `is-neighbor` facts); field stars are derived texture ("derived on demand, never serialized" — quote `starfield.rs`'s framing), the same population `figures()` clusters.
  - **The frame convention (normative):** RA/dec are genesis-epoch equatorial coordinates; the celestial equator is the anchor's rotational equator. Precession is deliberately not exported in v1.
  - **The consumer transform (normative, tested by the Orrery):** unit vector `(cos dec · cos ra, sin dec, cos dec · sin ra)` in the y-up equatorial frame, rotated about the x-axis (the vernal-equinox direction, RA 0) by the world's `obliquity_deg` from `scene/system/v1`, yields the y-up ecliptic scene frame; a dec = +90 star lands on the world's spin axis at `(0, cos ε, sin ε)`.
  - The u64-seed BigInt caveat (copy from the moons page).
  - A link to the committed seed-42 gallery document.
- [ ] **Step 2: Build**: `mdbook build book` — no errors, page reachable.
- [ ] **Step 3: Commit**: `docs(the-real-sky): scene/neighbors/v1 reference page`.

---

### Task 4: `MoonElem` rider — `inclination_deg` + `node_longitude_deg`

**Files:**
- Modify: `windows/scene/src/lib.rs` (`MoonElem` struct ~line 373 — APPEND both fields after `size_rel`, never reorder; `system_scene` builder ~line 420; the `system_scene_has_the_schema_moons_and_is_deterministic` test ~line 888)

**Interfaces:**
- Consumes: `hornvale_astronomy::Moon.inclination_deg`, `.node_longitude_deg` (both `pub f64`, `domains/astronomy/src/moons.rs:53,59`).
- Produces: `MoonElem.inclination_deg` / `.node_longitude_deg` in `scene/system/v1` JSON — Task 6 parses them as `inclinationDeg`/`nodeLongitudeDeg`; Task 8 renders from them.

- [ ] **Step 1: Extend the test** (red): in the existing system-scene test add

```rust
for m in &scene.moons {
    assert!((0.0..=180.0).contains(&m.inclination_deg));
    assert!((0.0..360.0).contains(&m.node_longitude_deg));
}
assert!(
    scene.moons.iter().any(|m| m.inclination_deg > 90.0),
    "seed 42 has a retrograde captured moon (The Reckoning)"
);
```

- [ ] **Step 2: Verify it fails to compile**: `cargo test -p hornvale-scene system_scene`.
- [ ] **Step 3: Implement**: append to `MoonElem` (with quantize serde attrs, doc comments noting "appended per the schema stability contract", and `pending(wave-1: …)` type-audit entries on the struct's tag line):

```rust
    /// Orbital inclination to the anchor's orbital plane, degrees; > 90 is
    /// retrograde (The Reckoning). Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub inclination_deg: f64,
    /// Ecliptic longitude of the ascending node at genesis, degrees.
    /// Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub node_longitude_deg: f64,
```

and in the builder map: `inclination_deg: m.inclination_deg, node_longitude_deg: m.node_longitude_deg,`.
- [ ] **Step 4: Green**: `cargo test -p hornvale-scene`.
- [ ] **Step 5: Drift sweep — re-pin in THIS commit** (the rebaseline rule): run `bash scripts/regenerate-artifacts.sh` (foreground, timeout 3600000) and `cargo test --workspace 2>&1 | tee /tmp/hv-t4.txt` via `make gate`. If any golden pins the system-scene bytes (candidates: `windows/scene/examples/ephemeris_golden.rs` fixtures, e2e fixtures, gallery JSONs), regenerate with its documented mechanism (`REBASELINE=1 …` / the regen script) and commit the re-pins together with the field change. `git status` must be clean of unexplained drift afterward.
- [ ] **Step 6: type-audit check** (fields are pub): run check; regen report if flagged.
- [ ] **Step 7: Commit**: `feat(the-real-sky): scene/system/v1 moons carry inclination + node (additive)`.

---

### Task 5: wasm export `hw_scene_neighbors` + fresh binary for the consumer

**Files:**
- Modify: `clients/world-wasm/src/lib.rs` (new export after `hw_scene_moons`, ~line 217)
- Produces (not committed): a rebuilt `hornvale_world.wasm` copied into the orrery worktree.

**Interfaces:**
- Consumes: Task 1's `neighbors_scene`/`neighbors_json`; the file's `WORLD`/`set_out`/`set_error` statics.
- Produces: `hw_scene_neighbors() -> i32` (0 ok / 2 scene error / -3 no world) — Task 6's catalog calls it by exactly this name.

- [ ] **Step 1: Implement** (mirror `hw_scene_moons` verbatim, swapping the two function calls and the doc comment):

```rust
/// Emit the current world's `scene/neighbors/v1` JSON (night-sky catalog).
/// 0 ok; 2 scene error (envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_neighbors() -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::neighbors_scene(world) {
        Ok(s) => {
            set_out(hornvale_scene::neighbors_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}
```

- [ ] **Step 2: Run the crate's own checks**: `cargo fmt --manifest-path clients/world-wasm/Cargo.toml && cargo clippy --manifest-path clients/world-wasm/Cargo.toml -- -D warnings`, plus its tests if `clients/world-wasm` has any (`cargo test --manifest-path clients/world-wasm/Cargo.toml`).
- [ ] **Step 3: Build the binary**: `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release` (install the target via `rustup target add wasm32-unknown-unknown` if missing).
- [ ] **Step 4: Ship it to the consumer worktree**: `cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm ~/.config/superpowers/worktrees/orrery/the-real-sky/public/hornvale_world.wasm` (this is the dev-loop equivalent of the orrery's `npm run wasm:local`; the v5 release re-pin is a G6 action, NOT yours).
- [ ] **Step 5: Commit** (hornvale side only): `feat(the-real-sky): hw_scene_neighbors wasm export`.

---

### Task 6: Orrery parsers + catalog + wasm-fixture tests

**Files (all in the ORRERY worktree):**
- Modify: `src/sim/scene.ts` (new `NeighborsScene`/`NeighborElem`/`FieldStar` interfaces + `parseNeighbors`; `MoonElem`-equivalent system interface + parser gain `inclinationDeg`/`nodeLongitudeDeg`; the moons `MoonSurface` interface + parser gain `densityGCm3`/`formation` — all wire names snake_case, parsed to camelCase per house style)
- Modify: `src/sim/scene.test.ts` (mirror the `parseMoons` test block)
- Modify: `src/sim/catalog.ts` (add `sceneNeighbors(): string` to the interface ~line 66 and the implementation ~line 95, mirroring `sceneMoons()` exactly, calling `hw_scene_neighbors`)
- Modify: `src/testHelpers/wasmFixture.ts` (add `loadSeed42Neighbors`, mirroring `loadSeed42Moons`)
- Modify: `src/sim/catalogFixture.test.ts` (new end-to-end assertions)

**Interfaces:**
- Consumes: the Task-5 binary already at `public/hornvale_world.wasm` (verify: `ls -la public/hornvale_world.wasm` shows today's date; if not, STOP — Task 5 must run first).
- Produces: `parseNeighbors(text: string): NeighborsScene` with `neighbors: {index, className, color, distanceLy, brightnessRel, raDeg, decDeg}[]` and `stars: {raDeg, decDeg, magnitudeClass}[]`; system moons carry `inclinationDeg`/`nodeLongitudeDeg`; moon surfaces carry `densityGCm3: number` and `formation: string`. Tasks 7/8 import these exact names.

- [ ] **Step 1: Write failing parser tests** in `scene.test.ts` (follow the parseMoons block's style exactly): a valid two-neighbor + two-star document round-trips with camelCase fields; schema literal enforced (`'scene/neighbors/v0'` throws); `stars` non-array throws mentioning `stars`; `magnitude_class` of 0 or 6 throws (range 1–5); `dec_deg` of 91 throws; missing `inclination_deg` in a system moon throws; a moons document without `formation` throws.
- [ ] **Step 2: Red**: `npx vitest run src/sim/scene.test.ts` — new tests fail.
- [ ] **Step 3: Implement the parsers** with the file's existing `requireNumber`/`requireString`/array helpers and range validation (declination |x| ≤ 90, RA in [0,360), brightness > 0, magnitudeClass an integer 1–5, distanceLy > 0).
- [ ] **Step 4: Green**: `npx vitest run src/sim/scene.test.ts`.
- [ ] **Step 5: Catalog + fixture helper + end-to-end test**: add `sceneNeighbors()` and `loadSeed42Neighbors()`; in `catalogFixture.test.ts`:

```ts
test("the vendored binary's neighbors document parses strictly", async () => {
  const sky = await loadSeed42Neighbors();
  expect(sky.schema).toBe('scene/neighbors/v1');
  expect(sky.neighbors.length).toBeGreaterThanOrEqual(2);
  expect(sky.neighbors.length).toBeLessThanOrEqual(5);
  expect(sky.stars.length).toBeGreaterThanOrEqual(100);
  expect(sky.stars.length).toBeLessThanOrEqual(300);
});

test("the vendored binary's system moons carry inclination and node", async () => {
  const sys = await loadSeed42System();
  expect(sys.moons.some((m) => m.inclinationDeg > 90)).toBe(true); // the retrograde capture
  for (const m of sys.moons) {
    expect(m.nodeLongitudeDeg).toBeGreaterThanOrEqual(0);
    expect(m.nodeLongitudeDeg).toBeLessThan(360);
  }
});
```

- [ ] **Step 6: Full green**: `npm test` and `npx tsc --noEmit` (the whole suite must pass — the new binary also feeds every old fixture test).
- [ ] **Step 7: Commit**: `feat(the-real-sky): parse scene/neighbors/v1 + system inclination/node + moon formation fields`.

---

### Task 7: The real starfield view

**Files (orrery):**
- Create: `src/views/starfield.ts` + `src/views/starfield.test.ts`
- Modify: `src/views/system.ts` (delete `buildStarfield`, `STARFIELD_COUNT` and the `prng` import if now unused; `createSystemView` gains a `neighbors: NeighborsScene` parameter and mounts the new module)
- Modify: `src/main.ts` and any test constructing `createSystemView` (thread the parsed neighbors scene through — `sceneNeighbors()` from Task 6, parsed once beside the moons scene)

**Interfaces:**
- Consumes: `parseNeighbors`' `NeighborsScene` (Task 6), `sys.world.obliquityDeg` (already parsed), the existing shell-radius convention (`reach * 3`).
- Produces: `equatorialToEcliptic(raDeg, decDeg, obliquityDeg): THREE.Vector3` (unit), `neighborIntensity(brightnessRel, all: number[]): number`, `fieldStarIntensity(magnitudeClass): number`, `STAR_COLOR: Record<string, [number, number, number]>`, `buildRealStarfield(sky: NeighborsScene, obliquityDeg: number, reach: number): THREE.Group`.

- [ ] **Step 1: Failing unit tests** (`starfield.test.ts`, no WebGL needed — pure math + geometry attribute counts):
  - `equatorialToEcliptic(0, 0, 23)` ≈ `(1, 0, 0)` (equinox axis is the rotation axis).
  - `equatorialToEcliptic(_, 90, ε)` ≈ `(0, cos ε, sin ε)` — the pinned normative convention from the reference page (Task 3).
  - Every result has unit length (a few RA/dec samples).
  - `neighborIntensity` is monotone nondecreasing in brightness across a sample document and stays within [0.35, 1]; a single-neighbor document maps to 1.
  - `fieldStarIntensity(1) > fieldStarIntensity(5)`; all in (0, 1].
  - `STAR_COLOR` has an entry for every producer color word — pin the list in the test with a comment pointing at `neighborhood.rs`'s `class_color`: `"dim red", "warm yellow", "pale white", "deep orange", "smoldering red", "hard blue-white"`.
  - `buildRealStarfield(sample, 23, 10)` returns a group whose two `THREE.Points` children carry `neighbors.length` and `stars.length` vertices respectively.
- [ ] **Step 2: Red**: `npx vitest run src/views/starfield.test.ts`.
- [ ] **Step 3: Implement.** Core math:

```ts
const DEG = Math.PI / 180;
export function equatorialToEcliptic(raDeg: number, decDeg: number, obliquityDeg: number): THREE.Vector3 {
  const ra = raDeg * DEG, dec = decDeg * DEG;
  const v = new THREE.Vector3(
    Math.cos(dec) * Math.cos(ra),
    Math.sin(dec),
    Math.cos(dec) * Math.sin(ra),
  );
  return v.applyAxisAngle(new THREE.Vector3(1, 0, 0), obliquityDeg * DEG);
}
```

`neighborIntensity`: document-relative log scale — `0.35 + 0.65 * (log10(b) - log10(min)) / (log10(max) - log10(min))`, guarding the single-star / equal-brightness case to 1. `fieldStarIntensity`: the fixed ladder `{1: 0.65, 2: 0.5, 3: 0.38, 4: 0.28, 5: 0.2}`. `buildRealStarfield`: one `THREE.Group`; neighbors as a `THREE.Points` with per-vertex colors (`STAR_COLOR[color] ?? [0.8, 0.8, 0.85]`, scaled by intensity; `PointsMaterial({ vertexColors: true, size: 0.06, sizeAttenuation: true })`), field stars as a second `Points` (neutral `[0.75, 0.78, 0.85]` scaled by `fieldStarIntensity`, size 0.025); every position `equatorialToEcliptic(...).multiplyScalar(reach * 3)`.
- [ ] **Step 4: Wire into `system.ts`**: replace `root.add(buildStarfield(sys.seed, reach))` with `root.add(buildRealStarfield(neighbors, sys.world.obliquityDeg, reach))`; delete the old function and constant; update `createSystemView` callers (grep `createSystemView(` across `src/` and tests).
- [ ] **Step 5: Green**: `npm test` + `npx tsc --noEmit` (whole suite).
- [ ] **Step 6: Commit**: `feat(the-real-sky): render the world's actual night sky (two populations, real RA/dec)`.

---

### Task 8: Retrograde moon orbits + Reckoning card lines

**Files (orrery):**
- Modify: `src/views/system.ts` (`moonLocalPosition` ~line 74; if a per-moon orbit ring/line exists — grep how moon orbits are drawn — orient it with the same transform)
- Modify: `src/views/system.test.ts` (position tests)
- Modify: `src/ui/inspect.ts` (`moonInfo` — formation/density lines + retrograde tag) + `src/ui/inspect.test.ts`

**Interfaces:**
- Consumes: `sys.moons[i].inclinationDeg` / `.nodeLongitudeDeg` (Task 6), `moons.moons[i].formation` / `.densityGCm3` (Task 6), the warm-up's `moonInfo(sys, moons, i, day)` signature (commit b14fe95).
- Produces: nothing downstream; this is the last feature task.

- [ ] **Step 1: Failing position tests** (`system.test.ts`, pure math):
  - inclination 0 → `moonLocalPosition` equals the pre-change circular value (pin one day/moon case numerically).
  - inclination 90 → some sampled day has `|y| > 0.01 * radius`.
  - inclination 180 → the projected xz sweep direction reverses versus inclination 0 (compare `atan2(z, x)` deltas over a small day step: opposite signs).
  - node longitude 0 vs 90 with the same inclination → different positions (the node matters).
- [ ] **Step 2: Red.**
- [ ] **Step 3: Implement** — tilt the existing circle about the node axis (backward-compatible at inclination 0 by construction):

```ts
export function moonLocalPosition(sys, i, day, trueScale = false): THREE.Vector3 {
  const m = sys.moons[i]!;
  const angle = moonOrbitalPhase(sys, i, day) * TAU;
  const radius = moonOrbitRadiusUnits(i, m.distanceMm, trueScale);
  const v = new THREE.Vector3(radius * Math.cos(angle), 0, radius * Math.sin(angle));
  const node = m.nodeLongitudeDeg * DEG;
  const nodeAxis = new THREE.Vector3(Math.cos(node), 0, Math.sin(node));
  return v.applyAxisAngle(nodeAxis, m.inclinationDeg * DEG);
}
```

Apply the same axis-angle to any moon orbit ring object (set its quaternion once at construction: `ring.quaternion.setFromAxisAngle(nodeAxis, inclRad)`).
- [ ] **Step 4: Card** (TDD in `inspect.test.ts` first): `moonInfo` adds a line `` `formed by ${surface.formation} · density ${surface.densityGCm3.toFixed(2)} g/cm³` `` and, when `sys.moons[i].inclinationDeg > 90`, prefixes the kind line: `` `retrograde ${surface.surfaceClass} moon` ``.
- [ ] **Step 5: Green**: `npm test` + `npx tsc --noEmit`.
- [ ] **Step 6: Commit**: `feat(the-real-sky): retrograde/inclined moon orbits + formation and density on the card`.

---

### Task 9: Assembly verification (controller-run, not dispatched)

- [ ] Hornvale worktree: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean (no drift); `mdbook build book`.
- [ ] Orrery worktree: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e (`npx playwright test`; install browsers if needed), all green.
- [ ] Visual verification (The Lens's lesson — open the real render): serve the built orrery, screenshot seed 42's system view; confirm by eye — real starfield (a few bright tinted stars + ~200 faint ones), the retrograde moon sweeping opposite its sibling, the card's formation/density/retrograde lines.
- [ ] Conflict-marker sweep (all three markers) across both worktrees; followup register updated; G6 package assembled (ledger digest with #11 leading, screenshot, gate evidence, wasm-v5 release + re-pin + push plan for Nathan's carve-out approvals).
