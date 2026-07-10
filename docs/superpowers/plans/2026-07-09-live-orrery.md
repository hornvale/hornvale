# The Live Orrery Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Emit a new `scene/system/v1` scene kind and build an interactive, computed browser orrery (`clients/orrery/`) that reads it plus the existing terrain scene, replacing the asciinema players as the gallery centerpiece.

**Architecture:** The Rust simulation emits semantic orbital *elements* (`windows/scene`); a Deno/TypeScript client reads them and the committed `scene/tiles/v1` terrain, evaluates body positions/phases at any time, and animates on a 2D canvas — glowing star, computed-terminator moons, and the world as its real orthographic globe. The client owns all presentation; the sim never renders positions-over-time (decision 0022). The client follows the atlas's shape exactly (decision 0023): own Deno toolchain, own CI job, bundled to a committed `orrery.js`.

**Tech Stack:** Rust (edition 2024, `serde`/`serde_json` only) for the scene kind; Deno 2.9.2 + native TypeScript for the client; 2D canvas; mdBook for the book.

## Global Constraints

- **Determinism:** same seed → byte-identical `scene/system/v1`; the sim never touches wall-clock. The client's `requestAnimationFrame` drives *playback speed only*, never sim state. (spec §5)
- **Dependency allowlist:** Rust workspace stays `serde` + `serde_json` only (decision 0004). The client takes **no** dependency on the crates and only `jsr:@std/assert` for tests (decision 0022/0023), matching `clients/atlas/deno.json`.
- **Semantic-only scene data:** `scene/system/v1` emits physical elements — no colors, glyphs, projection hints, positions-over-time, or provider identity. (scene-protocol spec §2)
- **No `HashMap`/`HashSet`** in Rust; `BTreeMap`/`Vec` only. `#![warn(missing_docs)]` — every public item documented one line.
- **Deno version is exactly 2.9.2** (reproducible bundle; decision 0023).
- **Rust gate:** `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`, plus the artifact drift check. **Client gate (separate CI job):** `deno fmt --check && deno lint && deno task check && deno task test && deno task build` + `git diff --exit-code` on the bundle.
- **Never** `--no-verify`; run `cargo fmt` / `deno fmt` as the final step before each commit.

---

## File Structure

**Rust (produces the data + the contract):**
- Modify `windows/scene/src/lib.rs` — add `SYSTEM_SCHEMA`, `SystemScene`/`StarElem`/`WorldElem`/`MoonElem`, `system_scene(&World)`, `system_json(&SystemScene)`, tests.
- Create `windows/scene/examples/ephemeris_golden.rs` — writes the cross-language golden contract.
- Modify `cli/src/main.rs` — `cmd_scene` gains a `"system"` arm; usage/help; tests.
- Modify `.github/workflows/ci.yml` — add `scene system` + golden lines to "Artifacts are current"; remove the two `orrery … --cast` lines; add an `orrery` client CI job.
- Delete `book/src/gallery/orrery-seed-42.cast`, `book/src/gallery/orrery-emoji-seed-42.cast`.
- Create `book/src/gallery/scene-system-seed-42.json` (committed artifact), `clients/orrery/testdata/ephemeris-seed-42.json` (committed contract).

**Client (`clients/orrery/`, mirrors `clients/atlas/`):**
- `deno.json`, `src/main.ts`, `src/scene.ts`, `src/ephemeris.ts`, `src/palette.ts`, `src/moon.ts`, `src/globe.ts`, and a `_test.ts` beside each logic module.
- Bundles to `book/src/gallery/orrery.js` (committed).

**Book:**
- Modify `book/src/gallery/orrery-seed-42.md` (live client replaces cast players), `book/src/domains/astronomy.md` (orrery paragraph), `book/src/SUMMARY.md` (chronicle entry).
- Create `book/src/chronicle/NN-the-live-orrery.md`, `docs/retrospectives/campaign-NN.md`.
- Modify `docs/vision/idea-registry.md` (RENDER row).

Each task ends with an independently testable deliverable. Rust tasks 1–3 come first (the client needs their committed outputs); client tasks 4–10 follow; the book/DoD task closes.

---

### Task 1: The `scene/system/v1` types and builder (Rust)

**Files:**
- Modify: `windows/scene/src/lib.rs`
- Test: `windows/scene/src/lib.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `hornvale_worldgen::sky_of(&World) -> Result<Sky, BuildError>`, `Sky::system() -> Option<&StarSystem>`; `StarSystem { star, anchor, moons, forcing }`; `Star { luminosity: SolarLuminosities, class_name: String, habitable_zone: HabitableZone }`; `Anchor { orbit: Au, year: StdDays, rotation: Rotation, obliquity: Degrees }`; `Rotation::{Spinning { day: StdDays }, Locked}`; `Moon { distance: Megameters, period: StdDays, angular_diameter_rel: f64 }`; `OrbitalForcing { obliquity_mean: f64, year_phase_offset: f64, moon_phase_offsets: Vec<f64> }`; `HabitableZone::{inner, outer} -> Au`; all newtypes have `.get() -> f64`.
- Produces: `hornvale_scene::{SYSTEM_SCHEMA, SystemScene, StarElem, WorldElem, MoonElem, system_scene, system_json}`.

- [ ] **Step 1: Write the failing test.** Add to the `tests` module in `windows/scene/src/lib.rs`. It reuses the module's existing `world()` helper pattern but needs a *generated* sky, so add a local builder:

```rust
#[test]
fn system_scene_has_the_schema_moons_and_is_deterministic() {
    use hornvale_worldgen::{build_world, SkyChoice};
    use hornvale_kernel::Seed;
    let gen = || {
        build_world(
            Seed(42),
            &Default::default(),
            SkyChoice::Generated,
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 42 builds")
    };
    let scene = system_scene(&gen()).expect("generated world has a system");
    assert_eq!(scene.schema, "scene/system/v1");
    assert_eq!(scene.seed, 42);
    assert_eq!(scene.moons.len(), 2, "seed 42 has two moons");
    assert!(scene.world.year_days > 0.0);
    assert!(scene.world.day_length_days.is_some(), "seed 42 spins");
    assert!(scene.star.hz_inner_au < scene.star.hz_outer_au);
    // Byte-identical when serialized: determinism.
    assert_eq!(system_json(&scene), system_json(&system_scene(&gen()).unwrap()));
}

#[test]
fn system_scene_errors_on_a_constant_sun() {
    use hornvale_worldgen::{build_world, SkyChoice};
    use hornvale_kernel::Seed;
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Constant,
        &Default::default(),
        &Default::default(),
    )
    .unwrap();
    assert!(system_scene(&world).is_err(), "constant sun has no system");
}
```

- [ ] **Step 2: Run to verify it fails.** Run: `cargo test -p hornvale-scene system_scene`. Expected: FAIL (`system_scene`/`SystemScene` not found).

- [ ] **Step 3: Implement.** Add near `TILES_SCHEMA` and `tiles_scene` in `windows/scene/src/lib.rs` (import `Serialize` the same way `TilesScene` does):

```rust
/// The schema identifier for the system (orrery) scene kind.
pub const SYSTEM_SCHEMA: &str = "scene/system/v1";

/// The central star's semantic elements.
#[derive(Debug, Serialize)]
pub struct StarElem {
    /// Descriptive spectral class name (e.g. `"yellow dwarf (G)"`).
    pub class_name: String,
    /// Luminosity in solar luminosities.
    pub luminosity_rel: f64,
    /// Habitable-zone inner edge, AU.
    pub hz_inner_au: f64,
    /// Habitable-zone outer edge, AU.
    pub hz_outer_au: f64,
}

/// The anchor world's orbital and rotational elements.
#[derive(Debug, Serialize)]
pub struct WorldElem {
    /// Orbital radius, AU.
    pub orbit_au: f64,
    /// Year length, standard days.
    pub year_days: f64,
    /// Solar-day length, standard days; `None` when tidally locked (no spin).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub day_length_days: Option<f64>,
    /// Mean axial obliquity, degrees.
    pub obliquity_deg: f64,
    /// Genesis orbital phase offset (turns) so day 0 is an ordinary day.
    pub year_phase_offset: f64,
}

/// One moon's orbital elements.
#[derive(Debug, Serialize)]
pub struct MoonElem {
    /// Sidereal orbital period, standard days.
    pub sidereal_days: f64,
    /// Genesis synodic-phase offset (turns).
    pub phase_offset: f64,
    /// Orbital distance from the world, megameters.
    pub distance_mm: f64,
    /// Angular-diameter ratio (the size-word input).
    pub size_rel: f64,
}

/// One `scene/system/v1` document: the system's orbital geometry as elements.
#[derive(Debug, Serialize)]
pub struct SystemScene {
    /// Always `scene/system/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The central star.
    pub star: StarElem,
    /// The anchor world.
    pub world: WorldElem,
    /// The moons, generation order.
    pub moons: Vec<MoonElem>,
}

/// Build the `scene/system/v1` scene for `world`. Errors when the world has no
/// generated sky (the tier-0 constant sun has no orrery to draw).
pub fn system_scene(world: &World) -> Result<SystemScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let anchor = &system.anchor;
    let day_length_days = match &anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day } => Some(day.get()),
        hornvale_astronomy::Rotation::Locked => None,
    };
    let moons = system
        .moons
        .iter()
        .enumerate()
        .map(|(i, m)| MoonElem {
            sidereal_days: m.period.get(),
            phase_offset: system.forcing.moon_phase_offsets.get(i).copied().unwrap_or(0.0),
            distance_mm: m.distance.get(),
            size_rel: m.angular_diameter_rel,
        })
        .collect();
    Ok(SystemScene {
        schema: SYSTEM_SCHEMA.to_string(),
        seed: world.seed.0,
        star: StarElem {
            class_name: system.star.class_name.clone(),
            luminosity_rel: system.star.luminosity.get(),
            hz_inner_au: system.star.habitable_zone.inner().get(),
            hz_outer_au: system.star.habitable_zone.outer().get(),
        },
        world: WorldElem {
            orbit_au: anchor.orbit.get(),
            year_days: anchor.year.get(),
            day_length_days,
            obliquity_deg: system.forcing.obliquity_mean,
            year_phase_offset: system.forcing.year_phase_offset,
        },
        moons,
    })
}

/// Serialize a `SystemScene` to compact JSON (mirrors [`scene_json`]).
pub fn system_json(scene: &SystemScene) -> String {
    serde_json::to_string(scene).expect("a SystemScene always serializes")
}
```

If a needed type (`Rotation`, `SkyChoice`) is not already re-exported through `hornvale_worldgen`/`hornvale_astronomy` in this crate's imports, add the crate to `[dev-dependencies]` for tests only if it's test-side; for `system_scene` itself use the paths shown (`hornvale_astronomy::Rotation` — `hornvale-astronomy` is already a dependency of `windows/scene`).

- [ ] **Step 4: Run to verify it passes.** Run: `cargo test -p hornvale-scene system_scene`. Expected: PASS (both tests).

- [ ] **Step 5: Gate + commit.** Run `cargo fmt`, then `cargo clippy -p hornvale-scene --all-targets -- -D warnings`. Commit:

```bash
git add windows/scene/src/lib.rs
git commit -m "feat(scene): scene/system/v1 — the orrery's orbital elements"
```

---

### Task 2: CLI `scene system` + committed example + CI (Rust)

**Files:**
- Modify: `cli/src/main.rs` (`cmd_scene`, `usage`, tests)
- Modify: `.github/workflows/ci.yml`
- Create: `book/src/gallery/scene-system-seed-42.json` (generated, committed)

**Interfaces:**
- Consumes: `hornvale_scene::{system_scene, system_json}` (Task 1); existing `load_world`, `flag_value`.
- Produces: the `hornvale scene system` command and its committed artifact.

- [ ] **Step 1: Write the failing test.** In `cli/src/main.rs`'s `cmd_scene` test area, add:

```rust
#[test]
fn scene_system_emits_the_schema() {
    // Reuse the same world-on-disk fixture pattern the other cmd tests use, or
    // build via the REPL test's world helper; assert the command succeeds and
    // the JSON names the schema. If cmd_scene writes to stdout, refactor the
    // "system" arm to return the JSON string from a helper `system_scene_json`
    // that the test calls directly, mirroring how `tiles` is exercised.
    let json = hornvale_scene::system_json(
        &hornvale_scene::system_scene(&test_generated_world()).unwrap(),
    );
    assert!(json.contains("\"scene/system/v1\""));
    assert!(json.contains("\"moons\""));
}

#[test]
fn scene_unknown_kind_names_system() {
    let err = cmd_scene(&args(&["scene", "dioramas"])).unwrap_err();
    assert!(err.contains("system"), "known kinds must include system: {err}");
}
```

Add a `test_generated_world()` helper if none exists, using `world_builder::build_world(Seed(42), &Default::default(), world_builder::SkyChoice::Generated, &Default::default(), &Default::default()).unwrap()`.

- [ ] **Step 2: Run to verify it fails.** Run: `cargo test -p hornvale scene_`. Expected: FAIL (`system` arm missing; error string lacks "system").

- [ ] **Step 3: Implement.** In `cmd_scene`, add the arm and update the two error strings:

```rust
        Some("system") => {
            let world = load_world(args)?;
            let scene = hornvale_scene::system_scene(&world).map_err(|e| e.to_string())?;
            println!("{}", hornvale_scene::system_json(&scene));
            Ok(())
        }
        Some(other) => Err(format!("unknown scene kind '{other}'; known kinds: tiles, system")),
        None => Err("scene needs a kind; known kinds: tiles, system".to_string()),
```

In `usage()`, add a line under the existing `scene tiles` line:

```
  hornvale scene system [--world <PATH>]              emit scene/system/v1 JSON to stdout
```

- [ ] **Step 4: Run to verify it passes.** Run: `cargo test -p hornvale scene_`. Expected: PASS. Also confirm usage text test still passes.

- [ ] **Step 5: Generate the committed artifact and wire CI.** Run locally:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
cargo run -p hornvale -- scene system --world /tmp/hv-ci-sky.json > book/src/gallery/scene-system-seed-42.json
```

In `.github/workflows/ci.yml`, in the "Artifacts are current" step, add after the `scene tiles` line:

```yaml
          cargo run -p hornvale -- scene system --world /tmp/hv-ci-sky.json > book/src/gallery/scene-system-seed-42.json
```

- [ ] **Step 6: Gate + commit.** `cargo fmt`; `cargo clippy --workspace --all-targets -- -D warnings`; confirm `git diff --exit-code book/src/gallery/scene-system-seed-42.json` is clean after a regen. Commit:

```bash
git add cli/src/main.rs .github/workflows/ci.yml book/src/gallery/scene-system-seed-42.json
git commit -m "feat(cli): hornvale scene system + committed seed-42 example"
```

---

### Task 3: The cross-language ephemeris golden contract (Rust)

**Files:**
- Create: `windows/scene/examples/ephemeris_golden.rs`
- Create: `clients/orrery/testdata/ephemeris-seed-42.json` (generated, committed)
- Modify: `.github/workflows/ci.yml`

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world, SkyChoice}`; `hornvale_astronomy::Calendar::{year_phase, moon_phase}`; `hornvale_astronomy::units::StdDays`; `Sky::calendar()`.
- Produces: `clients/orrery/testdata/ephemeris-seed-42.json` — the values Task 6's `ephemeris.ts` must reproduce. Schema: `{"seed":42,"samples":[{"t":<days>,"world_phase":<0..1>,"rotation_phase":<0..1>,"moons":[<0..1>, …]}, …]}` where `world_phase = year_phase(t)`, `moons[i] = moon_phase(t,i)`, `rotation_phase = frac(t / day_length_days)`.

- [ ] **Step 1: Write the example.** `windows/scene/examples/ephemeris_golden.rs`:

```rust
//! Emit the ephemeris golden contract for seed 42: the authoritative orbital
//! phase, moon synodic phases, and world rotation phase at sample days, which
//! the orrery client's `ephemeris.ts` must reproduce from the emitted elements.
use hornvale_astronomy::units::StdDays;
use hornvale_kernel::Seed;
use hornvale_worldgen::{SkyChoice, build_world, sky_of};

fn main() {
    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 42 builds");
    let sky = sky_of(&world).expect("generated sky");
    let system = sky.system().expect("system");
    let cal = sky.calendar().expect("calendar");
    let day_length = match &system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day } => day.get(),
        hornvale_astronomy::Rotation::Locked => f64::INFINITY,
    };
    let mut rows = Vec::new();
    let mut d = 0.0_f64;
    while d < 365.0 {
        let t = StdDays::new(d).unwrap();
        let world_phase = cal.year_phase(t);
        let rotation_phase = if day_length.is_finite() {
            (d / day_length).rem_euclid(1.0)
        } else {
            0.0
        };
        let moons: Vec<f64> = (0..system.moons.len())
            .map(|i| cal.moon_phase(t, i).unwrap_or(0.0))
            .collect();
        let moons_json = moons
            .iter()
            .map(|p| format!("{p}"))
            .collect::<Vec<_>>()
            .join(",");
        rows.push(format!(
            "{{\"t\":{d},\"world_phase\":{world_phase},\"rotation_phase\":{rotation_phase},\"moons\":[{moons_json}]}}"
        ));
        d += 20.0;
    }
    println!("{{\"seed\":42,\"samples\":[{}]}}", rows.join(","));
}
```

- [ ] **Step 2: Generate and commit the contract.** Run:

```bash
mkdir -p clients/orrery/testdata
cargo run -p hornvale-scene --example ephemeris_golden > clients/orrery/testdata/ephemeris-seed-42.json
```

- [ ] **Step 3: Verify determinism.** Run the command a second time to a temp file and `diff` — must be byte-identical.

- [ ] **Step 4: Wire CI.** In `.github/workflows/ci.yml` "Artifacts are current", add:

```yaml
          cargo run -p hornvale-scene --example ephemeris_golden > clients/orrery/testdata/ephemeris-seed-42.json
```

and extend the drift check to cover `clients/orrery/testdata/` (the existing `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/` line gains `clients/orrery/testdata/`).

- [ ] **Step 5: Gate + commit.** `cargo fmt`; `cargo clippy -p hornvale-scene --all-targets -- -D warnings`. Commit:

```bash
git add windows/scene/examples/ephemeris_golden.rs clients/orrery/testdata/ephemeris-seed-42.json .github/workflows/ci.yml
git commit -m "test(orrery): commit the ephemeris golden contract for seed 42"
```

---

### Task 4: Client scaffold + CI job (Deno)

**Files:**
- Create: `clients/orrery/deno.json`, `clients/orrery/src/main.ts`
- Create/commit: `book/src/gallery/orrery.js` (bundle)
- Modify: `.github/workflows/ci.yml` (new `orrery` job)

**Interfaces:**
- Produces: a building, testing, bundling client harness — the base every later client task extends.

- [ ] **Step 1: Write `clients/orrery/deno.json`** (mirror `clients/atlas/deno.json`, note the output path):

```json
{
  "//": "Reproducible bundle requires Deno 2.9.2 exactly (see decision 0023 and the orrery CI job).",
  "tasks": {
    "check": "deno check src/",
    "test": "deno test --allow-read src/",
    "build": "deno bundle --platform browser --minify -o ../../book/src/gallery/orrery.js src/main.ts"
  },
  "imports": {
    "@std/assert": "jsr:@std/assert@^1"
  },
  "fmt": { "lineWidth": 100 },
  "lint": { "rules": { "tags": ["recommended"] } }
}
```

- [ ] **Step 2: Write a minimal `src/main.ts`** that compiles and does nothing yet (later tasks fill it):

```ts
/** Entry point for the live orrery client (filled in by later tasks). */
function boot(): void {
  const el = document.getElementById("orrery");
  if (el) el.textContent = "orrery loading…";
}
if (typeof document !== "undefined") boot();
```

- [ ] **Step 3: Build the bundle and verify the harness.** Run from `clients/orrery/`: `deno task check`, then `deno task build`. Expected: `book/src/gallery/orrery.js` written, no errors. Run `deno fmt` and `deno lint`.

- [ ] **Step 4: Add the CI job.** In `.github/workflows/ci.yml`, copy the `atlas:` job to an `orrery:` job with `working-directory: clients/orrery` and the bundle path `../../book/src/gallery/orrery.js`:

```yaml
  orrery:
    name: Orrery client
    runs-on: ubuntu-latest
    defaults:
      run:
        working-directory: clients/orrery
    steps:
      - uses: actions/checkout@v4
      - uses: denoland/setup-deno@v2
        with:
          deno-version: "2.9.2"
      - name: Format
        run: deno fmt --check
      - name: Lint
        run: deno lint
      - name: Typecheck
        run: deno task check
      - name: Test
        run: deno task test
      - name: Bundle is current (freshness check)
        run: |
          deno task build
          git diff --exit-code ../../book/src/gallery/orrery.js
```

- [ ] **Step 5: Commit.**

```bash
git add clients/orrery/deno.json clients/orrery/src/main.ts book/src/gallery/orrery.js .github/workflows/ci.yml
git commit -m "chore(orrery): scaffold the Deno client + CI job"
```

---

### Task 5: `scene.ts` — parse both scene documents

**Files:**
- Create: `clients/orrery/src/scene.ts`, `clients/orrery/src/scene_test.ts`

**Interfaces:**
- Produces: `interface SystemScene { schema; seed; star; world; moons }` (mirroring the Rust structs), `interface TilesScene` (copy the needed fields from `clients/atlas/src/scene.ts`), `parseSystem(text): SystemScene`, `parseTiles(text): TilesScene`, and `class SceneFormatError`.

- [ ] **Step 1: Write the failing test** `scene_test.ts`:

```ts
import { assertEquals, assertThrows } from "@std/assert";
import { parseSystem, SceneFormatError } from "./scene.ts";

const DOC = JSON.stringify({
  schema: "scene/system/v1",
  seed: 42,
  star: { class_name: "yellow dwarf (G)", luminosity_rel: 1, hz_inner_au: 0.9, hz_outer_au: 1.4 },
  world: { orbit_au: 1, year_days: 372, day_length_days: 1, obliquity_deg: 23, year_phase_offset: 0.1 },
  moons: [{ sidereal_days: 16, phase_offset: 0.4, distance_mm: 384, size_rel: 1 }],
});

Deno.test("parseSystem reads a valid document", () => {
  const s = parseSystem(DOC);
  assertEquals(s.seed, 42);
  assertEquals(s.moons.length, 1);
  assertEquals(s.world.dayLengthDays, 1);
});

Deno.test("parseSystem rejects the wrong schema", () => {
  assertThrows(() => parseSystem(JSON.stringify({ schema: "scene/tiles/v1" })), SceneFormatError);
});
```

- [ ] **Step 2: Run to verify it fails.** Run from `clients/orrery/`: `deno task test`. Expected: FAIL (module missing).

- [ ] **Step 3: Implement `scene.ts`.** Provide the interfaces (camelCase in TS; map from the snake_case JSON) and validating parsers. Include a `TilesScene` interface with `{ schema, width, height, sea_level_m, elevation_m }` (all the globe needs) and `parseTiles` that validates `schema === "scene/tiles/v1"`, `height*2 === width`, and `elevation_m.length === width*height`. Model the validation style on `clients/atlas/src/scene.ts` (a `fail()` helper throwing `SceneFormatError`, field-by-field checks). For `parseSystem`, validate `schema === "scene/system/v1"`, `moons` is an array, and map `day_length_days` (may be absent) to `world.dayLengthDays: number | null`.

- [ ] **Step 4: Run to verify it passes.** Run: `deno task test`. Expected: PASS.

- [ ] **Step 5: Gate + commit.** `deno fmt`, `deno lint`, `deno task check`. Commit:

```bash
git add clients/orrery/src/scene.ts clients/orrery/src/scene_test.ts
git commit -m "feat(orrery): parse scene/system/v1 and scene/tiles/v1"
```

---

### Task 6: `ephemeris.ts` — the evaluator, pinned to the golden contract

**Files:**
- Create: `clients/orrery/src/ephemeris.ts`, `clients/orrery/src/ephemeris_test.ts`

**Interfaces:**
- Consumes: `SystemScene` (Task 5); the committed `clients/orrery/testdata/ephemeris-seed-42.json` (Task 3).
- Produces: `worldPhase(sys, t)`, `moonPhase(sys, i, t)`, `rotationPhase(sys, t)` (all → `[0,1)`), and `synodicDays(sys, i): number | null`. Angles are derived by callers as `τ·phase`.

Physics (identical to the Rust calendar): `worldPhase = frac(t/year_days + year_phase_offset)`; `synodicDays = P·Y/(Y−P)` (null if `P ≥ Y`); `moonPhase = frac(t/synodicDays + phase_offset)`; `rotationPhase = frac(t/day_length_days)` (0 if locked).

- [ ] **Step 1: Write the failing test** `ephemeris_test.ts` — it loads the committed golden and asserts reproduction. It reads the emitted **elements** from the committed `scene-system-seed-42.json` so the test proves elements → phases matches the sim:

```ts
import { assertAlmostEquals } from "@std/assert";
import { parseSystem } from "./scene.ts";
import { moonPhase, rotationPhase, worldPhase } from "./ephemeris.ts";

const sys = parseSystem(
  await Deno.readTextFile("../../book/src/gallery/scene-system-seed-42.json"),
);
const golden = JSON.parse(
  await Deno.readTextFile("./testdata/ephemeris-seed-42.json"),
) as { samples: { t: number; world_phase: number; rotation_phase: number; moons: number[] }[] };

Deno.test("ephemeris reproduces the Rust golden phases", () => {
  for (const row of golden.samples) {
    assertAlmostEquals(worldPhase(sys, row.t), row.world_phase, 1e-9);
    assertAlmostEquals(rotationPhase(sys, row.t), row.rotation_phase, 1e-9);
    row.moons.forEach((p, i) => assertAlmostEquals(moonPhase(sys, i, row.t), p, 1e-9));
  }
});
```

Note the test reads `scene-system-seed-42.json` at `../../book/src/gallery/…`; `deno test --allow-read` already grants read. The golden lives at `./testdata/…` relative to `src/` — adjust the relative path if the test runner CWD differs (Deno runs tests from the package root, so `./testdata/…` and `../../book/…` resolve from `clients/orrery/`).

- [ ] **Step 2: Run to verify it fails.** Run: `deno task test`. Expected: FAIL (module missing).

- [ ] **Step 3: Implement `ephemeris.ts`:**

```ts
import type { SystemScene } from "./scene.ts";

const frac = (x: number) => x - Math.floor(x);

/** The world's orbital phase in [0,1): 0 at the genesis reference. */
export function worldPhase(sys: SystemScene, t: number): number {
  return frac(t / sys.world.yearDays + sys.world.yearPhaseOffset);
}

/** Synodic month of moon `i` (days), or null if it never laps the sun. */
export function synodicDays(sys: SystemScene, i: number): number | null {
  const p = sys.moons[i].siderealDays, y = sys.world.yearDays;
  return p >= y ? null : (p * y) / (y - p);
}

/** Moon `i`'s illumination phase in [0,1): 0 new, 0.5 full. */
export function moonPhase(sys: SystemScene, i: number, t: number): number {
  const syn = synodicDays(sys, i);
  if (syn === null) return 0;
  return frac(t / syn + sys.moons[i].phaseOffset);
}

/** The world's rotation phase in [0,1); 0 for a tidally locked world. */
export function rotationPhase(sys: SystemScene, t: number): number {
  const d = sys.world.dayLengthDays;
  return d === null ? 0 : frac(t / d);
}
```

(Ensure Task 5's `SystemScene` uses `yearDays`, `yearPhaseOffset`, `dayLengthDays`, and moon `siderealDays`/`phaseOffset` — camelCase — matching these accessors.)

- [ ] **Step 4: Run to verify it passes.** Run: `deno task test`. Expected: PASS (golden reproduced within 1e-9).

- [ ] **Step 5: Gate + commit.** `deno fmt`, `deno lint`, `deno task check`. Commit:

```bash
git add clients/orrery/src/ephemeris.ts clients/orrery/src/ephemeris_test.ts
git commit -m "feat(orrery): ephemeris evaluator pinned to the Rust golden"
```

---

### Task 7: `palette.ts` — elevation ramp + star tint

**Files:**
- Create: `clients/orrery/src/palette.ts`, `clients/orrery/src/palette_test.ts`

**Interfaces:**
- Produces: `elevationColor(elevation, seaLevel): [number, number, number]` (copied verbatim from `clients/atlas/src/palette.ts`), and `starTint(className): [number, number, number]` (spectral class → warm/cool RGB).

- [ ] **Step 1: Write the failing test** `palette_test.ts`:

```ts
import { assert, assertEquals } from "@std/assert";
import { elevationColor, starTint } from "./palette.ts";

Deno.test("ocean is bluer than land at the same delta", () => {
  const sea = elevationColor(0, 400);
  const land = elevationColor(800, 400);
  assert(sea[2] > sea[0], "ocean is blue-dominant");
  assert(land[1] >= land[2], "land is not blue-dominant");
});

Deno.test("star tint is total and hot≠cool", () => {
  const g = starTint("yellow dwarf (G)");
  const m = starTint("red dwarf (M)");
  assertEquals(g.length, 3);
  assert(g[0] + g[1] > m[1] + m[2] || g[2] > m[2], "distinct by class");
});
```

- [ ] **Step 2: Run to verify it fails.** Run: `deno task test`. Expected: FAIL.

- [ ] **Step 3: Implement `palette.ts`.** Copy `lerp` and `elevationColor` verbatim from `clients/atlas/src/palette.ts` (do not re-derive — the ramp must match the atlas and the elevation raster). Add `starTint`, keyed on the spectral letter in the class name parenthetical (mirror `render.rs`'s `star_color` mapping: O/B blue-white, A/F white, G warm yellow, K orange, M red), defaulting to warm yellow when no letter is found:

```ts
/** Warm/cool RGB tint for a star, by the spectral letter in its class name. */
export function starTint(className: string): [number, number, number] {
  const m = className.match(/\(([OBAFGKM])\)/);
  switch (m?.[1]) {
    case "O": case "B": return [170, 200, 255];
    case "A": case "F": return [245, 245, 255];
    case "K": return [255, 180, 90];
    case "M": return [230, 110, 80];
    default: return [255, 214, 120]; // G / unknown
  }
}
```

- [ ] **Step 4: Run to verify it passes.** Run: `deno task test`. Expected: PASS.

- [ ] **Step 5: Gate + commit.** `deno fmt`, `deno lint`, `deno task check`. Commit:

```bash
git add clients/orrery/src/palette.ts clients/orrery/src/palette_test.ts
git commit -m "feat(orrery): elevation ramp (from atlas) + star tint"
```

---

### Task 8: `moon.ts` — the computed terminator disc

**Files:**
- Create: `clients/orrery/src/moon.ts`, `clients/orrery/src/moon_test.ts`

**Interfaces:**
- Consumes: `moonPhase` semantics (phase in `[0,1)`: 0 new, 0.5 full).
- Produces: `illuminatedFraction(phase): number` (`(1−cos(2π·phase))/2`), and `drawMoon(ctx, cx, cy, r, phase, sunAngle)` — draws the disc with a terminator sized by the fraction and rotated so the lit limb faces `sunAngle` (radians, screen frame, 0 = +x). Split the pure geometry into a tested helper `terminatorRadius(phase): number` = `r·|1−2k|` scaled; test that, not the canvas.

- [ ] **Step 1: Write the failing test** `moon_test.ts`:

```ts
import { assert, assertAlmostEquals } from "@std/assert";
import { illuminatedFraction, litOffset } from "./moon.ts";

Deno.test("fraction: new 0, full 1, quarter 0.5", () => {
  assertAlmostEquals(illuminatedFraction(0), 0, 1e-9);
  assertAlmostEquals(illuminatedFraction(0.5), 1, 1e-9);
  assertAlmostEquals(illuminatedFraction(0.25), 0.5, 1e-9);
});

Deno.test("terminator offset: half at quarter, full disc at new/full", () => {
  // litOffset returns the terminator ellipse x-radius as a fraction of r:
  // |1-2k|. Quarter (k=.5) → 0 (straight terminator); new/full → 1.
  assertAlmostEquals(litOffset(0.25), 0, 1e-9);
  assertAlmostEquals(litOffset(0.5), 1, 1e-9);
  assert(litOffset(0.125) > 0 && litOffset(0.125) < 1);
});
```

- [ ] **Step 2: Run to verify it fails.** Run: `deno task test`. Expected: FAIL.

- [ ] **Step 3: Implement `moon.ts`:**

```ts
const TAU = Math.PI * 2;

/** Illuminated fraction (0 new → 1 full) for a synodic phase in [0,1). */
export function illuminatedFraction(phase: number): number {
  return (1 - Math.cos(TAU * phase)) / 2;
}

/** Terminator ellipse x-radius as a fraction of the disc radius: |1−2k|. */
export function litOffset(phase: number): number {
  return Math.abs(1 - 2 * illuminatedFraction(phase));
}

/**
 * Draw a moon: dark disc, sunward semicircle lit, then a terminator ellipse
 * that carves a crescent (k<½) or extends a gibbous (k>½). The whole is
 * rotated so the lit limb faces `sunAngle` (screen radians, 0 = +x).
 */
export function drawMoon(
  ctx: CanvasRenderingContext2D,
  cx: number,
  cy: number,
  r: number,
  phase: number,
  sunAngle: number,
  lit = "#ece6cf",
  dark = "#31363f",
): void {
  const k = illuminatedFraction(phase);
  ctx.save();
  ctx.translate(cx, cy);
  ctx.rotate(sunAngle); // local frame: sun toward +x
  ctx.beginPath();
  ctx.arc(0, 0, r, 0, TAU);
  ctx.fillStyle = dark;
  ctx.fill();
  ctx.beginPath();
  ctx.arc(0, 0, r, -Math.PI / 2, Math.PI / 2, false); // sunward (right) half
  ctx.closePath();
  ctx.fillStyle = lit;
  ctx.fill();
  ctx.beginPath();
  ctx.ellipse(0, 0, r * litOffset(phase), r, 0, 0, TAU);
  ctx.fillStyle = k < 0.5 ? dark : lit;
  ctx.fill();
  ctx.restore();
}
```

- [ ] **Step 4: Run to verify it passes.** Run: `deno task test`. Expected: PASS. (`drawMoon` is exercised visually in the book; the pure `illuminatedFraction`/`litOffset` are unit-tested.)

- [ ] **Step 5: Gate + commit.** `deno fmt`, `deno lint`, `deno task check`. Commit:

```bash
git add clients/orrery/src/moon.ts clients/orrery/src/moon_test.ts
git commit -m "feat(orrery): computed-terminator moon disc"
```

---

### Task 9: `globe.ts` — the real terrain as an orthographic globe

**Files:**
- Create: `clients/orrery/src/globe.ts`, `clients/orrery/src/globe_test.ts`

**Interfaces:**
- Consumes: `TilesScene` (`elevation_m`, `sea_level_m`, `width`, `height`), `elevationColor` (Task 7).
- Produces: `sample(px, py, r, rotationTurns, tiltDeg): {lat, lon} | null` (inverse orthographic projection of a disc pixel offset `(px,py)` in [−1,1]×[−1,1] to sphere coords, or null if outside the disc), `isLit(px, py, sunDir): boolean` (day side toward the on-screen sun direction), and `drawGlobe(ctx, cx, cy, r, tiles, rotationTurns, tiltDeg, sunAngle)` writing pixels via an `ImageData`.

Projection model (spec §4): a disc pixel at normalized `(x,y)` (y down) with `x²+y²≤1` has `z=√(1−x²−y²)` toward the viewer. Undo axial tilt `ε` (north pole leans by ε in the screen plane): `x' = x·cosε + y·sinε`, `y' = −x·sinε + y·cosε`, `z' = z`. Then `lat = asin(−y')` (north is −y on screen), `lon = atan2(x', z') + rotationTurns·360`. Sample `elevation_m` at `col = frac(lon/360)·width`, `row = ((90−lat)/180)·height`. Lit iff the surface normal (the view point `(x,y,z)`) dotted with the on-screen sun direction `(cos sunAngle, sin sunAngle, 0)` > 0; darken the night side by blending toward black.

- [ ] **Step 1: Write the failing test** `globe_test.ts`:

```ts
import { assert, assertAlmostEquals } from "@std/assert";
import { isLit, sample } from "./globe.ts";

Deno.test("disc center maps to lon = rotation, lat ~ 0 (no tilt)", () => {
  const s = sample(0, 0, 1, 0.25, 0)!;
  assertAlmostEquals(s.lat, 0, 1e-9);
  assertAlmostEquals(((s.lon % 360) + 360) % 360, 90, 1e-6); // 0.25 turn = 90°
});

Deno.test("outside the disc is null", () => {
  assert(sample(0.99, 0.99, 1, 0, 0) === null);
});

Deno.test("terminator: the point toward the sun is lit, opposite dark", () => {
  // sun to the right (angle 0): the right limb pixel is lit, the left dark.
  assert(isLit(0.6, 0, 0) === true);
  assert(isLit(-0.6, 0, 0) === false);
});
```

- [ ] **Step 2: Run to verify it fails.** Run: `deno task test`. Expected: FAIL.

- [ ] **Step 3: Implement `globe.ts`** per the projection model above. `sample(px,py,r,...)` takes already-normalized `(px,py)` in [−1,1] (the test passes normalized coords directly; `drawGlobe` converts pixel offsets to normalized by dividing by `r`). `isLit(x,y,sunAngle)` computes `z=√(max(0,1−x²−y²))` and returns `x·cos(sunAngle)+y·sin(sunAngle) > 0` (z-term drops since the sun direction has no z). `drawGlobe` allocates an `ImageData(2r,2r)`, loops pixels, calls `sample`+`elevationColor`, blends toward `[8,10,20]` on the night side (e.g. lit ? color : mix(color, night, 0.72)), and `ctx.putImageData` at `(cx−r, cy−r)`. A tidally-locked world passes `rotationTurns` frozen to the sub-solar longitude so one face holds toward the star (caller's concern; `globe.ts` just renders the given rotation).

- [ ] **Step 4: Run to verify it passes.** Run: `deno task test`. Expected: PASS.

- [ ] **Step 5: Gate + commit.** `deno fmt`, `deno lint`, `deno task check`. Commit:

```bash
git add clients/orrery/src/globe.ts clients/orrery/src/globe_test.ts
git commit -m "feat(orrery): orthographic terrain globe with day/night terminator"
```

---

### Task 10: `main.ts` — canvas loop, controls, and the book embed

**Files:**
- Modify: `clients/orrery/src/main.ts`
- Rebuild: `book/src/gallery/orrery.js`
- Modify: `book/src/gallery/orrery-seed-42.md`, `book/src/domains/astronomy.md`
- Delete: `book/src/gallery/orrery-seed-42.cast`, `book/src/gallery/orrery-emoji-seed-42.cast`
- Modify: `.github/workflows/ci.yml` (remove the two `orrery … --cast` lines)

**Interfaces:**
- Consumes: everything above (`parseSystem`/`parseTiles`, `worldPhase`/`moonPhase`/`rotationPhase`/`synodicDays`, `drawMoon`, `drawGlobe`, `elevationColor`/`starTint`).
- Produces: the running client (no unit test — verified by building the book and watching it, per spec §5). A single pure helper `clockToDay(elapsedMs, speed, yearDays): number` is unit-tested.

- [ ] **Step 1: Write the failing test** for the one pure helper, `clients/orrery/src/main_test.ts` — but keep playback math in a separate importable module `clock.ts` if `main.ts` imports `document` at top level (Deno test can't import DOM). Create `src/clock.ts`:

```ts
/** Map elapsed real ms and a speed (days per second) to sim-day time. */
export function clockToDay(elapsedMs: number, daysPerSecond: number): number {
  return (elapsedMs / 1000) * daysPerSecond;
}
```

and `src/clock_test.ts`:

```ts
import { assertAlmostEquals } from "@std/assert";
import { clockToDay } from "./clock.ts";
Deno.test("clock maps seconds to days by speed", () => {
  assertAlmostEquals(clockToDay(1000, 30), 30, 1e-9);
  assertAlmostEquals(clockToDay(500, 30), 15, 1e-9);
});
```

- [ ] **Step 2: Run to verify it fails, then implement `clock.ts`, then pass.** Run: `deno task test` (FAIL → implement the one-liner → PASS).

- [ ] **Step 3: Implement `main.ts`** — the render loop and controls. It: fetches `./scene-system-seed-42.json` and `./scene-tiles-seed-42.json`; parses both; sizes a canvas; and on each frame at sim-day `t` draws: faint HZ + orbit rings (from `hz_inner_au`/`hz_outer_au`/`orbit_au`, scaled), the star glow (`starTint`) at center, the world globe (`drawGlobe` at the world's screen position `θ = τ·worldPhase(t)`, rotation `rotationPhase(t)`, tilt `obliquity_deg`, `sunAngle` = screen direction from world to center), and each moon (`drawMoon` at its orbital angle `θ + π + moonPhase·τ`, radius schematic by moon index but ordered by `distance_mm`, `sunAngle` = screen direction from moon to center). Controls: a play/pause button (toggles the RAF loop), a range input scrubbing `t` over `[0, yearDays)`, and a speed range (days/sec) feeding `clockToDay`. Use `requestAnimationFrame`; the scrubber sets `t` directly and pauses autoplay. Keep it dependency-free and DOM-guarded (`if (typeof document …)`).

- [ ] **Step 4: Swap the gallery page.** Replace the body of `book/src/gallery/orrery-seed-42.md` — drop both `<div id="orrery*">` blocks and the `<script>` with the two `AsciinemaPlayer.create` calls; add the canvas, the controls, and `<script type="module" src="./orrery.js"></script>`. New content:

```markdown
# The Orrery of Seed 42

The seed-42 system, computed live in your browser from the data the simulation
emitted — [`scene-system-seed-42.json`](./scene-system-seed-42.json) (the orbital
elements) and [`scene-tiles-seed-42.json`](./scene-tiles-seed-42.json) (the real
terrain). The star is drawn from its class, the moons show their true phase, and
the world is its own globe — its actual continents, spinning at its day rate,
tilted at its obliquity, with the day/night line sweeping across.

<div id="orrery-holder">
  <canvas id="orrery-canvas" width="640" height="640" style="max-width:100%"></canvas>
  <div id="orrery-controls" style="font-family:monospace;margin-top:.5em"></div>
</div>

<script type="module" src="./orrery.js"></script>

---

*Generated deterministically: this seed always yields this system.*
```

- [ ] **Step 5: Delete the casts and their CI lines.** `git rm book/src/gallery/orrery-seed-42.cast book/src/gallery/orrery-emoji-seed-42.cast`. In `.github/workflows/ci.yml`, remove the two `cargo run -p hornvale -- orrery … --cast …` lines. (Keep the `hornvale orrery` CLI command and `render.rs` — untouched.)

- [ ] **Step 6: Update the astronomy chapter.** In `book/src/domains/astronomy.md`, revise the orrery paragraph: the gallery now shows the *live* client — the computed globe of the world's real terrain, interactive (play/scrub/speed) — while `hornvale orrery` remains the terminal render. Keep it at the book's altitude.

- [ ] **Step 7: Build, gate, watch.** From `clients/orrery/`: `deno fmt`, `deno lint`, `deno task check`, `deno task test`, `deno task build`. Then `mdbook build book` and open `book/book/gallery/orrery-seed-42.html` — confirm the orrery animates, moons face the star, the globe shows real continents with a moving terminator, and the controls work. Confirm `git diff --exit-code book/src/gallery/orrery.js` is clean after a rebuild.

- [ ] **Step 8: Commit.**

```bash
git add clients/orrery/src book/src/gallery/orrery.js book/src/gallery/orrery-seed-42.md book/src/domains/astronomy.md .github/workflows/ci.yml
git rm book/src/gallery/orrery-seed-42.cast book/src/gallery/orrery-emoji-seed-42.cast
git commit -m "feat(orrery): live interactive client replaces the cast players"
```

---

### Task 11: Book Definition of Done — chronicle, retrospective, registry, freshness

**Files:**
- Create: `book/src/chronicle/NN-the-live-orrery.md`, `docs/retrospectives/campaign-NN.md`
- Modify: `book/src/SUMMARY.md`, `docs/vision/idea-registry.md`

**Interfaces:** none (documentation).

- [ ] **Step 1: Confirm the campaign number.** Run `git log --oneline -20` and check `book/src/SUMMARY.md`'s chronicle list + `docs/retrospectives/` for the next free sequence number (decision 0017: forward-only). Use that `NN` consistently. **Also re-check `git merge-base HEAD main` and main's numbering for parallel-campaign collisions before finalizing NN.**

- [ ] **Step 2: Write the chronicle entry** `book/src/chronicle/NN-the-live-orrery.md` at the book's altitude: the story from the terminal orrery's four-glyph limits to a computed browser client — `scene/system/v1` as the exocentric sibling of the awaited situated pole, the client reading sim data (0022/0023), the real-terrain globe, and the moons finally lit correctly and continuously. Add its line to `book/src/SUMMARY.md` under the chronicle list.

- [ ] **Step 3: Write the retrospective** `docs/retrospectives/campaign-NN.md` (decision 0020): process lessons — the two-language golden contract, following the atlas's grain making the client cheap, replacing (not duplicating) the casts, the visual-companion moon-rendering decision.

- [ ] **Step 4: Add the registry row.** In `docs/vision/idea-registry.md`, add a RENDER-cluster row for `scene/system/v1` + the orrery client (the exocentric system pole, sibling to the situated pole the atlas awaits), status `shipped`, pointing at this spec. Run `cargo test -p hornvale --test docs_consistency` to confirm links/IDs resolve.

- [ ] **Step 5: Freshness sweep.** Grep the book for stale references to the orrery casts (`orrery-seed-42.cast`, "asciinema", "phase emoji") and fix any that now misdescribe reality. Confirm no SUMMARY entry references a deleted file.

- [ ] **Step 6: Full gate + commit.** Run the complete Rust gate and the client gate; `mdbook build book`. Commit:

```bash
git add book/src/chronicle/NN-the-live-orrery.md book/src/SUMMARY.md docs/retrospectives/campaign-NN.md docs/vision/idea-registry.md
git commit -m "docs(book): chronicle, retrospective, and registry for the live orrery"
```

---

## Self-Review

- **Spec coverage:** §2 scene kind → Tasks 1–2; §5 golden/testing → Task 3, 6; §3 client modules → Tasks 4–10 (scene/ephemeris/palette/moon/globe/main); §4 rendering → Tasks 7–10; §6 book/CLI fallout → Task 10 (casts deleted, CLI kept) + Task 11; §8 consequences → covered across Tasks 2, 10, 11. Non-goals (§7) respected: no situated pole, no Milankovitch drift (mean obliquity only), schematic moon radius, 2D canvas.
- **Type consistency:** the Rust snake_case JSON (`day_length_days`, `year_phase_offset`, `sidereal_days`, `phase_offset`) is mapped to camelCase TS accessors (`dayLengthDays`, `yearPhaseOffset`, `siderealDays`, `phaseOffset`) in Task 5 and used consistently in Tasks 6/10. `worldPhase`/`moonPhase`/`rotationPhase`/`synodicDays` names match between Task 6's definition and Task 10's use.
- **Placeholder scan:** every code step carries real code; the two artifact files and the bundle are generated by exact commands; `NN` is resolved in Task 11 Step 1 (the one deliberately deferred value, per decision 0017).
- **Golden contract:** the phase formulas are duplicated in Rust (Task 3 example, via the calendar) and TS (Task 6), and Task 6's test pins TS to the Rust-emitted golden within 1e-9 — the two-source-of-truth risk the spec flagged is closed.
