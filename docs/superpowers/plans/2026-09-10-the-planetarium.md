# The Planetarium Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Deliver a beautiful, inspectable Bevy astronomical scene and a verified ten-second 4K moving study made from the same scene.

**Architecture:** An independent `clients/visual` workspace contains a native observation source, a reusable Bevy view library, and the Planetarium composition application. Evaluated astronomy belongs to Hornvale; serialized documents cross into the renderer. Film direction belongs to Planetarium, while cameras, observation handling, materials and capture belong to the reusable view where actually shared.

**Tech Stack:** Rust 1.96.1; Bevy exactly 0.19.1; serde/serde_json; client-side SHA-256; PNG frames; external ffmpeg/ffprobe. Commit the client workspace lockfile. Existing simulation dependency limits remain in force.

**Spec:** [The Planetarium approved design](../specs/2026-09-10-the-planetarium-design.md)

**Ledger:** [Decisions and evidence](../ledgers/2026-09-10-the-planetarium.md)

**Status:** Execution in progress. Tasks 1–9 local implementation and evidence are independently reviewed; Stages 1–3 have green canonical reports. The final clean 4K package, full moving review and performance/repeatability measurements are complete. Whole-branch technical review is approved with documentation corrections recorded; final canonical stage and census accounting are complete. Nathan approved the final visual direction and merge at G6 on 2026-09-11; canonical landing remains pending.

## Global Constraints

- Final output: 3840×2160, 30 fps, 300 frames sampled over `[0, 10)` seconds.
- Interactive target: 30 fps at 1920×1080 on the development Mac; measure it.
- First GPU witness: Apple M1 Max, 32 GPU cores, Metal 3. Record actual OS/backend/adapter at execution; the inventory is not a render qualification.
- Preserve physical body-size and distance ratios within every shot. Neither vertical exaggeration nor enlarged moons are implicit art-direction permissions.
- No Bevy, GPU, application or new external dependency in the simulation workspace. No simulation dependency, even transitively, in `hornvale-bevy-view`; neither library depends on Planetarium.
- No second orbital integrator, CLI-per-frame world generation, guessed moon spin, missing values replaced with zero, or implicit omniscient source inside the renderer.
- The pilot's source is scientific/unrestricted. Future situated gameplay needs a distinct permitted producer and mirror; a UI visibility switch is insufficient.
- Cosmetic appearance is stable for its presentation seed, recorded, and creates no simulated facts or walkable moon terrain.
- Existing scene schemas retain their meaning and ordering. Add `scene/astronomy-at/v1` separately; no new simulation draws or save-epoch change are presumed.
- Source/observation-scope/world/request identities bind every reply and cached entity. Scrubbing never accepts an obsolete reply.
- Export follows semantic state and frame indices, not wall-clock time. PNG completion and encoding success are required; no static-film fallback.
- Human visual acceptance and merge were approved at G6 on 2026-09-11; publication is outside this campaign.
- Use subagent-driven development for execution, per Nathan's standing preference. Before each dispatch follow `dispatching-hornvale-subagents`, verify live signatures, and give the worker this spec, plan, exact worktree and branch. Planning self-review is local.
- Use one existing integration-test binary per simulation crate. Do not add a top-level test target for every module.
- Local iteration: scoped checks and `make gate-commit`. Stage/merge/heavy/census run through the canonical queue. GPU evidence on the Mac does not authorize a local workspace stage gate.
- Inspect actual artifact diffs: new audit declarations → regenerate and review; changed existing semantic fixtures → classify the cause before accepting; changed world/save behavior → record a determinism/schema ruling. Never predict an empty regeneration.
- After three failed attempts at one issue, record evidence and reassess before another approach.

## Verified starting points and toolchain

Planning source revision: `27461440c` (implementation has not begun). Seed 42 was freshly generated and exported twice: single topology, two moons, two wanderers, identical system JSON. It is a candidate, not a frozen composition.

Existing interfaces read during planning:

```rust
// hornvale_kernel
WorldTime::from_ticks(ticks: i64) -> WorldTime
WorldTime::ticks(self) -> i64
WorldTime::as_std_days(self) -> f64
WorldTime::TICKS_PER_STD_DAY // 100_000

// hornvale_astronomy
StdInstant::new(value: f64) -> Result<StdInstant, UnitError>
calendar_of(system: &StarSystem) -> Calendar
stellar_positions_at(system: &StarSystem, instant: StdInstant) -> Vec<OrbitalPosition>
wanderer_position_at(system: &StarSystem, index: usize, instant: StdInstant)
    -> Option<OrbitalPosition>
stellar_illumination_at(system: &StarSystem, instant: StdInstant) -> StellarIllumination
moon_ecliptic_longitude_deg(calendar: &Calendar, index: usize, t: StdInstant)
    -> Option<f64>
moon_ecliptic_latitude_deg(calendar: &Calendar, moon: &Moon, index: usize, t: StdInstant)
    -> Option<f64>
sub_solar_longitude_deg(calendar: &Calendar, t: StdInstant) -> f64

// hornvale_scene
SceneContext::build(world: &World) -> Result<SceneContext, SceneError>
system_scene(world: &World) -> Result<SystemScene, SceneError>
moons_scene(world: &World) -> Result<MoonsScene, SceneError>
```

`anchor_position_at` is currently private in `domains/astronomy/src/ephemeris.rs`.
The moon helpers use the solar/equinox longitude convention; the system-plane
positions need the documented half-turn conversion. Inclination/node and synodic
phase cannot be replaced with `time / sidereal_period` in a shader. Existing
moon geometry is an approximation; this campaign observes that model rather
than silently replacing the eclipse campaign's work.

The anchor has no physical radius: `domains/astronomy/src/anchor.rs::Anchor`
and the unit-sphere declaration in `domains/terrain/src/channel.rs` establish
this. Unlike the moon radius, it is not simply a missing export. Task 1 resolves
that prerequisite explicitly before any anchor mesh claims physical scale.

The tagged [Bevy Cargo manifest](https://github.com/bevyengine/bevy/blob/v0.19.1/Cargo.toml)
declares Rust 1.95.0 as its minimum. Hornvale's 1.96.1 exceeds that declaration;
Task 3 still has to build and qualify the real dependency graph. Use the tagged
[externally driven renderer](https://github.com/bevyengine/bevy/blob/v0.19.1/examples/app/externally_driven_headless_renderer.rs)
and [headless renderer](https://github.com/bevyengine/bevy/blob/v0.19.1/examples/app/headless_renderer.rs)
for image targets and readback lifecycle. Do not copy example frame-count warmup
as a guarantee of asset/shader readiness. The locally installed ffmpeg reported
8.1.1 during planning; package the version actually used.

## File ownership

| Area | Files and responsibility |
|---|---|
| Astronomy observation | `domains/astronomy/src/ephemeris.rs`: evaluated positions/orientation using existing models; tests in the existing astronomy suite |
| Evaluated scene | `windows/scene/src/astronomy_at.rs`: persistent astronomy context, semantic DTOs and serialization; `windows/scene/src/lib.rs`: exports only |
| CLI witness | `cli/src/main.rs`: thin `scene astronomy-at --ticks` arm; `cli/tests/suite/scene_astronomy_at_cli.rs`: command behavior |
| Native source | `clients/visual/source/src/{lib,protocol}.rs`: world lifecycle, static documents, exact queries and envelopes |
| Shared view | `clients/visual/bevy/src/{lib,documents,binding,coordinates,timeline,camera,capture}.rs`; `astronomy/{mod,surface,lighting}.rs`; `shaders/atmosphere.wgsl` |
| Application | `clients/visual/planetarium/src/{main,lib,bridge,pilot,shots,controls,package}.rs`; `films/pilot.json`: source selection, edit and presentation settings |
| Tests | Each visual crate has `tests/suite.rs` and modules under `tests/suite/`; small direct unit tests stay beside the implementation |
| Evidence | `docs/audits/the-planetarium/`: qualification, source-conformance, GPU, performance and final-review records; large capture packages live outside Git |
| Integration | `clients/visual/{Cargo.toml,Cargo.lock,README.md}`; `Makefile`; `scripts/visual-dependencies.py`; client/CLI gate tests |
| Documentation | Scene reference, client guide, book navigation, campaign decision records, chronicle/retrospective and reconciliation row |

New source files get focused responsibilities, not a single giant application.
Task 1 adds `domains/astronomy/src/anchor_radius.rs` and extends the existing astronomy model card. Changing
the physical model later requires source-model evidence, not a renderer edit.

## Shared contracts

These are **new** interfaces to implement, not claims about existing symbols.
The signatures below are the cross-task handoff; private helpers remain local.

### Native observation

```rust
// windows/scene/src/astronomy_at.rs
pub const ASTRONOMY_AT_SCHEMA: &str = "scene/astronomy-at/v1";
pub struct AstronomyContext { /* private: seed and initialized StarSystem */ }
impl AstronomyContext {
    pub fn build(world: &World) -> Result<Self, SceneError>;
}
pub fn astronomy_at_scene(world: &World, at: WorldTime)
    -> Result<AstronomyAtScene, SceneError>;
pub fn astronomy_at_scene_in(ctx: &AstronomyContext, at: WorldTime)
    -> Result<AstronomyAtScene, SceneError>;
pub fn astronomy_at_json(scene: &AstronomyAtScene) -> String;
```

`AstronomyAtScene` serializes these keys, in this order: `schema`, `seed`,
`ticks`, `ticks_per_std_day`, `frame`, `models`, `bodies`, `lights`.
`ticks` is an i64, with no JSON float conversion. `frame` is the literal
`anchor-centered-system-plane/km/right-handed`; +X is the ephemeris phase-zero
axis, +Y is the quarter-turn axis, +Z is the orbit normal. The anchor is the
origin. Subtract anchor positions in native f64 before emitting kilometre
coordinates; this avoids rounding two AU-sized values before subtraction.

A body record is `id: String`, `kind: String`, `position_km: [f64; 3]`,
`radius_km: Option<f64>`, `body_to_frame: Option<[[f64; 3]; 3]>`.
Serialize absent values as null and explain their meaning. The basis stores
three **columns**: local longitude 0, longitude 90°, north; its local sphere
convention matches `kernel::math::unit_sphere_from_lat_lon`.
`models` is an ordered list of named source approximations and validity notes.
IDs: `anchor`, `star:0`, `star:1`, `moon:N`, `wanderer:N`; use native catalog
indices as local IDs, then bind them to world/revision/scope in the envelope.
No ID is a globally stable identity across source revisions.

A light record is `star_id: String`, `direction_from_anchor: [f64; 3]`,
`flux_rel: f64`, `luminosity_rel: f64`. The unit direction, Earth-relative
unattenuated flux and current epoch luminosity come from the native stellar
evaluator. Other-body lighting uses the emitted current source positions and
luminosity, rather than substituting static catalog luminosity. Task 2 supplied
this evaluated contribution under the existing source-owned contract; Bevy does
not grow an independent time-dependent astronomy model. Do not apply stellar finite-disc
shadows without a verified stellar radius contract. Missing wanderer radius
means a point. Missing moon spin means recorded cosmetic material orientation,
not physical synchronous rotation inferred by the renderer.

### Native library boundary

```rust
// hornvale_visual_source; errors implement Display + std::error::Error
pub enum SourceError {
    Load(String), InvalidRequest(String), Observation(String), Serialize(String),
}
pub struct Source { /* private world, contexts, binding, static JSON */ }
impl Source {
    pub fn open(world_path: &std::path::Path, revision: &str, source_id: &str)
        -> Result<Self, SourceError>;
    pub fn initial_document(&mut self, tile_width: u32) -> Result<String, SourceError>;
    pub fn observe(&mut self, request_json: &str) -> Result<String, SourceError>;
}
```

Initial envelope `visual/initial/v1` contains `binding`, `system`, `moons`,
`tiles`, `ticks_per_std_day`. `binding` has `source_id`, `scope_id` (literal
`scientific:unrestricted` for this source), `world_sha256`, `source_revision`.
Hash the exact loaded world bytes, not seed alone. Pins and the whole loaded
world remain part of the recorded source files. Loading follows the existing
`World::load` plus `worldgen::register_all` discipline. No typed World or
StarSystem leaves this library. Static tile export uses one SceneContext.

Request `visual/request/v1`: `binding`, `request_id: u64`, `ticks: i64`.
Reply `visual/reply/v1`: the exact binding, request ID, ticks, and `astronomy`
document. Reject any requested binding the source does not own. Transport errors
are Results, never a valid-looking zero-state reply. A single worker owns Source;
application channels carry Strings. Thread/channel setup belongs to the app.

### View and timeline boundary

```rust
// hornvale_bevy_view; all are client-owned, no simulation imports
pub struct Binding {
    pub source_id: String, pub scope_id: String,
    pub world_sha256: String, pub source_revision: String,
}
pub enum ViewError { Document(String), Binding(String), Range(String), Capture(String) }
pub struct ObservationMirror { /* private parsed source state and pending identity */ }
impl ObservationMirror {
    pub fn new(initial_json: &str) -> Result<Self, ViewError>;
    pub fn request(&mut self, ticks: i64) -> Result<String, ViewError>;
    pub fn accept(&mut self, reply_json: &str) -> Result<bool, ViewError>;
    pub fn reset(&mut self, initial_json: &str) -> Result<(), ViewError>;
    pub fn current_ticks(&self) -> Option<i64>;
}
pub struct FilmClock { pub start_ticks: i64, pub end_ticks: i64, pub frames: u32 }
impl FilmClock { pub fn tick_at(&self, frame: u32) -> Result<i64, ViewError>; }
pub struct CameraPose {
    pub eye_km: [f64; 3], pub target_km: [f64; 3], pub up: [f64; 3],
    pub vertical_fov_radians: f64, pub focus_distance_km: f64,
}
pub struct VisualPlugin;
```

`accept` returns false for a well-formed obsolete reply and Err for malformed
or conflicting data. `request` advances a checked request counter; overflow is
an error. `reset` clears pending replies, entities, selection, material/static
bindings and render history; frame capture cannot span a reset.

For `0 <= frame < frames`, compute `start + round_away_from_zero(frame *
(end-start) / frames)` with checked i128 integer arithmetic, then checked i64
conversion. Round the signed **offset**, not an absolute float instant. The
presentation endpoint is never sampled. Quantization can repeat simulation ticks
and can round a final offset to the end tick when the interval is short; do not
clamp away that result. Playback and direct seeking call the same function. A still simulation interval (`start == end`) is allowed.

`VisualPlugin` owns observation application, camera primitives, body visuals
and capture systems. Planetarium inserts the mirror and sends serialized replies;
the plugin never discovers or starts a source. Readiness means a whole matching
observation has been applied, assets/shader pipelines are ready, and the selected
history policy has completed; it is not merely elapsed wall time.

## Stage 1 — a source-backed moving visual witness

**Goal:** Resolve physical prerequisites, expose evaluated observations, and render an actual attractive moving draft through the shared libraries.
**Success Criteria:** Source conformance passes; one declared topology works; a full-resolution still and at least two seconds of moving Bevy output exist with recorded settings.
**Tests:** Tasks 1–3; source/CLI equality; dependency containment; GPU image and motion inspection.
**Status:** Complete

### Task 1: add a documented physical anchor radius and qualify geometry

**Files:**
- Read: `domains/astronomy/src/{anchor,ephemeris,calendar,eclipses,moons,units}.rs`.
- Read: `domains/terrain/src/channel.rs`; `book/src/reference/scene-system-v1.md`.
- Create: `domains/astronomy/src/anchor_radius.rs`, `domains/astronomy/tests/suite/anchor_radius.rs`.
- Modify: `book/src/domains/astronomy.md` (existing model-card section).
- Modify: `domains/astronomy/src/lib.rs`, `domains/astronomy/tests/suite.rs`.
- Create: `docs/audits/the-planetarium/source-qualification.md`.
- Modify: campaign ledger/spec only for the resolved radius prerequisite.
- Test in the subsequent source implementation: `domains/astronomy/tests/suite/planetarium_geometry.rs`, registered in `domains/astronomy/tests/suite.rs`.

**Interfaces:** Consumes `EarthMasses` and the existing calendar/ephemeris models. Produces `anchor_radius(mass: EarthMasses) -> Result<Megameters, UnitError>` exported from `hornvale_astronomy`, plus a qualification record containing model provenance, orientation convention, validity range and reproducible seed/time search recipe. This is a derived observation over existing mass, not a new random draw or stored Anchor field.

- [x] Record Nathan's answer, “Include a physical-radius prerequisite”, as authorization for the source-side addition. Read the actual astronomy model-card and unit conventions before editing.
- [x] Implement model `earthlike-rocky-zeng2019-linear/v1`: a spherical bulk-radius approximation for the existing 0.5–2 Earth-mass anchor range, using the published Earth-like rocky curve (32.5% Fe, 67.5% MgSiO3). This is an explicit default composition assumption, not a claim that Hornvale has simulated interior composition. No atmospheric-envelope radius, oblateness, composition diversity or radius feedback into existing dynamics is introduced.

The author's [model page](https://lweb.cfa.harvard.edu/~lzeng/planetmodels.html)
identifies the composition and Earth-unit axes. The
[numerical curve](https://lweb.cfa.harvard.edu/~lzeng/tables/massradiusEarthlikeRocky.txt)
was retrieved during planning: 49 rows; SHA-256
`dcc5080f2186983b7e36200373878dc06a8d8083ec21ce1c4f670659c0404b38`.
Use this covering subset as frozen numerical reference data:

```rust
const EARTHLIKE_MASS_RADIUS: [(f64, f64); 8] = [
    (0.4093, 0.7725), (0.5304, 0.8330), (0.6835, 0.8964),
    (0.8756, 0.9625), (1.1150, 1.0309), (1.4114, 1.1015),
    (1.7763, 1.1741), (2.2233, 1.2485),
];
```

For adjacent points bracketing mass `m`, linearly interpolate
`r = r0 + (m-m0)*(r1-r0)/(m1-m0)` in Earth radii. Return exact table values at
knots. Convert with a declared Earth reference radius of 6371 km, consistent
with [NASA's volumetric mean value](https://nssdc.gsfc.nasa.gov/planetary/factsheet/plutofact.html):
`Megameters::new(r * 6.371)`. Model evaluation is supported only on `[0.5,2]`;
outside it return UnitError with the offending mass and a range explanation.
Use named/provenance-tagged constants and the project's normal type/plumb tags.
No network download occurs at runtime.

Do not use the simpler 2016 analytic power law over this whole interval: its
[published applicability starts at one Earth mass](https://arxiv.org/abs/1512.08827).
The covering table avoids silently extrapolating that formula below its range.
Linear interpolation is Hornvale's declared numerical approximation; it is not
an additional finding attributed to the authors.

- [x] Establish a compiling error-returning function seam, then capture behavioral red from independent reference tests before filling in interpolation:

```rust
#[test]
fn radius_matches_a_published_interior_reference_point() {
    let radius = anchor_radius(EarthMasses::new(1.115).unwrap()).unwrap();
    assert!((radius.get() - 1.0309 * 6.371).abs() < 1e-12);
}
#[test]
fn unsupported_mass_is_not_silently_extrapolated() {
    assert!(anchor_radius(EarthMasses::new(0.49).unwrap()).is_err());
}
```

- [x] Implement the checked bracket search and interpolation; test both supported boundaries, every interior tabulated reference, positive monotonic radius across the range, one-Earth-mass agreement within 1%, and rejection above 2. Verify existing generated-world/sky fixtures against their current baseline and classify any diff rather than assuming none.

```bash
cargo test -p hornvale-astronomy --test suite anchor_radius
```

- [x] Check source conventions with executable probes against the fresh candidate world. Use the existing CLI, not invented geometry, for the initial inventory:

```bash
work=$(mktemp -d)
target/debug/hornvale new --seed 42 --out "$work/world.json"
target/debug/hornvale scene system --world "$work/world.json" > "$work/system.json"
target/debug/hornvale scene moons --world "$work/world.json" > "$work/moons.json"
```

- [x] Record the world path/hash, exact commands, topology and catalog. Bound the alternative seed search to seeds 0–63 if composition requires another candidate. Freeze the final seed/pins/time only after the first actual look-development output. Record all attempted candidates; selection is not a prevalence claim.
- [x] Write conformance cases for anchor orbital position, tilted/retrograde/locked surface orientation, moon latitude/longitude/node at negative/zero/positive instants, and all three stellar topologies. For the anchor basis, transforming the calendar's subsolar lon/lat normal must agree with the native solar direction; basis columns must be orthonormal and right-handed. If existing source models cannot agree, document the discrepancy and narrow the source contract or resolve it at its owner—do not hide it in camera placement.
- [x] Record a conservative eclipse-avoidance procedure: inspect the existing eclipse export for the proposed interval, then inspect emitted source/body alignment for every final sample. Use a single-star candidate; mark any occultation/shadow with unsupported dimensions as unqualified and select another interval. This is avoidance evidence, not validation of the independent eclipse campaign.
- [x] Run scoped astronomy format/clippy/tests and the local commit gate; commit the radius implementation, model card and qualification record. Public-item audit changes are reviewed and committed with the code. A completed radius task is not yet a completed moving-visual stage.

### Task 2: ship the evaluated scene query and reusable native source

**Files:**
- Modify: `domains/astronomy/src/ephemeris.rs`, `domains/astronomy/tests/suite.rs`.
- Create: `domains/astronomy/tests/suite/planetarium_geometry.rs`.
- Create: `windows/scene/src/astronomy_at.rs`, `windows/scene/tests/suite/astronomy_at.rs`.
- Modify: `windows/scene/src/lib.rs`, `windows/scene/tests/suite.rs`, `cli/src/main.rs`, `cli/tests/suite.rs`.
- Create: `cli/tests/suite/scene_astronomy_at_cli.rs`.
- Create: `clients/visual/Cargo.toml`, `clients/visual/source/Cargo.toml`, `clients/visual/source/src/{lib,protocol}.rs`, `clients/visual/source/tests/suite.rs`, `clients/visual/source/tests/suite/source.rs`.
- Modify: root `Cargo.toml` excludes; create/update the visual lockfile.

**Interfaces:** Consumes Task 1's approved geometry/radius contract. Produces `AstronomyContext`, `AstronomyAtScene`, the three observation functions, `Source`, `SourceError`, and the serialized contracts above.

- [x] Capture a behavioral red on today's command surface before introducing new types:

```rust
#[test]
fn astronomy_at_accepts_exact_negative_ticks() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"))
        .args(["scene", "astronomy-at", "--world",
               "tests/fixtures/world-seed-42.json", "--ticks", "-1"])
        .output().unwrap();
    assert!(out.status.success(), "{}", String::from_utf8_lossy(&out.stderr));
    let doc: serde_json::Value = serde_json::from_slice(&out.stdout).unwrap();
    assert_eq!(doc["ticks"].as_i64(), Some(-1));
}
```

```bash
cargo test -p hornvale --test suite scene_astronomy_at_cli
```

The current unknown-subcommand behavior should be captured as actual evidence;
a missing module/import is not the behavioral red.

- [x] Add the smallest native geometry wrappers needed. Reuse stellar/wanderer/calendar/moon helpers and kernel libm. Keep orbital position derivation at its domain owner. Call Task 1's `anchor_radius(system.anchor.mass)` and convert its Megameters to emitted kilometres. Derive anchor basis from the existing calendar and source frame, with explicit half-turn and retrograde conventions. Emit unavailable moon spin/radii as null.
- [x] Implement AstronomyContext owning its initialized StarSystem and seed; `_in` queries take the context, not an unrelated World. The convenience world wrapper builds once for CLI use. Convert `WorldTime` to `StdInstant` only at the existing continuous ephemeris boundary. Use existing quantize-at-emit helpers, with field-aware precision tests for kilometre positions and basis vectors; retain full precision internally.
- [x] Add the CLI arm `scene astronomy-at --world PATH --ticks I64`; require ticks, reject malformed/out-of-range integers, and update every command help/known-kinds string found by searching its emitted text. Keep `scene system` byte behavior under the existing tests.
- [x] Create the independent workspace and source crate with optimized dev profiles matching `clients/game`. Initial members may contain just source until Task 3 adds view/app. Source owns immutable loaded-world bytes, registered World, AstronomyContext and one lazy terrain SceneContext. Hash world bytes client-side using SHA-256. Validate the caller's full revision format and binding; store it in all envelopes.
- [x] Add semantic source tests: native/CLI/source astronomy bytes agree for the same world and ticks; request order `0, 100000, -1, 0` returns identical astronomy for repeated 0; same seed with different pins has a different world binding; wrong scope/world/revision errors; static docs and astronomy context are reused without repeated world generation. Count generation/context construction through a test seam, not elapsed-time assertions.
- [x] Validate all three topologies with real generated/pinned fixtures, missing moon spin, absent wanderer radius, invalid query, and no-synodic-cycle absence. Record actual query costs separately from correctness tests.

```bash
cargo test -p hornvale-astronomy --test suite planetarium_geometry
cargo test -p hornvale-scene --test suite astronomy_at
cargo test -p hornvale --test suite scene_astronomy_at_cli
cargo test --manifest-path clients/visual/Cargo.toml -p hornvale-visual-source
```

- [x] Run scoped format/clippy; review required type/placement/plumb declarations and generated diffs; run the local commit gate and commit working source changes. Any new dependency outside clients must satisfy the existing allowlist, not a convenient exemption.

### Task 3: render a real scene through the reusable Bevy library

**Files:**
- Create: `clients/visual/bevy/Cargo.toml`, `src/{lib,documents,binding,coordinates,camera}.rs`, `src/astronomy/{mod,surface,lighting}.rs` under that crate.
- Create: `clients/visual/planetarium/Cargo.toml`, `src/{main,lib,bridge,pilot}.rs` under that crate.
- Create: `clients/visual/bevy/tests/suite.rs`, `tests/suite/{documents,coordinates,binding}.rs` under that crate.
- Create: `clients/visual/planetarium/films/pilot.json`, `docs/audits/the-planetarium/early-visual.md`.
- Modify: `clients/visual/Cargo.toml`, `Cargo.lock`, root excludes.

**Interfaces:** Consumes Source's serialized initial/reply documents. Produces `Binding`, `ViewError`, `ObservationMirror`, `CameraPose`, `VisualPlugin` and `planetarium inspect --world PATH --revision SHA --film PATH`. The application owns source worker/channels; the view accepts documents and owns no source.

- [x] Implement document parsing/validation first. Behavioral tests reject unknown schemas/frame/units, repeated body IDs, non-finite or invalid radii, and incomplete initial documents. Accept and preserve explicit nulls. Establish compiling parser stubs that return errors before capturing valid-document assertion failures.
- [x] Add Bevy `=0.19.1`, `default-features = false`, and an explicit feature list grounded in the tagged manifest: `std`, `async_executor`, `multi_threaded`, `bevy_asset`, `bevy_log`, `bevy_state`, `bevy_winit`, `x11`, `3d_bevy_render`, `ui_api`, `ui_bevy_render`, `default_font`, `png`. Verify the resolved features and Linux build prerequisites; remove only unused features deliberately. Avoid audio/gamepad dependencies the pilot does not need.
- [x] Bootstrap a real app using VisualPlugin and the source bridge. Begin with synchronous exact observations in a persistent app; full scrub scheduling comes in Task 4. Apply an entire snapshot before rendering it, keyed by `(binding, body_id)`.
- [x] Use camera-relative f64 subtraction and an explicit kilometres-per-render-unit scale before f32 conversion:

```rust
pub fn render_position(position_km: [f64; 3], origin_km: [f64; 3], km_per_unit: f64)
    -> Result<[f32; 3], ViewError>
{
    if !km_per_unit.is_finite() || km_per_unit <= 0.0 {
        return Err(ViewError::Range("km_per_unit must be finite and positive".into()));
    }
    let p = std::array::from_fn(|i| ((position_km[i] - origin_km[i]) / km_per_unit) as f32);
    if p.iter().any(|v| !v.is_finite()) {
        return Err(ViewError::Range("position exceeds rendering range".into()));
    }
    Ok(p)
}
```

Test translation invariance and angular screen error over the **declared**
camera range, including a large origin and a nearby moon. Bound projective
error to 0.25 pixel at 4K in those tests. Fail a supported-range check rather
than allowing float overflow or silently compressing distances.

- [x] Build the anchor from actual exported tiles, using the documented tile projection/order and elevation datum. Resolve sea level from its source; do not equate an isostatic elevation with height above sea level. Keep radial relief unexaggerated. Moon surface descriptors drive only disclosed cosmetic material variation. Missing-radius wanderers use sourced points. Use a quiet background and source-fed illumination; leave unsupported eclipse shadows disabled.
- [x] Start with Bevy PBR/tonemapping and a single camera. Select a lit limb composition and a readable moon using camera placement and cuts, not changed scale. Save an actual 4K still and at least 60 ordered 1080p frames using the tagged image-target screenshot approach; wait for each completion before advancing its source tick. Record this as draft capture evidence, not the final verified package.
- [x] Inspect the actual still and moving clip at desktop and phone size. Record which visual properties succeeded/failed and revise the cheapest consequential issue. If images remain schematic, profile the missing appearance work before advancing to production tooling; the deliverable includes an actual visual direction, not only successful initialization.
- [x] Commit CPU-tested code and the early-visual report, with paths/hashes for the actual files. Large frames/videos stay outside Git; preserve the review copies in the campaign artifact directory.

**Stage 1 boundary:** source correctness and a moving GPU witness both required. Update `IMPLEMENTATION_PLAN.md`, push the branch, and submit the full current SHA through `make sluice-stage BRANCH=campaign/the-planetarium REF=<full-sha>`. Use the submitting-to-the-sluice skill. Inspect the reported result; do not treat queued as passed.

## Stage 2 — exact time, interaction and authored direction

**Goal:** Make the source-backed scene seekable and inspectable, and define the final edit without tying it to rendering speed.
**Success Criteria:** Direct seek/playback equivalence, stale-reply rejection and scope resets pass; all camera/time controls work; the selected pilot and shot data are frozen.
**Tests:** Tasks 4–5, including semantic clock/state tests and real interactive inspection.
**Status:** Complete

### Task 4: exact clocks and asynchronous observation state

**Files:**
- Create: `clients/visual/bevy/src/timeline.rs`, `clients/visual/bevy/tests/suite/timeline.rs`.
- Modify: `clients/visual/bevy/src/{binding,lib}.rs`, its existing suite registration/binding tests, `clients/visual/planetarium/src/bridge.rs`.

**Interfaces:** Consumes ObservationMirror and the wire contracts. Produces FilmClock and a source bridge that coalesces interactive requests but waits for exact export replies. `ObservationMirror::accept` implements the documented obsolete/invalid distinction.

- [x] Add exact endpoint/tie/overflow behavioral tests against a compiling error-returning clock seam:

```rust
#[test]
fn three_frames_exclude_the_endpoint_and_round_signed_offsets() {
    let clock = FilmClock { start_ticks: 0, end_ticks: -5, frames: 3 };
    let actual: Vec<_> = (0..3).map(|i| clock.tick_at(i).unwrap()).collect();
    assert_eq!(actual, vec![0, -2, -3]);
}
#[test]
fn a_paused_simulation_still_has_a_presentation_timeline() {
    let clock = FilmClock { start_ticks: 77, end_ticks: 77, frames: 300 };
    assert_eq!(clock.tick_at(299).unwrap(), 77);
}
```

- [x] Implement checked i128 arithmetic. Reject zero frames/out-of-range indices and overflow; include exact half-tick ties in both directions and i64 boundary cases. No repeated addition or float seconds-to-ticks accumulation.
- [x] Implement one source-owning worker. A bounded channel/request slot prevents scrub floods from growing unbounded work; handle disconnect and worker errors in the visible application state. Coalesce only work not yet begun. Export disables coalescing for its outstanding frame.
- [x] Test requests A then B, replies B then A: B stays displayed; test world/scope/revision reset followed by the old reply: no old entities or data reappear. Reused IDs under a new binding must not preserve selection/material state. Test the empty rendered catalog immediately after a source reset and before its first reply, and a valid new source with no optional moons/wanderers. Old optional bodies must disappear; do not invent an anchorless astronomy document to exercise removal.
- [x] Separate simulation tick, presentation playhead and wall-clock diagnostics. Pause/reverse/seek updates a desired instant; the last committed observation can remain visible with an explicit pending indicator until its replacement arrives. Never label old physical state with the requested new tick.
- [x] Reset temporal history on discontinuity; record the reset policy as part of capture settings. Direct frame 150 and sequential playback to 150 must produce identical semantic JSON, camera input and caption selection. Pixel comparison is separately measured later.

```bash
cargo test --manifest-path clients/visual/Cargo.toml -p hornvale-bevy-view
cargo test --manifest-path clients/visual/Cargo.toml -p hornvale-visual-source
```

- [x] Commit after the applicable local gates; record measured query latency and pending behavior in the ledger.

### Task 5: direct the pilot and complete inspection controls

**Files:**
- Create: `clients/visual/planetarium/src/{shots,controls}.rs`, `clients/visual/planetarium/tests/suite.rs`, `clients/visual/planetarium/tests/suite/shots.rs`.
- Create: `clients/visual/bevy/shaders/atmosphere.wgsl` only if the qualified Bevy effect needs a custom material.
- Modify: `clients/visual/bevy/src/camera.rs`, `src/astronomy/{surface,lighting}.rs`, `clients/visual/planetarium/src/{lib,pilot}.rs`, `films/pilot.json`.

**Interfaces:** Consumes FilmClock, CameraPose, current mirrored body positions. Produces application-owned `FilmDefinition`, `Shot`, and `sample_shot(film: &FilmDefinition, frame: u32, body_positions: &std::collections::BTreeMap<String, [f64; 3]>) -> Result<CameraPose, ViewError>`; FilmDefinition contains dimensions/fps/frame count, binding, start/end ticks, presentation seed, ordered shots/captions/settings. A Shot contains start/end frame, target body ID, start/end camera offsets in kilometres, up, FOV, focus distance and caption. No source World appears in these types.

- [x] Write tests for exactly one shot per frame, boundary cuts at 90 and 210, no gap/overlap, missing target rejection, and identical camera/caption results regardless of query order. Pose interpolation uses a pure function of frame; use the same function in inspection reset and export.

```rust
pub fn smoothstep(u: f64) -> f64 {
    let u = u.clamp(0.0, 1.0);
    u * u * (3.0 - 2.0 * u)
}
```

Within a shot interpolate camera offsets with this curve and resolve its target
against the exact current observation. Do not interpolate physical body states.
A cut has a declared boundary and resets render history.

- [x] Author three initial shot ranges `[0,90)`, `[90,210)`, `[210,300)` following the spec's establish/limb/moon beats. Initial captions: “A world in motion”, “Turning into the light”, “Moons keep their own time”. Revise any caption the source witness does not support. Commit real measured camera values from Task 3, with no unevaluated preset placeholders.
- [x] Freeze the chosen source world/revision/pins, represented tick interval, supported camera bounds and selection/avoidance evidence. Film parsing rejects an incompatible binding, count/fps/dimensions, target identity or unsupported interval. Future production data need not follow these three beats; do not bake their ranges into VisualPlugin.
- [x] Add orbit (left drag), pan (middle/shift drag), dolly (wheel), focus selected body (F), reset authored pose (R), pause/play (Space), scrub slider, signed rate controls, and film/inspection toggle (Tab). Clamp dolly/focus to the declared range and above the body surface. Marker picking never changes physical body dimensions.
- [x] Refine lighting, color, terrain material, bounded atmosphere and selective focus from the real moving draft. Drive existing environmental features from exported values; distinguish a presentation haze from a simulated atmosphere profile. No sampled climate state is animated merely because the camera moves. Keep technical metadata in inspection and manifest, not film captions.
- [x] Inspect input focus conflicts, window resize, high-DPI text, phone-size captions and a camera move while simulation is paused. Record video evidence of controls, including reverse/scrub and a return to the authored pose.
- [x] Run the shot/binding/camera tests and applicable gates, then commit the frozen film definition and source-selection report.

**Stage 2 boundary:** submit a stage request for the tested full SHA; record controls/seek evidence and queue result before marking complete.

## Stage 3 — complete 4K capture and verifiable packages

**Goal:** Capture every exact frame from the same scene and assemble a package that cannot mistake a partial or mismatched render for success.
**Success Criteria:** 300 correct PNGs, MP4, per-frame source/camera records and hash verification; interruption/encoding failures remain incomplete.
**Tests:** Tasks 6–7; small GPU runs followed by the full study.
**Status:** Complete; canonical report recorded in the ledger.

### Task 6: make GPU capture an acknowledged frame pipeline

**Files:**
- Create: `clients/visual/bevy/src/capture.rs`, `clients/visual/bevy/tests/suite/capture.rs`.
- Modify: `clients/visual/bevy/src/lib.rs`, suite registration, `clients/visual/planetarium/src/{main,lib}.rs`.

**Interfaces:** Consumes exact observation readiness and CameraPose. Produces `CaptureSettings { width: u32, height: u32, frames: u32, warmup_frames: u32, timeout_seconds: u32 }`, `CaptureState` and `planetarium capture --world PATH --revision SHA --film PATH --out DIR`.

```rust
pub enum CaptureState {
    Preparing,
    AwaitingObservation { frame: u32 },
    Warming { frame: u32, remaining: u32 },
    AwaitingReadback { frame: u32 },
    Writing { frame: u32 },
    Complete,
    Failed(String),
}
```

Every callback carries the frame ID it was issued for; only a matching outstanding
frame can advance. Capture completion here means all frame files written; the
package completion marker belongs to Task 7 after encode and verification.

- [x] Write a CPU transition test feeding readbacks in the wrong order; a duplicate/obsolete acknowledgment cannot advance the next frame. Add failed/missing-readback/timeout tests, plus an asset readiness failure that never writes frame zero.
- [x] Replace the early draft capture loop with the explicit state machine. Use an image target at requested dimensions independent of window size. Qualify the built-in Screenshot::image route first; use the tagged manual copy-buffer example only if observed limitations require it. Document the evidence for that switch.
- [x] Keep one GPU readback outstanding. Hold source tick/camera/caption constant until warmup and readback complete. Decode/validate width, height, format/row padding and row orientation. Write `frames/000000.png` through `frames/000299.png` without overwriting existing files; flush/close each file before acknowledging it.
- [x] Qualify assets/shader pipelines, missing asset errors, fixed exposure, history reset and warmup. A timeout fails the run with frame/stage details. Use deterministic presentation-noise seeds. Disable temporal effects initially if they cannot yet be reset reliably; restore only after a repeat-render witness.
- [x] Run a small GPU capture with a diagnostic corner-color image and frame counter to verify color channels/orientation/frame correspondence, then capture two seconds of the actual scene. The diagnostic is a separate test scene, never substituted for study footage.
- [x] Export the full 300 frames at 4K. Observe the process until termination and inspect file counts, dimensions and representative frames before encoding. Record wall time, per-frame capture time and peak memory; do not change output resolution to obtain a passing result.
- [x] Commit the capture implementation and measured GPU report after CPU/gate checks. A window-hosted capture is acceptable if its independent output target is demonstrated; a GPU-unavailable result is an explicit unmet acceptance condition.

### Task 7: package, encode and independently verify the study

**Files:**
- Create: `clients/visual/planetarium/src/package.rs`, `clients/visual/planetarium/tests/suite/package.rs`.
- Modify: `clients/visual/planetarium/src/{main,lib}.rs`, suite registration.
- Read: `scripts/observation-render.sh` and the actual Observation Series manifest/verification code located by its command names before choosing reuse.

**Interfaces:** Consumes capture records/files and FilmDefinition. Produces `verify_package(directory: &std::path::Path) -> Result<(), PackageError>` and `planetarium verify --out DIR`. `PackageError` has `Io(String)`, `Manifest(String)`, `Hash(String)`, `Encode(String)` variants and implements Display/Error.

Package layout:

```text
source/world.json
source/initial.json
source/observations/000000.json ... 000299.json
frames/000000.png ... 000299.png
film.json
frames.jsonl
study.mp4
manifest.json
COMPLETE
```

`frames.jsonl` has one ordered record per frame: frame index, rational
presentation time `{numerator: index, denominator: 30}`, exact ticks,
observation SHA-256, camera pose, caption, PNG SHA-256. Manifest schema
`visual/study/v1` records source revision/binding, world/film/initial/asset hashes,
frame-record hash, video hash, dimensions/fps/count, renderer/toolchain/ffmpeg,
OS/GPU/backend, history/warmup settings, all cosmetic treatments, source-model
limitations and whether the rendering source tree was clean. Final acceptance
requires a clean, pinned revision. Draft dirty runs must be labeled and cannot
claim the final source revision as their full provenance.

The committed authored film may name an earlier, existing source revision. For
final clean capture, explicitly freeze an external package-local film copy bound
to the actual clean capture HEAD, retaining the chosen world/interval/direction
unless a change is recorded. Query fresh initial and observation documents;
never relabel old frames. Record the exact film hash and executable hash with the
actual build revision. No commit is expected to contain its own SHA (ledger #19).

- [x] Write CPU package tests that create a tiny fixture package, then remove a frame, duplicate an index, change an observation tick, alter a PNG byte, alter the video, and swap the world binding. Each must fail verification for the stated reason. Helpers generate disposable directories; do not mutate committed fixtures.
- [x] Require a freshly created output directory. Initial creation succeeds only when absent; interrupted output is preserved for diagnosis. A second run chooses a new directory. Refuse path traversal/symlink escapes while resolving manifest-listed files; all package members are relative to its root.
- [x] Use Rust `std::process::Command` with explicit argument arrays for ffmpeg/ffprobe, never interpolated shell. Candidate video command:

```bash
ffmpeg -nostdin -v error -framerate 30 -start_number 0 \
  -i frames/%06d.png -frames:v 300 -c:v libx264 -crf 16 \
  -pix_fmt yuv420p -movflags +faststart -n study.mp4
ffprobe -v error -count_frames -select_streams v:0 \
  -show_entries stream=width,height,avg_frame_rate,nb_read_frames \
  -of json study.mp4
```

Qualify RGB-to-video color handling against the captured PNGs; record the
conversion metadata actually chosen. No HDR-delivery claim: this study's
review MP4 is SDR. Preserve the original PNGs as the high-quality source.

- [x] Missing ffmpeg, nonzero encoder exit, missing codec, bad probe count/dimensions/rate or mismatched hash leave no COMPLETE marker. Verification checks content, identity and exact frame mapping independently from encoder success. Stub failed processes in CPU tests; run the real tools for the final package.
- [x] Write manifest last, verify the completed content, then atomically publish COMPLETE containing the manifest SHA-256. Public `verify` requires and validates the marker. Internal pre-completion verification uses the same checks with only the marker requirement deferred; do not create a temporary success marker to satisfy the verifier.
- [x] Test interruption after frame 299 but before encode, and after encode before marker; neither is complete. Reverify the actual 4K study with the standalone command and record its manifest/video hashes.
- [x] Commit implementation and package verification evidence after the applicable gates. Do not adapt old episode packet records by inventing correspondence to make its assembler accept these frames.

**Stage 3 boundary:** complete package plus actual moving review copies required. Submit the canonical stage gate and record its result. Packaging correctness does not award visual acceptance.

## Stage 4 — durable integration, visual refinement and handoff

**Goal:** Make the libraries/client maintainable, demonstrate the final visual and performance result, and prepare the G6 review package.
**Success Criteria:** Client CPU gates run in the canonical client phase; docs match behavior; the final moving package and measurements are reviewable; G6 decision is recorded before merge.
**Tests:** Tasks 8–9; dependency mutation witnesses, scoped client gates, final package verification, canonical stage evidence and GPU review.
**Status:** In Progress

### Task 8: enforce reuse boundaries and integrate client checks

**Files:**
- Create: `scripts/visual-dependencies.py`, `scripts/test-visual-dependencies.py`.
- Modify: `Makefile`, `cli/tests/suite/architecture.rs`, `cli/tests/suite/lane_sets.rs` only if the live tests require the new client hook to be declared.
- Create: `clients/visual/README.md`; update the actual client guide (`clients/CLAUDE.md` in this checkout).
- Create: `book/src/reference/scene-astronomy-at-v1.md`, `book/src/clients/planetarium.md`; modify `book/src/SUMMARY.md` using its existing client/reference organization.

**Interfaces:** Consumes Cargo metadata and the existing `clients-check-run` aggregation. Produces `make visual-check` (timed wrapper), `make visual-check-run` (CPU check body), and a dependency guard with an explicit forbidden-reachability direction.

- [x] Read `scripts/game-no-vessel-dep.sh` and architecture/gate tests. Implement the visual guard over resolved `cargo metadata --format-version 1 --locked`, not a text grep that misses aliases/transitive edges. Identify workspace paths and package IDs; allow app→source/view, forbid source→Bevy/app, view→any simulation/source/app. Test normal and dev dependency paths; a test-only leak is still a leak.
- [x] Add synthetic graph tests for direct, renamed and transitive forbidden edges and allowed app edges. Prove a real temporary manifest mutation is detected, asserting the target text exists before editing and restoring it after the run. Library tests instantiate their own Source or a CPU Bevy App/ObservationMirror without importing Planetarium.
- [x] Add `visual-check-run`:

```make
	cd clients/visual && cargo +1.96.1 fmt --check
	cargo +1.96.1 clippy --locked --manifest-path clients/visual/Cargo.toml --workspace --all-targets -- -D warnings
	cargo +1.96.1 test --locked --manifest-path clients/visual/Cargo.toml --workspace
	python3 scripts/visual-dependencies.py
	python3 scripts/test-visual-dependencies.py
```

CPU tests must not start a GPU/window. GPU qualification has a separately
documented command and required evidence; absence is not a successful skip.
Add the visual target to both target lists in `clients-check-run` so invocation
and log collection stay matched. Check canonical Linux development libraries
from the pinned Bevy requirements before submitting the client phase; report
installation failures rather than suppressing compilation.

- [x] Extend the architecture test to require the three-crate boundary, own workspace/lockfile and optimized profiles without enumerating implementation details. Root tests must not link Bevy. Check the test-binary roster when registering the new suite targets; justify any new compiled unit explicitly.
- [x] Document commands, physical/model limitations, source authority, exact clocks, supported camera range, capture/verification failures, assets/licenses, and the future situated-game boundary. Explain how another application supplies permitted documents without inheriting Planetarium shots. Do not claim a second renderer/game exists.
- [x] Reserve decision numbers with `make decision-block NAME=the-planetarium` if not already reserved; record the actual allocated numbers. Add accepted campaign decisions for graphical-client scheduling/Observation Series amendments, native evaluated observation seam, and reusable client ownership. Use allocated paths, never guessed numbers. Add supersession pointers to the earlier spec while retaining historical manifests and records.
- [x] Run `make visual-check`, the root commit gate and documentation checks as applicable, inspect every result, then commit. Expensive combined client/workspace checks belong to the canonical stage request.

Actual allocation: 0956–0965, with 0956–0958 authored in Task 8. The original
root-CWD virtual-manifest formatter invocation failed to find targets; the final
CWD-based recipe was qualified against its verbose source/view/app target list.
Canonical compilation of this new client remains the final Stage 4 request.

### Task 9: refine and measure the final moving result; prepare G6

**Files:**
- Modify: `clients/visual/planetarium/films/pilot.json` and source/view appearance files only where actual review requires changes.
- Create: `docs/audits/the-planetarium/{performance,final-review}.md`, `docs/retrospectives/the-planetarium.md`, `book/src/chronicle/the-planetarium.md`.
- Modify: campaign ledger, reconciliation row, relevant approved registry status, book navigation and `IMPLEMENTATION_PLAN.md`.

**Interfaces:** Consumes a verified 4K package and CPU/canonical evidence. Produces a concrete G6 package: exact video and representative still paths/hashes, measured limitations, post-G3 ledger digest, test/census status and a candidate commit SHA. It does not itself grant merge/publication approval.

- [x] View the full 10-second film, full-resolution stills near frames 0/89/90/209/210/299, and a 360-pixel-wide downscaled review. Inspect limb silhouette, terrain/ocean datum, moon shading, color, caption readability, focal target, aliasing, exposure pumping, frame repeats and cut/history artifacts. Record findings tied to frame IDs.
- [x] Fix demonstrated visual defects and recapture the affected draft. Preserve old packages; a changed film/material/source revision means a new manifest and full final package. Re-run only tests relevant to code changes, then required gates for the new candidate.
- [x] Measure first-load wall time, observation p50/p95, 1080p interactive p50/p95/p99 frame times, peak resident memory, capture seconds/frame and total wall time. State host/GPU/backend, resolution, source/film/settings, warm/cold setup and measurement duration. Measure a 60-second fixed camera/time interaction script after readiness; report raw sample count, not only an average. Target p95 frame time ≤33.33 ms; if unmet, profile presentation cost and show the measured result to Nathan rather than silently weakening the target.
- [x] Repeat-render the same representative frame set twice in the same environment and compare pixel differences plus source/camera hashes. Record exact-match fraction and maximum/mean channel error; measure temporal variance with a short repeated sequence too. Semantic bytes must agree. GPU byte identity is an observation, not a cross-host guarantee.
- [x] Produce one final clean-revision package, run `planetarium verify --out DIR`, and preserve the actual review video/stills in the artifact location referenced by final-review.md. Tests may not point only at disposable `/tmp` files. Record source/tool/asset licenses and no-publication status.
- [x] Run the required canonical stage request for the final candidate and record actual artifact changes/costs. Request `req-a9593ffa367e-20260911T120920Z` passed all four phases, including Linux visual-client CPU checks; the ledger records exact merge/artifact revisions.
- [x] Complete census-close accounting: the authorized census returned no scientific golden changes. A same-ref, 1000-row live main-study profile supports the scoped per-run finding; original timing delivery incorporated at `e8fdb804c` through the green normal hook. The meeting study and pipeline tail were not profiled; no scaling guarantee or changed alarm is claimed.
- [x] Reconcile all ledger follow-ups. Write the chronicle/retrospective from measured results, not the concept image. Update reconciliation/spec/plan links and the four-stage tracker; do not call the campaign complete while visual acceptance remains pending.
- [x] Present G6: leading schema/model/determinism rulings (including radius), post-G3 ledger digest, verified video/stills, performance and limitations, actual check results, and candidate SHA. Wait for Nathan's final visual/merge decision under campaign-autopilot; this approval is separate from publication.
- [ ] After approval use `closing-a-campaign` and `submitting-to-the-sluice` for the actual integration. Remove only the Planetarium section of `IMPLEMENTATION_PLAN.md` when these stages are complete; preserve the inherited unrelated tracker above it. Remove the whole file only if no unfinished tracker remains. Keep this permanent plan with checked steps and final evidence links. Do not release the worktree or declare a landed result before the queue reports the tested merge has landed.

## Execution and review rules

Each implementation task uses a fresh SDD worker and the prescribed spec/code
review sequence. Source verification happens immediately before dispatch because
parallel campaigns can move shared astronomy/CLI conventions. Respect the live
eclipse campaign's ownership; no unsolicited cross-agent messages are needed to
write this plan. Shared-source conflicts require reading the real merge product,
not choosing an old side from this document.

A task can take multiple small commits. Commit only working increments after
relevant tests and the normal gate; include generated audit changes actually
required by that increment. Update the ledger when a ruling occurs and the
tracker when a stage changes. A green code review cannot substitute for the
stage's GPU or visual deliverable.

## Spec coverage and planning self-review

- Spec §§1–5: Tasks 1, 3, 5 and 9 — actual moving output, subject, visual direction, physical scale and fidelity.
- Spec §6: Tasks 2–4 and 8 — evaluated source, independent libraries, source/scope containment, native reuse and stale replies.
- Spec §7: Tasks 4–6 — three clocks, exact rounding, direct seeking, history reset.
- Spec §8: Tasks 5–7 — controls, shared scene, full capture, provenance and failure behavior.
- Spec §9: Tasks 2–4 and 6–9 — CPU semantics, real GPU evidence, independent package checks, visual and measured performance review.
- Spec §§2,10–11: Tasks 1, 8–9 — policy records, source prerequisite, campaign gates, publication boundary and close.

The physical-radius scope answer is incorporated into Task 1 and the approved
spec's planning amendment. The selected source model, range, data, units and
independent tests are concrete. Self-review also corrected the shot interface to
consume the current observed body positions explicitly, and preserved the
inherited unfinished stage tracker instead of overwriting it. No unapproved
fidelity change, source/GPU execution claim or outstanding planning question
remains. G4 passes; execution follows the user's standing SDD preference.
