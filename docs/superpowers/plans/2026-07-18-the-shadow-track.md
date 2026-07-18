# The Shadow Track Implementation Plan

> **Status: COMPLETE (2026-07-18).** All 9 tasks shipped; final review fixes
> applied (closed-interval docs, band-occlusion lift, orphaned golden removed).
> See the [chronicle](../../../book/src/chronicle/the-shadow-track.md) and
> [retrospective](../../retrospectives/the-shadow-track.md).

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Export the sim's dated eclipses (`scene/eclipses/v1`: solar+lunar
events in a queried window, with solar ground tracks) and make the Orrery
mark the year's eclipses on the day scrubber and draw each solar eclipse's
shadow band on the globe.

**Architecture:** Producer-first — `windows/scene` gains a parameterized
scene document (a windowed query like `scene/tiles-region/v1`), then CLI /
wasm / golden / reference page, then the freshly built wasm feeds the Orrery
consumer (parser → catalog method → scrubber marks → globe band). Spec:
`docs/superpowers/specs/2026-07-18-the-shadow-track-design.md`.

**Tech Stack:** Rust (`hornvale-scene`, `hornvale`, standalone
`clients/world-wasm`), TypeScript + three.js + vitest (orrery).

## Global Constraints

- Hornvale worktree: `~/.config/superpowers/worktrees/hornvale/the-shadow-track`, branch `the-shadow-track`. Orrery worktree: `~/.config/superpowers/worktrees/orrery/the-shadow-track`, branch `the-shadow-track`. Never commit elsewhere.
- Dependencies: `serde`/`serde_json` only (Rust); no new npm packages.
- No `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only); no wall-clock time. f64 transcendentals via `hornvale_kernel::math`, never raw `f64::sin` (determinism, decision 0041).
- Every serialized f64 quantizes at emit: `#[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]` (decision 0033), emit boundary only. Strings (`body`/`kind`) and integers (`seed`, `moon_index`) are not quantized.
- New JSON fields are APPENDED after existing fields; field order is contract. Every new `pub` item gets a doc comment + a `type-audit:` tag on the struct line (mirror `NeighborElem`'s tags: `pending(wave-1: <deg/day field>)`, `bare-ok(count: index)`, `bare-ok(identifier-text: <string field>)`, `bare-ok(ratio: <dimensionless>)`). Run `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` after adding pub fields; regenerate the report (`-- report > docs/audits/type-audit-report.md`) in the SAME commit if flagged.
- **Determinism:** no ledger epoch, no new stream draws, no seed-derivation label changes. `scene/eclipses/v1` is a pure read of `eclipse_events`/`ground_track` (closed-form over the star system + calendar). Worlds/almanacs/censuses byte-identical; the only new bytes are the new documents + goldens.
- `cargo fmt` before every Rust commit; commit gate `make gate` (Bash `timeout: 3600000`). Orrery: `npm test` + `npx tsc --noEmit`.
- Commit trailer: `Claude-Session: https://claude.ai/code/session_01UvQLaLygGoqqQbKwuBYSbJ`

---

### Task 1: `eclipses_scene` producer in `windows/scene`

**Files:**
- Modify: `windows/scene/src/lib.rs` (new section after the neighbors-scene block ~line 830; new tests beside the neighbors tests)

**Interfaces:**
- Consumes: `hornvale_worldgen::sky_of`, `sky_of(world).system()` (returns `Option<&StarSystem>`), `hornvale_astronomy::calendar_of(system) -> Calendar`, `hornvale_astronomy::eclipse_events(system, &calendar, from: StdDays, until: StdDays) -> Vec<EclipseEvent>`, `hornvale_astronomy::ground_track(system, &calendar, &event) -> Option<GroundTrack>`. `EclipseEvent` fields: `day: StdDays`, `moon: usize`, `body: EclipseBody` (`Solar`/`Lunar`), `kind: EclipseKind` (`Total`/`Annular`). `GroundTrack` fields: `center_lat_deg`, `half_width_deg`, `start_lon_deg`, `end_lon_deg`, `duration_days` (all f64). Confirm these enum/field names by grep in `domains/astronomy/src/eclipses.rs` before writing; if `calendar_of` isn't re-exported at the crate root, use the full path `hornvale_astronomy::calendar::calendar_of` (it lives in the calendar module).
- Produces: `ECLIPSES_SCHEMA: &str = "scene/eclipses/v1"`, `pub struct EclipseElem`, `pub struct GroundTrackElem`, `pub struct EclipsesScene`, `pub fn eclipses_scene(&World, from: f64, until: f64) -> Result<EclipsesScene, SceneError>`, `pub fn eclipses_json(&EclipsesScene) -> String`. Tasks 2/4/6 call exactly these.

- [ ] **Step 1: Write the failing tests** (inside the existing `#[cfg(test)] mod`, reusing the `mooned_world()` helper the moons/neighbors tests use — grep it):

```rust
#[test]
fn eclipses_scene_has_schema_window_and_is_deterministic() {
    let w = mooned_world();
    // A wide window so seed 42's two moons produce several events.
    let a = eclipses_scene(&w, 0.0, 2000.0).expect("mooned world has eclipses");
    assert_eq!(a.schema, "scene/eclipses/v1");
    assert_eq!(a.seed, w.seed.0);
    assert_eq!(a.from_day, 0.0);
    assert_eq!(a.until_day, 2000.0);
    assert!(!a.events.is_empty(), "seed 42's moons eclipse within 2000 days");
    // Day-ascending, inside the window.
    for e in &a.events {
        assert!((0.0..=2000.0).contains(&e.day));
        assert!(e.body == "solar" || e.body == "lunar");
        assert!(e.kind == "total" || e.kind == "annular");
    }
    for win in a.events.windows(2) {
        assert!(win[0].day <= win[1].day, "events are day-ascending");
    }
    // Solar events carry a track; lunar events carry none.
    for e in &a.events {
        if e.body == "solar" {
            let t = e.track.as_ref().expect("a solar event has a ground track");
            assert!((-90.0..=90.0).contains(&t.center_lat_deg));
            assert!((-180.0..180.0).contains(&t.start_lon_deg));
        } else {
            assert!(e.track.is_none(), "a lunar event has no ground track");
        }
    }
    // Byte-identical on rebuild.
    assert_eq!(eclipses_json(&a), eclipses_json(&eclipses_scene(&w, 0.0, 2000.0).unwrap()));
}

#[test]
fn eclipses_scene_rejects_a_world_with_no_generated_sky() {
    // Mirror the moons/neighbors constant-sun refusal test (grep the sibling).
    let w = constant_sun_world(); // the helper the moons_scene refusal test uses
    assert!(eclipses_scene(&w, 0.0, 100.0).is_err());
}
```

- [ ] **Step 2: Run to verify failure**: `cargo test -p hornvale-scene eclipses_scene` — compile FAIL (`eclipses_scene` not found).
- [ ] **Step 3: Implement** (mirror `neighbors_scene`'s shape — schema const + structs + builder + json fn, with doc comments and type-audit lines):

```rust
/// The `scene/eclipses/v1` schema tag.
pub const ECLIPSES_SCHEMA: &str = "scene/eclipses/v1";

/// One solar eclipse's shadow band on the globe.
/// type-audit: pending(wave-1: center_lat_deg), pending(wave-1: half_width_deg), pending(wave-1: start_lon_deg), pending(wave-1: end_lon_deg), pending(wave-2: duration_days)
#[derive(Debug, Serialize)]
pub struct GroundTrackElem {
    /// Band-center latitude at mid-event, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub center_lat_deg: f64,
    /// Half-width of the full-omen band, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub half_width_deg: f64,
    /// Sub-solar longitude at crossing start, degrees [-180, 180).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub start_lon_deg: f64,
    /// Sub-solar longitude at crossing end, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub end_lon_deg: f64,
    /// Crossing duration, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub duration_days: f64,
}

/// One dated eclipse.
/// type-audit: pending(wave-2: day), bare-ok(count: moon_index), bare-ok(identifier-text: body), bare-ok(identifier-text: kind)
#[derive(Debug, Serialize)]
pub struct EclipseElem {
    /// The syzygy, absolute standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub day: f64,
    /// Distance-sorted index into the system's moons.
    pub moon_index: usize,
    /// "solar" or "lunar".
    pub body: String,
    /// "total" or "annular".
    pub kind: String,
    /// The shadow band — `Some` for solar events, `None` (serialized as JSON
    /// `null`) for lunar (the anchor's shadow is the whole night side). NOT
    /// `skip_serializing_if` — the field is always present so the client sees
    /// an explicit `"track": null`, matching the spec's `track | null`.
    pub track: Option<GroundTrackElem>,
}

/// One `scene/eclipses/v1` document: the dated eclipses in a queried window.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), pending(wave-2: from_day), pending(wave-2: until_day)
#[derive(Debug, Serialize)]
pub struct EclipsesScene {
    /// Always `scene/eclipses/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The queried window start, echoed back (standard days).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub from_day: f64,
    /// The queried window end.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub until_day: f64,
    /// The dated eclipses, day-ascending.
    pub events: Vec<EclipseElem>,
}

/// Build the `scene/eclipses/v1` scene for `world` over `[from, until]`
/// standard days. Errors when the world has no generated sky (no moons, no
/// eclipses) — mirrors [`moons_scene`]. Pure read: consumes no draws.
pub fn eclipses_scene(world: &World, from: f64, until: f64) -> Result<EclipsesScene, SceneError> {
    use hornvale_astronomy::StdDays;
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky
        .system()
        .ok_or_else(|| SceneError::Build("this world has no generated sky".to_string()))?;
    let calendar = hornvale_astronomy::calendar_of(system);
    let events = hornvale_astronomy::eclipse_events(system, &calendar, StdDays(from), StdDays(until))
        .into_iter()
        .map(|ev| {
            let track = hornvale_astronomy::ground_track(system, &calendar, &ev).map(|g| GroundTrackElem {
                center_lat_deg: g.center_lat_deg,
                half_width_deg: g.half_width_deg,
                start_lon_deg: g.start_lon_deg,
                end_lon_deg: g.end_lon_deg,
                duration_days: g.duration_days,
            });
            EclipseElem {
                day: ev.day.get(),
                moon_index: ev.moon,
                body: match ev.body {
                    hornvale_astronomy::EclipseBody::Solar => "solar",
                    hornvale_astronomy::EclipseBody::Lunar => "lunar",
                }.to_string(),
                kind: match ev.kind {
                    hornvale_astronomy::EclipseKind::Total => "total",
                    hornvale_astronomy::EclipseKind::Annular => "annular",
                }.to_string(),
                track,
            }
        })
        .collect();
    Ok(EclipsesScene {
        schema: ECLIPSES_SCHEMA.to_string(),
        seed: world.seed.0,
        from_day: from,
        until_day: until,
        events,
    })
}

/// Serialize an `EclipsesScene` to compact JSON (mirrors [`moons_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn eclipses_json(scene: &EclipsesScene) -> String {
    serde_json::to_string(scene).expect("an EclipsesScene always serializes")
}
```

Adjust import paths / accessors to what `hornvale_astronomy` actually re-exports (grep its `lib.rs` for `EclipseEvent`, `EclipseBody`, `EclipseKind`, `GroundTrack`, `eclipse_events`, `ground_track`, `calendar_of`, `StdDays`; add root re-exports beside the existing `class_name`/`starfield` re-exports if any are missing). `StdDays(x)` / `.get()` — match how `StdDays` is constructed elsewhere in the crate (grep `StdDays(`).

- [ ] **Step 4: Verify green**: `cargo test -p hornvale-scene`.
- [ ] **Step 5: type-audit + fmt + clippy**: `cargo run --manifest-path tools/type-audit/Cargo.toml -- check`; regen the report if flagged. `cargo fmt && cargo clippy -p hornvale-scene --all-targets -- -D warnings`.
- [ ] **Step 6: Commit**: `feat(the-shadow-track): scene/eclipses/v1 — dated eclipses + solar ground tracks`.

---

### Task 2: CLI `scene eclipses` + committed gallery artifact

**Files:**
- Modify: `cli/src/main.rs` (usage block beside `scene tiles-region` ~line 49; `cmd_scene` new arm mirroring the `tiles-region` arm)
- Modify: `scripts/regenerate-artifacts.sh` (beside the `scene tiles-region` line)
- Check/modify: `.github/workflows/ci.yml` — if the "Artifacts are current" step enumerates scene commands individually, add the eclipses line; if it calls the script, nothing to do.
- Create (generated): `book/src/gallery/scene-eclipses-seed-42.json`

**Interfaces:**
- Consumes: `hornvale_scene::{eclipses_scene, eclipses_json}` (Task 1).
- Produces: the committed seed-42 gallery JSON the CI drift-checks; Task 3's reference page links it.

- [ ] **Step 1: Add the CLI arm** (mirror the `tiles-region` arm — parse `--from`/`--until` as f64):

```rust
Some("eclipses") => {
    let world = load_world(args)?;
    let parse_f64 = |flag: &str| -> Result<f64, String> {
        flag_value(args, flag)
            .ok_or_else(|| format!("scene eclipses requires {flag}"))?
            .parse::<f64>()
            .map_err(|e| format!("{flag} must be a number: {e}"))
    };
    let from = parse_f64("--from")?;
    let until = parse_f64("--until")?;
    let scene = hornvale_scene::eclipses_scene(&world, from, until).map_err(|e| e.to_string())?;
    println!("{}", hornvale_scene::eclipses_json(&scene));
    Ok(())
}
```

Add the usage line: `hornvale scene eclipses --world W --from D --until D   emit scene/eclipses/v1 JSON`.
- [ ] **Step 2: Add the regen line** after the `scene tiles-region` line in `scripts/regenerate-artifacts.sh` (use a window covering seed 42's year, e.g. 0 to 2000 — pick a window that yields a few events; the seed-42 year is ~368 days, so `--from 0 --until 2000` spans several eclipse seasons):

```bash
run -p hornvale -- scene eclipses --world "$wsky" --from 0 --until 2000 > book/src/gallery/scene-eclipses-seed-42.json
```

- [ ] **Step 3: Smoke + generate**: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json && cargo run -p hornvale -- scene eclipses --world /tmp/hv42.json --from 0 --until 2000 | head -c 400` (expect the schema tag + a populated `events` array), then `bash scripts/regenerate-artifacts.sh` (foreground, `timeout: 3600000`; skips censuses) and confirm `git status` shows ONLY the new gallery JSON as new plus your source edits — any OTHER drift is a BLOCKED stop-and-report.
- [ ] **Step 4: ci.yml check** as described under Files.
- [ ] **Step 5: Gate**: `make gate` (timeout 3600000). Expect green.
- [ ] **Step 6: Commit**: `feat(the-shadow-track): CLI scene eclipses + seed-42 gallery artifact`.

---

### Task 3: Reference page `scene-eclipses-v1.md`

**Files:**
- Create: `book/src/reference/scene-eclipses-v1.md`
- Modify: `book/src/SUMMARY.md` (after the `Scene Schema: neighbors v1` line: `- [Scene Schema: eclipses v1](./reference/scene-eclipses-v1.md)`)

**Interfaces:**
- Consumes: the Task-1 schema; the ground-track latitude approximation from the producer model card (`domains/astronomy/src/eclipses.rs` `ground_track` doc comment).
- Produces: the normative contract page.

- [ ] **Step 1: Write the page**, following `book/src/reference/scene-neighbors-v1.md`'s structure and altitude (open with what it is and why it exists; a field table in contract order; the "derived vs approximated" boundary; consumer notes). It MUST contain:
  - The **parameterized window**: this is a *query* (`from_day`/`until_day` echoed), not a snapshot — the tiles-region precedent — and why (eclipses are a temporal series; the client asks for the window it shows).
  - The event fields (`day`, `moon_index`, `body`, `kind`) and the solar-only `track` (lunar events omit it — the anchor's shadow is the whole night side).
  - The **ground-track latitude is a declared approximation**: `center = solar_declination + (β/θ)·(90° − |declination|)` — quote/cite the producer's `ground_track` model-card comment.
  - The u64-seed BigInt caveat (copy from the neighbors page).
  - A link to the committed seed-42 gallery document.
- [ ] **Step 2: Build**: `mdbook build book` — no errors, page reachable.
- [ ] **Step 3: Commit**: `docs(the-shadow-track): scene/eclipses/v1 reference page`.

---

### Task 4: wasm export `hw_scene_eclipses` + fresh binary for the consumer

**Files:**
- Modify: `clients/world-wasm/src/lib.rs` (new export after `hw_scene_tiles_region` ~line 265)
- Produces (not committed): a rebuilt `hornvale_world.wasm` copied into the orrery worktree `public/`.

**Interfaces:**
- Consumes: Task 1's `eclipses_scene`/`eclipses_json`; the file's `WORLD`/`set_out`/`set_error` statics.
- Produces: `hw_scene_eclipses(from: f64, until: f64) -> i32` (0 ok / 2 scene error / -3 no world) — Task 6's catalog calls it by exactly this name.

- [ ] **Step 1: Implement** (mirror `hw_scene_tiles_region`'s parameterized shape + `hw_scene_moons`'s error contract):

```rust
/// Emit the current world's `scene/eclipses/v1` JSON over `[from, until]`
/// standard days. 0 ok; 2 scene error (envelope set); -3 when no world is live.
#[unsafe(no_mangle)]
pub extern "C" fn hw_scene_eclipses(from: f64, until: f64) -> i32 {
    let world_ptr = &raw const WORLD;
    let Some(world) = (unsafe { (*world_ptr).as_ref() }) else {
        set_error("no world; call hw_new first");
        return -3;
    };
    match hornvale_scene::eclipses_scene(world, from, until) {
        Ok(s) => {
            set_out(hornvale_scene::eclipses_json(&s));
            0
        }
        Err(e) => {
            set_error(&format!("{e}"));
            2
        }
    }
}
```

- [ ] **Step 2: fmt + clippy + tests**: `cargo fmt --manifest-path clients/world-wasm/Cargo.toml && cargo clippy --manifest-path clients/world-wasm/Cargo.toml -- -D warnings`; run the crate's tests if any.
- [ ] **Step 3: Build the binary**: `cargo build --manifest-path clients/world-wasm/Cargo.toml --target wasm32-unknown-unknown --release` (install the target if missing).
- [ ] **Step 4: Ship it to the consumer worktree**: `cp clients/world-wasm/target/wasm32-unknown-unknown/release/hornvale_world_wasm.wasm ~/.config/superpowers/worktrees/orrery/the-shadow-track/public/hornvale_world.wasm` (dev-loop handoff; the v7 release re-pin is a G6 action). `ls -la` the destination to prove freshness. Confirm the binary exports the symbol: `strings <dest> | grep -c hw_scene_eclipses` (≥1).
- [ ] **Step 5: Commit** (hornvale side only): `feat(the-shadow-track): hw_scene_eclipses wasm export`.

---

### Task 5: Producer-sourced golden (orrery testdata) — controller/task handoff

**Files:**
- Create (committed to orrery in Task 6): `testdata/eclipses-golden-seed42.json` (the seed-42 eclipses document at the fixed window).

**Interfaces:**
- Consumes: the Task-2 CLI (`scene eclipses`).
- Produces: the producer-sourced golden Task 6's fixture test pins the client parser/consumer against.

- [ ] **Step 1: Generate the golden** from the merged producer: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json && cargo run -p hornvale -- scene eclipses --world /tmp/hv42.json --from 0 --until 2000 > ~/.config/superpowers/worktrees/orrery/the-shadow-track/testdata/eclipses-golden-seed42.json`. (This is producer-sourced — never a JS reconstruction. It lands in the orrery commit in Task 6.)
- [ ] **Step 2: Sanity-check** the golden: it has `"schema":"scene/eclipses/v1"`, a non-empty `events` array, and at least one solar event with a `track` and at least one lunar event without. (This task produces no hornvale commit; it prepares the orrery fixture.)

> **Note:** this "task" is really a controller handoff — fold it into Task 6's dispatch (generate the golden, then implement the consumer against it) rather than a separate review gate. Kept numbered so the golden's provenance is explicit.

---

### Task 6: Orrery — `parseEclipses` + `sceneEclipses` catalog + fixture test

**Files (orrery worktree):**
- Modify: `src/sim/scene.ts` (new `EclipsesScene`/`EclipseEvent`/`GroundTrack` interfaces + `parseEclipses`)
- Modify: `src/sim/catalog.ts` (add `sceneEclipses(fromDay, untilDay): string` to the interface + impl, mirroring `sceneTiles(width)`; add `hw_scene_eclipses(from: number, until: number): number` to `HwExports`)
- Modify: `src/testHelpers/wasmFixture.ts` (add `loadSeed42Eclipses(from, until)`, mirroring `loadSeed42Tiles`)
- Create (committed): `testdata/eclipses-golden-seed42.json` (from Task 5)
- Modify: `src/sim/scene.test.ts`, `src/sim/catalogFixture.test.ts`

**Interfaces:**
- Consumes: the Task-4 binary at `public/hornvale_world.wasm` (verify `ls -la` shows today's date; if not, STOP — Task 4 must run first). Golden from Task 5.
- Produces: `parseEclipses(text): EclipsesScene` with `events: {day, moonIndex, body, kind, track: GroundTrack | null}[]`, `track: {centerLatDeg, halfWidthDeg, startLonDeg, endLonDeg, durationDays}`; `sceneEclipses(fromDay, untilDay): string` catalog method. Tasks 7/8 import these.

- [ ] **Step 1: Failing parser tests** in `scene.test.ts` (mirror the `parseTiles`/`parseRegion` test style): a valid document round-trips with camelCase fields; the schema literal is enforced (`'scene/eclipses/v0'` throws); `events` non-array throws mentioning `events`; a solar event's `track` parses; a lunar event's `track` is `null`; `body`/`kind` are validated to their two values; `centerLatDeg` of 91 throws.
- [ ] **Step 2: Red**: `npx vitest run src/sim/scene.test.ts`.
- [ ] **Step 3: Implement the parser** with the file's `requireNumber`/`requireString`/array helpers + range validation (centerLat |x|≤90, lon in [-180,180), body ∈ {solar,lunar}, kind ∈ {total,annular}, `track` present iff solar — or at least parsed as optional/null and required for solar).
- [ ] **Step 4: Catalog + fixture helper + end-to-end test**: add `sceneEclipses(fromDay, untilDay) { check(e.hw_scene_eclipses(fromDay, untilDay)); return readOut(e); }` and `hw_scene_eclipses` to `HwExports`; add `loadSeed42Eclipses`; in `catalogFixture.test.ts`:

```ts
test("the vendored binary's eclipses document parses strictly", async () => {
  const ecl = await loadSeed42Eclipses(0, 2000);
  expect(ecl.schema).toBe('scene/eclipses/v1');
  expect(ecl.events.length).toBeGreaterThan(0);
  expect(ecl.events.some((e) => e.body === 'solar' && e.track !== null)).toBe(true);
  expect(ecl.events.some((e) => e.body === 'lunar' && e.track === null)).toBe(true);
});
```

- [ ] **Step 5: Full green**: `npm test` and `npx tsc --noEmit` (the whole suite; the new binary also feeds every existing fixture test — a red there is a real regression).
- [ ] **Step 6: Commit** (including the golden CSV/JSON): `feat(the-shadow-track): parse scene/eclipses/v1 + sceneEclipses catalog`.

---

### Task 7: Orrery — eclipse marks on the day scrubber + inspector

**Files (orrery):**
- Create: `src/ui/eclipseMarks.ts` + `src/ui/eclipseMarks.test.ts` (the pure mark-position + inspector-content math)
- Modify: `src/ui/hud.ts` (overlay marks on the `hud-scrubber`; a `setEclipses(events, maxDay)` on the HUD surface)
- Modify: `src/main.ts` (parse `sceneEclipses(0, system.world.yearDays)` once the world loads; pass to the HUD; the inspector wiring)
- Modify: `src/ui/inspect.ts` (an `eclipseInfo(event)` InfoCard) + `src/ui/inspect.test.ts`

**Interfaces:**
- Consumes: `parseEclipses`' events (Task 6); the scrubber's `maxDay` (`system.world.yearDays`, already set via `setDayRange`).
- Produces: `eclipseMarkPositions(events, maxDay): {leftFraction, body, kind, event}[]` (pure — `leftFraction = day / maxDay`, clamped [0,1]); `eclipseInfo(event): InfoCard`.

- [ ] **Step 1: Failing unit tests** (`eclipseMarks.test.ts`): `eclipseMarkPositions` places a day-184 event at leftFraction 0.5 on a 368-day scrubber; a solar-total and a lunar event get distinct `body`/`kind` for styling; events outside `[0, maxDay]` are dropped (or clamped — pick and pin). `inspect.test.ts`: `eclipseInfo` names the body, kind, moon, and day.
- [ ] **Step 2: Red**: `npx vitest run src/ui/eclipseMarks.test.ts src/ui/inspect.test.ts`.
- [ ] **Step 3: Implement** `eclipseMarkPositions` (pure) + the HUD overlay: a `div.hud-eclipse-marks` positioned over the scrubber track, one child per mark at `left: ${leftFraction*100}%`, class by `body`/`kind` (CSS: solar vs lunar color, total vs annular fill). Clicking a mark calls a callback → `main.ts` shows `eclipseInfo(event)` in the existing InfoCard. `setEclipses(events, maxDay)` rebuilds the overlay. `main.ts` fetches `sceneEclipses(0, yearDays)` after the world loads and calls `setEclipses`.
- [ ] **Step 4: Green**: `npm test` + `npx tsc --noEmit`.
- [ ] **Step 5: Commit**: `feat(the-shadow-track): mark the year's eclipses on the day scrubber`.

---

### Task 8: Orrery — the globe ground-track band

**Files (orrery):**
- Create: `src/views/eclipseBand.ts` + `src/views/eclipseBand.test.ts`
- Modify: `src/views/globe.ts` (mount the band; show/hide by the clock's `day` vs each solar event)
- Modify: `src/main.ts` (thread the eclipses into `createGlobeView`, and the day into the band's update)

**Interfaces:**
- Consumes: `parseEclipses`' solar events + tracks (Task 6); `latLonToUnit(latDeg, lonDeg)` and `GLOBE_RADIUS` (`globe.ts`).
- Produces: `buildEclipseBand(track, radius): THREE.Mesh` (a latitude strip `center_lat ± half_width` spanning `start_lon → end_lon` on the sphere), and a predicate `bandVisibleAt(event, day, marginDays): boolean` (`event.body === 'solar' && |day - event.day| <= marginDays`).

- [ ] **Step 1: Failing unit tests** (`eclipseBand.test.ts`, pure math + geometry counts — no WebGL): `bandVisibleAt` is true for a solar event within the margin of `day`, false for lunar and for far days; `buildEclipseBand` returns a mesh whose vertices all lie within `[center_lat - half_width, center_lat + half_width]` latitude when mapped back (sample the geometry positions → lat via `asin(z/r)`), and span the `start_lon → end_lon` arc. Pick a margin constant (e.g. 3 days) and pin it.
- [ ] **Step 2: Red**: `npx vitest run src/views/eclipseBand.test.ts`.
- [ ] **Step 3: Implement** `buildEclipseBand`: build a strip of quads between the two latitude circles (`center_lat ± half_width`) over the longitude arc `[start_lon, end_lon]`, each vertex `latLonToUnit(lat, lon).multiplyScalar(radius * 1.001)` (just above the surface, like the markers' clearance), a semi-transparent `MeshBasicMaterial` (umbral gray, `transparent: true`, `opacity ~0.4`, `depthWrite: false`). In `globe.ts`, mount a group; in `update(day)`, for each solar event where `bandVisibleAt(event, day, MARGIN)`, ensure its band is shown (build lazily, cache per event index), hide the rest. Thread `eclipses` into `createGlobeView` and the day through `update`.
- [ ] **Step 4: Green**: `npm test` + `npx tsc --noEmit`.
- [ ] **Step 5: Commit**: `feat(the-shadow-track): sweep the solar eclipse shadow band on the globe`.

---

### Task 9: Assembly verification + G6 package (controller-run)

- [ ] Hornvale worktree: `make gate` green; `bash scripts/regenerate-artifacts.sh` → `git status` clean (no drift); `mdbook build book`.
- [ ] Orrery worktree: `npm test`, `npx tsc --noEmit`, `npm run build`, Playwright e2e (all green with the Task-4 binary in `public/`).
- [ ] **Visual verification (The Lens rule):** serve the built orrery (base `/orrery/` now works — orrery#7 fixed; use the e2e serve or `vite preview`), load a mooned seed, scrub to a solar eclipse mark, confirm the shadow band lands where the almanac's ground track says; screenshot both the scrubber marks and the globe band for the G6 evidence.
- [ ] Conflict-marker sweep (all three markers) across both worktrees; absorb main at this boundary if it moved (regenerate the generated `type-audit-report.md` on conflict); followup register updated; G6 package assembled (ledger digest with the save-format entry leading, screenshots, gate evidence, world-wasm-v7 release + orrery re-pin + push plan for Nathan's carve-out approvals).
