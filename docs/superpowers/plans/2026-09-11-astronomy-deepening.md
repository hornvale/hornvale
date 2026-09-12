# Astronomy Deepening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a deterministic, human-history astronomical substrate for Hornvale using a thin hierarchical Keplerian model, persistent small bodies, modeled notable stars, and species-specific naked-eye observation.

**Architecture:** Genesis draws create immutable orbital and stellar records. Analytic evaluators derive state at an explicit `StdInstant`; observer queries then apply geometry, atmosphere, illumination, and species traits. Astronomy owns physical state and observability. Culture owns constellation grouping, names, and meanings. No N-body integration, tidal evolution, luminosity drift, asteroid field, impact model, or guaranteed stellar catastrophe is included.

**Tech Stack:** Rust workspace, `domains/astronomy`, existing `StdInstant`/unit types, `hornvale_kernel::Seed` streams, existing libm wrappers and quantize-at-emit conventions, existing astronomy/species test utilities, cargo nextest.

**Spec:** `docs/superpowers/specs/2026-09-11-astronomy-deepening-design.md`

## Global Constraints

- Preserve deterministic stream consumption and save-format compatibility; new draws use dedicated streams and do not redraw existing objects.
- Keep the host system stable across the human-history window. Stellar evolution is descriptive metadata unless an explicitly scheduled neighbor event falls inside the window.
- Keep all analytic paths finite and total for malformed or degenerate periods; return typed absence where an event is not defined.
- Store tens of modeled catalog stars, not thousands of background stars. Background stars are lazy, stable, and query-order independent.
- Use species-level `activity_cycle`, `night_vision`, and `sky_attention`; individual variation is out of scope.
- Every implementation task follows red-green-refactor, runs the narrowest relevant test first, and commits a small coherent change.

---

## Stage 1: Reconcile the orbital-state spine

### Task 1: Inventory and freeze the shared orbital contract

**Files:**
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/astronomy/src/calendar.rs`
- Modify: `domains/astronomy/src/forcing.rs`
- Modify: `domains/astronomy/src/eclipses.rs`
- Test: existing module tests in those files and the astronomy integration suite

**Interfaces:** `ephemeris` consumes `StarSystem` and `StdInstant`; it produces authoritative body positions, velocities, and frame metadata. Calendar, eclipse, forcing, and scene code consume that contract rather than reconstructing orbital phase independently.

- [ ] Write failing tests for explicit epoch handling, negative `StdInstant` values, eccentric anomaly/true-anomaly consistency, and a single position result shared by calendar/eclipses.
- [ ] Run `cargo test -p hornvale-astronomy` and record the red failures.
- [ ] Define the smallest internal orbital element/state types needed to carry epoch, semi-major axis, eccentricity, phase/orientation, and validity range.
- [ ] Route existing circular anchor/moon/wanderer calculations through the shared evaluator while preserving current outputs where eccentricity is zero.
- [ ] Run the focused tests and `cargo fmt --check`.
- [ ] Refactor duplicated phase/angle normalization into one private helper; rerun focused tests.
- [ ] Commit with message `refactor(astronomy): unify orbital state contract`.

### Task 2: Migrate dependent geometry and harden boundaries

**Files:**
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/astronomy/src/calendar.rs`
- Modify: `domains/astronomy/src/eclipses.rs`
- Modify: `domains/astronomy/src/sky_position.rs`
- Modify: `domains/astronomy/src/render.rs` if compilation requires API adaptation
- Test: astronomy module tests plus `cli/tests` astronomy contract tests discovered with `rg -n "astronom|ephemeris|eclipse" cli/tests domains/astronomy`

**Interfaces:** Existing public functions remain source-compatible where practical; new orbital reads expose state at an explicit epoch and reject/represent invalid ranges without panics.

- [ ] Add failing boundary tests for zero/infinite synodic rates, high eccentricity, retrograde spin, locked worlds, and observations outside the historical validity window.
- [ ] Run the focused astronomy test command.
- [ ] Replace independent circular assumptions in eclipse, solar-equatorial, and calendar phase paths with the shared state read.
- [ ] Preserve quantization only at emitted boundaries and preserve existing pinned generation streams.
- [ ] Run focused tests, `cargo clippy -p hornvale-astronomy --all-targets -- -D warnings`, and format checks.
- [ ] Commit with message `refactor(astronomy): route sky consumers through orbital state`.

## Stage 2: Persistent comets and meteor showers

### Task 3: Add persistent comet identities and analytic returns

**Files:**
- Create: `domains/astronomy/src/comets.rs`
- Modify: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/system.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Test: new `domains/astronomy/src/comets.rs` tests or the existing wanderer test module

**Interfaces:** Genesis produces a stable `Comet` record with ID, orbital elements, epoch, period, orientation, activity parameters, and visibility parameters. A time query produces a `CometReturn`/appearance without mutating the identity.

- [ ] Write failing tests for deterministic identity, dedicated-stream isolation, stable period/return indexing, and persistence across two distant observations.
- [ ] Run the focused astronomy tests and verify red status.
- [ ] Implement bounded Keplerian comet returns using the Stage 1 evaluator; derive activity from identity and return index with no new identity draw.
- [ ] Add latent, naked-eye, and great-comet visibility tiers based on apparent magnitude, solar elongation, observer darkness, and atmospheric suppression.
- [ ] Run focused tests and inspect serialized/generated outputs for stable IDs.
- [ ] Commit with message `feat(astronomy): model persistent comets`.

### Task 4: Derive meteor streams and observer-specific showers

**Files:**
- Create or modify: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/comets.rs`
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Test: stream and observer-geometry tests in `domains/astronomy/src/streams.rs`

**Interfaces:** A comet may expose a deterministic debris stream with validity epoch, orbital geometry, width, density, and activity profile. An observer query consumes location, local time, atmosphere, and species-independent physical visibility inputs and produces radiant, rate, duration, and visibility tier.

- [ ] Write failing tests for annual recurrence, radiant-from-relative-velocity, hemisphere/latitude differences, dark-sky gating, and stream validity.
- [ ] Run focused tests and verify failures.
- [ ] Implement the simplest annual stream first; keep dense clumps and outbursts represented as absent/unsupported data rather than simulated noise.
- [ ] Ensure no event is guaranteed: low-density streams may produce no visible shower for a valid observer query.
- [ ] Run focused tests, then astronomy clippy and nextest.
- [ ] Commit with message `feat(astronomy): derive observer-specific meteor showers`.

## Stage 3: Modeled neighboring stars

### Task 5: Replace class-only neighbors with stable modeled catalog stars

**Files:**
- Modify: `domains/astronomy/src/neighborhood.rs`
- Modify: `domains/astronomy/src/star.rs`
- Modify: `domains/astronomy/src/system.rs`
- Modify: `domains/astronomy/src/facts.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Test: neighborhood/star tests and relevant concept/fact contract tests

**Interfaces:** `Neighbor` becomes or wraps a stable catalog entry with ID, distance, sky coordinates, mass, age, evolutionary stage, effective temperature, luminosity, and derived apparent brightness/color. The host star remains a separate stable object.

- [x] Write failing tests for tens-count bounds, stable IDs, physical ordering, brightness consistency, host stability, and one scheduled transient eligibility check.
- [x] Run focused tests and capture the compatibility failures from existing class-based callers. The retained `Neighbor` projection keeps these callers green; only three authored `StarSystem` fixtures need the added catalog field.
- [x] Add dedicated catalog streams and generate a bounded modeled set without changing existing streams; derive class/name/color from physical properties.
- [x] Preserve old public descriptions and registry facts through compatibility accessors while migrating callers.
- [x] Run focused tests, format, and clippy.
- [x] Commit with message `feat(astronomy): model stable neighbor catalog`.

Task 5 sequencing: `neighbor_catalog` stores 24–40 physical records, while
`neighbors` remains the original notable subset until Task 6 migrates figure
identity and brightness. See campaign ledger entry #4. Stage 3 remains open
until Task 6 is implemented.

### Task 6: Add lazy deterministic background stars and repair figures

**Files:**
- Modify: `domains/astronomy/src/starfield.rs`
- Modify: `domains/astronomy/src/figures.rs`
- Modify: `domains/astronomy/src/night_sky.rs`
- Test: starfield, figures, and night-sky tests

**Interfaces:** Background-star queries take `(astronomy_seed, sky_cell/region, observer constraints)` and return the same generated stars regardless of query order. `Figure` stores stable member IDs and uses a shared brightness threshold.

- [x] Write failing tests for query-order independence, repeated-cell identity, magnitude cutoffs, and figure member-ID stability.
- [x] Run focused tests and verify red status.
- [x] Implement deterministic cell-keyed lazy generation; do not retain a global mutable cache as part of simulation state.
- [x] Update figure clustering to consume stable IDs and avoid assumptions that every neighbor has the same magnitude class; retain naked-eye limits.
- [x] Run focused tests and compare generated figure counts/descriptions against intentional baselines.
- [x] Commit with message `refactor(astronomy): make starfield lazy and figures identity-based`.

## Stage 4: Observer sky and cultural constellations

### Task 7: Expose species-specific naked-eye sky visibility

**Files:**
- Modify: `domains/astronomy/src/night_sky.rs`
- Modify: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/species/src/lib.rs` only if an existing trait accessor is insufficient
- Modify: `windows/worldgen/src/observer.rs`
- Test: astronomy and species/worldgen observation tests

**Interfaces:** A sky-observation query consumes species baseline traits plus observer latitude, local time, atmosphere, moonlight, and sky geometry; it returns visible stable-star/comet/shower candidates. It does not assign cultural names or meanings.

- [ ] Write failing tests distinguishing diurnal, crepuscular, and nocturnal schedules; night vision and attention modify thresholds continuously; individual variation is absent.
- [ ] Run focused tests and verify red status.
- [ ] Implement deterministic hard schedule gating plus continuous acuity/attention, twilight, atmosphere, moonlight, and horizon modifiers.
- [ ] Keep physical visibility separate from salience/attention so downstream cultural code receives candidates and evidence, not preassigned significance.
- [ ] Run focused tests, clippy, and format checks.
- [ ] Commit with message `feat(astronomy): expose species-specific sky visibility`.

### Task 8: Add culture-owned constellation inputs and close the first slice

**Files:**
- Create: `domains/culture/src/constellations.rs`
- Modify: `domains/culture/src/lib.rs`
- Modify: `windows/worldgen/src/observer.rs`
- Modify: `domains/astronomy/src/figures.rs` only for stable physical candidate output
- Test: culture/worldgen tests plus astronomy integration tests
- Documentation: update `docs/superpowers/specs/2026-09-11-astronomy-deepening-design.md` only if an implemented interface differs from the approved contract

**Interfaces:** Astronomy supplies visible stable-star IDs, positions, brightness, and optional physical descriptors. Culture consumes those candidates and produces constellation groupings, names, and meanings; different cultures may group the same candidates differently.

- [x] Write failing tests showing two cultures can produce different groupings from the same observer-specific candidate set, including low-acuity and daylight cases.
- [x] Run focused tests and verify red status.
- [x] Implement the smallest culture-owned grouping/name interface; keep it naked-eye-only and avoid telescope-only catalog obligations.
- [x] Add an explicit follow-up boundary for eclipsing binaries, transient stellar events, dense meteor-stream clumps, and terminal stellar evolution; do not implement them in this slice.
- [x] Run the full relevant package tests and `cargo clippy --workspace --all-targets -- -D warnings`.
- [x] Update stage statuses in `IMPLEMENTATION_PLAN.md` as each stage lands (repository has no such file; this plan records status).
- [ ] Commit with message `feat(astronomy): support culture-owned constellations`.

## Verification and handoff

- [ ] Run `cargo fmt --check`.
- [ ] Run `cargo clippy --workspace --all-targets -- -D warnings`.
- [ ] Run the relevant astronomy/species/worldgen nextest filters, then the project-required gate at the stage boundary.
- [ ] Run the repository type, placement, plumb, and report-freshness checks required by `gate-commit`.
- [ ] Perform a final `git diff --check`, inspect generated artifacts, and confirm no unrelated files changed.
- [ ] Use `superpowers:requesting-code-review` before merge and `superpowers:verification-before-completion` before claiming completion.
