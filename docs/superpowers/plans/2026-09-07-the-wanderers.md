# The Wanderers Instrument Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generalize Hornvale's stellar root to single, wide-binary, and bounded close-binary systems, then expose phase-aware wanderers and observational ephemerides through the native and wasm scene contracts.

**Architecture:** Keep `StarSystem::star` as the primary-star compatibility field while adding a topology-owned stellar configuration and topology-aware derived helpers. Generate binary parameters in the astronomy domain, keep orbital positions as pure functions of elements plus `StdInstant`, and append `stellar` then `wanderers` to `scene/system/v1` without storing sampled trajectories.

**Tech Stack:** Rust workspace, `hornvale-astronomy`, `hornvale-worldgen`, `hornvale-scene`, raw `extern "C"` world-wasm catalog, serde JSON, nextest, committed scene/reference artifacts.

**Spec:** `docs/superpowers/specs/2026-09-07-the-wanderers-design.md`

## Global Constraints

- Preserve deterministic seeded generation and stream-consumption order; new draws use appended `StreamLabel`s owned by `domains/astronomy/src/streams.rs`.
- Preserve the existing single-star behavior and existing `StarSystem::star` field for current consumers; binary behavior routes through new topology-aware helpers.
- Use typed astronomy units at API boundaries and `hornvale_kernel::math` for transcendentals; do not add a dimensional-analysis dependency.
- Quantize only at scene/artifact emission; keep internal calculations at full precision.
- Keep sibling bodies astronomy-first: no sibling terrain, climate, settlement, language, or culture generation.
- Keep the first orbital model bounded: single, wide-binary/circumprimary, close-binary/circumbinary; no arbitrary N-body, close circumsecondary planets, eccentric/inclined wanderers, transits, or occultations.
- Native scene JSON and world-wasm scene JSON must use the same Rust producer path and remain byte-identical for the same world.
- Write tests before implementation for each behavior; run the narrowest relevant tests before broader checks.
- Every task ends with a focused commit; do not use `--no-verify`.

---

### Task 1: Add the stellar-topology root and binary admission

**Files:**
- Create: `domains/astronomy/src/stellar.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Modify: `domains/astronomy/src/system.rs`
- Modify: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/pins.rs`
- Modify: `domains/astronomy/src/star.rs`
- Modify: `domains/astronomy/src/anchor.rs`
- Modify: `domains/astronomy/tests/suite/genesis_properties.rs`
- Modify: `cli/src/main.rs`
- Test: inline astronomy tests plus `domains/astronomy/tests/suite/genesis_properties.rs`

**Interfaces:**
- Consumes: existing `Star`, `Anchor`, `SkyPins`, `GenesisError`, `StreamLabel`, and `StarSystem::star`.
- Produces: `StellarTopology::{Single, WideBinary, CloseBinary}`, `BinaryOrbit`, `StellarConfiguration`, `generate_stellar`, `stellar_gravity_mass`, `stellar_luminosity_at`, and a topology pin accepted by both CLI and wasm pin parsing.

- [ ] **Step 1: Write failing topology tests.** Add tests for: default generation returns `Single`; a wide-binary pin produces a circumprimary configuration; a close-binary pin produces a circumbinary configuration; binary mass/period/phase are deterministic; binary pins do not change the existing primary-star draw; invalid topology-specific orbit ranges return `GenesisError` with the violated constraint named.

- [ ] **Step 2: Run the focused tests and verify they fail.**

  Run: `cargo nextest run -p hornvale-astronomy --test suite genesis_properties`

  Expected: compilation or assertion failures naming the missing topology types/pin behavior. Do not modify production code before observing this red result.

- [ ] **Step 3: Add topology types and appended stream labels.** In `stellar.rs`, define the topology enum, typed binary orbit elements, companion-star record, and `StellarConfiguration`. Keep `StarSystem::star` as the primary-star field and add `stellar: StellarConfiguration`. Add one count/topology stream and one binary-parameter stream after existing astronomy labels; do not insert labels before `WANDERER_COUNT` or `WANDERERS`.

- [ ] **Step 4: Implement topology generation and validation.** Generate single systems through the existing star path unchanged. For wide binaries, generate a bound companion and circumprimary stability envelope. For close binaries, generate two stars, a bounded two-body orbit, barycentric anchor admission, and combined luminosity metadata. Reject pins that cannot admit the requested topology or anchor orbit; never retry across seeds.

- [ ] **Step 5: Route CLI and pin parsing through the same constructor.** Add the topology vocabulary to `parse_pin`, `pin_strings`, CLI usage/help, and `clients/world-wasm/src/lib.rs`'s `SKY_KEYS`. Keep invalid values descriptive and preserve existing pin round trips.

- [ ] **Step 6: Add topology-aware scalar helpers without breaking single-star callers.** Keep `insolation_rel(&Star, &Anchor)` and existing primary-star consumers intact. Add helpers that accept `&StarSystem` and an instant for combined stellar illumination and topology-specific gravitational mass; default single-star results must equal the existing helpers exactly.

- [ ] **Step 7: Run focused tests and inspect the diff.**

  Run: `cargo nextest run -p hornvale-astronomy --test suite genesis_properties`

  Expected: topology tests pass, existing genesis properties remain green, and single-star fixtures show no changed primary-star values.

- [ ] **Step 8: Commit the topology task.**

  Run: `git add domains/astronomy cli/src/main.rs clients/world-wasm/src/lib.rs && git commit -m "feat(astronomy): add stellar system topologies"`

### Task 2: Add phase-aware wanderer and stellar ephemerides

**Files:**
- Create: `domains/astronomy/src/ephemeris.rs`
- Modify: `domains/astronomy/src/lib.rs`
- Modify: `domains/astronomy/src/wanderers.rs`
- Modify: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/calendar.rs`
- Modify: `domains/astronomy/src/provider.rs`
- Modify: `domains/astronomy/tests/suite/genesis_properties.rs`
- Modify: `domains/astronomy/tests/suite/night_sky_regimes.rs`
- Test: `domains/astronomy/src/ephemeris.rs` and astronomy integration suite

**Interfaces:**
- Consumes: `StellarConfiguration`, `StarSystem`, `Wanderer`, `StdInstant`, anchor phase, and typed orbital units from Task 1.
- Produces: `stellar_positions_at(&StarSystem, StdInstant)`, `wanderer_phase_at(&Wanderer, StdInstant)`, `wanderer_position_at(&StarSystem, usize, StdInstant)`, `anchor_relative_longitude_at`, `wanderer_events`, and `wanderer_visibility`.

- [ ] **Step 1: Write failing ephemeris tests.** Cover phase normalization at day zero, negative days, and large days; Kepler period/orbit consistency; inner elongation bounds; outer opposition; complete versus truncated retrograde intervals; single-star equivalence; and wide/close stellar positions.

- [ ] **Step 2: Run the focused tests and verify they fail.**

  Run: `cargo nextest run -p hornvale-astronomy --lib ephemeris`

  Expected: missing module/function or assertion failures for each new evaluator.

- [ ] **Step 3: Add the isolated wanderer phase draw.** Append a `WANDERER_PHASES` stream label. Draw one normalized phase per generated wanderer after existing orbit/class/albedo draws; preserve the existing values and sort order. Add a pin-isolation test proving star, anchor, moons, neighbors, forcing, and pre-phase wanderer fields are unchanged.

- [ ] **Step 4: Implement circular ephemerides.** Use `frac(t / period + phase_offset)` for body phase. Build heliocentric positions in a shared plane, subtract the anchor vector for apparent longitude, and use `kernel::math` for trigonometric operations. For close binaries, use the barycentric stellar frame; for wide binaries, use the circumprimary frame.

- [ ] **Step 5: Implement event and visibility derivations.** Detect conjunction/opposition from relative longitude; emit a retrograde interval only after an apparent-longitude derivative sign change is followed by a return; apply inner glare and outer opposition rules without deleting hidden bodies. Return honest non-recurrence for infinite/zero synodic rates.

- [ ] **Step 6: Integrate provider/almanac vocabulary.** Reuse the existing twilight and phenomenon pathways for morning/evening wanderers. Add multi-star illumination descriptions without changing the single-star text path. Keep proper names and culture-specific interpretations out of astronomy.

- [ ] **Step 7: Run focused tests and verify single-star regression.**

  Run: `cargo nextest run -p hornvale-astronomy --lib ephemeris provider night_sky_regimes`

  Expected: all new ephemeris/event tests pass; existing single-star and regime tests remain green; no existing phenomenon loses its source.

- [ ] **Step 8: Commit the ephemeris task.**

  Run: `git add domains/astronomy && git commit -m "feat(astronomy): derive binary and wanderer ephemerides"`

### Task 3: Emit the generalized system through scene/system/v1 and wasm

**Files:**
- Modify: `windows/scene/src/lib.rs`
- Modify: `windows/scene/tests/suite/golden.rs`
- Modify: `clients/world-wasm/src/lib.rs`
- Modify: `clients/world-wasm/drive.mjs`
- Modify: `book/src/reference/scene-system-v1.md`
- Modify: `windows/scene/examples/ephemeris_golden.rs`
- Test: scene serialization tests and the world-wasm byte-identity smoke

**Interfaces:**
- Consumes: Task 1's `StellarConfiguration` and Task 2's `Wanderer`/ephemeris elements.
- Produces: appended `SystemScene::stellar`, `SystemScene::wanderers`, `StellarElem`/binary element serializers, and the normative v1 evaluator fields.

- [ ] **Step 1: Write failing scene-shape tests.** Assert that existing `schema`, `seed`, `star`, `world`, and `moons` fields retain their order and values; new fields appear after `moons` in the order `stellar`, `wanderers`; single-star seed 42 has a `Single` stellar object; pinned binary fixtures carry both stellar bodies and the expected wanderer count.

- [ ] **Step 2: Run the focused scene tests and verify they fail.**

  Run: `cargo nextest run -p hornvale-scene --test suite system_scene`

  Expected: missing fields or mismatched serialized shape.

- [ ] **Step 3: Add scene element structs and serializers.** Add typed serde output for stellar topology, binary orbit, companion star, and wanderer phase/elements. Quantize only through the existing scene serializers. Omit fields only where the contract explicitly defines absence; do not encode sentinel zeros for unavailable values.

- [ ] **Step 4: Wire native scene generation.** Extend `system_scene` to read the generated system once, serialize the appended `stellar` object, then serialize wanderers in orbital order. Do not recompute physics in `windows/scene`.

- [ ] **Step 5: Wire the wasm path and golden driver.** Keep `hw_scene_system` on the native `hornvale_scene::system_scene` path. Extend the drive script's expected fixture labels and add binary/wanderer fixture coverage without hand-editing generated JSON.

- [ ] **Step 6: Update the normative reference page.** Document topology variants, appended field order, phase normalization, star-position evaluation, circular/two-body approximations, and the exact distinction between primary-star compatibility fields and topology-aware fields.

- [ ] **Step 7: Run native and wasm checks.**

  Run: `cargo nextest run -p hornvale-scene --test suite system_scene`

  Then run: `make world-check`. Expected: the world-wasm build, lint, native/wasm scene smoke, and size gate pass; native and wasm scene JSON are byte-identical for seed 42 and the binary fixture; old single-star scene fields remain unchanged.

- [ ] **Step 8: Commit the scene contract task.**

  Run: `git add windows/scene clients/world-wasm book/src/reference/scene-system-v1.md && git commit -m "feat(scene): expose stellar topologies and wanderers"`

### Task 4: Close documentation, artifacts, and campaign verification

**Files:**
- Modify: `book/src/reference/concept-registry-generated.md` when topology/illumination concepts are registered; otherwise verify the generated report has no drift
- Modify: `book/src/reference/stream-manifest-generated.md`
- Modify: `book/src/frontier/idea-registry.md`
- Modify: `docs/audits/campaign-reconciliation.tsv`
- Modify: `IMPLEMENTATION_PLAN.md`
- Create: `book/src/chronicle/the-wanderers.md` at campaign close
- Create: `docs/retrospectives/the-wanderers.md` at campaign close
- Test: documentation consistency, type/placement/plumb reports, scoped astronomy/scene/client checks

**Interfaces:**
- Consumes: all prior task outputs and generated artifact commands.
- Produces: current stream/concept documentation, captured deferred ideas, completed campaign status, and the close artifacts required by the campaign process.

- [ ] **Step 1: Write documentation freshness checks or update existing fixtures first.** Ensure the stream manifest includes the phase/topology labels and any newly registered concept rows resolve in the registry. Extend `windows/scene/tests/suite/golden.rs` with the v1 field-order assertion; this test owns the contract even if the emitted fixture is later regenerated.

- [ ] **Step 2: Regenerate only the declared artifacts.** Run `make artifacts`, which invokes `scripts/regenerate-artifacts.sh` for the committed non-census artifacts, then inspect the diff. If only expected astronomy/scene/reference rows moved, retain and document them; if unrelated artifacts move, stop and investigate before staging.

- [ ] **Step 3: Capture deferred branches.** Keep `ORRERY-ellipse-truth`, `SKY-transits`, and `SKY-figures-per-species` linked to the campaign; add a registry row for arbitrary multi-star/N-body dynamics or full binary-world promotion only if it is not already represented.

- [ ] **Step 4: Update the status tracker.** Change each stage in `IMPLEMENTATION_PLAN.md` to `Complete` only after its tests and commit exist. Remove `IMPLEMENTATION_PLAN.md` when the campaign is fully closed, per repository guidance; the detailed plan and close artifacts remain in their canonical locations.

- [ ] **Step 5: Run the final scoped verification ladder.** Run formatting, targeted astronomy tests, targeted scene tests, the native/wasm byte-identity check, and documentation consistency. Do not claim full workspace success until the canonical stage/merge gate runs through the sluice.

- [ ] **Step 6: Commit the close documentation.** Use a clear commit explaining the result and preserve any red or deferred findings in the ledger/retrospective rather than weakening tests.
