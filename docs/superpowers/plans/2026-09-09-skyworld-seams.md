# Skyworld Seams Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Prove and, only where necessary, tighten the read-only terrain/climate-to-Skyworld boundary so the next Skyworld slice has measured ownership, deterministic environment-axis propagation, and bounded construction cost.

**Architecture:** Retain `skyworld_from(&World, &GeneratedTerrain, &GeneratedClimate, SkyWorldConfig)` as the composition-root entry point. Treat terrain and climate as already-built surface substrate, Skyworld fields and territories as derived overlay state, and rendering as a pure observation lens; introduce no new build rung, save fact, dense atmospheric field, or universal biome abstraction. The implementation is measurement-first: existing typed inputs and direct borrowing are preferred, and a named wrapper is added only if a probe shows that it clarifies ownership or prevents an actual misuse.

**Tech Stack:** Rust workspace; `hornvale-worldgen`; existing `GeneratedTerrain`, `GeneratedClimate`, `Biome`/`BiomeExpr`, `SkyWorld`, `SkyWorldConfig`, `VertexMap`, labeled `Seed` streams, worldgen integration suite, and the existing terrain/climate reconstruction counters.

**Spec:** `docs/superpowers/specs/2026-09-09-skyworld-seams-design.md`

## Global Constraints

- Keep the worldly vocabulary: terrain, climate, biome, fields, stocks, territories, and rendered consequences remain distinct concepts.
- Do not add a `BuildDepth` rung, save-format fact, lifecycle mutation, organisms, species, tethering, mutable atmosphere, full fluid solver, or other realm implementation.
- Preserve `Biome`/`BiomeExpr`, existing surface outputs, stream order, save facts, and ordinary renderer output; any intentional epoch change requires a separate decision.
- Reuse one `GeneratedTerrain` and one `GeneratedClimate` for all Skyworld territories and samples; never reconstruct them per territory, pixel, query, or time slice.
- Keep generation and rendering deterministic, cache-independent, and free of random draws during queries, sorting, rendering, or readout.
- Any perturbation test must prove that its intended input changed before asserting a dependent Skyworld difference; a no-op perturbation fails.
- Work must scale with active territories, requested trajectory samples, and rendered pixels—not vertices multiplied by temporal samples.
- Censuses are not part of this campaign; generated audit reports are refreshed only when the normal gate requires them.

---

### Task 1: Establish the seam inventory and non-vacuous environment probes

**Goal:** Make the existing composition boundary observable in tests and record exactly which source values feed each Skyworld layer before changing production APIs.

**Status:** Complete — commits `e2eac9dbd` and `7985595d7`; scoped review approved.

**Files:**
- Modify: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `windows/worldgen/src/skyworld.rs` only if a narrowly scoped public read-only seam is required by a failing test
- Modify: `windows/worldgen/src/lib.rs` only if that seam must be re-exported from the composition root
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`

**Interfaces:**
- Consumes: `GeneratedTerrain::{elevation_at,is_ocean,unrest_at,boundary_at,features}`, `GeneratedClimate::{mean_temperature_at,moisture_at,storm_propensity_at,current_at,biome_expr_at}`, the composition-root call to `hornvale_climate::prevailing_wind`, and `skyworld_from`.
- Produces: deterministic test helpers that build the existing `Fixture`, sample a `Vertex`, and compare `SkyWorld` fields/stocks/footprints without mutating source artifacts. If production ownership is already clear through the existing typed arguments, this task produces no wrapper.

- [x] **Step 1: Add a fixture sampling helper and write the failing axis matrix tests.** Use the existing terrain-depth fixture and select vertices by ascending `Vertex` id. For each selected vertex, record a source tuple containing `is_ocean`, elevation, mean temperature, moisture, storm propensity, biome expression, and tectonic feature state; record the corresponding Skyworld field/territory output. Add tests named `surface_axis_is_a_read_only_substrate`, `environment_axes_have_non_vacuous_sources`, and `skyworld_outputs_change_only_through_dependent_axes`.

  The test must assert the source-side facts first:

  ```rust
  let before = sample_surface(&fixture, vertex);
  let after = sample_surface(&perturbed, vertex);
  assert_ne!(before, after, "VACUOUS: intended source perturbation did not change");
  ```

  Then assert that land/ocean remains a terrain fact, `BiomeExpr` remains the climate-owned taxonomy, and a surface input perturbation does not silently rewrite unrelated Skyworld axes.

- [x] **Step 2: Run the new tests and confirm the first red result is meaningful.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld::seams`

  Expected: a focused failure naming either a missing probe helper or a genuinely unobservable/unchanged axis; do not accept a test that passes because its source perturbation was a no-op.

- [x] **Step 3: Inspect the failure against existing APIs before adding code.** Verify the source can be varied through existing `TerrainPins`, climate derivation, or a test-only copied value. Reject any proposed wrapper that only renames `&GeneratedTerrain` and `&GeneratedClimate`; retain direct typed inputs when they already express ownership. If a consumer cannot identify substrate ownership without duplicating access logic, add one small read-only `SkySurfaceSample`/accessor at `windows/worldgen/src/skyworld.rs`, with no cached mutable state and no dependency edge from `domains/terrain` or `domains/climate`.

- [x] **Step 4: Implement the minimum seam or test-only sampler and make the matrix green.** Keep all production derivation in `skyworld_from`; the seam may expose only the source values needed by the tests. Do not add a new `BuildDepth`, alter `BuildArtifacts`/`RungArtifacts`, or serialize the sample.

- [x] **Step 5: Run the complete focused suite and record the ownership map.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: all existing Skyworld tests plus the seam tests pass, with nonempty land/ocean, altitude, moisture, temperature, wind, feature, radiation, and aether denominators. Record the measured source→field/stock/footprint ownership and any rejected wrapper in the ledger.

- [x] **Step 6: Commit the independently testable seam inventory.**

  ```bash
  git add windows/worldgen/tests/suite/skyworld.rs windows/worldgen/src/skyworld.rs windows/worldgen/src/lib.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "test: map Skyworld environment seams"
  ```

### Task 2: Prove independent environmental propagation

**Goal:** Demonstrate that stable changes in surface and atmospheric inputs reach only the Skyworld quantities that should depend on them, without introducing a second biome taxonomy or authored placement rules.

**Status:** Complete — commits `b5c9b39d3`, `a410a2bfb`, and `4d9f3005b`; scoped review approved after two fix rounds.

**Files:**
- Modify: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `windows/worldgen/src/skyworld.rs` only for the smallest justified pure derivation correction
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`

**Interfaces:**
- Consumes: Task 1’s fixture/sample seam, `skyworld_from`, `SkyFields::at_altitude`, existing `TerrainPins`, climate accessors, and terrain feature accessors.
- Produces: a deterministic one-axis-at-a-time probe matrix covering substrate, climate, high-sky radiation, aether, and tectonic/environmental features.

- [x] **Step 1: Write the red perturbation tests.** Add `radiation_and_aether_are_independently_perturbable`, `surface_climate_axes_feed_distinct_overlay_values`, `tectonic_features_affect_distribution_without_authored_placement`, and `perturbations_preserve_surface_projection`. Each test must assert source inequality first, then compare only the dependent `SkyWorld` projection (fields, territory selection, stocks, footprint, or readout) and assert unchanged source/surface values where appropriate.

- [x] **Step 2: Run the matrix to identify real missing propagation.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld::seams`

  Expected: FAIL for a missing dependency or no-op probe, not for nondeterministic ordering or a changed unrelated surface biome. Save the exact source and output counts in the test failure notes while iterating.

- [x] **Step 3: Make the smallest pure derivation change required by the red test.** Keep `derive_fields`, `distribution_score`, and stock derivation deterministic and composition-root-owned. Use existing climate/terrain values rather than adding a Skyworld-specific replacement for `BiomeExpr`; environmental features may bias coverage/distribution only through derived scores, never through fixed authored placements.

- [x] **Step 4: Run all Skyworld tests and verify cross-seed variation remains real.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: same-seed identity, different-seed variation, radiation/aether independence, mixed land/ocean activation, surface preservation, and all one-axis dependency tests pass.

- [x] **Step 5: Commit the propagation matrix.**

  ```bash
  git add windows/worldgen/tests/suite/skyworld.rs windows/worldgen/src/skyworld.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "test: verify Skyworld environmental propagation"
  ```

### Task 3: Measure artifact reuse and bounded construction cost

**Goal:** Establish with counters and bounded-work assertions that Skyworld generation and rendering reuse existing substrate artifacts and do not scale as a planet-by-time rebuild.

**Status:** Complete — commits `f2fa42771` and `2a398be5e`; scoped review approved.

**Files:**
- Modify: `windows/worldgen/src/lib.rs` only if existing test-only reconstruction counters need a narrowly scoped reset/read helper
- Modify: `windows/worldgen/src/skyworld.rs` only if a pure work counter is needed to expose requested territory/sample work
- Modify: `windows/worldgen/src/skyworld_render.rs` only if a measured renderer path reconstructs inputs or materializes unnecessary detail
- Modify: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`

**Interfaces:**
- Consumes: `TERRAIN_OF_CALLS`, `CLIMATE_FROM_CALLS`, `skyworld_from`, `trajectory_at`, `propagation_at`, and `render_skyworld_{png,readout,diagnostic_readout}`.
- Produces: deterministic reuse and scaling probes; any added counter must be test-only or an explicit returned diagnostic, must not affect stream consumption, and must not become a saved world fact.

- [x] **Step 1: Write the red reuse tests.** Add `skyworld_generation_reuses_passed_substrate`, `rendering_does_not_reconstruct_substrate`, `queries_do_not_consume_randomness`, and `detail_changes_materialization_not_generation`. Reset the existing counters around generation, query, and rendering; assert zero additional `terrain_of`/`climate_from` calls after the fixture has supplied its artifacts, and compare generated Skyworld values before and after queries/renders.

- [x] **Step 2: Run the cost probes and inspect the first failing path.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld::cost`

  Expected: a failure identifies an actual reconstruction or missing observable bound. If integration tests cannot access the existing `#[cfg(test)]` counters, place the counter assertion in the worldgen crate’s existing unit-test module or add a narrowly scoped `#[cfg(test)]` helper rather than making counters part of the public runtime API.

- [x] **Step 3: Add only the required reuse fix or measurement hook.** Thread already-built `GeneratedTerrain`/`GeneratedClimate` references through any offending call path. Do not add a cache that changes ownership or output. For scaling, compare small/large `trajectory_samples`, territory counts, and render detail while asserting work is bounded by those requested quantities; do not introduce a full vertex×time field.

- [x] **Step 4: Run deterministic scaling and rendering checks.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: zero substrate reconstructions, byte-identical repeated queries and renders, stable stream behavior, and measured work that grows with active territories/samples/pixels rather than with all vertices multiplied by all times. Record actual counts and command output in the ledger.

- [x] **Step 5: Commit the cost evidence.**

  ```bash
  git add windows/worldgen/src/lib.rs windows/worldgen/src/skyworld.rs windows/worldgen/src/skyworld_render.rs windows/worldgen/tests/suite/skyworld.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "test: measure Skyworld substrate reuse"
  ```

### Task 4: Capture the reusable cross-realm contract and close the slice

**Goal:** Turn the measured seam into a small reusable contract for future Waterworld and Underworld overlays while keeping those realms unimplemented and all deferred worldbuilding ideas discoverable.

**Status:** Complete — final contract and immutability probes captured in
`7e9e6be81` and hardened in its review fix round.

**Files:**
- Modify: `windows/worldgen/tests/suite/skyworld.rs` with final immutability and contract probes
- Modify: `docs/superpowers/ledgers/2026-09-09-skyworld-seams.md` with measurements, reusable contract, rejected abstractions, and follow-ups
- Modify: `docs/audits/campaign-reconciliation.tsv` to include this plan path when the campaign record is complete
- Modify: `docs/superpowers/specs/2026-09-09-skyworld-seams-design.md` only if implementation evidence requires an approved clarification
- Modify: `book/src/frontier/idea-registry.md` only if a measured implementation result changes a registered idea’s status

**Interfaces:**
- Consumes: Tasks 1–3’s probes and measured counts, existing `SkyWorld`/render APIs, and the approved seam spec.
- Produces: a ledger-backed contract consisting of surface substrate, ambient prerequisite fields, aggregate stocks, movement/adjacency, bounded propagation channels, and ordinary/diagnostic observation; realm-specific dimensions remain explicit (vents/currents, cave energy, aether bands, and projection rules).

- [x] **Step 1: Write the final contract and immutability probes.** Add `render_and_query_paths_leave_inputs_unchanged` and `skyworld_contract_has_no_build_or_save_surface`. Assert that terrain/climate values, `BiomeExpr`, generated Skyworld data, and ordinary output remain unchanged after all query/detail paths; inspect the public exports and `BuildDepth`/`BuildArtifacts` source to ensure no new rung or fact was introduced.

- [x] **Step 2: Run the complete focused suite and documentation consistency test.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: all Skyworld probes pass.

  Run: `cargo test -p hornvale --test suite -- docs_consistency`

  Expected: reconciliation, spec, plan, ledger, registry, and generated-document checks pass.

- [x] **Step 3: Update the ledger with evidence and explicit non-results.** Record actual command output/counts, the four-layer ownership table, the smallest cross-realm contract, any rejected wrapper/cache/abstraction, and follow-ups for lifecycle, co-evolution, tethering, species, and Waterworld transfer. Do not claim a census or realm implementation.

- [x] **Step 4: Run the local commit gate and inspect the diff.**

  Run: `make gate-commit`

  Expected: formatting, clippy, type/placement/plumb audits, report freshness, and the sub-floor tier pass. Review the diff manually; preserve valid trailing empty TSV columns in the reconciliation file.

- [x] **Step 5: Commit the completed seam-audit slice.**

  ```bash
  git add windows/worldgen/tests/suite/skyworld.rs docs/superpowers/ledgers/2026-09-09-skyworld-seams.md docs/audits/campaign-reconciliation.tsv docs/superpowers/specs/2026-09-09-skyworld-seams-design.md book/src/frontier/idea-registry.md
  git commit -m "docs: capture Skyworld seam audit"
  ```

## Spec coverage and intentional gaps

- Sections 2–3: Task 1 preserves the worldly model and makes the four ownership layers observable without forcing a wrapper.
- Section 4: Task 2 exercises land/ocean, climate, atmospheric, and feature axes with non-vacuous one-axis perturbations.
- Section 5: Task 3 measures substrate reuse, query/render purity, and requested-work scaling.
- Section 6: Task 4 records the shared contract and keeps vents, cave energy, currents, aether bands, and projection realm-specific.
- Section 7: Tasks 1–4 preserve deterministic streams, surface outputs, build depth, and save facts.
- Sections 8–9: Tasks 1–4 deliver focused probes, ledger measurements, and documentation consistency.
- Deferred lifecycle, organisms, species, tethering, mutable atmosphere, cross-realm implementation, magic, technology, and census work remain outside this slice.

## Self-review checklist

- [x] Every spec section has a named task or an explicit intentional gap.
- [x] No task adds a `BuildDepth` rung, save-format fact, sibling-domain dependency, or dense planet×time simulation.
- [x] Every perturbation proves its source changed before checking downstream behavior.
- [x] Existing reconstruction counters are reused before any new instrumentation is proposed.
- [x] The plan permits no-op production changes when direct typed inputs already satisfy the seam.
- [x] Every task ends with a focused test cycle and a commit.
- [x] The ledger and reconciliation record remain synchronized with the spec and plan paths.

## Broad-review correction

Tasks 1–3 and their recorded steps are complete. The broad-review fixes retain
the production score-helper extraction at `4d9f3005b` and add only test-build
input overrides and work diagnostics. The ocean-fraction integration probe
is a joint perturbation; independent temperature, moisture, elevation, storm,
wind, derived-current, and altitude probes live in the source-module tests.
`GeneratedClimate::current_at` is not consumed by Skyworld propagation.

Run both test locations with:
`cargo test -p hornvale-worldgen --lib --test suite -- skyworld`.

The dedicated seam ledger #5 records actual counts and limits: fixed surface
and index work, sample-dependent footprint traversal, territory-pair adjacency,
and detail-dependent overlay marks. It does not claim a universal complexity
bound, a census, or another realm implementation. Campaign review/close remains
pending.
