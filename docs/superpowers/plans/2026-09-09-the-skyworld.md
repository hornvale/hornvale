# The Skyworld Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a deterministic, generated, and rendered Skyworld overlay whose first specimen is a free-drifting mature orchard descended from a biological sky reef.

**Architecture:** Keep the existing fixed-cell climate/biome projection intact and add a compact mobile-habitat model at the `windows/worldgen` composition root. The model derives world-level sky fields, sparse territories, trajectories, projected cells, bounded propagation, and readouts from existing built world inputs; it does not add an entity per plankton, plant, or sky cell. Rendering remains a pure lens over generated data, with physical, exchange, influence, and rendered footprints kept separate.

**Tech Stack:** Rust workspace; `hornvale-worldgen`; `hornvale-kernel` `Seed`, `Stream`, `WorldTime`, `VertexMap`, geosphere and PNG helpers; existing `hornvale-climate`, `hornvale-astronomy`, and `hornvale-terrain` providers; consolidated `windows/worldgen/tests/suite.rs` integration tests.

**Spec:** `docs/superpowers/specs/2026-09-09-the-skyworld-design.md`

## Global Constraints

- Sky coverage is drawn and world-specific, but never exceeds the global 10% projected-cell ceiling.
- The surface layer remains intact beneath the sky overlay; sky territories occupy an additional layer and never replace land or ocean cells.
- The first slice stops at deterministic generation and rendering; lifecycle mutation, tethering, co-evolution, agent affordances, GOAP, and species simulation remain deferred follow-ups recorded in `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`.
- World seed is identity and controls every draw; draw streams are labeled by independent concern: coverage, distribution, atmosphere, territory phenotype, lineage, and movement variation.
- Randomness is consumed during generation or explicit time-slice derivation, never during rendering, cache lookup, or readout sorting.
- Stable ordering and explicit tie-breaking are required wherever territories, cells, or corridors compete.
- Caches may reduce work but must not alter output, stream consumption, or observable ordering.
- Coarse fields, sparse territories, bounded influence queries, and event records are preferred to all-cells/all-ticks integration.
- The first slice carries only a small fixed vector of habitat stocks and traits; it does not materialize sky plankton or every plant as an entity.
- Work scales with active territories, sampled trajectory segments, and requested detail rather than with the full planet multiplied by every temporal sample.
- Ordinary rendering presents phenomena and consequences, not hidden causal state as an inhabitant's observation; diagnostic fields are a separate readout.
- New seeded draws require stable labeled streams and must not be inserted into an existing sequential stream.
- No new runtime dependency is needed; use existing kernel and workspace utilities.

---

## Task 1: Skyworld data model and deterministic field derivation

**Goal:** Establish the compact overlay types and deterministic world-level fields without changing existing biome semantics.

**Success Criteria:** `SkyWorld::generate` can produce a nonempty, bounded sky overlay from a built world plus existing terrain and climate; all public types have stable ordering and no hidden renderer-only state.

**Tests:** deterministic equality, seed variation, 10% ceiling, land/ocean activation, intact surface layer, world-specific altitude profiles, independent aether/radiation fields, and no new domain dependency edges.

**Files:**

- Create: `windows/worldgen/src/skyworld.rs`
- Modify: `windows/worldgen/src/lib.rs` to expose the module and composition-root entry points
- Modify: `windows/worldgen/src/streams.rs` to add the Skyworld stream labels
- Modify: `windows/worldgen/tests/suite.rs` to register the Skyworld integration test module
- Create: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md` with implementation decisions discovered during this stage

**Interfaces:**

- Consumes: `&World`, `&GeneratedTerrain`, `&GeneratedClimate`, the built geosphere, existing `Biome`/`BiomeExpr` projections, existing wind/current/climate accessors, and `Seed`-derived labeled streams.
- Produces: `SkyWorldConfig`, `SkyWorld`, `SkyFields`, `SkyTerritory`, `SkyPhenotype`, `SkyLineage`, `SkyStocks`, `SkyPosition`, `SkyFootprint`, `SkyPropagation`, and `skyworld_from(world, terrain, climate, config) -> SkyWorld`.

- [ ] **Step 1: Write the failing data-model tests.** Add tests that construct the same built fixture twice and assert that the future public result can support byte-stable comparison; assert that the test fixture exposes at least one land and one ocean projection candidate; assert that a coverage count can be compared against `floor(vertex_count * 0.10)` without replacing the underlying surface.

- [ ] **Step 2: Run the focused tests and verify they fail for the missing module and entry point.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: compilation failure identifying the not-yet-defined Skyworld module or public types.

- [ ] **Step 3: Add stable stream labels and compact public types.** Define labels under the worldgen-owned stream roster, using names such as `SKYWORLD_COVERAGE`, `SKYWORLD_DISTRIBUTION`, `SKYWORLD_ATMOSPHERE`, `SKYWORLD_PHENOTYPE`, `SKYWORLD_LINEAGE`, and `SKYWORLD_MOVEMENT`, each with a versioned path and documentation explaining its independent concern. Define plain `Clone`, `Debug`, `PartialEq` data types with explicit scalar fields:

  ```rust
  pub struct SkyWorldConfig {
      pub max_projected_fraction: f64,
      pub trajectory_samples: u16,
      pub propagation_radius: u16,
  }

  pub struct SkyFields {
      pub pressure: f64,
      pub density: f64,
      pub high_sky_radiation: f64,
      pub aether: f64,
      pub moisture: f64,
      pub wind: [f64; 3],
      pub wind_shear: f64,
      pub lunar_forcing: f64,
      pub stellar_forcing: f64,
  }

  pub struct SkyWorld {
      pub fields: SkyFields,
      pub territories: Vec<SkyTerritory>,
  }

  pub struct SkyTerritory {
      pub id: u32,
      pub lineage: SkyLineage,
      pub phenotype: SkyPhenotype,
      pub stocks: SkyStocks,
      pub origin: SkyPosition,
      pub trajectory: Vec<SkyPosition>,
      pub physical: SkyFootprint,
      pub exchange: SkyFootprint,
      pub influence: SkyPropagation,
  }
  ```

  Keep numeric fields in the project's existing raw-compute/quantize-at-emit style and use ordered vectors rather than hash iteration.

- [ ] **Step 4: Implement pure field derivation.** Derive altitude bands, pressure/density/lapse proxies, high-sky radiation, aether concentration, moisture, wind, shear, and lunar/stellar forcing from existing climate and astronomy values plus the Skyworld atmosphere stream. Make the radiation filter and aether availability independently testable: lower bands can retain moisture while losing high-sky radiation, and aether must not be inferred from radiation.

- [ ] **Step 5: Implement bounded, mixed coverage and territory draws.** Draw a coverage target below the configured ceiling; choose both clustered and isolated territory seeds using ordered vertex traversal and existing terrain/ocean classification; assign each territory a compact phenotype, lineage, movement regime, stability profile, and initial orchard stocks. Ensure the fixed surface `Biome`/`BiomeExpr` data is read-only input and is never mutated or replaced.

- [ ] **Step 6: Run the focused tests and add property tests for bounds and variation.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: PASS for same-seed equality, different-seed variation, coverage ceiling, mixed land/ocean projections, clustered/isolated representation, intact surface data, and independent atmospheric fields.

- [ ] **Step 7: Commit the stage.**

  ```bash
  git add windows/worldgen/src/skyworld.rs windows/worldgen/src/lib.rs windows/worldgen/src/streams.rs windows/worldgen/tests/suite.rs windows/worldgen/tests/suite/skyworld.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "feat: add Skyworld overlay data model"
  ```

## Task 2: Orchard resource chain, trajectories, adjacency, and propagation

**Goal:** Turn each generated territory into a coherent mature orchard with prerequisites, deterministic movement, temporal adjacency, and bounded influence readouts.

**Success Criteria:** The orchard’s resources explain fruit through ambient prerequisites and aggregate stocks; trajectories are cache-independent and deterministic; local kernels, wind corridors, and sparse events remain distinct.

**Tests:** productivity requires aether, radiation, and moisture; resource prerequisites are visible; trajectory determinism/cache independence; vertical and lateral adjacency; footprint separation; propagation shape separation; performance scales with active territories and samples.

**Files:**

- Modify: `windows/worldgen/src/skyworld.rs`
- Create: `windows/worldgen/src/skyworld_propagation.rs` if propagation exceeds the single-responsibility boundary of the generator
- Modify: `windows/worldgen/src/lib.rs` for public trajectory/propagation accessors
- Modify: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`

**Interfaces:**

- Consumes: Stage 1 `SkyWorld`, `SkyTerritory`, world time, existing prevailing winds/ocean currents where applicable, and `ResourceAxis`/`ResourceKind` semantics from `hornvale-kernel::ecology`.
- Produces: the expanded `SkyStocks` beginning with `SkyStocks::plankton` as the productivity value, `SkyTrajectorySample`, `SkyFootprint`, the existing `SkyPropagation` struct's explicit `local`, `corridors`, and `events` channels, `SkyAdjacency`, `trajectory_at(territory_id, time_slice)`, and `propagation_at(territory_id, detail)`.

- [ ] **Step 1: Write failing orchard-chain tests.** Assert that zero aether, zero high-sky radiation, or zero moisture independently prevents sky-plankton productivity; assert that positive values produce plankton, fungal/root support, soil fertility, canopy biomass, flowers, pollination, and fruit in dependency order; assert that stock quantities remain finite and bounded.

- [ ] **Step 2: Run the orchard tests to verify the prerequisite chain is absent.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld::orchard`

  Expected: compilation failure or failed assertions for the missing productivity and stock derivation.

- [ ] **Step 3: Implement aggregate orchard stocks.** Add a fixed stock vector with named fields for plankton, fungal/root support, soil fertility, canopy biomass, flowers, pollination capacity, fruit, cloud water, detritus, seed/spore reserve, and animal forage. Derive each from the previous stage and the sampled atmospheric fields; use existing `Field`/`Stock` vocabulary in documentation and do not add individual microbe or plant entities.

- [ ] **Step 4: Write failing trajectory and footprint tests.** Assert that a territory’s sampled position is stable for the same `(world, territory, time slice)`, changes for a different seed or movement profile, remains ordered, and yields distinct physical, exchange, and influence footprints. Assert that vertical adjacency projects onto the current land or ocean cell while lateral adjacency is limited to reachable territory/corridor candidates.

- [ ] **Step 5: Implement coarse deterministic movement.** Sample existing wind/current-like fields along a trajectory using the movement stream, with lunar and stellar forcing as modulation rather than a fluid solver. Store only requested trajectory samples; derive the physical projection at each sample from the geosphere’s nearest vertices. Use explicit vertex/id ordering and `f64::total_cmp` where a choice depends on floating-point scores.

- [ ] **Step 6: Implement the three bounded propagation forms.** Use the existing `SkyPropagation` struct's `local` channel for a bounded local kernel, its `corridors` channel for ordered wind corridors carrying seeds, spores, plankton, and routes, and its `events` channel for sparse bloom, storm, and collapse records. Do not broadcast a territory’s influence to every planet cell. Keep the channels explicit so consumers cannot confuse a corridor with a dense field.

- [ ] **Step 7: Run focused tests and a small scaling probe.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: PASS for prerequisite productivity, deterministic/cache-independent trajectory, footprint separation, adjacency, propagation-form separation, and a probe showing work grows with territory/sample count rather than full planet × time grid.

- [ ] **Step 8: Commit the stage.**

  ```bash
  git add windows/worldgen/src/skyworld.rs windows/worldgen/src/skyworld_propagation.rs windows/worldgen/src/lib.rs windows/worldgen/tests/suite/skyworld.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "feat: derive Skyworld orchard and trajectories"
  ```

## Task 3: Deterministic ordinary and diagnostic rendering

**Goal:** Render the Skyworld as an additional layer at planet, regional, and habitat detail without repainting or re-deriving the full planet for each moving territory.

**Success Criteria:** Rendering is byte-stable, shows the surface beneath the overlay, keeps the four footprints distinct, and does not expose diagnostic causes in the ordinary view.

**Tests:** PNG/readout determinism, ordinary/diagnostic separation, level-of-detail behavior, surface preservation, and render cost independent of full-world repaint for one moving territory.

**Files:**

- Create: `windows/worldgen/src/skyworld_render.rs`
- Modify: `windows/worldgen/src/lib.rs` to expose render functions
- Modify: `windows/worldgen/tests/suite/skyworld.rs`
- Modify: `windows/worldgen/tests/suite.rs` only if a dedicated rendering module needs registration
- Modify: `docs/generated-paths.txt` only if a committed Skyworld artifact is intentionally added
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md`

**Interfaces:**

- Consumes: `&SkyWorld`, existing terrain/climate surface readouts, `SkyWorldDetail::{Planet, Regional, Habitat}`, and an observer-independent render request.
- Produces: `render_skyworld_png(&SkyWorld, &GeneratedTerrain, detail) -> Vec<u8>`, `render_skyworld_readout(&SkyWorld, detail) -> String`, and a diagnostic-only readout that names atmospheric/resource fields separately from the ordinary phenomenon view.

- [ ] **Step 1: Write failing render tests.** Assert that the same generated input yields byte-identical PNG and text output; assert that planet detail omits full orchard internals, regional detail includes physical projection and broad route/influence, and habitat detail includes phenotype, lifecycle, and stocks; assert that ordinary output contains consequences such as shadow/spores/rain/cloud contact only when present and does not contain raw aether/radiation causes.

- [ ] **Step 2: Run focused rendering tests and verify the renderer is absent.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld::render`

  Expected: compilation failure for the missing render module or functions.

- [ ] **Step 3: Implement the ordinary surface-plus-overlay renderer.** Reuse the existing equirectangular PNG and nearest-vertex conventions. Paint the ordinary land/sea base first, then the selected Skyworld physical footprint, then optional exchange/influence indicators according to detail. Keep palette selection exhaustive over phenotype/lifecycle projections and use ordered territory traversal.

- [ ] **Step 4: Implement diagnostic and text readouts.** Expose a separate diagnostic view for pressure, radiation, aether, wind, moisture, stocks, and propagation form. The ordinary readout should describe visible consequences and the orchard’s projected presence without claiming that an observer sees hidden causal fields.

- [ ] **Step 5: Add level-of-detail materialization.** At planet detail, rasterize coverage masks, centroids, broad corridors, and sparse events. At regional detail, materialize physical footprint, projection, local influence, and route. At habitat detail, include the compact orchard state. Avoid rebuilding unrelated territory geometry when one trajectory sample changes.

- [ ] **Step 6: Run focused render tests and inspect generated bytes.**

  Run: `cargo test -p hornvale-worldgen --test suite -- skyworld`

  Expected: PASS for byte identity, ordinary/diagnostic separation, detail-specific fields, preserved surface, and distinct footprint rendering.

- [ ] **Step 7: Commit the stage.**

  ```bash
  git add windows/worldgen/src/skyworld_render.rs windows/worldgen/src/lib.rs windows/worldgen/tests/suite/skyworld.rs docs/superpowers/ledgers/2026-09-09-the-skyworld.md
  git commit -m "feat: render Skyworld overlay"
  ```

## Task 4: Integration probes, documentation, and campaign close preparation

**Goal:** Validate the full generation/rendering slice against the project’s determinism, coverage, layering, and artifact conventions without prematurely adding lifecycle simulation or cross-realm mutation.

**Success Criteria:** The complete Skyworld surface is exercised through the composition root, all spec probes pass, no stale generated artifact is committed, and deferred ideas remain discoverable in the ledger and registry.

**Tests:** workspace-scoped Skyworld tests, full worldgen suite, documentation consistency, gate-commit, and the appropriate queued stage gate before merge.

**Files:**

- Modify: `windows/worldgen/tests/suite/skyworld.rs` with cross-seed and edge-case probes
- Modify: `windows/worldgen/src/lib.rs` only if final composition-root adapters are needed
- Modify: `docs/superpowers/ledgers/2026-09-09-the-skyworld.md` with measured results and explicit non-results
- Modify: `docs/superpowers/specs/2026-09-09-the-skyworld-design.md` only for approved clarifications discovered during implementation
- Modify: `docs/audits/campaign-reconciliation.tsv` at the campaign boundary when the plan and implementation records are complete
- Modify: `book/src/frontier/idea-registry.md` only when an implementation result changes a registered idea’s status

- [ ] **Step 1: Add cross-seed and edge-case probes.** Cover a world with minimal valid sky coverage, a world with ocean-dominant activation, a world with land-dominant activation, a world with both clustered and isolated territories, repeated trajectory queries, and detail changes that must not change the underlying generated data.

- [ ] **Step 2: Run the focused and worldgen suites.**

  Run: `cargo nextest run -p hornvale-worldgen --test suite -- skyworld`

  Expected: all Skyworld tests pass with nonempty denominators for coverage and activation probes.

- [ ] **Step 3: Run documentation and formatting checks.**

  Run: `cargo test -p hornvale --test suite -- docs_consistency`

  Expected: all documentation links, registry rows, reconciliation rows, and campaign record paths pass.

- [ ] **Step 4: Run the local commit gate and inspect the diff.**

  Run: `make gate-commit`

  Expected: formatting, clippy, audits, and the sub-floor tier pass; inspect `git diff --check` while recognizing that valid empty trailing TSV columns in the reconciliation file are schema delimiters.

- [ ] **Step 5: Submit the stage boundary to the canonical queue.** Push the branch, capture the exact commit object, and submit that object to the queue.

  ```bash
  git push origin campaign/skyworld
  SKYWORLD_REF="$(git rev-parse HEAD)"
  make sluice-stage BRANCH=campaign/skyworld REF="$SKYWORLD_REF"
  ```

  Do not run an expensive stage gate locally or bypass the queue.

- [ ] **Step 6: Record the implementation result and stop for review.** Update the ledger with measured coverage, timing, determinism, and any rejected seam; update the reconciliation row only with paths that the current repository audit population accepts. The campaign remains open for the next approved slice until the Skyworld implementation and rendering behavior have been reviewed.

---

## Spec coverage and intentional gaps

- Sections 3.1–3.3: Stage 1 preserves the biome taxonomy, bounds coverage, and derives stable world-specific fields.
- Sections 3.4–3.5 and 4.2–4.5: Stages 1–2 represent the orchard specimen, phenotype, compact lifecycle lineage, stocks, and distinct mobile identity without simulating transitions.
- Sections 5.1–5.3: Stage 2 implements coarse forcing, four footprints, temporal adjacency, and three bounded propagation forms.
- Sections 6.1–6.3: Stage 3 implements generation readouts and planet/regional/habitat rendering detail.
- Section 7: All stages use labeled streams, ordered collections, explicit tie-breaking, bounded work, and cache-independent outputs.
- Section 8: Stages 1–4 carry every listed deterministic, coverage, resource, propagation, rendering, and observation probe.
- Section 10: Lifecycle mutation, tethering, archipelago recombination, habitat genetics, lunar resonance memory, Waterworld transfer, affordance discovery, GOAP, ability reservoirs, species expansion, and magical/technological research remain ledger follow-ups rather than hidden implementation scope.

## Self-review checklist

- [x] Every spec section has a named stage or an explicit deferred entry.
- [x] No task depends on a sibling domain importing another sibling; cross-domain composition stays in `windows/worldgen`.
- [x] Public type names and function signatures are consistent across stages.
- [x] Tests are specified before implementation steps and include deterministic, bounded, varied, and rendering behavior.
- [x] No individual plankton, plant, or full-resolution sky-cell entity is introduced.
- [x] The plan does not require a biome rewrite, a new runtime dependency, or a full fluid simulation.
- [x] The plan preserves the user’s broader affordance/species/resource ideas in the follow-up ledger rather than silently dropping them.
