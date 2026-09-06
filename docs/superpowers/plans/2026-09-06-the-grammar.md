# The Grammar Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement the first executable BIO-3 substrate: body-plan reproductive affordances, composable reproductive operations, hybrid compatibility, and a narrow handoff into the two-tier population substrate. Leave full SOC-2 household and institution simulation for its successor campaign while making its input contract explicit and testable.

**Architecture:** Authored species data projects into a narrow `ReproductiveAffordances` interface. A pure species-domain grammar evaluates permitted reproductive pathways without world state or random draws. The kernel-only demography domain consumes plain, species-independent reproductive summaries and produces cohort-level population inputs. `windows/worldgen` resolves authored species data and supplies those summaries at the composition-root boundary. Projection and narrative layers remain consumers.

**Tech Stack:** Rust workspace, `hornvale-kernel` value types and deterministic math, species-domain unit/integration tests, demography-domain pure tests, worldgen composition-root tests, committed fixture tables only where they are genuine emit-boundary contracts.

**Spec:** `docs/superpowers/specs/2026-09-06-the-grammar-design.md`

## Global Constraints

- Preserve the Murrain boundary: the population substrate is causally authoritative; projections never become biological truth.
- Keep `domains/species` and `domains/demography` kernel-only. Species data must not be imported by demography; worldgen translates between them.
- Do not implement magic, `changeGender`, household simulation, genus taxonomy, or a new canon for existing species in this campaign.
- Keep reproductive state, body form, social gender, and personal identity as distinct concepts. BIO-3 may expose transition capability and realized transition history, but not social interpretation.
- Represent hybrids through compatibility relations and typed outcomes, never through a species-name pair allowlist or a premature genus tree.
- Separate possibility, typicality, and realization in both names and types. A realized cohort event must not mutate an authored possibility or typicality value.
- Use deterministic pure functions for grammar and compatibility evaluation; random realization belongs to the population substrate and must consume streams only at its established boundary.
- Add tests before implementation for every new behavior. Commit each completed task with a message explaining why the task exists.
- Run `cargo fmt --check`, targeted tests, and the relevant `cargo clippy` checks after each stage; run the repository gate before campaign submission.

---

## Task 1: Establish the species-owned reproductive affordance contract

**Status:** Complete.

**Goal:** Add a narrow, body-plan-facing BIO-3 vocabulary that can express ordinary, fantastic, asexual, parasitic, manufactured, non-reproducing, and naturally transitioning bodies without exposing detailed anatomy to consumers.

**Files:**

- `domains/species/src/reproduction.rs` — new public types and pure validation/accessor logic.
- `domains/species/src/lib.rs` — declare and re-export the reproduction module without changing existing registry semantics.
- `domains/species/tests/suite.rs` — register the new integration-test module.
- `domains/species/tests/suite/reproduction.rs` — contract and coverage tests.

**Tests first:**

- [x] Add tests proving an affordance can represent no reproductive pathway without error.
- [x] Add tests covering `make`, `join`, `grow`, `support`, `release`, `copy`, `change`, `build`, and `convert` as declared operation capabilities, including a profile with more than one pathway.
- [x] Add tests proving development sites and support requirements are explicit and not inferred from a readable label such as “live-bearing.”
- [x] Add tests proving reproductive transition capability is independent from social gender or identity fields; no such fields are added to the BIO-3 type.
- [x] Add a coverage table for every declared enum state, following the existing species `coverage.rs` pattern, so unsupported branches are deliberate rather than silently untested.

**Implementation:**

- [x] Define `ReproductiveOperation`, `DevelopmentSite`, `SupportMode`, `ReproductiveRole`, `TransitionCapability`, and `ReproductiveAffordances` in `domains/species/src/reproduction.rs` with documented, closed enums and collection fields that preserve deterministic order.
- [x] Give `ReproductiveAffordances` a `validate()` method returning descriptive errors for contradictory combinations, while accepting a valid empty profile for non-reproducing or externally manufactured kinds.
- [x] Keep the public projection narrow: callers can ask what operations, sites, roles, support modes, and natural transitions are possible, but cannot inspect a future detailed body-plan representation.
- [x] Re-export only the contract types from `hornvale_species`; do not add reproductive fields to `BiosphereTraits` until the probe profiles establish the authored shape.

**Verification and commit:**

- [x] Run `cargo test -p hornvale-species --lib` and `cargo nextest run -p hornvale-species --test suite`.
- [x] Run `cargo fmt --check` and `cargo clippy -p hornvale-species --all-targets -- -D warnings`.
- [x] Commit as `feat(species): add BIO-3 reproductive affordance contract`.

## Task 2: Implement the composable reproductive grammar and compatibility relation

**Status:** Complete.

**Goal:** Turn affordances into a pure, inspectable grammar that distinguishes what is possible from how often it occurs, and evaluate hybrid outcomes as relations between profiles rather than species-name exceptions.

**Files:**

- `domains/species/src/reproduction.rs` — grammar result types and evaluator, unless the module becomes materially clearer when split into `domains/species/src/reproductive_grammar.rs`.
- `domains/species/src/lib.rs` — re-export the stable grammar and compatibility API.
- `domains/species/tests/suite/reproduction.rs` — behavior tests for grammar composition and compatibility.

**Tests first:**

- [x] Add a test that a pairborn profile composes `make + join + grow + support + release` into one possible pathway without a species-specific branch.
- [x] Add tests for budded/copy, broodweave/group support, host-convert, and built/manufactured profiles.
- [x] Add tests that sequential or metamorphic reproduction uses `change` without implying a fixed social gender.
- [x] Add compatibility tests for fertile, viable-but-sterile, one-way, assisted, magic-only, unstable, and impossible outcomes.
- [x] Add tests proving compatibility is symmetric only when the declared relation says so; parental direction and developmental assistance remain observable.
- [x] Add tests proving possibility and typicality are separate values and that no realized event is needed to construct a possibility profile.

**Implementation:**

- [x] Define `ReproductivePathway`, `ReproductiveProfile`, `CompatibilityOutcome`, `CompatibilityRelation`, and `CompatibilityContext` with explicit guards for role availability, developmental timing, environment, support, resource cost, and future assistance.
- [x] Implement a pure `possible_pathways(&ReproductiveAffordances, &CompatibilityContext) -> Vec<ReproductivePathway>` that composes operations and returns deterministic ordering.
- [x] Implement `compatibility(&ReproductiveAffordances, &ReproductiveAffordances, &CompatibilityContext) -> CompatibilityRelation`; do not key the decision on `KindId` or authored species names.
- [x] Represent future magical assistance as a capability/requirement in the relation, not as an implemented magic operation or a hidden success path.
- [x] Keep typicality as an input to later substrate realization rather than inventing rates in the grammar module.

**Verification and commit:**

- [x] Run the species unit and integration suite plus `cargo clippy -p hornvale-species --all-targets -- -D warnings`.
- [x] Review operation composition against §§3–7 of the design spec and confirm no social interpretation leaked into the biology API.
- [x] Commit as `feat(species): compose reproductive pathways and hybrid relations`.

## Task 3: Add probe profiles and a species-independent population handoff

**Status:** Complete.

**Goal:** Exercise the grammar with synthetic calibration profiles and define the plain-data contract that BIO-3 hands to the Murrain-compatible population substrate.

**Files:**

- `domains/species/tests/suite/reproduction.rs` — synthetic probe profiles and invariant tests.
- `domains/demography/src/reproductive.rs` — new kernel-only population input/output contract and pure summary functions.
- `domains/demography/src/lib.rs` — module declaration and re-exports.
- `domains/demography/tests/suite.rs` — new consolidated integration-test crate for the reproductive handoff.
- `domains/demography/tests/suite/reproductive.rs` — substrate contract tests.
- `domains/demography/Cargo.toml` — no species dependency; update only if the existing test layout requires a dev dependency.

**Tests first:**

- [x] Add named synthetic profiles for Pairborn, Turning, Broodweave, Budded, Forged, Guestborn, Crossing, and a non-reproducing control; keep them test fixtures, not canonical species.
- [x] Add tests that every probe reaches the intended grammar branch and that the negative control remains valid rather than being rejected as malformed.
- [x] Add tests for a substrate input carrying maturity age, generation length, offspring distribution, survival to independence, dependency duration, care burden, reproductive-role distribution, hybrid outcomes, and persistence inputs.
- [x] Add tests that the handoff contains no `KindId`, species registry, anatomy object, social-gender value, or projection-only identity.
- [x] Add tests showing the same BIO-3 input can be consumed by two different social contexts without changing the biological summary.
- [x] Add deterministic repeatability tests over identical plain inputs and distinct-seed realization tests only at the established population realization boundary.

**Implementation:**

- [x] Define a species-independent `ReproductivePopulationInput` in `domains/demography/src/reproductive.rs` using kernel value types or plain validated scalars, plus typed distributions for offspring, survival, care, and role availability.
- [x] Define `ReproductivePopulationSummary` and `SocialSubstrateInput` as the explicit BIO-3→population→SOC-2 handoff; keep relations and distributions separate from realized cohort records.
- [x] Implement pure validation and normalization that rejects non-finite/negative values with context and preserves zero-valued legitimate cases such as no reproduction.
- [x] Implement the minimal summary transform from a reproductive profile to the handoff shape without introducing a household lattice or person materialization.
- [x] Document which fields are possibility, typicality, and realization inputs, and where the future two-tier population realization will own draws and cohort state.

**Verification and commit:**

- [x] Run targeted species and demography tests, then `cargo clippy -p hornvale-demography --all-targets -- -D warnings`.
- [x] Run the architecture/layering tests if the workspace test harness checks new module dependencies.
- [x] Commit as `feat(demography): define BIO-3 population handoff`.

## Task 4: Wire the composition-root boundary without changing world behavior

**Status:** Complete.

**Goal:** Make worldgen able to resolve species affordances and hand plain reproductive inputs to demography while preserving all existing generated-world outputs until a later campaign opts into realization.

**Files:**

- `windows/worldgen/src/reproductive.rs` — composition-root adapter from species registry data to demography inputs.
- `windows/worldgen/src/lib.rs` — module declaration and public/internal export as appropriate.
- `windows/worldgen/tests/suite.rs` — register adapter tests.
- `windows/worldgen/tests/suite/reproductive.rs` — boundary and zero-drift tests.
- `windows/worldgen/Cargo.toml` — use existing species and demography dependencies; do not add a reverse domain dependency.

**Tests first:**

- [x] Add a boundary test proving worldgen is the only layer that reads `hornvale_species` and constructs the demography handoff.
- [x] Add tests for an authored ordinary profile, a synthetic hybrid relation, and a non-reproducing profile at the adapter boundary.
- [x] Add a zero-drift test proving the adapter is inert unless explicitly requested by a future population-bake option; current settlements, history, and existing artifacts remain unchanged.
- [x] Add a determinism test over repeated construction from the same registry and configuration.

**Implementation:**

- [x] Implement the adapter with explicit conversion functions and descriptive errors; do not make demography inspect species registries.
- [x] Thread the handoff through an additive worldgen data structure or optional build output, leaving the existing `DemographyReport` settlement path authoritative.
- [x] Add documentation at the boundary explaining that the substrate is the future authority and that projection windows must consume it rather than recompute biology.
- [x] Do not update population goldens or census fixtures in this stage; there is no realization behavior to calibrate yet.

**Verification and commit:**

- [x] Run targeted worldgen tests, `cargo fmt --check`, and `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`.
- [x] Run the relevant architecture and docs tests to catch layer or prose drift.
- [x] Commit as `feat(worldgen): expose additive reproductive substrate input`.

## Task 5: Freeze the probe panel and handoff documentation

**Status:** Complete locally; stage submission pending.

**Goal:** Make the new model measurable and leave SOC-2 with a stable, explicit contract for the successor campaign.

**Files:**

- `domains/species/tests/suite/reproduction.rs` — final probe matrix and invariant panel.
- `domains/demography/tests/suite/reproductive.rs` — handoff measurements and deterministic repeatability panel.
- `docs/superpowers/specs/2026-09-06-the-grammar-design.md` — update implementation status and record any resolved shape decisions.
- `docs/superpowers/ledgers/2026-09-06-the-grammar.md` — record final BIO-3 decisions and successor questions.
- `docs/superpowers/plans/2026-09-06-the-grammar.md` — mark completed stages.
- `docs/audits/campaign-reconciliation.tsv` — add or update only the campaign’s canonical artifact row if the repository’s reconciliation checks require it.

**Tests first:**

- [x] Add assertions for the preregistered measurements: birth intensity inputs, generation length, offspring distribution, survival to independence, dependency duration, care topology/burden, role distribution, hybrid outcome, persistence, and handoff stability.
- [x] Add a cross-seed test that confirms deterministic structural outputs while allowing future realization-specific variation to remain outside this stage.
- [x] Add a test that the probe panel does not silently become an existing-species canon or a social prejudice table.

**Implementation and documentation:**

- [x] Freeze the synthetic probe names, expected branches, and measurement labels in the test modules.
- [x] Document the SOC-2 successor input as reproductive roles, offspring pathway, dependency profile, care topology, descent relation, compatibility relation, and transition history, combined later with subsistence, property, mobility, pressure, authority, religion, and contact.
- [x] Record that the successor may implement `recognize`, `associate`, `bind`, `care`, `assign`, `inherit`, `adopt`, `exchange`, `exclude`, and `dissolve`, plus lifecycle states and historical transmission, but must not reinterpret BIO-3 possibility as social destiny.
- [x] Update the ledger with the exact public type names and any deliberate deviations from this plan.

**Final verification and campaign handoff:**

- [x] Run `cargo fmt --check`.
- [x] Run targeted package tests and the full local `make gate-commit` gate.
- [x] Run `make docs-tests` and `git diff --check`, resolving any real formatting issue while preserving required empty TSV fields.
- [x] Review the complete diff for accidental changes to The Murrain, existing species canon, magic, household behavior, or committed census artifacts.
- [x] Commit as `test(grammar): freeze BIO-3 probe and handoff panel`.
- [ ] Submit the branch at the stage boundary through the project’s sluice process; do not declare the campaign complete until the required stage/merge verification has run.

## Success Criteria

- [x] BIO-3 can express all approved probe families through shared operations and guards, with no species-pair exception table.
- [x] Possibility, typicality, and realization are distinct in the public API and tests.
- [x] Hybrid outcomes are typed relations with directional and assistance-sensitive behavior.
- [x] The population handoff is species-independent, deterministic, and compatible with the Murrain two-tier substrate/projection boundary.
- [x] Existing world generation remains zero-drift until a later campaign explicitly realizes the new substrate inputs.
- [x] SOC-2 has a documented, test-backed input contract but no premature household or institutional implementation.
