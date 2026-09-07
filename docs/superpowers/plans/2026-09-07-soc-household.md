# SOC-household Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement a deterministic, test-backed cohort-to-person social substrate whose temporal relations and derived group projections let The Lot observe sex/gender distinctions, kinship, care, association, lifecycle, migration, recomposition, and parental death without making one family form universal.

**Architecture:** `hornvale-demography` owns species-independent aggregate social inputs and pure transition summaries. `hornvale-person` owns realized person predicates, while `hornvale-history` owns append-only relation events and their provenance. `windows/worldgen` is the only composition root: it realizes synthetic cohort transitions, emits facts, and derives group/household projections. `windows/lot` consumes committed facts and preserves silence when a slot has no source.

**Tech Stack:** Rust workspace, kernel value types, deterministic ordered `Vec` storage, existing history streams and fact predicates, consolidated nextest suites, and synthetic test-only probe societies. No new external dependencies.

**Spec:** `docs/superpowers/specs/2026-09-07-soc-household-design.md`

## Global Constraints

- Preserve the two-tier population boundary: aggregate cohorts are causally authoritative; realized persons and groups are projections and never rewrite cohort truth.
- Keep sex traits, body plan, reproductive role, social gender, personal identity, and transition history distinct in names, storage, tests, and Lot output.
- Keep possibility, typicality, realization, and institutional recognition distinct; no statistical aggregate may be emitted as an individual biography without a realization step.
- Keep domains kernel-only and avoid sibling-domain dependencies; `windows/worldgen` translates between demography, person, and history contracts.
- Use ordered vectors and deterministic iteration; do not introduce `HashMap`/`HashSet`, wall-clock time, or random draws outside the established worldgen stream boundary.
- Do not add a universal household type, existing-species household canon, detailed anatomy, full economy, full institutions, or magical sex/reproductive-state transitions.
- Preserve history: separation, dissolution, migration, and death close future participation but do not delete prior relation events.
- Every probe must assert path exercise and anti-vacuity; an empty result is not evidence that a capability works.
- Write behavioral tests before implementation and commit each task independently after its targeted tests and lint pass.
- Run `cargo fmt --check`, targeted nextest, and the relevant clippy checks after each task; run `make gate-commit` before queue submission.

---

### Task 1: Add the aggregate social substrate contract

**Goal:** Define the species-independent cohort inputs and pure validation/summary operations that consume the landed SOC-2 handoff without materializing people or household objects.

**Files:**

- Create: `domains/demography/src/social.rs` — aggregate lifecycle, association, descent, care, migration, and inheritance distributions plus validation and summary functions.
- Modify: `domains/demography/src/lib.rs` — declare and re-export the stable social contract.
- Modify: `domains/demography/tests/suite.rs` — register `social`.
- Create: `domains/demography/tests/suite/social.rs` — red/green contract tests.

**Interfaces:**

- Consumes: `SocialSubstrateInput` and reproductive summaries from `hornvale-demography::reproductive`.
- Produces: `SocialCohortInput`, `SocialCohortSummary`, `LifecycleTransition`, `AssociationDistribution`, `DescentDistribution`, `CareTopology`, `MigrationDistribution`, and `InheritanceDistribution`; pure `validate_social_cohort` and `summarize_social_cohort` functions.

**Steps:**

- [x] Write failing tests for zero-reproduction, zero-care, non-pair association, overlapping care, migration, dissolution, and parental-death distributions; assert legitimate zeroes are accepted.
- [x] Write failing tests proving invalid negative/non-finite values identify the field and reject without partial normalization.
- [x] Write failing tests proving `SocialCohortSummary` carries possibility/typicality inputs but no `EntityId`, person identity, gender identity, or household object.
- [x] Implement ordered, species-independent value types and validation in `social.rs`; keep all scalar units explicit in names and doc comments.
- [x] Implement `summarize_social_cohort` as a pure transform that preserves care topology and does not flatten it into only a care-burden scalar.
- [x] Run `cargo nextest run -p hornvale-demography --test suite -E 'test(social::)'` and `cargo clippy -p hornvale-demography --all-targets -- -D warnings`.
- [x] Commit as `feat(demography): define aggregate social substrate`.

### Task 2: Add realized person axes and append-only social events

**Goal:** Give the person and history domains minimal, provenance-bearing contracts for realized identities and relation events without importing demography or cultural interpretation into either domain.

**Files:**

- Modify: `domains/person/src/lib.rs` — add predicates and typed `PersonSocialSeed`/`PersonSocialFact` helpers for sex traits, reproductive role references, social gender claims, personal identity claims, and transition history.
- Modify: `domains/person/Cargo.toml` — retain kernel-only dependencies.
- Modify: `domains/person/tests/suite.rs` — register `social` tests.
- Create: `domains/person/tests/suite/social.rs` — predicate direction, interval, provenance, and axis-separation tests.
- Create: `domains/history/src/social.rs` — ordered relation/event vocabulary, event validation, and fact conversion helpers.
- Modify: `domains/history/src/lib.rs` — declare and re-export the event contract.
- Modify: `domains/history/tests/suite.rs` — register `social` tests.
- Create: `domains/history/tests/suite/social.rs` — event lifecycle and non-deletion tests.

**Interfaces:**

- Consumes: kernel `EntityId`, `WorldTime`, `Fact`, `Value`, and ordered event data supplied by worldgen.
- Produces: person predicates for `sex-trait`, `reproductive-role`, `gender-identity`, `gender-recognition`, `transitioned`, and `person-social-provenance`; history types `RelationKind`, `RelationEvent`, `AssociationForm`, `LifecycleEvent`, `GroupMembershipEvent`, and `SocialEvent::fact`.

**Steps:**

- [x] Write failing person tests showing sex traits, reproductive role, gender identity, social recognition, and transition history can coexist without one deriving another.
- [x] Write failing history tests for `origin`, `descent`, `care`, `dependency`, `association`, `residence`, `custody`, `transfer`, and `recognition` with explicit direction and time intervals.
- [x] Write failing tests proving `separate`, `dissolve`, and `die` close future activity while prior facts remain queryable.
- [x] Implement the smallest predicate/event set with descriptive validation for reversed participants, empty intervals, missing provenance, and illegal self-relations where the event requires distinct participants.
- [x] Keep cultural labels out of the foundational event types; store association form and interpretation as explicit values rather than treating `marriage` as the only association.
- [x] Run targeted person/history suites and both crate clippy checks.
- [x] Commit as `feat(social): add realized person and relation events`.

### Task 3: Realize synthetic cohorts and emit deterministic history

**Goal:** Add a worldgen composition-root projection that turns aggregate cohort distributions into a bounded, deterministic set of realized persons and social events for synthetic probe societies only.

**Files:**

- Create: `windows/worldgen/src/social_projection.rs` — realization inputs, stream labels, ordered person/event emission, and lifecycle transition application.
- Modify: `windows/worldgen/src/lib.rs` — expose the projection entry point without changing existing worldgen defaults.
- Modify: `windows/worldgen/src/history_bake.rs` or the existing history composition hook identified by the implementer — invoke the projection only at the social realization boundary and preserve existing stream consumption order outside it.
- Modify: `windows/worldgen/src/history_emit.rs` — emit person/social predicates and relation events through the existing ledger path.
- Modify: `windows/worldgen/tests/suite.rs` — register `social_projection`.
- Create: `windows/worldgen/tests/suite/social_projection.rs` — synthetic society tests, byte identity, lifecycle, and anti-vacuity witnesses.

**Interfaces:**

- Consumes: `SocialCohortSummary`, a seed, a labeled realization stream, and a test-only `SyntheticSociety` configuration.
- Produces: `SocialProjection { persons, events, groups }`, with deterministic `PersonId`/`EntityId` assignment and no mutation of aggregate inputs.

**Steps:**

- [x] Write failing tests for the six synthetic societies in the design: independent-origin, dual-descent, care-cluster, recomposing mobility, institutional-recognition, and lifecycle-transition.
- [x] Add anti-vacuity counters proving each test reaches its intended formation, care, descent, migration, dissolution, recognition, or transition path.
- [x] Add byte-identity tests for identical `(seed, pins, synthetic society)` and a control showing distinct seeds change realized events only at this projection boundary.
- [x] Implement ordered person realization and event emission using one named stream with documented consumption order; do not use `HashMap`/`HashSet` or wall-clock values.
- [x] Implement lifecycle operations so parental death leaves prior descent/care facts readable while changing dependency and future-care projections.
- [x] Add a test proving no synthetic society changes existing authored species registries or current default world bytes when the projection is disabled.
- [x] Run targeted worldgen tests, `cargo fmt --check`, and `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`.
- [x] Commit as `feat(worldgen): realize synthetic social cohorts`.

### Task 4: Derive kinship, care, inheritance, and group projections

**Goal:** Build pure projections over realized events that answer kinship and household questions without making a household container foundational.

**Files:**

- Create: `domains/demography/src/kinship.rs` — descent closure, sibling derivation, parent/child readings, adoption/care distinctions, and bounded traversal validation.
- Create: `domains/demography/src/groups.rs` — group-basis vocabulary and deterministic membership projection over residence, care, subsistence, property, ritual, or institutional recognition.
- Modify: `domains/demography/src/lib.rs` — re-export projection contracts.
- Modify: `domains/demography/tests/suite.rs` — register `kinship` and `groups`.
- Create: `domains/demography/tests/suite/kinship.rs` — descent, sibling, adoption, and death tests.
- Create: `domains/demography/tests/suite/groups.rs` — overlapping groups, dissolution, migration, and non-household associations.
- Create: `windows/worldgen/tests/suite/social_readout.rs` — worldgen-to-projection integration tests.

**Interfaces:**

- Consumes: ordered `RelationEvent` values and external `SocialContext { subsistence, property, mobility, authority, religion, contact, population_pressure }`.
- Produces: `KinshipRelation`, `CareProjection`, `GroupProjection`, `InheritanceClaim`, and pure `derive_kinship`, `derive_care`, `derive_groups`, and `derive_inheritance` results with provenance.

**Steps:**

- [ ] Write failing tests showing siblings are derived from shared descent context rather than stored as an independent universal edge.
- [ ] Write failing tests distinguishing origin/descent, adoption, care, custody, and institutional recognition; no one relation may satisfy all five meanings.
- [ ] Write failing tests for overlapping care groups, migration between groups, association separation, group dissolution, and inheritance after death.
- [ ] Write failing tests proving the same realized relations produce different recognized labels under different external social contexts without changing the underlying events.
- [ ] Implement deterministic projections with explicit traversal bounds and stable ordering; preserve event provenance on every derived relation.
- [ ] Ensure groups are time-bounded projections with declared bases; no `Household` constructor may require marriage, parents, children, or co-residence.
- [ ] Run targeted demography/worldgen tests and the architecture/layering checks.
- [ ] Commit as `feat(social): derive kinship care and group projections`.

### Task 5: Expose sourced social observations through The Lot

**Goal:** Let The Lot observe realized social facts and preserve honest silence when the ledger cannot answer a question.

**Files:**

- Modify: `windows/lot/src/slots.rs` — add social slot resolution for sex traits, reproductive role, gender identity/recognition, associations, children, siblings, descent, adoption, care, group membership, migration, parental death, and inheritance.
- Modify: `windows/lot/src/json.rs` — serialize social observations with source facts and explicit silence reasons.
- Modify: `windows/lot/src/narrate.rs` — render sourced social clauses without inferring marriage, gender, or parenthood from proxy facts.
- Modify: `windows/lot/tests/suite.rs` — register `social` tests.
- Create: `windows/lot/tests/suite/social.rs` — positive, negative, provenance, and silence tests.
- Modify: `windows/worldgen/tests/suite/lot_probe.rs` — add synthetic-projection Lot integration coverage without changing existing species canon.
- Modify: `docs/audits/campaign-reconciliation.tsv` only if new campaign records are created during execution.

**Interfaces:**

- Consumes: committed person predicates, relation events, derived group projections, and the existing Lot draw context.
- Produces: sourced Lot slot values or `SilenceReason::NoCommittedSource`; no inferred fallback values.

**Steps:**

- [ ] Write failing tests for a synthetic Lot whose sex traits and reproductive role are present but whose gender identity is silent.
- [ ] Write failing tests for recognized and unrecognized associations, non-residential parentage, siblings, adoption, care, migration, recomposition, and parental death.
- [ ] Write failing tests proving co-residence does not produce marriage, care does not produce parentage, and reproductive role does not produce gender.
- [ ] Write failing JSON/prose tests that every filled clause cites source facts and every unfilled clause carries a reason rather than an invented value.
- [ ] Implement slot readers as pure ledger reads or derived reads over committed facts; keep The Lot a consumer and never write social facts from the draw.
- [ ] Add the synthetic Lot probe and report its filled/silent slot counts without assigning canon to existing peoples.
- [ ] Run `cargo nextest run -p hornvale-lot --test suite`, targeted worldgen Lot tests, and the Lot client/local check if the payload schema changes.
- [ ] Commit as `feat(lot): observe sourced social biographies`.

## Final verification and handoff

- [ ] Run `cargo fmt --check`.
- [ ] Run targeted nextest for demography, person, history, worldgen, and Lot suites.
- [ ] Run `cargo clippy --workspace --all-targets -- -D warnings`.
- [ ] Run `make gate-commit` and review its complete output.
- [ ] Review the diff for accidental existing-species canon, biological/social conflation, unordered collections, and unstated generated-artifact claims.
- [ ] Submit a stage gate through the Sluice with the full campaign SHA; do not run censuses locally or merge manually.
