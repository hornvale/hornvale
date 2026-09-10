# Underworld Peoples Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add mountain dwarf, duergar, svirfneblin, and kuo-toa as measured, deterministic Underworld participants without using surface elevation as underground depth.

**Architecture:** Keep authored species data in `domains/species`, chamber/environment facts in their existing domain and worldgen projections, and use `windows/worldgen/src/delve_seating.rs` as the composition point for per-chamber seating. The first task measures the live substrate and controls admission; later tasks only author niches whose distinguishing signals are consumed by the existing seating path.

**Tech Stack:** Rust workspace, cargo-nextest, existing `ComponentStore` registries, deterministic worldgen, committed prose and artifact checks.

**Spec:** `docs/superpowers/specs/2026-09-08-underworld-peoples-design.md`

## Global Constraints

- Preserve deterministic stream labels and consumption order; prefer pure projections over new draws.
- Never encode underground depth as metres above sea level.
- Keep aquatic habitat medium distinct from subterranean realm and delve depth.
- Keep dwarf-family traits shared; do not duplicate family behavior per kind.
- Do not add a biome taxonomy or redesign settlement/coexistence allocation.
- Run tests before claiming a task complete; do not accept generated drift without reviewing the diff.

## File map

- `domains/species/src/lib.rs`: authored kind rows, condition niches, realm, locomotion, family, and existing total registries.
- `domains/species/tests/suite/coverage.rs`: explicit coverage ratchets for registry dimensions.
- `windows/worldgen/src/delve_seating.rs`: chamber-fit and seating behavior for environment niches.
- `windows/worldgen/tests/suite/`: measurement probes and regression tests for Underworld seating and water-medium behavior.
- `domains/climate/src/underworld.rs` and `domains/climate/tests/suite/underworld.rs`: only if a consumed environment-corpus row is required by measurement.
- `cli/src/proto.rs` and language-related registry files: deterministic human-readable family output and proto coverage.
- `docs/audits/campaign-reconciliation.tsv`: campaign record reconciliation; the ledger remains outside its five record columns.

### Stage 1: Measure the admission substrate
**Goal**: Establish which existing chamber facts can distinguish the four requested peoples.
**Success Criteria**: A committed probe or test records chamber reach, seating variability, water-bearing coverage, and dwarf overlap; its result selects the authoring branch without invented thresholds.
**Tests**: Existing Underworld probes plus a new targeted worldgen probe, run on the project’s established seed set.
**Status**: Complete

### Task 1: Add the admission measurement

**Files:**
- Create or modify: `windows/worldgen/tests/suite/underworld_peoples_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs`
- Test reference: `windows/worldgen/tests/suite/underworld_conditions_probe.rs`, `windows/worldgen/tests/suite/deep_realm_substrate.rs`, `windows/worldgen/src/delve_seating.rs`

**Interfaces:**
- Consumes: `World`, `made_chambers`, chamber condition helpers, `chamber_fit`, and `seat_at`.
- Produces: a deterministic report/test surface for reachable delve rungs, per-kind candidate fit, water-bearing chamber count, and top-quartile overlap.

- [x] Read the existing probe helpers and copy their seed enumeration and chamber traversal shape into the new probe.
- [x] Write failing assertions for the required observations: more than one reachable seating value where the current design requires it, water-bearing chambers being counted separately, and overlap being computed from the same chamber population for both dwarf candidates.
- [x] Run the targeted probe and capture its actual output; if a criterion is false, record the null in the campaign ledger and stop the affected admission path.
- [x] Keep only measurements that are consumed by a later authoring decision; remove redundant report columns.
- [x] Run `cargo nextest run -p hornvale-worldgen --test suite -E 'test(underworld_peoples_probe)'` and commit the measurement with a message explaining the admission result.

### Stage 2: Author the new species and registry projections
**Goal**: Add only the kinds admitted by Stage 1, with complete registry consistency.
**Success Criteria**: Every admitted kind resolves across required registries; rejected kinds remain absent and have a recorded null; dwarf and aquatic distinctions are represented by the correct existing registries.
**Tests**: Species coverage, registry total-map checks, and targeted component tests.
**Status**: Complete

### Task 2: Add biosphere and environment niches

**Files:**
- Modify: `domains/species/src/lib.rs` near the existing dwarf, drow, and environment-niche functions and registries.
- Modify: `domains/species/tests/suite/coverage.rs`.
- Test: `windows/worldgen/tests/suite/radiation_admission.rs` patterns for authored-vs-consumed niche checks.

**Interfaces:**
- Consumes: `EnvironmentNiche`, `ConditionResponse`, `BiosphereTraits`, `HabitatRealm`, `Locomotion`, and `family_of`.
- Produces: `KindId` rows for admitted names, niche functions consumed by `environment_niche_registry`, and registry rows used by worldgen.

- [x] Add coverage tests first for the admitted roster, dwarf-family membership, subterranean realm rows, and kuo-toa’s water-capability row.
- [x] Add the minimal authored rows to `biosphere_registry`, `environment_niche_registry`, `habitat_realm_registry`, and `locomotion_registry`, following the existing sparse/total registry conventions.
- [x] Add mountain dwarf and duergar to the dwarf family map; leave svirfneblin and kuo-toa outside that family.
- [x] Author each niche from measured chamber fields. Do not add a preference for an axis that the seating path does not consume.
- [x] Run species coverage and targeted worldgen compilation; inspect every changed registry list for ordering and completeness.
- [x] Commit the species-domain change separately from worldgen seating.

### Task 3: Extend language and cross-registry coverage

**Files:**
- Modify: `domains/language/src/lib.rs:872` and nearby dwarf-family rows.
- Modify: `cli/src/proto.rs` only where the existing rendered-family roster requires the new dwarf family members.
- Modify: `domains/species/tests/suite/coverage.rs` and relevant language tests.

**Interfaces:**
- Consumes: the new `KindId` roster and existing dwarf-family proto contract.
- Produces: stable family/proto output for the expanded dwarf family and explicit coverage for names that remain singleton families.

- [x] Write a failing test that the dwarf family’s member set includes the surviving and admitted dwarf kinds and that non-dwarf kinds do not enter it.
- [x] Extend the family/proto data in the same order and representation used by the existing dwarf family.
- [x] Update the CLI rendered-family list only if the current output contract requires the expanded family to appear; preserve deterministic ordering.
- [x] Run the targeted language/proto tests and compare rendered output before accepting any fixture change.
- [x] Commit the language and coverage changes.

### Stage 3: Wire and verify chamber seating
**Goal**: Make admitted kinds participate in real chamber seating with the intended terrestrial/aquatic distinctions.
**Success Criteria**: New kinds seat only on reachable Underworld rungs; mountain dwarf, duergar, and svirfneblin retain intended overlap/difference; kuo-toa’s fit changes with water state and not darkness alone; surface behavior is unchanged.
**Tests**: `delve_seating` unit tests, worldgen Underworld integration tests, determinism tests.
**Status**: Complete

### Task 4: Add seating behavior tests before implementation changes

**Files:**
- Modify: `windows/worldgen/src/delve_seating.rs` tests if the existing module owns the unit seam.
- Create or modify: `windows/worldgen/tests/suite/underworld_peoples.rs`.

**Interfaces:**
- Consumes: `chamber_fit`, `seat_at`, `seating_for`, the species niche registry, and chamber condition fixtures.
- Produces: failing behavioral tests that distinguish reachable-rung truncation, chamber water state, overlap, and darkness controls.

- [x] Add a test that every admitted terrestrial seat is one of the chamber’s reachable rungs.
- [x] Add a test that a chamber with water and its dry counterpart can differ for kuo-toa while holding light constant.
- [x] Add a darkness-control test proving kuo-toa is not admitted solely by `insolation == 0.0`.
- [x] Add a dwarf-pair comparison over the measured chamber population, asserting the recorded overlap and distinction properties rather than hardcoding an invented modal rung.
- [x] Run the targeted tests and confirm they fail for the absent roster or missing seating behavior.

### Task 5: Implement minimal seating integration

**Files:**
- Modify: `windows/worldgen/src/delve_seating.rs` only where the current admitted-kind iteration or chamber condition projection excludes the new registry rows.
- Modify: `windows/worldgen/src/components.rs` only if component validation needs the new registry relationships.
- Test: `windows/worldgen/tests/suite/underworld_peoples.rs`.

**Interfaces:**
- Consumes: the Stage 2 registries and existing chamber condition values.
- Produces: `seating_for` results containing the admitted kinds without new nondeterministic draws or surface-slot changes.

- [x] Implement the smallest change that makes the Stage 4 tests pass, preserving existing `Band` names and stream inputs.
- [x] Run the targeted worldgen tests once, inspect all failures, and fix only failures caused by this roster.
- [x] Run `cargo fmt --check` and `cargo clippy -p hornvale-worldgen --all-targets -- -D warnings`.
- [x] Commit the seating integration with its tests.

### Stage 4: Artifacts, documentation, and final verification
**Goal**: Reconcile all user-visible rosters and prove deterministic integration.
**Success Criteria**: Workspace gates pass, generated output is reviewed, no stale campaign documentation remains, and the branch is ready for the canonical stage gate.
**Tests**: targeted tests, `make quick`, `make gate-commit`, and the repository’s queued stage gate when submitted.
**Status**: Complete

### Task 6: Update artifacts and documentation from live output

**Files:**
- Modify: generated roster/proto artifacts identified by the repository’s generated-path checks.
- Modify: Underworld/species reference or chronicle pages only when the implementation changes their stated roster.
- Modify: `docs/audits/campaign-reconciliation.tsv` only for additional campaign records created during execution.

**Interfaces:**
- Consumes: live registry output and deterministic worldgen output.
- Produces: reviewed generated artifacts and documentation consistent with the accepted admission result.

- [x] Run the project’s artifact discovery/check command before regeneration so the author for each path is known.
- [x] Regenerate only artifacts whose live source changed; inspect the complete diff for moved bytes, roster ordering, and stale counts.
- [x] Update prose tests or committed fixtures when their subject is intentionally changed; do not hand-edit generated output.
- [x] Run the targeted species/worldgen suites and the repository gates.
- [x] Commit artifacts and documentation with the source changes.

### Task 7: Final local gate and handoff

**Files:**
- No source changes unless a verification failure identifies a scoped defect.
- Modify: `IMPLEMENTATION_PLAN.md` and this plan’s status checkboxes as tasks complete.

- [x] Run `make quick` and record the exact result.
- [x] Run `make gate-commit` and record the exact result; do not use `--no-verify`.
- [x] Review `git diff main...HEAD`, working-tree status, and commit history.
- [x] Submit the full branch SHA to the repository’s stage queue according to the Sluice workflow; do not claim merge readiness before the queued result is green.
