# The Staple D3B Implementation Plan

> For agentic workers: REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox syntax for tracking.

**Goal:** Implement the approved D3B Task 0 diagnostic that measures source-gradient occupancy and realized typed subsistence/exchange variation without changing save-facing history or assigning specialization roles.

**Architecture:** Extend the existing history bake with a read-only per-community D2 witness accumulated at the phase loop, and add pure D3B band/signature helpers in a focused worldgen module. Add an ignored fixed-roster integration probe that joins those witnesses to live occupations, evaluates the preregistered per-seed falsifier, and reports pooled totals descriptively. No production Function, portfolio, epoch, or settlement behavior changes.

**Tech Stack:** Rust workspace, hornvale-worldgen, consolidated nextest integration suite, existing quantize emit boundary, deterministic ordered vectors.

**Spec:** docs/superpowers/specs/2026-09-08-the-staple-d3b-design.md

## Global Constraints

- Count one live BakeOccupation at History::now against existing census(h).alive_at_now.
- Keep source support separate from realized projection; categorical labels never enter either measurement.
- Use existing source bars: surplus 0.4/0.6, river proximity 0/interior/1, and capacity 150/200.
- Use the phase-integrated typed A/B vector and separate per-community exchange-access companion.
- Keep raw vectors visible; structural signatures use only endpoint/order semantics.
- Quantize only at emit/equality boundaries; never in the compute path.
- Report empty, underpowered, missing, duplicate, zero-demand, disabled-treatment, conservation, and non-negativity branches explicitly.
- Verdicts are per adequately powered seed; pooled counts are descriptive only.
- The D3B sidecar must not alter save-facing emission, stream consumption, or existing exchange behavior.
- Do not run censuses locally; the fixed-roster probe remains ignored and uses the sanctioned sluice path.
- Every commit must pass the applicable repository gate; docs-only changes use the prose gate, Rust changes use make gate-commit.

---

### Task 1: Add the per-community D2 diagnostic witness

**Files:**
- Modify: windows/worldgen/src/history_bake.rs — public witness type, History sidecar, phase accumulation, final live-witness translation.
- Modify: windows/worldgen/src/lib.rs — re-export the witness type.
- Test: windows/worldgen/src/history_bake.rs unit tests and new integration assertions in Task 3.

**Interfaces:**
- Produces DiagnosticSubsistenceWitness with public community: BakeId, site: Vertex, phase_count: u32, coverage: [f64; 2], shortfall: [f64; 2], and per-resource status counters for attempts/proposed/accepted/settled/partial/refused/impossible.
- Produces History::diagnostic_subsistence: Vec<DiagnosticSubsistenceWitness>, containing only live communities at History::now, in deterministic BakeId order.
- Preserves exchange_census and all existing emitted records byte-for-byte.

- [ ] Step 1: Write failing unit tests for typed component ratios and witness invariants.
  Add tests beside the existing D2 helpers for full typed coverage, one-sided A/B shortfall, twelve-phase averaging, nonnegative counters, and unchanged conservation residuals.
- [ ] Step 2: Run the focused tests to verify failure.
  Run: cargo test -p hornvale-worldgen history_bake::tests --lib typed_subsistence
  Expected: FAIL because the component-ratio helper and witness fields do not exist.
- [ ] Step 3: Implement the minimal sidecar.
  Add the public witness type and History field. Refactor the existing max shortfall helper through a typed [f64; 2] ratio helper. Accumulate per-resource shortfall and phase count in exchange_phases. Extend existing exchange-clearing accounting to count statuses by requester/resource. At bake close, translate only live communities into ordered witnesses; leave save-facing History consumers untouched.
- [ ] Step 4: Run focused unit and existing D2 tests.
  Run cargo test -p hornvale-worldgen history_bake::tests --lib typed_subsistence and cargo test -p hornvale-worldgen --test suite staple_d2_probe.
  Expected: PASS, with existing D2 conservation and disabled-treatment behavior unchanged.
- [ ] Step 5: Commit.
  git add windows/worldgen/src/history_bake.rs windows/worldgen/src/lib.rs && git commit -m "feat(staple-d3b): expose typed subsistence diagnostic witness"

### Task 2: Add pure D3B source and projection signatures

**Files:**
- Create: windows/worldgen/src/d3b.rs — source bands, projection structural signatures, and pure branch-support helpers.
- Modify: windows/worldgen/src/lib.rs — declare and re-export the D3B module/types.
- Test: windows/worldgen/src/d3b.rs unit tests.

**Interfaces:**
- Produces D3bTernaryBand with Low, Middle, High.
- Produces D3bCapacityBand with BelowHamlet, HamletToLonghouse, AtLeastLonghouse.
- Produces D3bCoverageBand with None, Partial, Full.
- Produces D3bOrdering with ALessThanB, Equal, AGreaterThanB.
- Produces D3bSourceSignature and D3bProjectionSignature.
- Provides pure functions d3b_surplus_band, d3b_river_band, d3b_capacity_band, d3b_source_signature, and d3b_projection_signature.

- [ ] Step 1: Write failing unit tests.
  Pin exact boundary semantics: surplus <= 0.4, (0.4, 0.6], > 0.6; river 0, interior, 1; capacity <150, [150,200), >=200; coverage 0, interior, 1; A/B ordering and finite-value rejection.
- [ ] Step 2: Run the focused tests to verify failure.
  Run: cargo test -p hornvale-worldgen d3b --lib
  Expected: FAIL because the module and signatures are absent.
- [ ] Step 3: Implement pure helpers.
  Use existing constants and semantics at the helper boundary; do not introduce a new tolerance or quantize compute values. Reject non-finite inputs consistently with neighboring worldgen helpers. Derive signatures from continuous values, never Subsistence or Function labels.
- [ ] Step 4: Run focused tests and formatting.
  Run cargo test -p hornvale-worldgen d3b --lib and cargo fmt --check.
  Expected: PASS.
- [ ] Step 5: Commit.
  git add windows/worldgen/src/d3b.rs windows/worldgen/src/lib.rs && git commit -m "feat(staple-d3b): add gradient band signatures"

### Task 3: Add the fixed-roster D3B falsifier probe

**Files:**
- Create: windows/worldgen/tests/suite/staple_d3b_probe.rs — deterministic unit tests and ignored fixed-roster report.
- Modify: windows/worldgen/tests/suite.rs — register the new module.
- Modify: windows/worldgen/src/lib.rs — expose the same-run read-only `History`
  from the existing `ExchangeTreatmentBuild` boundary so the probe can consume
  enabled per-community witnesses without reconstructing or aggregating them.
- Modify: cli/tests/fixtures/world-build-sites.tsv — roster the probe's one
  real world build so the commit gate can account for its cost.
- Modify: docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md — record implementation rulings and probe contract as they occur.

**Interfaces:**
- Build each seed through existing build_world_with_exchange_treatment with ExchangeTreatment::Enabled.
- Join live BakeOccupation records to History::diagnostic_subsistence by BakeId, asserting total and unique joins.
- Compute source signatures from actual settlement-affecting site inputs available to the build, then apply the approved per-seed branch table.
- Keep fixed roster 1..=200, report empty and underpowered seeds, and do not write committed census artifacts locally.

- [ ] Step 1: Write deterministic fixture tests.
  Prove that labels alone cannot pass; one source axis or one singleton joint signature is insufficient; equal committed projection vectors are collapse; same structural signature with different committed raw vectors is measurement saturation; empty and one-unit seeds are underpowered; sidecar removal leaves emitted ledger bytes identical; repeated same-seed histories have identical D3B witnesses.
- [ ] Step 2: Run focused integration tests to verify failure.
  Run: cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d3b_probe)'
  Expected: FAIL until the probe module and Task 1/2 interfaces exist.
- [ ] Step 3: Implement the probe report and branch table.
  Add a report type carrying per-seed denominator, source-band occupancy, source signatures, projection signatures, raw-vector spread, missing/duplicate branches, and pooled descriptive totals. The ignored fixed-roster test prints the report but asserts only preregistered integrity and branch conditions; it must not silently convert mixed or underpowered results into success.
- [ ] Step 4: Run focused non-census integration tests and inspect output.
  Run focused non-ignored tests from Task 3. Do not run the ignored fixed-roster report locally. Verify sidecar absence from emitted ledger JSON and unchanged D2 behavior.
- [ ] Step 5: Commit.
  git add windows/worldgen/tests/suite/staple_d3b_probe.rs windows/worldgen/tests/suite.rs docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md && git commit -m "test(staple-d3b): add gradient sufficiency falsifier"

### Task 4: Cross-domain re-instantiation and repository verification

**Files:**
- Modify: docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md — record final implementation rulings and verification evidence.
- Modify: docs/audits/campaign-reconciliation.tsv only if the final artifact set changes.
- No new runtime files unless verification requires a scoped fix.

- [ ] Step 1: Run focused Rust tests and inspect the diff.
  Run cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d3b_probe) or test(staple_d2_probe)'.
- [ ] Step 2: Run the local commit gate.
  Run make gate-commit. Do not run a census.
- [ ] Step 3: Generate the capped review package and conduct broad review.
  Use scripts/review-package.sh docs/superpowers/plans/2026-09-08-the-staple-d3b.md origin/main HEAD; review all changed paths, with special attention to public History shape and stream/order preservation.
- [ ] Step 4: Record any finding as a ledger ruling before changing code; make one scoped fix cycle and rerun relevant focused tests and gate.
- [ ] Step 5: Stop at the campaign boundary. Do not run the sanctioned census, submit to the sluice, merge, or close the campaign in this plan. Those require later G6 process.

### Task 5: Preserve enabled zero-phase live witnesses

**Files:**
- Modify: windows/worldgen/src/history_bake.rs — retain enabled live
  communities whose accumulator has zero phases as explicit incomplete
  witnesses; preserve disabled-sidecar emptiness.
- Modify: docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md — record the
  post-run root-cause ruling and verification.

The fixed-roster run showed that live `BakeOccupation` units can have no
observed exchange/consumption phase. They remain in the existing
`alive_at_now` denominator, so omitting their witness creates a false join
failure. A zero-phase witness is an explicit incomplete observation: its
coverage/shortfall payload is a non-evidentiary zero sentinel and the probe
must branch on `phase_count == 0` before consuming it. Disabled treatment
continues to produce no sidecar witnesses.

- [ ] Step 1: Add a failing unit test for an enabled live zero-phase witness.
- [ ] Step 2: Implement the smallest translation/witness correction.
- [ ] Step 3: Run focused history/D3B tests and the commit gate.
- [ ] Step 4: Rerun the local ignored fixed-roster report and inspect the
  per-seed verdicts; do not run a census.

### Task 6: Classify explicit incomplete observations

**Files:**
- Modify: windows/worldgen/tests/suite/staple_d3b_probe.rs — distinguish
  expected zero-phase incompleteness from malformed measurements, and keep the
  fixed-roster report non-panicking and scientifically conservative.
- Modify: docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md — record the
  post-fix report ruling.

The post-Task-5 report confirms that missing joins are gone, but the probe's
final assertion still treats its explicit `phase_incomplete_units` branch as
fatal `InvalidMeasurement`. Give that branch its own non-clearing verdict;
the roster may remain mixed/underpowered, but it must report rather than
panic. Other malformed measurements remain fatal integrity failures.

- [x] Step 1: Add a failing reducer test for the distinct incomplete verdict.
- [x] Step 2: Implement the smallest verdict/assertion correction.
- [x] Step 3: Run focused non-ignored tests and the commit gate.
- [x] Step 4: Rerun the local ignored fixed-roster report and inspect the
  per-seed verdict distribution; do not run a census.
