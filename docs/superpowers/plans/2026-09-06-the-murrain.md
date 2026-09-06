# The Murrain Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add disease to Hornvale as a species-backed statistical population phenomenon, with baked epidemics, derived endemic burden, and salience-aware Lot projections.

**Architecture:** The worldgen/history substrate remains the causal authority for population, connected host populations, and epidemic facts. A kernel-only epidemiology crate owns pure persistence and outbreak rules; `windows/worldgen` wires those rules into the bake; `windows/lot` consumes substrate and committed facts to materialize aggregate, composite, and individual projections without defining epidemiological population. The implementation preserves the existing deterministic stream and advances the history epoch from v3 to v4.

**Tech Stack:** Rust 2024 workspace, `cargo nextest`, existing ledger predicates and component registries, `libm` through kernel math, deterministic `Stream`, existing Lot JSON/client ABI, laboratory census metrics.

**Spec:** `docs/superpowers/specs/2026-09-06-the-murrain-design.md`

## Global Constraints

- Pathogen kinds are authored in `domains/species`; pure epidemiology rules depend on `kernel` only.
- The population substrate, not `windows/lot::shape::population_at`, is authoritative for disease persistence and spread.
- The endemic burden is derived and does not change Siler mortality magnitude or `GROWTH_RATE`.
- Epidemic draws use the existing sequential history stream in commit order and bump `history/bake/v3` to `history/bake/v4`.
- No wall clock, `HashMap`, or platform-dependent transcendental math; quantize only at ledger emission.
- New facts are the paired dated predicates `struck-by` and `outbreak-deaths`, emitted together.
- The catalogue has exactly five pathogen kinds; the count assertion and the H-M tests are load-bearing.
- Composite projections may be analytical or in-world, but they are non-causal; only aggregate state and materialized individuals write persistent consequences.
- Every task ends with focused tests and a commit; no `--no-verify` and no disabled tests.

## File Map

- `domains/species/src/lib.rs` — `PathogenClass`, `PathogenTraits`, five-row pathogen registry, concept registration, catalogue count assertion.
- `domains/epidemiology/Cargo.toml`, `domains/epidemiology/src/lib.rs` — kernel-only pure rules: CCS, persistence, wave reach, outbreak arithmetic, attribution, and predicate registration.
- `domains/epidemiology/tests/suite.rs` and submodules — H-M1, H-M2, H-M6 and pure-rule tests.
- `windows/worldgen/src/lib.rs` — authoritative population-substrate accessors and era graph access already used by Task 0.
- `windows/worldgen/src/plague_bake.rs` — epidemic phase kept separate from the large history bake.
- `windows/worldgen/src/history_bake.rs` — call-site for `plague_phase` and stream label v4.
- `windows/worldgen/src/history_emit.rs` — paired fact emission and round-trip assertions.
- `windows/worldgen/tests/suite/murrain_probe.rs` — retain Task 0 as the preregistered substrate measurement; add the post-implementation readout only where the spec requires it.
- `windows/lot/src/endemic.rs` — derived endemic burden over the substrate and committed facts.
- `windows/lot/src/draw.rs`, `hazard.rs`, `slots.rs`, `json.rs`, `narrate.rs`, `lib.rs` — causes, outbreak endings, slot sources, and additive payload changes.
- `windows/lot/tests/suite/{shape,draw,hazard,slots,lot_readout}.rs` — population/materialization, attribution, byte-identity, and readout tests.
- `windows/lab/src/metrics.rs`, `domesday/*`, and the lab suite — six epidemic/Lot metrics and census columns.
- `book/src/laboratory/generated/`, `book/src/domesday/history.md`, `book/src/chronicle/the-murrain.md` — generated and authored closure artifacts.
- `docs/superpowers/ledgers/2026-09-06-the-murrain.md` — task decisions, measurements, H-P results, and post-G3 follow-ups.

## Task 1: Population Substrate and Pure Epidemiology (Stage 1)

**Goal:** Establish the authoritative population view and the kernel-only disease rules before wiring any epidemic into history.

**Success Criteria:** `domains/epidemiology` compiles with no non-kernel dependency; the five pathogen rows are registered and counted; H-M1, H-M2, and H-M6 pass; disease persistence can consume a substrate population without importing Lot.

**Tests:** Pure rule tests for CCS bands/monotonicity, persistence at `2 × CCS` and `CCS / 2`, exact outbreak arithmetic, wave reach boundaries, and five-kind catalogue count.

- [ ] **Step 1: Write failing pure-rule tests** in `domains/epidemiology/tests/suite.rs` for the CCS anchor-derived band, monotonicity, persistence threshold, wave reach, outbreak deaths, and plague threshold.
- [ ] **Step 2: Run the focused test target** with `cargo nextest run -p hornvale-epidemiology`; confirm the crate/tests are absent or fail for the missing interfaces.
- [ ] **Step 3: Create the kernel-only crate** with `critical_community_size(r0: f64, infectious_years: f64, births_per_person_year: f64) -> f64`, `persists(population: f64, ccs: f64) -> bool`, `wave_reach(origin: u32, occupied_neighbors: &[(u32, &[u32])], radius: u32) -> Vec<u32>`, and `outbreak(population: f64, susceptible: f64, attack: f64, fatality: f64, plague_fraction: f64) -> OutbreakOutcome`.
- [ ] **Step 4: Add pathogen data** to `domains/species/src/lib.rs`: `PathogenClass`, `PathogenTraits`, the five frozen rows, host weights, niche data, virulence data, concept registration, and `pathogen_registry()` count assertion.
- [ ] **Step 5: Add the authoritative substrate seam** in `windows/worldgen/src/lib.rs` as a read-only era/site population view consumed by windows and passed as plain numbers to epidemiology; do not import `hornvale_lot` into worldgen.
- [ ] **Step 6: Run Stage 1 tests and checks**: `cargo fmt --check`, `cargo nextest run -p hornvale-epidemiology -p hornvale-species`, and the relevant worldgen suite.
- [ ] **Step 7: Commit** with `feat(the-murrain): add pathogen catalogue and epidemiology rules`.

## Task 2: Baked Epidemic and History Facts (Stage 2)

**Goal:** Add the deterministic epidemic phase to the history bake, including substrate population updates, relocation behavior, Plague endings, and paired facts.

**Success Criteria:** The bake uses `history/bake/v4`; each epidemic draw is consumed in snapshot/catalogue order; `struck-by` and `outbreak-deaths` round-trip together; H-M3 and H-M5 pass; old v3 artifacts are rejected or regenerated by the normal epoch machinery.

**Tests:** History phase ordering, graph-radius/component boundaries, exact population reduction, Plague at `φ` but not `φ − ε`, paired-fact emission, same-seed byte identity, and existing relocation/raid behavior after an epidemic.

- [ ] **Step 1: Add failing bake tests** in `windows/worldgen/tests/suite/history_bake.rs` and `history_emit.rs` for stream version, phase ordering, paired facts, wave boundaries, and Plague endings.
- [ ] **Step 2: Implement `windows/worldgen/src/plague_bake.rs`** with deterministic spillover, wave traversal, susceptible calculation, outbreak arithmetic, persistence/newborn attack, survivor relocation through existing paths, and snapshot/catalogue ordering.
- [ ] **Step 3: Register and emit `struck-by` and `outbreak-deaths`** through the epidemiology-owned predicate registry and `history_emit`; assert one paired event per `(occupation, day)`.
- [ ] **Step 4: Wire the phase into `history_bake.rs`** after community growth and before raids, and change only the bake stream label from v3 to v4 as specified.
- [ ] **Step 5: Run focused worldgen/history tests** and inspect the first deterministic diff; resolve only expected epoch movement through the declared artifact path.
- [ ] **Step 6: Rebaseline declared goldens and fixtures** using the repository's artifact commands, read the diff, and commit the source/epoch change separately from generated artifacts.
- [ ] **Step 7: Commit** with `feat(the-murrain): bake epidemic events and plague endings`.

## Task 3: Endemic Read and Lot Projection (Stage 3)

**Goal:** Make disease visible in the Lot without making the Lot the population authority, while adding composite/individual projection semantics and named causes.

**Success Criteria:** Endemic burden is a pure read over substrate and committed facts; Siler survival and `e₀` remain byte-identical; every dead Lot receives a cause; outbreak and Plague endings name their pathogen; JSON/client payload changes are additive; composite cases cannot write world state.

**Tests:** H-M4 and H-M5, per-band attribution sums, outbreak hazard integration, cause slot source completeness, payload compatibility, composite non-causality, and Lot byte-identity smoke tests.

- [ ] **Step 1: Add failing Lot tests** for substrate-based endemic burden, per-band cause weights, `Ending::Outbreak`, Plague cause naming, the new cause slot, additive JSON, and composite/non-causal projection metadata.
- [ ] **Step 2: Implement `windows/lot/src/endemic.rs`** against the substrate view and committed facts; keep it draw-free and preserve the existing hazard magnitude.
- [ ] **Step 3: Extend `windows/lot/src/draw.rs` and `hazard.rs`** with the hash-expanded `cause` draw, band-specific attribution, outbreak hazard integration, and `Ending::Outbreak`.
- [ ] **Step 4: Extend `slots.rs`, `json.rs`, `narrate.rs`, and client-facing types** with the cause slot, pathogen labels, additive `ending.cause`, outbreak kind, and `odds.causes`.
- [ ] **Step 5: Add the projection boundary** so aggregate/composite projections can be rendered or narrated, while only materialized individuals and aggregate state are eligible for persistent write-back.
- [ ] **Step 6: Run focused Lot tests** including `cargo nextest run -p hornvale-lot` and `make lot-check`; verify the seed-42 byte-identity/refusal cases before accepting any fixture drift.
- [ ] **Step 7: Commit** with `feat(the-murrain): attribute disease in Lot projections`.

## Task 4: Laboratory Instrumentation and Predictions (Stage 4)

**Goal:** Add the six preregistered epidemic/Lot metrics, the H-P readout, and the measured census-cost record.

**Success Criteria:** The six metrics are registered with stable names and sources; H-P1 through H-P6 run once and are recorded verbatim; metric computation shares the per-world substrate/era-graph derivation; the isolation cost measurement is compared against the documented 1,320/1,650 second alarm/refusal limits.

**Tests:** Metric registry/column tests, fresh census schema checks, readout determinism, metric source provenance, and cost measurement guard.

- [ ] **Step 1: Add failing lab tests** for the six metric names, source facts, expected domains, and the H-P readout's count—not-ratio assertions.
- [ ] **Step 2: Implement metrics** in `windows/lab/src/metrics.rs` and the Domesday/census wiring, including largest present metapopulation, crowd endemicity, Plague endings, outbreak events, named disease deaths, and Lot slot fill.
- [ ] **Step 3: Add the hand-run `murrain_readout`** and run it once over the nine preregistered seeds; record the complete output and verdicts in the ledger without retuning after unblinding.
- [ ] **Step 4: Measure era-graph/substrate cost in isolation** and append the command/output arithmetic to the ledger and `docs/timings.md`; if caching is needed, cache once per Lot context rather than per metric.
- [ ] **Step 5: Run focused lab tests and the appropriate census fixture checks**; verify the six columns are declared in the generated-path roster.
- [ ] **Step 6: Commit** with `feat(the-murrain): add disease laboratory metrics`.

## Task 5: Genesis Closure and Campaign Documentation (Stage 5)

**Goal:** Finish the epoch transition, refresh generated artifacts on lefford, update the book and registry, and leave a reviewable campaign record.

**Success Criteria:** All declared artifacts are regenerated; census refresh and anomaly/Gnomon procedures complete through the sluice; the chronicle, retrospective, registry, Domesday, and freshness sweep agree with the shipped behavior; all stage/merge gates pass.

- [ ] **Step 1: Reconcile all generated artifacts** with `make rebaseline`, `make rebaseline-goldens`, and the declared artifact commands; inspect every moved file and commit the artifact-only changes.
- [ ] **Step 2: Re-author the anomaly evaluable-column witness and Gnomon injection arms** on lefford under the canonical claim, recording commands and outputs in the ledger.
- [ ] **Step 3: Queue the census** with `make sluice-census BRANCH=campaign/the-murrain REF=<full-sha>`; merge its branch only through the sluice after reviewing moved columns.
- [ ] **Step 4: Update authored documentation and registry**: `book/src/chronicle/the-murrain.md`, `book/src/domesday/history.md`, `windows/CLAUDE.md`, the named registry Where cells, and the retrospective/freshness records.
- [ ] **Step 5: Run the complete local commit gate** and submit the stage/merge request with the full SHA; let the canonical queue run the stage phases and heavy merge phase.
- [ ] **Step 6: Commit** with `docs(the-murrain): close disease campaign record`.

## Global Verification

Before declaring the campaign complete, run and record:

```bash
cargo fmt --check
cargo clippy --workspace --all-targets -- -D warnings
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --quiet --manifest-path tools/placement-audit/Cargo.toml -- check
cargo run --quiet --manifest-path tools/plumb/Cargo.toml -- check
cargo nextest run --workspace -E "$(bash scripts/subfloor-roster.sh)"
```

The full workspace nextest/doc-test suite, census, heavy tier, artifact drift, and merge-product verification belong to the stage/sluice gates described in the repository guide; do not substitute a local partial run for those canonical checks.
