# The Staple D4 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a zero-impact D4 diagnostic that determines whether existing Hornvale dynamics produce recurrent, locally interpretable portfolio regimes before any specialization labels or persistent state exist.

**Architecture:** Preserve the existing D2 history sidecar and add a separate phase-resolved D4 observation sidecar at the worldgen composition root. Pure D4 helpers classify typed profiles, recurrence, mechanism separation, and axis debt; an ignored fixed-roster integration probe joins those observations to the existing live-community denominator and reports per-seed falsifier branches. No save-facing history, world behavior, seed stream, `Function`, or epoch changes are permitted.

**Tech Stack:** Rust workspace, `hornvale-worldgen`, consolidated nextest integration suite, deterministic `BTreeMap`/ordered vectors, existing D2 exchange treatment, local `make gate-commit`, sanctioned sluice path for any fixed-roster run.

**Spec:** `docs/superpowers/specs/2026-09-09-the-staple-d4-design.md`

## Global Constraints

- Count one live `BakeOccupation` at `History::now` against `census(history).alive_at_now`.
- Treat one complete D2 window as provisional; require comparable recurrence before a regime verdict.
- Retain production, voluntary exchange, imports, shortfall, coercive transfer, and protection access as separate channels.
- Preserve phase identity; never use an all-window average as the sole temporal evidence.
- Keep raw typed vectors beside normalized composition and structural signatures.
- Report incomplete, zero, isolated, hub-dominated, singleton, and mechanism-unavailable branches explicitly.
- Verdicts are per adequately powered seed; pooled totals are descriptive only.
- Keep D4 observation sidecars out of save emission and preserve existing D2 behavior and stream consumption.
- Do not add `Function`, occupation, learning, movement, price, or persistent specialization state.
- Do not run ignored fixed-roster reports or censuses locally; use the sanctioned sluice path only after the campaign boundary authorizes it.
- Each Rust task ends with focused tests and a commit; Rust changes require `make gate-commit` before stage submission.

---

### Task 1: Define pure D4 portfolio and regime types

**Files:**
- Create: `windows/worldgen/src/d4.rs` — typed portfolio channels, signatures, recurrence classes, mechanism classifications, axis-debt branches, and pure comparison helpers.
- Modify: `windows/worldgen/src/lib.rs` — declare `d4` and re-export only the public diagnostic types/functions.
- Test: `windows/worldgen/src/d4.rs` unit tests.

**Interfaces:**
- Produces `D4PortfolioVector` with separate `[f64; 2]` typed fields for realized output, voluntary exchange, imports, shortfall, coercive transfer, and protection access.
- Produces `D4PortfolioProfile` containing raw vector, normalized composition, phase identity, completeness, and mechanism availability without a specialization label.
- Produces `D4RecurrenceClass`: `Incomplete`, `Transient`, `Seasonal`, `Rotating`, `Drifting`, and `PersistentCandidate`.
- Produces `D4RegimeVerdict`: `NoRealizedDifferentiation`, `ProjectionCollapse`, `VacuousDifferentiation`, `TransientContrast`, `SeasonalRegime`, `PersistentCandidate`, and `MixedOrUnderpowered`.
- Provides pure helpers `d4_normalize_profile`, `d4_profile_signature`, `d4_recurrence_class`, and `d4_regime_verdict` that accept already-computed observations and never read labels or mutate world state.

- [ ] **Step 1: Write failing unit tests for typed separation and normalization.**
  Add tests proving that equal outputs with different imports remain distinct, imports and shortfalls are not merged, coercive transfer never satisfies the voluntary-exchange channel, all-zero vectors are non-evidentiary, and normalized composition never changes the raw vector.

- [ ] **Step 2: Run the focused tests to verify failure.**
  Run: `cargo test -p hornvale-worldgen d4 --lib`
  Expected: FAIL because the module and D4 types do not exist.

- [ ] **Step 3: Implement the pure data model and helpers.**
  Use fixed typed arrays and deterministic ordering. Reject non-finite values and negative quantities where the source channel is non-negative. Return an explicit unavailable/mechanism-debt state instead of fabricating zero. Keep normalization separate from raw-value comparison and do not introduce quantization.

- [ ] **Step 4: Add recurrence and falsifier branch tests.**
  Pin one-window transient, repeated same-phase seasonal, cross-phase persistent-candidate, rotating, drifting, flat, collapse, and vacuity cases. Assert that a single differing pair cannot clear the positive branch.

- [ ] **Step 5: Run tests and commit.**
  Run: `cargo test -p hornvale-worldgen d4 --lib && cargo fmt --check`
  Expected: PASS.
  Commit: `git add windows/worldgen/src/d4.rs windows/worldgen/src/lib.rs && git commit -m "feat(staple-d4): define portfolio regime diagnostics"`

### Task 2: Add the zero-impact phase-resolved portfolio observation seam

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` — phase/window observation structs, accumulators, production and D2 exchange recording, tribute/protection recording, and live sidecar translation.
- Modify: `windows/worldgen/src/lib.rs` — re-export the public D4 observation type through `hornvale-worldgen`.
- Test: `windows/worldgen/src/history_bake.rs` unit tests and existing D2 integration tests.
- Modify: `docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md` — record any seam rulings before changing behavior.

**Interfaces:**
- Produces `History::diagnostic_portfolios: Vec<DiagnosticPortfolioWitness>` ordered by `BakeId`, empty when exchange treatment is disabled.
- Each witness carries the live community/site identity and an ordered vector of complete phase records. A phase record carries the existing phase index, typed production/realization values, voluntary exchange outcomes, typed shortfall, and coercive/protection values when the existing tribute path observed them.
- A missing production, coercive, or protection path is represented as mechanism debt/availability, never as a zero flow.
- The new sidecar is not consumed by `history_emit`, not serialized, and does not alter `History::diagnostic_subsistence` or `ExchangeCensus` semantics.

- [ ] **Step 1: Write failing unit tests for phase identity and mechanism separation.**
  Add tests proving that twelve phase records retain their phase indices, a harvest spike is not replaced by its annual average, voluntary exchange counters remain separate from tribute, and disabled treatment emits no portfolio sidecar.

- [ ] **Step 2: Run the focused tests to verify failure.**
  Run: `cargo test -p hornvale-worldgen history_bake::tests --lib d4_portfolio`
  Expected: FAIL because phase-resolved D4 observations do not exist.

- [ ] **Step 3: Accumulate only already-computed values.**
  At `produce_subsistence_phase`, record the typed production delta already computed for the community. At exchange clearing, record the existing requester/counterparty status as voluntary exchange evidence. At consumption, record the existing typed shortfall. At tribute collection/protection evaluation, record the already-computed remittance/protection relation in a separate coercive channel. Do not add draws, new flows, or feedback.

- [ ] **Step 4: Translate live observations at history close.**
  Retain live zero-phase communities as explicit incomplete witnesses, matching D3B’s established denominator rule. Sort by `BakeId`; preserve site identity; leave all emitted ledger bytes and save-facing history fields unchanged.

- [ ] **Step 5: Run focused tests and commit.**
  Run: `cargo test -p hornvale-worldgen history_bake::tests --lib d4_portfolio && cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d2_probe) or test(staple_d3b_probe)'`
  Expected: PASS with existing D2/D3B behavior unchanged.
  Commit: `git add windows/worldgen/src/history_bake.rs windows/worldgen/src/lib.rs docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md && git commit -m "feat(staple-d4): expose phase-resolved portfolio witness"`

### Task 3: Build the per-seed D4 falsifier probe

**Files:**
- Create: `windows/worldgen/tests/suite/staple_d4_probe.rs` — joins, profile reduction, recurrence comparison, branch table, fixtures, and ignored fixed-roster report.
- Modify: `windows/worldgen/tests/suite.rs` — register `staple_d4_probe`.
- Modify: `cli/tests/fixtures/world-build-sites.tsv` — roster the probe’s one real world build if the gate requires it.
- Modify: `docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md` — record probe rulings and evidence.

**Interfaces:**
- Builds each seed through `build_world_with_exchange_treatment(..., ExchangeTreatment::Enabled)` and consumes the same-run `History` sidecars.
- Joins live `BakeOccupation` records to `DiagnosticPortfolioWitness` by `BakeId`, asserting denominator, uniqueness, site agreement, phase completeness, and mechanism availability.
- Computes per-community profiles and compares same-phase windows before cross-phase aggregates.
- Emits per-seed branch records and descriptive pooled totals; the fixed roster remains `1..=200` and ignored locally.

- [ ] **Step 1: Write deterministic fixture tests.**
  Prove that one-cycle variation is transient, repeated same-phase variation is seasonal, cross-phase recurrence is a persistent candidate, a tribute-only profile is mixed rather than voluntary specialization, equal normalized profiles with different raw scale are not automatically distinct regimes, equal outputs with different dependencies remain distinct, and missing mechanism paths become axis debt rather than zeroes.

- [ ] **Step 2: Run the focused tests to verify failure.**
  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d4_probe)'`
  Expected: FAIL until the probe module and Task 1/2 interfaces exist.

- [ ] **Step 3: Implement deterministic joins and reduction.**
  Sort all live units by `BakeId`, join each sidecar exactly once, retain raw vectors and phase records, and apply the D4 branch table per seed. Equal-weight communities; never pool population, degree, or total exchange volume into the verdict. Report `axis_debt` categories for unavailable output, capability, access, temporal, mechanism, and causal paths.

- [ ] **Step 4: Add vacuity and emission regressions.**
  Assert that all-zero, isolated, single-type, singleton, hub-only, zero-phase, missing-join, and disabled-treatment inputs cannot clear the positive branch. Compare emitted ledger bytes before and after sidecar population to prove the diagnostic remains save-inert.

- [ ] **Step 5: Run focused non-roster tests and commit.**
  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d4_probe) or test(staple_d2_probe) or test(staple_d3b_probe)'`
  Expected: PASS; the ignored fixed-roster test is listed but not run.
  Commit: `git add windows/worldgen/tests/suite/staple_d4_probe.rs windows/worldgen/tests/suite.rs cli/tests/fixtures/world-build-sites.tsv docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md && git commit -m "test(staple-d4): add portfolio regime falsifier"`

### Task 4: Cross-domain review and campaign-boundary verification

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-08-the-staple-d3b.md` — final implementation rulings and verification evidence.
- Modify: `docs/superpowers/specs/2026-09-09-the-staple-d4-design.md` only if a review finding changes the approved contract.
- No census fixture or save artifact unless the sanctioned campaign-close process authorizes it.

- [ ] **Step 1: Run the focused D2/D3B/D4 suite and inspect the diff.**
  Run: `cargo nextest run -p hornvale-worldgen --test suite -E 'test(staple_d2_probe) or test(staple_d3b_probe) or test(staple_d4_probe)'`
  Expected: PASS for all non-ignored tests.

- [ ] **Step 2: Run the local commit gate.**
  Run: `make gate-commit`
  Expected: fmt, clippy, audits, freshness checks, and the sub-floor roster pass. Do not run a census locally.

- [ ] **Step 3: Review the measurement against the approved spec.**
  Check that the implementation never invents `Function`, never treats coercion as voluntary specialization, preserves seasonal identity, reports axis debt, and leaves save emission and D2 behavior unchanged. Apply the cross-domain re-instantiation check to ecology, network flow, and a non-economic domain; record the result in the ledger.

- [ ] **Step 4: Record review findings before any fix.**
  Add each finding as a ledger ruling, make at most one scoped correction cycle per issue, then rerun the affected focused tests and `make gate-commit`.

- [ ] **Step 5: Stop at the campaign boundary.**
  Do not run the sanctioned fixed-roster report, census, stage submission, merge, or campaign close from this plan. Those require the next campaign gate and G6 close process after review of the implementation result.

## Plan self-review

- The spec’s denominator, profile channels, recurrence rules, mechanism
  separation, axis-debt contract, vacuity branches, and deferred scope are
  covered by Tasks 1–4.
- No task assigns a role, changes a save format, adds a dynamics feedback
  loop, or assumes a fantasy axis has a consumer before proving the join.
- The only measurement gap is reported as mechanism or axis debt; no missing
  channel is fabricated as zero.

## Completion note

Tasks 1–4 are complete. The approved D4 diagnostic is implemented, locally
gated, and green through the canonical stage and merge gates; it landed on
main at `8c5e6e95`. The fixed-roster report and census remain intentionally
unrun because D4 is a diagnostic rung, not a dynamics rung.
