# The Staple D2 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a deterministic, typed, local subsistence exchange channel to the history bake and decide its viability with a preregistered paired 200-seed probe.

**Architecture:** Keep the existing history path as the control. Add a parallel typed stock ledger to live bake communities, derive total production from the existing harvest/productivity signal, and split it into two non-convertible commodities without changing total production. Run exchange as a pure deterministic clearing step before consumption; route only delivered stock into the existing pressure path through a bounded shortfall adapter.

**Tech Stack:** Rust workspace, `hornvale-worldgen` history bake, existing `cargo nextest` suites, existing Lab census descriptors, and the repository's deterministic `BTreeMap`/`Vec` conventions.

**Spec:** `docs/superpowers/specs/2026-09-06-the-staple-d2-design.md`

## Global Constraints

- `ASSESS_RATE` remains coupled to `GROWTH_RATE / 8`; D2 does not retune it.
- The control must preserve the existing history path and draw sequence.
- `stores` remains accumulated non-edible wealth and never enters subsistence consumption.
- Subsistence quantities are fungible only within their typed resource.
- D2 permits immediate spot delivery only; no debt, interest, labor promise, conversion, currency, spoilage, route loss, or price discovery.
- Task 0 verdicts are counts with denominators: zero activation is the inert pole; more than half treatment-only breaches of each existing bar is the instability pole.
- Every assertion must name the property it proves and reviewers must ask whether it can pass vacuously.

---

### Task 1: Freeze the paired Task 0 probe contract

**Files:**
- Create: `windows/worldgen/tests/suite/staple_d2_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs` to include the probe module if the suite manifest requires explicit inclusion.
- Modify: `docs/superpowers/ledgers/2026-09-06-the-staple-d2.md` with the measured harness ruling and any deferred minor.

**Interfaces:**
- Consumes: `hornvale_worldgen::build_world_to`, `BuildDepth::Settlements`, `WorldComponents`, and the existing history census accessors. Task 1's fixture does not depend on production exchange code.
- Produces: a stable report type local to the probe with `activation_worlds`, per-outcome attempt counts, stock-conservation residuals, and separate treatment-only breach counts for settlement count `40..=400`, collapse share `0.05`, and alive-at-now `50`.

- [ ] **Step 1: Write the failing probe assertions** for 200 fixed seeds using same-seed control/treatment pairs. Assert that the report has a world denominator of 200, that each outcome count has its own non-zero attempt denominator when the corresponding status is reported, and that the control path is byte-identical when exchange is disabled.
- [ ] **Step 2: Run the fixture to verify the expected behavioral red** on an intentionally empty outcome set. A compile failure is not sufficient evidence for the behavioral assertions; the fixture must execute its conservation and denominator checks.
- [ ] **Step 3: Add the pure report and fixed seed roster** without adding a random draw or modifying `history/bake/v3` stream consumption. Keep all verdict thresholds in one probe module and copy the bar values from `history_tumult.rs`.
- [ ] **Step 4: Run the focused probe fixture** and verify the report distinguishes zero activation from zero attempts and distinguishes control-existing bar breaches from treatment-only breaches.
- [ ] **Step 5: Commit** with `git commit -m "test: freeze The Staple D2 probe contract"`.

### Task 2: Add typed subsistence state and production

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` beside `Community`, `PHASES_PER_YEAR`, `epoch_growth`, and the existing store phase helpers.
- Modify: `windows/worldgen/tests/suite/history_units.rs` or a focused new suite module for typed stock tests.

**Interfaces:**
- Consumes: the existing `Community` population, site, people index, seasonal production curve, `epoch_growth`, and existing terrain/climate inputs.
- Produces: a private typed resource enum with exactly the two D2 variants; a fixed two-component inventory on `Community`; pure helpers for total production partition, basket demand, inventory carry, and typed shortfall. Do not expose private bake state solely for the probe.

- [ ] **Step 1: Write failing tests** for the properties: partitioned quantities sum exactly to existing total production; the two resource variants cannot be added or consumed interchangeably; unconsumed inventory carries unchanged; a full basket has zero shortfall; `stores` is unchanged by subsistence consumption.
- [ ] **Step 2: Run those focused tests** and verify behavioral failures rather than merely missing-type errors.
- [ ] **Step 3: Add the typed resource and inventory helpers** using `f64` quantities with explicit one-person-phase units. Derive the specialization coefficient from an existing terrain/climate value, bound it, and preserve the total. If the chosen input cannot be read at the production site without a new draw, stop and record the tree finding in the ledger before choosing another existing input.
- [ ] **Step 4: Integrate lossless carry-over** into the existing phase state while leaving the control's current `stores` and population arithmetic untouched when D2 is disabled.
- [ ] **Step 5: Run focused tests and inspect the diff** for any accidental `stores` or `population` replacement; then commit with `git commit -m "feat: add typed subsistence inventory"`.

### Task 3: Implement deterministic local exchange clearing

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` or create a private module under `windows/worldgen/src/` only if the existing file's tests and visibility conventions make the split smaller and clearer.
- Modify: `windows/worldgen/tests/suite/history_units.rs` with clearing tests.

**Interfaces:**
- Consumes: typed inventories, fixed complementary basket, `traversable_neighbors` in `history_bake.rs`, and the pinned 1:1 typed bundle ratio inherited from the half-and-half basket. No ratio schedule or price discovery exists in D2.
- Produces: a pure clearing function that accepts a snapshot of communities and inventories and returns atomic typed deliveries plus explicit outcome statuses; it must not consume a random stream or mutate `stores`.

- [ ] **Step 1: Write failing tests** for bilateral exchange, competing requests, partial delivery, funded acyclic chain, funded reciprocal cycle, unfunded cycle, and iteration-order identity.
- [ ] **Step 2: Run the tests** and confirm each fails on the missing clearing behavior; a compile-only failure does not discharge the test step.
- [ ] **Step 3: Generate current-phase proposals** from projected demand and opening stock: reserve the projected basket first, offer only remaining stock, and restrict eligibility to the conductance-positive one-hop set returned by `traversable_neighbors`.
- [ ] **Step 4: Clear simultaneously and pro rata** using sorted stable entity identifiers and atomic reservations. Allow incoming stock to satisfy already-declared downstream requests. Reject unfunded cycles rather than treating promises as stock.
- [ ] **Step 5: Return explicit derived statuses** (`proposed`, `accepted`, `settled`, `partial`, `refused`, `impossible`) and quantities for the study trace. Assert conservation in the function's tests.
- [ ] **Step 6: Run focused tests**, including a reordered input vector, and verify identical results. Commit with `git commit -m "feat: add deterministic local exchange clearing"`.

### Task 4: Integrate phase order, pressure adapter, and paired study

**Files:**
- Modify: `windows/worldgen/src/history_bake.rs` at the existing phase walk, growth, consumption, and pressure call sites identified during Task 2.
- Modify: `windows/worldgen/tests/suite/staple_d2_probe.rs` to run the treatment through the integrated bake.
- Modify: `windows/worldgen/tests/suite/history_units.rs` for phase-order and shortfall properties.

**Interfaces:**
- Consumes: Task 2 inventory and Task 3 clearing; existing growth and pressure functions; `ASSESS_RATE`, `GROWTH_RATE`, and `PHASES_PER_YEAR`.
- Produces: a D2-enabled treatment path and a control path that share all prior inputs and draws; a report containing per-attempt outcome counts and per-seed bar comparisons.

- [ ] **Step 1: Write failing phase tests** proving production precedes exchange, exchange precedes consumption, consumption precedes pressure/growth, and tribute remains after the exchange path.
- [ ] **Step 2: Run the focused tests** and verify the current phase order fails only where D2 behavior is absent.
- [ ] **Step 3: Add the phase integration** with exchange disabled in the control. Use the bounded monotone shortfall adapter: full satisfaction is identity; increasing typed shortfall cannot reduce pressure; the adapter cannot produce an unbounded multiplier.
- [ ] **Step 4: Add the integrated paired 200-seed report** and print activation, attempt outcomes, conservation residuals, and each treatment-only breach count. Do not add verdict thresholds after seeing results.
- [ ] **Step 5: Run the probe once with captured output**, inspect all rows without rerunning it merely to grep another line, and record the verdict plus characterization findings in the ledger.
- [ ] **Step 6: Commit** with `git commit -m "feat: integrate The Staple D2 exchange treatment"`.

### Task 5: Rebaseline, convert pins to invariants, and document the epoch

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-06-the-staple-d2.md` with every ruling, deferred minor, and probe result.
- Modify: `docs/superpowers/specs/2026-09-06-the-staple-d2-design.md` with measured values and final status.
- Modify: `docs/superpowers/specs/2026-09-04-the-staple-metaplan.md` only for accepted D2 ownership and verdict consequences.
- Create: `book/src/chronicle/the-staple-d2.md` and `docs/retrospectives/the-staple-d2.md` at close.
- Modify: the relevant census fixtures and generated history artifacts only through the sanctioned lefford census path.

**Interfaces:**
- Consumes: the accepted Task 0 report and the live history fixture outputs.
- Produces: committed rebaselined artifacts, invariants for every history-adjacent pin touched by D2, and a complete close package.

- [x] **Step 1: Run the sanctioned census on lefford** through `make sluice-census BRANCH=... REF=...`; do not run census generation locally.
- [x] **Step 2: Reconcile any moved census columns** as a union, regenerate `docs/digest/decisions-in-force.md` rather than merging it, run `make rebaseline`, and inspect `git status` before resubmitting.
- [x] **Step 3: Add invariants** for typed-unit conservation, control identity, phase order, and the exact treatment-only bar directions. Each invariant must state its direction and guard against vacuous success.
- [x] **Step 4: Write the chronicle and retrospective**, naming the outcome of every deferred minor in the ledger.
- [x] **Step 5: Run the stage/merge gates** through the sluice and stop at G6 for Nathan's review.
- [x] **Step 6: Commit documentation updates** with `git commit -m "docs: record The Staple D2 epoch"`.

Task 5 evidence: the canonical lefford census delivery at `34e66fcc2`
reported `1282s`, no census columns moved, and matching columns. Main was
absorbed with the required union/regeneration path in `b2df2308f` and
`d12b54b3c`; the post-absorption and final gate reports are recorded by
`2853629cd` and `eaab5fc14`. The invariant annotations and close documents
landed in `3c9af5894`. The final merge remains pending Nathan's G6 approval;
the checked steps record reported evidence, not a claim that main has moved.
