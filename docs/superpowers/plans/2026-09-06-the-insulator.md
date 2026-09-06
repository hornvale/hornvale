# The Insulator Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Measure the real contributor build closure and qualify one narrow build island without weakening Hornvale's correctness gates.

**Architecture:** Extend the existing Python experiment pattern under `tools/digest/experiments/the-insulator/`. The recorder runs immutable baseline and candidate commands in owned checkouts and targets, writes bounded manifests and raw attempts, and compares dependency graphs, invalidation behavior, and outputs. The candidate remains an experiment until two-host evidence and independent review justify a minimal tooling change.

**Tech Stack:** Python 3 standard library, `unittest`, Cargo metadata/build/test commands, Git worktrees and bundles, JSON manifests, SHA256 hashes, existing `make` gates and Sluice queue.

**Spec:** `docs/superpowers/specs/2026-09-06-the-insulator-design.md`

## Global Constraints

- Keep the root Cargo workspace correctness gates unchanged throughout the campaign.
- Do not change simulation behavior, committed world outputs, or golden values.
- Use immutable owned checkouts and owned targets; never reset or write the caller's checkout.
- Record preparation, compilation, test, queue, and authoring costs separately.
- Retain every failed or incomplete attempt with bounded output and cleanup status.
- Candidate outputs are compared with the authoritative path; candidate data never becomes facts.
- Unknown inputs, missing provenance, incomplete cleanup, and undeclared dependencies fail closed.
- Do not choose a performance target before the baseline exists.

## File map

- Create `tools/digest/experiments/the-insulator/README.md`: experiment contract, commands, workload, cold/warm policy, and interpretation limits.
- Create `tools/digest/experiments/the-insulator/measure.py`: pure manifest validation, command capture, graph extraction, timing records, and paired-run orchestration.
- Create `tools/digest/experiments/the-insulator/test_measure.py`: deterministic tests for validation, bounded capture, graph closure, invalidation, and failure retention.
- Create `tools/digest/experiments/the-insulator/workloads.json`: frozen representative digest commands and expected output paths.
- Create `tools/digest/experiments/the-insulator/results/baseline.json`: committed baseline summary after qualification, with raw attempts retained beneath the experiment evidence directory.
- Create `tools/digest/experiments/the-insulator/results/comparison.json`: baseline/candidate graph, cost, invalidation, and output comparison.
- Modify `docs/audits/campaign-reconciliation.tsv`: keep the spec and later plan/reader artifacts in the campaign audit population.
- Create `docs/superpowers/ledgers/2026-09-06-the-insulator.md`: record campaign rulings, follow-ups, and close evidence.
- Modify `book/src/frontier/idea-registry.md`: update the existing contributor-build-boundary row only after the final decision; do not create a new production promise before evidence.
- Create `docs/retrospectives/the-insulator.md`: process lessons, measured result, limitations, and deferred work at close.
- Modify `docs/superpowers/plans/2026-09-06-the-insulator.md`: check completed stages and remove it at campaign close when all stages are complete, as required by the project process.

### Task 1: Freeze the baseline contract and recorder

**Files:**
- Create: `tools/digest/experiments/the-insulator/README.md`
- Create: `tools/digest/experiments/the-insulator/workloads.json`
- Create: `tools/digest/experiments/the-insulator/measure.py`
- Test: `tools/digest/experiments/the-insulator/test_measure.py`

**Interfaces:**
- `load_workloads(path: Path) -> dict`: loads and validates the frozen workload list, including command argv, expected outputs, and output comparison mode.
- `capture(workload_id: str, checkout: Path, target: Path, evidence_root: Path, destination: Path, timeout_s: int = 3600) -> dict`: resolves one frozen named workload, enforces a read-only checkout plus writable target/evidence roots, and retains bounded stdout/stderr, timing, exit status, and cleanup metadata.
- `manifest_for_attempt(...) -> dict`: returns a complete attempt record with source, graph, toolchain, target, command, timing, output, and failure fields.
- `validate_attempt(record: dict) -> None`: raises `ValueError` for missing identity, unbounded output, incomplete cleanup, invalid status, or missing cost fields.

- [x] **Step 1: Write failing tests for workload and attempt validation.**

  Add tests for a valid workload, a missing output declaration, a non-list command, an attempt with incomplete cleanup, an attempt whose stdout exceeds the byte cap, and an expected registration refusal represented as a completed command with non-zero status.

- [x] **Step 2: Run the focused tests and verify they fail.**

  Run:

  ```bash
  python3 -m unittest discover -s tools/digest/experiments/the-insulator -p 'test_measure.py' -v
  ```

  Expected: import or validation failures because the recorder interfaces do not exist.

- [x] **Step 3: Implement the recorder and frozen workload loader.**

  Reuse the Counterpart `sha256`, JSON persistence, subprocess deadline, and bounded-output patterns. Use `time.monotonic_ns()` for wall duration, write attempts to a temporary file before renaming, and keep preparation/build/test timings as separate fields. Resolve only named workload identifiers. Canonicalize the checkout, target, and evidence roots before constructing the macOS or Linux policy; make the checkout read-only, allow writes only to target/evidence, and refuse before launch when enforcement is unavailable. Do not run a live build from unit tests.

- [x] **Step 4: Run the focused tests and verify they pass.**

  Run the same command. Expected: all validation and capture tests pass.

- [x] **Step 5: Commit the baseline recorder.**

  ```bash
  git add tools/digest/experiments/the-insulator
  git commit -m "test(insulator): define bounded build measurement records"
  ```

### Task 2: Collect and report the current build closure

**Files:**
- Modify: `tools/digest/experiments/the-insulator/measure.py`
- Modify: `tools/digest/experiments/the-insulator/README.md`
- Modify: `tools/digest/experiments/the-insulator/workloads.json`
- Create: `tools/digest/experiments/the-insulator/test_baseline.py`
- Create: `tools/digest/experiments/the-insulator/results/baseline.json`

**Interfaces:**
- `cargo_graph(manifest: Path, target_dir: Path) -> dict`: runs locked offline Cargo metadata and returns the canonical graph identity and package records.
- `changed_closure(graph: dict, changed_paths: list[str]) -> dict`: returns directly changed packages, reverse dependents, and the full invalidation set.
- `run_baseline(root: Path, output: Path, host_class: str, cold: bool) -> dict`: executes the frozen workload on an owned checkout and target and returns a validated baseline dossier.
- `summarize_baseline(attempts: list[dict]) -> dict`: returns paired cold/warm cost distributions and graph counts without combining nested timings.

- [ ] **Step 1: Write failing tests for graph closure and summary rules.**

  Test a graph with protocol, digest, lab, and kernel packages; assert direct changes and reverse dependents are distinct, unrelated changes do not enter the closure, nested preparation is not added to compilation cost, and incomplete attempts are rejected from the summary.

- [ ] **Step 2: Run the focused baseline tests and verify they fail.**

  ```bash
  python3 -m unittest discover -s tools/digest/experiments/the-insulator -p 'test_baseline.py' -v
  ```

  Expected: missing graph and summary functions.

- [ ] **Step 3: Implement graph extraction and baseline orchestration.**

  Measure the existing `tools/digest` workspace, including `digest-thing` and `digest-census-publication`. Include one cold target, one warm target, and invalidation probes for a protocol edit, an observer edit, a lab edit, and an unrelated workspace edit. Freeze the workload and source identities before collecting timing records.

- [ ] **Step 4: Run the baseline on Mac and inspect the records.**

  Run the recorder with an owned output directory and verify every attempt has a source identity, graph identity, output hash, cleanup result, and separate preparation/build/test durations. Inspect the output diffs manually before committing the summary.

- [ ] **Step 5: Commit the frozen baseline contract and local summary.**

  ```bash
  git add tools/digest/experiments/the-insulator
  git commit -m "feat(insulator): record current contributor build closure"
  ```

- [ ] **Step 6: Queue the minutes-scale canonical baseline.**

  Push the commit, run `make gate-commit`, and submit a stage-only diagnostic through `make sluice-stage BRANCH=... REF=<full-sha>`. Record queue and canonical phase receipts separately from the experiment's build timings.

### Task 3: Prototype the narrowest build island

**Files:**
- Modify: `tools/digest/experiments/the-insulator/measure.py`
- Modify: `tools/digest/experiments/the-insulator/test_measure.py`
- Create: `tools/digest/experiments/the-insulator/candidate/README.md`
- Create: `tools/digest/experiments/the-insulator/candidate/Cargo.toml`
- Create: `tools/digest/experiments/the-insulator/candidate/src/main.rs`

**Interfaces:**
- `candidate_manifest(baseline: dict, candidate: Path) -> dict`: records the candidate workspace, declared dependencies, source tree, and authoritative comparison command.
- `declared_boundary(manifest: dict) -> set[str]`: returns the candidate's admitted package and path set.
- `check_boundary(graph: dict, boundary: set[str]) -> None`: raises on an undeclared dependency or a production implementation copied into the candidate.
- `compare_outputs(authoritative: dict, candidate: dict) -> dict`: returns path, size, hash, and byte-equality results for each workload output.

- [ ] **Step 1: Write failing tests for boundary admission and output comparison.**

  Test that an admitted protocol dependency passes, an undeclared `windows/lab` edge fails, a candidate output hash mismatch fails, and a candidate that omits an expected output is incomplete rather than green.

- [ ] **Step 2: Run the focused tests and verify they fail.**

  ```bash
  python3 -m unittest discover -s tools/digest/experiments/the-insulator -p 'test_measure.py' -v
  ```

  Expected: missing candidate boundary and comparison interfaces.

- [ ] **Step 3: Build the smallest candidate around the baseline-selected observer.**

  Start with `digest-census-publication` as the candidate because the existing graph shows it reaches `windows/lab`. Keep the candidate outside the root workspace. Expose the same workload protocol, depend only on the data and APIs actually required by the observer, and stop immediately if the prototype needs a duplicate production rule.

- [ ] **Step 4: Run boundary, build, and output tests.**

  Use an owned candidate target and run the authoritative and candidate commands sequentially. Verify source identities, dependency graph, output hashes, and cleanup. Record any compile failure, missing input, or output difference as a retained failed attempt.

- [ ] **Step 5: Commit the candidate prototype or a measured rejection.**

  If the boundary is valid, commit the candidate files and manifest. If it is invalid, commit the failure report and do not add a second implementation.

  ```bash
  git add tools/digest/experiments/the-insulator
  git commit -m "feat(insulator): prototype a bounded digest build island"
  ```

### Task 4: Qualify, review, and decide

**Files:**
- Modify: `tools/digest/experiments/the-insulator/measure.py`
- Create: `tools/digest/experiments/the-insulator/test_qualification.py`
- Create: `tools/digest/experiments/the-insulator/results/comparison.json`
- Modify: `tools/digest/experiments/the-insulator/README.md`
- Modify: `book/src/frontier/idea-registry.md`
- Create: `docs/retrospectives/the-insulator.md`

**Interfaces:**
- `run_paired_qualification(baseline: dict, candidate: dict, hosts: list[str]) -> dict`: returns complete paired records for both paths and host classes.
- `invalidation_matrix(records: list[dict]) -> dict`: maps each representative edit to rebuilt packages and output identity.
- `decide(comparison: dict) -> str`: returns exactly `"admit"` or `"reject"`; rejects incomplete evidence, output mismatch, undeclared imports, duplicated authority, or non-repeatable savings.

- [ ] **Step 1: Write failing tests for qualification completeness and decision rules.**

  Cover missing host records, mismatched source identities, incomplete cleanup, output mismatch, undeclared dependency, duplicated authority, stable reduction, noisy non-reduction, and a negative result that must return `"reject"` while remaining valid evidence.

- [ ] **Step 2: Run the focused tests and verify they fail.**

  ```bash
  python3 -m unittest discover -s tools/digest/experiments/the-insulator -p 'test_qualification.py' -v
  ```

  Expected: missing qualification and decision interfaces.

- [ ] **Step 3: Run the paired Mac and canonical Linux qualification.**

  Freeze all inputs before the run. Retain baseline and candidate archives separately, compare invalidation matrices and output bytes, and keep preparation, build, test, queue, and authoring costs in separate fields. Do not re-run a failed attempt without retaining the original.

- [ ] **Step 4: Independently review the evidence package.**

  Dispatch a fresh reviewer with the spec, README, manifests, raw archives, and comparison only. Require it to verify source identities, dependency boundary, all workloads, all failures, output hashes, cost decomposition, and the decision. Resolve findings through the three-attempt reassessment rule.

- [ ] **Step 5: Write the final decision and route the registry.**

  Record whether the island is admitted or rejected, what the measurements support, and what remains unknown. Update the existing `PROC-contributor-build-boundary` row with measured evidence; do not mark selective verification or general federation shipped.

- [ ] **Step 6: Run the final local gate and canonical merge process if admitting code.**

  Run focused tests, `make gate-commit`, and the full stage/merge queue for the exact candidate. If the result is rejection, the final merge contains only tooling, evidence, and documentation. Preserve the root full correctness gates in either case.

- [ ] **Step 7: Close the campaign.**

  Complete the retrospective, reconcile the plan and audit rows, verify the published record, present the post-G3 ledger digest at G6, and leave the clean worktree in the pool. Remove the completed implementation plan only after every stage is complete.
