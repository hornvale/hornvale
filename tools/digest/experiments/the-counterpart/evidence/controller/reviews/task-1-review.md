# Task 1 review

## Spec Compliance

✅ Spec compliant for the Task 1 implementation visible in `7b97e43f8450c198e11305118ed9130ead4841a6..5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`.

- `tools/digest/packages/counterpart/src/lib.rs:36` implements the requested public `snapshot()` operation. It reads production source/component/borrowing lists, observes Settlement registration and the lender namespace before Thing, captures Thing registration separately, then records the composed namespace. The object contains the seven prescribed facts and no checker derivation.
- `tools/digest/packages/counterpart/src/lib.rs:20` preserves returned registry errors and caught panic text as refused observations. Source/component observation remains outside the catch boundaries, so those failures cannot silently become empty successful observations.
- `tools/digest/packages/counterpart/src/lib.rs:55` collects the candidate separately from production facts. The controller-approved `default()`/`contribution()` API correction is recorded in the reviewed plan and ledger changes; it is not an unexplained interface substitution.
- `tools/digest/experiments/the-counterpart/contract.json:1` freezes the exact four questions, package/source mappings, contributor, scope, and authority pointers. `README.md:10` discloses writer roles and material shared inputs; `README.md:57` explains retention, cleanup, immutable inputs, and the polling threshold's overshoot limitation.
- `scripts/charter-measure.sh:4` resolves the wrapper directory and delegates to the extracted module. The diff preserves the original supervisor and eight lifecycle tests, with optional retention/bounds added to that same implementation. Import and two-file HEAD checks are exercised in `scripts/test_charter_measure.py:35` and `scripts/test_charter_measure.py:40`.
- `tools/digest/packages/counterpart/Cargo.toml:1` is development-only, uses the specified existing dependencies, and introduces no contributor enrollment metadata. The lock delta adds one local package record and no external version/checksum changes. No production simulation or golden file appears in the diff.

⚠️ Cannot verify from this task diff: independent checker derivation, owner freeze chronology beyond the controller's recorded statement, all-question execution on every arm, immutable experiment attempts, both supported-host qualification, replay, stage gates, census, or G6. Those are later-task/controller obligations, not missing Task 1 code. The controller should verify them at their corresponding boundaries.

## Strengths

- `tools/digest/packages/counterpart/src/lib.rs:92` uses literal real-base ownership/borrowing expectations instead of deriving expected owners through the observed function. `lib.rs:75` also checks preserved refusal details.
- `scripts/test_charter_measure.py:8` verifies exact NUL-containing stdout, stderr, sizes, and hashes. `:18` covers fast oversize exit, `:24` covers termination of a still-running oversize writer, and `:31` checks deadline classification.
- `scripts/charter_measure.py` retains the existing session-based cleanup path, failure evidence before abort, and owned-worktree retention on uncertain cleanup. Its extracted self-tests continue to cover child groups, interruption, abnormal parent exit, and emit-before-stop behavior. The extension does not introduce a competing supervisor.
- `tools/digest/experiments/the-counterpart/README.md:64` accurately distinguishes the output polling threshold from a hard quota; it also assigns persist-before-stop responsibility to direct `measure()` consumers. Exact bytes are preserved rather than truncated to make the limit appear satisfied.

## Issues

### Critical

None found.

### Important

None found.

### Minor

None requiring a change in this task.

## Assessment

**Task quality: Approved.** The implementation preserves the established lifecycle while adding the required opt-in evidence, and keeps raw production observations separate from candidate judgments. The reviewed code and reported focused checks support Task 1 acceptance; later integration evidence remains explicitly unverified.

## Review evidence

Read the supplied brief, shared contract, report, reviewer method, and entire diff once in three consecutive ranges. No changed source file was separately reread, no unchanged-code investigation was necessary, and no already-evidenced suite was rerun. No source/index/branch mutation was performed; this requested report is the sole review artifact written.

First-command output:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next
codex/federation-next
```

Recorded implementation evidence (from the report, not fresh reviewer execution):

```text
cargo test -p digest-counterpart: 2 passed; 0 failed
python3 scripts/test_charter_measure.py: Ran 6 tests in 2.382s; OK
bash scripts/charter-measure.sh --self-test: Ran 8 tests in 8.263s; OK
cargo fmt --manifest-path tools/digest/Cargo.toml --all --check: exit 0
selected package clippy -D warnings: exit 0
shellcheck scripts/charter-measure.sh: exit 0
subfloor-run-chunked: all 3 chunk(s) passed.
timed.sh: 'gate-commit' wall=99.504s user=127.226s sys=24.972s cpu_ratio=1.53 (recorded) rc=0
[codex/federation-next 5cc62d8b6] Add Counterpart raw observer and retained process evidence
```

Reviewed head: `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`. No reviewer commit was made.
