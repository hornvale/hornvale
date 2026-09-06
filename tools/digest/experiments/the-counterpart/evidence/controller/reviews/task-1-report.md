# Task 1 — raw observer and reusable process evidence

Working directory: `/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next`
Branch: `codex/federation-next` (verified first action and immediately before commit).

## Changes

- Added development-only `digest-counterpart`, without contributor metadata. `snapshot()` records the actual roster, component IDs, borrowing declarations, registries before/after Thing, separate registration outcomes, and separately caught candidate contribution.
- Froze `tools/digest/experiments/the-counterpart/contract.json`: SHA256 `37ea8fa06f086000f94cf215a391b5bcbd74bef92b9502bdc02ebb0a9ad32b34`. README discloses writers, shared inputs, schema, authority and measurement limits.
- Extracted the existing Charter supervisor and eight self-tests into importable `scripts/charter_measure.py`. The shell wrapper resolves its own directory. Import does not mutate argv, install handlers, print, or run workloads. CLI checks both executed files against HEAD.
- Added opt-in exact binary stream retention, both stream sizes/hashes, deadline parameter, polling output thresholds and final-size invalidation for fast exits. Existing lifecycle and cleanup semantics preserved; failed cleanup evidence is retained before abort.
- Verified source API correction, approved by controller: `ConceptRegistry::default()` replaces nonexistent `new`; `digest_thing::contribution() -> Result<Contribution,String>` replaces nonexistent `collect`. Candidate shape is `{outcome, detail, contribution}`, with null contribution after refusal/error.

## Behavioral RED evidence

The retention test initially called the extracted, unchanged real measure operation. The child successfully emitted NUL-prefixed stdout, but retained stdout was unavailable:

```
python3 scripts/test_charter_measure.py
AssertionError: b'' != b'\x00ok'
Ran 1 test in 0.136s
FAILED (failures=1)
```

The final test requests retention explicitly after the new keyword was introduced.
The raw observer test used literal ownership expectations against an initially empty snapshot scaffold (compiled successfully):

```
cargo test --manifest-path tools/digest/Cargo.toml -p digest-counterpart --offline
Finished `test` profile ... in 7.01s
running 1 test
test tests::observes_real_lender_and_thing_owners ... FAILED
assertion failed: facts["before_concepts"].as_array().unwrap().contains(&json!({"name":"hearth", "owner":"settlement"}))
test result: FAILED. 0 passed; 1 failed
```

Additional edge coverage was added after these implementation cycles; those checks are GREEN regression coverage, not claimed as separately witnessed behavioral REDs.

## GREEN evidence

```
cargo test --manifest-path tools/digest/Cargo.toml -p digest-counterpart --locked --offline
Finished `test` profile ... in 0.55s
test tests::preserves_returned_error_and_panic_as_refused_observations ... ok
test tests::observes_real_lender_and_thing_owners ... ok
test result: ok. 2 passed; 0 failed

cargo fmt --manifest-path tools/digest/Cargo.toml --all --check
(exit 0, no output)

cargo clippy --manifest-path tools/digest/Cargo.toml -p digest-counterpart --all-targets --offline -- -D warnings
Finished `dev` profile ... in 6.30s
(exit 0)

python3 scripts/test_charter_measure.py
Ran 6 tests in 2.382s
OK

bash scripts/charter-measure.sh --self-test
Ran 8 tests in 8.263s
OK

shellcheck scripts/charter-measure.sh
(exit 0, no output)

cargo run --quiet --manifest-path tools/digest/Cargo.toml -p digest-counterpart --locked --offline
(decoded stdout independently)
raw object: counterpart-v1 facts: 7 candidate: accepted
registration: {'detail': '', 'outcome': 'accepted'} {'detail': '', 'outcome': 'accepted'}
```

One combined edit/fmt/scoped-test tool command was falsely rejected by the whole-workspace-test hook because it contained both `fmt --all` and a later `cargo test -p digest-counterpart`. The rejected command did not execute. Separate ordinary commands succeeded, without an override.

## Lock delta and self-review

`tools/digest/Cargo.lock` adds exactly one local `digest-counterpart` record (11 lines), depending on existing digest-thing, kernel, Settlement, Thing, and serde_json. No external versions/checksums changed. No production simulation source or golden changed. No census, full-workspace intermediate suite, checker, or specimen was authored/run.

Review compared the extracted module directly against the embedded predecessor; changes are limited to import mechanics, requested measurement options and evidence, success classification for output limit, two-file source validation, and exact retention coverage on cleanup failure. The existing eight lifecycle tests are retained. Concept rows are sorted; authored rosters and borrowing preserve duplicate entries. Expected caught panic output remains on stderr. Unexpected component/source observation faults propagate to process failure.

The optional output limit is per stream and is a polling/termination threshold, not a hard filesystem quota. Fast writers can overshoot between polls and during cleanup; bytes are retained without truncation. This limit is disclosed. Direct `measure()` consumers must persist failed attempts before stopping on cleanup failure; the existing `record_sample()` already enforces emit-before-stop. Parent remains responsible for immutable owned checkouts and staged experiment runs.

## Commit and gate result

Ordinary commit (no hook bypass): `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`.

```
     Summary [   6.357s] 1435 tests run: 1435 passed, 4133 skipped
     Summary [   7.024s] 1383 tests run: 1383 passed, 4185 skipped
     Summary [   6.260s] 1227 tests run: 1227 passed, 4341 skipped
subfloor-run-chunked: all 3 chunk(s) passed.
timed.sh: 'gate-commit' wall=99.504s user=127.226s sys=24.972s cpu_ratio=1.53 (recorded) rc=0
[codex/federation-next 5cc62d8b6] Add Counterpart raw observer and retained process evidence
```

The hook's timing row may remain as `docs/timings.md` working-tree drift; it belongs to the controller's subsequent commit, not simulation source/golden drift. Final `git status --short` is recorded in the handoff.

