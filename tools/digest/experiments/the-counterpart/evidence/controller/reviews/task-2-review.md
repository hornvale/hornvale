# Task 2 review

## Spec compliance

- **Approved.** The reviewed diff implements the requested independent checker and hand-derived cases. `checker.py:103` validates both inputs before evaluation; `checker.py:191` returns exactly the four frozen questions. `checker.py:41` rejects missing, duplicate, and unknown question IDs. Paths below are relative to `tools/digest/experiments/the-counterpart/` unless otherwise stated.
- The controller-authorized `checker-derivation.md` replaces this task's concurrent README edit. Its disclosure at `checker-derivation.md:8` explicitly names shared production context and incidental production-test exposure, without claiming security blindness.
- **Cannot verify across tasks:** Task 1 must commit the same frozen contract and link this disclosure from README; the controller must record checker identity and complete the Stage 1 gate. Raw duplicate-key rejection, frozen contract identity, observation completeness, candidate isolation at the call boundary, and attempt validity remain runner/integration checks, explicitly identified at `checker-derivation.md:77` and `checker.py:3`. This diff neither establishes those guarantees nor claims to.

## Strengths

- `checker.py:120` retains source/component multiplicity and checks membership in both directions. `checker.py:138` independently checks declaration uniqueness, membership, non-self borrowing, and actual prior lender ownership.
- `checker.py:162` derives expected owners from declarations/default ownership independently of the observed owner; `checker.py:169` rejects extra Thing-owned names, and `checker.py:178` preserves every prior lender concept, including unborrowed ones. No candidate declaration or Charter verdict is consumed.
- `checker.py:151` retains structural borrowing violations after lender refusal, otherwise returns unknown; `checker.py:158` never certifies partial composition. Thing refusal alone leaves completed lender evidence usable.
- `test_checker.py:34` uses literal hand-derived input and expected outcomes. Cases cover the required wrong-owner counterexample (`:46`), undeclared collisions (`:56`), reverse obligations (`:90`, `:106`, `:118`), and refusal independence (`:136`). Malformed facts/contracts and unknown/duplicate IDs are exercised from `:175` onward.
- The retained task report distinguishes the original missing-result failures from behavioral RED, and provides an applied, executable wrong-owner mutation with different original/mutated hashes. Its GREEN and hook outputs contain no unresolved warnings or failures.

## Issues

No Critical, Important, or Minor findings.

## Assessment

**Task quality: Approved.** The finite predicates are direct, deterministic, and maintain multiplicity where collapsing entries would hide defects. Tests exercise observable behavior with literal expected answers rather than borrowing the implementation as an oracle.

Review was read-only except this requested review artifact. The initial combined read was output-truncated; the missing derivation/checker portion was recovered from the same diff package, and retained test-summary receipts were recovered from the report. No changed source file was separately reread, no Charter contributor implementation was read, no broader code crawl was performed, and no evidenced suite was rerun.

## Evidence

Branch assertion output:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-checker
codex/counterpart-checker
```

Reviewed base: `1e248b2c9`; reviewed head: `26c693ce027bbfb8c937e4f36f024949a6b97589`.
Retained report excerpts (not reruns):

```text
Ran 32 tests in 0.017s
FAILED (failures=74)
Ran 32 tests in 0.005s
OK
Applied mutation: remove observed-owner versus expected-owner comparison
checker.py and test_checker.py compile: PASS
Behavioral test exit: 1
Ran 1 test in 0.001s
FAILED (failures=1)
     Summary [   4.949s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-checker 26c693ce0] Add independent raw-fact ownership checker for The Counterpart
```
