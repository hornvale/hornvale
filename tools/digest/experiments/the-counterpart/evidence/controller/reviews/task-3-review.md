# Task 3 review

Spec compliance: ✅ Compliant within Task 3 scope and controller clarifications.
Task quality: Approved.

Reviewed base `5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd`, head `846f3fa27760480ed5aac7f1f6a30368bd23faa5`, using the supplied review package. Read its diff in one logical pass; the initial combined tool output truncated, so recovered the missing evidence portion in bounded reads. No implementation changes, new source commits, suite reruns, or subagents.

## Strengths and compliance evidence

Paths below are relative to `tools/digest/experiments/the-counterpart/` unless qualified.

- `patches/thing-safe-addition.patch:1`, `patches/thing-joint-claim.patch:1`, and `patches/thing-unusual-borrow.patch:1`: three nonempty Git patches exclusively target `domains/thing/src/lib.rs`. The first two add matching source/component rows for distinct new names; the third adds matching home rows and an explicit Settlement borrowing entry at line 32. No production Rust, lock, checker, raw observer, or Settlement source modification ships in the reviewed artifact-only commit.
- `evidence/thing-author/thing-safe-addition-raw.stdout:1` and `evidence/thing-author/thing-joint-claim-raw.stdout:1`: actual raw facts show 20 unique source/component names, accepted registration, and the new token/marker absent before Thing but Thing-owned afterward. These are observable non-no-op source effects, independently readable from the candidate's satisfied declarations.
- `evidence/thing-author/thing-unusual-borrow-raw.stdout:1`: home is already supplied by Settlement, appears in both Thing rosters and BORROWED, and remains Settlement-owned after Thing. The unusual borrow is a real qualified operation, not a guessed replacement.
- `owners/thing.json:3`, `evidence/thing-author/source-integrity.json:2`: fixed reviewed source base attribution agrees. The integrity record at lines 4–5 retains identical before/after SHA-256 values; the three variants retain distinct source/patch hashes and actual command metadata. Restoration is supporting integrity evidence, not claimed atomic capture.
- `owners/thing.json:33`: positive lender/roster assumptions and separate unclaimed-name negative assumptions are meaningful. The qualified union at line 50 and per-variant supply/consume deltas keep existing imports distinct from new-name absence assumptions. The joint candidate explicitly remains a prediction at line 158.
- `owners/thing.json:51`: declaration authority is limited; no full-domain or world/save safety is inferred. Shared-name/base/raw-observer disclosure, baseline lender reading, and absence of sibling author implementation access are documented at lines 63–66. FROZEN production roster tests are intentionally untouched under the supplied scope clarification.
- `evidence/thing-author/thing-safe-addition-build.stderr:19`, `thing-joint-claim-build.stderr:4`, and `thing-unusual-borrow-build.stderr:4`: retained successful build completions agree with zero-exit attempt metadata. Raw stderr streams are empty; build output contains ordinary Cargo progress and no warnings. `owners/thing.json:347` preserves attempt history, and lines 339–345 qualify elapsed time and ambient gate overlap rather than presenting a controlled benchmark.

## Findings

Critical: None.
Important: None.
Minor: None.

## Cannot verify from this task diff

- Task 5 must reconstruct immutable source objects from the fixed base/patch hashes, retain the bundle, run all four frozen questions on every valid arm, and observe joint composition. The isolated Thing evidence cannot establish the predicted combined collision (`owners/thing.json:158`).
- Owner/checker/comparator freeze chronology, reserved independent Task 6 challenge, replay, canonical stage gates/census, and G6 are controller obligations outside these artifacts. Owner-record assertions do not independently prove separation of author contexts; controller dispatch history must establish it (`owners/thing.json:63`).
- Restoration hashes establish the recorded source identity, not atomic capture or absence of concurrent writers. Controller ownership/scheduling records remain necessary (`evidence/thing-author/source-integrity.json:4`).

## Focused verification and command evidence

Named risk: the report could omit hook warnings or claim an unavailable gate result. Inspected only its cited `/tmp/counterpart-thing-commit.log` using `rg -n 'warning|error|Summary|pre-commit|846f3fa27|Finished'`; apparent error matches were passing test names, with no diagnostics. No suites rerun.

Initial required command output:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/counterpart-thing
codex/counterpart-thing
```

Existing hook evidence (log lines 1, 10, 90–91):

```text
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
    Finished `test` profile [optimized + debuginfo] target(s) in 6.81s
     Summary [  12.023s] 75 tests run: 75 passed, 246 skipped
[codex/counterpart-thing 846f3fa27] Retain independent Thing registry specimens and observations
```

The hook skips are its ordinary artifact-path filter, not skipped assay obligations. No review commit was made.
