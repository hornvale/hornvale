# Task 5 scoped re-review, round 1

Spec verdict: PASS for R1/R2 implementation readiness. Both original findings are closed; the corrected code is ready for the canonical diagnostic. Full Task 5 acceptance remains Cannot verify pending actual 11-arm canonical execution.

Quality verdict: PASS within this scoped re-review. No open findings or new consequences requiring changes were found in the reviewed repairs. This is not a fresh review of unrelated code.

## Scope and identities

Reviewed actual fix code/tests/README in `02796fe0ae719a55f431f8aedc2ab6853c9a2776`, followed by frozen panel update `57f030395cac159dc6b93a7989a3df291e4a9a71`. Read the author's complete exact correction-round-1 report and retained `task-5-fix1-tests.txt` before this verdict. Original `task-5-review.md` remains intact. Controller's independent ledger/timing work was not reviewed or modified.

No implementation edits, source builds, behavioral assay, delegation, commits, or additional tests were performed by this reviewer. Existing focused tests directly cover the repairs; no concrete residual doubt required rerunning them.

## R1 — Closed

`run.py:186-197` defines exact role invocations and verifies the real checkout commit/tree immediately before capture. Role and arm/source context are inserted into the result before persistence and before cleanup/interruption handling. `run.py:210-237` requires the owned absolute output directory, checkout and target, exact role-specific argv/cwd (including locked/offline build settings), and expected source context. Base metadata binds to the frozen base tree; changed metadata/build/observe bind to the current arm. Recorded Cargo repository roots must agree with the owned checkout at lines 249-251.

`test_run.py:76-107` exercises unrelated/missing argv and cwd, missing/wrong capture context, removed locked/offline flags, missing/external owned locations, and the distinction between base and changed-arm metadata. The real nonzero child capture test additionally reads back persisted attribution. This closes the demonstrated accidental mixed-evidence acceptance without claiming hostile-author authentication.

## R2 — Closed

`run.py:242-243` requires the separately labeled candidate copy to equal the retained raw observation, without using candidate as checker authority. Lines 257-258 recompute and require the whole supplemental result. `supplemental_comparisons` at lines 370-385 validates the pair references and derives explicit left-source/right-change/joint-observation scope, selection and score from frozen inputs and validated outcomes.

`test_run.py:109-134` covers missing/contradictory candidate and missing/extra supplement pairs, changed scope, invented selection and invented score. Positive fixtures remain accepted. The original R2 fields can no longer silently contradict the raw evidence while obtaining completion.

## Freeze and consequences verified

- New implementation identity is `02796fe0ae719a55f431f8aedc2ab6853c9a2776`; current run.py/compare.py/checker.py bytes match that commit.
- Explicit base tree `7350d1b4abc83775de94f35860a47e158c61dedc` matches both the base arm and actual frozen base commit.
- All 11 source arms are unchanged. Previous panel is preserved byte-for-byte in `evidence/runner-preparation/panel-before-review-fix-1.json`; its SHA256 matches `prior_panel_sha256`.
- Current panel SHA256: `27648766387798093fd683580a38644b1a4b3b246a9a8869a5fa96acaa1581df`.
- Frozen checker, rule implementation, contract and bundle remain unchanged; bundle hash still matches. Q9's finite owner adapters/manual integration accounting remain in force.
- Author explicitly reports zero new source compilation/assay observations during the correction. The fixes concern evidence validation, not selector tuning against outcomes. Earlier source-only receipts remain historical receipts, not receipts for new behavioral execution.

## Evidence

First action output:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next
codex/federation-next
```

Author's retained focused suite output, read directly (not rerun):

```text
.......................................................
----------------------------------------------------------------------
Ran 55 tests in 21.295s

OK
```

Independent read-only identity checks:

```text
New implementation: 02796fe0ae719a55f431f8aedc2ab6853c9a2776
Base tree matches base arm: True
Base tree matches actual commit: True
Roster unchanged: True
run.py matches frozen implementation: True
compare.py matches frozen implementation: True
checker.py matches frozen implementation: True
Prior panel exact bytes preserved: True
Frozen contract unchanged: True
Bundle hash matches: True
Prior panel hash reference matches: True
```

## Cannot verify

The actual full 11-arm canonical compilation/observation and its safe/joint-only/lender/unusual behavioral properties remain unverified. Task 6 reserved challenge, supported-host behavioral qualification and independent full replay, controller freeze/stage acceptance and downstream campaign-close obligations remain outstanding. Passing this re-review closes the two code findings; it does not complete Task 5 or the campaign.
