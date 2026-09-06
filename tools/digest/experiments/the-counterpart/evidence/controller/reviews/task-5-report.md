# Task 5 implementation handoff

DONE: bounded runner, pure shadow comparators, 11-arm source panel and bundle are committed for review. Full real-source qualification and Task 5 acceptance remain pending canonical execution by the controller, as explicitly directed; no complete assay or behavioral class result is claimed here.

## Placement and commits

First command and precommit checks printed:

```text
/Users/nathan/.config/superpowers/worktrees/hornvale/federation-next
codex/federation-next
```

- `792a2df84ba144b207c7bac94c27d2872d11ed72`: comparator/runner/tests/README implementation.
- `9536a75d8224dd42b4fbc5fc536a9f38685021e2`: final implementation identity; routes Git preparation through reviewed measure+persist, with the same deadlines/output limits as Cargo/observer commands.
- `179a706fd0d521f6239fe1b3d3647175c1263f49`: final panel, compact bundle and source-only reconstruction receipt. The source-construction preliminary panel is retained explicitly because the first receipt references its exact earlier identity. No source behavior was sampled between these implementation revisions.

Controller commit/index slot was requested and granted. Only Task 5 paths were staged. No source commit bypassed hooks; experimental objects were authored only with commit-tree in an owned clone. Final `git status --short` printed nothing. `git diff 5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd -- domains/thing/src/lib.rs domains/settlement/src/lib.rs tools/digest/Cargo.lock` printed nothing: production owner sources and reconciled source lock remain untouched.

## Actual verification

Initial missing-module REDs were setup evidence, not behavioral proof. Meaningful behavioral RED/GREEN cycles were then observed:

1. Outboard Cargo workspace root initially excluded production package directories; test failed with `AssertionError: ['missing Cargo membership: domains/thing/src/lib.rs'] is not false`. Recording the repository root separately from actual Cargo metadata made it pass.
2. Reviewed Settlement name subjects/full supply roster/prose assumptions initially fell through as missing subjects and negative assumptions. Finite explicit per-owner normalization made the hand-derived test pass; unknown syntax retains full fallback.
3. A stale pin/input hash mutation initially produced `AssertionError: ValueError not raised`. Exact expected source input/lock/pin validation made it fail closed.
4. Missing selected-set evidence and altered confusion scores initially produced two `ValueError not raised` failures. Recomputing all selectors and scores from retained metadata/declarations and independently re-evaluated raw facts made them fail closed.
5. An omitted score question initially produced `ValueError not raised`; exact four-question score validation made it pass.
6. A real Git command initially bypassed the measured bounded path (`AssertionError: [] is not true`). The corrected test wraps the real measure function, observes actual process execution, checks deadline=3600/output threshold=16777216, and verifies one persisted Git command record.

Final focused command:

```text
python3 -m unittest discover -s tools/digest/experiments/the-counterpart -p 'test_*.py'
...................................................
----------------------------------------------------------------------
Ran 51 tests in 17.947s

OK
```

This includes the unchanged 32 checker tests, 10 comparator tests and 9 runner tests. Real fixtures execute child processes, retain failed stdout, import actual Git bundles, compose disjoint patches, reject empty patches and reject textual conflicts. No copied process supervisor or new fault supervisor was introduced; Charter's existing fault coverage remains applicable. `run.py --self-test` and `--help` were exercised before the final Git boundedness correction (49 tests then); focused discovery exercises the same files after the correction. `py_compile` succeeded before that correction; final Python discovery imports/executes the corrected module successfully.

Actual ordinary commit-hook outputs:

```text
pre-commit: no Rust-relevant paths staged — running the prose-subject tests instead of 'make gate-commit'.
Summary [   4.295s] 75 tests run: 75 passed, 246 skipped
[... 792a2df84] feat(digest): retain bounded Counterpart replay and shadow comparisons
Summary [   4.365s] 75 tests run: 75 passed, 246 skipped
[... 9536a75d8] fix(digest): bound and retain Counterpart Git preparation commands
Summary [   3.973s] 75 tests run: 75 passed, 246 skipped
[... 179a706fd] test(digest): freeze eleven reconstructible Counterpart source arms
```

Tooling exposed foreground process handles rather than a Bash timeout parameter; all yielded commands were immediately polled in bounded foreground waits through completion. No watcher, detached source workload, local full-workspace intermediate suite, or census was started.

## Panel and identities

The four pairs are safe, collision, lender, and unusual; each maps base/left/right/joint explicitly. Sharing leaves 11 unique arms: one base, six owner solos, four compositions. The missing-negative comparison reuses the collision joint with a disclosed right-owner request scope; its imports-only supplement retains raw omission and unknown/full effective fallback. It is not a fourth primary comparator. The reserved correlated-wrong challenge is left entirely to Task 6.

The bundle is 4,642 bytes, SHA256 `6199ffccfe88ad3782ce7b88a7b6a064029bcddb3307cef18bf4b9e8e878609b`. Final panel SHA256 is `8c21bea63869fa29d177ad603e6ae37fa4629e46904d7f416d9d479a0fc25086`.

```text
tools/digest/experiments/the-counterpart/specimens.bundle is okay
The bundle contains these 10 refs:
[ten individually named specimen refs; exact IDs are in panel.json and receipt]
The bundle requires this ref:
5cc62d8b6ae30552f22a68e85fd9a2885ac6e1dd
The bundle uses this hash algorithm: sha1
```

All 11 commit/tree/input identities were reconstructed in a separate owned clone from the controller repository and bundle, without reading the original construction clone. Each experimental object has exactly the frozen base as parent. Receipt: `tools/digest/experiments/the-counterpart/evidence/runner-preparation/bundle-replay.json`. Construction used the initial implementation, before the Git measurement-only correction; the exact preliminary construction panel is retained beside the receipt. Final observation implementation is separately frozen at `9536a75d8...`.

Retained construction clone: `/var/folders/_0/j0_zkq_d3jn0klz033gq33cc0000gn/T/counterpart-panel-xs2zrvf0/construction/source-objects`.
Retained independent source replay clone: `/var/folders/_0/j0_zkq_d3jn0klz033gq33cc0000gn/T/counterpart-bundle-replay-d5eivsch/checkout`.

Current controller source checkout does not need either scratch for replay; the bundle plus explicit repository-history prerequisite is sufficient.

## Canonical handoff and limits

Run in the existing stage-only diagnostic transport under the canonical claim, against the final implementation/panel commit or its ordinary merge product:

```sh
python3 tools/digest/experiments/the-counterpart/run.py \
  --panel tools/digest/experiments/the-counterpart/panel.json \
  --output <absolute-new-owned-directory>
```

The output directory MUST NOT exist. Output contains Git preparation command records, toolchain records, per-arm metadata/build/observer records, selections persisted before behavior, arm records and incremental progress. Successful completion additionally writes `dossier.json` and `summary.json`; any failed command/unsafe cleanup stops sampling and retains available records and checkout. The entire directory is owned and retained; the controller should inspect and commit a bounded evidence selection, especially if a failed process overshoots a stream threshold.

No new subset CLI flag was added. A focused Mac qualification can use an explicitly retained derived one-arm panel with new roster identity, scope and parent-panel hash; it must never be substituted into the full panel's summary. No source compilation or source behavior observation was performed by Task 5 locally. Thus safe/joint-only/lender/unusual classifications remain preregistered properties, not certified results. `qualify_pairs` reports actual properties only after complete canonical observations.

The comparator reports selected-satisfied without calling it waste. Raw/effective selected-violating, selected-satisfied, unselected-violating, unknown and selected-unknown sets are retained. Agreement's broad affected-question mapping may yield little or no selection benefit; only actual results can establish the comparative null. Normalization is disclosed manual integration work and uses no candidate/owner observation as an authority. Metadata uses the full outboard Digest graph including production dependencies, not every unrelated package in the separate simulation workspace.

The supervisor limit is a polling threshold with possible overshoot, not a hard disk quota. Successful command records are bounded by it; failed full bytes remain retained in owned directories. The environment is allowlisted for reporting, not claimed hermetic. Target reuse is sequential and warm-state aware, not cold-build isolation. Queue wait and author effort remain null in runner output rather than invented. Existing source lock/pin hashes are recorded; this task performed no lock reconciliation, owner/checker modification, simulation change, or census.

Controller owns independent review, final freeze.json, canonical qualification, ledger/stage records, and Stage 2 submission/acceptance. Commit slot is released.


## Independent review addendum (2026-09-05)

The independent reviewer requested two P2 validation repairs: command/cwd attribution and candidate/imports-only supplemental evidence revalidation. See `task-5-review.md` for exact locations, reproductions, and separate spec/quality verdicts. The 51-test implementation record above is preserved; no suite or source build was rerun. Narrow probes returned completed=True for unrelated invocation, missing/contradictory candidate, and fabricated supplemental result. Bundle verification and frozen checker/rule/contract/bundle hashes passed. Full actual 11-arm canonical behavior remains Cannot verify; Task 5 acceptance is pending. No implementation file or commit was changed by this reviewer.

## Independent review correction round 1

The prior handoff and its command receipts above are preserved as originally reported. Independent review requested two P2 repairs; these supersede the earlier implementation identity before any accepted assay observation.

Read the complete `task-5-review.md`, receiving-code-review and test-driven-development skills. First action and both precommit checks again printed the expected worktree and `codex/federation-next`. No checker, comparison rules, owner record, source specimen, bundle, production source, lock or supervisor changed. No source compilation or assay was run. Controller's ledger and timings remained unstaged and untouched by this author.

R1: Required exact argv/cwd, owned output/checkout/target shape, locked/offline role parameters and per-command arm/source attribution. Metadata-base names the frozen base commit/tree; metadata-arm/build/observe name the current arm. `capture_arm` verifies the actual owned checkout immediately before each capture. Role and capture context are included in each persisted record before cleanup/interruption can abort. Metadata repository roots must match the recorded owned checkout. These checks protect against accidental evidence mixing, not hostile-author authentication.

Behavioral RED before R1 implementation:

```text
Ran 11 tests in 29.995s
FAILED (failures=14)
AssertionError: ValueError not raised
```

Failures covered unrelated observe argv/cwd, omitted argv/cwd/context, wrong arm/source context, omitted locked/offline flags, missing owned locations, external target, and changed-arm context falsely attributed to metadata-base. After implementation:

```text
Ran 11 tests in 29.456s
OK
```

R2: Candidate must be present and exactly equal to the raw observation's candidate object, while remaining separate from checker authority. Required imports-only supplements are recomputed from the full frozen pair roster, frozen declarations and verified outcomes. Structured request scope names the left source arm, right changed arm, joint observed arm, changed paths and variants. Missing/extra pairs, altered scope, selection or score refuse completion.

Behavioral RED before R2 implementation:

```text
candidate: Ran 1 test in 0.002s; FAILED (failures=2)
supplement: Ran 1 test in 0.008s; FAILED (failures=6)
AssertionError: ValueError not raised
```

After implementation, those focused tests passed in 0.001s and 0.011s. The actual child-process retention test additionally checks that a nonzero command's persisted file contains its attribution and role.

Final focused suite and ordinary hooks:

```text
python3 -m unittest discover -s tools/digest/experiments/the-counterpart -p 'test_*.py'
.......................................................
----------------------------------------------------------------------
Ran 55 tests in 21.295s

OK

Summary [   4.506s] 75 tests run: 75 passed, 246 skipped
[... 02796fe0a] fix(digest): validate Counterpart command and copied evidence attribution
Summary [   4.527s] 75 tests run: 75 passed, 246 skipped
[... 57f030395] test(digest): retain Counterpart review correction identity before observation
```

The final focused output is retained in `.superpowers/sdd/2026-09-05-the-counterpart/task-5-fix1-tests.txt`. This is 32 frozen checker tests, 10 unchanged comparator tests and 13 runner tests. No no-verify bypass or broader local suite was used.

Final correction commits:

- Implementation: `02796fe0ae719a55f431f8aedc2ab6853c9a2776`.
- Subsequent panel identity: `57f030395cac159dc6b93a7989a3df291e4a9a71`.

Panel SHA256 is now `27648766387798093fd683580a38644b1a4b3b246a9a8869a5fa96acaa1581df`. It adds the explicit frozen base tree `7350d1b4abc83775de94f35860a47e158c61dedc`, records the corrected implementation commit, and links the prior panel hash. Exact previous panel bytes are preserved at `evidence/runner-preparation/panel-before-review-fix-1.json`, alongside the earlier construction panel and receipt. Bundle, arm roster, checker, rules, contract and owners are unchanged. Final independent hash checks printed:

```text
checker unchanged frozen hash: True
rules unchanged frozen hash: True
contract unchanged frozen hash: True
Bundle unchanged: True
```

Canonical invocation remains unchanged and awaits scoped re-review/ordinary claim transport:

```sh
python3 tools/digest/experiments/the-counterpart/run.py \
  --panel tools/digest/experiments/the-counterpart/panel.json \
  --output <absolute-new-owned-directory>
```

A derived subset panel must retain `base_tree`, create its own roster identity, and declare its limited scope. No full-assay completion is claimed. Final working-tree status contains only the controller's `docs/superpowers/ledgers/2026-09-05-the-counterpart.md` and `docs/timings.md` changes. Index/commit slot is released to the controller.
