# Decision ledger — the atomic claim (tooling/sluice-atomic-claim)

Branch: `tooling/sluice-atomic-claim`. Scope: the merge queue's dispatch
interlock. No decision block reserved and none minted — precedent is
`tooling/census-delivery-yellow-bypass` (2026-09-04), a queue-substrate fix
that minted no decision and put its rationale in code comments and
`scripts/CLAUDE.md`. This follows it.

## What was broken

`sluice-queue.sh next` returns the first `state=queued` row under a flock that
is released when the process exits. `sluice-drain.sh` then ran the mouth check
— seconds to a minute — and only afterwards called `set-state running` in a
**second** process. The row read `queued` for that entire window.

`sluice-run.sh` and `sluice-census.sh` touched the queue not at all, so a run
launched directly left its row reading `queued` for the whole run and forever
after.

Observed live 2026-09-04, both halves:

- **Duplicate execution.** A direct `sluice-run.sh` (pid 1240741) and a
  hand-run drain (pid 1253175) both dispatched `48aa9373b6f2`. Two ~800 KB
  logs, both rc=0.
- **Ghost rows.** The operator set terminal states by hand twice in one
  session (`req-fbe2f5a9003f-…` census; `req-a126fe1fc312-…` /
  `req-d6fdcd723579-…` merges).

## #1 [G1] — where the interlock lives

**Question**: how should the queue prevent duplicate execution and stale rows?

**Decision**: adopt **A + B** — an atomic `claim` subcommand that selects and
marks under one lock, *and* an executor that claims its own row when nothing
claimed it for it. Explicitly **not** C (forbid direct invocation).

**Why**: `sluice-queue.sh` already owns a lock and a state machine, so an
atomic claim is native to the design rather than bolted onto it. The row state
was already being used as the interlock; it simply was not transactional.

**Alternatives discarded**:

- **C — forbid direct `sluice-run.sh`, force drain.** Costs the operator
  escape hatch, which is documented and was used productively all session
  (a by-hand census delivery that saved ~1200 s of box time). Rejected: the
  escape hatch was never the problem, the *unbookkept* escape hatch was.
- **D — per-SHA process-liveness guard.** Requires matching process command
  lines, and `pgrep -f` matches its own cmdline; a guard whose answer depends
  on how the query is spelled is not a guard.
- **E — make the flock the interlock.** **A null result.** The flock is the
  *box* interlock (one job at a time), not the *row* interlock: two runs of
  the same row are not prevented by it, they merely serialize and both
  execute. It looks like a fix and is not one.

**ideonomy passes / overturns**: 1 pass (dimension-identification +
combination, organon: atlas, prompts: side-effect / predictability /
naturalness). **1 overturn** — the pass reversed the initial answer. The
instinct was C, framed as operator discipline ("I should have used drain").
The side-effect axis showed C pays with the escape hatch, and the naturalness
axis showed the fix belongs in the queue rather than in a rule people must
remember. The repo's own standing lesson agrees: if the answer to "what stops
this recurring?" is a person, it is not in code.

**Capture actions**: rationale into `scripts/CLAUDE.md` (both the
`sluice-queue.sh` and `sluice-drain.sh` entries); eight tests in
`scripts/test-sluice.sh`.

## #2 [Q] — a proposed `substrate` phase, approved and then WITHDRAWN

**Question**: are the queue's own suites (`test-sluice.sh` 218,
`test-sluice-census.sh` 9, `test-census-guard.sh` 14) gated anywhere?

**What I claimed**: nothing ran them — no lane set, no Makefile target, no Rust
test, and since 0125 no CI. On that basis Nathan approved a conditional
`substrate` phase and it was ratified as 0766.

**THE CLAIM WAS FALSE.** `scripts/lane-outboard.sh` — the `outboard` phase,
already on both the stage and merge lists — runs all three:
`test-sluice.sh` at line 75, `test-census-guard.sh` at 116,
`test-sluice-census.sh` at 127. Its own header at line 57 records that The
Staff closed this gap. The proof is in the run that was supposed to land the
phase: those suites executed under `=== phase outboard ===` in its log.

**How the search missed it**: I grepped `scripts/lane-sets.tsv`, the
`Makefile`, and `cli/tests/`, and concluded "nothing". The roster names the
*command* (`bash scripts/lane-outboard.sh`), never the suites it invokes, so
the roster cannot answer this question and I read its silence as an answer.
The one directory never searched was `scripts/` — the directory being edited.
A single `grep -rn test-sluice scripts/` settles it.

**Decision**: 0766 and the `substrate` phase are WITHDRAWN — reverted whole
(wrapper, roster row, both phase lists, the decision record, the digest row,
the 0426 pin edit). Nathan approved the reversal. The decision block 0766-0775
stays reserved and unused; gaps cost nothing.

**ideonomy passes / overturns**: 0 — the question was put to Nathan and the
ruling was his. Stated rather than left blank.

**The lesson is not "grep harder."** It is that a *negative* claim
("nothing runs X") needs a positive search over the place the thing would
live, not an absence across three places it would not. An absence is only
evidence where presence was possible.

## #3 [G5] — the incident: a test that reset the chamber

The first submission of this branch reported **rc=0 and LANDED**, moved main
`db498e453..e4126833e`, and **did not land**. The tested merge product
`fb09c3c31` (parents `db498e453` + `772a3d81b`, containing the fix) is not an
ancestor of main; the first surviving phase commit `191aeb7b2` has parent
`db498e453`, the pre-merge base. main received only artifact regens
(`timings.md`, `subfloor-roster.tsv`, `test-baseline-lefford.tsv`). No wrong
code landed and nothing was lost, but a merge said green about a tree it had
discarded.

**Cause, and it was in this branch's own new tests.** Two facts compose:

- the harness exports `HV_CENSUS_LOCK` to a temp lock, so a nested
  `sluice-run.sh` acquires a FREE lock instead of blocking on the chamber's;
- T7/T8 overrode `HV_SLUICE_REPO_ROOT="$repo_root"` — the real repo, which
  inside the chamber is its own live worktree.

So during `outboard`, the suite ran the real `sluice-run.sh` against the
chamber's checkout, which detached and reset it to `origin/main`.

**The isolation already existed and was overridden.** Line 1181 exports
`HV_SLUICE_REPO_ROOT="$chamber_repo"`, a scratch repo with a scratch origin
under `$tmp`, built for exactly this. The override was added to make an
unexplained `rc=127` go away — the scratch repo simply lacked
`sluice-queue.sh`, which the executor now calls. The correct fix was one `cp`.

**Fix**: the overrides are deleted (with a comment saying why one must never
be re-added), `sluice-queue.sh` is copied into the scratch repo, and a
**blast-radius guard** is the last assertion in the file: the real repo's HEAD
is recorded before any test runs and must be unmoved at the end. Proven live
via the `HV_TEST_FAKE_START_HEAD` seam — the guard fails when HEAD appears to
have moved. Verifying it the direct way would mean running a chamber against a
live worktree, which is the incident itself.

**Why nothing caught it**: every assertion in a 218-test suite was about what
the queue DID, and none about what the suite MUST NOT TOUCH. Mutation-testing
each assertion — which was done, three times — cannot find this, because the
assertions were all correct. The defect was in the environment they ran in.

## Verification

Every guard was mutation-tested — a guard that has never been observed failing
is not known to work.

| mutation | expected | observed |
| --- | --- | --- |
| remove `with_lock` from `claim` | atomicity test fails | **12 of 12** claimants won the same row |
| `exit 4` → `exit 0` (already-held) | T4 fails | T4 failed |
| remove executor `exit 9` | T7 fails | T7 failed (ran on into real work, rc=128) |

`test-sluice` 218 passed / 0 failed; `test-sluice-census` 9/0;
`test-census-guard` 14/0.

## Follow-ups

- **Cross-candidate overlap is still unchecked.** The mouth compares a
  candidate against `origin/main`, never against the *other queued
  candidates*. On 2026-09-04 `campaign/the-weft` was launched directly behind
  `campaign/the-housemark`; they shared 20+ files and the second died rc=10 at
  the `<merge>` step. `comm -12` over the two file lists would have predicted
  it. Not fixed here: it is a vetting aid, not an interlock, and this branch
  is scoped to the interlock.
- **Row elapsed includes claim wait.** `sluice-drain.sh` measures `SECONDS`
  around a call that blocks on the flock, so a row's "in NNNNs" is queue wait
  plus run, not run. It misled this session's own cost analysis (a stage gate
  reported 2677 s had done 1869 s of work). Phase sums in the run log are the
  honest figure.
- **A merge can report `LANDED` while pushing a tree that is not the merge
  product.** `sluice-run.sh` computed its final tree from whatever the worktree
  held after the phases, and never re-checked that the merge product was still
  an ancestor of it. The proximate cause here was a rogue test and is fixed, but
  the chamber's own assertion is missing: before pushing, it should verify the
  merge product is an ancestor of the tree it is about to land, and refuse if
  not. That check would have turned this silent mislanding into a loud refusal.
  Not fixed here — it is a change to the push path, which is the one place this
  branch should not also be touching.
