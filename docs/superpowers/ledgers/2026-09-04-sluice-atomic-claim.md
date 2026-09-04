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
