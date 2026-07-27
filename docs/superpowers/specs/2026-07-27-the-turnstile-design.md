# The Turnstile — design

**Campaign:** The Turnstile
**Date:** 2026-07-27
**Status:** spec, awaiting G3 review

One writer per goldens directory — enforced where the writing happens, named
correctly in the docs, and visible in the timing ledger afterwards.

## 1. The problem

Census goldens can be written three ways. Only one of them takes the lock.

```
entry point                                          host guard   lock
---------------------------------------------------  ----------  ------
scripts/census-run.sh                                    yes       yes
HV_CENSUS=1 bash scripts/regenerate-artifacts.sh         yes       NO
cargo run -p hornvale -- lab run studies/the-census…     yes(Rust) NO
```

The host guard has this covered because it was given **parity**: it lives in
shell (`scripts/census-canonical-host.sh`) *and* in Rust
(`windows/lab/src/census_guard.rs`, invoked from `publish()`). That module's
own doc says why, and the reasoning transfers verbatim:

> It cannot guard `cargo run -p hornvale -- lab run <study>`: that path calls
> straight into `publish` with no shell wrapper in between… This module is
> that missing guard, invoked from `publish` itself so every caller — present
> or future — inherits it.

The lock never got that treatment. It sits in the outermost wrapper only.

**The docs point at the unlocked path.** `CLAUDE.md:71` reads "the sanctioned
refresh is therefore local now — `HV_CENSUS=1 bash
scripts/regenerate-artifacts.sh`". That is the command that skips the lock,
and it is the command a careful reader will run. This is the root cause, not
carelessness.

**Observed 2026-07-27.** During The Hoist's close, an authorized regen was
launched with the documented command while a census triggered from the other
machine over SSH (`census-run.sh` → `hornvale-census-wt`) was already 3
minutes in. Two censuses shared 40 cores for seven minutes. Nothing broke —
they targeted different trees and determinism makes contention irrelevant to
output — but the regen's census phase read 6m57s against a clean 5m53s, an
apparent 18% regression from a campaign that had just measured −24.3%. It was
identified only because an unrelated `ps` surfaced a `hornvale` process at
2668% CPU that nobody in the session had started.

### 1.1 The hazard is not what it first looks like

Two collisions, two very different severities:

- **Different trees** (what happened): harmless to output. Costs throughput
  and *corrupts measurements*.
- **Same tree** (what did not happen, by luck): `publish()` runs
  `remove_stale_artifacts` and then writes summary / `rows.csv` /
  `schema.json` / hundreds of SVGs. Two publishers interleaving there can
  leave a `rows.csv` from one run beside SVGs from another. That is silent,
  permanent, and **drift-check-invisible** — both files are individually
  well-formed.

The second case is what justifies enforcement at the write seam. The first
justifies observability.

### 1.2 The lock key is the box, not the directory (owner decision, G3)

The spec initially proposed re-keying the lock on the goldens **directory**,
reasoning that the tearing hazard is per-directory and that runs in
different trees could safely proceed in parallel.

**Overturned at G3 by Nathan:** the runs must serialize *completely*. The
reasoning is that the binding constraint is not the hazard, it is the
**machine** — there is one Lefford, a census saturates ~38 of its 40 cores,
and two concurrent runs in different trees still halve each other's
throughput. "Safely in parallel" is true of the output and false of the
clock. Since the whole point of the local census is that it is a ~6-minute
operation a human waits on, protecting wall time is the requirement;
directory-level parallelism would be a correctness refinement that makes the
actual experience worse.

So the key stays **global** — one census at a time on this box, others queue.
`/tmp/hv-census.lock` was right about the key all along; it was only wrong
about how many entry points respect it.

The per-directory tearing hazard (§1.1) does not disappear — it is simply
subsumed: complete serialization prevents same-directory tearing *and*
different-directory contention with one rule. It survives in this spec as an
additional argument for enforcing at the **write seam** rather than only in
the shell, not as an argument about the key.

Framed in the codebase's own idiom: `schedule::genesis_systems().
single_writer_check(..)` enforces *exactly one declared writer per
predicate*. This is that rule one level up — **exactly one census writer per
box**.

## 2. Non-goals

- **Not removing `census-run.sh`'s queueing.** Blocking-and-queueing is
  load-bearing for the remote-trigger workflow: Nathan fires a run from the
  other machine over SSH and walks away. That must keep working.
- **Not making the lock gate the build.** No new failure mode in CI; the
  guard fires only on a genuine concurrent write.
- **Not changing what a census computes.** Zero effect on goldens; the
  acceptance test is that a regen still produces a zero diff.
- **No new crates.** `std` has no `flock`; the Rust side uses atomic file
  creation plus PID liveness, which needs no dependency.

## 3. Design

### Item 1 — the docs name the locked path

`scripts/census-run.sh` becomes *the* documented way to refresh census
goldens. Change every place that currently names the unlocked command:

- `CLAUDE.md:71` — "the sanctioned refresh".
- `.claude/skills/closing-a-campaign/SKILL.md` — the DoD walk.
- `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md`.
- `book/src/laboratory/overview.md`.
- Decision 0063 gets a **superseding note**, not an edit (decisions are
  superseded, never rewritten).

**Also fix a stale assertion found while reading:** `Makefile:77`'s help text
for `rebaseline` says *"census regen is AWS-only: make regen-remote"*, which
decision 0063 reversed — AWS is abandoned and the census is local-canonical.
That text is in `make help` output, contradicting the constitution.

### Item 2 — a re-entrant flock in `regenerate-artifacts.sh`

The host guard is already hoisted to the top of that script for exactly this
reason. Put the lock beside it.

Re-entrancy is required: `census-run.sh` holds the lock on fd 9 and *then*
calls `regenerate-artifacts.sh`. `flock` is per open-file-description, so a
child re-`flock`ing the same path on a fresh fd would **deadlock against its
own parent**. `census-run.sh` therefore exports `HV_CENSUS_LOCK_HELD=$$`, and
`regenerate-artifacts.sh` skips acquisition when that names a live ancestor.

### Item 3 — a global write-claim, waited on at the `publish()` seam

Beside the host guard in `census_guard.rs`, and invoked the same way.

- **Key:** global — one claim for the box, the same scope as
  `/tmp/hv-census.lock`. Not per-directory (§1.2).
- **Mechanism:** a claim file at a fixed `/tmp` path containing PID, host,
  ISO start time, and the goldens directory being written. Created with
  `OpenOptions::new().create_new(true)`, which is atomic — the winner is
  whoever creates it. Removed on completion, including on the error path.
- **Behaviour: BLOCK until the claim is free**, so runs serialize completely
  rather than one failing. `std` has no `flock`, but no dependency is needed:
  poll the claim file on a few-second interval. A census is a ~6-minute
  operation, so poll granularity is irrelevant to throughput, and polling has
  none of `flock`'s inheritance subtleties. While waiting, print the same
  shape of message `census-run.sh` already prints — `census-claim: waiting
  for the claim held by pid 1573508 on lefford since 18:53:41Z …` — so a
  waiting run is never mistaken for a hung one.
- **Staleness:** if the claim exists but its PID is not live (`/proc/<pid>`
  absent), it is stale — from a crashed or killed run — and is taken over
  with a loud note. This matters more under blocking than it would under
  refusing: without it, one crashed census wedges every future run on the
  canonical box until a human deletes a file. Linux-only is acceptable;
  decision 0063 makes this box the single golden-authoring platform.
- **Re-entrancy is mandatory, not a nicety.** With everything blocking on one
  global claim, the normal nested path — `census-run.sh` (holds flock) →
  `regenerate-artifacts.sh` → `cargo run … lab run` (wants the claim) —
  would **deadlock against its own ancestor** and hang the box for the full
  poll-forever duration. `census-run.sh` exports `HV_CENSUS_LOCK_HELD=$$`;
  both `regenerate-artifacts.sh` and the Rust guard treat "that variable
  names a live ancestor PID" as "already serialized, proceed". This is the
  single highest-risk detail in the campaign and gets its own test.
- **Claim early, like the host check.** `cli/src/main.rs` already calls
  `require_canonical_host_for` before the study runs, "to fail fast, before
  spending any compute". The claim takes the same position — acquired before
  the run, released after `publish` — so it covers the whole run rather than
  only the final write. That is what makes serialization *complete*: two
  censuses never overlap in their compute phases, which is where the
  contention actually lives.
- **Scope:** census studies only, reusing the existing `is_census_study`
  predicate, for parity with the host guard. (See §5.)

### Item 4 — censuses enter the timing ledger

`scripts/timed.sh` already exists, already writes `docs/timings.md`, and
already records `cpu_ratio = (user+sys)/wall`. Its header states the column's
purpose exactly:

> `cpu_ratio` ≈ parallelism achieved: it separates *more work* (user climbs)
> from *more contention* (wall climbs, ratio falls).

That is precisely 2026-07-27's signature — and the ledger would have shown it
instantly. The ledger's header also says it records "full-fixture regens,
**censuses**, full gates".

**It has never recorded a census.** Every row is `rebaseline`, every row is
from `MacBookPro`, and `lefford` has never written one — because
`make rebaseline` is the only `timed.sh` caller and it skips censuses.

So item 4 is mostly wiring, not invention:

1. `census-run.sh` wraps its regen in `scripts/timed.sh census -- …`.
2. The row is annotated when the run was contended — the claim file from item
   3 makes "was another census live during my run?" answerable, checked at
   start and end, recorded as a `contended` column.

A contaminated timing then announces itself in the ledger instead of waiting
to be inferred from a lucky `ps`.

## 4. Verification

- **Concurrency test (shell):** launch two `regenerate-artifacts.sh` runs at
  the same goldens dir; the second must block, not interleave. With
  `HV_CENSUS_LOCK_HELD` set by a fake parent, it must NOT block (re-entrancy).
- **Concurrency test (Rust):** a unit test over `census_guard`'s claim
  functions — claim, second claim observes the holder and waits, release,
  waiter proceeds. Plus a stale-claim test writing a dead PID and asserting
  takeover rather than an indefinite wait.
- **Re-entrancy test (the deadlock guard):** with `HV_CENSUS_LOCK_HELD` set
  to a live ancestor PID, a claim must return immediately even though the
  claim file exists. This is the test that fails if the nested
  `census-run.sh` → `regenerate-artifacts.sh` → `lab run` path would
  self-deadlock, and it must exist at both layers (shell and Rust).
- **Serialization is complete, not just at publish:** two censuses launched
  together must not overlap in their COMPUTE phases either — assert by
  timestamps that the second's start follows the first's release, since
  overlapping compute is the contention this campaign exists to remove.
- **A census regen still produces a zero diff** — this campaign must not
  touch goldens.
- **`make gate` green**, and `make help` no longer asserts AWS-only regen.

Every claim in §1 and §3 was checked against the tree rather than inferred:
the entry-point table, the `CLAUDE.md:71` wording, `timed.sh`'s columns, the
absence of census rows in `docs/timings.md`, and `census-run.sh`'s fd-9 lock
followed by its call into `regenerate-artifacts.sh`.

## 5. Flagged for review (G3)

- **Scope of the Rust claim: census studies only, or every published
  study?** The spec says census-only, mirroring the host guard. But the
  tearing hazard is generic — any two concurrent publishes to one study
  directory can interleave. Widening is a one-line change to the predicate
  and would protect `the-chorus`, `branches-family`, and the smaller censuses
  too. I lean census-only for parity and blast radius; reviewer's call.
- **`/tmp` for claim files.** Consistent with the existing
  `/tmp/hv-census.lock`, and cleared on reboot, which is a feature for
  staleness. The alternative (a state dir under the repo) would dirty
  `git status`. Recommend `/tmp`.
- **Refuse vs queue at the Rust seam — RESOLVED at G3: queue.** Runs
  serialize completely; a second census waits rather than failing. Achieved
  by polling the claim file, which needs no dependency (§3 item 3).
- **Should the wait be bounded?** `census-run.sh`'s `flock` blocks
  indefinitely today, and stale-claim takeover already prevents a crashed run
  from wedging the box, so the spec's default is an unbounded wait for
  consistency. The alternative is a generous timeout (say 30 min) that fails
  loudly rather than waiting forever on a pathological case. Recommend
  unbounded; flagging because "it hung" and "it is waiting correctly" look
  identical from outside, which is why the waiting message matters.
- **Decision log:** this campaign should probably ratify a short decision
  ("one writer per goldens directory; the claim lives at the write seam"),
  since it establishes a rule future campaigns must not re-litigate.

## 6. Acceptance criteria

- **Census runs serialize completely on this box, from all three entry
  points** — a second run waits for the first to finish rather than
  contending with it or failing, and their compute phases never overlap.
- The nested `census-run.sh` → `regenerate-artifacts.sh` → `lab run` path
  does NOT deadlock against itself.
- `census-run.sh` still queues, and still works when triggered over SSH from
  the other machine.
- A crashed run leaves no wedged claim: the next run takes over the stale one.
- `docs/timings.md` gains census rows, with contention visible.
- No doc in the tree names `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`
  as the sanctioned refresh; `make help` no longer contradicts decision 0063.
- A full census regen still produces a zero diff.
