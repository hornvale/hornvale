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

### 1.2 The lock key is wrong

`/tmp/hv-census.lock` is one lock **per box**. The invariant that matters is
one writer **per goldens directory**. There were three candidate trees in
play on 2026-07-27 (a campaign worktree, the SSH run's `hornvale-census-wt`
scratch worktree, the main checkout). A per-box lock is simultaneously too
strict — it serializes runs that could safely proceed in parallel — and
aimed at the wrong thing.

Framed in the codebase's own idiom: `schedule::genesis_systems().
single_writer_check(..)` enforces *exactly one declared writer per
predicate*. This is that rule one level up — **exactly one writer per
goldens directory**.

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

### Item 3 — a write-claim at the `publish()` seam, keyed by directory

Beside the host guard in `census_guard.rs`, and invoked the same way.

- **Key:** `std::fs::canonicalize(base_dir)` — the resolved absolute goldens
  directory, so two different worktrees never contend and two writers to one
  tree always do.
- **Mechanism:** a claim file under `/tmp` whose name encodes a hash of the
  canonical path, containing PID, host, ISO start time, and the directory.
  Created with `OpenOptions::new().create_new(true)` — atomic. Removed on
  completion.
- **Staleness:** if the claim exists but its PID is not live (`/proc/<pid>`
  absent), it is stale — from a crashed or killed run — and is taken over.
  This is why a PID-carrying claim beats a bare lockfile: a crash must not
  wedge the canonical box until someone deletes a file by hand. Linux-only is
  acceptable; decision 0063 makes this box the single golden-authoring
  platform.
- **Behaviour: refuse, do not queue.** With a message naming the holder —
  `census already publishing to <dir>: pid 1573508 on lefford since
  18:53:41Z; queue with scripts/census-run.sh`. Queueing belongs at the shell
  seam where it is deliberate; a fast, specific refusal is more useful at the
  write seam and needs no blocking primitive.
- **Claim early, like the host check.** `cli/src/main.rs` already calls
  `require_canonical_host_for` before the study runs, "to fail fast, before
  spending any compute". The write-claim takes the same position — acquired
  before the run, released after `publish` — so it covers the whole run, not
  just the final write.
- **Scope:** census studies only, reusing the existing `is_census_study`
  predicate, for parity with the host guard. (See §5 — this is a question for
  review.)

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
  functions — claim, second claim refused with the holder named, release,
  claim succeeds. Plus a stale-claim test writing a dead PID and asserting
  takeover.
- **Different directories do not contend:** two claims on two distinct
  canonical paths both succeed. This is the test that would have failed under
  the old per-box key, and is the point of the re-keying.
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
- **Refuse vs queue at the Rust seam** — spec says refuse. If a future
  workflow wants a bare `cargo run -- lab run` to queue behind a running
  census, that needs a blocking primitive `std` does not provide, and would
  push toward a dependency. Worth deciding now rather than later.
- **Decision log:** this campaign should probably ratify a short decision
  ("one writer per goldens directory; the claim lives at the write seam"),
  since it establishes a rule future campaigns must not re-litigate.

## 6. Acceptance criteria

- All three entry points refuse a concurrent same-directory census write;
  none refuses a different-directory one.
- `census-run.sh` still queues, and still works when triggered over SSH from
  the other machine.
- A crashed run leaves no wedged claim: the next run takes over the stale one.
- `docs/timings.md` gains census rows, with contention visible.
- No doc in the tree names `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`
  as the sanctioned refresh; `make help` no longer contradicts decision 0063.
- A full census regen still produces a zero diff.
