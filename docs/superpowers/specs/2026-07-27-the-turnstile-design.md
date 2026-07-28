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
goldens. The sweep is enumerated rather than left to "everywhere
appropriate", since the whole failure was a doc naming the wrong command.
Every tracked file mentioning `HV_CENSUS=1`, from `git ls-files | xargs grep
-ln "HV_CENSUS=1"`:

- `CLAUDE.md` — line 26 (the directory-guide summary) and line 71 ("the
  sanctioned refresh is therefore local now"). The load-bearing one.
- `.claude/skills/closing-a-campaign/SKILL.md` — the DoD walk's regen step.
- `.claude/skills/dispatching-hornvale-subagents/dispatch-preamble.md`.
- `book/src/laboratory/overview.md` — the published description.
- `scripts/census-run.sh`, `scripts/regenerate-artifacts.sh` — comments that
  describe the guard set; both must now describe the claim too.
- `docs/decisions/0046` (superseded), `0063`, `0079` — decisions are
  **superseded, never rewritten**: 0063 and 0079 get a pointer line to 0081,
  and 0046 is left alone as already-superseded history.
- Retrospectives and old plans mentioning the command (`the-standing-offer`,
  `the-demesne`, `temperature`, `2026-07-12-lab-performance`, …) are
  **historical record and are NOT edited** — they describe what was true when
  written. Only live instructional docs change.

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
- **Behaviour: BLOCK until the claim is free, up to a bounded timeout**, so
  runs serialize completely rather than one failing — but a pathological hold
  fails loudly instead of hanging forever. `std` has no `flock`, but no
  dependency is needed: poll the claim file on a few-second interval. A
  census is a ~6-minute operation, so poll granularity is irrelevant to
  throughput, and polling has none of `flock`'s inheritance subtleties.
- **Timeout: 45 minutes by default**, overridable via
  `HV_CENSUS_WAIT_TIMEOUT` (seconds). The bound must exceed the longest
  legitimate hold: a full `HV_CENSUS=1` regen is ~12 minutes, and a queue two
  deep is ~24, so 45 leaves headroom without approaching "forever". On
  timeout the run **fails** (it must never proceed anyway — proceeding is
  exactly the contention this campaign removes) with exit code 75
  (`EX_TEMPFAIL`), so a caller can distinguish "busy, retry later" from a
  genuine error.
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
- **Scope — by COST, not by name (G3, decision 0081).** A run claims the box
  when it projects **≥ 200 world-builds** (`seeds.count × pin_sets.len()`,
  known before the run starts — roughly 40 seconds of exclusive box time), OR
  when it writes census goldens at any size (keeping the correctness half of
  §1.1 independent of the threshold).

  The cost rule is preferred over the existing `is_census_study` name test
  deliberately. A name list is precise today and **rots**: every future
  census-scale study must remember to join it, and the one that forgets is
  exactly the one that collides. A threshold derived from the property the
  lock actually protects — machine time — covers a new 1000-seed study on the
  day it is written, by nobody's diligence. `is_census_study` stays for the
  HOST guard, where naming is the right test because the hazard there is
  about which goldens are authored, not how long a run takes.

  Coverage at 200: `the-census` (1000) and `census-of-the-meeting` (2000)
  claim; `the-chorus` (50) does not; the 200-seed perf probe does — correctly,
  since it is a measurement, and measurements are what contention corrupts.

- **Short jobs never queue behind long ones.** Below the threshold, contention
  is a rounding error both ways: a 10-second study finishes in 20 contended
  seconds and steals a negligible slice of a 12-minute census. Serializing two
  LONG jobs is free in aggregate (contended, both end at ~2T; serialized, the
  first ends at T and the second still at 2T) — that is why the queue costs
  nothing where it applies, and why it must not apply below the line.

- **`make gate` / `make gate-full` advise, never block.** They print `note: a
  census is running (pid 1573508, 6m in) — your timings will be contended`
  and proceed. A developer waiting twelve minutes to *start* a four-minute
  gate is worse than the contention, and a gate is not a measurement.
  Upgradeable to blocking later once the ledger has data.

### Item 3b — what a waiting caller is told

The failure this campaign exists to fix was not really the collision — it was
that **a contended run and a normal run were indistinguishable**. A blocking
claim makes that worse unless the waiting is legible: an unexplained silent
pause on a 40-core box is exactly what a hang looks like. So the claim is
also the campaign's observability surface, and its contents are a contract,
not a debug aid.

**The claim file carries the context** — plain `key=value` lines (it is
`/tmp` scratch; no serde, greppable by eye and by script):

```
pid=1573508
host=lefford
user=nathan
started=2026-07-27T18:53:41Z
goldens=/home/nathan/Projects/hornvale-census-wt/book/src/laboratory/generated
label=census-of-the-meeting
ref=the-hoist@94bcc07a
cmdline=hornvale lab run studies/census-of-the-meeting.study.json
```

Every field earns its place by answering a question a blocked caller asks:
*who* (pid/host/user), *since when* (started), *doing what to which tree*
(label/goldens/cmdline), *from which code* (ref) — that last one matters
because "the census that is blocking me is running someone else's branch" is
a materially different situation from "it is running main".

**Message cadence**, shell and Rust alike, all to stderr:

- On first contention, immediately — never a silent pause:
  `census-claim: waiting — held by pid 1573508 (nathan@lefford) since
  18:53:41Z (4m12s ago), writing …/hornvale-census-wt, running
  census-of-the-meeting @ the-hoist. Waiting up to 45m0s.`
- Every 60s thereafter, with both clocks, so progress is visible:
  `census-claim: still waiting (6m30s elapsed; holder now 10m42s in).`
- On acquiring after a wait: `census-claim: acquired after 7m03s.` — this
  line is what tells the caller its own wall time includes a queue.
- On stale takeover, loudly: `census-claim: taking over a STALE claim — pid
  1573508 is not alive (started 18:53:41Z). Its run died without releasing.`
- On timeout, actionably rather than just fatally:
  `census-claim: TIMED OUT after 45m0s. Holder pid 1573508 has been running
  52m. Inspect: ps -p 1573508 -o pid,etimes,args. If it is dead, remove
  /tmp/hv-census.claim.`

**A status query**, so the question "is a census running right now?" has an
answer that is not `ps | grep`: `scripts/census-run.sh status` prints the
claim's fields and the holder's elapsed time, or "no census running". Cheap,
and it is the command that would have answered today's question in one step.

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
2. The ledger gains a **`waited_s`** column, fed by the claim's
   "acquired after" figure. This is the sharpest instrument the campaign
   produces: it splits a slow run into *queued* versus *slow*, which is
   precisely the distinction that made 2026-07-27's 6m57s look like an 18%
   regression. A row reading `wall=1240 waited_s=620` is self-explaining;
   the same row without the column is a mystery.

Under complete serialization `cpu_ratio` should now stay high and stable
across census rows — a fall in it becomes a real signal (something else on
the box) rather than the expected noise of two censuses sharing 40 cores.

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
- **Timeout fires and is actionable:** with `HV_CENSUS_WAIT_TIMEOUT=2` and a
  held claim, the waiter exits **75** within a few seconds and its message
  names the holder's pid, elapsed time, and the claim path to remove. Assert
  on the exit code and on the message naming the pid — a timeout that fails
  without saying who held the lock is the bug this item exists to prevent.
- **The waiting messages actually appear:** assert the first-contention line
  is emitted immediately (not after the first poll interval) and that it
  carries pid, host, start time, goldens dir, and label. Silence during a
  wait is indistinguishable from a hang, which is the failure mode being
  designed against — so its absence is a test failure, not a cosmetic one.
- **`census-run.sh status`** reports a live claim's fields, and reports "no
  census running" when the claim is absent or stale.
- **A census regen still produces a zero diff** — this campaign must not
  touch goldens.
- **`make gate` green**, and `make help` no longer asserts AWS-only regen.

Every claim in §1 and §3 was checked against the tree rather than inferred:
the entry-point table, the `CLAUDE.md:71` wording, `timed.sh`'s columns, the
absence of census rows in `docs/timings.md`, and `census-run.sh`'s fd-9 lock
followed by its call into `regenerate-artifacts.sh`.

## 5. Flagged for review (G3)

- **Claim scope — RESOLVED at G3: by cost, ≥200 world-builds** (or writing
  census goldens at any size). Neither "census studies only" nor
  "everything": the line is drawn on the axis the lock actually protects —
  machine time — computed before the run, so it needs no maintenance as new
  studies are added. Ratified in decision 0081.
- **Still open — the 200-build threshold is chosen, not derived**, like the
  45-minute timeout. It is reasoned (≈40s of exclusive box time at ~0.2s per
  build on 38 cores, calibrated against the observed 3m13s for 1000 builds)
  but `docs/timings.md` has never carried a census row to check it against.
  Item 4 produces that data; revisit both constants once real rows exist.
- **`/tmp` for claim files.** Consistent with the existing
  `/tmp/hv-census.lock`, and cleared on reboot, which is a feature for
  staleness. The alternative (a state dir under the repo) would dirty
  `git status`. Recommend `/tmp`.
- **Refuse vs queue at the Rust seam — RESOLVED at G3: queue.** Runs
  serialize completely; a second census waits rather than failing. Achieved
  by polling the claim file, which needs no dependency (§3 item 3).
- **Bounded wait — RESOLVED at G3: bounded.** 45 minutes by default,
  `HV_CENSUS_WAIT_TIMEOUT` to override, exit 75 on expiry. `census-run.sh`'s
  existing `flock` gains `-w` to match, so the shell and Rust layers agree
  rather than one waiting forever behind the other's timeout.
- **Still open — the timeout VALUE is a guess.** 45 minutes is reasoned (a
  ~12-minute full regen, a queue two deep, headroom) but not measured, and
  the ledger has no census rows yet to reason from. Item 4 will produce that
  data; expect to revisit the constant once a few real rows exist. Recorded
  here so a future reader knows it was chosen, not derived.
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
- **A blocked caller is never left guessing:** it learns who holds the claim,
  since when, writing which tree, from which ref — immediately, then at
  intervals, and again on timeout with a command to investigate. `scripts/
  census-run.sh status` answers "is a census running?" without `ps | grep`.
- **A queued run is legible after the fact:** `docs/timings.md` gains census
  rows carrying `waited_s`, so a long wall time is attributable to the queue
  rather than mistaken for a regression.
- No doc in the tree names `HV_CENSUS=1 bash scripts/regenerate-artifacts.sh`
  as the sanctioned refresh; `make help` no longer contradicts decision 0063.
- A full census regen still produces a zero diff.
