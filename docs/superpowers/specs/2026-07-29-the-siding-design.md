# The Siding — Design

**Status:** drafted, awaiting G3 · **Date:** 2026-07-29 ·
**Campaign:** The Siding

Placement, not mechanism. This campaign decides *which machine runs which
job* and makes the canonical box serialize the jobs that saturate it. No
domain code changes; no draws, no seed labels, no save-format surface.

## 1. The problem

On 2026-07-29 at 15:15 UTC, lefford (40 cores) carried a load average of
**85.27** — three concurrent Hornvale test runs from three parallel campaign
sessions, each living in its own worktree under
`/home/nathan/.config/superpowers/worktrees/hornvale/`:

```
worktree         command                              elapsed at observation
---------------  -----------------------------------  ----------------------
the-watershed    make gate                            14:56  (baseline ~4 min)
the-wearing      timeout 3600 make gate               12:01  (baseline ~4 min)
the-winnowing    make gate-full -> heavy tier         39:09
```

A four-minute commit gate was **~3.7× over budget and still running**. The
cost is worse than the idealized model in decision
[0081](../../decisions/0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md),
which reasons that two contended jobs each finish near `2T`; three-way
oversubscription on a 40-core box also pays context-switching, memory
pressure, and cache thrashing, and the observed factor exceeded the naive
`3T`.

**Nothing was broken.** No lock leaked and nothing was bypassed: the census
claim (`/tmp/hv-census.claim`) was absent, and `/tmp/hv-census.lock` was
untouched since the previous day. The gates simply do not take it, **by
design** — 0081 ruled that `make gate` and `make gate-full` *advise rather
than block*, on the reasoning that "a developer waiting twelve minutes to
START a four-minute gate is a worse experience than the contention."

That ruling is sound and is **not** being reversed. It was calibrated against
a *census* holder. It never considered gate-versus-gate, where the wait is
another four-minute job rather than a twelve-minute one — and where 0081's
own aggregate math ("serializing two long jobs costs nothing; the queue is
free") applies in favour of queueing.

## 2. What is already host-locked, and was never written down

Investigating the above surfaced a correctness fact that outranks the
scheduling one.

**`census_fixtures_match_a_probe_of_live_seeds`**
(`windows/lab/tests/fixture_staleness.rs:120`) runs a **live probe** and
compares it against the committed census fixture. Those fixtures are
lefford-authored by [0079](../../decisions/0079-census-goldens-are-authored-on-one-enforced-host.md);
[0063](../../decisions/0063-census-regen-is-local-again.md) records that two
boxes disagree by one unit on ~0.1% of discrete-count metrics, decided in the
**compute** path upstream of the quantize-at-emit boundary
([0033](../../decisions/0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)),
where nothing can absorb it. The test is therefore only meaningful on
lefford. It is in the heavy tier, so today it is right *by accident*.

Three more heavy-tier tests **write committed artifacts**:

```
test source                                 writes
------------------------------------------  ----------------------------------------
cli/tests/history_battery.rs                book/src/laboratory/generated/the-history/
                                              summary.md, rows.csv
windows/chronicle/tests/sounding_sweep.rs   book/src/laboratory/generated/the-sounding/
                                              summary.md, rows.csv,
                                              sample-biographies.txt
windows/worldgen/tests/occupancy_readout.rs windows/worldgen/tests/fixtures/
                                              occupancy.csv
```

So the heavy tier is not merely *expensive*; it is **an authoring path for
committed artifacts, in the same class as census goldens**. "Run the heavy
tier on lefford" is a correctness rule that has been operating as an
unstated convention.

## 3. Decisions

Recorded here rather than in a scratch ledger, since the spec is the durable
record. `Q` entries were resolved from precedent per the campaign-autopilot
policy; `N` entries are Nathan's direct calls this session.

**N1 — one FCFS claim, no priority tiers.** Presented alongside a
lower-priority-lane variant and a semaphore-of-2 variant. Nathan chose
first-come-first-served, observing that a design needing to *differentiate* a
4-minute gate from a 40-minute one has a problem upstream of the lock.

**N2 — campaigns move to the Mac; lefford becomes a heavy-tier box.** The
differentiation N1 declined to build into the lock is achieved by
**placement** instead. Once the commit gate is off lefford, every remaining
holder of the claim is long, and 0081's "the queue is free for long jobs"
math applies cleanly — the starvation case is removed by construction rather
than by priority rules.

**N3 — the commit gate gets no host guard.** Convention only. It writes
nothing host-sensitive, and 0040 measured its ~4-minute budget on an M1 Max,
so the Mac is the gate's design target rather than a downgrade.

**Q1 — `heavy-run.sh` mirrors `census-run.sh` rather than extending it.**
Precedent: `census-run.sh` is 130 lines of census-specific naming, guards,
and claim semantics; the canonical-host check means something subtly
different for a golden-authoring run than for a test battery. Alternatives
discarded: a job argument on `census-run.sh` (conflation now, untangling
later); lock-only generalization with manual dispatch (leaves the
artifact-return path as tribal knowledge, a failure mode this repo has
recorded before). *Ideonomy: 1 pass, 0 overturns.*

**Q2 — censuses and the heavy tier share one claim.** Precedent: 0081,
"The binding constraint is the machine. There is one Lefford." Both
saturate the box; both are long. A second, independent lock would permit
exactly the oversubscription this campaign exists to remove.

**Q3 — `heavy-run.sh` carries the canonical-host guard.** Precedent: 0079
requires the host check at *every path that can write the committed
goldens*, because a wrong-host run does not error — it commits ~1-in-1000
wrong values that then drift-check green forever. §2 establishes that the
heavy tier is such a path. Scope limit stated in §6.

**Q4 — `make gate-fast` stays unlocked.** Precedent: 0081, "short jobs never
queue behind long ones." Making a scoped iteration run wait behind a
40-minute battery would be strictly worse than the contention it avoids.

*Ideonomy across the campaign: 2 passes, 0 overturns, 1 enrichment.* The
enrichment was an adaptive-admission variant (read `/proc/loadavg`; queue
only when the box is already hot) that the pass generated and then
eliminated: an uncontended `flock` acquire costs microseconds, so the fixed
rule is **already** free in the common case, and the sensor only adds a race
in which two runs both observe an idle box and both launch.

## 4. The design

### 4a. Placement rule (convention)

| Machine | Cores | Runs |
|---|---|---|
| MacBookPro | 10 | campaign worktrees, `make gate`, `make gate-fast`, `make rebaseline` |
| lefford | 40 | the heavy tier, censuses, and the artifacts they author |

Documented in root `CLAUDE.md` and `scripts/CLAUDE.md`. Not enforced (N3).

**Migrating the campaigns already on lefford.** At drafting time three live
campaigns (`the-watershed`, `the-wearing`, `the-winnowing`) have worktrees
under `/home/nathan/.config/superpowers/worktrees/hornvale/` on lefford — the
rule is not a no-op. Migration is per-campaign and must not interrupt work in
flight: a campaign moves at a plan-stage boundary by pushing its branch and
re-creating the worktree on the Mac, **never mid-measurement** (a
preregistered study's baseline and readout must see the same physics, and
here they would additionally see the same *host*). A campaign mid-measurement
finishes its readout on lefford first. Scratch ledgers under
`.superpowers/sdd/` are worktree-local and die with the worktree, so they are
promoted before teardown.

### 4b. One box claim

Generalize the census claim into a **heavy-job claim** that censuses and the
heavy tier both take. The existing claim-file fields (`pid`, `host`, `user`,
`started`, `goldens`, `label`, `ref`, `cmdline`), the 60-second waiter
heartbeat, the stale-PID takeover, and the bounded wait all carry over
unchanged; `label` gains a `heavy` value beside `census-run`.

`HV_CENSUS_LOCK_HELD` keeps doing its nesting job unchanged. Note that
`gate-full: gate` is a make **prerequisite**, so the gate half completes
before the heavy recipe body runs — two sequential acquisitions, never
nested, so no new deadlock surface is introduced.

### 4c. `scripts/heavy-run.sh`

Beat for beat with `census-run.sh`:

```
step  action                                        note
----  --------------------------------------------  -------------------------------
 1    `status` subcommand                           before the host guard: asking
                                                    is not authoring, legal anywhere
 2    canonical-host guard                          Q3
 3    acquire the shared claim (bounded wait)       announce holder + both clocks
 4    HV_HEAVY_REF=<ref> -> fetch, scratch worktree reuses census-run.sh's
                                                    worktree discipline
 5    run scripts/gate-full-heavy.sh                the existing roster discovery
 6    print the artifact diff                       review and commit ON lefford
 7    release on every exit path (trap)             a failed run never wedges the box
```

Wrapped by `make heavy-remote` for one-command dispatch from the Mac, and run
under `scripts/timed.sh` so it lands in `docs/timings.md` with the `waited_s`
column, exactly as a census does.

## 5. Non-goals

- **Not reversing 0081's advisory ruling for gates.** §1 explains why the
  gate-versus-gate case is distinct; the advisory behaviour against a
  *census* holder is unchanged.
- **Not touching `make gate-fast`** (Q4).
- **Not touching `scripts/aws-gate/`.** Abandoned by 0063; out of scope.
- **No domain, kernel, or window code.** No draws, no stream labels, no
  save-format surface, no epoch.

## 6. Assumptions requiring measurement

Stated as assumptions because they are **unverified**, and this repo's
recorded failure mode is confident prose about generated output that nobody
ran.

**A1 — the bare-cargo hole is left open.** Q3 guards the *wrapper*. A bare
`cargo nextest run --run-ignored only …` on the Mac still writes
`the-history` and `the-sounding` unguarded — precisely the hole 0079 had to
close in Rust for the census path. Deliberately deferred, because closing it
is only justified if A2 says it matters.

**A2 — whether `the-history` and `the-sounding` actually diverge Mac-vs-
lefford is UNMEASURED.** 0063 documents divergence for discrete-count metrics
in general; nobody has checked these two sweeps. The experiment that settles
it: regenerate both artifacts on each box from the same ref and diff. A clean
diff retires A1; a dirty one promotes it to a Rust-level guard at the write
seam, mirroring `census_guard.rs`.

**A3 — the heavy tier's uncontended runtime on lefford is UNKNOWN.** The only
datapoint is 39:09 under 3× contention. The wait-timeout constant therefore
ships as a placeholder inheriting the census default, to be revisited against
`docs/timings.md` once a quiet-box run exists — the same treatment 0081 gave
its own chosen constants.

## 7. Verification

- `shellcheck` clean on `heavy-run.sh` (`make shellcheck` covers
  `scripts/**`).
- **Mutual exclusion proven, not assumed:** two `heavy-run.sh` invocations
  started concurrently on lefford; the second must report the first as holder
  and start only after it exits. Asserting the lock exists is not the same as
  asserting it *excludes* — this repo has shipped tests that assert nothing.
- `status` returns truthfully from the Mac while lefford holds the claim, and
  never blocks.
- A killed holder releases the claim (`flock` frees the fd on process death);
  the next waiter proceeds.
- `make gate` on the Mac is green and within its ~4-minute budget on a quiet
  box.
- Heavy-tier artifacts regenerated via `heavy-run.sh` produce a **zero diff**
  against a clean tree — the run must not change what anything computes.

## 8. Definition of Done

- `scripts/heavy-run.sh`, `make heavy-remote`, shared claim, all verified per
  §7.
- A decision record ratifying the placement rule and the shared claim,
  refining 0081 (append-only; supersede, never edit).
- Root `CLAUDE.md` and `scripts/CLAUDE.md` state the placement rule.
- Chronicle entry (`book/src/chronicle/`) plus a freshness sweep of stale
  chapters.
- Retrospective in `docs/retrospectives/`.
- The three live lefford campaigns migrated per §4a, or explicitly recorded
  as deferred with the reason (mid-measurement).
- A2 and A3 recorded as follow-ups with the experiment that settles each.
