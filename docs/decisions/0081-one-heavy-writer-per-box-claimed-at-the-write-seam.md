# 0081. One heavy writer per box, claimed at the write seam

**Status:** Accepted (2026-07-27) · **Decider:** Nathan · **Refines:** [0063](0063-census-regen-is-local-again.md), [0079](0079-census-goldens-are-authored-on-one-enforced-host.md)

Decision 0063 made the census a local operation and 0079 enforced *which
machine* may author its goldens. Neither said anything about **how many runs
may proceed at once**, and the answer in practice was "as many as you start".

`scripts/census-run.sh` has always taken `/tmp/hv-census.lock`. But it is one
of three ways to write census goldens, and the other two — `HV_CENSUS=1 bash
scripts/regenerate-artifacts.sh` and `cargo run -p hornvale -- lab run
studies/the-census.study.json` — take no lock at all. The first of those is
the command `CLAUDE.md` itself named as "the sanctioned refresh", so the
documented path was the unguarded one. This is the same shape 0079 fixed for
the host check, which is enforced in shell *and* in Rust at `publish()`
precisely because a shell wrapper cannot guard a bare `cargo run`.

On 2026-07-27 two censuses ran concurrently on the canonical box for seven
minutes — one triggered over SSH from the other machine, one launched with
the documented command during a campaign close. Nothing broke: they wrote to
different trees, and determinism makes contention irrelevant to output. The
damage was to a *measurement*. The regen's census phase read 6m57s against a
clean 5m53s — an apparent 18% regression from a campaign that had just
measured a 24.3% speedup — and it was diagnosed only because an unrelated
`ps` happened to surface a second `hornvale` process at 2668% CPU.

## Decision

**One heavy writer at a time on the canonical box.** The claim is global, it
blocks rather than refusing, and it is taken at the write seam so that every
entry point inherits it.

**Global, not per-directory.** An earlier draft keyed the claim on the
goldens directory, reasoning that the corruption hazard (two publishers
interleaving into one directory) is per-directory and that runs in separate
trees are independent. They are independent in their *output* and not in
their *clock*: a census saturates ~38 of 40 cores, so two runs in different
trees still halve each other's throughput. The binding constraint is the
machine. There is one Lefford.

**Blocking, not refusing, with a bounded wait.** Serializing two long jobs
costs nothing in aggregate — contended, both finish around `2T`; serialized,
the first finishes at `T` and the second still at `2T` — so the queue is free
and the timings come out clean. The wait is bounded (45 min default,
`HV_CENSUS_WAIT_TIMEOUT` to override, exit 75) so a wedged holder fails
loudly instead of hanging forever, and a stale claim whose PID is no longer
alive is taken over rather than wedging the box.

**The line is drawn by cost, not by name.** A job claims the box when it
projects **≥ 200 world-builds** (`seeds.count × pin_sets.len()`, known before
the run — roughly 40 seconds of exclusive box time), or when it writes census
goldens at any size. Anything writing census goldens is covered regardless,
which keeps the correctness half independent of the cost threshold.

The cost rule is deliberately preferred over a name list (`the-census`,
`census-of-*`). A name list is precise today and **rots**: every future
census-scale study must remember to join it, and the one that forgets is
exactly the one that collides. A threshold derived from the physical property
the lock protects — machine time — covers a new 1000-seed study on the day it
is written, by nobody's diligence.

**Short jobs never queue behind long ones.** Below the threshold, contention
is a rounding error in both directions: a 10-second study finishes in 20
contended seconds, and steals a negligible slice of a 12-minute census.
Making it wait 12 minutes to do 10 seconds of work would be strictly worse
than the problem. For the same reason `make gate` and `make gate-full`
**advise rather than block** — they print that a census is running and that
timings will be contended. A developer waiting twelve minutes to *start* a
four-minute gate is a worse experience than the contention, and a gate is not
a measurement.

**Waiting must be legible.** Blocking makes the original failure worse unless
the wait explains itself — an unexplained pause on a 40-core box is exactly
what a hang looks like, and this whole decision exists because a contended
run and a normal run were indistinguishable. The claim file therefore carries
`pid`, `host`, `user`, `started`, `goldens`, `label`, `ref`, and `cmdline`;
waiters report the holder immediately, again every 60 seconds with both
clocks, on acquisition ("acquired after 7m03s"), on stale takeover, and on
timeout with a command to investigate. `docs/timings.md` gains a `waited_s`
column so a queued run is legible after the fact rather than mistaken for a
regression.

## Consequences

- `scripts/census-run.sh` remains the documented way to refresh census
  goldens; `CLAUDE.md` and the campaign-closing skill are corrected to name
  it instead of the unlocked `regenerate-artifacts.sh` invocation.
- Nesting is now a deadlock risk and is handled explicitly:
  `census-run.sh` → `regenerate-artifacts.sh` → `lab run` would otherwise
  block against its own ancestor. `HV_CENSUS_LOCK_HELD` names the holding
  PID; a live ancestor means "already serialized, proceed".
- The 200-build threshold and the 45-minute timeout are **chosen, not
  derived**. `docs/timings.md` has never carried a census row; once it does,
  both constants should be revisited against data.
- This does not change what any run computes. A census regen under the claim
  must still produce a zero diff.
