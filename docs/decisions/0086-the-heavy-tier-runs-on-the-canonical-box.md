# 0086. The heavy tier runs on the canonical box; campaigns run on the Mac

**Status:** Accepted (2026-07-29) · **Decider:** Nathan · **Refines:** [0081](0081-one-heavy-writer-per-box-claimed-at-the-write-seam.md), [0079](0079-census-goldens-are-authored-on-one-enforced-host.md)

Decision 0081 established one heavy writer per box and, in the same breath,
carved the gates out of it: `make gate` and `make gate-full` **advise rather
than block**, because "a developer waiting twelve minutes to START a
four-minute gate is a worse experience than the contention, and a gate is not
a measurement."

That carve-out was calibrated against a **census** holder. It never considered
gate-versus-gate.

On 2026-07-29 three campaign sessions, each living in its own worktree on the
canonical box, ran test suites concurrently. Load average reached **85.27 on
40 cores**. A commit gate budgeted at ~4 minutes was **at 14:56 and still
running** — roughly 3.7×, worse than the naive `3T` the aggregate model
predicts, because three-way oversubscription also pays context-switching,
memory pressure, and cache thrashing. Nothing was bypassed and no lock leaked:
the gates simply do not take the claim, exactly as 0081 specified.

## The ruling

**The heavy tier and censuses run on the canonical box. Campaigns and the
commit gate run on the Mac.**

| Machine | Cores | Runs |
|---|---|---|
| MacBookPro | 10 | campaign worktrees, `make gate`, `make gate-fast`, `make rebaseline` |
| lefford | 40 | the heavy tier, censuses, and the artifacts they author |

**Differentiation by placement, not by priority tiers.** An earlier draft gave
the heavy tier a separate, lower-priority lane so a 40-minute `gate-full` could
not starve a 4-minute `gate`. That was rejected: a design that needs to rank a
4-minute job against a 40-minute one on the same box has a problem upstream of
the lock. Once the commit gate is off the canonical box, **every remaining
claim holder is long**, 0081's "serialising two long jobs costs nothing in
aggregate" applies cleanly, and first-come-first-served has no starvation case
left to engineer around.

**This does not reverse 0081's advisory carve-out.** A gate contending with a
*census* still advises rather than blocks, for 0081's stated reason. What
changes is that the heavy tier is no longer treated as part of "the gates": it
is a heavy job, and it claims the box like one.

**One claim, shared.** Censuses and the heavy tier take the same claim, because
the binding constraint is the machine. There is one Lefford.

**The claim is taken at the seam, not the wrapper.** It lives in
`scripts/gate-full-heavy.sh`, so a direct `make gate-full` inherits it —
0081's own lesson, since a wrapper cannot guard a bare invocation.
`HV_CENSUS_LOCK_HELD` keeps `heavy-run.sh` from deadlocking against its own
child.

**The heavy tier is host-locked for correctness, not convenience.** This was
the campaign's real finding and had been operating as an unstated convention.
The tier is an **authoring path**: `cli/tests/history_battery.rs` writes
`book/src/laboratory/generated/the-history/`,
`windows/chronicle/tests/sounding_sweep.rs` writes `.../the-sounding/`, and
`windows/worldgen/tests/occupancy_readout.rs` writes
`tests/fixtures/occupancy.csv`. Further,
`census_fixtures_match_a_probe_of_live_seeds` compares a **live probe** against
census fixtures authored on the canonical box, and 0063 measured that two boxes
disagree by one unit on ~0.1% of discrete-count metrics — decided in the
compute path, upstream of quantize-at-emit ([0033](0033-serialized-floats-are-quantized-for-cross-platform-determinism.md)),
where nothing can absorb it. So `heavy-run.sh` carries the canonical-host guard
for the same reason a census does, per 0079.

**The commit gate gets no host guard.** Convention only. It writes nothing
host-sensitive, and 0040 measured its ~4-minute budget on an M1 Max, so the
Mac is the gate's design target rather than a downgrade. A guard here would
buy nothing and would break a legitimate debugging path.

## Consequences

- `make heavy-remote REF=<full-sha>` dispatches the tier from the Mac. A SHA,
  not a branch name: `HV_HEAVY_REF` feeds `reset --hard`, which can otherwise
  land on a stale local branch on the canonical box, so the script echoes the
  resolved HEAD.
- Artifacts the tier authors are reviewed and committed **on the canonical
  box**, the same flow census goldens already use.
- `flock` is util-linux and macOS ships none. `gate-full-heavy.sh` therefore
  proceeds **unserialised with a note** where there is no `flock`, rather than
  failing: campaigns run on the Mac and the tier runs on the canonical box, so
  an unserialised `gate-full` on a dev machine is the discouraged path, not the
  contended one. `scripts/test-heavy-lock.sh` skips for the same reason —
  a skip is honest; a green pass on a machine that never ran the assertions
  would not be.
- `status_line()` now names the job kind, so a heavy holder is not announced
  as a census.
- `require_canonical_census_host` takes an optional job kind for the refusal
  prose only. The host rule is identical either way.
- **Two constants remain chosen, not derived.** The wait timeout inherits the
  census's 2700s, and the heavy tier's uncontended runtime on the canonical box
  was unmeasured at ratification (the only datapoint, 39:09, was under 3×
  contention). Both should be revisited against `docs/timings.md`, exactly as
  0081 said of its own constants.
- Migration is per-campaign and **never mid-measurement**: a preregistered
  study's baseline and readout must see the same physics, and here they would
  additionally see the same host.

## Amendment (2026-07-29, same day, before merge)

Additive correction of a factual premise; the ruling above is unchanged.

The ruling cited 0040's "~4 minute" gate budget to argue that the Mac is the
gate's design target rather than a downgrade. **That figure no longer holds.**
Measured the same day on a quiet Mac: `make gate` ran **934.5 s** — 2548 tests
passed, 86 skipped — roughly 4× the documented budget. A contended run on the
same machine read 940.8 s, so contention accounted for six seconds; the suite
itself has simply grown. `docs/timings.md` carries **zero rows labelled
`gate`**, so the creep was never observable: the ledger built to catch a suite
creeping "65s → 43.5 min" was never wired to the most-run expensive command in
the repo.

The consequence for this decision is narrow but real. The **artifact-authorship**
justification is untouched — the heavy tier writes committed artifacts and
probes lefford-authored fixtures, and must run on the canonical box. The
**performance** half is weaker than the ruling implies: a quiet Mac gate
(934.5 s) is not clearly better than the contended lefford gate (14:56) that
prompted the campaign, and nobody has measured a gate on an idle lefford, so
the comparison that would settle it does not exist. Recorded here rather than
left to imply a benefit that has not been demonstrated.
