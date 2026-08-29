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

## Amendment (2026-08-28, The Governor)

Additive correction of a factual premise; the ruling above is unchanged.

The ruling's own text (above) counted three tests writing committed
artifacts: `cli/tests/history_battery.rs`, `windows/chronicle/tests/
sounding_sweep.rs`, and `windows/worldgen/tests/occupancy_readout.rs`. The
Governor's heavy-tier adjudication (`docs/audits/heavy-tier-adjudication.md`)
demoted `sounding_sweep::run_the_sounding_and_write_the_report` out of the
`heavy:` tier: its preregistered coupling-exponent hypotheses are printed to
`book/src/laboratory/generated/the-sounding/`, never asserted, the same
"report" shape as the campaign's other demotions. **The count of tests that
write committed artifacts is now two, not three**, and the guard's other
justification — `census_fixtures_match_a_probe_of_live_seeds` comparing a
live probe against lefford-authored fixtures — is untouched.

**A real cost this demotion carries, named rather than left silent.**
`scripts/regenerate-artifacts.sh` does not write `the-sounding`; the demoted
test was the tree's only producer of it, including
`book/src/laboratory/generated/the-sounding/sample-biographies.txt` (148 KB,
byte-deterministic), which [0087](0087-a-benchmarks-timings-are-a-record-not-a-golden.md)
*deliberately* kept under the strict drift check when it excluded
`rows.csv`/`summary.md`'s timing columns, specifically because it is "the
file that would catch a real regression in what The Sounding computes."
`book/src/laboratory/` remains declared in `docs/generated-paths.txt` and
`sample-biographies.txt` remains under its drift check, but nothing in any
automated path (`make rebaseline`, a stage gate, a merge) regenerates it any
longer — the test that did so is now `run by hand` only. The file is not
excluded from the check; it is orphaned from the thing that would keep it
current. This does not weaken the drift check's honesty (a stale committed
file still diffs against a fresh run and fails), but it does mean nobody
notices unless someone runs `sounding_sweep::run_the_sounding_and_write_the_report`
by hand and diffs the result.

## Amendment (2026-08-28, The Governor, fix round 1)

Additive correction of a factual premise **in the amendment immediately
above, not in the original ruling**, which the previous amendment already
left unchanged and this one leaves unchanged too.

**The previous amendment's corrected count — two — was itself wrong. It
should have been one.** That amendment reasoned at the *file* level
(`windows/worldgen/tests/occupancy_readout.rs` is a file the original ruling
named as a writer) rather than checking, per test, which function in that
file actually carries the `heavy:` tag and which function performs the
filesystem write. They are not the same function:

- `occupancy_readout::occupancy_readout_is_current` is the one `heavy:`-tagged
  test in that file (the one The Governor demoted). It only ever **compares**
  a freshly rendered readout against the committed `tests/fixtures/
  occupancy.csv`, byte for byte (`assert_eq!(rendered, committed, …)`). It
  never writes anything.
- `occupancy_readout::regenerate_occupancy_readout`, the actual writer
  (`std::fs::write(FIXTURE_PATH, &body)`), carries its own `#[ignore]` reason
  and was **never** `heavy:` — its preceding comment says so outright:
  "Deliberately NOT a `heavy:` reason. The heavy tier is what `make gate-full`
  runs, and this test WRITES the fixture — running it there would have CI
  silently rewrite the artifact the drift check above exists to check."

So `occupancy.csv` never belonged in either amendment's writer count. Verified
directly rather than re-trusting the previous count: every file that still
carries a surviving `heavy:` tag after this campaign's demotions was
enumerated and grepped for a filesystem write inside its `heavy:`-tagged
test bodies. Exactly one write to a committed path was found —
`history_battery::history_gates_full_world_and_cross_seed`'s
`std::fs::write` calls into `book/src/laboratory/generated/the-history/`.
Every other write found in the tier's surviving tests (in
`underworld_per_rung_switch.rs`, `anomaly_holdout.rs`, and
`windows/lab/src/runner.rs`'s test module) either lives in a non-`heavy:`
sibling function or writes to a scratch/temp directory the test itself
cleans up, never a committed path.

**The corrected count: one test writes a committed artifact
(`history_battery::history_gates_full_world_and_cross_seed`, writing
`book/src/laboratory/generated/the-history/`), and one compares a live probe
against lefford-authored fixtures
(`fixture_staleness::census_fixtures_match_a_probe_of_live_seeds`, per the
original ruling, untouched by any of this).** The guard's justification is
otherwise exactly as the ruling above states it.

**A second orphaning, the same shape as the first amendment's, recorded
here because it was named at length for `sample-biographies.txt` and not for
its twin.** `windows/worldgen/tests/fixtures/occupancy.csv` is **not**
declared in `docs/generated-paths.txt` — it was never part of the drift-check
regime `sample-biographies.txt` sits under — but `occupancy_readout_is_current`
was nonetheless its only *automated* witness: the one test in the tree that
ever compared the committed fixture against a live re-render and could fail
if the two disagreed. Demoting it out of `heavy:` does not change what
verdict was correct (the adjudication table's DEMOTE stands — the test's own
failure message already reads as the report-branch instruction "rewrite the
fixture in the SAME commit as the change that drifted it"), but it does mean
nobody runs that comparison automatically any more; catching drift in
`occupancy.csv` now depends entirely on a human remembering to run
`occupancy_readout_is_current` by hand. Named here so the cost is visible,
not silent, the same standard the first amendment applied to
`sample-biographies.txt`.

## Amendment (2026-08-28, The Governor, final whole-branch review)

**The amendment immediately above named a cost and accepted it; on review the
cost is not acceptable, so the underlying verdict was reversed instead.** The
ruling above and the corrected writer count are both untouched — this changes
only which tests carry a `heavy:` tag, not what the canonical-host guard is
for.

`occupancy_readout::occupancy_readout_is_current` is **restored to `heavy:`**.
The Governor's adjudication had demoted it on the strength of its failure
message reading as the report branch ("rewrite the fixture in the SAME commit
as the change that drifted it"), but that argument applies word for word to
`fixture_staleness::census_fixtures_match_a_probe_of_live_seeds`, which the
same adjudication KEPT and which
[0426](0426-the-heavy-tier-is-a-phase-of-the-queue-again.md) builds a section
on. It is an exact `assert_eq!` of a live render against a committed byte
golden — a change detector, not a pinned historical number — and, as the
paragraph above established, `windows/worldgen/tests/fixtures/occupancy.csv`
is under no drift check at all, so it was the artifact's only automated
witness of any kind. `docs/audits/heavy-tier-adjudication.md` carries the
flipped row and the corrected totals.

**The count of tests that write committed artifacts is still one**, exactly as
the previous amendment derived it, and for exactly the reason that amendment
gave: the restored test only ever *compares*; the writer in that file,
`regenerate_occupancy_readout`, was never `heavy:` and still is not. The
`sample-biographies.txt` orphaning named in the first amendment is unaffected
and remains open — `sounding_sweep::run_the_sounding_and_write_the_report`
stays demoted.
