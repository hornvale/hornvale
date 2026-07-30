# The Siding

At 15:15 on 29 July, Lefford's load average was **85.27** on forty cores.
Three campaign sessions were running test suites at once, each in its own
worktree, each unaware of the others. A commit gate budgeted at four minutes
had been running for **fourteen minutes and fifty-six seconds** and was not
finished. Nothing was broken. No lock had leaked and no guard had been
bypassed — the gates do not take the box's claim, deliberately, because
decision 0081 ruled that a developer waiting twelve minutes to *start* a
four-minute gate is worse than the contention.

That ruling was calibrated against a census holder. It had never considered
a gate waiting on another gate, where the wait is four minutes rather than
twelve — and where 0081's own arithmetic, that serialising two long jobs costs
nothing in aggregate, argues the other way.

## The fix was placement, not priority

The first design gave the heavy tier a lower-priority lane so that a
forty-minute `gate-full` could not starve a four-minute `gate`. Nathan
rejected it in one sentence: a design that needs to *rank* a four-minute job
against a forty-minute one on the same box has a problem upstream of the lock.

So the differentiation moved out of the lock and into the machine. Campaign
worktrees and the commit gate run on the Mac; the heavy tier and the censuses
run on Lefford. Once the gate is off the canonical box every remaining claim
holder is long, 0081's aggregate arithmetic applies cleanly, and
first-come-first-served has no starvation case left to engineer around. The
lock became trivial instead of clever. That is decision 0086.

## The tier was already host-locked; nobody had written it down

Investigating turned up something that outranked the scheduling problem. The
heavy tier is not merely expensive — it is an **authoring path**.
`history_battery` writes `the-history`, `sounding_sweep` writes
`the-sounding`, `occupancy_readout` writes `occupancy.csv`, and
`census_fixtures_match_a_probe_of_live_seeds` compares a live probe against
census fixtures authored on one enforced host. Three committed artifacts and
one host-sensitive comparison, governed by convention alone.

The campaign assumed this meant the two boxes disagreed, and set out to
measure it. They do not. Regenerated at the same ref on both machines,
`the-history` came back byte-identical, and `the-sounding`'s deterministic
columns matched across all twenty-seven rows. The placement rule survives on
artifact authorship and machine contention — a narrower justification than the
campaign started with, and the honest one.

## A benchmark is not a golden

`the-sounding` did diff, on both boxes. Every `peak_bytes,events` pair
appeared exactly twice in that diff, once removed and once added: the
deterministic columns had not moved at all. What moved were `bake_ns`,
`read_ns_per_op`, `replay_ns`, and the exponents fitted from them — 2.14 to
1.95, thirteen-fold to fourteen-fold.

The Sounding does not diverge across hosts. It diverges across **runs**,
because it records wall-clock time, and it had been sitting inside the
strictly drift-checked `book/src/laboratory/` tree the whole while. It never
fired because `make rebaseline` does not run the heavy tier and the heavy tier
is not followed by a drift check: the one command that rewrites the file and
the one command that would notice have never run together. Decision 0087
reclassifies it — the timings are a record, the biographies stay checked.

## What the contention was hiding

Running the tier on a quiet box, finally, produced two failures. One was
`scene_api_cost_is_bounded_on_seed_42`, complaining that genesis took
19,722 ms against a 13,000 ms ceiling. On an idle machine the same test reads
**3,818 ms** and passes — a factor of 5.2, and a timing-budget test on a
shared box producing false red is precisely the harm this campaign exists to
remove.

The other was real. `census_fixtures_match_a_probe_of_live_seeds` had been
failing on main, invisible to `make gate` because the heavy tier is
`#[ignore]`d out of it. The committed census was missing three columns and had
drifted on twenty-one more — `name-length-goblin` from 17.8 to 10.45,
`name-collision-rate` from 0.038 to 0.575. Not noise: The Wearing had made
name shape a per-culture drawn distribution and the nucleus a template set,
added three new readings, and then written this into a merge message at its
close:

> `00ed687b merge: absorb origin/main at the close, deferring the census regen`

One hundred and thirty-nine commits ago. The deferral was recorded, unowned,
and never paid off — the documented rule that a pin is re-pinned in the commit
that drifts it, failing in exactly the manner the rule anticipates. The regen
is paid off now, on the canonical box, as its own change attributed to the
campaign that caused it. Four calibration pins remain red, and were left that
way: they encode The Wearing's measured values, and accepting them is a claim
about that campaign's physics rather than a mechanical rebaseline.

## The shape underneath

Four separate failures in one session, and one shape beneath all of them: **a
check that exists but never runs in the configuration where it would fire.**
The census, anchored and unobserved. The Sounding's timings, checked by a
command that never sees them. And two checks this campaign wrote into its own
spec and believed without executing — a zero-diff over a tree containing
nanosecond timings, which could never have passed, and a claim-status probe
that answered "no" because the claim lives in each machine's own `/tmp`.

Both of those were caught the same way: by running them against reality
instead of reasoning about them. The deferred regen is the same failure in
social form — recorded, unowned, never triggered.
