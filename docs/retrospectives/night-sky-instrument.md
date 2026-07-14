# The Night Sky Instrument — retrospective

**Completed:** 2026-07-14 (name-only designation per decision
`slugs-not-numbers`; plan
`docs/superpowers/plans/2026-07-13-night-sky-instrument.md`)

**A brief's own code can contradict its own prose, and only a targeted
test catches it.** Task 3's brief defined the pole star as a system-level
fact — identical from every latitude, since it is declaration ("this star
sits near a pole"), not observation — but the code it shipped computed
`pole_star` *inside* the visibility partition, so the `continue` on the
never-rises branch silently skipped the pole-star check for
opposite-hemisphere stars. The bug was invisible to every test the task
had written, because none of them queried the same system from two
latitudes. The fix (commit `814d178`) hoisted pole-star selection into its
own pass over all neighbors, independent of the visibility loop, and added
`pole_star_is_latitude_independent` as the pinning test. The generalizable
point: when a brief states an invariant in prose ("X is independent of Y"),
the review pass should look for a test that varies Y and holds X constant —
a battery that never varies the one axis the invariant is about will pass
right through a violation of it.

**A degenerate-input guard is a different finding than a logic bug, and
review is where it surfaces.** Task 10's figure-clustering centroid divided
by a normalized-mean vector's length unconditionally; a near-zero sum
(members nearly surrounding the sphere) would produce NaN and let it
propagate silently into the centroid, the region word, and the ecliptic
flag. Nothing in the task's own test suite exercised this — the frozen
calibration constants make it practically unreachable (mean cluster degree
~1.8 against a ~4.5 percolation threshold) — so it shipped clean through
`make gate` and was caught only by a review pass explicitly looking for
unguarded division. The lesson repeats a shape this project keeps
finding: a gate that runs the tests a task wrote can only catch what that
task thought to test; an unreachable-in-practice edge case still needs an
explicit guard, because "practically unreachable" is not "provably
unreachable."

**Two wrong-checkout near-misses, both caught by the dispatch preamble,
neither landing in a commit.** Task 2's first edit attempt wrote to the
main checkout instead of the worktree — the implementer had copied a file
path from the brief (which correctly names paths relative to the main
repo for *reading* the brief) without re-deriving it against the worktree
root — caught when a stale test result didn't match what had just been
written, confirmed via `git branch --show-current` on the target path,
reverted, redone. Task 7 repeated the same trap from the harness's default
Bash working directory rather than a copy-paste error. Both were caught
before commit by the preamble's pre-commit `pwd && git branch
--show-current` check, and both required zero cleanup on `main` beyond a
`git checkout --` of the one touched file. The preamble is doing exactly
the job it was written for; the trap itself remains a standing hazard of
subagents dispatched into worktrees, not something this campaign found a
way to eliminate.

**Honest documentation beat a silent behavioral change when a modeling
assumption broke.** Task 8 discovered that `wanderer-class` (a bare
two-valued Text predicate: rock or giant) dedupes on the ledger's
exact-fact collision rule whenever two pinned wanderers share a class —
which is *likely*, since every inner wanderer is always Rock. Rather than
redesign the predicate under task pressure, the fix documented the
characteristic precisely on the constant, the genesis function, and the
task report, and flagged it for the controller rather than silently
shipping a fact count that doesn't match the wanderer count. The census-
lag red (32 pre-existing `hornvale-lab` calibration failures, inherited
from main and reverified unchanged at every subsequent task by diffing
against the same baseline) got the identical treatment: named once,
verified stable, never quietly absorbed into "everything's green."

**A deviation from a plan's literal wording is fine when it's adjudicated
and recorded once, not repeated as silent drift.** The plan called for a
single combined almanac sentence for all wanderers; Task 8 shipped one
line per wanderer instead, matching the `NightSkyLines`/heliacal
`Vec<String>` precedent already in the codebase. The deviation was
adjudicated against the existing pattern, recorded in the plan file at the
exact step it diverged from, and never repeated without note in Tasks 9 or
10. A plan is a hypothesis about the code, not a contract with it; the
discipline that matters is naming the departure once, at the point it
happens, rather than letting later tasks quietly assume the plan's
original wording was followed.
