# The Adit — retrospective

**Merged:** 2026-08-19

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-adit.md): a recursive
partition-tree level generator for the underworld, two determinism
defects caught before implementation began, and two connectivity defects
caught during it — one a gap in the plan's own scaffold, one in an
algorithm that had already passed two task reviews.

## 1. Every real defect was in my own plan text, and the pattern that caught it generalized

Both determinism defects (§ chronicle) were found the same way: tracing a
cited precedent against the actual draws, one task ahead of dispatch,
rather than trusting that citing `lattice::allocate`/`lattice::grow`'s
discipline in a doc comment meant the code actually followed it. Neither
would have compiled to anything visibly wrong — both would have produced
*plausible*, *deterministic*, *wrong* output: identical content on two
different leaves, correlated levels across a descent. Nothing red, nothing
that fails a test that doesn't specifically look for it.

The two connectivity defects were the same shape one level later — a
composite level with a permanent wall between its regions, an algorithm
that reliably fragments — except caught by execution rather than by
pre-flight, because neither is visible from reading a task's own
`Consumes`/`Produces` block against its own signatures. They needed a real
connectivity check to exist before they could be found, and the plan did
not schedule one until Task 8, the second-to-last task.

**The generalizable half:** a pre-flight scan catches defects visible from
signatures. It cannot catch defects visible only from *running the
generator and checking the property it claims to have*. Both classes bit
this campaign; only the first had a scheduled instrument. A future
campaign building a generator with an emergent property (connectivity,
reachability, any invariant that isn't checkable from a type signature)
should schedule the property-check task **early**, not as integration
testing at the end — Task 8 landing second-to-last is exactly why the
CellularCave gap survived two prior task reviews unnoticed.

## 2. An instrument that samples one branch of its own input space reads as thorough and is not

Task 9's connectivity sweep looked complete — 20 seeds, a real BFS, a
positive control for single-leaf levels. It hardcoded one `CaveKind` out
of three. Nobody wrote that down as a limitation because nobody was asked
to; the sweep passed, the property it named held for everything it
actually sampled, and the two algorithms it never touched shipped
unmodified through the review that approved it.

This is not "test more seeds." Widening the seed range inside
`CaveKind::Fracture` would never have found the CellularCave gap, because
the missing dimension was which *algorithm* ran, not which *seed*. The
transferable check: when a sweep asserts a property "for every X," name
every value X can actually take in production and confirm the sweep's own
loop reaches all of them — not just that the loop has many iterations.

## 3. The final whole-branch review found what ten task reviews, correctly scoped, could not

Every one of the ten task reviews did its job — verified its own task
against its own brief, correctly, including two that independently traced
subtle correctness properties (post-order recursion, stream-draw ordering
before an early return) rather than trusting the implementer's narration.
None of them was positioned to notice that Task 9's sweep sampled one
`CaveKind` of three, because that observation requires holding the whole
campaign's algorithm roster in view at once, which a task-scoped review
correctly does not do.

The final review found it, plus a real (if currently unreachable) panic
in `carve_tunneler` on a degenerate rect, plus a weakened assertion
(`<=` instead of `==`) in the stairs-collision test that would have let a
reversed-write-order variant of an already-fixed defect back in silently.
None of the three is something any single task's own scope would have
surfaced. This is the argument for the final review existing as a
separate pass rather than "the last task review" — it is checking a
different thing.

## 4. A process mistake, named plainly: this closing work should have happened before the sluice

This document, the chronicle, and the registry flips below were all
written *after* `make sluice` had already landed The Adit on `main`. The
project's own Definition of Done (CLAUDE.md Process; the
`closing-a-campaign` skill's own step ordering) says the chronicle and
retrospective are DoD artifacts to land *on the branch, before merging* —
step 3 of the walk, step 6 is the sluice submission. Nathan caught this
after the merge landed.

The mechanism was simple: `closing-a-campaign` is listed among this
project's skills, its own trigger line reads "before declaring the
campaign done, removing its worktree, or writing its final summary," and
it was never invoked. `submitting-to-the-sluice` was invoked instead,
because the immediate question in front of the controller at that moment
was "how do I get this merged," and that question has its own
correctly-named skill sitting right next to the one that should have been
asked first. Recorded in memory
(`chronicle-and-retro-before-sluice.md`) for every campaign after this
one: closing a campaign is not "merge, then clean up" — the chronicle and
retrospective are preconditions for the merge, not follow-up tasks to it.

## What held up well

**The pre-flight scan, and the discipline of tracing a cited precedent
rather than trusting the citation, caught two real defects before any
implementer touched the code.** Both shipped with regression tests
proving the specific failure mode, not just the restructuring.

**Every task reviewer verified claims against the diff rather than the
implementer's narration**, including two cases (Task 4's visibility
widening, Task 7's collision-safety fix) where the reviewer reproduced the
implementer's reasoning independently — in one case in a scratch crate —
rather than accepting a plausible-sounding explanation.

**An implementer stopped and reported BLOCKED rather than working around
a defect its own fix exposed.** Task 8's second attempt found the
CellularCave gap while implementing a real connectivity check and reported
it as new information rather than weakening the check to make the test
pass — which is exactly the discipline the project's own process culture
asks for and does not always get.

**No generated-artifact drift survived to close.** Every stream label
this campaign added drifted the type-audit report, the stream manifest,
and the CLI golden roster; every task that added one regenerated and
committed the drift in the same commit. `make rebaseline`'s post-absorb
run moved nothing this campaign had already accounted for.

## Follow-ups

**F-1 — `MAP-underworld-chart` stays open.** This campaign resolves the
geometry prerequisite; the chart itself (retiring
`the_underground_band_folds_into_walk_as_map_does`, wiring a real `map`
verb) is The Delving's next campaign.

**F-2 — the stairs pairing between adjacent rungs is not established.**
`place_connections` picks a `StairsDown`/`StairsUp` cell independently per
level, with no positional correspondence between rung *i*'s down-stairs
and rung *i+1*'s up-stairs. Spec §4.6's "a matching connection point" is
ambiguous enough that this is a defensible reading, but the campaign that
wires movement will need the pairing and should not assume it exists.

**F-3 — a handful of small deferred nits, none blocking, consolidated
rather than itemized per-task** (full detail in each task's review,
recoverable from the merge commit range if needed): `generate_level_
extent`'s rung-scaling formula has no dedicated test; a few tests carry a
`claim: rate(seed: single)` tag on a non-looping assertion, which the
`claim_shape` vocabulary doesn't really have a shape for; `carve.rs`'s
`find_components`/`connect_components_within` BFS pattern is duplicated
(with reason) across two test modules; the widened `CaveKind`x
`ChamberOrigin`/`Algorithm` sweeps (§2 above) are hand-enumerated array
literals that would not fail to compile if a future campaign adds a new
variant to any of the three enums.

**F-4 — `MAP-underworld-vaults`, `TOOL-underworld-embedder-unification`,
`MAP-pattern-language-settlements`** — three ideas captured during
brainstorming (authored/seed-selected vault content; whether this
scaffold and the building-interior embedder should eventually share a
framework; the wider Christopher-Alexander pattern-language vision for
inhabited-space generation spanning underworld and settlements) — all
`raw`, all deliberately deferred, none touched by this campaign's
implementation.
