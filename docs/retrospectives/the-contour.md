# Retrospective — The Contour

One page of process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-contour.md): a second contest
axis, measured against decision 0096's keystone claim, and a preregistered
null that ships as the headline unsoftened.

## The headline lesson: four instances of one failure

**A claim frozen against an instrument nobody verified could carry it,**
four times in one campaign:

1. **Amendment 3.** M2 was specified to read community population "at bake
   end." There is no end-state population accessor anywhere in the data
   model — `Community.population` is bake-internal and discarded the moment
   `bake()` returns. Caught only because the implementer tried to build
   against the spec's own snippet and it did not compile against anything
   real.
2. **Amendment 4.** M4 was specified to read the bake's final-era connection
   graph and capacity map. Both are computed and discarded by
   `bake_history_from`. Caught the same way.
3. **The uninstrumented M1.** `cascade_sizes` — the metric *The Tumult* and
   *The Tithe* both reported — was never registered as a lab metric, and the
   spec inherited it as a continuity metric without checking that it was
   reachable through the instrument this campaign actually runs studies
   through.
4. **M3's missing half.** The spec asked for peoples-diversity in two
   halves — a count, and the effective-diversity reading `coexist.rs`
   already computes in space — and only the count was ever wired up as a
   registered metric. Neither the Task 4 brief nor the Task 4 review caught
   this, because both checked M3 against what was *built* rather than
   against what was *specified*. It surfaced only in an ideonomy pass on
   the finished readout, after the disappointing number was already in
   hand — which is exactly the moment a fix has the shape of metric-chasing
   even when the logic behind it is clean, so it was deferred rather than
   run.

All four share one root: **preregistering a metric's *behaviour* is not the
same as verifying its *inputs are reachable*.** The spec's own §4.4 says
this in almost so many words, after the fact — "a future spec in this
program should verify that each preregistered metric's inputs are
*reachable* before freezing the metric, and that check costs one grep" — and
that sentence should be read as a standing instruction for every campaign in
this sequence, not a note specific to this one. Amendments 1 and 2, by
contrast, were the calibration doing its job: a pre-measurement caught a
wrong assumption about the world's own data before any behaviour was built
on it, and neither followed a disappointing number. The distinction that
matters to a future reader is not the amendment count, which matched *The
Tithe*'s exactly, but whether the amendment was forced by a reachability
check that should have run earlier (worse) or was the calibration's own
discipline working as designed (fine). Two of each, here.

## Three decision-number collisions, all missed by preflight

This campaign minted two decisions. Both were renumbered more than once
before landing at 0096 and 0097 — one of them three times (0086 → 0089 → 0091
→ 0096), across two absorptions of `origin/main` at different points in the
campaign. `make preflight`'s ancestry check is mechanical and caught nothing
here, because it is a filename-and-content collision on a shared *numbering
sequence*, not a merge conflict on a shared *file* — two branches can each
independently pick decision 0091 for content that never otherwise touches the
same line of the same file, and git will merge both additions cleanly,
leaving two files claiming the same number. The fix each time was manual: grep
every citation of the colliding number across the tree, confirm (via
`git diff --name-only <merge-base> HEAD/origin/main`) which files belong to
which branch's campaign, and renumber. **The mechanised half of collision
detection (`make preflight`) does not cover decision numbering, and a
campaign that runs longer than one absorption cycle should expect to check
this by hand at every absorption, not assume the tool has it.** This is the
same shape of gap "Parallel campaign collisions" and "The Standing Offer"
already named for other kinds of semantic collision; decision numbering is a
third instance of the same class and is not yet on any checklist.

## Three investigations, three scratch instruments, three deletions

Three separate investigations in this campaign — the cold-built diagnosis,
its event-level root-cause trace, and the population-level endings
decomposition that overturned it — each wrote scratch bake instrumentation
from nothing, learned something real, and deleted the instrument before
reporting. None of the three findings is retrievable without re-deriving the
instrument that produced it. The structural reason is that this campaign's
own metrics (M1–M4) are **all outcome, no process**: they report what a
world *ended up as*, and the bake computes a much richer event history —
every raid attempted, every eviction, every cascade depth, every founding's
cause — and discards nearly all of it once occupation records close.
**When the same scratch instrument gets written three times in one
campaign, it wants to be a registered metric, not a fourth investigation's
throwaway test.** This is promoted below as a followup rather than fixed
here, because building it was out of this campaign's scope, but naming it
once was cheap and worth doing before the pattern needs a fourth instance to
notice.

## A census regen silently reverted a prose fix and desynced two calibration files

Independent of the above: `make gate` was found red at the start of Task 1
on `hornvale::docs_consistency::the_history_page_prose_names_the_cell_it_
renders`, which the dispatch expected. What was not expected is that the
canonical census regen commit (`4c46b45e`, landed on top of the merge
reconciliation) had **reverted** `book/src/gallery/history-seed-42.md`'s
hand-authored prose to a stale, pre-merge description while leaving the
generated render block it now disagrees with untouched — the exact fix the
merge-reconciliation pass had already made was silently undone by a
downstream commit that never touched that file's hand-authored half on
purpose. The same regen also left ten `calibration.rs` assertions and
nineteen `golden-pins.sql` rows disagreeing with the very `rows.csv` it had
just regenerated, because the regen's own commit message describes only the
census-specific artifacts it intended to touch, not the full set of
committed literals that read from the census fixture. **This is *The
Tithe*'s own retrospective lesson — "the golden-pin tripwire fires wider
than the tripwire reports" — recurring a third time, on a different census
regen, in a different campaign.** `make census-check` was reported clean
immediately after that regen commit because it was run *before* the
regen's own output was what the check would later be run against — a
sequencing gap, not a tooling defect, but one that a mechanical "run
census-check as the literal last step of any commit that touches
`rows.csv`" rule would have caught. Left as a followup rather than a
process-doc change here, because it is the second recorded instance of the
identical lesson and a third recording does not change the fix; what would
change it is making the check run automatically at the right point, which
is a tooling change outside this campaign's mandate.

## The crop-band witness needed a fresh sweep, not a re-pin

`the_independent_reading_covers_every_staple_worldgen_can_steep` pinned seed
5's bugbear as its witness — the one (seed, species) pair, out of a sweep of
seeds 0..20 and every placed people, whose settlements span all six crop
bands worldgen can steep. Position-aware conflict reseated settlements
across the board, and seed 5's bugbear no longer clears all six. The
temptation with a single-witness existence test is to hunt nearby seeds
until one works and call it a re-pin; the correct move, taken here, was to
re-run the *same diagnostic method* the original witness was found by — a
sweep over a wider seed range, every placed people read dynamically off the
live `FullView` rather than a hardcoded species list, so a future roster
change stays swept automatically — and report how the new witness (seed 83,
bugbear again, independently corroborated by seed 83's kobold clearing the
same six bands) was actually found, not just that a red test is green again.
A re-pin without a disclosed method is indistinguishable from a re-pin
chosen to make a test pass.

## The epoch's two survivors were opposite failures of the same kind

The epoch re-mint left exactly two tests red across 2780. Neither is a defect
in this campaign's mechanism, and the pair is worth recording together because
they are the two opposite corners of the triangle decision
[0097](../decisions/0097-assert-the-robust-half-measure-the-fragile-half.md)
names in its Consequences — *"both end in a green suite nobody believes."*

**One is a check that could never fire.**
`sky_exit_criterion::moons_flip_flips_..._without_displacing_the_head` extracts
its subject as `gods.split("\n\n").nth(1)`. In the rendered Gods section
paragraph 0 is the heading and **paragraph 1 is the settlement lead line**
(`windows/almanac/src/lib.rs:519`); deity blocks start at paragraph 2. A helper
named `head_belief` has therefore always compared a settlement *name string*
and never a deity. Measured at pre-Contour `main` — where the test **passes** —
the head deity is `Doodo the Vngadodo` (*wandering-star*) at `--moons 0` and
`Doodo the Vngoododo` (*eclipse*) at `--moons 3`, and the two pantheons (2
deities against 14) share no member at all. Both halves of the test's stated
claim — that moons never displace the head, and that moon count "only ADDs
deities" — are false, were false before this campaign, and were never checked.
What finally reddened it is not a leak but **toponymic wear**: the same entity
still heads the section in both arms (subject 7, cell 13980, 118 souls,
identical gloss), and only its rendered name moved, because The Wearing keys a
morpheme's wear to its own culture's corpus size and the moons pin moves that
corpus (bugbear settlements 111→136 pre-Contour, 26→4 here). The
non-pin-isolation is documented at `windows/worldgen/src/lib.rs:5602-5636`; the
test's apparent stability was a coincidence of corpus sizes, and the epoch
expired it.

The general lesson, and it is not about moons: **a test whose subject is
extracted positionally from rendered prose is one layout change away from
asserting something nobody chose.** This one survived years of green by
comparing the wrong paragraph. It also quietly disproves a premise several
tests may lean on — *the moons pin is not sky-local*. It moves total settlement
count (293→329 pre-Contour, 180→158 here), so "coarse constrains fine" does not
describe it.

**The other is a check that cries wolf.** The Tithe's
`concealment_moves_what_a_patron_collects...` guards its comparison with nine
structural-invariance assertions, on the stated ground that the arms "differ in
EXACTLY ONE input." They differ in exactly one input and not in outcomes:
concealment moves tribute, tribute moves population, population moves strength,
and strength enters the `RAID_MARGIN` comparison that decides a takeover. The
guard held at seed 42's old draws only because no takeover happened to sit
astride the margin. Widened to seeds 1..=100, **the fixture is structurally
invariant on 63 of 100 seeds**, and the surviving sign claim (insular <
expansive) holds on **77 of 100** — 0097's row three exactly, *a value pin's
noise profile with an invariant's authority*. It passes at pre-epoch
`e8c85d68` with the mechanism live, so the re-mint moved it, not the second
axis. And the second axis could not have: `full_land_graph` gives every edge
conductance 1.0, so all 240 ordered adjacent pairs evaluate to the single
constant `0.750001838` (`DEF_MIN`) — on that fixture defensibility is a uniform
25% discount on `RAID_MARGIN` and is arm-invariant by construction.

Two process notes worth keeping. First, **widening the sample cost 0.05 s**:
the fixture's construction is seed-independent and only `bake` takes a `Seed`,
so the 100-seed sweep that turned "this broke" into "this is a 63% property"
was nearly free. The reason nobody had run it is that nothing prompted the
question while the test was green. Second, **neither remedy was taken here.**
Both edit another campaign's shipped test, and this campaign had already set
that precedent by escalating the cold-built edit as a cross-campaign call;
0097's closing clause makes the choice among its sanctioned moves "the
claim-owner's," and the claim-owner is not The Contour.

## What went right

- **Preregistration held the readout to its own frozen conclusion even
  though the result was disappointing on every axis.** M3 moved down, not
  up; M2 stayed flat. Nothing in the readout tuned a constant or re-rolled a
  seed to chase a better number, and the one prediction that moved cleanly
  (M3) is reported as moving *against* the prediction, as the headline,
  exactly as the task brief that governs this whole sequence requires.
- **The ideonomy pass on the finished readout caught the M3-half gap before
  the campaign closed, not after.** Running that pass specifically *because*
  the result was disappointing, rather than accepting a clean-looking null
  at face value, is what surfaced the ceiling-saturation finding — the
  single sharpest thing this campaign produced — and it is a checkable
  discipline other campaigns closing on a null result should adopt as a
  standing step, not a one-off instinct.
- **A pre-specified fallback was checked against what it actually did before
  being executed**, and it was found to erase the exact distinction Amendment
  1 existed to express. §4.4a records the general lesson: a pre-specified
  fallback protects against metric-chasing and nothing else, and reads as
  more authoritative for having been written down first — which makes it
  *more* likely to be run unexamined, not less.

## Confidence Gradient

The sigmoid bet's rescore is folded directly into `book/src/open-questions.md`
rather than summarised here — see its own new section, "The sigmoid's first
axis is measured, and the null is in." One line for this file: the bet moves
from *unmeasured* to *measured on one axis and null there*, with the second,
headroom-bearing axis (effective diversity) still unmeasured rather than
merely undiscussed, and that distinction is now recorded precisely rather
than left as an undifferentiated "low confidence."

## Follow-ups

Carried from `.superpowers/sdd/2026-07-29-the-contour/followups.md` before
the worktree that holds it dies, per standing practice.

| | |
|---|---|
| **F21** | Register `cascade_sizes` as a lab metric. It already exists and is already computed; it is exactly the M1 this campaign could not adjudicate through the instrument, and its absence is why every comparison to *The Tumult* and *The Tithe* has had to be made by hand from their chronicles, three campaigns running. Cheapest of the observability gaps below; closes a hole this campaign hit directly. |
| **F22** | A bake process-census — raids attempted vs. resolved, evictions, cascade depths, foundings by cause — as registered lab metrics rather than three separate campaigns' scratch instruments. This campaign wrote the same shape of throwaway instrumentation three times (cold-built diagnosis, its root-cause trace, the endings decomposition) and deleted it each time; see "Three investigations, three scratch instruments, three deletions" above. |
| **F23** | Type `Founding` the way `ended_by` is typed. Endings are typed (decision-adjacent to SOC-casus-belli); foundings are not — `Founding` has only `Genesis(CellId)` and `From(EntityId)`, so a daughter settlement, a refounding after flight, and a seat taken by conquest are indistinguishable. A new variant on a committed record is a save-format change and wants its own epoch, or a ride on one already taking one. |
| **F24** | **For whichever campaign answers this chronicle next, as its first act:** build M3's missing half, effective diversity, and preregister it cleanly. `domains/demography/src/byproducts.rs::strife` already computes the reading; `coexist.rs` documents it at ≈2.4 in space at β = 2.0. The count metric this campaign shipped is ceiling-saturated (76.7% of baseline worlds already hold all five peoples), so the headline null is strong on "does not rescue from extinction" and blind on "does not improve diversity in surviving worlds." Deliberately not run inside this campaign — see the chronicle's own section on why — but the preregistration should declare its headroom in advance, which is the rule this campaign learned the hard way. |
| **F26** | **Repair `sky_exit_criterion::moons_flip_flips_..._without_displacing_the_head`** (cross-campaign — the sky arc owns it). Its subject extraction reads the settlement lead line, not a deity, so the claim in its comment has never been asserted and is false besides. Assert the robust half per 0097: the head settlement's *identity*, read from the ledger, which is immune to naming/wear drift by construction. Then correct the comment — moons **do** reseat the pantheon, and that is the sky working. State plainly that the replacement is still a one-seed claim: widening it is expensive because this is a CLI integration test that shells out to full world builds. |
| **F27** | **Re-instrument The Tithe's `concealment_moves_what_a_patron_collects...`** (cross-campaign — The Tithe owns it). Its structural-invariance guard is a 63/100 property and its sign claim a 77/100 property, both measured. The 0097-sanctioned moves are to convert them to census-measured rates or to widen in place; the fixture parameterises on seed and the full 100-seed sweep runs in 0.05 s under `--release`, so widening is nearly free and is a real strengthening rather than a re-pin. Explicitly *not* sanctioned: dropping `patronage_transfers` from the guard list to clear the red. |
| **F28** | **`full_land_graph` cannot exercise the second contest axis.** Its uniform conductance 1.0 collapses `defensibility` to the single constant `DEF_MIN` over every pair. Any future test meaning to exercise position-aware conflict on a synthetic fixture must vary conductance; on this one the mechanism is only a uniform 25% discount on `RAID_MARGIN`. Worth a line in `history_bake.rs`'s fixture docs so the next campaign does not mistake a green synthetic test for coverage of the axis. |
| **F25** | `make census-check`'s cleanliness is only as current as the moment it was last run relative to the census artifacts it checks. This campaign found it reported clean immediately after a census regen commit that had, in fact, desynced ten `calibration.rs` assertions and nineteen `golden-pins.sql` rows from the very `rows.csv` that regen produced — because the check had been run *before* that commit's own output existed, not after. This is the second recorded instance of *The Tithe*'s "the golden-pin tripwire fires wider than the tripwire reports" retrospective lesson. A mechanical rule — run `census-check` as the literal last step of any commit touching a committed `rows.csv`, or gate the commit on it directly — would close this at the tooling level rather than relying on a third campaign to notice by hand. |
