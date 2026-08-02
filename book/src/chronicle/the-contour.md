# The Contour

*The cheapest of three campaigns tested the keystone claim first, and the
keystone claim did not hold — a second contest axis does not, by itself, hold
diversity open in this world. The null ships as the headline, unsoftened, and
it comes with a sharper finding underneath it: the metric that was supposed to
adjudicate the question was specified as two halves and built as one, and the
half that shipped was already sitting at its ceiling in three worlds out of
four.*

*The Tumult* and *The Tithe* measured conflict deeply sub-critical, twice, and
named the term each was missing. Decision 0096 named a different gap in the
same mechanism: the deep-history bake settles every contest — a raid, a
subordination, a founding's survival — on one scalar, strength. Three
literatures agree on what that means. Ricardo: comparative advantage needs
more than one good, or the weaker party has nothing to trade. Chesson:
coexistence needs each competitor limited more by itself than by its rivals,
which a single shared limiting factor cannot supply. Ammann: a creature's
tactics come from the *contour* of its stat block, not its magnitude — weak
creatures do not win the strength contest, they decline to have it. **Weakness
is only absolute when there is one axis.** *The Contour* is the campaign that
adds a second one and asks whether that, alone, is enough.

## Why position, and why first

The sequence this campaign opens — *The Contour* → *The Appraisal* → *The
Deviation* — was designed so the cheapest test runs first. Position is
uncorrelated with strength by construction (a fact about terrain, not about
who holds the terrain), it costs no new authored data and no new seeded draw,
and if it moves nothing, the two costlier campaigns behind it — which do touch
species data — become much less attractive to run at all. That is the whole
shape of the bet: one campaign's price to learn whether the keystone claim is
worth the other two.

A cell's **defensibility** is how hard it is to come at, and everything it
needed already existed: `domains/topology`'s `Edge` already carries a
dimensionless conductance and an `EdgeKind` (`Adjacency`, `WaterRoute`,
`LandRoute`), and the bake already holds a per-era connection graph and
already walks it inside the raid scan. Defensibility multiplies the
*holder's* side of the dominance test at exactly two call sites — a raider's
candidate walk, and a homeless roller's search for ground — and touches
nothing else: not strength, not pressure, not tribute assessment. The same
discipline *The Tithe* applied when it kept tribute's store out of the
pressure term that kills.

## The calibration overturned the spec's own form before any behaviour existed

The spec's original form saturated defensibility from a cell's *aggregate*
approach ease — sum the conductance of every route in. Task 2's calibration,
scheduled deliberately ahead of any behavioural code, measured that aggregate
first and found it was not one distribution. Split by `EdgeKind`, two
completely disjoint physical regimes emerged: roughly 87% of cells reachable
only overland, at conductances clustered two orders of magnitude below the
remaining 13%, which are water-connected and cluster near the ceiling. The
two populations do not overlap — the global maximum of the land population sat
an order of magnitude below the threshold that separated the two groups. A
coastal cell is defensible against land and wide open to sea, and those two
facts are not one number to average.

So the mechanism was rewritten to read the **approach's own conductance**
rather than a cell aggregate — defensibility as a property of the route
travelled, not the destination — with parallel edges (a measured 6.7% of
cells carry an `Adjacency` and a `LandRoute` to the same neighbour) resolved
by maximum rather than summed, which fixed a double-count the original
aggregate form had been silently committing. This is Amendment 1, and it was
taken before any behavioural readout existed: nothing was being chased,
because there was no number yet to chase.

Amendment 2 followed the same pattern one step later. The transform's shape
constant, `DEF_SCALE`, was to be calibrated against the land population's
spread, with a **pre-specified fallback** written into the frozen spec in
case that spread came in too narrow: normalize each edge's cost within its
own `EdgeKind` before the saturating curve. The trigger fired — the measured
spread was 0.077 against a 0.10 floor — and the fallback was checked before
being taken, which is the only reason this is a record rather than a defect.
Within-kind normalization medians *every* kind at exactly 1.0 by construction,
which erases the water/land distinction Amendment 1 exists to express. A
pre-specified fallback protects against metric-chasing and does nothing at
all about being wrong, and it reads as more authoritative for having been
written down first — which makes it more likely to be executed unexamined,
not less. The replacement is a **centred tanh**: the pooled median maps to
exactly 1.0 (the median world is unchanged), water sits at the floor, land
grades across a spread five times the trigger, and both bounds are approached
but never required to be attained.

Amendments 1 and 2 are the calibration doing exactly what it exists to do:
catching a wrong assumption about the world's own data before any behaviour
was built on top of it. Amendments 3 and 4 are a different, worse story, and
the honest telling of this campaign has to keep the two apart.

## Two amendments the calibration could not have caught

The measurement plan named four metrics against a data model nobody had
checked could produce them. M2 was specified to read community population "at
bake end" — and there is no end-state population accessor anywhere in the
data model. `OccupationRecord::peak_population` is a high-water mark that
never falls, and `Community.population`, the only place a live figure exists,
is discarded the moment `bake()` returns. Exposing it would mean adding a
field to a committed record — a save-format change and an epoch, which the
spec had already named a non-goal. M2 was reworded to read peak population
instead, on its own merits (peak extent is the standard target in the
literature this metric is drawn from — Taagepera measures empire size at its
peak reach for exactly this reason, and an end-state snapshot catches every
polity mid-collapse) but it is still a reworded preregistered headline metric,
disclosed rather than settled quietly.

M4 hit the identical shape of problem from the opposite direction: specified
to read the bake's *own* final era, whose connection graph and capacity map
are computed and then discarded by `bake_history_from`. Present-day terrain
was substituted, labelled rather than silent, because the question M4 checks
— is defensible ground also poor ground — is a claim about the structure of
the geography, which present-day terrain samples fully even though it is not
literally the bake's last word.

Both amendments were pre-readout — no behavioural measurement existed at
either point, so nothing was being chased — and both are nonetheless a
different failure than the first two. The plan's own text warned, on the very
next page from where it made the mistake, that reachability needed verifying
by test and not assumed — and then assumed it anyway for M2 and M4. **A
future spec in this sequence should verify that every preregistered metric's
inputs are reachable before the metric is frozen, and that check costs one
grep.**

## What the world did

The readout is a matched pair against a frozen thirty-seed baseline, one
mechanism commit apart, with a hundred-seed run as an unpaired replication.

**M3 — the count of peoples alive at bake end — did not rise. It moved
fractionally down.** The entire thirty-seed delta is one seed losing one
people; every other seed's count is byte-identical to baseline, including the
exact identity of the six seeds that go to total extinction both before and
after. The mechanism rescued zero worlds from extinction and caused zero new
extinctions. **M2 — the entity-size distribution, the sigmoid wager's own
headline variable — stayed geometric.** Mean, median and interquartile range
sit within a few percent of baseline at both sample sizes; the one statistic
that moved cleanly in one direction (the ratio of the maximum to the median)
moves inside the band a single outlier seed can produce on its own, not the
signature of a distributional shift.

Both preregistered conditions for the spec's own named null are met, so its
stated conclusion is this campaign's headline, verbatim: **a second contest
axis, uncorrelated with the first and entering at the decision point, is not
sufficient to hold diversity open in this world.** That is a finding about
0096 clause 1's *chosen mechanism*, and it sends the sequence back to design
rather than forward to *The Appraisal*. It is not a reason to add a third
mechanism inside this campaign, and none was added.

## The null was measured on half the instrument that was specified

An ideonomy pass on the finished readout, run before the campaign closed,
found a defect in the measurement itself rather than in the mechanism. The
spec's §4.1 asked for M3 in two halves: "count of peoples with a live
community, **and the effective diversity** — the same reading `coexist.rs`
already uses in space, computed here in time." Only the count was ever
registered as a lab metric. The effective-diversity half was never built,
though the function that computes it already exists in
`domains/demography/src/byproducts.rs` and is already documented at roughly
2.4 in space, at the same weighting the spec calls for.

**A count measures presence; decision 0096 is about diversity, and the two
are not the same claim.** A world with five peoples where one holds 95% of
everything is a monoculture with survivors, and a bare count cannot
distinguish that world from one where all five hold real shares. Writing the
prediction in power-analysis notation exposes the slot this campaign left
empty: `peoples_alive` is discrete, bounded at five (the roster's own size),
and the baseline sits at that ceiling in **76.7%** of worlds already. "M3
rises" was close to unfalsifiable upward — in twenty-three of thirty seeds
the metric could not rise, because all five peoples were already alive there.

So the null this campaign ships decomposes into two claims of unequal
strength, and reporting them as one verdict — which the readout's own body
text does, before the addendum corrects it — overstates what was measured.
**"Does not rescue worlds from extinction" is strong**: six extinction seeds
at baseline, six live, the identical seed set, a detectable effect measured
at exactly zero. **"Does not improve diversity in surviving worlds" is
untested**, because the instrument that would answer it is saturated at its
ceiling in twenty-three of the twenty-four surviving worlds, and the metric
with headroom to see past that ceiling was specified and never wired up. This
is the campaign's sharpest process finding, and it is the fourth instance of
one pattern this campaign kept tripping over: **a claim frozen against an
instrument nobody verified could carry it** — amendment 3, amendment 4, the
uninstrumented M1, and now M3's missing half.

Building the effective-diversity half and re-running immediately would be
*executing* the preregistration rather than amending it, and that argument is
sound — but running a new measurement right after a disappointing one has the
shape of metric-chasing even when the logic behind it is clean, and *The
Tithe* already showed how a chain of individually-clean local justifications
adds up to something that reads as metric-chasing in aggregate. So the
effective-diversity reading is deferred to whichever campaign answers this
chapter next, with its own fresh preregistration, both branches made
informative, and its headroom declared **before** any code exists rather than
discovered after a disappointing number.

## M4 withdrew the frontier reading before the mechanism was even wired

M4 — the rank correlation between a cell's weakest-point defensibility and
its capacity — was adjudicated pre-mechanism, on the frozen baseline alone,
because it is a fact about the geography and does not need the raid rule
running to be measured. Mean **−0.020**, 95% confidence interval
**[−0.069, +0.029]**: centered on zero. The spec's §2.2 frontier hypothesis —
that defensible ground is also poor ground, so the strong expand onto rich
exposed cells while marginal defensible ground shelters whoever happens to be
on it — is **withdrawn**. In this geography, defensible ground is not, in
aggregate, poor ground. The spec had already flagged this as a hypothesis
about the world rather than an assumption the mechanism depended on, so the
withdrawal costs the campaign nothing structurally — the second-axis thesis
only needed defensibility to be uncorrelated with *strength*, which held —
but it does mean the "shelters the weak and drains the strong" reading that
made position feel like more than a modifier does not describe Hornvale's
actual terrain.

## A different campaign's invariant, moved by this one

`hearth_population_calibration::cold_built_settlements_are_common_not_rare`
asserts that at least one of fifteen sampled seeds is cold-**dominated** —
more than half its built rooms sitting below 5°C. Seed 13 was 107 of 188
rooms cold at baseline (56.9%) and is 97 of 199 with position-aware conflict
live (48.7%) — five rooms short of the bar, the only seed anywhere near it.
Widening the sweep to thirty seeds, doubling every sample-scaled threshold in
step, did not rescue the claim: the closest seed at thirty is still short,
and a second near-miss appears one room short of its own bar. This is a real
finding, not a sampling artifact.

An event-level trace (seed 4, the campaign's worst-case seed) found the
mechanism working exactly as specified rather than malfunctioning: position-
aware conflict raises the bar for taking held land, so fewer raids clear
`RAID_MARGIN`, so the whole eviction-and-refounding cascade that used to
terminate on cold, contested-by-nobody-else ground fires less often. Cold
land was disproportionately the *terminus of eviction chains* under the old
single-axis rule — a community loses a fight on good land, re-enters the
search, loses again, and the chain eventually bottoms out wherever nobody
else wants to fight over it, structurally often the coldest cells left.
Shortening those chains shrinks the population of last-resort refugees, which
is exactly the downstream shape of "settlement churn drops, and the
population pushed onto marginal refuge land shrinks with it" the spec
predicted in its own determinism section. A population-wide check ruled out
the simpler confound — a cell's own defensibility correlates with neither its
temperature nor its elevation at any usable scale (ρ ≈ +0.03, ρ ≈ −0.05, over
179,440 cells) — so this is not defensibility *targeting* cold ground, it is
a second-order consequence of the mechanism doing its first-order, intended
job.

A companion measurement then overturned the seed-4 story at the population
level, in exactly the shape this campaign kept re-learning to distrust: a
single seed does not generalize. Pooled over thirty seeds, every ending
category rose under the live mechanism — raid-evictions +10.2%, orderly
migration +3.9%, famine collapse +3.0%, total endings +6.3% — and the
alive-at-bake-end population moved down by less than one percent.
Defensibility is a **two-sided** multiplier, symmetric about the world's
median approach, and it eases some raids exactly as it hardens others; seed 4
happened to land on the hard side, and the aggregate lands on the easy one.
The cold-built decline is a **redistribution** of where the still-abundant
churn deposits its refugees, not a volume effect — displacement routes around
newly-defensible interior ground rather than there being less displacement
overall.

The cold-built invariant is left failing, deliberately, per this campaign's
own instruction not to paper over a long tail: it belongs to *The Hearth*,
which shipped it, and the fix — if one is wanted — is that campaign's call to
make. What this campaign contributes is the diagnosis and a companion
decision (0097) drawn directly from the experience: a claim's *fragility*,
not its subject, should decide which instrument enforces it. An existence
claim decided by whichever single world happens to sit nearest a threshold
carries a value pin's noise profile with an invariant's authority — the worst
available combination, because it fires when nothing about the underlying
physics is wrong, and its label discourages anyone from asking whether it
should have.

## σ, a third time

`cascade_sizes` — the instrument *The Tumult* and *The Tithe* both reported —
has no registered lab metric, confirmed again at this commit exactly as the
baseline recorded, so M1 could not be adjudicated through the instrument
proper. A hand-obtained reading, mirroring the prior two campaigns' own
scratch instrument exactly and cross-checked against *The Tithe*'s own
published figures (which it reproduces precisely), shows a real, modest,
directionally-consistent move: the branching ratio σ rose roughly 15% at
thirty seeds and 23–27% at a hundred, tracking a 7–10% rise in raid volume
that independently matches the endings-decomposition figure above almost
exactly. What did not move, across three campaigns and three mechanisms now,
is the **shape**: cascades occupy exactly two bins — size one, and size two
to three — with not one cascade of size four or larger in any of the roughly
2,500 pooled events this reading covers, at any sample size, under any of the
three mechanisms measured so far.

| campaign | mechanism | σ (pooled, 30 seeds) |
|---|---|---|
| *The Tumult* | predation (dissipation, no accumulation) | **≈ 0.051** |
| *The Tithe* | tribute (accumulation added) | **0.109–0.115** |
| *The Contour* | position (second contest axis added) | **0.125–0.133** |

σ has now moved in the same direction three campaigns running, and the shape
has never once changed. This is not this campaign's headline — M1 is
inherited and explicitly demoted in the spec, and the reading is
hand-obtained rather than instrument-verified, so it is reported with lower
confidence than M2 through M4 — but a reader relying only on "M2 and M3
stayed flat" would be overstating what stayed still. The mechanism is
measurably live; it simply was not enough of the right *kind* of something to
move diversity.

## What this campaign is, and is not

It ships defensibility as a per-approach, terrain-derived multiplier on the
holder's side of the dominance test, resolved from the connection graph with
no new authored data and no new seeded draw; a corrected calibration that
found the aggregate approach-ease statistic was two disjoint physical regimes
rather than one distribution, and a centred transform that grades land
without erasing the water/land distinction that mattered; a withdrawn
frontier hypothesis, measured pre-mechanism rather than assumed; a diagnosed,
not fixed, effect on a different campaign's shipped invariant, and a decision
(0097) about which instrument should own which kind of claim, drawn directly
from that diagnosis; and a preregistered, unsoftened null on the keystone
question the whole three-campaign sequence exists to answer.

It does **not** ship the effective-diversity half of M3, deliberately
deferred with its headroom declared in advance rather than run under this
campaign's own disappointing count. It does not ship a fix to
`hearth_population_calibration`, which belongs to the campaign that shipped
it. It does not touch species data, contour classification's `is-a`
derivation over already-authored vectors (specified but not built this
campaign, and left for its successor to pick up), or any of the mechanisms —
protection, chained tribute, collapse-release — the two campaigns before it
already deferred by name.

Position, alone, does not hold diversity open in Hornvale. That is the
keystone test the sequence exists to run, run first because it was the
cheapest, and it says: the next axis has to do something position does not.
