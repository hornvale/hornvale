# The Underworld

A geological column and a mine plan describe the same rock and share almost no
numbers. The column is a stack of named units — regolith, cover, basement — and
its boundaries fall wherever the rock changes, which is to say wherever
something happened a long time ago. The mine plan is a stack of *levels*, cut at
regular depths, and its boundaries fall wherever a person can work. Neither is
derivable from the other. A seam can outcrop at the surface and be worked at
four hundred metres; a level can pass through three units without noticing any
of them.

Hornvale had the column and was being asked the mine plan's question. `BandKind`
spaces its five rungs by rock-unit boundary — `Cover` at the soil depth,
`Basement` at soil plus sediment, `Roots` at half the crustal thickness — and a
chamber's habitation depth was read off it. The Delvers had authored two
subterranean dwarf kinds on that reading, found they could not be told apart,
and withdrew both before merge with the reason written into the source: *both
are defined by depth, and the model's elevation axis is metres above sea level.*
This campaign was the successor that was supposed to give the underworld its own
ladder and bring the two kinds back.

It built the ladder. It did not bring the kinds back, and it can now say
precisely why not.

## The design was falsified by its own first task

The plan was to space habitation rungs by **temperature offset** above the
surface — ΔT rather than depth — on the reasoning that the geothermal gradient
varies by cell, so the same rung would sit twice as deep under an ancient craton
as under young thin crust, and the chambers would spread out along it.

The first task measured the ladder before anything was built on it. Over seeds
42, 7 and 1234:

| seed | caves | [0,2) K | [2,10) | [10,25) | [25,50) | [50,∞) | `Roots` band |
|---|---|---|---|---|---|---|---|
| 42 | 874 | 655 | 6 | 4 | 1 | 208 | 208 |
| 7 | 1681 | 998 | 24 | 16 | 4 | 639 | 638 |
| 1234 | 1266 | 786 | 25 | 9 | 2 | 444 | 444 |

Two occupied classes out of five, the middle three holding between one and
twenty-five caves apiece, and the bottom class matching the `Roots` band count
essentially one for one on every seed. The heat ladder was not refining the band
ladder; it was reproducing it.

The arithmetic explaining that is one line and should have been done before the
design was written. ΔT = gradient × depth. The measured gradient spans **1.27×**
across the whole world (p10 21.795 → p90 27.780 K/km) while depth spans about
**10⁴×**. So ΔT is depth rescaled by a near-constant, and inherits depth's shape
exactly. The gradient is a rounding error on the quantity it was meant to
redistribute.

The prior diagnosis — that there is a thirteen-kilometre hole in the ladder —
was right as a *description* and wrong as a *cause*. Re-spacing rungs cannot
populate a hole that no chamber's depth ever lands in: `top_depth_m` of the
deepest band is effectively two-valued, because soil plus sediment is about zero
almost everywhere and the `Roots` top is about half the Moho depth. Any monotone
function of a two-valued input is two-valued.

The repair was not available inside the campaign's declared scope. It required
promoting a non-goal: **a cave's depth stops being a band and becomes a budget
in metres**, derived from void closure under lithostatic load with rock strength
interpolated across field grades, reading the cave kind and the lithology but
deliberately *not* the presence proneness — because that shared scalar is the
weld, and the presence gate and the depth budget want opposite calibrations.

The same buckets, after:

| seed | [0,2) | [2,10) | [10,25) | [25,50) | [50,∞) | modal share |
|---|---|---|---|---|---|---|
| 42 | 77 | 149 | 381 | 53 | 214 | 43.6% |
| 7 | 84 | 639 | 81 | 150 | 727 | 43.2% |
| 1234 | 91 | 162 | 348 | 129 | 536 | 42.3% |

The preregistered claim that the ladder varies failed before the repair — two
occupied rungs against a floor of four, and a 75% modal share against a 70%
ceiling — and passed after it, at 42–44% modal, without any threshold moving.
The criterion was written to catch exactly this shape, and it caught it.

## Where an edge goes, and where it may not

The shipped ladder is five rungs at ΔT thresholds of 0, 2, 8, 25 and 50 K:
`Undercroft`, `Shallows`, `Deeps`, `Underdeep`, `Sunless`. Three of the four
interior boundaries are the bins the probe was measured *against*, published in
the spec as an illustration before any measurement ran — an a-priori bin read
back, not an edge fitted and called a result.

The fourth moved, and the reason is a property of the distribution nobody had
looked for. The depth budget carries hard clamps — lava tubes at 200 m, the
general ceiling at 3 km — so cave reach has **atoms**, and with the gradient
spanning only 1.27× those atoms survive into ΔT. Seed 42's single fattest reach
value accounts for 23.1% of its caves. Re-binned at one kelvin, nine bins hold
essentially everything, and the bin `[10, 11)` alone holds **39.9% of seed 42**,
with 19.6% of that seed lying within half a kelvin of the old 10 K edge. The
edge was sitting on the fattest spike in the distribution, where a 0.7 K
perturbation would migrate a quarter of the population between rungs.

It moved to 8 K — into `[7, 9)`, which is empty on all three seeds — and the
rule stated for the move is that an edge goes inside a measured *valley*, never
at a percentile and never at a round number. The rule was then applied exactly
once, to the boundary that had been flagged. The two interior edges left alone
carry ±0.5 K masses of 0.0–0.2% (the 2 K edge) and 0.2–1.1% (the 25 K edge:
0.6% on seed 42, 1.1% on seed 7, 0.2% on seed 1234), an order of magnitude
under the 19.6% that condemned the third; moving an unflagged edge after seeing
the data is the metric-chasing that having a rule is supposed to prevent.

One edge is known to be badly placed and could not move. The habitable ceiling
at 50 K is an *authored* fidelity constant, frozen in the spec before the fit,
and the re-bin showed 1.5% of seed 1234 lying within ±0.5 K of it, with
occupied 1 K bins immediately below on two seeds — 48 **and** 49 K on seed 7,
49 K alone on seed 1234. (Seed 1234 supplies the 1.5%; seed 7 supplies the 48 K
bin. This paragraph used to attribute both to seed 1234.) Moving it after
unblinding would be retuning a frozen value. It is
recorded as an accepted cost of having authored it rather than left it as a
silence.

## The water table, and rock that cannot shed water

A cave column is vadose above the water table and phreatic below it, and the
preregistered claim was only that the split not be degenerate — neither under 5%
nor over 95% of cave-bearing columns wholly drowned. Measured: **31.9 / 43.6 /
41.5%**, and no constant was moved after the measurement.

Getting there corrected a category error of exactly the kind this campaign kept
finding. The relief half-point was set at 800 m — **Earth's mean land
elevation**, a real number answering a different question. Used as a vadose
thickness half-point it made a 2300 m massif drain 1.48× an 800 m upland where
karst hydrology says roughly 2.8×. Solved instead from its own datum, it is
8848 m, and the proportionality it was wrong about now has a test: 1.79 against
an ideal 2.0, where the retired value scores 1.26 and fails.

Then the structural finding, which is the more durable half. Even a **sixteenfold
departure from Earth calibration** leaves the `Sunless` rung dry in 0.0–0.7% of
columns. The lever saturates, and the cause is not calibration at all: columns
that reach `Sunless` have median porosity **0.056**, against 0.781 / 0.374 /
0.379 elsewhere, because depth reach rises with induration while porosity is
built as one minus induration. The two derivations are **anti-correlated by
construction** — the deep caves are in rock that cannot shed water. No scale
constant opens the deep. Only draining a chamber can, which is what a settled
people does to a working depth, and which is why the shipped rule makes a made
chamber dry by definition.

That conclusion carries its own disclosure, recorded rather than fixed. The
model uses *matrix* porosity as a conductivity proxy, and clay is the standing
counterexample: porous, and an aquitard. The columns in question have near-zero
carbonate, so they are fracture caves in metamorphic rock — precisely the
population where fracture permeability rules and matrix porosity is beside the
point. "Deep caves sit in rock that cannot shed water" may therefore be a fact
about the model rather than about the world, and it is captured as such.

## Chambers stop being constants, and nothing could see it

Before this campaign, a chamber's temperature simply *was* its surface
temperature and its moisture was a global constant. The ΔT between the
shallowest and deepest cave-reach deciles of a world was therefore **identically
0.0 K**, by construction, in every world — which makes the after-figure a
controlled result against an exact zero rather than an improvement over an
estimate. After: **0.7 → 56.0 K** on seed 42, 5.5 → 57.1 and 0.8 → 58.4
elsewhere. The count of cave columns carrying a distinct (temperature, moisture)
pair went 691 → 807, 1323 → 1483, 1030 → 1172.

And no live consumer could read any of it. The condition response floors its
buffer-able axes at the sovereignty floor and passes elevation a literal zero,
so a kind whose authored elevation devotion falls below that floor has elevation
as its limiting axis on every cell of every world, and every other axis is
discarded by the minimum before it is read. Drow's elevation devotion is 0.30
against a floor of 0.4248. The new chamber conditions could not reach its score
by any path — a fact derivable at that point in the campaign rather than
discoverable four tasks later, and it became a precondition on any kind the
campaign might author.

## One axis with no purchase

Twenty-two underworld communities were placed as points in The Axes' five-axis
environment basis — the same rulers, the same value grid, asserted off the
surface corpus's own scale so that a distance between a chamber and a surface
formation is a distance rather than a coincidence of units.

The preregistered prediction was that **light collapses**: at most two distinct
values across the whole underworld corpus. Measured, exactly two — `{0.0, 0.2}`,
the 0.2 carried by the four surface-breaching rows and 0.0 by the other
eighteen. The test asserts the floor as well as the ceiling, so it could not
have passed by collapsing to one value either. An axis that takes two values
over an entire realm is not a defect in the basis; it is the measured statement
that one of five questions has almost nothing to ask underground.

The same corpus evaluated a forward prediction The Axes had recorded and left
ungated: that this campaign's underworld communities would land in the cave
region of the space. **The weak form holds** — 9 of 22 nearest-neighbour hits,
40.9% against a 4.7% chance rate, a lift of **8.7×**. **The strong form fails,
and fails systematically.** Thirteen of twenty-two land nearest a hydrothermal
vent, a smoker field, or the deep pelagic ladder. The reason is structural and
is a genuine limit of the basis rather than an error in the assignment: the five
axes group places by *what they are like*, and carry no realm coordinate. A
dark, chemotrophic, wet, rocky chamber and a hydrothermal vent are the same kind
of place in axis terms. That is a better answer than a bare yes.

## The gate, and what closed it

Two candidate niches were authored, their fit tables and argmaxes recorded
before any world was built, and the preregistered criteria evaluated.

| | claim | verdict | measured |
|---|---|---|---|
| H1 | the ladder varies | **holds** | 5/5 rungs; top rung 31.3 / 27.9 / 26.8% (≤70%) |
| H2 floor | the modal rungs differ | **fails** | equal on seed 1234 (both `Undercroft`) |
| H2 overlap | ≥20% top-quartile overlap | **fails** | 7.3 / 16.4 / 16.5% |
| H2c | seating varies across cells | **holds** | mountain 2/2/2, duergar 3/4/4 distinct rungs |
| H3 | the water table is non-degenerate | **holds** | 31.9 / 43.6 / 41.5% phreatic |
| H4 | the axis-space prediction | **weak holds, strong fails** | 8.7× lift; 13/22 misplaced |
| H5 | light collapses | **holds** | exactly 2 values |

**The floor failed, so the two kinds are not authored.** That is the same
conclusion The Delvers reached, and the spec had said in advance that reaching
it from a measurement rather than from a merge is the cheap outcome, not the bad
one.

Two details make the null narrower and stranger than a bare fail.

The first is that the overlap statistic is **well-defined**. The spec had
guarded against a tie-dense capacity field, which would have made the quartile
boundary an artifact of sort order; the composed field carries 862–1579 distinct
values per kind and the boundary is a singleton on every seed, so the guard was
checked and not needed. The headline also uses the *generous* share statistic
rather than the pessimistic Jaccard (3.8 / 8.9 / 9.0%), with the quartile taken
over cave-bearing cells only, which inflates overlap. The failure is
conservative in every direction it could have been flattered.

The second is the attribution control, and it is the finding beneath the
finding. Capacity is a base field multiplied by this campaign's seating
multiplier, and only the second factor is new. Decomposed:

| seed | composed | condition niches alone | the delve multiplier alone |
|---|---|---|---|
| 42 | 7.3% | **0.5%** | undefined (4–5 distinct values; ties of 284 / 576) |
| 7 | 16.4% | **0.0%** | undefined (4 distinct; ties of 879 / 787) |
| 1234 | 16.5% | undefined (tie 12) | undefined (4–5 distinct; ties of 619 / 610) |

**The seating multiplier had no resolution at all** — four or five distinct
values with hundreds of cells tied at the quartile boundary, which is the
failure mode the spec's own guard predicted in as many words. What separated the
two candidates was the depth-routed *conditions* built earlier in the campaign,
and they separated them **nearly completely** — at 0.5% overlap on seed 42 and
0.0% on seed 7. **On seed 1234 the statistic is undefined**, its quartile
boundary falling inside a tie of 12, exactly as the table one line above
records; the honest range is therefore "0.0–0.5% on two seeds of three", never
a flat three-seed 0.0–0.5%. The multiplier's own overlap is undefined on *every*
seed, and this paragraph already says so — the asymmetry between how the two
were reported was the defect. Composing the multiplier on top *raises* overlap
toward 16%, pulling the two kinds slightly back together.

So the depth work succeeded and then separated the candidates into different
**peoples** rather than different **dwarves** — which is the third branch the
spec wrote before any of this was measured, describing it as "a failure that
would otherwise read as a spectacular success."

One clause of the criterion could not have been evaluated as written even had it
passed. The raid rule looks up only the raider's own rung, so two kinds at
different modal rungs cannot interact at all. Measured, that suppresses 86.5 /
92.1 / 88.6% of the co-seating columns. An overlap that *had* cleared 20% would
not have demonstrated one family; it would have shown shared ground between
peoples the raid gate keeps apart by construction.

## The string comparison that would have reversed the verdict

The fit function matched a cave's genus by comparing `CaveKind::name()` —
`"karst"`, `"lava-tube"`, `"fracture"` — against the corpus's own spellings,
which are `"karst-cave"`, `"lava-tube"` and `"fracture-cave"`. **One of the
three agreed by coincidence**, and that is why it survived: lava tubes matched,
the rule looked like it was working, and the guard that existed asked only
whether a fit *existed* — which the genus-blind fallback always supplies. Karst
and fracture columns were being scored against every community at their depth,
and returned bit-identical tables.

| | seed 42 | seed 7 | seed 1234 |
|---|---|---|---|
| overlap, broken join | 34.4% | 93.3% | 84.5% |
| overlap, repaired join | **7.3%** | **16.4%** | **16.5%** |

The broken instrument clears the 20% floor on every seed. The repaired one fails
it on every seed. The campaign came within one string comparison of authoring
two peoples on a measurement that meant nothing.

The replacement guard pins the class rather than the instance: a transposed
mapping would still match a corpus row and still keep the genera distinct, and
the old guards stayed green under exactly that mutation. The new one asserts
that each genus *extends its own cave kind's name*, plus pairwise distinctness,
and breaks under all five non-identity permutations.

That was the twelfth instrument this campaign found that looked like it was
measuring and was not, in twelve different places. A criterion satisfiable by
its own three-armed match. A `compile_fail` doctest pinning an error code
rustdoc never checks on stable — so the annotation reads as an assertion and is
documentation. A differential pair that could not catch a mutation on the one
line it existed to differ on. A mutation silently defeated by `cargo fmt`
rewrapping the line it was searching for, twice. A test selector matching
nothing and reporting `0 passed`. A shell idiom whose `grep -v grep` deleted
every hit, because the process being hunted was ri**pgrep**. A distinguishability
claim with no control. Five numbers written as measurements that were estimates
— of those five, one was in the campaign's own plan text rather than an
implementer's. And one further instance that was not a number at all: a
report's strongest framing repeated without anyone asking for its scope.

The best statement of the pattern came from the author of five of them,
summarising: *"numbers written while explaining, when the arithmetic felt too
small to run. The three that were caught were caught by running something; the
two that weren't were the two where I ran nothing. The countermeasure that works
is mechanical, not attentional."*

Which is why the durable output of that thread is not vigilance. It is a
mutation script that refuses to write unless its target text is found **and
unique**, and a standing preference for making a measurement re-runnable from
the tree over stating it correctly in a report.

## Two seams closed, one opened cleanly and priced

The campaign opened by naming three shipped seams with no producer — a niche
type with no consumer, a chamber origin with no writer, a depth-temperature
function with no caller — and framed closing all three as one finding rather
than three fixes.

Two are closed. The depth-temperature function has a live production consumer;
the niche type has one, wired to drow so that its closure did not depend on the
dwarves landing. The third is **not**, and saying it is would be the exact
defect this campaign is about. `ChamberOrigin` moved from *a seam with no
writer* to *a writer with no call site*: the producer exists, is correct, and
counts **7 / 25 / 42** made chambers across seeds 42 / 7 / 1234 — re-measured
2026-08-18 against the repaired genus join; the 29 / 14 / 15 first published
here was taken before that repair moved every seating — and nothing in the
shipped path calls it. The one production caller of the chamber lookup hands it
a freshly-constructed empty override map, so in every world a player can reach,
every chamber still resolves `Found`.

Wiring it was measured rather than argued about, and the measurement moved
underneath its own conclusion — which is worth recording, because the first
version of the disclosure was committed and then found false. At the time it was
written, every settled underworld column in all three seeds seated at the second
rung, and the descent verb enters at the first, so passing the real overrides in
would have handed an empty map to every measured world: the seam would have read
closed and been open. Repairing the genus join then moved drow, and the first
rung stopped being empty — 1, 1 and 15 columns, a clear majority on seed 1234.
The disclosure still stands, for a weaker reason: closing one band of five, with
no verb to reach the other four, is a partial closure presented as a whole one.
The close is priced instead — a descent verb, chamber state that tracks an
address rather than a single chamber, prose that tells a cut hall from a found
void, and a home for the overrides — as work for a client campaign rather than a
capacity task.

The campaign that set out to close three seams closes two and moves the third,
one level, cleanly.

## What did work

The ladder varies, on every seed, with no rung over 31.3%. Prefix truncation —
a column offering only the rungs its cave actually reaches — genuinely bites for
a kind authored deep: the deeper candidate seats at three or four distinct rungs
across the cells of a seed, against drow's constant one, and its modal rung on
one seed is **no formation's argmax**, meaning terrain produced an answer that
was not on offer from the authoring. That clause was added to the
preregistration before unblinding, precisely because a criterion satisfiable by
choosing niches measures the authoring rather than the world, and it is the
clause that earned its place.

A one-community-per-cell rule that decision 0102 had identified as an index
artifact — ratified twelve days earlier and never executed — is executed here.
The bake's node index is keyed on `(cell, rung)`, so an underworld community no
longer evicts whoever lives overhead. Surface density is unchanged structurally
rather than hopefully: the surface is one rung, so a cell still holds exactly one
surface community. That structural argument was corroborated by building twelve
species-pinned single-people worlds — four peoples across three seeds — before
and after the re-key and finding an order-sensitive record-stream digest
identical on all twelve. **The corroboration is a one-off comparison, not a
standing check.** `a_pinned_surface_people_builds_the_same_world` deliberately
*prints* its digests rather than asserting a committed literal, because the
comparison that matters is against the same command on the parent commit and a
literal re-pinned after the change would prove nothing; it is also `heavy:`-
gated, so no everyday gate runs it. Nothing re-checks the twelve worlds today.
The property itself does not need one — it is a consequence of the key's
shape — which is why the readout was written the way it was.

## The proof

The address space's meaning changed, so the chamber key takes an epoch —
`chamber/v1` → `chamber/v2` — and every seeded golden rebaselined. The census
was refreshed on the canonical box: 884.6 s, 77 files, **76 of 224 metric
distributions moved with the schema unchanged**. Settlement placement leads
(count 257.5 → 254.2, total population 7327.6 → 7251.4, tribute standing 84.2 →
81.5), chambers follow, and naming, religion and history move downstream of
which settlements survive to be named. Every shift is a few worlds in a thousand:
a re-placement, not a new mechanism.

One committed witness moved with it, and its non-movement is the finding. A
falsified-recall statistic whose pin forbids a silent integer bump read 72 of
120, having read 73 before and 68 before that — landing **exactly on** its own
preregistered 0.60 bar. At that bar the standard error over 120 pairs is 0.0447,
so the three readings sit at −0.745, +0.186 and 0.000 standard errors, across
three census epochs. Landing on the line is the most uninformative position
available. The verdict remains *cannot tell*, the instrument remains
underpowered, and the fix remains more pairs rather than a moved bar.

The dwarves are still owed. What is no longer owed is the reason: the underworld
now has a depth coordinate that varies with the thing that makes depth matter, a
water table that is not degenerate, chambers whose conditions differ from each
other and from the surface, and a settled people at depth. Two kinds still could
not be separated *by* depth — and the measurement says the separation that does
exist came from somewhere else entirely.
