# The Range

A range is the ground a creature actually occupies, as against the ground it
could tolerate. This campaign set out to give the model a way to say which
biomes a kind belongs in, and it began by discovering that the mechanism it
meant to extend could not reach a world at all.

## A gate that moved a readout and left the world alone

The preceding campaign had shipped a realm gate: a declaration that a kind
lives underground, enforced as a hard zero on the roughly eighty-eight percent
of land with no cave beneath it. It is the strongest selection mechanism the
habitat model has. It was mutated here as the first act of the campaign — one
line of source declaring `gnoll`, a peopled and settling kind, as
`Subterranean` — and seed 42 was rebuilt on either side of the edit.

```
                        readout                      committed world
                (per_species_suitability)            (seed 42, sha256/16)
  gnoll Surface   11010 / 11066 land = 99.49%        78256db156fb1c87
  gnoll Subterr.    622 / 11066 land =  5.62%        78256db156fb1c87
```

The readout moves by ninety-four points. The world moves by **zero bytes** —
the same hash, the same 1 738 950 bytes, the same 7 764 facts, the same
flagship village.

An empty diff is worth nothing without a positive control, so three were run
before that null was believed: the mutation asserted its target text was
present before substituting, so it could not have silently failed to apply; the
rebuilt binary's timestamp postdates the source edit, ruling out a stale build;
and the readout column is itself the control, since it proves the edit reached
compiled code and changed behaviour there.

The explanation is a single missing parameter. `per_species_suitability`, which
carries the gate, has exactly one caller outside the tests, and that caller is a
report — a demography index whose own comment says it is "never serialized,
never identity". The function that decides where settlements actually go takes
no realm argument and applies no realm gate. The two functions had not always
been different: the draft placer that once used the suitability path was retired
on 2026-07-20, and the realm gate was wired into that same now-orphaned path on
2026-08-06. **Seventeen days separate the retirement from the wiring.** Nobody
made a mistake at either end; the second change was correct against a codebase
that had stopped existing two and a half weeks earlier.

## Why no lint could fire, and the sharper point underneath

The compiler cannot help here, and the reason is worth stating precisely.
`per_species_suitability` is `pub` in a library crate, so `dead_code` never
applies — a public item is API surface by definition. And the function is not
dead in any case: it is genuinely called, on every world, by a report that runs.
*Is this item reachable?* and *does this item decide anything when the product
runs?* are different questions, and no lint in the language answers the second.

The sharper point is that the project's own discipline was followed and still
missed it. The peoples programme mandates that every campaign in it perform a
mutation step — a demonstration that the readout would report differently if the
axis moved — and grades that demonstration against a ladder whose top rung reads
"genuinely measured: **the readout differentiates the axis**". The preceding
campaign performed exactly that step. Its 99.49 % against 5.62 % *is* the
mandated demonstration, and it earns a perfect score.

So the ladder tops out one rung below the thing that matters. A mechanism can be
authorable, read, correctly coupled, and demonstrably differentiating, and still
never touch a world — and until this campaign the programme had no vocabulary in
which that sentence could be said. The ladder now carries a fifth rung,
*reaches world identity*: perturbing the axis changes the committed world. Rung
four remains necessary. It had simply been reading as sufficient.

## Biome, because the vocabulary already existed

A kind's fit in a cell is computed from four scalar tolerances — temperature,
moisture, insolation, elevation — and there is no biome among them. The species
domain does not name the `Biome` type anywhere; climate, travel, locale, render,
scene and the laboratory all consume it, and habitat never has.

Meanwhile the biome enumeration carries twenty-two variants —
`TemperateForest`, `Desert`, `Tundra`, `Ice`, `Taiga`, `CoralReef`,
`Epipelagic` — and those are the exact words the elf roster waiting behind this
campaign is specified in. Wood is temperate forest. Sea is ocean. Snow is
tundra.

Authoring those elves in the four-scalar vocabulary would mean fitting Gaussian
curves until their minimum happens to light up where a biome classification
already sits: authoring a **proxy for a thing the model already computes**. And
the model is not shy about biomes elsewhere. Travel already gates on them.
Marine forage productivity is read straight off the biome class — upwelling at
1.0, coral reef and kelp forest at 0.85, abyssal at 0.02. The omission was in
habitat alone.

## A mask, outside the minimum

Two design questions decided everything downstream, and both were settled by
reading the world rather than by argument.

**Where the factor multiplies.** It is applied *outside* the four-way minimum,
beside the realm mask, and not folded in as a fifth tolerance axis. That is a
claim rather than a style preference. The preceding campaign had measured that
going underground improves a kind's moisture and insolation readings and that
the minimum never sees the improvement, because an unfloored axis is scarcer
than either. Generalised: a non-lethal preference cannot matter while some
unfloored axis is scarcer. A biome affinity folded into the minimum would be
dominated in exactly the same way and would ship inert — the third pass of the
same defect, and the most elaborate no-op the project would have shipped.

**What its range is.** A read of the settlement bake, performed before any
mechanism was written, established that the contest is a per-people one: each
people ranks the whole map in its own units and takes its best ground, and
migration scores every candidate as capacity *for the rolling people*. A
downward-only mask therefore relocates rather than merely suppressing —
multiplying gnoll's non-desert cells down raises its desert cells within the
only ranking that decides anything. Two consequences fall straight out. A
**uniform** affinity is inert by construction, because a scale-free ranking
cannot be reordered by a constant; only the shape across biomes carries
information, and the level is gauge.

> **That whole sentence is false — both halves of it — and The Radiation
> falsified it on 2026-08-10.** It is left standing above rather than quietly
> rewritten, because it is what this campaign reasoned from and because it then
> survived a spec, a plan, a ledger and this chronicle without anyone having to
> say where it came from.
>
> **The first draft of this correction fell short in the same way the sentence
> did**, and that is worth recording next to it. It opened "that *last clause* is
> false", scoping itself to *the level is gauge* and leaving "a **uniform**
> affinity is inert by construction" standing, in bold, carrying a stronger modal
> ("by construction") than the clause it had just corrected. The uncorrected half
> has the wider blast radius, and it took a measurement rather than an argument
> to see it.
>
> **What is true, and all that is true:** a uniform affinity cannot reorder a
> *single kind's own ranking of cells*, which is exactly what the read of the
> settlement bake examined. Everything past that is false. The same factor
> multiplies the capacity that becomes a settlement's **population**, the
> founding pool filters on strictly positive capacity, and the history bake's
> volume is a function of population — so a level that is gauge for one kind's
> internal ranking is load-bearing for the world that ranking feeds.
>
> **Measured, on two arms that differ in nothing but the level.** Gnoll's row
> replaced by a genuinely uniform one — every biome at the same factor, no shape
> at all — and seed 42 rebuilt from scratch each time:
>
> | gnoll's affinity | facts | settlements | ruins | occupation records | total population | gnoll's own settlements |
> |---|---|---|---|---|---|---|
> | uniform **1.00** | 12461 | 212 | 477 | 689 | 6261 | 29 |
> | uniform **0.50** | 10758 | 194 | 374 | 568 | 5856 | 4 |
>
> A uniform `0.5` on **one** kind of thirty-nine removes 1,703 facts — 13.7% of
> the world — takes it from 212 settlements to 194 and 689 occupation records to
> 568, and takes gnoll itself from **29 settlements to 4**. Not inert; and not
> inert *for placement*, which is this clause's own stated justification.
>
> **The evidence this correction first offered was the wrong evidence for its own
> mechanism.** It cited the tithe series — 552 occupation records with no elf
> rows, **193** at the authored `0.25` level, **704** with the level derived from
> each kind's `sovereignty_floor` — which is the measurement that made the
> project re-derive the level, and is reported unchanged here for that reason.
> But that series moves a row's *contrast* as well as its level (the remap pins a
> stronghold at 1.00 in both arms and moves only the steps beneath it), so it
> cannot isolate what a *uniform* factor does. The two arms above hold the shape
> empty and move nothing but the level. Cite the arm that isolates the mechanism,
> not the arm that happens to be nearby.

And `0.0` is not a strong preference but a hard exclusion, because the founding
pool filters on strictly positive capacity.

The occupants declared are `gnoll` — desert 1.00, temperate grassland and
shrubland 0.70, savanna 0.45, everything else 0.25 — and `woolly-mammoth` —
ice and tundra 1.00, taiga 0.70, alpine 0.45, default 0.25.

> **Superseded, 2026-08-10 (The Radiation).** Those four numbers were a *shape*
> and a *level* run together, and the level was never derived — `0.25` reached
> this campaign from illustrative fixture code in its own plan. The shape
> stands; the level is now each kind's `sovereignty_floor`, with the rungs read
> as preferences and mapped `floor + (1 − floor) · p`. Gnoll's row therefore
> reads 1.00 / 0.848615 / 0.722461 / 0.495384 and the mammoth's 1.00 / 0.907710
> / 0.830802 / 0.692367. Every figure quoted below was measured under the old
> level and is kept as this campaign published it.
>
> **Two of those figures are not figures but interpretations, and both reverse.**
> A stale number is inert; a stale reading is not, because the next reader
> carries it forward as a property of the mechanism. The "nine settlements
> removed for every one relocated" caveat below is corrected in place where it
> appears, and this campaign's Confidence Gradient entry in
> `book/src/open-questions.md` — which scores the bet on a mask that "relocates
> rather than merely thinning" — is re-scored at The Radiation's close under
> decision 0030, not here.

Both were admitted by an arithmetic test rather than chosen by theme. A
tolerance floored by the kind's sovereignty can never read below that floor; the
unfloored elevation axis can never read above its own devotion. So a kind whose
elevation devotion sits below its sovereignty floor has elevation binding on
every cell of every world, and its climate curves are computed and then
discarded. Gnoll's floor is 0.495384 against an elevation devotion of 0.40; the
mammoth's is 0.692367 against 0.50. Both are below. That is what makes a biome
affinity an *addition* to these two kinds rather than a second helping of a
preference their climate curves already express — and it is asserted in a test,
so that a future edit to a mass or a devotion cannot silently turn a row into a
double count.

## Both predictions confirmed, which is the surprising part

Two predictions were frozen before the rows existed.

The first asked whether a mask **relocates** rather than merely thinning: the
arid share of gnoll's settlements must rise. The share was preregistered instead
of the count deliberately, because a falling count with a rising share is
success and both falling is the failure mode. The baseline was captured by
running the test red against an empty registry, so it failed on the prediction
and not on its setup:

```
  seed 42   affinity ABSENT    20 settlements   0 arid   share 0.000000
            affinity SHIPPED    2 settlements   1 arid   share 0.500000
```

Confirmed, and the count is the honest figure beside it: on seed 42 the affinity
removes nine gnoll settlements for every one it moves. Two further seeds were
run descriptively after unblinding and are kinder — seed 7 keeps all four of its
gnoll settlements and moves every one of them onto arid ground — but seed 42's
baseline of twenty is the outlier, and the collapse should not be read as the
mechanism's general behaviour.

> **The caveat does not survive, 2026-08-10 (The Radiation).** "Nine settlements
> removed for every one relocated" was a consequence of the undeviced `0.25`
> level, not of the mechanism. Under the derived level the arms read
>
> ```
>   seed 42   affinity ABSENT    13 settlements    0 arid   share 0.000000
>             affinity SHIPPED   40 settlements   33 arid   share 0.825000
> ```
>
> — the count **rises** 13 → 40 while the arid share rises to 0.825, where the
> published reading had it fall 20 → 2 for a share of 0.500. The descriptive
> seeds move the same way (seed 7: 67 → 31 at 0.645; seed 1234: 7 → 6 at 1.000).
> So P1″ passes far more strongly than it was frozen to, and the *thinning* this
> campaign apologised for was the level rather than the mask. The correction is
> pinned in `windows/worldgen/tests/range_readout.rs`, which carries the arms
> live; the paragraph above is left as published so that the two readings can be
> compared.

The second asked whether the factor **differentiates**: gnoll's capacity field
must separate from the other peoples'. Pairwise Pearson correlation over every
land cell, on the three seeds a previous campaign published:

```
  seed    mean pairwise r, gnoll vs the other eight peoples
          before     after      delta
    42    0.850738   0.795263   -0.055
     7    0.790098   0.705700   -0.084
  1234    0.857340   0.806109   -0.051

  all 24 individual pairs fell, on every seed
```

The instrument was checked against an independent reading before it was
believed: gnoll against kobold on seed 42 reads 0.291124 in the unaffinitied
arm, matching the previously published value to six places, so the two tables
are directly comparable.

**Both confirming is the finding, because the previous campaign established
that they need not.** That campaign measured a family of three whose one live
climate niche bought the *least* spatial distinctness of the three, while the
pair differing only in an elevation optimum separated cleanly — and named the
lesson: binding and differentiating are not the same property, and a kind can
win the first everywhere and still lose the second. The biome mask wins both.
It is the first mechanism in this model measured to do so, and it does it on an
axis outside the minimum, which is precisely the position that made it immune to
the domination that had silenced the realm gate.

The honest scope limits are two. Pearson is scale-invariant, so this measures
how the fields *sort* cells and not how large they are. And the falls are real
but modest: gnoll still correlates at 0.71 to 0.93 with most peoples afterwards.
Two readings survive that — the affinity may be a genuine but partial
differentiator, or the peoples may be so alike in their *surviving*
elevation-only tolerance that no biome factor can pull them far apart. This
measurement does not separate the two, and the earlier family result makes the
second worth taking seriously.

## One authored row redistributes the whole placement

Neither prediction asked what happens to everybody else, and the answer is much
larger than either measured effect. The world's total settlement count barely
moves; **who founds them** changes wholesale.

```
  seed 7    total 274 -> 287     gnoll  4 ->   4   (unchanged)
            bugbear 49 -> 153    goblin 48 ->  7    kobold 38 ->  5
            hill-dwarf 2 -> 28   desert-dwarf 51 -> 23
  seed 1    total 265 -> 184     gnoll 61 ->  13
            desert-dwarf 39 -> 91   hobgoblin 42 -> 3   kobold 79 -> 24
  seed 42   total 145 -> 143     gnoll 20 ->   2
```

Seed 7 is the arresting row: the kind whose affinity was declared does not move
at all, and bugbear triples.

Attribution is proven rather than argued. Two rows landed in one commit, and
"which row did that?" has no answer unless one of them is shown inert — so the
fauna occupant was chosen for exactly that property (it is gregarious, and the
bake's roster admits only settled kinds), and a test rebuilds each of three
seeds with and without its row and asserts the complete list of
(people, cell) placements is identical. It is, on all three. Every placement
movement in this campaign is gnoll's row.

**The mechanism behind the cascade is not established, and is deliberately not
narrated here.** A multi-era contest for ground plainly has room for one
people's suppression to free cells the rest contest, and there is a plausible
route through the raid comparison that ranks a weakened defender lower. That is
a hypothesis. It was not measured, and this project has recently mistaken one
difference for a cause. What is established is the magnitude: declaring one
kind's affinity is not a local edit, and a future occupant should expect to move
every people's numbers. Twenty tests across five crates reddened as witnesses of
the old world, and one standing sweep — the count of seed-and-people pairs that
independently witness every staple concept — fell from eleven qualifying pairs
to three, the first fall in that count's history.

## The claim that was right the whole time

The moving world falsified an unrelated campaign's calibration invariant: that
at least one of fifteen sampled seeds is *cold-dominated*, meaning over half its
built settlement rooms are cold enough that an interior would compose a hearth.
The best of the fifteen now read 46.4 %, against a bar of 50 %.

The reflex is to re-scope the invariant, and this campaign recommended exactly
that before checking. The decision log already held the answer. A ratified
decision — *assert the robust half in the gate; measure the fragile half in the
census* — prescribes converting an existence claim sitting on its threshold into
a census-measured rate, names **this very test** as its worked example, binds
any test a campaign has moved, and explicitly forecloses relaxing a threshold to
clear a red. The test's own doc comment said so four lines above the code that
was read. The decision's argument is that such a claim "carries a value pin's
noise profile with an invariant's authority, which is the worst available
combination: it fires when nothing is wrong, and its label discourages anyone
from asking whether it should have."

Converted, the answer inverts. Over the thousand-world census, **222 worlds are
cold-dominated — a rate of 22.2 %**. The band was fixed by a rule chosen before
the number was read (the measured rate plus or minus five binomial standard
errors, rounded outward to half a percentage point), giving `[15.5 %, 29.0 %]`,
with a pre-committed contingency to assert no positive lower bound had fewer
than thirty worlds qualified. Both mutations required of it were run: perturbing
the band fires it, and swapping the domination predicate from `>= 0.5` to
`> 0.0` sends the rate to 94.6 % — so the predicate discriminates rather than
reading the same under a broken definition. Fifty-four of the thousand worlds
are entirely temperate.

And the fifteen-seed sweep had been misread twice in the same direction. Its
panic message reports the highest cold **count** — seed 13 at 109 of 235 — while
the predicate it guards tests a **ratio**. By ratio the world nearest the bar is
seed 6 at 50 of 101: **49.5 %**. The claim missed by half a percentage point,
not by 3.6. That does not soften the diagnosis, it sharpens it into the cleanest
illustration the project has of what the decision describes — a claim decided by
whichever single world happens to sit nearest the threshold. Re-scoping the
threshold, which this campaign proposed, would have weakened a bound that was
correct.

## Two thousand worlds, twice

The census was refreshed twice on the one host permitted to author it: once for
the moved world, once for the new column.

The first refresh rewrote **1000 of 1000 rows** of the settlement census, which
is what re-deciding the settlement contest always does, and its schema changed
by exactly one line — its own content hash. No column was added, no metric
documentation moved, so the wholesale rewrite is content rather than text. Its
companion study was **byte-unchanged, all thousand rows**: that study's rosters
are synthetic solo and twin worlds where a new affinity never competes, so it
moves only on a column change and this pass added none. The programme's budget
had assumed two wholesale fixture rewrites; one was paid.

Eleven calibration witnesses were re-pinned to the regenerated values, and every
directional claim each of them encodes was re-checked rather than accepted. None
inverted. Blind attribution still beats chance at 0.907 against a floor of 0.75;
name transparency rose to 0.752 while its minimum *fell* to 0.165, widening the
span from below, which is what rules out a relapse into the uniformity defect
that number exists to watch. One real finding fell out of the sweep: a declared
biome range now blocks goblin's flagship settlement from placing at all on **2
of the 1000 seeds**, which had carried one on every seed since the roster last
grew — and that single cause reconciles four separately-moving witness counts.

The second refresh added the cold-built column, and with it the pairing the
converted claim needed: a census column without a verifier scores as unchecked
no matter how large its sample.

## What the readout finally showed

The committed occupancy readout — a per-kind, per-biome measurement of where
each of the roster's kinds actually lives, and the fixture the elves will be
authored against — had been regenerating with an all-absent affinity slice. Its
drift check was green because nothing had moved, not because nothing should
have. Threaded with the live store, it moves exactly where the mechanism says
and nowhere else: of 386 rows, **24 changed — twelve gnoll, twelve
woolly-mammoth — and 362 are byte-identical**.

Two internal consistencies confirm it is the affinity and not some other drift.
A `1.00` stronghold leaves the absolute capacity alone and moves only the share:
gnoll's desert mean, median and 95th percentile are unchanged, while its share
of the kind's occupied ground rises from 0.0052262188 to 0.017680314, a factor
of 3.38 — the biome did not improve, everything else was scaled down beneath it.
And the eight biomes taking the `0.25` default fall together by an identical
factor of 0.846, holding their order to eight figures, which is the gauge
property of a uniform mask visible in the artifact.

> **Third correction, 2026-08-10 (The Radiation), and it is the same reading as
> the first two.** *Order preserved to eight figures* is true and is all that was
> measured. Calling it "the gauge property of a uniform mask" restates, two
> hundred lines below the erratum that dismantles it, the claim that a uniform
> factor is inert — and it does so with the superseded `0.25` as its subject. It
> is gauge for the order of one kind's own cells, which is what the artifact
> shows; it is not gauge for the magnitudes, and the magnitudes are what the
> founding pool and the history bake read. Two measured arms are in the first
> erratum above.

## What this leaves

**The mask is a ratchet, and that is stated rather than solved.** A
downward-only factor composes multiplicatively with every future preference
layer, so each campaign that adds one makes the world monotonically emptier.
No single campaign observes it; the sum is a slow drain. The alternative — an
affinity permitted above 1.0 — was pre-committed as the repair path if the mask
failed to relocate. It did not fail, so the boost was not taken, and taking it
later is a decision to record rather than a literal to edit. The ceiling is now
asserted in a test, on the default as well as on every named override, because
the floor at zero had been guarded and the ceiling at one had not.

**The elves are the point of all this and are not in it.** Six of them are
waiting, and they will now be authored in a vocabulary the model computes rather
than fitted as curves approximating it.

> They landed in [The Radiation](./the-radiation.md), all six on that route and
> on no other, taking the settling roster to fifteen. That campaign is also where
> the level was derived and where the corrections above were measured.

**One test proves less than its name suggests, and says so.** The check that an
absent affinity is a no-op compares the absent branch against an explicit
unity-valued one and additionally requires a non-uniform arm to differ from
both; the second clause is what makes the first capable of failing. It remains a
*relative* check. Were both resolution sites broken to the same wrong constant,
it would still pass. That is thin given how little arithmetic sits between them,
but it is real, and it is written at the test rather than left for a later
reader to assume otherwise.
