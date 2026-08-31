# The Winze

*A winze is a shaft driven downward from inside a working. It is the part of a
mine that goes looking.*

**August 2026 · outcome: merged — a settlement is founded to reach ore, digs a
committed depth, and carries a per-metre chance of breaking through into
something; the delvings that broke through are the deep ones, and the world
does not know what they found**

## Three mechanisms died before this one, and all three died of the same error

The keystone was Nathan's, stated in a sentence: *a delving stops where it
finds something. What it found is legible only to the people it killed, and
only for as long as anyone remembers.* Depth neither guarantees nor precludes a
wound — **a place is deep because a wound stopped the digging there.** That
inversion is what makes the design cheap: nothing has to place wounds at depth,
because depth and breach are both downstream of the same act.

Getting there cost three designs. The first proposed a wound as a property of
a cell, gated on ancient crust; measured across three seeds it produced 0, 0
and 36 candidate cells, and **zero** occupations standing over one. The second
proposed a wound as the deepest terminus of a fraction of caves, selected by
rank — better, because it dropped an absolute threshold on a distribution
nobody had measured, but still geometry: it placed wounds *by depth*, which is
the causality running backwards. The third was this campaign's own opening
move: derive `Mine` from ore on the settlements a world already has.

**All three are the same error.** A derivation cannot reclassify a population
that was placed for a different objective. Every settlement in Hornvale is
sited by capacity-maximising agrarian logic, and the first task measured what
that logic does with ore:

```text
prospectivity = 0.6·setting + 0.3·unrest + 0.1·metamorphic_grade
                setting = 0.1 off a plate boundary, 0.4–0.7 on one

seed 42 land:  min 0.0600  p50 0.0602  p75 0.0667  p90 0.1442  max 0.6771
seed  7 land:  min 0.0600  p50 0.0623  p75 0.1191  p90 0.2113  max 0.7398

share of occupations falling in land's own top prospectivity decile
  0.89%   1.98%   8.86%        against the ~10% chance alone would give
```

(Those are readings of the world **before** this campaign; the last section
returns to them.)

Two facts, and the second is the one that killed the design. Prospectivity is a
**near-constant floor with a thin tail** — off a plate boundary the setting term
pins three quarters of seed 42's land inside a band 0.0067 wide, so there is
almost nothing to separate on. And settlements are **under-represented** in
high-ore ground rather than merely indifferent to it. That is the model being
*correct*: ore concentrates on plate boundaries and tectonic unrest, farmland
does not, and agrarian siting has already put the settlements where the ore is
not. Relabelling them cannot work, and a model that made it work would be a
worse model.

**Reclassification, where creation was needed** — three times, before anyone
wrote the word down.

## A mine is founded, not relabelled

The settlement bake already spawns daughters, and it already scores candidate
sites — today by capacity times a river factor, one objective. The change is a
**second objective inside one existing choice**: an expansion is occasionally a
*working*, scored by ore rather than by fertility, opening with `Function::Mine`
and a parent that supplies it. A mining camp is exactly that, and the founding
path it needs was already there.

Genesis is untouched, which bounds the blast radius: the most byte-identity-
sensitive path in the bake is never entered.

The first cut of this shipped and measured **1, 13 and 2 mines** on the panel
seeds — one mining camp in 1,240 settlements on seed 42. The rate was not the
constraint and neither was the ore threshold; what bound it was that a daughter
looks only at its parent's direct neighbours, and ore is rarely one hop from
farmland. That is the first task's finding arriving one hop out and biting the
design built to answer it.

So a working **searches outward**, by a ring scan bounded at three rings, and
the argument is from the objective rather than from the count. One hop is right
for an agrarian daughter — a village spreads to the next field, and the next
field is next. A working is founded *because of where the ore is*, so the thing
it is searching for must set its radius. The two objectives had been sharing a
radius only because they were sharing a code path. There is precedent in the
same file: relocation already resolves its destination by a ring scan, and a
working is closer in kind to a relocation than to a daughter — in both, a people
moves to reach something specific rather than spilling into adjacent room.

The bound is real and was argued rather than assumed. Unbounded, the scan
self-reinforces: a working on an ore belt is surrounded by ore, so its own
daughters are workings, and the settlement network migrates onto low-capacity
plate-boundary land — measured at 118 / 74 / 8 mines with pooled occupations
down 20.4% and a founding ring reaching 38.

One consequence is recorded rather than smoothed over. A sea route is one hop
to the bake and up to twenty ocean vertices to the geography, so three rings can
chain three lanes: ordinary daughters cross a lane 0.7% of the time and workings
do so **20%** of the time. Every individual instance is precedented — water
routes have always been traversable for every siting decision the bake makes,
because traversability filters on conductance and not on edge kind — but the
*rate* is new, and the world now has a visible class of overseas workings where
before it had a curiosity. Two thousand kilometres of open water is a better
supply line than three hundred of mountain, and the conductance model is what
says so.

## The clock is the whole mechanism

Each increment of delving carries a small probability of breaking through, and
a delving that breaches **stops, and ends**. Nothing selects on depth; the
survivorship shape is supposed to fall out.

**The design never said what an increment is, and that omission was very nearly
fatal to the measurement.** Every other rate in the settlement bake is per
epoch, which is the obvious reading. It is also the one reading under which the
campaign's central question cannot be asked at all.

Breach and the ordinary ends are competing risks in time. With a constant
per-epoch breach probability `p` and a constant per-epoch ordinary-end
probability `q`, the time to *any* end is geometric in `p + q`, and given that
a delving ended at epoch `t`, the probability it ended by breaching is

```text
    p / (p + q)      — independent of t
```

The two groups therefore share a tenure distribution exactly. Depth accrues
with tenure. So the two groups share a depth distribution, and the
preregistered null — *breached and ordinary are indistinguishable, therefore
the mechanism is decoration* — would have fired **as a theorem**, on a
mechanism that had never been given a chance to produce the effect. The
measurement would have been an engineered result published as a discovery.

Clocked per **metre**, it works:

```text
    P(breach this epoch) = 1 − exp(−metres_cut_this_epoch / 3000 m)
```

Integrated over a working's life that is `1 − exp(−depth / 3000)`, an
exponential in depth — while the rule evaluated at any single epoch reads only
the metres cut *that* epoch, a purely local quantity. Two workings cutting
100 m this epoch face the identical hazard whether one stands at 50 m and the
other at 3,000 m. Depth appears nowhere in the rule.

This is the nearest the campaign comes to the threshold design it replaced, and
the difference is exactly Nathan's inversion. A threshold *places* wounds at
depth: depth is the cause. Here digging is the cause, and depth and breach are
both its consequences. The correlation between them is the mechanism working,
not a rule reading a depth.

The constant is read off terrain rather than chosen: 3,000 m is the declared
depth of the world's void-bearing crust, so what the number encodes is *a
working that cuts through the entire void-bearing column has, on average, found
something.* Survival across that whole window is `1/e ≈ 37%`; an
ordinary-depth working is under 1%.

**The general form outlives the campaign.** When a preregistered comparison is
between two sub-populations of one process, the clock the process runs on can
decide the answer before any data exists. Ask what distribution the mechanism
implies under each candidate clock; if one of them makes the preregistered null
a theorem, that clock is not a modelling choice, it is a way of not running the
experiment.

## The shape appears, and it survives the question that mattered

The comparison was frozen before the hazard existed, as a branch table over
three outcomes. It landed on the middle row: **breached delvings are deeper,
with substantial overlap in both directions.** Nothing was tuned.

```text
PANEL [42, 7, 1234, 0, 1, 2, 3, 4, 5, 6, 8, 9]
  196 workings — 26 breached, 96 ordinarily ended, 74 STILL OPEN (excluded)

              n   at floor      min     q1   median      q3      max
 breached    26   1 ( 3.8%)    12.0  127.8    398.5  1094.0   2656.2
 ordinary    96  35 (36.5%)     4.0   12.0     28.5    86.2   1022.5

  AUC 0.8654   z 5.702
  OVERLAP  18/26 (69%) breached below the deepest ordinary end
           56/96 (58%) ordinary above the shallowest breach
```

(Medians here interpolate between the two central order statistics; taking the
upper one instead — the convention the hazard's own commit reported — gives
569.1 m against 31.1 m on identical data, every count and extremum matching.
Both are printed so neither can be read as evidence the world moved.)

The overlap is not an apology. Perfect separation would have been the *third*
branch and a warning sign: it would say the hazard had become a depth threshold
in disguise.

**The part that matters is what the branch table could not ask.** A pooled
comparison of two depth distributions cannot tell *"breached delvings sit at
their own maximum without being selected for depth"* from *"breach is a tenure
lottery and depth is a bystander"* — the two produce identical pooled
distributions. Breached median tenure is 17.5 epochs against 3.0, so the weak
reading was live and large. What closes it is conditioning on tenure:

```text
by epochs dug     breached           ordinary        stratum AUC
  1             n= 1 med    12.0   n=20 med   12.0      0.675
  2–3           n= 2 med    29.9   n=31 med   12.0      0.823
  4–8           n= 4 med   105.0   n=27 med   61.2      0.731
  9–20          n= 8 med   210.2   n=12 med  165.9      0.646
  21+           n=11 med  1274.3   n= 6 med  335.2      0.939
STRATIFIED  AUC 0.7599  z 3.303 — direction holds in every stratum
```

The pooled gap attenuates from 0.8654 to 0.7599 — the honest size of the tenure
contribution — and it survives. Under a per-metre hazard both halves are the
mechanism, since the hazard integrates total metres and total metres is tenure
times rate; but only the stratified result rules out the reading in which the
hazard merely re-labels long-lived workings.

A third population is reported and then set aside. Seventy-four of the 196
workings — 38%, the largest single group — had not ended when the record stops.
They are right-censored: a working still digging has no *final* depth, and
treating a reading taken mid-dig as a completed one is simply wrong. The
honesty note that must travel with the exclusion is that it is also the choice
that shows the **larger** effect. Still-open workings are deep (median 674.9 m,
above the breached median), so pooling them into "ordinary" raises that group's
median from 28.5 m to 99.4 m and *shrinks* AUC to 0.6767. The exclusion costs
the finding evidence rather than inflating it, and the reason to prefer it is
censoring, not conservatism.

## What a later people can know, and what it cannot

Three states were wanted, and the shipped derivations already produced all
three: a warning whose legibility decays as `exp(−age / 300 y)` against a dread
that rises as it fades. Measured over the panel before any test was written:

```text
RECENT   youngest breach   age   25 y   legibility 0.920   dread 0.632
DECAYED  oldest   breach   age 1500 y   legibility 0.0067  dread 0.997
WARDED   a living occupation over a breached delving — 3 vertices
```

So this half of the campaign shipped as **assertions rather than machinery**,
which is the outcome the plan named as legitimate and welcome. The formulas
were right; what was missing was anything pinning them.

Pinning them found that the third state is subtler than the design said. A
breached delving's own layer can never read as a kept ward — a maintained seal
holds exactly when nothing has ended, and a breach *is* an ending — so the
WARDED state lives one layer up, as a **living settlement standing over the
breach**. All three such layers read `Maintained` / `Venerated` / dread 0.1 /
legibility 1.0, byte-identical to a living layer at a site where nothing ever
happened. That is the design's best property arriving for free: the model can be
mistaken in the direction that kills people, with nobody authoring a deception,
and it is source-blindness arriving structurally — a later culture receives an
appearance, never a source.

But the residue field takes a maximum over a vertex's whole palimpsest, so those
same three vertices read **0.936 / 0.997 / 0.998** at the field. **The world can
lie to you only if you read the layer.** Read the field and the danger is plain.
The model is source-blind at the layer and not amnesiac at the vertex, and both
halves are now asserted rather than left to be discovered. Nothing consumes that
field today; whichever campaign wires an avoidance or knowledge layer decides,
by picking a read, whether its cultures can be wrong about what is behind a
wall. It should decide that on purpose.

## Nothing is named

A breach records that a delving ended by breaking through. It does not record
what came through, because nothing knows. No entity is minted, no metaphysical
charge is written, and the ending carries no agent — there is no antagonist to
name even if the prose wanted one. The permission that a breach's consequence
*may* persist and travel the lattice it was released into is on the record and
is deliberately unmodelled: it needs an underworld with its own productive base
to bound how far a thing can range, and a bound that varies per world has to be
derived from the rock rather than chosen as a constant.

That constraint is a claim about **words**, so the almanac's one conditional
line is pinned verbatim and a mutation that names what was found reddens:

> *3 of those delvings ended where they broke through — the digging stopped
> there, and no account of what was found survives.*

Every clause is a fact the record holds, and the sentence ends on the one it
does not. It says nothing about depth — pairing "breached" with "how deep"
would assert the mechanism the design exists to refuse — and nothing about the
ground being cursed. Seven of the twelve panel worlds render it; five say
nothing at all, because a line that appears in every world is a template rather
than narration. Seed 42 makes the point three times over, because a sky changes
a climate and a climate changes where people settle: under a fixed golden sun
three of its delvings broke through, under a tidally locked sun six did, and
under the generated star system sixteen abandoned delvings lie beneath its land
and **none** of them broke through — so that page says nothing at all.

The ground is not cursed, and the world proves it. On seed 4 a **living mine
stands over a breached delving** — someone digging again where the last lot
died. One instance across twelve seeds, too thin to build on, and exactly what
the design permits by refusing to put a permanent penalty on a cell. A cell
that is forever unsettleable is a scar on the map; what this campaign kills is
a culture.

## The measurement that authorised the design no longer describes the world

The first task's finding was that settlements are *under*-represented in
high-ore ground: 0.89% / 1.98% / 8.86% of occupations in land's own top
prospectivity decile, against the ~10% indifferent siting would give. Run again
on the finished world, that reading is **2.64% / 9.76% / 9.96%**. The
under-representation is gone; two of three seeds now sit at the base rate.

That is the mechanism working, and it is a fair summary of the whole campaign.
Settlements are no longer placed without reference to ore, because some of them
are placed *for* it — and a working's own daughters are founded near it, on
ground that is ore-rich for the same reason its parent was. The occupation
totals corroborate the attribution rather than merely permitting it: they read
1,212 / 656 / 914 against a pre-campaign 1,240 / 661 / 898, which are exactly
the moves the ring scan produced and nothing else moved.

The honest limit is that nothing splits that decile by what kind of settlement
sits in it, so *mines and their descendants* is inference from the totals rather
than a measurement. It is worth stating because the finding that made this
campaign necessary is now a fact about a world that no longer exists — true when
it was taken, and no longer a description of the tree it authorised.

## What it leaves

Of the five kinds of subsurface residue the world's own legend names, exactly
one could occur before this campaign. `AbandonedDelving` can now, on every seed
that grows a working. `SealedVault` still cannot: it reads off the `Fort` and
`Cult` functions, and across twelve seeds the world produces 9,394 agrarian
settlements, 196 mines, and **zero** of either. Manufacturing one needs a second
function derivation — a route, a shrine-worthy feature, a defensible seat — and
each of those is its own argument. This campaign derived exactly one function
and said so.
