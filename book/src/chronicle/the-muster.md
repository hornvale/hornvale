# The Muster

A muster is a roll-call: every individual named, counted, and distinguishable
from every other. This campaign was specced to do that twice — once for a
quantity nothing was watching, and once for a person whose identity could
silently be someone else's.

It shipped the first half, refuted its own founding premise on the way in, and
gave the second half away. All three of those are the result.

## The premise, refuted by its own instrument

The campaign began with a registered claim: that the biome-affinity ladder's
**level** — the factor a kind still takes on ground that is not its country —
does two jobs at once and should be split into two numbers. The argument was
respectable. The level multiplies a kind's carrying capacity, which becomes a
settlement's population, which becomes the volume of history the bake grows;
and the level also sets how sharply a kind concentrates in its stronghold. A
number serving a productivity consumer and a placement consumer at the same
time is a number that might be asked for two different values.

So the question was measured before it was designed. A preregistration froze
the decision rule to disk before any code ran:

> **Is there a single λ interval satisfying every band simultaneously?**
>
> - **YES → the level is one quantity.** The consumers were never in conflict.
>   Then ask only whether the shipped per-kind values land inside that
>   interval. Inside → **LEAVE**. Outside → **RE-DERIVE**. No split either way.
> - **NO — the bands are disjoint in λ → the consumers genuinely want different
>   values.** That, and only that, is the empirical case for a **SPLIT**.

Seven readouts carried published bands: per-cell diversity in `[1.5, 3.0]`,
four history fidelity floors, a collapse-share ceiling, and per-kind stronghold
concentration measured against a no-affinity baseline. The rule was written so
it could come back and forbid the split, which is the only reason it was worth
running.

**It did.** The verdict is **LEAVE**, and the registry row that started the
campaign is refuted by the instrument the campaign built to test it.

## But the first arm answered a different question

The way the refutation arrived is worth more than the refutation.

The first sweep varied λ **globally** — one level for every kind, replacing the
shipped per-kind spread. It returned **SPLIT**, on a hairline: a single kind
(drow) short of its concentration band by 0.0067 of pooled share, with the
verdict flipping to RE-DERIVE if that one kind were excluded.

The confound is structural, and it is not about affinity at all. The
hypothesis under test was a substitution along the **consumer** axis: one
number becomes two, each still flat across kinds. The arm substituted along the
**kind** axis: the per-kind structure was destroyed and a single scalar put in
its place. Those are different rungs of the same hierarchy, and an arm on the
wrong rung cannot discriminate the hypothesis however carefully its bands are
scored.

The mechanism shows up cleanly in the concentration readout. What drives a kind
into its stronghold is the *contrast* between the stronghold factor — pinned at
exactly `1.00` for every kind — and the level it takes elsewhere. Under a
uniform λ that contrast is all there is, and it vanishes linearly: at λ = 1.00
a uniform level **is** the no-affinity world. Pooled over the four kinds whose
concentration is asserted, sweep 1's λ = 1.00 column read 0.2029 — precisely
the no-affinity baseline, to four digits, because it *was* the no-affinity
baseline. Concentration could therefore only ever be satisfied near λ = 0,
which put it in irreconcilable opposition to history volume, which needs λ away
from 0. **The opposition between the two consumers was manufactured by the
parameterisation.**

An amendment replaced the arm and left the rule verbatim. The new arm scales
each kind's shipped level, `level_k = λ · floor_k`, so the eight levels keep
their relative order and only their common magnitude moves. It **adds** two
constraints and removes none — ordering is preserved, and λ = 1.0 must
reproduce the shipped world exactly — which is the project's own test for
whether a mid-measurement change is a correction or a rescue.

On that arm, pooled concentration at λ = 1.00 reads **0.7531** against the same
0.2029 baseline, and stays 3× the baseline out to λ = 1.20. The bands are not
disjoint. The all-bands-satisfied set is λ ∈ {0.25, 0.50, 1.00, 1.20}, with the
shipped configuration interior to it and comfortably clear of every binding
band. One quantity, correctly valued, and now defensible on evidence rather
than on parsimony.

Two sweeps over the same seven bands, the same five seeds, the same readout
code, differing in nothing but whether per-kind ordering survives, return
**opposite verdicts**.

## The control that makes a sweep interpretable, and it generalises

Sweep 1 could not express the check that would have caught it. Its shipped
configuration was not a grid point at all — there was no λ at which the sweep
reproduced the world it claimed to be perturbing, so every number it produced
was measured on a world nobody had ever built.

Sweep 2's arm makes the shipped configuration grid point five, and the identity
is demonstrated rather than assumed, at three independent strengths:

- **the components** — all eight kinds' levels and all 35 authored factors
  compared on `to_bits()`, not on a tolerance, at λ = 1.0. This was not
  definitional: `1.0 · floor_k` is an IEEE no-op, but the *shape* still makes
  the round trip `pref = (factor − floor)/(1 − floor)` and back, and a round
  trip can lose a ULP. It did not.
- **the serialized world** — built to full depth on five seeds, byte-identical
  every time (seed 42: 2 104 029 bytes, both arms).
- **the readouts** — the shipped arm run separately through diversity, history
  and placement, every number equal digit-for-digit to its λ = 1.00 twin.

The transferable rule, and it costs almost nothing to apply:

> **For any one-scalar sweep over a per-kind quantity, ask whether the shipped
> configuration reproduces byte-identically somewhere on your grid. If it
> cannot, the sweep is not interpolating the shipped world — it is measuring
> something else, and the first thing to suspect is the collapse of the
> per-kind structure the shipped configuration relies on.**

Both sweeps agree on exactly one thing, and it refutes the failure mode the
preregistration named in advance. Diversity was expected to want a **high**
level while concentration wanted a **low** one, in genuine opposition. It does
not: diversity swings **0.039, 2.7%** across the entire λ range on both arms
and on both rosters, and never approaches a band edge anywhere. **The diversity
debt this world carries is not an affinity-level problem.** It is real, it is
open, and it lives elsewhere.

## The guard that could not see

The sweep found the campaign's actual subject on its way past.

The commit-gate guard for the coexistence behaviour the affinity level drives
builds its own component set, and it built that set with empty stores for
`biome_affinity` and `habitat_realm`. Both are sparse stores read through a
default on absence — a missing realm row means *surface*, a missing affinity
row means *unrestricted at every biome* — so an empty store raised nothing at
all. It silently supplied the null hypothesis. Every authored affinity row in
the registry scored identically inside a test whose purpose was to notice when
they changed.

That is the arc's signature defect in its purest form: a guard reading as
though it protects a quantity, one level away from the quantity it protects.
The level had by then completed two full campaigns — authored, copied, made
load-bearing, breaching four fidelity floors, and re-derived from first
principles — with no test phase anywhere in the cycle.

Both stores now hold live rows, scoped to the peopled key-set: seven of the
eight canonical affinity rows, one of the three realm rows. Three further
stores stay empty on a traced reason and the code says which.

## What the repaired guard sees, decomposed

The two stores went live in one change, so the effect was decomposed rather
than attributed. Measured at the frozen competition temperature over the
guard's own five seeds:

| component set | realm rows | affinity rows | mean claimed diversity | Δ vs blind |
|---|---|---|---|---|
| pre-repair (both empty) | 0 | 0 | 2.3734235460211663 | — |
| realm only | 1 | 0 | 2.3678005458279160 | −0.0056 (0.24%) |
| **affinity only** | 0 | 7 | 2.2024420744011466 | **−0.1710 (7.2%)** |
| repaired, as shipped | 1 | 7 | 2.1127601185602627 | −0.2607 (11.0%) |
| full canonical registry | 3 | 8 | 1.5063123260334543 | — |

The headline 11% is **not** the biome-affinity effect, and the campaign's own
first report said it was. The parts do not add: −0.0056 + −0.1710 = −0.1766
against a combined −0.2607, so **32.2% of the headline is interaction** between
a single realm row and the affinity rows. Affinity's own contribution is
**−0.171, 7.2%**. Anything sized against the level is sized against that
figure.

## The prediction that failed, and why it could not have held

The campaign predicted that the guard's verdict would flip on the **roster
counted** alone — that the same worlds at the same temperature would pass over
the eighteen peopled kinds and fail over the thirty-nine-row biosphere. The
prediction is **falsified**, twice over.

It is falsified numerically: the biosphere arm reads **1.5063123260334543**,
not the 1.42–1.46 the campaign asserted, clearing the floor then in force by
**+0.0063** — with two of the five seeds (1.3483, 1.3784) individually beneath
it.

And it is falsified structurally, which is the more useful half. The band's
ceiling is *derived* from the peopled count rather than written as a literal,
and that count is 18 in **both** arms — the biosphere arm changes which kinds
carry rows, not how many are peopled. Both rosters therefore derive the
identical band `[1.5, 13.5]`, and only the floor could ever separate them. The
flip the campaign predicted was not merely absent; the instrument as built
could not produce it.

What survives is the sharper half of the original observation. A margin of
0.4% of the floor is not a band that means the same thing under both readings,
and the published figure for this bound does not state which instrument
produced it. So the assertion now names its own roster: a reader of a red gate
learns the population counted, how it was derived, how many of those kinds
carry an affinity row, and that the band was written about that population and
not the biosphere.

## Level-only is a null, and the control is an existence proof

A repaired guard that stays green proves nothing, so the campaign owed a
mutation that makes it fail. What it got is a *qualified* confirmation, and the
qualification is the finding.

**At today's reach, the level alone cannot redden this guard at any value.**
Its whole attainable range, every authored shape carried through unchanged:

| arm (the 7 authored rows) | mean claimed diversity | verdict |
|---|---|---|
| affinity off | 2.3678005458279161 | PASS |
| **shipped** | **2.1127601185602627** | **PASS** |
| the level's minimum | 2.0691689632978352 | PASS |
| hard exclusion off-shape | 2.9734723549133930 | PASS |

The floor sits 0.569 below that minimum. Driven to its extreme the mean *rises*
— away from the only edge it could cross — because the level registers as a
contrast between kinds that carry a row and kinds that do not, and deepening
that contrast suppresses the carriers rather than concentrating them.

Widening the reach to all eighteen kinds is necessary and **nowhere near
sufficient**. Given a common shape at their own floors, no depth crosses either
edge, including one arm that more than doubles claimed-cell diversity.

The control that fires needs a roster differentiated across space — and then
the inversion, which two independent probes reproduced bit-for-bit:

| assignment of the 11 added rows | at depth 1.00 | at depth 1.70 | Δ | verdict |
|---|---|---|---|---|
| nine biomes, two collisions | 2.5789073591583951 | 1.4155085834088321 | −1.163 | **RED** |
| the same, both collisions moved onto unused biomes | 2.7441247810223062 | 1.5222025802564441 | −1.222 | PASS |
| fully distinct: eleven kinds, eleven biomes | 2.7514058715031107 | 1.8150142892666530 | −0.936 | PASS |

**The most differentiated arrangement is the one that does not cross**, and it
does not cross at greater depths either (1.6876 / 1.7626 / 1.6945 out to 2.20),
so it is not merely slower to arrive. Repairing two colliding rows — changing
nothing else, moving no level — takes the verdict from RED to PASS. **The
collisions are load-bearing.**

Read the Δ column and not the verdict column for the physics. The level's
effect is large and stable across all three arrangements: it removes about
1.0–1.2 of diversity in every one. What the arrangement decides is where the
baseline sits, and therefore whether that consistent effect lands short of the
floor or past it. The honest statement of the property this guard has:

> It detects the biome-affinity level only on a roster whose rows both reach
> every kind and point at different ground, and even then whether the level
> crosses depends on the particular arrangement. At today's seven overlapping
> rows in eighteen kinds it detects the level at no value whatsoever.

That is weaker than "the guard can see the level", and it is what the
measurements support. The control is an **existence proof**, not a
demonstration that the band tracks the level. Quoting the single red number
without its two green siblings would state the opposite of what was measured.

## The band can only fail from below

One more property fell out, and it belongs beside the number rather than in a
footnote. Cell diversity cannot exceed the number of kinds actually present in
a cell, and the mean claimed cell holds **6.4351 of the eighteen**, pooled over
177 336 claimed cells across the five seeds. The derived ceiling is 13.5. **The
ceiling asks for more coexistence than the world puts in a cell at all**, so
this band is one-sided by construction: it can only ever be failed from below,
and no mutation of the affinity store can reach its upper edge.

That is adjacent to the open recalibration this world's diversity debt already
carries, and it was deliberately not acted on here. Recording it is the
campaign's contribution; repairing it is a different campaign's, with a
different preregistration.

## The exemption everyone believed, and it was false

The level had been defended for two campaigns as *gauge*: a uniform factor
cannot reorder a kind's own ranking of cells, so within-kind ranking is exempt
and only the downstream consumers need care about the magnitude. The campaign
set out to write that exemption down properly, naming the level's three other
consumers beside it.

The exemption is **wrong**, and the sentence asserting it came from this
campaign's own planning text.

A change to the level is not a uniform rescale. The constructor maps each
preference to `floor + (1 − floor) · p`, which holds a stronghold at exactly
`1.00` while pulling every lower rung down: it changes the ladder's
**contrast**, not its scale. The factor then multiplies the capacity field per
cell, keyed on that cell's biome, so it reweights biome against every other
condition in the product — and cells reorder.

Measured on seed 42, moving the seven authored rows from their shipped level
with every shape held fixed: **all seven row-carrying kinds have their own cell
ranking changed**, and gnoll's argmax — the cell the placement routine would
pick as a stronghold — moves from cell 30312 to cell 2276, with 5 of its top 50
cells surviving. **All eleven row-less kinds are bit-identical**, which is the
control: for a kind with no authored row the factor is `1.0` at every level,
and there the level genuinely is gauge.

Two claims had been wearing one sentence, and only one of them is true:

- *A uniform rescale of a whole row cannot reorder that kind's own ranking.*
  **True**, and it is what the previous campaign's chapter says.
- *Changing the level preserves within-kind ranking.* **False**, because the
  constructor is not a uniform rescale.

For a kind carrying a shaped row the level is load-bearing in all four
consumers and gauge in none. The general form is the campaign's most portable
finding, and it is ratified as a decision: **when a quantity is described as
gauge, name the transformation it is gauge under.** "Level is gauge" names
none, which is exactly how it stayed unfalsified for two campaigns while being
wrong about four consumers out of four.

## The other half, shipped by someone else

The campaign's second debt was a founder's identity. The key that names every
founder in every world folded a community's people, site, founding, ending and
peak population — and deliberately excluded the entity id, so that a name is
reproducible from material facts alone. Two occupations identical in all of
those therefore collided by construction, at a rate of about two worlds per
thousand, and the world generator's response was an authorized fidelity cut: a
colliding founder is dropped rather than fatal.

That half was held rather than built here, and the hold was the right call. The
campaign that ships a units repair to the fact envelope was in flight, and it
retypes the very fields the founder key reads — a founding's and an ending's
year. Landing a new identity key first would have renamed every founder in
every world, and then that campaign would have renamed them all again: two
identity epochs back to back, the second silently invalidating the first's
regeneration, for one campaign's worth of doubled artifact churn.

What happened next is better than the plan. **That campaign shipped the
widening itself**, rather than handing the tree back — riding an epoch it was
already paying for, which is precisely the cost this campaign declined to pay
twice. Its own measurement then refuted the narrower key its spec had
ratified, and the resulting decision names a distinction this campaign did not
have: an **identity** key ("is this the same thing?") and a **discrimination**
key ("give me a distinct deterministic draw") are different kinds of object,
and a key's definition must say which it is.

It honoured the constraint this campaign's spec made non-negotiable. **The drop
backstop stays** — no key is total, and a world generator must not panic on a
legal seed. Over the thousand-world census range the shipped key now leaves
**0 colliding worlds and 0 dropped founders**, against 2 and 2 before. The
residual over three thousand seeds is exactly two worlds, and it is the case
this campaign's own diagnosis predicted no key would remove: the colliding
pair's two *parents* are themselves twins, so the ancestry hop folds
identically however deep it reaches.

## No census, and how that was known

A campaign that moves a world owes a refresh of the thousand-world census on
the canonical box — about fifteen minutes of a machine other work is queued
behind. This campaign owes none, and the reasoning is worth stating because
declining correctly is as much a result as running one.

Everything Part A shipped is a test file and a doc comment. The one production
crate it touched carries **zero non-comment changed lines**; the other file is
the guard itself. No seeded draw moved, no stream consumption order moved, no
rendering changed. The seven-path artifact drift check comes back clean after a
full regeneration, which is the mechanical statement of the same fact.

The campaign's numbers were nonetheless re-measured rather than restated after
absorbing the units repair, because **Part A's figures live in doc comments,
not assertions**, and a doc comment cannot go red. The guard passing said
nothing about whether the tables it documents still reproduce. An independent
probe rebuilt every arm from scratch — the store decomposition, the biosphere
arm and its per-seed array, the level-only null, all three arrangements, the
common-shape sweep, the clamp onsets, the claimed-cell census, the seed-42
ranking split — and every figure is bit-identical to what is written above.

The reason is structural rather than lucky, which is what makes it worth
recording: this guard builds only to terrain depth and reads a packing
byproduct. The quantities the units repair retyped are a fact's day — committed
at genesis on that rung — and a founding's and an ending's year, authored by
the history bake strictly downstream of everything measured here. A units
change reaches these tables only through terrain, climate, the affinity or
realm stores, or the packer, and it reached none of the four.

## What is carried forward

- **The level is one quantity**, valued defensibly, load-bearing in four
  consumers and gauge in none of them for a kind carrying a shaped row.
- **The guard can now see an affinity change**, and states the roster it
  counted when it fails — but it is one-sided, and at today's roster it is
  blind to the level specifically.
- **A known fragility in the sweep, carried forward rather than buried.** The
  strictest of the three defensible scorings condemns the shipped configuration
  through a single kind that places two or three settlements a seed and ties
  its baseline exactly on three of five. On denominators that small a tie is
  the modal outcome, so that reading is not measuring what it claims for that
  kind. Widen the panel before spending any design decision on it.
- **Seed 1234 fails the history fidelity floors under every parameterisation**,
  including two independent no-affinity constructions. Those floors are
  seed-42 pins and do not generalise; a campaign adopting them as a panel
  criterion will chase a migration-dominated bake.
