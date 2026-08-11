# The Radiation

Six elves — wood, high, drow, sea, desert and snow — take the settling roster
from nine peoples to fifteen and the biosphere from thirty-three kinds to
thirty-nine. They are the largest family the world holds: six daughters against
goblinoid's three and dwarf's three, and the first family big enough that the
question *what shape is this family* has a real answer rather than a
hypothetical one. With them the peoples programme closes.

The campaign's most useful result is not any of the six. It is a number.
`0.25` sat in the biome-affinity registry as the level every authored row fell
back to away from its home ground, and it had never been derived from anything.
Deriving it moved more of the world than the six new peoples did — and moved a
finding a previous campaign had already published.

## Which channel each elf speaks in, stated before it was measured

A roster of six kinds differing on several axes at once measures nothing. The
six were authored so that two pairs each isolate a single variable — wood
against high isolates *mind*, wood against drow isolates *realm* — and, before
any of them was placed in a world, each kind's observable channel was written
down:

```
  Wood / Desert / Snow / Sea   PLACEMENT. The biome affinity moves where they
                               live; the settlement distribution is the probe.

  Drow                         PLACEMENT, via the REALM GATE (~1,667 cells).
                               Its dark-adaptation authoring is DORMANT and
                               contributes nothing — by measurement, not by
                               omission. Guarded by the Warren tripwire.

  High                         NOT PLACEMENT, by design. Its identity lives in
                               psyche, society and language facts only. A
                               placement readout is the wrong instrument for it
                               and will correctly show nothing.
```

Written down in advance, high-elf is a control. Discovered afterwards, high-elf
is a failed elf and drow's dormant half is a bug. The difference between those
two readings is that table and its date. It is reproduced here first, and
verbatim, so that a reader meets it before meeting high-elf's empty placement
result.

## One route, and the inequality that permits it

Every elf is differentiated by **biome affinity** — a per-biome mask applied
outside the four-way tolerance minimum — and by nothing else environmental. A
kind may take an affinity only when its authored elevation devotion sits *below*
its sovereignty floor, because then the unfloored elevation term is the binding
axis at every cell and the climate curves are computed and discarded; an
affinity restores a preference the model was throwing away rather than
duplicating one it already honours. All six clear the bar, and by a wide margin:

```
  kind          mass kg   sovereignty floor   elevation devotion
  desert-elf       50.0            0.421703                 0.30
  drow             52.0            0.424802                 0.30
  high-elf         55.0            0.429202                 0.30
  sea-elf          58.0            0.433335                 0.30
  snow-elf         60.0            0.435955                 0.30
  wood-elf         55.0            0.429202                 0.30
```

Sea-elf takes the productive shallow band — coral reef, kelp forest, upwelling,
epipelagic — rather than the whole ocean. The ocean is 2.7 times the land in
total cells, which reads as a runaway until the denominator is corrected to
*per-elf habitat*: on the shelf band sea-elf gets about 1,425 cells, larger than
desert-elf's 241 and smaller than snow-elf's 4,633, with drow confined to the
roughly 1,667 land cells holding an enterable cave. Across the family the spread
is nineteen-fold with no outlier. The runaway existed only in the wrong unit.

Sea-elf also escapes the trap drow sits in, for a precise reason: **in the
ocean, depth is −(height above sea level)**. The two quantities coincide, so an
elevation curve can honestly say *shallow*. On land they do not coincide, which
is why no depth is encoded in drow's elevation curve at all — a deep chamber
under a mountain sits high above the sea. Drow is authored as one cave kind
whose only required separation is from surface elves, and the realm gate does
that.

## The level was never derived

An affinity row carries two separable things. Its **shape** says which biomes
are the kind's country and how far out each one sits. Its **level** says how
much of a cell the kind still takes where the biome is not its country at all.
The shape was derived per kind, from the biome classifier read at the kind's own
authored climate. The level was `0.25`, and its provenance turned out to be
illustrative example code written while planning an earlier campaign. It was
never in that campaign's design; it was adopted as an authored constant, and
then adopted again as house style for six more kinds. An example became a
convention became a world-shaping constant, unexamined because it looked like a
detail.

It was not a detail. At `0.25`, the six elf rows took seed 42's history from 552
occupation records to **193**, settlements from 192 to 100, subordinations from
232 to 41, standing tribute relations from 83 to 17 — four deliberate fidelity
floors breached at once, and the dose was non-linear (wood, high and drow alone
gave 366; desert, sea and snow alone gave 660; all six together gave 193).

The repair was to stop authoring the level and derive it from the model's own
algebra. The ladder's four steps became **preferences** in `[0, 1]` —
stronghold `1.00`, near `0.70`, marginal `0.45`, elsewhere `0.00` — mapped
through the kind's own sovereignty floor:

```
  factor(biome) = floor + (1 − floor) · preference(biome)
  floor         = sovereignty_floor(mass, potency)
```

so a row's default **is** that floor exactly, a stronghold is exactly `1.00` for
every kind however heavy, and the rungs stay comparable across the registry.
This is the identical form the model already uses for a condition tolerance
(`floor + (1 − floor) · devotion · bump`) — the same floor, in the same
position, mapping a preference into `[floor, 1]` — and `sovereignty_floor` is
the model's single existing statement of how much environmental unsuitability a
creature's mass and potency buy it off. A second answer to that question would
have been a second model.

Replacing the fourth step alone would not have worked, and the two heaviest
occupants are the proof: gnoll's floor is `0.495` and the woolly mammoth's is
`0.692`, both *above* the old `0.45` marginal step, so a biome declared marginal
would have scored below an unlisted one and declaring a preference would have
been a penalty for holding it. Mapping the whole ladder keeps
`stronghold > near > marginal > default` true by construction for any floor.

Derived, the same six rows give seed 42 **704** occupation records — *above* the
552 measured with no elf rows at all. The derivation is registry-wide, so it
relaxed the two rows a previous campaign authored as well, which nobody had
costed. The consequence reads correctly as biology: a woolly mammoth is far less
diminished by being off its ground (`0.692`) than a wood elf is (`0.429`), which
is roughly what six tonnes of homeostatic buffering ought to buy. That is a
plausibility reading and it is offered as nothing more; nothing measured here
distinguishes the sovereignty floor from any other function increasing in mass.

### It corrects a published finding

The Range measured gnoll falling from 20 settlements to 2 while its arid share
rose only 0.000 → 0.500, and built a caveat on it: a downward-only mask
*suppresses* rather than *relocates*, nine settlements removed for every one
that moved. Under the derived ladder the same row gives gnoll **13 → 40** with
its arid share rising **0.000 → 0.825**.

The caveat does not survive. The old fourth step had been suppressing the world
at The Range too; with one settling people in nine, no census could see it, so
the suppression was read as a property of the mechanism rather than of a
constant. The direction The Range bet on was right and was scored on evidence
that understated it.

And a related claim in that campaign's own record was measured false by a wide
margin: *a uniform affinity is inert by construction*. It is inert for how a
single kind **ranks** cells — a scale-free ranking cannot be reordered by a
constant — and it is emphatically not inert for what happens next, because the
same factor multiplies the capacity that becomes a settlement's population, and
the history bake's volume is a function of population. Replacing gnoll's row
with a genuinely uniform one, shape empty and only the level moving, takes seed
42 from 12,461 facts at `1.00` to 10,758 at `0.50`: a uniform mask on **one kind
of thirty-five** removes 13.7% of the world's facts.

That is the campaign's central lesson, and it is Nathan's diagnosis rather than
the campaign's: **the affinity level is two quantities wearing one number.** A
rung sets how a kind ranks cells *and* how productive it is on them. The level
is arbitrary for the first job and load-bearing for the second. "The level is
gauge" was never careless — it was a true statement about one of the two jobs,
applied to both, which is exactly why it survived a design, an implementation
and a published chronicle unchallenged.

The derivation makes the number principled. It does not split the job. The same
class of surprise can recur.

## The predictions, against the falsifiers they were frozen with

### The roster moves the committed world

Declaring the six changes the committed world on all three tested seeds: seed 42
goes 7,486 → 10,549 facts, seed 7 13,335 → 14,445, seed 1234 8,174 → 15,446, and
all three world hashes differ. Stated as an obligation, not a discovery — the
failure mode it guards against is real and specific (a previous campaign's first
commit was byte-neutral because the bake filters to settled kinds and both of
its new kinds were fauna), and it did not occur.

The per-elf version is sharper and it confirmed on **18 of 18** cells:

| elf | seed 42 (with → without) | seed 7 | seed 1234 | own settlements 42/7/1234 |
| --- | --- | --- | --- | --- |
| desert-elf | 13389 → 8393 | 17709 → 15463 | 14531 → 13267 | 3 / 3 / 1 |
| drow | 13389 → 11558 | 17709 → 12421 | 14531 → 13590 | 5 / 3 / 13 |
| high-elf | 13389 → **13845** | 17709 → 13175 | 14531 → 13916 | 2 / 2 / 4 |
| sea-elf | 13389 → 12371 | 17709 → 17436 | 14531 → 14225 | 3 / 2 / 11 |
| snow-elf | 13389 → 10853 | 17709 → 14865 | 14531 → 12250 | 25 / 11 / 2 |
| wood-elf | 13389 → 8711 | 17709 → 17279 | 14531 → 13865 | 6 / 10 / 8 |

Both riskiest kinds cleared the roster floor with no allowlist: drow founds
5/3/13 settlements, every one of them confined to caves, and sea-elf founds
3/2/11 on water nobody had ever settled.

**Read that pass for less than it looks like.** Removing a kind removes its
seeded draws, and every roster kind draws *something*, so the frozen falsifier —
*some elf is byte-neutral in both directions* — could not have fired even for a
genuinely inert kind. The content is in a column that was added afterwards and
reported beside the frozen axis rather than replacing it: whether removing one
elf moves *the other fourteen peoples' placement*. It does, in all eighteen
cells. And one number is worth keeping on its own: removing high-elf **grows**
seed 42's ledger, 13,389 → 13,845. A people's absence is not a subtraction from
the world.

### Concentration — confirmed for four, exempt for one, falsified for one

The axis is the *share* of an elf's settlements sited in its authored
**stronghold** biomes, against an arm with its affinity row absent, on three
seeds; the falsifier is a flat or falling share while the count also falls, on a
majority of seeds. Reported per elf, never pooled.

| elf | seed 42 (absent → shipped) | seed 7 | seed 1234 | verdict |
| --- | --- | --- | --- | --- |
| wood-elf | 30 @ 0.967 → 6 @ **1.000** | 5 @ 0.000 → 10 @ **0.100** | 3 @ 0.000 → 8 @ **1.000** | share rose 3/3 |
| snow-elf | 5 @ 0.000 → 25 @ **0.960** | 15 @ 0.000 → 11 @ **1.000** | 4 @ 0.250 → 2 @ **1.000** | share rose 3/3 |
| sea-elf | 3 @ 0.000 → 3 @ 0.000 | 2 @ 0.500 → 2 @ 0.500 | 10 @ 0.300 → 11 @ **0.636** | rose 1/3; count never fell |
| drow | 5 @ 0.400 → 5 @ 0.400 | 3 @ 0.000 → 3 @ **0.333** | 43 @ 0.023 → 13 @ **0.462** | rose 2/3 |
| high-elf | 8 @ 0.500 → 2 @ **1.000** | 9 @ 0.000 → 2 @ 0.000 | 6 @ 0.333 → 4 @ **0.750** | exempt — reported, not asserted |
| **desert-elf** | 3 @ 0.000 → 3 @ 0.000 | 19 @ 0.000 → 3 @ 0.000 | 5 @ 0.000 → 1 @ 0.000 | **falsified** |

Snow-elf is the strongest single result in the campaign: **0.000 → 0.960 while
the count rises 5 → 25** on seed 42, and 1.000 on both other seeds. High-elf, the
control, behaves exactly as the visibility table said it would — it moves *with*
wood-elf, not independently of it, because wood's row is its row. That is the
null control reading correctly, not an elf succeeding.

### The first falsification: the mechanism worked and the axis could not see it

Desert-elf's stronghold share is `0.000000` in both arms on all three seeds while
its count falls 19 → 3 and 5 → 1. The frozen falsifier fired, and it fired
correctly.

But the descriptive biome histograms — printed for every elf, asserted for none —
say what the frozen axis cannot:

```
  seed 42    absent  temperate-forest 2, tropical-rainforest 1   ->  shrubland 2, savanna 1
  seed 7     absent  trop-seasonal-forest 16, trop-rainforest 3  ->  savanna 3
  seed 1234  absent  temperate-forest 3, taiga 2                 ->  shrubland 1
```

Desert-elf's row reads `desert 1.00 / savanna 0.827 / temperate-grassland 0.827
/ shrubland 0.682`. Across three seeds it went from **0 of 27 settlements on any
biome its row names** to **7 of 7** — total relocation onto authored ground,
landing on the `near` and `marginal` rungs rather than the stronghold.

The axis is the stronghold rung *alone*, and this kind's stronghold is a single
scarce biome. It is structurally blind to a relocation one rung down, so it reads
complete relocation as suppression. The pre-committed diagnosis for this failure
— a mask too shallow to relocate, thinning a people and leaving it in place — is
the exact opposite of what happened, and it was re-derived and ruled out rather
than reached for: desert-elf's off-row occupancy went to *zero*.

**Nothing was retuned and the axis was not widened.** Widening it to "any
authored biome" and calling the prediction confirmed six times over would have
changed what counts as success after seeing the answer. The five elves the axis
can see keep the frozen assertion and pass; desert-elf keeps the *same* frozen
assertion and the *same* falsifier, carried in the project's own idiom for a
preregistered prediction that was not met, with its diagnosis attached, awaiting
a rung-weighted successor axis that a later campaign must preregister before
writing any code. The failure stays on the record and stays greppable.

This is the campaign's recurring shape arriving in its own measuring instrument:
**a check one level away from the thing it protects, reading as though it
guarded it.**

### Wood and high: identical fields, disjoint ground

The null control was predicted to be boring on the capacity field and it was.
Wood-elf's and high-elf's per-cell capacity fields are **bit-identical** over
every land cell of every seed — `max|d| = 0.000000`, `r = 1.000000000` over
11,066 / 19,046 / 11,571 land cells — which is what two kinds sharing a mass
(55.0 kg) and an affinity row must be. The limit was stated in advance: a wiring
check with no information in it, run because a wiring check that has never been
run is not a wiring check.

The information was in the second axis, and it is a genuine discovery:

```
  seed 42    wood  6 settlements (home 1.000)   high 2 (home 1.000)   SHARED CELLS 0
  seed 7     wood 10 settlements (home 0.100)   high 2 (home 0.000)   SHARED CELLS 0
  seed 1234  wood  8 settlements (home 1.000)   high 4 (home 0.750)   SHARED CELLS 0
```

Two kinds whose capacity fields agree to the last bit over eleven to nineteen
thousand cells found **different numbers of settlements on wholly disjoint cell
sets**, at different stronghold shares. Wood outnumbers high three to one, five
to one and two to one; they differ only in psyche, society and language, none of
which the capacity path reads.

**The contest is not a function of the field.** Iteration order, tie-breaks,
migration and the raid comparison all participate, and this is the first
measurement in the project to isolate that with the field held bitwise constant.
It is a finding about the contest, not a fact about elves.

### Drow separates by the realm gate, and the gate does 94% of the work

With its subterranean row present, the share of drow settlements on a cell
holding an enterable cave is exactly `1.000000` on all three seeds — 5 of 5, 3 of
3, 13 of 13.

The attribution was tested with a five-arm factorial rather than the single
knockout originally designed, because drow differs from wood-elf in its resource
vector as well as its realm, and one arm cannot tell "the gate did it" from "the
niche did it". Correlation of drow's capacity field against wood-elf's, over
land:

| arm | seed 42 | seed 7 | seed 1234 | bit-identical |
| --- | --- | --- | --- | --- |
| shipped (both) | r 0.135376340 | r 0.147157435 | r 0.347868177 | no |
| realm removed (niche only) | r 0.948400585 | r 0.916886480 | r 0.936537980 | no |
| wood's niche (gate only) | r 0.188034190 | r 0.212502192 | r 0.392852360 | no |
| **both removed (closure)** | **r 1.000000000** | **r 1.000000000** | **r 1.000000000** | **yes** |
| wood given the gate (vs high) | r 0.188034190 | r 0.212502192 | r 0.392852360 | no |

The closure arm is the one that matters. With both knockouts, drow's field is
bit-identical to wood-elf's on all three seeds — **there is no unenumerated
third difference**. Note that drow and wood still differ in mass (52.0 against
55.0 kg) and in all three climate curves in that arm, and the field cannot see
either, which independently re-confirms that the floor computed inside the
suitability path is discarded for exactly the kinds an affinity admits.

On seed 42 the total departure from identity is 0.865. The gate alone accounts
for 0.812; the resource vector alone for 0.052. **The gate does about 94% of the
work**, and the niche's contribution is now measured rather than assumed away.

The sufficiency arm — giving wood-elf the subterranean value read out of drow's
own row, and comparing it against high-elf — reproduces the gate-only figures to
nine places. It is the only arm that demonstrates sufficiency *additively*, which
no subtractive arm can, but its numbers are arithmetically determined by the
closure arm plus the wood/high identity. It is a consistency arm, not an
independent one, and should not be cited as a second confirmation.

**The companion null held.** Drow's dark adaptation is authored and dormant:
perturbing its insolation devotion from 0.55 to 0.05 — a large perturbation, not
a nudge — leaves the committed ledger byte-identical on all three seeds, facts
and placement both. Dormant by measurement, not by omission, exactly as
preregistered. The arm is positively controlled by a separate assertion that the
perturbation actually lands, so the null is not a tautology.

### The language products

**Every elf daughter's proto-root matches an independent re-draw** of the shared
elf family proto — 469, 468 and 469 elf roots checked on the three seeds, **zero
breaks** — and the re-draw reconstructs the lexicon builder's *own* universe rule
rather than restating it, because re-deriving the rule instead of the draw was a
real bug in the goblinoid version once. Census-wide the same property reads
`true` on **1000 of 1000** worlds. In the meeting census, whose rosters no new
kind can join, the column reads *absent* on all 1000 — the roster change did not
reach a study no roster change should reach.

**Divergence is real at six.** Of the concepts rooted in all six daughters, **68
of 68 diverge** into two or more present-day forms, on every seed. A numerator
equal to its denominator is worth re-reading as a possible tautology and this one
is not: the denominator is "rooted in all six", the numerator is "≥ 2 distinct
modern forms", and nothing forces the second from the first. Six draws have more
room to differ than three and they use all of it.

**Homophony does not leak the sibling count — not falsified, and stated no more
strongly than that.** Homophony is a within-daughter property; the number of
siblings should not enter it. Colliding pairs, per-daughter means:

| seed | elf (6 daughters) | goblinoid + dwarf (6) | direction |
| --- | --- | --- | --- |
| 42 | 7.167 | 4.333 | elf above, 1.65× |
| 7 | 1.667 | 12.667 | **elf 7.60× below** |
| 1234 | 18.167 | 6.833 | elf above, 2.66× |
| **panel** | **9.000** | **7.944** | 1.13× |

The headline is the **sign reversal**, not the pooled magnitude: the direction
reverses across the panel and the largest single-seed excursion is *downward*.
That contradicts the mechanism the falsifier names far more directly than the
pooled 1.13× does.

Three limits are recorded because they bound how strongly this may be read.
Only **one of the three counts** the clause names is live — core and confusable
collisions read exactly zero for all twelve daughters on all three seeds, so two
of the six assertions compare zero against zero and confirm nothing. The holding
**rests on one seed of three**: drop seed 7, the seed that reverses, and both
assertions fire. And a six-against-six panel over three seeds, with per-daughter
counts ranging 0 to 41, cannot resolve a two-fold systematic leak from noise in
either direction. This clause is weak evidence *both* ways, and its successor
needs to be preregistered — many more seeds, and a panel on which the two silent
counts are non-zero — before any code is written against it.

**Census-wide language products stayed in band.** Blind attribution rose, 0.9071
→ 0.9112 of present rows, well above its 0.75 floor. Name transparency's low tail
did not vanish: sub-0.60 mass grew by half again (126 → 190 worlds) and sub-0.50
mass grew, which is the opposite of the uniformity relapse the clause exists to
watch — what collapsed was the *high* tail (139 worlds above 0.90 fell to 29).
The literal minimum did rise, 0.1654 → 0.3013, and the span narrowed 19%; that is
what averaging a per-world mean over 30% more settlements predicts, and the
observed per-world standard-deviation ratio of 0.880 matches the predicted 0.875
to within half a percent, on both tails at once. The mean's separate 6.3% fall is
a mixture shift — six new lexicons entering an average that is not weighted by
species — and this census cannot decompose it further, because no per-species
transparency column exists.

### Longevity is silent in language drift

The elves are the longest-lived people in the world, paced harder than the
dwarves. **It buys them nothing in language.** The drift regime is *binary* at a
lifespan of 120 years, and the dwarves already clear it with a wide margin, so
pacing an elf harder moves the regime not at all: all six sit on the slow regime,
doubling the pacing factor again leaves the regime unmoved, and dropping to pure
allometry *does* move it — which is what keeps the first two clauses from being
satisfied by a branch that never fires.

Longevity remains legible where it belongs: lifespan, age at maturity and
generation length stay linear and unbounded. Pace of life and reproductive tempo
saturate at exactly 1.0 for any people paced this hard, and are uninformative for
elves and dwarves alike. This is recorded because a later reader looking at
long-lived elves and slow-drifting elf tongues will otherwise connect them, and
the connection is not there.

## What the world looks like with fifteen peoples

The census was re-run over a thousand worlds. The campaign adds exactly one
column, so every row reads as changed; diffing only the 194 shared columns
separates schema growth from physics. In the meeting census — whose two rosters
no new kind can enter — **194 of 194 shared columns are byte-identical**. In the
main census, 80 of 194 moved.

The 114 that did not move are exactly the substrate upstream of the settlement
contest, with no exceptions: all astronomy, all tectonics and geology and
hydrology and biome, the incumbent daughters' within-language properties, the
per-kind biology of kinds that gained no competitors. That is the clean statement
the campaign wanted — **adding six peoples moved nothing that is not decided by,
or downstream of, where peoples settle.**

Of what did move:

| | before | after |
| --- | --- | --- |
| peoples placed | 8.916 | 14.854 |
| settlements per world | 186.7 | 243.6 |
| total population | 5,650 | 6,991 |
| standing tribute relations | 64.5 | 81.0 |
| climate displacement events | 114.3 | 180.3 |
| name-pattern signatures | 6.964 | 8.964 |
| largest holding's share | 0.0198 | 0.0138 |

The world is fuller, more crowded, and less dominated by any one holding. Two
worlds that had failed to place a flagship settlement at all now place one, so
all thousand worlds have a first city again.

One number needs its denominator read before it is believed. *Name-people
recoverability* — the fraction of a world's peoples whose naming signature is
unique — fell 39.8%, from 0.564 to 0.339. The **numerator did not move**: about
five uniquely-signatured peoples per world, before and after. The six elves
contributed exactly two new distinct signature classes between them and shared
the rest. They are naming-pattern-degenerate *among themselves*, which is a real
finding about a family of six and not a degradation of the naming machinery; the
per-world criterion that watches for that degradation still passes 1000 of 1000
at an unchanged margin.

## Two claims left standing red, and why

**A disposition claim was falsified and has not been retired.** The assertion is
that peoples who do not raid hold their founding settlement longer than raiders
do; the statistic is a separation between the minimum of one group and the
maximum of the other. At fifteen peoples that separation reads **0.680** and the
claim fails. It is not a roster-size accident and it would be dishonest to
present it as one: the same measurement over the pre-Radiation nine gives
**1.045** — already within 5% of failing before this campaign began.

What died is the statistic, not the direction. Rank correlation between raiding
disposition and flagship tenure is **0.840** over fifteen peoples, *up* from
0.831 over nine, and 0.829 over the six new elves alone. A min-versus-max
statistic over a two-set partition is fragile by construction, and the file that
carries it had predicted its own death when an earlier campaign made the raid
gate a per-settlement draw. Nothing was retuned; the diagnosis is written where
the assertion is; retiring it is a decision for whoever next has cause to look.

**A founder can now go unremembered.** Two founding records that fold to the same
handle used to end the world — a hard failure in world construction. It now drops
the loser from the remembered cast instead. The rate was measured over three
thousand consecutive seeds rather than estimated: **five worlds in three thousand
lose exactly one founder each**, while 958 worlds contain handle-sharing pairs
that the memory depth never reaches. The change is a null on every world that
does not collide — 62 worlds rebuilt byte-identically, with a deliberately broken
variant confirming the comparison could detect a difference — and the seed-42 cast
of 148 founders is entirely distinct. The proper repair is to widen the handle so
it folds its referents' material facts, which is a save-format epoch and is not
this campaign's; the code says so where the handle is defined.

## What was deliberately not measured

**The correlation between how a people speaks and where it lives.** It would be
the obvious headline — *do elves in more distant niches speak more distant
tongues?* — and it is circular, because the same hand authors the articulation
vectors and the environmental niches. Any correlation between them measures the
authoring convention. The project has shipped that error once and caught it: a
dwarf authored at a 300-metre elevation optimum to mean *deep* selected lowland
marshes, and its toponymy came back as an emergent finding until one question
dissolved it. The toponymy was reporting the authoring. Every measurement above
is a *downstream* product of the cascade, chosen because the author does not
control it.

**The per-daughter homophony and inventory-closure families were not extended to
elf as census columns.** They are frozen at four kinds; the dwarves never got
them either, so the homophony clause cannot compare elf against both
three-daughter families as columns — one of the two has none. Extending them
would add two dozen metrics across nine studies. The comparison was made in a
probe instead, and the six-daughter divergence result is a probe result for the
same reason. The one family-level metric that *is* derived from the roster
rather than authored already covers every elf daughter with no change at all.

**The family is a star, not a tree.** With no time-since-split anywhere in the
model, all six daughters are equidistant from the proto. The world can say that
six tongues descend from proto-Elvish; it cannot say that drow split before snow.
There is no field in which that sentence could be written, and authoring one
would be the circular error in its topological costume. The star is nonetheless
worth having: a three-daughter family barely distinguishes a star from a tree,
and a six-daughter family makes the missing structure conspicuous. The tree stays
blocked on reading split time off the deep-history march, which now has this
family as its motivating case.

## The programme closes

Five peoples became fifteen. The original design read *seventeen*, with five
dwarves; The Delvers shipped three and withdrew Mountain-dwarf and Duergar with
the return condition written into the code — *they return when the underworld has
biomes*. The biome vocabulary has twenty-two variants and not one of them is
subterranean, so those two are owed to the campaign that makes the underworld a
place rather than a graph of rooms.

Drow does not wait on that campaign, and the distinction is worth stating because
it is what makes drow separable from the two dwarves: the trap is not *authoring
a subterranean kind*, it is *distinguishing two kinds by depth*. Drow is one cave
kind that needs only to differ from surface elves, which the realm gate does,
measurably, today. Mountain and Duergar are two cave kinds whose entire mutual
distinction is stratum, and nothing in the model can say that yet.
