# The Tumult

*The campaign built the wrong thing first, measured it honestly, and pivoted —
and the falsification it shipped names its own successor.*

The living-community program had, by its third campaign, an engine and a map
and no war. *The Living Community* grew settlements out of a derived deep
history instead of placing them; *The Connection Graph* and *The Sundering*
gave that history a transport topology and then let the sea move over it. Both
ended with the same measured remark: the world has vacant land to spare, so a
people driven off its ground simply walks to an empty cell, and organised
conflict is somebody else's campaign. This is that campaign. Its charge was to
make conflict **emerge** — never floored, never forced — and to test the
project's standing wager that the size distribution of the resulting violence
would be a **power law**, the signature of a system that has self-organized to
its own critical point.

It emerged. The power law did not. Both of those sentences are the result.

## The first build: a crowding sandpile

The original design was a Bak sandpile with peoples for grains. Population
grows; a community frozen out of its cell looks for somewhere to go; if no
vacant ground is reachable it must displace whoever holds the nearest occupied
cell, and that victim must then displace somebody in turn. Crowding is the slow
drive, the forced displacement is the threshold event, and the chain of
evictions is the avalanche. Measure the avalanche sizes; if they fall off as a
power law, the world found its critical point on its own.

It was built, and it was measured, and it failed in both directions at once.
Seed forty-two fired **zero** cascades — that world never crowds, so the drive
never reaches the threshold and nothing happens. On the seeds that *did*
saturate, every relaxation ran straight to the depth cap, because on a
saturated graph a displaced people has nowhere that is not somebody's, so the
chain cannot terminate and simply runs until the bound stops it. The pooled
histogram was bimodal with a spike in the top bin: not a heavy tail, a
**truncation artifact**.

Two diagnoses came out of that, and the second is the one that mattered. The
shallow one: the model had a **drive but no dissipation**. Sand leaves a real
sandpile at the edges of the table; nothing left this one, so a chain that
started could not stop except by hitting the cap. The deeper one: **density was
never the driver.** The Sea Peoples did not move because Anatolia was full.
Amerindian nations warred with a continent of unclaimed land between them.
Europeans in North America took *cleared and cultivated* indigenous fields in
preference to breaking wilderness that was free for the breaking. In none of
these is scarcity the cause, and a model that fires only when the map runs out
of room is not a model of any of them.

## The reframe: predation on a value field, down a strength gradient

What those cases share is **covetousness for a specific thing** — a valley, a
mine, a fishing ground, a herd, a field already cleared — pursued **down a
strength gradient**. The strong take from the weaker; the displaced, now
desperate and still armed, take from someone weaker still. Shit rolls downhill.
Crowding never enters it.

So the mechanism was rewritten, and it is worth stating how little new
machinery it needed: none. Every quantity was already in the bake. A
community's **strength** is its population scaled by its technological horizon
— iron beats bronze beats neolithic. A cell's **coveted value** is its
era-effective carrying capacity, the same field the growth term already reads,
multiplied by that era's habitability so that no one conquers ground the ice
has just ruined. Each epoch, after growth, a community scans the occupied cells
it can reach across that era's connection graph and raids the best one that
satisfies two conditions: **covetousness** (the target's cell is worth more
than its own) and **dominance** (it is stronger than the holder by a margin).
No crowding term appears anywhere.

Predation is `motive × capability × inhibition`, and the first two of those are
the raid rule above. Without the third, every people that *can* raid *does*,
which is both unhistorical and self-defeating: heterogeneity between peoples is
the fuel criticality burns, and a rule with no inhibition term throws it away.
Two vetoes were taken, the cheapest of each kind. **No spoils** is momentary — a
target already starving against its own capacity has nothing worth contending
over, however weak it is — and it also blocks the pathological regress of
remnants preying on remnants all the way to zero. **Disposition** is durable: a
people whose authored threat response falls below a threshold does not raid at
all, however strong it is on paper. That second gate does something free and
slightly beautiful. Because each people gates on *its own* trait rather than on
a pairwise matrix, the aversion structure is **asymmetric** with no pairwise
machinery at all: A declines B while B raids A. On the shipped roster it means
the goblin, alone among the four peoples, never raids anyone.

The outcome is a **conquest**, not a plunder, and this distinction cost the
campaign a task to learn. The raider **takes the cell** — land is a perfectly
rival immobile good, so taking is evicting — and the loser is driven off. War is
**lossy on both sides**: a fraction of the combined population is destroyed in
the taking rather than transferred, and that is the primary dissipation, value
leaving the system rather than moving within it. The displaced loser, carrying
its reduced strength, then **re-enters the same rule**. Not a different rule for
refugees: the same one, with a substituted baseline. A raider compares
candidates against what it already holds; a homeless roller holds nothing, so
its baseline is the best thing it can get for free, and it makes one comparison
over the nearest ring of cells that contains anything admissible at all —
vacant cells at their plain value, held cells at a premium because a rival's
holding comes already made to work, and admitted only if it can beat the
holder. Each hop costs it more. Below a viable minimum it dies rather than
cascading forever, and that is the second dissipation and the natural avalanche
cutoff.

Nothing in this enumerates a strategy. A strong remnant preys, because it beats
holders and proven ground scores higher; a weak one flees to the empties,
because held cells never enter its option set at all. "Shit rolls downhill"
falls out of the strength gradient. There is no `if fleeing else raiding`
anywhere in the mechanism.

## What the world did

**Conflict fires on value, not on density.** Seed forty-two — the world that
never crowds, the world that fired zero cascades under the sandpile and zero
raids under every model before it — resolves **seventy-six conquests** with
land to spare. That is the campaign's first result and it is a clean
discrimination between two hypotheses: the driver is value × strength, and it
was never density.

**And the map is not depopulated by it.** The obvious risk of a lossy war model
is that it eats the world. It does the opposite. A mutation probe that deletes
predation entirely — the honest counterfactual, since the roll-downhill is only
reachable through it — puts seed forty-two's living communities at 138 without
predation and **203 with it**, and the pooled thirty-seed sample at 1583
against 1955, a 23 % *rise*. Predation does not empty the map; it re-seats and
refounds. Six of the thirty sampled worlds do end with no settlements at all,
and the same six end empty in both arms: those are killed by the ice, and were
before this campaign existed.

**The power law is falsified.** Pooled over a hundred seeds and **2974
conquests**, the cascade-size histogram is `[138, 3, 0, 0, …]` — 141 cascades,
and **nothing at all above size three**. Three independent readings agree and
none of them is near the line:

- **Support.** Occupied sizes span 1 through 3, which is **0.48 decades**
  against the preregistered threshold of roughly 1.5. There is not enough range
  for a scale-free claim to be well-posed, let alone true.
- **Decay rate.** The first bin falls to the second by about **46× per octave**.
  A power law falls by `2^(1−τ)` per octave — between 2× and 4× across the
  exponent range real critical systems occupy. Forty-six-fold is exponential,
  not algebraic.
- **Branching ratio.** One relaxation triggers on average **σ ≈ 0.051** further
  ones, twentyfold below the critical value of one, and *stable* to three
  significant figures across a 3.3× change of sample size. This is not a
  critical system with a finite-size cutoff. It is a system sitting a long way
  from its critical point.

Geometric with a hard exponential cutoff, deeply sub-critical. **No constant was
tuned toward a power law at any point**, and this is emphatically not the
earlier degenerate result: under the first predation build the branching ratio
was zero *by construction* — every loser found empty land, so the question could
not be asked. It is now a measured physical quantity that is simply small. That
is the honest form of a falsification. The mechanism works; the shape is not the
one the payoff wanted.

## The bracketing insight — the two failures locate the answer

The campaign's most useful product is not either measurement but the fact that
it has **two**, and that they fail from opposite sides.

The crowding sandpile had **drive without dissipation**: pressure accumulated,
nothing bled off, and every avalanche ran to the cap. Predation has
**dissipation without accumulation**: every hop pays a war loss and a journey,
every victim is by construction weaker than whoever displaced it, so a chain's
strength decays geometrically while the strength it must overcome does not —
and it dies within a hop or two. Nothing is stored *between* relaxations whose
release could produce a large event.

Self-organized criticality needs both, and the missing half has a name, and it
is one this campaign already deferred in writing before it knew it would need
the argument: an explicit **dominance hierarchy**. Tribute and subordination are
a mechanism that *milks* rather than evicts — the protection racket, the
Danegeld — and what they do is **concentrate value into a topple-able
structure**. The large avalanche is then the collapse-release: bring down a
dominant power and its entire subordinate network is freed at once. That is the
accumulation term, and it is exactly the slice the spec had set aside as the
next one.

So the falsification is not a dead end. It **names its own successor and hands
it a measured argument** rather than a speculative one. Two builds, failing from
either side of the answer, bracket it.

## Two amendments, both after unfavourable observations

The metric's mechanism was amended twice mid-campaign, both times *after* seeing
a result that was not wanted, and both amendments are labelled post-observation
here, in the spec, in the module documentation, and in the retrospective. They
pull in **opposite directions**.

The first followed the initial all-zero histogram: the displaced-loser rule
became one best-value comparison over vacant *and* held cells, with the
settled-land premium. This **raises** the branching ratio by construction. Its
justification is fidelity to the approved spec rather than to the metric — the
spec already asserted that a rival's holding "comes already made to work" and
that a loser "re-enters the raid rule", neither of which the vacant-first
implementation encoded, and under vacant-first the branching ratio is zero by
construction so the campaign's central question is unaskable rather than
answered.

The second followed a different unfavourable observation: the unrestricted
version of that same comparison, scanning every reachable cell, breached the
world-level population-conservation gate, because the occupied set drifted
toward the globe's best land and foundings inflated. Restoring locality —
nearest admissible ring, best value within it — repaired a distance term the
spec had dropped by accident. It **lowers** the branching ratio, and it cut seed
forty-two from six cascades to one, removing the campaign's largest single
cascade.

Neither change was made to move the number, and each moved it the other way.
The honest consequence is that seed forty-two's own histogram, with one cascade
in it, can say nothing whatever about the tail. The verdict above therefore rests
on the pooled samples, twice measured, and a strongly sub-critical branching
ratio is a legitimate falsification that ships as one.

## An inverted hypothesis, and what was under it

The epoch inverted a preregistered directional hypothesis from an earlier
campaign: that kobold chief settlements are *less* coastal than goblin ones,
the kobold being the highlander of the four peoples. The proposed mechanism was
plausible and half true — the raid rule compares a single **species-blind**
value field, so conquest should drag every people toward the same high-capacity
coastal land and hit hardest the people whose niche is furthest from it.

The structural half of that is confirmed and then some. The deep-history bake is
**niche-blind end to end**: the condition niche never enters it, not the raid
rule, not the roll-downhill score, not genesis placement, and the only
per-people input the bake receives at all is the disposition trait. And
seizures do measurably cost a raider its niche — about two thirds of them lower
the raider's own habitat fit while raising capacity by construction, and they
run roughly 130 m downhill, riverward, and thirteen percentage points more
coastal, for every raiding people at nearly identical magnitude.

The causal half is disconfirmed, and the investigation is the interesting part.
The chief settlement is resolved as a species' **oldest surviving occupation**,
not its largest — and a raid closes the raider's *own* record and reopens it at
the back of the queue. Raiding peoples therefore lose about 44 % of their
founding-era records to self-closure where the non-raiding goblin loses 18 %,
and the records they lose are systematically **inland**, for a reason that is
obvious once said: an inland cell has good neighbours to covet, and a
high-capacity coastal cell has few. So the kobold chief settlement changes
hands in 34.7 % of worlds against the goblin's 5.6 %, and the entire coastal
movement lives inside that subset — the rate is byte-identical before and after
wherever the cell did not change. The kobold sites that were replaced sat at
0.549 coastal, far *below* the base rate of kobold founding sites; their
replacements land at 0.688, back at base. Nobody relocated. A biased subset was
deleted and the survivors regressed to the pool mean.

The test was therefore never measuring a niche claim, because the bake has
never placed a people by its niche. It was measuring a difference the shipped
model predicts to be **zero**, which it had passed by half a standard deviation
of draw noise and now fails by two. It was retired with a tombstone rather than
flipped, and replaced by a hypothesis on the one axis the bake actually
differentiates: a non-raiding people holds its founding site far longer than a
raiding one — 0.167 against 0.426 to 0.500, a 2.55× separation, and it reddens
in both directions under mutation of the disposition gate.

## The datum that had deleted a niche

That investigation turned up something it was not looking for. The condition
niche's elevation axis was scored against the **isostatic reference datum** — a
crustal zero, the right frame for a planet and the wrong one for a creature —
while its optima were authored as if in a fixed frame. But a world's sea level
is itself a value of that type, and it differs on every world: across a hundred
seeds it ranged from −1723 m to −3478 m. So an optimum written as "2600" did not
name an altitude at all. It named 5200 m above sea level on one world and
5900 m on another, in both cases at or above the highest land those worlds had.

The kobold's authored highland stronghold — the one axis meant to be its
exclusive, uncontestable ground — was therefore not merely uncontested but
**unoccupiable**, and its habitat fit ran roughly twenty-five-fold below every
other people's *everywhere*, which is the signature of a niche that has been
quietly deleted rather than merely lost. The fix is one subtraction at the
substrate boundary, plus re-authoring the optima against the measured
distribution of settleable land. The kobold's best-fit share of settleable
cells goes from **0.4 % to 26.5 %**; it becomes the best-fit people on every
settleable cell above 3000 m, a band that had belonged to the *plains*
hobgoblin. A unit is not a frame. Both quantities were honest metres; only one
of them was metres from a place a creature could care about.

And the correction exposed what the bug had been hiding. Under the old datum an
ocean cell sat some four kilometres from every authored optimum, so the
elevation axis had been incidentally acting as a **land mask** — and two of the
habitat model's five supply axes had never had one of their own. With elevation
corrected, the three kinds that eat detritus or rock scored right across the
seafloor. The repair is a supply-term one and deliberately narrow: each of the
resource axes is declared *terrestrial*, so a species' habitat follows from what
it eats, rather than the model announcing as a law that nothing lives in water —
a law that would have to be unstated the day an aquatic kind is authored.

## What this campaign is, and is not

It ships an emergent conflict mechanism driven by covetousness and strength
rather than crowding; two inhibition vetoes and the asymmetric aversion
structure they produce for free; conquest, lossy war, the downhill cascade, and
the death of broken remnants; a corrected elevation frame and a land-masked
supply model; and a measured, published falsification of the criticality wager
with the branching ratio to back it.

It does **not** ship the dominance hierarchy, tribute, or collapse-release — the
accumulation term the measurement now argues for, and the one that needs a
persistent relation between communities that the occupation record has no shape
for. Nor captives, revenge, prestige, cohesion, or the remaining inhibition
gates, of which niche-relative value is the notable one: the bake reads a single
global capacity field and thereby asserts that every people values every cell
identically, a claim the condition niche it ignores flatly contradicts. The
elevation re-datum removed that gate's stated prerequisite, so the argument for
it is now stronger than when it was deferred.

Conflict in Hornvale is real, it is opportunistic, and it is sub-critical. The
next campaign has a number to beat.
