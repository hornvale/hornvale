# The Action Clock

*Actions cost time, and time is shared.*

The campaign began with a gap that turned out to be misstated. The inventory
that produced it — twelve roguelike subsystems crossed against Hornvale's
mechanisms — had found exactly one cell unanswered and recorded it as *nothing
in Hornvale schedules time at action granularity.* Run against the tree, that is
too strong, and the truer picture is the more useful one.

Five actions existed. `MoveTo` cost a tenth of a day, the same tenth for a
kobold as for a giant elk. `Drink` and `Eat` cost nothing whatsoever. `Rest` and
`Hold` charged nothing either, but they did something more interesting: they
*jumped* — to the next waking, to the next moment an act could cross a
threshold. Three separate facts, then, where the inventory had seen one absence.
Three of five actions were free; the single cost that existed was uniform across
every creature that could bear it; and the two jumping arms were an
event-driven scheduler in embryo, the same mechanism that stops a roguelike
ticking through nothing.

So this campaign did not build a clock. It finished one that was half-built, and
the half already standing decided much of the shape of the half that was not.

## Why the free actions had been harmless

A free action is not obviously a bug. Drives had been bounding them: a sated
drive switches off, so a zero-cost `Drink` bought a creature nothing it could
spend. The protection was real but incidental — it came from the drive layer's
homeostasis, not from the clock — and it was about to be withdrawn. The Hearth
had landed the anchor graph; the interior it describes is where a creature takes
steps measured in seconds rather than days. Free actions plus free within-room
steps would let a creature do arbitrarily much in one instant. The gap becomes
load-bearing exactly as the fine layer arrives, which is why the campaign ran
before its consumer rather than after.

## Three rungs

**Every action costs.** The five base costs are authored constants in *ticks* —
one dial each, replacing the single `MOVE_DURATION`. A drink is a couple of
minutes; a meal is the better part of an hour; lying down is quick, because the
sleep itself is the jump-to-waking and not a cost at all. None is zero, so the
cost model is total, and a property test holds it total: every `Action` advances
the clock, so a future arm cannot be added for free by omission.

**Cost varies per agent, derived and never authored.** The constitutional
constraint here (decision 0021) forbids authoring per-creature behaviour, so
tempo has to fall out of what a creature already is. The spec's first answer was
metabolic class, on the stated grounds that body mass was not a species
property. That was simply false — `SPECIES_MASS_KG` is a registered trait, and
the derivation that builds an NPC was already reading the biosphere registry to
thread two sibling traits onto it. Mass was one more line of a move already made
three times.

The correction mattered for more than accuracy. Metabolic class has four
variants of which the roster uses two, so class-derived tempo would have given
the world roughly two speeds — a per-agent cost that barely varies per agent.
Mass is continuous and per-species. A mouse and a bear are both endotherms.

Tempo is therefore allometric: the quarter power of mass relative to a
human-scale reference, the same exponent biology uses for stride period and
heart interval and lifespan, and the same one the species domain already invokes
for basal rate. Seed 42's derived population spans seven species and seven
distinct tempi, a continuum from a 13.6 kg kobold at 0.664 to a 450 kg giant elk
at 1.592 — a spread, not two buckets, which is the observable that proves the
trait is genuinely reaching the agent rather than being defaulted.

**Agents interleave.** The tick became a priority queue over
`(next_action_time, entity)` instead of a loop walking each creature through the
whole interval in turn.

This third rung was recommended for deferral, on the honest grounds that
ordering nobody can observe is machinery without a consumer: Hornvale has no
contested resources, no combat, no conversation. The owner overrode the
deferral, and the reasoning is sequencing rather than appetite. The Threshold is
landing anchor occupancy, and two creatures at one anchor is precisely the first
place order becomes observable. Restructuring the hottest loop in the sim is
cheaper now than inside a campaign that must also do something else.

## An integer clock under a floating-point contract

A priority queue needs a total order with deterministic ties, and `f64` is not
`Ord`. Worse, accumulating float additions across thousands of scheduling
decisions is exactly the drift the kernel's pathfinder already avoids by keying
its costs in `u64`. So scheduling is integer and internal; committing remains
`f64` days and is unchanged. `Ticks` is never serialized. This is
quantize-at-emit applied to time — the third instance of one discipline, after
floats and space — which is why nothing about the ledger's shape moved.

The allometry crosses that boundary at a rounding edge: a `powf` routed through
the platform libm, whose last ULP differs between implementations, immediately
becomes an integer tick count where one ULP can flip the result. The tempo is
therefore quantized *before* it is rounded, and a test asserts it rather than
trusting it.

**The tick divides the planet's day.** Rather than fixing a granularity, the
rate is derived: `round(day_length_std × 100_000)` ticks to the local day, at
least one. Two properties then hold at once. The tick stays approximately
absolute, so a base cost authored in ticks means the same absolute duration on
every world — a bear's gait is set by the bear and not by the sky. And the local
day is an *exact integer* of ticks.

The second property is the determinism argument, not the aesthetic one. The
activity cycle is the sim's one genuinely local-day-keyed mechanism, and under
an arbitrary granularity every dawn rounds to the nearest tick, leaving a
residual that *beats* against the day cycle over a long run — a creature waking
a hair earlier each morning for no physical reason. Exact division removes the
beat by construction instead of bounding it. A tidally-locked world has no day
to divide, and takes the base rate; that is stated rather than defaulted,
because a world with no dawn is exactly the world a day-derived clock cannot
derive from.

## What interleaving deliberately does not do

Reordering *acting* is safe. Reordering *perceiving* would not be. Two creatures
acting at the same simulated moment do not see each other within that tick:
cross-agent reads stay against the frozen population, the same one-tick latency
that makes the existing alarm field's wave terminate and the reason that field
was designed the way it was. Interleaving is therefore additive to the
determinism story rather than a threat to it — the queue's tie-break, not the
input order, decides, so a shuffled input yields the same emission sequence.

The refactor that made this tractable came first and separately: the
three-hundred-line per-creature loop was hoisted into an explicit walk state and
a single-step function, proved byte-identical, and only then charged and
reordered. Each subsequent drift is attributable to the change that caused it,
which is the whole reason a campaign that drifts by nature can be reviewed at
all.

## The control that fired, and what it turned out to be measuring

The campaign preregistered seven predictions before the first task, with signs
fixed and a baseline frozen from main's tip. Six confirmed. The seventh —
population-health *chronicity* stays at zero, and movement is a **stop**, not a
finding — refuted on one seed.

The investigation found no pathology. Exactly one tick had flipped: the
separator between two identical four-tick thirst blocks, from healthy to a
moment of fatigue-frustration, welding four-plus-one-plus-four into a single run
of nine. The metric counted consecutive distress *labels*, cause-agnostic, so
two separately-recoverable rhythms that happened to phase-align read as one
chronic episode. The creature was not worse off; that seed's overall prevalence
had in fact fallen.

The tempting move was available and was refused. The failing control's own
comment defined the alarm as a conjunction — chronic *and* never recovering —
while asserting only the first half, so applying the control's documented
philosophy would have turned the gate green. That is precisely why this campaign
could not be the one to do it: a campaign may not repair the instrument that is
judging it. The work parked at the merge gate, and the metric was fixed as its
own campaign — [The Convalescence](./the-convalescence.md), which moved the
bound onto the conjunction and left chronicity as a diagnostic.

The re-measurement afterwards is the part worth recording. By the time this
campaign resumed, main had moved a hundred and twelve commits and five code
campaigns, so the baseline was frozen again from scratch — the old table
predated both the corrected metric and a great deal of physics, and comparing
against it would have aliased other campaigns' work into this one's delta. On
the new baseline, chronicity reads zero on all five seeds, seed 42 included.

The refuted prediction confirms **on its own original terms**, without leaning
on the metric's redefinition at all. The knife-edge had been a property of one
moment's physics, and main's own subsequent changes moved seed 42 off it before
the correction ever applied. Nothing in the cost model was implicated by the
investigation, and nothing in it was touched.

## Measured

Against a baseline re-frozen from main's tip, the campaign moves population
health by at most two hundredths of prevalence in either direction, with
`danger` and `social` distress at zero on both sides. On seed 42, three of ten
creature traces move and seven are byte-identical. The Ametabolic control is the
sharp one: a construct carries no drives at all, so its `drives` vector is empty,
it never selects an action, and it never incurs a cost — and its forty-tick trace
is identical to the byte across the two trees while the metabolic creatures
beside it shift.

The restructure of the sim's hottest loop cost nothing measurable; the health
battery, which is where that loop spends its time, ran slightly faster after the
change than before it.

The macro-scale drift is smaller than the campaign expected, and for a reason
worth naming: on the pre-campaign baseline the day-0 possession walk stirred
1230 creatures over ninety days and 946 after the clock landed, a fall of
twenty-three percent. On current main the same walk stirs 231 without the
campaign and 226 with it. Other campaigns' physics had already removed most of
the motion this one was accounting for, and a delta measured against a stale
baseline would have claimed all of it.

## What is deliberately not here

Within-room action costs have no consumer yet — The Hearth ships the anchor
graph, but nothing derives an interior or places a creature at an anchor. That
is The Threshold. This campaign's only obligation to the fine layer was to
choose a base resolution fine enough that its arrival needs no clock change: at
a hundred thousand ticks to the standard day a tick is roughly 0.86 seconds, so
a within-room step is representable, where at a thousand it would not have been.

Maintenance conditions — "she was interrupted" — need a condition that holds
*throughout* an action rather than at its entry, and v1's actions are
instantaneous-with-a-cost rather than genuinely durative. Nothing yet happens
*during* one. That lands with the first action long enough to interrupt, and
Allen's interval algebra is the settled vocabulary waiting for it, as jointly
exhaustive and pairwise disjoint as the region calculus The Hearth borrowed, and
needed for the same reason: not while actions do not overlap.

Initiative as a *contested* quantity — who acts first when it actually matters —
is a combat concern, and combat is ordered after vitality.
