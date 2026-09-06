# The Hidage

*The Tribal Hidage assessed each people's land in hides — the unit of worked
land that feeds one household — and it did the assessing before anyone
ploughed. A hide is not a measure of ground; it is a measure of what ground
will carry. This campaign assessed the world's catchments before the engine
was taught to feed a settlement from one, and the assessment came back saying
the ploughing would change nothing worth having.*

The predecessor campaign left a metaplan: nine things that must become true
between a dwelling and a city, split into readings and dynamics, with a
standing rule that no dynamics rung may start without a preregistered probe
that could kill it. The first dynamics rung was *a settlement has worked
land*. This campaign is that probe, and nothing else: one measurement, one
verdict, no mechanism.

## The question, in a form that can come back "no"

Today a community grows toward the carrying capacity of the single vertex it
stands on. The proposal was to grow it instead toward the summed capacity of
its **catchment** — the whole watershed of ground that drains toward it. The
question the probe had to answer was not "is that more realistic" but:

> If the deep-history bake grew a community toward its catchment instead of
> its own vertex, would any settlement become something other than a hamlet —
> and would only some?

Both halves matter. A world where nothing clears the hamlet ceiling has not
gained a city. A world where *everything* clears it has not gained one either:
that is a uniform rescale, and the engine already has a constant that performs
uniform rescales.

## What the code said, before anything was measured

The brief and the metaplan both stated that settlement genesis reads a
catchment while the history bake discards it — two live halves of one model
disagreeing, and the rung was framed as reconciling them. Checked against the
tree first, that turned out to be one clause stale. The bake has been the
settlement provider since an earlier campaign replaced the condensation
placer; the catchment code's only caller is the laboratory's report accessor,
which no build stage runs. So **both** halves of the model that decide where a
settlement is and how large it grows are single-vertex reads, and the
watershed model exists only as an instrument.

That correction made the rung smaller and the probe harder. Smaller, because
D1 is "wire in an instrument that already exists" rather than "reconcile two
mechanisms". Harder, because no committed artifact anywhere carries a
catchment distribution on the field that matters — the probe had to build one,
on the bake's own growth field, from public API only.

## The measurement

Five worlds at default pins (seeds 42, 7, 13, 100 and 1234 — one world is an
anecdote). For each settling people, the probe rebuilds the bake's growth
field at the present era, runs the demography flow algorithm over it — every
land vertex climbs to its highest-capacity neighbour, and a vertex's
*accumulation* is the sum of capacity over everything whose up-path passes
through it — and reads three populations off the result: the alive bake
settlements, the ended ones, and the top-`N` **attractors** by accumulation,
where `N` is that people's own alive settlement count. That last population is
the counterfactual stated as a number: *if this world's current settlement
count sat at its largest catchments instead of where it sits, what would each
be worth?*

The death criterion was frozen before the probe was written, and it is a
**count**, not a dispersion statistic. The brief had proposed the dispersion
test — *if accumulation is spatially flat, every catchment sums alike and this
is a rescale in disguise* — and it could never have fired. The flow algorithm
is the terrain drainage algorithm with the gradient flipped, and
drainage-basin sizes are heavy-tailed by construction on any field the
algorithm is run over; a flatness test would have returned "not flat" on
every world and read like a passing guard. What replaced it counts how many of
the top-`N` catchments clear the hamlet population ceiling the engine already
carries, and names **two** dead poles: none of them, and nearly all of them.

## The readout

Taken at `20f48585e`; five world builds and one flow field per settling people
per world, 17.767 s of wall time:

```
  seed   N_s  ended   c_s/N_s  c200_s/N_s  occupied  S1     S2     S3     S4
  ----   ---  -----   -------  ----------  --------  -----  -----  -----  -----
  42     390    822   390/390     388/390    21/390  0.352  0.323  0.398   5.60
  7      250    406   250/250     250/250     8/250  0.262  0.218  0.614   3.13
  13     262    799   262/262     262/262    17/262  0.446  0.366  0.542  11.24
  100     60     99     60/60       60/60      2/60  0.328  0.265  0.754   2.93
  1234    44    870     44/44       44/44      3/44  0.320  0.238  0.813   3.07

  seed   catchment accumulation      attainment (peak/K)   hops to      sharing an   K==0
         min      median     max     median  min   max     attractor    attractor    sites
  ----   -------  --------  ------   ------  ----  ----    ---------    ----------   -----
  42       183.2   1504.1   8424.7     0.66  0.07  2.05    med 2 max 9    354/390       2
  7        387.6   2054.4   6421.6     0.82  0.08  1.54    med 1 max 5    211/250       3
  13       275.2   1365.7  15352.3     0.65  0.05  1.02    med 1 max 7    208/262      19
  100      513.1   2929.3   8571.4     0.40  0.08  0.91    med 1 max 6     28/60        1
  1234     349.1   2297.2   7061.0     0.11  0.05  1.13    med 1 max 5     12/44        3
```

`c_s` is the count of top-`N` catchments clearing 150, the hamlet ceiling;
`c200_s` the count clearing 200, the longhouse floor. `S1` and `S2` are Gini
coefficients of accumulation and of the catchment multiplier, `S3` the rank
correlation between a catchment's accumulation and its own vertex's capacity,
`S4` the ratio of the largest accumulation to the median. Only 52 of seed 42's
390 alive settlements are attractors of their own people's field; the rest sit
somewhere on a slope leading to one.

## The verdict

**Every top-`N` catchment on every seed clears the ceiling.** Not most —
all of them, at the extreme value of the rule's own fraction, and all but two
of seed 42's clear the longhouse floor as well. The frozen rule returns
**RESCALE**: D1 as stated is struck.

The rule carried one caveat, and it was worked rather than waved. The bar is
on summed *capacity*, and a community's realized population sits at or below
its ceiling — the `attainment` column measures how far below. Two of the five
worlds have a median attainment under the caveat's trigger of 0.5. So the bar
was divided by each world's own median attainment, giving corrected bars of
227, 183, 231, 375 and 1,364 headcount, and the catchments still clearing them
counted: at least 378 of 390, then 250 of 250, 262 of 262, 60 of 60, and 35 of
44. A majority survives on every seed, tightest on seed 1234. The margin is
not close: the median top-`N` catchment holds 1,366 to 2,929 headcount of
capacity against a 150 bar.

## What the predictions did

Four characterizations were stated before the probe existed, so that being
wrong would be visible. Two held and two failed.

**Held:** accumulation is heavy-tailed on every seed (Gini 0.262 to 0.446,
above the frozen floor of 0.25) — which is the hydrology argument confirming
itself, and the reason the flatness criterion was replaced. And most
settlements are not attractors of their own field (0.13 to 0.40 against a
frozen ceiling of 0.5), so the change would have moved settlements as well as
resizing them.

**Failed, and the more interesting of the two:** the prediction that a
catchment's size tracks its own vertex's capacity, at rank correlation 0.7 or
better, holds on only two of five worlds (0.398, 0.614, 0.542, 0.754, 0.813).
The biggest vertex is not reliably the biggest basin. Had D1 been built, it
would not merely have made settlements larger — it would have **re-ordered**
them, and the rung downstream that derives comparative notability from a
place's dominance over its neighbours inherits a different question than the
metaplan planned for.

The other failure is a fact about today rather than about D1: median
attainment is 0.40 and 0.11 on the two small worlds, which says the ceiling
binding those communities is not capacity at all. Lifting a capacity ceiling
would have done nothing there.

## The honest limits

- **The field is a present-era reconstruction.** The bake's final simulated
  era carries its own temperature offset and sea level, not the present's, so
  a community founded and grown against a colder, lower-sea-level world can
  outlive the ground that fed it. That is not hypothetical: 2, 3, 19, 1 and 3
  alive settlements per seed stand where the present-era field gives them
  exactly zero capacity. Those sites are counted and printed, never dropped.
  Every ratio in the table is taken over one field, so the era choice moves
  numerator and denominator together; the *count* is the statistic the
  approximation could move, and its margins are an order of magnitude wide.
- **The bar is on capacity, not population.** Capacity is what a place could
  carry; the attainment column is the only thing that says how much of it is
  taken up. The verdict rests on the corrected-bar arithmetic above rather
  than on capacity alone.
- **Two other fields were not summed.** The bake *sites* a community from a
  different field again — capacity times a river factor — and the laboratory
  keeps a third, dimensionless suitability field whose settlement counts the
  metaplan had been quoting. Neither is the field a community grows against,
  so neither is measured here; the metaplan's "~22 people per catchment" is
  the laboratory's field at a threshold retuned twice, and never was
  comparable with a headcount.
- **One count is per settlement-entry, not per place.** The multi-people
  attractor figure counts each alive `(vertex, people)` record whose vertex is
  shared, so a vertex hosting two peoples counts twice. Read it as "settlement
  records at a shared attractor", which is what every neighbouring statistic
  in the table means.
- **The probe's own text had a defect, and a reviewer caught it.** The path
  length from a settlement to its attractor closed with an assertion that
  compared a value against itself whenever the flow field had no attractor to
  offer — exactly the zero-capacity sites the run had just surfaced. It was
  fixed and the readout re-taken. **Not one number moved**: the spurious zeros
  had landed among genuine zero-hop sites that are their own attractors, so
  the defect corrupted a statistic without disturbing it. That is the argument
  for repairing the assertion rather than re-checking the output.

## What happens next

D1 is struck and the dynamics arc re-plans from D2 — split people from
subsistence, and put a voluntary exchange beside the coercive one — whose own
probe is the next campaign under the same standing rule.

The record is careful not to overclaim in the other direction. The catchment
field **does** carry an apex: its largest basin is 2.9 to 11.2 times its
median, and the distribution is genuinely uneven. What died is D1 as
specified — the catchment wired in as the growth ceiling *at today's scale*,
which lifts everything over the ceiling at once. Whether a rescaled catchment
would make a differentiated apex is a question about the constant that turns
suitability into headcount, and that constant's future moves out of the
metaplan's open list and into D2's opening brief rather than being answered
here.

Four observations go back to the metaplan as material rather than findings.
Bake settlements cluster into remarkably few watersheds — 354 of seed 42's 390
share an attractor with another settlement of the same people — so a catchment
per settlement would need a splitting rule before it could be wired anywhere.
Alive settlements with zero present-era capacity exist. Attainment above 1.0
occurs, up to 2.05, which the collapse-pressure threshold permits. And
occupancy is low: 21 of seed 42's 390 top-ranked catchments carry an alive
settlement of that people, 2 of 60 on the smallest world. The places the flow
field would rank first are mostly not where anybody lives.

Nothing in world state moved. No epoch, no census, no constant, no label — one
committed measurement anybody can retake, and a rung the project now knows it
does not want in the form it was written.
