# The Axes

A distinctive feature, in phonology, is not a sound. It is one of a dozen
questions you can ask about a sound — is the airflow stopped, is the larynx
buzzing, is the tongue high — and a phoneme is the answers taken together. The
insight that made the twentieth century's phonology possible is that the
questions are *fewer than the sounds*, and that the sounds are their
cross-product rather than a list.

Hornvale's world had a list. Twenty-one `Formation`s, hand-authored: tundra,
taiga, reef, karst cave. Fifty-three `Variant`s beneath them. Each one an atom,
each one meaning whatever its name suggested, with nothing shared between them
that a program could read. This campaign asked whether that list is a
cross-product in disguise — and, because the question is easy to answer badly,
asked it in a way that could come back *no*.

## The criterion that could not fail

The metaplan's §7 gated this campaign on a **reconstruction test**: assign an
axis vector to all 74 names; two names colliding on one vector means the axes
are too coarse, a name that resists assignment means the list is incomplete.

That criterion is satisfied perfectly by one axis with twenty-one values —
which is the enum. Any corpus is reconstructible by a sufficiently fine axis
set, so "it reconstructs" is not evidence of anything. The test had a floor and
no ceiling, and it would have passed on the degenerate solution while looking
like a result.

So the first thing this campaign built was the missing half:

| clause | requirement | derived from | measured |
|---|---|---|---|
| coverage | product of axis cardinalities ≥ 500 | ≥ 74 needed to distinguish the corpus; 500 is ≈7× headroom | **6,480** |
| compression | sum of cardinalities ≤ 30 | the enum costs 74 symbols; 30 is under 41% of that | **29** |
| collisions | no two names share a vector | §7's original floor, kept | **0** |

The bounds derive from *corpus size*, which was known before any assignment
existed — which is what makes them auditable as un-tuned, and what made it
possible to freeze them before the fit rather than after.

Five axes, quantised: physiognomy at six levels, energy at five, water at six,
substrate at six, light at six. Twenty-nine knobs describing seventy-four
names, in a space of 6,480 cells.

## What the sixth axis measured by staying empty

Six axes were proposed. Five carry values. `DISTURBANCE` carries none —
anywhere, in the entire corpus.

It is the basis's only `Rate` axis, and that is the whole explanation. A
formation is a *state*; disturbance is a *frequency*. Speed a rate up and it
becomes a state (everything always burning is a stable community); slow it down
and it vanishes into the climax. There is no value on it that a
whole-community name can carry, so every name that would need one carries the
unassigned vector instead.

This is the measured form of a fact the metaplan could only assert: nothing in
the workspace computes disturbance. The basis now ships with a declared axis the
entire existing vocabulary cannot occupy, which is a more useful shape for that
gap than a sentence in a document. Campaign 4 is what fills it.

## The prediction, named before the fit

The campaign's spec froze a list of ten names predicted to resist assignment,
on one sentence of reasoning: *a phase is not a point in a state space.* Six are
post-event succession — `forest-gap`, `mossy-deadfall`, `burn`, `fire-scrub`,
`reef-rubble`, `urchin-barren`. Four are seasonal phases of sea ice —
`pressure-ridge`, `ice-lead`, `rafted-floe`, `melt-pond`.

Measured: exactly those ten, no substitutions.

A prediction that names its members in advance can fail in a way a count
cannot, and this one did not. It also corroborated itself from an unexpected
direction: a vacuity guard on an unrelated test asserted the number of assigned
*marine* names and came back two short, because `reef-rubble` and
`urchin-barren` are post-event succession in the sea exactly as `burn` and
`forest-gap` are on land. The phase model predicted its own arithmetic error.

## A genus does not fix what its species disagree about

The first fit produced two collisions: `ice` with `snowfield`, and `desert`
with `erg`.

Both are a formation colliding with its own default variant. An erg *is* what a
desert is on sand; a snowfield *is* what permanent ice is, unbroken. The axes
were not too coarse — the error was pinning a genus to one species' values.

The correction is a rule, and the sparse vector already existed to express it:
**a formation declines any axis its own variants disagree on.** `Desert` does
not fix substrate, because `variant_pool` branches a desert into erg, playa,
hamada and reg precisely *by* ground. `Ice` does not fix light, because its
three variants are surface forms that scatter it differently.

That rule then had to be applied a second time, under pressure, because the
first application had been reactive.

## The coarse value is a boundary condition

The program's keystone corollary — adopted verbatim from The Rill — says that a
coarse value constrains a fine one rather than merely suggesting it. Nothing had
tested it. The corpus makes it testable, because it has two grains: formations,
and the variants beneath them.

The test needed a distinction the campaign had already been forced to declare.
A variant may **refine** its genus on a scalar axis — a damper hollow inside a
temperate forest is a finer reading of the same quantity. It may not
**contradict** it on a *nominal* axis, where the value names an unordered class:
a forest on soil does not contain a variant on ice. That is a different kind of
place, not a closer look at the same one. `AxisValence`, declared per axis
because the axes are not homogeneous, is what draws the line.

Eight variants contradicted their genus, all on substrate: `old-growth`,
`damp-hollow`, `closed-canopy`, `liana-forest`, `sargassum-drift`,
`marine-snow`, `nodule-field`, `trench-wall`.

One cause. The genus-declines rule had been applied only where a *collision*
forced it — to desert and ice — and nowhere else. Applied systematically, five
more formations decline substrate because their own variants disagree with them
on it, and all eight contradictions dissolve. The compression figures do not
move: sum 29, product 6,480, because other names still occupy those classes.

The rule written into the module's documentation had been broader than the rule
actually applied, and only an executable check found the gap.

## Two parents, and a boundary that binds nothing

Deriving each variant's genus from `variant_pool`'s own match arms — rather
than from the section comments in the assignment, which were written by the same
hand that would have been checking them — corrected two things.

`Burn` is a taiga variant, not a shrubland one. And several variants have **two**
genera: `Tundra | Alpine`, `TemperateForest | TemperateRainforest`, `Savanna |
TemperateGrassland`, `TropicalRainforest | TropicalSeasonalForest`.

Those pairs do not always agree with each other. Tundra is organic ground;
alpine is rock. A variant under both is therefore bound by *neither* on that
axis — a boundary condition binds only where the boundary is single-valued,
which is the genus-declines rule arriving one level up, unprompted.

## The affinity ladder was already a two-axis claim

The strongest evidence for the decomposition was written years before it, by
someone not arguing for it.

`domains/species` scores how well a people suits a biome on a ladder:
`AFFINITY_NEAR` at 0.70 is *"one band out in the classifier's lookup table"*,
and `AFFINITY_MARGINAL` at 0.45 is *"two bands out, **or the right climate in
the wrong form**."*

That second clause says two distinct moves reach the same rung — two steps along
climate, or one step along form. It is a statement that the biome space has at
least two dimensions with an exchange rate between them, sitting in a domain
that cannot see `Formation` at all, keyed by strings because the layering rule
forbids anything better.

If the axes are right, a species' better-liked biomes should sit nearer its
stronghold in axis space. Measured: **38 of 49 ordered pairs concordant,
77.6%** — well above the half a meaningless geometry would give, on a sample of
49 pairs across eight authored species. Reported rather than gated: retuning
axis values to raise that number would be fitting to the check.

## What the sea did not need

The keystone claims one space *in every realm*. The spec's design was to fit the
axes on land and then assign the marine names without revising the axis list.

That is not what happened. The implementer authored all seventy-four names in
one pass, and the blinding was gone before the arm ran. The campaign records
that rather than quietly downgrading it.

What survives is a question the committed data answers regardless of what
anyone intended: **did the sea use any axis, or any axis value, that no land
name uses?** A stretch is the failure the arm exists to detect, deliberate or
not. Comparing axis *identifiers* alone would have been satisfied by
construction — the assignment writes all five occupied axes for nearly every
name — so the check compares values, and that is the whole repair.

The sea occupies no axis and no axis value the land does not. The keystone
survives its first real test, in the weaker form the campaign is entitled to
claim.

## Factoring, not fidelity — stated at the strength it was measured

The metaplan suspected two of the six axes might be pure functions of inputs
`classify_land` already reads, and asked the campaign to measure which.

The land answer needs no measurement. `classify_land` is a decision tree: an
ice cut on temperature, an alpine gate on elevation against the tree line, then
a two-dimensional cut on temperature × moisture producing the remaining ten
formations. Any vector assigned *per land formation* is therefore a
deterministic function of exactly the inputs that function already reads — not
two of the six axes but **all of them**, provably, by construction.

So on land the decomposition is a **factoring**. It buys compositionality; it
cannot buy fidelity. Fidelity is only reachable from inputs `classify_land`
does not read, and this campaign identified three: substrate, which reads rock
and slope and soil depth; the realm column, which The Fathom built and nothing
consumes; and disturbance, which nothing computes at all.

The campaign says the weaker, true thing rather than the stronger, convenient
one.

## The proof

Every committed artifact was regenerated at the close and diffed. The world is
byte-identical: this campaign adds a module and consumes nothing, so no seeded
draw moved and no rendered surface changed.

The one artifact that did move is `docs/audits/type-audit-report.md`, which
drifts on any pub-boundary change and was regenerated in the same commit as
each of the `pub` items that moved it.
