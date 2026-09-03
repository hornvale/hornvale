# The Prospect

A prospect is both the view of what lies ahead and a place worth investigating.
This campaign set out to make the walk band say what is here, let you walk into
it, and widen what counts as a place from *somebody built it* to *there is
something here*. It did all three. Then it measured how often that happens, and
the measurement is the campaign's real output — because the number says the
promise cannot be kept by the mechanism that was being improved.

The promise, in the words that framed the work: **every square mile, something
interesting.** Standing at seed 42's flagship before this campaign, the world
said:

> Tropical seasonal forest — buttressed canopy, sun-warmed, damp, on a rise —
> in the lands of Doaba.

`enter` worked there. Nothing said so. What you entered was *"a small room,
holding a doorway and a screen"* — anonymous. Doaba was named as a region you
were **in**, never as a place you could walk **into**. Three tiles east, 3.4 km,
the answer was *"Nothing here is built; there is nothing to enter."*

Meanwhile 103 exotic sites sat in seed 42 — each with a vertex, a biome, and an
authored reason it is strange, *"grown with mineral crystal, with biota found
nowhere else"* — reachable only through a debugging flag whose own
documentation called them *"generated but unreachable."*

## Two lattices, and the ratio between them

Hornvale stores the world on two grids of very different grain.

| lattice | role | count | spacing |
| --- | --- | ---: | --- |
| geosphere vertices | the field substrate — climate, tectonics, elevation | 40,962 | 110–132 km |
| walk-band facets | the surface an occupant stands on | 402,653,184 | 1.126 km |

Every feature this campaign made enterable is born on the **coarse** lattice.
A cave's existence is answered per vertex; the exotic sites are vertex-placed;
a settlement's territory is a set of vertices. So the ratio between the two
grids is not a detail of implementation — it is a **ceiling on how dense the
inhabited world can be**, and it is available before any seed is built:

$$
\frac{40{,}962}{402{,}653{,}184} = \frac{1}{9{,}830} \approx 0.0102\%
$$

At most one facet in 9,830 can hold a placed feature of a given kind, even if
every vertex on the globe held one. Admitting more kinds raises the ceiling in
proportion and no further — the measured ceiling admits a cave and an exotic
site at every vertex plus the settlements a world actually founds, which is
**2.01x** the single-kind figure rather than threefold. This arithmetic is what the campaign's two preregistered rate
hypotheses ran into, and neither survived contact with it.

## H2 was falsified by a factor of a thousand, and no constant could have saved it

H2 predicted that 1%–8% of land facets would hold a cave — a band chosen so
that *"below 1% the widening buys nothing; above 8% caves stop being
remarkable."* Measured over the world's one cave predicate on five seeds:

| seed | vertices holding a cave | placed on land | pooled |
| ---: | ---: | ---: | --- |
| 42 | 874 | 827 | |
| 13 | 1,647 | 1,528 | |
| 7 | 1,681 | 1,584 | |
| 1 | 1,116 | 1,033 | |
| 100 | 2,440 | 2,360 | |

Pooled exact rate: **9.34 × 10⁻⁶** — 0.00093% of land facets, roughly **1,070×
under the band's floor**. The sampled rate over 10,000 drawn facets was zero,
against 100 hits expected at the floor; a sample of that size cannot resolve a
rate this small, which is itself the finding.

The important part is *why nothing was retuned*. H2 was frozen while the spec
still described a cave as a per-facet derivation from a field, where a
percentage of facets is a coherent target. A mid-campaign correction made caves
**placed** — one facet per warranting vertex — and a placed point process on a
41,000-vertex lattice **cannot express a per-facet percentage at all**. It was
not a constant that missed the band. It was the quantity.

## H3, which had no predicted value, is the number the project needed

H3 asked only: what fraction of land facets holds at least one site of any
kind? No prediction, because nobody had the number. Recording it was the
success criterion.

| seed | settlement | exotic | cave | union | on land | exact rate |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 42 | 389 | 103 | 874 | 1,366 | 1,297 | 1.087 × 10⁻⁵ |
| 13 | 259 | 143 | 1,647 | 2,049 | 1,892 | 1.393 × 10⁻⁵ |
| 7 | 250 | 180 | 1,681 | 2,111 | 2,000 | 1.027 × 10⁻⁵ |
| 1 | 301 | 136 | 1,116 | 1,553 | 1,455 | 1.014 × 10⁻⁵ |
| 100 | 60 | 175 | 2,440 | 2,675 | 2,585 | 1.406 × 10⁻⁵ |

Pooled: **1.1875 × 10⁻⁵ — one enterable site per ~84,200 land facets.**

A walk facet at depth 13 is 1.126 km on a side, so a square mile is **2.04
facets** and the stated aim is one site per two of them. The shortfall is
therefore **~41,200x — 4.6 orders of magnitude.** (An earlier draft of this
paragraph said ~84,200x, having read the *rate* as though the aim were one site
per facet. The rate is right; the conversion was missing. The stale "~1.7 km"
in `windows/vessel`'s own depth documentation, a pre-cube-sphere figure, is
where that error came from.)

And the decisive figure is not the shortfall but the **headroom**: shipped
worlds sit at **5.8%** of what the placement mechanism can possibly deliver.
Saturating it — a cave and an exotic site at every vertex on the globe, atop the
settlements a world actually founds — buys about **17x**, and leaves the world
still ~2,400x short of the promise.

So the answer to *"is this a rendering problem or a generation problem?"* is
neither. It is the **shape of the model**. Density cannot be bought by tuning
thresholds on a lattice with 41,000 points, because 41,000 things cannot fill
402 million places. That conclusion is now pinned by an assertion that reddens
the day the globe level, the walk band, the tier set or a site's extent changes,
so the finding cannot quietly rot into folklore.

## What the campaign shipped, and the vocabulary it settled

The enterability gate stopped being a boolean named `built`. That word means
*made by hands* — false of a cave, which water dissolved, and false of a fungal
canopy, which grew — so widening it would have placed a falsehood in the
predicate every enterable place hangs from. The gate became a **site**, and
`built` narrowed to what it always honestly meant: one property of a
settlement. **Two** uses of it survive deliberately, both because *constructed
or natural* is exactly the question `built` can answer: one chooses between
rectilinear rooms and a grown interior, and the other picks the noun a place is
described by — a settlement's interior is a *room*, a cave's is a *hollow*.
Rewriting either to consult the site instead would generate every cave as a
rectilinear building, and describe it as a room.

A site also gained an **address**. Its existence is answered per vertex, but a
vertex spans 110–132 km, and reading *is there a cave near me* off that lattice
would smear one cave across every facet for tens of kilometres — the defect the
project already carries in its water labels, where a per-vertex label paints a
river across a whole band. A seeded draw re-sites each placed feature onto one
facet inside its own vertex's territory, keyed on position and never on
generation order. Existence and address are now separate questions.

Above all, the campaign named the two tiers the surface layer will need:

| | **placed** | **derived** |
| --- | --- | --- |
| born from | a vertex, by a seeded draw | noise at facet resolution |
| count | bounded, ~10²–10⁴ planet-wide | unbounded, ~10⁶ and beyond |
| deterministic by | **record** — facts in the ledger | **derivation** — a pure function of seed and position |
| costs | a stream label, a save-format contract, storage | **nothing** |
| may shape world history | yes | no |

A placed delve is a kingdom whose fate other systems read: invade it and the
things that come out can devastate the countryside, because what happened to it
is a committed fact. A derived delve may be every bit as large, as intricate,
and as ruined, and leaves no mark on the world's evolution. The distinction is
not a matter of taste — it is the ratio above, made into an architecture. And
because a derived feature is a pure function of seed and position, it is as
fixed in place as a placed one; determinism is what lets it cost nothing, since
storage is only needed to make an arbitrary thing stable.

The naming argument is worth recording because the obvious answer was wrong.
The first proposal named the *criterion* — whether a feature participates in the
record — rather than the mechanism. An inversion pass killed it: a derived site
a player enters **joins** the record, so that vocabulary names a mutable state
while the generation tier is immutable, conflating two orthogonal axes.
Implication-mining then surfaced a third position the two-name scheme would have
hidden entirely: a promoted derived site is in the ledger but **never shaped the
past**. It can shape the future and not the history.

## The map now shows what is there without saying who

A placed site's glyph draws whether or not it has been discovered; its proper
name is still withheld until it has. The world map had shown only discovered
sites, which on a world with 874 caves and 103 exotic sites all undiscovered at
genesis meant unbroken biome and nothing to walk toward. Regional context is
fair — *a small village in the rolling hills of Blorble* — while the village's
own name is not.

This was cheap because the two gates were already separable, and one of them
had said so in writing: the link a cursor readout resolves carries the real name
and documents that it is *"never withheld or replaced here"*, deferring the
decision to the formatter. The map's site record carries no name field at all,
only a character and a colour. So drawing the glyph could not leak an identity.
Landscape extents — a volcano — stay gated, and for a structural reason rather
than a stylistic one: a volcano's identity flows through the readout machinery,
so its drawn-ness and its name are coupled in a way a site's are not.

## Corrections this campaign made to itself

Four claims in its own design document were wrong, and the record says so
where a later reader will meet them rather than in a footnote.

Caves were specified as needing **no seeded draw**, on the reasoning that the
proneness function is pure and therefore answerable anywhere. The function is
pure; its *inputs* are vertex-bound, so proneness exists only at 110-132 km
spacing and thresholding the nearest vertex would have made every facet for
tens of kilometres a cave. The precise form of the error is worth keeping: a
function was checked for purity and its answer inferred to be available
everywhere, without checking where its inputs live. A second correction
followed the first — the invented threshold was deleted outright once it
emerged that the world already had a cave model, better on four axes, which
nobody had looked for.

The re-siting draw was specified as minting an **epoch** — a deliberate break
in which every existing world regenerates differently. It mints none. The
project's rule is that a *new* seed label is safe and only a changed or reused
one is an epoch; a new label consumes no draws from any existing stream, and
site placement is new behaviour with no prior placement to move. Both
pin-isolation suites stayed green and the golden rebaseline was a no-op. The
campaign cost no epoch at all, and a subsequent decision had to withdraw an
argument that had been resting on the epoch being paid for anyway.

The prose was specified to name **at most two** sites at a facet. One is what
shipped, and the type forbids two. The number was wrong and the rationale — *the
fiction is a world that does not inventory itself* — argues for one more
strongly than for two.

The map's discovery gate was listed as *existing behaviour that must survive*.
It was reversed by direct ruling, and for a while that reversal lived only in
code comments while the design document still asserted the opposite — which is
the shape of defect that produces wrong answers from readers acting in good
faith, since the next person to consult the document would have found the
pre-reversal rule and restored the code to match it.

## What the next campaign inherits

A measured baseline, an architecture for exceeding it, and one tier of that
architecture modelled but unbuilt. The derived tier costs no determinism
contract — no stream label, no epoch, no stored bytes — which is what makes
*unbounded* affordable and why a surface layer can be built without touching any
save-format contract.

One instrument is waiting for it. The rule that decides which site a facet
names when it holds several is correct, is the single authority for that
ordering, and **has no live case**: on all five measured seeds no facet anywhere
holds two kinds, because each kind's draw deliberately sends it to a different
facet within the vertex's territory. It will start mattering the moment features
arrive in their millions with no such separation — which is the next campaign's
whole subject.
