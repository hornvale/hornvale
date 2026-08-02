# The Namesake

The world names its places, its gods and its epithets. It did not name its
people. This campaign built the two things a personal name needs — a relation
between one individual and another, and a grammar that can cite it — and then
measured whether the naming rule it shipped earns its keep.

It does not, and the reason is the campaign's result. The rule that computes a
name's length from how much disambiguation the moment demands is correct,
tested, and almost never called upon, because the given-name draw underneath it
produces names that scarcely ever collide. The rule is priced for a collision
rate the name generator declines to produce.

Two earlier claims died the same way, before a line of the implementation was
written. That is three measurements overturning three designs in one campaign,
and it is the shape of the campaign more than any of its code.

## The first falsification: a founding gap is not a generation

The ledger already commits community lineage. Every occupation records where
it was founded from, and on a seed-42 world 1 759 of 1 776 occupations carry
such an edge, with seventeen genesis roots and a tree running twenty-nine
links deep.

The design that opened this campaign read that edge as a genealogy directly:
the founder of a daughter community is the *child* of the founder of the
mother community, so the committed community tree *is* the person tree and
nothing further needs deriving.

Measuring the edge kills it. The founding gap between parent and child
occupation runs:

```
median  50 y     mean 106 y
p25     25 y     p75  150 y     p95 375 y     max 975 y
```

A median of fifty years is already two generations for every people on the
roster. Nine hundred and seventy-five years is not a parent-child link under
any lifespan the world contains. The edge is a *descent* relation at an
unknown remove, and the remove has to be derived rather than assumed.

The material for deriving it was already shipped. Generation length falls out
of the allometry from body mass and metabolic class — 21.7 years for a goblin,
30.2 for a kobold, 30.9 for a hobgoblin, 35.6 for a bugbear — computed on
demand and stored nowhere. Dividing each edge's gap by the occupying people's
generation length gives the remove between two named founders directly:

```
IMPLIED GENERATIONS between a founder and their forebear   (n = 1115)
  median 2 | p75 6 | p90 10 | p95 13 | p99 20 | max 32 | mean 3.8
  60% of edges resolve in <= 3 implied ancestors
  13% are ZERO-hop -> same generation -> the two founders are SIBLINGS
```

Three things fall out of that distribution rather than being designed. Walking
a lineage is cheap, because the median walk is two hops and the deepest is
thirty-two. Siblinghood is free — thirteen per cent of edges resolve to zero
generations, which *is* a sibling relation, obtained without modelling
siblings at all. And because generation length varies by a factor of 1.6
across the roster, a goblin lineage runs roughly 1.6 times as many
generations as a bugbear lineage over the same two thousand years, so lineage
depth is genuinely species-differentiated.

On the seed-42 world the resulting graph resolves 1 776 founders into 211
sibling relations, 1 548 ancestral ones, and 17 with no forebear at all — a
count identical to the seventeen genesis roots, which is the arithmetic
saying the walk terminates exactly where the ledger says it should.

## The second falsification: an inherited name is not a fossil

The campaign's original headline was prettier. An inherited element — a clan
name minted once at the root of a lineage — would run through the language's
sound-change cascade for the whole age of the clan, while an element
re-derived each generation from a living parent's name would always sound
current. Archaism would then *measure* inheritance depth, and because
generation length differs by species, different peoples would archaise at
different rates.

It was withdrawn on two measurements, both taken before implementation began.

The first is that the drift rate does not vary across the peoples at all.
Drift is keyed on social form, and goblin, kobold, hobgoblin and bugbear are
every one of them settled, so all four draw at one identical rate. The regime
only separates gregarious beasts, long-lived solitaries and sessile
organisms. The species-differentiation half of the claim was false by
construction, and "measuring" it would have produced a null that was an
artifact of misreading the engine rather than a fact about the world.

The second was worse. Counting which sound rules actually fire in the
committed seed-42 dictionary gave zero for goblin across seventy rows, zero
for hobgoblin across seventy-four, zero for kobold across seventy-eight. For
three of five peoples an inherited element and a re-derived one would come out
byte-identical. There was no signal to preregister against.

**One world is an anecdote, and this one was.** Rather than take a single
seed's reading as a fact about the engine, the campaign's first piece of work
was an instrument: a metric counting distinct sound rules fired, run across
two hundred worlds. It reports goblin firing a mean of 1.305 rules and zero
on 19.0% of worlds; bugbear a mean of 1.220 and zero on 22.0%. The modal
outcome for both is one or two rules firing. Seed 42's goblin reading of zero
is a real but minority tail, roughly one world in five — not the engine being
decorative.

So the cascade is **not** systemically inert, and the suspicion that opened
this campaign is retired. The fossil claim stays unbuilt for the other reason
only: with every settling people drawing at one drift rate, there is still
nothing for archaism to differentiate.

Building the instrument also exposed a defect in the instrument. Its first
version deduplicated rule firings by position in the cascade rather than by
rule *kind*, while its own documentation promised distinct kinds. Across four
hundred measured cases the two readings differ twice — and one of those two
was the sole occupant of the published high bucket, so the histogram's tail
was an artifact of a definition that did not match its own prose. Corrected,
that bucket is empty.

## What a name is

A name here is an ordered list of elements, each carrying where it came from
and who conferred it:

```rust
pub struct PersonName { pub elements: Vec<NameElement> }

pub struct NameElement {
    pub source: ElementSource,
    pub author: Author,
    pub conferred: Option<f64>,   // None for conferred-at-birth
}

pub enum ElementSource { Stem, Gloss(GlossBasis), Relation(Cite), Index(IndexBasis), Deed }
pub enum Cite { Parent, Clan, Community, Place, Deity, Mentor, Child }
pub enum Author { Kin, Community, Witnesses, Institution, Selfward, Outsiders, Inherent }
```

Two of the five sources were already built and merely aimed at settlements:
drawn phonology, and compounding from a site's concepts. Only the relational
source needed the new graph.

The authorship axis is the one the surface vocabulary hides, and it does more
work than it looks like it should. Who conferred an element determines whether
it can be revoked, who may confer another, and whether it can be *false*. An
outsider-authored element is one the bearer cannot revoke — which is exactly
what a deity's epithet already is, so epithets become a special case of this
system rather than a parallel one. Self-authored is the only source that can
lie.

One variant ships with no resolver behind it. Teknonymy — the parent named for
the child, Arabic *Abu Bakr*, "father of Bakr" — is derivable from the same
graph walked backwards, and the schema carries the variant so the shape is
complete. Nothing in the engine resolves it yet, and the chapter says so
rather than counting it as delivered.

## The pattern is derived, never authored

Which elements a people uses is a function of what the people already is. A
per-culture authored naming table would be exactly the lookup table this
project has rejected repeatedly; instead the pattern reads off the society
vector that already exists:

| Society dimension | Effect on the pattern |
|---|---|
| hierarchic sociality | kin or institutional authorship, citing a parent or a clan — *descent legitimates* |
| communal sociality | community authorship, citing the community or a deed — *what you did legitimates* |
| status by rank | an honorific prefix (already shipped) plus a parent citation |
| status by knowledge | a mentor citation — the transmission lineage, the anthroponymic twin of language drift tracking mentorship distance |
| status by generosity | a deed |
| in-group radius | how many elements the pattern carries — insular peoples need fewer to tell each other apart |
| stereotypy exponent | how uniform the pattern is across the culture |

Across the shipped roster that yields four distinct signatures from five
peoples:

```
goblin      [Stem, Relation(Parent), Relation(Clan)]
kobold      [Stem, Relation(Mentor)]
hobgoblin   [Stem, Relation(Parent)]
bugbear     [Stem, Relation(Parent)]
gnoll       [Stem, Deed, Relation(Clan), Gloss(Bearing)]
```

## The shortest-prefix rule

Rendering takes a name and a scope, and returns the shortest prefix of its
elements that is unique among the names in that scope: the given name inside a
household, given-plus-byname inside a settlement, the full stack across a
region. Length is computed at the point of utterance, never authored into the
name.

This is a generalisation, not an invention. The world already ratified that
uniqueness is a property of a *reference* and not of a name, accepting that
committed settlement names may collide and disambiguating at render time from
the sites' own facts, as Earth accepts its forty-one Springfields. The
structure is git's shortest-unique-SHA prefix, DNS search-domain suffixing,
and *E. coli* after the first *Escherichia coli*.

The rule's own documentation states the premise it rests on:

> Personal names collide far harder than toponyms and *should*: Earth's
> commonest given name is borne by tens of millions.

That sentence is where the campaign's result lands.

## What the measurement said

Four criteria were frozen before the code existed, and measured over two
hundred worlds. No constant was retuned to rescue any of them.

| | criterion | measured | |
|---|---|---|---|
| A1 | ≥ 3 distinct pattern signatures | min **3**, mean **3.99** | **pass** |
| A2 | people recoverable from name structure at ≥ 2× chance | **2.99×** chance | **pass** |
| B1 | ≥ 80% resolve in one element at settlement scope | mean **99.53%**, min **86.05%** | **pass** |
| B2 | median ≥ 2 elements at region scope, and < 50% need the full stack | median **1.0**; full stack **35.2%** | **fail** |

The first three hold, and two of them hold comfortably. The patterns really
do differ between peoples, and a figure's people really is recoverable from
the shape of its name alone at three times chance.

The fourth fails, and it fails on the arm that says disambiguation pressure is
real. The median figure resolves in **one** element at the widest scope the
world offers. Only four worlds of two hundred reach a median of two. The
second half of that criterion — fewer than half needing the full stack — holds
easily at 35.2%, which is the null's other pole saying the names are not
merely long.

Two honesty notes belong with those numbers. The two structural criteria are
measured on 156 worlds rather than 200, because forty-four worlds place fewer
than two peoples and the metrics correctly report absence rather than
inventing a reading. And two worlds sit *exactly* on the recoverability bound
— a margin of zero, not a comfortable clearance.

## Why the failure is real, and what it means

A null is only worth something if it cannot be explained away, and there were
two ways to explain this one away. Both were checked and both fail.

The first is that names were being shortened by an accounting artifact. Three
of the five element sources this campaign shipped have resolvers; a mentor
citation, a deed and a per-person gloss do not, and an element with no
resolver is *dropped* rather than filled with a placeholder — filling it would
fabricate disambiguating entropy the engine does not have. So the question is
whether perfect filling would rescue the median. It would not, and the proof
is stronger than a sample: for any founder carrying at least two elements,
spending exactly one is *equivalent* to that founder's stem being unique in
the whole world. That equivalence was machine-checked across fifty-four seeds
with zero mismatches, which makes the maximum-fill counterfactual computable
rather than estimable — and under it, no world with a median of one flips to a
median of two.

The second is that "region" was the wrong scope. It was the *most generous*
one available: every founder in the world, across two thousand years of
history. A true spatial region is a subset of that, and a smaller competitor
set can only push the median down. The criterion may in fact have been
unsatisfiable by any naming system — which is a different failure from the one
it was written to detect, and is recorded as such.

What is left is a mechanism, and it is the campaign's real finding. Person
name stems draw from an effective space of some five to seventeen thousand
forms, which makes between 62% and 99% of a world's given names unique within
that world. Disambiguation pressure never materialises because there is
nothing to disambiguate.

The contrast was already sitting in the repository. The instrument that
measures settlement and deity name collisions reports a mean rate of **0.567**
across the census — those names collide constantly, which is why the
reference-time disambiguation rule was built for them in the first place.
Personal names, in the same world, in the same engine, barely collide at all.
Real given-name distributions are steeply Zipfian: a handful of names carry an
enormous share of the population, which is precisely why patronymics and
bynames exist. The person draw is near-uniform.

So the shortest-prefix rule is not dead code, and the difference matters. It
fires — 13.4% of founders on seed 42 do extend to a second element, and the
settlement-scope margin is 86% at its worst rather than a flat 100%. It is
built correctly against a collision rate the *name generator* declines to
produce. The defect the measurement found is upstream of the rule that
measured it, in the draw, and it names its own successor work: give personal
names a realistic frequency distribution, and the rule that already exists
will start earning its keep without a line of it changing.

## What it cost

Nothing. No fact is committed, no entity minted, no stream drawn, no seed
label added, no epoch declared. Every name is a pure total function of facts
the ledger already carries plus the world seed, and the whole artifact set —
the gallery, the reference dumps, the laboratory studies — regenerates
byte-identical. That claim was asserted at the one point in the campaign that
touched a save-format contract, and again at the close, rather than assumed.

The place where this would stop being true is named and held back: a
*committed* settlement name citing a *derived* person name binds the
derivation forever, so changing the naming function afterwards would silently
rewrite every saved world's toponyms. "Shaman's Creek" is where the epoch
lands, and it is not here.

What the campaign leaves is a lineage graph that costs nothing to carry, a
naming grammar derived from what a people already is rather than authored per
culture, a rendering rule that computes name length instead of asserting it,
and a measured account of why that rule is currently idle — which is not a
fact about the rule.
