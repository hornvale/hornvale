# The Kiln

A kiln — from Latin *culina*, a cooking stove — is the instrument of
irreversibility. Clay entering one is plastic, and clay leaving one is not;
firing is the transformation that cannot be walked back. It is the right name
for this campaign because the thing it went looking at turned out to be
irreversible too, and for a much cheaper reason: not because the physics
forbids the return trip, but because nobody wrote one.

Hornvale ships a technology model. It was never ratified, it is a clock, and
it cannot express loss. This campaign does not replace it. It freezes two
external catalogues against it first, scores them, and reports what the score
says — which is very nearly nothing, and the *shape* of the nothing is the
finding.

## The model nobody ratified

No ratified decision covers the four-rung technology horizon. Its entire
written rationale is one paragraph of
[The Living Community](./the-living-community.md)'s design — item seven of
twelve committed facts, in a record whose subject was the stratigraphy of
ruins:

> Coarse: a monotone function of world-age (the era) plus a per-people
> advancement rate, clamped to a small ordinal ladder (e.g. neolithic → bronze
> → iron → …). It is *committed* per occupation because a people's trajectory
> is globally dependent (contact, displacement); it is not re-derivable from
> the ruin alone.

Two sentences, written as a sketch, in a campaign about something else. They
have since been the only place the project states its own position on
technology — and because they describe what was subsequently built, they read
today as documentation rather than as the provisional note they were. That
drift is the reason this paragraph is quoted at length rather than summarised.
A sketch that nothing supersedes becomes the specification by default.

## It is a clock

The function behind the ladder says so in its own doc comment:

> The tech horizon for an (offset-adjusted) absolute `year`. Callers pass
> `year + per-people offset`; **monotone in `year`, so tech only ever rises.**

Below year 400 a people is neolithic; below 900, bronze-working; below 1400,
iron-working; thereafter classical. The horizon is a function of the calendar
and of nothing else — not of biome, not of surplus, not of population, not of
whether anyone is left who remembers how. **Loss is not unimplemented here. It
is inexpressible by construction**, in the same way that a monotone function
has no descending branch to leave unwritten.

What follows from the numbers is sharper than the monotonicity, and it is pure
arithmetic over three committed constants. The per-people advancement offset is
drawn uniformly on the integers `[0, 300]`. The history bake's default horizon
ends at year 2000. The top rung opens at 1400. So every living community's
argument to the clock lies in `[2000, 2300]`, at least six hundred years past
the last threshold the ladder has. **Every survivor is classical, on every
seed, necessarily.**

The committed gallery shows both halves of that. The biographical readout for
seed 42 carries ten community sketches at world-year 1699, and all ten of them
say the same sentence: *They had classical statecraft and engineering.* Ten of
ten, with no second phrasing anywhere in the file. Meanwhile the ruin
stratigraphy for the same seed carries seventeen occupation layers whose
horizons, read from the deepest upward, run one neolithic, six
bronze-working, one classical, three iron-working, and then seven classical to
the surface. **The ladder dates the dead. It does not describe the living** —
the whole of its observable variance is buried, and it is buried in the
literal sense that it only appears in strata.

## The one term that could have diverged, and does not

The sketch's argument for *committing* the horizon per occupation was that a
people's trajectory is globally dependent — "contact, displacement". That is
the right reason to commit a fact rather than re-derive it, and the mechanism
it describes was never built. The advancement offset is drawn once, at the
moment a genesis community opens, and inherited down the lineage; of twelve
references to it in the history bake, not one is an assignment. No contact
moves it. No displacement moves it. No famine, no conquest, no collapse.

There is a second, quieter fact inside that one. The field's own doc comment
calls the offset *per-people*, and the draw sits inside the loop that opens one
people's genesis communities — one draw per community. So the offset is
per-community, two communities of one people can sit on different rungs at
genesis, and the project's own description of its own field is wrong about
which subject it belongs to. It cost this campaign a sentence of its own
analysis before anyone checked the loop rather than the comment. The
correction changes nothing about the conclusion above — `[0, 300]` against
year 2000 washes out under any aggregation — but it changes the *reason*: the
model does produce divergence between peoples at genesis, and the clock erases
it by the present day.

## A scalar the project had already rejected, by name

The horizon is not inert. It is multiplied into raiding strength through a
weight of 1.0 for neolithic, 1.5 for bronze, 2.25 for iron and 3.0 for
classical — so a classical people raids at three times the strength its
population alone would give it, because the calendar says so.

The project's own speculative doctrine rejects exactly that object, in the
essay that opens with the phrase *technology as capability thresholds*:

> The instinct here is a tech tree, and the tech tree is the ontology trap
> wearing an engineer's hat — a directed graph of nodes you "research," a
> progress scalar, a civilisation with a *level*.

And states what it wants instead:

> A technology in Hornvale is not a node a people unlocks; it is a
> **capability threshold the world crosses when a computed bar is cleared** —
> biome resources × subsistence mode × accumulated surplus.

What shipped is the rejected object with the graph removed: **a level without
a tree.** A tech tree at least encodes prerequisites, incomparability and
genuine branch points; a monotone scalar keyed to the year encodes only that
later is more. The doctrine's preferred shape needs a bar to clear and
something to clear it with, and the ladder consults neither. This gap — not
the monotonicity, which is a symptom — is the campaign's real subject.

The ladder is also load-bearing, which is why this campaign did not simply fix
it. The committed horizon fact is read by raiding strength, by the accrual of
delve depth, by which structures a ruin leaves behind, by the prose of two
windows, and by four census metrics. Changing what it means is an epoch, not a
patch.

## Why a catalogue first, and a better model second

The project's measurement discipline is that a hypothesis freezes before the
code that would move it, and a corpus drawn from what the world already models
cannot falsify anything. Both rules point the same way here: author the
catalogues *now*, while the model is still a clock, and the first score is a
genuine preregistered miss rather than a retrofit. Author them after the
replacement and the instrument inherits whatever the replacement happens to
do.

So the freeze is structural rather than promised, in the manner
[The Seedbed](./the-seedbed.md) established for its own family: both columns
were authored and committed in tasks that ran **before a line of evaluation
code existed in the repository.** There was nothing to tune them against.

## Two columns, because an invention catalogue shares the clock's bug

The obvious corpus is a chronology of invention, and it would have been the
wrong one alone. An invention catalogue is a **progress narrative**: ordered by
date of first attestation, every item entering once and never leaving. That is
precisely the assumption the shipped ladder encodes. **A corpus drawn from it
is structurally blind to loss** — it will never carry a row for Roman concrete
forgotten, Linear B lost, or deep-water voyaging abandoned — so scoring
Hornvale against it could not possibly find the defect that motivated the
campaign. The instrument would have shared its subject's bug.

Hence two columns, frozen together, because a corpus matrix is read *across*
and the disagreement between columns is the finding:

- **An invention column**, selected from a published chronology of science and
  discovery by the catalogue's *own* named story arcs — three of them, on
  knighthood, the republic of letters and the diffusion of steam, with sixteen,
  ten and fifteen inventions and a union of forty-one. The selection rule is
  keyed to the catalogue's structure rather than to anything about Hornvale,
  which is what keeps the selection from becoming the measurement. Two
  independent enumerations of the source agreed on every slug.
- **A collapse column**, of technologies documented as held and then lost,
  hung from Joseph Henrich's 2004 treatment of maladaptive cultural loss and
  widened past his cases by whole episodes, each carrying its own citation and
  contested cases marked contested.

**The two columns carry incompatible theories of what a technology is**, and
that is the point rather than a flaw. The chronology frames a technology as an
individual invention — who made it, where, in what year. Henrich frames it as a
*distribution of skill in a population*, which rises when the number of
effective social learners clears a threshold set by the ratio of two inference
parameters and ebbs below it, skill by skill, compatibly with individuals
reinventing the thing repeatedly and it never reaching the record. Neither
theory is a correction of the other.

The uncomfortable part is which one the project prefers. Its own registry
language for technology — a capability threshold crossed when a computed bar is
cleared — is Henrich's shape, not the chronology's. **The doctrine already
holds the population theory, and what the repository built is the other one,
minus the inventors.**

## The reading

Forty-one items each, rendered to
[`docs/audits/technology-coverage-asimov-1989.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/technology-coverage-asimov-1989.md)
and
[`docs/audits/technology-coverage-henrich-2004-extended.md`](https://github.com/hornvale/hornvale/blob/main/docs/audits/technology-coverage-henrich-2004-extended.md):

| verdict | invention column | collapse column |
|---|---|---|
| present | 0 | 0 |
| refused | 0 | 0 |
| deferred | 6 | 8 |
| absent | 35 | 31 |
| inapplicable | 0 | 0 |
| grown / flat / lost | 0 | 0 |
| unmeasured | 0 | 2 |

There is no `present` in either column, and that is the least interesting fact
on this page. The interesting one needs two numbers rather than one:

> **Of forty-one documented technology losses, Hornvale's mechanism reaches two
> of the technologies at all — and of those two, it can represent the loss of
> exactly zero.**

A single tally cannot say that. A column reported as uniformly unscored cannot
say it either, which is what an earlier draft of the design would have
produced: it instructed the collapse column to read *unmeasured* on every row,
on the grounds that nothing in Hornvale can lose anything. That is true and it
throws away the two. Under the ratified vocabulary a verdict is a pipeline — a
measured value is reachable only once reach has succeeded — so a technology the
world cannot model at all fails at the first stage and scores `absent`, and
only a technology it *does* model reaches the stage where loss would be
measured and stalls there unscored. Thirty-one of the collapse items are the
first kind. **Two are the second, and they are the only rows in this campaign
that say anything positive about the world at all.**

Those two are copper-alloy smelting and iron smelting, lost in Roman Britain
in the generations around 400 CE. They are reachable because two rungs of the
ladder are *named* — bronze and iron exist as values a committed fact can
carry. That is the whole of it. A rung is not a capability: nothing consults a
smelter, an ore body, or a population of people who know how. The instrument's
single most favourable finding is that Hornvale has the words.

The invention column reaches zero, and the honest reading of its
thirty-five `absent` rows is *nobody's yet* rather than *refused*: not one item
in either column is refused by a ratified decision, and not one is
`inapplicable`. The catalogue asks for pneumatics, for movable type, for a
standing army; fourteen items across the two columns are `deferred` against a
planned-but-unbuilt idea the project has already written down, and the project
has no position at all on the rest. A reader who wants
to know where Hornvale actually stands on technology should read that as *the
question has not been opened*, which is exactly what the absence of a ratified
decision on the ladder already said.

## The demand set is the output that carries information today

Against a clock, the verdict column is largely predictable before it is
computed. The part that is not predictable — and the part the successor
campaign is built against — is the **demand set**: what a world would actually
need in order to grow each technology.

No item states its demands. Each names the single capability token it
*introduces* and the items it *presupposes*; the demand set is the transitive
closure over that lattice, computed on read and never written into the file.
The reason is measured rather than aesthetic: a hand-written demand list in a
sibling family under-described itself three times in a twelve-item corpus, and
a derived set cannot under-describe, because no human restates it.

The two lattices then differ in a way neither column was authored to show:

| | invention column | collapse column |
|---|---|---|
| items | 41 | 41 |
| distinct capability tokens | 41 | 41 |
| roots (no prerequisites) | 10 | 23 |
| `presupposes` edges | 40 | 24 |
| deepest demand set | 14 | 6 |
| mean demand set | 4.27 | 1.61 |

**Invention is a lattice; loss is a list.** The chronology's deepest item — an
improved steam engine — needs fourteen distinct capabilities to exist at once,
through a chain running from the evacuation of air from a vessel to latent
heat to rotary power. The collapse column is half roots, because *losses do not
presuppose one another*: Tasmanian bone-tool manufacture and Inughuit skin-boat
construction are not rungs of a shared ladder, they are independent episodes of
the same mechanism.
The structural asymmetry between the two columns is itself a statement about
the theories behind them — cumulation has a shape, and ebbing does not.

## One new verdict, and why it had to name a scope

The family's vocabulary inherits five verdicts of reach and three of
measurement from its siblings, and adds exactly one: **`lost` — measured:
acquired, then given up.** The sibling families can express degrees of
*absence*, and one of them a measured *miss*; none can express a capability
that was held and released. That is the single thing this family exists to make
sayable, and the axis on which the shipped model is provably silent.

A verdict that names a loss must say *whose*. A capability can be given up by
one community, by a whole people, or by every people in the world, and those
are three different claims. An instrument that picks silently per row, with the
choice tracking whichever produced the nicer answer, is not hypothetical here:
[The Compendium](./the-compendium.md)'s family acquired exactly that defect and
it survived five reviews, because every anchor resolved and the anchor
discipline cannot see a verdict quietly narrowing its subject. So `lost` means
**a people that held the capability no longer holds it**, and the resolver
states that scope in the direction it enforces.

The same reasoning forced a second thing, and it is the one a bare verdict
would have hidden. The defect in the shipped ladder is not that Hornvale fails
to acquire technologies; it is that **every surviving community acquires all of
them.** Under a boolean verdict, "every people has bronze" and "half the
peoples have bronze" score identically — so the instrument would have been
blind to the precise pathology it was built to detect. A technology item
therefore carries a statistic over the *distribution across peoples* and a band
it must land in. Today's holding fraction is 1.0, outside any band that
expresses divergence, which means a correct measurement of today's world reads
as a miss. A boolean would have reported success.

## What the instrument is least entitled to

`present` is always the weakest verdict in this family: a path that exists is
not a working feature, and a resolvable test name is not proof a capability is
met. With zero `present` rows in either column, the weakest claim actually
made here is the *reach* half of those two `unmeasured` rows — and that half is
a positive assertion, exactly as checkable as a `present` would be. So it
carries a mechanism anchor too, which neither sibling family required.

That is not a formality about two cells. **The figure a reader will quote from
this campaign is the two**, and an unanchored two would have been the one
number in the whole instrument resting on nobody's word but its author's, while
reading as the most informative thing on the page.

The corpora are also authored by people who had read the model, which the
freeze cannot undo. The mitigations are a selection rule keyed to the source's
own structure, and a per-item disclosure wherever a verdict turned on that
knowledge — sixteen of the invention column's rows carry one and thirty-two of
the collapse column's do. Both are mitigations. Neither is a blind.

## What it does not claim

**The column is not a roadmap.** A corpus here is an instrument with a declared
bias, never a standard, and deciding which technologies Hornvale should
implement remains a human act performed *on* the reading rather than an output
of it. The sibling family that forgot this once published seven capabilities
the world already had under a heading reading *missing*.

**The catalogues are not a specification of technology.** One is a
mid-twentieth-century Western progress narrative ordered by
European attestation, under-recording non-material technique; for a world of
goblins, kobolds and drow that bias is load-bearing rather than incidental, and
it will demand things a subterranean people would never want. The other is one
paper's theory of loss, widened by this campaign past the cases that paper
documents, with fourteen of its forty-one rows held to a transcribed source and
twenty-seven to a weaker standard that the artifact names rather than averages
away.

**And the family is arguable.** This is closest to the macro-regularity family
and shares its verdict machinery wholesale; a reader who concludes it is that
family with extra fields is making a defensible case. It was the lead flagged
item when the design was ratified, and it is preserved in the decision record,
in the design and in the family's own guide, because a ratification that
deletes the case against itself leaves the next reader unable to tell a
decision from an assumption.

What this campaign emphatically does not do is fix anything. The ladder is
untouched; the weight is untouched; the committed fact means what it meant. A
successor campaign replaces the clock with something that can express
prerequisites, divergence between peoples, and loss — and when it does, two
cells in one column become measurable, the two-way guard shipped here acquires
its first live case, and a column that reads as nothing today becomes the thing
the replacement has to survive.

A fired pot cannot become clay again. That is a property of kilns. It should
not be a property of a world.
