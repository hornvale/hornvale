# The First Sentence

*The world learns to say what it is.*

Until this campaign, Hornvale could generate a world but could not *describe*
one in its own words. The almanac rendered worlds through hand-authored English
templates; the peoples spoke, but only of their gods. No machinery composed a
plain declarative sentence about the world itself. The First Sentence is the
opening campaign of a longer program — *The Self-Writing Book* — whose thesis is
that the world should document itself from its own Fact graph, with no
human- or model-authored prose in the loop.

The deliverable is one true sentence, generated three times:

> Vebe is a planet. · Waobwoe is a planet. · Zhqea is a planet.

Those are three worlds (seeds 1, 2, 3), and those are their names — each the
dominant people's own word for *the ground*. This is how human languages name
their world: *Earth* is Proto-Germanic for "the soil," *Terra* and *eretz* mean
"land," *Miðgarðr* is "the middle enclosure where people live." A pre-Copernican
people does not call its world "the third body from the star"; it calls it *the
ground*, and the astronomical fact that the ground is a planet is a later,
learned overlay. So the campaign derives each world's name from the dominant
people's lexicon entry for the universal concept *earth*, while the
classification — *is a planet* — is spoken in the god's-eye voice of a
gazetteer.

## What "the dominant people" means, measured not assumed

A world's namer is the peopled race with the greatest total presence, defined as
`Σ(population × mass)` over the races actually *placed* on the world. Two
disciplines are folded into that one definition. Mass, not headcount, decides
dominance — 525 kobolds do not outweigh a smaller people of much larger
individuals — because presence is biomass, not a census. And only *placed*
peoples are candidates: a species that exists in the registry but was never
settled on this world contributes nothing, because a species claim is a
measurement of the world, never a deduction from the roster. (The first
implementation forgot the second discipline and named a world after a race that
sorts first alphabetically but settles nowhere; the review caught it, and the
regression test reproduces the exact failure.)

## The engine: from a fact to a sentence

Classification enters the ledger as a fact. A new kernel-core predicate, `is-a`,
is functional — an entity holds one immutable class — and the planet is minted
as a first-class entity that commits `(is-a, planet)` and its `(name, ⟨endonym⟩)`
during genesis. Because the class is read from structure and the name is a
lexicon lookup, no new random draw enters the world: the facts are a projection
of what the seed already determined, so the change is a *derived view* over the
world, never an epoch of it.

The grammar generalizes the shipped content→render seam. A language-neutral
`ClauseSpec` — a frame, its participants, and features (number, definiteness) —
is realized into Common by a small function: copula agreement, the *a/an* rule,
the bare determiner of a generic plural. `windows/book` reads the ledger, builds
a `ClauseSpec` per classification fact, and calls the realizer; the CLI emits
*n* volumes as a drift-checked artifact. The whole pipeline is deterministic and
standard-library-only — the grammar is authored, the sentence is rolled.

## The Book as a proof-obligation on the world

The campaign inverts the usual relationship between a system and its
documentation. The Book is not a description maintained *alongside* the world; it
is a function *of* the world, and the set of facts it cannot yet render is the
world model's own to-do list. `hornvale book` reports that list: some ninety
predicates the world holds as facts — day length, moon count, stellar class,
subsistence, a deity's name — that no construction can yet turn into prose. Each
is a sentence the world will learn to speak in a later campaign. The classifier
`is-a` is the only one it can render today; everything else is the road ahead.

With this campaign The Book becomes the primary face of the project book, and
the chronicle and the frontier step back into appendices — the first structural
move toward a longer aim: that the same engine which describes the world will
one day describe the program's understanding of itself.
