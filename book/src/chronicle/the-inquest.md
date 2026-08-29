# The Inquest

*An inquest is the proceeding that establishes, on the record, what happened:
who acted, upon whom, at what time, and what did not happen. That list is the
grammar this campaign builds, and the frozen corpus of dialogue it is measured
against is a witness statement about a killing.*

The sentence this campaign came to fix was on the record already:

```text
Nwamvam is the home of the hobgoblins, in the clearing at vertex 18822,
founded in year 25; it ended in year 1600.
```

It asserts a present state and then reports its own ending six hundred years
earlier. Nothing was wrong with the facts; the ledger knows perfectly well the
occupation is over. What was wrong is that Common's copula slot read the
clause's `number` and nothing else, so *is* and *are* were the only two things
it could say. A world can only make the distinctions its grammar has.

It now reads:

```text
Nwamvam was the home of the hobgoblins, in the clearing at vertex 18822,
founded in year 25; it ended in year 1600.
```

## Tense is not a property of a clause

The obvious repair is to hand the realizer the occupation's own ending fact and
let it decide. That repair is refused, and the refusal is the campaign's
ratified claim ([decision
0296](../../../docs/decisions/0296-tense-is-stated-never-derived.md)).

Every feature the clause carried before this campaign is a property **of** the
clause. Its number is a fact about the referent. Its definiteness is a fact
about how the referent is being introduced. Its evidential is a fact about the
speaker's access. Polarity, added alongside tense here, is a fact about whether
the clause asserts or denies. Each is settled entirely by the clause's own
content, which is why each can be read back off the surface by a parser.

Tense is not like that. Tense is a **relation to a moment outside the clause** —
the first clause feature in this project to require a deictic centre. Deriving
it means the clause must know when it is being spoken, which means the clause
must carry a clock. So the relation is computed by the caller, which is the one
party holding both instants, and arrives already reduced: `Present`, or `Past`.

The cost is stated rather than hidden. A caller may state a tense that
contradicts the world and nothing will object, because at this layer the clause
does not know what world it came from. What replaces a check is that the
computation lives where the facts are: the one live construction site selects an
occupation that ended and then asserts that its sentence says *was* and does not
say *is*.

## Why transitivity had to ship in the same campaign

The corollary is ratified with the rule, because it is what made the campaign's
scope coherent rather than merely large: **the tense host is the verb.**

Noun class marks the complement. The evidential marks the copula — or, under a
zero copula, the predicate nominal. Tense wants a verb, and *a zero-copula
nominal clause has none.* A good many drawn tongues have no copula at all, so
`X is a Y` in the past would have had nothing to bind to, and a tense-only
campaign would have been forced to invent a rule about affixing a tense marker
onto a noun and calling that tense.

`ConstituentOrder` was already `Sov/Svo/Vso/Vos/Ovs/Osv` — built for
subject-verb-object, with the copula standing in for the verb. A clause whose
predicate is an *act* supplies a real verb, and a natural host. The larger scope
made the core feature simpler rather than harder, which is the whole argument
for it.

## One table, read in both directions, twice

Common realizes forward and parses backward, and the campaign kept that property
at a level below the clause skeleton where it had not previously reached.
`COPULA_PARADIGM` pairs each of the eight surfaces over
{present, past} × {singular, plural} × {positive, negative} with the features it
realizes: the realizer looks a row up, and the parser searches a sentence for
any row's form and reads the features back off it. A copula form therefore
cannot be realizable but unrecognizable, or the reverse. `PRONOUN_PARADIGM` does
the same for person.

The verb's own table is the interesting one, because it is **not** injective.
The past neutralizes number in both polarities — *killed*, *did not kill* — so
eight rows yield six forms, unlike the copula's eight distinct *was*/*were*. The
parser carries a candidate *set* of numbers off the verb group and lets the
object's own plural break the tie, which keeps the round trip complete with no
documented loss.

Two roughnesses in Common are asserted by tests rather than quietly smoothed.
Common has no person agreement, so a first-person subject in the positive
present surfaces third-person: *"I eats the bread."* And the third-person
singular is *they*, not *it* or *she*, because nothing in the ledger assigns
gender or animacy to a clause and inventing a distinction to make one line read
better would be authoring rather than deriving. Both are pinned by assertions,
so a future person axis or gender axis arrives as a red test rather than a
silent correction. The same posture covers the verb table's naive regular past:
*eat* surfaces as ***eated***, asserted outright, so an irregular table also
arrives as a red test.

## What the closed construction table is actually for

The construction inventory has always been a closed list — one row per
predicate — and that has always read like a concession to the
realize-and-parse discipline: a wildcard matcher would be tidier, and the closed
table is the price of bidirectionality.

Writing the inverse for a transitive clause showed it is the other way round.
**The closed table is what makes the inverse computable at all.** A lexical
verb's surface is stem-dependent, so recognizing a verb group means generating
every paradigm row's form for every construction's stem and searching the
sentence for one of them — and the construction inventory *is* that enumeration
domain. A matcher that accepted any act predicate would leave the parser with
nothing to enumerate over: it would have to segment an arbitrary sentence, guess
which token is the verb, un-inflect it, and only then ask whether the result
names an act — which it cannot ask, because Common's vocabulary carries no kinds
and this layer may not reach sideways to the registry.

## Valence belongs to the predicate, not to either language

A tongue has no part list. Common has one, and before this campaign the shape of
a predicate was encoded incidentally in Common's spelling: a construction
carrying a copula slot was nominal; one carrying a verb slot would be
transitive. That is a fact about English sitting where a fact about the
predicate belongs.

So argument structure is stated once, as a property of the predicate, and
**Common's part list is selected from that statement rather than written beside
it** ([decision
0297](../../../docs/decisions/0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md)).
There is exactly one table, which means there is no agreement test to write and
none to rot. The tongue realizer references neither the part type nor the
construction type, so the peer relationship [decision
0286](../../../docs/decisions/0286-each-realizer-ignores-part-of-the-clause.md)
established holds by construction rather than by convention.

The distinction that needed guarding is that this is not the `Frame` enum The
Interlinear deleted. `Frame` was one variant per relation and the construction
lookup was keyed by it, so every predicate meant a new variant. Valence is
many-to-one — *eat*, *kill* and *know* are one transitive between them — and
nothing is keyed by it. The tripwire sits in the enum's own documentation: a
campaign that finds itself adding a variant per predicate has rebuilt `Frame`
and should stop.

Case, similarly, turned out to belong to the *slot* rather than to the clause.
The realizer already knows whether it is filling a subject or an object, so
Common owns the nominative/accusative distinction and the drawn tongue
inventories carry person crossed with number only. That is 0286's asymmetry one
level down: Common surfaces a distinction no tongue does, and that is the law.

## `kill` enters as a word, not as an action

The corpus's central event is a killing, and the world had no concept for it.
`kill` is now registered as a causative of the core concept `die`, at the
universal stratum — which means every people's lexicon holds a real root for it,
drawn through the same family proto-form and sound-change cascade every other
root uses.

It creates no obligation to implement combat, and that is a verified claim
rather than a hope: the audit that reconciles the planner's action roster
against the concept registry walks the *actions* and reports any with no
concept. It is structurally blind to a concept with no action. So the world
gains the ability to *say* `kill` long before anything can *do* it, which is the
intended direction of travel.

## The corpus reads 2 of 12, and "covered" does not mean sayable

The frozen corpus of merchant dialogue has scored zero since it was founded. It
now scores **two**, the first non-zero result in its history: *"A guard killed a
woman."* and *"I didn't know her."*

The second one cannot be spoken.

That is not a defect in the instrument, and it is the sharpest thing this
campaign has to say about its own measurement. A demand token in the corpus
names a **grammatical capability**, and this is a grammar score. All three of
the demands that line makes — negation, past tense, pronoun reference — are
genuinely built, and the shape *"I did not V them"* realizes in Common and in a
tongue. But `know` has no valence row, so the realizer refuses it outright; and
`know` sits in the action-suite vocabulary pack with no exposure rule, so every
lexicon returns a gap saying this people has no exposure to it. The sentence is
unsayable in **both** registers, not merely in the tongue.

The other covered line is the contrast that makes the point legible.
*"A guard killed a woman."* has grammar **and** lexicon: Common says
`Nwamvam killed a person.`, and every placed people of the reference world says
it in its own tongue with no gap — the bugbear, for instance, as
`Nwamvam Doobo Dabo.` Two lines, two states: one with both halves, one with the
grammar and none of the lexicon. Keeping those apart is the entire reason the
corpus family exists, and adding one line to the valence table would have
collapsed the contrast into a muddier third state for no measurement gain.

Beside the headline the resolver now reports **distance** — how many demands
each entry is short — because a conjunctive score is a lagging indicator. Four
entries sit at exactly one missing demand, each blocked by a different thing:
a question word, a temporal adjunct, a yes-or-no question, an epistemic hedge.
Those four moved this campaign without moving the headline number, and an
instrument that cannot show that is hiding its own progress.

## What reaches production, and what the book does not show

Pronouns reach production. So does each tongue's drawn paradigm: the assembler
that hands a tongue its tense and polarity depths is wired into the same place
that hands it its morphology, so the features are live rather than
test-only.

**And none of it changes a single line of the rendered book.** The book emits
only present-tense positive statements — a people's self-statement about what it
is, and a world-statement about the planet — and present and positive are the
**zero member** of their axes. No marker is drawn for a zero member, so none is
ever looked up. The committed gallery is byte-identical across this whole
campaign.

That is worth stating plainly rather than leaving a reader to notice, and it is
worth reading as a confirmation rather than a shortfall: it is direct evidence
that marking is gated on the features the clause actually states, not applied
opportunistically wherever a marker exists. A campaign that moved those pages
would have had a bug.

The headline change is therefore invisible in the published book. Making it
visible means a chapter that renders something that ended, which is real scope
and a separate campaign.

## What this does not reach

No mood — the fifth promised speaker feature is still undrawn. No aspect: tense
is present and past, with no progressive and no perfect. No questions, no
coordination, no temporal adjuncts, no existentials.

And above all, no clause inside another clause. A clause remains an island: no
field points at a second one. That single absence gates embedded clauses
directly and entangles epistemic hedging — *"I think her name was Gilda"* is one
clause inside another — and it is the next ceiling. It is a structural change
rather than a feature, which is what makes it the natural successor.
