# The Rail

*[The Stile](the-stile.md) seated the rungs. A rail is what you actually
climb — and what tells you which rung is next.*

The capability ladder that arrived with The Stile is a dependency graph over
grammatical capability: 214 rungs, each an ordinary sentence introducing
exactly one demand token and naming the rungs it presupposes, with two roots
and fifteen control rungs that introduce nothing. Resolved against the
grammar's implemented tokens by transitive closure, it read **1 of 214
covered**. Its **frontier** — the rungs every one of whose dependencies is
covered but which are not themselves covered — held five:

```
  r002  intransitive-frame     "The guard sleeps."
  r003  property-predication   "The road is long."
  r005  locative-predication   "The merchant is at the gate."
  r011  person-deixis          "I am a merchant. You are a guard."
  r083  polar-question         "Are you a merchant?"
```

This campaign built all five. The ladder now reads **11 of 214**, and its
frontier reads **20**.

The interesting number is neither of those.

## Two instruments, and the one that says nothing

The five rungs were priced against the dialogue corpora *before any code was
written*, which is how the campaign learned that its two instruments were
about to disagree:

```
                          entries covered      demand instances met
  the-merchant   before      5 / 12              19 / 30    (63.3%)
                 after       6 / 12              21 / 30    (70.0%)

  the-flood-watch before     0 / 139            175 / 1128  (15.5%)
                 after       0 / 139            270 / 1128  (23.9%)
```

Five capabilities moved the flood-watch corpus's headline **not at all** while
meeting 95 more of its demand instances — a 54% relative gain the binary
statistic cannot express. That is not a wash and it is not an argument for
rescoping. `person-deixis` is that corpus's third-highest-fanout blocker at 44
entries; `property-predication` blocks 21, `locative-predication` 16.

The arithmetic is simple and worth stating plainly, because the conclusion
depends only on it. Entry coverage is **conjunctive**: an entry counts as
covered when *every* token it demands is implemented. The flood-watch corpus
runs about **eight demand tokens per entry** across 1128 instances. A single
unimplemented token anywhere in an entry's list holds the whole entry out, and
at that depth every entry has one. So the headline sits at zero and will sit
there through several more campaigns, no matter how much capability lands
underneath it.

**Both instruments are right, and the disagreement is the finding.** The
ladder's frontier is the typologically correct build order: it says which
capability is reachable next, and it moves every time one lands. Entry
coverage is simply the wrong statistic for a corpus whose entries are that
deep — not a broken one, a wrong-question one. The remedy is a
**complementary** statistic and never a replacement: the merchant corpus's
entry coverage, read by its existing unmodified method, held at 5 of 12
across three prior campaigns and moved to 6 of 12 under this one's own
`polar-question` landing — comparable precisely because the method never
changed. Loosening what *covered* means to make a headline move sooner would
have destroyed that comparability in the same edit. So the report now carries
both numbers per corpus, and a campaign quoting either alone reports something
false — "0 of 139" alone says a grammar standing still, "23.9%" alone says
progress toward sentences none of which can be said.

The ladder's frontier is published with the same regeneration as everything
else, as a table of rung ids, tokens, texts and — per rung — how many *other*
rungs building it alone would unblock, computed by re-running the resolver
with that token added rather than estimated. That is the campaign's name: the
ladder stops being an instrument read once at a campaign's opening and becomes
one that states the build-next list on every regeneration.

## Four strategies, one taxonomy, closed

`Valence` — the field that decides which surface parts a predicate's clause is
built from — held two values, `Nominal` and `Transitive`. It now holds five.

Three new variants in one campaign is exactly the motion the project warned
itself against when it refused a sentential valence: *if a future campaign
finds itself adding a variant per predicate, it has rebuilt the frame enum and
should stop.* The reason this is a different thing is that the list is closed
by somebody else's typology. `Nominal`, `Property`, `Locative` and
`Intransitive` are Stassen (1997)'s four intransitive predication strategies —
nominal, adjectival, locational and verbal — and `Transitive` is the
two-argument case. A fifth **predicate** is one row in a lookup table. A fifth
**strategy** would need the typology to be wrong.

Two of the three are copular and differ from each other only in what they do
with the object slot:

```
  PROPERTY   [Subject, " ", Copula, " ", PredicateWord,                       Tail, "."]
  LOCATIVE   [Subject, " ", Copula, " ", PredicateWord, " ", Det, Complement, Tail, "."]
  INTRANS    [Subject, " ", Verb,                                             Tail, "."]
```

`PredicateWord` is the clause's own predicate rendered *uninflected* — a
property word and an adposition take no tense, number or polarity — and one
part serves both strategies, because a property relates a subject to a state
and a locative relates it to a located thing. That is not what the design said
before it was built. The design gave the property valence a complement slot
and no predicate slot, which forced the property word to be carried in the
object slot **as well as** the predicate: the right string produced by stating
one word twice. It was caught while checking the design against the code, and
it is worth naming because it is the same trap the property valence exists to
fix, restated one layer up.

The trap it fixes is this. Common could already produce *"the road is a
long"*: the classification construction renders `Subject Copula Determiner
Complement`, and forcing an adjective through it says *road is-a long* — the
right meaning stated through the wrong relation. The tempting repair is a bare
definiteness value, which produces the right **string** the same wrong way.
Property predication produces the right string by asserting what the sentence
actually means, so definiteness gains no third value.

## The object that is not there

An intransitive clause relates a subject to nothing, and a clause's object
field is mandatory. The honest type is an optional object; it was measured
rather than waved away, at **105 full-literal construction sites across five
files** against roughly five structural match arms for a new `Absent` variant.

The variant wins, and the fact-shape claim survives it — *an utterance is a
fact* — because the kernel already spells an objectless assertion. Facts carry
a mandatory object too, and several predicates commit theirs as a true flag.
The utterance is still a fact; `Absent` is the object that fact carries. A
`Flag(bool)` argument was rejected on substance rather than passed over: false
with positive polarity and true with negative polarity would be two spellings
of one denial, and the round trip could not choose between them.

One consequence is worth keeping. The compiler found three exhaustive matches
needing a new arm and named all three. A **fourth** site absorbed the new
variant silently, through a catch-all older than the variant, and produced a
wrong surface rather than a compile error — an empty complement ordered into a
tongue's constituent sequence. *"The compiler found nothing else"* is evidence
about exhaustive matches and about nothing else; a catch-all that predates a
variant converts a gap into a wrong answer, and the two look identical from a
build log.

## Person lives on the subject

Common's copula table was keyed by tense, number and polarity, and it was
**injective**: eight rows spelled eight distinct words, so a form named exactly
one row and reading features backwards off a surface was a function. That is
what makes one committed grammar readable in both directions.

English does not spell person that finely. `are` covers 2sg, 1pl, 2pl and 3pl;
`were` covers the same four. Adding a person axis takes the table to 24 rows
spelling **ten** forms, and the backward read stops being injective for the
first time in Common's history.

The expected consequence was that the parse would have to nominate a canonical
row and the loss would be pinned by a test naming which distinctions are
unrecoverable. **There is no loss.** A clause has no person field at all —
person lives inside the *subject*, and a subject that is a name or a nested
clause is third person by definition — so the parse reads person off the
subject and uses it to narrow the verb group's candidates. The round-trip
property test did not go red; its enumeration was widened from one person to
three and passed at full width.

The table is genuinely non-injective; the parse simply never has to ask which
person `are` is. That matters beyond this table, because the obvious repair —
collapse the candidates to one per distinct form and pick a canonical person
for each — is a rule made by **table ordering**, invisible at the point it
decides anything and rotten the moment a row is inserted above another. The
design avoids needing the rule rather than documenting it.

What the syncretism does cost is real and is stated where it is paid: the
parse search runs 24 candidate rows per copular construction where it ran 8 —
not 10, which is the count of distinct *words* and understates the work
threefold — and an invariant that had been silently relied on, that a winning
row's tense and polarity are unambiguous, became an asserted fact at exactly
the moment the old reasoning stopped carrying it.

One roughness the same table has pinned for two campaigns is fixed by this and
one is deliberately not. *"I eats the bread"* is gone. *"They is a planet"*
stays, because agreement is keyed on features and third-person singular's
bundle takes `is`: that surface's awkwardness comes from the pronoun
**inventory** — Common spells third-person singular *they* because nothing in
the ledger assigns gender or animacy — not from agreement. Fixing it means
letting a pronoun's chosen form override its features, which is a different
mechanism.

## A question is not a fact

A clause is fact-shaped, and the shape is the claim. A polar question is
precisely the utterance that is not one: it asserts nothing. So interrogative
force is an **operator over** a clause — the third such operator, after the
embedding slot and the coordination list — and never a field on it. A `force`
field would make the fact-shape claim false for every clause in order to serve
one.

Common asks by **inversion**, and refuses loudly where inversion is not what
English does. English inverts an auxiliary, and the copula is the only
auxiliary Common has, so a clause whose verb slot holds a lexical verb does
not invert: *"Sleeps the guard?"* and *"Knew you the woman?"* are not Common,
and emitting either is the plausible garbage this project's realizers refuse
on principle. What English uses there is periphrastic *do*-support, and half
of it already exists in the verb paradigm — the *negative* half, `"did not "`
and its siblings — so reaching the positive by stripping a substring is text
surgery. The honest route is a mood axis on a key that is read in both
directions, which is a parse-side change as much as a realize-side one, and it
is a later campaign's.

This is where a frozen corpus earns its keep. The merchant entry that
`polar-question` unblocks is `m08`, *"Did you know the woman?"* — a past
question on a lexical verb, which the grammar cannot say. Its two declared
demand tokens, `polar-question` and `past-tense`, are both built. **The entry
needs a third capability neither token names**, and it is the third of twelve
merchant entries to under-describe itself in exactly that way: no amount of
care at labelling time would have caught it, and resolving the labels against
a real grammar did.

## Marking a question a reader cannot hear

A tongue draws its own interrogative strategy, and the majority strategy
cross-linguistically is *no overt marker at all* — a question marked by
intonation. A text renderer cannot show intonation.

The temptation is to give every tongue a particle so that every tongue has a
marker. That authors a grammatical fact the draw did not produce, and hides it
in a place nobody inspects. What ships instead is a **transcription
convention**: a tongue that drew no particle renders its declarative surface
plus `?`, and the code says in as many words that this is transcription and
not morphology. An orthography is a stated view, and punctuation is how
writing encodes prosody; the general rule is that transcription can express
any contrast the model **draws** and cannot manufacture one it does not.
Contrastive focus and prosodic timing are unrenderable for a different and
more fixable reason — they are unmodelled.

The tongue's question operator is a different operator from Common's rather
than a port of it. Neither of its strategies moves a constituent — a particle
prefixes the clause, and the convention rewrites terminal punctuation — so a
tongue asks a lexical-verb clause exactly as readily as a copular one, which
is the one place a tongue is *less* constrained than Common.

## A count is not a membership

Twice this campaign a number stood still while the set behind it changed, and
both times the only thing that caught it was an assertion over the ordered set
rather than over its size.

Building the locative rung left the frontier at 16 either way — but `r005`
left it and `r051` (`present-progressive`) arrived, because progressives are
traced to locative sources, so building a locative genuinely pulls a
progressive into reach. And the merchant corpus's set of entries sitting one
token short of covered stayed at three entries while `m08` left it for zero
and `m11` arrived from two.

Neither is a defect. Both are facts about the graph that a count would have
hidden, and the general form is worth carrying: in an instrument of this
shape, **a count is not a membership**, and a compensating pair of changes
passes any assertion that only checks the size.

## What this does not reach

**Two of the five rungs ship in Common only.** A tongue refuses a property
clause and a locative clause rather than emitting a partial surface, and the
refusal is the right answer under this crate's standing law that a realizer
renders fully or gaps entirely. Both deferrals have a real reason rather than
a shrug: the tongue half of property predication is Stassen's
adjectival-encoding typology — property words encoded as verbs, as nouns, or
as a distinct adjective class — a drawn axis with its own weights; the tongue
half of the locative is adposition order, which is one of the strongest known
correlates of verb-object order and therefore **not an independent draw** at
all. It needs its correlation designed, not a coin flipped.

**This is the first adposition surface in Common, and it does not give a
tongue role marking.** A tongue still marks no adjunct roles, and that gap is
untouched: rendering *under* in a locative predication is a different job from
saying which role a trailing phrase plays.

**Nothing parses a question.** The interrogative is production-side, and the
one merchant entry it moves is a player line — something the grammar's job is
to *read* — scored on a production capability, because the corpus states no
direction and inferring one from the speaker field is forbidden. That is the
scoring method behaving exactly as specified and a real limit on what "6 of
12" means; it is printed in the report rather than left in a design document.

**The flood-watch corpus still has no coverage resolver of its own.** It has
an entry score that reads zero for a structural reason, a demand-instance
count that moves, and a vocabulary cross-check. What *covered* ought to mean
for a corpus the grammar was never built toward is a question to settle before
a number exists to chase.

**203 of 214 rungs remain uncovered**, and the ladder is not ordered by how
much any corpus wants a rung. It is ordered by what depends on what. The
frontier is the honest answer to *what next*; the demand counts are the honest
answer to *what for*; and this campaign is the first evidence that the two
give different answers and that both should be printed.
