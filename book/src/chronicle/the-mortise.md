# The Mortise

*A mortise is the cavity cut into one timber to receive another. The Scarf was
a joint that made two pieces into one length; this campaign cuts a cavity that
holds a second piece whole — and, because a real joint is never only one cut,
a second way of setting two pieces side by side.*

Before this campaign a `Clause` was an island. No field pointed at another
one, so *"I don't know why he killed her"* could not be built at all: not a
clause with a hedge feature, but one clause holding a second clause where an
argument should go. The merchant corpus's `m06` line needed exactly that
shape, and it had been unreachable since the corpus was founded.

It now realizes — honestly, in Common's limited register — as:

```text
I does not know they killed them.
```

That is not elegant English. It is a `know`-clause whose object is itself a
full `Clause`, realized through machinery built for this campaign and checked
by a witness that has to be able to fail. Getting to that sentence is most of
what follows.

## Two operators, not one

The obvious framing — "connecting clauses" — is a trap this campaign's spec
was written to name and refuse. Embedding and coordination sound like one
feature. They are two, and treating them as one would have meant forcing a
list into a slot or a slot reading onto a list, either of which breaks
something a whole campaign was just spent protecting.

**Embedding is a slot.** `Argument::Clause(Box<Clause>)` and
`Subject::Clause(Box<Clause>)` hold exactly one nested clause where an
argument used to go. Nothing about `Clause`'s own shape changes, so every one
of its 61 literal construction sites (re-derived directly: `git grep -c
'Clause {' -- '*.rs'` returns 65 raw hits at the campaign's base commit,
minus the struct's own `pub struct Clause {` definition and three
`-> Clause {` return-signature false positives) — the number this campaign
had to recount twice, because a naive `grep -c 'Clause {'` counts the
struct's own definition as a construction site, the same trap that produced
three wrong counts in the previous campaign — is untouched.

**Coordination is a list at a node.** `Coordination { clauses: Vec<Clause> }`
arrives *above* `Clause`, with its own realize entry points
(`realize_common_coordination`, `realize_tongue_coordination`,
`realize_tongue_deep_coordination`) — no parse entry point; recovering a
`Coordination` back out of its own realized text is not attempted at all,
as this chronicle states further down. `realize_common(&Clause)` keeps its
signature and every existing caller is unchanged — the coordination node did
not cost a single edit at any of `Clause`'s construction sites, which is
exactly what putting the list *above* the type rather than *inside* it buys.

Ratified as [decision 0327](../../../docs/decisions/0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md).
What the two operators share is a boundary marker — a complementizer says *a
clause hangs below here*; a conjunction says *a clause sits beside here* — and
that shared discipline is what keeps the parser's inverse computable for both
at once, below.

## No sentential valence — the overturn this campaign argued against itself

The obvious repair for a clause-shaped object is a new `Valence::Sentential`.
It is wrong, and the file explaining why is unusually explicit about its own
reasoning, because `know` needs **both** an NP object (*"I didn't know
her"*, `m10`, built by [The Inquest](the-inquest.md)) and a clause object
(*"I don't know why he killed her"*, `m06`). `PREDICATE_VALENCE` resolves by
`.find()` — first row wins — so one predicate cannot carry two valences
without re-keying the table on `(predicate, object-shape)`, which duplicates
exactly the single-source-of-truth structure The Inquest's own decision
([0297](../../../docs/decisions/0297-a-predicates-valence-is-stated-once-and-commons-parts-are-selected-from-it.md))
built to avoid a second table.

So a clause complement rides the existing `Valence::Transitive` frame. `know`
and `think` gain no new argument structure; they gain a category-flexible
object. The one honest special case is `Part::Determiner`, which becomes a
no-op when the object is a clause — the single place that already switches on
the object's shape. Skipping that produces *"I do not know **a** he killed
her."* Ratified as
[decision 0326](../../../docs/decisions/0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md).

Tense stays absolute through the same discipline The Inquest ratified
([0296](../../../docs/decisions/0296-tense-is-stated-never-derived.md)): the
inner clause's tense is caller-stated and never backshifts against the
matrix's, and its evidential, number, definiteness and polarity are its own,
never rewritten by the clause that holds it. A `Clause` still has no clock.

## A cap that states demonstrated depth, not a safety belt

`Argument`/`Subject` gaining a recursive variant opens a question this crate
never had to answer before: how deep can a clause nest inside a clause inside
a clause? The obvious answer is "bound it, for safety." That answer does not
survive examination here. `Clause` derives `Clone, Debug, PartialEq` and not
`Serialize`, so recursion carries no save-format risk at all, and `Box<Clause>`
is unique ownership with no `Rc` — a clause graph **cannot cycle**. Every
caller in this tree is repo code; a stack overflow would have to be built by
hand, thousands of boxes deep, on purpose.

So `CLAUSE_EMBED_MAX_DEPTH = 1` is not a guard rail. It is a statement: *one
level is what this campaign builds, tests, and can show working.* An uncapped
type ships reach that nothing constructs and nothing covers — the shape
`LANG-in-character-acts-are-unspeakable` already names for The Deed's seven
inert concepts, arriving here through a type rather than a registry row.
Nesting past the cap is refused in both directions — the realizer will not
render it, and the parser will not descend into it — by an assertion that
fails without the cap present, so the cap is load-bearing rather than
decorative. Ratified as
[decision 0328](../../../docs/decisions/0328-embedding-nests-one-level-a-cap-on-demonstrated-depth.md).

## Two typological axes, drawn the way the copula already is

A hardcoded complementizer or coordinator would make every tongue subordinate
and coordinate like English, which is precisely the failure `realize_tongue`
exists to prevent. So both boundary markers are drawn, on `tongue_grammar`'s
existing pattern for the copula: one stream produces presence and, when
present, the marker's own form.

**Subordination strategy** (`SUBORDINATOR`) governs an embedded clause's
boundary; **conjunction** (`CONJUNCTION`) governs a coordinated one. Both
admit a genuine zero-marker outcome — bare parataxis, bare juxtaposition — and
neither is degenerate: a tongue with no subordinator is not a tongue that
cannot subordinate, any more than a tongue with no copula cannot classify.
Both draw 50/50, a stated choice rather than an inherited one, because no
literature-backed skew was cited for either axis and neither strategy is
degenerate the way, say, a marked/unmarked tense pair is. Ratified as
[decision 0329](../../../docs/decisions/0329-subordination-strategy-and-conjunction-are-drawn-per-tongue.md).

These are the campaign's only two one-way doors: two new permanent stream
labels, additive, never renamed. Vocabulary itself costs neither — a word is a
`dynamic(concept)` value on the existing `PROTO_ROOT` axis, so `think`
entering every tongue's lexicon moved no stream label at all. `know` entered
none: it renders `gap (experiential)` in all 18 rows of the committed
dictionary, unchanged from base, a deliberate asymmetry
[`packs::THINK`](../../../domains/language/src/packs.rs)'s own doc states —
`know`'s exposure gate is a question a prior campaign left unresolved, and
moving it is out of this one's scope. The asymmetry is real: the exotic tail
of this project's vocabulary is structurally free, and a function word's mere
*presence* is what is expensive, because presence is typological rather than
lexical.

`think` entering the dictionary also moves the Burr's assignment-accuracy
pin (`windows/lab/tests/burr_calibration.rs`): every one of the 18 daughters
gains one word, so the readout's denominator grows from 1568 to 1586 and its
numerator from 1251 to 1266 — `think` classified correctly in 15 of the 18
tongues. The pin moves from 0.7978316326530612 to 0.798234552332913, a rise
of 0.00040, re-pinned in the same commit as this campaign's readout, per that
test's own instruction.

## The parser follows, and the discriminator falls out for free

`common_constructions`' own doc calls the pairing between realizing and
parsing *"bidirectional by construction."* Shipping a shape nothing could
parse back would break that promise silently, so `parse_common_with_tail`
gained a depth-budgeted recursive attempt at the point the walk already gave
up — the earliest unresolved verb-group split, which for a right-branching
complement is exactly the matrix verb.

The interesting part is what makes it safe rather than merely possible. A
sentence with two verb groups is embedding *or* coordination, never
ambiguous, because the marker says which. Common's coordination marker is the
fixed word *"and"*; its embedding marker is no word at all, since Common does
not spend a complementizer. So the parser checks for a top-level `" and "`
**before** ever attempting the recursive split, at every depth, not only the
outermost call — closing a hole where a coordinated sentence's second conjunct
could otherwise be recursively misread as a clause embedded under the first
conjunct's predicate. Constructed and verified: *"they kills them and Vebe is
a planet"* would, without that check, parse as a plausible but wrong embedded
clause. With it, the sentence is correctly refused as coordination-shaped.

Two limits are stated rather than left to be discovered. The parser already
could not recover `Argument::Pronoun` before this campaign — it returns
`Argument::Concept` unconditionally — so bidirectionality was already partial,
and this campaign's growth in what can be *realized* widens that same gap
rather than closing it (`LANG-parse-cannot-recover-a-pronoun-object`). And
recovering a `Coordination` back out of its own realized text is not
attempted at all: the success criterion asks the parser to round-trip
embedding and *distinguish* the two operators by their marker, not to recover
a coordinated structure, and doing the latter — including inverting tier 2's
subject-elision — is a real, separate piece of work, named on the
`Coordination` type itself rather than left findable only on the private
function that enforces it.

## The realization witness, and what it found

`sentence_corpus.rs`'s own module doc named its weak point: `IMPLEMENTED_DEMANDS`
is *"a hand-maintained declaration and nothing mechanically proves it."* This
campaign supplies the missing half —
[decision 0330](../../../docs/decisions/0330-the-corpus-score-is-demonstrated-not-declared.md).
For every entry the resolver calls covered, a committed `Clause` and the
Common surface it actually realizes sit beside the corpus's own English, and a
test asserts the realized surface matches exactly.

It went red the moment it was written: `m10` was already scored covered while
`realize_common` panicked outright, because `know` carried no
`PREDICATE_VALENCE` row before this campaign. And it went red a second,
sharper way once built: a review asked whether a covered entry's witness
truly exercises every demand token credited to it, or merely produces *some*
passing sentence. For `m07` — coordination *and* an embedded clause *and*
pronoun reference — the first draft's witness gave the coordinated clauses
bare pronoun subjects, which passed the assertion while never constructing an
`Argument::Clause`/`Subject::Clause` at all. Fixed by giving `m07` a
`Subject::Clause` on both coordinated clauses, which is what the spec's own
justification for keeping `m07` in scope had already promised in writing.

The score moved **2 of 12 → 5 of 12** (`m05`, `m06`, `m07`, `m09`, `m10`), and
the witness's other surfaces are honestly plain rather than polished:

```text
m06  I does not know they killed them.
m09  I thinked them.
```

`m06` drops the corpus's *"why"* — Common has no indirect-question machinery,
which was never in scope. `m09` drops *"her name"* — a lexical gap, the same
class The Inquest's `m10` already documented. Neither is new; both were
invisible until something actually realized a sentence that showed them.

## A garden path, and a finding sharper on its second reading

`m07`'s witness, once it genuinely exercises both operators, realizes as:

```text
they killed them killed me and knowed me.
```

That is garden-path garbage to a human reader, and the cause is structural,
not accidental. Common has no complementizer at all — Task 5 gave every
*tongue* a drawn subordinator, but Common was left to mark embedding by bare
juxtaposition, and nothing before this campaign had a sentence shaped enough
to expose the cost.

The first diagnosis was "Common has no complementizer, so subject embedding is
garbled." Review sharpened it, and the sharper version is worth keeping: the
asymmetry is **partly true of English too**, not only of Common. **Object**
embedding reads fine without a marker — *"I know they killed them"* is
colloquial English — because the matrix verb itself is the boundary cue; a
bridge verb like *know* or *think* licenses a zero-complementizer complement
in both languages. **Subject** embedding puts two verb groups back-to-back
with nothing between but a space, which is a genuine garden path *in English
too* — exactly why English subject-clauses require an overt *that* or a
nominalization, and Common has neither.

This is deliberately not fixed here. A complementizer would mean a new `Part`
in the shared `common_constructions()` table that **both**
`realize_common_with_subject` and `parse_common_with_tail` read from one
source, so it would move every embedded surface this campaign built,
including `m06`'s, and the parser work Task 8 just finished. It is recorded
instead, as the honest output of a witness doing exactly its job: exposing a
real gap that no test asserted and no reader would have predicted.

## Something must actually say one

`LANG-in-character-acts-are-unspeakable` records this project's own version of
a capability that ships and nobody wires: The Deed minted fourteen concepts
and seven were inert, verified in a committed manifest, because three
successive tasks each shipped without deciding who would use them. This
campaign's spec foreclosed that outcome by name: name a production caller
that emits an embedded clause into a committed artifact, or declare the
capability inert, in writing, with a reason.

The answer is **no** — every one of `windows/book`'s nine production
`Clause`-construction sites hardcodes an `is-a` classification with an
`Argument::Concept` object; `know`/`think` are not even imported there, and
`Coordination` has zero production callers anywhere in `windows/` or `cli/`.
Doctrine's "taught" register comes closest to a hedge but is deliberately an
`Evidential` **feature** on the same is-a clause, not a `THINK`/`KNOW`
**matrix** wrapping it — wrapping it would double-encode a fact already stated
once, which is not what an epistemic hedge means.

What makes this a decision rather than a shrug is that the declaration is
**checked, not merely noted**. A test scans this window's own production
source and reddens the day any of it constructs a clause-embedded
subject/object, a `Coordination`, or a `KNOW`/`THINK` matrix — a
one-directional acknowledgement that can only ever be satisfied would rot; this
one fails the moment someone adds what it declares absent, on the same
STALE-DECL discipline `seam-guard`'s `expect(survives: …)` uses. Verified
load-bearing by inserting a production `KNOW`-matrix construction and watching
it fail with the stale-declaration message, then restoring the file
byte-identical.

The guard's own review found a real gap this campaign closes: the original
scan matched the compound string `"predicate: KNOW"`, which an ordinary
fully-qualified reference (`hornvale_language::packs::KNOW`, exactly how that
constant must be written today since it is not imported into the window) could
dodge without dodging the construction itself. The scan now tokenizes on
identifier boundaries and matches `KNOW`/`THINK` as whole words on any
non-comment line, which catches the qualified form too — verified by the same
insert-and-restore mutation, reproduced against the corrected check.

## What this does not reach

**Right-node raising** stays cut. *"Seeing it confused and upset me"* shares
both the subject *and* the object across two verbs; this campaign ships
*"Seeing it confused me and upset me"* — tiers 1 and 2 of a three-tier ladder,
never tier 3. Ellipsis is where quality dies rather than merely where effort
is spent: what a language may drop is language-specific, and getting it wrong
produces *plausible* garbage, which is the failure mode that survives review
rather than the one that gets caught by it
(`LANG-clause-nominalization-and-ellipsis`).

**Nominalization** stays cut with it. *"Seeing it"* is a gerund subject, and
it was never load-bearing for `m07`'s three demand tokens — only for its exact
wording. Substituting a complementizer subject clause, *"That he killed her
confused me and upset me,"* satisfies the identical demands through machinery
this campaign already built. Nominalization is a real, separate feature, not
a shortfall in this one.

**A `Coordination` cannot itself be embedded.** `Argument::Clause` and
`Subject::Clause` wrap only a `Box<Clause>`, never a `Box<Coordination>`, so
*"I know he killed her and she died"* — a coordination nested under an
embedding — is inexpressible today
([decision 0327](../../../docs/decisions/0327-embedding-and-coordination-are-two-operators-a-slot-and-a-list.md)).
This is not an oversight to fix later so much as the other half of why the
parser's discriminator is sound at every recursion depth: because a
`Coordination` can never hide inside a slot, seeing the top-level `" and "`
marker is always conclusive, at depth zero and at the one level embedding
reaches beneath it.

**Temporal adverbials** stay cut, and this is a finding rather than a scoping
preference. `m02` — *"Everything was fine until last night"* — needs only
`past-tense` and `temporal-adverbial` by its own hand-authored demand tokens.
But *"Everything was fine"* is **adjectival predication**, and
`Valence::Nominal` renders `Subject Copula Determiner Complement` with
`Definiteness` offering only `Indef`/`Def` — no bare singular. Shipping the
temporal token would have moved the headline to 6 of 12 while producing
*"Everything was **a** fine."* The cheap-looking reversal — a
`Definiteness::Bare` variant — is a trap worth naming rather than taking: it
would produce the right string only by modelling an adjective as a concept and
asserting *"Everything is-a fine,"* the classification relation, for property
predication. That authors a false distinction to move a number.
`m02` is the second merchant entry (after `m10`) whose demand tokens
under-describe what it needs; `m10`'s gap is lexical, which the corpus was
founded to keep separate, but `m02`'s is **grammatical**, which is exactly
what the score claims to measure (`LANG-adjectival-predication`).

**The merchant corpus itself is nearly spent as a scoping instrument.**
`past-tense` covers 7 of 12 entries and is already built. What remains is
thin and scattered — three entries need a temporal adverbial, two a
*wh*-question, two a polar question, two a witness-set, one an existential,
one a named-entity list — and no single remaining token unlocks more than
three entries. The next campaign that wants to move this corpus's score
should expect diminishing, structural returns, not a matter of picking the
next feature better. The successor is a **second corpus**
(`LANG-merchant-corpus-is-nearly-spent`), recorded now rather than discovered
by a campaign that scopes against a flat instrument.
