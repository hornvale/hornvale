# The Scarf

*A scarf joint cuts two members on a long taper and laps them so the join
carries load as a single piece. It is the joint you use when you have two
timbers and need one beam.*

This campaign's product is a deletion. Twelve lines in a test file:

```rust
fn tongue_view(spec: &ClauseSpec) -> TongueClause {
    TongueClause {
        subject: match &spec.subject {
            Subject::Name(name) => name.clone(),
            Subject::Pronoun(pronoun) => (*pronoun).to_string(),
        },
        complement_concept: match &spec.object {
            Argument::Concept(id) => id.clone(),
            other => panic!("the interlinear's clause predicates a concept, not {other:?}"),
        },
        evidential: Evidential::Witnessed,
        adjuncts: spec.adjuncts.clone(),
    }
}
```

That function is gone, and with it the second clause type it existed to reach.
Nothing else in the campaign matters as much as that, so it is worth being
precise about what those twelve lines were doing.

## Four lossy moves, and where each one leaked

[The Interlinear](./the-interlinear.md) made the clause fact-shaped and demoted
Common from privileged renderer to one realizer among the tongues. It stopped
one step short and said so in its own chronicle: *"Common is not yet a peer in
full. `TongueClause` still takes a different shape from `ClauseSpec`."* The
projection above is the shape of that stop.

1. **`Subject::Pronoun` was stringified into the tongue.** An English pronoun
   crossed into tongue output — the exact leak The Interlinear existed to
   close, surviving inside the helper that *demonstrates* The Interlinear.
2. **A non-concept object panicked.** A tongue could not predicate a name, a
   count or a quantity, and the refusal was a crash rather than a gap.
3. **`evidential` was invented out of band.** `ClauseSpec` had no such field,
   so the projection supplied a value the caller never stated. Every clause
   that reached a tongue was `Witnessed` because a test helper said so.
4. **`number` and `definiteness` were dropped silently.** A tongue was never
   told its subject was plural.

And the function lived in a **test file**, so `domains/language` had no seam at
which its own lossiness could be tested, or even seen. A projection between two
structs is a translation, and this one had no home in the crate that owns
translation.

## One clause

`TongueClause` is deleted. `ClauseSpec` absorbed its one unique field —
`evidential` — and, with nothing left to distinguish it from, dropped the
`Spec` suffix: the type is `Clause`. All three realizers take `&Clause`.

Each of the four losses closes for a different reason, and the reasons are more
interesting than the deletion.

**The pronoun becomes a gap, not a string.** `realize_tongue` returns
`Result<String, TongueGap>` and a pronoun subject now takes that path, with a
reason naming what is missing: this tongue has no pronoun inventory. That is a
true statement about the world — no campaign has drawn pronouns for a generated
tongue — rather than a silent substitution of English.

**The object slot and the adjunct slot now share one resolver.** The asymmetry
in move 2 was never a design; it was the two struct definitions drifting.
`realize_adjuncts` already resolved all four `Argument` variants — a concept
through the lexicon, a name verbatim, a count or quantity as digits — while the
object slot resolved only `Concept`. So a tongue could say a bare numeral **in
an adjunct** and could not say the same numeral **in the object slot**.
Extracting one `resolve_argument` and pointing both slots at it deletes the
panic rather than relocating it.

That widening raised a question with no precedent either way: what does a
`Count` object do under affixal morphology? `layer_affix` panics on a word
whose segments are unknown, and only a lexicalized root carries segments. The
answer taken is that a `Name`, `Count` or `Quantity` object **realizes bare and
bears no noun-class mark** — and that this is not the silent degradation the
panic exists to refuse. The two segment-less cases differ in kind: a compound's
missing segments are a lexicon bug, and the code says so in place ("close the
lexicon gap before Affix-marking a Compound"), so degrading there would hide a
fixable defect; a numeral has no segments **by nature**, so declining to affix
hides nothing. What makes this a discovery rather than a judgement call is that
`realize_adjuncts` had already been rendering names and numerals unmarked for
as long as it existed. The object slot was made to **agree with the adjunct
slot**, not given a rule of its own.

The evidential needed its own sentence, because it does not attach to the
object in the general case — it marks predicate-finally, on the overt copula
or, under a zero copula, on the predicate nominal. With a copula present a
non-lexical object changes nothing. With a zero copula *and* a non-lexical
object there is no nominal to bear the mark, and the clause simply goes
unmarked for evidentiality. That is the same shape as a tongue that drew no
morphology at all: a clause stating something the grammar has nowhere to put.

## The law that reads like a defect list

Which is the campaign's ratified claim, [decision
0286](../../../docs/decisions/0286-each-realizer-ignores-part-of-the-clause.md).
After the collapse, `Clause` carries features neither realizer reads in full:

| feature | Common | a tongue |
|---|---|---|
| predicate | construction table | asserted `IS_A` |
| subject | `Name` \| `Pronoun` | `Name`; `Pronoun` gaps |
| object | all four `Argument`s | all four `Argument`s |
| number | copula + plural | **ignored** (today) |
| definiteness | determiner slot | **ignored** (always) |
| evidential | **ignored** (today) | predicate-final mark |
| adjuncts | per-role surface | argument order only |

Read as two separate gaps — "Common can't do evidential", "the tongue can't do
number" — this invites a plausible wrong repair: teach a tongue to render
English number, and put the author's register straight back inside the tongue.
Read as a matched pair it is the correct shape. **A language-neutral clause
states more than any one realizer surfaces**, because it encodes what a speaker
might mean rather than the intersection of what the available grammars can say.
Each realizer surfaces what its own grammar has and drops the rest.

Both halves now carry a test, so a later campaign that changes either has to
delete an assertion deliberately rather than drift past it.

The companion clause is the one most likely to be "fixed" next, so it is
ratified alongside: **the input is symmetric and the output is not.** Both
realizers take the same `&Clause`; they do not return the same type.
`realize_common` is total, because Common is the author's register and its
vocabulary lookup cannot fail. The tongue realizers are partial. That asymmetry
is what makes a `TongueGap` mean *something true about the world* — this people
has no word for the sea — rather than an authoring hole. A `CommonGap`
introduced for symmetry's sake would destroy the reading.

## The flagship sentence does not improve

It is still:

```text
COMMON: Nwamvam is the home of the hobgoblins, in the clearing at vertex 18822,
        founded in year 25; it ended in year 1600.

TONGUE: Nwamvam Qoqe Bae 18822 25 1600.
```

A name, two lexicalized words, and three undifferentiated numerals. A hobgoblin
speaker still could not tell the founding year from the ending, because
`realize_adjuncts` still matches on the argument and never reads the role.

This was a deliberate choice between three candidate campaigns, and it is worth
naming the trade plainly: **this campaign bought the pipe, not a better
sentence.** Role-marking would have moved the visible line and left the two
clause structures in place; collapsing them moves nothing a reader can see and
makes role-marking, pronoun inventories, and a tongue-side construction table
each an ordinary next step rather than a step that must first navigate a lossy
projection. The demonstration test now hands **one** `Clause` to each realizer
instead of projecting between two structs, which is the whole difference and is
invisible in the output.

What did change in that test file: a tongue can now realize an
`Argument::Count` **object**, which was impossible before and had always been
possible in an adjunct; and a `Subject::Pronoun` produces a gap whose reason
names the missing pronoun inventory. The shallow-identity guarantee is
unchanged — the deep realizer with no marking drawn still equals the shallow
one, byte for byte.

## Two corrections to the record

The Interlinear's chronicle was found to carry a false claim and an unverifiable
one. Both are corrected in place there, and both are the same kind of failure:
prose that nothing asserts, standing beside code that moved.

**The false claim.** "The honest limits" said a tongue's role surfaces are
"adpositions and case morphology, which `paradigm.rs` already draws and nothing
realizes". There is no adposition or case machinery in `domains/language` at
all — one grep returns a single doc comment, and the typology module has no
case dimension. What `paradigm.rs` draws is **Number and Tense**, with zero
consumers outside its own module. The unrealized machinery is real; it is not
the machinery the sentence named. Left standing, that sentence tells the next
campaign role-marking is nearly free — pick up morphology already drawn and
surface it — when in fact adpositions and case must be **drawn**, which means
new permanent stream labels, which is a save-format contract. The framing hid
the cost.

**The unverifiable one.** That chronicle's flagship line reads `Nwamvam Qoqe Bae
8835 25 375`; the live value is `18822 25 1600`. The numerals are ledger data
read straight off the occupation's own facts, and they moved when 202 commits
of `main` were absorbed into that branch the day after the line was written.
Nothing objected because **no test asserts those numerals** — `git grep 8835 --
'*.rs'` finds only synthetic literals. The recorded line is kept rather than
rewritten, with a note: the words are what that campaign built and are still
right, and erasing the numbers would erase the finding. A demonstration no test
asserts is prose, and absorbing `main` is exactly when prose goes stale.

## What the campaign did not build

No role-marking. No tense, polarity or mood — wiring `paradigm.rs` is a future
campaign. No pronoun inventories; a tongue gaps. No `parse_tongue`: the
collapse makes it *namable* for the first time (one clause type means the
inverse of `realize_tongue` has a target), and does not build it. No tongue-side
construction table; the tongue still asserts its predicate is classification.

And no generation change at all. Nothing here touches a draw, a stream label or
a seed, and the committed artifacts held still. That empty diff is evidence
rather than an absence for the reason The Interlinear established: `the-book.md`
is rewritten on every rebaseline through the very realizer path this campaign
retyped, and it carries ninety tongue renderings. A second, weaker control sits
beside it — the type-audit report *did* move, 752 tags to 750, when the `pub`
boundary changed — but that one only shows the regeneration ran, since a
different generator wrote it.
