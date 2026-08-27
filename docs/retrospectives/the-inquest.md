# Campaign The Inquest — retrospective

**Merged:** 2026-08-26

## The headline: sixteen defects, every one in the controller's plan/spec/dispatch text, zero in implementer code

This is now the third campaign in a row with that shape (The Scarf: eleven; The
Reticence: five), and the count going *up* while the process holds is the
finding. The loop reviews the plan against itself and the code against the
plan; the only thing that reviews the plan against the tree is somebody running
a command. Every one of the sixteen died to a command, and none died to a
re-read.

Four of them are worth carrying past this campaign.

### 1. A near-vacuous check, billed in capital letters as the campaign's most important

Task 3's dispatch said the byte-golden additivity check was "THE MOST IMPORTANT
THING IN THIS CAMPAIGN". It was close to vacuous, and the implementer
established that *structurally* rather than taking the claim at face value: all
four production call sites of the deep realizer passed `paradigm: None`, and
`paradigm_depths` had no caller outside `domains/language` at all. A change
confined to `paradigm.rs` could not reach genesis, so an unmoved golden was
nearly guaranteed whether or not the additivity claim was true.

It then measured additivity **directly**: 40 seeds × 3 species, printing the
four pre-existing axes with and without the new polarity draw — 120 rows
identical — with a **positive control** that inserted one extra draw into the
number stream and moved 70 of the 120. The probe was removed afterward and
verified gone.

`an-empty-diff-needs-a-positive-control` was already a standing lesson in this
project when that dispatch was written. The check was not *wrong*; it was
answering a narrower question than the claim attached to it, which is this
session's dominant failure mode and was in the notes at the time.

### 2. A branch table that named one cause when there were two

The plan's rule read: "a moved byte-golden means a draw was inserted into an
existing stream's consumption order — STOP." Goldens moved at Task 6. The
implementer stopped, diagnosed, proved the named premise absent, proceeded, and
flagged it loudly — which is exactly the behaviour a branch table should
produce, and the reason to keep writing them as branches rather than as
predictions.

The rule named one cause and there are two. A `World` is seed + registry +
ledger, so registering **any** concept necessarily adds its row to the
serialized registry. No placement avoids it, and it is not a draw perturbation.

The implementer's proposed discriminator — a zero-deletion diff — was *nearly*
right, and checking it found the residual imprecision: the concept manifest is
+7/−6, because one long comma-separated line re-renders with `kill` in sort
position plus five counts each incrementing by one. A bare zero-deletion test
would have false-flagged it. **The correct discriminator is whether an existing
VALUE moved**, not whether lines were deleted. The final state of the campaign
is the model: one golden moved, by exactly six added lines, and not one existing
value changed.

### 3. A rebuild-from-source inside a layering pipeline is latent until a second layer exists

New failure shape for this repo, found at Task 2. The evidential arm rebuilt the
copula word from the grammar's drawn copula form, in place. With tense now
affixing first, that rebuild would have **silently discarded the tense join** —
no panic, no red, just a dropped marker. It was unreachable before only because
exactly one thing had ever marked the copula.

Fixed by building the copula once and having both layers join onto the same
segments in turn. Polarity became a third layer immediately afterward, so the
shape would have recurred within the same campaign.

### 4. `stream_labels()` has no completeness check, and it bit the same day it was noticed

The stream manifest is drift-checked for **staleness**, which is a different
property from **completeness**. A draw added without its roster entry yields a
silently incomplete published manifest and nothing goes red.

This was grepped before dispatching Task 3 and named in that dispatch — which is
the only reason it was caught, because Task 3's draws created **three** label
paths where the plan named two. The third is a family-level morph axis reached
through a dynamic label leg, invisible to any roster that is not hand-maintained
to match. Task 7 corrected the plan's rule again in the other direction: the
"three-part stream-label change" does not apply to a family-level axis at all,
because the axis leg is a dynamic string. It added a roster spot-check labelled
explicitly as "a spot check, not the missing completeness test" — the right
honesty about what a check proves.

The gap is now an idea-registry row.

## Did the byte-golden additivity check ever fire?

Yes, once, and correctly — at Task 6, for the benign cause above. It never fired
for the cause the plan named, and the campaign's own direct measurement is what
actually confirmed additivity. Recorded because the plan asked.

## Two things the process got right, and both are worth repeating

**A mutation that reddens the *right subset* is stronger evidence than a uniform
red.** Task 9's three mutations were designed to separate the assertions rather
than merely to prove each one fires:

- swap one demand token for another: the **count stays 2** and the pair changes,
  so the count test passes and only the by-id test reds — the wrong-pair failure
  demonstrated, which is exactly why assertion by id was required;
- add an unimplemented token: **only** the distance report reds, count and ids
  hold — the lagging-indicator argument shown rather than argued;
- drop `past-tense`: all three red.

A uniform red across all three would have been consistent with one test doing
all the work and two riding along. Reddening a predicted subset proves the tests
are independent.

**An implementer refused to let an agreement test stand in for a correctness
test.** Task 5's second mutation did not redden the shallow-identity test, and
that was correct: it changed both realizers equally. The implementer said so
explicitly rather than reporting a green as coverage, and wrote a third mutation
to show the identity test discriminates at all. Same epistemics as (1) above,
applied by the implementer rather than to it.

## A domain's technical vocabulary can collide with a controlled vocabulary

The lexicon guard counts `cell` in doc-comment prose, because in this project a
cell is a mesh vertex. In linguistics, an inflection-table cell is standard
vocabulary. Four tasks hit it. Every one reworded rather than waived, and Task 4
chose **row**, which the code already used and which is more accurate anyway —
a paradigm is a slice of rows, not a matrix.

Worth stating as a real recurring cost of the ratchet rather than as an
annoyance: the next campaign in this domain will hit it too, and the useful
advice is "reword to row", not "consider a waiver".

## A stale board notice nearly shaped the campaign

A peer branch had posted a hold-off for an epoch in this very domain. It was
read as live and relayed; it had merged long before. A peer's notice is judged
by time-to-live alone and is never verified against local state, so it reads as
live indefinitely.

**One command settles it**: ask whether the notice's branch is an ancestor of
`main` before relaying it. That check now belongs in the habit, not in the tool.

## Two smaller ones, recorded because their shape recurs

**A constraint inferred from a construction site is a hypothesis about a
producer.** A possessive pronoun was flagged as a case no drawn inventory
covers. It turned out never to be *produced* at all — it existed only on the
parse side, inherited from an old fragment whose producer had since been
removed. Asking what actually produces it dissolved the constraint entirely.

**A one-missing list with three of four ids wrong.** The texts were right and
the ids were read off the texts rather than re-derived against the running
resolver. The corrected list matched the spec's own projected blocker tally
exactly, which is the cross-check that the distance report measures what was
predicted.

## What was deliberately not fixed

A pre-existing ordering inconsistency between how particles and affixes layer
was found and **reported factually** rather than theorised or repaired: for
affixes the evidential ends outermost; for particles the last splice lands
nearest the predicate. It predates this campaign, the implementer followed the
existing convention exactly, and it is now an idea-registry row rather than an
in-scope change.

Likewise `know` was left without a valence row, at a cost of one line, because
the resulting two-state contrast — one corpus line with grammar *and* lexicon,
one with grammar and none — teaches exactly the distinction the corpus family
exists to keep visible. That is a deliberate omission recorded **with its
reason**, which is a tripwire; recording the conclusion alone would have been a
wall.
