# 0388. The ladder is a production instrument; parse-robustness is a separate axis

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0387](0387-an-absent-direction-is-unknown-never-inferred.md) ·
[The Stile](../../book/src/chronicle/the-stile.md)

In the context of cross-checking a capability ladder against a dialogue
corpus — how many of the corpus's demand tokens name a rung — we decided
that **the ladder is a production instrument**, that **parse-robustness is a
different axis and gets no rungs**, and that the honest ceiling for the
cross-check against `the-flood-watch` is therefore **147 of 149, not 149**,
accepting that the headline number can never read 100%.

## The two refused tokens, and why they are refused rather than missing

`unpunctuated-input` (68 instances) and `contraction-elision` (12) are
properties of the **input surface**. Both appear only on `parse` entries.
Neither names anything a generator can produce or fail to produce: every
ladder text is well-formed prose, so a rung for "the input had no
punctuation" would be a rung about the reader, filed among rungs about the
writer.

Minting them anyway is the tempting move and it is the one that corrupts the
instrument. A ladder mixing "the generator can build a cleft" with "the
parser tolerates a missing apostrophe" measures two capabilities under one
count, and the resulting number answers neither question.

## What ships

The ladder file carries a top-level `production_axis` block stating this,
including its consequence for anyone quoting the number. The cross-check is a
standing test rather than a one-time analysis, and it asserts the refused
pair **as a set with its reason doc-commented**, so the ceiling ships with
its explanation attached rather than as a bare figure someone later reads as
a shortfall.

The sensitivity of that test was demonstrated, not asserted: nulling one
rung's `introduces` in a temp copy makes the newly-absent token appear, and
the real ladder is byte-compared before and after to prove it was never
written. A separate dry run stubbed the set-difference helper to return the
empty set and reddened both coverage tests, which is what separates "my test
detects a missing token" from "my test detects anything at all".

## Consequences we accept

- **The number never reads 149 of 149, and anyone reporting it owes the
  reader the sentence explaining why.** A campaign that "closes the gap" by
  adding two rungs has moved the ceiling, not the coverage.
- **Parse-robustness is unmeasured, not measured-and-passing.** The 68
  player lines carry real demands about a natural-language interface's input
  distribution; nothing in this project scores them. That axis wants its own
  instrument, and this record is the reason it cannot borrow the ladder's.
- **52 ladder tokens are unexercised by the flood-watch corpus**, and that is
  not evidence they are wrong. Five scenes of one register cannot indict a
  rung about switch-reference. The count is worth recording at each
  cross-check; the tokens are not worth pruning.
