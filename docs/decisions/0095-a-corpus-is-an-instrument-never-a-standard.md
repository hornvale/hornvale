# 0095. A trope corpus is a provenance-stamped *instrument*, never a *standard*

**Status:** Accepted (2026-08-01) · **Decider:** Nathan · **Relates to:**
[0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-measurement-is-preregistered.md);
[The Repertoire](../../book/src/chronicle/the-repertoire.md)

In the context of scoring Hornvale against an external catalogue of dramatic
situations, facing the risk that a coverage number reads as a verdict on the
world rather than a reading taken with one biased ruler, we decided that **a
trope corpus is an instrument carrying a declared provenance and a bias, never
a standard the world is measured against** — and that its output is therefore a
matrix over corpora rather than a score, accepting that a single corpus can
only ever produce one column of that matrix.

## The principle

Three commitments, each mechanical rather than aspirational:

- **Provenance is emitted, not documented.** The corpus file carries `corpus`,
  `provenance` and `frozen` fields, and the generated report prints them
  *before* any number. A reader cannot reach the score without passing the
  statement that the catalogue is one instrument with known bias.
- **A third verdict exists.** Beside *stageable* and *blocked*, a situation may
  resolve `inapplicable(reason)` — the world deliberately lacks a
  precondition. Without it, every difference between Hornvale and a
  nineteenth-century French dramaturgical taxonomy reads as a deficiency.
  Georges Polti is not owed a world.
- **The output is a matrix.** Corpora disagree, and the disagreement is the
  finding. Polti asks what situations exist; Propp asks what sequence they
  come in; a fan taxonomy asks what an audience notices. A single corpus is a
  column, and any claim that rests on comparing columns waits for a second
  one.

## Context

The Repertoire scored Polti's 36 situations against the concept registry and
returned 0 stageable. That number is only honest if the document carrying it
cannot be mistaken for a judgement, and the campaign's review history is the
argument for why that is not automatic: its one Critical finding was a table
that listed seven capabilities the world already had under a heading reading
*missing*, and a later round found a genuinely missing capability silently
absent from the same ranking. Both were failures of the same kind — the
artifact misrepresenting the backlog — and neither was a failure of the
measurement.

The `inapplicable` verdict earned its place least of the three, which is itself
evidence for this decision rather than against it. Exactly one of thirty-six
situations used it, and that one is arguably contestable: #31, *Conflict with a
God*, is excluded on the ground that the world holds no divine will, while the
registry independently reports that it holds no `divine-will` predicate. The
verdict distinguishes *will not* from *has not yet*, and the report cannot
currently show which is in play. A verdict that is hard to earn is not a
verdict that should be removed; it is the one doing the most work when it fires.

## Why not the alternatives

**Score against a Hornvale-native corpus.** Rejected: a catalogue drawn from
what the world already models cannot falsify anything, and it forfeits
preregistration entirely. The external corpus's bias is the price of it being
able to say no.

**Treat coverage as a percentage to raise.** Rejected under Goodhart. The
ranked misses are the deliverable; the percentage is a summary that invites
registering vocabulary nothing uses. The report's Supply section exists as the
counterweight and is explicit that its own second half — tokens no *readout*
consumes — is not yet built.

**Drop `inapplicable` and let a deliberate absence read as missing.** Rejected:
that is precisely the claim that an external catalogue defines completeness.

## Scope

Binds any corpus scored against the world in this manner, and the shape of the
report that presents one. It does not bind which corpora exist, nor oblige a
second — it settles what a second would be *for*.

## Consequences

- Adding a corpus means adding a column, and the first genuine matrix claim
  waits for corpus number two. Until then the report says *reach against this
  catalogue*, never *coverage*.
- `inapplicable` requires a written reason, and the reason is published.
- A campaign may not retune a corpus to move a score after unblinding (0016),
  which this decision extends from studies to corpora.
