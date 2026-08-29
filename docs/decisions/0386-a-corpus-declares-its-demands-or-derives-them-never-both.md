# 0386. A corpus declares its demands or derives them, never both

**Status:** Accepted (2026-08-28) · **Decider:** Nathan (autopilot) ·
**Relates:** [0011](0011-studies-are-data-metrics-are-code.md),
[0016](0016-studies-preregister-hypotheses.md),
[0261](0261-a-rule-duplicated-on-purpose-carries-a-two-way-agreement-test.md),
[0330](0330-the-corpus-score-is-demonstrated-not-declared.md) ·
[The Stile](../../book/src/chronicle/the-stile.md)

In the context of `sentences/` holding two corpus shapes at once — dialogue
corpora that state a `demands` list per utterance, and a capability ladder
where each rung names the *one* token it introduces plus the rungs it
presupposes — we decided that the resolver gains **one internal entry type
and two readers**, and that **no corpus file carries both shapes**, accepting
that a reader must know which file it is opening.

## Why the second shape exists at all

A hand-written demand list can under-describe its own sentence, and in a
twelve-entry corpus it already did three times: `m02`'s tokens omit adjectival
predication, `m10` is scored covered and is unspeakable, `m07` was credited
for a construction its first witness never built. **A derived set cannot
under-describe, because no human restates it.** The ladder's demand set for a
rung is the transitive closure of its presuppositions, collecting each rung's
`introduces`; it is computed on read and never written down.

## Why not write the derived list back into the file

Materializing the closure into the ladder's own JSON would state the same
fact twice — once as an edge list, once as a token list — and the two would
have to be kept in agreement. 0261 is the standing rule for a deliberately
duplicated rule, and it names the failure this avoids: **the cheapest repair
for a disagreeing pair is to delete the check**, which loses the guard
rather than fixing it. Not duplicating the fact removes the agreement test
before it has to be written.

## What ships

`Entry { id, speaker: Option<String>, text, demands, direction:
Option<Direction> }` as the one internal type, produced by `read_declared`
(the-merchant, the-flood-watch) and `read_derived` (the-ladder). The readers
enforce **different requirements at the JSON boundary**: the declared reader
requires `speaker` and `demands` and panics loudly on either being absent;
the derived reader has no such fields to require. `speaker` is optional on
the internal type because the ladder genuinely has no speaker, which is a
structural fact about one shared type — not a relaxation of what a dialogue
corpus must carry.

## Consequences we accept

- **A reader must know which file it holds.** The loaders
  (`load_merchant_corpus`, `load_flood_watch_corpus`, `load_ladder_corpus`)
  are the only sanctioned entry points, and each names its reader.
- **A future corpus must pick a side.** Adding a third shape — declared
  *and* derived in one file — reopens this record rather than extending it.
- **Transitivity is the load-bearing property and is guarded by mutation,
  not by assertion alone.** A one-level closure passes a shallow test on 11
  of the ladder's 214 rungs; the standing test is built on `r183`, whose
  shallow answer is 3 tokens and whose transitive answer is 22, and it was
  demonstrated red under a one-level mutation of `derived_demands` before it
  was trusted (decision 0353).
