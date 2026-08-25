# 0261. A rule duplicated on purpose carries a pointer in both copies and a two-way agreement test

**Status:** Accepted (2026-08-25) · **Decider:** Nathan · **Relates:**
[0011](0011-studies-are-data-metrics-are-code.md) (why the second copy exists
at all),
[0002](0002-domains-depend-only-on-kernel.md),
[0259](0259-conceptual-deficiency-is-derived-not-authored.md);
[The Confidant](../../book/src/chronicle/the-confidant.md)

In the context of the Laboratory deliberately re-deriving a worldgen
classification rule so that its metric is an *independent* reading rather than
an echo, we decided that **a rule duplicated on purpose must name its
counterpart in both copies and be held together by an agreement test that
reddens whichever side moves** — accepting the maintenance of two copies rather
than collapsing them into one.

## Context

The Laboratory's exposure metric re-derives which concepts a species is steeped
in, from the same authored inputs worldgen uses, without calling worldgen. That
independence is the point: a metric that asks worldgen what worldgen decided
measures nothing but its own call.

This campaign taught worldgen a new exposure rule and did not teach the
Laboratory's copy. Nothing objected at the time; the divergence surfaced later
as moved census columns, and the temptation at that moment is precise and
strong. **The cheapest-looking repair is the one that removes the check.**
Collapsing the two copies into one makes the drift impossible and the metric
worthless in the same edit — and it would read, in a diff, as a tidy
de-duplication.

## The rule

1. **Keep both copies.** A second derivation that exists to be independent may
   not be replaced by a call to the first.
2. **Each copy's doc comment names the other**, by path and function, and names
   the test that holds them together. A duplicate nobody can navigate between
   is how the divergence stayed invisible.
3. **An agreement test sweeps real inputs** — several seeds × every placed
   species × every registered concept — and fails naming the seed, the species
   and the concept of the first disagreement.
4. **It must be mutation-proved in both directions.** A test that only reddens
   when one side moves is an echo wearing a better name, and it privileges that
   side as the source of truth without saying so. Perturb each copy in turn and
   confirm the test objects to each.

## Consequences

- **Adding a classification rule to either copy is now a two-file change**, and
  forgetting the second file is a red test rather than a moved census column
  three days later.
- **What we give up:** the duplication is real and permanent. Two rules stated
  twice will drift in *wording* even when they agree in behaviour, and only
  behaviour is checked.
- **This does not license duplication generally.** It applies where
  independence is the *purpose* — a measurement re-deriving what it measures.
  Every other duplicate is still a defect.
- **The wider question this leaves open** is how many other rulebooks in this
  tree are duplicated with no pointer between the copies. Nobody has counted.
  It is recorded as a followup rather than answered here.

## See also

`windows/lab/CLAUDE.md`; the campaign retrospective's account of the three
redundancies that paid in one night.
