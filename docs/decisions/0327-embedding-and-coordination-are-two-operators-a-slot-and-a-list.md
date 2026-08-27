# 0327. Embedding and coordination are two operators — a slot and a list

**Status:** Accepted (2026-08-27) · **Decider:** Nathan · **Relates:**
[0326](0326-a-clause-complement-rides-the-transitive-frame-no-sentential-valence.md),
[0286](0286-each-realizer-ignores-part-of-the-clause.md);
[The Mortise](../../book/src/chronicle/the-mortise.md)

In the context of adding both "one clause inside another" (*"I don't know
**why he killed her**"*) and "two clauses beside each other" (*"It confused me
**and** upset me"*) in one campaign, we decided these are **two separate
operators**, not one feature under the umbrella name "connecting clauses."

## Why the umbrella name is a trap

They share a structural fact — both are marked at a **boundary**, and that
marker is what keeps the parser's inverse computable — and nothing else.
Embedding is a **slot**: `Argument::Clause(Box<Clause>)` and
`Subject::Clause(Box<Clause>)` hold exactly one nested clause where an
argument used to go, and every one of `Clause`'s ~89 literal construction
sites is untouched by it. Coordination is a **list**: `Coordination { clauses:
Vec<Clause> }` arrives **above** `Clause` as an additive node, so
`realize_common(&Clause)` keeps its signature and every existing caller is
unchanged.

Conflating them would have meant either forcing a list into the slot
(`Argument::Clause` holding a `Vec`, which breaks the one-clause-per-argument
shape §4.1 relies on) or forcing a slot reading onto the list (treating
`Coordination` as a `Clause` field, which reintroduces exactly the 65-vs-89
site-count argument decision 0326's sibling avoided for the object slot).

## What ships instead

`Argument::Clause` / `Subject::Clause` for embedding; `Coordination` as an
independent public type with its own realize/parse entry points
(`realize_common_coordination`, `realize_tongue_coordination`,
`realize_tongue_deep_coordination`) for coordination. Both share the same
boundary-marking discipline: a complementizer marks *a clause hangs below
here*; a conjunction marks *a clause sits beside here*. The parser's
discriminator — coordination or embedding — falls directly out of which
marker is seen, at zero extra cost, which is the concrete payoff of keeping
the two apart rather than the abstract one.

## Consequences we accept

**Two save-format contracts, not one.** §4.6's subordination strategy and
§4.10's conjunction presence-and-form are separate drawn axes with separate
permanent stream labels (`SUBORDINATOR`, `CONJUNCTION`), each on the copula's
exact draw pattern. Neither could have been folded into the other without
making one operator's typology apply to the other's grammar, which is false
of real languages.

**A `Coordination` can never itself be embedded.** `Argument::Clause` /
`Subject::Clause` wrap a single `Clause`, never a `Coordination`, so the
boundary-marker check the parser runs is correct at every recursion depth,
not just the outermost call.
