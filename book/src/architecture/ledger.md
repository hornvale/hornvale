# The Fact Ledger

The ledger is the world's memory: an append-only record of everything that
has been *observed*, and therefore committed as permanently true. Once a
goblin king has a name, he has it forever; re-observation is consistent
forever. If fields are the world's statistics, the ledger is its history —
the **posterior** to the fields' prior.

## The dumb envelope

A fact is deliberately minimal: *subject, predicate, object*, plus optional
place, optional time, and provenance (which system asserted it). The envelope
carries no meaning of its own — all semantics live in the predicates, and
predicates are registered in the [concept registry](../reference/concept-registry.md)
by the domains that need them. This dumbness is a scar-tissue decision: rich
universal schemas (the "ontology trap") have killed prior projects and
famously consumed decades-long AI effort. The envelope stays dumb so the
vocabulary can grow campaign by campaign, reviewed rather than designed up
front.

## What the ledger enforces

Three things, all at commit time:

1. **Vocabulary** — a fact using an unregistered predicate is rejected. You
   cannot assert what the world has no concept for.
2. **Contradiction** — predicates declared *functional* (a thing has at most
   one canonical name) reject a second, different value for the same subject.
   This is the mechanical heart of "coarse constrains fine": commitments
   constrain all later generation.
3. **Sanity of numbers** — non-finite numbers are rejected outright, because
   a NaN admitted into the world's memory would poison both persistence and
   the very notion of "the same fact." (This guard exists because the final
   review of Campaign 1a asked what would happen otherwise, and the answer
   was: a crash at save time, far from the culprit.)

Identical re-commits are silent no-ops — observing the same thing twice is
not news. Entities are minted with identifiers that are never reused, and a
loaded save that violates that invariant (a corrupted or hand-edited file) is
rejected at load rather than trusted.
