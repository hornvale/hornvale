# Refinement

Refinement is the act of generating new detail that is consistent with
everything already true — with the fields (the statistical prior) and with
every fact previously committed to the ledger. It is the project's central
recurring hard problem, and it will be for years: when the player returns to
a village they watched for a week, the coarse simulation that ran in their
absence must be *elaborated* into specifics that contradict nothing they saw.

## Tier 0, honestly described

The current engine is the simplest thing that upholds the invariant: given a
deterministic stream, a set of candidates, and the ledger, it starts at a
seeded position and returns the first candidate whose commitment would be
*accepted* — i.e., would contradict nothing. Uniform when unconstrained,
consistent when constrained, empty-handed when nothing survives.

That is genuinely all it does, and as of Campaign 1b it has exactly one real
client: settlement genesis chooses each village's name through it, so no
village can ever take a name that contradicts a committed fact. Its
signature, not its sophistication, is the commitment: everything that
chooses *through* this interface inherits consistency checking for free, and
the machinery behind the interface can grow — constraint propagation,
backtracking, aesthetic scoring — without callers changing.

## Why this is the boss fight

In the general case, refinement is constraint satisfaction against an
arbitrary accumulated history, with the extra requirement that the output
read as *intentional* rather than merely non-contradictory. Making a
generated myth fit three committed astronomical facts is search; making it
*good* is the research problem. The [Open Questions](../open-questions.md)
chapter keeps this honest.
