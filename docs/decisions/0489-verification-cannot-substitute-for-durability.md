# 0489. Verification cannot substitute for durability

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md)

In the context of The Quoin and The Lodestar both responding to an earlier
promotion loss by adding verification — a step that reads the ledger back and
confirms each item landed — and both then losing the ledger anyway, we
decided **a check that reads an artifact cannot detect that artifact ceasing
to exist, so verification is not a substitute for making the artifact durable
in the first place**, accepting that this closes off a whole class of
"add a check" remedies for this specific failure shape.

## Context

Verification answers "did the promoted material match the source?" It cannot
answer "does the source still exist to be checked?" Both Quoin and Lodestar's
verification steps were reasonable and correctly implemented; they simply ran
against nothing, because the scratch ledger was already gone by the time
either check executed. A verification step and the artifact it verifies share
a fate when both live in the same doomed location.

## The rule

When a failure mode is "the source disappears before anything can check it,"
the fix is to change where the source lives, not to add another reader of the
same location. This is the reasoning [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md)
acts on, stated as its own principle because it is the thing that actually
ruled out "just add a stronger check" as a sixth attempt at the same remedy.

## Consequences

- This is a negative decision — it does not build anything by itself. Its
  job is to be citable the next time a scratch-durability problem is proposed
  to be solved by verification alone, so that proposal gets checked against
  this record before being accepted.
- It does not argue against verification in general — Task 3's ratchet
  (see [0491](0491-a-stated-blindness-gets-a-visible-ratchet-not-a-silent-fix.md))
  is itself a verification mechanism, built on top of a ledger whose
  *existence* no longer depends on anyone remembering to check it.
