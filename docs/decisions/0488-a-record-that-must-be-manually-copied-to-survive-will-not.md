# 0488. A record that must be manually copied to survive will not

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md) ·
[0020](0020-campaigns-write-retrospectives.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md)

In the context of five separate campaigns each independently choosing
"promote scratch material into the retrospective at close" as the way a
decision ledger survives its worktree, we decided **that this is not a
per-campaign execution failure to keep re-attempting, but a structural
property of any record whose durability depends on a manual step performed
under time pressure at the moment the campaign is trying to finish** —
accepting that this generalizes past this campaign's own ledger to any future
scratch artifact someone proposes to "just remember to copy over."

## Context

The Ell promoted nine items and lost six. The Quoin wrote the promotion
instruction down next to the material and still lost it. The Gallery/
Lodestar wrote the retrospective, skipped the verification step it had
itself added, and belief disagreed with `grep` on eleven counts. The Overture
and The Attestation both lost material to worktree recycling that happened
before the close walk that was supposed to do the promoting. The Quoin's own
reading governs here: the second loss matters more than the first, because
one loss reads as a careless close and two do not — the failure is the
ordinary case, not an exception needing a backstop.

## The rule

Where a record must outlive a worktree, its durability cannot be a step in a
closing checklist. It has to be true from the moment the record is first
written — see [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md),
which applies this specifically to campaign decision ledgers.

## Consequences

- This decision does not, by itself, fix anything — it is the generalized
  claim that justifies 0486's mechanism. A future artifact proposing "copy it
  over at the end" as its durability plan should be read against this record
  before being accepted.
- It does not extend to material this campaign deliberately keeps scratch —
  implementer reports and review packages remain in `.superpowers/sdd/` and
  die with the worktree by design, because they are regenerable or
  derivative of the ledger, not the ledger itself (§2 of the spec; the
  registry row this campaign adds for the deferred half,
  `PROC-implementer-reports-and-reviews-still-die-with-the-worktree`).
