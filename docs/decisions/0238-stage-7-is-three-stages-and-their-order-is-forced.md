# 0238. Stage 7 is three stages, and their order is forced

**Status:** Accepted (2026-08-24) · **Decider:** Nathan · **Relates:**
[0236](0236-a-fold-advances-it-is-not-invalidated.md);
[0237](0237-the-reset-event-is-the-checkpoint.md);
[The Penstock metaplan](../superpowers/specs/2026-08-22-the-penstock-metaplan.md)
§6; [The Tailrace spec](../superpowers/specs/2026-08-24-the-tailrace-design.md)
§0, §9

In the context of the Penstock metaplan's §6 carving **log bounding** as a
single stage 7 — "a fact-lifetime mechanism: what may leave the log, and how
the seed plus the surviving prefix still re-derives the world" — while the
idea registry carried *two* incompatible rows for it, we decided that
**stage 7 is three stages, and that their order is not a preference but a
consequence**:

| | delivers |
|---|---|
| **7a** | the read side: nothing folds raw history any more |
| **7b** | the typed, compositional intention |
| **7c** | fact lifetime: what may leave, and how the prefix re-derives |

This amends the metaplan's §6 stage table. 7a shipped as The Tailrace.

## Why the order is forced rather than chosen

**7c cannot precede 7a.** Six production folds in
`windows/vessel/src/liveness.rs` read the per-step `agent-at` trail as
*semantics*, not as a cache: the thirst and hunger path integrals segment by
sighting and integrate temperature × duration, `believed_water` folds every
water facet the agent has stood in, `hazard_memory_memo` keeps
latest-visit-per-facet, and `build_emitter_scan` derives alarm halos from
other agents' positions. Removing facts — by abstention at the commit site or
by compaction after it — silently changes all of them. The registry row that
proposed abstention described itself as "cheap, localized" with "zero
committed-artifact blast radius"; the second half is true and is precisely
why the first is not, because nothing committed would redden.

**7b must precede 7c, and this is a fidelity call.** The trail is *content*,
not bookkeeping. Each step's `provenance` is authored prose — *"went down to
the river it knew (thirst)"*, *"fled the uncanny ground (fear)"* — and it is
rendered, by `windows/historiography/src/lib.rs` and `cli/src/repl.rs`.
Replacing per-step commits with per-errand ones before the intention carries
its own compositional `why?` would delete readable content. That is a
fidelity cut, which this project brings to a human rather than absorbing as
an optimisation.

## What this costs

7a removes no fact, so it does not reduce the ledger by one byte; the
metaplan's storage argument (§6.1) is untouched by it. What 7a buys is that
the *precondition* both other stages silently assumed is now true, and that
is the only reason either is safe to attempt.

## Alternatives rejected

- **Deliver 7a under the name "stage 7"**, leaving the metaplan recording a
  solved storage problem. Rejected: the metaplan's own §6.3 calls stage 7
  "the one item in this program with no alternative", and a reader who took
  it as closed would stop looking.
- **7a + 7c together, skipping 7b.** Rejected on the fidelity argument above.
- **Keep one stage and let its implementer choose.** Rejected: the two
  registry rows had already existed side by side for the life of the program
  without either being entered, which is what a mis-carve looks like from
  the outside.
