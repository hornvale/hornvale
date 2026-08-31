# 0460. A committed declaration is checked against its siblings without the remote ledger

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0456](0456-a-rule-stated-in-two-places-needs-a-bidirectional-agreement-test.md) ·
[The Attestation](../../book/src/chronicle/the-attestation.md)

In the context of spec §5a asking for a check that a decision record's number
falls inside the block its own campaign reserved, and that check needing
`scripts/decision-block.sh`'s ledger — `$HOME/.local/state/hornvale/decision-
blocks/blocks.tsv`, per-machine, never committed, reachable only by ssh — we
decided **the specced check is not buildable from a checkout and a workspace
test must not ssh to get it, so a narrower, repo-only substitute ships instead:
pairwise disjointness of the `Decision block: NNNN-MMMM` declarations already
committed in every spec header**, accepting that the substitute cannot see the
one incident (`the-stride`/`the-burr`) that never declared a block at all.

## Context

`git log --all -- '*blocks.tsv'` and a full-repo search both return nothing —
the reservation ledger has never been committed under any name, at any point
in this repository's history. `scripts/test-decision-blocks.sh` only exercises
the allocator's own gap/collision arithmetic against a temp-dir override; it
never reads the real ledger and self-skips off the canonical box. Building the
specced check would mean either sshing from a workspace test (forbidden) or
moving where the ledger lives (a separate, infrastructure-scope decision) —
neither of which this task's scope covers.

Looking for what *is* writable from the repository alone surfaced a third
confirmed instance of the same defect class, already recorded in
`docs/retrospectives/the-quadrat.md`: The Scarf and The Quadrat both committed
`Decision block: 0286–0295` in their own spec headers, a day apart, and
nothing diffed the two headers against each other. That collision needs no
remote ledger at all — both declarations already sit in already-committed
files.

## The rule

`cli/tests/suite/docs_consistency.rs` gains
`decision_blocks_do_not_overlap_across_campaigns`: parse every
`Decision block: NNNN[-–]MMMM` occurrence out of
`docs/superpowers/{specs,plans}/*.md` and assert the ranges are pairwise
disjoint, failing with both files and the overlapping span named — one stage
earlier in the pipeline than the existing `decision_numbers_are_unique`,
which checks the *records*, not the *declarations*. A companion ratchet,
`decision_block_declaration_count_has_not_dropped`, guards against the
tolerant header-format regex silently losing coverage as spec prose drifts
(four format variants were found across 18 occurrences on the day this
landed). The one already-resolved historical collision (Scarf/Quadrat) is
append-never waived — editing either header would misstate what each campaign
actually reserved.

**Stated honestly, not sold:** this check would have caught Scarf/Quadrat
mechanically, before either decision was minted. It plausibly catches a
timing-sensitive case like The Overture's wrong `0357–0366`, depending on
branch-merge ancestry at the moment it runs. It **cannot** catch
the-stride/the-burr — the-stride declared no block at all, and a check that
diffs declarations against each other has nothing to diff when one side never
declared anything.

## Consequences

- This is a strict subset of the specced check, not a substitute for it — if
  the remote ledger is ever made committable, the ledger-reading check (now
  an idea-registry row, `PROC-decision-block-ledger-not-committable`) becomes
  buildable and should still be added.
- A repo-only check only sees what has already merged into the tree it runs
  against; two campaigns that both draft a spec before either merges remain
  invisible to each other under this check, same as today.
