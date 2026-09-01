# 0486. A campaign's decision ledger is a committed document, not scratch

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0020](0020-campaigns-write-retrospectives.md) (the retrospective
this practice used to feed) · [The Cartulary](../../book/src/chronicle/the-cartulary.md)

In the context of a campaign's rulings, deferred minors and parked findings
living only in `.superpowers/sdd/`, git-ignored and per-worktree, and the
prescribed remedy — promote that material into the retrospective at close, by
hand — having failed five recorded times, we decided **a campaign's decision
ledger is written from the start to a committed, per-campaign path,
`docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, and nothing is ever promoted
into it from scratch at close**, accepting that this is a reversal of a
standing hook comment that called committing the ledger "always a mistake,
never a judgment call."

## Context

Five campaigns lost exactly this material: The Ell (promoted nine items, lost
six), The Quoin ("the material was written down, the instruction to promote it
was written down beside it, and it was still lost"), The Gallery/Lodestar
(wrote a 193-line retrospective, skipped verification, and belief and `grep`
disagreed on eleven counts), The Overture (worktree recycled before close;
nine decision records reconstructed from module doc comments), and The
Attestation (worktree recycled between the merge landing and the close walk;
nine deferred minors and two parked findings reconstructed from a session
transcript).

The two most recent failures — The Quoin and The Lodestar — had already tried
the obvious fix: verification that reads the ledger back and confirms each
item landed. It did not hold, for a reason verification cannot address: the
ledger was gone before anything could verify against it. A check that
requires an artifact to exist cannot detect that artifact ceasing to exist
(see [0489](0489-verification-cannot-substitute-for-durability.md)).

## The rule

A campaign's ledger is a real file in the tree from Task 1 (or the earliest
task after the campaign has anything to record), committed at each task
boundary and each ruling — not written to `.superpowers/sdd/decision-
ledger.md` and copied over later. `docs/superpowers/ledgers/README.md` states
what belongs here and what deliberately does not
(see [0487](0487-per-campaign-ledger-paths-are-what-make-committing-safe.md)).

## Consequences

- The five-times-failed promote-at-close step is retired outright, not
  patched again — see [0488](0488-a-record-that-must-be-manually-copied-to-survive-will-not.md).
- `scripts/hooks/pre-commit`'s `.superpowers/` guard comment and `CLAUDE.md`'s
  absorption-clobber warning are both superseded, visibly, in the same
  campaign that reverses them (Task 2).
- H1 — that a committed ledger actually survives a recycled worktree — was
  measured before this decision was allowed to stand on faith alone; see the
  chronicle and [The Cartulary](../../book/src/chronicle/the-cartulary.md).
