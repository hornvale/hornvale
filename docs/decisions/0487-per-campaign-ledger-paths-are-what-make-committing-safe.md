# 0487. Per-campaign ledger paths are what make committing safe

**Status:** Superseded by [0493](0493-decision-ledger-md-not-progress-md-is-the-shared-scratch-file.md) (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md)

In the context of `CLAUDE.md` warning that a committed ledger "silently
clobbers every parallel session's on absorption, raising no conflict," and
that warning being read as an objection to committing a ledger at all, we
decided **the clobber is a property of the shared filename, not of
committing** — every campaign's scratch ledger lived at the identical path,
`.superpowers/sdd/decision-ledger.md`, so two campaigns editing it merged to
one side silently — and the fix is a path keyed by campaign slug,
`docs/superpowers/ledgers/YYYY-MM-DD-<slug>.md`, touched by exactly one
campaign, ever, accepting that this narrows a previously blanket-sounding
warning rather than overturning it.

## Context

Two remedies were considered and discarded at spec time: a single shared
ledger file for all campaigns (reintroduces exactly the collision `CLAUDE.md`
already warns about), and committing only at campaign close (both worktree-
recycling losses — The Overture, The Attestation — happened before or during
the close walk, so a ledger that first reaches git at close is exposed for
the entire campaign up to that point).

## The rule

`docs/superpowers/ledgers/` holds one file per campaign, matching `specs/`
and `plans/`'s own naming convention and sibling tree. An absorption sees a
clean add for a new campaign's ledger — there is no collision to have,
because no other campaign's commits ever touch that path.

## Consequences

- `CLAUDE.md`'s absorption-clobber sentence is narrowed, not deleted: it
  stays exactly true of `.superpowers/sdd/decision-ledger.md`, the shared
  scratch path every campaign still writes for the kinds that stay scratch
  (task state, resume-after-compaction material — see
  [0490](0490-two-ledgers-two-owners-the-committed-one-is-primary-from-the-start.md)).
- A stray file dropped into `docs/superpowers/ledgers/` later (a README, a
  misplaced note) is not mistaken for a campaign's own ledger by any check
  that resolves the ledger **by slug**, never by listing the directory — see
  the ratchet built to prove that (Task 3, `docs/superpowers/ledgers/README.md`).
