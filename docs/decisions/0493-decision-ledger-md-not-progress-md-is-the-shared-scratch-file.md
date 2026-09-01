# 0493. `decision-ledger.md`, not `progress.md`, is the shared scratch file

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Supersedes:** [0487](0487-per-campaign-ledger-paths-are-what-make-committing-safe.md) ·
**Relates:** [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md) ·
[0490](0490-two-ledgers-two-owners-the-committed-one-is-primary-from-the-start.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md)

In the context of 0487 naming `.superpowers/sdd/progress.md` as the shared,
unkeyed scratch file whose collision hazard per-campaign paths dissolve, and
the campaign's final review measuring across 13 live checkouts that
`progress.md` has always lived at the per-campaign-keyed
`.superpowers/sdd/<slug>/progress.md` while `.superpowers/sdd/decision-
ledger.md` is the file that actually sits flat and shared, we decided
**0487's exemplar was wrong and is corrected here: the shared file the
absorption-clobber hazard describes is `decision-ledger.md`**, accepting
that this also means 0487's Consequences bullet — which additionally
mis-attributed `progress.md`'s job (task state, resume-after-compaction) to
`decision-ledger.md` and called that file "still written" — was wrong on
both counts and needed more than a citation fix.

## Context

Ground truth, measured 2026-08-30 across 13 live campaign worktrees:

```
.superpowers/sdd/decision-ledger.md       <- flat, SHARED, present at this exact path in most worktrees
.superpowers/sdd/<campaign-slug>/progress.md   <- ALWAYS under a per-campaign subdirectory
```

`CLAUDE.md`'s own wording and 0487's Context section both correctly named
`decision-ledger.md`. The error entered in 0487's Consequences bullet, and
independently in spec §3, spec §4, the plan, and two places in this
campaign's own exemplar ledger — five places in total, all citing
`progress.md` where `decision-ledger.md` was meant. The spec's own §7
verification table marked the shared-filename claim "verified — read at
spec time from `CLAUDE.md`'s own wording," which checked that a path was
named, not which one — `CLAUDE.md`'s original sentence named none
explicitly, and every citation of a specific file downstream of that
verification introduced its own, independent chance to get the file wrong.
Four of five did.

The conclusion 0487 reached — that a path keyed by campaign slug dissolves
the collision, because a shared unkeyed file is what created it — is
unaffected by which file was named as the example. This decision corrects
the example and the resulting mischaracterization; it does not reopen the
conclusion.

## The rule

- The shared, unkeyed scratch file this project's absorption-clobber hazard
  describes is `.superpowers/sdd/decision-ledger.md`, defined by this
  repository's own `campaign-autopilot` skill. Per decision
  [0490](0490-two-ledgers-two-owners-the-committed-one-is-primary-from-the-start.md),
  campaigns no longer write to it going forward: rulings are written
  directly to the committed, per-campaign ledger instead. Where a copy of
  `decision-ledger.md` still exists in an older checkout, it is a leftover,
  not a live write target.
- `.superpowers/sdd/<campaign-slug>/progress.md` is defined by the vendored
  superpowers plugin, has always been per-campaign-keyed, and never carried
  the shared-filename hazard. It keeps its own separate job — task state,
  fix rounds, resume-after-compaction material — untouched by this
  correction and by 0490.
- A path keyed by campaign slug (`docs/superpowers/ledgers/<slug>.md`) is
  touched by exactly one campaign, ever — an absorption sees a clean add,
  and there is no collision to have. This is 0487's original conclusion,
  restated here because 0487 is superseded wholesale rather than left
  half-correct.

## Consequences

- 0487 is superseded in full. Its Y-statement and rule were correct; this
  record restates them with the citation corrected rather than leaving a
  reader to reconcile a right conclusion against a wrong Consequences
  bullet.
- Five citations of the wrong file — spec §3, spec §4, the plan, and two
  places in this campaign's own committed ledger — are corrected in place,
  each with a visible note pointing here, per the same supersede-visibly
  idiom this campaign already applied to `CLAUDE.md` and the pre-commit hook
  comment.
- `docs/superpowers/ledgers/2026-08-30-the-cartulary.md` and
  `docs/decisions/0487-*.md` are the two documents this correction was
  found against; a future reader following either now reaches this record
  rather than a dead end.
