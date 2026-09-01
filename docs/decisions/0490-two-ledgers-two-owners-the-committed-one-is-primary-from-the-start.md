# 0490. Two ledgers, two owners: the committed one is primary from the start

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0486](0486-a-campaigns-decision-ledger-is-a-committed-document-not-scratch.md) ·
[The Cartulary](../../book/src/chronicle/the-cartulary.md) §4a

In the context of plan-writing discovering that there are actually **two**
scratch ledgers with different owners — `decision-ledger.md` (rulings, Q
entries, ideonomy passes; defined by this repo's own `campaign-autopilot`
skill) and `progress.md` (task state, fix rounds, deferred minors, parked
findings; defined by the vendored superpowers plugin, not editable from this
repository) — and the material The Attestation actually lost living in the
plugin-owned file, we decided **the committed ledger becomes the PRIMARY home
for the durable kinds — rulings, deferred minors, parked findings — written
there directly by the controller from the start, via the in-repo skills**,
accepting that the vendored plugin's `progress.md` is neither touched nor
mirrored and keeps its narrower, genuinely-scratch job.

## Context

A campaign cannot change where the plugin writes, and a local edit to a
versioned plugin path would be silently overwritten by the next plugin
update — so "update the plugin's ledger location" was never an available
remedy, however the spec first framed it. Mirroring from scratch to durable
at intervals was considered and rejected (again) for reintroducing the manual
copy step this whole campaign exists to remove.

## The rule

Rulings, deferred minors and parked findings are written directly to
`docs/superpowers/ledgers/<slug>.md`, by the controller, per the in-repo
skills this repository can edit (`campaign-autopilot`, `closing-a-campaign`,
`dispatching-hornvale-subagents`) — not promoted, not mirrored, not copied
from `progress.md` at any point. `progress.md` keeps task state and
resume-after-compaction material, which does not need to survive worktree
recycling: a recycled worktree means the campaign is over, and task-
completion facts are recoverable from `git log` regardless.

## Consequences

- Task 4 re-pointed the three in-repo skills' ledger references. One of its
  own edits then broke on contact with the change it made:
  `closing-a-campaign`'s step 2 kept instructing a closer to route ledger
  entries by grepping `.superpowers/sdd/`, which is exactly the location the
  entries no longer live in. Fixed the same task, and the fix was split into
  two named halves (scratch sweep, committed-ledger read) precisely so a
  closer executing the step literally, not reading it for intent, still
  finds the material.
- 81 hits in frozen specs, 59 in plans, 38 in retrospectives, and 2 in
  `CLAUDE.md`'s own superseding paragraph were left untouched, deliberately —
  they describe the ledger as it stood when written, and editing history to
  match current practice would destroy the evidence this campaign argues
  from.
