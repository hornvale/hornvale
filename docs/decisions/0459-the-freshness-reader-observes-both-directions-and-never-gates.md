# 0459. The freshness reader observes both directions, and never gates

**Status:** Accepted (2026-08-30) · **Decider:** Nathan (autopilot) ·
**Relates:** [0458](0458-tracked-ness-proves-coverage-never-freshness.md),
[0457](0457-a-generated-paths-author-absence-is-declared-not-deleted.md),
[0426](0426-the-heavy-tier-is-a-phase-of-the-queue-again.md) (a gate that reds
for a benign reason trains people to ignore it) ·
[The Attestation](../../book/src/chronicle/the-attestation.md)

In the context of building `cli/src/attest.rs` to diff `docs/timings.md`
against what the roster and the generated-paths declarations say a job owes,
we decided that **the reader reports in both directions — a phase owed and
absent, and a phase present but not owed — and it reports, it does not
gate**, accepting that a real absence sits in the ledger, visible, until a
human or a later campaign decides to act on it.

## Context

Two properties bound what this reader may claim, and both are load-bearing.
First, `docs/timings.md` carries no exit code — a row witnesses that a phase
*ran*, never that it passed, so the reader must never imply otherwise.
Second, a job is identified only by its rows' adjacency (the ledger carries
no job id), so the reader cannot always attribute one commit to a whole job's
phases.

The reader's first real run against the committed ledger reported five
"owed but absent: clients" jobs and called the payoff hypothesis (H3)
confirmed. Every one was a false positive: `scripts/sluice-phases.sh`
deliberately drops `clients` (and `heavy`, `seam-guard`) from a candidate
whose every changed path is hand-written prose, and each of the five was
exactly that case, verified against the queue's own logs. The first version
of this reader had computed "owed" from the roster's rungs alone and ignored
that the chamber's own phase list is *conditional* on the candidate — the
campaign's payoff instrument produced a confident false positive on its
first run, the same defect shape the campaign exists to find, committed by
the tool built to find it.

## The rule

- `unconditionally_owed` (a rung the chamber never drops) feeds
  `owed_but_absent`, a real claim of absence.
- `conditionally_owed` (a phase `sluice-phases.sh` may legitimately drop for
  a prose-only candidate) feeds a separate `undetermined` category, reported
  honestly as "cannot verify from the ledger alone whether this is a
  legitimate narrowing or a real absence" — never folded into the confident
  claim.
- `none(<reason>)`-declared authors (decision 0457) are their own category,
  never looked up in the ledger and never reported as an absent author.
- The reader reports; it does not fail a gate. Nathan chose observability
  over prevention: a gate stops one known failure and leaves the record
  silent about the rest, and a gate that reds for a benign, already-explained
  reason trains people to ignore it — the disease decision 0426 diagnosed in
  the heavy tier itself. A later campaign may add a gate on evidence this
  reader accumulates.

## Consequences

- The payoff hypothesis this campaign preregistered (H3: the reader surfaces
  an absence nobody already knew about) came back **NULL** once the false
  positives were corrected: eighteen "present but unowed: seam-guard" rows
  are pre-0148 history, and `census`'s permanent inability to produce a
  `sluice:census` row is a standing, already-documented dispatch fact, not a
  new defect. A null is a preregistered, legitimate result: the instrument is
  correct and the repository is currently clean of any absence not already
  known.
- `CONDITIONALLY_DROPPABLE` is a literal mirror of `sluice-phases.sh`'s own
  drop list, guarded by its own agreement test
  (`the_conditionally_droppable_set_agrees_with_sluice_phases_sh`) rather than
  left as a second, uncross-checked copy — applying decision 0456 to the
  reader that decision 0459 itself introduces.
- A future campaign adding a gate on top of this reader must preserve the
  `undetermined` category rather than collapsing it back into
  `owed_but_absent` — that collapse is precisely the false positive this
  decision records.
