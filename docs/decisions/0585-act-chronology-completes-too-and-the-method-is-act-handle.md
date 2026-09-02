# 0585. `bundle:act-chronology` completes too, and the method is `Act::handle`

**Status:** Accepted (2026-09-02) · **Decider:** Nathan (autopilot) · **Campaign:** The Avowal · **Supersedes:** 0580

## Context

Task 8 (Definition of Done) found two errors in decision 0580, both caught by
re-reading the shipped code and the regenerated audit reports against 0580's
own text, not by anything a gate flagged:

1. **Point 2 names a method that was never shipped.** 0580 says "`ActHandle::
   of` folds four *different* constituents through separate steps." The
   shipped method is `Act::handle(&self) -> ActHandle`
   (`windows/vessel/src/act.rs:151`) — an inherent method on `Act`, not an
   associated function on `ActHandle`. `ActHandle` itself has no `of`. This
   is a naming slip in the decision's prose; the design it describes (four
   constituents, four separate `mix` steps, the collision found and fixed)
   is correct and unchanged.

2. **Point 6 reports one completed bundle where the task actually completed
   two.** 0580 states "`bundle:witnessing` reads 2/2 tokens" and stops there.
   `bundle:act-chronology` (`predicate:act-occurred-on`,
   `predicate:act-precedes`, `predicate:deed-of`, `predicate:history-now` —
   `tropes/polti.trope.json`) also completed in this task, and 0580 never
   says so. `predicate:history-now` was already served through the ledger
   home before this task (it is a pre-existing registered predicate,
   `hornvale_history::HISTORY_NOW`, and `Provision::build` gives every
   registered predicate an automatic `Present(Home::Ledger)` row — see
   decision 0576); the three session-home tokens this task adds are the
   other three members of the bundle. Once all four resolve, the bundle is
   complete, the same as `bundle:witnessing`. `bundle:consanguineal-kin`
   (Task 5) is the only other bundle either wired-total campaign has fully
   satisfied; this task ships the second and third.

## The correction

**Both bundles this task completed are `bundle:witnessing` (2/2) and
`bundle:act-chronology` (4/4).** Verified against the regenerated report
committed in this task's own commit (`f24b668c1`), by diffing it against its
immediate parent:

- `docs/audits/trope-coverage-polti-1895.md`'s Leverage section: **the
  corpus's missing-bundle count moved 30 → 28** ("The corpus holds 28
  missing bundles against the 27 ranked here" replaces "30 ... 29"), and
  both `bundle:witnessing` and `bundle:act-chronology` disappear from the
  fan-in table entirely — neither blocks any situation any longer.
- `polti-03-crime-pursued-by-vengeance`, `polti-04-vengeance-for-kin-upon-kin`,
  `polti-17-fatal-imprudence` and `polti-34-remorse` each lose exactly three
  tokens from their `missing` list — `predicate:act-occurred-on`,
  `predicate:act-precedes`, `predicate:deed-of` — the three
  `bundle:act-chronology` tokens this task newly serves (`predicate:
  history-now` was never in any of these four lists, confirming it already
  resolved before this task). `polti-06-disaster`, `polti-19`, `polti-27`,
  `polti-32`, `polti-33` and `polti-36` each lose `predicate:present-at` and
  `predicate:witnessed`, the `bundle:witnessing` pair.
- `docs/audits/trope-coverage-tvtropes-2012.md`'s Leverage table loses both
  bundle rows too: `bundle:act-chronology` (40 situations) and
  `bundle:witnessing` (28 situations) no longer appear.
- Neither corpus's `Stageable` count moved: `polti-1895` holds at 0 of 36,
  `tvtropes-2012` at 0 of 409 — completing a bundle shortens a `Blocked`
  reason, it does not by itself clear the witness bar (decision 0577/0583).
  This is spec §5's preregistered null, unaffected by which of the two
  bundles' completion is being described.

No code changed to produce this finding — it corrects what 0580 *reported*
about a result it had already shipped, not the result itself.

## Consequences

- `docs/decisions/0580-acts-are-addressable-without-being-stored.md` is
  restored to its original text (per this campaign's own append-only
  practice — see 0577, 0578, 0581, 0582's own restorations) with only its
  **Status** line marked `Superseded by 0585`. Its `ActHandle::of` naming
  slip and its one-bundle report remain visible in that restored text as the
  historical record of what was written and when.
- No artifact regenerates from this record: the reports it cites were
  already committed correctly by Task 7 (`f24b668c1`); this decision
  corrects the *prose that describes* them, not their bytes.
- A reader following `bundle:act-chronology` or `bundle:act-precedes` in the
  audits should expect zero remaining `missing` occurrences in either frozen
  corpus, the same as `bundle:witnessing` and `bundle:consanguineal-kin`.

## See also

Decision 0580 (superseded by this record); decision 0576 (the ledger home's
automatic per-registered-predicate row, which is why `predicate:history-now`
predates this task); decision 0578/0584 (`bundle:consanguineal-kin`, the
first bundle this project ever fully satisfied); `windows/vessel/src/act.rs`
(`Act::handle`); `docs/audits/trope-coverage-polti-1895.md`,
`docs/audits/trope-coverage-tvtropes-2012.md`; `tropes/polti.trope.json`'s
`bundles.act-chronology`; `docs/superpowers/ledgers/2026-09-01-the-avowal.md`
ledger entry #17; `.superpowers/sdd/2026-09-01-the-avowal/task-8-report.md`.
