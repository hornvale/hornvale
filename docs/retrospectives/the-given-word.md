# Retrospective — The Given Word (PROC-17)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **The expand-contract structure absorbed a real, mid-execution planning
  defect without anything ever breaking.** Task 10's own review found that
  this campaign's original plan — built from a single `grep '\.derive("'`
  pass during writing-plans — had a structural blind spot: it could see
  inline string literals but not call sites already passing a
  *pre-declared* `&str` constant (`.derive(streams::STAR_MASS)`). An
  entire category of debt (~21 constants in `domains/astronomy` alone,
  plus `domains/climate`, `windows/locale`, `windows/vessel`, and
  `kernel`'s own `room.rs`) was invisible to the plan from the start. This
  was serious — had it gone unnoticed, the campaign's own final task
  (deleting `Seed::derive(&str)`) would have broken the build in ~40
  places across 8 crates — but it was never *dangerous*, because the
  expand-contract design kept the old method alive and working through
  every intermediate task. The gap surfaced as a review finding, not a
  production incident, precisely because nothing destructive happens
  until the last, gated task.
- **Four new tasks (10b-10e), inserted mid-execution, closed the gap using
  the exact same technique the other ten tasks already used** (retype a
  constant, migrate its call sites, run that crate's own unchanged test
  suite, prove byte-identity) — no new mechanism, no re-architecture, just
  more instances of an already-proven pattern. The final whole-branch
  review explicitly endorsed this as the right-sized response: not
  reopening Task 10's own (verified-correct) diff, and not a full
  stop-and-replan for what was, in the end, "the plan missed some files,"
  not a design defect.
- **The new type-audit check found a real gap in already-approved work
  the moment it existed.** `kernel/src/noise.rs`'s `OCTAVE_LABELS` table
  had been correctly migrated to `StreamLabel` in Task 10, but nobody had
  noticed it was declared outside `streams.rs` — exactly the shape the
  brand-new Task 11 check exists to catch. It caught its own campaign's
  own prior work before that work ever reached main. This is the
  strongest possible validation that the check does what it was built
  for.
- **Two agents independently made, and were corrected out of, the same
  parking mistake** — a dispatched subagent launching a background test
  run and then waiting for an async notification that will never come
  (Monitor tools and background jobs both die with a subagent's own
  turn). Both were resumed with an explicit foreground-poll instruction
  and completed correctly afterward. Worth a standing note: this trap
  recurs even after `dispatching-hornvale-subagents`' own preamble
  discipline, because it's not about the dispatch prompt's wording — it's
  that a subagent's own instinct, faced with a genuinely long-running
  command, reaches for the SAME async tools the *controller* session
  legitimately has access to, without registering that it is not the
  controller.

## What was awkward

- **The plan's own research method (one grep pass, run once, during
  writing-plans) was never re-validated against a DIFFERENT search
  pattern before execution began.** The self-review step the
  writing-plans skill itself prescribes ("spec coverage: can you point to
  a task that implements it") checked coverage against the SPEC's own
  claims, but the spec itself inherited the same blind spot from the same
  original grep. A second, differently-shaped sweep (e.g. searching for
  every `pub const.*: &str` declaration inside any `streams.rs`-adjacent
  file, independent of how it's called) during brainstorming or planning
  — rather than only during execution's own task-level reviews — would
  have caught this before a single task was dispatched. Generalizable
  lesson: when a plan's own scope was DERIVED by grep, the plan is only
  as complete as that grep's pattern; a plan-level "did we search for
  every SHAPE of the thing we're hunting, not just the one pattern we
  first thought of" check belongs in writing-plans' own self-review, not
  left to be discovered by a task reviewer three-quarters of the way
  through execution.
- **Main moved 14 commits during this campaign's own long execution**,
  requiring a real absorption at close (one conflict, the now-familiar
  `type-audit-report.md` regen). Nothing broke, and the absorption cadence
  itself (merge main in, re-run the full gate, re-preflight) worked
  exactly as designed — but a campaign running this long (17 tasks
  including the remediation wave) might have benefited from at least one
  mid-execution absorption per CLAUDE.md's own stage-boundary cadence,
  rather than one absorption at the very end. Worth naming as a
  reminder for any future campaign of comparable size: check whether main
  has moved BETWEEN task waves, not only at close.

## Follow-ups

- **A trailing-comma false negative in the new type-audit check**
  (`StreamLabel::from_static("x",)` slips past the token scanner) — real,
  low severity (the workspace is currently clean of this shape, and
  `rustfmt` strips trailing commas on single-arg single-line calls
  anyway), captured as a future one-line follow-up rather than fixed now.
- **PROC-18** (a `stream_label!` declaration macro unifying constant
  declaration with `stream_labels()` manifest-registration) remains
  captured, not built — this campaign's own scope stayed at "stop passing
  bare strings," not "also automate the manifest-doc/code sync."
