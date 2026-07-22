# Retrospective — The Single Saying (PROC-18)

One page of process, not product. The product is chronicled; this is what
the close learned that the code does not record.

## What went well

- **Research done BEFORE writing the plan found two live, currently-uncaught
  bugs on `main`, and the plan's own migration closed both as forced side
  effects rather than as separately-scoped fixes.** Reading
  `domains/terrain`'s actual `streams.rs` and `stream_labels()` during
  brainstorming (not just the spec's abstract description of the desync
  class) found the manifest was missing 5 real, already-drawn constants
  (`LOBING`, three crust noise slices, a rift crenulation leg) — silent,
  undocumented, `make gate` green throughout, no test anywhere would have
  caught it. `domains/climate` had a smaller version of the same thing
  (`WEATHER_PHASE` existed, the manifest still said "none yet"). Because
  the macro requires every declared constant to appear in the one list
  that also generates the manifest, migrating these crates could not
  silently carry the gaps forward — closing them cost nothing beyond
  writing real descriptions for the previously-missing entries. This is a
  stronger form of "test-driven bug discovery" than usual: the fix
  mechanism itself made the omission impossible to reproduce, rather than
  a test merely detecting it.
- **A design decision got corrected mid-brainstorm by directly reading the
  actual enforcement code, not by trusting a stale memory note.** The
  original design draft justified "macro over lint" by citing a memory
  note claiming `tools/type-audit` was CI-only and invisible to
  `make gate`. Reading the `Makefile` directly (`gate: fmt-check clippy
  type-audit test`) showed this was stale — The Named's own retrospective
  followup ("fold type-audit into `make gate`") had since been done. The
  real justification (the existing check verifies an orthogonal invariant
  — *where* a literal is written, never *whether* a declared constant
  reached the manifest — and cannot be extended to catch this campaign's
  bug without re-deriving the same list the macro already builds for
  free) is stronger and more honest than the one first drafted. The stale
  memory note was corrected in place during this same session. **Lesson:
  a memory claim about tooling behavior is a snapshot, not a live fact —
  verify against the actual config file when a design decision leans on
  it, especially when the claim is convenient for the argument you're
  already making.**
- **Astronomy's own prior mitigation attempt (`ALL_LABELS` + a
  cross-check test, added after a real incident during The Reckoning) is
  concrete evidence that hand-maintained redundancy doesn't scale — it
  just moves the duplication.** That test worked (astronomy's manifest was
  correct on `main`), but cost a THIRD authored list to maintain, and gave
  zero protection to any other crate (terrain, sitting right next to it,
  drifted anyway). The retrospective's generalizable point: when a bug
  recurs after a test was already added to catch it (elsewhere in the same
  codebase), that's a signal the fix needs to move up a level — from
  "add a check" to "remove the possibility of divergence" — not a signal
  the check needs to be copied to more places.
- **The recurring subagent-parking trap hit again, a third documented
  time this session (after two occurrences during PROC-17's own
  execution).** Task 9's reviewer launched a background `make gate` job
  mid-review and reported "waiting for the notification" — resumed with an
  explicit un-parking message, and on resuming discovered its own earlier
  background attempt had, in fact, already finished (a false alarm, not a
  real hang), then re-verified independently anyway rather than trusting
  that. The dispatch preamble was present and unchanged from PROC-17's.
  **This is now a 3-for-3 pattern across two consecutive campaigns using
  the same mitigation — the preamble reduces frequency but does not
  eliminate the failure mode.** Worth flagging as a standing open problem,
  not a solved one, for whoever next revisits subagent dispatch tooling.

## What was awkward

- **The closing walk itself needed TWO rounds of main-absorption**, not
  one — main moved 63 commits during this campaign's execution (a large
  gap, since the campaign ran long enough that many parallel sessions
  landed work), then moved 7 more commits during the ~15-30 minute full
  `make gate` run triggered by the FIRST absorption. This is the system
  working as designed (`make preflight` caught both), but is worth naming
  as a scaling reality: on a project with this much parallel campaign
  activity, a long-running campaign's close should expect at least one
  "main moved again while I was gating" cycle, and budget for it rather
  than being surprised by it.
- **Local `main` and `origin/main` had diverged** (local `main` carried an
  unpushed campaign close, "the-assay," that `origin/main` didn't have
  yet) — the first absorption attempt merged `origin/main` and still
  failed preflight's ancestry check, because the script checks against
  local `main`, not the remote-tracking ref. A second merge (`git merge
  main`) was required to actually satisfy the check. Lesson: when
  absorbing main into a long-running worktree, merge the LOCAL `main`
  branch (not `origin/main`) unless you've first confirmed the two are
  identical — this project's convention treats a local main merge as
  effectively "published" (per prior campaigns' own notes), so local can
  legitimately be ahead of origin.
- **A brand-new crate, `domains/history`, landed on `main` mid-campaign**
  (from an unrelated, parallel campaign) using the pre-PROC-18 pattern —
  hand-authored `StreamLabel` constants plus a separately hand-typed
  `stream_labels()`. Worse, its shape doesn't fit either of this
  campaign's two macro forms cleanly: it mixes a genuine root+leg pair
  (`ROOT`/`RESIDUE`/`STRUCTURES`) with several already-fully-qualified
  flat entries (`BAKE`, `GENESIS`, `FLESH`) in the SAME file, which
  neither the flat nor the root+leg macro form was designed to combine in
  one invocation. Migrating it was explicitly kept out of this close's
  scope rather than expanded into ad hoc — see Follow-ups.

## Follow-ups

- **`domains/history` needs migrating to `stream_labels!`**, but its mixed
  root+leg/flat shape (one crate, one `ROOT`+2 legs, plus 3 independent
  flat full-path entries) isn't covered by either of this campaign's two
  macro forms. Either the macro needs a third, hybrid form, or the crate's
  flat entries need to move to a small second `stream_labels!` invocation
  whose output gets merged into the crate's single `stream_labels()` by
  hand (a much smaller, bounded fix — not a full third macro arm). Not
  built here: this crate didn't exist when the spec was written, and
  fitting its shape properly is real, if small, design work that deserves
  its own five minutes of thought rather than a rushed change during a
  close.
- **`domains/language`'s composed multi-leg pattern documentation**
  remains out of scope, exactly as PROC-18's own registry row scoped it
  from the start — unifying `"language/<species>/phonology/inventory"`-
  style prose with the real per-leg `.derive()` chain building it is a
  materially different, harder problem than fusing one constant with one
  manifest row.
- **The subagent-parking recurrence (3rd instance, 2 campaigns)** is
  banked as an open problem, not a fix — the existing mitigation
  (dispatch preamble + explicit un-parking messages) works when applied,
  but doesn't prevent the parking from happening in the first place.
