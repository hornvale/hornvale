# The Rack — retrospective

Process, not product. The chronicle carries what was built.

## The gate that passed at nine times its ceiling, and why nobody had checked

The campaign's sharpest process finding is not that a wall-clock gate is
blunt — everybody knew that, and `session_cost.rs`'s own doc said so in
writing. It is that the gate **was not running at all**, and the reason was
sitting in two correct-looking facts in two different files.

`BASIS_HOST = "aarch64-10"` gates the millisecond assertions to the Mac. That
is a defensible choice and its doc argues for it well. Decision 0133 moved the
heavy tier to run exclusively on lefford (`x86_64-40`). That is also a
defensible choice and its record argues for it well. **Nothing compares the
two.** Their intersection is empty, so the ceilings have fired nowhere since
the tier moved boxes, and a control that grew twenty-one-fold under The Roll
passed every run in between.

The generalisable lesson is not "check your host guards". It is that **a check
is worth only the configurations it actually runs in**, and the configuration
is usually the product of two facts neither of which is wrong. This project
already has that lesson recorded once (The Siding); this is the same shape
found in a second place, which is the argument for treating it as structural.

The half worth stealing is the method, and it was cheap: **run the same
instrument on the same box in the same profile against main's tip and against
the branch.** One control run turned "something has grown by 2.4× in two and a
half weeks, cause unknown" into "The Roll moved it 21× and The Rack recovered
4.6× of that" — and turned a re-pin that Task 5 correctly refused to make into
a reviewed act with an attribution attached.

## Task 5 declined to do the task, and was right

The brief asked for a downward re-pin following the fold removal. The reading
came back **above** the ceiling, which is not what a downward re-pin looks
like, and moving the basis alone would have set `basis > budget` — the
permanently-red inverted state another constant's doc in the same file warns
about. So Task 5 moved the one pin it could, wrote the finding into the
constant's own doc at length, and asked for a control run before anyone raised
anything.

That is the behaviour a brief should be able to survive, and it is worth
naming because the alternative — moving the number to make the arithmetic work
— would have produced a green file, a plausible commit message, and a gate
that meant even less than the one it replaced.

## Three plan-prescribed mutations were nulls, and one prescribed design was wrong

All four failures have the same root, and it is not "written from outside the
code": **seed 42's flagship population never changes room.** Probed at spans
of 1, 5, 30, 100 and 365 days, zero `agent-at` facts across all sixty-eight
bodies. The settlement condenses onto fresh water, so its residents drink
where they stand.

- **Task 2, `at_turn_zero_every_slot_stands_at_home`.** The prescribed
  mutation was to seed `position` from `resource` instead of `home`. At seed
  42 every one of the sixty-eight bodies has `home == resource`, so the test
  stayed green. The implementer probed it, said so in the test's doc, and
  found a replacement that reds.
- **Task 3, P3's write-back deletion.** Delete the tick's position write and a
  moving population's column goes stale — except nothing moves at seed 42, so
  the mutation was null. P3 now runs seed 42 for the driven writer and seed 7,
  where seventy-four of a hundred and two bodies leave home within thirty
  days, for the tick's, each half asserted non-vacuous.
- **Task 4, two prescribed mutations replaced.** One did not compile after the
  memos were deleted; the other named a symbol a refactor had removed. Both
  replaced with compiling edits and the observed reds recorded rather than the
  predicted ones — three of six predictions were wrong in detail.
- **Task 4, the sighting memo — a prescribed *design*, not a mutation.** The
  brief specified a per-turn memo cleared at the top of `handle`. That is
  wrong, because `wait` reads the sighting on both sides of its own tick, and
  a per-turn memo makes both reads agree, silently deleting every chamber
  arrival and departure narration. **No test in the crate covers that, and
  seed 42 would never have shown it.** The implementer keyed the memo instead,
  on everything a sighting reads.

The Roll's retrospective already recorded five prescribed mutations that
proved nothing, at this same seed, for this same reason. This campaign
produced four more. **The lesson has now been learned twice and the practice
has not changed, which means the fix is not a longer brief.** The candidate
that would actually bite: a brief that prescribes a mutation over a population
must name the seed *and* state what that seed's population does — and if the
author cannot state it, that is the signal to make measuring it the first step
of the task.

## An invariant test is only as wide as the writers it exercises

Task 3's VIEW ≡ SCAN battery ran five verb classes across two seeds and passed.
The reviewer found the defect it could not see: `wait` writes the driven slot's
position from the driven body's solo walk while discarding that walk's facts,
so under an **imposed controller** — which is what possession is — the walk can
move the body and the ledger never records it. The battery's script never
possessed.

Verbs are the axis a test author naturally enumerates because they are what the
player types. Controllers are the axis the *writer* varies. Reproduced at seed
7 after `!possess` then `!wait 5`, fixed at the writer, and pinned by a
possessed-session test that is red before and green after.

## Absorb main before the DoD artifacts, twice over

Two absorbs, and they failed in two different ways — which is the argument for
doing them at boundaries rather than at close.

- **Stage 2 was refused at the mouth**, in milliseconds, because
  `docs/audits/type-audit-report.md` conflicted. That file is an **aggregate**:
  it is regenerated, never text-merged. Absorbing, regenerating and resubmitting
  cost minutes; discovering it at close would have cost the merge.
- **The Pawl's absorption merged `session.rs` cleanly** — four hunks, no
  conflict, the fold store threaded through by the compiler — and produced a
  **duplicate registry row**. Both campaigns had edited
  `UNI-ecs-is-the-adaptive-cache`; git kept both lines; `registry_ids_are_unique`
  went red. This is the **third recorded instance** of a hand-edited table
  auto-merging clean and wrong, and it is worth separating from the aggregate
  case: the aggregate habit ("regenerate, never text-merge") does not fire for a
  file a human types into. The gate caught it. A diff review would not have.

The semantic half went the other way for once: The Pawl's chronicle argues the
seam is at *read* and The Rack's argues it is at *write*, which sounds like a
collision and is not — The Rack retires the path The Pawl was protecting rather
than reopening it. That was checked by reading The Pawl's chronicle, not its
diff, which is the standing advice and the reason it exists.

## A controller committed in a worktree an implementer was working in

Three bookkeeping commits (a timings row, two ledger entries) landed in the
campaign worktree while the Task 5 implementer was mid-edit, and something in
that sequence reset the working tree: the implementer's uncommitted edits to
two files were silently wiped. It caught this with `git status`, redid the
edits, and committed promptly the second time — at the cost of real minutes and
a report paragraph.

**The standing memory rule covers `git add -A` while a subagent works. This is
the same hazard by another route**, and scoping the `git add` to one path did
not help, because the destruction was not in the staging. The rule generalises:
a controller does not write to a worktree an implementer holds, for any reason,
including its own ledger.

## The brief assumed a passing test's numbers were in the log

Task 5's brief said to read the pooled median off the heavy-tier run.
`session_cost::a_possessed_turn_stays_within_its_ceilings` **passed** in that
run — and nextest captures a passing test's stdout, so the numbers were nowhere.
The reading had to be retaken by running that one test with `--nocapture` in
lefford's heavy worktree once the queue went idle.

Cheap to recover from, worth recording because the assumption is natural and
wrong in exactly one direction: **a green expensive run emits less evidence
than a red one.** If a number is the deliverable, the run that produces it has
to be told to print it.

## Deferred minors, and what became of each

| carried | outcome |
| --- | --- |
| Task 2 → Task 3: does the felt-seeding read get counted? | Resolved in Task 4, in the affirmative — `affect_folds` is bumped by `seed_felts`, which is the one surviving stateless read on the session path. |
| Task 2 → Task 6: restore the `decision 0596` cite on `Felt`'s doc | Done, in the commit after 0596 landed. The cite could not exist earlier: `docs_consistency::decision_cites_in_sources_resolve` refuses a cite to a record that does not exist, correctly, and caught it on its first commit. |
| Task 2 → Task 3: `push` should push `on_roll` too | Done. Unreachable on the call graph as it stood, but Task 3 added a tick writer to the same file and must not inherit the gap. |
| Task 2 → Task 3: one band for felt seeding | Done — both seeding sites now seed against the roster as it stands after the pushes. |
| Task 3 → Task 4: what does a never-walked body render? | Ruled at dispatch and shipped: `felts()` always answers with the append's seed; `resolved_felt` is the gated accessor behind the `written` flag. |
| Task 3 → Task 4: `place_agent_now` skips the column (parked Concern 5) | Fired immediately. It was the one ledger writer outside the VIEW ≡ SCAN discipline, invisible while nothing read the column, and reddened a test the moment `colocated_npcs` became an array scan. Fixed in Task 4. |
| Task 4 → Task 6: `SightingKey`'s fields are not independent | Done — one doc paragraph naming the coupling: the occupancy write counter restarts across a tick, so `day` is what separates the two sides of a `wait`. |
| Task 4 → close: the "67 identical sentences" finding | **Corrected, not carried.** The reviewer compared the pre-change gallery and found it was already 63-of-67 identical. The flatness is pre-existing bucketing; the ruling changed which phrase a crowd shares. Now a registry row, not a campaign finding. |
| Task 3 → close: a possessed body's walk facts are discarded | Parked as pre-existing (since The Coercion) and out of scope. Recorded on the registry row that already covers the imposed-controller divergence. |
| The Pawl absorb → close: `Session.folds`' `RefCell` | Now unexercised by any `&self` reader, since the snapshot no longer folds. Noted, not removed — the store itself is live through the seeding sites. |
| The Pawl absorb → close: its whole-tick figures | One absorption staler than its own limits section admits, and The Rack moved the walk on the axis it measured. Re-measuring is a campaign, not an absorb. |

## Follow-ups, with reasons

- **`TOOL-session-cost-has-no-canonical-basis`** — the vacuity itself. Closing
  it means recalibrating `START_*`, `TURN_*` and `INDOOR_SNAPSHOT_*` together
  against a new basis host (the precedent is `scene_cost.rs`), which is a
  campaign, not a task. Not attempted here on purpose: a partial recalibration
  would leave three constants disagreeing about which machine they describe.
- **The JSON and spatial-channel residue** (~70 KB a turn, a 4.2 ms fold-free
  floor). New registry row. Not attacked here because it is the wire, not the
  loop, and this campaign's whole thesis was the loop.
- **The chamber shadowcast** (~8 ms, the largest single item left). New
  registry row, with the cheap lever named: extend the memo's sharing to `map`
  and a chamber `go`, which needs `chamber_plan` to want the sighting it
  already has. **Corrected by The Terrier:** the ~8 ms was `brief_of`
  re-surveying the world's occupation register per call, not the shadowcast
  (0.012 ms); the cheap lever named here would have saved one of two brief
  calls per snapshot and left the other.
- **`RENDER-felt-phrase-buckets-a-crowd`** — a design question for Nathan, not
  a defect, and explicitly not a Rack finding once the control was checked.
- **Invalidation dispatched from the commit** (Penstock §5.7) stays out of
  scope, and the reason is still the one the spec gave: the rack has exactly
  one writer per column, so it needs no dependency keys. If a second writer
  ever appears, that is the moment the `Ledger` validity class gets its tenant
  — and `place` is already a second writer for `position`, which is worth
  watching.

## The counted budget had the same blind zone it was built to close

The final review found it, one commit before merge, and it is the sharpest
lesson here because it is this campaign's own instrument failing this
campaign's own way.

`TurnWork` is a field on `Session`, so it counts what `session.rs` does. A
walk-band `snapshot` builds the chart through `Session::purview(0)`, and
`purview_scene` — a different module — folded `agent_position` once per NPC to
place its mark. **`a_snapshot_performs_no_folds` read 0 while 67 folds ran.**
The campaign then wrote "no turn path performs one" in three places, all of
them true of `session.rs` and none of them true of a turn.

Three things follow.

- **A counted budget bounds the module it is threaded through, never a verb.**
  The unit of a counter is not the thing the counter is named after.
- **The fold was deleted, not instrumented.** Routing a counter into `purview`
  would have asserted the absence of the thing it was added to measure — the
  permanently-green zero this campaign had already argued against, one module
  over.
- **The replacement had to be a behavioural test, and writing it exposed a
  second hole.** The obvious mutation (draw the mark from `home` instead) was
  run against the whole vessel crate first: **626 lib + 399 integration tests,
  all green.** Nothing anywhere asserted that a creature's chart mark follows
  the creature, because every chart test is at seed 42, where nobody leaves
  home. The deleted fold could have been returning `home` all along. The new
  test runs at seed 7 and reds on that mutation with a real message.

That is the seed-42 lesson for the third time in one campaign and the eighth
across two — this time not costing a mutation but hiding a live defect for the
campaign's whole length.

## Four smaller things, each recorded because nothing else would keep them

- **`lexicon_guard` counts `Cell` the type as a bare `cell` token**, and
  `cargo fmt` can wrap a same-line `lexicon:` waiver off its line, silently
  un-waiving it. Task 1 dodged it by keeping `Cell` out of every signature
  behind small `bump_*` methods rather than fighting the ratchet. Worth
  knowing before designing an API around an interior-mutable counter.
- **The Task 3 brief under-counted `step_with_occupancy`'s callers, 2 named
  against 4 real.** It listed the two in `windows/lab/src/health.rs`; the grep
  also found two `windows/vessel/examples/`. Examples compile under
  `clippy --all-targets`, so the gate would have refused. Caught at brief
  verification. **The generalisable half: a brief that names call sites should
  say how they were enumerated**, because "the ones I remembered" and "what
  `grep` returns" look identical on the page.
- **A controller ruling was reversed by an implementer on a code-grounded
  reason, and the reversal was right.** I ruled `step_one_with_controller`
  should go `pub` so P4 could live in `tests/suite`. The implementer showed
  that the "same inputs" P4 must compare against are the session's private
  context, calendar and pre-wait ledger — so an external test could only ever
  compare against an approximation, and the visibility change would not have
  fixed that. P4 is an in-module test and the method stayed `pub(crate)`.
  Recorded because the healthy direction is the rare one.
- **The heavy tier went red at `813c74726` on a test outside this campaign.**
  `graph_cost::tumult_predation_bake_stays_within_budget`, 34.1 s against a
  30 s wall-clock budget, on a box at cpu_ratio 24.22 with a chamber job and a
  census queued beside it. Contention-shaped, in a worldgen bake this campaign
  never touched. **Not discharged here** — the merge's own `heavy` phase
  re-runs it under the serial claim, which is the only reading that settles it.

## What went right, briefly

Every implementer that hit a null said so in the test's own doc rather than
quietly substituting. Every mutation red in this campaign is the **observed**
one, not the predicted one, and three of six predictions were wrong in detail —
which is only visible because the practice is to paste the output. The
preregistered budget was reported as **missed** with a decomposition rather
than tuned toward, and the decomposition came out of the pre-change measurement
that was already sitting in the file.
