# The Hand — retrospective

*Arc II of The Bridle. Merged `Npc` and `Agent` into one `Body`, turned
possession into a roster index, and made the source of intent a `Controller`
parameter of the tick. Eight tasks, four decision records (0226-0229), one
must-fix from The Deed closed. Process lessons only; the campaign's own account
is [the chronicle](../../book/src/chronicle/the-hand.md).*

## The headline: the same measurement was vacuous twice, for two unrelated reasons

The spec preregistered a null — routing a possessed body through the creature
tick will not move the ledger's growth rate — with the argument that a driven
body holds most ticks and holding commits nothing. It was measured twice and
came back 0.25 facts per body per tick both times, exactly as predicted.

Both readings were vacuous, and **the two vacuities have nothing to do with
each other**:

- **Round 0** measured an implementation that never touched the ledger,
  because the controller it was measuring was not in any tick. The intent it
  computed was assigned to `_`.
- **Round 1** measured a real walk inside the real tick — whose emitted facts
  `Session::wait` discards *unconditionally* before they can reach the ledger.
  Force `intend` to return `Intent::Do(Action::Rest)` and the guarding test is
  still green.

So the number was right, stable, reproducible and meaningless, and the second
time it was meaningless for a reason the first fix had introduced no part of.
Fixing one vacuity is not evidence about the next one; a confirming number that
survives a repair should be *re-interrogated*, not treated as corroborated.

**What was done about it is the part worth transferring: a third measurement
was declined.** The discard is correct — what the player types is what the body
*does*, and the walk supplies only what the host *wants* — so no reachable
experiment can separate "commits on `Do`, nothing on `Hold`" from its negation
in this design. The spec was amended at close to say so, and the smaller true
claim was recorded in its place: *routing a possessed body through the tick
costs no committed facts.* Staging measurements until one agrees is how a
preregistration is laundered into a confirmation. The honest move when the
instrument cannot discriminate is to write down that it cannot.

## Three fix rounds, and each round's defect was the previous one in thinner form

Task 5 is the arc's centrepiece and it took three rounds. The rounds are worth
reading as a sequence rather than as three separate misses:

| round | what shipped | what was wrong |
|---|---|---|
| 0 | a `Controller` trait | nothing dispatched through it; the acceptance test passed before any production code existed |
| 1 | a real driven walk inside the real tick | no assertion observed it — substituting the whole walk for `self.driven_mode = Some(Mode::Idle)` left **611 tests green** |
| 2 | assertions on the walk's content | a five-line literal reading only `self.day` still satisfied them |

The defect never changed identity. It thinned. Each round moved the vacuity one
layer inward — from *the mechanism is absent* to *the mechanism runs and nobody
watches* to *somebody watches something a constant can produce* — and at each
layer the code looked more convincing than at the last.

**Not one of the three was found by reading.** Every one was found by handing a
reviewer a *specific mutation to run*: replace this call with a constant,
substitute this whole walk, reintroduce the swap. The verdicts were red, green,
green, and the green ones were the findings.

## Two of the campaign's defects were in the controller's instructions, not in implementer code

This repeats the shape The Deed measured, and adds a sharper version of it: in
both cases the *observation* was sound and the *remedy* was reasoning that had
never been run against the code.

**The trait signature could not carry a decision.** The brief specified
`Controller::intend(body, view, mode)`. That is enough for the thirst-only
computation the drive layer started life with, and nowhere near enough for
`decide_step`, which takes twenty threaded parameters — terrain, hazards, alarm
state, visited and frozen sets, a mesh memo, a home-navigation cache. A trait
that narrow can only ever wrap a *different, simpler* computation than the one
that actually drives every creature, which is precisely what round 0 built. The
implementer's diagnosis of this was correct and it was my defect.

**The remedy for round 2 introduced a latent bug.** I instructed that
`catch_up` be routed through the controller. `PlayerController::intend` is
`self.pending.take()`, and `catch_up` runs *before* `advance_one` — so the
moment a verb queues an action, the catch-up pass takes it, hits
`Intent::Do(_) => break`, and discards it; `advance_one` then sees `None` and
holds. **The player's action vanishes silently.** It is dormant only because
`queue()` has no callers yet. The fix is a fresh controller for the catch-up
call.

That was the third time in the campaign that a *remedy* rather than an
observation was the wrong half, and the second where the remedy was mine. A
correction is unaudited text, and a correction that reads as obviously
right — *of course the catch-up pass should ask the controller* — is the kind
nobody re-derives against a stateful implementation.

## A reviewer ran a positive control on its own instrument, and the instrument had none

Checking whether the NPCs still walked identically after the controller landed,
a reviewer compared the world JSON and the rendered snapshots — the obvious
artifacts, and the ones the project reaches for by habit.

Then it ran the control: forced *every* catch-up intent to `Hold` and compared
again. **Byte-identical.** The comparison it was about to trust would have
certified any answer at all.

What discriminates is the decision stream itself, instrumented at the site. It
found 8 differences across 224 decisions in 4 seeds, and every one of the eight
was the possessed body's own — which is the actual claim the arc needed, proved
by an instrument shown able to fail. **A byte-identity check over a downstream
artifact is not automatically a control over an upstream mechanism**, and the
cheapest way to learn which it is remains breaking the mechanism on purpose and
watching whether the artifact notices.

## A correction fixed one site of a commit-wide defect

A Python heredoc had eaten backslash continuations in `session.rs`. It was
found, attributed correctly to this campaign's own unmerged commit (the report
had called it pre-existing; the reviewer re-derived the provenance and it was
not), and fixed — at that one line.

**Ten more instances from the same commit sat in another file.** Nothing
grepped for siblings. This is the same shape as the campaign's other correction
misses and it has a one-line remedy: when a defect is traced to a *commit*,
grep the whole commit for the pattern before declaring the fix complete. The
provenance re-derivation is what made the sibling search possible and nobody
ran it.

## The implementer's honesty is what made round 0 catchable at all

Round 0's report said, plainly and unprompted, that its acceptance test passed
*before* any production code existed, and that its fact-rate null was
"trivially true". Every finding raised against that round is quoted from the
implementer's own report.

A report that volunteers its own weakness is worth more than one that looks
clean, and it is worth saying so out loud in a retrospective, because the
incentive runs the other way. Nothing in the process rewards a self-incriminating
report; the only thing that can is a controller noticing and saying so.

## Deleting a verified duplicate revealed a dependency nobody had named

Task 2 asserted, field by field, that the flagship body and derived creature #0
were the same villager. That equality was real, and it licensed Task 3's
deletion of one of them.

The deletion was still a loss, because the duplicate was load-bearing on an
axis nobody had measured: it was the *only* thing guaranteeing that a fresh
possession started beside another creature. A chamber-creature seed search that
hit ~19 of 24 seeds hit **0 of 64** afterwards, and twenty-one tests had been
passing on the artifact rather than on world behaviour.

**An equality proof licenses a deletion only on the axis it measured.** The two
objects were identical as *data* and not interchangeable as *population*. The
campaign stopped, brought the fidelity question to Nathan rather than repairing
it inside a refactor, and the ruling was to add a documented test seam now and
give the population question its own campaign — because the fix multiplies
agent count, and The Penstock measured tick cost superlinear in exactly that
dimension (2.17 across 100→200 agents). It is recorded as
`SOC-one-creature-per-settlement`, carrying its numbers.

## Process notes

- **The branch never absorbed main.** The campaign ran inside one day and main
  moved 81 commits in that window; no stage gate was submitted at any task
  boundary, so the merge is the branch's first meeting with main. That is the
  cadence CLAUDE.md asks for, missed — recorded here per the closing skill
  rather than discovered at the merge.
- **The absorption is what found a broken build, and no gate could have.**
  This campaign deleted `Session::agent` when `Npc` and `Agent` merged.
  `clients/game/bin` called it — one site at the branch tip, five after main
  landed Portolan II's four more — and **nothing in the project compiles that
  crate**. It depends on `hornvale-vessel` *by path* while sitting outside the
  cargo workspace, so `cargo check --workspace`, `gate-commit` and a full
  `nextest run --workspace` all build vessel and never build its consumer.
  With no CI since decision 0125, `make game-check` is the only thing that
  builds it, and **no local rung runs that** — `gate-commit` and `make quick`
  are fmt, `clippy --workspace`, type-audit and the sub-floor tier, none of
  which reach outside the workspace.

  > **Corrected before merge, and the correction is the more useful half.** A
  > first draft of this entry said the break "would have reached main" and
  > that `make game-check` "is in no gate rung". Both are false, and the merge
  > queue's operator refused them with file:line rather than agreeing:
  > `scripts/lane-sets.tsv:44` makes `clients` a lane set,
  > `sluice-run.sh:360-361` puts it in **both** `merge_phases` and
  > `stage_phases`, and `Makefile:876-879`'s `clients-check-run` fans out to
  > `game-check-run`. **The chamber builds and tests `clients/game/bin` on
  > every merge and every stage**, so this break would have reddened there
  > even unfixed. Verified here rather than accepted.
  >
  > The real finding is narrower and survives: the gap is **local feedback
  > latency**, not coverage. You can break that client and stay green through
  > every check you would plausibly run before pushing, and learn about it
  > minutes into a chamber run. One genuine hole remains — a **prose-only**
  > candidate skips the `clients` phase entirely (`sluice-run.sh:394-395`),
  > which is correct on its face but means the phase is not universal.
  >
  > Recording the distinction because the strong version is actively harmful:
  > someone reading "invisible to every phase" would go add a gate rung the
  > chamber already covers. This is the campaign's own recurring failure in
  > its last available form — a true local observation over-generalised into a
  > false global one, and caught only because someone ran the commands.

  The general shape is worth more than the instance: **a green gate says
  nothing about a consumer the gate does not compile.** Decision 0125 named
  three coverage gaps at ratification and The Staff closed the `clients/atlas`
  one; this is the same gap in a different client, and it stayed open because
  nobody had removed a public API that a path-dependent client used. Recorded
  as F-H8.
- **A red run wrote a green-looking row.** The failing `game-check` recorded
  itself in `docs/timings.md` indistinguishably from a passing one, because
  the ledger has no exit-code column. The row was dropped by hand. Second
  witness for a gap already in memory (F-H9).
- **Two commit messages had to be rewritten** because backticks in a
  `git commit` heredoc execute: command output and a stray `EOF` were spliced
  into a permanent record. Free to fix only because the branch was unpushed.
  The rule is the one already in memory — name identifiers bare in commit
  messages.
- **`make rebaseline` then `make rebaseline-goldens`, in that order**, was
  added to the plan as a global constraint *before* Task 3 rather than
  discovered during it, taken from a board `technique` post. The vessel
  session fixtures are outside the drift check, so the first command alone
  leaves them stale and the gate reds later. This is the second campaign to
  hit it and the first not to.

## The Confidence Gradient: no bet moved, and here is why

Grepped `open-questions.md` for possession, occupancy, creature and player
before concluding it. The chapter's bets are about *world* properties — lazy
retrospective generation, coarse-constrains-fine, refinement at scale, emergent
economics, historiography — and this arc changed no world quantity: genesis is
byte-identical, the census never starts a `Session`, and the driven body's
facts are discarded rather than committed. The nearest live thread is The
Deed's correction to the lazy-generation bet (the player *does* write to the
ledger now), and this arc adds nothing to it, because a driven body's own walk
writes nothing.

What the chapter did gain is a paragraph in its measurement-discipline thread,
beside The Threshold, The Millrace and The Mire: the case where the instrument
is structurally blind and the correct response is to record a smaller claim
rather than to measure a third time. That is an addition to the chapter's
accounting method, not a re-score of a bet, and it is filed as such.

## Follow-ups

**Read the observation and the remedy as separate objects** — the observations
below were measured, the remedies mostly were not. The Deed's own register had
a carefully written, reviewed followup whose *recommendation* was wrong, caught
only when someone tried to implement it.

- **F-H1 · the cross-seed driven-mode pin is stable but not robust.**
  `a_driven_bodys_early_mode_depends_on_which_seeds_population_not_merely_elapsed_time`
  asserts seeds 42 and 13 differ. Probed and *not* knife-edge on drive tuning
  (`THERMAL_ACT` 0.5→0.7, `FATIGUE_RISE` 0.3→1.0) nor on checkpoint (`!wait`
  1..8 all differ). But a 20-seed sweep at `!wait 1` yields only 3 of `Mode`'s
  8 reachable values — Fatigue 12/20, Idle 6/20, Thermal 2/20 — so a *randomly
  redrawn* pair coincides at p ≈ 0.46, and seed 13 sits in the rarest bucket.
  Worldgen churn can redden it spuriously, and its failure message would then
  misdescribe the cause. *Remedy (reasoned, not run): assert ≥2 distinct modes
  across 3+ seeds rather than `assert_ne!` on a chosen pair.*
- **F-H2 · this arc's two behavioural additions are unpinned.** Measured twice,
  by two reviewers: deleting `catch_up`'s call in `step_one_with_controller`,
  or bypassing the controller routing inside `catch_up`, each leaves the whole
  crate green. Parked deliberately — a real pin needs a fixture engineered
  around hysteresis carry-over at a drive-switch boundary. The risk is a silent
  future refactor, not a live wrong answer.
- **F-H3 · `hold_step`'s day-jump is thirst-specific.** A `PlayerController`-
  forced `Hold` can fire the jump even when a different drive was truly active,
  pacing the driven body's re-arbitration unevenly inside a long wait. Verified
  not to affect anything committed (`out` is local and discarded; the
  `HomeNavCache` is keyed by `EntityId` and the driven entity is excluded from
  `other_bodies`). No remedy proposed.
- **F-H4 · the driven body's arbitration is band-of-one by design.** It cannot
  sense ambient population fear or pooled belief, so that a possessed body's
  history cannot leak into another creature's shared field. A deliberate
  scoping choice, recorded so a future reader does not read it as a bug.
- **F-H5 · a session-level guard that cannot be mutation-proven on seed 42.**
  `the_driven_walks_own_facts_never_reach_the_ledger_while_the_player_says_nothing` is a real regression guard
  and structurally cannot discriminate, for the reason the headline section
  gives. Its doc comment says so. **Do not "strengthen" it without first
  reading why the discard is correct** — the obvious strengthening reverses a
  deliberate design choice.
- **F-H6 · discharged.** The spec correction owed at close is written: risk 2
  of `docs/superpowers/specs/2026-08-23-the-hand-design.md` now carries the
  amendment, and decision 0226 states the same limit in its consequences.
- **F-H7 · `plan.rs:821` carries an eaten backslash continuation from
  `72784fea1`, which is on `origin/main`.** Genuinely pre-existing and out of
  this campaign's scope. A candidate for the board lane.
- **F-H8 · co-presence is pinned by tests and invisible to a player.**
  `Session::driven_mode()`'s only caller anywhere is
  `windows/vessel/tests/suite/controller_swap.rs`. The retained mode is real
  and asserted — that is what makes 0226 a shipped claim rather than an
  intention — but no CLI verb, no client, and no prose reads it, so nothing a
  player can do surfaces the host's inner state. *Observation only; the remedy
  is `PLAY-host-is-a-narrator`'s and is deliberately not proposed here.*
- **F-H9 · `Body` carries the same datum by two routes, and nothing asserts
  the round trip.** `Body.activity` comes from `species_activity`, a read of
  the committed `species-activity-cycle` fact; `Body.perception.activity` comes
  straight from `perception_registry()`. They lived on *different types* before
  0229 put them on one struct, so nobody had ever had to look at them side by
  side.
  **The review that raised this called them "two independent sources", and
  they are not** — worth recording, because the wrong version is the alarming
  one. `windows/worldgen` commits that fact *from* `perception.activity` at
  genesis, so the registry is the single upstream author and the ledger read is
  a round trip through it. They therefore agree by construction today, and the
  two fallbacks coincide as well: a missing fact yields `Diurnal` and an
  unresolved species yields `PerceptionVector::MANIKIN`, whose `activity` is
  also `Diurnal`.
  What survives as a real observation is narrower and still worth a followup:
  nothing asserts the round trip, so a future writer of either path can
  diverge silently. And that coincidence is not hypothetical — Task 3's fix
  round 1 *deleted* an assertion comparing the two fields precisely because it
  was mutation-vacuous (`windows/vessel/tests/suite/body_fields.rs` says so:
  replacing the whole perception resolution with `MANIKIN` still passed,
  because seed 42's hobgoblins are `Diurnal` too). The replacement asserts
  against the authored registry directly, which is the right assertion for
  *perception* and is not a round-trip check. *No remedy proposed: a cheap one
  would compare the two fields and be vacuous for the reason just given, so
  the honest version asserts the committed fact against the registry vector
  the genesis path wrote it from.*
- **F-H10 · `windows/lab/src/synthetic.rs` hardcodes an inline copy of
  `PerceptionVector::MANIKIN`** — `{ Diurnal, 0.5, 0.5 }` written out rather
  than the named constant, so a change to the constant silently stops being
  reflected in synthetic scenarios. Measured, trivial, and left alone because
  this wave changed no behaviour.
- **F-H11 · a test name that says `mints` after 0227 says selection.**
  `the_most_populous_target_mints_at_a_different_settlement_than_flagship`
  (`clients/game/bin/tests/driver.rs`) survives the rename to selection.
  Its doc comment is corrected; the *name* is not, because a rename is a code
  change and this wave was prose. **The first draft of this followup gave the
  wrong reason for that** — it cited the commit-gate hazard, that
  `docs/timings/subfloor-roster.tsv` selects by exact name and a stale id
  drops tests silently. That hazard does not apply here: `clients/game/bin` is
  in root `Cargo.toml`'s `exclude` list, so the roster contains no `game`
  entry at all and only `make game-check` ever runs this test. Worth recording
  because the wrong reason was the *more* cautious one and would have made the
  rename look expensive. *Remedy (reasoned, not run): rename it, and run
  `make game-check`, which is the only thing that names it.*

**Deferred minors, each with a home rather than a memory:**

- `the_objective_wait_narrates_an_arrival_the_body_could_not_see` stays
  `#[ignore]`d with a verified architectural reason: it needs a
  `before=false`/`after=true` transition that only a drive tick's own mid-call
  commit produces, which no position-only seam can inject. Closing it needs a
  tick-time seam, which is a design decision rather than a fixture.
- `mint_at`'s double `species_of` call is gone with `mint_at`; the three
  species registries now build once per settlement inside `derive_npcs`'
  loop rather than once per batch (~15-entry literal lists, session-start
  only, never per tick — assessed negligible and left alone).
- The lowercase `npc`/`npcs` field and function names were deliberately kept
  through the `Npc`→`Body` rename. The one site with semantic risk
  (`DriveMovements.npcs`) was checked and is accurate by construction, since
  `other_bodies` excludes the driven body.
