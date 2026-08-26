# The Tailrace — retrospective

Process lessons only; what shipped is in
[the chronicle](../../book/src/chronicle/the-tailrace.md). Stages 1-2 of
Penstock stage 7; stages 3-5 deferred with homes (below).

## Ten controller-side errors, and all ten were the same error

None was invented. Each was a **true thing generalised one step too far**, and
each was settled — usually by someone else — by running the check the claim
implied. **Row 10 was added by the pre-merge review of this very file**, which
is the table's own thesis arriving on schedule: nine of these were caught during
the campaign, and the tenth had already reached a ratified decision record and
two published chapters before anyone read the function it described.

| # | the claim | what was actually true |
|---|---|---|
| 1 | `Body` is private to its crate | `hornvale_vessel::liveness::Body` is private. `body::Body` is `pub`. A compiler error about a *path* was read as a fact about a *type* |
| 2 | agents drink regularly, so `S` is bounded in production | true of the drive model, never checked for *this* agent. The probe agent drank zero times in 200 ticks, in all four runs |
| 3 | the probe is a fair sample | the probe is chosen as the max-history roster member — by construction the one still walking, therefore the one that never arrives and never drinks. A selection effect I introduced myself |
| 4 | the interpolated crossover is `H ≈ 190` under both tables | 146 and 189, a ~30% gap. Relayed from a reviewer's report with both tables in hand and a minute's arithmetic available |
| 5 | `git diff <merge-base> HEAD -- kernel/src/ledger.rs` proves `Ledger` unmodified | wrong instrument for a branch that absorbs `main` five times. The diff is non-empty from a commit already on `main`; `git diff origin/main HEAD` is empty, and the claim held |
| 6 | the "identical at depth 10" doc line is already hedged | the hedge did not exist in the file |
| 7 | `home = room_for(0)` is never read at depth ≥ 10 | it is read once per call at every depth — the first segment starts at `s = 0.0` and no posting precedes it. Harmless only because the terrain is constant |
| 8 | a 643 s timings row is a mild outlier against kernel-layer rows | that commit is docs-only, so the comparison is the docs-only floor: a 4–9× anomaly |
| 9 | the spec's citations were re-dated | the commit carrying that edit was refused, and the `git merge --abort` that followed discarded the working-tree edit with it. Reported as done |
| 10 | six folds walk the `agent-at` trail | **five** do. `fatigue_at` folds `rested` only, so abstaining from `agent-at` would not move it by one bit — and `shared_believed_water`, which walks the trail once per co-located peer, was missing from the enumeration instead. The campaign's own §2 table and §4 attribution had this right ("O(1) — a max"; "the one fold that is cleanly NOT" history-scaling); §0's headline generalised *fatigue is one of the six folds we timed* into *therefore it walks the trail*, and that spread to six files including a ratified decision record. Caught by a reviewer reading the function, at the last possible moment |

Two lessons, and the first is not "be more careful".

**The relay is a measurement site.** A number passing through the controller
needs the same derivation check the controller demands of an implementer. Error
4 is the sharpest instance because it was *worse* than the thing it fixed: a
reviewer had correctly flagged a fragile bracket, and the fix substituted a
different fragile number, presented as verified, which the implementer then
committed in good faith. Handing over reasoning is right; handing over a verdict
is what produced this.

**Work reported complete inside a transaction that later aborts is not
complete, and an abort is silent about what it took with it.** Error 9 was
caught only because the next absorption's re-date asserted that the old SHA was
not already current, and found a SHA nobody expected. Anything reported between
a merge and its commit needs re-verifying after an abort.

One process error of a different kind, recorded because it is the kind that
repeats: a review dispatch contained the words *"do not flag the two-regime
structure as scope creep."* That is pre-judging a finding, which the process
explicitly forbids. Retracted by a follow-up telling the reviewer to flag it
normally and that I would adjudicate against my own ruling — and it then
confirmed, unprompted, that it would have approved the split on its own
judgment. Give a reviewer the reasoning, never the verdict.

## A hold-off blocks *stages*, not campaigns, and asking per-stage is what shipped this

The most reusable thing this campaign produced, and it was nearly lost: it lived
only in the git-ignored scratch, because it is a *method* finding rather than a
measurement and the sweep looks for measurements.

Ledger entry #1 read a concurrent campaign's `hold-off` as a campaign-level
verdict and concluded **"spec now, implement after The Escapement lands."** Entry
#10 overturned it by asking the same question **per stage** instead: stage 2 —
the core deliverable, the kernel primitive — turned out enterable *immediately*,
on a conflict risk of two registration lines. Nothing about the hold-off had
changed; the unit of the question had.

**A hold-off is a claim about a region of the tree, and a campaign is not the
unit that touches a region — a stage is.** Reading it at campaign granularity
takes the maximum over every stage's risk and blocks on the worst one, which is
how a two-line risk in one stage blocks a kernel-layer stage that never goes
near it. The spec carries only the result, so the re-framing itself would have
evaporated; it is why this campaign shipped anything at all rather than a spec.

Related, and one line because it is a data point rather than a lesson: **the
ideonomy pass went two-for-two on premises this campaign** — it overturned
*which campaign this was* (#2) and *its sequencing* (#10), and separately added
the synthetic depth sweep (#11). Whether the pass moves premises rather than
answers is a standing open question, and a two-for-two run is evidence on it.

## Mutation testing: the rule, not the narration

Mutation testing found three things review-by-reading did not — a test
comparing a function against itself, a chaos schedule too aggressive to be
diagnostic, and a "I checked three broken implementations" claim that turned out
to be reasoning rather than running. The chronicle's *"The tests were checked by
breaking the code"* narrates all three and narrates them better; what belongs
here is the rule they share.

**On any check whose failure mode is silence, reasoning about a mutation is not
evidence.** Every one of those three was found by making a green test go red on
command, and two of the three were *self-reported* as verified by the
implementer who had only reasoned about them.

## Nine instrument defects, and the three rules worth keeping

The measurement side produced nine defects and the code side produced none.
[The chronicle](../../book/src/chronicle/the-tailrace.md) narrates them, because
they are findings about the world as much as about the process. Three rules are
what belong here.

- **A patch must prove it applied.** One patch carried six edits, five asserting
  on their target line and one not; the un-asserted one silently did not apply
  and the instrument printed a table that looked fine and was wrong everywhere.
  The project's own standing rule, turned on its author.
- **"The box is noisy" is the flattering explanation, and it was wrong.** A
  column disagreeing about its own sign was diagnosed as contention; contention
  was real and was not the cause — the cause was regressing against an x-axis
  that barely moved. Accepting it would have justified escalating to another
  machine and changing nothing. **Before blaming the apparatus, check the
  statistic is well-conditioned** — and note the yardstick built to detect this
  exact contention could not see it, so its green reading was not evidence
  either.
- **No hook in this repository auto-commits.** An implementer attributed an
  unexplained commit to "the project's own gate/hook infrastructure"; `git log`
  named the controller. A session that believes tooling makes commits will
  attribute a mystery commit to tooling instead of finding out who made it.

The generalisable form, and the shape of the other seven: **an instrument's own
conditions are part of its output.** A fit's conditioning, an exclusion's
declaration, a hardcoded constant standing in for a computed one, and whether a
patch applied are all *results*. An instrument that does not report them will
report the ones it does report confidently and wrongly.

## Five absorptions of `main`, and what a clean merge does not mean

`git merge-tree` reported **zero conflicts** before every absorption. It was
misleading in both directions, repeatedly.

- **Loud.** Two absorptions renamed types this branch used (`RoomAddr` →
  `Facet`, `MetabolicClass` → `ThermalStrategy`). No textual conflict, because
  `main` never touched this branch's files and this branch never touched its
  renamed lines — and the merge did not compile. Three compile rounds on the
  second one.
- **Silent, twice.** The idea registry merged *wrong* without conflicting: six
  rows where three belong, keeping both the re-scoped versions and the
  pre-campaign originals. The mechanism, understood only on the second
  occurrence: **a row replaced in place reads to git as delete+add**, so any
  branch still carrying the original re-introduces it on merge. It recurs on
  every absorption until the branch lands. Caught by
  `docs_consistency::registry_ids_are_unique`, never by the merge.
- **Silent, and nearly a real loss.** Taking `main`'s side of the generated
  decision index dropped **this campaign's own decisions 0236 and 0237** from
  the committed artifact. `make rebaseline` restored them; the type-audit report
  had moved four lines the same way. A hand-resolution trusting either side
  would have silently lost two decision records. **Never hand-resolve a
  generated file — regenerate it.** This is that rule's first concrete
  near-miss rather than a hypothetical.
- **And the thing rebaseline does not cover.** `docs/timings.md`'s sub-floor
  roster is not regenerated by `make rebaseline`, so a bad merge there drops
  tests from the commit gate with nothing ever going red. Verified
  byte-identical at 3,344 rows on all three sides.

## A red `main` blocked the campaign for a day, and the reason it was invisible is the finding

`main` was red on `census-check` from its own commit: three calibration files
re-pinned to a new census, with the SQL tripwire left at the old literals — 28
of 45 pins disagreeing. Verified not ours (the SQL was byte-identical between
`main` and this branch).

**Why it was a hard block rather than something to route around**, in three
verified links: the sluice's mouth refuses on conflict, so absorbing locally is
mandatory before submitting; absorbing stages the file that is the pre-commit
hook's `census-check` trigger; and bypassing a commit hook is forbidden without
exception. So the drift sat on the critical path even though none of the
chamber's four phases runs `census-check` at all.

**Why nothing caught it earlier:** `census-check` runs only when its trigger
file is staged, `gate-commit` excludes it, and there is no CI. A gate that fires
only on a staged path is not a gate on `main`; it is a gate on the next person
to touch that path.

The right call was to wait rather than fix it: another campaign was already
editing that file, so 28 provenance lines and 56 literal edits would have
manufactured a conflict in the very file that caused the stall, to solve our own
problem. Cost was **measured rather than guessed** when asked (1,091 lines, 797
of them comment, 28 pins × 3 edits, two 2.5-minute check runs — ~20–25 min).

## Line-number citations in a durable spec rot by default

Three absorptions, three rots: uniform +1 after one, between 1 and 61 lines
after the next. One citation landed on unrelated comment text, and it had
already been wrong after the *first* absorption — so the first sweep was itself
incomplete.

The remedy adopted is not "check them more often": the spec's citation block now
carries an **as-of SHA and a date**, plus a note that the function names are the
stable handle. A bare line number beside a 15,000-line file under continuous
churn is the wrong instrument; a dated one is cheap to re-confirm (one grep per
absorption, and the last two cost exactly that) and **cannot silently become
wrong** — a stale one reads as historical rather than as false.

## Two shell traps, both already documented in this repo, both paid for again

- **Backticks in prose written through an unquoted heredoc are
  command-substituted**, and the words are replaced by empty strings while the
  write still returns success. Three identifiers vanished from committed prose.
  The board documents this exact trap for its own `NOTE=` argument; the rule
  generalises: **prose containing backticks goes through a quoted heredoc or a
  file, never an interpolating one.** Caught only by reading the file back.
- **A commit message with prose punctuation wants `git commit -F <file>`**, not
  an inline `-m` string. An implementer hit the heredoc-quoting failure and
  recovered that way; every commit after used a file.

## One instrument finding worth keeping about `make`

`make worktree-take` printed a shell syntax error, and it was **not** a bug on
`main`. That target resolves the worktree pool from the **main checkout**, so it
runs *that* checkout's code at whatever revision it is parked on — here an older
branch where the script does not parse (`bash -n` exits 2 there, 0 on `main`).
The general rule: an error printed by a target that reaches into the main
checkout says nothing about the branch you are working in. Worth chasing rather
than reporting as seen — the first draft of the spec asserted a syntax error on
`main` and would have sent the next reader to fix a bug that does not exist.

## Deferred, with homes

Every followup is registered, and the rows carry their motivating measurement.
**Bare IDs on purpose** — the row says what the idea is, and restating it here
is the duplication that had grown this section to sixty lines. Grep the
registry.

**Ten new rows** (nine `TOOL-*`, one `PROC-*`):
`TOOL-incremental-ledger-fold` (stage 7a, shipped — the primitive);
`TOOL-penstock-7a-tenant-migration` (stages 3-5, unblocked, with the corrected
`agent_sightings` scope); `TOOL-windowed-fact-retention` (the unnamed middle of
the forgetting spectrum, deliberately not taken);
`TOOL-within-fold-cost-attribution-profile` (stage 1's unfinished half);
`TOOL-golden-pins-guarded-update` (the defect that cost this campaign a day —
and why no *wholesale* regenerator can exist);
`TOOL-calibrate-memory-bandwidth`; `TOOL-derived-bodies-commit-no-position`;
`TOOL-make-target-runs-main-checkout-code`;
`TOOL-lexicon-guard-cell-substring-overmatch`; and
`PROC-profiling-task-needs-a-reading-step` (a profile cannot be handed to a
subagent, and the obstacle is the *reading* step, not the `--save-only` flag,
which exists). Two existing rows already carried their homes:
`TOOL-agent-scaling-memo-clone-per-tick` and
`TOOL-scan-at-prefill-or-faster-scan`.

**Nothing checks that a cited registry ID exists, and this very section proved
it.** It shipped citing `TOOL-interleaved-depth-sweep-attribution`, which was
never a row. A plausible ID is indistinguishable from a real one by eye and no
gate reads prose citations, so: **grep every ID a retrospective cites against
the registry before committing it.** That grep also found the list omitting
`TOOL-windowed-fact-retention`, and the close reporting *eight* new rows against
a verified **nine** — counted by diffing the set of row IDs against
`origin/main` rather than by reading a total off either side, because two
independent counting methods gave two different absolutes for the same delta.
A bare list is what invites the grep; the sixty-line version did not.

One finding stayed out of the registry deliberately: the whole-tick column's
x-axis is a mean over a wide spread (per-agent history min 0, max 420, mean
143.9), recorded in the spec's §4 limitations because the decisive column avoids
it by probing one fixed agent.

**Three of the spec's four candidate decisions were minted, and 0238's near-miss
is the lesson.** The stage-7 re-carve was ratified at G3 and lived in the spec's
§0 and §9, in three registry rows, and in a postscript on the parent program's
chronicle — four places, **none of them `docs/decisions/`**, the one place a
future campaign greps before relitigating. Being written down in four places
that are not the index is not the same as being recorded. (The fourth candidate,
"the trail's provenance is content", is carried by the 7c row instead, as §9
itself proposed.)

**Four deferred minors:** the corrected `home = room_for(0)` reading (read once
per call at every depth, harmless only because the terrain is constant — the
wrong version must not travel); `fold_depth_sweep.rs` printing only final-band
µs/call; spec §4's amortized `hazard_memory_memo` range excluding run 1 while
the ratio range includes all four; and `docs/timings.md`'s docs-only commit
`0f207e959` at 643.4 s, a 4-9x anomaly against the docs-only floor. Two others
closed in the final fix waves: the depth-10 doc line now carries its hedge, and
the every-position chaos test now says in a comment that it gives no signal on
the per-fact absorb step.

## What went right, and is worth repeating

- **Pre-dispatch verification found a defect in the plan's own text before
  several tasks ran** — a missing `.0` on a newtype unpack, a `Terrain` trait
  with three required methods rather than nine, a test-local loop variable
  shadowing a helper defined earlier in the same file.
- **An accuracy audit on the decision records paid for itself immediately.**
  Reviewing a ratified record against *code* rather than against the
  implementer's report found two predicate names that do not exist (`ate` and
  `slept`, against the real `eaten` and `rested`) in a record a future
  implementer would read as their specification. The spec's own table already
  had them right, so the record contradicted its own campaign.
- **The neighbour-audit habit, twice.** Asked to fix one eyeballed range, the
  implementer audited the ranges written by the same hand in the same pass and
  found more, both times. An eyeballed range is a habit, not an incident. The
  third sweep came back clean, which is the right answer to report rather than
  to manufacture — and a re-reviewer spot-checked it independently, because a
  clean audit that was not actually run is worse than no audit.
- **A fixer refused to fabricate.** Asked to commit four per-run values for a
  measurement, it searched every report and found none had been recorded, and
  stated the absence in the spec rather than re-running the instrument — which
  would have produced a fifth, non-matching measurement and quietly substituted
  it for the baseline.
