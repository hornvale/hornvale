# The Tailrace — retrospective

**Merged:** pending. Process lessons only; what shipped is in
[the chronicle](../../book/src/chronicle/the-tailrace.md).

## Nine controller-side errors, and all nine were the same error

None was invented. Each was a **true thing generalised one step too far**, and
each was settled — usually by someone else — by running the check the claim
implied.

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

## Mutation testing found three things review-by-reading did not

Reviewers applied type-checking mutations to the new kernel module and required
the intended test to go red, confirming each hunk applied before trusting a
result and confirming a clean tree after reverting.

1. **A test compared a function against itself.** An off-by-one in the skip
   count reddened one fold-equals-scan test and left its sibling green, because
   `rebuild` is *implemented by calling* `advance_to` — a bug inside
   `advance_to` applies identically to both sides of that comparison and
   cancels. Both sides absorbed facts 1–19, both missed fact 0, and both
   agreed. Not a coverage hole (an independent path caught it) but a naming and
   advice defect, and the advice was the dangerous half: the module doc tells
   every future tenant to use `rebuild` to make its own property test cheap.
2. **The most aggressive chaos schedule is not the most diagnostic.** An
   implementer *self-reported* that discarding state at every position gives
   almost no signal on the per-fact absorb step, because the state is replaced
   immediately after every absorb. It had not tested its own claim. The reviewer
   built the mutation and settled it: the every-position schedule stayed green,
   the every-third-position schedule went red.
3. **The claim "I checked three broken implementations" was reasoning, not
   running.** An implementer reported that three hypothetical bugs would be
   caught. A reviewer was asked to verify by applying a mutation instead, and
   that is the pass that found item 1.

The generalisable rule: on any check whose failure mode is silence, *reasoning
about a mutation is not evidence.* Every one of these was found by making a
green test go red on command.

## Nine instrument defects, one lesson, and the one that is purely process

The measurement side of this campaign produced nine defects and the code side
produced none. They are narrated in
[the chronicle](../../book/src/chronicle/the-tailrace.md) because they are
findings about the world as much as about the process; what belongs here is the
two that are purely about how work was done.

**A mutation must prove it mutated.** One patch carried six edits, five of them
asserting on their target line and one not. The un-asserted one silently did not
apply, and the instrument printed eight column headers over ten columns with
every value one heading to the left — a table that looked fine and was wrong
everywhere. This is the project's own standing rule turned on its author: a
patch that cannot fail loudly is a patch you have not applied.

**"The box is noisy" is the flattering explanation, and it was wrong.** A column
disagreeing about its own sign across runs was diagnosed as contention on a
loaded machine. Contention was real (a band ran 3.5× out of line while the
calibration yardstick moved 19%) and it was not the cause; the cause was
regressing against an x-axis that barely moved. Accepting the flattering
diagnosis would have justified escalating the work to another machine and
changing nothing. Before blaming the apparatus, check that the statistic is
well-conditioned — and note that the yardstick built to detect exactly this
contention could not see it, so its green reading was not evidence either.

The generalisable form, which is also the shape of the other seven: **an
instrument's own conditions are part of its output.** A fit's conditioning, an
exclusion's declaration, a hardcoded constant standing in for a computed one,
and whether a patch applied are all *results*. An instrument that does not
report them will report the ones it does report confidently and wrongly.

## One belief correction worth stating flatly

An implementer attributed an unexplained commit to "the project's own gate/hook
infrastructure … not something I authored by hand." **No hook in this repository
auto-commits** — `git log` named the controller as that commit's author. The
belief matters more than the instance: a session that thinks tooling makes
commits will attribute a mystery commit to tooling instead of finding out who
made it. Corrected in the review chain so the wrong version did not travel.

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

Every followup and parked finding is registered. New idea-registry rows carry
the measurement that motivates them rather than a hunch:

- **`TOOL-golden-pins-guarded-update`** — the guarded `--update` mode for
  `golden-pins.sql`, the defect that cost this campaign a day. The reason no
  *wholesale* regenerator can exist is the point of the row: the pinned literal
  is a human ratification that a moved value is acceptable (~800 of that file's
  1,091 lines are that ratification), so regenerating the literals from the
  computation makes computed == pinned by construction and the tripwire never
  fires again. A guarded mode that refuses to write until each moved pin has a
  reason is the same shape as `make rebaseline-goldens`.
- **`TOOL-penstock-7a-tenant-migration`** — stages 3–5 are now **unblocked**;
  The Escapement landed 2026-08-25. Carries the crossover measurement and the
  `agent_sightings` hub finding.
- **`TOOL-interleaved-depth-sweep-attribution`** — stage 1's unfinished half:
  which of the six folds carries the rest of `k`.
- **`TOOL-calibrate-memory-bandwidth`** — the yardstick normalises CPU speed and
  not memory bandwidth (one band 3.5× out of line on wall time while the
  yardstick moved 19%).
- **`TOOL-derived-bodies-commit-no-position`** — some derived bodies commit no
  `agent-at` fact at all (min 0 across 50 agents over 200 ticks); uninvestigated,
  and the reason a positionally-chosen probe read zero.
- **`TOOL-make-target-runs-main-checkout-code`** — the `make worktree-take`
  lesson above, as a durable row rather than a spec footnote.
- **`TOOL-lexicon-guard-cell-substring-overmatch`** — `lexicon_guard`'s
  `cell_tokens` counts any token containing "cell", so `cancelling`,
  `excellent` and `cellar` each score 1. Demonstrated rather than inferred. The
  over-match is deliberate (a `[Cc]ell` grep missed ~80 ALL-CAPS `CELL_ID`
  occurrences), so this campaign reworded its prose around the guard rather
  than misusing the reasoned-waiver mechanism, which means "this really is an
  area".
- Two existing rows already carried their homes: the per-tick memo clone
  (`TOOL-agent-scaling-memo-clone-per-tick`, inherited unchanged by the new
  instrument and deliberately kept outside its timed span) and
  distinct-rooms-per-tick (`TOOL-scan-at-prefill-or-faster-scan`, which the new
  instrument is now a few lines from reporting).
- **The whole-tick column's x-axis is a mean over a wide spread** (per-agent
  history min 0, max 420 against a mean of 143.9). Recorded in the spec's §4
  limitations rather than as a row, because the decisive column avoids it by
  probing one fixed agent — but if the whole-tick fit is ever load-bearing it
  needs a median or the probe's own history.

**Three of the spec's four candidate decisions were minted, and the fourth
deliberately was not.** 0236 (a fold advances; it is not invalidated), 0237 (the
reset event is the checkpoint), and 0238 (stage 7 is three stages, and their
order is forced) are records. **0238 was minted at close, and the reason it
almost was not is the lesson.** The re-carve was ratified at G3, and it lived in
the spec's §0 table and §9, in the registry rows for all three stages, and in a
postscript on the parent program's own chronicle — four places, none of them
`docs/decisions/`, which is the one place a future campaign greps before
relitigating an architectural question. Being written down in four places that
are not the index is not the same as being recorded. It amends another
document's stage table, which is exactly the shape of thing the decision log
exists to hold. "The trail's
provenance is content" is likewise carried by the 7c row rather than as a
record, which is what §9 itself proposed. If a later campaign wants either as a
citable number, it mints it; nothing is lost in the meantime, but nothing
enforces them either.

**Four deferred minors still stand**, and this list is their home:

1. The corrected reading of `home = room_for(0)` — read once per call at every
   depth, harmless because the terrain is constant (row 7 above). No code change
   warranted; the wrong version must not travel.
2. `fold_depth_sweep.rs`'s table prints only final-band absolute µs/call, not
   first-band. Pre-existing `report_affine` behaviour, not a regression.
3. Spec §4's amortized `hazard_memory_memo` range excludes run 1 while the
   72–96× ratio range includes all four. Stated in the prose, so not an
   inconsistency — but a future re-derivation needs to know which scoping
   applies to which figure.
4. `docs/timings.md`'s row for the docs-only commit `0f207e959` (643.4 s wall /
   1,506.5 s user) is a 4–9× anomaly against the docs-only floor. Worth a glance
   if the duration alarm ever reads it.

Two others were closed by the final fix wave rather than deferred: the
depth-10 doc line now carries its hedge, and the every-position chaos test now
says in a comment that it gives no signal on the per-fact absorb step.

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
