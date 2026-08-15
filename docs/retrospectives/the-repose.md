# Retrospective — The Repose

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-repose.md): a hazard field and a
named mountain, a magnitude law recovered from its own authored input, a
motivating premise wrong in both halves, and an exposure gradient that neither
modelled channel carries **on the shipped roster** — the scope clause is part
of the finding, because one of the two channels is read by 0.4% of the
settlements measured and this instrument cannot acquit it.

This campaign's process story has one spine: **eight assertions that could not
fail.** Five originated in text the controller wrote. What makes it worth a
page is not the count — it is that the last three were caught by their own
authors, which is the direction that has to hold if the count is ever going to
come down.

## 1. Eight assertions that could not fail

An assertion that cannot fail is worse than a missing one. A missing test is a
known gap; a vacuous test is a *claimed* guarantee, and every reader downstream
spends it. Eight were found across eight tasks. They are listed in the order
they were found, with the shape named, because the shapes recur and the
instances do not.

**(1) Task 1 — the empty-fixture red recipe.** The brief's step-2 instruction
for proving the readout test could fail was: write a zero-byte fixture, stub the
producer to return an empty string, observe the red. An empty string equals an
empty file byte-for-byte. The recipe's own red was a trivial pass. Caught by the
implementer running it; repaired with a non-empty sentinel stub against a still
empty fixture, which produces a real content mismatch.

**(2) Task 2 — an identity test comparing a code path against itself.** The
brief drafted the channel-mask no-op check as
`suitability_fields(NONE) == suitability_fields_unmasked(…)`. After the mask was
threaded, the unmasked entry point *delegates* to the masked one with `NONE`.
The two sides are literally the same call. It would have been green whatever the
mask did. Repaired by **independent recomputation**: the test writes the
pre-mask formula out by hand at each of the two application points and compares
on `to_bits()` over every cell of three worlds. Both directions
mutation-proven.

**(2b) Its mirror image, the same round.** The same brief's connectivity
tripwire asserted that a name did *not* appear in a set of files. It already
appeared in one of them. That assertion could never have *passed*. A defect in
plan text has two symmetric failure modes and reviewing for only one of them
finds half.

**(3) Task 3 — a property already guaranteed by the architecture.** The brief
sketched "naming a landform between two person-namings must not disturb the
second". The namer derives a fresh stream from its seed on every call and holds
no interior mutability, so the property holds unconditionally — including if the
new variant's label *collided* with the existing one, which is the only thing
the test was there to catch. The implementer proved the vacuity rather than
arguing it: aliasing the new label to `"person"` left the sketch green. Repaired
with a distinctness property at three salts, plus an exhaustiveness sentinel — a
wildcard-free match that fails to **compile** when a variant is added without
being listed — replacing a hand-listed array that had been silently
under-covering the widening.

**(4) Task 5 — a name with no coverage, and a test that was its sibling's
theorem.** Replacing the per-mountain salt in the naming derivation with a
constant left the nine-test suite 9/9 green: every mountain could have shared
one name per people, undetected. Found by a reviewer. In the same file, a test
whose doc claimed it "can fail on its own" could not — naming is a pure function
of a mountain value, and the sibling test already asserts full structural
equality of that value across the cone, so equal names follow by construction.
It was **deleted, not kept**: a known-vacuous test retained as "redundant but
cheap" is still a claimed guarantee.

**(5) Task 6 — a sub-window test whose windows were round numbers.** The
mutation *filter before drawing the magnitude, instead of after* came back
green. The two designs differ only when a query boundary lands strictly between
two events of one block, and a boundary at 0, 365 250, 730 500 … almost never
does — the chance of catching it across all eight sampled pairs was about 10%,
and it did not fire. Repaired by cutting the windows at **days taken from the
draw itself**, plus a floor on how many such interior cuts the run must contain
so the discriminating case cannot silently vanish. **Caught by its own author** —
the campaign's first.

**(6) Task 6 — two more, same round.** A half-open-interval claim asserted
against ordinary drawn events is vacuous, because the probability that a drawn
day equals an arbitrary boundary is zero; repaired by taking the boundary *from
a drawn event* and asserting inclusion and exclusion at that exact value. And a
magnitude-range test passed under a clamped law as happily as a truncated one,
because clamping is monotone and reaches both endpoints; repaired with a
discriminating probe near the top of the unit interval, where the two laws read
9.38 and 9.5.

**(7) Task 7 — a test that derived its expectation from the constant it was
pinning.** The memory sink test computed its horizon as
`MEMORY_HORIZON_HALF_LIVES × half_life`. Raising the constant from ten to twenty
moved the test's own expectation along with it, and the whole file stayed green:
the constant that decides where the sink sits was pinned by nothing. Repaired
with an **absolute** expectation — 400 years, hand-derivable from a 20-year
generation — which a change to the constant cannot follow, plus a second
assertion pinning the depth from the other side. **Caught by its own author** —
the second.

**(8) Task 7 — `most-recent-wins`, pinned by nothing at all.** The memory fold
takes the *latest* eruption in the horizon. Deleting the reversal, so it takes
the earliest, left all eleven tests green — while moving the answer by
essentially the whole unit interval on thousands of samples. Every existing test
had selected an eruption with a quiet gap *after* it and never constrained the
window *before* it, and the 400-year horizon is usually shorter than the
200–5,000-year eruption intervals, so a second eruption rarely landed inside one
by chance. A probe found 4,644 samples holding more than one eruption in a
single horizon, worst-case stock delta 0.9989. Found by a reviewer. Repaired
with a selector that **brackets the gap on both sides** — wide enough that the
two candidate answers cannot converge by rounding, narrow enough that the
earlier eruption is genuinely still in the window — and that asserts the earlier
eruption is inside before asserting which one wins.

### The generalisation worth keeping

Six of the eight are one shape in different clothes: **the test and the thing
under test shared a source.** A path compared to itself (2), a property
guaranteed by the architecture rather than the code (3), a name derived from a
value the sibling already pinned (4), an expectation computed from the constant
it pinned (7). The fifth and eighth are a different shape and also one shape:
**a fixture that never reaches the discriminating case** — round-numbered
windows (5), and a one-candidate sample for a rule that picks among candidates
(8).

The second shape produced this campaign's most portable sentence, posted to the
board when it was found:

> A fold that **picks one element** from a set needs a test with **two
> candidates**, because a one-candidate test passes under every selection rule
> there is.

## 2. What actually found them: mutation, run by the author, before review

None of the eight was found by reading. Every one was found by *changing the
code and watching what the suite said*. Two consequences the campaign should
carry forward:

- **Mutation-prove at authoring time, not at review time.** The three
  self-caught ones (5, 6, 7) came from implementers who ran a mutation harness
  over their own new tests as a matter of course. The reviewer-caught ones (4,
  8) cost a full extra round each. The cost of the harness is a few minutes; the
  cost of the round is an hour and a re-gate.
- **Re-derive a reported mutation rather than inheriting it.** When Task 5's
  reviewer reported a surviving mutation, the implementer reproduced it
  independently before repairing, and the re-reviewer proved the fix a third
  time. That is three cheap confirmations of a claim that would otherwise have
  been a sentence in a report.

## 3. A measurement defect with a signature you can read in the artifact

The exposure readout tallied land under a settleable-land filter but counted
settlements unfiltered. Marine settlements have no elevation above sea level, so
the band classifier's running label fell through to its initial value and put
every one of them in the lowest band — contaminating 51.9% of the pooled sample
with a single aquatic people.

The lesson is not "apply the filter to both sides", which is obvious in
hindsight and was obvious to nobody in advance. It is that **the defect was
legible in the committed fixture without re-running anything**:

> Three kinds reading *exactly* 100.0% of one band with exact zeros in the other
> three, beside one kind distributing normally.

An exact 100.0% is not an ecology; it is a default branch. A category sitting at
exactly the extreme of one bucket should be read as a bug report about the
classifier until proven otherwise. That is the same reading rule an earlier
campaign wrote down after a category came in at exactly 100% of one bucket, and
it fired again here, which is the point of writing such rules down.

The ruling on the repair is also worth keeping: the population was **narrowed
back to the spec's own words** rather than widened to a new ocean band.
Inventing a stratum after seeing the data is changing a preregistered population
post hoc, however reasonable the new stratum sounds.

## 4. Prose outrunning its own table — five rounds, including inside the fix

Every defect in three consecutive review rounds was a *sentence* that claimed
more than the table directly above it, and the code was right every time. The
implementer's own diagnosis:

> I generalise from the row I computed most recently, instead of reading across
> the table I just produced.

Instances: "monotonic in every band" against a table with two intermediate dips;
"the prediction is refuted" against an arm with no power to refute it; a filter
credited with closing a hole it does not touch, when what closed the hole was
changing the statistic. And an adjacent one — **an appended correction that
leaves the original standing is not a correction.** A report led with a
retracted claim and carried its retraction 367 lines later. The fix was to
rewrite the claim in place *and* post the correction to the board, because a
stale claim in a durable ledger keeps misleading after the report is closed.

**And a fourth round, which is the part worth carrying.** The whole-branch
review found the same shape again in the *published* prose — the chronicle,
this file's own opening sentence, and the test file's fix-round-1 record.
Three findings, all of the same family:

- The chronicle called the andosol series "monotonic" directly above a table
  containing `d2 = 0.006012 → d3 = 0.007355`. The identical error had been
  found and corrected 56 lines further down, about the *exposure* table, in the
  same document. **A correction applied to one table does not propagate to its
  neighbour**, and nothing mechanical connects them.
- The source claim under it was worse: the test file asserted the share fell
  "monotonically decreasing within EVERY band checked separately". Falsified in
  all four bands, and the band that falsifies it hardest — `0-250m`, where the
  share *rises* with unrest — holds 65.5% of all settlements. So the campaign's
  stated mechanism had the wrong sign on the ground most people live on, and
  the pooled table could not show it because the two upper bands hold 75.8% of
  the land.
- The scope clause "on the shipped roster" was written down as load-bearing,
  in the test file, in a paragraph explicitly warning against dropping it — and
  then dropped from **four of the five places it was restated**, including this
  file's opening sentence and the chronicle's conclusion, which used the word
  *innocent*. An acquittal is exactly what arm B has no power to issue.

The generalisation the third bullet forces: **the summary sentences are where a
qualifier dies.** A caveat survives in the paragraph that argues for it and
evaporates in the abstract, the heading, and the conclusion — the three places
a reader in a hurry actually reads. Writing "this clause is load-bearing" next
to the clause did not protect it. Re-reading every restatement against the
clause did.

**And then a fifth round, which is the one that actually settles the shape:
the fix wave reproduced the defect inside its own correction, twice.** Both
new sentences were written to discharge an overclaim and overclaimed in turn.
One said `make rebaseline` *never* rewrites the laboratory directory — an
absolute reached for while correcting an absolute, and false, because the
chorus study regenerates there unconditionally from fifty live worlds. The
other, written specifically to replace a stale sentence, stated a model
retired two campaigns earlier (hostility as "the worse of unrest and
aridity"), copied without checking from a book chapter that was itself stale.

Two things this pins down that the first four rounds did not:

- **A denial is a claim.** "X never happens" needs its own source read exactly
  as "X always happens" does. Correcting an overclaim creates a fresh
  opportunity to overclaim in the opposite direction, and the corrector feels
  careful the whole time.
- **Copying prose copies its staleness, silently.** The aridity error entered
  by being lifted from `book/src/domains/terrain.md`, which had carried it
  since The Tilth. The fix was not only to correct the copy but to correct the
  source, because otherwise the next campaign makes the same copy. A freshness
  sweep that repairs the passage it was pointed at and not the passage that
  passage was copied from has done half the job.

The count is now five rounds across one campaign, every instance the same
shape, several of them in text written by the people fixing the previous
instance. That is not a discipline failure to be exhorted away; it is
evidence that **prose has no gate**. Code has a compiler and tests; a sentence
has only whoever re-reads it against the table. The only mechanism that has
actually worked here is the boring one — open the source, re-derive the
number, then write the sentence — and it has to be applied to corrections with
exactly the same suspicion as to originals.

## 5. Two amendments that measurement forced, and one that expired

Three times the plan was amended mid-campaign, and the amendments split into two
distinct kinds worth distinguishing.

**Measured-wrong.** The recovery batteries were tagged as a heavy-tier battery
"(minutes)" and measured 0.55 s; the byte-identity probes were tagged the same
and measured 1.00 / 2.05 / 1.66 s. Both were untagged and moved into the commit
gate. The canonical heavy reason string is checked *verbatim*, so a wrong cost
estimate inside it is a false claim the guard actively preserves. **If a
plan asserts a cost, measure it before you inherit it.**

**Expired.** The design declined a cross-species memory prediction because the
species trait it would rest on had no occupant. During the campaign, two other
campaigns landed nine occupants on it. The decision was not wrong when written;
its *reason* simply stopped being true. The ruling was kept anyway — adding a
hypothesis after watching its axis go live is the post-hoc move preregistration
exists to prevent, and the fact that it would now be a better prediction is what
makes it inadmissible rather than what excuses it.

That is a category the process did not previously name: **a decision can
outlive its justification and still be right.** A plan amendment should say
which kind it is, because "the reason is gone" and "the decision is wrong" get
handled very differently.

## 6. A crate-scoped green marked a task complete on a red branch

Task 1 was marked complete while the branch was red: its population guard
violated a workspace-wide enforcement test that lives in the CLI crate, and the
task's evidence was a crate-scoped run that could not see it. This is the
documented shape and it happened anyway.

**Process change adopted mid-campaign and worth keeping:** no task is marked
complete without a green whole-workspace gate. The cost is one gate per task;
the alternative is discovering the red one task later, as the corpus already
records twice.

## 7. Attributing a heavy-tier red, cheaply

The close's heavy tier came back 77 of 79, and the two failures were of
completely different kinds — which is worth knowing because the reflex is to
treat both the same way.

- One was a **wall-clock ceiling** test that passes when re-run alone on the
  same checkout. The full-tier run had 33 slow tests and a CPU ratio of 17.63 on
  forty cores. A timing ceiling inside an oversubscribed tier is not evidence
  about a branch.
- The other was a **real red, inherited**: identical numbers at the merge-base
  as at the branch head. Its own output prints the diagnosis — a min-versus-max
  statistic that died when an earlier campaign widened the species roster, while
  the whole-roster correlation survives at 0.840.

The method generalises and is cheap:

> For any heavy-tier red: **re-run it alone** (separates contention), then
> **re-run it at the merge-base** (separates inheritance). Two scoped runs of a
> few minutes each replace two full tiers of half an hour.

The standing warning holds and gained a second instance: the heavy tier is
invisible to the ordinary gate **including on main**, so a red there is
invisible until someone happens to run it.

## 8. An empty diff needed four positive controls, and they did not overlap

The campaign's central claim is that nothing moved. Proving a negative needs the
instrument shown failing, so each byte-identity probe was made red before it was
trusted. The reds did not fall where the plan assumed they would:

| mutation | world JSON | scene | almanac |
|---|---|---|---|
| edifice decay length 1.5 → 1.6 | RED | RED | green |
| settlement name draw 2–3 → 2–4 syllables | RED | green | green |
| a sentence prepended to the almanac render | green | green | RED |

The almanac is a **summary** document, so it is nearly blind to a per-cell
terrain move and to a name change that lands late in the ledger — and it is the
only one of the three that sees a rendering change. The plan had treated the
three as interchangeable evidence for one claim. They are not: each covers a
different half, and had only one been written it would have been the wrong one
roughly two times in three.

A fourth probe sweeps every rendered artifact for the campaign's vocabulary and
was reddened by appending a sentence naming a volcano to a published page. It
is the cheap always-running half of the same claim.

## 9. Smaller things worth carrying

- **A control must be reproducible from its record.** The campaign's most
  quotable number — 409 settlements on a volcano, 89 remembering — first came
  from a probe that was run once and deleted. It is now three columns in the
  committed artifact, and the columns reproduce the deleted probe's integers
  exactly.
- **A regenerated artifact has no merge.** The type-audit count table merged
  wrongly on every row without conflicting, on **all four** of this branch's
  absorptions — four for four, not the "both" this bullet said when it was
  written after the second. Main carries a fifth instance of its own in
  `b19c2166`, inside the very range the fourth absorption pulled in, so the
  repo-wide count is **five**. The fourth was still producing fresh wrong
  numbers (`bare-ok(count)` 417 against a true 420). A defect that recurs on
  every single occurrence is not a mistake anyone is making; it is the merge
  algorithm doing exactly what it is specified to do to a file that has no
  meaningful line-wise merge. The method that catches it:
  regenerate the artifact and diff against what the merge produced — a correct
  merge makes the regen a no-op, so any diff at all is the merge's error. Note
  the asymmetry that makes this bite: a conflict-free merge is auto-committed
  and runs no pre-commit hook, while a conflicted one does. The easy merge is
  the unguarded one.
- **An unmoved laboratory directory is not evidence about the census**, because
  the regeneration script skips censuses. The gallery is the decisive tree.
  This rule was written here and then violated by the chronicle, which claimed
  the laboratory tables and the survey "regenerated without moving a byte" — a
  conclusion that is very likely true and whose evidence was never taken. A
  rule stated in the retrospective does not audit the chronicle.
- **A tripwire behind a `heavy:` tag is not a tripwire.** Spec §6.6 declared the
  exposure probe a regression tripwire — re-run it after any campaign touching
  siting or soil — and its mechanism was two *source greps costing
  milliseconds*, parked inside a 322-second heavy-tier test because that is
  where the arm they belonged to lived. Since decision 0125 there is no CI, so
  they ran only when a human dispatched `make heavy-remote`: the campaign that
  would trip the wire is exactly the one that would not run it. The fix was to
  extract the free half into its own untagged test and call it from both.
  Generalisation: **tag a test by the cost of its most expensive clause only
  after checking whether the cheap clauses can be split out** — a `heavy:` tag
  is applied to a whole test but earned by one part of it.
- **The host name is not stable.** This Mac reported two different names across
  a reboot, and the timings baseline is keyed on it. Read `hostname -s` before
  reading a baseline.
- **A killed session's work is worth assessing before resetting it.** When an
  editor crash killed a task mid-flight, the sweep found a coherent, compiling,
  passing 480-line state including the two tests carrying the property the task
  existed for. A fresh session finished it with no design changes.
- **A truncated artifact is indistinguishable from drift, so check the regen's
  exit code before believing a deletion.** `make rebaseline` exited 2 and the
  drift check reported `docs/digest/decisions-in-force.md` and
  `intent-vs-reality.md` losing exactly 130 and 5 lines — a clean, plausible,
  entirely fictitious diff. Both digest renders had been **`Killed: 9`**,
  SIGKILLed under memory pressure from parallel sessions on this box, and had
  truncated their output files mid-write. Re-rendering them serially restored
  exactly those 130 and 5 lines. The diff alone cannot separate a partial write
  from a real artifact move, and the reflex it rewards — "the regen deleted
  something, so something changed" — is exactly backwards. Read the exit code
  first. (Recorded only in merge commit `b98d6853` until this close.)
- **A per-test wall-time alarm measures co-scheduling, not cost.** The gate's
  `durations_have_not_regressed` went red on ~24 tests at 2–4× baseline after an
  absorption, and the first diagnosis — plain contention — did not survive its
  second data point: the alarm fired *identically* at `cpu_ratio` 4.71 and at
  7.41, the latter with **lower** user CPU than the green runs. What settled it
  was isolation, not argument. The flagged tests cluster by test **binary**
  (`scene::surrounds`, `worldgen::demesne`, `worldgen::depth`), which is how
  nextest batches; the **suite total moved only +3%** (331.5 → 341.0 s), which a
  genuine 2–4× slowdown of 24 tests cannot produce; and run alone they came in
  at 2.2–3.8 s against 2.5–4.2 s baselines. A later green run at load 5.24
  confirmed it. The baseline was deliberately **not** re-recorded: nothing had
  caused the shift, so recording it would bake co-scheduling noise into the
  reference every later run is judged against. Generalisation: when a
  duration alarm flags a *cluster* while the aggregate barely moves, suspect the
  scheduler before the code — and the cheap decisive test is one isolated run.
- **A quiet box needs all three load averages low.** This campaign took a false
  red by gating on a one-minute dip to 6.73 while the 5- and 15-minute averages
  read 17.75 and 19.58 — a *draining* box, not a quiet one. The one-minute
  figure is the one that recovers first and means least.
- **`git checkout -- <file>` destroyed an unrelated uncommitted edit.** During a
  mutation restore it reverted the whole file, silently taking a live edit to a
  different part of it. Nothing warned, and no test could have: the lost edit was
  prose. It was caught by grepping for a phrase that should have been there.
  `git checkout` on a path is a file-level operation wearing a hunk-level intent.
- **A guard that scans command text cannot tell setting a variable from writing
  about one.** A pre-commit hook blocked a *ledger write* whose prose merely
  quoted the census env-var name. This is the same shape as this campaign's own
  `no_rendered_artifact_names_a_geohazard` guard reddening on the word "volcano"
  appearing in a swept tree — and the two ended differently, which is the useful
  part: the volcano red was **correct** (the word really had entered a rendered
  artifact) while this one was pure false positive. A text-scanning guard cannot
  distinguish use from mention, so before weakening one, establish which of the
  two you are looking at.
- **Rebuilding a world per arm cost 3× what reusing it did.** Task 2's per-arm
  world rebuild measured **376 s** against the arms test's **127 s** for the
  *same* worlds. Worth knowing before writing the next counterfactual battery:
  the arms are cheap and the worlds are not, so build once and vary the arm.

## 10. Two questions this campaign did not get to answer

Recorded here because they exist nowhere else — the campaign's scratch dies
with its worktree, and `git diff main...HEAD -- docs/superpowers/specs/
docs/decisions/` is empty, so neither of these left a trace in a spec or a
decision record.

- **OPEN, FOR THE OWNER — does the framing change earn a spec amendment
  record?** Spec §1's motivating sentence ("a world's most fertile and most
  mineral-rich ground is its most tectonically violent ground, and nothing in
  the model ever charges for it") is **half wrong in both halves**: the
  fertility never reaches settlement siting at all, and unrest *is* already
  charged for, through `hostility`. The campaign sharpened rather than
  invalidated itself on this, and the chronicle carries the substance — but the
  question of whether a spec whose premise did not survive contact with the code
  should carry an amendment record was explicitly reserved for Nathan at G6 and
  was never put to him. It is a process question, not a product one: the same
  situation will recur.
- **Provenance for two G4 ideonomy overturns, whose *decisions* landed in the
  plan but whose *reasoning* did not.** Plan §0.1/§0.2 and §0.3 record what was
  chosen; neither records that the first pass had chosen otherwise. (1) The
  single-arm counterfactual: the first pass accepted the spec's single arm and
  proposed only widening its tolerance; a **modularity substitution** over the
  derive → project → price → site → render cycle overturned it, by showing
  andosol enters at `render` and never at `price` — which is what turned one arm
  into three and promoted the two live channels to controls. (2) Volcano naming:
  the first pass named mountains in the flagship people's language; the **cycle
  organon** overturned it, on the ground that naming → forgetting → renaming is
  the *same* repose cycle the campaign is about, so a people losing its mountain
  should lose its name for it — which only works if the name is per-people. Both
  overturns changed the shape of the work, and in both the first answer was the
  obvious one. That is the argument for the second pass being routine rather
  than optional.
