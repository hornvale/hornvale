# Retrospective — The Domesday (a generated survey of the thousand worlds, 2026-08-08)

Process lessons only; the product story is
[the chronicle](../../book/src/chronicle/the-domesday.md). Decision 0020 governs
the form.

## The headline: every defect originated in the plan's text

**Every defect this campaign hit originated in the controller's own plan or
spec text. Zero originated in implementer code.** That is the same headline The
Digest wrote the day before, and the two campaigns together now put the count
well past twenty. It has stopped being an observation about two campaigns and
become a property of how the work is organized: a plan written as a literal code
listing is reviewed for faithful transcription, and faithful transcription of a
wrong predicate produces a wrong predicate.

Eight defects, all caught. Three are worth writing up because their mechanisms
generalize; the rest are listed after.

### 1. A convention the repo had already settled, in a file that names the exact mistake

The statistics task shipped a percentile helper and a median helper, and its
brief supplied a hand-computed oracle to pin them against. The brief's own
formula and its own oracle used two different nearest-rank conventions, which
agree everywhere except on an even-n exact tie — and the census has exactly a
thousand rows. The implementer refused to tune the algorithm to the assertion
and stopped, correctly.

The resolution was not a judgement call. The repo had settled this months
earlier: `windows/lab/tests/the_fare_calibration.rs` defines `median()` as the
average of the two middle values on even n, defines `percentile()` as
nearest-rank, and states in a doc comment that `percentile(0.5)` may differ from
`median()` and that the two are never interchanged. The same comment warns, in
those words, that a bare `sorted[len / 2]` is an *upper*-median.

The controller's oracle was a bare `sorted[len / 2]`.

Three values were in play — the repo's median (−11.988568), the bare upper
median (−11.900311), and nearest-rank (−12.076825). The implementer's code
matched the repo's `percentile()` byte for byte. The code was right and the
oracle was wrong, and the oracle was wrong because it was computed by hand
without opening the file that owns the convention.

**The lesson is not "be careful with percentiles."** It is: *grep for an
existing helper before writing a new one, and when one exists, compute the
oracle with it rather than beside it.* A doc comment predicted this exact
mistake and the mistake was made anyway, which means reading the file is the
step that has to be mandatory — not knowing that the file might exist. The
campaign's stated numbers moved as a result: the census's median land
temperature is −11.99 °C, not the −11.90 °C carried in a registry row.

### 2. A preregistered ceiling set without measuring the distribution it bounded

The spec froze a prediction that the degeneracy detector would fire on at most
ten descriptor metrics after the role split. It fires on twenty-seven.

The ceiling was a guess. The pre-implementation diagnostic had measured the
*unsplit* distribution (forty metrics at or above the 80 % bar) and had reasoned
that the role split would account for most of it, but never measured what the
split would actually leave. Ten was the number that felt like a success.

The falsification was handled correctly — the twenty-seven were inspected, ruled
overwhelmingly real, the threshold left untouched, and the ceiling retired
rather than raised — but the process lesson sits earlier than the handling.
**A preregistered bound on a quantity you can measure before freezing it is not
preregistration; it is a wish.** The diagnostic that produced the role field was
one query away from also producing the post-split count. Freezing a bound you
could have measured spends the credibility of preregistration on a number that
was never at risk of being right.

The corollary, which the spec got right and is worth keeping: after the count
came in, an *expected-skew* class was proposed that would have cut it. It was
declined, because a count-reducing mechanism proposed after seeing the count is
the same phase-order violation that had already cost the strength detector its
threshold. Declining it is the reason the twenty-seven can be believed.

### 3. Three unenforced guards inside the detector built to find unenforced guards

The weakness detectors are the campaign's product, and the module contained
three constraints that were not constraints:

- **A rendered sentence that was false as worded.** The unmeasured-domain
  detector rendered "no census metric reaches the `paleoclimate` crate." By that
  literal rule the claim is wrong — the composition root calls
  `hornvale_paleoclimate::{glaciated, extract, genesis}` inside the terrain
  build, so every terrain metric transitively "reaches" it. The substance was
  right and the stated rule was not. Tightened to the rule actually applied: no
  census metric *measures any quantity the crate produces*.
- **A frozen list with no live guard.** The roster of measured crates was a
  hand-written `const` beside a partner roster that *did* have a live test
  asserting it still matched the tree. Nothing compared the measured roster to
  anything. A crate could gain its first metric and the survey would go on
  reporting it unmeasured, green forever.
- **A boundary test that never called the code it tested.** The detector's
  fires/does-not-fire test rebuilt the membership filter in its own body instead
  of calling the detector. Emptying the real function leaves it green. Fixed by
  splitting the detector into a frozen-roster entry point and an explicit-roster
  core the test can drive.

All three were found by an independent reimplementation that ran the system
rather than reading it — the same method that found the Confidence Gradient's
earlier instances. **A constraint that is not executed is not a constraint**,
and the fact that these were inside the instrument built to detect exactly this
shape is the useful part: knowing the failure mode confers no immunity to it.

### The other five

4. **A quine guard.** The never-builds-a-world test used `include_str!` on its
   own file to scan for forbidden identifiers — which were present in that file
   by definition, so it always matched itself and could never fail. Relocated to
   the module root and proved red by injecting a violation.
5. **`split(',')` over a CSV with quoted embedded commas.** The plan's parser
   text would have shredded every row carrying a value like
   `"farmer,shaman,chief"` — silently, into misaligned columns.
6. **A stale-schema ordering trap the plan created.** The annotation task's
   "no drift" report never looked at the census schemas, because
   `regenerate-artifacts.sh` skips censuses by default. Both schemas predated
   the new `domain`/`role` fields by six hours. Resolved with a backfill
   subcommand that rebuilds a schema from committed rows and builds no world.
7. **`env!("CARGO_MANIFEST_DIR")` in production `src/`,** which trips the
   build-path-embedding check (decision 0090) by raw textual match — even inside
   a doc comment, and `#[cfg(test)]` does not exempt it. The controller's
   suggested fix was also wrong; the implementer solved it with a relative
   `PathBuf`.
8. **A misclassified subject that would have published a false absence.** See
   below — this one earned a spec section rather than a bullet.

## The distinction that had to be invented: world versus instrument

The campaign started with a good principle and it was not sharp enough.

Nathan's rule was that **a gap discovered while building the instrument gets
rendered, not fixed in passing** — because an absence quietly repaired during
construction is one the instrument never learns to see. Applied to a `Hydrology`
domain that had come out of the annotation pass with zero metrics, the rule said
publish the empty chapter.

Review overturned it. Twelve hydrology metrics existed and were filed under
`Terrain`, inherited mechanically from the build rung. Publishing "water is
unmeasured" would have asserted something false about the project, in the one
artifact whose purpose is being trustworthy.

**A gap in the WORLD is rendered. An error in the INSTRUMENT is fixed.** The
original rule collapsed the two. Sixteen metrics moved, and the finding worth
keeping was never the empty chapter — it was that mechanical rung-inheritance
misfiled sixteen metrics, which is a lesson about the annotation method.

The generalization: **a survey's credibility is the only thing it has.** Any
rule that can cause a survey to publish a false claim about its subject is
wrong, however good the reason. The rendering rule is about the world's
absences, not the instrument's.

## Two process errors of the controller's own

**Committed while a subagent had the tree dirty.** A docs commit was made while
a fix-round agent had a source file staged; `git add`ing only the controller's
own paths did not help, because the agent's already-staged work rode along.
Forty lines of a partial reclassification landed under a commit message entirely
about something else. The tree compiled, the hook passed, and the agent was
unaffected — the damage is history attribution only. **Staging discipline is not
sufficient protection; the rule is never to commit while a subagent has the tree
dirty.**

**An amendment to the spec is not an amendment to the plan.** After the strength
detector's threshold was superseded by a declared-class design, the spec was
updated and the plan was not — leaving the retired `min_abs_r` in six places
across the document the *implementer* reads. Caught by the controller before
dispatch, but only by chance. Both documents govern, for different readers, and
an ideonomy pass that overturns a design owes an edit to both.

## What worked

**Refusing to tune.** Two thresholds were under pressure to move and neither
did: the degeneracy bar was not lowered to 60 % so that ice dominance would
fire, and it was not raised after the twenty-seven came in. Both refusals are
why the survey's numbers can be read at face value.

**Independent reimplementation as review.** The detector task was reviewed by
reproducing all eight tallies and all 113 finding names in Python, directly over
the committed census. It matched exactly, which is what made the falsified
prediction believable as a finding rather than a bug — and it is what found the
three unenforced guards, none of which a reading review had raised.

**Mutation as proof, twice.** The drift check was proved red on command by
corrupting one census value and confirming that the rendered maximum moved to
exactly the injected number and the mean shifted by exactly the implied
arithmetic. The empty-domain branch — dead code until a test reached it — was
proved both ways: change the message and it reddens, delete the branch and it
reddens.

## Follow-ons this campaign deliberately did not do

- **The expected-skew role class.** Would separate correct physics
  (`tidally-locked` false on 95.2 %) from genuine degeneracy. Preregister before
  the next run or not at all.
- **Re-annotating role from the observed distributions.** Roughly thirty metrics
  read as invariants-by-construction while declared `descriptor`. Same
  phase-order hazard: doing it now is tuning the count after seeing it.
- **Mode-bucket statistics.** Specified, unimplemented; blocked on the
  statistics interface, correctly flagged rather than scope-crept.
- **The 73 ranked findings themselves**, which are the campaign's product and
  are recorded in the close-out report and promoted to registry rows here.

## Housekeeping found in passing

`docs/digest/` is missing from the drift-check path lists quoted in
`cli/CLAUDE.md` and `windows/CLAUDE.md` — pre-existing, unrelated to this
campaign, and not fixed here to keep the diff honest.
