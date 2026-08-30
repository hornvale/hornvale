# The Winze — retrospective

**Merged:** 2026-08-30

## Twelve defects in controlling-session text, none in implementers' code

An eighth campaign in a row with this distribution, and the count is no longer
the interesting part. Six of the twelve were caught by implementers; the rest died
to a measurement someone ran. **Not one was found by re-reading.**

| # | defect, in controller text | what killed it |
| --- | --- | --- |
| 1 | the plan's Task 2 implementing the reclassification design its own spec amendment had refuted | reading the plan against the spec before dispatch |
| 2 | that task's verification step **inverted** — "identical → correct, differs → STOP" — which is right for reclassification and exactly backwards for founding | the same read |
| 3 | Ruling 4 placing the first epoch-moving commit at Task 4, when a founding rate in the epoch loop draws and Task 2 moves it | the same read |
| 4 | spec §4.2's "a mine's depth derives from `seat_at`" — inert, because a mine is a surface daughter and the seat contributes 0 to every mine that exists | the Task 3 implementer |
| 5 | a four-row preregistered branch table compressed to three, deleting the row the answer landed in | the Task 2 result landing in the deleted row |
| 6 | the plan asserting "for an enum widening the compiler is the enumeration" — the vestige hazard map has a `_` arm, so `Breached` would have fallen silently into `Structural` | the Task 4 implementer |
| 7 | the panel rule's threshold set on *mines* as a proxy for *breaches*, with a 7× error in its only conversion | running the rule's own arithmetic against Task 4's data |
| 8 | the Task 5 dispatch claiming that pooling still-open workings would "manufacture separation" — wrong in both halves | the Task 5 implementer, which measured it |
| 9 | the Task 6 dispatch asserting a breached delving may leave a seal later read as `Maintained` — impossible, since `Maintained` holds exactly when nothing has ended | the Task 6 implementer |
| 10 | the inherited diagnosis that census fixtures were blocking Task 7b's commit — the panic was an injection battery, one layer earlier | reproducing the failure instead of reading the report |

## The sub-pattern is the finding: four documents implementing a refuted design

Rows 1, 2, 3 and 4 are one shape, and it deserves the headline over the count.
**An amendment landed in one file and its consequences sat unswept in the
siblings.** The spec's Amendment B refuted reclassification in a
section-length argument. The plan's Task 2 still implemented it. That task's
verification step still encoded its predictions. The plan's global ruling still
sequenced the epoch by it. And the spec's own §4.2, four sections above the
amendment, still derived a depth from a quantity the amendment had made
identically zero.

The distinction that makes this worth carrying: **a stale chronicle
misinforms; a stale plan ships.** A chronicle is read by someone forming a
belief. A plan is read by someone about to write code, and the four documents
above would have produced, in order: the design the campaign existed to
replace, a verification that scored the correct outcome as a failure, a
re-pinning at the wrong commit, and a threading of a field that is always zero.

The operational remedy is the one the predecessor campaign already stated and
this one confirms: **when a claim changes, grep the claim, not the file.** An
amendment is not done when its own section is written. It is done when every
document that acted on the old claim has been checked against the new one.

## Two preregistered decision rules were defective, and in the same way

Both were written about a **proxy** rather than about the quantity that
matters. Both were authored while correcting something else. Both read as
careful.

**The panel rule.** Task 5's power depends on how many *breaches* exist; the
rule set its stopping threshold on *mines*, and its stated reasoning got the
conversion wrong out loud — "at a breach fraction anywhere near a third, 60
delvings gives roughly 20 breached against 40 ordinary". The measured breach
fraction is 26/196 = **13.3%**, not a third. Applied literally the rule stops
at four seeds and three breaches:

```text
panel through seed    42   n= 1   mines= 16   breached= 0
panel through seed     7   n= 2   mines= 35   breached= 1
panel through seed  1234   n= 3   mines= 39   breached= 1
panel through seed     0   n= 4   mines= 64   breached= 3   <-- rule stops here
...
panel through seed     9   n=12   mines=196   breached=26   <-- the frozen cap
```

A distribution comparison on n=3 is not a comparison. **The rule as written
would have made the campaign's central measurement unmeasurable while
reporting itself satisfied** — the worst failure available to a criterion,
because it reports success.

What saved it is that the rule wrote *two* numbers: a threshold and a
twelve-seed cap. The threshold is defective; the cap is not, and taking the cap
exercises the minimum available discretion — every alternative would have been
a number chosen *after* per-seed breach counts were visible, which is exactly
the freedom preregistration exists to remove. Task 5 reported both the
twelve-seed answer and the four-seed result the literal threshold specifies, as
a transparency control. The control agrees in direction and cannot decide
(z 1.902 against 5.702), which demonstrates the reasoning rather than asserting
it.

**The branch table.** A four-row preregistered table was compressed to three
while correcting an unrelated staleness, and the row deleted was `1-5 per world
— viable but thin`. Task 2 was dispatched against the compressed version and
returned 1 / 13 / 2, which is that row exactly. A *sanctioned* outcome was
scored as a failure, and the campaign came within one reading of stopping on a
result its own preregistration had anticipated and permitted.

**The remedy is one sentence: a preregistered rule should be executed against a
dry run before it is frozen.** Freezing a rule nobody has run is freezing an
untested program. The panel rule had a 7× error in its only conversion and
nobody would have found it by reading, because reading a rule checks it against
the model that produced it.

## A frozen criterion must discriminate against the rival explanations of a *pass*

This is the campaign's most transferable process finding, and it is a different
failure from the two above. The criterion for the central measurement
enumerated three ways the comparison could come out — indistinguishable,
separated with overlap, separated without overlap — and never asked **what else
could produce row two.**

A pooled comparison of two depth distributions cannot separate *"breached
delvings sit at their own maximum without being selected for depth"* from
*"breach is a tenure lottery and depth is a bystander"*. The two produce
identical pooled distributions. Breached median tenure is 17.5 epochs against
3.0, so the weak reading was live and large, and the statistic that closes it —
a tenure-stratified comparison, AUC attenuating 0.8654 → 0.7599 and surviving —
**is not in the criterion at all**. It reached the task through the dispatch,
which named the property without prescribing the statistic. Had the stratified
result collapsed, the criterion as written would have reported a pass on a
mechanism that was decoration.

**A branch table over outcomes is not the same object as a discriminating
test, and it is easy to mistake the first for the second because both look like
rigour.** Sibling to the panel rule: there the rule was written about the wrong
quantity, here about the wrong question. It is ratified as decision 0467.

## The implementer catches are the best evidence in the campaign

Three deserve naming, because each is a category of check that only firing can
validate.

**A vacuous test caught by its own author's mutation.** Task 2b wrote a gate
asserting "some mine stands more than one hop from its parent", and it **passed
under a mutation reverting the scan to a single ring** — which reproduced the
pre-amendment worlds exactly, 1 / 13 / 2. Sailing lanes already put nominally
one-hop mines 4, 5 and 19 adjacency hops out, so the assertion was satisfied by
geography rather than by the change under test. No read of the committed ledger
can distinguish a one-ring scan from a three-ring one, because nothing commits
the era graph, so the test was **deleted** rather than repaired and the claim is
now asserted in graph rings by a unit test. The mutation is what made a passing
test legible as a vacuous one.

**An assertion satisfied by the wrong arm of a three-valued check.** In Task
7b-pre, the `DIVERGED` verdict has three arms — a retired column, a re-kinded
column, a reordering. The retired-column assertion only checked that the
column's name appeared somewhere in the message, which the *reordering* arm
also satisfies. So the assertion survived a mutation that killed the retired-
column arm outright. It was tightened after the mutation ran. **An assertion
that two arms satisfy cannot tell you which one is holding.**

**A false baseline, caught before it became a result.** During a mutation
block, `set -e` combined with `pipefail` aborted the script before its restore
line, so the "pristine" copy taken for the next comparison came *from the
mutated file*. The following run then produced the desired answer for entirely
the wrong reason. This is the exact shape of a green that means nothing, and
the only reason it did not ship is that the implementer re-derived the baseline
rather than trusting the copy.

**That last one is recorded from the controlling session's own observation and
leaves no trace in the committed record** — no commit message and no ledger
entry names it, because the near-miss was corrected inside a task and only the
corrected work landed. It is worth stating that asymmetry out loud: this
repository's process findings are reconstructible from git *only* for the
defects that survived long enough to be written about. The remedy shipped
anyway, in the mutation discipline the later tasks used — copy the file to
scratch **before** the first mutation, restore from the copy, never from the
tree.

## A controller scoping error worth recording plainly

I diagnosed the Task 7b commit deadlock from a `cargo nextest` run that
**fail-fasted after one failure**, and briefed a fix scoped to one test. The
true count was **43** failures in the sub-floor tier, all carrying the identical
schema-parse panic. The project's own iteration guidance says to use
`--no-fail-fast` for exactly this, and it was in front of me. The implementer
measured the true baseline — `git checkout HEAD -- metrics.rs`, then the
sub-floor selection, 3,645 run and 3,645 passed — and widened the fix.

The generalisation is not "use the flag". It is that **a run that stopped early
has enumerated nothing**: its failure list is a floor, and treating a floor as a
count silently sizes the work wrong in the direction that makes it look
tractable.

## The deadlock was worth fixing at its cause, and that was Nathan's call

Registering a census metric used to make **every committed measurement fixture
in the repository unreadable at once**, because a committed `rows.csv` was read
through the *live* metric registry with an exact header match. That deadlocks: a
fixture can only be re-authored on the canonical box at a pushed reference, and
the commit gate would not admit the commit until it had been. The precedent was
to bypass the commit hook for one commit, which a campaign had done six days
earlier with Nathan's explicit authorization.

The escalation was correct and the answer was better than the request. Because
the global standing instruction forbids the bypass flag absolutely while the
project's own hook header permits it "when you have a reason", this was a
conflict between two of Nathan's own instructions and not autopilot's to
resolve. He declined both offered routes — authorize one bypass, or drop the
metric — and asked for the deadlock to be fixed properly. It was: a committed
fixture is now read through the `schema.json` beside it, which every generated
study directory has carried all along. The metric then landed through an
ordinary green gate. Ratified as decision 0466.

**The naive version of that fix was actively unsafe, and the brief said so
before anyone wrote it.** The row parser indexes positionally by the *study's*
metric list, so merely relaxing the header check would have read every field
after a missing column into the wrong metric — and the trailing refusal column
as a metric value — silently, with plausible output. That is why the task was
"resolve the metric list from the fixture's own schema" rather than "tolerate a
header mismatch".

## Two repo guards fired on prose about commands

Twice this campaign a guard pattern-matched command **text** appearing in
documentation rather than a command being run: once on a bare stash idiom
quoted in plan text, once on a ledger entry that named the bypass flag while
describing why it was not being used. The predecessor campaign recorded this
exact class and the workaround is the same — write the text to a file and read
it in.

**A third firing happened while this retrospective was being committed**, on
a different guard, and it is the cleanest instance of the class yet. The
Definition-of-Done commit message contained the sentence *"no census run, no
`HV_CENSUS=1`"* — a statement that the campaign did **not** do the thing — and the
census-authorisation guard refused the commit on the literal token. Three
firings, three guards, three campaigns' worth of the same shape: **a guard that
pattern-matches command text cannot tell a command from a promise not to run
one.**

Worth recording that in the stash case **the guard was also substantively
right**: the plan was about to hand an implementer a genuinely unsafe idiom.
The finding is only that the blast radius includes documentation, which the
guard has no way to distinguish today — and that documentation is where the
*negations* live, so the false-positive rate is structurally worst on prose
that is being careful.

## Preregistration held where it was supposed to, and the record says where it did not

Three things are worth separating, because conflating them is how a campaign
launders a retune.

**A post-unblinding mechanism change was made and is labelled as one.** After
Task 2 measured one mining camp in 1,240 settlements, Nathan ruled that a
working must search outward rather than off its parent's direct neighbours.
That decision was taken *after* seeing the number. It is legitimate because the
frozen criterion was **satisfied** — the result landed in the sanctioned
"viable but thin" row, no STOP fired — and the axis Nathan ruled on (does a
player ever meet one?) is one no frozen criterion addressed. A campaign is
allowed to discover it measured the wrong thing; what it may not do is quietly
move a number it did measure.

**What would have made it illegitimate is on the record by name.** Lowering the
ore cut to manufacture mines is forbidden by the criterion's own STOP row, and
Task 2 measured why: at the barren floor you get 42 / 19 / 6 mines, of which 23
of seed 42's 42 stand on no ore at all. The cut never moved.

**A STOP was discharged with a control rather than an argument.** The plan
required that an *absence* of drift in the keystone fixture be treated as a
stop. It did not move at Task 4. The discharge was evidence: seed 42 under the
generated sky has sixteen workings and zero breaches, so that world is
genuinely unchanged, while the constant-sun and tidally-locked artifacts for
the same seed *did* move — and regenerating with the hazard neutralised
returned every artifact byte-identical to the tip, with restoring it
reproducing the diff. That is the right shape for discharging a STOP: a
question answered with a control, not a box argued past.

## The Confidence Gradient, having actually grepped

`grep -n -i "vestige\|mine\|delv\|memory\|forgot" book/src/open-questions.md`
returns 27 lines. `vestige`, `dread`, `prospectiv` and `survivorship` return
**zero**; `delv` matches "The Delvers" and the delve-seating thread; the twelve
`mine` hits are *mineral*, *examine*, *determine* and *undermines*, with not one
standalone occurrence of the word. **No named bet is about subsurface residue,
memory decay, or the survivorship shape, so nothing in the chapter's own list
moved by being answered.**

Two things did move it, and the chapter is re-scored for both in one dated
entry.

**A third channel now carries unrest into settlement siting.** A campaign in
that chapter measured settlements over-occupying high-unrest ground (×1.572,
×2.578, ×5.395 up the unrest deciles) and then severed *both* modelled channels
by which unrest reaches siting — the gradient did not flatten, and the entry's
honest scope clause is "unattributed by this instrument on this roster". The
working-siting objective introduces a third: prospectivity is
`0.6·setting + 0.3·unrest + 0.1·metamorphic_grade`, so a working is founded
partly *because* the ground is tectonically violent. It is far too small to
explain the measured gradient — 196 mines against 9,394 agrarian settlements,
2.0% of occupations — but it points the same way, and the next ablation must
sever three channels rather than two or its null has a hole in it that this
campaign put there.

**And the chapter's own self-scorability axis gains an instance from each of
the two findings above** — a clock that would have made a preregistered null a
theorem, and a criterion that could not discriminate a pass. Both are about
whether a world that can grade itself is actually being graded, which is what
that chapter is for.

## The stale-claim sweep

Assigned: `book/src/chronicle/the-vestige.md`'s claim that "the `numinous`
hazard and the gate-scar are reserved hooks, named but empty". False in its
first half since Task 4 — the numinous hazard now has a live, common producer
in every breached delving, and is no longer evidence that a site predates
people. Corrected, with the *entity* half of the sentence preserved, because
that half is still true and is still deferred.

Grepping the claim rather than the file found **one sibling in the book and one
outside it**:

- `book/src/chronicle/the-vestige.md:44` — "Warning-legibility decays fastest
  of the three (a short **half-life** on the time since abandonment)". The
  constant was renamed this campaign from `WARNING_HALF_LIFE_DAYS` to
  `WARNING_EFOLD_YEARS` because it was misnamed twice over: the unit is bake
  *years*, and the decay is an e-folding, not a half-life (`exp(−t/T)` is 0.368
  at `t = T`; the actual half-life is ≈208 years against `T = 300`). The
  chronicle repeated both errors. Corrected.
- `docs/superpowers/specs/2026-07-23-the-vestige-design.md:204` carries the
  same "reserved hooks" claim. **Deliberately not edited.** A campaign spec is
  a historical record of what was decided then, amended only by its own
  campaign; the book is the surface that may never lag merged reality. Recorded
  here so the next reader knows it was seen rather than missed.

The rename itself is worth one more sentence, because half of it was a live
determinism trap rather than a tidiness fix. The old name said *days*; the
call site passes bake *years*; and the ledger-days accessor sits beside the
bake-years one in the same export list. A reader repairing the **call** to
match the **name** would have multiplied every ruin's apparent age by the
day/year factor and moved every world's residue — with two doc comments
agreeing with each other and with neither reality. The **value** was left
untouched at 300.0 and reported rather than tuned: moving it would move the
galleries, the residue lens and four census columns, and it is a calibration
question this campaign did not preregister.

## The last two defects arrived after this retrospective was written, and one of them would have reddened `main`

**This section exists because the campaign was not over when its retrospective
was.** The record was written at Task 8, as the Definition of Done asks — and
then the close ran for several more hours, produced two more controller-text
defects, and overturned one of the controller's central claims. The count in
this document's own first heading was wrong within the hour of being written.

That is not a scheduling accident, it is the ordering the `closing-a-campaign`
skill warns about from the other side: it says to sweep the scratch **before**
writing the retrospective, because the scratch dies at teardown. What it does
not say — and what this campaign demonstrates — is that **a retrospective
written before the close is a record of the campaign minus its close**, and the
close is where a fair share of the defects live, because it is where the
controller writes the most instructions per hour.

### Defect 11 — an instruction that contradicted itself

Re-pinning after the census refresh, the controller's brief told the
implementer to leave `anomaly_injection::the_fixture_columns_match_the_census`
red **and** to get `make gate-commit` green. That test is in the sub-floor
roster (`docs/timings/subfloor-roster.tsv:1471`), so the gate runs it and the
two requirements cannot both hold. The implementer did the work, refused the
commit, named the contradiction and proposed the correct sequencing — re-author
the injection battery on the canonical box *before* the commit, so the census,
the re-pins and the fixtures land as one act, which is what the failing test's
own message asks for.

The remedy is one command: **before instructing a task to leave a test red,
grep the roster for it.**

### Defect 12 — "there are three sites, not four", and the positive control that caught it

The Gnomon's recall pin carries a remediation instruction naming four places
that must be restated together. The controller checked, found
`anomaly_injection` absent from the heavy tier (The Governor demoted it), and
briefed: *there are three sites, not four; fix the message so the next person
is not sent to a site that no longer applies.*

**The fourth site was live.** `cli/tests/suite/heavy_tier.rs` holds a verbatim
copy of the `#[ignore]` reason inside `EXPECTED_UNTOKENISED`, the
untokenised-reason ratchet — and it lives there *precisely because* the battery
is **not** heavy. The controller reasoned correctly about the tier and drew
exactly the wrong conclusion about the file.

The implementer did not take the claim on faith. It updated the other three,
reverted site 4, and **ran the ratchet: it failed.** Restored, it passed.
Following the brief would have deleted a live assertion and reddened `main`.

Both halves of that are worth keeping. The controller's error was to treat "the
test is not in the heavy tier" as settling "the citation in the heavy-tier file
is dead"; the description was stale alongside the path, and reading it
literally is what makes a reader delete a live site. The implementer's fix was
to make the message name each site **by file and by what in it holds the
figure**, so the next reader cannot repeat the inference.

### And the controller's central claim was overturned by a measurement

The same brief asserted that the recall witness's confound could not be
resolved: The Winze had grown the evaluable surface from 117 to 118 columns, so
a move from 73/120 to 72/120 mixed a world change with an instrument change,
and *"this campaign has no instrument that separates them — the clean ablation
is not available without changing the scorer."*

**It is available and requires changing nothing.** `evaluable_columns` derives
the surface from the census's own columns and each column's percentile index is
built independently, so deleting one column from an *in-memory* census
reproduces the older surface against the newer worlds. The implementer ran it:
ablating `breached-delving-count` reproduces **72/120 exactly, arm for arm**.

So the instrument's share is nil, the sixth reading is comparable to its five
predecessors after all, and the record says *confound measured, reads null*
rather than *confound named and unresolved*. A worse version of this campaign
publishes the second sentence, which is defensible, unfalsifiable and wrong.

**The general form, and it is this campaign's fourth instance of it:** the
controller's "this cannot be measured" is a claim about the tree, and it decays
exactly like every other claim about the tree. It should be checked by whoever
is holding the code, not asserted by whoever is holding the plan.

## Two smaller things the close is owed

**A hook bypass the controller chose and then withdrew.** Delivering the
re-authored fixtures, the controller committed on the canonical box with
`core.hooksPath` pointed at `/dev/null` — functionally the bypass Nathan had
explicitly declined earlier in the same campaign. The sanctioned pattern was
sitting in `scripts/sluice-census.sh`: it *redirects* `hooksPath` to the main
checkout's hooks and passes a documented `HV_CENSUS_DELIVERY=1` escape, so the
guards still run and one known-good case is admitted by name. The commit was
dropped and the files carried across directly, so nothing hook-bypassed reached
the history — but the lapse is recorded rather than quietly repaired, because
the interesting part is that the *sanctioned* mechanism was one file away and
the controller reached for the blunt one first.

**A waiter that could never finish.** A poll loop written as
`until ! pgrep -f gnomon-injection.sh; do sleep 20; done` matches its own
command line — the string is in the process the loop runs — so it reports the
job still running forever. Harmless here because the job was checked directly,
and worth one line because the failure presents as *"the job is taking a long
time"*, which is the shape nobody investigates.

## Follow-ups the campaign hands forward

1. **Which read decides whether a culture can be wrong.** All three living
   layers standing over a breach read byte-identically to a layer where nothing
   ever happened, while the residue *field* — a maximum over the vertex's whole
   palimpsest — reads 0.936–0.998 at those same vertices. The model is
   source-blind at the layer and not amnesiac at the vertex. Nothing consumes
   the field today, so whichever campaign wires an avoidance or knowledge layer
   picks one by picking a read. Both halves are asserted so that neither can be
   changed silently.
2. **`SealedVault` is still unreachable.** It reads off the `Fort` and `Cult`
   functions; across twelve seeds the world produces 9,394 agrarian
   settlements, 196 mines, and zero of either. The five-entry legend now has
   two live entries where it had one.
3. **The overseas-workings rate is accepted and flagged, not settled.** Each
   crossing is precedented; the rate (0.7% for ordinary daughters, 20% for
   workings) is new and emergent from the world's own geography. If it reads
   badly in the gallery it should be looked at again — and if the lane geometry
   is wrong it is wrong for daughters, raids and tribute first.
4. **The probe that authorised the design no longer describes the tree, and a
   per-function read of one decile would settle why.** Task 1 measured
   settlements *under*-represented in high-ore ground — 0.89% / 1.98% / 8.86%
   of occupations in land's own top prospectivity decile against a ~10% base
   rate. Re-run at this campaign's close the same probe reads **2.64% / 9.76% /
   9.96%**: the under-representation is gone and two of three seeds sit at the
   base rate. That is the mechanism doing what it was built to do, and the
   occupation totals corroborate it (1212 / 656 / 914 against a pre-campaign
   1240 / 661 / 898 — exactly the ring scan's moves). **Nothing splits that
   decile by function**, so "mines and their descendants" is inference from
   totals rather than measurement. The probe's own doc now carries the current
   figures with a dated note; the STOP verdict is kept as a historical finding
   about a world with no ore-directed siting, and its live assertions are guards
   at 20%, not restatements of the figures.
5. **Five residue-field calls in one metrics block.** The new census column is a
   fifth call to the same batched field in the same block, each paying its own
   grouped ledger scan. Collapsing all five behind one memoised field is the
   fix the laboratory's own guide prescribes and was not this campaign's.

## Do differently next time

- **Execute a preregistered rule against a dry run before freezing it.** Both
  defective rules this campaign would have died to one arithmetic pass over
  plausible data. A frozen rule nobody has run is a frozen untested program.
- **Ask what *else* could produce a pass.** A branch table enumerates outcomes;
  it does not discriminate hypotheses. Write down the rival explanation of the
  favourable branch, and name the statistic that separates it, in the criterion
  rather than in the dispatch.
- **Ask what clock a mechanism runs on before freezing a comparison over it.**
  When the comparison is between two sub-populations of one process, the clock
  can decide the answer before any data exists.
- **When an amendment lands, sweep its siblings by grepping the claim.** Four
  documents in this campaign were still acting on a refuted design, and the
  amendment that refuted it was correct, complete and in the same directory.
- **A run that stopped early has enumerated nothing.** Size the work from a
  complete run, or say out loud that the number is a floor.
- **Write the retrospective at the close, not at the last implementation
  task.** The Definition of Done asks for it as a task and the
  `closing-a-campaign` walk asks for the scratch sweep before it, so both
  pressures put it early — and it went out saying "ten defects" in a campaign
  that finished with twelve, missing the two most consequential. A final pass
  after the merge queue is satisfied costs minutes and is the difference
  between a record and a draft.
- **"This cannot be measured" is a claim about the tree, and decays like one.**
  The controller asserted an ablation was unavailable without changing the
  scorer; it needed no change at all and read null. Check it with whoever is
  holding the code, rather than asserting it from the plan.
- **Before telling a task to leave a test red, grep the sub-floor roster.** If
  the gate you also demanded runs that test, the instruction is
  self-contradicting and the implementer has to spend a round discovering it.
