# Retrospective — The Gnomon

**Merged:** 2026-08-14

Process lessons, not product. The product is in
[the chronicle](../../book/src/chronicle/the-gnomon.md): a first-occurrence
index shipped as nineteen census columns, an anomaly report whose preregistered
usefulness claim was **falsified** at recall@10 = 0.5667 against a 0.60 bar, a
seventh registry status (`refuted`) whose first user was this campaign's own
headline, and two enum variants found unreachable in a thousand worlds by
accident.

Seven tasks, one mid-campaign census, one close absorption of 76 commits.
This campaign's scratch dies with its checkout; everything below was promoted
out of it first.

## 1. The one real defect was in the plan's own text, again

Task 5's plan told the implementer to `assert!(src.contains(OLD))`, substitute
a constant inside `windows/lab/tests/anomaly_injection.rs`, and measure the
result. **A compiled test binary cannot change the constants it was compiled
from.** Source substitution takes effect only after a rebuild, which no
`#[test]` can perform on itself.

This was not caught by reasoning about it. It was caught by looking up how the
repo already mutates source: `tools/seam-guard` is a standalone binary
*outside the cargo workspace*, and it is outside for precisely this reason.
The plan's own next step pointed at `make seam-guard-list`, so the intent was
right there; the mechanism sentence simply did not follow it.

That is the project's standing finding — **every defect this campaign shipped
originated in plan text I wrote** — and it recurred despite the plan carrying
a self-review section that declared "no placeholders". A self-review checks
for *absence*; this defect was a confident *presence*. Three earlier
identifier defects in the same plan (`Seed::new(42)` for `Seed(42)`,
`metrics()` for `registry()`, twice) were found the same way, in three
minutes, by grepping every named identifier in the plan against the tree.

**Do differently:** grep the plan's *mechanisms* against the tree, not only
its identifiers. "Can the thing I am asking for physically happen in the
process it runs in?" is a question with a cheap answer and the plan never
asked it.

The replacement — a host-pinned authoring script producing committed fixtures,
read cheaply forever — is the shape `scripts/census-run.sh` already uses. When
a mechanism has to be replaced mid-plan, look for the existing instance of the
shape before inventing one.

## 2. A reviewer's counter-argument was better than my framing, and the
correction is worth more than the fix

I ruled that a test passing by arithmetic coincidence was "worse than no
test". The reviewer disagreed with the reasoning while accepting the fix:
worse-than-no-test means a guard that can *never* fire and therefore masks a
real regression, and that was not this case — every mutation was still killed
by something committed. The real defect was narrower: the test's **name**
overclaimed, and the overclaim would only bite after a specific future
refactor.

The correction stands, and it changes what the fix is for. I then overruled
the reviewer on *timing* (it proposed a follow-up; the roster was being frozen
into a census golden in the next step, so a follow-up would have had to argue
its way back past a green suite and a committed golden).

Then the fix itself overturned my instruction. I told the implementer to key
the test on a species where the filtered and unfiltered minima genuinely
differ. **All fifteen `occ-people` objects on seed 42 tie the unfiltered
minimum at 0.0**, so no species-keyed test could ever have discriminated a
dropped object filter, and my instruction was unsatisfiable. The implementer
probed, found that out, and pivoted *predicate* rather than species. This is
exactly why the plan forbids prescribing a mutation from outside the code, and
it is now the second time in this campaign that a confident instruction
written without reading the data was wrong.

## 3. A red gate is a sequencing decision, and it was worth escalating

Adding one metric reddened **43 tests** — all one shape (`rows.csv header does
not match study 'the-census' schema`), none a world-content mismatch — and all
43 stay red until the census is regenerated on lefford. The prior campaign's
remembered figure was "~34"; the plan declined to carry the estimate and
measured instead, which is the only reason the number is right.

The consequence was structural, not cosmetic: Tasks 3–7 would have run against
a 43-red baseline in which any *new* breakage is invisible. Nathan authorised a
census refresh immediately after Task 2 rather than at close, which restored
the gate as a signal for five tasks (3507 run / 3507 passed). Raising it beat
running red to close and comparing each suite result against a frozen list of
43.

**Estimate delta worth recording:** that mid-campaign census cost 920.212 s
(cpu_ratio 29.43 on 40 cores) against 949.579 s for the run before it — the
nineteen new columns are free at census scale, as the plan predicted from the
measured 8.3 s-per-world build cost. Adding columns to the census is cheap;
adding *worlds* is not.

**And the additivity was proven rather than asserted.** The `rows.csv` diff
was 2002 lines on a 1000-row table, which is the shape that should make you
stop. A line count cannot answer whether an epoch just happened; a shared-
column diff can. 206 → 225 columns, 19 added, none removed, seed sets
identical, **0 of the 206 pre-existing columns moved**.

## 4. The convention for recording a falsification has a rot, and it is
one assertion wide

The project's way of recording a preregistered-but-not-met result is
`#[ignore = "PREREGISTERED, not met: awaits <registry-slug> (<reason>)"]`, with
the reason rostered in `cli/tests/heavy_tier.rs`. There are now five.

**An ignored measurement stops being measured.** The figure quoted in the
reason is a claim nothing re-derives: change `REPORT_SIZE`, the tail-depth bar
or the scorer, and the published number silently becomes fiction with nothing
going red. None of the file's other tests pinned it.

The remedy is deliberately *not* a sixth ratchet — the campaign's Global
Constraints forbid that, and the essay it implements exists because five tools
already instantiate one ratchet pattern. It is one always-running assertion
beside the ignored test, pinning the measured integers (`hits == 68`,
`counted == 120`) as a **witness rather than a claim**: moving it means the
falsification was re-measured, and the chronicle, the registry row and the
roster entry must be re-read and re-stated in the same commit. It was
mutation-proved (`REPORT_SIZE` 10 → 6 takes it to 62/120 and reddens it; clean
revert) rather than assumed non-vacuous. The roster's doc comment now asks the
next entry to carry the same.

## 5. A decision rule that covers one failure direction covers neither

Task 3's demonstration query was given a decision rule for returning **zero**
rows. It returned **991 of 1000** — near-universality, the opposite failure,
which undercuts "an intractable search made cheap" just as effectively and had
no rule at all. The honest response was to reframe onto the half that stays
useful when the intersection is dull (`replay_from`, the world-time a replay
must start at: 58 distinct values across the matching seeds) rather than to
pick a more selective conjunction after seeing the result.

**Do differently:** when writing a decision rule for a measurement, write both
tails. "Zero rows" and "every row" are both failures of a selectivity claim.

## 6. The census was verified current at close, not refreshed — and this is
the evidence

The plan's Task 7 prescribed a second census refresh. It was **deliberately
skipped**, and the skip is recorded here rather than left silent because a
~950 s no-op is worth not paying and worth being able to justify later.

- Before the close absorption:
  `git diff --stat 5eb5d5f5..HEAD -- domains/ kernel/ windows/worldgen/
  windows/lab/src/metrics.rs windows/lab/src/roster.rs` was **empty** —
  nothing that produces a metric had moved since the census was authored.
- After absorbing 76 commits of `main`, it was **not** empty: The Holdfast's
  `perf(worldgen)` change (elevation binds first, so three `exp` calls need
  not happen) is a generative-path edit.
- That change is nonetheless byte-identical on the census path, and the proof
  is live rather than inherited: The Sexton's census sentinel rebuilds the
  census's first three worlds against all 222 committed columns, with **zero
  waivers**, and it is green on this branch after the absorption.

A three-world sentinel is not a thousand-world census, and it says so in its
own module doc. It is, however, the difference between "a chronicle told me
this was byte-identical" and "I watched it be byte-identical here". **The
decision to skip or refresh is the controller's; the evidence is above.**

## 7. Followups promoted out of the dying scratch

**F1 — `normalize_status` validates a row's *former* status.**
`cli/tests/docs_consistency.rs` takes the *head* of a transition arrow;
`normalize_status("rejected → ratified") == "rejected"` is an asserted test.
So every row written as `<old> → <new>` has its `<old>` policed and its
`<new>` never looked at. A row could read `shipped → nonsense` and pass. Found
while deciding whether `shipped → refuted` could express "shipped the code,
refuted the prediction"; it cannot, for this reason. **Deliberately not fixed
here**: correcting the head-taking silently re-validates every existing arrow
row under a rule they were never checked against, which is its own
campaign-sized blast radius.

**F2 — the frozen census columns are a finding in their own right.**
`hue-depth-goblin`, `lifespan-years-*`, `core-homophony-*`,
`channel-connectivity` and thirty-odd others are `min == max` across all 1000
worlds. This campaign only *excludes* them from the anomaly report, and
Domesday D2 already reports them, so nothing is lost — but a column that never
varies across a thousand worlds is either a metric measuring a constant or a
generator that is not generating, and nobody has walked the list to say which
is which, per column.

**F3 — the first-occurrence roster will drift as the history bake grows.**
The roster is frozen at ≤40 columns precisely so the census schema cannot
become seed-dependent, which means a predicate added to the bake later will
silently not be indexed, and nothing checks for that. Candidate: a
default-deny scan asserting every predicate observed with a non-genesis day in
the seed-42 fixture is either in the roster or in an explicit exclusion list —
which should **extend an existing ratchet**, not become a sixth.

**F4 — the injection fixtures have no staleness alarm beyond a column-set
check.** `windows/lab/tests/fixtures/injection/` is authored by
`scripts/gnomon-injection.sh`, which mutates tracked source and restores it;
that is why it is excluded from `docs/generated-paths.txt` (the artifact sweep
must never mutate source). The committed test asserts the fixture column set
against the census's and fails loudly on a mismatch, catching a refresh that
*adds or removes* columns — but **not** one that alters an existing column's
*values* while leaving the schema intact. In that case the fixtures still
parse, still score, and quietly measure recall against a census the perturbed
rows were never generated alongside. Candidate fix, not taken here (it is a
heavy-tier authoring check, and a fifth ratchet is forbidden): re-run the
*baseline* arm live in the heavy tier and byte-compare it against the
committed baseline fixture, extending `fixture_staleness.rs` rather than
adding a file.

**A warning for whoever builds F4's check: the obvious mutation proves the
wrong guard.** The natural way to test a fixture-staleness assertion is to
drop a column from an injection fixture's `schema.json` and confirm something
reddens. Something does redden, and it is not the assertion under test —
`census::load` runs its own schema/CSV agreement check first and fires there,
so the mutation goes green-to-red for a reason that has nothing to do with
staleness and proves nothing about it. To reach the staleness assertion you
must keep the loader satisfied: **rename a column consistently in *both*
`schema.json` and `rows.csv`**, which loads cleanly and then fails the
staleness check with its own message. Recorded because the failure mode is
invisible — the mutation "works", the test goes red, and the guard is
pronounced non-vacuous on evidence that never touched it. This is the
`five-vacuous-guards` shape one layer up: not a guard that cannot fail, but a
mutation that reddens a *different* guard than the one it is crediting.

**F5 (new, from this task) — `docs/README.md`'s enumeration problem has a
sibling in the heavy-tier roster's prose.** The roster's doc comment said the
preregistered-not-met class was "here three times" while the array held four,
because the campaign's own Task 4 added one and did not update the sentence.
Prose that counts a list next to the list is a duplication that rots on the
first addition; the count is now five and correct, but the shape will recur.

## 8. Four incidental measurements worth not re-deriving

**Cross-host byte-identity held on a metric surface that did not exist when it
was last audited.** The injection battery was authored twice — once on
aarch64/Darwin as a pilot, once on x86_64/Linux for adjudication, with **no
generative code changed between the two authoring SHAs** (verified, not
assumed: the only files that moved were `docs/timings.md`, the test file and
the fixtures themselves). Eight arms, 222 metric columns × 20 seeds each, and
the two authorings differ *only* in the manifest's `host` and `sha` fields.
Every `rows.csv` and `schema.json` is byte-identical.

**A constant's blast radius is not predictable from its domain.** Perturbing a
terrain *lithology* threshold moves *naming* columns — name length, syllable
counts, toponymic roots, collision rate — in all twenty seeds; so does a
religion constant. The coupling runs through the shared draw sequence, not
through anything inferable from the file the constant lives in. Any estimate
of "what could this constant affect" made by reading its domain will be wrong
in this direction, and this campaign's recall spread is partly an artifact of
it: an injection that disturbs a downstream draw moves many columns and gets
many chances to place one in a ten-slot report.

**A second estimate delta, and it went the same way as §3's.** The plan
projected H2 at **~28 minutes** — "200 world-builds at the measured 8.3 s each
is ≈28 minutes single-threaded" (`docs/superpowers/plans/2026-08-13-the-gnomon.md:774`).
It ran in **170.6 s** locally on ten cores and **136.8 s** on lefford. An
order of magnitude, in the safe direction, and the whole of the error is the
word *single-threaded*: the projection multiplied a per-world cost by a world
count and never divided by the cores the run would actually get. Worth
recording alongside §3's census delta because the two together say the same
thing from opposite ends — a cost model built by multiplying out a unit
measurement is wrong in whichever direction the parallelism you forgot
happens to point, and this campaign got one over-estimate and one
under-estimate out of the same habit. Cheap to fix: state the core count in
the estimate, or state that the estimate is a ceiling.

**The settlement rung is where `is-settlement` finishes, measured rather than
assumed.** `is-settlement` carries **230 facts and 62 distinct non-genesis
days at both `BuildDepth::Settlements` and `BuildDepth::Full`** — identical on
both numbers, so nothing after the settlement rung touches those facts. The
consequence a future optimizer will want: `Extractor::Full` on the nineteen-
column first-occurrence roster is required by **roster uniformity and the
`extract` helper's Full-only panic**, not by the metrics themselves. Cutting
the roster to the settlement rung is therefore a question about the extractor's
contract, not a question about whether the data survives — and the probe that
would otherwise have to be rebuilt to learn that is the one recorded here.

## 9. The close absorption

76 commits, two campaigns (The Holdfast, The Sexton). One conflict, and it was
structural rather than textual: The Sexton restructured
`scripts/regenerate-artifacts.sh` from a sequential list into a dependency DAG
while this branch had appended `lab anomalies` to the old serial tail.
Resolved by taking main's structure wholesale and re-attaching the anomaly
report as a **serial trailer** after `lab domesday` — not as a Group B+C
`spawn`, because it reads the schema the backfill loop writes, which is the
same dependency that keeps `domesday` out of the parallel groups. Spawning it
would have raced the file it reads, and the tests would not have caught it
reliably. The DAG's header comment was updated in the same edit so the
schedule it documents still matches the schedule it runs.

## 10. Two deferred minors, and where the close found them

The close walk (`closing-a-campaign` step 2) is a sweep of the dying scratch,
and it earned its place here: `F1`–`F5` had all been promoted into §7 by the
task agents, but two items marked `minor (deferred)` in the SDD progress
ledger had no home anywhere, and one of them was still live in the tree.

**The stale comment, fixed at close.** Task 2's new iron test made a
neighbouring doc comment untrue: `metrics.rs` claimed that no other test
reached `first_day`'s comparison loop with the object filter active, and after
the iron test landed, one did. The fix diff never touched that line, so nothing
went red — a doc comment is not a test, and no check reads one. The task agent
flagged it in the scratch with the right instinct, quoted here because the
instinct is the transferable part: *a campaign about stale self-description
should not ship a comment that just became untrue.* It is corrected now, and
corrected by naming both tests and why they are complementary rather than by
deleting the sentence, so the next reader inherits the reason instead of a gap.

**The spec's stale quotation, deliberately left.** Task 6's grep-hit list for
the `six`→`seven` status change omitted this campaign's own spec, which still
quotes the pre-change strings. Left as-is: a spec is a historical record of
what was believed when it was written, and back-dating one destroys the very
provenance it exists to carry. Recorded here rather than silently dropped,
because the reviewer's objection was correct in shape — silently dropping a
genuine grep hit is the same move as the defect `R5` was about (§11, which
states `R5` rather than assuming you can look it up: it was a label in the
git-ignored scratch ledger this section is about, and would have died with
it) — and the answer is "this hit is out of scope by policy", not "this hit
does not exist".

**The process point.** Both were found by walking `.superpowers/sdd/` line by
line at close, not by remembering the campaign. A `minor (deferred)` line in a
git-ignored progress ledger is indistinguishable from a fixed item once the
worktree is gone, and neither of these appeared in any task's final report —
they appeared only in the running notes. Grep the scratch for `deferred` and
`minor`, not just for `follow-up`.

## 11. Three rulings the close had to state, because their labels died with
the scratch

Each of these was decided during the campaign and recorded only under a
label (`R5`, "Task 6 vs Task 7") in `.superpowers/sdd/`, which is git-ignored
and per-worktree. A cross-reference to a label that exists nowhere committed
is worse than no cross-reference: it reads as a pointer and resolves to
nothing. So they are stated here in full rather than named.

**R5 — an enumeration is an assertion of completeness, and this one was short
by one.** The spec's §4.3 list of consumers of the registry's status
vocabulary carried the parenthetical "grepped; this list is complete". It was
not complete: `docs/CLAUDE.md:58` read *"Status is one of the six documented
values"* and was absent from it. The original grep had searched a hand-picked
set of paths and that file was not among them; it surfaced only because the
file later loaded into context for an unrelated reason. Re-grepping the
**claim** rather than the paths — the token "six" near "status", repo-wide —
found it at once, and established that there was no fifth consumer outside
historical plans and retrospectives, which are not edited. The line now reads
"seven".

Why that is a ruling and not a typo fix: the failure is structural. An
enumeration asserts completeness, and enumerating from remembered paths
cannot support that assertion no matter how careful the remembering is. Left
alone, a governing document goes on saying "six" while the vocabulary holds
seven — precisely the stale self-description this campaign exists to attack,
in the campaign's own diff.

**The transferable half, which appears in no other committed file: ship the
grep, not the list.** The fix was not "add the missing path to the spec's
list" — it was to change what the dispatch to Task 6 handed the implementer.
A dispatch carrying an *enumeration* asks the implementer to trust the
author's completeness, and they have no way to check it short of redoing the
work. A dispatch carrying the *grep that generated it* makes them re-derive
completeness as a side effect of doing the task, and the derivation is
cheap — they were going to open those files anyway. The list is the output of
a query; hand over the query. This generalises past greps to any
"I checked all of X" claim in a brief: state the operation, not its result.

**PROC-refuted-status is `ratified (0131)`, not `shipped`.** Task 6 argued
for `ratified (0131)` on the precedent of `GRAIN-inheritance-policy |
ratified (0121)`; Task 7 wrote `shipped`, overwriting the argument rather
than answering it. Decided here on the precedent, which is real on both
sides — 14 rows read `ratified (NNNN)` and 11 read `shipped` while citing a
decision in **Where** — and the line between them is *what the row
delivered*. `ratified` marks a row whose own content became the rule
(`MAP-5` → 0009 models-author-dice-roll, `PROC-2` → 0020 retrospectives,
the `GRAIN-*` family → 0121–0124); `shipped` marks a row that delivered a
mechanism which a decision merely sanctions or scopes (`SKY-1` cites 0054 for
a branch it *declined*). `PROC-refuted-status`'s deliverable is a
vocabulary term and a semantic distinction — a rule — enforced mechanically
the way `PROC-2`'s is. Row set to `ratified (0131)`.

**CLIENT-depth-follows-content is `shipped`, not `raw (mechanism amended)`.**
Task 6's audit flagged the status cell as stale but ruled re-labelling out of
its own scope. Verified here against The Lintel's chronicle rather than
inherited: below the walk band "existence is a **predicate**", derived from
"what the committed history says stands on this ground", and the amended
mechanism is stated as shipped — "the band changes only at a threshold, and
thresholds are always visible", with lateral movement never changing band.
Both halves of the row's claim are live, so the row is `shipped` and its
**Where** now points at the chronicle as shipping evidence rather than as the
source of an amendment.

## Appendix A. The `refuted` reclassification audit — all 47 candidates

The spec (`docs/superpowers/specs/2026-08-13-the-gnomon-design.md:397`) states
that **"the enumeration is the reviewable artifact"**, but only the *result* —
exactly one row reclassified — reached a committed file. The enumeration
itself lived in the campaign's scratch. It is reproduced here in full so that
the next person asking "why isn't `SOC-criticality` refuted?" gets an answer
instead of a 47-row audit to redo.

**Admission rule, frozen before the audit ran:** a row takes `refuted` only if
**its own central claim was tested and found false, and no artifact shipped
from it.** Explanatory findings about *other* rows' falsifications,
forward-looking or untested proposals, and rows that shipped a mechanism (even
alongside a falsified sub-prediction) are all excluded by that rule. Applied
to 47 candidates it admitted **one**, well under the 12-row cap the plan
allowed — which is the finding: the greppable `refuted` record begins with the
next falsified campaign rather than being back-filled.

"Current status" is the status at audit time (2026-08-13); two cells have
since moved by the rulings in §11 above.

| # | ID | current status | verdict | one-line reason |
|---|---|---|---|---|
| 1 | MAP-desire-path-is-not-a-readout | elaborated | reject | names Tumult/Tithe's falsifications as a caution for a *new, not-yet-tested* prediction of its own |
| 2 | MAP-biconnectivity-is-a-feudalism-knob | elaborated | reject | preregistered but not yet run |
| 3 | MAP-16 | elaborated | reject | describes an in-fiction unfalsifiable-prophecy structure; not a claim of this row's own that was tested |
| 4 | MAP-63 | raw | reject | a proposed future campaign, not yet built or tested |
| 5 | LANG-49 | shipped (floor…) | reject | already carries its citation; mechanism shipped, so it stays `shipped` under the rule |
| 6 | LANG-51 | raw | reject | not built |
| 7 | LANG-52 | raw | reject | not built |
| 8 | EXP-11 | raw | reject | grep false positive — "overturning of hierarchy" is Bakhtinian carnival, not a falsification |
| 9 | PSY-11 | elaborated | reject | a large compound row, almost entirely already-shipped sub-mechanisms; "overturned" refers to one internal redesign inside shipped work, not a refuted central claim |
| 10 | SOC-criticality | shipped (slices 1–2…) | reject | already carries its citation; shipped code exists (slices 1–2), stays `shipped` |
| 11 | SOC-multi-axis | shipped | reject | the position mechanism shipped even though its prediction failed |
| 12 | SOC-flagship-selection | raw | reject | explains why an *earlier, different* preregistered hypothesis (kobold vs. goblin coastal flagships) failed; this row's own claim (re-selection as an identity artifact) is a true finding, not a refuted one |
| 13 | CUL-20 | raw | reject | proposes a metric to make something "currently unfalsifiable" measurable — not yet run |
| 14 | CUL-founding-order-religion | elaborated | reject | explains the cause of a *different* campaign's (The Terminator's) falsification; this row's own diagnosis stands as accepted, not refuted |
| 15 | BIO-19 | raw | reject | forward-looking ("cheap and falsifiable"), not yet tested |
| 16 | BIO-climate-links-untestable | raw | reject | its own text says "not a refutation" |
| 17 | BIO-40 | elaborated | reject | layered, still-active idea (a built-but-undeployed replacement, then a repointing that found the diversity debt lies elsewhere); no single clean falsified claim with nothing shipped |
| 18 | BIO-rung-weighted-concentration | raw | reject | diagnoses an *instrument* blind spot in a prior test, not a refutation of this row's own claim; proposes a future re-measurement |
| 19 | **BIO-affinity-level-is-two-quantities** | rejected | **RECLASSIFY** | own claim ("affinity level is two quantities") tested via a lambda sweep and found to be a measurement artifact; verdict LEAVE means nothing shipped from the two-quantities idea — clean fit |
| 20 | MEM-founder-handle-epoch | shipped | reject | the *narrowing* alternative was refuted, but the *widening* mechanism shipped |
| 21 | ALCH-7 | shipped | reject | this row's own split-latent/manifest claim shipped (the `hue` formula); it overturned a *different* row's (ALCH-2) mitigation |
| 22 | TOOL-24 | raw | reject | huge ongoing perf backlog; the one "refuted" premise sits inside already-shipped work (The Millrace) |
| 23 | TOOL-packed-roomaddr | raw | reject | states a falsifiable claim for a *future* campaign to test |
| 24 | TOOL-choice-idioms | raw | reject | proposes a future measurement; "a null is the interesting result" is a prediction, not a finding |
| 25 | TOOL-component-convergence-rates | elaborated | reject | preregistered, not yet run |
| 26 | TOOL-kernel-vec3 | raw | reject | "refutes" a *different* rule (no-sibling-dependencies) elsewhere; this row's own claim (triplicated code) stands true |
| 27 | MAP-perceive-apertures | elaborated | reject | grep false positive — "0 overturns" is an ideonomy-pass count, not a falsification |
| 28 | MAP-connectors-as-apertures | raw | reject | grep false positive — same "0 overturns" pattern |
| 29 | CLIENT-turn-cost-ratchet | shipped (ratchet half) | reject | shipped mechanism; not a refuted-with-nothing-shipped case |
| 30 | CLIENT-fine-layer-live | shipped | reject | the overall feature shipped (room/furnishing/v1 live); one sub-prediction (warmth) came back null |
| 31 | CLIENT-depth-follows-content | raw (mechanism amended) | reject | the amended (threshold-based) mechanism shipped via The Lintel — confirmed by checking the chronicle ("The Lintel made compass movement refuse indoors... shipped a week earlier"); status cell itself looks stale but re-labeling it is out of this task's scope |
| 32 | CLIENT-four-channels | raw | reject | this row's *own* claim (weight carries trust) is what overturned an *older* design; its own claim stands, unrefuted |
| 33 | CLIENT-substance-is-a-surface-not-a-swatch-set | raw | reject | only *partly* falsified by its own text ("the classification is still 2-D... survives") — not a clean whole-claim refutation |
| 34 | GRAIN-inheritance-policy | ratified (0121) | reject | already settled via decision 0121; a sub-thesis was falsified en route but the row is already at its terminal status |
| 35 | KNOW-grid | elaborated | reject | states two falsifiable predictions, not yet measured |
| 36 | SKY-11 | raw | reject | the "correction" was a design realization before any code existed (aurorae are "missing entirely") — no empirical test occurred |
| 37 | SKY-dynamo-and-the-null | elaborated | reject | describes a *predicted* null result for an unbuilt mechanism, not a tested-and-failed one |
| 38 | CLIM-substrate-integral | shipped | reject | the recurrence model shipped even though its 5%-floor prediction failed |
| 39 | CLIM-daily-precip | shipped | reject | this is the row that was *not* falsified — confirmed at full study scale |
| 40 | CLIM-cost-not-passability | shipped | reject | shipped mechanism; median-swing prediction failed but the 99th-percentile one held |
| 41 | DESIGN-weather-cannot-be-routed-around | shipped | reject | shipped (The Fare); its own hypothesis was overturned but the mechanism is live |
| 42 | PSY-expertise-per-individual | elaborated | reject | its own text says it "cannot currently be falsified in simulation" — a structural gap, not a tested claim |
| 43 | KNOW-proxy-inference | raw | reject | grep false positive — "refutation of a causal hypothesis" describes an in-world example scene, not this row's own claim |
| 44 | PROC-board-cross-host | shipped | reject | the mechanism shipped (The Beacon); only its timing budget was falsified |
| 45 | PROC-readout-is-not-identity | shipped | reject | rung 5 shipped; the finding about kind-removal is a discovered limit, not a refutation of the row's own claim |
| 46 | PROC-comments-are-the-falsification | raw | reject | states an accepted, already-validated process lesson, not something tested and found false |
| 47 | PROC-refuted-status | raw | reject (not reclassified as `refuted`; its terminal status is settled in §11) | this is the row that *proposed* the feature this task ships — its own claim wasn't refuted, it was adopted |

**Result: 1 of 47 qualifies.** `BIO-affinity-level-is-two-quantities`:
`rejected` → `refuted (The Muster)`.
