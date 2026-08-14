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

**F5 (new, from this task) — `docs/README.md`'s enumeration problem has a
sibling in the heavy-tier roster's prose.** The roster's doc comment said the
preregistered-not-met class was "here three times" while the array held four,
because the campaign's own Task 4 added one and did not update the sentence.
Prose that counts a list next to the list is a duplication that rots on the
first addition; the count is now five and correct, but the shape will recur.

## 8. Two incidental measurements worth not re-deriving

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
