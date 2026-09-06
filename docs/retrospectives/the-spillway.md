# Campaign The Spillway — retrospective

**Close:** 2026-09-06, awaiting G6 · **Ledger:**
[`2026-09-06-the-spillway.md`](../superpowers/ledgers/2026-09-06-the-spillway.md)
(entries #0–#3 plus five task sections, a close digest, and two fix-wave
sections) · **Chronicle:**
[the-spillway](../../book/src/chronicle/the-spillway.md) · **Decisions:** 0836

## The headline: the registry row named two legs and the delivery log had three

The predecessor filed the deadlock as a registry row, and the row is accurate
about what it says: the commit gate's fixture-columns test reds when the
census gains a column, and the arms' authoring script refuses the delivery's
own dirty tree. Two legs, and a fix that follows from them.

The queue's own log carried a third. The test that failed **first** in The
Warp's refused delivery was not the fixture-columns test at all — it was the
column-count witness whose name carries the count
(`evaluable_columns_measured_surface_on_the_<N>_column_census`), selected by
exact name out of the sub-floor roster, and unfixable by any machine because
its whole content is that a person re-measured and wrote down what moved.
A design that had shipped the row's two legs would have re-authored the arms
correctly and been refused anyway, by a test nobody had budgeted for.

Reading the log is what found it, and reading the log took a minute. The
generalisation is not "registry rows are unreliable" — the row was a good
row. It is that **a row is a shelf-mark and the delivery log is the evidence**,
and a campaign whose whole subject is a recorded failure should start from the
recording.

## The rule, not the list

The two checks the delivery already stood down had each been justified
separately, in prose, at the site. This campaign found the sentence they were
both instances of and wrote it into a decision: *a delivery satisfies every
check whose remedy is a regeneration, and defers only a check whose remedy is
a human re-statement.* That sentence places the third and fourth checks
without arguing about them, and it is what a fifth will be placed by.

Worth noticing what made the rule findable: the two existing opt-outs each
carried a *reason* at the site, not just a flag. A stand-down recorded as
`# skip this here` would have generalised to nothing.

## Do differently next time

1. **Read a harness's own guards before deciding how a script degrades.** The
   lock decision's first draft had the step degrade to unlocked on a host
   without `flock`, on the belief that the shell test harness runs on this
   Mac. It does not — that harness's *first* line is a `flock`-absent SKIP.
   Reading it turned "degrade" into "refuse" before a line of code existed,
   and removed a second locking story written for a caller that does not
   exist. The correction is in ledger #3 and cost nothing because it happened
   at design time; the same belief acted on at implementation time would have
   shipped.
2. **A reconciliation row is owed the moment a record file exists, not at
   close.** This campaign's very first commit — the spec and the ledger —
   was refused by its own gate for a missing `campaign-reconciliation.tsv`
   row. The row is not a closing artifact; it is the index entry that says a
   record file has an owner, and the gate is right to demand it as soon as
   the file lands.
3. **A decision must be minted before the first *script* comment cites it.**
   The plan staged the decision record for Task 5 on the reasoning that a
   Rust comment cannot cite an unminted number. The cite scan reads `scripts/`
   too, and the Task 2 commit was a scripts-and-docs commit, so it was checked
   at Task 2 — three tasks early. Ruling: mint the record then. The lesson is
   narrower than "mint early": **check what the citing gate actually scans**,
   because a constraint derived from one file extension silently mis-schedules
   every other.
4. **A fixture missing an authority file makes the classifier that reads it
   return zero, silently.** Task 3's first RED run failed for a reason the
   brief had not predicted, and the cause was three layers back: the census
   test fixture had never carried a `docs/generated-paths.txt`, the path-author
   lookup tolerates a missing file by returning an empty author rather than
   failing, and so `census_golden_count` had read **0** on every moving-census
   run that test file has ever driven. Nothing noticed, because until this
   campaign nothing *depended* on the count — three arms had been passing
   vacuously. Two rules come out of it: a tolerant lookup over a missing
   authority file needs a positive control, and a fixture is only evidence for
   the case it actually reproduces.
5. **A lint that passes locally proves only what the local version checks.**
   The stage gate went red at the outboard phase on a single shellcheck style
   finding (SC2002) that this Mac's shellcheck 0.11 does not report and
   lefford's 0.9.0 does. `make shellcheck` was clean locally the whole time,
   and `make gate-commit` does not run shellcheck at all — so a scripts-only
   change is **unlinted until the chamber takes the box**. Run the box's own
   version before submitting a scripts-only change. (The gap is filed as
   `TOOL-out-of-workspace-crates-have-no-local-lint`.)
6. **Re-derive every figure at close; never restate a drafting-time number.**
   The spec's verification block recorded 285 name keys and 284 columns. By
   the close the trunk had moved six campaigns and two census-schema changes:
   290 CSV columns, 287 metric columns, and the count witness renamed
   accordingly. Every number in the chronicle and this document was computed
   from the tree at close, and the spec's are labelled as what they are —
   measurements with a date.
7. **Check a figure by running the code, not by reading it.** Re-deriving the
   column counts for this document is what surfaced the campaign's own live
   defect (below). Nothing in five task reviews had run the staleness
   comparison against the two real files; every test of it ran against
   synthetic ones.

## The defect the close found, and fixed, and why five reviews missed it

`injection_arms_stale` reported **all eight arms stale at this campaign's own
tip**, by exactly one column each way — and the two columns are the study
names (`the-census` versus `gnomon-injection`), which the extractor was
written to skip.

The extractor skipped the study-name line by indentation, on a stated belief
(in the spec, and repeated in the Task 2 ledger section) that the study's own
name sits at indent 2 while column names sit at indent 6. In the real serde
output the study name sits at indent **4**, nested under a `"study"` object,
and the rule `^ {4,}` caught it too. Consequences, in order of size:

- The "arms unchanged" branch was **unreachable**. Every census delivery
  re-authored the arms, including a null one.
- It failed in the safe direction — a mis-parse can only cause a needless
  re-authoring, never a missed one, which the ledger states as the design's
  own safety argument — and the arms *were* current at this tip (manifest
  `sha=d2bd513f1`, The Lot's delivery).
- The test arm written to catch this (`the study-name line is ignored on both
  sides`) **passed**, because `write_schema` in the test file emitted the study
  name at indent 2 — the shape the belief predicted, not the shape the
  pretty-printer produces.

That is the *same failure mode* as finding 4 above, in the same campaign, one
file over: a fixture that does not reproduce the real file's shape reads
exactly like coverage. The campaign found it once by running RED and reading
the failure, wrote the lesson down, and then shipped a second instance of it
in a test that had been green from its first run.

The verification that would have caught it is one command against the two
real files, and it was never run at drafting time: the spec's own drafting
block ran `grep -c '"name":'` (no indent filter) on both files, saw a
one-line difference, and *explained* it rather than testing the explanation.

**Fixed before merge, in the campaign's own fix wave (one commit, four
changes).** `census_schema_columns` now anchors on exactly six spaces
(`^ {6}"name": "`) rather than `^ {4,}`, verified against every committed
schema.json at this tip (`grep -c '"kind":'` and `grep -c '^      "name":'`
both read 290, on the census and each of the eight arms; the study's own
`"name"` is the sole indent-four hit, one, on every file). `write_schema` in
`scripts/test-sluice-census.sh` now emits the real shape — a `"study"` object
at indent two, its `"name"` at indent four — so the "ignores the study's own
name" test arm can actually discriminate. Three controls were added: a
fixture-shape control pinning `write_schema`'s assumption to the real
committed files, a real-tree control asserting `injection_arms_stale` over
the actual checkout prints nothing (the same fact
`anomaly_injection::the_fixture_columns_match_the_census` asserts in Rust, so
it cannot be vacuous), and a positive control that deletes one column from a
copied real census schema and confirms the drift is caught, with the exact
expected message. A census of this tip is now expected to report `Gnomon
arms unchanged` on a null.

## Confidence Gradient

N/A, no bet moved. This campaign is tooling: it changes what a delivery
commit carries, not what the world is or what is known about it.
`book/src/open-questions.md` is untouched, deliberately.

## Estimate deltas

| | planned | actual |
| --- | --- | --- |
| tasks | 6 | 6, plus two fix waves (the close-found column-extractor Critical, then the final whole-branch review's prose-truth findings); two absorptions of the trunk |
| decisions | 0836–0845 reserved | **1 minted** (0836), three tasks earlier than planned |
| checks stood down under `HV_CENSUS_DELIVERY` | 2 → 3 | 3, pinned by a test that fails on a fourth |
| production proof of the growing-census path | out of scope (no ref registers a metric) | still out of scope; the stubbed harness is the whole evidence |
| defects found by running versus by reading | — | every one of them: three in Task 3's RED run, one at the close's figure re-derivation |

## Deferred minors

| item | disposition |
| --- | --- |
| shellcheck SC2329 on `sluice-census.sh`'s `release_census_row` (local shellcheck newer than lefford's) | **closed in Task 3** by adding SC2329 to the existing SC2317 disable directive |
| a Task 2 ledger sentence says 38 inserted lines where the diff shows 40 | accepted close minor; cosmetic, and the sentence is a narrative not a count anything reads |
| `sort` without `LC_ALL=C` in `census_schema_columns` | **closed in Task 3**, pinned while editing the file |
| `LC_ALL=C` is pinned on that `sort` but not on the paired `comm` calls in `injection_arms_stale` | **closed in the fix wave** — both `comm` invocations now carry `LC_ALL=C` alongside the sort |
| same-second log collisions can let the corroborating `re-authoring` grep in the failure arms match a previous run's bytes | **closed in the fix wave** — `test-sluice.sh` now records each arm's log path and byte offset before its run and greps only the tail written past that offset; both arms still carry their second independent assertion (rc=4, no branch) too |
| the post-authoring `git add` of the fixtures swallows failure (`\|\| true`) and the second `add -u` is unbounded | accepted close minor; the hardening is to assert the staged set stays within `book/`, `docs/` and the fixture directory |
| `exec 9>"$arms_lock"` is unguarded — a redirection failure exits 1 with no `ARMS NOT RE-AUTHORED` message | accepted close minor; the message is the only loss, and a failure to open the lock path on the canonical box is not a case that has occurred |
| rationale prose is duplicated between `subfloor-roster.sh` and `pre-commit` | accepted close minor; polish, and the two-way agreement test reads the pattern out of the hook so they cannot disagree on the thing that matters |
| `windows/lab/CLAUDE.md:174` punctuation differs from the brief's text | accepted close minor; cosmetic |
| timings-label additivity for the new `gnomon-injection` label | **discharged in review**: the attestation groups only `sluice:` rows and `census_duration` filters `\| census \|`, so an added label is additive |
| `census_schema_columns` does not skip the study-name line on the real files, so the "arms unchanged" branch is unreachable and its test arm is vacuous | **NOT a minor — was the campaign's own live Critical defect, found at close and fixed before merge in the fix wave.** `census_schema_columns` now anchors on exactly six spaces (`^ {6}"name": "`); `write_schema` nests the study name where serde nests it, at indent four under a `"study"` object; a fixture-shape control, a real-tree control, and a positive control were added. A census of this tip is expected to report `Gnomon arms unchanged` on a null. |
| `test-sluice-census.sh` depends on `python3` (the positive-control script added in the fix wave) | accepted; precedented in `test-census-path.sh` and `test-pre-push.sh`, both of which already shell out to `python3 scripts/mutate.py` |
