# The Reservoir — retrospective

**Merged:** 2026-09-02

## The honest census: 11 controller-text, 2 implementer-prose, 1 implementer-code

This section originally opened by claiming every defect this campaign found
was in controlling-session text, full stop. **That claim was false, and false
in the flattering direction** — checked and corrected during this task's own
third fix round, after two of this document's own permanent records (decision
0606 and the chronicle) turned out to carry defects that were neither in the
spec, the plan, nor a dispatch, but in this task's own prose. **It was then
corrected a second time, in the other direction that matters: the
implementer-code count is one, not zero.** The final whole-branch review found
a code defect all six task reviews missed. The honest count:

```
  controller text (spec, plan, dispatches, ledger)   11
  implementer prose (decision + chronicle text)       2
  implementer CODE                                    1
```

**Each count is still a statement about what this campaign's reviews
*found*** — not a claim that any of the three is exhaustive. Two kinds of
prose were not clean: the controlling session's, and — new to this campaign's
accounting — this closing task's own. And the code was not clean either,
which is the more useful record: a campaign that reports one defect found from
a vantage its own review structure lacked says something a zero cannot.

**A related claim was also false, and it is worth naming separately because
it was supplied by the coordinator's own dispatch, not authored here.** An
earlier draft headlined "a ninth consecutive campaign with this
distribution," incrementing The Winze's stated "eighth" by one without
checking what landed in between. The Chattel merged between The Winze and
this campaign, and its own retrospective (`docs/retrospectives/the-chattel.md`,
"Defects by origin") tabulates 43 controller-prose defects against
**approximately 68 implementer-code defects** — the opposite distribution,
not a continuation of it — and its own text says "Four campaigns running have
reported the same distribution," not eight. The false claim would have gone
into a permanent record as a headline had it not been checked.

**Chattel's own methodological warning sharpens this record rather than
undercutting it, and is worth repeating here.** Chattel's controller count was
*complete* — every brief was verified pre-dispatch, so its defects were
enumerated exhaustively — while its implementer count was a *floor*, because
three tasks' reviews were never triaged into the ledger. "Read the
denominators before the ratio": the two sides of any such count are rarely
measured the same way, in any campaign, including this one. This campaign's
**one** implementer-code defect is a statement about what its reviews *found*,
not a bound on what its code contains — and the way it was found is direct
evidence for that reading, since it was invisible to six reviews that were
each doing their job correctly on the diff in front of them. An earlier draft
of this section headlined a zero here and hedged it carefully; the hedge was
honest and the number was still the wrong one to publish. No streak is
asserted.

### The one implementer-code defect

| # | defect, in implementer code | what caught it |
| --- | --- | --- |
| I | Task 5 migrated `windows/worldgen/src/lib.rs`'s `generated(seed)` helper to return the committed fixture at seed 42. Two of its 43 seed-42 callers — `generated_worlds_are_deterministic` and `glossed_names_are_stable_across_two_builds` — existed to compare **two independent builds**, and became comparisons of two reads of one file: the first then asserted only that `World::from_json(x).to_json()` is a pure function of `x`, and the second performed zero builds. Both stayed green | the **final whole-branch review**, from a **timing**: the pair passed together in **0.06 s** for what should have been four ~3.0 s builds. No assertion could fail, so no assertion could report it; the clock was the only witness. Fixed by giving each test a local builder (the remedy Task 3 had already applied one crate over, in `windows/vessel/src/session.rs`), after which they run ~11 s each |

**Why a per-task review could not have found it.** The migration is one edit
in one file, reviewed as one edit; the damage is distributed across callers
written by five other tasks, and is visible only as an absence — a test that
still passes, faster. Task 3's review had handled the identical shape
correctly in `windows/vessel`, which is the sharpest part of the record: the
project already knew the hazard and had the remedy in the tree. What was
missing was the step of enumerating a migrated helper's callers, which is a
whole-branch act. The campaign's own positive-control instrument (breaking
`FIXTURE` to a nonexistent path and watching the panic fire from inside the
seed-42 arm) would have exposed both tests immediately had they been among
its targets.

The same review found two prose defects of the same origin —
`ENTRY_POINTS` omitting a sixth real entry point, and two documents asserting
an `artifacts` roster row that has no rows — which are counted in neither
table above because they were **found and fixed on this branch before merge**,
in the same commit as row I. They are recorded in decisions 0606 and 0607
where a reader will actually meet them.

### The eleven controller-text defects

| # | defect, in controlling-session text | what killed it |
| --- | --- | --- |
| 1 | Spec §3.4 conflated *concentration* (which helper has the most callers) with *reachability* (which helper actually builds the fixture's identity), and would have sent an implementer to migrate `book::generated` and `worldgen::constant` to a seed-42 fixture that none of their 83 combined callers wants | pre-plan verification against each helper's actual body and each caller's actual argument (ledger #6) |
| 2 | Spec §3.1 and the plan's Task 1 both listed `hornvale_lab::health::simulate_world` as a sixth build entry point; it takes an already-built world and derives from it, making it a decision-0092 weir site, not a build site | reading the function's signature and body before dispatch (ledger #7) |
| 3 | Task 1's guard module doc cited decision 0606 on a plan that scheduled the decision record itself to be written only at campaign close — which would have reddened `gate-commit`'s sub-floor tier (`docs_consistency::decision_cites_in_sources_resolve`) on every intermediate commit | checking the citation against the guard that resolves it before Task 1 ran (ledger #8) |
| 4 | The spec did not anticipate that a fixture loader living in production `src/` would widen decision 0090's frozen `manifest-dir-uses.txt` roster by one file | the gate itself — `build_path_embedding.rs` reddening on the new, unlisted site (ledger #9) |
| 5 | Spec §2.2's payoff estimate (`~3,000 ms -> ~1,110 ms = ~2.7x`) divided a quiet-box numerator by a contended-box denominator from two different measurement runs — the exact fault `docs/timings.md`'s own header exists to prevent | Task 4 measuring 4.0x/4.2x, which prompted re-derivation instead of celebration (ledger #10) |
| 6 | The controller's own re-derived caller counts, computed with `grep -o '<helper>()' \| wc -l`, counted substrings inside longer identifiers (`world()` matching inside `seam_world()`) and produced an aggregate (244) *further* from the truth than the number it was correcting (240) | Task 5's reviewer independently recounting with a word-boundary regex and reporting the disagreement (ledger #11) |
| 7 | The plan's Step 7 predicted a non-empty `docs/audits/` diff after adding `seed_42_world() -> World`; measured after all five code tasks had landed, it did not move — the function exposes no primitive at a `pub` boundary and `FIXTURE` is a private `const` | pre-dispatch verification rewriting the step as a branch table rather than refreshing the prediction (commit `499ca2441`) |
| 8 | This task's own dispatch asserted a ninth consecutive campaign with this defect distribution, extrapolating from The Winze's "eighth" without checking the campaign that landed in between | reading the intervening campaign's own retrospective before the claim was published (ledger #14) |
| 9 | Verifying that step's branch table with `make type-audit-report \| tail; echo $?` read `tail`'s exit status, not `make`'s, and nearly banked a staleness conclusion no command in the pipe could have supported | redoing the check without the pipe (Task 6 pre-dispatch verification) |
| 10 | A path-existence check run against bare basenames rather than repo-relative paths tested each against the repository root and reported five legitimate prose references as `MISSING` | re-running the check with the correct paths before trusting the negative result |
| 11 | A duplicate ledger entry, both numbered `#12`, one written by this task and one by the coordinator's own review | renumbering both to `#13`/`#14` (commit `584fff560`) |

### The two implementer-prose defects, and what is actually interesting about them

Rows 1 through 11 above are controller text — spec, plan, dispatch, and this
task's own verification instruments. Two further defects belong to neither
that bucket nor to implementer *code*: they are prose this closing task wrote
into two permanent records.

| # | defect, in this task's own prose | what killed it |
| --- | --- | --- |
| A | Decision 0606 and the chronicle both named `windows/book/src/lib.rs::generated` as the helper with the 43-call-site migration. The real file is `windows/worldgen/src/lib.rs::generated` — `book/src/lib.rs::generated` is the seed-1 helper spec §3.4 put explicitly out of scope, and neither the brief nor the dispatch named any file for this case at all; the wrong name was this task's own invention | a controller reading the claim against the tree rather than against memory (fix round 1) |
| B | The chronicle cited registry ID `MAP-25` from `book/src/chronicle/`, which decision 0031 forbids outside `book/src/frontier/` — also invented by this task; neither the brief nor the dispatch cited a registry ID anywhere | `docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary`, a gate test that already existed and already sat in the sub-floor roster (fix round 2) |

What makes these two worth their own section, rather than folding them into
the table above as two more rows, is not that they happened — it is *what
caught them*. Neither was caught by re-reading. The wrong filename was caught
the same way every controller-text defect in this campaign was: a claim
checked against the tree instead of trusted from memory. The registry
citation was caught by a gate test built for exactly that failure, sitting in
the sub-floor roster the whole time — see the next section for why it did not
fire on its own. Both are better evidence for this book's Confidence Gradient
chapter's central thesis than a clean sweep would have been: the practice that
works is verification against ground truth, and it does not care whether the
prose under test came from a spec, a dispatch, or the task currently
writing the retrospective about it.

## The gate that would have caught defect B did not run, and the reason generalizes

`docs_consistency::the_book_carries_no_registry_ids_or_process_vocabulary` is
in `docs/timings/subfloor-roster.tsv`, so it runs inside `gate-commit` — it did
not fail to exist, and it did not fail to be wired in. It failed to *run*.
Every commit this task made before fix round 2 was docs-only, and
`scripts/hooks/pre-commit` fast-paths past `make gate-commit` whenever no
Rust-relevant path is staged, printing "no Rust-relevant paths staged —
skipping 'make gate-commit'" — which it did, correctly by its own logic, on
roughly twelve docs-only commits this campaign made.

**The premise behind that fast path is "Rust paths staged implies Rust checks
matter," and the counterexample is a Rust test whose subject is prose.**
`docs_consistency` scans `book/src` and `docs/`; a change to either is exactly
what it exists to check, and exactly what the fast path decides needs no
checking, because it reasons about the staged paths' blast radius in one
direction only. Had the merge queue been the first thing to run this test
against the `MAP-25` citation, it would have reddened on the `gate` phase
after taking the canonical box — a one-sentence prose defect that a local
gate held the test for and declined to run.

This is not this task's defect to fix. The hook is shared substrate used by
every campaign, and changing its fast-path logic wants its own measurement of
what it would cost every commit if `gate-commit` ran on every docs change,
not a fix folded into this one's close. The finding is captured as ledger
entry #13, and the cheap mitigation available today, to any campaign, is to
run the check by hand before a docs-only commit that touches `book/src`:

```
cargo test -p hornvale --test suite -- docs_consistency
```

It costs about a second.

## The correction that made the number worse

Row 6 deserves its own section because it inverts the usual shape of a
finding. Every other row in this table is a case where re-reading,
re-deriving, or re-measuring something produced a *better* number. Here,
re-deriving produced a *worse* one, and it
did so while looking exactly as credible as a real correction at every
intermediate step: each individual count seemed plausible, the totals stayed
in a believable range, and the two files where the substring bug fired were
wrong in the same direction a genuine correction would have pushed them.
Nothing about the output said "this instrument is answering a neighbouring
question."

The frozen number (239 call sites, decomposing as 110/54/75 across the three
migration tasks) came from a reviewer's refusal to accept a stated figure
without recomputing it independently — the second time in this campaign that
a reviewer's insistence on an independent recount, rather than trust in a
controller's stated arithmetic, caught something re-reading would not have.
The lesson generalizes past this campaign: a correction is a second claim,
exactly as checkable as the first one, and treating it as settled because it
arrived as a correction is the failure mode. `book/src/open-questions.md`'s
Confidence Gradient chapter now carries this instance alongside the campaign's
positive-control episode (Task 5's `FIXTURE`-repointed-to-a-nonexistent-path
test, run to a verified panic by both the implementer and the reviewer
independently) as opposite-polarity confirmations of the same standing bet.

## Reading the decision log first would have shortened the analysis

Nathan's own two proposed remedies at the approach gate — memoize the build in
a process-local cache, or mark the raw constructors deprecated and allowlist
sanctioned call sites via `clippy.toml` — were both aimed slightly off-target,
and the repository already held the refutation of the first one. Decision
0032, ratified four weeks earlier, rejected an in-process `LazyLock` memo for
the census on the identical structural ground this campaign re-derived by
measurement: nextest's process-per-test model re-initializes any such cache
once per test, so it recovers on the order of a few percent rather than the
bulk of the redundancy. The campaign's own probe (93 of 100 world-building
processes building exactly one world) confirmed 0032's reasoning rather than
discovering anything new.

This is not a criticism of the proposal — a decider proposing an approach and
letting the analysis phase test it against evidence is exactly how the process
is supposed to work, and the ideonomy pass that ran against it did its job,
overturning the mechanism half of the original proposal (ledger #1, #2). The
process lesson is narrower and cheaper to apply: **grep the decision log for
the shape of the problem before spending an analysis pass rediscovering an
answer already on record.** `docs/decisions/` is the durable, grep-able home
for exactly this, and a five-minute search for "nextest" or "process-per-test"
would have surfaced 0032 before the ideonomy pass needed to re-derive its
conclusion from first principles. The pass was not wasted — it also produced
the reason-code taxonomy and the homogeneity argument that shaped decision
0606 — but the process-model half of its work was a rediscovery, not a
finding.

## A known skill/doctrine conflict, confirmed again rather than fixed

`campaign-autopilot`'s own instructions still direct a campaign's decision
ledger to `.superpowers/sdd/decision-ledger.md` — the exact path The Cartulary
(2026-08-30) named as the collision-prone one, because every campaign's
scratch ledger lived at that identical location and two campaigns editing it
in parallel would silently merge to one side. Root `CLAUDE.md` already carries
the superseding rule: rulings, deferred minors, and parked findings belong in
the committed per-campaign ledger (`docs/superpowers/ledgers/<date>-<name>.md`)
as they occur, not in a shared scratch file at campaign close.

This campaign followed the skill first, as instructed, wrote to the scratch
path, then found the doctrine and moved everything into the committed ledger
before merge — the failure Cartulary exists to prevent did not materialize
here, because this session happened to read far enough to catch it. That is a
near miss, not a fix. A `PROC-*` row
(`PROC-autopilot-names-the-superseded-ledger-path`) is already in the idea
registry recording the fix that is still owed: one edit to the skill's own
text. Recorded here again because a near miss that is not converted into a
structural fix is a near miss the next campaign gets to have too.

## The abandoned full-suite measurement, and whether that was the right call

The 71-redundant-builds figure that motivated this campaign is a sample of the
first 285 of 4,869 tests — a fail-fast run stopped early by an unrelated,
pre-existing flake (parked as P1: a fixed, unsuffixed temp-file path in
`cli/tests/suite/scene_surrounds_colour_cli.rs`). A full instrumented run was
attempted and abandoned: at the sampled throughput it projected to roughly
three hours on a box that was already at load average 42 from concurrent
sessions, and running it to completion would have monopolized shared hardware
for that entire window to produce one number.

**The call to abandon it was right, and the spec's own reasoning for
abandoning it holds up under what the campaign then measured.** The design
does not depend on the suite-wide total: a ~200x load-versus-build ratio on a
bare fact read justifies migrating any given site regardless of how many
redundant builds exist in aggregate, and the ratchet's enforcement value is
independent of the count entirely — it prevents new redundancy from
accumulating whether the current total is 71 or 7,100. Nothing the campaign
measured afterward — not the 239 migrated call sites, not the 567.0 CPU-seconds
recovered, not the debt-counter distinction decision 0606 now states
explicitly — would have been sized or shaped differently by knowing the exact
suite-wide total. The honest cost of the decision is that the campaign's
headline "CPU-seconds saved" number is a sum over six *measured* modules, not
a share of a known total, and the chronicle and decision 0606 both say so
rather than implying a suite-wide percentage that was never computed. A
nightly, unattended instrumented run (in the spirit of the scheduled-census
idea already parked for a different measurement) would answer the question
without contending for shared hardware, and is worth a registry row if a
future campaign wants the exact denominator — but it was correctly judged not
worth this campaign's time to build.

## What this campaign confirms about the practice, not just the product

Nothing above is a new category of process failure. Every row in all three
defect tables is an instance of a diagnosis this book's Confidence Gradient
chapter has been refining for two months: verify a brief against the tree before
dispatch, treat a correction as a claim requiring its own check, and prefer a
decision-log search to a fresh derivation when the shape of the problem looks
familiar. What this campaign adds is not a longer streak — Chattel's own
count shows the distribution is not uniform across campaigns — but three
fresh instances of the same cost, all controller-side: a headline claim built
by incrementing a prior campaign's number without reading the campaign in
between (row 8), an exit status read off the wrong command in a pipe (row 9),
and a path check run against the wrong base (row 10). `book/src/open-questions.md`'s
"make it fail on command" bet now scores this campaign at three such
instruments, not two.

**And this record's own closing task supplied the distribution's other
half, which its first draft nearly erased.** A defect count that reports
zero on every axis it is capable of producing a nonzero on is not a clean
result — it is a count that has not looked hard enough, and this document's
own first draft demonstrated the failure it was trying to describe by
claiming exactly that. The two implementer-prose rows, and the one
implementer-code row, exist because a
coordinator's review kept checking after the controller-text table looked
complete, which is the same practice the table itself argues for, aimed one
level up. The implementer-code row went further: it needed a *different
vantage*, not more diligence at the same one. Six task reviews each read a
correct diff correctly; the defect lived in the relation between one task's
edit and five other tasks' callers, and only a whole-branch reading — and, in
the end, a clock — could see it.

## Deferred minors, and the discipline miss the close found

Six minors were deferred across the six tasks. **Four of them existed only in
`.superpowers/sdd/<campaign>/progress.md`, which is git-ignored and dies when
the worktree is recycled** — found by `closing-a-campaign` step 2A's walk, which
greps that file rather than trusting the closer's memory of the campaign. They
are backfilled into the committed ledger as entry #20 with the outcomes below.

| deferred minor | outcome |
|---|---|
| Task 1's report kept the disproven "expected drift" account of the roster total | accepted as-is — the report is scratch; its committed half (decision 0606's baseline) was corrected in fix round 1 |
| `docs/timings.md` gained 3 `gate-commit` rows across 2 commits, keyed to the pre-commit `HEAD` | accepted as-is, pre-existing — `ci-record` runs inside `pre-commit`, before the commit object exists |
| `manifest-dir-uses.txt`'s count of 2 for `fixture.rs` includes a doc-comment mention | accepted as-is, deliberately (ledger #9) — the over-count is the guard's documented textual tolerance and errs safe |
| `world-build-sites.tsv`'s "sort-order violation" | **closed as a non-issue** — `LC_ALL=C sort` yields zero differing lines; it appeared only under locale-aware collation, and the roster is read into a `BTreeMap`. It never existed in any committed document |
| Task 3's commit message says 110 call sites where the brief said 108 | accepted as-is — commit messages are immutable, and the frozen figure (ledger #11) is 239 |
| duplicate ledger entry `#12` | fixed, `584fff560` |

**The miss is worth more than the minors.** The Cartulary moved the decision
ledger out of `.superpowers/sdd/` precisely because that tree dies, and this
campaign reintroduced the same loss by writing rulings into the durable ledger
and deferred minors into the scratch one — in the same sittings, hours apart,
without noticing the two files have different lifetimes. The plugin's
`progress.md` is where task state legitimately belongs, so writing there is not
wrong in general; it was wrong for *this*. Nothing in the workflow makes that
distinction visible at the moment of writing, which is why the backstop is a
closer's grep and not an author's care.

The same walk found a second omission of the same shape: ledger #13's Capture
line promised a `PROC-*` registry row for the campaign's most reusable finding —
that `pre-commit`'s docs-only fast-path is blind to a Rust test whose subject is
prose — and no row had been written. It exists now as
`PROC-docs-only-commits-skip-a-gate-that-checks-docs`. A Capture line is a
promise, and nothing checks that promises are kept except this walk.
