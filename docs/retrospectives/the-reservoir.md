# The Reservoir — retrospective

**Merged:** 2026-09-02

## Every defect this campaign found was in controlling-session text

Every review this campaign ran (Tasks 1 through 5) found defects only in
the spec, the plan, or a dispatch — none in an implementer's code. That is a
true statement about what this campaign's reviews found, and this record
stops there rather than turning it into a streak.

**An earlier draft of this section claimed a ninth consecutive campaign with
this distribution, incrementing The Winze's stated "eighth" by one without
checking what landed in between.** The Chattel merged between The Winze and
this campaign, and its own retrospective (`docs/retrospectives/the-chattel.md`,
"Defects by origin") tabulates 43 controller-prose defects against
**approximately 68 implementer-code defects** — the opposite distribution,
not a continuation of it. Chattel's own text says "Four campaigns running have
reported the same distribution," not eight. The false claim would have gone
into a permanent record as a headline had it not been checked, which is the
same shape of error this campaign spent six tasks catching in other people's
prose — including, this time, the controlling session's own.

**Chattel's own methodological warning sharpens this record rather than
undercutting it, and is worth repeating here.** Chattel's controller count was
*complete* — every brief was verified pre-dispatch, so its defects were
enumerated exhaustively — while its implementer count was a *floor*, because
three tasks' reviews were never triaged into the ledger. "Read the
denominators before the ratio": the two sides of any such count are rarely
measured the same way, in any campaign, including this one. This campaign's
"every defect found was in controller text" is a statement about what six
reviews *found*, not a claim that implementer code in general produces no
defects, or that this campaign's own reviews were exhaustive in the way
Chattel's pre-dispatch brief check was. No streak is asserted.

| # | defect, in controlling-session text | what killed it |
| --- | --- | --- |
| 1 | Spec §3.4 conflated *concentration* (which helper has the most callers) with *reachability* (which helper actually builds the fixture's identity), and would have sent an implementer to migrate `book::generated` and `worldgen::constant` to a seed-42 fixture that none of their 83 combined callers wants | pre-plan verification against each helper's actual body and each caller's actual argument (ledger #6) |
| 2 | Spec §3.1 and the plan's Task 1 both listed `hornvale_lab::health::simulate_world` as a sixth build entry point; it takes an already-built world and derives from it, making it a decision-0092 weir site, not a build site | reading the function's signature and body before dispatch (ledger #7) |
| 3 | Task 1's guard module doc cited decision 0606 on a plan that scheduled the decision record itself to be written only at campaign close — which would have reddened `gate-commit`'s sub-floor tier (`docs_consistency::decision_cites_in_sources_resolve`) on every intermediate commit | checking the citation against the guard that resolves it before Task 1 ran (ledger #8) |
| 4 | The spec did not anticipate that a fixture loader living in production `src/` would widen decision 0090's frozen `manifest-dir-uses.txt` roster by one file | the gate itself — `build_path_embedding.rs` reddening on the new, unlisted site (ledger #9) |
| 5 | Spec §2.2's payoff estimate (`~3,000 ms -> ~1,110 ms = ~2.7x`) divided a quiet-box numerator by a contended-box denominator from two different measurement runs — the exact fault `docs/timings.md`'s own header exists to prevent | Task 4 measuring 4.0x/4.2x, which prompted re-derivation instead of celebration (ledger #10) |
| 6 | The controller's own re-derived caller counts, computed with `grep -o '<helper>()' \| wc -l`, counted substrings inside longer identifiers (`world()` matching inside `seam_world()`) and produced an aggregate (244) *further* from the truth than the number it was correcting (240) | Task 5's reviewer independently recounting with a word-boundary regex and reporting the disagreement (ledger #11) |

Two things are worth saying about the table before its parts. First, five of
six were caught before an implementer ever wrote code against the defect —
verification is cheaper than remediation, and every campaign that has run this
practice has confirmed it again. Second, the sixth is not a defect an
implementer introduced; it is a defect the *controlling session* introduced
while trying to correct someone else's number, and it is the sharpest entry in
the table for exactly that reason — see below.

## The correction that made the number worse

Row 6 deserves its own section because it inverts the usual shape of a
finding. Every other row in this table, and in the eight retrospectives before
it, is a case where re-reading, re-deriving, or re-measuring something
produced a *better* number. Here, re-deriving produced a *worse* one, and it
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

Nothing above is a new category of process failure. Every row in the defect
table is an instance of a diagnosis this book's Confidence Gradient chapter
has been refining for two months: verify a brief against the tree before
dispatch, treat a correction as a claim requiring its own check, and prefer a
decision-log search to a fresh derivation when the shape of the problem looks
familiar. What this campaign adds is not a longer streak — Chattel's own
count shows the distribution is not uniform across campaigns — but a fresh
instance of the *cost* of asserting one without checking: a headline claim
built by incrementing a prior campaign's number, caught only because this
task's own coordinator went and read the campaign that landed in between.
