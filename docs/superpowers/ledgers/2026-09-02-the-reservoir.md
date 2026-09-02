# The Reservoir — decision ledger

Campaign: **The Reservoir** — one built world, drawn many times. Branch
`campaign/the-reservoir`, based on `25ee1d830`. Decision block **0606-0615**
(main ceiling 0585 at reservation).

Autopilot is engaged (G3 and G6 are the hard stops).

---

#1 [G1] — **Which lever against redundant world builds in the test suite.**

*Question.* The suite builds the seed-42 world many times over. Which lever:
in-process memoisation, a committed fixture read as an input, depth-scoping, a
lint, or a ratchet?

*Decision.* **Ratchet plus fixture, staged** — a source-scan ratchet that refuses
a new build site, plus a fixture-loading path for the dominant seed-42 identity;
existing sites migrate opportunistically rather than in one pass.

*Why.* Nathan's own call at the approach gate, against three stated
alternatives. It is also the reading the ideonomy pass produced: on the
*decomposability* axis the problem is fully decomposable (site by site, and 74%
of sites sit behind 70 helper functions), and on the *reversibility* axis both
halves undo by deletion — which argues for a ratchet rather than a migration.

*Alternatives discarded.* (a) Aggressive fixture-by-default — largest and
fastest win, but the largest single change and the broadest coverage residue.
(b) Ratchet only, no fixture — introduces no new fixture role and no coverage
argument at all, but leaves the measured 71 redundant seed-42 Full builds in
place. (c) Measure the suite-wide prize first — declined: the ~200x
load-versus-build ratio makes the fix worth doing at any plausible count, and
the enforcement half is independent of the count entirely.

*ideonomy passes / overturns.* 1 / 1 — the pass overturned the mechanism half of
the original proposal (see #2).

*Capture.* Spec §3; `.superpowers/sdd/decision-ledger.md`.

---

#2 [Q] — **Clippy `disallowed-methods`, or a source-scan ratchet.**

*Question.* Nathan proposed marking the build pathway deprecated and
allowlisting sanctioned patterns. Decision 0092 already does exactly that for
three derivation entry points via `clippy.toml`. Extend that list, or build
something else?

*Decision.* **A source-scan enforcement test** in `cli/tests/suite/`, roster in
`cli/tests/fixtures/`, checked in both directions. Not a `clippy.toml` row.

*Why.* Decision 0092's own ratified text records the trap:
`disallowed-methods` is **one lint with one on/off switch per scope**. A single
crate-level `#[allow]` there silenced all 24 platform-libm bans (decision 0041)
across worldgen — a constitutional determinism guard, disabled as a side effect,
caught only in review. 0092 carried ~31 production sites. This would carry
hundreds of grandfathered sites, and therefore hundreds of scopes in which the
libm ban *and* the weir both go dark. That trades a cost problem for a
determinism-guard hole, which is the wrong trade in this repository above all
others. `cli/tests/suite/test_binary_ratchet.rs` is the shipped idiom for
precisely this shape, and CLAUDE.md names `cli/` as the home of the
workspace-wide enforcement tests.

*And it answers 0092's own objection to a roster file.* 0092 argues "the
`#[allow]` attribute at the site IS the sanctioned-site list — one source of
truth, greppable, never a second document to keep in sync." The objection is
sound against a *unidirectional* list. Bidirectional checking dissolves it: a
stale row is an error, so the document cannot rot. That is exactly why
`test_binary_ratchet.rs` checks both directions and says so in its module doc.

*Alternatives discarded.* (a) The clippy entry, above. (b) An `#[allow]`-only
scheme with no roster — not a ratchet at all, because nothing then measures
whether the grandfathered set shrinks.

*ideonomy passes / overturns.* 1 / 1. Tuple: substitution + organon-construction
over a matrix organon; dimensions homogeneity / decomposability / reversibility.
The *homogeneity* reading is what produced #4 — the population is homogeneous in
world **identity** (14 distinct identities across 107 observed builds, 71 of them
one identity) but heterogeneous in **why it builds**, and an allowlist must key
on the latter, not on whether a call site memoises.

*Capture.* Spec §3.1; decision 0606.

---

#3 [Q] — **Where the fixture loader lives, and whether it `include_str!`s.**

*Decision.* In `hornvale-worldgen` production source — the composition root —
reading the fixture at **runtime**, never `include_str!`.

*Why.* `kernel/src/golden.rs` is already test support living in production
kernel source, so the posture is settled precedent rather than a new smell.
Worldgen specifically because every crate that builds a world already depends on
it, so no new edge is added to the layering graph. Runtime read rather than
`include_str!` because 5.5 MB baked into N test binaries hits the
compilation-unit cost that *is* this project's gate cost — CLAUDE.md records the
260 test targets alone costing +157.5 s of kernel time merely to exist, and
`test_binary_ratchet.rs` exists to stop that re-accreting. Baking a 5.5 MB
literal into dozens of units would be a regression on the very axis this
campaign is trying to improve.

*Mechanism note — verified, and the tree's own idiom proved only a neighbouring
claim.* `env!("CARGO_MANIFEST_DIR")` expands against the crate containing the
macro — worldgen — so one fixed `../../` prefix resolves correctly regardless of
which crate calls the function. `windows/worldgen/tests/suite/repose_exposure.rs:1924`
looks like precedent but is worldgen's own test reading worldgen's dir, i.e. the
*cheaper neighbour* of the claim actually being made. Measured with a scratch
probe instead — a `pub fn` in worldgen returning `env!("CARGO_MANIFEST_DIR")`,
called from an example in `cli`:

```
  worldgen fn reports:               .../windows/worldgen   <-- called FROM cli
  cli's own env! is:                 .../cli
  cli-relative ../../ resolves?      false     <-- negative control
  worldgen-relative ../../ resolves? true
```

The negative control is the load-bearing half: the prefix is **crate-specific**,
so the loader must live in exactly one crate. A per-crate helper duplicating
`../../` would resolve outside the repository from `cli` and `kernel` (one level
deep) while working from every `windows/*` and `domains/*` crate (two deep) — a
bug that passes in most crates and fails in two. Scratch reverted; `git status`
clean.

*ideonomy passes / overturns.* 1 / 0.

*Capture.* Spec §3.3.

---

#4 [Q] — **What a roster row records.**

*Decision.* A **reason code** — `build-path` / `artifacts` / `identity` /
`production` / `unmigrated(<why>)` — and the ratchet is one assertion: the count
of `unmigrated(...)` rows never increases.

*Why.* This is the three-valued shape `tropes check`, the timings baseline,
type-audit's `waiver(...)` and seam-guard all already use. A guard that failed
on the mere *existence* of the grandfathered set would be red on day one and
trained away by day two — `test_binary_ratchet.rs` says exactly that in its own
module doc, having watched 13 test binaries creep back. Separating the permanent
reasons from `unmigrated(...)` is what makes the ratchet measurable without making it
a wall, and it puts the why-it-builds taxonomy where a reader will find it.

*ideonomy passes / overturns.* 1 / 0 — this entry is the homogeneity reading
from #2's pass, promoted to a decision of its own.

*Capture.* Spec §3.2; decision 0606.

---

#5 [Q] — **Whether `cli/tests/fixtures/world-seed-42.json` moves.**

*Decision.* No. It stays; worldgen reaches across to it.

*Why.* `docs/generated-paths.txt` carries explicit prose that
`cli/tests/fixtures/` is byte-golden and frozen-historical-pin territory — "A
golden has an accept path a human may deliberately take. A frozen historical pin
has NONE" — and that declaring the directory artifacts-authored "is a category
error rather than an inert mistake". Moving the file means rewriting that prose,
a category-sensitive edit that does not belong bundled into this campaign.

*Alternatives discarded.* Move it under `windows/worldgen/tests/fixtures/` —
tidier ownership, but drags the generated-paths rewrite along with it.

*ideonomy passes / overturns.* 1 / 0.

*Capture.* Spec §3.3. Flagged to Nathan as a judgment call at the design gate
and approved there.

---

#6 [Q] — **Correction: concentration is not reachability. The spec's own §3.4 was
wrong, and it was caught pre-plan.**

*Question.* The approved spec said "six helper bodies reach 285 call sites".
Planning against that number required knowing which of those six actually build
the identity the fixture holds. Do they?

*Finding.* **Two of the six do not, and one of those cannot ever.** Checked
against each helper's body and each caller's real argument, not inferred:

```
  book/src/lib.rs::generated(seed)    — 47 callers, and seed 42 appears
                                        ZERO times: 28x seed 1, 4x seed 2,
                                        2x seed 3. Needs a seed-1 fixture.
  worldgen/src/lib.rs::constant(seed) — 36 callers, builds under
                                        SkyChoice::Constant, a different pin
                                        signature. Needs its own fixture.
  worldgen/src/lib.rs::generated(seed)— 43 of 53 callers pass 42; only that
                                        arm is reachable.
```

Four helpers the original table ranked *below* the top six are all seed-42
Generated and fully reachable (`worldgen/tests/suite/exposure.rs::world` 27,
`vessel/tests/suite/session.rs::seam_world` 24, `…/session_snapshot.rs::world`
16, `…/the_blocking.rs::world` 15).

*Decision.* Restate the surface as **240 reachable sites across seven helpers**,
with 83 sites explicitly blocked behind a second (seed 1, Generated) and third
(seed 42, Constant) fixture that §7 keeps out of scope. Spec §3.4 rewritten,
retitled, and the §7 non-goal now names and sizes the two blocked fixtures
instead of saying "the remaining 13 identities".

*Why it matters more than the arithmetic.* The old number was not merely
optimistic, it pointed the plan at the wrong two tasks. A plan built on it would
have dispatched an implementer to migrate `book::generated` to a seed-42 fixture
that none of its 47 callers wants, and the implementer would have discovered it
only after editing. This is `verify-the-brief-against-the-code` doing its job at
the pre-dispatch step the practice exists for — the ranking was real, the
inference from ranking to reachability was the defect.

*ideonomy passes / overturns.* n/a — a factual correction, not a design choice.

*Capture.* Spec §3.4 (rewritten), §7 (non-goal rewritten); this entry.

---

#7 [G5] — **`simulate_world` is not a build entry point. Removed from the scan
before Task 1 was dispatched.**

*Question.* The spec's §3.1 and the plan's Task 1 both listed six build entry
points to scan. Pre-dispatch verification greps every named identifier. Do all
six actually build a world?

*Finding.* **Five do; the sixth does not.**
`hornvale_lab::health::simulate_world` is
`simulate_world(world: &World) -> Vec<AffectTrace>` — it takes an
**already-built** world and derives terrain and climate from it:

```rust
    let Ok(terrain) = hornvale_worldgen::terrain_of(world) else { … };
    let Ok(climate) = hornvale_worldgen::climate_from(world, &terrain) else { … };
```

It already carries decision 0092's scoped `#[allow(clippy::disallowed_methods)]`
and the comment "Named construction site (decision 0092)". It is a **weir site,
governed by clippy**, not a build site.

*Decision.* Drop it. The scan is five entry points. Both the spec and the plan
are corrected, and the plan's `ENTRY_POINTS` const carries a doc comment telling
a future reader — or a future implementer tempted to "complete" the list — why
the sixth is absent.

*Amended by entry #15 (final review).* The ruling on `simulate_world` stands
unchanged and is still correct. The **count** does not: the scan is **six**
entry points, because `build_world_from_components` was missing from the list
this entry was adjudicating. Rejecting one candidate is not enumerating the
set, and this entry is the exact place that distinction went unnoticed — it
reasoned carefully about the wrong sixth. "Why the sixth is absent" now reads
as being about `simulate_world`, which is a seventh.

*Why it matters beyond the count.* The arithmetic is small: 355 sites across 201
files becomes **350 across 200**, the difference landing entirely in
`windows/lab` (`health.rs` leaves the roster; `affect_trace_golden.rs` 2→1,
`health_calibration.rs` 4→1). The real cost would have been conceptual —
**0606 governs construction OF a world, 0092 governs derivation FROM one**, and
folding a 0092 site into 0606's roster would have given two mechanisms
overlapping jurisdiction over the same call, with no rule for which wins. That
is the kind of defect that reads as thoroughness.

*Cost if wrong.* Nil in the other direction: if a future campaign decides
derivation sites belong on this roster too, adding them is additive and 0092's
clippy entries would then be the redundant half.

*ideonomy passes / overturns.* n/a — a factual correction.

*Capture.* Spec §3.1 (entry-point list plus the rejection rationale), §3.2
(roster table 355→350); plan Task 1 (`ENTRY_POINTS`, the generator script,
`UNMIGRATED_CEILING`, the module doc); this entry.

---

#8 [G5] — **A decision citation in Task 1's source needed the record to exist
before Task 6 wrote it. Plan defect, caught by the implementer.**

*Question.* Task 1's guard module doc cites "decision 0606", and the plan
scheduled Task 6 to create that record at campaign close. Does anything object
in between?

*Finding.* Yes, and it would have objected on every intermediate commit.
`cli/tests/suite/docs_consistency.rs:1210`'s
`decision_cites_in_sources_resolve` runs inside the sub-floor tier that
`make gate-commit` executes, and it fails on any decision citation in source
that does not resolve to a record. So Tasks 1 through 5 would each have hit a
red commit gate for a reason unrelated to their own work — the kind of red that
gets worked around rather than read.

*Decision.* **The stub stands.** Task 1 created
`docs/decisions/0606-a-world-build-is-a-named-site.md` with
`Status: Proposed (2026-09-02)`, drawing its content from spec §8.
`docs/decisions/README.md` explicitly permits that status — "`Accepted`,
`Superseded by NNNN`, or (rarely) `Proposed`" — and three records already carry
it, so this is a sanctioned state and not an invented one. Decisions are
append-only *once Accepted*; a `Proposed` record is still editable, which is
exactly the affordance this needs.

Task 6 now **edits** 0606 to `Accepted` rather than creating it. The plan's
Task 6 Files list and Step 1 have both been rewritten to say so, in bold, along
with the reason — the failure mode otherwise is a second record minted for the
same number, which is the shape CLAUDE.md records two campaigns hitting on 0134.

*Cost if wrong.* Immaterial. If the campaign were abandoned, a `Proposed` record
describing a mechanism nobody built would sit in the log — which is what
`Proposed` means, and what the three existing ones already do.

*ideonomy passes / overturns.* n/a — a mechanical consequence of a guard, not a
design choice.

*Capture.* Plan Task 6 Files list and Step 1 (rewritten); this entry.

---

#9 [G5] — **DETERMINISM-ADJACENT: the fixture loader widens decision 0090's
build-path embedding set by one file. Accepted, with the cost stated.**

*This entry leads the G6 digest, per campaign-autopilot's rule that
determinism-contract entries lead.*

*What happened.* `windows/worldgen/src/fixture.rs` resolves the fixture with
`concat!(env!("CARGO_MANIFEST_DIR"), "/../../cli/tests/fixtures/world-seed-42.json")`.
Every `env!("CARGO_MANIFEST_DIR")` in production `src/` bakes the absolute build
directory into the shipped binary, which is why two builds of one commit in
different directories do not hash the same. Decision **0090 amendment 2** freezes
the list of such sites in `cli/tests/fixtures/manifest-dir-uses.txt` and says it
"may shrink freely; growing it is a deliberate act that should say what it does
to the oracle." The spec did not anticipate this consequence; the gate caught it.

*The list went from two entries to three:*

```
  cli/src/main.rs                  2
  windows/lab/src/blackbox.rs      1
  windows/worldgen/src/fixture.rs  2   <-- new
```

*Ruling: accepted.* Three reasons, in order of weight.

1. **It does not change what 0090's oracle REQUIRES**, only how many ways there
   are to violate it. The oracle qualifies a candidate host by comparing one
   sha256sum instead of running a census, and it holds only while both hosts
   build at the same absolute path — the condition the container it is designed
   around satisfies for free. Two sites already violated it for anyone building
   outside one. A third changes the count, not the shape.
2. **The alternative is worse.** Removing the `env!` means resolving the
   workspace root at runtime by walking up from `current_dir()`. That trades a
   *documented, guarded, compile-time* fact for an *undocumented runtime
   dependency on CWD* — and routing around a guard whose whole purpose is to
   make this class of change visible is the wrong instinct.
3. **The repo's nearest precedent does not settle it.** `kernel/src/golden.rs`
   is test support in production source and avoids `env!` by taking a `&Path`,
   leaving callers to supply `concat!(env!(...))` from their own test code. That
   would work here only if every caller had the right prefix, and the prefix is
   crate-depth-dependent — one level from `cli`/`kernel`, two from every
   `windows/*` and `domains/*` crate. Ledger #3's probe proved `cli`'s own
   `../../` resolves outside the repository. One loader with one prefix is the
   reason this design exists.

*The implementer discharged 0090's own requirement unprompted*, in the commit
message for `b44db18ee`: "This widens the set of absolute build paths hornvale's
binaries embed by one file; it does not change what decision 0090's cross-host
binary-identity oracle requires (both hosts building at the same absolute path)."
That is what amendment 2 asks for, and it was written without being asked.

*One imprecision, noted not fixed.* The row reads 2, but only ONE of those is a
real embedding (`fixture.rs:35`); the other is the doc comment at `:27` naming
the macro it uses, counted because `build_path_embedding.rs` is a textual scan
too. Ruling: **keep the doc comment.** Prose explaining why the macro is there
is high value for exactly the reader auditing this exposure, and rewording it to
dodge a counter would be the tail wagging the dog. Flagged for the reviewer to
weigh; a clarifying comment in the roster file would be a cheap improvement.

*Cost if wrong.* A future campaign wanting 0090's oracle to hold outside a
container has one more file to fix. The fix is mechanical and the roster names it.

*Capture.* This entry; decision 0607 must repeat the oracle statement, since a
commit message is not where a durable consequence belongs.

---

#10 [G5] — **The spec's own payoff estimate was invalidly derived. Replaced with
measurement.**

*What happened.* §2.2 estimated an artifact-needing test at
`3,000 ms -> ~1,110 ms = ~2.7x`. Task 4 measured 4.0x and 4.2x on the two
modules that fit that description, which prompted a re-derivation rather than a
celebration — a prediction beaten deserves the same scrutiny as one missed.

*The defect.* The ratio mixed two runs. Its numerator (3,000 ms) came from the
QUIET profile run of §1; its denominator (1,094 ms) came from the CONTENDED
single-build run of §1.2, taken at load 24-32. §10 of this very spec already
recorded those absolutes as inflated roughly 3x and instructed the reader to
trust only the ratios — and then §2.2 built a ratio across the two anyway. That
is the fault `docs/timings.md`'s header exists to prevent ("read host/cores/
cpu_ratio, not the raw seconds, across different machines") and that
`scene_cost.rs`'s module doc names as its own discriminator.

*Derived properly from one run:* the quiet profile's per-world stage totals are
Full 2.996 s and terrain 0.337 s, giving 2.996 -> ~0.352 s, or **~8.5x**.

*So both estimates were wrong in opposite directions*, and the measured 4.0x/4.2x
sits between them because these tests need the climate fit and a locale context
as well as the sculpt.

*Decision.* Replace the estimate with the measured table (Task 3's 41% on a
fact-read-dominated module, Task 4's 4.0x/4.2x on artifact-needing ones), keep
the ~200x figure only where it is what was actually measured — a bare fixture
read against a bare build — and record the derivation error in the spec rather
than quietly restating the number.

*The lesson, which is not "one estimate was closer".* Both were guesses about a
cost this campaign could measure, and the measurement was two commits away the
whole time. The estimate existed only because it was written before the loader
did.

*Cost if wrong.* None; the measured figures are reproducible from the timings
rows the tasks recorded.

*ideonomy passes / overturns.* n/a — an arithmetic correction.

*Capture.* Spec §2.2 (rewritten, with the correction stated); this entry. The
chronicle must carry the measured numbers, never the estimate.

---

#11 [G5] — **I corrected the plan's caller counts with a broken instrument and
made the aggregate worse. The frozen number is 239.**

*What happened.* From Task 3 onward I "re-derived" every caller count with
`grep -o '<helper>()' | wc -l`, minus one for the definition, and rewrote the
plan's figures accordingly — twice, in commits `329add1f3` and in three
dispatches. Task 5's reviewer flagged that two of my counts disagreed with its
own recount. Re-measured with a real word boundary
(`(?<![A-Za-z0-9_])NAME\(\)`):

```
  helper                      plan   my grep   rigorous   who was right
  seam_world  src/session       84        86         86   my grep
  seam_world  tests/session      24        24         24   both
  world       surrounds          31        35         31   THE PLAN
  world       exposure           27        23         23   my grep
  world       session_snapshot   16        17         17   my grep
  world       the_blocking       15        16         15   THE PLAN
  generated(42)                  43        43         43   both
  ------------------------------------------------------------------
  TOTAL                         240       244        239
```

*The defect.* `grep -o 'world()'` matches `world()` **inside** longer
identifiers — `seam_world()`, `played_world()`, anything ending in `_world()`.
So my instrument counted substring occurrences while I reported it as counting
calls. It inflated exactly the two files where such identifiers exist, and my
aggregate (244) ended up **further from the truth (239) than the plan's original
240 was.**

*Why this is worth a ledger entry rather than a quiet fix.* It is the shape my
own standing note names — the observing tool answering a neighbouring question —
and I committed it while warning three implementers about it in their dispatches.
Worse, it has the specific character of being *invisible on inspection*: every
individual number looked plausible, the totals stayed in the right range, and the
two wrong ones were wrong in the same direction as a real correction would have
been. Nothing about the output said "substring".

*What saved it.* Not care — a reviewer's independent recount, on a Minor finding
it could easily have left unstated. That is the second time in this campaign a
reviewer's refusal to accept a stated number caught a controller error.

*Decision.* **239 is the frozen number**, measured once with a word-boundary
regex at `c3f35ef9e`, decomposing as T3 110 / T4 54 / T5 75. It goes in decision
0606 and the chronicle in that form. Every earlier figure in this ledger and in
the plan's prose — 240, 244, 248 — is superseded by it; they are left in place
above rather than rewritten, because the drift is the point.

*And the honest caveat on the caveat:* 239 is a count of syntactic call sites,
not of tests. Several helpers are called more than once per test and some calls
sit in non-test helpers. The number that actually matters is the measured
**567.0 CPU-seconds**, which required no counting at all.

*Cost if wrong.* Low, and bounded: the campaign's result is the CPU-second
measurement; the call-site count is colour.

*ideonomy passes / overturns.* n/a — a measurement correction.

*Capture.* This entry; decision 0606 and the chronicle carry 239 and 567.0.

---

#12 [G6] — **Task 6 close: decisions accepted, book written, retrospective
filed.**

*What happened.* Decisions 0606 and 0607 were flipped from `Proposed` to
`Accepted`, their content finished (0606 gains the closing ceiling — 350 to
334, sixteen points — and the explicit debt-counter-not-performance-metric
statement obligation #2 of the dispatch demanded; 0607 gains the corrected
measured spread replacing the invalid ~2.7x estimate, and the decision-0090
consequence that previously lived only in `b44db18ee`'s commit message), and
their `docs/decisions/README.md` Index rows updated to match. The chronicle
(`book/src/chronicle/the-reservoir.md`) and retrospective
(`docs/retrospectives/the-reservoir.md`) were written and
`book/src/SUMMARY.md` updated. `book/src/open-questions.md`'s "make it fail on
command" bet (scored last by The Avowal, 2026-09-01) was re-scored with this
campaign's two instances — the `FIXTURE`-repointed-to-nonexistent-path
positive control, run twice independently (Task 5), and the controller's own
substring-matching `grep -o` miscounting call sites and making the aggregate
worse (this ledger's own entry #11).

*Freshness sweep.* `grep -rn "build_world\|seed 42" book/src --include=*.md |
grep -v chronicle` turned up only references to seed 42 as an example world
(introduction, religion, language, open-questions) and to `build_world_to`/
`build_world` as historical or production-path facts (`laboratory/overview.md`'s
depth-ladder description, two idea-registry rows). None describe test-suite
build redundancy as current fact, so none needed correction; nothing was
edited beyond the open-questions re-score.

*Step 7 — the branch table.* Measured at this task's own commit, after
0606/0607 flipped to `Accepted`: `make rebaseline` moved `docs/digest/`
only — the in-force decision index and delta report, which is exactly what
adding two newly-`Accepted` decisions to the index does — and nothing in
`docs/audits/`, no byte-golden, and no `book/src/gallery/` path. The first row
of the brief's branch table fired. See this task's report for the pasted
`git diff --stat`.

*Cost if wrong.* Nil — this entry documents completed work, not a decision.

*Capture.* This entry; `.superpowers/sdd/2026-09-02-the-reservoir/task-6-report.md`.

---

#13 [G5] — **The docs-only fast-path is blind to docs regressions that Rust
tests catch. This campaign made ~12 commits through that hole.**

*What happened.* Task 6's chronicle cited registry ID `MAP-25` from
`book/src/chronicle/`, which decision 0031 permits only from
`book/src/frontier/`. `cli/tests/suite/docs_consistency.rs`'s
`the_book_carries_no_registry_ids_or_process_vocabulary` catches exactly that,
**is in `docs/timings/subfloor-roster.tsv`**, and therefore runs in
`gate-commit`. It never fired. Reproduced by hand at rc=101 after the fact.

*Why it never fired.* `scripts/hooks/pre-commit` fast-paths past `gate-commit`
when no Rust-relevant path is staged — it printed "no Rust-relevant paths
staged — skipping 'make gate-commit'" on every docs-only commit this campaign
made, roughly twelve of them. The heuristic reasons about the *staged* paths'
blast radius, and it is structurally blind to the inverse case: **a Rust test
whose subject is a non-Rust file.** `docs_consistency` scans `book/src` and
`docs/`; a change to those is exactly what it exists to check, and exactly what
the fast-path decides needs no checking.

*This is not a bug in the hook's implementation but in its premise.* The premise
is "Rust paths staged → Rust checks matter". The counterexample is a Rust check
whose inputs are prose. Every campaign that lands docs through docs-only commits
has the same hole, and the hole is invisible because the skip message reads as
an optimisation.

*Consequence for this campaign, and it is not hypothetical:* the merge queue
would have reddened on the `gate` phase, after taking the canonical box, for a
one-sentence prose defect that a local gate held the test for and declined to
run.

*Decision.* Fix the citation (fix round 2), and **capture the hole as a
`PROC-*` registry row** rather than changing the hook inside this campaign —
the hook is shared substrate, the change wants its own measurement of what it
would cost every commit, and this campaign has no mandate for it. The cheap
mitigation available to any campaign today is to run
`cargo test -p hornvale --test suite -- docs_consistency` by hand before a
docs-only commit that touches `book/src`; it costs ~1.2 s.

*Cost if wrong.* If the hole is narrower than stated, the registry row is
cheap and gets closed by whoever measures it.

*ideonomy passes / overturns.* n/a — a mechanism finding.

*Capture.* This entry; a `PROC-*` row; the retrospective.

---

#14 [G5] — **I asserted a defect-distribution streak without checking it, and
it nearly landed as a retrospective headline.**

*What happened.* Task 6's dispatch told the implementer "**THIS CAMPAIGN IS THE
NINTH** in a row with that distribution", sourced from The Winze's
retrospective calling itself the eighth. The implementer wrote it as the
retrospective's opening heading. Task 6's reviewer checked what landed in
between and found it false.

*The counterexample.* `docs/retrospectives/the-chattel.md:92` ("Defects by
origin") tabulates **43 controller prose** against **≈68 implementer code**, on
a 14-task campaign that landed between The Winze and this one. Chattel's own
prose says "Four campaigns running have reported the same distribution" — not
eight. So the streak was already broken before I incremented it, and the number
I incremented from was itself describing a shorter run than I assumed.

*The shape.* I incremented a count from a prior document without checking the
interval. No instrument was involved this time — ledger #11 was at least a
broken measurement; this was arithmetic on hearsay. It is the plainest instance
in the campaign of the thing the campaign kept finding.

*And Chattel supplies the deeper correction, which is the part worth keeping.*
Its warning: "Read the denominators before the ratio" — its controller count
was *complete* (every brief verified pre-dispatch, so defects enumerated
exhaustively) while its implementer count was a *floor* (three tasks' reviews
never triaged). The two sides are never measured the same way, in any campaign,
including this one. **So "zero implementer-code defects" here is a statement
about what six reviews FOUND, not about what exists** — and stating it as a
property of the code rather than of the reviewing would be a category error
dressed as a compliment.

*Decision.* The retrospective states what is true of this campaign, drops the
streak entirely, and carries Chattel's denominator warning. No replacement
count.

*Amended by entry #15.* The reasoning above stands and the number it defended
does not. A seventh review — the whole-branch one — found an implementer-code
defect, so the census reads **1**, not 0. The hedge this entry argued for was
honest and correctly scoped, and it was still guarding the wrong figure: what
"six reviews found" was a real statement about six reviews, and a seventh
existed. Read that as support for the hedge rather than against it — the
number moved the moment a new vantage was applied, exactly as the hedge said
it might.

*Cost if wrong.* None; a weaker claim cannot be falsified by the next campaign
the way a streak can.

*ideonomy passes / overturns.* n/a — a factual correction.

*Capture.* This entry; the retrospective's opening rewritten in fix round 2.

---

#15 [G1] — **The final whole-branch review's five findings, and the two
judgement calls its fix wave had to make.**

*Question.* The final review (CHANGES REQUESTED) found one implementer-code
defect and three prose defects in permanent records, plus two minors. Two of
its remedies needed a judgement call rather than a transcription.

*Findings, and what was done.*

1. **Two determinism tests made vacuous by Task 5's migration** (code).
   `generated_worlds_are_deterministic` and
   `glossed_names_are_stable_across_two_builds` in
   `windows/worldgen/src/lib.rs` compare two builds; `generated(42)` returns
   the fixture, so both compared two reads of one file and passed together in
   **0.06 s**. Each now keeps a local `build_world` builder, the remedy
   `windows/vessel/src/session.rs` already applied deliberately. Measured
   after: **11.115 s and 11.142 s** (nextest, per test, two builds each).
   The roster row moved `42 unmigrated:42` → part of `48` with
   `build-path:2` added for these.
2. **`ENTRY_POINTS` omitted `build_world_from_components`** — 9 live sites
   across 5 files, one file (`repose_exposure.rs`) with no row at all. Added;
   sorted before `build_world` so longest-first counting is unaffected.
3. **`fixture.rs` and 0607 asserted an `artifacts` roster row that has no
   rows.** Both now say the reason code is declared and unused, and why it is
   unreachable today (the artifact-needing callers re-derive from the loaded
   world).
4. **0606 and the chronicle inverted which seeds stopped building.** The 43
   seed-42 sites stopped; the ~10 other-seed callers still build. Both match
   `generated`'s own doc comment now.
5. **The retrospective census is 11 / 2 / 1**, not 11 / 2 / 0, with a code
   table naming the timing as what caught it.

*Decision — the ceiling route for finding 2.* Two honest routes existed:
classify the 9 newly-admitted sites on their merits, or raise
`UNMIGRATED_CEILING` with a comment calling it a scan-coverage correction.
**Classified on their merits; the ceiling stays 334.** The sites justified it
individually, which is the deciding fact and not a convenience: two are
production delegations (`windows/lab/src/metrics.rs`'s view-chain root,
`build_world`'s own delegation in `lib.rs`), two assert byte-identity between
this entry point and `build_world` (`build-path`), and five build an identity
no fixture carries — `goblin_solo`'s single-kind component set,
`warren_readout`'s emptied-realm-registry pair, and `repose_exposure`'s seed
sweep. Raising the ceiling would have been the wrong record even though the
sites are genuinely not new debt: `unmigrated` means "nobody has looked yet",
and by the time the question was asked somebody had.

*Consequence — `identity`'s gloss widened by two words.* Its definition was
"a seed or pin set with no committed fixture", and three of the five sites
above are distinguished by a **component set**, not a seed or a pin. The
review had already flagged the same hole one axis over (a `build_world_to`
site at default pins distinguished only by **depth**). The gloss now reads "a
seed, a pin set, a build depth or a component set", in the module doc, the TSV
header and 0606. This is a taxonomy correction, not a reclassification: no
row's reason changed.

*Decision — the roster's stated scope.* Three documents claimed the roster
covered "every world-build call site in the workspace". It never did, in two
independent ways: the missing entry point, and `examples/` being unscanned
(17 sites, zero test-time cost, four of them golden-authoring). Both are now
**stated** rather than implied, in the TSV header and the guard's module doc.
The scan itself is unchanged — extending it to `examples/` is a separate,
larger question about a tree that never runs.

*Decision — finding 6 (minor), the laundering hole.* `total <=
UNMIGRATED_CEILING` permits headroom created by reclassification. Changed to
`assert_eq!`. This makes the documented workflow mandatory and cost nothing
today (334 == 334, zero slack).

*Closed as a non-issue.* The deferred "TSV sort-order violation at ~line 32".
`LC_ALL=C sort` on the path column yields zero differing lines; the report
came from a locale-aware collation that ignores `_` and `.`, and `roster()`
reads into a `BTreeMap`, so file order is not load-bearing at all. **No
committed document carried the claim**, so nothing needed correcting — it
lived only in review scratch. Re-verified after this wave's edits (the new
`repose_exposure.rs` row sorts correctly under `LC_ALL=C`).

*Cost if wrong.* The ceiling route is the reversible half — a later campaign
that disagrees can reclassify any of the 9 to `unmigrated` and raise the
constant, and the roster will tell it exactly which rows to look at. The
`assert_eq!` is the irreversible-feeling half and is not: it only ever forces
a number to be edited in the commit that earned the edit.

*ideonomy passes / overturns.* n/a — remediation of a review, not a design
choice, except for the ceiling route, where the deciding argument was that
`unmigrated`'s meaning ("nobody has looked") is falsified by the act of
asking.

*Capture.* This entry; decisions 0606 and 0607; the chronicle's new
whole-branch section; the retrospective's code table; spec §5 residue item
(4); `world_build_sites.rs` and the TSV header.

---

#16 [G6] — **In-place edits to this campaign's own decision records: allowed,
with the boundary named.**

*Question.* The final fix wave edited decisions 0606 and 0607 in place.
`docs/CLAUDE.md` and `docs/decisions/README.md` both say decisions are
append-only: "never edit a ratified decision's substance; supersede it with a
new record." Was that permitted, or does it need a 0608 erratum?

*Ruling: the in-place edits stand.* Three reasons, and the third is the one that
decides it.

1. These are **this campaign's own records, authored hours earlier on an
   unmerged branch**. They have never existed on `main`; nothing in the
   repository or outside it can have read or cited them.
2. The edits **correct statements that were false at ratification** — a wrong
   filename, an inverted claim about which call sites stopped building, a
   present-tense assertion about an `artifacts` roster row that has zero rows.
   They do not revise a choice that was right and later became wrong.
3. **That distinction is what append-only exists to protect.** The rule guards
   against rewriting a record a reader relied on, so that the log stays a
   truthful history of what was decided when. A record that was never published
   and was wrong on the day it was written has no such history to protect; an
   erratum correcting a same-day, never-merged 0606 would make the log *harder*
   to read while adding no fidelity.

*The boundary, stated so nobody re-derives it.* This holds **pre-merge only**.
Once these records land on `main`, they are immutable: any later correction —
including of something that was false all along — needs a superseding record.
The fix-wave implementer and the final re-reviewer reached this independently,
and the re-reviewer suggested a one-line addendum to `docs/CLAUDE.md`'s
append-only rule naming "not yet merged to `main`" as the boundary.

*That addendum is NOT made here.* `docs/CLAUDE.md` is shared substrate that
every campaign reads, this campaign has no mandate to amend it, and a rule
change wants its own review rather than riding in on a fix wave. Captured as a
follow-up for Nathan instead — see the G6 package.

*Cost if wrong.* Bounded and visible: if Nathan reads the rule strictly, a 0608
erratum can be added on top without unwinding anything, because the edited text
is honest about being a correction.

*ideonomy passes / overturns.* n/a — a governance ruling on precedent.

*Capture.* This entry; flagged in the G6 package; the `docs/CLAUDE.md` addendum
left to Nathan.

---

#17 [G6] — **The ratchet caught two world builds that landed on `main` during
this campaign. The branch cannot be submitted until it absorbs them.**

*How this was found.* `git merge-tree origin/main HEAD` reports **CLEAN** — no
textual conflict across the 108 commits `main` has taken since our base
`25ee1d830`. That is exactly the reassurance CLAUDE.md warns not to trust:
"No gate has an opinion about whether two campaigns changed the same idea in
incompatible ways." So the campaign's own instrument was run against the merge
product's inputs instead, scanning `origin/main` for build sites and comparing
three ways — base, main, ours.

*Two genuinely new sites on `main`, neither visible to merge-tree:*

```
  cli/src/main.rs                          base 9  ->  main 10   (+1)
  windows/worldgen/src/circuit_readout.rs  base 0  ->  main  1   (NEW FILE)
```

Six other rows differ only because **we** migrated them and `main` has not seen
that work — `surrounds.rs`, `session.rs` (both), `session_snapshot.rs`,
`the_blocking.rs`, `exposure.rs`. Those are not conflicts; separating them from
the two real ones required comparing against the base rather than against our
roster, which is why the first pass read eight and the correct answer is two.

*Consequence.* After the merge, `world_build_sites::no_unrostered_world_build_appears`
will refuse the product: one file with a build and no row, one row one short of
its file. The merge queue would have found this — on the canonical box, after
taking the staff, in the `gate` phase.

**This is the mechanism working, and it is the campaign's first live proof.**
The ratchet exists to refuse a world build that arrives without a reason. Two
arrived on `main` while this campaign ran, from other sessions that had no way
to know the roster existed, and the guard caught both before a merge attempt.
The instrument found its own first real defect on the day it shipped.

*Ruling: absorb `main` before submitting, then add the two rows on their merits
— and expect `UNMIGRATED_CEILING` to RISE.* Unlike Fix 2's scan-coverage
correction, this is **genuinely new debt**: two builds that did not exist when
the ceiling was set. A ratchet that cannot admit new debt arriving from
elsewhere is not a ratchet, it is a wall that the next merge breaks. So the
ceiling rises by however many of the two are `unmigrated` after a human reads
them, and the commit says so.

*Not done here.* Absorbing 108 commits is a merge, and G6 is a hard stop. The
absorb, the two rows, and the re-gate are presented to Nathan as the campaign's
one outstanding action rather than performed unasked.

*Cost if wrong.* If the absorb surfaces further semantic drift beyond these two
rows, it surfaces locally and cheaply rather than in the queue.

*ideonomy passes / overturns.* n/a — a mechanism result.

*Capture.* This entry; the G6 package's lead action item.

---

## Parked findings

### P1 — `scene_surrounds_colour_cli.rs` uses a fixed temp path and flakes

`cli/tests/suite/scene_surrounds_colour_cli.rs:42` writes its world to
`std::env::temp_dir().join("hv-scene-surrounds-colour-test.json")` — a fixed,
unsuffixed name shared by every concurrent run of that test on the box.

Observed 2026-09-02 during this campaign's measurement pass, on a full-workspace
run: the test asserted `new` succeeded — so the CLI *had* written the file — then
panicked at line 80 with
`world.json reloads: Os { code: 2, kind: NotFound, message: "No such file or directory" }`.
Under the default fail-fast profile it took the whole run down at test 274 of
4869, which is how this campaign ended up with a 285-test sample instead of a
full census of build sites.

Not caused by this campaign's instrumentation: the probe appended only to an
absolute path under the session scratchpad and never touched that filename.

Unrelated to the reservoir work and **not fixed here**. Fix shape: a per-process
suffix, or `CARGO_TARGET_TMPDIR`.

### P2 — nothing pins the derived artifacts' bytes

`GeneratedTerrain` and `GeneratedClimate` are not `Serialize`, and no committed
artifact pins the terrain sculpt byte-for-byte. Pre-existing; this campaign
neither creates nor worsens it, but §5 of the spec has to name it as the one
real coverage residue that fixture-loading does not cover. Worth a `TOOL-*` row
if the registry does not already carry one.

---

## Capture manifest

- **The scratch ledger was created and then removed.** This campaign first wrote
  `.superpowers/sdd/decision-ledger.md`, following `campaign-autopilot`'s own
  instruction. That instruction is **superseded by The Cartulary** (2026-08-30):
  the root `CLAUDE.md` names that exact path as the collision-prone one — every
  campaign's scratch ledger lived at the identical path, so two campaigns editing
  it merged to one side silently — and rules that rulings, deferred minors and
  parked findings go in the committed per-campaign ledger *as they occur*. The
  scratch files were deleted and their content folded in here. Recorded because
  the skill and the doctrine currently disagree, and CLAUDE.md wins; the skill
  text is worth a `PROC-*` correction.
- **Two parked findings, P1 and P2 above** — the fixed-temp-path flake, and the
  absence of any byte pin on the derived artifacts. Neither is fixed here.
- **No new idea-registry row for the in-process-memo rejection.** Decision 0032
  already records it in terms this campaign only confirms: "cargo-nextest (its
  process-per-test model re-initialises the `LazyLock` census once *per test* —
  strictly worse for this suite, not better)". The repo holds the fact; a row
  would duplicate it.
- **MAP-25 relation.** The depth-scoping half of this problem is MAP-25, already
  `shipped` as the `BuildDepth` ladder. This campaign does not reopen it; spec §7
  makes depth-scoping an explicit non-goal so the two do not blur.
