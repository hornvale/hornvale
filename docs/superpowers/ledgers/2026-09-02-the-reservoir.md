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
