# The Governor — design

**Campaign:** The Governor (rung after The Sources)
**Date:** 2026-08-28
**Branch:** `campaign/the-governor`, based on `2f8faf243`

A governor is the mechanism that regulates how fast an engine may run and
stops it running away. This campaign is about the heavy tier: what it costs,
what it is *for*, and whether it throttles the merge queue.

---

## 1. The problem, stated from measurement

Decision 0148 took `heavy` and `seam-guard` off the merge phase list because
together they were 80.5% of a merge's wall time. That removed the only
automatic dispatcher the heavy tier had. Nothing replaced it. Since then the
tier has run when a human remembered — and it has accumulated ten failures
that nobody saw until The Sources ran it by hand at close, then nearly
mis-attributed nine of them to itself.

The tier's life cycle is broken at one specific joint:

```
  1 AUTHORED --> 2 DEFERRED --> 3 DISPATCHED --> 4 READ
      ^                              ^                |
      |                              |                v
  7 (decays) <- 6 DISCHARGED <- 5 ATTRIBUTED <--------+
                                 NOBODY MOVES IT FROM 2 TO 3
```

**So the campaign's question is not "how do we make heavy fast".** It is
"what re-enters the cycle at phase 3, and what must the tier cost for that
dispatcher to be affordable". Cost is instrumental. That ordering is why the
cost work comes first and the gating decision comes last.

### 1.1 Measured baseline

Two heavy runs on the canonical box, three days apart, different SHAs:

```
                          a56d91320        2f8faf243 (main today)
nextest wall              1571.159 s       1551.631 s
tests                     118              118
failures                  11               10
argmin (the pole)         913.208 @46/118  891.732 @46/118
mire battery (exclusive)  144.776 @47/118  148.579 @47/118
timed.sh wall / user      1593 / —         1622.132 / 22137.994
wrapper (git+claim+build) —                1886 s
```

Sources: `/tmp/hornvale-heavy/heavy-20260828T002634Z-4075724.log` and
`/tmp/hornvale-heavy/heavy-20260828T145124Z-548769.log` on lefford;
`/tmp/hornvale-heavy/runs.tsv`.

**No `heavy` row has been written to `docs/timings.md` since 2026-08-05.**
The tier's own ledger is blind to its last five runs. That is a defect in its
own right and Stage 6 fixes it.

### 1.2 The wall decomposes into three serial segments, not one pole

`the_mires_preregistered_readout` carries `threads-required = "num-cpus"` in
`.config/nextest.toml`, so it reserves the entire runner. nextest therefore
drains every running test — including the 891 s pole — before starting it,
then restarts the remaining 71 tests from cold:

```
  t 0    -> 891   argmin runs                      (completes 46/118)
  t 891  -> 1040  mire battery runs ALONE          (completes 47/118)
  t 1040 -> 1552  remaining 71 tests, cold restart (completes 118/118)
```

Index 48 onward on both runs are 2-8 s tests, which is the cold-restart
signature. The optimum for this roster is
`sum(exclusive) + max(pole, CPU/cores)` ~= 1068 s, so the barrier tax is
**484 s and 485 s on the two runs** — stable, and larger than any single
test except the pole.

**Verified, not argued.** Seven sleep-tests, pool of 4, one
`threads-required = "num-cpus"` test placed mid-order:

| arm | config | wall | barrier's completion index |
|---|---|---|---|
| A | no `priority` (today's shape) | **10.076 s** | 3/7 |
| B | barrier `priority=100`, long test `priority=50` | **8.041 s** | 1/7 |

Positive control: `priority = 500` is rejected
(`invalid type: 64-bit integer 500, expected an signed 8 bit integer`), which
proves nextest was reading the file rather than ignoring an unknown key.
`cargo-nextest 0.9.140` on both hosts, so a config change behaves identically
in both places.

---

## 2. The four levers, and the one that is rejected

### 2.1 REJECTED — deduplicate the redundant world builds

Four probe binaries each declare `const PANEL: [u64; 12] = [0..11]` and their
own private `fn build(seed)` calling `build_world` at full depth: the same
twelve worlds built four times.

**Measured against its own control and cut.** `undertow_readout` has the
identical `for seed in PANEL { build_world(...); read_world(...); ... }` shape
at 40 seeds / 426.651 s = **10.7 s per seed**, against argmin's 12 seeds /
891.732 s = **74.3 s per seed**. So the world build is ~10.7 s and ~86% of
argmin's per-seed cost is its own measurement. The 48 redundant builds are
~515 CPU-s of a 12,280 s tier — **~4%** — and removing them requires an
on-disk fixture, because nextest is process-per-test and no in-process cache
can survive between tests.

Worst cost-to-benefit in the campaign. Recorded here so it is not
re-proposed.

### 2.2 LEAD LEVER — demote calcified campaign instruments to `probe:`

**The tier already exists and the practice is decision-backed.**
`scripts/gate-full-heavy.sh:62` greps `#[ignore = "heavy:` and nothing else,
so every other reason token is already run-by-hand:

```
  118  #[ignore = "heavy:      <- the automatic tier
   43  #[ignore = "probe:      <- already off-gate
    3  search:   3  measurement:   2  timekeeper:   1  readout:   1  calibration:
```

Decision 0148 — the same decision that took heavy off the merge path — already
demoted The Scatter's two fare batteries, and their tags are the template:

```rust
#[ignore = "probe: the-fare exploratory/pilot readouts over a live-worldgen
            battery; run by hand (decision 0148 took them off the heavy set)"]
/// claim: readout(preregistered) — off-gate (probe:, so no gate runs it at all)
```

This was applied to two tests and never continued, while 118 accumulated.

**The reds and the cost are the same population.** Nine of the ten failures
are question-named campaign instruments —
`how_much_the_twelve_seed_prefix_over_reads`,
`whether_the_tiebreak_or_the_contact_pooled_the_accounts`,
`which_way_the_account_crosses_the_seam`,
`does_the_crossing_penalty_change_the_non_argmin_defect`, and four `_readout`s.
A test named as a question was written to answer that question once. The argmin
file's own module doc concedes its pinned figure already moved: *"THE
CONCENTRATION FIGURE IS RE-DERIVED AND IT MOVED ... The Underworld changed
settlement placement."* The tenth,
`the_sub_floor_raider_reading_is_pinned_as_a_witness`, says in its own name
that it is a witness. That one stays.

**Six** heavy tests live in files with **zero assertion macros anywhere in the
file**, and contain only `println!`s plus build/lookup preconditions:

```
windows/hearsay/tests/suite/probe_filter_mismatch.rs
windows/hearsay/tests/suite/probe_filter_variation.rs
windows/hearsay/tests/suite/probe_lossy_quadrants.rs
windows/hearsay/tests/suite/probe_stance_cost.rs
windows/worldgen/tests/suite/delver_depth_probe.rs
windows/worldgen/tests/suite/warren_liebig_probe.rs
```

None delegates to an assertion helper. They can fail only if world
construction itself panics through an `expect(` — or through a `_ =>
panic!(...)` match arm on a build precondition, which is the same thing
written differently. `delver_depth_probe.rs:116` says so in its own words:
*"This test asserts nothing at all: every check in it is a build/lookup
`expect`."* So they assert nothing whatever about their own findings, and
they have been running in a gate tier.

*(**This number has been wrong four times and the sequence is the lesson:
3, 2, 5, "at least 9", 6.** (1) A body-scoped `assert!` regex gave three.
(2) Correcting to file-wide gave two -- but the scan behind it ranked heavy
tests by `println!` count and file-wide-verified only the top three, so a
zero-assertion test with few prints could never enter the candidate set; the
claim was tier-wide and the evidence covered a ranked prefix. (3) An
exhaustive file-wide scan gave five. (4) An adjudicating agent, warned that
the number had been wrong twice, counted exhaustively and applied the wrong
PREDICATE -- it scored tests with no assertions in a file that has some,
which is a different unit, and reported "at least nine". (5) Six is the
current answer: five, plus `delver_depth_probe.rs`, which the exhaustive scan
missed because that scan's regex counted `panic!` as an assertion. It is not
one -- this section's own definition of the class says these tests fail only
through a build precondition, and a `_ => panic!(...)` arm is exactly that.
**Each correction fixed the previous error's mechanism and introduced a new
one at a different layer** -- scope, then sampling, then unit, then
predicate. The filter authorises a demotion with NO adjudication, so it is
the one place in this spec where being approximately right is not good
enough.)*

### 2.3 Front-load the barrier (`priority`)

Config only, mechanism verified in §1.2. Worth ~484 s on today's roster and
proportionally less once the roster shrinks — but it is the one lever whose
value does not depend on any adjudication, so it lands early and cheaply.

### 2.4 Roll out `map_seeds` to the surviving seed-panel witnesses

`windows/lab/tests/seed_sweep/mod.rs` already provides an ordered parallel
seed sweep — `std::thread::scope`, results reassembled by seed position not
completion order, `HV_SEED_SWEEP_THREADS=1` reproducing the serial loop
exactly for a byte-identity proof, no new dependency. Its module doc states
this campaign's diagnosis verbatim. The Scatter wrote it for three lab
batteries; nine-plus seed-panel tests across `windows/hearsay` and
`windows/worldgen` never adopted it, and use `map_seeds` zero times.

**This lever has a blocker with a name, found by reading rather than
guessing** — see §5.

---

## 3. The demotion rule

There is **no existing mechanical classifier** for this, and the spec must not
pretend otherwise. The `claim:` vocabulary (`invariant(` 194, `structural(`
135, `readout(` 67, `rate(` 48, `reachability(` 37, ...) is The Assay's
*seed-loop quantifier* lint — it records whether a seed loop is a search or a
fixed panel, not whether a test asserts a witness. `claim_shape.rs`'s own
module doc says it "cannot check that a declared shape is the RIGHT one — that
is a review question." Joining the heavy roster against `claim:` kind confirms
it is the wrong axis: the five largest failures are all tagged `structural(`.

So the rule is an adjudication with a stated test, applied per test, recorded
in the tag.

### 3.1 The question

> **If this test went red tomorrow, what would we do?**
>
> - *"Investigate a regression in the world or the program"* — it is a
>   **witness**. Stays `heavy:`.
> - *"Note that the number moved and update it, because the question this test
>   asked was answered in campaign X and the pin is just the answer we
>   recorded"* — it is a **report**. Demote to `probe:`.

**The burden is on keeping a test in `heavy:`.** A test with no answer to the
first branch is demoted. This direction is deliberate: the tier's failure mode
for eleven weeks has been silent accumulation, and a rule whose default is
"keep" reproduces it.

### 3.2 Mechanical pre-filters

These make the adjudication cheap and auditable; they do not replace it.

- **Zero assertions ⇒ demote, no adjudication needed.** A test that cannot
  fail witnesses nothing. **Counted file-wide, not body-scoped, and after
  checking the body's call list for assertion helpers** — a body-scoped literal
  `assert!` count is not sufficient evidence, and produced a wrong number
  four times on this very spec (3, 2, 5, "at least 9"). **Six tests qualify
  today.** The scan that establishes it must be exhaustive over every
  heavy-bearing file — not a ranked prefix — and must count the right thing:
  **assertion macros (`assert!`/`assert_eq!`/`assert_ne!`/`debug_assert*`),
  file-wide.** A `panic!` or `expect` reached only from a build or lookup
  precondition is NOT an assertion and does not disqualify a file; a test
  with no assertions of its own, in a file that has some, is a DIFFERENT unit
  and does not qualify for the automatic path at all — it gets an ordinary
  adjudication.

  **The automatic path may not be reached by interpretation.** This filter is
  the only rule in the spec that demotes a test without anyone arguing the
  case, so its trigger stays mechanical on purpose. Do not broaden it to
  "asserts something about its own results", however tempting: that phrasing
  swallows non-vacuity guards by judgement, which is precisely how an
  automatic path eats cases that were supposed to be argued. Where a test is
  genuinely assertion-free but its file is not, reach the same verdict the
  long way and say so in the reason. See §2.2 for all four wrong numbers and
  the distinct mechanism behind each.
- A test whose name is a **question** (`how_`, `which_`, `whether_`, `what_`,
  `could_`, `is_the_`) is a demotion *candidate* and must be adjudicated
  explicitly. It is not auto-demoted — `which_way_the_account_crosses_the_seam`
  might still pin something load-bearing, and the rule must be able to say so.
- A test whose name asserts a **witness** or a **bound** is a keep candidate.

**The 71-test / 7,955 s figure from the name filter is a CEILING on the prize,
never a roster.** It over-captures: it sweeps in `radiation_readout`, which is
assertion-shaped. Any task brief that quotes 71 as a target is wrong.

### 3.3 Anti-silencing — the discipline that makes this safe

The Sources' retrospective states the governing rule: *"A regeneration with no
named cause is a silencing."* **Demotion with no named cause is the same shape
and this campaign must not commit it.** Therefore:

1. Every demotion tag names the **campaign whose question it answered**, in
   the style of the 0148 fare tags.
   **Carve-out, and it is narrow: some heavy tests never answered a campaign's
   question at all.** `kernel::ledger::tests::bench_commit_scaling_before_vs_after_index`
   is a wall-time micro-bench of `Ledger::commit`, and its own comment says it
   is in the tier only because `heavy_tier_reason_strings_are_canonical`
   requires every `heavy:` reason to be one verbatim string, so it shares that
   string with the live-worldgen batteries. It has no campaign to name. Such a
   tag names **the decision or convention that put the test in the tier**
   instead — here decision 0132. Do not invent a campaign to satisfy the
   clause; a false attribution is worse than a named convention.
   *Found by a reviewer reading this clause against the finished table, not by
   the clause itself: the rule assumed every heavy test descends from a
   campaign's question, and one does not.*
2. A test that is **currently red** and is being demoted must say so *in the
   tag*, and say why that is acceptable — normally "the pin records an answer
   from campaign X; the world has since moved; the question is closed." A
   demotion that hides a live red is a silencing wearing this campaign's
   clothes.
3. **No fixture is regenerated to make a demoted test green.** Demotion and
   repair are different acts; mixing them would let a real regression exit the
   tier under cover of a cost campaign.
4. The adjudication for every one of the 118 is written down — kept or
   demoted, with its reason — and the ones that were *kept* are the half most
   worth reviewing, because keeping is the silent choice.

---

## 4. Anti-re-accretion: the `heavy:` tag is unpriced

Nothing today stops a campaign adding a 900-second battery to the tier, and
nothing charges it for doing so. That is the upstream cause of every number in
this campaign, and without a ratchet the campaign gets re-run in three months.

The pattern already exists twice in this tree —
`cli/tests/fixtures/top-level-test-binaries.txt` freezes the roster of
top-level test binaries (13 had crept back before it was written), and
`heavy_tier.rs::the_untokenised_ignore_reasons_are_exactly_this_roster` freezes
the untokenised-ignore roster.

**Freeze the `heavy:` roster the same way.** Adding a `heavy:` tag then
requires editing a committed fixture in the same commit: a deliberate,
reviewable act with a visible diff, rather than a tag nobody sees.

Chosen over a **total-wall ratchet** deliberately: a wall budget is a committed
baseline, and this project's own standing guidance is that a committed baseline
is a claim with a date. Roster membership does not decay; a duration does.

---

## 5. The blocker Stage 4 walks into

`cli/tests/suite/heavy_tier.rs::the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps`
is a strict two-way `assert_eq!` between `.config/nextest.toml`'s
`# class: scatter-sweep` filter and **every** test calling
`seed_sweep::map_seeds(` — with its own anti-vacuity assertion so it cannot
quietly stop checking.

So adding `map_seeds` to nine hearsay/worldgen tests **fails that guard** until
each is also given `threads-required = "num-cpus"` — which would create nine
full-drain barriers and make the tier catastrophically worse than it is now.
The guard is correct and must not simply be widened.

**The amendment:** a second, *sized* class. The existing class exists because
The Scatter's batteries sweep 200 seeds and genuinely want the whole box. A
12-seed panel does not; it wants twelve slots. `threads-required = <int>` is
supported (verified alongside `priority` in §1.2), so:

- `# class: scatter-sweep` — `threads-required = "num-cpus"`, unchanged, for
  batteries that saturate the box.
- `# class: sized-sweep` — `threads-required = <panel width>`, for panel tests
  parallelised by this campaign. No full drain; no barrier.

Both classes keep the two-way agreement property, checked independently, in
both directions, against their own class marker — which is the shape the file's
two existing guards already use.

**Second-order:** the guard detects parallelism by the literal
`seed_sweep::map_seeds(` call text, and its own module doc admits a hand-rolled
`std::thread::scope` is invisible to it. So wherever `map_seeds` ends up
hosted (§8), the call spelling is load-bearing for a guard, and changing it
silently disarms that guard. This must be stated at the guard, not only here.

---

## 6. Stages

Each stage ends with a heavy run on the canonical box (~26 min queued), because
every claim in this campaign is about wall time and none of them can be
verified locally.

**Stage 0 — The two census redundancies.** Independent of everything below and
runnable in parallel with it. Both confirmed present at `2f8faf243`:

- `spearman_defensibility_capacity` (`windows/lab/src/metrics.rs:5540`) calls
  `connection_graph_of(v.world(), ...)`, which re-derives terrain and climate
  through `crate::terrain_of` / `crate::climate_from`
  (`windows/worldgen/src/graph_derive.rs:193-200`) while the `FullView` it was
  handed already holds both. Fix: a `connection_graph_from(world, &terrain,
  &climate, cfg)` adapter that `connection_graph_of` also delegates to — the
  adapter split is already the file's own idiom (`connection_graph` /
  `connection_graph_at` / `connection_graph_of`).
- `hornvale_worldgen::demography_report_from` is called at `metrics.rs:2431`
  and `:2476` from two separate metric closures, rebuilding the identical
  report.

Both are pure plumbing; a correct fix is byte-identical, so no census refresh
is needed and a moved golden means the change is wrong.

**Why it is in this campaign rather than its own.** It shares the subject —
an expensive tier costing more than it needs to — and, more decisively, its
verification window is **open now and perishable**: piece A's whole correctness
proof is "the census goldens do not move", and the goldens were independently
confirmed current today (§9). Every census that lands between now and whenever
this is picked up re-opens the question of what the fix is being compared
against. A confirmed-current baseline is an asset that decays.

*Success:* `make lab-diff` shows no metric moved and the live-probe fixture
test stays green; the census's measured cost falls.

**Stage 1 — Front-load the barrier.** `priority` on the exclusive tests in
`.config/nextest.toml`. Config only, no adjudication, mechanism already
verified. Ships first because it is the cheapest thing that moves the number
and it de-risks the rest.
*Success:* heavy wall falls ~480 s; the exclusive battery's completion index
moves to the front; no test changes result.

**Stage 2 — Adjudicate all 118.** Produce the kept/demoted table with a reason
per test, against §3. This is the campaign's substance and it is review work,
not code. No tags change in this stage — the table is reviewed first.
*Success:* every one of the 118 has a recorded verdict and reason; the kept set
is defensible test-by-test.

**Stage 3 — Apply the demotions.** Tag edits only, in the 0148 template, with
§3.3's discipline. No fixture regenerated, no test body touched.
*Success:* heavy roster shrinks; the demoted tests still run by hand; the reds
remaining in `heavy:` are only those adjudicated as witnesses.

**Stage 4 — Ratchet the roster.** The committed fixture plus its guard (§4),
and the `docs/timings.md` recording gap from §1.1.
*Success:* adding a `heavy:` tag without editing the fixture fails; verified by
actually adding one and observing the refusal, not by reasoning that it would.

**Stage 5 — `map_seeds` on the surviving panel witnesses**, plus the guard
amendment in §5.
*Success:* byte-identical output proven via `HV_SEED_SWEEP_THREADS=1` against
the serial result for each converted test; both `threads-required` classes
agree with their rosters in both directions.

**Stage 6 — Diagnose the reds that remain**, each with a named cause, per The
Sources' rule. Bisection before repair; classification, not confirmation.

**Stage 7 — The gating decision (C), and its decision record.** Taken against
the measured post-Stage-5 cost, not a projection.

Stage 0 is independent of all of them and touches no file any other stage
touches (`windows/lab/src/metrics.rs`, `windows/worldgen/src/graph_derive.rs`
against scheduling config, `#[ignore]` strings and test bodies). Stages 1-4 are
independent of 5-6 and may interleave; 7 depends on 1-6.

---

## 7. What C will be decided against

Deliberately not pre-judged here — the point of the ordering is that the number
exists before the decision. The options, and what each one's *real* argument
is:

| form | mechanism | the actual argument for it |
|---|---|---|
| every merge | heavy back on the merge phase list | strongest signal; cost falls on the queue, paid by every campaign waiting |
| **stage gate** | a required `stage`-rung phase | cost falls on the campaign that caused it, not on bystanders |
| **conditional** | only when the diff reaches world-generating code | **symmetry**, not cost: today a campaign can move shipped world values, pass every gate it is asked to pass, and leave the tier red for the *next* campaign to inherit and mis-attribute. That is exactly what bit The Sources. |
| scheduled | `scripts/scheduled/` | has a demonstrated failure mode in this repo: it was written and **never installed**, and standing guidance already warns against closing a campaign on "the nightly was empty" |

Note the conditional row: the handoff framed it as a cost optimisation. It is
not — it is the only form that removes the author/inheritor asymmetry, and that
is a better argument than the one it was given.

Projected costs, to be replaced by measurement:

```
  today                                        1551.6 s
  + Stage 1 (barrier front-loaded)             ~1070 s
  + Stage 3 (demotion, no code change)          ~610 s
  + Stage 5 (map_seeds on survivors)            ~290 s   (~5 min)
```

**These are projections and the spec says so.** The `~290 s` assumes the
adjudication demotes roughly the population the name filter suggests, and §3.2
already warns that filter over-captures. If Stage 2 keeps far more than
expected, Stage 7's decision changes, and that is the correct behaviour rather
than a failure of the plan.

---

## 8. Open questions, flagged for review

**8.1 Where does a shared `map_seeds` live?** It is currently a test-only
module in `windows/lab/tests/`. `windows/hearsay` and `windows/worldgen` tests
cannot reach it. The workspace is `members = ["kernel", "domains/*",
"windows/*", "cli"]`, so a test-support crate is neither a domain nor a window
and would need an `architecture.rs` amendment. Candidates: a `pub mod
seed_sweep` in `hornvale-worldgen` (the composition root; every panel test
already dev-depends on it); the same in `hornvale-kernel` (which already owns
`Seed`/`Stream`, but is constitutional); or duplicating the module per crate
(rejected — this tree fails builds on second copies of a single source of
truth). **Interacts with §5's literal-call-text detector.** Not decided here.

**8.2 Could the read-only tier run off the canonical box entirely?**
`heavy-run.sh` carries the canonical-host guard because *three* of its tests
write committed artifacts and one compares against lefford-authored fixtures.
That is an argument about four tests, not 118. Decision 0090 (The Pyx) measured
a 40-world, all-metric probe **byte-identical between x86_64/Linux and
aarch64/Darwin** after libm landed. If that holds for this tier's pins, the
other 114 could run on the Mac, concurrently with the queue, never taking the
serial mutex — which would make C nearly free.

**This is exactly the shape of an inherited constraint that ages into an
apparent requirement after its producer is removed, and it needs its own probe,
not an assumption.** Explicitly out of scope for this campaign's stages; raised
because deciding C without knowing it is deciding with a missing option.

**8.3 Does the mire battery still need the whole box?** It is 148.6 s now,
against the ~2427 s that motivated `threads-required = "num-cpus"`. A sized
class might serve it too. Cheap to test once §5's machinery exists.

---

## 9. Determinism and save-format impact

**None intended, and the campaign is structured so that "none" is checkable.**

- Stages 1-4 change no production code and no test bodies: scheduling config,
  `#[ignore]` reason strings, and a committed roster fixture.
- Stage 5 changes test bodies only, and `map_seeds`' contract is that output
  order is seed order rather than completion order. Each conversion is proven
  byte-identical against its own serial result via `HV_SEED_SWEEP_THREADS=1`.
- No seed labels, no stream consumption order, no committed artifact, no
  quantization boundary is touched.
- If any golden moves in any stage, the change is wrong. That is the same
  correctness proof The Sources' piece A was given, and it is available here
  because the census baseline was independently confirmed current: the
  2026-08-28T13:23Z census on `2f8faf243` pushed no `census/*` branch, and
  `fixture_staleness::census_fixtures_match_a_probe_of_live_seeds` — red at
  `a56d91320` — is **green** on today's main.

---

## 10. Definition of Done

- All seven stages complete, each with its heavy run recorded in
  `docs/timings.md` (closing §1.1's gap).
- Decision record for C, superseding/amending 0148. Numbers reserved with
  `make decision-block`.
- The kept/demoted adjudication table committed as a durable artifact, not
  left in campaign scratch — the *kept* half is the part a future reader needs.
- Chronicle entry (`book/src/chronicle/the-governor.md`) and a freshness sweep;
  Confidence Gradient re-scored if this moves a bet.
- Retrospective (`docs/retrospectives/the-governor.md`).
- Idea-registry rows for 8.1, 8.2, 8.3 whatever their outcome.
