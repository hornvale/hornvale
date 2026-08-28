# The Governor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the heavy test tier cheap enough that a dispatcher is affordable, then decide which dispatcher gates what.

**Architecture:** Four independent levers applied in cost order — front-load nextest's exclusive-test barrier (config), demote calcified campaign instruments from `heavy:` to the existing `probe:` tier (tag edits), ratchet the roster so the tier cannot silently re-accrete, and parallelise the surviving seed-panel witnesses with the existing `map_seeds` helper. A fifth, independent stage fixes two confirmed census redundancies. The gating decision comes last, against a measured number.

**Tech Stack:** Rust 2024, `cargo-nextest` 0.9.140, `std::thread::scope` (no new dependencies — the workspace allowlist is `serde`/`serde_json`/`libm` only).

**Spec:** `docs/superpowers/specs/2026-08-28-the-governor-design.md`

## Global Constraints

- **No new dependencies.** Workspace allowlist is `serde`, `serde_json`, `libm` (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`, decision 0004 as amended by 0041).
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only, enforced by `clippy.toml`.
- **No wall-clock time.** Enforced by the same lint.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and variant needs a one-line doc comment.
- **`cargo fmt` is the last step before every commit.** Fmt-gate skips are this project's most common review finding.
- **`--no-verify` is forbidden, without exception.** If a hook refuses, the refusal is the finding.
- **`make gate-commit` before every commit that touches Rust.** The pre-commit hook runs it for Rust-relevant paths.
- **No golden may move in any task of this plan.** If one does, the change is wrong — see each task's decision table.
- **Push the branch at every task boundary.** An unpushed branch is invisible to the mouth, the chamber, and every peer.
- **Absorb main at every plan-stage boundary** via `make sluice-stage BRANCH=campaign/the-governor REF=<full-sha>`.

## Before dispatching ANY task

**Verify the task's brief against the tree immediately before dispatch, not at plan-authoring time.** Every line number in this plan was correct on 2026-08-28 and will rot. In The Sources every defect but one originated in plan or brief prose rather than implementer code. Specifically: re-run the greps a task quotes and confirm the counts still match before handing the task to anyone.

**Line numbers marked `[VERIFY]` are known-perishable anchors.** Re-derive them with the grep given beside them.

---

## File Structure

| file | responsibility | tasks |
|---|---|---|
| `.config/nextest.toml` | test scheduling: priority, `threads-required` classes | 1, 8 |
| `cli/tests/suite/heavy_tier.rs` | two-way guards binding config rosters to source facts | 1, 7, 8 |
| `cli/tests/fixtures/heavy-roster.txt` | **new** — the frozen `heavy:` roster | 7 |
| `windows/worldgen/src/graph_derive.rs` | `connection_graph_from` adapter | 2 |
| `windows/lab/src/metrics.rs` | the two redundant call sites | 2, 3 |
| `docs/audits/heavy-tier-adjudication.md` | **new** — the kept/demoted verdict table | 4a–4c, 5 |
| `docs/generated-paths.txt` | declares the new audit file by name | 4a |
| the 118 test sites | `#[ignore]` reason strings only | 5 |
| `windows/*/tests/**` | `map_seeds` conversions | 9 |
| `docs/decisions/` | the C decision record | 11 |

---

## Task 1: Front-load the exclusive-test barrier

**Files:**
- Modify: `.config/nextest.toml`
- Modify: `cli/tests/suite/heavy_tier.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: a `# class: front-loaded` marker convention in `.config/nextest.toml`, read by the guard added here and reused by Task 8.

**Why this task exists.** `the_mires_preregistered_readout` carries `threads-required = "num-cpus"` and sits mid-order, so nextest drains the whole runner — including the 891 s pole — before starting it, then restarts 71 tests cold. Measured barrier tax: 484 s and 485 s on two runs at different SHAs.

- [ ] **Step 1: Confirm the mechanism still holds on this tree**

The scratch probe that established it is reproduced here so the implementer sees the evidence rather than taking it on trust. Run it in a scratch directory outside the repo:

```bash
mkdir -p /tmp/gov-probe/src /tmp/gov-probe/.config
cat > /tmp/gov-probe/Cargo.toml <<'EOF'
[package]
name = "gov-probe"
version = "0.1.0"
edition = "2021"
EOF
cat > /tmp/gov-probe/src/lib.rs <<'EOF'
use std::{thread::sleep, time::Duration};
#[test] fn a_long_6s()      { sleep(Duration::from_secs(6)); }
#[test] fn b_med_2s()       { sleep(Duration::from_secs(2)); }
#[test] fn c_exclusive_2s() { sleep(Duration::from_secs(2)); }
#[test] fn d_tail1_2s()     { sleep(Duration::from_secs(2)); }
#[test] fn e_tail2_2s()     { sleep(Duration::from_secs(2)); }
#[test] fn f_tail3_2s()     { sleep(Duration::from_secs(2)); }
#[test] fn g_tail4_2s()     { sleep(Duration::from_secs(2)); }
EOF
cat > /tmp/gov-probe/.config/nextest.toml <<'EOF'
[[profile.default.overrides]]
filter = 'test(/c_exclusive_2s$/)'
threads-required = "num-cpus"
EOF
cd /tmp/gov-probe && cargo nextest run -j 4 2>&1 | tail -4
```

Expected: wall ~10 s, and `c_exclusive_2s` completing **third**, after `a_long_6s`.

Then add `priority = 100` to that override, add a second override giving `a_long_6s` `priority = 50`, and re-run.

Expected: wall ~8 s, `c_exclusive_2s` completing **first**.

**Decision table** — do not proceed on a null:

| observation | action |
|---|---|
| ~10 s → ~8 s, order flips | mechanism confirmed; continue |
| both arms identical | STOP. The pool may not be constrained (check `-j 4` took effect) or nextest's scheduler changed. Report before touching the repo. |
| config rejected | STOP and report the parse error verbatim |

- [ ] **Step 2: Write the failing guard**

The project's rule is that a rule duplicated on purpose needs a two-way agreement test — a one-sided check is an echo. `.config/nextest.toml` will now state a fact (which tests are front-loaded) that must stay equal to another fact (which tests reserve the whole runner).

Add to `cli/tests/suite/heavy_tier.rs`, following the shape of the two existing pin guards in that file (`the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps` and its wall-clock-budget mirror — read both first; they are the pattern to copy, including their anti-vacuity assertions):

```rust
/// Every test that reserves the whole runner must also be front-loaded, and
/// nothing else may be.
///
/// DIRECTION, STATED SO IT CANNOT BE MISREAD AS TOTAL: this asserts set
/// EQUALITY between the `threads-required = "num-cpus"` roster and the
/// `priority`-carrying roster. It does not check that the priority VALUE is
/// sensible, only that the two rosters name the same tests.
///
/// WHY: a `threads-required = "num-cpus"` test that is not front-loaded makes
/// nextest drain the entire runner mid-run and restart the remainder cold.
/// Measured on the canonical box, twice, at different SHAs: a 484-485 s
/// barrier tax on a 1551 s tier.
#[test]
fn every_whole_runner_test_is_front_loaded() {
    let reserving = whole_runner_filter_names();
    let front_loaded = front_loaded_filter_names();

    assert!(
        !reserving.is_empty(),
        "found no test reserving the whole runner in {NEXTEST_CONFIG}. Either \
         the roster emptied (then this guard asserts nothing) or the key was \
         renamed — the one outcome it must never quietly reach."
    );
    assert_eq!(
        reserving, front_loaded,
        "\n{NEXTEST_CONFIG}: the whole-runner roster and the front-loaded \
         roster have diverged.\n  reserves the runner: {reserving:?}\n  \
         front-loaded:        {front_loaded:?}\nA reserving test that is not \
         front-loaded costs a full drain plus a cold restart of everything \
         scheduled after it."
    );
}
```

`whole_runner_filter_names()` and `front_loaded_filter_names()` are new helpers. `pinned_filter_names_for_class(marker)` already exists in this file `[VERIFY: git grep -n 'fn pinned_filter_names_for_class' cli/tests/suite/heavy_tier.rs]` — reuse it rather than writing a third parser, and read its contract before assuming what it returns.

- [ ] **Step 3: Run the guard and verify it fails for the right reason**

```bash
cargo nextest run -p hornvale --test suite -- every_whole_runner_test_is_front_loaded
```

Expected: FAIL on the `assert_eq!`, reporting a non-empty `reserves the runner` set and an empty `front-loaded` set.

**A failure from a compile error proves nothing about the assertion.** If it does not compile, fix the compile error and re-run until the failure is behavioural, before proceeding.

- [ ] **Step 4: Add the priority overrides**

In `.config/nextest.toml`, add `priority` to each existing `threads-required = "num-cpus"` override block, and a `# class: front-loaded` marker comment matching the file's existing `# class:` convention. Write a comment block in the file's established voice recording the measured 484/485 s tax and the scratch-probe arms from Step 1 — that file documents *why* every setting exists, and a bare setting would be out of keeping with it.

Priority is `i8`, range `-128..=127`, higher runs earlier. Use `100` for the whole-runner tests. Do not give priorities to anything else in this task.

- [ ] **Step 5: Verify the guard passes and nothing else broke**

```bash
cargo nextest run -p hornvale --test suite -- heavy_tier 2>&1 | tee /tmp/hv-t1.txt
```

Expected: all `heavy_tier` tests PASS, including the two pre-existing pin guards.

- [ ] **Step 6: fmt, gate, commit**

```bash
cargo fmt
make gate-commit
git add .config/nextest.toml cli/tests/suite/heavy_tier.rs
git commit   # message: perf(heavy): front-load the whole-runner tests (The Governor)
git push origin campaign/the-governor
```

- [ ] **Step 7: Measure on the canonical box**

```bash
make heavy-remote REF=$(git rev-parse HEAD)
```

Read the result with `make heavy-log`. Record the run in `docs/timings.md` if `timed.sh` did not (see Task 7, which fixes the recording gap; until then, note the number in the task report).

**Decision table:**

| observation | reading | action |
|---|---|---|
| wall falls ~400-500 s; the whole-runner test completes in the first few indices | as predicted | continue |
| wall falls, but the reserving test still completes late | `priority` is not doing what Step 1 measured | STOP, report, do not tune |
| wall unchanged | the barrier was not the cost on this roster | STOP and report — this invalidates spec §1.2 and Task 8's premise |
| **any test changes PASS/FAIL** | scheduling changed a result | STOP. This is the serious one: a result that depends on scheduling is a latent bug the tier was hiding. Report it as a finding, do not proceed. |

---

## Task 2: Stop `spearman_defensibility_capacity` re-deriving terrain and climate

**Files:**
- Modify: `windows/worldgen/src/graph_derive.rs` `[VERIFY: git grep -n 'pub fn connection_graph_of' windows/worldgen/src/graph_derive.rs]`
- Modify: `windows/lab/src/metrics.rs` `[VERIFY: git grep -n 'fn spearman_defensibility_capacity' windows/lab/src/metrics.rs]`

**Interfaces:**
- Consumes: nothing.
- Produces: `pub fn connection_graph_from(world: &World, terrain: &GeneratedTerrain, climate: &GeneratedClimate, cfg: &GraphConfig) -> ConnectionGraph`. Task 3 does not use it. **Confirm the exact terrain/climate types by reading `connection_graph_of`'s body before writing the signature** — the plan author must not invent a signature from outside the code, and a signature with no possible caller is a real failure mode in this repo.

**Why this task exists.** `connection_graph_of(world, cfg)` calls `crate::terrain_of(world)` and `crate::climate_from(world, &terrain)` — a full re-derivation — while the metric's own `FullView` already holds both via `v.terrain()` / `v.climate()`. Profiled at 13.17% of census study cycles.

- [ ] **Step 1: Capture the before-value as a byte-identity baseline**

The correctness proof is that nothing moves. Establish what "nothing" means *before* the change, on a small, fast, reproducible slice — not the full census.

Read `windows/lab/`'s own test conventions and find the existing instrument that evaluates this metric against committed rows. **Do not use `make lab-diff STUDY=the-census` as the check**: it compares a committed file against its own working-tree copy, which nothing regenerates without a census run on the canonical box, so it reports "no metric moved" whatever happened. The Sources was given that check twice and it was vacuous both times.

Record the chosen instrument and its before-output in the task report.

- [ ] **Step 2: Add the adapter, with `connection_graph_of` delegating to it**

In `graph_derive.rs`, add `connection_graph_from` taking already-built terrain and climate, and rewrite `connection_graph_of` to derive them and then call it. The edge-assembly logic must not be duplicated — that file's existing three-function ladder (`connection_graph` / `connection_graph_at` / `connection_graph_of`) is explicit that the adapters never duplicate derivation, and the new one must hold to it.

The `#[allow(clippy::disallowed_methods)]` and the "Named construction site (decision 0092)" comment on `connection_graph_of` exist for its sculpt-once behaviour — work out which function that annotation belongs on after the split, and say why in the commit message.

- [ ] **Step 3: Point the metric at the adapter**

In `spearman_defensibility_capacity`, replace the `connection_graph_of(v.world(), …)` call with `connection_graph_from(v.world(), v.terrain(), v.climate(), …)`.

- [ ] **Step 4: Prove it is byte-identical**

Re-run the instrument from Step 1.

**Decision table:**

| observation | action |
|---|---|
| output byte-identical | correct; continue |
| any value moved | the change is wrong. Do not re-baseline. Revert and report which metric moved by how much. |
| instrument cannot run | STOP and report — do not substitute a weaker check |

- [ ] **Step 5: Demonstrate the redundancy is actually gone**

A green test proves the change was harmless, not that it did anything. Show the re-derivation no longer happens on this path. **Find your own evidence** — the property to demonstrate is "`terrain_of`/`climate_from` are not called during this metric's evaluation". Do not take a prescribed mutation from this plan; the implementer reading the code will find a better one than the plan author can specify from outside it.

- [ ] **Step 6: fmt, gate, commit, push**

```bash
cargo fmt && make gate-commit
git add windows/worldgen/src/graph_derive.rs windows/lab/src/metrics.rs
git commit   # perf(lab): stop re-deriving terrain and climate the FullView already holds
git push origin campaign/the-governor
```

---

## Task 3: Compute the demography report once, not twice

**Files:**
- Modify: `windows/lab/src/metrics.rs` `[VERIFY: git grep -n 'demography_report_from' windows/lab/src/metrics.rs — the plan saw two production call sites at :2431 and :2476, plus one at :12865 that must be checked separately before being assumed part of the pair]`

**Interfaces:**
- Consumes: nothing from Task 2.
- Produces: nothing later tasks depend on.

**Why this task exists.** Two metric closures each call `hornvale_worldgen::demography_report_from` and rebuild the identical report. Profiled at 7.58% + 7.53% = 15.13% of census study cycles.

- [ ] **Step 1: Establish the call sites are genuinely identical**

Read all three call sites. The two flagged ones must be shown to be called with the same arguments and to produce the same report before any sharing is correct. **If they differ in any argument, STOP** — the redundancy claim is then wrong and the profile attributed cost to the wrong shape.

Record in the task report: the three sites, their arguments, and which are identical.

- [ ] **Step 2: Capture the byte-identity baseline**

Same instrument as Task 2 Step 1, same prohibition on `make lab-diff` as the check.

- [ ] **Step 3: Share the single computation**

Read how `windows/lab`'s metric registry evaluates closures before choosing the mechanism. **Two shapes are available and the right one depends on facts the plan author cannot see from outside:** if both closures run within one `FullView` evaluation, the report can be computed once and threaded; if they are independent registry entries evaluated separately, a memo keyed on the view is needed instead. Determine which, implement that one, and state in the task report why the other was wrong.

Whatever the shape: no `HashMap`/`HashSet` (workspace lint), and no interior mutability that would make evaluation order observable — a metric's value must not depend on which metric ran first.

- [ ] **Step 4: Prove byte-identity, same decision table as Task 2 Step 4**

- [ ] **Step 5: Demonstrate the second computation is gone**

Same standard as Task 2 Step 5: find your own evidence that the report is built once per world rather than twice.

- [ ] **Step 6: fmt, gate, commit, push**

---

## Task 4a: Adjudicate the `hearsay` heavy tests (23 tests, 4,504 s)

**Files:**
- Create: `docs/audits/heavy-tier-adjudication.md`
- Modify: `docs/generated-paths.txt`

**Interfaces:**
- Produces: the adjudication table's format and its `hearsay` rows. Tasks 4b and 4c append rows in the same format. Task 5 reads the `DEMOTE` rows.

**Why this task exists and why it is review work, not code.** The demotion rule cannot be mechanised — the `claim:` vocabulary is The Assay's *seed-loop quantifier* lint, not an assert-vs-report classifier, and the five largest failures are all tagged `structural(`, which only means "fixed panel, not a search". Read spec §3 in full before starting.

- [ ] **Step 1: Create the artifact and declare it**

Create `docs/audits/heavy-tier-adjudication.md` with a header stating the rule from spec §3.1 verbatim, the date, and the SHA the roster was read at. One table with columns:

`test | binary | wall_s | last result | verdict (KEEP/DEMOTE) | reason`

Add **both** `docs/audits/heavy-tier-adjudication.md` (by name) to `docs/generated-paths.txt`. The directory is already declared, but a new file inside an already-declared directory inherits nothing: the directory's other tracked files keep the tracked-ness check green while `git diff` cannot see the new file at all. Declaring it by name makes the tracked-ness check refuse until it is `git add`-ed.

- [ ] **Step 2: Enumerate the hearsay heavy tests from source**

```bash
grep -rEA1 '#\[ignore = "heavy:' --include='*.rs' windows/hearsay \
  | grep -oE 'fn [a-z0-9_]+' | sed 's/^fn //' | sort -u
```

Cross-check the count against the plan's figure of 23. **If it differs, the tree has moved — use the tree's number and say so in the report.**

- [ ] **Step 3: Adjudicate each one**

For each test, read it and answer spec §3.1's question: *if this went red tomorrow, what would we do?* Burden is on KEEP.

Apply the pre-filters from spec §3.2 — but note the correction recorded there: the zero-assertion filter is **file-wide and helper-aware**, not a body-scoped `assert!` count. A body-scoped count gave a wrong number on the first pass of the spec itself.

A DEMOTE reason must name the campaign whose question the test answered. A KEEP reason must say what regression the test would catch. **"It asserts things" is not a KEEP reason** — the question is what those assertions are *for*.

- [ ] **Step 4: Record what the adjudication could NOT settle**

Any test you cannot confidently classify gets verdict `UNDECIDED` with the specific question that would settle it. An UNDECIDED row is a finding, not a failure — and it is strictly better than a confident wrong verdict, since Task 5 only acts on `DEMOTE`.

- [ ] **Step 5: Commit and push**

```bash
git add docs/audits/heavy-tier-adjudication.md docs/generated-paths.txt
git commit   # docs(the-governor): adjudicate the hearsay heavy tests
git push origin campaign/the-governor
```

No `gate-commit` needed for a docs-only commit, but run it if `docs/generated-paths.txt` triggers the hook.

---

## Task 4b: Adjudicate the `worldgen` heavy tests (65 tests, 4,267 s)

**Files:** Modify `docs/audits/heavy-tier-adjudication.md`

**Interfaces:** Consumes 4a's table format. Produces the `worldgen` rows.

Identical process to Task 4a, Steps 2–5, with `windows/worldgen` substituted in the Step 2 grep and 65 as the expected count. Do not re-create the artifact or re-edit `docs/generated-paths.txt`.

**One population-specific note:** this binary holds many `report_*`-named tests from recent underworld campaigns. Recency is not a KEEP reason and neither is age a DEMOTE reason — a report written last week whose question closed last week is exactly as demotable as one from March. Apply §3.1 and nothing else.

---

## Task 4c: Adjudicate the remaining heavy tests (30 tests, 3,509 s)

**Files:** Modify `docs/audits/heavy-tier-adjudication.md`

**Interfaces:** Consumes 4a's table format. Produces the remaining rows; after this task every one of the 118 has a verdict.

Identical process. Covers `hornvale-lab` (15 tests, 2,039 s), `hornvale` (7), `hornvale-worldgen` standalone binaries (3), and one each in `hornvale-locale`, `hornvale-scene`, `hornvale-terrain` (+3 in its suite), `hornvale-chronicle`, `hornvale-kernel`.

**Two tests in this population are pinned in `.config/nextest.toml`'s wall-clock-budget class** (`a_possessed_turn_stays_within_its_ceilings`, `scene_api_cost_is_bounded_on_seed_42`). Demoting either would leave a roster naming a test the tier no longer runs — check `heavy_tier.rs`'s budget-class guard before assigning them a verdict, and record the interaction whichever way you decide.

- [ ] **Final step: assert completeness**

```bash
grep -c '^|' docs/audits/heavy-tier-adjudication.md
```

Every one of the 118 must have a row. A missing row is indistinguishable from a KEEP once this campaign closes.

---

## Task 5: Apply the demotions

**Files:** Modify the `#[ignore]` attribute of each `DEMOTE` row's test. **Nothing else.**

**Interfaces:** Consumes the `DEMOTE` rows from Tasks 4a–4c.

**The hard constraint, from spec §3.3.** This task changes reason strings. It does **not** change a test body, regenerate a fixture, or make a demoted test green. Demotion and repair are different acts, and mixing them lets a real regression exit the tier under cover of a cost campaign.

- [ ] **Step 1: Rewrite each DEMOTE tag**

Template, from the decision-0148 fare batteries already in the tree:

```rust
#[ignore = "probe: <what it measures>; run by hand (<campaign> answered its question; \
            demoted by The Governor 2026-08-28)"]
```

For a test that is **currently red**, the tag must say so and why that is acceptable:

```rust
#[ignore = "probe: <what it measures>; RED as of 2026-08-28 — its pin records \
            <campaign>'s answer and the world has since moved; the question is \
            closed. Run by hand; demoted by The Governor."]
```

- [ ] **Step 2: Verify the token guard still passes**

```bash
cargo nextest run -p hornvale --test suite -- heavy_tier
```

`heavy_tier_reason_strings_are_canonical` holds tokens to their format verbatim. Expected: PASS. If it fails, the tag format is wrong — fix the tag, never the guard.

- [ ] **Step 3: Confirm the roster shrank by exactly the DEMOTE count**

```bash
grep -rc '#\[ignore = "heavy:' --include='*.rs' . | awk -F: '{s+=$2} END{print s+0}'
```

Expected: `118 - <DEMOTE count>`. Any other number means a tag was edited that the adjudication did not authorise.

- [ ] **Step 4: fmt, gate, commit, push, then measure**

```bash
cargo fmt && make gate-commit
git add -u && git commit   # chore(heavy): demote the campaign instruments whose questions closed
git push origin campaign/the-governor
make heavy-remote REF=$(git rev-parse HEAD)
```

**Decision table:**

| observation | action |
|---|---|
| wall falls; remaining reds are exactly the adjudicated-KEEP failures | as designed; continue |
| a test that was KEEP is now failing and was not failing before | a demotion changed a kept test's result — investigate before proceeding |
| the tier is green | note it; Task 10 then has nothing to do, which is a result, not a shortcut |

---

## Task 6: Absorb main and take a stage gate

- [ ] **Step 1**

```bash
git fetch origin && git merge origin/main
```

**Build and test after the absorb — git's silence is not agreement.** Three times in one campaign The Sources saw a file merge with no conflict marker and be wrong. If `docs/audits/type-audit-report.md`, `docs/digest/decisions-in-force.md`, or the decision index conflict, **regenerate them, never text-merge** — and remember the `>` redirect is what writes the file; running the render command bare regenerates nothing and the drift check then reads clean.

- [ ] **Step 2**

```bash
make gate-commit
git push origin campaign/the-governor
make sluice-stage BRANCH=campaign/the-governor REF=$(git rev-parse HEAD)
```

---

## Task 7: Ratchet the roster, and fix the timings recording gap

**Files:**
- Create: `cli/tests/fixtures/heavy-roster.txt`
- Modify: `cli/tests/suite/heavy_tier.rs`
- Modify: `scripts/heavy-run.sh`

**Interfaces:** Consumes the post-Task-5 roster. Produces the frozen fixture that Task 9 must update when it changes nothing — it does not change roster membership, so this is a check that Task 9 leaves it alone.

**Why this task exists.** Nothing prices a `heavy:` tag. That is the upstream cause of every number in this campaign, and without a ratchet the campaign is re-run in three months.

- [ ] **Step 1: Write the failing guard first**

Model it on `the_untokenised_ignore_reasons_are_exactly_this_roster` in the same file `[VERIFY: git grep -n 'fn the_untokenised_ignore_reasons_are_exactly_this_roster' cli/tests/suite/heavy_tier.rs]`, and on `cli/tests/suite/test_binary_ratchet.rs`, which froze a roster that had already crept back to 13 entries.

The guard asserts set equality between the fixture and the tags found in source, **in both directions**, with an anti-vacuity assertion. State the direction in the doc comment — a guard asserting only `declared ⊆ found` is structurally blind to a new tag and still reads as total.

- [ ] **Step 2: Run it and confirm it fails behaviourally, not by compile error**

- [ ] **Step 3: Write the fixture from source, then make the guard pass**

- [ ] **Step 4: Prove the ratchet actually refuses**

Add a `heavy:` tag to any test **without** updating the fixture, run the guard, and observe the refusal. Then revert the tag.

This is a real, observed refusal rather than a hoped-for one. An allow-list gate that cannot see its own list go short is a known failure mode here; so is a guard a comment can satisfy. Record the refusal's output in the task report.

- [ ] **Step 5: Fix the `docs/timings.md` recording gap**

No `heavy` row has been written since 2026-08-05 while five runs happened. Determine why by reading `scripts/heavy-run.sh` and `scripts/timed.sh` — the run happens in a dedicated worktree under `HV_HEAVY_REF`, and the row lands wherever `timed.sh` writes it.

**Do not guess the cause and do not write a fix for a cause you have not demonstrated.** State the mechanism, show the evidence, then fix it. If the fix is out of proportion to this campaign, record the mechanism in the task report and file it rather than building it — a named cause is the deliverable here.

- [ ] **Step 6: fmt, gate, commit, push**

---

## Task 8: Size the `threads-required` classes, and amend the guard that blocks Task 9

**Files:**
- Modify: `.config/nextest.toml`
- Modify: `cli/tests/suite/heavy_tier.rs`

**Interfaces:** Produces the `# class: sized-sweep` marker that Task 9's conversions attach to.

**Why this task exists — read this before touching anything.** `the_serialization_pin_names_exactly_the_batteries_that_scatter_their_sweeps` is a strict two-way `assert_eq!` between `.config/nextest.toml`'s scatter-sweep filter and **every** test calling `seed_sweep::map_seeds(`. So Task 9 will fail that guard the moment it converts a test.

**The naive fix is a trap.** Adding each newly-parallel test to the `"num-cpus"` roster turns the guard green and creates one full-drain barrier per converted test — making the tier far worse than it is now while every check passes. Task 1 measured what a *single* such barrier costs.

- [ ] **Step 1: Add a second, sized class**

Keep `# class: scatter-sweep` (`threads-required = "num-cpus"`) for batteries that genuinely saturate the box. Add `# class: sized-sweep` (`threads-required = <int>`) for panel tests whose sweep is bounded by a small panel. Both keys are supported by nextest 0.9.140 — verified alongside `priority`.

- [ ] **Step 2: Split the guard in two, both directions each**

One guard per class, each with its own anti-vacuity assertion, each checked in both directions against its own class marker — the shape the file's two existing guards already use, and for the reason its comments already give: a renamed test drops out of a filter silently, and a newly-marked one never enters it.

- [ ] **Step 3: Record the detector's fragility at the guard**

The detector recognises parallelism by the literal text `seed_sweep::map_seeds(`, and the file's own module doc already admits a hand-rolled `std::thread::scope` is invisible to it. Task 9 may change where the helper lives and therefore how the call is spelled. **Write that dependency into the guard's doc comment**, so the next person to move the helper learns that moving it silently disarms the guard.

- [ ] **Step 4: Verify both guards pass on the unconverted tree, fmt, gate, commit, push**

- [ ] **Step 5: Answer spec §8.3 while the machinery is fresh**

Does `the_mires_preregistered_readout` still need the whole box? It is 148.6 s
now, against the ~2427 s that motivated `threads-required = "num-cpus"` in the
first place. You have just built the sized class, so you are the cheapest
person in the campaign to answer this.

Move it to `sized-sweep` at a bounded width in a **scratch commit**, run
`make heavy-remote`, and compare against Task 1's number.

**Decision table:**

| observation | action |
|---|---|
| wall falls | keep the change; it removes the last full-drain barrier |
| wall rises | revert; the battery still wants the box, and that is now measured rather than inherited |
| the battery's result changes | revert immediately and report — a result that moves with thread count is a finding about the battery, not about scheduling |

Report the number either way. **A null is a result here** — "still needs the
whole box, measured 2026-08-28" retires an eleven-week-old assumption just as
usefully as a win would.

---

## Task 9: Parallelise the surviving seed-panel witnesses

**Files:**
- Modify: whichever KEEP tests from Tasks 4a–4c contain a serial seed-panel loop
- Modify: `.config/nextest.toml` (add each converted test to the `sized-sweep` class)
- Possibly create: a shared home for `map_seeds`

**Interfaces:** Consumes Task 8's `sized-sweep` class and Task 4's KEEP set.

**Do not convert a DEMOTED test.** They no longer run in the tier; converting them spends effort on wall time nothing pays for, and enlarges this task's blast radius for no gain.

- [ ] **Step 1: Decide where `map_seeds` lives — this is spec §8.1 and it is genuinely open**

It is currently a test-only module in `windows/lab/tests/seed_sweep/mod.rs`, unreachable from `windows/hearsay` and `windows/worldgen` tests. The workspace is `members = ["kernel", "domains/*", "windows/*", "cli"]`, so a test-support crate is neither a domain nor a window and would need an `architecture.rs` amendment.

Candidates and their costs — evaluate against the tree, do not take this plan's ordering as a recommendation:

| option | cost |
|---|---|
| `pub mod seed_sweep` in `hornvale-worldgen` | test scaffolding in a shipped library; every panel test already dev-depends on worldgen |
| same in `hornvale-kernel` | kernel is constitutional; it already owns `Seed`/`Stream` |
| a new workspace member | needs an `architecture.rs` and layering amendment, probably a decision record |
| duplicate per crate | **rejected** — this tree fails builds on second copies of a single source of truth |

Whatever is chosen interacts with Task 8 Step 3. Record the decision and its reason in the task report; if it needs a decision record, say so rather than deciding it silently.

- [ ] **Step 2: Convert one test first, and prove byte-identity before converting any others**

`map_seeds`' contract is that output order is seed order, never completion order, and `HV_SEED_SWEEP_THREADS=1` reproduces the serial loop exactly — that override exists precisely to drive this proof, and it fails fast on a malformed value rather than silently restoring parallelism.

Run the converted test both ways and compare output.

**Decision table:**

| observation | action |
|---|---|
| identical under `=1` and unset | conversion is sound; continue to the rest |
| differs | the loop body was not pure — STOP and report what shared state it touched |
| test now fails | the conversion changed a result; revert it and report |

- [ ] **Step 3: Convert the remainder, one commit per test, re-running the proof each time**

- [ ] **Step 4: Add each converted test to the `sized-sweep` class; both Task 8 guards must pass**

- [ ] **Step 5: fmt, gate, commit, push, measure**

```bash
make heavy-remote REF=$(git rev-parse HEAD)
```

This is the number Task 11 decides against.

---

## Task 10: Diagnose the reds that remain in `heavy:`

**Files:** whatever the diagnosis reaches.

**The rule, from The Sources' retrospective:** each red needs a **named cause** before anything is regenerated. A regeneration with no named cause is a silencing, and following an inherited attribution nearly wrote one campaign's name onto another's redness.

- [ ] **Step 1: Ask for a classification, not a confirmation**

For each remaining red, establish what kind of thing it is before deciding what to do: subterranean / marine / surface, or whatever axis discriminates for that test. That framing is what made "surface kinds are failing" legible as evidence *against* an attribution in The Sources, rather than as more work to do.

- [ ] **Step 2: Bisect before repairing**

Run each failure standalone at a SHA predating the suspected cause. `git worktree add` a scratch tree at that SHA; do not reason from the diff.

- [ ] **Step 3: Repair, or re-pin with the cause named in the commit message**

- [ ] **Step 4: STOP rule** — if any red cannot be attributed to a named cause, stop and report it rather than regenerating it.

---

## Task 11: The gating decision, and its record

**Files:**
- Create: `docs/decisions/<NNNN>-*.md`

- [ ] **Step 1: Reserve the number properly**

```bash
make decision-block
```

Decision numbers are reserved in blocks. `max+1` is wrong and collides silently — two campaigns have both minted the same number this way.

- [ ] **Step 2: Write the record against the measured cost**

It supersedes or amends 0148. It must state the post-Task-9 measured number, not a projection, and weigh the four forms in spec §7 — noting that the conditional form's real argument is **symmetry** (today a campaign can move shipped world values, pass every gate it is asked to pass, and leave the tier red for the next campaign to inherit and mis-attribute), not cost.

- [ ] **Step 3: Implement whichever form the record ratifies**

- [ ] **Step 4: Regenerate the digest — the redirect is what writes the file**

```bash
cargo run --manifest-path tools/digest/Cargo.toml -- render decisions > docs/digest/decisions-in-force.md
cargo run --manifest-path tools/digest/Cargo.toml -- render delta      > docs/digest/intent-vs-reality.md
```

---

## Task 12: Definition of Done

- [ ] `book/src/chronicle/the-governor.md`, plus a freshness sweep of chapters this touches; re-score `book/src/open-questions.md` if a bet moved
- [ ] `docs/retrospectives/the-governor.md` — process lessons, not product
- [ ] Idea-registry rows for spec §8.1, §8.2 (the off-queue probe) and §8.3, whatever their outcome. New registry ids must be **named, not numbered**; Idea cells cap at 600 characters
- [ ] `make rebaseline` and the drift check:
      `git diff --exit-code -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$')`
- [ ] Chronicle and retrospective land **before** the merge submission, not after
- [ ] Merge via `make sluice BRANCH=campaign/the-governor REF=<full-sha>` — the submission needs a `Sluice-Headline:` trailer in the final trailer block, same block as `Claude-Session`, no blank line between them

---

## Self-review

**Spec coverage.** §1 → Tasks 1, 7 (recording gap). §2.1 rejected lever → no task, correctly. §2.2 → Tasks 4a–4c, 5. §2.3 → Task 1. §2.4 → Task 9. §3 → Tasks 4a–4c. §3.3 → Task 5's hard constraint. §4 → Task 7. §5 → Task 8. §6 Stage 0 → Tasks 2, 3. §7 → Task 11. §8.1 → Task 9 Step 1. §8.2 → Task 12 (registry row only — out of scope by design). §8.3 → **not covered by any task**; it is a cheap check once Task 8's machinery exists, so it is folded into Task 8 as an optional observation rather than left silently unimplemented — an implementer who has just built the sized class is the cheapest person to answer it. §9 → the global no-golden-moves constraint and each task's decision table. §10 → Task 12.

**Placeholder scan.** No TBD/TODO. Three tasks deliberately do not prescribe an implementation (2 Step 5, 3 Step 3, 9 Step 1) — each names the *property* to demonstrate or the *decision* to make with its options, because a plan author cannot see from outside the code which mutation discriminates or which sharing shape is correct. Both prescribed mutations in The Quire were nulls and the implementers found better ones by hunting.

**Type consistency.** `connection_graph_from` appears only in Task 2, which requires reading the real terrain/climate types before writing the signature rather than taking one from this plan. `whole_runner_filter_names` / `front_loaded_filter_names` (Task 1) and the `# class: sized-sweep` marker (Tasks 8, 9) are used consistently. `map_seeds` and `HV_SEED_SWEEP_THREADS` match the existing helper.
