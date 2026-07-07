# Campaign 0: Firm Ground — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the coastal-flagship placement degeneracy, revive the dead subsistence modes, diagnose (without silently retuning) the frozen-biome skew, and re-baseline every census and calibration exactly once.

**Architecture:** One derived-formula fix at the composition root (`windows/worldgen`) — coastal cells stop being treated as having perfect freshwater; sea access stays priced by the suitability formula's coast term. Everything else is instrumentation (one new Lab metric), re-baselining (drift study, 10k censuses, gallery artifacts, calibration pins), and a preregistered written diagnosis (Study 005). No new domains, no new predicates, no new draws — **no stream label or consumption-order changes anywhere**, so no epoch suffixes are needed.

**Tech Stack:** Rust edition 2024, workspace deps `serde`/`serde_json` only. The Lab (`windows/lab`), the CLI (`cargo run -p hornvale`), mdbook.

**Spec:** `docs/superpowers/specs/2026-07-07-year-2-metaplan-design.md` §4 (Campaign 0 is fully specified there; this plan implements it).

## Global Constraints

- Every commit passes the full gate: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` as the final step before every commit.
- Determinism is constitutional: no wall-clock time, no `HashMap`/`HashSet`, float sorts via `total_cmp`. Same seed → byte-identical worlds and artifacts.
- No new dependencies (workspace allowlist: `serde`, `serde_json`; enforced by `cli/tests/architecture.rs`).
- Domains depend only on `hornvale-kernel`; composition happens only in `windows/worldgen`.
- `#![warn(missing_docs)]` everywhere: every public item gets a one-line doc comment.
- Distribution changes here are deliberate re-baselines: **verify each changed number is physically plausible before pinning it** — never re-pin blindly.
- The calibration tests in `windows/lab/tests/calibration.rs` each rebuild 500 worlds; they take minutes. That is the established pattern — do not "optimize" it in this campaign.
- Work on branch `campaign-y2-0-firm-ground` (worktree per `superpowers:using-git-worktrees`).

---

### Task 1: Fix the coastal-freshwater conflation

The degeneracy: `windows/worldgen/src/lib.rs` gives every coastal cell `freshwater = 1.0`, which combined with the suitability formula (`0.45·freshwater + 0.20·coast + 0.35·temperance − 0.5·hostility`, `domains/settlement/src/placement.rs:51-52`) makes the global argmax coastal in 100% of censused worlds. Seawater is not freshwater; coastal access is already priced by the `0.20 · coast` term. The fix removes the conflation — freshwater becomes `max(drainage, moisture)` — so inland river cells can win the argmax.

**Files:**
- Modify: `windows/worldgen/src/lib.rs:424-428` (the `freshwater` binding inside the `sites` map)
- Modify: `domains/settlement/src/placement.rs:20-21` (the `SiteInput::freshwater` doc comment)
- Test: `windows/lab/tests/calibration.rs` (new test at end of file)

**Interfaces:**
- Consumes: `flagship-coastal` metric (`MetricValue::Flag`) from the existing Lab registry; `load_study`/`run` from `hornvale_lab` (see the five existing tests in the same file for the exact pattern).
- Produces: the fixed `freshwater` input. `SiteInput`'s shape and `suitability()`'s formula are **unchanged** — only the value fed in at the composition root changes.

- [ ] **Step 1: Write the failing calibration test**

Append to `windows/lab/tests/calibration.rs`:

```rust
#[test]
fn flagships_are_sometimes_inland_and_sometimes_coastal() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let coastal_i = idx("flagship-coastal");
    let (mut coastal, mut inland) = (0u32, 0u32);
    for row in &result.rows {
        match row.values[coastal_i] {
            MetricValue::Flag(true) => coastal += 1,
            MetricValue::Flag(false) => inland += 1,
            _ => {}
        }
    }
    // Campaign Y2-0: seawater is not freshwater. Before the fix every
    // flagship was coastal (2,000/2,000 censused worlds); after it, both
    // kinds must occur. Task 2 pins the exact counts.
    assert!(
        inland > 0,
        "placement degeneracy: all {coastal} flagships coastal"
    );
    assert!(coastal > 0, "overcorrection: all {inland} flagships inland");
}
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cargo test -p hornvale-lab --test calibration flagships_are_sometimes_inland -- --nocapture`
Expected: FAIL with `placement degeneracy: all N flagships coastal` (N ≈ 490s; some worlds refuse or have no flagship). Takes minutes — it builds 500 worlds.

- [ ] **Step 3: Apply the fix**

In `windows/worldgen/src/lib.rs`, replace:

```rust
            let freshwater = drainage_norm
                .max(if coastal { 1.0 } else { 0.0 })
                .max(moisture)
                .clamp(0.0, 1.0);
```

with:

```rust
            // Seawater is not freshwater: coastal access is priced by the
            // coast term in settlement's suitability, not smuggled in here.
            let freshwater = drainage_norm.max(moisture).clamp(0.0, 1.0);
```

In `domains/settlement/src/placement.rs`, update the `SiteInput::freshwater` doc comment from:

```rust
    /// Freshwater availability in `[0, 1]` (drainage/coast/moisture, at root).
```

to:

```rust
    /// Freshwater availability in `[0, 1]` (drainage/moisture, at root).
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-lab --test calibration flagships_are_sometimes_inland -- --nocapture`
Expected: PASS. If it still fails with all-coastal, stop and investigate rather than tuning weights — weight changes are outside this campaign's scope (spec §4).

- [ ] **Step 5: Survey the workspace fallout (do not fix yet)**

Run: `cargo test --workspace 2>&1 | tail -40`
Expected: the placement-dependent tests may fail (candidates: `cli/tests/exit_criterion.rs` if it pins seed-42 settlement names/beliefs; the other `calibration.rs` tests should still pass because they assert *relationships*, not distributions). Record the exact failing test list in the task notes — Task 2 re-pins them. Do NOT commit yet if the workspace is red; Task 2 completes the commit… **unless** the only failures are the known re-pin candidates, in which case proceed to Task 2 immediately and commit there (every commit must compile and pass tests, so Tasks 1–2 land as one commit if needed).

### Task 2: Re-pin fallout and pin the new placement baseline exactly

**Files:**
- Modify: `windows/lab/tests/calibration.rs` (harden the Task 1 test to exact counts)
- Modify: whatever tests Task 1's survey found (expected: `cli/tests/exit_criterion.rs`; possibly others that pin seed-42 placement facts)

**Interfaces:**
- Consumes: Task 1's fix and its failing-test survey.
- Produces: a green workspace and the exact-count calibration row the spec requires ("the exact expected rate is set by the fixed model, then pinned").

- [ ] **Step 1: Re-pin each fallout test, verifying semantics first**

For each failing test from Task 1's survey: read what it asserts; regenerate the value it expects (e.g. `cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json && cargo run -p hornvale -- almanac --world /tmp/hv.json` and read off the new flagship name/biome/subsistence); confirm the new value is physically sensible (a settlement on a habitable cell, a subsistence mode consistent with its biome+coast per `domains/culture/src/subsistence.rs::subsistence`); then update the pinned literal. Sanity, not blind acceptance: if any new value looks wrong (flagship on ice, farming in desert), stop — that is a bug in the fix, not a re-pin.

- [ ] **Step 2: Measure and pin the exact coastal/inland counts**

Run: `cargo test -p hornvale-lab --test calibration flagships_are_sometimes_inland -- --nocapture`
Then harden the test: replace the two `assert!` lines with exact pins using the measured values (shown here as `«M»`/`«N»` — substitute the real numbers):

```rust
    // Exact-count pin over the 500-seed drift study (deterministic): the
    // fixed model's realized split, measured 2026-07 at re-baseline.
    assert_eq!(coastal, «M», "coastal flagship count drifted");
    assert_eq!(inland, «N», "inland flagship count drifted");
```

To read the counts, temporarily add `panic!("coastal={coastal} inland={inland}")` before the asserts, run, copy the numbers, remove the panic. Keep the test name and doc comment.

- [ ] **Step 3: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all green. (Note: CI's artifact drift check will be red until Task 4 regenerates artifacts — that's the local `git diff` step, not the test suite; it is handled in Task 4.)

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "fix(worldgen): seawater is not freshwater — inland flagships exist

Coastal cells no longer get freshwater = 1.0 at the composition root;
freshwater is max(drainage, moisture) and sea access is priced only by
the suitability coast term. Kills the 100%-coastal-flagship degeneracy
(spec: Year-2 metaplan §4, fix 1). Exact coastal/inland split over the
500-seed drift study pinned as a calibration row."
```

### Task 3: Add the `mean-land-temperature-c` metric

The Study 005 diagnosis (Task 6) needs a direct temperature column to test whether frozen-dominance follows from the insolation/obliquity envelope; no such metric exists. New measurement = Rust extractor (ADR 0011).

**Files:**
- Modify: `windows/lab/src/metrics.rs` (one new `Metric` in `registry()`, placed after the `"dominant-land-biome"` entry to keep land metrics adjacent; one line in the kinds test near line 729)

**Interfaces:**
- Consumes: `WorldView { terrain, climate, .. }` (`windows/lab/src/metrics.rs:13-28`); `v.terrain.geosphere()`, `v.terrain.is_ocean(CellId)`, `v.climate.mean_temperature_at(CellId)` — all already used by neighboring metrics.
- Produces: metric `"mean-land-temperature-c"` (`MetricValue::Number`, °C averaged over land cells; `Absent` on an all-ocean world). Task 6's study page cites this column by name.

- [ ] **Step 1: Write the failing kinds assertion**

In the existing metric-kinds test in `windows/lab/src/metrics.rs` (the block asserting with the `m(...)` helper around line 729), add:

```rust
        assert!(matches!(
            m("mean-land-temperature-c"),
            MetricValue::Number(_) | MetricValue::Absent
        ));
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-lab metrics`
Expected: FAIL — the `m()` helper panics/unwraps on the unknown metric name.

- [ ] **Step 3: Implement the metric**

Add to `registry()` in `windows/lab/src/metrics.rs`, directly after the `"dominant-land-biome"` entry:

```rust
        Metric {
            name: "mean-land-temperature-c",
            doc: "Annual-mean temperature averaged over land cells, °C; Absent \
                   if the world has no land",
            summary: SummaryKind::Numeric {
                bucket_edges: &[-30.0, -20.0, -10.0, 0.0, 10.0, 20.0, 30.0],
            },
            extract: |v| {
                let geo = v.terrain.geosphere();
                let (mut sum, mut count) = (0.0_f64, 0_u32);
                for cell in geo.cells() {
                    if !v.terrain.is_ocean(cell) {
                        sum += v.climate.mean_temperature_at(cell);
                        count += 1;
                    }
                }
                if count == 0 {
                    MetricValue::Absent
                } else {
                    MetricValue::Number(sum / f64::from(count))
                }
            },
        },
```

(If `geo.cells()` yields `&CellId` rather than `CellId` here, deref as the neighboring `"dominant-land-biome"` extractor does — mirror its exact iteration idiom.)

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-lab metrics`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): mean-land-temperature-c metric for the biome-skew diagnosis"
```

### Task 4: Regenerate every committed artifact — the re-baseline, part 1

Placement moved for every world, so every committed artifact downstream of settlement is stale; the new metric also adds a column/chart to the drift study. Regenerate the full CI list exactly once.

**Files:**
- Modify (generated): `book/src/gallery/*.md`, `book/src/gallery/*.ppm`, `book/src/reference/*-generated.md`, `book/src/laboratory/generated/census-lands-drift/*`

**Interfaces:**
- Consumes: Tasks 1–3 (all artifact-affecting code changes must land before this single regeneration).
- Produces: fresh committed artifacts; CI's "Artifacts are current" step goes green.

- [ ] **Step 1: Regenerate with the CI-exact command list**

Run (this is the authoritative list from `.github/workflows/ci.yml`, "Artifacts are current"):

```bash
cargo run -p hornvale-kernel --example first_light
cargo run -p hornvale -- new --seed 42 --sky constant --out /tmp/hv-ci-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-sky.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-sky.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- new --seed 42 --rotation locked --out /tmp/hv-ci-locked.json
cargo run -p hornvale -- almanac --world /tmp/hv-ci-locked.json > book/src/gallery/almanac-seed-42-locked.md
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
cargo run -p hornvale -- map --world /tmp/hv-ci-sky.json --out book/src/gallery/elevation-seed-42.ppm > book/src/gallery/elevation-seed-42.md
cargo run -p hornvale -- biome-map --world /tmp/hv-ci-sky.json --out book/src/gallery/biome-seed-42.ppm > book/src/gallery/biome-seed-42.md
cargo run -p hornvale -- biome-map --world /tmp/hv-ci-locked.json --out book/src/gallery/biome-seed-42-locked.ppm > book/src/gallery/biome-seed-42-locked.md
cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-sky.json --out book/src/gallery/settlement-seed-42.ppm > book/src/gallery/settlement-seed-42.md
cargo run -p hornvale -- settlement-map --world /tmp/hv-ci-locked.json --out book/src/gallery/settlement-seed-42-locked.ppm > book/src/gallery/settlement-seed-42-locked.md
cargo run -p hornvale -- lab run studies/census-lands-drift.study.json
```

- [ ] **Step 2: Inspect the diff for plausibility**

Run: `git diff --stat book/src/ && git diff book/src/gallery/almanac-seed-42-sky.md | head -80`
Expected: sky/astronomy sections byte-identical (placement cannot reach the sky — if any sky number changed, STOP: the fix leaked upstream, which is a determinism bug). Settlement sections changed: new flagship possible, settlement count shifted. Read the new seed-42 almanac end to end and confirm the flagship's biome/subsistence/coast are mutually consistent.

- [ ] **Step 3: Verify the CI drift check passes locally**

Run: `git status --short book/src/ && git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ ; echo "exit: $?"`
Expected after `git add`: exit 0 on a re-run of the regeneration commands (regenerate twice to prove byte-stability if in doubt).

- [ ] **Step 4: Commit**

```bash
git add book/src/gallery/ book/src/reference/ book/src/laboratory/generated/census-lands-drift/
git commit -m "chore(artifacts): re-baseline all committed artifacts after the placement fix

Single re-baseline for Campaign Y2-0 (spec §4, fix 4): seed-42 almanacs,
maps, registry/manifest dumps, and the 500-seed drift study, regenerated
once after the freshwater fix and the new land-temperature metric."
```

### Task 5: Re-run the 10k censuses, update the study pages, check the four modes

**Files:**
- Modify: `book/src/laboratory/study-002.md`, `study-003.md`, `study-004.md`, `book/src/laboratory/overview.md` (quoted 10k numbers)
- Modify: `.gitignore` (ignore the 10k censuses' published chart dirs)
- Not touched: `study-001.md` (Census of Skies — placement cannot reach the sky)

**Interfaces:**
- Consumes: `lab run` writes `lab-out/<study>/rows.csv` (columns: `seed,pin_set,<metric names>`) and publishes charts into `book/src/laboratory/generated/<study>/`.
- Produces: updated study-page prose; `lab-out/census-of-lands/rows.csv` and `lab-out/census-of-peoples/rows.csv` for Task 6; the four-modes verdict.

- [ ] **Step 1: Ignore the 10k chart output (policy: committed charts are drift-only)**

`book/src/laboratory/generated/census-of-lands/` is currently sitting untracked in the working tree (leftover from the retrospective's local 10k run). Per `study-002.md`'s own stated policy, committed charts come only from the CI-rerun 500-seed drift study; 10k charts are author-time ephemera. Add to `.gitignore`:

```gitignore
# 10k author-time census output; committed charts come from census-lands-drift only
book/src/laboratory/generated/census-of-lands/
book/src/laboratory/generated/census-of-peoples/
book/src/laboratory/generated/census-of-faiths/
book/src/laboratory/generated/census-of-skies/
lab-out/
```

(If `lab-out/` is already ignored, keep the existing line and skip the duplicate.)

- [ ] **Step 2: Run the three affected censuses at 10,000 seeds**

```bash
cargo run --release -p hornvale -- lab run studies/census-of-lands.study.json
cargo run --release -p hornvale -- lab run studies/census-of-peoples.study.json
cargo run --release -p hornvale -- lab run studies/census-of-faiths.study.json
```

Expected: long runs (10,000 worlds each — use `--release`; the outputs are prose sources, not drift-checked artifacts). Skies is skipped: its columns are provably upstream of placement.

- [ ] **Step 3: The four-modes check (spec §4 fix 2's success criterion)**

Run: `awk -F, 'NR==1{for(i=1;i<=NF;i++)if($i=="flagship-subsistence")c=i} NR>1{print $c}' lab-out/census-of-peoples/rows.csv | sort | uniq -c | sort -rn`
Expected: **all four modes present** (farming, fishing, herding, foraging), with farming/fishing still dominant and herding/foraging as minority modes on worlds whose best cell is inland arid/cold.

**Decision gate:** if herding or foraging is absent at 10,000 seeds, do NOT change the `subsistence()` rule table or the suitability weights — the table was audited during planning and is sound (`domains/culture/src/subsistence.rs:71-82`; the modes were dead because the flagship could never be inland). Absence now means the suitability landscape still never crowns an inland arid/cold argmax; record the measured counts and STOP for a checkpoint with Nathan — accepting a three-mode world vs. reworking placement weights is a taste call outside this campaign's spec.

- [ ] **Step 4: Update the study pages' quoted numbers**

In `study-002.md`, `study-003.md`, `study-004.md`, and `laboratory/overview.md`: every statistic quoted from the old 10k runs (flagship-coastal 100%, farming 68.8% / fishing 31.1%, settlement count mean 57.7, structure-size split 37.6%/62.4%, pantheon figures, etc.) gets the new measured value from the fresh `lab-out/*/rows.csv` / published summaries. Sky-only numbers (457 locked, moon-refusal 3.6%, obliquity envelope) are unchanged — verify a couple to confirm, since placement cannot reach them. Where a page *narrates* the old degeneracy (e.g. text noting every flagship is coastal), rewrite the sentence to describe the new distribution and note the Campaign Y2-0 fix — the book may never lag merged reality.

- [ ] **Step 5: Build the book and commit**

Run: `mdbook build book`
Expected: clean build.

```bash
git add .gitignore book/src/laboratory/
git commit -m "docs(lab): re-baseline the 10k census numbers after the placement fix

Census of Lands/Peoples/Faiths re-run at 10,000 seeds; study pages'
quoted statistics updated; all four subsistence modes now observed
(spec §4 fix 2). 10k chart output is author-time ephemera and now
gitignored — committed charts come from the drift study only."
```

(Adjust the "all four modes" line to the measured truth if the decision gate fired.)

### Task 6: Study 005 — the frozen-worlds diagnosis (preregistered)

Spec §4 fix 3: a **written diagnosis** of the ~60% frozen-dominant biome skew — distributions change only if the cause is a defect, and not in this campaign.

**Files:**
- Create: `book/src/laboratory/study-005.md`
- Modify: `book/src/SUMMARY.md` (add `- [Study 005: The Frozen Worlds](./laboratory/study-005.md)` after the Study 004 line)

**Interfaces:**
- Consumes: `lab-out/census-of-lands/rows.csv` from Task 5 (columns used: `dominant-land-biome`, `star-class`, `obliquity-degrees`, `mountain-coverage`, `habitable-fraction`, `tidally-locked`, `mean-land-temperature-c` — the last from Task 3).
- Produces: the diagnosis page with a verdict: **defect** or **modeling consequence**.

- [ ] **Step 1: Write the preregistration section FIRST and commit it before any analysis**

ADR 0016: hypotheses are stated before the study runs. Create `book/src/laboratory/study-005.md` with a header matching the study-001..004 pages' altitude, containing (verbatim structure, prose at book altitude):

- **Question:** why is the dominant land biome frozen (ice or alpine) in ~60% of worlds, and why do desert, grassland, shrubland, and rainforest never dominate?
- **H1 (insolation):** ice-dominance is predicted by low `mean-land-temperature-c`; the temperature distribution over worlds straddles freezing because of where the habitable-zone placement and stellar-class envelope put the planet — i.e. cold worlds are cold, not misclassified.
- **H2 (relief):** alpine-dominance is predicted by high `mountain-coverage` — a consequence of the uplift model, independent of temperature.
- **H3 (classification):** if neither H1 nor H2 holds — i.e. warm, low-relief worlds still come out frozen-dominant — the biome classifier's thresholds are misassigning classes, which would be a **defect**.
- **Analysis plan:** cross-tabulate `dominant-land-biome` against buckets of `mean-land-temperature-c` and `mountain-coverage`, and against `star-class`, `obliquity-degrees`, and `tidally-locked`, over the 10,000-seed Census of Lands. Verdict criteria: H1/H2 confirmed → modeling consequence, no code change in this campaign, note candidate Year-2+ envelope adjustments as an explicit follow-up decision; H3 confirmed → file the defect, still no silent retune (fix goes through its own reviewed change).

Commit the preregistration alone:

```bash
git add book/src/laboratory/study-005.md book/src/SUMMARY.md
git commit -m "docs(lab): preregister Study 005 — the frozen-worlds diagnosis (ADR 0016)"
```

- [ ] **Step 2: Run the analysis**

Cross-tabs over `lab-out/census-of-lands/rows.csv` with scratch scripts (awk or python in the scratchpad — author-time analysis, not committed tooling). Example for H1:

```bash
awk -F, 'NR==1{for(i=1;i<=NF;i++){if($i=="dominant-land-biome")b=i;if($i=="mean-land-temperature-c")t=i}}
NR>1 && $t!=""{bucket=($t<-10?"<-10":($t<0?"-10..0":($t<10?"0..10":">=10")));print bucket, $b}' \
  lab-out/census-of-lands/rows.csv | sort | uniq -c | sort -k2,2 -k1,1rn
```

Repeat for `mountain-coverage` vs alpine-dominance, and the star-class/obliquity/lock cross-tabs from the preregistered plan.

- [ ] **Step 3: Write the findings and verdict into study-005.md**

Fill the page's Results section with the measured tables (small markdown tables, numbers explained in prose) and state the verdict against the preregistered criteria. Do not soften: if it's a defect, say defect; if it's the model doing what the model card says, say that, and record the candidate follow-ups (e.g. star-class envelope, obliquity draw range) as *decisions deferred*, with pointers.

- [ ] **Step 4: Build, gate, commit**

Run: `mdbook build book && cargo fmt --check`
Expected: clean.

```bash
git add book/src/laboratory/study-005.md
git commit -m "docs(lab): Study 005 findings — the frozen-worlds verdict"
```

### Task 7: Refresh the seed-42 capstones, write the chronicle, sweep for freshness

**Files:**
- Modify: `book/src/gallery/the-gods-seed-42.md`, `book/src/gallery/world-seed-42.md` (curated pages quoting seed-42 placement facts; NOT in the CI regen list, so they don't auto-heal)
- Create: `book/src/chronicle/campaign-y2-0.md`
- Modify: `book/src/SUMMARY.md` (chronicle entry, mirroring how campaign-5 is listed)
- Modify: any other chapter the freshness grep finds

**Interfaces:**
- Consumes: the regenerated seed-42 worlds from Task 4 (`/tmp/hv-ci-sky.json`, `/tmp/hv-ci-locked.json` — rebuild them if gone).
- Produces: a book that nowhere describes the pre-fix world.

- [ ] **Step 1: Diff the seed-42 story**

Rebuild both seed-42 worlds (spinning + locked), render both almanacs, and list what the curated pages assert: flagship name, cell/biome/coast, caste count, priesthood, head deity, minor deities. Compare old page text against the new almanacs.

- [ ] **Step 2: Update the curated pages**

Update every stale fact in `the-gods-seed-42.md` and `world-seed-42.md` to the new seed-42 reality, preserving the pages' prose altitude. **Decision gate:** if the *qualitative* story broke — e.g. the spinning and locked worlds no longer share flagship/structure, so "the only thing that differs is the sky" stops being true — STOP and checkpoint with Nathan before rewriting: the Year-1 exit-demo narrative is his taste call. (Note: the fix is rotation-independent, so both variants' placement shifts identically unless climate differences between spinning/locked move the argmax differently — check rather than assume.)

- [ ] **Step 3: Freshness grep**

Run: `grep -rn "always coastal\|100%\|68.8\|31.1\|57.7\|coastal flagship" book/src/ --include=*.md | grep -v generated | grep -v study-005`
Expected: hits only in pages already updated (Tasks 5–7) or in historical/chronicle context that explicitly narrates the past (leave those — chronicles are records, not living docs). Fix anything else.

- [ ] **Step 4: Write the chronicle entry**

Create `book/src/chronicle/campaign-y2-0.md` in the voice and shape of `book/src/chronicle/campaign-5.md`: what the campaign set out to fix, the one-line freshwater conflation and why it produced a 100% invariant, the new measured distributions, the four-modes outcome, the Study 005 verdict, and what this re-baseline buys Year 2 (every later study inherits these numbers exactly once). Add the SUMMARY.md entry.

- [ ] **Step 5: Build and commit**

Run: `mdbook build book`

```bash
git add book/src/
git commit -m "docs(book): campaign Y2-0 chronicle, seed-42 capstone refresh, freshness sweep"
```

### Task 8: Final gate and merge readiness

**Files:** none new.

- [ ] **Step 1: The full gate, from clean**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green (the calibration tests take minutes).

- [ ] **Step 2: The CI artifact check, exactly as CI runs it**

Re-run the full Task 4 Step 1 command list, then:
Run: `git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/ ; echo "exit: $?"`
Expected: `exit: 0` — byte-identical regeneration.

- [ ] **Step 3: Verify the working tree is clean and the branch is merge-ready**

Run: `git status --short`
Expected: empty (the formerly-untracked `census-of-lands/` chart dir is now gitignored; `.superpowers/` should also be ignored or absent).

- [ ] **Step 4: Hand off**

Use `superpowers:finishing-a-development-branch` to present merge options (Year-1 pattern: no-ff merge to main closing the campaign).

## Self-Review Notes

- **Spec coverage:** §4 fix 1 → Tasks 1–2; fix 2 → Task 5 Step 3 (with the decision gate the spec's "audit" implies); fix 3 → Task 6 (diagnosis only, no retune, epoch-suffix rule respected by changing nothing); fix 4 → Tasks 4–5 (regenerate once, after all code changes). Out-of-scope list honored: no name-pool, no culture-scope, no time-axis work.
- **Contracts:** no stream labels added, renamed, or reordered; the freshwater change is a derived formula at the composition root. The only permanent additions are one Lab metric (code, not a draw) and calibration pins (tests).
- **Known unknowns, handled explicitly:** exact coastal/inland counts (measured then pinned, Task 2); workspace fallout list (surveyed Task 1, re-pinned Task 2); four-modes outcome (decision gate, Task 5); seed-42 narrative survival (decision gate, Task 7); frozen-worlds verdict (preregistered criteria, Task 6).
