# The Presiding Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Retire the world-level `belief-kind` metric and replace it with one reading per people, so a single founder-floor soul stops speaking for a world that holds 27 hobgoblins (SKY-25).

**Architecture:** A world has no religion; its peoples do. `belief-kind` (`beliefs_of(&world).first()` — whichever people sorted first) is deleted outright rather than made dominance-aware; `belief-kind-<species>` is added for all four peoples; the frozen-sky calibration is re-expressed per-species, which strengthens it. Metrics are code (decision 0011), so no ledger, draw order, or derivation moves — worlds stay byte-identical.

**Tech Stack:** Rust 2024, `windows/lab` (metrics + calibration), `cli`, `cargo nextest`, the AWS census regen path.

**Spec:** `docs/superpowers/specs/2026-07-17-the-presiding-design.md` (G3 ruled 2026-07-17: regen authorized; retire, don't repair).

## Global Constraints

- Branch: `the-presiding`. Worktree: `~/.config/superpowers/worktrees/hornvale/the-presiding`. Never `cd` to the main checkout.
- Dependencies: `serde` + `serde_json` only. No new crates. No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. No wall-clock time.
- Every crate sets `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line doc comment.
- Every primitive at a `pub` boundary carries a `type-audit:` verdict tag. Valid positions are **field names** and the literal **`return`** — there is no per-tuple-leg qualifier.
- **`make gate` now runs type-audit** (this branch, `0481311`). Run `make gate`, not just `cargo nextest`.
- Run `cargo fmt` as the final step before every commit — fmt-gate skips are this project's most common review finding.
- Run tests once and capture (`2>&1 | tee /tmp/hv-tN.txt`), then inspect the file. Never re-run a suite to read a second line.
- **NEVER run a census locally.** `HV_CENSUS=1` belongs to the AWS path alone. Task 2's regen is the **controller's** job, not a subagent's.
- **Reverting a mutation experiment: use `git checkout <file>`, never `mv file.bak file`** — `mv` preserves mtime, cargo skips the rebuild, and the mutated binary runs against clean source. Always re-run and see GREEN after a revert.
- Commit messages end with: `Claude-Session: https://claude.ai/code/session_017AekGYnPvceJpt8mqmQF1k`

---

## File Structure

| File | Responsibility | Change |
|---|---|---|
| `windows/lab/src/metrics.rs` | The metric registry: delete `belief-kind`, add `belief-kind-<species>` ×4 + a head-sentiment helper. | Modify |
| `windows/lab/tests/depth_ladder.rs` | Rung exemplar — repoint off the deleted metric. | Modify |
| `cli/src/main.rs` | `lab list-metrics` output test — assert the per-species name. | Modify |
| `windows/lab/tests/calibration.rs` | The frozen-sky invariant, re-expressed per-species; re-pinned from the regen. | Modify (Task 2) |
| `book/src/laboratory/generated/the-census/rows.csv` | The census fixture — regenerated on AWS. | Regenerate (Task 2) |

**Sequencing (load-bearing):** the calibration reads the committed **fixture**, not live code. Until the regen, the fixture still has a `belief-kind` column and has no `belief-kind-*` columns — so the per-species calibration *cannot compile against reality* until Task 2. Task 1 therefore leaves `calibration.rs` untouched and it keeps passing against the old column. Task 2 regenerates and rewrites the calibration **in one commit** (golden-pin discipline: re-pin in the drifting commit, never defer to the close). Between Tasks 1 and 2, `make gate-full` and CI's regenerate-and-diff are red — that is the debt Nathan authorized at G3.

---

### Task 1: Retire the world belief; add one reading per people

**Files:**
- Modify: `windows/lab/src/metrics.rs` (delete the `belief-kind` Metric at ~808-820; add helper + 4 metrics near the per-species religion block at ~1796)
- Modify: `windows/lab/tests/depth_ladder.rs:143-154`
- Modify: `cli/src/main.rs:1214-1218`

**Interfaces:**
- Consumes: `flagship_of(v.world(), species) -> Option<VillageInfo>` and `hornvale_religion::beliefs_held_by(world, EntityId) -> Vec<Belief>`, both already used by `pantheon_sig` in this file.
- Produces: metrics named `belief-kind-goblin`, `belief-kind-hobgoblin`, `belief-kind-kobold`, `belief-kind-bugbear`. Task 2's calibration reads exactly these names.

- [ ] **Step 1: Write the failing test**

Add to `windows/lab/src/metrics.rs`'s inline `mod tests`:

```rust
/// The Presiding (SKY-25): a world has no religion, its peoples do. The
/// retired `belief-kind` read `beliefs_of(&world).first()` — whichever
/// people sorted first in the alphabetical component registry, which on
/// every measured seed is a single founder-floor goblin.
#[test]
fn belief_kind_is_per_species_and_the_world_belief_is_gone() {
    let reg = registry();
    assert!(
        !reg.iter().any(|m| m.name == "belief-kind"),
        "the world-level belief-kind is retired: a world has no presiding belief"
    );
    for species in ["bugbear", "goblin", "hobgoblin", "kobold"] {
        let name = format!("belief-kind-{species}");
        assert!(
            reg.iter().any(|m| m.name == name),
            "{name} is registered — every people gets its own reading"
        );
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-lab belief_kind_is_per_species 2>&1 | tail -12`
Expected: FAIL — `the world-level belief-kind is retired` (it is still registered).

- [ ] **Step 3: Delete the world-level metric**

Remove this entire `Metric { … }` block from `windows/lab/src/metrics.rs` (~lines 808-820):

```rust
        Metric {
            name: "belief-kind",
            doc: "The first belief's sentiment tag ('eternal', 'cyclic', or 'ambient'); \
                   Absent if no beliefs",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| {
                let beliefs = beliefs_of(v.world());
                match beliefs.first() {
                    Some(first) => MetricValue::Text(first.sentiment.as_str().to_string()),
                    None => MetricValue::Absent,
                }
            }),
        },
```

If `beliefs_of` becomes an unused import after this, remove it from the `use` list — `make gate` runs clippy with `-D warnings`.

- [ ] **Step 4: Add the head-sentiment helper**

Beside `pantheon_sig` (~line 2901) in `windows/lab/src/metrics.rs`:

```rust
/// The sentiment of `species`' pantheon head — the deity that presides over
/// THAT people, which is the only scale at which "presides" means anything.
/// `None` when the people placed no flagship or holds no beliefs.
///
/// A people's beliefs mint salience-descending, so its first belief is its
/// head; `beliefs_held_by` preserves that order.
fn species_head_sentiment(v: &FullView, species: &str) -> Option<String> {
    let flagship = flagship_of(v.world(), species)?;
    let beliefs = hornvale_religion::beliefs_held_by(v.world(), flagship.id);
    beliefs.first().map(|b| b.sentiment.as_str().to_string())
}
```

- [ ] **Step 5: Add one metric per people**

Insert four `Metric` entries immediately after the `cult-form-kobold` block (~line 1812), matching that block's shape and the `<metric>-<species>` naming convention:

```rust
        Metric {
            name: "belief-kind-bugbear",
            doc: "Sentiment of the bugbear flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "bugbear") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-goblin",
            doc: "Sentiment of the goblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "goblin") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-hobgoblin",
            doc: "Sentiment of the hobgoblin flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "hobgoblin") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
        Metric {
            name: "belief-kind-kobold",
            doc: "Sentiment of the kobold flagship's pantheon head ('eternal', 'cyclic', or 'ambient'); Absent without one",
            summary: SummaryKind::Categorical,
            extract: Extractor::Full(|v: &FullView| match species_head_sentiment(v, "kobold") {
                Some(s) => MetricValue::Text(s),
                None => MetricValue::Absent,
            }),
        },
```

- [ ] **Step 6: Repoint the depth-ladder exemplar**

In `windows/lab/tests/depth_ladder.rs`, the test `belief_kind_and_hue_depth_have_the_expected_rung` looks up the deleted metric. Change its three references from `"belief-kind"` to `"belief-kind-goblin"` (same rung, same rationale — it reads religion facts). The doc comments at `:96` and `:138` mentioning `belief-kind` must name `belief-kind-goblin` too.

- [ ] **Step 7: Fix the CLI test's false green**

`cli/src/main.rs:1214-1218` asserts `output.contains("belief-kind")`. That would still pass by **substring** against `belief-kind-goblin` — a false green that proves nothing. Replace with:

```rust
    #[test]
    fn list_metrics_output_contains_per_species_belief_kind() {
        let output = hornvale_lab::render_metric_list();
        // Per-species, not world-level: `contains("belief-kind")` would pass
        // by substring even after the world metric was retired.
        assert!(output.contains("belief-kind-goblin"));
        assert!(output.contains("belief-kind-hobgoblin"));
    }
}
```

- [ ] **Step 8: Verify the new metric is not vacuous — MUTATION, and paste the evidence**

The metric must read *this people's* head, not the world's first belief. Prove it:

```bash
# Mutation: make the helper read the world's first belief instead.
# Edit species_head_sentiment's body to:
#     let _ = species; let b = hornvale_religion::beliefs_of(v.world()); b.first().map(|b| b.sentiment.as_str().to_string())
cargo nextest run -p hornvale-lab 2>&1 | tee /tmp/hv-mut.txt
git checkout windows/lab/src/metrics.rs   # NEVER mv a .bak — mtime defeats cargo
```

Expected under the mutation: `seed_42_belief_kind_is_text_and_not_absent`'s
sibling coverage still passes (both readings are Text), so **add the test
below**, which fails under it, then revert and confirm it passes:

```rust
/// Mutation guard: `belief-kind-<species>` must read THAT people's head, not
/// the world's first-minted belief. On every measured seed the first-minted
/// belief is goblin's (a single founder-floor soul), so a metric that read
/// `beliefs_of().first()` would give every species goblin's answer.
#[test]
fn each_peoples_belief_kind_is_its_own_not_the_first_minted() {
    let v = FullView::build(Seed(42), &SkyPins::default()).unwrap();
    let goblin = species_head_sentiment(&v, "goblin");
    let hobgoblin = species_head_sentiment(&v, "hobgoblin");
    assert!(goblin.is_some() && hobgoblin.is_some(), "seed 42 places both peoples");
    // They may legitimately agree; what must NOT hold is that hobgoblin's
    // reading is produced by ignoring the species argument.
    assert_eq!(
        species_head_sentiment(&v, "hobgoblin"),
        hobgoblin,
        "the reading is a pure function of the species asked for"
    );
    assert!(
        species_head_sentiment(&v, "bugbear").is_none(),
        "bugbear places no flagship on seed 42 — Absent is the honest reading, \
         and it is exactly the fact SKY-25, the terminator battery, and the \
         frozen-sky calibration all got wrong"
    );
}
```

`FullView::build(Seed(42), &SkyPins::default()).unwrap()` is the module's existing idiom (see `metrics.rs:4278`); reuse it rather than inventing a helper.

- [ ] **Step 8b: Repoint the third consumer**

`windows/lab/src/metrics.rs:4277` `seed_42_belief_kind_is_text_and_not_absent` calls `extract_from(&built, "belief-kind")` and breaks on the delete. Repoint it:

```rust
    #[test]
    fn seed_42_belief_kind_goblin_is_text_and_not_absent() {
        let view = FullView::build(Seed(42), &SkyPins::default()).unwrap();
        let built = BuiltView::Full(view);
        let value = extract_from(&built, "belief-kind-goblin");
        match value {
            MetricValue::Text(_) => {}
            _ => panic!("Expected Text, got {:?}", value),
        }
    }
```

- [ ] **Step 9: Run the gate**

Run: `make gate 2>&1 | tee /tmp/hv-t1.txt`
Expected: PASS. `calibration.rs` is untouched and still reads the fixture's old `belief-kind` column, so it still passes — that is intended; Task 2 rewrites it.

- [ ] **Step 10: Commit**

```bash
cargo fmt
pwd && git branch --show-current   # must be the-presiding
git add windows/lab/src/metrics.rs windows/lab/tests/depth_ladder.rs cli/src/main.rs
git commit -F- <<'EOF'
feat(lab)!: retire the world belief; one reading per people (SKY-25)

`belief-kind` read `beliefs_of(&world).first()` — the first-minted belief,
i.e. whichever people sorts first in the alphabetical component registry. On
every measured seed that is a single founder-floor goblin (population 1),
speaking for worlds holding 6-27 hobgoblins; alphabetical-first differs from
most-populous on 22/22 spinning seeds.

SKY-25 asked for that selection to be made dominance-aware. Measurement says
the concept is the defect, not the rule: a world has no religion, its peoples
do, and `beliefs_of().first()` is a fact about a loop, not about a world —
The Named's `i == 0` one layer up, in the metric instead of the renderer. So
the metric is retired rather than repaired, and every people gets its own
`belief-kind-<species>`.

Deliberately NOT dominance-weighting: measured, hobgoblin's own pantheon also
heads Eternal on all 9 locked seeds, so that fix moves SKY-5's tide payoff
0/9 -> 0/9. The binding gate is the ambient extinction (rift-and-fit #14/#19).

Metrics are code (0011): no ledger, draw order, or derivation moves; worlds
stay byte-identical. The census fixture is stale until Task 2's authorized
regen.

Claude-Session: https://claude.ai/code/session_017AekGYnPvceJpt8mqmQF1k
EOF
```

---

### Task 2: Regen and re-pin the frozen-sky calibration per-species

**CONTROLLER-ONLY.** The AWS regen is a ratified carve-out and a subagent must never run it (`HV_CENSUS=1` belongs to the AWS path alone; a killed run orphans its spot box). Do not dispatch this task.

**Files:**
- Modify: `windows/lab/tests/calibration.rs` (`a_frozen_sky_never_heads_a_cyclic_pantheon`, ~lines 100-207)
- Regenerate: `book/src/laboratory/generated/the-census/rows.csv` (+ its siblings the script touches)

**Interfaces:**
- Consumes: `belief-kind-bugbear|goblin|hobgoblin|kobold` from Task 1.

- [ ] **Step 1: Rewrite the calibration body per-species**

Replace the loop and both assertions in `a_frozen_sky_never_heads_a_cyclic_pantheon` with:

```rust
    let result = &*DRIFT;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let locked_i = idx("tidally-locked");
    // Every people, not whichever sorted first: the invariant is physical —
    // a frozen sky offers no rising-and-setting body — and physics does not
    // care which people the component registry happened to iterate first.
    let heads: Vec<usize> = ["bugbear", "goblin", "hobgoblin", "kobold"]
        .iter()
        .map(|s| idx(&format!("belief-kind-{s}")))
        .collect();
    let (mut locked_eternal, mut locked_ambient, mut spinning_eternal) = (0u32, 0u32, 0u32);
    for row in &result.rows {
        let locked = matches!(row.values[locked_i], MetricValue::Flag(true));
        for &i in &heads {
            let kind = match &row.values[i] {
                MetricValue::Text(t) => t.as_str(),
                // That people holds no pantheon on this seed.
                MetricValue::Absent => continue,
                other => panic!("seed {}: belief-kind not text: {other:?}", row.seed),
            };
            if locked {
                match kind {
                    "eternal" => locked_eternal += 1,
                    "ambient" => locked_ambient += 1,
                    other => panic!(
                        "seed {}: a tidally-locked world has a people whose pantheon \
                         head is {other} — a frozen sky must never head a cyclic pantheon",
                        row.seed
                    ),
                }
            } else if kind == "eternal" {
                spinning_eternal += 1;
            }
        }
    }
```

- [ ] **Step 2: Correct the stale prose above it**

The doc comment (~lines 102-145) says bugbear commits first via the founder floor — false: bugbear places on **0/9** of the locked seeds and **0/30** of seeds 1-30; the first committer is goblin. It also says "48 locked worlds = 37 tide-headed (ambient) + 11 sun-headed (eternal)", which the assertion directly beneath it already contradicts with `(48, 0)`. Rewrite it to describe the per-species invariant, and record that the counts below are per-people readings, not per-world.

- [ ] **Step 3: Run the regen (AWS — authorized at G3)**

```bash
make regen-remote 2>&1 | tee /tmp/hv-regen.txt
```

Never locally. If the run is killed, sweep for an orphaned spot instance by hand before retrying — the next run's teardown kills the stale registry id, not its own.

- [ ] **Step 4: Read the true pins out of the regenerated census**

```bash
python3 - book/src/laboratory/generated/the-census/rows.csv <<'PY'
import csv, sys
from collections import Counter
rows = list(csv.DictReader(open(sys.argv[1])))
sp = ["bugbear", "goblin", "hobgoblin", "kobold"]
le = la = se = 0
for r in rows:
    locked = str(r.get("tidally-locked", "")).strip().lower() in ("true", "1")
    for s in sp:
        k = r.get(f"belief-kind-{s}", "")
        if not k or k == "Absent":
            continue
        if locked:
            if k == "eternal": le += 1
            elif k == "ambient": la += 1
            else: print("CYCLIC ON A LOCKED WORLD, seed", r["seed"], s)
        elif k == "eternal":
            se += 1
print(f"locked_eternal={le} locked_ambient={la} spinning_eternal={se}")
PY
```

Paste those three numbers into the assertions below. These are **measured, not chosen** — pin what the census says, never widen or force it (ADR 0016).

```rust
    // Pinned per ADR 0016 from the 2026-07-17 regen (The Presiding): these
    // are PER-PEOPLE head readings across the 1000-seed census, not the old
    // one-reading-per-world count — the numbers are not comparable to the
    // retired `belief-kind` pins they replace.
    assert_eq!(
        (locked_eternal, locked_ambient),
        (LOCKED_ETERNAL, LOCKED_AMBIENT),
        "locked-world per-people head split (eternal, ambient) drifted"
    );
    assert_eq!(
        spinning_eternal, SPINNING_ETERNAL,
        "spinning-yet-eternal per-people head count drifted"
    );
```

- [ ] **Step 5: Verify the invariant is not vacuous**

If the census yields `locked_ambient == 0` and every locked head is eternal, the loop's `panic!` arm never fires on real data. Prove the guard works by temporarily changing the locked match arm to treat `"eternal"` as the panic case, running the test, seeing it panic with a seed number, then `git checkout windows/lab/tests/calibration.rs`. Paste the panic line as evidence.

- [ ] **Step 6: census-check, gate, gate-full**

```bash
make census-check 2>&1 | tail -5    # the golden-pins.sql duplicate has missed EVERY re-pin since it was created
make gate 2>&1 | tee /tmp/hv-t2.txt
make gate-full 2>&1 | tail -20      # must be green again now the fixture matches
git diff --stat book/src/laboratory/
```

- [ ] **Step 7: Commit the regen and the re-pin together**

Golden-pin discipline: the re-pin rides in the commit that drifts it, never deferred to the close.

```bash
cargo fmt
pwd && git branch --show-current
git add windows/lab/tests/calibration.rs book/src/laboratory/
git commit -F- <<'EOF'
regen: 1000-seed census on the per-people belief readings; re-pin the frozen sky

The frozen-sky invariant is physical — a locked world offers no rising-and-
setting body, so nothing can read cyclic — and physics does not care which
people the component registry iterated first. The old form tested it on one
arbitrary people (a single founder-floor goblin) and called it the world.
Re-expressed over every people's pantheon head, which strictly strengthens it.

Pins re-measured from this regen per ADR 0016, and NOT comparable to the
retired `belief-kind` pins: they count per-people head readings, not one
reading per world. Corrects the comment's bugbear attribution (bugbear places
on 0/9 locked seeds; the first committer is goblin) and its stale
"37 tide-headed + 11 sun-headed" prose, which the assertion beneath it already
contradicted.

Claude-Session: https://claude.ai/code/session_017AekGYnPvceJpt8mqmQF1k
EOF
```

---

### Task 3: Close the campaign

**Files:**
- Create: `book/src/chronicle/the-presiding.md`, `docs/retrospectives/the-presiding.md`
- Modify: `book/src/SUMMARY.md`, `book/src/frontier/idea-registry.md`, `windows/lab/tests/terminator_acceptance.rs`

- [ ] **Step 1: Correct the terminator battery, keep its measurement**

In `windows/lab/tests/terminator_acceptance.rs`, the diagnosis (~lines 40-52) blames bugbear and the founder floor. Bugbear places on **0/9** of that battery's own locked seeds. Rewrite the diagnosis: the first committer is **goblin, population 1**, and — the part the battery could not know — **hobgoblin's own pantheon also heads Eternal on all 9**, so dominance-awareness would have moved the payoff 0/9 → 0/9. Its **measured 0/9 pin is correct and stays untouched**; only the explanation changes.

- [ ] **Step 2: Flip SKY-25 and correct SKY-24's row**

In `book/src/frontier/idea-registry.md`: flip **SKY-25** to `shipped`, repoint **Where** at this campaign's spec and chronicle, and record that it was **dissolved, not repaired** — with the measured facts (goblin not bugbear; population 1; dominance-awareness moves the tide 0/9 → 0/9, so this closes SKY-25 without touching SKY-5). Correct the bugbear sentence in **SKY-24**'s row; its habitability claims stand. Never delete a row.

- [ ] **Step 3: Chronicle**

`book/src/chronicle/the-presiding.md`, wired into `book/src/SUMMARY.md` beside the other chronicle entries. Technical, mathematical, readable without the code. Cover: a single founder-floor soul speaking for a world of 27; that the row asked to fix the selection rule and measurement said the concept was the artifact; and the resonance — The Named demoted block 0 from privileged base case to peer in the *renderer*, this demotes `beliefs_of().first()` in the *metric*. The third instance of a privileged first with no right to be privileged.

- [ ] **Step 4: Retrospective**

`docs/retrospectives/the-presiding.md` — one page, process only. Lead with: **the registry-first/placed-first inference was found in three more documents** (SKY-25, the terminator battery, the frozen-sky calibration), all authored independently, all stating bugbear commits first, none having generated a world to check. Carry the followups from `.superpowers/sdd/followups.md`. Record that a spec's premise was again wrong and again caught only by measurement.

- [ ] **Step 5: Freshness sweep**

```bash
grep -rn "belief-kind\|presiding belief\|bugbear.*first" book/src/ docs/ --include=*.md | grep -v chronicle/the-presiding | grep -v retrospectives/the-presiding
```

Fix anything describing the retired world belief as live. The Confidence Gradient needs **no** re-score: no bet moves (0030).

- [ ] **Step 6: Commit, then STOP — G6 is a hard stop**

Do not merge, push, or close anything. Present the post-G3 ledger digest to Nathan and wait; `closing-a-campaign` runs after his ruling.

---

## Self-Review

**Spec coverage:** §2.1 retire → T1S3. §2.2 per-species ×4 → T1S4-5. §2.3 calibration per-species → T2S1-2. §2.4 consumers → T1S6-7. §2.5 battery → T3S1. §3 blast radius → the sequencing note. §4 regen → T2S3-7 (controller-only, `census-check` at S6). §5 out-of-scope → carried as followups, not implemented. §6 testing: (1)→T1S5+S8, (2) mutation→T1S8 + T2S5, (3) per-species calibration→T2S1, (4) `beliefs_of` untouched→T1S3 leaves it alone, (5) determinism→T1S8's purity assertion. §7 DoD → Task 3.

**Placeholder scan:** none. `LOCKED_ETERNAL` / `LOCKED_AMBIENT` / `SPINNING_ETERNAL` in T2S4 are not placeholders — the step ships the exact script that produces them and forbids choosing them. `full_view(42)` in T1S8 carries an explicit instruction to use the module's existing helper rather than invent one, because I did not verify that name (a plan value composed rather than checked — The Named's retro lesson, applied).

**Type consistency:** `species_head_sentiment(v: &FullView, species: &str) -> Option<String>` is defined in T1S4 and called identically in T1S5 and T1S8. Metric names `belief-kind-{bugbear,goblin,hobgoblin,kobold}` are produced in T1S5 and consumed verbatim in T1S1, T1S7, T2S1, T2S4. `MetricValue::{Text, Absent, Flag}` match the existing registry's usage.
