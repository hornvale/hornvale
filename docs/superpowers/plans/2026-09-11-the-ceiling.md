# The Ceiling Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Measure whether the underworld's energy composition can tell worlds
apart, then author the first settled people that lives on chemical energy
underground — where today nothing settled does.

**Architecture:** Stage 1 is measurement only and writes no production code; it
extends an existing off-gate probe over a frozen twelve-seed set. Stage 2
authors exactly one `BiosphereTraits` row and proves the new kind's
chemotrophic weight is load-bearing by ablation, not by description. A branch
table (spec §3.3) decides Stage 2's shape from Stage 1's result.

**Tech Stack:** Rust 2024, `cargo nextest`, no new dependencies (workspace
allowlist is `serde`/`serde_json`/`libm` only, decision 0004).

**Spec:** `docs/superpowers/specs/2026-09-11-the-ceiling-design.md`

**Ledger:** `docs/superpowers/ledgers/2026-09-11-the-ceiling.md` — write
rulings there **as they occur**, never to `.superpowers/sdd/`.

## Global Constraints

- **No new crates.** Allowlist is `serde`, `serde_json`, `libm`
  (`ALLOWED_EXTERNAL` in `cli/tests/architecture.rs`).
- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only; float sort is
  `total_cmp` (enforced by `clippy.toml` `disallowed-types`).
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field and
  variant gets a one-line doc comment.
- **`cargo fmt` is the last step before every commit.** fmt-gate skips are the
  most common review finding here.
- **Commit gate:** `make gate-commit` must pass on every commit.
- **Report every number before drawing any verdict** (spec §4). A statistic
  that prints only its verdict hides its distribution.
- **A falsified prediction is a finding, never a failure** (decision 0016).
  **Never retune a source, a constant, or an assignment to rescue a
  prediction.** If a measurement lands where the spec did not predict, record
  the measured value with today's date and assert on what was measured.
- **This campaign authors no marine kind, touches no `TrophicMode` variant
  list, and gives no `Surface`-realm kind a `CHEMOSYNTHATE` weight** — The
  Tidemark's boundary (its spec §7, confirmed on the wire both ways).
- **EVERY FILE THAT BUILDS A WORLD NEEDS A ROSTER ROW** in
  `cli/tests/fixtures/world-build-sites.tsv` (decision 0606), or
  `world_build_sites::no_unrostered_world_build_appears` REDs. The row is
  `<path>\t<count>\t<kind>` and the count is a **per-file ratchet on the
  number of world-build CALL SITES** — not invocations. Task 1 discovered
  this and added `windows/worldgen/tests/suite/ceiling_composition_probe.rs
  \t1\tidentity:1`. **Consequences for later tasks:** reuse the single
  `world_at` helper rather than adding a second call site, and if you do add
  one, bump that file's count in the SAME commit. A new probe file needs a
  new row. This is not in any task's `git add` list by default — it was
  missing from the plan's first draft and is called out here because the
  failure is invisible until the gate runs.

---

## File Structure

| file | responsibility |
|---|---|
| `windows/worldgen/tests/suite/ceiling_composition_probe.rs` | **Create.** Stage 1's M1 (per-rung composition separation) and M2 (mean vs max, with the positive control). A new file rather than an extension of `subterranean_energy_probe.rs`, because that file's two tests pin The Sources' own falsifications and must not be disturbed. |
| `windows/worldgen/tests/suite.rs` | **Modify.** Register each new probe module with `#[path = "suite/<name>.rs"] mod <name>;` — see its line 224 for `subterranean_energy_probe`. **There is no `tests/suite/mod.rs`**; an earlier draft named one and it does not exist (pre-flight ruling R1). |
| `domains/species/src/lib.rs` | **Modify.** Stage 2: one `BiosphereTraits` row in `biosphere_registry()`, one entry in `habitat_realm_registry()`. |
| `domains/species/tests/suite/metabolic_pairs.rs` | **Modify.** Its carrier list and count assertions move when a second `Chemotrophic` kind exists. |
| `windows/worldgen/tests/suite/ceiling_tenant_probe.rs` | **Create.** Stage 2's M4 ablation and the axis-discrimination readout. |

---

## Stage 1 — Measurement

### Task 1: The composition probe's scaffolding and its positive control

**Files:**
- Create: `windows/worldgen/tests/suite/ceiling_composition_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs`

**Interfaces:**
- Consumes: `subterranean_energy_probe.rs`'s own idiom for world construction.
  **Read that file first** — `world_at` (line 75), `SEEDS` (50), `Q6_SEEDS`
  (58), `UNDERGROUND_RUNGS` (63), `median` (213), `iqr` (238). Do not import
  them across files; test modules do not share private helpers. Copy the
  `world_at` idiom and cite the source file in a doc comment.
- Produces: `separation(rule)` and the per-seed sample builder that Task 2
  consumes.

- [ ] **Step 1: Read the source of truth before writing anything**

Read `windows/worldgen/tests/suite/subterranean_energy_probe.rs` in full.
Its `between_worlds_separation_and_within_world_width` doc comment carries the
exact Q1 formula, the twelve per-seed medians, and the measured
`separation = 0.145249`. Your control must reproduce that number.

- [ ] **Step 2: Write the positive control as a failing test**

The control comes first and alone. If the re-implementation cannot reproduce
the shipped rule's published number, nothing else in this file is
interpretable.

```rust
/// THE CEILING, M2 positive control: does this file's re-implementation of
/// Q1's `separation` reproduce the number `subterranean_energy_probe.rs`
/// published for the SHIPPED combination rule?
///
/// `0.145249` was measured 2026-08-26 and re-measured at this campaign's base
/// `26003913d`; it reproduced exactly. A mismatch here means this file is
/// measuring something else, and every M2 conclusion drawn from it would be
/// uninterpretable — so this test asserts and the rest of M2 depends on it.
///
/// claim: readout(off-gate, run by hand; the control for M2)
#[test]
#[ignore = "probe: M2's positive control; run by hand (The Ceiling, Stage 1)"]
fn mean_of_seven_reproduces_the_published_separation() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let sep = separation(&wc, CombinationRule::MeanOfSeven);
    println!("separation(mean-of-seven) = {sep:.6}");
    assert!(
        (sep - 0.145_249).abs() < 5e-7,
        "positive control FAILED: separation(mean-of-seven) = {sep:.6}, \
         expected 0.145249 as published by subterranean_energy_probe.rs. \
         This file is measuring something else; do not interpret M2."
    );
}
```

- [ ] **Step 3: Run it and watch it fail to compile**

Run: `cargo nextest run -p hornvale-worldgen --test suite --run-ignored all -E 'test(mean_of_seven_reproduces_the_published_separation)'`
Expected: FAIL — `separation` and `CombinationRule` do not exist.

**A red from a compile error proves nothing about an assertion.** It only
confirms the test is wired in. The behavioural red is Step 5.

- [ ] **Step 4: Implement `CombinationRule` and `separation`**

```rust
/// How the seven per-source yields at one chamber are combined into the one
/// scalar `separation` is computed over. The SHIPPED rule is the mean; `Max`
/// is M2's DIAGNOSTIC — the rule that discards the least composition, so it
/// bounds what the mean is costing. **Neither is proposed as a replacement.**
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CombinationRule {
    /// `subterranean_energy`'s own rule: the mean of all seven yields.
    MeanOfSeven,
    /// The composition-preserving extreme: the largest single yield.
    MaxOfSeven,
}

impl CombinationRule {
    fn combine(self, yields: &[f64]) -> f64 {
        match self {
            CombinationRule::MeanOfSeven => yields.iter().sum::<f64>() / yields.len() as f64,
            CombinationRule::MaxOfSeven => yields.iter().copied().fold(f64::MIN, f64::max),
        }
    }
}

/// Q1's formula, verbatim from `subterranean_energy_probe.rs`, with ONLY the
/// combination rule substituted:
///
/// ```text
/// separation = IQR({ median(E_s) }) / median({ IQR(E_s) })
/// ```
fn separation(wc: &WorldComponents, rule: CombinationRule) -> f64 {
    let mut medians = Vec::with_capacity(Q6_SEEDS.len());
    let mut iqrs = Vec::with_capacity(Q6_SEEDS.len());
    for &seed in &Q6_SEEDS {
        let mut sample = pooled_sample(wc, seed, rule);
        sample.sort_by(f64::total_cmp);
        medians.push(median(&mut sample.clone()));
        iqrs.push(iqr(&sample));
    }
    medians.sort_by(f64::total_cmp);
    iqrs.sort_by(f64::total_cmp);
    iqr(&medians) / median(&mut iqrs.clone())
}
```

`pooled_sample` walks every cave-bearing vertex at every rung of
`UNDERGROUND_RUNGS`, computes each `EnergySource::yield_at`, and applies
`rule.combine`. **Mirror `subterranean_energy_probe.rs`'s own loop** — same
`rung_evaluation_depth_m` guard, same `subterranean_substrate_field_per_rung`
moisture, same per-vertex `drainage`.

- [ ] **Step 5: Run the control and require it to PASS**

Run: `cargo nextest run -p hornvale-worldgen --test suite --run-ignored all --no-capture -E 'test(mean_of_seven_reproduces_the_published_separation)'`
Expected: PASS, printing `separation(mean-of-seven) = 0.145249`.

**If it prints any other number, STOP.** Do not adjust the tolerance. Read
`pooled_sample` against the published loop until they agree, and if they cannot
be made to agree, record that as a finding in the ledger — a committed number
that no longer reproduces is itself the result.

- [ ] **Step 6: `cargo fmt`, gate, commit**

```bash
cargo fmt
make gate-commit
git add windows/worldgen/tests/suite/ceiling_composition_probe.rs windows/worldgen/tests/suite.rs
git commit -m "test(the-ceiling): M2's positive control, reproducing 0.145249"
```

---

### Task 2: M2 — rock or mean?

**Files:**
- Modify: `windows/worldgen/tests/suite/ceiling_composition_probe.rs`

**Interfaces:**
- Consumes: `separation(&WorldComponents, CombinationRule) -> f64` and
  `CombinationRule::{MeanOfSeven, MaxOfSeven}` from Task 1.
- Produces: the M2 verdict that Task 4's branch table reads.

- [ ] **Step 1: Write the test, reporting before asserting**

```rust
/// THE CEILING, M2: is the magnitude compression caused by the ROCK or by
/// `subterranean_energy`'s mean-of-seven?
///
/// The metaplan attributes it to the rock ("roughly three near-constant
/// categorical states") and instructs rung 3 to design against that. A mean of
/// seven gated terms compresses by construction. Nothing had separated the two
/// causes.
///
/// PREREGISTERED (spec §4): `separation(MaxOfSeven) >= 0.25` — the bar Q1 set
/// and the shipped rule failed at 0.145249.
///
/// BOTH POLES SHIP. Clearing it means the combination rule is a major cause
/// and composition is more available than the metaplan's inherited diagnosis
/// implies. Failing it means the rock is the cause and that diagnosis stands
/// unqualified. Neither is a failure; record whichever was measured.
///
/// claim: readout(off-gate, prints both rules' separation before any verdict)
#[test]
#[ignore = "probe: M2, rock vs mean; run by hand (The Ceiling, Stage 1)"]
fn max_of_seven_separates_worlds_the_mean_does_not() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mean = separation(&wc, CombinationRule::MeanOfSeven);
    let max = separation(&wc, CombinationRule::MaxOfSeven);

    // Report BEFORE asserting: the pair is the finding, the bar is its floor.
    println!("separation(mean-of-seven) = {mean:.6}   [shipped rule, control]");
    println!("separation(max-of-seven)  = {max:.6}   [diagnostic]");
    println!("ratio max/mean            = {:.4}", max / mean);

    assert!(
        (mean - 0.145_249).abs() < 5e-7,
        "control drifted inside M2: mean-of-seven = {mean:.6}, expected 0.145249"
    );
    assert!(
        max >= 0.25,
        "PREREGISTRATION NOT MET (spec §4): separation(max-of-seven) = {max:.6} \
         < 0.25. The rock is the cause, not the combination rule; the \
         metaplan's inherited diagnosis stands. Record this measured value \
         with today's date in this doc comment and flip the assertion to the \
         measured branch. DO NOT retune any EnergySource."
    );
}
```

- [ ] **Step 2: Run it**

Run: `cargo nextest run -p hornvale-worldgen --test suite --run-ignored all --no-capture -E 'test(max_of_seven_separates)'`

- [ ] **Step 3: Record the measured result in the doc comment**

Whichever branch landed, write the measured numbers and today's date into the
test's doc comment, exactly as `subterranean_energy_probe.rs` does for its own
falsifications. If the prediction was not met, **flip the assertion to pin the
measured null** and say so in the message — never delete the test, never
loosen the bar to make it pass.

- [ ] **Step 4: Ledger the result**

Append an entry to `docs/superpowers/ledgers/2026-09-11-the-ceiling.md`
recording both numbers and which branch-table row Stage 2 is now on.

- [ ] **Step 5: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/worldgen/tests/suite/ceiling_composition_probe.rs docs/superpowers/ledgers/2026-09-11-the-ceiling.md
git commit -m "test(the-ceiling): M2 -- rock or mean, measured"
```

---

### Task 3: M1 — does composition separate worlds?

**Files:**
- Modify: `windows/worldgen/tests/suite/ceiling_composition_probe.rs`
- Possibly modify: `cli/tests/fixtures/world-build-sites.tsv` — **only if** you
  add a second world-build call site to this file. Reusing Task 1's existing
  `world_at` helper adds none and needs no change; see Global Constraints.

**Interfaces:**
- Consumes: Task 1's world-construction idiom.
- Produces: `M1` (a count) and the per-rung table Task 4 reads.

- [ ] **Step 1: Write the test**

**The statistic is PER-RUNG and that is the whole of its validity.** Pooling
the rungs would return the falsifier for a methodological reason: composition
is driven hard by depth, which every world shares (spec §4).

```rust
/// THE CEILING, M1: holding the rung fixed, do worlds disagree about which
/// energy source dominates?
///
/// ```text
/// h(s,r) = normalized dominant-source histogram for seed s at rung r
/// a(s,r) = argmax(h(s,r))
/// M1(r)  = |{ a(s,r) : s in S }|        -- distinct modal sources ACROSS WORLDS
/// M1     = max over r of M1(r)
/// ```
///
/// PREREGISTERED (spec §4): `M1 >= 2`. Two is what decision 0966's quadrants
/// require — the allocation axis must take more than one value across worlds.
/// `M1 == 1` is the falsifier and supersedes 0966.
///
/// claim: readout(off-gate, prints all sixty histograms and the per-rung
/// pairwise TV distances before any verdict)
#[test]
#[ignore = "probe: M1, per-rung composition separation; run by hand (The Ceiling, Stage 1)"]
fn composition_separates_worlds_at_some_rung() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let mut per_rung_distinct = Vec::new();

    for (ri, &rung) in UNDERGROUND_RUNGS.iter().enumerate() {
        let mut argmaxes = Vec::new();
        let mut hists = Vec::new();
        for &seed in &Q6_SEEDS {
            let h = normalized_dominant_histogram(&wc, seed, rung);
            println!("{rung:?} seed {seed}: h = {h:?}");
            argmaxes.push(argmax_index(&h));
            hists.push(h);
        }
        let mut distinct: Vec<usize> = argmaxes.clone();
        distinct.sort_unstable();
        distinct.dedup();

        // The TV distances make a noise-driven argmax visible rather than
        // hidden behind the count.
        let mut tvs = Vec::new();
        for i in 0..hists.len() {
            for j in (i + 1)..hists.len() {
                tvs.push(total_variation(&hists[i], &hists[j]));
            }
        }
        tvs.sort_by(f64::total_cmp);
        println!(
            "{rung:?}: M1(r) = {} distinct argmaxes {:?}, median pairwise TV = {:.4}",
            distinct.len(),
            distinct,
            median(&mut tvs.clone())
        );
        per_rung_distinct.push((ri, distinct.len()));
    }

    let m1 = per_rung_distinct.iter().map(|(_, n)| *n).max().unwrap_or(0);
    println!("M1 = {m1}");
    assert!(
        m1 >= 2,
        "PREREGISTRATION NOT MET (spec §4): M1 = {m1}. Every world shares one \
         modal source at every rung, so the allocation axis is constant across \
         worlds and decision 0966's quadrants are unreachable. This is the \
         NULL and it is the headline: record it, supersede 0966 with a record \
         choosing between C.3's original two, and take branch-table row 3 or 4. \
         DO NOT retune a source to spread the histogram."
    );
}
```

`total_variation(p, q) = 0.5 * Σ|p_i − q_i|`. `argmax_index` resolves an exact
tie by the LATER index, matching `dominant_source`'s documented
`Iterator::max_by` behaviour — state that in its doc comment.

- [ ] **Step 2: Run it**

Run: `cargo nextest run -p hornvale-worldgen --test suite --run-ignored all --no-capture -E 'test(composition_separates_worlds)'`

- [ ] **Step 3: Record the measurement in the doc comment and the ledger**

Same discipline as Task 2 Step 3. Record `M1(r)` for all five rungs
separately — a single rung carrying the whole result is a materially different
finding from all five carrying it.

- [ ] **Step 4: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/worldgen/tests/suite/ceiling_composition_probe.rs docs/superpowers/ledgers/2026-09-11-the-ceiling.md
git commit -m "test(the-ceiling): M1 -- per-rung composition separation, measured"
```

---

### Task 4: Stage-1 gate — read the branch table, record the verdict

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-11-the-ceiling.md`
- Modify: `docs/superpowers/specs/2026-09-11-the-ceiling-design.md` (§3.3 only:
  mark which row landed)

This task writes no code. It is the point where Stage 2's shape is fixed.

- [ ] **Step 1: Read spec §3.3 and identify the row**

Four rows, keyed on `(M1, M2)`. Write the landed row into the ledger with both
measured numbers.

- [ ] **Step 2: If row 4 landed (M1 == 1, rock is the cause), STOP and escalate**

Row 4 supersedes decision 0966. That is a decision-log act and a carve-out:
**present it to Nathan before writing any Stage 2 code.** Stage 2 still ships
(the consumer eats the aggregate supply), but 0966's supersession is his.

- [ ] **Step 3: Submit a stage gate**

```bash
make sluice-stage BRANCH=campaign/the-ceiling REF=$(git rev-parse HEAD)
```

Stage 1 is a plan-stage boundary. The stage gate merges main into the branch in
the chamber and gates that product without pushing.

- [ ] **Step 4: Commit the verdict**

```bash
git add docs/superpowers/ledgers/2026-09-11-the-ceiling.md docs/superpowers/specs/2026-09-11-the-ceiling-design.md
git commit -m "docs(the-ceiling): Stage 1 verdict -- branch <N> of the table"
```

---

## Stage 2 — The consumer

### Task 5: Author one settled subterranean chemotroph

**Files:**
- Modify: `domains/species/src/lib.rs` (`biosphere_registry()`,
  `habitat_realm_registry()`)
- Modify: `domains/species/tests/suite/metabolic_pairs.rs`

**Interfaces:**
- Consumes: Task 4's branch verdict — it decides whether the kind's niche
  favours a **named dominant source** (rows 1–2) or the **aggregate** supply
  (rows 3–4).
- Produces: a `KindId` that Task 6's ablation and Task 7's readout both use.
  **Write the chosen `KindId` string into the ledger** so later tasks do not
  guess it.

- [ ] **Step 1: Read `xorn`'s row as the template**

`domains/species/src/lib.rs:3800-3830`. It is the only existing
`Chemotrophic` kind and carries the reasoning for every field. `BiosphereTraits`
has exactly eight fields: `mass`, `thermal_strategy`, `trophic_mode`, `niche`,
`condition_niche`, `potency`, `social_form`, `schedule`.

- [ ] **Step 2: Write the failing test first**

```rust
#[test]
fn a_settled_chemotroph_exists_and_is_subterranean() {
    let bio = hornvale_species::biosphere_registry();
    let realms = hornvale_species::habitat_realm_registry();
    let id = hornvale_kernel::KindId(THE_KIND);
    let traits = bio.get(&id).expect("the settled chemotroph is registered");
    assert_eq!(traits.trophic_mode, hornvale_species::TrophicMode::Chemotrophic);
    assert_eq!(traits.social_form, hornvale_species::SocialForm::Settled);
    assert_eq!(
        realms.get(&id).copied(),
        Some(hornvale_species::HabitatRealm::Subterranean)
    );
    assert!(
        traits.niche.weight(hornvale_kernel::CHEMOSYNTHATE) > 0.0,
        "a settled chemotroph that weights no CHEMOSYNTHATE is the defect this \
         campaign exists to close"
    );
}
```

- [ ] **Step 3: Run it and confirm it fails**

Run: `cargo nextest run -p hornvale-species --test suite -E 'test(a_settled_chemotroph_exists)'`
Expected: FAIL — the kind is not registered.

- [ ] **Step 4: Add the row**

**The thermal/trophic pair is constrained.** `metabolic_pairs.rs`'s
`SANCTIONED` table admits `(Absent, Chemotrophic)` and nothing else pairs with
`Chemotrophic`. Either use `ThermalStrategy::Absent`, or extend `SANCTIONED`
deliberately — that table's own doc explains why adding a pair is a considered
act, and a count assertion will fire either way.

Author `condition_niche` against the two live axes (temperature, moisture);
spec §3.5 explains why the other two are starved underground.

- [ ] **Step 5: Run the test and the pair battery**

Run: `cargo nextest run -p hornvale-species --test suite`
Expected: the new test PASSES; `metabolic_pairs.rs`'s carrier-list and count
assertions FAIL, naming the old count.

- [ ] **Step 6: Update the pair battery's carrier list and counts**

Its assertions name `xorn` as the sole `Chemotrophic` carrier. Update them to
the measured set and say in the doc comment which campaign added the second.

- [ ] **Step 7: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add domains/species/src/lib.rs domains/species/tests/suite/metabolic_pairs.rs
git commit -m "feat(the-ceiling): a settled subterranean chemotroph"
```

---

### Task 6: M4 — the ablation

**Files:**
- Create: `windows/worldgen/tests/suite/ceiling_tenant_probe.rs`
- Modify: `windows/worldgen/tests/suite.rs`
- Modify: `cli/tests/fixtures/world-build-sites.tsv` — **required**, a new
  probe file that builds worlds has no roster row and the gate REDs without
  one. See Global Constraints for the row format.

**Interfaces:**
- Consumes: `THE_KIND` from Task 5 (read it from the ledger, do not invent it).
- Produces: the M4 verdict.

- [ ] **Step 1: Write the ablation test**

```rust
/// THE CEILING, M4: is the new kind's CHEMOSYNTHATE weight load-bearing, or
/// decorative?
///
/// ```text
/// arm (a) FULL      the authored kind, unmodified
/// arm (b) ABLATED   the same kind with its CHEMOSYNTHATE niche weight removed
/// PREDICTION: placed(b) < placed(a)
/// ```
///
/// `placed(b) == placed(a)` is a RED, not a curiosity: the kind places on its
/// other niche axes and chemical energy underground still feeds nothing
/// settled — the exact defect spec §2a documents. The remedy is to return to
/// the niche, NOT to write the equality up as a finding.
///
/// claim: readout(off-gate, prints both arms before any verdict)
///
/// **Arm (b) ablates the KIND'S NICHE, not the supply field — pre-flight
/// ruling R2, correcting this plan's first draft.**
/// `per_species_capacity_at` builds `chemosynthate_per_rung` INTERNALLY
/// (inside `per_species_capacity_at_with_invariant`); it is not an argument,
/// so no caller can zero the supply. What IS an argument is
/// `species_biosphere: &[&BiosphereTraits]`, so pass a modified traits value
/// whose `niche` drops `CHEMOSYNTHATE`. That measures the question M4 asks --
/// is this kind's chemotrophic weight load-bearing -- more directly than
/// zeroing the field would.
#[test]
#[ignore = "probe: M4, the chemotroph ablation; run by hand (The Ceiling, Stage 2)"]
fn the_chemosynthate_weight_is_load_bearing() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let full = placement_count(&wc, Ablation::Full);
    let ablated = placement_count(&wc, Ablation::ZeroChemosynthate);

    println!("placed(full)    = {full}");
    println!("placed(ablated) = {ablated}");

    assert!(
        ablated < full,
        "M4 FAILED: zeroing the CHEMOSYNTHATE supply changed nothing \
         ({ablated} == {full}). The weight is DECORATIVE — the kind places on \
         its other axes and chemical energy still feeds nothing settled. \
         Return to the niche authoring (Task 5); do not write this up as a \
         finding."
    );
}
```

- [ ] **Step 2: Run it and record**

Run: `cargo nextest run -p hornvale-worldgen --test suite --run-ignored all --no-capture -E 'test(the_chemosynthate_weight_is_load_bearing)'`

If it REDs, go back to Task 5 Step 4 and re-author the niche. **The three-attempt
rule applies:** after three failed authorings, stop, write what was tried and
why it failed into the ledger, and escalate.

- [ ] **Step 3: Add arm (c), the per-source diagnostic**

**Re-scoped by pre-flight ruling R2.** A per-source *placement* ablation is
unreachable: zeroing one `EnergySource` means changing a supply derivation
`per_species_capacity_at` performs internally. Arm (c) is a **supply-side
diagnostic** instead — over the kind's occupied vertices, report each
`EnergySource::yield_at` contribution and which `dominant_source` returns
(both are `pub`). That answers the provenance question — *which rock
chemistry does this kind actually sit on* — without claiming a placement
counterfactual the API cannot produce. **Reported, asserted on nothing.**
Call out `DetritalImport` separately in the report: it reads `drainage` and is
surface productivity flowing downward, so a kind depending on it is eating the
surface at depth rather than living on chemical energy.

- [ ] **Step 4: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/worldgen/tests/suite/ceiling_tenant_probe.rs windows/worldgen/tests/suite.rs cli/tests/fixtures/world-build-sites.tsv docs/superpowers/ledgers/2026-09-11-the-ceiling.md
git commit -m "test(the-ceiling): M4 -- the chemotroph ablation, with per-source arms"
```

---

### Task 7: The rung and axis-discrimination readout

**Files:**
- Modify: `windows/worldgen/tests/suite/ceiling_tenant_probe.rs`

- [ ] **Step 1: Report placement by rung, naming WHICH rung**

"At which rungs" is two questions, and D5B has now measured that they have
different answers. Every figure must name which of the two it reports; report
both where both are available. **Do not reconcile them** — that is D5B's.

**Their measurement, quoted so you do not re-derive a worse version** (board
`reply`, thread `the-ceiling`): on seed 5, the public endpoint probe found
**2 unique live underground Drow endpoints** (sites 894 and 14056). Both — 2
of 2 — had **capacity-winning rung `Nadir`** and **seated rung `Undercroft`**,
seated multiplier 0.875. The arm-observation count is 4 of 4 because each site
appears in both their arms; **the independent-site denominator is 2**.

**Treat this as a PRIOR, not as evidence.** Two sites on one seed for one
existing kind is an anecdote, and D5B scopes it explicitly as "the observed
Drow endpoint cohort, not a claim about every underground vertex or future
Ceiling kind". **Do not assert against it and do not assume it generalizes to
the kind this campaign authors.** What it earns is this: the readout must not
be built assuming the two rungs agree, and if this campaign's kind shows the
same split, say so with its own denominator rather than borrowing theirs.

**Why the direction is interesting enough to state.** The divergence is not a
near-miss — it is opposite ends of the ladder, and it runs against where the
energy is. S2 measured `fed` band occupancy opening only at `Nadir` (5.68%),
and M1's per-rung histograms put `SulphideOxidation` dominant in the deep
rungs. So the food is deep and the people are seated shallow. If this
campaign's chemotroph reproduces that, it is seated away from its own
subsistence, which is a finding worth the chronicle.

- [ ] **Step 2: Report how many tolerance axes discriminate the kind**

`ConditionNiche`'s four axes are temperature, moisture, insolation, elevation.
Report, per axis, whether it varies across the kind's occupied vertices.
Spec §3.5 explains why two are starved underground and why the count is the
successor's inheritance rather than this campaign's complaint.

- [ ] **Step 3: `cargo fmt`, gate, commit**

```bash
cargo fmt && make gate-commit
git add windows/worldgen/tests/suite/ceiling_tenant_probe.rs
git commit -m "test(the-ceiling): the rung and axis-discrimination readout"
```

---

### Task 8: The determinism branch check

**Files:**
- Modify: `docs/superpowers/ledgers/2026-09-11-the-ceiling.md`

**This task gates whether the campaign may commit at all** (spec §5). Run it
before any artifact regeneration.

- [ ] **Step 1: Establish which branch this change is on**

```bash
cargo run -p hornvale -- streams > /tmp/streams-after.txt
git show HEAD~1:<committed stream manifest path> > /tmp/streams-before.txt
diff /tmp/streams-before.txt /tmp/streams-after.txt
```

- **No new stream label and no reordering** → additive. Continue to Step 2.
- **A new stream label** → still additive (labels are independently keyed), but
  it is a save-format contract. Record it deliberately in the ledger and in the
  manifest.
- **An existing stream's consumption order moved** → **STOP.** That silently
  corrupts every world. It needs an epoch suffix and **returns to Nathan
  before anything else is written.**

- [ ] **Step 2: Rebaseline and inspect the moved artifacts**

```bash
make rebaseline
git diff --stat -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1)
```

A new kind moves placement, so placement-dependent artifacts SHOULD move. Read
the diff rather than accepting it — confirm what moved is what a new people
would move.

- [ ] **Step 3: `cargo fmt`, gate, commit the regenerated artifacts together**

```bash
cargo fmt && make gate-commit
git add -- $(grep -v '^#' docs/generated-paths.txt | grep -v '^$' | cut -f1) docs/superpowers/ledgers/2026-09-11-the-ceiling.md
git commit -m "chore(the-ceiling): rebaseline after the settled chemotroph"
```

---

### Task 9: Close — book, retrospective, census, merge

- [ ] **Step 1: Chronicle entry** — `book/src/chronicle/the-ceiling.md`.
  Report both stages' measurements including any falsified prediction; a null
  is the headline where one landed.

- [ ] **Step 2: Freshness sweep.** The book may never lag merged reality.
  Re-score any Confidence Gradient bet this campaign moved
  (`book/src/open-questions.md`, decision 0030).

- [ ] **Step 3: Registry rows.** Flip or amend `BIO-chemotrophy`,
  `BIO-underworld-has-no-energy` (its stale capacity clause, ledger #5
  Follow-ups), and the three rows this campaign added.

- [ ] **Step 4: Retrospective** — `docs/retrospectives/the-ceiling.md`
  (decision 0020). Process lessons, not product.

- [ ] **Step 5: Reconciliation row.** Update `the-ceiling` in
  `docs/audits/campaign-reconciliation.tsv` from `active` to its terminal
  disposition, and add the plan path to its `plans` column.

- [ ] **Step 6: Census.** A new kind moves the goldens the calibration
  batteries assert against.

```bash
make sluice-census BRANCH=campaign/the-ceiling REF=$(git rev-parse HEAD)
```

- [ ] **Step 7: G6 — HARD STOP.** Present the post-G3 ledger digest to Nathan,
  save-format entries leading. Then `closing-a-campaign`, unchanged.

- [ ] **Step 8: Merge**

```bash
make sluice BRANCH=campaign/the-ceiling REF=$(git rev-parse HEAD)
```

---

## Self-Review

**Spec coverage.** §2/§2a → Tasks 1, 5 (read before authoring). §3.2 → Tasks
1–3. §3.3 → Task 4. §3.4 → Task 3 Step 3 and Task 7. §3.5 → Tasks 5, 7. §4 M1
→ Task 3. M2 → Tasks 1–2. M3 → Task 3 Step 3 (the occupancy table rides the
same run). M4 → Task 6. §5 → Task 8. §6 → nothing, by construction (it is the
NOT list). §7 → Task 9 Step 4.

**Placeholder scan.** Every code step carries real code. Two steps deliberately
name a *property* rather than prescribing an implementation — Task 5 Step 4
(the niche's values) and Task 6 Step 3 (the per-source arms) — because a plan
author does not know which values discriminate and the implementer will, after
reading. That is the rule, not an omission.

**Type consistency.** `CombinationRule`/`separation` (Task 1) are consumed
unchanged in Task 2. `THE_KIND` is produced by Task 5 and read from the ledger
by Tasks 6–7 rather than re-derived. `Ablation::{Full, ZeroChemosynthate}` is
introduced and used only in Task 6. `BiosphereTraits`' eight fields are named
once, in Task 5 Step 1, from the struct itself.
