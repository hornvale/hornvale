# The Generalist Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Author `human` as the sixth people — one kind, surface only — and run
the preregistered Gause probe against the coexistence stack's `BETA = 2.0`.

**Architecture:** A kind is authored in six registries and validated in a
seventh. Nothing new is built: `human` is a `Settled`, `Endotherm`, surface
generalist using machinery that has shipped since The Vacancy. The campaign's
real product is a *measurement* — whether a competitor with no refuge collapses
the stack — so the authoring tasks come first and the readout task is gated
behind a vacuity check and a mutation test.

**Tech Stack:** Rust 2024, `cargo nextest`, the workspace's three allowed
external crates (`serde`, `serde_json`, `libm`). No new dependencies.

## Global Constraints

- **Dependencies:** `serde`, `serde_json`, `libm` only. No new crates.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (`clippy.toml`
  `disallowed-types`). No wall-clock time.
- **`#![warn(missing_docs)]`** — every public item, field and variant gets a
  one-line doc comment.
- **type-audit tags:** grammar is `bare-ok(<class>)` or
  `bare-ok(<class>: <field>)`. Every primitive at a `pub` boundary needs one.
- **`cargo fmt` is the final step before every commit.** fmt-gate skips are the
  project's most common review finding.
- **Layering:** `domains/species` and `domains/language` may depend on
  `hornvale-kernel` and nothing else — never on each other. Cross-domain work
  happens in `windows/worldgen`, the composition root.
- **Registry iteration is sorted** (`ComponentStore` is BTreeMap-backed), so
  array position is cosmetic but every coverage table is **alphabetical** and
  must stay so.
- **Gate budget:** `make gate` measured 22–37 min on this Mac. Use
  `timeout: 3600000`. Never `--no-verify`.

---

### Task 1: Measure the land distribution and the pre-human baseline

The spec's D3 refuses to author niche values from taste: a quantile from the
wrong population carries the authority of evidence. This task produces the
numbers D3 needs **and** the pre-human baseline the §4 readout compares
against. No production code changes.

**Files:**
- Create: `windows/worldgen/tests/generalist_baseline.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::{build_world, WorldComponents, climate_of,
  terrain_of, sky_of, niche_per_species_k}`, `hornvale_demography::{BETA,
  FLOOR}` — the exact idiom `non_void_roster.rs` uses.
- Produces: printed quantiles and per-people mean fits, pasted into Task 2's
  doc comment. No API.

- [ ] **Step 1: Write the measurement test**

```rust
//! The Generalist, Task 1: the land distribution human's ConditionNiche is
//! authored against, and the pre-human per-people fit baseline the campaign's
//! preregistered readout compares to.
//!
//! Ignored: builds 30 worlds. Reason token `heavy:` puts it in the heavy tier
//! (cli/tests/heavy_tier.rs), not the commit gate.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{KindId, Seed};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, build_world, climate_of,
    niche_per_species_k, sky_of, terrain_of,
};

const SEEDS: std::ops::RangeInclusive<u64> = 1..=30;

#[test]
#[ignore = "heavy: builds 30 worlds to measure the settleable-land distribution"]
fn report_land_distribution_and_pre_human_fits() {
    let mut elevations: Vec<f64> = Vec::new();
    let mut per_people: std::collections::BTreeMap<&'static str, Vec<f64>> =
        std::collections::BTreeMap::new();

    for seed in SEEDS {
        let (elev, fits) = measure_one(Seed::new(seed));
        elevations.extend(elev);
        for (kind, vals) in fits {
            per_people.entry(kind).or_default().extend(vals);
        }
    }

    elevations.sort_by(f64::total_cmp);
    for p in [15u32, 22, 49, 79, 95] {
        let idx = (elevations.len() * p as usize) / 100;
        println!("elevation p{p} = {:.1} m", elevations[idx]);
    }
    for (kind, vals) in &per_people {
        let mean = vals.iter().sum::<f64>() / vals.len() as f64;
        println!("pre-human mean fit {kind} = {mean:.4}");
    }

    // Guard assertions (pre-flight ruling, 2026-08-03). This is a measurement
    // harness, not a hypothesis test — H1/H2/H3 are REPORTED in Task 6, never
    // asserted, because H3 firing is the campaign's most valuable finding and
    // must not present as a red build. But a harness that silently measures
    // nothing looks identical to one that works, so it guards its own inputs.
    assert!(!elevations.is_empty(), "no settleable land sampled");
    assert!(
        elevations.iter().all(|e| e.is_finite()),
        "non-finite elevation in the sample"
    );
    assert_eq!(
        per_people.len(),
        5,
        "all five pre-human peoples must be measured; got {:?}",
        per_people.keys().collect::<Vec<_>>()
    );
}
```

**Note the count is 5, not 6:** this test runs *before* Task 2 adds human. Task 6
raises it to 6 when it extends this file, and that bump is the cheapest possible
proof that human actually entered the packer.

`measure_one` is written in Step 3 — copy the world-building preamble from
`windows/worldgen/tests/non_void_roster.rs` verbatim rather than inventing one;
it is the sanctioned test-fixture posture under decision 0092.

- [ ] **Step 2: Run it to confirm it compiles and is skipped by the gate**

Run: `cargo nextest run -p hornvale-worldgen --test generalist_baseline`
Expected: `0 tests run, 1 skipped` — the `#[ignore]` holds.

- [ ] **Step 3: Implement `measure_one` and run for real**

Run: `cargo nextest run -p hornvale-worldgen --test generalist_baseline --run-ignored all --no-capture 2>&1 | tee /tmp/hv-baseline.txt`
Expected: PASS, with the quantile and mean-fit lines printed.

**Do not re-run to grep a second line.** The output is in `/tmp/hv-baseline.txt`;
grep the file. This run builds 30 worlds and a surprise must never cost a re-run.

- [ ] **Step 4: Verify the heavy-tier token is accepted**

Run: `cargo nextest run -p hornvale --test heavy_tier`
Expected: PASS — the `heavy:` reason token is well-formed.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/generalist_baseline.rs
git commit -m "test(generalist): measure the land distribution and pre-human fit baseline"
```

---

### Task 2: The biosphere row, the niche, the family, the concept

**Files:**
- Modify: `domains/species/src/lib.rs` — `biosphere_registry`, `family_of`,
  `KIND_CONCEPTS`, and a new `human_condition_niche()` beside the others
- Modify: `domains/species/tests/coverage.rs` — the metabolic and social-form
  tables

**Interfaces:**
- Consumes: Task 1's measured quantiles.
- Produces: `KindId("human")` present in `biosphere_registry()`,
  `family_of()`, and `KIND_CONCEPTS`.

- [ ] **Step 1: Write the failing coverage-table updates**

In `domains/species/tests/coverage.rs`, `metabolic_class_coverage_matches_the_table`,
insert `"human"` into the `Endotherm` list **alphabetically, between
`"hobgoblin"` and `"killer-whale"`**:

```rust
                "gnoll",
                "goblin",
                "hobgoblin",
                "human",
                "killer-whale",
```

In `social_form_coverage_matches_the_table`, the `Settled` row becomes:

```rust
        (
            SocialForm::Settled,
            Rung::Witnessed,
            &["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"],
        ),
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo nextest run -p hornvale-species --test coverage`
Expected: FAIL — both tables mismatch, because `human` is not yet in the
registry. This is the point: the tables are the gate that a new kind is
registered everywhere.

- [ ] **Step 3: Add `human_condition_niche()`**

Place it beside the other `*_condition_niche` helpers. **Substitute Task 1's
measured values for the bracketed numbers** — do not ship the brackets.

```rust
/// Human condition niche: the roster's first true GENERALIST — the widest,
/// least-devoted curves on temperature and moisture of any kind, with only a
/// mild low-to-mid elevation lean.
///
/// Authored as a deliberate contrast to goblin, and the contrast is
/// **devotion, not optimum**. Goblin is a warm-*marginal* generalist: wide,
/// but with a real temperature lean (devotion 0.40 on its widest axis). Human
/// is flatter still, because the Gause prediction this kind exists to test
/// requires a competitor with **no refuge** — a species that does not
/// out-compete kobold on a mountain or bugbear in a rainforest, and holds no
/// stronghold of its own.
///
/// Frame: elevation is metres above the world's sea level (see
/// [`ConditionNiche`]). Measured over seeds 1..=30 at the census mesh level,
/// filtered to settleable land, by
/// `windows/worldgen/tests/generalist_baseline.rs`: the land median is
/// [MEDIAN] m (p49) and the optimum is centred there, so a wide low-devotion
/// curve reads as genuine indifference rather than a quiet lowland lean —
/// the same correction The Tumult's re-datum applied to goblin.
fn human_condition_niche() -> ConditionNiche {
    ConditionNiche {
        temperature: ConditionResponse {
            optimum: 14.0,
            width: 22.0,
            devotion: 0.20,
        },
        moisture: ConditionResponse {
            optimum: 0.50,
            width: 0.70,
            devotion: 0.20,
        },
        insolation: ConditionResponse {
            optimum: 0.14,
            width: 0.30,
            devotion: 0.25,
        },
        elevation: ConditionResponse {
            optimum: 1200.0,
            width: 2000.0,
            devotion: 0.30,
        },
    }
}
```

- [ ] **Step 4: Add the biosphere row**

Append to `biosphere_registry()`'s array:

```rust
        // The Generalist (C2-0): the sixth people, and the roster's first
        // competitor with no refuge. Mass is 5E canon for a Medium humanoid.
        // The trophic split is deliberately close to goblin's 0.50/0.50 —
        // humans are not trophically novel, and the generalism this kind
        // exists to test lives on the CONDITION axes, not the resource axes.
        (
            KindId("human"),
            BiosphereTraits {
                mass: Mass::new(70.0).unwrap(),
                metabolic_class: MetabolicClass::Endotherm,
                niche: ResourceVector::new(&[(PLANT_FORAGE, 0.55), (ANIMAL_PREY, 0.45)]).unwrap(),
                condition_niche: human_condition_niche(),
                potency: 0.0,
                social_form: SocialForm::Settled,
            },
        ),
```

- [ ] **Step 5: Add the family and the concept**

In `family_of()`:

```rust
        // The Generalist (C2-0): a singleton family, following kobold's and
        // gnoll's shape — `family_proto` in `hornvale_language` carries no
        // "human" entry, because `check_integrity` requires a proto only for
        // a label held by >= 2 kinds. The dwarf and elf families of C2c/C2d
        // will be the roster's first new multi-member families.
        (KindId("human"), "human"),
```

In `KIND_CONCEPTS`:

```rust
    ("human-kind", "a human"),
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `cargo nextest run -p hornvale-species`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add domains/species/src/lib.rs domains/species/tests/coverage.rs
git commit -m "feat(species): author human's biosphere row, niche, family and concept"
```

---

### Task 3: The mind, society and perception vectors

**Files:**
- Modify: `domains/species/src/lib.rs` — `psyche_registry`,
  `society_registry`, `perception_registry`
- Modify: `domains/species/tests/coverage.rs` — status-basis and
  activity-cycle tables
- Modify: `windows/worldgen/tests/dissolve_equivalence.rs:28` — the pinned
  psyche count

**Interfaces:**
- Consumes: `KindId("human")` from Task 2.
- Produces: human present in all three vector registries, satisfying
  `speech ⊆ perception ⊆ mind`.

- [ ] **Step 1: Write the failing table and count updates**

`status_basis_coverage_matches_the_table` — `Knowledge` gains a second witness,
and its comment must say so rather than leaving kobold's singleton framing:

```rust
        // The Generalist (C2-0) gives Knowledge its SECOND witness. Human
        // standing rests on craft and lore rather than dominance, which is
        // also what distinguishes the sixth people from the Rank-heavy
        // goblinoids.
        (
            StatusBasis::Knowledge,
            Rung::Witnessed,
            &["human", "kobold"],
        ),
```

`activity_cycle_coverage_matches_the_table` — `Diurnal`:

```rust
            &["goblin", "hobgoblin", "human", "red-dragon"],
```

`windows/worldgen/tests/dissolve_equivalence.rs:28`:

```rust
    assert_eq!(wc.psyche.len(), 9, "six peoples + three minded dragons");
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo nextest run -p hornvale-species --test coverage && cargo nextest run -p hornvale-worldgen --test dissolve_equivalence`
Expected: FAIL on both — human is in no vector registry yet.

**Re-verify this count by RUNNING the test, never by reading it.** A pinned
registry count is a known parallel-campaign merge hazard; if main was absorbed
since this plan was written, 9 may be wrong.

- [ ] **Step 3: Add the three vector rows**

`psyche_registry()`:

```rust
        // The Generalist (C2-0). `threat_response` sits AT the manikin by
        // authorship, not by default: humans genuinely both flee and stand,
        // and The Manikin moved the model to the rung where a kind may
        // coincide with the reference vector. Stated explicitly because a
        // people welded to the identity element is the bug that campaign
        // removed.
        (
            KindId("human"),
            MindVector {
                threat_response: 0.5,
                deliberation_latency: 0.6,
                time_horizon: 0.75,
            },
        ),
```

`society_registry()`:

```rust
        // The Generalist (C2-0). `in_group_radius` 0.8 is the widest in the
        // roster, above gnoll's 0.7: an expansive "us" is the social twin of
        // a broad niche, and is what a no-refuge generalist looks like from
        // the inside.
        (
            KindId("human"),
            SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Knowledge,
                in_group_radius: 0.8,
            },
        ),
```

`perception_registry()`:

```rust
        // The Generalist (C2-0). Night vision sits BELOW the manikin, and
        // below every other people (goblin 0.5 .. kobold 0.9) — the call The
        // Manikin identified and deferred to this campaign: human scotopic
        // vision is genuinely poor, so authoring it at 0.5 would have made
        // "typical" mean "weak" and silently rescaled kobold's 0.9.
        //
        // 0.15 rather than 0.25 is deliberate and visible. `pack_depths` is a
        // step function, `hue = 2 + ((1 - night_vision) * 3).round()`: 0.25
        // yields depth 4, TIED with goblin, while <= 0.166 yields depth 5 and
        // makes human the only kind at the ladder's deepest rung. The hue
        // ladder is Berlin & Kay's, derived from human languages; a model
        // whose colour hierarchy is human-derived and then denies humans its
        // deepest rung is incoherent. Luminance is 1 either way — the shallow
        // dark-vocabulary is the cost side of the same trade.
        (
            KindId("human"),
            PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.15,
                sky_attention: 0.65,
            },
        ),
```

- [ ] **Step 4: Run to verify they pass**

Run: `cargo nextest run -p hornvale-species && cargo nextest run -p hornvale-worldgen --test dissolve_equivalence`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/species/src/lib.rs domains/species/tests/coverage.rs windows/worldgen/tests/dissolve_equivalence.rs
git commit -m "feat(species): author human's mind, society and perception vectors"
```

---

### Task 4: The language rows, and the depth-5 hue witness

**Files:**
- Modify: `domains/language/src/lib.rs` — `articulation_registry`,
  `lexicon_registry`
- Modify: `windows/worldgen/tests/exposure.rs` — the `pack_depths` witness

**Interfaces:**
- Consumes: `KindId("human")`, `night_vision = 0.15` from Task 3.
- Produces: human satisfies `speech ⊆ perception ⊆ mind`; `pack_depths`
  returns `hue: 5, luminance: 1` for human.

- [ ] **Step 1: Write the failing hue-depth test**

In `windows/worldgen/tests/exposure.rs`:

```rust
#[test]
fn human_is_the_hue_ladders_deepest_witness() {
    let wc = WorldComponents::assemble().unwrap();
    let p = *wc.perception.get(&KindId("human")).unwrap();
    let d = pack_depths(&p);
    assert_eq!(d.hue, 5, "human's poor night vision buys the deepest hue ladder");
    assert_eq!(d.luminance, 1, "and the shallowest luminance ladder");
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo nextest run -p hornvale-worldgen --test exposure human_is_the_hue`
Expected: FAIL — human has no articulation/lexicon row, so
`WorldComponents::assemble` rejects it on `speech ⊆ perception`.

- [ ] **Step 3: Add the articulation row**

```rust
            // The Generalist (C2-0). Humans are authored at the envelope's
            // neutral settings, and this is the ONE vector family where that
            // is an argument rather than a default: the phonology envelope is
            // built on IPA, a human-calibrated inventory, so a human anchor
            // here is better founded than any other kind's (The Manikin §2).
            //
            // These values coincide with goblin's, which are legacy — goblin
            // sits at 0.5 because it was the baseline, not because anyone
            // decided goblins sound unremarkable. The collision is a known
            // artifact of a deferred campaign (goblin characterization) and
            // resolves when goblin moves, not when human does.
            (
                KindId("human"),
                ArticulationVector {
                    labiality: 0.5,
                    vowel_space: 0.5,
                    voicing: 0.5,
                    sibilance: 0.5,
                    voice_loudness: 0.5,
                    tonality: 0.0,
                    exotic: ExoticManner::None,
                },
            ),
```

- [ ] **Step 4: Add the lexicon row**

```rust
            // The Generalist (C2-0): a settled agricultural people's rungs.
            (
                KindId("human"),
                Lexicon {
                    noun: "town",
                    worker_override: Some("farmer"),
                    warrior: "guard",
                    artisan: "wright",
                    shaman: "priest",
                    top: "steward",
                },
            ),
```

- [ ] **Step 5: Run to verify it passes**

Run: `cargo nextest run -p hornvale-language && cargo nextest run -p hornvale-worldgen --test exposure`
Expected: PASS, including the new hue-depth witness.

- [ ] **Step 6: Confirm the singleton-family rule still holds**

Run: `cargo nextest run -p hornvale-worldgen 2>&1 | tee /tmp/hv-t4.txt`
Expected: PASS. `check_integrity` requires a `family_proto` only for a label
held by ≥ 2 kinds; `"human"` has one member, so no proto is owed. If this
fails with a missing-proto error, the singleton rule has changed and the plan's
assumption is wrong — stop and report rather than adding a proto.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add domains/language/src/lib.rs windows/worldgen/tests/exposure.rs
git commit -m "feat(language): author human's articulation and lexicon; depth-5 hue witness"
```

---

### Task 5: The vacuity check and the mutation test

The program's shared acceptance criterion. If human's niche is goblin's
recentred, the campaign has added a synonym and Task 6's readout is
meaningless. A green suite proves the code ran; only the mutation proves the
axis is visible.

**Files:**
- Create: `windows/worldgen/tests/generalist_distinctness.rs`

**Interfaces:**
- Consumes: `human_condition_niche()` via `biosphere_registry()`.
- Produces: nothing; this task is a gate.

- [ ] **Step 1: Write the distinctness test**

```rust
//! The Generalist, Task 5: human must be measurably NOT goblin.
//!
//! The vacuity check the spec §4 gates its readout behind. Two generalists
//! whose per-cell fit vectors are a monotone rescaling of one another are one
//! generalist with two names, and every H1-H3 result would be an artifact.
#![allow(clippy::disallowed_methods)]

/// Spearman rank correlation above which two kinds' per-cell fits are judged
/// the same shape. 0.98 is deliberately strict: these are two GENERALISTS, so
/// a high correlation is expected and only a near-perfect one is damning.
const RANK_CORRELATION_CEILING: f64 = 0.98;

#[test]
#[ignore = "heavy: builds 30 worlds to compare human and goblin fit shapes"]
fn human_is_not_goblin_recentred() {
    let rho = fit_rank_correlation(KindId("human"), KindId("goblin"));
    assert!(
        rho < RANK_CORRELATION_CEILING,
        "human's fit shape is a rescaling of goblin's (rho = {rho:.4}); \
         the campaign has authored a synonym and the Gause readout is vacuous"
    );
}
```

`fit_rank_correlation` reuses Task 1's world-building preamble.

- [ ] **Step 2: Run it and confirm it PASSES**

Run: `cargo nextest run -p hornvale-worldgen --test generalist_distinctness --run-ignored all --no-capture 2>&1 | tee /tmp/hv-vacuity.txt`
Expected: PASS with `rho` printed.

If it FAILS, **do not loosen the ceiling.** Re-author `human_condition_niche()`
in Task 2 to be genuinely flatter and re-run. The constant exists to be a
gate, and moving a gate to admit the thing it was built to reject is the
failure mode this campaign is meant to model good behaviour for.

- [ ] **Step 3: Write the mutation test**

Prove the readout can tell the two apart — a test that goes RED when human's
niche is replaced by goblin's:

```rust
#[test]
#[ignore = "heavy: the mutation proof — builds 30 worlds twice"]
fn substituting_goblins_niche_for_humans_is_detected() {
    let real = fit_rank_correlation(KindId("human"), KindId("goblin"));
    let mutated = fit_rank_correlation_with_niche_override(
        KindId("human"),
        goblin_niche_from_registry(),
        KindId("goblin"),
    );
    assert!(real < RANK_CORRELATION_CEILING);
    assert!(
        mutated >= RANK_CORRELATION_CEILING,
        "the mutation was NOT detected (rho = {mutated:.4}): this test cannot \
         distinguish human from goblin, so the vacuity check above proves nothing"
    );
}
```

- [ ] **Step 4: Run and confirm BOTH assertions hold**

Run: `cargo nextest run -p hornvale-worldgen --test generalist_distinctness --run-ignored all --no-capture 2>&1 | tee /tmp/hv-mutation.txt`
Expected: PASS. The second assertion is the one that matters: it proves the
instrument would have caught a synonym.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/generalist_distinctness.rs
git commit -m "test(generalist): the vacuity check and its mutation proof"
```

---

### Task 6: Run the preregistered readout

**Files:**
- Modify: `windows/worldgen/tests/generalist_baseline.rs` — add the
  post-human readout

**Interfaces:**
- Consumes: everything above. Task 5 must be GREEN before this task's output
  is believed.

- [ ] **Step 1: Add the H1/H2/H3 readout**

Report, over seeds 1..=30: human's mean per-cell share; the human–goblin Pianka
overlap and share correlation (H1); and for each of kobold's highland stronghold
(settleable land ≥ 3000 m), bugbear's warm wet lowland (≤ 500 m), hobgoblin's
and gnoll's ground — which kind is best-fit (H2).

**H1, H2 and H3 are REPORTED, never asserted** (pre-flight ruling, 2026-08-03).
Encoding a preregistered prediction as a build failure creates direct pressure
to retune the niche until the suite goes green, which §5 of the spec forbids.
The guard assertions stay and the peoples count rises from 5 to 6 — that bump is
the proof human actually entered the packer:

```rust
    assert_eq!(
        per_people.len(),
        6,
        "human must be in the packer; got {:?}",
        per_people.keys().collect::<Vec<_>>()
    );
    assert!(
        per_people.contains_key("human"),
        "the readout measured every people EXCEPT the one this campaign added"
    );
```

- [ ] **Step 2: Run it once, capture everything**

Run: `cargo nextest run -p hornvale-worldgen --test generalist_baseline --run-ignored all --no-capture 2>&1 | tee /tmp/hv-readout.txt`
Expected: PASS. Read the numbers from the file.

- [ ] **Step 3: Record the result, whichever way it came out**

Write the outcome into the chronicle draft **before** interpreting it. If H2
fails — human displaces a specialist from its stronghold — that is **H3, the
preregistered falsification**, and it is the campaign's headline finding about
`BETA = 2.0`.

**Do not retune `human_condition_niche()` to rescue H2.** The spec's §5 makes
changing `BETA` a non-goal for the same reason. A retune after unblinding is a
separate, argued decision, and the argument gets committed before the retune.

- [ ] **Step 4: Commit**

```bash
cargo fmt
git add windows/worldgen/tests/generalist_baseline.rs
git commit -m "test(generalist): the preregistered Gause readout"
```

---

### Task 7: Artifacts, epoch, book, and close

**Files:**
- Modify: generated artifacts under `book/src/{gallery,reference,laboratory}/`,
  `docs/audits/`
- Modify: `book/src/domains/species.md` and the chapters naming the peoples count
- Create: `book/src/chronicle/the-generalist.md`,
  `docs/retrospectives/the-generalist.md`
- Modify: `book/src/frontier/idea-registry.md`

- [ ] **Step 1: Regenerate and inspect the drift**

Run: `make rebaseline 2>&1 | tee /tmp/hv-rebaseline.txt`
Then: `git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/ docs/audits/`

`docs/audits/` holds the type-audit report and drifts on any pub-boundary
change. It is the commonly missed path.

- [ ] **Step 2: Decide the epoch on evidence, not expectation**

Adding a settling people changes the roster the packer iterates, so existing
worlds' settlement placement almost certainly moves — but decision 0084 says an
epoch is declared **only when a derivation moved**. The `git diff` from Step 1
is that evidence. If seed-42 artifacts moved, declare and stamp per 0089. If
they did not, say so in the chronicle and declare nothing.

- [ ] **Step 3: Refresh BOTH census fixtures**

Refreshing one leaves a suite that looks green for the wrong reason.
*[Figure corrected by The Delvers, 2026-08-07: this read "31 rows in
`the-census`, 3 in `census-of-the-meeting`". A new settling people refreshes
`the-census` wholesale — 1000 of 1000 rows — and a new metric column rewrites
every row of both fixtures. See the peoples-program metaplan §5.]*

**Census regen is a carve-out.** Ask Nathan for explicit authorization, then
run `bash scripts/census-run.sh` on lefford — never
`HV_CENSUS=1 regenerate-artifacts.sh`, which does not serialize the box or
ledger the run.

- [ ] **Step 4: Book, sweeping the stale counts**

Update `book/src/domains/species.md`'s roster and dimension tables. Then grep
for `four peoples` and `five peoples` across `book/src/` **and**
`domains/*/src/` — several doc comments still say "the four peoples", predating
gnoll. A count is scoped to the paragraph that derived it; fix each in place
rather than assuming one number is right everywhere. Chronicle entries are
excluded — they record what past campaigns did.

Write `book/src/chronicle/the-generalist.md`. **The book may not carry registry
IDs** — `docs_consistency` permits them only in the Frontier part, and it also
bans a set of process words. Name the concept ("a body that is nobody", "the
roster grid"), never `BIO-three-probes` or `PSY-2`.

- [ ] **Step 5: Retrospective and registry bookkeeping**

`docs/retrospectives/the-generalist.md` per decision 0020 — process lessons,
not product. Then the metaplan §9 rows, at minimum the `BIO-three-probes`
correction and the new `BIO-elf-radiation` row.

- [ ] **Step 6: The full gate**

Run: `make gate 2>&1 | tee /tmp/hv-gate.txt; echo "EXIT=$?"`
Expected: `EXIT=0`.

**Check the exit code explicitly.** A wrapper's exit code has masked a red gate
in this repo before; "it printed a lot and finished" is not a pass.

Then the checks `make gate` never runs: `make census-check`, and `shellcheck`
on any changed script.

- [ ] **Step 7: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(generalist): chronicle, retrospective, artifacts and registry bookkeeping"
```

---

## Self-review

**Spec coverage.** D1 → Task 2 (singleton family) and Task 4 Step 6. D2 →
Task 2 Step 4. D3 → Tasks 1 and 2 Step 3, gated by Task 5. D4 → Task 3 Step 3
and Task 4's hue witness. D5 → Task 3. D6 → Task 4 Step 3. §4's four
predictions → Tasks 5 and 6. Every §7 DoD checkbox maps to a step in Tasks 2–7.

**Placeholders.** One remains and is deliberate: `[MEDIAN]` in Task 2's doc
comment, which Task 1 measures. It is called out in the step text as a
substitution, because inventing the number here is exactly the failure D3
exists to prevent.

**Type consistency.** `human_condition_niche()`, `KindId("human")`,
`RANK_CORRELATION_CEILING`, `fit_rank_correlation` are used with the same names
and signatures wherever they appear.

**One gap found and closed during review:** Task 3 originally omitted
`dissolve_equivalence.rs:28`, whose `psyche.len() == 8` assertion is invisible
to a grep for "human" or "peoples" — it is a bare integer with the roster
count in a *string*. It is now Step 1 of that task, with an instruction to
re-verify by running rather than reading.
