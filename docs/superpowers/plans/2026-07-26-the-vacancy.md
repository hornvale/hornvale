# The Vacancy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fill the world model's uninhabited declared states — ten empty marine biomes, three unoccupied land climate regions, four dark trait combinations, and one unwitnessed status basis — by opening the sea with a new resource axis and authoring ~13 fauna plus the gnoll as a fifth people, behind an instrument that makes any remaining vacancy visible.

**Architecture:** Five stages, one variable each. Stages 1–2 are provably drift-free (the instrument, then the marine axis, which is byte-identical because existing kinds' sparse niche vectors never mention the new axis id). Stage 3 authors fauna, stage 4 the gnoll; both drift committed genesis behaviour and re-pin always-run goldens in the drifting commit. Stage 5 closes with a single census regen.

**Tech Stack:** Rust 2024, workspace crates only. `serde`/`serde_json` are the only permitted dependencies (`cli/tests/architecture.rs` enforces this). Tests via `cargo nextest`; no new dev-dependencies.

**Spec:** `docs/superpowers/specs/2026-07-26-the-vacancy-design.md`

**Task count:** 11. The gnoll's body and mind are one task (Task 9), not two: a `Settled` kind without its peopled component set fails `check_integrity`, so splitting them would mean committing a state that fails existing tests — which the project's quality gate forbids outright.

## Global Constraints

- **No `HashMap`/`HashSet`** anywhere — `BTreeMap`/`BTreeSet`/`Vec` only. Enforced by `clippy.toml` `disallowed-types`.
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Float sorting uses `total_cmp`** with a deterministic tie-break.
- **No new crates or dependencies.** Randomness comes from the kernel's `Seed`/`Stream`; there is none in this campaign — species are authored, and this campaign **draws nothing and adds no stream label**.
- **Every crate sets `#![warn(missing_docs)]`** — every `pub` item, field, and variant needs a one-line doc comment.
- **Every primitive at a `pub` boundary needs a `type-audit:` verdict tag** on the item, in the form `bare-ok(<class>)`, `bare-ok(<class>: <field-or-return>)`, `waiver(<reason>)`, or `pending(wave-N)`. Note the grammar: the class comes first and the field name after a colon — `bare-ok(ratio: potency)`, never `bare-ok(potency: ratio)`.
- **`cargo fmt` is the final step before every commit.** Skipped fmt is the project's most common review finding.
- **The commit gate is `make gate`** (~4 min). Iterate cost-ordered: fmt + clippy first, then scope tests to the changed crate, and run the full gate once at the end. Capture output once and grep the file — never re-run a suite to read a second line.
- **Masses and `potency` come from the 5E Monster Manual, verified at authoring time.** `potency` is CR/30 and is `0.0` for every mundane beast. Real-animal masses are not mixed in (spec §5).
- **Condition-niche optima are authored against Task 3's committed occupancy readout, on named percentiles** — never invented. This is the BIO-39 discipline (spec §5).

---

## File Structure

**Created:**

- `domains/species/tests/coverage.rs` — the coverage table: every declared state, its intended rung, its witnesses.
- `windows/worldgen/tests/non_void_roster.rs` — the hard assertion that no kind is a ghost.
- `windows/worldgen/tests/occupancy_readout.rs` — generates and drift-checks the committed occupancy measurement.
- `windows/worldgen/tests/fixtures/occupancy.csv` — the committed readout (the artifact stages 3–4 author optima against).

**Modified:**

- `kernel/src/ecology.rs` — the `MARINE_FORAGE` axis and the basis slice.
- `windows/worldgen/src/lib.rs` — the marine supply field and its wiring into `niche_per_species_k`'s `per_axis` array.
- `domains/species/src/lib.rs` — the `Autotroph` doc correction, then ~13 fauna rows and the gnoll across `biosphere_registry`, `family_of`, `psyche_registry`, `society_registry`, `perception_registry`.
- `domains/language/src/lib.rs` — the gnoll's articulation and lexicon rows.
- `domains/species/tests/social_form.rs` — its expected table gains every new kind.

---

# Stage 1 — The instrument (no behaviour change)

### Task 1: The coverage table

**Files:**
- Create: `domains/species/tests/coverage.rs`

**Interfaces:**
- Consumes: `hornvale_species::{biosphere_registry, psyche_registry, society_registry, perception_registry, MetabolicClass, SocialForm, ActivityCycle, StatusBasis, Sociality}`; `hornvale_kernel::KindId`.
- Produces: nothing other tasks call. Later tasks **edit** this file's expected tables when they add kinds.

- [ ] **Step 1: Write the failing test**

Create `domains/species/tests/coverage.rs`. This is an expected-table test in the shape of `domains/species/tests/social_form.rs` — read that file first for the idiom.

```rust
//! The Vacancy: the coverage table. Every declared state of the species model,
//! the rung it is intended to occupy, and its witnesses.
//!
//! A state's rung is a claim about how well the model is exercised, not about
//! how good it is:
//!
//! - `Declared`  — the variant or branch exists; nothing carries it.
//! - `Witnessed` — at least one kind carries it.
//! - `Pinned`    — a test fails if it breaks.
//!
//! This table deliberately does NOT assert that every declared variant has a
//! witness. Deliberately-empty cells are legitimate creature-design
//! predictions, and an assertion forbidding them would either be false or
//! would force junk kinds into the roster to satisfy it. What it DOES assert
//! is that the intended rung matches reality — so promoting a state, or
//! letting one rot, forces a deliberate edit here.

use hornvale_kernel::KindId;
use hornvale_species::{
    ActivityCycle, MetabolicClass, SocialForm, StatusBasis, biosphere_registry,
    perception_registry, psyche_registry, society_registry,
};

/// How well a declared state is exercised by the shipped roster.
#[derive(Debug, PartialEq, Eq)]
enum Rung {
    /// The variant or branch exists; no kind carries it.
    Declared,
    /// At least one kind carries it.
    Witnessed,
}

/// The witnesses of each `MetabolicClass`, ascending by `KindId`.
fn metabolic_witnesses(class: MetabolicClass) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.metabolic_class == class)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `SocialForm`, ascending by `KindId`.
fn social_form_witnesses(form: SocialForm) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.social_form == form)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `ActivityCycle` in the perception registry.
fn activity_witnesses(cycle: ActivityCycle) -> Vec<&'static str> {
    perception_registry()
        .iter()
        .filter(|(_, p)| p.activity == cycle)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `StatusBasis` in the society registry.
fn status_basis_witnesses(basis: StatusBasis) -> Vec<&'static str> {
    society_registry()
        .iter()
        .filter(|(_, s)| s.status_basis == basis)
        .map(|(k, _)| k.0)
        .collect()
}

#[test]
fn metabolic_class_coverage_matches_the_table() {
    let expected: &[(MetabolicClass, Rung, &[&str])] = &[
        (MetabolicClass::Endotherm, Rung::Witnessed, &[
            "black-dragon", "bugbear", "giant-elk", "giant-goat", "goblin",
            "hobgoblin", "otyugh", "owlbear", "red-dragon", "white-dragon",
            "woolly-mammoth",
        ]),
        (MetabolicClass::Ectotherm, Rung::Witnessed, &["kobold", "rust-monster"]),
        // WITNESSED but NOT exercised: allometry computes Autotroph exactly as
        // Endotherm despite the class doc's surface-limited claim. See BIO-42
        // and `autotroph_is_computed_as_an_endotherm_today` in this file.
        (MetabolicClass::Autotroph, Rung::Witnessed, &["treant", "twig-blight"]),
        // The sole carrier of the `None` life-history branch.
        (MetabolicClass::Ametabolic, Rung::Witnessed, &["xorn"]),
    ];
    for (class, rung, witnesses) in expected {
        let actual = metabolic_witnesses(*class);
        assert_eq!(&actual, witnesses, "{class:?} witnesses");
        let actual_rung = if actual.is_empty() { Rung::Declared } else { Rung::Witnessed };
        assert_eq!(&actual_rung, rung, "{class:?} rung");
    }
}

#[test]
fn status_basis_coverage_matches_the_table() {
    let expected: &[(StatusBasis, Rung, &[&str])] = &[
        (StatusBasis::Rank, Rung::Witnessed, &["bugbear", "goblin", "hobgoblin"]),
        (StatusBasis::Knowledge, Rung::Witnessed, &["kobold"]),
        // DECLARED, no witness. Blocker: none — this is a productive vacancy,
        // and stage 4's gnoll is its intended first witness.
        (StatusBasis::Generosity, Rung::Declared, &[]),
    ];
    for (basis, rung, witnesses) in expected {
        let actual = status_basis_witnesses(*basis);
        assert_eq!(&actual, witnesses, "{basis:?} witnesses");
        let actual_rung = if actual.is_empty() { Rung::Declared } else { Rung::Witnessed };
        assert_eq!(&actual_rung, rung, "{basis:?} rung");
    }
}

#[test]
fn activity_cycle_coverage_matches_the_table() {
    let expected: &[(ActivityCycle, Rung, &[&str])] = &[
        (ActivityCycle::Diurnal, Rung::Witnessed, &["goblin", "hobgoblin", "red-dragon"]),
        (ActivityCycle::Nocturnal, Rung::Witnessed, &["black-dragon", "bugbear", "kobold"]),
        // Witnessed only by a dragon; stage 3 adds a mundane witness.
        (ActivityCycle::Crepuscular, Rung::Witnessed, &["white-dragon"]),
    ];
    for (cycle, rung, witnesses) in expected {
        let actual = activity_witnesses(*cycle);
        assert_eq!(&actual, witnesses, "{cycle:?} witnesses");
        let actual_rung = if actual.is_empty() { Rung::Declared } else { Rung::Witnessed };
        assert_eq!(&actual_rung, rung, "{cycle:?} rung");
    }
}

#[test]
fn social_form_coverage_matches_the_table() {
    let expected: &[(SocialForm, Rung, &[&str])] = &[
        (SocialForm::Sessile, Rung::Witnessed, &["treant", "twig-blight"]),
        (SocialForm::Solitary, Rung::Witnessed, &[
            "black-dragon", "otyugh", "owlbear", "red-dragon", "rust-monster",
            "white-dragon", "xorn",
        ]),
        (SocialForm::Gregarious, Rung::Witnessed, &[
            "giant-elk", "giant-goat", "woolly-mammoth",
        ]),
        (SocialForm::Settled, Rung::Witnessed, &["bugbear", "goblin", "hobgoblin", "kobold"]),
    ];
    for (form, rung, witnesses) in expected {
        let actual = social_form_witnesses(*form);
        assert_eq!(&actual, witnesses, "{form:?} witnesses");
        let actual_rung = if actual.is_empty() { Rung::Declared } else { Rung::Witnessed };
        assert_eq!(&actual_rung, rung, "{form:?} rung");
    }
}

#[test]
fn the_dark_trait_combinations_are_named() {
    // Combinations, not single variants — each is a cell the roster does not
    // occupy, recorded so the vacancy is a decision rather than an oversight.
    use hornvale_kernel::{ANIMAL_PREY, DETRITUS};

    let bio = biosphere_registry();

    // `Gregarious x ANIMAL_PREY`: every herder today is a pure forager.
    let gregarious_predators: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| {
            b.social_form == SocialForm::Gregarious && b.niche.weight(ANIMAL_PREY) > 0.0
        })
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        gregarious_predators,
        Vec::<&str>::new(),
        "Gregarious x ANIMAL_PREY is DECLARED; stage 3 witnesses it"
    );

    // `Sessile x DETRITUS`: both Sessile kinds are photosynthate autotrophs.
    let sessile_detritivores: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Sessile && b.niche.weight(DETRITUS) > 0.0)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        sessile_detritivores,
        Vec::<&str>::new(),
        "Sessile x DETRITUS is DECLARED; stage 3 witnesses it"
    );

    // A minded `Gregarious` kind — decision 0068's whole reason for existing,
    // shipped with zero instances. DELIBERATELY left dark by this campaign
    // (spec S6): the blocker is that settlement-free peoples are unaudited
    // downstream, which is its own campaign.
    let psy = psyche_registry();
    let minded_gregarious: Vec<&str> = bio
        .iter()
        .filter(|(k, b)| b.social_form == SocialForm::Gregarious && psy.contains(k))
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        minded_gregarious,
        Vec::<&str>::new(),
        "minded Gregarious stays DECLARED - deferred, not forgotten"
    );
}

#[test]
fn autotroph_is_computed_as_an_endotherm_today() {
    // A KNOWN DIVERGENCE, pinned deliberately so BIO-42's fix is a visible
    // diff rather than a silent change. `MetabolicClass::Autotroph`'s doc says
    // a phototroph's basal rate is surface/area-limited so Kleiber's 3/4 mass
    // exponent does not apply; `allometry.rs` nonetheless gives it
    // `B0_ENDOTHERM` and a pace multiplier of 1.0. This test asserts the
    // SHIPPED behaviour, not the correct one. When BIO-42 lands, this test is
    // expected to fail, and its failure is the point.
    use hornvale_kernel::Mass;
    use hornvale_species::allometry;

    let mass = Mass::new(1800.0).expect("positive mass");
    assert_eq!(
        allometry::basal_metabolic_rate_w(mass, MetabolicClass::Autotroph),
        allometry::basal_metabolic_rate_w(mass, MetabolicClass::Endotherm),
        "Autotroph BMR is identical to Endotherm today (BIO-42)"
    );
    assert_eq!(
        allometry::lifespan(mass, MetabolicClass::Autotroph),
        allometry::lifespan(mass, MetabolicClass::Endotherm),
        "Autotroph lifespan is identical to Endotherm today (BIO-42)"
    );
}
```

- [ ] **Step 2: Run the test to verify the witness lists**

Run: `cargo test -p hornvale-species --test coverage 2>&1 | tee /tmp/hv-coverage.txt`

Expected: the first run very likely FAILS on at least one witness list, because the lists above were transcribed from a reading of the registries rather than generated. That is the intended workflow — the assertion failure prints the actual list. **Correct the expected lists from the failure output; do not weaken an assertion to match.** `ComponentStore::iter()` yields ascending `KindId` order, so every list is alphabetical.

- [ ] **Step 3: Confirm the whole file passes**

Run: `cargo test -p hornvale-species --test coverage`
Expected: PASS, 6 tests.

- [ ] **Step 4: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-species --all-targets -- -D warnings
git add domains/species/tests/coverage.rs
git commit -m "test(the-vacancy): the coverage table - every declared state's rung (T1)"
```

---

### Task 2: Correct the `Autotroph` doc

**Files:**
- Modify: `domains/species/src/lib.rs` (the `MetabolicClass::Autotroph` variant doc, around line 815)

**Interfaces:**
- Consumes: nothing.
- Produces: nothing. Documentation only — no code change, no drift.

- [ ] **Step 1: Read the current doc**

Run: `sed -n '804,822p' domains/species/src/lib.rs`

The `Autotroph` variant currently reads: "Phototroph (plant-folk/fungal analogue). Energy from light; its basal rate is SURFACE/area-limited, so the §4 universal exponent does NOT apply — activating this class is its own modelling decision. Unused seam."

Two claims in it are false as of The Menagerie: the class **is** used (treant, twig-blight), and the universal exponent **is** applied to it.

- [ ] **Step 2: Replace the variant doc**

```rust
    /// Phototroph (plant-folk/fungal analogue). Energy from light.
    ///
    /// **Documented intent, not shipped behaviour.** A phototroph's basal rate
    /// is physically SURFACE/area-limited, so §4's universal ¾ mass exponent
    /// should not apply to it. It nonetheless does: [`crate::allometry`] gives
    /// this class `B0_ENDOTHERM` and a pace multiplier of 1.0, so the two
    /// shipped autotrophs (treant, twig-blight) are computed exactly as
    /// endotherms of the same mass. The class was witnessed by The Menagerie
    /// without the modelling decision ever being made, and this doc claimed
    /// "unused seam" for three campaigns after it stopped being one.
    ///
    /// Making it real needs an area-scaling exponent and an autotroph `B0`
    /// calibrated against a photosynthetic-productivity anchor — a genuine
    /// modelling call that moves both kinds' life-history and every golden
    /// they touch, tracked as BIO-42 and deliberately NOT bundled with the
    /// roster expansion that would destroy its attribution. The current
    /// divergence is pinned by
    /// `autotroph_is_computed_as_an_endotherm_today` in
    /// `tests/coverage.rs`, so the fix will present as a visible diff.
    Autotroph,
```

- [ ] **Step 3: Verify nothing else changed**

Run: `cargo test -p hornvale-species 2>&1 | tail -5`
Expected: PASS. This is a comment-only edit; any test change means the wrong lines were touched.

- [ ] **Step 4: fmt and commit**

```bash
cargo fmt
git add domains/species/src/lib.rs
git commit -m "docs(species): Autotroph's doc outlived its truth (the-vacancy T2)"
```

---

### Task 3: The occupancy readout

**Files:**
- Create: `windows/worldgen/tests/occupancy_readout.rs`
- Create: `windows/worldgen/tests/fixtures/occupancy.csv`

**Interfaces:**
- Consumes: `hornvale_worldgen::{niche_per_species_k, substrate_field}`; `hornvale_species::biosphere_registry`. Read `windows/worldgen/tests/demesne.rs` and `waterline_probe.rs` first for the established world-building idiom in this crate's tests — **use whatever helper they use to build a world at `BuildDepth::Full`, rather than inventing one.**
- Produces: `windows/worldgen/tests/fixtures/occupancy.csv` — the committed artifact Tasks 7, 8, and 9 author condition optima against.

The readout answers two questions per kind, over seeds 1..=30: **which biome does it actually occupy**, and **what fraction of settleable cells does it reach**. Percentile columns are what makes an authored optimum checkable (spec §5).

- [ ] **Step 1: Write the readout test**

Create `windows/worldgen/tests/occupancy_readout.rs`. The CSV schema, one row per (kind, biome):

```
kind,biome,cells_occupied,share_of_kind_k,mean_k,p50_k,p95_k
```

`cells_occupied` counts cells where this kind's K exceeds the viability floor; `share_of_kind_k` is this biome's fraction of the kind's total K across all cells; the K columns are quantized to 8 significant digits via `hornvale_kernel::quantize` before rendering, exactly as `render_csv` does — this is a serialization boundary, so quantize at emit and never in the computation.

Mark it heavy: 30 full worlds is minutes, not seconds, so it must not sit in the commit gate.

```rust
#[test]
#[ignore = "heavy: 30 full-depth worlds; regenerates the committed occupancy readout"]
fn occupancy_readout_is_current() {
    let rendered = render_occupancy_readout(1..=30);
    let committed = include_str!("fixtures/occupancy.csv");
    assert_eq!(
        rendered, committed,
        "occupancy readout drifted - if this is intended, rewrite the fixture \
         in the SAME commit as the change that drifted it"
    );
}
```

Write `render_occupancy_readout` as a plain function in the same file returning `String`, plus a `main`-less regeneration path: a second `#[test] #[ignore = "heavy: regenerates the fixture"]` named `regenerate_occupancy_readout` that writes the file with `std::fs::write`. Rows sort by `(kind, biome)` ascending — no float ordering in the sort, so no `total_cmp` needed there.

- [ ] **Step 2: Generate the fixture**

```bash
touch windows/worldgen/tests/fixtures/.gitkeep
cargo test -p hornvale-worldgen --test occupancy_readout regenerate -- --ignored 2>&1 | tail -20
```

Expected: writes `windows/worldgen/tests/fixtures/occupancy.csv`.

- [ ] **Step 3: Inspect the readout before trusting it**

Run: `column -s, -t < windows/worldgen/tests/fixtures/occupancy.csv | head -40`

**This is a required review step, not a formality.** Confirm by eye:
- every one of the 16 kinds appears with at least one row (a kind absent entirely is the BIO-39 ghost class, and Task 4 turns that into a hard failure);
- no kind's K lands predominantly on marine biomes (the land mask should make marine rows absent or zero — if a kind shows marine occupancy *now*, before Stage 2, the land mask has a hole and that is a finding to report before continuing);
- the kobold shows real highland occupancy (The Tumult's re-datum fixed this; if it does not, something has regressed).

Record the answer to the second bullet in the commit message.

- [ ] **Step 4: Confirm the drift check passes**

Run: `cargo test -p hornvale-worldgen --test occupancy_readout -- --ignored`
Expected: PASS, 2 tests.

- [ ] **Step 5: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/tests/occupancy_readout.rs windows/worldgen/tests/fixtures/
git commit -m "test(the-vacancy): commit the occupancy readout, the frame new optima are authored against (T3)"
```

---

### Task 4: The non-void roster test

**Files:**
- Create: `windows/worldgen/tests/non_void_roster.rs`

**Interfaces:**
- Consumes: the same world-building helper Task 3 used; `hornvale_species::biosphere_registry`; `hornvale_worldgen::niche_per_species_k`.
- Produces: the assertion every later task's new kind must satisfy. Tasks 7, 8, and 9 each run this test as their own gate.

- [ ] **Step 1: Write the test**

```rust
//! The Vacancy: no kind may be a ghost.
//!
//! A kind can be authored, load, satisfy every referential-integrity check in
//! `components.rs`, and still have K = 0 on every cell of every world — present
//! in the registry, absent from the world, with no error anywhere. That is not
//! hypothetical: the kobold's elevation optimum once sat at or above the
//! highest land on most seeds, so its documented "exclusive highland
//! stronghold" was unoccupiable and its fit ran ~25x below every other
//! people's everywhere. It shipped, and was found by hand campaigns later
//! (BIO-39).
//!
//! This test is the refusal. It is cheap, it passes today, and it fails the
//! moment a kind is authored into a niche no world can satisfy.

#[test]
fn every_kind_is_viable_somewhere() {
    // A small seed set: this is a "somewhere, ever" existence check, not a
    // distributional claim, so a handful of worlds is the right cost. The
    // per-kind DISTRIBUTION is the occupancy readout's job (Task 3).
    let seeds = [1u64, 7, 42, 99];
    let roster: Vec<&'static str> = hornvale_species::biosphere_registry()
        .iter()
        .map(|(k, _)| k.0)
        .collect();

    let mut void: Vec<&'static str> = Vec::new();
    for name in &roster {
        let viable_on_some_seed = seeds.iter().any(|seed| kind_is_viable_on(*seed, name));
        if !viable_on_some_seed {
            void.push(name);
        }
    }

    assert_eq!(
        void,
        Vec::<&'static str>::new(),
        "these kinds have no viable cell on any of {seeds:?} - they are \
         registered but absent from every world. Either the condition niche is \
         authored outside the range any world produces (the BIO-39 class: check \
         the optima against tests/fixtures/occupancy.csv percentiles), or the \
         uptake vector points at supply axes that are zero everywhere the \
         condition terms allow (an aquatic niche before a marine supply axis \
         exists)."
    );
}
```

Implement `kind_is_viable_on(seed, name) -> bool` in the same file: build the world at `BuildDepth::Full` using the same helper as Task 3, call `niche_per_species_k`, and return whether any cell's K for that kind exceeds the viability floor. Use the *same* floor definition the occupancy readout uses — extract it as a shared `const` in this file and have Task 3's file reference the same value if it is not already a published constant; two different floors would let a kind pass one test and fail the other.

- [ ] **Step 2: Run it — expect FOUR failures, and allowlist them**

Run: `cargo test -p hornvale-worldgen --test non_void_roster 2>&1 | tee /tmp/hv-nonvoid.txt`

Expected: **FAIL**, naming `black-dragon`, `owlbear`, `red-dragon`, `white-dragon`. This is correct and already known — Task 3's readout returned 12 of 16 kinds. Those four are authored as pure-`ANIMAL_PREY` obligate predators, and `ANIMAL_PREY` supply is hard-coded `0.0` in the K assembly (`windows/worldgen/src/lib.rs:910`, "Stage 2's placeholder zero"), so their K is exactly zero on every cell of every seed. They are in the registry and have never existed in any world.

Add them as an **explicit, documented allowlist** — not a weakened assertion:

```rust
/// The four kinds known to be void, and the single reason all four are.
///
/// Each is a pure-`ANIMAL_PREY` obligate predator, and `ANIMAL_PREY` supply is
/// hard-coded `0.0` in the K assembly, so their carrying capacity is exactly
/// zero everywhere. They are registered, they satisfy every integrity check,
/// and they are absent from every world ever generated.
///
/// **This allowlist is deleted in Task 6b**, which lands a real prey supply
/// field. Its deletion is the proof the fix worked — a non-void test that
/// passes while carrying an allowlist proves nothing about the kinds on it.
const KNOWN_VOID_PENDING_PREY_SUPPLY: &[&str] =
    &["black-dragon", "owlbear", "red-dragon", "white-dragon"];
```

Filter the assertion by that list, and assert its *exact* membership too, so a fifth kind going void cannot hide inside it:

```rust
assert_eq!(
    void, KNOWN_VOID_PENDING_PREY_SUPPLY,
    "the set of void kinds must be exactly the four awaiting prey supply - \
     a kind appearing here that is not on the list is a new ghost"
);
```

Run again. Expected: PASS.

- [ ] **Step 3: Mutation-verify the test actually asserts something**

A test that cannot fail is worse than no test. Temporarily add a deliberately void kind to `biosphere_registry` — copy the `xorn` row, rename it `KindId("void-probe")`, and set its `elevation` `ConditionResponse` to `optimum: 50_000.0, width: 1.0, devotion: 1.0` (far above any land):

Run: `cargo test -p hornvale-worldgen --test non_void_roster`
Expected: **FAIL**, naming `void-probe`.

Then revert the probe completely:

```bash
git checkout domains/species/src/lib.rs
cargo test -p hornvale-worldgen --test non_void_roster
```
Expected: PASS again. Confirm `git diff domains/species/src/lib.rs` is empty before continuing.

- [ ] **Step 4: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/tests/non_void_roster.rs
git commit -m "test(the-vacancy): refuse a ghost - no kind may be void everywhere (T4)"
```

- [ ] **Step 5: Stage 1 gate**

Run: `make gate 2>&1 | tee /tmp/hv-gate-s1.txt`
Expected: PASS, and **`git status` shows no modified generated artifact**. Stage 1 changes no behaviour; if any committed artifact drifted, stop and report — something in the "instrument" is not inert.

---

# Stage 2 — The marine axis (byte-identical)

### Task 5: The `MARINE_FORAGE` axis

**Files:**
- Modify: `kernel/src/ecology.rs` (after `MINERAL`, around line 83, and `v1_basis` at line 88)

**Interfaces:**
- Consumes: nothing.
- Produces: `hornvale_kernel::MARINE_FORAGE: ResourceAxis` with `id: 5`. Tasks 6 and 8 both use it.

**Why this is safe, and the one way to make it unsafe.** Axis ids are explicit `u16`s and every existing kind's `ResourceVector` is sparse, so `weight(MARINE_FORAGE)` returns exactly `0.0` for all sixteen. The four consumers of `v1_basis()` were each checked:

| consumer | effect of a trailing zero-weight axis |
|---|---|
| `demography/coexist.rs:227` — sums weights over the basis | adds `0.0` last; `x + 0.0 == x` exactly for finite non-negative `x` |
| `demography/niche.rs:75` — `dominant_axis` | **safe only because line 79 uses strict `Ordering::Greater`** — a `0.0` weight never displaces a leader, and the zero vector still resolves to `v1_basis()[0]` |
| `demography/niche.rs:214` — `total_non_detritus` | adds `0.0` last |
| `worldgen/tests/insolation_probe.rs:183` — sums uptake | adds `0.0` last |

**The axis must be APPENDED last, never inserted.** Float addition is not associative, so reordering the existing terms would shift results in the last ULP and break byte-identity even though every value is unchanged.

- [ ] **Step 1: Add the axis**

```rust
/// Marine primary production and the prey web it supports — the sea's single
/// trophic axis at this fidelity.
///
/// Deliberately conflates what the land resolves into three axes
/// (`PHOTOSYNTHATE` → `PLANT_FORAGE` → `ANIMAL_PREY`), because one axis needs
/// one calibration knob and three would need three. The consequence is real
/// and worth knowing: a reef grazer and a pelagic apex predator are
/// differentiated only by their condition-response curves, not by what they
/// eat, so marine food-chain *length* is not yet an emergent property. Splitting
/// it is BIO-44, and costs only new ids — never a reinterpretation of this one.
///
/// `Stock` rather than `Field`: what a consumer eats here is standing biomass,
/// even though its supply is derived from production.
pub const MARINE_FORAGE: ResourceAxis = ResourceAxis {
    id: 5,
    label: "marine forage",
    kind: ResourceKind::Stock,
};
```

- [ ] **Step 2: Extend the basis — appending, and say why**

```rust
/// The registered resource-axis basis, in ascending id order. The basis is
/// open — later campaigns may register further axes with higher ids — so this
/// slice is a snapshot of what's registered today, not a closed enum. The
/// name is historical (`v1` predates the sea) and is kept because renaming it
/// would churn four call sites for no behavioural gain.
///
/// **Append only.** Consumers sum `weight(axis)` across this slice in order,
/// and float addition is not associative: inserting an axis anywhere but the
/// end reorders those sums and shifts results in the last ULP. Appending a
/// zero-weight axis is exact.
pub fn v1_basis() -> &'static [ResourceAxis] {
    &[
        PHOTOSYNTHATE,
        PLANT_FORAGE,
        ANIMAL_PREY,
        DETRITUS,
        MINERAL,
        MARINE_FORAGE,
    ]
}
```

- [ ] **Step 3: Write the byte-identity test**

Add to `kernel/src/ecology.rs`'s test module:

```rust
#[test]
fn a_trailing_zero_weight_axis_does_not_perturb_a_terrestrial_niche() {
    // The stage-2 keystone: every existing kind's niche must be numerically
    // untouched by the basis extension. Both properties below are what make
    // that true, and both are checked rather than assumed.
    let terrestrial = ResourceVector::new(&[(PLANT_FORAGE, 0.65), (ANIMAL_PREY, 0.35)]).unwrap();

    // 1. The new axis contributes an exact zero.
    assert_eq!(terrestrial.weight(MARINE_FORAGE), 0.0);

    // 2. Summing over the extended basis is bit-identical to summing over the
    //    five-axis prefix — the property that keeps `coexist.rs` and
    //    `niche.rs` byte-identical.
    let over_full: f64 = v1_basis().iter().map(|a| terrestrial.weight(*a)).sum();
    let over_prefix: f64 = v1_basis()[..5].iter().map(|a| terrestrial.weight(*a)).sum();
    assert_eq!(over_full.to_bits(), over_prefix.to_bits());

    // 3. Overlap against another terrestrial niche is unchanged by the
    //    extension (Pianka gains only zero terms).
    let other = ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).unwrap();
    let overlap = terrestrial.overlap(&other);
    assert!(overlap > 0.0 && overlap <= 1.0);
}
```

- [ ] **Step 4: Run the kernel tests**

Run: `cargo test -p hornvale-kernel ecology 2>&1 | tail -10`
Expected: PASS.

- [ ] **Step 5: The real gate — nothing anywhere drifted**

Run: `make gate 2>&1 | tee /tmp/hv-gate-t5.txt`
Expected: PASS. Then:

```bash
git status --short
```

Expected: **only `kernel/src/ecology.rs` modified.** A drifted almanac, map, registry dump, or lab study means the basis extension was not inert — stop and report which artifact moved rather than re-pinning it. This is the task's exit condition; "the tests pass" is not sufficient.

- [ ] **Step 6: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add kernel/src/ecology.rs
git commit -m "feat(kernel): MARINE_FORAGE, the sea's supply axis - byte-identical for the sixteen (the-vacancy T5)"
```

---

### Task 6: The marine supply field

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (a new field function beside `detritus_supply_field` at line 755; the `per_axis` array at lines 905–911; the hoisted field construction at lines 888–890)

**Interfaces:**
- Consumes: `hornvale_kernel::MARINE_FORAGE` (Task 5); `GeneratedTerrain::{is_ocean, elevation_at, sea_level}`; `GeneratedClimate::biome_map()` → `CellMap<Biome>`; `hornvale_climate::Biome`.
- Produces: `pub fn marine_forage_supply_field(geo, terrain, climate, scale) -> CellMap<f64>`, and the wired `per_axis` entry. Task 8's marine kinds depend on both.

**The inputs are all already computed.** `substrate_field` (line 1416) shows the pattern: `climate.mean_temperature_at(cell)` gives SST on ocean cells, and `terrain.elevation_at(cell) - sea_level` is the signed height above sea level — **negative on ocean cells, so its magnitude is depth**. `climate.biome_map()` gives the marine class per cell, including `Upwelling` (documented as the high-productivity case), `CoralReef` and `KelpForest` (the shallow productive classes).

- [ ] **Step 1: Add the amplitude constant**

Place it beside `MINERAL_SUPPLY_SCALE` (line 803):

```rust
/// The `MARINE_FORAGE` supply amplitude — the campaign's single marine
/// calibration knob, following The Demesne's one-knob-per-axis precedent.
///
/// Set so a productive shallow marine cell supplies roughly what a productive
/// land cell supplies, making marine and terrestrial K comparable rather than
/// one silently dominating. Fit in Task 6 Step 4 against the measured
/// land/sea supply ratio, not chosen by taste. If one constant cannot make the
/// two comparable, that is a finding to report (spec §11) — not an invitation
/// to add a second knob.
/// type-audit: bare-ok(ratio)
const MARINE_SUPPLY_SCALE: f64 = 1.0;
```

- [ ] **Step 2: Write the supply field**

```rust
/// The `MARINE_FORAGE` supply field (The Vacancy): marine primary production
/// and the prey web it supports, **0 on every land cell** — the exact mirror
/// of the terrestrial axes' land mask (see [`DETRITUS_AMBIENT`]'s
/// terrestrial-supply frame), stated on the supply so no consumer needs a
/// per-species exemption.
///
/// Derived from what climate already computes, never drawn: the cell's marine
/// biome class sets a productivity multiplier (`Upwelling` highest — climate
/// documents it as the high-productivity class — then `CoralReef` and
/// `KelpForest`, then the sunlit `Epipelagic`, falling through the aphotic
/// classes to near-zero at `Abyssal` and `HadalTrench`), and `SeaIce` is
/// suppressed. `HydrothermalVent` is deliberately left near-zero rather than
/// productive: a real vent community is CHEMOTROPHIC, which is a metabolic
/// class the enum does not have (BIO-45), so making it productive here would
/// feed vent biomass to photosynthesis-based consumers.
/// type-audit: bare-ok(ratio: scale), bare-ok(count: return)
pub fn marine_forage_supply_field(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    scale: f64,
) -> hornvale_kernel::CellMap<f64> {
    let biome = climate.biome_map();
    hornvale_kernel::CellMap::from_fn(geo, |c| {
        if !terrain.is_ocean(c) {
            return 0.0;
        }
        let productivity = match biome.get(c) {
            hornvale_climate::Biome::Upwelling => 1.0,
            hornvale_climate::Biome::CoralReef | hornvale_climate::Biome::KelpForest => 0.85,
            hornvale_climate::Biome::Epipelagic => 0.45,
            hornvale_climate::Biome::Mesopelagic => 0.15,
            hornvale_climate::Biome::Bathypelagic => 0.05,
            hornvale_climate::Biome::Abyssal | hornvale_climate::Biome::HadalTrench => 0.02,
            // Chemotrophic in reality; not modellable as forage yet (BIO-45).
            hornvale_climate::Biome::HydrothermalVent => 0.02,
            hornvale_climate::Biome::SeaIce => 0.05,
            // Every land class: unreachable under the `is_ocean` guard above,
            // but the match must be total and a wrong default here would be a
            // silent land leak.
            _ => 0.0,
        };
        productivity * scale
    })
}
```

- [ ] **Step 3: Wire it in — appending last**

At the hoisted construction (after line 890):

```rust
    let marine = marine_forage_supply_field(geo, terrain, climate, MARINE_SUPPLY_SCALE);
```

Then extend the `per_axis` array (lines 905–911), **appending** and updating the `use` line:

```rust
                use hornvale_kernel::{
                    ANIMAL_PREY, DETRITUS, MARINE_FORAGE, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
                };
                let per_axis = [
                    (PHOTOSYNTHATE, *base_carrying.get(cell)),
                    (PLANT_FORAGE, *forage.get(cell)),
                    (MINERAL, *mineral.get(cell)),
                    (DETRITUS, *detritus.get(cell)),
                    (ANIMAL_PREY, 0.0),
                    (MARINE_FORAGE, *marine.get(cell)),
                ];
```

Do **not** reorder the existing five entries. `axis_supply` sums in slice order and float addition is not associative.

- [ ] **Step 4: Fit the amplitude, and record the measurement**

Write a throwaway probe (do not commit it) that prints, over seeds 1..=10, the median `MARINE_FORAGE` supply on productive marine cells against the median `PHOTOSYNTHATE + PLANT_FORAGE` supply on settleable land cells. Adjust `MARINE_SUPPLY_SCALE` until the two medians are within a factor of ~2, then delete the probe and put the measured before/after ratio in the commit message.

Run: `cargo test -p hornvale-worldgen marine 2>&1 | tail -20`

- [ ] **Step 5: Add the two field-level tests**

In `windows/worldgen/src/lib.rs`'s test module:

```rust
#[test]
fn marine_forage_is_zero_on_land_and_positive_in_the_shallows() {
    // The mirror of the land mask: this axis must not leak onto land, or the
    // terrestrial roster silently gains a sixth food source.
    // Build a world with the same helper the neighbouring supply-field tests
    // use, then:
    //   - assert every non-ocean cell reads exactly 0.0;
    //   - assert at least one ocean cell reads > 0.0 (otherwise the sea is
    //     open in name only, and Task 8's kinds would all be ghosts).
}

#[test]
fn the_existing_sixteen_get_no_supply_from_the_marine_axis() {
    // Every shipped kind's uptake vector must have weight 0.0 on
    // MARINE_FORAGE, so the new `per_axis` entry contributes an exact zero to
    // its dot product. This is the assertion that makes Step 6's byte-identity
    // result a property rather than a coincidence.
    for (kind, bio) in hornvale_species::biosphere_registry().iter() {
        assert_eq!(
            bio.niche.weight(hornvale_kernel::MARINE_FORAGE),
            0.0,
            "{kind:?} must not weight the marine axis before Task 8"
        );
    }
}
```

Fill both bodies following the idiom of the existing supply-field tests in that module (`substrate_field_is_finite_and_insolation_peaks_at_the_equator` at line 8984 is the nearest example).

- [ ] **Step 6: The gate — the stage's exit condition is zero drift**

Run: `make gate 2>&1 | tee /tmp/hv-gate-t6.txt`
Expected: PASS. Then:

```bash
git status --short
cargo test -p hornvale-worldgen --test occupancy_readout -- --ignored
cargo test -p hornvale-worldgen --test non_void_roster
```

Expected: only `windows/worldgen/src/lib.rs` modified; **the occupancy readout fixture unchanged** (no kind gained marine occupancy, because none weights the axis yet); non-void still passes.

If the occupancy fixture drifted, the marine field is leaking onto land or a terrestrial kind weights the new axis. Stop and report — do not re-pin the fixture.

- [ ] **Step 7: fmt, clippy, commit**

```bash
cargo fmt
cargo clippy --workspace --all-targets -- -D warnings
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): the marine forage supply field - the sea has supply, nothing eats it yet (the-vacancy T6)"
```

---

### Task 6b: The `ANIMAL_PREY` supply field

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (a new field function beside `forage_supply_field`; the `per_axis` array's `ANIMAL_PREY` entry; the hoisted field construction)
- Modify: `windows/worldgen/tests/non_void_roster.rs` (delete the allowlist)
- Modify: `windows/worldgen/tests/fixtures/occupancy.csv` (regenerate)

**Interfaces:**
- Consumes: `forage_supply_field` (already shipped) and `base_carrying`, both already hoisted in `niche_per_species_k`.
- Produces: `pub fn prey_supply_field(geo, forage) -> CellMap<f64>`, and four previously-nonexistent kinds appearing in worlds.

**Why this exists.** Task 3's instrument found that `ANIMAL_PREY` supply is hard-coded `0.0`, so the three chromatic dragons and the owlbear have K = 0 on every cell of every seed and have never existed in any world. Task 4 allowlisted them. This task makes them real and deletes the allowlist.

**The design is fixed — do not invent an alternative.** Prey supply is a **scale of the forage field by a trophic-transfer efficiency**, exactly parallel to the shipped `forage_supply_field` being `FORAGE_FRACTION = 0.5` of `base_carrying`. This is Lindeman's ~10% rule: secondary production is roughly a tenth of the primary production supporting it. It is non-circular by construction — it reads primary production, never predator or prey *populations* — and it is land-masked transitively, because forage already is. Do not attempt a population-coupled predator/prey model; that is a later campaign.

- [ ] **Step 1: Add the constant and the field**

```rust
/// Fraction of grazable forage that becomes prey biomass available to a
/// predator — Lindeman's trophic-transfer efficiency, ~10%.
///
/// The campaign's second and last calibration knob. Deliberately a single
/// constant scale of [`forage_supply_field`] rather than a population-coupled
/// predator/prey model: this reads primary production, never predator or prey
/// populations, so it cannot feed back on itself. A real bidirectional trophic
/// coupling is BIO-24's campaign, not this one.
/// type-audit: bare-ok(ratio)
const PREY_FRACTION: f64 = 0.1;

/// The `ANIMAL_PREY` supply field (The Vacancy): prey biomass as a
/// trophic-transfer fraction of grazable forage.
///
/// Replaces a hard-coded `0.0` that had kept every obligate predator in the
/// roster — the three chromatic dragons and the owlbear — out of every world
/// ever generated. Land-masked transitively (forage is already 0 on submerged
/// cells); marine predators eat `MARINE_FORAGE` instead. Pure, deterministic,
/// no RNG — a direct scale of an already-computed field.
/// type-audit: bare-ok(count: forage), bare-ok(count: return)
pub fn prey_supply_field(
    geo: &Geosphere,
    forage: &hornvale_kernel::CellMap<f64>,
) -> hornvale_kernel::CellMap<f64> {
    hornvale_kernel::CellMap::from_fn(geo, |c| forage.get(c) * PREY_FRACTION)
}
```

- [ ] **Step 2: Wire it in, replacing the placeholder in place**

Hoist it beside the others, after the `forage` binding:

```rust
    let prey = prey_supply_field(geo, &forage);
```

Then replace **only** the `ANIMAL_PREY` tuple's value in the `per_axis` array — the entry stays in its existing position, because reordering the array changes the summation order and shifts every existing kind's K:

```rust
                    (ANIMAL_PREY, *prey.get(cell)),
```

- [ ] **Step 3: Delete the allowlist**

In `windows/worldgen/tests/non_void_roster.rs`, remove `KNOWN_VOID_PENDING_PREY_SUPPLY`, its filter, and its membership assertion; restore the plain `void == []` assertion from Task 4 Step 1.

```bash
cargo test -p hornvale-worldgen --test non_void_roster
```

Expected: PASS with **no allowlist**. If any of the four is still void, the prey field is not reaching it — report which and why rather than restoring the allowlist.

- [ ] **Step 4: Confirm the four kinds actually materialize, and say by how much**

```bash
cargo test -p hornvale-worldgen --test occupancy_readout regenerate -- --ignored
cut -d, -f1 windows/worldgen/tests/fixtures/occupancy.csv | tail -n +2 | sort -u | tr '\n' ' '
```

Expected: all 16 kinds present. Report each new kind's biome and occupancy — a dragon that materializes on 3 cells worldwide is a different outcome from one on 30 000, and both are worth knowing. Note that potency raises a mighty creature's sovereignty floor, so the dragons should tolerate more marginal cells than the owlbear.

- [ ] **Step 5: Reconcile the whole test surface — this stage drifts genesis**

Four kinds appearing where there were none changes the coexistence stack, settlement outcomes, and every downstream readout.

```bash
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t6b.txt
```

Grep that one file for every failure — do not re-run the suite to read a second line. Classify each: **mechanical** (a settlement count, a population figure, a golden string) → re-pin in this commit. **Non-mechanical** (a property test, a determinism or pin-isolation contract, an architecture test) → **STOP and report**, do not update the assertion.

- [ ] **Step 6: Regenerate the always-run artifacts, in this commit**

```bash
bash scripts/regenerate-artifacts.sh
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Re-pin what drifted, here, in the drifting commit. Do **not** set `HV_CENSUS=1`.

- [ ] **Step 7: Gate, fmt, commit**

```bash
make gate 2>&1 | tee /tmp/hv-gate-t6b.txt
cargo fmt
git add -A
git commit -m "feat(worldgen): prey exists - four kinds enter the world for the first time (the-vacancy T6b)"
```

Put the four kinds' measured occupancy in the commit message.

---

# Stage 3 — The fauna

Both tasks in this stage drift committed genesis behaviour. The Menagerie's retrospective is explicit that this blast radius is under-predicted: it expected census fixtures and drifted **twelve always-run tests** across cli/lab/scene/worldgen, and reframed a named determinism contract. Budget a reconciliation pass over the whole test surface, and **STOP-and-report on any non-mechanical drift** — a changed settlement count is mechanical; a changed determinism contract or a failing property test is not.

### Task 7: Terrestrial fauna

**Files:**
- Modify: `domains/species/src/lib.rs` (`biosphere_registry`, `family_of`, and a `*_condition_niche` helper per kind)
- Modify: `domains/species/tests/social_form.rs` (the expected table)
- Modify: `domains/species/tests/coverage.rs` (the expected witness lists)

**Interfaces:**
- Consumes: `windows/worldgen/tests/fixtures/occupancy.csv` (Task 3) — the percentile frame every optimum is authored against.
- Produces: eight new `KindId`s. Task 8 adds to the same registries; Task 10 measures the result.

Eight kinds, each promoting a named cell from spec §5.1. **Source every mass and CR from the 5E Monster Manual and state the source in the doc comment**; where a candidate has no MM entry, pick the listed alternative rather than inventing a magnitude.

| cell | candidate | notes |
|---|---|---|
| hot-arid, mundane | giant scorpion (or ankheg) | third `Ectotherm`; the largest land gap |
| savanna, `Gregarious × ANIMAL_PREY` | giant hyena (or lion) | the dark combination |
| boreal, `Gregarious × ANIMAL_PREY` | dire wolf | same combination, second climate |
| hot-arid/savanna herbivore | rhinoceros (or giant boar) | prey base for the two new predators |
| tropical, `Ectotherm` apex | giant constrictor snake | — |
| `DETRITUS`, second witness | carrion crawler | — |
| `Sessile × DETRITUS` | shrieker | a decomposer that cannot move |
| `Crepuscular`, mundane | giant badger | needs a `perception_registry` row **only if minded** — it is not, so it gets none; `ActivityCycle` lives in `PerceptionVector`, which is carried by minded speaking kinds only |

**Resolve the `Crepuscular` cell before authoring.** `ActivityCycle` is a field of `PerceptionVector`, and `perception_registry` is keyed to minded *speaking* kinds (`speech ⊆ perception ⊆ mind`, The Vigil). A mundane beast therefore **cannot** carry an `ActivityCycle` under the current component layout, so "a mundane `Crepuscular` witness" is not authorable as fauna. Do not widen the perception registry to force it — that breaks The Vigil's enforced lattice. Instead: drop the giant badger, record `Crepuscular`'s single-witness state as a `DECLARED`-adjacent row in the coverage table naming *this* blocker, and report the finding. Spec §5.1 listed the cell without checking the lattice; this is the plan correcting it.

**The `shrieker` family decision.** One fungus is a singleton family and needs no `family_proto`. Only add a second fungus if you also add the `fungus` proto to `domains/language`; one fungus is the cheaper and sufficient choice for the `Sessile × DETRITUS` cell.

- [ ] **Step 1: Read the percentile frame**

Run: `column -s, -t < windows/worldgen/tests/fixtures/occupancy.csv | less`

Note, for each cell you are filling, which biome rows are sparse or empty. Every optimum you author must be justified by a percentile or biome row you can cite in its doc comment, in the style of the existing `kobold_condition_niche` doc.

- [ ] **Step 2: Author one kind, fully, and gate it**

Do these one kind at a time. For each: add the `*_condition_niche` helper with a doc comment citing the readout, add the `biosphere_registry` row, add the `family_of` row, then:

```bash
cargo test -p hornvale-species
cargo test -p hornvale-worldgen --test non_void_roster
```

Expected: PASS. A non-void failure means the optima are outside what any world produces — fix the optima against the readout, never the test.

- [ ] **Step 3: Update the two expected tables**

`domains/species/tests/social_form.rs`'s `expected` array and `domains/species/tests/coverage.rs`'s witness lists both need every new kind, alphabetically. `coverage.rs`'s `the_dark_trait_combinations_are_named` must flip: `Gregarious × ANIMAL_PREY` and `Sessile × DETRITUS` now have witnesses, so change those assertions from empty-vec to the actual names, and move each cell's rung from `Declared` to `Witnessed`.

- [ ] **Step 4: Reconcile the whole test surface**

Run: `cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t7.txt`

Then grep that one file for every failure — do not re-run to read a second line. Classify each:
- **mechanical** (a settlement count, a population figure, a golden string): re-pin **in this commit**.
- **non-mechanical** (a property test, a determinism or pin-isolation contract, an architecture test): **STOP and report.** Do not update the assertion.

- [ ] **Step 5: Regenerate the always-run artifacts, in this commit**

```bash
bash scripts/regenerate-artifacts.sh
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Re-pin what drifted here, in the drifting commit — never deferred to the close. Do **not** run the census (`HV_CENSUS=1`); that is Stage 5's single regen.

- [ ] **Step 6: Refresh the occupancy readout**

```bash
cargo test -p hornvale-worldgen --test occupancy_readout regenerate -- --ignored
column -s, -t < windows/worldgen/tests/fixtures/occupancy.csv | grep -E "hyena|wolf|scorpion|shrieker"
```

Confirm each new kind occupies the biome it was authored for. A kind that is viable (Task 4 passes) but occupies the *wrong* biome is a real finding — report it rather than quietly accepting it.

- [ ] **Step 7: Gate, fmt, commit**

```bash
make gate 2>&1 | tee /tmp/hv-gate-t7.txt
cargo fmt
git add -A
git commit -m "feat(species): seven terrestrial fauna filling hot-arid, savanna and boreal (the-vacancy T7)"
```

State in the commit message which cells were promoted, and that the `Crepuscular` cell was found unauthorable as fauna.

---

### Task 8: Marine and amphibious fauna

**Files:**
- Modify: `domains/species/src/lib.rs`, `domains/species/tests/social_form.rs`, `domains/species/tests/coverage.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::MARINE_FORAGE` (Task 5), the supply field (Task 6), the readout (Task 3).
- Produces: five new `KindId`s including the amphibious proof case.

| cell | candidate | uptake |
|---|---|---|
| `CoralReef` | reef shark | `MARINE_FORAGE` 1.0 |
| `KelpForest` | giant octopus | `MARINE_FORAGE` 1.0 |
| `Epipelagic`, `Gregarious × ANIMAL_PREY` | killer whale | `MARINE_FORAGE` 1.0, `Gregarious` |
| `Bathypelagic`/`Abyssal` | giant squid | `MARINE_FORAGE` 1.0 |
| **amphibious** | giant crocodile | `MARINE_FORAGE` ~0.4 **and** `ANIMAL_PREY`/`PLANT_FORAGE` — the multi-medium vector |

**The elevation axis is the one to get right.** A marine kind's `elevation` optimum is **negative** — metres *below* sea level, since `substrate_field` computes `elevation_at(cell) - sea_level`. The rust monster's `-500.0` is the existing precedent for a negative optimum. Read the readout's depth distribution before choosing; an optimum deeper than the world's ocean floor is the BIO-39 failure in its marine form, and Task 4 will catch it.

**The amphibious kind is the design's proof (spec §3.4).** If the giant crocodile needs *any* new branch, special case, or exemption to work, the axis-extension design is wrong — report that as a finding rather than adding the branch.

- [ ] **Step 1: Author the four marine kinds, one at a time, gating each**

Same loop as Task 7 Step 2. After each, run:

```bash
cargo test -p hornvale-worldgen --test non_void_roster
```

A marine kind failing non-void means either the elevation optimum is below the seafloor, or the marine supply field is zero where its condition terms allow — check `marine_forage_supply_field`'s productivity ladder against the biome the kind was authored for.

- [ ] **Step 2: Author the amphibious kind and verify it needed no special case**

```bash
git diff --stat
```

Expected: **only `domains/species/src/lib.rs` and the two test files.** Any change to `windows/worldgen/src/lib.rs` or `kernel/` to make the crocodile work is the finding described above — report it.

- [ ] **Step 3: Update the expected tables**

Both `social_form.rs` and `coverage.rs`, alphabetically, as in Task 7 Step 3. `Gregarious × ANIMAL_PREY` gains a marine witness.

- [ ] **Step 4: Reconcile, regenerate, refresh the readout**

Repeat Task 7 Steps 4–6. In the refreshed readout, confirm at least **four of the ten marine biomes** now carry occupancy — this is preregistered exit criterion 6.

```bash
cargo test -p hornvale-worldgen --test occupancy_readout regenerate -- --ignored
awk -F, 'NR>1 && $3>0 {print $2}' windows/worldgen/tests/fixtures/occupancy.csv | sort -u
```

- [ ] **Step 5: Gate, fmt, commit**

```bash
make gate 2>&1 | tee /tmp/hv-gate-t8.txt
cargo fmt
git add -A
git commit -m "feat(species): the sea is inhabited - four marine kinds and the amphibious proof case (the-vacancy T8)"
```

Put the marine-biome occupancy count and the amphibious result in the commit message.

---

# Stage 4 — The gnoll

### Task 9: The gnoll, in one commit

**Files:**
- Modify: `domains/species/src/lib.rs` (`biosphere_registry`, `family_of`, `gnoll_condition_niche`, `psyche_registry`, `society_registry`, `perception_registry`)
- Modify: `domains/language/src/lib.rs` (`articulation_registry`, `lexicon_registry`)
- Modify: `domains/species/tests/coverage.rs`, `domains/species/tests/social_form.rs`

**Interfaces:**
- Consumes: the readout (Task 3).
- Produces: `KindId("gnoll")`, a complete fifth people. `StatusBasis::Generosity` reaches its first witness here.

**This task is deliberately one commit, not two.** A `Settled` kind without its peopled component set fails `components.rs`'s referential-integrity check, so every `hornvale-worldgen` test breaks between "add the biosphere row" and "add the peopled rows". The project's quality gate is absolute — every commit compiles and passes existing tests — so the body and the mind land together. Author in the order below (it localises failures), but commit once at the end.

Read `components.rs`'s `check_integrity` first (around lines 280–320) for the exact invariants: `speech ⊆ perception ⊆ mind`, and `society ⟺ minded ∧ social`. A `MalformedKind` error names exactly which registry is missing a row.

**Gnoll follows kobold's shape, not the goblinoids'.** Kobold is the roster's singleton-family people: `family_of` maps it to its own name and it carries **no** `family_proto` entry, because `components.rs` requires a proto only for a label held by ≥2 kinds (`family_proto` holds exactly `goblinoid`, `draconic`, `plant`). So `family_of` gets `(KindId("gnoll"), "gnoll")` and no proto is added.

- [ ] **Step 1: Author the condition niche against the readout**

Hot-arid. Cite percentiles from `fixtures/occupancy.csv` in the doc comment, in the style of `hobgoblin_condition_niche`'s "p24 of settleable land, band p10–p60". The elevation optimum must be **metres above sea level** (The Tumult's frame) — this is the axis that has already rotted once.

- [ ] **Step 2: Add the biosphere and family rows**

`mass` from the 5E MM (gnolls are ~7 ft humanoids — source the weight, do not estimate it); `metabolic_class: Endotherm`; `potency: 0.0` (mundane CR); `social_form: SocialForm::Settled`; a mixed omnivore niche weighted toward `ANIMAL_PREY`, consistent with a high-variance forager.

- [ ] **Step 3: The mind and society vectors**

`MindVector` scalars are ratios in `[0,1]` with 0.5 ≡ the goblin baseline. `SocietyVector` gets **`status_basis: StatusBasis::Generosity`** — the campaign's headline promotion, and the reason this people exists. Justify it in a doc comment from the ecology, not from lore: where forage is scarce and high-variance, sharing a windfall is the status currency.

- [ ] **Step 4: The perception vector**

Read `activity` off the gnoll's own authored `insolation` optimum, the way The Vigil derived the dragons' schedules. A hot-arid kind at high insolation is `Diurnal`; if you author a low-insolation optimum, it is `Crepuscular` — which would incidentally give that cell its second witness. Note in the doc which way it went and why.

- [ ] **Step 5: The language rows**

Add `articulation_registry` and `lexicon_registry` entries in `domains/language/src/lib.rs`, following kobold's singleton-family shape. No `family_proto` entry (Task 9).

- [ ] **Step 6: Verify integrity, then the full surface**

```bash
cargo test -p hornvale-species -p hornvale-language 2>&1 | tail -10
cargo nextest run --workspace --no-fail-fast 2>&1 | tee /tmp/hv-t10.txt
```

A `MalformedKind` error names exactly which registry is missing a row. Then classify every failure as in Task 7 Step 4 — mechanical drift is re-pinned here, non-mechanical stops the task.

- [ ] **Step 7: Update the coverage table’s headline row**

`status_basis_coverage_matches_the_table` flips `Generosity` from `Rung::Declared` / `&[]` to `Rung::Witnessed` / `&["gnoll"]`.

- [ ] **Step 8: Regenerate always-run artifacts and the readout**

```bash
bash scripts/regenerate-artifacts.sh
cargo test -p hornvale-worldgen --test occupancy_readout regenerate -- --ignored
git diff --stat
```

The language artifacts (dictionary, chorus) will move here — that is expected for a fifth people. Re-pin in this commit.

- [ ] **Step 9: Gate, fmt, commit**

```bash
make gate 2>&1 | tee /tmp/hv-gate-t9.txt
cargo fmt
git add -A
git commit -m "feat(species): the gnoll speaks - a fifth people, and Generosity's first witness (the-vacancy T9)"
```

---

### Task 10: Measure the preregistered exit criteria

**Files:**
- Create: nothing permanent unless a criterion fails.

**Interfaces:**
- Consumes: everything above.
- Produces: the measured verdict on spec §9's seven criteria, for the G6 package.

- [ ] **Step 1: Measure criterion 7 — the falsifiable one**

The spec names this the likeliest failure: a fifth competitor in the settlement stack is exactly the pressure The Sundering's gate caught as depopulation. Count the peoples holding settlements on the canonical seed:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-vacancy.json
cargo run -p hornvale -- almanac --world /tmp/hv-vacancy.json | tee /tmp/hv-almanac.txt
```

Extract the settlement composition by people. **Expected: at least four peoples hold settlements.**

- [ ] **Step 2: If criterion 7 fails, do not weaken it**

Take The Menagerie's honest move: ship the structural deliverables, convert the criterion into a preregistered `#[ignore]`d test that names the prerequisite, and report. Do **not** adjust the gnoll's niche until the number comes out right — that is fitting the world to the criterion.

- [ ] **Step 3: Verify criteria 1–6 and record each**

| # | check |
|---|---|
| 1 | `cargo test -p hornvale-species --test coverage` — every row justified |
| 2 | `cargo test -p hornvale-worldgen --test non_void_roster` — passes on all ~34 kinds |
| 3 | Task 5/6 commits showed zero artifact drift (already recorded) |
| 4 | `Generosity` and a second `DETRITUS` witness are PINNED by the coverage table |
| 5 | `Gregarious × ANIMAL_PREY`, `Sessile × DETRITUS`, multi-axis uptake are WITNESSED |
| 6 | ≥1 kind centred in each of hot-arid/savanna/boreal, and ≥4 marine biomes occupied |

- [ ] **Step 4: Commit the verdict**

```bash
git commit --allow-empty -F - <<'EOF'
test(the-vacancy): the preregistered verdict on all seven exit criteria (T10)
EOF
```

Record each criterion's measured result in the message body.

---

# Stage 5 — Close

### Task 11: Absorb, regenerate, and close

**Files:**
- Create: `book/src/chronicle/the-vacancy.md`, `docs/retrospectives/the-vacancy.md`
- Modify: `book/src/frontier/idea-registry.md`, the species chapter, `book/src/open-questions.md`, `book/src/SUMMARY.md`

- [ ] **Step 1: Absorb `the-tithe` BEFORE the regen**

`the-tithe` re-pins `book/src/laboratory/generated/the-history/rows.csv`; our regen rewrites every census golden. A regen that races it clobbers or is clobbered.

```bash
make preflight
```

On an ancestry NO-GO, merge main into this branch and re-run the gate **here**:

```bash
git merge origin/main
make gate 2>&1 | tee /tmp/hv-gate-absorb.txt
```

A clean textual merge has hidden a semantic collision on this exact surface before — never assume, always re-run the full gate on the merged result.

- [ ] **Step 2: The single census regen, on `lefford`**

The Mac is **not** canonical (decision 0063). Run the census on `lefford` with a resolved worktree path and a pinned ref, and verify HEAD before trusting the output:

```bash
HV_CENSUS=1 bash scripts/regenerate-artifacts.sh
```

Then confirm the goldens moved as expected and `golden-pins.sql`'s four touched files agree (column order is `(live, pinned)`).

- [ ] **Step 3: The chronicle**

`book/src/chronicle/the-vacancy.md`, at the project's deliberate altitude — technical and mathematical, comprehensible without reading the code. The story is: the model declared more states than the roster could inhabit; the sea was open all along and the block was a misreading; and the instrument now makes a vacancy visible. **No registry IDs outside `book/src/frontier/`** — `docs_consistency` enforces the ban; name the concept instead.

- [ ] **Step 4: Freshness sweep and the Confidence Gradient**

Update the species chapter with the new roster and the coverage table's meaning. Re-score any `book/src/open-questions.md` bet this campaign moved (decision 0030) — the peoples-diversity and habitat bets are the candidates.

- [ ] **Step 5: The retrospective**

`docs/retrospectives/the-vacancy.md` — process lessons only, one page. The lead lesson is already known and must be written honestly: **I scoped the sea out on a blocker I had not read to the end of.** Three docs independently described the aquatic path; I stopped one sentence early, at the sentence that confirmed my prior. Nathan's question caught it. The rule that would have caught it is "re-verify a blocking external fact where it blocks."

Also carry: the `Crepuscular` cell that spec §5.1 listed without checking The Vigil's `speech ⊆ perception ⊆ mind` lattice (Task 7) — a plan-stage catch that a spec-stage check would have made.

- [ ] **Step 6: Flip the registry rows**

`BIO-37`'s roster-generation half → `shipped`. `MAP-11` gains the water-fauna half as shipped with the people half still open. `BIO-42`/`BIO-43`/`BIO-44`/`BIO-45` stay `raw`; repoint their **Where** at the chronicle. Promote the followup register into the retrospective's follow-up section.

- [ ] **Step 7: The final whole-branch review**

Not optional even when green. A task-scoped review cannot see a sibling crate, and the final whole-branch pass has caught a bug invisible to every per-task review before.

```bash
make gate 2>&1 | tee /tmp/hv-gate-final.txt
cargo test -p hornvale --test docs_consistency
```

- [ ] **Step 8: Present the G6 package**

The post-G3 ledger digest, save-format and determinism entries first, then the measured exit criteria, then the merge. **G6 is a hard stop — do not merge before Nathan clears it.**

---

## Self-Review

**Spec coverage.** §1's six deliverables map to T1/T3 (instrument), T4 (non-void), T5–T6 (marine axis), T7–T8 (fauna), T9 (gnoll), T2 (`Autotroph` doc). §4.1→T1, §4.2→T4, §3.3→T2, §3.4→T5/T6, §5.1→T7, §5.2→T8, §5.3→T9, §7's reconciliation→T7/T8 steps 4–5 and T9 step 6, §8's five stages→the five stage headers, §9's criteria→T10, §11's risks→the STOP-and-report steps and T11 step 1.

**One spec requirement the plan corrects rather than implements:** §5.1's "a mundane `Crepuscular` witness". `ActivityCycle` lives in `PerceptionVector`, carried only by minded speaking kinds under The Vigil's enforced lattice, so a mundane beast cannot hold one. Task 7 drops the giant badger, records the real blocker, and reports it. Task 9 notes that the gnoll may incidentally provide the second witness depending on its insolation optimum.

**Type consistency.** `MARINE_FORAGE` (id 5, `ResourceKind::Stock`) is defined in T5 and used by the same name in T6 and T8. `marine_forage_supply_field(geo, terrain, climate, scale) -> CellMap<f64>` is defined in T6 step 2 and called in T6 step 3 with `MARINE_SUPPLY_SCALE`. `Rung::{Declared, Witnessed}` is defined in T1 and edited by the same names in T7, T8, T9. `kind_is_viable_on(seed, name) -> bool` is defined and used within T4.

**Known deliberate looseness.** Three test bodies (T6 step 5's two, T3's render function) specify contract, schema, and required assertions but delegate world-construction to "the helper the neighbouring tests use" rather than quoting a signature. That is deliberate: I verified the seams these tests consume, but not the test-harness idiom of every file, and quoting a signature I have not read is how a plan invents an API. Each such step names the exact file and nearby test to read first.
