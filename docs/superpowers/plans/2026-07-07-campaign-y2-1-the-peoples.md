# Campaign Y2-1: The Peoples — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** A species substrate (`domains/species`) with goblins as the parameterized baseline and kobolds as a second people; two-species worlds by default; placement and social structure diverging per species for recountable reasons.

**Architecture:** Species are authored data (closed six-dimension psychology vector; goblin ≡ baseline, every downstream formula identity-at-baseline); the social grammar stays code. New kernel-only crate `domains/species`; culture's rule table gains a `PsychSummary`; settlement gains weighted+tagged placement and species-qualified draws; `windows/worldgen` composes everything (joint greedy across species, per-species weights, `peopled-by` facts, goblin-only religion). The keystone test is the superset contract: a goblin-pinned world reproduces pre-C1 main's almanac byte-for-byte and its ledger exactly on pre-C1 predicates.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, mdbook.

**Spec:** `docs/superpowers/specs/2026-07-07-campaign-y2-1-the-peoples-design.md` (governs; the Year-2 metaplan §5 sits above it).

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; run `cargo fmt` before committing.
- Determinism constitutional: no wall-clock, no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec`), float sorts via `total_cmp` with deterministic tie-breaks.
- `domains/species` depends on `hornvale-kernel` and NOTHING else (`cli/tests/architecture.rs` enforces; workspace members glob `domains/*` auto-includes it).
- No new external dependencies. `#![warn(missing_docs)]` on the new crate; every public item documented.
- Stream labels are permanent: goblins keep `settlement/name` + `settlement/placement` untouched; kobolds add `settlement/kobold/name` + `settlement/kobold/population`. No renames, no reordering of existing consumption.
- Goblin baseline values: scalars 0.5, `Hierarchic`, `Rank`. Every modulation formula must be the identity function at these values — this is what makes Task 8's superset contract hold by construction.
- Species entities are minted AFTER all settlements and commit facts ONLY under new predicates (never `name`, never any pre-C1 predicate) — entity-id and filtered-ledger stability depend on this.
- Evidence discipline (campaign-standing): implementer reports carry verbatim command transcripts; paraphrase-as-transcript gets bounced in review.
- Work on branch `campaign-y2-1-the-peoples` (worktree per `superpowers:using-git-worktrees`).

---

### Task 1: The species book chapter (book-driven development — no code)

**Files:**
- Create: `book/src/domains/species.md`
- Modify: `book/src/SUMMARY.md` (add `- [Species](./domains/species.md)` alongside the other domain chapters, before Culture)

**Interfaces:**
- Consumes: the spec (read it in full) and the voice/altitude of `book/src/domains/culture.md` (read it first — match its register).
- Produces: the chapter later tasks' code must live up to; Task 11 revisits it in the freshness sweep.

- [ ] **Step 1: Write the chapter**

Content (prose at book altitude — technical, no code): why peoples-plural is Year 2's first substrate; the closed six-dimension vector with the goblin-baseline trick (0.5 ≡ goblin, modulations identity at baseline — the byte-identity argument in prose); species-are-data / the-social-grammar-is-code and why per-species rule tables were refused (the ADR 0011 rhyme); the D&D 5E SRD (CC-BY-4.0) authoring-corpus method with the kobold model card: a table of all six dimensions × (goblin, kobold) values, each kobold value with its one-line SRD lore derivation (spec §3 has the exact table — reproduce its content, not its markdown, in the book's own voice); deliberation latency declared idle this campaign; kobold nocturnality banked for The Eyes; the ontology-trap posture (closed vector, widening needs a campaign).

- [ ] **Step 2: Build and verify**

Run: `mdbook build book`
Expected: clean build, chapter reachable from SUMMARY.

- [ ] **Step 3: Commit**

```bash
git add book/src/domains/species.md book/src/SUMMARY.md
git commit -m "docs(book): species chapter opens Campaign Y2-1 (book-driven development)"
```

### Task 2: Commit the pre-species fixtures

The superset contract needs the CURRENT (pre-any-code-change) seed-42 outputs frozen as fixtures. This task must land before any code task.

**Files:**
- Create: `cli/tests/fixtures/pre-species-seed-42-almanac.md`
- Create: `cli/tests/fixtures/pre-species-seed-42-world.json`

**Interfaces:**
- Produces: fixture paths consumed verbatim by Task 8's identity test.

- [ ] **Step 1: Generate the fixtures from the current tree**

```bash
mkdir -p cli/tests/fixtures
cargo run -p hornvale -- new --seed 42 --out cli/tests/fixtures/pre-species-seed-42-world.json
cargo run -p hornvale -- almanac --world cli/tests/fixtures/pre-species-seed-42-world.json > cli/tests/fixtures/pre-species-seed-42-almanac.md
```

- [ ] **Step 2: Sanity-check the fixtures**

Run: `grep -c "goblin village" cli/tests/fixtures/pre-species-seed-42-almanac.md`
Expected: `1` (the flagship line is present; the almanac is the real current output, flagship Grumoknar).

- [ ] **Step 3: Commit**

```bash
git add cli/tests/fixtures/
git commit -m "test(fixtures): freeze pre-species seed-42 world + almanac for the superset contract"
```

### Task 3: The `domains/species` crate

**Files:**
- Create: `domains/species/Cargo.toml`
- Create: `domains/species/src/lib.rs`
- Modify: `cli/src/streams.rs` (6-array → 7 with `hornvale-species`)
- Modify: `cli/Cargo.toml` (dependency `hornvale-species` so streams can call it; mirror how `hornvale-culture` is declared)

**Interfaces:**
- Consumes: kernel only (`ConceptRegistry`, `EntityId`, `Fact`, `LedgerError`, `RegistryError`, `Value`, `World`).
- Produces (Tasks 5–9 rely on these exact names):
  - `PsychVector { threat_response: f64, deliberation_latency: f64, in_group_radius: f64, time_horizon: f64, sociality: Sociality, status_basis: StatusBasis }`
  - `enum Sociality { Hierarchic, Communal }`, `enum StatusBasis { Rank, Knowledge, Generosity }`
  - `SpeciesDef { name: &'static str, noun: &'static str, psych: PsychVector, worker_override: Option<&'static str>, warrior: &'static str, artisan: &'static str, shaman: &'static str, top: &'static str, syllables: &'static [&'static str] }`
  - `registry() -> BTreeMap<&'static str, SpeciesDef>` (goblin, kobold)
  - `register_concepts(&mut ConceptRegistry) -> Result<(), RegistryError>`
  - `genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError>` — mints one entity per registry species (registry order), commits vector facts
  - `people(world: &mut World, settlement: EntityId, species: &str) -> Result<(), LedgerError>` — commits the `peopled-by` fact
  - `species_of(world: &World, settlement: EntityId) -> Option<String>`
  - `PEOPLED_BY: &str = "peopled-by"`, `stream_labels() -> Vec<(&'static str, &'static str)>` (empty — species draws nothing)

- [ ] **Step 1: Crate manifest**

```toml
# domains/species/Cargo.toml
[package]
name = "hornvale-species"
version = "0.1.0"
edition.workspace = true
license.workspace = true

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests (inside lib.rs `#[cfg(test)]`)**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn goblin_is_the_baseline_vector() {
        let reg = registry();
        let g = &reg["goblin"].psych;
        for v in [g.threat_response, g.deliberation_latency, g.in_group_radius, g.time_horizon] {
            assert_eq!(v, 0.5, "goblin scalars must sit exactly at baseline");
        }
        assert_eq!(g.sociality, Sociality::Hierarchic);
        assert_eq!(g.status_basis, StatusBasis::Rank);
    }

    #[test]
    fn registry_is_ordered_goblin_first_and_kobold_contrasts() {
        let reg = registry();
        let names: Vec<&str> = reg.keys().copied().collect();
        assert_eq!(names, vec!["goblin", "kobold"]);
        let k = &reg["kobold"].psych;
        assert_eq!(k.sociality, Sociality::Communal);
        assert_eq!(k.status_basis, StatusBasis::Knowledge);
        assert!(k.in_group_radius < 0.5 && k.time_horizon > 0.5 && k.threat_response > 0.5);
        assert_eq!(reg["kobold"].noun, "warren");
        assert_eq!(reg["kobold"].top, "elders");
    }

    #[test]
    fn genesis_commits_vector_facts_and_people_links_settlements() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let settlement = w.ledger.mint_entity();
        let ids = genesis(&mut w).unwrap();
        assert_eq!(ids.len(), 2);
        people(&mut w, settlement, "kobold").unwrap();
        assert_eq!(species_of(&w, settlement).as_deref(), Some("kobold"));
        // The species entity carries its vector under species predicates.
        let kobold = ids["kobold"];
        assert!(matches!(
            w.ledger.value_of(kobold, THREAT_RESPONSE),
            Some(Value::Number(n)) if *n > 0.5
        ));
        assert_eq!(
            w.ledger.text_of(kobold, SPECIES_NAME),
            Some("kobold")
        );
    }

    #[test]
    fn species_facts_touch_no_pre_existing_predicate() {
        let mut w = World::new(Seed(1));
        register_concepts(&mut w.registry).unwrap();
        genesis(&mut w).unwrap();
        // No species fact may land under the kernel NAME predicate (or any
        // other pre-C1 predicate) — the superset contract depends on it.
        for f in w.ledger.all_facts() {
            assert!(f.predicate.starts_with("species-") || f.predicate == PEOPLED_BY,
                "unexpected predicate {}", f.predicate);
        }
    }
}
```

(If the kernel's `Ledger` has no `all_facts()` iterator, use the closest existing iteration — `find`/`facts_about` per predicate — and assert `w.ledger.find(hornvale_kernel::NAME).next().is_none()` instead; note which you used.)

- [ ] **Step 3: Run to verify failure**

Run: `cargo test -p hornvale-species`
Expected: FAIL to compile (nothing implemented).

- [ ] **Step 4: Implement lib.rs**

```rust
//! Species, tier 1: authored species definitions — a closed six-dimension
//! psychology vector, vocabulary stopgaps, and placeholder syllables. Species
//! are data; the social grammar stays code (spec §2). Goblin is the baseline:
//! scalars 0.5, default enum variants; every downstream modulation is the
//! identity function at this vector.
#![warn(missing_docs)]

use std::collections::BTreeMap;

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World,
};

/// Predicate: a species entity's name (functional, Text).
pub const SPECIES_NAME: &str = "species-name";
/// Predicate: how a species answers threat, flee 0 ↔ stand 1 (functional, Number).
pub const THREAT_RESPONSE: &str = "species-threat-response";
/// Predicate: how slowly a species decides (functional, Number).
pub const DELIBERATION_LATENCY: &str = "species-deliberation-latency";
/// Predicate: how wide a species draws "us" (functional, Number).
pub const IN_GROUP_RADIUS: &str = "species-in-group-radius";
/// Predicate: how far ahead a species plans (functional, Number).
pub const TIME_HORIZON: &str = "species-time-horizon";
/// Predicate: hierarchic or communal sociality (functional, Text).
pub const SOCIALITY_MODE: &str = "species-sociality-mode";
/// Predicate: what earns standing — rank, knowledge, generosity (functional, Text).
pub const STATUS_BASIS: &str = "species-status-basis";
/// Predicate: the species that peoples a settlement (functional, Text).
pub const PEOPLED_BY: &str = "peopled-by";

/// How a species organizes authority.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sociality {
    /// Ranked authority under a single head.
    Hierarchic,
    /// Collective authority, consensus-run.
    Communal,
}

/// What earns standing in a species' societies.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StatusBasis {
    /// Dominance and position.
    Rank,
    /// Craft, lore, and cunning.
    Knowledge,
    /// Provision and largesse.
    Generosity,
}

/// The closed six-dimension psychology vector (spec §3). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PsychVector {
    /// How a society answers threat: flee 0 ↔ stand 1.
    pub threat_response: f64,
    /// How slowly decisions are made (idle this campaign; banked).
    pub deliberation_latency: f64,
    /// How wide "us" is drawn: insular 0 ↔ expansive 1.
    pub in_group_radius: f64,
    /// How far ahead works are planned: immediate 0 ↔ generational 1.
    pub time_horizon: f64,
    /// Authority shape.
    pub sociality: Sociality,
    /// What earns standing.
    pub status_basis: StatusBasis,
}

/// One authored species: vector, vocabulary stopgaps (deleted by The
/// Tongues), and a placeholder syllable pool for names.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SpeciesDef {
    /// The species name ("goblin", "kobold").
    pub name: &'static str,
    /// The settlement noun ("village", "warren").
    pub noun: &'static str,
    /// The psychology vector.
    pub psych: PsychVector,
    /// Worker-role override; `None` = the subsistence worker word.
    pub worker_override: Option<&'static str>,
    /// The warrior-rung word.
    pub warrior: &'static str,
    /// The artisan-rung word.
    pub artisan: &'static str,
    /// The shaman-rung word.
    pub shaman: &'static str,
    /// The top-rung word ("chief", "elders").
    pub top: &'static str,
    /// Placeholder name syllables (goblin's pool stays in settlement).
    pub syllables: &'static [&'static str],
}

/// Kobold placeholder syllables — distinct mouth-feel from the goblin pool.
const KOBOLD_SYLLABLES: [&str; 10] = [
    "zik", "thur", "kra", "ssk", "vex", "mir", "dak", "usz", "pli", "kek",
];

/// The authored species registry, ordered (goblin sorts first). Kobold
/// values are derived from D&D 5E SRD lore — see the species chapter's
/// model card for each derivation.
pub fn registry() -> BTreeMap<&'static str, SpeciesDef> {
    let mut reg = BTreeMap::new();
    reg.insert(
        "goblin",
        SpeciesDef {
            name: "goblin",
            noun: "village",
            psych: PsychVector {
                threat_response: 0.5,
                deliberation_latency: 0.5,
                in_group_radius: 0.5,
                time_horizon: 0.5,
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
            },
            worker_override: None,
            warrior: "warrior",
            artisan: "artisan",
            shaman: "shaman",
            top: "chief",
            syllables: &[], // goblin names keep settlement's legacy pool
        },
    );
    reg.insert(
        "kobold",
        SpeciesDef {
            name: "kobold",
            noun: "warren",
            psych: PsychVector {
                threat_response: 0.8,
                deliberation_latency: 0.7,
                in_group_radius: 0.2,
                time_horizon: 0.8,
                sociality: Sociality::Communal,
                status_basis: StatusBasis::Knowledge,
            },
            worker_override: Some("digger"),
            warrior: "warden",
            artisan: "shaper",
            shaman: "keeper",
            top: "elders",
            syllables: &KOBOLD_SYLLABLES,
        },
    );
    reg
}

/// Every seed-derivation label this crate uses (none — species are authored).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register species' contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(SPECIES_NAME, true, "a species entity's name")?;
    registry.register_predicate(THREAT_RESPONSE, true, "flee 0 ↔ stand 1")?;
    registry.register_predicate(DELIBERATION_LATENCY, true, "decision slowness, 0-1")?;
    registry.register_predicate(IN_GROUP_RADIUS, true, "how wide 'us' is drawn, 0-1")?;
    registry.register_predicate(TIME_HORIZON, true, "planning depth, 0-1")?;
    registry.register_predicate(SOCIALITY_MODE, true, "hierarchic or communal")?;
    registry.register_predicate(STATUS_BASIS, true, "rank, knowledge, or generosity")?;
    registry.register_predicate(PEOPLED_BY, true, "the species that peoples a settlement")
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "species".to_string(),
    }
}

/// Mint one entity per species (registry order) and commit its authored
/// vector as facts. Species entities carry facts ONLY under this crate's
/// predicates — never `name` or any pre-existing predicate (the superset
/// contract, spec §8, depends on it).
pub fn genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let mut ids = BTreeMap::new();
    for (name, def) in registry() {
        let id = world.ledger.mint_entity();
        let p = def.psych;
        let sociality = match p.sociality {
            Sociality::Hierarchic => "hierarchic",
            Sociality::Communal => "communal",
        };
        let status = match p.status_basis {
            StatusBasis::Rank => "rank",
            StatusBasis::Knowledge => "knowledge",
            StatusBasis::Generosity => "generosity",
        };
        world.ledger.commit(fact(id, SPECIES_NAME, Value::Text(name.to_string())), &world.registry)?;
        world.ledger.commit(fact(id, THREAT_RESPONSE, Value::Number(p.threat_response)), &world.registry)?;
        world.ledger.commit(fact(id, DELIBERATION_LATENCY, Value::Number(p.deliberation_latency)), &world.registry)?;
        world.ledger.commit(fact(id, IN_GROUP_RADIUS, Value::Number(p.in_group_radius)), &world.registry)?;
        world.ledger.commit(fact(id, TIME_HORIZON, Value::Number(p.time_horizon)), &world.registry)?;
        world.ledger.commit(fact(id, SOCIALITY_MODE, Value::Text(sociality.to_string())), &world.registry)?;
        world.ledger.commit(fact(id, STATUS_BASIS, Value::Text(status.to_string())), &world.registry)?;
        ids.insert(name.to_string(), id);
    }
    Ok(ids)
}

/// Commit the `peopled-by` fact linking a settlement to its species.
pub fn people(world: &mut World, settlement: EntityId, species: &str) -> Result<(), LedgerError> {
    world.ledger.commit(
        fact(settlement, PEOPLED_BY, Value::Text(species.to_string())),
        &world.registry,
    )
}

/// The species a settlement is peopled by, if committed.
pub fn species_of(world: &World, settlement: EntityId) -> Option<String> {
    match world.ledger.value_of(settlement, PEOPLED_BY) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

- [ ] **Step 5: Register in the stream manifest**

In `cli/src/streams.rs`, change the array to 7 entries, inserting alphabetically:

```rust
    let sources: [(&str, Vec<(&'static str, &'static str)>); 7] = [
        ("hornvale-astronomy", hornvale_astronomy::stream_labels()),
        ("hornvale-climate", hornvale_climate::stream_labels()),
        ("hornvale-culture", hornvale_culture::stream_labels()),
        ("hornvale-religion", hornvale_religion::stream_labels()),
        ("hornvale-settlement", hornvale_settlement::stream_labels()),
        ("hornvale-species", hornvale_species::stream_labels()),
        ("hornvale-terrain", hornvale_terrain::stream_labels()),
    ];
```

Add `hornvale-species = { path = "../domains/species" }` to `cli/Cargo.toml` mirroring the other domain entries.

- [ ] **Step 6: Run tests to verify pass**

Run: `cargo test -p hornvale-species && cargo test -p hornvale --lib`
Expected: PASS (species unit tests; streams still renders).

- [ ] **Step 7: Full gate, then commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/species cli/src/streams.rs cli/Cargo.toml Cargo.lock
git commit -m "feat(species): domains/species — closed psychology vector, goblin baseline, kobold authored from SRD lore"
```

Note: the committed `concept-registry-generated.md`/`stream-manifest-generated.md` artifacts do NOT change yet — worldgen doesn't call `species::register_concepts` until Task 6; CI's drift check regenerates from wired code. Do not regenerate artifacts in this task.

### Task 4: Settlement — weighted suitability, tagged placement, species-qualified draws

**Files:**
- Modify: `domains/settlement/src/placement.rs`
- Modify: `domains/settlement/src/lib.rs`

**Interfaces:**
- Consumes: existing `SiteInput`, `Placement`, `suitability`, `place`.
- Produces (Task 6 relies on exact names):
  - `SuitabilityWeights { freshwater: f64, coast: f64, temperance: f64, hostility: f64 }` with `BASELINE_WEIGHTS: SuitabilityWeights` (0.45, 0.20, 0.35, 0.50)
  - `suitability_weighted(site: &SiteInput, w: &SuitabilityWeights) -> Option<f64>`; existing `suitability(site)` delegates with `BASELINE_WEIGHTS`
  - `place_tagged(scored: &[(SiteInput, f64, u32)], min_separation_dot: f64, floor: f64) -> Vec<(Placement, u32)>` — pre-scored tagged sites, sort by (score desc, cell asc, tag asc), greedy with shared spacing; existing `place()` reimplemented as tag-0 wrapper (identical output for one tag — that is a test)
  - `generate_species_name(seed: Seed, species: &str, syllables: &[&str], salt: u64) -> String` drawing from `settlement/<species>/name`
  - `draw_species_population(seed: Seed, species: &str, salt: u64, suitability: f64) -> u32` drawing from `settlement/<species>/population`
  - `stream_labels()` gains `settlement/kobold/name` + `settlement/kobold/population` rows

- [ ] **Step 1: Write the failing tests**

Append to `placement.rs` tests:

```rust
    #[test]
    fn weighted_suitability_is_identity_at_baseline_weights() {
        let s = site(1, [1.0, 0.0, 0.0], true, 0.7, true, 12.0, 0.2);
        assert_eq!(suitability(&s), suitability_weighted(&s, &BASELINE_WEIGHTS));
    }

    #[test]
    fn place_tagged_with_one_tag_matches_place_exactly() {
        let sites = vec![
            site(10, [1.0, 0.0, 0.0], true, 0.9, true, 15.0, 0.0),
            site(12, [-1.0, 0.0, 0.0], true, 0.85, true, 15.0, 0.0),
            site(13, [0.0, 1.0, 0.0], true, 0.05, false, 40.0, 0.9),
        ];
        let sep = (12.0_f64.to_radians()).cos();
        let scored: Vec<(SiteInput, f64, u32)> = sites
            .iter()
            .filter_map(|s| suitability(s).map(|sc| (*s, sc, 0u32)))
            .collect();
        let tagged: Vec<Placement> =
            place_tagged(&scored, sep, 0.25).into_iter().map(|(p, _)| p).collect();
        assert_eq!(tagged, place(&sites, sep, 0.25));
    }

    #[test]
    fn tagged_placement_enforces_spacing_across_tags() {
        let close_a = [1.0, 0.0, 0.0];
        let close_b = [0.9998, 0.02, 0.0]; // ~1.1° away
        let a = site(1, close_a, true, 0.9, true, 15.0, 0.0);
        let b = site(2, close_b, true, 0.8, true, 15.0, 0.0);
        let sep = (12.0_f64.to_radians()).cos();
        let placed = place_tagged(&[(a, 0.9, 0), (b, 0.8, 1)], sep, 0.25);
        assert_eq!(placed.len(), 1, "cross-tag spacing must exclude the close site");
        assert_eq!(placed[0].1, 0, "higher score wins regardless of tag");
    }
```

Append to `lib.rs` tests:

```rust
    #[test]
    fn species_name_draws_are_disjoint_from_goblin_draws() {
        // Kobold names come from a different labeled stream AND pool; the
        // legacy goblin path must be untouched by their existence.
        let goblin_before = generate_name(Seed(9), 7);
        let kobold = generate_species_name(Seed(9), "kobold", &["zik", "thur", "kra"], 7);
        let goblin_after = generate_name(Seed(9), 7);
        assert_eq!(goblin_before, goblin_after);
        assert_ne!(kobold, goblin_before);
        assert!(kobold.chars().next().unwrap().is_uppercase());
    }

    #[test]
    fn species_population_is_deterministic_and_species_scoped() {
        let a = draw_species_population(Seed(3), "kobold", 5, 0.5);
        let b = draw_species_population(Seed(3), "kobold", 5, 0.5);
        let g = draw_population(Seed(3), 5, 0.5);
        assert_eq!(a, b);
        // Different labeled stream ⇒ (almost surely) different jitter; assert
        // determinism and range, not inequality with the goblin draw.
        assert!((30..=625).contains(&a));
        let _ = g;
    }

    #[test]
    fn stream_labels_declare_the_kobold_streams() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in ["settlement/kobold/name", "settlement/kobold/population"] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-settlement`
Expected: FAIL to compile (new names missing).

- [ ] **Step 3: Implement**

In `placement.rs`:

```rust
/// The four suitability weights; per-species values are derived at the
/// composition root from the psychology vector (spec §4).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SuitabilityWeights {
    /// Weight on freshwater availability.
    pub freshwater: f64,
    /// Weight on coastal access.
    pub coast: f64,
    /// Weight on temperance.
    pub temperance: f64,
    /// Penalty weight on hostility.
    pub hostility: f64,
}

/// The goblin-baseline weights — the pre-species formula, unchanged.
pub const BASELINE_WEIGHTS: SuitabilityWeights = SuitabilityWeights {
    freshwater: 0.45,
    coast: 0.20,
    temperance: 0.35,
    hostility: 0.50,
};

/// Suitability under explicit weights; `None` if uninhabitable.
pub fn suitability_weighted(site: &SiteInput, w: &SuitabilityWeights) -> Option<f64> {
    if !site.habitable {
        return None;
    }
    let temperance = (1.0 - (site.temperature_c - 15.0).abs() / 15.0).clamp(0.0, 1.0);
    let coast = if site.coastal { 1.0 } else { 0.0 };
    let raw = w.freshwater * site.freshwater.clamp(0.0, 1.0)
        + w.coast * coast
        + w.temperance * temperance
        - w.hostility * site.hostility.clamp(0.0, 1.0);
    Some(raw.clamp(0.0, 1.0))
}

/// Suitability under the goblin-baseline weights (the original formula).
pub fn suitability(site: &SiteInput) -> Option<f64> {
    suitability_weighted(site, &BASELINE_WEIGHTS)
}
```

(Delete the old `suitability` body; keep its doc comment spirit.)

```rust
/// Place a spaced scatter from pre-scored, tagged sites (tag = species index
/// at the root). Sort by score descending, ties by ascending cell then
/// ascending tag; greedily accept sites at least `min_separation_dot`-far
/// from EVERY already-placed site regardless of tag, at or above `floor`.
pub fn place_tagged(
    scored: &[(SiteInput, f64, u32)],
    min_separation_dot: f64,
    floor: f64,
) -> Vec<(Placement, u32)> {
    let mut ranked: Vec<&(SiteInput, f64, u32)> =
        scored.iter().filter(|(_, s, _)| *s >= floor).collect();
    ranked.sort_by(|a, b| {
        b.1.total_cmp(&a.1)
            .then(a.0.cell.0.cmp(&b.0.cell.0))
            .then(a.2.cmp(&b.2))
    });
    let mut placed: Vec<(Placement, u32)> = Vec::new();
    for (site, score, tag) in ranked {
        let too_close = placed
            .iter()
            .any(|(p, _)| dot(p.position, site.position) > min_separation_dot);
        if too_close {
            continue;
        }
        placed.push((
            Placement {
                cell: site.cell,
                position: site.position,
                suitability: *score,
            },
            *tag,
        ));
    }
    placed
}

/// Place a spaced scatter under baseline weights (the original single-people
/// path) — a tag-0 wrapper over `place_tagged`.
pub fn place(sites: &[SiteInput], min_separation_dot: f64, floor: f64) -> Vec<Placement> {
    let scored: Vec<(SiteInput, f64, u32)> = sites
        .iter()
        .filter_map(|s| suitability(s).map(|score| (*s, score, 0u32)))
        .collect();
    place_tagged(&scored, min_separation_dot, floor)
        .into_iter()
        .map(|(p, _)| p)
        .collect()
}
```

In `lib.rs` (species-qualified draws — note the label path `settlement/<species>/name` inserts the species segment between ROOT and NAME):

```rust
/// Generate a settlement name for a non-goblin species from its own labeled
/// stream (`settlement/<species>/name`) and its own syllable pool. Goblin
/// names keep `generate_name` and the legacy label untouched.
pub fn generate_species_name(
    seed: hornvale_kernel::Seed,
    species: &str,
    syllables: &[&str],
    salt: u64,
) -> String {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(species)
        .derive(streams::NAME)
        .derive(&salt.to_string())
        .stream();
    let count = stream.range_u32(2, 3);
    let raw: String = (0..count)
        .map(|_| *stream.pick(syllables).expect("syllable pool is non-empty"))
        .collect();
    format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
}

/// Draw a non-goblin settlement's population from its species-qualified
/// stream (`settlement/<species>/population`), same capacity curve as the
/// goblin draw.
pub fn draw_species_population(
    seed: hornvale_kernel::Seed,
    species: &str,
    salt: u64,
    suitability: f64,
) -> u32 {
    let mut stream = seed
        .derive(streams::ROOT)
        .derive(species)
        .derive("population")
        .derive(&salt.to_string())
        .stream();
    let base = 40.0 + 460.0 * suitability.clamp(0.0, 1.0);
    let jitter = 0.75 + 0.5 * stream.next_f64();
    (base * jitter).round() as u32
}
```

Extend `stream_labels()`:

```rust
        (
            "settlement/kobold/name",
            "per-settlement generated name, kobold stream (species-qualified; goblin keeps settlement/name)",
        ),
        (
            "settlement/kobold/population",
            "per-settlement population, kobold stream (species-qualified; goblin keeps settlement/placement)",
        ),
```

- [ ] **Step 4: Run tests to verify pass**

Run: `cargo test -p hornvale-settlement`
Expected: PASS, including the pre-existing placement tests (`place` byte-compatible via the wrapper).

- [ ] **Step 5: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/settlement
git commit -m "feat(settlement): weighted suitability, cross-tag placement, species-qualified draws"
```

### Task 5: Culture — the modulated rule table

**Files:**
- Modify: `domains/culture/src/structure.rs`
- Modify: `domains/culture/src/lib.rs` (genesis gains the psych parameter; re-exports)

**Interfaces:**
- Consumes: existing `EnvSummary`, `Subsistence`.
- Produces (Task 6 relies on):
  - `RoleVocabulary { worker_override: Option<String>, warrior: String, artisan: String, shaman: String, top: String }` with `Default` = goblin words
  - `PsychSummary { threat_response: f64, time_horizon: f64, communal: bool, rank_status: bool, vocabulary: RoleVocabulary }` with `Default` = goblin baseline (0.5, 0.5, false, true, goblin vocabulary)
  - `structure(env: &EnvSummary, psych: &PsychSummary) -> Vec<String>` (signature change; all callers updated)
  - `genesis(world, settlement, env: &EnvSummary, psych: &PsychSummary)` (signature change)

- [ ] **Step 1: Write the failing tests**

Replace/extend `structure.rs` tests (existing tests updated to pass `&PsychSummary::default()`; identity property test added):

```rust
    #[test]
    fn baseline_psych_reproduces_the_pre_species_table_exactly() {
        // The identity property (spec §2.2): at the goblin baseline the
        // modulated table equals the original on a dense grid.
        let base = PsychSummary::default();
        for sub in [Subsistence::Farming, Subsistence::Herding, Subsistence::Fishing, Subsistence::Foraging] {
            for surplus10 in 0..=10 {
                for pop in [10u32, 150, 201, 250, 301, 500] {
                    for threat10 in 0..=10 {
                        let env = EnvSummary {
                            subsistence: sub,
                            surplus: f64::from(surplus10) / 10.0,
                            population: pop,
                            threat: f64::from(threat10) / 10.0,
                        };
                        let expected = original_structure_for_test(&env);
                        assert_eq!(structure(&env, &base), expected, "diverged at {env:?}");
                    }
                }
            }
        }
    }

    /// The pre-species table, frozen verbatim as the identity oracle.
    fn original_structure_for_test(env: &EnvSummary) -> Vec<String> {
        let mut roles: Vec<String> = Vec::new();
        if env.surplus > 0.6 && env.population > 300 {
            roles.push("slave".to_string());
        }
        roles.push(env.subsistence.worker().to_string());
        if env.threat > 0.4 {
            roles.push("warrior".to_string());
        }
        if env.surplus > 0.6 && env.population > 200 {
            roles.push("artisan".to_string());
        }
        if env.surplus > 0.4 {
            roles.push("shaman".to_string());
        }
        roles.push("chief".to_string());
        roles
    }

    #[test]
    fn communal_knowledge_peoples_never_enslave_and_top_out_collectively() {
        let kobold = PsychSummary {
            threat_response: 0.8,
            time_horizon: 0.8,
            communal: true,
            rank_status: false,
            vocabulary: RoleVocabulary {
                worker_override: Some("digger".to_string()),
                warrior: "warden".to_string(),
                artisan: "shaper".to_string(),
                shaman: "keeper".to_string(),
                top: "elders".to_string(),
            },
        };
        let env = EnvSummary {
            subsistence: Subsistence::Farming,
            surplus: 0.9,
            population: 1000,
            threat: 0.5,
        };
        let s = structure(&env, &kobold);
        assert!(!s.contains(&"slave".to_string()));
        assert_eq!(s.first().map(String::as_str), Some("digger"));
        assert_eq!(s.last().map(String::as_str), Some("elders"));
        assert!(s.contains(&"warden".to_string()), "0.5 > 0.4·0.7 = 0.28");
        assert!(s.contains(&"shaper".to_string()), "long horizon lowers the artisan gates");
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-culture`
Expected: FAIL to compile.

- [ ] **Step 3: Implement**

In `structure.rs`:

```rust
/// Role words a species uses for the rungs (pre-language stopgap; The
/// Tongues deletes this). `worker_override: None` = the subsistence word.
#[derive(Clone, Debug, PartialEq)]
pub struct RoleVocabulary {
    /// Worker-rung override ("digger"); `None` = subsistence worker word.
    pub worker_override: Option<String>,
    /// Warrior-rung word.
    pub warrior: String,
    /// Artisan-rung word.
    pub artisan: String,
    /// Shaman-rung word.
    pub shaman: String,
    /// Top-rung word.
    pub top: String,
}

impl Default for RoleVocabulary {
    fn default() -> Self {
        RoleVocabulary {
            worker_override: None,
            warrior: "warrior".to_string(),
            artisan: "artisan".to_string(),
            shaman: "shaman".to_string(),
            top: "chief".to_string(),
        }
    }
}

/// The psychology a settlement's people bring to structure formation,
/// assembled at the composition root from the species domain (culture never
/// imports species). `Default` is the goblin baseline: every modulation
/// below is the identity function at it.
#[derive(Clone, Debug, PartialEq)]
pub struct PsychSummary {
    /// Flee 0 ↔ stand 1; 0.5 baseline.
    pub threat_response: f64,
    /// Planning depth; 0.5 baseline.
    pub time_horizon: f64,
    /// Communal (true) suppresses singular chieftainship.
    pub communal: bool,
    /// Rank-based status (true) is what makes the slave rung possible.
    pub rank_status: bool,
    /// The species' role words.
    pub vocabulary: RoleVocabulary,
}

impl Default for PsychSummary {
    fn default() -> Self {
        PsychSummary {
            threat_response: 0.5,
            time_horizon: 0.5,
            communal: false,
            rank_status: true,
            vocabulary: RoleVocabulary::default(),
        }
    }
}

/// The ordered role list (lowest → highest) a settlement grows, from its
/// environment modulated by its people's psychology (spec §5). Every
/// modulation is identity at the goblin baseline.
pub fn structure(env: &EnvSummary, psych: &PsychSummary) -> Vec<String> {
    let invest = 1.5 - psych.time_horizon; // 1.0 at baseline
    let mut roles: Vec<String> = Vec::new();
    if psych.rank_status && env.surplus > 0.6 && env.population > 300 {
        roles.push("slave".to_string());
    }
    roles.push(match &psych.vocabulary.worker_override {
        Some(w) => w.clone(),
        None => env.subsistence.worker().to_string(),
    });
    if env.threat > 0.4 * (1.5 - psych.threat_response) {
        roles.push(psych.vocabulary.warrior.clone());
    }
    if env.surplus > 0.6 * invest && f64::from(env.population) > 200.0 * invest {
        roles.push(psych.vocabulary.artisan.clone());
    }
    if env.surplus > 0.4 {
        roles.push(psych.vocabulary.shaman.clone());
    }
    roles.push(psych.vocabulary.top.clone());
    roles
}
```

Note the artisan population gate: `f64::from(env.population) > 200.0 * invest` equals the original strict `> 200` at `invest = 1.0` for every `u32` (integers only; verified by the identity grid test, which includes 201 and 250).

In `lib.rs`, thread the parameter through `genesis` (the `structure(env)` call becomes `structure(env, psych)`; signature `pub fn genesis(world, settlement, env: &EnvSummary, psych: &PsychSummary)`), update the re-export line to `pub use structure::{EnvSummary, PsychSummary, RoleVocabulary, structure};`, and update this crate's existing tests to pass `&PsychSummary::default()`.

- [ ] **Step 4: Run tests to verify pass**

Run: `cargo test -p hornvale-culture`
Expected: PASS (the identity grid is the important one). `cargo test --workspace` will FAIL at `windows/worldgen` (genesis signature) — expected; Task 6 fixes the caller. Do NOT commit yet: Tasks 5+6 land as one commit if needed (every commit compiles), or update the worldgen call site minimally here (`&hornvale_culture::PsychSummary::default()`) so the workspace stays green, and let Task 6 replace it properly. **Prefer the minimal call-site update — commit green.**

- [ ] **Step 5: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add domains/culture windows/worldgen
git commit -m "feat(culture): psychology-modulated structure, identity at the goblin baseline"
```

### Task 6: Worldgen — species pin, joint greedy, species genesis, per-species culture

**Files:**
- Modify: `windows/worldgen/src/settlement_pins.rs` (add `species: Option<String>`, key `species=`)
- Modify: `windows/worldgen/src/lib.rs` (the build pipeline between terrain reconstruction and religion)
- Modify: `cli/src/main.rs` (`--species NAME` flag → pin; extend `usage()`)

**Interfaces:**
- Consumes: Task 3 (`hornvale_species::{registry, register_concepts, genesis, people, species_of, SpeciesDef, PsychVector}`), Task 4 (`SuitabilityWeights`, `suitability_weighted`, `place_tagged`, `generate_species_name`, `draw_species_population`), Task 5 (`PsychSummary`, `RoleVocabulary`, 4-arg culture genesis).
- Produces (Tasks 7–9 rely on):
  - `species_weights(p: &hornvale_species::PsychVector) -> hornvale_settlement::SuitabilityWeights` (pub, in worldgen)
  - `flagship_of(world: &World, species: &str) -> Option<hornvale_settlement::VillageInfo>` (pub, in worldgen) — the first-placed settlement of that species (first `is-settlement` fact whose subject is `peopled-by` that species)
  - two-species default build; goblin-only religion; `species::register_concepts` wired into world construction alongside the other domains

- [ ] **Step 1: Write the failing integration test**

Create `windows/worldgen/tests/species_worlds.rs`:

```rust
//! Two-species worlds: both peoples place, flagships are per-species, the
//! species pin restricts, and unknown species fail loudly.
use hornvale_worldgen::{BuildError, SettlementPins, SkyChoice, build_world, flagship_of};

fn pins(species: Option<&str>) -> SettlementPins {
    SettlementPins {
        species: species.map(str::to_string),
        ..SettlementPins::default()
    }
}

#[test]
fn default_worlds_carry_both_peoples_with_their_own_flagships() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(None),
    )
    .unwrap();
    let goblin = flagship_of(&world, "goblin").expect("goblin flagship");
    let kobold = flagship_of(&world, "kobold").expect("kobold flagship");
    assert_ne!(goblin.id, kobold.id);
    // Both flagships carry committed culture.
    assert!(hornvale_culture::subsistence_of(&world, goblin.id).is_some());
    assert!(hornvale_culture::subsistence_of(&world, kobold.id).is_some());
    let kobold_castes = hornvale_culture::castes_of(&world, kobold.id);
    assert_eq!(kobold_castes.last().map(String::as_str), Some("elders"));
    assert!(!kobold_castes.contains(&"slave".to_string()));
    // Religion runs on the goblin flagship only (spec §6).
    assert!(!hornvale_religion::beliefs_of(&world, goblin.id).is_empty());
    assert!(hornvale_religion::beliefs_of(&world, kobold.id).is_empty());
}

#[test]
fn species_pin_restricts_and_unknown_species_fail_loudly() {
    let goblin_only = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(Some("goblin")),
    )
    .unwrap();
    assert!(flagship_of(&goblin_only, "kobold").is_none());
    assert!(flagship_of(&goblin_only, "goblin").is_some());

    let err = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &pins(Some("elf")),
    )
    .unwrap_err();
    let BuildError::Pins(msg) = err else {
        panic!("expected a pin error, got {err:?}")
    };
    assert!(msg.contains("elf") && msg.contains("goblin") && msg.contains("kobold"));
}
```

(Check `beliefs_of`'s exact signature in `domains/religion/src/lib.rs` before using — if it returns beliefs world-wide rather than per-entity, assert instead that the pantheon's committed subject ids do not include the kobold flagship; note the substitution in your report. Adjust the `build_world` argument list to its real arity — it is 5 args today; the test must call it exactly as `windows/lab/src/metrics.rs::WorldView::build` does.)

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen --test species_worlds`
Expected: FAIL to compile (`species` field, `flagship_of` missing).

- [ ] **Step 3: Implement the pin**

In `settlement_pins.rs`: add the field and parsing (mirror `min_suitability` exactly):

```rust
    /// Restrict the placed species set to this one species; `None` = all
    /// registry species. Validated against the species registry at build.
    pub species: Option<String>,
```

`pin_strings()` gains `if let Some(s) = &self.species { out.push(format!("species={s}")); }`; `parse_pin` gains a `"species" => pins.species = Some(value.to_string()),` arm. Update the struct's `Copy` derive: `String` is not `Copy` — drop `Copy` from the derive list and fix any call sites that relied on it (clone instead). Extend the round-trip test with a `species: Some("kobold".into())` case.

- [ ] **Step 4: Implement the build pipeline**

In `windows/worldgen/src/lib.rs`, where concepts are registered for settlement/culture/religion, add `hornvale_species::register_concepts(&mut world.registry)?;` alongside them (find the existing block with `grep -n "register_concepts" windows/worldgen/src/lib.rs`).

Add the weight derivation:

```rust
/// Per-species suitability weights derived from the psychology vector
/// (spec §4); identity at the goblin baseline.
pub fn species_weights(p: &hornvale_species::PsychVector) -> hornvale_settlement::SuitabilityWeights {
    hornvale_settlement::SuitabilityWeights {
        freshwater: 0.45 * (0.5 + p.time_horizon),
        coast: 0.20 * (2.0 * p.in_group_radius),
        temperance: 0.35,
        hostility: 0.50 * (1.5 - p.threat_response),
    }
}
```

Replace the placement/genesis block (currently: score sites → `place` → build `PlacedSettlement`s → `genesis` → flagship culture+religion) with:

```rust
    // Which species this world places: the whole registry, or the pinned one.
    let all_species = hornvale_species::registry();
    let species_set: Vec<&hornvale_species::SpeciesDef> = match &settlement_pins.species {
        None => all_species.values().collect(),
        Some(name) => match all_species.get(name.as_str()) {
            Some(def) => vec![def],
            None => {
                let known: Vec<&str> = all_species.keys().copied().collect();
                return Err(BuildError::Pins(format!(
                    "unknown species '{name}'; known species: {}",
                    known.join(", ")
                )));
            }
        },
    };

    // Joint greedy across species: every (site × species) pair scored with
    // that species' psychology-derived weights, one shared spacing pass.
    let mut scored: Vec<(hornvale_settlement::SiteInput, f64, u32)> = Vec::new();
    for (tag, def) in species_set.iter().enumerate() {
        let weights = species_weights(&def.psych);
        for site in &sites {
            if let Some(score) = hornvale_settlement::suitability_weighted(site, &weights) {
                scored.push((*site, score, tag as u32));
            }
        }
    }
    let placements = hornvale_settlement::place_tagged(&scored, min_sep, floor);
    let placed: Vec<hornvale_settlement::PlacedSettlement> = placements
        .iter()
        .map(|(p, tag)| {
            let def = species_set[*tag as usize];
            let coord = geo.coord(p.cell);
            let (name, population) = if def.name == "goblin" {
                (
                    hornvale_settlement::generate_name(seed, u64::from(p.cell.0)),
                    hornvale_settlement::draw_population(seed, u64::from(p.cell.0), p.suitability),
                )
            } else {
                (
                    hornvale_settlement::generate_species_name(
                        seed, def.name, def.syllables, u64::from(p.cell.0),
                    ),
                    hornvale_settlement::draw_species_population(
                        seed, def.name, u64::from(p.cell.0), p.suitability,
                    ),
                )
            };
            hornvale_settlement::PlacedSettlement {
                cell: p.cell.0,
                latitude: coord.latitude,
                longitude: coord.longitude,
                biome: climate.biome_at(p.cell).name().to_string(),
                name,
                population,
            }
        })
        .collect();
    let ids = hornvale_settlement::genesis(&mut world, &placed)?;

    // Species entities AFTER settlements (entity-id stability, spec §8),
    // then the peopled-by link for every settlement.
    hornvale_species::genesis(&mut world)?;
    for (id, (_, tag)) in ids.iter().zip(placements.iter()) {
        hornvale_species::people(&mut world, *id, species_set[*tag as usize].name)?;
    }

    // Per-species flagship culture; religion on the goblin flagship only.
    for (tag, def) in species_set.iter().enumerate() {
        let Some(pos) = placements.iter().position(|(_, t)| *t as usize == tag) else {
            continue; // a species may place nothing on a hostile world
        };
        let flagship = ids[pos];
        let fcell = placements[pos].0.cell;
        let coastal = geo.neighbors(fcell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(fcell);
        let class = biome_class(climate.biome_at(fcell));
        let subsistence = hornvale_culture::subsistence(class, coastal);
        let surplus = (hornvale_culture::fertility(class) * moisture).clamp(0.0, 1.0);
        let threat = terrain.unrest_at(fcell).clamp(0.0, 1.0);
        let env = hornvale_culture::EnvSummary {
            subsistence,
            surplus,
            population: placed[pos].population,
            threat,
        };
        let psych = hornvale_culture::PsychSummary {
            threat_response: def.psych.threat_response,
            time_horizon: def.psych.time_horizon,
            communal: def.psych.sociality == hornvale_species::Sociality::Communal,
            rank_status: def.psych.status_basis == hornvale_species::StatusBasis::Rank,
            vocabulary: hornvale_culture::RoleVocabulary {
                worker_override: def.worker_override.map(str::to_string),
                warrior: def.warrior.to_string(),
                artisan: def.artisan.to_string(),
                shaman: def.shaman.to_string(),
                top: def.top.to_string(),
            },
        };
        hornvale_culture::genesis(&mut world, flagship, &env, &psych)?;
        if def.name == "goblin" {
            let castes = hornvale_culture::castes_of(&world, flagship);
            let society = hornvale_religion::SocietySummary {
                strata: castes.len(),
                has_priesthood: castes.iter().any(|c| c == "shaman"),
            };
            let seen = observed_phenomena(&world, 0.0)?;
            hornvale_religion::genesis(&mut world, flagship, &seen, &society)?;
        }
    }
```

Add the per-species flagship accessor:

```rust
/// The first-placed settlement of `species` (its flagship), if any.
pub fn flagship_of(world: &World, species: &str) -> Option<hornvale_settlement::VillageInfo> {
    let id = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .find(|s| hornvale_species::species_of(world, *s).as_deref() == Some(species))?;
    let name = world
        .ledger
        .text_of(id, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("settlement {}", id.0));
    let population = match world.ledger.value_of(id, hornvale_settlement::POPULATION) {
        Some(hornvale_kernel::Value::Number(n)) => *n as u32,
        _ => 0,
    };
    Some(hornvale_settlement::VillageInfo { id, name, population })
}
```

Add `hornvale-species = { path = "../../domains/species" }` to `windows/worldgen/Cargo.toml`.

In `cli/src/main.rs`: add a `--species NAME` flag parsed into `SettlementPins.species` wherever `--min-suitability` is parsed (grep for `min-suitability` in main.rs and mirror it, including `scout` if it takes settlement pins), and add the flag to `usage()` with the line `--species <NAME>  place only this species (default: all known species)`.

- [ ] **Step 5: Run the integration test, then survey the workspace**

Run: `cargo test -p hornvale-worldgen --test species_worlds`
Expected: PASS.
Run: `cargo test --workspace 2>&1 | tail -30`
Expected fallout candidates: `cli/tests/exit_criterion.rs` and lab metrics tests (worlds now carry two peoples; `village_info` may point at a kobold if one out-scored every goblin site). Fix ONLY compile errors here (e.g. `SettlementPins` lost `Copy`); behavioral re-pins belong to Tasks 7–10 — record the failing list for them. If behavioral failures block a green commit, fold the minimal re-pin into this commit and say so.

- [ ] **Step 6: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add windows/worldgen cli domains
git commit -m "feat(worldgen): two-species worlds — joint greedy placement, species genesis, per-species culture"
```

### Task 7: Presentation — per-species almanac and REPL lines

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`settlement_lines`, `culture_lines` → per-species)
- Modify: `windows/almanac/src/lib.rs` (the People section renders per-species blocks)
- Modify: whatever builds `AlmanacContext` (find with `grep -rn "AlmanacContext" windows/ cli/` — it gains per-species people blocks)

**Interfaces:**
- Consumes: `flagship_of`, `hornvale_species::registry()` (noun per species), Task 6's per-species culture facts.
- Produces: `PeopleBlock { species: String, noun: String, name: String, population: u32, culture_lines: Vec<String> }` replacing the single `village`/`culture_lines` pair in `AlmanacContext`, rendered in registry order (goblin first).

**Byte-identity requirement (this task's acceptance bar):** in a goblin-only world the rendered strings must be byte-identical to today's: `The goblin village of **{name}**, population {population}.` then the two culture lines `{name} lives by {subsistence}.` / `Its roles, lowest to highest: {roles}.`, and `settlement_lines`' `The land holds {n} settlement(s).` + `The chief settlement, {name}, holds {pop} souls amid {biome}.` The per-species generalization must reduce to these exact strings for a lone goblin block: `The {species} {noun} of **{name}**, population {population}.` and per-species chief lines emitted goblin-first.

- [ ] **Step 1: Write the failing test**

In `windows/almanac`'s existing test module (find the current People-section test with `grep -n "goblin village" windows/almanac/src/lib.rs`), add a two-block context test asserting both `The goblin village of **Grum**` and `The kobold warren of **Zikthur**` render, kobold block after goblin, each followed by its own culture lines; and a one-block goblin context test asserting the exact legacy string (copy it verbatim from the current almanac renderer).

- [ ] **Step 2: Run to verify failure** — `cargo test -p hornvale-almanac`; FAIL (context shape).

- [ ] **Step 3: Implement**

Replace `AlmanacContext.village: Option<VillageInfo>` + `culture_lines: Vec<String>` with `peoples: Vec<PeopleBlock>` (shape above). Renderer:

```rust
    if ctx.peoples.is_empty() {
        doc.push_str("No settlements are known.\n\n");
    } else {
        for p in &ctx.peoples {
            doc.push_str(&format!(
                "The {} {} of **{}**, population {}.\n\n",
                p.species, p.noun, p.name, p.population
            ));
            for line in &p.culture_lines {
                doc.push_str(&format!("{line}\n"));
            }
            if !p.culture_lines.is_empty() {
                doc.push('\n');
            }
        }
    }
```

In worldgen: `culture_lines(world, flagship: &VillageInfo) -> Vec<String>` (same two strings, per flagship); `settlement_lines` keeps the total-count line and emits one chief line per species in registry order (`The chief settlement, {name}, holds {pop} souls amid {biome}.` — in two-species worlds prefix with the species: decide the exact two-species wording as `The chief {species} settlement, {name}, holds {pop} souls amid {biome}.` BUT keep the legacy unprefixed string when the world holds exactly one species, so goblin-only output is byte-stable). The context builder assembles `PeopleBlock`s via `flagship_of` + `hornvale_species::registry()` nouns.

- [ ] **Step 4: Verify** — `cargo test -p hornvale-almanac -p hornvale-worldgen`; then regenerate a seed-42 almanac locally (`cargo run -p hornvale -- new --seed 42 --out /tmp/two.json && cargo run -p hornvale -- almanac --world /tmp/two.json | sed -n '/## The Land/,/## The Gods/p'`) and eyeball: goblin block first, kobold block present, roles end with "elders". Paste this transcript in the report.

- [ ] **Step 5: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add windows cli
git commit -m "feat(almanac): the People section speaks of peoples, plural"
```

### Task 8: The superset contract and pin isolation

**Files:**
- Create: `cli/tests/species_identity.rs`
- Test fixtures: `cli/tests/fixtures/pre-species-seed-42-*` (Task 2)

**Interfaces:**
- Consumes: everything above; the fixtures; `hornvale_species` predicate constants.
- Produces: the CI-asserted keystone (spec §8).

- [ ] **Step 1: Write the test**

```rust
//! The superset contract (spec §8): a goblin-pinned world reproduces
//! pre-species main — almanac byte-identical, ledger identical on pre-C1
//! predicates (species predicates and the pin fact set aside).
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

const NEW_PREDICATES: [&str; 9] = [
    hornvale_species::SPECIES_NAME,
    hornvale_species::THREAT_RESPONSE,
    hornvale_species::DELIBERATION_LATENCY,
    hornvale_species::IN_GROUP_RADIUS,
    hornvale_species::TIME_HORIZON,
    hornvale_species::SOCIALITY_MODE,
    hornvale_species::STATUS_BASIS,
    hornvale_species::PEOPLED_BY,
    hornvale_settlement::SETTLEMENT_PIN, // records the --species pin itself
];

#[test]
fn goblin_pinned_seed_42_is_a_superset_of_pre_species_main() {
    let fixture: hornvale_kernel::World = serde_json::from_str(include_str!(
        "fixtures/pre-species-seed-42-world.json"
    ))
    .expect("fixture parses");
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
            ..SettlementPins::default()
        },
    )
    .unwrap();

    // Ledger, restricted to pre-species predicates, is identical.
    let filtered = |w: &hornvale_kernel::World| -> Vec<String> {
        w.ledger
            .all_facts()
            .filter(|f| !NEW_PREDICATES.contains(&f.predicate.as_str()))
            .map(|f| format!("{f:?}"))
            .collect()
    };
    assert_eq!(filtered(&world), filtered(&fixture), "filtered ledgers diverge");

    // Almanac byte-identical.
    let rendered = hornvale_almanac::render(&/* context built exactly as the CLI builds it */world)
        /* use the same path `cargo run -p hornvale -- almanac` uses; if that
           path is a cli-internal fn, move the comparison to a std::process
           invocation of the built binary instead — see Step 2 */;
    let expected = include_str!("fixtures/pre-species-seed-42-almanac.md");
    assert_eq!(rendered, expected, "almanac diverged from the pre-species fixture");
}
```

- [ ] **Step 2: Resolve the almanac-rendering path**

The almanac comparison must go through the SAME code path as `hornvale almanac`. Find it: `grep -n "almanac" cli/src/main.rs`. If the render entry is library-accessible (worldgen/almanac pub fn taking the world), call it directly; if it is CLI-internal, write the world to a temp file and compare `std::process::Command::new(env!("CARGO_BIN_EXE_hornvale"))` output (the `CARGO_BIN_EXE_<name>` env var is available to integration tests of the crate that defines the binary — this test lives in `cli/tests/`, so it is). State in your report which path you used. If `Ledger::all_facts()` does not exist, iterate via whatever full-facts accessor the kernel offers (check `kernel/src/ledger.rs`); if none exists, add a doc-commented `pub fn all_facts(&self) -> impl Iterator<Item = &Fact>` to the kernel in this task (one accessor, no behavior change) and note it.

- [ ] **Step 3: Add the pin-isolation test**

Same file: build seed 42 twice — unpinned, and pinned `--species goblin` — and assert the goblin flagship's name and population are identical in both (goblin draws are per-cell-labeled, so pinning must not shift them for cells goblins win in both worlds — use the goblin flagship of the PINNED world and look up the same cell's settlement in the unpinned world via its `cell-id` fact; if that cell went to kobolds in the unpinned world, fall back to the first cell goblins hold in both). Assert byte-equal name+population for the compared cell.

- [ ] **Step 4: Run** — `cargo test -p hornvale --test species_identity`; Expected: PASS. If the filtered-ledger assertion fails, diff the first mismatching fact — the likely culprits are entity-id drift (species genesis running before settlements) or a species fact under a legacy predicate; fix the ORDER, never the test.

- [ ] **Step 5: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add cli kernel
git commit -m "test(cli): the superset contract — goblin-pinned worlds reproduce pre-species main"
```

### Task 9: Lab — per-species metrics and the preregistered calibrations

The three hypotheses are already preregistered in the committed spec (§9) — ADR 0016 is satisfied; this task builds the instruments and asserts them.

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `windows/lab/tests/calibration.rs`
- Modify: `windows/lab/Cargo.toml` (+ `hornvale-species` if metrics need the registry)

**Interfaces:**
- Consumes: `flagship_of` (worldgen), `species_of`, culture accessors, `WorldView { world, terrain, climate, .. }`.
- Produces metrics (exact names; all `Absent` when the species has no flagship):
  - `goblin-flagship-roles` / `kobold-flagship-roles` — Text, the committed castes comma-joined
  - `goblin-flagship-population` / `kobold-flagship-population` — Number
  - `goblin-flagship-surplus` / `kobold-flagship-surplus` — Number, RECOMPUTED from providers (fertility(biome_class) × moisture at the flagship cell), the independent column the slave calibration needs
  - `goblin-flagship-coastal` / `kobold-flagship-coastal` — Flag, recomputed from terrain like the existing `flagship-coastal`
  - `goblin-settlement-count` / `kobold-settlement-count` — Number
- Also: repoint the EXISTING `flagship-*` metrics at the goblin flagship explicitly (they currently use `village_info` = first-placed overall, which in a two-species world may be a kobold) — use `flagship_of(world, "goblin")`. Their doc strings gain "goblin flagship" wording. This keeps every Year-1 metric's meaning stable.

- [ ] **Step 1: Failing kinds assertions** — extend the metric-kinds test with one `matches!` per new metric (mirror the Task-3-of-Y2-0 pattern: `Text(_) | Absent`, `Number(_) | Absent`, `Flag(_) | Absent`) and bump the registry-count test 33 → 43. Run `cargo test -p hornvale-lab metrics`; expected FAIL.

- [ ] **Step 2: Implement the metrics.** Pattern per entry (kobold roles shown; the other nine follow it exactly with the obvious substitutions):

```rust
        Metric {
            name: "kobold-flagship-roles",
            doc: "The kobold flagship's committed role ladder, comma-joined, \
                   lowest to highest; Absent if kobolds placed no settlement",
            summary: SummaryKind::Categorical,
            extract: |v| match hornvale_worldgen::flagship_of(&v.world, "kobold") {
                Some(info) => {
                    let castes = hornvale_culture::castes_of(&v.world, info.id);
                    if castes.is_empty() { MetricValue::Absent }
                    else { MetricValue::Text(castes.join(",")) }
                }
                None => MetricValue::Absent,
            },
        },
```

For the surplus metrics, recompute exactly as worldgen does (this is the independence that makes the calibration non-tautological — it re-derives from providers, not from culture's own inputs): look up the flagship's `cell-id` fact, then `fertility(biome_class(v.climate.biome_at(cell))) * v.climate.moisture_at(cell)`, clamped. `biome_class` is worldgen's mapping — if it is not `pub`, make it `pub` in this task (one-line visibility change, noted in the report).

- [ ] **Step 3: The calibrations** (append to `windows/lab/tests/calibration.rs`, same 500-seed drift-study pattern as the five existing):

```rust
#[test]
fn kobold_structures_never_enslave_and_top_out_with_elders() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (kob_i, gob_i) = (idx("kobold-flagship-roles"), idx("goblin-flagship-roles"));
    for row in &result.rows {
        if let MetricValue::Text(roles) = &row.values[kob_i] {
            assert!(!roles.contains("slave"), "seed {}: kobold slavery", row.seed);
            assert!(roles.ends_with("elders"), "seed {}: kobold top rung", row.seed);
        }
        if let MetricValue::Text(roles) = &row.values[gob_i] {
            assert!(roles.ends_with("chief"), "seed {}: goblin top rung", row.seed);
        }
    }
}

#[test]
fn the_slave_rung_is_an_exact_function_of_rank_surplus_and_scale() {
    // Preregistered (spec §9.2): slave ⇔ Rank ∧ surplus > 0.6 ∧ population >
    // 300, checked on goblin rows (Rank) and kobold rows (¬Rank) from
    // independent recomputed columns.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    for species in ["goblin", "kobold"] {
        let (r_i, s_i, p_i) = (
            idx(&format!("{species}-flagship-roles")),
            idx(&format!("{species}-flagship-surplus")),
            idx(&format!("{species}-flagship-population")),
        );
        for row in &result.rows {
            let MetricValue::Text(roles) = &row.values[r_i] else { continue };
            let MetricValue::Number(surplus) = &row.values[s_i] else { continue };
            let MetricValue::Number(pop) = &row.values[p_i] else { continue };
            let expected = species == "goblin" && *surplus > 0.6 && *pop > 300.0;
            assert_eq!(
                roles.split(',').any(|r| r == "slave"),
                expected,
                "seed {}: slave calibration violated ({species}, surplus={surplus}, pop={pop})",
                row.seed
            );
        }
    }
}

#[test]
fn kobold_flagships_are_less_coastal_than_goblin_flagships() {
    // Preregistered directional hypothesis (spec §9.1); Task 10 pins exact
    // counts after measurement.
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let rate = |col: usize| {
        let (mut t, mut n) = (0u32, 0u32);
        for row in &result.rows {
            match row.values[col] {
                MetricValue::Flag(true) => { t += 1; n += 1 }
                MetricValue::Flag(false) => n += 1,
                _ => {}
            }
        }
        f64::from(t) / f64::from(n.max(1))
    };
    let goblin = rate(idx("goblin-flagship-coastal"));
    let kobold = rate(idx("kobold-flagship-coastal"));
    assert!(kobold < goblin, "kobold {kobold:.3} !< goblin {goblin:.3}");
}
```

- [ ] **Step 4: Run** — `cargo test -p hornvale-lab` (the calibration tests build 500 worlds each; minutes). Expected: PASS. The EXISTING `flagships_are_sometimes_inland_and_sometimes_coastal` exact pins (498/2) will FAIL — the default world changed shape (two species). Update that test's pinned counts to the newly measured values IN THIS TASK (measure via the panic trick, verify plausibility: goblin coastal rate should stay ≈99%), with a comment noting the Y2-1 re-pin. Any other drift-study-dependent calibration (belief⇔lock etc.) should still pass — they assert relationships. If one fails, STOP and investigate before re-pinning anything.

- [ ] **Step 5: Full gate, commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add windows/lab windows/worldgen
git commit -m "feat(lab): per-species metrics + the three preregistered Y2-1 calibrations"
```

### Task 10: The re-baseline, once

**Files:**
- Modify (generated): `book/src/gallery/*`, `book/src/reference/*-generated.md`, `book/src/laboratory/generated/census-lands-drift/*`
- Modify: `book/src/laboratory/study-003.md` + new `book/src/laboratory/study-006.md`, `book/src/SUMMARY.md`

**Interfaces:** consumes everything; produces the two-peoples baseline every later study inherits.

- [ ] **Step 1: Regenerate the CI-exact artifact list** (copy the command block verbatim from `.github/workflows/ci.yml` "Artifacts are current" — same list Firm Ground used). Verify: sky sections byte-identical; the People section shows both peoples; `concept-registry-generated.md` gains the species predicates; `stream-manifest-generated.md` gains `hornvale-species` (empty) and the two kobold settlement labels.
- [ ] **Step 2: Byte-stability double-run** — regenerate twice, `git diff --exit-code` clean on the second; paste the transcript.
- [ ] **Step 3: 10k censuses** — re-run `census-of-lands`, `census-of-peoples`, `census-of-faiths` (`--release`); skip skies (upstream of placement).
- [ ] **Step 4: Study 006** — new page "The Census of Peoples II: Two Peoples" following study-005's structure: the preregistered hypotheses (§9, quoted from the spec with its commit as the preregistration timestamp), measured results (kobold vs goblin coastal rates, kobold settlement-count distribution, role-ladder distributions, the slave calibration at 10k), and honest notes (whatever surprises the census turns up — e.g. worlds where kobolds out-place goblins). Update study-003's numbers where the two-species default changed them, with a pointer to 006. SUMMARY entry after Study 005.
- [ ] **Step 5: Full gate + commit**

```bash
cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings
git add book/src studies
git commit -m "chore(artifacts): the two-peoples re-baseline + Study 006"
```

### Task 11: Book close and the final gate

**Files:**
- Modify: `book/src/domains/culture.md`, `book/src/domains/settlement.md` (multi-species tier), `book/src/domains/species.md` (freshness against as-built), `book/src/introduction.md` ("Where things stand" — stale since Campaign 2; Y2-0's review deferred it to "the first Year-2 book sweep", which is this one)
- Create: `book/src/chronicle/campaign-y2-1.md`; Modify: `book/src/SUMMARY.md`

- [ ] **Step 1: Chronicle** — in campaign-5/campaign-y2-0's voice: the substrate, the identity trick, the superset contract, the joint greedy, the measured divergences, the SRD authoring method, what The Eyes inherits (nocturnality banked, per-species pantheons next).
- [ ] **Step 2: Freshness sweep** — `grep -rn "goblin-only\|only species\|one species\|The goblin village" book/src/ --include=*.md | grep -v chronicle | grep -v generated`; fix living pages (culture.md's "role vocabulary stays goblin" note, settlement.md's naming section, the gods page's framing if it says goblins are the world's only people); update introduction.md's status section through Y2-1. Chronicles stay records.
- [ ] **Step 3: `mdbook build book`** — clean; commit `docs(book): Campaign Y2-1 close — chronicle, multi-species chapters, freshness sweep`.
- [ ] **Step 4: Final gate from clean** — full gate + CI-exact regeneration + `git diff --exit-code` + `git status --short` empty.
- [ ] **Step 5: Hand off** — `superpowers:finishing-a-development-branch` (Year-1 pattern: no-ff merge closing the campaign).

## Self-Review Notes

- **Spec coverage:** §1/§6 → Tasks 6–7; §2 principles → Tasks 3 (data/closed vector), 5 (identity), 1+10 (5E model card, preregistration); §3 → Task 3; §4 → Tasks 4+6; §5 → Task 5; §7 → Tasks 3+6 (facts/pins) + 8 (isolation); §8 → Tasks 2+8; §9 → Task 9 (+10 pins); §10 → Tasks 1+11; §11 → Tasks 8–11; §12 honored (no perception, no language, no kobold religion).
- **Known unknowns, handled explicitly:** `Ledger::all_facts` existence (Task 8 Step 2 fallback); `beliefs_of` arity (Task 6 Step 1 note); `AlmanacContext` construction site (Task 7 grep); `build_world` arity (mirror `WorldView::build`); exact-count re-pins measured in-task (9, 10).
- **Type consistency:** `PsychVector`/`SpeciesDef` fields (T3) match T6's consumption; `SuitabilityWeights`/`place_tagged` (T4) match T6; `PsychSummary`/`RoleVocabulary` (T5) match T6's mapping; metric names (T9) match T10's study prose.
- **Deliberate deviations, named:** the sixth calibration checks surplus recomputed from providers (independent column) rather than a committed surplus fact — committing surplus just for calibration would add a predicate to every world (YAGNI; noted for the registry review). `flagship-*` legacy metrics are explicitly repointed at the goblin flagship to preserve Year-1 meaning.
