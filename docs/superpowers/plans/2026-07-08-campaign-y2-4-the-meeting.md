# The Meeting (Campaign Y2-4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Cash the Year-2 exit criterion — prove that two species differing only in authored vectors yield legibly different cultures from one sky — with a preregistered comparative study suite, a solo-roster null control, a capstone artifact, and the Year-2 book close.

**Architecture:** Add one piece of new machinery — a **study-scoped species roster** threaded through `hornvale-worldgen`'s world build, defaulting to today's shipped `{goblin, kobold}` so every shipped world stays byte-identical. Use it to build **solo-roster** null-control populations (`[goblin]` and `[goblin-twin]`, a goblin clone under a fresh name), which land in identical cells and diverge only through name-salted noise. Everything else is measurement (a committed 500-seed twin census + calibrations), formalization (the suite as a preregistration ledger), and presentation (the capstone page + book close).

**Tech Stack:** Rust (edition 2024), `serde`/`serde_json` only, std-only threading. Crates touched: `domains/species`, `windows/worldgen`, `windows/lab`, `cli` (CI only), plus `book/` and `studies/`.

## Global Constraints

- **Determinism is constitutional.** Same seed + same pins + same roster → byte-identical output. No wall-clock. No `HashMap`/`HashSet` — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorts use `total_cmp`.
- **Shipped path never moves.** A default-roster build (`{goblin, kobold}`) must be byte-identical to its pre-Meeting form. All existing call sites keep their signatures; new behavior is added alongside as new functions.
- **No new dependencies** (allowlist: `serde` + `serde_json`, enforced by `cli/tests/architecture.rs`). Distributional distances are hand-rolled on `std`.
- **Layering** (`cli/tests/architecture.rs`): `kernel → domains/* → windows/* → cli`. The roster lives at the composition root (`windows/worldgen`); no domain depends on another. `domains/species` gains only a roster-taking genesis; it still depends on the kernel alone.
- **Every crate `#![warn(missing_docs)]`** — every new public item, field, and variant gets a one-line doc comment.
- **The gate (every commit passes all three):** `cargo test --workspace` · `cargo fmt --check` · `cargo clippy --workspace --all-targets -- -D warnings`. Run `cargo fmt` as the final step before every commit.
- **Preregistration (ADR 0016):** directional hypotheses committed before the census; exact rates/counts pinned only after measurement, never tuned to pass. A failed threshold is an alarm to surface to the owner, not a number to lower.
- **The book carries no engineering-process vocabulary and no idea-registry IDs** (`cli/tests/docs_consistency.rs` enforces it). `docs/vision` stays out of `book/`.
- **Spec:** `docs/superpowers/specs/2026-07-08-campaign-y2-4-the-meeting-design.md`. Section references below (§N) are to it.

## File Structure

- `domains/species/src/lib.rs` — add `genesis_in(world, roster)`; `genesis` becomes a default-roster wrapper. (Task 1)
- `windows/worldgen/src/lib.rs` — add `default_roster()`, `observed_phenomena_as_in`, `language_of_in`, `build_world_with_roster`; existing public fns become wrappers. (Task 2)
- `windows/lab/src/metrics.rs` — `WorldView` gains a `roster` field + `build_with_roster`; metric helpers resolve species via the view's roster; six new `goblin-twin`/cyclic-share metrics. (Tasks 3, 4)
- `windows/lab/src/roster.rs` *(new)* — `goblin_solo_roster()`, `goblin_twin_solo_roster()`. (Task 4)
- `windows/lab/src/study.rs` — `PinSet` gains `roster: Option<String>`; `pin_sets_parsed` carries it. (Task 5)
- `windows/lab/src/runner.rs` — resolve a pin set's roster name to a `Vec<SpeciesDef>`; build via `WorldView::build_with_roster`. (Task 5)
- `studies/census-of-the-meeting.study.json` *(new)* — 500-seed, two solo pin sets. (Task 6)
- `.github/workflows/ci.yml` — one new `lab run` line. (Task 6)
- `windows/lab/tests/calibration.rs` — a second `LazyLock` census + the two null-control calibrations. (Task 7)
- `book/src/gallery/the-meeting-seed-42.md` *(new)*, `book/src/gallery/the-gods-seed-42.md`, `book/src/SUMMARY.md` — capstone + forward pointer. (Task 8)
- `book/src/domains/species.md`, `book/src/domains/language.md` — model cards. (Task 9)
- `book/src/laboratory/study-009.md` *(new)*, `book/src/laboratory/overview.md`, `book/src/chronicle/*`, freshness sweep. (Task 10)
- Re-baseline + `docs/retrospectives/campaign-y2-4.md`. (Task 11)

---

### Task 1: Roster-taking species genesis

**Files:**
- Modify: `domains/species/src/lib.rs` (the `genesis` fn, ~line 297)
- Test: `domains/species/src/lib.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Produces: `pub fn genesis_in(world: &mut World, roster: &[SpeciesDef]) -> Result<BTreeMap<String, EntityId>, LedgerError>` — mints one entity per roster entry, in slice order, committing its vector facts exactly as `genesis` does today. `pub fn genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError>` becomes a wrapper over `genesis_in` with the shipped registry.

- [ ] **Step 1: Write the failing test** — byte-identity of the wrapper against the roster form.

```rust
// in mod tests
#[test]
fn genesis_in_with_registry_slice_matches_genesis_exactly() {
    let roster: Vec<SpeciesDef> = registry().into_values().collect();
    let mut a = World::new(Seed(42));
    register_concepts(&mut a.registry).unwrap();
    let ids_a = genesis(&mut a).unwrap();

    let mut b = World::new(Seed(42));
    register_concepts(&mut b.registry).unwrap();
    let ids_b = genesis_in(&mut b, &roster).unwrap();

    assert_eq!(ids_a, ids_b, "same ids in same order");
    let fa: Vec<_> = a.ledger.iter().collect();
    let fb: Vec<_> = b.ledger.iter().collect();
    assert_eq!(fa, fb, "genesis_in over the registry slice must be byte-identical to genesis");
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-species genesis_in_with_registry_slice_matches_genesis_exactly`
Expected: FAIL — `cannot find function genesis_in`.

- [ ] **Step 3: Refactor `genesis` into `genesis_in` + wrapper.** Rename the current `genesis` body to `genesis_in` taking `roster: &[SpeciesDef]`, change the loop header from `for (name, def) in registry()` to `for def in roster`, and set `let name = def.name;` at the top of the loop body (every existing `name`/`def` use is unchanged — for the shipped registry `def.name == name`). Add the wrapper:

```rust
/// Mint one entity per species in `roster` (in slice order) and commit its
/// authored vector as facts — the roster-scoped form of [`genesis`]. Species
/// entities carry facts ONLY under this crate's predicates (the superset
/// contract, spec §8, depends on it).
pub fn genesis_in(
    world: &mut World,
    roster: &[SpeciesDef],
) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let mut ids = BTreeMap::new();
    for def in roster {
        let name = def.name;
        // ... the existing body, unchanged, using `name` and `def` ...
    }
    Ok(ids)
}

/// Mint one entity per shipped species (registry order) and commit its
/// authored vector as facts.
pub fn genesis(world: &mut World) -> Result<BTreeMap<String, EntityId>, LedgerError> {
    let roster: Vec<SpeciesDef> = registry().into_values().collect();
    genesis_in(world, &roster)
}
```

- [ ] **Step 4: Run the test to verify it passes**

Run: `cargo test -p hornvale-species`
Expected: PASS — all existing species tests plus the new one.

- [ ] **Step 5: `cargo fmt` and commit**

```bash
cargo fmt
git add domains/species/src/lib.rs
git commit -m "feat(species): roster-taking genesis_in; genesis is its shipped-registry wrapper (Y2-4 Task 1)"
```

---

### Task 2: Roster-aware worldgen build

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (`observed_phenomena_as` ~414, `language_of` ~467, `build_world` ~561)
- Test: `windows/worldgen/src/lib.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `hornvale_species::genesis_in`, `hornvale_species::SpeciesDef` (Task 1).
- Produces:
  - `pub fn default_roster() -> Vec<hornvale_species::SpeciesDef>`
  - `pub fn observed_phenomena_as_in(world: &World, roster: &[SpeciesDef], species: &str) -> Result<Vec<Phenomenon>, BuildError>`
  - `pub fn language_of_in(world: &World, roster: &[SpeciesDef], species: &str) -> hornvale_language::Phonology`
  - `pub fn build_world_with_roster(seed: Seed, pins: &SkyPins, sky: SkyChoice, terrain_pins: &TerrainPins, settlement_pins: &SettlementPins, roster: &[SpeciesDef]) -> Result<World, BuildError>`
  - `build_world`, `observed_phenomena_as`, `language_of` keep their current signatures as default-roster wrappers.

- [ ] **Step 1: Write the failing test** — default roster is byte-identical to the shipped path.

```rust
#[test]
fn build_world_with_default_roster_matches_build_world_byte_for_byte() {
    use hornvale_terrain::TerrainPins;
    let sp = SettlementPins::default();
    for seed in [Seed(7), Seed(42), Seed(1000)] {
        let a = build_world(seed, &SkyPins::default(), SkyChoice::Generated, &TerrainPins::default(), &sp).unwrap();
        let b = build_world_with_roster(seed, &SkyPins::default(), SkyChoice::Generated, &TerrainPins::default(), &sp, &default_roster()).unwrap();
        let fa: Vec<_> = a.ledger.iter().collect();
        let fb: Vec<_> = b.ledger.iter().collect();
        assert_eq!(fa, fb, "seed {seed:?}: default-roster build must equal build_world exactly");
    }
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-worldgen build_world_with_default_roster_matches_build_world_byte_for_byte`
Expected: FAIL — `cannot find function build_world_with_roster`.

- [ ] **Step 3: Add `default_roster` and the roster-aware resolvers.** Insert near the existing functions:

```rust
/// The shipped species roster — the whole authored registry, in key order.
/// The default every shipped verb builds with (spec §3).
pub fn default_roster() -> Vec<hornvale_species::SpeciesDef> {
    hornvale_species::registry().into_values().collect()
}

/// Resolve `species` within `roster` or fail loudly.
fn def_in<'a>(
    roster: &'a [hornvale_species::SpeciesDef],
    species: &str,
) -> Result<&'a hornvale_species::SpeciesDef, BuildError> {
    roster.iter().find(|d| d.name == species).ok_or_else(|| {
        let known: Vec<&str> = roster.iter().map(|d| d.name).collect();
        BuildError::Pins(format!(
            "unknown species '{species}'; roster: {}",
            known.join(", ")
        ))
    })
}
```

Refactor `observed_phenomena_as` so the registry lookup becomes a roster lookup, keeping the wrapper:

```rust
/// The phenomena a species (resolved within `roster`) observes.
pub fn observed_phenomena_as_in(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
) -> Result<Vec<Phenomenon>, BuildError> {
    let def = def_in(roster, species)?;
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let day = observation_time(world, def.perception.activity)?;
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext { place, time: WorldTime { day }, lens: perception_lens(&def.perception) },
    ))
}

/// The phenomena a shipped species observes (default roster).
pub fn observed_phenomena_as(world: &World, species: &str) -> Result<Vec<Phenomenon>, BuildError> {
    observed_phenomena_as_in(world, &default_roster(), species)
}
```

And `language_of` likewise:

```rust
/// Draw `species`' phonology, resolving `species` within `roster`.
pub fn language_of_in(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
) -> hornvale_language::Phonology {
    let def = def_in(roster, species)
        .unwrap_or_else(|e| panic!("language_of_in: {e}"));
    hornvale_language::draw_phonology(&world.seed, species, &envelope_of(&def.articulation))
}

/// Draw a shipped species' phonology (default roster).
pub fn language_of(world: &World, species: &str) -> hornvale_language::Phonology {
    language_of_in(world, &default_roster(), species)
}
```

- [ ] **Step 4: Turn `build_world` into a wrapper over `build_world_with_roster`.** Rename the current `build_world` body to `build_world_with_roster` with the extra `roster: &[SpeciesDef]` parameter, and make these four changes inside it:
  1. Replace the `let all_species = hornvale_species::registry();` block and the `species_set` match so it resolves against `roster`:

```rust
    let species_set: Vec<&hornvale_species::SpeciesDef> = match &settlement_pins.species {
        None => roster.iter().collect(),
        Some(name) => vec![def_in(roster, name)?],
    };
```

  2. Line ~697: `language_of(&world, def.name)` → `language_of_in(&world, roster, def.name)`.
  3. Line ~774: `observed_phenomena_as(&world, def.name)` → `observed_phenomena_as_in(&world, roster, def.name)`.
  4. Line ~788: `hornvale_species::genesis(&mut world)?;` → `hornvale_species::genesis_in(&mut world, roster)?;`.

  Then add the wrapper:

```rust
/// Build a complete world with the shipped species roster.
pub fn build_world(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
) -> Result<World, BuildError> {
    build_world_with_roster(seed, pins, sky, terrain_pins, settlement_pins, &default_roster())
}
```

- [ ] **Step 5: Run the tests**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS — every existing worldgen test plus the new byte-identity test. (If any existing test regressed, the wrapper is not truly transparent — do not adjust the test; fix the wrapper.)

- [ ] **Step 6: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen): build_world_with_roster + roster-aware phenomena/language; shipped build is the default-roster wrapper (Y2-4 Task 2)"
```

---

### Task 3: WorldView carries a roster

**Files:**
- Modify: `windows/lab/src/metrics.rs` (`WorldView` struct + `build`; helpers `pantheon_sig`, `phonotactic_validity`, `epithet_honorific`)
- Test: `windows/lab/src/metrics.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Consumes: `hornvale_worldgen::{default_roster, build_world_with_roster, observed_phenomena_as_in, language_of_in}`, `hornvale_species::SpeciesDef`.
- Produces: `WorldView { …, pub roster: Vec<hornvale_species::SpeciesDef> }`; `WorldView::build(seed, pins)` (unchanged signature, default roster); `pub fn build_with_roster(seed: Seed, pins: &SkyPins, roster: Vec<SpeciesDef>) -> Result<WorldView, BuildError>`.

- [ ] **Step 1: Write the failing test** — a solo goblin-twin view builds and resolves the twin's phonology.

```rust
#[test]
fn build_with_roster_resolves_a_renamed_solo_species() {
    use hornvale_species::SpeciesDef;
    let goblin = hornvale_species::registry()["goblin"];
    let twin = SpeciesDef { name: "goblin-twin", ..goblin };
    let view = WorldView::build_with_roster(Seed(42), &SkyPins::default(), vec![twin]).unwrap();
    // The twin resolves through the view's roster (it is NOT in the global registry).
    let ph = hornvale_worldgen::language_of_in(&view.world, &view.roster, "goblin-twin");
    assert!(!ph.inventory.is_empty(), "twin phonology must draw");
    // And it placed a flagship peopled by the twin's name.
    assert!(flagship_of(&view.world, "goblin-twin").is_some());
}
```

- [ ] **Step 2: Run it to verify it fails**

Run: `cargo test -p hornvale-lab build_with_roster_resolves_a_renamed_solo_species`
Expected: FAIL — `no function build_with_roster`.

- [ ] **Step 3: Add the `roster` field and `build_with_roster`.** In the `WorldView` struct add:

```rust
    /// The species roster this view was built from (default = shipped).
    pub roster: Vec<hornvale_species::SpeciesDef>,
```

Replace `WorldView::build` and add the roster form:

```rust
impl WorldView {
    /// Build a world view with the shipped species roster.
    pub fn build(seed: Seed, pins: &SkyPins) -> Result<WorldView, BuildError> {
        Self::build_with_roster(seed, pins, hornvale_worldgen::default_roster())
    }

    /// Build a world view with an explicit species roster (spec §3).
    pub fn build_with_roster(
        seed: Seed,
        pins: &SkyPins,
        roster: Vec<hornvale_species::SpeciesDef>,
    ) -> Result<WorldView, BuildError> {
        let world = build_world(
            seed,
            pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &hornvale_worldgen::SettlementPins::default(),
        )?;
        // ↑ replace this call with build_world_with_roster(…, &roster)
        let sky = sky_of(&world)?;
        let Sky::Generated(sky) = sky else {
            return Err(BuildError::Pins("expected Generated sky, got Constant".to_string()));
        };
        let terrain = terrain_of(&world)?;
        let globe = hornvale_terrain::summarize(terrain.globe());
        let climate = climate_of(&world)?;
        Ok(WorldView {
            world,
            system: sky.system().clone(),
            calendar: sky.calendar().clone(),
            notes: sky.notes().to_vec(),
            globe,
            terrain,
            climate,
            roster,
        })
    }
}
```

Then change the `build_world(…)` call inside `build_with_roster` to `build_world_with_roster(seed, pins, SkyChoice::Generated, &hornvale_terrain::TerrainPins::default(), &hornvale_worldgen::SettlementPins::default(), &roster)`. Add `build_world_with_roster` to the `use hornvale_worldgen::{…}` import list.

- [ ] **Step 4: Route the three name-resolving helpers through the view's roster.**
  - In `pantheon_sig`: `let seen = hornvale_worldgen::observed_phenomena_as(&v.world, species).ok()?;` → `observed_phenomena_as_in(&v.world, &v.roster, species).ok()?`.
  - In `phonotactic_validity`: `let ph = hornvale_worldgen::language_of(&v.world, species);` → `language_of_in(&v.world, &v.roster, species)`.
  - In `epithet_honorific`: `let ph = hornvale_worldgen::language_of(&v.world, species);` → `language_of_in(&v.world, &v.roster, species)`.
  Add `observed_phenomena_as_in, language_of_in` to the imports.

- [ ] **Step 5: Run the tests**

Run: `cargo test -p hornvale-lab`
Expected: PASS — every existing metric test (they use `WorldView::build`, now the default-roster wrapper) plus the new one.

- [ ] **Step 6: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/lab/src/metrics.rs
git commit -m "feat(lab): WorldView carries a roster; metric helpers resolve species through it (Y2-4 Task 3)"
```

---

### Task 4: Solo rosters + null-control signature metrics

**Files:**
- Create: `windows/lab/src/roster.rs`
- Modify: `windows/lab/src/lib.rs` (`pub mod roster; pub use`), `windows/lab/src/metrics.rs` (six new metrics + count test)
- Test: `windows/lab/src/metrics.rs`

**Interfaces:**
- Produces: `hornvale_lab::{goblin_solo_roster, goblin_twin_solo_roster} -> Vec<SpeciesDef>`; six new registry metrics: `head-deity-domain-goblin-twin`, `pantheon-size-goblin-twin`, `cult-form-goblin-twin`, `name-length-goblin-twin`, `pantheon-cyclic-share-goblin`, `pantheon-cyclic-share-goblin-twin`.

- [ ] **Step 1: Create the roster module.** `windows/lab/src/roster.rs`:

```rust
//! Null-control rosters (spec §3). The twin never ships — it exists only for
//! Lab studies and tests. Two identical-vector species cannot coexist in one
//! world (placement ties break by species order, so the second places nothing),
//! so the null control runs each goblin-vectored species ALONE.

use hornvale_species::SpeciesDef;

/// The shipped goblin, placed alone (`[goblin]`).
pub fn goblin_solo_roster() -> Vec<SpeciesDef> {
    vec![hornvale_species::registry()["goblin"]]
}

/// A clone of goblin under the fresh name `goblin-twin`, placed alone. Identical
/// vectors, independent name-salted streams — the null-control twin (spec §3).
pub fn goblin_twin_solo_roster() -> Vec<SpeciesDef> {
    let goblin = hornvale_species::registry()["goblin"];
    vec![SpeciesDef { name: "goblin-twin", ..goblin }]
}
```

- [ ] **Step 2: Wire the module.** In `windows/lab/src/lib.rs` add `pub mod roster;` and `pub use roster::{goblin_solo_roster, goblin_twin_solo_roster};`.

- [ ] **Step 3: Write the failing test** — solo goblin and solo twin place in the same cell and have equal head-deity domain, differing names.

```rust
#[test]
fn solo_goblin_and_twin_share_placement_and_head_domain_at_seed_42() {
    let g = WorldView::build_with_roster(Seed(42), &SkyPins::default(), crate::goblin_solo_roster()).unwrap();
    let t = WorldView::build_with_roster(Seed(42), &SkyPins::default(), crate::goblin_twin_solo_roster()).unwrap();
    let gf = flagship_of(&g.world, "goblin").unwrap();
    let tf = flagship_of(&t.world, "goblin-twin").unwrap();
    // Identical vectors + no competitor ⇒ identical cell (spec §3).
    let gcell = g.world.ledger.value_of(gf.id, hornvale_settlement::CELL_ID).cloned();
    let tcell = t.world.ledger.value_of(tf.id, hornvale_settlement::CELL_ID).cloned();
    assert_eq!(gcell, tcell, "solo goblin and twin must land in the same cell");
    // Same cell, same sky, same perception ⇒ same head-deity domain.
    let reg = registry();
    let dom = |view: &WorldView, name: &str| match (reg.iter().find(|m| m.name == name).unwrap().extract)(view) {
        MetricValue::Text(s) => s,
        other => panic!("expected domain text, got {other:?}"),
    };
    assert_eq!(dom(&g, "head-deity-domain-goblin"), dom(&t, "head-deity-domain-goblin-twin"));
    // But names differ (independent stream).
    assert_ne!(gf.name, tf.name, "twin names must differ from goblin's");
}
```

- [ ] **Step 4: Run it to verify it fails**

Run: `cargo test -p hornvale-lab solo_goblin_and_twin_share_placement_and_head_domain_at_seed_42`
Expected: FAIL — the `head-deity-domain-goblin-twin` metric does not exist (`unwrap` on `find` panics).

- [ ] **Step 5: Add the six metrics.** Append to the `registry()` vec, before the closing `]`:

```rust
        Metric {
            name: "head-deity-domain-goblin-twin",
            doc: "Venue domain of the goblin-twin flagship's head deity (null control, spec §4); Absent without a goblin-twin pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-goblin-twin",
            doc: "Number of deities in the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Numeric { bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0] },
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-goblin-twin",
            doc: "Cult form of the goblin-twin flagship's pantheon (null control); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "name-length-goblin-twin",
            doc: "Mean character length of every generated name attributed to the goblin-twin (null control); Absent if it produced no names",
            summary: SummaryKind::Numeric { bucket_edges: &[2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0] },
            extract: |v| mean_name_length(v, "goblin-twin"),
        },
        Metric {
            name: "pantheon-cyclic-share-goblin",
            doc: "Fraction of the goblin flagship pantheon's source phenomena that are periodic (the pick_kobold input the null control needs); Absent without a goblin pantheon",
            summary: SummaryKind::Numeric { bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0] },
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-cyclic-share-goblin-twin",
            doc: "Fraction of the goblin-twin flagship pantheon's source phenomena that are periodic (null control); Absent without one",
            summary: SummaryKind::Numeric { bucket_edges: &[0.0, 0.2, 0.4, 0.6, 0.8, 1.0] },
            extract: |v| match pantheon_sig(v, "goblin-twin") {
                Some(s) => MetricValue::Number(s.cyclic_share),
                None => MetricValue::Absent,
            },
        },
```

- [ ] **Step 6: Update the registry-count test.** Change `registry_has_fifty_seven_metrics_after_the_tongues` to:

```rust
    #[test]
    fn registry_has_sixty_three_metrics_after_the_meeting() {
        assert_eq!(registry().len(), 63);
    }
```

- [ ] **Step 7: Run the tests**

Run: `cargo test -p hornvale-lab`
Expected: PASS — the placement/head-domain test and the count test.

- [ ] **Step 8: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/lab/src/roster.rs windows/lab/src/lib.rs windows/lab/src/metrics.rs
git commit -m "feat(lab): solo null-control rosters + six goblin-twin/cyclic-share metrics (Y2-4 Task 4)"
```

---

### Task 5: Per-pin-set roster in the study schema

**Files:**
- Modify: `windows/lab/src/study.rs` (`PinSet`, `pin_sets_parsed`), `windows/lab/src/runner.rs` (`run`, `run_pin_set`, `build_row`, `run_sequential`)
- Test: `windows/lab/src/study.rs`, `windows/lab/src/runner.rs`

**Interfaces:**
- Consumes: `hornvale_worldgen::default_roster`, `hornvale_lab::{goblin_solo_roster, goblin_twin_solo_roster}`, `WorldView::build_with_roster`.
- Produces: `PinSet { label, pins, roster: Option<String> }`; `pin_sets_parsed(&self) -> Result<Vec<(String, SkyPins, Option<String>)>, StudyError>`; `runner::resolve_roster(name: Option<&str>) -> Result<Vec<SpeciesDef>, StudyError>`.

- [ ] **Step 1: Write the failing tests.** In `study.rs` tests — existing studies still deserialize (roster defaults None); in `runner.rs` tests — an unknown roster errors, a known one builds.

```rust
// study.rs
#[test]
fn pinset_roster_defaults_to_none_for_existing_studies() {
    let json = r#"{ "name": "x", "description": "d",
        "seeds": { "from": 0, "count": 1 },
        "pin_sets": [ { "label": "default", "pins": [] } ],
        "metrics": "all" }"#;
    let s: Study = serde_json::from_str(json).unwrap();
    assert_eq!(s.pin_sets[0].roster, None);
}
```

```rust
// runner.rs
#[test]
fn unknown_roster_is_a_loud_error() {
    let study = Study {
        name: "t".into(), description: "d".into(),
        seeds: Seeds { from: 0, count: 1 },
        pin_sets: vec![PinSet { label: "x".into(), pins: vec![], roster: Some("bogus".into()) }],
        metrics: MetricSelection::Named(vec!["star-class".into()]),
    };
    let err = run(&study).unwrap_err();
    assert!(err.message.contains("bogus"), "error must name the bad roster: {}", err.message);
}

#[test]
fn goblin_twin_solo_roster_builds_and_populates_twin_metrics() {
    let study = Study {
        name: "t".into(), description: "d".into(),
        seeds: Seeds { from: 42, count: 1 },
        pin_sets: vec![PinSet { label: "twin".into(), pins: vec![], roster: Some("goblin-twin-solo".into()) }],
        metrics: MetricSelection::Named(vec!["head-deity-domain-goblin-twin".into()]),
    };
    let r = run(&study).unwrap();
    assert!(matches!(r.rows[0].values[0], MetricValue::Text(_)), "twin metric must populate");
}
```

- [ ] **Step 2: Run to verify they fail**

Run: `cargo test -p hornvale-lab pinset_roster_defaults_to_none_for_existing_studies unknown_roster_is_a_loud_error goblin_twin_solo_roster_builds_and_populates_twin_metrics`
Expected: FAIL — `PinSet` has no `roster` field.

- [ ] **Step 3: Add the `roster` field.** In `study.rs`:

```rust
#[derive(Debug, Clone, PartialEq, serde::Deserialize)]
pub struct PinSet {
    /// The label identifying this pin set (e.g., "default", "twin").
    pub label: String,
    /// Pin specifications in "key=value" format.
    pub pins: Vec<String>,
    /// The species roster this pin set builds with; `None` = the shipped
    /// `{goblin, kobold}` roster. Resolved by the runner (spec §5).
    #[serde(default)]
    pub roster: Option<String>,
}
```

Update every `PinSet { … }` literal in existing tests across `study.rs`, `runner.rs`, and `publish.rs` to add `roster: None`. (Search: `grep -rn "PinSet {" windows/lab/src`.)

- [ ] **Step 4: Carry the roster through `pin_sets_parsed`.** Change its return type to `Vec<(String, SkyPins, Option<String>)>`, pushing `(label.clone(), pins, ps.roster.clone())`.

- [ ] **Step 5: Resolve and build in the runner.** Add to `runner.rs`:

```rust
use hornvale_species::SpeciesDef;

/// Resolve a pin set's roster name to a concrete species roster. The closed
/// set the Lab knows (spec §5): `None`/`"default"` = shipped; the two solo
/// null-control rosters otherwise. Unknown ⇒ loud `StudyError`.
fn resolve_roster(name: Option<&str>) -> Result<Vec<SpeciesDef>, StudyError> {
    match name {
        None | Some("default") => Ok(hornvale_worldgen::default_roster()),
        Some("goblin-solo") => Ok(crate::goblin_solo_roster()),
        Some("goblin-twin-solo") => Ok(crate::goblin_twin_solo_roster()),
        Some(other) => Err(StudyError {
            message: format!("unknown roster '{other}'; known: default, goblin-solo, goblin-twin-solo"),
        }),
    }
}
```

Thread the roster name through `run`, `run_pin_set`, `build_row`, and the test-only `run_sequential`: each `for (label, pins) in &pin_sets` becomes `for (label, pins, roster) in &pin_sets`; `run_pin_set`/`build_row` gain a `roster: Option<&str>` parameter. In `build_row`, resolve once and build with it:

```rust
    let roster = resolve_roster(roster)?;
    match WorldView::build_with_roster(Seed(seed_value), pins, roster) {
        // … unchanged Ok/Err arms …
    }
```

(Resolving per row keeps `build_row` a pure function of its inputs — the parallel-work unit; the clone is one or two `Copy` structs.)

- [ ] **Step 6: Run the tests**

Run: `cargo test -p hornvale-lab`
Expected: PASS — new roster tests plus every existing runner/study/publish test (including `parallel_run_matches_sequential`, which proves the roster threading stayed deterministic).

- [ ] **Step 7: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/lab/src/study.rs windows/lab/src/runner.rs windows/lab/src/publish.rs
git commit -m "feat(lab): per-pin-set roster field; runner resolves + builds solo rosters (Y2-4 Task 5)"
```

---

### Task 6: The census-of-the-meeting study + committed artifact + CI

**Files:**
- Create: `studies/census-of-the-meeting.study.json`
- Create (generated, committed): `book/src/laboratory/generated/census-of-the-meeting/`
- Modify: `.github/workflows/ci.yml`

**Interfaces:** Consumes Task 5's roster resolution and Task 4's metrics.

- [ ] **Step 1: Write the study file.** `studies/census-of-the-meeting.study.json`:

```json
{ "name": "census-of-the-meeting",
  "description": "The Census of the Meeting: the solo-roster null control over 500 seeds. Two goblin-vectored species (goblin, goblin-twin) each placed ALONE, landing in identical cells and diverging only through name-salted noise — the population whose distributional indistinguishability and at-chance blind attribution prove Year-2's measured divergence is attributable to the vectors, not stream noise (spec §4).",
  "seeds": { "from": 0, "count": 500 },
  "pin_sets": [
    { "label": "goblin-solo", "pins": [], "roster": "goblin-solo" },
    { "label": "goblin-twin-solo", "pins": [], "roster": "goblin-twin-solo" }
  ],
  "metrics": "all" }
```

- [ ] **Step 2: Run it and publish the committed artifact.**

Run: `cargo run -p hornvale -- lab run studies/census-of-the-meeting.study.json`
Expected: prints `study census-of-the-meeting: 1000 rows, N refusals; summary + M charts published.` and writes `book/src/laboratory/generated/census-of-the-meeting/`.

- [ ] **Step 3: Add the CI drift line.** In `.github/workflows/ci.yml`, in the "Artifacts are current" step, add immediately after the `census-lands-drift` line:

```yaml
          cargo run -p hornvale -- lab run studies/census-of-the-meeting.study.json
```

(The existing `git diff --exit-code … book/src/laboratory/` line now guards this study too.)

- [ ] **Step 4: Verify no drift on a re-run.**

Run: `cargo run -p hornvale -- lab run studies/census-of-the-meeting.study.json && git diff --exit-code book/src/laboratory/generated/census-of-the-meeting/`
Expected: exit 0 (deterministic — a second run reproduces the bytes).

- [ ] **Step 5: Commit**

```bash
git add studies/census-of-the-meeting.study.json book/src/laboratory/generated/census-of-the-meeting/ .github/workflows/ci.yml
git commit -m "feat(lab): census-of-the-meeting — committed 500-seed solo null-control census + CI drift guard (Y2-4 Task 6)"
```

---

### Task 7: The null-control calibrations (both teeth)

**Files:**
- Modify: `windows/lab/tests/calibration.rs`

**Interfaces:** Consumes the committed `census-of-the-meeting` study and Task 4's metric columns.

> **Preregistration note (ADR 0016):** the two claims below are directional and are committed *before* their exact numbers are measured — (a) among decided `pick_kobold` pairs the twin-pick rate is statistically indistinguishable from 0.5, with a high indistinguishable rate; (b) every structural distribution distance falls within the conservative independence bound. Write the assertions with the *direction* first; the exact pinned counts (`assert_eq!`) are filled in from the first green measurement, never tuned to pass. If either direction fails, STOP and report to the owner (spec §2/§4).

- [ ] **Step 1: Add the shared solo census.** At the top of `calibration.rs`, beside `DRIFT`:

```rust
/// The 500-seed solo null-control census (spec §4), run ONCE and shared by both
/// null-control calibrations. A genuinely different population from `DRIFT`
/// (solo rosters, not the shipped `{goblin, kobold}`), so it is its own
/// `LazyLock` — not a re-run of `DRIFT`.
static MEETING: LazyLock<RunResult> = LazyLock::new(|| {
    let study = load_study(Path::new("../../studies/census-of-the-meeting.study.json"))
        .expect("load census-of-the-meeting study");
    run(&study).expect("run census-of-the-meeting study")
});
```

- [ ] **Step 2: Write the at-chance calibration.** Reconstruct the `pick_kobold` decision (its three rules, reimplemented here so the test is independent of the engine) per seed from the two pin sets' signature columns, and assert at-chance + high-indistinguishable.

```rust
#[test]
fn null_control_blind_attribution_is_at_chance() {
    let result = &*MEETING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    // Collect (domain, cyclic_share, size) per seed for each solo pin set.
    let g = collect_sig(result, "goblin-solo",
        idx("head-deity-domain-goblin"), idx("pantheon-cyclic-share-goblin"), idx("pantheon-size-goblin"));
    let t = collect_sig(result, "goblin-twin-solo",
        idx("head-deity-domain-goblin-twin"), idx("pantheon-cyclic-share-goblin-twin"), idx("pantheon-size-goblin-twin"));
    let (mut picks_twin, mut decided, mut indistinguishable, mut pairs) = (0u32, 0u32, 0u32, 0u32);
    for (seed, gs) in &g {
        let Some(ts) = t.get(seed) else { continue };
        pairs += 1;
        match pick_second([gs, ts]) {
            Some(1) => { decided += 1; picks_twin += 1; }
            Some(_) => { decided += 1; }
            None => { indistinguishable += 1; }
        }
    }
    // Direction (preregistered): decisively NOT separable — most pairs
    // indistinguishable, and among decided pairs the twin is picked ~half.
    assert!(pairs > 0, "no attributable solo pairs");
    assert!(indistinguishable as f64 / pairs as f64 > 0.5,
        "expected the null control to be mostly indistinguishable, got {indistinguishable}/{pairs}");
    if decided > 0 {
        let rate = picks_twin as f64 / decided as f64;
        assert!((rate - 0.5).abs() < 0.2, "twin-pick rate {rate:.3} not at chance");
    }
    // Pinned calibration row (fill from the first green measurement):
    // assert_eq!(indistinguishable, __);
    // assert_eq!(decided, __);
    // assert_eq!(picks_twin, __);
}
```

Add the helpers at the bottom of the file:

```rust
/// A solo pantheon's pick_kobold-relevant signature.
struct Sig { domain: String, cyclic_share: f64, size: f64 }

/// Per-seed signatures for one pin set (rows where the pantheon exists).
fn collect_sig(r: &RunResult, pin_set: &str, d: usize, c: usize, s: usize)
    -> std::collections::BTreeMap<u64, Sig> {
    let mut out = std::collections::BTreeMap::new();
    for row in r.rows.iter().filter(|row| row.pin_set == pin_set) {
        if let (MetricValue::Text(domain), MetricValue::Number(cyclic), MetricValue::Number(size)) =
            (&row.values[d], &row.values[c], &row.values[s]) {
            out.insert(row.seed, Sig { domain: domain.clone(), cyclic_share: *cyclic, size: *size });
        }
    }
    out
}

/// The pick_kobold rule (spec §4), reimplemented independently: lunar, then
/// more-cyclic, then larger; None when identical. Returns the index picked.
fn pick_second(pair: [&Sig; 2]) -> Option<usize> {
    match (pair[0].domain == "lunar", pair[1].domain == "lunar") {
        (true, false) => return Some(0),
        (false, true) => return Some(1),
        _ => {}
    }
    if pair[0].cyclic_share != pair[1].cyclic_share {
        return Some(if pair[0].cyclic_share > pair[1].cyclic_share { 0 } else { 1 });
    }
    if pair[0].size != pair[1].size {
        return Some(if pair[0].size > pair[1].size { 0 } else { 1 });
    }
    None
}
```

- [ ] **Step 3: Write the distributional-distance calibration.**

```rust
#[test]
fn null_control_distributions_are_within_the_sampling_bound() {
    let result = &*MEETING;
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    // Categorical: total-variation distance; numeric: standardized mean diff.
    // Bound: the conservative independent-two-sample envelope (spec §4.2). The
    // two solo builds share seed/cell/phenomena ⇒ POSITIVELY correlated ⇒ true
    // distances are smaller than independence predicts, so this bound is safe.
    let cat = |a: &str, b: &str| tv_distance(text_dist(result, "goblin-solo", idx(a)),
                                             text_dist(result, "goblin-twin-solo", idx(b)));
    let num = |a: &str, b: &str| std_mean_diff(nums(result, "goblin-solo", idx(a)),
                                               nums(result, "goblin-twin-solo", idx(b)));
    let head = cat("head-deity-domain-goblin", "head-deity-domain-goblin-twin");
    let cult = cat("cult-form-goblin", "cult-form-goblin-twin");
    let size = num("pantheon-size-goblin", "pantheon-size-goblin-twin");
    let namelen = num("name-length-goblin", "name-length-goblin-twin");
    // Directional (preregistered): all small — the twin is a goblin.
    // n≈480 present rows/side; a 3σ two-sample envelope: TVD < ~0.15, |SMD| < ~0.2.
    assert!(head < 0.15, "head-domain TVD {head:.4} exceeds the bound");
    assert!(cult < 0.15, "cult-form TVD {cult:.4} exceeds the bound");
    assert!(size.abs() < 0.2, "pantheon-size SMD {size:.4} exceeds the bound");
    assert!(namelen.abs() < 0.2, "name-length SMD {namelen:.4} exceeds the bound");
    // Pinned calibration rows (fill from the first green measurement):
    // assert!((head - __).abs() < 1e-9); … etc.
}
```

Add the hand-rolled distance helpers (BTreeMap-based, deterministic):

```rust
/// Empirical categorical distribution of a Text column over a pin set.
fn text_dist(r: &RunResult, pin_set: &str, col: usize) -> std::collections::BTreeMap<String, f64> {
    let mut counts: std::collections::BTreeMap<String, u32> = std::collections::BTreeMap::new();
    let mut n = 0u32;
    for row in r.rows.iter().filter(|row| row.pin_set == pin_set) {
        if let MetricValue::Text(t) = &row.values[col] { *counts.entry(t.clone()).or_default() += 1; n += 1; }
    }
    counts.into_iter().map(|(k, c)| (k, f64::from(c) / f64::from(n.max(1)))).collect()
}

/// Total-variation distance between two categorical distributions.
fn tv_distance(a: std::collections::BTreeMap<String, f64>, b: std::collections::BTreeMap<String, f64>) -> f64 {
    let mut keys: std::collections::BTreeSet<String> = a.keys().cloned().collect();
    keys.extend(b.keys().cloned());
    0.5 * keys.iter().map(|k| (a.get(k).copied().unwrap_or(0.0) - b.get(k).copied().unwrap_or(0.0)).abs()).sum::<f64>()
}

/// Present numeric values of a column over a pin set.
fn nums(r: &RunResult, pin_set: &str, col: usize) -> Vec<f64> {
    r.rows.iter().filter(|row| row.pin_set == pin_set)
        .filter_map(|row| if let MetricValue::Number(n) = row.values[col] { Some(n) } else { None }).collect()
}

/// Standardized mean difference (mean gap in pooled-standard-deviation units).
fn std_mean_diff(a: Vec<f64>, b: Vec<f64>) -> f64 {
    let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len().max(1) as f64;
    let var = |v: &[f64], m: f64| v.iter().map(|x| (x - m).powi(2)).sum::<f64>() / (v.len().max(1) as f64);
    let (ma, mb) = (mean(&a), mean(&b));
    let pooled = ((var(&a, ma) + var(&b, mb)) / 2.0).sqrt();
    if pooled == 0.0 { 0.0 } else { (ma - mb) / pooled }
}
```

- [ ] **Step 4: Run, then pin the measured numbers.**

Run: `cargo test -p hornvale-lab --test calibration null_control`
Expected: PASS on the directional assertions. Read the measured values from a `--nocapture`/`dbg!` pass, then uncomment and fill the pinned `assert_eq!`/exact-distance rows. Re-run to confirm they hold. **If a directional assertion fails, STOP — do not loosen it; report to the owner (spec §2).**

- [ ] **Step 5: `cargo fmt` and commit**

```bash
cargo fmt
git add windows/lab/tests/calibration.rs
git commit -m "test(lab): null-control calibrations — at-chance blind attribution + within-bound distributions (Y2-4 Task 7)"
```

---

### Task 8: The capstone gallery page

**Files:**
- Create: `book/src/gallery/the-meeting-seed-42.md`
- Modify: `book/src/gallery/the-gods-seed-42.md` (forward pointer), `book/src/SUMMARY.md`

**Interfaces:** Quotes the committed `book/src/gallery/almanac-seed-42-sky.md` (the default unpinned seed-42, two shipped-species almanac). No code, no new generation.

- [ ] **Step 1: Confirm the source almanac and gather the `why` reasons.** The capstone quotes verbatim from `book/src/gallery/almanac-seed-42-sky.md` (goblin village + kobold warren, one fixed generated sky). Build it and open the REPL to capture each divergence's recount:

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json
cargo run -p hornvale -- repl --world /tmp/hv42.json   # `why <id>` on each flagship + head deity
```

- [ ] **Step 2: Write `the-meeting-seed-42.md`.** Structure (prose at the-gods altitude — technical, comprehensible without the code; ≤ the-gods page's length):
  - **Framing:** one seed, one fixed sky; the two peoples diverge because their authored vectors differ, and every divergence traces to one of three vectors.
  - **Psychology:** the two flagships (quote the almanac's settlement lines) — placement weights from the psych vector, and the two social ladders (`slave…chief` vs `digger…elders`), recounted by `why` to `status-basis` / `sociality` / `time-horizon`.
  - **Perception:** the two pantheons (quote the "The Gods" blocks) — solar-headed goblin vs lunar-headed kobold, recounted to `activity-cycle` / `sky-attention` / `night-vision`.
  - **Articulation:** the names and the two myth-voices (honorific-dense goblin vs repeated-refrain kobold), recounted to `status-basis` (honorifics) / `sociality` (repetition) and the articulation envelope (mouth-feel).
  - **What this proves:** the same code, two vectors, one sky ⇒ two cultures; and forward to Study 009 for the null control that rules out stream noise.
  - **Cross-links:** `../domains/species.md`, `../domains/perception.md`, `../domains/language.md`, `../domains/religion.md`, `../laboratory/study-009.md`, and the chronicle close.

- [ ] **Step 3: Add the forward pointer.** In `the-gods-seed-42.md`, add one sentence (near the top or the closing cross-links) pointing to `the-meeting-seed-42.md` as the Year-2 capstone this page's two-pantheon demo grew into. Keep the-gods page otherwise intact.

- [ ] **Step 4: Add to nav.** In `book/src/SUMMARY.md`, under `# The Gallery`, add `- [The Meeting of Seed 42](./gallery/the-meeting-seed-42.md)` after the-gods line.

- [ ] **Step 5: Verify the book builds and links resolve.**

Run: `mdbook build book && cargo test -p hornvale --test docs_consistency`
Expected: both PASS (all cross-links resolve; no process vocabulary / idea IDs in the new page).

- [ ] **Step 6: Commit**

```bash
git add book/src/gallery/the-meeting-seed-42.md book/src/gallery/the-gods-seed-42.md book/src/SUMMARY.md
git commit -m "docs(book): The Meeting of Seed 42 — the Year-2 capstone, three vectors on one sky (Y2-4 Task 8)"
```

---

### Task 9: Model cards for species and language

**Files:**
- Modify: `book/src/domains/species.md`, `book/src/domains/language.md`

**Interfaces:** Prose only. Each parameter declared derived / approximated / drawn / authored (metaplan §8, spec §10).

- [ ] **Step 1: Read the two chapters** to place the card where the existing model-card sections live (mirror religion's card, referenced in the Y2-2 retro).

- [ ] **Step 2: Species model card.** In `species.md`, add/complete a model-card section: a table with every psychology (6), perception (3), and articulation (6) dimension, each tagged **authored** (the vast majority — species are authored data) with the two shipped values (goblin/kobold), and note the banked-but-idle dimensions (deliberation latency, crepuscular activity). State plainly that species carry no *drawn* parameters — the only seeded draws downstream are names/populations, which belong to language/settlement.

- [ ] **Step 3: Language model card.** In `language.md`, add/complete a model-card section: phoneme inventory and phonotactics are **drawn** (labeled streams, constrained by the authored articulation envelope); the envelope→`Envelope` and psych→voice/morph mappings are **derived** (1:1 carries / fixed functions — cite `voice_params`, `morph_options`); romanization/IPA are **authored** renderings. Note voice-loudness's banked derivation.

- [ ] **Step 4: Verify book + lint.**

Run: `mdbook build book && cargo test -p hornvale --test docs_consistency`
Expected: PASS (no process vocabulary, no idea-registry IDs).

- [ ] **Step 5: Commit**

```bash
git add book/src/domains/species.md book/src/domains/language.md
git commit -m "docs(book): model cards for species and language — every parameter typed (Y2-4 Task 9)"
```

---

### Task 10: Study 009 page + book close (chronicle, freshness, registry review)

**Files:**
- Create: `book/src/laboratory/study-009.md`
- Modify: `book/src/laboratory/overview.md`, `book/src/SUMMARY.md`, `book/src/chronicle/*` (+ SUMMARY), and any drifted chapters found in the sweep

**Interfaces:** Cites the pinned calibration numbers from Task 7 and the 10k author-time figures (Step 2 below).

- [ ] **Step 1: Write `study-009.md` — the preregistration ledger + null control.** Structure (mirroring study-008's scale/provenance/preregistration notes):
  - Scale/provenance note: 500-seed committed `census-of-the-meeting` (CI-drift-checked) vs the 10k author-time headline run (Step 2).
  - **The preregistration ledger:** a table of every comparative-suite hypothesis (§6) — its preregistered direction and its measured/pinned result, cross-referencing the calibration test that asserts it (from `calibration.rs`) and the earlier study (004/007/008) that measured it at 10k.
  - **The null control:** the solo-roster design (why two identical-vector species can't share a world; why solo isolates stream noise), and the two teeth with their pinned numbers — at-chance/indistinguishable blind attribution, and the within-bound distances — read against the standard roster's decisive 0.875 / perfect-on-mooned separation.

- [ ] **Step 2: Produce the 10k headline figures.** Author-time run (not committed, per precedent), quoting its numbers in the page's headline prose:

```bash
sed 's/"count": 500/"count": 10000/' studies/census-of-the-meeting.study.json > /tmp/meeting-10k.study.json
cargo run -p hornvale -- lab run /tmp/meeting-10k.study.json   # read the summary; do NOT commit /tmp output
git checkout book/src/laboratory/generated/census-of-the-meeting/   # restore the committed 500-seed artifact
```

- [ ] **Step 3: Register the study in nav + overview.** Add `- [Study 009: The Census of the Meeting](./laboratory/study-009.md)` to `SUMMARY.md`; add its row to `overview.md`.

- [ ] **Step 4: Write the Year-2 chronicle close.** Create the chronicle entry for The Meeting (follow the numbering of `book/src/chronicle/16-the-tongues.md` → `17-the-meeting.md`), closing Year 2; add it to `SUMMARY.md`.

- [ ] **Step 5: Freshness sweep.** Check line-by-line and update where prose lags reality: `laboratory/overview.md`, `domains/perception.md`, `domains/species.md`, `domains/language.md`, `domains/religion.md`, and the introduction/cascade overview — anywhere the Year-2 exit criterion, the null control, or "the two peoples" is described. Do not restate; only correct drift.

- [ ] **Step 6: Concept-registry review.** Confirm no new shipped predicates were added (The Meeting adds none): `cargo run -p hornvale -- concepts` unchanged vs committed `book/src/reference/concept-registry-generated.md`. Note the review's outcome in the chronicle.

- [ ] **Step 7: Verify book + lint.**

Run: `mdbook build book && cargo test -p hornvale --test docs_consistency`
Expected: PASS.

- [ ] **Step 8: Commit**

```bash
git add book/src/laboratory/study-009.md book/src/laboratory/overview.md book/src/SUMMARY.md book/src/chronicle/ book/src/domains/ book/src/introduction.md
git commit -m "docs(book): Study 009 + Year-2 chronicle close + freshness sweep + registry review (Y2-4 Task 10)"
```

---

### Task 11: Re-baseline, full gate, retrospective

**Files:**
- Modify (regenerated): `book/src/laboratory/generated/census-lands-drift/` (gains Absent twin columns), `book/src/gallery/*` almanacs (must be byte-identical), `book/src/reference/*`
- Create: `docs/retrospectives/campaign-y2-4.md`

**Interfaces:** None new — this is the single end-of-campaign re-baseline (spec §8, metaplan §9).

- [ ] **Step 1: Regenerate every committed artifact** exactly as CI does (the "Artifacts are current" step in `.github/workflows/ci.yml`), including the two new lines from Task 6. Run each command in that step in order.

- [ ] **Step 2: Verify the re-baseline is *light*.**

Run: `git status --porcelain book/`
Expected: the ONLY changes are (a) `census-lands-drift`'s summary/charts gaining the six new `Absent`/dead twin columns, (b) `census-of-the-meeting` (already committed in Task 6). The gallery almanacs, `reference/`, and every *existing* column of `census-lands-drift` must be **unchanged** — a changed value there means the roster refactor drifted the shipped path (a Task 2 bug). If so, STOP and fix Task 2; do not commit the drift.

- [ ] **Step 3: Run the full gate.**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all PASS, including `census-lands-drift`'s calibrations (unchanged — new columns are Absent, existing calibrations index by name) and the two null-control calibrations.

- [ ] **Step 4: Write the retrospective.** `docs/retrospectives/campaign-y2-4.md` (decision 0020) — process lessons, not product. Name at minimum: the two-slot→solo null-control correction caught at plan time by reading `place_tagged` (spec-vs-reality, corrected in place); the roster-as-wrapper pattern that kept the shipped path byte-identical; and the re-baseline's shape (census-lands-drift gains Absent columns, nothing else drifts).

- [ ] **Step 5: Commit**

```bash
git add book/ docs/retrospectives/campaign-y2-4.md
git commit -m "chore(y2-4): campaign re-baseline + retrospective — The Meeting closes Year 2 (Y2-4 Task 11)"
```

---

## Self-Review

**Spec coverage:** §3 roster mechanism → Tasks 1–2, 4; §4 null control (both teeth) → Tasks 4, 6, 7; §5 schema + Study 009 → Tasks 5, 6; §6 comparative suite ledger → Task 10; §7 capstone + Study 009 page → Tasks 8, 10; §8 determinism/re-baseline → Tasks 2, 11; §9 constitutional → threaded through (no new deps/domains/predicates); §10 book close → Tasks 9, 10; §11 success criteria → all covered; §12 deferred → nothing built. No gaps.

**Placeholder scan:** The only intentionally-deferred values are the *pinned calibration numbers* in Task 7 (filled from first measurement per ADR 0016 — a discipline, not a placeholder) and the *prose* of the book pages in Tasks 8–10 (structure + verification gates given; prose is authored at execution). All code steps carry complete code.

**Type consistency:** `genesis_in(&mut World, &[SpeciesDef])` (T1) is called by `build_world_with_roster` (T2); `default_roster() -> Vec<SpeciesDef>` (T2) feeds `WorldView::build` (T3) and `resolve_roster` (T5); `goblin_solo_roster`/`goblin_twin_solo_roster` (T4) are consumed by `resolve_roster` (T5); the six metric names (T4) are read by the study (T6) and calibrations (T7) by exact string. `pick_second`/`Sig` (T7) mirror the private `pick_kobold`/`PantheonSig` deliberately (independent check). Consistent.
