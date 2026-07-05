# Campaign 1b: The Tier-0 Cascade & Windows — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the kernel its first real consumers: six tier-0 domain crates (astronomy, climate, terrain, settlement, culture, religion), the almanac window, and a `hornvale` CLI with `new` / `almanac` / `repl` / `concepts` — so that `hornvale new --seed 42` creates a world you can interrogate and render.

**Architecture:** Every domain crate depends on `hornvale-kernel` and NOTHING else (Constitution §2.6) — cross-domain information flows only through facts, fields, and phenomena. The CLI crate is the composition root: the only place all domains meet. Windows (almanac) may depend on domains, because they present them.

**Tech Stack:** Rust edition 2024 (toolchain 1.96+). Dependencies remain `serde` + `serde_json` only (kernel); domain crates depend only on `hornvale-kernel`; CLI uses std-only arg parsing (no clap).

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-hornvale-longterm-plan-design.md`. Constitution §2 governs. Trace Protocol §3.1.6: domains never import each other.
- **Dependency rule (enforced via Cargo.toml):** each `domains/*` crate lists exactly one dependency: `hornvale-kernel`. The almanac window may additionally depend on domain crates. The CLI may depend on everything.
- **No new external dependencies anywhere.** No clap, no rustyline, no rand, no chrono. CLI arg parsing and the REPL loop are std-only.
- **No wall-clock, ever.** All time is `WorldTime { day: f64 }`.
- **Determinism:** same seed → byte-identical almanac, byte-identical saved world. All randomness flows from `world.seed` via labeled `derive` chains. No `HashMap` anywhere near ordered or serialized output.
- **Ratified save-format decisions (do not relitigate, per plan 1a self-review):** `Fact.day` stays `Option<f64>`; `PredicateDef.name` duplicates its registry key.
- Every commit compiles, passes `cargo test --workspace`, and is clean under `cargo fmt --check` and `cargo clippy --workspace --all-targets -- -D warnings`.
- Every new crate's `lib.rs` starts with a `//!` doc and `#![warn(missing_docs)]`. Every public item gets a one-line doc comment — **including struct fields and enum variants** (the 1a briefs' samples sometimes lapsed; the constraint governs, and this plan's samples include them — keep them).
- **Naming is deferred** (spec §7): provider names below (`ConstantSun`, `UniformClimate`, `SingleVale`) are descriptive placeholders.

## File Structure

```
Cargo.toml                          — MODIFY: members += domains/*, windows/*, cli
domains/astronomy/Cargo.toml        — hornvale-astronomy (dep: kernel)
domains/astronomy/src/lib.rs        — ConstantSun, SkyReport, register_concepts, CELESTIAL_BODY
domains/climate/Cargo.toml          — hornvale-climate (dep: kernel)
domains/climate/src/lib.rs          — UniformClimate, ClimateReport, register_concepts, AMBIENT
domains/terrain/Cargo.toml          — hornvale-terrain (dep: kernel)
domains/terrain/src/lib.rs          — genesis (the vale), PlaceInfo, places, IS_PLACE/BIOME
domains/settlement/Cargo.toml       — hornvale-settlement (dep: kernel)
domains/settlement/src/lib.rs       — genesis (goblin village, syllable names), VillageInfo, village_info
domains/culture/Cargo.toml          — hornvale-culture (dep: kernel)
domains/culture/src/lib.rs          — genesis (castes), castes_of, HAS_CASTE, CASTES
domains/religion/Cargo.toml         — hornvale-religion (dep: kernel)
domains/religion/src/lib.rs         — genesis (belief from top phenomenon), Belief, beliefs_of, why
windows/almanac/Cargo.toml          — hornvale-almanac (deps: kernel + all six domains)
windows/almanac/src/lib.rs          — AlmanacContext, render
cli/Cargo.toml                      — package hornvale, binary hornvale (deps: everything)
cli/src/main.rs                     — arg parsing, command dispatch
cli/src/world_builder.rs            — register_all, build_world, almanac_context (composition root)
cli/src/repl.rs                     — run(world, input, output): the REPL loop
cli/src/concepts.rs                 — render_concepts(registry) -> String (generated book reference)
cli/tests/exit_criterion.rs         — end-to-end tests against the built binary
kernel/src/ledger.rs                — MODIFY: add iter()
kernel/src/registry.rs              — MODIFY: add predicates(), phenomenon_kinds()
.github/workflows/ci.yml            — MODIFY: regenerate almanac + concepts artifacts, diff-check
book/src/SUMMARY.md                 — MODIFY: gallery + reference entries for generated files
book/src/gallery/almanac-seed-42.md — GENERATED: committed output of `hornvale almanac`
book/src/reference/concept-registry-generated.md — GENERATED: committed output of `hornvale concepts`
book/src/reference/concept-registry.md — MODIFY: prose + include of generated file
```

---

### Task 1: Kernel enumeration APIs

**Files:**
- Modify: `kernel/src/ledger.rs` (add method + test)
- Modify: `kernel/src/registry.rs` (add methods + tests)

**Interfaces:**
- Consumes: existing `Ledger`, `ConceptRegistry`.
- Produces: `Ledger::iter(&self) -> impl Iterator<Item = &Fact>` (commit order); `ConceptRegistry::predicates(&self) -> impl Iterator<Item = &PredicateDef>` (name order); `ConceptRegistry::phenomenon_kinds(&self) -> impl Iterator<Item = (&str, &str)>` (name order, `(kind, doc)`). The almanac, REPL, and concepts dump all rely on these.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` module in `kernel/src/ledger.rs` (it already has `registry()` and `named()` helpers):

```rust
    #[test]
    fn iter_yields_facts_in_commit_order() {
        let r = registry();
        let mut l = Ledger::default();
        let a = named(&mut l, "Zaggrak");
        let b = named(&mut l, "Bolnar");
        l.commit(a, &r).unwrap();
        l.commit(b, &r).unwrap();
        let names: Vec<&Value> = l.iter().map(|f| &f.object).collect();
        assert_eq!(
            names,
            vec![
                &Value::Text("Zaggrak".to_string()),
                &Value::Text("Bolnar".to_string())
            ]
        );
    }
```

Add to the `tests` module in `kernel/src/registry.rs`:

```rust
    #[test]
    fn predicates_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_predicate("zeta", false, "z").unwrap();
        r.register_predicate("alpha", true, "a").unwrap();
        let names: Vec<&str> = r.predicates().map(|p| p.name.as_str()).collect();
        assert_eq!(names, vec!["alpha", "zeta"]);
    }

    #[test]
    fn phenomenon_kinds_iterate_in_name_order() {
        let mut r = ConceptRegistry::default();
        r.register_phenomenon_kind("wind", "moving air").unwrap();
        r.register_phenomenon_kind("aurora", "sky lights").unwrap();
        let kinds: Vec<(&str, &str)> = r.phenomenon_kinds().collect();
        assert_eq!(kinds, vec![("aurora", "sky lights"), ("wind", "moving air")]);
    }
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-kernel iter_yields && cargo test -p hornvale-kernel _iterate_in_name_order`
Expected: compile errors — methods not defined.

- [ ] **Step 3: Implement**

Add to `impl Ledger` in `kernel/src/ledger.rs`:

```rust
    /// Iterate over every committed fact, in commit order.
    pub fn iter(&self) -> impl Iterator<Item = &Fact> {
        self.facts.iter()
    }
```

Add to `impl ConceptRegistry` in `kernel/src/registry.rs`:

```rust
    /// Iterate over registered predicates, in name order.
    pub fn predicates(&self) -> impl Iterator<Item = &PredicateDef> {
        self.predicates.values()
    }

    /// Iterate over registered phenomenon kinds as (kind, doc), in name order.
    pub fn phenomenon_kinds(&self) -> impl Iterator<Item = (&str, &str)> {
        self.phenomenon_kinds
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
    }
```

- [ ] **Step 4: Run the gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass (58 tests: 55 prior + 3 new).

- [ ] **Step 5: Commit**

```bash
git add kernel/
git commit -m "feat(kernel): enumeration APIs for ledger and registry"
```

---

### Task 2: Astronomy domain (tier 0: the constant sun)

**Files:**
- Modify: `Cargo.toml` (workspace members)
- Create: `domains/astronomy/Cargo.toml`
- Create: `domains/astronomy/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `ConceptRegistry`, `RegistryError`, `Phenomenon`, `PhenomenaSource`, `ObserverContext`, `WorldTime`.
- Produces: `CELESTIAL_BODY: &str = "celestial-body"`; `register_concepts(&mut ConceptRegistry) -> Result<(), RegistryError>`; `ConstantSun` (unit struct) with `sky_at(&self, time: WorldTime) -> SkyReport` and `impl PhenomenaSource`; `SkyReport { description: String, bodies: Vec<String> }`. The almanac, REPL, and religion (via phenomena only) build on these.

- [ ] **Step 1: Add workspace members**

In root `Cargo.toml`, change the members line to:

```toml
members = ["kernel", "domains/*"]
```

(Cargo errors on member paths and glob patterns that match nothing, so the
workspace grows as directories appear: Task 8 adds `"windows/*"`, Task 9 adds
`"cli"`.)

`domains/astronomy/Cargo.toml`:

```toml
[package]
name = "hornvale-astronomy"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale astronomy domain: the sky and its bodies."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/astronomy/src/lib.rs`:

```rust
//! Astronomy, tier 0: a sun that never sets. All downstream systems see
//! astronomy only through phenomena — never this crate.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, ObserverContext, PhenomenaSource, Phenomenon, RegistryError, WorldTime,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::EntityId;

    fn ctx(day: f64) -> ObserverContext {
        ObserverContext {
            place: EntityId(1),
            time: WorldTime { day },
        }
    }

    #[test]
    fn the_sky_never_changes() {
        let sun = ConstantSun;
        let a = sun.sky_at(WorldTime { day: 0.0 });
        let b = sun.sky_at(WorldTime { day: 9999.5 });
        assert_eq!(a.description, b.description);
        assert_eq!(a.bodies, b.bodies);
        assert!(a.description.contains("zenith"));
    }

    #[test]
    fn phenomena_are_constant_and_maximally_salient() {
        let sun = ConstantSun;
        let seen = sun.phenomena(&ctx(0.0));
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, CELESTIAL_BODY);
        assert_eq!(seen[0].period_days, None);
        assert_eq!(seen[0].salience, 1.0);
        assert_eq!(seen, sun.phenomena(&ctx(500.25)));
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.phenomenon_kind(CELESTIAL_BODY).is_some());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-astronomy`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Phenomenon kind for bodies visible in the sky.
pub const CELESTIAL_BODY: &str = "celestial-body";

/// Register astronomy's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(CELESTIAL_BODY, "a body visible in the sky")
}

/// Tier-0 astronomy: the sun is always up, fixed at zenith.
pub struct ConstantSun;

/// What the sky looks like at a given moment.
pub struct SkyReport {
    /// Human-readable description of the sky.
    pub description: String,
    /// Names of the visible bodies.
    pub bodies: Vec<String>,
}

impl ConstantSun {
    /// The sky at `_time` — which, at tier 0, never changes.
    pub fn sky_at(&self, _time: WorldTime) -> SkyReport {
        SkyReport {
            description:
                "A golden sun hangs fixed at zenith. It has never been seen to move.".to_string(),
            bodies: vec!["the sun".to_string()],
        }
    }
}

impl PhenomenaSource for ConstantSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: CELESTIAL_BODY.to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }]
    }
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.toml Cargo.lock domains/astronomy/
git commit -m "feat(astronomy): tier-0 constant sun"
```

---

### Task 3: Climate domain (tier 0: uniform mildness)

**Files:**
- Create: `domains/climate/Cargo.toml`
- Create: `domains/climate/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `ConceptRegistry`, `RegistryError`, `Phenomenon`, `PhenomenaSource`, `ObserverContext`, `Position`.
- Produces: `AMBIENT: &str = "ambient"`; `register_concepts(&mut ConceptRegistry) -> Result<(), RegistryError>`; `UniformClimate` with `climate_at(&self, pos: Position) -> ClimateReport` and `impl PhenomenaSource`; `ClimateReport { temperature_c: f64, description: String }`.

- [ ] **Step 1: Create the manifest**

`domains/climate/Cargo.toml`:

```toml
[package]
name = "hornvale-climate"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale climate domain: weather and seasons."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/climate/src/lib.rs`:

```rust
//! Climate, tier 0: mild and temperate everywhere, forever.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, ObserverContext, PhenomenaSource, Phenomenon, Position, RegistryError,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{EntityId, WorldTime};

    #[test]
    fn climate_is_uniform_in_space() {
        let c = UniformClimate;
        let here = c.climate_at(Position { x: 0.0, y: 0.0 });
        let far = c.climate_at(Position { x: 1e6, y: -1e6 });
        assert_eq!(here.temperature_c, far.temperature_c);
        assert_eq!(here.description, far.description);
        assert_eq!(here.temperature_c, 18.0);
    }

    #[test]
    fn climate_contributes_a_low_salience_phenomenon() {
        let c = UniformClimate;
        let seen = c.phenomena(&ObserverContext {
            place: EntityId(1),
            time: WorldTime { day: 3.0 },
        });
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, AMBIENT);
        assert!(seen[0].salience < 0.5);
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.phenomenon_kind(AMBIENT).is_some());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-climate`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Phenomenon kind for pervasive atmospheric conditions.
pub const AMBIENT: &str = "ambient";

/// Register climate's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(AMBIENT, "a pervasive atmospheric condition")
}

/// Tier-0 climate: the same mild air everywhere.
pub struct UniformClimate;

/// Local climate conditions.
pub struct ClimateReport {
    /// Air temperature in degrees Celsius.
    pub temperature_c: f64,
    /// Human-readable description of the conditions.
    pub description: String,
}

impl UniformClimate {
    /// The climate at `_pos` — which, at tier 0, is the same everywhere.
    pub fn climate_at(&self, _pos: Position) -> ClimateReport {
        ClimateReport {
            temperature_c: 18.0,
            description: "Mild and temperate. The air is warm, still, and unchanging.".to_string(),
        }
    }
}

impl PhenomenaSource for UniformClimate {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: AMBIENT.to_string(),
            description: "warm, still, unchanging air".to_string(),
            period_days: None,
            salience: 0.15,
        }]
    }
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock domains/climate/
git commit -m "feat(climate): tier-0 uniform climate"
```

---

### Task 4: Terrain domain (tier 0: one hand-placed vale)

**Files:**
- Create: `domains/terrain/Cargo.toml`
- Create: `domains/terrain/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `World`, `ConceptRegistry`, `RegistryError`, `LedgerError`, `EntityId`, `Fact`, `Value`.
- Produces: `IS_PLACE: &str = "is-place"`, `BIOME: &str = "biome"`; `register_concepts(&mut ConceptRegistry) -> Result<(), RegistryError>`; `genesis(world: &mut World) -> Result<EntityId, LedgerError>` (commits the vale, returns its id); `places(world: &World) -> Vec<PlaceInfo>`; `PlaceInfo { id: EntityId, name: String, biome: String }`. Settlement's genesis takes the vale id; the almanac and REPL list places.

- [ ] **Step 1: Create the manifest**

`domains/terrain/Cargo.toml`:

```toml
[package]
name = "hornvale-terrain"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale terrain domain: places and land."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/terrain/src/lib.rs`:

```rust
//! Terrain, tier 0: one hand-placed vale. Real generation (the region
//! graph, elevation fields) arrives in Campaign 3.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world() -> World {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        w
    }

    #[test]
    fn genesis_commits_one_named_place() {
        let mut w = world();
        let vale = genesis(&mut w).unwrap();
        let all = places(&w);
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].id, vale);
        assert_eq!(all[0].name, "the Vale");
        assert_eq!(all[0].biome, "temperate forest");
    }

    #[test]
    fn genesis_is_idempotent_on_facts() {
        // Re-running genesis on the same world must not contradict; the
        // second entity is distinct but its facts must still commit cleanly.
        let mut w = world();
        genesis(&mut w).unwrap();
        genesis(&mut w).unwrap();
        assert_eq!(places(&w).len(), 2);
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.predicate(IS_PLACE).is_some());
        assert!(r.predicate(BIOME).is_some());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-terrain`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Predicate marking an entity as a traversable place.
pub const IS_PLACE: &str = "is-place";
/// Predicate giving a place's biome.
pub const BIOME: &str = "biome";

/// Register terrain's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_PLACE, true, "subject is a traversable place")?;
    registry.register_predicate(BIOME, true, "biome of a place")
}

/// A place as terrain knows it.
pub struct PlaceInfo {
    /// The place's entity id.
    pub id: EntityId,
    /// The place's canonical name.
    pub name: String,
    /// The place's biome.
    pub biome: String,
}

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "terrain".to_string(),
    }
}

/// Tier-0 genesis: commit one hand-placed vale; return its entity id.
pub fn genesis(world: &mut World) -> Result<EntityId, LedgerError> {
    let vale = world.ledger.mint_entity();
    world.ledger.commit(
        fact(vale, "name", Value::Text("the Vale".to_string())),
        &world.registry,
    )?;
    world
        .ledger
        .commit(fact(vale, IS_PLACE, Value::Flag(true)), &world.registry)?;
    world.ledger.commit(
        fact(vale, BIOME, Value::Text("temperate forest".to_string())),
        &world.registry,
    )?;
    Ok(vale)
}

/// Every known place, in commit order.
pub fn places(world: &World) -> Vec<PlaceInfo> {
    world
        .ledger
        .find(IS_PLACE)
        .map(|f| f.subject)
        .map(|id| PlaceInfo {
            id,
            name: text_of(world, id, "name").unwrap_or_else(|| format!("place {}", id.0)),
            biome: text_of(world, id, BIOME).unwrap_or_else(|| "unknown".to_string()),
        })
        .collect()
}

fn text_of(world: &World, subject: EntityId, predicate: &str) -> Option<String> {
    match world.ledger.value_of(subject, predicate) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

Note: `genesis_is_idempotent_on_facts` requires the *name* fact not to
contradict — two entities named "the Vale" is fine because `name` is
functional per subject, and the subjects differ. If the test fails on
contradiction, the bug is in the test's expectation, not the ledger.

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock domains/terrain/
git commit -m "feat(terrain): tier-0 hand-placed vale"
```

---

### Task 5: Settlement domain (tier 0: the goblin village)

**Files:**
- Create: `domains/settlement/Cargo.toml`
- Create: `domains/settlement/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `World`, `Stream`, `choose_consistent`, `ConceptRegistry`, `RegistryError`, `LedgerError`, `EntityId`, `Fact`, `Value`.
- Produces: `IS_SETTLEMENT: &str = "is-settlement"`, `LOCATED_IN: &str = "located-in"`, `POPULATION: &str = "population"`; `register_concepts(...)`; `genesis(world: &mut World, home: EntityId) -> Result<EntityId, LedgerError>`; `village_info(world: &World) -> Option<VillageInfo>`; `VillageInfo { id: EntityId, name: String, population: u32, located_in: Option<EntityId> }`. Culture and religion geneses take the village id; almanac/REPL display `VillageInfo`.

- [ ] **Step 1: Create the manifest**

`domains/settlement/Cargo.toml`:

```toml
[package]
name = "hornvale-settlement"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale settlement domain: villages and their people."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/settlement/src/lib.rs`:

```rust
//! Settlement, tier 0: one goblin village with a generated name and
//! population, placed in a home terrain entity.
#![warn(missing_docs)]

use hornvale_kernel::{
    choose_consistent, ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Stream,
    Value, World,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let home = w.ledger.mint_entity();
        (w, home)
    }

    #[test]
    fn genesis_produces_a_named_populated_village() {
        let (mut w, home) = world(42);
        let village = genesis(&mut w, home).unwrap();
        let info = village_info(&w).expect("village exists");
        assert_eq!(info.id, village);
        assert!(!info.name.is_empty());
        assert!(info.name.chars().next().unwrap().is_uppercase());
        assert!((40..=80).contains(&info.population));
        assert_eq!(info.located_in, Some(home));
    }

    #[test]
    fn genesis_is_deterministic() {
        let (mut a, home_a) = world(7);
        let (mut b, home_b) = world(7);
        genesis(&mut a, home_a).unwrap();
        genesis(&mut b, home_b).unwrap();
        let ia = village_info(&a).unwrap();
        let ib = village_info(&b).unwrap();
        assert_eq!(ia.name, ib.name);
        assert_eq!(ia.population, ib.population);
    }

    #[test]
    fn names_vary_across_seeds() {
        let names: Vec<String> = (1..=8)
            .map(|s| {
                let (mut w, home) = world(s);
                genesis(&mut w, home).unwrap();
                village_info(&w).unwrap().name
            })
            .collect();
        assert!(names.windows(2).any(|p| p[0] != p[1]));
    }

    #[test]
    fn no_village_means_none() {
        let (w, _home) = world(1);
        assert!(village_info(&w).is_none());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-settlement`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Predicate marking an entity as a settlement.
pub const IS_SETTLEMENT: &str = "is-settlement";
/// Predicate relating a settlement to the place containing it.
pub const LOCATED_IN: &str = "located-in";
/// Predicate giving a settlement's population.
pub const POPULATION: &str = "population";

const SYLLABLES: [&str; 10] = [
    "zag", "gru", "mok", "nar", "bol", "ish", "rak", "ug", "tor", "gna",
];

/// Register settlement's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_SETTLEMENT, true, "subject is a settlement")?;
    registry.register_predicate(LOCATED_IN, false, "spatial containment")?;
    registry.register_predicate(POPULATION, true, "population of a settlement")
}

/// A settlement as this domain knows it.
pub struct VillageInfo {
    /// The settlement's entity id.
    pub id: EntityId,
    /// The settlement's canonical name.
    pub name: String,
    /// How many live there.
    pub population: u32,
    /// The entity containing this settlement, if recorded.
    pub located_in: Option<EntityId>,
}

/// Generate candidate goblin names: 2-3 syllables, capitalized.
fn candidate_names(stream: &mut Stream) -> Vec<String> {
    (0..4)
        .map(|_| {
            let count = stream.range_u32(2, 3);
            let raw: String = (0..count)
                .map(|_| *stream.pick(&SYLLABLES).expect("SYLLABLES is non-empty"))
                .collect();
            format!("{}{}", raw[..1].to_uppercase(), &raw[1..])
        })
        .collect()
}

fn fact(subject: EntityId, predicate: &str, object: Value, home: EntityId) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: Some(home),
        day: Some(0.0),
        provenance: "settlement".to_string(),
    }
}

/// Tier-0 genesis: commit one goblin village in `home`; return its id.
pub fn genesis(world: &mut World, home: EntityId) -> Result<EntityId, LedgerError> {
    let village = world.ledger.mint_entity();

    let candidates = candidate_names(&mut world.seed.derive("settlement").derive("name").stream());
    let name_fact =
        |n: &String| fact(village, "name", Value::Text(n.clone()), home);
    let mut pick_stream = world.seed.derive("settlement").derive("name-pick").stream();
    let idx = choose_consistent(
        &mut pick_stream,
        &world.ledger,
        &world.registry,
        &candidates,
        name_fact,
    )
    .expect("tier 0: at least one of four fresh names must survive an early-world ledger");
    world
        .ledger
        .commit(name_fact(&candidates[idx]), &world.registry)?;

    let population = world
        .seed
        .derive("settlement")
        .derive("population")
        .stream()
        .range_u32(40, 80);
    world.ledger.commit(
        fact(village, IS_SETTLEMENT, Value::Flag(true), home),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(village, LOCATED_IN, Value::Entity(home), home),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(village, POPULATION, Value::Number(f64::from(population)), home),
        &world.registry,
    )?;
    Ok(village)
}

/// The first settlement in the world, if any.
pub fn village_info(world: &World) -> Option<VillageInfo> {
    let id = world.ledger.find(IS_SETTLEMENT).next()?.subject;
    let name = match world.ledger.value_of(id, "name") {
        Some(Value::Text(t)) => t.clone(),
        _ => format!("settlement {}", id.0),
    };
    let population = match world.ledger.value_of(id, POPULATION) {
        Some(Value::Number(n)) => *n as u32,
        _ => 0,
    };
    let located_in = match world.ledger.value_of(id, LOCATED_IN) {
        Some(Value::Entity(e)) => Some(*e),
        _ => None,
    };
    Some(VillageInfo {
        id,
        name,
        population,
        located_in,
    })
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock domains/settlement/
git commit -m "feat(settlement): tier-0 goblin village with generated name and population"
```

---

### Task 6: Culture domain (tier 0: castes)

**Files:**
- Create: `domains/culture/Cargo.toml`
- Create: `domains/culture/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `World`, `ConceptRegistry`, `RegistryError`, `LedgerError`, `EntityId`, `Fact`, `Value`.
- Produces: `HAS_CASTE: &str = "has-caste"`; `CASTES: [&str; 5]` = slave/fighter/cook/shaman/chief; `register_concepts(...)`; `genesis(world: &mut World, village: EntityId) -> Result<(), LedgerError>`; `castes_of(world: &World, village: EntityId) -> Vec<String>`.

- [ ] **Step 1: Create the manifest**

`domains/culture/Cargo.toml`:

```toml
[package]
name = "hornvale-culture"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale culture domain: social structure."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/culture/src/lib.rs`:

```rust
//! Culture, tier 0: a fixed caste structure for the goblin village,
//! straight from the vision book's goblin village chapter.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, RegistryError, Value, World,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    #[test]
    fn genesis_gives_the_village_all_five_castes_in_order() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        genesis(&mut w, village).unwrap();
        assert_eq!(castes_of(&w, village), CASTES.to_vec());
    }

    #[test]
    fn other_entities_have_no_castes() {
        let mut w = World::new(Seed(42));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        let other = w.ledger.mint_entity();
        genesis(&mut w, village).unwrap();
        assert!(castes_of(&w, other).is_empty());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-culture`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Predicate relating a settlement to a caste present in it.
pub const HAS_CASTE: &str = "has-caste";

/// The tier-0 caste structure, lowest to highest.
pub const CASTES: [&str; 5] = ["slave", "fighter", "cook", "shaman", "chief"];

/// Register culture's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(HAS_CASTE, false, "a caste present in a settlement")
}

/// Tier-0 genesis: give the village the fixed caste structure.
pub fn genesis(world: &mut World, village: EntityId) -> Result<(), LedgerError> {
    for caste in CASTES {
        world.ledger.commit(
            Fact {
                subject: village,
                predicate: HAS_CASTE.to_string(),
                object: Value::Text(caste.to_string()),
                place: None,
                day: Some(0.0),
                provenance: "culture".to_string(),
            },
            &world.registry,
        )?;
    }
    Ok(())
}

/// The castes of `village`, in commit order.
pub fn castes_of(world: &World, village: EntityId) -> Vec<String> {
    world
        .ledger
        .facts_about(village)
        .filter(|f| f.predicate == HAS_CASTE)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect()
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock domains/culture/
git commit -m "feat(culture): tier-0 caste structure"
```

---

### Task 7: Religion domain (tier 0: belief from the sky)

**Files:**
- Create: `domains/religion/Cargo.toml`
- Create: `domains/religion/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: kernel `World`, `ConceptRegistry`, `RegistryError`, `LedgerError`, `EntityId`, `Fact`, `Value`, `Phenomenon`.
- Produces: `IS_BELIEF`, `HELD_BY`, `TENET`, `DERIVED_FROM_PHENOMENON` predicate consts; `register_concepts(...)`; `genesis(world: &mut World, village: EntityId, phenomena: &[Phenomenon]) -> Result<Option<EntityId>, LedgerError>`; `beliefs_of(world: &World) -> Vec<Belief>`; `why(world: &World, belief: EntityId) -> Option<String>`; `Belief { id: EntityId, tenet: String, source_kind: String }`.
- **This is the trace-protocol proof:** religion consumes ONLY `Phenomenon` values — it must not import or reference any other domain crate.

- [ ] **Step 1: Create the manifest**

`domains/religion/Cargo.toml`:

```toml
[package]
name = "hornvale-religion"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale religion domain: belief from observed phenomena."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

- [ ] **Step 2: Write the failing tests**

`domains/religion/src/lib.rs`:

```rust
//! Religion, tier 0: mythologize the most salient phenomenon. This crate
//! never learns what produced a phenomenon — that ignorance is the trace
//! protocol working (spec §3.1.6).
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, Phenomenon, RegistryError, Value, World,
};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Seed;

    fn world(seed: u64) -> (World, EntityId) {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let village = w.ledger.mint_entity();
        (w, village)
    }

    fn eternal_sun() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }
    }

    fn monthly_wanderer() -> Phenomenon {
        Phenomenon {
            kind: "celestial-body".to_string(),
            description: "a pale wanderer".to_string(),
            period_days: Some(30.0),
            salience: 0.8,
        }
    }

    #[test]
    fn an_eternal_phenomenon_yields_an_eternal_tenet() {
        let (mut w, village) = world(42);
        let belief = genesis(&mut w, village, &[eternal_sun()]).unwrap().unwrap();
        let all = beliefs_of(&w);
        assert_eq!(all.len(), 1);
        assert_eq!(all[0].id, belief);
        assert_eq!(all[0].source_kind, "celestial-body");
        assert!(all[0].tenet.contains("never")); // eternal template speaks of changelessness
    }

    #[test]
    fn a_periodic_phenomenon_yields_a_cyclic_tenet() {
        let (mut w, village) = world(42);
        genesis(&mut w, village, &[monthly_wanderer()]).unwrap();
        let tenet = &beliefs_of(&w)[0].tenet;
        assert!(tenet.contains("30"));
        assert!(tenet.contains("return"));
    }

    #[test]
    fn no_phenomena_means_no_religion_yet() {
        let (mut w, village) = world(42);
        assert!(genesis(&mut w, village, &[]).unwrap().is_none());
        assert!(beliefs_of(&w).is_empty());
    }

    #[test]
    fn genesis_is_deterministic_and_seed_sensitive() {
        let tenet_for = |seed| {
            let (mut w, village) = world(seed);
            genesis(&mut w, village, &[eternal_sun()]).unwrap();
            beliefs_of(&w)[0].tenet.clone()
        };
        assert_eq!(tenet_for(7), tenet_for(7));
        let tenets: Vec<String> = (1..=8).map(tenet_for).collect();
        assert!(tenets.windows(2).any(|p| p[0] != p[1]));
    }

    #[test]
    fn why_names_the_source_phenomenon() {
        let (mut w, village) = world(42);
        let belief = genesis(&mut w, village, &[eternal_sun()]).unwrap().unwrap();
        let explanation = why(&w, belief).unwrap();
        assert!(explanation.contains("celestial-body"));
        assert!(explanation.contains("religion"));
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-religion`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Predicate marking an entity as a belief.
pub const IS_BELIEF: &str = "is-belief";
/// Predicate relating a belief to a community that holds it.
pub const HELD_BY: &str = "held-by";
/// Predicate giving a belief's tenet text.
pub const TENET: &str = "tenet";
/// Predicate recording which phenomenon kind a belief mythologizes.
pub const DERIVED_FROM_PHENOMENON: &str = "derived-from-phenomenon";

const ETERNAL_EPITHETS: [&str; 3] = ["the Unblinking Eye", "the Ever-Flame", "the Gold Warden"];
const CYCLIC_EPITHETS: [&str; 3] = ["the Returning One", "the Tidewalker", "the Promised Lamp"];

/// Register religion's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_predicate(IS_BELIEF, true, "subject is a belief")?;
    registry.register_predicate(HELD_BY, false, "a community holding a belief")?;
    registry.register_predicate(TENET, true, "the tenet text of a belief")?;
    registry.register_predicate(
        DERIVED_FROM_PHENOMENON,
        true,
        "phenomenon kind a belief mythologizes",
    )
}

/// A belief as this domain knows it.
pub struct Belief {
    /// The belief's entity id.
    pub id: EntityId,
    /// The belief's tenet text.
    pub tenet: String,
    /// The phenomenon kind it mythologizes.
    pub source_kind: String,
}

/// Tier-0 genesis: mythologize the most salient phenomenon (the first in
/// the salience-sorted slice). Returns None if there is nothing to revere.
pub fn genesis(
    world: &mut World,
    village: EntityId,
    phenomena: &[Phenomenon],
) -> Result<Option<EntityId>, LedgerError> {
    let Some(top) = phenomena.first() else {
        return Ok(None);
    };
    let mut stream = world.seed.derive("religion").derive("epithet").stream();
    let tenet = match top.period_days {
        None => {
            let epithet = *stream.pick(&ETERNAL_EPITHETS).expect("non-empty");
            format!(
                "{epithet} is {}; it has never departed and will never blink.",
                top.description
            )
        }
        Some(period) => {
            let epithet = *stream.pick(&CYCLIC_EPITHETS).expect("non-empty");
            format!(
                "{epithet} departs and returns every {period} days; its absences are mourned \
                 and its returns feasted."
            )
        }
    };

    let belief = world.ledger.mint_entity();
    let fact = |predicate: &str, object: Value| Fact {
        subject: belief,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "religion".to_string(),
    };
    world
        .ledger
        .commit(fact(IS_BELIEF, Value::Flag(true)), &world.registry)?;
    world
        .ledger
        .commit(fact(TENET, Value::Text(tenet)), &world.registry)?;
    world
        .ledger
        .commit(fact(HELD_BY, Value::Entity(village)), &world.registry)?;
    world.ledger.commit(
        fact(DERIVED_FROM_PHENOMENON, Value::Text(top.kind.clone())),
        &world.registry,
    )?;
    Ok(Some(belief))
}

/// Every belief in the world, in commit order.
pub fn beliefs_of(world: &World) -> Vec<Belief> {
    world
        .ledger
        .find(IS_BELIEF)
        .map(|f| f.subject)
        .map(|id| Belief {
            id,
            tenet: text_of(world, id, TENET).unwrap_or_default(),
            source_kind: text_of(world, id, DERIVED_FROM_PHENOMENON).unwrap_or_default(),
        })
        .collect()
}

/// Explain a belief from its committed provenance: which phenomenon kind
/// it mythologizes and which system asserted it.
pub fn why(world: &World, belief: EntityId) -> Option<String> {
    let source = text_of(world, belief, DERIVED_FROM_PHENOMENON)?;
    let provenance = world
        .ledger
        .facts_about(belief)
        .find(|f| f.predicate == DERIVED_FROM_PHENOMENON)
        .map(|f| f.provenance.clone())?;
    Some(format!(
        "Derived from the most salient observed phenomenon (kind: {source}); \
         asserted by {provenance}."
    ))
}

fn text_of(world: &World, subject: EntityId, predicate: &str) -> Option<String> {
    match world.ledger.value_of(subject, predicate) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock domains/religion/
git commit -m "feat(religion): tier-0 belief mythologizing the top phenomenon"
```

---

### Task 8: Almanac window

**Files:**
- Create: `windows/almanac/Cargo.toml`
- Create: `windows/almanac/src/lib.rs` (tests inline)

**Interfaces:**
- Consumes: `SkyReport` (astronomy), `ClimateReport` (climate), `PlaceInfo` (terrain), `VillageInfo` (settlement), `Belief` (religion), kernel `Phenomenon`.
- Produces: `AlmanacContext { seed: u64, sky: SkyReport, climate: ClimateReport, phenomena: Vec<Phenomenon>, places: Vec<PlaceInfo>, village: Option<VillageInfo>, castes: Vec<String>, beliefs: Vec<Belief> }`; `render(ctx: &AlmanacContext) -> String` (deterministic markdown). The CLI constructs the context (Task 9) and prints the render (Task 10).

- [ ] **Step 1: Create the manifest**

In root `Cargo.toml`, extend the members line to:

```toml
members = ["kernel", "domains/*", "windows/*"]
```

`windows/almanac/Cargo.toml`:

```toml
[package]
name = "hornvale-almanac"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale almanac window: generated world documents."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-astronomy = { path = "../../domains/astronomy" }
hornvale-climate = { path = "../../domains/climate" }
hornvale-terrain = { path = "../../domains/terrain" }
hornvale-settlement = { path = "../../domains/settlement" }
hornvale-culture = { path = "../../domains/culture" }
hornvale-religion = { path = "../../domains/religion" }
```

- [ ] **Step 2: Write the failing tests**

`windows/almanac/src/lib.rs`:

```rust
//! The almanac window: render a world as a one-page document. Windows may
//! depend on domains (they present them); domains never depend on windows.
#![warn(missing_docs)]

use hornvale_astronomy::SkyReport;
use hornvale_climate::ClimateReport;
use hornvale_kernel::Phenomenon;
use hornvale_religion::Belief;
use hornvale_settlement::VillageInfo;
use hornvale_terrain::PlaceInfo;

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::EntityId;

    fn sample_context() -> AlmanacContext {
        AlmanacContext {
            seed: 42,
            sky: SkyReport {
                description: "A golden sun hangs fixed at zenith.".to_string(),
                bodies: vec!["the sun".to_string()],
            },
            climate: ClimateReport {
                temperature_c: 18.0,
                description: "Mild and temperate.".to_string(),
            },
            phenomena: vec![Phenomenon {
                kind: "celestial-body".to_string(),
                description: "a golden sun fixed at zenith".to_string(),
                period_days: None,
                salience: 1.0,
            }],
            places: vec![PlaceInfo {
                id: EntityId(1),
                name: "the Vale".to_string(),
                biome: "temperate forest".to_string(),
            }],
            village: Some(VillageInfo {
                id: EntityId(2),
                name: "Bolnar".to_string(),
                population: 60,
                located_in: Some(EntityId(1)),
            }),
            castes: vec!["slave".to_string(), "chief".to_string()],
            beliefs: vec![Belief {
                id: EntityId(3),
                tenet: "the Ever-Flame never blinks.".to_string(),
                source_kind: "celestial-body".to_string(),
            }],
        }
    }

    #[test]
    fn render_contains_every_section_and_datum() {
        let doc = render(&sample_context());
        for expected in [
            "# The Almanac of Seed 42",
            "## The Sky",
            "zenith",
            "## The Land",
            "the Vale",
            "temperate forest",
            "## The People",
            "Bolnar",
            "60",
            "slave",
            "chief",
            "## The Gods",
            "Ever-Flame",
            "celestial-body",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn render_is_deterministic() {
        assert_eq!(render(&sample_context()), render(&sample_context()));
    }

    #[test]
    fn empty_world_renders_honestly() {
        let ctx = AlmanacContext {
            village: None,
            castes: vec![],
            beliefs: vec![],
            places: vec![],
            ..sample_context()
        };
        let doc = render(&ctx);
        assert!(doc.contains("No settlements are known."));
        assert!(doc.contains("No beliefs are recorded."));
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale-almanac`
Expected: compile error — items not defined.

- [ ] **Step 4: Implement**

Add above the `tests` module:

```rust
/// Everything the almanac needs, gathered by the composition root.
pub struct AlmanacContext {
    /// The world seed, for the title.
    pub seed: u64,
    /// The sky at genesis.
    pub sky: SkyReport,
    /// The climate at the world's first place.
    pub climate: ClimateReport,
    /// Phenomena salient at the world's first place, salience-ranked.
    pub phenomena: Vec<Phenomenon>,
    /// Known places.
    pub places: Vec<PlaceInfo>,
    /// The settlement, if any.
    pub village: Option<VillageInfo>,
    /// The settlement's castes, lowest to highest.
    pub castes: Vec<String>,
    /// Recorded beliefs.
    pub beliefs: Vec<Belief>,
}

/// Render the one-page world document as markdown. Deterministic: same
/// context, same bytes.
pub fn render(ctx: &AlmanacContext) -> String {
    let mut doc = String::new();
    doc.push_str(&format!("# The Almanac of Seed {}\n\n", ctx.seed));

    doc.push_str("## The Sky\n\n");
    doc.push_str(&format!("{}\n\n", ctx.sky.description));
    doc.push_str(&format!("Visible bodies: {}.\n\n", ctx.sky.bodies.join(", ")));

    doc.push_str("## The Land\n\n");
    if ctx.places.is_empty() {
        doc.push_str("No places are known.\n\n");
    } else {
        for place in &ctx.places {
            doc.push_str(&format!("- **{}** — {}\n", place.name, place.biome));
        }
        doc.push_str(&format!(
            "\n{} ({:.0}°C)\n\n",
            ctx.climate.description, ctx.climate.temperature_c
        ));
    }

    doc.push_str("## The People\n\n");
    match &ctx.village {
        None => doc.push_str("No settlements are known.\n\n"),
        Some(v) => {
            doc.push_str(&format!(
                "The goblin village of **{}**, population {}.\n\n",
                v.name, v.population
            ));
            if !ctx.castes.is_empty() {
                doc.push_str(&format!("Castes, lowest to highest: {}.\n\n", ctx.castes.join(", ")));
            }
        }
    }

    doc.push_str("## The Gods\n\n");
    if ctx.beliefs.is_empty() {
        doc.push_str("No beliefs are recorded.\n\n");
    } else {
        for belief in &ctx.beliefs {
            doc.push_str(&format!(
                "> {}\n>\n> — a belief derived from the phenomenon *{}*\n\n",
                belief.tenet, belief.source_kind
            ));
        }
    }

    doc.push_str("---\n\n*Generated deterministically: this seed always yields this page.*\n");
    doc
}
```

- [ ] **Step 5: Run the gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

```bash
git add Cargo.lock windows/almanac/
git commit -m "feat(almanac): one-page world document renderer"
```

---

### Task 9: CLI scaffold, composition root, and `new`

**Files:**
- Create: `cli/Cargo.toml`
- Create: `cli/src/main.rs`
- Create: `cli/src/world_builder.rs` (tests inline)

**Interfaces:**
- Consumes: every crate above.
- Produces: binary `hornvale` with `new --seed N [--out PATH]`; `world_builder::register_all(&mut ConceptRegistry) -> Result<(), RegistryError>`; `world_builder::build_world(seed: Seed) -> Result<World, BuildError>`; `world_builder::almanac_context(world: &World) -> AlmanacContext`; `world_builder::observed_phenomena(world: &World, day: f64) -> Vec<Phenomenon>`; `BuildError` (wraps registry/ledger errors, implements `Display` + `Error`). Tasks 10–12 add commands that reuse these.

- [ ] **Step 1: Create the manifest and main skeleton**

In root `Cargo.toml`, extend the members line to its final form:

```toml
members = ["kernel", "domains/*", "windows/*", "cli"]
```

`cli/Cargo.toml`:

```toml
[package]
name = "hornvale"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale: an experimental text adventure and computational social science sandbox."

[dependencies]
hornvale-kernel = { path = "../kernel" }
hornvale-astronomy = { path = "../domains/astronomy" }
hornvale-climate = { path = "../domains/climate" }
hornvale-terrain = { path = "../domains/terrain" }
hornvale-settlement = { path = "../domains/settlement" }
hornvale-culture = { path = "../domains/culture" }
hornvale-religion = { path = "../domains/religion" }
hornvale-almanac = { path = "../windows/almanac" }
```

`cli/src/main.rs`:

```rust
//! The hornvale CLI: create worlds, render almanacs, interrogate via REPL.
#![warn(missing_docs)]

mod world_builder;

use hornvale_kernel::{Seed, World};
use std::process::ExitCode;

const USAGE: &str = "\
usage:
  hornvale new --seed <N> [--out <PATH>]   create a world (default out: world.json)
  hornvale almanac [--world <PATH>]        render the almanac (default: world.json)
  hornvale repl [--world <PATH>]           interrogate a world interactively
  hornvale concepts                        dump the concept registry as markdown
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let result = match args.first().map(String::as_str) {
        Some("new") => cmd_new(&args),
        Some("help") | None => {
            print!("{USAGE}");
            Ok(())
        }
        Some(other) => Err(format!("unknown command '{other}'\n{USAGE}")),
    };
    match result {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}

/// Value of `--flag` in args, if present.
fn flag_value<'a>(args: &'a [String], flag: &str) -> Option<&'a str> {
    args.iter()
        .position(|a| a == flag)
        .and_then(|i| args.get(i + 1))
        .map(String::as_str)
}

fn cmd_new(args: &[String]) -> Result<(), String> {
    let seed: u64 = flag_value(args, "--seed")
        .ok_or("new requires --seed <N>")?
        .parse()
        .map_err(|e| format!("--seed must be a u64: {e}"))?;
    let out = flag_value(args, "--out").unwrap_or("world.json");
    let world = world_builder::build_world(Seed(seed)).map_err(|e| e.to_string())?;
    world
        .save(std::path::Path::new(out))
        .map_err(|e| format!("saving {out}: {e}"))?;
    let village = hornvale_settlement::village_info(&world)
        .map(|v| v.name)
        .unwrap_or_else(|| "no settlement".to_string());
    println!(
        "world of seed {seed} written to {out} ({} facts; village: {village})",
        world.ledger.len()
    );
    Ok(())
}

/// Load a world from `--world` (default world.json).
fn load_world(args: &[String]) -> Result<World, String> {
    let path = flag_value(args, "--world").unwrap_or("world.json");
    World::load(std::path::Path::new(path)).map_err(|e| format!("loading {path}: {e}"))
}
```

Note: `load_world` is unused until Task 10 — mark it `#[allow(dead_code)]`
with a `// used from Task 10 (almanac/repl commands)` comment, or simply add
it in Task 10 instead. Prefer adding it now with the allow, so the file's
final shape is stable; remove the allow in Task 10.

- [ ] **Step 2: Write the failing tests**

`cli/src/world_builder.rs`:

```rust
//! The composition root: the only place all domains meet. Wires
//! registrations and geneses in cascade order, and gathers the almanac's
//! context. Domains stay ignorant of each other; this module is where the
//! application composes them (Constitution §2.6).

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::ConstantSun;
use hornvale_climate::UniformClimate;
use hornvale_kernel::{
    observe, ConceptRegistry, LedgerError, ObserverContext, PhenomenaSource, Phenomenon,
    RegistryError, Seed, World, WorldTime,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_world_produces_the_full_cascade() {
        let world = build_world(Seed(42)).unwrap();
        assert_eq!(hornvale_terrain::places(&world).len(), 1);
        let village = hornvale_settlement::village_info(&world).expect("village");
        assert_eq!(
            hornvale_culture::castes_of(&world, village.id).len(),
            hornvale_culture::CASTES.len()
        );
        assert_eq!(hornvale_religion::beliefs_of(&world).len(), 1);
    }

    #[test]
    fn build_world_is_deterministic() {
        let a = build_world(Seed(42)).unwrap().to_json();
        let b = build_world(Seed(42)).unwrap().to_json();
        assert_eq!(a, b);
    }

    #[test]
    fn different_seeds_differ() {
        let worlds: Vec<String> = (1..=4)
            .map(|s| build_world(Seed(s)).unwrap().to_json())
            .collect();
        assert!(worlds.windows(2).any(|w| w[0] != w[1]));
    }

    #[test]
    fn almanac_context_gathers_everything() {
        let world = build_world(Seed(42)).unwrap();
        let ctx = almanac_context(&world);
        assert_eq!(ctx.seed, 42);
        assert!(!ctx.places.is_empty());
        assert!(ctx.village.is_some());
        assert!(!ctx.castes.is_empty());
        assert!(!ctx.beliefs.is_empty());
        assert!(!ctx.phenomena.is_empty());
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p hornvale`
Expected: compile error — functions not defined.

- [ ] **Step 4: Implement the composition root**

Add above the `tests` module in `cli/src/world_builder.rs`:

```rust
/// Errors from building a world.
#[derive(Debug)]
pub enum BuildError {
    /// A concept registration conflicted.
    Registry(RegistryError),
    /// A genesis commit was rejected.
    Ledger(LedgerError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Registry(e) => write!(f, "registry: {e}"),
            BuildError::Ledger(e) => write!(f, "ledger: {e}"),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<RegistryError> for BuildError {
    fn from(e: RegistryError) -> Self {
        BuildError::Registry(e)
    }
}

impl From<LedgerError> for BuildError {
    fn from(e: LedgerError) -> Self {
        BuildError::Ledger(e)
    }
}

/// Register every domain's concepts.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    hornvale_astronomy::register_concepts(registry)?;
    hornvale_climate::register_concepts(registry)?;
    hornvale_terrain::register_concepts(registry)?;
    hornvale_settlement::register_concepts(registry)?;
    hornvale_culture::register_concepts(registry)?;
    hornvale_religion::register_concepts(registry)
}

/// The tier-0 phenomena sources, observed at the world's first place.
pub fn observed_phenomena(world: &World, day: f64) -> Vec<Phenomenon> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Vec::new();
    };
    let sun = ConstantSun;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sun, &climate];
    observe(&sources, &ObserverContext {
        place,
        time: WorldTime { day },
    })
}

/// Build a complete tier-0 world: registrations, then geneses in cascade
/// order (terrain → settlement → culture → religion-from-phenomena).
pub fn build_world(seed: Seed) -> Result<World, BuildError> {
    let mut world = World::new(seed);
    register_all(&mut world.registry)?;
    let vale = hornvale_terrain::genesis(&mut world)?;
    let village = hornvale_settlement::genesis(&mut world, vale)?;
    hornvale_culture::genesis(&mut world, village)?;
    let seen = observed_phenomena(&world, 0.0);
    hornvale_religion::genesis(&mut world, village, &seen)?;
    Ok(world)
}

/// Gather everything the almanac renders, reconstructing the stateless
/// tier-0 providers.
pub fn almanac_context(world: &World) -> AlmanacContext {
    let sun = ConstantSun;
    let climate = UniformClimate;
    let village = hornvale_settlement::village_info(world);
    let castes = village
        .as_ref()
        .map(|v| hornvale_culture::castes_of(world, v.id))
        .unwrap_or_default();
    AlmanacContext {
        seed: world.seed.0,
        sky: sun.sky_at(WorldTime { day: 0.0 }),
        climate: climate.climate_at(hornvale_kernel::Position { x: 0.0, y: 0.0 }),
        phenomena: observed_phenomena(world, 0.0),
        places: hornvale_terrain::places(world),
        village,
        castes,
        beliefs: hornvale_religion::beliefs_of(world),
    }
}
```

- [ ] **Step 5: Run the gate, then the binary**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

Run: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv-world-42.json`
Expected: one line like `world of seed 42 written to /tmp/hv-world-42.json (16 facts; village: <Name>)` — 3 terrain + 4 settlement + 5 culture + 4 religion facts.

- [ ] **Step 6: Commit**

```bash
git add Cargo.lock cli/
git commit -m "feat(cli): composition root and 'new' command — the cascade lives"
```

---

### Task 10: `almanac` and `concepts` commands

**Files:**
- Create: `cli/src/concepts.rs` (tests inline)
- Modify: `cli/src/main.rs` (dispatch two commands; remove the Task 9 allow on `load_world`)

**Interfaces:**
- Consumes: `world_builder::almanac_context`, `hornvale_almanac::render`, kernel registry enumeration (Task 1).
- Produces: `hornvale almanac [--world PATH]` printing the almanac markdown to stdout; `hornvale concepts` printing the registry reference markdown; `concepts::render_concepts(registry: &ConceptRegistry) -> String`. Task 13's CI wiring redirects both into the book.

- [ ] **Step 1: Write the failing tests**

`cli/src/concepts.rs`:

```rust
//! Render the concept registry as the book's generated reference page.

use hornvale_kernel::ConceptRegistry;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world_builder::register_all;

    #[test]
    fn renders_all_registered_concepts() {
        let mut registry = ConceptRegistry::default();
        registry
            .register_predicate("name", true, "canonical name of an entity")
            .unwrap();
        register_all(&mut registry).unwrap();
        let doc = render_concepts(&registry);
        for expected in [
            "<!-- GENERATED FILE",
            "| `name` | yes |",
            "| `is-place` |",
            "| `has-caste` | no |",
            "| `tenet` |",
            "`celestial-body`",
            "`ambient`",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn render_is_deterministic() {
        let mut registry = ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        assert_eq!(render_concepts(&registry), render_concepts(&registry));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale concepts`
Expected: compile error. (Add `mod concepts;` to `cli/src/main.rs` first.)

- [ ] **Step 3: Implement**

Add above the `tests` module in `cli/src/concepts.rs`:

```rust
/// Render the registry as markdown for the book's reference section.
pub fn render_concepts(registry: &ConceptRegistry) -> String {
    let mut doc = String::new();
    doc.push_str("<!-- GENERATED FILE — do not edit. Regenerate with `hornvale concepts`. -->\n\n");
    doc.push_str("### Predicates\n\n");
    doc.push_str("| Predicate | Functional | Meaning |\n|---|---|---|\n");
    for p in registry.predicates() {
        let functional = if p.functional { "yes" } else { "no" };
        doc.push_str(&format!("| `{}` | {} | {} |\n", p.name, functional, p.doc));
    }
    doc.push_str("\n### Phenomenon kinds\n\n");
    let mut any = false;
    doc.push_str("| Kind | Meaning |\n|---|---|\n");
    for (kind, kind_doc) in registry.phenomenon_kinds() {
        any = true;
        doc.push_str(&format!("| `{kind}` | {kind_doc} |\n"));
    }
    if !any {
        doc.push_str("| *(none registered)* | |\n");
    }
    doc
}
```

In `cli/src/main.rs`: add `mod concepts;` below `mod world_builder;`, remove
the `#[allow(dead_code)]` from `load_world`, and extend the dispatch:

```rust
        Some("almanac") => cmd_almanac(&args),
        Some("concepts") => cmd_concepts(),
```

(placed between the `"new"` and `"help"` arms), plus the two functions:

```rust
fn cmd_almanac(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let ctx = world_builder::almanac_context(&world);
    print!("{}", hornvale_almanac::render(&ctx));
    Ok(())
}

fn cmd_concepts() -> Result<(), String> {
    let mut registry = hornvale_kernel::ConceptRegistry::default();
    registry
        .register_predicate("name", true, "canonical name of an entity")
        .map_err(|e| e.to_string())?;
    world_builder::register_all(&mut registry).map_err(|e| e.to_string())?;
    print!("{}", concepts::render_concepts(&registry));
    Ok(())
}
```

- [ ] **Step 4: Run the gate, then the commands**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

Run: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json && cargo run -p hornvale -- almanac --world /tmp/hv42.json`
Expected: the almanac markdown, with all four sections populated.

Run: `cargo run -p hornvale -- concepts | head -8`
Expected: the generated-file comment and the predicates table.

- [ ] **Step 5: Commit**

```bash
git add cli/
git commit -m "feat(cli): almanac and concepts commands"
```

---

### Task 11: The REPL

**Files:**
- Create: `cli/src/repl.rs` (tests inline)
- Modify: `cli/src/main.rs` (dispatch `repl`)

**Interfaces:**
- Consumes: `world_builder::{almanac_context, observed_phenomena}`, domain query functions, kernel `EntityId`.
- Produces: `repl::run(world: &World, input: impl BufRead, output: impl Write) -> std::io::Result<()>` and the `hornvale repl [--world PATH]` command. Commands: `help`, `sky [day]`, `climate`, `places`, `village`, `castes`, `beliefs`, `why <n>`, `phenomena [day]`, `facts <entity-id>`, `quit`.

- [ ] **Step 1: Write the failing tests**

`cli/src/repl.rs`:

```rust
//! The REPL window: interrogate a world line by line. Generic over
//! input/output so tests drive it with buffers.

use hornvale_astronomy::ConstantSun;
use hornvale_climate::UniformClimate;
use hornvale_kernel::{EntityId, Position, Value, World, WorldTime};
use std::io::{BufRead, Write};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::world_builder::build_world;
    use hornvale_kernel::Seed;

    fn drive(commands: &str) -> String {
        let world = build_world(Seed(42)).unwrap();
        let mut out = Vec::new();
        run(&world, commands.as_bytes(), &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn sky_reports_the_constant_sun() {
        assert!(drive("sky\nquit\n").contains("zenith"));
    }

    #[test]
    fn village_and_castes_and_beliefs_report() {
        let out = drive("village\ncastes\nbeliefs\nquit\n");
        assert!(out.contains("population"));
        assert!(out.contains("shaman"));
        assert!(out.contains("1."));
    }

    #[test]
    fn why_explains_belief_one() {
        let out = drive("why 1\nquit\n");
        assert!(out.contains("celestial-body"));
    }

    #[test]
    fn facts_lists_an_entity() {
        let out = drive("facts 2\nquit\n");
        assert!(out.contains("(settlement)") || out.contains("(terrain)"));
    }

    #[test]
    fn unknown_commands_are_reported_not_fatal() {
        let out = drive("dance\nsky\nquit\n");
        assert!(out.contains("unknown command"));
        assert!(out.contains("zenith"));
    }

    #[test]
    fn eof_ends_the_loop_without_quit() {
        assert!(drive("sky\n").contains("zenith"));
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale repl`
Expected: compile error. (Add `mod repl;` to `cli/src/main.rs` first.)

- [ ] **Step 3: Implement**

Add above the `tests` module in `cli/src/repl.rs`:

```rust
const HELP: &str = "\
commands:
  sky [day]        what the sky looks like (default day 0)
  climate          local climate
  places           known places
  village          the settlement
  castes           the settlement's castes
  beliefs          recorded beliefs
  why <n>          why belief n (from `beliefs`) is held
  phenomena [day]  salient phenomena (default day 0)
  facts <id>       every fact about entity id
  quit             leave
";

/// Run the REPL over a world until `quit` or EOF.
pub fn run(world: &World, input: impl BufRead, mut output: impl Write) -> std::io::Result<()> {
    writeln!(output, "hornvale repl — world of seed {} ('help' for commands)", world.seed.0)?;
    for line in input.lines() {
        let line = line?;
        let mut parts = line.split_whitespace();
        let command = parts.next().unwrap_or("");
        let argument = parts.next();
        match command {
            "" => {}
            "quit" | "exit" => break,
            "help" => write!(output, "{HELP}")?,
            "sky" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                let report = ConstantSun.sky_at(WorldTime { day });
                writeln!(output, "{}", report.description)?;
            }
            "climate" => {
                let report = UniformClimate.climate_at(Position { x: 0.0, y: 0.0 });
                writeln!(output, "{} ({:.0}°C)", report.description, report.temperature_c)?;
            }
            "places" => {
                for place in hornvale_terrain::places(world) {
                    writeln!(output, "{} — {} (entity {})", place.name, place.biome, place.id.0)?;
                }
            }
            "village" => match hornvale_settlement::village_info(world) {
                Some(v) => writeln!(
                    output,
                    "{} — population {} (entity {})",
                    v.name, v.population, v.id.0
                )?,
                None => writeln!(output, "no settlement is known")?,
            },
            "castes" => match hornvale_settlement::village_info(world) {
                Some(v) => {
                    let castes = hornvale_culture::castes_of(world, v.id);
                    writeln!(output, "{}", castes.join(", "))?;
                }
                None => writeln!(output, "no settlement is known")?,
            },
            "beliefs" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                if beliefs.is_empty() {
                    writeln!(output, "no beliefs are recorded")?;
                }
                for (i, belief) in beliefs.iter().enumerate() {
                    writeln!(output, "{}. {}", i + 1, belief.tenet)?;
                }
            }
            "why" => {
                let beliefs = hornvale_religion::beliefs_of(world);
                let picked = argument
                    .and_then(|a| a.parse::<usize>().ok())
                    .and_then(|n| beliefs.get(n.checked_sub(1)?));
                match picked {
                    Some(belief) => match hornvale_religion::why(world, belief.id) {
                        Some(reason) => writeln!(output, "{reason}")?,
                        None => writeln!(output, "no recorded derivation")?,
                    },
                    None => writeln!(output, "usage: why <n> — n from `beliefs`")?,
                }
            }
            "phenomena" => {
                let day = argument.and_then(|a| a.parse().ok()).unwrap_or(0.0);
                for p in crate::world_builder::observed_phenomena(world, day) {
                    writeln!(output, "[{:.2}] {} — {}", p.salience, p.kind, p.description)?;
                }
            }
            "facts" => {
                let id = argument.and_then(|a| a.parse::<u64>().ok());
                match id {
                    Some(id) => {
                        for f in world.ledger.facts_about(EntityId(id)) {
                            writeln!(
                                output,
                                "{} = {} ({})",
                                f.predicate,
                                render_value(&f.object),
                                f.provenance
                            )?;
                        }
                    }
                    None => writeln!(output, "usage: facts <entity-id>")?,
                }
            }
            other => writeln!(output, "unknown command '{other}' — try 'help'")?,
        }
    }
    Ok(())
}

fn render_value(value: &Value) -> String {
    match value {
        Value::Text(t) => t.clone(),
        Value::Number(n) => n.to_string(),
        Value::Flag(b) => b.to_string(),
        Value::Entity(e) => format!("entity {}", e.0),
    }
}
```

In `cli/src/main.rs`: add `mod repl;`, a dispatch arm (between `"almanac"`
and `"concepts"`):

```rust
        Some("repl") => cmd_repl(&args),
```

and the command function (note: no `?` in `main` itself — its arms are
`Result<(), String>` values, and `main` returns `ExitCode`):

```rust
fn cmd_repl(args: &[String]) -> Result<(), String> {
    let world = load_world(args)?;
    let stdin = std::io::stdin();
    let stdout = std::io::stdout();
    repl::run(&world, stdin.lock(), stdout.lock()).map_err(|e| e.to_string())
}
```

- [ ] **Step 4: Run the gate, then try it**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

Run: `cargo run -p hornvale -- new --seed 42 --out /tmp/hv42.json && printf 'sky\nvillage\nbeliefs\nwhy 1\nquit\n' | cargo run -p hornvale -- repl --world /tmp/hv42.json`
Expected: sun description, village line, one belief, one derivation.

- [ ] **Step 5: Commit**

```bash
git add cli/
git commit -m "feat(cli): the REPL window"
```

---

### Task 12: Exit-criterion integration suite

**Files:**
- Create: `cli/tests/exit_criterion.rs`

**Interfaces:**
- Consumes: the built `hornvale` binary via `env!("CARGO_BIN_EXE_hornvale")` (a std/Cargo feature; no dev-dependencies).
- Produces: end-to-end proof of the campaign exit criterion.

- [ ] **Step 1: Write the tests (they should pass immediately — this task is the proof, not new behavior)**

`cli/tests/exit_criterion.rs`:

```rust
//! Campaign 1b exit criterion, verbatim from the spec: `hornvale new
//! --seed 42`, interrogate sky/village/belief in the REPL, and `hornvale
//! almanac` emits a one-page world document — deterministically.

use std::path::PathBuf;
use std::process::{Command, Stdio};

fn bin() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hornvale"))
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("hornvale-1b-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn make_world(dir: &std::path::Path, seed: u64) -> PathBuf {
    let path = dir.join(format!("world-{seed}.json"));
    let out = bin()
        .args(["new", "--seed", &seed.to_string(), "--out", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success(), "new failed: {:?}", out);
    path
}

fn almanac_of(path: &std::path::Path) -> String {
    let out = bin()
        .args(["almanac", "--world", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success());
    String::from_utf8(out.stdout).unwrap()
}

#[test]
fn new_creates_a_world_and_reports_the_village() {
    let dir = temp_dir("new");
    let path = make_world(&dir, 42);
    assert!(path.exists());
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn almanac_is_byte_deterministic_and_seed_sensitive() {
    let dir = temp_dir("almanac");
    let w42 = make_world(&dir, 42);
    let w43 = make_world(&dir, 43);
    let a = almanac_of(&w42);
    let b = almanac_of(&w42);
    let c = almanac_of(&w43);
    assert_eq!(a, b, "same seed must yield byte-identical almanacs");
    assert_ne!(a, c, "different seeds must yield different almanacs");
    for section in ["# The Almanac of Seed 42", "## The Sky", "## The Land", "## The People", "## The Gods"] {
        assert!(a.contains(section), "missing section: {section}");
    }
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn repl_answers_sky_village_and_belief() {
    let dir = temp_dir("repl");
    let world = make_world(&dir, 42);
    let mut child = bin()
        .args(["repl", "--world", world.to_str().unwrap()])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    use std::io::Write as _;
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"sky\nvillage\nbeliefs\nwhy 1\nquit\n")
        .unwrap();
    let out = child.wait_with_output().unwrap();
    assert!(out.status.success());
    let stdout = String::from_utf8(out.stdout).unwrap();
    assert!(stdout.contains("zenith"), "sky answered");
    assert!(stdout.contains("population"), "village answered");
    assert!(stdout.contains("1."), "belief listed");
    assert!(stdout.contains("celestial-body"), "why answered");
    std::fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn concepts_dump_contains_every_domain() {
    let out = bin().arg("concepts").output().unwrap();
    assert!(out.status.success());
    let doc = String::from_utf8(out.stdout).unwrap();
    for concept in ["is-place", "is-settlement", "has-caste", "tenet", "celestial-body", "ambient"] {
        assert!(doc.contains(concept), "missing concept: {concept}");
    }
}
```

- [ ] **Step 2: Run the suite**

Run: `cargo test -p hornvale --test exit_criterion`
Expected: 4 tests PASS. If any fails, the defect is in Tasks 9–11 — fix there, not by weakening the test.

- [ ] **Step 3: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all pass.

- [ ] **Step 4: Commit**

```bash
git add cli/tests/
git commit -m "test(cli): campaign 1b exit criterion, end to end"
```

---

### Task 13: Book artifacts and CI wiring

**Files:**
- Modify: `.github/workflows/ci.yml` (artifact regeneration step)
- Create: `book/src/gallery/almanac-seed-42.md` (generated + committed)
- Create: `book/src/reference/concept-registry-generated.md` (generated + committed)
- Modify: `book/src/reference/concept-registry.md` (prose keeps context; tables become the generated include)
- Modify: `book/src/SUMMARY.md` (gallery entry for the almanac)
- Create: `book/src/gallery/almanac.md` (gallery page framing the generated almanac)

**Interfaces:**
- Consumes: `hornvale almanac`, `hornvale concepts` (Task 10).
- Produces: the book's anti-drift loop now covers the real almanac and the registry reference; CI fails if committed copies go stale.

- [ ] **Step 1: Generate and commit the artifacts**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-book-42.json
cargo run -p hornvale -- almanac --world /tmp/hv-book-42.json > book/src/gallery/almanac-seed-42.md
cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
```

- [ ] **Step 2: Frame the almanac in the gallery**

`book/src/gallery/almanac.md`:

```markdown
# The Almanac of Seed 42

Campaign 1b's exit artifact: the first true almanac — the same document
`hornvale almanac` prints, generated from seed 42's world and regenerated in
CI, which fails if this committed copy ever goes stale. When this page
changes, the world changed.

---

{{#include almanac-seed-42.md}}
```

In `book/src/SUMMARY.md`, extend the Gallery section to:

```markdown
# The Gallery

- [First Light (seed 42)](./gallery/first-light.md)
- [The Almanac of Seed 42](./gallery/almanac.md)
```

- [ ] **Step 3: Swap the reference tables for the generated include**

In `book/src/reference/concept-registry.md`, replace everything from the
`> **Honesty note:**` block through the end of the file with:

```markdown
The tables below are **generated** — dumped from the registry of a freshly
built world by `hornvale concepts` and verified fresh by CI. They cannot
drift from the code.

{{#include concept-registry-generated.md}}
```

- [ ] **Step 4: Extend CI's artifact check**

In `.github/workflows/ci.yml`, replace the "Artifacts are current
(determinism check)" step's `run:` block with:

```yaml
        run: |
          cargo run -p hornvale-kernel --example first_light
          cargo run -p hornvale -- new --seed 42 --out /tmp/hv-ci-42.json
          cargo run -p hornvale -- almanac --world /tmp/hv-ci-42.json > book/src/gallery/almanac-seed-42.md
          cargo run -p hornvale -- concepts > book/src/reference/concept-registry-generated.md
          git diff --exit-code book/src/gallery/ book/src/reference/
```

- [ ] **Step 5: Verify locally, build the book, commit**

Run: `mdbook build book && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: book builds (includes resolve), gate passes.

Rerun the three generation commands from Step 1 and `git status` — the
generated files must show **no changes** (regeneration is a no-op on a clean
tree; this is exactly what CI asserts).

```bash
git add book/ .github/workflows/ci.yml
git commit -m "feat(book): generated almanac and concept-registry artifacts, CI drift check extended"
```

---

## Post-plan (not subagent work)

The campaign's *comprehension* deliverables — the Chronicle chapter for 1b,
Domains section chapters, updating the Gallery's First Light footnote, and
the vision-book chapter ritual — are written in-session with Nathan's review
(the comprehension gate), after implementation completes. The concept-registry
end-of-campaign review ("do these names carve the world at its joints?") also
happens then.

## Self-Review Notes

- **Spec coverage:** Campaign 1's remaining items — tier-0 providers for every
  domain (T2–7), REPL (T11), almanac (T8, T10), the `hornvale new --seed 42`
  exit criterion end to end (T9, T12), 1a-deferred enumeration APIs (T1), and
  the book/CI anti-drift extension promised in the book's introduction (T13).
- **Type consistency spot-checks:** `genesis` signatures — terrain
  `(world) -> Result<EntityId, _>`, settlement `(world, home) -> Result<EntityId, _>`,
  culture `(world, village) -> Result<(), _>`, religion
  `(world, village, &[Phenomenon]) -> Result<Option<EntityId>, _>`.
  `AlmanacContext` field names match Task 9's constructor. REPL uses
  `observed_phenomena(world, day)` exactly as Task 9 defines it.
- **Dependency rule audit:** every `domains/*/Cargo.toml` in this plan lists
  only `hornvale-kernel`. The reviewer of each domain task should treat any
  additional dependency as a Critical finding (Constitution §2.6).
- **Known deferrals:** WASM demo widget (post-1b), registry-driven provider
  tiers (when providers gain state), gallery/first-light page prose update
  (post-plan book work).
