# Campaign Y2-2: The Eyes — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Salience becomes a function of the observer: a closed three-dimension perception vector per species derives a venue-weighted lens; each species observes at its characteristic hour; religion runs per species-flagship — one seed, one sky, two pantheons (goblin solar-headed, kobold moon-headed), divergence recountable via `why`.

**Architecture:** The kernel gains `Venue` (producer-declared on `Phenomenon`) and `PerceptionLens` (carried by `ObserverContext`; `observe()` applies it, with the identity lens a byte-level no-op). `domains/species` gains the authored `PerceptionVector`. `windows/worldgen` derives the lens, picks the observation moment, and runs religion for every species-flagship; non-goblin epithets draw under species-qualified stream labels. The keystone test: a goblin-pinned seed-42 world reproduces pre-Eyes main as a superset (almanac byte-identical; ledger identical off the three new perception predicates), and the default world's goblin pantheon is unmoved.

**Tech Stack:** Rust edition 2024, `serde`/`serde_json` only, mdbook.

**Spec:** `docs/superpowers/specs/2026-07-07-campaign-y2-2-the-eyes-design.md` (governs; the Year-2 metaplan §6 sits above it).

## Global Constraints

- Full gate on every commit: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`; run `cargo fmt` as the final step before every commit.
- Determinism constitutional: no wall-clock, no `HashMap`/`HashSet` (`BTreeMap`/`BTreeSet`/`Vec` only), float sorts via `total_cmp` with deterministic tie-breaks.
- No new external dependencies. Every new public item, field, and variant gets a one-line doc comment (`#![warn(missing_docs)]` is already on every crate).
- Layering: producers change ONLY by declaring venues (one line per phenomenon). Lens derivation and the characteristic hour live in `windows/worldgen`. Religion learns a stream qualifier, never a species concept.
- Stream labels are permanent: goblins keep `religion/epithet` untouched; kobolds add `religion/kobold/epithet`. No renames, no reordering of existing consumption (ADR 0006).
- Goblin perception baseline: `Diurnal`, night_vision `0.5`, sky_attention `0.5` → lens exactly `(1.0, 1.0, 1.0)`; the identity path in `observe()` performs no arithmetic (no multiply, no clamp, no round). This is what makes Task 7's superset contract hold by construction.
- Kobold perception (authored, Nathan's to retune): `Nocturnal`, night_vision `0.9`, sky_attention `0.8` → lens `(0.52, 1.82, 0.70)`.
- The only new randomness this campaign is the kobold epithet draw. Perception is authored; there are no new pins.
- Preregistration discipline (ADR 0016): directional claims in this plan's calibration tests are written BEFORE the census runs; exact rates are pinned after measurement, never tuned to pass.
- Evidence discipline (campaign-standing, per the Y2-1 retrospective): implementer reports carry verbatim command transcripts; gate claims are void unless the controller independently reruns the gate.
- Work on branch `campaign-y2-2-the-eyes` (worktree per `superpowers:using-git-worktrees`).

---

### Task 1: The perception book chapter (book-driven development — no code)

**Files:**
- Create: `book/src/domains/perception.md`
- Modify: `book/src/SUMMARY.md` (add `- [Perception](./domains/perception.md)` directly after the Species chapter entry)

**Interfaces:**
- Consumes: the spec (read it in full) and the voice/altitude of `book/src/domains/species.md` (read it first — match its register).
- Produces: the chapter later tasks' code must live up to; Task 12 revisits it in the freshness sweep.

- [ ] **Step 1: Write the chapter**

Content (prose at book altitude — technical and mathematical, comprehensible without the code): why salience must be a function of the observer (the frontier's EXP-3 argument: different species see different night skies from identical heavens, and build different religions in the same valley, each empirically correct about what it sees); the two knobs with one job each — the characteristic hour decides *what is seen* (night-stars are only emitted when the producer sees night), the lens decides *what demands attention*; venue as character-not-cause (the trace protocol survives: consumers still never learn which system produced a phenomenon; `DaySky`/`NightSky`/`Ambient` is character in the same sense as `period_days`); the closed-at-three vector and the ontology-trap posture (no spectral curves, no hearing, no smell, no per-individual variation — widening needs a campaign); the lens derivation with the identity-at-goblin argument in prose (`day_sky = activity_factor · (0.5 + σ)`, `night_sky = (0.5 + ν) · (0.5 + σ)`, `ambient = 1.5 − σ`; goblin `(1.0, 1.0, 1.0)` and the no-arithmetic identity path); the model card: a table of all three dimensions × (goblin, kobold), every dimension **authored**, each kobold value with its one-line 5E SRD derivation (`Nocturnal` — darkvision 60 ft, sunlight sensitivity; night_vision 0.9 — darkvision, a life underground; sky_attention 0.8 — omen-readers, the warren's night spent under the open sky), `Crepuscular` declared idle this campaign (the deliberation-latency precedent).

- [ ] **Step 2: Build and verify**

Run: `mdbook build book`
Expected: clean build, chapter reachable from SUMMARY.

- [ ] **Step 3: Commit**

```bash
git add book/src/domains/perception.md book/src/SUMMARY.md
git commit -m "docs(book): perception chapter opens Campaign Y2-2 (book-driven development)"
```

### Task 2: Commit the pre-Eyes fixtures

The identity contract (Task 7) needs the CURRENT (pre-any-code-change) seed-42 outputs frozen. This task must land before any code task.

**Files:**
- Create: `cli/tests/fixtures/pre-eyes-seed-42-goblin-world.json`
- Create: `cli/tests/fixtures/pre-eyes-seed-42-goblin-almanac.md`
- Create: `cli/tests/fixtures/pre-eyes-seed-42-default-world.json`
- Create: `cli/tests/fixtures/pre-eyes-seed-42-default-almanac.md`

**Interfaces:**
- Produces: the four fixture files Task 7's `eyes_identity.rs` includes via `include_str!`.

- [ ] **Step 1: Generate the four fixtures from current main**

```bash
cargo run -p hornvale -- new --seed 42 --species goblin --out cli/tests/fixtures/pre-eyes-seed-42-goblin-world.json
cargo run -p hornvale -- almanac --world cli/tests/fixtures/pre-eyes-seed-42-goblin-world.json > cli/tests/fixtures/pre-eyes-seed-42-goblin-almanac.md
cargo run -p hornvale -- new --seed 42 --out cli/tests/fixtures/pre-eyes-seed-42-default-world.json
cargo run -p hornvale -- almanac --world cli/tests/fixtures/pre-eyes-seed-42-default-world.json > cli/tests/fixtures/pre-eyes-seed-42-default-almanac.md
```

Expected: four files; the goblin almanac has one pantheon under `## The Gods`; the default almanac shows both peoples but still exactly one (goblin) pantheon.

- [ ] **Step 2: Sanity-check the fixtures are not empty and differ**

Run: `wc -l cli/tests/fixtures/pre-eyes-*` and `grep -c "presides" cli/tests/fixtures/pre-eyes-seed-42-default-almanac.md`
Expected: non-trivial line counts; at least one presiding god.

- [ ] **Step 3: Commit**

```bash
git add cli/tests/fixtures/pre-eyes-seed-42-*
git commit -m "test(fixtures): freeze pre-Eyes seed-42 outputs for the identity contract"
```

### Task 3: Venue and the perception lens through the kernel (one atomic compile)

`Phenomenon` gains a field, so every construction site in the workspace updates in this one task — the workspace must compile at the commit boundary. Producer edits are the one-line venue declarations the spec's design principle 1 authorizes.

**Files:**
- Modify: `kernel/src/phenomena.rs` (the seam itself)
- Modify: `kernel/tests/determinism.rs`, `kernel/examples/first_light.rs` (construction sites)
- Modify: `domains/astronomy/src/lib.rs` (ConstantSun phenomenon ~line 149), `domains/astronomy/src/provider.rs` (`impl PhenomenaSource for GeneratedSky`, lines 294–351, plus its tests' `ObserverContext` literals)
- Modify: `domains/climate/src/lib.rs` (UniformClimate phenomenon, plus test `ObserverContext` literals)
- Modify: `domains/religion/src/lib.rs` (test helper `ph()` only)
- Modify: `windows/almanac/src/lib.rs` (test `Phenomenon` literals only)
- Modify: `windows/worldgen/src/lib.rs` (`ObserverContext` literal at ~line 347 → `ObserverContext::at`)
- Test: `kernel/src/phenomena.rs` (unit tests in-module, the existing pattern)

**Interfaces:**
- Produces (later tasks consume exactly these):
  - `pub enum Venue { DaySky, NightSky, Ambient }` (`Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize`)
  - `Phenomenon { …, pub venue: Venue }`
  - `pub struct PerceptionLens { pub day_sky: f64, pub night_sky: f64, pub ambient: f64 }` (`Clone, Copy, Debug, PartialEq`) with `pub fn identity() -> Self`, `pub fn is_identity(&self) -> bool`
  - `ObserverContext { pub place: EntityId, pub time: WorldTime, pub lens: PerceptionLens }` with `pub fn at(place: EntityId, time: WorldTime) -> Self` (identity lens)
  - `observe(sources, ctx)` — signature unchanged; applies the lens when not identity.

- [ ] **Step 1: Write the failing kernel tests** (append to `kernel/src/phenomena.rs` tests; update the module's `ph()` helper and `ctx()` to the new fields)

```rust
#[test]
fn identity_lens_is_a_byte_level_no_op() {
    // Salience values chosen to be non-representable in binary (0.15) so
    // any multiply-then-round would show: identity must skip arithmetic.
    let a = FixedSource(vec![ph("breeze", 0.15), ph("sun", 1.0)]);
    let plain = observe(&[&a], &ctx());
    let via_identity = observe(
        &[&a],
        &ObserverContext {
            lens: PerceptionLens::identity(),
            ..ctx()
        },
    );
    assert_eq!(plain, via_identity);
    assert_eq!(plain[1].salience.to_bits(), 0.15_f64.to_bits());
}

#[test]
fn a_lens_reweights_by_venue_and_reranks() {
    let a = FixedSource(vec![
        ph_venue("sun", 1.0, Venue::DaySky),
        ph_venue("moon", 0.7, Venue::NightSky),
        ph_venue("air", 0.15, Venue::Ambient),
    ]);
    let lens = PerceptionLens {
        day_sky: 0.52,
        night_sky: 1.82,
        ambient: 0.70,
    };
    let out = observe(&[&a], &ObserverContext { lens, ..ctx() });
    // moon 0.7 × 1.82 = 1.274 → clamp 1.0; sun 1.0 × 0.52 = 0.52; air 0.15 × 0.7 = 0.11 (round2).
    assert_eq!(out[0].kind, "moon");
    assert_eq!(out[0].salience, 1.0);
    assert_eq!(out[1].kind, "sun");
    assert_eq!(out[1].salience, 0.52);
    assert_eq!(out[2].salience, 0.11);
}

#[test]
fn lens_ties_break_by_kind_then_description() {
    // Two night phenomena both clamp to 1.0 under a strong lens.
    let a = FixedSource(vec![
        ph_venue("night-star", 0.6, Venue::NightSky),
        ph_venue("celestial-body", 0.7, Venue::NightSky),
    ]);
    let lens = PerceptionLens {
        day_sky: 1.0,
        night_sky: 1.82,
        ambient: 1.0,
    };
    let out = observe(&[&a], &ObserverContext { lens, ..ctx() });
    assert_eq!(out[0].kind, "celestial-body", "kind breaks the 1.0 tie");
}
```

Add the venue-aware helper alongside the existing `ph()` (which sets `venue: Venue::Ambient` so existing tests keep compiling):

```rust
fn ph_venue(kind: &str, salience: f64, venue: Venue) -> Phenomenon {
    Phenomenon {
        venue,
        ..ph(kind, salience)
    }
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-kernel phenomena 2>&1 | tail -20`
Expected: compile errors (`Venue` not found) — that is the red state for a struct change.

- [ ] **Step 3: Implement the kernel seam**

In `kernel/src/phenomena.rs`:

```rust
/// Where a phenomenon lives, as its producer honestly knows: the day sky,
/// the night sky, or the ambient world. Character, not cause — declaring a
/// venue reveals nothing about which system produced the phenomenon.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Venue {
    /// Seen in the daytime sky (the sun).
    DaySky,
    /// Seen in the night sky (moons, stars).
    NightSky,
    /// Felt through the world rather than watched (air, seasons).
    Ambient,
}
```

Add to `Phenomenon`:

```rust
    /// Where this phenomenon lives (producer-declared character).
    pub venue: Venue,
```

Add the lens:

```rust
/// Multiplicative per-venue salience weights: how much attention an
/// observer's eyes give each venue. The identity lens is a byte-level
/// no-op in `observe` — it triggers no arithmetic at all.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PerceptionLens {
    /// Weight for `Venue::DaySky` phenomena.
    pub day_sky: f64,
    /// Weight for `Venue::NightSky` phenomena.
    pub night_sky: f64,
    /// Weight for `Venue::Ambient` phenomena.
    pub ambient: f64,
}

impl PerceptionLens {
    /// The identity lens: every venue weighted 1.0.
    pub fn identity() -> Self {
        PerceptionLens {
            day_sky: 1.0,
            night_sky: 1.0,
            ambient: 1.0,
        }
    }

    /// Whether this lens is exactly the identity (bitwise 1.0 weights).
    pub fn is_identity(&self) -> bool {
        self.day_sky == 1.0 && self.night_sky == 1.0 && self.ambient == 1.0
    }

    fn weight(&self, venue: Venue) -> f64 {
        match venue {
            Venue::DaySky => self.day_sky,
            Venue::NightSky => self.night_sky,
            Venue::Ambient => self.ambient,
        }
    }
}
```

Extend `ObserverContext` and give it the constructor:

```rust
    /// The observer's perception lens; `PerceptionLens::identity()` for an
    /// unlensed (instrument's-eye) observation.
    pub lens: PerceptionLens,
```

```rust
impl ObserverContext {
    /// An unlensed observation at a place and time (identity lens).
    pub fn at(place: EntityId, time: WorldTime) -> Self {
        ObserverContext {
            place,
            time,
            lens: PerceptionLens::identity(),
        }
    }
}
```

In `observe`, after collecting and before sorting:

```rust
    if !ctx.lens.is_identity() {
        for p in &mut all {
            let w = ctx.lens.weight(p.venue);
            p.salience = ((p.salience * w).clamp(0.0, 1.0) * 100.0).round() / 100.0;
        }
    }
```

Update the in-module test `ctx()` helper to use `ObserverContext::at(EntityId(1), WorldTime { day: 0.0 })` and `ph()` to set `venue: Venue::Ambient`.

- [ ] **Step 4: Declare venues at every construction site**

| File | Phenomenon | `venue:` value |
|---|---|---|
| `domains/astronomy/src/lib.rs` ~149 (ConstantSun) | the constant sun | `Venue::DaySky` |
| `domains/astronomy/src/provider.rs` ~300 (spinning sun) | the sun | `Venue::DaySky` |
| `domains/astronomy/src/provider.rs` ~306 (locked sun) | fixed sun | `Venue::DaySky` |
| `domains/astronomy/src/provider.rs` ~316 (moons) | each moon | `Venue::NightSky` |
| `domains/astronomy/src/provider.rs` ~326 (seasons) | seasonal cycle | `Venue::Ambient` |
| `domains/astronomy/src/provider.rs` ~338 (night-stars) | each neighbor star | `Venue::NightSky` |
| `domains/climate/src/lib.rs` ~58 (UniformClimate) | ambient air | `Venue::Ambient` |

Import `Venue` where needed (these crates already import kernel phenomena types). Update remaining `Phenomenon` literals in tests (`domains/religion/src/lib.rs` `ph()` helper → `Venue::Ambient` default is fine there; `windows/almanac/src/lib.rs` test literals → any venue, use `Venue::Ambient`; `kernel/tests/determinism.rs`; `kernel/examples/first_light.rs`). Update `ObserverContext` struct literals to `ObserverContext::at(...)` in: `windows/worldgen/src/lib.rs` ~347, `domains/astronomy/src/provider.rs` tests, `domains/climate/src/lib.rs` tests, `domains/astronomy/src/lib.rs` tests.

- [ ] **Step 5: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all green — in particular `cli/tests/species_identity.rs` still passes (nothing lensed yet) and the first-light example output is unchanged (venue is not printed).

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat(kernel): producer-declared Venue and the PerceptionLens seam in observe()"
```

### Task 4: The perception vector in the species domain

**Files:**
- Modify: `domains/species/src/lib.rs`
- Modify: `cli/tests/species_identity.rs` (NEW_PREDICATES grows by three)

**Interfaces:**
- Consumes: nothing new (kernel-only crate; the lens itself stays in kernel/worldgen).
- Produces:
  - `pub enum ActivityCycle { Diurnal, Nocturnal, Crepuscular }` (`Clone, Copy, Debug, PartialEq, Eq`)
  - `pub struct PerceptionVector { pub activity: ActivityCycle, pub night_vision: f64, pub sky_attention: f64 }` (`Clone, Copy, Debug, PartialEq`)
  - `SpeciesDef { …, pub perception: PerceptionVector }`
  - predicates `SPECIES_ACTIVITY_CYCLE = "species-activity-cycle"` (Text), `SPECIES_NIGHT_VISION = "species-night-vision"` (Number), `SPECIES_SKY_ATTENTION = "species-sky-attention"` (Number)
  - `pub fn species_entity(world: &World, name: &str) -> Option<EntityId>`

- [ ] **Step 1: Write the failing tests** (in `domains/species/src/lib.rs` tests)

```rust
#[test]
fn goblin_perception_is_the_baseline_and_kobold_contrasts() {
    let reg = registry();
    let g = &reg["goblin"].perception;
    assert_eq!(g.activity, ActivityCycle::Diurnal);
    assert_eq!(g.night_vision, 0.5);
    assert_eq!(g.sky_attention, 0.5);
    let k = &reg["kobold"].perception;
    assert_eq!(k.activity, ActivityCycle::Nocturnal);
    assert!(k.night_vision > 0.5 && k.sky_attention > 0.5);
}

#[test]
fn genesis_commits_perception_facts() {
    let mut w = World::new(Seed(42));
    register_concepts(&mut w.registry).unwrap();
    let ids = genesis(&mut w).unwrap();
    let kobold = ids["kobold"];
    assert_eq!(
        w.ledger.text_of(kobold, SPECIES_ACTIVITY_CYCLE),
        Some("nocturnal")
    );
    assert!(matches!(
        w.ledger.value_of(kobold, SPECIES_NIGHT_VISION),
        Some(Value::Number(n)) if *n > 0.5
    ));
    assert_eq!(species_entity(&w, "kobold"), Some(kobold));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-species 2>&1 | tail -10`
Expected: compile errors (`perception` field, `ActivityCycle` not found).

- [ ] **Step 3: Implement**

Types and predicates:

```rust
/// Predicate: a species' activity cycle — diurnal, nocturnal, crepuscular (functional, Text).
pub const SPECIES_ACTIVITY_CYCLE: &str = "species-activity-cycle";
/// Predicate: how well a species sees at night, 0-1 (functional, Number).
pub const SPECIES_NIGHT_VISION: &str = "species-night-vision";
/// Predicate: how much of a species' attention the sky claims, 0-1 (functional, Number).
pub const SPECIES_SKY_ATTENTION: &str = "species-sky-attention";

/// When a species is awake and watching.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ActivityCycle {
    /// Awake by day (the goblin baseline).
    Diurnal,
    /// Awake by night.
    Nocturnal,
    /// Awake at the boundaries (idle this campaign; authored now so a
    /// future species is a data change).
    Crepuscular,
}

/// The closed three-dimension perception vector (spec §4). Scalars are bare
/// ratios in `[0, 1]` with 0.5 ≡ the goblin baseline; widening the vector
/// requires its own campaign. Every dimension is authored — nothing drawn.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PerceptionVector {
    /// When this species observes.
    pub activity: ActivityCycle,
    /// Night-sky acuity: blind 0 ↔ owl-eyed 1.
    pub night_vision: f64,
    /// Celestial vs. terrestrial attention: earthbound 0 ↔ sky-rapt 1.
    pub sky_attention: f64,
}
```

`SpeciesDef` gains `pub perception: PerceptionVector` (doc: `/// The perception vector.`). Registry values — goblin: `Diurnal, 0.5, 0.5`; kobold: `Nocturnal, 0.9, 0.8` (moon-eyed stargazers; 5E derivations live in the book's model card, Task 1).

`register_concepts` registers the three predicates:

```rust
    registry.register_predicate(
        SPECIES_ACTIVITY_CYCLE,
        true,
        "when a species is awake: diurnal, nocturnal, crepuscular",
    )?;
    registry.register_predicate(SPECIES_NIGHT_VISION, true, "night-sky acuity, 0-1")?;
    registry.register_predicate(SPECIES_SKY_ATTENTION, true, "sky vs. ground attention, 0-1")?;
```

`genesis` commits, per species, after the psych facts:

```rust
        let activity = match def.perception.activity {
            ActivityCycle::Diurnal => "diurnal",
            ActivityCycle::Nocturnal => "nocturnal",
            ActivityCycle::Crepuscular => "crepuscular",
        };
        world.ledger.commit(
            fact(id, SPECIES_ACTIVITY_CYCLE, Value::Text(activity.to_string())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                SPECIES_NIGHT_VISION,
                Value::Number(def.perception.night_vision),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                SPECIES_SKY_ATTENTION,
                Value::Number(def.perception.sky_attention),
            ),
            &world.registry,
        )?;
```

The species-entity lookup:

```rust
/// The species entity carrying `name`'s authored vector, if genesis ran.
pub fn species_entity(world: &World, name: &str) -> Option<EntityId> {
    world
        .ledger
        .find(SPECIES_NAME)
        .find(|f| matches!(&f.object, Value::Text(t) if t == name))
        .map(|f| f.subject)
}
```

In `cli/tests/species_identity.rs`, grow the exclusion list (the Y2-1 superset rule absorbs the new predicates):

```rust
const NEW_PREDICATES: [&str; 12] = [
    // …the existing nine…
    hornvale_species::SPECIES_ACTIVITY_CYCLE,
    hornvale_species::SPECIES_NIGHT_VISION,
    hornvale_species::SPECIES_SKY_ATTENTION,
];
```

- [ ] **Step 4: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green. `species_identity` passes because perception facts land only under the newly excluded predicates.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(species): authored perception vector, closed at three, committed as facts"
```

### Task 5: Lens derivation and the characteristic hour (composition root)

**Files:**
- Modify: `windows/worldgen/src/lib.rs`
- Test: in-module `#[cfg(test)]` tests (the crate's existing pattern)

**Interfaces:**
- Consumes: `PerceptionLens`, `ObserverContext::at` (Task 3); `PerceptionVector`, `ActivityCycle` (Task 4); `Calendar::{day_length, is_daylight}` (`domains/astronomy/src/calendar.rs`); `StdDays::new`.
- Produces (Tasks 6–10 rely on these exact names):
  - `pub fn perception_lens(p: &hornvale_species::PerceptionVector) -> PerceptionLens`
  - `pub fn observation_time(world: &World, activity: hornvale_species::ActivityCycle) -> Result<f64, BuildError>`
  - `pub fn observed_phenomena_as(world: &World, species: &str) -> Result<Vec<Phenomenon>, BuildError>`

- [ ] **Step 1: Write the failing tests**

```rust
#[test]
fn goblin_lens_is_exactly_identity() {
    let reg = hornvale_species::registry();
    assert!(perception_lens(&reg["goblin"].perception).is_identity());
}

#[test]
fn kobold_lens_matches_the_spec_derivation() {
    let reg = hornvale_species::registry();
    let lens = perception_lens(&reg["kobold"].perception);
    assert!((lens.day_sky - 0.52).abs() < 1e-12);
    assert!((lens.night_sky - 1.82).abs() < 1e-12);
    assert!((lens.ambient - 0.70).abs() < 1e-12);
}

#[test]
fn goblin_observation_reproduces_the_unlensed_path_bytewise() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    assert_eq!(
        observed_phenomena_as(&world, "goblin").unwrap(),
        observed_phenomena(&world, 0.0).unwrap(),
    );
}

#[test]
fn a_nocturnal_observer_on_a_spinning_world_sees_night_stars() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let seen = observed_phenomena_as(&world, "kobold").unwrap();
    assert!(
        seen.iter().any(|p| p.kind == "night-star"),
        "the characteristic hour must land in the dark"
    );
    assert_eq!(
        seen[0].venue,
        hornvale_kernel::Venue::NightSky,
        "the kobold ranking is night-headed"
    );
}

#[test]
fn observation_time_is_zero_for_constant_and_locked_skies() {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Constant,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let t = observation_time(&world, hornvale_species::ActivityCycle::Nocturnal).unwrap();
    assert_eq!(t, 0.0);
}
```

(Seed-42 note: the generated seed-42 sky is spinning with two moons — the fixture almanac proves it; if an assertion here surprises you, check the fixture, not the test.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-worldgen 2>&1 | tail -10`
Expected: compile errors (`perception_lens` not found).

- [ ] **Step 3: Implement**

```rust
/// Derive a species' perception lens from its authored vector (spec §4).
/// Identity at the goblin baseline (Diurnal, 0.5, 0.5) by construction:
/// every factor is exactly 1.0 there.
pub fn perception_lens(p: &hornvale_species::PerceptionVector) -> PerceptionLens {
    let activity_factor = match p.activity {
        hornvale_species::ActivityCycle::Diurnal => 1.0,
        hornvale_species::ActivityCycle::Crepuscular => 0.7,
        hornvale_species::ActivityCycle::Nocturnal => 0.4,
    };
    let sky = 0.5 + p.sky_attention;
    PerceptionLens {
        day_sky: activity_factor * sky,
        night_sky: (0.5 + p.night_vision) * sky,
        ambient: 1.5 - p.sky_attention,
    }
}

/// The characteristic hour: when a species with this activity cycle
/// observes (spec §5). Diurnal observes at day 0.0 (the legacy path,
/// byte-identical); Nocturnal at the first non-daylight instant found by a
/// deterministic scan of 1/24-local-day steps over two local days;
/// Crepuscular at the first light/dark boundary the same scan finds.
/// Worlds without a day/night cycle (constant sun, tidal lock) observe at
/// day 0.0 regardless.
pub fn observation_time(
    world: &World,
    activity: hornvale_species::ActivityCycle,
) -> Result<f64, BuildError> {
    use hornvale_species::ActivityCycle;
    if activity == ActivityCycle::Diurnal {
        return Ok(0.0);
    }
    let sky = sky_of(world)?;
    let Some(calendar) = sky.calendar() else {
        return Ok(0.0);
    };
    let Some(day_len) = calendar.day_length() else {
        return Ok(0.0); // locked: no day/night cycle
    };
    let step = day_len.get() / 24.0;
    let daylight_at = |t: f64| {
        hornvale_astronomy::StdDays::new(t)
            .ok()
            .and_then(|d| calendar.is_daylight(d))
    };
    let at_zero = daylight_at(0.0);
    for k in 0..48 {
        let t = k as f64 * step;
        let here = daylight_at(t);
        let hit = match activity {
            ActivityCycle::Nocturnal => here == Some(false),
            ActivityCycle::Crepuscular => here != at_zero,
            ActivityCycle::Diurnal => unreachable!("early-returned above"),
        };
        if hit {
            return Ok(t);
        }
    }
    Ok(0.0) // pathological all-daylight window: fall back deterministically
}

/// The phenomena a species observes: its characteristic hour, its lens,
/// the world's first place (spec §5 — the place debt is SEQ-4's).
pub fn observed_phenomena_as(
    world: &World,
    species: &str,
) -> Result<Vec<Phenomenon>, BuildError> {
    let registry = hornvale_species::registry();
    let Some(def) = registry.get(species) else {
        let known: Vec<&str> = registry.keys().copied().collect();
        return Err(BuildError::Pins(format!(
            "unknown species '{species}'; known species: {}",
            known.join(", ")
        )));
    };
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let day = observation_time(world, def.perception.activity)?;
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: perception_lens(&def.perception),
        },
    ))
}
```

Check `StdDays::new`'s actual signature in `domains/astronomy/src/units.rs` before wiring `daylight_at` (if it returns `Option` rather than `Result`, drop the `.ok()`). Import what's missing (`hornvale_astronomy::StdDays`).

- [ ] **Step 4: Run tests to verify they pass, then the full gate**

Run: `cargo test -p hornvale-worldgen && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(worldgen): perception lens derivation and the characteristic hour"
```

### Task 6: Religion — species-qualified epithet streams and per-community reads

**Files:**
- Modify: `domains/religion/src/lib.rs`
- Modify: `windows/worldgen/src/lib.rs` (only the one `hornvale_religion::genesis` call gains the new argument; the per-species loop itself is Task 7)

**Interfaces:**
- Consumes: nothing new.
- Produces:
  - `genesis(world, community, phenomena, society, stream_qualifier: Option<&str>)` — `None` = the legacy `religion/epithet` stream (goblin and every existing test); `Some("kobold")` = `religion/kobold/epithet`.
  - `pub fn beliefs_held_by(world: &World, community: EntityId) -> Vec<Belief>`
  - `pub fn cult_form_held_by(world: &World, community: EntityId) -> Option<String>`
  - `stream_labels()` gains `("religion/kobold/epithet", "deity epithet pick (kobold pantheon)")`.

- [ ] **Step 1: Write the failing tests** (in `domains/religion/src/lib.rs`)

```rust
#[test]
fn a_qualified_stream_draws_independently_of_the_legacy_stream() {
    let (mut w, c) = world(42);
    let c2 = w.ledger.mint_entity();
    genesis(&mut w, c, &sky(), &society(), None).unwrap();
    genesis(&mut w, c2, &sky(), &society(), Some("kobold")).unwrap();
    let all = beliefs_of(&w);
    let held_c: Vec<_> = beliefs_held_by(&w, c);
    let held_c2: Vec<_> = beliefs_held_by(&w, c2);
    assert_eq!(held_c.len() + held_c2.len(), all.len());
    assert_eq!(held_c.len(), 3);
    assert_eq!(held_c2.len(), 3);
    // Same phenomena, same code — but independent streams, so the epithet
    // sequences must not be forced equal. (They may coincide per-deity by
    // chance; the tenets differing anywhere is the check.)
    let tenets = |bs: &[Belief]| bs.iter().map(|b| b.tenet.clone()).collect::<Vec<_>>();
    assert_ne!(
        tenets(&held_c),
        tenets(&held_c2),
        "kobold epithets replayed the goblin stream — the qualifier is not wired"
    );
}

#[test]
fn cult_form_is_read_per_community() {
    let (mut w, c) = world(42);
    let c2 = w.ledger.mint_entity();
    genesis(&mut w, c, &sky(), &SocietySummary { strata: 5, has_priesthood: true }, None).unwrap();
    genesis(&mut w, c2, &sky(), &SocietySummary { strata: 2, has_priesthood: false }, Some("kobold")).unwrap();
    assert_eq!(cult_form_held_by(&w, c).as_deref(), Some("organized"));
    assert_eq!(cult_form_held_by(&w, c2).as_deref(), Some("folk"));
}
```

Add a `society()` helper mirroring the existing test style (`strata: 5, has_priesthood: true`). Every existing `genesis(...)` call in the religion tests gains a trailing `None`.

(If seed 42 happens to draw identical epithet sequences on both streams, the first test's `assert_ne!` would be a false alarm — check by eye when it first runs; if so, switch the test seed to 7. Do not weaken the assertion.)

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-religion 2>&1 | tail -10`
Expected: compile errors (arity, `beliefs_held_by` not found).

- [ ] **Step 3: Implement**

In `genesis`, the signature gains `stream_qualifier: Option<&str>` (doc the parameter in the fn doc: "`stream_qualifier`: `None` draws epithets from the legacy `religion/epithet` stream; `Some(s)` from `religion/<s>/epithet` — permanent labels, ADR 0006"). The stream derivation becomes:

```rust
    let base = world.seed.derive(streams::ROOT);
    let mut stream = match stream_qualifier {
        None => base.derive(streams::EPITHET).stream(),
        Some(q) => base.derive(q).derive(streams::EPITHET).stream(),
    };
```

The reads:

```rust
/// The beliefs held by one community, in commit order (element 0 is the
/// pantheon's most salient deity — its head where one presides).
pub fn beliefs_held_by(world: &World, community: EntityId) -> Vec<Belief> {
    beliefs_of(world)
        .into_iter()
        .filter(|b| {
            matches!(
                world.ledger.value_of(b.id, HELD_BY),
                Some(Value::Entity(c)) if *c == community
            )
        })
        .collect()
}

/// The cult form of one community's pantheon, from its first belief.
pub fn cult_form_held_by(world: &World, community: EntityId) -> Option<String> {
    let first = beliefs_held_by(world, community).into_iter().next()?;
    match world.ledger.value_of(first.id, CULT_FORM) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}
```

`stream_labels()` gains the kobold row. In `windows/worldgen/src/lib.rs`, the existing goblin-gated call gains `, None` as its last argument (loop restructuring is Task 7's).

- [ ] **Step 4: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green; `species_identity` still passes (goblin religion still draws from the legacy stream).

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(religion): species-qualified epithet streams and per-community pantheon reads"
```

### Task 7: Two religions in the composition root + the identity keystone

**Files:**
- Modify: `windows/worldgen/src/lib.rs` (the per-species loop, ~lines 524–566)
- Create: `cli/tests/eyes_identity.rs`

**Interfaces:**
- Consumes: `observed_phenomena_as` (Task 5), `genesis(…, Option<&str>)` and `beliefs_held_by` (Task 6), the Task 2 fixtures.
- Produces: default worlds carry two pantheons; `flagship_of(world, species)` + `beliefs_held_by` is how every later task reads them.

- [ ] **Step 1: Rewire the loop** (in `build_world`; replace the `if def.name == "goblin"` block)

```rust
        // Religion for every species-flagship (spec §5): each species sees
        // the sky through its own lens at its own hour. The priesthood
        // check uses the species' own shaman-rung word — kobold "keeper"
        // is a priesthood exactly as goblin "shaman" is.
        let castes = hornvale_culture::castes_of(&world, flagship);
        let society = hornvale_religion::SocietySummary {
            strata: castes.len(),
            has_priesthood: castes.iter().any(|c| c == def.shaman),
        };
        let seen = observed_phenomena_as(&world, def.name)?;
        let qualifier = if def.name == "goblin" { None } else { Some(def.name) };
        hornvale_religion::genesis(&mut world, flagship, &seen, &society, qualifier)?;
```

Note the goblin path is byte-for-byte the old path: identity lens + hour 0.0 (`observed_phenomena_as(w, "goblin") ≡ observed_phenomena(w, 0.0)`, proven in Task 5), legacy stream (`None`), and `c == def.shaman` is `c == "shaman"` for goblins. Species genesis stays where it is (after this loop — kobold beliefs mint before species entities; the spec §7 accepts the species-entity id shift).

- [ ] **Step 2: Write the keystone tests** (`cli/tests/eyes_identity.rs`)

```rust
//! The Eyes identity contract (spec §7): a goblin-pinned world reproduces
//! pre-Eyes main as a superset (almanac byte-identical; ledger identical
//! off the three new perception predicates), and the default world's goblin
//! pantheon — entities, facts, and rendered section — is unmoved.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

const PERCEPTION_PREDICATES: [&str; 3] = [
    hornvale_species::SPECIES_ACTIVITY_CYCLE,
    hornvale_species::SPECIES_NIGHT_VISION,
    hornvale_species::SPECIES_SKY_ATTENTION,
];

fn build(species: Option<&str>) -> hornvale_kernel::World {
    let world = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: species.map(str::to_string),
            ..SettlementPins::default()
        },
    )
    .unwrap();
    // Round-trip through the save format — same rationale as
    // species_identity.rs (serde_json's 1-ULP float quirk).
    hornvale_kernel::World::from_json(&world.to_json()).unwrap()
}

fn filtered(world: &hornvale_kernel::World) -> Vec<String> {
    world
        .ledger
        .iter()
        .filter(|f| !PERCEPTION_PREDICATES.contains(&f.predicate.as_str()))
        .map(|f| format!("{f:?}"))
        .collect()
}

#[test]
fn goblin_pinned_seed_42_is_a_superset_of_pre_eyes_main() {
    let fixture: hornvale_kernel::World =
        serde_json::from_str(include_str!("fixtures/pre-eyes-seed-42-goblin-world.json"))
            .unwrap();
    let world = build(Some("goblin"));
    assert_eq!(filtered(&world), filtered(&fixture), "filtered ledgers diverge");
    let ctx = hornvale_worldgen::almanac_context(&world).unwrap();
    assert_eq!(
        hornvale_almanac::render(&ctx),
        include_str!("fixtures/pre-eyes-seed-42-goblin-almanac.md"),
        "goblin-pinned almanac diverged from the pre-Eyes fixture"
    );
}

#[test]
fn the_default_worlds_goblin_pantheon_is_unmoved() {
    let fixture: hornvale_kernel::World =
        serde_json::from_str(include_str!("fixtures/pre-eyes-seed-42-default-world.json"))
            .unwrap();
    let world = build(None);
    let goblin_flagship = |w: &hornvale_kernel::World| {
        hornvale_worldgen::flagship_of(w, "goblin").expect("goblin flagship").id
    };
    let ours = hornvale_religion::beliefs_held_by(&world, goblin_flagship(&world));
    let theirs = hornvale_religion::beliefs_held_by(&fixture, goblin_flagship(&fixture));
    assert_eq!(ours, theirs, "goblin beliefs (ids, tenets, kinds, heads) moved");
    assert!(!ours.is_empty(), "vacuous identity — no goblin pantheon at all");

    // The kobold pantheon exists and is differently headed (spec §12.1).
    let kobold_flagship = hornvale_worldgen::flagship_of(&world, "kobold")
        .expect("seed 42 places a kobold flagship")
        .id;
    let kobold = hornvale_religion::beliefs_held_by(&world, kobold_flagship);
    assert!(!kobold.is_empty(), "the kobold pantheon exists");
    let seen = hornvale_worldgen::observed_phenomena_as(&world, "kobold").unwrap();
    assert_eq!(
        seen[0].venue,
        hornvale_kernel::Venue::NightSky,
        "the kobold head deity derives from the night sky"
    );
}
```

(`hornvale-almanac`, `hornvale-religion`, `hornvale-species`, `hornvale-astronomy`, `hornvale-terrain` are already dev-dependencies of the CLI crate — `species_identity.rs` uses them; verify with `grep hornvale- cli/Cargo.toml` and add any missing one to `[dev-dependencies]`.)

- [ ] **Step 3: Run the identity tests**

Run: `cargo test -p hornvale --test eyes_identity 2>&1 | tail -15`
Expected: **`goblin_pinned…` passes already; `the_default_worlds_goblin_pantheon_is_unmoved` fails only if the loop rewire broke something** — but note the fixture-almanac assertion in the goblin test cannot pass until the almanac still renders single-pantheon worlds byte-identically, which is true right now because `almanac_context` is untouched until Task 8. Both tests green here; Task 8 must keep them green.

- [ ] **Step 4: Run the full gate**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green. `species_identity.rs` (the Y2-1 contract) also still green — the goblin-pinned path never lensed, never re-timed, never re-streamed.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(worldgen): religion runs per species-flagship; the Eyes identity keystone"
```

### Task 8: Two pantheons in the almanac

**Files:**
- Modify: `windows/almanac/src/lib.rs` (`AlmanacContext`, `render`, tests)
- Modify: `windows/worldgen/src/lib.rs` (`almanac_context`, ~lines 759–790)

**Interfaces:**
- Consumes: `beliefs_held_by`, `cult_form_held_by` (Task 6); `flagship_of` (existing); `hornvale_species::registry()`.
- Produces:
  - `pub struct PantheonBlock { pub species: String, pub noun: String, pub settlement: String, pub cult_form: Option<String>, pub beliefs: Vec<Belief> }`
  - `AlmanacContext` drops `beliefs`/`cult_form`, gains `pub pantheons: Vec<PantheonBlock>`.

- [ ] **Step 1: Write the failing render tests** (in `windows/almanac/src/lib.rs`; adapt `sample_context()` to build one goblin `PantheonBlock` from the old fields)

```rust
#[test]
fn a_single_pantheon_renders_exactly_as_before() {
    // Byte-identity: the first block must reproduce the legacy section.
    let ctx = sample_context(); // one goblin pantheon, organized, one high god
    let doc = render(&ctx);
    assert!(doc.contains("## The Gods\n\nAn organized priesthood tends a pantheon:\n\n"));
    assert!(!doc.contains("its own"), "single-pantheon worlds name no species");
}

#[test]
fn a_second_pantheon_gets_a_species_lead() {
    let mut ctx = sample_context();
    ctx.pantheons.push(PantheonBlock {
        species: "kobold".to_string(),
        noun: "warren".to_string(),
        settlement: "Zikthur".to_string(),
        cult_form: Some("folk".to_string()),
        beliefs: vec![Belief {
            id: EntityId(99),
            tenet: "the Tidewalker departs and returns every 18 days; its absences are mourned and its returns feasted.".to_string(),
            source_kind: "celestial-body".to_string(),
            high_god: true,
        }],
    });
    let doc = render(&ctx);
    assert!(doc.contains("The warren of **Zikthur** keeps its own folk pantheon:"));
    let gods = doc.split("## The Gods").nth(1).unwrap();
    assert!(
        gods.find("priesthood tends a pantheon").unwrap()
            < gods.find("its own folk pantheon").unwrap(),
        "goblin block renders first, exactly as before"
    );
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale-almanac 2>&1 | tail -10`
Expected: compile errors (`pantheons` not found).

- [ ] **Step 3: Implement the context and renderer**

```rust
/// One community's pantheon, ready to render: the species and settlement
/// it belongs to, its cult form, and its beliefs in salience order.
pub struct PantheonBlock {
    /// The species name ("goblin", "kobold"); empty for legacy saves that
    /// predate species facts.
    pub species: String,
    /// The settlement noun ("village", "warren").
    pub noun: String,
    /// The holding settlement's name.
    pub settlement: String,
    /// The pantheon's cult form (`"organized"` or `"folk"`), if recorded.
    pub cult_form: Option<String>,
    /// The pantheon's beliefs, head first.
    pub beliefs: Vec<Belief>,
}
```

`render`'s Gods section becomes:

```rust
    doc.push_str("## The Gods\n\n");
    if ctx.pantheons.iter().all(|p| p.beliefs.is_empty()) {
        doc.push_str("No beliefs are recorded.\n\n");
    } else {
        for (i, pantheon) in ctx.pantheons.iter().enumerate() {
            if pantheon.beliefs.is_empty() {
                continue;
            }
            // The first block reproduces the legacy section byte-for-byte
            // (the identity contract); later blocks name their people.
            if i == 0 {
                if let Some(form) = &pantheon.cult_form {
                    let lead = match form.as_str() {
                        "organized" => "An organized priesthood tends a pantheon:",
                        _ => "The people keep a folk pantheon:",
                    };
                    doc.push_str(&format!("{lead}\n\n"));
                }
            } else {
                let lead = match pantheon.cult_form.as_deref() {
                    Some("organized") => format!(
                        "In the {} of **{}**, an organized priesthood tends its own pantheon:",
                        pantheon.noun, pantheon.settlement
                    ),
                    _ => format!(
                        "The {} of **{}** keeps its own folk pantheon:",
                        pantheon.noun, pantheon.settlement
                    ),
                };
                doc.push_str(&format!("{lead}\n\n"));
            }
            for belief in &pantheon.beliefs {
                let mark = if belief.high_god { " *(who presides)*" } else { "" };
                doc.push_str(&format!(
                    "> {}{mark}\n>\n> — derived from the phenomenon *{}*\n\n",
                    belief.tenet, belief.source_kind
                ));
            }
        }
    }
```

In `windows/worldgen::almanac_context`, replace the `beliefs:`/`cult_form:` fields with:

```rust
        pantheons: {
            let mut blocks = Vec::new();
            for (name, def) in hornvale_species::registry() {
                if let Some(v) = flagship_of(world, name) {
                    let beliefs = hornvale_religion::beliefs_held_by(world, v.id);
                    if !beliefs.is_empty() {
                        blocks.push(hornvale_almanac::PantheonBlock {
                            species: name.to_string(),
                            noun: def.noun.to_string(),
                            settlement: v.name.clone(),
                            cult_form: hornvale_religion::cult_form_held_by(world, v.id),
                            beliefs,
                        });
                    }
                }
            }
            // Legacy fallback: pre-species saves have beliefs but no
            // peopled-by facts — render them as the single anonymous
            // pantheon they always were.
            if blocks.is_empty() {
                let beliefs = hornvale_religion::beliefs_of(world);
                if !beliefs.is_empty() {
                    blocks.push(hornvale_almanac::PantheonBlock {
                        species: String::new(),
                        noun: String::new(),
                        settlement: String::new(),
                        cult_form: hornvale_religion::cult_form_of(world),
                        beliefs,
                    });
                }
            }
            blocks
        },
```

(Check `VillageInfo`'s field for the settlement name — `species_identity.rs` and the REPL use `v.id`; confirm the name field with `grep -n "pub struct VillageInfo" -A 8 domains/settlement/src/*.rs` and adjust `v.name` if it differs.) Update the worldgen in-module tests that referenced `ctx.beliefs`/`ctx.cult_form` to `ctx.pantheons`.

- [ ] **Step 4: Run the identity keystone, then the full gate**

Run: `cargo test -p hornvale --test eyes_identity --test species_identity --test exit_criterion --test sky_exit_criterion && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: all green — the goblin-pinned almanac still matches its fixture byte-for-byte (first-block rendering is the legacy rendering), and the default almanac now carries a second, kobold pantheon.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(almanac): one Gods section, two pantheons — first block byte-stable"
```

### Task 9: The REPL — `phenomena --as` and the species hop in `why`

**Files:**
- Modify: `cli/src/repl.rs` (the `"phenomena"` and `"why"` arms, the help text, tests)

**Interfaces:**
- Consumes: `observed_phenomena_as` (Task 5), `hornvale_species::{species_of, species_entity}` (Task 4), `hornvale_historiography::recount` (unchanged — it stays domain-agnostic; the composition lives here).

- [ ] **Step 1: Write the failing tests** (in `cli/src/repl.rs` tests; the existing `drive()` helper builds a Constant-sky world — add a Generated-sky driver)

```rust
fn drive_generated(commands: &str) -> String {
    let world = build_world(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &world_builder::SettlementPins::default(),
    )
    .unwrap();
    let mut out = Vec::new();
    run(&world, commands.as_bytes(), &mut out).unwrap();
    String::from_utf8(out).unwrap()
}

#[test]
fn phenomena_as_kobold_ranks_the_night_sky_first() {
    let out = drive_generated("phenomena --as kobold\n");
    let first = out.lines().next().unwrap();
    assert!(
        first.contains("moon") || first.contains("star"),
        "kobold-lensed ranking must be night-headed, got: {first}"
    );
}

#[test]
fn phenomena_as_unknown_species_fails_loudly() {
    let out = drive_generated("phenomena --as elf\n");
    assert!(out.contains("unknown species 'elf'"));
    assert!(out.contains("goblin"), "the error lists known species");
}

#[test]
fn why_a_belief_recounts_through_the_species_eyes() {
    let out = drive_generated("beliefs\n");
    let first_id: u64 = out
        .lines()
        .next()
        .and_then(|l| l.split(['[', ']']).nth(1))
        .and_then(|s| s.parse().ok())
        .expect("beliefs lists at least one id");
    let recounted = drive_generated(&format!("why {first_id}\n"));
    assert!(
        recounted.contains("Seen through goblin eyes:"),
        "the species hop is missing: {recounted}"
    );
    assert!(recounted.contains("night-sky acuity"));
}
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `cargo test -p hornvale repl 2>&1 | tail -10`
Expected: FAIL (`--as` unparsed; no species hop).

- [ ] **Step 3: Implement**

The `"phenomena"` arm:

```rust
            "phenomena" => {
                let tokens: Vec<&str> = argument.map(str::split_whitespace).into_iter().flatten().collect();
                let species = tokens
                    .iter()
                    .position(|t| *t == "--as")
                    .and_then(|i| tokens.get(i + 1).copied());
                let result = match species {
                    Some(s) => world_builder::observed_phenomena_as(world, s),
                    None => {
                        let day = tokens.first().and_then(|a| a.parse().ok()).unwrap_or(0.0);
                        world_builder::observed_phenomena(world, day)
                    }
                };
                match result {
                    Ok(phenomena) => {
                        for p in phenomena {
                            writeln!(output, "[{:.2}] {} — {}", p.salience, p.kind, p.description)?;
                        }
                    }
                    Err(e) => writeln!(output, "error: {e}")?,
                }
            }
```

The `"why"` arm, after the successful `recount` write, appends the species hop (historiography stays domain-agnostic; the CLI composes):

```rust
                Some(id) => match hornvale_historiography::recount(world, EntityId(id)) {
                    Some(text) => {
                        write!(output, "{text}")?;
                        // A belief recounts onward to the eyes that ranked
                        // its phenomenon: held-by → settlement → peopled-by
                        // → the species entity's authored vector.
                        if let Some(Value::Entity(community)) =
                            world.ledger.value_of(EntityId(id), hornvale_religion::HELD_BY)
                        {
                            if let Some(species) = hornvale_species::species_of(world, *community) {
                                if let Some(entity) = hornvale_species::species_entity(world, &species)
                                    && let Some(text) = hornvale_historiography::recount(world, entity)
                                {
                                    writeln!(output, "Seen through {species} eyes:")?;
                                    write!(output, "{text}")?;
                                }
                            }
                        }
                    }
                    None => writeln!(output, "nothing is recorded about entity {id}")?,
                },
```

Update the REPL help text's `phenomena` line to `phenomena [day] [--as <species>] — salient phenomena, optionally through a species' eyes` (match the existing help formatting; find it with `grep -n "help" cli/src/repl.rs`). Add `use hornvale_species;` / Cargo dev additions only if the compiler asks (the CLI already depends on species for `settlements`).

- [ ] **Step 4: Run tests to verify they pass, then the full gate**

Run: `cargo test -p hornvale && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green (`why_a_belief_recounts_through_the_species_eyes` relies on the predicate doc strings from Task 4 — "night-sky acuity, 0-1").

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(cli): phenomena --as <species> and the why species hop"
```

### Task 10: Lab metrics — head-deity domain, pantheon structure, blind attribution

**Files:**
- Modify: `windows/lab/src/metrics.rs`
- Modify: `windows/lab/tests/calibration.rs`

**Interfaces:**
- Consumes: `observed_phenomena_as`, `flagship_of`, `beliefs_held_by`, `cult_form_held_by`, `Venue`.
- Produces metrics (names are permanent once a study page cites them): `head-deity-domain-goblin`, `head-deity-domain-kobold`, `pantheon-size-goblin`, `pantheon-size-kobold`, `cult-form-goblin`, `cult-form-kobold`, `blind-attribution-correct`.

- [ ] **Step 1: Implement the shared extraction helpers** (private, in `metrics.rs`)

```rust
/// A flagship pantheon's structural signature — every lexical channel
/// (names, epithets, tenets) deliberately absent (spec §9.2).
struct PantheonSig {
    /// solar | lunar | ambient — the venue of the perceived top phenomenon.
    domain: &'static str,
    /// Number of deities.
    size: usize,
    /// Whether one deity presides.
    ranked: bool,
    /// organized | folk.
    cult: String,
    /// Fraction of the pantheon's source phenomena that are periodic.
    cyclic_share: f64,
}

fn pantheon_sig(v: &WorldView, species: &str) -> Option<PantheonSig> {
    let flagship = flagship_of(&v.world, species)?;
    let beliefs = hornvale_religion::beliefs_held_by(&v.world, flagship.id);
    if beliefs.is_empty() {
        return None;
    }
    let seen = hornvale_worldgen::observed_phenomena_as(&v.world, species).ok()?;
    let top = seen.first()?;
    let domain = match top.venue {
        hornvale_kernel::Venue::DaySky => "solar",
        hornvale_kernel::Venue::NightSky => "lunar",
        hornvale_kernel::Venue::Ambient => "ambient",
    };
    let members = &seen[..beliefs.len().min(seen.len())];
    let cyclic = members.iter().filter(|p| p.period_days.is_some()).count();
    Some(PantheonSig {
        domain,
        size: beliefs.len(),
        ranked: beliefs.iter().any(|b| b.high_god),
        cult: hornvale_religion::cult_form_held_by(&v.world, flagship.id)
            .unwrap_or_else(|| "folk".to_string()),
        cyclic_share: cyclic as f64 / members.len().max(1) as f64,
    })
}

/// The fixed blind-attribution rule (spec §9.2, preregistered): given two
/// unlabeled signatures, pick the kobold. Structure only — no lexical
/// input. Returns the index (0/1), or None when indistinguishable.
fn pick_kobold(pair: [&PantheonSig; 2]) -> Option<usize> {
    // Rule 1: exactly one lunar-headed pantheon → it is the kobolds'.
    match (pair[0].domain == "lunar", pair[1].domain == "lunar") {
        (true, false) => return Some(0),
        (false, true) => return Some(1),
        _ => {}
    }
    // Rule 2: the more cyclic pantheon (moon-and-star gods recur).
    if pair[0].cyclic_share != pair[1].cyclic_share {
        return Some(if pair[0].cyclic_share > pair[1].cyclic_share { 0 } else { 1 });
    }
    // Rule 3: the larger pantheon (the boosted night sky seats more gods).
    if pair[0].size != pair[1].size {
        return Some(if pair[0].size > pair[1].size { 0 } else { 1 });
    }
    None // indistinguishable: scored as a miss
}
```

- [ ] **Step 2: Register the seven metrics** (append to `registry()`; the doc strings are the manifest — keep them exact)

```rust
        Metric {
            name: "head-deity-domain-goblin",
            doc: "Venue domain of the goblin flagship's head deity: solar, lunar, or ambient; Absent without a goblin pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "head-deity-domain-kobold",
            doc: "Venue domain of the kobold flagship's head deity: solar, lunar, or ambient; Absent without a kobold pantheon",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.domain.to_string()),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-goblin",
            doc: "Number of deities in the goblin flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric { bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0] },
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "pantheon-size-kobold",
            doc: "Number of deities in the kobold flagship's pantheon; Absent without one",
            summary: SummaryKind::Numeric { bucket_edges: &[0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0] },
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Number(s.size as f64),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-goblin",
            doc: "Cult form of the goblin flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "goblin") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "cult-form-kobold",
            doc: "Cult form of the kobold flagship's pantheon (organized/folk); Absent without one",
            summary: SummaryKind::Categorical,
            extract: |v| match pantheon_sig(v, "kobold") {
                Some(s) => MetricValue::Text(s.cult),
                None => MetricValue::Absent,
            },
        },
        Metric {
            name: "blind-attribution-correct",
            doc: "Whether the fixed structural rule (lunar head, then cyclic share, then size — no lexical input) attributes the kobold pantheon correctly; Absent unless both peoples hold pantheons",
            summary: SummaryKind::Flag,
            extract: |v| {
                let (Some(g), Some(k)) = (pantheon_sig(v, "goblin"), pantheon_sig(v, "kobold")) else {
                    return MetricValue::Absent;
                };
                // The rule is a symmetric function of the unordered pair;
                // presenting (goblin, kobold) and requiring index 1 is the
                // correctness check, not a labeling leak.
                MetricValue::Flag(pick_kobold([&g, &k]) == Some(1))
            },
        },
```

- [ ] **Step 3: Write the preregistered calibration tests** (append to `windows/lab/tests/calibration.rs`; same style as the existing row-by-row tests over `census-lands-drift.study.json` — these are the DIRECTIONAL preregistrations, written before any census runs)

```rust
#[test]
fn goblin_heads_are_always_solar_and_mooned_kobold_heads_always_lunar() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let (g_i, k_i, moons_i) = (
        idx("head-deity-domain-goblin"),
        idx("head-deity-domain-kobold"),
        idx("moons-admitted"),
    );
    for row in &result.rows {
        if row.refusal.is_some() {
            continue;
        }
        if let MetricValue::Text(domain) = &row.values[g_i] {
            assert_eq!(domain, "solar", "seed {}: goblin head not solar", row.seed);
        }
        let mooned = matches!(&row.values[moons_i], MetricValue::Text(n) if n != "0");
        if mooned && let MetricValue::Text(domain) = &row.values[k_i] {
            assert_eq!(
                domain, "lunar",
                "seed {}: kobold head not lunar despite a moon",
                row.seed
            );
        }
        // Moonless kobold heads split night-star/sun by star brightness —
        // recorded, not asserted; the split is pinned at the 10k census
        // (spec §9.1), Task 11.
    }
}

#[test]
fn blind_attribution_beats_chance_decisively() {
    let study = load_study(Path::new("../../studies/census-lands-drift.study.json")).unwrap();
    let result = run(&study).unwrap();
    let idx = |name: &str| result.metric_names.iter().position(|n| *n == name).unwrap();
    let a_i = idx("blind-attribution-correct");
    let (mut correct, mut total) = (0u32, 0u32);
    for row in &result.rows {
        match &row.values[a_i] {
            MetricValue::Flag(true) => {
                correct += 1;
                total += 1;
            }
            MetricValue::Flag(false) => total += 1,
            _ => {}
        }
    }
    assert!(total > 0, "no attributable world pairs in the drift study");
    // Directional preregistration (spec §9.2): decisively above chance.
    // The exact rate is pinned in Task 11 after the 10k census; this
    // 500-seed floor is deliberately conservative.
    let accuracy = f64::from(correct) / f64::from(total);
    assert!(
        accuracy >= 0.9,
        "blind attribution at {accuracy:.3} — below the preregistered floor"
    );
}
```

- [ ] **Step 4: Run the lab tests, then the full gate**

Run: `cargo test -p hornvale-lab 2>&1 | tail -15 && cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Expected: green. If `blind_attribution_beats_chance_decisively` fails its 0.9 floor, STOP — that is the reskin alarm ringing, not a threshold to lower; investigate which structural channel is degenerate and report before touching the number.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(lab): per-species pantheon metrics and the preregistered blind-attribution rule"
```

### Task 11: The re-baseline — 10k censuses, Study 007, pinned calibration rows, artifacts

The one artifact shift of the campaign (spec §7), after all code has landed.

**Files:**
- Create: `studies/census-of-eyes.study.json`
- Create: `book/src/laboratory/study-007.md` (mirror `study-006.md`'s structure exactly — read it first)
- Modify: `book/src/laboratory/overview.md` (add the study row), `book/src/SUMMARY.md` (add the study page)
- Modify: `windows/lab/tests/calibration.rs` (pin the measured rates as exact rows)
- Regenerate: `book/src/gallery/*.md` seed-42 almanacs, `book/src/reference/*` (registry + manifest dumps), every committed study output under `book/src/laboratory/generated/`
- Modify (only if the drift check's command list needs the new study): `.github/workflows/ci.yml`

- [ ] **Step 1: Author the study**

```json
{ "name": "census-of-eyes",
  "description": "The Census of Eyes: per-species head-deity domain, pantheon size, cult form, and blind attribution over 10,000 worlds — the two-pantheon baseline and the Y2-2 calibrations at author-time scale.",
  "seeds": { "from": 0, "count": 10000 },
  "pin_sets": [ { "label": "default", "pins": [] } ],
  "metrics": "all" }
```

- [ ] **Step 2: Run the author-time censuses**

```bash
cargo run --release -p hornvale -- lab run studies/census-of-eyes.study.json
cargo run --release -p hornvale -- lab run studies/census-of-faiths.study.json
cargo run --release -p hornvale -- lab run studies/census-of-peoples.study.json
cargo run --release -p hornvale -- lab run studies/census-of-lands.study.json
cargo run --release -p hornvale -- lab run studies/census-of-skies.study.json
cargo run --release -p hornvale -- lab run studies/census-lands-drift.study.json
```

Expected: outputs land under the lab's publish path (`book/src/laboratory/generated/`); record the headline numbers — blind-attribution accuracy, the kobold head-domain split on moonless worlds, per-species pantheon-size distributions — verbatim in your report.

- [ ] **Step 3: Pin the measured rates**

In `windows/lab/tests/calibration.rs`, extend `blind_attribution_beats_chance_decisively` with the exact drift-study count, pinned from the run (the Y2-1 pattern — an exact equality the drift study guards in CI):

```rust
    // Pinned calibration row (measured at the Y2-2 re-baseline; the drift
    // study is 500 seeds, so this is an exact count, not a rate):
    assert_eq!(correct, /* PIN: the measured count */, "blind-attribution count drifted");
    assert_eq!(total, /* PIN: the measured count */, "attributable-pair count drifted");
```

Replace both `/* PIN */` comments with the observed integers. Do the same style of pin for the moonless kobold head split if the drift study contains any moonless rows (guard with the observed count).

- [ ] **Step 4: Regenerate every committed artifact and run the drift check locally**

Run the "Artifacts are current" command list from `.github/workflows/ci.yml` verbatim (it regenerates the three seed-42 almanacs, the first-light output, the map, the registry/manifest dumps, and the CI-scale studies), then:

```bash
git diff --stat book/src/gallery/ book/src/reference/ book/src/laboratory/
```

Expected diffs, all explainable: the seed-42 almanacs gain the kobold pantheon block; the concept-registry dump gains the three perception predicates; the stream manifest gains `religion/kobold/epithet`; study pages refresh. Anything else diffing is a bug — stop and diagnose.

If `census-of-eyes` should be CI-guarded like the other committed studies, add its `lab run` line to the CI step (follow the existing lines' pattern); if its 10k scale is author-time-only (the census-of-faiths precedent — check whether ci.yml runs faiths), leave CI alone and note it in the study page.

- [ ] **Step 5: Write Study 007**

`book/src/laboratory/study-007.md`, mirroring study-006: question (do two peoples on one sky keep different gods, and is the difference structural rather than lexical?), method (census-of-eyes, 10k, metrics list), the preregistered claims verbatim from the spec §9 WITH their pre-census dates, the measured results (head-domain matrix by activity × lock × mooned-ness, blind-attribution accuracy, the twin-control note: defined here, runs in The Meeting), and the pinned calibration rows.

- [ ] **Step 6: Full gate + commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && mdbook build book`
Expected: green, including the newly pinned calibration rows.

```bash
git add -A
git commit -m "chore(artifacts): the Eyes re-baseline + Study 007 (census of eyes, pinned calibrations)"
```

### Task 12: Book close — freshness sweep, chronicle, retrospective

**Files:**
- Modify: `book/src/domains/religion.md` (goes two-species: per-flagship genesis, the lens in the inputs, the "goblin-only, declared" note falls)
- Modify: `book/src/domains/species.md` (the "nocturnality banked for The Eyes" note flips to "spent in The Eyes"; link the perception chapter)
- Modify: `book/src/domains/perception.md` (Task 1's chapter, updated against merged reality — line-by-line code-vs-prose check, the Y2-1 retrospective's lesson)
- Create: `book/src/chronicle/<next-number>-the-eyes.md` (follow the existing chronicle naming; read the latest entry for voice)
- Modify: `book/src/SUMMARY.md` (chronicle entry)
- Create: `docs/retrospectives/campaign-y2-2.md` (decision 0020 — process lessons, not product; one page)
- Modify: `docs/vision/idea-registry.md` (EXP-3 flips `spec'd` → `shipped`, Where stays on the spec)

- [ ] **Step 1: Freshness sweep**

Sweep every chapter that mentions religion, phenomena, salience, or "goblin-only": `grep -rn "goblin-only\|goblin only\|single species\|one pantheon" book/src/` and check each hit against merged reality. Also re-verify the perception chapter's numbers (lens values, the worked ranking) against the actual seed-42 almanac — regenerate nothing from memory (the Y2-1 lesson: rerun, don't trust stale prose).

- [ ] **Step 2: Chronicle + retrospective**

Chronicle: the campaign's story at book altitude — the inverted Year-1 demo (one sky, two pantheons), venue-as-character, the identity contract holding, what the censuses showed. Retrospective: estimate deltas, evidence-discipline notes, what to do differently (per decision 0020; read `docs/retrospectives/campaign-y2-1.md` for the form).

- [ ] **Step 3: Registry flip**

EXP-3 `spec'd` → `shipped` in `docs/vision/idea-registry.md` (Where keeps pointing at the spec). PSY-2 and SEQ-4 stay `raw` — they are debts this campaign recorded, not work it did.

- [ ] **Step 4: Full gate + book build + docs consistency**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings && mdbook build book && cargo test -p hornvale --test docs_consistency`
Expected: all green.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "docs(book): Campaign Y2-2 close — chronicle, two-species religion, freshness sweep"
```

---

## Self-Review Notes (kept for the executor)

- **Spec coverage:** §3 → Task 3; §4 → Tasks 1, 4, 5; §5 → Tasks 5, 6, 7; §6 → Tasks 4, 9; §7 → Tasks 2, 7, 8, 11; §8 → Tasks 8, 9; §9 → Tasks 10, 11; §10–§12 → Tasks 1, 11, 12; §11's registry rows already exist (committed with the spec).
- **The identity chain has three proofs, run them all when touching Tasks 5–8:** `species_identity` (Y2-1's contract), `eyes_identity` (this campaign's), and the worldgen unit test `goblin_observation_reproduces_the_unlensed_path_bytewise`.
- **Line numbers are as of commit `a27106e`** — re-grep before editing; they will have drifted if tasks land out of order.
- **Any lens-constant retune (Nathan's prerogative, spec §4) invalidates Task 10's pinned counts and Task 11's census numbers** — retune before Task 10 runs, or redo the pins.
