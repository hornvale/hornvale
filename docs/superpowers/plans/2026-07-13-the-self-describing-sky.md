# The Self-Describing Sky — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Commit the astronomy domain's dropped derived facts (star, anchor, moons, neighbors) to the world ledger and build a fact-reading `hornvale explain --world w.json sky` verb that narrates the derivation from the ledger alone (SKY-15; TOOL-1 fact-reading tier).

**Architecture:** Facts are additive on the world entity for singletons (star/anchor) and per-moon parallel lists; neighbors are promoted to minted entities (mirroring `settlement::genesis`) with the opaque `notable-neighbor` Text blob retired under an epoch. A new `windows/explain` crate reads only the committed facts and joins them against a hardcoded derivation DAG to narrate the sky's physics. No new seed streams and no new draws — every committed value is a pure read of an already-computed quantity, so determinism holds by construction.

**Tech Stack:** Rust 2024, workspace crates only (`serde`/`serde_json`/`libm` are the sole allowed externals). Randomness via the kernel `Seed`/`Stream`; no `rand`/`clap`/`chrono`. `cargo nextest` for tests.

## Global Constraints

- **Dependencies:** no new external crates; only `serde`, `serde_json`, `libm` are allowlisted (enforced by `cli/tests/architecture.rs`).
- **Layering:** `kernel → domains/* → windows/* → cli`. A domain depends only on the kernel; a window may depend on kernel/domains/windows. `windows/explain` depends on `hornvale-kernel` + `hornvale-astronomy` only.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only; float sorts use `total_cmp` (enforced by `clippy.toml`).
- **No wall-clock time.** Time is `WorldTime { day: f64 }`.
- **Determinism:** same seed + pins → byte-identical worlds. Numeric fact objects are quantized to 8 significant digits on `Ledger::commit` (do not quantize in the compute path).
- **Docs:** every crate `#![warn(missing_docs)]`; every public item, field, and variant gets a one-line doc comment.
- **Type-audit:** every fact-name constant carries a `type-audit: bare-ok(identifier-text)` tag comment (matching the existing constants in `facts.rs`).
- **Final step before every commit:** `cargo fmt`, then `cargo clippy --workspace --all-targets -- -D warnings`.
- **Facts contract (`facts.rs` header):** downstream systems never read committed facts for dataflow; facts exist for persistence, interrogation, and reconstruction. `explain` is an interrogation consumer and lives in a window, not a domain.

---

### Task 1: Star & anchor facts + the shared insolation formula

**Files:**
- Modify: `domains/astronomy/src/star.rs` (add `insolation_rel`)
- Modify: `domains/astronomy/src/facts.rs` (7 new predicate constants + commits in `genesis`)
- Modify: `domains/astronomy/src/lib.rs:82-164` (`register_concepts` — register the 7 predicates)

**Interfaces:**
- Consumes: `Star { mass: SolarMasses, luminosity: SolarLuminosities, habitable_zone: HabitableZone }`, `Anchor { mass: EarthMasses, orbit: Au }`, `HabitableZone::inner()/outer() -> Au`. All typed getters expose `.get() -> f64`.
- Produces: `pub fn insolation_rel(star: &Star, anchor: &Anchor) -> f64`; the fact-name constants `STAR_MASS_SOLAR`, `STAR_LUMINOSITY_SOLAR`, `HAB_ZONE_INNER_AU`, `HAB_ZONE_OUTER_AU`, `ANCHOR_MASS_EARTH`, `ANCHOR_ORBIT_AU`, `INSOLATION_REL` (all `&str`, in module `facts`).

- [ ] **Step 1: Write the failing test** for the shared formula, in `domains/astronomy/src/star.rs` `#[cfg(test)] mod tests`:

```rust
#[test]
fn insolation_is_luminosity_over_orbit_squared() {
    use crate::anchor::generate_anchor;
    use crate::pins::SkyPins;
    let star = generate_star(Seed(42));
    let anchor = generate_anchor(Seed(42), &star, &SkyPins::default()).unwrap();
    let expected = star.luminosity.get() / (anchor.orbit.get() * anchor.orbit.get());
    assert!((insolation_rel(&star, &anchor) - expected).abs() < 1e-12);
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-astronomy insolation_is_luminosity_over_orbit_squared`
Expected: FAIL — `cannot find function insolation_rel`.

- [ ] **Step 3: Implement `insolation_rel`** in `domains/astronomy/src/star.rs` (after `sun_angular_diameter_rel`). Add `use crate::anchor::Anchor;` at the top if not present:

```rust
/// Top-of-atmosphere stellar flux at the anchor's orbit, relative to Earth's
/// (L / a², Earth = 1). Global annual mean; a genesis-time scalar that does
/// not carry the seasonal (obliquity) or deep-time (eccentricity) variation
/// the forcing parameters model. This is the single definition of insolation
/// the whole workspace shares (SKY-15).
pub fn insolation_rel(star: &Star, anchor: &crate::anchor::Anchor) -> f64 {
    star.luminosity.0 / (anchor.orbit.0 * anchor.orbit.0)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cargo test -p hornvale-astronomy insolation_is_luminosity_over_orbit_squared`
Expected: PASS.

- [ ] **Step 5: Add the 7 predicate constants** to `domains/astronomy/src/facts.rs` (after `OBLIQUITY_AMPLITUDE`, line ~70):

```rust
/// Host star mass in solar masses (functional, Number; drawn).
/// type-audit: bare-ok(identifier-text)
pub const STAR_MASS_SOLAR: &str = "star-mass-solar";
/// Host star luminosity in solar units (functional, Number; derived M^3.5).
/// type-audit: bare-ok(identifier-text)
pub const STAR_LUMINOSITY_SOLAR: &str = "star-luminosity-solar";
/// Habitable-zone inner bound in AU (functional, Number; derived 0.95√L).
/// type-audit: bare-ok(identifier-text)
pub const HAB_ZONE_INNER_AU: &str = "hab-zone-inner-au";
/// Habitable-zone outer bound in AU (functional, Number; derived 1.37√L).
/// type-audit: bare-ok(identifier-text)
pub const HAB_ZONE_OUTER_AU: &str = "hab-zone-outer-au";
/// Anchor world mass in Earth masses (functional, Number; drawn).
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_MASS_EARTH: &str = "anchor-mass-earth";
/// Anchor orbital distance in AU (functional, Number; drawn or pinned).
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_ORBIT_AU: &str = "anchor-orbit-au";
/// Insolation at the anchor relative to Earth (functional, Number; derived
/// L/a², global annual mean).
/// type-audit: bare-ok(identifier-text)
pub const INSOLATION_REL: &str = "insolation-rel";
```

- [ ] **Step 6: Commit the 7 facts** in `facts::genesis` (`domains/astronomy/src/facts.rs`), immediately after the `OBLIQUITY_AMPLITUDE` commit (line ~164), before the `for moon` loop:

```rust
    world.ledger.commit(
        fact(subject, STAR_MASS_SOLAR, Value::Number(system.star.mass.get())),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            STAR_LUMINOSITY_SOLAR,
            Value::Number(system.star.luminosity.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HAB_ZONE_INNER_AU,
            Value::Number(system.star.habitable_zone.inner().get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HAB_ZONE_OUTER_AU,
            Value::Number(system.star.habitable_zone.outer().get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(subject, ANCHOR_MASS_EARTH, Value::Number(system.anchor.mass.get())),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(subject, ANCHOR_ORBIT_AU, Value::Number(system.anchor.orbit.get())),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            INSOLATION_REL,
            Value::Number(crate::star::insolation_rel(&system.star, &system.anchor)),
        ),
        &world.registry,
    )?;
```

- [ ] **Step 7: Register the 7 predicates** in `register_concepts` (`domains/astronomy/src/lib.rs`), after the `OBLIQUITY_AMPLITUDE` registration (line ~164):

```rust
    registry.register_predicate(facts::STAR_MASS_SOLAR, true, "host star mass in solar masses")?;
    registry.register_predicate(
        facts::STAR_LUMINOSITY_SOLAR,
        true,
        "host star luminosity in solar units (derived M^3.5)",
    )?;
    registry.register_predicate(
        facts::HAB_ZONE_INNER_AU,
        true,
        "habitable-zone inner bound in AU (derived 0.95√L)",
    )?;
    registry.register_predicate(
        facts::HAB_ZONE_OUTER_AU,
        true,
        "habitable-zone outer bound in AU (derived 1.37√L)",
    )?;
    registry.register_predicate(facts::ANCHOR_MASS_EARTH, true, "anchor world mass in Earth masses")?;
    registry.register_predicate(facts::ANCHOR_ORBIT_AU, true, "anchor orbital distance in AU")?;
    registry.register_predicate(
        facts::INSOLATION_REL,
        true,
        "insolation at the anchor relative to Earth (derived L/a²)",
    )?;
```

- [ ] **Step 8: Write the fact-presence test** in `facts.rs` `mod tests`:

```rust
#[test]
fn genesis_commits_the_star_and_anchor_numbers() {
    let outcome = generate(Seed(42), &SkyPins::default()).unwrap();
    let mut w = world_with(42);
    let subject = w.ledger.mint_entity();
    genesis(&mut w, subject, &outcome).unwrap();
    for pred in [
        STAR_MASS_SOLAR,
        STAR_LUMINOSITY_SOLAR,
        HAB_ZONE_INNER_AU,
        HAB_ZONE_OUTER_AU,
        ANCHOR_MASS_EARTH,
        ANCHOR_ORBIT_AU,
        INSOLATION_REL,
    ] {
        assert!(w.ledger.value_of(subject, pred).is_some(), "missing {pred}");
    }
    assert_eq!(
        w.ledger.value_of(subject, INSOLATION_REL),
        Some(&Value::Number(hornvale_kernel::quantize(
            crate::star::insolation_rel(&outcome.system.star, &outcome.system.anchor)
        )))
    );
}
```

- [ ] **Step 9: Run the astronomy suite**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS (new + existing tests).

- [ ] **Step 10: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/src/star.rs domains/astronomy/src/facts.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): commit star & anchor derived facts + shared insolation_rel (SKY-15)"
```

---

### Task 2: Route worldgen through the shared insolation formula (DRY)

**Files:**
- Modify: `windows/worldgen/src/lib.rs:~376-377` (replace open-coded `luminosity / (orbit*orbit)`)
- Test: `windows/worldgen/src/lib.rs` `#[cfg(test)]`

**Interfaces:**
- Consumes: `hornvale_astronomy::insolation_rel(&Star, &Anchor)` from Task 1; the `sky`/`system` value available at the `stellar_inputs` site.

This is a behavior-preserving refactor (the same `L/a²`), so the guard is the existing worldgen climate/temperature suite plus a grep that the open-coded copy is gone — not a new fabricated fixture. Do not invent a `ContextBuilder`/`stellar_inputs` test harness that the file does not already have.

- [ ] **Step 1: Read the call site.** Open `windows/worldgen/src/lib.rs` around line 370-386 (`stellar_inputs`). Confirm the in-scope handle for the star & anchor (the function already has `luminosity` and `obliquity` from the sky; find where `star`/`anchor` — or the `sky.system()` they come from — are reachable at this point). Capture the exact expressions for the star and anchor references; you will pass them to `insolation_rel`.

- [ ] **Step 2: Capture the baseline (the test that must still pass).** Run the current worldgen suite and note it is green — this is the regression guard the refactor must preserve:

Run: `cargo test -p hornvale-worldgen 2>&1 | tail -5`
Expected: PASS (record the pass count).

- [ ] **Step 3: Replace the open-coded formula.** At `windows/worldgen/src/lib.rs:~377`, change:

```rust
            // Insolation relative to Earth (L=1, d=1): L / d².
            let insolation = luminosity / (orbit * orbit);
```

to call the shared function on the system's star & anchor (use the exact handles confirmed in Step 1 — e.g. `sky.system().star`, `sky.system().anchor`):

```rust
            // Insolation relative to Earth: the single shared definition (SKY-15).
            let insolation = hornvale_astronomy::insolation_rel(star, anchor);
```

If `luminosity`/`orbit` become unused after this, delete their `let` bindings; if other lines still read them, leave them.

- [ ] **Step 4: Confirm no other copy of the formula remains**

Run: `grep -rn "luminosity / (orbit" windows/worldgen/src/lib.rs`
Expected: no matches (the single source now lives in astronomy).

- [ ] **Step 5: Run the worldgen suite — the baseline must still be green**

Run: `cargo test -p hornvale-worldgen`
Expected: PASS, same count as Step 2 (behavior preserved; insolation values byte-identical because the formula is unchanged).

- [ ] **Step 6: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/src/lib.rs
git commit -m "refactor(worldgen): single-source insolation via astronomy::insolation_rel (SKY-15)"
```

---

### Task 3: Moon facts (mass, distance, angular size)

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (3 constants + commits in the `for moon` loop)
- Modify: `domains/astronomy/src/lib.rs` (`register_concepts` — register 3 predicates)

**Interfaces:**
- Consumes: `Moon { mass: LunarMasses, distance: Megameters, angular_diameter_rel: f64 }`.
- Produces: `MOON_MASS_LUNAR`, `MOON_DISTANCE_MM`, `MOON_ANGULAR_SIZE_REL` constants.

- [ ] **Step 1: Add the 3 constants** to `facts.rs` (after `MOON_INCLINATION_DEGREES`, line ~47):

```rust
/// Mass of a moon in lunar masses (non-functional, Number — one per moon).
/// type-audit: bare-ok(identifier-text)
pub const MOON_MASS_LUNAR: &str = "moon-mass-lunar";
/// Orbital distance of a moon in megameters (non-functional, Number — one per moon).
/// type-audit: bare-ok(identifier-text)
pub const MOON_DISTANCE_MM: &str = "moon-distance-mm";
/// Apparent size of a moon relative to Luna-from-Earth (non-functional, Number
/// — one per moon; derived).
/// type-audit: bare-ok(identifier-text)
pub const MOON_ANGULAR_SIZE_REL: &str = "moon-angular-size-rel";
```

- [ ] **Step 2: Commit them in the `for moon in &system.moons` loop** in `facts::genesis`, alongside the existing period/tide/inclination commits:

```rust
        world.ledger.commit(
            fact(subject, MOON_MASS_LUNAR, Value::Number(moon.mass.get())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(subject, MOON_DISTANCE_MM, Value::Number(moon.distance.get())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                MOON_ANGULAR_SIZE_REL,
                Value::Number(moon.angular_diameter_rel),
            ),
            &world.registry,
        )?;
```

- [ ] **Step 3: Register the 3 predicates** in `register_concepts` (after `MOON_INCLINATION_DEGREES`, line ~134):

```rust
    registry.register_predicate(facts::MOON_MASS_LUNAR, false, "mass of a moon in lunar masses")?;
    registry.register_predicate(
        facts::MOON_DISTANCE_MM,
        false,
        "orbital distance of a moon in megameters",
    )?;
    registry.register_predicate(
        facts::MOON_ANGULAR_SIZE_REL,
        false,
        "apparent size of a moon relative to Luna-from-Earth",
    )?;
```

- [ ] **Step 4: Write the test** in `facts.rs` `mod tests` (mirror `genesis_commits_one_tide_fact_per_moon`):

```rust
#[test]
fn genesis_commits_mass_distance_and_size_per_moon() {
    let pins = SkyPins {
        moons: Some(MoonsPin::exact(2).unwrap()),
        ..SkyPins::default()
    };
    let outcome = generate(Seed(1), &pins).unwrap();
    let mut w = world_with(1);
    let subject = w.ledger.mint_entity();
    genesis(&mut w, subject, &outcome).unwrap();
    for pred in [MOON_MASS_LUNAR, MOON_DISTANCE_MM, MOON_ANGULAR_SIZE_REL] {
        assert_eq!(
            w.ledger.facts_about(subject).filter(|f| f.predicate == pred).count(),
            2,
            "expected 2 {pred} facts"
        );
    }
}
```

- [ ] **Step 5: Run the suite**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS.

- [ ] **Step 6: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/src/facts.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy): commit per-moon mass, distance, angular size (SKY-15)"
```

---

### Task 4: Neighbors become entities (the epoch)

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (retire `NOTABLE_NEIGHBOR`; add 6 neighbor-entity constants; rewrite the neighbor loop in `genesis`; update tests)
- Modify: `domains/astronomy/src/lib.rs` (`register_concepts` — drop `NOTABLE_NEIGHBOR`, register the 6)

**Interfaces:**
- Consumes: `Neighbor { class: NeighborClass, distance: LightYears, apparent_brightness: f64, declination: f64, right_ascension: f64 }`; `crate::neighborhood::class_name(NeighborClass) -> &'static str`.
- Produces: `IS_NEIGHBOR`, `NEIGHBOR_CLASS`, `NEIGHBOR_DISTANCE_LY`, `NEIGHBOR_BRIGHTNESS_REL`, `NEIGHBOR_DECLINATION_DEG`, `NEIGHBOR_RA_DEG` constants; neighbor entities discoverable via `world.ledger.find(IS_NEIGHBOR).map(|f| f.subject)`.

- [ ] **Step 1: Replace the `NOTABLE_NEIGHBOR` constant** in `facts.rs` with the 6 neighbor-entity constants:

```rust
/// Flags a minted entity as a notable neighbor star (functional, Flag).
/// type-audit: bare-ok(identifier-text)
pub const IS_NEIGHBOR: &str = "is-neighbor";
/// Spectral-class name of a neighbor star (functional, Text; one per entity).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_CLASS: &str = "neighbor-class";
/// Distance to a neighbor star in light-years (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_DISTANCE_LY: &str = "neighbor-distance-ly";
/// Apparent brightness of a neighbor, relative units (functional, Number; derived L/d²).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_BRIGHTNESS_REL: &str = "neighbor-brightness-rel";
/// Declination of a neighbor in degrees from the celestial equator (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_DECLINATION_DEG: &str = "neighbor-declination-deg";
/// Right ascension of a neighbor in degrees (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_RA_DEG: &str = "neighbor-ra-deg";
```

- [ ] **Step 2: Rewrite the neighbor loop** in `facts::genesis`. Replace the existing `for neighbor in &system.neighbors { … NOTABLE_NEIGHBOR … }` block with per-entity minting:

```rust
    for neighbor in &system.neighbors {
        let id = world.ledger.mint_entity();
        world.ledger.commit(
            fact(id, IS_NEIGHBOR, Value::Flag(true)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_CLASS,
                Value::Text(crate::neighborhood::class_name(neighbor.class).to_string()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, NEIGHBOR_DISTANCE_LY, Value::Number(neighbor.distance.get())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_BRIGHTNESS_REL,
                Value::Number(neighbor.apparent_brightness),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, NEIGHBOR_DECLINATION_DEG, Value::Number(neighbor.declination)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, NEIGHBOR_RA_DEG, Value::Number(neighbor.right_ascension)),
            &world.registry,
        )?;
    }
```

- [ ] **Step 3: Update `register_concepts`** in `lib.rs`: delete the `NOTABLE_NEIGHBOR` registration and add:

```rust
    registry.register_predicate(facts::IS_NEIGHBOR, true, "a minted entity is a notable neighbor star")?;
    registry.register_predicate(facts::NEIGHBOR_CLASS, true, "spectral-class name of a neighbor star")?;
    registry.register_predicate(facts::NEIGHBOR_DISTANCE_LY, true, "distance to a neighbor star in light-years")?;
    registry.register_predicate(
        facts::NEIGHBOR_BRIGHTNESS_REL,
        true,
        "apparent brightness of a neighbor, relative units (derived L/d²)",
    )?;
    registry.register_predicate(
        facts::NEIGHBOR_DECLINATION_DEG,
        true,
        "declination of a neighbor in degrees from the celestial equator",
    )?;
    registry.register_predicate(facts::NEIGHBOR_RA_DEG, true, "right ascension of a neighbor in degrees")?;
```

- [ ] **Step 4: Update the two affected existing tests** in `facts.rs`:
  - In `genesis_commits_the_expected_facts_for_a_locked_two_moon_system`, replace the `NOTABLE_NEIGHBOR` count assertion with a neighbor-entity count:

```rust
        assert!(
            w.ledger.find(IS_NEIGHBOR).count() >= 2,
            "at least two neighbor entities"
        );
```

  - In `every_committed_fact_has_astronomy_provenance`, broaden the provenance check to include neighbor entities (their facts are on other subjects):

```rust
    #[test]
    fn every_committed_fact_has_astronomy_provenance() {
        let outcome = generate(Seed(3), &SkyPins::default()).unwrap();
        let mut w = world_with(3);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        assert!(w.ledger.iter().all(|f| f.provenance == "astronomy"));
    }
```

- [ ] **Step 5: Add a neighbor-entity shape test**:

```rust
#[test]
fn genesis_mints_one_neighbor_entity_per_neighbor_with_structured_facts() {
    let outcome = generate(Seed(3), &SkyPins::default()).unwrap();
    let mut w = world_with(3);
    let subject = w.ledger.mint_entity();
    genesis(&mut w, subject, &outcome).unwrap();

    let neighbor_ids: Vec<_> = w.ledger.find(IS_NEIGHBOR).map(|f| f.subject).collect();
    assert_eq!(neighbor_ids.len(), outcome.system.neighbors.len());
    for id in neighbor_ids {
        assert!(w.ledger.value_of(id, NEIGHBOR_CLASS).is_some());
        assert!(w.ledger.value_of(id, NEIGHBOR_DISTANCE_LY).is_some());
        assert!(w.ledger.value_of(id, NEIGHBOR_BRIGHTNESS_REL).is_some());
    }
    // The opaque blob is gone.
    assert_eq!(w.ledger.iter().filter(|f| f.predicate == "notable-neighbor").count(), 0);
}
```

- [ ] **Step 6: Run the astronomy suite + the docs/architecture guards**

Run: `cargo test -p hornvale-astronomy`
Then: `cargo test -p hornvale --test architecture` (layering unaffected, but confirm).
Expected: PASS. If any other crate referenced `facts::NOTABLE_NEIGHBOR`, the compile error names it — fix by reading the neighbor entities via `find(IS_NEIGHBOR)`. (Grep first: `grep -rn "NOTABLE_NEIGHBOR" --include=*.rs .` — expect only the deletion site.)

- [ ] **Step 7: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/src/facts.rs domains/astronomy/src/lib.rs
git commit -m "feat(astronomy)!: neighbors become ledger entities; retire notable-neighbor blob (SKY-15 epoch)"
```

---

### Task 5: The `windows/explain` crate — the fact-reading narrator

**Files:**
- Create: `windows/explain/Cargo.toml`
- Create: `windows/explain/src/lib.rs`

**Interfaces:**
- Consumes: `hornvale_kernel::{World, EntityId, Value}`; the astronomy fact-name constants (`hornvale_astronomy::facts::*`).
- Produces: `pub fn explain_sky(world: &World) -> Option<String>` — narrates the sky's derivation from ledger facts, or `None` if the world carries no sky facts.

- [ ] **Step 1: Create `windows/explain/Cargo.toml`**:

```toml
[package]
name = "hornvale-explain"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale explain window: narrate a world's derivation from its ledger."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
hornvale-astronomy = { path = "../../domains/astronomy" }
```

- [ ] **Step 2: Write the failing test.** Create `windows/explain/src/lib.rs` with only the test module first (so the crate compiles a failing test):

```rust
//! The explain window: narrate a world's derivation by reading its committed
//! facts and joining them against the known derivation DAG. Reads only the
//! ledger — never the in-memory system — which is how it validates that the
//! ledger is self-describing (SKY-15 / TOOL-1 fact-reading tier).
#![warn(missing_docs)]

use hornvale_astronomy::facts;
use hornvale_kernel::{EntityId, Value, World};

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_astronomy::register_concepts;
    use hornvale_astronomy::system::generate;
    use hornvale_astronomy::pins::SkyPins;
    use hornvale_kernel::Seed;

    fn world_with_sky(seed: u64) -> World {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
        let subject = w.ledger.mint_entity();
        facts::genesis(&mut w, subject, &outcome).unwrap();
        w
    }

    #[test]
    fn explain_sky_narrates_the_insolation_chain_from_the_ledger() {
        let w = world_with_sky(42);
        let text = explain_sky(&w).expect("a world with sky facts explains");
        assert!(text.contains("sunlight") || text.contains("insolation"));
        assert!(text.contains("luminosity"));
        assert!(text.contains("M"), "names the star mass");
        // The narration is derived purely from committed facts.
    }

    #[test]
    fn explain_sky_is_none_without_sky_facts() {
        let w = World::new(Seed(7)); // empty ledger
        assert!(explain_sky(&w).is_none());
    }

    #[test]
    fn explain_sky_reads_the_ledger_only_not_the_system() {
        // A world reconstructed from JSON has no in-memory System — only facts.
        let w = world_with_sky(1);
        let json = w.to_json();
        let reloaded = World::from_json(&json).unwrap();
        assert_eq!(explain_sky(&w), explain_sky(&reloaded));
    }
}
```

Note: confirm `World::from_json`/`to_json` names against `kernel/src` (the almanac/CLI use `World::load`/`to_json`); if the in-memory round-trip helper differs, use whatever `cli`'s `load_world` path uses. If no string round-trip exists, drop the third test's reload and instead assert `explain_sky` references only `world.ledger` (it structurally cannot touch a system it never receives).

- [ ] **Step 3: Run test to verify it fails**

Run: `cargo test -p hornvale-explain`
Expected: FAIL — `cannot find function explain_sky`.

- [ ] **Step 4: Implement `explain_sky`.** Add above the test module in `windows/explain/src/lib.rs`:

```rust
/// Locate the world entity: the unique subject carrying a `star-class` fact.
fn world_entity(world: &World) -> Option<EntityId> {
    world.ledger.find(facts::STAR_CLASS).map(|f| f.subject).next()
}

/// Read a functional Number fact off `subject`.
fn num(world: &World, subject: EntityId, predicate: &str) -> Option<f64> {
    match world.ledger.value_of(subject, predicate) {
        Some(Value::Number(n)) => Some(*n),
        _ => None,
    }
}

/// Read a functional Text fact off `subject`.
fn text(world: &World, subject: EntityId, predicate: &str) -> Option<String> {
    world.ledger.text_of(subject, predicate).map(str::to_string)
}

/// Narrate the sky's derivation chain from the world's committed facts.
/// `None` if the world has no generated sky. Each node is tagged with its
/// provenance in the derivation DAG (rolled / derived / pinned) and its value
/// read from the ledger; the join of DAG and values is the explanation.
pub fn explain_sky(world: &World) -> Option<String> {
    let e = world_entity(world)?;
    let class = text(world, e, facts::STAR_CLASS)?;
    let star_mass = num(world, e, facts::STAR_MASS_SOLAR)?;
    let luminosity = num(world, e, facts::STAR_LUMINOSITY_SOLAR)?;
    let zone_in = num(world, e, facts::HAB_ZONE_INNER_AU)?;
    let zone_out = num(world, e, facts::HAB_ZONE_OUTER_AU)?;
    let anchor_mass = num(world, e, facts::ANCHOR_MASS_EARTH)?;
    let orbit = num(world, e, facts::ANCHOR_ORBIT_AU)?;
    let insolation = num(world, e, facts::INSOLATION_REL)?;

    // Was the orbit pinned? The DAG's orbit leaf is pinned iff a year-days
    // scenario-pin was committed.
    let pinned_orbit = world
        .ledger
        .facts_about(e)
        .any(|f| f.predicate == facts::SCENARIO_PIN && matches!(&f.object, Value::Text(t) if t.contains("year")));
    let orbit_tag = if pinned_orbit { "pinned" } else { "rolled" };

    let edge = if orbit < (zone_in + zone_out) / 2.0 { "the warm edge" } else { "the cool edge" };

    let mut out = String::new();
    out.push_str(&format!(
        "This world receives {insolation:.2}× Earth's sunlight (insolation, global annual mean).\n"
    ));
    out.push_str(&format!(
        "Its star is a {class} — mass {star_mass:.2} M☉ (rolled) — giving luminosity \
         {luminosity:.2} L☉ (derived, L = M³·⁵) and a habitable zone of {zone_in:.2}–{zone_out:.2} AU \
         (derived, 0.95√L–1.37√L).\n"
    ));
    out.push_str(&format!(
        "The anchor world — mass {anchor_mass:.2} M⊕ (rolled) — orbits at {orbit:.2} AU ({orbit_tag}), \
         so insolation = {luminosity:.2} / {orbit:.2}² = {insolation:.2} (derived, L/a²), near {edge} \
         of the zone.\n"
    ));

    // Moons: parallel lists on the world entity, distance-sorted.
    let masses: Vec<f64> = world
        .ledger
        .facts_about(e)
        .filter(|f| f.predicate == facts::MOON_MASS_LUNAR)
        .filter_map(|f| match f.object { Value::Number(n) => Some(n), _ => None })
        .collect();
    if masses.is_empty() {
        out.push_str("It has no moons.\n");
    } else {
        out.push_str(&format!("It has {} moon(s) (rolled count).\n", masses.len()));
    }

    // Neighbors: one entity each, discovered by the is-neighbor flag.
    let neighbor_count = world.ledger.find(facts::IS_NEIGHBOR).count();
    out.push_str(&format!(
        "{neighbor_count} notable neighbor star(s) stand fixed in its night sky (rolled).\n"
    ));

    Some(out)
}
```

Note: if `World` has no `find` re-export at the ledger, call `world.ledger.find(...)` (confirmed present: `Ledger::find`). Use ASCII fallbacks for `☉`/`⊕`/`³·⁵` only if a fmt/encoding test objects; the gallery is UTF-8, so the glyphs are fine.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p hornvale-explain`
Expected: PASS.

- [ ] **Step 6: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-explain --all-targets -- -D warnings
git add windows/explain/Cargo.toml windows/explain/src/lib.rs
git commit -m "feat(explain): fact-reading sky narrator (TOOL-1 fact-reading tier)"
```

---

### Task 6: Wire the `explain` CLI verb + commit the artifact

**Files:**
- Modify: `cli/Cargo.toml` (add `hornvale-explain` dependency)
- Modify: `cli/src/main.rs` (dispatch `explain`; `cmd_explain`; usage line)
- Modify: `scripts/regenerate-artifacts.sh` (emit the new gallery page)
- Modify: `.github/workflows/ci.yml` (the "Artifacts are current" step already diffs `book/src/gallery/`; add the generate line)
- Create: `book/src/gallery/explain-seed-42-sky.md` (generated)

**Interfaces:**
- Consumes: `hornvale_explain::explain_sky(&World)`; `load_world(args)`.

- [ ] **Step 1: Add the dependency** to `cli/Cargo.toml` (alongside the other `hornvale-*` window deps):

```toml
hornvale-explain = { path = "../windows/explain" }
```

- [ ] **Step 2: Add dispatch + command** in `cli/src/main.rs`. In the `match` (near line 98), add:

```rust
        Some("explain") => cmd_explain(&args),
```

Then add the function (near `cmd_almanac`):

```rust
/// The first positional (non-flag) argument after the subcommand, skipping
/// `--world <value>` and any other `--flag`. `None` if only flags are present.
fn positional_target(args: &[String]) -> Option<&str> {
    let mut it = args.iter().skip(1); // skip the "explain" subcommand token
    while let Some(a) = it.next() {
        if a == "--world" {
            it.next(); // consume the flag's value
            continue;
        }
        if a.starts_with("--") {
            continue;
        }
        return Some(a.as_str());
    }
    None
}

fn cmd_explain(args: &[String]) -> Result<(), String> {
    // Only "sky" is supported this campaign; default to it when omitted.
    let target = positional_target(args).unwrap_or("sky");
    if target != "sky" {
        return Err(format!("explain: unknown target '{target}' (only 'sky' is supported)"));
    }
    let world = load_world(args)?;
    let out = hornvale_explain::explain_sky(&world)
        .ok_or("this world has no generated sky to explain")?;
    print!("{out}");
    Ok(())
}
```

- [ ] **Step 3: Add the usage line.** Find the `usage()` string in `cli/src/main.rs` and add, near the `almanac` entry:

```
  explain --world <PATH> sky   narrate the sky's derivation from the ledger
```

- [ ] **Step 4: Build + smoke-test the verb**

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv-explain.json
cargo run -p hornvale -- explain --world /tmp/hv-explain.json sky
```

Expected: prints the insolation/luminosity/anchor narration; exit 0.

- [ ] **Step 5: Generate the committed artifact**

```bash
cargo run -p hornvale -- explain --world /tmp/hv-explain.json sky > book/src/gallery/explain-seed-42-sky.md
```

- [ ] **Step 6: Add the generate line to `scripts/regenerate-artifacts.sh`.** After the almanac block (line ~50), using the existing `$wsky` world variable already built at line 44:

```bash
echo "regenerate-artifacts: explain" >&2
run -p hornvale -- explain --world "$wsky" sky > book/src/gallery/explain-seed-42-sky.md
```

- [ ] **Step 7: Confirm CI covers it.** `.github/workflows/ci.yml` line ~57 already runs `git diff --exit-code book/src/gallery/ …`, so the new page is drift-checked once it is generated by the script. Add the same `explain` generate command to the CI "Artifacts are current" step's command list (mirror where `almanac … > book/src/gallery/almanac-seed-42-sky.md` appears) so CI regenerates it before diffing.

- [ ] **Step 8: Run the regenerate script end-to-end to confirm no drift**

```bash
SKIP_CENSUS=1 ./scripts/regenerate-artifacts.sh
git diff --exit-code book/src/gallery/
```

Expected: no diff (the committed page matches regeneration).

- [ ] **Step 9: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale --all-targets -- -D warnings
git add cli/Cargo.toml cli/src/main.rs scripts/regenerate-artifacts.sh .github/workflows/ci.yml book/src/gallery/explain-seed-42-sky.md
git commit -m "feat(cli): hornvale explain sky verb + committed gallery artifact (SKY-15/TOOL-1)"
```

---

### Task 7: Re-source the deity name seed to semantic identity (naming epoch)

**Files:**
- Modify: `windows/worldgen/src/lib.rs` — add a pure `deity_name_seed` helper; add a `deity_seed: Seed` field to `LanguageDeityNamer`; use the helper in `LanguageDeityNamer::deity` and `::epithet`; set the field at the namer's construction site (search for `LanguageDeityNamer {`).
- Test: `windows/worldgen/src/lib.rs` test module.

**Interfaces:**
- Consumes: `hornvale_kernel::Seed` (`.derive(&str) -> Seed`, `.stream().next_u64() -> u64`); `LanguageDeityNamer` fields (`namer`, `morph`, `lexicon`, `phenomena`, `index`, `glosses`); `Phenomenon.kind`.
- Produces: `fn deity_name_seed(base: Seed, kind: &str, rank: usize) -> u64` (pure, entity-id-free).

**Background (why):** today `LanguageDeityNamer::deity(salt)` / `::epithet(salt)` pass the belief-entity-id `salt` straight into `hornvale_language::glossed_name(kind, salt, …)`, so deity names are seeded by entity mint order and drift whenever unrelated entities (this campaign's neighbor entities) are minted earlier. This task re-seeds names from the phenomenon's semantic identity. **`salt` stays the gloss key** — `self.glosses.insert(salt, …)` and the `name_gloss_fact(EntityId(salt), …)` commit are UNCHANGED, so glosses stay attached to belief entities and `recount`/`explain` do not regress. `domains/religion` is not touched.

- [ ] **Step 1: Write the failing keystone test** in the `windows/worldgen/src/lib.rs` test module:

```rust
#[test]
fn deity_name_seed_is_pure_and_entity_id_free() {
    use hornvale_kernel::Seed;
    let base = Seed(42).derive("religion/deity/v2").derive("goblin");
    // Same species-seed + kind + rank -> same name seed, no entity id involved.
    assert_eq!(
        deity_name_seed(base, "celestial-body", 0),
        deity_name_seed(base, "celestial-body", 0)
    );
    // Rank disambiguates members that share a kind (two moons, two same-colour stars).
    assert_ne!(
        deity_name_seed(base, "celestial-body", 0),
        deity_name_seed(base, "celestial-body", 1)
    );
    // Kind leads semantically.
    assert_ne!(
        deity_name_seed(base, "celestial-body", 0),
        deity_name_seed(base, "tide", 0)
    );
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen deity_name_seed_is_pure`
Expected: FAIL — `cannot find function deity_name_seed`.

- [ ] **Step 3: Add the pure helper** (near `LanguageDeityNamer`, `windows/worldgen/src/lib.rs`). Ensure `use hornvale_kernel::Seed;` is in scope (it almost certainly already is at the crate top):

```rust
/// The seed for a deity's generated name: a pure function of the per-species
/// deity seed (`base`), the phenomenon KIND the deity is of, and the
/// phenomenon's RANK among its pantheon's members. Deliberately carries no
/// entity id, so deity names are invariant to entity mint order — the fix
/// for the `/v2` naming epoch (spec §8).
fn deity_name_seed(base: Seed, kind: &str, rank: usize) -> u64 {
    base.derive(kind).derive(&rank.to_string()).stream().next_u64()
}
```

- [ ] **Step 4: Run to verify the test passes**

Run: `cargo test -p hornvale-worldgen deity_name_seed_is_pure`
Expected: PASS.

- [ ] **Step 5: Add the `deity_seed` field and use it.** In `LanguageDeityNamer`'s struct definition add:

```rust
    /// Per-species base seed for deity name generation (`/v2` epoch): the name
    /// seed is derived from this + phenomenon kind + rank, never an entity id.
    deity_seed: Seed,
```

At the construction site (`let mut deity_namer = LanguageDeityNamer {`), add the field, derived from the in-scope root world seed and the species name (`def.name`):

```rust
        deity_seed: seed.derive("religion/deity/v2").derive(def.name),
```

(Use whatever the in-scope binding for the root world `Seed` is — likely `seed`. If it is not reachable at that site, thread it in from the enclosing genesis function; do not use `world.seed` after `world` is mutably borrowed.)

- [ ] **Step 6: Use the helper in `deity()`** — capture the rank before the `self.index += 1`, and pass the derived seed to `glossed_name` instead of `salt` (leave the `glosses.insert(salt, gloss)` line exactly as is):

```rust
    fn deity(&mut self, salt: u64) -> (String, String) {
        let rank = self.index;
        let phenomenon = self
            .phenomena
            .get(rank)
            .expect("religion calls deity() once per member phenomenon, in phenomena order");
        self.index += 1;
        let sentiment = hornvale_religion::Sentiment::of(phenomenon);
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts { concepts: &concepts };
        let name_seed = deity_name_seed(self.deity_seed, &phenomenon.kind, rank);
        let (g, gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Deity,
            name_seed,
            &self.morph,
            &site,
            self.lexicon,
        );
        if !gloss.is_empty() {
            self.glosses.insert(salt, gloss);
        }
        (g.roman, g.ipa)
    }
```

- [ ] **Step 7: Use the helper in `epithet()`** — same phenomenon and rank the paired `deity()` used (`self.index - 1`), pass the derived seed to `glossed_name`:

```rust
    fn epithet(&mut self, salt: u64, sentiment: hornvale_religion::Sentiment) -> (String, String) {
        let rank = self.index - 1;
        let phenomenon = self
            .phenomena
            .get(rank)
            .expect("deity() always runs before epithet() for the same belief");
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts { concepts: &concepts };
        let name_seed = deity_name_seed(self.deity_seed, &phenomenon.kind, rank);
        let (g, _gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Epithet,
            name_seed,
            &self.morph,
            &site,
            self.lexicon,
        );
        (g.roman, g.ipa)
    }
```

The `salt` parameter is now used only as the gloss key (`deity`) and is unused in `epithet` — prefix it `_salt` in `epithet` if clippy flags it, but do NOT change the `DeityNamer` trait signature.

- [ ] **Step 8: Run the worldgen suite; re-pin any in-crate tests that assert specific deity-name strings.**

Run: `cargo test -p hornvale-worldgen 2>&1 | tee /tmp/hv-wg.txt`
Expected: the new keystone test passes. Any EXISTING worldgen unit test that asserts a specific deity/epithet name string will now FAIL — the names changed deterministically (that is the epoch). For each, read the new deterministic value from the failure output and re-pin the expected string. **Do not weaken the assertion** to dodge the change; pin the new exact name. (Almanac/world *fixtures* are rebaselined later in Task 8 — this step is only the `-p hornvale-worldgen` in-crate tests.)

- [ ] **Step 9: fmt + clippy, then commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen/src/lib.rs
git commit -m "feat(worldgen)!: seed deity names from phenomenon identity, not entity id (naming /v2 epoch)"
```

---

### Task 8: Rebaseline, book, registry, decisions, retrospective (Definition of Done)

**Files:**
- Rebaseline: `cli/tests/fixtures/world-seed-42.json` (golden world ledger — new facts + stable deity names)
- Rebaseline: `book/src/gallery/almanac-seed-42.md`, `-sky.md`, `-locked.md` (stable deity names)
- Rebaseline: `book/src/reference/concept-registry-generated.md` (new predicates), `docs/audits/type-audit-report.md` (new constants)
- Create: `book/src/chronicle/<next-entry>.md` (+ add to `book/src/SUMMARY.md`)
- Modify: `book/src/frontier/idea-registry.md` (flip SKY-15, TOOL-1; note SKY-19)
- Create: `docs/decisions/<N>-entity-hood-for-collections.md` and `docs/decisions/<N+1>-name-salt-stable-identity.md`
- Modify: `book/src/open-questions.md` (Confidence Gradient re-score, if SKY-15/TOOL-1 sits on a bet)
- Create: `docs/retrospectives/<campaign>.md`

**Interfaces:** none (docs + regenerated artifacts).

**Precondition:** Task 7 (deity-name seed) is committed, so every artifact below is regenerated ONCE with names already stable.

- [ ] **Step 1: The single full rebaseline.** Regenerate every artifact this campaign drifts and confirm the ONLY changes are the intended ones (new facts, new predicates, new constants, stable-but-changed deity names — never a physics/period change):

```bash
# The whole committed-artifact set (censuses skipped by default):
bash scripts/regenerate-artifacts.sh
# The golden world fixture the lens_purity test byte-checks (deliberate, per its module doc):
cargo run -p hornvale -- new --seed 42 --out cli/tests/fixtures/world-seed-42.json
# The type-audit report (new pub fact constants):
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
git status   # review EVERY changed file below before staging
```

Expected drift, all intended: `world-seed-42.json` gains the star/anchor/moon/neighbor facts and loses `notable-neighbor`; the three almanacs show new deity names (periods/physics identical); `concept-registry-generated.md` gains the new predicates; `type-audit-report.md` gains the new constants; `explain-seed-42-sky.md` present (Task 6). Confirm no numeric/period value moved (that would signal a real bug, not a rebaseline). Match the real committed filenames — `git status` names them.

- [ ] **Step 1b: Prove determinism + the deity-name fix held.** 

```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/a.json && cargo run -p hornvale -- new --seed 42 --out /tmp/b.json && diff /tmp/a.json /tmp/b.json && echo "byte-identical"
cargo test -p hornvale --test lens_purity   # now passes against the rebaselined fixture
```
Expected: byte-identical; `lens_purity` passes.

- [ ] **Step 2: Write the chronicle entry** `book/src/chronicle/<slug>.md` at the project's chronicle altitude (technical, comprehensible without the code). It must record: (a) SKY-15's remaining fact surface now committed (star/anchor/moons); (b) neighbors promoted to ledger entities and the `notable-neighbor` blob retired (the epoch); (c) the new `explain sky` verb and its DAG⋈values design; (d) the single shared `insolation_rel`. Add the entry to `book/src/SUMMARY.md` under the Chronicle part. Follow the format of the most recent chronicle entry in `book/src/chronicle/`.

- [ ] **Step 3: Flip the registry rows** in `book/src/frontier/idea-registry.md`:
  - **SKY-15**: change status `raw` → `shipped`; repoint **Where** at `docs/superpowers/specs/2026-07-13-the-self-describing-sky-design.md` (GitHub blob URL form); update the row text to state the fact surface and neighbor entities shipped.
  - **TOOL-1**: change status `elaborated` → `spec'd`; note the fact-reading tier shipped (the `explain sky` verb) and that the trace-replay tier remains open; add a blob-URL pointer to this spec.
  - **SKY-19**: append a note that edge-of-zone is now answerable from the ledger (hab-zone bounds + insolation facts). Do not change its status.

- [ ] **Step 4: Write the decision records** (next free numbers; follow `docs/decisions/README.md` format):
  - `<N>-entity-hood-for-collections.md`: **collections become ledger entities, singletons stay flat on the world entity**; neighbors and (future) moons are collections; moons grandfathered flat this campaign, an explicit debt. Also record the precise `insolation-rel` definition (TOA `L/a²`, Earth = 1, global annual-mean, genesis-time scalar).
  - `<N+1>-name-salt-stable-identity.md`: **procedural names must be salted by stable identity, never by a global mint counter.** Deity names were seeded by the belief entity id (mint-order-coupled); re-sourced to a semantic derivation over (species, phenomenon kind, rank) under the `religion/deity/v2` epoch. Record the general principle so future namers (and any reviewer) avoid the entity-id-salt trap. Cross-reference the neighbor-epoch that exposed it.

- [ ] **Step 5: Confidence Gradient re-score.** Open `book/src/open-questions.md`; if SKY-15 or TOOL-1 is named in a bet, re-score that chapter per decision 0030. If neither appears, note in the chronicle that no gradient bet moved.

- [ ] **Step 6: Write the retrospective** `docs/retrospectives/<campaign>.md` (process lessons, not product): the ideonomy pass caught the anchor-mass/hab-zone gaps and the DAG⋈values framing; the registry line was partly stale (much of SKY-15 had shipped incrementally) — a lesson about grounding a `raw` row against code before scoping.

- [ ] **Step 7: Build the book + run the docs drift-check**

```bash
mdbook build book
cargo test -p hornvale --test docs_consistency
```

Expected: book builds; docs-consistency passes (all registry cross-links resolve, ToC complete, IDs unique).

- [ ] **Step 8: Commit** (the rebaselined artifacts land with the docs — one campaign-close commit):

```bash
git add book/ docs/decisions/ docs/retrospectives/ \
        cli/tests/fixtures/world-seed-42.json docs/audits/type-audit-report.md
git commit -m "docs(chronicle,frontier,decisions)+rebaseline: The Self-Describing Sky close (SKY-15 shipped, TOOL-1 spec'd, deity-name /v2 epoch)"
```

---

## Final verification (run before declaring the campaign done)

- [ ] **Full commit gate:** `make gate` (fmt + clippy + nextest + doctests) passes.
- [ ] **Artifact freshness:** `SKIP_CENSUS=1 ./scripts/regenerate-artifacts.sh && git diff --exit-code book/src/gallery/ book/src/reference/ cli/tests/fixtures/ docs/audits/` clean.
- [ ] **Determinism spot-check:** `cargo run -p hornvale -- new --seed 42 --out /tmp/a.json && cargo run -p hornvale -- new --seed 42 --out /tmp/b.json && diff /tmp/a.json /tmp/b.json` — identical.
- [ ] **Deity-name stability:** the seed-42 almanac deity names are unchanged by an unrelated entity-count change — spot-check that `--neighbor red-giant` (which alters a neighbor entity) leaves the flagship's deity names identical to the default run (they now depend only on phenomenon identity).
- [ ] **Type-audit:** `cargo run --manifest-path tools/type-audit/Cargo.toml -- check` passes (all new fact constants carry `type-audit:` tags).
- [ ] **Spec coverage:** every §2–§10 spec section maps to a task above.
