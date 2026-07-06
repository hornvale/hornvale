# Campaign 2a: Phase 0 & System Genesis — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land the ratified Phase-0 chores (kernel ergonomics, provider centralization, stream-manifest discipline) and the anchor-first star-system generator — star, anchor world, moons, stellar neighborhood — with pins, stability inequalities, and a property-test battery.

**Architecture:** Phase 0 refactors existing crates without behavior change (byte-identical almanac before/after is the acceptance test). The generator is new code inside `hornvale-astronomy`: pure functions from `(Seed, &SkyPins)` to `Result<StarSystem, GenesisError>`, no facts committed yet (Plan 2b wires genesis into worlds). Physics per the spec's model card (§5): derived quantities use real formulas; approximations and draws are exactly as listed there.

**Tech Stack:** Rust edition 2024. No new dependencies anywhere.

## Global Constraints

- Spec: `docs/superpowers/specs/2026-07-05-campaign-2-the-sky-design.md`. Parent constitution governs; domain crates depend ONLY on `hornvale-kernel`.
- **Units convention (used verbatim everywhere):** star mass in solar masses; luminosity in solar; planet orbits in AU; anchor mass in Earth masses; day lengths in standard hours; moon mass in lunar masses (Luna = 1); moon distance in Mm (1 Mm = 1000 km; Luna = 384.4); periods in standard days; neighbor distance in light-years.
- **Physics formulas (save-format contracts once shipped):** luminosity `L = M^3.5`; habitable zone `(0.95·√L, 1.37·√L)` AU; planet year `365.25·√(a³/M_star)` std days; moon period `27.32·√((d/384.4)³ / M_anchor_earths)` std days; moon angular diameter (relative to Luna) `m^(1/3)·384.4/d`; moon tide (relative to Luna) `m/(d/384.4)³`; Hill radius `a_au·(M_earths·3.003e-6/(3·M_star))^(1/3)·1.496e5` Mm; neighbor apparent brightness `L_class/d_ly²`.
- **Stability inequalities:** moon distance ≥ 20 Mm (Roche floor) and ≤ 0.4·Hill radius; adjacent moon orbit ratio ≥ 1.5; combined tide ≤ 8.0.
- **Draw ranges:** star mass 0.6–1.4; anchor mass 0.5–2.0; day length 16–40 h (5% locked when undrawn); obliquity 0–35°; moon count weighted {0:15, 1:40, 2:30, 3:15}; moon mass 0.05–2.5; moon distance 60–min(900, 0.4·Hill) Mm; neighbors 2–5 at 4–80 ly.
- **Seed-derivation labels are permanent contracts**; every label is declared as a constant and exposed via each crate's `stream_labels()` (Task 3).
- Pins fail loudly: `GenesisError` carries the pin name and the physical reason. The generator never silently bends.
- Phase 0 must not change behavior: the committed seed-42 almanac and concepts artifacts must be byte-identical after Tasks 1–4 (CI's drift check enforces this; do not regenerate them).
- Every commit passes: `cargo test --workspace`, `cargo fmt --check`, `cargo clippy --workspace --all-targets -- -D warnings`. New public items: one-line doc comments (crates have `#![warn(missing_docs)]`).

## File Structure

```
kernel/src/ledger.rs                 — MODIFY: text_of accessor
kernel/src/world.rs                  — MODIFY: pub const NAME
kernel/src/lib.rs                    — MODIFY: re-export NAME
domains/*/src/lib.rs                 — MODIFY: use kernel text_of/NAME; derives on report structs; stream_labels()
cli/src/world_builder.rs             — MODIFY: sky_report/climate_report accessors
cli/src/repl.rs                      — MODIFY: presentation-only (accessors)
cli/src/streams.rs                   — CREATE: render_streams() for the manifest
cli/src/main.rs                      — MODIFY: `streams` command
book/src/reference/stream-manifest.md            — CREATE: prose + include
book/src/reference/stream-manifest-generated.md  — GENERATED + committed
book/src/SUMMARY.md                  — MODIFY: reference entry
.github/workflows/ci.yml             — MODIFY: streams drift check
domains/astronomy/src/lib.rs         — MODIFY: module decls + re-exports
domains/astronomy/src/pins.rs        — CREATE: SkyPins, RotationPin, NeighborClass, GenesisError
domains/astronomy/src/streams.rs     — CREATE: astronomy stream-label constants
domains/astronomy/src/star.rs        — CREATE: Star, generate_star
domains/astronomy/src/anchor.rs      — CREATE: Anchor, Rotation, generate_anchor
domains/astronomy/src/moons.rs       — CREATE: Moon, generate_moons, stability checks
domains/astronomy/src/neighborhood.rs — CREATE: Neighbor, generate_neighbors
domains/astronomy/src/system.rs      — CREATE: StarSystem, generate()
domains/astronomy/tests/genesis_properties.rs — CREATE: property battery
```

---

### Task 1: Kernel ergonomics

**Files:**
- Modify: `kernel/src/ledger.rs`, `kernel/src/world.rs`, `kernel/src/lib.rs`
- Modify: `domains/terrain/src/lib.rs`, `domains/settlement/src/lib.rs`, `domains/religion/src/lib.rs` (use the new accessors)
- Modify: `domains/astronomy/src/lib.rs`, `domains/climate/src/lib.rs`, `domains/terrain/src/lib.rs`, `domains/settlement/src/lib.rs`, `domains/religion/src/lib.rs` (derives on report structs)

**Interfaces:**
- Produces: `kernel::NAME: &str = "name"` (the core predicate, re-exported from `world`); `Ledger::text_of(&self, subject: EntityId, predicate: &str) -> Option<&str>`; `#[derive(Debug, Clone, PartialEq)]` on `SkyReport`, `ClimateReport`, `PlaceInfo`, `VillageInfo`, `Belief`.
- Behavior must not change: seed-42 artifacts stay byte-identical.

- [ ] **Step 1: Write the failing kernel tests**

Add to the `tests` module in `kernel/src/ledger.rs`:

```rust
    #[test]
    fn text_of_returns_text_values_only() {
        let r = registry();
        let mut l = Ledger::default();
        let f = named(&mut l, "Zaggrak");
        let subject = f.subject;
        l.commit(f, &r).unwrap();
        assert_eq!(l.text_of(subject, "name"), Some("Zaggrak"));
        assert_eq!(l.text_of(subject, "located-in"), None);
    }
```

Add to the `tests` module in `kernel/src/world.rs`:

```rust
    #[test]
    fn name_constant_is_the_registered_core_predicate() {
        let w = World::new(Seed(1));
        assert!(w.registry.predicate(crate::world::NAME).unwrap().functional);
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-kernel text_of && cargo test -p hornvale-kernel name_constant`
Expected: compile errors — `text_of`, `NAME` not defined.

- [ ] **Step 3: Implement**

In `kernel/src/ledger.rs`, add to `impl Ledger`:

```rust
    /// The text value of (subject, predicate), if present and textual.
    pub fn text_of(&self, subject: EntityId, predicate: &str) -> Option<&str> {
        match self.value_of(subject, predicate) {
            Some(Value::Text(t)) => Some(t.as_str()),
            _ => None,
        }
    }
```

In `kernel/src/world.rs`, above `World`:

```rust
/// The core name predicate, registered by `World::new` for every world.
pub const NAME: &str = "name";
```

and change the registration line in `World::new` to use it:

```rust
        registry
            .register_predicate(NAME, true, "canonical name of an entity")
            .expect("core concept registration cannot conflict in an empty registry");
```

In `kernel/src/lib.rs`, extend the world re-export: `pub use world::{NAME, World};`

- [ ] **Step 4: Sweep the domains**

In `domains/terrain/src/lib.rs` and `domains/religion/src/lib.rs`: delete the
private `fn text_of(...)` helpers and replace their call sites with
`world.ledger.text_of(id, ...)` (terrain: `world.ledger.text_of(id, hornvale_kernel::NAME)` and `text_of(id, BIOME)`; religion: `text_of(id, TENET)` and `text_of(id, DERIVED_FROM_PHENOMENON)`, mapping to owned `String` with `.map(str::to_string)` where the struct needs ownership).

In `domains/settlement/src/lib.rs`: replace the two `"name"` string literals
with `hornvale_kernel::NAME`, and the inline name match in `village_info`
with `world.ledger.text_of(id, hornvale_kernel::NAME).map(str::to_string).unwrap_or_else(|| format!("settlement {}", id.0))`.

Add `#[derive(Debug, Clone, PartialEq)]` to: `SkyReport` (astronomy),
`ClimateReport` (climate), `PlaceInfo` (terrain), `VillageInfo` (settlement),
`Belief` (religion).

- [ ] **Step 5: Verify no behavior change, gate, commit**

Run: `cargo test --workspace && cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Then regenerate artifacts WITHOUT committing them and confirm no diff:
`cargo run -q -p hornvale -- new --seed 42 --out /tmp/hv-2a.json && cargo run -q -p hornvale -- almanac --world /tmp/hv-2a.json | diff - book/src/gallery/almanac-seed-42.md && cargo run -q -p hornvale -- concepts | diff - book/src/reference/concept-registry-generated.md`
Expected: all pass, both diffs empty.

```bash
git add kernel/ domains/
git commit -m "refactor(kernel): NAME constant, Ledger::text_of, report-struct derives"
```

---

### Task 2: Provider centralization

**Files:**
- Modify: `cli/src/world_builder.rs` (accessors + tests)
- Modify: `cli/src/repl.rs` (use accessors)

**Interfaces:**
- Produces: `world_builder::sky_report(world: &World, time: WorldTime) -> SkyReport`; `world_builder::climate_report(world: &World) -> ClimateReport`. After this task, `ConstantSun`/`UniformClimate` are constructed ONLY inside `world_builder` — Plan 2b swaps the provider in exactly one file.

- [ ] **Step 1: Write the failing tests**

Add to the `tests` module in `cli/src/world_builder.rs`:

```rust
    #[test]
    fn sky_and_climate_reports_come_from_the_composition_root() {
        let world = build_world(Seed(42)).unwrap();
        let sky = sky_report(&world, hornvale_kernel::WorldTime { day: 0.0 });
        assert!(sky.description.contains("zenith"));
        let climate = climate_report(&world);
        assert_eq!(climate.temperature_c, 18.0);
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale sky_and_climate`
Expected: compile error — functions not defined.

- [ ] **Step 3: Implement**

In `cli/src/world_builder.rs` (imports: add `hornvale_astronomy::SkyReport`, `hornvale_climate::ClimateReport`):

```rust
/// The sky at `time`, from whichever astronomy provider this world uses.
/// The single construction site for the provider (Constitution §2.4 tiers).
pub fn sky_report(_world: &World, time: WorldTime) -> SkyReport {
    ConstantSun.sky_at(time)
}

/// The local climate, from whichever climate provider this world uses.
pub fn climate_report(_world: &World) -> ClimateReport {
    UniformClimate.climate_at(hornvale_kernel::Position { x: 0.0, y: 0.0 })
}
```

Rewrite `almanac_context` to use them (`sky: sky_report(world, WorldTime { day: 0.0 })`, `climate: climate_report(world)`), removing its direct `ConstantSun`/`UniformClimate` construction. Keep `observed_phenomena` as the phenomena-side construction site.

In `cli/src/repl.rs`: remove the `use hornvale_astronomy::ConstantSun;` and `use hornvale_climate::UniformClimate;` imports; the `"sky"` arm becomes `crate::world_builder::sky_report(world, WorldTime { day })` and `"climate"` becomes `crate::world_builder::climate_report(world)` (same output formatting).

- [ ] **Step 4: Verify no behavior change, gate, commit**

Run the full gate plus the two artifact diffs exactly as in Task 1 Step 5.
Expected: all pass, diffs empty.

```bash
git add cli/
git commit -m "refactor(cli): providers constructed only in the composition root"
```

---

### Task 3: Stream-label constants

**Files:**
- Modify: `domains/settlement/src/lib.rs`, `domains/religion/src/lib.rs` (label constants + `stream_labels()`)
- Modify: `domains/astronomy/src/lib.rs`, `domains/climate/src/lib.rs`, `domains/terrain/src/lib.rs`, `domains/culture/src/lib.rs` (empty `stream_labels()` — no streams yet)
- Modify: `kernel/src/noise.rs` (document the internal octave label; no API change)

**Interfaces:**
- Produces: every domain crate exports `pub fn stream_labels() -> Vec<(&'static str, &'static str)>` returning `(label-or-pattern, doc)` pairs, and uses declared constants for its `derive` calls. Settlement's labels: `"settlement"`, `"name"`, `"name-pick"`, `"population"`. Religion's: `"religion"`, `"epithet"`. Task 4's manifest and Task 5+'s astronomy labels build on this convention.

- [ ] **Step 1: Write the failing tests**

Add to settlement's tests:

```rust
    #[test]
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        for expected in [
            "settlement",
            "settlement/name",
            "settlement/name-pick",
            "settlement/population",
        ] {
            assert!(labels.contains(&expected), "missing {expected}");
        }
    }
```

Add to religion's tests:

```rust
    #[test]
    fn stream_labels_declare_every_derivation() {
        let labels: Vec<&str> = stream_labels().iter().map(|(l, _)| *l).collect();
        assert!(labels.contains(&"religion"));
        assert!(labels.contains(&"religion/epithet"));
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-settlement stream_labels && cargo test -p hornvale-religion stream_labels`
Expected: compile errors.

- [ ] **Step 3: Implement**

In `domains/settlement/src/lib.rs`:

```rust
/// Seed-derivation labels used by this crate. Labels are permanent
/// save-format contracts (spec §3); regeneration uses epoch suffixes.
mod streams {
    /// Root stream label for settlement.
    pub const ROOT: &str = "settlement";
    /// Candidate-name generation stream.
    pub const NAME: &str = "name";
    /// Name-pick offset stream.
    pub const NAME_PICK: &str = "name-pick";
    /// Population draw stream.
    pub const POPULATION: &str = "population";
}

/// Every seed-derivation label (or pattern) this crate uses, with docs.
/// Slash-joined paths document derivation chains; the manifest renders them.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("settlement", "root stream for settlement generation"),
        ("settlement/name", "candidate village names"),
        ("settlement/name-pick", "which candidate survives refinement"),
        ("settlement/population", "village population draw"),
    ]
}
```

and replace the string literals in `genesis` with the constants:
`world.seed.derive(streams::ROOT).derive(streams::NAME)`, `.derive(streams::NAME_PICK)`, `.derive(streams::POPULATION)`.

In `domains/religion/src/lib.rs`, the same pattern:

```rust
/// Seed-derivation labels used by this crate (permanent contracts).
mod streams {
    /// Root stream label for religion.
    pub const ROOT: &str = "religion";
    /// Epithet-pick stream.
    pub const EPITHET: &str = "epithet";
}

/// Every seed-derivation label this crate uses, with docs.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("religion", "root stream for religion generation"),
        ("religion/epithet", "deity epithet pick"),
    ]
}
```

with `world.seed.derive(streams::ROOT).derive(streams::EPITHET)` in `genesis`.

In astronomy, climate, terrain, culture — each gets:

```rust
/// Every seed-derivation label this crate uses (none yet).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}
```

In `kernel/src/noise.rs`, extend the `fbm_2d` doc comment with one line:
`/// Internally derives per-octave streams labeled "octave-{n}" (n ≥ 1).`

- [ ] **Step 4: Gate, artifact diffs, commit**

Full gate + the two artifact diffs (Task 1 Step 5). Byte-identical is the
point: constants must equal the old literals exactly.

```bash
git add kernel/ domains/
git commit -m "refactor(domains): seed-derivation labels as declared constants with stream_labels()"
```

---

### Task 4: The stream manifest — command, book page, CI

**Files:**
- Create: `cli/src/streams.rs` (tests inline)
- Modify: `cli/src/main.rs` (dispatch `streams`)
- Create: `book/src/reference/stream-manifest.md`
- Create: `book/src/reference/stream-manifest-generated.md` (generated + committed)
- Modify: `book/src/SUMMARY.md`, `.github/workflows/ci.yml`

**Interfaces:**
- Produces: `streams::render_streams() -> String` (aggregates every crate's `stream_labels()` plus the kernel's documented internal octave pattern); `hornvale streams` printing it; the book page and CI drift check. Astronomy's Task 5+ labels appear here automatically once its `stream_labels()` grows.

- [ ] **Step 1: Write the failing tests**

`cli/src/streams.rs`:

```rust
//! Render the stream manifest: every seed-derivation label in the project.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manifest_lists_every_crate_and_label() {
        let doc = render_streams();
        for expected in [
            "<!-- GENERATED FILE",
            "| `settlement/name` |",
            "| `religion/epithet` |",
            "octave-{n}",
        ] {
            assert!(doc.contains(expected), "missing: {expected}");
        }
    }

    #[test]
    fn manifest_is_deterministic() {
        assert_eq!(render_streams(), render_streams());
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `mod streams;` to `cli/src/main.rs`, then:
Run: `cargo test -p hornvale streams::`
Expected: compile error — `render_streams` not defined.

- [ ] **Step 3: Implement**

In `cli/src/streams.rs`:

```rust
/// Render every registered crate's stream labels as the book's generated
/// reference page. Labels are permanent save-format contracts.
pub fn render_streams() -> String {
    let sources: [(&str, Vec<(&'static str, &'static str)>); 6] = [
        ("hornvale-astronomy", hornvale_astronomy::stream_labels()),
        ("hornvale-climate", hornvale_climate::stream_labels()),
        ("hornvale-culture", hornvale_culture::stream_labels()),
        ("hornvale-religion", hornvale_religion::stream_labels()),
        ("hornvale-settlement", hornvale_settlement::stream_labels()),
        ("hornvale-terrain", hornvale_terrain::stream_labels()),
    ];
    let mut doc = String::new();
    doc.push_str(
        "<!-- GENERATED FILE — do not edit. Regenerate with `hornvale streams`. -->\n\n",
    );
    doc.push_str(
        "Labels are permanent save-format contracts; regeneration uses epoch \
         suffixes (e.g. `settlement/name/v2`), never renames.\n\n",
    );
    for (crate_name, labels) in sources {
        doc.push_str(&format!("### {crate_name}\n\n"));
        if labels.is_empty() {
            doc.push_str("*(no seed-derivation streams)*\n\n");
            continue;
        }
        doc.push_str("| Label | Meaning |\n|---|---|\n");
        for (label, meaning) in labels {
            doc.push_str(&format!("| `{label}` | {meaning} |\n"));
        }
        doc.push('\n');
    }
    doc.push_str("### hornvale-kernel (internal)\n\n");
    doc.push_str("| Label | Meaning |\n|---|---|\n");
    doc.push_str(
        "| `octave-{n}` | per-octave noise streams derived inside fbm (n ≥ 1) |\n",
    );
    doc
}
```

In `cli/src/main.rs`: dispatch arm `Some("streams") => cmd_streams(),` (after
`"concepts"`), USAGE line `  hornvale streams                         dump the stream manifest as markdown`, and:

```rust
fn cmd_streams() -> Result<(), String> {
    print!("{}", streams::render_streams());
    Ok(())
}
```

- [ ] **Step 4: Generate, frame, wire CI**

```bash
cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
```

`book/src/reference/stream-manifest.md`:

```markdown
# The Stream Manifest

Every random decision in Hornvale flows from the world seed through a chain
of *labeled derivations*. Those labels are permanent save-format contracts:
renaming one silently re-rolls everything downstream of it in every world
(the Bolnar/Gruugish lesson). This manifest is the complete registry of the
labels in use — generated, drift-checked in CI, and reviewed at each
campaign close alongside the concept registry. Deliberate regeneration of a
stream is an **epoch bump** (`settlement/name/v2`), never a rename.

{{#include stream-manifest-generated.md}}
```

`book/src/SUMMARY.md`, Reference section becomes:

```markdown
# Reference

- [The Concept Registry](./reference/concept-registry.md)
- [The Stream Manifest](./reference/stream-manifest.md)
```

`.github/workflows/ci.yml` — add one line to the artifact step's `run:` block,
after the `concepts` line:

```yaml
          cargo run -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
```

(the existing `git diff --exit-code book/src/gallery/ book/src/reference/` already covers it).

- [ ] **Step 5: Gate, book build, no-op check, commit**

Run: full gate + `mdbook build book`. Then rerun the `streams` generation and
confirm `git status` shows the generated file unchanged (committed copy is
current). The seed-42 almanac/concepts diffs from Task 1 Step 5 must still be
empty.

```bash
git add cli/ book/ .github/
git commit -m "feat(cli): stream manifest command, book reference page, CI drift check"
```

---

### Task 5: Astronomy pins and errors

**Files:**
- Modify: `domains/astronomy/src/lib.rs` (module decls + re-exports)
- Create: `domains/astronomy/src/pins.rs` (tests inline)

**Interfaces:**
- Produces (consumed by every later task):

```rust
pub struct SkyPins {
    pub moons: Option<u32>,
    pub rotation: Option<RotationPin>,
    pub obliquity_deg: Option<f64>,
    pub year_local_days: Option<f64>,
    pub neighbor: Option<NeighborClass>,
}   // Default: all None

pub enum RotationPin { Normal, Locked, PeriodHours(f64) }
pub enum NeighborClass { RedDwarf, SunLike, WhiteDwarf, OrangeGiant, RedGiant, BlueGiant }
pub enum GenesisError {
    InvalidPin { pin: String, reason: String },
    UnsatisfiablePin { pin: String, reason: String },
}
```

All derive `Debug, Clone, PartialEq`; `SkyPins` also `Default`; `NeighborClass` also `Copy`; `GenesisError` implements `Display` + `Error`.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/pins.rs`:

```rust
//! Scenario pins: parameters supplied instead of drawn (spec §2.2).
//! Downstream generation conditions on pinned values identically to drawn
//! ones; unsatisfiable pins fail loudly.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_pins_pin_nothing() {
        let pins = SkyPins::default();
        assert!(pins.moons.is_none());
        assert!(pins.rotation.is_none());
        assert!(pins.obliquity_deg.is_none());
        assert!(pins.year_local_days.is_none());
        assert!(pins.neighbor.is_none());
    }

    #[test]
    fn genesis_errors_carry_pin_and_reason() {
        let e = GenesisError::UnsatisfiablePin {
            pin: "year-days".to_string(),
            reason: "orbit outside habitable zone".to_string(),
        };
        let text = e.to_string();
        assert!(text.contains("year-days"));
        assert!(text.contains("habitable zone"));
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `pub mod pins;` and `pub use pins::{GenesisError, NeighborClass, RotationPin, SkyPins};` to `domains/astronomy/src/lib.rs`.
Run: `cargo test -p hornvale-astronomy pins`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests module in `domains/astronomy/src/pins.rs`:

```rust
/// The scenario pins for sky genesis. Every field: `None` = drawn from the
/// seed; `Some` = supplied by the experimenter and conditioned on.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SkyPins {
    /// Number of moons (0–3).
    pub moons: Option<u32>,
    /// Rotation regime of the anchor world.
    pub rotation: Option<RotationPin>,
    /// Axial tilt in degrees (0–35); 0 = no seasons.
    pub obliquity_deg: Option<f64>,
    /// Year length in local days (requires a spinning world).
    pub year_local_days: Option<f64>,
    /// Force one showpiece neighbor of this class.
    pub neighbor: Option<NeighborClass>,
}

/// Pinnable rotation regimes.
#[derive(Debug, Clone, PartialEq)]
pub enum RotationPin {
    /// Drawn-length ordinary spin.
    Normal,
    /// Tidally locked: no local solar day.
    Locked,
    /// A specific day length in standard hours (4–100).
    PeriodHours(f64),
}

/// Spectral classes for notable neighbor stars.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NeighborClass {
    /// Dim red dwarf.
    RedDwarf,
    /// Sun-like yellow star.
    SunLike,
    /// Dense white remnant.
    WhiteDwarf,
    /// Swollen orange giant.
    OrangeGiant,
    /// Vast red giant.
    RedGiant,
    /// Blazing blue giant.
    BlueGiant,
}

/// Why sky genesis refused to produce a system.
#[derive(Debug, Clone, PartialEq)]
pub enum GenesisError {
    /// A pin's value is outside its legal range.
    InvalidPin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The rule it violates.
        reason: String,
    },
    /// A legal pin has no physically consistent solution under the model.
    UnsatisfiablePin {
        /// The pin's CLI-facing name.
        pin: String,
        /// The physical conflict.
        reason: String,
    },
}

impl std::fmt::Display for GenesisError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenesisError::InvalidPin { pin, reason } => {
                write!(f, "invalid pin '{pin}': {reason}")
            }
            GenesisError::UnsatisfiablePin { pin, reason } => {
                write!(f, "unsatisfiable pin '{pin}': {reason}")
            }
        }
    }
}

impl std::error::Error for GenesisError {}
```

- [ ] **Step 4: Gate, commit**

Full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): scenario pins and genesis errors"
```

---

### Task 6: Star generation

**Files:**
- Create: `domains/astronomy/src/star.rs` (tests inline)
- Create: `domains/astronomy/src/streams.rs`
- Modify: `domains/astronomy/src/lib.rs` (modules, re-exports, `stream_labels()`)

**Interfaces:**
- Produces: `Star { mass_solar: f64, luminosity_solar: f64, class_name: String, habitable_zone_au: (f64, f64) }` (derives `Debug, Clone, PartialEq`); `generate_star(seed: Seed) -> Star` where `seed` is the astronomy domain seed (callers pass `world_seed.derive("astronomy")`; this module derives `"star-mass"` from it). Astronomy's `stream_labels()` starts growing.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/star.rs`:

```rust
//! Main-sequence star generation: mass drawn, everything else derived
//! (model card: L = M^3.5; habitable zone 0.95√L–1.37√L AU).

use crate::streams;
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn star_is_deterministic_and_in_range() {
        let a = generate_star(Seed(42));
        assert_eq!(a, generate_star(Seed(42)));
        assert!((0.6..=1.4).contains(&a.mass_solar));
    }

    #[test]
    fn luminosity_and_zone_are_derived() {
        let s = generate_star(Seed(7));
        let expected_l = s.mass_solar.powf(3.5);
        assert!((s.luminosity_solar - expected_l).abs() < 1e-12);
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner - 0.95 * expected_l.sqrt()).abs() < 1e-12);
        assert!((outer - 1.37 * expected_l.sqrt()).abs() < 1e-12);
        assert!(inner < outer);
    }

    #[test]
    fn class_names_track_mass() {
        for seed in 0..32 {
            let s = generate_star(Seed(seed));
            let expected = if s.mass_solar < 0.8 {
                "orange dwarf (K)"
            } else if s.mass_solar < 1.05 {
                "yellow dwarf (G)"
            } else {
                "yellow-white dwarf (F)"
            };
            assert_eq!(s.class_name, expected);
        }
    }
}
```

- [ ] **Step 2: Run to verify failure**

Create `domains/astronomy/src/streams.rs`:

```rust
//! Seed-derivation labels for astronomy (permanent contracts, spec §3).

/// Star mass draw.
pub const STAR_MASS: &str = "star-mass";
/// Anchor mass draw.
pub const ANCHOR_MASS: &str = "anchor-mass";
/// Rotation regime/period draw.
pub const ROTATION: &str = "rotation";
/// Anchor orbital-distance draw.
pub const ORBIT: &str = "orbit";
/// Obliquity draw.
pub const OBLIQUITY: &str = "obliquity";
/// Moon count draw.
pub const MOON_COUNT: &str = "moon-count";
/// Per-moon parameter draws (one stream reused across attempts).
pub const MOONS: &str = "moons";
/// Neighborhood draws.
pub const NEIGHBORS: &str = "neighbors";
```

In `domains/astronomy/src/lib.rs`: add `pub mod star; pub mod streams;`,
`pub use star::{generate_star, Star};`, and replace `stream_labels()` with:

```rust
/// Every seed-derivation label this crate uses, with docs. All chains hang
/// off the world seed's "astronomy" derivation.
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("astronomy", "root stream for sky genesis"),
        ("astronomy/star-mass", "main-sequence star mass draw"),
        ("astronomy/anchor-mass", "anchor world mass draw"),
        ("astronomy/rotation", "rotation regime and period draw"),
        ("astronomy/orbit", "anchor orbital distance draw"),
        ("astronomy/obliquity", "axial tilt draw"),
        ("astronomy/moon-count", "how many moons"),
        ("astronomy/moons", "per-moon mass/distance draws (sequential attempts)"),
        ("astronomy/neighbors", "notable-neighbor class/distance draws"),
    ]
}
```

Run: `cargo test -p hornvale-astronomy star`
Expected: compile error — `generate_star` not defined.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/star.rs`:

```rust
/// A main-sequence star: mass drawn, everything else derived.
#[derive(Debug, Clone, PartialEq)]
pub struct Star {
    /// Mass in solar masses (drawn, 0.6–1.4).
    pub mass_solar: f64,
    /// Luminosity in solar units (derived: M^3.5).
    pub luminosity_solar: f64,
    /// Human-readable spectral character.
    pub class_name: String,
    /// Habitable-zone bounds in AU (derived: 0.95√L, 1.37√L).
    pub habitable_zone_au: (f64, f64),
}

/// Generate the star from the astronomy domain seed.
pub fn generate_star(astronomy_seed: Seed) -> Star {
    let mut stream = astronomy_seed.derive(streams::STAR_MASS).stream();
    let mass_solar = 0.6 + stream.next_f64() * 0.8;
    let luminosity_solar = mass_solar.powf(3.5);
    let sqrt_l = luminosity_solar.sqrt();
    let class_name = if mass_solar < 0.8 {
        "orange dwarf (K)"
    } else if mass_solar < 1.05 {
        "yellow dwarf (G)"
    } else {
        "yellow-white dwarf (F)"
    }
    .to_string();
    Star {
        mass_solar,
        luminosity_solar,
        class_name,
        habitable_zone_au: (0.95 * sqrt_l, 1.37 * sqrt_l),
    }
}
```

- [ ] **Step 4: Gate, regenerate stream manifest, commit**

Full gate. The manifest changed (astronomy labels appeared):

```bash
cargo run -q -p hornvale -- streams > book/src/reference/stream-manifest-generated.md
git add domains/astronomy/ book/src/reference/stream-manifest-generated.md
git commit -m "feat(astronomy): main-sequence star generation with derived zone"
```

---

### Task 7: Anchor generation

**Files:**
- Create: `domains/astronomy/src/anchor.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: `Star` (Task 6), `SkyPins`/`RotationPin`/`GenesisError` (Task 5), `streams` (Task 6).
- Produces: `Rotation::{Spinning { day_std_days: f64 }, Locked}`; `Anchor { mass_earths: f64, orbit_au: f64, year_std_days: f64, rotation: Rotation, obliquity_deg: f64 }` (both derive `Debug, Clone, PartialEq`); `generate_anchor(astronomy_seed: Seed, star: &Star, pins: &SkyPins) -> Result<Anchor, GenesisError>`.
- Physics: year = `365.25 * (a³ / M_star).sqrt()` std days. Pinned year: local days require `Spinning`; `a = (M_star * (year_std/365.25)²)^(1/3)`; out-of-zone → `UnsatisfiablePin`.

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/anchor.rs`:

```rust
//! The anchor world: placed in the habitable zone by construction
//! (spec §2.1). Rotation and obliquity drawn or pinned.

use crate::pins::{GenesisError, RotationPin, SkyPins};
use crate::star::Star;
use crate::streams;
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::star::generate_star;

    fn star() -> Star {
        generate_star(Seed(42))
    }

    #[test]
    fn anchor_is_deterministic_and_in_zone() {
        let s = star();
        let a = generate_anchor(Seed(42), &s, &SkyPins::default()).unwrap();
        assert_eq!(
            a,
            generate_anchor(Seed(42), &s, &SkyPins::default()).unwrap()
        );
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner..=outer).contains(&a.orbit_au));
        assert!((0.5..=2.0).contains(&a.mass_earths));
        assert!((0.0..=35.0).contains(&a.obliquity_deg));
    }

    #[test]
    fn year_satisfies_kepler() {
        let s = star();
        let a = generate_anchor(Seed(7), &s, &SkyPins::default()).unwrap();
        let expected = 365.25 * (a.orbit_au.powi(3) / s.mass_solar).sqrt();
        assert!((a.year_std_days - expected).abs() < 1e-9);
    }

    #[test]
    fn rotation_pins_are_honored() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        assert_eq!(a.rotation, Rotation::Locked);

        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(30.0)),
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        assert_eq!(a.rotation, Rotation::Spinning { day_std_days: 1.25 });
    }

    #[test]
    fn rotation_period_pin_validates_range() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(2.0)),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }

    #[test]
    fn pinned_year_places_orbit_by_kepler_or_fails_loudly() {
        let s = star();
        // Round-trip: pin the year a default anchor actually has (converted
        // to local days at a pinned 24h day) and the derived orbit must land
        // where Kepler puts it — inside the zone, near the unpinned orbit.
        // Seed-robust: works whatever star the seed drew.
        let default_anchor = generate_anchor(Seed(1), &s, &SkyPins::default()).unwrap();
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(default_anchor.year_std_days), // 24h day => local == std
            ..SkyPins::default()
        };
        let a = generate_anchor(Seed(1), &s, &pins).unwrap();
        let (inner, outer) = s.habitable_zone_au;
        assert!((inner..=outer).contains(&a.orbit_au));
        assert!((a.orbit_au - default_anchor.orbit_au).abs() < 1e-9);
        // An absurd year lands far outside the zone for ANY legal star
        // (a >= (0.6·(4000/365.25)²)^(1/3) ≈ 4.2 AU; max outer ≈ 2.5 AU).
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            year_local_days: Some(4000.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn pinned_year_on_locked_world_is_a_conflict() {
        let s = star();
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            year_local_days: Some(300.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn obliquity_pin_is_honored_and_validated() {
        let s = star();
        let pins = SkyPins {
            obliquity_deg: Some(0.0),
            ..SkyPins::default()
        };
        assert_eq!(
            generate_anchor(Seed(1), &s, &pins).unwrap().obliquity_deg,
            0.0
        );
        let pins = SkyPins {
            obliquity_deg: Some(60.0),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_anchor(Seed(1), &s, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `pub mod anchor;` and `pub use anchor::{generate_anchor, Anchor, Rotation};` to `lib.rs`.
Run: `cargo test -p hornvale-astronomy anchor`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/anchor.rs`:

```rust
/// Rotation regime of the anchor world.
#[derive(Debug, Clone, PartialEq)]
pub enum Rotation {
    /// Ordinary spin with a solar day of this many standard days.
    Spinning {
        /// Day length in standard days.
        day_std_days: f64,
    },
    /// Tidally locked: no local solar day exists.
    Locked,
}

/// The habitable anchor world.
#[derive(Debug, Clone, PartialEq)]
pub struct Anchor {
    /// Mass in Earth masses (drawn, 0.5–2.0).
    pub mass_earths: f64,
    /// Orbital distance in AU (in the habitable zone by construction).
    pub orbit_au: f64,
    /// Year length in standard days (derived: Kepler III).
    pub year_std_days: f64,
    /// Rotation regime (drawn or pinned).
    pub rotation: Rotation,
    /// Axial tilt in degrees (drawn 0–35 or pinned).
    pub obliquity_deg: f64,
}

fn year_from_orbit(orbit_au: f64, star_mass: f64) -> f64 {
    365.25 * (orbit_au.powi(3) / star_mass).sqrt()
}

/// Generate the anchor: in-zone by construction, pins conditioned on.
pub fn generate_anchor(
    astronomy_seed: Seed,
    star: &Star,
    pins: &SkyPins,
) -> Result<Anchor, GenesisError> {
    let mass_earths = 0.5 + astronomy_seed.derive(streams::ANCHOR_MASS).stream().next_f64() * 1.5;

    let rotation = match &pins.rotation {
        Some(RotationPin::Locked) => Rotation::Locked,
        Some(RotationPin::PeriodHours(h)) => {
            if !(4.0..=100.0).contains(h) {
                return Err(GenesisError::InvalidPin {
                    pin: "day-hours".to_string(),
                    reason: format!("{h} hours is outside the legal range 4–100"),
                });
            }
            Rotation::Spinning {
                day_std_days: h / 24.0,
            }
        }
        Some(RotationPin::Normal) | None => {
            let mut stream = astronomy_seed.derive(streams::ROTATION).stream();
            let lock_roll = stream.next_f64();
            let locked = pins.rotation.is_none() && lock_roll < 0.05;
            if locked {
                Rotation::Locked
            } else {
                Rotation::Spinning {
                    day_std_days: (16.0 + stream.next_f64() * 24.0) / 24.0,
                }
            }
        }
    };

    let (inner, outer) = star.habitable_zone_au;
    let (orbit_au, year_std_days) = match pins.year_local_days {
        Some(local_days) => {
            let Rotation::Spinning { day_std_days } = rotation else {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: "a tidally locked world has no local days to count a year in"
                        .to_string(),
                });
            };
            if !local_days.is_finite() || local_days <= 0.0 {
                return Err(GenesisError::InvalidPin {
                    pin: "year-days".to_string(),
                    reason: format!("{local_days} local days is not a positive, finite year"),
                });
            }
            let year_std = local_days * day_std_days;
            let orbit = (star.mass_solar * (year_std / 365.25).powi(2)).powf(1.0 / 3.0);
            if !(inner..=outer).contains(&orbit) {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "year-days".to_string(),
                    reason: format!(
                        "a {local_days}-local-day year places the anchor at {orbit:.2} AU, \
                         outside the habitable zone ({inner:.2}–{outer:.2} AU)"
                    ),
                });
            }
            (orbit, year_std)
        }
        None => {
            let orbit =
                inner + astronomy_seed.derive(streams::ORBIT).stream().next_f64() * (outer - inner);
            (orbit, year_from_orbit(orbit, star.mass_solar))
        }
    };

    let obliquity_deg = match pins.obliquity_deg {
        Some(deg) => {
            if !(0.0..=35.0).contains(&deg) {
                return Err(GenesisError::InvalidPin {
                    pin: "obliquity".to_string(),
                    reason: format!("{deg}° is outside the legal range 0–35"),
                });
            }
            deg
        }
        None => astronomy_seed.derive(streams::OBLIQUITY).stream().next_f64() * 35.0,
    };

    Ok(Anchor {
        mass_earths,
        orbit_au,
        year_std_days,
        rotation,
        obliquity_deg,
    })
}
```

- [ ] **Step 4: Gate, commit**

Full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): anchor world in the habitable zone by construction"
```

---

### Task 8: Moons with stability inequalities

**Files:**
- Create: `domains/astronomy/src/moons.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: `Anchor`, `Star`, `SkyPins`, `GenesisError`, `streams`.
- Produces: `Moon { mass_lunar: f64, distance_mm: f64, period_std_days: f64, angular_diameter_rel: f64, tide_rel: f64 }` (derives `Debug, Clone, PartialEq`); `generate_moons(astronomy_seed: Seed, star: &Star, anchor: &Anchor, pins: &SkyPins) -> Result<Vec<Moon>, GenesisError>` (sorted by distance ascending).
- Physics/constraints from Global Constraints, verbatim. Attempt budget: 128 per moon; exhaustion with a pinned count → `UnsatisfiablePin`; with a drawn count → accept the moons admitted so far (the system cannot hold another).

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/moons.rs`:

```rust
//! Moons: drawn, then admitted only past the stability inequalities
//! (Roche floor, Hill cap, mutual spacing, combined-tide cap).

use crate::anchor::Anchor;
use crate::pins::{GenesisError, SkyPins};
use crate::star::Star;
use crate::streams;
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::anchor::generate_anchor;
    use crate::star::generate_star;

    fn system(seed: u64) -> (Star, Anchor) {
        let star = generate_star(Seed(seed));
        let anchor = generate_anchor(Seed(seed), &star, &SkyPins::default()).unwrap();
        (star, anchor)
    }

    #[test]
    fn moon_count_pin_is_honored_for_every_legal_value() {
        let (star, anchor) = system(42);
        for count in 0..=3u32 {
            let pins = SkyPins {
                moons: Some(count),
                ..SkyPins::default()
            };
            let moons = generate_moons(Seed(42), &star, &anchor, &pins).unwrap();
            assert_eq!(moons.len() as u32, count);
        }
    }

    #[test]
    fn moon_count_pin_validates_range() {
        let (star, anchor) = system(42);
        let pins = SkyPins {
            moons: Some(7),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_moons(Seed(42), &star, &anchor, &pins),
            Err(GenesisError::InvalidPin { .. })
        ));
    }

    #[test]
    fn every_generated_moon_satisfies_the_inequalities() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let moons =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            let hill = hill_radius_mm(&star, &anchor);
            let mut previous: Option<f64> = None;
            let mut total_tide = 0.0;
            for moon in &moons {
                assert!(moon.distance_mm >= 20.0, "Roche floor");
                assert!(moon.distance_mm <= 0.4 * hill, "Hill cap");
                if let Some(prev) = previous {
                    assert!(moon.distance_mm / prev >= 1.5, "mutual spacing");
                }
                previous = Some(moon.distance_mm);
                total_tide += moon.tide_rel;
                // Derived quantities match the model card.
                let expected_period = 27.32
                    * ((moon.distance_mm / 384.4).powi(3) / anchor.mass_earths).sqrt();
                assert!((moon.period_std_days - expected_period).abs() < 1e-9);
                let expected_theta = moon.mass_lunar.powf(1.0 / 3.0) * 384.4 / moon.distance_mm;
                assert!((moon.angular_diameter_rel - expected_theta).abs() < 1e-9);
                let expected_tide = moon.mass_lunar / (moon.distance_mm / 384.4).powi(3);
                assert!((moon.tide_rel - expected_tide).abs() < 1e-9);
            }
            assert!(total_tide <= 8.0, "combined tide cap");
        }
    }

    #[test]
    fn moons_are_deterministic() {
        let (star, anchor) = system(9);
        let a = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        let b = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        assert_eq!(a, b);
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `pub mod moons;` and `pub use moons::{generate_moons, hill_radius_mm, Moon};` to `lib.rs`.
Run: `cargo test -p hornvale-astronomy moons`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/moons.rs`:

```rust
/// A moon of the anchor world. Mass and distance drawn; the rest derived.
#[derive(Debug, Clone, PartialEq)]
pub struct Moon {
    /// Mass in lunar masses (drawn, 0.05–2.5).
    pub mass_lunar: f64,
    /// Orbital distance in Mm (drawn within Roche/Hill bounds).
    pub distance_mm: f64,
    /// Orbital period in standard days (derived: Kepler III).
    pub period_std_days: f64,
    /// Apparent size relative to Luna-from-Earth (derived).
    pub angular_diameter_rel: f64,
    /// Tidal strength relative to Luna-on-Earth (derived: m/d³).
    pub tide_rel: f64,
}

/// The anchor's Hill radius in Mm (model card formula).
pub fn hill_radius_mm(star: &Star, anchor: &Anchor) -> f64 {
    anchor.orbit_au
        * (anchor.mass_earths * 3.003e-6 / (3.0 * star.mass_solar)).powf(1.0 / 3.0)
        * 1.496e5
}

const ATTEMPTS_PER_MOON: u32 = 128;
const TIDE_CAP: f64 = 8.0;

fn derive_moon(mass_lunar: f64, distance_mm: f64, anchor: &Anchor) -> Moon {
    Moon {
        mass_lunar,
        distance_mm,
        period_std_days: 27.32 * ((distance_mm / 384.4).powi(3) / anchor.mass_earths).sqrt(),
        angular_diameter_rel: mass_lunar.powf(1.0 / 3.0) * 384.4 / distance_mm,
        tide_rel: mass_lunar / (distance_mm / 384.4).powi(3),
    }
}

/// Generate the moons: count drawn or pinned; each admitted only past the
/// stability inequalities, with a bounded redraw budget.
pub fn generate_moons(
    astronomy_seed: Seed,
    star: &Star,
    anchor: &Anchor,
    pins: &SkyPins,
) -> Result<Vec<Moon>, GenesisError> {
    let count = match pins.moons {
        Some(n) if n <= 3 => n,
        Some(n) => {
            return Err(GenesisError::InvalidPin {
                pin: "moons".to_string(),
                reason: format!("{n} moons requested; the legal range is 0–3"),
            });
        }
        None => {
            let roll = astronomy_seed
                .derive(streams::MOON_COUNT)
                .stream()
                .range_u32(1, 100);
            match roll {
                1..=15 => 0,
                16..=55 => 1,
                56..=85 => 2,
                _ => 3,
            }
        }
    };

    let hill = hill_radius_mm(star, anchor);
    let max_distance = (0.4 * hill).min(900.0);
    let mut stream = astronomy_seed.derive(streams::MOONS).stream();
    let mut moons: Vec<Moon> = Vec::new();

    for index in 0..count {
        let mut admitted = false;
        for _ in 0..ATTEMPTS_PER_MOON {
            let mass = 0.05 + stream.next_f64() * 2.45;
            let distance = 60.0 + stream.next_f64() * (max_distance - 60.0).max(0.0);
            let candidate = derive_moon(mass, distance, anchor);
            let spacing_ok = moons.iter().all(|m| {
                let (near, far) = if m.distance_mm < distance {
                    (m.distance_mm, distance)
                } else {
                    (distance, m.distance_mm)
                };
                far / near >= 1.5
            });
            let tide_total: f64 = moons.iter().map(|m| m.tide_rel).sum::<f64>() + candidate.tide_rel;
            if distance >= 20.0 && distance <= 0.4 * hill && spacing_ok && tide_total <= TIDE_CAP
            {
                moons.push(candidate);
                admitted = true;
                break;
            }
        }
        if !admitted {
            if pins.moons.is_some() {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "moons".to_string(),
                    reason: format!(
                        "moon {} of {count} found no stable orbit within the attempt budget \
                         (Hill radius {hill:.0} Mm, tide cap {TIDE_CAP})",
                        index + 1
                    ),
                });
            }
            // Drawn count: this system genuinely cannot hold another stable
            // moon — accept the ones admitted (spec §4.3: loud failure is
            // for pins; drawn configurations degrade honestly).
            break;
        }
    }

    moons.sort_by(|a, b| a.distance_mm.total_cmp(&b.distance_mm));
    Ok(moons)
}
```

- [ ] **Step 4: Gate, commit**

Full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): moons admitted past the stability inequalities"
```

---

### Task 9: The stellar neighborhood

**Files:**
- Create: `domains/astronomy/src/neighborhood.rs` (tests inline)
- Modify: `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: `SkyPins`, `NeighborClass`, `streams`.
- Produces: `Neighbor { class: NeighborClass, distance_ly: f64, apparent_brightness: f64, color: String }` (derives `Debug, Clone, PartialEq`); `generate_neighbors(astronomy_seed: Seed, pins: &SkyPins) -> Vec<Neighbor>` (sorted by brightness descending; infallible — the neighbor pin has no illegal values). Class luminosities (solar): RedDwarf 0.02, SunLike 1.0, WhiteDwarf 0.005, OrangeGiant 60.0, RedGiant 300.0, BlueGiant 10_000.0. Class weights (of 100): 40/25/10/10/10/5 in that order. Colors: "dim red", "warm yellow", "pale white", "deep orange", "smoldering red", "hard blue-white".

- [ ] **Step 1: Write the failing tests**

`domains/astronomy/src/neighborhood.rs`:

```rust
//! Notable neighbor stars: the objects that dominate a night sky.
//! Observational only (declared approximation): no physical effect on the
//! anchor. A full starfield and constellations are tier 3.

use crate::pins::{NeighborClass, SkyPins};
use crate::streams;
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn neighborhood_is_deterministic_and_sized() {
        let a = generate_neighbors(Seed(42), &SkyPins::default());
        assert_eq!(a, generate_neighbors(Seed(42), &SkyPins::default()));
        assert!((2..=5).contains(&a.len()));
    }

    #[test]
    fn brightness_is_derived_and_sorted_descending() {
        let neighbors = generate_neighbors(Seed(7), &SkyPins::default());
        for pair in neighbors.windows(2) {
            assert!(pair[0].apparent_brightness >= pair[1].apparent_brightness);
        }
        for n in &neighbors {
            let expected = class_luminosity(n.class) / (n.distance_ly * n.distance_ly);
            assert!((n.apparent_brightness - expected).abs() < 1e-12);
            assert!((4.0..=80.0).contains(&n.distance_ly));
        }
    }

    #[test]
    fn neighbor_pin_forces_the_showpiece() {
        let pins = SkyPins {
            neighbor: Some(NeighborClass::BlueGiant),
            ..SkyPins::default()
        };
        let neighbors = generate_neighbors(Seed(3), &pins);
        assert!(neighbors.iter().any(|n| n.class == NeighborClass::BlueGiant));
        // A blue giant at 4–80 ly usually dominates; these seeds' draws make
        // it brightest here (asserted, not assumed).
        assert_eq!(neighbors[0].class, NeighborClass::BlueGiant);
        assert_eq!(neighbors[0].color, "hard blue-white");
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `pub mod neighborhood;` and `pub use neighborhood::{class_luminosity, generate_neighbors, Neighbor};` to `lib.rs`.
Run: `cargo test -p hornvale-astronomy neighborhood`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/neighborhood.rs`:

```rust
/// A notable neighbor star, visible in the night sky.
#[derive(Debug, Clone, PartialEq)]
pub struct Neighbor {
    /// Spectral class (drawn or pinned).
    pub class: NeighborClass,
    /// Distance in light-years (drawn, 4–80).
    pub distance_ly: f64,
    /// Apparent brightness, relative units (derived: L/d²).
    pub apparent_brightness: f64,
    /// Human-readable color character.
    pub color: String,
}

/// Luminosity of a spectral class in solar units (model card).
pub fn class_luminosity(class: NeighborClass) -> f64 {
    match class {
        NeighborClass::RedDwarf => 0.02,
        NeighborClass::SunLike => 1.0,
        NeighborClass::WhiteDwarf => 0.005,
        NeighborClass::OrangeGiant => 60.0,
        NeighborClass::RedGiant => 300.0,
        NeighborClass::BlueGiant => 10_000.0,
    }
}

fn class_color(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "dim red",
        NeighborClass::SunLike => "warm yellow",
        NeighborClass::WhiteDwarf => "pale white",
        NeighborClass::OrangeGiant => "deep orange",
        NeighborClass::RedGiant => "smoldering red",
        NeighborClass::BlueGiant => "hard blue-white",
    }
}

fn draw_class(roll: u32) -> NeighborClass {
    match roll {
        1..=40 => NeighborClass::RedDwarf,
        41..=65 => NeighborClass::SunLike,
        66..=75 => NeighborClass::WhiteDwarf,
        76..=85 => NeighborClass::OrangeGiant,
        86..=95 => NeighborClass::RedGiant,
        _ => NeighborClass::BlueGiant,
    }
}

/// Generate the notable neighbors, brightest first. The neighbor pin forces
/// the first star's class; the rest are drawn.
pub fn generate_neighbors(astronomy_seed: Seed, pins: &SkyPins) -> Vec<Neighbor> {
    let mut stream = astronomy_seed.derive(streams::NEIGHBORS).stream();
    let count = stream.range_u32(2, 5);
    let mut neighbors: Vec<Neighbor> = (0..count)
        .map(|index| {
            let roll = stream.range_u32(1, 100);
            let class = match (index, pins.neighbor) {
                (0, Some(pinned)) => pinned,
                _ => draw_class(roll),
            };
            let distance_ly = 4.0 + stream.next_f64() * 76.0;
            Neighbor {
                class,
                distance_ly,
                apparent_brightness: class_luminosity(class) / (distance_ly * distance_ly),
                color: class_color(class).to_string(),
            }
        })
        .collect();
    neighbors.sort_by(|a, b| b.apparent_brightness.total_cmp(&a.apparent_brightness));
    neighbors
}
```

- [ ] **Step 4: Gate, commit**

Full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): notable neighbor stars, brightest first"
```

---

### Task 10: System assembly and the property battery

**Files:**
- Create: `domains/astronomy/src/system.rs` (tests inline)
- Create: `domains/astronomy/tests/genesis_properties.rs`
- Modify: `domains/astronomy/src/lib.rs` (module + re-exports)

**Interfaces:**
- Consumes: everything above.
- Produces: `StarSystem { star: Star, anchor: Anchor, moons: Vec<Moon>, neighbors: Vec<Neighbor> }` (derives `Debug, Clone, PartialEq`); `generate(world_seed: Seed, pins: &SkyPins) -> Result<StarSystem, GenesisError>` — THE entry point Plan 2b calls. Note: takes the **world seed** and derives `"astronomy"` internally, so callers never touch the domain-seed convention.

- [ ] **Step 1: Write the failing unit tests**

`domains/astronomy/src/system.rs`:

```rust
//! Sky genesis assembly: world seed in, complete star system out.

use crate::anchor::{generate_anchor, Anchor};
use crate::moons::{generate_moons, Moon};
use crate::neighborhood::{generate_neighbors, Neighbor};
use crate::pins::{GenesisError, SkyPins};
use crate::star::{generate_star, Star};
use hornvale_kernel::Seed;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_assembles_a_complete_system() {
        let system = generate(Seed(42), &SkyPins::default()).unwrap();
        assert!(system.star.luminosity_solar > 0.0);
        assert!(system.anchor.year_std_days > 0.0);
        assert!(system.moons.len() <= 3);
        assert!(!system.neighbors.is_empty());
    }

    #[test]
    fn generate_is_deterministic() {
        let a = generate(Seed(42), &SkyPins::default()).unwrap();
        let b = generate(Seed(42), &SkyPins::default()).unwrap();
        assert_eq!(a, b);
    }
}
```

- [ ] **Step 2: Run to verify failure**

Add `pub mod system;` and `pub use system::{generate, StarSystem};` to `lib.rs`.
Run: `cargo test -p hornvale-astronomy system`
Expected: compile error.

- [ ] **Step 3: Implement**

Above the tests in `domains/astronomy/src/system.rs`:

```rust
/// A complete generated star system.
#[derive(Debug, Clone, PartialEq)]
pub struct StarSystem {
    /// The main-sequence host star.
    pub star: Star,
    /// The habitable anchor world.
    pub anchor: Anchor,
    /// Moons, nearest first.
    pub moons: Vec<Moon>,
    /// Notable neighbor stars, brightest first.
    pub neighbors: Vec<Neighbor>,
}

/// Generate the sky for a world: anchor-first, pins conditioned on,
/// loud failure on unsatisfiable pins. Takes the WORLD seed and derives
/// the astronomy domain seed internally.
pub fn generate(world_seed: Seed, pins: &SkyPins) -> Result<StarSystem, GenesisError> {
    let astronomy_seed = world_seed.derive("astronomy");
    let star = generate_star(astronomy_seed);
    let anchor = generate_anchor(astronomy_seed, &star, pins)?;
    let moons = generate_moons(astronomy_seed, &star, &anchor, pins)?;
    let neighbors = generate_neighbors(astronomy_seed, pins);
    Ok(StarSystem {
        star,
        anchor,
        moons,
        neighbors,
    })
}
```

- [ ] **Step 4: Write the property battery**

`domains/astronomy/tests/genesis_properties.rs`:

```rust
//! The property battery (spec §10): every generated system, across many
//! seeds and the pin matrix, satisfies the model card's invariants.

use hornvale_astronomy::{
    generate, hill_radius_mm, GenesisError, NeighborClass, Rotation, RotationPin, SkyPins,
};
use hornvale_kernel::Seed;

#[test]
fn every_default_system_satisfies_every_invariant() {
    for seed in 0..128 {
        let system = generate(Seed(seed), &SkyPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let (inner, outer) = system.star.habitable_zone_au;
        assert!(
            (inner..=outer).contains(&system.anchor.orbit_au),
            "seed {seed}: anchor out of zone"
        );
        let expected_year =
            365.25 * (system.anchor.orbit_au.powi(3) / system.star.mass_solar).sqrt();
        assert!((system.anchor.year_std_days - expected_year).abs() < 1e-9);
        let hill = hill_radius_mm(&system.star, &system.anchor);
        let mut total_tide = 0.0;
        for pair in system.moons.windows(2) {
            assert!(pair[1].distance_mm / pair[0].distance_mm >= 1.5, "seed {seed}: spacing");
        }
        for moon in &system.moons {
            assert!(moon.distance_mm >= 20.0 && moon.distance_mm <= 0.4 * hill);
            total_tide += moon.tide_rel;
        }
        assert!(total_tide <= 8.0, "seed {seed}: tide cap");
        assert!((2..=5).contains(&system.neighbors.len()));
    }
}

#[test]
fn the_pin_matrix_is_honored() {
    for moons in 0..=3u32 {
        let pins = SkyPins { moons: Some(moons), ..SkyPins::default() };
        assert_eq!(generate(Seed(42), &pins).unwrap().moons.len() as u32, moons);
    }
    let pins = SkyPins { rotation: Some(RotationPin::Locked), ..SkyPins::default() };
    assert_eq!(generate(Seed(42), &pins).unwrap().anchor.rotation, Rotation::Locked);
    let pins = SkyPins { obliquity_deg: Some(0.0), ..SkyPins::default() };
    assert_eq!(generate(Seed(42), &pins).unwrap().anchor.obliquity_deg, 0.0);
    let pins = SkyPins { neighbor: Some(NeighborClass::BlueGiant), ..SkyPins::default() };
    assert_eq!(
        generate(Seed(42), &pins).unwrap().neighbors[0].class,
        NeighborClass::BlueGiant
    );
}

#[test]
fn unsatisfiable_pins_fail_loudly_with_the_physical_reason() {
    let pins = SkyPins {
        rotation: Some(RotationPin::PeriodHours(24.0)),
        year_local_days: Some(4000.0),
        ..SkyPins::default()
    };
    match generate(Seed(42), &pins) {
        Err(GenesisError::UnsatisfiablePin { pin, reason }) => {
            assert_eq!(pin, "year-days");
            assert!(reason.contains("habitable zone"));
        }
        other => panic!("expected UnsatisfiablePin, got {other:?}"),
    }
}

#[test]
fn pinned_worlds_differ_from_unpinned_only_downstream_of_the_pin() {
    // Same seed, moons pinned to the drawn count => identical system.
    let default = generate(Seed(42), &SkyPins::default()).unwrap();
    let pinned_same = SkyPins {
        moons: Some(default.moons.len() as u32),
        ..SkyPins::default()
    };
    assert_eq!(generate(Seed(42), &pinned_same).unwrap(), default);
}
```

Note on the last test: pinning the count skips the `moon-count` stream but the
`moons` stream draws identically, so the system must be identical. If this
fails, count-pinning is (wrongly) perturbing sibling streams — exactly the
bug it exists to catch.

- [ ] **Step 5: Run the battery, gate, commit**

Run: `cargo test -p hornvale-astronomy --test genesis_properties`
Expected: 4 tests PASS. If `every_default_system_satisfies_every_invariant`
fails on some seed, the draw ranges vs. the budget need retuning — report it
as a concern with the failing seed rather than loosening an assertion.

Full gate.

```bash
git add domains/astronomy/
git commit -m "feat(astronomy): sky genesis assembly with the property battery"
```

---

## Self-Review Notes

- **Spec coverage (2a slice):** Phase 0 chores §3 (T1–T4), generation order
  and pins §4 (T5–T10), model card formulas §5 (Global Constraints, verified
  in tests), property battery §10 (T10). Calendar §6, provider §7,
  persistence/CLI §8, exit demo §9 are Plan 2b by design.
- **Type consistency:** `SkyPins` field names match across T5/T7/T8/T9/T10;
  `generate` takes the world seed, submodule functions take the astronomy
  domain seed (stated in each Interfaces block); `hill_radius_mm` re-exported
  for the battery.
- **Determinism guards:** per-moon attempts draw from one sequential `moons`
  stream; pinning the count must not perturb sibling streams (tested).
- **Known tuning risk:** the 64/128-seed batteries may surface a seed whose
  drawn configuration exhausts the moon budget; the plan directs reporting
  over loosening. Ranges were chosen so admission is easy (distance floor 60
  Mm is 3× Roche; tide cap 8 admits three heavy close moons), so this is
  unlikely but not impossible. (Fired during implementation: seeds 10/15/42/47/55/58 exhausted 32 attempts on 3-moon configurations — spacing is the binding constraint. Budget raised to 128 pre-merge; inequalities and ranges unchanged. Seed 10 additionally has NO feasible third slot at any budget, so drawn-count exhaustion now degrades honestly while pinned counts still fail loudly per spec §4.3.)
- (Final review: three pin-isolation defects fixed pre-merge — negative-year guard, rotation-Normal lock-roll consumption, neighbor-pin class-roll consumption; spec §2.2 governs over the plan's original samples.)
