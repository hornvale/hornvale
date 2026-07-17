# The Reckoning Implementation Plan

**STATUS: COMPLETE — all 7 tasks executed and reviewed; closed 2026-07-17, pending merge.**
Chronicle: `book/src/chronicle/the-reckoning.md`. Retrospective: `docs/retrospectives/the-reckoning.md`.

**Two amendments execution made to this plan, both Nathan-ratified at execution stops — recorded here because the plan below does not describe what shipped:**

1. **Task 4b (added): the eclipse small-angle correction.** The plan's Task 4 assumed the eclipse model could accept the inclinations `Capture` introduces. It could not — its forms were linear in `i`, valid only for the pre-campaign `[0, 10)` band. Correcting them to the exact spherical `β = asin(sin i · sin u)` **turned this campaign's epoch from PARTIAL to TOTAL** (decision ledger #14; spec §5.3/§7 amended at close). The plan's central property — "a world whose moons all draw `GiantImpact` is byte-identical" — is **superseded**: it holds at the *draw* level and fails at the *world* level.
2. **Task 5b (added): unify `scene/moons/v1` onto the real density.** The header below reads "No scene contract (that is campaign B, The Moons)" — **that is no longer true.** Absorbing main pulled in *The Faces*, which shipped `scene/moons/v1` deriving moon radius at an assumed constant lunar density: the exact assumption this campaign exists to retire. Left alone, the repo would carry two answers for one quantity (ledger #11).

**One plan claim was falsified and should not be inherited:** the gloss "the innermost moon is almost certainly an impact child" was unmeasured, and the `inner_rate > 0.7` threshold derived from it was arithmetically unreachable. The real distribution is an order statistic (ledger #13). See the retrospective.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the star's clock a zero point (a drawn age), give the planet an age, and give every moon an **origin story** — a drawn formation mechanism that determines its age, its density, and whether its orbit is regular or irregular.

**Architecture:** Everything lives in `domains/astronomy` (crate `hornvale-astronomy`), which depends on `hornvale-kernel` and nothing else. Three new seed streams (`star-age`, `moon-formation`, `moon-density`) draw *after* the existing draws, so existing draws stay byte-identical. No kernel change. No scene contract (that is campaign B, The Moons).

**Tech Stack:** Rust 2024, `serde`/`serde_json` only, `hornvale-kernel` for `Seed`/`Stream`/`math`.

**Spec:** `docs/superpowers/specs/2026-07-16-the-reckoning-design.md` — **read §3 (containment), §5 (mechanism), and §7 (blast radius) before Task 1.**

## Global Constraints

- **THE CONTAINMENT RULE (spec §3) — the single most important constraint in this plan.** Age must **never** feed `Star::luminosity`, `HabitableZone`, orbit admission, or anything downstream of insolation. `luminosity` stays `M^3.5` exactly as it is. If any task finds itself changing luminosity or the habitable zone, **STOP and report** — the epoch has leaked out of the moons and into climate, which is not what this campaign is authorized to do.
- **Masses and distances must stay byte-identical** (spec §5.3). Do not touch the admission loop's draws in `generate_moons`. New quantities draw from new streams **after** the distance sort, exactly as SKY-6 and Eclipse Seasons did.
- **`T_MAX = 15 Gyr`** — not 13.8. It is a bound, not a cosmology (spec §4). Do not "correct" it.
- **No new dependencies.** `serde` + `serde_json` only, workspace-wide.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only. Float sorting uses `total_cmp`.
- **No wall-clock time.** Ages are in **gigayears** (`Gyr`), a new unit; `WorldTime` remains standard days and is unrelated.
- **Every crate sets `#![warn(missing_docs)]`** — every public item, field, and variant gets a one-line doc comment.
- **Typed quantities:** coherent physical units crossing a `pub` boundary are hand-rolled newtypes with validating constructors (`Gyr`, `GramsPerCm3`). Every primitive at a `pub` boundary carries a `type-audit:` verdict tag. Run the type audit (Task 7).
- **Golden pins re-pin in the commit that drifts them**, never deferred to the close.
- The gate is `make gate` (fmt + clippy + nextest + doctests). Iterate cost-ordered: `cargo fmt` and `cargo clippy` first, then `cargo test -p hornvale-astronomy`, and `--workspace` only at the pre-commit gate. **Never regenerate censuses locally** (never set `HV_CENSUS=1`).

---

### Task 1: The `Gyr` and `GramsPerCm3` units

**Files:**
- Modify: `domains/astronomy/src/units.rs`
- Test: `domains/astronomy/src/units.rs` (its own `#[cfg(test)] mod tests`, following the file's existing pattern)

**Interfaces:**
- Consumes: nothing.
- Produces:
  - `pub struct Gyr(pub f64)` — gigayears. Validating constructor `Gyr::new(f64) -> Option<Gyr>` rejecting negative and non-finite.
  - `pub struct GramsPerCm3(pub f64)` — bulk density. `GramsPerCm3::new(f64) -> Option<GramsPerCm3>` rejecting non-positive and non-finite.

**Context:** Read `domains/astronomy/src/units.rs` first and **follow its existing newtype pattern exactly** — the file already has `LunarMasses`, `Megameters`, `StdDays`, `SolarMasses`, `SolarLuminosities`, `Au`. Match their constructor style, their doc-comment style, their `type-audit:` tags, and their derives. Do not invent a new convention.

- [x] **Step 1: Write the failing tests**

Add to the tests module in `units.rs`, matching the style of the tests already there:

```rust
#[test]
fn gyr_rejects_negative_and_nonfinite() {
    assert!(Gyr::new(-1.0).is_none());
    assert!(Gyr::new(f64::NAN).is_none());
    assert!(Gyr::new(f64::INFINITY).is_none());
    assert_eq!(Gyr::new(4.5).unwrap().0, 4.5);
    assert_eq!(Gyr::new(0.0).unwrap().0, 0.0); // a zero-age star is degenerate, not invalid
}

#[test]
fn density_rejects_nonpositive_and_nonfinite() {
    assert!(GramsPerCm3::new(0.0).is_none());
    assert!(GramsPerCm3::new(-3.0).is_none());
    assert!(GramsPerCm3::new(f64::NAN).is_none());
    assert_eq!(GramsPerCm3::new(3.34).unwrap().0, 3.34);
}
```

- [x] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy units`
Expected: FAIL — `Gyr` / `GramsPerCm3` do not exist.

- [x] **Step 3: Implement**

Add both newtypes to `units.rs` following the file's established pattern. Doc comments state the unit and what rejects. Zero is *valid* for `Gyr` (a star at age 0 is degenerate but not a contract violation) and *invalid* for `GramsPerCm3` (a massless body is not a body).

- [x] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-astronomy units`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
cargo fmt
git add domains/astronomy/src/units.rs
git commit -m "feat(astronomy): Gyr and GramsPerCm3 units

The Reckoning needs a time unit that is not WorldTime (which is standard
days from genesis) — ages are pre-genesis history in gigayears — and a
density unit, because moon density stops being an assumption once formation
mechanism is known."
```

---

### Task 2: Stellar age and planet age

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (the `STAR_AGE` label)
- Modify: `domains/astronomy/src/star.rs` (`Star::age`, `main_sequence_lifetime`, the draw)
- Test: `domains/astronomy/src/star.rs` tests + `domains/astronomy/tests/genesis_properties.rs`

**Interfaces:**
- Consumes: `Gyr` (Task 1).
- Produces:
  - `pub const STAR_AGE: &str = "star-age";` in `streams`.
  - `pub fn main_sequence_lifetime(star: &Star) -> Gyr` — `10 Gyr · M^-2.5`.
  - `pub const T_MAX: Gyr = Gyr(15.0);`
  - `Star::age: Gyr` — a new public field.
  - `pub fn planet_age(star: &Star) -> Gyr` — `max(0, star.age − 0.05)`.

**Context — the containment rule applies hardest here.** `generate_star` currently derives `luminosity = M^3.5` and `habitable_zone` from it. **You are adding a field, not changing a derivation.** `luminosity` and `habitable_zone` must come out byte-identical for every seed. Step 5 is the guard.

`brightening_per_gyr` (already in this file) is `0.10 · M^2.5`, and its doc comment already notes the `t_MS ∝ M^-2.5` scaling — `main_sequence_lifetime` makes that scaling explicit rather than implicit.

`GYR_DAYS` already exists in this module (exported from `lib.rs`).

- [x] **Step 1: Write the failing tests**

```rust
#[test]
fn main_sequence_lifetime_scales_as_mass_to_the_minus_five_halves() {
    // Sol-calibrated: 1.0 Msun -> 10 Gyr.
    let sol = generate_star(Seed(1));
    let t = main_sequence_lifetime(&Star { mass: SolarMasses(1.0), ..sol.clone() });
    assert!((t.0 - 10.0).abs() < 1e-9, "{}", t.0);
    // A heavier star burns out faster; a lighter one outlives the bound.
    let heavy = main_sequence_lifetime(&Star { mass: SolarMasses(1.4), ..sol.clone() });
    let light = main_sequence_lifetime(&Star { mass: SolarMasses(0.6), ..sol });
    assert!(heavy.0 < 5.0, "1.4 Msun t_MS = {}", heavy.0);
    assert!(light.0 > 30.0, "0.6 Msun t_MS = {}", light.0);
}

#[test]
fn age_stays_inside_the_guard_rails_and_the_bound() {
    for seed in 0..200u64 {
        let star = generate_star(Seed(seed));
        let t_ms = main_sequence_lifetime(&star);
        let ceiling = t_ms.0.min(T_MAX.0);
        assert!(star.age.0 >= 0.05 * ceiling, "seed {seed}: age {} below rail", star.age.0);
        assert!(star.age.0 <= 0.95 * ceiling, "seed {seed}: age {} above rail", star.age.0);
        assert!(star.age.0 <= T_MAX.0, "seed {seed}: age {} exceeds T_MAX", star.age.0);
    }
}

#[test]
fn a_k_dwarf_is_bounded_young_in_main_sequence_terms() {
    // The emergent result the 15 Gyr bound buys (spec §4): a 0.6 Msun star's
    // t_MS is ~35.9 Gyr, so T_MAX caps it at ~42% of its life — it has
    // necessarily brightened little. This is true of real K dwarfs.
    let sol = generate_star(Seed(1));
    let k = Star { mass: SolarMasses(0.6), ..sol };
    let t_ms = main_sequence_lifetime(&k);
    let max_fraction = T_MAX.0 / t_ms.0;
    assert!(max_fraction < 0.45, "K dwarf can reach {max_fraction} of its life");
}

#[test]
fn planet_age_trails_the_star_by_the_accretion_interval() {
    let star = generate_star(Seed(42));
    let p = planet_age(&star);
    assert!((star.age.0 - p.0 - 0.05).abs() < 1e-9);
    assert!(p.0 >= 0.0);
}

#[test]
fn age_is_deterministic() {
    assert_eq!(generate_star(Seed(42)).age, generate_star(Seed(42)).age);
}
```

- [x] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy star`
Expected: FAIL — `age`, `main_sequence_lifetime`, `T_MAX`, `planet_age` do not exist.

- [x] **Step 3: Implement**

Add to `streams.rs`, matching the file's existing style (every label carries a doc comment and a `type-audit:` tag):

```rust
/// Stellar age draw (The Reckoning).
/// type-audit: bare-ok(identifier-text)
pub const STAR_AGE: &str = "star-age";
```

In `star.rs`:

```rust
/// The main-sequence bound on a drawn age. **Not 13.8 Gyr**: this is a bound,
/// not a cosmology. Capping at the age of *our* universe would settle a
/// metaphysical question (UNI-2) that the project deliberately leaves open, as
/// a side effect of a generator constant. 15 Gyr bounds the draw without
/// dating the universe.
/// type-audit: bare-ok(constant)
pub const T_MAX: Gyr = Gyr(15.0);

/// Main-sequence lifetime: t_MS = 10 Gyr · M^-2.5 (declared approximation —
/// the Sol-calibrated scaling already implicit in `brightening_per_gyr`, made
/// explicit; not a stellar-structure model).
pub fn main_sequence_lifetime(star: &Star) -> Gyr {
    Gyr(10.0 * math::powf(star.mass.0, -2.5))
}

/// The planet's age: terrestrial accretion completes within ~30–100 Myr of the
/// star, so the planet trails it by a token 0.05 Gyr. Derived, not drawn — the
/// difference is under 1% and below anything the sim observes; it is modelled
/// only so the number exists and is honest about its own precision.
pub fn planet_age(star: &Star) -> Gyr {
    Gyr((star.age.0 - 0.05).max(0.0))
}
```

and in `generate_star`, **after** the existing luminosity/zone derivations (so those are visibly untouched):

```rust
    let ceiling = (10.0 * math::powf(mass.0, -2.5)).min(T_MAX.0);
    let age = Gyr((0.05 + astronomy_seed.derive(streams::STAR_AGE).stream().next_f64() * 0.90) * ceiling);
```

Add `age` to the `Star` struct with a doc comment naming it as drawn, and stating that **it does not feed luminosity** (spec §3).

- [x] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-astronomy star`
Expected: PASS.

- [x] **Step 5: Write the CONTAINMENT test — the plan's most important assertion**

Add to `domains/astronomy/tests/genesis_properties.rs`:

```rust
/// The Reckoning's containment rule (spec §3): drawing an age must not move
/// luminosity, the habitable zone, or the anchor's admitted orbit. If this
/// ever fails, the epoch has leaked out of the moons and into climate.
#[test]
fn stellar_age_does_not_touch_luminosity_or_the_habitable_zone() {
    for seed in 0..500u64 {
        let star = generate_star(Seed(seed));
        // Luminosity is M^3.5 and NOTHING else — recompute it independently.
        let expected_l = hornvale_kernel::math::powf(star.mass.0, 3.5);
        assert!(
            (star.luminosity.0 - expected_l).abs() < 1e-12,
            "seed {seed}: luminosity {} != M^3.5 {}",
            star.luminosity.0, expected_l
        );
        let sqrt_l = expected_l.sqrt();
        assert!((star.habitable_zone.inner().0 - 0.95 * sqrt_l).abs() < 1e-12, "seed {seed}");
        assert!((star.habitable_zone.outer().0 - 1.37 * sqrt_l).abs() < 1e-12, "seed {seed}");
    }
}
```

Use the real accessor names for `HabitableZone` — read the struct first; if the bounds are public fields rather than methods, adjust.

- [x] **Step 6: Run the containment test and the full crate**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS. **If any pre-existing star/anchor/climate test's expected values changed, STOP and report** — that is the containment leaking, not a re-baseline.

- [x] **Step 7: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/
git commit -m "feat(astronomy): the star gets an age

The Long Count shipped brightening (0.10*M^2.5 per Gyr, scaled by
t_MS ∝ M^-2.5) anchored at genesis, so the model knew how fast the star
brightens but never how far along it is. It reasoned about t_MS without
placing the star on it. Now it does.

T_MAX = 15 Gyr is a bound, not a cosmology: 13.8 would settle UNI-2 as a
side effect of a generator constant. The bound still buys the physics — a
0.6 Msun K dwarf is capped at ~42% of its life, so it has necessarily
brightened little, which is true of real K dwarfs.

Containment (spec §3): age does NOT feed luminosity or the habitable zone,
and a 500-seed test asserts luminosity is still exactly M^3.5. Letting age
correct luminosity would move every world's climate."
```

---

### Task 3: The `Formation` mechanism, drawn after admission

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (`MOON_FORMATION`)
- Modify: `domains/astronomy/src/moons.rs` (`Formation`, the draw, `Moon::formation`)
- Test: `domains/astronomy/src/moons.rs` tests

**Interfaces:**
- Consumes: `Star::age` / `planet_age` (Task 2).
- Produces:
  - `pub enum Formation { GiantImpact, Capture }` — with doc comments naming the real exemplar of each.
  - `pub const MOON_FORMATION: &str = "moon-formation";`
  - `Moon::formation: Formation` — a new public field.

**Context — read spec §5.3 before writing a line.** The draw goes **after** `moons.sort_by(distance)` and **before** the inclination draw, from its own stream, exactly as SKY-6's inclination and Eclipse Seasons' node do. It must **not** touch the admission loop: count, mass, and distance stay byte-identical.

Weight **by distance**, which is physical and free: an impact child forms close and tidally recedes; irregular satellites are distant. Use the moon's distance as a fraction of the admitted range:

```
p_capture = clamp(0.10, 0.85, (distance − 60.0) / (max_distance − 60.0))
```

so the innermost moon is almost certainly an impact child and the outermost is probably a stray. `max_distance` is already computed in `generate_moons`.

**Do not model co-accretion** (spec §5.1): it needs a massive circumplanetary disk, which is a giant-planet mechanism. Hornvale's anchor is terrestrial.

- [x] **Step 1: Write the failing tests**

```rust
#[test]
fn masses_and_distances_are_untouched_by_the_formation_draw() {
    // Spec §5.3: the whole point of drawing after admission. These values are
    // the pre-campaign ones; if this fails, the admission loop was disturbed.
    let (star, anchor) = system(42);
    let (moons, _) = generate_moons(Seed(42), &star, &anchor, &SkyPins::default()).unwrap();
    // Record the pre-campaign masses/distances for seed 42 by running this
    // test BEFORE implementing the draw, and paste the printed values here.
    // (Step 2 prints them.)
    let expected: &[(f64, f64)] = &[/* filled in at Step 2 */];
    assert_eq!(moons.len(), expected.len());
    for (m, (mass, dist)) in moons.iter().zip(expected) {
        assert_eq!(m.mass.0, *mass);
        assert_eq!(m.distance.0, *dist);
    }
}

#[test]
fn the_innermost_moon_is_an_impact_child_and_the_outermost_tends_to_stray() {
    // Distribution claim over seeds, not a per-seed assertion.
    let (mut inner_impact, mut inner_total) = (0u32, 0u32);
    let (mut outer_capture, mut outer_total) = (0u32, 0u32);
    for seed in 0..400u64 {
        let (star, anchor) = system(seed);
        let (moons, _) = generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
        if moons.len() < 2 { continue; }
        inner_total += 1;
        if moons[0].formation == Formation::GiantImpact { inner_impact += 1; }
        outer_total += 1;
        if moons[moons.len() - 1].formation == Formation::Capture { outer_capture += 1; }
    }
    assert!(inner_total > 20 && outer_total > 20, "too few multi-moon seeds to judge");
    let inner_rate = f64::from(inner_impact) / f64::from(inner_total);
    let outer_rate = f64::from(outer_capture) / f64::from(outer_total);
    assert!(inner_rate > 0.7, "innermost impact rate {inner_rate}");
    assert!(outer_rate > 0.3, "outermost capture rate {outer_rate}");
}

#[test]
fn formation_is_deterministic() {
    let (star, anchor) = system(7);
    let a = generate_moons(Seed(7), &star, &anchor, &SkyPins::default()).unwrap().0;
    let b = generate_moons(Seed(7), &star, &anchor, &SkyPins::default()).unwrap().0;
    let fa: Vec<_> = a.iter().map(|m| m.formation).collect();
    let fb: Vec<_> = b.iter().map(|m| m.formation).collect();
    assert_eq!(fa, fb);
}
```

- [x] **Step 2: Capture the pre-campaign masses/distances, then run to verify failure**

Before implementing, print seed 42's current moons so the byte-identity test has real expected values:

```bash
cargo test -p hornvale-astronomy moons -- --nocapture
```

Add a temporary `println!` of `(mass.0, distance.0)` per moon, paste the values into Step 1's `expected`, and remove the `println!`. **These values are the contract**: they must be identical after your change.

Run: `cargo test -p hornvale-astronomy moons`
Expected: FAIL — `Formation` does not exist.

- [x] **Step 3: Implement**

In `streams.rs`:

```rust
/// Per-moon formation-mechanism draw (The Reckoning). Drawn after admission
/// and the distance sort, so count/mass/distance stay byte-identical.
/// type-audit: bare-ok(identifier-text)
pub const MOON_FORMATION: &str = "moon-formation";
```

In `moons.rs`:

```rust
/// How a moon came to be. The mechanism predicts the body's age, its density,
/// and whether its orbit is regular or irregular.
///
/// Co-accretion (the Galilean moons, Titan) is deliberately absent: it needs a
/// massive circumplanetary disk, which is a giant-planet mechanism, and the
/// anchor is terrestrial. Fission (Darwin's proposal for Luna) is discredited.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Formation {
    /// A body struck the proto-planet and the debris re-accreted — Luna.
    /// Coeval with the planet, iron-poor (mantle debris, no core), and
    /// regular: prograde, low inclination.
    GiantImpact,
    /// A passing body was captured — Triton. Its age is decoupled (it formed
    /// elsewhere), its composition is from another reservoir, and its orbit is
    /// irregular: high inclination, often retrograde.
    Capture,
}
```

and in `generate_moons`, **after** the `moons.sort_by(...)` line and **before** the inclination draw:

```rust
    // The Reckoning: formation draws from its own stream, after the distance
    // sort, so every admission draw (count, masses, distances) stays
    // byte-identical — the same discipline SKY-6 and Eclipse Seasons used.
    // Weighted by distance: an impact child forms close and tidally recedes;
    // irregular satellites are distant.
    let mut formations = astronomy_seed.derive(streams::MOON_FORMATION).stream();
    for moon in &mut moons {
        let span = (max_distance - 60.0).max(1e-9);
        let p_capture = ((moon.distance.0 - 60.0) / span).clamp(0.10, 0.85);
        moon.formation = if formations.next_f64() < p_capture {
            Formation::Capture
        } else {
            Formation::GiantImpact
        };
    }
```

Add `formation` to the `Moon` struct. Give it a placeholder in `derive_moon` (`Formation::GiantImpact`) with a comment mirroring the existing inclination/node placeholders — the real value is assigned after admission.

- [x] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-astronomy moons`
Expected: PASS — **including the byte-identity test**. If masses or distances moved, you disturbed the admission loop: revert and re-read spec §5.3.

- [x] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/
git commit -m "feat(astronomy): moons get a formation mechanism

A moon's mass, distance, and inclination floated free of one another because
nothing said where the body came from — and in reality that one fact predicts
nearly all of them.

Drawn after admission from its own stream, weighted by distance (an impact
child forms close and tidally recedes; irregulars are distant), so every
admission draw stays byte-identical — the SKY-6 / Eclipse Seasons discipline.

Co-accretion is not modelled: it needs a circumplanetary disk, which is a
giant-planet mechanism, and the anchor is terrestrial."
```

---

### Task 4: Mechanism drives inclination — the epoch

**Files:**
- Modify: `domains/astronomy/src/moons.rs` (the inclination draw)
- Test: `domains/astronomy/src/moons.rs` tests; the eclipse batteries re-pin here

**Interfaces:**
- Consumes: `Formation` (Task 3).
- Produces: no new API — `inclination_deg`'s *distribution* now depends on `formation`.

**Context — this is the epoch, and it is a PARTIAL one.** Spec §5.3/§7: the draw must consume **exactly one** `next_f64()` per moon from the **existing** `MOON_INCLINATIONS` stream, whichever branch is taken, so index-stability holds:

```
GiantImpact -> incl = s.next_f64() * 10.0            <- IDENTICAL to today
Capture     -> incl = 20.0 + s.next_f64() * 140.0    <- differs; >90 is retrograde
```

Consequence, and the property to protect: **a world whose moons all draw `GiantImpact` is byte-identical to today.** Only captured moons move.

**This task re-pins the eclipse goldens** — in this commit, not deferred to the close (CLAUDE.md golden-pin discipline). Find them: `grep -rn "eclipse" domains/astronomy/tests/ cli/tests/`.

- [x] **Step 1: Write the failing tests**

```rust
#[test]
fn impact_moons_are_regular_and_captured_moons_are_irregular() {
    for seed in 0..400u64 {
        let (star, anchor) = system(seed);
        let (moons, _) = generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
        for m in &moons {
            match m.formation {
                Formation::GiantImpact => assert!(
                    (0.0..=10.0).contains(&m.inclination_deg),
                    "seed {seed}: impact moon at {}°", m.inclination_deg
                ),
                Formation::Capture => assert!(
                    (20.0..=160.0).contains(&m.inclination_deg),
                    "seed {seed}: captured moon at {}°", m.inclination_deg
                ),
            }
        }
    }
}

#[test]
fn some_captured_moons_are_retrograde() {
    // >90° is retrograde. This is the campaign's visible deliverable: worlds
    // that have never had a retrograde moon can now have one.
    let mut retrograde = 0u32;
    let mut captured = 0u32;
    for seed in 0..400u64 {
        let (star, anchor) = system(seed);
        let (moons, _) = generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
        for m in moons.iter().filter(|m| m.formation == Formation::Capture) {
            captured += 1;
            if m.inclination_deg > 90.0 { retrograde += 1; }
        }
    }
    assert!(captured > 20, "too few captured moons to judge ({captured})");
    let rate = f64::from(retrograde) / f64::from(captured);
    assert!((0.3..0.7).contains(&rate), "retrograde rate {rate} among {captured} captured");
}

#[test]
fn an_all_impact_world_is_byte_identical_to_the_pre_campaign_draw() {
    // The partial-epoch property (spec §7). For a seed whose moons all draw
    // GiantImpact, inclination uses the identical formula off the identical
    // single stream draw — so nothing moved. Find such a seed, and assert its
    // inclinations against the pre-campaign values recorded at Step 2.
}
```

- [x] **Step 2: Record the pre-campaign inclinations, then run to verify failure**

Before implementing, print inclinations for a handful of seeds and identify one whose moons will all draw `GiantImpact` (Task 3 already landed, so `formation` is available). Paste that seed's pre-campaign inclinations into Step 1's third test.

Run: `cargo test -p hornvale-astronomy moons`
Expected: FAIL — captured moons are still at 0–10°.

- [x] **Step 3: Implement**

Replace the inclination loop:

```rust
    // The Reckoning: inclination is now conditioned on formation, and this is
    // the epoch. Both branches consume EXACTLY ONE draw from the same stream,
    // so index-stability holds (SKY-6's guarantee) and an all-GiantImpact
    // world stays byte-identical: its formula is unchanged.
    let mut inclinations = astronomy_seed.derive(streams::MOON_INCLINATIONS).stream();
    for moon in &mut moons {
        let roll = inclinations.next_f64();
        moon.inclination_deg = match moon.formation {
            // Regular: prograde, low inclination. The pre-campaign formula.
            Formation::GiantImpact => roll * 10.0,
            // Irregular. Above 90° the orbit is retrograde — Triton's case.
            Formation::Capture => 20.0 + roll * 140.0,
        };
    }
```

- [x] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-astronomy`
Expected: the new tests PASS. **Eclipse tests will now fail on captured-moon seeds — that is the epoch, and it is expected.**

- [x] **Step 5: Re-pin the eclipse goldens IN THIS COMMIT**

Run the eclipse batteries and re-pin the drifted values. For each drifted pin, confirm the drift is explained by a captured moon (check that seed's `formation`) — **a drifted pin on an all-impact seed is a bug, not a re-pin**; stop and report that.

Run: `cargo test -p hornvale-astronomy --test genesis_properties` and any eclipse test files.
Expected: PASS after re-pinning.

- [x] **Step 6: Run the full gate**

Run: `make gate`
Expected: PASS. Census fixtures may drift — **do NOT regenerate them** (`HV_CENSUS=1` is forbidden here); note the drift for Task 6.

- [x] **Step 7: Commit**

```bash
cargo fmt
git add domains/astronomy/
git commit -m "feat(astronomy): formation drives inclination — the epoch

A captured body's defining signature is an irregular orbit: high
inclination, often retrograde. This is where worlds stop being uniform.

PARTIAL epoch by construction: both branches consume exactly one draw from
the existing inclination stream, so index-stability holds and an
all-GiantImpact world is byte-identical to today. Masses and distances never
move. Only worlds that actually receive a captured moon change.

Eclipse goldens re-pinned in this commit, per golden-pin discipline. Every
drift was verified to fall on a captured-moon seed."
```

---

### Task 5: Mechanism drives age and density

**Files:**
- Modify: `domains/astronomy/src/streams.rs` (`MOON_DENSITY`)
- Modify: `domains/astronomy/src/moons.rs` (`Moon::age`, `Moon::density`, `radius_km`)
- Test: `domains/astronomy/src/moons.rs` tests

**Interfaces:**
- Consumes: `Formation` (Task 3), `planet_age` (Task 2), `Gyr`/`GramsPerCm3` (Task 1).
- Produces:
  - `pub const MOON_DENSITY: &str = "moon-density";`
  - `Moon::age: Gyr`, `Moon::density: GramsPerCm3`.
  - `pub fn radius_km(moon: &Moon) -> f64` — `(3M / 4πρ)^{1/3}`.

**Context:** This is the payload campaign B was blocked on — **density stops being an assumption**. Spec §5.2:

- `GiantImpact` → density **3.34 g cm⁻³**, derived not drawn: re-accreted mantle debris with no iron core. This is exactly why Luna is 3.34 against Earth's 5.51.
- `Capture` → drawn from `{rocky 3.0, icy 1.6}` — a different reservoir entirely.
- `GiantImpact` age → `planet_age − U(0.03, 0.10) Gyr` (coeval; Luna is 4.51 against Earth's 4.54).
- `Capture` age → `U(0.05, 0.95) · planet_age` — decoupled; the body formed elsewhere.

Both draws come from streams **after** formation, so they disturb nothing. Reuse `MOON_FORMATION`'s stream for the age jitter or add a stream — your call, but **say which in the report**, and keep one draw per moon per stream so index-stability is obvious.

Lunar mass is `7.342e22 kg`; use it to turn `LunarMasses` into a radius.

- [x] **Step 1: Write the failing tests**

```rust
#[test]
fn impact_moons_are_iron_poor_and_coeval() {
    for seed in 0..300u64 {
        let (star, anchor) = system(seed);
        let star_age = generate_star(Seed(seed));
        let p_age = planet_age(&star_age);
        let (moons, _) = generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
        for m in moons.iter().filter(|m| m.formation == Formation::GiantImpact) {
            assert_eq!(m.density.0, 3.34, "seed {seed}");
            let gap = p_age.0 - m.age.0;
            assert!((0.03..=0.10).contains(&gap), "seed {seed}: gap {gap} Gyr");
        }
    }
}

#[test]
fn captured_moons_have_alien_density_and_decoupled_age() {
    let mut icy = 0u32;
    let mut captured = 0u32;
    for seed in 0..300u64 {
        let (star, anchor) = system(seed);
        let p_age = planet_age(&generate_star(Seed(seed)));
        let (moons, _) = generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
        for m in moons.iter().filter(|m| m.formation == Formation::Capture) {
            captured += 1;
            assert!(m.density.0 == 3.0 || m.density.0 == 1.6, "seed {seed}: {}", m.density.0);
            if m.density.0 == 1.6 { icy += 1; }
            assert!(m.age.0 <= p_age.0, "seed {seed}: captured moon older than its planet");
            assert!(m.age.0 >= 0.0);
        }
    }
    assert!(captured > 20, "too few captured moons ({captured})");
    assert!(icy > 0, "no icy captured body in 300 seeds");
}

#[test]
fn radius_follows_from_mass_and_real_density_not_an_assumption() {
    // Luna: 1.0 lunar mass at 3.34 g/cm3 -> ~1737 km.
    let luna = Moon {
        mass: LunarMasses(1.0),
        density: GramsPerCm3(3.34),
        ../* the rest from a generated moon */
    };
    let r = radius_km(&luna);
    assert!((r - 1737.0).abs() < 30.0, "Luna radius came out {r} km");
    // An icy body of the same mass is larger.
    let icy = Moon { density: GramsPerCm3(1.6), ..luna };
    assert!(radius_km(&icy) > r * 1.2);
}
```

- [x] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-astronomy moons`
Expected: FAIL — `age`, `density`, `radius_km` do not exist.

- [x] **Step 3: Implement**

Add the fields (with doc comments naming derived vs drawn), the density/age assignment after the formation loop, and:

```rust
/// Bulk radius in km from mass and density: r = (3M / 4πρ)^{1/3}.
/// Derived — and derived from a REAL density, not an assumed one, which is
/// the whole point of knowing the formation mechanism.
pub fn radius_km(moon: &Moon) -> f64 {
    const LUNAR_MASS_KG: f64 = 7.342e22;
    let m_kg = moon.mass.0 * LUNAR_MASS_KG;
    let rho_kg_m3 = moon.density.0 * 1000.0;
    let v = 3.0 * m_kg / (4.0 * std::f64::consts::PI * rho_kg_m3);
    math::powf(v, 1.0 / 3.0) / 1000.0
}
```

- [x] **Step 4: Run to verify pass**

Run: `cargo test -p hornvale-astronomy`
Expected: PASS.

- [x] **Step 5: Commit**

```bash
cargo fmt && cargo clippy -p hornvale-astronomy --all-targets -- -D warnings
git add domains/astronomy/
git commit -m "feat(astronomy): mechanism drives moon age and density

Density stops being an assumption. A giant-impact moon is 3.34 g/cm3 because
it is re-accreted mantle debris with no iron core — which is exactly why Luna
is 3.34 against Earth's 5.51. A captured body comes from another reservoir
and may be icy. Radius then follows from mass and a REAL density.

This is the honesty upgrade The Moons (campaign B) was blocked on."
```

---

### Task 6: Measure the blast radius

**Files:**
- Create: a temporary measurement script or `#[ignore]`d test — **not committed as a permanent test**
- Modify: `.superpowers/sdd/progress.md` (record the number)

**Context:** Spec §7 says the affected fraction of seeds "is measurable before merge and **must be measured**". Nobody has this number, and it is the honest input to the census-regen conversation — which is a carve-out needing Nathan's explicit authorization.

- [x] **Step 1: Measure**

Across the census seed set (1000 seeds — read `studies/the-census.study.json` for the exact set), compute and report:

- the share of worlds with **at least one** captured moon (these are the worlds that moved);
- the share that are **byte-identical** (moonless, or all-`GiantImpact`);
- the share of all moons that are `Capture`;
- among captured moons, the retrograde share.

Run it in the foreground; it is generation-only (no census, no `HV_CENSUS`).

- [x] **Step 2: Sanity-check the number against the design**

The distance weighting is `clamp(0.10, 0.85, …)`, so a lone close moon is ~10% likely to be captured and a distant one up to 85%. If the measured "worlds that moved" share is near 0% or near 100%, the weighting is wrong — **report it rather than accepting it**. A plausible outcome is a large minority.

- [x] **Step 3: Record and report — do NOT regenerate the census**

Write the numbers into `.superpowers/sdd/progress.md` and your task report. **The regen itself is Nathan's carve-out; you are producing the input to that decision, not making it.**

---

### Task 7: Facts, model card, and the type audit

**Files:**
- Modify: `domains/astronomy/src/facts.rs` (new predicates)
- Modify: `book/src/domains/astronomy.md` (the model card)
- Test: `domains/astronomy/src/facts.rs` tests

**Context:** The domain publishes facts through the trace protocol; new quantities that a consumer could care about get predicates. Follow the existing pattern exactly (see `BRIGHTENING_PER_GYR` at `facts.rs:153` — a const, a registration, and a test that finds it).

Candidate predicates: `star-age-gyr`, `moon-formation`, `moon-age-gyr`, `moon-density`. Register them in the concept registry per the file's convention; naming follows the book's concept-registry chapter.

**The model card** (`book/src/domains/astronomy.md`, "## The model card") must gain this campaign's delta — spec §6. It is the book stating plainly what is physics, what is approximation, and what is dice, and it is **not optional**: the campaign's biggest claims are admissions.

- [x] **Step 1: Add the facts, with tests, following the existing pattern**
- [x] **Step 2: Update the model card** — derived: planet age, coeval moon age, radius-from-real-density. Drawn: star age, formation, capture age/density. Approximated (declared): `t_MS` scaling; **`L = M^3.5` stays genesis-epoch and age does NOT correct it** (spec §3); the distance weighting is a plausibility rule; two density classes, not a composition model; **capture is modelled as an outcome, not an event** — no encounter dynamics.
- [x] **Step 3: Run the type audit**

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
```
Expected: PASS (default-deny — every new pub-boundary primitive needs a `type-audit:` tag). Then regenerate the report:
```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```

- [x] **Step 4: Full gate + commit**

Run: `make gate` and `mdbook build book`.

```bash
cargo fmt
git add domains/astronomy/ book/ docs/audits/
git commit -m "feat(astronomy): publish the ages and origins as facts; model card

The campaign's biggest claims are admissions, so the book states them: age
does not correct luminosity (deliberate containment), the distance weighting
is a plausibility rule, and capture is modelled as an outcome, not an event —
a real capture needs energy dissipation this model does not simulate."
```

---

## Close (not a task — the campaign checklist)

See `closing-a-campaign`. Specific to this campaign:

- **The census regen is a CARVE-OUT.** Task 6's number is the input; the authorization is Nathan's, and **rift-and-fit is queued for the same budget**. This is the one thing that can block the close.
- **Keystone refreeze** from main's tip at merge — identity fixtures must measure this campaign's delta, not another's.
- Chronicle (`book/src/chronicle/the-reckoning.md` + SUMMARY), retrospective, book freshness sweep, Confidence Gradient re-score if a bet moved.
- **Unpark The Moons** (campaign B): its spec's §0 lists exactly what this campaign retires from it — chiefly the assumed density, and the better crater model (highlands saturate; maria reset the clock).
- Registry: `SKY-minimoon` is the honest sibling of this campaign's permanent capture — cross-link rather than restate.
