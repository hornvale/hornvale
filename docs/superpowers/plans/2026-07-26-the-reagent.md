# The Reagent Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
> Hornvale-specific: also read `.claude/skills/dispatching-hornvale-subagents` before dispatching any implementer.

**Goal:** Ship the material ground truth for alchemy — latent quality vectors,
manifest signs, a production table, and mass-balance admissibility — with no
practitioner, no belief, and no new draws.

**Architecture:** A new kernel-only domain crate `domains/alchemy` owns
`QualityVector` (8 latent axes), `SignVector` (5 manifest channels), the
`Process`/`Production` table, and `permits`. It defines its own `Substrate`
struct — alchemy's copy of the material dimensions — and never imports
`hornvale-terrain` or `hornvale-species`. `windows/worldgen` carries terrain and
biosphere vocabulary into `Substrate` with pure functions, exactly as
`envelope_of` does for language today.

**Tech Stack:** Rust edition 2024, std only, `hornvale-kernel` as the sole
dependency. No serde in this crate — nothing here is serialized.

## Global Constraints

Copied from the project's standing rules; every task's requirements include
these implicitly.

- **No `HashMap`/`HashSet`.** `BTreeMap`/`BTreeSet`/`Vec` only (enforced by
  `clippy.toml` `disallowed-types`).
- **No wall-clock time anywhere.** Time is `WorldTime { day: f64 }`.
- **Dependencies:** `serde`, `serde_json`, `libm` only, workspace-wide. This
  crate adds none of them.
- **Every crate sets `#![warn(missing_docs)]`;** every public item, field, and
  variant gets a one-line doc comment.
- **Typed quantities:** coherent physical units get newtypes; **dimensionless
  ratios stay bare `f64`.** Every quality and sign here is a ratio in [0,1], so
  they stay bare `f64` and carry a `type-audit: bare-ok(ratio: <name>)` tag at
  the `pub` boundary — the same verdict `BiosphereTraits::potency` uses.
  **A `type-audit:` tag must be exactly ONE line** — the tool rejects a tag
  wrapped across two doc-comment lines, so comma-separate every verdict on a
  single line however long it gets. Tag `pub fn`s that take or return a bare
  `f64` too, using the parameter name or `return`
  (`/// type-audit: bare-ok(ratio: return)`). Confirmed against the tool in
  Task 1; the format is visible in `domains/alchemy/src/quality.rs:53,76,91`.
- **No transcendentals.** This crate uses only `+ - * clamp`, so it never
  touches `libm` and cannot introduce a cross-platform ULP difference.
- **`cargo fmt` is the final step before every commit.** Skipped fmt is the
  project's most common review finding.
- **The Reagent draws nothing.** No `streams.rs`, no `StreamLabel`, no `Seed`
  parameter anywhere in this crate. If a task seems to need a draw, stop — that
  is a spec violation, not an implementation detail.

The workspace manifest needs **no edit**: `members = ["kernel", "domains/*",
"windows/*", "cli"]` is a glob and picks the new crate up automatically.

## File structure

| File | Responsibility |
| --- | --- |
| `domains/alchemy/Cargo.toml` | Crate manifest; `hornvale-kernel` only |
| `domains/alchemy/src/lib.rs` | Crate docs, `#![warn(missing_docs)]`, module wiring, `Substrate` |
| `domains/alchemy/src/quality.rs` | `Quality`, `QualityVector`, `qualities_of` |
| `domains/alchemy/src/sign.rs` | `Sign`, `SignVector`, `signs_of` |
| `domains/alchemy/src/production.rs` | `Process`, `Requirement`, `Output`, `Production`, `PRODUCTIONS`, `permits` |
| `domains/alchemy/tests/production_properties.rs` | The property battery |
| `windows/worldgen/src/alchemy.rs` | The pure carries: `substrate_of_commodity`, `substrate_of_rock`, `substrate_of_soil`, `substrate_of_life`, `reachable_productions` |

---

### Task 1: The crate, the substrate, and the latent qualities

**Files:**
- Create: `domains/alchemy/Cargo.toml`
- Create: `domains/alchemy/src/lib.rs`
- Create: `domains/alchemy/src/quality.rs`

**Interfaces:**
- Consumes: nothing.
- Produces: `hornvale_alchemy::Substrate { metallic, organic, saline, refractory, purity: f64 }`;
  `hornvale_alchemy::quality::{Quality, QualityVector, qualities_of}` where
  `Quality` is a fieldless enum with variants `Fixity, Volatility,
  Combustibility, Solubility, Malleability, Density, Causticity, Vitality`,
  `QualityVector` has one `pub f64` field per variant (snake_case), and
  `qualities_of(s: &Substrate) -> QualityVector`. Also
  `QualityVector::get(&self, q: Quality) -> f64`.

- [ ] **Step 1: Write the failing test**

Create `domains/alchemy/src/quality.rs` with only this test module at the bottom
(the code above it comes in step 3):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::Substrate;

    /// Every axis of the derived vector stays inside [0,1] for the extreme
    /// corners of the substrate space — the invariant every later stage
    /// assumes.
    #[test]
    fn qualities_stay_in_range_at_the_corners() {
        for metallic in [0.0, 1.0] {
            for organic in [0.0, 1.0] {
                for saline in [0.0, 1.0] {
                    for refractory in [0.0, 1.0] {
                        for purity in [0.0, 1.0] {
                            let s = Substrate { metallic, organic, saline, refractory, purity };
                            let q = qualities_of(&s);
                            for axis in Quality::ALL {
                                let v = q.get(axis);
                                assert!(
                                    (0.0..=1.0).contains(&v),
                                    "{axis:?} out of range at {s:?}: {v}"
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    /// Impurity raises causticity: the hook by which The Lode's already-drawn
    /// ore `grade` reaches the quality layer. This is the mechanism the
    /// provenance confound runs on — a doctrine formed on rich ore meets poor
    /// ore in the next valley and fails.
    #[test]
    fn impurity_raises_causticity() {
        let rich = Substrate { metallic: 0.9, organic: 0.0, saline: 0.0, refractory: 0.7, purity: 0.9 };
        let poor = Substrate { purity: 0.1, ..rich };
        assert!(
            qualities_of(&poor).causticity > qualities_of(&rich).causticity,
            "poor ore must be more caustic than rich ore"
        );
    }

    /// Vitality is organic origin, carried through unchanged.
    #[test]
    fn vitality_tracks_organic_origin() {
        let living = Substrate { metallic: 0.0, organic: 1.0, saline: 0.0, refractory: 0.0, purity: 1.0 };
        let stone = Substrate { organic: 0.0, ..living };
        assert_eq!(qualities_of(&living).vitality, 1.0);
        assert_eq!(qualities_of(&stone).vitality, 0.0);
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-alchemy`
Expected: FAIL — the package does not exist yet ("error: package ID specification `hornvale-alchemy` did not match any packages").

- [ ] **Step 3: Write the minimal implementation**

Create `domains/alchemy/Cargo.toml`:

```toml
[package]
name = "hornvale-alchemy"
version = "0.1.0"
edition.workspace = true
license.workspace = true
description = "Hornvale alchemy domain: latent substance qualities, manifest signs, and the production grammar."

[dependencies]
hornvale-kernel = { path = "../../kernel" }
```

Create `domains/alchemy/src/lib.rs`:

```rust
//! The alchemy domain: the material ground truth, and nothing that believes
//! anything about it.
//!
//! A substance is a bundle of LATENT qualities ([`quality::QualityVector`]).
//! Nothing perceives a quality directly; what an observer gets are manifest
//! [`sign::SignVector`] channels — weak, partly misleading functions of the
//! qualities — and the outcomes of [`production`]s. That latent/manifest split
//! is what makes a practitioner's doctrine capable of being WRONG, and it is
//! the whole reason this domain exists (spec: The Reagent §2).
//!
//! THIS DOMAIN DRAWS NOTHING. There is no `streams.rs`, no `StreamLabel`, and
//! no `Seed` parameter anywhere in it. Chemistry is universal; per-world
//! variation arrives through which substances a world HAS, which is derived at
//! the composition root from state other domains already drew.
//!
//! LAYERING: like `hornvale_language`'s `Envelope`, [`Substrate`] is this
//! domain's OWN copy of the material dimensions it needs, populated by the
//! composition root. This crate never imports `hornvale-terrain` or
//! `hornvale-species`.

#![warn(missing_docs)]

pub mod production;
pub mod quality;
pub mod sign;

/// Alchemy's own copy of the material dimensions a substance source carries.
///
/// Populated by `windows/worldgen` from terrain's `Commodity`/`RockClass`/
/// `SoilOrder` and species' `BiosphereTraits`. Every field is a dimensionless
/// ratio in [0,1].
/// type-audit: bare-ok(ratio: metallic), bare-ok(ratio: organic),
/// type-audit: bare-ok(ratio: saline), bare-ok(ratio: refractory),
/// type-audit: bare-ok(ratio: purity)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Substrate {
    /// How metal-like the source is.
    pub metallic: f64,
    /// Living or once-living origin.
    pub organic: f64,
    /// Evaporite/salt character.
    pub saline: f64,
    /// Resistance to heat.
    pub refractory: f64,
    /// Freedom from contaminants — carries The Lode's drawn ore `grade`.
    pub purity: f64,
}

/// Clamp to the unit interval. Free of transcendentals by construction, so
/// this crate cannot introduce a cross-platform ULP difference.
pub(crate) fn clamp01(x: f64) -> f64 {
    x.clamp(0.0, 1.0)
}
```

Now prepend to `domains/alchemy/src/quality.rs`, above the test module written
in step 1:

```rust
//! The eight latent quality axes and their derivation from a [`Substrate`].
//!
//! Qualities are LATENT: nothing in the simulation reads them off a substance
//! directly. They are inferable — each one moves at least one manifest sign or
//! production outcome — but never observable. See [`crate::sign`].

use crate::{Substrate, clamp01};

/// One latent quality axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Quality {
    /// Survives fire unchanged.
    Fixity,
    /// Passes into air on heating.
    Volatility,
    /// Feeds fire.
    Combustibility,
    /// Yields to water.
    Solubility,
    /// Deforms without breaking.
    Malleability,
    /// Mass per bulk.
    Density,
    /// Attacks other matter.
    Causticity,
    /// Of living or once-living origin.
    Vitality,
}

impl Quality {
    /// Every axis, in declaration order. Iteration order is fixed so that any
    /// consumer folding over the axes is deterministic.
    pub const ALL: [Quality; 8] = [
        Quality::Fixity,
        Quality::Volatility,
        Quality::Combustibility,
        Quality::Solubility,
        Quality::Malleability,
        Quality::Density,
        Quality::Causticity,
        Quality::Vitality,
    ];
}

/// A substance's latent quality bundle. A material is a BUNDLE of qualities
/// exactly as a phoneme is a feature bundle; an atomic substance list would be
/// a lookup table, which is the catalogue-not-a-language failure this design
/// exists to avoid.
///
/// `fixity` and `volatility` are deliberately NOT forced complements — a
/// substance may be low in both (it decomposes rather than surviving or flying
/// off), and that region is where the interesting productions live.
/// type-audit: bare-ok(ratio: fixity), bare-ok(ratio: volatility),
/// type-audit: bare-ok(ratio: combustibility), bare-ok(ratio: solubility),
/// type-audit: bare-ok(ratio: malleability), bare-ok(ratio: density),
/// type-audit: bare-ok(ratio: causticity), bare-ok(ratio: vitality)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct QualityVector {
    /// Survives fire unchanged.
    pub fixity: f64,
    /// Passes into air on heating.
    pub volatility: f64,
    /// Feeds fire.
    pub combustibility: f64,
    /// Yields to water.
    pub solubility: f64,
    /// Deforms without breaking.
    pub malleability: f64,
    /// Mass per bulk.
    pub density: f64,
    /// Attacks other matter.
    pub causticity: f64,
    /// Of living or once-living origin.
    pub vitality: f64,
}

impl QualityVector {
    /// Read one axis by name.
    pub fn get(&self, q: Quality) -> f64 {
        match q {
            Quality::Fixity => self.fixity,
            Quality::Volatility => self.volatility,
            Quality::Combustibility => self.combustibility,
            Quality::Solubility => self.solubility,
            Quality::Malleability => self.malleability,
            Quality::Density => self.density,
            Quality::Causticity => self.causticity,
            Quality::Vitality => self.vitality,
        }
    }

    /// Write one axis by name, clamped to [0,1].
    pub fn set(&mut self, q: Quality, v: f64) {
        let v = clamp01(v);
        match q {
            Quality::Fixity => self.fixity = v,
            Quality::Volatility => self.volatility = v,
            Quality::Combustibility => self.combustibility = v,
            Quality::Solubility => self.solubility = v,
            Quality::Malleability => self.malleability = v,
            Quality::Density => self.density = v,
            Quality::Causticity => self.causticity = v,
            Quality::Vitality => self.vitality = v,
        }
    }
}

/// Derive a substance's latent qualities from its substrate.
///
/// Linear forms with clamping — deliberately simple and explainable, because
/// this is a tuning surface with no data behind it and the accuracy metric of
/// a later campaign is what will eventually judge the coefficients.
pub fn qualities_of(s: &Substrate) -> QualityVector {
    QualityVector {
        fixity: clamp01(0.55 * s.refractory + 0.35 * s.metallic - 0.30 * s.organic + 0.20),
        volatility: clamp01(0.60 * s.organic + 0.30 * s.saline - 0.40 * s.refractory + 0.15),
        combustibility: clamp01(0.75 * s.organic - 0.35 * s.metallic + 0.10),
        solubility: clamp01(0.80 * s.saline - 0.45 * s.metallic - 0.20 * s.refractory + 0.25),
        malleability: clamp01(0.80 * s.metallic - 0.30 * s.refractory + 0.05),
        density: clamp01(0.70 * s.metallic + 0.15 * s.refractory - 0.35 * s.organic + 0.25),
        // Impurity is caustic: this is where The Lode's drawn ore `grade`
        // reaches the quality layer, and therefore where the provenance
        // confound gets its physical mechanism.
        causticity: clamp01(0.55 * s.saline + 0.25 * (1.0 - s.purity) + 0.10),
        vitality: clamp01(s.organic),
    }
}
```

**Keep the orderings aligned.** The `Quality` variants, the `Quality::ALL`
array, the `QualityVector` fields, and the `get`/`set` match arms are all in the
same order deliberately. Match arms are order-independent to the compiler, but a
reader checks the four against each other; keep them in step when adding an
axis.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-alchemy`
Expected: PASS, 3 tests.

- [ ] **Step 5: Check formatting, lints, and layering**

```bash
cargo fmt
cargo clippy -p hornvale-alchemy --all-targets -- -D warnings
cargo test -p hornvale --test architecture
```
Expected: clean; `architecture` passes (the new crate depends on the kernel alone).

- [ ] **Step 6: Commit**

```bash
git add domains/alchemy Cargo.lock
git commit -m "feat(alchemy): the crate, the substrate, and the eight latent qualities (The Reagent T1)"
```

---

### Task 2: Manifest signs, and the collision that makes signatures wrong

**Files:**
- Create: `domains/alchemy/src/sign.rs`

**Interfaces:**
- Consumes: `crate::quality::QualityVector` from Task 1.
- Produces: `hornvale_alchemy::sign::{Sign, SignVector, signs_of}` where `Sign`
  is a fieldless enum with variants `Heft, Grain, Lustre, Odour, Hue`,
  `SignVector` has one `pub f64` field per variant, and
  `signs_of(q: &QualityVector) -> SignVector`.

This task's second test is the campaign's most important assertion: it proves
the confound is *possible*. Nothing else in the suite tests for the existence
of the gap between truth and appearance, and without it the program could ship
with its subject matter deleted and every other test still green.

- [ ] **Step 1: Write the failing test**

Create `domains/alchemy/src/sign.rs` with only this test module (code above it
comes in step 3):

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::quality::QualityVector;

    /// Heft is a near-faithful read of density: a practitioner who reasons
    /// from weight is reasoning well.
    #[test]
    fn heft_tracks_density_faithfully() {
        let heavy = QualityVector { density: 0.9, ..QualityVector::default() };
        let light = QualityVector { density: 0.1, ..QualityVector::default() };
        assert!(signs_of(&heavy).heft > signs_of(&light).heft);
    }

    /// THE CONFOUND, MECHANIZED. `hue` conflates causticity with vitality, so
    /// a violently caustic mineral and an inert living thing present the SAME
    /// colour. A practitioner reasoning "the root is red, so it treats blood"
    /// is therefore wrong for a derivable reason -- and nobody authored a
    /// superstition to make it happen.
    ///
    /// If this test ever fails because the two hues diverge, the doctrine of
    /// signatures has become sound in-world and the program has lost its
    /// subject matter. Do not "fix" it by making hue faithful.
    #[test]
    fn hue_collides_across_utterly_different_substances() {
        let caustic_mineral =
            QualityVector { causticity: 0.9, vitality: 0.0, ..QualityVector::default() };
        let inert_living =
            QualityVector { causticity: 0.0, vitality: 1.0, ..QualityVector::default() };

        let a = signs_of(&caustic_mineral);
        let b = signs_of(&inert_living);

        assert_eq!(a.hue, b.hue, "hue must not distinguish these two");
        assert_ne!(
            caustic_mineral.vitality, inert_living.vitality,
            "...while the substances differ maximally in what matters"
        );
    }

    /// The projection is total: every quality vector yields signs in range.
    #[test]
    fn signs_stay_in_range() {
        for v in [0.0, 0.5, 1.0] {
            let q = QualityVector {
                fixity: v,
                volatility: v,
                combustibility: v,
                solubility: v,
                malleability: v,
                density: v,
                causticity: v,
                vitality: v,
            };
            let s = signs_of(&q);
            for channel in [s.heft, s.grain, s.lustre, s.odour, s.hue] {
                assert!((0.0..=1.0).contains(&channel), "sign out of range: {channel}");
            }
        }
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-alchemy --lib sign`
Expected: FAIL to compile — `signs_of` and `SignVector` are not defined.

- [ ] **Step 3: Write the minimal implementation**

Prepend to `domains/alchemy/src/sign.rs`:

```rust
//! The five manifest sign channels — everything an observer can actually
//! perceive of a substance.
//!
//! Each sign is a PARTIAL and partly misleading function of the latent
//! [`crate::quality::QualityVector`]. The faithfulness gradient is deliberate
//! and is the design's whole point of contact with the historical doctrine of
//! signatures: `heft` is near-faithful, `hue` is weak and misleading. A
//! practitioner reasoning from colour will be wrong in a specific, derivable,
//! reproducible way.

use crate::clamp01;
use crate::quality::QualityVector;

/// One manifest sign channel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Sign {
    /// Perceived weight. Near-faithful to density.
    Heft,
    /// Perceived texture. A good read of fixity and malleability.
    Grain,
    /// Perceived shine. A moderate read.
    Lustre,
    /// Perceived smell. Weak — conflates volatility with vitality.
    Odour,
    /// Perceived colour. Weak and misleading — conflates causticity with
    /// vitality, and is the mechanized doctrine of signatures.
    Hue,
}

/// What an observer gets. Every field is a dimensionless ratio in [0,1].
/// type-audit: bare-ok(ratio: heft), bare-ok(ratio: grain), bare-ok(ratio: lustre), bare-ok(ratio: odour), bare-ok(ratio: hue)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub struct SignVector {
    /// Perceived weight.
    pub heft: f64,
    /// Perceived texture.
    pub grain: f64,
    /// Perceived shine.
    pub lustre: f64,
    /// Perceived smell.
    pub odour: f64,
    /// Perceived colour.
    pub hue: f64,
}

/// Project latent qualities into what can be perceived.
///
/// The coefficients encode the faithfulness gradient documented on [`Sign`].
/// `hue`'s two coefficients are close together on purpose: it is the channel
/// that must FAIL to distinguish substances a practitioner cares about.
pub fn signs_of(q: &QualityVector) -> SignVector {
    SignVector {
        heft: clamp01(0.95 * q.density + 0.05),
        grain: clamp01(0.55 * q.fixity + 0.40 * q.malleability),
        lustre: clamp01(0.60 * q.malleability + 0.30 * q.fixity),
        odour: clamp01(0.50 * q.volatility + 0.45 * q.vitality),
        hue: clamp01(0.50 * q.causticity + 0.45 * q.vitality),
    }
}
```

Check the collision arithmetic by hand before running: the caustic mineral
gives `0.50 * 0.9 + 0.45 * 0.0 = 0.45`; the living thing gives
`0.50 * 0.0 + 0.45 * 1.0 = 0.45`. Equal, exactly, with no floating-point
tolerance needed.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-alchemy`
Expected: PASS, 6 tests.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-alchemy --all-targets -- -D warnings
git add domains/alchemy
git commit -m "feat(alchemy): manifest signs, and the hue collision that mechanizes signatures (The Reagent T2)"
```

---

### Task 3: Processes, productions, and mass balance

**Files:**
- Create: `domains/alchemy/src/production.rs`

**Interfaces:**
- Consumes: `crate::quality::{Quality, QualityVector}` from Task 1;
  `crate::sign::Sign` from Task 2.
- Produces: `hornvale_alchemy::production::{Process, Requirement, Output,
  Production, PRODUCTIONS, permits, admits}` where
  `permits(p: &Production) -> bool` is mass balance and
  `admits(p: &Production, q: &QualityVector) -> bool` is the precondition
  match. `PRODUCTIONS` is a `&'static [Production]` of length 7.

- [ ] **Step 1: Write the failing test**

Create `domains/alchemy/src/production.rs` with only this test module:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::quality::QualityVector;

    /// Every authored production balances mass. This is the mundane tier's
    /// single invariant, and the one predicate UNI-2 would later relax.
    #[test]
    fn every_production_balances_mass() {
        for p in PRODUCTIONS {
            assert!(permits(p), "{} does not balance mass", p.name);
        }
    }

    /// Every process in the inventory is exercised by at least one
    /// production -- no dead vocabulary.
    #[test]
    fn every_process_is_used() {
        for process in Process::ALL {
            assert!(
                PRODUCTIONS.iter().any(|p| p.process == process),
                "{process:?} has no production"
            );
        }
    }

    /// A production admits a substance only when every requirement holds.
    #[test]
    fn admission_respects_requirements() {
        let burn = PRODUCTIONS.iter().find(|p| p.name == "burn-fuel").expect("burn-fuel exists");
        let fuel = QualityVector { combustibility: 0.8, ..QualityVector::default() };
        let stone = QualityVector { combustibility: 0.1, ..QualityVector::default() };
        assert!(admits(burn, &fuel));
        assert!(!admits(burn, &stone));
    }

    /// An unbalanced production is rejected -- proving `permits` can say no,
    /// rather than passing because it never fires.
    #[test]
    fn permits_rejects_an_unbalanced_production() {
        let bad = Production {
            name: "ex-nihilo",
            process: Process::Calcine,
            inputs: 1,
            requires: &[],
            outputs: &[Output { bulk: 1.5, deltas: &[] }],
            emits: Sign::Hue,
        };
        assert!(!permits(&bad), "1.5 out of 1.0 in must not balance");
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-alchemy --lib production`
Expected: FAIL to compile — `Process`, `Production`, `PRODUCTIONS`, `permits`, `admits` undefined.

- [ ] **Step 3: Write the minimal implementation**

Prepend to `domains/alchemy/src/production.rs`:

```rust
//! The production grammar: preconditions, a process, effects, and a
//! conservation witness.
//!
//! The authoring notation each entry in [`PRODUCTIONS`] transcribes is:
//!
//! ```text
//!   calcine:  [ volatility >= 0.4, fixity < 0.5 ]
//!             --fire-->
//!             [ volatility -= 0.4, fixity += 0.3 ]
//!             ! mass-balance
//!             ~ fume(acrid)
//! ```
//!
//! The `~` slot can only be filled by a [`Sign`], never by a quality, because
//! it is what an OBSERVER gets — the notation's required slots are what forced
//! the latent/manifest split in the first place.

use crate::quality::{Quality, QualityVector};
use crate::sign::Sign;

/// Tolerance for the mass-balance comparison. Fixed and tiny; the arithmetic
/// is a sum of authored constants, so this absorbs representation error only.
const BALANCE_EPSILON: f64 = 1e-9;

/// An authored operation a practitioner can perform.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Process {
    /// Reduce to powder.
    Grind,
    /// Drive with fire.
    Calcine,
    /// Take up in water.
    Dissolve,
    /// Separate by boiling and catching the vapour.
    Distil,
    /// Let living matter work on itself.
    Ferment,
    /// Combine two inputs into one body.
    Amalgamate,
}

impl Process {
    /// Every process, in declaration order.
    pub const ALL: [Process; 6] = [
        Process::Grind,
        Process::Calcine,
        Process::Dissolve,
        Process::Distil,
        Process::Ferment,
        Process::Amalgamate,
    ];
}

/// A precondition on one quality axis: `min <= value <= max`.
/// type-audit: bare-ok(ratio: min), bare-ok(ratio: max)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Requirement {
    /// The axis constrained.
    pub quality: Quality,
    /// Inclusive lower bound.
    pub min: f64,
    /// Inclusive upper bound.
    pub max: f64,
}

/// One product of a production.
/// type-audit: bare-ok(ratio: bulk)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Output {
    /// Share of the total input bulk this product carries. The `bulk` values
    /// of a production's outputs must sum to its input count.
    pub bulk: f64,
    /// Quality deltas applied to the input bundle to make this product.
    pub deltas: &'static [(Quality, f64)],
}

/// An authored production: the grammar's primitive.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Production {
    /// Stable identifier, used in tests and diagnostics.
    pub name: &'static str,
    /// The operation performed.
    pub process: Process,
    /// How many substance bodies go in (1 for most; 2 for `Amalgamate`).
    /// type-audit: bare-ok(count: inputs)
    pub inputs: usize,
    /// Preconditions on the input bundle.
    pub requires: &'static [Requirement],
    /// What comes out.
    pub outputs: &'static [Output],
    /// The sign channel in which the reaction manifests to an observer.
    pub emits: Sign,
}

/// Admissibility: does this production balance mass?
///
/// The summed bulk of the outputs — fume and residue included — must equal the
/// input count. This is the mundane tier's single invariant; opening UNI-2
/// later relaxes exactly this predicate and nothing else in the architecture.
pub fn permits(p: &Production) -> bool {
    let total: f64 = p.outputs.iter().map(|o| o.bulk).sum();
    (total - p.inputs as f64).abs() < BALANCE_EPSILON
}

/// Does a substance with these qualities satisfy the production's
/// preconditions?
pub fn admits(p: &Production, q: &QualityVector) -> bool {
    p.requires.iter().all(|r| {
        let v = q.get(r.quality);
        v >= r.min && v <= r.max
    })
}

/// The authored production table. Universal — identical in every world. Per-
/// world difference arrives through which of these are REACHABLE, which
/// depends on what a world is made of.
pub const PRODUCTIONS: &[Production] = &[
    Production {
        name: "calcine-ore",
        process: Process::Calcine,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Fixity, min: 0.4, max: 1.0 }],
        outputs: &[
            Output { bulk: 0.7, deltas: &[(Quality::Fixity, 0.2), (Quality::Malleability, 0.3)] },
            Output { bulk: 0.3, deltas: &[(Quality::Volatility, 0.5)] },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "dissolve-salt",
        process: Process::Dissolve,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Solubility, min: 0.6, max: 1.0 }],
        outputs: &[Output { bulk: 1.0, deltas: &[(Quality::Solubility, 0.1)] }],
        emits: Sign::Hue,
    },
    Production {
        name: "grind-stone",
        process: Process::Grind,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Malleability, min: 0.0, max: 0.3 }],
        outputs: &[Output { bulk: 1.0, deltas: &[] }],
        emits: Sign::Grain,
    },
    Production {
        name: "burn-fuel",
        process: Process::Calcine,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Combustibility, min: 0.6, max: 1.0 }],
        outputs: &[
            Output {
                bulk: 0.2,
                deltas: &[(Quality::Combustibility, -0.6), (Quality::Fixity, 0.4)],
            },
            Output { bulk: 0.8, deltas: &[(Quality::Volatility, 0.6)] },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "distil-spirit",
        process: Process::Distil,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Volatility, min: 0.5, max: 1.0 }],
        outputs: &[
            Output { bulk: 0.4, deltas: &[(Quality::Volatility, 0.3)] },
            Output { bulk: 0.6, deltas: &[(Quality::Volatility, -0.4), (Quality::Fixity, 0.2)] },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "ferment-must",
        process: Process::Ferment,
        inputs: 1,
        requires: &[Requirement { quality: Quality::Vitality, min: 0.5, max: 1.0 }],
        outputs: &[
            Output { bulk: 0.9, deltas: &[(Quality::Vitality, -0.2), (Quality::Volatility, 0.3)] },
            Output { bulk: 0.1, deltas: &[(Quality::Volatility, 0.7)] },
        ],
        emits: Sign::Odour,
    },
    Production {
        name: "amalgamate-alloy",
        process: Process::Amalgamate,
        inputs: 2,
        requires: &[Requirement { quality: Quality::Malleability, min: 0.5, max: 1.0 }],
        outputs: &[Output { bulk: 2.0, deltas: &[(Quality::Malleability, 0.1)] }],
        emits: Sign::Lustre,
    },
];
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-alchemy`
Expected: PASS, 10 tests.

- [ ] **Step 5: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-alchemy --all-targets -- -D warnings
git add domains/alchemy
git commit -m "feat(alchemy): processes, the production table, and mass balance (The Reagent T3)"
```

---

### Task 4: The composition-root carry, and per-world reachability

**Files:**
- Create: `windows/worldgen/src/alchemy.rs`
- Modify: `windows/worldgen/Cargo.toml` (add the dependency)
- Modify: `windows/worldgen/src/lib.rs` (add `pub mod alchemy;` beside the existing module declarations)

**Interfaces:**
- Consumes: `hornvale_alchemy::{Substrate, quality::qualities_of, production::{PRODUCTIONS, admits}}`.
- Produces: `hornvale_worldgen::alchemy::{substrate_of_commodity, substrate_of_rock, substrate_of_soil, substrate_of_life, reachable_productions}`.
  Signatures:
  - `substrate_of_commodity(c: Commodity, grade: f64) -> Substrate`
  - `substrate_of_rock(r: RockClass) -> Substrate`
  - `substrate_of_soil(s: SoilOrder) -> Substrate`
  - `substrate_of_life() -> Substrate`
  - `reachable_productions(sources: &[Substrate]) -> Vec<&'static str>` — sorted, deduplicated production names.

This is the only task that touches terrain and species vocabulary. The domain
crate must remain ignorant of both; if you find yourself adding
`hornvale-terrain` to `domains/alchemy/Cargo.toml`, stop — that is the mistake
this task exists to prevent, and `cli/tests/architecture.rs` will reject it.

- [ ] **Step 1: Write the failing test**

Append to `windows/worldgen/src/alchemy.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    /// Salt dissolves; gold does not. The carry has to preserve the material
    /// distinctions that make productions differ.
    #[test]
    fn salt_dissolves_and_gold_does_not() {
        let salt = qualities_of(&substrate_of_commodity(Commodity::Salt, 0.8));
        let gold = qualities_of(&substrate_of_commodity(Commodity::Gold, 0.8));
        assert!(salt.solubility > 0.6, "salt must be soluble: {}", salt.solubility);
        assert!(gold.solubility < 0.3, "gold must not be: {}", gold.solubility);
    }

    /// Coal burns; granite does not.
    #[test]
    fn coal_burns_and_granite_does_not() {
        let coal = qualities_of(&substrate_of_commodity(Commodity::Coal, 0.7));
        let granite = qualities_of(&substrate_of_rock(RockClass::Granite));
        assert!(coal.combustibility > 0.6);
        assert!(granite.combustibility < 0.2);
    }

    /// Ore grade reaches the quality layer: poor ore is more caustic than
    /// rich ore of the SAME commodity. This is the provenance confound's
    /// physical mechanism, and the reason a doctrine formed in one valley
    /// fails in the next.
    #[test]
    fn ore_grade_moves_causticity() {
        let rich = qualities_of(&substrate_of_commodity(Commodity::Copper, 0.9));
        let poor = qualities_of(&substrate_of_commodity(Commodity::Copper, 0.1));
        assert!(poor.causticity > rich.causticity);
    }

    /// Worlds made of different things reach different productions. This is
    /// the campaign's one substantive claim about the WORLD rather than about
    /// the code.
    #[test]
    fn different_material_endowments_reach_different_productions() {
        let mining_world = [
            substrate_of_commodity(Commodity::Copper, 0.6),
            substrate_of_commodity(Commodity::Iron, 0.7),
            substrate_of_rock(RockClass::Granite),
        ];
        let marsh_world = [
            substrate_of_soil(SoilOrder::Histosol),
            substrate_of_commodity(Commodity::Salt, 0.5),
            substrate_of_life(),
        ];

        let mining = reachable_productions(&mining_world);
        let marsh = reachable_productions(&marsh_world);

        assert_ne!(mining, marsh, "endowments this different must diverge");
        assert!(marsh.contains(&"ferment-must"), "a living marsh can ferment: {marsh:?}");
        assert!(!mining.contains(&"ferment-must"), "bare rock cannot: {mining:?}");
    }

    /// The carry is a pure function: same input, same output, always. It
    /// takes no seed and consumes no stream, which is what keeps The Reagent
    /// free of a save-format contract.
    #[test]
    fn the_carry_is_pure() {
        for _ in 0..3 {
            assert_eq!(
                substrate_of_commodity(Commodity::Tin, 0.42),
                substrate_of_commodity(Commodity::Tin, 0.42)
            );
        }
    }
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cargo test -p hornvale-worldgen --lib alchemy`
Expected: FAIL to compile — the module and its functions do not exist.

- [ ] **Step 3: Write the minimal implementation**

Add to `windows/worldgen/Cargo.toml` under `[dependencies]`, keeping the
existing entries' style:

```toml
hornvale-alchemy = { path = "../../domains/alchemy" }
```

Add to `windows/worldgen/src/lib.rs`, beside the other module declarations:

```rust
pub mod alchemy;
```

Prepend to `windows/worldgen/src/alchemy.rs`:

```rust
//! The composition root's alchemy carry: terrain and biosphere vocabulary in,
//! `hornvale_alchemy::Substrate` out.
//!
//! This is the ONLY place the two vocabularies meet. `domains/alchemy` knows
//! about qualities, not about rocks — it never imports `hornvale-terrain` or
//! `hornvale-species` — so the translation lives here, in the same shape and
//! for the same reason as [`crate::envelope_of`] does for language.
//!
//! Every function here is PURE: no seed, no stream, no draw. That is what
//! keeps The Reagent free of a save-format contract.

use hornvale_alchemy::Substrate;
use hornvale_alchemy::production::{PRODUCTIONS, admits};
use hornvale_alchemy::quality::qualities_of;
use hornvale_species::BiosphereTraits;
use hornvale_terrain::{Commodity, RockClass, SoilOrder};

/// Carry an ore deposit into a substrate. `grade` is the deposit's already-
/// drawn ore grade in [0,1] — the one place a drawn quantity reaches alchemy,
/// and therefore the physical basis of the provenance confound.
pub fn substrate_of_commodity(c: Commodity, grade: f64) -> Substrate {
    let (metallic, organic, saline, refractory) = match c {
        Commodity::Copper => (0.90, 0.00, 0.10, 0.40),
        Commodity::Gold => (1.00, 0.00, 0.00, 0.60),
        Commodity::LeadZinc => (0.85, 0.00, 0.15, 0.35),
        Commodity::Iron => (0.90, 0.00, 0.00, 0.70),
        Commodity::Salt => (0.00, 0.00, 1.00, 0.10),
        Commodity::Coal => (0.00, 0.90, 0.00, 0.15),
        Commodity::Gems => (0.20, 0.00, 0.00, 0.95),
        Commodity::Tin => (0.80, 0.00, 0.05, 0.25),
        Commodity::Bauxite => (0.60, 0.00, 0.10, 0.50),
    };
    Substrate { metallic, organic, saline, refractory, purity: grade.clamp(0.0, 1.0) }
}

/// Carry a rock class into a substrate, by petrological family. Grouped rather
/// than enumerated one-per-variant because the families are what the quality
/// axes actually respond to.
pub fn substrate_of_rock(r: RockClass) -> Substrate {
    let (metallic, organic, saline, refractory) = match r {
        // Felsic igneous.
        RockClass::Granite | RockClass::Rhyolite => (0.25, 0.00, 0.00, 0.75),
        // Mafic igneous.
        RockClass::Gabbro | RockClass::Basalt | RockClass::Andesite => (0.45, 0.00, 0.00, 0.80),
        // Siliceous clastics and their metamorphic equivalent.
        RockClass::Sandstone
        | RockClass::Conglomerate
        | RockClass::Chert
        | RockClass::Quartzite => (0.10, 0.00, 0.00, 0.90),
        // Argillaceous.
        RockClass::Shale | RockClass::Slate => (0.20, 0.05, 0.05, 0.50),
        // Chemical.
        RockClass::Evaporite => (0.00, 0.00, 1.00, 0.10),
        RockClass::Ironstone => (0.85, 0.00, 0.00, 0.70),
        RockClass::ReefLimestone | RockClass::Marble => (0.05, 0.10, 0.05, 0.35),
        RockClass::Coal => (0.00, 0.90, 0.00, 0.15),
        // Higher-grade metamorphic.
        RockClass::Schist | RockClass::Gneiss => (0.30, 0.00, 0.00, 0.70),
        // Unconsolidated.
        RockClass::Alluvium => (0.15, 0.10, 0.05, 0.40),
    };
    // Bedrock is what it is: no ore-grade dilution applies.
    Substrate { metallic, organic, saline, refractory, purity: 1.0 }
}

/// Carry a soil order into a substrate.
pub fn substrate_of_soil(s: SoilOrder) -> Substrate {
    let (metallic, organic, saline, refractory) = match s {
        SoilOrder::Laterite => (0.55, 0.05, 0.00, 0.50),
        SoilOrder::Podzol => (0.10, 0.40, 0.00, 0.25),
        SoilOrder::Chernozem => (0.05, 0.60, 0.00, 0.20),
        SoilOrder::Aridisol => (0.10, 0.10, 0.60, 0.30),
        SoilOrder::Loam => (0.10, 0.45, 0.05, 0.25),
        SoilOrder::Andosol => (0.25, 0.35, 0.00, 0.50),
        SoilOrder::Leptosol => (0.20, 0.10, 0.00, 0.60),
        SoilOrder::Histosol => (0.00, 0.95, 0.00, 0.10),
        SoilOrder::Gley => (0.10, 0.50, 0.05, 0.20),
    };
    Substrate { metallic, organic, saline, refractory, purity: 1.0 }
}

/// Carry living matter into a substrate. Every organism is, alchemically, the
/// same kind of thing at this fidelity: organic and nothing else. Species
/// differentiation is deliberately out of scope for The Reagent.
pub fn substrate_of_life() -> Substrate {
    Substrate { metallic: 0.0, organic: 1.0, saline: 0.05, refractory: 0.05, purity: 1.0 }
}

/// Carry a species' biosphere traits into a substrate. Currently identical to
/// [`substrate_of_life`]; the parameter is taken so that later campaigns can
/// differentiate by trait without changing every call site.
pub fn substrate_of_traits(_t: &BiosphereTraits) -> Substrate {
    substrate_of_life()
}

/// Which productions a world endowed with these material sources can reach.
///
/// Returned sorted and deduplicated so the result is deterministic and
/// directly comparable between worlds.
pub fn reachable_productions(sources: &[Substrate]) -> Vec<&'static str> {
    let mut names: Vec<&'static str> = PRODUCTIONS
        .iter()
        .filter(|p| sources.iter().any(|s| admits(p, &qualities_of(s))))
        .map(|p| p.name)
        .collect();
    names.sort_unstable();
    names.dedup();
    names
}
```

These import paths are verified against the tree: `domains/terrain/src/lib.rs:32`
re-exports `Commodity` and `Deposit` at the crate root, and line 34 does the
same for `RockClass` and `SoilOrder`. Import from the crate root, as
`windows/worldgen` already does elsewhere (`use hornvale_terrain::RockClass::*`
at `lib.rs:2368`) — do not reach into the `features`/`lithology` submodules and
do not add a new re-export.

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p hornvale-worldgen --lib alchemy`
Expected: PASS, 5 tests.

- [ ] **Step 5: Verify the layering law still holds**

```bash
cargo test -p hornvale --test architecture
```
Expected: PASS. If this fails with a domain-depends-on-domain error, the
dependency was added to `domains/alchemy/Cargo.toml` instead of
`windows/worldgen/Cargo.toml`. Move it.

- [ ] **Step 6: Format, lint, commit**

```bash
cargo fmt
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings
git add windows/worldgen domains/alchemy Cargo.lock
git commit -m "feat(worldgen): carry terrain and biosphere into alchemy substrates (The Reagent T4)"
```

---

### Task 5: The property battery, the type audit, and the gate

**Files:**
- Create: `domains/alchemy/tests/production_properties.rs`

**Interfaces:**
- Consumes: everything from Tasks 1–3.
- Produces: no new public API — this task is evidence.

- [ ] **Step 1: Write the property battery**

Create `domains/alchemy/tests/production_properties.rs`:

```rust
//! The Reagent's property battery: the invariants every later campaign in The
//! Crucible inherits.

use hornvale_alchemy::Substrate;
use hornvale_alchemy::production::{PRODUCTIONS, admits, permits};
use hornvale_alchemy::quality::{Quality, QualityVector, qualities_of};
use hornvale_alchemy::sign::signs_of;

/// A coarse sweep of the substrate space, used by several properties below.
fn sweep() -> Vec<Substrate> {
    let mut out = Vec::new();
    for m in [0.0, 0.5, 1.0] {
        for o in [0.0, 0.5, 1.0] {
            for s in [0.0, 0.5, 1.0] {
                for r in [0.0, 0.5, 1.0] {
                    for p in [0.0, 0.5, 1.0] {
                        out.push(Substrate {
                            metallic: m,
                            organic: o,
                            saline: s,
                            refractory: r,
                            purity: p,
                        });
                    }
                }
            }
        }
    }
    out
}

#[test]
fn every_production_balances_mass() {
    for p in PRODUCTIONS {
        assert!(permits(p), "{} does not balance", p.name);
    }
}

#[test]
fn production_names_are_unique() {
    let mut names: Vec<&str> = PRODUCTIONS.iter().map(|p| p.name).collect();
    names.sort_unstable();
    let before = names.len();
    names.dedup();
    assert_eq!(before, names.len(), "duplicate production name");
}

#[test]
fn qualities_and_signs_stay_in_range_across_the_sweep() {
    for s in sweep() {
        let q = qualities_of(&s);
        for axis in Quality::ALL {
            let v = q.get(axis);
            assert!((0.0..=1.0).contains(&v), "{axis:?} out of range at {s:?}");
        }
        let sg = signs_of(&q);
        for channel in [sg.heft, sg.grain, sg.lustre, sg.odour, sg.hue] {
            assert!((0.0..=1.0).contains(&channel), "sign out of range at {s:?}");
        }
    }
}

/// Derivation is a pure function: the same substrate always yields the same
/// qualities. The Reagent draws nothing, so this must hold trivially -- and is
/// asserted anyway, because it is the claim a later change is most likely to
/// break silently.
#[test]
fn derivation_is_pure() {
    for s in sweep() {
        assert_eq!(qualities_of(&s), qualities_of(&s));
    }
}

/// No production is dead: each one admits at least one reachable substance.
/// An unreachable production would be authored vocabulary nothing can ever
/// use.
#[test]
fn no_production_is_unreachable() {
    let space: Vec<QualityVector> = sweep().iter().map(qualities_of).collect();
    for p in PRODUCTIONS {
        assert!(
            space.iter().any(|q| admits(p, q)),
            "{} is unreachable from any substrate",
            p.name
        );
    }
}

/// No production admits EVERYTHING: a precondition that always holds is not a
/// precondition, and would make the material layer uniform.
#[test]
fn no_production_admits_everything() {
    let space: Vec<QualityVector> = sweep().iter().map(qualities_of).collect();
    for p in PRODUCTIONS {
        if p.requires.is_empty() {
            continue;
        }
        assert!(
            space.iter().any(|q| !admits(p, q)),
            "{} admits every substance -- its requirements are vacuous",
            p.name
        );
    }
}
```

- [ ] **Step 2: Run the battery**

Run: `cargo test -p hornvale-alchemy --test production_properties`
Expected: PASS, 6 tests. If `no_production_is_unreachable` or
`no_production_admits_everything` fails, the production table needs its
thresholds adjusted — that is a real finding about the authored inventory, not
a test to relax.

- [ ] **Step 3: Regenerate the type-audit report**

The audit is default-deny: any untagged primitive at a `pub` boundary fails it.

```bash
cargo run --manifest-path tools/type-audit/Cargo.toml -- check
cargo run --manifest-path tools/type-audit/Cargo.toml -- report > docs/audits/type-audit-report.md
```
Expected: `check` exits 0. If it names an untagged primitive, add the
`type-audit: bare-ok(ratio: <name>)` tag to that item — every bare `f64` in
this campaign is a dimensionless ratio and takes that verdict.

- [ ] **Step 4: Run the full commit gate**

```bash
cargo fmt
make gate
```
Expected: green, ~4 minutes. This campaign registers no lab metric and commits
no generated artifact, so **no census regeneration is owed** — do not run one.

- [ ] **Step 5: Commit**

```bash
git add domains/alchemy docs/audits/type-audit-report.md
git commit -m "test(alchemy): the property battery, and the type audit (The Reagent T5)"
```

---

## Self-review

**Spec coverage.** §3.1 qualities → T1. §3.2 signs → T2. §3.3 processes and §4
notation → T3. §5 universal/derived/drawn, and the "draws nothing" claim → T1's
crate docs, T4's purity test, T5's `derivation_is_pure`. §6 architecture → T1
(crate), T4 (carry), T1/T4 step-5 `architecture` runs. §7 conservation → T3
`permits`. §8 evidence: item 1 → T5 battery; item 2 → T5 `derivation_is_pure`;
item 3 → T4 `different_material_endowments_reach_different_productions`; item 4
→ T1 and T4 architecture runs. §2's latent/manifest amendment → T2's
`hue_collides_across_utterly_different_substances`.

**No spec requirement is unimplemented.**

**Type consistency.** `Substrate` fields (`metallic`, `organic`, `saline`,
`refractory`, `purity`) are identical in T1's definition, T4's four carries, and
T5's `sweep`. `qualities_of(&Substrate) -> QualityVector` and
`signs_of(&QualityVector) -> SignVector` keep their signatures across T1, T2,
T4, T5. `admits(&Production, &QualityVector)` and `permits(&Production)` are
used with those exact argument orders in T3, T4, and T5.

**Ordering:** `Quality`'s variants, `Quality::ALL`, `QualityVector`'s fields,
and the `get`/`set` match arms are all in the same order, and T1 step 3 says to
keep them in step.

**Known dead branch:** `no_production_admits_everything` skips productions with
no requirements, and all seven current productions have at least one. The guard
is there so the property stays correct when a later campaign adds an
unconditional production; it is not an oversight.

## Close (G6)

Not part of the task list — run after T5 is green, per the campaign definition
of done: chronicle entry in `book/src/chronicle/the-assay.md`; freshness sweep;
Confidence Gradient re-score if a bet moved; retrospective in
`docs/retrospectives/the-assay.md`; flip `ALCH-1`/`ALCH-3`/`ALCH-7` registry
rows from `spec'd` to `shipped`; absorb `main` and re-run `make gate` before
merging.
