# The Formations Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Status:** COMPLETE — merged. See [the chronicle](../../../book/src/chronicle/the-formations.md).

**Goal:** Disentangle `climate::Biome`'s two conflated taxonomies — community
types and depth strata — into the faceted expression The Stratum specifies,
without moving a single byte of any world.

**Architecture:** A new `BiomeExpr { realm, formation, stratum }` becomes the
*truth*, and the existing `Biome` enum becomes a **projection** of it. Every
current consumer keeps calling `biome_at()` and keeps receiving exactly the
`Biome` it receives today. The projection is the compatibility seam and the
byte-identity guarantee: if it is right, nothing downstream can tell the
refactor happened.

**Design authority:** `docs/superpowers/specs/2026-07-28-the-stratum-design.md`
(The Stratum). This plan implements its campaign 1 only.

**One clarification against the spec.** §3.1 says `classify_marine` "loses its
precedence chain". That is true of the *stratum* — depth no longer competes
with community, so nothing has to win — but **formation selection keeps its
order**, because that order is current behaviour and byte-identity depends on
it. The chain does not disappear; it stops deciding two questions at once.
Task 3's sweep is what holds this honest.

**Tech Stack:** Rust 2024, std + `serde`/`serde_json` only. `cargo nextest`.

## Global Constraints

- **Layering:** `kernel/` → `domains/*` → `windows/*` → `cli/`. `domains/climate`
  depends on `hornvale-kernel` and nothing else.
- **No new dependencies.** No `HashMap`/`HashSet`. No wall-clock time.
- **`#![warn(missing_docs)]`** — every public item, field, and variant gets a
  one-line doc comment, and every `pub`-boundary primitive a `type-audit:` tag
  **on a single line** (a multi-line tag is silently malformed).
- **Valid `bare-ok` classes only:** `ratio, count, index, constructor-edge,
  envelope, identifier-text, prose, artifact, diagnostic-value,
  render-internal, flag`. There is no `threshold` class.
- **THE INVARIANT: no world byte moves.** This campaign is a pure refactor.
  `cli/tests/lens_purity.rs::seed_42_world_json_matches_the_committed_fixture`
  already pins seed-42's world JSON, so drift fails the gate automatically —
  **never rebaseline that fixture in this campaign.** If it goes red, the
  projection is wrong; fix the projection.
- **No new concepts, no epoch, no census regen.** Concepts and name glosses
  belong to campaign 2 (The Variants). If a task appears to need one, STOP.
- **`Biome::catalog()` order is the `scene/tiles` legend order.** Never reorder
  it; never remove a variant.
- Run `cargo fmt` as the final step before every commit.
- Gate per task: `cargo nextest run -p hornvale-climate`. Full `make gate` at
  the close.

## The projection table (load-bearing)

Legacy `classify_marine` (`domains/climate/src/biome.rs:349`) decides in this
exact order. The new code must preserve it, because the projection has to
reproduce the same `Biome` for every input:

| # | legacy condition | new `formation` | new `stratum` | projects back to |
|---|---|---|---|---|
| 1 | `sst < SEA_ICE_C` | `SeaIce` | by depth | `SeaIce` |
| 2 | `Trench && depth > 6000` | `OpenWater` | `Hadal` | `HadalTrench` |
| 3 | `Ridge` | `Vent` | by depth | `HydrothermalVent` |
| 4 | `depth < 200 && sst > 20` | `Reef` | `Epipelagic` | `CoralReef` |
| 4 | `depth < 200 && sst < 12` | `Kelp` | `Epipelagic` | `KelpForest` |
| 5 | `upwelling && depth < 1000` | `Upwelling` | by depth | `Upwelling` |
| 6 | otherwise | `OpenWater` | by depth | per stratum |

**Two traps in that table.**

- Rule 4 has a **gap**: a shallow cell with `12 <= sst <= 20` matches neither
  reef nor kelp and falls through to rules 5–6. Do not "tidy" this into an
  if/else — the gap is the current behaviour and the fixture will catch you.
- Rule 2 fires **before** rule 3, so a cell that is both a deep trench and a
  ridge is `HadalTrench` today, not `HydrothermalVent`. Preserve the order.

Depth → stratum, used wherever the table says "by depth":

```
< 200 Epipelagic | < 1000 Mesopelagic | < 4000 Bathypelagic | < 6000 Abyssal | else Hadal
```

Land is simpler: every land cell is `(Overworld, <the existing land Biome as a
Formation>, Surface)`, and projects back to itself.

---

### Task 1: The facet types

**Files:**
- Create: `domains/climate/src/facets.rs`
- Modify: `domains/climate/src/lib.rs` (add `mod facets;` + re-exports)
- Test: `domains/climate/src/facets.rs` (inline `mod tests`)

**Interfaces:**
- Consumes: nothing.
- Produces: `Medium`, `Access`, `Realm`, `Stratum`, `Formation`, and
  `Realm::OVERWORLD` / `Realm::WATERWORLD`.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_realm_is_a_triple_not_a_flag() {
        // The Stratum §3.4: realms are values, so a later sky realm or plane
        // is a new value rather than a new axis. This test exists to fail if
        // someone collapses Realm back into a two-valued enum.
        assert_eq!(Realm::OVERWORLD.medium, Medium::AirOverRock);
        assert_eq!(Realm::OVERWORLD.access, Access::Default);
        assert_eq!(Realm::WATERWORLD.medium, Medium::Water);
        assert_eq!(Realm::WATERWORLD.access, Access::Dive);
        assert_ne!(Realm::OVERWORLD, Realm::WATERWORLD);
    }

    #[test]
    fn the_overworld_has_one_stratum_and_the_waterworld_a_column() {
        assert_eq!(Realm::OVERWORLD.strata(), &[Stratum::Surface]);
        assert_eq!(Realm::WATERWORLD.strata().len(), 5);
        assert_eq!(Realm::WATERWORLD.strata()[0], Stratum::Epipelagic);
        assert_eq!(Realm::WATERWORLD.strata()[4], Stratum::Hadal);
    }

    #[test]
    fn depth_maps_to_the_documented_stratum_bands() {
        assert_eq!(Stratum::at_depth_m(0.0), Stratum::Epipelagic);
        assert_eq!(Stratum::at_depth_m(199.9), Stratum::Epipelagic);
        assert_eq!(Stratum::at_depth_m(200.0), Stratum::Mesopelagic);
        assert_eq!(Stratum::at_depth_m(999.9), Stratum::Mesopelagic);
        assert_eq!(Stratum::at_depth_m(1000.0), Stratum::Bathypelagic);
        assert_eq!(Stratum::at_depth_m(3999.9), Stratum::Bathypelagic);
        assert_eq!(Stratum::at_depth_m(4000.0), Stratum::Abyssal);
        assert_eq!(Stratum::at_depth_m(5999.9), Stratum::Abyssal);
        assert_eq!(Stratum::at_depth_m(6000.0), Stratum::Hadal);
    }
}
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-climate facets`
Expected: FAIL — `file not found for module facets` / unresolved names.

- [ ] **Step 3: Implement**

Create `domains/climate/src/facets.rs`:

```rust
//! The biome facets (The Stratum §3): a room's biome is a faceted expression
//! — `realm : formation : stratum` — not a single enum value. [`crate::Biome`]
//! remains as the *projection* of an expression, so every existing consumer
//! is unaffected.

/// What fills a realm. A realm is `(medium, access, strata)`, never an
/// enumerated world, so a later sky realm — or an elemental plane — is a new
/// value rather than a new axis (The Stratum §3.4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Medium {
    /// Air over walkable ground — the overworld.
    AirOverRock,
    /// Salt or fresh water — the sea column.
    Water,
}

/// How a realm is reached. This — not materiality — is what separates the
/// world's own column (continuous movement with a medium change) from a plane
/// (transit). See The Stratum §3.4.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access {
    /// Simply being there; the default band.
    Default,
    /// Entered by descending through water.
    Dive,
}

/// A realm: a medium, the way in, and the column of strata it holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Realm {
    /// What fills it.
    pub medium: Medium,
    /// How it is reached.
    pub access: Access,
}

impl Realm {
    /// The surface world.
    pub const OVERWORLD: Realm = Realm {
        medium: Medium::AirOverRock,
        access: Access::Default,
    };
    /// The sea column.
    pub const WATERWORLD: Realm = Realm {
        medium: Medium::Water,
        access: Access::Dive,
    };

    /// The strata this realm holds, shallowest first.
    pub fn strata(&self) -> &'static [Stratum] {
        match self.medium {
            Medium::AirOverRock => &[Stratum::Surface],
            Medium::Water => &[
                Stratum::Epipelagic,
                Stratum::Mesopelagic,
                Stratum::Bathypelagic,
                Stratum::Abyssal,
                Stratum::Hadal,
            ],
        }
    }
}

/// A position within a realm's column. Realm-relative by construction: the
/// pelagic zones and (later) the underworld's geological layers are the same
/// construct at different realms.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stratum {
    /// The overworld's only stratum.
    Surface,
    /// Sunlit water, above 200 m.
    Epipelagic,
    /// Twilight water, 200–1000 m.
    Mesopelagic,
    /// Lightless water, 1000–4000 m.
    Bathypelagic,
    /// The abyss, 4000–6000 m.
    Abyssal,
    /// Trench depths, below 6000 m.
    Hadal,
}

impl Stratum {
    /// The stratum at a depth below the surface, in metres. The bands are the
    /// ones `classify_marine` has always used; they move here unchanged.
    /// type-audit: bare-ok(diagnostic-value: depth_m)
    pub fn at_depth_m(depth_m: f64) -> Stratum {
        if depth_m < 200.0 {
            Stratum::Epipelagic
        } else if depth_m < 1000.0 {
            Stratum::Mesopelagic
        } else if depth_m < 4000.0 {
            Stratum::Bathypelagic
        } else if depth_m < 6000.0 {
            Stratum::Abyssal
        } else {
            Stratum::Hadal
        }
    }
}

/// A community type — what *lives* here, independent of how deep it is. This
/// is the half of the old `Biome` enum that is genuinely a biome; the other
/// half was [`Stratum`] wearing the same coat.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Formation {
    /// Permanent land ice.
    Ice,
    /// Treeless cold ground.
    Tundra,
    /// Boreal coniferous forest.
    Taiga,
    /// Temperate grassland / steppe.
    TemperateGrassland,
    /// Dry temperate shrubland.
    Shrubland,
    /// Temperate broadleaf forest.
    TemperateForest,
    /// Wet temperate rainforest.
    TemperateRainforest,
    /// Hot desert.
    Desert,
    /// Tropical grassland with scattered trees.
    Savanna,
    /// Tropical forest with a dry season.
    TropicalSeasonalForest,
    /// Wet tropical rainforest.
    TropicalRainforest,
    /// High cold ground above the tree line.
    Alpine,
    /// Frozen sea surface.
    SeaIce,
    /// Warm shallow coral.
    Reef,
    /// Cold shallow kelp.
    KelpForest,
    /// A hydrothermal vent community.
    Vent,
    /// A nutrient-rich upwelling.
    Upwelling,
    /// Open sea with no distinguishing community — the marine default.
    OpenWater,
}
```

Add to `domains/climate/src/lib.rs`, beside the existing `mod biome;`:

```rust
mod facets;
pub use facets::{Access, Formation, Medium, Realm, Stratum};
```

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-climate facets`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
cargo test -p hornvale-climate
git add domains/climate/
git commit -m "feat(climate): the biome facets — realm, formation, stratum

Realm is a triple (medium, access) with its own column of strata, so a later
sky realm or plane is a new value rather than a new axis. Nothing consumes
these yet."
```

---

### Task 2: The expression and its projection

**Files:**
- Modify: `domains/climate/src/facets.rs` (add `BiomeExpr` + `biome()`)
- Test: `domains/climate/src/facets.rs`

**Interfaces:**
- Consumes: Task 1's types.
- Produces: `BiomeExpr { realm, formation, stratum }` and
  `BiomeExpr::biome(&self) -> Biome` — **the compatibility seam.**

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn every_legacy_biome_is_the_projection_of_some_expression() {
        // The projection must be ONTO: if a legacy Biome had no expression
        // that produces it, the refactor would silently drop a classification.
        for b in Biome::catalog() {
            assert!(
                BiomeExpr::for_legacy(*b).biome() == *b,
                "no expression projects back to {b:?}"
            );
        }
    }

    #[test]
    fn open_water_projects_by_its_stratum() {
        let ow = |s| BiomeExpr {
            realm: Realm::WATERWORLD,
            formation: Formation::OpenWater,
            stratum: s,
        };
        assert_eq!(ow(Stratum::Epipelagic).biome(), Biome::Epipelagic);
        assert_eq!(ow(Stratum::Mesopelagic).biome(), Biome::Mesopelagic);
        assert_eq!(ow(Stratum::Bathypelagic).biome(), Biome::Bathypelagic);
        assert_eq!(ow(Stratum::Abyssal).biome(), Biome::Abyssal);
        assert_eq!(ow(Stratum::Hadal).biome(), Biome::HadalTrench);
    }

    #[test]
    fn a_vent_keeps_its_identity_at_every_depth() {
        // The point of the split: a vent IS abyssal, rather than being a
        // community that had to displace a stratum to exist.
        for s in Realm::WATERWORLD.strata() {
            let e = BiomeExpr {
                realm: Realm::WATERWORLD,
                formation: Formation::Vent,
                stratum: *s,
            };
            assert_eq!(e.biome(), Biome::HydrothermalVent);
        }
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-climate facets`
Expected: FAIL — `BiomeExpr` not found.

- [ ] **Step 3: Implement**

```rust
/// A room's biome as a faceted expression. This is the truth; [`crate::Biome`]
/// is its projection, kept so every existing consumer is unaffected.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BiomeExpr {
    /// Which world.
    pub realm: Realm,
    /// Which community.
    pub formation: Formation,
    /// Where in the realm's column.
    pub stratum: Stratum,
}

impl BiomeExpr {
    /// The legacy [`crate::Biome`] this expression projects to.
    ///
    /// **This function is the campaign's byte-identity guarantee.** Every
    /// consumer still calls `biome_at()`, so as long as this reproduces what
    /// `classify` used to return, nothing downstream can tell the taxonomy was
    /// disentangled. `HadalTrench` is the tell: in the legacy enum it was a
    /// "biome", but it is really open water at hadal depth, which is why it
    /// falls out of the `OpenWater` arm rather than having a formation of its
    /// own.
    pub fn biome(&self) -> Biome {
        match self.formation {
            Formation::Ice => Biome::Ice,
            Formation::Tundra => Biome::Tundra,
            Formation::Taiga => Biome::Taiga,
            Formation::TemperateGrassland => Biome::TemperateGrassland,
            Formation::Shrubland => Biome::Shrubland,
            Formation::TemperateForest => Biome::TemperateForest,
            Formation::TemperateRainforest => Biome::TemperateRainforest,
            Formation::Desert => Biome::Desert,
            Formation::Savanna => Biome::Savanna,
            Formation::TropicalSeasonalForest => Biome::TropicalSeasonalForest,
            Formation::TropicalRainforest => Biome::TropicalRainforest,
            Formation::Alpine => Biome::Alpine,
            Formation::SeaIce => Biome::SeaIce,
            Formation::Reef => Biome::CoralReef,
            Formation::KelpForest => Biome::KelpForest,
            Formation::Vent => Biome::HydrothermalVent,
            Formation::Upwelling => Biome::Upwelling,
            Formation::OpenWater => match self.stratum {
                Stratum::Hadal => Biome::HadalTrench,
                Stratum::Abyssal => Biome::Abyssal,
                Stratum::Bathypelagic => Biome::Bathypelagic,
                Stratum::Mesopelagic => Biome::Mesopelagic,
                // Surface is unreachable for OpenWater in practice; treat it
                // as the shallowest water rather than inventing a new Biome.
                Stratum::Epipelagic | Stratum::Surface => Biome::Epipelagic,
            },
        }
    }

    /// The canonical expression for a legacy [`crate::Biome`] — the inverse of
    /// [`BiomeExpr::biome`], used to prove the projection is onto. Marine
    /// formations take the shallowest stratum that yields them.
    pub fn for_legacy(b: Biome) -> BiomeExpr {
        let (realm, formation, stratum) = match b {
            Biome::Ice => (Realm::OVERWORLD, Formation::Ice, Stratum::Surface),
            Biome::Tundra => (Realm::OVERWORLD, Formation::Tundra, Stratum::Surface),
            Biome::Taiga => (Realm::OVERWORLD, Formation::Taiga, Stratum::Surface),
            Biome::TemperateGrassland => (
                Realm::OVERWORLD,
                Formation::TemperateGrassland,
                Stratum::Surface,
            ),
            Biome::Shrubland => (Realm::OVERWORLD, Formation::Shrubland, Stratum::Surface),
            Biome::TemperateForest => (
                Realm::OVERWORLD,
                Formation::TemperateForest,
                Stratum::Surface,
            ),
            Biome::TemperateRainforest => (
                Realm::OVERWORLD,
                Formation::TemperateRainforest,
                Stratum::Surface,
            ),
            Biome::Desert => (Realm::OVERWORLD, Formation::Desert, Stratum::Surface),
            Biome::Savanna => (Realm::OVERWORLD, Formation::Savanna, Stratum::Surface),
            Biome::TropicalSeasonalForest => (
                Realm::OVERWORLD,
                Formation::TropicalSeasonalForest,
                Stratum::Surface,
            ),
            Biome::TropicalRainforest => (
                Realm::OVERWORLD,
                Formation::TropicalRainforest,
                Stratum::Surface,
            ),
            Biome::Alpine => (Realm::OVERWORLD, Formation::Alpine, Stratum::Surface),
            Biome::SeaIce => (Realm::WATERWORLD, Formation::SeaIce, Stratum::Epipelagic),
            Biome::CoralReef => (Realm::WATERWORLD, Formation::Reef, Stratum::Epipelagic),
            Biome::KelpForest => (
                Realm::WATERWORLD,
                Formation::KelpForest,
                Stratum::Epipelagic,
            ),
            Biome::HydrothermalVent => (Realm::WATERWORLD, Formation::Vent, Stratum::Abyssal),
            Biome::Upwelling => (
                Realm::WATERWORLD,
                Formation::Upwelling,
                Stratum::Epipelagic,
            ),
            Biome::Epipelagic => (
                Realm::WATERWORLD,
                Formation::OpenWater,
                Stratum::Epipelagic,
            ),
            Biome::Mesopelagic => (
                Realm::WATERWORLD,
                Formation::OpenWater,
                Stratum::Mesopelagic,
            ),
            Biome::Bathypelagic => (
                Realm::WATERWORLD,
                Formation::OpenWater,
                Stratum::Bathypelagic,
            ),
            Biome::Abyssal => (Realm::WATERWORLD, Formation::OpenWater, Stratum::Abyssal),
            Biome::HadalTrench => (Realm::WATERWORLD, Formation::OpenWater, Stratum::Hadal),
        };
        BiomeExpr {
            realm,
            formation,
            stratum,
        }
    }
}
```

Add `BiomeExpr` to the `pub use facets::{…}` list, and `use crate::Biome;` at
the top of `facets.rs`.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-climate`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/climate/
git commit -m "feat(climate): BiomeExpr, and Biome as its projection

The projection is the campaign's byte-identity guarantee: consumers keep
calling biome_at() and keep receiving what they receive today. HadalTrench is
the tell — it was never a community, only open water at hadal depth."
```

---

### Task 3: Classify into expressions

**Files:**
- Modify: `domains/climate/src/biome.rs` (`classify_marine`, `classify`)
- Test: `domains/climate/src/biome.rs`

**Interfaces:**
- Consumes: `BiomeExpr` (Task 2).
- Produces: `classify_marine_expr(...) -> BiomeExpr`,
  `classify_expr(...) -> BiomeExpr`. The existing `classify_marine` and
  `classify` keep their signatures and delegate through `.biome()`.

- [ ] **Step 1: Write the failing test**

The equivalence sweep is the whole point of this task — it is what proves the
refactor moved nothing:

```rust
    #[test]
    fn the_expression_path_reproduces_legacy_marine_classification_exactly() {
        // A dense sweep across every branch of the legacy precedence chain,
        // including the 12..=20 °C shallow gap that matches neither reef nor
        // kelp, and the trench-and-ridge overlap where trench wins.
        let features = [
            SeafloorFeature::None,
            SeafloorFeature::Trench,
            SeafloorFeature::Ridge,
        ];
        let depths = [
            0.0, 50.0, 199.0, 200.0, 500.0, 999.0, 1000.0, 3999.0, 4000.0, 5999.0, 6000.0, 9000.0,
        ];
        let ssts = [-5.0, 0.0, 5.0, 11.9, 12.0, 15.0, 20.0, 20.1, 30.0];
        let mut checked = 0usize;
        for f in features {
            for d in depths {
                for s in ssts {
                    for up in [false, true] {
                        let sst = t(s);
                        let legacy = classify_marine(d, sst, f, up);
                        let viaexpr = classify_marine_expr(d, sst, f, up).biome();
                        assert_eq!(
                            legacy, viaexpr,
                            "depth {d} sst {s} feature {f:?} upwelling {up}"
                        );
                        checked += 1;
                    }
                }
            }
        }
        assert!(checked > 600, "sweep must be dense; checked {checked}");
    }

    #[test]
    fn a_trench_outranks_a_ridge_exactly_as_it_did() {
        // Rule 2 fires before rule 3: a deep trench that is also a ridge is
        // hadal open water, not a vent. Preserved deliberately.
        let deep = classify_marine_expr(9000.0, t(4.0), SeafloorFeature::Trench, false);
        assert_eq!(deep.formation, Formation::OpenWater);
        assert_eq!(deep.stratum, Stratum::Hadal);
        assert_eq!(deep.biome(), Biome::HadalTrench);
    }

    #[test]
    fn a_vent_is_now_a_community_at_a_depth() {
        // The disentangling, visible: the vent keeps its stratum instead of
        // displacing it.
        let e = classify_marine_expr(3000.0, t(4.0), SeafloorFeature::Ridge, false);
        assert_eq!(e.formation, Formation::Vent);
        assert_eq!(e.stratum, Stratum::Bathypelagic);
        assert_eq!(e.biome(), Biome::HydrothermalVent);
    }
```

> `t(..)` is the existing `Temperature` helper in this module's tests; reuse
> it rather than writing another.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-climate biome`
Expected: FAIL — `classify_marine_expr` not found.

- [ ] **Step 3: Implement**

Add beside `classify_marine`, preserving the legacy order exactly:

```rust
/// [`classify_marine`] as a faceted expression. The legacy function delegates
/// to this, so the two cannot drift.
///
/// The precedence chain below is the legacy one, deliberately unchanged: a
/// deep trench outranks a ridge, and the shallow 12–20 °C band matches neither
/// reef nor kelp and falls through. Both are current behaviour, and the
/// seed-42 world fixture will catch any "tidying" of either.
/// type-audit: bare-ok(diagnostic-value: depth_m), bare-ok(flag: upwelling)
pub fn classify_marine_expr(
    depth_m: f64,
    sst_c: Temperature,
    feature: SeafloorFeature,
    upwelling: bool,
) -> BiomeExpr {
    let stratum = Stratum::at_depth_m(depth_m);
    let sea_ice_c = Temperature::new(SEA_ICE_C).expect("sea-ice threshold is finite");
    let formation = if sst_c < sea_ice_c {
        Formation::SeaIce
    } else if feature == SeafloorFeature::Trench && depth_m > 6000.0 {
        Formation::OpenWater
    } else if feature == SeafloorFeature::Ridge {
        Formation::Vent
    } else if depth_m < 200.0 && sst_c > Temperature::new(20.0).expect("reef threshold is finite") {
        Formation::Reef
    } else if depth_m < 200.0 && sst_c < Temperature::new(12.0).expect("kelp threshold is finite") {
        Formation::KelpForest
    } else if upwelling && depth_m < 1000.0 {
        Formation::Upwelling
    } else {
        Formation::OpenWater
    };
    BiomeExpr {
        realm: Realm::WATERWORLD,
        formation,
        stratum,
    }
}
```

Then reduce the legacy function to a delegation:

```rust
pub fn classify_marine(
    depth_m: f64,
    sst_c: Temperature,
    feature: SeafloorFeature,
    upwelling: bool,
) -> Biome {
    classify_marine_expr(depth_m, sst_c, feature, upwelling).biome()
}
```

Then the same for `classify`. It already decides marine-vs-land by
`elevation_m < sea_level_m`; keep that branch exactly and wrap each arm:

```rust
/// [`classify`] as a faceted expression; the legacy function delegates here.
/// type-audit: bare-ok(ratio: moisture), pending(wave-2: latitude_deg), bare-ok(flag: upwelling)
#[allow(clippy::too_many_arguments)]
pub fn classify_expr(
    temp_c: Temperature,
    moisture: f64,
    sst_c: Temperature,
    elevation_m: ReferenceElevation,
    sea_level_m: ReferenceElevation,
    latitude_deg: f64,
    feature: SeafloorFeature,
    upwelling: bool,
) -> BiomeExpr {
    // The marine/land split, the depth derivation, and the land lookup are
    // the legacy body verbatim — only the return type changes.
    if elevation_m < sea_level_m {
        let depth_m = sea_level_m.get() - elevation_m.get();
        return classify_marine_expr(depth_m, sst_c, feature, upwelling);
    }
    let land = classify_land(temp_c, moisture, elevation_m, latitude_deg);
    BiomeExpr {
        realm: Realm::OVERWORLD,
        formation: land_formation(land),
        stratum: Stratum::Surface,
    }
}
```

> Take the marine/land condition, the depth expression, and the land-lookup
> call from the CURRENT body of `classify` rather than the sketch above — the
> names here are indicative, and the sweep in Step 1 only covers the marine
> half. Whatever `classify` does today for land, `classify_expr` must do
> identically.

`land_formation(Biome) -> Formation` is the twelve-arm land mapping. Put it in
`facets.rs` next to `for_legacy` and have `for_legacy` call it, so the mapping
is written once. Finally reduce `classify` to `classify_expr(..).biome()`.

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-climate`
Expected: PASS, including the >600-case sweep.

- [ ] **Step 5: Prove no world moved**

```bash
cargo test -p hornvale --test lens_purity seed_42_world_json_matches_the_committed_fixture
```

Expected: PASS. **If this fails, the projection is wrong — fix the projection,
never the fixture.**

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/climate/
git commit -m "feat(climate): classify into expressions; legacy delegates

classify_marine loses nothing and gains a stratum: a vent is now a community
AT a depth rather than a community that displaced one. Proven by a >600-case
sweep asserting the expression path reproduces legacy classification exactly,
including the 12-20C shallow gap and the trench-outranks-ridge overlap."
```

---

### Task 4: Expose the expression to consumers

**Files:**
- Modify: `domains/climate/src/provider.rs` (add `biome_expr_at`, `biome_expr_map`)
- Test: `domains/climate/src/provider.rs`

**Interfaces:**
- Consumes: Tasks 1–3.
- Produces: `GeneratedClimate::biome_expr_at(cell) -> BiomeExpr`.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn the_expression_and_the_legacy_biome_agree_at_every_cell() {
        let c = test_climate();
        for cell in c.geosphere().cells() {
            assert_eq!(
                c.biome_expr_at(cell).biome(),
                c.biome_at(cell),
                "cell {cell:?} disagrees between the faceted and legacy views"
            );
        }
    }

    #[test]
    fn marine_cells_are_in_the_waterworld_and_land_cells_are_not() {
        let c = test_climate();
        for cell in c.geosphere().cells() {
            let e = c.biome_expr_at(cell);
            assert_eq!(
                e.realm == Realm::WATERWORLD,
                c.biome_at(cell).is_marine(),
                "realm disagrees with is_marine() at {cell:?}"
            );
            if e.realm == Realm::OVERWORLD {
                assert_eq!(e.stratum, Stratum::Surface);
            }
        }
    }
```

> Use whatever climate fixture `provider.rs`'s existing tests build; do not add
> a new one.

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-climate provider`
Expected: FAIL — `no method named biome_expr_at`.

- [ ] **Step 3: Implement**

Store the expression map where `biome_map` is built and derive the legacy map
from it, so the two cannot diverge — `biome_at` becomes
`self.biome_expr_at(cell).biome()` if that is a cheap lookup, or keep both
maps if `biome_map` is on a hot path (check before choosing; the census reads
it per cell).

- [ ] **Step 4: Run to verify passing**

Run: `cargo test -p hornvale-climate`
Expected: PASS.

- [ ] **Step 5: Re-prove no world moved, and check the cost**

```bash
cargo test -p hornvale --test lens_purity seed_42_world_json_matches_the_committed_fixture
cargo build --release -p hornvale
time ./target/release/hornvale new --seed 42 --out /tmp/hv-fmt.json
```

Expected: PASS, and generation time within noise of the pre-campaign ~1.9 s.
Record the measured figure in the commit message. If it regressed materially,
keep both maps rather than deriving per call.

- [ ] **Step 6: Commit**

```bash
cargo fmt
git add domains/climate/
git commit -m "feat(climate): biome_expr_at — the faceted view, per cell"
```

---

### Task 5: Retire the stale doc comment and the parallel classification

**Files:**
- Modify: `domains/climate/src/biome.rs` (the `Biome` doc comment)
- Modify: `windows/worldgen/src/lib.rs:490` (`biome_class`)
- Test: `windows/worldgen/src/lib.rs` (inline tests)

**Why:** `Biome`'s doc comment reads "A biome class — terrestrial or marine",
describing something it is not. And `worldgen::biome_class` maps `Biome ->
culture::BiomeClass` — a formation-group facet, hand-rolled at the composition
root because no principled tier existed (The Stratum §2). It should key off
`Formation`, which *is* that tier.

**Layering note:** the mapping lives in `windows/worldgen` and must stay there.
`domains/culture` may not depend on `domains/climate`; the composition root is
what bridges them. Do not move it into either domain.

**Care:** `BiomeClass` feeds `culture::fertility()` and therefore subsistence,
the flagship-surplus metrics, and the calibration battery. **The mapping must
be exactly value-preserving.** This task re-keys the *source*, never the
*result*.

- [ ] **Step 1: Write the failing test**

```rust
    #[test]
    fn the_formation_keyed_class_matches_the_biome_keyed_one_for_every_biome() {
        // Value-preserving by construction is not enough; prove it across the
        // whole catalog, because fertility feeds the calibration battery.
        for b in hornvale_climate::Biome::catalog() {
            let formation = hornvale_climate::BiomeExpr::for_legacy(*b).formation;
            assert_eq!(
                biome_class_of_formation(formation),
                biome_class(*b),
                "{b:?} changes fertility class under the formation mapping"
            );
        }
    }
```

- [ ] **Step 2: Run to verify failure**

Run: `cargo test -p hornvale-worldgen biome_class`
Expected: FAIL — `biome_class_of_formation` not found.

- [ ] **Step 3: Implement**

Add `biome_class_of_formation(Formation) -> BiomeClass` beside `biome_class`,
mirroring the existing arms exactly — `Reef`/`KelpForest`/`Vent`/`Upwelling`/
`SeaIce`/`OpenWater` all land on `BiomeClass::Barren`, as every marine `Biome`
does today. Then make `biome_class` delegate:

```rust
pub fn biome_class(biome: hornvale_climate::Biome) -> hornvale_culture::BiomeClass {
    biome_class_of_formation(hornvale_climate::BiomeExpr::for_legacy(biome).formation)
}
```

Fix the doc comment in `domains/climate/src/biome.rs`:

```rust
/// A biome, as the pre-facet taxonomy knew it: community types and depth
/// strata in one list. Retained as the **projection** of a [`crate::BiomeExpr`]
/// so every existing consumer is unaffected — see The Stratum §3.
```

> **Do not touch** the parallel string-keyed mapping in
> `windows/lab/tests/calibration.rs:121`. It is a third copy of this taxonomy
> and it keys off kebab-case *names*, so it is unaffected by this task.
> Note it in the retrospective as remaining debt; unifying it is its own change
> and would touch a preregistered battery.

- [ ] **Step 4: Run to verify passing**

Run: `cargo nextest run -p hornvale-worldgen -p hornvale-climate -p hornvale-culture`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add domains/ windows/worldgen/
git commit -m "refactor(worldgen): key the fertility class off Formation

biome_class was a formation-group facet hand-rolled at the composition root
because no principled tier existed. It now keys off the tier. Value-preserving
across the whole catalog by test — this re-keys the source, never the result.

Also corrects Biome's doc comment, which described a terrestrial/marine split
the enum never had."
```

---

### Task 6: Close

**Files:**
- Create: `book/src/chronicle/the-formations.md` (+ `book/src/SUMMARY.md`)
- Create: `docs/retrospectives/the-formations.md`
- Modify: `book/src/frontier/idea-registry.md`, `docs/audits/type-audit-report.md`

- [ ] **Step 1: Absorb main**

```bash
make preflight
```

On NO-GO, merge `origin/main` INTO the branch and re-run the gate there.

- [ ] **Step 2: Regenerate and confirm NOTHING drifted**

```bash
bash scripts/regenerate-artifacts.sh
git status --porcelain
```

Expected: **empty**, except the type-audit report. This campaign is a pure
refactor; any moved gallery artifact means the projection is wrong. Do not
accept drift here.

- [ ] **Step 3: Full gate**

```bash
make gate
```

- [ ] **Step 4: Chronicle, retrospective, registry**

- Chronicle: the through-line is that `HadalTrench` was never a community —
  the enum had been carrying a depth stratum in a list of biomes, and
  `classify_marine`'s precedence chain was the symptom.
- Retrospective: process lessons only. Include whether the projection-first
  approach (make the old type a projection of the new one) is worth reusing —
  it is the reason a nine-crate taxonomy change moved zero bytes.
- Registry: flip the relevant Stratum rows to reflect campaign 1 shipping;
  repoint **Where** at the new chronicle. Never delete a row.

- [ ] **Step 5: Commit**

```bash
cargo fmt
git add -A
git commit -m "docs(the-formations): close — chronicle, retro, registry"
```

---

## Notes for the executor

- **The fixture is the referee.** `seed_42_world_json_matches_the_committed_fixture`
  must stay green from Task 3 onward. It is never rebaselined in this campaign.
- **Tasks 1–2 are pure addition** and cannot break anything; Task 3 is where
  behaviour could move; Tasks 4–5 are wiring.
- **If a task seems to need a new concept, an epoch, or a census regen, STOP.**
  Those belong to campaign 2 (The Variants).
