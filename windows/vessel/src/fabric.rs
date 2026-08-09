//! What a built cell is made of, and what it reflects (The Lantern, spec §3).
//!
//! **Derived, never drawn.** A fabric is a projection of world-state that
//! already ships — the lithology buffer under the settlement, the biome it
//! sits in — so this module owns no seed label, no `streams.rs` constant and
//! no epoch. A window that draws has become a domain.
//!
//! The load-bearing half is [`reflectance_of`] for [`Fabric::Stone`]: it is
//! **derived from the local bedrock** through the same
//! `lithology::reflectance` mixture the ground itself uses, so a village on
//! granite and one on basalt are different buildings rather than the same
//! grey. That is the campaign's H1, and it is measured on real terrain in
//! `windows/vessel/tests/lantern_fabric.rs` — not on an authored buffer,
//! because an authored buffer answers a different question.
//!
//! The other three fabrics are authored constants. They are declared
//! approximations in exactly the sense `lithology`'s own `endmembers` are:
//! the claims rest on the *relations* between them, not on laboratory
//! accuracy.
//!
//! ## Which cell a built place is made of
//!
//! Nothing here resolves a cell — a caller hands one in, and there is one
//! rule for doing so, shared with the biome:
//!
//! - a **settlement** carries its own `hornvale_settlement::CELL_ID` fact,
//!   and that is exactly the cell the composition root read
//!   `climate.biome_at` at when it committed the settlement's biome, so the
//!   fact *is* the shared resolution;
//! - a **room** resolves through `hornvale_locale`'s `dominant_corner`
//!   (greatest blend weight, tie-broken to the lowest `CellId`) — the same
//!   rule `LocaleContext::describe` takes biome, water and substrate from,
//!   and the same one `LocaleContext::reflectance_at` already takes the
//!   ground's rock from.
//!
//! Fabric must not invent a third. A caption and a picture that disagree
//! about which ground a place stands on is the failure this note exists to
//! prevent.

use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::CellId;
use hornvale_kernel::color::{BANDS, Reflectance};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::lithology::{MaterialBuffer, RockClass};

use crate::lattice::CellKind;

/// What a built cell is made of. **Derived, never drawn** — from lithology,
/// biome and climate, all of which already ship. No seed label, no epoch.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Fabric {
    /// Vernacular stone. Its reflectance is DERIVED from the local bedrock,
    /// so a village on granite and one on basalt are visibly different.
    Stone,
    /// Timber: a forested, temperate place.
    Timber,
    /// Cob or brick: deep soil, dry climate.
    Cob,
    /// Thatch: grassland and wet — roofs and floors only.
    Thatch,
}

/// The ground a built cell stands on, as the fabric rules read it.
///
/// The two categorical fields (`rock`, `material`) are the *same* pair
/// `hornvale_locale::LocaleContext::reflectance_at` reads, and for the same
/// reason: rock class is categorical and averaging granite with basalt
/// would name a rock that is not there.
///
/// The four flags are re-projections of the **biome and the buffer**, not
/// new thresholds over temperature and rainfall — see [`FabricContext::at`].
/// type-audit: bare-ok(flag: forested), bare-ok(flag: temperate), bare-ok(flag: deep_soil), bare-ok(flag: dry)
#[derive(Debug, Clone, PartialEq)]
pub struct FabricContext {
    /// The rock class beneath the cell.
    pub rock: RockClass,
    /// The petrogenetic buffer beneath the cell — what [`Fabric::Stone`]'s
    /// reflectance is derived from.
    pub material: MaterialBuffer,
    /// Is there timber to build with?
    pub forested: bool,
    /// Is the climate the temperate band timber framing belongs to?
    pub temperate: bool,
    /// Is there enough regolith to dig cob or brick earth from?
    pub deep_soil: bool,
    /// Is the climate dry enough for unfired earth to stand?
    pub dry: bool,
}

/// Regolith thickness (metres) above which a place can dig its walls out of
/// the ground. **Authored, not measured** — a metre of subsoil is the depth
/// a cob wall's earth is won from, and no claim in this campaign rests on
/// it: [`Fabric::Stone`]'s reflectance, which is what H1 measures, ignores
/// every flag on [`FabricContext`].
/// type-audit: bare-ok(ratio)
pub const DEEP_SOIL_MIN_M: f64 = 1.0;

impl FabricContext {
    /// Read the ground at `cell`.
    ///
    /// **The four flags are read off the biome**, which is already a
    /// classification of temperature × moisture
    /// (`hornvale_climate::classify_land`). Re-deriving them from raw
    /// temperature and rainfall would author a second set of thresholds
    /// that could disagree with the biome the same place is *described* as
    /// — the same reason `lithology::is_iron_rich` mirrors `appearance`'s
    /// own hue match rather than re-reading the buffer. `deep_soil` is the
    /// exception and comes off the buffer, because the biome carries no
    /// regolith axis at all.
    ///
    /// **No land-only shortcut.** Settlements can be marine — founded on
    /// land that later drowned — so this reads whatever cell it is given,
    /// and `rock_at` answers for an ocean cell as readily as a land one.
    pub fn at(terrain: &GeneratedTerrain, climate: &GeneratedClimate, cell: CellId) -> Self {
        let biome = climate.biome_at(cell);
        let material = terrain.material_at(cell);
        FabricContext {
            rock: terrain.rock_at(cell),
            deep_soil: material.soil_depth.get() >= DEEP_SOIL_MIN_M,
            material,
            forested: is_forested(biome),
            temperate: is_temperate(biome),
            dry: is_dry(biome),
        }
    }
}

/// Biomes with standing timber. Mirrors `classify_land`'s own forested arms
/// (the wetter half of each temperature band) so the fabric and the prose
/// cannot disagree about whether a place has trees.
fn is_forested(biome: Biome) -> bool {
    matches!(
        biome,
        Biome::Taiga
            | Biome::TemperateForest
            | Biome::TemperateRainforest
            | Biome::TropicalSeasonalForest
            | Biome::TropicalRainforest
    )
}

/// The temperate band, as `classify_land` cuts it: the four biomes it
/// returns between its freeze/taiga floor and its 20 °C hot ceiling.
fn is_temperate(biome: Biome) -> bool {
    matches!(
        biome,
        Biome::TemperateGrassland
            | Biome::Shrubland
            | Biome::TemperateForest
            | Biome::TemperateRainforest
    )
}

/// The dry arms of `classify_land`: the driest biome each temperature band
/// resolves to, plus savanna's seasonal drought.
fn is_dry(biome: Biome) -> bool {
    matches!(
        biome,
        Biome::Desert
            | Biome::Shrubland
            | Biome::TemperateGrassland
            | Biome::Savanna
            | Biome::Tundra
    )
}

/// Rock classes too weak to stand as a wall. The complement is building
/// stone. Categorical rather than a threshold on `induration`, because
/// `classify_rock` has already spent that axis — a second, independently
/// chosen cut-off could put a cell's rock name and its wall in
/// disagreement.
fn is_weak(rock: RockClass) -> bool {
    matches!(
        rock,
        RockClass::Shale | RockClass::Coal | RockClass::Alluvium | RockClass::Evaporite
    )
}

/// What a built cell is made of, or `None` where the question does not
/// apply.
///
/// `None` for a [`CellKind::Threshold`]: an opening is not a fabric, which
/// The Beholding already established for the palette.
///
/// The wall list and the floor list are the same four in a different order.
/// A wall falls back to stone — you can always pile up what is underfoot —
/// while a floor falls back to thatch, the rush covering the spec restricts
/// to roofs and floors.
///
/// "Competent **and near**" (spec §3) collapses to competence alone at this
/// resolution: the context is the containing cell's own ground, which is
/// the nearest source of stone there is.
pub fn fabric_of(kind: CellKind, ctx: &FabricContext) -> Option<Fabric> {
    match kind {
        CellKind::Threshold(_, _) => None,
        CellKind::Wall => Some(if !is_weak(ctx.rock) {
            Fabric::Stone
        } else if ctx.forested && ctx.temperate {
            Fabric::Timber
        } else if ctx.deep_soil && ctx.dry {
            Fabric::Cob
        } else {
            Fabric::Stone
        }),
        CellKind::Floor(_) => Some(if ctx.forested && ctx.temperate {
            Fabric::Timber
        } else if !is_weak(ctx.rock) {
            Fabric::Stone
        } else if ctx.deep_soil && ctx.dry {
            Fabric::Cob
        } else {
            Fabric::Thatch
        }),
    }
}

/// Authored reflectance curves for the three fabrics that are not the
/// ground. On the kernel's ten-band grid (centres 360–720 nm), and declared
/// approximations in the same sense `lithology`'s `endmembers` are: what
/// matters is that timber is darker and warmer than cob, and thatch paler
/// and more golden than either.
mod authored {
    use hornvale_kernel::color::BANDS;

    /// Weathered oak and pine — dark, strongly warm.
    /// type-audit: bare-ok(ratio)
    pub const TIMBER: [f64; BANDS] = [0.06, 0.08, 0.10, 0.13, 0.18, 0.24, 0.30, 0.34, 0.37, 0.38];
    /// Earth and straw daub — buff, mid-toned.
    /// type-audit: bare-ok(ratio)
    pub const COB: [f64; BANDS] = [0.12, 0.16, 0.22, 0.28, 0.34, 0.40, 0.46, 0.50, 0.52, 0.53];
    /// Dry straw — pale and golden.
    /// type-audit: bare-ok(ratio)
    pub const THATCH: [f64; BANDS] = [0.10, 0.14, 0.20, 0.28, 0.38, 0.46, 0.52, 0.55, 0.57, 0.58];
}

/// What a fabric returns of the light that reaches it.
///
/// **[`Fabric::Stone`] is derived and the other three are authored.** Stone
/// re-projects the very buffer the ground under the wall already carries,
/// through `hornvale_terrain::lithology::reflectance` — the same producer
/// `hornvale_locale::LocaleContext::reflectance_at` calls for the ground
/// itself, integrated by the area law. Nothing new is computed and nothing
/// is stored; this is a projection, not a derivation.
///
/// The authored three ignore `ctx` on purpose: a timber wall is timber
/// wherever it stands. Only stone carries the place it came from, which is
/// why only stone can carry H1.
pub fn reflectance_of(fabric: Fabric, ctx: &FabricContext) -> Reflectance {
    match fabric {
        Fabric::Stone => {
            hornvale_terrain::lithology::reflectance(&ctx.material, ctx.rock).integrate()
        }
        Fabric::Timber => authored_reflectance(authored::TIMBER),
        Fabric::Cob => authored_reflectance(authored::COB),
        Fabric::Thatch => authored_reflectance(authored::THATCH),
    }
}

/// Wrap an authored curve. Every constant in [`authored`] is within `[0,1]`
/// by inspection, and `an_authored_curve_is_a_valid_reflectance` proves it
/// rather than leaving the `expect` to speak for itself.
fn authored_reflectance(bands: [f64; BANDS]) -> Reflectance {
    Reflectance::new(bands).expect("an authored fabric curve is within [0, 1]")
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A synthetic buffer for the UNIT tests below, which exercise the
    /// *selection* rules and the *authored* curves only.
    ///
    /// **This is an authored fixture and says so.** No claim about the
    /// world is made from it: H1 — the only claim that rests on what stone
    /// actually looks like — is measured on real terrain in
    /// `windows/vessel/tests/lantern_fabric.rs`, because The Beholding's
    /// 28-of-255 on authored fixtures collapsed to 2-of-255 on real ground.
    fn synthetic_ctx(rock: RockClass) -> FabricContext {
        FabricContext {
            rock,
            material: MaterialBuffer {
                silica: 0.6,
                grain: 0.5,
                induration: 0.7,
                carbonate: 0.1,
                metamorphic_grade: 0.0,
                porosity: 0.2,
                margin: hornvale_terrain::lithology::MarginPolarity::Interior,
                soil_depth: hornvale_terrain::lithology::SoilDepth::new(0.5),
                basement: hornvale_terrain::lithology::Basement::Continental,
                thaumic: 0.0,
            },
            forested: false,
            temperate: false,
            deep_soil: false,
            dry: false,
        }
    }

    /// FIRES WHEN: a threshold is given a fabric. An opening is not a
    /// fabric, and the palette must be able to tell "no material here" from
    /// "some material I have not named".
    #[test]
    fn a_threshold_has_no_fabric() {
        let ctx = synthetic_ctx(RockClass::Granite);
        assert_eq!(fabric_of(CellKind::Threshold(0, 1), &ctx), None);
        assert!(fabric_of(CellKind::Wall, &ctx).is_some());
        assert!(fabric_of(CellKind::Floor(0), &ctx).is_some());
    }

    /// FIRES WHEN: the selection rules stop distinguishing the four
    /// fabrics — every arm here names a different one, so collapsing any
    /// two reddens it.
    #[test]
    fn each_fabric_has_a_ground_that_selects_it() {
        let mut ctx = synthetic_ctx(RockClass::Granite);
        assert_eq!(fabric_of(CellKind::Wall, &ctx), Some(Fabric::Stone));

        ctx.rock = RockClass::Shale;
        ctx.forested = true;
        ctx.temperate = true;
        assert_eq!(fabric_of(CellKind::Wall, &ctx), Some(Fabric::Timber));

        ctx.forested = false;
        ctx.temperate = false;
        ctx.deep_soil = true;
        ctx.dry = true;
        assert_eq!(fabric_of(CellKind::Wall, &ctx), Some(Fabric::Cob));

        ctx.deep_soil = false;
        ctx.dry = false;
        assert_eq!(fabric_of(CellKind::Floor(0), &ctx), Some(Fabric::Thatch));
    }

    /// FIRES WHEN: an authored curve leaves `[0, 1]` — the constructor
    /// `reflectance_of` unwraps would panic in production instead.
    #[test]
    fn an_authored_curve_is_a_valid_reflectance() {
        let ctx = synthetic_ctx(RockClass::Granite);
        for fabric in [Fabric::Timber, Fabric::Cob, Fabric::Thatch] {
            let r = reflectance_of(fabric, &ctx);
            for band in r.get() {
                assert!(
                    (0.0..=1.0).contains(band),
                    "{fabric:?} left the unit interval at {band}"
                );
            }
        }
    }

    /// FIRES WHEN: `reflectance_of(Stone, ..)` stops reading `ctx` — the
    /// mutation Task 3 Step 5 applies deliberately. This is the UNIT-level
    /// half of that guard; the claim that the variation survives on real
    /// terrain is H1's, and lives in the integration battery.
    #[test]
    fn stone_reads_the_buffer_it_is_given() {
        let felsic = synthetic_ctx(RockClass::Granite);
        let mut mafic = synthetic_ctx(RockClass::Basalt);
        mafic.material.silica = 0.05;
        assert_ne!(
            reflectance_of(Fabric::Stone, &felsic).get(),
            reflectance_of(Fabric::Stone, &mafic).get(),
            "granite and basalt returned the same stone: derived stone is \
             not reading its buffer"
        );
    }

    /// FIRES WHEN: an authored fabric starts varying with the ground. Only
    /// stone is derived; a timber wall is timber wherever it stands, and a
    /// timber that quietly picked up the bedrock would make H1's mutation
    /// test unable to isolate stone.
    #[test]
    fn an_authored_fabric_ignores_the_ground() {
        let felsic = synthetic_ctx(RockClass::Granite);
        let mut mafic = synthetic_ctx(RockClass::Basalt);
        mafic.material.silica = 0.05;
        for fabric in [Fabric::Timber, Fabric::Cob, Fabric::Thatch] {
            assert_eq!(
                reflectance_of(fabric, &felsic).get(),
                reflectance_of(fabric, &mafic).get(),
                "{fabric:?} varied with the bedrock"
            );
        }
    }
}
