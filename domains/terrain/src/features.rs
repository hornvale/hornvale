//! Located subsurface features — caves and ore deposits (The Lode). Typed
//! bodies derived from the fluid-flow fields terrain already owns, living in
//! The Deep's column. A deterministic hash-noise point process: no sequential
//! stream draws, no committed facts, no epoch. Mundane only — magical ores are
//! metaphysics-gated and stay reserved.

use crate::RockClass;
use crate::boundaries::BoundaryKind;
use crate::lithology::MaterialBuffer;
use crate::strata::BandKind;

/// A cave type, by the lithologic process that opened the void.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaveKind {
    /// Carbonate dissolution (wet limestone).
    Karst,
    /// Drained basaltic/volcanic tube.
    LavaTube,
    /// Fault/fracture void in tectonically active rock.
    Fracture,
}

/// A located cave at a cell.
/// type-audit: bare-ok(count: depth_reach_bands)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cave {
    /// Which process opened it.
    pub kind: CaveKind,
    /// How many of the column's bands the void penetrates (1 = cover only).
    pub depth_reach_bands: u32,
}

/// The cave type implied by lithology: carbonate dissolves to karst, mafic
/// (low-silica) volcanic drains to lava-tube, else a fault fracture.
/// type-audit: bare-ok(flag: near_fault)
pub fn cave_kind(buf: &MaterialBuffer, near_fault: bool) -> CaveKind {
    if buf.carbonate > 0.5 {
        CaveKind::Karst
    } else if buf.silica < 0.3 {
        CaveKind::LavaTube
    } else {
        let _ = near_fault; // fracture is the fallback; near_fault reinforces presence, not kind
        CaveKind::Fracture
    }
}

/// Lineament proximity weight: features cluster into belts near plate contacts.
/// `hops` is boundary distance (fewer = closer); `None` = cratonic interior,
/// which is the floor — boundaries only *raise* the weight above it, so a
/// far-from-boundary cell never scores below the interior (the `.max` floor).
/// type-audit: bare-ok(count: hops), bare-ok(ratio: return)
pub fn belt_weight(hops: Option<u32>) -> f64 {
    const INTERIOR_FLOOR: f64 = 0.3;
    match hops {
        Some(h) => (1.0 / (1.0 + h as f64 * 0.1)).max(INTERIOR_FLOOR),
        None => INTERIOR_FLOOR,
    }
}

/// Belt-weighted presence probability for a feature, from its gating field.
/// type-audit: bare-ok(ratio: field), bare-ok(ratio: belt), bare-ok(ratio: return)
pub fn presence_prob(field: f64, belt: f64) -> f64 {
    (field * (0.4 + 0.6 * belt)).clamp(0.0, 1.0)
}

/// The genetic process that formed a deposit — the taxonomy's primary axis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DepositProcess {
    /// Arc/intrusion magmatic (porphyry).
    Magmatic,
    /// Fault/orogen hydrothermal vein.
    Hydrothermal,
    /// Carbonate-hosted (MVT).
    CarbonateHosted,
    /// Chemical/biogenic sediment — the areal bedded ores.
    ChemicalSediment,
    /// Felsic-intrusion pegmatite (and metamorphic gems).
    Pegmatite,
    /// Secondary placer (eroded, in alluvium).
    Placer,
    /// Residual laterite (hot+wet weathering) — wired at worldgen (climate).
    Lateritic,
}

/// The commodity a deposit yields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Commodity {
    /// Copper (magmatic arc).
    Copper,
    /// Gold (hydrothermal vein or placer).
    Gold,
    /// Lead-zinc (carbonate-hosted).
    LeadZinc,
    /// Iron (BIF/ironstone).
    Iron,
    /// Salt/potash (evaporite).
    Salt,
    /// Coal (biogenic sediment).
    Coal,
    /// Gems + rare metals (pegmatite/metamorphic).
    Gems,
    /// Tin (placer).
    Tin,
    /// Bauxite/nickel (laterite).
    Bauxite,
}

/// A located ore deposit.
/// type-audit: bare-ok(ratio: grade), bare-ok(ratio: tonnage)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Deposit {
    /// Its genetic process.
    pub process: DepositProcess,
    /// Its commodity.
    pub commodity: Commodity,
    /// Which column band it sits in.
    pub depth: BandKind,
    /// Ore grade [0,1], lognormal — many poor, few rich.
    pub grade: f64,
    /// Relative tonnage [0,1]: small rich veins → huge disseminated bodies.
    pub tonnage: f64,
}

/// The dominant deposit family for a cell, from rock + tectonic setting.
/// Areal ores (iron/salt/coal) project directly from the rock class; the point
/// ores read the setting. Returns `None` where nothing is prospective.
/// type-audit: bare-ok(flag: endorheic), bare-ok(ratio: crust_age)
pub fn deposit_kind(
    rock: RockClass,
    boundary: Option<BoundaryKind>,
    buf: &MaterialBuffer,
    endorheic: bool,
    crust_age: f64,
) -> Option<(DepositProcess, Commodity)> {
    use Commodity::*;
    use DepositProcess::*;
    // Areal bedded ores: the rock IS the ore. (`Ironstone` classifies only on
    // the ocean floor — BIF is marine — so that arm is dead on land today; the
    // ancient-craton branch below is iron's land-reachable route.)
    match rock {
        RockClass::Ironstone => return Some((ChemicalSediment, Iron)),
        RockClass::Evaporite => return Some((ChemicalSediment, Salt)),
        RockClass::Coal => return Some((ChemicalSediment, Coal)),
        RockClass::Alluvium => return Some((Placer, Gold)),
        _ => {}
    }
    // Exhumed ancient banded-iron formation: the great iron ranges are marine
    // BIF now uplifted onto old, metamorphosed cratons — iron's path on land.
    if crust_age > 0.75 && buf.metamorphic_grade > 0.3 && buf.carbonate < 0.3 {
        return Some((ChemicalSediment, Iron));
    }
    if endorheic && buf.carbonate < 0.2 {
        return Some((ChemicalSediment, Salt));
    }
    // Point/vein/belt ores by setting.
    if buf.carbonate > 0.5 {
        return Some((CarbonateHosted, LeadZinc));
    }
    if buf.silica > 0.6 && buf.grain > 0.5 {
        return Some((Pegmatite, Gems)); // felsic + coarse (plutonic)
    }
    match boundary.map(setting_of) {
        Some(Setting::Arc) => Some((Magmatic, Copper)),
        Some(Setting::Fault) => Some((Hydrothermal, Gold)),
        _ => None,
    }
}

/// Coarse tectonic setting for deposit typing.
enum Setting {
    /// Subduction/convergent-arc contact: a magmatic arc or porphyry belt.
    Arc,
    /// Transform, divergent, or rift contact: a hydrothermal-vein setting.
    Fault,
    /// Continent–continent collision and anything else: neither of the above.
    Other,
}

/// Map a boundary kind to a coarse setting. `CoastalRange` (ocean–continent
/// subduction) and `IslandArc` (ocean–ocean subduction) both raise a
/// magmatic arc on the overriding plate → `Arc`. `Transform`,
/// `ContinentalRift`, and `OceanicRidge` are all fault/fracture-dominated,
/// fluid-pathway settings → `Fault`. `ContinentalCollision` is a thick-skinned
/// orogen without arc magmatism or through-going faulting at this coarse
/// grain → `Other`.
fn setting_of(kind: BoundaryKind) -> Setting {
    match kind {
        BoundaryKind::CoastalRange | BoundaryKind::IslandArc => Setting::Arc,
        BoundaryKind::Transform | BoundaryKind::ContinentalRift | BoundaryKind::OceanicRidge => {
            Setting::Fault
        }
        BoundaryKind::ContinentalCollision => Setting::Other,
    }
}

/// The column band a deposit's process places it in (primary deep, secondary shallow).
pub fn deposit_depth(process: DepositProcess) -> BandKind {
    match process {
        DepositProcess::Placer | DepositProcess::Lateritic => BandKind::Regolith,
        DepositProcess::ChemicalSediment | DepositProcess::CarbonateHosted => BandKind::Cover,
        DepositProcess::Magmatic => BandKind::Roots,
        DepositProcess::Hydrothermal | DepositProcess::Pegmatite => BandKind::Basement,
    }
}

/// Grade (lognormal-shaped: many poor, few rich) and relative tonnage from the
/// process and the gating prospectivity, using a hash-noise value in `[0,1)`.
/// type-audit: bare-ok(ratio: prospectivity), bare-ok(ratio: noise), bare-ok(ratio: return)
pub fn deposit_grade_tonnage(
    process: DepositProcess,
    prospectivity: f64,
    noise: f64,
) -> (f64, f64) {
    // Lognormal-ish: cube the uniform noise so most grades are low, a few high.
    let grade = (prospectivity * noise * noise * noise * 4.0).clamp(0.0, 1.0);
    // Disseminated magmatic = huge tonnage/low grade; veins = small/rich.
    let tonnage = match process {
        DepositProcess::Magmatic | DepositProcess::ChemicalSediment => 0.7 + 0.3 * noise,
        DepositProcess::Hydrothermal | DepositProcess::Pegmatite => 0.1 + 0.3 * noise,
        _ => 0.3 + 0.4 * noise,
    };
    (grade, tonnage.clamp(0.0, 1.0))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::RockClass;
    use crate::lithology::{Basement, MarginPolarity, MaterialBuffer, SoilDepth};

    fn buf(carbonate: f64, silica: f64) -> MaterialBuffer {
        MaterialBuffer {
            silica,
            grain: 0.5,
            induration: 0.5,
            carbonate,
            metamorphic_grade: 0.0,
            porosity: 0.5,
            margin: MarginPolarity::Interior,
            soil_depth: SoilDepth::new(1.0),
            basement: Basement::Continental,
            thaumic: 0.0,
        }
    }

    #[test]
    fn cave_kind_reads_lithology() {
        assert_eq!(cave_kind(&buf(0.8, 0.5), false), CaveKind::Karst);
        assert_eq!(cave_kind(&buf(0.1, 0.15), false), CaveKind::LavaTube);
        assert_eq!(cave_kind(&buf(0.1, 0.6), true), CaveKind::Fracture);
    }

    #[test]
    fn belt_weight_is_higher_near_lineaments() {
        assert!(belt_weight(Some(0)) > belt_weight(Some(8)));
        assert!(belt_weight(Some(8)) > belt_weight(None));
        // Far-from-boundary cells never dip below the cratonic-interior floor
        // (max boundary distance is ~49 hops at GLOBE_LEVEL 6).
        assert!(belt_weight(Some(30)) >= belt_weight(None));
        assert!(belt_weight(Some(49)) >= belt_weight(None));
    }

    #[test]
    fn areal_rock_classes_project_to_their_ore() {
        assert_eq!(
            deposit_kind(RockClass::Ironstone, None, &buf(0.0, 0.5), false, 0.0),
            Some((DepositProcess::ChemicalSediment, Commodity::Iron))
        );
        assert_eq!(
            deposit_kind(RockClass::Evaporite, None, &buf(0.0, 0.5), true, 0.0),
            Some((DepositProcess::ChemicalSediment, Commodity::Salt))
        );
        assert_eq!(
            deposit_kind(RockClass::Coal, None, &buf(0.0, 0.5), false, 0.0),
            Some((DepositProcess::ChemicalSediment, Commodity::Coal))
        );
    }

    #[test]
    fn ancient_cratons_carry_land_reachable_iron() {
        // Iron's land path: `RockClass::Ironstone` classifies only on the ocean
        // floor, so on land iron comes from exhumed BIF on old, metamorphosed,
        // non-carbonate cratons.
        let mut b = buf(0.1, 0.5);
        b.metamorphic_grade = 0.5;
        assert_eq!(
            deposit_kind(RockClass::Gneiss, None, &b, false, 0.85),
            Some((DepositProcess::ChemicalSediment, Commodity::Iron))
        );
        // A young craton carries no exhumed BIF.
        assert_eq!(deposit_kind(RockClass::Gneiss, None, &b, false, 0.2), None);
    }

    #[test]
    fn carbonate_hosts_lead_zinc_and_alluvium_hosts_placer() {
        assert_eq!(
            deposit_kind(RockClass::Marble, None, &buf(0.8, 0.4), false, 0.0).map(|(_, c)| c),
            Some(Commodity::LeadZinc)
        );
        assert_eq!(
            deposit_kind(RockClass::Alluvium, None, &buf(0.1, 0.5), false, 0.0).map(|(p, _)| p),
            Some(DepositProcess::Placer)
        );
    }

    #[test]
    fn placer_is_shallower_than_a_hydrothermal_vein() {
        use crate::strata::BandKind;
        assert_eq!(deposit_depth(DepositProcess::Placer), BandKind::Regolith);
        assert_eq!(
            deposit_depth(DepositProcess::Hydrothermal),
            BandKind::Basement
        );
    }
}
