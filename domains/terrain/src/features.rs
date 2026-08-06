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

/// Felsic index at or below which rock reads as mafic enough to have flowed
/// as basalt. Matches the `silica < 0.3` boundary the retired `cave_kind`
/// used, kept so the taxonomy's meaning does not silently shift.
const MAFIC_SILICA_MAX: f64 = 0.3;

/// Lava-tube proneness, `[0,1]`: a drained basaltic flow. Needs mafic rock
/// (low `silica`), extrusive texture (fine `grain` — a pluton never flowed),
/// and young crust, because old tubes collapse and are buried.
/// type-audit: bare-ok(ratio: crust_age), bare-ok(ratio: return)
pub fn lavatube_proneness(buf: &MaterialBuffer, crust_age: f64) -> f64 {
    let mafic = ((MAFIC_SILICA_MAX - buf.silica) / MAFIC_SILICA_MAX).clamp(0.0, 1.0);
    let extrusive = (1.0 - buf.grain).clamp(0.0, 1.0);
    let youth = (1.0 - crust_age).clamp(0.0, 1.0);
    (mafic * extrusive * youth).clamp(0.0, 1.0)
}

/// Fracture proneness, `[0,1]`: a fault void. Needs stress (proximity to a
/// plate contact) and rock that breaks rather than flows — hard and
/// unmetamorphosed. Reuses [`belt_weight`] for the stress term so fracture
/// caves and ore belts read the same lineament field.
/// type-audit: bare-ok(count: boundary_distance), bare-ok(ratio: return)
pub fn fracture_proneness(buf: &MaterialBuffer, boundary_distance: Option<u32>) -> f64 {
    let stress = belt_weight(boundary_distance);
    let brittle = buf.induration * (1.0 - buf.metamorphic_grade);
    (stress * brittle).clamp(0.0, 1.0)
}

/// The void-opening process this cell's rock best supports, with that
/// process's own proneness — `None` where no process operates.
///
/// **Kind is chosen BEFORE existence is tested**, mirroring [`deposit_kind`].
/// The retired `cave_kind` was asked only after a carbonate-gated existence
/// test had already passed, so its two non-`Karst` branches — both of which
/// require carbonate to be LOW — were unreachable (The Hollow, spec §2.1).
///
/// Selection is argmax over the three pronenesses rather than a priority
/// ladder, so the mix follows the fields instead of a hand-chosen order.
/// Ties break by `total_cmp` with declaration order as the deterministic
/// tie-break.
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: crust_age), bare-ok(count: boundary_distance), bare-ok(ratio: return)
pub fn cave_process(
    buf: &MaterialBuffer,
    drainage: f64,
    crust_age: f64,
    boundary_distance: Option<u32>,
) -> Option<(CaveKind, f64)> {
    let candidates = [
        (
            CaveKind::Karst,
            crate::lithology::cave_proneness(buf, drainage),
        ),
        (CaveKind::LavaTube, lavatube_proneness(buf, crust_age)),
        (
            CaveKind::Fracture,
            fracture_proneness(buf, boundary_distance),
        ),
    ];
    let best = candidates
        .iter()
        .copied()
        .enumerate()
        // max_by returns the LAST maximum on a tie; negate the index so the
        // earliest-declared kind wins instead.
        .max_by(|(ia, (_, a)), (ib, (_, b))| a.total_cmp(b).then_with(|| ib.cmp(ia)))
        .map(|(_, kv)| kv)?;
    if best.1 <= 0.0 { None } else { Some(best) }
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
///
/// `Ord`/`PartialOrd` (declaration order below) exist only so callers can
/// collect commodities into a `BTreeMap` (the project bans `HashMap`) with a
/// deterministic tie-break — there is no meaningful ranking between
/// commodities.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
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
    fn each_kind_is_selectable_by_its_own_field() {
        // Carbonate platform, wet, porous -> Karst.
        let mut karst = buf(0.7, 0.5);
        karst.porosity = 0.8;
        let (kind, p) = cave_process(&karst, 500.0, 0.5, Some(4)).expect("karst rock hosts a cave");
        assert_eq!(kind, CaveKind::Karst);
        assert!(p > 0.0, "selected kind must carry a positive proneness");

        // Young mafic fine-grained rock, no carbonate -> LavaTube.
        let mut lava = buf(0.0, 0.1);
        lava.grain = 0.1;
        let (kind, _) = cave_process(&lava, 0.0, 0.05, None).expect("young basalt hosts a cave");
        assert_eq!(kind, CaveKind::LavaTube);

        // Hard unmetamorphosed rock right on a plate contact -> Fracture.
        let mut frac = buf(0.0, 0.7);
        frac.induration = 0.95;
        let (kind, _) =
            cave_process(&frac, 0.0, 0.9, Some(0)).expect("brittle fault rock hosts a cave");
        assert_eq!(kind, CaveKind::Fracture);
    }

    #[test]
    fn a_cell_supporting_no_process_hosts_no_cave() {
        // Nothing to dissolve, fully felsic (no tube), perfectly plastic (nothing
        // to fracture).
        let mut inert = buf(0.0, 1.0);
        inert.induration = 0.0;
        assert_eq!(cave_process(&inert, 0.0, 0.9, None), None);
    }

    #[test]
    fn selection_takes_the_strongest_process_not_a_fixed_order() {
        // Weak carbonate against strong fracture conditions: fracture must win,
        // which a Karst-first priority ladder would get wrong.
        let mut b = buf(0.05, 0.7);
        b.porosity = 0.05;
        b.induration = 1.0;
        assert!(
            crate::lithology::cave_proneness(&b, 0.0) > 0.0,
            "the karst term must be live, or a priority ladder would agree by accident"
        );
        let (kind, _) = cave_process(&b, 0.0, 0.9, Some(0)).expect("hosts a cave");
        assert_eq!(kind, CaveKind::Fracture);

        // An exact tie goes to the earliest-declared kind, which is what the
        // negated index in the argmax tie-break buys: `max_by` would otherwise
        // return the LAST maximum (Fracture).
        let mut tied = buf(0.5, 0.7);
        tied.porosity = 1.0;
        tied.induration = 0.5;
        assert_eq!(
            crate::lithology::cave_proneness(&tied, 500.0),
            fracture_proneness(&tied, Some(0)),
            "the tie-break case must actually tie, or the assertion below is vacuous"
        );
        let (kind, _) = cave_process(&tied, 500.0, 0.9, Some(0)).expect("hosts a cave");
        assert_eq!(kind, CaveKind::Karst);
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
