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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Cave {
    /// Which process opened it.
    pub kind: CaveKind,
    /// The deepest band of the cell's column the void penetrates.
    pub deepest_band: BandKind,
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

/// Hops beyond which a plate contact contributes no fault-void stress — the
/// width of the actively-deforming belt around the contact, in cells. A
/// provisional value; the campaign's calibration task sets it against the
/// scale one hop represents.
const FRACTURE_STRESS_REACH: f64 = 8.0;

/// Fault-void stress from boundary proximity, `[0,1]`: full on a plate
/// contact, tapering linearly to zero at [`FRACTURE_STRESS_REACH`], and zero
/// in a cratonic interior with no reachable boundary.
///
/// **Why this is not [`belt_weight`].** `belt_weight` carries an
/// `INTERIOR_FLOOR` deliberately — its own doc says boundaries only *raise*
/// the weight above it — and that is right for what it models: a feature
/// *belt* concentrates an ore province that also exists, thinly, in the
/// interior. A fault void is not a concentration of a background process; it
/// requires a fault. `boundary_distance` is this model's only proxy for fault
/// density, and a cell with no reachable same-plate boundary has, by that
/// proxy, no fault to host a void. Reading a floored weight as stress credits
/// every cratonic-interior cell with a third of the stress of a plate contact,
/// which makes distance from a fault inexpressible — a term that cannot fall
/// below 0.3 cannot say "far from any fault".
///
/// A linear taper to zero at a stated reach, rather than [`belt_weight`]'s
/// hyperbolic decay, because the physical claim being made is exactly that
/// there is a distance beyond which the boundary's damage does not reach; a
/// curve that never reaches zero is why `belt_weight` needed a floor at all.
/// type-audit: bare-ok(count: hops), bare-ok(ratio: return)
pub fn fracture_stress(hops: Option<u32>) -> f64 {
    match hops {
        Some(h) => (1.0 - h as f64 / FRACTURE_STRESS_REACH).clamp(0.0, 1.0),
        None => 0.0,
    }
}

/// Fracture proneness, `[0,1]`: a fault void. Needs stress (proximity to a
/// plate contact) and rock competent enough to hold an open void rather than
/// creep shut.
///
/// **Brittleness is induration, not the absence of metamorphism.** Until The
/// Hollow's Task 5 this read `induration * (1 - metamorphic_grade)`, which was
/// wrong twice over:
///
/// - *Wrong sign on the physics.* `metamorphic_grade` records peak burial
///   pressure and temperature — a rock's history, not its present rheology. At
///   the depths a cave occupies, gneiss, schist and quartzite are among the
///   most brittle rocks in the crust; ductile behaviour is a function of
///   current depth and temperature, which this term does not read. High grade
///   is not evidence of a rock that flows.
/// - *Anti-correlated with its own stress term by construction.*
///   [`crate::lithology::induration_at`] defines `metamorphic_grade` as
///   `1 - hops/OROGEN_REACH` within four hops of a boundary on continental
///   crust. It is therefore a decreasing function of exactly the distance the
///   stress term reads as increasing. The product was zero at `hops = 0` — on
///   a plate contact, the most faulted place in the model, no fault cave could
///   exist — and peaked at `hops = 4`, where the overprint has just run out.
///   Its maximum over all land was ~0.39, below
///   `DEEP_PROCESS_PRONENESS` (0.5), so [`BandKind::Roots`] was not rare but
///   *unreachable*: a ceiling below the threshold that reads it, which is the
///   spec's own §2.2 failure reproduced in new code.
///
/// Competence is what keeps a void open: cemented, indurated rock holds a
/// fracture; soft, poorly consolidated rock closes it by creep. `induration`
/// is that axis and needs no second factor.
/// type-audit: bare-ok(count: boundary_distance), bare-ok(ratio: return)
pub fn fracture_proneness(buf: &MaterialBuffer, boundary_distance: Option<u32>) -> f64 {
    let stress = fracture_stress(boundary_distance);
    let competent = buf.induration.clamp(0.0, 1.0);
    (stress * competent).clamp(0.0, 1.0)
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

/// Proneness at or above which a process is strong enough to reach one band
/// deeper than its host.
const DEEP_PROCESS_PRONENESS: f64 = 0.5;

/// The deepest band a cave of this kind penetrates, given the cell's column.
///
/// Mirrors [`deposit_depth`], which types an ore body's depth as a named
/// [`BandKind`] rather than a count. The retired `depth_reach_bands` was
/// `1 + (cave_proneness * 3.0) as u32`, which could not reach band 3 (it
/// needed proneness >= 2/3 against a theoretical ceiling of 0.573) nor band 4
/// (it needed exactly 1.0), so every cave in every world sat at band 2
/// (spec §2.2). A band derived from bands cannot reproduce that failure.
///
/// This restores The Lode's own §5 intent — "depth-reach from `cave_proneness`
/// x the cover/carbonate band depth" — whose band-depth half was never
/// implemented.
/// type-audit: bare-ok(ratio: proneness)
pub fn cave_depth(
    kind: CaveKind,
    column: &crate::strata::StratigraphicColumn,
    proneness: f64,
) -> BandKind {
    let strong = proneness >= DEEP_PROCESS_PRONENESS;
    match kind {
        // Dissolution works the sedimentary cover, and reaches the basement
        // contact where the cover is thin on ancient rock (an unconformity)
        // or where the process is strong.
        CaveKind::Karst => {
            if strong || column.unconformity {
                BandKind::Basement
            } else {
                BandKind::Cover
            }
        }
        // A tube is the flow it drained out of, so it never leaves the cover.
        CaveKind::LavaTube => BandKind::Cover,
        // Faults cut crystalline rock, and deep ones reach the roots.
        CaveKind::Fracture => {
            if strong {
                BandKind::Roots
            } else {
                BandKind::Basement
            }
        }
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

/// Spatial frequency of the cave presence gate's noise field. Named here
/// rather than inlined at the call site so the calibration in [`uniformize`]
/// and the field it calibrates against cannot drift apart.
/// type-audit: bare-ok(ratio)
pub const CAVE_GATE_FREQ: f64 = 5.0;
/// Octave count of the cave presence gate's noise field. See [`CAVE_GATE_FREQ`].
/// type-audit: bare-ok(count)
pub const CAVE_GATE_OCTAVES: u32 = 4;

/// Mean of `sphere_fbm01` at [`CAVE_GATE_FREQ`]/[`CAVE_GATE_OCTAVES`],
/// **measured, not assumed** — 0.500274 over 655 488 samples (64 seeds x a
/// level-5 globe). Statistically indistinguishable from 0.5, which is what
/// the three-slice construction implies; the measured figure is kept so the
/// provenance of the number is the measurement rather than the derivation.
/// See [`uniformize`].
const GATE_NOISE_MEAN: f64 = 0.5003;
/// Standard deviation of the same field, measured over the same 655 488
/// samples: 0.076443. The field is very nearly Gaussian there — skewness
/// -0.010, excess kurtosis -0.059 — which is what licenses the normal-CDF
/// warp in [`uniformize`]. Note this is much wider than the 0.058 The
/// Hollow's plan guessed from land-only bucket data.
const GATE_NOISE_SD: f64 = 0.0764;

/// Map an fbm sample onto a uniform `[0,1]` variate, so that comparing it
/// against a probability is a genuine Bernoulli trial.
///
/// **Why this exists.** `sphere_fbm01` returns values massed near 0.5, not
/// spread uniformly — the three-slice average compresses variance toward the
/// middle (see `crust.rs`), leaving a field with SD ~0.076 rather than a
/// uniform's 0.289. Measured over 64 level-5 globes, every sample fell in
/// `[0.161, 0.832]` and `P(noise < 0.35) = 0.0022`. Comparing a probability
/// directly against it — which the model did from The Lode until The Hollow —
/// makes [`presence_prob`] a probability in name only, firing a nominal 0.325
/// at 0.011 (spec §2.3).
///
/// **Why a monotone warp specifically.** The noise serves two purposes at
/// once: it sets the presence *rate* and it makes features *cluster*. A
/// monotone transform preserves the spatial ordering exactly, so clustering
/// is untouched by construction while the marginal is corrected — the one
/// repair that fixes the first purpose without touching the second.
///
/// **Why it is applied here and not inside `sphere_fbm01`.** Two other
/// callers depend on the raw distribution: `deposit_at` feeds the sample to
/// `deposit_grade_tonnage` as a *value*, and `prehuman_scar_at` compares it
/// against a threshold calibrated against exactly this marginal. Changing the
/// shared function would break both.
///
/// The transform is the normal CDF via the standard tanh approximation
/// (accurate to ~1e-4), which needs only `hornvale_kernel::math::tanh` and so
/// stays on the pinned `libm` path. Its argument is strictly increasing in
/// `noise` (`d/dz [A z (1 + B z^2)] = A (1 + 3 B z^2) > 0`), so the whole map
/// is monotone for every finite input.
/// type-audit: bare-ok(ratio: noise), bare-ok(ratio: return)
pub fn uniformize(noise: f64) -> f64 {
    /// Coefficient of the tanh approximation to the normal CDF.
    const A: f64 = 0.7988;
    /// Cubic correction term of the same approximation.
    const B: f64 = 0.044_17;
    let z = (noise - GATE_NOISE_MEAN) / GATE_NOISE_SD;
    (0.5 * (1.0 + hornvale_kernel::math::tanh(A * z * (1.0 + B * z * z)))).clamp(0.0, 1.0)
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
        // Nothing to dissolve, fully felsic (no tube), and incompetent rock ON a
        // plate contact — so the fracture term is zeroed by the rock rather than
        // by the (also-sufficient) absence of a boundary, which would make the
        // fracture half of this assertion vacuous.
        let mut inert = buf(0.0, 1.0);
        inert.induration = 0.0;
        assert_eq!(fracture_stress(Some(0)), 1.0, "stress must be live here");
        assert_eq!(cave_process(&inert, 0.0, 0.9, Some(0)), None);
    }

    /// A fault void must be possible where the faults are. `metamorphic_grade`
    /// is `1 - hops/OROGEN_REACH` near a boundary, so the retired
    /// `induration * (1 - metamorphic_grade)` brittleness term was exactly zero
    /// at `hops = 0` — the most faulted place in the model could host no
    /// fracture cave, and the term's land maximum (~0.39) sat below
    /// `DEEP_PROCESS_PRONENESS`, making `Roots` unreachable.
    #[test]
    fn a_plate_contact_can_host_a_fault_void_and_reach_the_roots() {
        // Hard rock fully overprinted by the orogen it sits in — the exact cell
        // the old formula scored at zero.
        let mut contact = buf(0.0, 0.7);
        contact.induration = 0.9;
        contact.metamorphic_grade = 1.0;
        let p = fracture_proneness(&contact, Some(0));
        assert!(
            p > 0.0,
            "a plate contact scored {p}, so no fault cave can open"
        );
        assert!(
            p >= DEEP_PROCESS_PRONENESS,
            "fracture peaks at {p}, under the {DEEP_PROCESS_PRONENESS} a Roots-deep cave needs"
        );
        let col = crate::strata::column(
            35.0,
            0.3,
            true,
            400.0,
            1.0,
            RockClass::Sandstone,
            Basement::Continental,
        );
        assert_eq!(cave_depth(CaveKind::Fracture, &col, p), BandKind::Roots);
    }

    /// Fault-void stress must be able to say "far from any fault".
    /// `belt_weight`'s `INTERIOR_FLOOR` is right for feature belts and wrong
    /// here: it credits a cratonic interior with a third of a plate contact's
    /// stress, which made `Fracture` a background process rather than a
    /// boundary one.
    #[test]
    fn fault_stress_has_no_interior_floor_unlike_a_feature_belt() {
        assert_eq!(fracture_stress(None), 0.0);
        assert!(
            belt_weight(None) > 0.0,
            "the belt floor is deliberate; this is the contrast"
        );
        // ... and it reaches zero at a finite distance, which a hyperbolic decay
        // never does.
        assert_eq!(fracture_stress(Some(FRACTURE_STRESS_REACH as u32)), 0.0);
        assert!(fracture_stress(Some(1)) > fracture_stress(Some(4)));
        assert!(fracture_stress(Some(4)) > fracture_stress(Some(7)));

        // A cell with no reachable boundary hosts no fault void however hard
        // its rock is.
        let mut hard = buf(0.0, 0.7);
        hard.induration = 1.0;
        assert_eq!(fracture_proneness(&hard, None), 0.0);
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
    fn cave_depth_differs_by_kind() {
        use crate::strata::column;
        // Thick cover (401 m) on young crust: no unconformity.
        let thick = column(
            35.0,
            0.3,
            true,
            400.0,
            1.0,
            RockClass::Sandstone,
            Basement::Continental,
        );
        assert!(!thick.unconformity, "fixture must NOT be an unconformity");
        assert_eq!(cave_depth(CaveKind::Karst, &thick, 0.2), BandKind::Cover);
        assert_eq!(cave_depth(CaveKind::LavaTube, &thick, 0.9), BandKind::Cover);
        assert_eq!(
            cave_depth(CaveKind::Fracture, &thick, 0.2),
            BandKind::Basement
        );
    }

    #[test]
    fn a_strong_process_reaches_one_band_deeper() {
        use crate::strata::column;
        let thick = column(
            35.0,
            0.3,
            true,
            400.0,
            1.0,
            RockClass::Sandstone,
            Basement::Continental,
        );
        assert_eq!(cave_depth(CaveKind::Karst, &thick, 0.9), BandKind::Basement);
        assert_eq!(cave_depth(CaveKind::Fracture, &thick, 0.9), BandKind::Roots);
    }

    #[test]
    fn karst_on_thin_cover_reaches_the_basement_contact() {
        use crate::strata::column;
        // Thin cover (11 m) on ancient basement (age 0.9): an unconformity, so
        // dissolution reaches the contact however weak the process is.
        let thin = column(
            35.0,
            0.9,
            true,
            10.0,
            1.0,
            RockClass::ReefLimestone,
            Basement::Continental,
        );
        assert!(
            thin.unconformity,
            "fixture must actually be an unconformity"
        );
        assert_eq!(cave_depth(CaveKind::Karst, &thin, 0.1), BandKind::Basement);
    }

    #[test]
    fn a_lava_tube_never_leaves_the_cover() {
        use crate::strata::column;
        let thin = column(
            35.0,
            0.9,
            true,
            10.0,
            1.0,
            RockClass::Basalt,
            Basement::Continental,
        );
        assert_eq!(cave_depth(CaveKind::LavaTube, &thin, 1.0), BandKind::Cover);
    }

    #[test]
    fn uniformize_is_monotone_and_bounded() {
        let mut prev = -1.0;
        for i in 0..=1000 {
            let x = i as f64 / 1000.0;
            let u = uniformize(x);
            assert!(
                (0.0..=1.0).contains(&u),
                "uniformize({x}) = {u} escaped [0,1]"
            );
            assert!(u >= prev, "uniformize is not monotone at {x}: {u} < {prev}");
            prev = u;
        }
    }

    /// Worlds pooled by [`uniformize_turns_the_cave_gate_noise_into_a_uniform_variate`].
    ///
    /// **One globe is not a sample of the marginal.** At [`CAVE_GATE_FREQ`] a
    /// single level-5 world holds only ~10^2 independent noise blobs, so its
    /// own mean and spread wander: measured over 64 seeds, per-world mean
    /// ranged 0.4835-0.5237 and per-world SD 0.0662-0.0854 against a pooled
    /// 0.50027 / 0.07644. A per-world decile histogram therefore measures that
    /// sampling wobble, not the warp — under the correct constants the
    /// per-world worst decile deviation still has median ~0.025 and reaches
    /// 0.073, so a single-globe +/-0.035 test fails for roughly a quarter of
    /// seeds no matter how well [`uniformize`] is calibrated. Pooling twelve
    /// worlds averages the wobble down (worst decile deviation 0.0068) so the
    /// histogram tests the transform instead of the draw.
    const UNIFORMITY_WORLDS: std::ops::RangeInclusive<u64> = 1..=12;

    #[test]
    fn uniformize_turns_the_cave_gate_noise_into_a_uniform_variate() {
        use crate::provider::GeneratedTerrain;
        use crate::{TerrainPins, generate};
        use hornvale_kernel::{Geosphere, Seed};

        let geo = Geosphere::new(5);
        let mut deciles = [0usize; 10];
        let mut n = 0usize;
        for seed in UNIFORMITY_WORLDS {
            let outcome = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
            let terrain = GeneratedTerrain::new(geo.clone(), outcome);
            let noise_seed = terrain.globe().features_noise_seed();
            for cell in geo.cells() {
                let raw = crate::crust::sphere_fbm01(
                    noise_seed,
                    geo.position(cell),
                    CAVE_GATE_FREQ,
                    CAVE_GATE_OCTAVES,
                );
                let u = uniformize(raw);
                deciles[((u * 10.0) as usize).min(9)] += 1;
                n += 1;
            }
        }

        for (i, &count) in deciles.iter().enumerate() {
            let share = count as f64 / n as f64;
            assert!(
                (share - 0.1).abs() < 0.035,
                "decile {i} holds {share:.4} of samples, not ~0.10 — the warp did \
                 not uniformize the field (n={n}, deciles={deciles:?})"
            );
        }
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
