//! The eight world-invariant snap-judgment axes (spec §5) and their
//! per-axis normalized distance functions.
//!
//! **Design note on the distance formulas (0021: structural distance only,
//! never an authored preference between named peoples).** Every formula
//! below is a generic function of the *shape* of two peoples' attribute
//! vectors — a categorical mismatch, a normalized vector gap, a
//! mass-log-ratio — with no constant that names or singles out a
//! particular people. The handful of literal constants that do appear
//! (the predation familiarity/fear shift, the soft-saturation curve's unit
//! divisor) are applied uniformly to every pair by the same rule; they
//! shape how a *given* gap maps to `[0,1]`, not which pairs are close.
//!
//! Each vector axis picks a normalization independently and documents the
//! choice on its helper function, per the brief's design latitude:
//! - **Categorical fields** (habitat realm, sociality, status basis,
//!   activity cycle, exotic articulation manner) use 0/1 mismatch — the
//!   field has no natural metric, only identity.
//! - **Scalar ratio fields already in `[0,1]`** (`in_group_radius`,
//!   `night_vision`, `sky_attention`, the six articulation scalars,
//!   `reproductive_tempo`) use plain absolute difference — already
//!   normalized by construction.
//! - **`ConditionNiche`'s per-axis optimum gap** is normalized by the pair's
//!   combined tolerance width (`|Δoptimum| / (width_a + width_b)`), which is
//!   dimensionless and axis-unit-agnostic (temperature in °C, moisture/
//!   insolation as bare ratios, elevation in metres all divide out), then
//!   soft-saturated into `[0,1)` via `x / (x + 1)`.
//! - **`SizeThreat`** uses a soft-saturated positive-log-mass-ratio, floored
//!   at zero when the *other* people is not the larger one — the
//!   directional half the brief calls for (a small people finds a much
//!   larger one threatening; the reverse need not hold).
//! - **`DietPredation`** starts from Pianka niche overlap (`1 - overlap`,
//!   already `[0,1]` and symmetric) and folds in the precomputed predation
//!   direction: a predator's distance to its own prey narrows (familiarity)
//!   while the prey's distance to its predator widens (fear) by the same
//!   fixed shift, which is what makes the axis directional.
//!
//! Every vector axis composes its per-component terms (each already bounded
//! to `[0,1]`) by plain arithmetic mean, which keeps the aggregate in
//! `[0,1]` automatically and keeps every component's contribution legible.

use hornvale_kernel::ConditionResponse;
use hornvale_language::speech::ArticulationVector;
use hornvale_species::{ConditionNiche, PerceptionVector, SocietyVector};

use crate::PeopleTraits;

/// The eight world-invariant snap-judgment axes (spec §5).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Axis {
    /// Surface vs. subterranean realm (categorical; near-degenerate today —
    /// only drow is Subterranean among the fifteen peoples).
    Habitat,
    /// Diet-niche overlap folded with the precomputed predator/prey
    /// direction. Directional.
    DietPredation,
    /// The four-axis environmental condition-tolerance profile
    /// (temperature/moisture/insolation/elevation).
    ConditionNiche,
    /// Authority shape, status basis, and in-group breadth.
    Sociality,
    /// Waking schedule plus night-vision and sky-attention.
    ActivityCycle,
    /// Reproductive pace on the r–K axis (`reproductive_tempo`).
    Reproductive,
    /// Speech articulation (the six-scalar vector plus exotic manner).
    Language,
    /// Body-mass-derived threat: how much bigger the *other* people is.
    /// Directional.
    SizeThreat,
}

impl Axis {
    /// All eight axes, in declaration order.
    pub const ALL: [Axis; 8] = [
        Axis::Habitat,
        Axis::DietPredation,
        Axis::ConditionNiche,
        Axis::Sociality,
        Axis::ActivityCycle,
        Axis::Reproductive,
        Axis::Language,
        Axis::SizeThreat,
    ];

    /// A short, stable lowercase identifier for the axis (report columns,
    /// diagnostics).
    /// type-audit: bare-ok(identifier-text: return)
    pub fn label(self) -> &'static str {
        match self {
            Axis::Habitat => "habitat",
            Axis::DietPredation => "diet-predation",
            Axis::ConditionNiche => "condition-niche",
            Axis::Sociality => "sociality",
            Axis::ActivityCycle => "activity-cycle",
            Axis::Reproductive => "reproductive",
            Axis::Language => "language",
            Axis::SizeThreat => "size-threat",
        }
    }

    /// Whether the axis is directional (predation, size) or mutual. A
    /// directional axis's `axis_distance(axis, a, b)` need not equal
    /// `axis_distance(axis, b, a)`.
    /// type-audit: bare-ok(flag: return)
    pub fn is_asymmetric(self) -> bool {
        matches!(self, Axis::DietPredation | Axis::SizeThreat)
    }
}

/// The normalized distance A→B on one axis, in `[0,1]`. Symmetric axes
/// (every axis but `DietPredation` and `SizeThreat`) satisfy
/// `axis_distance(x, a, b) == axis_distance(x, b, a)`; the two asymmetric
/// ones need not (see the module doc for each formula's direction).
/// type-audit: bare-ok(ratio: return)
pub fn axis_distance(axis: Axis, a: &PeopleTraits, b: &PeopleTraits) -> f64 {
    match axis {
        Axis::Habitat => categorical(&a.habitat, &b.habitat),
        Axis::DietPredation => diet_predation_distance(a, b),
        Axis::ConditionNiche => condition_niche_distance(&a.condition_niche, &b.condition_niche),
        Axis::Sociality => sociality_distance(&a.society, &b.society),
        Axis::ActivityCycle => activity_cycle_distance(&a.perception, &b.perception),
        Axis::Reproductive => reproductive_distance(a, b),
        Axis::Language => language_distance(&a.articulation, &b.articulation),
        Axis::SizeThreat => size_threat_distance(a, b),
    }
}

/// 0 if `a == b`, else 1 — the mismatch distance for a field with no
/// natural metric, only identity.
fn categorical<T: PartialEq>(a: &T, b: &T) -> f64 {
    if a == b { 0.0 } else { 1.0 }
}

/// Soft-saturate a non-negative value into `[0, 1)`: `0` stays `0`, and the
/// curve approaches `1` as `x` grows without bound. Used wherever a raw gap
/// (a log-mass-ratio, a width-normalized optimum gap) needs a bounded
/// distance without an arbitrarily chosen scale constant — the literal `1`
/// here is the curve's own unit, not a tuned parameter.
fn saturate(x: f64) -> f64 {
    debug_assert!(x >= 0.0, "saturate expects a non-negative gap, got {x}");
    x / (x + 1.0)
}

/// `DietPredation`: `1 - ` Pianka overlap, shifted by the precomputed
/// predation direction (see the module doc). The shift is a fixed
/// constant applied uniformly by relation, not by identity.
fn diet_predation_distance(a: &PeopleTraits, b: &PeopleTraits) -> f64 {
    /// How far a predation edge moves the base overlap distance: negative
    /// (narrower) from the predator's side, positive (wider) from the
    /// prey's. AUTHORED modeling constant, not a per-pair preference — it
    /// is added or subtracted identically for every predation edge in the
    /// roster.
    /// plumb: pending(wave-1)
    const PREDATION_SHIFT: f64 = 0.15;

    let base = 1.0 - a.niche.overlap(&b.niche);
    let shifted = if a.preys_on.contains(&b.id) {
        base - PREDATION_SHIFT
    } else if b.preys_on.contains(&a.id) {
        base + PREDATION_SHIFT
    } else {
        base
    };
    shifted.clamp(0.0, 1.0)
}

/// `ConditionNiche`: the mean of the four per-axis condition-response
/// distances (temperature, moisture, insolation, elevation).
fn condition_niche_distance(a: &ConditionNiche, b: &ConditionNiche) -> f64 {
    let terms = [
        condition_response_distance(&a.temperature, &b.temperature),
        condition_response_distance(&a.moisture, &b.moisture),
        condition_response_distance(&a.insolation, &b.insolation),
        condition_response_distance(&a.elevation, &b.elevation),
    ];
    mean(&terms)
}

/// One `ConditionResponse` pair's distance: the mean of a width-normalized
/// optimum gap (`|Δoptimum| / (width_a + width_b)`, soft-saturated — see
/// [`saturate`]) and the raw devotion-amplitude gap (already `[0,1]`-ish by
/// construction).
fn condition_response_distance(a: &ConditionResponse, b: &ConditionResponse) -> f64 {
    let combined_width = a.width + b.width;
    let optimum_term = if combined_width > 0.0 {
        saturate((a.optimum - b.optimum).abs() / combined_width)
    } else {
        0.0
    };
    let devotion_term = (a.devotion - b.devotion).abs().min(1.0);
    mean(&[optimum_term, devotion_term])
}

/// `Sociality`: the mean of the sociality-shape mismatch, the status-basis
/// mismatch, and the `in_group_radius` gap (already `[0,1]`).
fn sociality_distance(a: &SocietyVector, b: &SocietyVector) -> f64 {
    let terms = [
        categorical(&a.sociality, &b.sociality),
        categorical(&a.status_basis, &b.status_basis),
        (a.in_group_radius - b.in_group_radius).abs().min(1.0),
    ];
    mean(&terms)
}

/// `ActivityCycle`: the mean of the activity-schedule mismatch and the
/// `night_vision`/`sky_attention` gaps (each already `[0,1]`).
fn activity_cycle_distance(a: &PerceptionVector, b: &PerceptionVector) -> f64 {
    let terms = [
        categorical(&a.activity, &b.activity),
        (a.night_vision - b.night_vision).abs().min(1.0),
        (a.sky_attention - b.sky_attention).abs().min(1.0),
    ];
    mean(&terms)
}

/// `Reproductive`: the absolute gap between the two peoples'
/// `hornvale_species::reproductive_tempo` (already `[0,1]` by
/// construction).
fn reproductive_distance(a: &PeopleTraits, b: &PeopleTraits) -> f64 {
    let tempo_a = hornvale_species::reproductive_tempo(a.mass, a.thermal_strategy, a.schedule);
    let tempo_b = hornvale_species::reproductive_tempo(b.mass, b.thermal_strategy, b.schedule);
    (tempo_a - tempo_b).abs().min(1.0)
}

/// `Language`: the mean of the six articulation scalars' gaps (each already
/// `[0,1]`) and the exotic-manner mismatch.
fn language_distance(a: &ArticulationVector, b: &ArticulationVector) -> f64 {
    let terms = [
        (a.labiality - b.labiality).abs().min(1.0),
        (a.vowel_space - b.vowel_space).abs().min(1.0),
        (a.voicing - b.voicing).abs().min(1.0),
        (a.sibilance - b.sibilance).abs().min(1.0),
        (a.voice_loudness - b.voice_loudness).abs().min(1.0),
        (a.tonality - b.tonality).abs().min(1.0),
        categorical(&a.exotic, &b.exotic),
    ];
    mean(&terms)
}

/// `SizeThreat`: how threatening B's size reads to A — a soft-saturated,
/// floored-at-zero function of `ln(mass_b) - ln(mass_a)`. Zero (not
/// merely small) whenever B is not the larger of the pair, which is what
/// makes the axis directional: the smaller party's view of the larger one
/// carries the whole signal.
fn size_threat_distance(a: &PeopleTraits, b: &PeopleTraits) -> f64 {
    let log_ratio = hornvale_kernel::math::ln(b.mass.kilograms())
        - hornvale_kernel::math::ln(a.mass.kilograms());
    saturate(log_ratio.max(0.0))
}

/// The arithmetic mean of a fixed-size slice of already-bounded `[0,1]`
/// terms — keeps every vector axis's aggregate in `[0,1]` without a further
/// clamp.
fn mean(terms: &[f64]) -> f64 {
    debug_assert!(!terms.is_empty());
    terms.iter().sum::<f64>() / terms.len() as f64
}

// Per-axis structural-property tests live in `tests/suite/axes.rs` (the
// consolidated integration-test binary), not here — they exercise only the
// public API (`Axis`, `axis_distance`, `PeopleTraits`'s public fields,
// `catalog`), which an integration test can already reach.
#[cfg(test)]
mod smoke_tests {
    use super::*;
    use hornvale_kernel::{Mass, PLANT_FORAGE, ResourceVector};
    use hornvale_language::speech::ExoticManner;
    use hornvale_species::{
        ActivityCycle, CarbonSource, ElectronDonor, EnergySource, HabitatRealm, LifeSchedule,
        MindVector, Sociality, StatusBasis, ThermalStrategy,
    };
    use std::collections::BTreeSet;

    /// A synthetic, self-consistent `PeopleTraits` for probing a single
    /// axis's structural properties without touching the real catalog (so
    /// nothing here can be read as an authored opinion about a named
    /// people — 0021).
    fn synthetic(id: &'static str) -> PeopleTraits {
        PeopleTraits {
            id: hornvale_species::KindId(id),
            habitat: HabitatRealm::SURFACE,
            niche: ResourceVector::new(&[(PLANT_FORAGE, 1.0)]).expect("valid weights"),
            condition_niche: ConditionNiche {
                temperature: ConditionResponse {
                    optimum: 15.0,
                    width: 10.0,
                    devotion: 0.5,
                },
                moisture: ConditionResponse {
                    optimum: 0.5,
                    width: 0.3,
                    devotion: 0.5,
                },
                insolation: ConditionResponse {
                    optimum: 0.5,
                    width: 0.3,
                    devotion: 0.5,
                },
                elevation: ConditionResponse {
                    optimum: 500.0,
                    width: 500.0,
                    devotion: 0.5,
                },
            },
            mass: Mass::new(70.0).expect("valid mass"),
            thermal_strategy: ThermalStrategy::Endothermic,
            energy_source: EnergySource::Chemotrophic,
            electron_donor: ElectronDonor::Organotrophic,
            carbon_source: CarbonSource::Heterotrophic,
            schedule: LifeSchedule::Allometric,
            society: SocietyVector {
                sociality: Sociality::Hierarchic,
                status_basis: StatusBasis::Rank,
                in_group_radius: 0.5,
            },
            mind: MindVector::MANIKIN,
            perception: PerceptionVector {
                activity: ActivityCycle::Diurnal,
                night_vision: 0.5,
                sky_attention: 0.5,
            },
            articulation: ArticulationVector {
                labiality: 0.5,
                vowel_space: 0.5,
                voicing: 0.5,
                sibilance: 0.5,
                voice_loudness: 0.5,
                tonality: 0.0,
                exotic: ExoticManner::None,
            },
            preys_on: BTreeSet::new(),
        }
    }

    fn close(x: f64, y: f64) -> bool {
        (x - y).abs() < 1e-9
    }

    #[test]
    fn every_axis_self_distance_is_zero() {
        let t = synthetic("test-self");
        for axis in Axis::ALL {
            let d = axis_distance(axis, &t, &t);
            assert!(
                close(d, 0.0),
                "{}: self-distance should be 0, got {d}",
                axis.label()
            );
        }
    }

    #[test]
    fn habitat_distance_is_categorical() {
        let mut a = synthetic("test-a");
        let mut b = synthetic("test-b");
        assert!(close(axis_distance(Axis::Habitat, &a, &b), 0.0));
        b.habitat = HabitatRealm::Subterranean;
        assert!(close(axis_distance(Axis::Habitat, &a, &b), 1.0));
        a.habitat = HabitatRealm::Subterranean;
        assert!(close(axis_distance(Axis::Habitat, &a, &b), 0.0));
    }
}
