//! Life-history allometry (BIO-2): pure scaling laws deriving a species'
//! life-history from body mass and metabolic class. Universal exponents;
//! per-class coefficients. No draws, no world state — see the design spec.

use crate::{LifeSchedule, MetabolicClass};
use hornvale_kernel::{Mass, Years, math};

// Exponents (discovered; spec §4).
const P_METABOLIC: f64 = 0.75; // Kleiber 1932
const P_TIME: f64 = 0.25; // metabolic-time scaling

// Calibration anchor: 40 kg Endotherm → 60 yr lifespan, 12 yr maturity.
const ANCHOR_MASS_KG: f64 = 40.0;
const ANCHOR_LIFESPAN_YR: f64 = 60.0;
const ANCHOR_MATURITY_YR: f64 = 12.0;

// Metabolic normalization (W·kg^-0.75).
const B0_ENDOTHERM: f64 = 3.4;
const ECTOTHERM_METABOLIC_FRACTION: f64 = 1.0 / 8.0;

/// The largest value the **class** component (`pace_multiplier`) returns —
/// the ectotherm shift — and the normalizer `pace_of_life` divides by.
///
/// This ceiling governs the class multiplier only. An authored
/// [`crate::LifeSchedule::Paced`] factor above it deliberately **saturates**
/// `pace_of_life` and `reproductive_tempo` at 1.0 rather than raising this
/// constant: raising it would rescale `pace_of_life` for every kind in the
/// roster, moving the committed `pace-of-life-*` census columns and every
/// almanac's pace headline. Saturation here is stated rather than silent,
/// which is what the original instruction was protecting against. A future
/// *class* needing a larger multiplier is still the case where this constant
/// must move (The Long Age, spec §3.5).
const MAX_PACE_MULTIPLIER: f64 = 1.5;

/// Single per-class pace multiplier: shifts lifespan, maturity, and tempo
/// together so the fast–slow covariation stays coherent (spec §4). Ectotherms
/// are slower on every axis at once.
fn pace_multiplier(class: MetabolicClass) -> f64 {
    match class {
        MetabolicClass::Endotherm => 1.0,
        MetabolicClass::Ectotherm => MAX_PACE_MULTIPLIER,
        MetabolicClass::Autotroph => 1.0,
        // `life_history` nulls the biological traits for Ametabolic before
        // any of the four time laws are called; the bare `lifespan` (etc.)
        // called directly on an Ametabolic mass returns a number that means
        // nothing.
        MetabolicClass::Ametabolic => 1.0,
    }
}

/// Basal metabolic rate in watts at a reference temperature (spec §4/§10 CAP-1
/// — this is the BASAL rate; ectotherm realized rate couples to climate and is
/// deferred). Surface-limited for `Autotroph` — see `MetabolicClass::Autotroph`.
/// type-audit: bare-ok(ratio: return)
pub fn basal_metabolic_rate_w(mass: Mass, class: MetabolicClass) -> f64 {
    let b0 = match class {
        MetabolicClass::Endotherm | MetabolicClass::Autotroph => B0_ENDOTHERM,
        MetabolicClass::Ectotherm => B0_ENDOTHERM * ECTOTHERM_METABOLIC_FRACTION,
        MetabolicClass::Ametabolic => return 0.0,
    };
    b0 * math::powf(mass.kilograms(), P_METABOLIC)
}

/// Maximum lifespan (spec §4). `k_life` is calibrated to the 40 kg endotherm
/// anchor; the per-class pace multiplier lengthens ectotherm life. `schedule`
/// is the third input (The Long Age, spec §3): `Allometric`'s factor is
/// `1.0`, an IEEE-754 no-op, so every pre-campaign value is unchanged.
pub fn lifespan(mass: Mass, class: MetabolicClass, schedule: LifeSchedule) -> Years {
    let k_life = ANCHOR_LIFESPAN_YR / math::powf(ANCHOR_MASS_KG, P_TIME);
    let yr =
        schedule.factor() * pace_multiplier(class) * k_life * math::powf(mass.kilograms(), P_TIME);
    Years::new(yr).expect("mass is positive, so lifespan is finite and non-negative")
}

/// Age at first reproduction (spec §4), ~20 % of lifespan at the anchor.
/// `schedule` stretches this alongside `lifespan` (The Long Age, spec §3).
pub fn age_at_maturity(mass: Mass, class: MetabolicClass, schedule: LifeSchedule) -> Years {
    let k_mat = ANCHOR_MATURITY_YR / math::powf(ANCHOR_MASS_KG, P_TIME);
    let yr =
        schedule.factor() * pace_multiplier(class) * k_mat * math::powf(mass.kilograms(), P_TIME);
    Years::new(yr).expect("mass is positive, so maturity is finite and non-negative")
}

/// Reproductive tempo on the r–K axis, 0 (fast/prolific) … 1 (slow/sparse),
/// rising with mass and the pace multiplier (spec §4/CAP-2 — this is
/// reproductive OUTPUT, distinct from overall pace-of-life). A saturating map
/// of `pace_multiplier · log10(mass)` over a fixed reference range keeps it
/// absolute (roster-independent). A strongly-paced `schedule` deliberately
/// **saturates** this at 1.0 rather than exceeding it (spec §3.4/§3.5).
/// type-audit: bare-ok(ratio: return)
pub fn reproductive_tempo(mass: Mass, class: MetabolicClass, schedule: LifeSchedule) -> f64 {
    // Fixed reference range: 1 kg → ~0, 1000 kg → ~1 (before the class shift).
    let raw = (math::log10(mass.kilograms()) / 3.0).clamp(0.0, 1.0);
    (raw * schedule.factor() * pace_multiplier(class)).clamp(0.0, 1.0)
}

/// Fraction of the post-maturity reproductive span at which generation length
/// falls (spec §5). Documented constant.
const GENERATION_FRACTION: f64 = 0.3;

/// A species' derived life-history profile (spec §5). Computed on demand from
/// the biosphere component — never stored. Biological fields are `None` for `Ametabolic`
/// (a construct has no mass-derived life-history); `pace_of_life` is a
/// size-derived position defined for anything with mass.
/// type-audit: bare-ok(ratio: basal_metabolic_rate_w), bare-ok(ratio: reproductive_tempo), bare-ok(ratio: pace_of_life)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct LifeHistory {
    /// Reference-temperature basal metabolic rate, watts; 0.0 if `Ametabolic`.
    pub basal_metabolic_rate_w: f64,
    /// Maximum lifespan; `None` if `Ametabolic`.
    pub lifespan: Option<Years>,
    /// Age at first reproduction; `None` if `Ametabolic`.
    pub age_at_maturity: Option<Years>,
    /// Reproductive output on the r–K axis, 0 fast … 1 slow; `None` if `Ametabolic`.
    pub reproductive_tempo: Option<f64>,
    /// Generation length (MEM-7's handle); `None` if `Ametabolic`.
    pub generation_length: Option<Years>,
    /// Overall life-history speed, 0 fast … 1 slow; absolute f(log mass).
    pub pace_of_life: f64,
}

/// Overall pace-of-life: absolute, roster-independent (spec §5). Maps log-mass
/// over a FIXED reference range (1 kg … 1000 kg) so adding a species never
/// shifts another's value. Larger/slower → 1. A strongly-paced `schedule`
/// deliberately **saturates** this at 1.0 rather than rescaling the ceiling
/// (spec §3.4/§3.5): `MAX_PACE_MULTIPLIER` normalizes the class component
/// only.
fn pace_of_life(mass: Mass, class: MetabolicClass, schedule: LifeSchedule) -> f64 {
    let raw = (math::log10(mass.kilograms()) / 3.0).clamp(0.0, 1.0);
    // Ectotherms read slower on the same size.
    (raw * schedule.factor() * pace_multiplier(class) / MAX_PACE_MULTIPLIER).clamp(0.0, 1.0)
}

/// Assemble the full life-history profile (spec §5). `schedule` is the third
/// input to the four time laws (The Long Age, spec §3); `basal_metabolic_rate_w`
/// deliberately keeps its two-argument signature and does not take it —
/// metabolic rate is mass-set (Kleiber), not schedule-set.
pub fn life_history(mass: Mass, class: MetabolicClass, schedule: LifeSchedule) -> LifeHistory {
    let bmr = basal_metabolic_rate_w(mass, class);
    let pace = pace_of_life(mass, class, schedule);
    if class == MetabolicClass::Ametabolic {
        return LifeHistory {
            basal_metabolic_rate_w: bmr,
            lifespan: None,
            age_at_maturity: None,
            reproductive_tempo: None,
            generation_length: None,
            pace_of_life: pace,
        };
    }
    let life = lifespan(mass, class, schedule);
    let mat = age_at_maturity(mass, class, schedule);
    let generation = Years::new(mat.get() + GENERATION_FRACTION * (life.get() - mat.get()))
        .expect("maturity ≤ lifespan, so generation length is non-negative");
    LifeHistory {
        basal_metabolic_rate_w: bmr,
        lifespan: Some(life),
        age_at_maturity: Some(mat),
        reproductive_tempo: Some(reproductive_tempo(mass, class, schedule)),
        generation_length: Some(generation),
        pace_of_life: pace,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::MetabolicClass::*;
    use hornvale_kernel::Mass;

    fn m(kg: f64) -> Mass {
        Mass::new(kg).unwrap()
    }

    #[test]
    fn anchor_hits_documented_targets() {
        assert!((lifespan(m(40.0), Endotherm, LifeSchedule::ALLOMETRIC).get() - 60.0).abs() < 1e-6);
        assert!(
            (age_at_maturity(m(40.0), Endotherm, LifeSchedule::ALLOMETRIC).get() - 12.0).abs()
                < 1e-6
        );
    }

    #[test]
    fn lifespan_and_maturity_increase_with_mass() {
        assert!(
            lifespan(m(132.0), Endotherm, LifeSchedule::ALLOMETRIC).get()
                > lifespan(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC).get()
        );
        assert!(
            age_at_maturity(m(132.0), Endotherm, LifeSchedule::ALLOMETRIC).get()
                > age_at_maturity(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC).get()
        );
    }

    #[test]
    fn ectotherms_outlive_endotherms_at_equal_mass() {
        assert!(
            lifespan(m(20.0), Ectotherm, LifeSchedule::ALLOMETRIC).get()
                > lifespan(m(20.0), Endotherm, LifeSchedule::ALLOMETRIC).get()
        );
    }

    #[test]
    fn metabolic_rate_rises_with_mass_and_is_lower_for_ectotherms() {
        assert!(
            basal_metabolic_rate_w(m(100.0), Endotherm)
                > basal_metabolic_rate_w(m(10.0), Endotherm)
        );
        assert!(
            basal_metabolic_rate_w(m(20.0), Ectotherm) < basal_metabolic_rate_w(m(20.0), Endotherm)
        );
    }

    #[test]
    fn tempo_slows_with_mass() {
        assert!(
            reproductive_tempo(m(132.0), Endotherm, LifeSchedule::ALLOMETRIC)
                > reproductive_tempo(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC)
        );
    }

    #[test]
    fn ametabolic_nulls_the_biological_traits() {
        let lh = life_history(m(500.0), Ametabolic, LifeSchedule::ALLOMETRIC);
        assert_eq!(lh.basal_metabolic_rate_w, 0.0);
        assert!(lh.lifespan.is_none());
        assert!(lh.age_at_maturity.is_none());
        assert!(lh.reproductive_tempo.is_none());
        assert!(lh.generation_length.is_none());
    }

    #[test]
    fn living_classes_fill_every_trait() {
        let lh = life_history(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC);
        assert!(lh.lifespan.is_some() && lh.generation_length.is_some());
        // generation length sits between maturity and lifespan
        // (`gen` is a reserved keyword in edition 2024 — see `gen` blocks)
        let generation = lh.generation_length.unwrap().get();
        assert!(generation > lh.age_at_maturity.unwrap().get());
        assert!(generation < lh.lifespan.unwrap().get());
    }

    #[test]
    fn pace_of_life_is_roster_independent() {
        // pace depends only on this species' own mass+class, not on any registry
        // state — computing it twice (as if the roster changed) is identical.
        let a = life_history(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC).pace_of_life;
        let b = life_history(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC).pace_of_life;
        assert_eq!(a, b);
        // and it is monotone in mass
        assert!(
            life_history(m(132.0), Endotherm, LifeSchedule::ALLOMETRIC).pace_of_life
                > life_history(m(18.0), Endotherm, LifeSchedule::ALLOMETRIC).pace_of_life
        );
    }

    #[test]
    fn the_default_schedule_reproduces_the_bare_allometry_bit_for_bit() {
        // THE LONG AGE: the campaign's whole null rests on this. A factor of
        // exactly 1.0 is an IEEE-754 no-op, so `Allometric` must not merely
        // be close to the old law -- it must be the same bits.
        for kg in [5.0, 18.1, 55.0, 70.0, 132.0, 2200.0, 6000.0] {
            for class in [Endotherm, Ectotherm, Autotroph, Ametabolic] {
                let lh = life_history(m(kg), class, LifeSchedule::ALLOMETRIC);
                assert_eq!(
                    lh.pace_of_life.to_bits(),
                    pace_of_life_bare_reference(m(kg), class).to_bits(),
                    "pace_of_life moved at {kg} kg / {class:?}"
                );
            }
        }
    }

    /// The pre-campaign expression, inlined verbatim as the witness the
    /// bit-identity test compares against.
    fn pace_of_life_bare_reference(mass: Mass, class: MetabolicClass) -> f64 {
        let raw = (math::log10(mass.kilograms()) / 3.0).clamp(0.0, 1.0);
        (raw * pace_multiplier(class) / MAX_PACE_MULTIPLIER).clamp(0.0, 1.0)
    }

    #[test]
    fn a_paced_schedule_lengthens_life_and_maturity_together() {
        let slow = LifeSchedule::paced(11.0).expect("11.0 is a valid factor");
        let base_life = lifespan(m(60.0), Endotherm, LifeSchedule::ALLOMETRIC).get();
        let base_mat = age_at_maturity(m(60.0), Endotherm, LifeSchedule::ALLOMETRIC).get();
        let slow_life = lifespan(m(60.0), Endotherm, slow).get();
        let slow_mat = age_at_maturity(m(60.0), Endotherm, slow).get();
        assert!(
            (slow_life / base_life - 11.0).abs() < 1e-9,
            "lifespan scales by the factor"
        );
        assert!(
            (slow_mat / base_mat - 11.0).abs() < 1e-9,
            "maturity scales with it"
        );
    }

    #[test]
    fn a_paced_schedule_never_moves_the_metabolic_rate() {
        // The clock's oscillator is mass-set; only the gear train is authored.
        // A long-lived creature is not a cold creature.
        let slow = LifeSchedule::paced(11.0).expect("11.0 is a valid factor");
        assert_eq!(
            life_history(m(60.0), Endotherm, slow)
                .basal_metabolic_rate_w
                .to_bits(),
            life_history(m(60.0), Endotherm, LifeSchedule::ALLOMETRIC)
                .basal_metabolic_rate_w
                .to_bits()
        );
    }

    #[test]
    fn pace_of_life_saturates_rather_than_rescaling_the_class_ceiling() {
        // Deliberate (spec 3.5): MAX_PACE_MULTIPLIER governs the CLASS
        // component only, and an authored factor above it saturates. Raising
        // the ceiling instead would move pace_of_life for all thirty kinds.
        let slow = LifeSchedule::paced(11.0).expect("11.0 is a valid factor");
        assert_eq!(life_history(m(60.0), Endotherm, slow).pace_of_life, 1.0);
    }

    #[test]
    fn a_nonpositive_or_nonfinite_factor_is_refused() {
        assert!(LifeSchedule::paced(0.0).is_none());
        assert!(LifeSchedule::paced(-1.0).is_none());
        assert!(LifeSchedule::paced(f64::NAN).is_none());
        assert!(LifeSchedule::paced(f64::INFINITY).is_none());
        assert!(LifeSchedule::paced(1.0).is_some());
    }

    #[test]
    fn ametabolic_still_nulls_the_biological_traits_under_any_schedule() {
        let slow = LifeSchedule::paced(11.0).expect("11.0 is a valid factor");
        let lh = life_history(m(500.0), Ametabolic, slow);
        assert!(lh.lifespan.is_none());
        assert!(lh.generation_length.is_none());
    }
}
