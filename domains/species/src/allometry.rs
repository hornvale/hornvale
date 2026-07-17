//! Life-history allometry (BIO-2): pure scaling laws deriving a species'
//! life-history from body mass and metabolic class. Universal exponents;
//! per-class coefficients. No draws, no world state — see the design spec.

use crate::MetabolicClass;
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

/// The largest value `pace_multiplier` returns (the ectotherm shift). Naming it
/// keeps the `pace_of_life` normalizer provably coupled to it: if a future
/// class needs a larger multiplier, this constant — and the ceiling it defines
/// — must move with it, rather than silently exceeding 1.0 and being masked by
/// the clamp.
const MAX_PACE_MULTIPLIER: f64 = 1.5;

/// Single per-class pace multiplier: shifts lifespan, maturity, and tempo
/// together so the fast–slow covariation stays coherent (spec §4). Ectotherms
/// are slower on every axis at once.
fn pace_multiplier(class: MetabolicClass) -> f64 {
    match class {
        MetabolicClass::Endotherm => 1.0,
        MetabolicClass::Ectotherm => MAX_PACE_MULTIPLIER,
        MetabolicClass::Autotroph => 1.0,
        // Ametabolic never reaches the time laws (handled in life_history).
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
/// anchor; the per-class pace multiplier lengthens ectotherm life.
pub fn lifespan(mass: Mass, class: MetabolicClass) -> Years {
    let k_life = ANCHOR_LIFESPAN_YR / math::powf(ANCHOR_MASS_KG, P_TIME);
    let yr = pace_multiplier(class) * k_life * math::powf(mass.kilograms(), P_TIME);
    Years::new(yr).expect("mass is positive, so lifespan is finite and non-negative")
}

/// Age at first reproduction (spec §4), ~20 % of lifespan at the anchor.
pub fn age_at_maturity(mass: Mass, class: MetabolicClass) -> Years {
    let k_mat = ANCHOR_MATURITY_YR / math::powf(ANCHOR_MASS_KG, P_TIME);
    let yr = pace_multiplier(class) * k_mat * math::powf(mass.kilograms(), P_TIME);
    Years::new(yr).expect("mass is positive, so maturity is finite and non-negative")
}

/// Reproductive tempo on the r–K axis, 0 (fast/prolific) … 1 (slow/sparse),
/// rising with mass and the pace multiplier (spec §4/CAP-2 — this is
/// reproductive OUTPUT, distinct from overall pace-of-life). A saturating map
/// of `pace_multiplier · log10(mass)` over a fixed reference range keeps it
/// absolute (roster-independent).
/// type-audit: bare-ok(ratio: return)
pub fn reproductive_tempo(mass: Mass, class: MetabolicClass) -> f64 {
    // Fixed reference range: 1 kg → ~0, 1000 kg → ~1 (before the class shift).
    let raw = (math::log10(mass.kilograms()) / 3.0).clamp(0.0, 1.0);
    (raw * pace_multiplier(class)).clamp(0.0, 1.0)
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
/// shifts another's value. Larger/slower → 1.
fn pace_of_life(mass: Mass, class: MetabolicClass) -> f64 {
    let raw = (math::log10(mass.kilograms()) / 3.0).clamp(0.0, 1.0);
    // Ectotherms read slower on the same size.
    (raw * pace_multiplier(class) / MAX_PACE_MULTIPLIER).clamp(0.0, 1.0)
}

/// Assemble the full life-history profile (spec §5).
pub fn life_history(mass: Mass, class: MetabolicClass) -> LifeHistory {
    let bmr = basal_metabolic_rate_w(mass, class);
    let pace = pace_of_life(mass, class);
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
    let life = lifespan(mass, class);
    let mat = age_at_maturity(mass, class);
    let generation = Years::new(mat.get() + GENERATION_FRACTION * (life.get() - mat.get()))
        .expect("maturity ≤ lifespan, so generation length is non-negative");
    LifeHistory {
        basal_metabolic_rate_w: bmr,
        lifespan: Some(life),
        age_at_maturity: Some(mat),
        reproductive_tempo: Some(reproductive_tempo(mass, class)),
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
        assert!((lifespan(m(40.0), Endotherm).get() - 60.0).abs() < 1e-6);
        assert!((age_at_maturity(m(40.0), Endotherm).get() - 12.0).abs() < 1e-6);
    }

    #[test]
    fn lifespan_and_maturity_increase_with_mass() {
        assert!(lifespan(m(132.0), Endotherm).get() > lifespan(m(18.0), Endotherm).get());
        assert!(
            age_at_maturity(m(132.0), Endotherm).get() > age_at_maturity(m(18.0), Endotherm).get()
        );
    }

    #[test]
    fn ectotherms_outlive_endotherms_at_equal_mass() {
        assert!(lifespan(m(20.0), Ectotherm).get() > lifespan(m(20.0), Endotherm).get());
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
        assert!(reproductive_tempo(m(132.0), Endotherm) > reproductive_tempo(m(18.0), Endotherm));
    }

    #[test]
    fn ametabolic_nulls_the_biological_traits() {
        let lh = life_history(m(500.0), Ametabolic);
        assert_eq!(lh.basal_metabolic_rate_w, 0.0);
        assert!(lh.lifespan.is_none());
        assert!(lh.age_at_maturity.is_none());
        assert!(lh.reproductive_tempo.is_none());
        assert!(lh.generation_length.is_none());
    }

    #[test]
    fn living_classes_fill_every_trait() {
        let lh = life_history(m(18.0), Endotherm);
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
        let a = life_history(m(18.0), Endotherm).pace_of_life;
        let b = life_history(m(18.0), Endotherm).pace_of_life;
        assert_eq!(a, b);
        // and it is monotone in mass
        assert!(
            life_history(m(132.0), Endotherm).pace_of_life
                > life_history(m(18.0), Endotherm).pace_of_life
        );
    }
}
