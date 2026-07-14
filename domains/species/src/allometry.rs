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

/// Single per-class pace multiplier: shifts lifespan, maturity, and tempo
/// together so the fast–slow covariation stays coherent (spec §4). Ectotherms
/// are slower on every axis at once.
fn pace_multiplier(class: MetabolicClass) -> f64 {
    match class {
        MetabolicClass::Endotherm => 1.0,
        MetabolicClass::Ectotherm => 1.5,
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
}
