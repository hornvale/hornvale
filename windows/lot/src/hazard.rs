//! A Siler mortality hazard in scaled age (spec §4.3): infant, background and
//! senescent terms, its age scale set by a people's allometric lifespan and
//! its background scaled by the site's strife. Deterministic and libm-only;
//! nothing here draws.

use hornvale_kernel::math::exp;

/// The lifespan at which scaled age equals real age: the allometric anchor
/// (a 40 kg endotherm reads 60 years).
/// plumb: universal(the allometric lifespan anchor in domains/species, not a lot-specific value)
/// type-audit: bare-ok(count)
pub const HUMAN_ANCHOR_YEARS: f64 = 60.0;
/// Infant term amplitude per scaled year.
/// plumb: universal(authored calibration, spec 2026-09-05-the-lot section 4.3, frozen before unblinding)
const A1: f64 = 0.35;
/// Infant term decay per scaled year.
/// plumb: universal(authored calibration, spec 2026-09-05-the-lot section 4.3, frozen before unblinding)
const B1: f64 = 1.0;
/// Background hazard per scaled year at zero strife.
/// plumb: universal(authored calibration, spec 2026-09-05-the-lot section 4.3, frozen before unblinding)
const A2: f64 = 0.012;
/// Senescent term amplitude per scaled year.
/// plumb: universal(authored calibration, spec 2026-09-05-the-lot section 4.3, frozen before unblinding)
const A3: f64 = 1.0e-4;
/// Senescent doubling rate: ln 2 over an 8-scaled-year mortality doubling time.
/// plumb: universal(authored calibration, spec 2026-09-05-the-lot section 4.3, frozen before unblinding)
const B3: f64 = std::f64::consts::LN_2 / 8.0;

/// The two inputs a people-at-a-site contributes.
/// type-audit: bare-ok(count: lifespan_years), bare-ok(ratio: strife)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Hazard {
    /// The people's allometric lifespan, in years.
    pub lifespan_years: f64,
    /// The site's strife in `[0, 1]`.
    pub strife: f64,
}

impl Hazard {
    /// Scaled age per real year.
    fn scale(&self) -> f64 {
        HUMAN_ANCHOR_YEARS / self.lifespan_years
    }
    /// The hazard at scaled age `u`, per scaled year.
    fn at_scaled(&self, u: f64) -> f64 {
        A1 * exp(-B1 * u) + A2 * (1.0 + self.strife) + A3 * exp(B3 * u)
    }
}

/// Survival `S(x)` at every integer REAL year from 0 to `2 × lifespan`,
/// by trapezoidal integration of the hazard at one-scaled-year steps.
/// `S(0) = 1`; nonincreasing.
/// type-audit: bare-ok(ratio: return)
pub fn survival_table(h: &Hazard) -> Vec<f64> {
    let years = (2.0 * h.lifespan_years).ceil() as usize;
    let k = h.scale();
    let mut table = Vec::with_capacity(years + 1);
    let mut cumulative = 0.0;
    table.push(1.0);
    for year in 1..=years {
        let u0 = (year as f64 - 1.0) * k;
        let u1 = year as f64 * k;
        cumulative += 0.5 * (h.at_scaled(u0) + h.at_scaled(u1)) * (u1 - u0);
        table.push(exp(-cumulative));
    }
    table
}

/// Life expectancy at birth, in real years: the area under `S`.
/// type-audit: bare-ok(count: return)
pub fn e0(h: &Hazard) -> f64 {
    let t = survival_table(h);
    t.windows(2).map(|w| 0.5 * (w[0] + w[1])).sum()
}

/// The probability of dying before real age `age`.
/// type-audit: bare-ok(count: age), bare-ok(ratio: return)
pub fn q_before(h: &Hazard, age: f64) -> f64 {
    let t = survival_table(h);
    let i = age.floor().clamp(0.0, (t.len() - 1) as f64) as usize;
    1.0 - t[i]
}

/// The age at death for a uniform `u ∈ (0, 1]`: the first real year at
/// which survival falls to `u`, with linear interpolation inside that year.
/// `u = 1` returns `0.0`; a `u` below `S(2L)` returns `2L`.
/// type-audit: bare-ok(ratio: u), bare-ok(count: return)
pub fn death_age(h: &Hazard, u: f64) -> f64 {
    let t = survival_table(h);
    if u >= 1.0 {
        return 0.0;
    }
    for i in 1..t.len() {
        if t[i] <= u {
            let span = t[i - 1] - t[i];
            let frac = if span > 0.0 {
                (t[i - 1] - u) / span
            } else {
                0.0
            };
            return (i - 1) as f64 + frac;
        }
    }
    (t.len() - 1) as f64
}
