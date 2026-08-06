//! The carrying-capacity field K: a closed-form, seed-free people-density a
//! cell can support, grounded in a Miami-model net-primary-productivity proxy
//! (Lieth) plus freshwater, coast, and aridity terms. All constants were
//! calibrated once (the-gathering, Task 8) against the real biomass-by-
//! latitude gradient and are now frozen as save-format constants.

use hornvale_kernel::ecology::CapacityMap;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The bare per-cell climate/terrain inputs the composition root assembles.
/// Demography never imports those domains; it sees only this.
/// type-audit: bare-ok(flag: is_land), pending(wave-3: temperature_c), bare-ok(diagnostic-value: precip_mm_yr), bare-ok(ratio: freshwater), bare-ok(flag: coastal), bare-ok(ratio: hostility)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CarryingInput {
    /// Whether the cell is dry land (elevation at or above sea level).
    ///
    /// **This is the ONLY hard gate on productivity** (decision 0103's
    /// decomposition applied to a conflated flag). It was `habitable`, carrying
    /// `is_habitable`'s three conjuncts at once — land AND a temperate band AND
    /// a moisture floor — which made a hard cut out of two predicates that the
    /// formula below *already grades smoothly*: `npp` is `min(temp_response,
    /// moisture)` and aridity rides `hostility`. Only land genuinely admits no
    /// gradient, so only land stays a flag.
    ///
    /// The deeper reason the old name was wrong: **habitability is a relation
    /// between a species and a location, not a property of the location alone.**
    /// Whether ground is dry IS a property of the ground, so it belongs here as a
    /// flag; whether ground is *livable* depends on who is asking, so it belongs
    /// in the per-species suitability term and nowhere else.
    pub is_land: bool,
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Annual precipitation, mm/yr. **Not** normalised moisture: Lieth's
    /// precipitation term is defined on a real total, and `climate.precip_at`
    /// already supplies one (`2000 · m^1.5`, Earth-ranged, provenance cited to
    /// the spec's model card). Taking the real total rather than re-deriving a
    /// second conversion is decision 0105 applied — there was never a constant
    /// here to author.
    pub precip_mm_yr: f64,
    /// Freshwater availability in `[0, 1]` (drainage/moisture, at root).
    pub freshwater: f64,
    /// Whether the cell borders the ocean.
    pub coastal: bool,
    /// Hostility in `[0, 1]` (aridity, tectonic unrest).
    pub hostility: f64,
}

// CALIBRATED (the-gathering, 2026-07-13): measured against the 200-seed
// `studies/census-of-the-gathering.study.json` census via the Lab's
// `capacity-by-abs-latitude` metric (design spec §5's headline calibration —
// mean per-land-cell K in the |latitude| < 30 band over the |latitude| > 60
// band, polar band floored at 1% of the baseline unit against an exact
// zero). The authored values below (drafted from the Miami/Lieth model's
// textbook tropical optimum, never a fit) already reproduce the real
// biomass-by-latitude gradient decisively — mean 27.15 across the 200 seeds
// (41/200 individual worlds read below the preregistered floor of 3, all
// barren/marginal worlds with little land in EITHER band, not a failure of
// the gradient itself) — so no adjustment was needed; frozen as measured,
// not as a placeholder. A save-format constant from here on.
/// kind: **hornvale-gauge** (decision 0105). Occurs exactly once, as a
/// multiplicative factor, and any change is absorbed downstream by
/// `SETTLERS_PER_CAPACITY` — so its value is unobservable and needs no source,
/// only this note, so nobody later "calibrates" an unmeasurable quantity.
const BASE: f64 = 1.0;
const FRESHWATER_BONUS: f64 = 0.5;
const COAST_BONUS: f64 = 0.2;

/// Lieth's Miami-model temperature term, normalised to its 3000 g/m²/yr
/// asymptote so `BASE` keeps meaning "one unit of productivity".
///
/// kind: **earth-biosphere** (decision 0105). Cited: Lieth & Box (1972), the
/// Miami model — `NPP_temp = 3000 / (1 + exp(1.315 − 0.119 T))`. The citation
/// and the implementation now agree, which they did not before (The Keeping):
/// the retired `temp_response` was a symmetric tent peaking at 22 °C, and this
/// is **monotone increasing and saturating with no optimum at all**. Above 22 °C
/// the two moved in opposite directions, and the tent reached *exactly zero* a
/// little above freezing where Lieth predicts ~26% of maximum — which is why no
/// world was ever inhabited cold.
///
/// A tent *is* a meaningful curve; it is a **tolerance** curve, and tolerance
/// belongs per-species in `ConditionNiche`, never in a species-blind
/// productivity field. Productivity is a property of ground; habitability is a
/// relation between a species and a place.
fn npp_temperature(t_c: f64) -> f64 {
    1.0 / (1.0 + hornvale_kernel::math::exp(1.315 - 0.119 * t_c))
}

/// Lieth's Miami-model precipitation term, normalised to the same asymptote.
///
/// kind: **earth-biosphere** (decision 0105). Cited: Lieth & Box (1972) —
/// `NPP_precip = 3000 · (1 − exp(−0.000664 P))`, `P` in mm/yr. Saturating: wetter
/// ground keeps helping, with diminishing return, and never turns harmful.
fn npp_precipitation(precip_mm_yr: f64) -> f64 {
    1.0 - hornvale_kernel::math::exp(-0.000664 * precip_mm_yr.max(0.0))
}

/// The carrying-capacity field: `0.0` at sea, else the NPP proxy scaled by
/// freshwater, coast, and hostility terms.
///
/// **Land is the only hard gate.** Temperature and moisture grade to zero on
/// their own (`npp = min(temp_response, moisture)`, zero outside 2–42 °C and at
/// zero moisture; aridity rides `hostility`), so the arid and very-hot bands the
/// old `habitable` flag excluded outright are now *reachable at low capacity*
/// rather than forbidden. The cold is still closed, by `temp_response` rather
/// than by any flag — see [`temp_response`].
///
/// Returns a [`CapacityMap`] — a people-DENSITY with units — rather than a bare
/// `CellMap<f64>`, per decision 0103. The type is what stops this field being
/// interchanged with a dimensionless suitability, which is a 20–100× silent
/// rescale that no guard in the workspace previously caught.
pub fn carrying_capacity(geo: &Geosphere, inputs: &CellMap<CarryingInput>) -> CapacityMap {
    let raw = CellMap::from_fn(geo, |c: CellId| {
        let i = inputs.get(c);
        if !i.is_land {
            return 0.0;
        }
        // Liebig's law of the minimum over Lieth's two terms — the one feature
        // of the Miami model that survived the original translation, and the one
        // this campaign keeps unchanged.
        let npp = npp_temperature(i.temperature_c).min(npp_precipitation(i.precip_mm_yr));
        let bonus = 1.0
            + FRESHWATER_BONUS * i.freshwater.clamp(0.0, 1.0)
            + if i.coastal { COAST_BONUS } else { 0.0 };
        let k = BASE * npp * bonus * (1.0 - i.hostility.clamp(0.0, 1.0));
        k.max(0.0)
    });
    // `k.max(0.0)` above establishes the invariant the constructor validates, so
    // this cannot fail; the constructor is still the only way in, so a future
    // change to the formula is caught here rather than downstream.
    CapacityMap::new(raw).expect("carrying capacity is non-negative and finite by construction")
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// `p_mm` is annual precipitation in mm/yr (see [`CarryingInput`]), not a
    /// normalised moisture — the fixtures below quote real totals so the Lieth
    /// terms are exercised on the range they are defined for.
    fn input(land: bool, t: f64, p_mm: f64, fw: f64, coast: bool, host: f64) -> CarryingInput {
        CarryingInput {
            is_land: land,
            temperature_c: t,
            precip_mm_yr: p_mm,
            freshwater: fw,
            coastal: coast,
            hostility: host,
        }
    }

    #[test]
    fn sea_is_zero_and_wet_temperate_beats_desert() {
        let geo = Geosphere::new(2);
        // cell 0 at sea, 1 wet-temperate land, 2 hot desert land.
        let inputs = CellMap::from_fn(&geo, |c| match c.0 {
            0 => input(false, 15.0, 1400.0, 0.9, true, 0.0),
            1 => input(true, 15.0, 1400.0, 0.9, true, 0.0),
            _ => input(true, 40.0, 20.0, 0.05, false, 0.8),
        });
        let k = carrying_capacity(&geo, &inputs);
        assert_eq!(k.at(CellId(0)), 0.0, "the sea supports no settlers");
        assert!(
            k.at(CellId(1)) > k.at(CellId(2)),
            "wet-temperate beats desert"
        );
        assert!(k.at(CellId(1)) >= 0.0 && k.at(CellId(2)) >= 0.0);
    }

    #[test]
    fn npp_proxy_is_liebig_minimum_of_temperature_and_moisture() {
        let geo = Geosphere::new(2);
        // Warm+dry and cool+wet should both be limited by their scarce factor.
        let inputs = CellMap::from_fn(&geo, |c| match c.0 {
            0 => input(true, 25.0, 60.0, 0.5, false, 0.0), // water-limited
            _ => input(true, 25.0, 1700.0, 0.5, false, 0.0), // ample both
        });
        let k = carrying_capacity(&geo, &inputs);
        assert!(
            k.at(CellId(0)) < k.at(CellId(1)),
            "the scarce factor caps K"
        );
    }
}
