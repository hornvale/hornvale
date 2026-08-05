//! The carrying-capacity field K: a closed-form, seed-free people-density a
//! cell can support, grounded in a Miami-model net-primary-productivity proxy
//! (Lieth) plus freshwater, coast, and aridity terms. All constants were
//! calibrated once (the-gathering, Task 8) against the real biomass-by-
//! latitude gradient and are now frozen as save-format constants.

use hornvale_kernel::ecology::CapacityMap;
use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The bare per-cell climate/terrain inputs the composition root assembles.
/// Demography never imports those domains; it sees only this.
/// type-audit: bare-ok(flag: is_land), pending(wave-3: temperature_c), bare-ok(ratio: moisture), bare-ok(ratio: freshwater), bare-ok(flag: coastal), bare-ok(ratio: hostility)
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
    /// Moisture in `[0, 1]`.
    pub moisture: f64,
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
const BASE: f64 = 1.0;
const TEMP_OPTIMUM_C: f64 = 22.0;
const TEMP_TOLERANCE_C: f64 = 20.0;
const FRESHWATER_BONUS: f64 = 0.5;
const COAST_BONUS: f64 = 0.2;

/// Temperature response in `[0,1]`: a triangular tolerance around the optimum.
fn temp_response(t: f64) -> f64 {
    (1.0 - (t - TEMP_OPTIMUM_C).abs() / TEMP_TOLERANCE_C).clamp(0.0, 1.0)
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
        // Miami NPP proxy: Liebig minimum of temperature and moisture responses.
        let npp = temp_response(i.temperature_c).min(i.moisture.clamp(0.0, 1.0));
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

    fn input(land: bool, t: f64, m: f64, fw: f64, coast: bool, host: f64) -> CarryingInput {
        CarryingInput {
            is_land: land,
            temperature_c: t,
            moisture: m,
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
            0 => input(false, 15.0, 0.8, 0.9, true, 0.0),
            1 => input(true, 15.0, 0.8, 0.9, true, 0.0),
            _ => input(true, 40.0, 0.05, 0.05, false, 0.8),
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
            0 => input(true, 25.0, 0.1, 0.5, false, 0.0), // moisture-limited
            _ => input(true, 25.0, 0.9, 0.5, false, 0.0), // ample both
        });
        let k = carrying_capacity(&geo, &inputs);
        assert!(
            k.at(CellId(0)) < k.at(CellId(1)),
            "the scarce factor caps K"
        );
    }
}
