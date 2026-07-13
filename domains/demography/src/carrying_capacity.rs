//! The carrying-capacity field K: a closed-form, seed-free people-density a
//! cell can support, grounded in a Miami-model net-primary-productivity proxy
//! (Lieth) plus freshwater, coast, and aridity terms. All constants are
//! calibrated once (the-gathering plan Task 8) and then frozen as save-format
//! constants.

use hornvale_kernel::{CellId, CellMap, Geosphere};

/// The bare per-cell climate/terrain inputs the composition root assembles.
/// Demography never imports those domains; it sees only this.
/// type-audit: bare-ok(flag: habitable), pending(wave-3: temperature_c), bare-ok(ratio: moisture), bare-ok(ratio: freshwater), bare-ok(flag: coastal), bare-ok(ratio: hostility)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CarryingInput {
    /// Whether the cell is habitable (land, water, tolerable season).
    pub habitable: bool,
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

// Calibrated constants (PLACEHOLDER until Task 8; frozen thereafter).
const BASE: f64 = 1.0;
const TEMP_OPTIMUM_C: f64 = 22.0;
const TEMP_TOLERANCE_C: f64 = 20.0;
const FRESHWATER_BONUS: f64 = 0.5;
const COAST_BONUS: f64 = 0.2;

/// Temperature response in `[0,1]`: a triangular tolerance around the optimum.
fn temp_response(t: f64) -> f64 {
    (1.0 - (t - TEMP_OPTIMUM_C).abs() / TEMP_TOLERANCE_C).clamp(0.0, 1.0)
}

/// The carrying-capacity field: `0.0` on uninhabitable cells, else the NPP
/// proxy scaled by freshwater, coast, and hostility terms.
/// type-audit: bare-ok(count: return)
pub fn carrying_capacity(geo: &Geosphere, inputs: &CellMap<CarryingInput>) -> CellMap<f64> {
    CellMap::from_fn(geo, |c: CellId| {
        let i = inputs.get(c);
        if !i.habitable {
            return 0.0;
        }
        // Miami NPP proxy: Liebig minimum of temperature and moisture responses.
        let npp = temp_response(i.temperature_c).min(i.moisture.clamp(0.0, 1.0));
        let bonus = 1.0
            + FRESHWATER_BONUS * i.freshwater.clamp(0.0, 1.0)
            + if i.coastal { COAST_BONUS } else { 0.0 };
        let k = BASE * npp * bonus * (1.0 - i.hostility.clamp(0.0, 1.0));
        k.max(0.0)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn input(hab: bool, t: f64, m: f64, fw: f64, coast: bool, host: f64) -> CarryingInput {
        CarryingInput {
            habitable: hab,
            temperature_c: t,
            moisture: m,
            freshwater: fw,
            coastal: coast,
            hostility: host,
        }
    }

    #[test]
    fn uninhabitable_is_zero_and_wet_temperate_beats_desert() {
        let geo = Geosphere::new(2);
        // cell 0 uninhabitable, 1 wet-temperate, 2 hot desert.
        let inputs = CellMap::from_fn(&geo, |c| match c.0 {
            0 => input(false, 15.0, 0.8, 0.9, true, 0.0),
            1 => input(true, 15.0, 0.8, 0.9, true, 0.0),
            _ => input(true, 40.0, 0.05, 0.05, false, 0.8),
        });
        let k = carrying_capacity(&geo, &inputs);
        assert_eq!(*k.get(CellId(0)), 0.0, "uninhabitable supports nobody");
        assert!(
            *k.get(CellId(1)) > *k.get(CellId(2)),
            "wet-temperate beats desert"
        );
        assert!(*k.get(CellId(1)) >= 0.0 && *k.get(CellId(2)) >= 0.0);
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
            *k.get(CellId(0)) < *k.get(CellId(1)),
            "the scarce factor caps K"
        );
    }
}
