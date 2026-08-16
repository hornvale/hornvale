//! Temperature over the globe (canonical °C; the kernel's `Temperature`
//! newtype is the typed boundary — see `hornvale_kernel::Temperature`).
//! Spinning worlds: an insolation baseline that falls with latitude and with
//! elevation (lapse rate), plus a hemisphere-signed seasonal swing set by
//! obliquity and damped near oceans. Tidally locked worlds: temperature is
//! organized around the substellar point (`+x`), hottest there and coldest
//! at the antistellar point. Declared approximations (spec §5 model card):
//! no ocean currents, smooth-sinusoid seasons, prograde-only.

use crate::circulation::RotationRegime;
use crate::diurnal::{diurnal_amplitude, diurnal_anomaly};
use hornvale_kernel::math;
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, TempAnomaly, Temperature};

/// Standard (ICAO) environmental lapse rate: °C lost per meter of elevation
/// above sea level. **Not** the dry adiabatic rate (9.8 K/km) — 6.5 K/km is
/// the observed/standard-atmosphere rate, and the two are not
/// interchangeable; see `docs/audits/land-elevation-attribution.md` §3.5,
/// which uses this same rate and explicitly disclaims 9.8 K/km.
///
/// kind: **physics** (decision 0106). Cited: ICAO/US Standard Atmosphere
/// 1976, troposphere lapse rate.
const LAPSE_C_PER_M: f64 = 6.5 / 1000.0;

/// The carbonate–silicate thermostat's **residual** fraction, `k`: how much
/// of a world's insolation deviation from Earth's (`S = 1`) the thermostat
/// FAILS to compensate before the ordinary blackbody `S^(1/4)` response is
/// applied (`Spinning` regime only; `Locked` keeps its own, unrelated `0.25`
/// exponent applied directly to `S` — see that branch, and
/// `greenhouse_forcing_k`'s doc comment for what this leaves disagreeing
/// between the two regimes). `k = 1.0` is no thermostat at all (the
/// pre-Glasshouse fixed-atmosphere model, full blackbody sensitivity to raw
/// `S`); `k = 0.0` is a perfect thermostat (temperature independent of
/// insolation). Implemented as `effective_S = 1 + k·(S − 1)`, then
/// `T_eq ∝ effective_S^(1/4)`.
///
/// kind: **physics** (decision 0106; re-decided by The Glasshouse). A
/// carbonate–silicate weathering thermostat (Walker, Hays & Kasting 1981)
/// draws CO₂ down as a world warms and lets outgassing rebuild it as a world
/// cools, DAMPING equilibrium temperature's sensitivity to insolation
/// relative to a fixed atmosphere — this is the mechanism the classic
/// habitable-zone width (0.53–1.11 `S`, `domains/astronomy/src/star.rs`)
/// already assumes: a planet at the outer edge is habitable only because
/// something keeps it from freezing solid at low insolation, and the
/// pre-Glasshouse fixed-288K-atmosphere model had no such mechanism, which is
/// exactly why it dominated the fixed population with ice
/// (`docs/superpowers/plans/2026-08-13-the-glasshouse-b-recentring.md`
/// Architecture). The thermostat is imperfect (weathering responds slowly,
/// and the feedback saturates near the zone edges), so `k` is a partial
/// compensation, not `0.0`.
///
/// **READ THE NAME CAREFULLY: this is the fraction that SURVIVES, not the
/// fraction that is compensated.** `0.3` means the thermostat removes 70% of
/// the insolation anomaly and 30% reaches equilibrium temperature. The
/// constant was called `THERMOSTAT_COMPENSATION_FRACTION` until this value
/// was re-decided, under which name every reader would infer the opposite;
/// the rename is the fix, and re-inverting it later would reintroduce the
/// confusion rather than resolve it.
///
/// **Its provenance is a claim about EARTH, deliberately, because no claim
/// about the census could ever pin it.** The plan asked for `k` to be fixed
/// from Earth rather than from the population, and that turned out to be
/// impossible as literally written: the model is `effective_S = 1 + k(S − 1)`,
/// so at `S = 1` the `k` term **vanishes identically** — and `S = 1` is
/// exactly where Earth's anchor sits. A single anchor point can pin a
/// LOCATION ([`THERMOSTAT_ANCHOR_K`] is pinned that way, correctly) but never
/// a SLOPE. `k` is a slope, so it needs a second point at `S ≠ 1`.
///
/// The second point is the faint young Sun, inverted. At `S = 0.75` (≈4 Gyr
/// ago) this model's area-weighted mean is `287.15·(1 − 0.25k)^0.25 − 273.15`,
/// so **choosing `k` IS choosing an Archean global mean temperature**:
///
/// ```text
///   k      Archean mean at S = 0.75      r(S, T) over the population
///   0.10           +12.19 °C                  −0.000   <- insolation stops mattering
///   0.20           +10.34 °C                  +0.129
///   0.30            +8.46 °C                  +0.257   <- chosen
///   0.40            +6.54 °C                  +0.376
///   0.725            0.00 °C                           <- surface water freezes: hard ceiling
/// ```
///
/// **`0.3` asserts that Earth's Archean global mean was ≈ +8.5 °C** — a
/// temperate early Earth, inside the (genuinely contested) literature range
/// and comfortably clear of both the frozen-Archean ceiling and a boiling
/// one. That claim is citable and falsifiable *about Earth*, which is the
/// property a swept preference never had; anyone who disputes this constant
/// should argue paleoclimate, not Hornvale's census median.
///
/// The lower bound is not physics but this project's own thesis. `r(S, T)`
/// reaches **zero at `k ≈ 0.10`**: drive `k` low enough and a world's orbit
/// stops influencing its climate at all, which is the very pathology
/// (spec §2.3, `r(L,T) = +0.013`) this campaign exists to remove, merely
/// relocated from the star to the orbit. Low `k` does not buy free warmth; it
/// buys warmth by severing climate from sky.
///
/// **`k` IS NOT THE DOMINANT LEVER, and the next person to reach for it
/// should know that first.** Measured over 191 spinning worlds: across `k`'s
/// entire range, 0.4 → 0.0 (perfect compensation), the spinning median land
/// temperature moves 7.2 K and the share of worlds with sub-freezing land
/// falls only 64% → **41%**. Two-fifths stay cold under a *perfect*
/// thermostat, because the population sits at a median `S` of 0.748 by
/// construction: the zone is `[0.95, 1.37]·√L`, the orbit is drawn uniform in
/// RADIUS across it, and `S = L/a²`, so `L` cancels exactly and
/// `S = 1/(0.95 + 0.42t)²` for a uniform `t`. The cold is a property of that
/// draw's MEASURE, not of this constant.
///
/// See also [`crate::provider::ClimateInputs::greenhouse_forcing_k`], the
/// ADDITIVE residual this thermostat's own spread draws around.
const THERMOSTAT_RESIDUAL_FRACTION: f64 = 0.3;

/// The thermostat's anchor temperature, kelvin: the `Spinning`-regime
/// equilibrium base at `S = 1` (before the latitude profile and lapse
/// cooling are applied).
///
/// kind: **earth-biosphere** (decision 0106). Earth's global-mean annual
/// surface temperature is ≈14 °C / 287.15 K (NASA GISS surface temperature
/// analysis; IPCC AR6 WG1 gives a comparable figure) — the same datum
/// `crate::provider::TEMPERATE_BASELINE_C` already anchors the felt-weather
/// baseline to. Because [`crate::temperature::mean_temperature`]'s latitude
/// term is constructed to have **zero area-weighted mean** (Task 5, spec
/// §3.2), this constant alone fixes the model's global area-weighted mean at
/// `S = 1` — it is the "+14 °C" anchor, not the "+8.6 °C land mean" anchor
/// (that figure falls out of this constant plus the post-craton-rescale
/// hypsometry's land-only lapse cooling, not a second free parameter).
const THERMOSTAT_ANCHOR_K: f64 = 287.15;

/// The latitude profile's value at the equator (`sin(lat) = 0`), °C offset
/// from [`THERMOSTAT_ANCHOR_K`].
///
/// kind: **earth-biosphere** (decision 0106). Solved as one of three
/// simultaneous constraints together with [`LAT_TERM_SIN2_COEFF_C`] and
/// [`LAT_TERM_SIN4_COEFF_C`] (spec §3.2, Task 5): the profile's
/// area-weighted mean over the whole sphere is exactly zero (⟨sin²⟩ = 1/3,
/// ⟨sin⁴⟩ = 1/5 for the `cos(lat)` area element — spec's own identity), the
/// equatorial value is +26 °C at `S = 1` with the greenhouse at its Earth
/// anchor (12 °C above the +14 °C area-mean anchor `THERMOSTAT_ANCHOR_K`
/// fixes), and the polar value is −25 °C (39 °C below that anchor). Cited
/// zonal-mean shape: NCEP/NCAR Reanalysis climatological surface air
/// temperature (flat, warm tropics; a steep high-latitude fall-off that a
/// pure `sin²` cannot reproduce — spec §3.2 names this explicitly, which is
/// why [`LAT_TERM_SIN4_COEFF_C`] exists at all).
const LAT_TERM_EQUATOR_C: f64 = 12.0;

/// The `sin²(lat)` coefficient of the latitude profile — see
/// [`LAT_TERM_EQUATOR_C`] for the three constraints these three constants
/// jointly solve.
///
/// kind: **earth-biosphere** (decision 0106), same citation and derivation
/// as [`LAT_TERM_EQUATOR_C`].
const LAT_TERM_SIN2_COEFF_C: f64 = -13.5;

/// The `sin⁴(lat)` coefficient of the latitude profile: the extra
/// high-latitude steepening a pure `sin²` term cannot supply on its own
/// (spec §3.2) — see [`LAT_TERM_EQUATOR_C`] for the three constraints these
/// three constants jointly solve.
///
/// kind: **earth-biosphere** (decision 0106), same citation and derivation
/// as [`LAT_TERM_EQUATOR_C`].
const LAT_TERM_SIN4_COEFF_C: f64 = -37.5;

/// Continentality: `1.0` fully inland, dropping toward `0.2` as a cell gains
/// ocean neighbors. Damps the seasonal swing (the sea is a thermal buffer).
/// type-audit: bare-ok(ratio: return)
pub fn continentality(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
) -> f64 {
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 1.0;
    }
    let ocean = neighbors
        .iter()
        .filter(|n| *elevation.get(**n) < sea_level)
        .count();
    let land_fraction = 1.0 - ocean as f64 / neighbors.len() as f64;
    0.2 + 0.8 * land_fraction
}

/// Annual-mean temperature per cell, °C. Spinning: a thermostatted insolation
/// baseline (equator warm, poles cold) minus lapse-rate cooling above sea
/// level. Locked: a substellar cosine, hottest at `+x` and floored on the
/// night side — unchanged by the thermostat (`CLIM-locked-regime` is a
/// separate campaign's concern; see `greenhouse_forcing_k`'s doc comment for
/// what that leaves disagreeing between the two regimes at equal insolation).
/// type-audit: pending(wave-2: insolation), pending(wave-2: greenhouse_forcing_k)
pub fn mean_temperature(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    insolation: f64,
    regime: &RotationRegime,
    greenhouse_forcing_k: f64,
) -> CellMap<Temperature> {
    // The Locked branch keeps the plain blackbody exponent applied to raw
    // `S` — its own, unthermostatted formula (`locked_cell_temperature`),
    // untouched by The Glasshouse. `Spinning` computes its own scale below,
    // through `THERMOSTAT_RESIDUAL_FRACTION`'s damped `effective_s`.
    let locked_scale = math::powf(insolation.max(0.0), 0.25);
    CellMap::from_fn(geo, |cell| {
        let above = (*elevation.get(cell) - sea_level).max(0.0);
        let lapse = LAPSE_C_PER_M * above;
        let c = match regime {
            RotationRegime::Spinning { .. } => {
                let lat = geo.coord(cell).latitude.to_radians();
                // The carbonate-silicate thermostat: insolation's deviation
                // from Earth's (`S = 1`) is damped by
                // `THERMOSTAT_RESIDUAL_FRACTION` before the ordinary
                // blackbody `S^(1/4)` response is applied to the result, then
                // this world's drawn greenhouse residual — already converted
                // to kelvin by the composition root (see
                // `ClimateInputs::greenhouse_forcing_k`) — is added.
                // Additive, not insolation-scaled: it is the spread AROUND
                // the thermostat, not a second insolation response.
                let effective_s = 1.0 + THERMOSTAT_RESIDUAL_FRACTION * (insolation.max(0.0) - 1.0);
                let spinning_scale = math::powf(effective_s.max(0.0), 0.25);
                let base_k = THERMOSTAT_ANCHOR_K * spinning_scale + greenhouse_forcing_k;
                let sin_lat = math::sin(lat);
                let s2 = sin_lat * sin_lat;
                let s4 = s2 * s2;
                let lat_term =
                    LAT_TERM_EQUATOR_C + LAT_TERM_SIN2_COEFF_C * s2 + LAT_TERM_SIN4_COEFF_C * s4;
                (base_k - 273.15) + lat_term - lapse
            }
            RotationRegime::Locked => {
                let p = geo.position(cell);
                let cos_theta = crate::substellar_cosine(p);
                crate::locked_cell_temperature(cos_theta, locked_scale, lapse)
            }
        };
        Temperature::new(c).expect("temperature is finite")
    })
}

/// The seasonal half-swing in °C at a cell: proportional to obliquity and to
/// continentality (coastal cells swing less). Zero when obliquity is zero.
/// type-audit: pending(wave-2)
pub fn seasonal_amplitude(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    obliquity_deg: f64,
    cell: CellId,
) -> f64 {
    let cont = continentality(geo, elevation, sea_level, cell);
    (obliquity_deg / 90.0) * 25.0 * cont
}

/// Temperature at a cell on a given day: the annual mean plus a
/// hemisphere-signed seasonal sinusoid on the orbital year phase,
/// `frac(day / year_length_std + year_phase_offset)`, plus (spinning worlds
/// only) a zero-mean diurnal swing over the rotation. Locked worlds have no
/// seasonal term (no year phase organizes their fixed day/night) and no
/// diurnal term (no rotation to swing over) — the `Locked` branch never
/// reads `diurnal_amp`.
/// type-audit: pending(wave-2)
#[allow(clippy::too_many_arguments)]
pub fn temperature_at(
    mean: &CellMap<Temperature>,
    diurnal_amp: &CellMap<f64>,
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    obliquity_deg: f64,
    insolation: f64,
    year_length_std: f64,
    year_phase_offset: f64,
    regime: &RotationRegime,
    cell: CellId,
    day: f64,
) -> Temperature {
    let base = *mean.get(cell);
    match regime {
        RotationRegime::Locked => {
            if obliquity_deg == 0.0 || year_length_std <= 0.0 {
                return base; // no libration to apply
            }
            let year_phase = (day / year_length_std + year_phase_offset).rem_euclid(1.0);
            let sub_lat = obliquity_deg * math::sin(std::f64::consts::TAU * year_phase);
            let dir = crate::substellar_at(sub_lat);
            let cos_theta = crate::substellar_cosine_dir(geo.position(cell), dir);
            let scale = math::powf(insolation.max(0.0), 0.25);
            let above = (*elevation.get(cell) - sea_level).max(0.0);
            let lapse = LAPSE_C_PER_M * above;
            Temperature::new(crate::locked_cell_temperature(cos_theta, scale, lapse))
                .expect("temperature is finite")
        }
        RotationRegime::Spinning { day_std } => {
            // Seasons need a year to organize a phase against; the diurnal
            // swing needs only rotation, so it fires regardless (below).
            let phase = if year_length_std > 0.0 {
                (day / year_length_std + year_phase_offset).rem_euclid(1.0)
            } else {
                0.0
            };
            let seasonal = if year_length_std > 0.0 && obliquity_deg != 0.0 {
                let amp = seasonal_amplitude(geo, elevation, sea_level, obliquity_deg, cell);
                let hemi = geo.coord(cell).latitude.signum();
                base + TempAnomaly::from_offset_c(
                    amp * hemi * math::sin(std::f64::consts::TAU * phase),
                )
            } else {
                base
            };
            seasonal
                + diurnal_anomaly(
                    *diurnal_amp.get(cell),
                    geo.coord(cell).latitude,
                    geo.coord(cell).longitude,
                    obliquity_deg,
                    phase,
                    day.rem_euclid(1.0),
                    *day_std,
                )
        }
    }
}

/// The precomputed per-cell diurnal half-range field, °C: [`diurnal_amplitude`]
/// evaluated at each cell from its moisture, continentality, and elevation
/// above sea level. Mirrors how [`mean_temperature`] is precomputed once per
/// world; the provider stores this field and threads it into `temperature_at`.
/// type-audit: bare-ok(ratio: moisture), bare-ok(diagnostic-value: return)
pub fn diurnal_amplitude_field(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    moisture: &CellMap<f64>,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        let cont = continentality(geo, elevation, sea_level, cell);
        let above = (*elevation.get(cell) - sea_level).max(0.0);
        diurnal_amplitude(*moisture.get(cell), cont, above)
    })
}

/// Locked-world seasonal temperature (°C) at an arbitrary unit position `p`
/// (not snapped to a climate cell), for the librating substellar at `day`.
/// The client reconstructs exactly this at each tile-center; the golden
/// (windows/scene/examples/locked_temperature_golden.rs) pins it. `lapse` is
/// the caller's precomputed elevation lapse (LAPSE_C_PER_M · max(0, elev−sea)).
/// Arithmetically identical to `temperature_at`'s `RotationRegime::Locked`
/// branch — same year_phase/sub_lat/dir/scale/mapping — with a position in
/// place of a `CellId` (so it never touches a `Geosphere` or `CellMap`).
/// type-audit: bare-ok(ratio: p), bare-ok(ratio: insolation), pending(wave-2: lapse), bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: year_phase_offset), bare-ok(diagnostic-value: year_length_std), pending(wave-2: return)
#[allow(clippy::too_many_arguments)]
pub fn locked_temperature_at_position(
    p: [f64; 3],
    insolation: f64,
    lapse: f64,
    day: f64,
    obliquity_deg: f64,
    year_phase_offset: f64,
    year_length_std: f64,
) -> f64 {
    let year_phase = (day / year_length_std + year_phase_offset).rem_euclid(1.0);
    let sub_lat = obliquity_deg * math::sin(std::f64::consts::TAU * year_phase);
    let dir = crate::substellar_at(sub_lat);
    let cos_theta = crate::substellar_cosine_dir(p, dir);
    let scale = math::powf(insolation.max(0.0), 0.25);
    crate::locked_cell_temperature(cos_theta, scale, lapse)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    fn flat_ocean_then_land(
        geo: &Geosphere,
        sea: ReferenceElevation,
    ) -> CellMap<ReferenceElevation> {
        // Half the globe below sea level (x<0), half above — a crude land mask.
        CellMap::from_fn(geo, |c| {
            let m = if geo.position(c)[0] < 0.0 {
                sea.get() - 1000.0
            } else {
                sea.get() + 200.0
            };
            ReferenceElevation::new(m).unwrap()
        })
    }

    /// Spec §3.2's three preregistered bounds, at `S = 1` with the
    /// greenhouse at its Earth anchor (residual `0.0`) and at sea level
    /// (elevation `0.0` everywhere, so lapse cooling is zero and this
    /// isolates the thermostat + latitude profile alone, matching the
    /// bounds' own stated condition): the area-weighted mean is within 1 K
    /// of +14 °C, the equatorial value within 3 K of +26 °C, and the polar
    /// value within 5 K of −25 °C. `⟨sin²(lat)⟩ = 1/3` over a sphere
    /// (`cos(lat)` area element) is why a naive per-cell average would be
    /// biased toward the poles if cells were not near-equal-area; this globe
    /// (level 6, 40,962 cells) is fine enough that a uniform per-cell
    /// average is a good proxy for the true area weighting, the same
    /// approximation `windows/lab/src/metrics.rs`'s own area-weighted
    /// latitude metrics rely on.
    #[test]
    fn spec_3_2_bounds_hold_at_s_equals_1_with_earth_anchor_greenhouse() {
        let geo = Geosphere::new(6);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let sea = ReferenceElevation::new(0.0).unwrap();
        let mean = mean_temperature(
            &geo,
            &elev,
            sea,
            1.0,
            &RotationRegime::Spinning { day_std: 1.0 },
            0.0,
        );
        let area_mean: f64 =
            geo.cells().map(|c| mean.get(c).get()).sum::<f64>() / geo.cell_count() as f64;
        assert!(
            (area_mean - 14.0).abs() <= 1.0,
            "area-weighted mean {area_mean} is not within 1 K of +14 C"
        );

        let equator = geo
            .cells()
            .min_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        let equatorial = mean.get(equator).get();
        assert!(
            (equatorial - 26.0).abs() <= 3.0,
            "equatorial value {equatorial} is not within 3 K of +26 C"
        );

        let pole = geo
            .cells()
            .max_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        let polar = mean.get(pole).get();
        assert!(
            (polar - (-25.0)).abs() <= 5.0,
            "polar value {polar} is not within 5 K of -25 C"
        );
        println!(
            "spec 3.2: area-mean {area_mean:.6} (target 14 +/-1), equatorial {equatorial:.6} \
             (target 26 +/-3), polar {polar:.6} (target -25 +/-5)"
        );
    }

    #[test]
    fn temperature_falls_with_latitude_on_a_spinning_world() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let mean = mean_temperature(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            1.0,
            &RotationRegime::Spinning { day_std: 1.0 },
            0.0,
        );
        let equator = geo
            .cells()
            .min_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        let pole = geo
            .cells()
            .max_by(|a, b| {
                geo.coord(*a)
                    .latitude
                    .abs()
                    .total_cmp(&geo.coord(*b).latitude.abs())
            })
            .unwrap();
        assert!(
            mean.get(equator) > mean.get(pole),
            "equator must be warmer than pole"
        );
    }

    #[test]
    fn temperature_falls_with_altitude() {
        let geo = Geosphere::new(3);
        let low = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let high = CellMap::from_fn(&geo, |_| ReferenceElevation::new(3000.0).unwrap());
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let sea = ReferenceElevation::new(0.0).unwrap();
        let mlow = mean_temperature(&geo, &low, sea, 1.0, &regime, 0.0);
        let mhigh = mean_temperature(&geo, &high, sea, 1.0, &regime, 0.0);
        for c in geo.cells() {
            assert!(
                mhigh.get(c) < mlow.get(c),
                "altitude must cool cell {}",
                c.0
            );
        }
    }

    #[test]
    fn locked_world_is_hottest_at_substellar_and_coldest_at_antistellar() {
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let mean = mean_temperature(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            1.0,
            &RotationRegime::Locked,
            0.0,
        );
        let sub = geo
            .cells()
            .max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        let anti = geo
            .cells()
            .min_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        assert!(
            *mean.get(sub) > *mean.get(anti) + TempAnomaly::from_offset_c(50.0),
            "substellar must tower over antistellar"
        );
    }

    #[test]
    fn spinning_seasonal_peak_tracks_the_year_phase_offset() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        // Zero diurnal amplitude: this test targets the seasonal term only.
        let diurnal_amp = CellMap::from_fn(&geo, |_| 0.0);
        // A clearly-northern cell.
        let north = geo
            .cells()
            .max_by(|a, b| geo.coord(*a).latitude.total_cmp(&geo.coord(*b).latitude))
            .unwrap();
        let year = 360.0;
        let offset = 0.2;
        // Final signature: (…, obliquity, insolation, year_length, year_phase_offset, …)
        let t = |day: f64| {
            temperature_at(
                &mean,
                &diurnal_amp,
                &geo,
                &elevation,
                sea,
                23.5,
                1.0,
                year,
                offset,
                &regime,
                north,
                day,
            )
            .get()
        };
        // Northern summer (max) is at frac(day/year + offset) = 0.25 -> day = (0.25 - offset)*year (mod year).
        let summer_day = ((0.25 - offset).rem_euclid(1.0)) * year;
        let winter_day = ((0.75 - offset).rem_euclid(1.0)) * year;
        assert!(
            t(summer_day) > t(winter_day) + 1.0,
            "north is warmest near its offset-shifted summer"
        );
        // And at the offset-shifted equinox the anomaly is ~0 (mean).
        let equinox_day = ((0.0 - offset).rem_euclid(1.0)) * year;
        assert!((t(equinox_day) - mean.get(north).get()).abs() < 0.2);
    }

    #[test]
    fn coastal_seasonal_swing_is_smaller_than_continental() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let elev = flat_ocean_then_land(&geo, sea);
        // A land cell touching ocean vs a land cell deep inland at similar latitude.
        let coastal = geo
            .cells()
            .find(|c| {
                elev.get(*c) >= &sea
                    && geo.neighbors(*c).iter().any(|n| elev.get(*n) < &sea)
                    && geo.coord(*c).latitude.abs() > 20.0
                    && geo.coord(*c).latitude.abs() < 60.0
            })
            .unwrap();
        let inland = geo
            .cells()
            .find(|c| {
                elev.get(*c) >= &sea
                    && geo.neighbors(*c).iter().all(|n| elev.get(*n) >= &sea)
                    && geo.coord(*c).latitude.abs() > 20.0
                    && geo.coord(*c).latitude.abs() < 60.0
            })
            .unwrap();
        let ac = seasonal_amplitude(&geo, &elev, sea, 23.5, coastal);
        let ai = seasonal_amplitude(&geo, &elev, sea, 23.5, inland);
        assert!(ac < ai, "coastal swing {ac} not smaller than inland {ai}");
    }

    #[test]
    fn locked_substellar_hot_spot_librates_with_obliquity() {
        let geo = Geosphere::new(4);
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Locked;
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |_| 0.0);
        let year = 240.0;
        let obliq = 22.0;
        // At the northern solstice (frac(day/year + 0) = 0.25) the substellar
        // latitude is +obliquity, so the warmest cell sits near +22 deg lat,
        // not the equator.
        let solstice = 0.25 * year;
        let warmest = geo
            .cells()
            .max_by(|a, b| {
                let ta = temperature_at(
                    &mean,
                    &diurnal_amp,
                    &geo,
                    &elevation,
                    sea,
                    obliq,
                    1.0,
                    year,
                    0.0,
                    &regime,
                    *a,
                    solstice,
                )
                .get();
                let tb = temperature_at(
                    &mean,
                    &diurnal_amp,
                    &geo,
                    &elevation,
                    sea,
                    obliq,
                    1.0,
                    year,
                    0.0,
                    &regime,
                    *b,
                    solstice,
                )
                .get();
                ta.total_cmp(&tb)
            })
            .unwrap();
        let lat = geo.coord(warmest).latitude;
        assert!(
            lat > 10.0,
            "at northern solstice the hot spot has climbed north, got lat {lat}"
        );
    }

    #[test]
    fn locked_temperature_at_position_matches_temperature_at_the_cells_own_position() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let elevation = CellMap::from_fn(&geo, |c| {
            let m = if geo.position(c)[0] > 0.0 { 500.0 } else { 0.0 };
            ReferenceElevation::new(m).unwrap()
        });
        let regime = RotationRegime::Locked;
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |_| 0.0);
        // The substellar-ish cell: the position formula must agree with the
        // cell-based one everywhere, but pick a cell with nonzero lapse too
        // (elevation above sea level) so the lapse term is exercised.
        let cell = geo
            .cells()
            .max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        let obliquity = 21.8;
        let year = 240.0;
        let offset = 0.15;
        let day = 63.0;
        let expected = temperature_at(
            &mean,
            &diurnal_amp,
            &geo,
            &elevation,
            sea,
            obliquity,
            1.0,
            year,
            offset,
            &regime,
            cell,
            day,
        )
        .get();
        let p = geo.position(cell);
        let above = (*elevation.get(cell) - sea).max(0.0);
        let lapse = LAPSE_C_PER_M * above;
        let actual = locked_temperature_at_position(p, 1.0, lapse, day, obliquity, offset, year);
        assert!(
            (expected - actual).abs() < 1e-9,
            "expected {expected} got {actual}"
        );
    }

    #[test]
    fn locked_temperature_is_static_at_zero_obliquity() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Locked;
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |_| 0.0);
        let cell = CellId(0);
        let a = temperature_at(
            &mean,
            &diurnal_amp,
            &geo,
            &elevation,
            sea,
            0.0,
            1.0,
            240.0,
            0.0,
            &regime,
            cell,
            0.0,
        )
        .get();
        let b = temperature_at(
            &mean,
            &diurnal_amp,
            &geo,
            &elevation,
            sea,
            0.0,
            1.0,
            240.0,
            0.0,
            &regime,
            cell,
            120.0,
        )
        .get();
        assert_eq!(
            a, b,
            "zero obliquity: no libration, temperature is day-independent"
        );
    }

    // Locked worlds have no diurnal term regardless of time-of-day. Zero
    // obliquity makes the *existing* Locked branch exactly day-independent
    // (it early-returns `base`), so this isolates the diurnal wiring: the
    // diurnal amplitude is deliberately nonzero (anti-vacuity) — if the
    // diurnal term were ever mistakenly wired into the Locked branch, these
    // two day fractions (differing local time-of-day) would diverge, since
    // `diurnal_waveform` depends on `day_fraction` even at zero obliquity.
    #[test]
    fn locked_worlds_have_no_diurnal_term() {
        let geo = Geosphere::new(3);
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(0.0).unwrap());
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Locked;
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |_| 12.0);
        let cell = geo
            .cells()
            .find(|c| {
                let lat = geo.coord(*c).latitude.abs();
                lat > 5.0 && lat < 80.0
            })
            .unwrap();
        let a = temperature_at(
            &mean,
            &diurnal_amp,
            &geo,
            &elevation,
            sea,
            0.0,
            1.0,
            240.0,
            0.0,
            &regime,
            cell,
            0.2,
        )
        .get();
        let b = temperature_at(
            &mean,
            &diurnal_amp,
            &geo,
            &elevation,
            sea,
            0.0,
            1.0,
            240.0,
            0.0,
            &regime,
            cell,
            0.7,
        )
        .get();
        assert_eq!(
            a, b,
            "locked worlds must have no diurnal term, even with a nonzero amplitude field"
        );
    }

    // Spinning: local afternoon must be markedly warmer than local pre-dawn
    // for a dry, fully-continental cell — the diurnal mechanism actually
    // firing, not stubbed to zero.
    #[test]
    fn spinning_afternoon_is_warmer_than_predawn() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // All land, well above sea level: every cell is fully continental.
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        // A near-equatorial cell: the sun reliably rises there every day.
        let cell = geo
            .cells()
            .find(|c| geo.coord(*c).latitude.abs() < 15.0)
            .unwrap();
        // A dry, fully-continental amplitude (desert-like swing) at that cell.
        let dry_amplitude = diurnal_amplitude(0.05, 1.0, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |c| if c == cell { dry_amplitude } else { 0.0 });
        let year = 360.0;
        let day = 100.0;
        // The diurnal term is phased on LOCAL solar time (day_fraction +
        // longitude/360), so the real `day_fraction` that lands the cell in
        // its local afternoon/pre-dawn depends on the cell's own longitude.
        let lon = geo.coord(cell).longitude;
        let local_to_day_fraction =
            |local_solar_time: f64| (local_solar_time - lon / 360.0).rem_euclid(1.0);
        let t = |day_fraction: f64| {
            temperature_at(
                &mean,
                &diurnal_amp,
                &geo,
                &elevation,
                sea,
                23.5,
                1.0,
                year,
                0.0,
                &regime,
                cell,
                day + day_fraction,
            )
            .get()
        };
        let afternoon = t(local_to_day_fraction(0.60));
        let predawn = t(local_to_day_fraction(0.05));
        assert!(
            afternoon > predawn + 5.0,
            "afternoon {afternoon} should tower over predawn {predawn} by a physical margin"
        );
    }

    // The diurnal swing is driven by ROTATION, not tilt: a zero-obliquity
    // spinning world still has a real day/night cycle and must show the same
    // afternoon-over-predawn margin as a tilted one (T2 review regression —
    // the diurnal term used to be gated behind the same `obliquity_deg ==
    // 0.0` early-return that also (correctly) zeroes the seasonal term).
    #[test]
    fn spinning_diurnal_fires_at_zero_obliquity() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // All land, well above sea level: every cell is fully continental.
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        // A near-equatorial cell: the sun reliably rises there every day.
        let cell = geo
            .cells()
            .find(|c| geo.coord(*c).latitude.abs() < 15.0)
            .unwrap();
        // A dry, fully-continental amplitude (desert-like swing) at that cell.
        let dry_amplitude = diurnal_amplitude(0.05, 1.0, 0.0);
        let diurnal_amp = CellMap::from_fn(&geo, |c| if c == cell { dry_amplitude } else { 0.0 });
        let year = 360.0;
        let day = 100.0;
        // The diurnal term is phased on LOCAL solar time (day_fraction +
        // longitude/360), so the real `day_fraction` that lands the cell in
        // its local afternoon/pre-dawn depends on the cell's own longitude.
        let lon = geo.coord(cell).longitude;
        let local_to_day_fraction =
            |local_solar_time: f64| (local_solar_time - lon / 360.0).rem_euclid(1.0);
        let t = |day_fraction: f64| {
            temperature_at(
                &mean,
                &diurnal_amp,
                &geo,
                &elevation,
                sea,
                0.0, // zero obliquity: no tilt, no seasons
                1.0,
                year,
                0.0,
                &regime,
                cell,
                day + day_fraction,
            )
            .get()
        };
        let afternoon = t(local_to_day_fraction(0.60));
        let predawn = t(local_to_day_fraction(0.05));
        assert!(
            afternoon > predawn + 5.0,
            "afternoon {afternoon} should tower over predawn {predawn} by a physical margin \
             even at zero obliquity — the diurnal term is rotation-driven, not tilt-driven"
        );
    }

    // The daily mean is unchanged by the diurnal term: averaging
    // `temperature_at` over many day-fractions within one rotation recovers
    // the pre-diurnal mean+seasonal value (the load-bearing zero-mean
    // invariant that keeps `mean_temperature` — and the census — untouched).
    #[test]
    fn daily_mean_is_unchanged_by_the_diurnal_term() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let mean = mean_temperature(&geo, &elevation, sea, 1.0, &regime, 0.0);
        // An equatorial cell: away from any polar-night clamp in the
        // waveform, so the zero-mean cancellation converges cleanly.
        let cell = geo
            .cells()
            .find(|c| geo.coord(*c).latitude.abs() < 10.0)
            .unwrap();
        // A real, nonzero diurnal amplitude everywhere — a stubbed-to-zero
        // diurnal term would trivially pass this test too, but
        // `spinning_afternoon_is_warmer_than_predawn` above already catches
        // that; here the amplitude stays nonzero so this test exercises the
        // actual zero-mean cancellation, not an absent term.
        let diurnal_amp = CellMap::from_fn(&geo, |_| 20.0);
        // A deliberately long year: the seasonal phase must stay effectively
        // fixed across the single rotation this test averages over, so the
        // only thing under test is the diurnal term's zero-mean cancellation
        // (not the seasonal term's ordinary drift across a day).
        let year = 36_525.0;
        let offset = 0.1;
        let day = 100.0;
        let n = 200;
        let sum: f64 = (0..n)
            .map(|i| {
                let frac = i as f64 / f64::from(n);
                temperature_at(
                    &mean,
                    &diurnal_amp,
                    &geo,
                    &elevation,
                    sea,
                    23.5,
                    1.0,
                    year,
                    offset,
                    &regime,
                    cell,
                    day + frac,
                )
                .get()
            })
            .sum();
        let daily_mean = sum / f64::from(n);
        let amp = seasonal_amplitude(&geo, &elevation, sea, 23.5, cell);
        let hemi = geo.coord(cell).latitude.signum();
        let phase = (day / year + offset).rem_euclid(1.0);
        let expected = mean.get(cell).get() + amp * hemi * math::sin(std::f64::consts::TAU * phase);
        assert!(
            (daily_mean - expected).abs() < 1e-2,
            "daily mean {daily_mean} should equal the pre-diurnal mean+seasonal {expected}"
        );
    }
}
