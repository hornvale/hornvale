//! Moisture over the globe (dimensionless `[0, 1]`, bare f64). Spinning
//! worlds: a base set by the circulation band (rising belts wet, sinking
//! belts dry), raised near oceans, then dried by an upwind moisture-budget
//! trace — oceans evaporate precipitable water, the prevailing wind carries
//! it, and it is depleted by orography (mountains), convection (rising
//! bands), and a per-step overland distance decay. That depletion is
//! normalized to a `[0, 1]` dryness that subtracts from the banded floor
//! (`base + ocean_bonus − DRY_STRENGTH · dryness`) — the same structure as
//! the original single-pass rain shadow, with the budget trace supplying a
//! richer (compounding, distance-sensitive) dryness signal in its place.
//! Locked worlds: wettest near the terminator, dry at the substellar and
//! antistellar points. Declared approximations (spec §5): a single upwind
//! trace (no advection field, no clouds/feedback).

use crate::circulation::{
    RotationRegime, band_count_for, band_index, is_rising_band, prevailing_wind,
};
use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, math};

/// Cells traced upwind when building the moisture-budget path.
const BUDGET_STEPS: usize = 48;
/// Precipitable water added per upwind step over open ocean (dimensionless).
const EVAP: f64 = 0.5;
/// Cap on precipitable water carried along the trace.
const W_CAP: f64 = 1.0;
/// Orographic rainout coefficient, scaled by uplift over `UPLIFT_SCALE_M`.
const OROG_K: f64 = 0.07;
/// Elevation scale (m) normalizing uplift for the orographic sink.
const UPLIFT_SCALE_M: f64 = 3000.0;
/// Convective rainout subtracted once per overland step falling in a rising
/// (wet) circulation band.
const CONVECTIVE: f64 = 0.005;
/// Fractional decay of precipitable water per overland step (distance
/// drying: continental interiors dry out even on flat terrain).
const DECAY: f64 = 0.006;
/// Carried-water level, at or above which a cell counts as fully supplied
/// (`budget_dryness` floors at `0`). Below this, dryness rises linearly to
/// `1` at zero carried water.
const W_REFERENCE: f64 = 0.95;
/// Weight of the budget-derived dryness against the banded wetness floor
/// (mirrors the original single-pass rain shadow's `0.5` — the value large
/// enough that a cell with no upwind ocean anywhere (`dryness == 1`) lands at
/// or below the aridity floor even in the wettest band).
const DRY_STRENGTH: f64 = 0.6;
/// Base wetness for a rising (wet) circulation band — the tuned
/// habitability floor, unchanged since the original banded model.
const WET_BAND_BASE: f64 = 0.6;
/// Base wetness for a sinking (dry) circulation band.
const DRY_BAND_BASE: f64 = 0.25;

/// Ocean-proximity bonus: `+0.3` if the cell itself is ocean-adjacent (or is
/// ocean), tapering to `0.0` fully inland.
fn ocean_bonus(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
) -> f64 {
    if is_ocean(elevation, sea_level, cell) {
        return 0.3;
    }
    let neighbors = geo.neighbors(cell);
    if neighbors.is_empty() {
        return 0.0;
    }
    let ocean = neighbors
        .iter()
        .filter(|n| is_ocean(elevation, sea_level, **n))
        .count();
    0.3 * (ocean as f64 / neighbors.len() as f64)
}

/// Whether a cell lies below sea level — an ocean cell, and thus its own
/// moisture source in the budget trace.
fn is_ocean(
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
) -> bool {
    *elevation.get(cell) < sea_level
}

/// Evaporative warmth at a cell: `cos(latitude)`, clamped to `[0, 1]` (the
/// equator evaporates most, the poles none). Deliberately uncoupled from any
/// temperature field (spec approximation).
fn warmth(geo: &Geosphere, cell: CellId) -> f64 {
    math::cos(geo.coord(cell).latitude.to_radians()).clamp(0.0, 1.0)
}

/// The upwind neighbor of `cell`: the one whose displacement from `cell` is
/// most opposite `wind` (i.e., the direction the wind blows *from*). Shared
/// by every upwind trace over the globe.
fn upwind_neighbor(geo: &Geosphere, cell: CellId, wind: [f64; 3]) -> Option<CellId> {
    let cp = geo.position(cell);
    geo.neighbors(cell).iter().copied().max_by(|a, b| {
        let da = geo.position(*a);
        let db = geo.position(*b);
        let sa =
            -((da[0] - cp[0]) * wind[0] + (da[1] - cp[1]) * wind[1] + (da[2] - cp[2]) * wind[2]);
        let sb =
            -((db[0] - cp[0]) * wind[0] + (db[1] - cp[1]) * wind[1] + (db[2] - cp[2]) * wind[2]);
        sa.total_cmp(&sb)
    })
}

/// The upwind moisture-budget dryness at a cell (spinning worlds, land
/// only): walk up to `BUDGET_STEPS` cells upwind (stopping early on a dead
/// end or self-loop), then replay from the farthest upwind end back to
/// `cell`, carrying precipitable water `W`. Oceans evaporate into `W` (each
/// is its own source, scaled by `warmth`); land depletes it — orographically
/// (uplift crossed since the prior upwind cell), convectively (rising
/// bands), and by a per-step distance decay. The water arriving at `cell`
/// (after its own local depletion) is normalized against `W_REFERENCE` into
/// a `[0, 1]` dryness: `0` when at least fully supplied, `1` when starved
/// (no upwind source reached within `BUDGET_STEPS`, or fully rained out en
/// route).
fn budget_dryness(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
    wind: [f64; 3],
    bands: u32,
) -> f64 {
    let w = carried_water(geo, elevation, sea_level, cell, wind, bands);
    // Normalize the carried water into dryness, `[0, 1]`.
    1.0 - (w / W_REFERENCE).clamp(0.0, 1.0)
}

/// The carried precipitable water `W` arriving at `cell` after the full
/// upwind budget replay (see `budget_dryness`), before normalization.
fn carried_water(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    cell: CellId,
    wind: [f64; 3],
    bands: u32,
) -> f64 {
    // Step 1: the upwind path, `cell` first, farthest upwind last.
    let mut path = vec![cell];
    if wind != [0.0, 0.0, 0.0] {
        let mut current = cell;
        for _ in 0..BUDGET_STEPS {
            let Some(next) = upwind_neighbor(geo, current, wind) else {
                break;
            };
            if next == current {
                break;
            }
            path.push(next);
            current = next;
        }
    }
    let last = path.len() - 1;

    // Step 2: replay from the farthest upwind end toward `cell`, carrying
    // (and depleting) precipitable water `W`. `cell` itself is `path[0]`, so
    // its own local orographic/convective/decay effects are folded in here.
    let mut w = 0.0;
    for i in (0..path.len()).rev() {
        let c = path[i];
        if is_ocean(elevation, sea_level, c) {
            w = (w + EVAP * warmth(geo, c)).min(W_CAP);
        } else {
            let prev = path[(i + 1).min(last)]; // one step upwind of `c`
            let uplift = (*elevation.get(c) - *elevation.get(prev)).max(0.0);
            w = (w - OROG_K * (uplift / UPLIFT_SCALE_M)).max(0.0);
            if is_rising_band(band_index(geo.coord(c).latitude, bands)) {
                w = (w - CONVECTIVE).max(0.0);
            }
            w *= 1.0 - DECAY;
        }
    }
    w
}

/// Moisture per cell, `[0, 1]`. See the module doc for the model.
/// type-audit: bare-ok(ratio: return)
pub fn moisture_field(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    regime: &RotationRegime,
) -> CellMap<f64> {
    match band_count_for(regime) {
        None => {
            // Locked: wettest at the terminator (|cos θ| ≈ 0), dry at the poles
            // of the day/night axis.
            CellMap::from_fn(geo, |cell| {
                let p = geo.position(cell);
                let cos_theta = crate::substellar_cosine(p);
                let base = 0.7 * (1.0 - cos_theta.abs());
                (base + ocean_bonus(geo, elevation, sea_level, cell)).clamp(0.0, 1.0)
            })
        }
        Some(bands) => CellMap::from_fn(geo, |cell| {
            let band = band_index(geo.coord(cell).latitude, bands);
            let base = if is_rising_band(band) {
                WET_BAND_BASE
            } else {
                DRY_BAND_BASE
            };
            let wind = prevailing_wind(geo, cell, bands);
            let dryness = if *elevation.get(cell) >= sea_level {
                budget_dryness(geo, elevation, sea_level, cell, wind, bands)
            } else {
                0.0
            };
            let raw = base + ocean_bonus(geo, elevation, sea_level, cell) - DRY_STRENGTH * dryness;
            raw.clamp(0.0, 1.0)
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::Geosphere;

    /// The neighbor of `from` most aligned with `wind` (i.e., downwind) —
    /// the mirror image of production's `upwind_neighbor`, used only to walk
    /// a deterministic downwind transect for the interior-drying test.
    fn downwind_neighbor(geo: &Geosphere, from: CellId, wind: [f64; 3]) -> CellId {
        let p = geo.position(from);
        *geo.neighbors(from)
            .iter()
            .max_by(|a, b| {
                let da = geo.position(**a);
                let db = geo.position(**b);
                let sa =
                    (da[0] - p[0]) * wind[0] + (da[1] - p[1]) * wind[1] + (da[2] - p[2]) * wind[2];
                let sb =
                    (db[0] - p[0]) * wind[0] + (db[1] - p[1]) * wind[1] + (db[2] - p[2]) * wind[2];
                sa.total_cmp(&sb)
            })
            .unwrap()
    }

    #[test]
    fn a_range_casts_a_rain_shadow() {
        let geo = Geosphere::new(5);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let bands = band_count_for(&regime).unwrap();

        // A single downwind land transect (the wind-walk depends only on
        // geo + wind, not elevation, so this chain is shared by both
        // scenarios below). Position 1 is either flat land or a tall ridge;
        // everything off the transect is ocean. Both scenarios put the same
        // number of overland hops between the transect's end and the ocean
        // source, so decay/convective drying is identical between them —
        // isolating the orographic sink as the only remaining difference.
        let start = geo.cells().nth(2000).unwrap();
        let mut chain = vec![start];
        for _ in 1..5 {
            let current = *chain.last().unwrap();
            let wind = prevailing_wind(&geo, current, bands);
            chain.push(downwind_neighbor(&geo, current, wind));
        }
        let end = *chain.last().unwrap();

        let build = |ridge_at: usize| {
            CellMap::from_fn(&geo, |c| {
                let e = match chain.iter().position(|&x| x == c) {
                    Some(idx) if idx == ridge_at => 5000.0,
                    Some(_) => 200.0,
                    None => -500.0,
                };
                ReferenceElevation::new(e).unwrap()
            })
        };
        let flat = build(usize::MAX); // no index matches: no ridge inserted
        let ridged = build(1);

        let m_flat = moisture_field(&geo, &flat, sea, &regime);
        let m_ridged = moisture_field(&geo, &ridged, sea, &regime);
        assert!(
            m_ridged.get(end) < m_flat.get(end),
            "downwind of a range ({}) not drier than the same transect flat ({})",
            m_ridged.get(end),
            m_flat.get(end)
        );
    }

    #[test]
    fn interiors_dry_with_distance_from_sea() {
        let geo = Geosphere::new(5);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let regime = RotationRegime::Spinning { day_std: 1.0 };
        let bands = band_count_for(&regime).unwrap();
        // Walk a downwind transect of flat land from an arbitrary start;
        // everything off the transect is ocean, so the start is coastal.
        let mut chain = vec![geo.cells().nth(500).unwrap()];
        for _ in 0..7 {
            let current = *chain.last().unwrap();
            let wind = prevailing_wind(&geo, current, bands);
            chain.push(downwind_neighbor(&geo, current, wind));
        }
        let coast = chain[0];
        let inland = *chain.last().unwrap();
        let elev = CellMap::from_fn(&geo, |c| {
            let e = if chain.contains(&c) { 200.0 } else { -500.0 };
            ReferenceElevation::new(e).unwrap()
        });
        let m = moisture_field(&geo, &elev, sea, &regime);
        assert!(
            m.get(inland) < m.get(coast),
            "inland {} not drier than coast {}",
            m.get(inland),
            m.get(coast)
        );
    }

    #[test]
    fn a_cell_with_no_upwind_ocean_is_dry() {
        // All land, flat, no ocean anywhere: no cell's upwind path (within
        // BUDGET_STEPS) can find a source, so precipitable water never
        // accumulates.
        let geo = Geosphere::new(4);
        let elev = CellMap::from_fn(&geo, |_| ReferenceElevation::new(200.0).unwrap());
        let m = moisture_field(
            &geo,
            &elev,
            ReferenceElevation::new(0.0).unwrap(),
            &RotationRegime::Spinning { day_std: 1.0 },
        );
        let cell = geo.cells().nth(1234).unwrap();
        assert!(
            *m.get(cell) < 0.05,
            "cell with no upwind ocean is not dry: {}",
            m.get(cell)
        );
    }

    #[test]
    fn moisture_is_bounded_and_locked_is_substellar() {
        let geo = Geosphere::new(4);
        let sea = ReferenceElevation::new(0.0).unwrap();
        // Bounded: a mixed ocean/ridge/land world under the spinning branch.
        let ridge = geo.cells().nth(1000).unwrap();
        let elev = CellMap::from_fn(&geo, |c| {
            let e = if c == ridge {
                5000.0
            } else if c.0.is_multiple_of(2) {
                -300.0
            } else {
                200.0
            };
            ReferenceElevation::new(e).unwrap()
        });
        let m = moisture_field(&geo, &elev, sea, &RotationRegime::Spinning { day_std: 1.0 });
        for (_, v) in m.iter() {
            assert!((0.0..=1.0).contains(v), "moisture {v} out of [0,1]");
        }

        // Locked: unchanged — wettest at the terminator, dry at the
        // substellar and antistellar points.
        let flat = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap());
        let ml = moisture_field(&geo, &flat, sea, &RotationRegime::Locked);
        let sub = geo
            .cells()
            .max_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        let anti = geo
            .cells()
            .min_by(|a, b| geo.position(*a)[0].total_cmp(&geo.position(*b)[0]))
            .unwrap();
        let term = geo
            .cells()
            .min_by(|a, b| {
                geo.position(*a)[0]
                    .abs()
                    .total_cmp(&geo.position(*b)[0].abs())
            })
            .unwrap();
        assert!(ml.get(term) > ml.get(sub));
        assert!(ml.get(term) > ml.get(anti));
    }
}
