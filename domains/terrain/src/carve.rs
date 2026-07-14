//! The carve (Sculpting, engine A — spec §5): a one-shot erosion/deposition
//! correction. Pure functions of existing fields; no draws, no time-steps.
//!
//! The seam is potential-agnostic (spec §2): this module reads "a potential
//! (elevation), a flow network derived from it (drainage), a resistivity
//! (erodibility)" — water is the only instantiation this campaign builds.
//! A banked ley-line re-instantiation (a different potential, a different
//! flow network, a different resistivity field) reuses this machinery
//! whole, unchanged.
//!
//! This module is **not yet wired into `generate`** (Task 10 does that): it
//! must change no world byte on its own. `CarveDelta` here carries the full
//! shape Tasks 8-9 will need (the marine wedge/delta/atoll fields), but this
//! task's functions populate only the incision and repose contributions —
//! see [`CarveDelta::from_incision_and_repose`].

use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, math};

/// Tuning knobs for the carve (engine A). Global constants only —
/// heterogeneity comes from fields (spec §5).
/// type-audit: pending(wave-2: incision_k_m), bare-ok(ratio: area_exponent), bare-ok(ratio: slope_exponent), bare-ok(count: area_ref), pending(wave-2: slope_ref_m), pending(wave-2: max_incision_m), pending(wave-2: repose_drop_m), bare-ok(count: repose_sweeps), pending(wave-2: deposit_slope_m), bare-ok(ratio: deposit_fraction), pending(wave-2: wedge_freeboard_m), bare-ok(count: wedge_reach_passive), bare-ok(count: wedge_reach_active), bare-ok(count: delta_count), pending(wave-2: delta_height_m), pending(wave-2: atoll_freeboard_m), pending(wave-2: atoll_max_depth_m), pending(wave-2: atoll_max_abs_lat)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CarveParams {
    /// Peak incision scale, meters (the stream-power prefactor `k`).
    pub incision_k_m: f64,
    /// Drainage-area exponent `m` in `(A/A0)^m`.
    pub area_exponent: f64,
    /// Slope exponent `n` in `(S/S0)^n`.
    pub slope_exponent: f64,
    /// Drainage normalization `A0` (drainage-field units: upstream
    /// land-cell count, per [`crate::drainage::drainage_field`]).
    pub area_ref: f64,
    /// Slope normalization `S0`, meters of drop per graph hop.
    pub slope_ref_m: f64,
    /// Hard cap on a single cell's incision depth, meters.
    pub max_incision_m: f64,
    /// Critical inter-cell elevation drop above which hillslope repose
    /// acts, meters.
    pub repose_drop_m: f64,
    /// Fixed count of repose relaxation sweeps (never iterated to a
    /// convergence criterion — determinism wants a fixed budget).
    pub repose_sweeps: u32,
    /// Inter-cell slope, meters per hop, flatter than which deposition
    /// occurs (Task 8: the deposition half of engine A).
    pub deposit_slope_m: f64,
    /// Fraction of transported material deposited per flat hop (Task 8).
    pub deposit_fraction: f64,
    /// Shelf cap below sea level for the passive-margin sediment wedge,
    /// meters (Task 9: marine deposition).
    pub wedge_freeboard_m: f64,
    /// BFS hops seaward the sediment wedge reaches on a passive margin
    /// (Task 9).
    pub wedge_reach_passive: u32,
    /// BFS hops seaward the sediment wedge reaches on an active margin
    /// (Task 9).
    pub wedge_reach_active: u32,
    /// Top-K river mouths that grow a subaerial delta lobe (spec §5,
    /// K = 3) (Task 9).
    pub delta_count: u32,
    /// Subaerial delta-lobe cap above sea level, meters (Task 9).
    pub delta_height_m: f64,
    /// Freeboard cap for an atoll rim above its drowned seamount, meters
    /// (Task 9).
    pub atoll_freeboard_m: f64,
    /// Maximum drowned-seamount depth eligible for an atoll rim, meters
    /// (Task 9).
    pub atoll_max_depth_m: f64,
    /// Maximum absolute latitude eligible for an atoll rim, radians
    /// (Task 9).
    pub atoll_max_abs_lat: f64,
}

impl Default for CarveParams {
    fn default() -> Self {
        CarveParams {
            incision_k_m: 90.0,
            area_exponent: 0.5,
            slope_exponent: 1.0,
            area_ref: 40.0,
            slope_ref_m: 200.0,
            max_incision_m: 900.0,
            repose_drop_m: 700.0,
            repose_sweeps: 4,
            deposit_slope_m: 40.0,
            deposit_fraction: 0.35,
            wedge_freeboard_m: 40.0,
            wedge_reach_passive: 4,
            wedge_reach_active: 1,
            delta_count: 3,
            delta_height_m: 15.0,
            atoll_freeboard_m: 5.0,
            atoll_max_depth_m: 600.0,
            atoll_max_abs_lat: 0.6,
        }
    }
}

/// The carve's output: an elevation delta plus the sediment bookkeeping.
/// This task ([`CarveDelta::from_incision_and_repose`]) populates
/// `delta_m`, `sediment_thickness_m`, `eroded_total_m3`, and
/// `deposited_total_m3` from incision + repose alone; `mouths` stays empty
/// and `ocean_loss_m3` stays `0.0` until Tasks 8-9 wire marine deposition
/// (the sediment wedge, river deltas, atoll rims) — every field is `pub`,
/// so those tasks extend a built `CarveDelta` by assignment rather than by
/// widening a constructor's argument list.
/// type-audit: pending(wave-2: delta_m), pending(wave-2: sediment_thickness_m), pending(wave-2: mouths), pending(wave-2: eroded_total_m3), pending(wave-2: deposited_total_m3), pending(wave-2: ocean_loss_m3)
#[derive(Debug, Clone, PartialEq)]
pub struct CarveDelta {
    /// Elevation delta to add to the base field, meters (± — incision
    /// subtracts, repose and deposition add).
    pub delta_m: CellMap<f64>,
    /// Deposited sediment thickness, meters (≥ 0). This task derives it
    /// from repose's receiver-side gains only; Tasks 8-9 add marine
    /// deposits (wedge/delta/atoll) on top.
    pub sediment_thickness_m: CellMap<f64>,
    /// River mouths and their exported sediment volume, sorted by volume
    /// descending (`CellId` tiebreak). Empty until Task 9 wires deltas.
    pub mouths: Vec<(CellId, f64)>,
    /// Total eroded volume proxy — a cell-area-weighted volume proxy
    /// (Σ depth, one unit per cell) — summing both stream-power incision
    /// and hillslope repose's donor-side losses.
    pub eroded_total_m3: f64,
    /// Total deposited volume proxy, summing repose's receiver-side gains
    /// (Tasks 8-9 add marine deposition here).
    pub deposited_total_m3: f64,
    /// Volume proxy routed to the ocean (river-mouth export, wedge
    /// burial). Always `0.0` until Task 9 routes incision's eroded
    /// material seaward.
    pub ocean_loss_m3: f64,
}

impl CarveDelta {
    /// Assemble a `CarveDelta` from this task's two contributors —
    /// stream-power incision and hillslope repose — leaving the marine
    /// fields at their empty/zero defaults. Tasks 8-9 build on this by
    /// mutating the returned struct's `pub` fields (folding wedge/delta/
    /// atoll contributions into `delta_m`/`sediment_thickness_m` and
    /// filling `mouths`/`ocean_loss_m3`) rather than by widening this
    /// function's argument list — the constructor stays this task's,
    /// permanently.
    /// type-audit: pending(wave-2: incision_m), pending(wave-2: repose_delta_m)
    pub fn from_incision_and_repose(
        geo: &Geosphere,
        incision_m: &CellMap<f64>,
        repose_delta_m: &CellMap<f64>,
    ) -> CarveDelta {
        let mut eroded_total_m3 = 0.0_f64;
        let mut deposited_total_m3 = 0.0_f64;
        let delta_m = CellMap::from_fn(geo, |c| {
            let inc = *incision_m.get(c);
            let rep = *repose_delta_m.get(c);
            eroded_total_m3 += -inc.min(0.0);
            if rep < 0.0 {
                eroded_total_m3 += -rep;
            } else {
                deposited_total_m3 += rep;
            }
            inc + rep
        });
        let sediment_thickness_m = CellMap::from_fn(geo, |c| repose_delta_m.get(c).max(0.0));
        CarveDelta {
            delta_m,
            sediment_thickness_m,
            mouths: Vec::new(),
            eroded_total_m3,
            deposited_total_m3,
            ocean_loss_m3: 0.0,
        }
    }
}

/// Erodibility: how fast material yields (spec §4). Soft (low induration)
/// erodes; carbonate additionally dissolves. Total on `[0.25, 2.5]`; the
/// gated overlay may inject sentinels later without a formula change.
/// type-audit: bare-ok(ratio: induration), bare-ok(ratio: carbonate), bare-ok(ratio: return)
pub fn erodibility(induration: f64, carbonate: f64) -> f64 {
    (0.25 + 1.5 * (1.0 - induration.clamp(0.0, 1.0)) + 0.75 * carbonate.clamp(0.0, 1.0))
        .clamp(0.0, 2.5)
}

/// Stream-power incision along the provisional drainage tree: depth =
/// `k * (A/A0)^m * (S/S0)^n * erodibility`, capped, land-only (a cell
/// below sea level always returns `0.0` — the ocean floor is not this
/// module's business). A local minimum (no lower neighbor) also returns
/// `0.0`: deposition country, Task 8. Iteration is in ascending `CellId`
/// order (via `CellMap::from_fn`).
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: induration), bare-ok(ratio: carbonate), pending(wave-2: return)
pub fn carve_incision(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    induration: &CellMap<f64>,
    carbonate: &CellMap<f64>,
    params: &CarveParams,
) -> CellMap<f64> {
    CellMap::from_fn(geo, |cell| {
        if *elevation.get(cell) < sea_level {
            return 0.0;
        }
        let here = elevation.get(cell).get();
        let drop = geo
            .neighbors(cell)
            .iter()
            .map(|n| here - elevation.get(*n).get())
            .fold(0.0_f64, f64::max);
        if drop <= 0.0 {
            return 0.0; // local minimum: deposition country, Task 8
        }
        let a = (*drainage.get(cell) / params.area_ref).max(0.0);
        let s = drop / params.slope_ref_m;
        let power = math::powf(a, params.area_exponent) * math::powf(s, params.slope_exponent);
        -(params.incision_k_m * power * erodibility(*induration.get(cell), *carbonate.get(cell)))
            .min(params.max_incision_m)
    })
}

/// Fixed-sweep angle-of-repose relaxation: `params.repose_sweeps` Jacobi-
/// style sweeps over land cells. Each sweep computes every cell's outgoing
/// moves against the *previous* sweep's elevation and applies them only at
/// the sweep's end — order-independent within a sweep, hence deterministic
/// and symmetric regardless of cell iteration order (still ascending
/// `CellId`, per house style). For each land cell and each of its lower
/// land neighbors whose inter-cell drop exceeds `params.repose_drop_m`, a
/// quarter of the excess drop moves from the high cell to the low one.
/// Ocean cells, and any land-cell edge touching one, are skipped entirely
/// — sea cliffs are real; only inland slopes relax. Mass-conserving by
/// construction: every subtraction has a matching addition, so the
/// returned map sums to (approximately) zero.
///
/// **Deviation from the brief's abbreviated signature:** the brief's
/// interface stub lists three parameters, but its own prose requires
/// "ocean cells... skipped" — undecidable without a land/ocean threshold.
/// `sea_level` is therefore a fourth, explicit parameter here rather than
/// smuggled through `CarveParams` (which the brief pins to exactly its
/// listed fields) or inferred from the reference-elevation datum (0 m is
/// *not* sea level; see `ReferenceElevation`'s own doc).
/// type-audit: pending(wave-2: return)
pub fn apply_repose(
    geo: &Geosphere,
    elevation_after_incision: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    params: &CarveParams,
) -> CellMap<f64> {
    let n = geo.cell_count();
    let is_land = |c: CellId| *elevation_after_incision.get(c) >= sea_level;
    let mut total = vec![0.0_f64; n];
    for _ in 0..params.repose_sweeps {
        let mut sweep_delta = vec![0.0_f64; n];
        for c in geo.cells() {
            if !is_land(c) {
                continue;
            }
            let here = elevation_after_incision.get(c).get() + total[c.0 as usize];
            for &nb in geo.neighbors(c) {
                if !is_land(nb) {
                    continue; // ocean-adjacent drop: sea cliffs are real
                }
                let there = elevation_after_incision.get(nb).get() + total[nb.0 as usize];
                let drop = here - there;
                if drop > params.repose_drop_m {
                    let moved = (drop - params.repose_drop_m) * 0.25;
                    sweep_delta[c.0 as usize] -= moved;
                    sweep_delta[nb.0 as usize] += moved;
                }
            }
        }
        for (t, s) in total.iter_mut().zip(sweep_delta.iter()) {
            *t += *s;
        }
    }
    CellMap::from_fn(geo, |c| total[c.0 as usize])
}

/// Route eroded volume down the drainage tree: deposit on flats, fill
/// endorheic sinks toward playa floors, and export the rest at each
/// coastal outlet ("mouth"). Land cells only, processed in **descending
/// elevation order** (`total_cmp`, `CellId` ascending tiebreak) — the
/// downhill forest guarantees every upstream cell is processed before its
/// downstream target, so a single forward sweep suffices (no iteration to
/// convergence).
///
/// Per cell: `flux = -incision[cell] + inflow` (inflow already accumulated
/// from upstream cells processed earlier in the sweep). If the cell has a
/// downhill target and its own steepest neighbor drop is flatter than
/// `params.deposit_slope_m`, a `params.deposit_fraction` share of `flux`
/// deposits here first. The remainder then follows `downhill`: onto a land
/// cell, it becomes that cell's inflow; onto the ocean, the *source* cell
/// (the coastal outlet) is a river mouth and the flux is exported. With no
/// downhill target at all (a local minimum) the flat-fraction step is
/// skipped entirely — there is no "remainder to pass downhill", so ALL
/// arriving flux goes through the sink branch: deposited in full, capped,
/// for an endorheic sink, at raising the sink to its lowest rim neighbor
/// minus 1 m (fill toward flat, **never overtop** — the cap binds the
/// whole deposit, not an installment of it); any surplus above the cap is
/// booked as `ocean_loss` (the playa's aquifer — books stay balanced).
///
/// Returns `(deposit_m, mouths, ocean_loss)`: `deposit_m` is the
/// non-negative floodplain/playa deposition thickness; `mouths` lists each
/// river-mouth land cell with its exported volume, sorted by volume
/// descending (`CellId` ascending tiebreak); `ocean_loss` is the playa
/// overflow only — the marine wedge (Task 9) adds to it separately.
/// type-audit: pending(wave-2: incision), bare-ok(flag: endorheic), pending(wave-2: return)
#[allow(clippy::too_many_arguments)]
pub fn route_sediment(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    incision: &CellMap<f64>,
    downhill: &[Option<CellId>],
    endorheic: &CellMap<bool>,
    params: &CarveParams,
) -> (CellMap<f64>, Vec<(CellId, f64)>, f64) {
    let n = geo.cell_count();
    let is_land = |c: CellId| *elevation.get(c) >= sea_level;

    // Descending elevation, CellId ascending tiebreak — upstream before
    // downstream in a single forward pass.
    let mut order: Vec<CellId> = geo.cells().filter(|c| is_land(*c)).collect();
    order.sort_by(|a, b| {
        elevation
            .get(*b)
            .total_cmp(*elevation.get(*a))
            .then(a.0.cmp(&b.0))
    });

    let mut flux = vec![0.0_f64; n];
    for &c in &order {
        flux[c.0 as usize] = -*incision.get(c);
    }

    let mut deposit = vec![0.0_f64; n];
    let mut mouths_acc = vec![0.0_f64; n];
    let mut ocean_loss = 0.0_f64;

    for &c in &order {
        let idx = c.0 as usize;
        let here = elevation.get(c).get();
        let drop = geo
            .neighbors(c)
            .iter()
            .map(|nb| here - elevation.get(*nb).get())
            .fold(0.0_f64, f64::max);
        // The flat-fraction carve-out only applies where a remainder can
        // continue downhill; at a true sink (downhill None) it would
        // bypass the rim cap below (reviewer-measured on seed 42 L4: a
        // 385 m uncapped flat installment against 16.6 m of rim headroom),
        // so the sink branch handles 100% of the arriving flux instead.
        if downhill[idx].is_some() && drop < params.deposit_slope_m {
            let flat_share = params.deposit_fraction * flux[idx];
            deposit[idx] += flat_share;
            flux[idx] -= flat_share;
        }
        match downhill[idx] {
            Some(target) if is_land(target) => {
                flux[target.0 as usize] += flux[idx];
            }
            Some(_ocean_target) => {
                // This cell is the coastal outlet: a river mouth.
                mouths_acc[idx] += flux[idx];
            }
            None => {
                if *endorheic.get(c) {
                    // Playa fill: cap at the sink's lowest rim neighbor
                    // minus 1 m (never overtop); surplus is the aquifer.
                    let rim = geo
                        .neighbors(c)
                        .iter()
                        .map(|nb| elevation.get(*nb).get())
                        .fold(f64::INFINITY, f64::min);
                    let cap = (rim - 1.0 - here).max(0.0);
                    let take = flux[idx].min(cap);
                    deposit[idx] += take;
                    ocean_loss += flux[idx] - take;
                } else {
                    // A sea-level-adjacent pit, not marked endorheic:
                    // deposit all remaining flux, no cap.
                    deposit[idx] += flux[idx];
                }
            }
        }
    }

    let deposit_m = CellMap::from_fn(geo, |c| deposit[c.0 as usize]);
    let mut mouths: Vec<(CellId, f64)> = geo
        .cells()
        .filter(|c| mouths_acc[c.0 as usize] > 0.0)
        .map(|c| (c, mouths_acc[c.0 as usize]))
        .collect();
    mouths.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.0.cmp(&b.0.0)));

    (deposit_m, mouths, ocean_loss)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{Geosphere, Seed};

    /// Maximum land-to-land inter-cell drop over the whole globe, under a
    /// caller-supplied elevation function (lets the repose test compare
    /// "before" and "after" without building two full `CellMap`s).
    fn max_land_drop(
        geo: &Geosphere,
        is_land: impl Fn(CellId) -> bool,
        elev: impl Fn(CellId) -> f64,
    ) -> f64 {
        let mut max_drop = 0.0_f64;
        for c in geo.cells() {
            if !is_land(c) {
                continue;
            }
            for &nb in geo.neighbors(c) {
                if !is_land(nb) {
                    continue;
                }
                max_drop = max_drop.max(elev(c) - elev(nb));
            }
        }
        max_drop
    }

    #[test]
    fn incision_is_monotone_in_erodibility_and_capped() {
        // Synthetic 2-level probe: same drainage/slope, harder rock cuts less.
        assert!(erodibility(0.9, 0.0) < erodibility(0.2, 0.0));
        assert!(
            erodibility(0.5, 0.8) > erodibility(0.5, 0.0),
            "carbonate must dissolve"
        );

        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let inc = carve_incision(
            &geo,
            &g.elevation,
            g.sea_level,
            &g.drainage,
            &g.induration,
            &carbonate,
            &CarveParams::default(),
        );
        for (c, d) in inc.iter() {
            assert!(*d <= 0.0 && *d >= -CarveParams::default().max_incision_m);
            if *g.elevation.get(c) < g.sea_level {
                assert_eq!(*d, 0.0, "ocean incised");
            }
        }

        // Valleys, not ridges: incision correlates with drainage — the most
        // incised decile has higher median drainage than the least.
        let mut pairs: Vec<(f64, f64)> = geo
            .cells()
            .map(|c| (*inc.get(c), *g.drainage.get(c)))
            .collect();
        pairs.sort_by(|a, b| a.0.total_cmp(&b.0)); // ascending: most-negative (most incised) first
        let count = pairs.len();
        let decile = count / 10;
        let median = |values: &[f64]| -> f64 {
            let mut v = values.to_vec();
            v.sort_by(|a, b| a.total_cmp(b));
            v[v.len() / 2]
        };
        let most_incised_drainage: Vec<f64> =
            pairs[..decile].iter().map(|(_, drain)| *drain).collect();
        let least_incised_drainage: Vec<f64> = pairs[count - decile..]
            .iter()
            .map(|(_, drain)| *drain)
            .collect();
        assert!(
            median(&most_incised_drainage) > median(&least_incised_drainage),
            "most-incised decile should drain more than the least-incised decile"
        );
    }

    #[test]
    fn repose_conserves_mass_and_respects_the_critical_drop() {
        let geo = Geosphere::new(3);
        let outcome =
            crate::globe::generate(Seed(7), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let adjusted = apply_repose(&geo, &g.elevation, g.sea_level, &p);
        let total: f64 = adjusted.iter().map(|(_, d)| *d).sum();
        assert!(total.abs() < 1e-6, "repose created/destroyed mass: {total}");

        // After applying, the maximum land-land inter-cell drop must not
        // have grown; if a violation of the critical drop existed before,
        // it must have strictly shrunk (a fixed sweep count needn't
        // eliminate every violation, only reduce it).
        let is_land = |c: CellId| *g.elevation.get(c) >= g.sea_level;
        let before_max = max_land_drop(&geo, is_land, |c| g.elevation.get(c).get());
        let after_max = max_land_drop(&geo, is_land, |c| {
            g.elevation.get(c).get() + adjusted.get(c)
        });
        assert!(after_max <= before_max, "repose grew the maximum drop");
        if before_max > p.repose_drop_m {
            assert!(
                after_max < before_max,
                "repose did not shrink a pre-existing violation: before {before_max}, after {after_max}"
            );
        }
    }

    #[test]
    fn carve_delta_assembles_from_incision_and_repose_only() {
        let geo = Geosphere::new(3);
        let outcome =
            crate::globe::generate(Seed(7), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let incision = carve_incision(
            &geo,
            &g.elevation,
            g.sea_level,
            &g.drainage,
            &g.induration,
            &carbonate,
            &p,
        );
        let repose = apply_repose(&geo, &g.elevation, g.sea_level, &p);
        let delta = CarveDelta::from_incision_and_repose(&geo, &incision, &repose);

        assert!(
            delta.mouths.is_empty(),
            "marine deltas not wired until Task 9"
        );
        assert_eq!(
            delta.ocean_loss_m3, 0.0,
            "ocean routing not wired until Task 9"
        );
        assert!(delta.eroded_total_m3 >= 0.0);
        assert!(delta.deposited_total_m3 >= 0.0);
        for (c, d) in delta.delta_m.iter() {
            assert_eq!(*d, *incision.get(c) + *repose.get(c));
        }
        for (c, s) in delta.sediment_thickness_m.iter() {
            assert!(*s >= 0.0);
            assert_eq!(*s, repose.get(c).max(0.0));
        }
    }

    #[test]
    fn sediment_books_balance_and_mouths_collect_the_rest() {
        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let incision = carve_incision(
            &geo,
            &g.elevation,
            g.sea_level,
            &g.drainage,
            &g.induration,
            &carbonate,
            &p,
        );
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let (deposit, mouths, ocean_loss) = route_sediment(
            &geo,
            &g.elevation,
            g.sea_level,
            &incision,
            &downhill,
            &g.endorheic,
            &p,
        );
        let eroded: f64 = incision.iter().map(|(_, d)| -*d).sum();
        let deposited: f64 = deposit.iter().map(|(_, d)| *d).sum();
        let exported: f64 = mouths.iter().map(|(_, v)| v).sum::<f64>();
        assert!(
            (eroded - (deposited + exported + ocean_loss)).abs() < 1e-6 * eroded.max(1.0),
            "books: eroded {eroded} vs deposited {deposited} + exported {exported} + loss {ocean_loss}"
        );
        for (_, d) in deposit.iter() {
            assert!(*d >= 0.0);
        }
        // Mouths sorted by exported volume descending, CellId ascending tiebreak.
        for w in mouths.windows(2) {
            assert!(w[0].1 > w[1].1 || (w[0].1 == w[1].1 && w[0].0 < w[1].0));
        }
        // Endorheic sinks received deposit (playas) on any world that has them.
        if g.endorheic.iter().any(|(_, e)| *e) {
            assert!(
                g.endorheic
                    .iter()
                    .filter(|(_, e)| **e)
                    .any(|(c, _)| *deposit.get(c) > 0.0),
                "no playa fill"
            );
        }
    }

    #[test]
    fn playa_deposit_never_overtops_the_rim_cap() {
        // The rim cap binds the WHOLE sink deposit, not an installment:
        // review of the first cut measured a seed-42/L4 endorheic sink
        // taking a 385.082 m uncapped flat-fraction installment against
        // only 16.574 m of rim headroom. Every downhill-None land cell's
        // total deposit must respect fill-toward-flat, never-overtop.
        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let incision = carve_incision(
            &geo,
            &g.elevation,
            g.sea_level,
            &g.drainage,
            &g.induration,
            &carbonate,
            &p,
        );
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let (deposit, _mouths, _ocean_loss) = route_sediment(
            &geo,
            &g.elevation,
            g.sea_level,
            &incision,
            &downhill,
            &g.endorheic,
            &p,
        );
        let mut sinks = 0usize;
        for c in geo.cells() {
            if *g.elevation.get(c) < g.sea_level || downhill[c.0 as usize].is_some() {
                continue;
            }
            sinks += 1;
            let here = g.elevation.get(c).get();
            let rim = geo
                .neighbors(c)
                .iter()
                .map(|nb| g.elevation.get(*nb).get())
                .fold(f64::INFINITY, f64::min);
            let cap = (rim - 1.0 - here).max(0.0);
            assert!(
                *deposit.get(c) <= cap + 1e-9,
                "sink {} overtops its rim: deposit {} vs cap {}",
                c.0,
                deposit.get(c),
                cap
            );
        }
        assert!(sinks > 0, "seed 42 L4 must have interior sinks to test");
    }
}
