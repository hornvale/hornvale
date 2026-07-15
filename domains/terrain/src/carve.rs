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

use crate::boundaries::{BoundaryKind, CellBoundary};
use crate::elevation::TrailSeamount;
use crate::lithology::MarginPolarity;
use hornvale_kernel::{CellId, CellMap, Geosphere, NearestCellIndex, ReferenceElevation, math};

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
/// `CarveDelta::from_incision_and_repose` seeds `delta_m`,
/// `sediment_thickness_m`, `eroded_total_m3`, and `deposited_total_m3` from
/// incision + repose alone; [`carve`] (Task 9) folds routing's floodplain/
/// playa deposit and the marine wedge/delta/atoll contributions on top of
/// that same struct by assignment, and fills `mouths`, `delta_cells`,
/// `atoll_cells`, and `ocean_loss_m3` — every field is `pub`, so later tasks
/// extend a built `CarveDelta` rather than widening a constructor's
/// argument list. `waterfall_sites` stays empty until Task 11.
/// type-audit: pending(wave-2: delta_m), pending(wave-2: sediment_thickness_m), pending(wave-2: mouths), pending(wave-2: eroded_total_m3), pending(wave-2: deposited_total_m3), pending(wave-2: ocean_loss_m3)
#[derive(Debug, Clone, PartialEq)]
pub struct CarveDelta {
    /// Elevation delta to add to the base field, meters (± — incision
    /// subtracts, repose/deposition/wedge/delta/atoll all add).
    pub delta_m: CellMap<f64>,
    /// Deposited sediment thickness, meters (≥ 0): repose's receiver-side
    /// gains, routing's floodplain/playa deposit, the marine wedge/delta
    /// fill, and atoll cap material, all summed.
    pub sediment_thickness_m: CellMap<f64>,
    /// River mouths and their exported sediment volume, sorted by volume
    /// descending (`CellId` tiebreak). Filled by [`carve`] (Task 9).
    pub mouths: Vec<(CellId, f64)>,
    /// Total eroded volume proxy — a cell-area-weighted volume proxy
    /// (Σ depth, one unit per cell) — summing stream-power incision,
    /// hillslope repose's donor-side losses, and atoll volume (a paired
    /// biogenic source, spec §5: carbonate grown in place books on both
    /// sides of the ledger).
    pub eroded_total_m3: f64,
    /// Total deposited volume proxy: repose's receiver-side gains, routing
    /// deposit, marine wedge/delta fill, and atoll cap volume, all summed.
    pub deposited_total_m3: f64,
    /// Volume proxy routed to the ocean and never placed: routing's playa
    /// overflow plus the wedge's own loss (trench shares, uncappable
    /// shelf overflow). Always `0.0` until Task 9 routes incision's eroded
    /// material seaward.
    pub ocean_loss_m3: f64,
    /// Cells a river-mouth delta lobe raised above sea level (Task 9).
    /// Empty until [`carve`] runs.
    pub delta_cells: Vec<CellId>,
    /// Cells an atoll rim capped (Task 9). Empty until [`carve`] runs; feeds
    /// lithology's carbonate override (Task 10).
    pub atoll_cells: Vec<CellId>,
    /// Waterfall sites. Always empty in this task — Task 11 fills it.
    pub waterfall_sites: Vec<CellId>,
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
            delta_cells: Vec::new(),
            atoll_cells: Vec::new(),
            waterfall_sites: Vec::new(),
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

/// Coastal sediment wedge (Task 9, spec §5): spread each river mouth's
/// exported volume across nearby ocean cells, offshore-reach set by the
/// mouth's own land-margin polarity, then — for the top-`params.delta_count`
/// mouths by volume — carve a discrete subaerial delta lobe out of that same
/// export before it ever reaches the ordinary spread. Both contributions
/// land in the single returned marine-deposit map: a delta lobe is
/// sediment too, only piled high enough to clear the waterline.
///
/// **Deviation from the brief's two-tuple return**: the brief's interface
/// stub returns `(CellMap<f64>, f64)`. `CarveDelta::delta_cells` needs to
/// know exactly which cells a lobe raised; deriving that after the fact
/// from "did this cell cross sea level" would only be sound because
/// `wedge_freeboard_m` happens to keep the ordinary wedge strictly below
/// sea level — a fragile inference tied to a tunable default rather than a
/// fact this function enforces. Returning the list directly (a third tuple
/// element) is the boring, explicit choice; see `apply_repose`'s own
/// documented deviation for the precedent.
///
/// Mechanics, per mouth (processed volume-descending, `CellId` ascending
/// tiebreak — re-sorted defensively even though `route_sediment` already
/// returns them that way):
/// - **Delta** (top-K only): the mouth cell itself (hop 0) plus its
///   adjacent ocean cells (hop 1) each have a target elevation
///   `sea_level + delta_height_m * exp(-hop)`. The volume needed to raise
///   every target cell to its own target, summed, is debited from the
///   mouth's export before the wedge spread runs; if the export is short,
///   every target cell's raise scales down by the same ratio (a
///   proportionally lower lobe, never minting mass) and only cells that
///   still clear sea level after scaling are recorded in `delta_cells`.
///   These hop-1 cells are then excluded from this mouth's own wedge BFS
///   below — the two mechanisms never double-fill one cell.
/// - **Wedge**: BFS over ocean cells from the mouth's (non-delta) adjacent
///   ocean cells, frontier expansion in ascending `CellId` order, depth-
///   limited to `wedge_reach_active` hops on an active-margin mouth or
///   `wedge_reach_passive` otherwise (`margins.get(mouth)`; Oceanic/
///   Interior treated as passive). Every reachable cell (trench cells
///   included) gets a weight `exp(-hops/1.5)`; the mouth's remaining
///   export splits across them proportional to weight. A trench cell —
///   the whole oceanic side of a `CoastalRange` contact (an Andean margin
///   carries no offshore arc, so its entire seaward flank is the trench;
///   a5ba274 adjudication), or the SUBDUCTING side only of an `IslandArc`
///   contact (the overriding/arc side takes normal wedge fill; side test
///   identical to `elevation.rs`'s `arc_side = plate.id >
///   contact.other_plate`) — has zero fill capacity — modeled as an
///   ordinary cap of `0.0` rather than a hard BFS wall, so its computed
///   share flows through the same cap-overflow machinery every other
///   capped cell uses — and blocks further BFS propagation past it (the
///   trench is a barrier, not a permeable cell). Each cell's
///   cap is the shelf freeboard headroom *remaining after every earlier
///   mouth in this same call* (`cap_m` reads the shared `fill` buffer), so
///   two mouths whose reach overlaps one shelf cell can never jointly
///   overtop it. Overflow beyond a cell's cap redistributes, in one more
///   pass, proportional to weight among cells that were not capped in the
///   first pass; whatever still doesn't fit is `ocean_loss` (no third
///   pass).
///
/// type-audit: pending(wave-2: mouths), bare-ok(index: plate_of), pending(wave-2: return)
#[allow(clippy::too_many_arguments)]
pub fn deposit_wedge(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    mouths: &[(CellId, f64)],
    margins: &CellMap<MarginPolarity>,
    boundary: &CellMap<Option<CellBoundary>>,
    plate_of: &CellMap<u32>,
    params: &CarveParams,
) -> (CellMap<f64>, Vec<CellId>, f64) {
    let n = geo.cell_count();
    let is_ocean = |c: CellId| *elevation.get(c) < sea_level;
    // The trench is the SUBDUCTING side only: a CoastalRange contact's
    // whole oceanic side (no offshore arc on an Andean margin; a5ba274
    // adjudication), or an IslandArc cell on the non-arc side — the side
    // test replicates `elevation.rs`'s `arc_side = plate.id >
    // contact.other_plate` (a plate's `id` equals its index, so
    // `plate_of` carries it directly); `arc_side` true is the overriding
    // plate, which takes normal wedge fill.
    let is_trench = |c: CellId| match boundary.get(c) {
        Some(b) => match b.kind {
            BoundaryKind::CoastalRange => true,
            BoundaryKind::IslandArc => *plate_of.get(c) <= b.other_plate,
            _ => false,
        },
        None => false,
    };
    // Headroom below the shelf cap, meters, net of whatever this call has
    // already filled at `c` (by an earlier mouth, or this mouth's own
    // pass-1 share) — never negative. Trench cells get none at all.
    let cap_m = |c: CellId, fill: &[f64]| -> f64 {
        if is_trench(c) {
            return 0.0;
        }
        let headroom = (sea_level.get() - params.wedge_freeboard_m) - elevation.get(c).get();
        (headroom - fill[c.0 as usize]).max(0.0)
    };

    let mut fill = vec![0.0_f64; n];
    let mut ocean_loss = 0.0_f64;
    let mut delta_cells: Vec<CellId> = Vec::new();

    let mut sorted_mouths: Vec<(CellId, f64)> = mouths.to_vec();
    sorted_mouths.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.0.cmp(&b.0.0)));

    for (mouth_idx, &(mouth, volume)) in sorted_mouths.iter().enumerate() {
        if volume <= 0.0 {
            continue;
        }
        let mut remaining = volume;

        // Delta lobe: top-`delta_count` mouths only.
        let mut lobe_cells: Vec<CellId> = Vec::new();
        if (mouth_idx as u32) < params.delta_count {
            lobe_cells.push(mouth); // hop 0
            let mut ring: Vec<CellId> = geo
                .neighbors(mouth)
                .iter()
                .copied()
                .filter(|&c| is_ocean(c))
                .collect();
            ring.sort_by_key(|c| c.0);
            lobe_cells.extend(ring); // hop 1

            let target_of =
                |hop: u32| sea_level.get() + params.delta_height_m * math::exp(-(hop as f64));
            let needed: Vec<f64> = lobe_cells
                .iter()
                .enumerate()
                .map(|(i, &c)| {
                    let hop = u32::from(i != 0);
                    (target_of(hop) - elevation.get(c).get() - fill[c.0 as usize]).max(0.0)
                })
                .collect();
            let total_needed: f64 = needed.iter().sum();
            if total_needed > 0.0 {
                let ratio = (remaining / total_needed).min(1.0);
                for (&c, &need) in lobe_cells.iter().zip(needed.iter()) {
                    let raise = ratio * need;
                    if raise <= 0.0 {
                        continue;
                    }
                    fill[c.0 as usize] += raise;
                    if elevation.get(c).get() + fill[c.0 as usize] >= sea_level.get() {
                        delta_cells.push(c);
                    }
                }
                remaining -= ratio * total_needed;
            }
        }
        if remaining <= 0.0 {
            continue;
        }

        let reach = match margins.get(mouth) {
            MarginPolarity::Active => params.wedge_reach_active,
            MarginPolarity::Passive | MarginPolarity::Interior | MarginPolarity::Oceanic => {
                params.wedge_reach_passive
            }
        };
        if reach == 0 {
            ocean_loss += remaining;
            continue;
        }

        // Hop-1 cells already claimed by this mouth's own delta lobe are
        // off-limits to its wedge BFS (the mouth cell itself, hop 0, was
        // never ocean, so it was never wedge-eligible anyway).
        let exclude: std::collections::BTreeSet<CellId> =
            lobe_cells.iter().skip(1).copied().collect();

        let mut visited = vec![false; n];
        let mut hop_of: Vec<u32> = vec![0; n];
        let mut reachable: Vec<CellId> = Vec::new();

        let mut frontier: Vec<CellId> = geo
            .neighbors(mouth)
            .iter()
            .copied()
            .filter(|&c| is_ocean(c) && !exclude.contains(&c))
            .collect();
        frontier.sort_by_key(|c| c.0);
        for &c in &frontier {
            visited[c.0 as usize] = true;
            hop_of[c.0 as usize] = 1;
        }
        reachable.extend(frontier.iter().copied());

        let mut hop = 1u32;
        let mut cur = frontier;
        while hop < reach && !cur.is_empty() {
            let mut next: Vec<CellId> = Vec::new();
            for &c in &cur {
                if is_trench(c) {
                    continue; // the trench is a wall: nothing propagates past it
                }
                for &nb in geo.neighbors(c) {
                    if is_ocean(nb) && !visited[nb.0 as usize] {
                        visited[nb.0 as usize] = true;
                        next.push(nb);
                    }
                }
            }
            next.sort_by_key(|c| c.0);
            for &c in &next {
                hop_of[c.0 as usize] = hop + 1;
            }
            reachable.extend(next.iter().copied());
            cur = next;
            hop += 1;
        }
        if reachable.is_empty() {
            ocean_loss += remaining;
            continue;
        }

        let weights: Vec<f64> = reachable
            .iter()
            .map(|&c| math::exp(-(hop_of[c.0 as usize] as f64) / 1.5))
            .collect();
        let weight_sum: f64 = weights.iter().sum();

        let mut share: Vec<f64> = weights
            .iter()
            .map(|&w| remaining * w / weight_sum)
            .collect();
        let mut capped = vec![false; reachable.len()];
        let mut overflow = 0.0_f64;
        for (i, &c) in reachable.iter().enumerate() {
            let room = cap_m(c, &fill);
            if share[i] > room {
                overflow += share[i] - room;
                share[i] = room;
                capped[i] = true;
            }
        }
        if overflow > 0.0 {
            let uncapped_weight: f64 = (0..reachable.len())
                .filter(|&i| !capped[i])
                .map(|i| weights[i])
                .sum();
            if uncapped_weight > 0.0 {
                for (i, &c) in reachable.iter().enumerate() {
                    if capped[i] {
                        continue;
                    }
                    let extra = overflow * weights[i] / uncapped_weight;
                    let room = (cap_m(c, &fill) - share[i]).max(0.0);
                    let add = extra.min(room);
                    share[i] += add;
                    ocean_loss += extra - add;
                }
            } else {
                ocean_loss += overflow;
            }
        }
        for (i, &c) in reachable.iter().enumerate() {
            fill[c.0 as usize] += share[i];
        }
    }

    let marine_deposit = CellMap::from_fn(geo, |c| fill[c.0 as usize]);
    delta_cells.sort_by_key(|c| c.0);
    delta_cells.dedup();
    (marine_deposit, delta_cells, ocean_loss)
}

/// Atoll caps (Task 9, spec §5): warm, shallow-submerged trail seamounts
/// (Task 6) grow a carbonate rim to just under the surface. Only trail
/// entries with `age_index >= 2` are old enough to have drifted off the
/// live hotspot dome and cooled into reef-building range; each maps to its
/// nearest cell (no interpolation — a seamount and a grid cell are both
/// point features at this resolution), and each cell caps at most once
/// (a later seamount mapping to an already-capped cell is skipped).
///
/// **Deviation from the brief's single-`CellMap` return**: as with
/// `deposit_wedge`, `CarveDelta::atoll_cells` needs the exact cell list,
/// not a value re-derived from the fill map after the fact — returning it
/// directly is the explicit, boring choice.
/// type-audit: pending(wave-2: return)
pub fn cap_atolls(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    trail_seamounts: &[TrailSeamount],
    params: &CarveParams,
) -> (CellMap<f64>, Vec<CellId>) {
    let n = geo.cell_count();
    let index = NearestCellIndex::new(geo);
    let cap = sea_level.get() - params.atoll_freeboard_m;
    let floor = sea_level.get() - params.atoll_max_depth_m;

    let mut fill = vec![0.0_f64; n];
    let mut done = vec![false; n];
    let mut atoll_cells: Vec<CellId> = Vec::new();

    for seamount in trail_seamounts {
        if seamount.age_index < 2 {
            continue;
        }
        let cell = index.nearest_to_position(geo, seamount.position);
        if done[cell.0 as usize] {
            continue;
        }
        let e = elevation.get(cell).get();
        if e >= cap || e < floor {
            continue; // at/above the cap already, or too deep to reef-cap
        }
        let lat = math::asin(geo.position(cell)[2].clamp(-1.0, 1.0)).abs();
        if lat >= params.atoll_max_abs_lat {
            continue;
        }
        fill[cell.0 as usize] = cap - e;
        done[cell.0 as usize] = true;
        atoll_cells.push(cell);
    }

    atoll_cells.sort_by_key(|c| c.0);
    let raised = CellMap::from_fn(geo, |c| fill[c.0 as usize]);
    (raised, atoll_cells)
}

/// The full engine-A carve (spec §2 seam): incision → repose (on
/// elevation+incision) → routing → wedge → deltas → atolls (on
/// elevation+wedge, so a reef caps the seabed the wedge already built —
/// the composed atoll cell ends exactly at `sea_level - atoll_freeboard_m`,
/// never above it), composed into one `CarveDelta`. Still not wired into
/// `generate` (Task 10 does that) —
/// every input is a plain reference the caller already has (or builds
/// exactly as Task 10's wiring will; see the tests), so this function adds
/// no new draws and changes no world byte on its own.
///
/// Books: `eroded_total_m3` and the repose-only part of `deposited_total_m3`
/// come from `CarveDelta::from_incision_and_repose`; this function adds
/// routing's floodplain/playa deposit, the marine wedge/delta fill, and
/// atoll cap volume to `deposited_total_m3`, routing's playa overflow and
/// the wedge's own loss to `ocean_loss_m3`, and atoll volume to
/// `eroded_total_m3` too (a paired biogenic source+sink: carbonate grown in
/// place books on both sides of the ledger, spec §5's tier-aware mass
/// balance — never free mass). River-mouth exports (`mouths`' volumes) are
/// not a separate top-level bucket: `deposit_wedge` fully consumes each
/// mouth's export into wedge/delta deposit or its own `ocean_loss`, so the
/// two-term identity `eroded ≈ deposited + ocean_loss` holds end to end.
/// type-audit: bare-ok(count: drainage), bare-ok(flag: endorheic), bare-ok(ratio: induration), bare-ok(ratio: carbonate), bare-ok(index: plate_of)
#[allow(clippy::too_many_arguments)]
pub fn carve(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    endorheic: &CellMap<bool>,
    downhill: &[Option<CellId>],
    induration: &CellMap<f64>,
    carbonate: &CellMap<f64>,
    margins: &CellMap<MarginPolarity>,
    boundary: &CellMap<Option<CellBoundary>>,
    plate_of: &CellMap<u32>,
    trail_seamounts: &[TrailSeamount],
    params: &CarveParams,
) -> CarveDelta {
    let incision = carve_incision(
        geo, elevation, sea_level, drainage, induration, carbonate, params,
    );
    let elevation_after_incision = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation.get(c).get() + *incision.get(c))
            .expect("elevation plus incision is finite")
    });
    let repose = apply_repose(geo, &elevation_after_incision, sea_level, params);

    let mut delta = CarveDelta::from_incision_and_repose(geo, &incision, &repose);

    let (routing_deposit, mouths, routing_ocean_loss) = route_sediment(
        geo, elevation, sea_level, &incision, downhill, endorheic, params,
    );
    let (marine_deposit, delta_cells, wedge_ocean_loss) = deposit_wedge(
        geo, elevation, sea_level, &mouths, margins, boundary, plate_of, params,
    );
    // Atolls cap the seabed the wedge already built, not the raw pre-wedge
    // floor — the same fold-forward the incision→repose seam uses above.
    // Without it, wedge fill and atoll fill each capped against raw
    // elevation independently and their SUM overtopped the atoll freeboard
    // (review-measured on seed 42/L4: CellId(1965) composed to
    // sea_level + 222.322 m from raw -2713.534 + wedge 227.322 + atoll
    // 296.707).
    let elevation_after_wedge = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation.get(c).get() + *marine_deposit.get(c))
            .expect("elevation plus wedge fill is finite")
    });
    let (atoll_fill, atoll_cells) = cap_atolls(
        geo,
        &elevation_after_wedge,
        sea_level,
        trail_seamounts,
        params,
    );

    let routing_total: f64 = routing_deposit.iter().map(|(_, d)| *d).sum();
    let marine_total: f64 = marine_deposit.iter().map(|(_, d)| *d).sum();
    let atoll_total: f64 = atoll_fill.iter().map(|(_, d)| *d).sum();

    let old_delta_m = delta.delta_m.clone();
    let old_sediment = delta.sediment_thickness_m.clone();
    delta.delta_m = CellMap::from_fn(geo, |c| {
        *old_delta_m.get(c) + *routing_deposit.get(c) + *marine_deposit.get(c) + *atoll_fill.get(c)
    });
    delta.sediment_thickness_m = CellMap::from_fn(geo, |c| {
        *old_sediment.get(c) + *routing_deposit.get(c) + *marine_deposit.get(c) + *atoll_fill.get(c)
    });
    delta.mouths = mouths;
    delta.deposited_total_m3 += routing_total + marine_total + atoll_total;
    delta.ocean_loss_m3 += routing_ocean_loss + wedge_ocean_loss;
    delta.eroded_total_m3 += atoll_total;
    delta.delta_cells = delta_cells;
    delta.atoll_cells = atoll_cells;
    // delta.waterfall_sites stays empty; Task 11 fills it.

    delta
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

    #[test]
    fn the_wedge_builds_a_shelf_mode_wide_on_passive_margins() {
        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        // Build inputs exactly as Task 10's wiring will.
        let carbonate = CellMap::from_fn(&geo, |c| g.lithology.get(c).carbonate);
        let margins = CellMap::from_fn(&geo, |c| g.lithology.get(c).margin);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let delta = carve(
            &geo,
            &g.elevation,
            g.sea_level,
            &g.drainage,
            &g.endorheic,
            &downhill,
            &g.induration,
            &carbonate,
            &margins,
            &g.boundary,
            &g.plate_of,
            &g.trail_seamounts,
            &p,
        );
        // Shelf mode: marine deposit exists, never raises seabed above the cap.
        let cap = g.sea_level.get() - p.wedge_freeboard_m;
        let mut any_marine = false;
        for (c, dep) in delta.sediment_thickness_m.iter() {
            let e = g.elevation.get(c).get();
            // Atoll cells are excluded here alongside delta cells: an
            // atoll's own freeboard (default 5 m) is deliberately shallower
            // than the ordinary wedge's (default 40 m) — a reef breaks the
            // surface closer to sea level than the shelf ever does — so any
            // atoll-capped cell structurally exceeds this wedge-specific
            // cap by design, exactly like a delta lobe. Verified on seed
            // 42/L4: an atoll cell landed at sea_level-5 while carrying a
            // legitimate wedge deposit underneath it too, both correct on
            // their own terms.
            if e < g.sea_level.get()
                && *dep > 0.0
                && !delta.delta_cells.contains(&c)
                && !delta.atoll_cells.contains(&c)
            {
                any_marine = true;
                assert!(
                    e + delta.delta_m.get(c) <= cap + 1e-9,
                    "wedge overtopped the cap"
                );
            }
        }
        assert!(any_marine, "no shelf built");
        // Deltas: exactly min(delta_count, mouths) lobes, subaerial.
        assert!(delta.delta_cells.len() as u32 <= p.delta_count);
        for c in &delta.delta_cells {
            assert!(
                g.elevation.get(*c).get() + delta.delta_m.get(*c) >= g.sea_level.get(),
                "delta lobe stayed submerged"
            );
        }
        // Atolls: the COMPOSED post-carve elevation of every atoll cell must
        // respect the atoll's own freeboard — the cap must bind the final
        // pipeline output, not the raw pre-wedge elevation (review finding:
        // seed 42/L4 CellId(1965) once ended +222 m ABOVE sea level because
        // wedge fill and atoll fill each capped against raw elevation
        // independently).
        let atoll_cap = g.sea_level.get() - p.atoll_freeboard_m;
        for c in &delta.atoll_cells {
            assert!(
                g.elevation.get(*c).get() + delta.delta_m.get(*c) <= atoll_cap + 1e-9,
                "atoll cell {} composed above its freeboard: {} vs cap {}",
                c.0,
                g.elevation.get(*c).get() + delta.delta_m.get(*c),
                atoll_cap
            );
        }
        // Books, full pipeline.
        assert!(
            (delta.eroded_total_m3 - (delta.deposited_total_m3 + delta.ocean_loss_m3)).abs()
                < 1e-6 * delta.eroded_total_m3.max(1.0)
        );
    }

    #[test]
    fn trench_cells_get_zero_reach_and_their_share_is_lost() {
        // Synthetic probe: every one of a single mouth's ocean neighbors
        // sits on the SUBDUCTING side of an island-arc boundary (own plate
        // 0 vs other_plate 1: arc_side false → trench). With every
        // reachable cell capped at zero and nowhere uncapped to
        // redistribute to, the mouth's whole export must land in
        // ocean_loss, and no trench cell may receive any fill.
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let mouth = CellId(0);
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == mouth {
                ReferenceElevation::new(50.0).unwrap()
            } else {
                ReferenceElevation::new(-500.0).unwrap()
            }
        });
        let margins = CellMap::from_fn(&geo, |_| MarginPolarity::Passive);
        let boundary = CellMap::from_fn(&geo, |_| {
            Some(CellBoundary {
                kind: BoundaryKind::IslandArc,
                magnitude: 1.0,
                other_plate: 1,
            })
        });
        let plate_of = CellMap::from_fn(&geo, |_| 0u32); // 0 <= 1: subducting side
        let p = CarveParams {
            delta_count: 0, // isolate the wedge/trench mechanic from deltas
            ..CarveParams::default()
        };
        let mouths = vec![(mouth, 1000.0)];
        let (marine_deposit, delta_cells, ocean_loss) = deposit_wedge(
            &geo, &elevation, sea, &mouths, &margins, &boundary, &plate_of, &p,
        );
        assert!(delta_cells.is_empty());
        for &nb in geo.neighbors(mouth) {
            assert_eq!(
                *marine_deposit.get(nb),
                0.0,
                "trench cell {} got fill",
                nb.0
            );
        }
        assert!(
            (ocean_loss - 1000.0).abs() < 1e-6,
            "every reachable cell is a trench: the whole export must be lost, got {ocean_loss}"
        );
    }

    #[test]
    fn island_arc_trench_takes_only_the_subducting_side() {
        // Same island-arc contact everywhere, but the mouth's ocean
        // neighbors are split between the two plates: cells on plate 2
        // (arc_side: 2 > 1, the overriding plate) must receive normal
        // wedge fill; cells on plate 0 (0 <= 1, the subducting plate) are
        // the trench and must receive none.
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let mouth = CellId(0);
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == mouth {
                ReferenceElevation::new(50.0).unwrap()
            } else {
                ReferenceElevation::new(-500.0).unwrap()
            }
        });
        let margins = CellMap::from_fn(&geo, |_| MarginPolarity::Passive);
        let boundary = CellMap::from_fn(&geo, |_| {
            Some(CellBoundary {
                kind: BoundaryKind::IslandArc,
                magnitude: 1.0,
                other_plate: 1,
            })
        });
        // Alternate the mouth's neighbors between the overriding plate (2)
        // and the subducting plate (0), by parity of CellId.
        let plate_of = CellMap::from_fn(&geo, |c| if c.0 % 2 == 0 { 2u32 } else { 0u32 });
        let p = CarveParams {
            delta_count: 0, // isolate the wedge/trench mechanic from deltas
            ..CarveParams::default()
        };
        let mouths = vec![(mouth, 1000.0)];
        let (marine_deposit, _delta_cells, _ocean_loss) = deposit_wedge(
            &geo, &elevation, sea, &mouths, &margins, &boundary, &plate_of, &p,
        );
        let mut overriding_filled = 0usize;
        for &nb in geo.neighbors(mouth) {
            if *plate_of.get(nb) > 1 {
                assert!(
                    *marine_deposit.get(nb) > 0.0,
                    "overriding-side cell {} got no wedge fill",
                    nb.0
                );
                overriding_filled += 1;
            } else {
                assert_eq!(
                    *marine_deposit.get(nb),
                    0.0,
                    "subducting-side cell {} got fill",
                    nb.0
                );
            }
        }
        assert!(
            overriding_filled > 0,
            "the probe needs at least one overriding-side neighbor"
        );
    }

    #[test]
    fn cap_atolls_caps_a_deep_submerged_trail_seamount_within_range() {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let target = CellId(0);
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == target {
                ReferenceElevation::new(-200.0).unwrap()
            } else {
                ReferenceElevation::new(-3000.0).unwrap()
            }
        });
        let p = CarveParams::default();
        let pos = geo.position(target);
        let seamounts = vec![
            TrailSeamount {
                position: pos,
                strength_m: 2000.0,
                age_index: 2,
            },
            // Too young (age_index < 2): must not cap anything.
            TrailSeamount {
                position: geo.position(CellId(1)),
                strength_m: 2000.0,
                age_index: 1,
            },
        ];
        let (fill, atoll_cells) = cap_atolls(&geo, &elevation, sea, &seamounts, &p);
        assert_eq!(atoll_cells, vec![target]);
        let expected = sea.get() - p.atoll_freeboard_m - elevation.get(target).get();
        assert!((*fill.get(target) - expected).abs() < 1e-9);
        for c in geo.cells() {
            if c != target {
                assert_eq!(*fill.get(c), 0.0);
            }
        }
    }
}
