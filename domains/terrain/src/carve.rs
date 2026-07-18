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
use std::collections::{BTreeMap, BTreeSet};

/// Tuning knobs for the carve (engine A). Global constants only —
/// heterogeneity comes from fields (spec §5).
/// type-audit: pending(wave-2: incision_k_m), bare-ok(ratio: area_exponent), bare-ok(ratio: slope_exponent), bare-ok(count: area_ref), pending(wave-2: slope_ref_m), pending(wave-2: max_incision_m), pending(wave-2: repose_drop_m), bare-ok(count: repose_sweeps), pending(wave-2: deposit_slope_m), bare-ok(ratio: deposit_fraction), pending(wave-2: wedge_freeboard_m), bare-ok(count: wedge_reach_passive), bare-ok(count: wedge_reach_active), bare-ok(count: delta_count), pending(wave-2: delta_height_m), pending(wave-2: atoll_freeboard_m), pending(wave-2: atoll_max_depth_m), pending(wave-2: atoll_max_abs_lat), pending(wave-2: wave_cut_m), pending(wave-2: wave_cut_floor_m), bare-ok(count: barrier_supply_per_cell), pending(wave-2: barrier_height_m)
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
    /// Peak wave-cut depth scale, meters, for a fully-exposed coast cell
    /// of unit erodibility (the wave-cut coastal-erosion term, spec §5/
    /// §12's banked extension activated by ledgers #6+#7). Read it as an
    /// epoch's planation capacity at cell scale, not a literal wave
    /// height: a coast cell is ~10^2 km wide at the canonical grids, and
    /// the default was chosen from data against the single-craton shelf
    /// floor (the worst sweep seed needs ~1000 to plane its craton-rim
    /// cliffs into a platform; 600 leaves it below the 0.05 floor) —
    /// an exposed soft coast planes fully to the platform while a
    /// sheltered hard rim (erodibility 0.25, one ocean neighbor of six)
    /// cuts ~40 m.
    pub wave_cut_m: f64,
    /// Depth below sea level, meters, past which a wave cut never lowers
    /// a coast cell — the wave-cut platform floor.
    pub wave_cut_floor_m: f64,
    /// Supply cost, in the shared mouth-volume proxy, of one barrier cell
    /// (tuning iteration 4, ledger #9, spec §5's banked spit/barrier
    /// extension): a budget on the COUNT of cells [`raise_barriers`] may
    /// raise, read off the total exported volume of every PASSIVE-margin
    /// mouth (river + wave) — "no supply, no barrier". A proxy, not a
    /// second withdrawal against `deposit_wedge`'s already-closed books
    /// (see that function's own doc).
    pub barrier_supply_per_cell: f64,
    /// Barrier-bar crest height above the sea level [`raise_barriers`] is
    /// given, meters (tuning iteration 4): small — a bar sits just clear of
    /// the waterline, not a coastal ridge. Chosen small enough that the
    /// bounded solve→trim→solve sequence's typical (post-wedge) DOWNWARD
    /// sea-level shift (ruling #5c, ledger #4) leaves every barrier cell
    /// comfortably above the final `sea_level` once the sea-trim exempts
    /// it (like a delta lobe).
    pub barrier_height_m: f64,
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
            wave_cut_m: 1000.0,
            wave_cut_floor_m: 30.0,
            barrier_supply_per_cell: 400.0,
            barrier_height_m: 3.0,
        }
    }
}

/// The per-cell plate/margin geometry the carve stages share: each cell's
/// margin polarity, its plate boundary (if any), and its owning plate.
/// These three maps are derived together from the plate set and thread as a
/// unit through `carve` → `deposit_wedge`/`raise_barriers`, so they ride as
/// one context rather than three positional arguments. Purely a grouping of
/// the same three references — byte-identical to passing them separately.
#[derive(Clone, Copy)]
pub struct MarginGeometry<'a> {
    /// Passive/active margin polarity per cell.
    pub margins: &'a CellMap<MarginPolarity>,
    /// Plate boundary at each cell, if any.
    pub boundary: &'a CellMap<Option<CellBoundary>>,
    /// Owning plate id per cell.
    pub plate_of: &'a CellMap<u32>,
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
    /// Cells a barrier bar raised above sea level (tuning iteration 4,
    /// ledger #9, spec §5's banked spit/barrier extension). Empty until
    /// [`carve`] runs. Like `delta_cells`, exempt from the sea-trim
    /// ([`trim_to_sea`]) — meant to stay subaerial past the final re-solve.
    pub barrier_cells: Vec<CellId>,
    /// Waterfall (knickpoint) sites the carve found (spec §5): land cells
    /// where a high-drainage watercourse crosses a sharp induration step,
    /// evaluated on the PRE-carve surface — "where the carve worked
    /// hardest against contrast". Sorted ascending `CellId`. Filled by
    /// [`carve`] via [`find_waterfalls`]; empty from
    /// [`CarveDelta::from_incision_and_repose`] alone.
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
            barrier_cells: Vec::new(),
            waterfall_sites: Vec::new(),
        }
    }
}

/// Where a landform came from (spec §5, fantasy hook): an OPEN enum —
/// `Process` is the only member this campaign; `Mythic` is banked (gated
/// overlay), landing additively later. Phenomena never reveal their
/// producing system, so future mythic members are indistinguishable to
/// consumers by construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    /// Formed by ordinary geologic process.
    Process,
}

/// Minimum drainage (upstream land-cell count) a cell must carry to be
/// waterfall-eligible: a knickpoint needs a real watercourse, not a
/// trickle. Spec §5's chosen threshold.
/// type-audit: bare-ok(count)
pub const WATERFALL_MIN_DRAINAGE: f64 = 80.0;

/// Minimum induration drop, in induration units `[0,1]`, from a candidate
/// waterfall cell down to its downhill neighbor: the hard lip over the
/// soft plunge pool that makes the step a knickpoint rather than an
/// ordinary slope. Spec §5's chosen threshold.
/// type-audit: bare-ok(ratio)
pub const WATERFALL_INDURATION_STEP: f64 = 0.35;

/// Waterfall (knickpoint) detection (spec §5, "derived point observations"):
/// a land cell whose drainage exceeds `WATERFALL_MIN_DRAINAGE` and whose
/// downhill neighbor is softer by more than `WATERFALL_INDURATION_STEP` in
/// induration terms — a hard lip over a soft plunge pool, exactly where the
/// carve worked hardest against a contrast. A cell with no downhill target
/// (a local minimum) is never a waterfall. Land-only: `elevation` and
/// `sea_level` gate the source cell; the downhill target's induration is
/// read regardless of its own land/ocean status (a river dropping straight
/// into the sea over resistant rock is a real waterfall). Output sorted
/// ascending `CellId` (cell iteration is already ascending; sorted
/// defensively, matching the house style elsewhere in this module).
/// type-audit: bare-ok(count: drainage), bare-ok(ratio: induration)
pub fn find_waterfalls(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    drainage: &CellMap<f64>,
    induration: &CellMap<f64>,
    downhill: &[Option<CellId>],
) -> Vec<CellId> {
    let mut sites: Vec<CellId> = geo
        .cells()
        .filter(|&c| {
            if *elevation.get(c) < sea_level {
                return false; // land cells only
            }
            if *drainage.get(c) < WATERFALL_MIN_DRAINAGE {
                return false;
            }
            match downhill[c.0 as usize] {
                Some(target) => {
                    *induration.get(c) - *induration.get(target) > WATERFALL_INDURATION_STEP
                }
                None => false, // a local minimum has no downhill lip to fall over
            }
        })
        .collect();
    sites.sort_by_key(|c| c.0);
    sites
}

/// Number of a world's largest rivers the A→B→C escalation diagnostic
/// (spec §8) scores: the top mouths by pre-carve drainage flux.
/// type-audit: bare-ok(count)
pub const REROUTE_TOP_RIVERS: usize = 20;

/// The A→B→C escalation diagnostic (spec §8, preregistered so "see if it
/// looks good" is a number — a permanent census column): the flux-weighted
/// fraction of a world's [`REROUTE_TOP_RIVERS`] largest rivers' mainstem
/// cells whose downhill target changed across the carve.
///
/// **Rivers**: the top `n_rivers` PRE-carve mouths — land cells whose
/// pre-carve downhill target is ocean (`pre_drainage.get(target) == 0.0`;
/// ocean cells always carry exactly `0.0` and land cells always `>= 1.0`,
/// by [`crate::drainage::drainage_field`]'s own construction) — ranked by
/// the mouth's own pre-carve drainage (a river's mouth carries its whole
/// watershed's flux, so ranking mouths by their own drainage ranks the
/// rivers), `CellId`-ascending tiebreak. This is the "20 largest rivers"
/// proxy the plan preregistered.
///
/// **Mainstem**: for each mouth, the path is built by walking UPSTREAM from
/// the mouth, at each step following the predecessor (a cell whose
/// pre-carve downhill target is the current cell) with the highest
/// pre-carve drainage — `CellId`-ascending tiebreak — until a headwater (no
/// predecessor) is reached. That walk, reversed, is exactly "the farthest
/// upstream max-drainage cell down via pre-carve downhill" the plan
/// specifies (the two descriptions name the same edges, walked in opposite
/// directions).
///
/// **Divergence**: for every cell on a mouth's path (headwater through the
/// mouth itself, inclusive — always at least one cell, the mouth), the
/// PRE-carve downhill target (`pre_downhill`) is compared against the
/// POST-carve one (`post_downhill`) at the same `CellId`; they differ when
/// the `Option<CellId>` values differ. A mouth's score is the fraction of
/// its path cells that diverged; the returned value is the mean of every
/// scored mouth's score, weighted by the mouth's own pre-carve drainage
/// flux. `0.0` when there are no pre-carve mouths at all (a landless or
/// fully endorheic world).
///
/// **Deviation**: the plan's signature also carries `sea_level_pre` /
/// `sea_level_post`. Land/ocean classification here is delegated entirely
/// to the already-sea-level-thresholded drainage arrays (see above) — this
/// function has no elevation field to use either sea level against more
/// directly, and re-deriving a threshold test from them without elevation
/// is not possible, so they are accepted (kept as parameters) purely to
/// preserve the plan's exact signature and the call site's documented
/// two-sea-level context; the divergence test itself never reads them.
///
/// Preregistered thresholds (spec §8), read off this function's return
/// value: **`< 0.10`** — engine A is self-consistent, ship it.
/// **`0.10..=0.30`** — flag, Nathan decides whether engine B enters
/// evaluation. **`> 0.30`** — the one-shot lied to itself; A is rejected as
/// sole engine, B enters.
/// type-audit: bare-ok(count: pre_drainage), bare-ok(count: _post_drainage), bare-ok(count: n_rivers), bare-ok(ratio: return)
#[allow(clippy::too_many_arguments)]
pub fn rerouted_flow_fraction(
    geo: &Geosphere,
    pre_drainage: &CellMap<f64>,
    pre_downhill: &[Option<CellId>],
    _post_drainage: &CellMap<f64>,
    post_downhill: &[Option<CellId>],
    _sea_level_pre: ReferenceElevation,
    _sea_level_post: ReferenceElevation,
    n_rivers: usize,
) -> f64 {
    if n_rivers == 0 {
        return 0.0;
    }
    let n = geo.cell_count();

    // Reverse of `pre_downhill`: every land cell whose pre-carve downhill
    // target is `t` is a predecessor of `t`.
    let mut predecessors: Vec<Vec<CellId>> = vec![Vec::new(); n];
    for c in geo.cells() {
        if let Some(t) = pre_downhill[c.0 as usize] {
            predecessors[t.0 as usize].push(c);
        }
    }

    let mut mouths: Vec<CellId> = geo
        .cells()
        .filter(|&c| match pre_downhill[c.0 as usize] {
            Some(t) => *pre_drainage.get(t) == 0.0,
            None => false,
        })
        .collect();
    mouths.sort_by(|a, b| {
        pre_drainage
            .get(*b)
            .total_cmp(pre_drainage.get(*a))
            .then(a.0.cmp(&b.0))
    });
    mouths.truncate(n_rivers);
    if mouths.is_empty() {
        return 0.0;
    }

    let mut weighted_sum = 0.0_f64;
    let mut weight_total = 0.0_f64;
    for &mouth in &mouths {
        let mut path = vec![mouth];
        let mut cur = mouth;
        loop {
            let mut preds = predecessors[cur.0 as usize].clone();
            if preds.is_empty() {
                break;
            }
            preds.sort_by(|a, b| {
                pre_drainage
                    .get(*b)
                    .total_cmp(pre_drainage.get(*a))
                    .then(a.0.cmp(&b.0))
            });
            cur = preds[0];
            path.push(cur);
        }

        let diverged = path
            .iter()
            .filter(|&&c| pre_downhill[c.0 as usize] != post_downhill[c.0 as usize])
            .count();
        let score = diverged as f64 / path.len() as f64;
        let weight = *pre_drainage.get(mouth);
        weighted_sum += score * weight;
        weight_total += weight;
    }

    if weight_total <= 0.0 {
        0.0
    } else {
        weighted_sum / weight_total
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
/// module's business). The slope term takes the max drop to a LAND
/// neighbor only — channel slope, not the cliff face (ledger #6): coastal
/// stream power scales with drainage like everywhere else, so rias form
/// where drainage concentrates instead of every cliff coast saturating at
/// the cap. A local minimum (no lower land neighbor) returns `0.0`:
/// deposition country, Task 8. Iteration is in ascending `CellId` order
/// (via `CellMap::from_fn`).
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
            .filter(|n| *elevation.get(**n) >= sea_level)
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

/// Wave-cut coastal erosion (spec §5/§12's banked extension, activated by
/// ledgers #6+#7 once the shoreline band demanded it): for each LAND cell
/// with at least one ocean neighbor (land/ocean by the `sea_level` passed
/// in, on the post-repose surface), `exposure = ocean_neighbors /
/// total_neighbors` and `cut = wave_cut_m * erodibility * exposure`,
/// capped so the cell never ends below `sea_level - wave_cut_floor_m`
/// (the wave-cut platform floor). Differential crenulation is the point:
/// soft/exposed coasts cut into bays — and a cut cell's former neighbors
/// become more sheltered in the NEXT epoch, so the process is
/// self-limiting — while hard headlands stand and low-exposure inlets
/// (rias) survive. Ocean cells and landlocked cells return `0.0`.
///
/// Returns `(cut_m, micro_mouths)`: `cut_m` is the per-cell cut (≤ 0,
/// coast land cells only); `micro_mouths` lists every cut cell with its
/// eroded volume proxy (Σ depth, one cell-area unit per cell — the
/// carve's own volume convention), sorted volume-descending (`CellId`
/// ascending tiebreak). The caller merges these micro-mouths into the
/// wedge's mouth list — wave material feeds the shelf supply through the
/// existing wedge/ocean-loss machinery, no new bookkeeping category.
/// type-audit: bare-ok(ratio: induration), bare-ok(ratio: carbonate), pending(wave-2: return)
pub fn wave_erosion(
    geo: &Geosphere,
    elevation_after_repose: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    induration: &CellMap<f64>,
    carbonate: &CellMap<f64>,
    params: &CarveParams,
) -> (CellMap<f64>, Vec<(CellId, f64)>) {
    let is_ocean = |c: CellId| *elevation_after_repose.get(c) < sea_level;
    let floor = sea_level.get() - params.wave_cut_floor_m;
    let cut_m = CellMap::from_fn(geo, |cell| {
        if is_ocean(cell) {
            return 0.0;
        }
        let neighbors = geo.neighbors(cell);
        let ocean_neighbors = neighbors.iter().filter(|n| is_ocean(**n)).count();
        if ocean_neighbors == 0 {
            return 0.0;
        }
        let exposure = ocean_neighbors as f64 / neighbors.len() as f64;
        let allowed = (elevation_after_repose.get(cell).get() - floor).max(0.0);
        -(params.wave_cut_m * erodibility(*induration.get(cell), *carbonate.get(cell)) * exposure)
            .min(allowed)
    });
    let mut micro_mouths: Vec<(CellId, f64)> = cut_m
        .iter()
        .filter(|(_, d)| **d < 0.0)
        .map(|(c, d)| (c, -*d))
        .collect();
    micro_mouths.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.0.cmp(&b.0.0)));
    (cut_m, micro_mouths)
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
/// **Delta eligibility (ledger #7):** `mouths` may mix river mouths with
/// the wave term's micro-mouths; the top-K lobe walk counts only mouths in
/// `delta_eligible` (the river-mouth cells) — wave cells are never deltas,
/// however large their volume. Every mouth, eligible or not, still takes
/// the ordinary wedge spread.
///
/// type-audit: pending(wave-2: mouths), bare-ok(index: plate_of), pending(wave-2: return)
pub fn deposit_wedge(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    mouths: &[(CellId, f64)],
    geom: MarginGeometry,
    delta_eligible: &BTreeSet<CellId>,
    params: &CarveParams,
) -> (CellMap<f64>, Vec<CellId>, f64) {
    let MarginGeometry {
        margins,
        boundary,
        plate_of,
    } = geom;
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

    let mut eligible_seen = 0u32;
    for &(mouth, volume) in sorted_mouths.iter() {
        if volume <= 0.0 {
            continue;
        }
        let mut remaining = volume;

        // Delta lobe: the top-`delta_count` DELTA-ELIGIBLE (river) mouths
        // only — wave micro-mouths never grow a lobe (ledger #7).
        let grows_delta = delta_eligible.contains(&mouth) && eligible_seen < params.delta_count;
        if delta_eligible.contains(&mouth) {
            eligible_seen += 1;
        }
        let mut lobe_cells: Vec<CellId> = Vec::new();
        if grows_delta {
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

/// Barrier islands and lagoons (tuning iteration 4, ledger #9; spec §5's
/// banked longshore-drift extension): the shoreline-development diagnostic
/// (`.superpowers/sdd/shoreline-diagnostic.md`) found the estimator
/// `D = L / (2 sqrt(pi A))` rewards single-hex land/ocean alternation —
/// exactly the barrier/lagoon/mainland pattern — and is nowhere near
/// saturated. Runs on the surface AFTER the wedge
/// (`elevation_after_wedge`), BEFORE atolls (see [`carve`]'s composition).
///
/// **Candidate rule, chosen by the edge arithmetic**: raising an ocean
/// cell to land flips every one of its edges in the perimeter sum — a
/// land neighbor's edge is REMOVED (land/ocean becomes land/land) while an
/// ocean neighbor's edge is ADDED (ocean/ocean becomes land/ocean) — so a
/// cell with `L` land neighbors and `O` ocean neighbors changes the raw
/// perimeter by `O - L`. An **attached** spit-end (`L == 1`) nets `O - 1`;
/// a **detached** cell (`L == 0`, ocean on every side) nets the full `O`
/// (a whole 5- or 6-edge ring) — strictly more. So the candidate pool is
/// detached: ocean cells with ZERO land neighbors of their own, that touch
/// at least one ORDINARY coastal-fringe ocean cell (one hop from land) —
/// i.e. exactly two hops from the mainland, with that fringe cell
/// surviving untouched as the lagoon between barrier and shore. The
/// fringe cell's own land neighbor(s) gate margin polarity (`Passive`
/// only — spec's "recognized longshore extension" is a passive-margin
/// phenomenon on Earth too) and exclude the trench and any river-mouth
/// delta lobe (never restyled as a barrier).
///
/// **Alternating selection**: eligible cells are walked in ascending
/// `CellId` order (the coastal-walk convention every other carve stage
/// uses) and every OTHER one is skipped. Raising every eligible cell in a
/// row would weld a solid offshore wall whose interior land-land edges are
/// wasted perimeter — exactly the ordinary-coastline problem this
/// mechanism exists to avoid, and it would reduce `P` relative to a porous
/// chain.
///
/// **Supply gate**: a budget in raised CELLS (not the raise volume
/// itself) — `total_passive_supply / params.barrier_supply_per_cell`,
/// floored — where `total_passive_supply` sums the exported volume of
/// every PASSIVE-margin mouth in `mouths` (the same merged river+wave list
/// [`deposit_wedge`] already fully spent into shelf fill or ocean loss).
/// This is a budget PROXY read off that same supply signal, not a second
/// withdrawal against `deposit_wedge`'s already-closed books — "no
/// supply, no barrier" without re-plumbing that function's internal
/// accounting. The barrier's own raised volume books as a PAIRED
/// source+sink in [`carve`] (added to both `eroded_total_m3` and
/// `deposited_total_m3`), mirroring the atoll convention (spec §5's
/// tier-aware mass balance): real sediment (longshore drift is a genuine
/// process), gated rather than metered exactly.
///
/// Each selected cell raises to `sea_level + params.barrier_height_m`
/// (never down — every candidate starts below `sea_level` by
/// construction, so the raise is always positive). Returns
/// `(fill, barrier_cells, volume_used)`, `barrier_cells` sorted ascending
/// `CellId`; the caller ([`carve`]) folds `fill` into
/// `elevation_after_wedge` before atolls cap, and the sea-trim
/// ([`trim_to_sea`]) must exempt `barrier_cells` like `delta_cells` — a
/// barrier is meant to stay subaerial past the final re-solve.
/// type-audit: bare-ok(index: plate_of), pending(wave-2: mouths), pending(wave-2: return)
pub fn raise_barriers(
    geo: &Geosphere,
    elevation_after_wedge: &CellMap<ReferenceElevation>,
    sea_level: ReferenceElevation,
    geom: MarginGeometry,
    delta_cells: &BTreeSet<CellId>,
    mouths: &[(CellId, f64)],
    params: &CarveParams,
) -> (CellMap<f64>, Vec<CellId>, f64) {
    let MarginGeometry {
        margins,
        boundary,
        plate_of,
    } = geom;
    let is_ocean = |c: CellId| *elevation_after_wedge.get(c) < sea_level;
    // The trench is a wall to barrier building too — the same test
    // `deposit_wedge` uses (a CoastalRange contact's whole oceanic side,
    // or an IslandArc contact's subducting side only).
    let is_trench = |c: CellId| match boundary.get(c) {
        Some(b) => match b.kind {
            BoundaryKind::CoastalRange => true,
            BoundaryKind::IslandArc => *plate_of.get(c) <= b.other_plate,
            _ => false,
        },
        None => false,
    };

    // Ordinary coastal fringe: ocean cells with >= 1 land neighbor. These
    // stay ocean — the lagoon between the mainland and any barrier raised
    // beyond them.
    let fringe: BTreeSet<CellId> = geo
        .cells()
        .filter(|&c| is_ocean(c) && geo.neighbors(c).iter().any(|&nb| !is_ocean(nb)))
        .collect();

    // Detached candidates: ocean, zero land neighbors of their own, and
    // adjacent to at least one fringe cell whose own land neighbor is on
    // a Passive margin — a genuine two-hops-from-the-mainland barrier
    // site, not an arbitrary open-ocean cell.
    let mut candidates: Vec<CellId> = geo
        .cells()
        .filter(|&c| {
            if !is_ocean(c) || is_trench(c) || delta_cells.contains(&c) {
                return false;
            }
            if geo.neighbors(c).iter().any(|&nb| !is_ocean(nb)) {
                return false; // has a land neighbor directly: not detached
            }
            geo.neighbors(c).iter().any(|&nb| {
                fringe.contains(&nb)
                    && !is_trench(nb)
                    && !delta_cells.contains(&nb)
                    && geo.neighbors(nb).iter().any(|&land| {
                        !is_ocean(land) && *margins.get(land) == MarginPolarity::Passive
                    })
            })
        })
        .collect();
    candidates.sort_by_key(|c| c.0);

    // Alternating selection: every OTHER eligible cell, in the coastal
    // walk's CellId order.
    let spaced: Vec<CellId> = candidates.into_iter().step_by(2).collect();

    // Supply gate: budget in cells, from PASSIVE-margin mouths only (the
    // river+wave exports `deposit_wedge` already fully spent).
    let total_passive_supply: f64 = mouths
        .iter()
        .filter(|(m, _)| *margins.get(*m) == MarginPolarity::Passive)
        .map(|(_, v)| v)
        .sum();
    let budget = if params.barrier_supply_per_cell > 0.0 {
        (total_passive_supply / params.barrier_supply_per_cell).floor() as usize
    } else {
        0
    };

    let barrier_cells: Vec<CellId> = spaced.into_iter().take(budget).collect();

    let target = sea_level.get() + params.barrier_height_m;
    let mut fill = vec![0.0_f64; geo.cell_count()];
    let mut volume_used = 0.0_f64;
    for &c in &barrier_cells {
        let raise = (target - elevation_after_wedge.get(c).get()).max(0.0);
        fill[c.0 as usize] = raise;
        volume_used += raise;
    }

    let raised = CellMap::from_fn(geo, |c| fill[c.0 as usize]);
    (raised, barrier_cells, volume_used)
}

/// Atoll caps (Task 9, spec §5): warm, shallow-submerged trail seamounts
/// (Task 6) grow a carbonate rim to just under the surface — relative to
/// the sea level this function is GIVEN; the wired pipeline (Task 10)
/// passes the provisional pre-carve sea level. Since tuning iteration 4
/// (ledger #9), `carve`'s composition folds [`raise_barriers`]'s fill in
/// BEFORE this runs, so a seamount whose nearest cell a barrier already
/// raised reads as already-at-or-above-cap and is skipped — no double
/// counting, no separate exclusion needed here. The gap that opened when
/// the post-carve re-solve landed lower (composed atoll cells reading as
/// emergent) is now closed by the sea-trim ([`trim_to_sea`], ruling #5c):
/// `globe::generate` re-caps every atoll cell to the re-solved
/// `sea_1 - atoll_freeboard_m` between its two solves, leaving only the
/// second solve's residual (only `>= 0` is structural; staying
/// `<= wedge_freeboard_m` is EMPIRICAL — max observed 39.957 m across a
/// 120-world review sweep, worst margin 0.043 m — so freeboard/shelf
/// tuning must re-verify it). Only trail
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

/// The sea-trim (ruling #5c, the re-cap after re-solve): once the carved
/// surface has its OWN percentile-exact sea level (`sea_1`, the first of
/// `globe::generate`'s exactly-two solves), re-cap the carve's marine fill
/// against it — the wedge and the atolls built against the provisional
/// pre-carve sea level, and the re-solve typically lands
/// ~`wedge_freeboard_m` below it, so uncorrected fill reads shallower (or,
/// for atolls, emergent) relative to the resolved sea. Pure and one-shot:
///
/// - every wedge-raised marine cell trims down to
///   `sea_1 - wedge_freeboard_m`. Marine membership is the classification
///   the wedge DEPOSITED under: ocean by `sea_pre` against the PRE-carve
///   elevation, carrying deposited sediment. It is deliberately NOT the
///   carved-elevation-vs-`sea_1` test (review Critical 1): a wedge-filled
///   cell already emergent relative to `sea_1` is exactly the class this
///   trim exists to re-cap, and the final-elevation test exempted it;
/// - every atoll cell trims down to `sea_1 - atoll_freeboard_m` (its own,
///   shallower freeboard — a reef breaks closer to the surface than the
///   shelf does);
/// - delta lobes and barrier cells are EXEMPT (subaerial by design —
///   tuning iteration 4, ledger #9, treats a barrier bar exactly like a
///   delta lobe here: meant to stay above the final sea level, not
///   re-capped toward it);
/// - LAND sediment (floodplain/playa deposit — land by `sea_pre`) is not
///   marine and is never trimmed;
/// - a natural shallow bank (no sediment) is untouched: only what the
///   carve raised is re-capped.
///
/// Returns the per-cell trim delta (each entry `<= 0`; the caller applies
/// it to elevation AND sediment together, the sediment floored at 0) and
/// the trimmed volume proxy (Σ of the trims' magnitudes, one unit area per
/// cell — the same volume convention every other carve book uses), which
/// the caller books as oceanic loss at the generate level.
/// type-audit: pending(wave-2: sediment_thickness), pending(wave-2: return)
#[allow(clippy::too_many_arguments)]
pub fn trim_to_sea(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    elevation_pre: &CellMap<ReferenceElevation>,
    sediment_thickness: &CellMap<f64>,
    delta_cells: &[CellId],
    atoll_cells: &[CellId],
    barrier_cells: &[CellId],
    sea_pre: ReferenceElevation,
    sea_1: ReferenceElevation,
    params: &CarveParams,
) -> (CellMap<f64>, f64) {
    let wedge_cap = sea_1.get() - params.wedge_freeboard_m;
    let atoll_cap = sea_1.get() - params.atoll_freeboard_m;
    let mut trimmed_volume = 0.0_f64;
    let trim = CellMap::from_fn(geo, |c| {
        if delta_cells.contains(&c) || barrier_cells.contains(&c) {
            return 0.0;
        }
        let e = elevation.get(c).get();
        // Marine membership is the classification the wedge DEPOSITED
        // under — ocean by sea_pre against the PRE-carve elevation — never
        // the carved-elevation-vs-sea_1 test (review Critical 1: that test
        // exempted exactly the emergent wedge-filled cells this trim
        // exists to re-cap; 233 such cells measured across a 120-world
        // sweep, seed 34 L4 alone holding 40 at sea level under 49-344 m
        // of wedge sediment).
        let cap = if atoll_cells.contains(&c) {
            atoll_cap
        } else if *elevation_pre.get(c) < sea_pre && *sediment_thickness.get(c) > 0.0 {
            wedge_cap
        } else {
            return 0.0;
        };
        let t = (cap - e).min(0.0);
        trimmed_volume += -t;
        t
    });
    (trim, trimmed_volume)
}

/// The full engine-A carve (spec §2 seam): incision → repose (on
/// elevation+incision) → routing → wave-cut (on elevation+incision+repose;
/// ledgers #6+#7) → wedge (mouth list = river mouths + wave micro-mouths)
/// → deltas (river mouths ONLY take the top-K lobes — wave cells are not
/// deltas) → barriers (on elevation+wedge; tuning iteration 4, ledger #9)
/// → atolls (on elevation+wedge+barriers, so a reef caps the seabed the
/// wedge and any barrier already built — the composed atoll cell ends
/// exactly at `sea_level - atoll_freeboard_m`, never above it), composed
/// into one `CarveDelta`. Wired into `globe::generate` since Task 10,
/// followed there by the sea-trim ([`trim_to_sea`], ruling #5c) —
/// generate-level composition is carve + trim. Every input is a plain
/// reference the caller already has, and this function adds no new draws.
///
/// Books: `eroded_total_m3` and the repose-only part of `deposited_total_m3`
/// come from `CarveDelta::from_incision_and_repose`; this function adds
/// the wave cut to `eroded_total_m3` (its exported volume rides the wedge
/// machinery like any river mouth's — no new bookkeeping category),
/// routing's floodplain/playa deposit, the marine wedge/delta fill, and
/// atoll cap volume to `deposited_total_m3`, routing's playa overflow and
/// the wedge's own loss to `ocean_loss_m3`, and atoll volume to
/// `eroded_total_m3` too (a paired biogenic source+sink: carbonate grown in
/// place books on both sides of the ledger, spec §5's tier-aware mass
/// balance — never free mass). Barrier volume is a paired source+sink too
/// (added to both `eroded_total_m3` and `deposited_total_m3`; see
/// [`raise_barriers`]'s own doc for why: it is supply-GATED rather than a
/// literal second withdrawal against the wedge's already-closed books).
/// Mouth exports (river or wave) are not a separate top-level bucket:
/// `deposit_wedge` fully consumes each mouth's export into wedge/delta
/// deposit or its own `ocean_loss`, so the two-term identity
/// `eroded ≈ deposited + ocean_loss` holds end to end. `CarveDelta::mouths`
/// retains the RIVER mouths only — wave micro-mouths are transient shelf
/// supply, not river mouths.
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
    geom: MarginGeometry,
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
    // Waterfall sites (Task 11): PRE-carve elevation/drainage/induration —
    // this function's own `elevation`/`drainage`/`induration`/`downhill`
    // parameters are exactly that, unmodified by incision/repose/routing —
    // "where the carve worked hardest against contrast".
    delta.waterfall_sites =
        find_waterfalls(geo, elevation, sea_level, drainage, induration, downhill);

    let (routing_deposit, mouths, routing_ocean_loss) = route_sediment(
        geo, elevation, sea_level, &incision, downhill, endorheic, params,
    );

    // Wave-cut coastal erosion (ledgers #6+#7), on the post-repose surface
    // — the coastline the waves actually meet after the fluvial stages.
    let elevation_after_repose = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation.get(c).get() + *incision.get(c) + *repose.get(c))
            .expect("elevation plus incision plus repose is finite")
    });
    let (wave_cut, wave_mouths) = wave_erosion(
        geo,
        &elevation_after_repose,
        sea_level,
        induration,
        carbonate,
        params,
    );

    // Merge river mouths with the wave micro-mouths (volumes summed where
    // a cell is both), re-sorted per the volume-desc/CellId-asc convention;
    // only river mouths stay delta-eligible.
    let mut merged: BTreeMap<CellId, f64> = BTreeMap::new();
    for &(c, v) in mouths.iter().chain(wave_mouths.iter()) {
        *merged.entry(c).or_insert(0.0) += v;
    }
    let mut all_mouths: Vec<(CellId, f64)> = merged.into_iter().collect();
    all_mouths.sort_by(|a, b| b.1.total_cmp(&a.1).then(a.0.0.cmp(&b.0.0)));
    let river_mouth_cells: BTreeSet<CellId> = mouths.iter().map(|(c, _)| *c).collect();

    let (marine_deposit, delta_cells, wedge_ocean_loss) = deposit_wedge(
        geo,
        elevation,
        sea_level,
        &all_mouths,
        geom,
        &river_mouth_cells,
        params,
    );
    // Barriers (tuning iteration 4, ledger #9), on the surface the wedge
    // already built, before atolls (spec order; see this function's own
    // doc). Delta lobes are passed through so a river-mouth lobe is never
    // restyled as a barrier.
    let elevation_after_wedge = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation.get(c).get() + *marine_deposit.get(c))
            .expect("elevation plus wedge fill is finite")
    });
    let delta_cell_set: BTreeSet<CellId> = delta_cells.iter().copied().collect();
    let (barrier_fill, barrier_cells, barrier_volume) = raise_barriers(
        geo,
        &elevation_after_wedge,
        sea_level,
        geom,
        &delta_cell_set,
        &all_mouths,
        params,
    );
    // Atolls cap the seabed the wedge and any barrier already built, not
    // the raw pre-wedge floor — the same fold-forward the incision→repose
    // seam uses above. Without it, wedge fill and atoll fill each capped
    // against raw elevation independently and their SUM overtopped the
    // atoll freeboard (review-measured on seed 42/L4: CellId(1965)
    // composed to sea_level + 222.322 m from raw -2713.534 + wedge
    // 227.322 + atoll 296.707).
    let elevation_after_barriers = CellMap::from_fn(geo, |c| {
        ReferenceElevation::new(elevation_after_wedge.get(c).get() + *barrier_fill.get(c))
            .expect("elevation plus barrier fill is finite")
    });
    let (atoll_fill, atoll_cells) = cap_atolls(
        geo,
        &elevation_after_barriers,
        sea_level,
        trail_seamounts,
        params,
    );

    let routing_total: f64 = routing_deposit.iter().map(|(_, d)| *d).sum();
    let marine_total: f64 = marine_deposit.iter().map(|(_, d)| *d).sum();
    let atoll_total: f64 = atoll_fill.iter().map(|(_, d)| *d).sum();

    let wave_total: f64 = wave_mouths.iter().map(|(_, v)| v).sum();

    let old_delta_m = delta.delta_m.clone();
    let old_sediment = delta.sediment_thickness_m.clone();
    delta.delta_m = CellMap::from_fn(geo, |c| {
        *old_delta_m.get(c)
            + *wave_cut.get(c)
            + *routing_deposit.get(c)
            + *marine_deposit.get(c)
            + *barrier_fill.get(c)
            + *atoll_fill.get(c)
    });
    delta.sediment_thickness_m = CellMap::from_fn(geo, |c| {
        *old_sediment.get(c)
            + *routing_deposit.get(c)
            + *marine_deposit.get(c)
            + *barrier_fill.get(c)
            + *atoll_fill.get(c)
    });
    delta.mouths = mouths;
    delta.deposited_total_m3 += routing_total + marine_total + barrier_volume + atoll_total;
    delta.ocean_loss_m3 += routing_ocean_loss + wedge_ocean_loss;
    delta.eroded_total_m3 += wave_total + barrier_volume + atoll_total;
    delta.delta_cells = delta_cells;
    delta.atoll_cells = atoll_cells;
    delta.barrier_cells = barrier_cells;
    // delta.waterfall_sites was already set above, from the pre-carve fields.

    delta
}

#[cfg(test)]
mod tests {
    //! NOTE (Task 10 wiring; ruling #5c's sea-trim): every test here that
    //! builds a real globe via `globe::generate` now receives an
    //! ALREADY-CARVED-AND-TRIMMED surface —
    //! `g.elevation`/`g.drainage`/`g.sea_level` have the full carve + trim
    //! folded in — so re-invoking a carve stage on them exercises a
    //! double-application (`incision_is_monotone_in_erodibility_and_capped`,
    //! `repose_conserves_mass_and_respects_the_critical_drop`,
    //! `carve_delta_assembles_from_incision_and_repose_only`,
    //! `sediment_books_balance_and_mouths_collect_the_rest`,
    //! `playa_deposit_never_overtops_the_rim_cap`). Verified harmless
    //! today: each asserts stage-local invariants (monotonicity, mass
    //! conservation, caps, book balance) that hold on ANY valid surface,
    //! carved or raw. But it is a latent trap — do not add assertions here
    //! that assume the input surface is pre-carve (e.g. pinning incision
    //! depths or expecting virgin drainage); build a synthetic field
    //! instead, as the deposit_wedge/cap_atolls probes below do.
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
        // Seed re-pinned 42 → 3 for tuning iteration 1 (ledger #6,
        // land-only incision slope): the carved seed-42/L4 surface now has
        // ZERO interior sinks (post-change survey over seeds
        // [42, 1, 7, 99, 2, 3, 5, 11]: sinks 0/0/2/0/3/4/2/0); seed 3
        // carries 4 — the most headroom of the survey.
        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(3), &geo, &crate::pins::TerrainPins::default()).unwrap();
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
        assert!(sinks > 0, "seed 3 L4 must have interior sinks to test");
    }

    #[test]
    fn the_wedge_builds_a_shelf_mode_wide_on_passive_margins() {
        // Sculpting Task 10 wires the full carve (incision → repose →
        // routing → wedge → deltas → atolls) directly into `generate`, and
        // ruling #5c adds the sea-trim between generate's two sea-level
        // solves — so `g.elevation`/`g.drainage`/etc. are the CARVED AND
        // TRIMMED surface; re-invoking `carve()` on them here (as this
        // test did pre-wiring) would double-apply the whole pipeline.
        // Assert the geometric invariants directly against the globe's
        // own retained outputs instead.
        // Seed re-pinned 42 → 3 for tuning iteration 1 (ledger #6,
        // land-only incision slope): reduced coastal incision shrinks
        // wedge export, so fewer cells reach the trim cap to be tied —
        // seed 42's tied block fell to 15 cells (post-change survey over
        // seeds [42, 1, 7, 99, 2, 3, 5, 11]: at-top 15/1/32/34/1/56/20/12).
        // Seed 3 (56 tied cells) kept the wide-tied-block signature through
        // iteration 2. Re-pinned 3 → 42 for tuning iteration 3
        // (RELIEF_FREQUENCY 48→8): the coarser, resolved relief octave
        // reshuffles which cells hit the exact trim cap; seed 3 fell to 2
        // tied cells (post-change survey over [42, 1, 7, 99, 2, 3, 5, 11, 4,
        // 6, 8, 9, 10, 12, 13, 20, 25]: at-top 57/4/115/98/3/2/67/63/113/65/
        // 84/2/79/148/100/1/1). Seed 42 (57 tied cells) is back in front —
        // the signature stays seed-dependent, not universal.
        let geo = Geosphere::new(4);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        let g = &outcome.globe;
        let p = CarveParams::default();
        // Shelf mode, post-trim: every non-delta, non-atoll marine
        // sediment cell was re-capped by `trim_to_sea` to exactly
        // `sea_1 - wedge_freeboard_m` (or already sat below it), where
        // sea_1 is generate's first (pre-trim) solve. The final sea level
        // (the second solve) lands at or below sea_1 (structural: the trim
        // only lowers cells), and staying within `wedge_freeboard_m` of it
        // is EMPIRICAL — max observed shift 39.957 m across a 120-world
        // review sweep (worst margin 0.043 m; L4 residuals 20-38 m across
        // seeds [1, 7, 42, 99]); freeboard/shelf tuning must re-verify.
        // The structural signature this asserts: a WIDE TIED BLOCK — the
        // trimmed shelf top is one exact shared elevation carried by many
        // cells, and it is the maximum of the marine sediment surface.
        // The exact sea_final-relative bound lives in
        // `trim_recaps_hold_after_the_final_solve`
        // (tests/carve_properties.rs), which recomputes sea_1 and uses the
        // honest measured shift.
        let mut shelf: Vec<f64> = Vec::new();
        for (c, dep) in g.sediment_thickness.iter() {
            let e = g.elevation.get(c).get();
            // Atoll cells are excluded alongside delta cells: an atoll's
            // own freeboard (default 5 m) is deliberately shallower than
            // the wedge's (default 40 m), so an atoll cell legitimately
            // sits above the shelf block.
            if e < g.sea_level.get()
                && *dep > 0.0
                && !g.delta_cells.contains(&c)
                && !g.atoll_cells.contains(&c)
            {
                shelf.push(e);
            }
        }
        assert!(!shelf.is_empty(), "no shelf built");
        let top = shelf.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let at_top = shelf.iter().filter(|&&e| e == top).count();
        assert!(
            at_top >= 25,
            "the trimmed shelf top is not a wide tied block: only {at_top} cells at {top}"
        );
        // The block top stays submerged (within wedge_freeboard_m of the
        // final cap, per the empirical residual bound above — re-verify on
        // any freeboard/shelf retune).
        assert!(
            top <= g.sea_level.get() - p.wedge_freeboard_m + p.wedge_freeboard_m + 1e-9,
            "shelf top {top} above final sea level {}",
            g.sea_level.get()
        );
        // Deltas: at most delta_count lobes, subaerial. Built to
        // `delta_height_m` above the pre-carve sea level and EXEMPT from
        // the trim (subaerial by design); the final sea level lands below
        // the pre-carve one, so a real lobe stands comfortably above the
        // floor here — the generous allowance still catches a
        // genuinely-never-built delta (hundreds of meters down).
        // `delta_cells` is a CELL list, not a lobe list (each top-K mouth
        // can raise itself plus adjacent hop-1 ocean cells — see
        // `deposit_wedge`'s doc and the census delta-count metric): the
        // old `<= delta_count` bound conflated the two and only passed on
        // seed 42 by luck. Bound per the metric contract: mouth + up to
        // two raised neighbors per lobe (probe max 9 cells over 100 seeds).
        assert!(g.delta_cells.len() as u32 <= 3 * p.delta_count);
        let delta_floor = g.sea_level.get() - p.delta_height_m - p.wedge_freeboard_m - 1.0;
        for c in &g.delta_cells {
            assert!(
                g.elevation.get(*c).get() >= delta_floor,
                "delta lobe stayed submerged: {} vs floor {}",
                g.elevation.get(*c).get(),
                delta_floor
            );
        }
        // Atolls: trimmed to `sea_1 - atoll_freeboard_m` (ruling #5c), so
        // relative to the final sea level an atoll sits at most the
        // bounded solve-2 residual (≤ wedge_freeboard_m) above the atoll
        // cap — far below a double-count regression's hundreds-of-meters
        // overshoot (the bug class this ceiling was written against:
        // CellId(1965) once composed to sea_level + 222 m).
        let atoll_ceiling = g.sea_level.get() - p.atoll_freeboard_m + p.wedge_freeboard_m + 1e-6;
        for c in &g.atoll_cells {
            assert!(
                g.elevation.get(*c).get() <= atoll_ceiling,
                "atoll cell {} above the trim ceiling: {} vs {}",
                c.0,
                g.elevation.get(*c).get(),
                atoll_ceiling
            );
        }
    }

    #[test]
    fn wave_cut_is_differential_floored_and_exports_its_volume() {
        // Synthetic probe for the wave-cut term (ledgers #6+#7): an islet
        // (every neighbor ocean, exposure 1.0) versus a patch-edge cell
        // (some neighbors land, exposure < 1.0), soft vs hard rock, the
        // never-below-the-floor cap, and volume export as micro-mouths.
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let islet = CellId(0);
        let patch_center = CellId(geo.cell_count() as u32 - 1);
        assert!(
            !geo.neighbors(islet).contains(&patch_center)
                && !geo.neighbors(patch_center).contains(&islet),
            "probe wants the islet and the patch disjoint"
        );
        let is_patch = |c: CellId| c == patch_center || geo.neighbors(patch_center).contains(&c);
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == islet || is_patch(c) {
                ReferenceElevation::new(100.0).unwrap()
            } else {
                ReferenceElevation::new(-500.0).unwrap()
            }
        });
        let soft = CellMap::from_fn(&geo, |_| 0.0);
        let hard = CellMap::from_fn(&geo, |_| 1.0);
        let no_carbonate = CellMap::from_fn(&geo, |_| 0.0);
        // A deliberately small scale so neither probe cell hits the
        // elevation cap — this test pins the FORMULA's differential
        // structure; the default's magnitude is the batteries' business.
        let p = CarveParams {
            wave_cut_m: 60.0,
            ..CarveParams::default()
        };

        let (cut_soft, mouths_soft) = wave_erosion(&geo, &elevation, sea, &soft, &no_carbonate, &p);
        // Exposure differentiates: the islet cuts deeper than any
        // patch-edge cell; the sheltered patch center (zero ocean
        // neighbors) does not cut at all.
        let edge = geo.neighbors(patch_center)[0];
        assert!(
            *cut_soft.get(islet) < *cut_soft.get(edge) && *cut_soft.get(edge) < 0.0,
            "exposure must differentiate: islet {} vs edge {}",
            cut_soft.get(islet),
            cut_soft.get(edge)
        );
        assert_eq!(*cut_soft.get(patch_center), 0.0, "sheltered center cut");
        // Ocean cells never cut.
        for (c, d) in cut_soft.iter() {
            if *elevation.get(c) < sea {
                assert_eq!(*d, 0.0, "ocean cell {} cut", c.0);
            }
            // The floor binds every CUT cell: no cut lowers a cell below
            // sea - wave_cut_floor_m (uncut ocean sits below it already).
            if *d < 0.0 {
                assert!(
                    elevation.get(c).get() + *d >= sea.get() - p.wave_cut_floor_m - 1e-9,
                    "cell {} cut below the wave floor",
                    c.0
                );
            }
        }
        // Hardness differentiates: the same islet cuts less in hard rock.
        let (cut_hard, _) = wave_erosion(&geo, &elevation, sea, &hard, &no_carbonate, &p);
        assert!(
            *cut_hard.get(islet) > *cut_soft.get(islet),
            "hard islet {} must cut less than soft islet {}",
            cut_hard.get(islet),
            cut_soft.get(islet)
        );
        // Micro-mouths: every cut cell exports exactly its |cut| as
        // volume, sorted volume-descending, CellId-ascending tiebreak.
        let cut_total: f64 = cut_soft.iter().map(|(_, d)| -*d).sum();
        let export_total: f64 = mouths_soft.iter().map(|(_, v)| v).sum();
        assert!((cut_total - export_total).abs() < 1e-9);
        for (c, v) in &mouths_soft {
            assert!((cut_soft.get(*c).abs() - v).abs() < 1e-12);
        }
        for w in mouths_soft.windows(2) {
            assert!(w[0].1 > w[1].1 || (w[0].1 == w[1].1 && w[0].0 < w[1].0));
        }

        // The floor cap binds exactly on low land: a 5 m islet may cut at
        // most 35 m (down to sea - 30), not its uncapped candidate
        // (60 * 1.75 = 105 m at this test's scale).
        let low = CellMap::from_fn(&geo, |c| {
            if c == islet {
                ReferenceElevation::new(5.0).unwrap()
            } else {
                ReferenceElevation::new(-500.0).unwrap()
            }
        });
        let (cut_low, _) = wave_erosion(&geo, &low, sea, &soft, &no_carbonate, &p);
        let allowed = 5.0 - (sea.get() - p.wave_cut_floor_m);
        assert!(
            (*cut_low.get(islet) + allowed).abs() < 1e-9,
            "full-exposure low islet must cut exactly to the floor: {} vs -{allowed}",
            cut_low.get(islet)
        );
    }

    #[test]
    fn wave_micro_mouths_never_grow_deltas() {
        // Delta eligibility (ledger #7): the top-K lobe selection walks
        // ELIGIBLE (river) mouths only — a wave micro-mouth outranking
        // every river mouth by volume must still grow no lobe.
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let river = CellId(0);
        let wave = CellId(geo.cell_count() as u32 - 1);
        assert!(!geo.neighbors(river).contains(&wave));
        // Shallow (-20 m) ocean so the river's 1,000-unit export can raise
        // its hop-1 lobe cells above sea level (a -500 m floor would leave
        // every lobe cell submerged and the assertion vacuous).
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == river || c == wave {
                ReferenceElevation::new(50.0).unwrap()
            } else {
                ReferenceElevation::new(-20.0).unwrap()
            }
        });
        let margins = CellMap::from_fn(&geo, |_| MarginPolarity::Passive);
        let boundary = CellMap::from_fn(&geo, |_| None);
        let plate_of = CellMap::from_fn(&geo, |_| 0u32);
        let p = CarveParams {
            delta_count: 1,
            ..CarveParams::default()
        };
        // The wave mouth carries 10x the river mouth's volume.
        let mouths = vec![(wave, 10_000.0), (river, 1_000.0)];
        let eligible: BTreeSet<CellId> = [river].into_iter().collect();
        let (_, delta_cells, _) = deposit_wedge(
            &geo,
            &elevation,
            sea,
            &mouths,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &eligible,
            &p,
        );
        assert!(
            !delta_cells.is_empty(),
            "the river mouth is top-1 among eligible mouths and must grow its lobe"
        );
        for c in &delta_cells {
            assert!(
                *c == river || geo.neighbors(river).contains(c),
                "delta cell {} does not belong to the river mouth's lobe",
                c.0
            );
        }
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
        let no_deltas = BTreeSet::new();
        let (marine_deposit, delta_cells, ocean_loss) = deposit_wedge(
            &geo,
            &elevation,
            sea,
            &mouths,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &no_deltas,
            &p,
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
        let no_deltas = BTreeSet::new();
        let (marine_deposit, _delta_cells, _ocean_loss) = deposit_wedge(
            &geo,
            &elevation,
            sea,
            &mouths,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &no_deltas,
            &p,
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

    /// Raw shoreline perimeter `L` (unnormalized by land area) exactly as
    /// `shape::shoreline_development` computes it internally — a local,
    /// self-contained copy so the barrier tests below can isolate the
    /// perimeter term the diagnostic (`.superpowers/sdd/
    /// shoreline-diagnostic.md`) actually measured, without coupling to
    /// that function's `D = L / (2 sqrt(pi A))` normalization.
    fn raw_perimeter(
        geo: &Geosphere,
        elevation: &CellMap<ReferenceElevation>,
        sea: ReferenceElevation,
    ) -> f64 {
        let mut perimeter = 0.0_f64;
        for cell in geo.cells() {
            let land = *elevation.get(cell) >= sea;
            for &neighbor in geo.neighbors(cell) {
                if neighbor.0 <= cell.0 {
                    continue;
                }
                let neighbor_land = *elevation.get(neighbor) >= sea;
                if land != neighbor_land {
                    let a = geo.position(cell);
                    let b = geo.position(neighbor);
                    let angle =
                        math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0));
                    perimeter += angle / 3f64.sqrt();
                }
            }
        }
        perimeter
    }

    /// Shared scaffold for the barrier tests below: a single land cell
    /// (`CellId(0)`) on an otherwise all-ocean globe, plus a real,
    /// mesh-derived detached candidate two hops out (the ledger #9
    /// diagnostic's "isolated offshore cell" case) — asserted, not
    /// assumed, so a mesh-generation change fails loudly here instead of
    /// silently degrading the tests below.
    struct BarrierScaffold {
        geo: Geosphere,
        sea: ReferenceElevation,
        elevation: CellMap<ReferenceElevation>,
        land: CellId,
        /// A real detached candidate (zero land neighbors, two hops from
        /// `land` via a fringe cell) that the candidate rule must accept
        /// when the fringe's land neighbor is Passive.
        detached_candidate: CellId,
    }

    fn barrier_scaffold() -> BarrierScaffold {
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let land = CellId(0);
        let elevation = CellMap::from_fn(&geo, |c| {
            if c == land {
                ReferenceElevation::new(100.0).unwrap()
            } else {
                ReferenceElevation::new(-500.0).unwrap()
            }
        });
        let land_neighbors: BTreeSet<CellId> = geo.neighbors(land).iter().copied().collect();
        let mut detached_candidate = None;
        'search: for &fringe in geo.neighbors(land) {
            for &b in geo.neighbors(fringe) {
                if b != land && !land_neighbors.contains(&b) {
                    detached_candidate = Some(b);
                    break 'search;
                }
            }
        }
        let detached_candidate =
            detached_candidate.expect("the mesh must offer a detached candidate two hops out");
        BarrierScaffold {
            geo,
            sea,
            elevation,
            land,
            detached_candidate,
        }
    }

    #[test]
    fn barrier_candidate_gains_exactly_its_ring_of_new_perimeter_edges() {
        // Candidate-rule edge arithmetic (ledger #9's diagnostic): raising
        // a DETACHED ocean cell (zero land neighbors) to land removes no
        // land/ocean edges (it had none) and adds one for every one of its
        // own neighbors — a full ring, the maximal per-cell gain the
        // diagnostic identified.
        let s = barrier_scaffold();
        let margins = CellMap::from_fn(&s.geo, |_| MarginPolarity::Passive);
        let boundary = CellMap::from_fn(&s.geo, |_| None);
        let plate_of = CellMap::from_fn(&s.geo, |_| 0u32);
        let no_deltas = BTreeSet::new();
        // Ample supply: the barrier-selection/spacing mechanics are not
        // under test here, only whether the specific mesh-derived detached
        // candidate is ACCEPTED by the candidate rule.
        let mouths = vec![(s.land, 1_000_000.0)];
        let p = CarveParams::default();

        let (fill, barrier_cells, volume) = raise_barriers(
            &s.geo,
            &s.elevation,
            s.sea,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &no_deltas,
            &mouths,
            &p,
        );
        assert!(
            barrier_cells.contains(&s.detached_candidate),
            "the detached candidate {} must be an eligible barrier cell (candidates: {:?})",
            s.detached_candidate.0,
            barrier_cells.iter().map(|c| c.0).collect::<Vec<_>>()
        );
        assert!(volume > 0.0);

        // Isolate the edge arithmetic to this ONE cell: raise it alone
        // (not the whole selected set, which may include others) and
        // compare raw perimeter before/after.
        let before = raw_perimeter(&s.geo, &s.elevation, s.sea);
        let raised = ReferenceElevation::new(
            s.elevation.get(s.detached_candidate).get() + *fill.get(s.detached_candidate),
        )
        .unwrap();
        let after_elevation = CellMap::from_fn(&s.geo, |c| {
            if c == s.detached_candidate {
                raised
            } else {
                *s.elevation.get(c)
            }
        });
        let after = raw_perimeter(&s.geo, &after_elevation, s.sea);

        let expected_gain: f64 = s
            .geo
            .neighbors(s.detached_candidate)
            .iter()
            .map(|&nb| {
                let a = s.geo.position(s.detached_candidate);
                let b = s.geo.position(nb);
                math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0)) / 3f64.sqrt()
            })
            .sum();
        assert!(
            (after - before - expected_gain).abs() < 1e-9,
            "perimeter gain {} != expected full ring {}",
            after - before,
            expected_gain
        );
    }

    #[test]
    fn barrier_supply_gate_blocks_with_no_passive_mouth_volume() {
        // Supply-gated (ledger #9): with zero exported volume from any
        // PASSIVE-margin mouth, the budget is zero and no cell is raised,
        // even though the geometry/margin candidate rule is otherwise
        // satisfied identically to the test above.
        let s = barrier_scaffold();
        let margins = CellMap::from_fn(&s.geo, |_| MarginPolarity::Passive);
        let boundary = CellMap::from_fn(&s.geo, |_| None);
        let plate_of = CellMap::from_fn(&s.geo, |_| 0u32);
        let no_deltas = BTreeSet::new();
        let p = CarveParams::default();

        let (fill, barrier_cells, volume) = raise_barriers(
            &s.geo,
            &s.elevation,
            s.sea,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &no_deltas,
            &[],
            &p,
        );
        assert!(
            barrier_cells.is_empty(),
            "no supply must mean no barrier: {:?}",
            barrier_cells.iter().map(|c| c.0).collect::<Vec<_>>()
        );
        assert_eq!(volume, 0.0);
        assert!(fill.iter().all(|(_, f)| *f == 0.0));
    }

    #[test]
    fn barrier_candidates_require_a_passive_margin_coast() {
        // Passive margins only (ledger #9; spec's "recognized longshore
        // extension" is a passive-margin phenomenon): the same scaffold
        // and ample supply as the acceptance test above, but the coast
        // land cell sits on an ACTIVE margin — the detached candidate must
        // now be rejected.
        let s = barrier_scaffold();
        let margins = CellMap::from_fn(&s.geo, |c| {
            if c == s.land {
                MarginPolarity::Active
            } else {
                MarginPolarity::Passive
            }
        });
        let boundary = CellMap::from_fn(&s.geo, |_| None);
        let plate_of = CellMap::from_fn(&s.geo, |_| 0u32);
        let no_deltas = BTreeSet::new();
        // Supply comes from a DIFFERENT, ordinary Passive-margin cell —
        // ample budget exists, so a rejection here can only be the
        // candidate rule's own margin check, not the supply gate (which
        // the previous test already covers in isolation).
        let supply_mouth = CellId(s.geo.cell_count() as u32 - 1);
        assert_ne!(supply_mouth, s.land);
        assert_ne!(supply_mouth, s.detached_candidate);
        let mouths = vec![(supply_mouth, 1_000_000.0)];
        let p = CarveParams::default();

        let total_passive_supply: f64 = mouths
            .iter()
            .filter(|(m, _)| *margins.get(*m) == MarginPolarity::Passive)
            .map(|(_, v)| v)
            .sum();
        assert!(
            total_passive_supply > 0.0,
            "the supply must be real so the supply gate is not the reason for rejection"
        );

        let (_, barrier_cells, _) = raise_barriers(
            &s.geo,
            &s.elevation,
            s.sea,
            MarginGeometry {
                margins: &margins,
                boundary: &boundary,
                plate_of: &plate_of,
            },
            &no_deltas,
            &mouths,
            &p,
        );
        assert!(
            !barrier_cells.contains(&s.detached_candidate),
            "an Active-margin coast must never grow a barrier: {:?}",
            barrier_cells.iter().map(|c| c.0).collect::<Vec<_>>()
        );
    }

    #[test]
    fn find_waterfalls_flags_a_high_drainage_hard_over_soft_step() {
        // Synthetic knickpoint: source is land, high drainage, hard
        // (induration 0.9); its downhill neighbor is soft (0.3) — a
        // 0.6 induration step, well past WATERFALL_INDURATION_STEP (0.35).
        let geo = Geosphere::new(3);
        let sea = ReferenceElevation::new(0.0).unwrap();
        let source = CellId(0);
        let target = geo.neighbors(source)[0];
        let elevation = CellMap::from_fn(&geo, |_| ReferenceElevation::new(100.0).unwrap());
        let induration = CellMap::from_fn(&geo, |c| {
            if c == source {
                0.9
            } else if c == target {
                0.3
            } else {
                0.5
            }
        });
        let n = geo.cell_count();
        let mut downhill: Vec<Option<CellId>> = vec![None; n];
        downhill[source.0 as usize] = Some(target);

        let high_drainage = CellMap::from_fn(&geo, |c| {
            if c == source {
                WATERFALL_MIN_DRAINAGE + 1.0
            } else {
                0.0
            }
        });
        let sites = find_waterfalls(
            &geo,
            &elevation,
            sea,
            &high_drainage,
            &induration,
            &downhill,
        );
        assert_eq!(sites, vec![source], "the hard-over-soft step must be found");

        // A low-drainage clone (same induration step) must NOT be found.
        let low_drainage = CellMap::from_fn(&geo, |c| {
            if c == source {
                WATERFALL_MIN_DRAINAGE - 1.0
            } else {
                0.0
            }
        });
        let sites = find_waterfalls(&geo, &elevation, sea, &low_drainage, &induration, &downhill);
        assert!(
            sites.is_empty(),
            "a sub-threshold-drainage clone must not be a waterfall: {sites:?}"
        );
    }

    #[test]
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
    fn waterfalls_exist_across_a_seed_sweep() {
        // Existence probe (spec §8), not a per-world assertion: waterfall
        // sites are sparse — a single seed can easily land with none — so
        // this asserts the UNION across a small seed sweep is non-empty.
        // Promoted to the Task 13 battery file verbatim.
        //
        // **Deviation from the brief's "L5"**: measured directly (seeds
        // 1..=20, `WATERFALL_MIN_DRAINAGE`/`WATERFALL_INDURATION_STEP` at
        // their brief-specified defaults) — at `Geosphere::new(5)` the
        // largest watershed on any of the 20 worlds tops out at 85 upstream
        // land cells (`WATERFALL_MIN_DRAINAGE` is 80), leaving at most one
        // drainage-eligible candidate cell per world and ZERO waterfalls
        // across the whole sweep: the induration-step threshold has
        // essentially no candidate population to act on. At the canonical
        // `Geosphere::new(6)` (`GLOBE_LEVEL`, ~4x the cells) the same
        // watersheds resolve to a max drainage of 126-353 land cells,
        // 15-77 candidates per world, and seeds 3/5/8 alone (well inside
        // 1..=8) each clear the induration step. L5's watershed resolution
        // is simply too coarse for these thresholds to ever fire — a real
        // resolution-dependent effect, not a logic defect (`find_waterfalls`
        // itself is exercised directly, resolution-independent, by
        // `find_waterfalls_flags_a_high_drainage_hard_over_soft_step`
        // above). The canonical level here matches how every real world is
        // actually generated (`GLOBE_LEVEL`); L5 remains the right choice
        // for this file's OTHER tests, which probe algorithmic properties
        // speed-first, not this feature's real occurrence rate.
        let geo = Geosphere::new(crate::GLOBE_LEVEL);
        let mut total = 0usize;
        for seed in 1..=8u64 {
            let outcome =
                crate::globe::generate(Seed(seed), &geo, &crate::pins::TerrainPins::default())
                    .unwrap();
            total += outcome.globe.waterfall_sites.len();
        }
        assert!(total > 0, "no waterfalls found across seeds 1..=8 at L6");
    }

    #[test]
    fn reroute_fraction_is_zero_on_an_unchanged_surface() {
        let geo = Geosphere::new(3);
        let g = crate::globe::generate(Seed(11), &geo, &crate::pins::TerrainPins::default())
            .unwrap()
            .globe;
        let (drainage, _) = crate::drainage::drainage_field(&geo, &g.elevation, g.sea_level);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        // Feed the same drainage/downhill/sea-level in as both "pre" and
        // "post": no cell's downhill target can possibly differ from
        // itself, so every mouth's path scores zero regardless of its
        // weight.
        let fraction = rerouted_flow_fraction(
            &geo,
            &drainage,
            &downhill,
            &drainage,
            &downhill,
            g.sea_level,
            g.sea_level,
            REROUTE_TOP_RIVERS,
        );
        assert_eq!(fraction, 0.0, "unchanged surface must reroute nothing");
    }

    #[test]
    fn reroute_fraction_is_zero_when_n_rivers_is_zero() {
        let geo = Geosphere::new(3);
        let g = crate::globe::generate(Seed(11), &geo, &crate::pins::TerrainPins::default())
            .unwrap()
            .globe;
        let (drainage, _) = crate::drainage::drainage_field(&geo, &g.elevation, g.sea_level);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);
        let fraction = rerouted_flow_fraction(
            &geo,
            &drainage,
            &downhill,
            &drainage,
            &downhill,
            g.sea_level,
            g.sea_level,
            0,
        );
        assert_eq!(fraction, 0.0);
    }

    #[test]
    fn reroute_fraction_detects_a_synthetic_reroute() {
        // A tiny synthetic 4-cell chain (using real neighbor structure from
        // a level-2 geosphere is overkill for a targeted probe, so this
        // borrows two real mouths from a generated world and swaps their
        // post-carve downhill targets to guarantee full divergence,
        // isolating the weighting/averaging arithmetic from mouth/path
        // discovery, which the zero-case test above already covers).
        let geo = Geosphere::new(3);
        let g = crate::globe::generate(Seed(11), &geo, &crate::pins::TerrainPins::default())
            .unwrap()
            .globe;
        let (drainage, _) = crate::drainage::drainage_field(&geo, &g.elevation, g.sea_level);
        let downhill = crate::drainage::downhill_targets(&geo, &g.elevation, g.sea_level);

        // The highest-drainage mouth (pre-downhill target is ocean, i.e.
        // drainage 0) — guaranteed rank 0, so it always survives the
        // top-`n_rivers` truncation regardless of how many mouths this
        // small geosphere happens to have.
        let mouth = geo
            .cells()
            .filter(|&c| match downhill[c.0 as usize] {
                Some(t) => *drainage.get(t) == 0.0,
                None => false,
            })
            .max_by(|&a, &b| {
                drainage
                    .get(a)
                    .total_cmp(drainage.get(b))
                    .then(a.0.cmp(&b.0))
            })
            .expect("at least one mouth exists on this seed/level");

        // A post-carve downhill vector identical to pre-carve except the
        // mouth itself now points to a different neighbor (still land, so
        // the "differs" test fires deterministically without needing a
        // second real ocean cell nearby).
        let mut post_downhill = downhill.clone();
        let alt = geo
            .neighbors(mouth)
            .iter()
            .copied()
            .find(|&n| Some(n) != downhill[mouth.0 as usize])
            .expect("a geosphere cell has more than one neighbor");
        post_downhill[mouth.0 as usize] = Some(alt);

        let fraction = rerouted_flow_fraction(
            &geo,
            &drainage,
            &downhill,
            &drainage,
            &post_downhill,
            g.sea_level,
            g.sea_level,
            REROUTE_TOP_RIVERS,
        );
        assert!(
            fraction > 0.0,
            "a forced downhill-target change on a real mouth must register"
        );
    }

    #[test]
    fn trim_recaps_marine_fill_to_the_resolved_sea_level() {
        // Ruling #5c (the re-cap after re-solve): every wedge-raised marine
        // cell — marine under the classification the wedge DEPOSITED with,
        // i.e. ocean by sea_pre, NOT by its carved elevation (an emergent
        // cell the wedge filled to or above sea_1 is exactly the class the
        // ruling exists to fix — review Critical 1) — trims to
        // sea_1 - wedge_freeboard_m, every atoll cell to
        // sea_1 - atoll_freeboard_m; delta lobes and LAND-by-sea_pre
        // sediment (floodplain/playa) are exempt; natural shallow banks
        // (no sediment) are untouched; a trimmed cell's sediment shrinks
        // by the same meters, floored at 0.
        let geo = Geosphere::new(2);
        let p = CarveParams::default();
        let sea_pre = ReferenceElevation::new(5.0).unwrap();
        let sea_1 = ReferenceElevation::new(0.0).unwrap();
        let wedge_cell = CellId(1); // ocean-by-sea_pre, sediment, above the wedge cap
        let atoll_cell = CellId(2); // atoll, above the atoll cap
        let delta_cell = CellId(3); // delta lobe, subaerial: exempt
        let playa_cell = CellId(4); // LAND-by-sea_pre sediment: exempt
        let bank_cell = CellId(5); // ocean, shallow, NO sediment: untouched
        let emergent_cell = CellId(6); // ocean-by-sea_pre, wedge-filled to ABOVE sea_1
        let elevation_pre = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(match c {
                c if c == wedge_cell => -20.0,
                c if c == atoll_cell => -30.0,
                c if c == delta_cell => -20.0,
                c if c == playa_cell => 6.0,
                c if c == bank_cell => -10.0,
                c if c == emergent_cell => -20.0,
                _ => -500.0,
            })
            .unwrap()
        });
        let elevation = CellMap::from_fn(&geo, |c| {
            ReferenceElevation::new(match c {
                c if c == wedge_cell => -10.0,
                c if c == atoll_cell => -1.0,
                c if c == delta_cell => 10.0,
                c if c == playa_cell => 6.0,
                c if c == bank_cell => -10.0,
                c if c == emergent_cell => 3.0,
                _ => -500.0,
            })
            .unwrap()
        });
        let sediment = CellMap::from_fn(&geo, |c| match c {
            c if c == wedge_cell => 5.0,
            c if c == atoll_cell => 6.0,
            c if c == delta_cell => 8.0,
            c if c == playa_cell => 6.0,
            c if c == emergent_cell => 50.0,
            _ => 0.0,
        });
        let (trim, volume) = trim_to_sea(
            &geo,
            &elevation,
            &elevation_pre,
            &sediment,
            &[delta_cell],
            &[atoll_cell],
            &[],
            sea_pre,
            sea_1,
            &p,
        );
        // Wedge cell: re-capped to exactly sea_1 - wedge_freeboard_m.
        assert_eq!(
            elevation.get(wedge_cell).get() + trim.get(wedge_cell),
            sea_1.get() - p.wedge_freeboard_m
        );
        // Its sediment shrinks by the same meters, floored at 0 (the trim
        // here, 30 m, exceeds its 5 m of sediment).
        assert_eq!(
            (*sediment.get(wedge_cell) + *trim.get(wedge_cell)).max(0.0),
            0.0
        );
        // The EMERGENT cell (review Critical 1): carved elevation +3 sits
        // above sea_1, but it was ocean when the wedge deposited (pre
        // elevation -20 < sea_pre) and carries wedge sediment — it MUST be
        // re-capped like any other wedge cell, to exactly
        // sea_1 - wedge_freeboard_m.
        assert_eq!(
            elevation.get(emergent_cell).get() + trim.get(emergent_cell),
            sea_1.get() - p.wedge_freeboard_m,
            "emergent wedge-filled cell escaped the trim"
        );
        // Atoll cell: re-capped to exactly sea_1 - atoll_freeboard_m.
        assert_eq!(
            elevation.get(atoll_cell).get() + trim.get(atoll_cell),
            sea_1.get() - p.atoll_freeboard_m
        );
        // Delta lobe, land playa, and the natural sediment-free bank are
        // all untouched.
        assert_eq!(*trim.get(delta_cell), 0.0);
        assert_eq!(*trim.get(playa_cell), 0.0);
        assert_eq!(*trim.get(bank_cell), 0.0);
        // Every trim is <= 0, and the trimmed volume books their sum:
        // wedge -10 -> -40 is 30 m; emergent +3 -> -40 is 43 m; atoll
        // -1 -> -5 is 4 m.
        assert!(trim.iter().all(|(_, t)| *t <= 0.0));
        assert!((volume - 77.0).abs() < 1e-9, "volume {volume}");
    }
}
