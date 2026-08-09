//! A one-off calibration: the distribution of `approach_ease` over habitable
//! cells, pooled across seeds. Run once, by hand, to set `DEF_SCALE` so the
//! median cell's defensibility is ~1.0 — i.e. the median world is unchanged
//! and only the extremes of the terrain move. `#[ignore]`d because it is a
//! measurement, not a gate; it asserts nothing about the result.
//!
//! **Era choice: present.** `history_bake`'s bake holds one
//! [`ConnectionGraph`] per paleoclimate era (`graphs: &[ConnectionGraph]`,
//! `windows/worldgen/src/history_bake.rs`), but the private `bake_history_from`
//! that assembles that per-era array is not reachable from an integration
//! test. This harness instead reads the graph through the crate's public
//! seam, [`connection_graph_of`] — the same present-day graph
//! `sundered_landmasses` and the almanac's connectivity view already read
//! elsewhere. That is, in effect, the bake's *last* era: `bake_eras`' final
//! entry sits at `era_day == 0` (present) with its `sea_level_change`
//! sampled at ice-history `t == 0`, so it reduces to the same present sea
//! level `connection_graph_of` reads off the built terrain. Picking ONE era
//! (present) rather than pooling across eras keeps the measured distribution
//! from blurring a glacial low-stand's wide land bridges together with a
//! high-stand's narrow ones — the brief is explicit that averaging across
//! eras would do exactly that.
//!
//! **Task 2d addendum.** Spec amendment 1 (2026-07-30, pre-readout) moved
//! `defensibility` from a per-cell aggregate to a per-APPROACH read
//! (`defensibility(graph, from, to)`, `windows/worldgen/src/history_bake.rs`):
//! Task 2c found the aggregate was two disjoint regimes with an empty gap
//! between them, which no single transform can grade. This harness now also
//! samples the quantity the amended mechanism actually reads: one
//! `cost_exponent = -ln(best_conductance)` value per ordered `(from, to)`
//! pair with a traversable edge, both ends restricted to the same
//! habitable-cell population every prior run used (raids and resettlement
//! both originate from and land on settled ground; a stray candidate cell in
//! `best_home`'s ring walk that never becomes a home is not what
//! `DEF_SCALE` needs to be calibrated against). Parallel edges between the
//! same pair are deduplicated to their MAXIMUM conductance first — mirroring
//! `defensibility`'s own `best` computation exactly, which is also the fix
//! for the 6.7% duplicate-`to` defect Task 2b found (measurement only here;
//! that fix is Task 3's, done under review, not folded into a calibration
//! commit).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed};
use hornvale_terrain::TerrainPins;
use hornvale_topology::{ConnectionGraph, EdgeKind};
use hornvale_worldgen::{
    BuildDepth, GraphConfig, SETTLERS_PER_CAPACITY, SettlementPins, SkyChoice, WorldComponents,
    build_world_to_with_artifacts, carrying_inputs_of, connection_graph_of,
};

/// The summed conductance of `cell`'s traversable (`conductance > 0.0`)
/// edges. Mirrors `history_bake::approach_ease` deliberately: that fn is
/// private to its module, and a measurement harness is not a reason to make
/// it `pub` or add a second `#[doc(hidden)] pub` wrapper — this is the
/// two-line fold over the public `ConnectionGraph::edges` the brief asks
/// for instead.
fn approach_ease(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.conductance)
        .sum()
}

/// Task 2b companion measurement: the single largest conductance among
/// `cell`'s traversable (`conductance > 0.0`) edges, or `0.0` if it has
/// none. A sum conflates "how many ways in" with "how good the best way in
/// is" (an attacker uses one approach — Thermopylae is defensible because
/// its best approach is bad, not because a total is low); this isolates the
/// latter.
fn max_approach(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| e.conductance)
        .fold(0.0_f64, f64::max)
}

/// Task 2b companion measurement: how many traversable (`conductance >
/// 0.0`) edges lead into `cell`. Isolates "how many ways in" from `sum`'s
/// conflation of that with "how good the best way in is".
fn approach_count(graph: &ConnectionGraph, cell: CellId) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .count() as f64
}

/// Task 2c companion measurement: the single largest conductance among
/// `cell`'s traversable edges of exactly `kind`, or `0.0` if it has none.
/// Same shape as [`max_approach`], restricted to one `EdgeKind` — the water/
/// land split the successor hypothesis needs.
fn max_approach_of_kind(graph: &ConnectionGraph, cell: CellId, kind: EdgeKind) -> f64 {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0 && e.kind == kind)
        .map(|e| e.conductance)
        .fold(0.0_f64, f64::max)
}

/// Task 2c: whether `cell` has at least one traversable edge of `kind` —
/// the denominator context for [`max_approach_of_kind`]'s quantiles (a `0.0`
/// in that series can mean either "has this kind, but it's ocean-touching/
/// impassable" or "has no edge of this kind at all"; this disambiguates).
fn has_kind(graph: &ConnectionGraph, cell: CellId, kind: EdgeKind) -> bool {
    graph
        .edges(cell)
        .iter()
        .any(|e| e.conductance > 0.0 && e.kind == kind)
}

/// Task 2c's cross-tab: the overall-max-supplying edge's `EdgeKind`, and its
/// conductance — `None` for an isolated cell with no traversable edges at
/// all (excluded from the cross-tab, not silently bucketed as "low").
/// `Iterator::max_by` returns the LAST of equal maxima, and `graph.edges`
/// iterates in deterministic insertion order, so ties resolve
/// deterministically without a second sort key.
fn max_approach_with_kind(graph: &ConnectionGraph, cell: CellId) -> Option<(f64, EdgeKind)> {
    graph
        .edges(cell)
        .iter()
        .filter(|e| e.conductance > 0.0)
        .map(|e| (e.conductance, e.kind))
        .max_by(|a, b| a.0.total_cmp(&b.0))
}

/// Task 2c's per-`EdgeKind` tally for one population bucket (the high or low
/// group of the cross-tab). A fixed 3-field struct, not a map — the
/// constitutional ban on `HashMap`/`HashSet` applies to test code too, and
/// there are exactly three `EdgeKind` variants.
#[derive(Default)]
struct KindTally {
    adjacency: u64,
    water_route: u64,
    land_route: u64,
}

impl KindTally {
    fn bump(&mut self, kind: EdgeKind) {
        match kind {
            EdgeKind::Adjacency => self.adjacency += 1,
            EdgeKind::WaterRoute => self.water_route += 1,
            EdgeKind::LandRoute => self.land_route += 1,
        }
    }

    fn total(&self) -> u64 {
        self.adjacency + self.water_route + self.land_route
    }

    /// Print each kind's share of this bucket's total, `0.0` for an empty
    /// bucket rather than a `NaN` division.
    fn print_fractions(&self, label: &str) {
        let total = self.total();
        let frac = |n: u64| {
            if total == 0 {
                0.0
            } else {
                n as f64 / total as f64
            }
        };
        println!(
            "{label}n = {total}, adjacency = {:.4}, water_route = {:.4}, land_route = {:.4}",
            frac(self.adjacency),
            frac(self.water_route),
            frac(self.land_route)
        );
    }
}

/// Sort `values` and print its five brief-standard quantiles under `label`
/// (empty for the original, unlabeled `sum` series so its output stays
/// byte-identical to Task 2a's). `with_stats` additionally prints min/mean/max
/// (Task 2b asks for these on the two new series, not on `sum`, which Task 2a
/// already froze in the plan without them).
fn print_quantiles(label: &str, mut values: Vec<f64>, with_stats: bool) {
    values.sort_by(f64::total_cmp);
    for q in [0.05, 0.25, 0.50, 0.75, 0.95] {
        let i = ((values.len() as f64 - 1.0) * q).round() as usize;
        println!("{label}q{:.2} = {:.6}", q, values[i]);
    }
    if with_stats {
        let n = values.len();
        let sum: f64 = values.iter().sum();
        println!("{label}min = {:.6}", values[0]);
        println!("{label}mean = {:.6}", sum / n as f64);
        println!("{label}max = {:.6}", values[n - 1]);
    }
}

/// Task 2d: like [`print_quantiles`] with `with_stats = true`, but also
/// returns the five quantiles `[q0.05, q0.25, q0.50, q0.75, q0.95]` so the
/// `DEF_SCALE` arithmetic and the fallback-trigger check below can read the
/// median and the land population's q0.05/q0.95 straight out of what was
/// just printed, rather than recomputing them separately (and risking the
/// two disagreeing).
fn print_quantiles_capture(label: &str, mut values: Vec<f64>) -> [f64; 5] {
    values.sort_by(f64::total_cmp);
    let qs = [0.05, 0.25, 0.50, 0.75, 0.95];
    let mut out = [0.0; 5];
    for (idx, q) in qs.iter().enumerate() {
        let i = ((values.len() as f64 - 1.0) * q).round() as usize;
        println!("{label}q{:.2} = {:.6}", q, values[i]);
        out[idx] = values[i];
    }
    let n = values.len();
    let sum: f64 = values.iter().sum();
    println!("{label}min = {:.6}", values[0]);
    println!("{label}mean = {:.6}", sum / n as f64);
    println!("{label}max = {:.6}", values[n - 1]);
    out
}

/// Task 2d: the maximum conductance among the (possibly several, parallel)
/// traversable edges from `from` to `to`, and which `EdgeKind` supplied it —
/// `None` if `from` has no traversable edge to `to`. Mirrors the amended
/// `defensibility`'s own `best` computation exactly (spec amendment 1,
/// `windows/worldgen/src/history_bake.rs`): an attacker takes the easiest
/// parallel road, so duplicates resolve by MAX, not sum — the fix for the
/// Task 2b double-count defect, read here only to measure, not to fix.
fn best_conductance_with_kind(
    graph: &ConnectionGraph,
    from: CellId,
    to: CellId,
) -> Option<(f64, EdgeKind)> {
    graph
        .edges(from)
        .iter()
        .filter(|e| e.to == to && e.conductance > 0.0)
        .map(|e| (e.conductance, e.kind))
        .max_by(|a, b| a.0.total_cmp(&b.0))
}

/// Task 2c's population thresholds for the water/land cross-tab, chosen off
/// Task 2b's already-measured `max_conductance` quantiles
/// (q0.75 = 0.005602, well under 0.01; q0.95 = 0.998020, well over 0.5) — the
/// coordinator's suggested 0.5 / 0.01 sit cleanly on either side of the
/// observed gap, so they are used as given rather than re-picked.
const HIGH_POPULATION: f64 = 0.5;
/// See [`HIGH_POPULATION`].
const LOW_POPULATION: f64 = 0.01;

/// claim: sanctioned-sweep(calibration: run by hand, prints approach_ease
/// quantiles over seeds 1..=30 — not census-eligible, own #[ignore] reason)
#[test]
#[ignore = "calibration: run by hand, prints the approach_ease quantiles"]
fn print_approach_ease_quantiles() {
    let mut all: Vec<f64> = Vec::new();
    let mut all_max: Vec<f64> = Vec::new();
    let mut all_count: Vec<f64> = Vec::new();
    let mut adjacency_max: Vec<f64> = Vec::new();
    let mut water_route_max: Vec<f64> = Vec::new();
    let mut land_route_max: Vec<f64> = Vec::new();
    let mut adjacency_present: u64 = 0;
    let mut water_route_present: u64 = 0;
    let mut land_route_present: u64 = 0;
    let mut total_habitable: u64 = 0;
    let mut isolated_no_edges: u64 = 0;
    let mut high_tally = KindTally::default();
    let mut low_tally = KindTally::default();
    // Task 2d: one cost_exponent per ordered (from, to) pair, both ends
    // habitable, deduplicated to the best parallel route.
    let mut cost_exponent_all: Vec<f64> = Vec::new();
    let mut cost_exponent_adjacency: Vec<f64> = Vec::new();
    let mut cost_exponent_water_route: Vec<f64> = Vec::new();
    let mut cost_exponent_land_route: Vec<f64> = Vec::new();
    for seed in 1u64..=30 {
        // Build to Settlements depth: the terrain/climate the present-day
        // connection graph and the capacity field both read off exist there,
        // and the full stack (culture/religion/deep-history) is not needed.
        let wc = WorldComponents::assemble().expect("registries well-formed");
        let artifacts = build_world_to_with_artifacts(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .expect("seed builds");
        let terrain = artifacts
            .terrain
            .expect("terrain is Some at BuildDepth::Settlements");
        let climate = artifacts
            .climate
            .expect("climate is Some at BuildDepth::Settlements");
        let geo = terrain.geosphere();

        // "Habitable" == non-zero capacity, the same field the deep-history
        // bake feeds communities from (`bake_history_from`'s `capacity`).
        let productivity = hornvale_demography::carrying_capacity(
            geo,
            &carrying_inputs_of(geo, &terrain, &climate),
        );
        // `scaled` keeps this a capacity by construction (decision 0103).
        let capacity = productivity.scaled(SETTLERS_PER_CAPACITY).into_cell_map();

        let graph = connection_graph_of(&artifacts.world, &GraphConfig::default());

        for (cell, cap) in capacity.iter() {
            if *cap > 0.0 {
                total_habitable += 1;
                all.push(approach_ease(&graph, cell));
                all_max.push(max_approach(&graph, cell));
                all_count.push(approach_count(&graph, cell));

                adjacency_max.push(max_approach_of_kind(&graph, cell, EdgeKind::Adjacency));
                water_route_max.push(max_approach_of_kind(&graph, cell, EdgeKind::WaterRoute));
                land_route_max.push(max_approach_of_kind(&graph, cell, EdgeKind::LandRoute));
                if has_kind(&graph, cell, EdgeKind::Adjacency) {
                    adjacency_present += 1;
                }
                if has_kind(&graph, cell, EdgeKind::WaterRoute) {
                    water_route_present += 1;
                }
                if has_kind(&graph, cell, EdgeKind::LandRoute) {
                    land_route_present += 1;
                }

                match max_approach_with_kind(&graph, cell) {
                    Some((mx, kind)) if mx >= HIGH_POPULATION => high_tally.bump(kind),
                    Some((mx, kind)) if mx <= LOW_POPULATION => low_tally.bump(kind),
                    Some(_) => {}
                    None => isolated_no_edges += 1,
                }
            }
        }

        // Task 2d: one cost_exponent per ordered (from, to) pair, `from` and
        // `to` both habitable, deduplicated to the best parallel route
        // before taking `-ln`.
        for (from, cap_from) in capacity.iter() {
            if *cap_from <= 0.0 {
                continue;
            }
            let mut tos: Vec<CellId> = graph
                .edges(from)
                .iter()
                .filter(|e| e.conductance > 0.0)
                .map(|e| e.to)
                .collect();
            tos.sort();
            tos.dedup();
            for to in tos {
                if *capacity.get(to) <= 0.0 {
                    continue;
                }
                let Some((best, kind)) = best_conductance_with_kind(&graph, from, to) else {
                    continue;
                };
                let cost_exponent = -hornvale_kernel::math::ln(best);
                cost_exponent_all.push(cost_exponent);
                match kind {
                    EdgeKind::Adjacency => cost_exponent_adjacency.push(cost_exponent),
                    EdgeKind::WaterRoute => cost_exponent_water_route.push(cost_exponent),
                    EdgeKind::LandRoute => cost_exponent_land_route.push(cost_exponent),
                }
            }
        }
    }
    print_quantiles("", all, false);
    print_quantiles("max_conductance ", all_max, true);
    print_quantiles("edge_count ", all_count, true);

    println!("total_habitable = {total_habitable}");
    println!("isolated_no_edges = {isolated_no_edges} (excluded from the cross-tab below)");

    print_quantiles("adjacency_max ", adjacency_max, true);
    println!("adjacency_present = {adjacency_present} / {total_habitable}");
    print_quantiles("water_route_max ", water_route_max, true);
    println!("water_route_present = {water_route_present} / {total_habitable}");
    print_quantiles("land_route_max ", land_route_max, true);
    println!("land_route_present = {land_route_present} / {total_habitable}");

    println!("cross_tab high (max >= {HIGH_POPULATION}):");
    high_tally.print_fractions("  high ");
    println!("cross_tab low (max <= {LOW_POPULATION}):");
    low_tally.print_fractions("  low ");

    // Task 2d: the cost_exponent distribution the amended mechanism reads,
    // pooled and split by EdgeKind.
    println!("cost_exponent_all n = {}", cost_exponent_all.len());
    let all_q = print_quantiles_capture("cost_exponent_all ", cost_exponent_all);
    println!(
        "cost_exponent_adjacency n = {}",
        cost_exponent_adjacency.len()
    );
    let adjacency_q = print_quantiles_capture("cost_exponent_adjacency ", cost_exponent_adjacency);
    println!(
        "cost_exponent_water_route n = {}",
        cost_exponent_water_route.len()
    );
    let _water_q = print_quantiles_capture("cost_exponent_water_route ", cost_exponent_water_route);
    println!(
        "cost_exponent_land_route n = {}",
        cost_exponent_land_route.len()
    );
    let _land_q = print_quantiles_capture("cost_exponent_land_route ", cost_exponent_land_route);

    // Task 2d: DEF_SCALE = median(cost_exponent) / atanh((1 - DEF_MIN) / (DEF_MAX - DEF_MIN)).
    // `atanh` is not in `hornvale_kernel::math`, so it is computed here from
    // its identity over `ln` (`atanh(x) = 0.5 * ln((1+x)/(1-x))`), using the
    // SAME kernel-routed `ln` the production `defensibility` fn will use —
    // not `f64::ln` — so this arithmetic is not silently a different
    // quantity from the one the mechanism computes (decision 0041).
    const DEF_MIN: f64 = 0.75;
    const DEF_MAX: f64 = 1.40;
    let median = all_q[2];
    let x = (1.0 - DEF_MIN) / (DEF_MAX - DEF_MIN);
    let divisor = 0.5 * hornvale_kernel::math::ln((1.0 + x) / (1.0 - x));
    let def_scale = median / divisor;
    println!(
        "DEF_SCALE arithmetic: median(cost_exponent) = {median:.6}, atanh({x:.10}) = {divisor:.10}, DEF_SCALE = median / divisor = {def_scale:.6}"
    );

    // Task 2d Step 4: the fallback-trigger check (spec §4.4), evaluated with
    // the SAME kernel-routed `tanh` the production formula uses.
    let defensibility_at = |cost_exponent: f64| {
        DEF_MIN + (DEF_MAX - DEF_MIN) * hornvale_kernel::math::tanh(cost_exponent / def_scale)
    };
    let land_q05 = adjacency_q[0];
    let land_q95 = adjacency_q[4];
    let d_low = defensibility_at(land_q05);
    let d_high = defensibility_at(land_q95);
    let spread = d_high - d_low;
    println!(
        "fallback check: adjacency cost_exponent q0.05 = {land_q05:.6} -> defensibility = {d_low:.6}"
    );
    println!(
        "fallback check: adjacency cost_exponent q0.95 = {land_q95:.6} -> defensibility = {d_high:.6}"
    );
    println!(
        "fallback check: spread = {spread:.6} ({} 0.10) -> {}",
        if spread < 0.10 { "<" } else { ">=" },
        if spread < 0.10 {
            "FALLBACK TRIGGERED (normalize cost_exponent within EdgeKind before tanh)"
        } else {
            "single scale suffices, no fallback"
        }
    );
}
