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

/// Task 2c's population thresholds for the water/land cross-tab, chosen off
/// Task 2b's already-measured `max_conductance` quantiles
/// (q0.75 = 0.005602, well under 0.01; q0.95 = 0.998020, well over 0.5) — the
/// coordinator's suggested 0.5 / 0.01 sit cleanly on either side of the
/// observed gap, so they are used as given rather than re-picked.
const HIGH_POPULATION: f64 = 0.5;
/// See [`HIGH_POPULATION`].
const LOW_POPULATION: f64 = 0.01;

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
        let suitability = hornvale_demography::carrying_capacity(
            geo,
            &carrying_inputs_of(geo, &terrain, &climate),
        );
        let capacity =
            hornvale_kernel::CellMap::from_fn(geo, |c| *suitability.get(c) * SETTLERS_PER_CAPACITY);

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
}
