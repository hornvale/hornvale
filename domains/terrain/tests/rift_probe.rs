//! Stage-0 rift probe (rift-and-fit spec §6, preregistered): synthetic
//! injections of each banked rift mechanism into real v3 land masks,
//! measuring the shoreline-development delta each produces in isolation.
//! Read-only against v3 — no generator code changes with this test; the
//! whole point of Stage 0 is that this instrument's readout is committed
//! *before* any rift-and-fit generator code lands (spec §6, ledger #5).
//!
//! Run by hand (release; a debug build easily exceeds the ~10 min budget
//! at the canonical L6 mesh over 20 seeds):
//!   cargo test -p hornvale-terrain --release --test rift_probe -- --ignored --nocapture

use hornvale_kernel::{CellId, CellMap, Geosphere, ReferenceElevation, Seed};
use hornvale_terrain::shape::shoreline_development;
use hornvale_terrain::{GLOBE_LEVEL, TerrainPins, generate};
use std::collections::BTreeSet;

/// Seeds probed: world seeds 1..=20 at default pins, canonical globe level
/// (release build keeps the hand-run under ~10 min; the probe ranks
/// mechanisms, it is not a census).
const PROBE_SEEDS: u64 = 20;

/// Flip a cell to just-above-sea land or just-below-sea ocean (± 1.0 m) in a
/// mutable per-cell elevation copy. `CellMap` itself has no in-place
/// mutation (by design — see `hornvale_kernel::geosphere`), so injections
/// work over a plain `Vec<ReferenceElevation>` indexed by `CellId.0`
/// (ascending, dense, one entry per cell) and get rebuilt into a `CellMap`
/// via `CellMap::from_fn` once the injection is complete.
fn flip(copy: &mut [ReferenceElevation], cell: CellId, above: bool, sea: f64) {
    let delta = if above { 1.0 } else { -1.0 };
    copy[cell.0 as usize] = ReferenceElevation::new(sea + delta).expect("sea ± 1.0 m is finite");
}

/// A fresh mutable copy of `elevation`, indexed by `CellId.0`.
fn copy_elevation(elevation: &CellMap<ReferenceElevation>) -> Vec<ReferenceElevation> {
    elevation.iter().map(|(_, e)| *e).collect()
}

/// Rebuild a `CellMap` from an injected `Vec`.
fn to_cell_map(geo: &Geosphere, copy: &[ReferenceElevation]) -> CellMap<ReferenceElevation> {
    CellMap::from_fn(geo, |c| copy[c.0 as usize])
}

/// Ocean cells with at least one land neighbor, ascending `CellId`.
fn coastal_ocean_cells(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea: ReferenceElevation,
) -> Vec<CellId> {
    geo.cells()
        .filter(|&c| {
            *elevation.get(c) < sea && geo.neighbors(c).iter().any(|&n| *elevation.get(n) >= sea)
        })
        .collect()
}

/// Land cells with at least one ocean neighbor, ascending `CellId`.
fn coastal_land_cells(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea: ReferenceElevation,
) -> Vec<CellId> {
    geo.cells()
        .filter(|&c| {
            *elevation.get(c) >= sea && geo.neighbors(c).iter().any(|&n| *elevation.get(n) < sea)
        })
        .collect()
}

/// Rift-shoulder sliver strings (spec §5): flip a swept fraction `f` of
/// coastal ocean cells to just-above-sea land. Candidates are the coastal
/// ocean cells in ascending `CellId` order; every `k`-th (`k = ceil(1/f)`)
/// is flipped, skipping any candidate whose neighbor was already flipped
/// this pass — the strings stay alternating, never adjacent, matching real
/// rift-shoulder slivers (Seychelles, Jan Mayen) rather than a solid new
/// coastline.
fn inject_slivers(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea: ReferenceElevation,
    f: f64,
) -> CellMap<ReferenceElevation> {
    let candidates = coastal_ocean_cells(geo, elevation, sea);
    let k = (1.0 / f).ceil() as usize;
    let mut flipped: BTreeSet<CellId> = BTreeSet::new();
    for (i, &cell) in candidates.iter().enumerate() {
        if i % k != 0 {
            continue;
        }
        if geo.neighbors(cell).iter().any(|n| flipped.contains(n)) {
            continue;
        }
        flipped.insert(cell);
    }
    let mut copy = copy_elevation(elevation);
    let sea_m = sea.get();
    for cell in &flipped {
        flip(&mut copy, *cell, true, sea_m);
    }
    to_cell_map(geo, &copy)
}

/// Failed rift arms / aulacogens (spec §5): from `n_arms` evenly-spaced
/// coastal land cells (every `m`-th in the coastal-land list, ascending
/// `CellId`, `m = max(1, len / n_arms)`), walk inland via the
/// highest-(pre-injection-)elevation unvisited neighbor for `depth` cells
/// total (including the coastal starting cell), flipping every visited cell
/// to just-below-sea ocean — a 1-cell-wide bay running from the coast into
/// the craton's interior.
fn inject_arms(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea: ReferenceElevation,
    n_arms: usize,
    depth: usize,
) -> CellMap<ReferenceElevation> {
    let coastal_land = coastal_land_cells(geo, elevation, sea);
    let mut copy = copy_elevation(elevation);
    let sea_m = sea.get();
    if coastal_land.is_empty() || depth == 0 {
        return to_cell_map(geo, &copy);
    }
    let m = (coastal_land.len() / n_arms).max(1);
    let mut starts = Vec::new();
    let mut i = 0usize;
    while starts.len() < n_arms && i < coastal_land.len() {
        starts.push(coastal_land[i]);
        i += m;
    }
    for start in starts {
        let mut visited: BTreeSet<CellId> = BTreeSet::new();
        visited.insert(start);
        flip(&mut copy, start, false, sea_m);
        let mut current = start;
        for _ in 0..depth.saturating_sub(1) {
            let next = geo
                .neighbors(current)
                .iter()
                .copied()
                .filter(|n| !visited.contains(n))
                .max_by(|a, b| (*elevation.get(*a)).total_cmp(*elevation.get(*b)));
            match next {
                Some(n) => {
                    visited.insert(n);
                    flip(&mut copy, n, false, sea_m);
                    current = n;
                }
                None => break,
            }
        }
    }
    to_cell_map(geo, &copy)
}

/// Fracture-line crenulation (spec §5): re-runs Sculpting's parity
/// experiment (Census of Coasts III's supersession note) — for a swept
/// fraction `f` of coastal land cells (every `k`-th, ascending `CellId`,
/// `k = ceil(1/f)`), flip to just-below-sea ocean: single-hex alternation
/// along the existing coast, no neighbor-skip guard (unlike slivers) since
/// the mechanism itself IS the single-cell-scale alternation.
fn inject_crenulation(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    sea: ReferenceElevation,
    f: f64,
) -> CellMap<ReferenceElevation> {
    let candidates = coastal_land_cells(geo, elevation, sea);
    let k = (1.0 / f).ceil() as usize;
    let mut copy = copy_elevation(elevation);
    let sea_m = sea.get();
    for (i, &cell) in candidates.iter().enumerate() {
        if i % k == 0 {
            flip(&mut copy, cell, false, sea_m);
        }
    }
    to_cell_map(geo, &copy)
}

/// Post-carve coastal fit-degradation proxy (spec §6, ledger #10, unbanded):
/// over cells that are coastal under the FINAL (post-carve) elevation
/// reading, the fraction whose land/ocean classification differs between
/// the pre-carve reading (`elevation - carve_delta_m`) and the final
/// reading, both judged against the same final sea level — how much of the
/// carve's wave-cut/wedge/barrier texture blurs a fitted margin.
fn fit_degradation(
    geo: &Geosphere,
    elevation: &CellMap<ReferenceElevation>,
    carve_delta_m: &CellMap<f64>,
    sea: ReferenceElevation,
) -> f64 {
    let sea_m = sea.get();
    let mut coastal = 0usize;
    let mut flipped = 0usize;
    for cell in geo.cells() {
        let post_land = *elevation.get(cell) >= sea;
        let is_coastal = geo
            .neighbors(cell)
            .iter()
            .any(|&n| (*elevation.get(n) >= sea) != post_land);
        if !is_coastal {
            continue;
        }
        coastal += 1;
        let pre_elev_m = elevation.get(cell).get() - *carve_delta_m.get(cell);
        let pre_land = pre_elev_m >= sea_m;
        if pre_land != post_land {
            flipped += 1;
        }
    }
    if coastal == 0 {
        0.0
    } else {
        flipped as f64 / coastal as f64
    }
}

/// Deterministic median (ascending `total_cmp` sort, no NaN ambiguity).
fn median(values: &mut [f64]) -> f64 {
    values.sort_by(f64::total_cmp);
    let n = values.len();
    if n % 2 == 1 {
        values[n / 2]
    } else {
        (values[n / 2 - 1] + values[n / 2]) / 2.0
    }
}

/// Sweep points for the two fraction-based mechanisms — shared so the
/// slivers/crenulation rows are directly comparable at the same nominal
/// coastal-fraction values.
const FRACTION_SWEEP: [f64; 3] = [0.05, 0.15, 0.30];

/// Sweep points for the arms mechanism: every `(n_arms, depth)` pair.
const ARM_SWEEP: [(usize, usize); 6] = [(4, 3), (4, 6), (8, 3), (8, 6), (16, 3), (16, 6)];

#[test]
#[ignore = "probe: Stage-0 rift instrument, run by hand (spec §6)"]
fn rift_probe_tables() {
    let geo = Geosphere::new(GLOBE_LEVEL);

    let mut sliver_deltas: Vec<Vec<f64>> = vec![Vec::new(); FRACTION_SWEEP.len()];
    let mut arm_deltas: Vec<Vec<f64>> = vec![Vec::new(); ARM_SWEEP.len()];
    let mut crenulation_deltas: Vec<Vec<f64>> = vec![Vec::new(); FRACTION_SWEEP.len()];
    let mut fit_degradation_values: Vec<f64> = Vec::new();

    for seed in 1..=PROBE_SEEDS {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed}: default genesis failed: {e}"));
        let globe = &outcome.globe;
        let sea = globe.sea_level;
        let baseline = shoreline_development(&geo, &globe.elevation, sea)
            .unwrap_or_else(|| panic!("seed {seed}: baseline D has no land/ocean split"));
        println!("seed {seed}: baseline D = {baseline:.4}");

        for (i, &f) in FRACTION_SWEEP.iter().enumerate() {
            let mutated = inject_slivers(&geo, &globe.elevation, sea, f);
            let d = shoreline_development(&geo, &mutated, sea).unwrap_or(baseline);
            let delta = d - baseline;
            println!("  slivers f={f:.2}: D={d:.4} ΔD={delta:.4}");
            sliver_deltas[i].push(delta);
        }

        for (i, &(n_arms, depth)) in ARM_SWEEP.iter().enumerate() {
            let mutated = inject_arms(&geo, &globe.elevation, sea, n_arms, depth);
            let d = shoreline_development(&geo, &mutated, sea).unwrap_or(baseline);
            let delta = d - baseline;
            println!("  arms n={n_arms} depth={depth}: D={d:.4} ΔD={delta:.4}");
            arm_deltas[i].push(delta);
        }

        for (i, &f) in FRACTION_SWEEP.iter().enumerate() {
            let mutated = inject_crenulation(&geo, &globe.elevation, sea, f);
            let d = shoreline_development(&geo, &mutated, sea).unwrap_or(baseline);
            let delta = d - baseline;
            println!("  crenulation f={f:.2}: D={d:.4} ΔD={delta:.4}");
            crenulation_deltas[i].push(delta);
        }

        let fd = fit_degradation(&geo, &globe.elevation, &globe.carve_delta_m, sea);
        println!("  fit-degradation proxy: {fd:.4}");
        fit_degradation_values.push(fd);
    }

    println!("\n--- median ΔD over {PROBE_SEEDS} seeds ---");
    println!("| mechanism | sweep point | median ΔD |");
    println!("|---|---|---:|");
    for (i, &f) in FRACTION_SWEEP.iter().enumerate() {
        println!(
            "| slivers | f={f:.2} | {:.4} |",
            median(&mut sliver_deltas[i])
        );
    }
    for (i, &(n_arms, depth)) in ARM_SWEEP.iter().enumerate() {
        println!(
            "| arms | n={n_arms}, depth={depth} | {:.4} |",
            median(&mut arm_deltas[i])
        );
    }
    for (i, &f) in FRACTION_SWEEP.iter().enumerate() {
        println!(
            "| crenulation | f={f:.2} | {:.4} |",
            median(&mut crenulation_deltas[i])
        );
    }
    println!(
        "\nfit-degradation proxy, median over {PROBE_SEEDS} seeds: {:.4}",
        median(&mut fit_degradation_values)
    );
}
