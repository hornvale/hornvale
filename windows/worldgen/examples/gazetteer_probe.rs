//! Throwaway probe (The Gazetteer, Task 1): how many components of each
//! class exist at seed 42 across a range of floors, and what the traversal
//! costs relative to the terrain build. Deleted at Task 10 — this measures,
//! it does not ship. No later task calls anything in this file.
//!
//! **World constructor** (Step 1): the `volcano.rs` test module's idiom
//! (`windows/worldgen/src/volcano.rs::globe_of`) — `Geosphere::new(LEVEL)`
//! plus `hornvale_terrain::generate` under `TerrainPins::default()`, built
//! at `hornvale_terrain::GLOBE_LEVEL` (6), the production level
//! (`windows/worldgen/src/lib.rs` falls back to it via
//! `pins.globe_level.unwrap_or(GLOBE_LEVEL)`). Later tasks should reuse this
//! same constructor.
//!
//! **Timing** (Step 4): this crate's only non-`Instant` timing idiom is
//! `profiled`/`stage` (`windows/worldgen/src/lib.rs`). It does not apply
//! here: `stage` is crate-private, called only from `build_world`'s own
//! cascade, and neither `hornvale_terrain::generate` nor this probe's own
//! traversal code ever calls it — an example binary outside the crate has
//! no way to record a span with it, and wrapping this probe's code in
//! `profiled(..)` would silently capture zero stages rather than fail
//! loudly. So there is no accessible non-`Instant` idiom for what this step
//! asks to measure. Per the brief's fallback, this probe is timed
//! *externally*, with `time`, run twice — once with `GAZETTEER_SKIP_TRAVERSAL=1`
//! set (terrain build only, probe exits before touching a single cell) and
//! once unset (build + full traversal + printing) — and the traversal share
//! is reported as an **upper bound**: the difference also carries two
//! independent process start-ups, normal wall-clock variance between the two
//! runs, and (in the second run only) every `println!` call.

use hornvale_kernel::{CellId, Geosphere, Seed};
use hornvale_terrain::water::WaterKind;
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins};
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// Connected components of the cells satisfying `member`, as cell sets.
fn components(geo: &Geosphere, member: &dyn Fn(CellId) -> bool) -> Vec<BTreeSet<CellId>> {
    let mut visited = vec![false; geo.cell_count()];
    let mut out = Vec::new();
    for start in geo.cells() {
        if visited[start.0 as usize] || !member(start) {
            continue;
        }
        visited[start.0 as usize] = true;
        let mut queue = VecDeque::from([start]);
        let mut set = BTreeSet::new();
        while let Some(cell) = queue.pop_front() {
            set.insert(cell);
            for &nb in geo.neighbors(cell) {
                if !visited[nb.0 as usize] && member(nb) {
                    visited[nb.0 as usize] = true;
                    queue.push_back(nb);
                }
            }
        }
        out.push(set);
    }
    out
}

fn report(label: &str, sizes: &mut [usize], floors: &[usize]) {
    sizes.sort_unstable_by(|a, b| b.cmp(a));
    println!("{label}: {} components", sizes.len());
    println!("  ten largest: {:?}", &sizes[..sizes.len().min(10)]);
    for f in floors {
        println!(
            "  floor {f:>4}: {}",
            sizes.iter().filter(|s| *s >= f).count()
        );
    }
}

/// The Step 1 constructor: `volcano.rs`'s test-module idiom, at the
/// production globe level.
fn build_seed_42_terrain() -> (Geosphere, GeneratedTerrain) {
    let geo = Geosphere::new(GLOBE_LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    (geo, terrain)
}

fn main() {
    // Step 4's external-timing fallback: exit immediately after the terrain
    // build when asked, so the caller can time this binary twice and
    // subtract. See the module doc comment for why no in-crate idiom applies.
    let skip_traversal = std::env::var_os("GAZETTEER_SKIP_TRAVERSAL").is_some();

    let (geo, terrain) = build_seed_42_terrain();
    if skip_traversal {
        println!("GAZETTEER_SKIP_TRAVERSAL set: terrain built, exiting before traversal.");
        return;
    }

    let globe = terrain.globe();

    let land = |c: CellId| *globe.elevation.get(c) >= globe.sea_level;
    let ocean = |c: CellId| terrain.water_kind_at(c) == WaterKind::Ocean;
    let salt = |c: CellId| terrain.water_kind_at(c) == WaterKind::SaltBasin;

    for (label, pred) in [
        ("landmass", &land as &dyn Fn(CellId) -> bool),
        ("sea", &ocean),
        ("salt-lake", &salt),
    ] {
        let mut sizes: Vec<usize> = components(&geo, pred).iter().map(BTreeSet::len).collect();
        report(label, &mut sizes, &[1, 5, 10, 20, 50, 100]);
    }

    // Rivers: partition land cells by the terminal of their downhill chain.
    let downhill =
        hornvale_terrain::drainage::downhill_targets(&geo, &globe.elevation, globe.sea_level);
    let mut catchment: BTreeMap<CellId, usize> = BTreeMap::new();
    for c in geo.cells() {
        if !land(c) {
            continue;
        }
        let mut at = c;
        // Bounded by cell_count: the flow forest is acyclic by construction
        // (every hop strictly decreases elevation). Belt-and-braces.
        for _ in 0..geo.cell_count() {
            match downhill[at.0 as usize] {
                Some(next) => at = next,
                None => break,
            }
        }
        *catchment.entry(at).or_default() += 1;
    }
    let mut sizes: Vec<usize> = catchment.values().copied().collect();
    report("river (by catchment)", &mut sizes, &[4, 12, 24, 50, 100]);

    // Coordinator follow-up: this probe's 183 land components at floor 1
    // disagrees with the committed census's landmass-count=23 for
    // seed=42/pin_set=default. Head-to-head on the SAME `GeneratedTerrain`
    // this run already built, against the shipped extractor the census
    // metric actually calls.
    println!();
    println!("=== coordinator follow-up: shipped fn vs. this probe's fn ===");
    println!("geo.cell_count() = {}", geo.cell_count());

    let shipped_sizes =
        hornvale_terrain::shape::land_component_sizes(&geo, &globe.elevation, globe.sea_level);
    println!(
        "shipped land_component_sizes(): {} components",
        shipped_sizes.len()
    );
    println!(
        "  ten largest: {:?}",
        &shipped_sizes[..shipped_sizes.len().min(10)]
    );
    let shipped_total_land: usize = shipped_sizes.iter().sum();
    println!("  sum of sizes (total land cells): {shipped_total_land}");

    let probe_components = components(&geo, &land);
    let mut probe_sizes: Vec<usize> = probe_components.iter().map(BTreeSet::len).collect();
    probe_sizes.sort_unstable_by(|a, b| b.cmp(a));
    println!(
        "probe's own components(&land): {} components",
        probe_sizes.len()
    );
    println!(
        "  ten largest: {:?}",
        &probe_sizes[..probe_sizes.len().min(10)]
    );
    let probe_total_land: usize = probe_sizes.iter().sum();
    println!("  sum of sizes (total land cells): {probe_total_land}");

    let land_cell_count_direct = geo.cells().filter(|&c| land(c)).count();
    println!("geo.cells().filter(land).count() = {land_cell_count_direct}");

    println!(
        "AGREE at count level: {}",
        shipped_sizes.len() == probe_sizes.len()
    );
    println!(
        "AGREE at size-multiset level: {}",
        shipped_sizes == probe_sizes
    );

    // continent-count's own rule, run against this probe's own traversal:
    // floor = 0.5% of total land cells, count components >= floor.
    let continent_floor = 0.005 * probe_total_land as f64;
    let continent_count_from_probe = probe_sizes
        .iter()
        .filter(|&&s| s as f64 >= continent_floor)
        .count();
    println!(
        "continent-count rule (floor=0.005*land={continent_floor:.3}) over this probe's sizes: {continent_count_from_probe}"
    );
    let continent_count_from_shipped = shipped_sizes
        .iter()
        .filter(|&&s| s as f64 >= 0.005 * shipped_total_land as f64)
        .count();
    println!("continent-count rule over the shipped fn's sizes: {continent_count_from_shipped}");
}
