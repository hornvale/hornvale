//! The Watershed's measurement harness — re-takes the spec's §1.3 arity
//! table and its §5 landmass/river multipliers against the codebase the
//! change will actually run on.
//!
//! This exists because the campaign's original figures were simulated over a
//! corpus from a branch that never merged (`the-shibboleth`), and the spec
//! itself flags them: "the landmass and river multipliers have NOT been
//! re-taken against main." Twice already this campaign has specified from the
//! wrong codebase.
//!
//! `#[ignore]`d: it builds eight full worlds and is a measurement, not a
//! gate. Run it explicitly:
//!
//! ```text
//! cargo test -p hornvale-worldgen --test watershed_measure -- --ignored --nocapture
//! ```
//!
//! Nothing here asserts; it prints. The numbers it prints are the input to
//! the campaign's G3 package.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{CellId, Seed, Value};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to_with_artifacts,
};
use std::collections::{BTreeMap, BTreeSet};

/// The seed battery. The original 8-seed / 1842-settlement battery was never
/// committed anywhere, so it is re-declared here — and committed, so the next
/// session measures the same thing this one did.
///
/// These are NOT the original seeds: they yield 1837 settlements, not 1842,
/// while seed 42 reproduces the spec's counts exactly (so the pipeline has not
/// moved, and a different total can only mean a different seed set). Numbers
/// from this battery are therefore a new baseline, never a delta against 59.8%.
const BATTERY: [u64; 8] = [1, 2, 3, 4, 5, 6, 7, 8];

/// A river is named only if its catchment reaches this many cells (spec §3,
/// Item 3 — "the naming tier is catchment size, not mouth drainage").
const RIVER_MIN_CATCHMENT: usize = 24;

/// A landmass is named only if it reaches this many cells (spec §3, Item 2).
const LANDMASS_MIN_CELLS: usize = 20;

/// One settlement, reduced to what the measurement needs.
struct Site {
    name: String,
    gloss: String,
    landmass: Option<u32>,
    river: Option<u32>,
}

/// Connected components of non-ocean cells, each keyed by the lowest cell id
/// it contains — the spec's `LandmassId`, simulated. Returns a per-cell map
/// of component key, and the component sizes.
fn landmasses(
    terrain: &hornvale_terrain::GeneratedTerrain,
) -> (BTreeMap<u32, u32>, BTreeMap<u32, usize>) {
    let geo = terrain.geosphere();
    let n = geo.cell_count();
    let mut component: Vec<Option<u32>> = vec![None; n];
    let mut sizes: BTreeMap<u32, usize> = BTreeMap::new();
    for start in geo.cells() {
        if terrain.is_ocean(start) || component[start.0 as usize].is_some() {
            continue;
        }
        // Flood fill. The key is the lowest cell id in the component, which
        // `start` already is: `geo.cells()` walks in ascending id order, so
        // the first unvisited land cell of a component is its minimum.
        let key = start.0;
        let mut stack = vec![start];
        component[start.0 as usize] = Some(key);
        let mut size = 0usize;
        while let Some(c) = stack.pop() {
            size += 1;
            for &nb in geo.neighbors(c) {
                if terrain.is_ocean(nb) || component[nb.0 as usize].is_some() {
                    continue;
                }
                component[nb.0 as usize] = Some(key);
                stack.push(nb);
            }
        }
        sizes.insert(key, size);
    }
    let map = component
        .iter()
        .enumerate()
        .filter_map(|(i, k)| k.map(|k| (i as u32, k)))
        .collect();
    (map, sizes)
}

/// The flow forest's terminal cell per land cell — the spec's `RiverId`,
/// simulated. A river's identity is the ocean cell it empties into or the
/// interior minimum it dies in. Returns the per-cell terminal and the
/// catchment size per terminal.
fn rivers(
    terrain: &hornvale_terrain::GeneratedTerrain,
) -> (BTreeMap<u32, u32>, BTreeMap<u32, usize>) {
    let geo = terrain.geosphere();
    let n = geo.cell_count();
    let sea = terrain.sea_level();
    // Downhill pointer per land cell, mirroring `drainage::downhill_targets`:
    // the strictly-lowest neighbor, `None` at a local minimum or on ocean.
    let mut downhill: Vec<Option<CellId>> = vec![None; n];
    for c in geo.cells() {
        if terrain.elevation_at(c) < sea {
            continue;
        }
        let here = terrain.elevation_at(c);
        let mut best: Option<CellId> = None;
        let mut best_e = here;
        for &nb in geo.neighbors(c) {
            let e = terrain.elevation_at(nb);
            if e < best_e {
                best_e = e;
                best = Some(nb);
            }
        }
        downhill[c.0 as usize] = best;
    }
    // Walk each land cell to its terminal, memoizing.
    let mut terminal: Vec<Option<u32>> = vec![None; n];
    for c in geo.cells() {
        if terrain.elevation_at(c) < sea || terminal[c.0 as usize].is_some() {
            continue;
        }
        let mut path = Vec::new();
        let mut cur = c;
        let end = loop {
            if let Some(t) = terminal[cur.0 as usize] {
                break t;
            }
            path.push(cur);
            match downhill[cur.0 as usize] {
                Some(next) => cur = next,
                None => break cur.0,
            }
        };
        for p in path {
            terminal[p.0 as usize] = Some(end);
        }
    }
    let mut catchment: BTreeMap<u32, usize> = BTreeMap::new();
    for t in terminal.iter().flatten() {
        *catchment.entry(*t).or_insert(0) += 1;
    }
    let map = terminal
        .iter()
        .enumerate()
        .filter_map(|(i, t)| t.map(|t| (i as u32, t)))
        .collect();
    (map, catchment)
}

/// Validate the simulated individuation against the counts the spec reports
/// for seed 42 (§3: 30 landmass components, 14 at or above
/// [`LANDMASS_MIN_CELLS`]; 115 rivers at catchment >= 24). If these disagree,
/// every downstream number here is suspect — so this prints them rather than
/// leaving the flow-forest walk unchecked.
#[test]
#[ignore = "measurement: builds one full world; run explicitly with --ignored"]
fn watershed_individuation_matches_the_spec_counts() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let built = build_world_to_with_artifacts(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds");
    let terrain = built
        .terrain
        .as_ref()
        .expect("terrain rung sculpts terrain");
    let (_, sizes) = landmasses(terrain);
    let (_, catchments) = rivers(terrain);
    let named_lm = sizes.values().filter(|&&s| s >= LANDMASS_MIN_CELLS).count();
    let named_rv = catchments
        .values()
        .filter(|&&c| c >= RIVER_MIN_CATCHMENT)
        .count();
    let mut big: Vec<usize> = sizes.values().copied().filter(|&s| s >= 100).collect();
    big.sort_unstable_by(|a, b| b.cmp(a));
    println!("\n== individuation at seed 42, vs the spec's stated counts ==");
    println!("landmass components:  {}  (spec says 30)", sizes.len());
    println!("named (>= {LANDMASS_MIN_CELLS} cells): {named_lm}  (spec says 14)");
    println!("components >= 100 cells: {big:?}");
    println!("(spec says [1994, 1976, 1842, 1277, 907, 874, 831, 703, 356, 104])");
    println!("named rivers (catchment >= {RIVER_MIN_CATCHMENT}): {named_rv}  (spec says 115)");
    let endorheic = catchments
        .iter()
        .filter(|(t, c)| **c >= RIVER_MIN_CATCHMENT && !terrain.is_ocean(CellId(**t)))
        .count();
    println!("of which endorheic: {endorheic}  (spec says 66)");
}

/// Build one world and reduce its settlements to [`Site`]s.
fn sites_for(seed: u64) -> Vec<Site> {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let built = build_world_to_with_artifacts(
        Seed(seed),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed builds");
    let world = &built.world;
    let terrain = built.terrain.as_ref().expect("full build sculpts terrain");

    let (cell_landmass, landmass_sizes) = landmasses(terrain);
    let (cell_river, catchments) = rivers(terrain);

    world
        .ledger
        .find(hornvale_settlement::IS_PLACE)
        .map(|f| f.subject)
        .filter_map(|id| {
            let name = world.ledger.text_of(id, hornvale_kernel::NAME)?.to_string();
            let gloss = world
                .ledger
                .text_of(id, hornvale_kernel::NAME_GLOSS)?
                .to_string();
            let Value::Number(cell) = world.ledger.value_of(id, hornvale_settlement::CELL_ID)?
            else {
                return None;
            };
            let cell = *cell as u32;
            let landmass = cell_landmass
                .get(&cell)
                .copied()
                .filter(|k| landmass_sizes.get(k).copied().unwrap_or(0) >= LANDMASS_MIN_CELLS);
            let river = cell_river
                .get(&cell)
                .copied()
                .filter(|t| catchments.get(t).copied().unwrap_or(0) >= RIVER_MIN_CATCHMENT);
            Some(Site {
                name,
                gloss,
                landmass,
                river,
            })
        })
        .collect()
}

/// The **floor** on the collision rate that items 2–4 can reach, measured
/// rather than inferred.
///
/// Distinct-gloss counts bound the payoff only loosely: what actually decides
/// the criterion is how many settlements still share a full discriminator
/// tuple. So group settlements by `(gloss, landmass, river)` and count those
/// whose group has more than one member. Even if naming were perfectly
/// injective on the tuple — every distinct tuple minting a distinct name, which
/// no real namer achieves — that share would still collide. It is the best
/// case for the landscape layer alone, before Item 1 raises arity.
fn collision_floor_pct(sites: &[Site]) -> f64 {
    let mut counts: BTreeMap<(&str, Option<u32>, Option<u32>), usize> = BTreeMap::new();
    for s in sites {
        *counts
            .entry((s.gloss.as_str(), s.landmass, s.river))
            .or_insert(0) += 1;
    }
    let dup = sites
        .iter()
        .filter(|s| counts[&(s.gloss.as_str(), s.landmass, s.river)] > 1)
        .count();
    100.0 * dup as f64 / sites.len() as f64
}

/// How many distinct landmasses and rivers the settlements actually occupy —
/// the cardinality the discriminator really has, as opposed to the number of
/// landmasses and rivers the world contains. Settlements cluster, so these
/// differ, and the gap is what bounds items 2–4.
fn occupied(sites: &[Site]) -> (usize, usize) {
    let lm: BTreeSet<Option<u32>> = sites.iter().map(|s| s.landmass).collect();
    let rv: BTreeSet<Option<u32>> = sites.iter().map(|s| s.river).collect();
    (lm.len(), rv.len())
}

/// Share of sites whose NAME is shared with another site, as a percentage.
fn collision_pct(sites: &[Site]) -> f64 {
    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
    for s in sites {
        *counts.entry(s.name.as_str()).or_insert(0) += 1;
    }
    let dup = sites.iter().filter(|s| counts[s.name.as_str()] > 1).count();
    100.0 * dup as f64 / sites.len() as f64
}

/// The spec's §1.3 arity table was taken at **seed 42 alone** (329
/// settlements). This re-takes it there, like-for-like, so the difference
/// between the spec's table and the battery's can be attributed: either main
/// moved again, or a one-world arity table is small-n noise.
#[test]
#[ignore = "measurement: builds one full world; run explicitly with --ignored"]
fn watershed_seed_42_arity_like_for_like() {
    let sites = sites_for(42);
    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
    for s in &sites {
        *counts.entry(s.name.as_str()).or_insert(0) += 1;
    }
    let g: BTreeSet<&str> = sites.iter().map(|s| s.gloss.as_str()).collect();
    let gl: BTreeSet<(&str, Option<u32>)> = sites
        .iter()
        .map(|s| (s.gloss.as_str(), s.landmass))
        .collect();
    let glr: BTreeSet<(&str, Option<u32>, Option<u32>)> = sites
        .iter()
        .map(|s| (s.gloss.as_str(), s.landmass, s.river))
        .collect();
    println!("\n== seed 42, like-for-like with spec §1.3 / §5 ==");
    println!("settlements: {}", sites.len());
    println!("colliding:   {:.1}%", collision_pct(&sites));
    println!("distinct glosses   {}", g.len());
    println!(
        "+ landmass         {}  (x{:.2})",
        gl.len(),
        gl.len() as f64 / g.len() as f64
    );
    println!(
        "+ landmass + river {}  (x{:.2})",
        glr.len(),
        glr.len() as f64 / g.len() as f64
    );
    let mut by_arity: BTreeMap<usize, (usize, usize)> = BTreeMap::new();
    for s in &sites {
        let e = by_arity.entry(s.gloss.split('-').count()).or_insert((0, 0));
        e.0 += 1;
        if counts[s.name.as_str()] > 1 {
            e.1 += 1;
        }
    }
    println!("concepts  settlements  colliding");
    for (arity, (n, dup)) in &by_arity {
        println!(
            "{arity:>8}  {n:>11}  {:>8.1}%",
            100.0 * *dup as f64 / *n as f64
        );
    }
    let (lm, rv) = occupied(&sites);
    println!("\n-- the ceiling on items 2-4, seed 42 --");
    println!("distinct landmasses occupied by settlements: {lm}");
    println!("distinct rivers occupied by settlements:     {rv}");
    println!(
        "BEST-CASE collision rate from the landscape layer alone: {:.1}%",
        collision_floor_pct(&sites)
    );
    println!("(spec §5 predicts the campaign as a whole reaches below 15%)");
}

#[test]
#[ignore = "measurement: builds eight full worlds; run explicitly with --ignored"]
fn watershed_baseline_and_multipliers() {
    let per_seed: Vec<(u64, Vec<Site>)> = BATTERY.iter().map(|&s| (s, sites_for(s))).collect();
    let mut all: Vec<Site> = Vec::new();
    let mut sum_g = 0usize;
    let mut sum_gl = 0usize;
    let mut sum_glr = 0usize;
    println!("\n== per seed ==");
    println!("seed  sites  colliding  glosses  +landmass  +river");
    for (seed, sites) in &per_seed {
        let sites = sites.as_slice();
        let g: BTreeSet<&str> = sites.iter().map(|s| s.gloss.as_str()).collect();
        let gl: BTreeSet<(&str, Option<u32>)> = sites
            .iter()
            .map(|s| (s.gloss.as_str(), s.landmass))
            .collect();
        let glr: BTreeSet<(&str, Option<u32>, Option<u32>)> = sites
            .iter()
            .map(|s| (s.gloss.as_str(), s.landmass, s.river))
            .collect();
        println!(
            "{:>4}  {:>5}  {:>8.1}%  {:>7}  {:>9}  {:>6}",
            seed,
            sites.len(),
            collision_pct(sites),
            g.len(),
            gl.len(),
            glr.len()
        );
        sum_g += g.len();
        sum_gl += gl.len();
        sum_glr += glr.len();
    }
    all.extend(per_seed.into_iter().flat_map(|(_, s)| s));

    println!("\n== battery total ==");
    println!("settlements: {}", all.len());
    println!("colliding:   {:.1}%", collision_pct(&all));
    println!("\n== §5 multipliers, re-taken on main ==");
    println!("distinct glosses            {sum_g}");
    println!(
        "+ landmass                  {sum_gl}  (x{:.2})",
        sum_gl as f64 / sum_g as f64
    );
    println!(
        "+ landmass + river          {sum_glr}  (x{:.2})",
        sum_glr as f64 / sum_g as f64
    );

    // The §1.3 arity table, re-taken: where does the collision cliff sit?
    println!("\n== §1.3 arity table, re-taken on main (battery) ==");
    println!("concepts  settlements  colliding");
    let mut counts: BTreeMap<&str, usize> = BTreeMap::new();
    for s in &all {
        *counts.entry(s.name.as_str()).or_insert(0) += 1;
    }
    let mut by_arity: BTreeMap<usize, (usize, usize)> = BTreeMap::new();
    for s in &all {
        let arity = s.gloss.split('-').count();
        let e = by_arity.entry(arity).or_insert((0, 0));
        e.0 += 1;
        if counts[s.name.as_str()] > 1 {
            e.1 += 1;
        }
    }
    for (arity, (n, dup)) in &by_arity {
        println!(
            "{arity:>8}  {n:>11}  {:>8.1}%",
            100.0 * *dup as f64 / *n as f64
        );
    }

    // How much of the river/landmass gain is genuinely new information, as
    // opposed to a restatement of site facts the gloss already carries? This
    // is the spec's own stated risk for the x1.99.
    let on_river = all.iter().filter(|s| s.river.is_some()).count();
    let gloss_says_water = all
        .iter()
        .filter(|s| {
            let g = s.gloss.as_str();
            g.contains("river") || g.contains("ford") || g.contains("spring")
        })
        .count();
    let both = all
        .iter()
        .filter(|s| {
            let g = s.gloss.as_str();
            s.river.is_some() && (g.contains("river") || g.contains("ford") || g.contains("spring"))
        })
        .count();
    let (lm, rv) = occupied(&all);
    println!("\n== the ceiling on items 2-4 (battery) ==");
    println!("distinct landmasses occupied by settlements: {lm}");
    println!("distinct rivers occupied by settlements:     {rv}");
    println!(
        "BEST-CASE collision rate from the landscape layer alone: {:.1}%",
        collision_floor_pct(&all)
    );

    println!("\n== is river identity orthogonal to the gloss? ==");
    println!("on a named river:        {on_river}");
    println!("gloss already says water: {gloss_says_water}");
    println!("both:                     {both}");
    println!("named landmasses reached: (per-seed above)");
}
