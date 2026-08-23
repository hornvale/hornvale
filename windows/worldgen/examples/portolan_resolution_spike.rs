//! THROWAWAY SPIKE (The Portolan, resolution question). A sibling of
//! `portolan_spike.rs`, which measured the map-label-collision problem; this
//! one answers the question the free-roaming-cursor campaign actually needs
//! answered first: **what does a single cell resolve to?**
//!
//! For every one of seed 42's cells, this computes the set of
//! [`Feature`]s whose `extent` contains it, and reports:
//!   1. the resolution-count histogram (0, 1, 2, 3, … features per cell),
//!   2. whether a 2+ stack is a proper nesting chain or a genuine overlap,
//!   3. the class-combination composition of stacks, top ~8 by frequency,
//!   4. what a 0-feature cell actually is,
//!   5. the naive full-string render length distribution, one people's name.
//!
//! Not a design and ships no interface anyone should call -- see
//! `.superpowers/sdd/the-portolan-resolution-report.md` for the write-up.
//!
//! Run: `cargo run -p hornvale-worldgen --example portolan_resolution_spike`

use hornvale_kernel::{CellId, Geosphere, Seed};
use hornvale_language::{Envelope, ExoticSeg, MorphOptions, draw_phonology};
use hornvale_terrain::landscape::{Feature, FeatureClass};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{feature_name, gazetteer_features};
use std::collections::BTreeMap;

/// One people's name, matching `portolan_spike.rs`'s choice for the same
/// reason: this crate's own gazetteer/volcano test fixtures already use it.
const PEOPLE: &str = "aeldrin";

/// The status-strip budget the next campaign is targeting.
const BUDGET: usize = 40;

fn morph() -> MorphOptions {
    MorphOptions {
        honorifics: false,
        shape_weights: [1.0, 1.0, 1.0],
        shape_beta: 1.0,
    }
}

fn main() {
    let level = hornvale_terrain::GLOBE_LEVEL;
    let seed = Seed(42);
    let geo = Geosphere::new(level);
    let outcome = hornvale_terrain::generate(seed, &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);

    let features: Vec<Feature> = gazetteer_features(seed, &geo, &terrain);
    let cell_count = geo.cell_count();
    println!("The Portolan resolution spike -- seed 42, GLOBE_LEVEL {level}, people {PEOPLE:?}");
    println!(
        "total cells: {cell_count}, total features: {}",
        features.len()
    );
    let mut by_class: BTreeMap<FeatureClass, usize> = BTreeMap::new();
    for f in &features {
        *by_class.entry(f.id.class).or_default() += 1;
    }
    for (class, n) in &by_class {
        println!("  {class:?}: {n}");
    }

    // ---- 1. Per-cell resolution set: which feature indices cover each cell.
    let mut res: Vec<Vec<usize>> = vec![Vec::new(); cell_count];
    for (idx, f) in features.iter().enumerate() {
        for &c in &f.extent {
            res[c.0 as usize].push(idx);
        }
    }

    // ---- Resolution-count histogram.
    let mut histogram: BTreeMap<usize, u32> = BTreeMap::new();
    for r in &res {
        *histogram.entry(r.len()).or_default() += 1;
    }
    println!();
    println!("=== 1. resolution-count histogram (cells -> feature-count) ===");
    for (count, cells) in &histogram {
        println!(
            "  {count} feature(s): {cells} cells ({:.3}%)",
            100.0 * f64::from(*cells) / cell_count as f64
        );
    }
    let max_count = histogram.keys().max().copied().unwrap_or(0);
    println!("  max: {max_count}");

    // ---- 2 & 3. Nesting vs competition, and composition, over cells with 2+.
    // Group by the exact stack (sorted feature indices) so we do the subset
    // work once per DISTINCT stack rather than once per cell -- many cells
    // share the identical stack (every cell inside one volcano's extent that
    // also sits inside one landmass shares that same two-feature stack).
    let mut stacks: BTreeMap<Vec<usize>, u32> = BTreeMap::new();
    for r in &res {
        if r.len() >= 2 {
            let mut key = r.clone();
            key.sort_unstable();
            *stacks.entry(key).or_default() += 1;
        }
    }

    let mut chain_cells: u64 = 0;
    let mut overlap_cells: u64 = 0;
    let mut chain_stacks: u32 = 0;
    let mut overlap_stacks: u32 = 0;
    // composition key: sorted class multiset -> (is_chain) -> cell count
    let mut composition: BTreeMap<(Vec<FeatureClass>, bool), u64> = BTreeMap::new();
    // For length-distribution work: (composition classes in size order,
    // is_chain, naive string length, cell weight)
    let mut length_samples: Vec<(Vec<FeatureClass>, bool, usize, u32)> = Vec::new();
    // One example stack per composition key, for the report to show.
    let mut examples: BTreeMap<(Vec<FeatureClass>, bool), (Vec<usize>, String)> = BTreeMap::new();

    let ph = draw_phonology(
        &Seed(7),
        PEOPLE,
        &Envelope {
            labiality: 1.0,
            vowel_space: 1.0,
            voicing: 1.0,
            sibilance: 1.0,
            voice_loudness: 1.0,
            tonality: 0.0,
            exotic: ExoticSeg::None,
        },
        &hornvale_language::typology::concatenative(),
    );
    let morph_opts = morph();

    for (stack, &weight) in &stacks {
        // Pairwise: is every pair subset-comparable? If so it's a chain.
        let mut is_chain = true;
        'pairs: for i in 0..stack.len() {
            for j in (i + 1)..stack.len() {
                let a = &features[stack[i]].extent;
                let b = &features[stack[j]].extent;
                if !(a.is_subset(b) || b.is_subset(a)) {
                    is_chain = false;
                    break 'pairs;
                }
            }
        }
        if is_chain {
            chain_cells += u64::from(weight);
            chain_stacks += 1;
        } else {
            overlap_cells += u64::from(weight);
            overlap_stacks += 1;
        }

        // Order smallest-extent-first for both the composition key and the
        // naive render -- when it IS a chain this is the true nesting order
        // (subset implies strictly-smaller size for distinct extents); when
        // it is NOT a chain there is no canonical order, so magnitude order
        // is the cursor's only cheap option and we render it anyway to
        // measure what that naive choice costs.
        let mut ordered = stack.clone();
        ordered.sort_by_key(|&idx| features[idx].extent.len());
        let classes: Vec<FeatureClass> =
            ordered.iter().map(|&idx| features[idx].id.class).collect();

        *composition.entry((classes.clone(), is_chain)).or_insert(0) += u64::from(weight);

        let names: Vec<String> = ordered
            .iter()
            .map(|&idx| feature_name(seed, features[idx].id, PEOPLE, &ph, &morph_opts).roman)
            .collect();
        let parts: Vec<String> = ordered
            .iter()
            .zip(&names)
            .map(|(&idx, name)| format!("{name} ({:?})", features[idx].id.class))
            .collect();
        let joiner = if is_chain { ", on " } else { " + " };
        let rendered = parts.join(joiner);
        length_samples.push((classes.clone(), is_chain, rendered.chars().count(), weight));
        examples
            .entry((classes, is_chain))
            .or_insert((ordered.clone(), rendered));
    }

    println!();
    println!("=== 2. nesting vs competition (over cells resolving to 2+) ===");
    let total_multi_cells = chain_cells + overlap_cells;
    println!(
        "  chain (proper nesting): {chain_cells}/{total_multi_cells} cells ({:.2}%), {chain_stacks} distinct stacks",
        100.0 * chain_cells as f64 / total_multi_cells.max(1) as f64
    );
    println!(
        "  overlap (not fully nested): {overlap_cells}/{total_multi_cells} cells ({:.2}%), {overlap_stacks} distinct stacks",
        100.0 * overlap_cells as f64 / total_multi_cells.max(1) as f64
    );
    if total_multi_cells > 0 {
        println!(
            "  chain fraction: {:.4}",
            chain_cells as f64 / total_multi_cells as f64
        );
    }

    println!();
    println!("=== 3. composition of 2+ stacks (top by cell count) ===");
    let mut comp_vec: Vec<((Vec<FeatureClass>, bool), u64)> = composition.into_iter().collect();
    comp_vec.sort_by_key(|&(_, cells)| std::cmp::Reverse(cells));
    for ((classes, is_chain), cells) in comp_vec.iter().take(8) {
        let joiner = if *is_chain { " subset-of " } else { " + " };
        let desc: Vec<String> = classes.iter().map(|c| format!("{c:?}")).collect();
        let label = desc.join(joiner);
        let tag = if *is_chain { "chain" } else { "overlap" };
        println!(
            "  {cells} cells ({:.3}%): {label}  [{tag}]",
            100.0 * (*cells as f64) / total_multi_cells.max(1) as f64
        );
        if let Some((_, rendered)) = examples.get(&(classes.clone(), *is_chain)) {
            println!(
                "      e.g. {rendered:?}  ({} chars)",
                rendered.chars().count()
            );
        }
    }

    // ---- 4. Cells resolving to nothing.
    println!();
    println!("=== 4. cells resolving to NOTHING ===");
    let mut none_ocean = 0u32;
    let mut none_land = 0u32;
    for (i, r) in res.iter().enumerate() {
        if r.is_empty() {
            let cell = CellId(i as u32);
            if terrain.is_ocean(cell) {
                none_ocean += 1;
            } else {
                none_land += 1;
            }
        }
    }
    let none_total = none_ocean + none_land;
    println!(
        "  total: {none_total}/{cell_count} ({:.3}%)",
        100.0 * f64::from(none_total) / cell_count as f64
    );
    println!(
        "    ocean, outside the one named Sea: {none_ocean} ({:.4}% of all cells)",
        100.0 * f64::from(none_ocean) / cell_count as f64
    );
    println!(
        "    land, outside any named Landmass: {none_land} ({:.4}% of all cells)",
        100.0 * f64::from(none_land) / cell_count as f64
    );

    // ---- 5. 40-column budget: weighted length distribution over ALL
    // multi-feature cells (weight = number of cells sharing that stack), and
    // per top-composition breakdown.
    println!();
    println!("=== 5. naive-string length distribution (all 2+ cells, weighted) ===");
    let (median, p90, max_len, over_budget_frac) = weighted_length_stats(&length_samples, BUDGET);
    println!("  median: {median}, p90: {p90}, max: {max_len}");
    println!(
        "  fraction exceeding {BUDGET} chars: {:.4} ({:.2}%)",
        over_budget_frac,
        over_budget_frac * 100.0
    );

    println!();
    println!("=== 5b. per top-composition length stats ===");
    for ((classes, is_chain), _cells) in comp_vec.iter().take(8) {
        let subset: Vec<(Vec<FeatureClass>, bool, usize, u32)> = length_samples
            .iter()
            .filter(|(c, chain, _, _)| c == classes && chain == is_chain)
            .cloned()
            .collect();
        let (median, p90, max_len, over) = weighted_length_stats(&subset, BUDGET);
        let desc: Vec<String> = classes.iter().map(|c| format!("{c:?}")).collect();
        println!(
            "  {}: median {median}, p90 {p90}, max {max_len}, >{BUDGET} chars: {:.1}%",
            desc.join(if *is_chain { " subset-of " } else { " + " }),
            over * 100.0
        );
    }
}

/// Weighted median / p90 / max / over-budget-fraction over
/// `(classes, is_chain, length, weight)` samples, weighted by `weight`
/// (each sample represents that many cells sharing the identical stack).
/// Returns `(median, p90, max, fraction_over_budget)`.
fn weighted_length_stats(
    samples: &[(Vec<FeatureClass>, bool, usize, u32)],
    budget: usize,
) -> (usize, usize, usize, f64) {
    if samples.is_empty() {
        return (0, 0, 0, 0.0);
    }
    let mut lens: Vec<(usize, u32)> = samples.iter().map(|(_, _, l, w)| (*l, *w)).collect();
    lens.sort_by_key(|&(l, _)| l);
    let total: u64 = lens.iter().map(|&(_, w)| u64::from(w)).sum();
    let max_len = lens.last().map(|&(l, _)| l).unwrap_or(0);

    let quantile = |q: f64| -> usize {
        let target = (q * total as f64).ceil() as u64;
        let mut cum = 0u64;
        for &(l, w) in &lens {
            cum += u64::from(w);
            if cum >= target.max(1) {
                return l;
            }
        }
        max_len
    };
    let median = quantile(0.5);
    let p90 = quantile(0.9);
    let over: u64 = lens
        .iter()
        .filter(|&&(l, _)| l > budget)
        .map(|&(_, w)| u64::from(w))
        .sum();
    let over_frac = over as f64 / total as f64;
    (median, p90, max_len, over_frac)
}
