//! THROWAWAY MEASUREMENT (The Portolan, Task 3). Answers the task's own
//! F2/F3/H1 questions directly against `resolve_at`/`CellFeatureIndex` —
//! seed 42, `GLOBE_LEVEL`, one people ("aeldrin", the same arbitrary choice
//! `portolan_spike.rs`/`portolan_resolution_spike.rs` made and for the same
//! reason: this crate's own gazetteer/volcano fixtures already draw with
//! it).
//!
//! Not a design and ships no interface anyone should call — a sibling of
//! the two spikes it measures downstream of, deleted at campaign close per
//! the plan's Stage 3 (`docs/superpowers/plans/2026-08-19-the-portolan.md`
//! §12).
//!
//! Run: `cargo run -p hornvale-worldgen --example portolan_measure --release`

// F2 measures wall-clock resolution cost per keypress (the task's own
// question), never touches a `Fact`, and this whole file is a throwaway
// measurement example rather than sim code — the same exemption
// `windows/worldgen/src/lib.rs`'s `BuildProfile` states for its own
// stage-timing use of `Instant` (decision 0001 bans wall-clock time from
// the deterministic sim; this is diagnostic, not sim, output).
#![allow(clippy::disallowed_types)]

use hornvale_kernel::{CellId, Geosphere, Seed};
use hornvale_language::{Envelope, ExoticSeg, MorphOptions, draw_phonology};
use hornvale_terrain::landscape::CellFeatureIndex;
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use hornvale_worldgen::{gazetteer_features, resolve_at};
use std::time::Instant;

const PEOPLE: &str = "aeldrin";
const PLATE_WIDTH: usize = 40;

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
    let features = gazetteer_features(seed, &geo, &terrain);

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

    // ---- F3: the index's size, and the extent total it was built from.
    let extent_total: usize = features.iter().map(|f| f.extent.len()).sum();
    let build_start = Instant::now();
    let index = CellFeatureIndex::build(&features);
    let build_elapsed = build_start.elapsed();

    let cell_count = geo.cell_count();
    let mut covered_cells = 0usize;
    let mut stack_entries = 0usize; // sum of |index.at(cell)| over covered cells
    for c in geo.cells() {
        let stack = index.at(c);
        if !stack.is_empty() {
            covered_cells += 1;
            stack_entries += stack.len();
        }
    }

    println!("The Portolan measurement -- seed 42, GLOBE_LEVEL {level}, people {PEOPLE:?}");
    println!(
        "total cells: {cell_count}, total features: {}",
        features.len()
    );
    println!();
    println!("=== F3: index size vs. the extent total it was built from ===");
    println!(
        "  extent total (sum of Feature::extent.len() over all {} features): {extent_total} CellIds",
        features.len()
    );
    println!(
        "  index: {covered_cells} covered cells (of {cell_count}, {:.3}%), {stack_entries} total (cell, FeatureId) stack entries",
        100.0 * covered_cells as f64 / cell_count as f64
    );
    println!(
        "  index/extent-total ratio: {:.4} ({stack_entries} index entries vs {extent_total} extent CellIds)",
        stack_entries as f64 / extent_total.max(1) as f64
    );
    println!("  CellFeatureIndex::build wall time: {build_elapsed:?}");

    // ---- F2: resolution cost per cursor move. One resolve_at call is what
    // a single keypress pays (index lookup + one name draw); measured over
    // every covered cell, both mean and a cheap-to-read total.
    let covered: Vec<CellId> = geo.cells().filter(|&c| !index.at(c).is_empty()).collect();
    let resolve_start = Instant::now();
    let mut names_drawn = 0usize;
    for &c in &covered {
        if resolve_at(&index, c, seed, PEOPLE, &ph, &morph_opts).is_some() {
            names_drawn += 1;
        }
    }
    let resolve_elapsed = resolve_start.elapsed();
    let per_call_ns = resolve_elapsed.as_nanos() as f64 / covered.len().max(1) as f64;

    println!();
    println!("=== F2: resolution cost per cursor move ===");
    println!(
        "  {} resolve_at calls (every covered cell) in {resolve_elapsed:?}",
        covered.len()
    );
    println!("  mean: {per_call_ns:.1} ns/call ({names_drawn} names drawn)");
    println!(
        "  decision rule: above 1 ms/call, cache the drawn name; below, do nothing -- {}",
        if per_call_ns > 1_000_000.0 {
            "ABOVE the 1ms threshold"
        } else {
            "well below the 1ms threshold"
        }
    );

    // ---- H1: preregistered -- at least 95% of resolvable cells' most-
    // specific name fits PLATE_WIDTH (40).
    let mut fits = 0usize;
    let mut over = 0usize;
    let mut max_len = 0usize;
    for &c in &covered {
        if let Some(name) = resolve_at(&index, c, seed, PEOPLE, &ph, &morph_opts) {
            let len = name.chars().count();
            max_len = max_len.max(len);
            if len <= PLATE_WIDTH {
                fits += 1;
            } else {
                over += 1;
            }
        }
    }
    let total_resolvable = fits + over;
    let fit_fraction = fits as f64 / total_resolvable.max(1) as f64;

    println!();
    println!("=== H1: does the most-specific name fit PLATE_WIDTH ({PLATE_WIDTH}) ===");
    println!(
        "  {fits}/{total_resolvable} resolvable cells fit ({:.4}%, preregistered >= 95%)",
        fit_fraction * 100.0
    );
    println!("  {over} over budget, max name length {max_len} chars");
    println!(
        "  H1 is {}",
        if fit_fraction >= 0.95 {
            "CONFIRMED"
        } else {
            "FALSIFIED"
        }
    );
}
