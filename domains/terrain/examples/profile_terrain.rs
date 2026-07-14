//! Terrain-internal micro-profiler (spec §4 / Stage 1 Task 4). Answers: is
//! the `strongest()` triple-recompute (`thickness_at`, `age_at`,
//! `continental_at` on `CrustField`) a material share of the terrain stage?
//! Stays in-domain (no worldgen dep) — it rebuilds the same `CrustField`
//! `hornvale_terrain::generate` builds internally, then reads every cell
//! through all three craton-sweeping readers, the worst case if a future
//! consumer read all three per cell.
//!
//! Run: `cargo run -p hornvale-terrain --example profile_terrain -- [SAMPLE]`
//! (SAMPLE defaults to 8 seeds starting at 0.)

// The profiler measures wall-clock durations for a committed diagnostic
// only — it never touches WorldTime or facts, so it is exempt from the
// no-wall-clock-in-the-sim rule (the sanctioned Instant use for this crate).
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_kernel::{Geosphere, Seed};
use hornvale_terrain::crust::{CrustField, draw_cratons};
use hornvale_terrain::elevation::resolve_ocean_fraction;
use hornvale_terrain::{GLOBE_LEVEL, TerrainPins, generate, streams};

fn main() {
    let sample: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(8);
    let pins = TerrainPins::default();
    let level = pins.globe_level.unwrap_or(GLOBE_LEVEL);
    let geo = Geosphere::new(level);

    let (mut gen_secs, mut triple_secs) = (0.0_f64, 0.0_f64);
    for seed in 0..sample {
        let world_seed = Seed(seed);

        #[allow(clippy::disallowed_types)]
        let t0 = Instant::now();
        generate(world_seed, &geo, &pins).expect("terrain builds");
        gen_secs += t0.elapsed().as_secs_f64();

        // Rebuild the same CrustField `generate` assembles internally
        // (same seed derivation, same craton draw), then read every cell
        // through all three craton-sweeping readers.
        let terrain_seed = world_seed.derive(streams::ROOT);
        let mut notes = Vec::new();
        let ocean_target = resolve_ocean_fraction(terrain_seed, &pins, &mut notes);
        let cratons = draw_cratons(terrain_seed, &pins, ocean_target, &mut notes);
        let field = CrustField::new(terrain_seed, cratons);

        #[allow(clippy::disallowed_types)]
        let t1 = Instant::now();
        let mut sink = 0.0_f64;
        for cell in geo.cells() {
            let p = geo.position(cell);
            sink += field.thickness_at(p).get();
            sink += field.age_at(p);
            sink += if field.continental_at(p) { 1.0 } else { 0.0 };
        }
        std::hint::black_box(sink);
        triple_secs += t1.elapsed().as_secs_f64();
    }
    println!("terrain profile over {sample} seeds:");
    println!("  generate             {gen_secs:8.3}s");
    println!(
        "  triple per-cell read {triple_secs:8.3}s  ({:.1}% of generate)",
        if gen_secs > 0.0 {
            100.0 * triple_secs / gen_secs
        } else {
            0.0
        }
    );
    println!(
        "(If the triple-read share is large, the single-sweep collapse is worth Stage 2 Task 8.)"
    );
}
