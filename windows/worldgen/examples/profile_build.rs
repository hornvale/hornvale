//! Committed build profiler (spec §7). Runs a representative seed sample,
//! sums each stage's wall-clock time, and prints per-stage shares. Kept in
//! the tree so the profile stays honest as later epochs add stages.
//!
//! Run: `cargo run -p hornvale-worldgen --example profile_build -- [SAMPLE]`
//! (SAMPLE defaults to 24 seeds starting at 0.)

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{BuildProfile, SettlementPins, SkyChoice, build_world, profiled};

fn main() {
    let sample: u64 = std::env::args()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(24);

    let mut totals: Vec<(&'static str, f64)> = Vec::new();
    for seed in 0..sample {
        let (result, profile): (_, BuildProfile) = profiled(|| {
            build_world(
                Seed(seed),
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
            )
        });
        if result.is_err() {
            continue; // a genesis refusal contributes no timing
        }
        for (label, dur) in profile.stages {
            let secs = dur.as_secs_f64();
            match totals.iter_mut().find(|(l, _)| *l == label) {
                Some((_, acc)) => *acc += secs,
                None => totals.push((label, secs)),
            }
        }
    }

    let grand: f64 = totals.iter().map(|(_, s)| *s).sum();
    println!("build profile over {sample} seeds (total {grand:.3}s):");
    for (label, secs) in &totals {
        let share = if grand > 0.0 {
            100.0 * secs / grand
        } else {
            0.0
        };
        println!("  {label:<28} {secs:8.3}s  {share:5.1}%");
    }
}
