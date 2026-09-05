//! What does it cost the observer to CLONE what a view needs, so a rung's
//! snapshot can cross an `mpsc::channel` from the genesis worker to the thread
//! that draws? The Overture Task 3, ruling R5.
//!
//! **Kept rather than thrown away** because `overture::genesis`'s module doc
//! cites this harness's numbers to justify a plain clone over an `Arc`, and a
//! committed cost is a claim with a date: a later campaign that grows the ledger
//! (the `Settlements` rung already carries 18,722 facts) needs to be able to
//! re-take the measurement rather than trust a paragraph. Same posture as
//! `rung_bench.rs` beside it — INFORMATIVE, never a gate; nothing in
//! `make game-check` runs it.
//!
//! Measured 2026-08-28 on the M1 Max, three runs: 21.870 / 24.489 / 23.663 ms
//! across all four rungs, 0.85%-0.94% of the observed build's wall time.
//!
//! Run: `cargo run --release --manifest-path clients/game/bin/Cargo.toml
//! --example overture_clone_cost`. ALWAYS `--release`: a debug build measures
//! the optimizer, not the code (the same warning `rung_bench.rs` carries).

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, RungArtifacts, SettlementPins, WorldComponents, build_world_observed,
};

/// Time `f` in milliseconds, returning its value alongside the elapsed time.
///
/// The single home of this harness's wall-clock read, so the `#[allow]` the root
/// `clippy.toml` requires (`disallowed-types` bans `Instant`, decision 0001, and
/// clippy's config lookup reaches `clients/`) sits in one place with one reason
/// rather than on a dozen statements. `rung_bench.rs` takes the same allow for
/// the same reason. Nothing measured here reaches a world, a ledger or a
/// committed artifact: it is printed to a terminal.
#[allow(clippy::disallowed_types)]
fn timed<T>(f: impl FnOnce() -> T) -> (T, f64) {
    let started = std::time::Instant::now();
    let value = f();
    (value, started.elapsed().as_secs_f64() * 1000.0)
}

fn main() {
    let wc = WorldComponents::assemble().expect("registries");
    let mut rows: Vec<(BuildDepth, usize, f64, f64, f64)> = Vec::new();

    let (world, build_ms) = timed(|| {
        build_world_observed(
            Seed(42),
            &SkyPins::default(),
            &TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Full,
            &mut |rung, world, art: RungArtifacts<'_>| {
                let (kept_world, world_ms) = timed(|| world.clone());
                let (kept_terrain, terrain_ms) = timed(|| art.terrain.cloned());
                let (kept_climate, climate_ms) = timed(|| art.climate.cloned());
                rows.push((rung, world.ledger.len(), world_ms, terrain_ms, climate_ms));
                // Held to the end of the callback so nothing is optimized away,
                // then dropped — which the real observer does not do (it sends
                // them), so this is if anything a slight over-estimate.
                std::hint::black_box((&kept_world, &kept_terrain, &kept_climate));
            },
        )
        .expect("seed 42 builds")
    });

    println!("rung          facts    world_ms  terrain_ms  climate_ms   total_ms");
    let mut total = 0.0;
    for (rung, facts, w, t, c) in &rows {
        println!(
            "{rung:<13?} {facts:>6}  {w:>9.3}  {t:>10.3}  {c:>10.3}  {:>9.3}",
            w + t + c
        );
        total += w + t + c;
    }
    println!("\nclone total: {total:.3} ms over {} rungs", rows.len());
    println!("observed build wall (incl. clones): {build_ms:.3} ms");
    println!("clone share of the build: {:.2}%", total / build_ms * 100.0);
    println!("final ledger: {} facts", world.ledger.len());
}
