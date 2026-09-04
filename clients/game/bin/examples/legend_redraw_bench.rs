//! The Legend's own H1 readout — a narrow, dedicated harness, kept separate
//! from The Quadrat's `rung_bench.rs` even though it exercises the same
//! `TileCache::compose` warm path, because H1 asks a different question
//! than that harness was built to answer.
//!
//! `rung_bench.rs` measures whether direct mesh addressing holds the
//! plate's redraw budget. **H1 (spec §7.1) measures something that happened
//! AFTER that harness was written**: Task 3 pulled the elevation classifier
//! `plate.rs`'s `TileTerrain::band` used to compute for itself out into
//! `hornvale_scene::relief_band`, the same function `windows/scene`'s tile
//! builder calls for the `scene/surrounds/v2` wire field. H1 is that this
//! extraction — a refactor of where the decision is WRITTEN, not of how a
//! tile is addressed — cost nothing at the one rung a player actually holds
//! down a key to reach: `GLOBE_RUNG`, the coarsest.
//!
//! **Success criterion 1 (spec §7.1): warm redraw at 200x200, `GLOBE_RUNG`,
//! stays under 0.20 ms** — a ~3.5x allowance against The Quadrat's own
//! 0.056 ms baseline for the identical quantity, tightened from an earlier
//! 1.0 ms draft specifically so an 18x allowance could not paper over a
//! real regression. A number at or above the bar is a NULL result to be
//! reported, never retuned away (decision 0016); the designed response if
//! it fires is named in the spec, not in this file.
//!
//! Method mirrors The Quadrat's own discipline for this exact measurement:
//! check `uptime` first (a loaded box moved this quantity 1.40x in that
//! campaign, and a fifth measurement was refused rather than reported), run
//! at least three replicates, and re-measure — never average — if they
//! disagree by more than 1.4x.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example legend_redraw_bench -- --runs 5`. ALWAYS `--release`: a debug
//! build measures the optimizer, not the code.
//!
//! INFORMATIVE, never a gate — nothing in `make game-check` runs this.

use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_game::tiles::TileCache;
use hornvale_kernel::{Geosphere, NearestVertexIndex, Seed};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};

/// Replicates, unless `--runs` overrides — The Quadrat's own default.
const DEFAULT_RUNS: usize = 5;

/// `--runs N`, std-only, matching every other example in this crate.
fn args() -> usize {
    let mut runs = DEFAULT_RUNS;
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let mut i = 0;
    while i < argv.len() {
        match argv[i].as_str() {
            "--runs" => {
                runs = argv
                    .get(i + 1)
                    .expect("--runs wants a count")
                    .parse()
                    .expect("--runs");
                i += 2;
            }
            other => panic!("unknown flag {other}"),
        }
    }
    runs
}

fn main() {
    // `#[allow]` for the same reason `rung_bench.rs` and `repossess_cost.rs`
    // carry it: the root `clippy.toml`'s `disallowed-types` bans `Instant`
    // workspace-wide (decision 0001: time is `WorldTime`), and a bench is
    // the sanctioned exception.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let runs = args();
    let (w, h) = (200u16, 200u16);
    let rung = plate::GLOBE_RUNG;

    // Built once, outside the timed section, matching `plate.rs`'s own
    // build-once-pass-in discipline and `rung_bench.rs`'s precedent.
    let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);

    let (vw, vh) = plate::virtual_dims(rung);
    // Parked near the equator, where Mercator's stretch is least — the same
    // placement `rung_bench.rs` uses for the identical rung.
    let win = Window {
        depth: rung,
        origin_col: vw / 2,
        origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
    };

    let mut cache = TileCache::default();
    // Prime the cache once (the COLD draw), then measure only the WARM
    // redraws below — the quantity H1 names, not the cold fill.
    let grid = cache.compose(
        &terrain,
        &geo,
        &index,
        &f,
        &win,
        w,
        h,
        false,
        &mut hornvale_game::plate::PlateLight::flat(false).unlit(),
    );
    std::hint::black_box(&grid);

    let mut warm = Vec::with_capacity(runs);
    for _ in 0..runs {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let grid = cache.compose(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &mut hornvale_game::plate::PlateLight::flat(false).unlit(),
        );
        warm.push(t0.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);
    }

    let min = warm.iter().copied().fold(f64::INFINITY, f64::min);
    let max = warm.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    println!(
        "legend_redraw_bench: {w}x{h} tiles at rung {rung} (GLOBE_RUNG, coarsest), {runs} runs"
    );
    print!("  warm redraw ms, each replicate:");
    for v in &warm {
        print!("  {v:.4}");
    }
    println!();
    println!("  min {min:.4}  max {max:.4}  spread {:.2}x", max / min);
    println!(
        "  bar: under 0.20 ms (spec Sec7.1); The Quadrat's own baseline for this quantity was 0.056 ms"
    );
}
