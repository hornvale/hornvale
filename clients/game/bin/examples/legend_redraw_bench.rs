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
use hornvale_kernel::{Seed, World};
use hornvale_locale::LocaleContext;

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
    //
    // **Through a `LocaleContext`, and lit, since The Wash's Task 6 fix
    // round 1.** This harness used to derive terrain directly and compose
    // with `PlateLight::flat(false).unlit()` — which, after Task 6, measured
    // a path the client does not ship. The shipped draw asks the locale for
    // every tile's reflectance and collapses it through the observer;
    // measured at 104x52, that is +79% on the COLD fill (46.7 -> 83.8 ms).
    // A benchmark that cannot see a 1.79x in its own subject is worse than
    // none, so this one now draws what `Driver::world_plate_for_redraw`
    // draws.
    //
    // Terrain, geosphere and index are taken OFF the context rather than
    // derived beside it: a context answering about one seed-42 terrain while
    // the plate draws another would be a cost measurement over an incoherent
    // pair. The seed-42 terrain a bare `World` derives is not bit-identical
    // to `hornvale_terrain::generate(Seed(42), ..)`'s (the world derives its
    // own stream from the root seed), so this is a different world from the
    // one this harness measured before Task 6 — the GEOMETRY, which is what
    // the cost tracks, is the same mesh at the same level.
    let ctx = LocaleContext::build(&World::new(Seed(42))).expect("seed 42 builds a context");
    let terrain = ctx.terrain();
    let geo = terrain.geosphere();
    let index = ctx.nearest_index();
    let f = mercator::frame_for(false);

    // ONE illuminant and ONE observer for the whole run, exactly as a real
    // draw resolves them once above the tile loop. The reflectance cache is
    // per PASS, not shared: sharing it would let the lit pass be warmed by
    // whatever ran before it.
    let light = plate::PlateLight::flat(false);

    let (vw, vh) = plate::virtual_dims(rung);
    // Parked near the equator, where Mercator's stretch is least — the same
    // placement `rung_bench.rs` uses for the identical rung.
    let win = Window {
        depth: rung,
        origin_col: vw / 2,
        origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
    };

    // ONE pass, LIT or not. Both are measured every run so the harness can
    // report its own subject's delta rather than a reader having to edit it
    // to find out (Task 6 fix round 1): the shipped path is the lit one, and
    // the unlit figure is the control that says how much of the number is
    // the spectral collapse.
    let pass = |lit: bool| -> (f64, Vec<f64>, u64, u64, usize) {
        let mut cache = TileCache::default();
        let mut store = plate::ReflectanceCache::new();

        // Prime the cache once (the COLD draw). H1 names the WARM redraw, so
        // that is still the number with a bar against it — but the cold fill
        // is TIMED and reported now, because the cold fill is where the lit
        // path's cost lives: a warm redraw consults no reflectance at all
        // (every tile is served from `TileCache`), so it is the half that did
        // NOT move, and a harness reporting only that half is blind to the
        // half that did.
        #[allow(clippy::disallowed_types)] // benchmark harness
        let cold_t0 = Instant::now();
        let grid = cache.compose(
            terrain,
            geo,
            index,
            &f,
            &win,
            w,
            h,
            false,
            // Rebuilt per compose because `Spectral` holds `&mut` on the
            // reflectance cache; the illuminant and observer behind it are
            // the ONE pair built above, never a fresh collapse per draw.
            &mut if lit {
                light.lit(
                    &ctx,
                    hornvale_kernel::WorldTime::GENESIS,
                    plate::season_bucket(0.0),
                    Some(&mut store),
                )
            } else {
                light.unlit()
            },
        );
        let cold = cold_t0.elapsed().as_secs_f64() * 1000.0;
        std::hint::black_box(&grid);
        let cold_misses = store.misses();

        let mut warm = Vec::with_capacity(runs);
        for _ in 0..runs {
            #[allow(clippy::disallowed_types)] // benchmark harness
            let t0 = Instant::now();
            let grid = cache.compose(
                terrain,
                geo,
                index,
                &f,
                &win,
                w,
                h,
                false,
                &mut if lit {
                    light.lit(
                        &ctx,
                        hornvale_kernel::WorldTime::GENESIS,
                        plate::season_bucket(0.0),
                        Some(&mut store),
                    )
                } else {
                    light.unlit()
                },
            );
            warm.push(t0.elapsed().as_secs_f64() * 1000.0);
            std::hint::black_box(&grid);
        }
        (cold, warm, cold_misses, store.hits(), store.len())
    };

    let (unlit_cold, unlit_warm, ..) = pass(false);
    let (cold, warm, cold_misses, hits, entries) = pass(true);

    let min = warm.iter().copied().fold(f64::INFINITY, f64::min);
    let max = warm.iter().copied().fold(f64::NEG_INFINITY, f64::max);
    let unlit_min = unlit_warm.iter().copied().fold(f64::INFINITY, f64::min);
    println!(
        "legend_redraw_bench: {w}x{h} tiles at rung {rung} (GLOBE_RUNG, coarsest), {runs} runs"
    );
    println!("  DRAWN LIT — the path Driver::world_plate_for_redraw ships.");
    println!(
        "  cold fill: {cold:.3} ms lit vs {unlit_cold:.3} ms unlit ({:.2}x); \
{cold_misses} reflectance misses, {hits} hits, {entries} entries",
        cold / unlit_cold
    );
    print!("  warm redraw ms, each replicate:");
    for v in &warm {
        print!("  {v:.4}");
    }
    println!();
    println!("  min {min:.4}  max {max:.4}  spread {:.2}x", max / min);
    println!(
        "  warm, unlit control: min {unlit_min:.4} ms ({:.2}x)",
        min / unlit_min
    );
    println!(
        "  bar: under 0.20 ms (spec Sec7.1); The Quadrat's own baseline for this quantity was 0.056 ms"
    );
}
