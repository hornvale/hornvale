//! The world plate's per-draw cost, by rung and plate size (The Quadrat,
//! Task 3) — the harness carrying this campaign's preregistered **H1**.
//!
//! H1, frozen before this code existed: *if a rung is a mesh depth, a tile
//! IS a facet, so terrain comes from that facet's three corner vertices by
//! direct addressing and the spatial search disappears — a 200x200
//! UNCACHED plate draws in under 50 ms.* The decision rule is the wall
//! time at `--tiles 200x200`, and a number at or above 50 ms is a NULL
//! result to be reported, never retuned away.
//!
//! `104x52 --rung 6` is the OLD reference size, kept so the before/after is
//! a comparison rather than a claim.
//!
//! INFORMATIVE, never a gate — nothing in `make game-check` runs this.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example rung_bench -- --tiles 200x200 --rung 12 --runs 5`. ALWAYS
//! `--release`: a debug build measures the optimizer, not the code (the
//! same warning `repossess_cost.rs` carries). Check `uptime` first — a
//! contended box makes any number here meaningless.

use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_kernel::{Geosphere, NearestVertexIndex, Seed};
use hornvale_terrain::{GeneratedTerrain, TerrainPins};
use std::collections::BTreeSet;

/// Runs per measured quantity, unless `--runs` says otherwise — matches
/// `repossess_cost.rs`'s five.
const DEFAULT_RUNS: usize = 5;

fn median(mut xs: Vec<f64>) -> f64 {
    xs.sort_by(f64::total_cmp);
    xs[xs.len() / 2]
}

/// `--tiles WxH`, `--rung N`, `--runs N`, std-only (the CLI's own idiom).
fn args() -> (u16, u16, u32, usize) {
    let (mut w, mut h) = (200u16, 200u16);
    let mut rung = plate::BAND_B_RUNG;
    let mut runs = DEFAULT_RUNS;
    let argv: Vec<String> = std::env::args().skip(1).collect();
    let mut i = 0;
    while i < argv.len() {
        match argv[i].as_str() {
            "--tiles" => {
                let v = argv.get(i + 1).expect("--tiles wants WxH");
                let (a, b) = v.split_once('x').expect("--tiles wants WxH");
                w = a.parse().expect("--tiles width");
                h = b.parse().expect("--tiles height");
                i += 2;
            }
            "--rung" => {
                rung = argv
                    .get(i + 1)
                    .expect("--rung wants a depth")
                    .parse()
                    .expect("--rung");
                i += 2;
            }
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
    (w, h, rung, runs)
}

fn main() {
    // `#[allow]` because the root `clippy.toml`'s `disallowed-types` bans
    // `Instant` workspace-wide (decision 0001: time is `WorldTime`) and
    // clippy's config lookup walks up the directory tree regardless of
    // workspace membership. A bench is the sanctioned exception
    // `repossess_cost.rs` already takes for the identical reason.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let (w, h, rung, runs) = args();

    // The same world `plate.rs`'s own tests build — `hornvale_terrain::
    // generate` directly, never a full `build_world`, since nothing here
    // reads the ledger.
    let geo = Geosphere::new(hornvale_terrain::GLOBE_LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate seed 42");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    // Built ONCE, outside the timed loop — `plate.rs`'s module doc's own
    // build-once-pass-in discipline.
    let index = NearestVertexIndex::new(&geo);
    let f = mercator::frame_for(false);

    let (vw, vh) = plate::virtual_dims(rung);
    // Park the window near the equator, where Mercator's stretch is least
    // and a plate covers the most distinct mesh ground per tile.
    let win = Window {
        depth: rung,
        origin_col: vw / 2,
        origin_row: (vh / 2).saturating_sub(u32::from(h) / 2),
    };
    let empty = BTreeSet::new();
    let undiscovered = hornvale_game::discovery::Discovered::default();

    let mut draws = Vec::new();
    for _ in 0..runs {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let grid = plate::draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &win,
            w,
            h,
            false,
            &empty,
            &empty,
            &undiscovered,
        );
        draws.push(t0.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);
    }

    // The SCAN count for one draw, by the kernel's own instrument rather
    // than a wall-clock proxy: `RoomMeshMemo::corner_weights_misses` counts
    // the calls that actually ran `Facet::corner_weights`, and each of those
    // is three `NearestVertexIndex::nearest_to_position` scans. A hit costs
    // none. Re-walks the plate's own tiles through the same
    // `plate::terrain_at_tile` `draw_with` paints from — one source of
    // truth, never a second copy of the loop's arithmetic.
    let mut memo = hornvale_kernel::RoomMeshMemo::default();
    for row in 0..u32::from(h) {
        for col in 0..u32::from(w) {
            let _ = plate::terrain_at_tile(
                &terrain, &geo, &index, &mut memo, &f, &win, vw, vh, row, col,
            );
        }
    }
    let misses = memo.corner_weights_misses();
    let tiles = u64::from(w) * u64::from(h);

    println!("rung_bench: {w}x{h} tiles at rung {rung} (chart {vw}x{vh}), {runs} runs");
    println!(
        "  draw wall ms:  median {:.3}  min {:.3}",
        median(draws.clone()),
        draws.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!(
        "  memo:          {misses} misses / {} hits over {tiles} tiles",
        memo.corner_weights_hits()
    );
    println!(
        "  probes/draw:   {} vertex scans (the pre-Task-3 path: {})",
        misses * 3,
        tiles * 49
    );
}
