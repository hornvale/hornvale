//! The world plate's per-draw cost, by rung and plate size (The Quadrat,
//! Tasks 3 and 5) — the harness carrying this campaign's preregistered
//! **H1**, extended by Task 5 to measure the same plate THROUGH the tile
//! cache and to price the feature layer that is composed over it.
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
//! **Five costs, not one, because the cache turns "a draw" into several
//! different questions** (Task 5):
//!
//! - **uncached** — `plate::draw_with` on the `win` window, the Task 3
//!   number, kept on THAT window so the before/after is a comparison rather
//!   than a claim;
//! - **uncached, ALIGNED** — the same call on the `aligned` window, which is
//!   the one everything below is measured on. It exists because a ratio taken
//!   across the two windows compares two different pieces of ground, which is
//!   exactly what fix round 1 caught this harness doing;
//! - **cold** — the same plate through a FRESH [`hornvale_game::tiles::
//!   TileCache`]. Never cheaper than its own window's uncached draw and
//!   usually dearer, because a plate's edge tiles are drawn whole;
//! - **warm** — the same window again. This is a cursor move, a resize to a
//!   subrect, and a discovery: the keystrokes that used to cost a full
//!   plate;
//! - **scroll** — one column of window movement, reported twice: the
//!   BOUNDARY case (the keystroke that uncovers a new tile column, the worst
//!   case) and the MEAN over [`TILE_EDGE`] consecutive columns (what a
//!   player pressing an arrow key actually averages).
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example rung_bench -- --tiles 200x200 --rung 12 --runs 5`. ALWAYS
//! `--release`: a debug build measures the optimizer, not the code (the
//! same warning `repossess_cost.rs` carries). Check `uptime` first — a
//! contended box makes any number here meaningless.

use hornvale_game::discovery::Discovered;
use hornvale_game::mercator;
use hornvale_game::plate::{self, Window};
use hornvale_game::tiles::{TILE_EDGE, TileCache};
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
    // The window's right edge is put ON a tile boundary, so the scroll
    // measured below is the WORST case (it uncovers a whole tile column)
    // rather than a lucky interior column that uncovers nothing.
    //
    // **It must also not WRAP**, and an earlier revision of this harness
    // forgot that and reported a rung-6 worst case of ZERO tiles. At rung 6
    // the chart is 363 columns and a 200-column plate parked at the equator
    // runs off the end of it, so the aligned right edge landed back inside
    // tile 0 — already resident — and one column of scroll uncovered
    // nothing at all. The number was real and it was not the worst case.
    let target = (TILE_EDGE - (u32::from(w) % TILE_EDGE)) % TILE_EDGE;
    let room = vw.saturating_sub(u32::from(w));
    let aligned = Window {
        origin_col: if room >= target {
            // the largest aligned origin that still fits without wrapping
            ((room - target) / TILE_EDGE) * TILE_EDGE + target
        } else {
            target
        },
        ..win
    };

    let empty: BTreeSet<hornvale_kernel::Vertex> = BTreeSet::new();
    let no_sites: Vec<plate::MapSite> = Vec::new();
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
            &no_sites,
            &empty,
            &[],
            &undiscovered,
        );
        draws.push(t0.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);
    }

    // THE SAME UNCACHED DRAW ON THE **ALIGNED** WINDOW — the like-for-like
    // partner of `cached cold` below, and the fix for a real defect in this
    // harness's first revision (fix round 1, Minor 2). `draws` above is
    // measured on `win` and MUST stay there: it is the number Task 3's
    // committed table can be compared against. But `cold` is measured on
    // `aligned`, so quoting "cold is 1.12x uncached" off those two columns
    // was comparing two different pieces of ground. Two windows, two
    // uncached numbers, and each ratio taken within its own window.
    let mut draws_aligned = Vec::new();
    for _ in 0..runs {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let grid = plate::draw_with(
            &terrain,
            &geo,
            &index,
            &f,
            &aligned,
            w,
            h,
            false,
            &no_sites,
            &empty,
            &[],
            &undiscovered,
        );
        draws_aligned.push(t0.elapsed().as_secs_f64() * 1000.0);
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
                &terrain,
                &geo,
                &index,
                &mut memo,
                &f,
                &win,
                vw,
                vh,
                row,
                col,
                None,
                hornvale_kernel::WorldTime::GENESIS,
            );
        }
    }
    let misses = memo.corner_weights_misses();
    let tiles = u64::from(w) * u64::from(h);

    // ---- Task 5: the same plate THROUGH the tile cache ----------------
    //
    let mut cold = Vec::new();
    let mut warm = Vec::new();
    let mut boundary = Vec::new();
    let mut resident = 0usize;
    let mut cold_tiles = 0u64;
    let mut scroll_tiles = 0u64;
    for _ in 0..runs {
        let mut cache = TileCache::default();
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let grid = cache.compose(&terrain, &geo, &index, &f, &aligned, w, h, false);
        cold.push(t0.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);
        cold_tiles = cache.misses();

        #[allow(clippy::disallowed_types)] // benchmark harness
        let t1 = Instant::now();
        let grid = cache.compose(&terrain, &geo, &index, &f, &aligned, w, h, false);
        warm.push(t1.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);

        let scrolled = Window {
            origin_col: aligned.origin_col + 1,
            ..aligned
        };
        let before = cache.misses();
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t2 = Instant::now();
        let grid = cache.compose(&terrain, &geo, &index, &f, &scrolled, w, h, false);
        boundary.push(t2.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&grid);
        scroll_tiles = cache.misses() - before;
        resident = cache.len();
    }

    // The AMORTISED keystroke: TILE_EDGE consecutive one-column scrolls, of
    // which exactly one crosses a tile boundary.
    let mut cache = TileCache::default();
    let mut walk = aligned;
    let _ = cache.compose(&terrain, &geo, &index, &f, &walk, w, h, false);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t3 = Instant::now();
    for _ in 0..TILE_EDGE {
        walk.origin_col += 1;
        let grid = cache.compose(&terrain, &geo, &index, &f, &walk, w, h, false);
        std::hint::black_box(&grid);
    }
    let scroll_mean = t3.elapsed().as_secs_f64() * 1000.0 / f64::from(TILE_EDGE);

    // ---- Task 5: the FEATURE layer, composed over every one of those ----
    //
    // The cave roster is what `Driver::start` builds by scanning every
    // vertex, and its cardinality is recorded nowhere else in the tree.
    // Settlements come from the ledger, which this harness deliberately does
    // not build (nothing else here reads one), so the roster measured is the
    // cave half — the LARGER half by construction, since it is a scan of the
    // mesh rather than a read of a few hundred committed facts.
    let caves: Vec<plate::MapSite> = terrain
        .cave_site_vertices()
        .into_iter()
        .map(|vertex| plate::MapSite {
            kind: hornvale_vessel::site::SiteKind::Cave,
            vertex,
            // The harness prices the ROSTER SCAN and the projection, and a
            // `None` here keeps the position read to one `Geosphere::coord`
            // lookup instead of a `Facet::centroid` — measuring the loop
            // this layer actually runs per site without also measuring
            // `site_facet_for`, which runs once per site at `Driver::start`
            // and never per frame.
            placed: None,
            population: 0,
        })
        .collect();
    let all_found: Discovered = {
        let mut d = Discovered::default();
        for c in &caves {
            d.record(c.feature_id());
        }
        d
    };
    let mut base = {
        let mut c = TileCache::default();
        c.compose(&terrain, &geo, &index, &f, &aligned, w, h, false)
    };
    let mut feature_none = Vec::new();
    let mut feature_all = Vec::new();
    for _ in 0..runs {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        plate::draw_feature_layer(
            &mut base,
            &geo,
            &f,
            &aligned,
            false,
            &caves,
            &empty,
            &[],
            &undiscovered,
        );
        feature_none.push(t0.elapsed().as_secs_f64() * 1000.0);
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t1 = Instant::now();
        plate::draw_feature_layer(
            &mut base,
            &geo,
            &f,
            &aligned,
            false,
            &caves,
            &empty,
            &[],
            &all_found,
        );
        feature_all.push(t1.elapsed().as_secs_f64() * 1000.0);
    }

    println!("rung_bench: {w}x{h} tiles at rung {rung} (chart {vw}x{vh}), {runs} runs");
    println!(
        "  uncached draw ms:   median {:.3}  min {:.3}",
        median(draws.clone()),
        draws.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!(
        "  uncached, ALIGNED:  median {:.3}  min {:.3}   (the like-for-like partner of cold)",
        median(draws_aligned.clone()),
        draws_aligned.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!(
        "  cached cold ms:     median {:.3}  min {:.3}   ({cold_tiles} tiles drawn)",
        median(cold.clone()),
        cold.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!(
        "  cached warm ms:     median {:.3}  min {:.3}",
        median(warm.clone()),
        warm.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!(
        "  scroll boundary ms: median {:.3}  min {:.3}   ({scroll_tiles} tiles drawn)",
        median(boundary.clone()),
        boundary.iter().copied().fold(f64::INFINITY, f64::min)
    );
    println!("  scroll mean ms:     {scroll_mean:.3}   (over {TILE_EDGE} columns)");
    println!("  tiles resident:     {resident} squares of {TILE_EDGE} chart columns");
    println!(
        "  feature layer ms:   undiscovered {:.4}  all-discovered {:.4}   (cave roster {} of {} vertices)",
        median(feature_none),
        median(feature_all),
        caves.len(),
        geo.vertex_count()
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
