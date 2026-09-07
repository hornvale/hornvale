//! The Sett's cost readout (Task 7) — what the walk view's rose raster
//! costs per keypress, and which half of it the cost is in.
//!
//! **INFORMATIVE, never a gate** — nothing in `make game-check` runs this,
//! and no figure below has a bar to clear. The measurement was
//! preregistered (decision 0016) with a prediction of **1.0 to 2.0 ms** per
//! keypress against a frozen baseline of **0.3801 ms**, and the prediction
//! was **falsified**: the shipped figure is **9.71 ms** (five clean takes
//! of five replicates each spanned 9.63-9.96 ms). What that means is
//! recorded in ledger S21; what this file does is let anyone re-take the
//! number and see the same split.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example sett_raster_bench -- --keys 40 --runs 5`. ALWAYS `--release`:
//! a debug build measures the optimizer, not the code. Check `uptime`
//! first, run at least three replicates, and **re-measure rather than
//! average** if they disagree by more than 1.4x — this repository has
//! already discarded one timing pair taken at load average 50 as 3.3x
//! wrong.
//!
//! # The four things measured, and why each one
//!
//! 1. **The shipped walk-band redraw**, per keypress at 104x56 — the same
//!    quantity, the same size and the same shape as
//!    `keypress_redraw_cost.rs`, whose pre-campaign figure is the frozen
//!    baseline.
//! 2. **The Mercator control, in the same binary.** A frozen baseline is a
//!    claim with a date, and comparing it against a figure from a later
//!    binary confounds the raster with everything else that moved between
//!    them. `Driver::drawing_the_walk_view` is `at_walk_band_rung() &&
//!    focus != Focus::Map`, so submitting `map` and zooming back in to
//!    `plate::BAND_B_RUNG` reaches the Mercator arm of the identical
//!    `world_plate_for_redraw`, on the same world, at the same rung and the
//!    same plate size — no source edit, so a future reader gets the control
//!    for free. Both preconditions are asserted rather than assumed. It
//!    resolves the map strip, which the walk view does not, so it measures
//!    a little MORE than the raster swap; the tighter control is the
//!    temporary `drawing_the_walk_view() -> false` mutation, which is not
//!    committable and whose figure (0.4161 ms) is in ledger S21. The two
//!    controls agree to within 1.01x, so the strip costs nothing worth
//!    separating and either one attributes the delta.
//! 3. **The raster build alone**, cold and warm, with the memo's hits,
//!    misses and entries. The memo is the whole of the design's cost
//!    argument (`rose::RoseMemo`'s own doc), so the hit rate is reported
//!    beside the wall time rather than left to be inferred.
//! 4. **The per-box facet clone.** `plate::terrain_at_facet` clones the
//!    borrowed facet into every returned `TileTerrain` — one `Vec<u8>` of
//!    `depth` bytes per box per frame, which the Mercator path did not pay
//!    (ledger, "Carried to Task 7"). It was one of the campaign's two named
//!    candidate costs, so it is priced here rather than argued about.
//!
//! # What the answer turned out to be
//!
//! **Neither named candidate, and not the addressing either.** Measured by
//! ablation — each switch a temporary edit, reverted, never committed;
//! ledger S21 carries every replicate. Shares rather than raw milliseconds,
//! because an instrumented binary is a few percent slower than the shipped
//! one and only the ratios are comparable across the two sets:
//!
//! | ablated | share of the redraw |
//! |---|---|
//! | the per-box terrain read (`terrain_at_facet` + `terrain_box`) | **81.8%** |
//! | — of which one line: `Facet::corner_weights` in the height blend | **65.3%** |
//! | — of which the reflectance path (context + `(facet, season)` cache) | 5.2% |
//! | the graph river arm (`walk_polyline_on_raster`) | 6.4% |
//! | everything else — raster build, feature layer, perception layer | 12.0% |
//!
//! Two causes compose, and the second is the one nobody named:
//!
//! 1. **The graph arm has no `TileCache`.**
//!    `Driver::world_plate_for_redraw`'s Mercator arm composes through
//!    `tiles::TileCache`, so a warm redraw recomputes nothing, while
//!    `plate::draw_terrain_layer_from_raster` reads every box of the plate
//!    on every frame. The rose figure is flat in `--keys` (9.89 ms at one
//!    keypress, 10.10 ms at forty) — there is nothing to amortise. The
//!    Mercator arm is flat for the opposite reason: its cold fill happens
//!    before the timer starts and never happens again.
//! 2. **A per-tile cost the cache had been hiding.**
//!    `terrain_at_facet` resolves corner weights TWICE — once at the
//!    grid-level ancestor through the shared `RoomMeshMemo`
//!    (`corner_weights_memo`), and once at the tile's OWN rung, un-memoised
//!    (`facet.corner_weights(geo, index)`), for The Hachure's bilinear
//!    height blend. At `BAND_B_RUNG` that second call is a fresh
//!    nearest-vertex resolution per box per frame, and it alone is 65.3% of
//!    the whole redraw. Removing the cache is what exposed it; it is not a
//!    cost the rose raster introduced.
//!
//! The two named candidates both measured small and are reported above by
//! the bench itself: the memoised `heading_rose` chain (the raster build,
//! ~6% warm) and the per-box facet clone (~0.8%). The memo works exactly as
//! `rose::RoseMemo`'s doc claims — every miss falls in the cold build, and
//! a warm frame is 100% hits.
//!
//! **No fix is attempted here.** The preregistered branch table (plan Task
//! 7) puts a figure above 5.0 ms in the "stop and report" band precisely
//! because something other than addressing would have to be being paid per
//! frame — which is what was found. Retuning after unblinding is what
//! decision 0016 forbids.

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;
use hornvale_game::plate;
use hornvale_game::rose::{RoseMemo, RoseRaster};
use hornvale_game_core::Focus;
use hornvale_vessel::PossessTarget;

/// Terminal width the redraw is asked for — the size `keypress_redraw_cost`
/// and several of `driver.rs`'s own acceptance tests already use, and the
/// size the baseline this bench answers to was taken at.
const W: u16 = 104;
/// See [`W`].
const H: u16 = 56;

/// Keypresses per run, unless `--keys` says otherwise: a short ordinary
/// word, repeated, so the sequence is "just typing" the whole way through.
const DEFAULT_KEYS: usize = 40;

/// Replicates per measured quantity, unless `--runs` says otherwise.
const DEFAULT_RUNS: usize = 5;

/// The frozen baseline this campaign preregistered against: the same
/// quantity, on this branch, before any of The Sett's code existed.
const BASELINE_MS: f64 = 0.3801;

/// An upper bound on how many `Action::Zoom(1)` presses can be needed to
/// climb from `plate::map_entry_rung` to `plate::BAND_B_RUNG` — the ladder
/// is `GLOBE_RUNG..=BAND_B_RUNG`, so its whole length is a safe cap and a
/// bounded loop cannot hang if the ladder is ever re-shaped.
const ZOOM_CAP: u32 = plate::BAND_B_RUNG - plate::GLOBE_RUNG + 1;

fn median(mut xs: Vec<f64>) -> f64 {
    xs.sort_by(f64::total_cmp);
    xs[xs.len() / 2]
}

fn args() -> (usize, usize) {
    let mut keys = DEFAULT_KEYS;
    let mut runs = DEFAULT_RUNS;
    let mut it = std::env::args().skip(1);
    while let Some(flag) = it.next() {
        match flag.as_str() {
            "--keys" => {
                keys = it
                    .next()
                    .and_then(|s| s.parse().ok())
                    .expect("--keys takes an integer");
            }
            "--runs" => {
                runs = it
                    .next()
                    .and_then(|s| s.parse().ok())
                    .expect("--runs takes an integer");
            }
            other => panic!("unrecognized flag {other}"),
        }
    }
    (keys, runs)
}

/// Submit the literal line `map`, then zoom back in to the walk rung.
///
/// This is the control's entire mechanism and it is worth stating plainly:
/// `Driver::drawing_the_walk_view` is a conjunction of the rung and the
/// focus, so satisfying the rung and breaking the focus selects the
/// Mercator arm of the very same redraw. The map deliberately does NOT open
/// at the walk rung (`plate::map_entry_rung` is coarser — a consultation
/// opened at band B draws one vertex's reading across the whole screen), so
/// the climb back is required, not incidental.
fn into_the_map_at_the_walk_rung(driver: &mut Driver) {
    for c in "map".chars() {
        driver.apply(Action::Type(c));
    }
    driver.apply(Action::Submit);
    for _ in 0..ZOOM_CAP {
        if driver.window().depth == plate::BAND_B_RUNG {
            break;
        }
        driver.apply(Action::Zoom(1));
    }
    assert_eq!(
        driver.focus(),
        Focus::Map,
        "the control must break the focus half of drawing_the_walk_view"
    );
    assert_eq!(
        driver.window().depth,
        plate::BAND_B_RUNG,
        "the control must hold the rung half, or it measures a coarser plate"
    );
}

/// One replicate of "type `keys` characters, redrawing after each" —
/// returning milliseconds per keypress.
///
/// The plate is drawn ONCE before the timer starts, exactly as
/// `keypress_redraw_cost` does it, so both arms are measured WARM. That is
/// load-bearing for reading the two figures against each other: the
/// Mercator arm's cold tile fill is expensive and happens exactly once, and
/// leaving it inside the timed section would price it as a per-keypress
/// cost at `--keys 1` and not at `--keys 40`.
fn one_run(keys: usize, word: &[char], as_map: bool) -> f64 {
    // `#[allow]` on the enclosing item, not on the `use`: the root
    // `clippy.toml`'s `disallowed-types` bans `Instant` workspace-wide
    // (decision 0001, time is `WorldTime`) and clippy's config lookup walks
    // up the directory tree regardless of workspace membership, so it
    // reaches this crate too. A bench is the sanctioned exception, and
    // `legend_redraw_bench.rs` is the precedent for where the attribute
    // goes.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let mut driver = Driver::start(42, PossessTarget::Flagship).expect("seed 42 generates");
    if as_map {
        into_the_map_at_the_walk_rung(&mut driver);
    }
    assert!(
        driver.world_plate_for_redraw(W, H).is_some(),
        "seed 42's flagship opens on the walk band, per Driver::start's own doc"
    );

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    for i in 0..keys {
        driver.apply(Action::Type(word[i % word.len()]));
        let grid = driver.world_plate_for_redraw(W, H);
        std::hint::black_box(&grid);
    }
    t0.elapsed().as_secs_f64() * 1000.0 / keys as f64
}

fn main() {
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let (keys, runs) = args();
    // Inert letters — `Action::Type` only inserts into the line buffer, so
    // what matters is the COUNT of redraws, never which letters they are.
    let word: Vec<char> = "walkingthroughthewoods".chars().collect();

    let rose: Vec<f64> = (0..runs).map(|_| one_run(keys, &word, false)).collect();
    let merc: Vec<f64> = (0..runs).map(|_| one_run(keys, &word, true)).collect();
    let rose_med = median(rose.clone());
    let merc_med = median(merc.clone());

    // The raster on its own. The anchor and the plate's size are taken off
    // a real driver rather than restated here, so this cannot drift from
    // what `world_plate_for_redraw` actually builds.
    let mut driver = Driver::start(42, PossessTarget::Flagship).expect("seed 42 generates");
    let grid = driver
        .world_plate_for_redraw(W, H)
        .expect("the flagship opens on the walk band");
    let (pw, ph) = (grid.width(), grid.height());
    let anchor = driver.observer_facet();

    let mut memo = RoseMemo::new();
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let built = RoseRaster::build(&anchor, pw, ph, &mut memo);
    let cold = t0.elapsed().as_secs_f64() * 1000.0;
    std::hint::black_box(&built);
    let (cold_hits, cold_misses) = (memo.hits(), memo.misses());

    let mut warm = Vec::with_capacity(runs);
    for _ in 0..runs {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t = Instant::now();
        let built = RoseRaster::build(&anchor, pw, ph, &mut memo);
        warm.push(t.elapsed().as_secs_f64() * 1000.0);
        std::hint::black_box(&built);
    }
    let warm_med = median(warm.clone());

    // The per-box facet clone, priced at the plate's own box count.
    let boxes = usize::from(pw) * usize::from(ph);
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut sink = 0usize;
    for _ in 0..boxes {
        sink += std::hint::black_box(anchor.clone()).path.len();
    }
    let clones = t.elapsed().as_secs_f64() * 1000.0;
    std::hint::black_box(sink);

    println!(
        "sett_raster_bench: {W}x{H} terminal, {pw}x{ph} plate = {boxes} boxes, {keys} keys/run, {runs} runs"
    );
    println!(
        "  INFORMATIVE, never a gate. Preregistered prediction 1.0-2.0 ms; FALSIFIED (ledger S21)."
    );
    print!("  rose raster (shipped), ms/keypress:");
    for v in &rose {
        print!("  {v:.4}");
    }
    println!("\n    median {rose_med:.4} ms");
    print!("  Mercator control (Focus::Map at BAND_B_RUNG, same binary):");
    for v in &merc {
        print!("  {v:.4}");
    }
    println!(
        "\n    median {merc_med:.4} ms   rose/control {:.2}x",
        rose_med / merc_med
    );
    println!(
        "  frozen pre-campaign baseline {BASELINE_MS:.4} ms; control/baseline {:.2}x (what else drifted)",
        merc_med / BASELINE_MS
    );
    println!(
        "  raster build alone: cold {cold:.4} ms ({cold_hits} memo hits / {cold_misses} misses), \
warm median {warm_med:.4} ms over {runs}"
    );
    println!(
        "    memo after the whole sweep: {} hits / {} misses, {} entries",
        memo.hits(),
        memo.misses(),
        memo.entries()
    );
    println!("  {boxes} facet clones (terrain_at_facet's per-box allocation): {clones:.4} ms");
    println!(
        "  so of the shipped {rose_med:.3} ms, the raster build is {:.1}% and the clones {:.1}%; \
the rest is the uncached per-box terrain read (see this file's own doc)",
        100.0 * warm_med / rose_med,
        100.0 * clones / rose_med
    );
}
