//! The walk-band redraw's per-keypress cost (The Gallery, Task 10).
//!
//! The Quadrat's F11 measured the walk loop at **24x** its own baseline per
//! redraw — 0.089 -> 2.124 ms, size-independent — and named the cause:
//! `walk_band_scene()` doing a full `Snapshot::parse(&self.cached)` plus a
//! `purview(0)` call on **every** redraw, including keypresses that are
//! just typing in the command pane. `Driver::on_walk_band` already answers
//! the one question that parse existed to ask, maintained once per turn by
//! `refresh()`; this bench is the before/after for reading it instead.
//!
//! INFORMATIVE, never a gate — nothing in `make game-check` runs this.
//!
//! Run: `cargo run --manifest-path clients/game/bin/Cargo.toml --release
//! --example keypress_redraw_cost -- --keys 40 --runs 5`. ALWAYS
//! `--release`: a debug build measures the optimizer, not the code (the
//! same warning `repossess_cost.rs` and `rung_bench.rs` carry). Check
//! `uptime` first — this campaign has already discarded one timing pair
//! taken at load average 50 as 3.3x wrong.

use hornvale_game::driver::Driver;
use hornvale_game::input::Action;
use hornvale_vessel::PossessTarget;

/// Fixed terminal size for the redraw call — matches the size several of
/// `driver.rs`'s own acceptance tests already use.
const W: u16 = 104;
/// See [`W`].
const H: u16 = 56;

/// Keypresses per run, unless `--keys` says otherwise: a short ordinary
/// word, repeated, so the sequence is "just typing" the whole way through
/// (no `Submit`, no session turn).
const DEFAULT_KEYS: usize = 40;

/// Runs per measured quantity — matches `repossess_cost.rs`'s and
/// `rung_bench.rs`'s five.
const DEFAULT_RUNS: usize = 5;

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

fn main() {
    // `#[allow]` because the root `clippy.toml`'s `disallowed-types` bans
    // `Instant` workspace-wide (decision 0001: time is `WorldTime`) and
    // clippy's config lookup walks up the directory tree regardless of
    // workspace membership, so it reaches this crate too even though
    // `clients/CLAUDE.md` says the workspace rules don't bind here. A bench
    // is the sanctioned exception `repossess_cost.rs` and `rung_bench.rs`
    // already take for the identical reason.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let (keys, runs) = args();

    // A word longer than any run needs, cycled — the letters themselves are
    // inert (`Action::Type` only inserts into the line buffer; see
    // `Driver::apply`), so what matters is the COUNT of redraws, not which
    // letters they are.
    let word: Vec<char> = "walkingthroughthewoods".chars().collect();

    let mut totals = Vec::new();
    for _ in 0..runs {
        // Built fresh per run so no run starts with a warm tile cache the
        // next does not have — the same posture `rung_bench.rs`'s `cold`
        // column takes, and the honest one here since this bench exists to
        // measure a per-keypress constant, not an amortised one.
        let mut driver = Driver::start(42, PossessTarget::Flagship).expect("seed 42 generates");
        assert!(
            driver.world_plate_for_redraw(W, H).is_some(),
            "seed 42's flagship opens on the walk band, per Driver::start's own doc"
        );

        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        for i in 0..keys {
            let c = word[i % word.len()];
            driver.apply(Action::Type(c));
            let _ = driver.world_plate_for_redraw(W, H);
        }
        let elapsed = t0.elapsed().as_secs_f64() * 1000.0;
        totals.push(elapsed / keys as f64);
    }

    println!(
        "walk-band redraw, per keypress ({keys} keys/run, {runs} runs)  median {:8.4} ms  {totals:?}",
        median(totals.clone())
    );
}
