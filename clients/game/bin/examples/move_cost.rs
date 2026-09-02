//! What the GAME CLIENT's driver pays per movement turn (handle + the
//! refresh it always does), under whatever profile this example is built
//! with (The Hone). It is what caught the missing dev profile in the first
//! place: `clients/game/bin/Cargo.toml` is a standalone workspace root the
//! root `Cargo.toml`'s Whetstone `[profile.dev]` (Cargo.toml:60-80) does not
//! reach, so `Driver::start` and every turn built at opt-level 0 until this
//! campaign gave the client workspaces their own copy of that block.
//!
//! INFORMATIVE, never a gate — nothing in `make game-check` runs this. Run
//! it twice, once at each profile, to see the gap the missing dev profile
//! used to leave:
//!   cargo run --manifest-path clients/game/bin/Cargo.toml --example move_cost
//!   cargo run --release --manifest-path clients/game/bin/Cargo.toml --example move_cost
//!
//! ## Measured
//!
//! ### before The Hone (dev profile at opt-level 0)
//!
//! `Driver::start` **27,522 ms**, movement turns **300-630 ms** each.
//!
//! ### after The Hone, default (`dev`) profile
//!
//! 2026-09-02, MacBookPro, `2c34f9e4c64fae3230a751cfca061c4bcdf3bb37`.
//! **CONTENDED** (`uptime`: `load averages: 9.27 12.44 9.86`), taken anyway
//! per the diagnostic fallback `session_wait_scaling.rs` documents.
//!
//! ```text
//! game driver: profile DEBUG; Driver::start 3730 ms
//!     look    39.667 ms
//!      map    43.054 ms
//!     go n     9.797 ms
//!     go n     9.698 ms
//!     back     9.611 ms
//!     back    40.183 ms
//!    needs    70.487 ms
//!    enter    87.199 ms
//!     look    66.578 ms
//!      map    49.694 ms
//!     go n    49.658 ms
//!     go e    49.375 ms
//!     go s    51.012 ms
//!     go w    48.523 ms
//!     look    67.454 ms
//!      out    39.681 ms
//!     go n    10.064 ms
//!     back    40.287 ms
//! ```
//!
//! ### after The Hone, `--release`
//!
//! Same run, `--release`. `uptime`: `load averages: 17.87 14.53 10.94`
//! (still CONTENDED).
//!
//! ```text
//! game driver: profile release; Driver::start 3486 ms
//!     look    35.314 ms
//!      map    39.331 ms
//!     go n     9.426 ms
//!     go n     9.170 ms
//!     back     8.905 ms
//!     back    36.564 ms
//!    needs    63.339 ms
//!    enter    78.742 ms
//!     look    60.121 ms
//!      map    45.658 ms
//!     go n    44.163 ms
//!     go e    43.323 ms
//!     go s    43.475 ms
//!     go w    43.784 ms
//!     look    59.991 ms
//!      out    35.733 ms
//!     go n     9.252 ms
//!     back    35.888 ms
//! ```
//!
//! `Driver::start` moved 27,522 ms -> 3,730 ms (dev) / 3,486 ms (release) —
//! a ~7.4-7.9x win from the profile alone, dev now within ~7% of release.
//! Turns moved from 300-630 ms to single digits through tens of ms.

use hornvale_game::driver::Driver;
use hornvale_vessel::PossessTarget;

fn main() {
    // `#[allow]` because the root `clippy.toml`'s `disallowed-types` bans
    // `Instant` workspace-wide (decision 0001: time is `WorldTime`) and
    // clippy's config lookup walks up the directory tree regardless of
    // workspace membership, so it reaches this crate too even though
    // `clients/CLAUDE.md` says the workspace rules don't bind here. A bench
    // is the sanctioned exception `windows/vessel/examples/turn_cost.rs` and
    // `repossess_cost.rs` already take for the identical reason.
    #[allow(clippy::disallowed_types)]
    use std::time::Instant;

    let profile = if cfg!(debug_assertions) {
        "DEBUG"
    } else {
        "release"
    };
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let mut d = Driver::start(42, PossessTarget::Flagship).expect("seed 42 starts");
    println!(
        "game driver: profile {profile}; Driver::start {:.0} ms",
        t0.elapsed().as_secs_f64() * 1000.0
    );
    let seq = [
        "look", "map", "go n", "go n", "back", "back", "needs", "enter", "look", "map", "go n",
        "go e", "go s", "go w", "look", "out", "go n", "back",
    ];
    for v in seq {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t = Instant::now();
        d.handle(v);
        println!("{:>8} {:>9.3} ms", v, t.elapsed().as_secs_f64() * 1000.0);
    }
}
