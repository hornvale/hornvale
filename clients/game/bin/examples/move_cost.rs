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
//!
//! ### AFTER The Rack, default (`dev`) profile — P6
//!
//! 2026-09-02, MacBookPro, this campaign's tip. **CONTENDED** (`uptime`
//! before: `load averages: 4.71 4.69 6.93`, after: `5.06 4.77 6.93` — all
//! three over The Repose's quiet-box threshold of 4), taken anyway per the
//! same diagnostic fallback the blocks above invoke; this file is
//! INFORMATIVE and gates nothing.
//!
//! ```text
//! game driver: profile DEBUG; Driver::start 3843 ms
//!     look    10.103 ms
//!      map    14.102 ms
//!     go n    10.264 ms
//!     go n    10.221 ms
//!     back    10.339 ms
//!     back    10.391 ms
//!    needs     9.969 ms
//!    enter    47.377 ms
//!     look    29.163 ms
//!      map    19.171 ms
//!     go n    18.717 ms
//!     go e    19.818 ms
//!     go s    20.046 ms
//!     go w    19.750 ms
//!     look    29.450 ms
//!      out    10.287 ms
//!     go n    10.387 ms
//!     back    10.363 ms
//! ```
//!
//! **P6 (spec §4: every movement turn ≤ 15 ms) MISSED on this reading.**
//! Outdoor turns (**9.97-14.10 ms**) clear it; every indoor turn does not —
//! `enter` 47.377 ms, the two post-`enter` `look`s 29.163/29.450 ms, and
//! `map`/`go n/e/s/w` inside the chamber 18.7-20.0 ms.
//! That was NOT a shadowcast cost, and this sentence used to say it was:
//! The Terrier (2026-09-03) decomposed the derivation and found the
//! shadowcast at 0.012 ms and the whole item in `brief_of` rebuilding the
//! world's occupation register per call — see the AFTER-Terrier block
//! below and `windows/vessel/examples/move_cost.rs`.
//! **Not tuned**: the task this block closes is measure and
//! report, not chase the ceiling.
//!
//! **The outdoor range, stated once with its rule** (The Rack, final review —
//! the ledger, the chronicle and this file had drifted to three different
//! low endpoints): it is the min and max over EVERY outdoor row in the block
//! above, `needs` included, rounded half-up to two decimals — `needs`
//! 9.969 -> **9.97** and `map` 14.102 -> **14.10**. An earlier draft here
//! read "10.1-14.1", which silently dropped `needs` from the population and
//! rounded to one decimal; the ledger and chronicle now quote 9.97-14.10 to
//! match this line. If a later reading replaces this block, restate the rule
//! with it rather than leaving the next reader to infer which rows counted. The comparison against the pre-Rack
//! dev-profile block above (9.6-87.2 ms) still holds turn-for-turn: every
//! verb here reads faster than the same verb there, indoor and outdoor
//! alike (`enter` 87.199 ms -> 47.377 ms, chamber `look` 66.578/67.454 ms ->
//! 29.163/29.450 ms, chamber `map`/`go *` 43.3-51.0 ms -> 18.7-20.0 ms) —
//! the win is real, it is just short of the P6 line under today's
//! contention.
//!
//! ### AFTER The Terrier, default (`dev`) profile — P6
//!
//! 2026-09-03, MacBookPro, `8b4f7f49065eb69852a9e2cded0d88e50b352a75`. **quiet**
//! (`uptime` before: `load averages: 2.67 2.18 2.11`, after: `2.48 2.15
//! 2.10` — all three under The Repose's quiet-box threshold of 4), taken on
//! the default profile a player launches. This file is INFORMATIVE and
//! gates nothing.
//!
//! ```text
//! game driver: profile DEBUG; Driver::start 3691 ms
//!     look     9.274 ms
//!      map    13.069 ms
//!     go n     9.766 ms
//!     go n     9.457 ms
//!     back     9.777 ms
//!     back    10.053 ms
//!    needs     9.733 ms
//!    enter     0.822 ms
//!     look     0.654 ms
//!      map     0.636 ms
//!     go n     0.610 ms
//!     go e     0.618 ms
//!     go s     0.611 ms
//!     go w     0.611 ms
//!     look     0.621 ms
//!      out     9.699 ms
//!     go n     9.809 ms
//!     back     9.421 ms
//! ```
//!
//! **P6 (spec §4 P3: every indoor movement turn ≤ 15 ms) MET on this
//! reading, for every indoor row:** `enter` 0.822 ms, the two post-`enter`
//! `look`s 0.654/0.621 ms, and `map`/`go n/e/s/w` inside the chamber
//! 0.610-0.636 ms — all MET, none within an order of magnitude of the
//! budget. Against the AFTER-Rack block above: `enter` 47.377 -> 0.822 ms,
//! chamber `look` 29.163/29.450 -> 0.654/0.621 ms, chamber `map`/`go *`
//! 18.7-20.0 -> 0.610-0.636 ms.
//!
//! **The outdoor range, stated once with its rule:** min and max over
//! EVERY outdoor row in the block above, `needs` included, rounded
//! half-up to two decimals — **9.27-13.07 ms** (`look` 9.274 -> 9.27,
//! `map` 13.069 -> 13.07). Against the AFTER-Rack block's 9.97-14.10 ms:
//! the control, unmoved beyond noise — this is the JSON-and-spatial-channel
//! floor The Rack measured, and The Terrier does not touch it.

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
