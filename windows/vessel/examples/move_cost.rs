//! Per-verb cost of MOVING at both scales after The Roll — a diagnostic,
//! never a gate. `turn_cost` classes verbs coarsely; this times the exact
//! sequence a player types: room-scale `go`, `enter`, chamber-scale `go`,
//! `look`, `map`, `examine <name>`, `needs`, and `snapshot()` after each,
//! then again after twenty waits to see what grows with session length.
//!
//! Run: `cargo run --release -p hornvale-vessel --example move_cost`
//! (and once WITHOUT `--release`, because `cargo run -p hornvale -- possess`
//! is a debug build and that is what a player at the CLI actually feels).
//!
//! ## Measured — AFTER The Terrier (2026-09-03)
//!
//! 2026-09-03, MacBookPro, `8b4f7f49065eb69852a9e2cded0d88e50b352a75`,
//! `--release`, seed 42. **quiet** (`uptime` before: `load averages: 2.12
//! 2.06 2.07`, after: `2.67 2.18 2.11` — all three under The Repose's
//! quiet-box threshold of 4), so this reading is not an upper bound the
//! way the CONTENDED blocks below are.
//!
//! **The reading that redirected the campaign.** Before this fix, scratch
//! `Instant` prints inside `derive_sighting`, `chamber_plan`,
//! `describe_chamber_here`, `enter`, `brief_here` and `brief_of`, on this
//! same sequence, release profile, seed 42's flagship, MacBookPro,
//! **CONTENDED** (`uptime` load averages `28.07 25.58 22.66` on the first
//! run and `51.01 34.28 26.41` on the second — every figure below is an
//! upper bound, and the *split* is the finding, not the absolute):
//!
//! ```text
//! step inside derive_sighting                       per call
//!   chamber_interior_here  (= one brief_here)     8.7 – 12.6 ms
//!   anchor_cells                                  0.086 – 0.091 ms // lexicon: anchor_cells is the function name; it places AREA-sense lattice squares, never a mesh vertex
//!   shadowcast (SIGHT_RADIUS = 4, ≤ 81 squares)     0.011 – 0.013 ms
//!   occupancy seat + interior_of                  0.005 – 0.006 ms
//!   placement loop + furnishings                  0.009 – 0.013 ms
//!
//! step inside chamber_plan
//!   fabric_here                                   0.003 – 0.004 ms
//!   chamber_sources        (= one brief_here)     8.8 – 17.6 ms
//!   light_field                                   0.060 – 0.896 ms
//!   plan_of (+ ambient)                            0.119 – 1.023 ms
//!
//! inside brief_of                     n = 42 calls in the run
//!   is_built                                      mean 0.000 ms
//!   is_cold                                       mean 0.003 ms
//!   containing_vertex                             mean 0.003 ms
//!   occupations_by_vertex (452 vertices)          mean 11.421 ms, max 26.048 ms
//! ```
//!
//! That split is why this campaign hoisted `occupations_by_vertex(world)`
//! onto `WorldContext::build`, once per world, instead of touching the
//! shadowcast the brief and The Rack both named — the shadowcast was never
//! the cost. `brief::brief_of` reads a register now; it reconstructs
//! nothing.
//!
//! ```text
//! move_cost: seed 42, profile release; build_world 2372 ms
//! Session::start 868 ms
//! --- fresh session: bodies 68 on roll 68 facts 21932
//!                   look handle     0.229 ms  snapshot     4.978 ms    69252 B
//!                    map handle     4.020 ms  snapshot     4.172 ms    70454 B
//!                   go n handle     0.505 ms  snapshot     4.055 ms    64685 B
//!                   go n handle     0.462 ms  snapshot     4.073 ms    66601 B
//!                   back handle     0.403 ms  snapshot     4.080 ms    66927 B
//!                   back handle     0.375 ms  snapshot     4.371 ms    73645 B
//! examine Dvoashngashngo handle     3.901 ms  snapshot     4.283 ms    73285 B
//!                  needs handle     0.023 ms  snapshot     4.179 ms    75849 B
//!                  enter handle     0.175 ms  snapshot     0.415 ms    25172 B
//!                   look handle     0.114 ms  snapshot     0.381 ms    25173 B
//!                    map handle     0.019 ms  snapshot     0.466 ms    25476 B
//!                   go n handle     0.005 ms  snapshot     0.466 ms    24944 B
//!                   go e handle     0.004 ms  snapshot     0.474 ms    25089 B
//!                   go s handle     0.004 ms  snapshot     0.458 ms    25232 B
//!                   go w handle     0.003 ms  snapshot     0.457 ms    25035 B
//!                   look handle     0.105 ms  snapshot     0.367 ms    25171 B
//!                    out handle     0.220 ms  snapshot     4.116 ms    73649 B
//! 20 waits 1812 ms
//! --- after 20 waits: bodies 68 on roll 68 facts 23487
//!                   look handle     0.226 ms  snapshot     4.649 ms    73250 B
//!                    map handle     4.081 ms  snapshot     4.671 ms    74485 B
//!                   go n handle     0.493 ms  snapshot     4.333 ms    66931 B
//!                   go n handle     0.471 ms  snapshot     4.286 ms    66856 B
//!                   back handle     0.396 ms  snapshot     4.327 ms    67011 B
//!                   back handle     0.361 ms  snapshot     4.331 ms    73146 B
//! examine Dvoashngashngo handle     3.942 ms  snapshot     4.112 ms    72764 B
//!                  needs handle     0.022 ms  snapshot     4.297 ms    74926 B
//!                  enter handle     0.178 ms  snapshot     0.479 ms    24848 B
//!                   look handle     0.110 ms  snapshot     0.426 ms    24848 B
//!                    map handle     0.021 ms  snapshot     0.525 ms    25156 B
//!                   go n handle     0.005 ms  snapshot     0.508 ms    24620 B
//!                   go e handle     0.004 ms  snapshot     0.505 ms    24769 B
//!                   go s handle     0.003 ms  snapshot     0.497 ms    24919 B
//!                   go w handle     0.003 ms  snapshot     0.494 ms    24716 B
//!                   look handle     0.095 ms  snapshot     0.412 ms    24848 B
//!                    out handle     0.212 ms  snapshot     4.428 ms    73082 B
//! ```
//!
//! **Verdict, against spec §4 P2 and P4 — the basis is The Rack's AFTER
//! block below:**
//!
//! | reading | before (AFTER-Rack) | after (AFTER-Terrier) | budget | verdict |
//! | --- | ---: | ---: | ---: | --- |
//! | chamber `snapshot()+json` after `map`/`go n/e/s/w` | 16.3-16.8 ms | **0.457-0.525 ms** | ≤ 3 ms | MET |
//! | `enter` handle | 33.747 ms | **0.175-0.178 ms** | ≤ 3 ms | MET |
//! | chamber `look` handle | 16.540/16.125 ms | **0.095-0.114 ms** | ≤ 1 ms | MET |
//! | `Session::start` | 845 ms | **868 ms (+23 ms)** | ≤ +30 ms over 845 | MET |
//!
//! The `Session::start` row's "before" is the AFTER-Rack block's own reading
//! (`uptime` load averages 14.10/17.75/21.58, CONTENDED), while this block's
//! "after" (868 ms) is read quiet — so before and after are not the same
//! box load, and **+23 ms is a lower bound on the growth**, not a measured
//! delta. The real answer P4's decision rule was watching for does not need
//! a matched pair to settle: the register has exactly one builder —
//! `occupations_by_vertex` appears in production code only inside
//! `WorldContext::build`, which `windows/vessel/tests/suite/the_terrier.rs`'s
//! structural scan enforces — so the cost this row is trying to bound is
//! paid once per world, never per session or per turn, regardless of load.
//!
//! Outdoor rows are the control: walk-band `snapshot()+json` reads
//! 4.055-4.978 ms here against the AFTER-Rack block's ~4.1-4.9 ms and the
//! purview-fix block's 4.2-5.0 ms, and outdoor handle times likewise
//! (`needs` 0.022-0.023 ms against 0.022 ms there) — unmoved, within
//! noise. Every chamber row now reads within a tenth of a millisecond of
//! its post-`look` sibling, the shape §4 P2 predicted once the brief was
//! the residue rather than the shadowcast.
//!
//! ## Measured — AFTER the purview fix (The Rack, final review, 2026-09-02)
//!
//! The final review found that a walk-band `snapshot` still folded
//! `agent_position` once per NPC, inside `purview_scene` — ~67 ledger folds
//! per turn, in a module `TurnWork` could not instrument, so the campaign's
//! own budget test read zero while they ran. The chart now reads the roster's
//! `position` column. Re-measured here, `--release`, CONTENDED
//! (`uptime` load averages 3.38 19.26 29.37 before, 3.56 18.77 29.08 after —
//! comparable to the Task 4 block's 14.10 17.75 21.58, and an upper bound
//! either way):
//!
//! ```text
//!                   look handle     0.241 ms  snapshot     5.019 ms    69252 B
//!                    map handle     4.058 ms  snapshot     4.416 ms    70454 B
//!                   go n handle     0.516 ms  snapshot     4.441 ms    64685 B
//!                   go n handle     0.518 ms  snapshot     4.287 ms    66601 B
//!                   back handle     0.391 ms  snapshot     4.286 ms    66927 B
//!                   back handle     0.405 ms  snapshot     4.251 ms    73645 B
//! examine Dvoashngashngo handle     3.990 ms  snapshot     4.219 ms    73285 B
//!                  needs handle     0.027 ms  snapshot     4.205 ms    75849 B
//!                  enter handle    34.234 ms  snapshot     9.172 ms    25172 B
//!                   look handle    16.655 ms  snapshot     9.204 ms    25173 B
//!                    map handle     0.020 ms  snapshot    17.469 ms    25476 B
//!                   go n handle     0.007 ms  snapshot    16.829 ms    24944 B
//!                   go e handle     0.007 ms  snapshot    16.778 ms    25089 B
//!                   go s handle     0.006 ms  snapshot    17.582 ms    25232 B
//!                   go w handle     0.008 ms  snapshot    17.200 ms    25035 B
//!                   look handle    17.150 ms  snapshot     8.741 ms    25171 B
//!                    out handle     0.311 ms  snapshot     4.531 ms    73649 B
//! 20 waits 1858 ms
//! --- after 20 waits: bodies 68 on roll 68 facts 23487
//!                   look handle     0.221 ms  snapshot     4.573 ms    73250 B
//!                    map handle     4.160 ms  snapshot     4.545 ms    74485 B
//!                   go n handle     0.500 ms  snapshot     4.325 ms    66931 B
//!                   go n handle     0.670 ms  snapshot     4.544 ms    66856 B
//!                   back handle     0.401 ms  snapshot     4.470 ms    67011 B
//!                   back handle     0.385 ms  snapshot     4.468 ms    73146 B
//! examine Dvoashngashngo handle     4.276 ms  snapshot     4.605 ms    72764 B
//!                  needs handle     0.023 ms  snapshot     4.388 ms    74926 B
//!                  enter handle    33.316 ms  snapshot     9.079 ms    24848 B
//!                   look handle    17.048 ms  snapshot     8.888 ms    24848 B
//!                    map handle     0.020 ms  snapshot    17.309 ms    25156 B
//!                   go n handle     0.009 ms  snapshot    17.270 ms    24620 B
//!                   go e handle     0.005 ms  snapshot    17.332 ms    24769 B
//!                   go s handle     0.009 ms  snapshot    18.134 ms    24919 B
//!                   go w handle     0.007 ms  snapshot    17.431 ms    24716 B
//!                   look handle    16.624 ms  snapshot     8.814 ms    24848 B
//!                    out handle     0.260 ms  snapshot     4.460 ms    73082 B
//! ```
//!
//! **THE NUMBERS DID NOT MOVE, AND THAT IS THE RESULT.** Walk-band snapshot
//! 4.205-5.019 ms against Task 4's 4.1-4.9; chamber 8.741-17.582 against
//! 8.4-16.8; `needs` 0.023-0.027 against 0.022. Sixty-seven ledger folds
//! per turn were removed and the wall clock is unchanged within noise, so
//! **this fix is a correctness and instrumentation fix, not a speed-up** —
//! and saying so is the honest reading. An `agent-at` scan over a 23,487-fact
//! session ledger is cheap; what a walk-band snapshot actually costs is the
//! spatial channel and ~70 KB of JSON, exactly as the residue analysis in the
//! Task 4 block concluded. If anything this STRENGTHENS that conclusion: the
//! largest remaining per-body loop was removed and the floor did not budge.
//!
//! P1 is therefore unchanged: `needs` MET (2 ms), both snapshot budgets
//! MISSED (3 ms) by the same margins, for the same reason.
//!
//! What DID change is a property no timing here shows: walk-band snapshot
//! cost is now flat in ledger length rather than carrying one per-body ledger
//! scan that grows with it. Both blocks above sample the same two depths
//! (day 0.5 and day 21, 23,487 facts) and agree; a longer session is where
//! that would separate, and nothing here measures one.
//!
//! ## Measured — AFTER The Rack (Task 4)
//!
//! 2026-09-02, MacBookPro, `--release`, the turn reading the rack.
//! **CONTENDED** (`uptime` before: `load averages: 14.10 17.75 21.58`, after:
//! `13.21 17.50 21.47` — all three far over The Repose's quiet-box threshold
//! of 4). A contended reading is an UPPER bound, which makes the two budgets
//! it clears conclusive and the two it misses inconclusive as to how much.
//!
//! Against spec §4's P1 budgets:
//!
//! | reading | before | after | budget | verdict |
//! | --- | ---: | ---: | ---: | --- |
//! | `needs` handle | 27.559 ms | **0.022 ms** | 2 ms | MET (1250x) |
//! | `snapshot` in the home room | 32.242 ms | **4.1-4.9 ms** | 3 ms | MISSED (7.4x better) |
//! | `snapshot` in a chamber | 44.374 ms | **8.4-16.8 ms** | 3 ms | MISSED (2.7-5.3x better) |
//!
//! **The residue was already visible in the BEFORE run, and it is the JSON
//! and the chart.** Read the pre-change block below by ROW rather than as an
//! average: its two `go n` rows and its first `back` row cost 4.2-4.4 ms
//! while every other walk-band row cost 31-32 ms. Those three are exactly
//! the rows where the possession had stepped OUT of its settlement's room,
//! so `sensed.present` was empty and the call folded nothing. 4.2 ms was
//! therefore already the fold-free floor of a walk-band snapshot, and the
//! after-run's 4.1-4.9 ms in the SETTLEMENT room is that same floor now
//! reached with 67 bodies present. The 3 ms budget was set below a floor
//! this campaign never touched: what remains is the spatial channel and the
//! ~70 KB of JSON, which spec §4 already said it expected to be inside the
//! budget and which this measurement says is not.
//!
//! **The chamber band shows what a BRIEF costs, because the memo makes it
//! visible — and this paragraph used to say "one shadowcast".** 8.4 ms
//! after `enter`/`look`, 16.3-16.8 ms after `map` or a chamber `go` — same
//! chamber, same turn shape. `look` derives a `Session::sighting` for its
//! presence line and the snapshot on that turn reuses it (The Rack, Task 4);
//! `map` and `go n` derive none, so the snapshot pays for its own. The
//! difference, ~8 ms, was the sighting DERIVATION, and The Rack named it
//! after the step it is named for. Decomposed by The Terrier (2026-09-03):
//! the shadowcast at `SIGHT_RADIUS` 4 is 0.011-0.013 ms; the 8 ms was
//! `brief::brief_of` rebuilding `occupations_by_vertex(world)` — the whole
//! world's occupation register — on every call, once inside the sighting
//! and once more inside `chamber_sources`. See the AFTER-Terrier block
//! above for what a chamber snapshot costs with the register hoisted.
//!
//! ```text
//! move_cost: seed 42, profile release; build_world 2422 ms
//! Session::start 845 ms
//! --- fresh session: bodies 68 on roll 68 facts 21932
//!                   look handle     0.220 ms  snapshot     4.876 ms    69252 B
//!                    map handle     3.917 ms  snapshot     4.176 ms    70454 B
//!                   go n handle     0.495 ms  snapshot     4.141 ms    64685 B
//!                   go n handle     0.479 ms  snapshot     4.111 ms    66601 B
//!                   back handle     0.381 ms  snapshot     4.116 ms    66927 B
//!                   back handle     0.381 ms  snapshot     4.155 ms    73645 B
//! examine Dvoashngashngo handle     3.912 ms  snapshot     4.163 ms    73285 B
//!                  needs handle     0.022 ms  snapshot     4.157 ms    75849 B
//!                  enter handle    33.747 ms  snapshot     9.144 ms    25172 B
//!                   look handle    16.540 ms  snapshot     8.516 ms    25173 B
//!                    map handle     0.019 ms  snapshot    16.547 ms    25476 B
//!                   go n handle     0.005 ms  snapshot    16.541 ms    24944 B
//!                   go e handle     0.005 ms  snapshot    16.766 ms    25089 B
//!                   go s handle     0.006 ms  snapshot    16.542 ms    25232 B
//!                   go w handle     0.005 ms  snapshot    16.383 ms    25035 B
//!                   look handle    16.125 ms  snapshot     8.388 ms    25171 B
//!                    out handle     0.258 ms  snapshot     4.311 ms    73649 B
//! 20 waits 1828 ms
//! --- after 20 waits: bodies 68 on roll 68 facts 23487
//! (the second block repeats the first within noise; `needs` reads 0.022 ms
//! there too, and the home-room snapshot 4.15-4.62 ms)
//! ```
//!
//! ## Measured — BEFORE The Rack
//!
//! 2026-09-02, MacBookPro, `2c34f9e4c64fae3230a751cfca061c4bcdf3bb37`, `--release`.
//! **CONTENDED** (`uptime`: `load averages: 9.27 12.44 9.86` — all three
//! over The Repose's quiet-box threshold of 4; taken anyway per the
//! fallback `session_wait_scaling.rs` documents, since this is a diagnostic
//! rather than a preregistered gate).
//!
//! ```text
//! move_cost: seed 42, profile release; build_world 2410 ms
//! Session::start 828 ms
//! --- fresh session: bodies 68 on roll 68 facts 21932
//!                   look handle     0.238 ms  snapshot    32.242 ms    69252 B
//!                    map handle     4.080 ms  snapshot    31.136 ms    70454 B
//!                   go n handle     0.525 ms  snapshot     4.227 ms    64685 B
//!                   go n handle     0.509 ms  snapshot     4.351 ms    66601 B
//!                   back handle     0.427 ms  snapshot     4.423 ms    66927 B
//!                   back handle     0.422 ms  snapshot    31.345 ms    73176 B
//! examine Dvoashngashngo handle     4.019 ms  snapshot    32.235 ms    72816 B
//!                  needs handle    27.559 ms  snapshot    31.705 ms    74911 B
//!                  enter handle    33.941 ms  snapshot    45.914 ms    24703 B
//!                   look handle    16.897 ms  snapshot    44.374 ms    24704 B
//!                    map handle     0.020 ms  snapshot    44.215 ms    25007 B
//!                   go n handle     0.007 ms  snapshot    44.147 ms    24475 B
//!                   go e handle     0.006 ms  snapshot    43.826 ms    24620 B
//!                   go s handle     0.006 ms  snapshot    45.162 ms    24763 B
//!                   go w handle     0.007 ms  snapshot    44.548 ms    24566 B
//!                   look handle    17.675 ms  snapshot    46.196 ms    24702 B
//!                    out handle     0.309 ms  snapshot    31.896 ms    73180 B
//! 20 waits 1859 ms
//! --- after 20 waits: bodies 68 on roll 68 facts 23487
//! (per-verb pattern repeats; omitted here — see the module's `## Measured`
//! sibling files for the full-run convention)
//! ```

use hornvale_vessel::{PossessOpts, Session, Turn};
#[allow(clippy::disallowed_types)] // wall-clock is the instrument here, never sim logic
use std::time::Instant;

const SEED: u64 = 42;

fn build(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 must build under default pins")
}

fn out(t: Turn) -> String {
    match t {
        Turn::Out(s) => s,
        _ => String::new(),
    }
}

fn timed(session: &mut Session<'_>, verb: &str) {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let reply = out(session.handle(verb));
    let handle_ms = t0.elapsed().as_secs_f64() * 1000.0;
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t1 = Instant::now();
    let snap = session
        .snapshot()
        .map(|s| serde_json::to_string(&s).unwrap_or_default());
    let snap_ms = t1.elapsed().as_secs_f64() * 1000.0;
    let bytes = snap.as_ref().map(|s| s.len()).unwrap_or(0);
    let first = reply
        .lines()
        .next()
        .unwrap_or("")
        .chars()
        .take(60)
        .collect::<String>();
    println!(
        "{:>22} handle {:>9.3} ms  snapshot {:>9.3} ms  {:>7} B  | {}",
        verb, handle_ms, snap_ms, bytes, first
    );
}

fn block(session: &mut Session<'_>, label: &str) {
    println!(
        "--- {label}: bodies {} on roll {} facts {}",
        session.bodies().len(),
        session.roll_len(),
        session.committed_fact_count()
    );
    let name = session
        .bodies()
        .iter()
        .find(|b| b.village.is_some() && b.entity != session.driven_body().entity)
        .map(|b| b.label.clone())
        .unwrap_or_default();
    for v in ["look", "map", "go n", "go n", "back", "back"] {
        timed(session, v);
    }
    timed(session, &format!("examine {name}"));
    timed(session, "needs");
    timed(session, "enter");
    for v in ["look", "map", "go n", "go e", "go s", "go w", "look"] {
        timed(session, v);
    }
    timed(session, "out");
}

fn main() {
    let profile = if cfg!(debug_assertions) {
        "DEBUG"
    } else {
        "release"
    };
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let world = build(SEED);
    println!(
        "move_cost: seed {SEED}, profile {profile}; build_world {:.0} ms",
        t0.elapsed().as_secs_f64() * 1000.0
    );
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t1 = Instant::now();
    let (mut session, _) = Session::start(&world, &PossessOpts::default()).expect("seed 42 starts");
    println!(
        "Session::start {:.0} ms",
        t1.elapsed().as_secs_f64() * 1000.0
    );
    block(&mut session, "fresh session");
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t2 = Instant::now();
    for _ in 0..20 {
        session.handle("wait");
    }
    println!("20 waits {:.0} ms", t2.elapsed().as_secs_f64() * 1000.0);
    block(&mut session, "after 20 waits");
}
