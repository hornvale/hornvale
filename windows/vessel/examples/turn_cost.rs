//! The session-level turn-cost bench (The Panes, Task 1).
//!
//! INFORMATIVE, NEVER A GATE — the gate is `cli/tests/session_cost.rs`. This
//! exists to settle two questions with numbers instead of extrapolation:
//! what one turn costs, and what `Session::snapshot()` adds to it.
//!
//! `CLIENT-four-clocks` records the 4.75 ms no-op turn floor as STALE (per-
//! tick behaviour landed with The Action Clock) and says the re-measurement
//! "wants a session-level benchmark nobody has built". This is it.
//!
//! Run: `cargo run --release -p hornvale-vessel --example turn_cost`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//! The Blocking measured its own spike ~10x slower in debug.
//!
//! ## Measured — baseline (before the spatial channel)
//!
//! Date: 2026-08-06. Box: `MacBookPro` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! Session::start   median 1451.026 ms
//! handle(verb)     median    1.071 ms
//! snapshot()+json  median    0.173 ms
//! snapshot bytes   walk 4235, chamber 4064
//! ```
//!
//! ## Measured — after the spatial channel (The Panes, Task 4)
//!
//! Date: 2026-08-06. Box: `MacBookPro` (`hostname -s`). Profile: `--release`.
//! Three runs, plus the per-verb-class split Task 4 added: a pooled median
//! across `SEQUENCE`'s ten heterogeneous verbs cannot show whether the
//! spec's §3.4 mitigation (memoizing chart construction on `(room, day,
//! zoom)`) would help, since that memo only pays off for verbs that move
//! neither the possession nor the day. Verbatim output:
//!
//! ```text
//! run 1:
//! Session::start   median 1317.037 ms
//! handle(verb)     median    1.010 ms
//! snapshot()+json  median    1.249 ms
//!   moving        n=10  handle median   11.641 ms   snapshot()+json median    1.197 ms
//!   day-advancing n=5   handle median    7.725 ms   snapshot()+json median    1.248 ms
//!   neither       n=35  handle median    0.190 ms   snapshot()+json median    1.306 ms
//! snapshot bytes   walk 11582, chamber 4759
//!
//! run 2:
//! Session::start   median  681.428 ms
//! handle(verb)     median    0.962 ms
//! snapshot()+json  median    1.103 ms
//!   moving        n=10  handle median   10.339 ms   snapshot()+json median    1.040 ms
//!   day-advancing n=5   handle median    5.603 ms   snapshot()+json median    1.168 ms
//!   neither       n=35  handle median    0.142 ms   snapshot()+json median    1.092 ms
//! snapshot bytes   walk 11582, chamber 4759
//!
//! run 3:
//! Session::start   median  705.885 ms
//! handle(verb)     median    0.969 ms
//! snapshot()+json  median    1.117 ms
//!   moving        n=10  handle median   10.497 ms   snapshot()+json median    1.046 ms
//!   day-advancing n=5   handle median    5.614 ms   snapshot()+json median    1.204 ms
//!   neither       n=35  handle median    0.148 ms   snapshot()+json median    1.145 ms
//! snapshot bytes   walk 11582, chamber 4759
//! ```
//!
//! **Verdict — the matched pair, slowest of three runs on each side:**
//! `snapshot()+json` moved from 0.173 ms (Task 1 baseline) to 1.249 ms — a
//! **7.22x** increase (1.249 / 0.173 = 7.220), +1.076 ms in absolute terms.
//! Snapshot bytes: walk 4235 -> 11582 (**2.73x**, +7347 bytes — matching
//! Task 3 review's independent estimate of "7,348 bytes per turn" for the
//! walk-band chart to within rounding); chamber 4064 -> 4759 (**1.17x**,
//! +695 bytes).
//!
//! The per-class split shows the increase is not uniform across verbs, but
//! not for the reason a reader might guess: `snapshot()+json` itself is
//! roughly *constant* across all three classes (~1.0-1.3 ms regardless of
//! which verb just ran) — the spatial channel is always rebuilt on every
//! `snapshot()` call, whichever band is current. What varies sharply by
//! class is `handle` itself: ~10-12 ms for `moving` verbs, ~6-8 ms for
//! `day-advancing`, ~0.14-0.19 ms for `neither`. That gap predates the
//! spatial channel — it is locale/day re-derivation cost inside `handle`,
//! not snapshot construction — so the spec's §3.4 memo, which targets
//! `snapshot()`'s chart construction, would not touch it; it would only
//! affect the now-roughly-flat `snapshot()+json` figure, and only when
//! consecutive turns share `(room, day, zoom)`.
//! `snapshot bytes` is deterministic on seed 42 (identical across all three
//! runs); only the timings vary run to run.
//!
//! ## Measured — through the wasm ABI (The Sighting, Task 1)
//!
//! Date: 2026-08-06. Box: `MacBookPro` (`hostname -s`). This discharges
//! `CLIENT-four-clocks`: every browser-side figure in this repo was this
//! native number multiplied by an assumed 3.6-3.8x ratio, never measured
//! through the ABI. It now is. Bench:
//! `clients/vessel/wasm/turn_bench.mjs`, driving `book/src/gallery/vessel.wasm`
//! (a fresh `make wasm-vessel` build) via `node`, `performance.now()`, 5 runs,
//! same `SEQUENCE`. Verbatim output:
//!
//! ```text
//! Session::start   median 2192.533 ms
//! hv_handle(verb)  median    3.740 ms
//! snapshot+decode  median    0.016 ms
//!   moving        n=10  handle median   15.032 ms   snapshot+decode median    0.020 ms
//!   day-advancing n=5   handle median    5.558 ms   snapshot+decode median    0.028 ms
//!   neither       n=35  handle median    2.556 ms   snapshot+decode median    0.014 ms
//! snapshot bytes   walk 12189, chamber 4752
//! ```
//!
//! **Architectural finding, found before any ratio was computed:** `hv_handle`
//! (`clients/vessel/wasm/src/lib.rs`) calls `set_snapshot()` internally on
//! every turn, so it already pays for `session.snapshot()` construction *and*
//! `snapshot_json()` serialization — the two things this file measures as
//! separate `handle(verb)` and `snapshot()+json` figures. `snapshot+decode`
//! above is therefore not the wasm analogue of this file's `snapshot()+json`:
//! it is only the cost of reading an *already-serialized* buffer out of
//! linear memory and UTF-8-decoding it in JS (0.014-0.028 ms — negligible).
//! The apples-to-apples comparison is `hv_handle` (bundled) against this
//! file's `handle(verb) + snapshot()+json` (also bundled, just measured as
//! two calls).
//!
//! **The real ratio, stated as a number — and it is not 3.6-3.8x:**
//! `hv_handle + snapshot-read` (wasm, bundled) ÷ `handle(verb) +
//! snapshot()+json` (native, run 3, the freshest of the three "after the
//! spatial channel" runs above): `3.756 / 2.086 = 1.80x`. Against run 1
//! (the slowest native run) the ratio is `3.756 / 2.259 = 1.66x`; against
//! the three-run average (`2.137`) it is `1.76x`. **Every apples-to-apples
//! per-turn ratio this bench found is in the 1.66-1.82x band — well under
//! half the assumed 3.6-3.8x.** That assumption traces most closely to
//! naively comparing wasm `hv_handle` against native `handle(verb)` alone
//! (ignoring that wasm bundles snapshot construction in): `3.740 / 0.980
//! (three-run average) = 3.82x` — a number essentially inside the assumed
//! band, but the wrong comparison, since it silently credits wasm's
//! `handle` figure with work the native `handle` figure never had to do.
//! **Every browser-side figure elsewhere in this repo built on the
//! 3.6-3.8x assumption should be re-examined**, not trusted at face value.
//!
//! Per-class ratios (same bundled-vs-bundled method, native run 3):
//! `moving` 15.052/11.543 = **1.30x**; `neither` 2.570/1.293 = **1.99x**;
//! `day-advancing` 5.586/6.818 = **0.82x** — wasm measured *faster* than
//! native for this class, which reads as implausible on its face and is
//! reported as such rather than smoothed away; it may reflect the small
//! sample (native n=5 per run vs wasm n=5 total) rather than a real effect.
//!
//! Genesis (`hv_start` vs `Session::start`) is the one figure where the
//! native side itself is too noisy (681-1317 ms across the three "after
//! spatial channel" runs, nearly 2x spread) to state a single confident
//! ratio: `2192.533 / 705.885 (run 3) = 3.11x`; `/ 681.428 (run 2) = 3.22x`;
//! `/ 1317.037 (run 1) = 1.66x`. Unlike the turn-cost ratio, genesis sits
//! closer to (if still generally under) the assumed band.
//!
//! **A confound, not a bug in this bench:** the wasm ABI's `hv_start`
//! (`clients/vessel/wasm/src/lib.rs`) hardcodes `PossessOpts { day:
//! WorldTime { day: 0.0 }, .. }`, while this file uses
//! `PossessOpts::default()`, which is noon (`day: 0.5`). The two paths are
//! not observing the same simulated moment, which plausibly explains part
//! of the walk-band snapshot byte gap above (wasm 12189 vs native's
//! recorded 11582, chamber much closer: 4752 vs 4759) — diurnal state
//! differs. Determinism is not violated (each side is internally
//! reproducible for its own fixed day), but a future re-measurement that
//! wants byte-identical snapshots between the two paths needs the same
//! `day` on both sides.

use hornvale_kernel::Seed;
use hornvale_vessel::{PossessOpts, Session};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// The fixed verb sequence every reading uses. Deliberately mixed: verbs
/// that move the possession (`enter`, `out`), verbs that advance the day
/// (`wait`), and verbs that do neither (`look`, `examine`) — because the
/// memo Task 4 may add only helps the third kind, and a sequence of only
/// `look` would flatter it.
const SEQUENCE: &[&str] = &[
    "look",
    "map",
    "examine me",
    "wait 1",
    "look",
    "enter",
    "map",
    "look",
    "out",
    "look",
];

/// How many times to run the sequence. Medians of repeated runs, per the
/// Rose Window metaplan §5's own measurement discipline.
const RUNS: usize = 5;

/// Which of the three effects a verb has on session state. Task 4 added this
/// split because the pooled median across `SEQUENCE`'s ten heterogeneous
/// verbs cannot show whether the spec's §3.4 mitigation (memoizing chart
/// construction on `(room, day, zoom)`) would help: that memo only pays off
/// for verbs that move neither the possession nor the day, and a slow
/// outlier verb in one class is invisible once averaged against the other
/// two.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum VerbClass {
    /// Advances the day: `wait N`. Ordered before `Moving` only so the
    /// `BTreeMap` iterates in a fixed, arbitrary-but-stable order — the
    /// order itself carries no meaning.
    DayAdvancing,
    /// Moves the possession: `enter`, `out`, `go`, `back`, `dive`, `surface`.
    Moving,
    /// Moves neither: `look`, `map`, `examine ...`.
    Neither,
}

impl VerbClass {
    /// Classify a verb line by its leading word.
    fn of(line: &str) -> Self {
        match line.split_whitespace().next().unwrap_or("") {
            "wait" => VerbClass::DayAdvancing,
            "enter" | "out" | "go" | "back" | "dive" | "surface" => VerbClass::Moving,
            _ => VerbClass::Neither,
        }
    }

    /// The label this class prints under.
    fn label(self) -> &'static str {
        match self {
            VerbClass::DayAdvancing => "day-advancing",
            VerbClass::Moving => "moving",
            VerbClass::Neither => "neither",
        }
    }
}

fn main() {
    // `#[allow]` because `clippy.toml` bans `Instant` workspace-wide
    // (decision 0001: time is `WorldTime`). A bench is the sanctioned
    // exception, the same one `cli/tests/scene_cost.rs` takes.
    #[allow(clippy::disallowed_types)] // benchmark harness
    use std::time::Instant;

    let world = build_world(
        Seed(42),
        &Default::default(),
        SkyChoice::Generated,
        &Default::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 builds");

    let mut starts = Vec::new();
    let mut turns = Vec::new();
    let mut snaps = Vec::new();
    let mut turns_by_class: std::collections::BTreeMap<VerbClass, Vec<f64>> =
        std::collections::BTreeMap::new();
    let mut snaps_by_class: std::collections::BTreeMap<VerbClass, Vec<f64>> =
        std::collections::BTreeMap::new();

    for _ in 0..RUNS {
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (mut session, _) =
            Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
        starts.push(t0.elapsed().as_secs_f64() * 1000.0);

        for line in SEQUENCE {
            let class = VerbClass::of(line);

            #[allow(clippy::disallowed_types)] // benchmark harness
            let t1 = Instant::now();
            let _ = session.handle(line);
            let turn_ms = t1.elapsed().as_secs_f64() * 1000.0;
            turns.push(turn_ms);
            turns_by_class.entry(class).or_default().push(turn_ms);

            #[allow(clippy::disallowed_types)] // benchmark harness
            let t2 = Instant::now();
            let snap = session.snapshot().expect("a live session snapshots");
            let snap_ms = t2.elapsed().as_secs_f64() * 1000.0;
            snaps.push(snap_ms);
            snaps_by_class.entry(class).or_default().push(snap_ms);

            // Serialize too: the emit is part of the per-turn cost the
            // client actually pays, and measuring construction alone would
            // under-report it.
            let json = hornvale_vessel::snapshot_json(&snap);
            std::hint::black_box(&json);
        }
    }

    println!("Session::start   median {:8.3} ms", median(&mut starts));
    println!("handle(verb)     median {:8.3} ms", median(&mut turns));
    println!("snapshot()+json  median {:8.3} ms", median(&mut snaps));

    // Split by verb class (Task 4): a pooled median can't show whether the
    // spec's §3.4 memo (keyed on `(room, day, zoom)`) would help, since that
    // memo only pays off for the `Neither` class.
    for class in [
        VerbClass::Moving,
        VerbClass::DayAdvancing,
        VerbClass::Neither,
    ] {
        let mut t = turns_by_class.remove(&class).unwrap_or_default();
        let mut s = snaps_by_class.remove(&class).unwrap_or_default();
        println!(
            "  {:<13} n={:<3} handle median {:8.3} ms   snapshot()+json median {:8.3} ms",
            class.label(),
            t.len(),
            median(&mut t),
            median(&mut s),
        );
    }

    // The byte figure the spec priced by radius. Printed per band so the
    // walk/chamber asymmetry is visible rather than averaged away.
    let (mut session, _) =
        Session::start(&world, &PossessOpts::default()).expect("seed 42 possesses");
    let walk = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    session.handle("enter");
    let chamber = hornvale_vessel::snapshot_json(&session.snapshot().unwrap()).len();
    println!("snapshot bytes   walk {walk}, chamber {chamber}");
}

/// The median of `xs`, which this sorts in place. `total_cmp` rather than
/// `partial_cmp().unwrap()`: the workspace sorts floats deterministically
/// and never panics on a NaN it did not expect.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(|a, b| a.total_cmp(b));
    xs[xs.len() / 2]
}
