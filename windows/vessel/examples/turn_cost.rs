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
//! same `SEQUENCE`. Verbatim output (re-run after the fix-round-1 byte-count
//! correction below; see "Fix round 1" for why the first posting's numbers
//! changed):
//!
//! ```text
//! Session::start   median 2157.978 ms
//! hv_handle(verb)  median    3.685 ms
//! snapshot+decode  median    0.015 ms
//!   moving        n=10  handle median   15.115 ms   snapshot+decode median    0.016 ms
//!   day-advancing n=5   handle median    5.482 ms   snapshot+decode median    0.026 ms
//!   neither       n=35  handle median    2.553 ms   snapshot+decode median    0.014 ms
//! snapshot bytes   walk 12204, chamber 4753
//! ```
//!
//! Paired against three **fresh** native runs of *this file* taken the same
//! session (not the older "after the spatial channel" runs recorded above —
//! see "Fix round 1" C2: a doc-comment figure from an earlier commit is not
//! a safe denominator once the code has moved):
//!
//! ```text
//! fresh run 1: handle(verb) 0.942 ms | snapshot() 1.154 ms
//!   moving 10.530/1.055   day-advancing 5.759/1.228   neither 0.149/1.154
//!   bytes: walk 12273, chamber 4802
//! fresh run 2: handle(verb) 0.951 ms | snapshot() 1.127 ms
//!   moving 10.501/1.063   day-advancing 5.790/1.253   neither 0.147/1.181
//!   bytes: walk 12273, chamber 4802
//! fresh run 3: handle(verb) 1.087 ms | snapshot() 1.267 ms
//!   moving 12.231/1.259   day-advancing 6.729/1.308   neither 0.167/1.267
//!   bytes: walk 12273, chamber 4802
//! ```
//!
//! **Architectural finding, found before any ratio was computed:** `hv_handle`
//! (`clients/vessel/wasm/src/lib.rs`) calls `set_snapshot()` internally on
//! every turn, so it already pays for `session.snapshot()` construction *and*
//! `snapshot_json()` serialization — the two things this file measures as
//! separate `handle(verb)` and `snapshot()` figures. `snapshot+decode` above
//! is therefore not the wasm analogue of this file's `snapshot()`: it is
//! only the cost of reading an *already-serialized* buffer out of linear
//! memory and UTF-8-decoding it in JS (0.014-0.026 ms — negligible). The
//! apples-to-apples comparison is `hv_handle` (bundled) against this file's
//! `handle(verb) + snapshot()` (also bundled work, just measured as two
//! calls). **A second, pre-existing mislabeling** (from The Panes, not
//! introduced here): the `snapshot()` timer stops at this function's
//! `t2.elapsed()` (line 271) *before* `snapshot_json(&snap)` runs (line 278,
//! unclocked) — so despite the `snapshot()+json` label used elsewhere in
//! this file, the native side never times JSON serialization at all. Every
//! ratio below therefore divides by a denominator that is missing real
//! work, which means **every ratio here is an upper bound** on the true
//! bundled-vs-bundled comparison — if anything, wasm's true relative
//! penalty is smaller than what is stated.
//!
//! **The real ratio, stated as a number — and it is not 3.6-3.8x:**
//! `hv_handle + snapshot-read` (wasm, bundled, `3.685 + 0.015 = 3.700`) ÷
//! `handle(verb) + snapshot()` (native, bundled, fresh runs above):
//! run 1 `0.942 + 1.154 = 2.096`, run 2 `0.951 + 1.127 = 2.078`, run 3
//! `1.087 + 1.267 = 2.354`. That gives `3.700 / 2.354 = 1.57x` (slowest
//! native run) to `3.700 / 2.078 = 1.78x` (fastest) — **band 1.57-1.78x**,
//! independently re-confirmed by review at 1.77x pooled. **Every
//! apples-to-apples per-turn ratio this bench found is in the 1.57-1.78x
//! band — well under half the assumed 3.6-3.8x**, and that gap would only
//! widen if the native denominator's missing JSON-serialization time were
//! added in. That assumption traces most closely to naively comparing wasm
//! `hv_handle` against native `handle(verb)` alone (ignoring that wasm
//! bundles snapshot construction in): `3.685 / 0.980` (fresh three-run
//! average `handle(verb)` alone) `= 3.76x` — a number essentially inside the
//! assumed band, but the wrong comparison, since it silently credits wasm's
//! `handle` figure with work the native `handle` figure never had to do.
//!
//! **Where 3.6-3.8x actually came from, and why it isn't wrong so much as
//! outdated:** the number is documented — Rose Window metaplan §5
//! (`docs/superpowers/specs/2026-07-25-the-rose-window-metaplan-design.md`),
//! committed 2026-07-25 10:46, states "§5's own native→wasm ratios (`look`
//! 0.12→0.46 ms, movement 0.32→1.15 ms) give ~3.6-3.8x", measured over the
//! real Casement ABI. At that moment it *was* a clean `handle`-vs-`handle`
//! comparison: `set_snapshot()` was not added inside `hv_handle` until
//! commit `3d0369c2`, the same day at 12:22 — about 96 minutes later. So the
//! 3.6-3.8x figure was valid for the **pre-snapshot handle clock**, and
//! today's `3.685 / 0.980 = 3.76x` landing back inside that band is
//! arithmetic coincidence (comparing the same two things the old number
//! compared), not a live confirmation of it. **The more interesting result
//! is the bundled one**: once `hv_handle` started paying for snapshot
//! construction and serialization too, the wasm penalty for *that* added
//! work turned out to be far smaller than the verb-handling penalty — small
//! enough to pull the bundled per-turn ratio down to 1.57-1.78x even though
//! the handle-alone ratio stayed near 3.6-3.8x. **Every browser-side figure
//! elsewhere in this repo built on the 3.6-3.8x assumption should be
//! re-examined for whether it means the handle-alone or the bundled cost**
//! — this task only re-measured the turn/genesis/snapshot clocks, not every
//! downstream figure derived from them.
//!
//! **The pooled figures above are not "the" turn cost.** With 35 of 50
//! pooled samples in `neither`, the pooled median (`xs[len/2]`) lands on a
//! single individual turn, not a representative one: the fresh runs' pooled
//! `handle(verb)` medians (0.942-1.087 ms) sit roughly 6-7x above the
//! `neither`-class-only medians computed from those same 35 samples
//! (0.147-0.167 ms) — confirming this is a specific sorted-midpoint sample
//! (review traced it to a `map` turn), not "the" neither-class cost. Both
//! sides use the same pooling rule and sequence, so the pooled *ratio*
//! above is self-consistent, but Task 6 should carry the **per-class**
//! numbers forward rather than the pooled ones.
//!
//! Per-class ratios (same bundled-vs-bundled method, wasm vs the three
//! fresh native runs, worst-to-best case shown as a band): `moving`
//! `(15.115+0.016)/[13.490 .. 11.564] = 1.12-1.31x`; `neither`
//! `(2.553+0.014)/[1.434 .. 1.303] = 1.79-1.97x`; `day-advancing`
//! `(5.482+0.026)/[8.037 .. 6.987] = 0.69-0.79x` — wasm measured *faster*
//! than native for this class in every pairing, across every re-run this
//! task did (0.69-0.82x band across three independent measurement passes,
//! including one by review). That rules out "small-sample noise" as the
//! explanation (fix round 1 C4/I4 below): it is systematic, and has a named
//! cause.
//!
//! ### Genesis: fixed for fix round 1 (was Critical defect C1)
//!
//! The first posting of this section computed the genesis ratio as
//! `hv_start` (wasm) ÷ `Session::start` (native, this file, lines 253-256) —
//! but `Session::start` alone **excludes** `build_world`, which this file
//! hoists out of the timed loop (line 234, before `for _ in 0..RUNS`),
//! while `hv_start` (`clients/vessel/wasm/src/lib.rs` line 79-119) calls
//! `build_world` **inside** every timed call (line 81-93, before
//! `Session::start` at line 103). Dividing wasm's inclusive figure by
//! native's exclusive one is not a ratio of comparable things, and the
//! error compounded: the first posting's "genesis sits closer to the
//! assumed band" reading was backwards — an inclusive-vs-inclusive
//! comparison moves genesis *further* below the assumed band, not closer.
//!
//! Corrected, timing the same work on both sides needs a native path that
//! also does both steps in one measurement: `hornvale possess --seed 42`
//! (`cli/src/main.rs` `cmd_possess`) builds the world and starts the session
//! together, matching `hv_start` exactly. Measured directly (wall-clock,
//! `echo release | ./target/release/hornvale possess --seed 42`, piped so
//! the process exits immediately after genesis and one turn):
//!
//! ```text
//! native `hornvale possess --seed 42`, pooled n=30 across this session: median 1.49 s
//! wasm   `hv_start(42)` alone, pooled n=17 raw samples across this session: median 2.20 s
//! ratio: 2.20 / 1.49 = 1.48x
//! ```
//!
//! This box ran under sustained, fluctuating contention while this was
//! measured (`uptime` load averages swung 7-27 across the session on 10
//! cores — see the root `CLAUDE.md` note on Mac contention), which is why
//! individual paired batches ranged as wide as 1.3-2.0x, and one acutely
//! contended pairing (load 27) was discarded rather than averaged in. A
//! second, independent measurement by review reported native 2.29-2.42 s vs
//! wasm 2.19-2.22 s ("roughly parity", ratio ~0.90-0.97x) — noticeably
//! closer to parity than this task's own pooled 1.48x. Both readings agree
//! on the load-bearing correction, though: **genesis is nowhere near the
//! old flawed 3.11x, and (fixed) sits further below the assumed 3.6-3.8x
//! band than the turn-cost ratio does, not closer to it** as the first
//! posting claimed backwards. The exact point estimate (1.48x here,
//! ~0.9-1.0x per review) should be read as noisy on this box rather than
//! as a settled number; a future re-measurement on a quiet box would settle
//! it.
//!
//! ### Snapshot bytes: fixed for fix round 1 (was Critical defect C2, plus I1)
//!
//! Two compounding errors in the first posting, both now fixed:
//!
//! 1. **The native comparison figure was stale.** The first posting
//!    compared against `walk 11582, chamber 4759` — the "after the spatial
//!    channel" figures recorded earlier above, on an earlier commit. This
//!    branch's `d36a6a79` (the v2 chart, adding `height_asl_m`/
//!    `sea_level_m`) superseded those same-day, before this task's wasm
//!    build. A fresh run of this file's own `walk`/`chamber` print (see
//!    "fresh run" blocks above) gives `walk 12273, chamber 4802` on every
//!    one of three fresh runs, matching the committed fixtures
//!    `windows/vessel/tests/fixtures/snapshot-seed-42-{walk,chamber}.json`
//!    (12273 / 4802 bytes) exactly. The lesson generalizes past this one
//!    number: compare against a fresh run or a committed fixture, never
//!    against a figure recorded in a doc comment on an earlier commit.
//! 2. **The wasm-side count itself used the wrong unit** (fix round 1 I1):
//!    `clients/vessel/wasm/turn_bench.mjs` originally reported
//!    `readSnapshotJson().length` — a JS string's UTF-16 *code-unit* count —
//!    under a label that says "bytes" and is compared against one. A
//!    handful of non-ASCII characters in the narration made that an
//!    undercount. The bench now reads `hv_snapshot_len()` directly (already
//!    destructured from the module's exports), which is the true byte
//!    count: `walk 12204, chamber 4753` (verbatim block above).
//!
//! **The real deltas: wasm walk is 69 bytes *smaller* than native (12204 vs
//! 12273), and chamber is 49 bytes smaller (4753 vs 4802) — wasm
//! consistently slightly smaller in both bands**, not the first posting's
//! fabricated "+607 on walk, -7 on chamber" (which does not survive either
//! correction: neither the stale native figure nor the code-unit undercount
//! it was computed from was real). The day-0.0-vs-day-0.5 confound
//! described next is real, but it is not the explanation for a byte gap
//! this small and in this direction — it turned out to matter for the
//! `day-advancing` timing anomaly instead.
//!
//! ### The `day-advancing` anomaly: reframed for fix round 1 (was I4)
//!
//! The first posting attributed wasm's faster-than-native `day-advancing`
//! reading to small-sample noise. It reproduced instead — 0.69-0.82x across
//! three independent measurement passes on this task alone, plus review's
//! own re-run — so it is systematic, not noise, and it has a named cause:
//! `hv_start` (`clients/vessel/wasm/src/lib.rs`) hardcodes `PossessOpts {
//! day: WorldTime { day: 0.0 }, .. }` (midnight), while this file uses
//! `PossessOpts::default()` (`windows/vessel/src/lib.rs` lines 85-97), which
//! is noon (`day: 0.5`) *specifically* so a single `wait 1` lands at the
//! next noon too — still inside the diurnal active band — rather than on
//! the midnight boundary every integer day crossing from `day: 0.0` hits.
//! Landing on that boundary is genuinely different, and more, work. Review
//! measured it directly: 100 sequential `wait 1` turns cost ~154 ms/day from
//! `day: 0.0` vs ~96 ms/day from `day: 0.5` (n=3 each) — the native
//! comparison figure in this file pays the *cheaper* midnight-avoiding path
//! by construction, while wasm's hardcoded `day: 0.0` pays the boundary
//! every time, which cuts the other way from what a first guess would
//! expect and is consistent with wasm still coming out faster here: the
//! *op* wasm is doing is not the same *op* the native side is doing. This
//! does not threaten the pooled turn-cost headline (`day-advancing` is a
//! small slice of `SEQUENCE`), but it does mean this class's ratio should
//! not be read as a measurement of wasm-vs-native performance at all until
//! both sides share a `day`.
//!
//! **A confound, not a bug in this bench:** changing `hv_start`'s hardcoded
//! day is a Casement behavior change, out of scope for a bench task, and is
//! left as a finding rather than fixed here. A future re-measurement that
//! wants a clean `day-advancing` comparison, or byte-identical snapshots
//! between the two paths, needs the same `day` on both sides.

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
