//! The Penstock stage 1, Task 3: how each ledger read scales with ledger
//! size.
//!
//! INFORMATIVE, NEVER A GATE — the same standing as
//! `windows/vessel/examples/turn_cost.rs`. Its job is to make an accidental
//! quadratic visible as a SLOPE, because a wall-time ceiling cannot: the
//! defect this stage fixes measured 13 ms at seed-42 scale and 48 seconds at
//! 1M facts, so any budget set against realistic data would have passed it
//! forever.
//!
//! Run: `cargo run --release -p hornvale-kernel --example query_scaling`
//! ALWAYS `--release`: a debug build measures the optimizer.
//!
//! ## Two sweeps, and only one of them discriminates
//!
//! **Sweep A (ledger size, agents fixed)** holds `AGENTS` constant and grows
//! `history`, so ledger size `n = AGENTS * history` is the only variable.
//! With `AGENTS` fixed, the per-agent `scan` loop touches all `n` facts and
//! the per-agent `facts_of` loop touches its own `history`-sized slice, so
//! the *totals* are `AGENTS * n` and `AGENTS * (log n + history)` — **both
//! linear in `n`**. Sweep A is NOT expected to separate the two axes; its
//! value is showing that ledger size alone, at a fixed query count, does not
//! distinguish an indexed read from an unindexed one.
//!
//! **Sweep B (agent count, history fixed) is THE DISCRIMINATING ONE.** Write
//! `A` for agent count and `H` for the now-fixed history, so `n = A * H`.
//! Each of `A` agents' `scan` still touches every one of the `n` facts with
//! that predicate, so the total is `A * n = A * (A * H) = A^2 * H` —
//! quadratic in `A`, **slope 2** against `n` (since `n` grows proportionally
//! to `A` at fixed `H`, a fit against `n` and a fit against `A` share the
//! same slope). Each of `A` agents' `facts_of` still touches only its own
//! `H` facts plus an index descent, so the total is `A * (log n + H)` —
//! linear in `A`, **slope 1**. This is the real defect's shape: more agents
//! means both more queries *and* more facts, which is what made the
//! pre-fix read quadratic in session length.
//!
//! ## Measured
//!
//! Date: 2026-08-22. Box: `ambrose` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! size_of::<Fact>()  = 104
//! size_of::<Value>() = 24
//!
//! Sweep A: ledger size (agents fixed at 200, history grows). Not
//! expected to discriminate scan from facts_of -- see the module doc.
//!   agents  history      facts      scan_ms  facts_of_ms  place_scan_ms
//!      200       25       5000        14.31         0.32           0.26
//!      200       50      10000        34.62         0.72           0.60
//!      200      100      20000       206.17         1.48           0.94
//!      200      200      40000       571.45         2.02           2.02
//!      200      400      80000      1023.04         5.84           6.44
//!
//! fitted log-log slope, sweep A (1.0 linear, 2.0 quadratic):
//!   scan          1.64
//!   facts_of      0.99
//!   place scan    1.10
//!
//! Sweep B: agent count (history fixed at 50) -- THE DISCRIMINATING
//! ONE. See the module doc for the A^2*H vs A*(log n + H) reasoning.
//!   agents  history      facts      scan_ms  facts_of_ms  place_scan_ms
//!       25       50       1250         0.79         0.09           0.02
//!       50       50       2500         1.39         0.11           0.10
//!      100       50       5000         9.45         0.33           0.40
//!      200       50      10000        41.94         0.58           0.79
//!      400       50      20000       278.55         2.62           1.05
//!
//! fitted log-log slope, sweep B (1.0 linear, 2.0 quadratic):
//!   scan          2.18
//!   facts_of      1.21
//!   place scan    1.51
//! ```
//!
//! A second run (same box, same profile) landed even closer to the
//! predicted 2-vs-1 contrast on sweep B: `scan` 2.01, `facts_of` 0.94,
//! `place scan` 0.98 — while sweep A again failed to discriminate (`scan`
//! 1.60, `facts_of` 1.29, `place scan` 1.35), confirming both halves of the
//! prediction above: sweep A cannot separate an indexed read from an
//! unindexed one because it holds agent count fixed, and sweep B does,
//! landing close to the theoretical 2 (unindexed `scan`) vs 1 (indexed
//! `facts_of`) split. `facts_of`'s slope runs a little above 1.0 in every
//! run (0.99, 1.21, 0.94, 1.29 across the two runs shown here, both on
//! `ambrose`) — most likely allocation/sort overhead in
//! `positions_for_subject_predicate` at millisecond scale, not an
//! algorithmic issue, since it stays far below `scan`'s slope every time.
//!
//! **What `place_scan` is expected to do, under either sweep.** Unlike
//! `scan`/`facts_of`, it is not part of the sweep-B contrast: its query
//! count is fixed at 64 rooms (`0..64`, `synthetic`'s own `place` range)
//! regardless of which dimension is swept, so it is expected to land near
//! **slope 1 (linear in `n`) under both Sweep A and Sweep B** — an
//! unindexed scan run a constant number of times, not a variable one. A
//! printed fitted slope with no stated expectation is the exact failure
//! this bench exists to prevent, so it is worth being explicit that
//! `place_scan`'s job here is corroboration, not discrimination: the
//! measured values (1.10 / 1.51, then 1.35 / 0.98 on the second run)
//! bracket 1 about as tightly as `facts_of`'s do.

// The measurement harness times derivation calls for a diagnostic (never sim
// logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), same pattern as
// `cli/tests/suite/scene_cost.rs` and `cli/tests/suite/graph_cost.rs`.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

use hornvale_kernel::{ConceptRegistry, EntityId, Fact, Ledger, Value};

const AGENT_AT: &str = "agent-at";

/// A synthetic ledger: `agents` subjects each holding `history` `agent-at`
/// facts, committed round-robin so subjects interleave in commit order the
/// way a real tick loop produces them.
fn synthetic(agents: u64, history: u64, reg: &ConceptRegistry) -> Ledger {
    let mut l = Ledger::default();
    for h in 0..history {
        for a in 1..=agents {
            l.commit(
                Fact {
                    subject: EntityId::new(a).expect("nonzero"),
                    predicate: AGENT_AT.to_string(),
                    object: Value::Number((h * 7 + a) as f64),
                    place: EntityId::new(1_000_000 + (a % 64)),
                    day: None,
                    provenance: "synthetic".to_string(),
                },
                reg,
            )
            .expect("synthetic commit");
        }
    }
    l
}

/// Least-squares slope of log(y) against log(x) — the scaling exponent.
/// 1.0 means linear in ledger size; 2.0 is the quadratic bug class.
fn log_log_slope(xs: &[f64], ys: &[f64]) -> f64 {
    let n = xs.len() as f64;
    let lx: Vec<f64> = xs.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let ly: Vec<f64> = ys.iter().map(|v| hornvale_kernel::math::ln(*v)).collect();
    let mx = lx.iter().sum::<f64>() / n;
    let my = ly.iter().sum::<f64>() / n;
    let num: f64 = lx.iter().zip(&ly).map(|(x, y)| (x - mx) * (y - my)).sum();
    let den: f64 = lx.iter().map(|x| (x - mx) * (x - mx)).sum();
    num / den
}

/// Time the three reads against one built ledger: an unindexed `find` +
/// filter by subject (once per agent), the SPO-indexed `facts_of` (once per
/// agent), and an unindexed scan by `place` (once per room — `place` is not
/// an index key, so this is the shape stage 2 exists to serve). Returns
/// `(scan_ms, facts_of_ms, place_scan_ms)`.
fn measure(l: &Ledger, agents: u64) -> (f64, f64, f64) {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut sink = 0usize;
    for a in 1..=agents {
        let e = EntityId::new(a).unwrap();
        sink += l.find(AGENT_AT).filter(|f| f.subject == e).count();
    }
    let scan = t.elapsed().as_secs_f64() * 1e3;
    std::hint::black_box(sink);

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut sink2 = 0usize;
    for a in 1..=agents {
        sink2 += l.facts_of(EntityId::new(a).unwrap(), AGENT_AT).count();
    }
    let idx = t.elapsed().as_secs_f64() * 1e3;
    std::hint::black_box(sink2);
    assert_eq!(sink, sink2, "INDEX != SCAN");

    #[allow(clippy::disallowed_types)] // benchmark harness
    let t = Instant::now();
    let mut sink3 = 0usize;
    for room in 0..64u64 {
        let p = EntityId::new(1_000_000 + room);
        sink3 += l.iter().filter(|f| f.place == p).count();
    }
    let place = t.elapsed().as_secs_f64() * 1e3;
    std::hint::black_box(sink3);

    (scan, idx, place)
}

/// Run one scaling sweep over `(agents, history)` points, printing one table
/// row per point, and return `(facts, scan_ms, facts_of_ms, place_scan_ms)`
/// for the caller to fit slopes against.
fn run_sweep(
    reg: &ConceptRegistry,
    points: &[(u64, u64)],
) -> (Vec<f64>, Vec<f64>, Vec<f64>, Vec<f64>) {
    let mut facts = Vec::new();
    let mut scan_ms = Vec::new();
    let mut index_ms = Vec::new();
    let mut place_ms = Vec::new();

    println!(
        "{:>8} {:>8} {:>10} {:>12} {:>12} {:>14}",
        "agents", "history", "facts", "scan_ms", "facts_of_ms", "place_scan_ms"
    );
    for &(agents, history) in points {
        let l = synthetic(agents, history, reg);
        let n = (agents * history) as f64;
        let (scan, idx, place) = measure(&l, agents);

        println!("{agents:>8} {history:>8} {n:>10.0} {scan:>12.2} {idx:>12.2} {place:>14.2}");
        facts.push(n);
        scan_ms.push(scan);
        index_ms.push(idx);
        place_ms.push(place);
    }
    (facts, scan_ms, index_ms, place_ms)
}

fn main() {
    println!("size_of::<Fact>()  = {}", std::mem::size_of::<Fact>());
    println!("size_of::<Value>() = {}", std::mem::size_of::<Value>());
    println!();

    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "synthetic")
        .unwrap();

    println!(
        "Sweep A: ledger size (agents fixed at 200, history grows). Not\n\
         expected to discriminate scan from facts_of -- see the module doc."
    );
    const SWEEP_A_AGENTS: u64 = 200;
    let sweep_a: Vec<(u64, u64)> = [25u64, 50, 100, 200, 400]
        .into_iter()
        .map(|h| (SWEEP_A_AGENTS, h))
        .collect();
    let (a_facts, a_scan, a_idx, a_place) = run_sweep(&reg, &sweep_a);

    println!();
    println!("fitted log-log slope, sweep A (1.0 linear, 2.0 quadratic):");
    println!("  scan          {:.2}", log_log_slope(&a_facts, &a_scan));
    println!("  facts_of      {:.2}", log_log_slope(&a_facts, &a_idx));
    println!("  place scan    {:.2}", log_log_slope(&a_facts, &a_place));

    println!();
    println!(
        "Sweep B: agent count (history fixed at 50) -- THE DISCRIMINATING\n\
         ONE. See the module doc for the A^2*H vs A*(log n + H) reasoning."
    );
    const SWEEP_B_HISTORY: u64 = 50;
    let sweep_b: Vec<(u64, u64)> = [25u64, 50, 100, 200, 400]
        .into_iter()
        .map(|a| (a, SWEEP_B_HISTORY))
        .collect();
    let (b_facts, b_scan, b_idx, b_place) = run_sweep(&reg, &sweep_b);

    println!();
    println!("fitted log-log slope, sweep B (1.0 linear, 2.0 quadratic):");
    println!("  scan          {:.2}", log_log_slope(&b_facts, &b_scan));
    println!("  facts_of      {:.2}", log_log_slope(&b_facts, &b_idx));
    println!("  place scan    {:.2}", log_log_slope(&b_facts, &b_place));
}
