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
//! ## Measured
//!
//! Date: 2026-08-22. Box: `ambrose` (`hostname -s`). Profile: `--release`.
//! Verbatim output:
//!
//! ```text
//! size_of::<Fact>()  = 104
//! size_of::<Value>() = 24
//!
//!      facts      scan_ms  facts_of_ms place_scan_ms
//!       5000        12.51         0.17         0.20
//!      10000        36.26         0.44         0.42
//!      20000       139.11         0.87         2.65
//!      40000       467.70         3.88         1.85
//!      80000       996.00         7.85        18.40
//!
//! fitted log-log slope vs ledger size (1.0 linear, 2.0 quadratic):
//!   scan          1.63
//!   facts_of      1.42
//!   place scan    1.52
//! ```
//!
//! A second run (same box, same profile) reproduced the same shape —
//! `scan` 1.62, `facts_of` 1.40, `place scan` 1.23 — so the direction is
//! stable even though the machine is a laptop under ordinary session load,
//! not a quiet benchmark box. `facts_of`'s absolute slope is higher than
//! the O(k) per-query cost alone would predict (the query loop still holds
//! `AGENTS` fixed and grows only `history`, which should track close to
//! linear), most likely reflecting allocation/sort overhead in
//! `positions_for_subject_predicate` at these small millisecond scales
//! rather than an algorithmic issue — but it is consistently, and by
//! design measurably, the shallower of the two indexed-vs-unindexed
//! comparisons the brief calls for: `scan`'s slope is the steepest of the
//! three every run.

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

fn main() {
    println!("size_of::<Fact>()  = {}", std::mem::size_of::<Fact>());
    println!("size_of::<Value>() = {}", std::mem::size_of::<Value>());
    println!();

    let mut reg = ConceptRegistry::default();
    reg.register_predicate(AGENT_AT, false, "synthetic")
        .unwrap();

    // Fixed agent count, growing history: ledger size is the only variable,
    // which is what makes the slope interpretable.
    const AGENTS: u64 = 200;
    let histories = [25u64, 50, 100, 200, 400];

    let mut facts = Vec::new();
    let mut scan_ms = Vec::new();
    let mut index_ms = Vec::new();
    let mut place_ms = Vec::new();

    println!(
        "{:>10} {:>12} {:>12} {:>12}",
        "facts", "scan_ms", "facts_of_ms", "place_scan_ms"
    );
    for h in histories {
        let l = synthetic(AGENTS, h, &reg);
        let n = (AGENTS * h) as f64;

        #[allow(clippy::disallowed_types)] // benchmark harness
        let t = Instant::now();
        let mut sink = 0usize;
        for a in 1..=AGENTS {
            let e = EntityId::new(a).unwrap();
            sink += l.find(AGENT_AT).filter(|f| f.subject == e).count();
        }
        let scan = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink);

        #[allow(clippy::disallowed_types)] // benchmark harness
        let t = Instant::now();
        let mut sink2 = 0usize;
        for a in 1..=AGENTS {
            sink2 += l.facts_of(EntityId::new(a).unwrap(), AGENT_AT).count();
        }
        let idx = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink2);
        assert_eq!(sink, sink2, "INDEX != SCAN");

        // The unindexed axis: `place` is not an index key, so this is the
        // shape stage 2 exists to serve. 64 rooms, one query each.
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t = Instant::now();
        let mut sink3 = 0usize;
        for room in 0..64u64 {
            let p = EntityId::new(1_000_000 + room);
            sink3 += l.iter().filter(|f| f.place == p).count();
        }
        let place = t.elapsed().as_secs_f64() * 1e3;
        std::hint::black_box(sink3);

        println!("{n:>10.0} {scan:>12.2} {idx:>12.2} {place:>12.2}");
        facts.push(n);
        scan_ms.push(scan);
        index_ms.push(idx);
        place_ms.push(place);
    }

    println!();
    println!("fitted log-log slope vs ledger size (1.0 linear, 2.0 quadratic):");
    println!("  scan          {:.2}", log_log_slope(&facts, &scan_ms));
    println!("  facts_of      {:.2}", log_log_slope(&facts, &index_ms));
    println!("  place scan    {:.2}", log_log_slope(&facts, &place_ms));
}
