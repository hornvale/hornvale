//! The Tailrace, Task 1: a SYNTHETIC depth sweep for `drive_at`'s history
//! term -- the complement to `session_length_scaling.rs`, which can only
//! OBSERVE history by growing it.
//!
//! INFORMATIVE, NEVER A GATE. A new file under `windows/vessel/examples/`, so
//! it cannot conflict with The Escapement's `liveness.rs` edits -- it only
//! calls that module's public surface.
//!
//! Run: `cargo run --release -p hornvale-vessel --example fold_depth_sweep`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//!
//! ## Why a synthetic sweep, when `session_length_scaling.rs` already measures history
//!
//! That bench grows history by ticking a real session forward, so it can
//! only ever observe the depths a 200-tick run happens to pass through --
//! measured there as a 2.48x span, starting well above zero. Two
//! consequences follow directly from that: the affine model's intercept `C`
//! is not identifiable from a range that never approaches zero, and depth is
//! PERFECTLY CORRELATED with elapsed wall-clock time, so any drift in
//! machine availability over the run is indistinguishable from a genuine
//! history effect -- which is exactly what happened there: three runs of its
//! whole-tick column disagreed about the sign.
//!
//! This bench sets depth directly instead of growing it, over a ~1000x span
//! that starts at 10, and visits every depth on EVERY pass, alternating the
//! sweep direction each pass so depth and elapsed time are decorrelated by
//! construction rather than merely hoped to be. A median across passes, not
//! a mean, because robustness to one disturbed pass is the entire point of
//! taking more than one.
//!
//! ## What this bench does, and does not, measure
//!
//! `drive_at` alone, over a synthetic ledger with no genesis, no A* search,
//! no roster, no commits inside the timed span, and a constant-answering
//! `Terrain` (`FlatTerrain`, below) so the only thing that varies across the
//! sweep is the number of segments the fold walks. `session_length_scaling
//! .rs`'s `fold_us` column is the same instrument, measured in situ instead;
//! this is its synthetic complement, not a replacement -- a disagreement
//! between the two is a real finding about one of them, not a tiebreak.

use hornvale_kernel::registry::ConceptRegistry;
use hornvale_kernel::{EntityId, Fact, Ledger, RoomAddr, Value, WorldTime};
use hornvale_species::MetabolicClass;
use hornvale_vessel::liveness::{AGENT_AT, DRANK, SUSTENANCE, Terrain, drive_at};
use std::collections::BTreeMap;
// The measurement harness times a derivation call for a diagnostic (never
// sim logic, never a fact, never seeded from wall-clock) -- exempt from the
// wall-clock ban (clippy.toml / decision 0001), the same pattern
// `session_length_scaling.rs` and `kernel/examples/query_scaling.rs` use.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// The depths swept. Spans ~1000x so the affine fit is well-conditioned and
/// `C` is identifiable -- the range `session_length_scaling.rs` cannot reach,
/// because it grows history by ticking and only ever spans 2.48x.
const DEPTHS: &[usize] = &[10, 32, 100, 320, 1_000, 3_200, 10_000];

/// Passes over `DEPTHS`. Each pass visits every depth; the ORDER reverses on
/// odd passes, so every depth is measured both early and late in the run.
///
/// THIS IS THE WHOLE POINT OF THIS BENCH. In `session_length_scaling.rs`
/// history can only grow, so depth and elapsed wall-clock time are perfectly
/// correlated and any drift in machine availability arrives disguised as a
/// history effect -- measured there as three runs disagreeing about the SIGN.
/// Here depth is set, not grown, so alternating the direction decorrelates the
/// two, and a median across passes is robust to a disturbance hitting one.
const PASSES: usize = 6;

/// Back-to-back `drive_at` calls averaged into one reading.
const FOLD_REPS: u32 = 50;

/// A `Terrain` that answers the same thing everywhere -- so the only variable
/// in this sweep is the ledger's depth.
///
/// The thirst integral reads `temperature` once per segment and nothing else
/// that varies, so a constant field makes every segment cost the same and the
/// measured difference between two depths is the *number* of segments alone.
/// That is the point: `LocaleTerrain` would fold real terrain reads (and a
/// `RoomMeshMemo` whose warmth changes over a run) into the number.
struct FlatTerrain;

impl Terrain for FlatTerrain {
    fn elevation(&self, _room: &RoomAddr) -> f64 {
        // Never read by `drive_at`'s call path (the thirst integral only
        // ever calls `temperature`, below) -- a finite, arbitrary value so
        // nothing panics if a future change routes through it.
        0.0
    }

    fn is_fresh_water(&self, _room: &RoomAddr) -> bool {
        // Same non-read as `elevation`; `false` is the simplest valid answer.
        false
    }

    fn temperature(&self, _room: &RoomAddr, _day: WorldTime) -> f64 {
        // THE ONE FIELD `drive_at`'s fold actually reads, once per segment
        // (via `rise_at`). A fixed, thermoneutral-ish value so every segment
        // costs the same -- see the struct doc.
        20.0
    }
}

/// A small fixed set of rooms (8 of the mesh's 20 base faces, at depth 0)
/// that the synthetic ledger's postings cycle through. A CONSTANT room would
/// let a future optimisation collapse the segments and silently flatter the
/// fold, so the postings must actually move.
fn room_for(i: usize) -> RoomAddr {
    const ROOM_COUNT: usize = 8;
    RoomAddr {
        face: (i % ROOM_COUNT) as u8,
        path: Vec::new(),
    }
}

/// Encode a `RoomAddr` exactly as `liveness.rs`'s own (private) `room_to_text`
/// does: the packed `RoomId`, rendered as a decimal `u64` string.
/// `drive_at` decodes this with `room_from_text`, so any other encoding either
/// panics or silently reads a different room -- the second failure mode would
/// produce a plausible but wrong number.
fn room_to_text(r: &RoomAddr) -> String {
    r.pack()
        .expect("a depth-0 face room always packs")
        .0
        .to_string()
}

/// One synthetic `agent-at` posting for day `i + 1`: cycles through
/// [`room_for`]'s small fixed room set rather than holding one room constant.
fn agent_at(entity: EntityId, i: usize) -> Fact {
    Fact {
        subject: entity,
        predicate: AGENT_AT.to_string(),
        object: Value::Text(room_to_text(&room_for(i))),
        place: None,
        day: Some(
            WorldTime::new(i as f64 + 1.0).expect("i + 1 is finite for every depth in DEPTHS"),
        ),
        provenance: "synthetic".to_string(),
    }
}

/// The one `drank` fact that bounds the thirst integral's window: the reset
/// day the fold starts from.
fn drank_at(entity: EntityId, day: f64) -> Fact {
    Fact {
        subject: entity,
        predicate: DRANK.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(WorldTime::new(day).expect("0.0 is finite")),
        provenance: "synthetic".to_string(),
    }
}

/// A ledger holding exactly `depth` `agent-at` facts for `entity`, one per day,
/// plus the one `drank` fact that bounds the thirst integral's window.
///
/// Days ascend by 1.0 so the integral has `depth` segments to walk. The rooms
/// cycle through a small fixed set rather than being constant, because a
/// constant room would let a future optimisation collapse the segments and
/// silently flatter the fold.
fn synthetic_ledger(entity: EntityId, depth: usize, registry: &ConceptRegistry) -> Ledger {
    let mut ledger = Ledger::default();
    ledger
        .commit(drank_at(entity, 0.0), registry)
        .expect("a synthetic drank fact commits");
    for i in 0..depth {
        ledger
            .commit(agent_at(entity, i), registry)
            .expect("a synthetic agent-at fact commits");
    }
    ledger
}

/// The median of `xs`, sorted in place with the constitutional `total_cmp`
/// (no native float comparison). `PASSES` is even, so an even-length slice
/// takes the mean of its two middle values; written generally rather than
/// hard-coded to `PASSES` in case a future run changes it.
fn median(xs: &mut [f64]) -> f64 {
    xs.sort_by(f64::total_cmp);
    let n = xs.len();
    if n % 2 == 1 {
        xs[n / 2]
    } else {
        (xs[n / 2 - 1] + xs[n / 2]) / 2.0
    }
}

/// One-sided binomial tail `P(X >= k)` for `n` fair coin flips -- the null
/// probability that a no-trend process produces at least `k` rises out of `n`
/// depth-to-depth comparisons. Exact (summed binomial coefficients in `f64`),
/// which is fine for the single-digit `n` this bench produces. Copied from
/// `session_length_scaling.rs`, which copied it from `agent_scaling.rs` --
/// each bench in this directory carries its own copy rather than sharing a
/// module for a fourth user.
fn binomial_tail(k: usize, n: usize) -> f64 {
    let mut total = 0.0_f64;
    for i in k..=n {
        let mut c = 1.0_f64;
        for j in 0..i {
            c = c * (n - j) as f64 / (j + 1) as f64;
        }
        total += c;
    }
    total / 2.0_f64.powi(n as i32)
}

/// Fit `ys = C + k * xs` by ordinary least squares and print it, with the `r^2`
/// that says how much of the variance the fit actually explains. Reused,
/// shape and all, from `session_length_scaling.rs::report_affine` -- same
/// reasons apply here: the relation is affine with a potentially large
/// intercept, not a power law, so this bench prints no log-log exponent.
fn report_affine(title: &str, unit: &str, xs: &[f64], ys: &[f64], x_first: f64, x_last: f64) {
    let n = xs.len() as f64;
    let mx = xs.iter().sum::<f64>() / n;
    let my = ys.iter().sum::<f64>() / n;
    let sxy: f64 = xs.iter().zip(ys).map(|(x, y)| (x - mx) * (y - my)).sum();
    let sxx: f64 = xs.iter().map(|x| (x - mx) * (x - mx)).sum();
    let syy: f64 = ys.iter().map(|y| (y - my) * (y - my)).sum();
    println!("  {title}:");
    if sxx <= 0.0 {
        println!("    x never moved across the sweep -- no fit is possible, and that is a");
        println!("    harness fault rather than a null result. Report it, do not read past it.");
        return;
    }
    let k = sxy / sxx;
    let c = my - k * mx;
    let r2 = if syy > 0.0 {
        sxy * sxy / (sxx * syy)
    } else {
        0.0
    };
    println!("    k = {k:.5} {unit} per additional fact of history  (r^2 = {r2:.3})");

    // Elasticity between the first and last swept depth: 1.0 means cost is
    // PROPORTIONAL to history (a pure walk over it), 0.0 means history is
    // free. Needs no intercept, so it survives `C` being unidentifiable --
    // same reasoning `session_length_scaling.rs` states at length.
    let y_first = my - k * (mx - x_first);
    let y_last = my - k * (mx - x_last);
    if x_first > 0.0 && y_first > 0.0 && y_last > 0.0 {
        let elasticity = hornvale_kernel::math::ln(y_last / y_first)
            / hornvale_kernel::math::ln(x_last / x_first);
        println!(
            "    elasticity over the measured range = {elasticity:.2} (1.0 = cost proportional to history, 0.0 = history free)"
        );
    }
    println!(
        "    depth {x_first:.1} -> {x_last:.1} ({:.2}x)",
        x_last / x_first
    );
    if c >= 0.0 {
        println!(
            "    C = {c:.3} {unit} floor; at the last depth the history term is {:.1}% of the total",
            100.0 * k * x_last / (c + k * x_last)
        );
    } else {
        println!(
            "    C = {c:.3} {unit} -- NEGATIVE, so not identifiable from this range (depth starts at {x_first:.0}, not 0). No share reported; read the elasticity."
        );
    }
}

fn main() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(AGENT_AT, false, "an agent's position on a day")
        .expect("AGENT_AT registers identically every run");
    registry
        .register_predicate(DRANK, false, "an agent satisfied its sustenance goal")
        .expect("DRANK registers identically every run");

    let entity = EntityId::new(1).expect("1 is nonzero");
    let home = room_for(0);
    let terrain = FlatTerrain;
    let class = MetabolicClass::Endotherm;

    println!(
        "fold_depth_sweep: {} depths x {PASSES} passes (alternating direction) x {FOLD_REPS} reps/reading",
        DEPTHS.len()
    );
    println!("depths swept: {DEPTHS:?}");
    println!();

    // Every pass's reading at each depth, keyed by depth -- a `BTreeMap`
    // rather than a `HashMap` (decision 0005: no nondeterministic iteration
    // order in this workspace, even in a bench harness).
    let mut by_depth: BTreeMap<usize, Vec<f64>> = BTreeMap::new();
    for &d in DEPTHS {
        by_depth.insert(d, Vec::with_capacity(PASSES));
    }

    for pass in 0..PASSES {
        // THIS IS THE WHOLE POINT OF THIS BENCH -- see the `PASSES` doc.
        let order: Vec<usize> = if pass % 2 == 0 {
            DEPTHS.to_vec()
        } else {
            DEPTHS.iter().rev().copied().collect()
        };
        for depth in order {
            let ledger = synthetic_ledger(entity, depth, &registry);
            // One past the last posted day, so the fold walks every one of
            // this depth's `depth` segments.
            let t = WorldTime::new(depth as f64 + 1.0).expect("depth + 1 is finite");

            #[allow(clippy::disallowed_types)] // benchmark harness
            let t0 = Instant::now();
            let mut sink = 0.0_f64;
            for _ in 0..FOLD_REPS {
                sink += drive_at(&ledger, entity, &home, t, &SUSTENANCE, &terrain, class);
            }
            let us = t0.elapsed().as_secs_f64() * 1e6 / f64::from(FOLD_REPS);
            // Consume `sink` so the calls cannot be optimized away.
            if sink < 0.0 {
                println!(
                    "thirst integral went negative -- impossible, reported so it is never silent"
                );
            }
            by_depth
                .get_mut(&depth)
                .expect("depth is one of DEPTHS, inserted above")
                .push(us);
        }
    }

    println!(
        "{:>8} {:>12} {:>12} {:>12}",
        "depth", "median us*", "min us", "max us"
    );
    println!("  (* median across all {PASSES} passes, both directions -- see report contract)");

    let mut depths_sorted: Vec<usize> = DEPTHS.to_vec();
    depths_sorted.sort_unstable();
    let mut xs: Vec<f64> = Vec::with_capacity(depths_sorted.len());
    let mut ys: Vec<f64> = Vec::with_capacity(depths_sorted.len());
    for &d in &depths_sorted {
        let mut readings = by_depth
            .get(&d)
            .expect("every depth in DEPTHS was measured on every pass")
            .clone();
        let med = median(&mut readings);
        let min = readings.iter().copied().fold(f64::INFINITY, f64::min);
        let max = readings.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        println!("{d:>8} {med:>12.3} {min:>12.3} {max:>12.3}");
        xs.push(d as f64);
        ys.push(med);
    }

    println!();
    report_affine(
        "DECISIVE -- drive_at alone (us/call) vs synthetic ledger depth, interleaved",
        "us/call",
        &xs,
        &ys,
        *xs.first().expect("DEPTHS is non-empty"),
        *xs.last().expect("DEPTHS is non-empty"),
    );

    // Monotonicity of the median column across ascending depth, with the
    // correct tail probability (see `binomial_tail`'s doc).
    let rises = ys.windows(2).filter(|w| w[1] > w[0]).count();
    let steps = ys.len() - 1;
    println!(
        "  monotonicity of the median column: {rises}/{steps} rises (one-sided binomial p = {:.4})",
        binomial_tail(rises, steps)
    );
}
