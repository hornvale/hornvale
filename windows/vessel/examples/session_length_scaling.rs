//! The Tailrace: does tick cost grow with SESSION LENGTH at a fixed agent
//! count — i.e. with the size of the committed history the drives fold over?
//!
//! INFORMATIVE, NEVER A GATE — the same standing as its sibling
//! `windows/vessel/examples/agent_scaling.rs`, whose construction this
//! copies almost verbatim so the two are comparable rung for rung.
//!
//! Run: `cargo run --release -p hornvale-vessel --example session_length_scaling`
//! ALWAYS `--release`: a debug build measures the optimizer, not the code.
//!
//! ## The question this exists to answer, and why the sibling cannot
//!
//! `agent_scaling.rs` sweeps AGENT COUNT at a fixed 20 ticks, and attributes
//! its superlinear residual to the per-creature population-wide reads
//! (`hazard_memory_memo`/`alarm_field_memo`, each threaded the full roster) —
//! an O(agents^2) shape. `tick_commit_budget.rs` sweeps ticks but measures
//! only facts committed, never TIME.
//!
//! So nothing in the tree measures the third axis: **six production folds in
//! `liveness.rs` walk an agent's committed history on every evaluation** —
//! `agent_sightings`/`integrate_thirst` via `drive_at` (the thirst path
//! integral), `hunger_at` (the same trail, `HUNGER` params), `fatigue_at`
//! (over `rested`), `believed_water` (the water belief), `hazard_memory_memo`
//! (latest-visit-per-cell) and `build_emitter_scan` (alarm halos). The ledger
//! is append-only, so each is O(history), and history grows every tick. Three
//! multipliers sit in the same call path: `shared_believed_water` calls
//! `believed_water` once per co-located peer, `build_emitter_scan` is threaded
//! the full roster inside a per-agent call, and `begin` re-folds `DRANK`,
//! `RESTED` and `EATEN` from scratch per creature per tick.
//!
//! That predicts a cost curve rising with SESSION LENGTH even when the
//! population is held perfectly still — a term neither sibling bench can see,
//! because one holds ticks fixed and the other does not time them.
//!
//! **The two hypotheses are discriminated by holding agents fixed.** If the
//! residual is purely the O(agents^2) roster read, banded ms/tick is FLAT
//! across a long run at fixed `AGENTS`. If the folds' O(history) term is
//! real, banded ms/tick RISES with the band index. Both can be true at once;
//! this bench measures only the second.
//!
//! ## What biases the answer, and in which direction
//!
//! Conservatively, against the hypothesis. `HomeNavCache` and `RoomMeshMemo`
//! are threaded across every tick and start COLD, so the opening band pays
//! search and mesh misses that later bands reuse — pushing early bands UP and
//! late bands DOWN, which is the opposite of the predicted trend. A rising
//! curve measured through that headwind is therefore a lower bound on the
//! real history term, and a FLAT curve is genuinely ambiguous (a real rise
//! could be exactly cancelling cache warming) rather than a clean null.
//!
//! Reported alongside each band so the reader can judge it: the cache's
//! search delta per band (warming shows up as a falling search count) and the
//! ledger length at the band's end (the history the folds actually walked).
//!
//! ## Wall time is noisy, and this bench carries its own yardstick
//!
//! Bands are measured in sequence, so history depth and elapsed wall-clock
//! time are perfectly correlated and any drift in machine availability over
//! the run masquerades as a history effect. This is not hypothetical: on the
//! development Mac a quiet-box run read 328 -> 535 ms/tick across its warm
//! bands while a run at load average 18.00 read 887 -> 504 — same code, same
//! seed, opposite conclusions. Bands cannot be shuffled to break the
//! correlation, because history only grows in one direction.
//!
//! So every band is divided by a `calibrate()` yardstick taken immediately
//! before it (see that function), and the `ms/tick*` column — the one every
//! statistic uses — is normalised to band 1's machine speed. The raw column
//! and the yardstick are both printed, so the normalisation is auditable
//! rather than trusted.
//!
//! `facts`, `searches`, `folded/a` and `ledger_len` are deterministic and
//! byte-stable regardless of load, so a shape claim the deterministic columns
//! contradict is a harness bug, not a finding.

use hornvale_kernel::RoomAddr;
use hornvale_kernel::{EntityId, Fact, Ledger, RoomMeshMemo, Value, World, WorldTime};
use hornvale_locale::LocaleContext;
use hornvale_species::MetabolicClass;
use hornvale_vessel::liveness::{
    DriveMovements, HomeNavCache, LocaleTerrain, SUSTENANCE, Terrain, derive_npcs, drive_at,
};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
// The measurement harness times each tick for a diagnostic (never sim logic,
// never a fact, never seeded from wall-clock) -- exempt from the wall-clock
// ban (clippy.toml / decision 0001), the same pattern `agent_scaling.rs`,
// `turn_cost.rs` and `kernel/examples/query_scaling.rs` all use.
#[allow(clippy::disallowed_types)]
use std::time::Instant;

/// Agents held FIXED for the whole run — the point of the bench is that this
/// does not move. Chosen mid-range on `agent_scaling.rs`'s own sweep (10/50/
/// 100/200) so the O(agents^2) roster term is present and non-trivial but
/// does not dominate the wall clock enough to bury a history term underneath
/// run-to-run noise.
const AGENTS: usize = 50;

/// Total ticks driven. Ten times `agent_scaling.rs`'s 20, because a history
/// term needs history: at ~2.5 facts/agent/tick and 50 agents this reaches
/// roughly 25,000 committed facts, a 10x span of ledger length across the run.
const TICKS: usize = 200;

/// Iterations in one `calibrate()` yardstick. Sized so the yardstick costs a
/// few milliseconds -- long enough to average over scheduler noise, short
/// enough that ten of them are free next to the run itself.
const CALIB_ITERS: u64 = 1 << 22;

/// Back-to-back `drive_at` calls averaged into one `fold_us` reading. Large
/// enough that scheduler noise averages out of a microsecond-scale span, small
/// enough to stay free next to the band it follows.
const FOLD_REPS: u32 = 200;

/// Ticks per reported band. `TICKS / BAND` bands, each a mean over `BAND`
/// individually-timed ticks — enough averaging that one scheduler hiccup does
/// not read as a trend, few enough bands that the shape is legible.
const BAND: usize = 20;

// NOTE: this bench deliberately carries NO log-log-slope helper, unlike its
// siblings `agent_scaling.rs` and `kernel/examples/query_scaling.rs`. Both of
// those sweep a variable across a wide range against a cost with a small
// intercept, where a power-law exponent is the right summary. Here the
// relationship is affine with a LARGE intercept (see the fit below), and an
// affine relation has no single power-law exponent -- forcing one yields a
// number that tracks the sampled x-range instead of the mechanism. Two
// earlier runs of this bench measured 1.23 and 1.69 that way and the spread
// was the fit's conditioning, not the sim. The affine `C + k*x` fit replaced
// it outright rather than being reported alongside it, so nobody reads the
// wrong number off this instrument.

/// A fixed, deterministic unit of arithmetic, timed — the bench's own yardstick
/// for how fast this machine is RIGHT NOW.
///
/// **Why this exists, and it is the difference between a usable instrument and
/// a useless one.** Bands are measured in sequence, so history depth and
/// elapsed wall-clock time are perfectly correlated: any drift in machine
/// availability over the run masquerades as a history effect, in whichever
/// direction the load happened to move. Measured on the development Mac: one
/// run of this bench on a quiet box read 328 -> 535 ms/tick across its warm
/// bands (rising), and a later run at load average 18.00 with an editor helper
/// at 141% CPU read 887 -> 504 (falling). Same code, same seed, opposite
/// conclusions. Bands cannot be shuffled to break that correlation, because
/// history only grows in one direction.
///
/// So each band is divided by this yardstick, taken immediately before it. A
/// slow box inflates both the band and the yardstick and the ratio survives;
/// only a change in the SIM's own work moves the normalised number. The
/// workload is a wrapping integer hash over a fixed range — no allocation, no
/// syscalls, no floating point, nothing the optimizer can elide (the result is
/// consumed), and identical on every host.
fn calibrate() -> f64 {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let mut acc = 0x9E3779B97F4A7C15_u64;
    for i in 0..CALIB_ITERS {
        acc = acc
            .wrapping_mul(6364136223846793005)
            .wrapping_add(i)
            .rotate_left(17);
    }
    let ms = t0.elapsed().as_secs_f64() * 1000.0;
    // Consume `acc` so nothing above can be optimized away. `black_box` is
    // unstable, so this is the stable equivalent: a branch on the value the
    // compiler cannot prove is never taken.
    if acc == 0 {
        println!("calibration produced 0 -- vanishingly unlikely, reported so it is never silent");
    }
    ms
}

/// Time one `drive_at` call, averaged over `FOLD_REPS` back-to-back calls.
///
/// **This is the instrument that answers the campaign's question, and the
/// whole-tick column is context around it.** Timing a whole tick puts the
/// history fold in the same span as A* search, the population-wide roster
/// reads, occupancy bookkeeping and the commits — so a disturbance anywhere on
/// the box lands on the number, and on the development Mac it repeatedly did:
/// three runs of the whole-tick column gave +2.05x, 0.70x and 0.64x across the
/// same warm bands, one of them with a single band 3.5x out of line. Those runs
/// disagree about the SIGN.
///
/// `drive_at` alone has none of that inside it. It folds `DRANK` for the reset
/// day, folds the agent's `agent-at` postings into a sightings vector, and
/// integrates. Nothing in that span scales with anything but the length of the
/// history it walks, so the ratio between two bands is the history term
/// directly — and being microsecond-scale and repeated `FOLD_REPS` times, it
/// averages rather than samples.
///
/// The `t` passed is the band's current day, so the fold walks every posting
/// the agent has, exactly as the live tick's own call does.
fn probe_fold_us(
    ledger: &Ledger,
    entity: EntityId,
    home: &RoomAddr,
    t: WorldTime,
    terrain: &dyn Terrain,
    class: MetabolicClass,
) -> f64 {
    #[allow(clippy::disallowed_types)] // benchmark harness
    let t0 = Instant::now();
    let mut sink = 0.0_f64;
    for _ in 0..FOLD_REPS {
        sink += drive_at(ledger, entity, home, t, &SUSTENANCE, terrain, class);
    }
    let us = t0.elapsed().as_secs_f64() * 1e6 / FOLD_REPS as f64;
    // Consume `sink` so the calls cannot be optimized away.
    if sink < 0.0 {
        println!("thirst integral went negative -- impossible, reported so it is never silent");
    }
    us
}

/// Each roster member's own `agent-at` posting count — the per-agent history
/// the folds in `liveness.rs` traverse on every evaluation. Read through
/// `facts_of`, the same indexed path those folds use, so this counts exactly
/// what they walk and not a superset.
fn folded_counts(ledger: &Ledger, roster: &[EntityId]) -> Vec<usize> {
    roster
        .iter()
        .map(|e| {
            ledger
                .facts_of(*e, hornvale_vessel::liveness::AGENT_AT)
                .count()
        })
        .collect()
}

/// Deterministic byte estimate for `ledger` — flat `Fact` size times the fact
/// count plus the heap each fact's `String`/`Value::Text` fields own (byte
/// length, not capacity). Copied from `agent_scaling.rs`.
fn ledger_bytes(ledger: &Ledger) -> usize {
    let flat = ledger.len() * std::mem::size_of::<Fact>();
    let heap: usize = ledger
        .iter()
        .map(|f| {
            f.predicate.len()
                + f.provenance.len()
                + match &f.object {
                    Value::Text(s) => s.len(),
                    _ => 0,
                }
        })
        .sum();
    flat + heap
}

/// One band's measured outcome.
struct Band {
    /// 1-based band index; `ticks_before` is what the run had already driven.
    index: usize,
    ticks_before: usize,
    ms_per_tick: f64,
    /// **THE DECISIVE MEASUREMENT.** Mean microseconds for one `drive_at`
    /// call on the probe agent, over `FOLD_REPS` back-to-back calls — the
    /// thirst path integral, which is exactly the fold whose incrementalisation
    /// this campaign turns on. Nothing else runs inside the timed span: no A*,
    /// no roster read, no occupancy, no commit. So this column cannot be moved
    /// by anything except the length of the history the fold walks.
    fold_us: f64,
    /// `fold_us` normalised to band 1's machine speed.
    norm_fold_us: f64,
    /// The probe agent's own `agent-at` count when `fold_us` was taken — the
    /// x-axis `fold_us` is regressed against.
    probe_history: usize,
    /// The `calibrate()` yardstick taken immediately before this band, in ms.
    calib_ms: f64,
    /// `ms_per_tick` scaled to band 1's machine speed: `ms_per_tick *
    /// (calib[1] / calib[this])`. THE COLUMN EVERY STATISTIC BELOW USES.
    norm_ms_per_tick: f64,
    /// Facts committed during this band.
    facts_delta: usize,
    /// `HomeNavCache::searches()` delta during this band — a falling value is
    /// the cache warming, the headwind the module doc names.
    searches_delta: u64,
    /// Ledger length at the END of the band. Reported for context, and
    /// deliberately NOT the x-axis of any fit below -- see `folded_len`.
    ledger_len: usize,
    ledger_bytes: usize,
    /// **The actual independent variable.** `Ledger::facts_of` is indexed on
    /// `(subject, predicate)`, so a fold like `agent_sightings` walks only
    /// THIS agent's own `agent-at` facts -- never the whole ledger. Genesis
    /// commits ~12,500 facts before the walk starts, so `ledger_len` moves
    /// only ~1.5x across this run while the per-agent history the folds
    /// actually traverse moves ~6x. Fitting against `ledger_len` therefore
    /// regresses cost on a near-constant and reads as ill-conditioned; this
    /// column is the quantity the mechanism is about. Measured by summing
    /// each roster member's own `agent-at` postings and dividing by the
    /// roster size -- the MEAN folded history per agent.
    folded_len: f64,
    /// The least and greatest per-agent `agent-at` count in the roster, so a
    /// reader can judge whether `folded_len`'s mean is representative rather
    /// than taking it on trust.
    folded_min: usize,
    folded_max: usize,
}

fn main() {
    let world = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 42 must build under default pins");
    let ctx = LocaleContext::build(&world).expect("seed 42 locale context must build");

    let settlement_count = hornvale_settlement::all_settlements(&world).len();
    let home_settlement = hornvale_settlement::village_info(&world)
        .expect("seed 42's flagship always exists")
        .id;
    let day_length_std = hornvale_worldgen::sky_of(&world)
        .ok()
        .and_then(|sky| sky.calendar().cloned())
        .and_then(|c| c.day_length())
        .map(|d| d.get());

    assert!(
        AGENTS <= settlement_count,
        "AGENTS={AGENTS} exceeds seed 42's {settlement_count} settlements (derive_npcs's ceiling)"
    );
    println!(
        "session_length_scaling: seed 42, {AGENTS} agents held FIXED, {TICKS} ticks in bands of {BAND}"
    );

    let bands = run(&world, &ctx, home_settlement, day_length_std);

    println!(
        "{:>5} {:>12} {:>11} {:>11} {:>9} {:>7} {:>9} {:>9} {:>12}",
        "band",
        "ticks",
        "fold us*",
        "ms/tick*",
        "calib_ms",
        "facts",
        "searches",
        "folded/a",
        "ledger_len"
    );
    println!(
        "  (* normalised to band 1's machine speed. `fold us*` is the DECISIVE column -- \
         see the module doc.)"
    );
    for b in &bands {
        println!(
            "{:>5} {:>5}-{:<6} {:>11.2} {:>11.2} {:>9.2} {:>7} {:>9} {:>9.1} {:>12}",
            b.index,
            b.ticks_before,
            b.ticks_before + BAND - 1,
            b.norm_fold_us,
            b.norm_ms_per_tick,
            b.calib_ms,
            b.facts_delta,
            b.searches_delta,
            b.folded_len,
            b.ledger_len
        );
    }

    // Is the mean representative? Printed once rather than per row, and
    // reported as a SPREAD so the reader judges it instead of trusting it.
    if let Some(last) = bands.last() {
        println!(
            "per-agent folded history at the final band: mean {:.1}, min {}, max {} \
             (a wide spread would mean the mean stands for nothing)",
            last.folded_len, last.folded_min, last.folded_max
        );
    }

    // BAND 1 IS EXCLUDED FROM EVERY STATISTIC BELOW, and the exclusion is
    // declared rather than silent. It pays every agent's cold `HomeNavCache`
    // and `RoomMeshMemo` miss -- measured at roughly 4x the steady-state
    // search count -- so it is not a sample of the same process the later
    // bands sample. Its row is still PRINTED above, so a reader can see what
    // was dropped and check the 4x claim rather than take it on trust.
    let warm = &bands[1.min(bands.len())..];
    if warm.len() >= 3 {
        let first = &warm[0];
        let last = warm.last().expect("len >= 3");
        println!();
        println!(
            "warm bands only (band 1 excluded -- cold cache, {} searches vs {} steady). \
             Ledger grew to {} bytes over {} facts.",
            bands[0].searches_delta, first.searches_delta, last.ledger_bytes, last.ledger_len
        );

        // ---- THE DECISIVE FIT: the fold alone, against its own history. ----
        //
        // The model is affine, not a power law. A fold that walks a history of
        // length h costs a fixed setup plus per-fact work:
        //
        //     us/call = C + k * h
        //
        // An affine relation with a nonzero intercept has NO single power-law
        // exponent, and forcing one yields a number that tracks the sampled
        // x-range rather than the mechanism -- which is why this bench carries
        // no log-log helper at all.
        report_affine(
            "DECISIVE -- drive_at alone (us/call) vs the probe agent's own history",
            "us/call",
            &warm
                .iter()
                .map(|b| b.probe_history as f64)
                .collect::<Vec<_>>(),
            &warm.iter().map(|b| b.norm_fold_us).collect::<Vec<_>>(),
            first.probe_history as f64,
            last.probe_history as f64,
        );

        // ---- CONTEXT: the whole tick, which mixes the fold with everything
        // else in the span and has proven unable to hold a sign on this box.
        report_affine(
            "context -- whole tick (ms/tick) vs mean per-agent history",
            "ms/tick",
            &warm.iter().map(|b| b.folded_len).collect::<Vec<_>>(),
            &warm.iter().map(|b| b.norm_ms_per_tick).collect::<Vec<_>>(),
            first.folded_len,
            last.folded_len,
        );

        // Monotonicity of the decisive column, with the CORRECT tail
        // probability. An earlier draft printed 2^-steps regardless of how many
        // rises were observed, so a 5-of-8 run reported p = 0.0039 -- the
        // probability of 8 of 8.
        let rises = warm
            .windows(2)
            .filter(|w| w[1].norm_fold_us > w[0].norm_fold_us)
            .count();
        let steps = warm.len() - 1;
        println!(
            "  monotonicity of the decisive column: {rises}/{steps} rises \
             (one-sided binomial p = {:.4})",
            binomial_tail(rises, steps)
        );
    }
}

/// One-sided binomial tail `P(X >= k)` for `n` fair coin flips — the null
/// probability that a no-trend process produces at least `k` rises out of `n`
/// band-to-band comparisons. Exact (summed binomial coefficients in `f64`),
/// which is fine for the single-digit `n` this bench produces.
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
/// that says how much of the variance the fit actually explains.
///
/// `r^2` is printed because it is the honest brake on the headline: a positive
/// `k` with `r^2` near zero is noise wearing a slope, and this bench has
/// produced exactly that on a loaded box. Read `k` only when `r^2` supports it.
fn report_affine(title: &str, unit: &str, xs: &[f64], ys: &[f64], x_first: f64, x_last: f64) {
    let n = xs.len() as f64;
    let mx = xs.iter().sum::<f64>() / n;
    let my = ys.iter().sum::<f64>() / n;
    let sxy: f64 = xs.iter().zip(ys).map(|(x, y)| (x - mx) * (y - my)).sum();
    let sxx: f64 = xs.iter().map(|x| (x - mx) * (x - mx)).sum();
    let syy: f64 = ys.iter().map(|y| (y - my) * (y - my)).sum();
    println!("  {title}:");
    if sxx <= 0.0 {
        println!("    x never moved across the bands -- no fit is possible, and that is a");
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

    // THE ELASTICITY IS THE HEADLINE, AND `C` IS OFTEN NOT IDENTIFIABLE.
    //
    // The sampled history range starts well away from zero (the probe agent
    // already carries ~130 facts by the first warm band), so `C` is an
    // extrapolation far outside the data and comes out NEGATIVE on some runs --
    // physically impossible for a cost, and it makes a "the history term is N%
    // of the total" line read ABOVE 100%. An earlier draft printed exactly
    // that: 106.5% and 103.0%. So the share prints only when `C` is a possible
    // floor, and the elasticity -- which needs no intercept -- leads instead.
    //
    // Elasticity is d(log cost)/d(log history) between the first and last warm
    // band: 1.0 means cost is PROPORTIONAL to history (a pure walk over it),
    // 0.0 means history is free. A descriptive ratio over the measured range,
    // deliberately NOT a claimed functional form -- the fit is affine because
    // the mechanism is affine; the elasticity is quoted because it is the one
    // summary that survives `C` being unidentifiable.
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
        "    history {x_first:.1} -> {x_last:.1} ({:.2}x)",
        x_last / x_first
    );
    if c >= 0.0 {
        println!(
            "    C = {c:.3} {unit} floor; at the last band the history term is {:.1}% of the total",
            100.0 * k * x_last / (c + k * x_last)
        );
    } else {
        println!(
            "    C = {c:.3} {unit} -- NEGATIVE, so not identifiable from this range (history starts at {x_first:.0}, not 0). No share reported; read the elasticity."
        );
    }
}

/// Drive `AGENTS` NPCs through `TICKS` ticks over one ledger, timing each
/// tick and reporting per-band means. Construction is `agent_scaling.rs`'s
/// `run_rung` verbatim (one caller-owned `HomeNavCache`/`RoomMeshMemo`
/// threaded across every tick, facts committed by hand into a bench-owned
/// ledger clone) — only the timing granularity differs.
fn run(
    world: &World,
    ctx: &LocaleContext,
    home_settlement: EntityId,
    day_length_std: Option<f64>,
) -> Vec<Band> {
    let mut ledger = world.ledger.clone();
    let mut registry = world.registry.clone();
    // The four predicates the NPC drive stack writes -- copied from
    // `agent_scaling.rs`, which copied them from `Session::start`.
    for (pred, doc) in [
        (
            hornvale_vessel::liveness::AGENT_AT,
            "an agent's position on a day",
        ),
        (
            hornvale_vessel::liveness::DRANK,
            "an agent satisfied its sustenance goal",
        ),
        (
            hornvale_vessel::liveness::RESTED,
            "an agent rested (eased its fatigue) on a day",
        ),
        (
            hornvale_vessel::liveness::EATEN,
            "an agent ate (eased its hunger) on a day",
        ),
    ] {
        registry
            .register_predicate(pred, false, doc)
            .expect("these four predicates register identically every run");
    }

    let npcs = derive_npcs(world, ctx, &mut ledger, AGENTS, home_settlement);
    let mut mesh_memo = RoomMeshMemo::new();
    let mut home_nav_cache = HomeNavCache::new();
    let mut day = WorldTime::new(0.5).expect("0.5 is finite");

    // NO SINGLE PROBE AGENT. An earlier draft reported one agent's own
    // `agent-at` count (`npcs.first()`), and it read 0 for every band of
    // every run -- a column that silently NaN'd the whole fit rather than
    // failing. Whatever the reason that particular body never committed, the
    // lesson is that a one-agent proxy can be unrepresentative in a way the
    // output does not advertise. The roster MEAN cannot be zero unless the
    // sim committed nothing at all, and `min`/`max` are printed beside it so
    // a reader can see whether the mean stands for anything.
    // `Body` is private to the crate, so the roster is carried here as bare
    // `EntityId`s -- everything below needs the identity, nothing needs the body.
    let roster: Vec<EntityId> = npcs.iter().map(|n| n.entity).collect();

    // The probe agent for `fold_us`, chosen ONCE and then fixed for the whole
    // run so every band's reading is about the same creature's growing
    // history rather than about a different creature. Chosen as the roster
    // member with the MOST `agent-at` postings after the first band -- not the
    // first member, which an earlier draft used and which read zero: the
    // per-agent spread is wide (min 0, max 420 measured at the final band), so
    // some derived bodies never commit a position at all and a positional
    // choice can silently land on one.
    let mut probe: Option<(EntityId, RoomAddr, MetabolicClass)> = None;

    let mut bands: Vec<Band> = Vec::new();
    let mut band_facts_before = ledger.len();
    let mut band_searches_before = home_nav_cache.searches();
    let mut band_ms = 0.0_f64;

    for tick in 0..TICKS {
        let from = day;
        day = WorldTime::new(day.day() + 1.0).expect("day advance stays finite");
        let mesh_snapshot = mesh_memo.clone();
        let terrain = LocaleTerrain::with_fields(ctx, None, None, None, None, Some(&mesh_snapshot));
        let sys = DriveMovements {
            npcs: npcs.clone(),
            from,
            to: day,
            params: SUSTENANCE,
            day_length_std,
            terrain: &terrain,
        };
        // Timed span: the drive evaluation AND the commits it produces, the
        // same pair `agent_scaling.rs` times as one. The `npcs.clone()` and
        // `mesh_memo.clone()` above are harness artifacts and stay OUTSIDE
        // the span deliberately -- the metaplan (6.7) already flags that
        // full-`BTreeMap` clone as a suspected cost of the harness rather
        // than of the sim, and a bench asking about the sim's history term
        // must not fold a per-tick harness clone into the answer.
        #[allow(clippy::disallowed_types)] // benchmark harness
        let t0 = Instant::now();
        let (facts, _occupancy) =
            sys.step_with_occupancy(&ledger, &mut mesh_memo, &mut home_nav_cache);
        for fact in facts {
            ledger
                .commit(fact, &registry)
                .expect("a real drive-movements fact always commits");
        }
        band_ms += t0.elapsed().as_secs_f64() * 1000.0;

        if (tick + 1) % BAND == 0 {
            let facts_after = ledger.len();
            let counts = folded_counts(&ledger, &roster);
            if probe.is_none() {
                let (i, _) = counts
                    .iter()
                    .enumerate()
                    .max_by_key(|(_, c)| **c)
                    .expect("the roster is non-empty");
                probe = Some((roster[i], npcs[i].home.clone(), npcs[i].metabolic_class));
            }
            let (p_entity, p_home, p_class) = probe
                .clone()
                .expect("set on the first band and never cleared");
            let p_history = ledger
                .facts_of(p_entity, hornvale_vessel::liveness::AGENT_AT)
                .count();
            let mesh_for_probe = mesh_memo.clone();
            let probe_terrain =
                LocaleTerrain::with_fields(ctx, None, None, None, None, Some(&mesh_for_probe));
            let fold_us = probe_fold_us(&ledger, p_entity, &p_home, day, &probe_terrain, p_class);
            let calib_ms = calibrate();
            let searches_after = home_nav_cache.searches();
            bands.push(Band {
                index: bands.len() + 1,
                ticks_before: tick + 1 - BAND,
                ms_per_tick: band_ms / BAND as f64,
                calib_ms,
                // Filled in after the loop, once band 1's yardstick is known.
                norm_ms_per_tick: 0.0,
                facts_delta: facts_after - band_facts_before,
                searches_delta: searches_after - band_searches_before,
                ledger_len: facts_after,
                ledger_bytes: ledger_bytes(&ledger),
                folded_len: counts.iter().sum::<usize>() as f64 / counts.len() as f64,
                folded_min: counts.iter().copied().min().unwrap_or(0),
                folded_max: counts.iter().copied().max().unwrap_or(0),
                fold_us,
                norm_fold_us: 0.0,
                probe_history: p_history,
            });
            band_facts_before = facts_after;
            band_searches_before = searches_after;
            band_ms = 0.0;
        }
    }
    // Normalise against band 1's yardstick, so the normalised column reads in
    // "ms/tick at band 1's machine speed" and band 1 is 1:1 with itself.
    let base = bands.first().map(|b| b.calib_ms).unwrap_or(0.0);
    if base > 0.0 {
        for b in bands.iter_mut() {
            b.norm_ms_per_tick = b.ms_per_tick * base / b.calib_ms;
            b.norm_fold_us = b.fold_us * base / b.calib_ms;
        }
    }
    bands
}
