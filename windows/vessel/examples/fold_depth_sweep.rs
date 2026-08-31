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
//! ## Why there are TWO instruments, and why neither may be deleted
//!
//! The distinction is **internal vs ecological validity**, and it is the
//! reason these two files are complements rather than duplicates.
//!
//! This bench has INTERNAL validity: depth is set directly and decorrelated
//! from elapsed time by construction, so the history term can be isolated and
//! `C` identified. What it CANNOT do is say what fraction of a REAL tick that
//! term is -- because it has no real tick. There is no roster, no A* search,
//! no occupancy bookkeeping, no commit; the denominator such a share would be
//! taken against does not exist here. Any "share of a tick" number from this
//! file would be a number about this file.
//!
//! `session_length_scaling.rs` has ECOLOGICAL validity and little internal
//! validity: it ticks a real 50-agent session, so its "70-80% of a tick" is a
//! claim about the system as it runs -- at the cost of a narrow, time-
//! correlated depth range.
//!
//! **So a shape finding wants this bench and a share finding wants the
//! sibling, and a future campaign that deletes either one loses a claim the
//! other cannot make.** Neither is the "better" instrument.
//!
//! This bench sets depth directly instead of growing it, over a ~1000x span
//! that starts at 10, and visits every depth on EVERY pass, alternating the
//! sweep direction each pass so depth and elapsed time are decorrelated by
//! construction rather than merely hoped to be. A median across passes, not
//! a mean, because robustness to one disturbed pass is the entire point of
//! taking more than one.
//!
//! ## The mechanism this bench found, and why it now sweeps TWO regimes
//!
//! An earlier version of this file ran one sweep only -- a single `drank`
//! fact at day 0.0, never repeated -- and measured a fit with `k > 0` but a
//! NEGATIVE `C`: the signature of a superlinear, not affine, relationship.
//! Reading `integrate_thirst` (`windows/vessel/src/liveness.rs`) explains it.
//! Write `H` for the agent's WHOLE `agent-at` history (what `agent_sightings`
//! builds, `O(H)`) and `S` for the sightings since its last drink (what
//! `integrate_thirst`'s outer loop over segment boundaries actually walks,
//! `O(S)`). Each of those `S` segments looks up its governing position with:
//!
//! ```text
//! let pos = sightings
//!     .iter()
//!     .rev()
//!     .find(|(d, _)| *d <= s)
//! ```
//!
//! -- a backward linear scan of the FULL `H`-length sightings vector, not an
//! indexed or incrementally-tracked lookup. So the real cost is
//! `O(H + S*H)`, not the `O(history)` the sibling benches' docs narrate for
//! the five trail-walking folds. The old single-reset sweep committed exactly
//! one `drank` at the start, so every posting is a sighting since that one
//! drink: `S == H`, and the measured cost is dominated by the quadratic
//! `S*H == H^2` term -- a real pathology, and (per the roster measurement in
//! spec §4) the WORST-CASE regime a production agent can run in, not a
//! synthetic impossibility: at the final band of a 50-agent roster, 23 of 50
//! agents (46%) had drunk zero times in 200 ticks and so sit exactly in this
//! regime, the probe agent among them. It is not the TYPICAL regime either --
//! the median agent drank 9 times, which keeps `S` small and bounded while `H`
//! grows, and is why the spec's own in-situ measurement of that half reads an
//! elasticity of 0.86-1.24, not ~2. See spec §4's "roster's own `drank`
//! distribution" for the full breakdown.
//!
//! So this bench now sweeps BOTH regimes, driven by the same `DEPTHS`/
//! `PASSES`/`FOLD_REPS` machinery, clearly labelled so neither is mistaken
//! for the other:
//!
//! - **PERIODIC RESETS** (`Some(RESET_EVERY)`): a `drank` fact every
//!   `RESET_EVERY` postings, so `S` stays bounded near `RESET_EVERY` while
//!   `H` grows across the sweep. This isolates the `O(H)` term.
//! - **SINGLE EARLY RESET** (`None`): the original regime, `S == H`,
//!   exposing the `O(S*H)` term in isolation.
//!
//! Both are real measurements of the same function; they are not in
//! conflict with each other, and a disagreement between either of them and
//! spec §4 is a finding about the mechanism, not a bench defect to smooth
//! over.
//!
//! **`RESET_EVERY = 20` is an AUTHORED GUESS, not a derived or measured
//! number, and the periodic sweep is only an approximation of production's
//! regime until it is grounded.** This bench is synthetic and knows its own
//! reset cadence by construction, which is exactly why it cannot tell you
//! whether 20 is close to how often a real agent actually drinks. The
//! production postings-per-drink ratio was UNMEASURED as of this bench, and
//! HAS SINCE BEEN MEASURED, by a later task in this campaign: a `drank`-count
//! column added to `session_length_scaling.rs` reports, at the final band of
//! a 50-agent roster, min 0 / median 9 / max 47 `drank` facts, with 23 of 50
//! agents (46%) never having drunk at all (spec §4 has the full breakdown).
//! So `RESET_EVERY = 20` describes neither half well: the never-drinks 46%
//! keep no reset within the run's horizon, and the drinking half's own median
//! (9 events / 200 ticks) is closer to "every 22 ticks" than "every 20
//! postings" -- ticks and postings are not the same unit, and this bench does
//! not know the conversion for that half specifically. Read the periodic
//! sweep below as "what the O(H) term looks like at a plausible, but still
//! only approximately grounded, cadence," not as a closed comparison to spec
//! §4.
//!
//! ## Reconciling with spec §4, with numbers (measured; expect wall-clock noise to move these on a re-run, and it did)
//!
//! `report_affine`'s "fitted" elasticity is derived from the OLS line, which
//! bakes the model's intercept into the number -- useful, but not the same
//! statistic as a plain endpoint ratio. `report_raw_elasticities` computes a
//! MODEL-FREE elasticity directly from two RAW medians (`ln(y2/y1) /
//! ln(x2/x1)`), reported over three ranges of the same periodic sweep: the
//! whole swept range, the top half (depth >= 1,000), and the top third
//! (depth >= 3,200). A positive intercept `C` drags an end-to-end
//! (whole-range) elasticity below the mechanism's true asymptotic exponent
//! whenever `C` is a real share of the cost at the low end -- which is why
//! the whole-range figure reads lower than the top-half one, and the
//! top-half/top-third figures (where `C`'s share is smallest) are the ones
//! actually comparable to spec §4's in-situ 0.86-1.24.
//!
//! TWO SEPARATE RUNS ON THIS BOX, NOT ONE, BECAUSE WALL TIME HERE IS NOISY
//! AND THAT IS THE POINT OF SHOWING BOTH RATHER THAN PICKING ONE:
//!
//! ```text
//!                    whole range     top half        top third
//! run 1 (periodic)      0.849       1.062 (in range)  0.829
//! run 2 (periodic)      0.874       1.073 (in range)  1.238 (in range, at the edge)
//! run 1 (single-reset)  1.518       2.023              2.088
//! run 2 (single-reset)  1.518       2.025              2.088
//! ```
//!
//! The periodic figures moved measurably between runs (as expected -- this
//! is a raw wall-clock measurement, not a fitted or averaged one, so it
//! carries the full noise the small periodic-regime medians already show at
//! low depth); the single-reset figures barely moved at all, because that
//! regime's cost is dominated by the `O(H^2)` term and swamps the same
//! wall-clock noise. The CONCLUSION did not move either way: in both runs
//! the periodic regime's top-half elasticity landed inside spec §4's
//! 0.86-1.24 in-situ range, and in both runs the single-reset regime's
//! elasticity sat near 2 at the high end. The periodic top-third figure
//! shows real run-to-run scatter (0.829 vs 1.238) -- report the range you
//! see, not the most flattering point in it.
//!
//! **Conclusion:** the periodic regime's high-depth elasticity lands inside
//! spec §4's in-situ range, so the two instruments agree about the regime
//! the DRINKING half of production runs in -- SUBJECT TO `RESET_EVERY`'s
//! cadence being a reasonable stand-in for reality, which is an open item,
//! not something this bench closes (see `RESET_EVERY`'s doc). The
//! single-reset regime's ~2 exponent is real, and belongs to a "never drinks"
//! regime that production DOES reach: the roster measurement in spec §4
//! found 46% of a 50-agent roster (23/50) had drunk zero times at the final
//! band, the probe agent among them. So this is the WORST CASE, reached by
//! nearly half the roster, not a synthetic impossibility -- it is simply not
//! the TYPICAL case, since the median agent drank 9 times and so sits in the
//! periodic-ish regime instead.
//!
//! ## Raw per-depth medians, both rounds, both regimes
//!
//! The durable source for spec §4's crossover-reconciliation subsection,
//! which cites specific absolute figures from these tables (depth 320 reads
//! 31.7-32.0 µs/call single-reset against the periodic sweep's 8.1-10.6
//! µs/call at the same depth). Recorded here, in the tree, rather than left
//! to live only in a task transcript: `.superpowers/sdd/` is git-ignored and
//! dies with the worktree that produced it, so a spec citing "Task 1's
//! report" for a number is citing something that will not outlive the
//! campaign that wrote it. This file is the durable home for these numbers;
//! the spec should cite it, not the transcript.
//!
//! ```text
//! PERIODIC RESETS -- median us/call by depth:
//!    depth      round 1      round 2
//!       10        0.825        0.816
//!       32        1.863        1.179
//!      100        3.029        3.093
//!      320        8.089       10.560
//!     1000       25.275       28.981
//!     3200      113.317       83.558
//!    10000      291.544      342.463
//!
//! SINGLE EARLY RESET -- median us/call by depth:
//!    depth      round 1      round 2
//!       10        0.725        0.731
//!       32        1.320        1.378
//!      100        5.122        5.695
//!      320       31.693       32.022
//!     1000      247.102      247.545
//!     3200     2408.472     2426.205
//!    10000    25998.897    26202.435
//! ```
//!
//! Round 1's figures are from the fix-round-1 measurement that first split
//! the sweep into the periodic and single-reset regimes; round 2's are the
//! independent re-run taken for the model-free-elasticity fix round (the
//! "TWO SEPARATE RUNS" elasticity table above is derived from these same two
//! sets of medians). Both are genuine measurements of the same sweep, run at
//! different times on the same box; neither supersedes the other, which is
//! why both are kept.
//!
//! ## What this bench does, and does not, measure
//!
//! `drive_at` alone, over a synthetic ledger with no genesis, no A* search,
//! no roster, no commits inside the timed span, and a constant-answering
//! `Terrain` (`FlatTerrain`, below) so the only thing that varies across
//! either sweep is the number of postings the fold walks. `session_length_
//! scaling.rs`'s `fold_us` column is the same instrument, measured in situ
//! instead (and, by construction, in the periodic-ish regime a real session
//! actually runs in); this file is its synthetic complement, not a
//! replacement -- a disagreement between the two is a real finding about
//! one of them, not a tiebreak.

use hornvale_kernel::registry::ConceptRegistry;
use hornvale_kernel::{EntityId, Facet, Fact, Ledger, Value, WorldTime};
use hornvale_species::ThermalStrategy;
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

/// Reset cadence for the PERIODIC sweep: a `drank` fact committed every this
/// many `agent-at` postings, so sightings-since-last-drink (`S`, in the
/// module doc's notation) stays bounded near this value while the whole
/// history (`H`) grows across `DEPTHS` -- isolating the `O(H)` term from the
/// `O(S*H)` one the single-reset sweep exposes. Below `RESET_EVERY` postings
/// (the `DEPTHS` entries of 10) no reset ever triggers, so at that one depth
/// the periodic sweep COMPUTES the exact same thing the single-reset sweep
/// does -- the same code path, not merely a similar one. That is why the two
/// depth-10 rows can print DIFFERENT measured timings (e.g. 0.825 vs 0.725
/// µs/call) without that being a discrepancy: they are two separately-timed
/// runs of identical computation, and the gap between them is ordinary
/// wall-clock noise at microsecond scale, not evidence the two regimes
/// diverge at depth 10. Expected, not a bug, and worth knowing when reading
/// that row.
///
/// **THIS IS AN AUTHORED GUESS, NOT A MEASURED OR DERIVED NUMBER.** Nothing
/// in this bench (or its sibling `session_length_scaling.rs`) establishes
/// how many `agent-at` postings a production agent actually accumulates
/// between drinks -- this file is synthetic and sets its own cadence by
/// construction, so it cannot ground that ratio from the inside. Do not read
/// "20" as a derivation; read it as "a plausible value, picked to keep `S`
/// small without being trivially tiny, pending the measurement that would
/// settle it" (see the module doc's reconciliation section for what that
/// measurement is and where it is scoped).
const RESET_EVERY: usize = 20;

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
    fn elevation(&self, _room: &Facet) -> f64 {
        // Never read by `drive_at`'s call path (the thirst integral only
        // ever calls `temperature`, below) -- a finite, arbitrary value so
        // nothing panics if a future change routes through it.
        0.0
    }

    fn is_fresh_water(&self, _room: &Facet) -> bool {
        // Same non-read as `elevation`; `false` is the simplest valid answer.
        false
    }

    fn temperature(&self, _room: &Facet, _day: WorldTime) -> f64 {
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
fn room_for(i: usize) -> Facet {
    const ROOM_COUNT: usize = 8;
    Facet {
        face: (i % ROOM_COUNT) as u8,
        path: Vec::new(),
    }
}

/// Encode a `Facet` as `drive_at` expects to decode it: the packed `FacetId`,
/// rendered as a decimal `u64` string.
///
/// **This used to be a hand-copy of `liveness.rs`'s own (then private)
/// `room_to_text`**, carrying a comment saying so. The Chattel made
/// `thing::room_key` the crate's one room-key encoder — `agent-at`,
/// `located-in` and this sweep all route through it now — so the copy is
/// gone. Any other encoding either panics in `room_from_text` or silently
/// reads a different room, and the second failure mode would produce a
/// plausible but wrong number.
fn room_to_text(r: &Facet) -> String {
    hornvale_vessel::thing::room_key(r).expect("a depth-0 face room always packs")
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
            WorldTime::from_std_days(i as f64 + 1.0)
                .expect("i + 1 is finite for every depth in DEPTHS"),
        ),
        provenance: "synthetic".to_string(),
    }
}

/// A `drank` fact on `day` -- either the one reset that bounds the
/// single-reset regime's window, or one of the periodic regime's repeated
/// resets (see [`synthetic_ledger`]'s doc).
fn drank_at(entity: EntityId, day: f64) -> Fact {
    Fact {
        subject: entity,
        predicate: DRANK.to_string(),
        object: Value::Flag(true),
        place: None,
        day: Some(WorldTime::from_std_days(day).expect("day is finite")),
        provenance: "synthetic".to_string(),
    }
}

/// A ledger holding exactly `depth` `agent-at` facts for `entity`, one per
/// day, under one of two reset regimes for the `drank` fact(s) that bound
/// the thirst integral's window (see the module doc's `O(H + S*H)` finding
/// for why the regime matters to what this bench actually measures):
///
/// - `reset_every == None`: ONE `drank` fact, at day 0.0, and never again --
///   the SINGLE-RESET regime. Sightings-since-last-drink `S` equals the
///   whole history `H`, so this exposes the `O(S*H)` term.
/// - `reset_every == Some(n)`: the initial day-0.0 `drank` fact, PLUS another
///   `drank` fact every `n` postings (same day as that posting) -- the
///   PERIODIC regime. `S` stays bounded near `n` regardless of how large `H`
///   grows, isolating the `O(H)` term.
///
/// Days ascend by 1.0 so the integral has `depth` segments to walk. The
/// rooms cycle through a small fixed set rather than being constant, because
/// a constant room would let a future optimisation collapse the segments and
/// silently flatter the fold.
fn synthetic_ledger(
    entity: EntityId,
    depth: usize,
    reset_every: Option<usize>,
    registry: &ConceptRegistry,
) -> Ledger {
    let mut ledger = Ledger::default();
    ledger
        .commit(drank_at(entity, 0.0), registry)
        .expect("a synthetic drank fact commits");
    for i in 0..depth {
        ledger
            .commit(agent_at(entity, i), registry)
            .expect("a synthetic agent-at fact commits");
        let posting_count = i + 1;
        if let Some(n) = reset_every
            && n > 0
            && posting_count % n == 0
        {
            ledger
                .commit(drank_at(entity, posting_count as f64), registry)
                .expect("a synthetic periodic drank fact commits");
        }
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

/// Monotonicity of a median column, ascending by depth, with the correct
/// tail probability (see `binomial_tail`'s doc). Shared by both sweeps so
/// neither prints it slightly differently.
fn report_monotonicity(ys: &[f64]) {
    let rises = ys.windows(2).filter(|w| w[1] > w[0]).count();
    let steps = ys.len() - 1;
    println!(
        "  monotonicity of the median column: {rises}/{steps} rises (one-sided binomial p = {:.4})",
        binomial_tail(rises, steps)
    );
}

/// A model-free elasticity between two swept points: `ln(y2/y1) /
/// ln(x2/x1)`, computed directly off their RAW medians -- never off a
/// fitted line. Distinguished from `report_affine`'s "fitted" elasticity
/// (which bakes the OLS model's intercept into the number) precisely
/// because the two can disagree, and a positive intercept `C` will pull the
/// fitted one down relative to this one whenever `C` is a real share of the
/// cost at the low end. `None` when either endpoint would make the ratio
/// undefined or non-finite (a non-positive `x1`, or a non-positive `y`).
fn raw_elasticity(x1: f64, y1: f64, x2: f64, y2: f64) -> Option<f64> {
    if x1 > 0.0 && x2 > 0.0 && y1 > 0.0 && y2 > 0.0 && x1 != x2 {
        Some(hornvale_kernel::math::ln(y2 / y1) / hornvale_kernel::math::ln(x2 / x1))
    } else {
        None
    }
}

/// Print the raw-median (model-free) elasticity over three ranges of one
/// sweep's medians: the whole swept range, the top half (depth >= 1,000),
/// and the top third (depth >= 3,200). Reported alongside, not instead of,
/// `report_affine`'s fitted number -- see [`raw_elasticity`]'s doc and the
/// module doc's reconciliation section for why a positive `C` makes the two
/// disagree, and by how much.
fn report_raw_elasticities(xs: &[f64], ys: &[f64]) {
    let last_i = xs.len() - 1;
    println!("  raw-median elasticity (model-free, off the endpoints' own medians, not the fit):");
    // "whole range" starts at index 0; "top half"/"top third" start at the
    // first swept depth at or above the named threshold, if the sweep
    // reaches that far (it always does for `DEPTHS` as authored, but this
    // stays honest if a future edit shortens it).
    let starts: [(&str, Option<usize>); 3] = [
        ("whole range", Some(0)),
        ("top half", xs.iter().position(|&x| x >= 1_000.0)),
        ("top third", xs.iter().position(|&x| x >= 3_200.0)),
    ];
    for (label, start) in starts {
        let Some(i) = start else {
            println!("    {label:<12} -- sweep does not reach this range, skipped");
            continue;
        };
        match raw_elasticity(xs[i], ys[i], xs[last_i], ys[last_i]) {
            Some(e) => println!(
                "    {label:<12} {:>6.0} -> {:>6.0} : {e:.3}",
                xs[i], xs[last_i]
            ),
            None => println!(
                "    {label:<12} {:>6.0} -> {:>6.0} : not computable (non-positive endpoint)",
                xs[i], xs[last_i]
            ),
        }
    }
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

    // FITTED elasticity between the first and last swept depth, off the OLS
    // line: 1.0 means cost is PROPORTIONAL to history (a pure walk over it),
    // 0.0 means history is free, ~2.0 means quadratic. Needs no intercept, so
    // it survives `C` being unidentifiable -- same reasoning
    // `session_length_scaling.rs` states at length. Labelled "fitted" and
    // kept distinct from `report_raw_elasticities`'s MODEL-FREE number below
    // -- the two can disagree (a positive `C` drags this one down), and two
    // numbers both called "elasticity" that disagree silently is worse than
    // either alone.
    let y_first = my - k * (mx - x_first);
    let y_last = my - k * (mx - x_last);
    if x_first > 0.0 && y_first > 0.0 && y_last > 0.0 {
        let elasticity = hornvale_kernel::math::ln(y_last / y_first)
            / hornvale_kernel::math::ln(x_last / x_first);
        println!(
            "    fitted elasticity (off the OLS line) = {elasticity:.2} (1.0 = proportional to history, 0.0 = history free, 2.0 = quadratic)"
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

/// Run one full interleaved sweep -- all `PASSES` passes, alternating
/// direction, `FOLD_REPS` reps per reading -- under one reset regime
/// (`reset_every`, see [`synthetic_ledger`]'s doc), printing its table.
/// Returns the swept depths and their medians (ascending by depth) for the
/// caller's fit and monotonicity check.
fn run_sweep(
    label: &str,
    reset_every: Option<usize>,
    registry: &ConceptRegistry,
) -> (Vec<f64>, Vec<f64>) {
    let entity = EntityId::new(1).expect("1 is nonzero");
    let home = room_for(0);
    let terrain = FlatTerrain;
    let class = ThermalStrategy::Endothermic;

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
            let ledger = synthetic_ledger(entity, depth, reset_every, registry);
            // One past the last posted day, so the fold walks every one of
            // this depth's `depth` segments.
            let t = WorldTime::from_std_days(depth as f64 + 1.0).expect("depth + 1 is finite");

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

    println!();
    println!("{label}:");
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
    (xs, ys)
}

fn main() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(AGENT_AT, false, "an agent's position on a day")
        .expect("AGENT_AT registers identically every run");
    registry
        .register_predicate(DRANK, false, "an agent satisfied its sustenance goal")
        .expect("DRANK registers identically every run");

    println!(
        "fold_depth_sweep: {} depths x {PASSES} passes (alternating direction) x {FOLD_REPS} reps/reading, two reset regimes",
        DEPTHS.len()
    );
    println!("depths swept: {DEPTHS:?}");

    let (periodic_xs, periodic_ys) = run_sweep(
        "PERIODIC RESETS (a drank fact every RESET_EVERY=20 postings -- S stays bounded, isolates O(H))",
        Some(RESET_EVERY),
        &registry,
    );
    report_affine(
        "PERIODIC -- drive_at (us/call) vs depth, S bounded near RESET_EVERY",
        "us/call",
        &periodic_xs,
        &periodic_ys,
        *periodic_xs.first().expect("DEPTHS is non-empty"),
        *periodic_xs.last().expect("DEPTHS is non-empty"),
    );
    // The model-free complement to the fitted elasticity above -- see the
    // module doc's reconciliation section for the numbers this produced on
    // one run and why the whole-range figure reads lower than the top-half
    // one.
    report_raw_elasticities(&periodic_xs, &periodic_ys);
    report_monotonicity(&periodic_ys);

    let (single_xs, single_ys) = run_sweep(
        "SINGLE EARLY RESET (one drank fact at day 0.0, never repeated -- S == H, exposes O(S*H))",
        None,
        &registry,
    );
    report_affine(
        "SINGLE-RESET -- drive_at (us/call) vs depth, S == H",
        "us/call",
        &single_xs,
        &single_ys,
        *single_xs.first().expect("DEPTHS is non-empty"),
        *single_xs.last().expect("DEPTHS is non-empty"),
    );
    report_raw_elasticities(&single_xs, &single_ys);
    report_monotonicity(&single_ys);
}
