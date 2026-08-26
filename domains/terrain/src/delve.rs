//! The delve ladder — habitation depth, spaced by heat (spec §4.1).
//!
//! [`crate::Horizon`] keeps its five stratigraphic rungs and its entire
//! archival job (`Era`, `RockClass`, `unconformity`); it is not modified and
//! nothing here reads it. This is a **second, independent ladder**: its rungs
//! are placed at temperature offsets above the vertex's surface datum, so a
//! rung's depth in metres is derived from
//! [`crate::strata::geothermal_gradient`] and therefore *varies by vertex* — the
//! same rung sits twice as deep under an ancient craton (15 K/km) as under
//! young thin crust (30 K/km). Neither ladder derives the other.
//!
//! Pure arithmetic over one shipped field: no draws, no facts, no state.
//!
//! ## Where the rung table came from
//!
//! Spec §4.1 froze the *principle* and its bounds before any measurement —
//! contiguous ordered rungs defined by ΔT thresholds, a top rung beginning at
//! ΔT = 0, an open-ended bottom rung, at least 4 and at most 6 habitation
//! rungs, and an authored habitable ceiling at 50 K (see
//! [`HABITABLE_CEILING_K`]). Everything else is fitted here, to the
//! **post-Task-1b** reading recorded in
//! `windows/worldgen/tests/underworld_ladder_probe.rs`'s module doc — the
//! second of that file's two tables, taken after a cave's depth became a
//! budget in metres (spec §4.0). Seeds 42 / 7 / 1234; 874 / 1681 / 1266
//! cave-bearing land vertices.
//!
//! The first of those two tables is retained there as a before-arm and is
//! **not** the input to this table. It was taken while depth was a band index,
//! which put 62–75% of caves in `[0, 2)` K and 24–38% in `[50, ∞)` K with
//! 1–25 caves total in between; fitting to it would have re-derived the
//! two-class ladder Task 1b exists to remove.
//!
//! Three of the four interior boundaries are the ones the probe **binned
//! against** — §4.1 published them as an illustration *before* Task 1 ran, so
//! adopting them is reading an a-priori bin, not fitting an edge and calling
//! it a measurement. They are also the only thresholds with a measured
//! per-class occupancy on every preregistered seed.
//!
//! **The fourth moved after the fact, and that is stated rather than
//! smuggled.** `Deeps` began at 10 K in the first landing of this file. A
//! finer re-bin (`how_lumpy_is_the_delta_t_distribution`, added to the same
//! probe) showed the reach distribution carries **atoms** — `cave_depth_reach_m`
//! clamps at `LAVATUBE_CEILING_M = 200.0` and `CAVE_REACH_CEILING_M = 3000.0`,
//! and a paleokarst arm clamps too — which ΔT inherits scaled by a gradient
//! that only spans 1.27×. So the distribution is a row of spikes, not a
//! spread, and 10 K sat on the lower lip of the largest one: the single 1 K
//! bin `[10, 11)` holds **39.9% of seed 42**, and 19.6% of that seed lies
//! within ±0.5 K of the edge. See [`DEEPS_TOP_K`] for the move and its rule.
//!
//! Per-class share of cave-bearing vertices, seeds 42 / 7 / 1234:
//!
//! ```text
//! rung        ΔT (K)      seed 42        seed 7         seed 1234
//! Undercroft  [ 0,  2)     77  ( 8.8%)     84  ( 5.0%)     91  ( 7.2%)
//! Shallows    [ 2,  8)    131  (15.0%)    599  (35.6%)    144  (11.4%)
//! Deeps       [ 8, 25)    399  (45.7%)    121  ( 7.2%)    366  (28.9%)
//! Underdeep   [25, 50)     53  ( 6.1%)    150  ( 8.9%)    129  (10.2%)
//! Nadir       [50,  ∞)    214  (24.5%)    727  (43.2%)    536  (42.3%)
//! ```
//!
//! Every rung is occupied on every seed; the thinnest class holds 5.0% and the
//! fattest 45.7%. Five rungs sits inside §4.1's 4–6 band with headroom in both
//! directions. Each boundary's own basis is recorded on its constant.
//!
//! ## How stable each edge is, measured
//!
//! Share of each seed's caves within **±0.5 K** of the edge — the direct
//! answer to "would a small move migrate a large population across this?":
//!
//! ```text
//! edge      seed 42   seed 7   seed 1234   verdict
//!  2 K        0.0%     0.0%      0.2%      in the empty valley [2,5)
//!  8 K        0.0%     0.0%      0.0%      in the empty valley [7,9)
//! 25 K        0.6%     1.1%      0.2%      sparse; no mode nearby
//! 50 K        0.0%     0.4%      1.5%      AUTHORED — see HABITABLE_CEILING_K
//! (10 K       19.6%     1.8%      6.5%      the edge that was moved)
//! ```
//!
//! **50 K is the least well-placed edge in the table and cannot be moved.**
//! It is authored, frozen before the fit, and seed 1234 puts 1.5% of its caves
//! within half a kelvin of it; the 1 K bins immediately below it are occupied
//! on two of three seeds (seed 7: 48:15, 49:16; seed 1234: 49:5). That is an
//! accepted cost of the ceiling being a fidelity choice rather than a measured
//! one, and it is recorded here so no later reader mistakes the silence for
//! stability.
//!
//! ## What this ladder is not
//!
//! [`rung_at_delta_t`] never returns [`Band::Surface`]. Being at the
//! surface is a fact about not being in the rock column at all, not a fact
//! about a temperature — the variant exists (spec §4.6) so that the overworld
//! is a rung of the same ladder and no reader can mistake a `None` for it.
//!
//! ## Nadir: a refused split, and its true bound
//!
//! [`Band::Nadir`] is the open-ended leftover bin, and leftover bins are
//! large by construction: 38.65% of cave-bearing land vertices terminate here
//! (24.49 / 43.25 / 42.34% on seeds 42 / 7 / 1234), which falsified the
//! campaign's own prediction that reaching it would be uncommon.
//!
//! A sixth rung splitting this one was measured and **refused**: 99% of the
//! whole super-50 K population lives in `[50, 61)` K, so there is nowhere
//! stable to cut (amendment B.2).
//!
//! **Open-ended formally, bounded empirically.** No upper threshold exists
//! here and none should — a ladder needs a class that cannot overflow. But no
//! *cave* can reach far into it: reach is capped at
//! [`crate::CAVE_REACH_CEILING_M`] (3000 m) and the gradient is clamped to
//! 15–30 K/km, so a cave's ΔT cannot exceed 90 K and measures ~[50, 68] K in
//! practice, unreachable at all below 16.7 K/km. Read "open-ended" as a
//! property of the ladder, not as a claim about how hot a chamber gets.
//!
//! ## Where the type itself lives
//!
//! [`hornvale_kernel::Band`] holds the roster and the ordering (decision 0044
//! clause (a); see that type's own docs for why) and is imported directly —
//! this module used to re-export it under a second name, but The Drift
//! deleted that alias (spec `2026-08-23-the-drift-design.md` §3.3) so the
//! depth axis carries one public name rather than two. This module keeps the
//! roster's *meaning* — the derivation from a ΔT to a band, and everything
//! measured above — which is what a domain keeps under 0044's other half.

use crate::strata::GeothermalGradient;
use hornvale_kernel::Band;

/// The ΔT (K above the surface datum) beyond which this campaign declares a
/// chamber uninhabitable, and therefore the ΔT at which [`Band::Nadir`]
/// begins.
///
/// **Authored, not derived.** Spec §4.1 fixes this value in the spec *before*
/// any fit precisely so that no measurement can be read as having produced it:
/// 50 K puts a temperate vertex's chamber near 60 °C, past sustained human
/// tolerance, and lands the ladder's floor at 1.7–3.3 km over the 15–30 K/km
/// gradient band — the same order as the deepest worked mines on Earth. It is
/// a fidelity choice and is recorded as one. The measured distribution is what
/// tells us the class is *occupied* (24.5 / 43.2 / 42.3% of cave-bearing vertices
/// on seeds 42 / 7 / 1234); it did not choose the number.
/// type-audit: bare-ok(diagnostic-value)
pub const HABITABLE_CEILING_K: f64 = 50.0;

/// The ΔT at which [`Band::Shallows`] begins.
///
/// **An a-priori bin, kept.** Spec §4.1 published `< 2 K` as its illustrative
/// top rung before Task 1 ran, the probe binned against it, and it is one of
/// the four thresholds with a measured per-class occupancy — 8.8 / 5.0 / 7.2%
/// below it on seeds 42 / 7 / 1234. That is the whole claim.
///
/// The finer re-bin is consistent with it and is why it was not moved: the
/// 1 K bins `[2, 5)` are **empty on all three seeds**, so the edge sits in a
/// measured valley (0.0 / 0.0 / 0.2% of caves within ±0.5 K). It separates a
/// shallow cluster from the bulk, which is what §4.1 guessed it would.
/// type-audit: bare-ok(diagnostic-value)
const SHALLOWS_TOP_K: f64 = 2.0;

/// The ΔT at which [`Band::Deeps`] begins.
///
/// **This edge was MOVED after seeing finer data — 10.0 → 8.0 — and it is the
/// only one in the table that was.** Recorded here rather than in a commit
/// message alone, because a post-hoc boundary move is legitimate exactly when
/// it is stated and illegitimate when it is not.
///
/// *What was wrong:* 10 K was §4.1's a-priori bin and the coarse table gave no
/// reason to doubt it — the `[2, 10)` class held 17.0 / 38.0 / 12.8% and the
/// ΔT-p10 of all three seeds landed inside it. A coarse histogram cannot
/// distinguish an edge that separates two populations from one that sits on
/// the lip of a spike, and this was the second: the single 1 K bin `[10, 11)`
/// holds **349 of seed 42's 874 caves (39.9%)**, seed 42's p25 is 9.973 and
/// its p50 is 10.735, and **19.6% of that seed lies within ±0.5 K of 10.0**.
/// Nudging the edge to 10.8 would have moved roughly a quarter of the seed
/// from `Deeps` to `Shallows`. That is the least stable place an edge can be.
///
/// *Why the distribution is spiky at all, which is the part worth carrying:*
/// [`crate::cave_depth::cave_depth_reach_m`] has hard clamps —
/// `LAVATUBE_CEILING_M = 200.0`, [`crate::CAVE_REACH_CEILING_M`] `= 3000.0`,
/// and the paleokarst arm clamps too — so reach has **atoms** (seed 42's
/// single fattest reach value covers 23.1% of its caves). ΔT is reach times a
/// gradient spanning only 1.27×, so it inherits them as smears, not as a
/// spread.
///
/// *The rule applied:* place the edge inside a **valley** — a 1 K bin range
/// empty on all three seeds — rather than at a percentile or a round number.
/// `[7, 9)` is such a valley; 8.0 is its interior and measures **0.0% within
/// ±0.5 K on every seed**. The rule was applied to this edge and not to the
/// others because no other edge sat in a mode (next worst is 25 K at 1.1%);
/// moving an edge nothing condemns, after unblinding, would be the
/// metric-chasing this move is not.
///
/// *Effect on the table:* `Deeps` gains the `[8, 10)` caves from `Shallows` —
/// 18 / 40 / 18 vertices — so its thinnest seed goes 4.8% → 7.2% and the table's
/// minimum class 4.8% → 5.0%. The rung names, arity and ordering are
/// unchanged, so `chamber/v2`'s key spellings do not move with it.
/// type-audit: bare-ok(diagnostic-value)
const DEEPS_TOP_K: f64 = 8.0;

/// The ΔT at which [`Band::Underdeep`] begins.
///
/// **An a-priori bin, kept.** Spec §4.1 published `25 – 50 K` before Task 1
/// ran, the probe binned against it, and the class holds 6.1 / 8.9 / 10.2% of
/// cave-bearing vertices on seeds 42 / 7 / 1234 — occupied on every seed, which
/// is the claim.
///
/// It is not in a valley the way [`SHALLOWS_TOP_K`] and [`DEEPS_TOP_K`] are:
/// 0.6 / 1.1 / 0.2% of caves lie within ±0.5 K of it, against 0.0% for those
/// two. It is kept because that is sparse rather than modal — an order of
/// magnitude below the 19.6% that condemned the old `Deeps` edge — and moving
/// an unflagged edge after unblinding buys a marginal improvement at the cost
/// of the discipline. `[18, 22)` is the nearest empty valley if a later
/// campaign has a reason to revisit it.
/// type-audit: bare-ok(diagnostic-value)
const UNDERDEEP_TOP_K: f64 = 25.0;

/// The habitation rungs, shallowest first, each paired with the ΔT (K above
/// the surface datum) at which it begins.
///
/// **The single source of truth for the ladder's shape.** [`rungs`],
/// [`rung_at_delta_t`] and [`delta_t_range_of`] all read this one array, so
/// the tiling invariant — contiguous, ordered, no gap, no overlap — holds by
/// construction rather than by three hand-written tables agreeing. The first
/// entry begins at 0.0 (spec §4.1's frozen "the top habitation rung begins at
/// ΔT = 0"); the last has no successor, which is what makes the bottom rung
/// open-ended.
const LADDER: [(Band, f64); 5] = [
    (Band::Undercroft, 0.0),
    (Band::Shallows, SHALLOWS_TOP_K),
    (Band::Deeps, DEEPS_TOP_K),
    (Band::Underdeep, UNDERDEEP_TOP_K),
    (Band::Nadir, HABITABLE_CEILING_K),
];

/// Every rung, `Surface` first and then the habitation rungs shallowest to
/// deepest — derived from [`LADDER`] in the same declaration so the two can
/// never disagree about which rungs exist or in what order.
const ALL_RUNGS: [Band; 6] = [
    Band::Surface,
    LADDER[0].0,
    LADDER[1].0,
    LADDER[2].0,
    LADDER[3].0,
    LADDER[4].0,
];

/// Every rung of the ladder in order, [`Band::Surface`] first.
///
/// Callers that want only the habitation rungs filter `Surface` out; it is
/// deliberately present so that iterating "the ladder" never silently omits
/// the overworld.
pub fn rungs() -> &'static [Band] {
    &ALL_RUNGS
}

/// The half-open ΔT interval a rung covers, in K above the surface datum:
/// `(low, Some(high))`, or `(low, None)` for the open-ended bottom rung.
///
/// [`Band::Surface`] is not a ΔT class at all, so it returns `(0.0,
/// Some(0.0))` — the empty interval `[0, 0)`, which contains nothing and
/// therefore cannot claim a temperature away from [`Band::Undercroft`].
/// type-audit: bare-ok(diagnostic-value: return)
pub fn delta_t_range_of(rung: Band) -> (f64, Option<f64>) {
    match LADDER.iter().position(|&(r, _)| r == rung) {
        // Surface: the empty interval at the datum. See this function's docs.
        None => (0.0, Some(0.0)),
        Some(index) => (
            LADDER[index].1,
            LADDER.get(index + 1).map(|&(_, next_low)| next_low),
        ),
    }
}

/// The rung a ΔT (K above the surface datum) falls in.
///
/// **Total.** A negative ΔT — a surface datum warmer than the rock beneath it,
/// which a cold-season or high-albedo vertex can genuinely produce — and a
/// non-finite ΔT both resolve to the top habitation rung rather than
/// panicking, because the ladder is a classification of places and every place
/// is somewhere. Never returns [`Band::Surface`]; see the module docs.
/// type-audit: bare-ok(diagnostic-value: delta_t_k)
pub fn rung_at_delta_t(delta_t_k: f64) -> Band {
    let mut found = LADDER[0].0;
    for &(rung, low) in LADDER.iter() {
        // NaN fails every comparison, so a non-finite ΔT keeps the initial
        // (top) rung — the total behaviour this function's docs promise.
        if delta_t_k >= low {
            found = rung;
        }
    }
    found
}

/// The rung a depth below the surface falls in, for a vertex with this
/// geothermal gradient.
///
/// ΔT = gradient × depth, the same expression
/// `windows/worldgen/tests/underworld_ladder_probe.rs` measured the
/// distribution with, so the table this ladder was fitted to and the function
/// that reads it cannot disagree about the coordinate. This is the whole point
/// of spacing by heat: at 1 km down a 15 K/km craton is 15 K above its datum
/// and a 30 K/km young crust is 30 K above its own, so the *same depth* is two
/// different rungs and the *same rung* is two different depths.
/// type-audit: bare-ok(ratio: depth_m)
pub fn rung_at_depth(depth_m: f64, gradient: GeothermalGradient) -> Band {
    rung_at_delta_t(gradient.get() * (depth_m / 1000.0))
}

/// The depth at which a rung's conditions are read, metres below the
/// surface — the **ΔT midpoint** of the rung's band, converted through the
/// vertex's own gradient, and never deeper than the cave actually reaches.
///
/// **The midpoint, not the top, and this is `MAP-per-rung-substrate`'s own
/// prescription** rather than a fresh choice: the top of a rung makes its
/// shallowest rank degenerate, because a rung's top ΔT is the next rung's
/// bottom.
///
/// [`Band::Nadir`] has no midpoint — [`delta_t_range_of`] gives it an open
/// top — so it reads `depth_reach_m`. **This is exactly `depth_reach_m`
/// only for the domain this function's one real producer emits**:
/// [`crate::cave_depth::cave_depth_reach_m`] (`cave_depth.rs:202-220`)
/// clamps every reach it returns to `[0.0, CAVE_REACH_CEILING_M]`
/// ([`crate::cave_depth::CAVE_REACH_CEILING_M`] = 3000 m), so every
/// `depth_reach_m` a real caller passes is finite and non-negative, and on
/// that domain Nadir's answer is exactly `depth_reach_m` — where EVERY rung
/// was read before per-rung resolution existed, which makes `Nadir` the
/// positive control for the change: its answer must not move.
///
/// **The guarantee is not total, and no code here makes it one.** A
/// negative `depth_reach_m` hits the trailing `.max(0.0)` below and
/// returns `Some(0.0)`, not the reach it was given. A `depth_reach_m` of
/// `f64::INFINITY` returns `Some(f64::INFINITY)` unchanged — `.max`
/// sanitizes `NaN` via its fixed non-NaN partner but has no such partner
/// for `+Infinity`. Neither input is reachable today (the one real
/// producer's clamp rules both out), so neither is defended against at
/// runtime — that would be paying rent for a caller that does not exist.
/// The precondition is instead named in a debug-only assertion below, the
/// same role `debug_assert!` plays on [`GeothermalGradient::new`]:
/// documentation a test build enforces, not a guard a release build pays
/// for.
///
/// [`Band::Surface`] names no chamber and returns `None`.
/// type-audit: bare-ok(diagnostic-value: depth_reach_m), bare-ok(diagnostic-value: return)
pub fn rung_evaluation_depth_m(
    rung: Band,
    gradient: GeothermalGradient,
    depth_reach_m: f64,
) -> Option<f64> {
    // Documents the domain the guarantee above actually covers; see the
    // doc comment. `cave_depth_reach_m` is the one real producer and it
    // never emits outside this range, so this never fires in production.
    debug_assert!(
        depth_reach_m.is_finite() && depth_reach_m >= 0.0,
        "rung_evaluation_depth_m's Nadir guarantee (== depth_reach_m) holds only for a \
         finite, non-negative depth_reach_m; got {depth_reach_m}"
    );
    if rung == Band::Surface {
        return None;
    }
    let (lo, hi) = delta_t_range_of(rung);
    let depth = match hi {
        // The open-ended bottom rung: read the column where it actually ends.
        None => depth_reach_m,
        Some(hi) => {
            let midpoint_k = 0.5 * (lo + hi);
            1000.0 * midpoint_k / gradient.get()
        }
    };
    Some(depth.min(depth_reach_m).max(0.0))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::GeothermalGradient;

    #[test]
    fn the_ladder_is_within_the_specs_bound() {
        // Spec §4.1: at least 4 and at most 6 habitation rungs.
        let habitation = rungs().iter().filter(|r| **r != Band::Surface).count();
        assert!(
            (4..=6).contains(&habitation),
            "habitation rungs = {habitation}, spec §4.1 allows 4..=6"
        );
    }

    #[test]
    fn the_rungs_tile_the_line_with_no_gap_and_no_overlap() {
        // Every ΔT >= 0 resolves to exactly one rung, and consecutive rungs
        // share a boundary. This is the invariant that a hand-written
        // threshold table gets wrong.
        let habitation: Vec<Band> = rungs()
            .iter()
            .copied()
            .filter(|r| *r != Band::Surface)
            .collect();
        let (first_lo, _) = delta_t_range_of(habitation[0]);
        assert_eq!(first_lo, 0.0, "the top habitation rung must begin at 0 K");
        for pair in habitation.windows(2) {
            let (_, upper_hi) = delta_t_range_of(pair[0]);
            let (lower_lo, _) = delta_t_range_of(pair[1]);
            assert_eq!(
                upper_hi,
                Some(lower_lo),
                "{:?} must end exactly where {:?} begins",
                pair[0],
                pair[1]
            );
        }
        let (_, last_hi) = delta_t_range_of(*habitation.last().unwrap());
        assert_eq!(last_hi, None, "the bottom rung is open-ended");
    }

    #[test]
    fn rung_at_delta_t_agrees_with_the_declared_ranges() {
        for rung in rungs().iter().copied().filter(|r| *r != Band::Surface) {
            let (lo, hi) = delta_t_range_of(rung);
            // A point just inside the low edge belongs to this rung.
            assert_eq!(rung_at_delta_t(lo), rung, "low edge of {rung:?}");
            if let Some(hi) = hi {
                // A point just below the high edge still belongs to it.
                assert_eq!(rung_at_delta_t(hi - 1e-9), rung, "high edge of {rung:?}");
                // The high edge itself belongs to the NEXT rung.
                assert_ne!(rung_at_delta_t(hi), rung, "{rung:?} must be half-open");
            }
        }
    }

    #[test]
    fn at_one_depth_a_hotter_gradient_sits_further_down_the_ladder() {
        // The campaign's whole point: a rung is a place-type, not a depth.
        // (The plan named this `the_same_rung_sits_deeper_under_a_cooler_
        // gradient`, which is the same statement transposed; the assertion is
        // the one written here, so the name is too.)
        let cool = GeothermalGradient::new(15.0);
        let hot = GeothermalGradient::new(30.0);
        let depth_m = 1000.0;
        let under_cool = rung_at_depth(depth_m, cool);
        let under_hot = rung_at_depth(depth_m, hot);
        assert!(
            under_hot >= under_cool,
            "at one depth the hotter gradient must be at or below the cooler \
             one on the ladder: cool={under_cool:?} hot={under_hot:?}"
        );
    }

    #[test]
    fn the_habitable_ceiling_is_the_authored_value() {
        assert_eq!(HABITABLE_CEILING_K, 50.0);
    }

    #[test]
    fn a_negative_or_nonfinite_delta_t_resolves_to_the_top_rung() {
        // Total, not panicking: a surface datum warmer than the rock is a
        // physical possibility the ladder must absorb rather than reject.
        let top = rungs()
            .iter()
            .copied()
            .find(|r| *r != Band::Surface)
            .unwrap();
        assert_eq!(rung_at_delta_t(-5.0), top);
        assert_eq!(rung_at_delta_t(f64::NAN), top);
    }

    /// The ladder is ordered shallow → deep and `Surface` is the least, so
    /// `Ord` means "deeper" rather than "declared later by accident". Task 8
    /// keys a `BTreeMap` on this; a reversed or partial order there would be
    /// a silent mis-ranking, not a compile error.
    #[test]
    fn the_derived_order_runs_shallow_to_deep() {
        let ladder = rungs();
        for pair in ladder.windows(2) {
            assert!(
                pair[0] < pair[1],
                "{:?} must sort above {:?}",
                pair[0],
                pair[1]
            );
        }
        assert_eq!(
            ladder.first().copied(),
            Some(Band::Surface),
            "Surface is the least rung"
        );
    }

    /// `Surface` is a rung of this ladder, not an absence of one (spec §4.6),
    /// but it is not a temperature class: no ΔT may resolve to it, and its
    /// declared interval must be empty so it cannot be read as owning 0 K.
    #[test]
    fn surface_owns_no_temperature() {
        assert_eq!(delta_t_range_of(Band::Surface), (0.0, Some(0.0)));
        for delta_t in [-1.0, 0.0, 1.0, 25.0, 50.0, 1_000.0, f64::NAN] {
            assert_ne!(
                rung_at_delta_t(delta_t),
                Band::Surface,
                "ΔT {delta_t} resolved to Surface"
            );
        }
    }

    /// A rung is a class of *place*, so the depth it occupies must move with
    /// the vertex's gradient rather than being a fixed metre band. Asserted as
    /// an inequality on the boundary depth, not on a rung lookup, so it says
    /// something about the mapping itself.
    #[test]
    fn a_rungs_depth_in_metres_varies_by_vertex() {
        let (deeps_low, _) = delta_t_range_of(Band::Deeps);
        let craton = GeothermalGradient::new(15.0);
        let young = GeothermalGradient::new(30.0);
        let depth_under = |g: GeothermalGradient| deeps_low / g.get() * 1000.0;
        assert!(
            depth_under(craton) > depth_under(young),
            "the Deeps must begin deeper under a cooler gradient: craton={} m, \
             young crust={} m",
            depth_under(craton),
            depth_under(young)
        );
        // And the mapping agrees with the boundary: just below the craton's
        // Deeps top is still Shallows, just at it is Deeps.
        assert_eq!(
            rung_at_depth(depth_under(craton) - 1.0, craton),
            Band::Shallows
        );
        assert_eq!(rung_at_depth(depth_under(craton), craton), Band::Deeps);
    }

    #[test]
    fn the_surface_rung_has_no_evaluation_depth() {
        let g = GeothermalGradient::new(25.0);
        assert_eq!(rung_evaluation_depth_m(Band::Surface, g, 800.0), None);
    }

    #[test]
    fn nadir_is_evaluated_at_the_caves_own_reach() {
        // THE POSITIVE CONTROL FOR THE WHOLE CAMPAIGN. Today every rung reads
        // `depth_reach_m`; after the per-rung change the deepest rung still
        // must, so its substrate and moisture are byte-identical across the
        // change and every movement is attributable to a shallower rung.
        let g = GeothermalGradient::new(25.0);
        for reach in [120.0, 800.0, 2500.0] {
            assert_eq!(
                rung_evaluation_depth_m(Band::Nadir, g, reach),
                Some(reach),
                "Nadir must read the cave's own reach, not a midpoint"
            );
        }
    }

    #[test]
    fn a_bounded_rung_is_evaluated_at_its_delta_t_midpoint() {
        let g = GeothermalGradient::new(25.0);
        // `rungs()` INCLUDES `Band::Surface` (it returns ALL_RUNGS, Surface
        // first) and Surface's range is the degenerate `(0.0, Some(0.0))`, so
        // it survives the `hi` filter below and would then panic on the
        // `expect`. Filter it explicitly.
        for rung in rungs().iter().filter(|r| **r != Band::Surface) {
            let (lo, hi) = delta_t_range_of(*rung);
            let Some(hi) = hi else { continue }; // Nadir, covered above
            let depth = rung_evaluation_depth_m(*rung, g, 100_000.0)
                .expect("a habitation rung has an evaluation depth");
            let delta_t = depth * g.get() / 1000.0;
            assert!(
                delta_t > lo && delta_t < hi,
                "{rung:?}: evaluation ΔT {delta_t} is not strictly inside ({lo}, {hi}) \
                 — the TOP of a rung is what MAP-per-rung-substrate says makes \
                 rank 0 degenerate"
            );
        }
    }

    #[test]
    fn evaluation_depth_never_exceeds_the_caves_reach() {
        // A rung deeper than the cave goes is not a place. Whatever the ΔT
        // midpoint says, the answer is bounded by the column that exists.
        let g = GeothermalGradient::new(25.0);
        for rung in rungs() {
            if let Some(d) = rung_evaluation_depth_m(*rung, g, 150.0) {
                assert!(d <= 150.0, "{rung:?} evaluated at {d} m in a 150 m column");
            }
        }
    }

    #[test]
    fn evaluation_depth_is_monotone_in_the_rung() {
        let g = GeothermalGradient::new(25.0);
        let depths: Vec<f64> = rungs()
            .iter()
            .filter_map(|r| rung_evaluation_depth_m(*r, g, 100_000.0))
            .collect();
        for w in depths.windows(2) {
            assert!(
                w[0] < w[1],
                "rung depths must increase with the ladder: {depths:?}"
            );
        }
    }
}
