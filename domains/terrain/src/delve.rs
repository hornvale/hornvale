//! The delve ladder — habitation depth, spaced by heat (spec §4.1).
//!
//! [`crate::BandKind`] keeps its five stratigraphic rungs and its entire
//! archival job (`Era`, `RockClass`, `unconformity`); it is not modified and
//! nothing here reads it. This is a **second, independent ladder**: its rungs
//! are placed at temperature offsets above the cell's surface datum, so a
//! rung's depth in metres is derived from
//! [`crate::strata::geothermal_gradient`] and therefore *varies by cell* — the
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
//! cave-bearing land cells.
//!
//! The first of those two tables is retained there as a before-arm and is
//! **not** the input to this table. It was taken while depth was a band index,
//! which put 62–75% of caves in `[0, 2)` K and 24–38% in `[50, ∞)` K with
//! 1–25 caves total in between; fitting to it would have re-derived the
//! two-class ladder Task 1b exists to remove.
//!
//! The boundaries below are the ones the probe **binned against**, which is
//! deliberate and is the whole of their justification: they are the only
//! thresholds in this campaign with a measured per-class occupancy on every
//! preregistered seed. A boundary moved to a rounder or more elegant number
//! would be a threshold whose occupancy nobody has looked at. Per-class share
//! of cave-bearing cells, seeds 42 / 7 / 1234:
//!
//! ```text
//! rung        ΔT (K)      seed 42        seed 7         seed 1234
//! Undercroft  [ 0,  2)     77  ( 8.8%)     84  ( 5.0%)     91  ( 7.2%)
//! Shallows    [ 2, 10)    149  (17.0%)    639  (38.0%)    162  (12.8%)
//! Deeps       [10, 25)    381  (43.6%)     81  ( 4.8%)    348  (27.5%)
//! Underdeep   [25, 50)     53  ( 6.1%)    150  ( 8.9%)    129  (10.2%)
//! Sunless     [50,  ∞)    214  (24.5%)    727  (43.2%)    536  (42.3%)
//! ```
//!
//! Every rung is occupied on every seed; the thinnest class holds 4.8% and the
//! fattest 43.6%. Five rungs sits inside §4.1's 4–6 band with headroom in both
//! directions. Each boundary's own basis is recorded on its constant.
//!
//! ## What this ladder is not
//!
//! [`rung_at_delta_t`] never returns [`DelveRung::Surface`]. Being at the
//! surface is a fact about not being in the rock column at all, not a fact
//! about a temperature — the variant exists (spec §4.6) so that the overworld
//! is a rung of the same ladder and no reader can mistake a `None` for it.

use crate::strata::GeothermalGradient;

/// A rung of the delve ladder — a habitation depth *class*, never a depth.
///
/// Ordered shallow → deep, so a **greater** rung is a **deeper** one, and
/// [`DelveRung::Surface`] is the least. The derived `Ord` is load-bearing in
/// two ways: it makes "further down the ladder" a comparison rather than a
/// convention, and it lets a rung serve as half of a `BTreeMap` key (the
/// workspace bans `HashMap`, so a map keyed by rung has no other option).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DelveRung {
    /// The overworld — above the rock column entirely, not a ΔT class.
    Surface,
    /// Cellars, cave mouths, the first few tens of metres of worked rock.
    Undercroft,
    /// Shallow inhabited depth: the top of the karst and lava-tube population.
    Shallows,
    /// The ladder's broad middle — a worked or walked depth, still temperate.
    Deeps,
    /// Deep habitation, warm enough that living here is a choice with a cost.
    Underdeep,
    /// Past the habitable ceiling: hot, open-ended, and the deepest class the
    /// measured cave population reaches.
    Sunless,
}

/// The ΔT (K above the surface datum) beyond which this campaign declares a
/// chamber uninhabitable, and therefore the ΔT at which [`DelveRung::Sunless`]
/// begins.
///
/// **Authored, not derived.** Spec §4.1 fixes this value in the spec *before*
/// any fit precisely so that no measurement can be read as having produced it:
/// 50 K puts a temperate cell's chamber near 60 °C, past sustained human
/// tolerance, and lands the ladder's floor at 1.7–3.3 km over the 15–30 K/km
/// gradient band — the same order as the deepest worked mines on Earth. It is
/// a fidelity choice and is recorded as one. The measured distribution is what
/// tells us the class is *occupied* (24.5 / 43.2 / 42.3% of cave-bearing cells
/// on seeds 42 / 7 / 1234); it did not choose the number.
/// type-audit: bare-ok(diagnostic-value)
pub const HABITABLE_CEILING_K: f64 = 50.0;

/// The ΔT at which [`DelveRung::Shallows`] begins.
///
/// Measured: this is the top of the gap between the shallow cluster and the
/// bulk. The post-Task-1b reading puts reach-p10 at 200.0 / 215.3 / 201.7 m
/// while `[0, 2)` K is, at the measured median gradients (24.4 / 25.0 / 23.1
/// K/km), everything above 80–87 m — so the class isolates a genuinely
/// separate sub-p10 population (8.8 / 5.0 / 7.2% of cave-bearing cells) rather
/// than slicing the body of the distribution.
/// type-audit: bare-ok(diagnostic-value)
const SHALLOWS_TOP_K: f64 = 2.0;

/// The ΔT at which [`DelveRung::Deeps`] begins.
///
/// Measured: the `[2, 10)` class holds 17.0 / 38.0 / 12.8% of cave-bearing
/// cells, and the ΔT-p10 of all three seeds (5.581 / 5.870 / 5.765 K) lands
/// inside it — a 0.29 K spread across three independent worlds, the tightest
/// seed-agreement anywhere in the reading, so the class is anchored on a real
/// feature of the population rather than on an arbitrary cut.
/// type-audit: bare-ok(diagnostic-value)
const DEEPS_TOP_K: f64 = 10.0;

/// The ΔT at which [`DelveRung::Underdeep`] begins.
///
/// Measured: `[25, 50)` holds 6.1 / 8.9 / 10.2% of cave-bearing cells — a
/// 4.1-percentage-point spread, the most seed-stable share in the table
/// (`Deeps` swings 38.8 points across the same three seeds). It is also the
/// only interior threshold between [`DEEPS_TOP_K`] and the authored
/// [`HABITABLE_CEILING_K`] with a measured occupancy at all.
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
const LADDER: [(DelveRung, f64); 5] = [
    (DelveRung::Undercroft, 0.0),
    (DelveRung::Shallows, SHALLOWS_TOP_K),
    (DelveRung::Deeps, DEEPS_TOP_K),
    (DelveRung::Underdeep, UNDERDEEP_TOP_K),
    (DelveRung::Sunless, HABITABLE_CEILING_K),
];

/// Every rung, `Surface` first and then the habitation rungs shallowest to
/// deepest — derived from [`LADDER`] in the same declaration so the two can
/// never disagree about which rungs exist or in what order.
const ALL_RUNGS: [DelveRung; 6] = [
    DelveRung::Surface,
    LADDER[0].0,
    LADDER[1].0,
    LADDER[2].0,
    LADDER[3].0,
    LADDER[4].0,
];

/// Every rung of the ladder in order, [`DelveRung::Surface`] first.
///
/// Callers that want only the habitation rungs filter `Surface` out; it is
/// deliberately present so that iterating "the ladder" never silently omits
/// the overworld.
pub fn rungs() -> &'static [DelveRung] {
    &ALL_RUNGS
}

/// The half-open ΔT interval a rung covers, in K above the surface datum:
/// `(low, Some(high))`, or `(low, None)` for the open-ended bottom rung.
///
/// [`DelveRung::Surface`] is not a ΔT class at all, so it returns `(0.0,
/// Some(0.0))` — the empty interval `[0, 0)`, which contains nothing and
/// therefore cannot claim a temperature away from [`DelveRung::Undercroft`].
/// type-audit: bare-ok(diagnostic-value: return)
pub fn delta_t_range_of(rung: DelveRung) -> (f64, Option<f64>) {
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
/// which a cold-season or high-albedo cell can genuinely produce — and a
/// non-finite ΔT both resolve to the top habitation rung rather than
/// panicking, because the ladder is a classification of places and every place
/// is somewhere. Never returns [`DelveRung::Surface`]; see the module docs.
/// type-audit: bare-ok(diagnostic-value: delta_t_k)
pub fn rung_at_delta_t(delta_t_k: f64) -> DelveRung {
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

/// The rung a depth below the surface falls in, for a cell with this
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
pub fn rung_at_depth(depth_m: f64, gradient: GeothermalGradient) -> DelveRung {
    rung_at_delta_t(gradient.get() * (depth_m / 1000.0))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::strata::GeothermalGradient;

    #[test]
    fn the_ladder_is_within_the_specs_bound() {
        // Spec §4.1: at least 4 and at most 6 habitation rungs.
        let habitation = rungs().iter().filter(|r| **r != DelveRung::Surface).count();
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
        let habitation: Vec<DelveRung> = rungs()
            .iter()
            .copied()
            .filter(|r| *r != DelveRung::Surface)
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
        for rung in rungs().iter().copied().filter(|r| *r != DelveRung::Surface) {
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
            .find(|r| *r != DelveRung::Surface)
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
            Some(DelveRung::Surface),
            "Surface is the least rung"
        );
    }

    /// `Surface` is a rung of this ladder, not an absence of one (spec §4.6),
    /// but it is not a temperature class: no ΔT may resolve to it, and its
    /// declared interval must be empty so it cannot be read as owning 0 K.
    #[test]
    fn surface_owns_no_temperature() {
        assert_eq!(delta_t_range_of(DelveRung::Surface), (0.0, Some(0.0)));
        for delta_t in [-1.0, 0.0, 1.0, 25.0, 50.0, 1_000.0, f64::NAN] {
            assert_ne!(
                rung_at_delta_t(delta_t),
                DelveRung::Surface,
                "ΔT {delta_t} resolved to Surface"
            );
        }
    }

    /// A rung is a class of *place*, so the depth it occupies must move with
    /// the cell's gradient rather than being a fixed metre band. Asserted as
    /// an inequality on the boundary depth, not on a rung lookup, so it says
    /// something about the mapping itself.
    #[test]
    fn a_rungs_depth_in_metres_varies_by_cell() {
        let (deeps_low, _) = delta_t_range_of(DelveRung::Deeps);
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
            DelveRung::Shallows
        );
        assert_eq!(rung_at_depth(depth_under(craton), craton), DelveRung::Deeps);
    }
}
