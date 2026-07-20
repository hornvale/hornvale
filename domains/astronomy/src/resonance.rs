//! LANG-48: proportional reasoning over already-witnessed quantities — a
//! culture's own account can now hold whether two moons' real periods sit
//! in a clean small-integer ratio. This module is the pure detection half
//! (no draws, no ledger access); see
//! `docs/superpowers/specs/2026-07-20-the-consonance-design.md`.
#![warn(missing_docs)]

use crate::StdDays;

/// A detected clean small-integer ratio between two of a world's moons'
/// real orbital periods (spec §3.1): `ratio` is the ACTUAL measured value
/// (e.g. `1.987`, not the idealized `2.0`) — ground truth, never replaced
/// by the idealized rational it happens to sit near. `numerator` and
/// `denominator` name the matched candidate (always `numerator >
/// denominator`, so `ratio` is always expressed slower:faster, >= 1.0).
/// type-audit: bare-ok(ratio: ratio), bare-ok(count: numerator), bare-ok(count: denominator)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MoonPeriodRatio {
    /// The actual measured period ratio (slower period / faster period).
    pub ratio: f64,
    /// The matched candidate's numerator (the slower side).
    pub numerator: u32,
    /// The matched candidate's denominator (the faster side).
    pub denominator: u32,
}

/// The closed set of low-order rational candidates a ratio is checked
/// against, each `(numerator, denominator)` with `numerator >=
/// denominator`. Deliberately small — real archaeoastronomy's noticed
/// ratios (the saros, the Metonic cycle) are low-order; a culture that
/// could notice a 47:31 relationship is not what this campaign models.
const RATIO_CANDIDATES: &[(u32, u32)] = &[
    (1, 1),
    (2, 1),
    (3, 1),
    (3, 2),
    (4, 1),
    (4, 3),
    (5, 1),
    (5, 2),
    (5, 3),
    (5, 4),
];

/// How close a measured ratio must sit to a candidate (relative
/// deviation) to count as a match. 5%: tight enough that "close to 2:1"
/// means genuinely close, loose enough that real drawn periods (never
/// exactly integer multiples) can still match.
const RATIO_TOLERANCE: f64 = 0.05;

/// The nearest candidate to `ratio` (always `ratio >= 1.0`) and its
/// relative deviation from that candidate's own value — always returns
/// a candidate (the list is fixed and non-empty), the caller decides
/// whether the deviation is within [`RATIO_TOLERANCE`].
fn nearest_candidate(ratio: f64) -> ((u32, u32), f64) {
    RATIO_CANDIDATES
        .iter()
        .map(|&(n, d)| {
            let candidate_value = n as f64 / d as f64;
            let deviation = (ratio - candidate_value).abs() / candidate_value;
            ((n, d), deviation)
        })
        .min_by(|a, b| a.1.total_cmp(&b.1))
        .expect("RATIO_CANDIDATES is a fixed non-empty slice")
}

/// Detect the single best clean small-integer ratio among every pair of
/// `periods` (spec §3.1). Zero pairs for 0-1 moons (returns `None`
/// immediately); every pair checked for 2+ moons. Among all matching
/// pairs (relative deviation within [`RATIO_TOLERANCE`] of their nearest
/// candidate), the single survivor is chosen by (deviation ascending,
/// simplicity — `max(numerator, denominator)` — ascending, pair index
/// ascending) for full determinism. Pure: no `Seed`/`Stream` use, no
/// ledger access.
pub fn detect_moon_period_ratio(periods: &[StdDays]) -> Option<MoonPeriodRatio> {
    let mut best: Option<(MoonPeriodRatio, f64, u32)> = None;
    for i in 0..periods.len() {
        for j in (i + 1)..periods.len() {
            let p_i = periods[i].get();
            let p_j = periods[j].get();
            let (slower, faster) = if p_i >= p_j { (p_i, p_j) } else { (p_j, p_i) };
            let measured_ratio = slower / faster;
            let ((numerator, denominator), deviation) = nearest_candidate(measured_ratio);
            if deviation > RATIO_TOLERANCE {
                continue;
            }
            let simplicity = numerator.max(denominator);
            let candidate = MoonPeriodRatio {
                ratio: measured_ratio,
                numerator,
                denominator,
            };
            let better = match &best {
                None => true,
                Some((_, best_deviation, best_simplicity)) => {
                    (deviation, simplicity) < (*best_deviation, *best_simplicity)
                }
            };
            if better {
                best = Some((candidate, deviation, simplicity));
            }
        }
    }
    best.map(|(candidate, _, _)| candidate)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn days(x: f64) -> StdDays {
        StdDays::new(x).expect("test fixture uses a finite positive value")
    }

    #[test]
    fn a_clean_two_to_one_pair_is_detected() {
        let periods = [days(30.0), days(60.0)];
        let found = detect_moon_period_ratio(&periods).expect("30/60 is a clean 2:1");
        assert_eq!(found.numerator, 2);
        assert_eq!(found.denominator, 1);
    }

    #[test]
    fn no_pair_means_no_ratio() {
        assert_eq!(detect_moon_period_ratio(&[]), None);
        assert_eq!(detect_moon_period_ratio(&[days(30.0)]), None);
    }

    #[test]
    fn an_unclean_pair_is_not_detected() {
        // 30 vs 55 days: ratio ~1.833, nearest candidates are 2:1 (8.3%
        // deviation) and 5:3 (10.0% deviation) — both exceed
        // RATIO_TOLERANCE, so no candidate matches.
        let periods = [days(30.0), days(55.0)];
        assert_eq!(detect_moon_period_ratio(&periods), None);
    }

    #[test]
    fn three_moons_picks_the_single_best_pair() {
        // Moon A/B are a clean 2:1 (30/60); moon C is unrelated to both.
        let periods = [days(30.0), days(60.0), days(41.0)];
        let found = detect_moon_period_ratio(&periods).expect("A/B is a clean 2:1");
        assert_eq!((found.numerator, found.denominator), (2, 1));
    }

    #[test]
    fn tied_deviation_pairs_break_by_simplicity_then_index() {
        // Moon periods (days): A=12, B=18, C=4.
        // Pair (A,B): ratio 18/12 = 1.5, exact match to 3:2 (simplicity 3,
        //   deviation 0.0).
        // Pair (A,C): ratio 12/4 = 3.0, exact match to 3:1 (simplicity 3,
        //   deviation 0.0) — the SAME deviation and simplicity as (A,B), so
        //   only pair index can break the tie between these two.
        // Pair (B,C): ratio 18/4 = 4.5, nearest candidate is 5:1 (10.0%
        //   deviation), exceeding RATIO_TOLERANCE — never competes.
        let periods = [days(12.0), days(18.0), days(4.0)];
        let found = detect_moon_period_ratio(&periods).expect("(A,B) is a clean 3:2");
        // (A,B) is visited before (A,C) (loop order `i in 0..len, j in
        // (i+1)..len`); since ties use strict `<`, the later (A,C) match
        // cannot displace it. If (A,B) had lost, `found` would be (3, 1)
        // instead.
        assert_eq!((found.numerator, found.denominator), (3, 2));
    }

    #[test]
    fn detect_moon_period_ratio_is_pure() {
        let periods = [days(30.0), days(60.0)];
        assert_eq!(
            detect_moon_period_ratio(&periods),
            detect_moon_period_ratio(&periods)
        );
    }
}
