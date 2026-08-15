//! The coarsening ladder a retold claim's day descends.

/// How precisely a claim's day is remembered. Distortion moves one rung
/// coarser per lossy retelling and never back — the anti-symmetry that makes
/// a rumour decay rather than sharpen.
/// type-audit: bare-ok(count: rungs)
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precision {
    /// The exact day.
    Day,
    /// Snapped to a 91-day season.
    Season,
    /// Snapped to a 365-day year.
    Year,
    /// Snapped to a 3650-day decade.
    Decade,
    /// Snapped to a 10950-day generation; the coarsest rung.
    Generation,
}

impl Precision {
    /// The next rung coarser, saturating at [`Precision::Generation`].
    pub fn coarser(self) -> Precision {
        match self {
            Precision::Day => Precision::Season,
            Precision::Season => Precision::Year,
            Precision::Year => Precision::Decade,
            Precision::Decade | Precision::Generation => Precision::Generation,
        }
    }

    /// The span of one rung, in standard days. `Day` is 1.0.
    fn span(self) -> f64 {
        match self {
            Precision::Day => 1.0,
            // 365 / 4 exactly, and exactly representable in binary (0.25 is
            // 2^-2). The rungs MUST nest — see `the_ladder_nests`.
            Precision::Season => 91.25,
            Precision::Year => 365.0,
            Precision::Decade => 3650.0,
            Precision::Generation => 10950.0,
        }
    }

    /// `day` snapped down to this rung. `Day` is the identity.
    /// type-audit: pending(wave-1: day), pending(wave-1: return)
    pub fn apply(self, day: f64) -> f64 {
        if self == Precision::Day {
            return day;
        }
        let s = self.span();
        (day / s).floor() * s
    }

    /// How many rungs the ladder has. The ceiling on distinct variants of one
    /// event, and therefore on what H2 can report.
    /// type-audit: bare-ok(count: return)
    pub fn rungs() -> usize {
        5
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coarser_walks_the_ladder_and_saturates_at_the_top() {
        assert_eq!(Precision::Day.coarser(), Precision::Season);
        assert_eq!(Precision::Season.coarser(), Precision::Year);
        assert_eq!(Precision::Year.coarser(), Precision::Decade);
        assert_eq!(Precision::Decade.coarser(), Precision::Generation);
        assert_eq!(Precision::Generation.coarser(), Precision::Generation);
    }

    #[test]
    fn applying_a_precision_snaps_a_day_down_to_its_rung() {
        // 3661.75 days: within year 10 (365-day rungs), decade 10.
        assert_eq!(Precision::Day.apply(3661.75), 3661.75);
        assert_eq!(Precision::Year.apply(3661.75), 3650.0);
        assert_eq!(Precision::Decade.apply(3661.75), 3650.0);
    }

    /// The ladder's rungs must NEST — each coarser span an exact multiple of
    /// the one below — and this is the invariant, not a sampled value.
    ///
    /// It is load-bearing rather than tidy. Floor-snapping onto a coarser grid
    /// is only guaranteed non-improving when the coarse grid is a subset of the
    /// fine one; otherwise coarsening can land a claim CLOSER to the truth,
    /// which would mean a rumour sharpens with retelling. An earlier draft used
    /// 91 for Season, which does not divide 365 (365/91 = 4.011), and at
    /// day 3661.75 it made the Year rung more accurate than the Season rung —
    /// 11.75 against 21.75. The fix belongs here, in the ladder, not in a test
    /// fixture chosen to avoid the misalignment.
    #[test]
    fn the_ladder_nests() {
        let rungs = [
            Precision::Season,
            Precision::Year,
            Precision::Decade,
            Precision::Generation,
        ];
        for pair in rungs.windows(2) {
            let (fine, coarse) = (pair[0].span(), pair[1].span());
            let k = coarse / fine;
            assert_eq!(
                k,
                k.floor(),
                "{:?} ({fine}) must divide {:?} ({coarse}) exactly; got {k}",
                pair[0],
                pair[1]
            );
        }
    }

    #[test]
    fn a_coarser_precision_is_never_more_precise_than_a_finer_one() {
        // The anti-symmetry that makes distortion monotone: coarsening can
        // only move a remembered day further from, never back toward, the
        // truth. Asserted over a swept population rather than one fixture,
        // because a single value cannot distinguish a real invariant from a
        // lucky alignment — which is exactly how the 91-day Season survived
        // its first test.
        let mut day = 0.0;
        while day < 40_000.0 {
            let mut p = Precision::Day;
            let mut prev_err = 0.0;
            for _ in 0..Precision::rungs() {
                p = p.coarser();
                let err = (p.apply(day) - day).abs();
                assert!(
                    err >= prev_err,
                    "coarsening to {p:?} improved accuracy at day {day}: \
                     {err} < {prev_err}"
                );
                prev_err = err;
            }
            day += 7.25;
        }
    }
}
