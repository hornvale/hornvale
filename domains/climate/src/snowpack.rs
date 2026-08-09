//! Snowpack — the slow substrate (τ ≈ months) and the reason the substrate
//! integral is a recurrence rather than a convolution.
//!
//! Its sink is a degree-day melt: **exactly zero at or below freezing**, and
//! proportional to how far above freezing the day is otherwise. A loss term
//! that switches on temperature and on how much pack is present cannot be
//! expressed as a fixed kernel over past snowfall, which is what rules the
//! convolution out for the whole family.

use crate::substrate::{DayContext, Substrate};

/// Accumulated snow, in millimetres of water equivalent.
/// type-audit: bare-ok(diagnostic-value: melt_per_degree_day_mm)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Snowpack {
    /// Millimetres of water equivalent melted per degree-day above freezing.
    pub melt_per_degree_day_mm: f64,
}

/// A temperate default: 3 mm w.e. per degree-day, within the 2–5 range
/// ordinary degree-day snowmelt models use.
pub const DEFAULT_SNOWPACK: Snowpack = Snowpack {
    melt_per_degree_day_mm: 3.0,
};

impl Substrate for Snowpack {
    /// The snow share of the day's precipitation.
    fn source(&self, ctx: &DayContext) -> f64 {
        ctx.precip_mm * ctx.snow_fraction.clamp(0.0, 1.0)
    }

    /// Degree-day ablation, floored at zero below freezing and capped at what
    /// is actually present.
    ///
    /// The cap is computed against `present` **plus this day's own gain**
    /// (recomputed via `source`, which is side-effect-free), not `present`
    /// alone — deliberately, mirroring [`crate::wetness::SurfaceWetness`]'s
    /// same choice and for the same underlying reason: a day's snowfall and
    /// a day's melt are both resolved within the same atomic time step, so
    /// there is no physical reason fresh snow that lands during a warm day
    /// must survive untouched until the next step. Capping at pre-gain
    /// `present` instead would be defensible too (it is the more
    /// conservative choice, and matches a model where the day's temperature
    /// reading logically precedes that day's precipitation), but it is not
    /// what is implemented here: chasing the wetness precedent keeps the
    /// substrate family's cap semantics uniform rather than having each
    /// substrate quietly pick its own.
    fn sink(&self, ctx: &DayContext, present: f64) -> f64 {
        let post_gain = present + self.source(ctx);
        let degree_days = ctx.mean_temp_c.max(0.0);
        (degree_days * self.melt_per_degree_day_mm).min(post_gain.max(0.0))
    }

    /// Enough years for a seasonal pack to settle, and a hard stop for a cell
    /// that accumulates indefinitely — a glacier is reported as
    /// non-convergence rather than spun on forever.
    fn spin_up_years(&self) -> u32 {
        12
    }

    /// At or below freezing, `degree_days` (`sink`'s own
    /// `ctx.mean_temp_c.max(0.0)`) is exactly `0.0`, so `sink` returns
    /// `(0.0 * melt_per_degree_day_mm).min(post_gain.max(0.0))` — `0.0`
    /// regardless of `present`, since `0.0.min(x)` is `0.0` for any `x >=
    /// 0.0` and `post_gain.max(0.0)` is always `>= 0.0`. This restates that
    /// condition verbatim (`ctx.mean_temp_c <= 0.0`), the exact threshold
    /// `sink` itself branches on — the contract `Substrate::
    /// sink_is_certainly_zero` requires of any override.
    fn sink_is_certainly_zero(&self, ctx: &DayContext) -> bool {
        ctx.mean_temp_c <= 0.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::substrate::{DayContext, Substrate, spin_up};

    fn day(precip_mm: f64, mean_temp_c: f64) -> DayContext {
        DayContext {
            precip_mm,
            snow_fraction: 1.0,
            mean_temp_c,
            cloud_fraction: 0.5,
        }
    }

    #[test]
    fn snow_never_ablates_below_freezing() {
        // THE property. This is why the integral is a recurrence and not a
        // convolution: the loss term reads the substrate's own state and the
        // temperature, so it cannot be a fixed kernel over past forcing.
        let s = DEFAULT_SNOWPACK;
        for temp in [-40.0, -10.0, -0.001, 0.0] {
            assert_eq!(
                s.sink(&day(0.0, temp), 500.0),
                0.0,
                "snow ablated at {temp} C"
            );
        }
    }

    #[test]
    fn ablation_rises_with_temperature_above_freezing() {
        let s = DEFAULT_SNOWPACK;
        let mild = s.sink(&day(0.0, 2.0), 500.0);
        let hot = s.sink(&day(0.0, 20.0), 500.0);
        assert!(mild > 0.0, "no melting at 2 C");
        assert!(hot > mild, "20 C ({hot}) did not out-melt 2 C ({mild})");
    }

    #[test]
    fn ablation_never_exceeds_what_is_present() {
        // With zero same-day precipitation, `present` and `present + gained`
        // coincide, so this call alone cannot distinguish a pre-gain cap
        // from a post-gain one.
        let s = DEFAULT_SNOWPACK;
        assert!(
            s.sink(&day(0.0, 40.0), 3.0) <= 3.0,
            "melted more snow than existed with no same-day snowfall"
        );

        // The distinguishing case: heavy same-day snowfall. This
        // implementation caps ablation at `present + today's gain` (see the
        // doc comment on `sink`), so a hot day that also drops a heavy
        // same-day snowfall may melt into that fresh snow, up to the total
        // now present — but never past it.
        let ctx = day(20.0, 40.0);
        let gained = s.source(&ctx);
        let melted = s.sink(&ctx, 3.0);
        assert!(
            melted <= 3.0 + gained,
            "melted {melted} mm, more than the 3.0 + {gained} mm present after today's fall"
        );
        // The assertion above alone would also be satisfied by a *stricter*
        // pre-gain cap (`present.max(0.0)`, which is always <= present +
        // gained) — so on its own it does not prove this implementation
        // actually draws on same-day snowfall. Pin that positively: with
        // degree-day demand (120 mm) far exceeding the 3 mm present before
        // today's fall, a post-gain cap must melt strictly more than the
        // 3 mm that was there this morning.
        assert!(
            melted > 3.0,
            "melted only {melted} mm, no more than the pre-gain 3.0 mm present \
             — same-day snowfall was never available to melt"
        );
    }

    #[test]
    fn only_the_snow_share_of_precipitation_accumulates() {
        let s = DEFAULT_SNOWPACK;
        let all_rain = DayContext {
            snow_fraction: 0.0,
            ..day(30.0, -5.0)
        };
        let all_snow = DayContext {
            snow_fraction: 1.0,
            ..day(30.0, -5.0)
        };
        assert_eq!(s.source(&all_rain), 0.0);
        assert_eq!(s.source(&all_snow), 30.0);
    }

    #[test]
    fn a_seasonal_year_builds_a_pack_in_winter_and_clears_it_in_summer() {
        // 180 days of sub-freezing snowfall, 180 days of warm dry weather.
        let winter = vec![day(6.0, -8.0); 180];
        let summer = vec![day(0.0, 18.0); 180];
        let year: Vec<DayContext> = winter.into_iter().chain(summer).collect();
        let out = spin_up(&DEFAULT_SNOWPACK, &year, 1e-6);
        assert!(
            out.converged,
            "a seasonal pack failed to reach a fixed point"
        );
        assert!(out.trajectory[179] > 0.0, "no pack accumulated over winter");
        assert_eq!(
            out.trajectory[359], 0.0,
            "the pack survived a full warm summer: {}",
            out.trajectory[359]
        );
    }

    #[test]
    fn a_permanently_frozen_cell_never_converges() {
        // The glacier. A cell that snows and never rises above freezing
        // accumulates without bound; the honest answer is non-convergence at
        // the cap, not a fabricated equilibrium.
        let year = vec![day(4.0, -20.0); 360];
        let out = spin_up(&DEFAULT_SNOWPACK, &year, 1e-6);
        assert!(!out.converged, "permanent ice reported a fixed point");
        assert_eq!(out.years_run, DEFAULT_SNOWPACK.spin_up_years());
        assert!(*out.trajectory.last().expect("non-empty") > 0.0);
    }
}
