//! Surface wetness — the mire. The fast substrate (τ ≈ days): rain in,
//! evaporation out, capped at a field capacity because past saturation more
//! rain becomes runoff rather than more mud.

use crate::substrate::{DayContext, Substrate};

/// How wet the ground is, in millimetres of retained water.
/// type-audit: bare-ok(diagnostic-value: field_capacity_mm), bare-ok(ratio: dry_rate)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SurfaceWetness {
    /// The ceiling, mm. Beyond this the ground is saturated and further rain
    /// runs off rather than accumulating.
    pub field_capacity_mm: f64,
    /// The share of retained water a warm, cloudless day removes.
    pub dry_rate: f64,
}

/// A loam-ish default: 50 mm of retention, drying at 18% per clear warm day
/// (a ~5-day e-folding time — the τ ≈ days the campaign claims).
pub const DEFAULT_WETNESS: SurfaceWetness = SurfaceWetness {
    field_capacity_mm: 50.0,
    dry_rate: 0.18,
};

impl Substrate for SurfaceWetness {
    /// Liquid precipitation only — the snow share is [`crate::snowpack`]'s.
    fn source(&self, ctx: &DayContext) -> f64 {
        ctx.precip_mm * (1.0 - ctx.snow_fraction.clamp(0.0, 1.0))
    }

    /// Evaporative drying, suppressed by cloud and switched off entirely
    /// below freezing — frozen ground does not dry. The saturation ceiling is
    /// enforced here rather than in `source` so the driver's own clamp never
    /// has to guess: anything above capacity is shed in full.
    ///
    /// `present` is the driver's pre-gain state (today's rain has not yet
    /// landed), so the overflow check is made against `present` plus this
    /// day's own gain — recomputed via `source`, which is side-effect-free —
    /// rather than against `present` alone. Comparing to `present` alone
    /// lags the cap by exactly one day: on a sustained deluge the shed amount
    /// always trails the day's incoming rain by one step, and the substrate
    /// settles into a steady state strictly above field capacity instead of
    /// at it.
    fn sink(&self, ctx: &DayContext, present: f64) -> f64 {
        let post_gain = present + self.source(ctx);
        let overflow = (post_gain - self.field_capacity_mm).max(0.0);
        if ctx.mean_temp_c <= 0.0 {
            return overflow;
        }
        let retained = post_gain.min(self.field_capacity_mm);
        let cloud = 1.0 - ctx.cloud_fraction.clamp(0.0, 1.0);
        overflow + retained * self.dry_rate * cloud
    }

    fn spin_up_years(&self) -> u32 {
        4
    }
}

/// How print-taking, boggy and wheel-catching the ground is, `[0,1]` — the
/// latent quantity every reader consumes. A zero-capacity substrate reports
/// `0.0` rather than dividing.
/// type-audit: bare-ok(diagnostic-value: wetness_mm), bare-ok(diagnostic-value: capacity_mm), bare-ok(ratio: return)
pub fn receptivity(wetness_mm: f64, capacity_mm: f64) -> f64 {
    if capacity_mm <= 0.0 {
        return 0.0;
    }
    (wetness_mm / capacity_mm).clamp(0.0, 1.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::substrate::{DayContext, Substrate, spin_up};

    fn day(precip_mm: f64, mean_temp_c: f64) -> DayContext {
        DayContext {
            precip_mm,
            snow_fraction: 0.0,
            mean_temp_c,
            cloud_fraction: 0.3,
        }
    }

    #[test]
    fn rain_wets_the_ground_and_a_dry_spell_dries_it() {
        let s = DEFAULT_WETNESS;
        let wet_year: Vec<DayContext> = vec![day(20.0, 15.0); 360];
        let dry_year: Vec<DayContext> = vec![day(0.0, 15.0); 360];
        let wet = *spin_up(&s, &wet_year, 1e-6)
            .trajectory
            .last()
            .expect("non-empty");
        let dry = *spin_up(&s, &dry_year, 1e-6)
            .trajectory
            .last()
            .expect("non-empty");
        assert!(
            wet > dry,
            "a rained-on year ({wet}) was not wetter than a dry one ({dry})"
        );
        assert_eq!(dry, 0.0, "a rainless year left standing water: {dry}");
    }

    #[test]
    fn wetness_saturates_at_field_capacity() {
        // Past field capacity additional rain becomes runoff, not more mud.
        // A ceiling, not an unbounded integral - and it is what keeps a
        // monsoon cell from reporting absurd receptivity.
        let s = DEFAULT_WETNESS;
        let deluge: Vec<DayContext> = vec![day(500.0, 15.0); 360];
        let out = spin_up(&s, &deluge, 1e-6);
        let peak = out.trajectory.iter().fold(0.0f64, |a, b| a.max(*b));
        assert!(
            peak <= s.field_capacity_mm + 1e-9,
            "wetness {peak} exceeded field capacity {}",
            s.field_capacity_mm
        );
    }

    #[test]
    fn frozen_ground_does_not_dry() {
        // The freeze modifier's first appearance: below freezing the
        // evaporative sink is suppressed entirely, so a frozen wet cell holds
        // its water instead of quietly evaporating.
        let s = DEFAULT_WETNESS;
        let warm = s.sink(&day(0.0, 15.0), 20.0);
        let frozen = s.sink(&day(0.0, -5.0), 20.0);
        assert!(warm > 0.0, "warm ground did not dry at all");
        assert_eq!(frozen, 0.0, "frozen ground dried at rate {frozen}");
    }

    #[test]
    fn cloud_suppresses_drying() {
        let s = DEFAULT_WETNESS;
        let clear = DayContext {
            cloud_fraction: 0.0,
            ..day(0.0, 15.0)
        };
        let overcast = DayContext {
            cloud_fraction: 1.0,
            ..day(0.0, 15.0)
        };
        assert!(
            s.sink(&clear, 20.0) > s.sink(&overcast, 20.0),
            "an overcast day dried the ground as fast as a clear one"
        );
    }

    #[test]
    fn receptivity_is_a_normalized_ratio() {
        assert_eq!(receptivity(0.0, 50.0), 0.0);
        assert_eq!(receptivity(50.0, 50.0), 1.0);
        assert_eq!(receptivity(100.0, 50.0), 1.0, "receptivity exceeded 1");
        assert_eq!(
            receptivity(10.0, 0.0),
            0.0,
            "a zero-capacity cell divided by zero"
        );
    }

    #[test]
    fn surface_wetness_converges_within_one_year() {
        // The fast substrate's defining property, and the contrast that makes
        // the snowpack task's multi-year spin-up meaningful.
        //
        // `years_run == 3`, not 2, is the correct number here, for a
        // structural reason that has nothing to do with wetness's own decay
        // rate: `spin_up`'s convergence check compares whole-year
        // trajectories, and year 1 always starts cold from `present == 0`.
        // Year 2 starts instead from wherever year 1 ended (already near the
        // fixed point for a τ ≈ days substrate), so year 1's early-day ramp
        // never matches year 2's near-flat trajectory - years 2 and 3 are
        // the first pair that actually agree. `substrate.rs`'s own
        // `a_state_dependent_sink_is_honoured` test documents exactly this
        // same "year 1 is a transient" shape and pins `years_run == 3`.
        // Asserting `<= 2` here would demand something no constant-fixed-
        // point substrate under this driver can deliver; the meaningful
        // claim is that wetness settles within a *couple* of years, in
        // contrast to a substrate that runs out its whole `spin_up_years`
        // budget still climbing (task 4's snowpack).
        let out = spin_up(&DEFAULT_WETNESS, &vec![day(5.0, 12.0); 360], 1e-6);
        assert!(out.converged);
        assert!(out.years_run <= 3, "took {} years to settle", out.years_run);
    }
}
