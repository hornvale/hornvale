//! Weather's memory. A **substrate** integrates weather over time and decays
//! — the operator behind mud, snowpack, soil moisture, fuel dryness and sea
//! ice, differing only in time constant.
//!
//! This is a forward **recurrence**, not a decaying convolution. A
//! convolution would let any day be evaluated without touching the days
//! before it, which is strictly nicer — and is wrong here, because the sink
//! is a function of how much of the substrate is *present*. Snowpack does not
//! ablate below freezing at any rate, so its loss term depends on its own
//! state, and a state-dependent rate is not a linear operator.
//!
//! The initial condition is therefore the real problem: a threshold substrate
//! has unbounded memory in a cold climate. The forcing is seasonally
//! periodic, so [`spin_up`] iterates whole years from zero until the
//! trajectory repeats within a tolerance. Substrates that never converge are
//! reported, not errors — permanent accumulation *is* a glacier.

/// The per-day environmental reads a substrate integrates.
/// type-audit: bare-ok(diagnostic-value: precip_mm), bare-ok(ratio: snow_fraction), bare-ok(diagnostic-value: mean_temp_c), bare-ok(ratio: cloud_fraction)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DayContext {
    /// Total precipitation falling on this day, mm.
    pub precip_mm: f64,
    /// The share of that precipitation falling as snow rather than rain,
    /// `[0,1]`.
    pub snow_fraction: f64,
    /// The day's mean temperature, °C — the freeze threshold every
    /// state-dependent sink reads.
    pub mean_temp_c: f64,
    /// The day's cloud fraction, `[0,1]` — cloud suppresses evaporative
    /// drying.
    pub cloud_fraction: f64,
}

/// A material that accumulates weather and loses it over time.
pub trait Substrate {
    /// What this substrate gains on this day.
    ///
    /// Implementations must return a non-negative value. The driver clamps
    /// `sink`'s result to `[0.0, present + gained]`; a negative `gained`
    /// would push that upper bound below the lower one and panic.
    /// type-audit: bare-ok(diagnostic-value: return)
    fn source(&self, ctx: &DayContext) -> f64;

    /// What it loses on this day, given how much is currently present.
    ///
    /// `present` is the argument that makes this a recurrence: a sink that
    /// reads the substrate's own state cannot be folded into a convolution
    /// kernel. Implementations must return a non-negative loss; the driver
    /// clamps the result to at most `present` **plus this day's `source`
    /// gain** (not `present` alone), since the loss is applied after the
    /// gain lands. A correct sink should not rely on the clamp, but may
    /// return up to that looser bound.
    /// type-audit: bare-ok(diagnostic-value: present), bare-ok(diagnostic-value: return)
    fn sink(&self, ctx: &DayContext, present: f64) -> f64;

    /// How many whole years [`spin_up`] may iterate before giving up on
    /// convergence. A substrate that can accumulate indefinitely uses this to
    /// bound the work; hitting the cap is a reported result, not a failure.
    /// type-audit: bare-ok(count: return)
    fn spin_up_years(&self) -> u32;
}

/// The outcome of spinning a substrate up against a periodic year.
/// type-audit: bare-ok(diagnostic-value: trajectory), bare-ok(flag: converged), bare-ok(count: years_run)
#[derive(Clone, Debug)]
pub struct SpinUp {
    /// The substrate's value on each day of the converged (or capped) year.
    pub trajectory: Vec<f64>,
    /// Whether successive years agreed within the tolerance. `false` means
    /// the substrate is still accumulating at the cap — a glacier, not a bug.
    pub converged: bool,
    /// How many whole years were iterated.
    pub years_run: u32,
}

/// Iterate `substrate` over the periodic `year` from zero until the
/// year-over-year trajectory stops moving, or until `spin_up_years` is spent.
///
/// The convergence test compares the maximum per-day change between the last
/// two years against `tolerance`.
/// type-audit: bare-ok(diagnostic-value: tolerance)
pub fn spin_up<S: Substrate + ?Sized>(
    substrate: &S,
    year: &[DayContext],
    tolerance: f64,
) -> SpinUp {
    let days = year.len();
    let mut trajectory = vec![0.0; days];
    let mut present = 0.0;
    let mut converged = false;
    let mut years_run = 0;

    for _ in 0..substrate.spin_up_years() {
        let previous = trajectory.clone();
        for (day, ctx) in year.iter().enumerate() {
            let gained = substrate.source(ctx);
            let lost = substrate.sink(ctx, present).clamp(0.0, present + gained);
            present = (present + gained - lost).max(0.0);
            trajectory[day] = present;
        }
        years_run += 1;

        let moved = previous
            .iter()
            .zip(&trajectory)
            .map(|(a, b)| (a - b).abs())
            .fold(0.0f64, f64::max);
        if years_run > 1 && moved <= tolerance {
            converged = true;
            break;
        }
    }

    SpinUp {
        trajectory,
        converged,
        years_run,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A linear test substrate: constant source, decay proportional to what
    /// is present. Its fixed point is analytically `source / rate`, which is
    /// what makes it the right instrument for testing the DRIVER rather than
    /// any real substrate's physics.
    struct Linear {
        source: f64,
        rate: f64,
    }

    impl Substrate for Linear {
        fn source(&self, _ctx: &DayContext) -> f64 {
            self.source
        }
        fn sink(&self, _ctx: &DayContext, present: f64) -> f64 {
            present * self.rate
        }
        fn spin_up_years(&self) -> u32 {
            50
        }
    }

    fn flat_year(days: usize) -> Vec<DayContext> {
        vec![
            DayContext {
                precip_mm: 0.0,
                snow_fraction: 0.0,
                mean_temp_c: 10.0,
                cloud_fraction: 0.0,
            };
            days
        ]
    }

    #[test]
    fn a_linear_substrate_converges_to_its_analytic_fixed_point() {
        let s = Linear {
            source: 2.0,
            rate: 0.1,
        };
        let out = spin_up(&s, &flat_year(360), 1e-9);
        assert!(out.converged, "linear substrate failed to converge");
        let expected = 2.0 / 0.1;
        let last = *out.trajectory.last().expect("non-empty year");
        assert!(
            (last - expected).abs() < 1e-6,
            "converged to {last}, analytic fixed point is {expected}"
        );
    }

    #[test]
    fn a_sourceless_substrate_stays_at_zero() {
        let s = Linear {
            source: 0.0,
            rate: 0.1,
        };
        let out = spin_up(&s, &flat_year(360), 1e-9);
        assert!(out.converged);
        assert!(out.trajectory.iter().all(|v| *v == 0.0));
    }

    #[test]
    fn a_substrate_that_never_loses_anything_reports_non_convergence() {
        // The glacier case. Accumulation with a zero sink grows without
        // bound; the driver must CAP and REPORT rather than spin forever.
        // Non-convergence is a result, not an error.
        struct Glacier;
        impl Substrate for Glacier {
            fn source(&self, _ctx: &DayContext) -> f64 {
                1.0
            }
            fn sink(&self, _ctx: &DayContext, _present: f64) -> f64 {
                0.0
            }
            fn spin_up_years(&self) -> u32 {
                5
            }
        }
        let out = spin_up(&Glacier, &flat_year(360), 1e-9);
        assert!(
            !out.converged,
            "unbounded accumulation reported convergence"
        );
        assert_eq!(out.years_run, 5, "the spin-up cap was not honoured");
        assert!(*out.trajectory.last().expect("non-empty") > 0.0);
    }

    #[test]
    fn the_trajectory_covers_exactly_one_year() {
        let s = Linear {
            source: 1.0,
            rate: 0.5,
        };
        let out = spin_up(&s, &flat_year(360), 1e-9);
        assert_eq!(out.trajectory.len(), 360);
    }

    #[test]
    fn a_state_dependent_sink_is_honoured() {
        // The property that forced a recurrence over a convolution: a sink
        // that switches on the substrate's OWN state cannot be expressed as a
        // weighted sum over past forcing. With `present` genuinely threaded
        // through, Thresholded locks onto a period-5 cycle once it crosses
        // the threshold: present after each day runs ...10 -> 6 -> 7 -> 8 ->
        // 9 -> 10 -> 6 -> ... . Year 1's climb from zero (days 1-10 rising
        // 1..10) is a transient that year 2 does not repeat, but years 2 and
        // 3 land on the identical cycle (both start their year at
        // present == 10.0), so convergence fires at years_run == 3. Because
        // 360 is divisible by the cycle's period of 5, the final day of
        // every post-transient year is exactly the cycle's peak, 10.0.
        //
        // A driver that silently discards `present` (e.g. always evaluating
        // the sink against 0.0, as a mutation test on `spin_up` did) turns
        // Thresholded into an unbounded accumulator: it never converges, and
        // its last day is some ever-growing value strictly greater than
        // 10.0 -- a bare `>= 10.0` cannot distinguish that from the correct,
        // convergent, EXACTLY-10.0 fixed point. Pinning `converged`,
        // `years_run`, and the exact value together is what makes the two
        // worlds distinguishable.
        struct Thresholded;
        impl Substrate for Thresholded {
            fn source(&self, _ctx: &DayContext) -> f64 {
                1.0
            }
            fn sink(&self, _ctx: &DayContext, present: f64) -> f64 {
                if present < 10.0 { 0.0 } else { present * 0.5 }
            }
            fn spin_up_years(&self) -> u32 {
                20
            }
        }
        let out = spin_up(&Thresholded, &flat_year(360), 1e-9);
        assert!(
            out.converged,
            "a correctly-threaded sink locks onto a period-5 cycle and converges"
        );
        assert_eq!(
            out.years_run, 3,
            "year 1 is a transient; years 2 and 3 must match exactly"
        );
        let last = *out.trajectory.last().expect("non-empty");
        assert_eq!(
            last, 10.0,
            "the threshold sink did not lock onto its exact fixed point: {last}"
        );
    }
}
