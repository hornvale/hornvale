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

use hornvale_kernel::CellId;

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

    /// Whether [`Self::sink`] is **certain** to evaluate to exactly `0.0` on
    /// this day, for *any* `present` value whatsoever — not merely "small"
    /// or "probably small". The default conservatively answers `false`
    /// (no shortcut is taken); a substrate that overrides this to `true`
    /// makes a load-bearing promise: [`spin_up`]'s glacier fast path (The
    /// Mire Glacier campaign, change B) trusts the answer instead of calling
    /// `sink` at all for a day it claims this about, and getting it wrong
    /// silently corrupts the trajectory the fast path returns. An override
    /// must restate its own `sink`'s actual zero condition verbatim, never a
    /// looser proxy — [`crate::snowpack::Snowpack`] is the only override
    /// today, and it repeats `sink`'s own `degree_days == 0.0` check exactly.
    /// type-audit: bare-ok(flag: return)
    fn sink_is_certainly_zero(&self, _ctx: &DayContext) -> bool {
        false
    }
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

/// Year-over-year forcing this far above [`CONVERGENCE_TOLERANCE`] is
/// certainly enough that [`spin_up`]'s glacier fast path can declare
/// non-convergence without paying for the per-year clone-and-diff check that
/// would otherwise prove it. See [`is_a_certain_glacier`]'s doc comment for
/// the full argument; 1000x tolerance is a comfortable margin against both
/// floating-point reassociation noise (many orders of magnitude smaller) and
/// any realistic near-zero forcing, while still being far below any real
/// day's precipitation in millimetres.
const GLACIER_SAFETY_MARGIN: f64 = CONVERGENCE_TOLERANCE * 1000.0;

/// Whether `year` is certainly a permanent-accumulator case for `substrate`:
/// every day's sink is certainly zero ([`Substrate::sink_is_certainly_zero`]),
/// and the total forcing over the year is comfortably above the noise floor
/// a real "never converges" cell must clear.
///
/// **Why this is safe, and the one case it deliberately excludes.** If every
/// day's sink is certainly zero, `present` only ever gains (never loses)
/// across the whole run — the loop degenerates to a running sum of `source`
/// over the periodic year, carried across years without reset. Two
/// sub-cases:
/// - If every day's `source` is *also* exactly `0.0`, `present` stays at
///   `0.0` forever and [`spin_up`]'s ordinary loop converges trivially at
///   `years_run == 2` (`previous == trajectory`, both all-zero). This
///   function returns `false` for that case (`annual_source` computed by
///   summing non-negative terms is `0.0` only if every term is `0.0`, since
///   `Substrate::source` must return a non-negative value), so `spin_up`
///   falls back to its exact, unmodified loop — which is cheap for a
///   trivially-converging cell anyway.
/// - Otherwise some day's `source` is strictly positive, so the *same*
///   nonzero forcing is added every single year (the periodic year and the
///   zero sink never change across years for this cell), never decaying —
///   there is no mechanism in this substrate family that could shrink it.
///   The year-over-year delta at every day position is that same total,
///   reordered by which day the comparison starts from; floating-point
///   reassociation moves it by at most a handful of ULPs, utterly negligible
///   against [`GLACIER_SAFETY_MARGIN`]. So once that total is confirmed to
///   clear the margin, it clears [`CONVERGENCE_TOLERANCE`] on **every**
///   year-over-year comparison the ordinary loop would make, meaning that
///   loop can never break early and must run out its full
///   `spin_up_years()` cap, reporting `converged: false` — exactly what this
///   function's caller then computes directly, without running the
///   per-year clone-and-diff bookkeeping that would otherwise be needed to
///   discover it.
///
/// This is a runtime-verified claim, not a theorem for arbitrary
/// `Substrate` implementations: it holds for any substrate whose
/// `sink_is_certainly_zero` override is honest (see that method's doc
/// comment) and whose `source` does not depend on accumulated state in a
/// way that could later shrink the annual total — true of every substrate
/// in this crate today. The campaign that introduced this fingerprinted it
/// against the unmodified loop across 5 seeds and ~41,000 cells each with an
/// empty diff (see `.superpowers/sdd/glacier-report.md`).
fn is_a_certain_glacier<S: Substrate + ?Sized>(substrate: &S, year: &[DayContext]) -> bool {
    let mut annual_source = 0.0;
    for ctx in year {
        if !substrate.sink_is_certainly_zero(ctx) {
            return false;
        }
        annual_source += substrate.source(ctx);
    }
    annual_source > GLACIER_SAFETY_MARGIN
}

/// Iterate `substrate` over the periodic `year` from zero until the
/// year-over-year trajectory stops moving, or until `spin_up_years` is spent.
///
/// The convergence test compares the maximum per-day change between the last
/// two years against `tolerance`.
///
/// Before running that exhaustive loop, checks [`is_a_certain_glacier`] (The
/// Mire Glacier campaign, change B): if the whole year's sink is certainly
/// zero and the forcing is comfortably nonzero, the outcome is already
/// determined analytically (permanent accumulation, `spin_up_years()` spent,
/// never converged), so the fast path below runs the *same* per-day
/// accumulation arithmetic, in the *same* order, for the *same* number of
/// years — skipping only the per-year `trajectory.clone()` and max-diff
/// comparison, which cannot succeed for this population anyway. Substituting
/// `substrate.sink(ctx, present)` with the literal `0.0` it is certain to
/// equal is exact, not approximate: `present + gained - 0.0 == present +
/// gained` bit-for-bit (subtracting an exact zero never rounds), so the
/// fast path's `trajectory` is bit-identical to what the unmodified loop
/// below would compute over the same `spin_up_years()` iterations.
/// type-audit: bare-ok(diagnostic-value: tolerance)
pub fn spin_up<S: Substrate + ?Sized>(
    substrate: &S,
    year: &[DayContext],
    tolerance: f64,
) -> SpinUp {
    let days = year.len();

    if is_a_certain_glacier(substrate, year) {
        let mut trajectory = vec![0.0; days];
        let mut present = 0.0;
        let years_run = substrate.spin_up_years();
        for _ in 0..years_run {
            for (day, ctx) in year.iter().enumerate() {
                let gained = substrate.source(ctx);
                present = (present + gained).max(0.0);
                trajectory[day] = present;
            }
        }
        return SpinUp {
            trajectory,
            converged: false,
            years_run,
        };
    }

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

/// A substrate evaluated over every cell and every day of the converged year
/// — derived on demand and **never committed**, exactly like the connection
/// graph. Computing it is opt-in, so a world that never asks pays nothing and
/// `GeneratedClimate::generate` is untouched.
#[derive(Clone, Debug)]
pub struct SubstrateField {
    trajectories: Vec<Vec<f64>>,
    converged: Vec<bool>,
    year_days: usize,
}

impl SubstrateField {
    /// Spin `substrate` up against every cell's own periodic year.
    pub fn compute<S: Substrate + ?Sized>(
        climate: &crate::provider::GeneratedClimate,
        substrate: &S,
    ) -> SubstrateField {
        let mut trajectories = Vec::new();
        let mut converged = Vec::new();
        let mut year_days = 0usize;
        for cell in climate.geosphere().cells() {
            let year = climate.year_of_day_contexts(cell);
            year_days = year.len();
            let out = spin_up(substrate, &year, CONVERGENCE_TOLERANCE);
            trajectories.push(out.trajectory);
            converged.push(out.converged);
        }
        SubstrateField {
            trajectories,
            converged,
            year_days,
        }
    }

    /// Spin two substrates up together, computing each cell's periodic year
    /// of [`DayContext`]s **once** and reusing it for both, rather than
    /// once per substrate.
    ///
    /// `climate.year_of_day_contexts(cell)` is a pure function of
    /// `(climate, cell)` alone -- it never reads `a` or `b` -- so calling
    /// [`Self::compute`] twice (once per substrate) rebuilds the identical
    /// `Vec<DayContext>` for every cell a second time. This shares that one
    /// build across both spin-ups instead. Each substrate's own
    /// [`spin_up`] call is otherwise byte-for-byte what [`Self::compute`]
    /// would run over the same year, and the two spin-ups do not share any
    /// mutable state, so the pair returned here is arithmetically identical
    /// to `(SubstrateField::compute(climate, a), SubstrateField::compute(climate, b))`
    /// -- a pure restructuring, not a behaviour change.
    pub fn compute_pair<A: Substrate + ?Sized, B: Substrate + ?Sized>(
        climate: &crate::provider::GeneratedClimate,
        a: &A,
        b: &B,
    ) -> (SubstrateField, SubstrateField) {
        let mut trajectories_a = Vec::new();
        let mut converged_a = Vec::new();
        let mut trajectories_b = Vec::new();
        let mut converged_b = Vec::new();
        let mut year_days = 0usize;
        for cell in climate.geosphere().cells() {
            let year = climate.year_of_day_contexts(cell);
            year_days = year.len();

            let out_a = spin_up(a, &year, CONVERGENCE_TOLERANCE);
            trajectories_a.push(out_a.trajectory);
            converged_a.push(out_a.converged);

            let out_b = spin_up(b, &year, CONVERGENCE_TOLERANCE);
            trajectories_b.push(out_b.trajectory);
            converged_b.push(out_b.converged);
        }
        (
            SubstrateField {
                trajectories: trajectories_a,
                converged: converged_a,
                year_days,
            },
            SubstrateField {
                trajectories: trajectories_b,
                converged: converged_b,
                year_days,
            },
        )
    }

    /// The substrate's value at `cell` on `day`, wrapping the year.
    /// type-audit: bare-ok(diagnostic-value: day), bare-ok(diagnostic-value: return)
    pub fn at(&self, cell: CellId, day: f64) -> f64 {
        if self.year_days == 0 {
            return 0.0;
        }
        let idx = (day.floor() as i64).rem_euclid(self.year_days as i64) as usize;
        self.trajectories
            .get(cell.0 as usize)
            .and_then(|t| t.get(idx))
            .copied()
            .unwrap_or(0.0)
    }

    /// How many cells never reached a fixed point — the glacier count. A
    /// reported quantity, not an error.
    /// type-audit: bare-ok(count: return)
    pub fn non_convergent_cells(&self) -> usize {
        self.converged.iter().filter(|c| !**c).count()
    }
}

/// Year-over-year movement below this counts as a fixed point.
const CONVERGENCE_TOLERANCE: f64 = 1e-6;

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

    #[test]
    fn a_cells_year_of_contexts_reproduces_its_annual_climatology() {
        // The invariant from Task 1, now asserted against REAL worlds rather
        // than a synthetic year - H3 of the preregistration.
        let climate = crate::provider::test_support::sample_climate();
        for cell in climate.geosphere().cells().take(64) {
            let year = climate.year_of_day_contexts(cell);
            let summed: f64 = year.iter().map(|c| c.precip_mm).sum();
            let annual = climate.precip_at(cell).get();
            assert!(
                (summed - annual).abs() <= annual.abs() * 1e-6 + 1e-6,
                "cell {cell:?}: daily precip summed to {summed}, climatology is {annual}"
            );
        }
    }

    #[test]
    fn a_substrate_field_is_deterministic_for_a_seed() {
        let climate = crate::provider::test_support::sample_climate();
        let a = SubstrateField::compute(&climate, &crate::wetness::DEFAULT_WETNESS);
        let b = SubstrateField::compute(&climate, &crate::wetness::DEFAULT_WETNESS);
        for cell in climate.geosphere().cells().take(64) {
            for day in [0.0, 90.0, 180.0, 270.0] {
                assert_eq!(a.at(cell, day), b.at(cell, day));
            }
        }
    }

    #[test]
    fn a_substrate_field_wraps_the_year() {
        let climate = crate::provider::test_support::sample_climate();
        let f = SubstrateField::compute(&climate, &crate::wetness::DEFAULT_WETNESS);
        let cell = climate.geosphere().cells().next().expect("non-empty mesh");
        let year = climate.year_length_std();
        assert_eq!(f.at(cell, 3.0), f.at(cell, 3.0 + year));
    }
}
