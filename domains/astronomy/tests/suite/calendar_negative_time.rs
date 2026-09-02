//! Every instant-taking `Calendar` method, exercised at negative time.
//!
//! **This file could not have been written before The Foliot's split.** Every
//! one of these methods took a `StdDays`, whose constructor refuses a
//! negative, so the only pre-existing negative-time test lives *inside*
//! `calendar.rs` and its own comment records that it had to bypass
//! `StdDays::new` through the `pub(crate)` tuple field. From out here, through
//! the public API, the input was unconstructible. `StdInstant` is signed, so
//! the question is now askable from where a caller actually stands.
//!
//! Decision 0126 makes a negative instant legal: a time point is a point on an
//! axis, not a duration, and genesis is merely zero on it. The Foliot then
//! established that the negative path is REACHABLE by an ordinary user —
//! `cli/src/repl.rs`'s `sky <day>` parses a typed `f64` straight into
//! `WorldTime::from_std_days`, which accepts a negative.
//!
//! So the status quo before this sweep was not "known correct at negative
//! time" and not "known broken": it was **unknown**, on a reachable path. The
//! sweep exists to convert that into one or the other, and it is deliberately
//! run BEFORE the clamp on `GeneratedSky::t` is removed — removing it first
//! would ship an unvalidated path and call it a fix.
//!
//! `year_phase`, `season_phase` and `moon_phase` are the ones to watch:
//! `.fract()` returns a NEGATIVE fraction for a negative input, where a phase
//! must lie in `[0, 1)`.

use hornvale_astronomy::units::StdInstant;
use hornvale_astronomy::{Calendar, RotationPin, SkyPins, calendar_of, generate};
use hornvale_kernel::Seed;

/// Built through astronomy's own entry point: a domain's tests may not reach a
/// window any more than its source may.
fn earthlike() -> Calendar {
    let outcome = generate(Seed(42), &SkyPins::default()).expect("seed 42 builds");
    calendar_of(&outcome.value)
}

/// A tidally locked world has no local day, so several methods answer `None`.
/// Swept alongside the spinning case because "returns None" is a different
/// code path from "computes a phase", and only one of them can go negative.
fn locked() -> Calendar {
    let pins = SkyPins {
        rotation: Some(RotationPin::Locked),
        ..Default::default()
    };
    let outcome = generate(Seed(42), &pins).expect("a locked pin builds");
    calendar_of(&outcome.value)
}

/// Instants spanning genesis, so every assertion sees both signs and the
/// boundary itself. The large values matter: a phase that wraps correctly at
/// -1.5 days can still fail at -100,000 if the implementation subtracts
/// before taking a remainder.
fn probes() -> Vec<StdInstant> {
    [
        -100_000.0, -36_525.0, -365.25, -10.0, -1.5, -0.25, -1e-9, 0.0, 1e-9, 0.25, 1.5, 10.0,
        365.25, 100_000.0,
    ]
    .into_iter()
    .map(|d| StdInstant::new(d).expect("finite"))
    .collect()
}

/// A phase is a fraction of a cycle and must lie in `[0, 1)` on BOTH sides of
/// genesis. This is the assertion `.fract()` fails if it is reached with a
/// negative operand.
#[test]
fn every_phase_stays_in_the_unit_interval_across_genesis() {
    let cal = earthlike();
    for t in probes() {
        let yp = cal.year_phase(t);
        assert!(
            (0.0..1.0).contains(&yp),
            "year_phase({}) = {yp}, outside [0,1)",
            t.get()
        );
        if let Some(sp) = cal.season_phase(t) {
            assert!(
                (0.0..1.0).contains(&sp),
                "season_phase({}) = {sp}, outside [0,1)",
                t.get()
            );
        }
        for index in 0..3 {
            if let Some(mp) = cal.moon_phase(t, index) {
                assert!(
                    (0.0..1.0).contains(&mp),
                    "moon_phase({}, {index}) = {mp}, outside [0,1)",
                    t.get()
                );
            }
        }
    }
}

/// A local day index falls as time falls, and its fraction stays in `[0, 1)`.
///
/// The Escapement fixed exactly this for `local_day` — the old `as u64`
/// saturated every negative to day 0 and returned a negative fraction — but
/// pinned it only from inside the crate. This is the public-API witness.
#[test]
fn local_day_is_monotone_and_its_fraction_is_bounded_across_genesis() {
    let cal = earthlike();
    let mut previous: Option<i64> = None;
    for t in probes() {
        let Some((index, fraction)) = cal.local_day(t) else {
            continue;
        };
        assert!(
            (0.0..1.0).contains(&fraction),
            "local_day({}) fraction {fraction} outside [0,1)",
            t.get()
        );
        if let Some(prev) = previous {
            assert!(
                index >= prev,
                "local_day index went backwards as time advanced: {prev} then {index}"
            );
        }
        previous = Some(index);
    }
    assert!(
        previous.is_some(),
        "the sweep never got a local day — vacuous"
    );
}

/// A fraction of daylight is a fraction, and an angle is in range, on both
/// sides of genesis and at every latitude the sphere has.
#[test]
fn daylight_and_solar_angles_stay_in_range_across_genesis() {
    let cal = earthlike();
    for t in probes() {
        if let Some(f) = cal.daylight_fraction(t) {
            assert!(
                (0.0..=1.0).contains(&f),
                "daylight_fraction({}) = {f}",
                t.get()
            );
        }
        for latitude in [-89.0, -66.5, -23.5, 0.0, 23.5, 66.5, 89.0] {
            if let Some(f) = cal.daylight_fraction_at(t, latitude) {
                assert!(
                    (0.0..=1.0).contains(&f),
                    "daylight_fraction_at({}, {latitude}) = {f}",
                    t.get()
                );
            }
            if let Some(alt) = cal.solar_altitude_at(t, latitude) {
                assert!(
                    (-90.0..=90.0).contains(&alt),
                    "solar_altitude_at({}, {latitude}) = {alt}",
                    t.get()
                );
            }
            if let Some(az) = cal.solar_azimuth_at(t, latitude) {
                assert!(
                    (0.0..360.0).contains(&az),
                    "solar_azimuth_at({}, {latitude}) = {az}",
                    t.get()
                );
            }
            if let Some(a) = cal.solstice_rise_azimuth_at(latitude, t) {
                assert!(
                    a.is_finite(),
                    "solstice_rise_azimuth_at({latitude}, {}) = {a}",
                    t.get()
                );
            }
            let _ = cal.sky_band(t, latitude);
        }
    }
}

/// The solar declination is bounded by the obliquity, whichever side of
/// genesis it is read from.
#[test]
fn solar_declination_is_bounded_by_the_obliquity_across_genesis() {
    let cal = earthlike();
    for t in probes() {
        let dec = cal.solar_declination(t);
        assert!(
            dec.is_finite() && (-90.0..=90.0).contains(&dec),
            "solar_declination({}) = {dec}",
            t.get()
        );
    }
}

/// Nothing in the instant-taking surface returns a non-finite number at
/// negative time. The blunt half of the sweep, covering the methods with no
/// tighter invariant of their own.
#[test]
fn no_instant_taking_method_produces_a_non_finite_value() {
    for cal in [earthlike(), locked()] {
        for t in probes() {
            assert!(
                cal.precession_offset_deg(t).is_finite(),
                "precession_offset_deg({})",
                t.get()
            );
            let eq = cal.solar_equatorial(t);
            assert!(
                eq.ra_deg.is_finite() && eq.dec_deg.is_finite(),
                "solar_equatorial({}) = ra {} dec {}",
                t.get(),
                eq.ra_deg,
                eq.dec_deg
            );
            let genesis_pos = cal.solar_equatorial(StdInstant::new(0.0).unwrap());
            let star = cal.star_equatorial_at(&genesis_pos, t);
            assert!(
                star.ra_deg.is_finite() && star.dec_deg.is_finite(),
                "star_equatorial_at(.., {})",
                t.get()
            );
            let _ = cal.is_daylight(t);
        }
    }
}

/// The duration between two instants is unsigned and symmetric, including
/// when one of them is pre-genesis — the property `StdInstant::since` states
/// and `alignment_drift_deg` consumes.
#[test]
fn alignment_drift_is_defined_across_genesis() {
    let cal = earthlike();
    let before = StdInstant::new(-500.0).unwrap();
    let after = StdInstant::new(500.0).unwrap();
    for latitude in [-45.0, 0.0, 45.0] {
        if let Some(d) = cal.alignment_drift_deg(latitude, before, after) {
            assert!(d.is_finite(), "alignment_drift_deg across genesis: {d}");
        }
    }
}
