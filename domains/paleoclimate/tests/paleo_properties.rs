//! Paleoclimate property battery: determinism and the physical invariants the
//! save-format contracts rest on. Domain-level (no worldgen) — the composition
//! root's determinism and the zero-forcing null control live in
//! `windows/worldgen` tests (Task 7).

use hornvale_kernel::math;
use hornvale_paleoclimate::{caloric_summer_index, integrate_ice};

const DAYS_PER_KYR: f64 = 1_000.0 * 365.25;

fn series(g: impl Fn(f64) -> f64, steps: usize) -> Vec<(f64, f64)> {
    (0..steps)
        .map(|k| {
            let day = k as f64 * 2.0 * DAYS_PER_KYR;
            (day, g(day))
        })
        .collect()
}

#[test]
fn integration_is_byte_identical_across_runs() {
    let s = series(
        |d| caloric_summer_index(23.5 + math::sin(d / 5e6), 23.5, 0.03, d / 3e6),
        500,
    );
    assert_eq!(integrate_ice(&s), integrate_ice(&s));
}

#[test]
fn ice_volume_stays_in_unit_range() {
    let s = series(
        |d| caloric_summer_index(20.0 + 5.0 * math::sin(d / 4e6), 23.5, 0.04, d / 2e6),
        800,
    );
    for st in integrate_ice(&s) {
        let v = st.volume.get();
        assert!((0.0..=1.0).contains(&v), "ice volume {v} left [0,1]");
    }
}

#[test]
fn flat_forcing_never_glaciates() {
    // Obliquity at its own reference and zero eccentricity gives an index of
    // exactly zero (the dead band), regardless of what that reference tilt
    // happened to be — the domain-level shadow of the null control.
    let s = series(|_| caloric_summer_index(23.5, 23.5, 0.0, 0.0), 500);
    assert!(integrate_ice(&s).iter().all(|st| st.volume.get() == 0.0));
}

#[test]
fn sustained_cold_then_warm_makes_a_sawtooth() {
    // Cold long enough to build ice, then warm: melt must outrun growth.
    let mut s = series(|_| -1.0, 400);
    s.extend(
        series(|_| 1.0, 200)
            .into_iter()
            .map(|(d, g)| (d + 400.0 * 2.0 * DAYS_PER_KYR, g)),
    );
    let h = integrate_ice(&s);
    let peak = h.iter().map(|st| st.volume.get()).fold(0.0_f64, f64::max);
    let end = h.last().unwrap().volume.get();
    assert!(
        peak > 0.5 && end < peak,
        "expected a rise then a faster fall"
    );
}
