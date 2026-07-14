//! The regime × feature matrix (spec §6–7): every stage-1 derivation
//! returns the regime-honest answer. Cheap enough for the commit gate
//! (3 seeds × 4 regimes; no live worldgen batteries).

use hornvale_astronomy::calendar::calendar_of;
use hornvale_astronomy::forcing::P_PRECESSION;
use hornvale_astronomy::heliacal::heliacal_events;
use hornvale_astronomy::night_sky::night_sky_at;
use hornvale_astronomy::pins::{ForcingPin, RotationPin, SkyPins, SpinPin};
use hornvale_astronomy::sky_position::EquatorialCoord;
use hornvale_astronomy::system::generate;
use hornvale_astronomy::units::{Degrees, StdDays};
use hornvale_kernel::Seed;

const SEEDS: [u64; 3] = [1, 7, 42];

/// A locked world has no local day, so every stage-1 derivation that reads
/// the solar hour must say so honestly: a frozen sky, no heliacal events,
/// no sky band.
#[test]
fn locked_worlds_freeze_the_instrument() {
    for seed in SEEDS {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        let system = generate(Seed(seed), &pins).unwrap().system;
        let calendar = calendar_of(&system);
        let t = StdDays::new(5.0).unwrap();

        let sky = night_sky_at(&system, &calendar, 35.0, t);
        assert!(
            sky.frozen,
            "seed {seed}: locked world must report a frozen sky"
        );

        assert!(
            heliacal_events(&system, &calendar, 35.0, t).is_empty(),
            "seed {seed}: a locked world has no local day, so no heliacal events"
        );

        assert!(
            calendar.sky_band(t, 35.0).is_none(),
            "seed {seed}: a locked world has no solar hour, so no sky band"
        );
    }
}

/// Zero obliquity removes the seasonal driver but not the diurnal or annual
/// ones: the sun's declination stays pinned to the equator all year, and
/// `season_phase` (whose contract is "None when both obliquity and
/// eccentricity are exactly zero") goes None under `ForcingPin::Zero`. But
/// the sun's right ascension still advances a full turn over the year (the
/// ecliptic-longitude term in `solar_equatorial` doesn't depend on
/// obliquity), so a star can still be lost behind the sun's glare and
/// recovered — heliacal events survive.
#[test]
fn zero_obliquity_keeps_heliacal_events_but_kills_seasons() {
    let mut any_pairs = false;
    for seed in SEEDS {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            obliquity: Some(Degrees::new(0.0).unwrap()),
            forcing: Some(ForcingPin::Zero),
            ..SkyPins::default()
        };
        let system = generate(Seed(seed), &pins).unwrap().system;
        let calendar = calendar_of(&system);
        let year = calendar.year_length().get();

        // Sampled across the year: the sub-solar declination never leaves
        // the equator.
        for k in 0..24 {
            let t = StdDays::new(k as f64 * year / 24.0).unwrap();
            assert!(
                calendar.solar_equatorial(t).dec_deg.abs() < 1e-9,
                "seed {seed}: zero obliquity must pin solar declination to the equator"
            );
        }

        // The shipped season_phase contract: None when obliquity AND
        // eccentricity are both exactly zero (ForcingPin::Zero gives both).
        assert!(
            calendar
                .season_phase(StdDays::new(100.0).unwrap())
                .is_none(),
            "seed {seed}: zero obliquity + zero forcing must have no season phase"
        );

        // Heliacal events are a per-seed empirical fact (they depend on
        // where the drawn neighbor stars happen to sit relative to latitude
        // 35), so we don't assert nonempty per seed — only that the
        // mechanism (RA still advances over the year even at zero
        // obliquity) produces at least one riser-and-setter somewhere in
        // this 3-seed sample.
        if !heliacal_events(&system, &calendar, 35.0, StdDays::new(0.0).unwrap()).is_empty() {
            any_pairs = true;
        }
    }
    assert!(
        any_pairs,
        "at least one of the 3 seeds must still show heliacal events at zero obliquity \
         (RA drift over the year survives obliquity=0; it's a per-seed empirical fact \
         which neighbor stars happen to cross the horizon at latitude 35)"
    );
}

/// Spin direction flips which way the sky wheels but must never touch
/// heliacal timing (SKY-22's rule): the rising and setting fractions are
/// computed from RA/dec geometry that doesn't know which way the local day
/// counts its hour angle.
#[test]
fn retrograde_flips_wheeling_not_dates() {
    for seed in SEEDS {
        let system_with_spin = |spin| {
            let pins = SkyPins {
                rotation: Some(RotationPin::PeriodHours(24.0)),
                spin: Some(spin),
                ..SkyPins::default()
            };
            generate(Seed(seed), &pins).unwrap().system
        };

        let pro = system_with_spin(SpinPin::Prograde);
        let retro = system_with_spin(SpinPin::Retrograde);
        let cal_pro = calendar_of(&pro);
        let cal_retro = calendar_of(&retro);
        let t = StdDays::new(0.0).unwrap();

        let sky_pro = night_sky_at(&pro, &cal_pro, 35.0, t);
        let sky_retro = night_sky_at(&retro, &cal_retro, 35.0, t);
        assert_ne!(
            sky_pro.wheels_backward, sky_retro.wheels_backward,
            "seed {seed}: prograde and retrograde must disagree on wheeling"
        );

        let pairs_pro = heliacal_events(&pro, &cal_pro, 35.0, t);
        let pairs_retro = heliacal_events(&retro, &cal_retro, 35.0, t);
        assert_eq!(
            pairs_pro.len(),
            pairs_retro.len(),
            "seed {seed}: spin direction must not change which stars have heliacal events"
        );
        for (a, b) in pairs_pro.iter().zip(pairs_retro.iter()) {
            assert_eq!(
                a.neighbor, b.neighbor,
                "seed {seed}: neighbor order must match"
            );
            assert!(
                (a.rising_frac - b.rising_frac).abs() < 1e-9,
                "seed {seed}: rising_frac must match spin-independent, got {} vs {}",
                a.rising_frac,
                b.rising_frac
            );
            assert!(
                (a.setting_frac - b.setting_frac).abs() < 1e-9,
                "seed {seed}: setting_frac must match spin-independent, got {} vs {}",
                a.setting_frac,
                b.setting_frac
            );
        }
    }
}

/// Precession moves the equinox — the epoch-referenced half of the sky —
/// while the orbital-mechanical quantities (wanderer periods, the year
/// itself) are entirely untouched: they're derived from Kepler's third law
/// and the anchor's orbit, not from where the equinox currently points.
#[test]
fn epoch_drift_moves_the_equinox_referenced_and_spares_the_orbital() {
    for seed in SEEDS {
        let pins = SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            wanderers: Some(2),
            ..SkyPins::default()
        };
        let system = generate(Seed(seed), &pins).unwrap().system;
        let calendar = calendar_of(&system);

        let t0 = StdDays::new(0.0).unwrap();
        let t1 = StdDays::new(P_PRECESSION / 4.0).unwrap();

        // Every neighbor's apparent RA has moved more than a degree.
        for (i, n) in system.neighbors.iter().enumerate() {
            let genesis = EquatorialCoord {
                ra_deg: n.right_ascension,
                dec_deg: n.declination,
            };
            let ra0 = calendar.star_equatorial_at(&genesis, t0).ra_deg;
            let ra1 = calendar.star_equatorial_at(&genesis, t1).ra_deg;
            let raw_diff = (ra1 - ra0).rem_euclid(360.0);
            let angular_diff = raw_diff.min(360.0 - raw_diff);
            assert!(
                angular_diff > 1.0,
                "seed {seed} neighbor {i}: RA must drift > 1 deg over P_PRECESSION/4, got {angular_diff}"
            );
        }

        // Pole-star presence/separation: when either epoch has one, the
        // 21-kyr gap must show up as either a different separation or a
        // different (or absent) star.
        let sky0 = night_sky_at(&system, &calendar, 45.0, t0);
        let sky1 = night_sky_at(&system, &calendar, 45.0, t1);
        if sky0.pole_star.is_some() || sky1.pole_star.is_some() {
            match (sky0.pole_star, sky1.pole_star) {
                (Some(a), Some(b)) => assert!(
                    a.neighbor != b.neighbor || (a.separation_deg - b.separation_deg).abs() > 1e-9,
                    "seed {seed}: a pole star surviving 21 kyr unchanged is a precession bug"
                ),
                _ => {
                    // Presence differs between epochs — that itself is the
                    // drift signature (a pole star retired or was crowned).
                }
            }
        }

        // The orbital-mechanical quantities are epoch-free by construction:
        // neither `year_length()` nor `Wanderer::period` takes a `t`
        // parameter, so the type system already forbids epoch drift. What
        // CAN be pinned is the derived relationship itself — each period is
        // Kepler III of its own orbit and the star's mass, the formula both
        // anchor.rs and wanderers.rs declare. A future regression that
        // threads epoch (or anything else) into these derivations breaks
        // this recomputation.
        let kepler = |orbit_au: f64| 365.25 * (orbit_au.powi(3) / system.star.mass.get()).sqrt();
        assert!(
            (calendar.year_length().get() - kepler(system.anchor.orbit.get())).abs() < 1e-9,
            "seed {seed}: year_length must be Kepler III of the anchor's orbit"
        );
        assert!(
            !system.wanderers.is_empty(),
            "seed {seed}: wanderers: Some(2) pin must be non-vacuous"
        );
        for (i, w) in system.wanderers.iter().enumerate() {
            assert!(
                (w.period.get() - kepler(w.orbit.get())).abs() < 1e-9,
                "seed {seed} wanderer {i}: period must be Kepler III of its orbit"
            );
        }
    }
}
