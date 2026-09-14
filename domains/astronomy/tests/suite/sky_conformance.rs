//! The sky-conformance battery: what a Hornvale sky *is*. Every generated
//! sky, on every seed, in every rotation regime, at every hour, keeps four
//! claims true — each stellar source has a day-sky sun, nothing outranks the primary, it
//! is never retracted from the visible bodies, and any period it carries is
//! the calendar's own day.
//!
//! These were the claims the retired tier-0 provider stipulated and the
//! cross-tier refinement battery checked the generated sky against
//! ("coarse constrains fine"). The tier is gone; the claims were always the
//! part doing the work, so they are asserted directly (decisions 0736 and
//! 0738).
//!
//! The three constants below were previously read out of the retired provider
//! at test time — production code inside the crate under test. Frozen here,
//! they can no longer drift with the implementation they check.

use hornvale_astronomy::{
    GeneratedSky, MoonsPin, RotationPin, SkyPins, SpinPin, StarSystem, StdDays, StdInstant,
    StellarTopology, anchor_state_at, calendar_of, generate, insolation_rel_at, luminosity_at,
    stellar_illumination_at, stellar_positions_at,
};
use hornvale_kernel::{EntityId, ObserverContext, PhenomenaSource, Seed, Venue, WorldTime};

/// The registered concept a sun is: every sky's day-sky body reports it.
const SUN_KIND: &str = "celestial-body";
/// The sun's salience. It is the unique maximum: nothing outranks it, and
/// nothing ties it.
const SUN_SALIENCE: f64 = 1.0;
/// The sun's entry in a rendered sky's `bodies` list, at every hour.
const SUN_BODY: &str = "the sun";

fn ctx(day: f64) -> ObserverContext {
    ObserverContext::at(
        EntityId::new(1).unwrap(),
        WorldTime::from_std_days(day).expect("a day value is finite"),
    )
}

/// The pin sets that span the rotation regimes a Hornvale sky must survive.
fn regimes() -> Vec<SkyPins> {
    vec![
        SkyPins::default(),
        SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        },
        SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            moons: Some(MoonsPin::graded(1, 2).unwrap()),
            ..SkyPins::default()
        },
        // Night-sky stage 2: a full wanderer complement must not disturb
        // the sun's rank either.
        SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            wanderers: Some(4),
            ..SkyPins::default()
        },
    ]
}

fn eccentric_system(pins: &SkyPins) -> StarSystem {
    let mut system = generate(Seed(42), pins).expect("seed 42 builds").value;
    system.forcing.ecc_mean = 0.2;
    system.forcing.ecc_amp = 0.0;
    system.forcing.ecc_phase = 0.0;
    system
}

fn turn_distance(a: f64, b: f64) -> f64 {
    ((a - b + 0.5).rem_euclid(1.0) - 0.5).abs()
}

/// Calendar season geometry follows the anchor's true orbital longitude.
/// Keeping the old mean-longitude projection makes this fail on eccentric
/// worlds. Rotation changes the local horizon, not the orbital longitude.
#[test]
fn calendar_anchor_coherence_covers_locked_and_retrograde_worlds() {
    let pin_sets = [
        SkyPins::default(),
        SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        },
        SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            spin: Some(SpinPin::Retrograde),
            ..SkyPins::default()
        },
    ];

    for pins in pin_sets {
        let system = eccentric_system(&pins);
        let calendar = calendar_of(&system);
        for day in [-10_000.0, -1.0, 0.0, 1.0, 10_000.0] {
            let instant = StdInstant::new(day).expect("finite instant");
            let state = anchor_state_at(&system, instant).expect("valid anchor orbit");
            let calendar_longitude = calendar
                .season_phase(instant)
                .expect("eccentricity defines a season");

            assert!(
                turn_distance(calendar_longitude, state.true_longitude_turns) < 1e-12,
                "day {day}: calendar {calendar_longitude} != anchor {}",
                state.true_longitude_turns
            );
        }
    }
}

/// Calendar longitude remains coherent on both sides of its wrap. Discovering
/// the wrap from the state avoids assuming mean and true anomaly cross zero at
/// the same instant.
#[test]
fn calendar_anchor_coherence_survives_the_true_longitude_wrap() {
    let system = eccentric_system(&SkyPins::default());
    let calendar = calendar_of(&system);
    let year = system.anchor.year.get();
    let mut before = StdInstant::new(0.0).expect("finite instant");
    let mut before_state = anchor_state_at(&system, before).expect("valid anchor orbit");
    let mut crossing = None;

    for step in 1..=4096 {
        let after = StdInstant::new(year * f64::from(step) / 4096.0).expect("finite instant");
        let after_state = anchor_state_at(&system, after).expect("valid anchor orbit");
        if after_state.true_longitude_turns < before_state.true_longitude_turns {
            crossing = Some((before, before_state, after, after_state));
            break;
        }
        before = after;
        before_state = after_state;
    }

    let (before, before_state, after, after_state) = crossing.expect("one orbit crosses zero");
    let before_calendar = calendar
        .season_phase(before)
        .expect("eccentricity defines a season");
    let after_calendar = calendar
        .season_phase(after)
        .expect("eccentricity defines a season");

    assert!(turn_distance(before_calendar, before_state.true_longitude_turns) < 1e-12);
    assert!(turn_distance(after_calendar, after_state.true_longitude_turns) < 1e-12);
    assert!(
        turn_distance(before_calendar, after_calendar) < 0.01,
        "calendar wrap must be angular, not a physical jump"
    );
}

/// Instantaneous stellar flux uses the evaluated anchor radius. Substituting
/// the semi-major axis makes the distance and inverse-square assertions fail;
/// the separate deep-time insolation function remains tied to that axis.
#[test]
fn insolation_anchor_coherence_uses_the_shared_instantaneous_radius() {
    let system = eccentric_system(&SkyPins {
        topology: Some(StellarTopology::Single),
        ..SkyPins::default()
    });

    for day in [-1234.5, 0.0, 9876.5] {
        let instant = StdInstant::new(day).expect("finite instant");
        let state = anchor_state_at(&system, instant).expect("valid anchor orbit");
        let illumination = stellar_illumination_at(&system, instant);
        let primary = illumination.sources.first().expect("single primary light");
        let expected_flux =
            luminosity_at(&system.star, instant).get() / (state.radius_au * state.radius_au);

        assert!((primary.distance.get() - state.radius_au).abs() < 1e-12);
        assert!((primary.flux_rel - expected_flux).abs() < 1e-12);
        assert!((illumination.combined_flux_rel - expected_flux).abs() < 1e-12);

        let semi_major_squared = system.anchor.orbit.get() * system.anchor.orbit.get();
        let expected_deep_time = luminosity_at(&system.star, instant).get() / semi_major_squared;
        assert_eq!(
            insolation_rel_at(&system.star, &system.anchor, instant),
            expected_deep_time,
            "deep-time insolation remains a semi-major-axis model"
        );
    }
}

/// Binary source distances use the anchor state's Cartesian position. Restoring
/// the legacy circular anchor position changes these distances on an eccentric
/// orbit even though the stars' own ephemerides are unchanged.
#[test]
fn insolation_anchor_coherence_uses_the_shared_position_for_binary_sources() {
    for topology in [StellarTopology::WideBinary, StellarTopology::CloseBinary] {
        let system = eccentric_system(&SkyPins {
            topology: Some(topology),
            ..SkyPins::default()
        });
        let instant = StdInstant::new(1234.5).expect("finite instant");
        let state = anchor_state_at(&system, instant).expect("valid anchor orbit");
        let stars = stellar_positions_at(&system, instant);
        let illumination = stellar_illumination_at(&system, instant);

        assert_eq!(illumination.sources.len(), stars.len());
        for (source, star) in illumination.sources.iter().zip(stars) {
            let dx = star.x_au - state.position_au[0];
            let dy = star.y_au - state.position_au[1];
            let expected_distance = if star.x_au == 0.0 && star.y_au == 0.0 {
                state.radius_au
            } else {
                (dx * dx + dy * dy).sqrt()
            };
            assert!(
                (source.distance.get() - expected_distance).abs() < 1e-12,
                "{topology:?} source {} distance {} != {expected_distance}",
                source.star,
                source.distance.get()
            );
        }
    }
}

/// The total illumination API retains its established origin fallback when a
/// malformed system cannot produce a physical anchor state.
#[test]
fn invalid_anchor_keeps_illumination_total_at_the_origin_fallback() {
    let mut system = eccentric_system(&SkyPins {
        topology: Some(StellarTopology::Single),
        ..SkyPins::default()
    });
    system.anchor.year = StdDays::new(0.0).expect("zero is a valid malformed period");

    let illumination =
        stellar_illumination_at(&system, StdInstant::new(0.0).expect("finite instant"));
    let primary = illumination.sources.first().expect("single primary light");
    assert_eq!(primary.distance.get(), 0.0);
    assert_eq!(primary.flux_rel, 0.0);
    assert_eq!(illumination.combined_flux_rel, 0.0);
}

/// There is a sun and it owns the day sky. Every generated sky keeps
/// exactly one top-salience DaySky body, whatever the seed, regime, or
/// hour.
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
#[test]
fn every_stellar_source_has_a_day_sky_sun() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 0.25, 10.5, 100.75, 3650.0] {
                let fine = sky.phenomena(&ctx(t));
                let suns: Vec<_> = fine
                    .iter()
                    .filter(|p| p.venue == Venue::DaySky && p.kind == SUN_KIND)
                    .collect();
                let expected = 1 + usize::from(sky.system().stellar.companion.is_some());
                assert_eq!(
                    suns.len(),
                    expected,
                    "seed {seed} t {t}: one sun per stellar source"
                );
                // The sun keeps its registered concept and its rank: the
                // registered kind, and the unique top salience.
                assert_eq!(suns[0].kind, SUN_KIND);
                assert_eq!(
                    suns[0].salience, SUN_SALIENCE,
                    "seed {seed} t {t}: the sun must stay the top-salience day-sky body"
                );
            }
        }
    }
}

/// Nothing outshines the sun. Moons, stars, seasons, and tides sit
/// strictly BENEATH it, so the sun stays the unique maximum, not merely a
/// co-maximum.
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
#[test]
fn nothing_in_a_sky_outranks_its_sun() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 10.5, 100.75] {
                let phenomena = sky.phenomena(&ctx(t));
                for p in &phenomena {
                    if p.venue == Venue::DaySky && p.kind == SUN_KIND {
                        continue;
                    }
                    assert!(
                        p.salience < 1.0,
                        "seed {seed} t {t}: {:?} (salience {}) rivals the sun",
                        p.referent,
                        p.salience
                    );
                }
                for p in phenomena.iter().filter(|p| p.kind == "eclipse") {
                    assert!(p.salience < 1.0, "an eclipse never outranks the sun");
                }
            }
        }
    }
}

/// "The sun" belongs in the sky's visible bodies at every hour, in every
/// generated sky (a superset, never a retraction), and the report is
/// deterministic.
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
#[test]
fn the_sun_never_leaves_the_visible_bodies_list() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 0.25, 10.5, 100.75] {
                let report =
                    sky.sky_at(WorldTime::from_std_days(t).expect("a day value is finite"));
                assert!(
                    report.bodies.contains(&SUN_BODY.to_string()),
                    "seed {seed} t {t}: the generated sky retracted the sun"
                );
                assert_eq!(
                    report.description,
                    sky.sky_at(WorldTime::from_std_days(t).expect("a day value is finite"))
                        .description,
                    "seed {seed} t {t}: report must be deterministic"
                );
            }
        }
    }
}

/// The sun may carry a period (a spinning world's day) or stay aperiodic
/// (a locked world) — but the period it carries must be the world's own
/// day length, not an invention.
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
#[test]
fn the_suns_added_period_is_the_day_the_calendar_already_holds() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            let day = sky.calendar().day_length().map(|d| d.get());
            let fine = sky.phenomena(&ctx(0.0));
            let sun = fine
                .iter()
                .find(|p| p.venue == Venue::DaySky && p.kind == SUN_KIND)
                .expect("the sun is always present");
            match day {
                // round2: the provider rounds periods for prose stability.
                Some(day) => {
                    let period = sun.period_days.expect("a spinning sun is periodic");
                    assert!(
                        (period - day).abs() <= 0.005 + 1e-12,
                        "seed {seed}: sun period {period} is not the day {day}"
                    );
                }
                None => assert_eq!(
                    sun.period_days, None,
                    "seed {seed}: a locked sun is aperiodic"
                ),
            }
        }
    }
}
