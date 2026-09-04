//! The sky-conformance battery: what a Hornvale sky *is*. Every generated
//! sky, on every seed, in every rotation regime, at every hour, keeps four
//! claims true — there is exactly one day-sky sun, nothing outranks it, it
//! is never retracted from the visible bodies, and any period it carries is
//! the calendar's own day.
//!
//! These were the claims the retired tier-0 provider stipulated and the
//! cross-tier refinement battery checked the generated sky against
//! ("coarse constrains fine"). The tier is gone; the claims were always the
//! part doing the work, so they are asserted directly (decisions 0736 and
//! 0738).
//!
//! The two constants below were previously read out of `ConstantSun` at
//! test time — production code inside the crate under test. Frozen here,
//! they can no longer drift with the implementation they check.

use hornvale_astronomy::{GeneratedSky, MoonsPin, RotationPin, SkyPins, generate};
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

/// There is a sun and it owns the day sky. Every generated sky keeps
/// exactly one top-salience DaySky body, whatever the seed, regime, or
/// hour.
/// claim: sanctioned-sweep(mixed-regime battery, 3 of 4 sub-regimes
/// pinned — no census home for the pinned sub-regimes)
#[test]
fn every_sky_has_exactly_one_day_sky_sun() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 0.25, 10.5, 100.75, 3650.0] {
                let fine = sky.phenomena(&ctx(t));
                let suns: Vec<_> = fine
                    .iter()
                    .filter(|p| p.venue == Venue::DaySky && p.kind == SUN_KIND)
                    .collect();
                assert_eq!(suns.len(), 1, "seed {seed} t {t}: exactly one day-sky sun");
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
