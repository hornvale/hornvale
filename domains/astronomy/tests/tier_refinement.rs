//! SKY-23: the cross-tier refinement battery — "coarse constrains fine"
//! (Constitution): the generated sky (tier 1/2) must REFINE the tier-0
//! `ConstantSun` without contradicting it. Tier 0's whole claim is small:
//! there is a sun, it owns the day sky, and nothing outshines it. Every
//! generated sky, on every seed, in every rotation regime, at every hour,
//! must keep that claim true — it may only add structure (a spectral
//! class, a period, moons, stars, tides) beneath it.

use hornvale_astronomy::{
    CELESTIAL_BODY, ConstantSun, GeneratedSky, MoonsPin, RotationPin, SkyPins, generate,
};
use hornvale_kernel::{EntityId, ObserverContext, PhenomenaSource, Seed, Venue, WorldTime};

fn ctx(day: f64) -> ObserverContext {
    ObserverContext::at(EntityId(1), WorldTime { day })
}

/// The pin sets that span the rotation regimes tier 0 must survive.
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
        // the sun's tier-0 rank either.
        SkyPins {
            rotation: Some(RotationPin::PeriodHours(24.0)),
            wanderers: Some(4),
            ..SkyPins::default()
        },
    ]
}

/// Tier 0's first claim: there is a sun and it owns the day sky. Every
/// generated sky keeps exactly one top-salience DaySky body, whatever the
/// seed, regime, or hour.
#[test]
fn every_generated_sky_keeps_the_one_sun_tier_0_promises() {
    let tier0 = ConstantSun.phenomena(&ctx(0.0));
    assert_eq!(tier0.len(), 1, "tier 0 claims exactly one thing");
    let coarse_sun = &tier0[0];

    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 0.25, 10.5, 100.75, 3650.0] {
                let fine = sky.phenomena(&ctx(t));
                let suns: Vec<_> = fine
                    .iter()
                    .filter(|p| p.venue == Venue::DaySky && p.kind == CELESTIAL_BODY)
                    .collect();
                assert_eq!(
                    suns.len(),
                    1,
                    "seed {seed} t {t}: exactly one day-sky sun, like tier 0"
                );
                // The refined sun keeps the coarse sun's kind and its rank:
                // same registered concept, same top salience.
                assert_eq!(suns[0].kind, coarse_sun.kind);
                assert_eq!(
                    suns[0].salience, coarse_sun.salience,
                    "seed {seed} t {t}: refinement must not demote the sun"
                );
            }
        }
    }
}

/// Tier 0's second claim: nothing outshines the sun. Refinement adds
/// moons, stars, seasons, and tides BENEATH it — strictly, so the sun
/// stays the unique maximum, not merely a co-maximum.
#[test]
fn refinement_adds_structure_only_beneath_the_sun() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 10.5, 100.75] {
                let phenomena = sky.phenomena(&ctx(t));
                for p in &phenomena {
                    if p.venue == Venue::DaySky && p.kind == CELESTIAL_BODY {
                        continue;
                    }
                    assert!(
                        p.salience < 1.0,
                        "seed {seed} t {t}: '{}' (salience {}) rivals the sun",
                        p.description,
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

/// Tier 0's report puts "the sun" in the sky's visible bodies at every
/// hour; the generated report keeps it there (a superset, never a
/// retraction), and keeps the report deterministic like tier 0's.
#[test]
fn the_sun_never_leaves_the_visible_bodies_list() {
    let coarse = ConstantSun.sky_at(WorldTime { day: 0.0 });
    assert_eq!(coarse.bodies, vec!["the sun".to_string()]);

    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            for t in [0.0, 0.25, 10.5, 100.75] {
                let report = sky.sky_at(WorldTime { day: t });
                assert!(
                    report.bodies.contains(&"the sun".to_string()),
                    "seed {seed} t {t}: the generated sky retracted the sun"
                );
                assert_eq!(
                    report.description,
                    sky.sky_at(WorldTime { day: t }).description,
                    "seed {seed} t {t}: report must be deterministic"
                );
            }
        }
    }
}

/// Refinement may add a period to the sun (a spinning world's day) or
/// keep it aperiodic (a locked world, like tier 0 itself) — but the
/// period it adds must be the world's own day length, not an invention.
#[test]
fn the_suns_added_period_is_the_day_the_calendar_already_holds() {
    for pins in regimes() {
        for seed in 0..32u64 {
            let sky = GeneratedSky::new(generate(Seed(seed), &pins).unwrap());
            let day = sky.calendar().day_length().map(|d| d.get());
            let fine = sky.phenomena(&ctx(0.0));
            let sun = fine
                .iter()
                .find(|p| p.venue == Venue::DaySky && p.kind == CELESTIAL_BODY)
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
                    "seed {seed}: a locked sun is aperiodic, like tier 0's"
                ),
            }
        }
    }
}
