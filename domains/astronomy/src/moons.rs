//! Moons: drawn, then admitted only past the stability inequalities
//! (Roche floor, Hill cap, mutual spacing, combined-tide cap).

use crate::anchor::Anchor;
use crate::pins::{GenesisError, SkyPins};
use crate::star::Star;
use crate::streams;
use crate::units::{LunarMasses, Megameters, StdDays};
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// A moon of the anchor world. Mass and distance drawn; the rest derived.
/// type-audit: bare-ok(ratio)
#[derive(Debug, Clone, PartialEq)]
pub struct Moon {
    /// Mass in lunar masses (drawn, 0.05–2.5).
    pub mass: LunarMasses,
    /// Orbital distance in Mm (drawn within Roche/Hill bounds).
    pub distance: Megameters,
    /// Orbital period in standard days (derived: Kepler III).
    pub period: StdDays,
    /// Apparent size relative to Luna-from-Earth (derived).
    pub angular_diameter_rel: f64,
    /// Tidal strength relative to Luna-on-Earth (derived: m/d³).
    pub tide_rel: f64,
    /// Orbital inclination to the anchor's orbital plane, in degrees
    /// (drawn 0–10, own stream — SKY-6): the node geometry that decides
    /// how often a new moon actually crosses the sun.
    /// type-audit: pending(wave-1)
    pub inclination_deg: f64,
}

/// The anchor's Hill radius in Mm (model card formula).
/// type-audit: pending(wave-1)
pub fn hill_radius_mm(star: &Star, anchor: &Anchor) -> f64 {
    anchor.orbit.0 * math::powf(anchor.mass.0 * 3.003e-6 / (3.0 * star.mass.0), 1.0 / 3.0) * 1.496e5
}

const ATTEMPTS_PER_MOON: u32 = 128;
const TIDE_CAP: f64 = 8.0;

fn derive_moon(mass: f64, distance: f64, anchor: &Anchor) -> Moon {
    Moon {
        mass: LunarMasses(mass),
        distance: Megameters(distance),
        period: StdDays(27.32 * ((distance / 384.4).powi(3) / anchor.mass.0).sqrt()),
        angular_diameter_rel: math::powf(mass, 1.0 / 3.0) * 384.4 / distance,
        tide_rel: mass / (distance / 384.4).powi(3),
        // Drawn after admission from its own stream (SKY-6), so the
        // admission draws above stay byte-identical to the pre-eclipse
        // save format.
        inclination_deg: 0.0,
    }
}

/// Generate the moons: count drawn or pinned; each admitted only past the
/// stability inequalities, with a bounded redraw budget. Returns moons and
/// notes about any degradation that occurred.
/// type-audit: bare-ok(prose)
pub fn generate_moons(
    astronomy_seed: Seed,
    star: &Star,
    anchor: &Anchor,
    pins: &SkyPins,
) -> Result<(Vec<Moon>, Vec<String>), GenesisError> {
    let (count, min) = match pins.moons {
        Some(pin) => (pin.want(), pin.min()),
        None => {
            let roll = astronomy_seed
                .derive(streams::MOON_COUNT)
                .stream()
                .range_u32(1, 100);
            let drawn_count = match roll {
                1..=15 => 0,
                16..=55 => 1,
                56..=85 => 2,
                _ => 3,
            };
            (drawn_count, 0)
        }
    };

    let hill = hill_radius_mm(star, anchor);
    let max_distance = (0.4 * hill).min(900.0);
    let mut stream = astronomy_seed.derive(streams::MOONS).stream();
    let mut moons: Vec<Moon> = Vec::new();
    let mut notes: Vec<String> = Vec::new();

    for index in 0..count {
        let mut admitted = false;
        for _ in 0..ATTEMPTS_PER_MOON {
            let mass = 0.05 + stream.next_f64() * 2.45;
            let distance = 60.0 + stream.next_f64() * (max_distance - 60.0).max(0.0);
            let candidate = derive_moon(mass, distance, anchor);
            let spacing_ok = moons.iter().all(|m| {
                let (near, far) = if m.distance.0 < distance {
                    (m.distance.0, distance)
                } else {
                    (distance, m.distance.0)
                };
                far / near >= 1.5
            });
            let tide_total: f64 =
                moons.iter().map(|m| m.tide_rel).sum::<f64>() + candidate.tide_rel;
            if distance >= 20.0 && distance <= 0.4 * hill && spacing_ok && tide_total <= TIDE_CAP {
                moons.push(candidate);
                admitted = true;
                break;
            }
        }
        if !admitted {
            let sought = index + 1;
            if (moons.len() as u32) < min {
                return Err(GenesisError::UnsatisfiablePin {
                    pin: "moons".to_string(),
                    reason: format!(
                        "moon {sought} of {count} found no stable orbit within the attempt budget \
                         (Hill radius {hill:.0} Mm, tide cap {TIDE_CAP})"
                    ),
                });
            }
            notes.push(format!(
                "moon {sought} of {count} was sought; no stable orbit exists within the \
                 Hill sphere, spacing, and tide budget"
            ));
            break;
        }
    }

    moons.sort_by(|a, b| a.distance.0.total_cmp(&b.distance.0));
    // SKY-6: inclinations draw from their own stream, after admission and
    // the distance sort, so every pre-eclipse draw (count, masses,
    // distances) is byte-identical and the draws are index-stable.
    let mut inclinations = astronomy_seed.derive(streams::MOON_INCLINATIONS).stream();
    for moon in &mut moons {
        moon.inclination_deg = inclinations.next_f64() * 10.0;
    }
    Ok((moons, notes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::anchor::generate_anchor;
    use crate::pins::MoonsPin;
    use crate::star::generate_star;

    fn system(seed: u64) -> (Star, Anchor) {
        let star = generate_star(Seed(seed));
        let anchor = generate_anchor(Seed(seed), &star, &SkyPins::default()).unwrap();
        (star, anchor)
    }

    #[test]
    fn graded_pin_degrades_with_a_note_above_min() {
        // Seed 10 cannot hold a third moon; graded 2+1 accepts 2 + a note.
        let (star, anchor) = system(10);
        let pins = SkyPins {
            moons: Some(MoonsPin::graded(2, 1).unwrap()),
            ..SkyPins::default()
        };
        let (moons, notes) = generate_moons(Seed(10), &star, &anchor, &pins).unwrap();
        assert_eq!(moons.len(), 2);
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("moon 3 of 3 was sought"));
    }

    #[test]
    fn graded_pin_fails_loudly_below_min() {
        let (star, anchor) = system(10);
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(3).unwrap()),
            ..SkyPins::default()
        };
        assert!(matches!(
            generate_moons(Seed(10), &star, &anchor, &pins),
            Err(GenesisError::UnsatisfiablePin { .. })
        ));
    }

    #[test]
    fn drawn_degradation_is_noted() {
        let (star, anchor) = system(10);
        let (moons, notes) = generate_moons(Seed(10), &star, &anchor, &SkyPins::default()).unwrap();
        assert!(moons.len() < 3);
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("was sought"));
    }

    #[test]
    fn moons_pin_constructors_validate() {
        assert!(MoonsPin::exact(3).is_ok());
        assert!(MoonsPin::exact(4).is_err());
        assert!(MoonsPin::graded(2, 1).is_ok());
        assert!(MoonsPin::graded(2, 2).is_err());
        let p = MoonsPin::graded(1, 2).unwrap();
        assert_eq!((p.min(), p.want()), (1, 3));
    }

    #[test]
    fn moon_count_pin_is_honored_for_every_legal_value() {
        let (star, anchor) = system(42);
        for count in 0..=3u32 {
            let pins = SkyPins {
                moons: Some(MoonsPin::exact(count).unwrap()),
                ..SkyPins::default()
            };
            let (moons, _) = generate_moons(Seed(42), &star, &anchor, &pins).unwrap();
            assert_eq!(moons.len() as u32, count);
        }
    }

    #[test]
    fn drawn_counts_degrade_when_no_stable_slot_exists() {
        // Seed 10 draws 3 moons but has no feasible third slot; unpinned
        // generation degrades to the admitted set instead of failing.
        let (star, anchor) = system(10);
        let (moons, notes) = generate_moons(Seed(10), &star, &anchor, &SkyPins::default()).unwrap();
        assert!(moons.len() < 3 && !moons.is_empty());
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("was sought"));
    }

    #[test]
    fn every_generated_moon_satisfies_the_inequalities() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            let hill = hill_radius_mm(&star, &anchor);
            let mut previous: Option<f64> = None;
            let mut total_tide = 0.0;
            for moon in &moons {
                assert!(moon.distance.get() >= 20.0, "Roche floor");
                assert!(moon.distance.get() <= 0.4 * hill, "Hill cap");
                if let Some(prev) = previous {
                    assert!(moon.distance.get() / prev >= 1.5, "mutual spacing");
                }
                previous = Some(moon.distance.get());
                total_tide += moon.tide_rel;
                // Derived quantities match the model card.
                let expected_period =
                    27.32 * ((moon.distance.get() / 384.4).powi(3) / anchor.mass.get()).sqrt();
                assert!((moon.period.get() - expected_period).abs() < 1e-9);
                let expected_theta =
                    math::powf(moon.mass.get(), 1.0 / 3.0) * 384.4 / moon.distance.get();
                assert!((moon.angular_diameter_rel - expected_theta).abs() < 1e-9);
                let expected_tide = moon.mass.get() / (moon.distance.get() / 384.4).powi(3);
                assert!((moon.tide_rel - expected_tide).abs() < 1e-9);
            }
            assert!(total_tide <= 8.0, "combined tide cap");
        }
    }

    /// SKY-6: every admitted moon draws an inclination in [0, 10)°,
    /// deterministically, from its own stream — the pre-eclipse draws
    /// (count, masses, distances) are pinned unchanged by
    /// `golden_seed_42.rs`.
    #[test]
    fn every_moon_draws_an_inclination_in_range() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for moon in &moons {
                assert!(
                    (0.0..10.0).contains(&moon.inclination_deg),
                    "seed {seed}: inclination {}",
                    moon.inclination_deg
                );
            }
        }
    }

    #[test]
    fn moons_are_deterministic() {
        let (star, anchor) = system(9);
        let a = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        let b = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        assert_eq!(a, b);
    }
}
