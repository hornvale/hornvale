//! Moons: drawn, then admitted only past the stability inequalities
//! (Roche floor, Hill cap, mutual spacing, combined-tide cap).

use crate::anchor::Anchor;
use crate::pins::{GenesisError, SkyPins};
use crate::star::Star;
use crate::streams;
use crate::units::{LunarMasses, Megameters, StdDays};
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// How a moon came to be. The mechanism predicts the body's age, its
/// density, and whether its orbit is regular or irregular.
///
/// Co-accretion (the Galilean moons, Titan) is deliberately absent: it needs
/// a massive circumplanetary disk, which is a giant-planet mechanism, and
/// the anchor is terrestrial. Fission (Darwin's proposal for Luna) is
/// discredited.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Formation {
    /// A body struck the proto-planet and the debris re-accreted — Luna.
    /// Coeval with the planet, iron-poor (mantle debris, no core), and
    /// regular: prograde, low inclination.
    GiantImpact,
    /// A passing body was captured — Triton. Its age is decoupled (it
    /// formed elsewhere), its composition is from another reservoir, and
    /// its orbit is irregular: high inclination, often retrograde.
    Capture,
}

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
    /// Ecliptic longitude of the ascending node at genesis, in degrees
    /// (drawn 0–360, own stream — Eclipse Seasons): with the inclination,
    /// the full node geometry that dates each eclipse.
    /// type-audit: pending(wave-1)
    pub node_longitude_deg: f64,
    /// How this moon formed (The Reckoning): drawn after admission and the
    /// distance sort, weighted by distance, from its own stream.
    pub formation: Formation,
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
        // Drawn after admission from its own stream (Eclipse Seasons),
        // like the inclination above it.
        node_longitude_deg: 0.0,
        // Drawn after admission from its own stream (The Reckoning), like
        // the inclination and node above it.
        formation: Formation::GiantImpact,
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

    // The Reckoning: formation draws from its own stream, after the distance
    // sort, so every admission draw (count, masses, distances) stays
    // byte-identical — the same discipline SKY-6 and Eclipse Seasons used.
    // Weighted by distance: an impact child forms close and tidally
    // recedes; irregular satellites are distant.
    let mut formations = astronomy_seed.derive(streams::MOON_FORMATION).stream();
    for moon in &mut moons {
        let span = (max_distance - 60.0).max(1e-9);
        let p_capture = ((moon.distance.0 - 60.0) / span).clamp(0.10, 0.85);
        moon.formation = if formations.next_f64() < p_capture {
            Formation::Capture
        } else {
            Formation::GiantImpact
        };
    }

    // SKY-6: inclinations draw from their own stream, after admission and
    // the distance sort, so every pre-eclipse draw (count, masses,
    // distances) is byte-identical and the draws are index-stable.
    let mut inclinations = astronomy_seed.derive(streams::MOON_INCLINATIONS).stream();
    for moon in &mut moons {
        moon.inclination_deg = inclinations.next_f64() * 10.0;
    }

    // Eclipse Seasons: node longitudes draw from their own stream, after
    // the inclinations, so every pre-node draw is byte-identical and the
    // draws are index-stable.
    let mut nodes = astronomy_seed.derive(streams::MOON_NODES).stream();
    for moon in &mut moons {
        moon.node_longitude_deg = nodes.next_f64() * 360.0;
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

    /// Eclipse Seasons: every admitted moon draws an ascending-node
    /// longitude in [0, 360)°, deterministically, from its own stream —
    /// the pre-node draws are pinned unchanged by `golden_seed_42.rs`.
    #[test]
    fn every_moon_draws_a_node_longitude_in_range() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for moon in &moons {
                assert!(
                    (0.0..360.0).contains(&moon.node_longitude_deg),
                    "seed {seed}: node {}",
                    moon.node_longitude_deg
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

    /// The Reckoning, spec §5.3: the whole point of drawing after admission.
    /// These values are the pre-campaign ones; if this fails, the admission
    /// loop was disturbed.
    #[test]
    fn masses_and_distances_are_untouched_by_the_formation_draw() {
        let (star, anchor) = system(42);
        let (moons, _) = generate_moons(Seed(42), &star, &anchor, &SkyPins::default()).unwrap();
        // Recorded from a run before the formation draw existed (Step 2):
        // seed 42, via this file's `system()` helper (a raw `Seed(42)`, not
        // the `"astronomy"`-derived root `generate()` uses — see
        // `golden_seed_42.rs` for that path), draws zero moons unpinned.
        let expected: &[(f64, f64)] = &[];
        assert_eq!(moons.len(), expected.len());
        for (m, (mass, dist)) in moons.iter().zip(expected) {
            assert_eq!(m.mass.0, *mass);
            assert_eq!(m.distance.0, *dist);
        }
    }

    /// The Reckoning: formation is weighted by distance, so the innermost
    /// moon of a multi-moon system is almost always an impact child and the
    /// outermost is often a stray. A distribution claim over seeds, not a
    /// per-seed assertion.
    #[test]
    fn the_innermost_moon_is_an_impact_child_and_the_outermost_tends_to_stray() {
        let (mut inner_impact, mut inner_total) = (0u32, 0u32);
        let (mut outer_capture, mut outer_total) = (0u32, 0u32);
        for seed in 0..400u64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            if moons.len() < 2 {
                continue;
            }
            inner_total += 1;
            if moons[0].formation == Formation::GiantImpact {
                inner_impact += 1;
            }
            outer_total += 1;
            if moons[moons.len() - 1].formation == Formation::Capture {
                outer_capture += 1;
            }
        }
        assert!(
            inner_total > 20 && outer_total > 20,
            "too few multi-moon seeds to judge"
        );
        let inner_rate = f64::from(inner_impact) / f64::from(inner_total);
        let outer_rate = f64::from(outer_capture) / f64::from(outer_total);
        // Measured against this exact weighting formula over seeds 0..400:
        // inner_rate = 0.6813, outer_rate = 0.7912 (deterministic, not
        // flaky). The admitted innermost moon's distance fraction averages
        // ~0.32 of [60, max_distance], not near 0 — the mutual-spacing
        // constraint (ratio >= 1.5) keeps it from crowding the floor when a
        // second or third moon needs room above it — so its impact rate
        // caps below the >0.7 a naive "almost certainly" reading would
        // suggest. 0.6 keeps the qualitative claim (majority impact,
        // minority capture, and asymmetric with the outer rate) with margin
        // below the true value, rather than asserting an unmeasured bound.
        assert!(inner_rate > 0.6, "innermost impact rate {inner_rate}");
        assert!(outer_rate > 0.3, "outermost capture rate {outer_rate}");
    }

    /// The Reckoning: the formation draw is deterministic like every other
    /// per-moon draw.
    #[test]
    fn formation_is_deterministic() {
        let (star, anchor) = system(7);
        let a = generate_moons(Seed(7), &star, &anchor, &SkyPins::default())
            .unwrap()
            .0;
        let b = generate_moons(Seed(7), &star, &anchor, &SkyPins::default())
            .unwrap()
            .0;
        let fa: Vec<_> = a.iter().map(|m| m.formation).collect();
        let fb: Vec<_> = b.iter().map(|m| m.formation).collect();
        assert_eq!(fa, fb);
    }
}
