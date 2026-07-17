//! Moons: drawn, then admitted only past the stability inequalities
//! (Roche floor, Hill cap, mutual spacing, combined-tide cap).

use crate::anchor::Anchor;
use crate::pins::{GenesisError, SkyPins};
use crate::star::{Star, planet_age};
use crate::streams;
use crate::units::{GramsPerCm3, Gyr, LunarMasses, Megameters, StdDays};
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
    /// Bulk density in g/cm3 (The Reckoning). Mechanism-dependent: a
    /// `GiantImpact` moon's density is the **derived** constant 3.34
    /// (re-accreted mantle debris, no iron core — exactly why Luna is 3.34
    /// against Earth's 5.51); a `Capture` moon's density is **drawn** from
    /// a different reservoir entirely ({rocky 3.0, icy 1.6}). Every moon
    /// still consumes one draw from `MOON_DENSITY` regardless of branch —
    /// see `generate_moons`.
    pub density: GramsPerCm3,
    /// Age in Gyr, pre-genesis (The Reckoning). Mechanism-dependent: a
    /// `GiantImpact` moon is coeval with its planet (`planet_age` minus a
    /// small **drawn** jitter — Luna is 4.51 against Earth's 4.54); a
    /// `Capture` moon's age is **drawn** independently of the planet's,
    /// decoupled, because the body formed elsewhere. Every moon still
    /// consumes one draw from `MOON_AGE` regardless of branch — see
    /// `generate_moons`.
    pub age: Gyr,
}

/// The anchor's Hill radius in Mm (model card formula).
/// type-audit: pending(wave-1)
pub fn hill_radius_mm(star: &Star, anchor: &Anchor) -> f64 {
    anchor.orbit.0 * math::powf(anchor.mass.0 * 3.003e-6 / (3.0 * star.mass.0), 1.0 / 3.0) * 1.496e5
}

const ATTEMPTS_PER_MOON: u32 = 128;
const TIDE_CAP: f64 = 8.0;

/// The probability that a moon admitted at `distance` Mm is a captured stray,
/// given the system's admitted ceiling `max_distance` Mm. The single
/// definition: `generate_moons` and the Luna calibration test both call this,
/// so the test pins the production formula rather than a copy of it (an
/// earlier version recomputed the arithmetic inline and therefore could not
/// catch a regression in this function at all).
///
/// The distance proxy is physical: an impact child forms close and tidally
/// recedes, while irregular satellites are distant. `frac` is cubed rather
/// than used linearly — see `generate_moons`'s call site for the Luna
/// calibration that forced it.
fn capture_probability(distance: f64, max_distance: f64) -> f64 {
    let span = (max_distance - 60.0).max(1e-9);
    let frac = (distance - 60.0) / span;
    (frac * frac * frac).clamp(0.02, 0.85)
}

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
        // Placeholder; overwritten after formation is known (The
        // Reckoning), like formation/inclination/node above it.
        density: GramsPerCm3(3.34),
        // Placeholder; overwritten after formation is known (The
        // Reckoning), like formation/inclination/node above it.
        age: Gyr(0.0),
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
    //
    // The map is `frac` CUBED, not linear (Nathan-ratified recalibration).
    // A linear map failed its own calibration: at Luna's real distance
    // (384.4 Mm from Earth, frac ≈ 0.386 of this model's admitted range)
    // the linear map called the Earth-Moon system — this model's own
    // GiantImpact exemplar — a capture 39% of the time. Cubing pushes
    // mid-range fracs down hard (0.386³ ≈ 0.057) while leaving the ends
    // (0³ = 0, 1³ = 1) fixed, so Luna reads as an impact child ~94% of the
    // time and the floor/ceiling still bound the tails. The floor also
    // drops 0.10 → 0.02 (the ceiling stays 0.85): cubing already suppresses
    // the low end, so the old linear-era floor would have overridden the
    // cubed value for a wide swath of close-in fracs.
    let mut formations = astronomy_seed.derive(streams::MOON_FORMATION).stream();
    for moon in &mut moons {
        let p_capture = capture_probability(moon.distance.0, max_distance);
        moon.formation = if formations.next_f64() < p_capture {
            Formation::Capture
        } else {
            Formation::GiantImpact
        };
    }

    // SKY-6: inclinations draw from their own stream, after admission and
    // the distance sort, so every pre-eclipse draw (count, masses,
    // distances) is byte-identical and the draws are index-stable.
    //
    // The Reckoning: this is the epoch, and it is a PARTIAL one. Both
    // branches consume EXACTLY ONE draw from this same stream, whichever is
    // taken, so index-stability holds regardless of which mechanism a moon
    // drew — and the GiantImpact formula is byte-identical to the pre-epoch
    // one, so an all-impact world's inclinations (and everything derived
    // from them: nodes, eclipses) never move. Only worlds that actually
    // receive a captured moon change.
    let mut inclinations = astronomy_seed.derive(streams::MOON_INCLINATIONS).stream();
    for moon in &mut moons {
        let roll = inclinations.next_f64();
        moon.inclination_deg = match moon.formation {
            // Regular: prograde, low inclination. The pre-epoch formula.
            Formation::GiantImpact => roll * 10.0,
            // Irregular. Above 90° the orbit is retrograde — Triton's case.
            Formation::Capture => 20.0 + roll * 140.0,
        };
    }

    // Eclipse Seasons: node longitudes draw from their own stream, after
    // the inclinations, so every pre-node draw is byte-identical and the
    // draws are index-stable.
    let mut nodes = astronomy_seed.derive(streams::MOON_NODES).stream();
    for moon in &mut moons {
        moon.node_longitude_deg = nodes.next_f64() * 360.0;
    }

    // The Reckoning (Task 5): density draws from its own stream, after
    // formation is known, so nothing above this line moves. Every moon
    // consumes EXACTLY ONE `next_f64()` here in BOTH branches, even
    // `GiantImpact`, whose density is a derived constant and needs no
    // draw at all — draw and discard it anyway. If `GiantImpact` skipped
    // the draw, moon i's density would depend on how many *earlier* moons
    // happened to draw `Capture`, so flipping moon 0's formation would
    // silently shift every later moon's density. Same index-stability
    // discipline as SKY-6 and this campaign's own inclination epoch.
    let mut densities = astronomy_seed.derive(streams::MOON_DENSITY).stream();
    for moon in &mut moons {
        let roll = densities.next_f64();
        moon.density = match moon.formation {
            // Derived, not drawn: re-accreted mantle debris with no iron
            // core. This is exactly why Luna is 3.34 against Earth's
            // 5.51. The roll above is discarded.
            Formation::GiantImpact => GramsPerCm3(3.34),
            // Drawn from a different reservoir entirely: a captured body
            // may be rocky or icy, unrelated to the anchor's composition.
            Formation::Capture => {
                if roll < 0.5 {
                    GramsPerCm3(3.0)
                } else {
                    GramsPerCm3(1.6)
                }
            }
        };
    }

    // The Reckoning (Task 5): age draws from its own stream, after
    // formation is known, for the same index-stability reason as density
    // above — one draw per moon in both branches.
    let planet_age_gyr = planet_age(star).0;
    let mut ages = astronomy_seed.derive(streams::MOON_AGE).stream();
    for moon in &mut moons {
        let roll = ages.next_f64();
        moon.age = match moon.formation {
            // Coeval with the planet: the impact happened during
            // accretion, so the moon's age trails the planet's by only a
            // small jitter. Luna is 4.51 against Earth's 4.54.
            Formation::GiantImpact => {
                let jitter = 0.03 + roll * (0.10 - 0.03);
                Gyr((planet_age_gyr - jitter).max(0.0))
            }
            // Decoupled: the body formed elsewhere, at some earlier point
            // in the planet's history, and was captured later.
            Formation::Capture => Gyr((0.05 + roll * (0.95 - 0.05)) * planet_age_gyr),
        };
    }

    Ok((moons, notes))
}

/// Bulk radius in km from mass and density: r = (3M / 4piρ)^(1/3).
/// Derived — and derived from a REAL density, not an assumed one, which is
/// the whole point of knowing the formation mechanism (The Reckoning).
/// type-audit: pending(wave-1)
pub fn radius_km(moon: &Moon) -> f64 {
    const LUNAR_MASS_KG: f64 = 7.342e22;
    let m_kg = moon.mass.0 * LUNAR_MASS_KG;
    let rho_kg_m3 = moon.density.0 * 1000.0;
    let v = 3.0 * m_kg / (4.0 * std::f64::consts::PI * rho_kg_m3);
    math::powf(v, 1.0 / 3.0) / 1000.0
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

    /// The Reckoning: every admitted moon draws an inclination whose range is
    /// conditioned on its `formation` — impact moons stay in the pre-epoch
    /// [0, 10)° band, captured moons draw the wide irregular [20, 160] band
    /// — deterministically, from the same single `MOON_INCLINATIONS` stream
    /// draw either way (index-stability; the pre-inclination draws (count,
    /// masses, distances) are pinned unchanged by `golden_seed_42.rs`).
    #[test]
    fn every_moon_draws_an_inclination_in_range() {
        for seed in 0..64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for moon in &moons {
                match moon.formation {
                    Formation::GiantImpact => assert!(
                        (0.0..10.0).contains(&moon.inclination_deg),
                        "seed {seed}: impact moon inclination {}",
                        moon.inclination_deg
                    ),
                    Formation::Capture => assert!(
                        (20.0..=160.0).contains(&moon.inclination_deg),
                        "seed {seed}: captured moon inclination {}",
                        moon.inclination_deg
                    ),
                }
            }
        }
    }

    /// The Reckoning: restates the range check as a distribution claim per
    /// mechanism, over a wider seed sweep than `every_moon_draws_an_
    /// inclination_in_range` — the brief's Step 1 test, asserting against
    /// production `generate_moons` rather than pinning its own arithmetic.
    #[test]
    fn impact_moons_are_regular_and_captured_moons_are_irregular() {
        for seed in 0..400u64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for m in &moons {
                match m.formation {
                    Formation::GiantImpact => assert!(
                        (0.0..=10.0).contains(&m.inclination_deg),
                        "seed {seed}: impact moon at {}°",
                        m.inclination_deg
                    ),
                    Formation::Capture => assert!(
                        (20.0..=160.0).contains(&m.inclination_deg),
                        "seed {seed}: captured moon at {}°",
                        m.inclination_deg
                    ),
                }
            }
        }
    }

    /// The Reckoning: the campaign's visible deliverable — worlds that could
    /// never have a retrograde moon before now can. Above 90° is retrograde;
    /// among captured moons the rate should land in a broad plausible band,
    /// neither ~0 nor ~1.
    #[test]
    fn some_captured_moons_are_retrograde() {
        let mut retrograde = 0u32;
        let mut captured = 0u32;
        for seed in 0..400u64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for m in moons.iter().filter(|m| m.formation == Formation::Capture) {
                captured += 1;
                if m.inclination_deg > 90.0 {
                    retrograde += 1;
                }
            }
        }
        assert!(
            captured > 20,
            "too few captured moons to judge ({captured})"
        );
        let rate = f64::from(retrograde) / f64::from(captured);
        assert!(
            (0.3..0.7).contains(&rate),
            "retrograde rate {rate} among {captured} captured"
        );
    }

    /// The Reckoning, spec §7: the partial-epoch property. Seed 218 (under
    /// this file's `system()` helper, a raw seed) admits 3 moons that all
    /// draw `GiantImpact` — checked with a throwaway probe, not asserted
    /// here. Because the `GiantImpact` branch's formula is byte-identical to
    /// the pre-epoch code, and both branches consume exactly one draw from
    /// the same `MOON_INCLINATIONS` stream, an all-impact world's
    /// inclinations must be unchanged.
    ///
    /// These three values are the pre-campaign ones: recorded by checking
    /// out `d30bcfd` (the commit immediately before this task) into a
    /// separate temporary worktree and printing `generate_moons(Seed(218),
    /// ...)`'s inclinations there directly — not by running the post-change
    /// code and pasting what it prints, which would pin whatever the code
    /// does and prove nothing.
    #[test]
    fn an_all_impact_world_is_byte_identical_to_the_pre_campaign_draw() {
        let (star, anchor) = system(218);
        let (moons, _) = generate_moons(Seed(218), &star, &anchor, &SkyPins::default()).unwrap();
        assert_eq!(moons.len(), 3);
        assert!(
            moons.iter().all(|m| m.formation == Formation::GiantImpact),
            "seed 218 must be an all-impact world for this property to test anything"
        );
        let expected = [4.474260413733722, 2.778959967462058, 6.084528594520218];
        for (m, want) in moons.iter().zip(expected) {
            assert_eq!(m.inclination_deg, want, "seed 218 inclination drifted");
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

    /// The Reckoning, Task 5: proves the "discard the roll, don't skip the
    /// draw" rule actually holds, which no range/membership assertion above
    /// can — a bug that skips `MOON_DENSITY`'s/`MOON_AGE`'s draw on the
    /// `GiantImpact` branch would shift a *later* `Capture` moon's density
    /// (still lands in {3.0, 1.6}) and age (still lands in `[0, planet_age]`)
    /// to a DIFFERENT value in that same set/range — invisible to a
    /// membership check, visible only to an exact pin. Seed 373 admits three
    /// moons under this file's `system()` helper with formation
    /// GiantImpact/Capture/GiantImpact — a mix in both directions (impact
    /// before capture and capture before impact) — so a stream-position
    /// bug in either branch's draw shows up here. These values are the
    /// production output of THIS commit's `generate_moons`, captured by
    /// running the code once and pasting its result (unlike the pre-epoch
    /// byte-identity tests above, there is no "before" value to independently
    /// recover: this property did not exist before this task).
    #[test]
    fn density_and_age_draws_are_index_stable_across_mixed_formation() {
        let (star, anchor) = system(373);
        let (moons, _) = generate_moons(Seed(373), &star, &anchor, &SkyPins::default()).unwrap();
        let formations: Vec<Formation> = moons.iter().map(|m| m.formation).collect();
        assert_eq!(
            formations,
            vec![
                Formation::GiantImpact,
                Formation::Capture,
                Formation::GiantImpact
            ],
            "seed 373 must mix formations in both directions for this property to test anything"
        );
        let expected: &[(f64, f64)] = &[
            (3.34, 0.6316516151065772),
            (1.6, 0.5941737938444442),
            (3.34, 0.6758533809396974),
        ];
        for (m, (density, age)) in moons.iter().zip(expected) {
            assert_eq!(m.density.0, *density, "seed 373 density drifted");
            assert_eq!(m.age.0, *age, "seed 373 age drifted");
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
    /// These values are the pre-campaign ones — captured from `dfb0782`
    /// (the commit immediately before the formation draw was introduced),
    /// via this file's `system()` helper (a raw `Seed(9)`, not the
    /// `"astronomy"`-derived root `generate()` uses — see `golden_seed_42.rs`
    /// for that path) — by checking out `dfb0782` into a separate worktree
    /// and printing `generate_moons(Seed(9), ...)`'s output directly, not by
    /// running the current code: the formation draw pulls from its own
    /// `MOON_FORMATION` stream *after* `moons.sort_by`, so it cannot move
    /// these values, but the whole point of this test is to catch it if a
    /// future change accidentally makes it able to. Seed 9 admits 2 moons
    /// under this helper (seed 42 admits none, which made the prior version
    /// of this test vacuous — `assert_eq!(0, 0)` — and unable to fail even
    /// when a reviewer injected an extra draw into the admission loop).
    #[test]
    fn masses_and_distances_are_untouched_by_the_formation_draw() {
        let (star, anchor) = system(9);
        let (moons, _) = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        let expected: &[(f64, f64)] = &[
            (1.2683733853675314, 342.5295368494006),
            (0.4635424548468193, 581.0120533420021),
        ];
        assert_eq!(moons.len(), expected.len());
        for (m, (mass, dist)) in moons.iter().zip(expected) {
            assert_eq!(m.mass.0, *mass);
            assert_eq!(m.distance.0, *dist);
        }
    }

    /// The Reckoning, Task 5, spec §5.2: a `GiantImpact` moon's density is
    /// the derived constant 3.34 (mantle debris, no iron core) and its age
    /// trails the planet's by only the drawn jitter — always coeval, never
    /// decoupled. A distribution claim over seeds, calling production
    /// `generate_moons` and `planet_age` rather than recomputing either.
    #[test]
    fn impact_moons_are_iron_poor_and_coeval() {
        let mut checked = 0u32;
        for seed in 0..300u64 {
            let (star, anchor) = system(seed);
            let p_age = planet_age(&star);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for m in moons
                .iter()
                .filter(|m| m.formation == Formation::GiantImpact)
            {
                checked += 1;
                assert_eq!(m.density.0, 3.34, "seed {seed}");
                let gap = p_age.0 - m.age.0;
                assert!((0.03..=0.10).contains(&gap), "seed {seed}: gap {gap} Gyr");
            }
        }
        assert!(checked > 20, "too few impact moons to judge ({checked})");
    }

    /// The Reckoning, Task 5, spec §5.2: a `Capture` moon's density is drawn
    /// from a different reservoir entirely ({rocky 3.0, icy 1.6}) and its
    /// age is decoupled from the planet's (drawn independently, always
    /// within `[0, planet_age]` since the body cannot postdate its own
    /// capture-by-a-planet-that-already-exists).
    #[test]
    fn captured_moons_have_alien_density_and_decoupled_age() {
        let mut icy = 0u32;
        let mut captured = 0u32;
        for seed in 0..300u64 {
            let (star, anchor) = system(seed);
            let p_age = planet_age(&star);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            for m in moons.iter().filter(|m| m.formation == Formation::Capture) {
                captured += 1;
                assert!(
                    m.density.0 == 3.0 || m.density.0 == 1.6,
                    "seed {seed}: {}",
                    m.density.0
                );
                if m.density.0 == 1.6 {
                    icy += 1;
                }
                assert!(
                    m.age.0 <= p_age.0,
                    "seed {seed}: captured moon older than its planet"
                );
                assert!(m.age.0 >= 0.0);
            }
        }
        assert!(captured > 20, "too few captured moons ({captured})");
        assert!(icy > 0, "no icy captured body in 300 seeds");
    }

    /// The Reckoning, Task 5: the campaign's payload — radius follows from
    /// mass and a REAL density, not an assumption. Builds the base moon from
    /// a real `generate_moons` output (never a from-scratch literal) so the
    /// non-density/mass fields are production values, then overrides mass
    /// and density to the Luna calibration point from the brief.
    #[test]
    fn radius_follows_from_mass_and_real_density_not_an_assumption() {
        let (star, anchor) = system(9);
        let (moons, _) = generate_moons(Seed(9), &star, &anchor, &SkyPins::default()).unwrap();
        let base = moons.into_iter().next().expect("seed 9 has moons");
        // Luna: 1.0 lunar mass at 3.34 g/cm3 -> ~1737 km.
        let luna = Moon {
            mass: LunarMasses(1.0),
            density: GramsPerCm3(3.34),
            ..base
        };
        let r = radius_km(&luna);
        assert!((r - 1737.0).abs() < 30.0, "Luna radius came out {r} km");
        // An icy body of the same mass is larger.
        let icy = Moon {
            density: GramsPerCm3(1.6),
            ..luna
        };
        assert!(radius_km(&icy) > r * 1.2);
    }

    /// The Reckoning, Nathan-ratified recalibration: at Luna's real distance
    /// (384.4 Mm from Earth), the linear weighting map called the model's
    /// own `GiantImpact` exemplar a capture 39% of the time — the model
    /// contradicting itself. Pins the fix: cubing `frac` before clamping
    /// drives `p_capture` for a Luna-like `frac` (384.4 / (900 - 60) at this
    /// system's `max_distance` ceiling of 900 Mm, ≈ 0.386) down to
    /// ≈ 0.057 = 0.386³, comfortably under the asserted 0.10 bound, so Luna
    /// reads as an impact child in the overwhelming majority of draws
    /// (≈ 94%, not asserted here directly — see the measured single-moon
    /// and inner/outer rates below for the draw-level confirmation).
    #[test]
    fn luna_like_distance_reads_as_impact_child() {
        // Calls the production `capture_probability` — NOT a copy of its
        // arithmetic. An earlier version recomputed the formula inline, so
        // reverting `generate_moons` to the linear map left this test
        // passing: it pinned nothing.
        let p_capture = capture_probability(384.4, 900.0);
        assert!(
            p_capture < 0.10,
            "Luna-like p_capture {p_capture} should be small — the whole point of cubing"
        );
    }

    /// The Reckoning: formation is weighted by distance, so the innermost
    /// moon of a multi-moon system is almost always an impact child, the
    /// outermost is often a stray, and — the coverage gap the code review
    /// found — single-moon worlds (about half of all mooned worlds) are
    /// mostly impact children too. A distribution claim over seeds, not a
    /// per-seed assertion.
    ///
    /// Root cause of *why* the innermost admitted moon isn't near frac=0
    /// (measured, not the mutual-spacing story an earlier version of this
    /// comment gave, which the code review A/B'd and found backwards):
    /// spacing alone pushes the innermost moon's mean frac *down* (0.163 vs
    /// 0.308 unconstrained), and the tide cap pushes it back *up* — the two
    /// nearly cancel (pure-uniform-with-both-constraints predicts frac ≈
    /// 0.6819; the generator measures 0.6818). The real driver is an order
    /// statistic: with every constraint switched off, the minimum of k
    /// uniform draws still averages 1/(k+1) of the range (1/3 for k=2, 1/4
    /// for k=3) — the innermost slot is a minimum-of-several, not a single
    /// draw near the floor, regardless of the admission constraints.
    #[test]
    fn the_innermost_moon_is_an_impact_child_and_the_outermost_tends_to_stray() {
        let (mut inner_impact, mut inner_total) = (0u32, 0u32);
        let (mut outer_capture, mut outer_total) = (0u32, 0u32);
        let (mut single_impact, mut single_total) = (0u32, 0u32);
        for seed in 0..400u64 {
            let (star, anchor) = system(seed);
            let (moons, _) =
                generate_moons(Seed(seed), &star, &anchor, &SkyPins::default()).unwrap();
            if moons.len() == 1 {
                single_total += 1;
                if moons[0].formation == Formation::GiantImpact {
                    single_impact += 1;
                }
            }
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
            inner_total > 20 && outer_total > 20 && single_total > 20,
            "too few seeds to judge: inner {inner_total} outer {outer_total} single {single_total}"
        );
        let inner_rate = f64::from(inner_impact) / f64::from(inner_total);
        let outer_rate = f64::from(outer_capture) / f64::from(outer_total);
        let single_rate = f64::from(single_impact) / f64::from(single_total);
        // Measured against the cubed weighting formula over seeds 0..400
        // (deterministic, not flaky): inner_rate = 0.9396, outer_rate =
        // 0.5549, single_rate = 0.6272. Thresholds set with headroom below
        // each measured value, not pasted from the brief's predictions —
        // inner and outer landed close to the brief's ≈0.93/≈0.56, but
        // single_rate landed well below its ≈0.71 prediction (reported to
        // the campaign, not silently accepted): a lone moon has no sibling
        // to space against, only its own tide cap, so its distance isn't
        // pulled from the same order-statistic distribution the inner slot
        // is — the tide cap alone appears to bias admitted single-moon
        // distances outward more than the naive near-uniform prediction
        // assumed.
        assert!(inner_rate > 0.85, "innermost impact rate {inner_rate}");
        assert!(outer_rate > 0.45, "outermost capture rate {outer_rate}");
        // Single-moon worlds are the blind spot the code review found: the
        // old test's `continue` on `moons.len() < 2` never looked at them,
        // and they are ~half of all mooned worlds — exactly where the
        // linear map was worst (40.5% impact, barely above coin-flip).
        assert!(single_rate > 0.55, "single-moon impact rate {single_rate}");
    }
}
