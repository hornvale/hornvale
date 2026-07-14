//! Sky genesis facts: the generated system's headline parameters, committed
//! to the world entity's ledger with provenance `"astronomy"` (spec §8).
//! Downstream systems never read these directly — they exist for
//! persistence, interrogation ("why"-style queries), and reconstruction
//! (`sky_of` at the application layer folds `scenario-pin` facts back
//! through `pins::parse_pin`).

use crate::anchor::Rotation;
use crate::system::GenesisOutcome;
use hornvale_kernel::{EntityId, Fact, LedgerError, Value, World};

/// The host star's descriptive spectral class (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const STAR_CLASS: &str = "star-class";
/// The anchor world is tidally locked: no local solar day exists
/// (functional, Flag).
/// type-audit: bare-ok(identifier-text)
pub const TIDALLY_LOCKED: &str = "tidally-locked";
/// Solar day length in standard days, for spinning worlds only
/// (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const DAY_LENGTH_STD: &str = "day-length-std";
/// Year length in standard days (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const YEAR_LENGTH_STD: &str = "year-length-std";
/// Axial tilt of the anchor world, in degrees (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const OBLIQUITY_DEGREES: &str = "obliquity-degrees";
/// How many moons the anchor world has (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const MOON_COUNT: &str = "moon-count";
/// Orbital period of a moon, in standard days (non-functional, Number —
/// one per moon).
/// type-audit: bare-ok(identifier-text)
pub const MOON_PERIOD_STD: &str = "moon-period-std";
/// Tidal strength of a moon relative to Luna-on-Earth (non-functional,
/// Number — one per moon; SKY-5).
/// type-audit: bare-ok(identifier-text)
pub const MOON_TIDE_REL: &str = "moon-tide-rel";
/// The anchor world spins backward: the sun rises in the west (functional,
/// Flag — committed only when true, like tidally-locked; SKY-22).
/// type-audit: bare-ok(identifier-text)
pub const RETROGRADE_SPIN: &str = "retrograde-spin";
/// Orbital inclination of a moon to the anchor's orbital plane, in degrees
/// (non-functional, Number — one per moon; SKY-6).
/// type-audit: bare-ok(identifier-text)
pub const MOON_INCLINATION_DEGREES: &str = "moon-inclination-degrees";
/// Mass of a moon in lunar masses (non-functional, Number — one per moon).
/// type-audit: bare-ok(identifier-text)
pub const MOON_MASS_LUNAR: &str = "moon-mass-lunar";
/// Orbital distance of a moon in megameters (non-functional, Number — one
/// per moon).
/// type-audit: bare-ok(identifier-text)
pub const MOON_DISTANCE_MM: &str = "moon-distance-mm";
/// Apparent size of a moon relative to Luna-from-Earth (non-functional,
/// Number — one per moon; derived).
/// type-audit: bare-ok(identifier-text)
pub const MOON_ANGULAR_SIZE_REL: &str = "moon-angular-size-rel";
/// A notable neighbor star visible in the night sky (non-functional, Text —
/// one per neighbor).
/// type-audit: bare-ok(identifier-text)
pub const NOTABLE_NEIGHBOR: &str = "notable-neighbor";
/// A degradation or refusal recorded during sky genesis (non-functional,
/// Text — one per note).
/// type-audit: bare-ok(identifier-text)
pub const GENESIS_NOTE: &str = "genesis-note";
/// Which astronomy provider a world uses: `"constant"` or `"generated"`
/// (functional, Text).
/// type-audit: bare-ok(identifier-text)
pub const SKY_PROVIDER: &str = "sky-provider";
/// An experimenter-supplied pin string conditioning genesis, in
/// `pins::pin_strings` format (non-functional, Text — one per pin).
/// type-audit: bare-ok(identifier-text)
pub const SCENARIO_PIN: &str = "scenario-pin";
/// Mean orbital eccentricity (deep-time forcing) (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const ECCENTRICITY_MEAN: &str = "eccentricity-mean";
/// Obliquity oscillation amplitude, degrees (moon-coupled) (functional,
/// Number).
/// type-audit: bare-ok(identifier-text)
pub const OBLIQUITY_AMPLITUDE: &str = "obliquity-amplitude";
/// Host star mass in solar masses (functional, Number; drawn).
/// type-audit: bare-ok(identifier-text)
pub const STAR_MASS_SOLAR: &str = "star-mass-solar";
/// Host star luminosity in solar units (functional, Number; derived M^3.5).
/// type-audit: bare-ok(identifier-text)
pub const STAR_LUMINOSITY_SOLAR: &str = "star-luminosity-solar";
/// Habitable-zone inner bound in AU (functional, Number; derived 0.95√L).
/// type-audit: bare-ok(identifier-text)
pub const HAB_ZONE_INNER_AU: &str = "hab-zone-inner-au";
/// Habitable-zone outer bound in AU (functional, Number; derived 1.37√L).
/// type-audit: bare-ok(identifier-text)
pub const HAB_ZONE_OUTER_AU: &str = "hab-zone-outer-au";
/// Anchor world mass in Earth masses (functional, Number; drawn).
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_MASS_EARTH: &str = "anchor-mass-earth";
/// Anchor orbital distance in AU (functional, Number; drawn or pinned).
/// type-audit: bare-ok(identifier-text)
pub const ANCHOR_ORBIT_AU: &str = "anchor-orbit-au";
/// Insolation at the anchor relative to Earth (functional, Number; derived
/// L/a², global annual mean).
/// type-audit: bare-ok(identifier-text)
pub const INSOLATION_REL: &str = "insolation-rel";

fn fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "astronomy".to_string(),
    }
}

/// Commit the generated system's headline parameters as facts about
/// `subject` (the world entity): star class; tidally-locked flag OR
/// day-length, depending on rotation regime; year length; obliquity;
/// moon count; one moon-period-std per moon; one notable-neighbor per
/// neighbor; one genesis-note per recorded degradation.
pub fn genesis(
    world: &mut World,
    subject: EntityId,
    outcome: &GenesisOutcome,
) -> Result<(), LedgerError> {
    let system = &outcome.system;

    world.ledger.commit(
        fact(
            subject,
            STAR_CLASS,
            Value::Text(system.star.class_name.clone()),
        ),
        &world.registry,
    )?;

    match system.anchor.rotation {
        Rotation::Locked => {
            world.ledger.commit(
                fact(subject, TIDALLY_LOCKED, Value::Flag(true)),
                &world.registry,
            )?;
        }
        Rotation::Spinning { day, retrograde } => {
            world.ledger.commit(
                fact(subject, DAY_LENGTH_STD, Value::Number(day.get())),
                &world.registry,
            )?;
            if retrograde {
                world.ledger.commit(
                    fact(subject, RETROGRADE_SPIN, Value::Flag(true)),
                    &world.registry,
                )?;
            }
        }
    }

    world.ledger.commit(
        fact(
            subject,
            YEAR_LENGTH_STD,
            Value::Number(system.anchor.year.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            OBLIQUITY_DEGREES,
            Value::Number(system.anchor.obliquity.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            MOON_COUNT,
            Value::Number(system.moons.len() as f64),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            ECCENTRICITY_MEAN,
            Value::Number(system.forcing.ecc_mean),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            OBLIQUITY_AMPLITUDE,
            Value::Number(system.forcing.obliquity_amp),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            STAR_MASS_SOLAR,
            Value::Number(system.star.mass.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            STAR_LUMINOSITY_SOLAR,
            Value::Number(system.star.luminosity.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HAB_ZONE_INNER_AU,
            Value::Number(system.star.habitable_zone.inner().get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            HAB_ZONE_OUTER_AU,
            Value::Number(system.star.habitable_zone.outer().get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            ANCHOR_MASS_EARTH,
            Value::Number(system.anchor.mass.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            ANCHOR_ORBIT_AU,
            Value::Number(system.anchor.orbit.get()),
        ),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(
            subject,
            INSOLATION_REL,
            Value::Number(crate::star::insolation_rel(&system.star, &system.anchor)),
        ),
        &world.registry,
    )?;

    for moon in &system.moons {
        world.ledger.commit(
            fact(subject, MOON_PERIOD_STD, Value::Number(moon.period.get())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(subject, MOON_TIDE_REL, Value::Number(moon.tide_rel)),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                MOON_INCLINATION_DEGREES,
                Value::Number(moon.inclination_deg),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(subject, MOON_MASS_LUNAR, Value::Number(moon.mass.get())),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                MOON_DISTANCE_MM,
                Value::Number(moon.distance.get()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                MOON_ANGULAR_SIZE_REL,
                Value::Number(moon.angular_diameter_rel),
            ),
            &world.registry,
        )?;
    }

    for neighbor in &system.neighbors {
        let description = format!(
            "a {} star at {:.0} light-years",
            neighbor.color,
            neighbor.distance.get()
        );
        world.ledger.commit(
            fact(subject, NOTABLE_NEIGHBOR, Value::Text(description)),
            &world.registry,
        )?;
    }

    for note in &outcome.notes {
        world.ledger.commit(
            fact(subject, GENESIS_NOTE, Value::Text(note.clone())),
            &world.registry,
        )?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pins::{MoonsPin, RotationPin, SkyPins};
    use crate::register_concepts;
    use crate::system::generate;
    use hornvale_kernel::Seed;

    fn world_with(seed: u64) -> World {
        let mut w = World::new(Seed(seed));
        register_concepts(&mut w.registry).unwrap();
        w
    }

    #[test]
    fn genesis_commits_the_expected_facts_for_a_locked_two_moon_system() {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        assert_eq!(
            w.ledger.value_of(subject, TIDALLY_LOCKED),
            Some(&Value::Flag(true))
        );
        assert!(w.ledger.value_of(subject, DAY_LENGTH_STD).is_none());
        assert_eq!(
            w.ledger.value_of(subject, MOON_COUNT),
            Some(&Value::Number(2.0))
        );
        assert_eq!(
            w.ledger
                .facts_about(subject)
                .filter(|f| f.predicate == MOON_PERIOD_STD)
                .count(),
            2
        );
        assert!(
            w.ledger
                .facts_about(subject)
                .filter(|f| f.predicate == NOTABLE_NEIGHBOR)
                .count()
                >= 2
        );
        assert!(w.ledger.value_of(subject, STAR_CLASS).is_some());
        assert!(w.ledger.value_of(subject, YEAR_LENGTH_STD).is_some());
        assert!(w.ledger.value_of(subject, OBLIQUITY_DEGREES).is_some());
    }

    #[test]
    fn genesis_commits_the_forcing_parameters() {
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        // The ledger quantizes numeric objects on commit, so the stored value
        // is the canonical form of the raw forcing parameter.
        assert_eq!(
            w.ledger.value_of(subject, ECCENTRICITY_MEAN),
            Some(&Value::Number(hornvale_kernel::quantize(
                outcome.system.forcing.ecc_mean
            )))
        );
        assert_eq!(
            w.ledger.value_of(subject, OBLIQUITY_AMPLITUDE),
            Some(&Value::Number(hornvale_kernel::quantize(
                outcome.system.forcing.obliquity_amp
            )))
        );
    }

    #[test]
    fn genesis_records_a_degradation_note_for_default_pins() {
        // Seed 23's default (unpinned) generation degrades to fewer moons
        // than drawn; genesis must carry that refusal forward as a fact.
        let outcome = generate(Seed(23), &SkyPins::default()).unwrap();
        assert!(
            !outcome.notes.is_empty(),
            "test setup must actually exercise a degradation"
        );
        let mut w = world_with(23);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        let notes: Vec<&str> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == GENESIS_NOTE)
            .filter_map(|f| match &f.object {
                Value::Text(t) => Some(t.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(notes.len(), 1);
        assert!(notes[0].contains("was sought"));
    }

    /// SKY-5: the tide is computed at genesis and must reach the ledger —
    /// one moon-tide-rel fact per moon, quantized on commit like every
    /// numeric object.
    #[test]
    fn genesis_commits_one_tide_fact_per_moon() {
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        let tides: Vec<f64> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == MOON_TIDE_REL)
            .filter_map(|f| match f.object {
                Value::Number(n) => Some(n),
                _ => None,
            })
            .collect();
        let expected: Vec<f64> = outcome
            .system
            .moons
            .iter()
            .map(|m| hornvale_kernel::quantize(m.tide_rel))
            .collect();
        assert_eq!(tides, expected);
    }

    /// SKY-6: each moon's node geometry reaches the ledger — one
    /// inclination fact per moon, quantized on commit.
    #[test]
    fn genesis_commits_one_inclination_fact_per_moon() {
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        let inclinations: Vec<f64> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == MOON_INCLINATION_DEGREES)
            .filter_map(|f| match f.object {
                Value::Number(n) => Some(n),
                _ => None,
            })
            .collect();
        let expected: Vec<f64> = outcome
            .system
            .moons
            .iter()
            .map(|m| hornvale_kernel::quantize(m.inclination_deg))
            .collect();
        assert_eq!(inclinations, expected);
    }

    /// SKY-15: mass, distance, and angular size reach the ledger — one of
    /// each per moon.
    #[test]
    fn genesis_commits_mass_distance_and_size_per_moon() {
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        for pred in [MOON_MASS_LUNAR, MOON_DISTANCE_MM, MOON_ANGULAR_SIZE_REL] {
            assert_eq!(
                w.ledger
                    .facts_about(subject)
                    .filter(|f| f.predicate == pred)
                    .count(),
                2,
                "expected 2 {pred} facts"
            );
        }
    }

    /// SKY-22: a backward-spinning world says so in the ledger; an ordinary
    /// one stays silent (the flag is committed only when true, like
    /// tidally-locked).
    #[test]
    fn genesis_commits_the_retrograde_flag_only_when_retrograde() {
        let retro_pins = SkyPins {
            spin: Some(crate::pins::SpinPin::Retrograde),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(1), &retro_pins).unwrap();
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        assert_eq!(
            w.ledger.value_of(subject, RETROGRADE_SPIN),
            Some(&Value::Flag(true))
        );

        let outcome = generate(Seed(1), &SkyPins::default()).unwrap();
        assert!(matches!(
            outcome.system.anchor.rotation,
            crate::anchor::Rotation::Spinning {
                retrograde: false,
                ..
            }
        ));
        let mut w = world_with(1);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        assert!(w.ledger.value_of(subject, RETROGRADE_SPIN).is_none());
    }

    #[test]
    fn genesis_is_deterministic() {
        let outcome = generate(Seed(42), &SkyPins::default()).unwrap();
        let mut a = world_with(42);
        let subject_a = a.ledger.mint_entity();
        genesis(&mut a, subject_a, &outcome).unwrap();

        let mut b = world_with(42);
        let subject_b = b.ledger.mint_entity();
        genesis(&mut b, subject_b, &outcome).unwrap();

        assert_eq!(a.to_json(), b.to_json());
    }

    #[test]
    fn every_committed_fact_has_astronomy_provenance() {
        let outcome = generate(Seed(3), &SkyPins::default()).unwrap();
        let mut w = world_with(3);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        assert!(
            w.ledger
                .facts_about(subject)
                .all(|f| f.provenance == "astronomy")
        );
    }

    #[test]
    fn genesis_commits_the_star_and_anchor_numbers() {
        let outcome = generate(Seed(42), &SkyPins::default()).unwrap();
        let mut w = world_with(42);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();
        for pred in [
            STAR_MASS_SOLAR,
            STAR_LUMINOSITY_SOLAR,
            HAB_ZONE_INNER_AU,
            HAB_ZONE_OUTER_AU,
            ANCHOR_MASS_EARTH,
            ANCHOR_ORBIT_AU,
            INSOLATION_REL,
        ] {
            assert!(w.ledger.value_of(subject, pred).is_some(), "missing {pred}");
        }
        assert_eq!(
            w.ledger.value_of(subject, INSOLATION_REL),
            Some(&Value::Number(hornvale_kernel::quantize(
                crate::star::insolation_rel(&outcome.system.star, &outcome.system.anchor)
            )))
        );
    }
}
