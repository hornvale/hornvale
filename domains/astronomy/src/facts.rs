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
/// Flags a minted entity as a notable neighbor star (functional, Flag).
/// type-audit: bare-ok(identifier-text)
pub const IS_NEIGHBOR: &str = "is-neighbor";
/// Spectral-class name of a neighbor star (functional, Text; one per entity).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_CLASS: &str = "neighbor-class";
/// Distance to a neighbor star in light-years (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_DISTANCE_LY: &str = "neighbor-distance-ly";
/// Apparent brightness of a neighbor, relative units (functional, Number; derived L/d²).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_BRIGHTNESS_REL: &str = "neighbor-brightness-rel";
/// Declination of a neighbor in degrees from the celestial equator (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_DECLINATION_DEG: &str = "neighbor-declination-deg";
/// Right ascension of a neighbor in degrees (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBOR_RA_DEG: &str = "neighbor-ra-deg";
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
/// A bright star stands within 10 degrees of the north celestial pole at
/// genesis (functional, Number = separation degrees; epoch-scoped:
/// precession retires pole stars).
/// type-audit: bare-ok(identifier-text)
pub const POLE_STAR_NORTH: &str = "pole-star-north";
/// A bright star stands within 10 degrees of the south celestial pole at
/// genesis (functional, Number = separation degrees; epoch-scoped:
/// precession retires pole stars).
/// type-audit: bare-ok(identifier-text)
pub const POLE_STAR_SOUTH: &str = "pole-star-south";
/// How many wandering planets cross this sky (functional, Number).
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_COUNT_FACT: &str = "wanderer-count";
/// Orbital distance of a wanderer, in AU (non-functional, Number — one per
/// wanderer, innermost order).
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_ORBIT_AU: &str = "wanderer-orbit-au";
/// Orbital period of a wanderer, in standard days (non-functional, Number —
/// one per wanderer, innermost order).
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_PERIOD_STD: &str = "wanderer-period-std";
/// A wanderer's kind: rock or giant (non-functional, Text — deduped to the
/// distinct kinds present in this sky; NOT zippable 1:1 against wanderer
/// index — orbit/period facts carry per-wanderer identity).
/// type-audit: bare-ok(identifier-text)
pub const WANDERER_CLASS: &str = "wanderer-class";
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
/// Predicate: the star's fractional main-sequence brightening per
/// gigayear (The Long Count).
/// type-audit: bare-ok(identifier-text)
pub const BRIGHTENING_PER_GYR: &str = "brightening-per-gyr";
/// How many star figures the reference observer's sky holds (functional,
/// Number).
/// type-audit: bare-ok(identifier-text)
pub const FIGURE_COUNT: &str = "figure-count";
/// Member count of a star figure (non-functional, Number — one per figure,
/// output order: descending member count, then centroid right ascension
/// ascending). Deduped: repeated member counts collapse to one committed
/// fact, the same honest characteristic as any other coarse non-functional
/// Number/Text fact with no other per-figure differentiator (see
/// [`WANDERER_CLASS`]'s doc for the precedent).
/// type-audit: bare-ok(identifier-text)
pub const FIGURE_MEMBERS: &str = "figure-members";
/// Sky region of a star figure: `"northern sky"` / `"southern sky"` /
/// `"the equator's road"`, by centroid declination (non-functional, Text —
/// one per figure, same output order and dedup characteristic as
/// [`FIGURE_MEMBERS`]).
/// type-audit: bare-ok(identifier-text)
pub const FIGURE_REGION: &str = "figure-region";
/// A star figure stands on the sun's road — the ecliptic band (functional,
/// Flag — committed only when true, like [`TIDALLY_LOCKED`]). Because
/// `Flag(true)` is identical across every figure, this fact records "at
/// least one figure is on the ecliptic", not a per-figure list; a caller
/// wanting the per-figure detail must recompute from [`crate::figures`].
/// type-audit: bare-ok(identifier-text)
pub const FIGURE_ON_ECLIPTIC: &str = "figure-on-ecliptic";

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
/// moon count; one moon-period-std per moon; one neighbor entity (with its
/// own structured facts) per neighbor; a pole-star fact (north or south,
/// never both) when the genesis-epoch night sky finds one; wanderer count
/// plus one orbit and period fact per wanderer (innermost order) and the
/// distinct wanderer kinds present (class facts dedupe — see
/// [`WANDERER_CLASS`]); figure count plus one members/region fact per
/// figure (output order) and whether any figure stands on the ecliptic
/// (dedupes — see [`FIGURE_ON_ECLIPTIC`]); one genesis-note per recorded
/// degradation.
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
    world.ledger.commit(
        fact(
            subject,
            BRIGHTENING_PER_GYR,
            Value::Number(crate::star::brightening_per_gyr(&system.star)),
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
        let id = world.ledger.mint_entity();
        world
            .ledger
            .commit(fact(id, IS_NEIGHBOR, Value::Flag(true)), &world.registry)?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_CLASS,
                Value::Text(crate::neighborhood::class_name(neighbor.class).to_string()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_DISTANCE_LY,
                Value::Number(neighbor.distance.get()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_BRIGHTNESS_REL,
                Value::Number(neighbor.apparent_brightness),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                id,
                NEIGHBOR_DECLINATION_DEG,
                Value::Number(neighbor.declination),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(id, NEIGHBOR_RA_DEG, Value::Number(neighbor.right_ascension)),
            &world.registry,
        )?;
    }

    // Pole star: a system-level, latitude-independent fact of the night sky
    // at genesis (day 0) — derive the unified view once and commit the
    // matching predicate, never both (spec §2 epoch honesty; precession
    // retires pole stars, so this is a genesis-epoch snapshot, not an
    // ongoing truth).
    let calendar = crate::calendar::calendar_of(system);
    let sky = crate::night_sky::night_sky_at(system, &calendar, 0.0, crate::units::StdDays(0.0));
    if let Some(pole_star) = &sky.pole_star {
        let predicate = match pole_star.pole {
            crate::night_sky::Hemisphere::North => POLE_STAR_NORTH,
            crate::night_sky::Hemisphere::South => POLE_STAR_SOUTH,
        };
        world.ledger.commit(
            fact(subject, predicate, Value::Number(pole_star.separation_deg)),
            &world.registry,
        )?;
    }

    world.ledger.commit(
        fact(
            subject,
            WANDERER_COUNT_FACT,
            Value::Number(system.wanderers.len() as f64),
        ),
        &world.registry,
    )?;
    for wanderer in &system.wanderers {
        world.ledger.commit(
            fact(
                subject,
                WANDERER_ORBIT_AU,
                Value::Number(wanderer.orbit.get()),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                WANDERER_PERIOD_STD,
                Value::Number(wanderer.period.get()),
            ),
            &world.registry,
        )?;
        let class_name = match wanderer.class {
            crate::wanderers::WandererClass::Rock => "rock",
            crate::wanderers::WandererClass::Giant => "giant",
        };
        world.ledger.commit(
            fact(subject, WANDERER_CLASS, Value::Text(class_name.to_string())),
            &world.registry,
        )?;
    }

    // Star figures (night-sky stage 3): the reference observer's sky
    // clusters into notable groups. `astronomy_seed` matches the same
    // derivation genesis itself performs (`system::generate`), so the
    // catalog `figures` clusters over is exactly the one the almanac and
    // lab metrics see.
    let astronomy_seed = world.seed.derive(crate::streams::ROOT);
    let figs = crate::figures::figures(astronomy_seed, system);
    world.ledger.commit(
        fact(subject, FIGURE_COUNT, Value::Number(figs.len() as f64)),
        &world.registry,
    )?;
    for figure in &figs {
        world.ledger.commit(
            fact(
                subject,
                FIGURE_MEMBERS,
                Value::Number(figure.member_count as f64),
            ),
            &world.registry,
        )?;
        world.ledger.commit(
            fact(
                subject,
                FIGURE_REGION,
                Value::Text(crate::figures::region_word(figure.centroid.dec_deg).to_string()),
            ),
            &world.registry,
        )?;
        if figure.on_ecliptic {
            world.ledger.commit(
                fact(subject, FIGURE_ON_ECLIPTIC, Value::Flag(true)),
                &world.registry,
            )?;
        }
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
            w.ledger.find(IS_NEIGHBOR).count() >= 2,
            "at least two neighbor entities"
        );
        assert!(w.ledger.value_of(subject, STAR_CLASS).is_some());
        assert!(w.ledger.value_of(subject, YEAR_LENGTH_STD).is_some());
        assert!(w.ledger.value_of(subject, OBLIQUITY_DEGREES).is_some());
        assert_eq!(
            w.ledger
                .facts_about(subject)
                .filter(|f| f.predicate == BRIGHTENING_PER_GYR)
                .count(),
            1
        );
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
        assert!(w.ledger.iter().all(|f| f.provenance == "astronomy"));
    }

    #[test]
    fn genesis_mints_one_neighbor_entity_per_neighbor_with_structured_facts() {
        let outcome = generate(Seed(3), &SkyPins::default()).unwrap();
        let mut w = world_with(3);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        let neighbor_ids: Vec<_> = w.ledger.find(IS_NEIGHBOR).map(|f| f.subject).collect();
        assert_eq!(neighbor_ids.len(), outcome.system.neighbors.len());
        for id in neighbor_ids {
            assert!(w.ledger.value_of(id, NEIGHBOR_CLASS).is_some());
            assert!(w.ledger.value_of(id, NEIGHBOR_DISTANCE_LY).is_some());
            assert!(w.ledger.value_of(id, NEIGHBOR_BRIGHTNESS_REL).is_some());
        }
        // The opaque blob is gone.
        assert_eq!(
            w.ledger
                .iter()
                .filter(|f| f.predicate == "notable-neighbor")
                .count(),
            0
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

    /// Sweep seeds 0..64, unpinned: for each, derive the night-sky view
    /// independently of `genesis` and assert the committed facts match it
    /// exactly — a pole-star fact present iff the view finds one, matching
    /// hemisphere, matching quantized separation, and never both facts at
    /// once. Also guards against a vacuous sweep (a prior review flagged
    /// the risk): at least one seed must actually commit a pole-star fact,
    /// or this test fails loudly with the per-seed minimum separations so
    /// the 10-degree threshold can get controller attention.
    #[test]
    fn genesis_commits_pole_star_facts_matching_the_derived_view_across_a_seed_sweep() {
        use crate::night_sky::{Hemisphere, night_sky_at};
        use crate::sky_position::EquatorialCoord;
        use crate::units::StdDays;

        let mut any_committed = false;
        let mut min_separations: Vec<(u64, f64)> = Vec::new();

        for seed in 0..64u64 {
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();

            let calendar = crate::calendar::calendar_of(&outcome.system);
            let sky = night_sky_at(&outcome.system, &calendar, 0.0, StdDays(0.0));

            let north = w.ledger.value_of(subject, POLE_STAR_NORTH).cloned();
            let south = w.ledger.value_of(subject, POLE_STAR_SOUTH).cloned();

            match sky.pole_star {
                Some(ps) if ps.pole == Hemisphere::North => {
                    assert_eq!(
                        north,
                        Some(Value::Number(hornvale_kernel::quantize(ps.separation_deg))),
                        "seed {seed}: north pole-star fact must match the derived view"
                    );
                    assert_eq!(south, None, "seed {seed}: never both pole-star facts");
                    any_committed = true;
                }
                Some(ps) if ps.pole == Hemisphere::South => {
                    assert_eq!(
                        south,
                        Some(Value::Number(hornvale_kernel::quantize(ps.separation_deg))),
                        "seed {seed}: south pole-star fact must match the derived view"
                    );
                    assert_eq!(north, None, "seed {seed}: never both pole-star facts");
                    any_committed = true;
                }
                _ => {
                    assert_eq!(north, None, "seed {seed}: no pole star, no north fact");
                    assert_eq!(south, None, "seed {seed}: no pole star, no south fact");
                }
            }

            let min_sep = outcome
                .system
                .neighbors
                .iter()
                .map(|n| {
                    let genesis_pos = EquatorialCoord {
                        ra_deg: n.right_ascension,
                        dec_deg: n.declination,
                    };
                    let pos = calendar.star_equatorial_at(&genesis_pos, StdDays(0.0));
                    (90.0 - pos.dec_deg).min(90.0 + pos.dec_deg)
                })
                .fold(f64::INFINITY, f64::min);
            min_separations.push((seed, min_sep));
        }

        if !any_committed {
            for (seed, min_sep) in &min_separations {
                eprintln!("seed {seed}: min pole separation {min_sep:.2} deg");
            }
            panic!(
                "BLOCKED: no seed in 0..64 committed a pole-star fact — the 10-degree \
                 threshold may need controller attention"
            );
        }
    }

    /// Night-sky stage 2: with two wanderers pinned, genesis commits the
    /// count fact plus one orbit/period/class trio per wanderer, innermost
    /// order, quantized like every numeric object on commit. Seed 118's pin
    /// draws one rock and one giant (not two of the same class): the ledger
    /// dedups exact-duplicate facts (`Ledger::commit`), and `wanderer-class`
    /// carries only two possible Text values, so two same-class wanderers
    /// would collapse to a single committed fact — a real, documented
    /// characteristic of a bare classification predicate with no other
    /// per-wanderer differentiator, same as any other coarse non-functional
    /// Text fact. A mixed-class seed keeps this test meaningful without
    /// exercising that collapse.
    #[test]
    fn genesis_commits_one_orbit_period_and_class_fact_per_wanderer() {
        use crate::wanderers::WandererClass;

        let pins = SkyPins {
            wanderers: Some(2),
            ..SkyPins::default()
        };
        let outcome = generate(Seed(118), &pins).unwrap();
        assert_eq!(outcome.system.wanderers.len(), 2);
        assert_ne!(
            outcome.system.wanderers[0].class, outcome.system.wanderers[1].class,
            "test setup must exercise two distinct wanderer classes"
        );
        let mut w = world_with(118);
        let subject = w.ledger.mint_entity();
        genesis(&mut w, subject, &outcome).unwrap();

        assert_eq!(
            w.ledger.value_of(subject, WANDERER_COUNT_FACT),
            Some(&Value::Number(2.0))
        );

        let orbits: Vec<f64> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == WANDERER_ORBIT_AU)
            .filter_map(|f| match f.object {
                Value::Number(n) => Some(n),
                _ => None,
            })
            .collect();
        let expected_orbits: Vec<f64> = outcome
            .system
            .wanderers
            .iter()
            .map(|wd| hornvale_kernel::quantize(wd.orbit.get()))
            .collect();
        assert_eq!(orbits, expected_orbits);

        let periods: Vec<f64> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == WANDERER_PERIOD_STD)
            .filter_map(|f| match f.object {
                Value::Number(n) => Some(n),
                _ => None,
            })
            .collect();
        let expected_periods: Vec<f64> = outcome
            .system
            .wanderers
            .iter()
            .map(|wd| hornvale_kernel::quantize(wd.period.get()))
            .collect();
        assert_eq!(periods, expected_periods);

        let classes: Vec<String> = w
            .ledger
            .facts_about(subject)
            .filter(|f| f.predicate == WANDERER_CLASS)
            .filter_map(|f| match &f.object {
                Value::Text(t) => Some(t.clone()),
                _ => None,
            })
            .collect();
        let expected_classes: Vec<String> = outcome
            .system
            .wanderers
            .iter()
            .map(|wd| {
                match wd.class {
                    WandererClass::Rock => "rock",
                    WandererClass::Giant => "giant",
                }
                .to_string()
            })
            .collect();
        assert_eq!(classes, expected_classes);
    }

    /// Night-sky stage 3: the figure-count fact tracks `figures().len()`
    /// exactly, quantized like every numeric object on commit, across a
    /// seed sweep.
    #[test]
    fn genesis_commits_a_figure_count_fact_matching_figures_len_across_a_seed_sweep() {
        for seed in 0..16u64 {
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            let astronomy_seed = Seed(seed).derive("astronomy");
            let expected = crate::figures::figures(astronomy_seed, &outcome.system).len();

            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();

            assert_eq!(
                w.ledger.value_of(subject, FIGURE_COUNT),
                Some(&Value::Number(hornvale_kernel::quantize(expected as f64))),
                "seed {seed}: figure-count fact must match figures().len()"
            );
        }
    }

    /// `figure-members` is a coarse non-functional Number fact with no
    /// per-figure differentiator: two figures with the same member count
    /// collapse to one committed fact (documented honestly on
    /// [`FIGURE_MEMBERS`], the same characteristic `wanderer-class`
    /// carries). This asserts the dedup rather than assuming one-per-figure.
    #[test]
    fn figure_members_facts_dedupe_repeated_counts() {
        // Find a seed whose figures include a repeated member count, so the
        // dedup is actually exercised rather than vacuously true.
        let mut found = false;
        for seed in 0..64u64 {
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            let astronomy_seed = Seed(seed).derive("astronomy");
            let figs = crate::figures::figures(astronomy_seed, &outcome.system);
            if figs.len() < 2 {
                continue;
            }
            let mut counts: Vec<usize> = figs.iter().map(|f| f.member_count).collect();
            counts.sort_unstable();
            let has_duplicate_count = counts.windows(2).any(|w| w[0] == w[1]);
            if !has_duplicate_count {
                continue;
            }
            found = true;

            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();

            let committed: Vec<f64> = w
                .ledger
                .facts_about(subject)
                .filter(|f| f.predicate == FIGURE_MEMBERS)
                .filter_map(|f| match f.object {
                    Value::Number(n) => Some(n),
                    _ => None,
                })
                .collect();
            let distinct_expected: std::collections::BTreeSet<u64> =
                counts.iter().map(|&c| c as u64).collect();
            assert_eq!(
                committed.len(),
                distinct_expected.len(),
                "seed {seed}: repeated member counts must dedupe to one fact each"
            );
            break;
        }
        assert!(
            found,
            "BLOCKED: no seed in 0..64 produced two figures sharing a member count — \
             the dedup claim is untested"
        );
    }

    /// `figure-on-ecliptic` is a functional Flag committed only when true;
    /// since `Flag(true)` is identical across every figure, at most one
    /// fact is ever committed per subject regardless of how many figures
    /// are on the ecliptic — assert presence tracks "any figure is",
    /// documented honestly on [`FIGURE_ON_ECLIPTIC`].
    #[test]
    fn figure_on_ecliptic_flag_tracks_whether_any_figure_qualifies() {
        for seed in 0..16u64 {
            let outcome = generate(Seed(seed), &SkyPins::default()).unwrap();
            let astronomy_seed = Seed(seed).derive("astronomy");
            let figs = crate::figures::figures(astronomy_seed, &outcome.system);
            let any_on_ecliptic = figs.iter().any(|f| f.on_ecliptic);

            let mut w = world_with(seed);
            let subject = w.ledger.mint_entity();
            genesis(&mut w, subject, &outcome).unwrap();

            let flag_present = w
                .ledger
                .facts_about(subject)
                .any(|f| f.predicate == FIGURE_ON_ECLIPTIC);
            assert_eq!(
                flag_present, any_on_ecliptic,
                "seed {seed}: figure-on-ecliptic fact presence must track \"any figure is\""
            );
            // At most one such fact, since every committed value is Flag(true).
            let count = w
                .ledger
                .facts_about(subject)
                .filter(|f| f.predicate == FIGURE_ON_ECLIPTIC)
                .count();
            assert!(
                count <= 1,
                "seed {seed}: figure-on-ecliptic must dedupe to one fact"
            );
        }
    }
}
