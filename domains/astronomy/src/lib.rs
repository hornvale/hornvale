//! Astronomy, tier 0: a sun that never sets. All downstream systems see
//! astronomy only through phenomena — never this crate.
#![warn(missing_docs)]

pub mod anchor;
pub mod calendar;
pub mod facts;
pub mod forcing;
pub mod moons;
pub mod neighborhood;
pub mod pins;
pub mod provider;
pub mod render;
pub mod sky_position;
pub mod star;
pub mod streams;
pub mod system;
pub mod units;

pub use anchor::{Anchor, Rotation, generate_anchor};
pub use calendar::{Calendar, SkyBand, TWILIGHT_DEPTH_DEG, calendar_of};
pub use moons::{Moon, generate_moons, hill_radius_mm};
pub use neighborhood::{Neighbor, class_luminosity, class_name, generate_neighbors};
pub use pins::{
    ForcingPin, GenesisError, MoonsPin, NeighborClass, RotationPin, SkyPins, SpinPin, parse_pin,
    pin_strings,
};
pub use provider::{ECLIPSE, GeneratedSky, NIGHT_STAR, SEASONAL_CYCLE, TIDE};
pub use sky_position::{EclipticCoord, EquatorialCoord, ecliptic_of, equatorial_at};
pub use star::{Star, generate_star};
pub use system::{GenesisOutcome, StarSystem, generate};
pub use units::{
    Au, Degrees, EarthMasses, HabitableZone, LightYears, LocalDays, LunarMasses, Megameters,
    SolarLuminosities, SolarMasses, StdDays, UnitError,
};

use hornvale_kernel::{
    ConceptKind, ConceptRegistry, ObserverContext, PhenomenaSource, Phenomenon, RegistryError,
    Venue, WorldTime,
};

/// Phenomenon kind for bodies visible in the sky.
/// type-audit: bare-ok(identifier-text)
pub const CELESTIAL_BODY: &str = "celestial-body";

/// Every seed-derivation label this crate uses, with docs. All chains hang
/// off the world seed's "astronomy" derivation.
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    vec![
        ("astronomy", "root stream for sky genesis"),
        ("astronomy/star-mass", "main-sequence star mass draw"),
        ("astronomy/anchor-mass", "anchor world mass draw"),
        ("astronomy/rotation", "rotation regime and period draw"),
        ("astronomy/orbit", "anchor orbital distance draw"),
        ("astronomy/obliquity", "axial tilt draw"),
        ("astronomy/moon-count", "how many moons"),
        (
            "astronomy/moons",
            "per-moon mass/distance draws (sequential attempts)",
        ),
        (
            "astronomy/neighbors",
            "notable-neighbor class/distance draws",
        ),
        ("astronomy/forcing", "deep-time orbital forcing"),
        ("astronomy/phase-offsets", "per-body genesis phase offsets"),
        (
            "astronomy/neighbor-positions",
            "per-neighbor celestial position draws (declination, right ascension)",
        ),
        (
            "astronomy/spin-direction",
            "spin-direction draw: prograde or retrograde",
        ),
        (
            "astronomy/moon-inclinations",
            "per-moon orbital-inclination draws",
        ),
    ]
}

/// Register astronomy's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(CELESTIAL_BODY, "a body visible in the sky")?;
    registry.register_phenomenon_kind(SEASONAL_CYCLE, "the annual daylight cycle")?;
    registry.register_phenomenon_kind(NIGHT_STAR, "a fixed star notable in the night sky")?;
    registry.register_phenomenon_kind(TIDE, "the rise and fall of the waters under the moons")?;
    registry.register_phenomenon_kind(ECLIPSE, "a moon crossing the face of the sun")?;

    registry.register_predicate(
        facts::STAR_CLASS,
        true,
        "the host star's descriptive spectral class",
    )?;
    registry.register_predicate(
        facts::TIDALLY_LOCKED,
        true,
        "the anchor world is tidally locked (no local day)",
    )?;
    registry.register_predicate(
        facts::DAY_LENGTH_STD,
        true,
        "solar day length in standard days, for spinning worlds",
    )?;
    registry.register_predicate(facts::YEAR_LENGTH_STD, true, "year length in standard days")?;
    registry.register_predicate(
        facts::OBLIQUITY_DEGREES,
        true,
        "axial tilt of the anchor world, in degrees",
    )?;
    registry.register_predicate(
        facts::MOON_COUNT,
        true,
        "how many moons the anchor world has",
    )?;
    registry.register_predicate(
        facts::MOON_PERIOD_STD,
        false,
        "orbital period of a moon, in standard days",
    )?;
    registry.register_predicate(
        facts::MOON_TIDE_REL,
        false,
        "tidal strength of a moon, relative to Luna on Earth",
    )?;
    registry.register_predicate(
        facts::RETROGRADE_SPIN,
        true,
        "the anchor world spins backward: the sun rises in the west",
    )?;
    registry.register_predicate(
        facts::MOON_INCLINATION_DEGREES,
        false,
        "orbital inclination of a moon to the anchor's orbital plane, in degrees",
    )?;
    registry.register_predicate(
        facts::NOTABLE_NEIGHBOR,
        false,
        "a notable neighbor star visible in the night sky",
    )?;
    registry.register_predicate(
        facts::GENESIS_NOTE,
        false,
        "a degradation or refusal recorded during sky genesis",
    )?;
    registry.register_predicate(
        facts::SKY_PROVIDER,
        true,
        "which astronomy provider this world uses (constant or generated)",
    )?;
    registry.register_predicate(
        facts::SCENARIO_PIN,
        false,
        "an experimenter-supplied pin string conditioning genesis",
    )?;
    registry.register_predicate(
        facts::ECCENTRICITY_MEAN,
        true,
        "mean orbital eccentricity (deep-time forcing)",
    )?;
    registry.register_predicate(
        facts::OBLIQUITY_AMPLITUDE,
        true,
        "obliquity oscillation amplitude, degrees (moon-coupled)",
    )?;

    registry.register_concept("sun", "astronomy", ConceptKind::Celestial, "the sun")?;
    registry.register_concept("moon", "astronomy", ConceptKind::Celestial, "a moon")?;
    registry.register_concept(
        "star",
        "astronomy",
        ConceptKind::Celestial,
        "a fixed point of light in the night sky",
    )?;
    registry.register_concept(
        "night",
        "astronomy",
        ConceptKind::Celestial,
        "the dark half of the day-night cycle",
    )
}

/// Tier-0 astronomy: the sun is always up, fixed at zenith.
pub struct ConstantSun;

/// What the sky looks like at a given moment.
/// type-audit: bare-ok(prose: description), bare-ok(identifier-text: bodies)
#[derive(Debug, Clone, PartialEq)]
pub struct SkyReport {
    /// Human-readable description of the sky.
    pub description: String,
    /// Names of the visible bodies.
    pub bodies: Vec<String>,
}

impl ConstantSun {
    /// The sky at `_time` — which, at tier 0, never changes.
    pub fn sky_at(&self, _time: WorldTime) -> SkyReport {
        SkyReport {
            description: "A golden sun hangs fixed at zenith. It has never been seen to move."
                .to_string(),
            bodies: vec!["the sun".to_string()],
        }
    }
}

impl PhenomenaSource for ConstantSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: CELESTIAL_BODY.to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
            venue: Venue::DaySky,
        }]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::EntityId;

    fn ctx(day: f64) -> ObserverContext {
        ObserverContext::at(EntityId(1), WorldTime { day })
    }

    #[test]
    fn the_sky_never_changes() {
        let sun = ConstantSun;
        let a = sun.sky_at(WorldTime { day: 0.0 });
        let b = sun.sky_at(WorldTime { day: 9999.5 });
        assert_eq!(a.description, b.description);
        assert_eq!(a.bodies, b.bodies);
        assert!(a.description.contains("zenith"));
    }

    #[test]
    fn phenomena_are_constant_and_maximally_salient() {
        let sun = ConstantSun;
        let seen = sun.phenomena(&ctx(0.0));
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, CELESTIAL_BODY);
        assert_eq!(seen[0].period_days, None);
        assert_eq!(seen[0].salience, 1.0);
        assert_eq!(seen, sun.phenomena(&ctx(500.25)));
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.phenomenon_kind(CELESTIAL_BODY).is_some());
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for name in ["sun", "moon", "star", "night"] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "astronomy");
            assert_eq!(c.kind, ConceptKind::Celestial);
        }
    }
}
