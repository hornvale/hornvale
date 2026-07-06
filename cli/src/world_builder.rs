//! The composition root: the only place all domains meet. Wires
//! registrations and geneses in cascade order, and gathers the almanac's
//! context. Domains stay ignorant of each other; this module is where the
//! application composes them (Constitution §2.6).

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::{
    ConstantSun, GeneratedSky, GenesisError, SkyPins, SkyReport, facts, generate, parse_pin,
    pin_strings,
};
use hornvale_climate::{ClimateReport, UniformClimate};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, LedgerError, ObserverContext, PhenomenaSource, Phenomenon,
    RegistryError, Seed, Value, World, WorldTime, observe,
};

/// Errors from building a world.
#[derive(Debug)]
pub enum BuildError {
    /// A concept registration conflicted.
    Registry(RegistryError),
    /// A genesis commit was rejected.
    Ledger(LedgerError),
    /// Sky genesis refused a pin.
    Genesis(GenesisError),
    /// A committed pin string could not be parsed back.
    Pins(String),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Registry(e) => write!(f, "registry: {e}"),
            BuildError::Ledger(e) => write!(f, "ledger: {e}"),
            BuildError::Genesis(e) => write!(f, "sky genesis: {e}"),
            BuildError::Pins(reason) => write!(f, "pins: {reason}"),
        }
    }
}

impl std::error::Error for BuildError {}

impl From<RegistryError> for BuildError {
    fn from(e: RegistryError) -> Self {
        BuildError::Registry(e)
    }
}

impl From<LedgerError> for BuildError {
    fn from(e: LedgerError) -> Self {
        BuildError::Ledger(e)
    }
}

/// Which astronomy provider a world is built with.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SkyChoice {
    /// Tier-0: the sun never sets.
    Constant,
    /// Tiers 1/2: a fully generated star system.
    Generated,
}

/// The live astronomy provider a world uses, reconstructed from its ledger.
pub enum Sky {
    /// Tier-0 constant sun.
    Constant(ConstantSun),
    /// Tiers 1/2 generated sky.
    Generated(Box<GeneratedSky>),
}

impl Sky {
    /// The sky at a moment, rendered, from whichever provider this is.
    pub fn sky_at(&self, time: WorldTime) -> SkyReport {
        match self {
            Sky::Constant(sun) => sun.sky_at(time),
            Sky::Generated(sky) => sky.sky_at(time),
        }
    }
}

impl PhenomenaSource for Sky {
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon> {
        match self {
            Sky::Constant(sun) => sun.phenomena(ctx),
            Sky::Generated(sky) => sky.phenomena(ctx),
        }
    }
}

/// Register every domain's concepts.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    hornvale_astronomy::register_concepts(registry)?;
    hornvale_climate::register_concepts(registry)?;
    hornvale_terrain::register_concepts(registry)?;
    hornvale_settlement::register_concepts(registry)?;
    hornvale_culture::register_concepts(registry)?;
    hornvale_religion::register_concepts(registry)
}

fn scenario_fact(subject: EntityId, predicate: &str, object: Value) -> Fact {
    Fact {
        subject,
        predicate: predicate.to_string(),
        object,
        place: None,
        day: Some(0.0),
        provenance: "scenario".to_string(),
    }
}

/// Reconstruct the live astronomy provider from whatever this world's
/// ledger says: absent `sky-provider` fact (1a/1b-era saves) → `Constant`;
/// `"constant"` → `Constant`; `"generated"` → fold every `scenario-pin`
/// fact back through `parse_pin` and regenerate deterministically from the
/// world's own seed.
pub fn sky_of(world: &World) -> Result<Sky, BuildError> {
    let Some(provider_fact) = world.ledger.find(facts::SKY_PROVIDER).next() else {
        return Ok(Sky::Constant(ConstantSun));
    };
    let subject = provider_fact.subject;
    match &provider_fact.object {
        Value::Text(choice) if choice == "constant" => Ok(Sky::Constant(ConstantSun)),
        Value::Text(choice) if choice == "generated" => {
            let mut pins = SkyPins::default();
            for pin_fact in world
                .ledger
                .facts_about(subject)
                .filter(|f| f.predicate == facts::SCENARIO_PIN)
            {
                if let Value::Text(s) = &pin_fact.object {
                    parse_pin(s, &mut pins).map_err(BuildError::Pins)?;
                }
            }
            let outcome = generate(world.seed, &pins).map_err(BuildError::Genesis)?;
            Ok(Sky::Generated(Box::new(GeneratedSky::new(outcome))))
        }
        other => Err(BuildError::Pins(format!(
            "unrecognized sky-provider value: {other:?}"
        ))),
    }
}

/// The tier-0/1/2 phenomena sources, observed at the world's first place.
pub fn observed_phenomena(world: &World, day: f64) -> Vec<Phenomenon> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Vec::new();
    };
    let sky = sky_of(world).expect("a saved world's own pins must reconstruct its provider");
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
        },
    )
}

/// Build a complete world: mint the world entity and record its sky choice
/// and scenario pins first; run sky genesis for `Generated`; then the
/// tier-0 cascade (terrain → settlement → culture →
/// religion-from-phenomena), with phenomena routed through whichever
/// provider this world uses.
pub fn build_world(seed: Seed, pins: &SkyPins, sky: SkyChoice) -> Result<World, BuildError> {
    let mut world = World::new(seed);
    register_all(&mut world.registry)?;

    let world_entity = world.ledger.mint_entity();
    let choice_text = match sky {
        SkyChoice::Constant => "constant",
        SkyChoice::Generated => "generated",
    };
    world.ledger.commit(
        scenario_fact(
            world_entity,
            facts::SKY_PROVIDER,
            Value::Text(choice_text.to_string()),
        ),
        &world.registry,
    )?;
    for pin_string in pin_strings(pins) {
        world.ledger.commit(
            scenario_fact(world_entity, facts::SCENARIO_PIN, Value::Text(pin_string)),
            &world.registry,
        )?;
    }

    if let SkyChoice::Generated = sky {
        let outcome = generate(seed, pins).map_err(BuildError::Genesis)?;
        facts::genesis(&mut world, world_entity, &outcome)?;
    }

    let vale = hornvale_terrain::genesis(&mut world)?;
    let village = hornvale_settlement::genesis(&mut world, vale)?;
    hornvale_culture::genesis(&mut world, village)?;
    let seen = observed_phenomena(&world, 0.0);
    hornvale_religion::genesis(&mut world, village, &seen)?;
    Ok(world)
}

/// The sky at `time`, from whichever astronomy provider this world uses.
/// The single construction site for the provider (Constitution §2.4 tiers).
pub fn sky_report(world: &World, time: WorldTime) -> SkyReport {
    sky_of(world)
        .expect("a saved world's own pins must reconstruct its provider")
        .sky_at(time)
}

/// The local climate, from whichever climate provider this world uses.
pub fn climate_report(_world: &World) -> ClimateReport {
    UniformClimate.climate_at(hornvale_kernel::Position { x: 0.0, y: 0.0 })
}

/// Gather everything the almanac renders, reconstructing the stateless
/// tier-0 providers.
pub fn almanac_context(world: &World) -> AlmanacContext {
    let village = hornvale_settlement::village_info(world);
    let castes = village
        .as_ref()
        .map(|v| hornvale_culture::castes_of(world, v.id))
        .unwrap_or_default();
    AlmanacContext {
        seed: world.seed.0,
        sky: sky_report(world, WorldTime { day: 0.0 }),
        climate: climate_report(world),
        phenomena: observed_phenomena(world, 0.0),
        places: hornvale_terrain::places(world),
        village,
        castes,
        beliefs: hornvale_religion::beliefs_of(world),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(seed: u64) -> World {
        build_world(Seed(seed), &SkyPins::default(), SkyChoice::Constant).unwrap()
    }

    fn generated(seed: u64) -> World {
        build_world(Seed(seed), &SkyPins::default(), SkyChoice::Generated).unwrap()
    }

    #[test]
    fn build_world_produces_the_full_cascade() {
        let world = constant(42);
        assert_eq!(hornvale_terrain::places(&world).len(), 1);
        let village = hornvale_settlement::village_info(&world).expect("village");
        assert_eq!(
            hornvale_culture::castes_of(&world, village.id).len(),
            hornvale_culture::CASTES.len()
        );
        assert_eq!(hornvale_religion::beliefs_of(&world).len(), 1);
    }

    #[test]
    fn build_world_is_deterministic() {
        let a = constant(42).to_json();
        let b = constant(42).to_json();
        assert_eq!(a, b);
    }

    #[test]
    fn different_seeds_differ() {
        let worlds: Vec<String> = (1..=4).map(|s| constant(s).to_json()).collect();
        assert!(worlds.windows(2).any(|w| w[0] != w[1]));
    }

    #[test]
    fn almanac_context_gathers_everything() {
        let world = constant(42);
        let ctx = almanac_context(&world);
        assert_eq!(ctx.seed, 42);
        assert!(!ctx.places.is_empty());
        assert!(ctx.village.is_some());
        assert!(!ctx.castes.is_empty());
        assert!(!ctx.beliefs.is_empty());
        assert!(!ctx.phenomena.is_empty());
    }

    #[test]
    fn sky_and_climate_reports_come_from_the_composition_root() {
        let world = constant(42);
        let sky = sky_report(&world, hornvale_kernel::WorldTime { day: 0.0 });
        assert!(sky.description.contains("zenith"));
        let climate = climate_report(&world);
        assert_eq!(climate.temperature_c, 18.0);
    }

    #[test]
    fn generated_sky_reconstructs_and_beliefs_are_non_empty() {
        let world = generated(42);
        assert!(matches!(sky_of(&world).unwrap(), Sky::Generated(_)));
        assert!(!hornvale_religion::beliefs_of(&world).is_empty());
    }

    #[test]
    fn generated_worlds_are_deterministic() {
        let a = generated(42).to_json();
        let b = generated(42).to_json();
        assert_eq!(a, b);
    }

    #[test]
    fn generated_sky_round_trips_through_save_and_load() {
        let world = generated(42);
        let before = sky_report(&world, WorldTime { day: 0.0 });
        let reloaded = World::from_json(&world.to_json()).unwrap();
        assert!(matches!(sky_of(&reloaded).unwrap(), Sky::Generated(_)));
        let after = sky_report(&reloaded, WorldTime { day: 0.0 });
        assert_eq!(before, after);
    }

    #[test]
    fn constant_choice_yields_constant_sky_and_unchanged_almanac_context() {
        let world = constant(42);
        assert!(matches!(sky_of(&world).unwrap(), Sky::Constant(_)));
        let ctx = almanac_context(&world);
        assert!(ctx.sky.description.contains("zenith"));
    }

    #[test]
    fn absent_sky_provider_fact_falls_back_to_constant() {
        // A 1a/1b-era world never committed a sky-provider fact at all.
        let mut world = World::new(Seed(1));
        register_all(&mut world.registry).unwrap();
        assert!(matches!(sky_of(&world).unwrap(), Sky::Constant(_)));
    }
}
