//! The composition root: the only place all domains meet. Wires
//! registrations and geneses in cascade order, and gathers the almanac's
//! context. Domains stay ignorant of each other; this module is where the
//! application composes them (Constitution §2.6).

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::ConstantSun;
use hornvale_climate::UniformClimate;
use hornvale_kernel::{
    ConceptRegistry, LedgerError, ObserverContext, PhenomenaSource, Phenomenon, RegistryError,
    Seed, World, WorldTime, observe,
};

/// Errors from building a world.
#[derive(Debug)]
pub enum BuildError {
    /// A concept registration conflicted.
    Registry(RegistryError),
    /// A genesis commit was rejected.
    Ledger(LedgerError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Registry(e) => write!(f, "registry: {e}"),
            BuildError::Ledger(e) => write!(f, "ledger: {e}"),
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

/// Register every domain's concepts.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    hornvale_astronomy::register_concepts(registry)?;
    hornvale_climate::register_concepts(registry)?;
    hornvale_terrain::register_concepts(registry)?;
    hornvale_settlement::register_concepts(registry)?;
    hornvale_culture::register_concepts(registry)?;
    hornvale_religion::register_concepts(registry)
}

/// The tier-0 phenomena sources, observed at the world's first place.
pub fn observed_phenomena(world: &World, day: f64) -> Vec<Phenomenon> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Vec::new();
    };
    let sun = ConstantSun;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sun, &climate];
    observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
        },
    )
}

/// Build a complete tier-0 world: registrations, then geneses in cascade
/// order (terrain → settlement → culture → religion-from-phenomena).
pub fn build_world(seed: Seed) -> Result<World, BuildError> {
    let mut world = World::new(seed);
    register_all(&mut world.registry)?;
    let vale = hornvale_terrain::genesis(&mut world)?;
    let village = hornvale_settlement::genesis(&mut world, vale)?;
    hornvale_culture::genesis(&mut world, village)?;
    let seen = observed_phenomena(&world, 0.0);
    hornvale_religion::genesis(&mut world, village, &seen)?;
    Ok(world)
}

/// Gather everything the almanac renders, reconstructing the stateless
/// tier-0 providers.
// used from Task 10 (almanac command)
#[allow(dead_code)]
pub fn almanac_context(world: &World) -> AlmanacContext {
    let sun = ConstantSun;
    let climate = UniformClimate;
    let village = hornvale_settlement::village_info(world);
    let castes = village
        .as_ref()
        .map(|v| hornvale_culture::castes_of(world, v.id))
        .unwrap_or_default();
    AlmanacContext {
        seed: world.seed.0,
        sky: sun.sky_at(WorldTime { day: 0.0 }),
        climate: climate.climate_at(hornvale_kernel::Position { x: 0.0, y: 0.0 }),
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

    #[test]
    fn build_world_produces_the_full_cascade() {
        let world = build_world(Seed(42)).unwrap();
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
        let a = build_world(Seed(42)).unwrap().to_json();
        let b = build_world(Seed(42)).unwrap().to_json();
        assert_eq!(a, b);
    }

    #[test]
    fn different_seeds_differ() {
        let worlds: Vec<String> = (1..=4)
            .map(|s| build_world(Seed(s)).unwrap().to_json())
            .collect();
        assert!(worlds.windows(2).any(|w| w[0] != w[1]));
    }

    #[test]
    fn almanac_context_gathers_everything() {
        let world = build_world(Seed(42)).unwrap();
        let ctx = almanac_context(&world);
        assert_eq!(ctx.seed, 42);
        assert!(!ctx.places.is_empty());
        assert!(ctx.village.is_some());
        assert!(!ctx.castes.is_empty());
        assert!(!ctx.beliefs.is_empty());
        assert!(!ctx.phenomena.is_empty());
    }
}
