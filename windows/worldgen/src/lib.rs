//! Hornvale composition root — the only place all domains meet.
//!
//! Wires registrations and geneses in cascade order, and gathers the
//! almanac's context. Domains stay ignorant of each other; this module is
//! where the application composes them (Constitution §2.6).
#![warn(missing_docs)]

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::{
    ConstantSun, GeneratedSky, GenesisError, SkyPins, SkyReport, facts, generate, parse_pin,
    pin_strings,
};
use hornvale_climate::{
    ClimateInputs, ClimateReport, GeneratedClimate, RotationRegime, SeafloorFeature, UniformClimate,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, Geosphere, LedgerError, ObserverContext, PhenomenaSource,
    Phenomenon, RegistryError, Seed, Value, World, WorldTime, observe,
};
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins};
use std::sync::OnceLock;

pub mod settlement_pins;
pub use settlement_pins::SettlementPins;

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
    /// Terrain genesis refused a pin.
    TerrainGenesis(hornvale_terrain::GenesisError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Registry(e) => write!(f, "registry: {e}"),
            BuildError::Ledger(e) => write!(f, "ledger: {e}"),
            BuildError::Genesis(e) => write!(f, "sky genesis: {e}"),
            BuildError::Pins(reason) => write!(f, "pins: {reason}"),
            BuildError::TerrainGenesis(e) => write!(f, "terrain genesis: {e}"),
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

    /// The derived calendar, if this world has a generated sky. `None` for
    /// the tier-0 constant sun, which has no cycles. Climate consumes this
    /// at the composition root (spec §13 opener).
    pub fn calendar(&self) -> Option<&hornvale_astronomy::Calendar> {
        match self {
            Sky::Constant(_) => None,
            Sky::Generated(sky) => Some(sky.calendar()),
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

/// The shared Geosphere at `GLOBE_LEVEL`: seed-independent, computed once
/// per process and cloned into providers, so per-world mesh cost is a
/// memcpy (spec §3: "computed once, shared across all worlds").
fn shared_geosphere() -> &'static Geosphere {
    static GEO: OnceLock<Geosphere> = OnceLock::new();
    GEO.get_or_init(|| Geosphere::new(GLOBE_LEVEL))
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

/// Reconstruct the tectonic terrain provider from the world's seed and its
/// committed terrain-pin facts (the `sky_of` pattern). Worlds saved before
/// terrain pins existed simply have none and regenerate with defaults. The
/// single construction site for the terrain provider.
pub fn terrain_of(world: &World) -> Result<GeneratedTerrain, BuildError> {
    let mut pins = TerrainPins::default();
    for pin_fact in world.ledger.find(hornvale_terrain::facts::TERRAIN_PIN) {
        if let Value::Text(s) = &pin_fact.object {
            hornvale_terrain::parse_pin(s, &mut pins).map_err(BuildError::Pins)?;
        }
    }
    let outcome = hornvale_terrain::generate(world.seed, shared_geosphere(), &pins)
        .map_err(BuildError::TerrainGenesis)?;
    Ok(GeneratedTerrain::new(shared_geosphere().clone(), outcome))
}

/// Map a terrain boundary contact to the seafloor feature climate consumes
/// (only ocean cells use it): ocean–ocean convergent arcs become trenches;
/// oceanic ridges become vent-bearing ridges; everything else is featureless.
fn seafloor_feature(boundary: Option<hornvale_terrain::CellBoundary>) -> SeafloorFeature {
    use hornvale_terrain::BoundaryKind;
    match boundary.map(|b| b.kind) {
        Some(BoundaryKind::IslandArc) => SeafloorFeature::Trench,
        Some(BoundaryKind::OceanicRidge) => SeafloorFeature::Ridge,
        _ => SeafloorFeature::None,
    }
}

/// Map a climate biome into culture's coarse biome class: total over every
/// biome, so culture's subsistence function is always defined. Culture
/// imports no domain (spec §2.6); this map lives only at the composition
/// root. Forest biomes (and taiga) farm; grassland/savanna farm or herd;
/// desert/shrubland herd; tundra forages; alpine, ice, and every marine
/// biome are barren (defensively — settlements never sit on open ocean, but
/// the map must still be total).
pub fn biome_class(biome: hornvale_climate::Biome) -> hornvale_culture::BiomeClass {
    use hornvale_climate::Biome;
    use hornvale_culture::BiomeClass;
    match biome {
        Biome::TropicalRainforest
        | Biome::TropicalSeasonalForest
        | Biome::TemperateRainforest
        | Biome::TemperateForest
        | Biome::Taiga => BiomeClass::Forest,
        Biome::Savanna | Biome::TemperateGrassland => BiomeClass::Grassland,
        Biome::Desert | Biome::Shrubland => BiomeClass::Arid,
        Biome::Tundra => BiomeClass::Cold,
        Biome::Alpine
        | Biome::Ice
        | Biome::SeaIce
        | Biome::CoralReef
        | Biome::KelpForest
        | Biome::HydrothermalVent
        | Biome::HadalTrench
        | Biome::Upwelling
        | Biome::Epipelagic
        | Biome::Mesopelagic
        | Biome::Bathypelagic
        | Biome::Abyssal => BiomeClass::Barren,
    }
}

/// The scalar stellar inputs climate needs, derived from this world's sky.
/// Constant-sky worlds get an Earth baseline so the biome map exists for
/// every world (spec: the coarse globe is generated for all).
fn stellar_inputs(sky: &Sky) -> (f64, f64, RotationRegime, f64) {
    match sky {
        Sky::Constant(_) => (1.0, 23.5, RotationRegime::Spinning { day_std: 1.0 }, 365.25),
        Sky::Generated(generated) => {
            let system = generated.system();
            let luminosity = system.star.luminosity.get();
            let orbit = system.anchor.orbit.get();
            // Insolation relative to Earth (L=1, d=1): L / d².
            let insolation = luminosity / (orbit * orbit);
            let obliquity = system.anchor.obliquity.get();
            let regime = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day } => {
                    RotationRegime::Spinning { day_std: day.get() }
                }
                hornvale_astronomy::Rotation::Locked => RotationRegime::Locked,
            };
            let year = generated.calendar().year_length().get();
            (insolation, obliquity, regime, year)
        }
    }
}

/// Reconstruct the tier-1 climate for this world: rebuild the terrain globe
/// and the sky, map their outputs into climate's kernel-only inputs, and
/// derive temperature/moisture/biome/habitability. The single construction
/// site for `GeneratedClimate` (the `terrain_of`/`sky_of` pattern).
pub fn climate_of(world: &World) -> Result<GeneratedClimate, BuildError> {
    let terrain = terrain_of(world)?;
    let sky = sky_of(world)?;
    let geo = terrain.geosphere();
    let elevation = &terrain.globe().elevation;
    let seafloor =
        hornvale_kernel::CellMap::from_fn(geo, |cell| seafloor_feature(terrain.boundary_at(cell)));
    let (insolation, obliquity_deg, regime, year_length_std) = stellar_inputs(&sky);
    Ok(GeneratedClimate::generate(&ClimateInputs {
        geosphere: geo,
        elevation,
        sea_level: terrain.sea_level(),
        seafloor: &seafloor,
        insolation,
        obliquity_deg,
        regime,
        year_length_std,
    }))
}

/// Headline biome/habitability lines for the almanac's Land section.
pub fn biome_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let climate = climate_of(world)?;
    let summary = hornvale_climate::summarize(&climate);
    let bands = match summary.band_count {
        Some(n) => format!("{n} circulation band(s) per hemisphere"),
        None => "a single day–night overturning (tidally locked)".to_string(),
    };
    Ok(vec![
        format!(
            "The air organizes into {bands}; {} land biomes and {} marine biomes cover the globe.",
            summary.land_biome_count, summary.marine_biome_count
        ),
        format!(
            "Some {:.0}% of the surface is habitable — land with water and a tolerable season.",
            summary.habitable_fraction * 100.0
        ),
    ])
}

/// The land's headline lines for the almanac: plates and ocean coverage,
/// then the highest land above the sea.
pub fn land_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let terrain = terrain_of(world)?;
    let summary = hornvale_terrain::summarize(terrain.globe());
    Ok(vec![
        format!(
            "The globe breaks into {} plates; the sea claims {:.0}% of its surface.",
            summary.plate_count,
            summary.ocean_fraction * 100.0
        ),
        format!(
            "The highest land stands {:.0} m above the sea.",
            summary.highest_elevation_m - summary.sea_level_m
        ),
    ])
}

/// The tier-0/1/2 phenomena sources, observed at the world's first place.
pub fn observed_phenomena(world: &World, day: f64) -> Result<Vec<Phenomenon>, BuildError> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
        },
    ))
}

/// Build a complete world: mint the world entity and record its sky choice
/// and scenario pins first; run sky genesis for `Generated`; commit the
/// terrain pins and run tectonic genesis; then assemble per-cell site inputs
/// from terrain and climate, place a spaced scatter of settlements (honoring
/// the settlement pins' suitability floor), commit each as its own place
/// entity, and run the culture/religion cascade on the flagship (the
/// most-suitable settlement, placed first) from its actual environment.
pub fn build_world(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
) -> Result<World, BuildError> {
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

    for pin_string in hornvale_terrain::pin_strings(terrain_pins) {
        world.ledger.commit(
            scenario_fact(
                world_entity,
                hornvale_terrain::facts::TERRAIN_PIN,
                Value::Text(pin_string),
            ),
            &world.registry,
        )?;
    }
    let terrain_outcome = hornvale_terrain::generate(seed, shared_geosphere(), terrain_pins)
        .map_err(BuildError::TerrainGenesis)?;
    hornvale_terrain::facts::genesis(&mut world, world_entity, &terrain_outcome)?;

    // Settlement pins are never reconstructed (settlements persist as their
    // own committed facts, not re-derived from a provider like sky/terrain);
    // record them on `world_entity` under their own predicate purely for
    // round-trip fidelity and scout-style inspection, mirroring terrain-pin
    // above — a distinct predicate keeps them from colliding with sky's own
    // scenario-pin facts without needing a freshly-minted subject.
    for pin_string in settlement_pins.pin_strings() {
        world.ledger.commit(
            scenario_fact(
                world_entity,
                hornvale_settlement::SETTLEMENT_PIN,
                Value::Text(pin_string),
            ),
            &world.registry,
        )?;
    }

    // Reconstruct terrain + climate, assemble per-cell site inputs, place a
    // spaced scatter of settlements, and commit each as its own place entity.
    let terrain = terrain_of(&world)?;
    let climate = climate_of(&world)?;
    let geo = terrain.geosphere();
    const DRAINAGE_REF: f64 = 200.0;
    let sites: Vec<hornvale_settlement::SiteInput> = geo
        .cells()
        .map(|cell| {
            let coastal = geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n));
            let moisture = climate.moisture_at(cell);
            let drainage_norm = (terrain.drainage_at(cell) / DRAINAGE_REF).min(1.0);
            let freshwater = drainage_norm
                .max(if coastal { 1.0 } else { 0.0 })
                .max(moisture)
                .clamp(0.0, 1.0);
            let aridity = ((0.2 - moisture).max(0.0) * 5.0).clamp(0.0, 1.0);
            let hostility = terrain.unrest_at(cell).max(aridity).clamp(0.0, 1.0);
            hornvale_settlement::SiteInput {
                cell,
                position: geo.position(cell),
                habitable: *climate.habitability().get(cell),
                freshwater,
                coastal,
                temperature_c: climate.mean_temperature_at(cell),
                hostility,
            }
        })
        .collect();
    let min_sep = (12.0_f64.to_radians()).cos();
    let floor = settlement_pins.min_suitability.unwrap_or(0.25);
    let placements = hornvale_settlement::place(&sites, min_sep, floor);
    let placed: Vec<hornvale_settlement::PlacedSettlement> = placements
        .iter()
        .map(|p| {
            let coord = geo.coord(p.cell);
            hornvale_settlement::PlacedSettlement {
                cell: p.cell.0,
                latitude: coord.latitude,
                longitude: coord.longitude,
                biome: climate.biome_at(p.cell).name().to_string(),
                name: hornvale_settlement::generate_name(seed, u64::from(p.cell.0)),
                population: hornvale_settlement::draw_population(
                    seed,
                    u64::from(p.cell.0),
                    p.suitability,
                ),
            }
        })
        .collect();
    let ids = hornvale_settlement::genesis(&mut world, &placed)?;
    if let Some(&flagship) = ids.first() {
        let fcell = placements[0].cell;
        let coastal = geo.neighbors(fcell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(fcell);
        let class = biome_class(climate.biome_at(fcell));
        let subsistence = hornvale_culture::subsistence(class, coastal);
        let surplus = (hornvale_culture::fertility(class) * moisture).clamp(0.0, 1.0);
        let threat = terrain.unrest_at(fcell).clamp(0.0, 1.0);
        let population = placed[0].population;
        let env = hornvale_culture::EnvSummary {
            subsistence,
            surplus,
            population,
            threat,
        };
        hornvale_culture::genesis(&mut world, flagship, &env)?;
        let seen = observed_phenomena(&world, 0.0)?;
        hornvale_religion::genesis(&mut world, flagship, &seen)?;
    }
    Ok(world)
}

/// Headline lines describing the world's people for the almanac: how many
/// settlements, and the flagship's name, population, and biome.
pub fn settlement_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let places = hornvale_terrain::places(world);
    let mut lines = vec![format!("The land holds {} settlement(s).", places.len())];
    if let Some(v) = hornvale_settlement::village_info(world) {
        let biome = places
            .iter()
            .find(|p| p.id == v.id)
            .map(|p| p.biome.clone())
            .unwrap_or_else(|| "unknown".to_string());
        lines.push(format!(
            "The chief settlement, {}, holds {} souls amid {}.",
            v.name, v.population, biome
        ));
    }
    Ok(lines)
}

/// Headline culture lines for the almanac's People section: the flagship's
/// subsistence mode, then a one-line summary of its emergent role structure.
/// Empty when there is no flagship (an empty world).
pub fn culture_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let Some(village) = hornvale_settlement::village_info(world) else {
        return Ok(Vec::new());
    };
    let Some(subsistence) = hornvale_culture::subsistence_of(world, village.id) else {
        return Ok(Vec::new());
    };
    let castes = hornvale_culture::castes_of(world, village.id);
    Ok(vec![
        format!("{} lives by {subsistence}.", village.name),
        format!("Its roles, lowest to highest: {}.", castes.join(", ")),
    ])
}

/// The sky at `time`, from whichever astronomy provider this world uses.
/// The single construction site for the provider (Constitution §2.4 tiers).
pub fn sky_report(world: &World, time: WorldTime) -> Result<SkyReport, BuildError> {
    Ok(sky_of(world)?.sky_at(time))
}

/// The local climate, from whichever climate provider this world uses.
pub fn climate_report(_world: &World) -> ClimateReport {
    UniformClimate.climate_at(hornvale_kernel::Position { x: 0.0, y: 0.0 })
}

/// The ordinal word for a zero-based moon index, capped at the legal
/// maximum of three moons.
fn moon_ordinal(index: usize) -> &'static str {
    ["first", "second", "third"][index]
}

/// The world's cycles as reader-facing lines: year length, the seasonal
/// swell of daylight (if the world has axial tilt), and one line per moon.
/// Empty for constant-sky worlds, which have no generated calendar to
/// describe.
pub fn calendar_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let sky = sky_of(world)?;
    let Sky::Generated(sky) = &sky else {
        return Ok(Vec::new());
    };
    let calendar = sky.calendar();
    let system = sky.system();
    let year_std = calendar.year_length().get();
    let day_std = calendar.day_length().map(|d| d.get());

    let mut lines = Vec::new();
    match day_std {
        Some(day_std) => lines.push(format!(
            "The year is {:.1} local days ({:.1} standard days).",
            year_std / day_std,
            year_std
        )),
        None => lines.push(format!(
            "This world is tidally locked: no local day exists; the year is {year_std:.1} standard days."
        )),
    }

    if day_std.is_some() && system.anchor.obliquity.get() > 0.0 {
        let half = (system.anchor.obliquity.get() / 90.0) / 2.0;
        let max = 0.5 + half;
        let min = 0.5 - half;
        lines.push(format!(
            "Daylight swells to {:.0}% at midsummer and shrinks to {:.0}% at midwinter.",
            max * 100.0,
            min * 100.0
        ));
    }

    for (index, moon) in system.moons.iter().enumerate() {
        let ordinal = moon_ordinal(index);
        let months = calendar
            .months_per_year(index)
            .expect("moon index in range");
        match day_std {
            Some(day_std) => lines.push(format!(
                "The {ordinal} moon circles every {:.1} local days — {:.1} months to a year.",
                moon.period.get() / day_std,
                months
            )),
            None => lines.push(format!(
                "The {ordinal} moon circles every {:.1} standard days — {:.1} months to a year.",
                moon.period.get(),
                months
            )),
        }
    }

    Ok(lines)
}

/// The night sky as a single sentence naming its notable neighbor stars,
/// brightest first. `None` for constant-sky worlds, which have no
/// neighborhood to describe.
pub fn night_sky_line(world: &World) -> Result<Option<String>, BuildError> {
    let sky = sky_of(world)?;
    let Sky::Generated(sky) = &sky else {
        return Ok(None);
    };
    let parts: Vec<String> = sky
        .system()
        .neighbors
        .iter()
        .map(|n| n.night_description())
        .collect();
    Ok(Some(format!("By night: {}.", parts.join("; "))))
}

/// Notes recorded during sky genesis. Empty for constant-sky worlds, which
/// are never generated.
pub fn genesis_notes(world: &World) -> Result<Vec<String>, BuildError> {
    let sky = sky_of(world)?;
    Ok(match &sky {
        Sky::Constant(_) => Vec::new(),
        Sky::Generated(sky) => sky.notes().to_vec(),
    })
}

/// Gather everything the almanac renders, reconstructing the stateless
/// tier-0 providers.
pub fn almanac_context(world: &World) -> Result<AlmanacContext, BuildError> {
    let village = hornvale_settlement::village_info(world);
    let castes = village
        .as_ref()
        .map(|v| hornvale_culture::castes_of(world, v.id))
        .unwrap_or_default();
    Ok(AlmanacContext {
        seed: world.seed.0,
        sky: sky_report(world, WorldTime { day: 0.0 })?,
        climate: climate_report(world),
        phenomena: observed_phenomena(world, 0.0)?,
        places: hornvale_terrain::places(world),
        land_lines: land_lines(world)?,
        biome_lines: biome_lines(world)?,
        village,
        castes,
        beliefs: hornvale_religion::beliefs_of(world),
        calendar_lines: calendar_lines(world)?,
        night_sky: night_sky_line(world)?,
        genesis_notes: genesis_notes(world)?,
        settlement_lines: settlement_lines(world)?,
        culture_lines: culture_lines(world)?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constant(seed: u64) -> World {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap()
    }

    fn generated(seed: u64) -> World {
        build_world(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap()
    }

    #[test]
    fn build_world_generates_settlements_and_no_vale() {
        let world = generated(42);
        let places = hornvale_terrain::places(&world);
        assert!(
            places.len() >= 3,
            "expected a scatter of settlements, got {}",
            places.len()
        );
        assert!(
            !places.iter().any(|p| p.name == "the Vale"),
            "the Vale must be retired"
        );
        // The flagship carries a settlement + population.
        let village = hornvale_settlement::village_info(&world).expect("flagship settlement");
        assert!(village.population >= 40);
        // The cascade still runs on the flagship.
        assert!(!hornvale_culture::castes_of(&world, village.id).is_empty());
        assert!(!hornvale_religion::beliefs_of(&world).is_empty());
    }

    #[test]
    fn settlements_reorganize_between_spinning_and_locked() {
        use hornvale_astronomy::RotationPin;
        let spinning = generated(42);
        let locked = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::Locked),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let cells = |w: &World| {
            hornvale_terrain::places(w)
                .iter()
                .map(|p| p.name.clone())
                .collect::<Vec<_>>()
        };
        assert_ne!(
            cells(&spinning),
            cells(&locked),
            "people must reorganize under a different sky"
        );
    }

    #[test]
    fn settlement_lines_describe_the_people() {
        let lines = settlement_lines(&generated(42)).unwrap();
        assert!(!lines.is_empty());
        assert!(
            lines
                .iter()
                .any(|l| l.contains("settlement") || l.contains("village"))
        );
    }

    #[test]
    fn build_world_produces_the_full_cascade() {
        let world = constant(42);
        let places = hornvale_terrain::places(&world);
        assert!(!places.is_empty());
        let village = hornvale_settlement::village_info(&world).expect("village");
        assert!(!hornvale_culture::castes_of(&world, village.id).is_empty());
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
        let ctx = almanac_context(&world).unwrap();
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
        let sky = sky_report(&world, hornvale_kernel::WorldTime { day: 0.0 }).unwrap();
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
        let before = sky_report(&world, WorldTime { day: 0.0 }).unwrap();
        let reloaded = World::from_json(&world.to_json()).unwrap();
        assert!(matches!(sky_of(&reloaded).unwrap(), Sky::Generated(_)));
        let after = sky_report(&reloaded, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(before, after);
    }

    #[test]
    fn constant_choice_yields_constant_sky_and_unchanged_almanac_context() {
        let world = constant(42);
        assert!(matches!(sky_of(&world).unwrap(), Sky::Constant(_)));
        let ctx = almanac_context(&world).unwrap();
        assert!(ctx.sky.description.contains("zenith"));
    }

    #[test]
    fn absent_sky_provider_fact_falls_back_to_constant() {
        // A 1a/1b-era world never committed a sky-provider fact at all.
        let mut world = World::new(Seed(1));
        register_all(&mut world.registry).unwrap();
        assert!(matches!(sky_of(&world).unwrap(), Sky::Constant(_)));
    }

    #[test]
    fn constant_world_has_no_calendar_or_night_sky_or_notes() {
        let world = constant(42);
        assert!(calendar_lines(&world).unwrap().is_empty());
        assert!(night_sky_line(&world).unwrap().is_none());
        assert!(genesis_notes(&world).unwrap().is_empty());
    }

    #[test]
    fn locked_rotation_pin_yields_a_tidally_locked_calendar_line() {
        use hornvale_astronomy::RotationPin;
        let pins = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..SkyPins::default()
        };
        let world = build_world(
            Seed(42),
            &pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = calendar_lines(&world).unwrap();
        assert!(lines[0].contains("tidally locked"));
    }

    #[test]
    fn two_moon_pin_yields_first_and_second_moon_lines() {
        use hornvale_astronomy::MoonsPin;
        let pins = SkyPins {
            moons: Some(MoonsPin::exact(2).unwrap()),
            ..SkyPins::default()
        };
        let world = build_world(
            Seed(42),
            &pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = calendar_lines(&world).unwrap();
        assert!(lines.iter().any(|l| l.contains("first moon")));
        assert!(lines.iter().any(|l| l.contains("second moon")));
    }

    #[test]
    fn seed_23_generated_default_has_genesis_notes() {
        let world = generated(23);
        assert!(!genesis_notes(&world).unwrap().is_empty());
    }

    #[test]
    fn sky_of_rejects_an_unrecognized_sky_provider_value() {
        // A corrupted save: sky-provider names a provider that doesn't
        // exist. sky_of must error, never panic.
        let mut world = World::new(Seed(1));
        register_all(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                scenario_fact(
                    subject,
                    facts::SKY_PROVIDER,
                    Value::Text("zeppelin".to_string()),
                ),
                &world.registry,
            )
            .unwrap();
        assert!(matches!(sky_of(&world), Err(BuildError::Pins(_))));
    }

    #[test]
    fn sky_of_rejects_an_unparseable_scenario_pin() {
        // A corrupted save: a scenario-pin fact that doesn't parse. sky_of
        // must error, never panic.
        let mut world = World::new(Seed(1));
        register_all(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                scenario_fact(
                    subject,
                    facts::SKY_PROVIDER,
                    Value::Text("generated".to_string()),
                ),
                &world.registry,
            )
            .unwrap();
        world
            .ledger
            .commit(
                scenario_fact(
                    subject,
                    facts::SCENARIO_PIN,
                    Value::Text("moons=banana".to_string()),
                ),
                &world.registry,
            )
            .unwrap();
        assert!(matches!(sky_of(&world), Err(BuildError::Pins(_))));
    }

    #[test]
    fn corrupt_scenario_pin_errors_instead_of_panicking_through_every_accessor() {
        // The same corrupted-save shape as above, driven through every
        // accessor that used to `.expect()` on sky_of. None may panic.
        let mut world = World::new(Seed(1));
        register_all(&mut world.registry).unwrap();
        let subject = world.ledger.mint_entity();
        world
            .ledger
            .commit(
                scenario_fact(
                    subject,
                    facts::SKY_PROVIDER,
                    Value::Text("generated".to_string()),
                ),
                &world.registry,
            )
            .unwrap();
        world
            .ledger
            .commit(
                scenario_fact(
                    subject,
                    facts::SCENARIO_PIN,
                    Value::Text("moons=banana".to_string()),
                ),
                &world.registry,
            )
            .unwrap();

        assert!(sky_report(&world, WorldTime { day: 0.0 }).is_err());
        // No place exists on this hand-built world, so observed_phenomena
        // short-circuits to Ok(empty) before ever touching sky_of — the
        // existing "no place, no phenomena" contract, not a panic risk.
        assert_eq!(observed_phenomena(&world, 0.0).unwrap(), Vec::new());
        assert!(calendar_lines(&world).is_err());
        assert!(night_sky_line(&world).is_err());
        assert!(genesis_notes(&world).is_err());
        assert!(almanac_context(&world).is_err());
    }

    #[test]
    fn sky_calendar_accessor_present_for_generated_absent_for_constant() {
        assert!(sky_of(&constant(42)).unwrap().calendar().is_none());
        let generated_sky = sky_of(&generated(42)).unwrap();
        let cal = generated_sky
            .calendar()
            .expect("generated sky has a calendar");
        assert!(cal.year_length().get() > 0.0);
    }

    #[test]
    fn terrain_reconstructs_from_seed_and_pins() {
        let world = constant(42);
        let a = terrain_of(&world).unwrap();
        let b = terrain_of(&world).unwrap();
        assert_eq!(a.globe(), b.globe());
        assert_eq!(a.geosphere().level(), hornvale_terrain::GLOBE_LEVEL);
    }

    #[test]
    fn terrain_pins_round_trip_through_the_ledger() {
        let pins = hornvale_terrain::TerrainPins {
            plates: Some(12),
            ocean_fraction: Some(0.7),
            ..hornvale_terrain::TerrainPins::default()
        };
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &pins,
            &SettlementPins::default(),
        )
        .unwrap();
        let terrain = terrain_of(&world).unwrap();
        assert_eq!(terrain.globe().plates.len(), 12);
        let summary = hornvale_terrain::summarize(terrain.globe());
        assert!((summary.ocean_fraction - 0.7).abs() < 0.01);
    }

    #[test]
    fn terrain_facts_are_committed_at_build() {
        let world = constant(42);
        assert!(
            world
                .ledger
                .find(hornvale_terrain::facts::PLATE_COUNT)
                .next()
                .is_some()
        );
        assert!(
            world
                .ledger
                .find(hornvale_terrain::facts::OCEAN_FRACTION)
                .next()
                .is_some()
        );
    }

    #[test]
    fn land_lines_describe_the_globe() {
        let world = constant(42);
        let lines = land_lines(&world).unwrap();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].contains("plates"));
        assert!(lines[1].contains("above the sea"));
    }

    #[test]
    fn climate_reconstructs_deterministically_and_maps_biomes() {
        let world = generated(42);
        let a = climate_of(&world).unwrap();
        let b = climate_of(&world).unwrap();
        assert_eq!(a.biome_map(), b.biome_map());
        assert_eq!(a.geosphere().level(), hornvale_terrain::GLOBE_LEVEL);
        // A generated spinning world has a band count; ocean cells are marine.
        assert!(
            a.band_count().is_some()
                || matches!(a.regime(), hornvale_climate::RotationRegime::Locked)
        );
    }

    #[test]
    fn locked_and_spinning_biome_maps_reorganize_from_the_same_land() {
        use hornvale_astronomy::RotationPin;
        let spinning = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let locked = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::Locked),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        // Same land seed, different sky → different biome map.
        assert_ne!(
            climate_of(&spinning).unwrap().biome_map(),
            climate_of(&locked).unwrap().biome_map()
        );
    }

    #[test]
    fn biome_lines_describe_the_globe() {
        let lines = biome_lines(&generated(42)).unwrap();
        assert!(!lines.is_empty());
        assert!(lines.iter().any(|l| l.contains("habitable")));
    }

    #[test]
    fn constant_sky_world_still_has_a_climate() {
        let world = constant(42);
        let climate = climate_of(&world).unwrap();
        assert!(climate.geosphere().cell_count() > 0);
    }

    /// The flagship's cell, read back from its committed `CELL_ID` fact
    /// (independent of `placements`, which build_world already consumed).
    fn flagship_cell(world: &World, village_id: EntityId) -> hornvale_kernel::CellId {
        match world
            .ledger
            .value_of(village_id, hornvale_settlement::CELL_ID)
        {
            Some(Value::Number(n)) => hornvale_kernel::CellId(*n as u32),
            _ => panic!("flagship has no cell-id fact"),
        }
    }

    #[test]
    fn flagship_has_a_non_empty_subsistence_and_castes() {
        let world = generated(42);
        let village = hornvale_settlement::village_info(&world).expect("flagship settlement");
        assert!(hornvale_culture::subsistence_of(&world, village.id).is_some());
        assert!(!hornvale_culture::castes_of(&world, village.id).is_empty());
    }

    #[test]
    fn flagship_castes_match_the_structure_recomputed_from_its_environment() {
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let geo = terrain.geosphere();
        let village = hornvale_settlement::village_info(&world).expect("flagship settlement");
        let cell = flagship_cell(&world, village.id);

        let coastal = geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(cell);
        let class = biome_class(climate.biome_at(cell));
        let subsistence = hornvale_culture::subsistence(class, coastal);
        let surplus = (hornvale_culture::fertility(class) * moisture).clamp(0.0, 1.0);
        let threat = terrain.unrest_at(cell).clamp(0.0, 1.0);
        let env = hornvale_culture::EnvSummary {
            subsistence,
            surplus,
            population: village.population,
            threat,
        };

        assert_eq!(
            hornvale_culture::castes_of(&world, village.id),
            hornvale_culture::structure(&env)
        );
        assert_eq!(
            hornvale_culture::subsistence_of(&world, village.id).as_deref(),
            Some(subsistence.name())
        );
    }

    #[test]
    fn locked_rotation_changes_the_flagship_cascade() {
        // Seed 7 (not 42: at 42 the globally best cell happens to land in
        // the same class under both regimes, so the cascade coincides).
        use hornvale_astronomy::RotationPin;
        let spinning = generated(7);
        let locked = build_world(
            Seed(7),
            &SkyPins {
                rotation: Some(RotationPin::Locked),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let cascade_state = |w: &World| {
            hornvale_settlement::village_info(w).map(|v| {
                (
                    hornvale_culture::subsistence_of(w, v.id),
                    hornvale_culture::castes_of(w, v.id),
                )
            })
        };
        assert_ne!(
            cascade_state(&spinning),
            cascade_state(&locked),
            "a different sky must enrich the flagship's environment differently"
        );
    }

    #[test]
    fn min_suitability_pin_reduces_the_settlement_count() {
        let default_world = generated(42);
        let pinned_world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins {
                min_suitability: Some(0.5),
            },
        )
        .unwrap();
        let default_count = hornvale_terrain::places(&default_world).len();
        let pinned_count = hornvale_terrain::places(&pinned_world).len();
        assert!(
            pinned_count < default_count,
            "raising the suitability floor must reduce the settlement count: {pinned_count} >= {default_count}"
        );
    }

    #[test]
    fn culture_lines_name_the_flagship_and_its_subsistence() {
        let world = generated(42);
        let village = hornvale_settlement::village_info(&world).expect("flagship settlement");
        let subsistence =
            hornvale_culture::subsistence_of(&world, village.id).expect("flagship subsistence");
        let lines = culture_lines(&world).unwrap();
        assert!(!lines.is_empty());
        assert!(lines[0].contains(&village.name));
        assert!(lines[0].contains(&subsistence));
    }
}
