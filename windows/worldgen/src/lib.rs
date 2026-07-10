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
    ConceptRegistry, EntityId, Fact, GeoCoord, Geosphere, LedgerError, ObserverContext,
    PerceptionLens, PhenomenaSource, Phenomenon, RegistryError, Seed, Value, World, WorldTime,
    observe,
};
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins};
use std::sync::OnceLock;

pub mod settlement_pins;
pub use settlement_pins::SettlementPins;

/// Errors from building a world.
/// type-audit: bare-ok(prose: Pins.0)
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

    /// The generated star system, if this world has one. `None` for the
    /// tier-0 constant sun. The star-chart command reads this.
    pub fn system(&self) -> Option<&hornvale_astronomy::StarSystem> {
        match self {
            Sky::Constant(_) => None,
            Sky::Generated(sky) => Some(sky.system()),
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
    hornvale_species::register_concepts(registry)?;
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

/// Per-species suitability weights derived from the psychology vector
/// (spec §4); identity at the goblin baseline.
pub fn species_weights(
    p: &hornvale_species::PsychVector,
) -> hornvale_settlement::SuitabilityWeights {
    hornvale_settlement::SuitabilityWeights {
        freshwater: 0.45 * (0.5 + p.time_horizon),
        coast: 0.20 * (2.0 * p.in_group_radius),
        temperance: 0.35,
        hostility: 0.50 * (1.5 - p.threat_response),
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
/// type-audit: bare-ok(prose: return)
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
/// type-audit: bare-ok(prose: return)
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

/// The geographic position of a place, from its committed latitude/longitude
/// facts (each set from `Geosphere::coord` at genesis — `domains/settlement`).
/// `None` for a place carrying no such facts (a legacy or non-settlement
/// place), leaving the observation position-blind — the pre-vantage behavior.
fn place_coord(world: &World, place: EntityId) -> Option<GeoCoord> {
    let latitude = match world
        .ledger
        .value_of(place, hornvale_settlement::LATITUDE)?
    {
        Value::Number(n) => *n,
        _ => return None,
    };
    let longitude = match world
        .ledger
        .value_of(place, hornvale_settlement::LONGITUDE)?
    {
        Value::Number(n) => *n,
        _ => return None,
    };
    Some(GeoCoord {
        latitude,
        longitude,
    })
}

/// The tier-0/1/2 phenomena sources, observed from the world's first place —
/// the flagship (SEQ-4). The vantage's hemisphere culls the sky (SEQ-5).
/// type-audit: pending(wave-3: day)
pub fn observed_phenomena(world: &World, day: f64) -> Result<Vec<Phenomenon>, BuildError> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let position = place_coord(world, place);
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: PerceptionLens::identity(),
            position,
        },
    ))
}

/// Derive a species' perception lens from its authored vector (spec §4).
/// Identity at the goblin baseline (Diurnal, 0.5, 0.5) by construction:
/// every factor is exactly 1.0 there.
pub fn perception_lens(p: &hornvale_species::PerceptionVector) -> PerceptionLens {
    let activity_factor = match p.activity {
        hornvale_species::ActivityCycle::Diurnal => 1.0,
        hornvale_species::ActivityCycle::Crepuscular => 0.7,
        hornvale_species::ActivityCycle::Nocturnal => 0.4,
    };
    let sky = 0.5 + p.sky_attention;
    PerceptionLens {
        day_sky: activity_factor * sky,
        night_sky: (0.5 + p.night_vision) * sky,
        ambient: 1.5 - p.sky_attention,
    }
}

/// The characteristic hour: when a species with this activity cycle
/// observes (spec §5). Diurnal observes at day 0.0 (the legacy path,
/// byte-identical); Nocturnal at the first non-daylight instant found by a
/// deterministic scan of 1/24-local-day steps over two local days;
/// Crepuscular at the first light/dark boundary the same scan finds.
/// Worlds without a day/night cycle (constant sun, tidal lock) observe at
/// day 0.0 regardless.
/// type-audit: pending(wave-3: return)
pub fn observation_time(
    world: &World,
    activity: hornvale_species::ActivityCycle,
) -> Result<f64, BuildError> {
    use hornvale_species::ActivityCycle;
    if activity == ActivityCycle::Diurnal {
        return Ok(0.0);
    }
    let sky = sky_of(world)?;
    let Some(calendar) = sky.calendar() else {
        return Ok(0.0);
    };
    let Some(day_len) = calendar.day_length() else {
        return Ok(0.0); // locked: no day/night cycle
    };
    let step = day_len.get() / 24.0;
    let daylight_at = |t: f64| {
        hornvale_astronomy::StdDays::new(t)
            .ok()
            .and_then(|d| calendar.is_daylight(d))
    };
    let at_zero = daylight_at(0.0);
    for k in 0..48 {
        let t = k as f64 * step;
        let here = daylight_at(t);
        let hit = match activity {
            ActivityCycle::Nocturnal => here == Some(false),
            ActivityCycle::Crepuscular => here != at_zero,
            ActivityCycle::Diurnal => unreachable!("early-returned above"),
        };
        if hit {
            return Ok(t);
        }
    }
    Ok(0.0) // pathological all-daylight window: fall back deterministically
}

/// The shipped species roster — the whole authored registry, in key order.
/// The default every shipped verb builds with (spec §3).
pub fn default_roster() -> Vec<hornvale_species::SpeciesDef> {
    hornvale_species::registry().into_values().collect()
}

/// Resolve `species` within `roster` or fail loudly.
fn def_in<'a>(
    roster: &'a [hornvale_species::SpeciesDef],
    species: &str,
) -> Result<&'a hornvale_species::SpeciesDef, BuildError> {
    roster.iter().find(|d| d.name == species).ok_or_else(|| {
        let known: Vec<&str> = roster.iter().map(|d| d.name).collect();
        BuildError::Pins(format!(
            "unknown species '{species}'; roster: {}",
            known.join(", ")
        ))
    })
}

/// The phenomena a species (resolved within `roster`) observes.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_in(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
) -> Result<Vec<Phenomenon>, BuildError> {
    let def = def_in(roster, species)?;
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let day = observation_time(world, def.perception.activity)?;
    let position = place_coord(world, place);
    let sky = sky_of(world)?;
    let climate = UniformClimate;
    let sources: [&dyn PhenomenaSource; 2] = [&sky, &climate];
    Ok(observe(
        &sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: perception_lens(&def.perception),
            position,
        },
    ))
}

/// The phenomena a species observes: its characteristic hour, its lens,
/// the world's first place (spec §5 — the place debt is SEQ-4's). Resolves
/// `species` within the shipped default roster.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as(world: &World, species: &str) -> Result<Vec<Phenomenon>, BuildError> {
    observed_phenomena_as_in(world, &default_roster(), species)
}

/// Map a species' articulation vector onto language's own `Envelope` copy
/// (spec §7): every scalar dimension is a direct 1:1 carry — both vectors
/// share the same 0–1 scale and semantics — and `ExoticManner` maps onto
/// language's own `ExoticSeg` one variant at a time. This is the only place
/// either vector is ever converted; language never imports species.
pub fn envelope_of(art: &hornvale_species::ArticulationVector) -> hornvale_language::Envelope {
    hornvale_language::Envelope {
        labiality: art.labiality,
        vowel_space: art.vowel_space,
        voicing: art.voicing,
        sibilance: art.sibilance,
        voice_loudness: art.voice_loudness,
        exotic: match art.exotic {
            hornvale_species::ExoticManner::None => hornvale_language::ExoticSeg::None,
            hornvale_species::ExoticManner::Trill => hornvale_language::ExoticSeg::Trill,
            hornvale_species::ExoticManner::Click => hornvale_language::ExoticSeg::Click,
            hornvale_species::ExoticManner::Ejective => hornvale_language::ExoticSeg::Ejective,
        },
    }
}

/// Draw `species`' phonology from this world's seed and its authored
/// articulation vector, resolving `species` within `roster` — rebuildable
/// from (seed, species, envelope) alone, the same reconstruction idiom as
/// `terrain_of`/`sky_of`/`climate_of`. The single construction site for a
/// species' `Phonology`. Panics if `species` is not in `roster`; every
/// caller sources `species` from the same roster it passes here.
/// type-audit: bare-ok(identifier-text: species)
pub fn language_of_in(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
) -> hornvale_language::Phonology {
    let def = def_in(roster, species).unwrap_or_else(|e| panic!("language_of_in: {e}"));
    hornvale_language::draw_phonology(&world.seed, species, &envelope_of(&def.articulation))
}

/// Draw a species' phonology, resolving `species` within the shipped
/// default roster.
/// type-audit: bare-ok(identifier-text: species)
pub fn language_of(world: &World, species: &str) -> hornvale_language::Phonology {
    language_of_in(world, &default_roster(), species)
}

/// A status basis' contribution to the `formality`/`epithet_density` voice
/// knobs (spec §7): `Rank` — the goblin baseline — reads as the "high" end;
/// `Knowledge`/`Generosity` read lower. `Rank`'s value is fixed at exactly
/// the voice engine's identity value, 0.5, so a Rank-basis species with an
/// otherwise-baseline psychology nets out to identity by construction.
fn status_register(status: hornvale_species::StatusBasis) -> f64 {
    use hornvale_species::StatusBasis;
    match status {
        StatusBasis::Rank => 0.5,
        StatusBasis::Knowledge | StatusBasis::Generosity => 0.2,
    }
}

/// A sociality mode's contribution to the `repetition` voice knob (spec
/// §7): `Communal` reads high (kobold's echoed refrains); `Hierarchic` — the
/// goblin baseline — sits at the voice engine's identity value, 0.5.
fn sociality_register(sociality: hornvale_species::Sociality) -> f64 {
    use hornvale_species::Sociality;
    match sociality {
        Sociality::Hierarchic => 0.5,
        Sociality::Communal => 0.8,
    }
}

/// Derive a species' voice knobs from its psychology vector (spec §7):
/// `formality` blends the status register with deliberation latency;
/// `repetition` follows sociality alone; `epithet_density` follows the
/// status register alone. Every field is exactly the identity 0.5 at the
/// goblin baseline (`Rank`, `Hierarchic`, `deliberation_latency == 0.5`):
/// `status_register(Rank) == sociality_register(Hierarchic) == 0.5`, so
/// blending two 0.5s (or reading either directly) always yields 0.5.
pub fn voice_params(psych: &hornvale_species::PsychVector) -> hornvale_language::VoiceParams {
    hornvale_language::VoiceParams {
        formality: (status_register(psych.status_basis) + psych.deliberation_latency) / 2.0,
        repetition: sociality_register(psych.sociality),
        epithet_density: status_register(psych.status_basis),
    }
}

/// Derive a species' naming morphology from its psychology vector (spec
/// §7): honorifics are drawn only for a rank-based status basis — the
/// goblin baseline — matching `voice_params`' epithet-density reading of
/// the same field.
pub fn morph_options(psych: &hornvale_species::PsychVector) -> hornvale_language::MorphOptions {
    hornvale_language::MorphOptions {
        honorifics: psych.status_basis == hornvale_species::StatusBasis::Rank,
    }
}

/// Backs religion's `DeityNamer` trait with a species' language `Namer`.
/// Each deity name and epithet is a single deterministic draw salted by the
/// belief's own id — no shared "used" set, no re-draw (names are pure
/// functions of seed+species+kind+salt, spec §8). Religion never learns
/// this exists; it only ever sees the `DeityNamer` trait (spec §6's
/// ignorance discipline).
struct LanguageDeityNamer<'a, 'b> {
    namer: &'a hornvale_language::Namer<'b>,
    morph: hornvale_language::MorphOptions,
}

impl hornvale_religion::DeityNamer for LanguageDeityNamer<'_, '_> {
    fn deity(&mut self, salt: u64) -> (String, String) {
        let g = self
            .namer
            .name(hornvale_language::NameKind::Deity, salt, &self.morph);
        (g.roman, g.ipa)
    }

    fn epithet(&mut self, salt: u64, _sentiment: hornvale_religion::Sentiment) -> (String, String) {
        // Sentiment fits the epithet at render time (Task 11's `render_line`
        // reads it from the belief's own committed `sentiment` fact); the
        // generated word itself is sentiment-agnostic.
        let g = self
            .namer
            .name(hornvale_language::NameKind::Epithet, salt, &self.morph);
        (g.roman, g.ipa)
    }
}

/// Build a complete world against a given species `roster`: mint the world
/// entity and record its sky choice and scenario pins first; run sky
/// genesis for `Generated`; commit the terrain pins and run tectonic
/// genesis; then assemble per-cell site inputs from terrain and climate,
/// place a spaced scatter of settlements (honoring the settlement pins'
/// suitability floor), commit each as its own place entity, and run the
/// culture/religion cascade on the flagship (the most-suitable settlement,
/// placed first) from its actual environment.
pub fn build_world_with_roster(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
    roster: &[hornvale_species::SpeciesDef],
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
            // Seawater is not freshwater: coastal access is priced by the
            // coast term in settlement's suitability, not smuggled in here.
            let freshwater = drainage_norm.max(moisture).clamp(0.0, 1.0);
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

    // Which species this world places: the whole roster, or the pinned one.
    let species_set: Vec<&hornvale_species::SpeciesDef> = match &settlement_pins.species {
        None => roster.iter().collect(),
        Some(name) => vec![def_in(roster, name)?],
    };

    // Joint greedy across species: every (site × species) pair scored with
    // that species' psychology-derived weights, one shared spacing pass.
    let mut scored: Vec<(hornvale_settlement::SiteInput, f64, u32)> = Vec::new();
    for (tag, def) in species_set.iter().enumerate() {
        let weights = species_weights(&def.psych);
        for site in &sites {
            if let Some(score) = hornvale_settlement::suitability_weighted(site, &weights) {
                scored.push((*site, score, tag as u32));
            }
        }
    }
    let placements = hornvale_settlement::place_tagged(&scored, min_sep, floor);

    // Each placed species' phonology, drawn once from the world seed and
    // its authored articulation vector, and a `Namer` built over it. Every
    // name below — settlement, then deity/epithet — is a single
    // deterministic draw salted by the entity's own id (the settlement cell,
    // the belief). No shared "used" set threads through them: names are pure
    // functions of seed+species+kind+salt, so settlement names are
    // pin-isolated by construction (spec §8) and cross-world uniqueness is
    // de-facto (measured as a calibration, spec §9), not enforced.
    let phonologies: std::collections::BTreeMap<&str, hornvale_language::Phonology> = species_set
        .iter()
        .map(|def| (def.name, language_of_in(&world, roster, def.name)))
        .collect();
    let namers: std::collections::BTreeMap<&str, hornvale_language::Namer> = phonologies
        .iter()
        .map(|(name, ph)| (*name, hornvale_language::Namer::new(&seed, name, ph)))
        .collect();

    let mut placed: Vec<hornvale_settlement::PlacedSettlement> =
        Vec::with_capacity(placements.len());
    for (p, tag) in &placements {
        let def = species_set[*tag as usize];
        let coord = geo.coord(p.cell);
        let salt = u64::from(p.cell.0);
        let namer = namers
            .get(def.name)
            .expect("a Namer was built for every placed species");
        let morph = morph_options(&def.psych);
        let generated = namer.name(hornvale_language::NameKind::Settlement, salt, &morph);
        let population = if def.name == "goblin" {
            hornvale_settlement::draw_population(seed, salt, p.suitability)
        } else {
            hornvale_settlement::draw_species_population(seed, def.name, salt, p.suitability)
        };
        placed.push(hornvale_settlement::PlacedSettlement {
            cell: p.cell.0,
            latitude: coord.latitude,
            longitude: coord.longitude,
            biome: climate.biome_at(p.cell).name().to_string(),
            name: generated.roman,
            population,
        });
    }
    let ids = hornvale_settlement::genesis(&mut world, &placed)?;

    // Per-species flagship culture and religion.
    for (tag, def) in species_set.iter().enumerate() {
        let Some(pos) = placements.iter().position(|(_, t)| *t as usize == tag) else {
            continue; // a species may place nothing on a hostile world
        };
        let flagship = ids[pos];
        let fcell = placements[pos].0.cell;
        let coastal = geo.neighbors(fcell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(fcell);
        let class = biome_class(climate.biome_at(fcell));
        let subsistence = hornvale_culture::subsistence(class, coastal);
        let surplus = (hornvale_culture::fertility(class) * moisture).clamp(0.0, 1.0);
        let threat = terrain.unrest_at(fcell).clamp(0.0, 1.0);
        let env = hornvale_culture::EnvSummary {
            subsistence,
            surplus,
            population: placed[pos].population,
            threat,
        };
        let psych = hornvale_culture::PsychSummary {
            threat_response: def.psych.threat_response,
            time_horizon: def.psych.time_horizon,
            communal: def.psych.sociality == hornvale_species::Sociality::Communal,
            rank_status: def.psych.status_basis == hornvale_species::StatusBasis::Rank,
            vocabulary: hornvale_culture::RoleVocabulary {
                worker_override: def.worker_override.map(str::to_string),
                warrior: def.warrior.to_string(),
                artisan: def.artisan.to_string(),
                shaman: def.shaman.to_string(),
                top: def.top.to_string(),
            },
        };
        hornvale_culture::genesis(&mut world, flagship, &env, &psych)?;

        // Religion for every species-flagship (spec §5): each species sees
        // the sky through its own lens at its own hour. The priesthood
        // check uses the species' own shaman-rung word — kobold "keeper"
        // is a priesthood exactly as goblin "shaman" is.
        let castes = hornvale_culture::castes_of(&world, flagship);
        let society = hornvale_religion::SocietySummary {
            strata: castes.len(),
            has_priesthood: castes.iter().any(|c| c == def.shaman),
        };
        let seen = observed_phenomena_as_in(&world, roster, def.name)?;
        let namer = namers
            .get(def.name)
            .expect("a Namer was built for every placed species");
        let morph = morph_options(&def.psych);
        let mut deity_namer = LanguageDeityNamer { namer, morph };
        hornvale_religion::genesis(&mut world, flagship, &seen, &society, &mut deity_namer)?;
    }

    // Species entities AFTER every pre-species subsystem (settlements,
    // culture, religion) — entity-id stability, spec §8: a goblin-pinned
    // world must mint the exact same ids for pre-C1 entities as pre-species
    // main, so the new, Y2-1-only entities are appended last rather than
    // interleaved. Then the peopled-by link for every settlement.
    hornvale_species::genesis_in(&mut world, roster)?;
    for (id, (_, tag)) in ids.iter().zip(placements.iter()) {
        hornvale_species::people(&mut world, *id, species_set[*tag as usize].name)?;
    }
    Ok(world)
}

/// Build a complete world with the shipped species roster.
pub fn build_world(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
) -> Result<World, BuildError> {
    build_world_with_roster(
        seed,
        pins,
        sky,
        terrain_pins,
        settlement_pins,
        &default_roster(),
    )
}

/// The first-placed settlement of `species` (its flagship), if any.
/// type-audit: bare-ok(identifier-text: species)
pub fn flagship_of(world: &World, species: &str) -> Option<hornvale_settlement::VillageInfo> {
    let id = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .find(|s| hornvale_species::species_of(world, *s).as_deref() == Some(species))?;
    let name = world
        .ledger
        .text_of(id, hornvale_kernel::NAME)
        .map(str::to_string)
        .unwrap_or_else(|| format!("settlement {}", id.0));
    let population = match world.ledger.value_of(id, hornvale_settlement::POPULATION) {
        Some(Value::Number(n)) => *n as u32,
        _ => 0,
    };
    Some(hornvale_settlement::VillageInfo {
        id,
        name,
        population,
    })
}

/// Headline lines describing the world's people for the almanac: how many
/// settlements, then one chief-settlement line per species that placed
/// (registry order, goblin first). A world with exactly one such species
/// keeps the legacy unprefixed wording — byte-stable for goblin-only worlds;
/// two-or-more-species worlds prefix each chief line with its species.
/// type-audit: bare-ok(prose: return)
pub fn settlement_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let places = hornvale_terrain::places(world);
    let mut lines = vec![format!("The land holds {} settlement(s).", places.len())];

    let registry = hornvale_species::registry();
    let flagships: Vec<(&str, hornvale_settlement::VillageInfo)> = registry
        .keys()
        .filter_map(|name| flagship_of(world, name).map(|v| (*name, v)))
        .collect();
    let multi_species = flagships.len() > 1;

    for (species, v) in &flagships {
        let biome = places
            .iter()
            .find(|p| p.id == v.id)
            .map(|p| p.biome.clone())
            .unwrap_or_else(|| "unknown".to_string());
        if multi_species {
            lines.push(format!(
                "The chief {species} settlement, {}, holds {} souls amid {}.",
                v.name, v.population, biome
            ));
        } else {
            lines.push(format!(
                "The chief settlement, {}, holds {} souls amid {}.",
                v.name, v.population, biome
            ));
        }
    }
    Ok(lines)
}

/// Headline culture lines for the almanac's People section: the flagship's
/// subsistence mode, then a one-line summary of its emergent role structure.
/// Empty when the flagship has no committed culture yet.
/// type-audit: bare-ok(prose: return)
pub fn culture_lines(world: &World, flagship: &hornvale_settlement::VillageInfo) -> Vec<String> {
    let Some(subsistence) = hornvale_culture::subsistence_of(world, flagship.id) else {
        return Vec::new();
    };
    let castes = hornvale_culture::castes_of(world, flagship.id);
    vec![
        format!("{} lives by {subsistence}.", flagship.name),
        format!("Its roles, lowest to highest: {}.", castes.join(", ")),
    ]
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
/// type-audit: bare-ok(prose: return)
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
        // The daylight range at the flagship's own latitude (SKY-8) — the
        // placed observer's sky. The year-phase offset (Plan 1) moves where the
        // solstices fall in `t`, so scan the year for the extremes. Falls back
        // to the equator (a flat half) if no vantage resolves.
        let latitude = hornvale_terrain::places(world)
            .first()
            .and_then(|p| place_coord(world, p.id))
            .map(|c| c.latitude)
            .unwrap_or(0.0);
        let year = calendar.year_length().get();
        let (mut max, mut min) = (0.0_f64, 1.0_f64);
        for k in 0..365 {
            let t = hornvale_astronomy::StdDays::new(k as f64 * year / 365.0).unwrap();
            if let Some(f) = calendar.daylight_fraction_at(t, latitude) {
                max = max.max(f);
                min = min.min(f);
            }
        }
        lines.push(format!(
            "Daylight swells to {:.0}% at midsummer and shrinks to {:.0}% at midwinter.",
            max * 100.0,
            min * 100.0
        ));
    }

    for (index, moon) in system.moons.iter().enumerate() {
        let ordinal = moon_ordinal(index);
        // `months_per_year` is `None` only in the degenerate case P_sid ≥ Y,
        // unreachable at genesis (the Hill cap keeps P_sid ≤ ~0.15×Y) but
        // handled honestly rather than panicking.
        match calendar.months_per_year(index) {
            Some(months) => match day_std {
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
            },
            None => lines.push(format!(
                "The {ordinal} moon circles — no phase cycle, its orbit outpaces the year."
            )),
        }
    }

    Ok(lines)
}

/// The night sky as a single sentence naming its notable neighbor stars,
/// brightest first. `None` for constant-sky worlds, which have no
/// neighborhood to describe.
/// type-audit: bare-ok(prose: return)
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
/// type-audit: bare-ok(prose: return)
pub fn genesis_notes(world: &World) -> Result<Vec<String>, BuildError> {
    let sky = sky_of(world)?;
    Ok(match &sky {
        Sky::Constant(_) => Vec::new(),
        Sky::Generated(sky) => sky.notes().to_vec(),
    })
}

/// Map religion's `Sentiment` onto language's own copy of the same
/// distinction (spec §6): `domains/language` never imports
/// `hornvale-religion`, so this conversion lives only here, at the
/// composition root.
fn line_sentiment_of(sentiment: hornvale_religion::Sentiment) -> hornvale_language::LineSentiment {
    match sentiment {
        hornvale_religion::Sentiment::Eternal => hornvale_language::LineSentiment::Eternal,
        hornvale_religion::Sentiment::Cyclic => hornvale_language::LineSentiment::Cyclic,
        hornvale_religion::Sentiment::Ambient => hornvale_language::LineSentiment::Ambient,
    }
}

/// Build one belief's `LineContent` (spec §6). Every field but the period
/// comes straight off the belief's own committed facts; `period_days` is
/// **not** itself a committed fact (`religion::genesis` never stores one on
/// the belief), so it is recovered from `phenomenon` — the source
/// observation at this belief's own position in the species' salience-
/// ranked list, the exact list `genesis` consumed, in that exact order, to
/// mint it. `None` (an eternal/ambient tenet, or a phenomenon that's gone
/// missing) simply renders no period.
fn line_content_for(
    belief: &hornvale_religion::Belief,
    phenomenon: Option<&Phenomenon>,
) -> hornvale_language::LineContent {
    hornvale_language::LineContent {
        deity: belief.deity.clone(),
        epithet: belief.epithet.clone(),
        sentiment: line_sentiment_of(belief.sentiment),
        period_days: phenomenon.and_then(|p| p.period_days),
        high_god: belief.high_god,
    }
}

/// Render every belief in `beliefs` — one community's pantheon, in commit
/// order — into its tenet, voiced by `voice`. `phenomena` must be the same
/// species-observed, salience-ranked list `religion::genesis` consumed to
/// mint these beliefs, so position `i` in each list names the same deity
/// (see [`line_content_for`]).
fn tenets_for(
    beliefs: &[hornvale_religion::Belief],
    phenomena: &[Phenomenon],
    voice: &hornvale_language::VoiceParams,
) -> Vec<String> {
    beliefs
        .iter()
        .enumerate()
        .map(|(i, b)| hornvale_language::render_line(&line_content_for(b, phenomena.get(i)), voice))
        .collect()
}

/// One species-flagship's pantheon, each belief paired with its rendered
/// tenet: the settlement holding it and the `(Belief, tenet)` pairs, head
/// first.
type RenderedPantheon = (
    hornvale_settlement::VillageInfo,
    Vec<(hornvale_religion::Belief, String)>,
);

/// `None` if `species` placed no flagship, or its flagship holds no
/// beliefs. The single site both [`almanac_context`] and
/// [`rendered_beliefs`] build a species' voiced pantheon (the
/// content→render seam, spec §6) through.
fn rendered_pantheon_of(
    world: &World,
    species: &str,
    def: &hornvale_species::SpeciesDef,
) -> Result<Option<RenderedPantheon>, BuildError> {
    let Some(v) = flagship_of(world, species) else {
        return Ok(None);
    };
    let beliefs = hornvale_religion::beliefs_held_by(world, v.id);
    if beliefs.is_empty() {
        return Ok(None);
    }
    let phenomena = observed_phenomena_as(world, species)?;
    let voice = voice_params(&def.psych);
    let tenets = tenets_for(&beliefs, &phenomena, &voice);
    Ok(Some((v, beliefs.into_iter().zip(tenets).collect())))
}

/// Render a pre-species save's beliefs (`beliefs_of` — no `peopled-by`
/// facts exist to recover a species from), as the single anonymous pantheon
/// they always were: voiced at the goblin baseline (voice's identity value,
/// since no species is recoverable) with periods recovered from the world's
/// unlensed phenomena (the pre-species observation path,
/// `observed_phenomena`). Empty if the world has no beliefs at all. Shared
/// by [`rendered_beliefs`] and [`almanac_context`]'s legacy fallback.
fn legacy_rendered_beliefs(
    world: &World,
) -> Result<Vec<(hornvale_religion::Belief, String)>, BuildError> {
    let beliefs = hornvale_religion::beliefs_of(world);
    if beliefs.is_empty() {
        return Ok(Vec::new());
    }
    let phenomena = observed_phenomena(world, 0.0)?;
    let voice = hornvale_language::VoiceParams {
        formality: 0.5,
        repetition: 0.5,
        epithet_density: 0.5,
    };
    let tenets = tenets_for(&beliefs, &phenomena, &voice);
    Ok(beliefs.into_iter().zip(tenets).collect())
}

/// Every belief in the world, each paired with its rendered tenet — grouped
/// by species-flagship pantheon in registry order (goblin first), each
/// pantheon's beliefs head first (matching `beliefs_of`/`beliefs_held_by`'s
/// own ordering). The seam-wiring site the REPL's `beliefs` command renders
/// through (spec §6, Task 11); the almanac renders the same seam via
/// [`almanac_context`]'s `PantheonBlock`s. Legacy fallback: see
/// [`legacy_rendered_beliefs`].
/// type-audit: bare-ok(prose: return)
pub fn rendered_beliefs(
    world: &World,
) -> Result<Vec<(hornvale_religion::Belief, String)>, BuildError> {
    let mut out = Vec::new();
    for (name, def) in hornvale_species::registry() {
        if let Some((_, rendered)) = rendered_pantheon_of(world, name, &def)? {
            out.extend(rendered);
        }
    }
    if out.is_empty() {
        out = legacy_rendered_beliefs(world)?;
    }
    Ok(out)
}

/// Gather everything the almanac renders, reconstructing the stateless
/// tier-0 providers.
pub fn almanac_context(world: &World) -> Result<AlmanacContext, BuildError> {
    let registry = hornvale_species::registry();
    let peoples = registry
        .iter()
        .filter_map(|(name, def)| {
            let flagship = flagship_of(world, name)?;
            Some(hornvale_almanac::PeopleBlock {
                species: (*name).to_string(),
                noun: def.noun.to_string(),
                name: flagship.name.clone(),
                population: flagship.population,
                culture_lines: culture_lines(world, &flagship),
            })
        })
        .collect();
    Ok(AlmanacContext {
        seed: world.seed.0,
        sky: sky_report(world, WorldTime { day: 0.0 })?,
        climate: climate_report(world),
        phenomena: observed_phenomena(world, 0.0)?,
        places: hornvale_terrain::places(world),
        land_lines: land_lines(world)?,
        biome_lines: biome_lines(world)?,
        peoples,
        pantheons: {
            let mut blocks = Vec::new();
            for (name, def) in hornvale_species::registry() {
                if let Some((v, rendered)) = rendered_pantheon_of(world, name, &def)? {
                    blocks.push(hornvale_almanac::PantheonBlock {
                        species: name.to_string(),
                        noun: def.noun.to_string(),
                        settlement: v.name.clone(),
                        cult_form: hornvale_religion::cult_form_held_by(world, v.id),
                        beliefs: rendered
                            .into_iter()
                            .map(|(belief, tenet)| hornvale_almanac::BeliefLine { belief, tenet })
                            .collect(),
                    });
                }
            }
            // Legacy fallback: pre-species saves have beliefs but no
            // peopled-by facts — render them as the single anonymous
            // pantheon they always were (see `legacy_rendered_beliefs`).
            if blocks.is_empty() {
                let rendered = legacy_rendered_beliefs(world)?;
                if !rendered.is_empty() {
                    blocks.push(hornvale_almanac::PantheonBlock {
                        species: String::new(),
                        noun: String::new(),
                        settlement: String::new(),
                        cult_form: hornvale_religion::cult_form_of(world),
                        beliefs: rendered
                            .into_iter()
                            .map(|(belief, tenet)| hornvale_almanac::BeliefLine { belief, tenet })
                            .collect(),
                    });
                }
            }
            blocks
        },
        calendar_lines: calendar_lines(world)?,
        night_sky: night_sky_line(world)?,
        genesis_notes: genesis_notes(world)?,
        settlement_lines: settlement_lines(world)?,
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
    fn the_default_vantage_resolves_to_the_flagship_and_is_deterministic() {
        let world = generated(42);
        let place = hornvale_terrain::places(&world).first().unwrap().id;
        assert!(
            place_coord(&world, place).is_some(),
            "the flagship must carry a coord"
        );
        let a = observed_phenomena(&world, 0.0).unwrap();
        let b = observed_phenomena(&world, 0.0).unwrap();
        assert_eq!(a, b, "the placed observation must be deterministic");
    }

    #[test]
    fn a_locked_worlds_pantheon_sees_exactly_one_hemisphere() {
        // The placed observer made observable: on a locked world the flagship
        // sees the sun XOR the night sky, never both (the pre-Plan-2 bug).
        let locked = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(hornvale_astronomy::RotationPin::Locked),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let ph = observed_phenomena(&locked, 0.0).unwrap();
        let sees_sun = ph.iter().any(|p| p.description.contains("sun"));
        let sees_night = ph.iter().any(|p| p.kind == hornvale_astronomy::NIGHT_STAR);
        assert!(
            sees_sun ^ sees_night,
            "a locked flagship sees exactly one hemisphere (sun XOR night sky)"
        );
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
        // The flagship's own pantheon (not the world total — a default world
        // now carries one pantheon per species-flagship, spec §5).
        assert_eq!(
            hornvale_religion::beliefs_held_by(&world, village.id).len(),
            1
        );
    }

    #[test]
    fn build_world_is_deterministic() {
        let a = constant(42).to_json();
        let b = constant(42).to_json();
        assert_eq!(a, b);
    }

    #[test]
    fn build_world_with_default_roster_matches_build_world_byte_for_byte() {
        use hornvale_terrain::TerrainPins;
        let sp = SettlementPins::default();
        for seed in [Seed(7), Seed(42), Seed(1000)] {
            let a = build_world(
                seed,
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &sp,
            )
            .unwrap();
            let b = build_world_with_roster(
                seed,
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &sp,
                &default_roster(),
            )
            .unwrap();
            let fa: Vec<_> = a.ledger.iter().collect();
            let fb: Vec<_> = b.ledger.iter().collect();
            assert_eq!(
                fa, fb,
                "seed {seed:?}: default-roster build must equal build_world exactly"
            );
        }
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
        assert!(!ctx.peoples.is_empty());
        assert!(!ctx.pantheons.is_empty());
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
            hornvale_culture::structure(&env, &hornvale_culture::PsychSummary::default())
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
                ..SettlementPins::default()
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
        let lines = culture_lines(&world, &village);
        assert!(!lines.is_empty());
        assert!(lines[0].contains(&village.name));
        assert!(lines[0].contains(&subsistence));
    }

    #[test]
    fn the_pantheon_reorganizes_between_spinning_and_locked() {
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
        let head_sentiment = |w: &World| {
            hornvale_religion::beliefs_of(w)
                .first()
                .map(|b| b.sentiment)
        };
        // The head deity's periodicity flips with the sun's rotation regime.
        assert_eq!(
            head_sentiment(&locked),
            Some(hornvale_religion::Sentiment::Eternal),
            "locked world: an eternal high god"
        );
        assert_ne!(
            head_sentiment(&spinning),
            Some(hornvale_religion::Sentiment::Eternal),
            "spinning world: not an eternal head deity"
        );
        assert_ne!(
            head_sentiment(&spinning),
            head_sentiment(&locked),
            "the two skies yield different religions"
        );
        // A pantheon, not a single belief.
        assert!(!hornvale_religion::beliefs_of(&spinning).is_empty());
    }

    #[test]
    fn the_flagship_pantheon_reflects_its_society() {
        let world = generated(42);
        let village = hornvale_settlement::village_info(&world).expect("village");
        // The flagship's own pantheon — a default world now carries one
        // pantheon per species-flagship (spec §5), so "at most one high
        // god" is a per-community invariant, not a world-wide one.
        let beliefs = hornvale_religion::beliefs_held_by(&world, village.id);
        assert!(!beliefs.is_empty(), "the flagship has a pantheon");
        // cult form is set and consistent.
        assert!(hornvale_religion::cult_form_held_by(&world, village.id).is_some());
        // At most one high god.
        assert!(beliefs.iter().filter(|b| b.high_god).count() <= 1);
    }

    #[test]
    fn goblin_lens_is_exactly_identity() {
        let reg = hornvale_species::registry();
        assert!(perception_lens(&reg["goblin"].perception).is_identity());
    }

    #[test]
    fn kobold_lens_matches_the_spec_derivation() {
        let reg = hornvale_species::registry();
        let lens = perception_lens(&reg["kobold"].perception);
        assert!((lens.day_sky - 0.52).abs() < 1e-12);
        assert!((lens.night_sky - 1.82).abs() < 1e-12);
        assert!((lens.ambient - 0.70).abs() < 1e-12);
    }

    #[test]
    fn goblin_observation_reproduces_the_unlensed_path_bytewise() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        assert_eq!(
            observed_phenomena_as(&world, "goblin").unwrap(),
            observed_phenomena(&world, 0.0).unwrap(),
        );
    }

    #[test]
    fn a_nocturnal_observer_on_a_spinning_world_sees_night_stars() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let seen = observed_phenomena_as(&world, "kobold").unwrap();
        assert!(
            seen.iter().any(|p| p.kind == "night-star"),
            "the characteristic hour must land in the dark"
        );
        assert_eq!(
            seen[0].venue,
            hornvale_kernel::Venue::NightSky,
            "the kobold ranking is night-headed"
        );
    }

    #[test]
    fn observation_time_is_zero_for_constant_and_locked_skies() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let t = observation_time(&world, hornvale_species::ActivityCycle::Nocturnal).unwrap();
        assert_eq!(t, 0.0);
    }

    #[test]
    fn goblin_voice_params_are_the_baseline() {
        let reg = hornvale_species::registry();
        let v = voice_params(&reg["goblin"].psych);
        assert!((v.formality - 0.5).abs() < 1e-12 && (v.epithet_density - 0.5).abs() < 1e-12);
    }

    #[test]
    fn seed_42_names_are_non_english_and_de_facto_unique() {
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let names: Vec<String> = hornvale_settlement::all_settlements(&world)
            .iter()
            .map(|v| v.name.clone())
            .collect();
        assert!(!names.is_empty());
        // No settlement name is a legacy syllable-pool word (the stopgap
        // pools are deleted; names now come from the language engine).
        assert!(
            names
                .iter()
                .all(|n| !["Zag", "Gru", "Bol"].iter().any(|s| n.starts_with(s)))
        );
        // Names are pure per-cell draws, so world-wide uniqueness is NOT
        // guaranteed — it is de-facto, arising from the vast phonology name
        // space, and is measured as a collision-rate calibration in Task 12.
        // Seed 42 happens to be collision-free; we assert that empirically
        // here to catch a gross regression (e.g. every name collapsing to
        // one string), NOT as a guaranteed invariant.
        let set: std::collections::BTreeSet<_> = names.iter().collect();
        assert_eq!(
            set.len(),
            names.len(),
            "seed 42 is de-facto collision-free (empirical, not guaranteed)"
        );
    }
}
