//! Hornvale composition root — the only place all domains meet.
//!
//! Wires registrations and geneses in cascade order, and gathers the
//! almanac's context. Domains stay ignorant of each other; this module is
//! where the application composes them (Constitution §2.6).
#![warn(missing_docs)]

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::{
    CELESTIAL_BODY, ConstantSun, GeneratedSky, GenesisError, NIGHT_STAR, SEASONAL_CYCLE, SkyPins,
    SkyReport, facts, generate, parse_pin, pin_strings,
};
use hornvale_climate::{
    AMBIENT, ClimateInputs, ClimateReport, GeneratedClimate, RotationRegime, SeafloorFeature,
    UniformClimate,
};
use hornvale_kernel::{
    ConceptRegistry, EntityId, Fact, GeoCoord, Geosphere, LedgerError, ObserverContext,
    PerceptionLens, PhenomenaSource, Phenomenon, ReferenceElevation, RegistryError, Seed,
    Temperature, Value, World, WorldTime, math, observe,
};
use hornvale_paleoclimate::{EraClimate, PaleoRecord, caloric_summer_index, integrate_ice};
use hornvale_terrain::{GLOBE_LEVEL, GeneratedTerrain, TerrainPins};
use std::cell::RefCell;
use std::sync::OnceLock;
// The profiler measures wall-clock stage durations for a committed diagnostic
// (`profile_build` example); it never reads `WorldTime` and never touches a
// fact, so it is exempt from the wall-clock ban (clippy.toml / decision 0001).
#[allow(clippy::disallowed_types)]
use std::time::{Duration, Instant};

thread_local! {
    // `Some` only inside a `profiled(..)` scope; `None` on the normal build
    // path, so the stage spans compile to a single thread-local read + branch.
    static PROFILE: RefCell<Option<Vec<(&'static str, Duration)>>> = const { RefCell::new(None) };
}

/// Per-stage wall-clock spans recorded during one profiled world build.
/// type-audit: bare-ok(prose: stages)
#[derive(Clone, Debug, Default)]
pub struct BuildProfile {
    /// `(stage label, elapsed)` in pipeline order.
    pub stages: Vec<(&'static str, Duration)>,
}

/// Run `f` with stage profiling active and return its result plus the profile.
/// Nesting is not supported (the inner scope's spans replace the outer's); the
/// lab/CLI call it at the top of one build.
pub fn profiled<T>(f: impl FnOnce() -> T) -> (T, BuildProfile) {
    PROFILE.with(|p| *p.borrow_mut() = Some(Vec::new()));
    let out = f();
    let stages = PROFILE.with(|p| p.borrow_mut().take().unwrap_or_default());
    (out, BuildProfile { stages })
}

/// Record `label` with the time `f` took, but only when a `profiled` scope is
/// active. Off the profiled path this is one thread-local read and a call.
#[allow(clippy::disallowed_types, dead_code)]
fn stage<T>(label: &'static str, f: impl FnOnce() -> T) -> T {
    let active = PROFILE.with(|p| p.borrow().is_some());
    if !active {
        return f();
    }
    let start = Instant::now();
    let out = f();
    let elapsed = start.elapsed();
    PROFILE.with(|p| {
        if let Some(v) = p.borrow_mut().as_mut() {
            v.push((label, elapsed));
        }
    });
    out
}

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

/// How deep to build the world's fact-committing pipeline (spec §4 / MAP-25).
/// Earlier rungs are a byte-identical prefix of later ones — the pipeline is
/// linear, so each rung reads only earlier rungs' facts and stopping early
/// simply omits later appends. Climate is *not* a rung: it commits no facts,
/// and is reconstructed on demand from a Terrain-depth world.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuildDepth {
    /// Sky genesis only.
    Astronomy,
    /// …plus terrain genesis.
    Terrain,
    /// …plus settlement placement, naming, and glosses.
    Settlements,
    /// …plus culture, religion, species, and deep time (today's full build).
    Full,
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

/// Predicate: the glossed meaning of an entity's generated name (functional
/// Text) — the composition root's own predicate (Task 9 of The Words),
/// since it names a fact about the *pairing* of a generated name with the
/// site facts it compounds over, which no single domain crate owns.
/// type-audit: bare-ok(identifier-text)
pub const NAME_GLOSS: &str = "name-gloss";

/// Register every domain's concepts, plus the composition root's own.
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    hornvale_astronomy::register_concepts(registry)?;
    hornvale_climate::register_concepts(registry)?;
    hornvale_terrain::register_concepts(registry)?;
    hornvale_settlement::register_concepts(registry)?;
    hornvale_species::register_concepts(registry)?;
    hornvale_culture::register_concepts(registry)?;
    hornvale_religion::register_concepts(registry)?;
    hornvale_language::register_concepts(registry)?;
    registry.register_predicate(
        NAME_GLOSS,
        true,
        "the glossed meaning of an entity's generated name",
    )?;
    hornvale_paleoclimate::register_concepts(registry)
}

/// Geospheres by canonical level: seed-independent, computed once per
/// process per level and cloned into providers (spec §3 posture kept;
/// Crust spec §5 makes the level world-chosen). BTreeMap per the
/// disallowed-types rule; Mutex because OnceLock alone can't grow.
fn geosphere_for(level: u32) -> Geosphere {
    use std::collections::BTreeMap;
    use std::sync::Mutex;
    static CACHE: OnceLock<Mutex<BTreeMap<u32, Geosphere>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(BTreeMap::new()));
    let mut map = cache.lock().expect("geosphere cache poisoned");
    map.entry(level)
        .or_insert_with(|| Geosphere::new(level))
        .clone()
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

/// A `name-gloss` fact for `subject` — the composition root's own
/// predicate (see [`NAME_GLOSS`]), so its provenance is `"worldgen"`
/// rather than any one domain's tag.
fn name_gloss_fact(subject: EntityId, gloss: &str) -> Fact {
    Fact {
        subject,
        predicate: NAME_GLOSS.to_string(),
        object: Value::Text(gloss.to_string()),
        place: None,
        day: Some(0.0),
        provenance: "worldgen".to_string(),
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
    let level = pins.globe_level.unwrap_or(GLOBE_LEVEL);
    let geo = geosphere_for(level);
    let outcome =
        hornvale_terrain::generate(world.seed, &geo, &pins).map_err(BuildError::TerrainGenesis)?;
    Ok(GeneratedTerrain::new(geo, outcome))
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
                hornvale_astronomy::Rotation::Spinning { day, .. } => {
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

/// The deep-time window (1 Myr) and sampling, standard days. These, the era
/// count, and the ice step order are save-format contracts (metaplan §9).
const DEEP_TIME_WINDOW_DAYS: f64 = 1_000_000.0 * 365.25;
/// Fine ice-integration step, standard days.
const ICE_STEP_DAYS: f64 = 2_000.0 * 365.25;
/// Number of coarse climate re-runs across the deep-time window.
const CLIMATE_ERAS: usize = 25;

/// Absolute freezing threshold for the deep-time snowline diagnostic
/// (`hornvale_paleoclimate::glaciated`), °C. A cell is under ice once its
/// temperature drops below this; the present temperature field varies by
/// latitude (~+45 °C equator to ~−15 °C pole — see
/// `domains/climate/src/temperature.rs`), so this absolute threshold —
/// unlike a global anomaly cutoff — lets the same albedo-cooling offset
/// move a latitudinal snowline instead of flipping the whole globe icebound
/// or bare at once. Deliberately set colder than
/// `hornvale_climate::habitability::HABITABLE_MIN_C` (−5 °C, not directly
/// importable across the domain boundary, but a known constant): equal
/// thresholds would partition every land cell into exactly "iced" or
/// "habitable" with no remainder, leaving no land cold-but-bare enough to
/// read as pure fossil shoreline once the ice and refugia masks are laid on
/// top (`render::paleo_ascii`'s priority order is envelope > refugium >
/// shoreline). Calibrated together with `hornvale_paleoclimate::
/// ALBEDO_GAIN_C` (a throwaway probe over generated-sky seeds 1..=12,
/// tuning both constants until the glacial maximum's median
/// `max_ice_fraction` landed at ~0.25, and until seed 42 specifically left a
/// nonzero cold-but-uninhabited gap) so a typical world's glacial maximum
/// advances ice over roughly a quarter of its land — a realistic Last
/// Glacial Maximum extent.
const FREEZE_C: f64 = -10.0;

/// The era-loop invariants: everything about the world that does not vary
/// across the ~25 coarse climate re-runs `paleoclimate_of` performs. Built
/// exactly once by `paleoclimate_of` and threaded through `climate_at_era`
/// for every era, so the loop no longer pays for a full terrain + sky
/// regeneration on each of its ~25 iterations.
struct EraContext<'a> {
    /// The shared geosphere (terrain's, reused for climate's grid).
    geo: &'a Geosphere,
    /// Present relief; elevation does not change across eras.
    elevation: &'a hornvale_kernel::CellMap<ReferenceElevation>,
    /// Seafloor feature per cell, derived once from terrain's boundaries.
    seafloor: &'a hornvale_kernel::CellMap<SeafloorFeature>,
    /// Insolation relative to Earth, from the world's sky (constant per world).
    insolation: f64,
    /// The world's rotation regime (constant per world).
    regime: RotationRegime,
    /// The calendar year length, standard days (constant per world).
    year_length_std: f64,
    /// The world's own present-day ice mask (diagnosed from
    /// `paleoclimate_of`'s present-temperature field against [`FREEZE_C`],
    /// no albedo offset) — the baseline every era's advance is measured
    /// against.
    present_ice: &'a hornvale_kernel::CellMap<bool>,
    /// The absolute snowline threshold ([`FREEZE_C`], wrapped once).
    freeze: Temperature,
}

/// This era's raw inputs, carried alongside its cheaply-diagnosed
/// [`EraClimate`] so the one era that turns out to be the glacial maximum
/// can be re-visited afterward for a full climate rebuild (see
/// `glacial_maximum_habitable`).
struct EraInputs {
    /// Absolute standard day of the era.
    day: f64,
    /// This era's sea level: present + eustatic change.
    sea_level: ReferenceElevation,
    /// This era's obliquity, degrees, from the sky's forcing.
    obliquity_deg: f64,
    /// This era's albedo-cooling offset from the integrated ice history.
    temp_offset: hornvale_paleoclimate::TempAnomaly,
}

/// Diagnose one past era's ice-ADVANCE mask without paying for a full
/// climate rebuild: `hornvale_climate::mean_temperature`'s field is a
/// latitude/insolation baseline (era-invariant here — obliquity and
/// insolation do not vary by era) minus a lapse term keyed to `sea_level`,
/// so an era's mean temperature differs from another only through that
/// term. Calling `hornvale_climate::temperature::mean_temperature` directly
/// — the exact function `GeneratedClimate::generate` calls internally to
/// build its own mean-temperature field — reproduces that field
/// bit-for-bit while skipping the moisture/biome/habitability work
/// `GeneratedClimate::generate` also does, none of which the ice mask
/// needs (a review finding: the old per-era loop paid for a full climate
/// rebuild — moisture field, biome classification, habitability map,
/// ~25 times over — purely to read its mean-temperature field back out).
/// `ctx` carries everything that does not vary by era (see [`EraContext`]).
/// The returned `EraClimate.habitable` is a placeholder (`strata::extract`
/// only ever reads the glacial-maximum era's, filled in afterward by
/// `glacial_maximum_habitable` — see `paleoclimate_of`).
fn climate_at_era(ctx: &EraContext, inputs: &EraInputs) -> EraClimate {
    let geo = ctx.geo;
    let elevation = ctx.elevation;
    let sea_level = inputs.sea_level;
    let mean_temp = hornvale_climate::temperature::mean_temperature(
        geo,
        elevation,
        sea_level,
        ctx.insolation,
        &ctx.regime,
    );
    // This era's absolute temperature: THIS era's own mean field (built with
    // this era's sea level, above — captures the lapse term) plus this
    // era's albedo-cooling offset, via `Temperature`'s `Add` impl (the sole
    // production path for combining the two, together with `Sub` —
    // decision 0008).
    let era_temperature =
        hornvale_kernel::CellMap::from_fn(geo, |c| *mean_temp.get(c) + inputs.temp_offset);
    // Ice is diagnosed against an ABSOLUTE snowline (`ctx.freeze`), not an
    // anomaly, so the same global cooling offset produces a spatially
    // structured mask (high latitudes ice first) rather than an all-or-
    // nothing flip. What strata preserve is the ADVANCE beyond what is
    // already iced at present — `era_ice` minus `ctx.present_ice` — not the
    // raw mask: this is what keeps the zero-forcing null control exact
    // regardless of how cold a world's present poles already run. Under zero
    // forcing the ice integrator never leaves its dead band, so volume stays
    // 0 every era: the offset is 0 AND sea level is unchanged, so this
    // era's mean field (built above from `sea_level`) is byte-identical to
    // the present's ⇒ `era_temperature` equals the present reading
    // pointwise ⇒ `era_ice` equals `ctx.present_ice` pointwise ⇒ zero
    // advance everywhere.
    let era_ice =
        hornvale_paleoclimate::glaciated(geo, elevation, &era_temperature, ctx.freeze, sea_level);
    let advance =
        hornvale_kernel::CellMap::from_fn(geo, |c| *era_ice.get(c) && !*ctx.present_ice.get(c));
    // This same `advance` mask is both summarized into `ice_fraction` below
    // and stored on the returned `EraClimate` unchanged, so
    // `strata::extract`'s envelope (an OR-union of every era's `ice` field)
    // can never disagree with `ice_fraction` about which cells this era
    // advanced — one mask, two consumers.
    let land = geo
        .cells()
        .filter(|c| *elevation.get(*c) >= sea_level)
        .count();
    let advanced = geo.cells().filter(|c| *advance.get(*c)).count();
    let ice_fraction = if land == 0 {
        0.0
    } else {
        advanced as f64 / land as f64
    };
    EraClimate {
        day: inputs.day,
        ice: advance,
        // Placeholder — see the doc comment above. Filled in for the
        // glacial-maximum era only, by `glacial_maximum_habitable`.
        habitable: hornvale_kernel::CellMap::from_fn(geo, |_| false),
        sea_level,
        ice_fraction,
    }
}

/// The one full climate rebuild the era loop still pays for: refugia
/// (habitability through the glacial maximum) needs moisture, which the
/// cheap per-era diagnostic above deliberately does not compute. Reproduces
/// the pre-refactor per-era habitable computation byte-for-byte for the
/// single era this is called on. `ctx` carries the era-invariant inputs
/// (see [`EraContext`]); `inputs` is this one era's own sea level,
/// obliquity, and albedo offset (see [`EraInputs`]).
fn glacial_maximum_habitable(
    ctx: &EraContext,
    inputs: &EraInputs,
) -> hornvale_kernel::CellMap<bool> {
    let geo = ctx.geo;
    let elevation = ctx.elevation;
    let sea_level = inputs.sea_level;
    let climate = GeneratedClimate::generate(&ClimateInputs {
        geosphere: geo,
        elevation,
        sea_level,
        seafloor: ctx.seafloor,
        insolation: ctx.insolation,
        obliquity_deg: inputs.obliquity_deg,
        regime: ctx.regime,
        year_length_std: ctx.year_length_std,
    });
    let era_temperature = hornvale_kernel::CellMap::from_fn(geo, |c| {
        climate.mean_temperature_at(c) + inputs.temp_offset
    });
    hornvale_kernel::CellMap::from_fn(geo, |c| {
        hornvale_climate::is_habitable(
            *era_temperature.get(c),
            climate.moisture_at(c),
            *elevation.get(c),
            sea_level,
        )
    })
}

/// The deep-time era loop: march the ice sheet on the sky's forcing at fine
/// steps, re-run climate at ~25 coarse eras, and extract the strata. The
/// single construction site for `PaleoRecord` and the sole definer of the
/// era-tick order (a save-format contract).
pub fn paleoclimate_of(world: &World) -> Result<PaleoRecord, BuildError> {
    // Build the era-loop invariants exactly once (terrain, sky, and every
    // scalar/field derived from them) — see `EraContext`.
    let sky = sky_of(world)?;
    let terrain = terrain_of(world)?;
    let geo = terrain.geosphere();
    let elevation = terrain.globe().elevation.clone();
    let present_sea_level = terrain.sea_level();

    // No forcing to read (constant sky) → no deep time; empty record.
    let Some(system) = sky.system() else {
        return Ok(hornvale_paleoclimate::extract(
            geo,
            &elevation,
            present_sea_level,
            &[],
        ));
    };
    let forcing = &system.forcing;

    let seafloor =
        hornvale_kernel::CellMap::from_fn(geo, |cell| seafloor_feature(terrain.boundary_at(cell)));
    // `mean_temperature` (used just below, for the present, and inside
    // `climate_at_era` for every other era) does not read obliquity at all
    // — only the seasonal-swing/moisture terms do, and neither the ice
    // diagnostic nor the present-temperature baseline needs those — so the
    // world's mean obliquity is unused here; `glacial_maximum_habitable`
    // reads each era's own obliquity from its `EraInputs` instead.
    let (insolation, _obliquity_deg, regime, year_length_std) = stellar_inputs(&sky);

    // The world's own unforced present temperature (era_day = 0, no albedo
    // offset), one per cell, absolute — the field every era's offset is
    // added to, and (via `present_ice` below) the baseline every era's
    // advance is measured against (see `climate_at_era`). Read directly off
    // `hornvale_climate::temperature::mean_temperature` — the same function
    // a full `GeneratedClimate::generate` call would use internally to
    // build its own mean-temperature field — rather than paying for a full
    // climate rebuild (moisture, biome, habitability) purely to read that
    // field back out: `obliquity_at(0.0) == obliquity_mean` exactly
    // (astronomy's forcing contract), the same value `stellar_inputs`
    // returns, so this reproduces `climate_of`'s present reading
    // byte-for-byte with no full regeneration at all. Already a
    // `CellMap<Temperature>`, so no wrapping is needed before `glaciated`.
    let present_temperature = hornvale_climate::temperature::mean_temperature(
        geo,
        &elevation,
        present_sea_level,
        insolation,
        &regime,
    );
    let freeze = Temperature::new(FREEZE_C).expect("FREEZE_C is finite");
    // The world's own present-day ice mask — no albedo offset, so this is
    // exactly the ice a present-day observer would already see. Every era's
    // `ice` field is the ADVANCE beyond this mask, not the raw diagnostic
    // (see `climate_at_era`).
    let present_ice = hornvale_paleoclimate::glaciated(
        geo,
        &elevation,
        &present_temperature,
        freeze,
        present_sea_level,
    );

    let ctx = EraContext {
        geo,
        elevation: &elevation,
        seafloor: &seafloor,
        insolation,
        regime,
        year_length_std,
        present_ice: &present_ice,
        freeze,
    };

    // Fine ice integration: sample the caloric index back through the window.
    // t = 0 is the present (newest); we look back to −WINDOW. Samples ascend
    // in absolute day so integration runs oldest → present.
    let n_steps = (DEEP_TIME_WINDOW_DAYS / ICE_STEP_DAYS).round() as usize;
    let mut samples: Vec<(f64, f64)> = Vec::with_capacity(n_steps + 1);
    for k in (0..=n_steps).rev() {
        let t = -(k as f64) * ICE_STEP_DAYS; // oldest (most negative) first
        let g = caloric_summer_index(
            forcing.obliquity_at(t),
            forcing.obliquity_mean,
            forcing.eccentricity_at(t),
            forcing.precession_at(t),
        );
        samples.push((t, g));
    }
    let history = integrate_ice(&samples);

    // Coarse climate eras: CLIMATE_ERAS days evenly across the window, each
    // reading the nearest integrated ice state for its offset and sea level.
    // The cheap ice-only diagnostic (`climate_at_era`) needs no full climate
    // rebuild, so every era's raw inputs are kept alongside its result —
    // once the glacial maximum is known (below), that ONE era gets a full
    // rebuild to fill in its habitable field for refugia.
    let mut era_inputs: Vec<EraInputs> = Vec::with_capacity(CLIMATE_ERAS);
    for e in 0..CLIMATE_ERAS {
        let era_day = -DEEP_TIME_WINDOW_DAYS
            + (e as f64) * DEEP_TIME_WINDOW_DAYS / (CLIMATE_ERAS as f64 - 1.0);
        // Nearest ice state by day (samples ascend).
        let state = history
            .iter()
            .min_by(|a, b| (a.day - era_day).abs().total_cmp(&(b.day - era_day).abs()))
            .expect("history is non-empty");
        era_inputs.push(EraInputs {
            day: era_day,
            sea_level: ReferenceElevation::new(
                present_sea_level.get() + state.sea_level_change.get(),
            )
            .expect("present sea level plus a finite eustatic change is finite"),
            obliquity_deg: forcing.obliquity_at(era_day),
            temp_offset: state.temp_offset,
        });
    }
    let mut eras: Vec<EraClimate> = era_inputs
        .iter()
        .map(|inputs| climate_at_era(&ctx, inputs))
        .collect();

    // The glacial maximum: the SAME peak-selection comparator
    // `strata::extract` uses below (greatest ice fraction, ties → earliest
    // day), so the era refined here is exactly the one `extract` will read
    // `habitable` from for refugia. Every other era's placeholder habitable
    // field is never read.
    if let Some(peak_idx) = (0..eras.len()).max_by(|&i, &j| {
        eras[i]
            .ice_fraction
            .total_cmp(&eras[j].ice_fraction)
            .then(eras[j].day.total_cmp(&eras[i].day))
    }) {
        eras[peak_idx].habitable = glacial_maximum_habitable(&ctx, &era_inputs[peak_idx]);
    }

    Ok(hornvale_paleoclimate::extract(
        geo,
        &elevation,
        present_sea_level,
        &eras,
    ))
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

/// The deep-time headline lines for the almanac; empty when the world has no
/// glacial past.
/// type-audit: bare-ok(prose: return)
pub fn deep_time_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let record = paleoclimate_of(world)?;
    if record.max_ice_fraction <= 0.0 {
        return Ok(Vec::new());
    }
    Ok(vec![format!(
        "The frost retreated: at the glacial maximum (day {:.0}), ice advanced over {:.0}% of the land.",
        record.glacial_maximum_day,
        record.max_ice_fraction * 100.0
    )])
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
    observed_phenomena_at(world, def, place)
}

/// [`observed_phenomena_as_in`]'s actual observation with the entity's
/// committed coordinates as the vantage: the entity's own hemisphere culls
/// the sky (SEQ-5). An entity with no committed latitude/longitude (e.g. a
/// bare stand-in id) observes the whole, un-culled sky.
fn observed_phenomena_at(
    world: &World,
    def: &hornvale_species::SpeciesDef,
    place: EntityId,
) -> Result<Vec<Phenomenon>, BuildError> {
    observed_phenomena_from(world, def, place, place_coord(world, place))
}

/// The phenomena a species (resolved within `roster`) observes from
/// `place`'s own committed vantage — its latitude/longitude fact culls the
/// sky by hemisphere (SEQ-5). This is the per-entity observation glossed
/// naming is truthful to (spec §9.3: a gloss composes THAT entity's own
/// site facts), public so the keystone (`cli/tests/words_identity.rs`) and
/// the lab's `name-gloss-true` metric can re-derive it independently
/// without importing worldgen's naming internals.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_at(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
    place: EntityId,
) -> Result<Vec<Phenomenon>, BuildError> {
    let def = def_in(roster, species)?;
    observed_phenomena_at(world, def, place)
}

/// The observation itself, factored out with an explicit `position` so
/// glossed settlement naming (Task 9) can observe from the settlement's own
/// cell coordinate BEFORE the settlement entity exists — names are drawn
/// ahead of `hornvale_settlement::genesis`'s (functional, one-shot) `name`
/// fact, when `hornvale_terrain::places` still finds nothing. The vantage's
/// hemisphere culls the sky (SEQ-5) exactly as it will for the committed
/// entity. No currently-registered `PhenomenaSource` (`Sky`,
/// `UniformClimate`) actually reads `ObserverContext::place` — only
/// `time`/`lens`/`position` shape the result (the place debt is SEQ-4's,
/// per the field's own history) — so a stand-in entity id carrying the
/// real coordinate is observationally identical to the committed place.
fn observed_phenomena_from(
    world: &World,
    def: &hornvale_species::SpeciesDef,
    place: EntityId,
    position: Option<GeoCoord>,
) -> Result<Vec<Phenomenon>, BuildError> {
    let day = observation_time(world, def.perception.activity)?;
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

/// The concept a phenomenon kind glosses to, for glossed naming (Task 9):
/// `celestial-body` disambiguates by its description text into whichever
/// body it actually is (astronomy's only two `celestial-body` producers —
/// `ConstantSun` and `GeneratedSky`'s sun/moon phenomena — describe
/// themselves with "sun"/"moon"; "star" is included for forward
/// compatibility even though no current producer emits it under this kind,
/// since neighbor stars are their own `night-star` kind instead);
/// `seasonal-cycle` glosses to `day` (the closest registered concept to
/// "the annual daylight cycle" — there is no dedicated `season` concept);
/// `night-star` glosses directly to `star`; climate's `ambient` glosses to
/// `wind` (the moving-air referent behind its one, always-present
/// phenomenon). Any other/future kind has no mapping yet (`None`) rather
/// than guessing. A composition-root judgment call, not a spec table —
/// adjustable here without touching the language engine.
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&'static str> {
    match phenomenon.kind.as_str() {
        CELESTIAL_BODY => {
            if phenomenon.description.contains("moon") {
                Some("moon")
            } else if phenomenon.description.contains("star") {
                Some("star")
            } else {
                Some("sun")
            }
        }
        SEASONAL_CYCLE => Some("day"),
        NIGHT_STAR => Some("star"),
        AMBIENT => Some("wind"),
        _ => None,
    }
}

/// The quality concept a belief's [`hornvale_religion::Sentiment`] glosses
/// to, for glossed deity/epithet naming (Task 9): `Eternal` (always
/// watched, never absent) reads as `light`; `Cyclic` (departs and returns)
/// reads as `shadow` (cast and receding, the pack's transient dark);
/// `Ambient` (felt through the world rather than watched) reads as `gloom`
/// (the pack's deepest, most pervasive dark) — deliberately distinct from
/// [`phenomenon_concept`]'s own `AMBIENT`-kind mapping (`wind`), so a
/// phenomenon that is itself `Ambient`-venued doesn't gloss to the same
/// word twice. A composition-root judgment call (no such table exists in
/// the registry or spec), chosen from the language engine's already-
/// registered quality vocabulary rather than adding new concepts for it.
fn sentiment_concept(sentiment: hornvale_religion::Sentiment) -> &'static str {
    match sentiment {
        hornvale_religion::Sentiment::Eternal => "light",
        hornvale_religion::Sentiment::Cyclic => "shadow",
        hornvale_religion::Sentiment::Ambient => "gloom",
    }
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
        tonality: art.tonality,
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

/// Draw a `family`'s proto phonology from this world's seed and the
/// family's authored proto ancestral vector
/// ([`hornvale_species::family_registry`]) — the family name occupies the
/// species slot in the seed-derivation (e.g. `draw_phonology(seed,
/// "goblinoid", env)`), a language with no speakers of its own, only
/// daughters. Panics if `family` is not in `family_registry` (a singleton
/// family has no entry there and never reaches this function — see
/// `lexicon_of`'s resolution).
/// type-audit: bare-ok(identifier-text: family)
pub fn proto_phonology_of(world: &World, family: &str) -> hornvale_language::Phonology {
    hornvale_language::draw_phonology(
        &world.seed,
        family,
        &envelope_of(&hornvale_species::family_registry()[family]),
    )
}

/// Map a species' perception vector onto the color pack's two acquisition
/// ladders (spec §7 model card, authored verbatim — implement exactly, do
/// not "improve"): `hue` runs 2 (dark/light only) through 5 (every hue
/// through brown) as night vision runs from owl-eyed to blind — a species
/// that sees well in the dark has spent less of its evolutionary history
/// straining at daylight hue distinctions. `luminance` is a coarse
/// two-step switch: a species with keen night vision (`night_vision >
/// 0.6`) has lexicalized the full gloom/shadow/starlit ladder (3); every
/// other species has only the coarsest term (1). At the goblin baseline
/// (`night_vision == 0.5`), `hue == 4` (blue lexicalized, brown not) and
/// `luminance == 1`; the kobold roster value (`night_vision == 0.9`) gives
/// `hue == 2` (blue *not* lexicalized — kobolds stop before blue) and
/// `luminance == 3`.
pub fn pack_depths(p: &hornvale_species::PerceptionVector) -> hornvale_language::PackDepths {
    let hue = 2 + ((1.0 - p.night_vision) * 3.0).round() as u8;
    let luminance = if p.night_vision > 0.6 { 3 } else { 1 };
    hornvale_language::PackDepths { hue, luminance }
}

/// The luminance-ladder concept ids within `color_pack` (mirrors the
/// private `LUMINANCE_CONCEPTS` list documented on
/// `hornvale_language::in_ladder`) — needed here only to word a Perceptual
/// gap's reason with the ladder it was actually excluded from; the
/// exclusion test itself always goes through `in_ladder`.
const LUMINANCE_CONCEPTS: &[&str] = &["gloom", "shadow", "starlit"];

/// Word a color-pack entry's Perceptual gap: which ladder excluded it, at
/// what rank, against what depth, from what night-vision value.
fn perceptual_reason(
    entry: &hornvale_language::PackEntry,
    depths: &hornvale_language::PackDepths,
    night_vision: f64,
) -> String {
    let (ladder, depth) = if LUMINANCE_CONCEPTS.contains(&entry.concept) {
        ("luminance", depths.luminance)
    } else {
        ("hue", depths.hue)
    };
    format!(
        "{ladder} rank {} exceeds depth {depth} from night-vision {night_vision}",
        entry.ladder_rank
    )
}

/// Whether `name` is one of climate's registered biome concepts (used to
/// word Unknown biome concepts with the brief's "no settlement in or
/// beside <biome>" phrasing rather than the generic fallback).
fn is_biome_concept(name: &str) -> bool {
    hornvale_climate::biome::ALL
        .iter()
        .any(|b| b.concept_name() == name)
}

/// Word an Unknown concept's Experiential gap: biome concepts (and `sea`,
/// which shares their geographic character though it is terrain's own
/// concept) read as a missing settlement; everything else reads as a
/// missing exposure for the species.
fn experiential_reason(species: &str, name: &str) -> String {
    if is_biome_concept(name) || name == "sea" {
        format!("no settlement in or beside {name}")
    } else {
        format!("{species} has no exposure to '{name}'")
    }
}

/// Every species placed in this world, in name order: the distinct
/// `peopled-by` objects across every committed settlement.
fn placed_species(world: &World) -> std::collections::BTreeSet<String> {
    world
        .ledger
        .find(hornvale_species::PEOPLED_BY)
        .filter_map(|f| match &f.object {
            Value::Text(s) => Some(s.clone()),
            _ => None,
        })
        .collect()
}

/// The Geosphere cells a species has settled: every committed settlement
/// `peopled-by` this species, read back by its `cell-id` fact.
fn settled_cells(world: &World, species: &str) -> Vec<hornvale_kernel::CellId> {
    world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .filter(|f| hornvale_species::species_of(world, f.subject).as_deref() == Some(species))
        .filter_map(|f| {
            match world
                .ledger
                .value_of(f.subject, hornvale_settlement::CELL_ID)
            {
                Some(Value::Number(n)) => Some(hornvale_kernel::CellId(*n as u32)),
                _ => None,
            }
        })
        .collect()
}

/// Whether some cell within `max_hops` of `start` (inclusive of `start`
/// itself) satisfies `pred` — the "lies within N cells" proximity test
/// `exposure_of` uses for `sea`. A plain breadth-first walk over the
/// Geosphere's adjacency; `max_hops` is small (2) so this is cheap per
/// settled cell.
fn within_hops(
    geo: &Geosphere,
    start: hornvale_kernel::CellId,
    max_hops: u32,
    pred: impl Fn(hornvale_kernel::CellId) -> bool,
) -> bool {
    use std::collections::BTreeSet;
    if pred(start) {
        return true;
    }
    let mut visited: BTreeSet<hornvale_kernel::CellId> = BTreeSet::new();
    visited.insert(start);
    let mut frontier = vec![start];
    for _ in 0..max_hops {
        let mut next = Vec::new();
        for cell in &frontier {
            for &n in geo.neighbors(*cell) {
                if visited.insert(n) {
                    if pred(n) {
                        return true;
                    }
                    next.push(n);
                }
            }
        }
        frontier = next;
    }
    false
}

/// Classify every concept in the world's registry for `species`'s culture
/// (spec §7): `Steeped` concepts get their own root word, `KnowsOf`
/// concepts are named as compounds, and `Unknown` concepts get a
/// recountable reason. Exactly one class per registered concept — the
/// map's keys are always exactly `world.registry.concepts()`'s names.
///
/// - **Steeped**: the universal stratum (always — water, fire, sun, one's
///   own name, and so on, `hornvale_language::universal_stratum`); every
///   body-pack and kin-pack entry (always in ladder — unranked); every
///   color-pack entry within the species' `pack_depths`; the biome of
///   every cell the species has settled; the species' own living-kind
///   concept (`"<species>-kind"` — a people always knows itself); and, once
///   the species has settled anywhere, the living kind of every species
///   placed in this world (coexistence in one shared world is exposure —
///   spec §3's free endonym/exonym) plus its own domestic and religious
///   social concepts (`home`, `hearth`, `god`, `spirit`).
/// - **KnowsOf**: the biome of every cell adjacent to a settled cell (that
///   isn't already `Steeped` from the species' own settlements); and
///   `sea`, if any settled cell lies within two cells of a below-sea-level
///   (ocean) cell.
/// - **Unknown**: every other registered concept — most visibly a
///   color-pack entry excluded by ladder depth (`GapReason::Perceptual`)
///   and a biome/`sea` concept the species neither settled nor neighbors
///   (`GapReason::Experiential`, "no settlement in or beside `<biome>`");
///   every remaining leftover concept (an unplaced species' living-kind,
///   or a social/geographic concept the species hasn't settled to reach)
///   gets a generic but still recountable `GapReason::Experiential`.
///
/// type-audit: bare-ok(identifier-text)
pub fn exposure_of(
    world: &World,
    species: &str,
) -> Result<std::collections::BTreeMap<String, hornvale_language::ExposureClass>, BuildError> {
    let roster = default_roster();
    let def = def_in(&roster, species)?;
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    let settled = settled_cells(world, species);
    // `exposure_of_impl` alone owns the "coexisting counts only once the
    // querying species has settled" rule; the outer gate this replaced was
    // vestigial belt-and-suspenders from the merge reconciliation.
    let coexisting = placed_species(world);
    exposure_of_impl(world, def, &settled, &coexisting, &terrain, &climate)
}

/// [`exposure_of`]'s classification rules (spec §7), factored out so
/// glossed naming (Task 9) can classify a species' exposure from the
/// scatter *this build pass is about to place* rather than from committed
/// facts: glossed settlement/deity names are drawn before
/// `hornvale_settlement::genesis` commits anything (the settlement's own
/// `name` fact is functional — it can only be committed once, with its
/// final value — and before `peopled-by` facts exist at all (species
/// entities mint last, entity-id stability, spec §8 of Y2-1)), so the
/// ledger-backed `settled_cells`/`placed_species` this species' own
/// `exposure_of` normally reads would see nothing yet. `settled` and
/// `coexisting` carry exactly what those two ledger reads would have
/// produced, sourced one step earlier from the in-memory placement scatter
/// instead; every other rule is identical to `exposure_of`'s doc comment.
fn exposure_of_impl(
    world: &World,
    def: &hornvale_species::SpeciesDef,
    settled: &[hornvale_kernel::CellId],
    coexisting: &std::collections::BTreeSet<String>,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<std::collections::BTreeMap<String, hornvale_language::ExposureClass>, BuildError> {
    use hornvale_language::{
        ExposureClass, GapReason, body_pack, color_pack, in_ladder, kin_pack, universal_stratum,
    };

    let species = def.name;
    let depths = pack_depths(&def.perception);
    let geo = terrain.geosphere();

    let mut classes: std::collections::BTreeMap<String, ExposureClass> =
        std::collections::BTreeMap::new();

    // Steeped: the universal stratum, unconditionally.
    for entry in universal_stratum() {
        classes.insert(entry.concept.to_string(), ExposureClass::Steeped);
    }

    // Steeped/Unknown: the two ladder-gated packs plus the two unranked
    // (always-in) packs.
    for entry in color_pack().iter().chain(body_pack()).chain(kin_pack()) {
        let class = if in_ladder(entry, &depths) {
            ExposureClass::Steeped
        } else {
            ExposureClass::Unknown {
                reason: GapReason::Perceptual(perceptual_reason(
                    entry,
                    &depths,
                    def.perception.night_vision,
                )),
            }
        };
        classes.insert(entry.concept.to_string(), class);
    }

    // Steeped: the biome of every settled cell.
    for &cell in settled {
        let name = climate.biome_at(cell).concept_name().to_string();
        classes.insert(name, ExposureClass::Steeped);
    }

    // Steeped: the species' own living kind (a people always knows itself),
    // and — once this species has settled anywhere — the living kind of
    // every species placed in this world, plus its own domestic/religious
    // social concepts. Coexistence in one shared world is exposure (spec
    // §3: each language holds its own words for goblin-kind and kobold-kind,
    // so endonym and exonym fall out free): the peoples' settlements are
    // placed by one shared spacing pass; they know each other. Refining
    // this to contact-graded exposure (distance, trade routes) waits for a
    // contact ledger. Kinds of species NOT placed in this world are left
    // to the closing Unknown loop — the placed set is read from the
    // ledger's `peopled-by` facts, never hardcoded to the roster.
    let own_kind = format!("{species}-kind");
    if world.registry.concept(&own_kind).is_some() {
        classes.insert(own_kind, ExposureClass::Steeped);
    }
    if !settled.is_empty() {
        for placed in coexisting {
            let kind = format!("{placed}-kind");
            if world.registry.concept(&kind).is_some() {
                classes.insert(kind, ExposureClass::Steeped);
            }
        }
        for concept in ["home", "hearth", "god", "spirit"] {
            if world.registry.concept(concept).is_some() {
                classes.insert(concept.to_string(), ExposureClass::Steeped);
            }
        }
    }

    // KnowsOf: the biome of every cell adjacent to a settled cell, unless
    // it is already Steeped (the species' own settled biome wins).
    for &cell in settled {
        for &n in geo.neighbors(cell) {
            let name = climate.biome_at(n).concept_name().to_string();
            classes.entry(name).or_insert(ExposureClass::KnowsOf);
        }
    }

    // KnowsOf: sea, if any settled cell lies within two cells of ocean.
    if world.registry.concept("sea").is_some() {
        let near_sea = settled
            .iter()
            .any(|&cell| within_hops(geo, cell, 2, |c| terrain.is_ocean(c)));
        if near_sea {
            classes
                .entry("sea".to_string())
                .or_insert(ExposureClass::KnowsOf);
        }
    }

    // Unknown: every remaining registered concept.
    for concept in world.registry.concepts() {
        classes
            .entry(concept.name.clone())
            .or_insert_with(|| ExposureClass::Unknown {
                reason: GapReason::Experiential(experiential_reason(species, &concept.name)),
            });
    }

    Ok(classes)
}

/// Build a species' full lexicon in one call — the re-derivation path
/// surfaces use (nothing about a lexicon is persisted): draw its phonology
/// (`language_of`), classify every concept's exposure (`exposure_of`), and
/// assemble the two into a `Lexicon` (`hornvale_language::build_lexicon`).
/// type-audit: bare-ok(identifier-text: species)
pub fn lexicon_of(world: &World, species: &str) -> Result<hornvale_language::Lexicon, BuildError> {
    lexicon_of_in(world, &default_roster(), species)
}

/// The family's members (all `roster` species sharing `family`), each as a
/// [`hornvale_language::Daughter`] — its drawn cascade and its own phonology —
/// so the merger-aware proto assignment (epoch `root/v3`) can choose core roots
/// that survive every daughter's descent distinct. The rejection is
/// order-independent (a candidate is rejected iff it merges in ANY daughter),
/// so the roster's order does not affect the result; a singleton family yields
/// a one-element slice (itself). Public so the proto-goblinoid reference page
/// and the Lab's monophyly/outgroup metrics can reproduce the SAME merger-aware
/// assignment `build_lexicon` consumes.
/// type-audit: bare-ok(identifier-text: family)
pub fn family_daughters(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    family: &str,
) -> Vec<hornvale_language::Daughter> {
    roster
        .iter()
        .filter(|d| d.family == family)
        .map(|d| hornvale_language::Daughter {
            cascade: hornvale_language::draw_cascade(&world.seed, d.name),
            phonology: language_of_in(world, roster, d.name),
        })
        .collect()
}

/// Build `species`' lexicon within an explicit `roster` — the merger-aware
/// composition-root path. Assembles the family's daughters so the proto
/// assignment drives core homophony to zero across the whole family.
/// type-audit: bare-ok(identifier-text: species)
pub fn lexicon_of_in(
    world: &World,
    roster: &[hornvale_species::SpeciesDef],
    species: &str,
) -> Result<hornvale_language::Lexicon, BuildError> {
    let ph = language_of_in(world, roster, species);
    let exposures = exposure_of(world, species)?;
    let def = *def_in(roster, species)?;
    let family = def.family;
    // A family with more than one member has a proto ancestral vector in
    // `family_registry` and draws a real shared proto phonology; a
    // singleton family (e.g. kobold) is absent there, so it stays its own
    // family label and its own phonology stands in for its proto — the
    // pre-family draw, preserved exactly.
    let (fam_label, proto_ph) = match hornvale_species::family_registry().get(family) {
        Some(_) => (family, proto_phonology_of(world, family)),
        None => (def.name, ph.clone()),
    };
    let daughters = family_daughters(world, roster, family);
    Ok(hornvale_language::build_lexicon(
        &world.seed,
        def.name,
        fam_label,
        &ph,
        &proto_ph,
        &exposures,
        &daughters,
    ))
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

/// The 1-2 site concepts a belief's glossed deity/epithet name draws over:
/// the phenomenon it mythologizes (if [`phenomenon_concept`] maps its
/// kind), plus its felt [`sentiment_concept`] — always present, so a belief
/// never has zero candidate concepts even when its phenomenon kind has no
/// mapping yet. Public because the Lab's `epithet-honorific` metric
/// re-derives the honorific-free glossed epithet from the same site
/// concepts to detect the committed prefix structurally, and Task 10's
/// structural-invariant suite / Task 12's `name-gloss-true` metric re-derive
/// the same composition to check committed glosses row-by-row.
/// type-audit: bare-ok(identifier-text: return)
pub fn deity_site_concepts(
    phenomenon: &Phenomenon,
    sentiment: hornvale_religion::Sentiment,
) -> Vec<&'static str> {
    let mut concepts = Vec::with_capacity(2);
    if let Some(concept) = phenomenon_concept(phenomenon) {
        concepts.push(concept);
    }
    concepts.push(sentiment_concept(sentiment));
    concepts
}

/// Backs religion's `DeityNamer` trait with a species' language `Namer`, at
/// the `/v2` glossed epoch. Each deity name and epithet is a single
/// deterministic draw salted by the belief's own id — no shared "used" set,
/// no re-draw (names are pure functions of seed+species+kind+salt, spec
/// §8). Religion never learns this exists; it only ever sees the
/// `DeityNamer` trait (spec §6's ignorance discipline) — in particular it
/// never passes the phenomenon a belief mythologizes into `deity`/
/// `epithet`, so this struct tracks it independently: `phenomena` is
/// exactly the salience-descending slice worldgen also hands
/// `hornvale_religion::genesis`, whose own doc comment guarantees
/// `deity(salt)` then `epithet(salt, sentiment)` are called once per member
/// phenomenon, in that phenomenon's order — and a member is always
/// `phenomena`'s own prefix (`members == &phenomena[..take]`), so `index`
/// (advanced only by `deity`) always names the phenomenon backing the
/// belief currently being named.
struct LanguageDeityNamer<'a, 'b, 'c> {
    namer: &'a hornvale_language::Namer<'b>,
    morph: hornvale_language::MorphOptions,
    lexicon: &'c hornvale_language::Lexicon,
    phenomena: &'c [Phenomenon],
    /// How many phenomena `deity()` has consumed so far; `epithet()` reuses
    /// `phenomena[index - 1]` without advancing it further.
    index: usize,
    /// Every named belief's non-empty gloss, keyed by its minted entity id
    /// (`salt`). `hornvale_religion::genesis` only ever sees this namer as
    /// `&mut dyn DeityNamer` — never the ledger — so it can't commit
    /// `name-gloss` facts itself; the composition root reads this map back
    /// once `genesis` returns and commits them there instead.
    glosses: std::collections::BTreeMap<u64, String>,
}

impl hornvale_religion::DeityNamer for LanguageDeityNamer<'_, '_, '_> {
    fn deity(&mut self, salt: u64) -> (String, String) {
        let phenomenon = self
            .phenomena
            .get(self.index)
            .expect("religion calls deity() once per member phenomenon, in phenomena order");
        self.index += 1;
        let sentiment = hornvale_religion::Sentiment::of(phenomenon);
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts {
            concepts: &concepts,
        };
        let (g, gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Deity,
            salt,
            &self.morph,
            &site,
            self.lexicon,
        );
        if !gloss.is_empty() {
            self.glosses.insert(salt, gloss);
        }
        (g.roman, g.ipa)
    }

    fn epithet(&mut self, salt: u64, sentiment: hornvale_religion::Sentiment) -> (String, String) {
        // Sentiment fits the epithet at render time (Task 11's `render_line`
        // reads it from the belief's own committed `sentiment` fact); the
        // generated word itself is glossed like any other name, over the
        // same phenomenon `deity()` just named for this same belief — only
        // the deity's own gloss is committed as a `name-gloss` fact (see
        // `glosses`), so the epithet's gloss is computed and discarded.
        let phenomenon = self
            .phenomena
            .get(self.index - 1)
            .expect("deity() always runs before epithet() for the same belief");
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts {
            concepts: &concepts,
        };
        let (g, _gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Epithet,
            salt,
            &self.morph,
            &site,
            self.lexicon,
        );
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
    build_to(
        seed,
        pins,
        sky,
        terrain_pins,
        settlement_pins,
        roster,
        BuildDepth::Full,
    )
}

/// Build a world only as deep as `depth` (spec §4 / MAP-25). At any depth the
/// committed facts are a byte-identical prefix of the full build's — stopping
/// early only omits later appends; it never changes a fact it does commit.
#[allow(clippy::too_many_arguments)]
pub fn build_world_to(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
    roster: &[hornvale_species::SpeciesDef],
    depth: BuildDepth,
) -> Result<World, BuildError> {
    build_to(
        seed,
        pins,
        sky,
        terrain_pins,
        settlement_pins,
        roster,
        depth,
    )
}

/// The full pipeline, run only as deep as `depth`. `build_world_with_roster`
/// delegates with `BuildDepth::Full`; `build_world_to` forwards its argument.
/// The only depth-dependent behavior is early `return Ok(world)` between
/// stages — every statement's order and borrows are otherwise unchanged, so
/// the Full path is identical to the pre-depth pipeline.
#[allow(clippy::too_many_arguments)]
fn build_to(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
    roster: &[hornvale_species::SpeciesDef],
    depth: BuildDepth,
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

    stage("astronomy", || -> Result<(), BuildError> {
        if let SkyChoice::Generated = sky {
            let outcome = generate(seed, pins).map_err(BuildError::Genesis)?;
            facts::genesis(&mut world, world_entity, &outcome)?;
        }
        Ok(())
    })?;

    if depth == BuildDepth::Astronomy {
        return Ok(world);
    }

    stage("terrain", || -> Result<(), BuildError> {
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
        let level = terrain_pins.globe_level.unwrap_or(GLOBE_LEVEL);
        let terrain_outcome = hornvale_terrain::generate(seed, &geosphere_for(level), terrain_pins)
            .map_err(BuildError::TerrainGenesis)?;
        hornvale_terrain::facts::genesis(&mut world, world_entity, &terrain_outcome)?;
        Ok(())
    })?;

    if depth <= BuildDepth::Terrain {
        return Ok(world);
    }

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
    #[allow(clippy::type_complexity)]
    let (terrain, climate, ids, placed, placements, lexicons, species_set, phonologies) = stage(
        "climate+settlements",
        || -> Result<
            (
                GeneratedTerrain,
                GeneratedClimate,
                Vec<EntityId>,
                Vec<hornvale_settlement::PlacedSettlement>,
                Vec<(hornvale_settlement::Placement, u32)>,
                std::collections::BTreeMap<&str, hornvale_language::Lexicon>,
                Vec<&hornvale_species::SpeciesDef>,
                std::collections::BTreeMap<&str, hornvale_language::Phonology>,
            ),
            BuildError,
        > {
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
                temperature_c: climate.mean_temperature_at(cell).get(),
                hostility,
            }
        })
        .collect();
    let min_sep = math::cos(12.0_f64.to_radians());
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

    // Per-species lexicon, for glossed naming (Task 9) below — built from
    // THIS pass's in-memory placement scatter via `exposure_of_impl` rather
    // than the ledger-backed `exposure_of`: glossed settlement/deity names
    // are drawn before `hornvale_settlement::genesis` commits the
    // (functional, one-shot) `name` fact, and well before `peopled-by`
    // facts exist at all (species entities mint last — entity-id stability,
    // spec §8 of Y2-1), so `exposure_of`'s usual ledger reads would see no
    // settlements yet.
    let mut lexicons: std::collections::BTreeMap<&str, hornvale_language::Lexicon> =
        std::collections::BTreeMap::new();
    for (tag, def) in species_set.iter().enumerate() {
        let settled_now: Vec<hornvale_kernel::CellId> = placements
            .iter()
            .filter(|(_, t)| *t as usize == tag)
            .map(|(p, _)| p.cell)
            .collect();
        // `exposure_of_impl` alone owns the "coexisting counts only once the
        // querying species has settled" rule.
        let coexisting_now: std::collections::BTreeSet<String> = species_set
            .iter()
            .enumerate()
            .filter(|(i, _)| placements.iter().any(|(_, t)| *t as usize == *i))
            .map(|(_, d)| d.name.to_string())
            .collect();
        let exposures = exposure_of_impl(
            &world,
            def,
            &settled_now,
            &coexisting_now,
            &terrain,
            &climate,
        )?;
        let ph = phonologies
            .get(def.name)
            .expect("a phonology was built for every placed species");
        let family = def.family;
        // A family with more than one member has a proto ancestral vector
        // in `family_registry` and draws a real shared proto phonology; a
        // singleton family (e.g. kobold) is absent there, so it stays its
        // own family label and its own phonology stands in for its proto —
        // the pre-family draw, preserved exactly.
        let (fam_label, proto_ph) = match hornvale_species::family_registry().get(family) {
            Some(_) => (family, proto_phonology_of(&world, family)),
            None => (def.name, ph.clone()),
        };
        let daughters = family_daughters(&world, roster, family);
        lexicons.insert(
            def.name,
            hornvale_language::build_lexicon(
                &seed, def.name, fam_label, ph, &proto_ph, &exposures, &daughters,
            ),
        );
    }

    let mut placed: Vec<hornvale_settlement::PlacedSettlement> =
        Vec::with_capacity(placements.len());
    let mut glosses: Vec<String> = Vec::with_capacity(placements.len());
    for (p, tag) in &placements {
        let def = species_set[*tag as usize];
        let coord = geo.coord(p.cell);
        let salt = u64::from(p.cell.0);
        let namer = namers
            .get(def.name)
            .expect("a Namer was built for every placed species");
        let morph = morph_options(&def.psych);
        let lexicon = lexicons
            .get(def.name)
            .expect("a lexicon was built for every placed species");
        let biome_concept = climate.biome_at(p.cell).concept_name();
        // The presiding phenomenon is observed from THIS settlement's own
        // cell coordinate — its hemisphere culls the sky (SEQ-5), so the
        // committed gloss is truthful to the sky this settlement actually
        // lives under (spec §9.3), and per-settlement skies widen the
        // descriptor space. Still a pure function of the entity's own
        // (cell, facts): pin-isolated by construction (spec §8). The
        // settlement entity doesn't exist yet, so `world_entity` stands in
        // as the (unread) `place` id while the real coordinate does the
        // culling — see `observed_phenomena_from`.
        let seen = observed_phenomena_from(&world, def, world_entity, Some(coord))?;
        let presiding = seen.first().and_then(phenomenon_concept);
        let mut site_concepts: Vec<&str> = vec![biome_concept];
        site_concepts.extend(presiding);
        let site = hornvale_language::SiteConcepts {
            concepts: &site_concepts,
        };
        let (generated, gloss) = namer.glossed_name(
            hornvale_language::NameKind::Settlement,
            salt,
            &morph,
            &site,
            lexicon,
        );
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
        glosses.push(gloss);
    }
    let ids = hornvale_settlement::genesis(&mut world, &placed)?;
    for (id, gloss) in ids.iter().zip(glosses.iter()) {
        if !gloss.is_empty() {
            world
                .ledger
                .commit(name_gloss_fact(*id, gloss), &world.registry)?;
        }
    }
            Ok((
                terrain,
                climate,
                ids,
                placed,
                placements,
                lexicons,
                species_set,
                phonologies,
            ))
        },
    )?;

    if depth <= BuildDepth::Settlements {
        return Ok(world);
    }

    // `geo` borrows `terrain`, and `namers` borrows `phonologies` (see
    // `Namer<'a>`), so neither can be returned out of the closure above
    // alongside the values they borrow from — both are cheap, pure,
    // side-effect-free recomputations of what the closure already derived.
    let geo = terrain.geosphere();
    let namers: std::collections::BTreeMap<&str, hornvale_language::Namer> = phonologies
        .iter()
        .map(|(name, ph)| (*name, hornvale_language::Namer::new(&seed, name, ph)))
        .collect();

    stage("culture+religion+species", || -> Result<(), BuildError> {
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
            // Religion (and the deity glosses drawn inside it) observes from
            // the world's first place — the flagship vantage, its hemisphere
            // culling the sky (SEQ-4/SEQ-5) — exactly the observation
            // `religion::genesis` derives its beliefs from, so every deity
            // name-gloss is truthful to the phenomenon its belief was actually
            // derived from. Settlements exist by now, so the placed-observer
            // path is live.
            let seen = observed_phenomena_as_in(&world, roster, def.name)?;
            let namer = namers
                .get(def.name)
                .expect("a Namer was built for every placed species");
            let morph = morph_options(&def.psych);
            let lexicon = lexicons
                .get(def.name)
                .expect("a lexicon was built for every placed species");
            let mut deity_namer = LanguageDeityNamer {
                namer,
                morph,
                lexicon,
                phenomena: &seen,
                index: 0,
                glosses: std::collections::BTreeMap::new(),
            };
            hornvale_religion::genesis(&mut world, flagship, &seen, &society, &mut deity_namer)?;
            for (salt, gloss) in &deity_namer.glosses {
                world.ledger.commit(
                    name_gloss_fact(hornvale_kernel::EntityId(*salt), gloss),
                    &world.registry,
                )?;
            }
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
        Ok(())
    })?;

    stage("deep-time", || -> Result<(), BuildError> {
        // Deep time: extract the glacial strata and commit their summary facts on
        // the world entity, so `recount`/`why` can speak the world's past.
        let paleo = paleoclimate_of(&world)?;
        hornvale_paleoclimate::genesis(&mut world, world_entity, terrain.geosphere(), &paleo)?;
        Ok(())
    })?;

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

/// The pole star and heliacal-return lines under **The Sky** (night-sky
/// stage 1): a pole-star sentence when the genesis-epoch night sky finds
/// one within `POLE_STAR_MAX_SEPARATION_DEG` of either celestial pole, plus
/// up to three heliacal-return sentences for the brightest neighbors
/// (index order), at the same flagship vantage latitude `calendar_lines`
/// resolves its daylight-swing line at (the first known place's committed
/// latitude; reference latitude 35.0 — a mid-temperate default — until the
/// flagship vantage reaches the almanac, if no place resolves). `None` for
/// constant-sky worlds, which have no neighborhood to describe.
/// type-audit: bare-ok(prose: return)
pub fn night_sky_lines(
    world: &World,
) -> Result<Option<hornvale_almanac::NightSkyLines>, BuildError> {
    let sky = sky_of(world)?;
    let Sky::Generated(sky) = &sky else {
        return Ok(None);
    };
    let calendar = sky.calendar();
    let system = sky.system();
    let t = hornvale_astronomy::StdDays::new(0.0).unwrap();

    let view = hornvale_astronomy::night_sky_at(system, calendar, 0.0, t);
    let pole_star = view.pole_star.map(|ps| {
        let neighbor = &system.neighbors[ps.neighbor];
        let hemisphere = match ps.pole {
            hornvale_astronomy::Hemisphere::North => "north",
            hornvale_astronomy::Hemisphere::South => "south",
        };
        let backward = if view.wheels_backward {
            " backward, on this backward-spinning world"
        } else {
            ""
        };
        format!(
            "A {} star stands {:.1}° from the {} celestial pole; the sky wheels around it{}.",
            neighbor.color, ps.separation_deg, hemisphere, backward
        )
    });

    // Reference latitude until the flagship vantage reaches the almanac —
    // the same fallback-free resolution `calendar_lines`' daylight-swing
    // line uses, but with a temperate default instead of the equator when
    // no place has been placed yet.
    let latitude = hornvale_terrain::places(world)
        .first()
        .and_then(|p| place_coord(world, p.id))
        .map(|c| c.latitude)
        .unwrap_or(35.0);
    let year_length_days = calendar.year_length().get();
    let pairs = hornvale_astronomy::heliacal_events(system, calendar, latitude, t);
    let heliacal = pairs
        .iter()
        .take(3)
        .map(|p| {
            let neighbor = &system.neighbors[p.neighbor];
            format!(
                "The {} star returns before dawn at year-phase {:.2}, after {:.0} days of absence.",
                neighbor.color,
                p.rising_frac,
                p.absence_fraction() * year_length_days
            )
        })
        .collect();

    Ok(Some(hornvale_almanac::NightSkyLines {
        pole_star,
        heliacal,
    }))
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
        deep_time_lines: deep_time_lines(world)?,
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
        night_sky_lines: night_sky_lines(world)?,
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
    #[ignore = "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full"]
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
    fn paleoclimate_is_deterministic_and_records_a_glacial_history() {
        // A generated-sky world has time-varying forcing, so ice cycles.
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let a = paleoclimate_of(&world).unwrap();
        let b = paleoclimate_of(&world).unwrap();
        assert_eq!(a.glacial_maximum_day, b.glacial_maximum_day);
        assert_eq!(a.max_ice_fraction, b.max_ice_fraction);
        assert!(
            a.max_ice_fraction > 0.0,
            "a forced world glaciates at least once"
        );
        assert!(
            a.envelope.iter().any(|(_, &b)| b),
            "a forced world's ice-extent envelope is non-empty"
        );
    }

    #[test]
    fn zero_forcing_world_has_no_glacial_history() {
        // The forcing pin flattens the Milankovitch triad → flat caloric index →
        // no ice cycling. The null control. Checked across several seeds: the
        // caloric index is an anomaly against each world's own mean obliquity
        // (so it is identically zero under zero forcing regardless of which
        // obliquity that particular seed drew), so the ice integrator never
        // leaves its dead band and volume stays 0 every era — zero albedo
        // offset AND sea level parked at the present stand throughout.
        //
        // `climate_at_era`'s glaciation diagnostic is an ABSOLUTE snowline
        // (`era_temperature < FREEZE_C`), not an anomaly — see that
        // function. What strata preserve is the ADVANCE beyond what is
        // already iced at present (`era_ice && !present_ice`), so a
        // naturally cold present — e.g. a very-low-obliquity world with
        // permanently cold poles, which reads as `present_ice` on its own —
        // is excluded from the envelope/`ice_fraction` regardless; only a
        // colder-than-present era's ADDITIONAL ice counts. Under zero
        // forcing every era's climate is byte-identical to the present
        // (zero offset, unchanged sea level ⇒ `climate_at_era`'s per-era
        // climate reproduces `paleoclimate_of`'s present climate exactly),
        // so `era_ice` equals `present_ice` pointwise and `advance` is empty
        // in every era — the null control holds regardless of how cold any
        // given seed's present poles happen to run.
        //
        // Every assertion here must hold for the same reason: zero forcing
        // means zero ice volume every era, so eustatic sea level never moves
        // off the present stand and every era's advance mask is empty. That
        // empty `advance` is what `climate_at_era` stores on each `ice`
        // field, and `strata::extract`'s envelope is the OR-union of
        // exactly those masks — so if `max_ice_fraction` is 0, the envelope
        // is structurally forced to be empty too. Asserting only
        // `max_ice_fraction` (as this test used to) does not exercise that
        // link: before the `EraClimate.ice` field existed, the envelope was
        // built from the *absolute* offset temperature via `glaciated`, and
        // a world whose poles sit naturally below the threshold produced a
        // non-empty envelope even though `max_ice_fraction` read 0.0.
        //
        // The shoreline is checked against a baseline record, not against
        // zero: `derive_sea_level` (terrain/src/elevation.rs) places sea
        // level at the exact elevation of one grid cell (a quantile pick),
        // so that cell always sits inside `extract`'s inclusive
        // `[min_sea, max_sea]` band even when every era's sea level equals
        // the present stand — that is the ordinary present coastline, not a
        // symptom of migration, and would appear with or without any deep
        // time at all. The correct null-control invariant is that zero
        // forcing produces the *same* shoreline as no deep-time eras
        // whatsoever (sea level parked at the present stand throughout), not
        // a literally empty one.
        for seed in [42, 7, 123] {
            let pins = SkyPins {
                forcing: Some(hornvale_astronomy::ForcingPin::Zero),
                ..SkyPins::default()
            };
            let world = build_world(
                Seed(seed),
                &pins,
                SkyChoice::Generated,
                &TerrainPins::default(),
                &SettlementPins::default(),
            )
            .unwrap();
            let rec = paleoclimate_of(&world).unwrap();
            assert_eq!(
                rec.max_ice_fraction, 0.0,
                "zero forcing must not glaciate (seed {seed})"
            );
            assert_eq!(
                rec.envelope.iter().filter(|&(_, &b)| b).count(),
                0,
                "zero forcing must leave an empty ice-extent envelope (seed {seed})"
            );
            let terrain = terrain_of(&world).unwrap();
            let baseline = hornvale_paleoclimate::extract(
                terrain.geosphere(),
                &terrain.globe().elevation,
                terrain.sea_level(),
                &[],
            );
            assert_eq!(
                rec.shoreline, baseline.shoreline,
                "zero forcing must not widen the shoreline past the present coastline (seed {seed})"
            );
            // The null control must also be silent at the FACT layer: a world
            // that never glaciated commits no shoreline, refugium, or
            // frost-retreat descriptor (spec §9) — only the honest
            // max-ice-fraction = 0 summary. `build_world` already ran
            // paleoclimate genesis, so check its committed ledger directly:
            // `recount`/`why` must not surface a glaciation that never happened.
            for pred in [
                hornvale_paleoclimate::facts::FOSSIL_SHORELINE,
                hornvale_paleoclimate::facts::REFUGIUM,
                hornvale_paleoclimate::facts::FROST_RETREAT,
            ] {
                assert_eq!(
                    world.ledger.find(pred).count(),
                    0,
                    "zero forcing must commit no '{pred}' strata fact (seed {seed})"
                );
            }
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
        assert!(night_sky_lines(&world).unwrap().is_none());
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

    /// Seed 9's default (unpinned) generation carries a north pole star at
    /// genesis (verified against the astronomy sweep in `facts.rs`); this
    /// pins the almanac-side wiring to a real seed rather than a
    /// hand-built fixture.
    #[test]
    fn seed_9_generated_default_has_a_pole_star_line() {
        let world = generated(9);
        let lines = night_sky_lines(&world).unwrap().unwrap();
        let pole_star = lines.pole_star.expect("seed 9 has a north pole star");
        assert!(pole_star.contains("celestial pole"));
        assert!(pole_star.contains("north"));

        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.night_sky_lines.unwrap().pole_star, Some(pole_star));
    }

    #[test]
    fn heliacal_lines_render_at_most_three_and_cap_the_year_phase_absence() {
        // Seed 42's default generation has several neighbors; assert the
        // heliacal instrument produces at least one line (or honestly
        // none, for a system with no risings this year) and never more
        // than three.
        let world = generated(42);
        let lines = night_sky_lines(&world).unwrap().unwrap();
        assert!(lines.heliacal.len() <= 3);
        for line in &lines.heliacal {
            assert!(line.contains("returns before dawn at year-phase"));
            assert!(line.contains("days of absence"));
        }
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
        assert!(night_sky_lines(&world).is_err());
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
    fn globe_level_pin_selects_the_canonical_grid() {
        let pins = hornvale_terrain::TerrainPins {
            globe_level: Some(4),
            ..hornvale_terrain::TerrainPins::default()
        };
        let world = build_world_with_roster(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &pins,
            &SettlementPins::default(),
            &default_roster(),
        )
        .expect("level-4 world builds");
        let terrain = terrain_of(&world).expect("terrain");
        assert_eq!(terrain.geosphere().level(), 4);
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

        // The Branches (4-species roster): the globally most-suitable cell
        // is not necessarily goblin's a priori (at seed 42, under the
        // current world, it happens to be), so the recomputed structure
        // must use the flagship's OWN species' psychology and role
        // vocabulary (the same construction `build_world_with_roster`
        // performs per-flagship), not a fixed goblin-baseline
        // `PsychSummary::default()` a single-species world could get away
        // with.
        let flagship_species = hornvale_species::species_of(&world, village.id)
            .expect("the flagship settlement has a species fact");
        let def = hornvale_species::registry()[flagship_species.as_str()];
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

        assert_eq!(
            hornvale_culture::castes_of(&world, village.id),
            hornvale_culture::structure(&env, &psych)
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
        // The head deity reorganizes with the sun's rotation regime. Since
        // SKY-5 the locked head is the felt tide (Ambient): on a frozen sky
        // the low-sky-attention first observer ranks the swelling water
        // above the motionless sun — SEQ-1's locked-world religion.
        assert_eq!(
            head_sentiment(&locked),
            Some(hornvale_religion::Sentiment::Ambient),
            "locked world: the tide heads the pantheon"
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
    fn seed_42_names_are_non_english_and_not_degenerate() {
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
        // At the /v2 glossed epoch a name compounds over its own species'
        // small site-concept vocabulary (its biome, its people's presiding
        // belief) rather than drawing a free stem from the vast phonology
        // name space, so world-wide uniqueness — never guaranteed even at
        // v1 — is meaningfully less de-facto now: "glossed compounds
        // shrink the name space" is this campaign's own documented
        // tradeoff (spec §9), re-measured honestly as a collision-rate
        // calibration in Task 12. This only guards against total collapse
        // (every settlement sharing one name), not a specific rate.
        let set: std::collections::BTreeSet<_> = names.iter().collect();
        assert!(
            set.len() > 1,
            "seed 42 must not collapse every settlement onto a single glossed name"
        );
    }

    #[test]
    fn name_gloss_predicate_is_registered_functional() {
        let mut registry = hornvale_kernel::ConceptRegistry::default();
        register_all(&mut registry).unwrap();
        let def = registry
            .predicate(NAME_GLOSS)
            .expect("name-gloss must be registered");
        assert!(def.functional, "an entity has exactly one glossed meaning");
    }

    #[test]
    fn settlements_and_deities_gain_name_gloss_facts_when_a_gloss_exists() {
        let world = generated(42);
        let settlement_glossed = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .any(|f| world.ledger.text_of(f.subject, NAME_GLOSS).is_some());
        let deity_glossed = world
            .ledger
            .find(hornvale_religion::IS_BELIEF)
            .any(|f| world.ledger.text_of(f.subject, NAME_GLOSS).is_some());
        assert!(
            settlement_glossed,
            "seed 42 should gloss at least one settlement (the sun is a Steeped concept for \
             every shipped species, so a presiding-belief gloss is near-universal)"
        );
        assert!(deity_glossed, "seed 42 should gloss at least one deity");
    }

    #[test]
    fn a_settlement_name_gloss_is_truthful_to_its_own_site_facts() {
        // Every settlement carrying a `name-gloss` fact must gloss to
        // concepts drawn only from its own site: its own biome, or one of
        // the phenomenon concepts a presiding belief can map to.
        let world = generated(42);
        let plausible_phenomenon_concepts = ["sun", "moon", "star", "day", "wind"];
        let mut checked_any = false;
        for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
            let id = f.subject;
            let Some(gloss) = world.ledger.text_of(id, NAME_GLOSS) else {
                continue;
            };
            checked_any = true;
            let biome = world
                .ledger
                .text_of(id, hornvale_settlement::BIOME)
                .expect("every settlement has a biome");
            let mut remainder = gloss.to_string();
            remainder = remainder.replace(biome, "");
            for concept in plausible_phenomenon_concepts {
                remainder = remainder.replace(concept, "");
            }
            assert!(
                remainder.chars().all(|c| c == '-'),
                "gloss {gloss:?} for settlement biome {biome:?} names a concept outside its \
                 own site facts"
            );
        }
        assert!(checked_any, "seed 42 should gloss at least one settlement");
    }

    #[test]
    fn glossed_names_are_stable_across_two_builds() {
        let a = generated(42);
        let b = generated(42);
        let names_a: Vec<String> = hornvale_settlement::all_settlements(&a)
            .iter()
            .map(|v| v.name.clone())
            .collect();
        let names_b: Vec<String> = hornvale_settlement::all_settlements(&b)
            .iter()
            .map(|v| v.name.clone())
            .collect();
        assert_eq!(names_a, names_b);
    }

    /// The concepts `lex` holds as a bare [`hornvale_language::LexEntry::Root`]
    /// (as opposed to a compound or a gap).
    fn root_concepts(lex: &hornvale_language::Lexicon) -> Vec<&str> {
        lex.entries()
            .filter(|(_, e)| matches!(e, hornvale_language::LexEntry::Root { .. }))
            .map(|(c, _)| c)
            .collect()
    }

    /// `concept`'s proto-root segments in `lex`. Panics if `concept` isn't
    /// held as a root — callers only ask after confirming membership via
    /// [`root_concepts`].
    fn root_proto<'a>(
        lex: &'a hornvale_language::Lexicon,
        concept: &str,
    ) -> &'a [hornvale_language::Segment] {
        match lex.entry(concept) {
            Some(hornvale_language::LexEntry::Root { derivation, .. }) => &derivation.proto,
            other => panic!("{concept} is not a Root entry in this lexicon: {other:?}"),
        }
    }

    #[test]
    fn goblinoid_daughters_are_cognate_where_both_hold_a_root() {
        // Whether a daughter is Steeped in a given concept depends on WHERE
        // it is placed, so do NOT hard-code "water" (a new species may not
        // be placed near it). Intersect the concepts each daughter actually
        // holds as a Root and assert cognacy over that set — robust to
        // placement.
        let world = generated(42);
        let g = lexicon_of(&world, "goblin").unwrap();
        let h = lexicon_of(&world, "hobgoblin").unwrap();
        let shared: Vec<&str> = root_concepts(&g)
            .into_iter()
            .filter(|c| root_concepts(&h).contains(c))
            .collect();
        assert!(
            !shared.is_empty(),
            "goblin and hobgoblin must share \u{2265}1 rooted concept"
        );
        for c in shared {
            // same family + proto_ph => identical proto-root; modern forms
            // may differ.
            assert_eq!(
                root_proto(&g, c),
                root_proto(&h, c),
                "{c}: same family proto-root"
            );
        }
    }

    #[test]
    fn goblinoid_daughters_actually_diverge() {
        // DIVERGENCE-REALITY GUARD (stemmatics: descent is proven by shared
        // INNOVATIONS, not a shared ancestor alone). Some concept rooted in
        // all three daughters must have >=2 distinct present-day forms —
        // else the "family" is aliases and L4 would have nothing to
        // reconstruct. Robust even if two cascades coincide, because the
        // daughters' inventories differ along the loudness axis and
        // nativization diverges them.
        fn some_shared_concept_has_distinct_forms(lexes: &[hornvale_language::Lexicon]) -> bool {
            let Some((first, rest)) = lexes.split_first() else {
                return false;
            };
            let candidates: Vec<&str> = root_concepts(first)
                .into_iter()
                .filter(|c| rest.iter().all(|lex| root_concepts(lex).contains(c)))
                .collect();
            candidates.into_iter().any(|c| {
                let forms: Vec<&hornvale_language::WordViews> = lexes
                    .iter()
                    .map(|lex| match lex.entry(c) {
                        Some(hornvale_language::LexEntry::Root { views, .. }) => views,
                        _ => panic!("{c} confirmed as a root above"),
                    })
                    .collect();
                !forms.windows(2).all(|w| w[0] == w[1])
            })
        }

        let world = generated(42);
        let lexes: Vec<_> = ["goblin", "hobgoblin", "bugbear"]
            .iter()
            .map(|s| lexicon_of(&world, s).unwrap())
            .collect();
        assert!(
            some_shared_concept_has_distinct_forms(&lexes),
            "the three daughters must not be identical (a degenerate family)"
        );
    }

    #[test]
    fn every_goblinoid_word_is_in_its_inventory() {
        let world = generated(42);
        for sp in ["goblin", "hobgoblin", "bugbear"] {
            let ph = language_of(&world, sp);
            let lex = lexicon_of(&world, sp).unwrap();
            for (_c, e) in lex.entries() {
                if let hornvale_language::LexEntry::Root { derivation, .. } = e {
                    assert!(
                        derivation.modern.iter().all(|s| ph.inventory.contains(s)),
                        "{sp}: a nativized root must draw only from its own inventory"
                    );
                }
            }
        }
    }

    #[test]
    fn kobold_lexicon_mechanism_is_stable_given_fixed_exposures() {
        let world = generated(42);
        let ph = language_of(&world, "kobold");
        let ex = exposure_of(&world, "kobold").unwrap();
        // Singleton path: family == species, proto_ph == ph; the merger-aware
        // daughters slice is the family's one member (kobold itself).
        let daughters = family_daughters(&world, &default_roster(), "kobold");
        let direct = hornvale_language::build_lexicon(
            &world.seed,
            "kobold",
            "kobold",
            &ph,
            &ph,
            &ex,
            &daughters,
        );
        assert_eq!(lexicon_of(&world, "kobold").unwrap(), direct);
    }
}
