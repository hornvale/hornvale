//! Hornvale composition root — the only place all domains meet.
//!
//! Wires registrations and geneses in cascade order, and gathers the
//! almanac's context. Domains stay ignorant of each other; this module is
//! where the application composes them (Constitution §2.6).
#![warn(missing_docs)]

use hornvale_almanac::AlmanacContext;
use hornvale_astronomy::{
    CELESTIAL_BODY, ConstantSun, GeneratedSky, GenesisError, NIGHT_STAR, SEASONAL_CYCLE, SkyPins,
    SkyReport, facts, figures, generate, parse_pin, pin_strings,
    streams::ROOT as ASTRONOMY_STREAM_ROOT,
};
use hornvale_climate::{
    AMBIENT, ClimateInputs, ClimateReport, GeneratedClimate, PrecipRegime, RotationRegime,
    SeafloorFeature, UniformClimate, diurnal_waveform,
};
use hornvale_kernel::math;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{
    ConceptRegistry, Domain, EntityId, Fact, GeoCoord, Geosphere, KindId, LedgerError,
    ObserverContext, PerceptionLens, PhenomenaSource, Phenomenon, ReferenceElevation,
    RegistryError, Seed, Temperature, Value, World, WorldContext, WorldTime, observe,
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

pub mod chorus;
pub mod components;
pub mod history_bake;
pub mod history_emit;
pub mod schedule;
pub mod settlement_pins;
pub mod streams;
pub use chorus::{
    ChorusVoice, DoctrineVoice, LadderRung, Observations, PredictionCrisis, account_params_of,
    accounts_from, accounts_of, beta_of, chorus_ground, crisis_of, cyclic_beliefs_of,
    day_schema_of, doctrine_beta_of, doctrine_of, doctrine_params_of, doctrines_of,
    folk_verifiable, ladder_of, noun_class_of, observability_table, observations_of,
    pathological_params, schema_prior, sky_capability, tongue_morphology_of,
};
pub use components::WorldComponents;
pub use history_bake::{BakeCensus, BakeConfig, History, bake, census};
pub use history_emit::{
    GOBLINOIDS, Stratigraphy, TERRITORY_DILATION_RINGS, emit_history, emit_now, goblinoid_overlap,
    goblinoid_region_overlap, migration_events, ruins_of_people, stratigraphy, territories,
};
pub use settlement_pins::SettlementPins;

/// Errors from building a world.
/// type-audit: bare-ok(prose: Pins.0), bare-ok(prose: MalformedKind.0)
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
    /// A kind's component-set is malformed (referential-integrity failure at
    /// assembly): e.g. a peopled kind missing a biosphere or a speech row.
    MalformedKind(String),
    /// The genesis capability schema rejected the world's registry (a
    /// declared cycle, or a functional predicate with two declared writers —
    /// spec §7 as a load-time gate, ecs-c6 T3).
    Schedule(hornvale_kernel::ScheduleError),
}

impl std::fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildError::Registry(e) => write!(f, "registry: {e}"),
            BuildError::Ledger(e) => write!(f, "ledger: {e}"),
            BuildError::Genesis(e) => write!(f, "sky genesis: {e}"),
            BuildError::Pins(reason) => write!(f, "pins: {reason}"),
            BuildError::TerrainGenesis(e) => write!(f, "terrain genesis: {e}"),
            BuildError::MalformedKind(reason) => write!(f, "malformed kind: {reason}"),
            BuildError::Schedule(e) => write!(f, "schedule: {e:?}"),
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

/// The composition root's domain roster — the single membership list every
/// per-domain aggregation draws on (registration; the streams manifest).
///
/// Order is REGISTRATION order, not alphabetical: `language::register_concepts`
/// references concepts it does not own (terrain's `stone`, religion's `god`, …)
/// and must run AFTER their owners or it claims them under domain "language"
/// and conflicts. Owners first, `language` last (before streamless
/// `paleoclimate`). `register_all` iterates this as stored; the streams
/// manifest sorts by crate name. Adding a domain is one line here plus its
/// `Domain` impl — respecting this order only if it lends/borrows stratum
/// concepts.
///
/// Membership is the *declarative* surface, not the `domains/` directory:
/// `hornvale-demography` registers no concepts and draws no streams (a
/// pure-function library this root calls directly), so it has nothing to
/// declare and stays off the roster.
pub const DOMAINS: &[&dyn Domain] = &[
    &hornvale_astronomy::Astronomy,
    &hornvale_climate::Climate,
    &hornvale_terrain::Terrain,
    &hornvale_settlement::Settlement,
    &hornvale_species::Species,
    &hornvale_culture::Culture,
    &hornvale_religion::Religion,
    &hornvale_language::Language,
    &hornvale_paleoclimate::Paleoclimate,
    // The Living Community: the deep-history bake commits occupation facts
    // (`is-occupation`, `occ-*`, `is-ruin`) and draws under the `history/*`
    // seed labels, so its predicates must be registered and its stream labels
    // published into the manifest before genesis emits them.
    &hornvale_history::History,
];

/// Register every domain's concepts. `NAME_GLOSS` itself is kernel-core
/// (ecs-c6 T3): `World::new` already registers it, so this no longer
/// registers it — a predicate is owned by its single definition site (the
/// kernel), so registration lives there. (Re-registering the identical
/// definition would be idempotent, not an error; the registry only rejects a
/// *conflicting* redefinition — but the ownership model keeps it in one place.)
pub fn register_all(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    for domain in DOMAINS {
        domain.register_concepts(registry)?;
    }
    Ok(())
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

/// One history-emitted settlement paired back to the naming pass's build-local
/// species tag. The deep-history bake places the world and `emit_history`
/// commits each alive occupation as an `is-settlement`; the culture, religion,
/// and peoples stages read these instead of the retired demography
/// `StackSettlement` scatter. The `tag` is `species_set`'s 0-based position of
/// this settlement's people — a build-local dense index, never serialized (a
/// people's durable identity is its `KindId` label / `occ-people` fact).
struct HistoryPlacement {
    /// The settlement entity `emit_history` minted (an alive occupation).
    id: EntityId,
    /// Build-local `species_set` index of this settlement's people.
    tag: usize,
    /// The Geosphere cell the settlement sits on.
    cell: hornvale_kernel::CellId,
    /// The settlement's committed population (the occupation's peak).
    population: u32,
}

/// Commit the present-frame descriptive facts a settlement carries beyond its
/// structural (`is-settlement`/`population`/`cell-id`) skeleton: its name,
/// place tag, biome, and coordinates. `emit_history` mints the settlement
/// entity and commits the structural facts; the composition root's naming pass
/// adds these onto the SAME entity, so history stays the sole placer (the
/// retired `settlement::genesis` used to mint them together with placement).
/// Provenanced to the bake; day-stamped at the present frame (0.0), matching
/// the old settlement-genesis convention for descriptors.
fn settlement_descriptor_facts(
    world: &mut World,
    id: EntityId,
    name: &str,
    biome: &str,
    latitude: f64,
    longitude: f64,
) -> Result<(), BuildError> {
    let fact = |predicate: &str, object: Value| Fact {
        subject: id,
        predicate: predicate.to_string(),
        object,
        place: Some(id),
        day: Some(0.0),
        provenance: hornvale_history::streams::BAKE.as_str().to_string(),
    };
    world.ledger.commit(
        fact(hornvale_kernel::NAME, Value::Text(name.to_string())),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(hornvale_settlement::IS_PLACE, Value::Flag(true)),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(hornvale_settlement::BIOME, Value::Text(biome.to_string())),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(hornvale_settlement::LATITUDE, Value::Number(latitude)),
        &world.registry,
    )?;
    world.ledger.commit(
        fact(hornvale_settlement::LONGITUDE, Value::Number(longitude)),
        &world.registry,
    )?;
    Ok(())
}

/// A `name-gloss` fact for `subject` — kernel-core (see
/// [`hornvale_kernel::NAME_GLOSS`]), committed by both the settlement and
/// religion genesis stages on disjoint subjects, so its provenance is
/// `"worldgen"` rather than any one domain's tag.
fn name_gloss_fact(subject: EntityId, gloss: &str) -> Fact {
    Fact {
        subject,
        predicate: hornvale_kernel::NAME_GLOSS.to_string(),
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

/// Weight the ambient moisture floor carries in the carrying-capacity
/// freshwater term (The Confluence), against the `river_proximity` term it
/// is `max`-combined with. Keeps riverless-but-wet regions habitable
/// (moisture alone can still contribute up to this fraction) while leaving
/// headroom for river proximity (`1.0` on a river, decaying over
/// `RIVER_REACH`) to dominate near actual rivers — the mechanism
/// condensation is meant to ride. TUNED (the-confluence T2, from the
/// brief's placeholder 0.5): the keystone measurement
/// (`windows/worldgen/tests/confluence.rs`) found that `RIVER_REACH`'s hop
/// radius is also the ruler the test uses for "near a river" — widening it
/// to inflate the settlement fraction just inflates the ambient land
/// coverage of "near" too (90% of seed-42's land sits within 7 hops of a
/// river), which trivializes the emergence claim rather than strengthening
/// it. `RIVER_REACH` therefore stays at its T1 value (3 hops; ~55% ambient
/// land coverage); this constant alone was pulled down from 0.5 to sharpen
/// the river-vs-moisture contrast without touching the ruler.
const MOISTURE_FLOOR_WEIGHT: f64 = 0.2;

/// Condensation threshold: an attractor whose catchment population clears
/// this becomes a settlement. CALIBRATED (the-gathering, 2026-07-13): tuned
/// against the carrying_capacity constants to a manageable seed-42
/// settlement count. Before tuning, the placeholder 0.5 condensed 998
/// settlements on the level-6 seed-42 world (avg catchment ~7 people); the
/// then-value of 10.0 condensed 182 (avg catchment ~22, max 71) — low
/// hundreds, an order of magnitude down from the placeholder and in the
/// range of the retired spaced scatter's town count.
///
/// RE-CALIBRATED (the-confluence T3, 2026-07-19): the sharper freshwater
/// term (T2's re-point at `river_proximity`, with `MOISTURE_FLOOR_WEIGHT`
/// pulled to 0.2) concentrates catchments along river corridors rather than
/// spreading them over broad riverless-but-moist land, and the old
/// `THRESHOLD = 10.0` condensed only 79 seed-42 settlements — below the
/// [100, 400] sane band (`windows/worldgen/tests/confluence.rs`,
/// `settlement_count_stays_in_the_sane_band_after_the_freshwater_repoint`).
///
/// A naive re-fit is not enough: lowering `THRESHOLD` alone trades settlement
/// COUNT against T2's keystone (`settlements_condense_near_rivers_emergently`)
/// — a sweep found both move together non-monotonically in a narrow band
/// (e.g. 0.9–1.5 clears the [100, 400] count band comfortably but the
/// near-river fraction hovers right AT the keystone's 0.7 floor, 0.6991–
/// 0.7018, effectively zero margin; 2.5+ gives the keystone real headroom but
/// drops the count below 100). `1.7` was chosen as the best point found in
/// that sweep: seed 42 condenses 108 settlements (comfortably inside the
/// band, well clear of its 100 floor) while the near-river fraction reads
/// 0.7222 — a real, if modest, margin over the keystone floor rather than
/// sitting on top of it. A save-format constant from here on. Module scope
/// (hoisted from the settlement-genesis stage closure, Task A16a) so
/// [`demography_report`]'s Lab accessor and the genesis path share the one
/// definition — they must never diverge.
const CONDENSATION_THRESHOLD: f64 = 1.7;

/// Settlers a maximal-suitability cell supports: the scale that turns
/// `demography::carrying_capacity`'s dimensionless suitability (~[0, 1.7])
/// into the headcount capacity the deep-history bake reasons in
/// (`pressure = population / eff_capacity`). Tuned with the bake's genesis
/// founding density so seed-42's live settlement count lands in the walkable
/// band (`windows/worldgen/tests/history_placement.rs`). A save-format
/// constant: changing it re-places every world. `pub` so the demography
/// calibration (`windows/lab`) can express the population-conservation
/// ceiling in the bake's own headcount units (see `COLLAPSE_PRESSURE`).
/// type-audit: bare-ok(ratio)
pub const SETTLERS_PER_CAPACITY: f64 = 100.0;

/// The bare per-cell carrying-capacity inputs, shared across species (spec
/// §2): the same terrain/climate reads the retired suitability scatter used.
/// Each species folds its own psychology on top via `species_carrying_input`.
/// Exposed (not inlined at the one call site) so a Lab metric can recompute
/// the identical field the composition root feeds into demography — the
/// gradient calibration (Task 8) reads it without duplicating the formula.
pub fn carrying_inputs_of(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> hornvale_kernel::CellMap<hornvale_demography::CarryingInput> {
    // The Confluence: freshwater rides proximity to the real river network,
    // not a smooth drainage/moisture proxy — so K spikes near rivers and
    // settlements condense there (emergent). A moisture floor keeps
    // riverless-but-wet regions habitable.
    let water_kind = hornvale_kernel::CellMap::from_fn(geo, |c| terrain.water_kind_at(c));
    let river_prox =
        hornvale_terrain::river_proximity(geo, &water_kind, hornvale_terrain::RIVER_REACH);
    hornvale_kernel::CellMap::from_fn(geo, |cell| {
        let coastal = geo.neighbors(cell).iter().any(|n| terrain.is_ocean(*n));
        let moisture = climate.moisture_at(cell);
        // Seawater is not freshwater: coastal access is priced by the
        // coast bonus in carrying_capacity, not smuggled in here.
        let freshwater = (moisture * MOISTURE_FLOOR_WEIGHT)
            .max(*river_prox.get(cell))
            .clamp(0.0, 1.0);
        let aridity = ((0.2 - moisture).max(0.0) * 5.0).clamp(0.0, 1.0);
        let hostility = terrain.unrest_at(cell).max(aridity).clamp(0.0, 1.0);
        hornvale_demography::CarryingInput {
            habitable: *climate.habitability().get(cell),
            temperature_c: climate.mean_temperature_at(cell).get(),
            moisture,
            freshwater,
            coastal,
            hostility,
        }
    })
}

/// Soil order per cell — the climate-coupled projection of The Ground
/// (spec §4). Pure: reads terrain lithology + climate temperature/moisture.
/// Every cell (ocean included) gets a classification from `classify_soil`;
/// ocean floors read whatever their (irrelevant but harmless) depth/slope
/// inputs classify to — callers that care about land only should guard
/// with `terrain.is_ocean`.
pub fn soil_of(
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    geo: &Geosphere,
) -> hornvale_kernel::CellMap<hornvale_terrain::SoilOrder> {
    hornvale_kernel::CellMap::from_fn(geo, |cell| {
        let mat = terrain.material_at(cell);
        let here = terrain.elevation_at(cell).get();
        let slope = geo
            .neighbors(cell)
            .iter()
            .map(|n| here - terrain.elevation_at(*n).get())
            .fold(0.0_f64, f64::max);
        hornvale_terrain::classify_soil(
            terrain.rock_at(cell),
            climate.mean_temperature_at(cell).get(),
            climate.moisture_at(cell),
            slope,
            &mat.soil_depth,
        )
    })
}

/// Fold a species' psychology (spec §4) into its carrying-capacity inputs.
/// The retired suitability formula scaled a people's freshwater weight by its
/// time horizon and softened its hostility penalty by its threat response; the
/// same psychology now scales each species' freshwater draw and hostility
/// sensitivity so per-species variation survives the move to the carrying-
/// capacity field. Exact fidelity to the old weights was not required —
/// CALIBRATED (the-gathering, 2026-07-13): the summed-across-roster gradient
/// these per-species factors feed into (the Lab's `capacity-by-abs-latitude`
/// metric) already reproduces the real biomass-by-latitude gradient
/// decisively against the 200-seed `census-of-the-gathering` census (mean
/// 27.15; see `carrying_capacity.rs`'s freeze note for the full measurement),
/// so these factors are frozen unchanged as save-format constants.
/// Identity-ish at the goblin baseline; deterministic in `p`.
pub fn species_carrying_input(
    base: hornvale_demography::CarryingInput,
    p: &hornvale_species::PsychVector,
) -> hornvale_demography::CarryingInput {
    // Long-horizon peoples value reliable water more; bold (high threat-
    // response) peoples tolerate hostile ground better (lower effective
    // hostility). Both factors are >= 0 and the results re-clamp to [0, 1].
    let freshwater_factor = 0.5 + p.time_horizon;
    let hostility_factor = (1.5 - p.threat_response).max(0.0);
    hornvale_demography::CarryingInput {
        freshwater: (base.freshwater * freshwater_factor).clamp(0.0, 1.0),
        hostility: (base.hostility * hostility_factor).clamp(0.0, 1.0),
        ..base
    }
}

/// Ambient detritus supply (BIO-35 Stage 1: The Demesne). Dead-matter
/// resource is treated as broadly available this stage — a small constant
/// floor, not a spatial field. A real spatial detritus field (litterfall /
/// carcass turnover) is a later refinement once T2 wires the per-axis dot
/// product this stage is building toward.
/// type-audit: bare-ok(ratio)
pub const DETRITUS_AMBIENT: f64 = 0.2;

/// Fraction of primary production that is grazable plant forage. Plant-
/// forage supply tracks photosynthate spatially, at a reduced amplitude
/// (not all NPP is grazable — wood, roots, and unpalatable growth are not).
/// type-audit: bare-ok(ratio)
const FORAGE_FRACTION: f64 = 0.5;

/// The `PLANT_FORAGE` supply field (BIO-35 Stage 1: The Demesne, task T1): a
/// fraction of the NPP-based `base_carrying` (grazable matter tracks primary
/// production spatially). Pure, deterministic, no RNG — a direct scale of an
/// already-computed field.
/// type-audit: bare-ok(count: base_carrying), bare-ok(count: return)
pub fn forage_supply_field(
    geo: &Geosphere,
    base_carrying: &hornvale_kernel::CellMap<f64>,
) -> hornvale_kernel::CellMap<f64> {
    hornvale_kernel::CellMap::from_fn(geo, |c| base_carrying.get(c) * FORAGE_FRACTION)
}

/// The `MINERAL` supply field (BIO-35 Stage 1: The Demesne, task T1): The
/// Ground's per-cell mineral prospectivity ([`GeneratedTerrain::prospectivity_at`],
/// `[0,1]`) scaled to the supply range so it is comparable to `base_carrying`
/// in the weighted sum T2 builds (`scale` is the mineral supply amplitude —
/// the one calibration knob, re-fit in T3). Pure, deterministic — a direct
/// read, no float ordering involved (`total_cmp`-free).
/// type-audit: bare-ok(ratio: scale), bare-ok(count: return)
pub fn mineral_supply_field(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    scale: f64,
) -> hornvale_kernel::CellMap<f64> {
    hornvale_kernel::CellMap::from_fn(geo, |c| terrain.prospectivity_at(c) * scale)
}

/// The mineral supply field's amplitude (BIO-35 Stage 1: The Demesne, task
/// T2) — the one calibration knob for [`mineral_supply_field`]'s `scale`
/// argument as consumed by [`niche_per_species_k`]. Re-fit in a later task
/// (T3) once the emergence keystone's measured diversification is read
/// against a wider seed sample; frozen here at parity with `MINERAL`'s
/// pre-repoint contribution magnitude (chosen, not fit — see the emergence
/// test's doc comment for the measured before/after).
/// type-audit: bare-ok(ratio)
const MINERAL_SUPPLY_SCALE: f64 = 1.0;

/// The per-axis resource supply for one niche at one cell (BIO-35 Stage 1:
/// The Demesne, task T2): the dot product of the species' uptake vector
/// (`niche`) with the per-cell supply vector (`per_axis`) — the
/// rank-restored replacement for the old `base_carrying(cell) × Σuptake`
/// scalar, which collapsed every species' spatial pattern onto the same
/// single field (differing only by an overall magnitude). A niche direction
/// now SELECTS which cells supply it — a photosynthate-heavy niche peaks
/// where `PHOTOSYNTHATE` supply peaks, a mineral-heavy niche peaks where
/// `MINERAL` supply peaks, even when those are different cells. Pure; no
/// RNG, no float ordering (a plain weighted sum, not a comparison).
/// type-audit: bare-ok(ratio: per_axis), bare-ok(ratio: return)
pub fn axis_supply(
    niche: &hornvale_kernel::ResourceVector,
    per_axis: &[(hornvale_kernel::ResourceAxis, f64)],
) -> f64 {
    per_axis
        .iter()
        .map(|(axis, supply)| niche.weight(*axis) * supply)
        .sum()
}

/// Per-species niche-differentiated carrying-capacity K = resource-supply ×
/// condition-response (The Niche). Pure; seed-free. Replaces the flat-NPP K
/// for the coexistence stack. `species_biosphere` index order tags the fields.
///
/// **The returned `u32` is a build-local dense index, not identity.** It is
/// `species_set`'s 0-based position for this one call, derived by
/// enumerating a `KindId`-ordered roster/registry slice; it is valid only
/// within this build and is **never serialized**. A kind's durable,
/// serialized identity is its [`KindId`](hornvale_kernel::KindId) label
/// (committed as the `SPECIES_NAME` `Value::Text` fact), never this index —
/// decision 0015: a name is its own key. Inserting or removing a kind
/// upstream can renumber every downstream `u32`, but never changes any
/// other kind's label. Callers that pair this index with other per-call
/// tuples (`species` below, `mass_map`, `.composition` tags) must rebuild
/// all of them together from the same `species_set` ordering; none of it
/// survives a save/load boundary. The other `.enumerate()` sites in this
/// file that mint a `(tag as u32, ..)` pair share this exact contract.
///
/// For each species and cell: `saturate(axis_supply(niche, per_axis))` (the
/// resource-supply term — BIO-35 Stage 1's rank-restored per-axis dot
/// product: `PHOTOSYNTHATE` rides the existing NPP-based `base_carrying`
/// (keeps its conditioning), `PLANT_FORAGE`/`MINERAL` read their own T1
/// supply fields, `DETRITUS` reads the ambient constant, and `ANIMAL_PREY`
/// is Stage 2's placeholder zero (a later stage's trophic wiring); see
/// [`axis_supply`], Type-II-saturated so intake plateaus) multiplied by the
/// four condition-response terms (temperature/moisture/insolation/elevation),
/// each [`hornvale_kernel::ConditionResponse::eval`]'d against that cell's
/// [`substrate_field`] reading. Temperature/moisture/insolation are
/// buffer-able (floored by the species'
/// [`hornvale_kernel::sovereignty_floor`]); elevation is hard (floor 0.0) —
/// sovereignty buffers physiology but not geometry.
/// type-audit: bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: insolation_scalar), bare-ok(index: return)
pub fn niche_per_species_k(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    obliquity_deg: f64,
    insolation_scalar: f64,
    regime: &RotationRegime,
    species_biosphere: &[&hornvale_species::BiosphereTraits],
) -> Vec<(u32, hornvale_kernel::CellMap<f64>)> {
    let base_inputs = carrying_inputs_of(geo, terrain, climate);
    let base_carrying = hornvale_demography::carrying_capacity(geo, &base_inputs);
    let substrate = substrate_field(
        geo,
        terrain,
        climate,
        obliquity_deg,
        insolation_scalar,
        regime,
    );
    // The Demesne/T2: per-axis supply fields, hoisted out of the per-species
    // loop below — each is a pure function of terrain/climate, built once
    // and shared by every species' dot product.
    let mineral = mineral_supply_field(geo, terrain, MINERAL_SUPPLY_SCALE);
    let forage = forage_supply_field(geo, &base_carrying);

    species_biosphere
        .iter()
        .enumerate()
        .map(|(tag, bio)| {
            let floor_buf = hornvale_kernel::sovereignty_floor(bio.mass, bio.potency);
            let cn = &bio.condition_niche;
            let k = hornvale_kernel::CellMap::from_fn(geo, |cell| {
                let s = substrate.get(cell);
                // Rank-restored supply via the extracted helper: the axis
                // dot product, not the old summed-uptake scalar.
                use hornvale_kernel::{
                    ANIMAL_PREY, DETRITUS, MINERAL, PHOTOSYNTHATE, PLANT_FORAGE,
                };
                let per_axis = [
                    (PHOTOSYNTHATE, *base_carrying.get(cell)),
                    (PLANT_FORAGE, *forage.get(cell)),
                    (MINERAL, *mineral.get(cell)),
                    (DETRITUS, DETRITUS_AMBIENT),
                    (ANIMAL_PREY, 0.0),
                ];
                let supply = axis_supply(&bio.niche, &per_axis);
                let saturated = supply / (1.0 + supply);
                saturated
                    * cn.temperature.eval(s.temperature_c, floor_buf)
                    * cn.moisture.eval(s.moisture, floor_buf)
                    * cn.insolation.eval(s.insolation, floor_buf)
                    * cn.elevation.eval(s.elevation, 0.0)
            });
            (tag as u32, k)
        })
        .collect()
}

/// Build the full coexistence-stack demography report for `world`, over
/// `roster` (spec: the whole roster, matching the unpinned settlement-
/// genesis path — a species-pinned world still reports every roster
/// species' field, since no pin is reconstructed here), against an
/// EXPLICITLY supplied `beta`/`floor` rather than the frozen
/// [`hornvale_demography::BETA`]/[`hornvale_demography::FLOOR`] constants.
/// Reconstructs terrain and climate, then assembles the report from
/// [`niche_per_species_k`] (The Niche's differentiated K) using demography's
/// pub building blocks, mirroring [`hornvale_demography::report`]'s body.
/// Since The Seam's cutover, settlement genesis packs the same niche-K stack
/// (peopled species only), so this accessor and genesis share one pipeline
/// shape; this variant just exposes it over the full roster with a tunable
/// `beta`. Pure and seed-free beyond the world's already-committed facts:
/// two calls with the same `(world, roster, beta, floor)` produce
/// byte-identical reports, so a β-sweep calibration harness (task A16b) can
/// vary `beta` across many calls without rebuilding the world or drawing new
/// seed state. [`demography_report`] is this function pinned to the frozen
/// constants — the Lab accessor worldgen ships.
///
/// type-audit: bare-ok(ratio: beta), bare-ok(count: floor)
pub fn demography_report_with_beta(
    world: &World,
    wc: &WorldComponents,
    beta: f64,
    floor: f64,
) -> Result<hornvale_demography::DemographyReport, BuildError> {
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    demography_report_with_beta_from(world, wc, beta, floor, &terrain, &climate)
}

/// [`demography_report_with_beta`], reusing ALREADY-BUILT terrain/climate
/// (a Lab view's `terrain()`/`climate()`) instead of re-sculpting the globe —
/// the census's demography metrics call this. Byte-identical: the passed
/// terrain/climate equal `terrain_of`/`climate_of`, and the report is pure
/// over the committed world (no seed draws).
/// type-audit: bare-ok(ratio: beta), bare-ok(count: floor)
pub(crate) fn demography_report_with_beta_from(
    world: &World,
    wc: &WorldComponents,
    beta: f64,
    floor: f64,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<hornvale_demography::DemographyReport, BuildError> {
    let sky = sky_of(world)?;
    let geo = terrain.geosphere();
    let (insolation_scalar, obliquity_deg, regime, _year, _year_phase_offset) =
        stellar_inputs(&sky);
    // Biosphere per kind, read from the world's component set (ECS c3) in
    // ascending-`KindId` order — the build-local dense index the `tag`s below
    // and `niche_per_species_k`'s returned `u32` share. This is the WHOLE
    // component set (fauna included), matching the report's "whole roster"
    // contract; the order equals the default roster's `registry()`-key order.
    let species_biosphere: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, bio)| bio).collect();

    let per_species_k = niche_per_species_k(
        geo,
        terrain,
        climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &species_biosphere,
    );
    // `tag as u32` here is the same build-local dense index documented on
    // `niche_per_species_k` — never serialized, never identity.
    let species: Vec<(u32, hornvale_kernel::Mass, hornvale_kernel::ResourceVector)> =
        species_biosphere
            .iter()
            .enumerate()
            .map(|(tag, bio)| (tag as u32, bio.mass, bio.niche.clone()))
            .collect();

    let settlements =
        hornvale_demography::condense_tagged(&per_species_k, geo, CONDENSATION_THRESHOLD);
    let stack = hornvale_demography::coexist::pack(geo, &per_species_k, &species, beta, floor);
    let mass_map: std::collections::BTreeMap<u32, hornvale_kernel::Mass> =
        species.iter().map(|(id, m, _)| (*id, *m)).collect();
    let stack_settlements = hornvale_demography::stack_condense::condense_stack(
        geo,
        &stack,
        &mass_map,
        CONDENSATION_THRESHOLD,
    );
    let byproducts =
        hornvale_demography::byproducts::byproducts(geo, &stack, &per_species_k, floor);

    Ok(hornvale_demography::DemographyReport {
        per_species_k,
        settlements,
        stack,
        stack_settlements,
        byproducts,
    })
}

/// Build the full coexistence-stack demography report for `world`, over
/// `roster`, at the FROZEN `BETA`/`FLOOR` constants — so the report is byte-
/// identical to the one settlement genesis built internally (task A16a: a
/// Lab accessor for the later A16b β calibration). Delegates to
/// [`demography_report_with_beta`]; see that function for the full wiring
/// doc. Deterministic: reads only already-committed facts, draws nothing new
/// from the seed.
pub fn demography_report(
    world: &World,
    wc: &WorldComponents,
) -> Result<hornvale_demography::DemographyReport, BuildError> {
    demography_report_with_beta(
        world,
        wc,
        hornvale_demography::BETA,
        hornvale_demography::FLOOR,
    )
}

/// The diet-niche `ANIMAL_PREY` weight above which a species counts as a
/// CARNIVORE for the predator-pressure field (The Quarry) — a prey-dominant
/// diet. The obligate apexes (dragons, owlbear) sit at `1.0`; a balanced
/// omnivore at `0.5` is NOT a predator by this threshold. Authored.
const CARNIVORE_THRESHOLD: f64 = 0.5;

/// The per-cell PREDATOR-PRESSURE field (The Quarry) — the ambient risk a quarry
/// senses of predator territory: the REALIZED density (the coexistence stack, not
/// mere carrying capacity) of CARNIVORE species (diet niche `ANIMAL_PREY`-dominant,
/// see [`CARNIVORE_THRESHOLD`]) summed per cell and normalized to `[0, 1]` by the
/// field's own maximum. Density, not capacity, is the honest signal: it is
/// competition-aware (peoples outcompete predators near their settlements, so
/// settled ground reads LOW even where the land COULD support carnivores) and
/// home-range-adjusted (an apex spread over a wide range is thin everywhere), so
/// it concentrates on genuine WILD predator territory rather than lighting up all
/// fertile land. Derived from [`demography_report`]'s stack (the same fit
/// settlement genesis uses) over the canonical roster — no seed, byte-identical
/// across calls. The first BIOTIC hazard — sourced from life, not climate.
/// type-audit: bare-ok(ratio: return)
pub fn predator_pressure(world: &World) -> Result<hornvale_kernel::CellMap<f64>, BuildError> {
    let wc = WorldComponents::assemble()?;
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    let geo = terrain.geosphere();
    let report = demography_report_from(world, &wc, &terrain, &climate)?;
    // Carnivore tags: the enumeration index into `wc.biosphere` (the same
    // build-local dense index the stack uses), for prey-dominant diets.
    let carnivore: std::collections::BTreeSet<u32> = wc
        .biosphere
        .iter()
        .enumerate()
        .filter(|(_, (_, bio))| {
            bio.niche.weight(hornvale_kernel::ANIMAL_PREY) > CARNIVORE_THRESHOLD
        })
        .map(|(i, _)| i as u32)
        .collect();
    // Sum carnivore REALIZED density (the stack) per cell.
    let carnivore_density: Vec<&hornvale_kernel::CellMap<f64>> = report
        .stack
        .density
        .iter()
        .filter(|(tag, _)| carnivore.contains(tag))
        .map(|(_, d)| d)
        .collect();
    let raw = hornvale_kernel::CellMap::from_fn(geo, |cell| {
        carnivore_density.iter().map(|d| *d.get(cell)).sum::<f64>()
    });
    let max = raw.iter().map(|(_, v)| *v).fold(0.0_f64, f64::max);
    Ok(hornvale_kernel::CellMap::from_fn(geo, |cell| {
        if max > 0.0 {
            (*raw.get(cell) / max).clamp(0.0, 1.0)
        } else {
            0.0
        }
    }))
}

/// The per-cell PREY-PRESSURE field (The Teeth) — the ambient draw a HUNTER
/// senses of prey territory, the anti-symmetric DUAL of [`predator_pressure`].
/// Where the predator field sums CARNIVORE realized density (so a quarry flees
/// it), this sums the PREY BASE's realized density (so a carnivore's hunger is
/// drawn UP it): the coexistence-stack density of **mobile-beast, non-carnivore**
/// species — the herbivores and omnivore-beasts a carnivore hunts. Peoples are
/// excluded from the v1 prey base (a carnivore is drawn to the WILD, not toward
/// settlements — the acute-hunt tier owns predators-stalk-towns), and autotrophs
/// are excluded (a plant is not a carnivore's prey). Realized density, not
/// capacity, so it concentrates on genuine wild prey ground (the same honesty
/// `predator_pressure` paid for). Normalized to `[0, 1]` by its own maximum.
/// Derived from the committed demography stack — no seed, no epoch, byte-identical
/// across calls.
/// type-audit: bare-ok(ratio: return)
pub fn prey_pressure(world: &World) -> Result<hornvale_kernel::CellMap<f64>, BuildError> {
    let wc = WorldComponents::assemble()?;
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    let geo = terrain.geosphere();
    let report = demography_report_from(world, &wc, &terrain, &climate)?;
    // Prey-base tags (the dense stack index): a mobile-beast, non-carnivore
    // species — not a settling people (`social_form != Settled`), not a
    // rooted `Autotroph`, and not itself prey-dominant (`ANIMAL_PREY <=
    // threshold`).
    let prey: std::collections::BTreeSet<u32> = wc
        .biosphere
        .iter()
        .enumerate()
        .filter(|(_, (_kind, bio))| {
            bio.niche.weight(hornvale_kernel::ANIMAL_PREY) <= CARNIVORE_THRESHOLD
                && bio.social_form != hornvale_species::SocialForm::Settled
                && !matches!(
                    bio.metabolic_class,
                    hornvale_species::MetabolicClass::Autotroph
                )
        })
        .map(|(i, _)| i as u32)
        .collect();
    // Sum prey-base REALIZED density (the stack) per cell.
    let prey_density: Vec<&hornvale_kernel::CellMap<f64>> = report
        .stack
        .density
        .iter()
        .filter(|(tag, _)| prey.contains(tag))
        .map(|(_, d)| d)
        .collect();
    let raw = hornvale_kernel::CellMap::from_fn(geo, |cell| {
        prey_density.iter().map(|d| *d.get(cell)).sum::<f64>()
    });
    let max = raw.iter().map(|(_, v)| *v).fold(0.0_f64, f64::max);
    Ok(hornvale_kernel::CellMap::from_fn(geo, |cell| {
        if max > 0.0 {
            (*raw.get(cell) / max).clamp(0.0, 1.0)
        } else {
            0.0
        }
    }))
}

/// The `k` densest WILD concentrations (The Wilding) — the herds and lairs of
/// distinct MOBILE BEAST species, as `(species label, unit-sphere position)`.
/// From [`demography_report`]'s coexistence-stack settlements (the per-cell
/// density condensations), keeps those whose DOMINANT species is a mobile beast —
/// *not* a settling people (`social_form != Settled`) and *not*
/// a rooted `Autotroph` (a plant is placed but never an *agent* that walks and
/// flees) — then takes the densest concentration of each DISTINCT species (a herd
/// leader, a lone apex; not five of the same twig-blight) up to `k`, by biomass.
/// Deterministic (mass-descending, label tie-break) and seed-free. Encapsulates
/// the demography — the vessel mints wild NPCs from these without ever reaching
/// into the stack.
/// type-audit: bare-ok(count: k), bare-ok(identifier-text: return)
pub fn wild_concentrations(world: &World, k: usize) -> Result<Vec<(String, [f64; 3])>, BuildError> {
    let wc = WorldComponents::assemble()?;
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    let report = demography_report_from(world, &wc, &terrain, &climate)?;
    // The dense-index → species-label map (the same ascending-`KindId` order the
    // stack's `dominant` tag indexes into).
    let labels: Vec<String> = wc
        .biosphere
        .iter()
        .map(|(kind, _)| kind.0.to_string())
        .collect();
    let biosphere = hornvale_species::biosphere_registry();
    let is_mobile_beast = |label: &str| -> bool {
        // A mobile beast: a WILD, non-sessile, non-settling kind — `social_form`
        // is `Solitary` or `Gregarious` (not `Settled`, the peoplehood axis; not
        // `Sessile`, a rooted `Autotroph` that is placed but never agentified).
        biosphere.get_by_label(label).is_some_and(|b| {
            matches!(
                b.social_form,
                hornvale_species::SocialForm::Solitary | hornvale_species::SocialForm::Gregarious
            )
        })
    };
    // Each mobile beast's DENSEST home — the stack settlement where its local
    // abundance (its composition fraction × the catchment biomass) peaks. So a
    // charismatic beast present but never *dominant* (an apex over a wide range,
    // a herd sharing its patch) still gets a home, not only the cell-dominators.
    let mut best: std::collections::BTreeMap<String, ([f64; 3], f64)> =
        std::collections::BTreeMap::new();
    for s in &report.stack_settlements {
        for (sid, frac) in &s.composition {
            let Some(label) = labels.get(*sid as usize) else {
                continue;
            };
            if !is_mobile_beast(label) {
                continue;
            }
            let abundance = frac * s.mass_total;
            let entry = best.entry(label.clone()).or_insert((s.position, f64::MIN));
            if abundance > entry.1 {
                *entry = (s.position, abundance);
            }
        }
    }
    // Top `k` distinct beasts by peak local abundance (label tie-break).
    let mut wild: Vec<(String, [f64; 3], f64)> =
        best.into_iter().map(|(l, (p, a))| (l, p, a)).collect();
    wild.sort_by(|a, b| b.2.total_cmp(&a.2).then_with(|| a.0.cmp(&b.0)));
    wild.truncate(k);
    Ok(wild.into_iter().map(|(l, p, _)| (l, p)).collect())
}

/// [`demography_report`], reusing ALREADY-BUILT terrain/climate (a Lab view's
/// `terrain()`/`climate()`) instead of re-sculpting the globe — the census's
/// demography metrics call this. Byte-identical to `demography_report`.
pub fn demography_report_from(
    world: &World,
    wc: &WorldComponents,
    terrain: &hornvale_terrain::GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<hornvale_demography::DemographyReport, BuildError> {
    demography_report_with_beta_from(
        world,
        wc,
        hornvale_demography::BETA,
        hornvale_demography::FLOOR,
        terrain,
        climate,
    )
}

/// The scalar stellar inputs climate needs, derived from this world's sky.
/// Constant-sky worlds get an Earth baseline so the biome map exists for
/// every world (spec: the coarse globe is generated for all).
fn stellar_inputs(sky: &Sky) -> (f64, f64, RotationRegime, f64, f64) {
    match sky {
        Sky::Constant(_) => (
            1.0,
            23.5,
            RotationRegime::Spinning { day_std: 1.0 },
            365.25,
            0.0,
        ),
        Sky::Generated(generated) => {
            let system = generated.system();
            // Insolation relative to Earth: the single shared definition (SKY-15).
            let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
            let obliquity = system.anchor.obliquity.get();
            let regime = match system.anchor.rotation {
                hornvale_astronomy::Rotation::Spinning { day, .. } => {
                    RotationRegime::Spinning { day_std: day.get() }
                }
                hornvale_astronomy::Rotation::Locked => RotationRegime::Locked,
            };
            let year = generated.calendar().year_length().get();
            let year_phase_offset = system.forcing.year_phase_offset;
            (insolation, obliquity, regime, year, year_phase_offset)
        }
    }
}

/// Reconstruct the tier-1 climate for this world: rebuild the terrain globe
/// and the sky, map their outputs into climate's kernel-only inputs, and
/// derive temperature/moisture/biome/habitability. The single construction
/// site for `GeneratedClimate` (the `terrain_of`/`sky_of` pattern).
pub fn climate_of(world: &World) -> Result<GeneratedClimate, BuildError> {
    let terrain = terrain_of(world)?;
    climate_from(world, &terrain)
}

/// Reconstruct the tier-1 climate from a PRE-BUILT terrain — the body of
/// `climate_of` after its `terrain_of` line, taking the terrain the caller
/// already sculpted instead of re-deriving it. Byte-identical to `climate_of`
/// whenever `terrain` equals `terrain_of(world)` (the "pass the pre-built
/// value" idiom; the sole extra construction site for callers that already
/// hold the terrain).
pub fn climate_from(
    world: &World,
    terrain: &GeneratedTerrain,
) -> Result<GeneratedClimate, BuildError> {
    let sky = sky_of(world)?;
    let geo = terrain.geosphere();
    let elevation = &terrain.globe().elevation;
    let seafloor =
        hornvale_kernel::CellMap::from_fn(geo, |cell| seafloor_feature(terrain.boundary_at(cell)));
    let (insolation, obliquity_deg, regime, year_length_std, year_phase_offset) =
        stellar_inputs(&sky);
    Ok(GeneratedClimate::generate(&ClimateInputs {
        geosphere: geo,
        elevation,
        sea_level: terrain.sea_level(),
        seafloor: &seafloor,
        insolation,
        obliquity_deg,
        regime,
        year_length_std,
        year_phase_offset,
        seed: world.seed,
    }))
}

/// The four v1 environmental fields at one cell — the substrate the habitat
/// model (The Niche) scores a species' condition niche against.
/// type-audit: bare-ok(diagnostic-value: temperature_c), bare-ok(ratio: moisture), bare-ok(diagnostic-value: insolation), bare-ok(diagnostic-value: elevation)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Substrate {
    /// Mean annual temperature, °C.
    pub temperature_c: f64,
    /// Moisture in `[0, 1]`.
    pub moisture: f64,
    /// Annual-mean top-of-atmosphere insolation, relative to the planet's
    /// global scalar (`hornvale_astronomy::insolation_rel`).
    pub insolation: f64,
    /// Terrain elevation scalar (meters-scale). Ocean cells included.
    pub elevation: f64,
}

/// Annual-mean top-of-atmosphere insolation at a latitude, relative to the
/// planet's global scalar `insolation_scalar`. Obliquity-aware: averages the
/// standard daily-mean insolation geometric factor (Milankovitch-cycle solar
/// forcing, the same formula climatology textbooks use for
/// latitude-by-latitude annual insolation) over `N` uniform samples of the
/// orbit — a static v1 approximation; seasonal `K(cell, t)` is deferred (The
/// Niche spec §8). All transcendentals route through `hornvale_kernel::math`
/// for cross-platform byte-identity (decision 0041).
/// type-audit: bare-ok(diagnostic-value: latitude_deg), bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: insolation_scalar), bare-ok(diagnostic-value: return)
pub fn annual_mean_insolation(
    latitude_deg: f64,
    obliquity_deg: f64,
    insolation_scalar: f64,
) -> f64 {
    let phi = latitude_deg.to_radians();
    let eps = obliquity_deg.to_radians();
    const N: u32 = 48;
    let mut sum = 0.0;
    for k in 0..N {
        let theta = 2.0 * std::f64::consts::PI * (k as f64) / (N as f64);
        // Solar declination at this orbital sample.
        let delta = math::asin(math::sin(eps) * math::sin(theta));
        // The clamp is load-bearing: at the poles `tan(phi)` is a huge
        // finite value (libm), so `cos_h0` saturates to ∓1 and `h0` resolves
        // to 0 (polar night) or π (polar day) — the physically correct
        // limit, with no pole special-casing needed.
        let cos_h0 = (-math::tan(phi) * math::tan(delta)).clamp(-1.0, 1.0);
        let h0 = math::acos(cos_h0);
        // Daily-mean insolation geometric factor.
        let f_k = (1.0 / std::f64::consts::PI)
            * (h0 * math::sin(phi) * math::sin(delta)
                + math::cos(phi) * math::cos(delta) * math::sin(h0));
        sum += f_k;
    }
    let geom = sum / (N as f64);
    insolation_scalar * geom
}

/// The per-cell substrate field for a built world's terrain/climate/sky — the
/// four environmental readings The Niche's habitat model scores each
/// species' condition niche against. Pure: draws nothing from the seed, only
/// the world's already-committed terrain/climate/sky.
///
/// Insolation is rotation-regime-aware (SKY-24): a `Spinning` world keeps
/// the latitude-only annual mean (byte-identical to the pre-fix path); a
/// `Locked` world instead rewards the substellar geometry — a Lambert
/// cosine off the substellar point, floored at `0.0` on the night side —
/// so the terminator ring, not the fixed noon point, reads as the
/// temperate band.
/// type-audit: bare-ok(diagnostic-value: obliquity_deg), bare-ok(ratio: insolation_scalar)
pub fn substrate_field(
    geo: &Geosphere,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    obliquity_deg: f64,
    insolation_scalar: f64,
    regime: &RotationRegime,
) -> hornvale_kernel::CellMap<Substrate> {
    hornvale_kernel::CellMap::from_fn(geo, |cell| Substrate {
        temperature_c: climate.mean_temperature_at(cell).get(),
        moisture: climate.moisture_at(cell),
        insolation: match regime {
            RotationRegime::Spinning { .. } => {
                annual_mean_insolation(geo.coord(cell).latitude, obliquity_deg, insolation_scalar)
            }
            RotationRegime::Locked => {
                insolation_scalar * hornvale_climate::substellar_cosine(geo.position(cell)).max(0.0)
            }
        },
        elevation: terrain.elevation_at(cell).get(),
    })
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
    /// The orbital phase offset at epoch, in turns (constant per world) —
    /// threaded through so a full climate rebuild (`glacial_maximum_habitable`)
    /// can retain it on `GeneratedClimate` just as `climate_of` does.
    year_phase_offset: f64,
    /// The world's own present-day ice mask (diagnosed from
    /// `paleoclimate_of`'s present-temperature field against [`FREEZE_C`],
    /// no albedo offset) — the baseline every era's advance is measured
    /// against.
    present_ice: &'a hornvale_kernel::CellMap<bool>,
    /// The absolute snowline threshold ([`FREEZE_C`], wrapped once).
    freeze: Temperature,
    /// The world seed — threaded through so `glacial_maximum_habitable`'s
    /// full climate rebuild derives the same weather seed `climate_of` would
    /// (climate is otherwise seed-free; this perturbs no existing draw).
    seed: hornvale_kernel::Seed,
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
        year_phase_offset: ctx.year_phase_offset,
        seed: ctx.seed,
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
    let terrain = terrain_of(world)?;
    paleoclimate_from(world, &terrain)
}

/// The deep-time era loop from a PRE-BUILT terrain — the body of
/// `paleoclimate_of` after its `terrain_of` line, taking the terrain the
/// caller already sculpted instead of re-deriving it. Byte-identical to
/// `paleoclimate_of` whenever `terrain` equals `terrain_of(world)`.
pub fn paleoclimate_from(
    world: &World,
    terrain: &GeneratedTerrain,
) -> Result<PaleoRecord, BuildError> {
    // Build the era-loop invariants exactly once (terrain, sky, and every
    // scalar/field derived from them) — see `EraContext`.
    let sky = sky_of(world)?;
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
    let (insolation, _obliquity_deg, regime, year_length_std, year_phase_offset) =
        stellar_inputs(&sky);

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
        year_phase_offset,
        present_ice: &present_ice,
        freeze,
        seed: world.seed,
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

/// The habitability series the deep-history bake replays. The bake's live
/// mask needs a full habitability field for EVERY era (a cell's effective
/// capacity is `capacity * factor(era)`, and `factor` reads both the era's
/// ice and its habitable mask) — but `paleoclimate_of` fills only the
/// glacial-maximum era's habitable field (every other era's is a discarded
/// placeholder, since `strata::extract` reads habitable at the peak alone).
/// This builds the series the bake actually needs:
///
/// - **Per-era habitability** is the cheap snowline diagnostic, not a full
///   climate rebuild: land above the era's own sea level whose absolute
///   temperature (that era's mean field plus its albedo-cooling offset) sits
///   at or above the freezing snowline. That is precisely the mask whose
///   swing drives displacement — a colder era pushes the snowline equatorward
///   and turns high-latitude cells hostile, forcing migration — and it costs
///   one mean-temperature field per era (no moisture/biome work), the same
///   order the coarse ice diagnostic already pays.
/// - **The day-axis is re-based onto the bake's `[start_year, end_year)`
///   window** (oldest era → `start_year`, present → `end_year`), so `bake`'s
///   `era_for` marches the glacial cycles forward across the simulated
///   millennia rather than seeing every era stamped in deep-negative time.
///
/// On the constant sky (no orbital forcing) there is no deep time: a single
/// present-era mask is returned and the bake sees a stable world (displacement
/// then arises only from crowding, never a mask swing).
///
/// The `ice` field is left empty on every era: the snowline is already folded
/// into `habitable` (an iced cell reads below-freezing, hence not habitable),
/// so `factor` gates purely on habitability and never double-counts ice.
fn bake_eras(
    world: &World,
    terrain: &GeneratedTerrain,
    cfg: &history_bake::BakeConfig,
) -> Result<Vec<EraClimate>, BuildError> {
    let sky = sky_of(world)?;
    let geo = terrain.geosphere();
    let elevation = terrain.globe().elevation.clone();
    let present_sea_level = terrain.sea_level();
    let (insolation, _obliquity_deg, regime, _year_length_std, _year_phase_offset) =
        stellar_inputs(&sky);
    let freeze = Temperature::new(FREEZE_C).expect("FREEZE_C is finite");

    // A cell is livable this era iff it is land above the era's sea level and
    // its absolute temperature is at or above the snowline.
    let livable_mask = |sea_level: ReferenceElevation,
                        offset: hornvale_kernel::TempAnomaly|
     -> hornvale_kernel::CellMap<bool> {
        let mean = hornvale_climate::temperature::mean_temperature(
            geo, &elevation, sea_level, insolation, &regime,
        );
        hornvale_kernel::CellMap::from_fn(geo, |c| {
            let elev = *elevation.get(c);
            elev >= sea_level && (*mean.get(c) + offset).get() >= freeze.get()
        })
    };

    // No forcing to replay (constant sky) → one present-era mask, no swing.
    let Some(system) = sky.system() else {
        let habitable = livable_mask(
            present_sea_level,
            hornvale_kernel::TempAnomaly::from_offset_c(0.0),
        );
        return Ok(vec![EraClimate {
            day: cfg.start_year,
            ice: hornvale_kernel::CellMap::from_fn(geo, |_| false),
            habitable,
            sea_level: present_sea_level,
            ice_fraction: 0.0,
        }]);
    };
    let forcing = &system.forcing;

    // Fine ice integration across the deep-time window — the identical
    // sampling `paleoclimate_from` performs (oldest → present), so the era
    // temperature offsets and eustatic sea levels the bake replays are the
    // same states the strata are extracted from.
    let n_steps = (DEEP_TIME_WINDOW_DAYS / ICE_STEP_DAYS).round() as usize;
    let mut samples: Vec<(f64, f64)> = Vec::with_capacity(n_steps + 1);
    for k in (0..=n_steps).rev() {
        let t = -(k as f64) * ICE_STEP_DAYS;
        let g = caloric_summer_index(
            forcing.obliquity_at(t),
            forcing.obliquity_mean,
            forcing.eccentricity_at(t),
            forcing.precession_at(t),
        );
        samples.push((t, g));
    }
    let history = integrate_ice(&samples);

    let mut eras: Vec<EraClimate> = Vec::with_capacity(CLIMATE_ERAS);
    for e in 0..CLIMATE_ERAS {
        let era_day = -DEEP_TIME_WINDOW_DAYS
            + (e as f64) * DEEP_TIME_WINDOW_DAYS / (CLIMATE_ERAS as f64 - 1.0);
        let state = history
            .iter()
            .min_by(|a, b| (a.day - era_day).abs().total_cmp(&(b.day - era_day).abs()))
            .expect("history is non-empty");
        let sea_level =
            ReferenceElevation::new(present_sea_level.get() + state.sea_level_change.get())
                .expect("present sea level plus a finite eustatic change is finite");
        let habitable = livable_mask(sea_level, state.temp_offset);
        // Re-base the deep-time era onto the bake window (oldest → start,
        // present → end), preserving order so `era_for` advances monotonically.
        let bake_day = cfg.start_year
            + (e as f64) * (cfg.end_year - cfg.start_year) / (CLIMATE_ERAS as f64 - 1.0);
        eras.push(EraClimate {
            day: bake_day,
            ice: hornvale_kernel::CellMap::from_fn(geo, |_| false),
            habitable,
            sea_level,
            ice_fraction: 0.0,
        });
    }
    Ok(eras)
}

/// Headline biome/habitability lines for the almanac's Land section.
/// type-audit: bare-ok(prose: return)
pub fn biome_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(biome_lines_from(&climate_of(world)?))
}

/// [`biome_lines`] from a PRE-BUILT climate — the body of `biome_lines`
/// without the internal `climate_of` re-derivation, so the almanac render
/// can share one climate across every Land-section accessor (The Single
/// Sculpt). Byte-identical to `biome_lines` for the same world's climate.
fn biome_lines_from(climate: &GeneratedClimate) -> Vec<String> {
    let summary = hornvale_climate::summarize(climate);
    let bands = match summary.band_count {
        Some(n) => format!("{n} circulation band(s) per hemisphere"),
        None => "a single day–night overturning (tidally locked)".to_string(),
    };
    vec![
        format!(
            "The air organizes into {bands}; {} land biomes and {} marine biomes cover the globe.",
            summary.land_biome_count, summary.marine_biome_count
        ),
        format!(
            "Some {:.0}% of the surface is habitable — land with water and a tolerable season.",
            summary.habitable_fraction * 100.0
        ),
    ]
}

/// The number of `day_fraction` samples used to find `diurnal_waveform`'s
/// peak for [`diurnal_lines`] — a one-time-per-render search, not the hot
/// generation path (mirrors the sampling `domains/climate`'s own tests use
/// to locate the same peak).
const DIURNAL_PEAK_SAMPLES: u32 = 200;

/// The diurnal-range headline lines for the almanac's Land section (The
/// Turning, spec §2): the peak-to-peak day/night swing at two sample
/// sites — the driest interior land cell (the globe's largest diurnal
/// amplitude) and the genuinely open-ocean cell (the globe's SMALLEST
/// diurnal amplitude among ocean cells — a coastal cell's nontrivial
/// continentality still gives it a small-but-nonzero swing, understating
/// the "small" claim the line makes) — so the reader sees both ends of the
/// range. Empty for tidally locked worlds and worlds with no land at all:
/// locked worlds have no rotation-scale day/night cycle (`temperature_at`'s
/// `Locked` branch never applies `diurnal_amp_at`).
/// type-audit: bare-ok(prose: return)
pub fn diurnal_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(diurnal_lines_from(&terrain_of(world)?, &climate_of(world)?))
}

/// [`diurnal_lines`] from a PRE-BUILT terrain and climate — the body without
/// the internal re-derivations, so the almanac render shares one terrain and
/// one climate (The Single Sculpt). Byte-identical to `diurnal_lines`.
fn diurnal_lines_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> Vec<String> {
    let RotationRegime::Spinning { day_std } = climate.regime() else {
        return Vec::new();
    };
    let geo = terrain.geosphere();
    let obliquity = climate.obliquity_deg();

    // Longitude 0.0: sweeping `day_fraction` over a full rotation sweeps all
    // local solar times regardless of longitude (a pure phase shift), so the
    // peak found here is longitude-invariant.
    let geo_peak_at = |latitude_deg: f64| -> f64 {
        (0..DIURNAL_PEAK_SAMPLES)
            .map(|i| {
                diurnal_waveform(
                    latitude_deg,
                    0.0,
                    obliquity,
                    0.0,
                    f64::from(i) / f64::from(DIURNAL_PEAK_SAMPLES),
                    day_std,
                )
            })
            .fold(f64::MIN, f64::max)
    };

    let mut driest: Option<(hornvale_kernel::CellId, f64)> = None;
    let mut ocean: Option<(hornvale_kernel::CellId, f64)> = None;
    for cell in geo.cells() {
        let amp = climate.diurnal_amp_at(cell);
        if terrain.is_ocean(cell) {
            // Genuinely open ocean is the MINIMUM-amplitude ocean cell — a
            // coastal cell's nontrivial continentality still gives it a
            // small-but-nonzero swing, understating the "small" claim.
            // `geo.cells()` is in CellId order, so `amp < best` (strict)
            // keeps the first-seen cell on ties, a deterministic tie-break.
            if ocean.is_none_or(|(_, best)| amp < best) {
                ocean = Some((cell, amp));
            }
            continue;
        }
        if driest.is_none_or(|(_, best)| amp > best) {
            driest = Some((cell, amp));
        }
    }

    let mut lines = Vec::new();
    if let Some((cell, amp)) = driest {
        let lat = geo.coord(cell).latitude;
        lines.push(hornvale_almanac::render_diurnal_range_line(
            "The driest interior",
            amp,
            geo_peak_at(lat),
        ));
    }
    if let Some((cell, amp)) = ocean {
        let lat = geo.coord(cell).latitude;
        lines.push(hornvale_almanac::render_diurnal_range_line(
            "The open ocean",
            amp,
            geo_peak_at(lat),
        ));
    }
    lines
}

/// Dot product a · b.
fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// The unit northward tangent at a cell, given its unit-sphere `position`
/// and eastward tangent: `normalize(cross(position, east))` — the same
/// (east, north) frame [`hornvale_scene`]'s `current_east`/`current_north`
/// project onto (The Gyre). Zero wherever `east` is zero (the poles).
fn tangent_north(position: [f64; 3], east: [f64; 3]) -> [f64; 3] {
    let n = [
        position[1] * east[2] - position[2] * east[1],
        position[2] * east[0] - position[0] * east[2],
        position[0] * east[1] - position[1] * east[0],
    ];
    let len = dot3(n, n).sqrt();
    if len < 1e-9 {
        [0.0, 0.0, 0.0]
    } else {
        [n[0] / len, n[1] / len, n[2] / len]
    }
}

/// The cardinal name for a current's dominant tangent-frame component:
/// whichever of `east`/`north` has the larger magnitude names the
/// direction, signed by its sign.
/// type-audit: bare-ok(ratio: east), bare-ok(ratio: north), bare-ok(identifier-text: return)
fn cardinal_current_direction(east: f64, north: f64) -> &'static str {
    if east.abs() >= north.abs() {
        if east >= 0.0 { "east" } else { "west" }
    } else if north >= 0.0 {
        "north"
    } else {
        "south"
    }
}

/// The seas' headline line for the almanac's Land section (The Gyre, spec
/// companion to The Turning): the dominant offshore current direction at a
/// coastal ocean sample site — the first ordinary coastal-fringe ocean cell
/// (>= 1 land neighbor, `CellId` order, the same coastal-walk convention
/// `domains/terrain`'s carve stage uses) carrying a nonzero current. Empty
/// for tidally locked worlds (no circulation bands to drive a current) and
/// for worlds with no such site (landless, or a coast whose current
/// happens to cancel to zero).
/// type-audit: bare-ok(prose: return)
pub fn seas_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(seas_lines_from(&terrain_of(world)?, &climate_of(world)?))
}

/// [`seas_lines`] from a PRE-BUILT terrain and climate — the body without the
/// internal re-derivations, so the almanac render shares one terrain and one
/// climate (The Single Sculpt). Byte-identical to `seas_lines`.
fn seas_lines_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> Vec<String> {
    let geo = terrain.geosphere();
    for cell in geo.cells() {
        if !terrain.is_ocean(cell) {
            continue;
        }
        if !geo.neighbors(cell).iter().any(|&n| !terrain.is_ocean(n)) {
            continue;
        }
        let current = climate.current_at(cell);
        if dot3(current, current) < 1e-12 {
            continue;
        }
        let east = hornvale_climate::circulation::wind_east_tangent(geo, cell);
        let north = tangent_north(geo.position(cell), east);
        let direction = cardinal_current_direction(dot3(current, east), dot3(current, north));
        return vec![format!(
            "The seas: a current runs {direction} along the coast."
        )];
    }
    Vec::new()
}

/// "rain" or "snow", by which phase carries the majority of the year's
/// precipitation at a site (The Rains): `>= 0.5` snow fraction reads as
/// snow, else rain.
/// type-audit: bare-ok(ratio: snow_fraction), bare-ok(identifier-text: return)
fn precip_phase_word(snow_fraction: f64) -> &'static str {
    if snow_fraction >= 0.5 { "snow" } else { "rain" }
}

/// The stable prose word for a seasonal precipitation regime (The Rains,
/// spec §2) — the same kebab-case labels the spec's model card names
/// (`hornvale_climate::precip_regime`'s four variants).
/// type-audit: bare-ok(identifier-text: return)
fn regime_word(regime: PrecipRegime) -> &'static str {
    match regime {
        PrecipRegime::Uniform => "uniform",
        PrecipRegime::SummerMax => "summer-max",
        PrecipRegime::WinterMax => "winter-max",
        PrecipRegime::Monsoon => "monsoon",
    }
}

/// The rains' headline lines for the almanac's Land section (The Rains,
/// spec §2/§7): annual precipitation, phase (rain/snow), and seasonal
/// regime at the same two sample sites `diurnal_lines` reports — the
/// driest interior land cell (highest diurnal amplitude, a continentality
/// proxy) and the open ocean cell (lowest diurnal amplitude among ocean
/// cells) — completing that section's thermal picture with the year's
/// moisture total. Unlike `diurnal_lines`, never empty: precipitation,
/// snow fraction, and regime are all defined regardless of rotation regime
/// (a tidally locked world's single day–night cell still carries a
/// moisture field and a `Uniform`-biased regime — `precip_regime`'s
/// `band == 0` default).
/// type-audit: bare-ok(prose: return)
pub fn rains_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(rains_lines_from(&terrain_of(world)?, &climate_of(world)?))
}

/// [`rains_lines`] from a PRE-BUILT terrain and climate — the body without
/// the internal re-derivations, so the almanac render shares one terrain and
/// one climate (The Single Sculpt). Byte-identical to `rains_lines`.
fn rains_lines_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> Vec<String> {
    let geo = terrain.geosphere();

    let mut driest: Option<(hornvale_kernel::CellId, f64)> = None;
    let mut ocean: Option<(hornvale_kernel::CellId, f64)> = None;
    for cell in geo.cells() {
        let amp = climate.diurnal_amp_at(cell);
        if terrain.is_ocean(cell) {
            // Same tie-break convention as `diurnal_lines`: strict `<`
            // keeps the first-seen (lowest `CellId`) cell on ties.
            if ocean.is_none_or(|(_, best)| amp < best) {
                ocean = Some((cell, amp));
            }
            continue;
        }
        if driest.is_none_or(|(_, best)| amp > best) {
            driest = Some((cell, amp));
        }
    }

    let mut lines = Vec::new();
    if let Some((cell, _)) = driest {
        lines.push(rains_line("The driest interior", climate, cell));
    }
    if let Some((cell, _)) = ocean {
        lines.push(rains_line("The open ocean", climate, cell));
    }
    lines
}

/// Below this annual total (mm/yr) a cell is effectively rainless, and its
/// seasonal-regime label — a categorical circulation classification computed
/// independently of the amount (spec §2) — carries no meaning: a "monsoon"
/// with no rain reads as a contradiction. The regime parenthetical is dropped
/// below this floor. 50 mm/yr is the conventional hyperarid boundary.
/// type-audit: bare-ok(threshold: mm/yr)
const REGIME_FLOOR_MM: f64 = 50.0;

/// Render one sample site's precipitation readout: annual mm, phase
/// (rain/snow), and — above the arid floor, where it means something — the
/// seasonal regime word (see [`rains_lines`] and [`REGIME_FLOOR_MM`]).
/// type-audit: bare-ok(prose: site), bare-ok(prose: return)
fn rains_line(site: &str, climate: &GeneratedClimate, cell: hornvale_kernel::CellId) -> String {
    let mm = climate.precip_at(cell).get();
    let phase = precip_phase_word(climate.snow_fraction_at(cell));
    // "about 0 mm" reads oddly for a cell that rounds to nothing; name the
    // bound instead.
    let amount = if mm < 1.0 {
        "under 1 mm".to_string()
    } else {
        format!("about {mm:.0} mm")
    };
    if mm < REGIME_FLOOR_MM {
        format!("{site} receives {amount} of {phase} a year.")
    } else {
        let regime = regime_word(climate.regime_at(cell));
        format!("{site} receives {amount} of {phase} a year ({regime}).")
    }
}

/// The plain-prose sky phrase for a weather state (The Firmament). `pub` so
/// both the almanac's headline lines ([`weather_line_for`]) and possession's
/// [`sky_report`] render the same weather vocabulary — one definition, no
/// drift between the two surfaces.
/// type-audit: bare-ok(prose: return)
pub fn sky_phrase(
    state: hornvale_climate::WeatherState,
    cloud: hornvale_climate::CloudType,
) -> &'static str {
    use hornvale_climate::{CloudType, WeatherState};
    match (state, cloud) {
        (WeatherState::Storm, _) => "torn by storm, towering thunderheads overhead",
        (WeatherState::Rain, _) => "a low grey rain-deck",
        (WeatherState::Overcast, _) => "a flat overcast",
        (WeatherState::Fair, _) => "fair, with scattered cumulus",
        (WeatherState::Clear, CloudType::Cirrus) => "clear but for high cirrus",
        (WeatherState::Clear, _) => "clear",
    }
}

/// The Firmament's headline lines for the almanac's Land section: the felt
/// sky at the same two sample sites the rains line uses (driest interior,
/// open ocean), on the almanac's reference day (0.0). Level 0 — a pure
/// observation.
/// type-audit: bare-ok(prose: return)
pub fn firmament_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(firmament_lines_from(
        &terrain_of(world)?,
        &climate_of(world)?,
    ))
}

/// [`firmament_lines`] from a PRE-BUILT terrain and climate (The Single
/// Sculpt).
fn firmament_lines_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> Vec<String> {
    let geo = terrain.geosphere();

    let mut driest: Option<(hornvale_kernel::CellId, f64)> = None;
    let mut ocean: Option<(hornvale_kernel::CellId, f64)> = None;
    for cell in geo.cells() {
        let amp = climate.diurnal_amp_at(cell);
        if terrain.is_ocean(cell) {
            if ocean.is_none_or(|(_, best)| amp < best) {
                ocean = Some((cell, amp));
            }
            continue;
        }
        if driest.is_none_or(|(_, best)| amp > best) {
            driest = Some((cell, amp));
        }
    }

    let mut lines = Vec::new();
    if let Some((cell, _)) = driest {
        lines.push(weather_line_for("The driest interior", climate, cell));
    }
    if let Some((cell, _)) = ocean {
        lines.push(weather_line_for("The open ocean", climate, cell));
    }
    lines
}

/// One sample site's weather line at day 0.0.
/// type-audit: bare-ok(prose: site), bare-ok(prose: return)
fn weather_line_for(
    site: &str,
    climate: &GeneratedClimate,
    cell: hornvale_kernel::CellId,
) -> String {
    let state = climate.weather_at(cell, 0.0);
    let cloud = climate.cloud_type_at(cell, 0.0);
    hornvale_almanac::render_weather_line(site, sky_phrase(state, cloud))
}

/// The deep-time headline lines for the almanac; empty when the world has no
/// glacial past.
/// type-audit: bare-ok(prose: return)
pub fn deep_time_lines(world: &World) -> Result<Vec<String>, BuildError> {
    deep_time_lines_from(world, &terrain_of(world)?)
}

/// [`deep_time_lines`] from a PRE-BUILT terrain — the body driving the
/// deep-time record off `paleoclimate_from` (one shared terrain) instead of
/// `paleoclimate_of`'s own re-sculpt (The Single Sculpt). Byte-identical to
/// `deep_time_lines`.
fn deep_time_lines_from(
    world: &World,
    terrain: &GeneratedTerrain,
) -> Result<Vec<String>, BuildError> {
    let record = paleoclimate_from(world, terrain)?;
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
/// the highest land above the sea, then the carve's derived point-
/// observation notables (Sculpting Task 11, spec §5) — waterfalls, deltas,
/// playas — the moment the provider reports at least one of a class: these
/// are sparse walk-scale landmarks, so "any" is the bar, not a share
/// (mirrors The Ground's `ground_lines` notable-emission pattern).
/// type-audit: bare-ok(prose: return)
pub fn land_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(land_lines_from(&terrain_of(world)?))
}

/// [`land_lines`] from a PRE-BUILT terrain — the body without the internal
/// `terrain_of` re-sculpt, so the almanac render shares one terrain (The
/// Single Sculpt). Byte-identical to `land_lines`.
fn land_lines_from(terrain: &GeneratedTerrain) -> Vec<String> {
    let summary = hornvale_terrain::summarize(terrain.globe());
    let mut lines = vec![
        format!(
            "The globe breaks into {} plates; the sea claims {:.0}% of its surface.",
            summary.plate_count,
            summary.ocean_fraction * 100.0
        ),
        format!(
            "The highest land stands {:.0} m above the sea.",
            summary.highest_elevation_m - summary.sea_level_m
        ),
    ];

    let mut notables = Vec::new();
    if !terrain.waterfalls().is_empty() {
        notables.push("the Great Falls".to_string());
    }
    if !terrain.deltas().is_empty() {
        notables.push("the Great Delta".to_string());
    }
    if !terrain.playas().is_empty() {
        notables.push("salt flats".to_string());
    }
    if !notables.is_empty() {
        lines.push(format!("Notable: {}.", notables.join(", ")));
    }
    lines
}

/// Human-readable rock-class name (The Ground, spec §4): lowercase, for
/// almanac prose and census categorical values — a pure naming projection,
/// no new draws.
/// type-audit: bare-ok(identifier-text: return)
pub fn rock_class_name(rock: hornvale_terrain::RockClass) -> &'static str {
    use hornvale_terrain::RockClass::*;
    match rock {
        Granite => "granite",
        Gabbro => "gabbro",
        Basalt => "basalt",
        Andesite => "andesite",
        Rhyolite => "rhyolite",
        Sandstone => "sandstone",
        Shale => "shale",
        Conglomerate => "conglomerate",
        Evaporite => "evaporite",
        Chert => "chert",
        Ironstone => "ironstone",
        ReefLimestone => "reef limestone",
        Coal => "coal",
        Slate => "slate",
        Schist => "schist",
        Gneiss => "gneiss",
        Marble => "marble",
        Quartzite => "quartzite",
        Alluvium => "alluvium",
    }
}

/// Human-readable soil-order name (The Ground, spec §4): lowercase, for
/// almanac prose and census categorical values.
/// type-audit: bare-ok(identifier-text: return)
pub fn soil_order_name(order: hornvale_terrain::SoilOrder) -> &'static str {
    use hornvale_terrain::SoilOrder::*;
    match order {
        Laterite => "laterite",
        Podzol => "podzol",
        Chernozem => "chernozem",
        Aridisol => "aridisol",
        Loam => "loam",
        Andosol => "andosol",
        Leptosol => "leptosol",
        Histosol => "histosol",
        Gley => "gley",
    }
}

/// Land-cell karst-hydrology share above which "karst country" is a notable
/// ground feature for the almanac (The Ground, spec §3/§6) — a chosen prose
/// threshold, not a physical constant.
const GROUND_KARST_NOTABLE: f64 = 0.05;
/// Land-cell andosol share above which "volcanic soils" is a notable ground
/// feature for the almanac.
const GROUND_ANDOSOL_NOTABLE: f64 = 0.1;

/// The ground's headline lines for the almanac: the dominant rock and soil
/// order over land, plus notable formations — karst country, salt flats,
/// volcanic soils (The Ground, spec §3/§4/§6). A pure projection over
/// existing terrain/climate fields: no new draws. Empty for a landless
/// world.
/// type-audit: bare-ok(prose: return)
pub fn ground_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(ground_lines_from(&terrain_of(world)?, &climate_of(world)?))
}

/// [`ground_lines`] from a PRE-BUILT terrain and climate — the body without
/// the internal re-derivations, so the almanac render shares one terrain and
/// one climate (The Single Sculpt). Byte-identical to `ground_lines`.
fn ground_lines_from(terrain: &GeneratedTerrain, climate: &GeneratedClimate) -> Vec<String> {
    let geo = terrain.geosphere();
    let soils = soil_of(terrain, climate, geo);

    let mut rocks: std::collections::BTreeMap<hornvale_terrain::RockClass, usize> =
        std::collections::BTreeMap::new();
    let mut orders: std::collections::BTreeMap<hornvale_terrain::SoilOrder, usize> =
        std::collections::BTreeMap::new();
    let (mut land, mut karst, mut andosol) = (0usize, 0usize, 0usize);
    let mut salt_flats = false;
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        land += 1;
        let rock = terrain.rock_at(cell);
        *rocks.entry(rock).or_insert(0) += 1;
        if terrain.hydro_at(cell) == hornvale_terrain::Hydro::Karst {
            karst += 1;
        }
        if terrain.is_endorheic(cell) && rock == hornvale_terrain::RockClass::Evaporite {
            salt_flats = true;
        }
        let order = *soils.get(cell);
        *orders.entry(order).or_insert(0) += 1;
        if order == hornvale_terrain::SoilOrder::Andosol {
            andosol += 1;
        }
    }
    if land == 0 {
        return Vec::new();
    }

    // Ties break to the lower-declared variant: RockClass/SoilOrder's `Ord`
    // exists precisely so callers can make a deterministic pick like this.
    let dominant_rock = rocks
        .iter()
        .max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(a.0)))
        .map(|(&r, _)| r)
        .expect("land > 0 guarantees at least one rock count");
    let dominant_soil = orders
        .iter()
        .max_by(|a, b| a.1.cmp(b.1).then(b.0.cmp(a.0)))
        .map(|(&s, _)| s)
        .expect("land > 0 guarantees at least one soil count");

    let mut lines = vec![format!(
        "The land is mostly {}, its soils mostly {}.",
        rock_class_name(dominant_rock),
        soil_order_name(dominant_soil)
    )];

    let mut notables = Vec::new();
    if karst as f64 / land as f64 > GROUND_KARST_NOTABLE {
        notables.push("karst country".to_string());
    }
    if salt_flats {
        notables.push("salt flats".to_string());
    }
    if andosol as f64 / land as f64 > GROUND_ANDOSOL_NOTABLE {
        notables.push("volcanic soils".to_string());
    }
    if !notables.is_empty() {
        lines.push(format!("Notable: {}.", notables.join(", ")));
    }
    lines
}

/// The Waters' headline line for the almanac (The Freshet, DOM-5 first
/// slice): the fresh-water (river) share of a world's land — the salt/fresh
/// distinction the ground substrate always computed but never reported. A
/// pure projection over `TectonicGlobe.water_kind`: no new draws. Empty for
/// a landless world.
/// type-audit: bare-ok(prose: return)
pub fn water_lines(world: &World) -> Result<Vec<String>, BuildError> {
    Ok(water_lines_from(&terrain_of(world)?))
}

/// [`water_lines`] from a PRE-BUILT terrain — the body without the internal
/// `terrain_of` re-sculpt, so the almanac render shares one terrain (The
/// Single Sculpt). Byte-identical to `water_lines`.
fn water_lines_from(terrain: &GeneratedTerrain) -> Vec<String> {
    let geo = terrain.geosphere();
    let globe = terrain.globe();
    let (mut land, mut fresh) = (0usize, 0usize);
    for cell in geo.cells() {
        if terrain.is_ocean(cell) {
            continue;
        }
        land += 1;
        if *globe.water_kind.get(cell) == hornvale_terrain::WaterKind::River {
            fresh += 1;
        }
    }
    if land == 0 {
        return Vec::new();
    }
    vec![format!(
        "Fresh water (rivers, including endorheic feeders bound for a salt sink) reaches {:.0}% of the land.",
        fresh as f64 / land as f64 * 100.0
    )]
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

/// The world's live phenomena sources: the sky (kept special/first — astronomy
/// is not yet migrated to the roster) followed by every domain's roster
/// contribution ([`Domain::phenomena_source`]). Today this yields exactly
/// `[sky, UniformClimate]` — the same set the old hardcoded fan-outs built.
/// [`observe`] re-sorts by salience with a kind→description tie-break, so the
/// order within the returned list never affects output bytes.
fn phenomena_sources(world: &World) -> Result<Vec<Box<dyn PhenomenaSource>>, BuildError> {
    phenomena_sources_from(world, &climate_of(world)?)
}

/// [`phenomena_sources`], but reusing an ALREADY-BUILT tier-1 climate rather
/// than re-deriving one. Deriving a `GeneratedClimate` runs the full terrain
/// sculpting pipeline plus the temperature/moisture/biome fields over the
/// whole globe (~O(cells), hundreds of ms) — so a genesis stage that already
/// holds the world's climate (the peopling stage builds it once) passes it
/// here instead of paying that cost per observation. Byte-identical to the
/// plain path: the reused climate is the same `climate_of(world)` value the
/// plain path derives (the peopling stage builds it with `climate_of`), and
/// the emitter reads only its fields.
fn phenomena_sources_from(
    world: &World,
    climate: &GeneratedClimate,
) -> Result<Vec<Box<dyn PhenomenaSource>>, BuildError> {
    let mut sources: Vec<Box<dyn PhenomenaSource>> = vec![Box::new(sky_of(world)?)];
    let mut ctx = WorldContext::new();
    // The tier-1 climate is composed from cross-domain inputs (terrain + sky),
    // so the composition root — the only layer where domains legally meet —
    // builds it and hands it to climate's domain to reclaim via `claim`
    // (layering-clean; see `WorldContext`). Climate refines, never contradicts,
    // the tier-0 stub (decision 0039): the AMBIENT claim still holds.
    ctx.provide("hornvale-climate", Box::new(climate.clone()));
    for domain in DOMAINS {
        if let Some(source) = domain.phenomena_source(world, &mut ctx) {
            sources.push(source);
        }
    }
    Ok(sources)
}

/// The tier-0/1/2 phenomena sources, observed from the world's first place —
/// the flagship (SEQ-4). The vantage's hemisphere culls the sky (SEQ-5).
/// type-audit: pending(wave-3: day)
pub fn observed_phenomena(world: &World, day: f64) -> Result<Vec<Phenomenon>, BuildError> {
    // The place-check short-circuits BEFORE any provider is built: a placeless
    // world (even one with a corrupt sky pin) returns Ok(empty) without ever
    // deriving climate/sky — the "no place, no phenomena" contract. So this
    // standalone accessor keeps its own body rather than eagerly building a
    // climate to delegate through `observed_phenomena_from_climate` (which the
    // almanac render, where a climate already exists, uses instead).
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let position = place_coord(world, place);
    let boxed = phenomena_sources(world)?;
    let sources: Vec<&dyn PhenomenaSource> = boxed.iter().map(|s| s.as_ref()).collect();
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

/// [`observed_phenomena`] from a PRE-BUILT climate — builds the phenomena
/// sources off the shared climate via [`phenomena_sources_from`] instead of
/// re-deriving one, so the almanac render shares a single climate (The Single
/// Sculpt). Byte-identical to `observed_phenomena` for the same world's
/// climate.
fn observed_phenomena_from_climate(
    world: &World,
    day: f64,
    climate: &GeneratedClimate,
) -> Result<Vec<Phenomenon>, BuildError> {
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    let position = place_coord(world, place);
    let boxed = phenomena_sources_from(world, climate)?;
    let sources: Vec<&dyn PhenomenaSource> = boxed.iter().map(|s| s.as_ref()).collect();
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

/// Resolve `species` to its canonical `'static` `KindId` label within `wc`,
/// or fail loudly with the known kinds. A kind's identity is its `KindId`, not
/// a god-struct — the composition root resolves names against the component
/// set the world was built from.
fn resolve_kind(wc: &WorldComponents, species: &str) -> Result<&'static str, BuildError> {
    wc.biosphere
        .ids()
        .map(|k| k.0)
        .find(|name| *name == species)
        .ok_or_else(|| {
            let known: Vec<&str> = wc.biosphere.ids().map(|k| k.0).collect();
            BuildError::Pins(format!(
                "unknown species '{species}'; roster: {}",
                known.join(", ")
            ))
        })
}

/// Whether a kind settles (forms villages) — the successor to the "has a
/// psyche entry" peoplehood proxy. Reads the universal `social_form`.
fn is_settled(wc: &WorldComponents, label: &str) -> bool {
    wc.biosphere
        .get_by_label(label)
        .is_some_and(|b| b.social_form == hornvale_species::SocialForm::Settled)
}

/// The phenomena a species (resolved within `wc`) observes.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_in(
    world: &World,
    wc: &WorldComponents,
    species: &str,
) -> Result<Vec<Phenomenon>, BuildError> {
    let name = resolve_kind(wc, species)?;
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    observed_phenomena_at(world, wc, name, place)
}

/// [`observed_phenomena_as_in`]'s actual observation with the entity's
/// committed coordinates as the vantage: the entity's own hemisphere culls
/// the sky (SEQ-5). An entity with no committed latitude/longitude (e.g. a
/// bare stand-in id) observes the whole, un-culled sky.
fn observed_phenomena_at(
    world: &World,
    wc: &WorldComponents,
    name: &'static str,
    place: EntityId,
) -> Result<Vec<Phenomenon>, BuildError> {
    observed_phenomena_from(world, wc, name, place, place_coord(world, place))
}

/// The phenomena a species (resolved within `wc`) observes from
/// `place`'s own committed vantage — its latitude/longitude fact culls the
/// sky by hemisphere (SEQ-5). This is the per-entity observation glossed
/// naming is truthful to (spec §9.3: a gloss composes THAT entity's own
/// site facts), public so the keystone (`cli/tests/words_identity.rs`) and
/// the lab's `name-gloss-true` metric can re-derive it independently
/// without importing worldgen's naming internals.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_at(
    world: &World,
    wc: &WorldComponents,
    species: &str,
    place: EntityId,
) -> Result<Vec<Phenomenon>, BuildError> {
    let name = resolve_kind(wc, species)?;
    observed_phenomena_at(world, wc, name, place)
}

/// [`observed_phenomena_as_at`], reusing an ALREADY-BUILT climate instead of
/// re-deriving one (which runs the full terrain-sculpting pipeline over the
/// globe). For a caller observing MANY places of one world — the lab's
/// `name-gloss-true` metric, one observation per settlement — this pays the
/// sculpt once (the caller's pre-built `climate`, e.g. a Lab view's
/// `climate()`) instead of once per place. Byte-identical to
/// [`observed_phenomena_as_at`]: the reused climate is the same
/// `climate_of(world)` value that path derives, threaded through
/// [`phenomena_sources_from`], and observation reads only its fields.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_at_from(
    world: &World,
    wc: &WorldComponents,
    species: &str,
    place: EntityId,
    climate: &GeneratedClimate,
) -> Result<Vec<Phenomenon>, BuildError> {
    let name = resolve_kind(wc, species)?;
    observed_phenomena_from_with_climate(world, wc, name, place, place_coord(world, place), climate)
}

/// [`observed_phenomena_as_in`], reusing an ALREADY-BUILT climate — the
/// world's-first-place counterpart of [`observed_phenomena_as_at_from`], for
/// the lab's per-species religion metrics (`pantheon-sig`, `epithet-honorific`).
/// Byte-identical to [`observed_phenomena_as_in`] for the same world's climate.
/// type-audit: bare-ok(identifier-text: species)
pub fn observed_phenomena_as_in_from(
    world: &World,
    wc: &WorldComponents,
    species: &str,
    climate: &GeneratedClimate,
) -> Result<Vec<Phenomenon>, BuildError> {
    let name = resolve_kind(wc, species)?;
    let Some(place) = hornvale_terrain::places(world).first().map(|p| p.id) else {
        return Ok(Vec::new());
    };
    observed_phenomena_from_with_climate(world, wc, name, place, place_coord(world, place), climate)
}

/// The shared core of the `_from` observers: [`observed_phenomena_from`] with
/// the phenomena sources built off an ALREADY-BUILT climate
/// ([`phenomena_sources_from`]) rather than re-derived per call. Byte-identical
/// to the per-call path — `phenomena_sources` is pure over the observed world,
/// and the reused climate equals the one that path would derive.
fn observed_phenomena_from_with_climate(
    world: &World,
    wc: &WorldComponents,
    name: &'static str,
    place: EntityId,
    position: Option<GeoCoord>,
    climate: &GeneratedClimate,
) -> Result<Vec<Phenomenon>, BuildError> {
    let boxed = phenomena_sources_from(world, climate)?;
    let sources: Vec<&dyn PhenomenaSource> = boxed.iter().map(|s| s.as_ref()).collect();
    observe_with_sources(world, wc, name, place, position, &sources)
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
    wc: &WorldComponents,
    name: &'static str,
    place: EntityId,
    position: Option<GeoCoord>,
) -> Result<Vec<Phenomenon>, BuildError> {
    // Build the sources for this single observation, then observe. A genesis
    // loop observing MANY times (per settlement, per deity) builds the
    // expensive sources ONCE and calls `observe_with_sources` directly —
    // byte-identical, since `phenomena_sources` is a pure function of a world
    // the loop does not mutate between observations.
    let boxed = phenomena_sources(world)?;
    let sources: Vec<&dyn PhenomenaSource> = boxed.iter().map(|s| s.as_ref()).collect();
    observe_with_sources(world, wc, name, place, position, &sources)
}

/// Observe from ALREADY-BUILT sources: the per-observation core, factored out
/// so a caller can construct the (expensive — a full `GeneratedClimate` over
/// the whole globe) phenomena sources once and reuse them across many
/// observations. Everything here is cheap: the kind's perception lens and its
/// characteristic hour. Byte-identical to building fresh sources per call,
/// because [`phenomena_sources`] is pure over the observed world.
fn observe_with_sources(
    world: &World,
    wc: &WorldComponents,
    name: &'static str,
    place: EntityId,
    position: Option<GeoCoord>,
    sources: &[&dyn PhenomenaSource],
) -> Result<Vec<Phenomenon>, BuildError> {
    // Source the kind's perception from the world's component set (ECS c3),
    // keyed by its `KindId` label.
    let perception = wc
        .perception
        .get(&KindId(name))
        .expect("peopled pass over a fauna kind");
    let day = observation_time(world, perception.activity)?;
    Ok(observe(
        sources,
        &ObserverContext {
            place,
            time: WorldTime { day },
            lens: perception_lens(perception),
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
    let wc = WorldComponents::assemble()?;
    observed_phenomena_as_in(world, &wc, species)
}

/// [`observed_phenomena_as`], reusing an ALREADY-BUILT climate (via
/// [`observed_phenomena_as_in_from`]) instead of re-deriving one — threaded
/// down the chorus `cyclic_beliefs_from` path so the census stops re-sculpting.
/// Byte-identical to `observed_phenomena_as`.
/// type-audit: bare-ok(identifier-text: species)
fn observed_phenomena_as_from(
    world: &World,
    species: &str,
    climate: &GeneratedClimate,
) -> Result<Vec<Phenomenon>, BuildError> {
    let wc = WorldComponents::assemble()?;
    observed_phenomena_as_in_from(world, &wc, species, climate)
}

/// Map a kind's (language-owned) articulation vector onto language's own
/// `Envelope` (spec §7): every scalar dimension is a direct 1:1 carry — both
/// share the same 0–1 scale and semantics — and `ExoticManner` maps onto
/// `ExoticSeg` one variant at a time. The composition root now sources the
/// articulation from a roster-derived `WorldComponents` (already the language
/// type), so no species→language conversion remains here.
pub fn envelope_of(art: &hornvale_language::ArticulationVector) -> hornvale_language::Envelope {
    hornvale_language::Envelope {
        labiality: art.labiality,
        vowel_space: art.vowel_space,
        voicing: art.voicing,
        sibilance: art.sibilance,
        voice_loudness: art.voice_loudness,
        tonality: art.tonality,
        exotic: match art.exotic {
            hornvale_language::ExoticManner::None => hornvale_language::ExoticSeg::None,
            hornvale_language::ExoticManner::Trill => hornvale_language::ExoticSeg::Trill,
            hornvale_language::ExoticManner::Click => hornvale_language::ExoticSeg::Click,
            hornvale_language::ExoticManner::Ejective => hornvale_language::ExoticSeg::Ejective,
        },
    }
}

/// Draw `species`' phonology from this world's seed and its authored
/// articulation vector, resolving `species` within `wc` — rebuildable
/// from (seed, species, envelope) alone, the same reconstruction idiom as
/// `terrain_of`/`sky_of`/`climate_of`. The single construction site for a
/// species' `Phonology`. Panics if `species` is not a speaking kind in `wc`;
/// every caller sources `species` from the same component set it passes here.
/// type-audit: bare-ok(identifier-text: species)
pub fn language_of_in(
    world: &World,
    wc: &WorldComponents,
    species: &str,
) -> hornvale_language::Phonology {
    // The kind's phonology is sourced from the world's component set (the
    // language-typed articulation) — a synthetic roster carries its own
    // translated values (ECS c3). `resolve_kind` resolves `species` to its
    // `'static` `KindId` label, the seed-derivation key.
    let name = resolve_kind(wc, species).unwrap_or_else(|e| panic!("language_of_in: {e}"));
    language_of_wc(world, wc, name)
}

/// [`language_of_in`]'s phonology draw against an ALREADY-built component set
/// (ECS c3): read the kind's language-typed articulation from `wc` rather
/// than rebuild the set from a roster per call, so a build threads its single
/// `wc`. `name` is the kind's `KindId` key (its `'static` species name).
/// Byte-identical to `language_of_in` on any roster whose component set is
/// `wc`. Panics if `name` is not a speaking kind in `wc`.
fn language_of_wc(
    world: &World,
    wc: &WorldComponents,
    name: &'static str,
) -> hornvale_language::Phonology {
    let art = wc.articulation.get(&KindId(name)).unwrap_or_else(|| {
        panic!("language_of_wc: '{name}' is not a speaking kind in the component set")
    });
    hornvale_language::draw_phonology(&world.seed, name, &envelope_of(art))
}

/// Draw a species' phonology, resolving `species` within the shipped
/// default roster.
/// type-audit: bare-ok(identifier-text: species)
pub fn language_of(world: &World, species: &str) -> hornvale_language::Phonology {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    language_of_in(world, &wc, species)
}

/// Draw a `family`'s proto phonology from this world's seed and the
/// family's authored proto ancestral vector
/// ([`hornvale_language::family_proto`]) — the family name occupies the
/// species slot in the seed-derivation (e.g. `draw_phonology(seed,
/// "goblinoid", env)`), a language with no speakers of its own, only
/// daughters. Panics if `family` is not in `family_proto` (a singleton
/// family has no entry there and never reaches this function — see
/// `lexicon_of`'s resolution).
/// type-audit: bare-ok(identifier-text: family)
pub fn proto_phonology_of(world: &World, family: &'static str) -> hornvale_language::Phonology {
    // The convenience entry: assemble the canonical component set (the sole
    // construction point for `family_proto`) and delegate to the wc-threaded
    // read path, so the proto vector is always sourced from `wc.family_proto`.
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    proto_phonology_of_in(world, &wc, family)
}

/// [`proto_phonology_of`]'s proto draw against an ALREADY-built component set
/// (ECS c3): read the family's proto ancestral vector from `wc.family_proto`
/// — the composed proto store, `worldgen`'s sole proto read path — rather than
/// re-fetch language's global `family_proto()` per call. A singleton family
/// has no entry there and never reaches this function (see `lexicon_of`'s
/// resolution). Panics if `family` is not in `wc.family_proto`.
fn proto_phonology_of_in(
    world: &World,
    wc: &WorldComponents,
    family: &'static str,
) -> hornvale_language::Phonology {
    let art = wc
        .family_proto
        .get(&KindId(family))
        .unwrap_or_else(|| panic!("proto_phonology_of: family '{family}' has no proto vector"));
    hornvale_language::draw_phonology(&world.seed, family, &envelope_of(art))
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
    let wc = WorldComponents::assemble()?;
    let name = resolve_kind(&wc, species)?;
    let terrain = terrain_of(world)?;
    let climate = climate_of(world)?;
    let settled = settled_cells(world, species);
    // `exposure_of_impl` alone owns the "coexisting counts only once the
    // querying species has settled" rule; the outer gate this replaced was
    // vestigial belt-and-suspenders from the merge reconciliation.
    let coexisting = placed_species(world);
    exposure_of_impl(world, &wc, name, &settled, &coexisting, &terrain, &climate)
}

/// [`exposure_of`], reusing ALREADY-BUILT terrain and climate instead of
/// re-sculpting them (`exposure_of` runs the terrain pipeline twice — once
/// directly, once inside its `climate_of`). Byte-identical: the sole
/// difference is that the passed terrain/climate replace `terrain_of(world)`/
/// `climate_of(world)`, which a Lab view's `terrain()`/`climate()` equal by
/// construction; every other input (the assembled roster, the settled/
/// coexisting cells) is derived exactly as `exposure_of` derives it. Threaded
/// down the `lexicon_from` chain so the census's ~14 lexicon metrics stop
/// re-sculpting per call.
/// type-audit: bare-ok(identifier-text)
fn exposure_from(
    world: &World,
    species: &str,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<std::collections::BTreeMap<String, hornvale_language::ExposureClass>, BuildError> {
    let wc = WorldComponents::assemble()?;
    let name = resolve_kind(&wc, species)?;
    let settled = settled_cells(world, species);
    let coexisting = placed_species(world);
    exposure_of_impl(world, &wc, name, &settled, &coexisting, terrain, climate)
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
    wc: &WorldComponents,
    name: &'static str,
    settled: &[hornvale_kernel::CellId],
    coexisting: &std::collections::BTreeSet<String>,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<std::collections::BTreeMap<String, hornvale_language::ExposureClass>, BuildError> {
    use hornvale_language::{
        ExposureClass, GapReason, body_pack, color_pack, in_ladder, kin_pack, universal_stratum,
    };

    let species = name;
    // Source perception from the world's component set (ECS c3), keyed by the
    // kind's `KindId` label.
    let perception = wc
        .perception
        .get(&KindId(name))
        .expect("peopled pass over a fauna kind");
    let depths = pack_depths(perception);
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
                    perception.night_vision,
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
    let wc = WorldComponents::assemble()?;
    lexicon_of_in(world, &wc, species)
}

/// The family's members (all kinds in `wc` sharing `family`), each as a
/// [`hornvale_language::Daughter`] — its drawn cascade and its own phonology —
/// so the merger-aware proto assignment (epoch `root/v3`) can choose core roots
/// that survive every daughter's descent distinct. The rejection is
/// order-independent (a candidate is rejected iff it merges in ANY daughter),
/// so the iteration order does not affect the result; a singleton family yields
/// a one-element slice (itself). Public so the proto-goblinoid reference page
/// and the Lab's monophyly/outgroup metrics can reproduce the SAME merger-aware
/// assignment `build_lexicon` consumes.
/// type-audit: bare-ok(identifier-text: family)
pub fn family_daughters(
    world: &World,
    wc: &WorldComponents,
    family: &str,
) -> Vec<hornvale_language::Daughter> {
    // Iterate the family-taxonomy store in ascending `KindId` — the same order
    // (and same members) the default roster's `registry()`-key traversal gave.
    wc.family_of
        .iter()
        .filter(|(_, fam)| **fam == family)
        .map(|(kind, _)| hornvale_language::Daughter {
            cascade: hornvale_language::draw_cascade(&world.seed, kind.0),
            phonology: language_of_wc(world, wc, kind.0),
        })
        .collect()
}

/// Build `species`' lexicon within an explicit component set `wc` — the
/// merger-aware composition-root path. Assembles the family's daughters so the
/// proto assignment drives core homophony to zero across the whole family.
/// type-audit: bare-ok(identifier-text: species)
pub fn lexicon_of_in(
    world: &World,
    wc: &WorldComponents,
    species: &str,
) -> Result<hornvale_language::Lexicon, BuildError> {
    let ph = language_of_in(world, wc, species);
    let exposures = exposure_of(world, species)?;
    let name = resolve_kind(wc, species)?;
    let family = *wc
        .family_of
        .get(&KindId(name))
        .expect("every kind has a family row (integrity-checked)");
    // A family with more than one member has a proto ancestral vector in the
    // canonical family-proto store and draws a real shared proto phonology; a
    // singleton family (e.g. kobold) is absent there, so it stays its own
    // family label and its own phonology stands in for its proto — the
    // pre-family draw, preserved exactly.
    let (fam_label, proto_ph) = match wc.family_proto.get(&KindId(family)) {
        Some(_) => (family, proto_phonology_of_in(world, wc, family)),
        None => (name, ph.clone()),
    };
    let daughters = family_daughters(world, wc, family);
    Ok(hornvale_language::build_lexicon(
        &world.seed,
        name,
        fam_label,
        &ph,
        &proto_ph,
        &exposures,
        &daughters,
    ))
}

/// [`lexicon_of_in`], reusing ALREADY-BUILT terrain and climate down the
/// exposure step ([`exposure_from`]) instead of re-sculpting the globe. Only
/// the `exposure_*` line differs from `lexicon_of_in`; the draw order (`ph`
/// before exposure, exactly as `lexicon_of_in`) is preserved so the seed
/// stream is consumed identically. Byte-identity pinned by the census A/B and
/// the `lexicon_from_matches_lexicon_of` test.
/// type-audit: bare-ok(identifier-text: species)
fn lexicon_of_in_from(
    world: &World,
    wc: &WorldComponents,
    species: &str,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<hornvale_language::Lexicon, BuildError> {
    let ph = language_of_in(world, wc, species);
    let exposures = exposure_from(world, species, terrain, climate)?;
    let name = resolve_kind(wc, species)?;
    let family = *wc
        .family_of
        .get(&KindId(name))
        .expect("every kind has a family row (integrity-checked)");
    let (fam_label, proto_ph) = match wc.family_proto.get(&KindId(family)) {
        Some(_) => (family, proto_phonology_of_in(world, wc, family)),
        None => (name, ph.clone()),
    };
    let daughters = family_daughters(world, wc, family);
    Ok(hornvale_language::build_lexicon(
        &world.seed,
        name,
        fam_label,
        &ph,
        &proto_ph,
        &exposures,
        &daughters,
    ))
}

/// [`lexicon_of`], reusing ALREADY-BUILT terrain and climate (a Lab view's
/// `terrain()`/`climate()`) so the census's many lexicon metrics stop
/// re-sculpting the globe per call — the terrain sculpt was ~80% of the
/// post-name-gloss census cost, almost all of it here. Byte-identical to
/// `lexicon_of` (same assembled roster, same draw order).
/// type-audit: bare-ok(identifier-text: species)
pub fn lexicon_from(
    world: &World,
    species: &str,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Result<hornvale_language::Lexicon, BuildError> {
    let wc = WorldComponents::assemble()?;
    lexicon_of_in_from(world, &wc, species, terrain, climate)
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
    /// Per-species base seed for deity name generation (`/v2` epoch): the name
    /// seed is derived from this + phenomenon kind + rank, never an entity id.
    deity_seed: Seed,
}

/// The seed for a deity's generated name: a pure function of the per-species
/// deity seed (`base`), the phenomenon KIND the deity is of, and the
/// phenomenon's RANK among its pantheon's members. Deliberately carries no
/// entity id, so deity names are invariant to entity mint order — the fix
/// for the `/v2` naming epoch (spec §8).
fn deity_name_seed(base: Seed, kind: &str, rank: usize) -> u64 {
    base.derive(StreamLabel::dynamic(kind))
        .derive(StreamLabel::dynamic(&rank.to_string()))
        .stream()
        .next_u64()
}

/// The per-species base seed every deity/epithet name for `species` derives
/// from (the `/v2` epoch label). The one place the `"religion/deity/v2"`
/// stream label is spelled, so [`build`] (constructing a
/// [`LanguageDeityNamer`]) and [`deity_name_seed_for`] (re-deriving the same
/// seed from outside this crate) can never diverge.
fn deity_base_seed(world_seed: &Seed, species: &str) -> Seed {
    world_seed
        .derive(streams::RELIGION_DEITY_V2)
        .derive(StreamLabel::dynamic(species))
}

/// Public entry point onto [`deity_name_seed`] for consumers outside this
/// crate that need to re-derive a committed deity/epithet name's seed from
/// the world seed directly (`windows/lab`'s `epithet_honorific` metric
/// structurally detects the honorific affix by re-deriving the plain word
/// the committed epithet was built from).
/// type-audit: bare-ok(identifier-text: species), bare-ok(identifier-text: kind), bare-ok(index: rank), pending(wave-3: return)
pub fn deity_name_seed_for(world_seed: &Seed, species: &str, kind: &str, rank: usize) -> u64 {
    deity_name_seed(deity_base_seed(world_seed, species), kind, rank)
}

impl hornvale_religion::DeityNamer for LanguageDeityNamer<'_, '_, '_> {
    fn deity(&mut self, salt: u64) -> (String, String) {
        let rank = self.index;
        let phenomenon = self
            .phenomena
            .get(rank)
            .expect("religion calls deity() once per member phenomenon, in phenomena order");
        self.index += 1;
        let sentiment = hornvale_religion::Sentiment::of(phenomenon);
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts {
            concepts: &concepts,
        };
        let name_seed = deity_name_seed(self.deity_seed, &phenomenon.kind, rank);
        let (g, gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Deity,
            name_seed,
            &self.morph,
            &site,
            self.lexicon,
        );
        if !gloss.is_empty() {
            self.glosses.insert(salt, gloss);
        }
        (g.roman, g.ipa)
    }

    fn epithet(&mut self, _salt: u64, sentiment: hornvale_religion::Sentiment) -> (String, String) {
        // Sentiment fits the epithet at render time (Task 11's `render_line`
        // reads it from the belief's own committed `sentiment` fact); the
        // generated word itself is glossed like any other name, over the
        // same phenomenon `deity()` just named for this same belief — only
        // the deity's own gloss is committed as a `name-gloss` fact (see
        // `glosses`), so the epithet's gloss is computed and discarded.
        let rank = self.index - 1;
        let phenomenon = self
            .phenomena
            .get(rank)
            .expect("deity() always runs before epithet() for the same belief");
        let concepts = deity_site_concepts(phenomenon, sentiment);
        let site = hornvale_language::SiteConcepts {
            concepts: &concepts,
        };
        let name_seed = deity_name_seed(self.deity_seed, &phenomenon.kind, rank);
        let (g, _gloss) = self.namer.glossed_name(
            hornvale_language::NameKind::Epithet,
            name_seed,
            &self.morph,
            &site,
            self.lexicon,
        );
        (g.roman, g.ipa)
    }
}

/// Build a complete world against a given component set `wc` (the world's
/// kinds): mint the world entity and record its sky choice and scenario pins
/// first; run sky genesis for `Generated`; commit the terrain pins and run
/// tectonic genesis; then assemble per-cell site inputs from terrain and
/// climate, place a spaced scatter of settlements (honoring the settlement
/// pins' suitability floor), commit each as its own place entity, and run the
/// culture/religion cascade on the flagship (the most-suitable settlement,
/// placed first) from its actual environment. The default world passes
/// [`WorldComponents::assemble`]; Lab passes a synthetic solo set.
pub fn build_world_from_components(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
    wc: &WorldComponents,
) -> Result<World, BuildError> {
    build_to(
        seed,
        pins,
        sky,
        terrain_pins,
        settlement_pins,
        wc,
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
    wc: &WorldComponents,
    depth: BuildDepth,
) -> Result<World, BuildError> {
    build_to(seed, pins, sky, terrain_pins, settlement_pins, wc, depth)
}

/// The full pipeline, run only as deep as `depth`. `build_world_from_components`
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
    wc: &WorldComponents,
    depth: BuildDepth,
) -> Result<World, BuildError> {
    let mut world = World::new(seed);
    register_all(&mut world.registry)?;
    // ecs-c6 T3, spec §7 as a load-time gate: every functional predicate the
    // genesis pipeline commits has exactly one declared writer, except
    // shared kernel-core infrastructure (`hornvale_kernel::NAME_GLOSS`
    // et al.), which `KERNEL_CORE_PREDICATES` exempts — see
    // `schedule::genesis_systems`'s doc comment for why `name-gloss`
    // specifically has two real, disjoint-subject writers.
    schedule::genesis_systems()
        .single_writer_check(&world.registry, hornvale_kernel::KERNEL_CORE_PREDICATES)
        .map_err(BuildError::Schedule)?;

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

    // Sculpt the terrain ONCE here and KEEP it: the same `GeneratedTerrain`
    // construction `terrain_of` performs (same geosphere level from the same
    // pins, same `generate` call, same `GeneratedTerrain::new`), so the value
    // kept is byte-identical to what `terrain_of(&world)` would return — but
    // built a single time and threaded through the climate/settlement and
    // deep-time stages below instead of re-derived (The Single Sculpt).
    let terrain = stage("terrain", || -> Result<GeneratedTerrain, BuildError> {
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
        let geo = geosphere_for(level);
        let terrain_outcome = hornvale_terrain::generate(seed, &geo, terrain_pins)
            .map_err(BuildError::TerrainGenesis)?;
        hornvale_terrain::facts::genesis(&mut world, world_entity, &terrain_outcome)?;
        Ok(GeneratedTerrain::new(geo, terrain_outcome))
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

    // The world's component set `wc` (ECS c3) is supplied by the caller: every
    // mind (psyche, perception), speech (articulation, lexicon, family proto),
    // biosphere, and family-taxonomy read below sources from it, keyed by a
    // kind's `KindId` label. The default world passes the canonical
    // `assemble()`; a synthetic Lab roster passes its own composed set.

    // Reconstruct terrain + climate, build each species' carrying-capacity
    // field, condense settlements off it (demography), and commit each as its
    // own place entity.
    #[allow(clippy::type_complexity)]
    let (terrain, climate, placements, lexicons, species_set, phonologies) = stage(
        "climate+settlements",
        || -> Result<
            (
                GeneratedTerrain,
                GeneratedClimate,
                Vec<HistoryPlacement>,
                std::collections::BTreeMap<&str, hornvale_language::Lexicon>,
                Vec<&'static str>,
                std::collections::BTreeMap<&str, hornvale_language::Phonology>,
            ),
            BuildError,
        > {
    // `terrain` here is the pre-built value hoisted from the `"terrain"` stage
    // (moved into this closure), not a fresh `terrain_of(&world)` re-sculpt;
    // climate is built once off it. Both are byte-identical to the prior
    // `terrain_of`/`climate_of` re-derivations (The Single Sculpt).
    let climate = climate_from(&world, &terrain)?;
    let geo = terrain.geosphere();

    // Which species this world places: the whole roster, or the pinned one.
    // Settlement genesis is a peopled-only pass (only settling, speaking
    // species place villages); a biosphere-only (fauna) kind is filtered out
    // here, at the one point `species_set` is assembled, so every pass below
    // that reads `.psych`/`.perception`/`.articulation`/etc via `peopled(def)`
    // never sees a fauna kind. The unpinned path's roster is all-peopled, so
    // that filter is a no-op today (byte-identical). A `--species` pin,
    // though, can now name one of the Task 4 menagerie's fauna kinds — those
    // never settle, so pinning one must fail loudly with the physical reason
    // (constitution: "pins fail loudly") rather than reach `peopled(def)`
    // downstream and panic.
    // The peopled kinds are exactly the settling ones — `social_form ==
    // Settled` (ascending `KindId`, `wc.biosphere`'s natural `BTreeMap`
    // order) — the same peopled subset, in the same order, the psyche
    // store's key-set gave before this re-key (byte-identical today).
    let species_set: Vec<&'static str> = match &settlement_pins.species {
        None => wc
            .biosphere
            .iter()
            .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
            .map(|(k, _)| k.0)
            .collect(),
        Some(name) => {
            let resolved = resolve_kind(wc, name)?;
            if !is_settled(wc, resolved) {
                return Err(BuildError::Pins(format!(
                    "'{name}' is not a settling people (a biosphere-only fauna kind)"
                )));
            }
            vec![resolved]
        }
    };

    // The epoch (The Living Community): the deep-history bake, not demography
    // stack-condensation, is now the settlement provider. The bake seeds an
    // ancient world, marches epochs across the paleoclimate era-variance, and
    // resolves the whole occupation skeleton; `emit_history` commits it — an
    // occupation still alive at `now` becomes an `is-settlement` (with its
    // `population`/`cell-id`), a dead one an `is-ruin`. The retired
    // `coexist::pack`/`condense_stack` placer is gone from genesis; the same
    // niche-differentiated stack still lives in `demography_report` for the
    // Lab's coexistence-stack readout, which this rewire leaves untouched.
    //
    // The bake reads three composition-root fields: the shared base carrying
    // capacity (`carrying_inputs_of` → `demography::carrying_capacity`, the
    // Confluence's freshwater-near-rivers term riding it), the paleoclimate
    // habitability series it replays (`bake_eras` — per-era snowline masks,
    // day-axis re-based onto the bake window), and the refugia mask
    // (`PaleoRecord.refugia`, habitable through the glacial maximum — the
    // migration preference). The peopled roster is `species_set` in its
    // `KindId` order. Same seed + pins ⇒ byte-identical `History` ⇒
    // byte-identical committed skeleton (the bake draws only under the
    // isolated `history/genesis/<people>` and `history/bake` streams).
    let suitability = hornvale_demography::carrying_capacity(
        geo,
        &carrying_inputs_of(geo, &terrain, &climate),
    );
    // `carrying_capacity` is a dimensionless suitability (~[0, 1.7]); the bake
    // reasons in headcounts (`pressure = population / eff_capacity`, genesis
    // pop 10). Scale the suitability into a per-cell headcount capacity so a
    // maximal-suitability cell supports ~a few hundred settlers and genesis
    // communities start comfortable rather than instantly over-pressure.
    // Tuned (with the genesis founding density below) to land seed-42's live
    // settlement count in the walkable band — the quality gate in
    // `tests/history_placement.rs`.
    let capacity =
        hornvale_kernel::CellMap::from_fn(geo, |c| *suitability.get(c) * SETTLERS_PER_CAPACITY);
    // River proximity, exposed as a DISTINCT bake weighting factor (Task 5b):
    // the same field `carrying_inputs_of` folds into K, passed separately so
    // the bake's site-picking paths (genesis, daughter founding, migration)
    // can bias toward fresh water directly — restoring The Confluence's
    // near-river condensation, which the epoch (Task 5a) diluted when
    // daughter/climate spreading chose cells without regard to rivers.
    let water_kind = hornvale_kernel::CellMap::from_fn(geo, |c| terrain.water_kind_at(c));
    let river_prox =
        hornvale_terrain::river_proximity(geo, &water_kind, hornvale_terrain::RIVER_REACH);
    let paleo = paleoclimate_from(&world, &terrain)?;
    let cfg = history_bake::BakeConfig::default_millennia();
    let eras = bake_eras(&world, &terrain, &cfg)?;
    let peoples: Vec<KindId> = species_set.iter().map(|&n| KindId(n)).collect();
    let history = history_bake::bake(
        seed,
        geo,
        &capacity,
        &river_prox,
        &eras,
        &paleo.refugia,
        &peoples,
        &cfg,
    );
    emit_history(&mut world, &history)?;
    // Commit the bake's `end_year` as the world's "now" (T8 review gap): the
    // present isn't the latest occupation event (a stochastic bake rarely
    // lands its last draw exactly on the boundary) — it's this fixed
    // scenario constant. `present_day` (windows/almanac) reads it back.
    history_emit::emit_now(&mut world, world_entity, cfg.end_year)?;

    // Pair each alive settlement `emit_history` just committed (tagged
    // `is-settlement` + `occ-people` + `cell-id` + `population`) back to its
    // build-local `species_set` tag, for the naming/culture/religion passes
    // below. Collect owned data first so the immutable ledger borrow is
    // released before those passes commit again. Commit order (records order,
    // alive filtered) is deterministic, so this vec is stable across builds.
    let placements: Vec<HistoryPlacement> = {
        let raw: Vec<(EntityId, String, hornvale_kernel::CellId, u32)> = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .map(|f| f.subject)
            .map(|id| {
                let people = world
                    .ledger
                    .text_of(id, hornvale_history::OCC_PEOPLE)
                    .expect("a history settlement carries occ-people")
                    .to_string();
                let cell = match world.ledger.value_of(id, hornvale_settlement::CELL_ID) {
                    Some(Value::Number(n)) => hornvale_kernel::CellId(*n as u32),
                    _ => unreachable!("a history settlement carries a numeric cell-id"),
                };
                let population = match world.ledger.value_of(id, hornvale_settlement::POPULATION)
                {
                    Some(Value::Number(n)) => *n as u32,
                    _ => unreachable!("a history settlement carries a numeric population"),
                };
                (id, people, cell, population)
            })
            .collect();
        raw.into_iter()
            .map(|(id, people, cell, population)| {
                let tag = species_set
                    .iter()
                    .position(|&n| n == people)
                    .expect("occ-people is one of the seeded peoples");
                HistoryPlacement {
                    id,
                    tag,
                    cell,
                    population,
                }
            })
            .collect()
    };

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
        .map(|&name| (name, language_of_wc(&world, wc, name)))
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
    for (tag, &name) in species_set.iter().enumerate() {
        // Each occupation is single-people (history places one community per
        // site), so a species is "settled" at exactly the cells its own alive
        // occupations sit on — the `people`-tagged settlements this build
        // emitted. `exposure_of_impl` alone owns the "coexisting counts only
        // once the querying species has settled" rule; `coexisting_now` spans
        // every people that placed any alive settlement this build.
        let settled_now: Vec<hornvale_kernel::CellId> = placements
            .iter()
            .filter(|s| s.tag == tag)
            .map(|s| s.cell)
            .collect();
        let coexisting_now: std::collections::BTreeSet<String> = placements
            .iter()
            .map(|s| species_set[s.tag].to_string())
            .collect();
        let exposures = exposure_of_impl(
            &world,
            wc,
            name,
            &settled_now,
            &coexisting_now,
            &terrain,
            &climate,
        )?;
        let ph = phonologies
            .get(name)
            .expect("a phonology was built for every placed species");
        let family = *wc
            .family_of
            .get(&KindId(name))
            .expect("every placed kind has a family row (integrity-checked)");
        // A family with more than one member has a proto ancestral vector
        // in the canonical family-proto store and draws a real shared proto
        // phonology; a singleton family (e.g. kobold) is absent there, so it
        // stays its own family label and its own phonology stands in for its
        // proto — the pre-family draw, preserved exactly.
        let (fam_label, proto_ph) = match wc.family_proto.get(&KindId(family)) {
            Some(_) => (family, proto_phonology_of_in(&world, wc, family)),
            None => (name, ph.clone()),
        };
        let daughters = family_daughters(&world, wc, family);
        lexicons.insert(
            name,
            hornvale_language::build_lexicon(
                &seed, name, fam_label, ph, &proto_ph, &exposures, &daughters,
            ),
        );
    }

    // Enrich each history-emitted settlement entity with its descriptive
    // facts (name, place tag, biome, coordinates). `emit_history` already
    // committed the structural skeleton (`is-settlement`, `population`,
    // `cell-id`, and the occupation facts); the naming pass adds the
    // descriptors onto the SAME entities so history stays the sole placer (the
    // retired `settlement::genesis` used to mint these together with
    // placement). Names are pure functions of seed+species+kind+cell-salt
    // (pin-isolated), so a shifted settlement set perturbs no other draw.
    // Build the phenomena sources ONCE for the whole pass, reusing this
    // stage's climate (no per-settlement climate rebuild — the Stage-2 perf
    // guard). Observations are gathered under one immutable ledger borrow,
    // then committed in a second pass (the borrow must end before `commit`).
    let boxed_sources = phenomena_sources_from(&world, &climate)?;
    let sources: Vec<&dyn PhenomenaSource> = boxed_sources.iter().map(|s| s.as_ref()).collect();
    struct SettlementDescriptor {
        id: EntityId,
        name: String,
        biome: String,
        latitude: f64,
        longitude: f64,
        gloss: String,
    }
    let mut descriptors: Vec<SettlementDescriptor> = Vec::with_capacity(placements.len());
    for s in &placements {
        let name = species_set[s.tag];
        let coord = geo.coord(s.cell);
        let salt = u64::from(s.cell.0);
        let namer = namers
            .get(name)
            .expect("a Namer was built for every placed species");
        let morph = morph_options(
            wc.psyche
                .get(&KindId(name))
                .expect("peopled pass over a fauna kind"),
        );
        let lexicon = lexicons
            .get(name)
            .expect("a lexicon was built for every placed species");
        let biome_concept = climate.biome_at(s.cell).concept_name();
        // The presiding phenomenon is observed from THIS settlement's own
        // cell coordinate — its hemisphere culls the sky (SEQ-5), so the
        // committed gloss is truthful to the sky this settlement actually
        // lives under (spec §9.3), and per-settlement skies widen the
        // descriptor space. Still a pure function of the entity's own
        // (cell, facts): pin-isolated by construction (spec §8). The place id
        // is unread by the observer — the coordinate does the culling — so
        // `world_entity` still stands in for it (the settlement entity now
        // exists, but observation never reads it) — see `observed_phenomena_from`.
        let seen =
            observe_with_sources(&world, wc, name, world_entity, Some(coord), &sources)?;
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
        descriptors.push(SettlementDescriptor {
            id: s.id,
            name: generated.roman,
            biome: climate.biome_at(s.cell).name().to_string(),
            latitude: coord.latitude,
            longitude: coord.longitude,
            gloss,
        });
    }
    for d in descriptors {
        settlement_descriptor_facts(
            &mut world,
            d.id,
            &d.name,
            &d.biome,
            d.latitude,
            d.longitude,
        )?;
        if !d.gloss.is_empty() {
            world
                .ledger
                .commit(name_gloss_fact(d.id, &d.gloss), &world.registry)?;
        }
    }
            Ok((
                terrain,
                climate,
                placements,
                lexicons,
                species_set,
                phonologies,
            ))
        },
    )?;

    stage("alignments", || -> Result<(), BuildError> {
        // The Long Count: each settlement's founding sightline. Skipped
        // wholesale on locked worlds / polar latitudes (the azimuth
        // function returns None) and on the constant sky (no calendar).
        // Placed before the settlement-depth early return (below) so a
        // world built only to `BuildDepth::Settlements` still carries
        // alignments — collecting first to avoid holding the sky borrow
        // across commits.
        let pairs: Vec<(EntityId, f64)> = {
            let sky = sky_of(&world)?;
            let Some(calendar) = sky.calendar() else {
                return Ok(());
            };
            hornvale_terrain::places(&world)
                .iter()
                // founding-solstice-azimuth-degrees is documented as a
                // settlement's founding sightline; gate on IS_SETTLEMENT
                // rather than assuming every place is one (today they all
                // are, but a future campaign may commit non-settlement
                // places).
                .filter(|p| {
                    world
                        .ledger
                        .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
                        .is_some()
                })
                .filter_map(|p| {
                    let coord = place_coord(&world, p.id)?;
                    let az = calendar.solstice_rise_azimuth_at(
                        coord.latitude,
                        hornvale_astronomy::StdDays::new(0.0).unwrap(),
                    )?;
                    Some((p.id, az))
                })
                .collect()
        };
        for (id, az) in pairs {
            facts::founding_alignment(&mut world, id, az)?;
        }
        Ok(())
    })?;

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
        // Build the phenomena sources ONCE for the whole per-species pass (as
        // the settlement loop does), reusing the already-derived `climate`:
        // every species observes the SAME flagship vantage, and the sky+climate
        // are invariant to the culture/religion facts committed inside this
        // loop — so this is byte-identical to the old per-species rebuild while
        // deriving no climate at all here. The flagship place and its
        // coordinate are likewise stable (settlements are already committed).
        let sp_place = hornvale_terrain::places(&world).first().map(|p| p.id);
        let sp_position = sp_place.and_then(|p| place_coord(&world, p));
        let boxed_sp_sources = phenomena_sources_from(&world, &climate)?;
        let sp_sources: Vec<&dyn PhenomenaSource> =
            boxed_sp_sources.iter().map(|s| s.as_ref()).collect();
        // Per-species flagship culture and religion.
        for (tag, &name) in species_set.iter().enumerate() {
            let Some(pos) = placements.iter().position(|s| s.tag == tag) else {
                continue; // a species may place nothing on a hostile world
            };
            let flagship = placements[pos].id;
            let fcell = placements[pos].cell;
            let coastal = geo.neighbors(fcell).iter().any(|n| terrain.is_ocean(*n));
            let moisture = climate.moisture_at(fcell);
            let class = biome_class(climate.biome_at(fcell));
            let subsistence = hornvale_culture::subsistence(class, coastal);
            let surplus = (hornvale_culture::fertility(class) * moisture).clamp(0.0, 1.0);
            let threat = terrain.unrest_at(fcell).clamp(0.0, 1.0);
            let env = hornvale_culture::EnvSummary {
                subsistence,
                surplus,
                population: placements[pos].population,
                threat,
            };
            // Mind + speech sourced from the world's component set (ECS c3):
            // psychology from `wc.psyche`, the role vocabulary from
            // `wc.lexicon`, both keyed by the kind's `KindId` label.
            let psych_v = wc
                .psyche
                .get(&KindId(name))
                .expect("peopled pass over a fauna kind");
            let lex = wc
                .lexicon
                .get(&KindId(name))
                .expect("peopled pass over a fauna kind");
            let psych = hornvale_culture::PsychSummary {
                threat_response: psych_v.threat_response,
                time_horizon: psych_v.time_horizon,
                communal: psych_v.sociality == hornvale_species::Sociality::Communal,
                rank_status: psych_v.status_basis == hornvale_species::StatusBasis::Rank,
                vocabulary: hornvale_culture::RoleVocabulary {
                    worker_override: lex.worker_override.map(str::to_string),
                    warrior: lex.warrior.to_string(),
                    artisan: lex.artisan.to_string(),
                    shaman: lex.shaman.to_string(),
                    top: lex.top.to_string(),
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
                has_priesthood: castes.iter().any(|c| c == lex.shaman),
            };
            // Religion (and the deity glosses drawn inside it) observes from
            // the world's first place — the flagship vantage, its hemisphere
            // culling the sky (SEQ-4/SEQ-5) — exactly the observation
            // `religion::genesis` derives its beliefs from, so every deity
            // name-gloss is truthful to the phenomenon its belief was actually
            // derived from. Settlements exist by now, so the placed-observer
            // path is live.
            let seen = match sp_place {
                Some(place) => {
                    observe_with_sources(&world, wc, name, place, sp_position, &sp_sources)?
                }
                None => Vec::new(),
            };
            let namer = namers
                .get(name)
                .expect("a Namer was built for every placed species");
            let morph = morph_options(psych_v);
            let lexicon = lexicons
                .get(name)
                .expect("a lexicon was built for every placed species");
            let mut deity_namer = LanguageDeityNamer {
                namer,
                morph,
                lexicon,
                phenomena: &seen,
                index: 0,
                glosses: std::collections::BTreeMap::new(),
                deity_seed: deity_base_seed(&seed, name),
            };
            hornvale_religion::genesis(&mut world, flagship, &seen, &society, &mut deity_namer)?;
            for (salt, gloss) in &deity_namer.glosses {
                world.ledger.commit(
                    name_gloss_fact(
                        hornvale_kernel::EntityId::new(*salt)
                            .expect("salt is a minted entity id's raw value, never 0"),
                        gloss,
                    ),
                    &world.registry,
                )?;
            }
        }

        // Species entities AFTER every pre-species subsystem (settlements,
        // culture, religion) — entity-id stability, spec §8: a goblin-pinned
        // world must mint the exact same ids for pre-C1 entities as pre-species
        // main, so the new, Y2-1-only entities are appended last rather than
        // interleaved. Then the peopled-by link for every settlement.
        species_genesis(&mut world, wc)?;
        for s in &placements {
            hornvale_species::people(&mut world, s.id, species_set[s.tag])?;
        }
        Ok(())
    })?;

    stage("deep-time", || -> Result<(), BuildError> {
        // Deep time: extract the glacial strata and commit their summary facts on
        // the world entity, so `recount`/`why` can speak the world's past.
        let paleo = paleoclimate_from(&world, &terrain)?;
        hornvale_paleoclimate::genesis(&mut world, world_entity, terrain.geosphere(), &paleo)?;
        Ok(())
    })?;

    stage("planet", || -> Result<(), BuildError> {
        // The planet asserts, in the ledger, what it is and what its
        // dominant people call it. Classification is read from structure (no
        // draw); the name is a lexicon lookup of the universal-stratum
        // `earth` concept (no draw). Run last, after peoples/settlements are
        // placed, so `world_name` (which reads `dominant_people` ->
        // `flagship_of`) resolves.
        //
        // The planet IS `world_entity`, the root fact-holder bound at the
        // top of `build_to` — not a fresh mint. `world_entity` already
        // carries every astronomical/terrain fact (moon-count, star-class,
        // …), so classifying it here (rather than an otherwise-empty
        // phantom entity) lets a single subject aggregate all of a world's
        // planetary facts into one rendered sentence.
        let planet = world_entity;
        world
            .ledger
            .commit(
                Fact {
                    subject: planet,
                    predicate: hornvale_kernel::world::IS_A.into(),
                    object: Value::Text("planet".into()),
                    place: None,
                    day: None,
                    provenance: "astronomy: the central body is a planet".into(),
                },
                &world.registry,
            )
            .expect("world_entity has no prior is-a fact, so this cannot conflict");
        if let Some(name) = world_name_in_from(&world, wc, &terrain, &climate) {
            world
                .ledger
                .commit(
                    Fact {
                        subject: planet,
                        predicate: hornvale_kernel::NAME.into(),
                        object: Value::Text(name),
                        place: None,
                        day: None,
                        provenance: "the dominant people's word for the world".into(),
                    },
                    &world.registry,
                )
                .expect("world_entity has no prior name fact, so this cannot conflict");
        }
        Ok(())
    })?;

    stage("peoples", || -> Result<(), BuildError> {
        // Every placed peopled species gets one collective entity: an
        // `instance-of` fact naming the species kind (the ECS Individuation
        // campaign's mechanism, first wired to genesis here — C2 §3), named
        // by that people's own word for "person" (Task 1's universal-stratum
        // concept). Mint order is `placed_peoples`' own order — already
        // deterministic (alphabetical, `BTreeMap`-backed registry keys; see
        // `placed_peoples_lists_flagship_holders_in_registry_order`). `day`
        // is `None` (no draw) and the mint provenance is fixed, matching
        // every other roster-kind mint's convention.
        //
        // A people whose lexicon has no "person" entry (a `LexEntry::Gap`,
        // or an absent entry) is left unnamed rather than erroring — a
        // PROC-15 coverage gap, not a build failure. Task 1's `person`
        // concept is universal-stratum (every lexicon resolves it today),
        // so this branch is not exercised by any current seed.
        // Collective names must be unique WITHIN a world. The account layer
        // keys collectives by subject NAME, so two peoples that render the
        // same autonym (bugbear and hobgoblin both draw "Babako" at seed 1,
        // sharing a proto-language) collapse into one — which breaks the
        // null-filter byte-identity law, ground-truth recoverability, and the
        // disclosure tripwire. Only the composition root sees the whole
        // peoples roster (the language domain names one people at a time), so
        // uniqueness is enforced here. On a collision we re-draw
        // deterministically from that same people's own namer — a bare stem
        // in its language, advancing the salt until the stem is unused. This
        // stays byte-identical (same world -> same names): `placed_peoples`
        // is registry-ordered (alphabetical), the `BTreeSet` is deterministic,
        // and `Namer::name` is a pure function of (seed, species, kind, salt)
        // — no `HashMap`, no RNG. The first holder of a name keeps its
        // autonym; only later collisions re-draw (hobgoblin, sorting after
        // bugbear, yields to it).
        let mut used_collective_names: std::collections::BTreeSet<String> =
            std::collections::BTreeSet::new();
        for kind in placed_peoples(&world) {
            let collective =
                mint_instance_of_kind(&mut world, wc, kind.0, None, "the people as a roster kind")?;
            let autonym = lexicon_of_in_from(&world, wc, kind.0, &terrain, &climate)?
                .entry("person")
                .and_then(|entry| match entry {
                    hornvale_language::LexEntry::Root { views, .. }
                    | hornvale_language::LexEntry::Compound { views, .. } => {
                        Some(views.roman.clone())
                    }
                    hornvale_language::LexEntry::Gap { .. } => None,
                });
            if let Some(mut name) = autonym {
                if used_collective_names.contains(&name) {
                    // Deterministic disambiguation: advance the salt through
                    // this people's namer until the rendered stem is unused.
                    // `NameKind::Settlement` is a bare stem (no honorifics, no
                    // reduplication) — the closest morphology to a noun
                    // autonym — so the disambiguated collective still reads as
                    // a word in that people's own language.
                    let namer = namers
                        .get(kind.0)
                        .expect("a Namer was built for every placed species");
                    let morph = hornvale_language::MorphOptions { honorifics: false };
                    let mut salt = 0u64;
                    loop {
                        let candidate = namer
                            .name(hornvale_language::NameKind::Settlement, salt, &morph)
                            .roman;
                        salt += 1;
                        if !used_collective_names.contains(&candidate) {
                            name = candidate;
                            break;
                        }
                    }
                }
                used_collective_names.insert(name.clone());
                world
                    .ledger
                    .commit(
                        Fact {
                            subject: collective,
                            predicate: hornvale_kernel::NAME.into(),
                            object: Value::Text(name),
                            place: None,
                            day: None,
                            provenance: "the people's own word for 'person'".into(),
                        },
                        &world.registry,
                    )
                    .expect("a freshly minted collective has no prior name fact, so this cannot conflict");
            }
        }
        Ok(())
    })?;

    Ok(world)
}

/// The entity carrying the world's planet classification (`is-a`,
/// `"planet"`) — `world_entity`, the root fact-holder, once `build_to`
/// reaches its final "planet" stage. Returns `None` before that (e.g. a
/// world stopped early via `BuildDepth`).
pub fn planet_entity(world: &World) -> Option<EntityId> {
    world
        .ledger
        .find(hornvale_kernel::world::IS_A)
        .find(|f| f.object == Value::Text("planet".into()))
        .map(|f| f.subject)
}

/// Mint one species entity per kind in `wc` and commit its authored vector as
/// facts, reading the composed [`WorldComponents`] rather than a god-struct
/// roster (the composition-root relocation of `species::genesis_in`, ECS c3).
///
/// **Mint order is byte-identity-critical:** entities are minted in ascending
/// `KindId` (`wc.biosphere.ids()`), which equals the default roster's slice
/// order (`registry().into_values()` is `KindId`-ascending) and is single-
/// element for Lab's synthetic rosters — so it reproduces `genesis_in`'s mint
/// sequence in every real case. Every fact (predicate, value, order,
/// `provenance="species"`, `day=Some(0.0)`) mirrors `genesis_in` exactly.
/// type-audit: bare-ok(identifier-text)
fn species_genesis(
    world: &mut World,
    wc: &WorldComponents,
) -> Result<std::collections::BTreeMap<String, EntityId>, LedgerError> {
    // Local mirror of species' `fact` helper: every species-genesis fact is
    // day 0, provenance "species", placeless.
    fn sfact(subject: EntityId, predicate: &str, object: Value) -> Fact {
        Fact {
            subject,
            predicate: predicate.to_string(),
            object,
            place: None,
            day: Some(0.0),
            provenance: "species".to_string(),
        }
    }

    use hornvale_species::{
        ActivityCycle, DELIBERATION_LATENCY, IN_GROUP_RADIUS, SOCIALITY_MODE,
        SPECIES_ACTIVITY_CYCLE, SPECIES_EXOTIC_MANNER, SPECIES_LABIALITY, SPECIES_NAME,
        SPECIES_NIGHT_VISION, SPECIES_SIBILANCE, SPECIES_SKY_ATTENTION, SPECIES_TONALITY,
        SPECIES_VOICE_LOUDNESS, SPECIES_VOICING, SPECIES_VOWEL_SPACE, STATUS_BASIS, Sociality,
        StatusBasis, THREAT_RESPONSE, TIME_HORIZON,
    };

    let mut ids = std::collections::BTreeMap::new();
    // Ascending KindId — the biosphere store is the canonical entity set.
    for kind in wc.biosphere.ids() {
        let name = kind.0;
        let id = world.ledger.mint_entity();
        world.ledger.commit(
            sfact(id, SPECIES_NAME, Value::Text(name.to_string())),
            &world.registry,
        )?;
        // Peopled-registry facts (mind + perception + speech) describe a
        // settling people's full cluster. A minded solitary (a dragon carries a
        // psyche but no perception or speech) emits only its `SPECIES_NAME`
        // above; its mind stays latent — read from the registry if it is ever
        // agentified — so every shipped world is byte-identical (The Eremite).
        // Gated on `Settled`, which the nested-capacity invariant guarantees
        // implies the full peopled cluster, so the reads below are total.
        let settled = wc
            .biosphere
            .get(kind)
            .is_some_and(|b| b.social_form == hornvale_species::SocialForm::Settled);
        if let Some(p) = wc.psyche.get(kind).filter(|_| settled) {
            let perception = wc
                .perception
                .get(kind)
                .expect("a Settled people carries the full peopled cluster (integrity-checked)");
            let articulation = wc
                .articulation
                .get(kind)
                .expect("a Settled people carries the full peopled cluster (integrity-checked)");
            let sociality = match p.sociality {
                Sociality::Hierarchic => "hierarchic",
                Sociality::Communal => "communal",
            };
            let status = match p.status_basis {
                StatusBasis::Rank => "rank",
                StatusBasis::Knowledge => "knowledge",
                StatusBasis::Generosity => "generosity",
            };
            world.ledger.commit(
                sfact(id, THREAT_RESPONSE, Value::Number(p.threat_response)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(
                    id,
                    DELIBERATION_LATENCY,
                    Value::Number(p.deliberation_latency),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, IN_GROUP_RADIUS, Value::Number(p.in_group_radius)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, TIME_HORIZON, Value::Number(p.time_horizon)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, SOCIALITY_MODE, Value::Text(sociality.to_string())),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, STATUS_BASIS, Value::Text(status.to_string())),
                &world.registry,
            )?;
            let activity = match perception.activity {
                ActivityCycle::Diurnal => "diurnal",
                ActivityCycle::Nocturnal => "nocturnal",
                ActivityCycle::Crepuscular => "crepuscular",
            };
            world.ledger.commit(
                sfact(
                    id,
                    SPECIES_ACTIVITY_CYCLE,
                    Value::Text(activity.to_string()),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(
                    id,
                    SPECIES_NIGHT_VISION,
                    Value::Number(perception.night_vision),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(
                    id,
                    SPECIES_SKY_ATTENTION,
                    Value::Number(perception.sky_attention),
                ),
                &world.registry,
            )?;
            let exotic = match articulation.exotic {
                hornvale_language::ExoticManner::None => "none",
                hornvale_language::ExoticManner::Trill => "trill",
                hornvale_language::ExoticManner::Click => "click",
                hornvale_language::ExoticManner::Ejective => "ejective",
            };
            world.ledger.commit(
                sfact(id, SPECIES_LABIALITY, Value::Number(articulation.labiality)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(
                    id,
                    SPECIES_VOWEL_SPACE,
                    Value::Number(articulation.vowel_space),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, SPECIES_VOICING, Value::Number(articulation.voicing)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, SPECIES_SIBILANCE, Value::Number(articulation.sibilance)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(
                    id,
                    SPECIES_VOICE_LOUDNESS,
                    Value::Number(articulation.voice_loudness),
                ),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, SPECIES_TONALITY, Value::Number(articulation.tonality)),
                &world.registry,
            )?;
            world.ledger.commit(
                sfact(id, SPECIES_EXOTIC_MANNER, Value::Text(exotic.to_string())),
                &world.registry,
            )?;
        }
        ids.insert(name.to_string(), id);
    }
    Ok(ids)
}

/// Build a complete world with the shipped species roster — the canonical
/// component set ([`WorldComponents::assemble`], byte-equal to the default
/// roster's composed set).
pub fn build_world(
    seed: Seed,
    pins: &SkyPins,
    sky: SkyChoice,
    terrain_pins: &TerrainPins,
    settlement_pins: &SettlementPins,
) -> Result<World, BuildError> {
    let wc = WorldComponents::assemble()?;
    build_world_from_components(seed, pins, sky, terrain_pins, settlement_pins, &wc)
}

/// Mint an instance of a known kind: the composition root's validated entry
/// to `Ledger::mint_instance` (the kernel is roster-blind; spec §4.2). Fails
/// loudly when the label is not in the union kind roster.
/// type-audit: bare-ok(identifier-text: kind), waiver(decision-0014: day), bare-ok(prose: provenance)
pub fn mint_instance_of_kind(
    world: &mut World,
    wc: &WorldComponents,
    kind: &str,
    day: Option<f64>,
    provenance: &str,
) -> Result<EntityId, BuildError> {
    if !wc.kinds().iter().any(|k| k.0 == kind) {
        return Err(BuildError::MalformedKind(format!(
            "cannot mint an instance of unknown kind {kind:?} (not in the union roster)"
        )));
    }
    world
        .ledger
        .mint_instance(kind, day, provenance, &world.registry)
        .map_err(BuildError::Ledger)
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

/// The peopled race with the greatest `Σ(population × mass)`, tie-broken by
/// ascending [`KindId`]. `None` if the world has no peopled race (or if
/// component assembly fails).
///
/// Population is the flagship settlement's population — the documented C1
/// stand-in; no reusable "total settled population per species" accessor
/// exists yet (a follow-up would sum every settlement a species holds, not
/// just its flagship). Mass is the biosphere component's authored value.
/// Comparison uses `f64::total_cmp` for determinism (no `partial_cmp`).
pub fn dominant_people(world: &World) -> Option<KindId> {
    let wc = WorldComponents::assemble().ok()?;
    dominant_people_in(world, &wc)
}

/// [`dominant_people`] over an explicit component set — the testable core
/// (mirrors `lexicon_of`/`lexicon_of_in`), so a mutation check can perturb a
/// candidate's mass without touching the canonical registries.
pub fn dominant_people_in(world: &World, wc: &WorldComponents) -> Option<KindId> {
    let mut best: Option<(f64, KindId)> = None;
    for (kind, bio) in wc.biosphere.iter() {
        if wc.lexicon.get(kind).is_none() {
            // Biosphere-only (a dragon/fungus) — not a naming candidate.
            continue;
        }
        // Candidacy requires actual placement (registry-first-is-not-
        // placed-first): a registered-but-unplaced kind must never compete
        // at weight 0.0 against a placed kind — it isn't a candidate at all.
        let Some(flagship) = flagship_of(world, kind.0) else {
            continue;
        };
        let weight = flagship.population as f64 * bio.mass.kilograms();
        let better = match best {
            None => true,
            Some((best_weight, best_kind)) => {
                weight.total_cmp(&best_weight).is_gt()
                    || (weight.total_cmp(&best_weight).is_eq() && *kind < best_kind)
            }
        };
        if better {
            best = Some((weight, *kind));
        }
    }
    best.map(|(_, kind)| kind)
}

/// The dominant race's word for "earth" — its endonym for the world it
/// inhabits — capitalized per the lexicon's surface-view convention.
/// `None` if the world has no dominant race, or if that race's lexicon has
/// no entry for the concept (a coverage gap, never a silent fallback).
///
/// Assembles the canonical [`WorldComponents`] and delegates to
/// [`world_name_in`]; callers that already hold a `wc` (e.g. the planet
/// genesis stage) should call that directly instead of re-deriving.
/// type-audit: bare-ok(identifier-text)
pub fn world_name(world: &World) -> Option<String> {
    let wc = WorldComponents::assemble().ok()?;
    world_name_in(world, &wc)
}

/// [`world_name`] over an explicit component set — the testable core
/// (mirrors `lexicon_of`/`lexicon_of_in` and `dominant_people`/
/// `dominant_people_in`), so the planet genesis stage can use the SAME
/// injected `wc` it already built rather than re-assembling components.
/// type-audit: bare-ok(identifier-text)
pub fn world_name_in(world: &World, wc: &WorldComponents) -> Option<String> {
    let terrain = terrain_of(world).ok()?;
    let climate = climate_of(world).ok()?;
    world_name_in_from(world, wc, &terrain, &climate)
}

/// [`world_name_in`], reusing ALREADY-BUILT terrain/climate down
/// [`lexicon_of_in_from`] instead of re-sculpting the globe. The planet genesis
/// stage — which already holds the sculpted terrain/climate — calls this so
/// naming the world costs no extra sculpt. Byte-identical to `world_name_in`.
/// type-audit: bare-ok(identifier-text)
pub fn world_name_in_from(
    world: &World,
    wc: &WorldComponents,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
) -> Option<String> {
    let kind = dominant_people_in(world, wc)?;
    let lex = lexicon_of_in_from(world, wc, kind.0, terrain, climate).ok()?;
    match lex.entry("earth")? {
        hornvale_language::LexEntry::Root { views, .. }
        | hornvale_language::LexEntry::Compound { views, .. } => Some(views.roman.clone()),
        hornvale_language::LexEntry::Gap { .. } => None,
    }
}

/// Every peopled species holding a flagship settlement, in registry
/// (alphabetical) order.
///
/// The single basis for the almanac's name-the-species-only-when-there-is-
/// more-than-one convention. Both the People section (`settlement_lines`)
/// and the Gods section's pantheon attribution read this one predicate, so
/// the two sections cannot drift into naming a world's peoples in one place
/// and withholding them in the other.
/// type-audit: bare-ok(identifier-text: return)
pub fn placed_peoples(world: &World) -> Vec<(&'static str, hornvale_settlement::VillageInfo)> {
    hornvale_species::biosphere_registry()
        .ids()
        .filter_map(|name| flagship_of(world, name.0).map(|v| (name.0, v)))
        .collect()
}

/// Headline lines describing the world's people for the almanac: how many
/// settlements, then one chief-settlement line per species that placed
/// (registry order — alphabetical; bugbear sorts first). A world with
/// exactly one such species keeps the legacy unprefixed wording — byte-
/// stable for goblin-only worlds; two-or-more-species worlds prefix each
/// chief line with its species.
/// type-audit: bare-ok(prose: return)
pub fn settlement_lines(world: &World) -> Result<Vec<String>, BuildError> {
    let places = hornvale_terrain::places(world);
    let mut lines = vec![format!("The land holds {} settlement(s).", places.len())];

    let flagships = placed_peoples(world);
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
///
/// Appends a weather clause (The Firmament, spec Weather Program C4): the
/// felt sky at the world's representative cell — the first settlement's
/// location, the same "first place" vantage `calendar_lines` and
/// `night_sky_lines` already read via [`place_coord`] — rendered through
/// [`sky_phrase`], the one weather vocabulary the almanac's headline lines
/// share. A placeless world (no settlement has been placed, or has no
/// committed coordinate) falls back to cell 0 — still informative, just not
/// settlement-anchored, mirroring the fallback-free-but-defaulting posture
/// `night_sky_lines`' heliacal latitude uses. `windows/vessel/src/vantage.rs`
/// turns the rendered `description` into `Vantage.sky`, so a possessed
/// agent's sky and the almanac's placed-observer lines describe the same
/// point on the globe.
pub fn sky_report(world: &World, time: WorldTime) -> Result<SkyReport, BuildError> {
    let mut report = sky_of(world)?.sky_at(time);
    let terrain = terrain_of(world)?;
    let climate = climate_from(world, &terrain)?;
    let cell = hornvale_terrain::places(world)
        .into_iter()
        .find(|p| {
            world
                .ledger
                .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
                .is_some()
        })
        .and_then(|p| place_coord(world, p.id))
        .map(|c| terrain.nearest_cell(c.latitude, c.longitude))
        .unwrap_or(hornvale_kernel::CellId(0));
    let state = climate.weather_at(cell, time.day);
    let cloud = climate.cloud_type_at(cell, time.day);
    report.description = format!(
        "{} The sky is {}.",
        report.description,
        sky_phrase(state, cloud)
    );
    Ok(report)
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

/// The pole star, heliacal-return, and wanderer lines under **The Sky**
/// (night-sky stage 1/2): a pole-star sentence when the genesis-epoch night
/// sky finds one within `POLE_STAR_MAX_SEPARATION_DEG` of either celestial
/// pole, up to three heliacal-return sentences for the brightest neighbors
/// (index order), at the same flagship vantage latitude `calendar_lines`
/// resolves its daylight-swing line at (the first known place's committed
/// latitude; reference latitude 35.0 — a mid-temperate default — until the
/// flagship vantage reaches the almanac, if no place resolves), plus one
/// sentence per wandering sibling planet, innermost order, plus a single
/// figure count/ecliptic summary line (night-sky stage 3; omitted for a sky
/// with no figures at all). `None` for constant-sky worlds, which have no
/// neighborhood to describe.
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
    let latitude_fallback = 35.0;
    let heliacal = {
        let latitude = hornvale_terrain::places(world)
            .first()
            .and_then(|p| place_coord(world, p.id))
            .map(|c| c.latitude)
            .unwrap_or(latitude_fallback);
        let year_length_days = calendar.year_length().get();
        let pairs = hornvale_astronomy::heliacal_events(system, calendar, latitude, t);
        pairs
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
            .collect()
    };

    let wanderers = system
        .wanderers
        .iter()
        .map(|w| {
            let class_word = match w.class {
                hornvale_astronomy::WandererClass::Rock => "rock",
                hornvale_astronomy::WandererClass::Giant => "giant",
            };
            let morning_evening = if w.max_elongation_deg.is_some() {
                " — a morning and evening star"
            } else {
                ""
            };
            format!(
                "A {} wanderer rounds the sun every {:.0} days{}.",
                class_word,
                w.period.get(),
                morning_evening
            )
        })
        .collect();

    // Figures (night-sky stage 3): a single count/ecliptic summary line,
    // never rendered for a sky with no figures at all. `world.seed` derives
    // the same astronomy seed `system::generate` and the lab metrics use.
    let astronomy_seed = world.seed.derive(ASTRONOMY_STREAM_ROOT);
    let figs = figures(astronomy_seed, system);
    let figure_lines = if figs.is_empty() {
        Vec::new()
    } else {
        let ecliptic_count = figs.iter().filter(|f| f.on_ecliptic).count();
        let figure_word = if figs.len() == 1 { "figure" } else { "figures" };
        let stand_word = if ecliptic_count == 1 {
            "stands"
        } else {
            "stand"
        };
        vec![format!(
            "The sky holds {} {}; {} {} on the sun's road.",
            figs.len(),
            figure_word,
            ecliptic_count,
            stand_word
        )]
    };

    // Eclipses (Eclipse Seasons): dated events over the next two years,
    // then the recurrence-ladder lines read off the innermost moon.
    let year = calendar.year_length().get();
    let events = hornvale_astronomy::eclipse_events(
        system,
        calendar,
        t,
        hornvale_astronomy::StdDays::new(t.get() + 2.0 * year).unwrap(),
    );
    let ordinal = |i: usize| {
        ["first", "second", "third"]
            .get(i)
            .copied()
            .unwrap_or("far")
    };
    let mut eclipses: Vec<String> = events
        .iter()
        .take(6)
        .map(|e| {
            use hornvale_astronomy::{EclipseBody, EclipseKind};
            match (e.body, e.kind) {
                (EclipseBody::Lunar, _) => format!(
                    "On day {:.0}, the full {} moon darkens to a bloodred coal.",
                    e.day.get(),
                    ordinal(e.moon)
                ),
                (EclipseBody::Solar, kind) => {
                    let verb = match kind {
                        EclipseKind::Total => "devours the sun whole",
                        EclipseKind::Annular => "leaves a burning ring of the sun",
                    };
                    match hornvale_astronomy::ground_track(system, calendar, e) {
                        Some(track) => format!(
                            "On day {:.0}, the {} moon {} along latitude {:.0}°.",
                            e.day.get(),
                            ordinal(e.moon),
                            verb,
                            track.center_lat_deg
                        ),
                        None => format!(
                            "On day {:.0}, the {} moon {}.",
                            e.day.get(),
                            ordinal(e.moon),
                            verb
                        ),
                    }
                }
            }
        })
        .collect();
    if eclipses.is_empty() && !system.moons.is_empty() {
        eclipses.push("No eclipse will darken the sun for two years.".to_string());
    }
    // The recurrence ladder, read off the innermost moon.
    if let (Some(moon), Some(synodic)) = (system.moons.first(), calendar.synodic_month(0)) {
        let year_len = calendar.year_length();
        let p_node =
            hornvale_astronomy::node_regression_period(year_len, moon.period, moon.inclination_deg);
        let ey = hornvale_astronomy::eclipse_year(year_len, p_node);
        let parade = hornvale_astronomy::parade_days_per_year(year_len, ey);
        eclipses.push(format!(
            "The eclipse seasons parade backward through the year at {parade:.0} days a year."
        ));
        let draconic =
            hornvale_astronomy::draconic_month(year_len, moon.period, moon.inclination_deg);
        if let Some(cycle) = hornvale_astronomy::best_cycle(synodic, draconic) {
            let sun_angular = hornvale_astronomy::sun_angular_rel_at(system, calendar, t);
            let theta = hornvale_astronomy::solar_eclipse_threshold_deg(
                sun_angular,
                moon.angular_diameter_rel,
            );
            let returns = hornvale_astronomy::series_returns(&cycle, theta, moon.inclination_deg);
            eclipses.push(format!(
                "Eclipses of the first moon repeat every {:.0} days ({} months); \
                 a family of them lives about {:.0} years.",
                cycle.period.get(),
                cycle.synodic_count,
                returns as f64 * cycle.period.get() / 365.25
            ));
        }
    }

    // The founding sightline and its drift rate (The Long Count): rendered
    // only when a real settlement exists (to ensure the "From the first
    // settlement" language is truthful). Requires the actual first place's
    // latitude; the 35° fallback (used for heliacal events) is insufficient
    // here since it would create a lying sentence.
    let alignment = hornvale_terrain::places(world)
        .into_iter()
        // Same settlement gate as the alignments stage: the "first
        // settlement" language below must name an actual settlement, not
        // just the first place (today every place is a settlement, but
        // that need not hold forever).
        .find(|p| {
            world
                .ledger
                .value_of(p.id, hornvale_settlement::IS_SETTLEMENT)
                .is_some()
        })
        .and_then(|p| place_coord(world, p.id))
        .map(|c| c.latitude)
        .and_then(|latitude| {
            calendar
                .solstice_rise_azimuth_at(latitude, t)
                .and_then(|az| {
                    let kyr = hornvale_astronomy::StdDays::new(t.get() + 1000.0 * 365.25).unwrap();
                    let drift = calendar.alignment_drift_deg(latitude, t, kyr)?;
                    Some(format!(
                        "From the first settlement, the midsummer sun rises at azimuth {az:.1}°; the sightline drifts {:.2}° in a thousand years.",
                        drift.abs()
                    ))
                })
        });

    Ok(Some(hornvale_almanac::NightSkyLines {
        pole_star,
        heliacal,
        wanderers,
        figures: figure_lines,
        eclipses,
        alignment,
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
    wc: &WorldComponents,
    species: &str,
) -> Result<Option<RenderedPantheon>, BuildError> {
    let Some(v) = flagship_of(world, species) else {
        return Ok(None);
    };
    let beliefs = hornvale_religion::beliefs_held_by(world, v.id);
    if beliefs.is_empty() {
        return Ok(None);
    }
    let phenomena = observed_phenomena_as(world, species)?;
    // Reached only once `flagship_of` above returned `Some`: only peopled
    // species place settlements/flagships, so `species` is guaranteed peopled
    // here. Psychology sourced from the ALREADY-built component set (ECS c3),
    // matched by its `KindId` label (the free-text `species` is not `'static`).
    let voice = voice_params(
        wc.psyche
            .iter()
            .find(|(k, _)| k.0 == species)
            .map(|(_, p)| p)
            .expect("peopled pass over a fauna kind"),
    );
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
/// by species-flagship pantheon in registry order — alphabetical; bugbear
/// sorts first — each pantheon's beliefs head first (matching
/// `beliefs_of`/`beliefs_held_by`'s own ordering). The seam-wiring site the
/// REPL's `beliefs` command renders through (spec §6, Task 11); the almanac
/// renders the same seam via [`almanac_context`]'s `PantheonBlock`s. Legacy
/// fallback: see [`legacy_rendered_beliefs`].
/// type-audit: bare-ok(prose: return)
pub fn rendered_beliefs(
    world: &World,
) -> Result<Vec<(hornvale_religion::Belief, String)>, BuildError> {
    let mut out = Vec::new();
    // The canonical component set, built once and threaded to each pantheon
    // (ECS c3): every species here is a canonical registry kind, so `assemble`
    // gives the byte-identical psyche rows a per-def rebuild would.
    let wc = WorldComponents::assemble()?;
    for name in wc.biosphere.ids() {
        if let Some((_, rendered)) = rendered_pantheon_of(world, &wc, name.0)? {
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
    // Speech (the settlement noun) and the life-history line's biosphere both
    // sourced from the canonical component set (ECS c3): every entity is a
    // biosphere row, iterated in `KindId`-ascending order (byte-identity).
    let wc = WorldComponents::assemble()?;
    let peoples = wc
        .biosphere
        .iter()
        .filter_map(|(name, biosphere)| {
            let flagship = flagship_of(world, name.0)?;
            let mut lines = culture_lines(world, &flagship);
            lines.push(hornvale_almanac::render_life_history_line(
                name.0, biosphere,
            ));
            Some(hornvale_almanac::PeopleBlock {
                species: name.0.to_string(),
                noun: wc
                    .lexicon
                    .get(name)
                    .expect("a peopled kind with a flagship has a lexicon")
                    .noun
                    .to_string(),
                name: flagship.name.clone(),
                population: flagship.population,
                culture_lines: lines,
            })
        })
        .collect();
    // The Single Sculpt: build the terrain ONCE and the climate ONCE for the
    // whole render, then thread them into every Land/Seas/deep-time accessor
    // and the phenomena observation below instead of each re-deriving its own
    // (each `terrain_of`/`climate_of` runs the full sculpt + climate fields).
    // Byte-identical: every `_from` accessor equals its `_of` wrapper for this
    // world's terrain/climate.
    let terrain = terrain_of(world)?;
    let climate = climate_from(world, &terrain)?;
    // The deep-time lines, plus the secular-brightening sentence (The Long
    // Count) for a generated sky only — constant-sky worlds have no star to
    // brighten.
    let mut deep_time_lines = deep_time_lines_from(world, &terrain)?;
    if let Sky::Generated(sky) = sky_of(world)? {
        let system = sky.system();
        deep_time_lines.push(format!(
            "The sun brightens by {:.0} parts in a hundred over a gigayear — the slow fire under every deeper clock.",
            hornvale_astronomy::brightening_per_gyr(&system.star) * 100.0
        ));
    }
    Ok(AlmanacContext {
        seed: world.seed.0,
        sky: sky_report(world, WorldTime { day: 0.0 })?,
        climate: climate_report(world),
        phenomena: observed_phenomena_from_climate(world, 0.0, &climate)?,
        places: hornvale_terrain::places(world),
        land_lines: land_lines_from(&terrain),
        biome_lines: biome_lines_from(&climate),
        ground_lines: ground_lines_from(&terrain, &climate),
        water_lines: water_lines_from(&terrain),
        diurnal_lines: diurnal_lines_from(&terrain, &climate),
        seas_lines: seas_lines_from(&terrain, &climate),
        rains_lines: rains_lines_from(&terrain, &climate),
        firmament_lines: firmament_lines_from(&terrain, &climate),
        deep_time_lines,
        peoples,
        pantheons: {
            let mut blocks = Vec::new();
            // The almanac names a people only when there is another to
            // distinguish them from — the same rule, and the same
            // predicate, the People section uses (`settlement_lines`).
            let named = placed_peoples(world).len() > 1;
            for name in wc.biosphere.ids() {
                if let Some((v, rendered)) = rendered_pantheon_of(world, &wc, name.0)? {
                    blocks.push(hornvale_almanac::PantheonBlock {
                        attribution: named.then(|| hornvale_almanac::PantheonAttribution {
                            species: name.0.to_string(),
                            noun: wc
                                .lexicon
                                .get(name)
                                .expect("a peopled kind with a flagship has a lexicon")
                                .noun
                                .to_string(),
                            settlement: v.name.clone(),
                        }),
                        cult_form: hornvale_religion::cult_form_held_by(world, v.id),
                        beliefs: rendered
                            .into_iter()
                            .map(|(belief, tenet)| hornvale_almanac::BeliefLine { belief, tenet })
                            .collect(),
                    });
                }
            }
            // Legacy fallback: pre-species saves have beliefs but no
            // peopled-by facts — no species is attributable, so these
            // render as the single anonymous pantheon they always were
            // (see `legacy_rendered_beliefs`).
            if blocks.is_empty() {
                let rendered = legacy_rendered_beliefs(world)?;
                if !rendered.is_empty() {
                    blocks.push(hornvale_almanac::PantheonBlock {
                        attribution: None,
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

    #[test]
    fn wild_concentrations_are_deterministic_beasts() {
        // THE WILDING: the wild concentrations are byte-identical across calls,
        // and they name BEASTS (not the peoples) at real positions.
        let world = build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let a = wild_concentrations(&world, 5).unwrap();
        let b = wild_concentrations(&world, 5).unwrap();
        assert_eq!(a, b, "deterministic");
        assert!(!a.is_empty(), "the wild is populated: {a:?}");
        let biosphere = hornvale_species::biosphere_registry();
        for (species, _pos) in &a {
            let social_form = biosphere
                .get_by_label(species)
                .unwrap_or_else(|| panic!("{species} has a biosphere entry"))
                .social_form;
            assert!(
                social_form != hornvale_species::SocialForm::Settled,
                "{species} is wild (not Settled)"
            );
        }
        eprintln!("WILD {:?}", a.iter().map(|(s, _)| s).collect::<Vec<_>>());
    }

    #[test]
    fn social_form_selects_the_settled_and_wild_sets() {
        // THE EREMITE: the re-key onto `SocialForm` selects the settlement
        // roster (byte-identical to the pre-Eremite psyche key-set) and the
        // wild-agentified set. After The Eremite the dragons carry a mind yet
        // stay Solitary, so both sets are pinned against NAMED references, not
        // the live psyche key-set (now a superset of Settled).
        let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

        // The `Settled` set is exactly the four peoples.
        let settled: std::collections::BTreeSet<&'static str> = wc
            .biosphere
            .iter()
            .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
            .map(|(k, _)| k.0)
            .collect();
        let four_peoples: std::collections::BTreeSet<&'static str> =
            ["bugbear", "goblin", "hobgoblin", "kobold"]
                .into_iter()
                .collect();
        assert_eq!(settled, four_peoples, "Settled is exactly the four peoples");

        // The wild-agentified `{Solitary, Gregarious}` set: the ten mobile,
        // non-settled kinds — the same kinds the retired `¬psyche ∧ ¬autotroph`
        // proxy selected before the dragons gained a mind (still agentified,
        // now with a temperament to read). Disjoint from the settling peoples.
        let mobile_beasts: std::collections::BTreeSet<&'static str> = wc
            .biosphere
            .iter()
            .filter(|(_, b)| {
                matches!(
                    b.social_form,
                    hornvale_species::SocialForm::Solitary
                        | hornvale_species::SocialForm::Gregarious
                )
            })
            .map(|(k, _)| k.0)
            .collect();
        let expected_wild: std::collections::BTreeSet<&'static str> = [
            "black-dragon",
            "giant-elk",
            "giant-goat",
            "otyugh",
            "owlbear",
            "red-dragon",
            "rust-monster",
            "white-dragon",
            "woolly-mammoth",
            "xorn",
        ]
        .into_iter()
        .collect();
        assert_eq!(
            mobile_beasts, expected_wild,
            "the {{Solitary, Gregarious}} set is the ten mobile non-settled kinds"
        );
        assert!(
            settled.is_disjoint(&mobile_beasts),
            "a settling people is never wild-agentified"
        );
    }

    #[test]
    fn predator_pressure_is_deterministic_and_nontrivial() {
        // THE QUARRY: the carnivore-density field is byte-identical across calls
        // (no seed, pure over committed facts) and genuinely varies — some cells
        // carry predator mass (the wild), many carry none (ocean / settled land).
        let world = build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let a = predator_pressure(&world).unwrap();
        let b = predator_pressure(&world).unwrap();
        let va: Vec<f64> = a.iter().map(|(_, v)| *v).collect();
        let vb: Vec<f64> = b.iter().map(|(_, v)| *v).collect();
        assert_eq!(va, vb, "two calls produce byte-identical fields");
        assert!(
            va.iter().any(|v| *v > 0.5),
            "some cells are dense predator territory"
        );
        assert!(va.contains(&0.0), "many cells carry no predators");
    }

    #[test]
    fn prey_pressure_is_deterministic_nontrivial_and_the_predator_dual() {
        // THE TEETH: the prey-base density field is byte-identical across calls
        // (no seed, pure over committed facts), genuinely varies, and is a
        // DISTINCT field from the predator pressure — prey range where predators
        // are thin and vice versa, so a hunter drawn up the prey gradient heads
        // somewhere other than the carnivore territory a quarry flees.
        let world = build_world(
            hornvale_kernel::Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let a = prey_pressure(&world).unwrap();
        let b = prey_pressure(&world).unwrap();
        let va: Vec<f64> = a.iter().map(|(_, v)| *v).collect();
        let vb: Vec<f64> = b.iter().map(|(_, v)| *v).collect();
        assert_eq!(va, vb, "two calls produce byte-identical fields");
        assert!(
            va.iter().any(|v| *v > 0.5),
            "some cells are dense prey territory"
        );
        // A gradient exists for a hunter to climb: density varies from a sparse
        // floor (~0.005) up to the normalized peak, so a prey-poor cell has a
        // prey-richer neighbour to forage toward.
        let min = va.iter().cloned().fold(f64::INFINITY, f64::min);
        assert!(
            min < 0.2,
            "prey density varies — prey-poor cells exist to leave"
        );
        // The prey field is not the predator field: at least some cells differ
        // (the two populations do not perfectly coincide).
        let pred: Vec<f64> = predator_pressure(&world)
            .unwrap()
            .iter()
            .map(|(_, v)| *v)
            .collect();
        assert!(
            va.iter()
                .zip(&pred)
                .any(|(prey, pred)| (prey - pred).abs() > 0.1),
            "prey and predator fields are distinct populations"
        );
    }

    #[test]
    fn deity_name_seed_is_pure_and_entity_id_free() {
        use hornvale_kernel::Seed;
        let base = Seed(42)
            .derive(streams::RELIGION_DEITY_V2)
            .derive(StreamLabel::dynamic("goblin"));
        // Same species-seed + kind + rank -> same name seed, no entity id involved.
        assert_eq!(
            deity_name_seed(base, "celestial-body", 0),
            deity_name_seed(base, "celestial-body", 0)
        );
        // Rank disambiguates members that share a kind (two moons, two same-colour stars).
        assert_ne!(
            deity_name_seed(base, "celestial-body", 0),
            deity_name_seed(base, "celestial-body", 1)
        );
        // Kind leads semantically.
        assert_ne!(
            deity_name_seed(base, "celestial-body", 0),
            deity_name_seed(base, "tide", 0)
        );
    }

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

    /// C1 T2: the dominant race is the peopled kind maximizing
    /// `Σ(population × mass)`, deterministic across rebuilds; `world_name`
    /// is that race's capitalized word for "earth" (its endonym).
    #[test]
    fn dominant_people_weights_by_mass_not_headcount() {
        let world = constant(1);
        let d = dominant_people(&world).expect("a peopled world has a dominant race");
        // deterministic across rebuilds
        assert_eq!(dominant_people(&world), Some(d));
        // the world name is that race's word for "earth", capitalized
        let name = world_name(&world).expect("dominant race names the world");
        assert!(
            name.chars().next().unwrap().is_uppercase(),
            "endonym is capitalized"
        );
    }

    /// Mutation check ([[measure-dont-narrate-the-mechanism]]): assert the
    /// weighing MECHANISM, not a hard-coded winner. Seed 1's constant world
    /// places goblin (population 1) and hobgoblin (population 18); hobgoblin
    /// wins on `Σ(population × mass)` (18 × 74.8 kg ≫ 1 × 18.1 kg). Crushing
    /// hobgoblin's mass to near-zero in a rebuilt component set must flip the
    /// winner to goblin — if it didn't, `dominant_people` would be reading
    /// headcount alone and this test would catch it.
    #[test]
    fn dominant_people_changes_when_the_winners_mass_is_crushed() {
        let world = constant(1);
        let wc = WorldComponents::assemble().unwrap();
        let winner = dominant_people_in(&world, &wc).expect("a peopled world has a dominant race");

        let shrunk_biosphere: hornvale_kernel::ComponentStore<
            KindId,
            hornvale_species::BiosphereTraits,
        > = wc
            .biosphere
            .iter()
            .map(|(k, bio)| {
                let mut bio = bio.clone();
                if *k == winner {
                    bio.mass = hornvale_kernel::Mass::new(bio.mass.kilograms() * 0.001).unwrap();
                }
                (*k, bio)
            })
            .collect();
        let shrunk_wc = WorldComponents::from_stores(
            shrunk_biosphere,
            wc.psyche.clone(),
            wc.perception.clone(),
            wc.articulation.clone(),
            wc.lexicon.clone(),
            wc.family_proto.clone(),
            wc.family_of.clone(),
            wc.deity.clone(),
            wc.culture.clone(),
            wc.material.clone(),
        )
        .expect("cloned canonical stores stay integrity-valid");

        let new_winner = dominant_people_in(&world, &shrunk_wc)
            .expect("still a peopled world after the mass edit");
        assert_ne!(
            new_winner, winner,
            "crushing the winner's mass must change who wins — dominant_people is \
             mass-weighted, not headcount-only"
        );
    }

    /// A minimal peopled world: one settlement per `(kind, population)`
    /// pair, each `peopled-by` its kind, with NO other facts. Lets a test
    /// hold every candidate's canonical (`wc`) mass fixed while choosing its
    /// population freely — `constant(1)`'s real settlements already carry a
    /// committed `population` fact, and that predicate is functional (a
    /// second commit to the same subject would be rejected as a
    /// contradiction), so population can only be varied on fresh entities.
    fn synthetic_peopled_world(seed: Seed, placements: &[(KindId, u32)]) -> World {
        let mut world = World::new(seed);
        hornvale_settlement::register_concepts(&mut world.registry)
            .expect("settlement concepts register on a fresh registry");
        hornvale_species::register_concepts(&mut world.registry)
            .expect("species concepts register on a fresh registry");
        for (kind, population) in placements {
            let id = world.ledger.mint_entity();
            world
                .ledger
                .commit(
                    Fact {
                        subject: id,
                        predicate: hornvale_settlement::IS_SETTLEMENT.into(),
                        object: Value::Flag(true),
                        place: None,
                        day: None,
                        provenance: "test: synthetic flagship".into(),
                    },
                    &world.registry,
                )
                .expect("is-settlement on a fresh entity cannot conflict");
            world
                .ledger
                .commit(
                    Fact {
                        subject: id,
                        predicate: hornvale_settlement::POPULATION.into(),
                        object: Value::Number(f64::from(*population)),
                        place: None,
                        day: None,
                        provenance: "test: synthetic population".into(),
                    },
                    &world.registry,
                )
                .expect("population on a fresh entity cannot conflict");
            world
                .ledger
                .commit(
                    Fact {
                        subject: id,
                        predicate: hornvale_species::PEOPLED_BY.into(),
                        object: Value::Text(kind.0.to_string()),
                        place: None,
                        day: None,
                        provenance: "test: synthetic peopled-by".into(),
                    },
                    &world.registry,
                )
                .expect("peopled-by on a fresh entity cannot conflict");
        }
        world
    }

    /// Companion to `dominant_people_changes_when_the_winners_mass_is_crushed`:
    /// that test crushes MASS and proves the result isn't headcount-only, but
    /// a mass-only (population-blind) implementation — `weight = bio.mass`,
    /// ignoring population entirely — would still pass it, because crushing
    /// the winner's mass flips the mass-only ranking too. This test isolates
    /// the OTHER factor: hold both candidates' masses at their canonical
    /// (`wc`) values (mass ranking unchanged from `constant(1)`) and instead
    /// invert POPULATION — give the real winner a population of 1 and the
    /// real loser a landslide population. A mass-only mutant, which never
    /// looks at population, would still declare the same winner (mass
    /// ranking didn't move) and this assertion would fail; the real
    /// `Σ(population × mass)` formula flips.
    #[test]
    fn dominant_people_changes_when_the_winners_population_is_crushed() {
        let world = constant(1);
        let wc = WorldComponents::assemble().unwrap();
        let winner = dominant_people_in(&world, &wc).expect("a peopled world has a dominant race");
        let loser = if winner == KindId("goblin") {
            KindId("hobgoblin")
        } else {
            KindId("goblin")
        };

        let synth = synthetic_peopled_world(world.seed, &[(winner, 1), (loser, 100_000)]);
        let new_winner =
            dominant_people_in(&synth, &wc).expect("the synthetic world has two candidates");
        assert_ne!(
            new_winner, winner,
            "crushing the winner's population (while holding both masses fixed at their \
             canonical values) must change who wins — dominant_people is population-weighted, \
             not mass-only"
        );
    }

    /// Correctness guard (registry-first-is-not-placed-first, the class
    /// behind hornvale#1): the chosen dominant race must always be a race
    /// actually PLACED on this world, never a registered-but-unplaced kind
    /// that merely tied at weight zero.
    #[test]
    fn dominant_people_is_always_a_placed_race() {
        let world = constant(1);
        let kind = dominant_people(&world).expect("a peopled world has a dominant race");
        assert!(
            flagship_of(&world, kind.0).is_some(),
            "dominant_people must never name a species that holds no flagship settlement"
        );
    }

    /// C1 T4: the composition root commits the planet's classification and
    /// endonym (C2 T2: onto `world_entity`, not a fresh mint), so the book
    /// (Task 5) can render "‹Endonym› is a planet" from committed facts
    /// alone.
    #[test]
    fn built_world_names_and_classifies_its_planet() {
        let world = constant(1);
        let p = planet_entity(&world).expect("a built world has a planet entity");
        assert_eq!(world.ledger.text_of(p, "is-a"), Some("planet"));
        let n = world
            .ledger
            .text_of(p, "name")
            .expect("the planet is named");
        assert_eq!(Some(n.to_string()), world_name(&world));
    }

    /// C2 T2: the planet classification lands on `world_entity`, the root
    /// fact-holder that already carries the astronomical/terrain facts —
    /// not a fresh, otherwise-empty mint. This lets a later stage (book
    /// Task 4) aggregate moon-count/star-class/etc. onto the same subject
    /// the "is a planet" sentence names.
    #[test]
    fn the_planet_is_the_world_root_fact_holder() {
        // Generated (not Constant) sky: moon-count is only ever committed
        // under `SkyChoice::Generated` (`astronomy::facts::genesis` is
        // gated on it), so this is the sky choice that actually exercises
        // "the planet carries the astronomical facts."
        let world = generated(1);
        let p = planet_entity(&world).expect("a planet entity");
        assert!(
            world.ledger.value_of(p, "moon-count").is_some(),
            "the planet entity holds the astronomical facts"
        );
    }

    /// The planet's facts are a pure function of the seed, like every other
    /// committed fact — no draw, no wall-clock, no entity-order sensitivity.
    #[test]
    fn planet_facts_are_deterministic() {
        assert_eq!(constant(1).to_json(), constant(1).to_json());
    }

    /// C1 T2 review regression: `dominant_people_in`'s candidacy loop must
    /// treat "registered but unplaced" differently from "placed with zero
    /// population" — both weighed 0.0 before the fix, so an unplaced species
    /// could win by alphabetical tie-break instead of the doc-promised
    /// `None`. Built with a fauna-only component set at `build_world_to`
    /// (mirrors `fauna_are_skipped_by_settlement_genesis`) so the world
    /// itself places no settlements at all, then queried against the
    /// CANONICAL component set (real lexicon entries for goblin, hobgoblin,
    /// …) so every candidate passes the "is a naming candidate" filter and
    /// none is placed — the exact shape of the bug.
    #[test]
    fn dominant_people_returns_none_when_no_peopled_race_is_placed() {
        use hornvale_kernel::ComponentStore;
        let goblin_bio = hornvale_species::biosphere_registry()
            .get(&KindId("goblin"))
            .expect("the shipped goblin has a biosphere row")
            .clone();
        // A fauna fixture: goblin's biosphere row minus peoplehood — the
        // nested-capacity invariant (Task 2) requires a `Settled` biosphere
        // row to carry all four peopled components, so a fauna-only kind
        // must be non-`Settled` (`Solitary` — the same social form the
        // dragon/xorn menagerie carries).
        let test_beast_bio = hornvale_species::BiosphereTraits {
            social_form: hornvale_species::SocialForm::Solitary,
            ..goblin_bio
        };
        let biosphere: ComponentStore<KindId, hornvale_species::BiosphereTraits> =
            [(KindId("test-beast"), test_beast_bio)]
                .into_iter()
                .collect();
        let family_of: ComponentStore<KindId, &'static str> =
            [(KindId("test-beast"), "test-beast")].into_iter().collect();
        let fauna_only_wc = WorldComponents::from_stores(
            biosphere,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
            hornvale_language::family_proto(),
            family_of,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
        )
        .expect("a fauna-only component set is well-formed (no peopled rows)");

        let world = build_world_to(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
            &fauna_only_wc,
            BuildDepth::Settlements,
        )
        .unwrap();

        let canonical_wc = WorldComponents::assemble().unwrap();
        assert_eq!(
            dominant_people_in(&world, &canonical_wc),
            None,
            "a world with no placed peopled race must have no dominant race"
        );
    }

    /// hornvale#1 regression: seed 42 places two peoples, so every pantheon
    /// names the people who hold it — none is anonymous by position. This is
    /// the test the ticket never had; it fails on the pre-campaign renderer.
    #[test]
    fn seed_42_names_both_its_peoples_pantheons() {
        let world = constant(42);
        assert!(
            placed_peoples(&world).len() > 1,
            "seed 42 places two peoples"
        );
        let ctx = almanac_context(&world).unwrap();
        assert!(!ctx.pantheons.is_empty());
        assert!(
            ctx.pantheons.iter().all(|b| b.attribution.is_some()),
            "with two peoples, every pantheon names its own"
        );
    }

    /// A one-people builder-level fixture: the shipped goblin, placed alone.
    /// Built from a single-kind component set (not a seed search) so the
    /// fixture never depends on which seeds happen to place exactly one
    /// people — the ECS-c3 analogue of the old single-species roster, keyed
    /// by `KindId("goblin")` and composed from the domain registries.
    fn goblin_solo(seed: u64) -> World {
        use hornvale_kernel::ComponentStore;
        let g = KindId("goblin");
        let biosphere: ComponentStore<KindId, _> = [(
            g,
            hornvale_species::biosphere_registry()
                .get(&g)
                .unwrap()
                .clone(),
        )]
        .into_iter()
        .collect();
        let psyche: ComponentStore<KindId, _> =
            [(g, *hornvale_species::psyche_registry().get(&g).unwrap())]
                .into_iter()
                .collect();
        let perception: ComponentStore<KindId, _> =
            [(g, *hornvale_species::perception_registry().get(&g).unwrap())]
                .into_iter()
                .collect();
        let articulation: ComponentStore<KindId, _> = [(
            g,
            *hornvale_language::articulation_registry().get(&g).unwrap(),
        )]
        .into_iter()
        .collect();
        let lexicon: ComponentStore<KindId, _> = [(
            g,
            hornvale_language::lexicon_registry()
                .get(&g)
                .unwrap()
                .clone(),
        )]
        .into_iter()
        .collect();
        let family_of: ComponentStore<KindId, _> = [(g, "goblinoid")].into_iter().collect();
        let wc = crate::components::WorldComponents::from_stores(
            biosphere,
            psyche,
            perception,
            articulation,
            lexicon,
            hornvale_language::family_proto(),
            family_of,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
        )
        .unwrap();
        build_world_from_components(
            Seed(seed),
            &SkyPins::default(),
            SkyChoice::Constant,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
        )
        .unwrap()
    }

    /// The Named's headline one-people rule, exercised at the builder level:
    /// a solo-species roster places exactly one people, and every pantheon
    /// block withholds attribution — the same predicate the People section
    /// uses to keep the legacy unprefixed wording (`settlement_lines`).
    #[test]
    fn one_peoples_pantheons_withhold_attribution() {
        let world = goblin_solo(42);
        assert_eq!(
            placed_peoples(&world).len(),
            1,
            "a solo-species roster places exactly one people"
        );
        let ctx = almanac_context(&world).unwrap();
        assert!(!ctx.pantheons.is_empty());
        assert!(
            ctx.pantheons.iter().all(|b| b.attribution.is_none()),
            "with one people, no pantheon block names its species"
        );
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
        // Task A15a's niche cutover changed what "population" means: it is
        // now the DOMINANT species' own rendered headcount (biomass share
        // divided by its own body mass), not the old flat-K catchment's
        // conserved population unit -- a much smaller, physically grounded
        // number; the old `>= 40` threshold measured the pre-cutover scale
        // and no longer applies. Terrain epoch v4 (rift-and-fit) then moved
        // the seed-42 coastline, so the settlement layout re-derived and
        // the largest coexistence attractor (`village_info`'s first,
        // highest-`mass_total` settlement) re-pinned 17 -> 14. The v4 tuning
        // season moved it again within the epoch: iteration 2's clip-taper
        // widening (`CLIP_TAPER` 0.08 -> 0.16, shelf-fraction recovery)
        // reshaped the seed-42 coast once more, re-pinning 14 -> 8. THREE
        // deliberate identity-moving campaigns now combine on the merged tree:
        // The Confluence re-pointed `carrying_inputs_of`'s freshwater term at
        // `river_proximity` (settlements condense near rivers); The Rains
        // (precipitation epoch) rewrote the moisture field, shifting biomes and
        // the settlement layout; The Demesne replaced `niche_per_species_k`'s
        // `base_carrying × Σuptake` scalar supply with the per-axis dot product
        // (`axis_supply`), redistributing catchments again. The value below is
        // MEASURED on the fully-merged tree (all three), not any branch's alone.
        // Re-pin here (with review) whenever a deliberate terrain/carrying-
        // capacity/moisture change moves world identity.
        // The Living Community epoch: history is the settlement provider now,
        // so `village_info`'s flagship (first `is-settlement` = first alive
        // occupation in the bake's records order) carries the OCCUPATION's peak
        // population, not the demography attractor's rendered headcount. Re-pin
        // 2 -> 118 (measured on the epoch); still a MEASURED value, re-pinned
        // (with review) whenever a deliberate bake/carrying-capacity change
        // moves world identity.
        assert_eq!(
            village.population, 118,
            "the flagship occupation's peak population is pinned at this seed (deep-history bake — SETTLERS_PER_CAPACITY x carrying-capacity, grown over the millennia)"
        );
        // The cascade still runs on the flagship.
        assert!(!hornvale_culture::castes_of(&world, village.id).is_empty());
        assert!(!hornvale_religion::beliefs_of(&world).is_empty());
    }

    #[test]
    fn species_facts_touch_no_pre_existing_predicate() {
        // Ported from the deleted species-genesis test (spec §8): species
        // genesis (`provenance == "species"`) commits facts ONLY under
        // `species-*` predicates or `PEOPLED_BY` — it never writes into
        // another domain's predicate namespace.
        let world = generated(42);
        for fact in world.ledger.iter() {
            if fact.provenance != "species" {
                continue;
            }
            assert!(
                fact.predicate.starts_with("species-")
                    || fact.predicate == hornvale_species::PEOPLED_BY,
                "species-provenance fact touched a non-species predicate: {:?}",
                fact.predicate
            );
        }
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

    /// The Named: the People and Gods sections must answer to ONE predicate,
    /// so they can never disagree about whether to name a world's peoples.
    #[test]
    fn placed_peoples_lists_flagship_holders_in_registry_order() {
        let world = constant(42);
        let placed = placed_peoples(&world);
        assert!(!placed.is_empty(), "seed 42 places at least one people");
        let names: Vec<&str> = placed.iter().map(|(s, _)| *s).collect();
        let mut sorted = names.clone();
        sorted.sort_unstable();
        assert_eq!(
            names, sorted,
            "registry order is alphabetical (BTreeMap keys)"
        );
    }

    /// C2 T5: every placed peopled species gets its own collective entity,
    /// `instance-of` the species kind and named by that people's own word
    /// for "person" (the autonym).
    #[test]
    fn each_placed_people_has_a_named_instance_of_collective() {
        let world = constant(1);
        // at least one entity carries instance-of a placed species kind + a name
        let has = world.ledger.find("instance-of").any(|f| {
            matches!(&f.object, Value::Text(_)) && world.ledger.text_of(f.subject, "name").is_some()
        });
        assert!(has, "a named collective per placed people");
    }

    #[test]
    fn build_world_produces_the_full_cascade() {
        let world = constant(42);
        let places = hornvale_terrain::places(&world);
        assert!(!places.is_empty());
        let village = hornvale_settlement::village_info(&world).expect("village");
        assert!(!hornvale_culture::castes_of(&world, village.id).is_empty());
        // The flagship's own pantheon (not the world total — a default world
        // now carries one pantheon per species-flagship, spec §5). The Living
        // Community epoch's final placement puts the flagship on a cell whose
        // vantage observes one salient phenomenon — re-pin to 1 (measured; a
        // cascade smoke test, the exact belief count is incidental to "the
        // cascade runs").
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
    fn build_world_from_assembled_components_matches_build_world_byte_for_byte() {
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
            let wc = WorldComponents::assemble().unwrap();
            let b = build_world_from_components(
                seed,
                &SkyPins::default(),
                SkyChoice::Generated,
                &TerrainPins::default(),
                &sp,
                &wc,
            )
            .unwrap();
            let fa: Vec<_> = a.ledger.iter().collect();
            let fb: Vec<_> = b.ledger.iter().collect();
            assert_eq!(
                fa, fb,
                "seed {seed:?}: assembled-components build must equal build_world exactly"
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

    /// The Long Count: a generated world's almanac context carries the
    /// founding sightline under The Sky and the brightening deep-time line
    /// under Deep Time.
    #[test]
    fn almanac_context_carries_sightline_and_slow_fire() {
        let world = generated(42);
        let ctx = almanac_context(&world).unwrap();
        let lines = ctx.night_sky_lines.expect("generated sky");
        assert!(lines.alignment.is_some());
        assert!(
            ctx.deep_time_lines.iter().any(|l| l.contains("slow fire")),
            "the brightening line reaches Deep Time"
        );
    }

    #[test]
    fn sky_and_climate_reports_come_from_the_composition_root() {
        let world = constant(42);
        let sky = sky_report(&world, hornvale_kernel::WorldTime { day: 0.0 }).unwrap();
        assert!(sky.description.contains("zenith"));
        let climate = climate_report(&world);
        assert_eq!(climate.temperature_c, 18.0);
    }

    /// The Firmament: possession's sky report narrates the felt weather, not
    /// just the astronomical scene — `sky_phrase` shared with the almanac's
    /// headline lines (DRY). Deterministic for a fixed `(seed, day)`.
    #[test]
    fn the_sky_report_names_the_weather() {
        let world = generated(42);
        let report = sky_report(&world, hornvale_kernel::WorldTime { day: 10.0 }).unwrap();
        let text = &report.description;
        assert!(
            ["clear", "fair", "overcast", "rain", "storm"]
                .iter()
                .any(|w| text.contains(w)),
            "the sky report must narrate the weather: {text}"
        );

        let again = sky_report(&world, hornvale_kernel::WorldTime { day: 10.0 }).unwrap();
        assert_eq!(report, again, "the weather clause is deterministic");
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

    /// Night-sky stage 2: two pinned wanderers each get their own line
    /// under The Sky — a rock's or a giant's round of the sun, in AU-Kepler
    /// days, with the inner (elongation-bound) wanderer flagged as a
    /// morning-and-evening star.
    #[test]
    fn wanderer_lines_render_one_sentence_per_pinned_wanderer() {
        let pins = SkyPins {
            wanderers: Some(2),
            ..SkyPins::default()
        };
        let world = build_world(
            Seed(1),
            &pins,
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = night_sky_lines(&world).unwrap().unwrap();
        assert_eq!(lines.wanderers.len(), 2);
        for line in &lines.wanderers {
            assert!(line.contains("wanderer rounds the sun every"));
            assert!(line.contains("days"));
        }

        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.night_sky_lines.unwrap().wanderers, lines.wanderers);
    }

    /// Night-sky stage 3: seed 6's default (unpinned) generation carries
    /// several figures with exactly one on the ecliptic (verified against
    /// the astronomy-layer sweep), so its almanac line reports a nonzero
    /// total and singular ecliptic-count grammar ("1 stands", not
    /// "1 stand").
    #[test]
    fn seed_6_generated_default_has_a_figures_summary_line() {
        let world = generated(6);
        let lines = night_sky_lines(&world).unwrap().unwrap();
        assert_eq!(lines.figures.len(), 1);
        let line = &lines.figures[0];
        assert!(line.starts_with("The sky holds "));
        assert!(line.contains("figures;"));
        assert!(line.contains("1 stands on the sun's road."));
        assert!(!line.contains("holds 0 figures"));

        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.night_sky_lines.unwrap().figures, lines.figures);
    }

    /// The seed-42 default world's almanac context carries dated eclipse
    /// lines and the recurrence-ladder lines (it has moons).
    #[test]
    fn almanac_context_dates_eclipses_and_the_ladder() {
        let world = generated(42);
        let ctx = almanac_context(&world).unwrap();
        let lines = ctx.night_sky_lines.expect("generated sky");
        assert!(!lines.eclipses.is_empty());
        assert!(
            lines.eclipses.iter().any(|l| l.contains("parade")),
            "ladder lines present"
        );
    }

    /// A sky with zero figures renders no figures line at all (never "The
    /// sky holds 0 figures").
    #[test]
    fn a_sky_with_zero_figures_renders_no_figures_line() {
        let world = generated(1);
        let lines = night_sky_lines(&world).unwrap().unwrap();
        assert!(
            lines.figures.is_empty(),
            "seed 1 has zero figures at genesis"
        );
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
    fn pinning_a_fauna_species_fails_loudly_instead_of_panicking() {
        // The Task 4 menagerie (windows/worldgen fold-in): a `--species`
        // pin naming a biosphere-only (fauna) kind must never reach the
        // peopled pass downstream — that pass has no psyche row for a fauna
        // kind. Fauna never settle, so the pin must fail
        // loudly with the physical reason (constitution: "pins fail
        // loudly"), the same `BuildError::Pins` shape `def_in` already uses
        // for an unknown name.
        let result = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins {
                species: Some("white-dragon".to_string()),
            },
        );
        assert!(result.is_err(), "a fauna pin must never build a world");
        let err = result.err().unwrap();
        assert!(
            matches!(err, BuildError::Pins(_)),
            "expected a graceful BuildError::Pins, got {err}"
        );
        if let BuildError::Pins(reason) = err {
            assert!(
                reason.contains("white-dragon"),
                "reason should name the offending species: {reason}"
            );
        }
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
        let world = build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &pins,
            &SettlementPins::default(),
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
        // Seed 42's default canonical-level globe carries a delta lobe and
        // playa fill but no waterfall (measured directly against the
        // terrain domain): the third line is the point-observation
        // notable.
        assert_eq!(lines.len(), 3);
        assert!(lines[0].contains("plates"));
        assert!(lines[1].contains("above the sea"));
        assert_eq!(lines[2], "Notable: the Great Delta, salt flats.");
    }

    #[test]
    fn land_lines_name_point_observation_notables_when_present() {
        let world = constant(42);
        let terrain = terrain_of(&world).unwrap();
        // Ground truth the notable line against the provider directly,
        // rather than re-asserting the exact seed-42 bytes twice.
        assert!(terrain.waterfalls().is_empty());
        assert!(!terrain.deltas().is_empty());
        assert!(!terrain.playas().is_empty());

        let lines = land_lines(&world).unwrap();
        let notable = lines.last().unwrap();
        assert!(!notable.contains("the Great Falls"), "no waterfalls exist");
        assert!(notable.contains("the Great Delta"));
        assert!(notable.contains("salt flats"));
    }

    #[test]
    fn ground_lines_name_the_dominant_rock_and_soil() {
        let world = constant(42);
        let lines = ground_lines(&world).unwrap();
        assert!(!lines.is_empty());
        assert!(lines[0].contains("The land is mostly"));
        assert!(lines[0].contains("its soils mostly"));
    }

    #[test]
    fn ground_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.ground_lines, ground_lines(&world).unwrap());
        let doc = hornvale_almanac::render(&ctx);
        assert!(doc.contains("## The Ground"));
    }

    #[test]
    fn water_lines_report_a_nonzero_fresh_water_share() {
        let world = constant(42);
        let lines = water_lines(&world).unwrap();
        assert_eq!(lines.len(), 1);
        assert!(lines[0].contains("Fresh water"));
        assert!(lines[0].contains('%'));
    }

    #[test]
    fn water_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.water_lines, water_lines(&world).unwrap());
        let doc = hornvale_almanac::render(&ctx);
        assert!(doc.contains("## The Waters"));
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

    // A spinning world names both sample sites and reads a real range for
    // the driest interior; a locked world has no rotation-scale day/night
    // cycle at all, so its diurnal lines are honestly empty.
    #[test]
    fn diurnal_lines_show_dry_interior_and_ocean_and_are_empty_when_locked() {
        use hornvale_astronomy::RotationPin;
        let spinning = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::Normal),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = diurnal_lines(&spinning).unwrap();
        assert_eq!(
            lines.len(),
            2,
            "expected one line per sample site: {lines:?}"
        );
        assert!(lines[0].contains("The driest interior"));
        assert!(lines[1].contains("The open ocean"));

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
        assert!(
            diurnal_lines(&locked).unwrap().is_empty(),
            "locked worlds have no diurnal cycle to report"
        );
    }

    #[test]
    fn diurnal_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.diurnal_lines, diurnal_lines(&world).unwrap());
    }

    // A spinning world with ocean names a coastal current direction; a
    // locked world drives no current at all (`ocean_current_field` is
    // all-zero when there are no circulation bands), so its seas line is
    // honestly empty.
    #[test]
    fn seas_lines_report_a_coastal_current_and_are_empty_when_locked() {
        use hornvale_astronomy::RotationPin;
        let spinning = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::Normal),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = seas_lines(&spinning).unwrap();
        assert_eq!(lines.len(), 1, "expected one seas line: {lines:?}");
        assert!(lines[0].contains("The seas:"));

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
        assert!(
            seas_lines(&locked).unwrap().is_empty(),
            "locked worlds drive no current to report"
        );
    }

    #[test]
    fn seas_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.seas_lines, seas_lines(&world).unwrap());
    }

    // A spinning world names both sample sites with a real precipitation
    // readout; unlike `diurnal_lines`/`seas_lines`, a locked world is NOT
    // empty — precipitation, snow fraction, and regime are all defined
    // regardless of rotation regime (The Rains).
    #[test]
    fn rains_lines_report_both_sample_sites_and_are_present_on_a_spinning_world() {
        use hornvale_astronomy::RotationPin;
        let spinning = build_world(
            Seed(42),
            &SkyPins {
                rotation: Some(RotationPin::Normal),
                ..SkyPins::default()
            },
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap();
        let lines = rains_lines(&spinning).unwrap();
        assert_eq!(
            lines.len(),
            2,
            "expected one line per sample site: {lines:?}"
        );
        assert!(lines[0].contains("The driest interior"));
        assert!(lines[1].contains("The open ocean"));
        assert!(
            lines[0].contains("mm of"),
            "expected an mm readout: {}",
            lines[0]
        );
        // The regime-floor guard (both arms exercised on seed 42): the driest
        // interior is effectively rainless, so it carries no seasonal-regime
        // parenthetical (a "monsoon" with no rain is a contradiction) and never
        // reads "about 0 mm"; the open ocean is well above the floor and keeps
        // its regime label.
        assert!(
            !lines[0].contains('('),
            "a near-rainless cell must not claim a seasonal regime: {}",
            lines[0]
        );
        assert!(
            !lines[0].contains("about 0 mm"),
            "a cell that rounds to zero should read 'under 1 mm', not 'about 0 mm': {}",
            lines[0]
        );
        assert!(
            lines[1].contains('('),
            "a wet cell above the arid floor still names its regime: {}",
            lines[1]
        );

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
        assert!(
            !rains_lines(&locked).unwrap().is_empty(),
            "precipitation is defined regardless of rotation regime"
        );
    }

    #[test]
    fn rains_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.rains_lines, rains_lines(&world).unwrap());
    }

    #[test]
    fn firmament_lines_report_the_sky_at_both_sample_sites() {
        let world = generated(42);
        let lines = firmament_lines(&world).unwrap();
        assert_eq!(
            lines.len(),
            2,
            "one weather line per sample site: {lines:?}"
        );
        assert!(lines[0].contains("The driest interior"));
        assert!(lines[1].contains("The open ocean"));
        // Each names a sky condition word.
        for l in &lines {
            assert!(
                ["clear", "fair", "overcast", "rain", "storm"]
                    .iter()
                    .any(|w| l.contains(w)),
                "a weather line must name a sky condition: {l}"
            );
        }
    }

    #[test]
    fn firmament_lines_feed_the_almanac_context() {
        let world = constant(42);
        let ctx = almanac_context(&world).unwrap();
        assert_eq!(ctx.firmament_lines, firmament_lines(&world).unwrap());
    }

    #[test]
    fn constant_sky_world_still_has_a_climate() {
        let world = constant(42);
        let climate = climate_of(&world).unwrap();
        assert!(climate.geosphere().cell_count() > 0);
    }

    #[test]
    fn soil_of_land_cells_show_at_least_two_orders() {
        use std::collections::BTreeSet;
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let geo = terrain.geosphere();
        let soil = soil_of(&terrain, &climate, geo);
        let land_orders: BTreeSet<_> = geo
            .cells()
            .filter(|c| !terrain.is_ocean(*c))
            .map(|c| *soil.get(c))
            .collect();
        assert!(!land_orders.is_empty(), "seed 42 has no land cells");
        assert!(
            land_orders.len() >= 2,
            "soil felt monolithic: {land_orders:?}"
        );
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
        // vocabulary (the same construction `build_world_from_components`
        // performs per-flagship), not a fixed goblin-baseline
        // `PsychSummary::default()` a single-species world could get away
        // with.
        let flagship_species = hornvale_species::species_of(&world, village.id)
            .expect("the flagship settlement has a species fact");
        // Reconstruct mind + speech through the same canonical component set
        // the production path (`build_to`) now uses (ECS c3).
        let wc = WorldComponents::assemble().expect("well-formed canonical registries");
        // `flagship_species` is free text read from the ledger (a committed
        // `Value::Text`), not a `KindId` — resolve it to its canonical
        // `'static` label within the component set.
        let kind = *wc
            .biosphere
            .ids()
            .find(|k| k.0 == flagship_species.as_str())
            .expect("flagship species must be in the registry");
        let psych_v = wc
            .psyche
            .get(&kind)
            .expect("peopled pass over a fauna kind");
        let lex = wc
            .lexicon
            .get(&kind)
            .expect("peopled pass over a fauna kind");
        let psych = hornvale_culture::PsychSummary {
            threat_response: psych_v.threat_response,
            time_horizon: psych_v.time_horizon,
            communal: psych_v.sociality == hornvale_species::Sociality::Communal,
            rank_status: psych_v.status_basis == hornvale_species::StatusBasis::Rank,
            vocabulary: hornvale_culture::RoleVocabulary {
                worker_override: lex.worker_override.map(str::to_string),
                warrior: lex.warrior.to_string(),
                artisan: lex.artisan.to_string(),
                shaman: lex.shaman.to_string(),
                top: lex.top.to_string(),
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
        // Seed 5 (re-pinned under The Living Community epoch, this merge):
        // history is the sole settlement placer now, and the deep-history
        // bake re-placed every world, so the seed that separates the two
        // rotation regimes' flagship cascades shifted again. Seed 1 (the
        // prior anchor) now coincides across regimes; a survey of seeds
        // 1..=25 found seed 5 the nearest that still separates them (and it
        // also separates the two regimes' pantheon heads — see
        // `the_pantheon_reorganizes_between_spinning_and_locked`).
        use hornvale_astronomy::RotationPin;
        let spinning = generated(5);
        let locked = build_world(
            Seed(5),
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
    fn settlement_populations_are_the_conserved_field_readout() {
        // Settlement populations are the demography catchment readout, not a
        // draw: a peopled world has positive total settlement population.
        let world = generated(42);
        let total: f64 = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .filter_map(|f| {
                match world
                    .ledger
                    .value_of(f.subject, hornvale_settlement::POPULATION)
                {
                    Some(Value::Number(n)) => Some(*n),
                    _ => None,
                }
            })
            .sum();
        assert!(total > 0.0, "a peopled world has positive total population");
    }

    #[test]
    fn no_zero_population_settlements() {
        // Full-pipeline regression guard for the founder-floor fix (design
        // spec §5, "no peopleless settlements"): the domain-level unit test
        // `founder_floor_never_places_a_zero_population_settlement`
        // (`domains/demography/src/founder.rs`) exercises the flooring logic
        // in isolation with a synthetic trace-K species; this asserts the
        // same invariant end-to-end on a real built world, where the emit
        // boundary's `.round() as u32` is what would actually commit a
        // POPULATION == 0 fact if the domain-level floor ever regressed.
        let world = generated(42);
        for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
            let pop = world
                .ledger
                .value_of(f.subject, hornvale_settlement::POPULATION);
            assert_ne!(
                pop,
                Some(&Value::Number(0.0)),
                "settlement {:?} committed a zero population",
                f.subject
            );
        }
    }

    /// The Long Count: every settlement in a spinning generated world
    /// carries its founding solstice-sunrise azimuth; it holds already at
    /// `BuildDepth::Settlements` — the alignments pass runs immediately
    /// after settlement placement, before the settlement-depth early
    /// return, so a shallow build gets alignments too (spec §4's "built to
    /// settlement depth or deeper").
    #[test]
    fn settlements_carry_founding_alignments() {
        let wc = WorldComponents::assemble().unwrap();
        let world = build_world_to(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .unwrap();
        let settlements: Vec<_> = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .map(|f| f.subject)
            .collect();
        assert!(!settlements.is_empty());
        for s in &settlements {
            let n = world
                .ledger
                .find(facts::FOUNDING_SOLSTICE_AZIMUTH_DEGREES)
                .filter(|f| f.subject == *s)
                .count();
            assert_eq!(n, 1, "settlement {s:?}");
        }
    }

    /// Task 3 (The Seam): a biosphere-only (fauna) kind carries no peopled
    /// components and must never reach settlement genesis — only settling,
    /// speaking species place villages. The fixture is a single-kind component
    /// set with a biosphere + family row but NO peopled rows (Task 4 mints the
    /// real menagerie; this is a minimal stand-in for the guard alone).
    #[test]
    fn fauna_are_skipped_by_settlement_genesis() {
        use hornvale_kernel::ComponentStore;
        let goblin_bio = hornvale_species::biosphere_registry()
            .get(&KindId("goblin"))
            .expect("the shipped goblin has a biosphere row")
            .clone();
        // A fauna fixture: goblin's biosphere row minus peoplehood — the
        // nested-capacity invariant (Task 2) requires a `Settled` biosphere
        // row to carry all four peopled components, so a fauna-only kind
        // must be non-`Settled` (`Solitary` — the same social form the
        // dragon/xorn menagerie carries).
        let test_beast_bio = hornvale_species::BiosphereTraits {
            social_form: hornvale_species::SocialForm::Solitary,
            ..goblin_bio
        };
        let biosphere: ComponentStore<KindId, hornvale_species::BiosphereTraits> =
            [(KindId("test-beast"), test_beast_bio)]
                .into_iter()
                .collect();
        let family_of: ComponentStore<KindId, &'static str> =
            [(KindId("test-beast"), "test-beast")].into_iter().collect();
        let wc = WorldComponents::from_stores(
            biosphere,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
            hornvale_language::family_proto(),
            family_of,
            ComponentStore::new(),
            ComponentStore::new(),
            ComponentStore::new(),
        )
        .expect("a fauna-only component set is well-formed (no peopled rows)");

        let world = build_world_to(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
            &wc,
            BuildDepth::Settlements,
        )
        .unwrap();
        assert!(
            world
                .ledger
                .find(hornvale_settlement::IS_SETTLEMENT)
                .next()
                .is_none(),
            "a fauna-only roster must place no settlements"
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
        // Seed 5 (re-pinned under The Living Community epoch, this merge):
        // history is the sole settlement placer now and re-placed every
        // world, so the seed that best separates the two regimes' pantheon
        // heads shifted again. Seed 5 is the shared anchor with
        // `locked_rotation_changes_the_flagship_cascade` (its flagship
        // cascade AND its pantheon head both separate across regimes there).
        use hornvale_astronomy::RotationPin;
        let spinning = generated(5);
        let locked = build_world(
            Seed(5),
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
        // The head deity reorganizes with the sun's rotation regime. The
        // A15a niche cutover had pinned the locked head to Eternal (the
        // fixed sun) at the then-flagship cell; The Living Community epoch's
        // re-placement REVERSED that — a survey of seeds 1..=25 finds the
        // locked head is now dominantly the felt-tide "Ambient" default
        // (24/25 seeds; Eternal survives only at the odd seed 16). The
        // flagship no longer sits reliably at the substellar point, so the
        // fixed-sun-heads-the-pantheon illustration no longer generalizes;
        // re-pin to the measured, dominant Ambient. The essential property —
        // the pantheon REORGANIZES between the regimes (here Cyclic ->
        // Ambient) — is unchanged and still asserted below.
        assert_eq!(
            head_sentiment(&locked),
            Some(hornvale_religion::Sentiment::Ambient),
            "locked world (post-epoch): the felt-tide default heads the pantheon"
        );
        assert_ne!(
            head_sentiment(&spinning),
            Some(hornvale_religion::Sentiment::Ambient),
            "spinning world: not an ambient head deity"
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
        let per = hornvale_species::perception_registry();
        assert!(perception_lens(per.get(&KindId("goblin")).unwrap()).is_identity());
    }

    #[test]
    fn kobold_lens_matches_the_spec_derivation() {
        let per = hornvale_species::perception_registry();
        let lens = perception_lens(per.get(&KindId("kobold")).unwrap());
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
        let psy = hornvale_species::psyche_registry();
        let v = voice_params(psy.get(&KindId("goblin")).unwrap());
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
        // NAME_GLOSS is kernel-core (ecs-c6 T3): `World::new` registers it,
        // not `register_all` — build the registry the way `build_to` does.
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        register_all(&mut world.registry).unwrap();
        let def = world
            .registry
            .predicate(hornvale_kernel::NAME_GLOSS)
            .expect("name-gloss must be registered");
        assert!(def.functional, "an entity has exactly one glossed meaning");
    }

    #[test]
    fn domains_roster_crate_names_are_unique_and_nonempty() {
        let mut names: Vec<&str> = DOMAINS.iter().map(|d| d.crate_name()).collect();
        assert_eq!(names.len(), 10, "expected ten domains in the roster");
        assert!(names.iter().all(|n| !n.is_empty()));
        let before = names.len();
        names.sort_unstable();
        names.dedup();
        assert_eq!(names.len(), before, "duplicate crate_name in DOMAINS");
    }

    #[test]
    fn domains_roster_registers_language_after_its_lenders() {
        // language::register_concepts references (and, if absent, claims) concepts
        // owned by these domains, so it must appear AFTER all of them in DOMAINS,
        // or register_all conflicts. This pins the registration-order invariant.
        let names: Vec<&str> = DOMAINS.iter().map(|d| d.crate_name()).collect();
        let idx = |n: &str| {
            names
                .iter()
                .position(|x| *x == n)
                .unwrap_or_else(|| panic!("{n} missing from DOMAINS"))
        };
        let language = idx("hornvale-language");
        for lender in [
            "hornvale-terrain",
            "hornvale-climate",
            "hornvale-settlement",
            "hornvale-species",
            "hornvale-religion",
        ] {
            assert!(
                idx(lender) < language,
                "{lender} must be registered before hornvale-language"
            );
        }
    }

    #[test]
    fn register_all_via_roster_registers_name_gloss() {
        // NAME_GLOSS is kernel-core (ecs-c6 T3): registered by `World::new`
        // itself, ahead of `register_all`'s domain roster loop — this test now
        // pins that `build_to`'s actual registration sequence (`World::new`
        // then `register_all`) leaves it registered, not that `register_all`
        // registers it a second time (that would be a `RegistryError`).
        let mut world = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
        register_all(&mut world.registry).expect("register_all succeeds after World::new");
        assert!(
            world
                .registry
                .predicate(hornvale_kernel::NAME_GLOSS)
                .is_some()
        );
    }

    #[test]
    fn settlements_and_deities_gain_name_gloss_facts_when_a_gloss_exists() {
        let world = generated(42);
        let settlement_glossed = world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .any(|f| {
                world
                    .ledger
                    .text_of(f.subject, hornvale_kernel::NAME_GLOSS)
                    .is_some()
            });
        let deity_glossed = world.ledger.find(hornvale_religion::IS_BELIEF).any(|f| {
            world
                .ledger
                .text_of(f.subject, hornvale_kernel::NAME_GLOSS)
                .is_some()
        });
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
            let Some(gloss) = world.ledger.text_of(id, hornvale_kernel::NAME_GLOSS) else {
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
        let wc = WorldComponents::assemble().unwrap();
        let daughters = family_daughters(&world, &wc, "kobold");
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

    #[test]
    fn substrate_field_is_finite_and_insolation_peaks_at_the_equator() {
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _year, _year_phase_offset) =
            stellar_inputs(&sky);
        let field = substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );
        for cell in geo.cells() {
            let s = field.get(cell);
            assert!(s.temperature_c.is_finite());
            assert!((0.0..=1.0).contains(&s.moisture));
            assert!(s.insolation.is_finite() && s.insolation >= 0.0);
            assert!(s.elevation.is_finite());
        }
        // Annual-mean insolation is higher at the equator than at a pole.
        let eq = annual_mean_insolation(0.0, obliquity_deg, insolation_scalar);
        let pole = annual_mean_insolation(85.0, obliquity_deg, insolation_scalar);
        assert!(eq > pole, "equator {eq} > pole {pole}");
    }

    /// SKY-24 guard: on a `Locked` world, `substrate_field`'s insolation
    /// rewards the substellar geometry — peaking at the substellar cell,
    /// zero at the antistellar cell (`cos_theta < 0`, floored), and
    /// strictly between the two at a terminator-ring cell.
    #[test]
    fn locked_insolation_peaks_at_substellar_and_zeroes_on_night_side() {
        let world = build_world(
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
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _year, _year_phase_offset) =
            stellar_inputs(&sky);
        assert!(
            matches!(regime, RotationRegime::Locked),
            "the Locked rotation pin must yield RotationRegime::Locked"
        );
        let field = substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );

        // Locate the substellar cell (max substellar_cosine), the
        // antistellar cell (min), and the cell nearest the terminator
        // (cosine closest to zero) by scanning the geosphere.
        let mut substellar = None;
        let mut antistellar = None;
        let mut terminator = None;
        for cell in geo.cells() {
            let cos = hornvale_climate::substellar_cosine(geo.position(cell));
            if substellar.map(|(_, best)| cos > best).unwrap_or(true) {
                substellar = Some((cell, cos));
            }
            if antistellar.map(|(_, best)| cos < best).unwrap_or(true) {
                antistellar = Some((cell, cos));
            }
            if terminator
                .map(|(_, best): (_, f64)| cos.abs() < best.abs())
                .unwrap_or(true)
            {
                terminator = Some((cell, cos));
            }
        }
        let (substellar_cell, _) = substellar.unwrap();
        let (antistellar_cell, antistellar_cos) = antistellar.unwrap();
        let (terminator_cell, _) = terminator.unwrap();
        assert!(
            antistellar_cos < 0.0,
            "the antistellar cell must sit on the night side"
        );

        let ins = |c| field.get(c).insolation;
        assert!(
            ins(substellar_cell) > ins(terminator_cell),
            "substellar {} must exceed terminator {}",
            ins(substellar_cell),
            ins(terminator_cell)
        );
        assert!(
            ins(terminator_cell) >= 0.0,
            "terminator insolation must not be negative"
        );
        assert_eq!(
            ins(antistellar_cell),
            0.0,
            "the antistellar cell reads zero insolation (Lambert floor)"
        );
    }

    /// SKY-24 hard guard: on a `Spinning` world, `substrate_field`'s
    /// insolation is EXACTLY `annual_mean_insolation` per cell — the
    /// Spinning arm is the unmodified pre-fix computation, so no spinning
    /// world's save bytes may move.
    #[test]
    fn spinning_worlds_are_byte_identical_across_the_fix() {
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _year, _year_phase_offset) =
            stellar_inputs(&sky);
        assert!(
            matches!(regime, RotationRegime::Spinning { .. }),
            "seed 42's default generated sky must be Spinning"
        );
        let field = substrate_field(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
        );
        for cell in geo.cells() {
            let expected =
                annual_mean_insolation(geo.coord(cell).latitude, obliquity_deg, insolation_scalar);
            assert_eq!(
                field.get(cell).insolation,
                expected,
                "cell {cell:?}: spinning insolation must equal the unchanged latitude-only formula exactly"
            );
        }
    }

    #[test]
    fn niche_k_differentiates_species_with_different_temperature_optima() {
        let world = generated(42);
        let terrain = terrain_of(&world).unwrap();
        let climate = climate_of(&world).unwrap();
        let sky = sky_of(&world).unwrap();
        let geo = terrain.geosphere();
        let (insolation_scalar, obliquity_deg, regime, _year, _year_phase_offset) =
            stellar_inputs(&sky);

        let wc = WorldComponents::assemble().unwrap();
        let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
        let bio: Vec<&hornvale_species::BiosphereTraits> =
            wc.biosphere.iter().map(|(_, b)| b).collect();
        let ks = niche_per_species_k(
            geo,
            &terrain,
            &climate,
            obliquity_deg,
            insolation_scalar,
            &regime,
            &bio,
        );

        // wc.biosphere.ids() = registry() key order (ascending KindId):
        // alphabetical -> bugbear, goblin, hobgoblin, kobold.
        let kobold_tag = names
            .iter()
            .position(|n| *n == "kobold")
            .expect("kobold in registry") as u32;
        let bugbear_tag = names
            .iter()
            .position(|n| *n == "bugbear")
            .expect("bugbear in registry") as u32;
        let k_cool = &ks.iter().find(|(tag, _)| *tag == kobold_tag).unwrap().1;
        let k_warm = &ks.iter().find(|(tag, _)| *tag == bugbear_tag).unwrap().1;

        // Over cells where both are positive, the ratio k_cool/k_warm is NOT
        // constant — the direct refutation of flat-NPP proportionality.
        let mut ratios: Vec<f64> = geo
            .cells()
            .filter_map(|c| {
                let (a, b) = (*k_cool.get(c), *k_warm.get(c));
                (a > 0.0 && b > 0.0).then_some(a / b)
            })
            .collect();
        assert!(ratios.len() > 10, "need enough co-occupied cells");
        ratios.sort_by(f64::total_cmp);
        let (lo, hi) = (ratios.first().unwrap(), ratios.last().unwrap());
        assert!(
            hi / lo > 1.5,
            "K fields are niche-differentiated, not proportional: lo={lo} hi={hi}"
        );
    }

    /// The three build-local reads
    /// [`menagerie_full_roster_dominant_breakdown`] returns: every kind's
    /// name (biosphere order, the `tag`-indexable slice), the per-kind
    /// dominant-cell count, and that count re-keyed by name and sorted for
    /// a readable failure message.
    type DominantBreakdown = (
        Vec<&'static str>,
        std::collections::BTreeMap<u32, usize>,
        Vec<(&'static str, usize)>,
    );

    /// The Seam's behavioral exit criterion (Task 6, ledger #48): does the
    /// full 16-species HABITAT stack (fauna included — distinct from the
    /// peopled-only SETTLEMENT stack `species_pin_isolation` etc. probe)
    /// show the menagerie breaking the goblinoid "oatmeal"? Packs the whole
    /// roster through the exact `niche_per_species_k` -> `coexist::pack`
    /// pipeline settlement genesis and `demography_report` share (same
    /// frozen `BETA`/`FLOOR`), then reads the per-cell DOMINANT species —
    /// the greatest realized individual density, tie-broken to the lowest
    /// species id (`report.stack.density` is already tag-ascending, so
    /// "replace only on strictly greater" keeps the first-seen id on an
    /// exact tie, matching `total_cmp`'s determinism).
    ///
    /// CALIBRATION FINDING (2026-07-16, seed 42): this does NOT emerge.
    /// Exactly 2 distinct dominants (`twig-blight`: 1801 cells, `rust-
    /// monster`: 1685 cells) sweep all 3486 dominated land cells; every one
    /// of the other 14 species (the 4 peoples AND the 10 other fauna,
    /// including `treant`, `xorn`, and all three chromatic dragons) is
    /// PRESENT with positive density at nearly every land cell but never
    /// the local arg-max, so each holds zero strongholds. This is not
    /// richer than the peopled-only 2-way split (goblin/hobgoblin,
    /// `beta_calibration_freeze`/Task 5) — it is the SAME shape, just with
    /// two different (smaller-bodied) winners.
    ///
    /// Root cause (traced, not guessed): `coexist::pack` converts a
    /// species' competition SHARE to a per-individual DENSITY by dividing
    /// by `footprint::home_range(mass)`, which scales super-linearly in
    /// body mass (Kleiber exponent 1.25). `treant` (1800 kg) vs its
    /// understory cousin `twig-blight` (5 kg) is a 360x mass ratio ->
    /// ~1568x home-range ratio; `white/red/black-dragon` (2200-2700 kg) vs
    /// `owlbear` (450 kg) is a ~5-6x mass ratio -> ~7x home-range ratio.
    /// Observed max per-cell density confirms the direction: `treant`
    /// 0.00728 vs `twig-blight` 0.613 (~84x), `xorn` 0.0367 vs
    /// `rust-monster` 0.307 (~8x) — the mighty specialists' resource-share
    /// advantage (potency-bought sovereignty floor, obligate niche fit)
    /// never closes a mass gap this large in a per-INDIVIDUAL metric. This
    /// is exactly the lever Task 4's report flagged forward to Task 6
    /// ("Mass flags... calibrate if biogeography-emerges needs it") and
    /// the kind of authored-value retune the B2b precedent handles — a
    /// controller/Nathan calibration call (fauna mass and/or sovereignty/
    /// devotion coefficients), not something this task retunes unilaterally.
    ///
    /// The preregistered `>= 6` target is NOT weakened to the observed value:
    /// the-demesne T3 splits this group into a pinned Stage-1 ACHIEVEMENT test
    /// (`menagerie_distinct_dominants_diversify_and_xorn_holds_a_stronghold`,
    /// un-ignored, asserts the measured 4 + xorn's stronghold) and the
    /// still-`#[ignore]`d preregistered `>= 6` target
    /// (`menagerie_distinct_dominants_reach_the_preregistered_six`), plus the
    /// treant (mass-calibration) and dragon (Stage-2) splits — each `#[ignore]`d
    /// with its true reason rather than forced, deleted, or silently rebased.
    /// Full breakdown, per-species max-density trace, and the calibration
    /// writeup: `.superpowers/sdd/task-6-report.md`.
    ///
    /// UPDATE (BIO-35, the-demesne T2): `niche_per_species_k` no longer
    /// scales one shared `base_carrying` field by each species' summed
    /// uptake — it now dot-products the uptake vector against per-axis
    /// supply fields (`axis_supply`; `windows/worldgen/tests/demesne.rs`).
    /// Re-measured post-repoint: distinct dominants rose 2 -> 4 (`xorn`
    /// 29136 cells, `rust-monster` 9551, `twig-blight` 1328, `goblin` 947)
    /// — `xorn`'s pure-`MINERAL` niche now tracks its own spatial supply
    /// field instead of a rescaled copy of everyone else's. `treant` is
    /// STILL swept by `twig-blight` (the Kleiber home-range root cause
    /// above is untouched by T2 — a resource-AXIS fix, not a body-mass
    /// one) and no chromatic dragon/`owlbear` ever wins (their pure-
    /// `ANIMAL_PREY` niche reads T2's placeholder zero supply — trophic
    /// wiring is explicitly out of Stage 2's scope). This test's `>= 6`
    /// bar and its `treant`/dragon assertions are therefore still correctly
    /// unmet; it stays `#[ignore]`d for the same calibration reason.
    ///
    /// UPDATE (BIO-35, the-demesne T3): re-measured after T3's settlement-
    /// count investigation (`FORAGE_FRACTION` swept 0.5..3.0, kept at 0.5 —
    /// see `windows/worldgen/tests/confluence.rs`'s settlement-count test
    /// for the full writeup; the sweep left `niche_per_species_k`'s
    /// per-cell dominance unchanged from T2's reading). The breakdown is
    /// BYTE-IDENTICAL to T2's: `[xorn: 29136, rust-monster: 9551,
    /// twig-blight: 1328, goblin: 947]`, 4 distinct dominants. Splitting
    /// this test three ways rather than un-ignoring the whole thing, per
    /// what is actually measured (not faked):
    ///
    /// - **Un-ignored**: distinct dominants materially exceed the pre-
    ///   repoint baseline of 2 (the-demesne's own emergence claim, mirrored
    ///   from `demesne.rs`'s `settlements_and_dominants_diversify_on_seed_42`),
    ///   and `xorn` — the pure-`MINERAL` niche the per-axis supply was built
    ///   to unlock — holds a real, large stronghold (29136 cells). Both are
    ///   Stage-1-reachable and both clear.
    /// - **Still `#[ignore]`d, mass/sovereignty calibration (NOT Stage 2)**:
    ///   `treant` holds ZERO strongholds. Measured, not faked: the
    ///   per-axis supply gave `treant` its OWN photosynthate-tracking
    ///   field (unlike the old shared-scalar model), but `twig-blight`
    ///   still wins every contested cell — the root cause traced above
    ///   (Kleiber home-range scaling: `treant` at 1800 kg has a ~1568x
    ///   larger home range than `twig-blight` at 5 kg, so `treant`'s
    ///   resource-share advantage never survives the per-INDIVIDUAL
    ///   density conversion) is orthogonal to the resource-AXIS fix T1/T2
    ///   made and is untouched by it. This is task 6's original finding,
    ///   confirmed still true after the-demesne; it awaits a body-mass /
    ///   sovereignty-devotion retune, not a resource-supply change.
    /// - **Still `#[ignore]`d, Stage 2 (`ANIMAL_PREY` field)**: no
    ///   chromatic dragon (or `owlbear`) ever wins a cell — their pure-
    ///   `ANIMAL_PREY` niche reads Stage 1's placeholder-zero supply, so
    ///   they can never out-compete a species riding a real axis. Trophic
    ///   wiring is explicitly Stage 2's job.
    ///
    /// Returns every kind's name (biosphere order, the `tag`-indexable
    /// slice), the per-kind dominant-cell count, and that count re-keyed by
    /// name and sorted for a readable failure message ([`DominantBreakdown`]).
    fn menagerie_full_roster_dominant_breakdown(seed: u64) -> DominantBreakdown {
        let world = generated(seed);
        let wc = WorldComponents::assemble().unwrap();
        let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();
        // Reuse the exact pack params (BETA/FLOOR) genesis and
        // `demography_report` share — no bespoke beta/floor here.
        let report = demography_report(&world, &wc).expect("demography report");

        let terrain = terrain_of(&world).unwrap();
        let geo = terrain.geosphere();

        // Per-cell dominant species id: greatest density wins; deterministic
        // tie-break to the lowest species id.
        let mut dominant_counts: std::collections::BTreeMap<u32, usize> =
            std::collections::BTreeMap::new();
        for cell in geo.cells() {
            let mut best: Option<(u32, f64)> = None;
            for (id, density) in &report.stack.density {
                let d = *density.get(cell);
                if d <= 0.0 {
                    continue;
                }
                best = match best {
                    None => Some((*id, d)),
                    Some((_, bd)) if d.total_cmp(&bd) == std::cmp::Ordering::Greater => {
                        Some((*id, d))
                    }
                    other => other,
                };
            }
            if let Some((id, _)) = best {
                *dominant_counts.entry(id).or_insert(0) += 1;
            }
        }

        let name_of = |id: u32| names[id as usize];
        let mut breakdown: Vec<(&str, usize)> = dominant_counts
            .iter()
            .map(|(id, count)| (name_of(*id), *count))
            .collect();
        breakdown.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));

        (names, dominant_counts, breakdown)
    }

    /// Pre-repoint baseline distinct full-roster dominant count (task 6,
    /// 2026-07-16, ledger #48): the flat shared-`base_carrying` scalar gave
    /// exactly 2 (`twig-blight`, `rust-monster`). Frozen before T1/T2's
    /// per-axis repoint landed; see the module doc above this test group.
    const PRE_DEMESNE_DISTINCT_DOMINANTS_42: usize = 2;

    /// The Stage-1 ACHIEVEMENT (BIO-35 the-demesne, measured post-repoint,
    /// seed 42): the per-axis vector supply raises distinct full-roster
    /// dominants from 2 to exactly 4 (`xorn`, `rust-monster`, `twig-blight`,
    /// `goblin`). Pinned as a `>=` regression floor — a future change that
    /// drops below this reddens. NOT the preregistered target: see
    /// [`PREREGISTERED_DISTINCT_DOMINANTS_TARGET`].
    const STAGE1_DISTINCT_DOMINANTS_42: usize = 4;

    /// The PREREGISTERED distinct-dominant target (task 6, frozen before
    /// BIO-35): the campaign's stated ambition is `>= 6` distinct dominants
    /// incl. treant/xorn/dragon strongholds. Stage 1 (abiotic) reaches 4 —
    /// the gap to 6 is the treant mass-calibration (below, `#[ignore]`d) + the
    /// dragon/`ANIMAL_PREY` Stage-2 field (below, `#[ignore]`d) + the still-OPEN
    /// peoples-diversity problem (the goblinoid niches don't span the new
    /// axes). The `>= 6` bar is DELIBERATELY NOT weakened — it stays pinned as
    /// an ignored target-to-reach, not silently rebased to the observed value.
    const PREREGISTERED_DISTINCT_DOMINANTS_TARGET: usize = 6;

    /// STAGE-1-REACHABLE (un-ignored, BIO-35 the-demesne T3): the per-axis
    /// vector supply diversifies full-roster (fauna-included) per-cell
    /// dominance to the measured Stage-1 achievement
    /// ([`STAGE1_DISTINCT_DOMINANTS_42`] = 4, up from the baseline 2), and
    /// `xorn` — the pure-`MINERAL` specialist the mineral supply field was
    /// built to unlock — holds a real stronghold. This pins the ACHIEVEMENT,
    /// not the preregistered `>= 6` target (which stays `#[ignore]`d below,
    /// honestly unmet — see the module doc for the treant/dragon/peoples gap).
    #[test]
    fn menagerie_distinct_dominants_diversify_and_xorn_holds_a_stronghold() {
        let (names, dominant_counts, breakdown) = menagerie_full_roster_dominant_breakdown(42);
        let distinct_dominants = dominant_counts.len();
        assert!(
            distinct_dominants >= STAGE1_DISTINCT_DOMINANTS_42,
            "expected the per-axis vector supply to hold the Stage-1 achievement of \
             {STAGE1_DISTINCT_DOMINANTS_42} distinct full-roster dominants (up from the \
             pre-repoint baseline {PRE_DEMESNE_DISTINCT_DOMINANTS_42}), got \
             {distinct_dominants}: {breakdown:?}"
        );

        let dominates_a_cell = |name: &str| {
            let id = names
                .iter()
                .position(|n| *n == name)
                .expect("species in registry") as u32;
            dominant_counts.get(&id).copied().unwrap_or(0) > 0
        };
        assert!(
            dominates_a_cell("xorn"),
            "the mineral specialist (xorn) should hold a stronghold: {breakdown:?}"
        );
    }

    /// STILL `#[ignore]`d — the PREREGISTERED `>= 6` target, honestly unmet
    /// (BIO-35 the-demesne T3, measured not faked): Stage 1 reaches
    /// [`STAGE1_DISTINCT_DOMINANTS_42`] = 4. Reaching
    /// [`PREREGISTERED_DISTINCT_DOMINANTS_TARGET`] = 6 needs the treant
    /// mass-calibration + the dragon/`ANIMAL_PREY` Stage-2 field + the open
    /// peoples-diversity problem. Kept (not deleted, not weakened) so the
    /// original ambition stands ready to re-run once those land.
    #[test]
    #[ignore = "PREREGISTERED >= 6 distinct-dominant target: Stage 1 (abiotic) reaches 4; \
                the gap to 6 is treant mass-calibration + dragon/ANIMAL_PREY (Stage 2) + \
                the open peoples-diversity problem. Not weakened — stands as the target."]
    fn menagerie_distinct_dominants_reach_the_preregistered_six() {
        let (_names, dominant_counts, breakdown) = menagerie_full_roster_dominant_breakdown(42);
        assert!(
            dominant_counts.len() >= PREREGISTERED_DISTINCT_DOMINANTS_TARGET,
            "the preregistered target is {PREREGISTERED_DISTINCT_DOMINANTS_TARGET} distinct \
             dominants; got {}: {breakdown:?}",
            dominant_counts.len()
        );
    }

    /// STILL `#[ignore]`d — mass/sovereignty calibration, NOT Stage 2
    /// (BIO-35 the-demesne T3, measured not faked): `treant` (the
    /// photosynthate specialist) holds ZERO strongholds at seed 42 even
    /// under the per-axis vector supply. Root cause (task 6, unchanged by
    /// the-demesne): Kleiber home-range scaling (treant 1800 kg vs
    /// twig-blight 5 kg, ~1568x home-range ratio) swamps treant's
    /// resource-share advantage once converted to a per-INDIVIDUAL
    /// density. This is a body-mass/sovereignty-devotion retune, not a
    /// resource-axis fix — out of the-demesne's scope.
    #[test]
    #[ignore = "mass/sovereignty calibration (task 6 CALIBRATION FINDING, confirmed unchanged \
                by the-demesne T1/T2/T3): treant holds zero full-roster strongholds at seed 42 \
                — Kleiber home-range scaling swamps its resource-share advantage in a \
                per-individual density metric, a body-mass/sovereignty-devotion retune the \
                per-axis vector supply does not touch. See \
                .superpowers/sdd/task-6-report.md and this test group's module doc."]
    fn menagerie_treant_stronghold_awaits_mass_calibration() {
        let (names, dominant_counts, breakdown) = menagerie_full_roster_dominant_breakdown(42);
        let dominates_a_cell = |name: &str| {
            let id = names
                .iter()
                .position(|n| *n == name)
                .expect("species in registry") as u32;
            dominant_counts.get(&id).copied().unwrap_or(0) > 0
        };
        assert!(
            dominates_a_cell("treant"),
            "the photosynthate specialist (treant) should hold a stronghold: {breakdown:?}"
        );
    }

    /// STILL `#[ignore]`d — Stage 2 (`ANIMAL_PREY` field), BIO-35 the-demesne
    /// T3: no chromatic dragon (a pure-`ANIMAL_PREY` niche) ever wins a
    /// full-roster per-cell dominance comparison, because Stage 1's
    /// `ANIMAL_PREY` supply is a placeholder zero — a species reading it
    /// can never out-compete one riding a real (nonzero) axis. Awaits the
    /// trophic/prey-field wiring a later stage adds.
    #[test]
    #[ignore = "Stage 2 (ANIMAL_PREY field): chromatic dragons' pure-ANIMAL_PREY niche reads \
                Stage 1's placeholder-zero ANIMAL_PREY supply, so no dragon can ever win a \
                per-cell density comparison against a species riding a real axis; awaiting the \
                trophic/prey-field wiring (BIO-35 Stage 2)."]
    fn menagerie_dragon_stronghold_awaits_animal_prey_stage_2() {
        let (names, dominant_counts, breakdown) = menagerie_full_roster_dominant_breakdown(42);
        let dominates_a_cell = |name: &str| {
            let id = names
                .iter()
                .position(|n| *n == name)
                .expect("species in registry") as u32;
            dominant_counts.get(&id).copied().unwrap_or(0) > 0
        };
        assert!(
            dominates_a_cell("white-dragon")
                || dominates_a_cell("red-dragon")
                || dominates_a_cell("black-dragon"),
            "at least one chromatic dragon (mighty apex) should hold a stronghold: {breakdown:?}"
        );
    }
}
