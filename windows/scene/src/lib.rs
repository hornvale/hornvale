//! The scene window: deterministic, semantic-only scene descriptions over
//! the query surface (rendering-strategy Ring 2; decision 0022). A scene
//! says *what an observer can see* — raw quantities, catalog names, point
//! features — never how to draw it. Schemas are save-format-class
//! contracts: additive changes stay in-version, changed meaning mints a
//! new version alongside. This crate holds the cartographic pole:
//! `scene/tiles/v1`, the equirectangular tile lattice; and the orrery pole:
//! `scene/system/v1`, the star system's orbital elements.

#![warn(missing_docs)]

use hornvale_astronomy::StdInstant;
use hornvale_climate::{Biome, GeneratedClimate};
use hornvale_kernel::{FacetId, NearestVertexIndex, Seed, VertexMap, World, WorldTime};
use hornvale_terrain::{
    EndpointSide, FacetAddress, FacetFieldSample, FeatureId, FeatureKind, GeneratedTerrain,
    RealizedCurve, TerminalKind,
};
use hornvale_worldgen::{
    SurfacePatch, SurfaceRealizationContext, SurfaceRevision, facet::stitch_transition,
};
use serde::Serialize;

mod astronomy_at;
pub use astronomy_at::*;

mod region;
pub use region::*;

mod surrounds;
pub use surrounds::*;

mod surrounds_ascii;
pub use surrounds_ascii::*;

/// The schema identifier this crate emits.
/// type-audit: bare-ok(identifier-text)
pub const TILES_SCHEMA: &str = "scene/tiles/v1";
/// Smallest legal lattice width.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const MIN_WIDTH: u32 = 16;
/// Largest legal lattice width.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const MAX_WIDTH: u32 = 1024;

/// Scene construction failed; the reason, loudly (the GenesisError manner).
/// type-audit: bare-ok(diagnostic-value: WidthOdd.0), bare-ok(diagnostic-value: WidthOutOfRange.0), bare-ok(prose: Build.0), bare-ok(prose: AstronomyQuery.0), bare-ok(prose: Surface.0), bare-ok(diagnostic-value: RegionFaceOutOfRange.0), bare-ok(diagnostic-value: RegionLevelOutOfRange.0), bare-ok(diagnostic-value: RegionTileOutOfRange.ix), bare-ok(diagnostic-value: RegionTileOutOfRange.iy), bare-ok(diagnostic-value: RegionTileOutOfRange.level), bare-ok(diagnostic-value: RegionSamplesOutOfRange.0), bare-ok(diagnostic-value: SurroundsRadiusOutOfRange.0), bare-ok(diagnostic-value: SurroundsUnaddressable.0), bare-ok(identifier-text: UnknownTileField.0), bare-ok(prose: MalformedTileFields.0), bare-ok(diagnostic-value: ObserverLatitudeOutOfRange.0), bare-ok(diagnostic-value: ObserverLongitudeNonFinite.0)
#[derive(Debug, Clone, PartialEq)]
pub enum SceneError {
    /// Width must be even (height is width / 2).
    WidthOdd(u32),
    /// Width must lie in `MIN_WIDTH..=MAX_WIDTH`.
    WidthOutOfRange(u32),
    /// The world could not be rebuilt from its ledger.
    Build(String),
    /// Evaluated astronomy is unavailable for the requested instant.
    AstronomyQuery(String),
    /// The coherent surface could not be queried or serialized.
    Surface(String),
    /// Regional query: `face` must be 0..=5.
    RegionFaceOutOfRange(u32),
    /// Regional query: `level` must be 0..=MAX_REGION_LEVEL.
    RegionLevelOutOfRange(u32),
    /// Regional query: `ix`/`iy` must be < 2^level.
    RegionTileOutOfRange {
        /// The offending column.
        ix: u32,
        /// The offending row.
        iy: u32,
        /// The level whose 2^level bound they violated.
        level: u32,
    },
    /// Regional query: `samples` must be 1..=MAX_REGION_SAMPLES.
    RegionSamplesOutOfRange(u32),
    /// Surrounds query: `radius` must be 0..=MAX_SURROUNDS_RADIUS.
    SurroundsRadiusOutOfRange(u32),
    /// Surrounds query: a neighbourhood cell's address could not be packed
    /// to a room id (see `Facet::pack`); the `FacetError` debug is
    /// carried. Mirrors `LocaleError::Unaddressable` — fail fast rather
    /// than mint a meaningless `room: 0`.
    SurroundsUnaddressable(String),
    /// Tile-field selection: a requested layer name is not one of
    /// [`TileFields::ALL_NAMES`]. Carries the offending name.
    UnknownTileField(String),
    /// Tile-field selection: the request was not a JSON array of strings.
    /// Carries the parser's message.
    MalformedTileFields(String),
    /// Eclipse observer latitude was non-finite or outside `[-90, 90]`.
    ObserverLatitudeOutOfRange(f64),
    /// Eclipse observer longitude was non-finite.
    ObserverLongitudeNonFinite(f64),
}

impl std::fmt::Display for SceneError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SceneError::WidthOdd(w) => write!(
                f,
                "--width {w} is odd; height is width/2, so width must be even"
            ),
            SceneError::WidthOutOfRange(w) => {
                write!(f, "--width {w} is outside {MIN_WIDTH}..={MAX_WIDTH}")
            }
            SceneError::AstronomyQuery(e) => write!(f, "astronomy query: {e}"),
            SceneError::Surface(e) => write!(f, "surface query: {e}"),
            SceneError::Build(e) => write!(f, "building the world: {e}"),
            SceneError::RegionFaceOutOfRange(f_) => {
                write!(f, "--face {f_} is outside 0..=5 (six cube faces)")
            }
            SceneError::RegionLevelOutOfRange(l) => {
                write!(f, "--level {l} is outside 0..={MAX_REGION_LEVEL}")
            }
            SceneError::RegionTileOutOfRange { ix, iy, level } => write!(
                f,
                "--ix {ix}/--iy {iy} out of range for level {level} (must be < {})",
                1u64 << level
            ),
            SceneError::RegionSamplesOutOfRange(s) => {
                write!(f, "--samples {s} is outside 1..={MAX_REGION_SAMPLES}")
            }
            SceneError::SurroundsRadiusOutOfRange(r) => {
                write!(f, "--radius {r} is outside 0..={MAX_SURROUNDS_RADIUS}")
            }
            SceneError::SurroundsUnaddressable(e) => {
                write!(f, "surrounds neighbourhood cell is unaddressable: {e}")
            }
            SceneError::UnknownTileField(name) => write!(
                f,
                "unknown tile field {name:?}; valid fields are {}",
                TileFields::ALL_NAMES.join(", ")
            ),
            SceneError::MalformedTileFields(e) => write!(
                f,
                "tile fields must be a JSON array of strings, e.g. [\"elevation_m\",\"ocean\"]: {e}"
            ),
            SceneError::ObserverLatitudeOutOfRange(latitude) => write!(
                f,
                "observer latitude {latitude} is outside the finite range -90..=90"
            ),
            SceneError::ObserverLongitudeNonFinite(longitude) => {
                write!(f, "observer longitude {longitude} is not finite")
            }
        }
    }
}

/// A named point on the lattice — settlements today, more kinds later.
/// type-audit: bare-ok(identifier-text: name), bare-ok(identifier-text: kind), pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Feature {
    /// The feature's canonical name.
    pub name: String,
    /// What kind of point this is: `"settlement"` or `"flagship"`.
    pub kind: String,
    /// Degrees north.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Degrees east.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// One `scene/tiles/v1` document (scene-protocol spec §2). Field order is
/// the JSON key order and is contract — never reorder. Layers are
/// row-major, top row first: latitude 90→−90 down, longitude −180→180
/// across, pixel centers.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(identifier-text: biome_legend), bare-ok(constructor-edge: seed), bare-ok(count: width), bare-ok(count: height), pending(wave-3: sea_level_m), waiver(elevation-convention: elevation_m), bare-ok(flag: ocean), bare-ok(index: biome), bare-ok(index: plate), bare-ok(ratio: unrest), bare-ok(diagnostic-value: t_mean_c), bare-ok(diagnostic-value: t_swing_c), bare-ok(diagnostic-value: t_diurnal_amp_c), bare-ok(diagnostic-value: current_east), bare-ok(diagnostic-value: current_north), bare-ok(diagnostic-value: season_period_days), bare-ok(count: circulation_bands), bare-ok(ratio: moisture), bare-ok(flag: locked), bare-ok(diagnostic-value: precip_mm_yr), bare-ok(diagnostic-value: snow_fraction), bare-ok(index: precip_regime), bare-ok(diagnostic-value: cloud_fraction), bare-ok(ratio: weather_propensity), bare-ok(index: cloud_type), bare-ok(index: water), bare-ok(identifier-text: water_legend), bare-ok(diagnostic-value: drainage)
#[derive(Debug, Serialize)]
pub struct TilesScene {
    /// Always `scene/tiles/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// Lattice width in tiles.
    pub width: u32,
    /// Lattice height in tiles (always `width / 2`).
    pub height: u32,
    /// Sea level in meters.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
    /// Elevation in meters per tile.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub elevation_m: Vec<f64>,
    /// Whether each tile is ocean (sea level baked in).
    pub ocean: Vec<bool>,
    /// Biome per tile, as an index into `biome_legend`.
    pub biome: Vec<u16>,
    /// The full biome catalog, in stable order.
    pub biome_legend: Vec<String>,
    /// Tectonic plate id per tile.
    pub plate: Vec<u32>,
    /// Tectonic unrest per tile.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub unrest: Vec<f64>,
    /// Named points: settlements, the flagship last.
    pub features: Vec<Feature>,
    /// Annual-mean temperature per tile, °C.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_mean_c: Vec<f64>,
    /// Hemisphere-signed seasonal half-swing per tile, °C (0 when locked/zero-obliquity).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_swing_c: Vec<f64>,
    /// Per-tile diurnal half-range amplitude, °C (always `>= 0`; the
    /// coefficient a client scales the diurnal waveform by — see
    /// `hornvale_climate::diurnal_waveform`).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub t_diurnal_amp_c: Vec<f64>,
    /// The ocean-current tangent vector's eastward component per tile
    /// (`dot(current, east)`), dimensionless (unit-sphere tangent-frame
    /// units, not m/s). Zero on land and on every tile when the world is
    /// tidally locked (The Gyre; see [`hornvale_climate::GeneratedClimate::current_at`]).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub current_east: Vec<f64>,
    /// The ocean-current tangent vector's northward component per tile
    /// (`dot(current, north)`), same units and zero-cases as
    /// [`Self::current_east`].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub current_north: Vec<f64>,
    /// The seasonal sinusoid's period, standard days (the world's year).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub season_period_days: f64,
    /// Circulation bands per hemisphere; omitted entirely on tidally locked worlds.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub circulation_bands: Option<u32>,
    /// Moisture index per tile, dimensionless [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub moisture: Vec<f64>,
    /// Whether this world is tidally locked — the client reads its seasonal
    /// temperature from the librating-substellar reconstruction rather than
    /// the hemisphere-signed sinusoid. Appended per the schema stability
    /// contract.
    pub locked: bool,
    /// Annual precipitation per tile, mm/yr (The Rains) — the moisture field
    /// mapped into an Earth-ranged total; see
    /// [`hornvale_climate::GeneratedClimate::precip_at`]. Appended per the
    /// schema stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub precip_mm_yr: Vec<f64>,
    /// The fraction of precipitation falling as snow per tile, `[0, 1]` (The
    /// Rains); see [`hornvale_climate::GeneratedClimate::snow_fraction_at`].
    /// Appended per the schema stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub snow_fraction: Vec<f64>,
    /// The seasonal precipitation regime per tile, as an index into
    /// `hornvale_climate::PrecipRegime`'s declaration order (The Rains); see
    /// [`hornvale_climate::GeneratedClimate::regime_at`]. Appended per the
    /// schema stability contract.
    pub precip_regime: Vec<u8>,
    /// Diagnostic cloud fraction per tile, `[0, 1]` (The Rains) — feeds
    /// nothing else in the sim, a readable field only; see
    /// [`hornvale_climate::GeneratedClimate::cloud_fraction_at`]. Appended
    /// per the schema stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub cloud_fraction: Vec<f64>,
    /// Per-tile climatological storm propensity `[0,1]` (The Firmament) — the
    /// slow prior the client animates typed clouds from. Appended after
    /// `cloud_fraction` per the append-at-end stability rule.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub weather_propensity: Vec<f64>,
    /// Per-tile cloud type at the scene's day (0 none, 1 cumulus, 2 stratus, 3
    /// nimbostratus, 4 cumulonimbus, 5 cirrus) — the weather state's face.
    pub cloud_type: Vec<u8>,
    /// The water classification per tile, as an index into `water_legend`
    /// (WaterKind: ocean / salt-basin / river / dry-land). Row-major, matching
    /// `elevation_m`. Appended per the schema stability contract.
    pub water: Vec<u8>,
    /// The water-kind catalog in stable index order — `water`'s values index
    /// into this. Appended per the schema stability contract.
    pub water_legend: Vec<String>,
    /// Flow-accumulation drainage per tile (0 on ocean/dry land); river
    /// magnitude. Appended per the schema stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub drainage: Vec<f64>,
    /// Waterfall sites — high-drainage watercourses crossing a scarp.
    /// Appended per the schema stability contract.
    pub waterfalls: Vec<WaterfallPoint>,
}

/// A waterfall site as a point on the lattice (spec §5) — where a
/// high-drainage watercourse crosses a scarp. lat/lon in degrees, quantized
/// at emit.
/// type-audit: pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, Serialize)]
pub struct WaterfallPoint {
    /// Latitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Longitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// The per-tile layers a caller wants in a `scene/tiles/v1` document.
///
/// The document's nineteen per-tile arrays are 99.8% of its bytes, and a
/// given client typically reads a subset — the Orrery reads eight. This is
/// `BuildDepth`'s "only as deep as the question asks" at the emit boundary:
/// the sim still computes every layer, the caller chooses what crosses the
/// wire (decision 0022).
///
/// Field names are the wire's own names. An unknown name is an error, never
/// a silently-dropped layer.
#[derive(Debug, Clone)]
pub struct TileFields {
    /// One flag per name in [`TileFields::ALL_NAMES`], same order.
    selected: Vec<bool>,
}

impl TileFields {
    /// The nineteen per-tile array names, in [`TilesScene`]'s declaration
    /// order. Document metadata (`schema`, the legends, `features`, …) is
    /// never selectable: it is always emitted.
    ///
    /// The tag below is currently unenforced: `tools/type-audit` walks
    /// module-level `Item::Const` only (`extract.rs:64`) and never descends
    /// into an `impl` block's associated consts, so the audit cannot see
    /// this surface. The tag is written anyway so the annotation is honest
    /// if the tool ever grows that case — a tool gap worth a followup, not
    /// a reason to leave a pub-boundary primitive unannotated.
    /// type-audit: bare-ok(identifier-text: ALL_NAMES)
    pub const ALL_NAMES: &'static [&'static str] = &[
        "elevation_m",
        "ocean",
        "biome",
        "plate",
        "unrest",
        "t_mean_c",
        "t_swing_c",
        "t_diurnal_amp_c",
        "current_east",
        "current_north",
        "moisture",
        "precip_mm_yr",
        "snow_fraction",
        "precip_regime",
        "cloud_fraction",
        "weather_propensity",
        "cloud_type",
        "water",
        "drainage",
    ];

    /// Every layer — the default document.
    pub fn all() -> TileFields {
        TileFields {
            selected: vec![true; Self::ALL_NAMES.len()],
        }
    }

    /// Exactly the named layers. An unknown name is an error naming the
    /// offender, never a silently-dropped layer.
    /// type-audit: bare-ok(identifier-text: names)
    pub fn only(names: &[&str]) -> Result<TileFields, SceneError> {
        let mut selected = vec![false; Self::ALL_NAMES.len()];
        for name in names {
            match Self::ALL_NAMES.iter().position(|n| n == name) {
                Some(i) => selected[i] = true,
                None => return Err(SceneError::UnknownTileField((*name).to_string())),
            }
        }
        Ok(TileFields { selected })
    }

    /// Whether `name` is a selected per-tile layer. A name that is not a
    /// per-tile layer at all is not selected (metadata is emitted
    /// unconditionally and never consults this).
    /// type-audit: bare-ok(identifier-text: name), bare-ok(flag: return)
    pub fn contains(&self, name: &str) -> bool {
        match Self::ALL_NAMES.iter().position(|n| *n == name) {
            Some(i) => self.selected[i],
            None => false,
        }
    }

    /// Parse a JSON array of layer names, e.g. `["elevation_m","ocean"]` —
    /// the wire form a client passes across the WASM boundary.
    /// type-audit: bare-ok(artifact: json)
    pub fn parse_json(json: &str) -> Result<TileFields, SceneError> {
        let names: Vec<String> = serde_json::from_str(json)
            .map_err(|e| SceneError::MalformedTileFields(e.to_string()))?;
        let refs: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        TileFields::only(&refs)
    }
}

/// Dot product a · b.
fn dot3(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// The unit northward tangent at a vertex, given its unit-sphere `position`
/// and eastward tangent: `normalize(cross(position, east))`, completing the
/// local (east, north) tangent frame the current components project onto.
/// Zero wherever `east` is zero (the poles, where east is undefined).
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

/// The reusable coarse-world build for scene documents. Constructed once and
/// reused across every scene call, so a document stays a cheap derived view.
///
/// Every terrain-facing entry point used to rebuild all of this per call —
/// 638 ms of terrain and climate derivation, 91.6% of a region patch (The
/// Sextant). The `x_scene` / `x_scene_in` pair here is the same one
/// [`surrounds_scene_in`] already uses with a `LocaleContext`.
pub struct SceneContext {
    /// The world this context was built from; guards against reuse across worlds.
    seed: Seed,
    /// The sculpted terrain, derived once.
    terrain: GeneratedTerrain,
    /// The derived climate, derived once.
    climate: GeneratedClimate,
    /// Nearest-vertex index over the terrain geosphere. Two indices, not one:
    /// terrain and climate each carry their own geosphere, and today both
    /// happen to share the same vertex level, so one index could in principle
    /// serve both. Keeping them separate is deliberate defensiveness against
    /// that ever diverging — behavior is identical while the two geospheres
    /// agree.
    terrain_index: NearestVertexIndex,
    /// Nearest-vertex index over the climate geosphere (see `terrain_index` for
    /// why the two are kept separate).
    climate_index: NearestVertexIndex,
    /// The per-vertex biome map (`biome_map()` returns by value, so it is built once).
    biomes: VertexMap<Biome>,
    /// The source-owned coherent surface realization, retained for patch queries.
    surface: SurfaceRealizationContext,
}

impl SceneContext {
    /// Derive terrain, climate, both nearest-vertex indices and the biome map once.
    // Named construction site (decision 0092): scene's entry wrapper —
    // sculpts/fits once, shared by every reader built from this context.
    #[allow(clippy::disallowed_methods)]
    pub fn build(world: &World) -> Result<SceneContext, SceneError> {
        Self::build_internal(world, None)
    }

    /// Build a scene context while preserving a source boundary's revision
    /// identity. Terrain and climate are handed to the surface context after
    /// this one derivation, rather than being reconstructed there.
    /// type-audit: bare-ok(identifier-text: source_revision)
    #[allow(clippy::disallowed_methods)]
    pub fn build_with_source_revision(
        world: &World,
        source_revision: &str,
    ) -> Result<SceneContext, SceneError> {
        Self::build_internal(world, Some(source_revision))
    }

    #[allow(clippy::disallowed_methods)]
    fn build_internal(
        world: &World,
        source_revision: Option<&str>,
    ) -> Result<SceneContext, SceneError> {
        let terrain =
            hornvale_worldgen::terrain_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
        let climate = hornvale_worldgen::climate_from(world, &terrain)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let terrain_index = NearestVertexIndex::new(terrain.geosphere());
        let climate_index = NearestVertexIndex::new(climate.geosphere());
        let biomes = climate.biome_map();
        let surface = match source_revision {
            Some(source_revision) => SurfaceRealizationContext::from_parts_with_source_revision(
                world,
                terrain.clone(),
                climate.clone(),
                source_revision,
            ),
            None => SurfaceRealizationContext::from_parts(world, terrain.clone(), climate.clone()),
        }
        .map_err(|e| SceneError::Surface(e.to_string()))?;
        Ok(SceneContext {
            seed: world.seed,
            terrain,
            climate,
            terrain_index,
            climate_index,
            biomes,
            surface,
        })
    }

    /// The seed this context was built from.
    pub fn seed(&self) -> Seed {
        self.seed
    }

    /// The revision of the retained source-owned coherent surface.
    pub fn surface_revision(&self) -> &SurfaceRevision {
        &self.surface.revision
    }
}

/// A request for one derived, addressed coherent ground patch.
pub struct SurfacePatchQuery {
    /// The Level-6 macro face and refinement path to realize.
    pub address: FacetAddress,
    /// The revision the caller has observed and is prepared to consume.
    pub expected_revision: SurfaceRevision,
}

/// Decode the compact source-wire address into a scene query.
/// type-audit: bare-ok(index: macro_face), bare-ok(index: child_path)
pub fn surface_patch_query_from_packed(
    macro_face: u32,
    child_path: Vec<u8>,
    expected_revision: SurfaceRevision,
) -> Result<SurfacePatchQuery, SceneError> {
    let macro_face = FacetId(u64::from(macro_face))
        .unpack()
        .map_err(|e| SceneError::Surface(format!("invalid macro face: {e:?}")))?;
    let address = FacetAddress::new(macro_face, child_path)
        .map_err(|e| SceneError::Surface(format!("invalid surface address: {e:?}")))?;
    Ok(SurfacePatchQuery {
        address,
        expected_revision,
    })
}

/// Query one coherent surface patch from a previously built scene context.
pub fn surface_patch_scene(
    context: &SceneContext,
    query: &SurfacePatchQuery,
) -> Result<SurfacePatch, SceneError> {
    surface_patch_scene_with_transition(context, query, None)
}

/// Query a patch and, when requested, attach source-computed replacement
/// triangles for its coarse edge beside an immediate finer neighbor.
pub fn surface_patch_scene_with_transition(
    context: &SceneContext,
    query: &SurfacePatchQuery,
    transition_address: Option<&FacetAddress>,
) -> Result<SurfacePatch, SceneError> {
    if query.expected_revision != context.surface.revision {
        return Err(SceneError::Surface(
            "expected surface revision does not match the scene context".into(),
        ));
    }
    let mut patch = context
        .surface
        .realize(&query.address)
        .map_err(|error| SceneError::Surface(error.to_string()))?;
    if let Some(transition_address) = transition_address {
        let fine = context
            .surface
            .realize(transition_address)
            .map_err(|error| SceneError::Surface(error.to_string()))?;
        patch.transition_triangles = stitch_transition(&patch, &fine)
            .map_err(|error| SceneError::Surface(error.to_string()))?;
    }
    Ok(patch)
}

#[derive(Serialize)]
struct SurfacePatchDocument {
    schema: &'static str,
    revision: SurfaceRevisionDocument,
    address: SurfaceAddressDocument,
    samples: Vec<SurfaceSampleDocument>,
    curves: Vec<SurfaceCurveDocument>,
    triangles: Vec<[u32; 3]>,
    #[serde(skip_serializing_if = "Vec::is_empty")]
    transition_triangles: Vec<[u32; 3]>,
}

#[derive(Serialize)]
struct SurfaceRevisionDocument {
    source_revision: String,
    algorithm_version: &'static str,
    configuration_hash_hex: String,
}

#[derive(Serialize)]
struct SurfaceAddressDocument {
    macro_face: u32,
    child_path: Vec<u8>,
}

#[derive(Serialize)]
struct SurfaceFeatureDocument {
    kind: &'static str,
    macro_anchor: u64,
    ordinal: u32,
}

#[derive(Serialize)]
struct SurfaceBoundaryDocument {
    address: SurfaceAddressDocument,
    edge: u8,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    t: f64,
}

#[derive(Serialize)]
struct SurfaceEndpointDocument {
    feature: SurfaceFeatureDocument,
    side: &'static str,
    boundary: Option<SurfaceBoundaryDocument>,
    terminal: &'static str,
}

#[derive(Serialize)]
struct SurfaceCurveDocument {
    feature: SurfaceFeatureDocument,
    points: Vec<Vec<f64>>,
    width_rad: Vec<f64>,
    endpoints: [SurfaceEndpointDocument; 2],
}

#[derive(Serialize)]
struct SurfaceSampleDocument {
    position: Vec<f64>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    height_m: f64,
    normal: Vec<f64>,
    material_weights: Vec<f64>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    shoreline_distance_m: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    water_depth_m: f64,
    flow_direction: Vec<f64>,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    flow_strength: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    channel_distance_m: f64,
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    channel_width_m: f64,
    floodplain_weight: f64,
    bank_weight: f64,
    terrace_weight: f64,
    delta_weight: f64,
    ridge_direction: Vec<f64>,
    ridge_strength: f64,
}

fn quantized_vector(values: impl IntoIterator<Item = f64>) -> Vec<f64> {
    values.into_iter().map(hornvale_kernel::quantize).collect()
}

fn surface_address_document(address: &FacetAddress) -> SurfaceAddressDocument {
    SurfaceAddressDocument {
        macro_face: address
            .macro_face
            .pack()
            .expect("surface patches carry valid macro faces")
            .0
            .try_into()
            .expect("Level-6 facet IDs fit in the surface wire integer"),
        child_path: address.child_path.clone(),
    }
}

fn surface_feature_document(feature: FeatureId) -> SurfaceFeatureDocument {
    SurfaceFeatureDocument {
        kind: match feature.kind {
            FeatureKind::ChannelReach => "channel_reach",
            FeatureKind::Confluence => "confluence",
            FeatureKind::Shoreline => "shoreline",
            FeatureKind::Ridge => "ridge",
            FeatureKind::MaterialTransition => "material_transition",
        },
        macro_anchor: u64::from(feature.macro_anchor.0),
        ordinal: feature.ordinal,
    }
}

fn surface_terminal_document(terminal: TerminalKind) -> &'static str {
    match terminal {
        TerminalKind::Headwater => "headwater",
        TerminalKind::Confluence => "confluence",
        TerminalKind::Lake => "lake",
        TerminalKind::Ocean => "ocean",
        TerminalKind::Continuation => "continuation",
    }
}

fn surface_endpoint_document(
    endpoint: &hornvale_terrain::FeatureEndpoint,
) -> SurfaceEndpointDocument {
    SurfaceEndpointDocument {
        feature: surface_feature_document(endpoint.feature),
        side: match endpoint.side {
            EndpointSide::Upstream => "upstream",
            EndpointSide::Downstream => "downstream",
        },
        boundary: endpoint
            .boundary
            .as_ref()
            .map(|boundary| SurfaceBoundaryDocument {
                address: surface_address_document(&boundary.address),
                edge: boundary.edge,
                t: boundary.t,
            }),
        terminal: surface_terminal_document(endpoint.terminal),
    }
}

fn surface_curve_document(curve: &RealizedCurve) -> SurfaceCurveDocument {
    SurfaceCurveDocument {
        feature: surface_feature_document(curve.feature),
        points: curve
            .points
            .iter()
            .map(|point| quantized_vector(*point))
            .collect(),
        width_rad: quantized_vector(curve.width.iter().copied()),
        endpoints: [
            surface_endpoint_document(&curve.endpoints[0]),
            surface_endpoint_document(&curve.endpoints[1]),
        ],
    }
}

fn surface_sample_document(sample: FacetFieldSample) -> SurfaceSampleDocument {
    SurfaceSampleDocument {
        position: quantized_vector(sample.position),
        height_m: sample.height_m,
        normal: quantized_vector(sample.normal),
        material_weights: quantized_vector(sample.material_weights.map(f64::from)),
        shoreline_distance_m: sample.shoreline_distance_m,
        water_depth_m: sample.water_depth_m,
        flow_direction: quantized_vector(sample.flow_direction),
        flow_strength: sample.flow_strength,
        channel_distance_m: sample.channel_distance_m,
        channel_width_m: sample.channel_width_m,
        floodplain_weight: hornvale_kernel::quantize(f64::from(sample.floodplain_weight)),
        bank_weight: hornvale_kernel::quantize(f64::from(sample.bank_weight)),
        terrace_weight: hornvale_kernel::quantize(f64::from(sample.terrace_weight)),
        delta_weight: hornvale_kernel::quantize(f64::from(sample.delta_weight)),
        ridge_direction: quantized_vector(sample.ridge_direction),
        ridge_strength: hornvale_kernel::quantize(f64::from(sample.ridge_strength)),
    }
}

/// Serialize a coherent surface patch as its canonical derived scene document.
/// The field order and quantized values are stable; this document is not saved
/// in the world ledger and carries no living-weather fields.
/// type-audit: bare-ok(artifact: return)
pub fn surface_patch_json(patch: &SurfacePatch) -> String {
    let revision = &patch.revision;
    let document = SurfacePatchDocument {
        schema: "scene/surface/v1",
        revision: SurfaceRevisionDocument {
            source_revision: revision.source_revision.clone(),
            algorithm_version: revision.algorithm_version,
            configuration_hash_hex: revision
                .configuration_hash
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect(),
        },
        address: surface_address_document(&patch.address),
        samples: patch
            .samples
            .iter()
            .copied()
            .map(surface_sample_document)
            .collect(),
        curves: patch.curves.iter().map(surface_curve_document).collect(),
        triangles: patch.triangles.clone(),
        transition_triangles: patch.transition_triangles.clone(),
    };
    serde_json::to_string(&document).expect("surface patch document always serializes")
}

/// The `width` contract shared by [`tiles_scene`] and [`temperature_grid`]:
/// even, and inside `MIN_WIDTH..=MAX_WIDTH`. Factored out so the `&World`
/// wrappers can reject a bad width *before* paying for
/// [`SceneContext::build`] while the `_in` forms still validate for every
/// caller. Duplicating the check on the wrapper path is the right trade
/// against making a bad-input call pay ~638 ms of derivation first.
fn validate_width(width: u32) -> Result<(), SceneError> {
    if !(MIN_WIDTH..=MAX_WIDTH).contains(&width) {
        return Err(SceneError::WidthOutOfRange(width));
    }
    if !width.is_multiple_of(2) {
        return Err(SceneError::WidthOdd(width));
    }
    Ok(())
}

/// Build the `scene/tiles/v1` scene for `world` at `width` tiles across
/// (height is `width / 2`), deriving a fresh [`SceneContext`]. Deterministic:
/// same world + same width → the same scene, byte-for-byte once serialized.
///
/// Prefer [`tiles_scene_in`] whenever a context is already in hand: the
/// derivation this performs costs ~638 ms against far less per-call work.
/// type-audit: bare-ok(count: width)
pub fn tiles_scene(world: &World, width: u32) -> Result<TilesScene, SceneError> {
    validate_width(width)?;
    tiles_scene_in(world, &SceneContext::build(world)?, width)
}

/// Build the `scene/tiles/v1` scene for `world` at `width` tiles across,
/// reusing a [`SceneContext`] the caller already built — the same
/// `x_scene` / `x_scene_in` pairing [`surrounds_scene_in`] uses.
/// type-audit: bare-ok(count: width)
pub fn tiles_scene_in(
    world: &World,
    ctx: &SceneContext,
    width: u32,
) -> Result<TilesScene, SceneError> {
    debug_assert_eq!(
        ctx.seed(),
        world.seed,
        "SceneContext was built for a different world than this call's"
    );
    validate_width(width)?;
    let height = width / 2;
    let terrain = &ctx.terrain;
    let climate = &ctx.climate;
    let terrain_index = &ctx.terrain_index;
    let climate_index = &ctx.climate_index;
    let biomes = &ctx.biomes;
    let catalog = hornvale_climate::Biome::catalog();
    let tiles = (width * height) as usize;
    let mut elevation_m = Vec::with_capacity(tiles);
    let mut ocean = Vec::with_capacity(tiles);
    let mut biome = Vec::with_capacity(tiles);
    let mut plate = Vec::with_capacity(tiles);
    let mut unrest = Vec::with_capacity(tiles);
    let mut t_mean_c = Vec::with_capacity(tiles);
    let mut t_swing_c = Vec::with_capacity(tiles);
    let mut t_diurnal_amp_c = Vec::with_capacity(tiles);
    let mut current_east = Vec::with_capacity(tiles);
    let mut current_north = Vec::with_capacity(tiles);
    let mut moisture = Vec::with_capacity(tiles);
    let mut precip_mm_yr = Vec::with_capacity(tiles);
    let mut snow_fraction = Vec::with_capacity(tiles);
    let mut precip_regime = Vec::with_capacity(tiles);
    let mut cloud_fraction = Vec::with_capacity(tiles);
    let mut weather_propensity = Vec::with_capacity(tiles);
    let mut cloud_type = Vec::with_capacity(tiles);
    let mut water = Vec::with_capacity(tiles);
    let mut drainage = Vec::with_capacity(tiles);
    // `tiles_scene` is a day-independent static document (unlike
    // `temperature_grid`, which takes a day): the cloud-type face is sampled
    // at day 0.0, the same snapshot convention the worldgen almanac's
    // weather line uses (`weather_line_for`, `windows/worldgen/src/lib.rs`).
    let scene_day = 0.0;
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let t_vertex = terrain_index.nearest(terrain.geosphere(), latitude, longitude);
            let c_vertex = climate_index.nearest(climate.geosphere(), latitude, longitude);
            elevation_m.push(terrain.elevation_at(t_vertex).get());
            ocean.push(terrain.is_ocean(t_vertex));
            water.push(terrain.water_kind_at(t_vertex).index());
            drainage.push(terrain.drainage_at(t_vertex));
            let b = *biomes.get(c_vertex);
            let index = catalog
                .iter()
                .position(|entry| *entry == b)
                .expect("every biome is in the catalog") as u16;
            biome.push(index);
            plate.push(terrain.plate_of(t_vertex));
            unrest.push(terrain.unrest_at(t_vertex));
            t_mean_c.push(climate.mean_temperature_at(c_vertex).get());
            t_swing_c.push(climate.seasonal_swing_at(c_vertex));
            t_diurnal_amp_c.push(climate.diurnal_amp_at(c_vertex));
            let current = climate.current_at(c_vertex);
            let east =
                hornvale_climate::circulation::wind_east_tangent(climate.geosphere(), c_vertex);
            let north = tangent_north(climate.geosphere().position(c_vertex), east);
            current_east.push(dot3(current, east));
            current_north.push(dot3(current, north));
            moisture.push(climate.moisture_at(c_vertex));
            precip_mm_yr.push(climate.precip_at(c_vertex).get());
            snow_fraction.push(climate.snow_fraction_at(c_vertex));
            precip_regime.push(climate.regime_at(c_vertex) as u8);
            cloud_fraction.push(climate.cloud_fraction_at(c_vertex));
            weather_propensity.push(climate.storm_propensity_at(c_vertex));
            cloud_type.push(climate.cloud_type_at(
                c_vertex,
                WorldTime::from_std_days(scene_day).expect("finite"),
            ) as u8);
        }
    }
    debug_assert!(
        elevation_m
            .iter()
            .chain(unrest.iter())
            .chain(t_mean_c.iter())
            .chain(t_swing_c.iter())
            .chain(t_diurnal_amp_c.iter())
            .chain(current_east.iter())
            .chain(current_north.iter())
            .chain(moisture.iter())
            .chain(precip_mm_yr.iter())
            .chain(snow_fraction.iter())
            .chain(cloud_fraction.iter())
            .chain(weather_propensity.iter())
            .chain(drainage.iter())
            .all(|v| v.is_finite()),
        "scene layers must be finite; serde_json would emit null"
    );
    let waterfalls = terrain
        .waterfalls()
        .iter()
        .map(|&vertex| {
            let c = terrain.geosphere().coord(vertex);
            WaterfallPoint {
                latitude: c.latitude,
                longitude: c.longitude,
            }
        })
        .collect();
    Ok(TilesScene {
        schema: TILES_SCHEMA.to_string(),
        seed: world.seed.0,
        width,
        height,
        sea_level_m: terrain.sea_level().get(),
        elevation_m,
        ocean,
        biome,
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        plate,
        unrest,
        features: features_of(world),
        t_mean_c,
        t_swing_c,
        t_diurnal_amp_c,
        current_east,
        current_north,
        season_period_days: climate.year_length_std(),
        circulation_bands: climate.band_count(),
        moisture,
        locked: climate.is_locked(),
        precip_mm_yr,
        snow_fraction,
        precip_regime,
        cloud_fraction,
        weather_propensity,
        cloud_type,
        water,
        water_legend: hornvale_terrain::WaterKind::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
        drainage,
        waterfalls,
    })
}

/// Per-tile actual temperature at `day`, °C, on the same lattice as
/// [`tiles_scene`] — `temperature_at` sampled at each tile's climate vertex.
/// This is the sim's ground truth that a client reconstructs from the
/// `t_mean_c`/`t_swing_c` layers; the cross-repo contract test compares the
/// client's reconstruction against these values. Full precision (not
/// quantized) — callers that need portable bytes quantize at their own
/// boundary.
/// type-audit: bare-ok(count: width), bare-ok(diagnostic-value: return)
pub fn temperature_grid(world: &World, width: u32, day: WorldTime) -> Result<Vec<f64>, SceneError> {
    validate_width(width)?;
    temperature_grid_in(world, &SceneContext::build(world)?, width, day)
}

/// Per-tile actual temperature at `day`, °C, reusing a [`SceneContext`] the
/// caller already built — the `_in` half of [`temperature_grid`]. The context
/// derives terrain then climate, which is exactly what `climate_of` does
/// (`windows/worldgen/src/lib.rs`), so only `ctx.climate` and
/// `ctx.climate_index` are read here — `world` is otherwise read only by the
/// context/world match assertion below, which is the same shape every `_in`
/// entry point takes.
/// type-audit: bare-ok(count: width), bare-ok(diagnostic-value: return)
pub fn temperature_grid_in(
    world: &World,
    ctx: &SceneContext,
    width: u32,
    day: WorldTime,
) -> Result<Vec<f64>, SceneError> {
    debug_assert_eq!(
        ctx.seed(),
        world.seed,
        "SceneContext was built for a different world than this call's"
    );
    validate_width(width)?;
    let height = width / 2;
    let climate = &ctx.climate;
    let climate_index = &ctx.climate_index;
    let tiles = (width * height) as usize;
    let mut temperature = Vec::with_capacity(tiles);
    for py in 0..height {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
        for px in 0..width {
            let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
            let c_vertex = climate_index.nearest(climate.geosphere(), latitude, longitude);
            temperature.push(climate.temperature_at(c_vertex, day).get());
        }
    }
    Ok(temperature)
}

/// Settlement point features: every place holding both coordinate facts,
/// in `places` order, with the flagship excluded there and appended last
/// under its own kind (spec §2: the flagship appears exactly once).
pub(crate) fn features_of(world: &World) -> Vec<Feature> {
    let flagship = hornvale_settlement::village_info(world);
    let flagship_id = flagship.as_ref().map(|v| v.id);
    let mut features = Vec::new();
    for place in hornvale_terrain::places(world) {
        if Some(place.id) == flagship_id {
            continue;
        }
        if let Some((latitude, longitude)) = place_latlon(world, place.id) {
            features.push(Feature {
                name: place.name,
                kind: "settlement".to_string(),
                latitude,
                longitude,
            });
        }
    }
    if let Some(village) = flagship
        && let Some((latitude, longitude)) = place_latlon(world, village.id)
    {
        features.push(Feature {
            name: village.name,
            kind: "flagship".to_string(),
            latitude,
            longitude,
        });
    }
    features
}

/// Latitude/longitude of a place from settlement's coordinate facts;
/// `None` if either is missing (such a place is skipped, the
/// settlement-map precedent).
pub(crate) fn place_latlon(world: &World, id: hornvale_kernel::EntityId) -> Option<(f64, f64)> {
    let lat = match world.ledger.value_of(id, hornvale_settlement::LATITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    let lon = match world.ledger.value_of(id, hornvale_settlement::LONGITUDE) {
        Some(hornvale_kernel::Value::Number(n)) => *n,
        _ => return None,
    };
    Some((lat, lon))
}

/// Serialize a scene as compact JSON — the wire and artifact form.
/// Deterministic: struct field order fixes key order; serde_json's float
/// text is shortest-round-trip.
/// type-audit: bare-ok(artifact: return)
pub fn scene_json(scene: &TilesScene) -> String {
    serde_json::to_string(scene).expect("a TilesScene always serializes")
}

/// Serializes a `Vec<f64>` through the kernel's quantizer, so a projected
/// document's floats are byte-identical to the derive's. Wrapping is what
/// lets a `serialize_with` function be reached from a manual `Serialize`
/// impl, where serde's field attribute is unavailable.
struct QVec<'a>(&'a [f64]);

impl serde::Serialize for QVec<'_> {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        hornvale_kernel::quantize::quantize_serde::vec_f64_field(self.0, s)
    }
}

/// The scalar counterpart of [`QVec`].
struct QF64(f64);

impl serde::Serialize for QF64 {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        hornvale_kernel::quantize::quantize_serde::f64_field(&self.0, s)
    }
}

/// A [`TilesScene`] viewed through a [`TileFields`] selection: the same
/// document with the unrequested per-tile arrays absent.
///
/// The `Serialize` impl is deliberately **manual** while [`scene_json`] keeps
/// the derive. Two independent paths is the point: `the_full_projection_
/// equals_the_derive` compares them, and a shared implementation would make
/// that test compare a thing to itself.
struct Projected<'a> {
    /// The document being projected.
    scene: &'a TilesScene,
    /// The per-tile layers to emit.
    fields: &'a TileFields,
}

impl serde::Serialize for Projected<'_> {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let scene = self.scene;
        let fields = self.fields;
        let mut m = s.serialize_map(None)?;
        // Keys are emitted in `TilesScene`'s declaration order — the JSON key
        // order is contract (spec §2). Metadata is unconditional; a per-tile
        // layer appears only when selected.
        macro_rules! layer {
            ($name:literal, $value:expr) => {
                if fields.contains($name) {
                    m.serialize_entry($name, &$value)?;
                }
            };
        }
        m.serialize_entry("schema", &scene.schema)?;
        m.serialize_entry("seed", &scene.seed)?;
        m.serialize_entry("width", &scene.width)?;
        m.serialize_entry("height", &scene.height)?;
        m.serialize_entry("sea_level_m", &QF64(scene.sea_level_m))?;
        layer!("elevation_m", QVec(&scene.elevation_m));
        layer!("ocean", scene.ocean);
        layer!("biome", scene.biome);
        m.serialize_entry("biome_legend", &scene.biome_legend)?;
        layer!("plate", scene.plate);
        layer!("unrest", QVec(&scene.unrest));
        m.serialize_entry("features", &scene.features)?;
        layer!("t_mean_c", QVec(&scene.t_mean_c));
        layer!("t_swing_c", QVec(&scene.t_swing_c));
        layer!("t_diurnal_amp_c", QVec(&scene.t_diurnal_amp_c));
        layer!("current_east", QVec(&scene.current_east));
        layer!("current_north", QVec(&scene.current_north));
        m.serialize_entry("season_period_days", &QF64(scene.season_period_days))?;
        // Matches the derive's `skip_serializing_if = "Option::is_none"`:
        // omitted entirely on tidally locked worlds, never emitted as null.
        if let Some(bands) = scene.circulation_bands {
            m.serialize_entry("circulation_bands", &bands)?;
        }
        layer!("moisture", QVec(&scene.moisture));
        m.serialize_entry("locked", &scene.locked)?;
        layer!("precip_mm_yr", QVec(&scene.precip_mm_yr));
        layer!("snow_fraction", QVec(&scene.snow_fraction));
        layer!("precip_regime", scene.precip_regime);
        layer!("cloud_fraction", QVec(&scene.cloud_fraction));
        layer!("weather_propensity", QVec(&scene.weather_propensity));
        layer!("cloud_type", scene.cloud_type);
        layer!("water", scene.water);
        m.serialize_entry("water_legend", &scene.water_legend)?;
        layer!("drainage", QVec(&scene.drainage));
        m.serialize_entry("waterfalls", &scene.waterfalls)?;
        m.end()
    }
}

/// Serialize a tiles document carrying only `fields`' per-tile layers.
/// Document metadata is always present. A field that IS emitted is
/// byte-identical to [`scene_json`]'s output for it.
/// type-audit: bare-ok(artifact: return)
pub fn scene_json_selected(scene: &TilesScene, fields: &TileFields) -> String {
    serde_json::to_string(&Projected { scene, fields }).expect("a TilesScene always serializes")
}

/// The schema identifier for the system (orrery) scene kind.
/// type-audit: bare-ok(identifier-text)
pub const SYSTEM_SCHEMA: &str = "scene/system/v1";

/// The central star's semantic elements.
/// type-audit: bare-ok(identifier-text: class_name), pending(wave-3: luminosity_rel), pending(wave-3: hz_inner_au), pending(wave-3: hz_outer_au)
#[derive(Debug, Serialize)]
pub struct StarElem {
    /// Descriptive spectral class name (e.g. `"yellow dwarf (G)"`).
    pub class_name: String,
    /// Luminosity in solar luminosities.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub luminosity_rel: f64,
    /// Habitable-zone inner edge, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub hz_inner_au: f64,
    /// Habitable-zone outer edge, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub hz_outer_au: f64,
}

/// A star's topology-aware physical elements.
/// type-audit: bare-ok(identifier-text: class_name), pending(wave-3: mass_rel), pending(wave-3: luminosity_rel)
#[derive(Debug, Serialize)]
pub struct StellarBodyElem {
    /// Descriptive spectral class name.
    /// type-audit: bare-ok(identifier-text: class_name)
    pub class_name: String,
    /// Stellar mass in solar masses.
    /// type-audit: pending(wave-3: mass_rel)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub mass_rel: f64,
    /// Luminosity in solar luminosities.
    /// type-audit: pending(wave-3: luminosity_rel)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub luminosity_rel: f64,
}

/// The emitted binary orbit elements.
/// type-audit: pending(wave-3: semi_major_axis_au), pending(wave-3: period_days), bare-ok(ratio: phase_offset)
#[derive(Debug, Serialize)]
pub struct BinaryOrbitElem {
    /// Separation semi-major axis, AU.
    /// type-audit: pending(wave-3: semi_major_axis_au)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub semi_major_axis_au: f64,
    /// Two-body period, standard days.
    /// type-audit: pending(wave-3: period_days)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub period_days: f64,
    /// Binary phase at day zero, turns in [0, 1).
    /// type-audit: bare-ok(ratio: phase_offset)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub phase_offset: f64,
}

/// The companion star and its binary orbit, absent for a single root.
#[derive(Debug, Serialize)]
pub struct CompanionElem {
    /// The secondary star.
    pub star: StellarBodyElem,
    /// The secondary's orbit relative to the primary.
    pub orbit: BinaryOrbitElem,
}

/// Topology-aware stellar elements appended to `scene/system/v1`.
/// type-audit: bare-ok(identifier-text: topology), pending(wave-3: combined_luminosity_rel), pending(wave-3: anchor_hz_inner_au), pending(wave-3: anchor_hz_outer_au), pending(wave-3: circumprimary_outer_limit_au), pending(wave-3: circumbinary_inner_limit_au)
#[derive(Debug, Serialize)]
pub struct StellarElem {
    /// `single`, `wide-binary`, or `close-binary`.
    /// type-audit: bare-ok(identifier-text: topology)
    pub topology: String,
    /// The primary star, retained separately from the compatibility `star`.
    pub primary: StellarBodyElem,
    /// The secondary and orbit for a binary topology.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub companion: Option<CompanionElem>,
    /// Sum of all root-star luminosities.
    /// type-audit: pending(wave-3: combined_luminosity_rel)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub combined_luminosity_rel: f64,
    /// Inner edge of the topology-aware anchor habitable zone, AU.
    /// type-audit: pending(wave-3: anchor_hz_inner_au)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub anchor_hz_inner_au: f64,
    /// Outer edge of the topology-aware anchor habitable zone, AU.
    /// type-audit: pending(wave-3: anchor_hz_outer_au)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub anchor_hz_outer_au: f64,
    /// Outer circumprimary stability limit, AU, for wide binaries.
    /// type-audit: pending(wave-3: circumprimary_outer_limit_au)
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field"
    )]
    pub circumprimary_outer_limit_au: Option<f64>,
    /// Inner circumbinary stability limit, AU, for close binaries.
    /// type-audit: pending(wave-3: circumbinary_inner_limit_au)
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field"
    )]
    pub circumbinary_inner_limit_au: Option<f64>,
}

/// One wandering sibling's circular orbital elements.
/// type-audit: pending(wave-3: orbit_au), pending(wave-3: period_days), bare-ok(ratio: phase_offset), bare-ok(identifier-text: class), bare-ok(ratio: albedo), pending(wave-3: synodic_period_days), pending(wave-3: max_elongation_deg)
#[derive(Debug, Serialize)]
pub struct WandererElem {
    /// Orbital radius, AU.
    /// type-audit: pending(wave-3: orbit_au)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub orbit_au: f64,
    /// Orbital period, standard days.
    /// type-audit: pending(wave-3: period_days)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub period_days: f64,
    /// Circular phase at day zero, turns in [0, 1).
    /// type-audit: bare-ok(ratio: phase_offset)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub phase_offset: f64,
    /// `rock` or `giant`.
    /// type-audit: bare-ok(identifier-text: class)
    pub class: String,
    /// Bond albedo.
    /// type-audit: bare-ok(ratio: albedo)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub albedo: f64,
    /// Synodic period against the anchor, standard days; may be infinite.
    /// type-audit: pending(wave-3: synodic_period_days)
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub synodic_period_days: f64,
    /// Maximum inner-body elongation, absent for outer bodies.
    /// type-audit: pending(wave-3: max_elongation_deg)
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field"
    )]
    pub max_elongation_deg: Option<f64>,
}

/// The anchor world's orbital and rotational elements.
/// type-audit: pending(wave-3: orbit_au), pending(wave-3: year_days), pending(wave-3: day_length_days), pending(wave-3: obliquity_deg), bare-ok(ratio: year_phase_offset)
#[derive(Debug, Serialize)]
pub struct WorldElem {
    /// Orbital radius, AU.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub orbit_au: f64,
    /// Year length, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub year_days: f64,
    /// Solar-day length, standard days; `None` when tidally locked (no spin).
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field"
    )]
    pub day_length_days: Option<f64>,
    /// Mean axial obliquity, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub obliquity_deg: f64,
    /// Genesis orbital phase offset (turns) so day 0 is an ordinary day.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub year_phase_offset: f64,
}

/// One moon's orbital elements.
/// type-audit: pending(wave-3: sidereal_days), pending(wave-3: distance_mm), bare-ok(ratio: phase_offset), bare-ok(ratio: size_rel), pending(wave-1: inclination_deg), pending(wave-1: node_longitude_deg)
#[derive(Debug, Serialize)]
pub struct MoonElem {
    /// Sidereal orbital period, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sidereal_days: f64,
    /// Genesis synodic-phase offset (turns).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub phase_offset: f64,
    /// Orbital distance from the world, megameters.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_mm: f64,
    /// Angular-diameter ratio (the size-word input).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub size_rel: f64,
    /// Orbital inclination to the anchor's orbital plane, degrees; > 90 is
    /// retrograde (The Reckoning). Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub inclination_deg: f64,
    /// Ecliptic longitude of the ascending node at genesis, degrees.
    /// Appended per the stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub node_longitude_deg: f64,
}

/// One `scene/system/v1` document: the system's orbital geometry as elements.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct SystemScene {
    /// Always `scene/system/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The central star.
    pub star: StarElem,
    /// The anchor world.
    pub world: WorldElem,
    /// The moons, generation order.
    pub moons: Vec<MoonElem>,
    /// Topology-aware stellar root, appended for v1 compatibility.
    pub stellar: StellarElem,
    /// Wandering siblings, orbital order.
    pub wanderers: Vec<WandererElem>,
}

/// Build the `scene/system/v1` scene for `world`.
pub fn system_scene(world: &World) -> Result<SystemScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky.system();
    let anchor = &system.anchor;
    let day_length_days = match &anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => Some(day.as_std_days()),
        hornvale_astronomy::Rotation::Locked => None,
    };
    let moons = system
        .moons
        .iter()
        .enumerate()
        .map(|(i, m)| MoonElem {
            sidereal_days: m.period.get(),
            phase_offset: system
                .forcing
                .moon_phase_offsets
                .get(i)
                .copied()
                .unwrap_or(0.0),
            distance_mm: m.distance.get(),
            size_rel: m.angular_diameter_rel,
            inclination_deg: m.inclination_deg,
            node_longitude_deg: m.node_longitude_deg,
        })
        .collect();
    let body = |star: &hornvale_astronomy::Star| StellarBodyElem {
        class_name: star.class_name.clone(),
        mass_rel: star.mass.get(),
        luminosity_rel: star.luminosity.get(),
    };
    let stellar = StellarElem {
        topology: match system.stellar.topology {
            hornvale_astronomy::StellarTopology::Single => "single",
            hornvale_astronomy::StellarTopology::WideBinary => "wide-binary",
            hornvale_astronomy::StellarTopology::CloseBinary => "close-binary",
        }
        .to_string(),
        primary: body(&system.star),
        companion: system
            .stellar
            .companion
            .as_ref()
            .map(|companion| CompanionElem {
                star: body(&companion.star),
                orbit: BinaryOrbitElem {
                    semi_major_axis_au: companion.orbit.semi_major_axis.get(),
                    period_days: companion.orbit.period.get(),
                    phase_offset: companion.orbit.phase,
                },
            }),
        combined_luminosity_rel: system.stellar.combined_luminosity.get(),
        anchor_hz_inner_au: system.stellar.anchor_habitable_zone.inner().get(),
        anchor_hz_outer_au: system.stellar.anchor_habitable_zone.outer().get(),
        circumprimary_outer_limit_au: system.stellar.circumprimary_outer_limit.map(|a| a.get()),
        circumbinary_inner_limit_au: system.stellar.circumbinary_inner_limit.map(|a| a.get()),
    };
    let wanderers = system
        .wanderers
        .iter()
        .map(|wanderer| WandererElem {
            orbit_au: wanderer.orbit.get(),
            period_days: wanderer.period.get(),
            phase_offset: wanderer.phase_offset,
            class: match wanderer.class {
                hornvale_astronomy::WandererClass::Rock => "rock",
                hornvale_astronomy::WandererClass::Giant => "giant",
            }
            .to_string(),
            albedo: wanderer.albedo,
            synodic_period_days: wanderer.synodic_period.get(),
            max_elongation_deg: wanderer.max_elongation_deg,
        })
        .collect();
    Ok(SystemScene {
        schema: SYSTEM_SCHEMA.to_string(),
        seed: world.seed.0,
        star: StarElem {
            class_name: system.star.class_name.clone(),
            luminosity_rel: system.star.luminosity.get(),
            hz_inner_au: system.star.habitable_zone.inner().get(),
            hz_outer_au: system.star.habitable_zone.outer().get(),
        },
        world: WorldElem {
            orbit_au: anchor.orbit.get(),
            year_days: anchor.year.get(),
            day_length_days,
            obliquity_deg: system.forcing.obliquity_mean,
            year_phase_offset: system.forcing.year_phase_offset,
        },
        moons,
        stellar,
        wanderers,
    })
}

/// Serialize a `SystemScene` to compact JSON (mirrors [`scene_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn system_json(scene: &SystemScene) -> String {
    serde_json::to_string(scene).expect("a SystemScene always serializes")
}

/// The `scene/moons/v1` schema tag.
/// type-audit: bare-ok(identifier-text)
pub const MOONS_SCHEMA: &str = "scene/moons/v1";

/// Newtonian gravitational constant, N·m²/kg² (CODATA recommended value).
/// plumb: pending(wave-1)
const G_SI: f64 = 6.6743e-11;

/// Surface gravity (m/s²) of a uniform sphere from its radius (km) and bulk
/// density (g/cm³). `g = GM/r²` and, for a uniform sphere, `M = (4/3)πρr³`,
/// so substituting gives `g = (4/3)πGρr` — no mass-in-kg term needed, which
/// lets this compose directly with [`hornvale_astronomy::radius_km`] (The
/// Reckoning's real-density radius) without re-deriving mass or duplicating
/// that crate's radius formula. Anchors to the same figure the old
/// constant-density formula encoded: a 1.0-lunar-mass `GiantImpact` moon
/// (density 3.34 g/cm³, radius ≈1737.77 km) comes out to ≈1.6226 m/s²,
/// matching Luna's real 1.62.
fn surface_gravity_ms2(radius_km: f64, density_g_cm3: f64) -> f64 {
    let r_m = radius_km * 1000.0;
    let rho_kg_per_m3 = density_g_cm3 * 1000.0;
    (4.0 / 3.0) * std::f64::consts::PI * G_SI * rho_kg_per_m3 * r_m
}

/// The baseline albedo band, before maria darkening, for a moon's
/// composition: icy reads bright (0.5–0.7, real ice), rocky/impact reads
/// dark (0.1–0.2, real regolith) — supplying the visual referent
/// `bright-icy` lacked before density was real (The Reckoning). Takes the
/// already-classified `is_icy` flag rather than a raw density: this crate
/// holds no density threshold of its own (review follow-up to Task 5b) —
/// `hornvale_astronomy::is_icy` is the single place that decides, so the
/// only boundary-precision test for "how close can a density be to icy"
/// lives in that domain crate, not duplicated here.
fn albedo_band(is_icy: bool) -> (f64, f64) {
    if is_icy { (0.5, 0.7) } else { (0.1, 0.2) }
}

/// The multiplier applied to `maria_fraction` for an icy composition
/// (review follow-up to Task 5b): maria are flood basalts, which an ice
/// body does not have, so a composition-blind `maria_fraction` could put
/// basaltic plains on an ice ball. Damped, not zeroed — real icy bodies
/// still carry resurfaced terrain (Europa's chaos terrain, Enceladus's
/// tiger stripes), just not basaltic maria, so a reduced residual stays
/// physically plausible. 0.3 was chosen judgmentally, not measured: it
/// caps an icy moon's `maria_fraction` at roughly the bottom third of the
/// hash-driven range rather than forcing it to read as pristine (a hard
/// zero), while still making an icy moon visibly less maria-marked than a
/// rocky one of the same mass in the vast majority of draws.
/// plumb: pending(wave-1)
const ICY_MARIA_DAMPING: f64 = 0.3;

/// A subtle near-gray tint, and the four seeded surface descriptors, as a
/// pure hash of the world seed and the moon index — no `Stream` draw. Mass
/// biases cratering/maria so a face reads plausibly: small moons cratered
/// highlands, large moons resurfaced maria plains ("models author, dice
/// roll", 0009). `is_icy` (from `hornvale_astronomy::is_icy`, real bulk
/// density) biases albedo's baseline band and damps `maria_fraction` (The
/// Reckoning; damping is a review follow-up) — an icy moon reads brighter
/// and less maria-marked than a rocky one of the same mass, while the hash
/// channel still perturbs within each band so two moons of a world differ.
fn seeded_descriptors(
    seed: hornvale_kernel::Seed,
    index: usize,
    mass_rel: f64,
    is_icy: bool,
) -> Descriptors {
    // value_noise_2d returns [0,1); integer coords sample the raw lattice
    // hash, one distinct channel per (index, channel) pair.
    let h = |channel: u32| hornvale_kernel::value_noise_2d(seed, index as f64, f64::from(channel));
    // "Largeness" in [0,1] over the drawn mass range [0.05, 2.5].
    let large = ((mass_rel - 0.05) / 2.45).clamp(0.0, 1.0);
    let small = 1.0 - large;

    // Cratering: hash pulled toward 1 as mass falls.
    let cratering = (0.35 * h(0) + 0.65 * small).clamp(0.0, 1.0);
    // Maria: hash pulled up with mass, then damped by cratering so a face is
    // not simultaneously all-craters and all-maria, then damped again for
    // an icy composition (basaltic maria are a rocky-body feature).
    let maria_fraction = ((0.35 * h(1) + 0.65 * large) * (1.0 - 0.5 * cratering)).clamp(0.0, 1.0);
    let maria_fraction = if is_icy {
        maria_fraction * ICY_MARIA_DAMPING
    } else {
        maria_fraction
    };
    // Albedo: a composition-baseline band (bright ice, dark rock), a hash
    // channel perturbing within it, then darkened where maria (dark plains)
    // is high.
    let (albedo_lo, albedo_hi) = albedo_band(is_icy);
    let albedo = (albedo_lo + (albedo_hi - albedo_lo) * h(2)) * (1.0 - 0.6 * maria_fraction);
    let albedo = albedo.clamp(0.04, 0.7);
    // Tint: three near-gray channels, deliberately subtle (moons are gray),
    // enough to tell two moons of a world apart.
    let tint = [
        (0.70 + 0.15 * (h(3) - 0.5)).clamp(0.0, 1.0),
        (0.70 + 0.15 * (h(4) - 0.5)).clamp(0.0, 1.0),
        (0.70 + 0.15 * (h(5) - 0.5)).clamp(0.0, 1.0),
    ];
    Descriptors {
        albedo,
        cratering,
        maria_fraction,
        tint,
    }
}

/// Stable lowercase word for a moon's formation mechanism (The Reckoning) —
/// serialized as text rather than the astronomy domain's enum discriminant,
/// the same convention `surface_class` already follows.
fn formation_word(formation: hornvale_astronomy::Formation) -> &'static str {
    match formation {
        hornvale_astronomy::Formation::GiantImpact => "giant-impact",
        hornvale_astronomy::Formation::Capture => "capture",
    }
}

struct Descriptors {
    albedo: f64,
    cratering: f64,
    maria_fraction: f64,
    tint: [f64; 3],
}

/// The normative `surface_class` classifier: a stable, append-only word for
/// clients that want a name, not a texture. Precedence (most specific
/// first): a bright icy face; else maria-rich; else heavily cratered; else
/// a cratered highland. Thresholds are the reference page's normative
/// table. **Since The Reckoning, `bright-icy` keys off the moon's real bulk
/// density, not the hash-noise `albedo`** — Nathan-ratified: The Faces
/// shipped the word for an icy moon before the model had a concept of ice;
/// this is the referent. Takes the already-classified `is_icy` flag
/// (`hornvale_astronomy::is_icy`) rather than a raw density (review
/// follow-up to Task 5b) — this crate holds no density threshold of its
/// own; the domain decides composition, this classifier only presents it.
/// type-audit: bare-ok(flag: is_icy), bare-ok(ratio: cratering), bare-ok(ratio: maria_fraction), bare-ok(identifier-text: return)
pub fn moon_surface_class(is_icy: bool, cratering: f64, maria_fraction: f64) -> &'static str {
    if is_icy {
        "bright-icy"
    } else if maria_fraction > 0.4 {
        "maria-rich"
    } else if cratering > 0.6 {
        "heavily-cratered"
    } else {
        "cratered-highland"
    }
}

/// One moon's surface document entry. Field order is fixed; every f64
/// quantizes at emit (decision 0033).
/// type-audit: bare-ok(count: index), bare-ok(ratio: mass_rel), bare-ok(ratio: albedo), bare-ok(ratio: cratering), bare-ok(ratio: maria_fraction), bare-ok(identifier-text: surface_class), pending(wave-3: radius_km), pending(wave-3: surface_gravity_ms2), bare-ok(ratio: tint), pending(wave-3: density_g_cm3), bare-ok(identifier-text: formation)
#[derive(Debug, Serialize)]
pub struct MoonSurface {
    /// The moon's generation index (matches `scene/system/v1`).
    pub index: usize,
    /// Mass in lunar masses (the generator's drawn value, surfaced).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub mass_rel: f64,
    /// Physical radius, km — derived from mass and the moon's real bulk
    /// density (`density_g_cm3`); see `hornvale_astronomy::radius_km`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub radius_km: f64,
    /// Surface gravity, m/s² — derived from `radius_km` and that same real
    /// density: `g = (4/3)πGρr` (see `surface_gravity_ms2` in this crate).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub surface_gravity_ms2: f64,
    /// Seeded reflectance in [0.04, 0.7]; composition-biased (bright for
    /// icy, dark for rocky/impact) and darkened where maria is high.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub albedo: f64,
    /// Seeded cratering intensity in [0, 1]; biased high for small moons.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub cratering: f64,
    /// Seeded smooth-maria fraction in [0, 1]; biased high for large moons,
    /// damped for an icy composition (maria are basaltic plains, which an
    /// ice body does not have).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub maria_fraction: f64,
    /// Seeded near-gray linear-RGB tint, each channel in [0, 1].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::vec_f64_field")]
    pub tint: [f64; 3],
    /// Derived descriptive class name (see `moon_surface_class`).
    pub surface_class: String,
    /// Bulk density, g/cm³ (The Reckoning) — the moon's real drawn/derived
    /// density (`hornvale_astronomy::Moon::density`), the physical basis
    /// `radius_km`, `surface_gravity_ms2`, and `surface_class`'s `bright-icy`
    /// branch all derive from. Additive field; appended after every
    /// existing field per the schema's stability contract.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub density_g_cm3: f64,
    /// How this moon formed (The Reckoning): `"giant-impact"` or
    /// `"capture"` — see `hornvale_astronomy::Formation`. Additive field;
    /// appended after every existing field per the schema's stability
    /// contract.
    pub formation: String,
}

/// One `scene/moons/v1` document: each moon's surface as derived physics
/// plus seeded procedural descriptors.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct MoonsScene {
    /// Always `scene/moons/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The moons, in generation order (matches `scene/system/v1`).
    pub moons: Vec<MoonSurface>,
}

/// Build the `scene/moons/v1` scene for `world`. Mirrors [`system_scene`].
/// A pure read plus hash: consumes no `Stream` draws.
pub fn moons_scene(world: &World) -> Result<MoonsScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky.system();
    let moons = system
        .moons
        .iter()
        .enumerate()
        .map(|(index, m)| {
            let mass_rel = m.mass.get();
            let density_g_cm3 = m.density.get();
            // Composition (review follow-up to Task 5b): the domain's own
            // predicate, not a local copy of a density threshold.
            let is_icy = hornvale_astronomy::is_icy(m);
            // Real-density physics (The Reckoning): call the domain's own
            // radius formula rather than re-deriving it, then compose
            // gravity from that radius and the same real density.
            let radius_km = hornvale_astronomy::radius_km(m);
            let gravity_ms2 = surface_gravity_ms2(radius_km, density_g_cm3);
            let d = seeded_descriptors(world.seed, index, mass_rel, is_icy);
            MoonSurface {
                index,
                mass_rel,
                radius_km,
                surface_gravity_ms2: gravity_ms2,
                albedo: d.albedo,
                cratering: d.cratering,
                maria_fraction: d.maria_fraction,
                tint: d.tint,
                surface_class: moon_surface_class(is_icy, d.cratering, d.maria_fraction)
                    .to_string(),
                density_g_cm3,
                formation: formation_word(m.formation).to_string(),
            }
        })
        .collect();
    Ok(MoonsScene {
        schema: MOONS_SCHEMA.to_string(),
        seed: world.seed.0,
        moons,
    })
}

/// Serialize a `MoonsScene` to compact JSON (mirrors [`system_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn moons_json(scene: &MoonsScene) -> String {
    serde_json::to_string(scene).expect("a MoonsScene always serializes")
}

/// The `scene/neighbors/v1` schema tag.
/// type-audit: bare-ok(identifier-text)
pub const NEIGHBORS_SCHEMA: &str = "scene/neighbors/v1";

/// One notable neighbor star as drawn by the generator, brightest first.
/// type-audit: bare-ok(count: index), bare-ok(identifier-text: class_name), bare-ok(identifier-text: color), pending(wave-1: distance_ly), bare-ok(ratio: brightness_rel), pending(wave-1: ra_deg), pending(wave-1: dec_deg)
#[derive(Debug, Serialize)]
pub struct NeighborElem {
    /// Generation index (stable identity; matches the ledger entity order).
    pub index: usize,
    /// Prose spectral class ("red giant"; `color` is the producer's color
    /// word, not this).
    pub class_name: String,
    /// The producer's color word (e.g. "smoldering red").
    pub color: String,
    /// Distance in light-years (drawn, 4-80).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_ly: f64,
    /// Apparent brightness, relative units (derived L/d²).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub brightness_rel: f64,
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
}

/// One anonymous background field star (texture, not a ledger entity).
/// type-audit: pending(wave-1: ra_deg), pending(wave-1: dec_deg), bare-ok(count: magnitude_class)
#[derive(Debug, Serialize)]
pub struct FieldStarElem {
    /// Right ascension, degrees [0, 360) — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub ra_deg: f64,
    /// Declination, degrees [-90, 90] — genesis-epoch equatorial.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub dec_deg: f64,
    /// Brightness class, 1 (brightest) ..= 5 (faintest).
    pub magnitude_class: u8,
}

/// One `scene/neighbors/v1` document: the night sky's two populations.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed)
#[derive(Debug, Serialize)]
pub struct NeighborsScene {
    /// Always `scene/neighbors/v1`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The notable neighbors, generation order (brightest first).
    pub neighbors: Vec<NeighborElem>,
    /// The background starfield, derivation order.
    pub stars: Vec<FieldStarElem>,
}

/// Build the `scene/neighbors/v1` scene for `world`. Mirrors [`moons_scene`].
/// Pure reads: consumes no genesis draws (the starfield derives on demand
/// from the astronomy seed, exactly as the almanac's figures path does).
pub fn neighbors_scene(world: &World) -> Result<NeighborsScene, SceneError> {
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky.system();
    let neighbors = system
        .neighbors
        .iter()
        .enumerate()
        .map(|(index, n)| NeighborElem {
            index,
            class_name: hornvale_astronomy::class_name(n.class).to_string(),
            color: n.color.clone(),
            distance_ly: n.distance.get(),
            brightness_rel: n.apparent_brightness,
            ra_deg: n.right_ascension,
            dec_deg: n.declination,
        })
        .collect();
    let astronomy_seed = world.seed.derive(hornvale_astronomy::streams::ROOT);
    let stars = hornvale_astronomy::starfield(astronomy_seed)
        .into_iter()
        .map(|s| FieldStarElem {
            ra_deg: s.ra_deg,
            dec_deg: s.dec_deg,
            magnitude_class: s.magnitude_class,
        })
        .collect();
    Ok(NeighborsScene {
        schema: NEIGHBORS_SCHEMA.to_string(),
        seed: world.seed.0,
        neighbors,
        stars,
    })
}

/// Serialize a `NeighborsScene` to compact JSON (mirrors [`moons_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn neighbors_json(scene: &NeighborsScene) -> String {
    serde_json::to_string(scene).expect("a NeighborsScene always serializes")
}

/// The `scene/eclipses/v3` schema tag.
/// type-audit: bare-ok(identifier-text)
pub const ECLIPSES_SCHEMA: &str = "scene/eclipses/v3";

/// One solar eclipse's shadow band on the globe.
/// type-audit: pending(wave-1: center_lat_deg), pending(wave-1: half_width_deg), pending(wave-1: start_lon_deg), pending(wave-1: end_lon_deg), pending(wave-2: duration_days), pending(wave-1: sweep_deg), bare-ok(flag: global_coverage)
#[derive(Debug, Serialize)]
pub struct GroundTrackElem {
    /// Band-center latitude at mid-event, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub center_lat_deg: f64,
    /// Half-width of the full-omen band, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub half_width_deg: f64,
    /// Sub-solar longitude at crossing start, degrees [-180, 180).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub start_lon_deg: f64,
    /// Sub-solar longitude at crossing end, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub end_lon_deg: f64,
    /// Crossing duration, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub duration_days: f64,
    /// Signed, unwrapped longitude sweep, degrees. Positive is eastward and
    /// negative is westward; magnitudes above 360 preserve completed turns.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sweep_deg: f64,
    /// Whether every surface longitude lies inside the directed sweep.
    pub global_coverage: bool,
}

/// Optional geographic observer input for an eclipse query.
/// type-audit: pending(wave-1: latitude_deg), pending(wave-1: longitude_deg)
#[derive(Debug, Clone, Copy, Serialize)]
pub struct EclipseObserverQuery {
    /// Geographic latitude in degrees, inclusive `[-90, 90]`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude_deg: f64,
    /// Geographic longitude in degrees, normalized to `[-180, 180)`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude_deg: f64,
}

/// One observer's result for one eclipse event.
///
/// The event owns its physical `region` and `track`; this result says only
/// which hemisphere contains the observer and what that observer sees.
/// type-audit: bare-ok(identifier-text: side), bare-ok(identifier-text: visibility)
#[derive(Debug, Serialize)]
pub struct EclipseObserverElem {
    /// `"day"` or `"night"` at the event midpoint. Central solar
    /// visibility is event-wide and may occur away from that midpoint.
    pub side: String,
    /// Solar: `"whole-sun"`, `"burning-ring"`, `"bitten"`, or
    /// `"unseen"`; lunar: `"visible"` or `"unseen"`.
    pub visibility: String,
}

/// One bounded synodic/draconic return.
/// type-audit: bare-ok(index: synodic_count), bare-ok(index: draconic_count), pending(wave-1: period_days), pending(wave-1: node_slip_deg)
#[derive(Debug, Serialize)]
pub struct EclipseCycleElem {
    /// Synodic months per return.
    pub synodic_count: u32,
    /// Draconic months per return.
    pub draconic_count: u32,
    /// Return period in standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub period_days: f64,
    /// Draconic-phase miss per return, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub node_slip_deg: f64,
}

/// The recurrence ladder for one moon and eclipse family.
/// type-audit: bare-ok(index: moon_index), bare-ok(identifier-text: body), pending(wave-1: draconic_month_days), pending(wave-1: eclipse_year_days), bare-ok(count: series_returns), pending(wave-1: series_lifetime_days), pending(wave-1: exeligmos_period_days), pending(wave-1: exeligmos_node_slip_deg), pending(wave-1: exeligmos_surface_longitude_shift_deg), pending(wave-1: parade_days_per_year)
#[derive(Debug, Serialize)]
pub struct EclipseRecurrenceElem {
    /// Distance-sorted index into the system's moons.
    pub moon_index: usize,
    /// `"solar"` or `"lunar"`.
    pub body: String,
    /// The moon's draconic month, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub draconic_month_days: f64,
    /// The sun's return to the moon's node line, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub eclipse_year_days: f64,
    /// The selected bounded return.
    pub cycle: EclipseCycleElem,
    /// Estimated number of returns in the eclipse series.
    pub series_returns: u32,
    /// Estimated eclipse-series lifetime, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub series_lifetime_days: f64,
    /// Three selected-cycle periods, standard days.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub exeligmos_period_days: f64,
    /// Node-phase slip accumulated over the exeligmos, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub exeligmos_node_slip_deg: f64,
    /// Signed residual surface-longitude shift after the exeligmos, degrees.
    /// Zero is exact rotational closure; positive is eastward.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub exeligmos_surface_longitude_shift_deg: f64,
    /// Backward eclipse-season migration through one civil year, standard
    /// days per year.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub parade_days_per_year: f64,
}

/// One dated eclipse.
///
/// **The `day` field is deliberately a bare `i64`, not `WorldTime`** (The
/// Escapement, ruling 12 — the same reasoning governs [`EclipsesScene`]'s
/// `from`/`until`). `WorldTime` is
/// `#[serde(transparent)]`, so typing them as `WorldTime` would serialize
/// identically. The explicit conversion keeps a future representation change
/// as a compile-time decision at the wire boundary.
///
/// With a `serde(transparent)` `WorldTime`, that same internal change could
/// become a silent wire break. The manual conversion is the intended seam.
/// type-audit: bare-ok(count: day), bare-ok(count: moon_index), bare-ok(identifier-text: body), bare-ok(identifier-text: kind), bare-ok(identifier-text: region)
#[derive(Debug, Serialize)]
pub struct EclipseElem {
    /// The syzygy, as an exact tick count since genesis.
    ///
    /// **v2 dropped the quantized `f64` day that sat beside this** (The
    /// Foliot). It existed for one stated reason — "kept for the external
    /// Orrery's existing consumers" — and both external clients (the Orrery
    /// and goldengrove) are out of scope by Nathan's ruling, so it had no
    /// consumer left: nothing in `clients/` ever read it. An integer needs no
    /// quantization and does not decay with world age (decision 0188), which
    /// is why the tick field was added BESIDE it rather than instead of it at
    /// the time — the additive step this release completes.
    ///
    /// A bare `i64` on the wire rather than a `WorldTime`: this is a JSON
    /// contract, and a serialized `WorldTime` would carry its own field name
    /// into the document for no gain.
    pub day: i64,
    /// Distance-sorted index into the system's moons.
    pub moon_index: usize,
    /// "solar" or "lunar".
    pub body: String,
    /// "total" or "annular".
    pub kind: String,
    /// `"ground-track"` for solar events or `"night-hemisphere"` for
    /// lunar events. This is the event's physical region, independent of
    /// any observer query.
    pub region: String,
    /// The shadow band — `Some` for solar events, `None` (serialized as JSON
    /// `null`) for lunar (the anchor's shadow is the whole night side). NOT
    /// `skip_serializing_if` — the field is always present so the client sees
    /// an explicit `"track": null`, matching the spec's `track | null`.
    pub track: Option<GroundTrackElem>,
    /// One supplied observer's result. The key is omitted when no observer
    /// was requested; an unseen result remains present as `"unseen"`.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub observer: Option<EclipseObserverElem>,
}

/// One `scene/eclipses/v3` document: recurrence and dated eclipses in a
/// queried window.
///
/// `from`/`until` are bare `i64` rather than `WorldTime` for the reason set
/// out on [`EclipseElem`].
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(count: from), bare-ok(count: until), bare-ok(count: coincidence_days)
#[derive(Debug, Serialize)]
pub struct EclipsesScene {
    /// Always `scene/eclipses/v3`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The queried window start, echoed back as an exact tick count.
    pub from: i64,
    /// The queried window end, echoed back as an exact tick count.
    pub until: i64,
    /// The normalized observer query. Omitted when no observer was supplied.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub observer: Option<EclipseObserverQuery>,
    /// Integer days carrying admitted events from at least two moons.
    pub coincidence_days: u32,
    /// Recurrence records in moon order, solar then lunar per moon.
    pub recurrences: Vec<EclipseRecurrenceElem>,
    /// The dated eclipses, day-ascending.
    pub events: Vec<EclipseElem>,
}

fn normalized_eclipse_observer(
    observer: Option<EclipseObserverQuery>,
) -> Result<Option<EclipseObserverQuery>, SceneError> {
    observer
        .map(|mut observer| {
            if !observer.latitude_deg.is_finite()
                || !(-90.0..=90.0).contains(&observer.latitude_deg)
            {
                return Err(SceneError::ObserverLatitudeOutOfRange(
                    observer.latitude_deg,
                ));
            }
            if !observer.longitude_deg.is_finite() {
                return Err(SceneError::ObserverLongitudeNonFinite(
                    observer.longitude_deg,
                ));
            }
            observer.longitude_deg = (observer.longitude_deg + 180.0).rem_euclid(360.0) - 180.0;
            Ok(observer)
        })
        .transpose()
}

fn eclipse_body_name(body: hornvale_astronomy::EclipseBody) -> &'static str {
    match body {
        hornvale_astronomy::EclipseBody::Solar => "solar",
        hornvale_astronomy::EclipseBody::Lunar => "lunar",
    }
}

fn eclipse_observer_elem(result: hornvale_astronomy::EclipseObserverResult) -> EclipseObserverElem {
    let side = match result.side {
        hornvale_astronomy::EclipseSide::Day => "day",
        hornvale_astronomy::EclipseSide::Night => "night",
    };
    let visibility = match result.visibility {
        hornvale_astronomy::EclipseVisibility::Solar(sight) => match sight {
            hornvale_astronomy::EclipseSight::WholeSun => "whole-sun",
            hornvale_astronomy::EclipseSight::BurningRing => "burning-ring",
            hornvale_astronomy::EclipseSight::Bitten => "bitten",
            hornvale_astronomy::EclipseSight::Unseen => "unseen",
        },
        hornvale_astronomy::EclipseVisibility::Lunar { visible: true } => "visible",
        hornvale_astronomy::EclipseVisibility::Lunar { visible: false } => "unseen",
    };
    EclipseObserverElem {
        side: side.to_string(),
        visibility: visibility.to_string(),
    }
}

fn ground_track_elem(track: hornvale_astronomy::GroundTrack) -> GroundTrackElem {
    GroundTrackElem {
        center_lat_deg: track.center_lat_deg,
        half_width_deg: track.half_width_deg,
        start_lon_deg: track.start_lon_deg,
        end_lon_deg: track.end_lon_deg,
        duration_days: track.duration_days,
        sweep_deg: track.sweep_deg,
        global_coverage: track.global_coverage,
    }
}

/// Build the `scene/eclipses/v3` scene for `world` over the closed
/// `[from, until]` standard-day window and an optional geographic observer.
/// Observer latitude must be finite and in `[-90, 90]`; finite longitude is
/// normalized to `[-180, 180)`. Pure read: consumes no draws.
pub fn eclipses_scene(
    world: &World,
    from: StdInstant,
    until: StdInstant,
    observer: Option<EclipseObserverQuery>,
) -> Result<EclipsesScene, SceneError> {
    let observer = normalized_eclipse_observer(observer)?;
    let from_ticks = WorldTime::from_std_days(from.get())
        .map_err(|e| SceneError::Build(e.to_string()))?
        .ticks();
    let until_ticks = WorldTime::from_std_days(until.get())
        .map_err(|e| SceneError::Build(e.to_string()))?
        .ticks();
    let query_from = StdInstant::new(WorldTime::from_ticks(from_ticks).as_std_days())
        .expect("an i64 tick is always a finite standard-day instant");
    let query_until = StdInstant::new(WorldTime::from_ticks(until_ticks).as_std_days())
        .expect("an i64 tick is always a finite standard-day instant");
    let sky = hornvale_worldgen::sky_of(world).map_err(|e| SceneError::Build(e.to_string()))?;
    let system = sky.system();
    // StdInstant admits every finite point, while the wire contract is the
    // narrower i64 tick axis. Converting both bounds before enumeration makes
    // every event inside the requested window representable too.
    let calendar = hornvale_astronomy::calendar_of(system);
    let recurrences = hornvale_astronomy::eclipse_recurrences(system, &calendar)
        .into_iter()
        .map(|recurrence| EclipseRecurrenceElem {
            moon_index: recurrence.moon,
            body: eclipse_body_name(recurrence.body).to_string(),
            draconic_month_days: recurrence.draconic_month.get(),
            eclipse_year_days: recurrence.eclipse_year.get(),
            cycle: EclipseCycleElem {
                synodic_count: recurrence.cycle.synodic_count,
                draconic_count: recurrence.cycle.draconic_count,
                period_days: recurrence.cycle.period.get(),
                node_slip_deg: recurrence.cycle.node_slip_deg,
            },
            series_returns: recurrence.series_returns,
            series_lifetime_days: recurrence.series_lifetime.get(),
            exeligmos_period_days: recurrence.exeligmos_period.get(),
            exeligmos_node_slip_deg: recurrence.exeligmos_node_slip_deg,
            exeligmos_surface_longitude_shift_deg: recurrence.exeligmos_surface_longitude_shift_deg,
            parade_days_per_year: recurrence.parade_days_per_year,
        })
        .collect();
    let domain_events =
        hornvale_astronomy::eclipse_events(system, &calendar, query_from, query_until);
    let coincidence_days = hornvale_astronomy::coincidence_days(&domain_events);
    let mut events = domain_events
        .into_iter()
        .map(|event| {
            let track = hornvale_astronomy::ground_track(system, &calendar, &event);
            let region = match event.body {
                hornvale_astronomy::EclipseBody::Solar => "ground-track",
                hornvale_astronomy::EclipseBody::Lunar => "night-hemisphere",
            };
            let observer_result = observer
                .map(|observer| {
                    hornvale_astronomy::eclipse_observer_result(
                        system,
                        &calendar,
                        &event,
                        observer.latitude_deg,
                        observer.longitude_deg,
                    )
                    .map(eclipse_observer_elem)
                    .ok_or_else(|| {
                        SceneError::Build(
                            "astronomy rejected a validated eclipse observer or event".to_string(),
                        )
                    })
                })
                .transpose()?;
            Ok(EclipseElem {
                day: WorldTime::from_std_days(event.day.get())
                    .expect("an eclipse's own instant is finite and in range")
                    .ticks(),
                moon_index: event.moon,
                body: eclipse_body_name(event.body).to_string(),
                kind: match event.kind {
                    hornvale_astronomy::EclipseKind::Total => "total",
                    hornvale_astronomy::EclipseKind::Annular => "annular",
                }
                .to_string(),
                region: region.to_string(),
                track: track.map(ground_track_elem),
                observer: observer_result,
            })
        })
        .collect::<Result<Vec<_>, SceneError>>()?;
    events.sort_by_key(|event| (event.day, event.moon_index));
    Ok(EclipsesScene {
        schema: ECLIPSES_SCHEMA.to_string(),
        seed: world.seed.0,
        from: from_ticks,
        until: until_ticks,
        observer,
        coincidence_days,
        recurrences,
        events,
    })
}

/// Serialize an `EclipsesScene` to compact JSON (mirrors [`moons_json`]).
/// type-audit: bare-ok(artifact: return)
pub fn eclipses_json(scene: &EclipsesScene) -> String {
    serde_json::to_string(scene).expect("an EclipsesScene always serializes")
}

#[cfg(test)]
mod tests {
    use super::*;

    // Build a world exactly the way the CLI's tests do (see cli/src/repl.rs
    // tests for the canonical minimal build_world call). Each pin argument's
    // type is fixed by build_world's signature, so `&Default::default()`
    // infers; add a domain crate to [dev-dependencies] only if a pin type
    // turns out not to be re-exported by worldgen.
    fn world() -> World {
        hornvale_worldgen::build_world(
            hornvale_kernel::Seed(1),
            &Default::default(),
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 1 builds")
    }

    fn mooned_world() -> World {
        gen_world_for(42)
    }

    fn gen_world_for(seed: u64) -> World {
        hornvale_worldgen::build_world(
            hornvale_kernel::Seed(seed),
            &Default::default(),
            &Default::default(),
            &Default::default(),
        )
        .expect("seed builds a generated sky")
    }

    /// Significant digits in a JSON number token (sign, leading zeros, the
    /// decimal point, and trailing zeros stripped). A cheap proxy for
    /// "quantized": raw libm-derived floats carry 15–16.
    fn significant_digits(token: &str) -> usize {
        let t = token.trim_start_matches('-').replace('.', "");
        let t = t.trim_start_matches('0');
        let t = t.trim_end_matches('0');
        t.len()
    }

    #[test]
    fn serialized_tiles_carry_no_more_than_eight_significant_digits() {
        let json = scene_json(&tiles_scene(&world(), 32).unwrap());
        // Split into candidate number tokens and check each.
        for token in json.split(|c: char| !(c.is_ascii_digit() || c == '.' || c == '-')) {
            if token.is_empty() || !token.contains('.') {
                continue;
            }
            assert!(
                significant_digits(token) <= 8,
                "un-quantized float in scene JSON: {token}"
            );
        }
    }

    #[test]
    fn scene_is_byte_deterministic() {
        let w = world();
        let a = scene_json(&tiles_scene(&w, 32).unwrap());
        let b = scene_json(&tiles_scene(&w, 32).unwrap());
        assert_eq!(a, b);
        let rebuilt = world();
        assert_eq!(a, scene_json(&tiles_scene(&rebuilt, 32).unwrap()));
    }

    #[test]
    fn layers_are_sized_and_legend_is_the_catalog() {
        let scene = tiles_scene(&world(), 32).unwrap();
        assert_eq!(scene.height, 16);
        let tiles = (scene.width * scene.height) as usize;
        assert_eq!(scene.elevation_m.len(), tiles);
        assert_eq!(scene.ocean.len(), tiles);
        assert_eq!(scene.biome.len(), tiles);
        assert_eq!(scene.plate.len(), tiles);
        assert_eq!(scene.unrest.len(), tiles);
        assert_eq!(scene.biome_legend.len(), 22);
        assert!(scene.biome.iter().all(|&i| (i as usize) < 22));
    }

    /// claim: structural(seed: 1 — generated sky) — false-positive seed-loop
    /// flag; `s` binds a swing f64
    #[test]
    fn climate_layers_are_sized_and_present() {
        let scene = tiles_scene(&world(), 32).unwrap(); // seed-1 generated sky: spins
        let tiles = (scene.width * scene.height) as usize;
        assert_eq!(scene.t_mean_c.len(), tiles);
        assert_eq!(scene.t_swing_c.len(), tiles);
        assert_eq!(scene.t_diurnal_amp_c.len(), tiles);
        assert!(
            scene
                .t_diurnal_amp_c
                .iter()
                .all(|a| a.is_finite() && *a >= 0.0)
        );
        assert_eq!(scene.moisture.len(), tiles);
        assert!(scene.moisture.iter().all(|&m| (0.0..=1.0).contains(&m)));
        // Seed 1's generated-sky year, measured 2026-09-04 (The Zenith). This
        // read `365.25` with the comment "constant-sun default year" — the
        // tier's Earth-baseline stand-in, not a world's own orbit.
        assert_eq!(scene.season_period_days, 538.084165906676);
        // Seed 1's drawn spinning regime resolves to three circulation bands.
        assert_eq!(scene.circulation_bands, Some(3));
        // Its drawn nonzero obliquity produces a nonzero swing somewhere.
        assert!(scene.t_swing_c.iter().any(|&s| s != 0.0));
        // Signed: some tile north-positive, some south-negative.
        assert!(scene.t_swing_c.iter().any(|&s| s > 0.0));
        assert!(scene.t_swing_c.iter().any(|&s| s < 0.0));
        assert_eq!(scene.current_east.len(), tiles);
        assert_eq!(scene.current_north.len(), tiles);
        // finite everywhere; exactly zero on land tiles
        for i in 0..tiles {
            assert!(scene.current_east[i].is_finite() && scene.current_north[i].is_finite());
            if !scene.ocean[i] {
                assert_eq!(scene.current_east[i], 0.0);
                assert_eq!(scene.current_north[i], 0.0);
            }
        }
    }

    /// claim: reachability(seed: exists seed in 44..=60 with a waterfall site) —
    /// an existence probe over a bounded range, not a distribution: waterfall
    /// sites are sparse and the scan stops at the first hit.
    #[test]
    fn water_fields_are_sized_legend_matches_and_ocean_has_no_drainage() {
        // The seed is SEARCHED FOR, not pinned. Waterfall sites are sparse
        // (`waterfalls_exist_across_a_seed_sweep`, domains/terrain/src/carve.rs)
        // and the population moved under the terrain epoch of decision 0134:
        // seed 44 carried 4 sites and now carries 0, because a coastline at
        // the shelf break drains land over a shorter distance, shrinking
        // catchments and with them the drainage that clears
        // `WATERFALL_MIN_DRAINAGE`. Measured over seeds 40..=60 after the
        // epoch, only 45, 50, 52 and 56 carry a site at all, one each.
        //
        // Re-pinning on 45 would restore exactly the sample the epoch just
        // falsified, so the test scans instead and asserts it found one
        // inside the bound: a world that stops making waterfalls entirely is
        // then a finding this test reports, rather than a pin that quietly
        // needs moving every epoch. It costs one extra world build today
        // (44 has none, 45 does).
        let mut found = None;
        for seed in 44..=60u64 {
            let s = tiles_scene(&gen_world_for(seed), 32).unwrap();
            if !s.waterfalls.is_empty() {
                found = Some((seed, s));
                break;
            }
        }
        let (seed, scene) = found.expect(
            "no seed in 44..=60 carries a waterfall site — the world has stopped \
             producing them, which is a finding to report, not a bound to widen",
        );
        println!(
            "water fields measured on seed {seed}, {} waterfalls",
            scene.waterfalls.len()
        );
        let tiles = scene.width as usize * scene.height as usize;
        assert_eq!(scene.water.len(), tiles);
        assert_eq!(
            scene.water_legend,
            vec!["ocean", "salt-basin", "river", "dry-land"]
        );
        assert_eq!(scene.drainage.len(), scene.water.len());
        // rivers exist (~6.7% of land on a typical world)
        assert!(
            scene
                .water
                .iter()
                .any(|&w| w == hornvale_terrain::WaterKind::River.index())
        );
        // ocean tiles carry no drainage
        for i in 0..scene.water.len() {
            if scene.ocean[i] {
                assert_eq!(scene.drainage[i], 0.0);
            }
        }
        // waterfalls exist and serialize
        assert!(!scene.waterfalls.is_empty());
        let json = scene_json(&scene);
        assert!(json.contains("water_legend"));
        assert!(json.contains("drainage"));
        assert!(json.contains("waterfalls"));
    }

    #[test]
    fn rains_layers_are_sized_finite_and_in_range() {
        // The four fields Task 5 added: precip_mm_yr / snow_fraction /
        // precip_regime / cloud_fraction (The Rains).
        let scene = tiles_scene(&world(), 32).unwrap();
        let tiles = (scene.width * scene.height) as usize;
        assert_eq!(scene.precip_mm_yr.len(), tiles);
        assert_eq!(scene.snow_fraction.len(), tiles);
        assert_eq!(scene.precip_regime.len(), tiles);
        assert_eq!(scene.cloud_fraction.len(), tiles);
        assert!(
            scene
                .precip_mm_yr
                .iter()
                .all(|p| p.is_finite() && *p >= 0.0)
        );
        assert!(
            scene
                .snow_fraction
                .iter()
                .all(|f| f.is_finite() && (0.0..=1.0).contains(f))
        );
        assert!(
            scene
                .cloud_fraction
                .iter()
                .all(|f| f.is_finite() && (0.0..=1.0).contains(f))
        );
        // PrecipRegime has 4 declared variants (Uniform, SummerMax,
        // WinterMax, Monsoon) — every index must land in 0..4.
        assert!(scene.precip_regime.iter().all(|&r| r < 4));
    }

    #[test]
    fn scene_carries_weather_fields_additively() {
        let scene = tiles_scene(&world(), 16).unwrap();
        let n = scene.width as usize * scene.height as usize;
        assert_eq!(scene.weather_propensity.len(), n);
        assert_eq!(scene.cloud_type.len(), n);
        assert!(
            scene
                .weather_propensity
                .iter()
                .all(|p| (0.0..=1.0).contains(p))
        );
        assert!(
            scene.cloud_type.iter().all(|&t| t <= 5),
            "cloud type is a 0..=5 enum tag"
        );
    }

    /// claim: structural(seed: 42) — pinned RotationPin::Locked
    #[test]
    fn locked_world_omits_circulation_bands_and_zeroes_swing() {
        use hornvale_astronomy::{RotationPin, SkyPins};
        use hornvale_kernel::Seed;
        use hornvale_worldgen::build_world;
        let sky = SkyPins {
            rotation: Some(RotationPin::Locked),
            ..Default::default()
        };
        let world = build_world(Seed(42), &sky, &Default::default(), &Default::default())
            .expect("seed 42 builds locked");
        let scene = tiles_scene(&world, 32).unwrap();
        assert_eq!(scene.circulation_bands, None, "locked world has no bands");
        assert!(
            scene.t_swing_c.iter().all(|&s| s == 0.0),
            "locked world has no seasonal swing"
        );
        // The omitted field must not appear in the JSON.
        let json = scene_json(&scene);
        assert!(
            !json.contains("circulation_bands"),
            "absent field must not serialize"
        );
        assert!(json.contains("season_period_days"));
    }

    #[test]
    fn tiles_scene_marks_locked_worlds() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::build_world;
        let build = |s| {
            build_world(
                Seed(s),
                &Default::default(),
                &Default::default(),
                &Default::default(),
            )
            .unwrap()
        };
        assert!(
            tiles_scene(&build(8), 16).unwrap().locked,
            "seed 8 is tidally locked"
        );
        assert!(
            !tiles_scene(&build(42), 16).unwrap().locked,
            "seed 42 spins"
        );
    }

    #[test]
    fn width_violations_are_loud() {
        // TilesScene has no PartialEq, so assert on the error side only.
        let w = world();
        assert!(matches!(
            tiles_scene(&w, 15),
            Err(SceneError::WidthOutOfRange(15))
        ));
        assert!(matches!(
            tiles_scene(&w, 8),
            Err(SceneError::WidthOutOfRange(8))
        ));
        assert!(matches!(
            tiles_scene(&w, 2048),
            Err(SceneError::WidthOutOfRange(2048))
        ));
        assert!(matches!(tiles_scene(&w, 17), Err(SceneError::WidthOdd(17))));
        assert!(tiles_scene(&w, 18).is_ok());
    }

    #[test]
    fn flagship_appears_exactly_once() {
        let scene = tiles_scene(&world(), 32).unwrap();
        let flagships = scene
            .features
            .iter()
            .filter(|f| f.kind == "flagship")
            .count();
        assert!(flagships <= 1);
        if let Some(f) = scene.features.iter().find(|f| f.kind == "flagship") {
            // Identity is POSITION, not name. Since the drawn stem was
            // retired a settlement name is a translatable description of its
            // site, and many places legitimately share one — at seed 42 nine
            // settlements are called "Ka", on four different continents. What
            // this test defends is that the flagship is not EMITTED twice,
            // once under each `kind`, and two emissions of one place
            // necessarily share its coordinates. Keying on name made this
            // pass only while the flagship's own name happened to be unique.
            assert_eq!(
                scene
                    .features
                    .iter()
                    .filter(|g| g.latitude == f.latitude && g.longitude == f.longitude)
                    .count(),
                1,
                "flagship duplicated as a settlement at its own coordinates"
            );
        }
    }

    #[test]
    fn system_scene_has_the_schema_moons_and_is_deterministic() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::build_world;
        // `gen` is a reserved keyword under this workspace's 2024 edition
        // (the brief's original name); `gen_world` sidesteps it.
        let gen_world = || {
            build_world(
                Seed(42),
                &Default::default(),
                &Default::default(),
                &Default::default(),
            )
            .expect("seed 42 builds")
        };
        let scene = system_scene(&gen_world()).expect("generated world has a system");
        assert_eq!(scene.schema, "scene/system/v1");
        assert_eq!(scene.seed, 42);
        assert_eq!(scene.moons.len(), 2, "seed 42 has two moons");
        assert!(scene.world.year_days > 0.0);
        assert!(scene.world.day_length_days.is_some(), "seed 42 spins");
        assert!(scene.star.hz_inner_au < scene.star.hz_outer_au);
        for m in &scene.moons {
            assert!((0.0..=180.0).contains(&m.inclination_deg));
            assert!((0.0..360.0).contains(&m.node_longitude_deg));
        }
        assert!(
            scene.moons.iter().any(|m| m.inclination_deg > 90.0),
            "seed 42 has a retrograde captured moon (The Reckoning)"
        );
        // Byte-identical when serialized: determinism.
        assert_eq!(
            system_json(&scene),
            system_json(&system_scene(&gen_world()).unwrap())
        );
    }

    #[test]
    fn temperature_grid_matches_direct_sampling_and_zero_phase_mean() {
        use hornvale_kernel::Seed;
        use hornvale_worldgen::{build_world, climate_of};
        let world = build_world(
            Seed(42),
            &Default::default(),
            &Default::default(),
            &Default::default(),
        )
        .expect("seed 42 builds");
        let width = 32;
        let height = width / 2;
        let climate = climate_of(&world).expect("climate builds");
        let climate_index = NearestVertexIndex::new(climate.geosphere());
        let day = 91.3;
        let grid = temperature_grid(
            &world,
            width,
            WorldTime::from_std_days(day).expect("finite"),
        )
        .expect("grid builds");
        assert_eq!(grid.len(), (width * height) as usize);

        // Independently reconstruct the same lattice sampling in the test
        // (not by calling into tiles_scene's loop) and compare tile-by-tile
        // against the direct `temperature_at` reading.
        let mut i = 0;
        for py in 0..height {
            let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
            for px in 0..width {
                let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
                let c_vertex = climate_index.nearest(climate.geosphere(), latitude, longitude);
                let expected = climate
                    .temperature_at(c_vertex, WorldTime::from_std_days(day).expect("finite"))
                    .get();
                assert_eq!(grid[i], expected, "tile {i} mismatch at day {day}");
                i += 1;
            }
        }

        // At the true zero-phase day the seasonal sine term is exactly zero,
        // so temperature_grid must agree with tiles_scene's t_mean_c (mean at
        // zero phase) plus the diurnal term — which does NOT vanish here (it
        // depends on the day's fractional part, not the year phase). Day zero
        // only coincides with zero phase when `year_phase_offset` is zero, so
        // shift the probed day by the offset.
        use hornvale_climate::RotationRegime;
        let period = climate.year_length_std();
        let offset = climate.year_phase_offset();
        let obliquity_deg = climate.obliquity_deg();
        let zero_phase_day = (-offset).rem_euclid(1.0) * period;
        // BOTH HALVES MUST SAMPLE THE SAME INSTANT (The Foliot, stage 4).
        // `temperature_grid` takes a `WorldTime` now, so it lands
        // `zero_phase_day` on the tick lattice; deriving `day_fraction` from
        // the UNROUNDED value would compare the grid at one instant against
        // an expectation computed at another, and the two disagreed in the
        // 8th decimal. Round once, here, and read the fraction back off the
        // instant the grid will actually use.
        let zero_phase_at =
            WorldTime::from_std_days(zero_phase_day).expect("a zero-phase day is finite");
        let day_fraction = zero_phase_at.as_std_days().rem_euclid(1.0);
        let zero_grid = temperature_grid(&world, width, zero_phase_at).expect("grid builds");
        let scene = tiles_scene(&world, width).expect("scene builds");
        let mut i = 0;
        for py in 0..height {
            let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(height) * 180.0;
            for px in 0..width {
                let longitude = (f64::from(px) + 0.5) / f64::from(width) * 360.0 - 180.0;
                let c_vertex = climate_index.nearest(climate.geosphere(), latitude, longitude);
                let diurnal = match climate.regime() {
                    RotationRegime::Locked => 0.0,
                    RotationRegime::Spinning { day_std } => hornvale_climate::diurnal_anomaly(
                        climate.diurnal_amp_at(c_vertex),
                        climate.geosphere().coord(c_vertex).latitude,
                        climate.geosphere().coord(c_vertex).longitude,
                        obliquity_deg,
                        // The year phase AT THE INSTANT THE GRID USES, not
                        // the literal 0.0 this once passed. `zero_phase_day`
                        // is by construction the day where the phase is
                        // exactly zero, but landing it on the tick lattice
                        // moves it by up to half a tick, so the phase there is
                        // near zero rather than at it. Passing 0.0 compared
                        // the grid at one phase against an expectation at
                        // another and disagreed in the 12th decimal.
                        (zero_phase_at.as_std_days() / period + offset).rem_euclid(1.0),
                        day_fraction,
                        day_std,
                    )
                    .get(),
                };
                let expected = scene.t_mean_c[i] + diurnal;
                // A TOLERANCE, AND THE WEAKENING IS DELIBERATE (The Foliot,
                // stage 4). This was `assert_eq!` on the bits, which held
                // while the grid took a raw `f64` day: the probe could sit
                // exactly on the zero-phase day, the seasonal term
                // `sin(TAU * phase)` was exactly zero, and `t_mean_c +
                // diurnal` was the whole value.
                //
                // The grid takes a `WorldTime` now, so the probe lands on the
                // tick lattice and the zero-phase instant is generally NOT
                // representable. The phase there is ~1e-9 rather than 0, and
                // the seasonal term it produces is ~1.6e-9 — real, and
                // omitted by this expectation on purpose, because adding it
                // would mean duplicating another slice of the temperature
                // model inside its own test.
                //
                // What the assertion is FOR survives intact: the grid
                // decomposes into the zero-phase mean plus the diurnal
                // anomaly. The bound is 1e-6, six orders under the ~1e-9
                // residual and far under any real temperature difference.
                assert!(
                    (zero_grid[i] - expected).abs() < 1e-6,
                    "zero-phase temperature_grid must equal t_mean_c + diurnal at tile {i}: \
                     {} vs {expected}",
                    zero_grid[i]
                );
                i += 1;
            }
        }
    }

    #[test]
    fn moons_scene_has_schema_indices_and_is_deterministic() {
        let a = moons_scene(&mooned_world()).expect("generated world has moons");
        assert_eq!(a.schema, "scene/moons/v1");
        assert_eq!(a.seed, mooned_world().seed.0);
        assert_eq!(a.moons.len(), 2, "seed 42 has two moons");
        assert_eq!(a.moons[0].index, 0);
        assert_eq!(a.moons[1].index, 1);
        // Byte-identical on rebuild (determinism from the seed alone).
        assert_eq!(
            moons_json(&a),
            moons_json(&moons_scene(&mooned_world()).unwrap())
        );
    }

    #[test]
    fn derived_params_follow_real_density_physics() {
        // Independently recompute r = (3M/4piρ)^(1/3) and g = GM/r^2 from
        // each moon's own surfaced mass_rel/density_g_cm3 fields — not by
        // calling hornvale_astronomy::radius_km or super::surface_gravity_ms2
        // again — so this catches a wiring bug (wrong mass/density passed
        // through) as well as a physics regression.
        const LUNAR_MASS_KG: f64 = 7.342e22;
        let scene = moons_scene(&mooned_world()).unwrap();
        assert_eq!(scene.moons.len(), 2, "seed 42 has two moons");
        for m in &scene.moons {
            let mass_kg = m.mass_rel * LUNAR_MASS_KG;
            let rho_kg_m3 = m.density_g_cm3 * 1000.0;
            let v_m3 = 3.0 * mass_kg / (4.0 * std::f64::consts::PI * rho_kg_m3);
            let expected_r_km = hornvale_kernel::math::powf(v_m3, 1.0 / 3.0) / 1000.0;
            assert!(
                (m.radius_km - expected_r_km).abs() < 1e-6,
                "radius must equal (3M/4piρ)^(1/3) at density {}: got {}, expected {}",
                m.density_g_cm3,
                m.radius_km,
                expected_r_km
            );
            const G_SI: f64 = 6.6743e-11;
            let expected_g = G_SI * mass_kg / (expected_r_km * 1000.0 * expected_r_km * 1000.0);
            assert!(
                (m.surface_gravity_ms2 - expected_g).abs() < 1e-9,
                "gravity must equal GM/r^2: got {}, expected {}",
                m.surface_gravity_ms2,
                expected_g
            );
        }
    }

    #[test]
    fn derived_params_equal_luna_at_one_lunar_mass_and_giant_impact_density() {
        use hornvale_astronomy::{GramsPerCm3, LunarMasses};
        // Base every non-overridden field on a real generated moon (never a
        // from-scratch literal), then override mass/density to the Luna
        // calibration point — same pattern the astronomy domain's own
        // `radius_follows_from_mass_and_real_density_not_an_assumption` uses.
        let w = mooned_world();
        let sky = hornvale_worldgen::sky_of(&w).unwrap();
        let base = sky.system().moons[0].clone();
        let luna = hornvale_astronomy::Moon {
            mass: LunarMasses::new(1.0).unwrap(),
            density: GramsPerCm3::new(3.34).unwrap(),
            ..base
        };
        let r = hornvale_astronomy::radius_km(&luna);
        // Real physics from the real lunar mass (7.342e22 kg) and density
        // (3.34 g/cm3) gives ~1737.77 km — a hair above the old assumed-
        // constant anchor of 1737.4 km (the two agree to 4 significant
        // figures because the old anchor WAS Luna's real observed radius,
        // and 1737.77 is what the same mass/density pair yields when run
        // through r = (3M/4piρ)^(1/3) instead of being looked up).
        assert!((r - 1737.77).abs() < 0.01, "radius came out {r} km");
        let g = super::surface_gravity_ms2(r, 3.34);
        assert!((g - 1.6226).abs() < 0.001, "gravity came out {g} m/s2");

        // An icy body of the same mass is ~28% larger (task calibration:
        // (3.34/1.6)^(1/3) - 1 ≈ 0.278).
        let icy = hornvale_astronomy::Moon {
            density: GramsPerCm3::new(1.6).unwrap(),
            ..luna
        };
        let r_icy = hornvale_astronomy::radius_km(&icy);
        let ratio = r_icy / r;
        assert!(
            (1.27..1.29).contains(&ratio),
            "icy/impact radius ratio came out {ratio}"
        );
    }

    #[test]
    fn albedo_band_is_bright_for_icy_and_dark_for_rocky() {
        // `albedo_band` takes the already-classified flag, not a raw
        // density (review follow-up to Task 5b) — so this crate has only
        // the two branches to check; the "how close can a density be to
        // icy" boundary-precision question now belongs entirely to
        // `hornvale_astronomy::is_icy`'s own test.
        assert_eq!(super::albedo_band(true), (0.5, 0.7), "icy composition");
        assert_eq!(super::albedo_band(false), (0.1, 0.2), "non-icy composition");
    }

    #[test]
    fn descriptors_are_in_range() {
        let scene = moons_scene(&mooned_world()).unwrap();
        for m in &scene.moons {
            assert!((0.04..=0.7).contains(&m.albedo), "albedo in [0.04,0.7]");
            assert!((0.0..=1.0).contains(&m.cratering));
            assert!((0.0..=1.0).contains(&m.maria_fraction));
            for c in m.tint {
                assert!((0.0..=1.0).contains(&c));
            }
        }
    }

    /// claim: reachability(seed: 0..50) — an icy-density draw exceeds the old
    /// 0.5 albedo cap somewhere, plus a forall-seed clamp check
    #[test]
    fn icy_albedo_can_exceed_the_old_zero_point_five_cap() {
        // Seed 42's two real moons both land below 0.5 (neither is icy), so
        // `descriptors_are_in_range` alone never exercises the widened cap.
        // This directly proves an icy composition needs headroom above the
        // old [0.04,0.5] range: at least one index/seed with density 1.6
        // must land above 0.5, still within [0.04,0.7].
        let found_above_half = (0u64..50).any(|seed| {
            let d = super::seeded_descriptors(hornvale_kernel::Seed(seed), 0, 1.0, true);
            d.albedo > 0.5
        });
        assert!(
            found_above_half,
            "no icy-density draw exceeded the old 0.5 cap across 50 seeds"
        );
        // And the clamp still holds even at the icy band's own upper bound.
        for seed in 0u64..50 {
            let d = super::seeded_descriptors(hornvale_kernel::Seed(seed), 0, 1.0, true);
            assert!(
                (0.04..=0.7).contains(&d.albedo),
                "seed {seed}: {}",
                d.albedo
            );
        }
    }

    /// claim: invariant(forall-seed) — icy maria_fraction damped below rocky,
    /// with a non-vacuity guard (compared_a_nonzero_case)
    #[test]
    fn icy_composition_damps_maria_fraction_but_never_zeroes_it() {
        // Review follow-up: composition-blind maria_fraction let an icy
        // moon read as basaltic plains on an ice ball. Compare the SAME
        // (seed, index, mass) pair under is_icy=false vs true so only the
        // damping under test varies; every seed's icy value must be
        // strictly smaller than its rocky counterpart whenever the rocky
        // value is nonzero, and the damping ratio must match
        // ICY_MARIA_DAMPING exactly (a smooth multiplicative damping, not
        // a hard zero).
        let mut compared_a_nonzero_case = false;
        for seed in 0u64..50 {
            let s = hornvale_kernel::Seed(seed);
            let rocky = super::seeded_descriptors(s, 0, 1.0, false);
            let icy = super::seeded_descriptors(s, 0, 1.0, true);
            if rocky.maria_fraction > 0.0 {
                compared_a_nonzero_case = true;
                assert!(
                    icy.maria_fraction < rocky.maria_fraction,
                    "seed {seed}: icy maria_fraction {} must be damped below rocky {}",
                    icy.maria_fraction,
                    rocky.maria_fraction
                );
                assert!(
                    (icy.maria_fraction - rocky.maria_fraction * super::ICY_MARIA_DAMPING).abs()
                        < 1e-12,
                    "seed {seed}: damping must be exactly the ICY_MARIA_DAMPING multiplier"
                );
            }
        }
        assert!(
            compared_a_nonzero_case,
            "no seed produced a nonzero rocky maria_fraction to damp against"
        );
    }

    #[test]
    fn moons_scene_surfaces_density_and_formation() {
        // Seed 42: moon 0 is GiantImpact (density 3.34), moon 1 is Capture
        // (density either the rocky or icy reservoir).
        let scene = moons_scene(&mooned_world()).unwrap();
        assert_eq!(scene.moons[0].formation, "giant-impact");
        assert!(
            (scene.moons[0].density_g_cm3 - 3.34).abs() < 1e-9,
            "giant-impact density must be the derived constant 3.34, got {}",
            scene.moons[0].density_g_cm3
        );
        assert_eq!(scene.moons[1].formation, "capture");
        assert!(
            scene.moons[1].density_g_cm3 == 3.0 || scene.moons[1].density_g_cm3 == 1.6,
            "capture density must be one of the two drawn reservoirs, got {}",
            scene.moons[1].density_g_cm3
        );
    }

    /// claim: rate(forall-seed, aggregate mass-bias direction) — over 1..200,
    /// no world-building (pure function of seed/index/mass)
    #[test]
    fn mass_bias_actually_bites_across_seeds() {
        // The bias is a pure function of (seed, index, mass) — exercise it
        // directly over synthetic masses spanning the drawn range [0.05, 2.5]
        // (real moon mass is 0.05 + u·2.45), no world-building. Small moons
        // must read higher cratering and large moons higher maria in aggregate
        // (the bias, not a single-seed fluke).
        let small_masses = [0.05, 0.2, 0.4, 0.6];
        let large_masses = [1.6, 1.9, 2.2, 2.5];
        let mut small_cratering = 0.0;
        let mut large_cratering = 0.0;
        let mut small_maria = 0.0;
        let mut large_maria = 0.0;
        // Composition held fixed (non-icy, so the maria damping added by
        // the icy-composition review follow-up never fires) across every
        // call so only the mass bias under test varies.
        const FIXED_IS_ICY: bool = false;
        for seed in 1..200u64 {
            let s = hornvale_kernel::Seed(seed);
            for (i, &m) in small_masses.iter().enumerate() {
                let d = super::seeded_descriptors(s, i, m, FIXED_IS_ICY);
                small_cratering += d.cratering;
                small_maria += d.maria_fraction;
            }
            for (i, &m) in large_masses.iter().enumerate() {
                let d = super::seeded_descriptors(s, i, m, FIXED_IS_ICY);
                large_cratering += d.cratering;
                large_maria += d.maria_fraction;
            }
        }
        assert!(
            small_cratering > large_cratering,
            "small moons more cratered in aggregate"
        );
        assert!(
            large_maria > small_maria,
            "large moons more maria in aggregate"
        );
    }

    #[test]
    fn moons_scene_does_not_consume_draws_or_mutate_the_world() {
        // The save-format guard: the document is a pure read + hash, so building
        // it leaves the world byte-identical (no Stream draw, no mutation).
        let w = mooned_world();
        let before = serde_json::to_string(&w).unwrap();
        let _ = moons_scene(&w).unwrap();
        let after = serde_json::to_string(&w).unwrap();
        assert_eq!(before, after, "moons_scene must not alter the world");
    }

    #[test]
    fn surface_class_table_is_normative() {
        // is_icy wins even under high cratering/maria — it is checked
        // first (most specific), per the reference page's normative table.
        // `moon_surface_class` takes the already-classified flag, not a
        // raw density (review follow-up to Task 5b): the "which densities
        // actually read icy" question — formerly this test's job via
        // literal density values (1.6, 1.9, 3.0, 3.34) — now belongs
        // entirely to `hornvale_astronomy::is_icy`'s own
        // `is_icy_agrees_with_the_domains_own_density_reservoirs` test, so
        // it is not duplicated here.
        assert_eq!(moon_surface_class(true, 0.9, 0.9), "bright-icy");
        assert_eq!(moon_surface_class(false, 0.1, 0.5), "maria-rich");
        assert_eq!(moon_surface_class(false, 0.8, 0.1), "heavily-cratered");
        assert_eq!(moon_surface_class(false, 0.3, 0.1), "cratered-highland");
    }

    #[test]
    fn neighbors_scene_has_schema_populations_and_is_deterministic() {
        let a = neighbors_scene(&mooned_world()).expect("generated world has a sky");
        assert_eq!(a.schema, "scene/neighbors/v1");
        assert_eq!(a.seed, mooned_world().seed.0);
        assert!(
            (2..=5).contains(&a.neighbors.len()),
            "2-5 notable neighbors"
        );
        assert!((100..=300).contains(&a.stars.len()), "100-300 field stars");
        for (i, n) in a.neighbors.iter().enumerate() {
            assert_eq!(n.index, i);
        }
        // Brightest first — the generator's own ordering, preserved.
        for w in a.neighbors.windows(2) {
            assert!(w[0].brightness_rel >= w[1].brightness_rel);
        }
        // Byte-identical on rebuild (determinism from the seed alone).
        assert_eq!(
            neighbors_json(&a),
            neighbors_json(&neighbors_scene(&mooned_world()).unwrap())
        );
    }

    /// claim: structural(seed: none — mooned_world() fixture) — false-positive
    /// seed-loop flag; `s` binds a star
    #[test]
    fn neighbors_scene_fields_are_in_range() {
        let a = neighbors_scene(&mooned_world()).unwrap();
        for n in &a.neighbors {
            assert!((-90.0..=90.0).contains(&n.dec_deg));
            assert!((0.0..360.0).contains(&n.ra_deg));
            assert!(n.brightness_rel > 0.0);
            assert!((4.0..=80.0).contains(&n.distance_ly));
            assert!(!n.class_name.is_empty() && !n.color.is_empty());
        }
        for s in &a.stars {
            assert!((-90.0..=90.0).contains(&s.dec_deg));
            assert!((0.0..360.0).contains(&s.ra_deg));
            assert!((1..=5).contains(&s.magnitude_class));
        }
    }

    #[test]
    fn neighbors_scene_does_not_consume_draws_or_mutate_the_world() {
        // The save-format guard: the document is a pure read (plus an
        // on-demand starfield derivation from the astronomy seed), so
        // building it leaves the world byte-identical.
        let w = mooned_world();
        let before = serde_json::to_string(&w).unwrap();
        let _ = neighbors_scene(&w).unwrap();
        let after = serde_json::to_string(&w).unwrap();
        assert_eq!(before, after, "neighbors_scene must not alter the world");
    }

    #[test]
    fn eclipses_scene_has_schema_window_and_is_deterministic() {
        let w = mooned_world();
        // A wide window so seed 42's two moons produce several events.
        let a = eclipses_scene(
            &w,
            StdInstant::new(0.0).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            None,
        )
        .expect("mooned world has eclipses");
        assert_eq!(a.schema, "scene/eclipses/v3");
        assert_eq!(a.seed, w.seed.0);
        // Echoed back as exact ticks now, not quantized days (v2).
        assert_eq!(a.from, 0);
        assert_eq!(a.until, 2000 * WorldTime::TICKS_PER_STD_DAY);
        assert_eq!(a.coincidence_days, 0);
        assert!(
            !a.events.is_empty(),
            "seed 42's moons eclipse within 2000 days"
        );
        let json = serde_json::to_value(&a).expect("scene serializes");
        assert_eq!(
            json["recurrences"]
                .as_array()
                .expect("v3 carries recurrence records")
                .iter()
                .map(|record| {
                    (
                        record["moon_index"].as_u64().unwrap(),
                        record["body"].as_str().unwrap(),
                    )
                })
                .collect::<Vec<_>>(),
            vec![(0, "solar"), (0, "lunar"), (1, "solar"), (1, "lunar")]
        );
        assert!(
            json.get("observer").is_none(),
            "an omitted observer query emits no top-level observer"
        );
        // Day-ascending, inside the window.
        for e in &a.events {
            assert!((0..=2000 * WorldTime::TICKS_PER_STD_DAY).contains(&e.day));
            assert!(e.body == "solar" || e.body == "lunar");
            assert!(e.kind == "total" || e.kind == "annular");
        }
        for win in a.events.windows(2) {
            assert!(
                (win[0].day, win[0].moon_index) <= (win[1].day, win[1].moon_index),
                "events are tick-ascending with moon-index tie breaks"
            );
        }
        // Solar events carry a track; lunar events carry none.
        for e in &a.events {
            if e.body == "solar" {
                let t = e.track.as_ref().expect("a solar event has a ground track");
                assert!((-90.0..=90.0).contains(&t.center_lat_deg));
                assert!((-180.0..180.0).contains(&t.start_lon_deg));
                assert_ne!(t.sweep_deg, 0.0);
                assert_eq!(t.global_coverage, t.sweep_deg.abs() >= 360.0);
            } else {
                assert!(e.track.is_none(), "a lunar event has no ground track");
            }
        }
        for event in json["events"].as_array().unwrap() {
            assert!(
                event.get("observer").is_none(),
                "events omit observer results when no observer was requested"
            );
            assert_eq!(
                event["region"],
                if event["body"] == "solar" {
                    "ground-track"
                } else {
                    "night-hemisphere"
                }
            );
        }
        // Byte-identical on rebuild.
        assert_eq!(
            eclipses_json(&a),
            eclipses_json(
                &eclipses_scene(
                    &w,
                    StdInstant::new(0.0).unwrap(),
                    StdInstant::new(2000.0).unwrap(),
                    None,
                )
                .unwrap()
            )
        );
        for token in
            eclipses_json(&a).split(|c: char| !(c.is_ascii_digit() || c == '.' || c == '-'))
        {
            if token.is_empty() || !token.contains('.') {
                continue;
            }
            assert!(
                significant_digits(token) <= 8,
                "unquantized float in eclipse scene JSON: {token}"
            );
        }
    }

    /// Every instant this schema emits is an exact tick count, and NO float
    /// instant survives beside it (inherited from v2, The Foliot).
    ///
    /// This test is the inverse of the one it replaces. The Escapement added
    /// the tick fields ADDITIVELY at v1 and pinned exactly that — schema
    /// still v1, the `f64` still present, a tick beside it — because the
    /// floats were a cross-repo contract that external consumers read. With
    /// both external clients out of scope the floats had no consumer left,
    /// so v2 completes the step 0188 deliberately left half-finished, and the
    /// property worth pinning inverts with it.
    ///
    /// Asserting the ABSENCE of the float fields is the load-bearing half:
    /// a test that only checked the tick fields exist would pass just as
    /// happily if the floats had been left behind.
    #[test]
    fn an_eclipse_emits_only_exact_ticks() {
        let w = mooned_world();
        let scene = eclipses_scene(
            &w,
            StdInstant::new(0.125).unwrap(),
            StdInstant::new(2000.375).unwrap(),
            None,
        )
        .expect("mooned world has eclipses");
        let json = serde_json::to_value(&scene).expect("serializes");

        assert_eq!(
            json["schema"], "scene/eclipses/v3",
            "the version moved with the shape, so a returning consumer fails \
             loudly on an unknown schema rather than on a missing field"
        );
        assert!(json["from"].is_i64(), "the window start is an exact tick");
        assert!(json["until"].is_i64(), "and so is the window end");
        for gone in ["from_day", "from_day_ticks", "until_day", "until_day_ticks"] {
            assert!(
                json.get(gone).is_none(),
                "v1's {gone} must not survive into v3 — checking only that the \
                 tick fields EXIST would pass with the floats left behind"
            );
        }

        assert!(
            !scene.events.is_empty(),
            "seed 42's moons eclipse within 2000 days"
        );
        let elem = &json["events"][0];
        assert!(elem["day"].is_i64(), "an event's instant is an exact tick");
        assert!(
            elem.get("day_ticks").is_none(),
            "and there is no separate tick field, because `day` IS the tick"
        );

        // The tick fields are exact conversions of the standard-day window
        // bounds and event days actually passed/produced, not the quantized
        // f64 round-tripped back through ticks.
        assert_eq!(scene.from, WorldTime::from_std_days(0.125).unwrap().ticks());
        assert_eq!(
            scene.until,
            WorldTime::from_std_days(2000.375).unwrap().ticks()
        );
        // THE ESCAPEMENT'S MINOR 6, CLOSED BY DELETION. This asserted that an
        // event's tick field agreed with converting its own emitted `f64`
        // day — and it could not fail, because both sides were built from the
        // same in-memory f64 (quantization happens at serialize, not here).
        // The review named it tautological and asked for the interesting
        // comparison instead: the tick against the EMITTED, quantized day.
        //
        // v2 removed the subject and v3 preserves that choice. There is no
        // emitted f64 day, so that comparison has nothing to compare and the
        // tautology has nothing to be tautological about. What is worth
        // asserting is that the event lands inside the window the caller
        // asked for, in the same units the window is expressed in — which the
        // old pair could not check, since one side was days and the other
        // ticks.
        assert!(
            (scene.from..=scene.until).contains(&scene.events[0].day),
            "an event's instant lies inside the queried window: {} not in {}..={}",
            scene.events[0].day,
            scene.from,
            scene.until
        );
    }

    /// Two caller bounds that emit the same tick must enumerate the same
    /// events. Querying with their hidden sub-tick residues would let equal
    /// v3 documents disagree about whether a boundary event exists.
    #[test]
    fn eclipse_event_enumeration_uses_the_emitted_tick_bounds() {
        let w = mooned_world();
        let broad = eclipses_scene(
            &w,
            StdInstant::new(0.0).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            None,
        )
        .unwrap();
        let boundary = broad.events[0].day;
        let per_day = WorldTime::TICKS_PER_STD_DAY as f64;
        let before = (boundary as f64 - 0.49) / per_day;
        let after = (boundary as f64 + 0.49) / per_day;
        assert_eq!(
            WorldTime::from_std_days(before).unwrap().ticks(),
            WorldTime::from_std_days(after).unwrap().ticks()
        );

        let before_scene = eclipses_scene(
            &w,
            StdInstant::new(before).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            None,
        )
        .unwrap();
        let after_scene = eclipses_scene(
            &w,
            StdInstant::new(after).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            None,
        )
        .unwrap();

        assert_eq!(before_scene.from, after_scene.from);
        assert_eq!(
            before_scene
                .events
                .iter()
                .map(|event| (event.day, event.moon_index))
                .collect::<Vec<_>>(),
            after_scene
                .events
                .iter()
                .map(|event| (event.day, event.moon_index))
                .collect::<Vec<_>>()
        );
    }

    /// The v3 track keeps the direction and unwrapped magnitude that wrapped
    /// endpoints cannot represent, including a full-world sweep.
    #[test]
    fn eclipse_track_wire_retains_directed_global_sweeps() {
        let track = ground_track_elem(hornvale_astronomy::GroundTrack {
            center_lat_deg: 12.0,
            half_width_deg: 2.0,
            start_lon_deg: 170.0,
            end_lon_deg: 80.0,
            duration_days: 0.25,
            sweep_deg: -450.0,
            global_coverage: true,
        });

        assert_eq!(track.sweep_deg, -450.0);
        assert!(track.global_coverage);
        let wire_track = serde_json::to_value(&track).unwrap();
        assert_eq!(wire_track["sweep_deg"], -450.0);
        assert_eq!(wire_track["global_coverage"], true);
    }

    #[test]
    fn eclipses_scene_rejects_a_lower_bound_below_the_wire_tick_range() {
        let w = mooned_world();
        let axis_edge_days = i64::MAX as f64 / WorldTime::TICKS_PER_STD_DAY as f64;
        let result = eclipses_scene(
            &w,
            StdInstant::new(-axis_edge_days - 2000.0).unwrap(),
            StdInstant::new(-axis_edge_days + 2000.0).unwrap(),
            None,
        );

        assert!(
            matches!(result, Err(SceneError::Build(message)) if message.contains("outside the representable tick range")),
            "a finite lower bound below i64::MIN ticks must return a scene error"
        );
    }

    #[test]
    fn eclipses_scene_rejects_an_upper_bound_above_the_wire_tick_range() {
        let w = mooned_world();
        let axis_edge_days = i64::MAX as f64 / WorldTime::TICKS_PER_STD_DAY as f64;
        let result = eclipses_scene(
            &w,
            StdInstant::new(axis_edge_days - 2000.0).unwrap(),
            StdInstant::new(axis_edge_days + 2000.0).unwrap(),
            None,
        );

        assert!(
            matches!(result, Err(SceneError::Build(message)) if message.contains("outside the representable tick range")),
            "a finite upper bound above i64::MAX ticks must return a scene error"
        );
    }

    /// Omitting an observer removes both the echoed query and every event's
    /// observation key. Supplying one keeps the physical region on the event
    /// and emits a distinct result even when that result is `unseen`.
    #[test]
    fn eclipses_scene_distinguishes_absent_observer_from_unseen() {
        let w = mooned_world();
        let observed = eclipses_scene(
            &w,
            StdInstant::new(0.0).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            Some(EclipseObserverQuery {
                latitude_deg: 0.0,
                longitude_deg: 540.0,
            }),
        )
        .expect("finite observer coordinates are valid");
        let json = serde_json::to_value(&observed).expect("scene serializes");

        assert_eq!(json["observer"]["latitude_deg"], 0.0);
        assert_eq!(
            json["observer"]["longitude_deg"], -180.0,
            "finite longitude is normalized to [-180, 180)"
        );
        let events = json["events"].as_array().unwrap();
        assert!(!events.is_empty());
        assert!(events.iter().all(|event| event["observer"].is_object()));
        assert!(
            events
                .iter()
                .any(|event| event["observer"]["visibility"] == "unseen"),
            "a supplied observer's unseen result is data, not absence"
        );
        for event in events {
            let result = event["observer"].as_object().unwrap();
            assert!(result.contains_key("side"));
            assert!(result.contains_key("visibility"));
            assert!(!result.contains_key("region"));
            assert!(!result.contains_key("track"));
            assert!(event.get("region").is_some());
            assert!(event.get("track").is_some());
        }
    }

    #[test]
    fn eclipses_scene_rejects_invalid_observer_coordinates() {
        let w = mooned_world();
        let scene = |observer| {
            eclipses_scene(
                &w,
                StdInstant::new(0.0).unwrap(),
                StdInstant::new(1.0).unwrap(),
                Some(observer),
            )
        };

        assert!(matches!(
            scene(EclipseObserverQuery {
                latitude_deg: 90.000_001,
                longitude_deg: 0.0,
            }),
            Err(SceneError::ObserverLatitudeOutOfRange(_))
        ));
        assert!(matches!(
            scene(EclipseObserverQuery {
                latitude_deg: f64::NAN,
                longitude_deg: 0.0,
            }),
            Err(SceneError::ObserverLatitudeOutOfRange(_))
        ));
        assert!(matches!(
            scene(EclipseObserverQuery {
                latitude_deg: 0.0,
                longitude_deg: f64::INFINITY,
            }),
            Err(SceneError::ObserverLongitudeNonFinite(_))
        ));
    }

    #[test]
    fn eclipses_scene_does_not_consume_draws_or_mutate_the_world() {
        // The save-format guard, mirroring the moons/neighbors parity tests:
        // the document is a pure read of already-derived astronomy, so building
        // it leaves the world byte-identical (no Stream draw, no mutation).
        let w = mooned_world();
        let before = serde_json::to_string(&w).unwrap();
        let _ = eclipses_scene(
            &w,
            StdInstant::new(0.0).unwrap(),
            StdInstant::new(2000.0).unwrap(),
            Some(EclipseObserverQuery {
                latitude_deg: 12.5,
                longitude_deg: -33.25,
            }),
        )
        .unwrap();
        let after = serde_json::to_string(&w).unwrap();
        assert_eq!(before, after, "eclipses_scene must not alter the world");
    }

    /// The Cistern's core guarantee: routing a scene through a prebuilt
    /// [`SceneContext`] moves zero bytes. `mooned_world()` is seed 42 with a
    /// generated sky — the same world `region.rs`'s `gen42()` builds, so this
    /// test covers the canonical fixture without a second helper.
    #[test]
    fn the_context_path_is_byte_identical_to_the_world_path() {
        let world = mooned_world();
        let ctx = SceneContext::build(&world).expect("context builds");

        // Tiles: the big document.
        let via_world = scene_json(&tiles_scene(&world, 64).expect("tiles"));
        let via_ctx = scene_json(&tiles_scene_in(&world, &ctx, 64).expect("tiles_in"));
        assert_eq!(
            via_world, via_ctx,
            "tiles_scene diverged from tiles_scene_in"
        );

        // Region: the hot path, across several addresses on one face.
        for ix in 0..3u32 {
            let via_world =
                region_json(&tiles_region_scene(&world, 0, 3, ix, 0, 8).expect("region"));
            let via_ctx = region_json(
                &tiles_region_scene_in(&world, &ctx, 0, 3, ix, 0, 8).expect("region_in"),
            );
            assert_eq!(via_world, via_ctx, "tiles_region_scene diverged at ix={ix}");
        }

        // Temperature: the third terrain-facing entry point.
        let via_world = temperature_grid(
            &world,
            64,
            WorldTime::from_std_days(100.0).expect("a finite sample day"),
        )
        .expect("temps");
        let via_ctx = temperature_grid_in(
            &world,
            &ctx,
            64,
            WorldTime::from_std_days(100.0).expect("a finite sample day"),
        )
        .expect("temps_in");
        assert_eq!(
            via_world, via_ctx,
            "temperature_grid diverged from temperature_grid_in"
        );

        // Regional temperature: the fourth, and the one a day loop sweeps —
        // so it is checked across several days on one address, not just one.
        for day in [0.0, 100.0, 233.5] {
            let via_world = temperature_grid_region(
                &world,
                0,
                3,
                0,
                0,
                8,
                WorldTime::from_std_days(day).expect("finite"),
            )
            .expect("region temps");
            let via_ctx = temperature_grid_region_in(
                &world,
                &ctx,
                0,
                3,
                0,
                0,
                8,
                WorldTime::from_std_days(day).expect("finite"),
            )
            .expect("region temps_in");
            assert_eq!(
                via_world, via_ctx,
                "temperature_grid_region diverged at day={day}"
            );
        }
    }

    /// The exact `"name":<value>` substring a document carries for `name`.
    ///
    /// Locates `"name":` and returns through the end of the value that
    /// follows: the matching `]` for an array (bracket depth, with string
    /// literals and their escapes skipped so a `"]"` inside a legend entry
    /// cannot close the run early), or the scalar run up to the next `,`/`}`.
    fn field_fragment<'a>(doc: &'a str, name: &str) -> &'a str {
        let key = format!("\"{name}\":");
        let start = doc
            .find(&key)
            .unwrap_or_else(|| panic!("field {name} is absent from the document"));
        let value_at = start + key.len();
        let bytes = doc.as_bytes();
        let mut i = value_at;
        if bytes[i] == b'[' {
            let mut depth = 0usize;
            let mut in_string = false;
            while i < bytes.len() {
                let c = bytes[i];
                if in_string {
                    match c {
                        b'\\' => i += 1,
                        b'"' => in_string = false,
                        _ => {}
                    }
                } else {
                    match c {
                        b'"' => in_string = true,
                        b'[' => depth += 1,
                        b']' => {
                            depth -= 1;
                            if depth == 0 {
                                return &doc[start..=i];
                            }
                        }
                        _ => {}
                    }
                }
                i += 1;
            }
            panic!("field {name}'s array is unterminated");
        }
        while i < bytes.len() && bytes[i] != b',' && bytes[i] != b'}' {
            i += 1;
        }
        &doc[start..i]
    }

    /// Assert two scene documents are byte-identical, reporting the FIRST
    /// divergence with context rather than dumping both strings.
    ///
    /// A bare `assert_eq!` on these is unusable: at width 64 it prints 20,014
    /// truncated characters and the reader still cannot see which field
    /// broke; at the Orrery's width 512 it would be 35 MB. Since a failure
    /// here always means one field moved, the byte index plus a window either
    /// side names that field immediately — the preceding `"key":` is visible
    /// in the left context.
    fn assert_same_document(expected: &str, actual: &str, what: &str) {
        if expected == actual {
            return;
        }
        let (e, a) = (expected.as_bytes(), actual.as_bytes());
        let at = (0..e.len().min(a.len()))
            .find(|&i| e[i] != a[i])
            .unwrap_or(e.len().min(a.len()));
        // Byte windows rendered lossily: a divergence can land mid-codepoint
        // (biome and settlement names are UTF-8), and a panicking slice would
        // replace the diagnostic with a worse one.
        let window = |s: &[u8]| {
            let lo = at.saturating_sub(80);
            let hi = (at + 80).min(s.len());
            String::from_utf8_lossy(&s[lo..hi]).into_owned()
        };
        panic!(
            "{what}: documents diverge at byte {at} \
             (expected {} bytes, actual {} bytes)\n  expected: ...{}...\n  actual:   ...{}...",
            e.len(),
            a.len(),
            window(e),
            window(a),
        );
    }

    /// The drift guard: the projected serializer at `all()` must reproduce the
    /// derive byte for byte. If someone adds a field to `TilesScene` and forgets
    /// the manual impl, this reds immediately.
    #[test]
    fn the_full_projection_equals_the_derive() {
        let world = mooned_world();
        let scene = tiles_scene(&world, 64).expect("tiles");
        assert_same_document(
            &scene_json(&scene),
            &scene_json_selected(&scene, &TileFields::all()),
            "the all() projection diverged from the derive",
        );
    }

    /// The design's load-bearing property: a field's bytes do not depend on
    /// which other fields were requested. Assert it for every layer, so the
    /// golden story is nineteen assertions instead of 2^19 documents.
    #[test]
    fn each_field_serializes_independently_of_the_others() {
        let world = mooned_world();
        let scene = tiles_scene(&world, 64).expect("tiles");
        let full = scene_json(&scene);
        for name in TileFields::ALL_NAMES {
            let one = scene_json_selected(&scene, &TileFields::only(&[name]).expect("known field"));
            let fragment = field_fragment(&one, name);
            assert!(
                full.contains(fragment),
                "field {name} serializes differently alone than in the full document"
            );
        }
    }

    /// The documented promise — "an unknown name is a hard error, never a
    /// silently dropped layer" (spec §3 item 1, and the schema chapter's
    /// projection section) — asserted at the Rust level.
    ///
    /// This exists because it did not. The whole-branch review mutated
    /// `only()`'s `None =>` arm to skip an unknown name instead of returning
    /// `UnknownTileField`, and every Rust guard in the workspace stayed green:
    /// the two projection tests above only ever pass names drawn from
    /// `ALL_NAMES`, and the goldens pin documents built from valid selections.
    /// Only the wasm drive script caught it, and `make world-check` is in
    /// neither `make gate` nor `make gate-full`. A promise about rejection
    /// cannot be tested by a suite that only ever supplies acceptable input.
    ///
    /// Both constructors are covered, because `parse_json` is the form a
    /// client actually reaches across the WASM boundary.
    #[test]
    fn an_unknown_layer_name_is_rejected_not_dropped() {
        // `TileFields` has no `PartialEq` (and does not need one), so assert on
        // the error rather than on the whole Result.
        fn assert_unknown(got: Result<TileFields, SceneError>, offender: &str, what: &str) {
            match got {
                Err(SceneError::UnknownTileField(name)) => {
                    assert_eq!(name, offender, "{what}: the refusal named the wrong layer")
                }
                Err(other) => panic!("{what}: expected UnknownTileField, got {other:?}"),
                Ok(_) => panic!("{what}: the unknown name was accepted, not refused"),
            }
        }

        // A near-miss is the realistic failure: the wire name is `elevation_m`.
        assert_unknown(
            TileFields::only(&["elevation"]),
            "elevation",
            "only() with a near-miss name",
        );
        // An unknown name among valid ones must reject the whole request
        // rather than quietly projecting the valid remainder.
        assert_unknown(
            TileFields::only(&["elevation_m", "not_a_layer", "ocean"]),
            "not_a_layer",
            "only() with an unknown name among valid ones",
        );
        // The same promise across the wasm boundary's wire form.
        assert_unknown(
            TileFields::parse_json("[\"elevation_m\",\"not_a_layer\"]"),
            "not_a_layer",
            "parse_json() with an unknown name among valid ones",
        );
        // Not vacuous: the valid forms of both constructors still succeed, and
        // agree about what was selected.
        let by_name = TileFields::only(&["elevation_m", "ocean"]).expect("both names are layers");
        let by_json =
            TileFields::parse_json("[\"elevation_m\",\"ocean\"]").expect("valid wire request");
        for name in TileFields::ALL_NAMES {
            let want = *name == "elevation_m" || *name == "ocean";
            assert_eq!(
                by_name.contains(name),
                want,
                "only(): wrong state for {name}"
            );
            assert_eq!(
                by_json.contains(name),
                want,
                "parse_json(): wrong state for {name}"
            );
        }
    }

    /// A request that is not a JSON array of strings is refused with the
    /// parser's reason, not silently treated as an empty selection — which
    /// would hand the caller a metadata-only document for a typo.
    #[test]
    fn a_malformed_layer_request_is_rejected() {
        for bad in [
            "",                     // empty input
            "[\"elevation_m\"",     // truncated array
            "{\"fields\":[]}",      // an object, not an array
            "[\"elevation_m\", 7]", // a non-string element
            "\"elevation_m\"",      // a bare string, not an array
        ] {
            match TileFields::parse_json(bad) {
                Err(SceneError::MalformedTileFields(msg)) => {
                    assert!(
                        !msg.is_empty(),
                        "the refusal of {bad:?} carried no parser reason"
                    );
                }
                other => panic!("parse_json({bad:?}) should be MalformedTileFields, got {other:?}"),
            }
        }
        // The empty ARRAY is legal and distinct from malformed input: it means
        // "metadata only" (schema chapter, projection section).
        let none = TileFields::parse_json("[]").expect("the empty selection is valid");
        for name in TileFields::ALL_NAMES {
            assert!(!none.contains(name), "the empty selection selected {name}");
        }
    }
}
