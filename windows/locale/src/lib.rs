#![warn(missing_docs)]
//! The locale window: a `RoomAddr` rendered as an observable place.

mod streams;
pub use streams::stream_labels;

mod regime;
pub use regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};

mod substrate;

mod micro;

mod grammar;

mod budget;
pub use budget::StrangeSite;
use budget::StrangenessBudget;

use hornvale_climate::{Biome, BiomeExpr, Formation, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{
    CellId, NearestCellIndex, RoomAddr, SeaLevelHeight, Seed, World, WorldTime, quantize,
};
use hornvale_terrain::GeneratedTerrain;
pub use hornvale_terrain::{CaveKind, WaterKind};
use hornvale_worldgen::{climate_from, terrain_of};
use serde::Serialize;

/// The versioned semantic schema this window emits (save-format class; a
/// changed meaning mints `locale/room/v2` alongside).
/// type-audit: bare-ok(identifier-text)
pub const ROOM_SCHEMA: &str = "locale/room/v2";

/// The document fields decided at canonical-grid resolution, in stable order.
/// The membership argument — including why four other families are absent —
/// is on [`Resolution::grid_resolution_fields`], and
/// `windows/locale/tests/water_reading.rs` pins both the inclusions and the
/// exclusions so the list reads as a decision rather than an oversight.
const GRID_RESOLUTION_FIELDS: [&str; 3] = ["biome", "cave", "fields.water"];

/// The document fields decided at channel (nearest-vertex) resolution, in
/// stable order. See [`Resolution::channel_resolution_fields`].
const CHANNEL_RESOLUTION_FIELDS: [&str; 1] = ["channel_bands"];

/// One placed exotic site, rendered for a reader.
/// type-audit: bare-ok(index: cell), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome), bare-ok(prose: descriptor)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StrangeSiteRow {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Site latitude, degrees (quantized).
    pub latitude: f64,
    /// Site longitude, degrees (quantized).
    pub longitude: f64,
    /// The base biome the site interrupts.
    pub biome: String,
    /// What makes it strange — the exotic clause for its negation vector.
    pub descriptor: String,
}

/// A room rendered as an observable place — ground truth, re-derivable, never
/// stored (UNI-20 derived view). Plain serializable values only.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(index: id), bare-ok(index: face), bare-ok(index: path), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome), pending(wave-1: channel_distance), pending(wave-1: channel_bands)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Locale {
    /// Schema tag (`locale/room/v2`).
    pub schema: &'static str,
    /// Packed room id (`RoomId.0`).
    pub id: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Child descent path.
    pub path: Vec<u8>,
    /// Refinement depth (`path.len()`).
    pub depth: u32,
    /// Centroid latitude, degrees (quantized).
    pub latitude: f64,
    /// Centroid longitude, degrees (quantized).
    pub longitude: f64,
    /// Inherited biome name (max-weight corner cell).
    pub biome: String,
    /// The same inherited biome, as the `hornvale_climate::Biome` enum
    /// `biome`'s prose string was rendered from. `#[serde(skip)]`: this
    /// carries no wire bytes, so `locale/room/v2`'s serialized shape is
    /// unchanged — it exists purely so an in-process consumer (e.g.
    /// `scene/surrounds/v1`) can index by enum identity instead of
    /// round-tripping through a string, the way `windows/scene/src/lib.rs`
    /// and `region.rs` already index tile/region biomes.
    #[serde(skip)]
    pub biome_kind: Biome,
    /// Blended continuous fields.
    pub fields: LocaleFields,
    /// The three canonical-grid corner cells and their integer weights.
    pub corners: Vec<CellWeight>,
    /// The strangeness overlay: descriptor, negation vector, and magnitude.
    pub regime: Regime,
    /// Base + vertical exits.
    pub exits: Vec<Exit>,
    /// The cave at the room's dominant corner (max-weight cell), if the
    /// terrain places one there — categorical, inherited, never blended,
    /// the same rule `fields.water` and `biome` follow (see
    /// [`dominant_corner`]). Appended after `exits` rather than inserted, so
    /// a document built before this field existed is still byte-identical up
    /// to this new trailing key.
    #[serde(serialize_with = "serialize_cave_kind")]
    pub cave: Option<CaveKind>,
    /// Signed angular distance from the room centroid to the nearest river
    /// channel, radians — **positive on the left bank facing downstream** —
    /// or `None` where the world has no channel network at all. Quantized at
    /// emit. Appended after `cave` rather than inserted, so a document built
    /// before this field existed is still byte-identical up to this new
    /// trailing key.
    ///
    /// This is the **quantity**, not a classification of it. A consumer bands
    /// it against [`Locale::channel_bands`] for its own question — a wader and
    /// a bridge-builder want different edges of the same number — and a stored
    /// class would answer only the one question whoever stored it had.
    ///
    /// **The sign means something only close in.** It is a signed distance to
    /// the nearest of many open arcs, so it also flips beyond a river's source
    /// and mouth and along the bisector between two arcs that meet, where the
    /// flip is a fact about the polyline soup rather than about water. Read it
    /// where the reading is inside its own bands — where banding it does not
    /// answer `Transverse::Dry` — and gate a crossing on the channel or bank
    /// band, not on the terrace. `ChannelNetwork::bank_signed_distance` carries
    /// the measurement that establishes this.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub channel_distance: Option<f64>,
    /// The four band edges that apply at this room — channel/bank,
    /// bank/floodplain, floodplain/terrace, terrace/dry — in the same angular
    /// units (radians) as [`Locale::channel_distance`], and `None` exactly
    /// when that is. Quantized at emit.
    ///
    /// **These edges are DISCHARGE-DEPENDENT**, and that asymmetry is the
    /// whole argument for storing a quantity beside them. The edges derive
    /// from the reach's discharge, gradient and local cell spacing
    /// (`hornvale_terrain::channel::band_edges`), so under a seasonality this
    /// campaign deliberately leaves open they move with the flood: the same
    /// room is bank in one season and channel in another **without moving**.
    /// The distance stays true across that; a stored classification would not.
    ///
    /// They are the edges of the nearest vertex of the polyline the distance
    /// was measured to — `ChannelNetwork::bank_reading` selects both in one
    /// call, so the pair can never disagree about which reach it describes.
    #[serde(serialize_with = "serialize_opt_quantized_array")]
    pub channel_bands: Option<[f64; 4]>,
    /// Which of this document's fields are decided at canonical-cell
    /// resolution and which at channel resolution (decision 0123).
    pub resolution: Resolution,
}

/// What this document's fields are decided at, so a reader can tell a field
/// that is flat from a field that is broken (decision 0123).
///
/// **Why a room says this at all.** A room at walking depth sits six
/// refinement levels below the canonical grid, so a field decided per grid
/// cell is necessarily identical across all `4^6 = 4096` rooms in that cell —
/// and now that the same document also carries a channel reading, it holds
/// fields at *three* different grains at once. Without this block a reader has
/// to guess which, and the last two campaigns' worth of diagnosis went into a
/// contradiction that guessing invented.
///
/// The shape deliberately mirrors `hornvale_scene::surrounds`'s `Resolution`
/// — the same three keys, in the same order, meaning the same things — rather
/// than inventing a second vocabulary for the same disclosure. It is not
/// *imported* from there only because `windows/scene` depends on this crate,
/// so the dependency cannot run the other way.
///
/// [`Resolution::channel_resolution_fields`] is the one addition: 0123's
/// single list assumed a single coarse category, and with two categories in
/// play a lone list would leave every unnamed key ambiguous between "finer
/// than the grid" and "not classified". A second parallel list is 0123's own
/// idiom applied again, not a new one.
///
/// **Declaring a resolution is not a step toward refining it** (0123 rule 4).
/// The disclosure is the finished answer for the fields it names.
/// type-audit: bare-ok(count: grid_level), bare-ok(count: depth_below_grid), bare-ok(identifier-text: grid_resolution_fields), bare-ok(identifier-text: channel_resolution_fields)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Resolution {
    /// The canonical grid's refinement level.
    pub grid_level: u32,
    /// How many levels below `grid_level` this room sits. Each level quarters
    /// a cell, so `4^depth_below_grid` rooms share one grid cell.
    pub depth_below_grid: u32,
    /// The names of this document's fields that are decided at canonical-grid
    /// resolution and therefore cannot vary below it, in stable order.
    ///
    /// Exactly `["biome", "cave", "fields.water"]` — the three categorical
    /// readings taken from the room's dominant corner cell (see
    /// [`dominant_corner`]), which is one grid cell and never a blend.
    ///
    /// Four families are deliberately absent, and the reasons differ, which is
    /// the test of whether a list like this means anything:
    ///
    /// - **The room's own address and geometry** (`schema`, `id`, `face`,
    ///   `path`, `depth`, `latitude`, `longitude`, `corners`, `exits`) — these
    ///   are not readings of the world at a place, they are the naming of the
    ///   place, and a reader never mistakes one for a flattened measurement.
    /// - **The blended continuous fields** (`fields.temperature_c`,
    ///   `fields.moisture`, `fields.elevation_m`, `fields.height_asl_m`) —
    ///   these are integer-barycentric means of three corner cells with
    ///   per-room weights, so they genuinely vary room by room. Listing them
    ///   would be false.
    /// - **`regime`** — mixed granularity, so 0123 rule 3 says list it in
    ///   neither: its substrate and biome expression come from the dominant
    ///   corner while `regime.micro` is hashed from the room address itself,
    ///   and the rendered descriptor reads both. Naming it would misstate half
    ///   of it, and naming `regime.micro` alone would claim a grain for a key
    ///   whose siblings do not share it.
    /// - **`channel_distance` and `channel_bands`** — not grid-resolution at
    ///   all; see [`Resolution::channel_resolution_fields`].
    ///
    /// `biome_kind` is `#[serde(skip)]` and carries no wire bytes, so it is
    /// not a document field and does not appear here — the same reason
    /// `hornvale_scene`'s list refuses to name a `cave` key its own document
    /// does not have. (This document *does* have one, which is why `cave` is
    /// listed above and is not there.)
    pub grid_resolution_fields: Vec<String>,
    /// The names of this document's fields decided at **channel** resolution
    /// — the nearest vertex of the nearest river polyline, which is neither
    /// the canonical cell nor the room — in stable order.
    ///
    /// Exactly `["channel_bands"]`. The band edges are a per-vertex property
    /// of a reach (its discharge, gradient and local cell spacing), so every
    /// room whose nearest vertex is the same vertex reads the same four
    /// numbers, and a walker sees them step rather than slide.
    ///
    /// **`channel_distance` is deliberately excluded, for the opposite reason
    /// to every exclusion above**: it is the *finest*-grained field this
    /// document carries, a continuous function of the room's own centroid that
    /// varies between any two rooms. It is constant below no resolution at
    /// all, so naming it here would be exactly the stale-and-trusted list 0123
    /// warns is worse than no list. (`hornvale_scene`'s `micro` is excluded
    /// from its list for the same reason.)
    pub channel_resolution_fields: Vec<String>,
}

/// A canonical-grid corner cell and its integer blend weight.
/// type-audit: bare-ok(index: cell), bare-ok(count: weight)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct CellWeight {
    /// Canonical-grid cell index.
    pub cell: u32,
    /// Integer weight (numerator over the summed denominator).
    pub weight: u64,
}

/// The blended continuous fields at the room centroid (weighted mean of the
/// three corner cells; quantized at emit).
/// type-audit: pending(wave-2: temperature_c), bare-ok(ratio: moisture), waiver(elevation-convention: elevation_m)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LocaleFields {
    /// Annual-mean temperature, °C.
    pub temperature_c: f64,
    /// Moisture (climate's dimensionless moisture field).
    pub moisture: f64,
    /// Elevation, meters.
    pub elevation_m: f64,
    /// Height above this world's sea level, metres — signed, negative below.
    /// `elevation_m` is the absolute isostatic reading and stays beside it,
    /// because every correct consumer already reads that one; this is the
    /// quantity a *reader* wants, and the one the relief bands are computed
    /// from (The Benchmark).
    #[serde(serialize_with = "serialize_height_asl")]
    pub height_asl_m: SeaLevelHeight,
    /// Salt/fresh water at the room: the water kind of this room's own
    /// dominant corner — categorical, never blended. `water.is_fresh()` is the
    /// drinkable query.
    ///
    /// **"Inherited" is the wrong word for this, and using it cost a campaign.**
    /// [`dominant_corner`] is evaluated per ROOM, over that room's own three
    /// corner weights, so this is *categorical nearest-neighbour
    /// interpolation* — the correct method for a nominal field — and not a
    /// value copied down from one cell to all 4^6 rooms inside it. The field
    /// does look flat across a narrow view, but that is the interpolation
    /// stencil being wider than the view rather than a defect in the field.
    ///
    /// **Do not refine it by thresholding a blend.** That was built and
    /// reverted: `WaterKind` is *nominal*, and a threshold is maximally
    /// nonlinear, so `classify(blend(drainage))` is not the area-weighted vote
    /// of `classify(drainage)` over the corners — it deletes the thin channels
    /// and shrank seed 42's fresh water at walking depth by 29%. Contrast
    /// `height_asl_m`'s relief bands, which may band a blend because relief is
    /// *ordinal*: a blend moves an ordinal value at most one band. Sub-cell
    /// water needs a flow graph, not a re-reading of this field. `WaterKind`
    /// lives in the terrain domain crate, which (decision 0002) depends on
    /// nothing but the kernel, so it cannot derive `Serialize` itself; this
    /// field serializes by its stable name instead (see `serialize_water_kind`).
    #[serde(serialize_with = "serialize_water_kind")]
    pub water: WaterKind,
}

/// Serialize a [`SeaLevelHeight`] as its quantized metres — the emit-boundary
/// quantization every float in this schema goes through (decision 0033). The
/// type cannot travel through JSON, so the *field name* carries the datum
/// instead; that pairing is the whole discipline.
fn serialize_height_asl<S: serde::Serializer>(h: &SeaLevelHeight, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_f64(quantize(h.get()))
}

/// Serialize a `WaterKind` by its stable lowercase-hyphenated name (the
/// `locale/room/v2` schema's water field). Unlike [`biome_prose_name`], this
/// stays kebab-case — water has no separate prose noun to protect, so there
/// is no shared-noun hazard here.
fn serialize_water_kind<S>(kind: &WaterKind, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(water_kind_name(*kind))
}

/// Stable name for a `WaterKind` (owned here, not Debug — kebab-case, unlike
/// [`biome_prose_name`]).
fn water_kind_name(k: WaterKind) -> &'static str {
    match k {
        WaterKind::Ocean => "ocean",
        WaterKind::SaltBasin => "salt-basin",
        WaterKind::River => "river",
        WaterKind::DryLand => "dry-land",
    }
}

/// Serialize an `Option<CaveKind>` by its stable name, `null` when absent —
/// the same shape [`serialize_water_kind`] uses for a kind that is always
/// present, extended for a field that may not be.
fn serialize_cave_kind<S>(kind: &Option<CaveKind>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match kind {
        Some(k) => serializer.serialize_str(k.name()),
        None => serializer.serialize_none(),
    }
}

/// Serialize an `Option<[f64; 4]>` as a quantized JSON array, `null` when
/// absent — the fixed-width companion to
/// [`hornvale_kernel::quantize::quantize_serde::opt_f64_field`], which the
/// kernel provides for a scalar and for a slice but not for an array. Same
/// emit-boundary quantization (decision 0033), so a consumer that bands
/// `channel_distance` against these edges bands exactly the numbers the
/// document shows it.
fn serialize_opt_quantized_array<S>(
    value: &Option<[f64; 4]>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match value {
        Some(edges) => hornvale_kernel::quantize::quantize_serde::vec_f64_field(edges, serializer),
        None => serializer.serialize_none(),
    }
}

/// Why a locale could not be described.
/// type-audit: bare-ok(prose: Build.0), bare-ok(prose: Unaddressable.0)
#[derive(Debug, Clone, PartialEq)]
pub enum LocaleError {
    /// Building the coarse world failed (worldgen).
    Build(String),
    /// The room is coarser than the canonical grid, so it has no inheritance.
    AboveGrid,
    /// The room address has no packed id (e.g. `path.len() > MAX_DEPTH`); its
    /// `RoomAddrError` debug is carried. Fail fast rather than mint a
    /// meaningless `id: 0`.
    Unaddressable(String),
}

impl std::fmt::Display for LocaleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocaleError::Build(m) => write!(f, "building the coarse world: {m}"),
            LocaleError::AboveGrid => {
                write!(
                    f,
                    "room is coarser than the canonical grid (no inheritance)"
                )
            }
            LocaleError::Unaddressable(m) => {
                write!(f, "room address is unaddressable: {m}")
            }
        }
    }
}

/// The reusable coarse-world build. Constructed once, reused across every
/// `describe` — so a locale stays a cheap derived view.
pub struct LocaleContext {
    seed: Seed,
    climate: GeneratedClimate,
    terrain: GeneratedTerrain,
    index: NearestCellIndex,
    globe_level: u32,
    budget: StrangenessBudget,
}

/// The corner cell a room's *categorical* readings come from: the greatest
/// blend weight, tie-broken to the lowest `CellId`.
///
/// One rule, one caller-visible consequence: every categorical field a room
/// reports — biome, water kind, substrate, and (since The Pigment) the rock
/// whose reflectance the colour layer reads — names the same cell. Splitting
/// this would let a room be described as granite lowland and drawn in
/// basalt grey.
fn dominant_corner(weights: &[(CellId, u64); 3]) -> (CellId, u64) {
    let mut best = weights[0];
    for &cand in &weights[1..] {
        if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
            best = cand;
        }
    }
    best
}

impl LocaleContext {
    /// Build the coarse world (climate + terrain + nearest-cell index) once.
    /// The sanctioned entry point for any caller that has not already
    /// sculpted terrain/climate itself — derives them once here and
    /// delegates to [`Self::build_from`] (the book-entry-point pattern: a
    /// wrapper that derives once, mirroring `windows/book`'s `parse_context`/
    /// `parse_context_from` split).
    // Named construction site (decision 0092): this entry wrapper sculpts/
    // fits once, then delegates to `build_from`.
    #[allow(clippy::disallowed_methods)]
    pub fn build(world: &World) -> Result<LocaleContext, LocaleError> {
        let terrain = terrain_of(world).map_err(|e| LocaleError::Build(e.to_string()))?;
        let climate =
            climate_from(world, &terrain).map_err(|e| LocaleError::Build(e.to_string()))?;
        Ok(Self::build_from(world, &terrain, &climate))
    }

    /// Build the coarse world from an ALREADY-sculpted terrain and fit
    /// climate (The Weir, Stage 2) — the "pass the pre-built value" idiom
    /// `hornvale_worldgen::climate_from` already established, so a caller
    /// that must also thread `terrain`/`climate` into further derivation
    /// (`Session::start`'s demography fit, the lab health sweep) builds them
    /// ONCE and shares the same pair with this context, instead of `build`
    /// quietly re-sculpting a second copy underneath it. Infallible: both
    /// inputs are already validated by construction (the caller obtained
    /// them from `terrain_of`/`climate_from` succeeding), so there is
    /// nothing left here that can fail. Byte-identical to `build` whenever
    /// `terrain` equals `terrain_of(world)` and `climate` equals
    /// `climate_from(world, &terrain)`.
    pub fn build_from(
        world: &World,
        terrain: &GeneratedTerrain,
        climate: &GeneratedClimate,
    ) -> LocaleContext {
        let index = NearestCellIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().level();
        let budget = StrangenessBudget::build(world.seed, climate, terrain);
        LocaleContext {
            seed: world.seed,
            climate: climate.clone(),
            terrain: terrain.clone(),
            index,
            globe_level,
            budget,
        }
    }

    /// The canonical globe level (canonical-grid refinement depth).
    /// type-audit: bare-ok(count)
    pub fn globe_level(&self) -> u32 {
        self.globe_level
    }

    /// The cached terrain provider — the reuse seam so a caller (e.g. the
    /// vessel window's `observable`) can pass it into `sky_report_from`
    /// instead of re-deriving it (The Retainer).
    pub fn terrain(&self) -> &GeneratedTerrain {
        &self.terrain
    }

    /// The cached climate provider — the reuse seam so a caller (e.g. the
    /// vessel window's `observable`) can pass it into `sky_report_from`
    /// instead of re-deriving it (The Retainer).
    pub fn climate(&self) -> &GeneratedClimate {
        &self.climate
    }

    /// The cached nearest-cell index — the reuse seam for a caller that must
    /// resolve an address to a cell itself (the same role `terrain()` plays for
    /// the terrain provider). Building a second index would duplicate a
    /// structure this context exists to hold once.
    pub fn nearest_index(&self) -> &NearestCellIndex {
        &self.index
    }

    /// The world's placed exotic sites, for findability (derived, not stored).
    pub fn strange_sites(&self) -> Vec<StrangeSite> {
        self.budget.sites()
    }

    /// Every placed exotic site, rendered for a reader: where it is, what
    /// biome it interrupts, and what makes it strange.
    ///
    /// The descriptor is not decoration. Sites are differentiated by their
    /// negation vector (energy × kingdom × endemic), so a listing of bare
    /// coordinates would render a world's worth of wonders as identical rows.
    pub fn strange_site_rows(&self) -> Vec<StrangeSiteRow> {
        self.strange_sites()
            .into_iter()
            .map(|s| {
                let cell = CellId(s.cell);
                let coord = self.climate.geosphere().coord(cell);
                StrangeSiteRow {
                    cell: s.cell,
                    latitude: quantize(coord.latitude),
                    longitude: quantize(coord.longitude),
                    biome: biome_prose_name(self.climate.biome_at(cell)).to_string(),
                    // `exotic_clause` reads only energy/kingdom/endemic, and a
                    // StrangeSite carries no substrate of its own (substrate is
                    // the ROOM's, from its derived regime), so `Ordinary` here
                    // is lossless rather than a stand-in.
                    descriptor: crate::grammar::exotic_clause(Negations {
                        substrate: Substrate::Ordinary,
                        energy: s.energy,
                        kingdom: s.kingdom,
                        endemic: s.endemic,
                    }),
                }
            })
            .collect()
    }

    /// A room's ground-truth locale at observation time `at`. Pure over
    /// (context, addr, at): same inputs → byte-identical `Locale`. v1 samples
    /// the time-independent annual mean and does not yet vary with `at`
    /// (threaded for the P8 temporal-phase layer).
    pub fn describe(&self, addr: &RoomAddr, at: WorldTime) -> Result<Locale, LocaleError> {
        self.describe_at(addr, at, None)
    }

    /// The reflectance of the rock underfoot at `addr`.
    ///
    /// A pure re-projection of the material buffer the terrain provider
    /// already holds — `material_at` and `rock_at` have been public all
    /// along, so this is an accessor, not a new derivation, and it stores
    /// nothing.
    ///
    /// The cell is the same *categorical* corner [`LocaleContext::describe`]
    /// takes its biome and water kind from (max blend weight, tie-break
    /// lowest `CellId` — the shared `dominant_corner`), never a blend of the
    /// three: rock class is categorical, and averaging granite with basalt
    /// would name a rock that is not there. Sharing that one rule is what
    /// makes the colour and the prose agree about which ground a room
    /// stands on.
    pub fn reflectance_at(
        &self,
        addr: &RoomAddr,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let cell = dominant_corner(&weights).0;
        let buffer = self.terrain.material_at(cell);
        let rock = self.terrain.rock_at(cell);
        Ok(hornvale_terrain::lithology::reflectance(&buffer, rock).integrate())
    }

    /// The water column at a marine cell: every stratum from the sunlit water
    /// down to the one the sea floor sits in, shallowest first. Empty on land.
    ///
    /// A cell's floor decides how deep its water goes — 50 m of water over a
    /// reef holds only the epipelagic, while 3,000 m holds three layers. This
    /// is the list a diver descends.
    pub fn water_column_at(&self, cell: CellId) -> Vec<Stratum> {
        let expr = self.climate.biome_expr_at(cell);
        if expr.realm != Realm::WATERWORLD {
            return Vec::new();
        }
        let floor = expr.stratum;
        Realm::WATERWORLD
            .strata()
            .iter()
            .copied()
            .take_while(|s| *s != floor)
            .chain(std::iter::once(floor))
            .collect()
    }

    /// The biome expression at `cell` as seen from `stratum`. At the sea floor
    /// this is the cell's own community — a reef, a vent, a kelp forest. Above
    /// it there is only open water: the community lives on the floor, and
    /// floating a thousand metres over a reef is not being at the reef.
    pub fn expr_at_stratum(&self, cell: CellId, stratum: Stratum) -> BiomeExpr {
        let expr = self.climate.biome_expr_at(cell);
        if stratum == expr.stratum {
            expr
        } else {
            BiomeExpr {
                realm: expr.realm,
                formation: Formation::OpenWater,
                stratum,
            }
        }
    }

    /// [`LocaleContext::describe`], optionally as seen from a stratum within
    /// the water column rather than from the surface.
    pub fn describe_at(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        stratum: Option<Stratum>,
    ) -> Result<Locale, LocaleError> {
        // Fail fast on an unaddressable room (e.g. `path.len() > MAX_DEPTH`)
        // rather than mint a meaningless `id: 0` (fields are public, so a
        // caller can hand us an over-deep address).
        let id = addr
            .pack()
            .map_err(|e| LocaleError::Unaddressable(format!("{e:?}")))?
            .0;
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let _ = at; // v1: time-independent (see the doc above)
        self.describe_with_weights(addr, stratum, id, weights)
    }

    /// [`Self::describe_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// the shape a `&self`-only reader needs: a `&dyn Terrain` implementor
    /// (`windows/vessel`'s `LocaleTerrain`) can hold a prefilled cache and
    /// consult it from an ordinary `&self` trait method, never needing `&mut`
    /// access at read time. A cache miss falls through to a fresh
    /// [`RoomAddr::corner_weights`] call — correctness never depends on the
    /// cache being complete, only speed does. `cache: None` is byte-identical
    /// to `describe_at` (always a miss). Byte-identical to `describe_at` on
    /// a hit too, by construction (`corner_weights_lookup` only ever returns
    /// what [`RoomAddr::corner_weights_memo`] would have inserted, which is
    /// pinned bit-equal to `corner_weights` itself). The same `corner_weights`
    /// result [`Self::temperature_at_cached`], [`Self::productivity_at_cached`],
    /// [`Self::blend_at_cached`], and [`Self::hazards_at_cached`] would each
    /// independently recompute for the SAME room in one read scope (e.g.
    /// `windows/vessel`'s per-tick drive stack), a caller that shares one
    /// cache across all five collapses that back down to one scan.
    pub fn describe_at_cached(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        stratum: Option<Stratum>,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Result<Locale, LocaleError> {
        let id = addr
            .pack()
            .map_err(|e| LocaleError::Unaddressable(format!("{e:?}")))?
            .0;
        let geo = self.climate.geosphere();
        let weights = self
            .corner_weights_for(addr, geo, cache)
            .ok_or(LocaleError::AboveGrid)?;
        let _ = at; // v1: time-independent (see `describe_at`'s doc)
        self.describe_with_weights(addr, stratum, id, weights)
    }

    /// The shared corner_weights read every `_cached` reader uses: a hit in
    /// `cache` returns the memoized answer (including a memoized above-grid
    /// `None`, which is why the lookup itself returns `Option<Option<_>>` —
    /// see [`hornvale_kernel::RoomMeshMemo::corner_weights_lookup`]'s own
    /// doc); a miss (or no cache at all) falls through to a fresh
    /// [`RoomAddr::corner_weights`] call. No mutation — this never fills a
    /// miss back into `cache`, which is exactly what lets a `&self` reader
    /// use it without `&mut` access.
    fn corner_weights_for(
        &self,
        addr: &RoomAddr,
        geo: &hornvale_kernel::Geosphere,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<[(CellId, u64); 3]> {
        if let Some(cache) = cache {
            // The read-path half of the geo-aliasing guard (the-waymark fix
            // round, round 2): `corner_weights_memo`'s own `debug_assert_eq!`
            // catches a memo FED from two different globe levels; this
            // catches the other half — a memo READ against a `geo` it was
            // never filled against in the first place (e.g. a `LocaleTerrain`
            // handed a cache built for a different world/context's
            // geosphere). `None` (nothing inserted yet) asserts nothing —
            // there is no recorded level to disagree with.
            debug_assert!(
                cache
                    .corner_weights_geo_level()
                    .is_none_or(|level| level == geo.level()),
                "RoomMeshMemo read against a geosphere at a different level than it was \
                 filled with — a RoomAddr alone does not name which (Geosphere, \
                 NearestCellIndex) resolved it, so reading a cache built for a different \
                 world/context silently returns a stale corner_weights answer"
            );
            if let Some(hit) = cache.corner_weights_lookup(addr) {
                return hit;
            }
        }
        addr.corner_weights(geo, &self.index)
    }

    /// The shared tail of [`Self::describe_at`]/[`Self::describe_at_cached`]:
    /// everything past resolving `id` and `weights`, so the two callers can
    /// never drift apart in how a `Locale` is built from them.
    fn describe_with_weights(
        &self,
        addr: &RoomAddr,
        stratum: Option<Stratum>,
        id: u64,
        weights: [(CellId, u64); 3],
    ) -> Result<Locale, LocaleError> {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();

        // Categorical biome: max weight, tie-break lowest CellId. Inherited,
        // never re-quantized (decision 0038).
        let best = dominant_corner(&weights);
        let biome = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st).biome(),
            None => self.climate.biome_at(best.0),
        };

        // Continuous fields: integer-weighted mean, full precision, quantize
        // at emit.
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            quantize(sum / denom as f64)
        };
        let elevation_m = blend(&|c| self.terrain.globe().elevation.get(c).get());
        // `from_metres`, not a subtraction: the left operand is a three-corner
        // BLEND, not any single cell's reading, so there is no pair of
        // `ReferenceElevation`s here to subtract. Derived from the already-
        // quantized `elevation_m` and a quantized sea level so that the value
        // emitted and the band computed from it agree exactly with what a
        // consumer re-derives from the document.
        let sea_level_m = quantize(self.terrain.globe().sea_level.get());
        let fields = LocaleFields {
            temperature_c: blend(&|c| self.climate.mean_temperature_at(c).get()),
            moisture: blend(&|c| self.climate.moisture_at(c)),
            elevation_m,
            height_asl_m: SeaLevelHeight::from_metres(quantize(elevation_m - sea_level_m)),
            water: *self.terrain.globe().water_kind.get(best.0),
        };

        let substrate = crate::substrate::substrate_at(&self.climate, &self.terrain, best.0);
        let micro = crate::micro::micro_field(addr.seed(self.seed));
        let expr = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st),
            None => self.climate.biome_expr_at(best.0),
        };
        let mut regime = crate::grammar::derived_regime(self.seed, addr, expr, substrate, micro);
        if let Some(placed) = self.budget.regime_at(best.0) {
            let negations = Negations {
                substrate: regime.negations.substrate,
                energy: placed.energy,
                kingdom: placed.kingdom,
                endemic: placed.endemic,
            };
            let (descriptor, descriptor_noun) =
                crate::grammar::render(negations, micro, expr, self.seed, addr);
            regime = Regime {
                negations,
                micro,
                descriptor,
                descriptor_noun,
                strangeness: negations.strangeness(),
            };
        }

        // The channel reading. ONE call, so the distance and the edges are the
        // same reading by construction — a second query for the edges would be
        // a second chance to select a different vertex (and, on an exact tie,
        // a different line, whose downstream direction is what the SIGN
        // reports). Full precision here; quantized at emit, like every other
        // float in this schema.
        let reading = self.terrain.channels().bank_reading(addr.centroid());
        let coord = addr.coord();
        Ok(Locale {
            schema: ROOM_SCHEMA,
            id,
            face: addr.face,
            path: addr.path.clone(),
            depth: addr.depth(),
            latitude: quantize(coord.latitude),
            longitude: quantize(coord.longitude),
            biome: biome_prose_name(biome).to_string(),
            biome_kind: biome,
            fields,
            corners: weights
                .iter()
                .map(|&(c, w)| CellWeight {
                    cell: c.0,
                    weight: w,
                })
                .collect(),
            regime,                // strangeness overlay (§5-§7)
            exits: exits_of(addr), // base + vertical exits (§6)
            cave: self.terrain.cave_at(best.0).map(|c| c.kind),
            channel_distance: reading.map(|(d, _)| d),
            channel_bands: reading.map(|(_, edges)| edges),
            resolution: Resolution {
                grid_level: self.globe_level,
                // Non-negative by construction: `corner_weights` returned
                // `Some`, which it only does when `depth >= geo.level()`.
                depth_below_grid: addr.depth() - self.globe_level,
                grid_resolution_fields: GRID_RESOLUTION_FIELDS
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                channel_resolution_fields: CHANNEL_RESOLUTION_FIELDS
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
            },
        })
    }

    /// The room's PER-DAY temperature at `at`, °C — the diurnal+seasonal
    /// signal a thermal drive senses at its own cell, distinct from
    /// [`describe`](Self::describe)'s annual-MEAN `temperature_c` render field
    /// (left untouched, so the walk/almanac stay byte-identical). Blends the
    /// three corner cells' [`GeneratedClimate::temperature_at`] by the SAME
    /// integer barycentric weights `describe` uses for the mean. Full
    /// precision — this is a compute-path read, never a serialization
    /// boundary, so it is NOT quantized (quantize-at-emit-only). `None` for a
    /// room the canonical grid does not cover (above the grid or unaddressable);
    /// the caller supplies the never-chosen fallback.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at(&self, addr: &RoomAddr, at: WorldTime) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        Some(self.temperature_with_weights(at, weights))
    }

    /// [`Self::temperature_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `temperature_at`.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at_cached(
        &self,
        addr: &RoomAddr,
        at: WorldTime,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(self.temperature_with_weights(at, weights))
    }

    /// The shared tail of [`Self::temperature_at`]/[`Self::temperature_at_cached`]:
    /// the blend itself, once `weights` is resolved (the-waymark fix round,
    /// round 2 — kills the base/`_cached` duplicate body).
    fn temperature_with_weights(&self, at: WorldTime, weights: [(CellId, u64); 3]) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.temperature_at(c, at.day).get())
            .sum();
        sum / denom as f64
    }

    /// The room's material food PRODUCTIVITY in `[0, 1]` — a Miami-model
    /// net-primary-productivity proxy over the climate, the food-value field
    /// the drive layer's hunger drive reads (The Provender). Blends the three
    /// corner cells' annual-mean temperature and moisture by the SAME integer
    /// barycentric weights [`describe`](Self::describe) uses, then takes the
    /// Liebig minimum of a triangular temperature response and moisture — the
    /// same NPP proxy demography's carrying-capacity uses, computed here from
    /// this context's own climate rather than depending up into demography (a
    /// sibling consumer, not required to match it bit-for-bit; it grades cells
    /// for a hungry forager, it does not set population). Full precision — a
    /// compute-path read, never a serialization boundary, so NOT quantized.
    /// `None` for a room the canonical grid does not cover (the caller supplies
    /// the never-fed fallback). Time-independent (standing biomass is a slow,
    /// annual field), so it takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at(&self, addr: &RoomAddr) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        Some(self.productivity_with_weights(weights))
    }

    /// [`Self::productivity_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `productivity_at`.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at_cached(
        &self,
        addr: &RoomAddr,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(self.productivity_with_weights(weights))
    }

    /// The shared tail of [`Self::productivity_at`]/[`Self::productivity_at_cached`]
    /// (the-waymark fix round, round 2).
    fn productivity_with_weights(&self, weights: [(CellId, u64); 3]) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let blend = |value: &dyn Fn(CellId) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            sum / denom as f64
        };
        let temp = blend(&|c| self.climate.mean_temperature_at(c).get());
        let moisture = blend(&|c| self.climate.moisture_at(c));
        miami_npp(temp, moisture)
    }

    /// Corner-blend an externally-supplied per-cell `field` (over the canonical
    /// geosphere) at `addr` — the integer-barycentric read `productivity_at`/
    /// `hazards_at` use, generalized so a caller can sample a field this context
    /// does not itself hold. The Quarry injects `worldgen::predator_pressure_from`
    /// (the carnivore-pressure field) and reads it here per room. Full precision
    /// (a compute-path read, not quantized). `None` for a room the canonical grid
    /// does not cover.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at(&self, addr: &RoomAddr, field: &hornvale_kernel::CellMap<f64>) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        Some(Self::blend_with_weights(weights, field))
    }

    /// [`Self::blend_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `blend_at`.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at_cached(
        &self,
        addr: &RoomAddr,
        field: &hornvale_kernel::CellMap<f64>,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(Self::blend_with_weights(weights, field))
    }

    /// The shared tail of [`Self::blend_at`]/[`Self::blend_at_cached`] (the-waymark
    /// fix round, round 2). No `&self` needed — the blend reads only `weights`
    /// and the injected `field`.
    fn blend_with_weights(
        weights: [(CellId, u64); 3],
        field: &hornvale_kernel::CellMap<f64>,
    ) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * *field.get(c)).sum();
        sum / denom as f64
    }

    /// The room's THREAT in `[0, 1]` — the hazard field the danger drive flees
    /// (The Dread, split per-axis by The Bane) as `(uncanny, heat, cold)`, each
    /// in `[0, 1]`: the **uncanny** (a placed exotic site's normalized strangeness
    /// — the "cursed ground"), and **heat**/**cold** — how far the cell's
    /// annual-mean temperature is *above* a hot-danger threshold / *below* a
    /// cold-danger one, graded up to the lethal extreme (the deep ice, the molten
    /// waste). Reads the dominant corner cell's placed regime (like
    /// [`describe`](Self::describe) picks its biome) and a corner-blended mean
    /// temperature. Full precision — a compute-path read, never a serialization
    /// boundary, so NOT quantized. `None` for a room the canonical grid does not
    /// cover (the caller supplies the safe fallback). Time-independent, so it
    /// takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at(&self, addr: &RoomAddr) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = addr.corner_weights(geo, &self.index)?;
        Some(self.hazards_with_weights(weights))
    }

    /// [`Self::hazards_at`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] (the-waymark fix round, Finding 1) —
    /// see [`Self::describe_at_cached`] for the `&self`-only-reader
    /// rationale. `cache: None` is byte-identical to `hazards_at`.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at_cached(
        &self,
        addr: &RoomAddr,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(self.hazards_with_weights(weights))
    }

    /// The shared tail of [`Self::hazards_at`]/[`Self::hazards_at_cached`] (the-waymark
    /// fix round, round 2).
    fn hazards_with_weights(&self, weights: [(CellId, u64); 3]) -> (f64, f64, f64) {
        // The dominant corner cell (max weight, tie-break lowest CellId) — the
        // same pick `describe` uses for the categorical biome/regime.
        let mut best = weights[0];
        for &cand in &weights[1..] {
            if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
                best = cand;
            }
        }
        // The uncanny: a placed exotic site's strangeness, normalized to [0,1].
        let uncanny = self
            .budget
            .regime_at(best.0)
            .map(|n| n.strangeness() / crate::regime::STRANGENESS_CEILING)
            .unwrap_or(0.0);
        // Graded heat/cold: 0 within the safe band, rising to 1 at the lethal
        // extreme.
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let temp: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.mean_temperature_at(c).get())
            .sum::<f64>()
            / denom as f64;
        let heat = ((temp - HOT_DANGER_C) / (LETHAL_HEAT_C - HOT_DANGER_C)).clamp(0.0, 1.0);
        let cold = ((COLD_DANGER_C - temp) / (COLD_DANGER_C - LETHAL_COLD_C)).clamp(0.0, 1.0);
        (uncanny.clamp(0.0, 1.0), heat, cold)
    }
}

/// The annual-mean temperature (°C) at/below which a cell's COLD becomes a hazard
/// (The Bane) — graded from here down to [`LETHAL_COLD_C`]. Above the coldest
/// species niche, so ordinary cold is thermal discomfort (thermal's job), not
/// dread.
const COLD_DANGER_C: f64 = -20.0;

/// The annual-mean temperature (°C) at/above which a cell's HEAT becomes a hazard
/// (The Bane) — graded from here up to [`LETHAL_HEAT_C`].
const HOT_DANGER_C: f64 = 40.0;

/// The coldest annual-mean temperature (°C) any creature survives — a lethal
/// frozen waste, where COLD hazard saturates to `1` (The Bane).
const LETHAL_COLD_C: f64 = -40.0;

/// The hottest annual-mean temperature (°C) any creature survives — a lethal
/// molten waste, where HEAT hazard saturates to `1`.
const LETHAL_HEAT_C: f64 = 60.0;

/// The optimum temperature (°C) of the Miami NPP proxy's triangular
/// temperature response — mirrors demography's carrying-capacity model (a
/// sibling consumer of the same proxy; see [`LocaleContext::productivity_at`]).
const NPP_TEMP_OPTIMUM_C: f64 = 20.0;

/// The temperature tolerance (°C) either side of [`NPP_TEMP_OPTIMUM_C`] over
/// which the triangular temperature response falls to zero.
const NPP_TEMP_TOLERANCE_C: f64 = 30.0;

/// The Miami-model net-primary-productivity proxy in `[0, 1]`: the Liebig
/// minimum of a triangular temperature response about [`NPP_TEMP_OPTIMUM_C`]
/// and the (clamped) moisture. The food-value field's material-productivity
/// term (The Provender).
fn miami_npp(temperature_c: f64, moisture: f64) -> f64 {
    let temp_response =
        (1.0 - (temperature_c - NPP_TEMP_OPTIMUM_C).abs() / NPP_TEMP_TOLERANCE_C).clamp(0.0, 1.0);
    temp_response.min(moisture.clamp(0.0, 1.0))
}

/// Stable, human-readable biome name — spaced, not kebab-case (owned here,
/// not Debug). This is the prose grain: it is what the `locale/room/v2`
/// schema's `biome` field carries, and what a player reads. It is distinct
/// from [`hornvale_climate::Biome::name`], the kebab-case identifier used
/// for machine-readable catalogs (e.g. `scene/surrounds/v1`'s `biome_legend`
/// index) — the two must never be confused, or the same biome becomes two
/// different examinable nouns (The Margin).
/// type-audit: bare-ok(prose: return)
pub fn biome_prose_name(b: Biome) -> &'static str {
    match b {
        Biome::Ice => "ice",
        Biome::Tundra => "tundra",
        Biome::Taiga => "taiga",
        Biome::TemperateGrassland => "temperate grassland",
        Biome::Shrubland => "shrubland",
        Biome::TemperateForest => "temperate forest",
        Biome::TemperateRainforest => "temperate rainforest",
        Biome::Desert => "desert",
        Biome::Savanna => "savanna",
        Biome::TropicalSeasonalForest => "tropical seasonal forest",
        Biome::TropicalRainforest => "tropical rainforest",
        Biome::Alpine => "alpine",
        Biome::SeaIce => "sea ice",
        Biome::CoralReef => "coral reef",
        Biome::KelpForest => "kelp forest",
        Biome::HydrothermalVent => "hydrothermal vent",
        Biome::HadalTrench => "hadal trench",
        Biome::Upwelling => "upwelling",
        Biome::Epipelagic => "epipelagic",
        Biome::Mesopelagic => "mesopelagic",
        Biome::Bathypelagic => "bathypelagic",
        Biome::Abyssal => "abyssal",
    }
}

/// A way out of a room. `ExitKind` is open so overlay kinds (river/road/
/// tunnel/portal) and passability compose additively later.
/// type-audit: bare-ok(index: to)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Exit {
    /// Which way this exit goes.
    pub direction: Direction,
    /// The kind of traversal.
    pub kind: ExitKind,
    /// Destination packed room id.
    pub to: u64,
}

/// An exit direction: a lateral compass bearing, or a vertical scale change.
/// type-audit: bare-ok(index: Enter.0)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Direction {
    /// A lateral edge, bucketed to eight compass points.
    Compass(Compass),
    /// Descend into finer child `digit` (0..4).
    Enter(u8),
    /// Step back out to the containing room.
    Exit,
}

/// Eight-point compass bucket.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Compass {
    /// North.
    N,
    /// North-east.
    Ne,
    /// East.
    E,
    /// South-east.
    Se,
    /// South.
    S,
    /// South-west.
    Sw,
    /// West.
    W,
    /// North-west.
    Nw,
}

impl Compass {
    /// Every bearing the exit graph can name, one per variant — the roster the
    /// correspondence audit reconciles against the concept registry, the same
    /// discipline The Actants applied to the GOAP action roster.
    ///
    /// Kept exhaustive by [`compass_variants_must_all_be_rostered`]: a new
    /// variant fails to compile until it is listed here, so a bearing can never
    /// enter the world without the audit noticing it has no word.
    pub fn all() -> [Compass; 8] {
        [
            Compass::N,
            Compass::Ne,
            Compass::E,
            Compass::Se,
            Compass::S,
            Compass::Sw,
            Compass::W,
            Compass::Nw,
        ]
    }

    /// The concept name that would name this bearing, whether or not it is
    /// registered. The audit reports the ones that are not. The four cardinals
    /// are roots in language's universal stratum; the four intercardinals are
    /// compound-only concepts, named here by the same ids the recipe table
    /// keys on.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn concept_name(self) -> &'static str {
        match self {
            Compass::N => "north",
            Compass::Ne => "north-east",
            Compass::E => "east",
            Compass::Se => "south-east",
            Compass::S => "south",
            Compass::Sw => "south-west",
            Compass::W => "west",
            Compass::Nw => "north-west",
        }
    }
}

/// Compile-time tripwire: a new [`Compass`] variant breaks this match — every
/// variant is named and there is no `_` arm — forcing [`Compass::all`] and
/// [`Compass::concept_name`] to be revisited. The `manifest.rs` destructure
/// tripwire applied to an enum. Never remove, never add a wildcard arm.
#[allow(dead_code)]
fn compass_variants_must_all_be_rostered(c: Compass) -> &'static str {
    match c {
        Compass::N => "north",
        Compass::Ne => "north-east",
        Compass::E => "east",
        Compass::Se => "south-east",
        Compass::S => "south",
        Compass::Sw => "south-west",
        Compass::W => "west",
        Compass::Nw => "north-west",
    }
}

/// The traversal class of an exit.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum ExitKind {
    /// A geometric base-mesh edge.
    Edge,
    /// A vertical scale change (enter/exit).
    Vertical,
}

/// Bucket a bearing (degrees clockwise from north) to eight points. Bucketing
/// on a quantized bearing keeps it cross-platform stable.
fn compass(bearing_deg: f64) -> Compass {
    let b = quantize((bearing_deg % 360.0 + 360.0) % 360.0);
    let idx = (((b + 22.5) / 45.0).floor() as i64).rem_euclid(8);
    [
        Compass::N,
        Compass::Ne,
        Compass::E,
        Compass::Se,
        Compass::S,
        Compass::Sw,
        Compass::W,
        Compass::Nw,
    ][idx as usize]
}

fn exits_of(addr: &RoomAddr) -> Vec<Exit> {
    let mut exits = Vec::new();
    for n in addr.neighbors() {
        exits.push(Exit {
            direction: Direction::Compass(compass(addr.bearing_to(&n))),
            kind: ExitKind::Edge,
            to: n.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    if let Some(parent) = addr.parent() {
        exits.push(Exit {
            direction: Direction::Exit,
            kind: ExitKind::Vertical,
            to: parent.pack().map(|r| r.0).unwrap_or(0),
        });
    }
    for digit in 0..4u8 {
        if let Ok(child) = addr.child(digit) {
            exits.push(Exit {
                direction: Direction::Enter(digit),
                kind: ExitKind::Vertical,
                to: child.pack().map(|r| r.0).unwrap_or(0),
            });
        }
    }
    exits
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{RoomAddr, Seed, World, WorldTime};

    fn land_world() -> World {
        // Seed 42 is the project's canonical fixture; it has land.
        World::new(Seed(42))
    }

    #[test]
    fn describe_is_deterministic_across_two_contexts() {
        let world = land_world();
        let addr = RoomAddr {
            face: 0,
            path: vec![1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0],
        };
        let a = LocaleContext::build(&world).unwrap();
        let b = LocaleContext::build(&world).unwrap();
        let la = a.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let lb = b.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(
            serde_json::to_string(&la).unwrap(),
            serde_json::to_string(&lb).unwrap()
        );
    }

    #[test]
    fn describe_above_the_grid_errors() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // A room coarser than the canonical grid has no corner weights.
        let coarse = RoomAddr {
            face: 0,
            path: vec![1],
        };
        assert!(matches!(
            ctx.describe(&coarse, WorldTime { day: 0.0 }),
            Err(LocaleError::AboveGrid)
        ));
    }

    /// The listing must be reachable AND legible: every site carries where it
    /// is and what makes it strange. A bare coordinate list would render a
    /// world's worth of wonders as identical rows.
    #[test]
    fn strange_site_rows_carry_their_own_descriptor() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let rows = ctx.strange_site_rows();
        assert_eq!(
            rows.len(),
            ctx.strange_sites().len(),
            "every placed site is listed"
        );
        for r in &rows {
            assert!(
                !r.descriptor.is_empty(),
                "cell {} is placed as exotic but reads as nothing",
                r.cell
            );
            assert!(!r.biome.is_empty());
            assert!((-90.0..=90.0).contains(&r.latitude), "lat {}", r.latitude);
            assert!(
                (-180.0..=180.0).contains(&r.longitude),
                "lon {}",
                r.longitude
            );
        }
    }

    #[test]
    fn fields_are_within_the_corner_range() {
        // A weighted blend never leaves the min..max of its inputs.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        // elevation blends three real cells; the value must be finite.
        assert!(loc.fields.elevation_m.is_finite());
        assert!(loc.fields.temperature_c.is_finite());
        assert_eq!(loc.schema, ROOM_SCHEMA);
    }

    #[test]
    fn a_locale_reports_height_above_sea_level_not_the_raw_reading() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // The same address `fields_are_within_the_corner_range` uses, for the same
        // reason: it resolves on seed 42's mesh without needing a settlement.
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let sea = hornvale_kernel::quantize(ctx.terrain().globe().sea_level.get());
        let expected = hornvale_kernel::quantize(loc.fields.elevation_m - sea);
        assert_eq!(
            loc.fields.height_asl_m.get(),
            expected,
            "height_asl_m must be elevation_m re-datumed onto sea level, exactly"
        );
    }

    #[test]
    fn locale_water_field_varies_and_includes_fresh_water_on_seed_42() {
        // Wired to real geography (not a stuck constant) AND fresh water
        // exists — the sanity that unblocks The Surmise.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let mut kinds: std::collections::BTreeSet<WaterKind> = Default::default();
        let mut saw_fresh = false;
        for i in 0..400u32 {
            let t = i as f64;
            // a deterministic spread of directions over the sphere
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, 6);
            if let Ok(loc) = ctx.describe(&addr, WorldTime { day: 0.0 }) {
                kinds.insert(loc.fields.water);
                if loc.fields.water == WaterKind::River {
                    saw_fresh = true;
                }
            }
        }
        assert!(
            kinds.len() >= 2,
            "water must vary across the globe (wired to real geography), got {kinds:?}"
        );
        assert!(
            saw_fresh,
            "seed 42 must have fresh water (River) reachable on land — else lower RIVER_MIN_DRAINAGE"
        );
    }

    /// The Grain, Task 3: a room's `cave` must name the same cell `biome`
    /// and `water` do — the dominant corner, never a blend.
    ///
    /// A direct aim at a cave cell's own canonical-grid position does not
    /// reliably land that cell as the resolved address's dominant corner
    /// (the room mesh's nearest-cell resolution does not coincide with the
    /// geosphere's own cell centroids closely enough to guarantee it), so
    /// this uses the same directional-sweep idiom as
    /// `describe_and_reflectance_agree_on_one_dominant_cell` and
    /// `locale_water_field_varies_and_includes_fresh_water_on_seed_42`:
    /// scan a deterministic spread of directions, and for each resolved
    /// address's ACTUAL dominant corner (not a guess), check whether the
    /// terrain places a cave there. Seed 42 has 628 of 11 066 land cells
    /// carrying a cave (~5.7%, confirmed by a throwaway probe), so a 2000-
    /// direction sweep finds one reliably.
    #[test]
    fn describe_reports_the_cave_at_its_dominant_corner_on_seed_42() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let geo = ctx.climate.geosphere();
        let terrain = ctx.terrain();

        let mut found = false;
        for i in 0..2000u32 {
            let t = i as f64;
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, ctx.globe_level());
            let Some(weights) = addr.corner_weights(geo, &ctx.index) else {
                continue;
            };
            let dominant = dominant_corner(&weights).0;
            let Some(cave) = terrain.cave_at(dominant) else {
                continue;
            };
            let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
            assert_eq!(
                loc.cave,
                Some(cave.kind),
                "describe must report the dominant corner's cave at {addr:?}"
            );
            found = true;
            break;
        }
        assert!(
            found,
            "seed 42 must have a reachable cave findable by this sweep — if this \
             fails, that is a finding to report (no cave reachable), not a test \
             to weaken"
        );
    }

    #[test]
    fn describe_over_deep_address_errors() {
        // A path deeper than MAX_DEPTH (29) has no packed id: fail fast with
        // Unaddressable, never mint a valid-looking Locale with id: 0.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let over_deep = RoomAddr {
            face: 0,
            path: vec![0; 30],
        };
        assert!(matches!(
            ctx.describe(&over_deep, WorldTime { day: 0.0 }),
            Err(LocaleError::Unaddressable(_))
        ));
    }

    #[test]
    fn blend_and_inheritance_pin_exact_values() {
        // §14 Q4 regression: pin the blend/inheritance for a fixed seed-42
        // world at a fixed deep address. Values captured from a known-good run.
        // We pin the platform-EXACT quantities only: the quantized blended
        // temperature (byte-identical cross-platform) and the corner
        // (cell, weight) pairs (pure integer barycentric numerators — the
        // inheritance-selection inputs). The biome NAME is a depth-band
        // classification thresholded on host-libm transcendentals (elevation +
        // a percentile sea_level), i.e. the cross-platform-divergence class CI
        // excludes elsewhere — so we assert membership, not the exact string,
        // to keep the both-platform workspace gate stable.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(loc.fields.temperature_c, 38.082618);
        assert_eq!(
            loc.corners,
            vec![
                CellWeight {
                    cell: 3799,
                    weight: 46
                },
                CellWeight {
                    cell: 15109,
                    weight: 16
                },
                CellWeight {
                    cell: 15099,
                    weight: 130
                },
            ]
        );
        // Depth-band biome name: platform-sensitive, so assert only that a
        // known biome was selected (never the exact string).
        const KNOWN_BIOMES: &[&str] = &[
            "ice",
            "tundra",
            "taiga",
            "temperate grassland",
            "shrubland",
            "temperate forest",
            "temperate rainforest",
            "desert",
            "savanna",
            "tropical seasonal forest",
            "tropical rainforest",
            "alpine",
            "sea ice",
            "coral reef",
            "kelp forest",
            "hydrothermal vent",
            "hadal trench",
            "upwelling",
            "epipelagic",
            "mesopelagic",
            "bathypelagic",
            "abyssal",
        ];
        assert!(KNOWN_BIOMES.contains(&loc.biome.as_str()));
    }

    /// H4 (The Grain, Task 2): `dominant_corner`'s own doc claims every
    /// categorical field a room reports -- biome, water kind, substrate, and
    /// the rock the colour layer reads -- names the same cell. Nothing
    /// checked that before this test, and a reverted campaign (`dd523ab2`,
    /// reverted at `76068e6a`) split it silently: it stayed green through
    /// 3350 tests while water alone moved to a non-dominant reading.
    ///
    /// Sampled over many addresses (the same directional-spread idiom
    /// `locale_water_field_varies_and_includes_fresh_water_on_seed_42` uses)
    /// rather than one fixed address: a single room's three corner weights
    /// can coincidentally agree across categories even when the underlying
    /// wiring has split, so one address is not enough to trust a pass.
    #[test]
    fn describe_and_reflectance_agree_on_one_dominant_cell() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let geo = ctx.climate.geosphere();
        let mut checked = 0;
        for i in 0..200u32 {
            let t = i as f64;
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, 6);
            let Some(weights) = addr.corner_weights(geo, &ctx.index) else {
                continue;
            };
            let expected_cell = dominant_corner(&weights).0;
            let locale = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();

            assert_eq!(
                locale.biome_kind,
                ctx.climate.biome_at(expected_cell),
                "biome must name the dominant corner at {addr:?}"
            );
            assert_eq!(
                locale.fields.water,
                *ctx.terrain.globe().water_kind.get(expected_cell),
                "water kind must name the dominant corner at {addr:?}"
            );
            assert_eq!(
                locale.regime.negations.substrate,
                crate::substrate::substrate_at(&ctx.climate, &ctx.terrain, expected_cell),
                "substrate must name the dominant corner at {addr:?}"
            );

            let reflectance = ctx.reflectance_at(&addr).unwrap();
            let buffer = ctx.terrain.material_at(expected_cell);
            let rock = ctx.terrain.rock_at(expected_cell);
            let expected_reflectance =
                hornvale_terrain::lithology::reflectance(&buffer, rock).integrate();
            assert_eq!(
                reflectance, expected_reflectance,
                "the colour layer's rock must name the dominant corner at {addr:?}"
            );
            checked += 1;
        }
        assert!(
            checked > 50,
            "too few addresses resolved on the grid to trust this test"
        );
    }

    /// The room's dominant corner, read back off a rendered [`Locale`] through
    /// the SAME rule `describe` used to pick it — [`dominant_corner`]: max
    /// weight, tie-break lowest `CellId`.
    ///
    /// **Not `max_by_key(|c| c.weight)`**, which returns the LAST maximum on a
    /// tie. Three equal weights are common enough on this mesh that the two
    /// rules disagree in practice, and a test that used `max_by_key` compared
    /// against a cell production never chose — passing for a reason unrelated
    /// to what it claimed to measure. (Two paths in `windows/vessel` still
    /// resolve a cell that way; that divergence is recorded and unfixed.)
    fn dominant_of(loc: &Locale) -> CellId {
        let w: [(CellId, u64); 3] = [
            (CellId(loc.corners[0].cell), loc.corners[0].weight),
            (CellId(loc.corners[1].cell), loc.corners[1].weight),
            (CellId(loc.corners[2].cell), loc.corners[2].weight),
        ];
        dominant_corner(&w).0
    }

    /// Rooms spread across the WHOLE of `cell`'s dual region, at `depth` — one
    /// fan of samples running from near the cell's centre out towards each of
    /// its neighbours.
    ///
    /// **Not a contiguous BFS neighbourhood, and the difference is the whole
    /// point.** A conservation claim is about a cell, and a radius-4 patch
    /// covers about 1/132 of one; across a patch that small the three-corner
    /// blend of a terrain statistic moves ~2%, so a patch cannot see the
    /// variation that a cell-wide aggregate must account for. Fanning outward
    /// instead sweeps the neighbour's blend weight from nearly 0 to nearly 1/3,
    /// which is the range that actually exists inside the cell.
    ///
    /// **No sample is a cell centre, and none lies on the arc between two of
    /// them.** Rooms and cells subdivide the *same* icosphere, so a level-6 cell
    /// centre is also an exact corner of the level-12 room lattice, and the arc
    /// between two adjacent centres is an exact edge path of it.
    /// `RoomAddr::containing`'s spherical point-in-triangle test straddles on
    /// both: the descent falls through to its middle-child fallback at every
    /// level and converges on the centre of a base-face sub-triangle — measured
    /// ~5° from the point asked for, with all three corner weights equal
    /// (64/64/64), so even the dominant corner is a coin toss there. A water
    /// test built on such a room **passed against code that did not yet do what
    /// it claimed.** Hence `t` never reaches 0, and every sample carries a
    /// small off-lattice third component (`SKEW`) to leave the arc. `containing`
    /// is sound for ordinary points; it is lattice coincidences that degenerate.
    fn rooms_across_cell(
        geo: &hornvale_kernel::Geosphere,
        cell: CellId,
        depth: u32,
    ) -> Vec<RoomAddr> {
        /// Off-lattice third component. Small enough not to move which cell
        /// owns the sample, large enough to leave the arc.
        const SKEW: f64 = 0.031;
        let a = geo.position(cell);
        let ns = geo.neighbors(cell);
        let mut out = Vec::new();
        for i in 0..ns.len() {
            let b = geo.position(ns[i]);
            let c = geo.position(ns[(i + 1) % ns.len()]);
            for t in [0.04, 0.11, 0.19, 0.26, 0.32] {
                let w = 1.0 - t - SKEW;
                let raw = [
                    w * a[0] + t * b[0] + SKEW * c[0],
                    w * a[1] + t * b[1] + SKEW * c[1],
                    w * a[2] + t * b[2] + SKEW * c[2],
                ];
                let n = (raw[0] * raw[0] + raw[1] * raw[1] + raw[2] * raw[2]).sqrt();
                out.push(RoomAddr::containing(
                    [raw[0] / n, raw[1] / n, raw[2] / n],
                    depth,
                ));
            }
        }
        out
    }

    /// The plurality water kind of a room set, tie-broken to the lowest
    /// `WaterKind::index()` so the answer cannot vary between runs. This is the
    /// "aggregate" in H5's sense: what a coarse observer would conclude the
    /// cell's water is, told only what its rooms report.
    fn plurality(kinds: &[WaterKind]) -> WaterKind {
        let mut tally: std::collections::BTreeMap<u8, (usize, WaterKind)> = Default::default();
        for k in kinds {
            let e = tally.entry(k.index()).or_insert((0, *k));
            e.0 += 1;
        }
        tally
            .values()
            .fold(None::<(usize, WaterKind)>, |acc, &(n, k)| match acc {
                Some((bn, _)) if bn >= n => acc,
                _ => Some((n, k)),
            })
            .expect("a non-empty room set has a plurality")
            .1
    }

    /// How many canonical cells the conservation scan below covers, in each of
    /// its two halves. Capped because the scan pays a `describe` per room.
    ///
    /// The scan takes two samples deliberately. A **stride** over `CellId`
    /// order spans the globe, so conservation is asserted over ocean and dry
    /// land as well as rivers. A **River prefix** targets the one category the
    /// reverted mechanism actually deleted: a stride sample is ~94% ocean and
    /// dry land, where a threshold on a blend agrees with the partition almost
    /// everywhere, so a stride alone would leave the tripwire arm unable to
    /// fire for a reason that has nothing to do with the criterion.
    const CONSERVATION_CELLS: usize = 40;

    /// **H5 (The Grain) — the conservation criterion, and the only test in this
    /// file written for a mechanism that does not exist yet.**
    ///
    /// The claim: aggregating room-level water back over a canonical cell
    /// reproduces that cell's own water kind. Nearest-corner assignment
    /// satisfies it *by construction* — every room whose dominant corner is
    /// cell `C` reports `C`'s water kind, so the aggregate is unanimous — which
    /// is precisely why this test is cheap and precisely why it is worth
    /// having. It is a **tripwire for a future mechanism** (`MAP-64`'s flow
    /// graph, or anything else that tries to put water somewhere in particular
    /// inside a cell), not a discovery about today's code.
    ///
    /// Why write down something that holds trivially: a campaign built a
    /// sub-cell water mechanism that passed both of its preregistered
    /// hypotheses and the whole 3350-test suite, and it was illegal — it
    /// deleted 29% of seed 42's fresh water at walking depth. Both hypotheses
    /// asked about *local variation*; the violated property was *global
    /// conservation*, and no local hypothesis can detect that. This is the test
    /// that mechanism would have failed before it was ever built.
    ///
    /// **It asserts the strong form (unanimity), not merely the aggregate.**
    /// A plurality-only criterion would still permit deleting a category from a
    /// minority of cells, which is exactly the 29% loss — a thin river is a
    /// minority landform everywhere it exists. Unanimity implies the aggregate;
    /// the aggregate does not imply unanimity.
    ///
    /// **Second arm: the check is proved discriminating rather than assumed
    /// so.** Over the same rooms it also computes what the reverted mechanism
    /// (`dd523ab2`) would have assigned — `water::classify` over the room's own
    /// blended elevation and blended drainage, with the two flags still taken
    /// from the dominant corner, which is that commit exactly — and asserts
    /// that assignment *violates* conservation somewhere in the scan. Without
    /// that arm this test could pass against a criterion too weak to catch
    /// anything, which is the failure mode the campaign that wrote it spent
    /// itself diagnosing.
    ///
    /// **Measured on seed 42** over 80 cells and 2151 rooms: the partition
    /// conserves on every one, while the reverted mechanism breaks the aggregate
    /// form on **11 of 80 cells** and unanimity on **27 of 80**. The first draft
    /// of this test sampled a radius-4 BFS patch per cell and the tripwire arm
    /// found **0 of 44** — across 1/132 of a cell the blend barely moves, so the
    /// scan could not see what it was built to catch. That near miss is why the
    /// sampling is a cell-wide fan and why the second arm exists at all: a
    /// tripwire nobody has watched trip is a comment.
    #[test]
    fn room_water_is_conserved_when_aggregated_over_a_canonical_cell() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let geo = ctx.climate().geosphere();
        let globe = ctx.terrain().globe();
        let depth = ctx.globe_level() + 6;
        let sea_level_m = quantize(globe.sea_level.get());

        let all: Vec<CellId> = geo.cells().collect();
        let stride = (all.len() / CONSERVATION_CELLS).max(1);
        let mut scan: Vec<CellId> = all
            .iter()
            .copied()
            .step_by(stride)
            .take(CONSERVATION_CELLS)
            .chain(
                all.iter()
                    .copied()
                    .filter(|c| *globe.water_kind.get(*c) == WaterKind::River)
                    .take(CONSERVATION_CELLS),
            )
            .collect();
        scan.sort_by_key(|c| c.0);
        scan.dedup();

        let mut cells_checked = 0usize;
        let mut rooms_checked = 0usize;
        // Violations the REVERTED blended-threshold mechanism would cause,
        // counted at both strengths so the failure message can say which.
        let mut simulated_aggregate_violations = 0usize;
        let mut simulated_unanimity_violations = 0usize;

        for cell in scan {
            let expected = *globe.water_kind.get(cell);

            let mut simulated: Vec<WaterKind> = Vec::new();
            let mut rooms_in_cell = 0usize;
            for addr in rooms_across_cell(geo, cell, depth) {
                let Ok(loc) = ctx.describe(&addr, WorldTime { day: 0.0 }) else {
                    continue;
                };
                let dominant = dominant_of(&loc);
                // Only the rooms this cell actually owns. A radius-4 patch can
                // straddle a cell boundary, and a room on the other side of it
                // is a different cell's business.
                if dominant != cell {
                    continue;
                }
                rooms_in_cell += 1;
                rooms_checked += 1;

                // THE CONSERVATION CLAIM, in its strong form.
                assert_eq!(
                    loc.fields.water, expected,
                    "room {:?} is owned by cell {} but reports {:?} where the cell itself is \
                     {:?}; sub-cell water has stopped conserving its cell's kind",
                    addr, cell.0, loc.fields.water, expected
                );

                // The reverted mechanism, reconstructed: classify from the
                // room's own blended elevation and blended drainage, with
                // `endorheic` and terminal-sink status still read off the
                // dominant corner because a flag has no weighted mean.
                let denom: f64 = loc.corners.iter().map(|c| c.weight as f64).sum();
                let drainage_blend: f64 = loc
                    .corners
                    .iter()
                    .map(|c| c.weight as f64 * *globe.drainage.get(CellId(c.cell)))
                    .sum::<f64>()
                    / denom;
                let is_terminal_sink =
                    matches!(*globe.water_kind.get(dominant), WaterKind::SaltBasin);
                simulated.push(hornvale_terrain::water::classify(
                    loc.fields.elevation_m,
                    sea_level_m,
                    drainage_blend,
                    *globe.endorheic.get(dominant),
                    is_terminal_sink,
                ));
            }

            if rooms_in_cell == 0 {
                continue;
            }
            cells_checked += 1;
            if plurality(&simulated) != expected {
                simulated_aggregate_violations += 1;
            }
            if simulated.iter().any(|k| *k != expected) {
                simulated_unanimity_violations += 1;
            }
        }

        assert!(
            cells_checked > 20 && rooms_checked > 200,
            "only {cells_checked} cells / {rooms_checked} rooms resolved; the scan is too thin \
             to trust either arm"
        );

        // THE TRIPWIRE ARM. If this fails, the criterion above is not
        // discriminating and must not be trusted as a guard.
        assert!(
            simulated_aggregate_violations > 0,
            "the reverted blended-threshold mechanism violated conservation on \
             {simulated_aggregate_violations} of {cells_checked} cells by aggregate and \
             {simulated_unanimity_violations} by unanimity — an aggregate count of zero means \
             THIS TEST CANNOT DETECT the mechanism it was written to catch, and the criterion \
             needs strengthening rather than the assertion relaxing"
        );
    }

    #[test]
    fn regime_is_deterministic_and_siblings_differ() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let a = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0],
        };
        let b = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1],
        };
        let ra = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().regime;
        let ra2 = ctx.describe(&a, WorldTime { day: 0.0 }).unwrap().regime;
        let rb = ctx.describe(&b, WorldTime { day: 0.0 }).unwrap().regime;
        assert_eq!(ra, ra2, "same room → identical regime");
        assert_ne!(ra.descriptor, rb.descriptor, "sibling rooms should differ");
        assert!(ra.strangeness >= 0.0);
        assert!(!ra.descriptor.is_empty());
    }

    #[test]
    fn schema_is_v2_and_regime_present() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        assert_eq!(loc.schema, "locale/room/v2");
        assert!(loc.regime.strangeness >= 0.0);
        assert!(!loc.regime.descriptor.is_empty());
    }

    #[test]
    fn strange_sites_are_exposed() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        // A derived query; may be empty on a mundane world but must not panic.
        let _ = ctx.strange_sites();
    }

    #[test]
    fn exits_are_three_lateral_plus_vertical() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime { day: 0.0 }).unwrap();
        let lateral = loc
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .count();
        assert_eq!(lateral, 3, "exactly three geometric edges");
        assert!(
            loc.exits.iter().any(|e| e.direction == Direction::Exit),
            "a mid-mesh room has a parent (Exit)"
        );
        let enters = loc
            .exits
            .iter()
            .filter(|e| matches!(e.direction, Direction::Enter(_)))
            .count();
        assert_eq!(enters, 4, "four children to enter");
        // every lateral destination is one of the substrate's neighbours
        let ns: Vec<u64> = addr
            .neighbors()
            .iter()
            .map(|n| n.pack().unwrap().0)
            .collect();
        for e in loc.exits.iter().filter(|e| e.kind == ExitKind::Edge) {
            assert!(ns.contains(&e.to), "lateral exit must be a neighbour");
        }
    }

    #[test]
    fn compass_buckets_cover_the_circle() {
        assert_eq!(compass(0.0), Compass::N);
        assert_eq!(compass(90.0), Compass::E);
        assert_eq!(compass(180.0), Compass::S);
        assert_eq!(compass(270.0), Compass::W);
        assert_eq!(compass(45.0), Compass::Ne);
        assert_eq!(compass(359.9), Compass::N);
    }

    /// A small walk-visited neighborhood: `start` plus every room reachable
    /// within `hops` edge-steps (BFS over `RoomAddr::neighbors`, the same
    /// mesh a real possession walk traverses) — the-waymark Task 3's
    /// "rooms a real walk visits" fixture, sized for a fast unit test rather
    /// than a full possession transcript.
    fn walk_visited(start: &RoomAddr, hops: u32) -> Vec<RoomAddr> {
        let mut seen: std::collections::BTreeSet<RoomAddr> = std::collections::BTreeSet::new();
        let mut frontier = vec![start.clone()];
        seen.insert(start.clone());
        for _ in 0..hops {
            let mut next = Vec::new();
            for r in &frontier {
                for n in r.neighbors() {
                    if seen.insert(n.clone()) {
                        next.push(n);
                    }
                }
            }
            frontier = next;
        }
        seen.into_iter().collect()
    }

    #[test]
    fn the_five_cached_readers_bit_equal_their_recomputing_siblings_with_a_partial_prefill() {
        // The-waymark fix round, Finding 1: a PREFILLED, READ-ONLY cache
        // (never mutated by the readers themselves) must still be
        // byte-identical to the raw recomputing siblings, on BOTH a cache
        // hit (prefilled rooms) and a cache miss (rooms deliberately left
        // out of the prefill, falling through to a fresh `corner_weights`
        // call) — correctness must never depend on prefill completeness.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let start = RoomAddr {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let rooms = walk_visited(&start, 3);
        assert!(rooms.len() > 10, "fixture must cover a real neighborhood");
        let at = WorldTime { day: 12.5 };
        let zero_field = hornvale_kernel::CellMap::from_fn(ctx.climate().geosphere(), |_| 0.0f64);

        // Prefill only the EVEN-indexed rooms (under `&mut`) — the rest stay
        // deliberately un-prefilled, so this run exercises both a hit and a
        // miss for every one of the five readers.
        let mut memo = hornvale_kernel::RoomMeshMemo::new();
        let geo = ctx.climate().geosphere();
        let mut prefilled = 0usize;
        for (i, addr) in rooms.iter().enumerate() {
            if i % 2 == 0 {
                addr.corner_weights_memo(geo, ctx.nearest_index(), &mut memo);
                prefilled += 1;
            }
        }
        assert!(
            prefilled > 0 && prefilled < rooms.len(),
            "fixture must actually mix hits and misses"
        );

        // Count hits vs misses directly against the read-only lookup — the
        // review's own ask: prove the prefill actually covers the hot rooms,
        // not just that the byte-identity holds regardless.
        let mut hits = 0usize;
        let mut misses = 0usize;
        for addr in &rooms {
            match memo.corner_weights_lookup(addr) {
                Some(_) => hits += 1,
                None => misses += 1,
            }
        }
        assert_eq!(hits, prefilled, "every prefilled room must be a cache hit");
        assert_eq!(
            misses,
            rooms.len() - prefilled,
            "every un-prefilled room must be a cache miss"
        );

        // Now the actual byte-identity check, reading through the cache
        // (Some(&memo)) for every room — hits AND misses both included.
        for addr in &rooms {
            let expected_describe = ctx.describe_at(addr, at, None);
            let got_describe = ctx.describe_at_cached(addr, at, None, Some(&memo));
            assert_eq!(
                expected_describe.is_ok(),
                got_describe.is_ok(),
                "describe_at_cached Ok/Err mismatch at {addr:?}"
            );
            if let (Ok(exp), Ok(got)) = (expected_describe, got_describe) {
                assert_eq!(
                    serde_json::to_string(&exp).unwrap(),
                    serde_json::to_string(&got).unwrap(),
                    "describe_at_cached mismatch at {addr:?}"
                );
            }

            let expected_temp = ctx.temperature_at(addr, at);
            let got_temp = ctx.temperature_at_cached(addr, at, Some(&memo));
            assert_eq!(got_temp, expected_temp, "temperature_at_cached at {addr:?}");

            let expected_prod = ctx.productivity_at(addr);
            let got_prod = ctx.productivity_at_cached(addr, Some(&memo));
            assert_eq!(
                got_prod, expected_prod,
                "productivity_at_cached at {addr:?}"
            );

            let expected_blend = ctx.blend_at(addr, &zero_field);
            let got_blend = ctx.blend_at_cached(addr, &zero_field, Some(&memo));
            assert_eq!(got_blend, expected_blend, "blend_at_cached at {addr:?}");

            let expected_hazards = ctx.hazards_at(addr);
            let got_hazards = ctx.hazards_at_cached(addr, Some(&memo));
            assert_eq!(
                got_hazards, expected_hazards,
                "hazards_at_cached at {addr:?}"
            );
        }

        // `cache: None` must also be byte-identical (always a miss).
        let addr = &rooms[0];
        assert_eq!(
            ctx.describe_at(addr, at, None).is_ok(),
            ctx.describe_at_cached(addr, at, None, None).is_ok()
        );
        assert_eq!(
            ctx.temperature_at(addr, at),
            ctx.temperature_at_cached(addr, at, None)
        );
    }

    #[test]
    #[should_panic(expected = "RoomMeshMemo read against a geosphere at a different level")]
    fn cached_readers_assert_against_geo_level_aliasing_on_read() {
        // The-waymark fix round, round 2: `RoomMeshMemo`'s own
        // `debug_assert_eq!` (kernel/src/room.rs) already catches the geo-
        // aliasing footgun on the WRITE path (a memo fed from two different
        // globe levels). This proves the READ path catches it too — a memo
        // filled entirely against an INDEPENDENT geosphere (never touching
        // `ctx` at all) must still trip `corner_weights_for`'s own guard the
        // moment a `_cached` reader consults it against `ctx`'s real one.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let real_level = ctx.climate().geosphere().level();
        let other_level = real_level + 1;
        let fake_geo = hornvale_kernel::Geosphere::new(other_level);
        let fake_index = NearestCellIndex::new(&fake_geo);
        let mut memo = hornvale_kernel::RoomMeshMemo::new();
        let fake_addr = RoomAddr {
            face: 0,
            path: vec![0; other_level as usize],
        };
        fake_addr.corner_weights_memo(&fake_geo, &fake_index, &mut memo);

        let real_addr = RoomAddr {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let _ = ctx.temperature_at_cached(&real_addr, WorldTime { day: 0.0 }, Some(&memo));
    }
}
