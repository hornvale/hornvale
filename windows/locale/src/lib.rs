#![warn(missing_docs)]
//! The locale window: a `Facet` rendered as an observable place.

mod streams;
pub use streams::stream_labels;

mod regime;
pub use micro::{grounded_wetness, wetness_is_grounded};
// `#[doc(hidden)]` on the item itself; re-exported so the draw-order and
// scope-guard witnesses in `tests/suite/wetness_reading.rs` can call the
// function they are about. See its own doc for why a test seam beats the
// committed fixture it replaces.
pub use hornvale_climate::GroundKind;
#[doc(hidden)]
pub use micro::micro_field;
pub use regime::{EnergySource, Kingdom, MicroField, Negations, Regime};

mod substrate;

mod micro;

mod surface;
pub use surface::CoverClass;

mod grammar;

mod budget;
pub use budget::StrangeSite;
use budget::StrangenessBudget;

use hornvale_climate::{Biome, BiomeExpr, Formation, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{
    Facet, NearestVertexIndex, SeaLevelHeight, Seed, Vertex, World, WorldTime, band, quantize,
};
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::branch::{CatchmentCut, rill_reading};
pub use hornvale_terrain::channel::Transverse;
pub use hornvale_terrain::{CaveKind, WaterKind};
use hornvale_worldgen::{SiteReason, climate_from, site_facet_for, terrain_of};
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
/// type-audit: bare-ok(index: vertex), pending(wave-3: latitude), pending(wave-3: longitude), bare-ok(prose: biome), bare-ok(prose: descriptor)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct StrangeSiteRow {
    /// Canonical-grid vertex index — the site's IDENTITY, and what
    /// [`StrangeSiteRow::biome`] is read at. It is not where the site
    /// stands: see [`StrangeSiteRow::latitude`].
    ///
    /// **Wire name frozen at `cell`** (decision 0246): it is a key of
    /// `locale/room/v2`, present in every committed locale JSON and in
    /// `tests/fixtures/pre-stage-2-rooms.jsonl`.
    #[serde(rename = "cell")]
    pub vertex: u32,
    /// Site latitude, degrees (quantized) — of the PLACED FACET's own
    /// centroid, never of [`StrangeSiteRow::vertex`].
    ///
    /// **It reported the vertex's coordinate until The Prospect's Task 8,
    /// and that was a disagreement between two readouts of one site.** A
    /// site is warranted at a geosphere vertex and *stands* on the facet
    /// `hornvale_worldgen::site_facet_for` addresses — the one facet
    /// `hornvale_vessel`'s `brief_of` will let a walker enter, up to a
    /// placement quad away. Measured over seed 42's 103 sites, the vertex
    /// and the placed facet sit a mean 0.003086 rad apart and a maximum
    /// 0.006671 rad — 18.9 and 40.8 walk-facet edges respectively — so this
    /// column pointed a reader at open ground roughly twenty walk-band rooms
    /// from the only room the site is in. For a listing whose whole job is
    /// findability that is the failure it exists to remove.
    pub latitude: f64,
    /// Site longitude, degrees (quantized) — of the placed facet's own
    /// centroid. See [`StrangeSiteRow::latitude`].
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
    /// Packed room id (`FacetId.0`).
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
    /// Inherited biome name (max-weight corner vertex).
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
    /// The three canonical-grid corner vertices and their integer weights.
    pub corners: Vec<VertexWeight>,
    /// The strangeness overlay: descriptor, negation vector, and magnitude.
    pub regime: Regime,
    /// Base + vertical exits.
    pub exits: Vec<Exit>,
    /// The cave at the room's dominant corner (max-weight vertex), if the
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
    /// from the reach's discharge, gradient and local vertex spacing
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
    /// Which of this document's fields are decided at canonical-vertex
    /// resolution and which at channel resolution (decision 0123).
    pub resolution: Resolution,
}

/// What this document's fields are decided at, so a reader can tell a field
/// that is flat from a field that is broken (decision 0123).
///
/// **Why a room says this at all.** A room at walking depth sits seven
/// refinement levels below the canonical grid, so a field decided per grid
/// vertex is necessarily identical across all `4^7 = 16384` rooms in that vertex —
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
    /// a vertex, so `4^depth_below_grid` rooms share one grid vertex.
    pub depth_below_grid: u32,
    /// The names of this document's fields that are decided at canonical-grid
    /// resolution and therefore cannot vary below it, in stable order.
    ///
    /// Exactly `["biome", "cave", "fields.water"]` — the three categorical
    /// readings taken from the room's dominant corner vertex (see
    /// [`dominant_corner`]), which is one grid vertex and never a blend.
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
    ///   these are integer BILINEAR means of the FOUR corner vertices of the
    ///   room's grid-level ancestor quad, with per-room weights, so they
    ///   genuinely vary room by room. Listing them would be false. (Three
    ///   barycentric corners until The Pavement moved the base mesh to a
    ///   cube-sphere quad lattice — see [`Facet::corner_weights`].)
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
    /// the canonical vertex nor the room — in stable order.
    ///
    /// Exactly `["channel_bands"]`. The band edges are a per-vertex property
    /// of a reach (its discharge, gradient and local vertex spacing), so every
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

/// A canonical-grid corner vertex and its integer blend weight.
/// type-audit: bare-ok(index: vertex), bare-ok(count: weight)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct VertexWeight {
    /// Canonical-grid vertex index.
    ///
    /// **Wire name frozen at `cell`** (decision 0246): it is a key of
    /// `locale/room/v2`, present in every committed locale JSON and in
    /// `tests/fixtures/pre-stage-2-rooms.jsonl`.
    #[serde(rename = "cell")]
    pub vertex: u32,
    /// Integer weight (numerator over the summed denominator).
    pub weight: u64,
}

/// The blended continuous fields at the room centroid (bilinear weighted mean
/// of the FOUR corner vertices of the room's grid-level ancestor quad — three
/// barycentric corners before The Pavement; quantized at emit).
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
    /// [`dominant_corner`] is evaluated per ROOM, over that room's own four
    /// corner weights, so this is *categorical nearest-neighbour
    /// interpolation* — the correct method for a nominal field — and not a
    /// value copied down from one vertex to all 4^7 rooms inside it. The field
    /// does look flat across a narrow view, but that is the interpolation
    /// stencil being wider than the view rather than a defect in the field.
    ///
    /// **Do not refine it by thresholding a blend.** That was built and
    /// reverted: `WaterKind` is *nominal*, and a threshold is maximally
    /// nonlinear, so `classify(blend(drainage))` is not the area-weighted vote
    /// of `classify(drainage)` over the corners — it deletes the thin channels
    /// and shrank seed 42's fresh water at walking depth by 29%. Contrast
    /// `height_asl_m`'s relief bands, which may band a blend because relief is
    /// *ordinal*: a blend moves an ordinal value at most one band. Sub-vertex
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
    /// `FacetError` debug is carried. Fail fast rather than mint a
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
    index: NearestVertexIndex,
    globe_level: u32,
    budget: StrangenessBudget,
}

/// The corner vertex a room's *categorical* readings come from: the greatest
/// blend weight, tie-broken to the lowest `Vertex`.
///
/// One rule, one caller-visible consequence: every categorical field a room
/// reports — biome, water kind, substrate, and (since The Pigment) the rock
/// whose reflectance the colour layer reads — names the same vertex. Splitting
/// this would let a room be described as granite lowland and drawn in
/// basalt grey.
fn dominant_corner(weights: &[(Vertex, u64); 4]) -> (Vertex, u64) {
    let mut best = weights[0];
    for &cand in &weights[1..] {
        if cand.1 > best.1 || (cand.1 == best.1 && cand.0.0 < best.0.0) {
            best = cand;
        }
    }
    best
}

/// A room's *continuous* reading at its four corners: the integer-weighted
/// mean, quantized once at the emit boundary (decision 0033).
///
/// Extracted from `describe_with_weights`'s own `blend` closure so
/// [`LocaleContext::reflectance_at_facet`] can read the SAME moisture the
/// document emits without a second copy of the arithmetic — a second copy
/// being how the colour layer and the prose would come to disagree about how
/// wet a room is.
fn blend_at_corners(weights: &[(Vertex, u64); 4], value: &dyn Fn(Vertex) -> f64) -> f64 {
    let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
    let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
    quantize(sum / denom as f64)
}

/// What a step from one room to another does to the water between them.
///
/// **Fordability is a property of a path, never of a place** — a ford is a
/// sign change, and a sign needs two positions to change between. That is why
/// this is the answer to a question about a *pair* and there is no `fordable`
/// field on [`Locale`]: a room cannot be asked whether it can be crossed, only
/// whether a particular step out of it crosses water.
///
/// **The reading is a snapshot at fixed discharge.** The band edges a crossing
/// is judged against move with the flood, so `Fordable` is what is true of
/// today's world and not a standing fact about a place.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Crossing {
    /// The step does not cross a channel: the two rooms are on the same bank,
    /// either room reads nothing at all, or the sign does change but neither
    /// room stands inside the channel or bank of its own reading — where the
    /// sign is a fact about the polyline soup rather than about water.
    NotACrossing,
    /// The step crosses a channel narrow enough and quiet enough to wade:
    /// full width below one room edge at the pair's depth, and discharge below
    /// [`hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE`].
    Fordable,
    /// The step crosses a channel, and that channel is too wide or too strong
    /// to wade.
    Impassable,
}

/// The canonical walk depth: seven refinement levels below the canonical grid.
///
/// **THE ONE STATEMENT OF THIS ARITHMETIC IN THE REPOSITORY.** Every other
/// site — `hornvale_vessel::walk_depth` (a re-export, not a second
/// definition), `hornvale locale`'s and `hornvale surrounds`' `--depth`
/// defaults, the scene goldens' observer, every walk-band probe and fixture —
/// calls this function. `cli/tests/suite/walk_depth_agreement.rs` scans the
/// whole tree and FAILS if a `globe_level()` offset appears anywhere outside
/// this line, so a seventeenth copy cannot re-accrete quietly.
///
/// **Why it lives HERE rather than in `windows/vessel`, where it used to.**
/// The dependency chain is `locale -> scene -> vessel -> cli`, so a definition
/// in `vessel` was unreachable from three of the four crates that need it, and
/// each of them restated the arithmetic instead. The Pavement found sixteen
/// such restatements, two of them production `--depth` defaults that had
/// silently fallen a whole band behind. `walk_depth` takes a
/// [`LocaleContext`] and reads nothing but [`LocaleContext::globe_level`], so
/// this crate — the bottom of the chain and the owner of both — is where it
/// can actually be called from. Moving it is what makes "call it, do not
/// restate it" possible at all.
///
/// **Seven, not six, since The Pavement (spec section 2.3).** The offset is
/// chosen to preserve the length of one step, not for round numbers: on the
/// icosphere, depth 12 gave an effective 1.08 km centre-to-centre step
/// (measured 1.083/1.107 km alternating, seed 42); on the cube-sphere, depth
/// 12 is 2.251 km per side and depth 13 is 1.126 km, so 13 is the one that
/// keeps `windows/vessel`'s `clock.rs` authored 0.1-day `MoveTo` — and every
/// duration calibrated against it — honest. Depth 12 would have silently
/// doubled the ground covered per step.
/// type-audit: bare-ok(count: return)
pub fn walk_depth(ctx: &LocaleContext) -> u32 {
    ctx.globe_level() + 7
}

/// The geometry factor a **diagonal** walk-band step carries: one corner-
/// adjacent step spans `√2` times the ground an edge-adjacent one does, so it
/// costs `√2` more time and reaches `√2` further.
///
/// **This is the one definition** (decision 0515). `windows/vessel`'s
/// `clock::DIAGONAL_STEP_FACTOR` — decision 0508's, and the name every
/// movement-cost caller already uses — is now an alias of this constant rather
/// than a second copy of `SQRT_2`. It lives here because `windows/locale` is
/// the lower of the two crates that need it (`hornvale-vessel` depends on
/// `hornvale-locale`, never the reverse) and a window may not reach sideways.
///
/// It prices two different things with one number on purpose: the movement
/// clock's DURATION and [`LocaleContext::crossing_between`]'s REACH. The
/// justification is the same in both directions — the distance covered — and
/// splitting it into two tunable constants would be two numbers nothing
/// measures instead of one.
/// type-audit: bare-ok(ratio)
/// plumb: pending(wave-1)
pub const DIAGONAL_STEP_FACTOR: f64 = std::f64::consts::SQRT_2;

/// Whether `b` is a **corner**-adjacent neighbour of `a` rather than an
/// edge-adjacent one — the question [`DIAGONAL_STEP_FACTOR`] answers for, and
/// the one definition of it (decision 0515). `windows/vessel`'s
/// `clock::step_factor` and this crate's
/// [`LocaleContext::crossing_between`] both ask it, so that the clock and the
/// reach can never disagree about which steps are diagonal.
///
/// Derived from [`Facet::neighbors`]'s pinned edge-first prefix, and the
/// prefix LENGTH is derived too rather than written as a `4`: an edge step is
/// exactly a [`Facet::neighbor_steps`] entry with one zero component, so
/// counting them asks the kernel's own step table how long its edge prefix is.
/// That matters because `neighbors()` DROPS one step at a cube-corner room and
/// the dropped step is always a diagonal, so index-past-the-prefix survives
/// the drop while `neighbor_steps()[i]` would not.
///
/// A pair that is not adjacent at all — which no caller passes, since a
/// crossing is a step — is **not** a diagonal, so the stride stays orthogonal
/// and the function is total. Symmetric: adjacency on this lattice is.
///
/// A caller that needs to tell "not a step" from "an edge step" — the movement
/// clock does, to fail loudly on a move the mesh does not admit — asks
/// [`step_kind`] instead, which is where the work happens; this is the
/// two-valued view of it.
/// type-audit: bare-ok(flag: return)
pub fn is_diagonal_step(a: &Facet, b: &Facet) -> bool {
    step_kind(a, b) == Some(StepKind::Diagonal)
}

/// Which of the two geometries a walk-band step has — the distinction
/// [`DIAGONAL_STEP_FACTOR`] exists to price.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StepKind {
    /// `b` shares a SIDE with `a`: one lattice step, the orthogonal unit.
    Edge,
    /// `b` shares only a CORNER with `a`: [`DIAGONAL_STEP_FACTOR`] times the
    /// ground of an edge step.
    Diagonal,
}

/// The step geometry of `a` -> `b`, or `None` when `b` is not a neighbour of
/// `a` at all.
///
/// **This is the one place the edge-first prefix is indexed**, and the three
/// public answers ([`is_diagonal_step`], `hornvale_vessel`'s
/// `clock::step_factor`, and [`LocaleContext::crossing_between`] through the
/// first) are all views of this single walk of `a.neighbors()`. That matters
/// for more than tidiness: `neighbors()` ALLOCATES, and `step_factor` used to
/// walk it once for its own adjacency check and once more inside
/// `is_diagonal_step` — two allocations per movement charge, on the path
/// `Session::charge`'s `MoveTo` and the creature walk both take.
///
/// The three-valued return is what let those two collapse into one call: an
/// `Option` distinguishes "not a step at all" (which the clock fails loudly
/// on) from "an edge step" (which it charges the orthogonal unit for), where a
/// bare `bool` conflates them.
///
/// Derived from [`Facet::neighbors`]'s pinned edge-first prefix, and the prefix
/// LENGTH is derived too rather than written as a `4`: an edge step is exactly
/// a [`Facet::neighbor_steps`] entry with one zero component, so counting them
/// asks the kernel's own step table how long its edge prefix is. That matters
/// because `neighbors()` DROPS one step at a cube-corner room and the dropped
/// step is always a diagonal, so index-past-the-prefix survives the drop while
/// `neighbor_steps()[i]` would not.
pub fn step_kind(a: &Facet, b: &Facet) -> Option<StepKind> {
    let edge_prefix = Facet::neighbor_steps()
        .iter()
        .filter(|(dx, dy)| *dx == 0 || *dy == 0)
        .count();
    a.neighbors().iter().position(|n| n == b).map(|i| {
        if i >= edge_prefix {
            StepKind::Diagonal
        } else {
            StepKind::Edge
        }
    })
}

/// The shortest of a room's four edges, radians — the smallest step the mesh
/// offers out of it, and the unit "narrower than one step" is measured in.
///
/// Derived from [`Facet::corners`], so it is the mesh's own geometry at
/// whatever depth the room sits at; there is no length scale anywhere in this
/// project to state a width in, and inventing one would be a defect. The
/// *shortest* edge rather than the mean or the longest because the criterion
/// is a claim about crossability and the strictest of a room's steps is the
/// one that has to clear the water.
///
/// Four edges rather than three since The Pavement: a room is a cube-sphere
/// quad. The four are the room's own sides, in `Facet::corners`'s winding —
/// NOT its diagonals, which are longer and are not steps the lattice offers.
/// type-audit: pending(wave-1: return)
pub fn room_edge(addr: &Facet) -> f64 {
    let [a, b, c, d] = addr.corners();
    let sep = |u: [f64; 3], v: [f64; 3]| -> f64 {
        let dp: f64 = u[0] * v[0] + u[1] * v[1] + u[2] * v[2];
        hornvale_kernel::math::acos(dp.clamp(-1.0, 1.0))
    };
    sep(a, b).min(sep(b, c)).min(sep(c, d)).min(sep(d, a))
}

impl LocaleContext {
    /// Build the coarse world (climate + terrain + nearest-vertex index) once.
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
        let index = NearestVertexIndex::new(climate.geosphere());
        let globe_level = climate.geosphere().depth();
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

    /// The cached nearest-vertex index — the reuse seam for a caller that must
    /// resolve an address to a vertex itself (the same role `terrain()` plays for
    /// the terrain provider). Building a second index would duplicate a
    /// structure this context exists to hold once.
    pub fn nearest_index(&self) -> &NearestVertexIndex {
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
    ///
    /// **The coordinates are the PLACED FACET's, not the vertex's** (The
    /// Prospect, Task 8) — see [`StrangeSiteRow::latitude`] for the measured
    /// gap this closed. [`hornvale_worldgen::site_facet_for`] is the one
    /// authority on where a placed site stands, and this reads it rather
    /// than deriving a second answer: the same call
    /// `hornvale_vessel::brief`'s own enterability gate makes, with the same
    /// `(seed, vertex, reason)`, so the listing and the walker cannot
    /// disagree about which facet holds the site.
    ///
    /// The BIOME stays a per-vertex read, and deliberately: it is the base
    /// biome the site *interrupts*, a canonical-grid field with no finer
    /// resolution to offer, and reading it at the placed facet would be the
    /// resolution lie `crate::LocaleContext::describe`'s own grid-resolution
    /// list exists to keep visible.
    pub fn strange_site_rows(&self) -> Vec<StrangeSiteRow> {
        let geo = self.climate.geosphere();
        let walk = walk_depth(self);
        self.strange_sites()
            .into_iter()
            .map(|s| {
                let vertex = Vertex(s.vertex);
                let coord =
                    site_facet_for(vertex, SiteReason::Exotic, self.seed, geo, walk).coord();
                StrangeSiteRow {
                    vertex: s.vertex,
                    latitude: quantize(coord.latitude),
                    longitude: quantize(coord.longitude),
                    biome: biome_prose_name(self.climate.biome_at(vertex)).to_string(),
                    // `exotic_clause` reads only energy/kingdom/endemic, and a
                    // StrangeSite carries no substrate of its own (substrate is
                    // the ROOM's, from its derived regime), so `Ordinary` here
                    // is lossless rather than a stand-in.
                    descriptor: crate::grammar::exotic_clause(Negations {
                        substrate: GroundKind::Ordinary,
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
    pub fn describe(&self, addr: &Facet, at: WorldTime) -> Result<Locale, LocaleError> {
        self.describe_at(addr, at, None)
    }

    /// The reflectance of the rock underfoot at `addr`.
    ///
    /// A pure re-projection of the material buffer the terrain provider
    /// already holds — `material_at` and `rock_at` have been public all
    /// along, so this is an accessor, not a new derivation, and it stores
    /// nothing.
    ///
    /// The vertex is the same *categorical* corner [`LocaleContext::describe`]
    /// takes its biome and water kind from (max blend weight, tie-break
    /// lowest `Vertex` — the shared `dominant_corner`), never a blend of the
    /// four: rock class is categorical, and averaging granite with basalt
    /// would name a rock that is not there. Sharing that one rule is what
    /// makes the colour and the prose agree about which ground a room
    /// stands on.
    ///
    /// `micro` is the room's own sub-vertex [`MicroField`] (`describe`'s
    /// `Locale::regime.micro` on a `Locale` already built for this address,
    /// or [`crate::surface`]'s doc for why a *second* `describe` call is the
    /// wrong way to get one) and `at` is when to read the seasonal cover
    /// term at (spec §3.2) — see [`Self::reflectance_mixture_at`], which
    /// this integrates.
    pub fn reflectance_at(
        &self,
        addr: &Facet,
        micro: &MicroField,
        at: WorldTime,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        Ok(self.reflectance_mixture_at(addr, micro, at)?.integrate())
    }

    /// The ground's spectral curve at a facet, with the [`MicroField`]
    /// computed internally — the facet-level entry point a *view* needs.
    ///
    /// [`Self::reflectance_at`] takes the micro-field from its caller, which
    /// is right for a caller that already has one (`describe`'s `Locale`
    /// carries it) and impossible for one that does not: every input to the
    /// wetness grounding — the climate's moisture field, the channel network,
    /// the globe, the geosphere, the nearest-vertex index and the rill
    /// partition seed — is private to this context, so a caller outside this
    /// crate cannot build an equivalent `MicroField` and passing `None`
    /// would silently swap moisture-and-river-grounded wetness for address
    /// noise (`surface.rs` reads `micro.wetness` for the cover mixture's
    /// `wet_share`).
    ///
    /// The surface read, so the biome expression is the vertex's own
    /// ([`GeneratedClimate::biome_expr_at`]) rather than a stratum's —
    /// `describe_with_weights`'s `stratum: None` arm, which is the only arm
    /// a map of the ground has.
    ///
    /// `Err(LocaleError::AboveGrid)` for an address coarser than the grid:
    /// there is no four-corner reading to blend there, and a caller (the
    /// world map) is expected to fall back rather than treat it as a fault.
    pub fn reflectance_at_facet(
        &self,
        addr: &Facet,
        at: WorldTime,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        self.reflectance_at_facet_with_weights(addr, &weights, at)
    }

    /// [`Self::reflectance_at_facet`], consulting a caller-owned, READ-ONLY
    /// [`hornvale_kernel::RoomMeshMemo`] — the same base/`_cached` pair
    /// [`Self::describe_at_cached`], [`Self::temperature_at_cached`] and
    /// [`Self::hazards_at_cached`] already offer, for the same reason: a
    /// caller sweeping many addresses (the world map paints ~1,920 tiles a
    /// frame) has already resolved most of these corner weights and should
    /// not pay a second nearest-vertex search for them.
    ///
    /// A miss falls through to a fresh [`Facet::corner_weights`], so the
    /// answer is the cache's or it is the same computation — never a third
    /// thing. `cache: None` is exactly [`Self::reflectance_at_facet`].
    pub fn reflectance_at_facet_cached(
        &self,
        addr: &Facet,
        at: WorldTime,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = self
            .corner_weights_for(addr, geo, cache)
            .ok_or(LocaleError::AboveGrid)?;
        self.reflectance_at_facet_with_weights(addr, &weights, at)
    }

    /// The shared tail of [`Self::reflectance_at_facet`] and
    /// [`Self::reflectance_at_facet_cached`]: build the room's own
    /// [`MicroField`] from `weights`, then compose the mixture through
    /// [`Self::reflectance_mixture_with_weights`] — the ONE resolution of
    /// `weights`, threaded all the way down rather than resolved again at
    /// the composition.
    fn reflectance_at_facet_with_weights(
        &self,
        addr: &Facet,
        weights: &[(Vertex, u64); 4],
        at: WorldTime,
    ) -> Result<hornvale_kernel::color::Reflectance, LocaleError> {
        let best = dominant_corner(weights);
        let expr = self.climate.biome_expr_at(best.0);
        let moisture = blend_at_corners(weights, &|c| self.climate.moisture_at(c));
        let grounded = self.grounded_wetness_for(addr, expr, moisture);
        let micro = crate::micro::micro_field(addr.seed(self.seed), grounded);
        Ok(self
            .reflectance_mixture_with_weights(weights, &micro, at)?
            .integrate())
    }

    /// Wetness is a budget and an allocation (The Rill, R-7/R-8): the
    /// climate supply this room's vertices receive, redistributed by where the
    /// room sits relative to its own sub-vertex watercourse. Grounded only
    /// where the axis means ground wetness — at sea the same axis is the
    /// set of the current, on ice it is snow cover, and in the rock column
    /// it is seep, and a river's proximity governs none of those.
    ///
    /// Extracted verbatim from `describe_with_weights`, which still calls it,
    /// so [`Self::reflectance_at_facet`] grounds its wetness through the one
    /// derivation rather than a second copy of it. `moisture` is the caller's
    /// already-blended, already-quantized four-corner reading
    /// ([`blend_at_corners`]), never a raw vertex sample.
    fn grounded_wetness_for(&self, addr: &Facet, expr: BiomeExpr, moisture: f64) -> Option<f64> {
        crate::micro::wetness_is_grounded(expr).then(|| {
            let globe = self.terrain.globe();
            crate::micro::grounded_wetness(
                moisture,
                rill_reading(
                    addr.centroid(),
                    self.terrain.channels(),
                    globe,
                    self.terrain.geosphere(),
                    &self.index,
                    // `Drawn`, never `Even`: `Even` is R-5's falsification
                    // arm and is not a production partition.
                    &CatchmentCut::Drawn(globe.rill_partition_seed()),
                ),
            )
        })
    }

    /// The surface mixture at `addr` on `at`, un-integrated, so a caller can
    /// reach the components. [`LocaleContext::reflectance_at`] is this,
    /// integrated.
    ///
    /// Composes the mineral mixture [`hornvale_terrain::lithology::
    /// reflectance`] already produced with a surface cover layer
    /// ([`surface::cover_weights`]) weighted by the covered fraction, per
    /// spec §3.2: `mineral * (1 - covered) + cover`, integrated once. Takes
    /// `micro` from the caller rather than re-deriving it (which would mean
    /// either re-running the whole `describe`/`grammar::render` pipeline, or
    /// duplicating the wetness-grounding call into `hornvale_terrain::
    /// branch::rill_reading` a second time for the same room) — see
    /// [`surface`]'s module doc.
    pub fn reflectance_mixture_at(
        &self,
        addr: &Facet,
        micro: &MicroField,
        at: WorldTime,
    ) -> Result<hornvale_kernel::color::Mixture, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        self.reflectance_mixture_with_weights(&weights, micro, at)
    }

    /// The shared tail of [`Self::reflectance_mixture_at`] and
    /// [`Self::reflectance_at_facet`]: the composition itself, once `weights`
    /// is resolved — the same base/`_with_weights` split
    /// [`Self::temperature_with_weights`] and
    /// [`Self::productivity_with_weights`] already use.
    ///
    /// Extracted in Task 3's fix round because `reflectance_at_facet` must
    /// resolve `weights` ITSELF (it needs the dominant corner for the biome
    /// expression and the four corners for the moisture blend, both upstream
    /// of the `MicroField` it builds), and calling
    /// `reflectance_mixture_at` afterwards resolved them a second time — a
    /// nearest-vertex search per tile, paid twice, on the path the world map
    /// is about to run 1,920 times a frame.
    fn reflectance_mixture_with_weights(
        &self,
        weights: &[(Vertex, u64); 4],
        micro: &MicroField,
        at: WorldTime,
    ) -> Result<hornvale_kernel::color::Mixture, LocaleError> {
        let vertex = dominant_corner(weights).0;
        let buffer = self.terrain.material_at(vertex);
        let rock = self.terrain.rock_at(vertex);
        let mineral = hornvale_terrain::lithology::reflectance(&buffer, rock);
        // `(1.0 - covered)` below is only "the mineral's share of the
        // ground" if `mineral.weights()` already sums to `1.0` — but
        // `Mixture::weights()` is documented (`kernel/src/color.rs`) as
        // explicitly UNNORMALIZED, so `domains/terrain` is free to change
        // that sum without this crate noticing. Pinned here rather than
        // trusted: today it always sums to 1.0 algebraically (`lithology::
        // reflectance`'s four weights reduce to `silicate_share + carbonate
        // == 1.0` for any input — see `mineral_weights_sum_to_one_across_a_
        // buffer_spread` in this module's tests for the swept, non-debug
        // check), so a debug build catches a `domains/terrain` change here,
        // at the one call site that assumes it, rather than every colour in
        // the world silently shifting with nothing red anywhere (Task 2b
        // fix round, FINDING 2).
        let mineral_weight_sum: f64 = mineral.weights().iter().sum();
        debug_assert!(
            (mineral_weight_sum - 1.0).abs() < 1e-6,
            "lithology::reflectance's weights summed to {mineral_weight_sum}, not ~1.0 — \
             reflectance_mixture_at's `(1 - covered)` mineral scaling assumes a normalized \
             mineral mixture; domains/terrain changed its own weight convention and this \
             composition needs to change with it"
        );
        let cover = surface::cover_weights(&self.climate, vertex, micro, at);
        let covered: f64 = cover.iter().map(|(_, w)| w).sum();
        let mut components: Vec<hornvale_kernel::color::Reflectance> =
            mineral.components().to_vec();
        let mut mix_weights: Vec<f64> = mineral
            .weights()
            .iter()
            .map(|w| w * (1.0 - covered))
            .collect();
        for (r, w) in cover {
            components.push(r);
            mix_weights.push(w);
        }
        hornvale_kernel::color::Mixture::new(components, mix_weights)
            .map_err(|e| LocaleError::Build(e.to_string()))
    }

    /// The dominant surface cover class at `addr` on `at`, modulated by this
    /// room's own sub-vertex `micro` field — the categorical read
    /// [`surface::cover_class_at`] computes, resolved from the same
    /// dominant corner [`Self::reflectance_mixture_at`] uses, so a vertex's
    /// `cover` always names the ground its `color` was actually drawn from
    /// (Task 9).
    pub fn cover_class_at(
        &self,
        addr: &Facet,
        micro: &MicroField,
        at: WorldTime,
    ) -> Result<CoverClass, LocaleError> {
        let geo = self.climate.geosphere();
        let weights = addr
            .corner_weights(geo, &self.index)
            .ok_or(LocaleError::AboveGrid)?;
        let vertex = dominant_corner(&weights).0;
        Ok(surface::cover_class_at(&self.climate, vertex, micro, at))
    }

    /// The water column at a marine vertex: every stratum from the sunlit water
    /// down to the one the sea floor sits in, shallowest first. Empty on land.
    ///
    /// A vertex's floor decides how deep its water goes — 50 m of water over a
    /// reef holds only the epipelagic, while 3,000 m holds three layers. This
    /// is the list a diver descends.
    ///
    /// Delegates to [`GeneratedClimate::strata_at`] for the water case (the
    /// same take-the-ladder-up-to-the-floor derivation this method used to
    /// hand-roll — collapsed to one implementation, The Fathom, so the two
    /// could not silently diverge). The non-water guard stays here rather
    /// than moving into `strata_at`: this method answers "what water is
    /// there to descend through", so on land the answer is `Vec::new()`, not
    /// climate's `[Surface]` (its answer for a *land* vertex's own one-stratum
    /// ladder — a different question this method never asks).
    pub fn water_column_at(&self, vertex: Vertex) -> Vec<Stratum> {
        if self.climate.biome_expr_at(vertex).realm != Realm::WATERWORLD {
            return Vec::new();
        }
        self.climate.strata_at(vertex)
    }

    /// The biome expression at `vertex` as seen from `stratum`. At the sea floor
    /// this is the vertex's own community — a reef, a vent, a kelp forest. Above
    /// it there is only open water: the community lives on the floor, and
    /// floating a thousand metres over a reef is not being at the reef.
    ///
    /// Delegates to [`GeneratedClimate::biome_expr_at_stratum`] for every
    /// stratum on the vertex's own realm ladder at or above its floor (the
    /// in-column cases). **The fallback below is live, not dead code**: the
    /// stratum a caller passes here does not provably always resolve to a
    /// vertex whose column it is in-bounds for — `windows/vessel/src/
    /// session.rs`'s `column_here()` (the source of a possessed session's
    /// `submerged` stratum) picks its vertex via
    /// `corners.iter().max_by_key(|c| c.weight)`, which is Rust's
    /// last-element-wins tie-break, while this window's own
    /// `dominant_corner` (used by the two `describe_*` callers at `:763`/
    /// `:792` that ultimately reach this method) tie-breaks to the *lowest*
    /// `Vertex` — and `Facet::corner_weights` does not sort its four
    /// corners by id, so the two selections are not provably identical on an
    /// exact corner-weight tie. (Three corners before The Pavement; four
    /// makes an exact tie MORE likely, not less, so the divergence recorded
    /// here is if anything wider now.) **BELOW-FLOOR FALLBACK, PRESERVED VERBATIM
    /// AND KNOWN WRONG:** a rung beneath the seabed (or, on land, any
    /// stratum but `Surface`) is rock, and this answers open water. Kept
    /// byte-for-byte because The Fathom may not move behaviour; see
    /// followup F-10.
    pub fn expr_at_stratum(&self, vertex: Vertex, stratum: Stratum) -> BiomeExpr {
        let expr = self.climate.biome_expr_at(vertex);
        self.climate
            .biome_expr_at_stratum(vertex, stratum)
            .unwrap_or(BiomeExpr {
                realm: expr.realm,
                formation: Formation::OpenWater,
                stratum,
            })
    }

    /// [`LocaleContext::describe`], optionally as seen from a stratum within
    /// the water column rather than from the surface.
    pub fn describe_at(
        &self,
        addr: &Facet,
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
    /// [`Facet::corner_weights`] call — correctness never depends on the
    /// cache being complete, only speed does. `cache: None` is byte-identical
    /// to `describe_at` (always a miss). Byte-identical to `describe_at` on
    /// a hit too, by construction (`corner_weights_lookup` only ever returns
    /// what [`Facet::corner_weights_memo`] would have inserted, which is
    /// pinned bit-equal to `corner_weights` itself). The same `corner_weights`
    /// result [`Self::temperature_at_cached`], [`Self::productivity_at_cached`],
    /// [`Self::blend_at_cached`], and [`Self::hazards_at_cached`] would each
    /// independently recompute for the SAME room in one read scope (e.g.
    /// `windows/vessel`'s per-tick drive stack), a caller that shares one
    /// cache across all five collapses that back down to one scan.
    pub fn describe_at_cached(
        &self,
        addr: &Facet,
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
    /// [`Facet::corner_weights`] call. No mutation — this never fills a
    /// miss back into `cache`, which is exactly what lets a `&self` reader
    /// use it without `&mut` access.
    fn corner_weights_for(
        &self,
        addr: &Facet,
        geo: &hornvale_kernel::Geosphere,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<[(Vertex, u64); 4]> {
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
                    .is_none_or(|level| level == geo.depth()),
                "RoomMeshMemo read against a geosphere at a different level than it was \
                 filled with — a Facet alone does not name which (Geosphere, \
                 NearestVertexIndex) resolved it, so reading a cache built for a different \
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
        addr: &Facet,
        stratum: Option<Stratum>,
        id: u64,
        weights: [(Vertex, u64); 4],
    ) -> Result<Locale, LocaleError> {
        // Categorical biome: max weight, tie-break lowest Vertex. Inherited,
        // never re-quantized (decision 0038).
        let best = dominant_corner(&weights);
        let biome = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st).biome(),
            None => self.climate.biome_at(best.0),
        };

        // Continuous fields: integer-weighted mean, full precision, quantize
        // at emit. The arithmetic itself is `blend_at_corners`, shared with
        // `reflectance_at_facet` so the moisture that grounds a wetness
        // reading there is the SAME number this document emits.
        let blend = |value: &dyn Fn(Vertex) -> f64| -> f64 { blend_at_corners(&weights, value) };
        let elevation_m = blend(&|c| self.terrain.globe().elevation.get(c).get());
        // `from_metres`, not a subtraction: the left operand is a four-corner
        // BLEND, not any single vertex's reading, so there is no pair of
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
        let expr = match stratum {
            Some(st) => self.expr_at_stratum(best.0, st),
            None => self.climate.biome_expr_at(best.0),
        };
        let grounded = self.grounded_wetness_for(addr, expr, fields.moisture);
        let micro = crate::micro::micro_field(addr.seed(self.seed), grounded);
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
                .map(|&(c, w)| VertexWeight {
                    vertex: c.0,
                    weight: w,
                })
                .collect(),
            regime,                // strangeness overlay (§5-§7)
            exits: exits_of(addr), // base + vertical exits (§6)
            cave: self.terrain.cave_at(best.0).map(|c| c.kind),
            channel_distance: reading.map(|r| r.signed_distance),
            channel_bands: reading.map(|r| r.band_edges),
            resolution: Resolution {
                grid_level: self.globe_level,
                // Non-negative by construction: `corner_weights` returned
                // `Some`, which it only does when `depth >= geo.depth()`.
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

    /// Whether the step from room `a` to room `b` crosses a channel, and if so
    /// whether it can be waded.
    ///
    /// # What makes a crossing
    ///
    /// **All three clauses, or it is not a crossing at all.**
    ///
    /// 1. The two rooms' readings are **of the same channel** — the same
    ///    polyline won [`hornvale_terrain::channel::ChannelNetwork::nearest_line`]
    ///    for both.
    /// 2. Their signed channel distances have **opposite signs** — one on the
    ///    left bank facing downstream, one on the right.
    /// 3. At least one of them stands **inside its own bank edge**
    ///    (`|d| < channel_bands[1]`, i.e. reads `Channel` or `Bank`).
    ///
    /// Clause 1 exists because the two readings are selected *independently*:
    /// a room nearest river X and a room nearest river Y each get a sign in
    /// that river's own frame, and the two frames have nothing to do with each
    /// other. Left-of-X beside right-of-Y satisfies clause 2 while the
    /// comparison that produced it is **uninterpretable**, and
    /// [`hornvale_terrain::channel::BankReading`] would then price the step
    /// against whichever reach happened to win. Confluences are where such
    /// pairs concentrate. The line index is the only thing that can tell them
    /// apart — which is why the reading carries it, as an in-process handle
    /// that is **never serialized**.
    ///
    /// **What clause 1 refuses, and what it costs.** It establishes that the
    /// two signs cannot be compared — *not* that no water lies between the
    /// rooms. So it has a false-negative side, at exactly the locus where
    /// cross-line pairs concentrate: a real crossing whose two rooms happen to
    /// select a tributary and its trunk is refused. The trade is taken
    /// knowingly. Pricing an uncomparable pair is a wrong answer stated
    /// confidently; refusing it is a missed crossing at a known and nameable
    /// locus, and a reading cannot distinguish the two on its own, since it
    /// knows only its own winning line. A confluence-aware query would need
    /// `ChannelNetwork::run_vertices`, which states the join topology outright —
    /// loosening this clause is not the way to it.
    ///
    /// Clause 3 is not belt-and-braces, and dropping it was a real draft of
    /// this design. The signed distance is measured against many *open arcs*,
    /// so its sign also flips beyond every river's source and mouth and along
    /// the bisector between two arcs that meet — on dry ground, about the
    /// polyline soup rather than about water. On seed 42 at level 5 that is
    /// **25 spurious flips against 3 real crossings**. Two measured properties
    /// of that locus decide the shape of clause 3:
    ///
    /// - **It is a ray, not a place.** Probing at radii 1.0e-2, 5.0e-3 and
    ///   3.1e-3 rad finds the same flips each time, at whatever `|d|` the probe
    ///   stands at. **No fixed distance threshold removes it** — only asking
    ///   whether the reading is inside its *own* bands does, since those scale
    ///   with the reach.
    /// - **Not-`Dry` is too generous.** The confluence-bisector flip sits at
    ///   `|d| = 4.7946e-3` against a widest terrace edge of 6.9e-3, so it is
    ///   *inside* the terrace. The gate is `Channel`-or-`Bank`, deliberately.
    ///
    /// # What makes it fordable (spec §8, a late freeze)
    ///
    /// Of the readings that clause 3 made interpretable — the ones inside
    /// their own bank edge — **every** one must satisfy both:
    ///
    /// - its channel's **full** width, twice `channel_bands[0]` (which is the
    ///   *half*-width), is less than **the stride the pair actually offers**;
    ///   and
    /// - that reach's discharge is below
    ///   [`hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE`].
    ///
    /// The unit is the traversal unit rather than the water: a room edge is the
    /// granularity at which a person moves, so "narrower than one step" is what
    /// crossing means to the thing doing the crossing — and it is expressible
    /// without a length scale, which no quantity in this project has.
    ///
    /// # THE STRIDE CARRIES THE DIAGONAL FACTOR (decision 0515)
    ///
    /// The stride is [`room_edge`] — the smaller of the two rooms' — times
    /// [`DIAGONAL_STEP_FACTOR`] when `a` and `b` are corner-adjacent rather
    /// than edge-adjacent. Until fix round 1 it was the bare room edge, so a
    /// diagonal step was judged against the same channel width an orthogonal
    /// one was even though it spans `√2` more ground. The movement clock has
    /// priced that factor since decision 0508; **the reach must use the same
    /// one, because the reason a diagonal costs `√2` more is that it covers
    /// `√2` more distance**, and a rule that charges for the distance while
    /// refusing to credit the reach is charging twice.
    ///
    /// The direction is **monotone**: the stride only ever grows, so a verdict
    /// can move `Impassable` -> `Fordable` and never the reverse. Nothing
    /// crossable before this change is uncrossable after it.
    ///
    /// **`√2` is a consistent simplification, not exact geometry, and the
    /// record says so.** Channel width is measured PERPENDICULAR to the
    /// channel, and the channel's bearing is arbitrary relative to the
    /// lattice — so a diagonal step is not reliably more oblique to a stream
    /// than an orthogonal one is. The exact model would divide the stride by
    /// the sine of the angle between the step and the channel, which needs a
    /// channel bearing this clause does not consult and
    /// [`hornvale_terrain::channel::BankReading`] does not carry. Decision
    /// 0515 accepts the uniform factor for the same reason 0508 did: one
    /// number, applied everywhere a diagonal is priced, is a simplification a
    /// reader can hold, and a per-crossing trigonometric correction is a
    /// tuning surface with no measurement behind it.
    ///
    /// Both the width and the discharge come from
    /// [`hornvale_terrain::channel::BankReading`], so they describe the same
    /// reach the distance was measured to and the same vertex the bands came
    /// from. Symmetric in its arguments: swapping `a` and `b` swaps a pair of
    /// symmetric tests and nothing else — [`is_diagonal_step`] is symmetric
    /// too, because adjacency on this lattice is.
    pub fn crossing_between(&self, a: &Facet, b: &Facet) -> Crossing {
        let net = self.terrain.channels();
        let (Some(ra), Some(rb)) = (
            net.bank_reading(a.centroid()),
            net.bank_reading(b.centroid()),
        ) else {
            return Crossing::NotACrossing;
        };
        // Clause 1. The two readings were selected independently, so they may
        // be about different rivers — in which case their signs live in
        // different frames and comparing them is meaningless. Asked first
        // because everything below reads the pair as one channel's geometry.
        if ra.line != rb.line {
            return Crossing::NotACrossing;
        }
        // Written as two explicit comparisons rather than a product, so a
        // reading of exactly 0.0 (a room centroid on the centreline) is neither
        // side rather than silently taking the sign of a signed zero.
        let sign_differs = (ra.signed_distance > 0.0 && rb.signed_distance < 0.0)
            || (ra.signed_distance < 0.0 && rb.signed_distance > 0.0);
        if !sign_differs {
            return Crossing::NotACrossing;
        }
        let interpretable =
            |r: &hornvale_terrain::channel::BankReading| r.signed_distance.abs() < r.band_edges[1];
        if !interpretable(&ra) && !interpretable(&rb) {
            return Crossing::NotACrossing;
        }
        let step = room_edge(a).min(room_edge(b))
            * if is_diagonal_step(a, b) {
                DIAGONAL_STEP_FACTOR
            } else {
                1.0
            };
        let wadeable = |r: &hornvale_terrain::channel::BankReading| {
            2.0 * r.band_edges[0] < step
                && self.terrain.drainage_at(r.vertex)
                    < hornvale_terrain::carve::WATERFALL_MIN_DRAINAGE
        };
        if [ra, rb].iter().filter(|r| interpretable(r)).all(wadeable) {
            Crossing::Fordable
        } else {
            Crossing::Impassable
        }
    }

    /// The room's transverse ordinal and its signed channel distance — the
    /// **function** half of this stage's keystone.
    ///
    /// The document stores the measured quantity (`channel_distance`) and the
    /// legend for reading it (`channel_bands`); it deliberately does not store
    /// a band, because a band is one consumer's classification. This is the
    /// convenience that recovers the classification anyway, so that "the
    /// ordinal is a function, not a field" names something callable rather
    /// than something merely describable.
    ///
    /// It is a *convenience*, not a second opinion. The reading comes from the
    /// same single [`hornvale_terrain::channel::ChannelNetwork::bank_reading`]
    /// selection [`describe`](Self::describe) emits and
    /// [`crossing_between`](Self::crossing_between) gates on, and the banding
    /// is [`hornvale_kernel::band`] over that reading's own edges — the same
    /// pair a consumer recomputes from the serialized document. A second
    /// derivation here would be the duplicate-selection defect this stage
    /// spent its review closing.
    ///
    /// Full precision, not quantized: this is a compute-path read, never a
    /// serialization boundary. A consumer banding the *document* works from
    /// eight significant digits and may therefore disagree with this within
    /// quantization of a band edge — which is a fact about the emit boundary,
    /// and `the_band_recomputes_from_the_stored_distance_and_edges` is where
    /// it is checked.
    ///
    /// `None` only on a world whose channel network is empty — there is no
    /// bank to be on, and `Dry` would be an answer about water rather than the
    /// absence of any.
    /// type-audit: pending(wave-1: return)
    pub fn transverse_of(&self, addr: &Facet) -> Option<(Transverse, f64)> {
        let reading = self.terrain.channels().bank_reading(addr.centroid())?;
        Some((
            Transverse::from_band(band(reading.signed_distance, &reading.band_edges)),
            reading.signed_distance,
        ))
    }

    /// The room's PER-DAY temperature at `at`, °C — the diurnal+seasonal
    /// signal a thermal drive senses at its own vertex, distinct from
    /// [`describe`](Self::describe)'s annual-MEAN `temperature_c` render field
    /// (left untouched, so the walk/almanac stay byte-identical). Blends the
    /// four corner vertices' [`GeneratedClimate::temperature_at`] by the SAME
    /// integer BILINEAR weights `describe` uses for the mean (three
    /// barycentric corners before The Pavement — see
    /// [`Facet::corner_weights`]). Full
    /// precision — this is a compute-path read, never a serialization
    /// boundary, so it is NOT quantized (quantize-at-emit-only). `None` for a
    /// room the canonical grid does not cover (above the grid or unaddressable);
    /// the caller supplies the never-chosen fallback.
    /// type-audit: pending(wave-2: return)
    pub fn temperature_at(&self, addr: &Facet, at: WorldTime) -> Option<f64> {
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
        addr: &Facet,
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
    fn temperature_with_weights(&self, at: WorldTime, weights: [(Vertex, u64); 4]) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights
            .iter()
            .map(|&(c, w)| w as f64 * self.climate.temperature_at(c, at).get())
            .sum();
        sum / denom as f64
    }

    /// The room's material food PRODUCTIVITY in `[0, 1]` — a Miami-model
    /// net-primary-productivity proxy over the climate, the food-value field
    /// the drive layer's hunger drive reads (The Provender). Blends the four
    /// corner vertices' annual-mean temperature and moisture by the SAME integer
    /// BILINEAR weights [`describe`](Self::describe) uses, then takes the
    /// Liebig minimum of a triangular temperature response and moisture — the
    /// same NPP proxy demography's carrying-capacity uses, computed here from
    /// this context's own climate rather than depending up into demography (a
    /// sibling consumer, not required to match it bit-for-bit; it grades vertices
    /// for a hungry forager, it does not set population). Full precision — a
    /// compute-path read, never a serialization boundary, so NOT quantized.
    /// `None` for a room the canonical grid does not cover (the caller supplies
    /// the never-fed fallback). Time-independent (standing biomass is a slow,
    /// annual field), so it takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn productivity_at(&self, addr: &Facet) -> Option<f64> {
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
        addr: &Facet,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<f64> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(self.productivity_with_weights(weights))
    }

    /// The shared tail of [`Self::productivity_at`]/[`Self::productivity_at_cached`]
    /// (the-waymark fix round, round 2).
    fn productivity_with_weights(&self, weights: [(Vertex, u64); 4]) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let blend = |value: &dyn Fn(Vertex) -> f64| -> f64 {
            let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * value(c)).sum();
            sum / denom as f64
        };
        let temp = blend(&|c| self.climate.mean_temperature_at(c).get());
        let moisture = blend(&|c| self.climate.moisture_at(c));
        miami_npp(temp, moisture)
    }

    /// Corner-blend an externally-supplied per-vertex `field` (over the canonical
    /// geosphere) at `addr` — the integer four-corner bilinear read
    /// `productivity_at`/`hazards_at` use, generalized so a caller can sample
    /// a field this context
    /// does not itself hold. The Quarry injects `worldgen::predator_pressure_from`
    /// (the carnivore-pressure field) and reads it here per room. Full precision
    /// (a compute-path read, not quantized). `None` for a room the canonical grid
    /// does not cover.
    /// type-audit: bare-ok(ratio: field), bare-ok(ratio: return)
    pub fn blend_at(&self, addr: &Facet, field: &hornvale_kernel::VertexMap<f64>) -> Option<f64> {
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
        addr: &Facet,
        field: &hornvale_kernel::VertexMap<f64>,
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
        weights: [(Vertex, u64); 4],
        field: &hornvale_kernel::VertexMap<f64>,
    ) -> f64 {
        let denom: u64 = weights.iter().map(|&(_, w)| w).sum();
        let sum: f64 = weights.iter().map(|&(c, w)| w as f64 * *field.get(c)).sum();
        sum / denom as f64
    }

    /// The room's THREAT in `[0, 1]` — the hazard field the danger drive flees
    /// (The Dread, split per-axis by The Bane) as `(uncanny, heat, cold)`, each
    /// in `[0, 1]`: the **uncanny** (a placed exotic site's normalized strangeness
    /// — the "cursed ground"), and **heat**/**cold** — how far the vertex's
    /// annual-mean temperature is *above* a hot-danger threshold / *below* a
    /// cold-danger one, graded up to the lethal extreme (the deep ice, the molten
    /// waste). Reads the dominant corner vertex's placed regime (like
    /// [`describe`](Self::describe) picks its biome) and a corner-blended mean
    /// temperature. Full precision — a compute-path read, never a serialization
    /// boundary, so NOT quantized. `None` for a room the canonical grid does not
    /// cover (the caller supplies the safe fallback). Time-independent, so it
    /// takes no observation time.
    /// type-audit: pending(wave-2: return)
    pub fn hazards_at(&self, addr: &Facet) -> Option<(f64, f64, f64)> {
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
        addr: &Facet,
        cache: Option<&hornvale_kernel::RoomMeshMemo>,
    ) -> Option<(f64, f64, f64)> {
        let geo = self.climate.geosphere();
        let weights = self.corner_weights_for(addr, geo, cache)?;
        Some(self.hazards_with_weights(weights))
    }

    /// The shared tail of [`Self::hazards_at`]/[`Self::hazards_at_cached`] (the-waymark
    /// fix round, round 2).
    fn hazards_with_weights(&self, weights: [(Vertex, u64); 4]) -> (f64, f64, f64) {
        // The dominant corner vertex (max weight, tie-break lowest Vertex) — the
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

/// The annual-mean temperature (°C) at/below which a vertex's COLD becomes a hazard
/// (The Bane) — graded from here down to [`LETHAL_COLD_C`]. Above the coldest
/// species niche, so ordinary cold is thermal discomfort (thermal's job), not
/// dread.
/// plumb: pending(wave-1)
const COLD_DANGER_C: f64 = -20.0;

/// The annual-mean temperature (°C) at/above which a vertex's HEAT becomes a hazard
/// (The Bane) — graded from here up to [`LETHAL_HEAT_C`].
/// plumb: pending(wave-1)
const HOT_DANGER_C: f64 = 40.0;

/// The coldest annual-mean temperature (°C) any creature survives — a lethal
/// frozen waste, where COLD hazard saturates to `1` (The Bane).
/// plumb: pending(wave-1)
const LETHAL_COLD_C: f64 = -40.0;

/// The hottest annual-mean temperature (°C) any creature survives — a lethal
/// molten waste, where HEAT hazard saturates to `1`.
/// plumb: pending(wave-1)
const LETHAL_HEAT_C: f64 = 60.0;

/// The optimum temperature (°C) of the Miami NPP proxy's triangular
/// temperature response — mirrors demography's carrying-capacity model (a
/// sibling consumer of the same proxy; see [`LocaleContext::productivity_at`]).
/// plumb: pending(wave-1)
const NPP_TEMP_OPTIMUM_C: f64 = 20.0;

/// The temperature tolerance (°C) either side of [`NPP_TEMP_OPTIMUM_C`] over
/// which the triangular temperature response falls to zero.
/// plumb: pending(wave-1)
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

    /// The canonical bearing this word names, degrees clockwise from north.
    ///
    /// Exhaustive by construction: adding a [`Compass`] variant fails to
    /// compile here until it is given a bearing, the same discipline
    /// [`compass_variants_must_all_be_rostered`] holds below.
    ///
    /// Moved down from `windows/vessel`'s `session::bearing_of` by The
    /// Pavement's Task 11, because [`heading_rose`] needs it and
    /// `hornvale-vessel` depends on this crate rather than the other way
    /// round. Vessel now calls this instead of holding a second copy.
    /// `diagnostic-value` rather than a newtype, following
    /// `windows/scene/src/surrounds.rs`'s own `bearing_deg` field, which is
    /// the same quantity at the same boundary and carries the same class:
    /// this is one of eight constants keyed on a closed enum, not a measured
    /// angle crossing a domain boundary, so a unit newtype here would wrap a
    /// literal per variant.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn bearing_deg(self) -> f64 {
        match self {
            Compass::N => 0.0,
            Compass::Ne => 45.0,
            Compass::E => 90.0,
            Compass::Se => 135.0,
            Compass::S => 180.0,
            Compass::Sw => 225.0,
            Compass::W => 270.0,
            Compass::Nw => 315.0,
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

/// The smaller of the two ways round between two bearings, in degrees:
/// `0..=180`.
///
/// Integer-free but transcendental-free too — a remainder and a comparison, no
/// `atan2` — so it adds nothing to the determinism surface. Moved down from
/// `windows/vessel`'s `session::bearing_gap_deg` with [`heading_rose`].
fn bearing_gap_deg(a: f64, b: f64) -> f64 {
    let d = ((a - b) % 360.0 + 360.0) % 360.0;
    if d > 180.0 { 360.0 - d } else { d }
}

/// The whole compass rose resolved at once: for each [`Compass::all`] word, in
/// that order, which of `from`'s neighbours it names — or `None` when that
/// bearing has no neighbour at all, which happens at exactly the 24 cube-corner
/// rooms (8 cube corners, three quads meeting at each) and for exactly one
/// bearing there.
///
/// # THE ONE RULE, and why it lives here rather than in `windows/vessel`
///
/// This is the *only* rule in the project that turns a room's neighbours into
/// compass words. It used to be two: `windows/vessel`'s `heading_rose` (which
/// `go` reads) and this crate's own `compass()`, which bucketed each bearing
/// into its own 45-degree sector and is what `look` printed. **On the cube mesh
/// those two disagreed, visibly, inside a single sentence.** `describe_here`
/// builds its letter list from `Locale::exits` and its "closed" clause from the
/// rose, so at a measured **10.0% of walk-band rooms** it printed something like
///
/// > *No direction here is closed; the nearest ground lies E, NW, W, SE, N, W,
/// > S, E.*
///
/// — E and W twice, NE and SW never — while `go ne` and `go sw` both worked.
/// The geography is sharp and is a seam effect, not noise: 4.2% on the four
/// equatorial faces against ~21.5% on faces 4 and 5, and within a face 19% of
/// the outermost ring, ~1% one ring in, 0% beyond. It is not "a duplicated
/// letter and a missing one" either — every sampled face-4 room had **two**
/// duplicated and **two** missing.
///
/// The Pavement's Task 11 deleted the bucket rule outright and moved the
/// assignment DOWN into this crate, rather than the reverse: `hornvale-vessel`
/// already depends on `hornvale-locale`, so a shared function can only live at
/// this level or lower. Between the two candidates — here beside [`Compass`],
/// or in the kernel on `Facet` — this crate wins because the kernel does not
/// know what a compass word is, and putting the rose there would mean moving
/// `Compass` down with it, dragging a presentation vocabulary into the
/// determinism substrate to serve one caller.
///
/// # Why an assignment, and not "the neighbour nearest that bearing"
///
/// Nearest-by-bearing never refuses: at a corner room, two compass words would
/// silently resolve to the same neighbour, which is the aliasing the walk band
/// says out loud instead. Bucketing each neighbour into its own 45-degree
/// sector refuses too MUCH — that is the 10.0% measured above, because a quad's
/// diagonals are not 45 degrees off its edges once the projection distorts
/// them. A gap threshold cannot separate the two cases either: the corner's own
/// missing direction sits 30 degrees from a real neighbour while an interior
/// bearing can sit 25 degrees from its nearest, and tuning a constant into that
/// 5-degree window is exactly the kind of number this project refuses to
/// author.
///
/// So the rule is a **one-to-one assignment** between the eight compass words
/// and the neighbours actually present. The candidate graph is complete —
/// every neighbour has an angular error to every word — so an assignment of
/// size `neighbors().len()` always exists, whatever the objective. That is the
/// structural half of the guarantee, and it is unchanged:
///
/// > **eight neighbours leaves no word unmatched; seven leaves exactly one.**
///
/// # THE OBJECTIVE IS THE OTHER HALF, AND ITS FIRST VERSION HAD NONE
///
/// Task 11 shipped a **greedy** matching: sort all `8 x neighbours` pairs by
/// error and take each pair whose word and neighbour are both still free.
/// Greedy guarantees cardinality and bounds *nothing*. It spends the good
/// pairs first and hands the last word whatever neighbour is left, so where
/// the room's local rose is rotated against true north — the normal condition
/// on a cube-sphere, not an edge case — the residue is arbitrary. Measured at
/// seed-42 room `FacetId(2169509120)` (face 0, face-lattice `(910, 0)`, walk
/// depth 13), reproduced through the shipped CLI: `go E` walked **west and
/// north**, 156.1 degrees off the word it names, and `look` agreed with it —
/// the prose and the movement told the same falsehood. Over a uniform sample
/// of 12,696 walk-depth rooms, 5.96% of rooms carried a word more than 45
/// degrees off and 0.82% more than 90. That is decision 0141's
/// "one-turn observable falsehood" exactly, and the bucket rule greedy
/// replaced was at most 22.5 degrees wrong *by construction*.
///
/// So the objective is now stated, and it is **lexicographic min-max**:
/// among all assignments, take the one whose errors, sorted DESCENDING,
/// compare smallest. That minimises the worst word's error first, then the
/// second worst, and so on — the right objective for a promise about the
/// worst case, which is what a compass word is. At the room above it achieves
/// 25.6 degrees where greedy achieved 156.1.
///
/// # HOW, and why exactly this algorithm
///
/// A subset dynamic program over the eight words: `best[mask]` is the best key
/// for having placed the first `mask.count_ones()` neighbours into exactly the
/// words in `mask`. It is exact because the key is monotone under extension —
/// inserting the same error into two descending-sorted multisets preserves
/// their lexicographic order — so an optimal whole assignment has an optimal
/// prefix for its own mask, which is the exchange argument a DP needs.
///
/// 256 masks times 8 words is 2,048 transitions of a bounded 8-element insert
/// and compare. **Measured 11.6 us per call** (release, 12,696 walk-depth
/// rooms, this campaign's Mac) against **5.9 us** for the greedy rule it
/// replaces — the same measurement, same rooms, and both figures include the
/// eight `bearing_to` calls neither rule avoids. [`exits_of`] calls it once
/// per `look`, so +5.7 us is the whole cost of the fix.
///
/// Two alternatives were measured rather than argued about. **Min-SUM** over
/// the same DP costs 7.0 us and reaches a worst error of 35.82 degrees against
/// this rule's 34.58 — cheaper, and worse at the one thing being bounded, so
/// the objective is the min-max one. **Hungarian** is asymptotically cheaper
/// than either, but it solves min-sum, so reaching min-max through it needs a
/// threshold search wrapped around a matching; at `n = 8` this DP is exact for
/// the objective actually wanted in a third of the code.
///
/// # DETERMINISM, and the tie-break
///
/// No map, no set, no iteration-order dependence: a fixed-size array indexed
/// by word mask, walked in increasing numeric order. Every float comparison is
/// `total_cmp`. Two neighbours CAN be exactly equidistant in bearing, so the
/// key carries an explicit second component: **among assignments whose sorted
/// error vectors are equal, the one whose word indices, read in
/// `neighbors()` order, are lexicographically smallest wins.** That resolves
/// every tie by index rather than by visitation order.
///
/// The bearing is **not** quantized. It was, until fix round 1: the stated
/// purpose was "where the deleted bucket rule's own cross-platform stability
/// came from", which decision 0041 had already answered — every
/// transcendental in [`Facet::bearing_to`] routes through the pure-Rust
/// `libm`, so the compute path is bit-identical without it. Quantization is an
/// emit-boundary instrument (decision 0033) and this is a compute path that
/// decides where the player moves; rounding to 8 significant digits here could
/// only ever make two distinct gaps compare equal and hand the pair to the
/// tie-break. Verified to change no assignment at any room sampled before it
/// was removed.
///
/// # WHAT BOUNDS THE ERROR NOW
///
/// `the_worst_compass_word_is_within_the_measured_ceiling` — the assertion
/// whose absence let greedy ship. Read its doc for the measured numbers, the
/// two populations they come from, and what it is blind to.
pub fn heading_rose(from: &Facet) -> Vec<Option<Facet>> {
    let words = Compass::all();
    let ns = from.neighbors();
    // `cost[i][w]` — the bearing error, in degrees, of naming neighbour `i`
    // with word `w`. Square and fixed-size: `ns.len()` is 8 in the interior
    // and 7 at a cube corner, never more, and the unused rows are simply not
    // visited.
    let mut cost = [[0.0f64; ROSE_WORDS]; ROSE_WORDS];
    debug_assert!(
        ns.len() <= ROSE_WORDS,
        "heading_rose was handed {} neighbours; the cube-sphere lattice offers \
         at most {ROSE_WORDS}, so the cost matrix would silently truncate",
        ns.len()
    );
    for (i, n) in ns.iter().enumerate().take(ROSE_WORDS) {
        let b = from.bearing_to(n);
        for (w, &c) in words.iter().enumerate() {
            cost[i][w] = bearing_gap_deg(b, c.bearing_deg());
        }
    }
    let placed = ns.len().min(ROSE_WORDS);
    let assignment = rose_assignment(&cost, placed);
    let mut out: Vec<Option<Facet>> = vec![None; words.len()];
    for (i, &w) in assignment.iter().enumerate().take(placed) {
        out[w as usize] = Some(ns[i].clone());
    }
    out
}

/// The number of compass words, hence the width of [`heading_rose`]'s
/// assignment problem and the bound on a room's lateral arity.
/// plumb: pending(wave-1)
const ROSE_WORDS: usize = 8;

/// One partial assignment, and the key [`rose_assignment`] minimises.
///
/// `errs[..len]` holds the errors placed so far, sorted **descending**;
/// `words[..len]` holds the word chosen for each neighbour, in `neighbors()`
/// order. Comparison is lexicographic on the first, then on the second — see
/// [`heading_rose`]'s determinism note for why the second component exists.
#[derive(Clone, Copy)]
struct RoseKey {
    /// The errors placed so far, sorted descending; only `..len` is meaningful.
    errs: [f64; ROSE_WORDS],
    /// The word index chosen for each placed neighbour, in `neighbors()` order.
    words: [u8; ROSE_WORDS],
    /// How many neighbours this key has placed.
    len: usize,
}

impl RoseKey {
    /// The empty assignment.
    /// plumb: pending(wave-1)
    const EMPTY: Self = Self {
        errs: [0.0; ROSE_WORDS],
        words: [0; ROSE_WORDS],
        len: 0,
    };

    /// This key with neighbour `len` placed at `word` for `err` degrees of
    /// error — the error inserted so `errs[..len + 1]` stays descending.
    fn extend(&self, err: f64, word: u8) -> Self {
        let mut errs = self.errs;
        let mut k = self.len;
        while k > 0 && errs[k - 1].total_cmp(&err) == core::cmp::Ordering::Less {
            errs[k] = errs[k - 1];
            k -= 1;
        }
        errs[k] = err;
        let mut words = self.words;
        words[self.len] = word;
        Self {
            errs,
            words,
            len: self.len + 1,
        }
    }

    /// Lexicographic order on the descending error vector, then on the word
    /// indices. Only ever called on two keys of equal `len`.
    fn rank(&self, other: &Self) -> core::cmp::Ordering {
        for k in 0..self.len.min(other.len) {
            let o = self.errs[k].total_cmp(&other.errs[k]);
            if o != core::cmp::Ordering::Equal {
                return o;
            }
        }
        self.words[..self.len].cmp(&other.words[..other.len])
    }
}

/// The lexicographic min-max assignment of `placed` neighbours to distinct
/// compass words, as `word_of[i]` for neighbour `i`.
///
/// Exact, by the subset DP [`heading_rose`] documents. `placed` is `0..=8`; a
/// `placed` of 0 yields a key nothing reads.
fn rose_assignment(cost: &[[f64; ROSE_WORDS]; ROSE_WORDS], placed: usize) -> [u8; ROSE_WORDS] {
    let mut best: [Option<RoseKey>; 1 << ROSE_WORDS] = [None; 1 << ROSE_WORDS];
    best[0] = Some(RoseKey::EMPTY);
    // Indexed by mask rather than iterated: the body READS `best[mask]` and
    // WRITES `best[mask | bit]`, which no single iterator can express. Masks
    // rise monotonically and `mask | bit > mask`, so a written slot is never
    // read again as a source — that ordering is what makes the DP a single
    // forward pass instead of a relaxation loop.
    #[allow(clippy::needless_range_loop)]
    for mask in 0..(1usize << ROSE_WORDS) {
        let i = (mask as u32).count_ones() as usize;
        if i >= placed {
            continue;
        }
        let Some(cur) = best[mask] else { continue };
        for (w, &err) in cost[i].iter().enumerate() {
            if mask & (1 << w) != 0 {
                continue;
            }
            let cand = cur.extend(err, w as u8);
            let next = mask | (1 << w);
            let better = match &best[next] {
                None => true,
                Some(held) => cand.rank(held) == core::cmp::Ordering::Less,
            };
            if better {
                best[next] = Some(cand);
            }
        }
    }
    let mut winner = RoseKey::EMPTY;
    let mut found = false;
    for (mask, slot) in best.iter().enumerate() {
        if (mask as u32).count_ones() as usize != placed {
            continue;
        }
        let Some(key) = slot else { continue };
        if !found || key.rank(&winner) == core::cmp::Ordering::Less {
            winner = *key;
            found = true;
        }
    }
    winner.words
}

/// A room's exits: one lateral edge per compass word [`heading_rose`] assigns,
/// in [`Compass::all`] order, then the vertical pair.
///
/// **Compass order, not `neighbors()` order, since The Pavement's Task 11** —
/// the list is now indexed by the word rather than by the mesh's own winding,
/// which is what makes it the same assignment `go` resolves against. The
/// lateral count is unchanged (one exit per neighbour; a cube-corner room's
/// seven neighbours still yield seven exits, with one word simply unassigned).
fn exits_of(addr: &Facet) -> Vec<Exit> {
    let mut exits = Vec::new();
    for (word, n) in Compass::all().into_iter().zip(heading_rose(addr)) {
        let Some(n) = n else { continue };
        exits.push(Exit {
            direction: Direction::Compass(word),
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
    use hornvale_kernel::{Facet, Seed, World, WorldTime};

    fn land_world() -> World {
        // Seed 42 is the project's canonical fixture; it has land.
        World::new(Seed(42))
    }

    /// FINDING 2 (Task 2b fix round): `reflectance_mixture_at` scales
    /// `lithology::reflectance(..).weights()` by `(1 - covered)` and
    /// appends absolute cover weights, which composes correctly only if
    /// the mineral weights already sum to `1.0` — but `Mixture::weights()`
    /// is documented as explicitly unnormalized, so nothing outside
    /// `domains/terrain` enforces that sum. This is the swept check the
    /// composition site's own `debug_assert!` comment points to: real
    /// generated terrain, several seeds, many vertices, not hand-built
    /// buffers — so a change to `lithology::reflectance`'s weight formula
    /// (not just an out-of-range input) is what this is watching for.
    ///
    /// claim: invariant(forall-seed) — the mineral-weights-sum-to-one
    /// property is asserted for every (seed, vertex) pair swept, not sampled
    /// to find one instance of it.
    #[allow(clippy::disallowed_methods)] // named construction site (decision 0092)
    #[test]
    fn mineral_weights_sum_to_one_across_a_buffer_spread() {
        let mut checked = 0;
        for seed in [1u64, 42, 7, 100] {
            let world = World::new(Seed(seed));
            let terrain = terrain_of(&world).expect("terrain sculpts");
            let geo = terrain.geosphere();
            for i in (0..geo.vertex_count() as u32).step_by(53) {
                let vertex = Vertex(i);
                let buffer = terrain.material_at(vertex);
                let rock = terrain.rock_at(vertex);
                let mineral = hornvale_terrain::lithology::reflectance(&buffer, rock);
                let sum: f64 = mineral.weights().iter().sum();
                assert!(
                    (sum - 1.0).abs() < 1e-6,
                    "seed {seed} vertex {vertex:?}: mineral weights summed to {sum}, not ~1.0"
                );
                checked += 1;
            }
        }
        assert!(
            checked > 500,
            "too few (seed, vertex) pairs swept to trust this check; got {checked}"
        );
    }

    /// The first grid-level facet in `ctx` whose surface biome expression
    /// grounds its wetness axis, walking the globe's own vertices in order.
    /// Searched rather than hardcoded: an address literal would silently
    /// stop naming a grounded room the next time the mesh or the biome fit
    /// moves, and the test would then pass by testing nothing.
    fn a_grounded_facet(ctx: &LocaleContext) -> (Facet, [(Vertex, u64); 4]) {
        let geo = ctx.climate.geosphere();
        let depth = ctx.globe_level();
        for i in 0..geo.vertex_count() as u32 {
            let addr = Facet::containing(geo.position(Vertex(i)), depth);
            let Some(weights) = addr.corner_weights(geo, &ctx.index) else {
                continue;
            };
            let expr = ctx.climate.biome_expr_at(dominant_corner(&weights).0);
            if crate::micro::wetness_is_grounded(expr) {
                return (addr, weights);
            }
        }
        panic!("seed 42 has no facet whose wetness axis is ground wetness");
    }

    /// The first biome expression on the globe whose wetness axis is NOT
    /// ground wetness — sea current or snow cover. Searched for the same
    /// reason [`a_grounded_facet`] is.
    fn an_ungrounded_expr(ctx: &LocaleContext) -> BiomeExpr {
        let geo = ctx.climate.geosphere();
        for i in 0..geo.vertex_count() as u32 {
            let expr = ctx.climate.biome_expr_at(Vertex(i));
            if !crate::micro::wetness_is_grounded(expr) {
                return expr;
            }
        }
        panic!("seed 42 has no vertex whose wetness axis is not ground wetness");
    }

    /// FIRES WHEN: `grounded_wetness_for` stops being the climate moisture
    /// supply redistributed by the room's own watercourse — dropped,
    /// scaled, or resolved against a different catchment partition.
    ///
    /// **Why this test exists at the UNIT tier specifically** (Task 3 fix
    /// round, Finding 1). The extraction that created this helper was
    /// covered only by `tests/suite/wetness_reading.rs`, and a 0.5x scaling
    /// of `moisture` inside it was caught by exactly ONE assertion there —
    /// an integration test absent from `docs/timings/subfloor-roster.tsv`.
    /// All 57 of this crate's unit tests stayed green under that mutation,
    /// so `make gate-commit` would have passed a corrupted grounding, on the
    /// function the world map is about to call for every tile it paints.
    ///
    /// Three clauses, because no one of them discriminates alone:
    ///
    /// 1. On a grounded expression the helper returns `Some`, and the value
    ///    is EXACTLY `grounded_wetness(moisture, rill_reading(…))`
    ///    recomposed here from the crate's own published pieces. This is the
    ///    clause a scaling, a swapped operand or a `CatchmentCut::Even`
    ///    fails — the composition is restated on purpose, because a private
    ///    helper's contract is what it composes.
    /// 2. That value differs from the ungrounded reading the SAME address
    ///    would take (`micro_field(seed, None)`, which is address noise), so
    ///    clause 1 is not pinning a no-op and a dropped grounding reddens.
    /// 3. On a non-grounded expression the helper returns `None` — the
    ///    predicate half, which is what keeps sea current and snow cover out
    ///    of a reading about rivers.
    #[test]
    fn the_grounded_wetness_is_the_moisture_redistributed_by_the_watercourse() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let (addr, weights) = a_grounded_facet(&ctx);
        let expr = ctx.climate.biome_expr_at(dominant_corner(&weights).0);
        let moisture = blend_at_corners(&weights, &|c| ctx.climate.moisture_at(c));

        let got = ctx
            .grounded_wetness_for(&addr, expr, moisture)
            .expect("clause 1: a grounded expression must ground its wetness");

        // Clause 1: the composition, restated from the published pieces.
        let globe = ctx.terrain.globe();
        let want = crate::micro::grounded_wetness(
            moisture,
            rill_reading(
                addr.centroid(),
                ctx.terrain.channels(),
                globe,
                ctx.terrain.geosphere(),
                &ctx.index,
                &CatchmentCut::Drawn(globe.rill_partition_seed()),
            ),
        );
        assert_eq!(
            got, want,
            "grounded_wetness_for is no longer the climate moisture ({moisture}) \
             redistributed by this room's own drawn-catchment rill reading"
        );

        // Clause 2: NON-VACUITY — the grounding actually moves the wetness
        // this room reads, so clause 1 is not pinning a value the ungrounded
        // path would have produced anyway.
        let room_seed = addr.seed(ctx.seed);
        let grounded_field = crate::micro::micro_field(room_seed, Some(got));
        let ungrounded_field = crate::micro::micro_field(room_seed, None);
        assert_ne!(
            grounded_field.wetness, ungrounded_field.wetness,
            "the grounded and ungrounded wetness agree at this address, so clause 1 \
             would pass with the grounding removed"
        );

        // Clause 3: the predicate half. A biome whose wetness axis is not
        // ground wetness grounds nothing, whatever the moisture reads —
        // asserted at the SAME address, so the only thing that varies
        // between this call and the one above is the expression.
        let ungrounded_expr = an_ungrounded_expr(&ctx);
        assert_eq!(
            ctx.grounded_wetness_for(&addr, ungrounded_expr, moisture),
            None,
            "a biome whose wetness axis is not ground wetness must ground nothing"
        );
    }

    /// [`LocaleContext::reflectance_at_facet_cached`] with a prefilled memo
    /// must return the identical curve the uncached path does. The cache is
    /// a nearest-vertex search skipped, never a different answer, and this
    /// repository's byte-identity guarantee is the reason that has to be
    /// asserted rather than assumed — no generated artifact exercises this
    /// path, so `make rebaseline` cannot see it.
    ///
    /// **This is the cheap half, on purpose.** The exhaustive check — every
    /// cached reader, over a walked neighbourhood, on cache HITS and cache
    /// MISSES both — is
    /// `every_cached_reader_bit_equals_its_recomputing_sibling_with_a_partial_prefill`
    /// below, which costs several seconds and so sits above the sub-floor
    /// duration threshold `make gate-commit` selects on. This one is a hit
    /// and a `None` at one address in 0.39 s, which is what makes it
    /// eligible for the commit gate at all.
    #[test]
    fn the_memoized_reflectance_is_the_uncached_one() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let (addr, _) = a_grounded_facet(&ctx);
        let at = WorldTime::GENESIS;

        let mut memo = hornvale_kernel::RoomMeshMemo::default();
        let geo = ctx.climate.geosphere();
        addr.corner_weights_memo(geo, &ctx.index, &mut memo)
            .expect("a grid-level facet resolves");

        let plain = ctx.reflectance_at_facet(&addr, at).unwrap();
        let cached = ctx
            .reflectance_at_facet_cached(&addr, at, Some(&memo))
            .unwrap();
        assert_eq!(plain.get(), cached.get());

        // And `cache: None` is the base method exactly.
        let no_cache = ctx.reflectance_at_facet_cached(&addr, at, None).unwrap();
        assert_eq!(plain.get(), no_cache.get());
    }

    #[test]
    fn describe_is_deterministic_across_two_contexts() {
        let world = land_world();
        let addr = Facet {
            face: 0,
            path: vec![1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0],
        };
        let a = LocaleContext::build(&world).unwrap();
        let b = LocaleContext::build(&world).unwrap();
        let la = a.describe(&addr, WorldTime::GENESIS).unwrap();
        let lb = b.describe(&addr, WorldTime::GENESIS).unwrap();
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
        let coarse = Facet {
            face: 0,
            path: vec![1],
        };
        assert!(matches!(
            ctx.describe(&coarse, WorldTime::GENESIS),
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
                "vertex {} is placed as exotic but reads as nothing",
                r.vertex
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
        let addr = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
        // elevation blends three real vertices; the value must be finite.
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
        let addr = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
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
            let addr = Facet::containing(dir, 6);
            if let Ok(loc) = ctx.describe(&addr, WorldTime::GENESIS) {
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

    /// The Grain, Task 3: a room's `cave` must name the same vertex `biome`
    /// and `water` do — the dominant corner, never a blend.
    ///
    /// A direct aim at a cave vertex's own canonical-grid position does not
    /// reliably land that vertex as the resolved address's dominant corner
    /// (the room mesh's nearest-vertex resolution does not coincide with the
    /// geosphere's own vertex centroids closely enough to guarantee it), so
    /// this uses the same directional-sweep idiom as
    /// `describe_and_reflectance_agree_on_one_dominant_vertex` and
    /// `locale_water_field_varies_and_includes_fresh_water_on_seed_42`:
    /// scan a deterministic spread of directions, and for each resolved
    /// address's ACTUAL dominant corner (not a guess), check whether the
    /// terrain places a cave there. Seed 42 has 628 of 11 066 land vertices
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
            let addr = Facet::containing(dir, ctx.globe_level());
            let Some(weights) = addr.corner_weights(geo, &ctx.index) else {
                continue;
            };
            let dominant = dominant_corner(&weights).0;
            let Some(cave) = terrain.cave_at(dominant) else {
                continue;
            };
            let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
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
        let over_deep = Facet {
            face: 0,
            path: vec![0; 30],
        };
        assert!(matches!(
            ctx.describe(&over_deep, WorldTime::GENESIS),
            Err(LocaleError::Unaddressable(_))
        ));
    }

    #[test]
    fn blend_and_inheritance_pin_exact_values() {
        // §14 Q4 regression: pin the blend/inheritance for a fixed seed-42
        // world at a fixed deep address. Values captured from a known-good run.
        // We pin the platform-EXACT quantities only: the quantized blended
        // temperature (byte-identical cross-platform) and the corner
        // (vertex, weight) pairs (pure integer bilinear numerators — the
        // inheritance-selection inputs; barycentric until The Pavement). The biome NAME is a depth-band
        // classification thresholded on host-libm transcendentals (elevation +
        // a percentile sea_level), i.e. the cross-platform-divergence class CI
        // excludes elsewhere — so we assert membership, not the exact string,
        // to keep the both-platform workspace gate stable.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
        // THE GLASSHOUSE, Stage B Tasks 4/5 re-pin: 38.082618 -> 37.232618.
        // The thermostat (a damped, greenhouse-forced insolation baseline
        // replacing the fixed 288 K blackbody one) plus the area-mean-zero
        // latitude profile move this address's blended temperature; the
        // corners below (then pure barycentric geometry over unchanged
        // terrain) were untouched, confirming only the climate field moved.
        // That reasoning is HISTORICAL — see The Pavement's note below, which
        // is the campaign where the corners themselves moved.
        //
        // THE GLASSHOUSE close re-pin (`k` settled at 0.30): 37.232618 ->
        // 23.999847. `k` moved after the Task 4/5 re-pin above, and this is a
        // 13.2 K fall at one address on a campaign whose population MEDIAN
        // rose 8.3 K — not a contradiction, and worth a line so the next
        // reader does not treat it as one. This address sits at face 3 down a
        // fixed deep path; the corrected latitude profile is area-mean-zero
        // where the old one carried a uniform +10 K, so it redistributes
        // rather than lifts, and a low-latitude address that was reading the
        // old offset loses more than the thermostat returns. The corners are
        // again untouched, which is what says the climate field moved and the
        // geometry did not.
        //
        // THE PAVEMENT re-pin (Task 3): 23.999847 -> 25.154255, and the
        // corners move from THREE to FOUR with entirely new vertex ids.
        // **Both halves of the note above are now out of date and the reason
        // is the point.** Every earlier re-pin here said "the corners are
        // untouched, which is what says the climate field moved and the
        // geometry did not". This campaign moved the geometry: the base mesh
        // is a tangent-warped cube-sphere, so a room at this address is a
        // different patch of the world (Task 2 — different corner positions,
        // hence different nearest grid vertices), and `corner_weights` is a
        // four-corner BILINEAR stencil rather than a three-corner barycentric
        // one (Task 3 — see `Facet::corner_weights`). The numerators sum to
        // `D = 4 << (2 * (depth - globe_level))` = `4^7` = 16384 at this
        // address, replacing the old `3 << (depth - globe_level)` = 192.
        //
        // So the temperature moved for a reason no climate change can
        // account for, and a reader must NOT read this row as a climate
        // result: it is the same climate field sampled at a different place
        // through a different stencil.
        assert_eq!(loc.fields.temperature_c, 25.154255);
        assert_eq!(
            loc.corners,
            vec![
                VertexWeight {
                    vertex: 36922,
                    weight: 2125
                },
                VertexWeight {
                    vertex: 29318,
                    weight: 8755
                },
                VertexWeight {
                    vertex: 29323,
                    weight: 4429
                },
                VertexWeight {
                    vertex: 29322,
                    weight: 1075
                },
            ]
        );
        // The denominator is a stated contract, not an incidental sum.
        assert_eq!(
            loc.corners.iter().map(|c| c.weight).sum::<u64>(),
            4 << (2 * (addr.depth() - ctx.globe_level)),
            "the four bilinear numerators must sum to D"
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
    /// the rock the colour layer reads -- names the same vertex. Nothing
    /// checked that before this test, and a reverted campaign (`dd523ab2`,
    /// reverted at `76068e6a`) split it silently: it stayed green through
    /// 3350 tests while water alone moved to a non-dominant reading.
    ///
    /// Sampled over many addresses (the same directional-spread idiom
    /// `locale_water_field_varies_and_includes_fresh_water_on_seed_42` uses)
    /// rather than one fixed address: a single room's four corner weights
    /// can coincidentally agree across categories even when the underlying
    /// wiring has split, so one address is not enough to trust a pass.
    ///
    /// **The final block is narrowed, not deleted, by the illumination
    /// campaign's Task 2b.** `reflectance_at`/`reflectance_mixture_at` now
    /// compose a surface cover layer above the mineral mixture
    /// (`surface::cover_weights`), so `reflectance_at(addr)` no longer
    /// equals the bare mineral reflectance of the dominant vertex whenever
    /// anything covers it — legitimately: a forested vertex reads green now,
    /// not granite-grey, and that is the whole point of this campaign. The
    /// exact byte-identity assertion this test used to make (colour layer's
    /// rock == recomputed mineral reflectance) still holds, but only where
    /// `covered == 0.0` — an uncovered vertex (open water, unfrozen, since
    /// `on_land` gates vegetation/sand-silt and freezing gates snow) has no
    /// cover layer to disturb it. `bare_checked` is the positive control
    /// that this narrowed subset is not empty (see the memory note "an
    /// empty diff needs a positive control" — a vacuously-true narrowed
    /// assertion would be worse than no assertion at all).
    #[test]
    fn describe_and_reflectance_agree_on_one_dominant_vertex() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let geo = ctx.climate.geosphere();
        let mut checked = 0;
        let mut bare_checked = 0;
        for i in 0..200u32 {
            let t = i as f64;
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = Facet::containing(dir, 6);
            let Some(weights) = addr.corner_weights(geo, &ctx.index) else {
                continue;
            };
            let expected_vertex = dominant_corner(&weights).0;
            let locale = ctx.describe(&addr, WorldTime::GENESIS).unwrap();

            assert_eq!(
                locale.biome_kind,
                ctx.climate.biome_at(expected_vertex),
                "biome must name the dominant corner at {addr:?}"
            );
            assert_eq!(
                locale.fields.water,
                *ctx.terrain.globe().water_kind.get(expected_vertex),
                "water kind must name the dominant corner at {addr:?}"
            );
            assert_eq!(
                locale.regime.negations.substrate,
                crate::substrate::substrate_at(&ctx.climate, &ctx.terrain, expected_vertex),
                "substrate must name the dominant corner at {addr:?}"
            );

            let micro = locale.regime.micro;
            let reflectance = ctx
                .reflectance_at(&addr, &micro, WorldTime::GENESIS)
                .unwrap();
            let buffer = ctx.terrain.material_at(expected_vertex);
            let rock = ctx.terrain.rock_at(expected_vertex);
            let expected_reflectance =
                hornvale_terrain::lithology::reflectance(&buffer, rock).integrate();
            let cover = crate::surface::cover_weights(
                &ctx.climate,
                expected_vertex,
                &micro,
                WorldTime::GENESIS,
            );
            let covered: f64 = cover.iter().map(|(_, w)| w).sum();
            if covered == 0.0 {
                assert_eq!(
                    reflectance, expected_reflectance,
                    "the colour layer's rock must name the dominant corner at {addr:?} \
                     (uncovered — bare mineral ground)"
                );
                bare_checked += 1;
            }
            checked += 1;
        }
        assert!(
            checked > 50,
            "too few addresses resolved on the grid to trust this test"
        );
        assert!(
            bare_checked > 0,
            "no uncovered address was sampled — the covered == 0.0 narrowing would be vacuous"
        );
    }

    /// The room's dominant corner, read back off a rendered [`Locale`] through
    /// the SAME rule `describe` used to pick it — [`dominant_corner`]: max
    /// weight, tie-break lowest `Vertex`.
    ///
    /// **Not `max_by_key(|c| c.weight)`**, which returns the LAST maximum on a
    /// tie. Three equal weights are common enough on this mesh that the two
    /// rules disagree in practice (four weights now, since The Pavement made
    /// the stencil a quad), and a test that used `max_by_key` compared
    /// against a vertex production never chose — passing for a reason unrelated
    /// to what it claimed to measure. (Two paths in `windows/vessel` still
    /// resolve a vertex that way; that divergence is recorded and unfixed.)
    fn dominant_of(loc: &Locale) -> Vertex {
        let w: [(Vertex, u64); 4] = [
            (Vertex(loc.corners[0].vertex), loc.corners[0].weight),
            (Vertex(loc.corners[1].vertex), loc.corners[1].weight),
            (Vertex(loc.corners[2].vertex), loc.corners[2].weight),
            (Vertex(loc.corners[3].vertex), loc.corners[3].weight),
        ];
        dominant_corner(&w).0
    }

    /// Rooms spread across the WHOLE of `vertex`'s dual region, at `depth` — one
    /// fan of samples running from near the vertex's centre out towards each of
    /// its neighbours.
    ///
    /// **Not a contiguous BFS neighbourhood, and the difference is the whole
    /// point.** A conservation claim is about a vertex, and a radius-4 patch
    /// covers about 1/132 of one; across a patch that small the corner blend
    /// of a terrain statistic moves ~2%, so a patch cannot see the variation
    /// that a vertex-wide aggregate must account for. Fanning outward instead
    /// sweeps the neighbour's blend weight across the range that actually
    /// exists inside the vertex.
    ///
    /// **THE TWO PARAGRAPHS BELOW ARE HISTORICAL AND THEIR PREMISE IS GONE.**
    /// They reason about an interpolant of THREE barycentric corners over a
    /// room mesh that subdivides the same icosphere as the vertices, with a
    /// spherical point-in-triangle descent. The Pavement retired all three
    /// premises: the base mesh is a tangent-warped cube-sphere quad lattice,
    /// [`Facet::corner_weights`] is a FOUR-corner bilinear stencil, and
    /// [`Facet::containing`] is a dyadic bisection of two face parameters with
    /// no triangle test and no middle-child fallback. The lattice-coincidence
    /// hazard they describe was real and the fixture still avoids it, so they
    /// are kept as the reason this helper is shaped the way it is — but the
    /// specific numbers (`1/3`, `64/64/64`, the ~5° miss) describe a mesh that
    /// no longer exists, and nobody should re-derive anything from them.
    ///
    /// **No sample is a vertex centre, and none lies on the arc between two of
    /// them.** Rooms and vertices subdivide the *same* icosphere, so a level-6 vertex
    /// centre is also an exact corner of the level-12 room lattice, and the arc
    /// between two adjacent centres is an exact edge path of it.
    /// `Facet::containing`'s spherical point-in-triangle test straddles on
    /// both: the descent falls through to its middle-child fallback at every
    /// level and converges on the centre of a base-face sub-triangle — measured
    /// ~5° from the point asked for, with all three corner weights equal
    /// (64/64/64), so even the dominant corner is a coin toss there. A water
    /// test built on such a room **passed against code that did not yet do what
    /// it claimed.** Hence `t` never reaches 0, and every sample carries a
    /// small off-lattice third component (`SKEW`) to leave the arc. `containing`
    /// is sound for ordinary points; it is lattice coincidences that degenerate.
    fn rooms_across_vertex(
        geo: &hornvale_kernel::Geosphere,
        vertex: Vertex,
        depth: u32,
    ) -> Vec<Facet> {
        /// Off-lattice third component. Small enough not to move which vertex
        /// owns the sample, large enough to leave the arc.
        const SKEW: f64 = 0.031;
        let a = geo.position(vertex);
        let ns = geo.neighbors(vertex);
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
                out.push(Facet::containing(
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
    /// vertex's water is, told only what its rooms report.
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

    /// How many canonical vertices the conservation scan below covers, in each of
    /// its two halves. Capped because the scan pays a `describe` per room.
    ///
    /// The scan takes two samples deliberately. A **stride** over `Vertex`
    /// order spans the globe, so conservation is asserted over ocean and dry
    /// land as well as rivers. A **River prefix** targets the one category the
    /// reverted mechanism actually deleted: a stride sample is ~94% ocean and
    /// dry land, where a threshold on a blend agrees with the partition almost
    /// everywhere, so a stride alone would leave the tripwire arm unable to
    /// fire for a reason that has nothing to do with the criterion.
    const CONSERVATION_VERTICES: usize = 40;

    /// **H5 (The Grain) — the conservation criterion, and the only test in this
    /// file written for a mechanism that does not exist yet.**
    ///
    /// The claim: aggregating room-level water back over a canonical vertex
    /// reproduces that vertex's own water kind. Nearest-corner assignment
    /// satisfies it *by construction* — every room whose dominant corner is
    /// vertex `C` reports `C`'s water kind, so the aggregate is unanimous — which
    /// is precisely why this test is cheap and precisely why it is worth
    /// having. It is a **tripwire for a future mechanism** (`MAP-64`'s flow
    /// graph, or anything else that tries to put water somewhere in particular
    /// inside a vertex), not a discovery about today's code.
    ///
    /// Why write down something that holds trivially: a campaign built a
    /// sub-vertex water mechanism that passed both of its preregistered
    /// hypotheses and the whole 3350-test suite, and it was illegal — it
    /// deleted 29% of seed 42's fresh water at walking depth. Both hypotheses
    /// asked about *local variation*; the violated property was *global
    /// conservation*, and no local hypothesis can detect that. This is the test
    /// that mechanism would have failed before it was ever built.
    ///
    /// **It asserts the strong form (unanimity), not merely the aggregate.**
    /// A plurality-only criterion would still permit deleting a category from a
    /// minority of vertices, which is exactly the 29% loss — a thin river is a
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
    /// **Measured on seed 42** over 80 vertices and 2151 rooms: the partition
    /// conserves on every one, while the reverted mechanism breaks the aggregate
    /// form on **11 of 80 vertices** and unanimity on **27 of 80**. The first draft
    /// of this test sampled a radius-4 BFS patch per vertex and the tripwire arm
    /// found **0 of 44** — across 1/132 of a vertex the blend barely moves, so the
    /// scan could not see what it was built to catch. That near miss is why the
    /// sampling is a vertex-wide fan and why the second arm exists at all: a
    /// tripwire nobody has watched trip is a comment.
    #[test]
    fn room_water_is_conserved_when_aggregated_over_a_canonical_vertex() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let geo = ctx.climate().geosphere();
        let globe = ctx.terrain().globe();
        // The walk band, via the crate's own `walk_depth` rather than a
        // restated offset. NOTE for whoever re-measures the locale suite after
        // The Pavement's epoch: the populations quoted in this test's doc
        // ("2151 rooms", "11 of 80", "27 of 80") were measured at
        // `globe_level + 6` on the icosphere, so they are stale twice over —
        // the band moved one level (4x the rooms per vertex) and the mesh
        // changed underneath them. The conserved-vs-reverted CONTRAST is the
        // claim; the counts are its witnesses and want re-taking.
        let depth = crate::walk_depth(&ctx);
        let sea_level_m = quantize(globe.sea_level.get());

        let all: Vec<Vertex> = geo.vertices().collect();
        let stride = (all.len() / CONSERVATION_VERTICES).max(1);
        let mut scan: Vec<Vertex> = all
            .iter()
            .copied()
            .step_by(stride)
            .take(CONSERVATION_VERTICES)
            .chain(
                all.iter()
                    .copied()
                    .filter(|c| *globe.water_kind.get(*c) == WaterKind::River)
                    .take(CONSERVATION_VERTICES),
            )
            .collect();
        scan.sort_by_key(|c| c.0);
        scan.dedup();

        let mut vertices_checked = 0usize;
        let mut rooms_checked = 0usize;
        // Violations the REVERTED blended-threshold mechanism would cause,
        // counted at both strengths so the failure message can say which.
        let mut simulated_aggregate_violations = 0usize;
        let mut simulated_unanimity_violations = 0usize;

        for vertex in scan {
            let expected = *globe.water_kind.get(vertex);

            let mut simulated: Vec<WaterKind> = Vec::new();
            let mut rooms_in_vertex = 0usize;
            for addr in rooms_across_vertex(geo, vertex, depth) {
                let Ok(loc) = ctx.describe(&addr, WorldTime::GENESIS) else {
                    continue;
                };
                let dominant = dominant_of(&loc);
                // Only the rooms this vertex actually owns. A radius-4 patch can
                // straddle a vertex boundary, and a room on the other side of it
                // is a different vertex's business.
                if dominant != vertex {
                    continue;
                }
                rooms_in_vertex += 1;
                rooms_checked += 1;

                // THE CONSERVATION CLAIM, in its strong form.
                assert_eq!(
                    loc.fields.water, expected,
                    "room {:?} is owned by vertex {} but reports {:?} where the vertex itself is \
                     {:?}; sub-vertex water has stopped conserving its vertex's kind",
                    addr, vertex.0, loc.fields.water, expected
                );

                // The reverted mechanism, reconstructed: classify from the
                // room's own blended elevation and blended drainage, with
                // `endorheic` and terminal-sink status still read off the
                // dominant corner because a flag has no weighted mean.
                let denom: f64 = loc.corners.iter().map(|c| c.weight as f64).sum();
                let drainage_blend: f64 = loc
                    .corners
                    .iter()
                    .map(|c| c.weight as f64 * *globe.drainage.get(Vertex(c.vertex)))
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

            if rooms_in_vertex == 0 {
                continue;
            }
            vertices_checked += 1;
            if plurality(&simulated) != expected {
                simulated_aggregate_violations += 1;
            }
            if simulated.iter().any(|k| *k != expected) {
                simulated_unanimity_violations += 1;
            }
        }

        assert!(
            vertices_checked > 20 && rooms_checked > 200,
            "only {vertices_checked} vertices / {rooms_checked} rooms resolved; the scan is too thin \
             to trust either arm"
        );

        // THE TRIPWIRE ARM. If this fails, the criterion above is not
        // discriminating and must not be trusted as a guard.
        assert!(
            simulated_aggregate_violations > 0,
            "the reverted blended-threshold mechanism violated conservation on \
             {simulated_aggregate_violations} of {vertices_checked} vertices by aggregate and \
             {simulated_unanimity_violations} by unanimity — an aggregate count of zero means \
             THIS TEST CANNOT DETECT the mechanism it was written to catch, and the criterion \
             needs strengthening rather than the assertion relaxing"
        );
    }

    #[test]
    fn regime_is_deterministic_and_siblings_differ() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let a = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0],
        };
        let b = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1],
        };
        let ra = ctx.describe(&a, WorldTime::GENESIS).unwrap().regime;
        let ra2 = ctx.describe(&a, WorldTime::GENESIS).unwrap().regime;
        let rb = ctx.describe(&b, WorldTime::GENESIS).unwrap().regime;
        assert_eq!(ra, ra2, "same room → identical regime");
        assert_ne!(ra.descriptor, rb.descriptor, "sibling rooms should differ");
        assert!(ra.strangeness >= 0.0);
        assert!(!ra.descriptor.is_empty());
    }

    #[test]
    fn schema_is_v2_and_regime_present() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
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
    fn exits_are_eight_lateral_plus_vertical() {
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let addr = Facet {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let loc = ctx.describe(&addr, WorldTime::GENESIS).unwrap();
        let lateral = loc
            .exits
            .iter()
            .filter(|e| e.kind == ExitKind::Edge)
            .count();
        // EIGHT, not three (The Pavement, Task 3). The base mesh is an
        // 8-connected cube-sphere quad lattice: four edge-adjacent rooms and
        // four corner-adjacent ones. A room at one of the cube's eight
        // corners has seven — this address is not one, and the assertion is
        // deliberately the exact 8 rather than `>= 7`, so that a walk which
        // silently lost a diagonal would fail here.
        assert_eq!(lateral, 8, "eight geometric edges away from a cube corner");
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

    /// The rule [`heading_rose`] replaced was `compass()`, a 45-degree
    /// bucket, and its test asserted that the buckets covered the circle.
    /// That property is not the one that broke: the buckets did cover the
    /// circle, and two neighbours still landed in one of them. So the
    /// replacement asserts the property the bucket rule could not give —
    /// **one word per neighbour, one neighbour per word**.
    ///
    /// # WHERE IT SWEEPS IS THE WHOLE TEST, AND THE FIRST VERSION GOT IT WRONG
    ///
    /// That version walked three hops from `path: [1,2,3,0,1,2,3,0,1,2,3,0]`
    /// on each face. Measured: every one of the six balls is **49 rooms with
    /// 0 off-face** — entirely face-INTERIOR. The bucket rule failed at 19% of
    /// a face's outermost ring, ~1% one ring in, and **0% beyond**, so the
    /// rule this test exists to prove gone **would have passed it**. A guard
    /// weaker than its own description is decision 0467's subject exactly, and
    /// this one's description claimed it covered "the seam faces where the
    /// bucket rule failed 21.5% of the time" — which conflated being on face 4
    /// or 5 with being on the ring where it failed.
    ///
    /// So the sweep is seeded at three DIFFERENT regimes, and it asserts that
    /// it reached each of them rather than trusting the seeds:
    ///
    /// 1. **face-interior** — the 8-neighbour case, where the rule must be a
    ///    perfect matching;
    /// 2. **seam-crossing** — an alternating-digit path lands part-way along a
    ///    face edge, and its 3-hop ball is 49 rooms with 21 off-face. This is
    ///    the ring the deleted rule actually failed on;
    /// 3. **cube-corner** — a constant-digit path lands on a corner, whose
    ///    ball is 40 rooms and which contains rooms with **seven** neighbours.
    ///    The guarantee there is that exactly one word goes unassigned, which
    ///    the interior regime cannot exercise at all.
    ///
    /// The three coverage assertions at the end are the load-bearing part: a
    /// future change to the seeds that quietly returned this to interior-only
    /// reddens them instead of passing.
    ///
    /// # MUTATION THIS MUST FAIL AGAINST, and it is the one that matters
    ///
    /// Restore the deleted rule: label each exit `bucket(addr.bearing_to(&n))`
    /// with the old 45-degree sector instead of the word [`heading_rose`]
    /// assigned. Confirmed red, 2026-08-31:
    ///
    /// ```text
    /// Facet { face: 0, path: [0,0,0,0,0,0,0,0,0,0,0,1] }:
    ///   the exit list repeats a compass word: [N, Ne, Se, Se, S, Sw, W, Nw]
    /// ```
    ///
    /// `Se` twice and `E` never — the exact defect, on a room only the
    /// corner-seeded ball reaches. Run against the interior-only seeds this
    /// test shipped with, the same mutation passes.
    #[test]
    fn every_room_labels_each_neighbour_with_its_own_compass_word() {
        let world = land_world();
        // Hoisted out of the loop. It was built once per swept room, which
        // cost 107 s for a test whose subject is pure mesh geometry — the
        // stage gate pays that every run.
        let ctx = LocaleContext::build(&world).unwrap();

        // Three seeds per face. `[1,2,3,0,...]` is deep inside the face;
        // `[a,b,a,b,...]` with `a != b` walks toward a face EDGE; `[a,a,a,...]`
        // descends into one quadrant every level and lands on a CORNER. The
        // last two are the construction `windows/scene`'s `seam_observer` uses
        // for the same reason.
        let mut bases: Vec<Facet> = Vec::new();
        for face in 0..6u8 {
            bases.push(Facet {
                face,
                path: vec![1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0],
            });
            bases.push(Facet {
                face,
                path: (0..12).map(|i| if i % 2 == 0 { 0 } else { 1 }).collect(),
            });
            bases.push(Facet {
                face,
                path: vec![0; 12],
            });
        }

        let mut rooms = 0usize;
        let mut interior = 0usize;
        let mut crossing = 0usize;
        let mut corners = 0usize;
        for base in &bases {
            for room in walk_visited(base, 3) {
                let ns = room.neighbors();
                let rose = heading_rose(&room);
                let assigned: Vec<&Facet> = rose.iter().flatten().collect();
                assert_eq!(
                    assigned.len(),
                    ns.len(),
                    "{room:?}: {} neighbours but {} compass words assigned — the \
                     assignment must place every neighbour exactly once",
                    ns.len(),
                    assigned.len()
                );
                let distinct: std::collections::BTreeSet<&Facet> =
                    assigned.iter().copied().collect();
                assert_eq!(
                    distinct.len(),
                    assigned.len(),
                    "{room:?}: a neighbour was named by two compass words at once"
                );
                // And the exits the document carries agree with it, letter for
                // letter — the whole point of collapsing the two rules.
                let loc = ctx.describe(&room, WorldTime::GENESIS).unwrap();
                let letters: Vec<Compass> = loc
                    .exits
                    .iter()
                    .filter(|e| e.kind == ExitKind::Edge)
                    .filter_map(|e| match e.direction {
                        Direction::Compass(c) => Some(c),
                        _ => None,
                    })
                    .collect();
                let unique: std::collections::BTreeSet<String> =
                    letters.iter().map(|c| format!("{c:?}")).collect();
                assert_eq!(
                    unique.len(),
                    letters.len(),
                    "{room:?}: the exit list repeats a compass word: {letters:?}"
                );
                assert_eq!(
                    letters.len(),
                    ns.len(),
                    "{room:?}: the document carries {} lateral exits for {} neighbours",
                    letters.len(),
                    ns.len()
                );

                if ns.len() == 7 {
                    corners += 1;
                    assert_eq!(
                        rose.iter().filter(|n| n.is_none()).count(),
                        1,
                        "{room:?}: a seven-neighbour room must leave exactly one \
                         compass word unassigned"
                    );
                }
                if ns.iter().any(|n| n.face != room.face) {
                    crossing += 1;
                } else {
                    interior += 1;
                }
                rooms += 1;
            }
        }

        println!(
            "rose sweep: {rooms} rooms — {interior} interior, {crossing} seam-crossing, \
             {corners} cube-corner"
        );
        // THE COVERAGE GUARD. Without these three the sweep can silently
        // narrow back to the interior-only population the first version had,
        // where the rule this test replaced would itself have passed.
        assert!(
            interior > 0,
            "the sweep reached no face-interior room, so the eight-neighbour \
             matching is untested"
        );
        assert!(
            crossing > 0,
            "the sweep reached no room whose neighbourhood crosses a base face — \
             that is the ring the deleted 45-degree bucket rule actually failed \
             on, so a sweep without one cannot tell this rule from that one"
        );
        assert!(
            corners > 0,
            "the sweep reached no cube-corner room, so 'seven neighbours leaves \
             exactly one word unmatched' is untested"
        );
        assert!(
            rooms > 50,
            "too few rooms swept to trust this ({rooms}); the fixture stopped \
             producing neighbourhoods"
        );
    }

    /// The ceiling, in degrees, on how far ANY compass word may point from the
    /// bearing of the room it names, anywhere in the walk band.
    ///
    /// **Measured 34.577273**, at face-3 ring-0 room `FacetId(2236617796)`,
    /// on the `E` word — over 215,400 rooms across the two populations
    /// [`the_worst_compass_word_is_within_the_measured_ceiling`] sweeps. The
    /// pin is the measured value rounded up in its fourth decimal, not a
    /// margin: the sweep is fixed and the mesh is integer arithmetic through
    /// pure-Rust `libm`, so the number is exactly reproducible and there is
    /// nothing for slack to absorb.
    ///
    /// The rule this replaced measured **156.5155** over the same rooms, with
    /// 17.31% of the cube-edge rings past 90 degrees.
    /// type-audit: bare-ok(angle)
    const ROSE_WORST_DEG: f64 = 34.578;

    /// The floor under [`ROSE_WORST_DEG`] — the arm a `<=` ceiling cannot
    /// give. An improvement that dropped the worst word below this is good
    /// news that must be banked into the ceiling rather than absorbed in
    /// silence. Set one full degree under the measured value, so an ordinary
    /// change of sample does not redden it and a change of KIND does.
    /// type-audit: bare-ok(angle)
    const ROSE_WORST_FLOOR_DEG: f64 = 33.5;

    /// The exact count of sampled (room, word) pairs whose step does not
    /// invert — `go W` then `go E` does not return you.
    ///
    /// **Measured 190 of 12,282 pairs (1.547%).** Two-sided, like
    /// [`ROSE_WORST_DEG`], and an exact count rather than a rate because the
    /// sample is fixed and deterministic.
    ///
    /// **This number went UP with the fix, and that is the trade, stated.**
    /// Greedy measured 82 of the same 12,282 (0.668%). A per-room assignment
    /// is computed without consulting the neighbour's own assignment, and an
    /// optimal rule redistributes error further from "nearest first", so the
    /// two rooms disagree more often. What they disagree ABOUT is small: 184
    /// of the 190 are off by exactly ONE word (the neighbour calls you `Nw`
    /// where you called it `W`), and 148 sit at a cube seam. Against that,
    /// greedy's 0.756% of uniformly-sampled rooms carrying a word more than
    /// 90 degrees off is now zero. A step that lands one word away is a step
    /// in about the right direction; a word 156 degrees off is not.
    ///
    /// # WHAT THE 190 IS THE PRICE OF — measured, because the first answer
    /// # here was wrong
    ///
    /// This doc used to say the 190 "is a property of the MESH plus any
    /// per-room rule, not of this objective". **That is refuted by its own
    /// paragraph above** — greedy is also a per-room rule and scores 82 — and
    /// more sharply by measurement. Over this test's exact 12,282-pair
    /// population, a **non-bijective nearest-word rule** (each neighbour
    /// independently takes the word nearest its bearing, duplicates and gaps
    /// allowed) scores **14 non-inverting pairs (0.114%) with a worst error of
    /// 22.4464 degrees** — better than this rule on BOTH counts. So ~14 is the
    /// mesh's own floor, and the other 176 are the price of the **bijection**:
    /// every neighbour gets exactly one word, and every word names at most one
    /// neighbour.
    ///
    /// **That bijection is not a stylistic preference; dropping it costs
    /// navigability.** Under the nearest-word rule, **152 of the 1,536 sampled
    /// rooms (9.9%) are not bijective**: 248 neighbour steps share a word with
    /// another neighbour of the same room, and 248 word slots go unused beyond
    /// the six a 7-arity cube corner legitimately leaves. That is 2.0% of all
    /// steps reachable by no unambiguous word — `go E` either refuses or picks
    /// one of two rooms — which is a worse thing to hand a player than a
    /// one-word round-trip failure.
    ///
    /// **The real shape is a trilemma: bijection, bounded per-room accuracy,
    /// and invertibility — any two, not all three.** This rule takes the first
    /// two. Supporting that choice, and measured in fix round 1 and its
    /// re-review rather than argued: min-sum over the same DP scores the
    /// identical 190; lexicographic min-max AND min-sum run on *symmetrised*
    /// (forced-antipodal) bearings also score 190 while degrading the worst
    /// error to 45 and 90 degrees, so the cheap "consult the pair, not the
    /// room" fix buys nothing and costs accuracy; and a rule that consulted
    /// the neighbour's own ASSIGNMENT is circular (a's rose depends on b's rose
    /// depends on a's) and would need a global matching over the whole
    /// 8-connected graph — non-local, uncacheable, and topologically
    /// obstructed at the eight cube corners.
    /// type-audit: bare-ok(count)
    const ROSE_NON_INVERTIBLE_PAIRS: usize = 190;

    /// Every room at `WALK` on `face` whose lattice coordinates this sweep
    /// visits, built by the inverse of [`Facet::face_lattice`]'s decode.
    fn lattice_room(face: u8, x: i64, y: i64, depth: u32) -> Facet {
        Facet {
            face,
            path: (0..depth)
                .rev()
                .map(|i| ((((x >> i) & 1) as u8) << 1) | (((y >> i) & 1) as u8))
                .collect(),
        }
    }

    /// The worst angular error [`heading_rose`] commits at `room`: over every
    /// word it assigned, the gap between the word's canonical bearing and the
    /// true bearing to the room it was given.
    fn worst_rose_error_deg(room: &Facet) -> (f64, usize) {
        let words = Compass::all();
        let mut worst = 0.0f64;
        let mut worst_word = 0usize;
        for (w, n) in heading_rose(room).iter().enumerate() {
            let Some(n) = n else { continue };
            let e = bearing_gap_deg(room.bearing_to(n), words[w].bearing_deg());
            if e > worst {
                worst = e;
                worst_word = w;
            }
        }
        (worst, worst_word)
    }

    /// **THE ASSERTION WHOSE ABSENCE LET `go E` WALK WEST.**
    ///
    /// Nothing in this workspace looked at [`heading_rose`]'s angular error
    /// before fix round 1 of The Pavement. The test above
    /// ([`every_room_labels_each_neighbour_with_its_own_compass_word`]) asserts
    /// cardinality and distinctness, and its own doc explicitly sets the
    /// accuracy property aside as "not the one that broke" — true of the
    /// 45-degree bucket rule it was written against, and precisely false of
    /// the greedy matching that replaced it. The greedy rule stranded a word on
    /// whatever neighbour was left: at seed-42 room `FacetId(2169509120)`,
    /// `go E` walked 156.1 degrees off, west and north, and `look` said `E` was
    /// open. A guarantee nothing measures is a guarantee that has not been made.
    ///
    /// # WHAT IT MEASURES, and why two populations rather than one
    ///
    /// The error is not uniform over the globe, so a uniform sample alone
    /// understates it. Two populations are swept and reported separately:
    ///
    /// 1. **A uniform grid** — 46 x 46 lattice positions per face, all six
    ///    faces, 12,696 rooms. This is the population a player mostly stands
    ///    in.
    /// 2. **The cube-edge rings, enumerated exactly** — every room in ring 0
    ///    (the outermost lattice ring, where a room's neighbourhood spans a
    ///    cube seam) plus rings 1 and 2 on a stride. Under greedy this
    ///    population carried 17.78% of its rooms past 90 degrees while ring 1
    ///    and inward carried none, so it is the one that decides the ceiling
    ///    and a sweep that misses it measures the wrong thing.
    ///
    /// # THE MEASURED NUMBERS (this tree, fix round 1)
    ///
    /// Both rules, over identical populations, so the improvement is a
    /// measurement rather than a claim:
    ///
    /// ```text
    ///                        worst      >45 deg          >90 deg
    ///   greedy   uniform   155.9378    760  (5.986%)     96 (0.756%)
    ///   greedy   rings     156.5155  52560 (25.929%)  35088 (17.310%)
    ///   optimal  uniform    34.3468      0  (0.000%)      0 (0.000%)
    ///   optimal  rings      34.5773      0  (0.000%)      0 (0.000%)
    ///
    ///   invertibility (12,282 pairs)   greedy 82 (0.668%)
    ///                                 optimal 190 (1.547%)
    /// ```
    ///
    /// The uniform row reproduces the review's independent figures (5.96% and
    /// 0.82% past 45 and 90 degrees, worst 156.1) closely enough to confirm
    /// the two instruments are looking at the same thing.
    ///
    /// # WHY THE CEILING IS TWO-SIDED
    ///
    /// A `<=` ceiling nothing can fall through quietly is a different
    /// instrument from one nothing can fall through at all: the upper arm
    /// catches a regression, and the lower arm catches an IMPROVEMENT nobody
    /// banked, which is how a ceiling drifts into meaninglessness. The idiom is
    /// `GROWN_RELAXATIONS` in `windows/vessel/src/lattice/anchor_cells.rs`. The  // lexicon: a FILE PATH, and the cells it names are chamber floor squares (areas), not mesh vertices
    /// floor here is a band rather than an equality, because the worst case is
    /// a float over ~200k rooms and pinning it to the last bit would redden on
    /// any change of sample rather than any change of quality.
    ///
    /// # WHAT THIS CHECK IS BLIND TO (decision 0491)
    ///
    /// 1. **It is a sample, not a proof.** The uniform grid touches 12,696 of
    ///    the band's 402,653,184 rooms and the ring sweep is exact only for
    ///    ring 0. A pathology confined to rooms neither population lands on is
    ///    invisible here. Ring 0 is enumerated exactly *because* it was the
    ///    offender; a future defect with a different geography would need its
    ///    own population added.
    /// 2. **It bounds the WORST word, not the DISTRIBUTION.** A change that
    ///    left the worst case alone and doubled the number of rooms carrying a
    ///    30-degree word would pass. (The example is 30 and not 40 because a
    ///    40-degree word is impossible under the 34.578 ceiling standing above
    ///    it — a blindness statement whose own example the test would catch is
    ///    worse than no example.) The `over_45` / `over_90` counts are
    ///    **printed, not asserted** — both read 0 today, so an assertion on
    ///    them would be strictly implied by `worst <= ROSE_WORST_DEG` and would
    ///    add no coverage; what they give a reader is the shape of the
    ///    distribution the ceiling summarises, and nothing in this test bounds
    ///    the mass below 34.578.
    /// 3. **It says nothing about which word is wrong.** A rose that named
    ///    every neighbour 20 degrees off would pass as readily as one that is
    ///    exact seven times and 20 degrees off once, and the second is the
    ///    better rose.
    /// 4. **Invertibility is bounded, not eliminated, and the residue is the
    ///    price of a BIJECTIVE optimal assignment rather than the mesh's
    ///    alone** — the mesh's own floor is 14 over the same population, not
    ///    190. See [`ROSE_NON_INVERTIBLE_PAIRS`]'s doc.
    /// 5. **It runs at one depth.** `WALK` is checked against the live walk
    ///    depth by `cli/tests/suite/walk_depth_agreement.rs`'s
    ///    `every_walk_constant_tracks_the_walk_band`, so it cannot go stale —
    ///    but a coarser band's error is not measured here at all.
    ///
    /// claim: rate(forall-room-sampled, worst <= ROSE_WORST_DEG, measured
    /// ceiling) — two-sided, over two populations, with a non-vacuity guard
    #[test]
    fn the_worst_compass_word_is_within_the_measured_ceiling() {
        // The walk band's depth. Named `WALK` so that
        // `every_walk_constant_tracks_the_walk_band` pins it to the live
        // `walk_depth` without this file having to be rostered anywhere.
        const WALK: u32 = 13;
        /// Lattice positions per side of the uniform grid, per face.
        const GRID: i64 = 46;
        let scale: i64 = 1 << WALK;

        struct Tally {
            rooms: usize,
            worst: f64,
            worst_at: Option<(Facet, usize)>,
            over_45: usize,
            over_90: usize,
            over_135: usize,
        }
        impl Tally {
            fn new() -> Self {
                Self {
                    rooms: 0,
                    worst: 0.0,
                    worst_at: None,
                    over_45: 0,
                    over_90: 0,
                    over_135: 0,
                }
            }
            fn add(&mut self, room: &Facet) {
                let (e, w) = worst_rose_error_deg(room);
                self.rooms += 1;
                if e > 45.0 {
                    self.over_45 += 1;
                }
                if e > 90.0 {
                    self.over_90 += 1;
                }
                if e > 135.0 {
                    self.over_135 += 1;
                }
                if e > self.worst {
                    self.worst = e;
                    self.worst_at = Some((room.clone(), w));
                }
            }
            fn line(&self, name: &str) -> String {
                format!(
                    "{name}: {} rooms, worst {:.4} deg at {:?} ({:?}), >45 {} ({:.3}%), \
                     >90 {}, >135 {}",
                    self.rooms,
                    self.worst,
                    self.worst_at.as_ref().map(|(r, _)| r.pack().map(|p| p.0)),
                    self.worst_at
                        .as_ref()
                        .map(|(_, w)| Compass::all()[*w])
                        .unwrap_or(Compass::N),
                    self.over_45,
                    self.over_45 as f64 * 100.0 / self.rooms.max(1) as f64,
                    self.over_90,
                    self.over_135,
                )
            }
        }

        // POPULATION 1 — the uniform grid.
        let mut uniform = Tally::new();
        for face in 0..6u8 {
            for i in 0..GRID {
                for j in 0..GRID {
                    let room = lattice_room(face, i * scale / GRID, j * scale / GRID, WALK);
                    uniform.add(&room);
                }
            }
        }

        // POPULATION 2 — the cube-edge rings. Ring 0 exactly; rings 1 and 2 on
        // a stride, because they were clean under greedy and are here to show
        // that the geography has not moved inward rather than to bound it.
        let mut rings = Tally::new();
        let mut ring0_rooms = 0usize;
        for face in 0..6u8 {
            for ring in 0..3i64 {
                let lo = ring;
                let hi = scale - 1 - ring;
                let stride = if ring == 0 { 1 } else { 64 };
                let mut edge = |x: i64, y: i64| {
                    let room = lattice_room(face, x, y, WALK);
                    rings.add(&room);
                    if ring == 0 {
                        ring0_rooms += 1;
                    }
                };
                let mut t = lo;
                while t <= hi {
                    edge(t, lo);
                    edge(t, hi);
                    if t != lo && t != hi {
                        edge(lo, t);
                        edge(hi, t);
                    }
                    t += stride;
                }
            }
        }

        // INVERTIBILITY. `go W` then `go E` must return you. Measured over a
        // coarser grid than the error sweep because each pair costs a second
        // `heading_rose`.
        let mut pairs = 0usize;
        let mut non_invertible = 0usize;
        let mut example: Option<(u64, usize)> = None;
        for face in 0..6u8 {
            for i in 0..16i64 {
                for j in 0..16i64 {
                    let room = lattice_room(face, i * scale / 16, j * scale / 16, WALK);
                    let rose = heading_rose(&room);
                    for (w, n) in rose.iter().enumerate() {
                        let Some(n) = n else { continue };
                        pairs += 1;
                        let back = heading_rose(n);
                        if back[(w + 4) % 8].as_ref() != Some(&room) {
                            non_invertible += 1;
                            if example.is_none() {
                                example = Some((room.pack().map(|p| p.0).unwrap_or(0), w));
                            }
                        }
                    }
                }
            }
        }

        let worst = uniform.worst.max(rings.worst);
        let per_mille = non_invertible * 1000 / pairs.max(1);
        println!("{}", uniform.line("uniform"));
        println!("{}", rings.line("rings"));
        println!("  ring 0 enumerated exactly: {ring0_rooms} rooms");
        println!(
            "invertibility: {non_invertible} of {pairs} pairs do not invert \
             ({:.4}%, {per_mille} per mille), first {example:?}",
            non_invertible as f64 * 100.0 / pairs.max(1) as f64
        );
        println!("WORST OVERALL: {worst:.6} deg");

        // NON-VACUITY. Without these the sweep can silently stop visiting the
        // population that decides the answer.
        assert_eq!(
            uniform.rooms,
            6 * (GRID * GRID) as usize,
            "the uniform grid did not visit every position it claims"
        );
        assert_eq!(
            ring0_rooms,
            6 * (4 * scale as usize - 4),
            "ring 0 was not enumerated exactly, so the population that decides \
             the ceiling is a sample"
        );
        assert!(
            pairs > 3000,
            "too few pairs to say anything about inversion"
        );

        assert!(
            worst <= ROSE_WORST_DEG,
            "a compass word points {worst:.4} degrees off the room it names, over the \
             measured ceiling of {ROSE_WORST_DEG}. This is the defect fix round 1 \
             closed: at 90 degrees or more the word names approximately the wrong \
             half of the compass, and `look` will agree with it.\n  {}\n  {}",
            uniform.line("uniform"),
            rings.line("rings")
        );
        // The other direction, and it is the half a `<=` cannot see: an
        // improvement nobody banks leaves a ceiling that stops meaning
        // anything. Not an equality — the worst case is a float over ~200k
        // rooms, so a band is the honest pin.
        assert!(
            worst >= ROSE_WORST_FLOOR_DEG,
            "the worst compass word is only {worst:.4} degrees off, UNDER the floor of \
             {ROSE_WORST_FLOOR_DEG} — the rule or the mesh improved, which is good news \
             that must be banked: lower ROSE_WORST_DEG toward {worst:.4} and say in its \
             doc what moved"
        );
        // INVERTIBILITY, two-sided for the same reason the angle is. The
        // residue is the price of a BIJECTIVE optimal per-room assignment, not
        // of the mesh alone — a non-bijective nearest-word rule scores 14 over
        // this same population, and `ROSE_NON_INVERTIBLE_PAIRS`'s doc has the
        // whole trade. What this pins is that the residue has not GROWN, and
        // that it is still the small kind (off by one word, mostly at a seam)
        // rather than the reversing kind.
        assert!(
            non_invertible <= ROSE_NON_INVERTIBLE_PAIRS,
            "{non_invertible} of {pairs} steps do not invert ({per_mille} per mille), \
             over the measured count of {ROSE_NON_INVERTIBLE_PAIRS}. First: {example:?}"
        );
        assert!(
            non_invertible >= ROSE_NON_INVERTIBLE_PAIRS,
            "only {non_invertible} of {pairs} steps fail to invert, UNDER the measured \
             {ROSE_NON_INVERTIBLE_PAIRS} — the rule or the mesh improved, which is good \
             news that must be banked: lower ROSE_NON_INVERTIBLE_PAIRS to \
             {non_invertible} and say in its doc what moved"
        );
    }

    /// The bearing table is the one [`heading_rose`] assigns against, and its
    /// eight words must name eight distinct bearings a full circle apart —
    /// the invariant the deleted `compass()` bucket test was really standing
    /// in for.
    #[test]
    fn the_eight_bearings_are_distinct_and_evenly_spaced() {
        let mut degs: Vec<f64> = Compass::all().iter().map(|c| c.bearing_deg()).collect();
        assert_eq!(degs.len(), 8);
        degs.sort_by(f64::total_cmp);
        for (i, d) in degs.iter().enumerate() {
            assert_eq!(*d, i as f64 * 45.0, "bearing {i} is {d}, not {}", i * 45);
        }
    }

    /// A small walk-visited neighborhood: `start` plus every room reachable
    /// within `hops` edge-steps (BFS over `Facet::neighbors`, the same
    /// mesh a real possession walk traverses) — the-waymark Task 3's
    /// "rooms a real walk visits" fixture, sized for a fast unit test rather
    /// than a full possession transcript.
    fn walk_visited(start: &Facet, hops: u32) -> Vec<Facet> {
        let mut seen: std::collections::BTreeSet<Facet> = std::collections::BTreeSet::new();
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
    fn every_cached_reader_bit_equals_its_recomputing_sibling_with_a_partial_prefill() {
        // The-waymark fix round, Finding 1: a PREFILLED, READ-ONLY cache
        // (never mutated by the readers themselves) must still be
        // byte-identical to the raw recomputing siblings, on BOTH a cache
        // hit (prefilled rooms) and a cache miss (rooms deliberately left
        // out of the prefill, falling through to a fresh `corner_weights`
        // call) — correctness must never depend on prefill completeness.
        let world = land_world();
        let ctx = LocaleContext::build(&world).unwrap();
        let start = Facet {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let rooms = walk_visited(&start, 3);
        assert!(rooms.len() > 10, "fixture must cover a real neighborhood");
        let at = WorldTime::from_std_days(12.5).expect("a day value is finite");
        let zero_field = hornvale_kernel::VertexMap::from_fn(ctx.climate().geosphere(), |_| 0.0f64);

        // Prefill only the EVEN-indexed rooms (under `&mut`) — the rest stay
        // deliberately un-prefilled, so this run exercises both a hit and a
        // miss for every one of the readers below. The count is deliberately
        // NOT in the name or this comment: a sixth (`reflectance_at_facet_
        // cached`, The Wash) arrived after the name was written, and a name
        // that carries a tally goes quietly false the moment one does.
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

            // The sixth reader (The Wash, Task 3): the world map's colour
            // path. `Reflectance` has no `Eq`, so compare the curves.
            let expected_refl = ctx.reflectance_at_facet(addr, at).map(|r| *r.get());
            let got_refl = ctx
                .reflectance_at_facet_cached(addr, at, Some(&memo))
                .map(|r| *r.get());
            assert_eq!(
                expected_refl.is_ok(),
                got_refl.is_ok(),
                "reflectance_at_facet_cached Ok/Err mismatch at {addr:?}"
            );
            if let (Ok(exp), Ok(got)) = (expected_refl, got_refl) {
                assert_eq!(exp, got, "reflectance_at_facet_cached at {addr:?}");
            }
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
        let real_level = ctx.climate().geosphere().depth();
        let other_level = real_level + 1;
        let fake_geo = hornvale_kernel::Geosphere::new(other_level);
        let fake_index = NearestVertexIndex::new(&fake_geo);
        let mut memo = hornvale_kernel::RoomMeshMemo::new();
        let fake_addr = Facet {
            face: 0,
            path: vec![0; other_level as usize],
        };
        fake_addr.corner_weights_memo(&fake_geo, &fake_index, &mut memo);

        let real_addr = Facet {
            face: 4,
            path: vec![2, 0, 3, 1, 2, 0, 3, 1, 2, 0, 3, 1],
        };
        let _ = ctx.temperature_at_cached(&real_addr, WorldTime::GENESIS, Some(&memo));
    }
}
