//! The situated pole of the scene protocol: `scene/surrounds/v2`, an
//! egocentric neighbourhood of rooms around an observer, placed by exact
//! integer lattice coordinates. Semantic-only and FOG-FREE — this builder
//! never invents epistemic state; a session-owning consumer (the vessel)
//! overlays what it alone knows.

use crate::{Feature, SceneError, features_of};
use hornvale_kernel::{RoomAddr, SeaLevelHeight, World, WorldTime};
use hornvale_locale::{CoverClass, Locale, LocaleContext, biome_prose_name};
use serde::Serialize;
use std::collections::{BTreeMap, BTreeSet, VecDeque};

/// The schema identifier this module emits.
/// type-audit: bare-ok(identifier-text)
pub const SURROUNDS_SCHEMA: &str = "scene/surrounds/v2";

/// The largest legal neighbourhood radius, in BFS rings. A ring-`k`
/// neighbourhood holds `1 + 3k(k+1)/2` cells, so 8 is 109 cells — past
/// what a coarse chart can say anything useful with.
/// type-audit: bare-ok(count)
pub const MAX_SURROUNDS_RADIUS: u32 = 8;

/// The relief catalog, in stable ascending order. Band boundaries are
/// contract: changing one, or the quantity they are measured against, mints
/// `scene/surrounds/v3`.
/// type-audit: bare-ok(identifier-text)
pub const RELIEF_LEGEND: [&str; 6] = ["abyss", "shelf", "lowland", "upland", "highland", "alpine"];

/// Height above sea level to an index into [`RELIEF_LEGEND`].
///
/// The parameter is a [`SeaLevelHeight`] and not a `ReferenceElevation` for the
/// reason The Benchmark exists: these thresholds are sea-level-relative, and
/// before v2 this function was handed the raw isostatic reading, so on a world
/// whose sea level sits near -2936 m almost all land classified as `shelf`.
/// type-audit: bare-ok(index: return)
fn relief_band(height: SeaLevelHeight) -> u32 {
    match height.get() {
        e if e < -3000.0 => 0,
        e if e < 0.0 => 1,
        e if e < 300.0 => 2,
        e if e < 1000.0 => 3,
        e if e < 2500.0 => 4,
        _ => 5,
    }
}

/// Where the observer stands.
/// type-audit: bare-ok(index: room), bare-ok(index: face), bare-ok(count: depth), pending(wave-3: latitude), pending(wave-3: longitude)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsObserver {
    /// Packed room id.
    pub room: u64,
    /// Base icosahedron face.
    pub face: u8,
    /// Refinement depth.
    pub depth: u32,
    /// Centroid latitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub latitude: f64,
    /// Centroid longitude, degrees.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub longitude: f64,
}

/// A salience-ranked thing standing on a cell. `noun` is the examinable key
/// — it is what joins this chart to the prose's own noun catalog.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(identifier-text: kind), bare-ok(prose: datum), bare-ok(index: salience)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Mark {
    /// The examinable noun.
    pub noun: String,
    /// What kind of thing this is. The engine emits two built-in kinds —
    /// `"settlement"` and `"cave"` — and a session-owning consumer adds
    /// `"agent"`. A consumer that does not recognize a kind should still
    /// render the mark: `legend_of` catalogs kinds generically and
    /// `render_surrounds_ascii` treats every non-`"agent"` kind alike, so a
    /// future kind needs no special case anywhere to appear.
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
}

/// One `(noun, datum)` pair of the chart's catalog — deliberately the same
/// shape as the focalizer's `Focalized.nouns`, because that identity is what
/// makes map and prose two grains of one lens.
/// type-audit: bare-ok(identifier-text: noun), bare-ok(prose: datum)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct LegendEntry {
    /// The examinable noun.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
}

/// One cell of the chart. Lattice coordinates are RELATIVE to the observer
/// and absent on a seam cell. Several fields below are `null` on every cell
/// but the observer's own — that is an EMIT gate (`is_here`), not a grain
/// gate: `ctx.describe` computes them for every cell in the build loop, and
/// the value is discarded rather than absent. See [`SurroundsCell::regime`]
/// for the full explanation. [`SurroundsCell::micro`] is the field that
/// genuinely is emitted for every cell, sub-cell grain included.
/// type-audit: bare-ok(index: room), bare-ok(index: u), bare-ok(index: v), bare-ok(index: w), bare-ok(flag: up), bare-ok(flag: seam), bare-ok(identifier-text: state), bare-ok(index: biome), bare-ok(index: water), bare-ok(index: relief), bare-ok(prose: regime), bare-ok(diagnostic-value: temperature_c), bare-ok(ratio: moisture), waiver(elevation-convention: elevation_m), bare-ok(diagnostic-value: height_asl_m), bare-ok(artifact: color), bare-ok(artifact: signal), bare-ok(index: cover), bare-ok(diagnostic-value: bearing_deg), bare-ok(diagnostic-value: distance_rad)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsCell {
    /// Packed room id.
    pub room: u64,
    /// Lattice offset from the observer on axis 0; `null` on a seam cell.
    pub u: Option<i64>,
    /// Lattice offset on axis 1; `null` on a seam cell.
    pub v: Option<i64>,
    /// Lattice offset on axis 2; `null` on a seam cell.
    pub w: Option<i64>,
    /// Triangle orientation; `null` on a seam cell.
    pub up: Option<bool>,
    /// Set when this cell lies on a different base face than the observer,
    /// so the lattice bends and no honest local coordinate exists.
    pub seam: bool,
    /// `"here"`, `"sensed"`, or (written only by a session-owning consumer)
    /// `"remembered"`.
    pub state: String,
    /// Index into `biome_legend`.
    pub biome: u32,
    /// Index into `water_legend`.
    pub water: u32,
    /// Index into `relief_legend`.
    pub relief: u32,
    /// The strangeness overlay's descriptor — **emitted only for the
    /// observer's own cell** (`state == "here"`), `null` on every other.
    ///
    /// This is an EMIT gate, not a grain gate. `ctx.describe` runs for every
    /// cell in the build loop, so the value exists everywhere and is
    /// discarded here. These comments previously read "fine grain, `null`
    /// when coarse", which led a campaign to design around the premise that
    /// the data did not exist at coarse grain. If you are looking for the
    /// per-cell sub-cell signal, it is [`SurroundsCell::micro`], which is
    /// emitted for every cell.
    pub regime: Option<String>,
    /// Annual-mean temperature, °C — emitted only for the observer's own
    /// cell (`is_here`), `null` on every other. See [`SurroundsCell::regime`].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub temperature_c: Option<f64>,
    /// Moisture — emitted only for the observer's own cell (`is_here`),
    /// `null` on every other. See [`SurroundsCell::regime`].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub moisture: Option<f64>,
    /// Elevation, metres — emitted only for the observer's own cell
    /// (`is_here`), `null` on every other. See [`SurroundsCell::regime`].
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub elevation_m: Option<f64>,
    /// Height above sea level, metres — emitted only for the observer's own
    /// cell (`is_here`), `null` on every other. See [`SurroundsCell::regime`].
    /// Signed: negative below. `relief` is banded from this.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_f64_field")]
    pub height_asl_m: Option<f64>,
    /// Display colour under the requested observer, absent unless this scene
    /// was built through [`surrounds_scene_colored_in`]. The key is skipped
    /// entirely when absent, so an uncoloured document is byte-for-byte what
    /// it was before the colour layer existed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
    /// The sub-cell micro-field at this room — always present, for every
    /// cell. See [`Micro`].
    pub micro: Micro,
    /// Salience-ranked things standing here.
    pub marks: Vec<Mark>,
    /// The observer's raw per-channel response at this cell — the producer's
    /// own render, rung 2 of spec §4.2's ladder — absent unless this scene
    /// was built through [`surrounds_scene_colored_in`], same key-omission
    /// discipline as `color`. Projecting this through the carried
    /// observer's [`Sight`] must reproduce `color` byte-for-byte (the
    /// migration control, spec §4.2) — that is what makes shipping both in
    /// one document strictly stronger than the cross-version comparison the
    /// spec's `RENDER-appearance-signal-protocol` proposal described.
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_vec_f64_field"
    )]
    pub signal: Option<Vec<f64>>,
    /// Index into [`SurroundsScene::cover_legend`] — the categorical surface
    /// cover class (spec §4.3), absent unless this scene was built through
    /// [`surrounds_scene_colored_in`], same key-omission discipline as
    /// `color`. Not derivable by thresholding `color`: two clients would
    /// threshold a continuous mixture differently, producing two
    /// disagreeing worlds from one document.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cover: Option<u32>,
    /// Great-circle initial azimuth from the observer to this cell's own
    /// centroid, degrees clockwise from north (`RoomAddr::bearing_to`).
    /// `Option`-free and present on every cell, including a seam cell (whose
    /// `room` is a plain `u64`, not the `Option`al lattice offsets above) —
    /// this and `distance_rad` are the whole north-up unblock (spec §5.1):
    /// together they are `other`'s polar coordinate about the observer,
    /// which is what a thin client needs to place a cell without doing
    /// spherical trigonometry itself.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub bearing_deg: f64,
    /// Great-circle angular distance from the observer to this cell's own
    /// centroid, radians (`RoomAddr::distance_rad_to`). See `bearing_deg`.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub distance_rad: f64,
}

/// The sub-cell micro-field at a room: four independent axes in `[-1, 1]`,
/// each drawn from the room's own address noise, so a walk through
/// homogeneous biome still varies room to room.
///
/// A scene-side type rather than a re-export of `hornvale_locale::MicroField`,
/// for the reason every other float on this schema is: the wire type carries
/// the emit-boundary quantization (decision 0033) and the producer's type
/// carries the compute-path value. Coupling the published schema to a
/// window's internal struct would make a refactor there a cross-repo schema
/// change here.
///
/// **Not `Option`.** Every room has a micro-field — it is a pure function of
/// the room's address and the world seed — so an absent value would mean
/// "this emitter chose not to say", which is the exact confusion the
/// `is_here` gate on the fields above created. See this module's doc.
/// type-audit: bare-ok(ratio: relief), bare-ok(ratio: aspect), bare-ok(ratio: wetness), bare-ok(ratio: openness)
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct Micro {
    /// Micro-relief, hollow (`-1`) to rise (`+1`).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub relief: f64,
    /// Slope aspect / insolation, shaded (`-1`) to sunlit (`+1`).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub aspect: f64,
    /// Local wetness, dry (`-1`) to wet (`+1`).
    ///
    /// **Still not a water kind.** Since The Rill this axis *is* grounded in
    /// hydrology on bare ground under open air — the climate moisture supply
    /// the room's cells receive, allocated by the room's distance to its own
    /// sub-cell watercourse, with the address draw retained only as local
    /// variation about that value. At sea, on ice and in the rock column it
    /// remains the address draw alone, because there the same axis is read as
    /// the set of the current, snow cover and seep.
    ///
    /// A consumer must still not band a water kind from it: it says how wet the
    /// ground is, never whether there is water to drink or cross. `water` is
    /// the field that answers that, and `channel_distance` is the one that says
    /// where the river is.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub wetness: f64,
    /// Canopy openness, closed (`-1`) to open (`+1`).
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub openness: f64,
}

/// The eye a coloured chart was seen through, and what its projection to
/// sRGB preserves. Declared rather than derived because a document alone
/// cannot say which species looked, and a caller must be able to say so —
/// but `channels`, `chromatic`, `projection`, `preserves`, `channel_roles`,
/// `projection_slots`, and `projection_norms` are
/// **overwritten by the builder** from the [`hornvale_kernel::color::Observer`]
/// actually used to colour the chart, discarding whatever the caller put
/// there. That overwrite is the whole reason this block can be trusted: a
/// caller can name an eye (`observer`) and a sun angle (`sun_altitude_deg`)
/// — the two things a bare `Observer` cannot supply — but it cannot make
/// the document claim an arity or a projection the eye did not actually
/// have.
///
/// **`channel_roles`, `projection_slots`, and `projection_norms` are the
/// calibration spec §4.1 requires alongside `signal`, or `signal` is
/// decoration**: a client receiving `signal: [0.31, 0.44, 0.09]` cannot
/// otherwise tell which index is chromatic, which drives R, G, and B, or
/// what to divide each by. Read together with `channels`/`chromatic`
/// (counts) and `projection`/`preserves` (a name and a caption), they are
/// what lets a client reproject `signal` itself — `(signal[projection_slots[i]]
/// / projection_norms[i]).clamp(0, 1)` per output slot, then the sRGB
/// transfer function (`kernel/src/color.rs`'s `encode_srgb_byte`) — rather
/// than merely caption a lost axis. All three are per-observer: a species
/// observer's roles, slots, and norms all differ from the standard
/// observer's.
///
/// **This covers the photopic path only.** [`hornvale_kernel::color::
/// Observer::to_srgb`] has a second, scotopic branch below the photopic
/// threshold that mixes in the rod response through global kernel constants
/// (`SCOTOPIC_GAIN`, `SCOTOPIC_NORM`) that are not carried on this wire, so a
/// cell that took that branch cannot be reprojected from `signal` and this
/// calibration alone — a client must fall back to the cell's own carried
/// `color` for it. See [`hornvale_kernel::color::Projection::rgb`] for the
/// same caveat stated on the kernel side.
/// type-audit: bare-ok(identifier-text: observer), bare-ok(count: channels), bare-ok(count: chromatic), bare-ok(identifier-text: projection), bare-ok(prose: preserves), bare-ok(diagnostic-value: sun_altitude_deg), bare-ok(identifier-text: channel_roles), bare-ok(index: projection_slots), bare-ok(ratio: projection_norms)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Sight {
    /// The species (or other named eye) the caller asserts this chart was
    /// coloured for — not knowable from an `Observer` alone, so it survives
    /// the builder's overwrite untouched.
    pub observer: String,
    /// How many channels the observer actually senses with.
    pub channels: u32,
    /// How many of those channels are chromatic (see
    /// [`hornvale_kernel::color::ChannelRole`]).
    pub chromatic: u32,
    /// The observer's projection name (`Projection::name`), or `"none"`
    /// when the observer carries no projection to sRGB.
    pub projection: String,
    /// What that projection preserves (`Projection::preserves`).
    pub preserves: String,
    /// The sun's elevation above the horizon, degrees — the caller's own
    /// datum; the builder does not know or check it.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sun_altitude_deg: f64,
    /// `"chromatic"` or `"achromatic"` per channel, in channel order (see
    /// [`hornvale_kernel::color::ChannelRole`]) — the calibration a client
    /// needs to know which index of a carried `signal` carries hue and
    /// which carries brightness only. Appended after `sun_altitude_deg`, the
    /// same additive-at-the-end discipline `sight`/`resolution` follow on
    /// [`SurroundsScene`]. Overwritten by the builder like `channels` and
    /// `chromatic` above — see this struct's own doc.
    pub channel_roles: Vec<String>,
    /// Which channel drives R, G, B (`Projection::rgb`), or `None` when the
    /// observer carries no projection to sRGB — half of the calibration
    /// `signal` needs to be reprojected. Overwritten by the builder, same as
    /// `projection`/`preserves` above.
    pub projection_slots: Option<[u32; 3]>,
    /// The per-output-slot normalizers (`Projection::norms`), or `None` when
    /// the observer carries no projection to sRGB — the **other** half of
    /// the calibration, and per-observer: a species observer's differ from
    /// the standard observer's (1.98/3.51/3.95). Without this, `signal[idx]
    /// / norm` cannot be computed at all — `projection_slots` alone says
    /// *which* index to read, not what to divide it by — so a client still
    /// could not reproject before this field existed. Overwritten by the
    /// builder, same as `projection_slots` above. A derived (non-`native`)
    /// observer's norms are computed live (`windows/worldgen/src/
    /// observer.rs::build`), not carried as a clean literal, so this is
    /// quantized at emit like every other computed float on this schema.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::opt_array3_f64_field")]
    pub projection_norms: Option<[f64; 3]>,
}

/// What resolution this chart's fields are decided at.
///
/// **Why a document says this at all.** A chart at walk depth sits six
/// refinement levels below the canonical grid, so a field decided per grid cell
/// is necessarily constant across the whole view — 4^6 rooms share one cell.
/// Read without this block, that flatness looks like the chart contradicting the
/// room's own prose ("open water" against "buttressed canopy, shaded, in a
/// hollow"); read with it, the flatness is the field's resolution stated out
/// loud. This is the same discipline [`Sight`] applies to colour, where
/// `preserves` names what the projection does *not* carry.
///
/// Declaring a resolution is deliberately NOT refining it. A campaign attempted
/// the refinement for `water` and reverted it: thresholding a blend of a
/// *nominal* field's underlay deletes categories and broke a calibrated coarse
/// statistic, where banding a blend of an *ordinal* field's underlay (which is
/// what `relief` does) moves a value at most one band. Sub-cell water belongs to
/// a hydrology model, not to this document.
/// type-audit: bare-ok(count: grid_level), bare-ok(count: depth_below_grid), bare-ok(identifier-text: grid_resolution_fields)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Resolution {
    /// The canonical grid's refinement level.
    pub grid_level: u32,
    /// How many levels below `grid_level` this chart's cells sit. Each level
    /// quarters a cell, so `4^depth_below_grid` rooms share one grid cell.
    pub depth_below_grid: u32,
    /// The names of this document's fields that are decided at grid resolution
    /// and therefore cannot vary below it, in stable order.
    pub grid_resolution_fields: Vec<String>,
}

/// One `scene/surrounds/v2` document. Field order is the JSON key order and
/// is contract — never reorder. `sight`, `resolution`, and `cover_legend` are
/// the exceptions to "never reorder" in letter only: each was appended after
/// the previous last field rather than inserted, so every document built
/// before it existed is still byte-identical. `#[serde(skip_serializing_if)]`
/// means an uncoloured document emits no `sight` key at all; `resolution` and
/// `cover_legend` carry no such gate and are always present — `cover_legend`
/// is a static catalog (like `biome_legend`/`water_legend`/`relief_legend`),
/// unconditionally cheap to emit even though the per-cell `cover` index it
/// backs is itself gated the same way `color` is.
///
/// **The rule this document follows for any future legend (fix round 1,
/// FINDING 5): a legend is an unconditional vocabulary declaration of the
/// schema version, not a per-document datum.** Its contents are a
/// compile-time constant, identical in every `scene/surrounds/v2` document
/// ever emitted, and as true of an uncoloured document as a coloured one —
/// so it is never gated, even when every per-cell index that resolves
/// against it is. A **per-cell index** may still be gated (`cover` follows
/// `color`'s `skip_serializing_if` discipline); the legend backing it may
/// not. Follow this precedent rather than re-arguing it.
/// type-audit: bare-ok(identifier-text: schema), bare-ok(constructor-edge: seed), bare-ok(diagnostic-value: day), bare-ok(count: radius), bare-ok(count: depth), bare-ok(identifier-text: orientation), bare-ok(identifier-text: biome_legend), bare-ok(identifier-text: water_legend), bare-ok(identifier-text: relief_legend), bare-ok(diagnostic-value: sea_level_m), bare-ok(identifier-text: cover_legend)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct SurroundsScene {
    /// Always `scene/surrounds/v2`.
    pub schema: String,
    /// The world's seed.
    pub seed: u64,
    /// The day observed.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub day: f64,
    /// Where the observer stands.
    pub observer: SurroundsObserver,
    /// Neighbourhood radius, in BFS rings.
    pub radius: u32,
    /// The refinement depth every cell sits at.
    pub depth: u32,
    /// Always `"lattice"`: the chart is lattice-aligned, never north-up. A
    /// consumer that wants north must ask the rooms for their bearings.
    pub orientation: String,
    /// The biome catalog, stable append-only order.
    pub biome_legend: Vec<String>,
    /// The water catalog, stable order.
    pub water_legend: Vec<String>,
    /// The relief catalog, stable ascending order.
    pub relief_legend: Vec<String>,
    /// This world's derived sea level, metres on the isostatic datum. The
    /// bands in `relief_legend` are measured from it, so a consumer can
    /// re-derive any cell's band from `height_asl_m` alone. Its absence from
    /// v1 left the one scene kind whose bands were wrong also the one kind a
    /// client could not correct.
    #[serde(serialize_with = "hornvale_kernel::quantize::quantize_serde::f64_field")]
    pub sea_level_m: f64,
    /// The cells, ascending by `room`.
    pub cells: Vec<SurroundsCell>,
    /// The chart's noun catalog, ascending by `noun`.
    pub legend: Vec<LegendEntry>,
    /// The eye this chart was coloured for, and what its projection
    /// preserves — absent (and the key entirely omitted) unless this scene
    /// was built through [`surrounds_scene_colored_in`]. Appended after
    /// `legend` rather than inserted, so an uncoloured document's bytes are
    /// unchanged by this field's existence.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sight: Option<Sight>,
    /// Which of this document's fields are decided at canonical-grid
    /// resolution and are therefore constant below it. Appended after
    /// `sight` so the change is additive to the wire.
    pub resolution: Resolution,
    /// The cover catalog, stable index order (`CoverClass::LEGEND`) — the
    /// legend a cell's `cover` index resolves against. Appended after
    /// `resolution` rather than beside the other three legends, so the
    /// change is additive to the wire; always present regardless of whether
    /// any cell in this document actually carries a `cover` index.
    pub cover_legend: Vec<String>,
}

/// Build the `scene/surrounds/v2` document for `room` at `radius` rings,
/// reusing a `LocaleContext` the caller already built. Fog-free: every cell
/// but the observer's is `"sensed"`.
///
/// Prefer this over [`surrounds_scene`] whenever a `LocaleContext` is
/// already in hand (e.g. a session-owning caller building one chart per
/// player turn): measured in release, `LocaleContext::build` costs ~1.19 s
/// against ~2 ms of this function's own per-cell work, so building a fresh
/// context per call would make a radius-0 chart cost the same as a
/// radius-8 one.
/// type-audit: bare-ok(count: radius)
pub fn surrounds_scene_in(
    world: &World,
    ctx: &LocaleContext,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
) -> Result<SurroundsScene, SceneError> {
    if radius > MAX_SURROUNDS_RADIUS {
        return Err(SceneError::SurroundsRadiusOutOfRange(radius));
    }
    let here = ctx
        .describe(room, at)
        .map_err(|e| SceneError::Build(e.to_string()))?;

    // Breadth-first over the mesh's edge-adjacency graph, out to `radius`
    // rings. BTreeSet/VecDeque only — no HashSet (determinism).
    let mut seen: BTreeSet<RoomAddr> = BTreeSet::new();
    let mut queue: VecDeque<(RoomAddr, u32)> = VecDeque::new();
    seen.insert(room.clone());
    queue.push_back((room.clone(), 0));
    let mut found: Vec<RoomAddr> = vec![room.clone()];
    while let Some((addr, ring)) = queue.pop_front() {
        if ring == radius {
            continue;
        }
        for n in addr.neighbors() {
            if seen.insert(n.clone()) {
                found.push(n.clone());
                queue.push_back((n, ring + 1));
            }
        }
    }

    let origin = room.face_lattice();
    let catalog = hornvale_climate::Biome::catalog();

    // Settlement marks, keyed by the room each settlement's coordinates land
    // in at this depth.
    let marks_by_room = settlement_marks(world, room.depth());

    let mut cells: Vec<SurroundsCell> = Vec::with_capacity(found.len());
    for addr in &found {
        let locale = ctx
            .describe(addr, at)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        let is_here = addr == room;
        let seam = addr.face != room.face;
        let lat = if seam {
            None
        } else {
            Some(addr.face_lattice())
        };
        let key = addr
            .pack()
            .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?
            .0;
        let mut marks = marks_by_room.get(&key).cloned().unwrap_or_default();
        // A cave is a CELL-level affordance, and marking every room of the
        // cell is faithful rather than sloppy: `delve` resolves its cave
        // from the cell the possession stands on
        // (`windows/vessel/src/session.rs::chamber_column_here`, which reads
        // the max-weight corner of the SAME fuzzy-resolved cell `describe`
        // uses — the dominant corner), so it already succeeds from any room
        // in a cave-bearing cell.
        //
        // ONE CAVEAT, so a reader of this file alone does not re-raise the
        // question: `chamber_column_here` breaks a weight TIE differently.
        // It picks with `max_by_key(|c| c.weight)`, which returns the LAST
        // maximum, where `LocaleContext::dominant_corner` breaks to the
        // lowest `CellId`. On an exact integer-weight tie — common on this
        // mesh, since a room sitting on a lattice point can weigh 64/64/64 —
        // the two can name different cells, so a marked cave and the cave
        // `delve` actually descends into can diverge. That divergence
        // predates the cave mark, reaches `column_here` (`dive`) the same
        // way, and is recorded rather than fixed here; it wants a ruling on
        // whether those two paths join `dominant_corner`'s coupling
        // invariant or are exempted in writing.
        //
        // Note also that `water` is NOT a counter-example to this pattern,
        // though an earlier draft of this comment said it was. `water` takes
        // the dominant corner of each ROOM's own three weights, which is
        // categorical nearest-neighbour interpolation and the correct method
        // for a nominal field; its flatness across a narrow view is the
        // interpolation stencil being wider than the view, not a defect. A
        // campaign refined it from a blend and reverted that (see the
        // `Resolution` block's doc).
        //
        // Salience 30 puts a cave mouth below both settlement ranks (10, 20)
        // in the legend, so a settlement outranks a cave mouth when both
        // stand on the same cell.
        if let Some(kind) = locale.cave {
            marks.push(Mark {
                noun: format!("a {} cave", kind.name()),
                kind: "cave".to_string(),
                datum: format!(
                    "A {} cave opens here — 'delve' descends into it.",
                    kind.name()
                ),
                salience: 30,
            });
        }
        marks.sort_by(|a, b| a.salience.cmp(&b.salience).then(a.noun.cmp(&b.noun)));
        cells.push(SurroundsCell {
            room: key,
            u: lat.map(|l| l.a - origin.a),
            v: lat.map(|l| l.b - origin.b),
            w: lat.map(|l| l.c - origin.c),
            up: lat.map(|l| l.up),
            seam,
            state: if is_here { "here" } else { "sensed" }.to_string(),
            biome: catalog
                .iter()
                .position(|e| *e == locale.biome_kind)
                .expect("every biome is in the catalog") as u32,
            water: u32::from(locale.fields.water.index()),
            relief: relief_band(locale.fields.height_asl_m),
            regime: is_here.then(|| locale.regime.descriptor.clone()),
            temperature_c: is_here.then_some(locale.fields.temperature_c),
            moisture: is_here.then_some(locale.fields.moisture),
            elevation_m: is_here.then_some(locale.fields.elevation_m),
            height_asl_m: is_here.then_some(locale.fields.height_asl_m.get()),
            // The default path never colours. `surrounds_scene_colored_in`
            // is the only writer, which is what keeps every committed
            // artifact byte-identical.
            color: None,
            micro: Micro {
                relief: locale.regime.micro.relief,
                aspect: locale.regime.micro.aspect,
                wetness: locale.regime.micro.wetness,
                openness: locale.regime.micro.openness,
            },
            marks,
            // Same posture as `color` above: only `surrounds_scene_colored_in`
            // populates `signal`/`cover`, so the default path's committed
            // artifacts stay byte-identical.
            signal: None,
            cover: None,
            // Option-free and present on every cell, seams included — the
            // north-up unblock (spec §5.1). `room`/`addr` are both already
            // `RoomAddr`s in scope; no lattice offset is needed.
            bearing_deg: hornvale_kernel::quantize(room.bearing_to(addr)),
            distance_rad: hornvale_kernel::quantize(room.distance_rad_to(addr)),
        });
    }
    cells.sort_by_key(|c| c.room);

    let legend = legend_of(&cells, &here, catalog);

    let observer_room = room
        .pack()
        .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?
        .0;

    Ok(SurroundsScene {
        schema: SURROUNDS_SCHEMA.to_string(),
        seed: world.seed.0,
        day: at.day(),
        observer: SurroundsObserver {
            room: observer_room,
            face: room.face,
            depth: room.depth(),
            latitude: here.latitude,
            longitude: here.longitude,
        },
        radius,
        depth: room.depth(),
        orientation: "lattice".to_string(),
        biome_legend: catalog.iter().map(|b| b.name().to_string()).collect(),
        water_legend: hornvale_terrain::WaterKind::LEGEND
            .iter()
            .map(|s| s.to_string())
            .collect(),
        relief_legend: RELIEF_LEGEND.iter().map(|s| s.to_string()).collect(),
        // Quantized at assignment, not just at serialization, so the emitted
        // datum agrees exactly with what a consumer re-derives from the
        // document (the same reasoning as `LocaleFields.height_asl_m`'s own
        // pre-quantization in `windows/locale`).
        sea_level_m: hornvale_kernel::quantize(ctx.terrain().globe().sea_level.get()),
        cells,
        legend,
        sight: None,
        resolution: Resolution {
            grid_level: ctx.globe_level(),
            depth_below_grid: room.depth() - ctx.globe_level(),
            // A cave mark is ALSO a dominant-corner fact (same `locale.cave`
            // read `biome`/`water` share) and is deliberately NOT added here.
            // This list names DOCUMENT FIELDS — actual `SurroundsCell` keys —
            // and there is no `cave` key: a cave surfaces only as one
            // possible `kind` inside `marks`, a field that ALSO carries
            // settlement marks keyed by the walking-depth room a
            // settlement's exact coordinates land in, which is finer than
            // the grid and genuinely varies below it. Listing `"marks"`
            // here would misstate that settlement half, and inventing a
            // `"cave"` entry would name a field that does not exist on the
            // wire. So the disclosure stays field-shaped and silent about
            // marks; `the_chart_declares_which_fields_are_grid_resolution`
            // pins this so a future change cannot add either by accident.
            //
            // `"color"` was REMOVED here by the illumination campaign's
            // Task 2b fix round (a wire VALUE change, not a shape change —
            // the `grid_resolution_fields` key itself is untouched, its
            // contents shrink by one entry). Before that campaign colour was
            // read from a room's dominant *canonical-grid* corner alone, so
            // it genuinely was constant below grid resolution and belonged
            // in this list. `surface::cover_weights` now composes in the
            // room's own `MicroField`, which — like `relief`, deliberately
            // NOT in this list — genuinely varies below the grid. Leaving
            // `"color"` here after that change would have this document
            // actively lie to a cross-repo client (`scene/surrounds/v2` is
            // a cross-repo contract): a client entitled to read one colour
            // per grid cell and reuse it would render a flat chart while the
            // sim disagrees.
            grid_resolution_fields: ["biome", "water"].iter().map(|s| s.to_string()).collect(),
        },
        // A static catalog (spec §4.3), unconditionally cheap — unlike the
        // per-cell `cover` index it backs, this needs no observer and no
        // colour layer, so it is populated here rather than only in
        // `surrounds_scene_colored_in`.
        cover_legend: CoverClass::LEGEND.iter().map(|s| s.to_string()).collect(),
    })
}

/// Build the `scene/surrounds/v2` document for `room` at `radius` rings.
/// Fog-free: every cell but the observer's is `"sensed"`.
///
/// Builds a fresh `LocaleContext` per call — a caller that already holds
/// one (or that will make more than one surrounds query, e.g. once per
/// player turn) should call [`surrounds_scene_in`] instead and hold the
/// context itself, since the rebuild dominates this function's cost.
///
/// The radius bound is checked here too, BEFORE `LocaleContext::build` —
/// measured in release, that build costs ~1.2 s, so an invalid radius must
/// fail before it, not after, or rejecting a bad argument would cost as
/// much as building a whole chart. `surrounds_scene_in` repeats the same
/// check, since it is public and must validate its own arguments
/// independently of this wrapper.
/// type-audit: bare-ok(count: radius)
pub fn surrounds_scene(
    world: &World,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
) -> Result<SurroundsScene, SceneError> {
    if radius > MAX_SURROUNDS_RADIUS {
        return Err(SceneError::SurroundsRadiusOutOfRange(radius));
    }
    let ctx = LocaleContext::build(world).map_err(|e| SceneError::Build(e.to_string()))?;
    surrounds_scene_in(world, &ctx, room, radius, at)
}

/// Build a `scene/surrounds/v2` document with a colour layer, as seen by
/// `observer` under the caller-supplied `light`.
///
/// A separate entry point rather than a parameter on
/// [`surrounds_scene_in`]: every committed artifact goes through the
/// uncoloured path, and this way they cannot move.
///
/// Cells whose observer has no truthful sRGB image keep `color: None` — the
/// mapping for a non-standard observer is a false-colour choice the caller
/// must declare (RENDER-9), not one this builder may invent.
///
/// This builder no longer computes its own light: it used to derive the
/// world star's daylight internally, which meant a caller could never
/// colour a chart under any other illuminant — dusk, an interior lantern, a
/// non-solar sky — without a second entry point. `light` is now the
/// caller's, and the returned document's `sight` block is how the choice is
/// disclosed rather than left implicit: `channels`, `chromatic`,
/// `projection`, and `preserves` are read from `observer` itself and
/// overwrite whatever the caller's `sight` argument claimed, so the
/// declaration cannot lie about the eye that was actually used. Only
/// `observer` (a species name, unknowable from an `Observer`) and
/// `sun_altitude_deg` (the caller's own datum) survive untouched — see
/// [`Sight`].
///
/// Eight parameters: the `world`/`ctx`/`room`/`radius`/`at` quintet is
/// shared with every other surrounds builder, and `observer`/`light`/
/// `sight` are the colour layer's own three — splitting either group into a
/// struct would just move the field count, not reduce it.
/// type-audit: bare-ok(count: radius)
#[allow(clippy::too_many_arguments)]
pub fn surrounds_scene_colored_in(
    world: &World,
    ctx: &LocaleContext,
    room: &RoomAddr,
    radius: u32,
    at: WorldTime,
    observer: &hornvale_kernel::color::Observer,
    light: &hornvale_kernel::color::Illuminant,
    sight: Sight,
) -> Result<SurroundsScene, SceneError> {
    let mut scene = surrounds_scene_in(world, ctx, room, radius, at)?;
    for cell in scene.cells.iter_mut() {
        let addr = hornvale_kernel::RoomId(cell.room)
            .unpack()
            .map_err(|e| SceneError::SurroundsUnaddressable(format!("{e:?}")))?;
        // `cell.micro` was already computed once, for every cell, in the
        // first pass this scene came from (`surrounds_scene_in`'s own build
        // loop, which drops the `Locale` it came from after copying this
        // struct-for-struct). Re-deriving it here via `ctx.describe(&addr,
        // at)` would rebuild an entire `Locale` — prose descriptor,
        // strangeness placement, and all — once per cell just to read four
        // floats already sitting on `cell`. Inverting the copy back into
        // `hornvale_locale::MicroField` is the cheap, correct alternative:
        // both are the same four axes in the same order, so this is a
        // straight field-for-field re-tag, not a re-derivation.
        let micro = hornvale_locale::MicroField {
            relief: cell.micro.relief,
            aspect: cell.micro.aspect,
            wetness: cell.micro.wetness,
            openness: cell.micro.openness,
        };
        let reflectance = ctx
            .reflectance_at(&addr, &micro, at)
            .map_err(|e| SceneError::Build(e.to_string()))?;
        // One `Signal`, used for both `color` and `signal` below — never
        // recomputed — so the migration control (projecting `signal` through
        // the observer must reproduce `color` byte-for-byte, spec §4.2) holds
        // by construction rather than by coincidence of two independent
        // computations agreeing.
        let signal = observer.sense(&reflectance, light);
        cell.color = observer.to_srgb(&signal);
        cell.signal = Some(signal.get().to_vec());
        cell.cover = Some(
            ctx.cover_class_at(&addr, &micro, at)
                .map_err(|e| SceneError::Build(e.to_string()))?
                .index(),
        );
    }
    scene.sight = Some(Sight {
        channels: observer.channels() as u32,
        chromatic: observer.chromatic_channels() as u32,
        projection: observer
            .projection()
            .map_or("none", |p| p.name())
            .to_string(),
        preserves: observer
            .projection()
            .map_or("nothing (no projection)", |p| p.preserves())
            .to_string(),
        channel_roles: observer
            .roles()
            .iter()
            .map(|r| match r {
                hornvale_kernel::color::ChannelRole::Chromatic => "chromatic".to_string(),
                hornvale_kernel::color::ChannelRole::Achromatic => "achromatic".to_string(),
            })
            .collect(),
        projection_slots: observer.projection().map(|p| p.rgb().map(|i| i as u32)),
        projection_norms: observer.projection().map(|p| *p.norms()),
        ..sight
    });
    Ok(scene)
}

/// Settlement marks keyed by the packed room id their coordinates fall in at
/// `depth`. The flagship outranks the rest.
fn settlement_marks(world: &World, depth: u32) -> BTreeMap<u64, Vec<Mark>> {
    let mut out: BTreeMap<u64, Vec<Mark>> = BTreeMap::new();
    for f in features_of(world) {
        let Feature {
            name,
            kind,
            latitude,
            longitude,
        } = f;
        let position = hornvale_kernel::math::unit_sphere_from_lat_lon(latitude, longitude);
        let Ok(id) = RoomAddr::containing(position, depth).pack() else {
            continue;
        };
        let flagship = kind == "flagship";
        out.entry(id.0).or_default().push(Mark {
            datum: if flagship {
                format!("{name} — the settlement this possession was minted from.")
            } else {
                format!("{name} — a settlement of this world.")
            },
            noun: name,
            kind: "settlement".to_string(),
            salience: if flagship { 10 } else { 20 },
        });
    }
    out
}

/// The chart's noun catalog: every mark's noun, plus one entry per distinct
/// terrain class drawn, plus the observer's own room. Biome nouns use the
/// spaced prose name ([`biome_prose_name`]), not the kebab-case identifier
/// `biome_legend` indexes into — the legend is player-facing text, and using
/// the prose name here makes the biome a noun shared with the prose
/// renderer's own catalog, joining the two grains on one datum (The Margin).
fn legend_of(
    cells: &[SurroundsCell],
    here: &Locale,
    catalog: &'static [hornvale_climate::Biome],
) -> Vec<LegendEntry> {
    let mut acc: BTreeMap<String, String> = BTreeMap::new();
    for c in cells {
        for m in &c.marks {
            acc.insert(m.noun.clone(), m.datum.clone());
        }
        let biome = catalog
            .get(c.biome as usize)
            .map(|b| biome_prose_name(*b).to_string())
            .unwrap_or_default();
        acc.entry(biome.clone()).or_insert_with(|| {
            format!(
                "{biome} — {} of the {} cells in view.",
                cells.iter().filter(|d| d.biome == c.biome).count(),
                cells.len()
            )
        });
    }
    acc.insert(
        here.regime.descriptor.clone(),
        format!(
            "The ground where you stand: {} (strangeness {:.0}).",
            here.regime.descriptor, here.regime.strangeness
        ),
    );
    acc.into_iter()
        .map(|(noun, datum)| LegendEntry { noun, datum })
        .collect()
}

/// Serialize a `SurroundsScene` to compact JSON (mirrors `scene_json`).
/// type-audit: bare-ok(artifact: return)
pub fn surrounds_json(scene: &SurroundsScene) -> String {
    serde_json::to_string(scene).expect("a surrounds scene serializes")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::place_latlon;
    use hornvale_kernel::{Seed, WorldTime};
    use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

    fn world() -> hornvale_kernel::World {
        build_world(
            Seed(42),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 42 builds")
    }

    fn observer(w: &hornvale_kernel::World) -> RoomAddr {
        let ctx = hornvale_locale::LocaleContext::build(w).unwrap();
        let depth = ctx.globe_level() + 6;
        // The flagship settlement's own room — the same place a possession
        // mints its agent, so the gallery scene shows the walked ground.
        let v = hornvale_settlement::village_info(w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(w, v.id).expect("the flagship has coordinates");
        RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon),
            depth,
        )
    }

    #[test]
    fn a_radius_four_neighbourhood_holds_thirty_one_cells() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime::GENESIS).unwrap();
        assert_eq!(s.schema, SURROUNDS_SCHEMA);
        assert_eq!(s.radius, 4);
        // Ball sizes in the triangular face-adjacency lattice are
        // 1 + 3k(k+1)/2: 1, 4, 10, 19, 31, ...
        assert_eq!(s.cells.len(), 31);
    }

    #[test]
    fn exactly_one_cell_is_here_and_it_sits_at_the_lattice_origin() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 3, WorldTime::GENESIS).unwrap();
        let here: Vec<&SurroundsCell> = s.cells.iter().filter(|c| c.state == "here").collect();
        assert_eq!(here.len(), 1);
        assert_eq!(
            (here[0].u, here[0].v, here[0].w),
            (Some(0), Some(0), Some(0))
        );
        assert_eq!(here[0].room, s.observer.room);
        assert!(!here[0].seam);
    }

    // The flagship observer's own neighbourhood never crosses a base face at
    // radius 4 (a coincidence of where seed 42 places its village) — so this
    // test alone never enters the `if c.seam` branch. It stays as coverage
    // of the no-seam case; `a_seam_observer_carries_no_coordinate_on_seam_cells`
    // below is the real seam-handling test.
    #[test]
    fn every_non_seam_cell_carries_a_lattice_coordinate_and_seam_cells_carry_none() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime::GENESIS).unwrap();
        for c in &s.cells {
            if c.seam {
                assert!(c.u.is_none() && c.v.is_none() && c.w.is_none() && c.up.is_none());
            } else {
                assert!(c.u.is_some() && c.v.is_some() && c.w.is_some() && c.up.is_some());
            }
        }
    }

    /// An observer verified to sit near a base-face seam (latitude -10°,
    /// longitude 0°, depth 12 lands on face 14), whose radius-4 neighbourhood
    /// genuinely crosses onto neighbouring faces — the real coverage for the
    /// seam branch the sibling test above never reaches. Uses the same
    /// lat/lon -> unit-sphere conversion as the `observer` helper above.
    #[test]
    fn a_seam_observer_carries_no_coordinate_on_seam_cells() {
        let w = world();
        let seam_observer = RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(-10.0, 0.0),
            12,
        );
        assert_eq!(
            seam_observer.face, 14,
            "fixture observer must land on the verified face"
        );
        let s = surrounds_scene(&w, &seam_observer, 4, WorldTime::GENESIS).unwrap();
        assert_eq!(s.cells.len(), 31, "no cell was dropped");

        let seam_count = s.cells.iter().filter(|c| c.seam).count();
        assert_ne!(
            seam_count, 0,
            "fixture observer must actually see seam cells, or this test is vacuous again"
        );
        assert_eq!(
            seam_count, 12,
            "verified fixture: 12 of 31 cells are seam cells at radius 4"
        );

        for c in &s.cells {
            if c.seam {
                assert!(
                    c.u.is_none() && c.v.is_none() && c.w.is_none() && c.up.is_none(),
                    "seam cell {} carries a lattice coordinate",
                    c.room
                );
            } else {
                assert!(
                    c.u.is_some() && c.v.is_some() && c.w.is_some() && c.up.is_some(),
                    "non-seam cell {} is missing a lattice coordinate",
                    c.room
                );
            }
        }
    }

    /// Step 6 of Task 9's brief: a seam cell has no honest lattice
    /// coordinate (`u`/`v`/`w`/`up` are all `None`), but it still carries a
    /// packed `room` id — a plain `u64`, not an `Option` — so `bearing_deg`
    /// and `distance_rad` must be computable and finite there too. This is
    /// the whole north-up unblock (spec §5.1/§5.2): a client that cannot
    /// draw a seam cell today gets a polar coordinate for it regardless of
    /// the lattice bending underneath. Reuses the exact verified
    /// seam-crossing fixture `a_seam_observer_carries_no_coordinate_on_seam_
    /// cells` establishes above (12 of 31 cells are seam cells at radius 4).
    #[test]
    fn a_seam_observer_still_carries_bearing_and_distance_on_seam_cells() {
        let w = world();
        let seam_observer = RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(-10.0, 0.0),
            12,
        );
        let s = surrounds_scene(&w, &seam_observer, 4, WorldTime::GENESIS).unwrap();
        let seam_cells: Vec<&SurroundsCell> = s.cells.iter().filter(|c| c.seam).collect();
        assert_ne!(
            seam_cells.len(),
            0,
            "fixture observer must actually see seam cells, or this test is vacuous"
        );
        for c in &seam_cells {
            assert!(
                c.bearing_deg.is_finite(),
                "seam cell {} has a non-finite bearing_deg",
                c.room
            );
            assert!(
                c.distance_rad.is_finite(),
                "seam cell {} has a non-finite distance_rad",
                c.room
            );
        }
        // A positive control: the observer's own cell (never a seam cell,
        // since it sits at ring 0 on its own face) has distance_rad == 0, so
        // a build that silently zeroed every cell's distance would still
        // pass an "is finite" check alone.
        assert!(
            seam_cells.iter().any(|c| c.distance_rad > 0.0),
            "every seam cell reported distance_rad == 0.0 — the field reads as a stub, \
             not a real great-circle distance"
        );
    }

    #[test]
    fn the_document_is_byte_identical_on_rebuild() {
        let w = world();
        let o = observer(&w);
        let a = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime::GENESIS).unwrap());
        let b = surrounds_json(&surrounds_scene(&w, &o, 4, WorldTime::GENESIS).unwrap());
        assert_eq!(a, b);
        // "Rebuild from the ledger": there is no `hornvale_worldgen::rebuild`
        // helper (confirmed absent workspace-wide) — the established pattern
        // for this exact assertion is a save/load round trip through the
        // world's own JSON save format (see e.g.
        // `windows/worldgen/src/lib.rs`'s
        // `generated_sky_round_trips_through_save_and_load`,
        // `kernel/src/world.rs`'s own round-trip test, and
        // `windows/explain/src/lib.rs`).
        let rebuilt = hornvale_kernel::World::from_json(&w.to_json())
            .expect("a world rebuilds from its own ledger");
        let c = surrounds_json(&surrounds_scene(&rebuilt, &o, 4, WorldTime::GENESIS).unwrap());
        assert_eq!(a, c, "same world + same query => byte-identical JSON");
    }

    #[test]
    fn the_radius_is_bounded_loudly() {
        let w = world();
        let e = surrounds_scene(
            &w,
            &observer(&w),
            MAX_SURROUNDS_RADIUS + 1,
            WorldTime::GENESIS,
        )
        .unwrap_err();
        assert_eq!(
            e,
            SceneError::SurroundsRadiusOutOfRange(MAX_SURROUNDS_RADIUS + 1)
        );
    }

    // `surrounds_scene_in` is public and must validate its own arguments
    // independently of the `surrounds_scene` wrapper's hoisted check above —
    // this pins that the inner function still rejects on its own, not just
    // because the wrapper happened to catch it first.
    #[test]
    fn the_inner_function_is_bounded_loudly_too() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let e = surrounds_scene_in(
            &w,
            &ctx,
            &observer(&w),
            MAX_SURROUNDS_RADIUS + 1,
            WorldTime::GENESIS,
        )
        .unwrap_err();
        assert_eq!(
            e,
            SceneError::SurroundsRadiusOutOfRange(MAX_SURROUNDS_RADIUS + 1)
        );
    }

    #[test]
    fn cells_are_ordered_by_room_id() {
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 4, WorldTime::GENESIS).unwrap();
        let ids: Vec<u64> = s.cells.iter().map(|c| c.room).collect();
        let mut sorted = ids.clone();
        sorted.sort_unstable();
        assert_eq!(ids, sorted, "cell order is contract: ascending room id");
    }

    /// The world star's daylight, the same illuminant the CLI's `colour`
    /// lens builds now that `surrounds_scene_colored_in` no longer computes
    /// it internally.
    fn daylight_for(w: &hornvale_kernel::World) -> hornvale_kernel::color::Illuminant {
        let star = hornvale_astronomy::star::generate_star(
            w.seed.derive(hornvale_astronomy::streams::ROOT),
        );
        hornvale_astronomy::illuminant::daylight(&star)
    }

    /// A `Sight` whose seven builder-owned fields are deliberately wrong
    /// placeholders — every caller of this helper is exercising a path that
    /// either overwrites them or doesn't care what they say, and a
    /// plausible-looking placeholder would hide a builder that forgot to
    /// overwrite.
    fn sight_of(observer: &str, sun_altitude_deg: f64) -> Sight {
        Sight {
            observer: observer.to_string(),
            channels: 0,
            chromatic: 0,
            projection: String::new(),
            preserves: String::new(),
            sun_altitude_deg,
            channel_roles: Vec::new(),
            projection_slots: None,
            projection_norms: None,
        }
    }

    /// A world, a `LocaleContext` built over it, and the flagship
    /// observer's room — the shared fixture the sight-declaration tests
    /// build on, reusing the module's existing `world()`/`observer()`
    /// helpers rather than adding a second world-builder.
    fn fixture_world() -> (
        hornvale_kernel::World,
        hornvale_locale::LocaleContext,
        RoomAddr,
    ) {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let room = observer(&w);
        (w, ctx, room)
    }

    fn colored(w: &hornvale_kernel::World, radius: u32) -> SurroundsScene {
        let ctx = hornvale_locale::LocaleContext::build(w).unwrap();
        let light = daylight_for(w);
        surrounds_scene_colored_in(
            w,
            &ctx,
            &observer(w),
            radius,
            WorldTime::GENESIS,
            &hornvale_kernel::color::standard_observer(),
            &light,
            sight_of("standard", 0.0),
        )
        .unwrap()
    }

    #[test]
    fn the_uncolored_builder_leaves_every_cell_without_a_color() {
        // This is what keeps book/src/gallery/scene-surrounds-seed-42.json
        // byte-identical: the field is skipped when None.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 2, WorldTime::GENESIS).unwrap();
        for cell in &s.cells {
            assert!(
                cell.color.is_none(),
                "the default builder invented a colour on room {}",
                cell.room
            );
        }
    }

    #[test]
    fn the_colored_builder_gives_placed_cells_a_color() {
        let w = world();
        let s = colored(&w, 2);
        let with = s.cells.iter().filter(|c| c.color.is_some()).count();
        assert_eq!(
            with,
            s.cells.len(),
            "{with} of {} cells received a colour — the standard observer has a \
             truthful sRGB image, so every placed cell must",
            s.cells.len()
        );
    }

    #[test]
    fn the_uncolored_json_emits_no_color_key() {
        // serde skip_serializing_if means an absent colour emits no key at
        // all, so the committed gallery JSON cannot move. Checked as a KEY
        // (`"color":`), not a bare substring: `resolution.grid_resolution_fields`
        // used to carry the string "color" as an array element (removed by
        // the illumination campaign's Task 2b fix round, FINDING 0 — see
        // `the_chart_declares_which_fields_are_grid_resolution`), which a
        // bare `"\"color\""` search would also have matched. Checked as a
        // KEY rather than a substring on principle, not because the
        // collision is live today.
        let w = world();
        let s = surrounds_scene(&w, &observer(&w), 1, WorldTime::GENESIS).unwrap();
        let json = crate::surrounds_json(&s);
        assert!(
            !json.contains("\"color\":"),
            "an absent colour still emitted a key"
        );
    }

    #[test]
    fn a_colored_document_does_emit_the_key() {
        // The negative test above is only meaningful if the key is emitted
        // when a colour IS present — otherwise `skip_serializing_if` could be
        // a blanket `serde(skip)` and nothing would notice.
        let w = world();
        let json = crate::surrounds_json(&colored(&w, 1));
        assert!(
            json.contains("\"color\""),
            "a coloured document dropped the key it was built to carry"
        );
    }

    #[test]
    fn coloring_is_deterministic_across_repeated_builds() {
        let w = world();
        let a: Vec<_> = colored(&w, 2).cells.iter().map(|c| c.color).collect();
        let b: Vec<_> = colored(&w, 2).cells.iter().map(|c| c.color).collect();
        assert_eq!(a, b);
    }

    /// A non-standard observer has no truthful sRGB image, so its cells keep
    /// `color: None` — a false-colour mapping is the caller's to declare
    /// (RENDER-9), never this builder's to invent.
    #[test]
    fn a_non_standard_observer_is_left_uncolored() {
        use hornvale_kernel::color::{Observer, Spectrum};
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        // Two channels: nothing sRGB can be made of.
        let dichromat = Observer::new(vec![
            Spectrum::new([1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap(),
            Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]).unwrap(),
        ])
        .unwrap();
        let light = daylight_for(&w);
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &observer(&w),
            2,
            WorldTime::GENESIS,
            &dichromat,
            &light,
            sight_of("dichromat", 0.0),
        )
        .unwrap();
        assert!(!s.cells.is_empty());
        for cell in &s.cells {
            assert!(
                cell.color.is_none(),
                "the builder invented an sRGB colour for a two-channel eye"
            );
        }
    }

    /// Everything else about a coloured chart must be the coloured chart's
    /// only difference: the colour layer is additive, and if it perturbed a
    /// biome index or a mark the committed artifacts would be at risk the
    /// moment anything switched builders. `sight` is stripped alongside
    /// `color` — `sight`, `color`, `signal`, and `cover` are all new,
    /// additive-only fields the coloured builder sets and the uncoloured one
    /// never does; `bearing_deg`/`distance_rad`/`cover_legend` are NOT
    /// stripped, because both builders set those identically (Task 9) and a
    /// divergence there is exactly what this test exists to catch.
    #[test]
    fn coloring_changes_nothing_but_the_color() {
        let w = world();
        let plain = surrounds_scene(&w, &observer(&w), 2, WorldTime::GENESIS).unwrap();
        let mut stripped = colored(&w, 2);
        for cell in stripped.cells.iter_mut() {
            cell.color = None;
            cell.signal = None;
            cell.cover = None;
        }
        stripped.sight = None;
        assert_eq!(plain, stripped);
    }

    /// Task 9's migration control (spec §4.2): for every cell that carries
    /// both `signal` and `color`, projecting `signal` back through the
    /// observer's own `to_srgb` must reproduce `color` byte-for-byte. This
    /// is spec's `RENDER-appearance-signal-protocol` control made strictly
    /// stronger — a live invariant inside ONE document, not a comparison
    /// across two schema versions.
    ///
    /// FINDING 3 (fix round 1): asserts on `serde_json::to_value(&s)` — the
    /// SERIALIZED, quantized wire form — rather than on `s.cells` directly.
    /// A client only ever reads the wire, and quantize-at-emit (decision
    /// 0033) means the in-memory `f64` and its serialized value are not
    /// guaranteed identical in general, even though they measure equal
    /// today (the emit-boundary rounding is well under a `u8` colour
    /// channel's resolution at this document's magnitudes). Asserting on
    /// the struct alone cannot see a future quantization regression widen
    /// that gap; asserting on the JSON can.
    ///
    /// The final assertion (`count > 0`) is not optional: an empty scene, or
    /// a scene whose cells all lack one of the two fields, would pass the
    /// loop above vacuously. `colored(&w, 2)` builds through the standard
    /// observer, which has a truthful sRGB image, so every one of its cells
    /// carries both fields — this asserts that population directly rather
    /// than trusting it.
    #[test]
    fn projecting_a_cells_signal_reproduces_its_colour_exactly() {
        let w = world();
        let s = colored(&w, 2);
        let obs = hornvale_kernel::color::standard_observer();
        let json = serde_json::to_value(&s).expect("the scene serializes");
        let cells = json["cells"].as_array().expect("cells is a JSON array");
        let mut count = 0;
        for cell in cells {
            let (Some(signal_json), Some(color_json)) = (cell.get("signal"), cell.get("color"))
            else {
                continue;
            };
            let signal: Vec<f64> = serde_json::from_value(signal_json.clone())
                .expect("the wire's signal is an array of numbers");
            let color: [u8; 3] = serde_json::from_value(color_json.clone())
                .expect("the wire's color is a 3-element byte array");
            let round = obs
                .to_srgb(&hornvale_kernel::color::Signal::from(signal))
                .expect("the standard observer projects every real signal it emitted");
            assert_eq!(
                round, color,
                "cell {} diverged under round-trip through the wire's own bytes",
                cell["room"]
            );
            count += 1;
        }
        assert!(
            count > 0,
            "the migration control ran over zero cells — the loop body never executed"
        );
        assert_eq!(
            count,
            cells.len(),
            "the standard observer has a truthful sRGB image, so every cell should have \
             carried both signal and color on the wire — {count} of {} did",
            cells.len()
        );
    }

    /// FINDING 2 (fix round 1): `cover` was unguarded in its own crate —
    /// mutating `cell.cover = Some(0)` left every `hornvale-scene` test,
    /// goldens included, green, because this crate's byte-goldens are all
    /// uncoloured and carry no `cover` key at all. This is the cheap,
    /// crate-local guard the finding asked for: every emitted `cover`
    /// index is in-bounds for `cover_legend`, and — the positive control —
    /// `cover` is not constant across a real band, so a mutation that
    /// collapses every cell to one class cannot pass silently.
    ///
    /// Reuses the exact walk-depth flagship band
    /// `the_color_now_varies_within_one_grid_cell_via_the_micro_field`
    /// measures colour variation over (radius 8, `globe_level + 6`):
    /// measured here, that band draws 2 distinct cover classes
    /// (`chlorophyll`, `litter`) across its cells.
    #[test]
    fn every_covers_index_is_in_bounds_and_cover_varies_across_a_real_band() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let gl = ctx.globe_level();
        let v = hornvale_settlement::village_info(&w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(&w, v.id).expect("the flagship has coordinates");
        let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);
        let light = daylight_for(&w);
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &RoomAddr::containing(pos, gl + 6),
            8,
            WorldTime::GENESIS,
            &hornvale_kernel::color::standard_observer(),
            &light,
            sight_of("standard", 0.0),
        )
        .unwrap();
        let mut seen = 0;
        let mut distinct: BTreeSet<u32> = BTreeSet::new();
        for cell in &s.cells {
            let Some(cover) = cell.cover else {
                continue;
            };
            assert!(
                (cover as usize) < s.cover_legend.len(),
                "cell {} carries cover index {cover}, out of bounds for a {}-entry legend",
                cell.room,
                s.cover_legend.len()
            );
            distinct.insert(cover);
            seen += 1;
        }
        assert!(
            seen > 0,
            "no cell carried a cover index at all — the loop body never executed"
        );
        assert!(
            distinct.len() > 1,
            "every one of {seen} cells carried the same cover index {distinct:?} — either \
             this band no longer varies (re-pick a fixture) or `cover_class_at` is not \
             reading the room's own micro-field the way the colour layer beside it does"
        );
    }

    /// **A colour chart at walking depth is now a HANDFUL of distinct
    /// colours, not one flat wash — that flatness was the H1 defect the
    /// illumination campaign's Task 2b exists to fix, and this test's own
    /// name and assertion predate that campaign.**
    ///
    /// Before Task 2b, rock class was the *only* colour input, read from the
    /// room's dominant *canonical-grid* corner
    /// (`LocaleContext::reflectance_at`), so a radius-8 walking-depth
    /// neighbourhood (109 cells, `globe_level + 6` — rooms roughly 64× finer
    /// per axis than a globe cell) reported one rock, one biome, one water
    /// kind, one relief band, and one colour: colour was exactly as
    /// spatially resolved as every OTHER categorical field the chart
    /// carried. Task 2b composes a surface-cover layer above the mineral
    /// mixture, weighted in part by each room's own [`Micro`] field — which,
    /// unlike rock class, biome, water kind or relief, is already emitted
    /// at ROOM grain (this module's own doc on [`SurroundsCell::micro`]) —
    /// so colour is now deliberately finer-grained than the categorical
    /// fields it used to match exactly. `walk_biomes == 1` still holds:
    /// nothing about *categorical* resolution moved, only colour's.
    ///
    /// The colour count is bounded, not unbounded, by design (spec §3's H1
    /// ceiling: a band must not read as address noise, "31 cells, 31
    /// colours"): `surface::cover_weights` bands the micro-field's three
    /// perturbing axes (aspect, openness, wetness) into three tiers each
    /// (`surface.rs::tier3`), so one climate regime can produce at most a
    /// handful of distinguishable mixtures, never a continuum. Measured on
    /// this world: a radius-8 walking-depth chart around the flagship now
    /// draws **3** distinct colours (was 1 pre-Task-2b); a radius-4
    /// grid-level chart still draws several more, unaffected (colour there
    /// was already varying with climate/lithology, not micro).
    #[test]
    fn the_color_now_varies_within_one_grid_cell_via_the_micro_field() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let gl = ctx.globe_level();
        let v = hornvale_settlement::village_info(&w).expect("seed 42 has a village");
        let (lat, lon) = place_latlon(&w, v.id).expect("the flagship has coordinates");
        let pos = hornvale_kernel::math::unit_sphere_from_lat_lon(lat, lon);

        let light = daylight_for(&w);
        let distinct = |depth: u32, radius: u32| -> (usize, usize) {
            let s = surrounds_scene_colored_in(
                &w,
                &ctx,
                &RoomAddr::containing(pos, depth),
                radius,
                WorldTime::GENESIS,
                &hornvale_kernel::color::standard_observer(),
                &light,
                sight_of("standard", 0.0),
            )
            .unwrap();
            let colors: BTreeSet<Option<[u8; 3]>> = s.cells.iter().map(|c| c.color).collect();
            let biomes: BTreeSet<u32> = s.cells.iter().map(|c| c.biome).collect();
            (colors.len(), biomes.len())
        };

        // At walking depth the whole neighbourhood is one grid cell — still
        // true of every CATEGORICAL field, but no longer true of colour.
        let (walk_colors, walk_biomes) = distinct(gl + 6, 8);
        assert!(
            walk_colors > 1,
            "a radius-8 walking-depth chart drew only {walk_colors} colour(s); \
             the micro-field modulation (surface::cover_weights) is absent or \
             is not reaching the colour layer"
        );
        // `9`, not a round "generous" number — chosen and PROVEN to fire on
        // the regression it names, not merely asserted to (Task 2b fix
        // round, FINDING 1). The tiered design (`surface.rs::tier3`) bounds
        // one climate regime to at most 3 (aspect) x 3 (openness) x 3
        // (wetness) = 27 combinations in the worst case, but this specific
        // band (unfrozen, so aspect's snow-only effect never activates)
        // measures 3 today. The design this guard exists to catch — reading
        // `aspect`/`openness`/`wetness` continuously instead of banding them
        // into tiers — was reinstated on this exact band as an experiment
        // and measured **18** distinct colours, comfortably under an
        // earlier, unproven `<= 20` ceiling (which is why that ceiling was
        // wrong: 15-18 colours from address noise passed it silently). `9`
        // sits strictly between the shipped design's 3 and the rejected
        // design's 18, and was confirmed to redden the continuous variant
        // and stay green on the shipped one before landing — see the Task
        // 2b fix-round report for both runs.
        assert!(
            walk_colors <= 9,
            "a radius-8 walking-depth chart drew {walk_colors} colours across \
             one climate cell — that reads as address noise (H1's ceiling), \
             not a bounded regime perturbation"
        );
        assert_eq!(
            walk_biomes, 1,
            "the fixture's premise moved: the biome is no longer constant here"
        );

        // At the grid's own level the same query crosses many cells, so the
        // colour varies — proving the flatness above is the GRAIN and not a
        // constant baked into the builder.
        let (coarse_colors, _) = distinct(gl, 4);
        assert!(
            coarse_colors > 1,
            "the builder returned one colour even across {coarse_colors} grid \
             cells — it is not reading lithology at all"
        );
    }

    #[test]
    fn the_emitted_relief_band_matches_the_emitted_height() {
        // THE test for this defect. A unit test on `relief_band` alone would pass
        // both before and after the fix, because what was wrong is which argument
        // the CALL SITE passes. This pins the band to the height in the same
        // document, so passing the raw reading again breaks it.
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let scene = surrounds_scene_in(&w, &ctx, &observer(&w), 2, WorldTime::GENESIS).unwrap();
        let mut checked = 0;
        for c in &scene.cells {
            if let Some(h) = c.height_asl_m {
                assert_eq!(
                    c.relief,
                    relief_band(SeaLevelHeight::from_metres(h)),
                    "room {} bands as {} but sits {h} m above sea level",
                    c.room,
                    scene.relief_legend[c.relief as usize],
                );
                checked += 1;
            }
        }
        assert!(
            checked > 0,
            "at least the observer's own cell carries a height"
        );
    }

    /// THE call-site guard. Reintroducing the original defect — banding
    /// `elevation_m` instead of `height_asl_m` — must fail this test.
    ///
    /// It exists because the obvious version was vacuous. `height_asl_m` is emitted
    /// only on the observer's own cell, so a self-consistency sweep checks exactly
    /// ONE cell; and the flagship room is at -0.2 m height over a -2936.4 m
    /// reading, which `relief_band` maps to `shelf` BOTH ways. A mutation test put
    /// the bug back and all 20 tests stayed green. The probe room must therefore be
    /// one where the two data actually disagree, and the `assert_ne!` below is what
    /// keeps that true if this world ever changes underneath the test.
    #[test]
    fn the_emitted_band_is_the_height_band_at_a_discriminating_room() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let globe = ctx.terrain().globe();
        let sea = globe.sea_level;

        // A land cell whose RAW reading is still negative: raw bands `shelf`, the
        // corrected height bands `lowland` or above, so the two disagree. The
        // majority of seed 42's land qualifies (8162 of 11,066 cells). Lowest
        // CellId wins, for determinism.
        let probe = globe
            .elevation
            .iter()
            .filter(|(_, e)| e.total_cmp(sea) != std::cmp::Ordering::Less && e.get() < 0.0)
            .map(|(c, _)| c)
            .next()
            .expect("seed 42 has land below the zero of the isostatic datum");
        let coord = ctx.climate().geosphere().coord(probe);
        let addr = RoomAddr::containing(
            hornvale_kernel::math::unit_sphere_from_lat_lon(coord.latitude, coord.longitude),
            ctx.globe_level() + 6,
        );

        let scene = surrounds_scene_in(&w, &ctx, &addr, 0, WorldTime::GENESIS).unwrap();
        let here = scene
            .cells
            .iter()
            .find(|c| c.state == "here")
            .expect("the observer's own cell is in the chart");
        let height = here.height_asl_m.expect("the `here` cell carries a height");
        let raw = here.elevation_m.expect("the `here` cell carries a reading");

        let height_band = relief_band(SeaLevelHeight::from_metres(height));
        let raw_band = relief_band(SeaLevelHeight::from_metres(raw));

        // ANTI-VACUITY. Without this the test can silently stop discriminating —
        // which is exactly how the first version of this guard passed while the
        // defect was live.
        assert_ne!(
            height_band, raw_band,
            "probe room is not discriminating: height {height} m and reading {raw} m \
             both band as {}; this test would pass with the defect reintroduced",
            RELIEF_LEGEND[height_band as usize]
        );

        assert_eq!(
            here.relief, height_band,
            "the emitted band is the RAW reading's band ({}) rather than the \
             height's ({}) — the datum defect is back",
            RELIEF_LEGEND[raw_band as usize], RELIEF_LEGEND[height_band as usize]
        );
    }

    #[test]
    fn the_document_carries_the_datum_its_bands_are_measured_from() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let scene = surrounds_scene_in(&w, &ctx, &observer(&w), 1, WorldTime::GENESIS).unwrap();
        assert_eq!(scene.schema, "scene/surrounds/v2");
        assert_eq!(
            scene.sea_level_m,
            hornvale_kernel::quantize(ctx.terrain().globe().sea_level.get()),
            "a client cannot re-derive a band without the datum"
        );
    }

    #[test]
    fn no_land_cell_bands_as_marine_relief() {
        // Guards `relief_band`'s THRESHOLD SEMANTICS, and nothing else. It computes
        // its own height and never calls the builder, so it CANNOT detect the
        // original defect — which was the argument the call site passed.
        // `the_emitted_band_is_the_height_band_at_a_discriminating_room` is the
        // call-site guard; a mutation test confirmed this one stays green with the
        // bug fully reintroduced. Kept because the thresholds are worth pinning,
        // labelled so nobody mistakes it for the detector.
        //
        // Stated over CELLS, where the invariant holds by definition: a land cell
        // IS one with `elevation >= sea_level`, so its height is >= 0 and its band
        // must be `lowland` or above. Deliberately NOT over rooms — a room's height
        // is a three-corner blend while its water kind is a point sample of the
        // dominant corner, so a shoreline room can be dry-land-dominant and still
        // blend centimetres below sea level (spec §12.4).
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let globe = ctx.terrain().globe();
        let sea = globe.sea_level;
        let mut land = 0usize;
        for (cell, e) in globe.elevation.iter() {
            if e.total_cmp(sea) == std::cmp::Ordering::Less {
                continue;
            }
            land += 1;
            let band = relief_band(e.above(sea));
            assert!(
                band >= 2,
                "land cell {cell:?} at {:.1} m ({:.1} m above sea level) banded as {}",
                e.get(),
                e.above(sea).get(),
                RELIEF_LEGEND[band as usize]
            );
        }
        assert!(
            land > 1000,
            "seed 42 has substantial land; got {land} cells"
        );
    }

    /// claim: invariant(forall-seed) — off-gate (heavy:); over [1,7,42,99,2026]
    #[test]
    #[ignore = "heavy: live-worldgen battery; deferred from the commit gate to the heavy set (decision 0132)"]
    fn no_land_cell_bands_as_marine_relief_across_seeds() {
        for seed in [1u64, 7, 42, 99, 2026] {
            let w = build_world(
                Seed(seed),
                &hornvale_astronomy::SkyPins::default(),
                SkyChoice::Generated,
                &hornvale_terrain::TerrainPins::default(),
                &SettlementPins::default(),
            )
            .expect("the seed builds");
            let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
            let globe = ctx.terrain().globe();
            let sea = globe.sea_level;
            for (cell, e) in globe.elevation.iter() {
                if e.total_cmp(sea) == std::cmp::Ordering::Less {
                    continue;
                }
                let band = relief_band(e.above(sea));
                assert!(
                    band >= 2,
                    "seed {seed}: land cell {cell:?} banded as {}",
                    RELIEF_LEGEND[band as usize]
                );
            }
            // The datum's distance from zero is what made this defect invisible;
            // print it so a future reader can see the spread across seeds.
            println!("seed {seed}: sea level {:.1} m", sea.get());
        }
    }

    #[test]
    fn an_uncoloured_document_is_byte_identical_to_one_built_before_sight_existed() {
        // `sight` and `color` are both skipped when absent, so the uncoloured
        // path must emit not one extra byte. This is what protects the three
        // committed gallery charts and the gallery scene JSON. `color` is
        // checked as a KEY (`"color":`), not a bare substring: `resolution.
        // grid_resolution_fields` used to carry the string "color" as an
        // array element (removed by the illumination campaign's Task 2b fix
        // round, FINDING 0), which a bare `"\"color\""` search would also
        // have matched. Checked as a KEY on principle, not because the
        // collision is live today.
        let (w, ctx, room) = fixture_world();
        let s = surrounds_scene_in(&w, &ctx, &room, 2, WorldTime::GENESIS).unwrap();
        let json = crate::surrounds_json(&s);
        assert!(
            !json.contains("\"sight\""),
            "uncoloured documents carry no sight block"
        );
        assert!(
            !json.contains("\"color\":"),
            "uncoloured documents carry no colour"
        );
    }

    #[test]
    fn the_sight_block_reports_the_observer_actually_used_not_the_one_claimed() {
        // A caller that lies about the projection must be corrected by the
        // builder, or the caption is unenforceable and RENDER-9's honesty is
        // decorative.
        let (w, ctx, room) = fixture_world();
        let obs = hornvale_kernel::color::standard_observer();
        let light =
            hornvale_astronomy::illuminant::daylight(&hornvale_astronomy::star::generate_star(
                w.seed.derive(hornvale_astronomy::streams::ROOT),
            ));
        let claimed = Sight {
            observer: "bugbear".to_string(),
            channels: 99,
            chromatic: 99,
            projection: "a lie".to_string(),
            preserves: "everything".to_string(),
            sun_altitude_deg: 12.5,
            channel_roles: vec!["a lie too".to_string()],
            projection_slots: Some([99, 99, 99]),
            projection_norms: Some([9.9, 9.9, 9.9]),
        };
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &room,
            2,
            WorldTime::GENESIS,
            &obs,
            &light,
            claimed,
        )
        .unwrap();
        let sight = s
            .sight
            .expect("a coloured document carries its declaration");
        assert_eq!(
            sight.projection, "native",
            "the builder overwrites the claim"
        );
        assert_eq!(sight.channels, 4);
        assert_eq!(sight.chromatic, 3);
        assert_eq!(
            sight.channel_roles,
            vec!["chromatic", "chromatic", "chromatic", "achromatic"],
            "the builder overwrites the claimed channel roles too"
        );
        assert_eq!(
            sight.projection_slots,
            Some([2, 1, 0]),
            "the builder overwrites the claimed projection slots too"
        );
        assert_eq!(
            sight.projection_norms,
            Some([3.95, 3.51, 1.98]),
            "the builder overwrites the claimed projection norms too"
        );
        // The two fields the builder CANNOT know are the caller's and survive.
        assert_eq!(sight.observer, "bugbear");
        assert_eq!(sight.sun_altitude_deg, 12.5);
    }

    /// Task 8: `Sight` must carry enough to interpret a `signal`, not just
    /// caption a lost axis. The cross-check is the point (per the task
    /// brief): two independently emitted facts about the same observer
    /// (`channels`/`chromatic`, the counts, and `channel_roles`, the
    /// per-channel detail) must agree, so a wrong one cannot pass quietly.
    #[test]
    fn sight_carries_enough_to_interpret_a_signal() {
        let w = world();
        let s = colored(&w, 2);
        let sight = s
            .sight
            .expect("a coloured document carries its declaration");
        assert_eq!(
            sight.channel_roles.len(),
            sight.channels as usize,
            "one role per channel"
        );
        assert_eq!(
            sight
                .channel_roles
                .iter()
                .filter(|r| *r == "chromatic")
                .count(),
            sight.chromatic as usize,
            "the chromatic role count must agree with the declared chromatic count"
        );
        // The standard observer carries a projection, so the wire's other
        // halves of the calibration — which channel drives which output
        // slot, and what to divide each by — must be present too.
        let slots = sight
            .projection_slots
            .expect("the standard observer carries a projection");
        for idx in slots {
            assert!(
                (idx as usize) < sight.channels as usize,
                "projection slot {idx} names a channel the observer does not have \
                 ({} channels)",
                sight.channels
            );
        }
        // FINDING 1 (fix round 1): `projection_slots` alone says which index
        // to read; without a normalizer beside it, a client still cannot
        // compute `signal[idx] / norm` at all. Every normalizer must be
        // finite and non-zero (`Projection::new`'s own validating
        // constructor already guarantees this at construction; this test
        // guards the WIRE copy of that guarantee, not the kernel's).
        let norms = sight
            .projection_norms
            .expect("the standard observer carries a projection");
        for norm in norms {
            assert!(
                norm.is_finite() && norm != 0.0,
                "projection norm {norm} is not usable as a divisor"
            );
        }
    }

    /// The dichromat fixture from `a_non_standard_observer_is_left_uncolored`
    /// has no projection, so `projection_slots` must say so honestly rather
    /// than carrying a stale or invented value — the same posture `color`
    /// itself takes for this observer.
    #[test]
    fn an_observer_with_no_projection_declares_no_projection_slots() {
        use hornvale_kernel::color::{Observer, Spectrum};
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let dichromat = Observer::new(vec![
            Spectrum::new([1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]).unwrap(),
            Spectrum::new([0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]).unwrap(),
        ])
        .unwrap();
        let light = daylight_for(&w);
        let s = surrounds_scene_colored_in(
            &w,
            &ctx,
            &observer(&w),
            2,
            WorldTime::GENESIS,
            &dichromat,
            &light,
            sight_of("dichromat", 0.0),
        )
        .unwrap();
        let sight = s
            .sight
            .expect("a coloured document carries its declaration");
        assert_eq!(sight.channel_roles.len(), 2);
        assert_eq!(
            sight.projection_slots, None,
            "an observer with no projection must declare no projection slots"
        );
        assert_eq!(
            sight.projection_norms, None,
            "an observer with no projection must declare no projection norms either"
        );
    }

    /// `micro` is emitted for EVERY cell, not just the observer's. The mutation
    /// that reintroduces an `is_here` gate must fail this.
    ///
    /// Stated over the count of cells whose micro is non-default rather than
    /// over a specific value, because the values are address noise and
    /// pinning one would pin the noise function rather than the emit.
    ///
    /// Not `#[ignore]`: every other single-seed-42-world-build test in this
    /// file (e.g. `a_radius_four_neighbourhood_holds_thirty_one_cells`) runs
    /// in the ordinary gate; `cli/tests/heavy_tier.rs`'s `heavy:` token is
    /// reserved for the one test in this file that sweeps multiple seeds
    /// (`no_land_cell_bands_as_marine_relief_across_seeds`), and tagging a
    /// single-world test with it would fail that guard's canonical-string
    /// check anyway.
    #[test]
    fn every_cell_carries_its_own_micro_field() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let here = observer(&w);
        let s =
            surrounds_scene_in(&w, &ctx, &here, 4, WorldTime::GENESIS).expect("the chart builds");

        // Every cell, including the 30 that are not `here`.
        assert_eq!(
            s.cells.len(),
            31,
            "the fixture's premise moved: a radius-4 chart is no longer 31 cells"
        );

        // The four axes are independent sub-streams, so distinct rooms give
        // distinct tuples. A single shared value would mean the field was read
        // once and copied.
        let distinct: std::collections::BTreeSet<String> = s
            .cells
            .iter()
            .map(|c| {
                format!(
                    "{},{},{},{}",
                    c.micro.relief, c.micro.aspect, c.micro.wetness, c.micro.openness
                )
            })
            .collect();
        assert!(
            distinct.len() > 25,
            "31 cells produced only {} distinct micro tuples; the field is being \
             shared rather than derived per room",
            distinct.len()
        );
    }

    /// The spec's H3, as a test: `openness` spans more than half of [-1, 1]
    /// within one neighbourhood. This is the property that makes the field worth
    /// drawing, and it is allowed to fail.
    #[test]
    fn micro_openness_spans_more_than_half_its_range_in_one_neighbourhood() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let here = observer(&w);
        let s =
            surrounds_scene_in(&w, &ctx, &here, 4, WorldTime::GENESIS).expect("the chart builds");

        let mut lo = f64::INFINITY;
        let mut hi = f64::NEG_INFINITY;
        for c in &s.cells {
            lo = lo.min(c.micro.openness);
            hi = hi.max(c.micro.openness);
        }
        assert!(
            hi - lo > 1.0,
            "openness spanned only {:.3} of its 2.0 range ({lo:.3}..{hi:.3}); a \
             glyph keyed to it would barely vary",
            hi - lo
        );
    }

    #[test]
    fn a_dimmer_light_yields_dimmer_colour() {
        // The caller-supplied illuminant must actually reach the pixels — the
        // positive control for Task 4's H4.
        let (w, ctx, room) = fixture_world();
        let obs = hornvale_kernel::color::standard_observer();
        let bright =
            hornvale_kernel::color::Illuminant::new([1.0; hornvale_kernel::color::BANDS]).unwrap();
        let dim =
            hornvale_kernel::color::Illuminant::new([0.2; hornvale_kernel::color::BANDS]).unwrap();
        let mk = |l| {
            surrounds_scene_colored_in(
                &w,
                &ctx,
                &room,
                2,
                WorldTime::GENESIS,
                &obs,
                l,
                sight_of("standard", 0.0),
            )
            .unwrap()
        };
        let (a, b) = (mk(&bright), mk(&dim));
        let lit: Vec<_> = a.cells.iter().filter_map(|c| c.color).collect();
        assert!(!lit.is_empty(), "the probe must find coloured cells at all");
        let mut moved = 0;
        for (x, y) in a.cells.iter().zip(&b.cells) {
            if let (Some(p), Some(q)) = (x.color, y.color)
                && q[0] < p[0]
            {
                moved += 1;
            }
        }
        assert!(
            moved > 0,
            "dimming the illuminant must darken at least one cell"
        );
    }

    /// The chart declares which of its fields are decided at canonical-grid
    /// resolution and are therefore constant below it.
    ///
    /// This exists because the chart's flatness at walk depth was read as a
    /// contradiction with the room's own prose, and the document gave a reader no
    /// way to tell "this area is uniform" from "this field does not resolve here".
    /// Same disclosure discipline `Sight::preserves` already applies to colour.
    #[test]
    fn the_chart_declares_which_fields_are_grid_resolution() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();
        let here = observer(&w);
        let s =
            surrounds_scene_in(&w, &ctx, &here, 4, WorldTime::GENESIS).expect("the chart builds");

        assert_eq!(s.resolution.grid_level, ctx.globe_level());
        assert_eq!(
            s.resolution.depth_below_grid,
            here.depth() - ctx.globe_level()
        );
        assert!(
            s.resolution
                .grid_resolution_fields
                .contains(&"water".to_string()),
            "water is decided at the dominant corner, so it must be declared"
        );
        assert!(
            s.resolution
                .grid_resolution_fields
                .contains(&"biome".to_string()),
            "biome is decided at the dominant corner, so it must be declared"
        );
        assert!(
            !s.resolution
                .grid_resolution_fields
                .contains(&"relief".to_string()),
            "relief is banded from the blend and DOES vary below the grid"
        );
        // The illumination campaign, Task 2b fix round (FINDING 0): before
        // that campaign, colour was read from a room's dominant
        // canonical-grid corner alone (bedrock only), so it genuinely
        // belonged in this list. `surface::cover_weights` now composes in
        // the room's own MicroField, which — like `relief` above — varies
        // below the grid, so declaring `"color"` here would state a
        // falsehood a cross-repo client is entitled to optimise on (read
        // one colour per grid cell and reuse it). This assertion did NOT
        // exist before the fix round; its absence is exactly why removing
        // `"color"` from the production array did not turn this test red —
        // nothing here was checking for it. Added now so a future
        // regression (re-adding `"color"` to the list) is caught.
        assert!(
            !s.resolution
                .grid_resolution_fields
                .contains(&"color".to_string()),
            "color now varies below grid resolution (the room's own MicroField), \
             so it must NOT be declared grid-resolution — a cross-repo client is \
             entitled to read a declared field as constant below the grid"
        );
        // The Grain, Task 3: a cave is ALSO a dominant-corner fact, and it is
        // deliberately NOT declared here — see the comment at this field's
        // construction site. `"marks"` (the actual document field a cave
        // surfaces through) ALSO carries settlement marks, which are keyed
        // by the walking-depth room a settlement's exact coordinates land
        // in and genuinely vary below the grid — so declaring `"marks"`
        // would misstate that half, and `"cave"` names no field that exists
        // on the wire at all. Pinned so a future change cannot add either by
        // accident without someone reading why it isn't already here.
        assert!(
            !s.resolution
                .grid_resolution_fields
                .contains(&"cave".to_string()),
            "\"cave\" names no document field — it is a mark kind, not a key"
        );
        assert!(
            !s.resolution
                .grid_resolution_fields
                .contains(&"marks".to_string()),
            "marks mixes a grid-resolution fact (cave) with a finer one \
             (settlement), so declaring the whole field would misstate the \
             settlement half"
        );
    }

    /// The Grain, Task 3: a chart centred on a cave-bearing cell emits
    /// exactly one `"cave"` mark, on the observer's own cell; a chart
    /// centred elsewhere emits none.
    ///
    /// **Direction this proves, and the direction it does not.** This checks
    /// *emitted ⊆ real*: every cave mark this test finds corresponds to a
    /// real `locale.cave` reading. It does NOT check the converse (*real ⊆
    /// emitted*) — it does not sweep every cave-bearing cell on the globe to
    /// confirm each one gets a mark somewhere in some chart. A cave the
    /// terrain has and the scene omits would not be caught by this test.
    #[test]
    fn a_cave_bearing_cell_emits_exactly_one_cave_mark_and_elsewhere_emits_none() {
        let w = world();
        let ctx = hornvale_locale::LocaleContext::build(&w).unwrap();

        // Find a room address whose dominant corner the terrain places a
        // cave at, the same directional-sweep idiom
        // `windows/locale`'s `describe_reports_the_cave_at_its_dominant_corner_on_seed_42`
        // uses (a direct aim at a cell's centroid does not reliably resolve
        // to that cell as the dominant corner).
        let mut cave_addr = None;
        for i in 0..2000u32 {
            let t = i as f64;
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, ctx.globe_level() + 6);
            if let Ok(loc) = ctx.describe(&addr, WorldTime::GENESIS)
                && loc.cave.is_some()
            {
                cave_addr = Some(addr);
                break;
            }
        }
        let cave_addr = cave_addr.expect(
            "seed 42 must have a reachable cave findable by this sweep — if this \
             fails, that is a finding to report, not a test to weaken",
        );

        let s = surrounds_scene_in(&w, &ctx, &cave_addr, 0, WorldTime::GENESIS).unwrap();
        assert_eq!(s.cells.len(), 1, "radius 0 is just the observer's own cell");
        let cave_marks: Vec<&Mark> = s.cells[0]
            .marks
            .iter()
            .filter(|m| m.kind == "cave")
            .collect();
        assert_eq!(
            cave_marks.len(),
            1,
            "exactly one cave mark must be emitted on a cave-bearing cell, got {cave_marks:?}"
        );
        assert_eq!(cave_marks[0].salience, 30);

        // Elsewhere: a chart centred on the flagship settlement's own room
        // Elsewhere: a chart centred on a cave-FREE room emits no cave mark.
        //
        // The room is found by the same directional sweep as the positive
        // half, rather than taken from the module's `observer()` fixture (the
        // flagship settlement's room). That fixture reached this branch only
        // because the flagship happened to stand on a cave-free cell, and the
        // terrain epoch of decision 0134 put a cave under it — the assertion
        // guarding against exactly that vacuity is what went red. A sweep
        // reaches the negative branch directly, so no re-pin can silently
        // restore a lucky sample: the branch is now selected for, not
        // inherited.
        let mut clear_addr = None;
        for i in 0..2000u32 {
            let t = i as f64;
            let dir = [
                hornvale_kernel::math::cos(t * 0.017),
                hornvale_kernel::math::sin(t * 0.023) * 0.5,
                hornvale_kernel::math::cos(t * 0.031),
            ];
            let addr = RoomAddr::containing(dir, ctx.globe_level() + 6);
            if let Ok(loc) = ctx.describe(&addr, WorldTime::GENESIS)
                && loc.cave.is_none()
            {
                clear_addr = Some(addr);
                break;
            }
        }
        let elsewhere = clear_addr.expect(
            "seed 42 must have a reachable cave-free cell findable by this sweep — a \
             world caved everywhere is a finding to report, not a test to weaken",
        );
        let s2 = surrounds_scene_in(&w, &ctx, &elsewhere, 0, WorldTime::GENESIS).unwrap();
        assert!(
            s2.cells[0].marks.iter().all(|m| m.kind != "cave"),
            "a cave-free cell must emit no cave mark"
        );
    }
}
