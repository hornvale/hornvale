//! Our own mirror of `vessel/session/v2`.
//!
//! Deliberately NOT the producer's types. `windows/vessel` derives
//! `Serialize` only, and a mirror written by an outsider is a stronger proof
//! that the emitted contract is sufficient than a derive would be — the same
//! role `clients/vessel/src/snapshot.ts` plays for the browser client.
//!
//! **`social` is omitted on purpose.** serde ignores unknown keys, so the
//! channel never enters this crate's address space. See The Quire spec §6.
//!
//! **The whole `sensed` channel is omitted too, and for a different reason.**
//! Task 9b mirrored `sensed` to reach `locale/room/v2`'s `exits`
//! (`Room`/`Exit`/`Direction`/`Compass`/`ExitKind`) for a dedicated ways-on
//! row. Task 9d deleted that row (see `entry.rs`'s module doc and decision
//! 0117) and with it every field mirrored solely to feed it, leaving `sensed`
//! holding one field — `sky` — which no draw path, test or example in either
//! crate has ever read, and whose content the sim already puts verbatim inside
//! `narration.prose`, which this client renders. That is the duplication 0117
//! forbids, so the channel went the same way `Sensed.room` did.
//!
//! **What "mirror only what a component needs" does and does not license.** It
//! is a rule about *channels*, not about every leaf field. A whole top-level
//! channel with no reader is carried weight and comes out. Fields *within* a
//! record this crate does read — `ChartCell`'s lattice coordinates,
//! `PaletteEntry::color` — stay even when nothing reads them yet, because the
//! mirror's other job is to be a faithful model of the wire record, and a
//! partial record is a worse proof that the emitted contract is sufficient.
//! Individual unread fields are called out at their own doc comments.
//!
//! `Snapshot` and its fields derive `Serialize` in addition to `Deserialize`
//! purely so `tests/schema.rs` can round-trip a parsed value back to a
//! `serde_json::Value` and inspect its keys — that is the mechanism the
//! redaction test uses to prove `social` never reaches this crate's address
//! space. Nothing in this crate itself re-serializes a `Snapshot`.

use serde::{Deserialize, Serialize};

/// One committed turn, as this client is willing to see it.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Snapshot {
    /// Schema tag; must be `vessel/session/v2`.
    pub schema: String,
    /// Turn counter; 0 is the opening.
    pub turn: u64,
    /// The observed day, in absolute standard days.
    pub day: f64,
    /// Who the player is. `self` is a Rust keyword on the wire.
    #[serde(rename = "self")]
    pub me: SelfChannel,
    /// The sim's own rendering.
    pub narration: Narration,
    /// Where the possession stands, as cells.
    pub spatial: Spatial,
    // `social` is NOT mirrored. Do not add it.
    // `known` is not mirrored either: no component in this campaign renders
    // it. Add it when a component needs it, not before.
    // `sensed` is not mirrored either — see the module doc's note on the
    // ways-on removal.
}

/// The possessed agent's identity.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct SelfChannel {
    /// The agent's minted id, as a decimal string (it exceeds 2^53).
    pub agent: String,
    /// The species whose perception this agent carries.
    pub species: String,
    /// The settlement the agent was minted from.
    pub settlement: String,
    /// How many live there.
    pub population: u32,
}

/// The sim's own rendering of this turn.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Narration {
    /// The passage, carried verbatim — prose is the constitutional primary
    /// and this client never re-derives it.
    pub prose: String,
    /// The examinable noun catalog, in prose order. The join key to the
    /// plate's marks and legend.
    pub nouns: Vec<NounEntry>,
}

/// One examinable noun and its datum.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct NounEntry {
    /// The noun as the prose mentions it.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
    /// Coarse kind (`creature`/`place`/`thing`/`unknown`) for completion.
    #[serde(default)]
    pub kind: String,
}

/// The band-tagged spatial channel. The wire tag is `band`.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(tag = "band", rename_all = "lowercase")]
pub enum Spatial {
    /// Not inside a built structure: the walk-band chart.
    Walk {
        /// `scene/surrounds/v2`.
        chart: Chart,
    },
    /// Inside a building: the chamber-band floor plan.
    Chamber {
        /// `vessel/plan/v1`.
        plan: Plan,
    },
    /// Inside a generated cave descent (The Gallery, Task 9; spec §4).
    Underground {
        /// `vessel/level/v1`.
        level: Level,
    },
}

/// One cell of the walk-band chart, mirroring `scene/surrounds/v2`'s
/// `SurroundsCell`.
///
/// Not every field here has a reader, and since the chart went north-up the
/// unread set is larger: the four lattice offsets ([`ChartCell::u`],
/// [`ChartCell::v`], [`ChartCell::w`], [`ChartCell::up`]) and `seam` are all
/// carried for record fidelity alone. `chart.rs` places a cell from
/// `bearing_deg` and `distance_rad` now, which is what let it stop skipping
/// seam cells. That is the deliberate line the module doc draws — a whole
/// unread *channel* comes out, a leaf field of a record this crate does read
/// stays.
///
/// The brief's field list put `marks` on [`Chart`] rather than here; the
/// producer (`windows/scene/src/surrounds.rs`) has no such top-level field —
/// `SurroundsScene` carries `cells`/`legend` and nothing named `marks`, while
/// `SurroundsCell` (this type's model) does. Mirrored where the wire
/// actually puts it, per "read the producer, do not guess."
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct ChartCell {
    /// Lattice offset from the observer on axis 0; `null` on a seam cell.
    pub u: Option<i64>,
    /// Lattice offset on axis 1; `null` on a seam cell.
    pub v: Option<i64>,
    /// Always `null` since the occupancy lattice became a cube-sphere quad
    /// (decision 0513) — the third barycentric axis a triangular mesh
    /// needed and a quad does not. Retained on the wire for schema
    /// stability, not for signal: `u`/`v` above still carry real per-cell
    /// values, only `w` and `up` are permanently null now, so do not infer
    /// "all four lattice fields move together" from the old mesh.
    pub w: Option<i64>,
    /// Always `null` since the occupancy lattice became a cube-sphere quad
    /// (decision 0513) — the triangle-orientation flag a quad has no use
    /// for (a quad's children all share its handedness). See
    /// [`ChartCell::w`].
    pub up: Option<bool>,
    /// Set when this cell lies on a different base face than the observer.
    pub seam: bool,
    /// `"here"`, `"sensed"`, or `"remembered"`.
    pub state: String,
    /// Index into `biome_legend`.
    pub biome: u32,
    /// Index into `water_legend`.
    pub water: u32,
    /// Index into `relief_legend`.
    pub relief: u32,
    /// The cell's colour as seen through the observing eye, present only when
    /// the scene was built through the producer's coloured builder — absence
    /// means "no colour claimed here", never black.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
    /// The sub-cell micro-field at this room, mirroring
    /// `scene/surrounds/v2`'s `Micro`. `chart.rs`'s impedance ladder (The
    /// Legend, Task 8) reads [`Micro::relief`] and [`Micro::openness`];
    /// `aspect` and `wetness` are carried for record fidelity alone (module
    /// doc: a leaf field of a record this crate reads stays, even unread).
    pub micro: Micro,
    /// Salience-ranked things standing here.
    pub marks: Vec<Mark>,
    /// Great-circle initial azimuth from the observer to this cell, degrees
    /// clockwise from north. Present on every cell including a seam one,
    /// unlike the lattice offsets above — this and `distance_rad` are the
    /// cell's polar coordinate about the observer, and the whole of what
    /// `chart.rs` needs to place it.
    pub bearing_deg: f64,
    /// Great-circle angular distance from the observer to this cell,
    /// radians. See `bearing_deg`.
    pub distance_rad: f64,
}

/// The walk-band chart, mirroring `scene/surrounds/v2`'s `SurroundsScene`.
/// The producer's whole record, minus channels this crate omits by rule
/// (module doc); not every field has a reader.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Chart {
    /// Neighbourhood radius, in BFS rings.
    pub radius: u32,
    /// The refinement depth every cell sits at.
    pub depth: u32,
    /// The biome catalog, stable append-only order.
    pub biome_legend: Vec<String>,
    /// The water catalog, stable order.
    pub water_legend: Vec<String>,
    /// The relief catalog, stable ascending order.
    pub relief_legend: Vec<String>,
    /// The cells, ascending by `room`.
    pub cells: Vec<ChartCell>,
    /// One `(noun, datum)` pair of the chart's catalog.
    pub legend: Vec<LegendEntry>,
    /// The chart's sight declaration (`scene/surrounds/v2`'s `sight` block),
    /// present only when the scene is coloured — an uncoloured scene omits
    /// the key entirely, so this is `Option` with a serde default and is
    /// skipped when `None` on the way back out.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sight: Option<Sight>,
}

/// The walk-band chart's sight declaration, mirroring `scene/surrounds/v2`'s
/// `Sight`. The fields the disclosure caption reads (`observer`,
/// `projection`, `preserves`) plus the rest of the wire record — the mirror's
/// other job is to be a faithful model of the emitted contract, and a partial
/// record is a worse proof that it is sufficient (module doc).
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Sight {
    /// The species (or other named eye) the chart was coloured through.
    pub observer: String,
    /// How many channels the observer senses with.
    pub channels: u32,
    /// How many of those are chromatic.
    pub chromatic: u32,
    /// The observer's projection name, or `"none"`.
    pub projection: String,
    /// What that projection preserves — and, read honestly, what it does not.
    pub preserves: String,
    /// The sun's elevation above the horizon, degrees, the light was built
    /// from.
    pub sun_altitude_deg: f64,
    /// `"chromatic"` or `"achromatic"` per channel, in channel order.
    pub channel_roles: Vec<String>,
    /// Which channel drives R, G, B, or `None` with no projection.
    pub projection_slots: Option<[u32; 3]>,
    /// The per-output-slot normalizers, or `None` with no projection.
    pub projection_norms: Option<[f64; 3]>,
}

/// A cell's sub-cell micro-field, mirroring `scene/surrounds/v2`'s `Micro`.
/// Every axis is `[-1, 1]`; `chart.rs`'s impedance ladder reads `relief` and
/// `openness` — see [`ChartCell::micro`].
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Micro {
    /// Micro-relief, hollow (`-1`) to rise (`+1`). Read by the impedance
    /// ladder as roughness: `|relief|`, since a hollow and a rise are
    /// equally uneven underfoot.
    pub relief: f64,
    /// Slope aspect / insolation, shaded (`-1`) to sunlit (`+1`). Not read
    /// by the impedance ladder — it says which way a slope faces, not how
    /// hard the ground is to cross.
    pub aspect: f64,
    /// Local wetness, dry (`-1`) to wet (`+1`). Not read by the impedance
    /// ladder.
    pub wetness: f64,
    /// Canopy openness, closed (`-1`) to open (`+1`). Read by the impedance
    /// ladder as canopy: `(1 - openness) / 2`.
    pub openness: f64,
}

/// A salience-ranked thing standing on a cell.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Mark {
    /// The examinable noun.
    pub noun: String,
    /// What kind of thing this is: `"settlement"` or `"agent"`.
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
}

/// One `(noun, datum)` pair of a chart's or plan's noun catalog.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LegendEntry {
    /// The examinable noun.
    pub noun: String,
    /// What `examine` prints for it.
    pub datum: String,
}

/// The chamber-band floor plan, mirroring `vessel/plan/v1`'s `SessionPlan`.
/// Task 9b briefly added `at`/`of` to reimplement the sim's own path-graph
/// invariant for a ways-on row The Quire (task 9d) removed — see `entry.rs`'s
/// module doc. What remains is the producer's record; not every field of it
/// has a reader (`PaletteEntry::color` has none — monochrome is the whole
/// visual channel in this campaign).
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Plan {
    /// The plan's bounds.
    pub extent: PlanExtent,
    /// The distinct cell types, in first-seen row-major order.
    pub palette: Vec<PaletteEntry>,
    /// One palette index per cell, row-major. Length is exactly `w * h`.
    pub cells: Vec<u32>,
    /// The cell the possession stands in.
    pub you: PlanPoint,
    /// The individuals standing on the plan.
    pub marks: Vec<PlanMark>,
}

/// The plan's bounds, in lattice-local cells.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PlanExtent {
    /// Left edge.
    pub x: i32,
    /// Top edge.
    pub y: i32,
    /// Width, in cells.
    pub w: i32,
    /// Height, in cells.
    pub h: i32,
}

/// One distinct cell type in a plan.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PaletteEntry {
    /// `"wall"`, `"floor"` or `"threshold"`.
    pub kind: String,
    /// Which chambers this cell type serves.
    pub chambers: Vec<usize>,
    /// The cell type's display colour, absent when none can be claimed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub color: Option<[u8; 3]>,
}

/// One lattice-local cell position.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PlanPoint {
    /// Column.
    pub x: i32,
    /// Row.
    pub y: i32,
}

/// A single individual standing on a cell of the plan.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct PlanMark {
    /// Column, lattice-local.
    pub x: i32,
    /// Row, lattice-local.
    pub y: i32,
    /// The examinable noun, shared with the prose's own noun catalog.
    pub noun: String,
    /// What kind of thing this is: `"settlement"`, `"agent"`, …
    pub kind: String,
    /// One line about it — the datum `examine` prints.
    pub datum: String,
    /// Rank key; lower is more salient.
    pub salience: u32,
}

/// The underground band's cave-level document, mirroring `vessel/level/v1`'s
/// `SessionLevel` (`windows/vessel/src/level_doc.rs`). Two things differ from
/// [`Plan`], and both are load-bearing rather than cosmetic — see
/// `level_doc.rs`'s own module doc, which this mirror follows:
///
/// - [`LevelPaletteEntry`] interns on `(kind, VISIBILITY)`, never on colour
///   — there is no `color` field at all, unlike [`PaletteEntry`]'s. The
///   producer's own doc states why: this client's renderers withhold tint
///   from a mark by rule, so encoding fog as colour would put this band's
///   field of view below the walk band's on the one axis the systems audit
///   credits it for. `level.rs` carries the remembered/lit distinction as a
///   **glyph twin** instead (spec §4.1).
/// - [`Level::cells`] is SPARSE — one entry per cell the possession has ever
///   seen, each naming its own `(x, y)` — unlike [`Plan::cells`]'s dense
///   row-major index sized to the whole extent. A never-seen cell is simply
///   absent (spec §4.1.1), so `level.rs`'s draw path iterates the list
///   directly rather than sweeping the extent the way `plan::draw` does.
///
/// `schema` (the per-document `"vessel/level/v1"` tag) is dropped from this
/// mirror, following [`Plan`]'s own precedent: `SessionLevel::schema` is
/// never read, and the top-level `Spatial` tag already discriminates the
/// band before this type is ever reached.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Level {
    /// Which habitation band this rung is — `"undercroft"`, `"shallows"`,
    /// … No reader in this campaign; kept for record fidelity.
    pub rung: String,
    /// The rung's own evaluation depth below the surface, metres. No reader
    /// in this campaign; kept for record fidelity.
    pub depth_m: f64,
    /// The level's bounds.
    pub extent: LevelExtent,
    /// The distinct `(cell kind, visibility)` pairs, in first-seen order.
    pub palette: Vec<LevelPaletteEntry>,
    /// The cells the possession has ever seen on this rung — sparse, unlike
    /// [`Plan::cells`] (see this type's own doc).
    pub cells: Vec<LevelCell>,
    /// The cell the possession stands on.
    pub you: LevelPoint,
    /// The individuals standing on the level. Reuses [`PlanMark`], exactly
    /// as the producer reuses `PlanMark` for this field rather than
    /// defining a fresh type. Drawn by [`crate::level::draw`]'s marks pass,
    /// which — unlike [`Plan::marks`]'s — draws a dedicated glyph rather
    /// than redrawing the cell's own terrain (see that module's doc).
    pub marks: Vec<PlanMark>,
}

/// A level's bounds, in level-local cells.
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct LevelExtent {
    /// Left edge.
    pub x: i32,
    /// Top edge.
    pub y: i32,
    /// Width, in cells.
    pub w: i32,
    /// Height, in cells.
    pub h: i32,
}

/// One distinct `(cell kind, visibility)` pair in a level's palette.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LevelPaletteEntry {
    /// `"floor"`, `"wall"`, `"flooded"`, `"stairs_down"`, `"stairs_up"`,
    /// `"threshold"`, `"deep"` or `"drop"` — the last three since the sim's
    /// spec §3.5. Additive: an unrecognised kind draws as rock
    /// ([`crate::level`]), so this list may grow without breaking a client.
    pub kind: String,
    /// `"here"`, `"lit"` or `"remembered"` — never a colour (spec §4.1).
    pub state: String,
}

/// One level-local cell position.
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct LevelPoint {
    /// Column.
    pub x: i32,
    /// Row.
    pub y: i32,
}

/// One cell the possession has ever seen on this rung — absent for every
/// cell it has not (spec §4.1.1).
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct LevelCell {
    /// Column, level-local.
    pub x: i32,
    /// Row, level-local.
    pub y: i32,
    /// Index into [`Level::palette`].
    pub ix: u32,
}
