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
}

/// One cell of the walk-band chart, mirroring `scene/surrounds/v2`'s
/// `SurroundsCell`.
///
/// Not every field here has a reader: [`ChartCell::u`] never enters the
/// projection (see `chart.rs`'s module doc, which records it as residue of the
/// wrong formula), and `seam` is carried for record fidelity. That is the
/// deliberate line the module doc draws — a whole unread *channel* comes out, a
/// leaf field of a record this crate does read stays.
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
    /// Lattice offset on axis 2; `null` on a seam cell.
    pub w: Option<i64>,
    /// Triangle orientation; `null` on a seam cell.
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
    /// Salience-ranked things standing here.
    pub marks: Vec<Mark>,
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
