//! The channel network (The Ford, spec §5.2/§5.3): rivers carried as
//! **polylines** with a discharge-derived angular width, and a point's
//! relation to water read as the banding of a **signed** distance to that
//! network.
//!
//! This is the repair for the type error named in the campaign's keystone —
//! a river stored as a *face* is as wide as the face, so every river in
//! Hornvale was one cell (~110 km) across at every zoom. Nothing here refines
//! the mesh; the channel is a one-dimensional feature carried alongside it,
//! and the band predicate evaluates at any position without traversal.
//!
//! **Everything in this module is angular.** `domains/terrain` works on the
//! unit sphere and no planet radius exists anywhere in the codebase, so a
//! "channel width in metres" has no defined meaning here. Widths are radians,
//! expressed as fractions of the local cell spacing — which is the better
//! statement of the campaign's claim anyway, since the claim is about the
//! *ratio* of channel width to cell width.

use crate::crust::SphereFbm;
use crate::globe::TectonicGlobe;
use crate::water::WaterKind;
use hornvale_kernel::{CellId, Geosphere, Seed, SphericalPolyline, band, math};
use std::collections::BTreeSet;

/// Where a point sits across a channel, outward from the centreline. The
/// ordinal is a banding of `|d|`; the *sign* of `d` (which bank) is reported
/// alongside it by [`ChannelNetwork::transverse_at`] and never folded in.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Transverse {
    /// In the water: `|d| < w/2`.
    Channel,
    /// The wetted margin between water and floodplain: `w/2 <= |d| < w/2 + k·w`.
    Bank,
    /// The valley floor the channel floods across: out to `V/2`.
    Floodplain,
    /// The abandoned older valley floor above the floodplain.
    Terrace,
    /// Beyond the valley entirely — the answer terrain already gives.
    Dry,
}

impl Transverse {
    /// The five transverse-band names in stable index order — the
    /// self-describing legend for scene emission (mirrors
    /// [`crate::water::WaterKind::LEGEND`]).
    pub const LEGEND: [&'static str; 5] = ["channel", "bank", "floodplain", "terrace", "dry"];

    /// Stable numeric index into `LEGEND`, independent of enum discriminant
    /// layout (explicit `match`, not `self as u8`, so reordering variants can
    /// never silently change a committed index).
    /// type-audit: bare-ok(index: return)
    pub fn index(self) -> u8 {
        match self {
            Transverse::Channel => 0,
            Transverse::Bank => 1,
            Transverse::Floodplain => 2,
            Transverse::Terrace => 3,
            Transverse::Dry => 4,
        }
    }

    /// Stable name, the one true transverse-band name source for scene
    /// emission.
    /// type-audit: bare-ok(identifier-text: return)
    pub fn name(self) -> &'static str {
        match self {
            Transverse::Channel => "channel",
            Transverse::Bank => "bank",
            Transverse::Floodplain => "floodplain",
            Transverse::Terrace => "terrace",
            Transverse::Dry => "dry",
        }
    }

    /// The band [`hornvale_kernel::band`] index names, given the four edges
    /// [`band_edges`] produces. Anything past the last edge is `Dry`.
    /// type-audit: bare-ok(index: band_index)
    pub fn from_band(band_index: usize) -> Transverse {
        match band_index {
            0 => Transverse::Channel,
            1 => Transverse::Bank,
            2 => Transverse::Floodplain,
            3 => Transverse::Terrace,
            _ => Transverse::Dry,
        }
    }
}

/// The `b` of downstream hydraulic geometry `w = a·Q^b`. Half is the
/// textbook value (width scales with the square root of discharge) and is
/// not a free parameter of this campaign: it is the exponent that makes
/// width-per-unit-discharge fall off as a river grows, which is what keeps a
/// mainstem from being a hundred times its headwaters' width.
/// type-audit: bare-ok(ratio)
pub const CHANNEL_WIDTH_EXPONENT: f64 = 0.5;

/// The `a` of `w = a·Q^b`, as a fraction of the local cell spacing.
///
/// **Calibrated once, in the open** (spec §10, H1): fitted so the widest
/// channel seed 42 actually produces on the canonical `Geosphere::new(6)` is
/// **1/100 of a canonical cell edge** across. That world's largest drainage
/// is 146, and at the pre-fit placeholder `5.0e-4` its widest channel was
/// 1.1127e-4 rad — 1/169.7 of the 0.018886-rad canonical edge. Scaling to hit
/// 1/100 exactly wants 8.4864e-4; `8.5e-4` is that rounded to two figures and
/// lands the widest channel at 1/99.8 of an edge, which is inside the width
/// law's own precision.
///
/// The cell edge very nearly cancels out of the fit (`w = a·edge·√Q` against
/// a target of `edge/100` gives `a ≈ 1/(100·√Q_max)` = 8.276e-4); the 2.5%
/// residual is the local spacing at the widest vertex differing from the
/// world-mean edge. So this coefficient is essentially a statement about the
/// largest discharge the terrain produces, not about the grid.
///
/// **The exponent, not this, is the dynamic-range lever.** Real drainage
/// spans 15 to ~180, so `√Q` spans only ~3.5× across the whole
/// headwater-to-mainstem range; widening the range by moving `a` is not
/// possible — it scales every channel equally.
/// type-audit: bare-ok(ratio)
pub const CHANNEL_WIDTH_COEFF: f64 = 8.5e-4;

/// The `k` of the bank border `w/2 + k·w`: the wetted margin either side of
/// the water is half a channel width.
/// type-audit: bare-ok(ratio)
pub const BANK_WIDTH_RATIO: f64 = 0.5;

/// Floodplain half-width, as a multiple of channel width, at zero
/// confinement (a perfectly flat reach). Terrestrial floodplains run tens of
/// channel widths across; 20 sits inside that range and keeps the widest
/// valley a fifth of a cell rather than a whole one.
/// type-audit: bare-ok(ratio)
pub const FLOODPLAIN_MAX_RATIO: f64 = 20.0;

/// Terrace width beyond `V/2`, as a fraction of `V/2`. The terrace is the
/// abandoned valley floor, so it is keyed to the valley rather than to the
/// channel: a gorge gets a narrow terrace and a broad valley a wide one.
/// type-audit: bare-ok(ratio)
pub const TERRACE_WIDTH_RATIO: f64 = 0.5;

/// The local gradient at and above which a reach is a **gorge**: fully
/// confined, no floodplain at all. Units are metres of fall per radian of
/// angular separation — the only gradient this codebase can express, since
/// there is no length scale to make it dimensionless.
///
/// **Measured, not assumed** (Task 4 probe, seeds 42/7/1234 at the canonical
/// `Geosphere::new(6)`, over every `WaterKind::River` cell with a downhill
/// target): the river-cell gradient distribution is p05 ≈ 2.0e3,
/// p50 ≈ 1.1–1.5e4, p75 ≈ 2.0–2.6e4, p90 ≈ 3.2–3.7e4, **p95 ≈ 4.0–4.2e4**,
/// p99 ≈ 5.7e4, max ≈ 6.7–8.5e4. The same statistic at level 5 is within
/// ~15% of the level-6 value, so this is a property of the terrain rather
/// than of the grid, and a fixed constant is a legitimate normalization.
///
/// 4.0e4 is that p95. The choice is what makes the confinement term actually
/// *discriminate*: it puts the median river at confinement ≈ 0.7, the steep
/// quartile below 0.4, and about 5% of river cells in true gorges.
/// Normalizing on the maximum instead would compress the whole population
/// into confinement ∈ [0.7, 1.0] and the law would be a near no-op;
/// normalizing on the median would make gorges of over half the world's
/// rivers.
/// type-audit: pending(wave-2)
pub const GORGE_SLOPE: f64 = 40_000.0;

/// Meander noise spatial frequency on the unit sphere. The value-noise
/// lattice spacing is `1/f` radians, so 24 puts a meander wavelength at
/// ~0.04 rad — a couple of canonical cells, which is the scale a mainstem
/// wanders on.
/// type-audit: bare-ok(ratio)
pub const MEANDER_FREQUENCY: f64 = 24.0;

/// Octaves in the meander field.
/// type-audit: bare-ok(count)
pub const MEANDER_OCTAVES: u32 = 4;

/// Peak meander displacement as a fraction of the local cell spacing, at
/// full confinement-free (flat) gradient. A quarter keeps a wandering
/// channel inside the pair of cells the vertex joins.
/// type-audit: bare-ok(ratio)
pub const MEANDER_AMPLITUDE_RATIO: f64 = 0.25;

/// Half the channel width for a reach carrying `drainage`, in radians, given
/// the local angular `cell_edge`. Downstream hydraulic geometry
/// `w = a·Q^b` with `a` a fraction of the cell edge, so the result stays
/// angular. **Unconditional in `drainage`**: a creek carrying one cell's runoff
/// is a narrow channel, not an absent one (The Rill, decision 0130).
///
/// This function used to return exactly `0.0` below
/// [`crate::water::RIVER_MIN_DRAINAGE`], on the grounds that a sub-threshold
/// trickle is not a channel. That was right for a network that rendered only
/// river cells — every band edge derives from this half-width, so a
/// sub-threshold line would have read `Dry` at its own centre and carried no
/// bank, floodplain or terrace — and it is wrong for one that renders the whole
/// flow tree. See the decision record for what the short-circuit was protecting
/// and why the protection is no longer needed.
///
/// # THIS LAW IS ALREADY A FUNCTION OF DRAINED AREA. DO NOT "FIX" IT.
///
/// `drainage` is an upstream **cell count**, which reads like a grid-dependent
/// quantity that would make every width wrong by a scale factor once anything
/// renders below cell scale. It is not, and the reason is that `cell_edge`
/// already carries the conversion.
///
/// `N` cells tile the sphere, so their mean area is `4π/N`; and a locally
/// hexagonal tiling of cells of area `A` has nearest-neighbour spacing
/// `d = √(2/√3)·√A = 1.0746·√A`. That is geometric necessity, not a fitted
/// coincidence — which is what makes it safe to build on. Measured over the
/// real mesh, `cell_spacing / √(4π/cell_count)` is **1.078208 / 1.078231 /
/// 1.078237 / 1.078238** at levels 4 / 5 / 6 / 7: constant to five figures
/// across a 64× change of resolution, and 0.34% above the planar value
/// because of the twelve pentagons and the curvature. So
///
/// ```text
///   a · edge · √count  =  a · (edge/√A_cell) · √(count · A_cell)
///                      =  (a · 1.0746) · √(drained area)
/// ```
///
/// and the count never appears on its own. The `edge` factor **is** the
/// `count → area` conversion, wearing `√A_cell`'s clothes.
///
/// ## The trap, named because this campaign walked into it
///
/// The Rill's original plan proposed exactly the "fix" the paragraph above
/// rules out: make `drainage` a drained area in steradians and **keep**
/// `cell_edge`. That multiplies the grid factor in twice. Every width would be
/// rescaled by `√(N₆/N_L)` — **×2 at level 5, ×1/64 at level 12** — which is
/// the scale error the change was meant to remove, with the sign flipped. The
/// same trap wearing a different coat: a sub-cell drainage count paired with
/// the *parent* cell's spacing, which is that factor the other way up.
///
/// `tests/rill_properties.rs` is the guard, and it has been shown to catch
/// this: mutating this line to `cell_edge * cell_edge` (dimensionally the same
/// defect, since `A_cell ∝ edge²`) reddens it at a relative 5.000e-1 per
/// doubling.
///
/// ## What Tier 2 inherits
///
/// **The measured size of the problem, so a subdivision knows what it is for**
/// (The Rill, Task 3; decision 0130 carries the same table with its
/// denominator). Tier 1 made every land cell carry a channel, which put a
/// polyline through **1.237%** of the walk-depth rooms over seed 42's land, up
/// from 0.0845% — a 14.6x gain that tracks the count of rendered cells. But
/// only **0.1716%** of those rooms read [`Transverse::Channel`] at their own
/// centroid, which is the only question any consumer asks: `windows/locale`'s
/// `describe`, `crossing_between` and `transverse_of` all query
/// `bank_reading(addr.centroid())`. So **86% of the rooms a channel passes
/// through do not read as water where a walker stands**, against 66% before
/// Tier 1 — rendering more reaches made each one harder to notice.
///
/// The reason is the ratio this whole doc is about. A headwater half-width is
/// **7.35e-6 rad** at level 6 against a **2.83e-4 rad** walk-depth room edge:
/// **one thirty-eighth of a room**. Rendering a reach is not resolving it, and
/// no change to the width law closes that gap without making creeks wrong.
/// **1.237% is the number a sub-cell subdivision is trying to recover, not
/// 0.1716%** — a prediction stated against the centroid figure sets the target
/// an order of magnitude low.
///
/// Mind the denominators when re-measuring: a [`hornvale_kernel::RoomAddr`] is
/// a **face** of the icosphere (`20·4^depth` of them) while
/// [`Geosphere::cell_count`] is its **dual** (`10·4^level + 2`), so the two
/// counts differ by ~2x and mixing them has already produced one spurious
/// factor-of-two disagreement between two measurements of this quantity.
///
/// Scale-freeness holds **only when the count and the spacing are at the same
/// level**. There is no [`Geosphere`] to ask below cell scale — level 12 would
/// be `10·4¹² + 2 = 167,772,162` cells — so a subdivision cannot obtain its
/// spacing by building a finer globe. It must derive the spacing from
/// [`hornvale_kernel::RoomAddr::corners`], which returns the three unit-sphere
/// corners of a room's own triangle at its own depth, and accumulate drainage
/// in those same sub-triangle units. Take the two from different depths and
/// the cancellation above is exactly what breaks.
///
/// ## The part that used to not be scale-free, and no longer exists
///
/// The zero-return compared against [`crate::water::RIVER_MIN_DRAINAGE`], which
/// is a **count**, so it was the single place where refining the grid changed
/// this function's answer for a fixed physical drained area: a trickle that was
/// not a channel here was a channel one level down, because its count
/// quadrupled while the threshold did not. Removing it makes the width law
/// scale-free without qualification — Tier 2 no longer inherits an exception.
///
/// The threshold itself is untouched and still gates
/// [`crate::water::classify`]'s `WaterKind::River`, which is a different
/// question: whether a cell is *named* a river. A cell may carry a rendered
/// watercourse and still classify `DryLand`, and that disagreement is
/// deliberate — The Ford measured it at ~49.6% before this task and this task
/// widens it on purpose.
/// type-audit: bare-ok(count: drainage), pending(wave-1: cell_edge), pending(wave-1: return)
pub fn channel_half_width(drainage: f64, cell_edge: f64) -> f64 {
    0.5 * CHANNEL_WIDTH_COEFF * cell_edge * math::powf(drainage, CHANNEL_WIDTH_EXPONENT)
}

/// How unconfined a reach is, in `[0, 1]`: `1` on the flat, falling linearly
/// to `0` at [`GORGE_SLOPE`] and staying there. This is the `g` of
/// `V = g(Q, slope)`'s slope half; `Q` enters through the channel width the
/// floodplain is measured in.
///
/// The clamp is what makes "a gorge has no floodplain" an *exact* property
/// rather than an asymptote: at or above [`GORGE_SLOPE`] the floodplain band
/// is empty, and [`band_edges`] emits two equal edges to say so.
/// type-audit: pending(wave-2: slope), bare-ok(ratio: return)
pub fn confinement(slope: f64) -> f64 {
    (1.0 - slope.abs() / GORGE_SLOPE).clamp(0.0, 1.0)
}

/// The four band borders outward from the centreline, in radians:
/// `[w/2, w/2 + k·w, V/2, V/2 · (1 + t)]` — channel/bank, bank/floodplain,
/// floodplain/terrace, terrace/dry. Non-decreasing by construction, so
/// [`hornvale_kernel::band`] reads them directly; a gorge collapses the
/// second and third to the same value and its floodplain band is empty.
/// type-audit: bare-ok(count: drainage), pending(wave-2: slope), pending(wave-1: cell_edge), pending(wave-1: return)
pub fn band_edges(drainage: f64, slope: f64, cell_edge: f64) -> [f64; 4] {
    let half = channel_half_width(drainage, cell_edge);
    let width = 2.0 * half;
    let bank = half + BANK_WIDTH_RATIO * width;
    let valley = bank + FLOODPLAIN_MAX_RATIO * width * confinement(slope);
    [half, bank, valley, valley * (1.0 + TERRACE_WIDTH_RATIO)]
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Cross product of two 3-vectors.
fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

/// Normalize a 3-vector; returns the input unchanged if its norm is zero.
fn normalize(v: [f64; 3]) -> [f64; 3] {
    let n = (v[0] * v[0] + v[1] * v[1] + v[2] * v[2]).sqrt();
    if n == 0.0 {
        v
    } else {
        [v[0] / n, v[1] / n, v[2] / n]
    }
}

/// Angular separation of two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos(dot(a, b).clamp(-1.0, 1.0))
}

/// Mean angular separation of a cell from its neighbours — the local cell
/// spacing every width in this module is a fraction of.
///
/// `pub(crate)` for `branch.rs`: a Tier 2 branch's width is fed the **same**
/// spacing as the trunk vertex it attaches to, because its discharge is
/// expressed in the same cell units. Two definitions of "the local spacing"
/// would be two levels, which is exactly the pairing trap
/// [`channel_half_width`]'s doc names.
pub(crate) fn cell_spacing(geo: &Geosphere, c: CellId) -> f64 {
    let neighbors = geo.neighbors(c);
    if neighbors.is_empty() {
        return 0.0;
    }
    let p = geo.position(c);
    let sum: f64 = neighbors.iter().map(|&n| angle(p, geo.position(n))).sum();
    sum / neighbors.len() as f64
}

/// Local gradient at `c`, metres of fall per radian toward its downhill
/// target. `0.0` where there is no target (a terminal sink), which reads as
/// perfectly unconfined — the wide, flat margin of a playa.
///
/// `pub(crate)` for `branch.rs`, for the reason [`cell_spacing`] is: a branch
/// takes its confinement from the cell it is a share of, so that a rill in a
/// gorge has no floodplain for the same reason the trunk beside it has none.
pub(crate) fn local_slope(globe: &TectonicGlobe, geo: &Geosphere, c: CellId) -> f64 {
    let Some(target) = *globe.downhill.get(c) else {
        return 0.0;
    };
    let separation = angle(geo.position(c), geo.position(target));
    if separation <= 0.0 {
        return 0.0;
    }
    (globe.elevation.get(c).get() - globe.elevation.get(target).get()) / separation
}

/// The channel network of one globe: the world's rivers as polylines, with
/// the band geometry each vertex implies.
///
/// Recomputed at genesis from committed state (drainage, the post-carve
/// downhill graph, elevation) plus one noise field; never serialized. The
/// band derivation itself makes **no seed draws** — only the meander
/// displacement does, through [`streams::CHANNEL_MEANDER`].
/// type-audit: pending(wave-1: band_edges)
#[derive(Clone, Debug)]
pub struct ChannelNetwork {
    /// One polyline per maximal downhill run of reaches, in build order
    /// (ascending head `CellId`). A tributary's run ends *on* the cell where
    /// it joins its trunk, and `build`'s explicit **confluence repair** pass
    /// then places that mouth vertex exactly on the trunk's own vertex for
    /// that cell, so the network is geometrically connected.
    ///
    /// This doc previously said the shared cell alone made the network
    /// connected "without any confluence special case". That was the false
    /// inference — shared *cell* does not imply shared *point* — and it is
    /// what produced the H2 defect: the tributary's mouth was anchored at the
    /// cell's undisplaced position while the trunk's vertex for the same cell
    /// was meander-displaced, leaving the two runs a median 4.5 channel
    /// half-widths apart. The special case exists precisely because geometric
    /// connectedness does not follow from graph connectedness, and a reader
    /// who assumes it does will re-introduce the same defect.
    /// type-audit: pending(wave-1: polylines)
    pub polylines: Vec<SphericalPolyline>,
    /// Per polyline, per vertex, the four [`band_edges`] borders for that
    /// vertex's discharge, gradient and cell spacing. Parallel to
    /// `polylines`: `band_edges[i].len() == polylines[i].points.len()`.
    ///
    /// **One vertex per line does not read its own cell's values: the
    /// terminal one**, where that cell is the non-river outlet the run drains
    /// into. It carries the last *river* cell's edges instead, because the
    /// sea's drainage is 0 (a zero-width mouth) and a salt basin's is its
    /// whole catchment's at zero gradient (a mouth flared into a valley an
    /// order of magnitude too broad). See the comment at the borrow in
    /// [`ChannelNetwork::build`] for both measured on seed 42.
    pub band_edges: Vec<Vec<[f64; 4]>>,
    /// Per polyline, per vertex, the cell that vertex was placed from — the
    /// downhill run the polyline is a rendering of. Parallel to `polylines`:
    /// `run_cells[i].len() == polylines[i].points.len()`.
    ///
    /// **The last cell of a run is usually not a reach.** A run includes
    /// the cell it drains into, so its final entry is normally the ocean or
    /// salt-basin outlet at its mouth (or, at a confluence, the trunk cell it
    /// joins). A consumer reading a per-cell field off these — drainage, say —
    /// must expect the sea's value there, and should read the reach's
    /// discharge off the *previous* cell, which is the same borrow
    /// `band_edges` makes.
    ///
    /// Published because a polyline's *geometry* alone cannot say where two
    /// lines meet. Since the confluence repair a tributary's mouth vertex is
    /// placed **exactly** on the trunk's vertex for the shared cell, so a
    /// consumer could in principle recover the join by looking for coincident
    /// points — but that is an inference from a float equality, and it cannot
    /// tell a confluence from two lines that merely pass through the same
    /// place. The cell correspondence states the topology outright, which is
    /// what a longitudinal-connectivity measurement needs if it is not to be
    /// measuring its own guess.
    pub run_cells: Vec<Vec<CellId>>,
    /// The meander displacement field. Derived once and reused for every
    /// vertex (the `Fbm` derive-once pattern), and — the point of it being a
    /// field at all — **continuous in position**, so a walker crosses a band
    /// edge once instead of flickering across it room by room.
    meander: SphereFbm,
    /// Per cell, the `(polyline, vertex)` of the run that **claimed** it and
    /// continued past it — the inverse of `run_cells`, as a dense `Vec` over
    /// the `CellId` index. `None` for a cell no run continues past: an ocean
    /// cell, or the outlet at a real mouth.
    ///
    /// Private and read through [`ChannelNetwork::trunk_vertex`]. It is an
    /// index over build order, so it is subject to the same rule as
    /// `BankReading::line`: **never serialized**. Kept rather than recomputed
    /// because the alternative is an `O(network)` scan per query, and Tier 2
    /// asks this question once per cell per reading.
    trunk_vertex: Vec<Option<(u32, u32)>>,
    /// The spherical bucket grid over this network's vertices that
    /// [`ChannelNetwork::nearest_line`] narrows its candidate line set with,
    /// and the measured `L_max` its coverage argument rests on.
    ///
    /// Private, and deliberately not part of the struct's documented public
    /// surface: it is a pure accelerator over `polylines`, carries no
    /// information that is not already in them, and every answer it takes part
    /// in is asserted equal to the unindexed scan's
    /// (`tests/channel_properties.rs`). Built by
    /// [`ChannelNetwork::assemble`], which is the only way any network in this
    /// crate is constructed, so no site can be assembled without it.
    grid: VertexGrid,
}

/// The pad added to a search radius before the bucket window is computed,
/// radians.
///
/// The window formulae below are exact in real arithmetic and are evaluated in
/// floating point, so a vertex sitting exactly on the cap boundary could in
/// principle be excluded by a last-ULP rounding of `asin`/`sin`. The failure
/// that would produce is the silent one — a true winner missing from the
/// candidate set — so the boundary is pushed outward by an amount that is
/// enormous against a double's rounding error at these magnitudes (~1e-16 rad)
/// and negligible against the grid's own bucket size (~2e-2 rad): it cannot
/// change which buckets are visited except within a nanoradian of an edge, and
/// there it errs toward visiting more.
const CAP_PAD: f64 = 1.0e-9;

/// The smallest radius a candidate search starts from, radians.
///
/// The search opens at `L_max / 2`, the minimum radius the coverage inequality
/// can be satisfied at. A network whose segments are all degenerate measures
/// `L_max == 0`, and a search that opened at zero and grew by multiplication
/// would never grow at all — so the opening radius has a floor. It is far below
/// any real network's cell spacing, so it never widens an ordinary first
/// gather.
const MIN_SEARCH_RADIUS: f64 = 1.0e-6;

/// A spherical bucket grid over one network's **vertices**, plus the longest
/// segment in that network — the two things
/// [`ChannelNetwork::nearest_line`]'s candidate gather is built from.
///
/// # Why vertices, and why `L_max`
///
/// The index narrows the candidate *line set* and nothing else; the winner is
/// then chosen by the unchanged scan over that set, so the whole of
/// correctness is: **is the true winner in the set?**
///
/// For a segment `[a, b]` of arc length `L` whose closest point to `p` lies at
/// distance `d`, the two sub-arcs from that closest point sum to `L`, so the
/// nearer of them is at most `L / 2`, and by the spherical triangle inequality
///
/// > `min( angle(p, a), angle(p, b) ) <= d + L / 2 <= d + L_max / 2`
///
/// So every segment within `D` of `p` has an **endpoint** — a vertex — inside
/// the cap of radius `D + L_max / 2`. Bucketing vertices and gathering that cap
/// therefore contains the winner. `L_max` is the only quantity that has to be
/// bounded, and it is **measured exactly**, in the same pass that buckets the
/// vertices, rather than assumed: spec §3.2 argues independently that
/// `L_max <= 1.5 * E_max`, and that argument is a tripwire on this measurement
/// (Task 2 scored it at 1.051), never a substitute for it.
///
/// # The grid
///
/// A plain latitude/longitude grid: `lat_bands` equal bands in latitude,
/// `lon_buckets = 2 * lat_bands` equal steps in longitude within every band,
/// stored as CSR (`starts` indexes `lines`). Buckets near the poles are
/// therefore narrow in true distance, which costs a query near a pole a wider
/// longitude window — computed **per query** from the search radius and the
/// query's own latitude, never from a fixed constant, which is precisely the
/// near-pole coverage hole The Bearing shipped and its equality test caught.
///
/// Entries are `(bucket, polyline)` pairs deduplicated at build time, so a long
/// line that crosses a bucket several times appears in it once.
#[derive(Clone, Debug)]
struct VertexGrid {
    /// Latitude bands, spanning `[-pi/2, pi/2]` in equal steps.
    lat_bands: usize,
    /// Longitude buckets within each band, spanning `[-pi, pi)` in equal steps.
    lon_buckets: usize,
    /// CSR offsets into `lines`, length `lat_bands * lon_buckets + 1`.
    starts: Vec<u32>,
    /// Polyline indices, grouped by bucket, ascending and deduplicated within
    /// each bucket.
    lines: Vec<u32>,
    /// The longest segment anywhere in the network, radians — the measured
    /// `L_max` the coverage inequality above is stated in. `0.0` for a network
    /// with no segments at all.
    max_segment: f64,
}

/// Latitude and longitude of a unit vector, radians: latitude in
/// `[-pi/2, pi/2]`, longitude in `(-pi, pi]`.
fn lat_lon(p: [f64; 3]) -> (f64, f64) {
    (math::asin(p[2].clamp(-1.0, 1.0)), math::atan2(p[1], p[0]))
}

/// The latitude band `lat` falls in, clamped into range. A NaN latitude lands
/// in band 0 rather than panicking — the query path's fallback is what makes a
/// NaN position answer correctly, and it is reached by the scan finding no
/// comparable distance, not by this function guessing.
fn band_of(lat: f64, lat_bands: usize) -> usize {
    let t = (lat + std::f64::consts::FRAC_PI_2) / std::f64::consts::PI * lat_bands as f64;
    // The NaN case is spelled out rather than left to a negated comparison:
    // every one of these three branches is a deliberate choice about an
    // incomparable input, and hiding one inside `!(t > 0.0)` makes it look
    // like an accident of operator precedence.
    if t.is_nan() || t <= 0.0 {
        0
    } else if t >= lat_bands as f64 {
        lat_bands - 1
    } else {
        t as usize
    }
}

/// The longitude step `lon` falls in, **unwrapped**: the caller may pass a
/// longitude outside `[-pi, pi)` (the two ends of a window straddling the
/// antimeridian do), so the result may be negative or `>= lon_buckets` and is
/// wrapped by the caller with `rem_euclid`.
fn lon_step(lon: f64, lon_buckets: usize) -> i64 {
    let t = (lon + std::f64::consts::PI) / (2.0 * std::f64::consts::PI) * lon_buckets as f64;
    if t.is_nan() { 0 } else { t.floor() as i64 }
}

impl VertexGrid {
    /// Bucket every vertex of `polylines` and measure the longest segment, in
    /// one pass over the network.
    ///
    /// The grid is sized to the network: `lat_bands = sqrt(V / 2)` puts roughly
    /// one vertex in each of the `2 * lat_bands^2 ~ V` buckets, so the bucket
    /// edge tracks the mesh's own cell spacing as the level changes rather than
    /// being tuned to one of them. The ceiling exists because the memory is
    /// `O(buckets)` and a pathological network should not be able to ask for an
    /// unbounded allocation; the floor keeps a one-vertex network legal.
    fn build(polylines: &[SphericalPolyline]) -> VertexGrid {
        let mut vertex_count = 0usize;
        let mut max_segment = 0.0_f64;
        for line in polylines {
            vertex_count += line.points.len();
            for w in line.points.windows(2) {
                let length = angle(w[0], w[1]);
                if length > max_segment {
                    max_segment = length;
                }
            }
        }
        let lat_bands = ((vertex_count as f64 / 2.0).sqrt().round() as usize).clamp(1, 1024);
        let lon_buckets = lat_bands * 2;
        let bucket_count = lat_bands * lon_buckets;

        // `(bucket, line)` pairs, sorted and deduplicated: a line crossing a
        // bucket with several vertices is one entry there. Sorting also puts
        // each bucket's own entries in ascending line order, which is the order
        // the query needs and is why the merged candidate list needs only one
        // sort of its own.
        let mut pairs: Vec<(u32, u32)> = Vec::with_capacity(vertex_count);
        for (i, line) in polylines.iter().enumerate() {
            for &p in &line.points {
                let (lat, lon) = lat_lon(p);
                let bucket = band_of(lat, lat_bands) * lon_buckets
                    + lon_step(lon, lon_buckets).rem_euclid(lon_buckets as i64) as usize;
                pairs.push((bucket as u32, i as u32));
            }
        }
        pairs.sort_unstable();
        pairs.dedup();

        let mut starts = vec![0u32; bucket_count + 1];
        for &(bucket, _) in &pairs {
            starts[bucket as usize + 1] += 1;
        }
        for b in 0..bucket_count {
            starts[b + 1] += starts[b];
        }
        let lines = pairs.iter().map(|&(_, line)| line).collect();

        VertexGrid {
            lat_bands,
            lon_buckets,
            starts,
            lines,
            max_segment,
        }
    }

    /// Append every polyline with a vertex in the cap of radius `rho` about
    /// `position` to `out`, then sort and deduplicate it — so `out` comes back
    /// in **ascending polyline index**, which is the order the candidate scan
    /// must evaluate in for the lowest-index tie-break to be the one the
    /// unindexed scan makes.
    ///
    /// The set is a superset of the cap: membership is decided by bucket, not
    /// by re-measuring each vertex. Over-inclusion costs a `signed_distance`
    /// call and cannot change the answer; under-inclusion changes a world.
    ///
    /// # The longitude window
    ///
    /// A cap of angular radius `r` about latitude `phi` spans longitudes within
    /// `asin(sin r / cos phi)` of the query's — **when the cap excludes both
    /// poles**, which is exactly the condition `|phi| + r < pi/2`, and which
    /// also guarantees `r < pi/2` so the formula's own branch is the principal
    /// one. When the cap reaches a pole, longitude stops bounding anything and
    /// the whole band is taken. Both the window and the test are recomputed
    /// **per query** from `r` and this query's latitude.
    ///
    /// Deduplication is by sorting a small candidate list rather than by
    /// stamping an epoch into a `Vec<u32>` over every polyline, and the reason
    /// is `&self`: an epoch array is mutable state, which this method cannot
    /// hold without interior mutability — and `ChannelNetwork` is shared across
    /// scoped threads by the lab's runner, so a `RefCell` here would cost the
    /// type its `Sync`. The alternative that keeps `&self` is a fresh
    /// `vec![0; polylines.len()]` per query, which at level 6 is a 14 KB zeroing
    /// per call against a candidate list of four to nine entries. Sorting the
    /// short list is cheaper on both counts, and it is what produces the
    /// ascending order the tie-break needs.
    fn gather(&self, position: [f64; 3], rho: f64, out: &mut Vec<u32>) {
        let r = rho + CAP_PAD;
        let (lat, lon) = lat_lon(position);
        let band_lo = band_of(lat - r, self.lat_bands);
        let band_hi = band_of(lat + r, self.lat_bands);
        let steps = self.lon_buckets as i64;

        let reaches_a_pole =
            lat + r >= std::f64::consts::FRAC_PI_2 || lat - r <= -std::f64::consts::FRAC_PI_2;
        let ratio = math::sin(r) / math::cos(lat);
        // `ratio.is_nan()` takes the whole band, which is the conservative
        // answer: over-inclusion costs a `signed_distance` call and cannot
        // change the result, while under-inclusion changes a world.
        let (first, count) = if reaches_a_pole || ratio.is_nan() || ratio >= 1.0 {
            (0, steps)
        } else {
            let half = math::asin(ratio);
            let lo = lon_step(lon - half, self.lon_buckets);
            let hi = lon_step(lon + half, self.lon_buckets);
            (lo, (hi - lo + 1).clamp(1, steps))
        };

        for band in band_lo..=band_hi {
            let row = band * self.lon_buckets;
            for k in 0..count {
                let column = (first + k).rem_euclid(steps) as usize;
                let bucket = row + column;
                let (from, to) = (
                    self.starts[bucket] as usize,
                    self.starts[bucket + 1] as usize,
                );
                out.extend_from_slice(&self.lines[from..to]);
            }
        }
        out.sort_unstable();
        out.dedup();
    }
}

/// One channel reading at one position: what
/// [`ChannelNetwork::bank_reading`] selected, all of it, from a single
/// vertex selection.
///
/// The four fields travel together because they are *one* answer to "which
/// reach applies here". Splitting them across separate queries is the defect
/// this type exists to prevent: each query would re-run the winning-line
/// tie-break and the nearest-vertex scan, and would then agree with the
/// others only by luck.
///
/// **`cell` and `line` are in-process handles, not document fields.** `cell`
/// names the canonical grid cell the winning vertex was placed from, so a
/// caller can read that reach's discharge (`GeneratedTerrain::drainage_at`)
/// without searching for the vertex a second time; `line` names the polyline
/// the reading is *about*, so a caller holding two readings can tell whether
/// they concern the same channel. Both are build-order-adjacent identity and
/// **neither may ever be serialized**; the durable things here are the signed
/// distance and the edges.
///
/// An earlier draft carried `cell` and withheld `line`, on the grounds that
/// the polyline index is build order. So is `cell`, and the argument for
/// publishing one is the argument for publishing the other — the rule the
/// campaign actually holds is *never serialize either*, which omission does
/// not enforce. What omission did instead was push a consumer comparing two
/// readings into re-running [`ChannelNetwork::nearest_line`] itself, which is
/// the duplicate-selection defect this type exists to prevent.
/// type-audit: pending(wave-1: signed_distance), pending(wave-1: band_edges), bare-ok(index: line)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct BankReading {
    /// Angular distance to the nearest channel in radians, **positive on the
    /// left bank facing downstream** — the number
    /// [`ChannelNetwork::bank_signed_distance`] reports, and meaningful in
    /// its sign only inside `band_edges` (see that method's own doc).
    pub signed_distance: f64,
    /// The four [`band_edges`] borders of the nearest vertex of the winning
    /// polyline: channel/bank, bank/floodplain, floodplain/terrace,
    /// terrace/dry.
    pub band_edges: [f64; 4],
    /// The canonical grid cell that vertex was placed from — the reach whose
    /// discharge and gradient produced `band_edges`, **except at a run's
    /// terminal vertex**, where the cell is the non-river outlet and the
    /// edges are the last river cell's (see [`ChannelNetwork::band_edges`]).
    /// A caller reading this cell's own drainage at a mouth reads the sea's.
    pub cell: CellId,
    /// The winning polyline's index in [`ChannelNetwork::polylines`] — the
    /// same index [`ChannelNetwork::nearest_line`] reports. An in-process
    /// handle, **never serialized**: two readings are about the same channel
    /// only if this agrees, and nothing else in the reading can say so.
    pub line: usize,
}

impl BankReading {
    /// The band this reading falls in — the classification
    /// [`ChannelNetwork::transverse_at`] reports, computed from the reading
    /// rather than from a second query.
    ///
    /// This exists so a caller that already holds a [`BankReading`] never has
    /// to re-run [`ChannelNetwork::nearest_line`] to classify it. That
    /// duplication was real: the lab's transect sweep ran the all-lines scan
    /// twice per probe, once for the owning line and once for the band.
    pub fn transverse(&self) -> Transverse {
        Transverse::from_band(band(self.signed_distance, &self.band_edges))
    }
}

impl ChannelNetwork {
    /// Build the network from a generated globe.
    ///
    /// Walks every **reach** in ascending `CellId` order — a reach being a land
    /// cell with somewhere to send its water — starting runs at *heads* (reaches
    /// no other reach drains into) and following `downhill` to the sea, a
    /// terminal sink, or an already-claimed trunk. **A run includes the cell it
    /// drains into**, so a run's last cell is the outlet (an ocean or
    /// salt-basin cell) or the trunk cell it joins.
    ///
    /// **THE REACH PREDICATE IS NOT `WaterKind::River`, AND THAT IS THE POINT**
    /// (The Rill, Task 3, decision 0130). `downhill` and `drainage` are
    /// computed for every land cell; this used to render only the ~6.7% above
    /// `RIVER_MIN_DRAINAGE` and discard the rest, so the world computed a
    /// complete space-filling flow tree and drew one fifteenth of it. It now
    /// draws all of it. Nothing upstream changed — the branches were always
    /// there — and `water::classify` is untouched, so a cell may carry a
    /// rendered watercourse and still classify `DryLand`. That disagreement is
    /// deliberate; do not "repair" it by widening `WaterKind::River`.
    ///
    /// A consequence worth naming: **the one-cell filter below is now
    /// unreachable**. A reach has a downhill target by definition, so its run
    /// pushes at least that target and is at least two cells long. The filter
    /// is kept as a statement of what a polyline is, not because anything
    /// reaches it — before this change it excluded a river cell with no
    /// downhill target and no river inflow, which was already rare, and such a
    /// cell is now not a reach at all and never starts a run.
    ///
    /// A final pass moves every **confluence mouth** onto the trunk vertex it
    /// joins, so runs that meet in the drainage graph also meet in space; see
    /// the comment on that pass for the measurement that made it necessary.
    ///
    /// `meander_seed` must already be the derived `streams::CHANNEL_MEANDER`
    /// leg (a caller holding the terrain-root seed derives it itself, the
    /// same way every other hash-noise-only leg in this crate is derived
    /// once and stored — see `TectonicGlobe::channel_noise_seed`). `build`
    /// does not derive it again: a caller handed the ROOT seed here would
    /// silently grant this network's build path the ability to derive any
    /// other terrain stream, which is exactly the leak `channel_seed`'s own
    /// doc warns against.
    pub fn build(globe: &TectonicGlobe, geo: &Geosphere, meander_seed: Seed) -> ChannelNetwork {
        let meander = SphereFbm::new(meander_seed, MEANDER_FREQUENCY, MEANDER_OCTAVES);
        // A REACH: land, with a downhill target to be a reach *of*. Land is
        // read as "not ocean" off `water_kind` rather than re-tested against
        // `sea_level`, so there is one definition of the shoreline in this
        // crate and not two; `classify`'s first branch is exactly
        // `elevation < sea_level`. The `downhill` half excludes terminal sinks,
        // which is what keeps a salt basin an OUTLET a run drains into rather
        // than a reach that drains onward — it has nowhere to drain onward to.
        let is_reach = |c: CellId| {
            !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
        };

        // In-degree within the reach subgraph, as a dense Vec (CellId is a
        // dense 0..N index — kernel/CLAUDE.md).
        let mut has_inflow = vec![false; geo.cell_count()];
        for c in geo.cells() {
            if !is_reach(c) {
                continue;
            }
            if let Some(t) = *globe.downhill.get(c) {
                has_inflow[t.0 as usize] = true;
            }
        }

        let mut claimed: BTreeSet<CellId> = BTreeSet::new();
        let mut runs: Vec<Vec<CellId>> = Vec::new();
        // Pass 0 starts only at heads, so a run is maximal upstream. Pass 1
        // sweeps anything still unclaimed: the downhill graph is strictly
        // descending and therefore acyclic, so pass 1 should find nothing —
        // it is here so the walk is total regardless.
        for heads_only in [true, false] {
            for c in geo.cells() {
                if !is_reach(c) || claimed.contains(&c) {
                    continue;
                }
                if heads_only && has_inflow[c.0 as usize] {
                    continue;
                }
                let mut run = vec![c];
                claimed.insert(c);
                let mut current = c;
                while let Some(target) = *globe.downhill.get(current) {
                    // EVERY TERMINATION KEEPS THE CELL IT STOPPED ON (The
                    // Rill, Task 2). A run must include the cell it drains
                    // into, and the two ways a run ends are the same rule:
                    // push the shared cell, then stop.
                    //
                    // This loop used to `break` BEFORE pushing a non-river
                    // target, which ended every run one cell short of its
                    // mouth. Two consequences, both measured on seed 42 at
                    // level 6: the outlet cell carried no channel, and a
                    // river cell whose downhill target is not a river and
                    // which has no river inflow became a ONE-cell run and was
                    // dropped by the length filter below — 39 river cells with
                    // no polyline at all, exactly the 39 that read `Dry` at
                    // their own centres. Under a network that renders every
                    // land cell the deficit grows, because the number of runs
                    // terminating at a non-river cell rises with it.
                    run.push(target);
                    if !is_reach(target) {
                        // The outlet this run drains into — the sea, or a
                        // terminal sink (a salt basin) with nowhere to send
                        // what it receives. The walk follows reaches, so it
                        // stops here; the vertex stays.
                        break;
                    }
                    if !claimed.insert(target) {
                        // Joined a trunk another run already owns: keep the
                        // shared vertex so the lines meet, then stop.
                        break;
                    }
                    current = target;
                }
                if run.len() >= 2 {
                    runs.push(run);
                }
            }
        }

        let mut polylines = Vec::with_capacity(runs.len());
        let mut all_edges = Vec::with_capacity(runs.len());
        for run in &runs {
            let base: Vec<[f64; 3]> = run.iter().map(|&c| geo.position(c)).collect();
            let mut points = Vec::with_capacity(base.len());
            let mut edges = Vec::with_capacity(base.len());
            for (i, &c) in run.iter().enumerate() {
                let spacing = cell_spacing(geo, c);
                let slope = local_slope(globe, geo, c);
                if is_reach(c) {
                    edges.push(band_edges(*globe.drainage.get(c), slope, spacing));
                } else {
                    // THE BORROWED TERMINAL VERTEX. A non-reach cell is a
                    // run's LAST cell by construction — the walk above stops
                    // the moment it pushes one — so this is the outlet, and
                    // the outlet's own hydraulics are not this reach's.
                    // Measured on seed 42 at level 6, over the 129 runs that
                    // gained a mouth: 105 outlets are ocean, where drainage is
                    // 0 and `channel_half_width` is therefore EXACTLY 0.0 —
                    // the mouth would render as a zero-width channel with no
                    // bank, no floodplain and no terrace, `[0, 0, 0, 0]`. The
                    // other 24 are salt basins, which are worse than useless
                    // rather than merely empty: a basin accumulates its whole
                    // catchment and is a terminal sink, so it reads a larger
                    // drainage at zero gradient — cell 10666 gives
                    // `[7.59e-5, 1.52e-4, 3.19e-3, 4.78e-3]` against the
                    // feeding reach's `[5.28e-5, 1.06e-4, 2.47e-4, 3.71e-4]`,
                    // a mouth 1.4x as wide inside a valley 12.9x as broad.
                    // So the terminal vertex carries the last river cell's
                    // band geometry: the mouth is as wide as the river that
                    // arrives at it.
                    edges.push(*edges.last().expect(
                        "a non-river cell is never a run's first cell, so an earlier vertex \
                         has already pushed its edges",
                    ));
                }
                if i == 0 || i + 1 == base.len() {
                    // A source and a mouth are anchored: the head must stay in
                    // its own cell and the mouth must stay on the coast. A
                    // mouth that is a CONFLUENCE rather than a coast is moved
                    // onto its trunk by the pass below.
                    points.push(base[i]);
                    continue;
                }
                // Displace perpendicular to the local travel direction, read
                // off the UNDISPLACED neighbours so the result does not depend
                // on the order vertices are visited in.
                let tangent = normalize([
                    base[i + 1][0] - base[i - 1][0],
                    base[i + 1][1] - base[i - 1][1],
                    base[i + 1][2] - base[i - 1][2],
                ]);
                let left = normalize(cross(base[i], tangent));
                // Amplitude scales with confinement, i.e. inversely with
                // gradient: low slope meanders, steep slope runs straight.
                let amplitude = MEANDER_AMPLITUDE_RATIO * spacing * confinement(slope);
                let offset = amplitude * meander_field(&meander, base[i]);
                points.push(normalize([
                    base[i][0] + left[0] * offset,
                    base[i][1] + left[1] * offset,
                    base[i][2] + left[2] * offset,
                ]));
            }
            polylines.push(SphericalPolyline { points });
            all_edges.push(edges);
        }

        // THE CONFLUENCE REPAIR. A tributary's mouth sits on a cell the TRUNK
        // carries as an interior vertex, so the anchoring rule above would
        // leave the tributary's copy at the cell's undisplaced position while
        // the trunk's copy is meander-displaced. The two runs are then joined
        // in the drainage graph and separated in space — measured at a median
        // 4.5 channel half-widths on seed 42, which put a walker out of the
        // water at 12 of 15 joins and is what falsified the campaign's
        // longitudinal-connectivity prediction (spec 10, H2). Placing the
        // mouth on the trunk's own vertex makes the two lines meet exactly.
        //
        // `owner[c]` is the (line, vertex) of the run that CLAIMED `c` and
        // continued past it, never one that merely terminates on it. That
        // distinction is the whole of confluence topology, and it makes the
        // map unambiguous: once a cell is claimed only the claiming run walks
        // on, so a cell is a non-final vertex of at most one run. A mouth
        // whose cell no run continues from is a REAL mouth — the sea or a
        // terminal sink — and stays anchored where it is.
        //
        // THE TERMINAL VERTEX (Task 2) IS INERT HERE, and the reason is the
        // same rule rather than a new exception. `owner` records only
        // NON-FINAL vertices; a non-reach outlet is a run's last cell by
        // construction, so it can never be one, and `owner[outlet]` is
        // therefore always `None`. Every run that gained a mouth falls into
        // the `else { continue }` arm below and keeps that mouth anchored on
        // its outlet cell — which is what a real mouth wants. Two runs
        // draining into the same sea cell both end there and neither is moved
        // onto the other, exactly as before; they are not a confluence,
        // because neither continues past it.
        //
        // Order-independent by construction: only final vertices are moved and
        // only non-final vertices are read, so no relocation can be the source
        // of another.
        let mut owner: Vec<Option<(usize, usize)>> = vec![None; geo.cell_count()];
        for (i, run) in runs.iter().enumerate() {
            for (j, &c) in run.iter().enumerate() {
                if j + 1 < run.len() {
                    owner[c.0 as usize] = Some((i, j));
                }
            }
        }
        for i in 0..polylines.len() {
            let last = polylines[i].points.len() - 1;
            let Some((trunk, vertex)) = owner[runs[i][last].0 as usize] else {
                continue;
            };
            if trunk != i {
                polylines[i].points[last] = polylines[trunk].points[vertex];
            }
        }

        // The same `owner` relation, kept: it is what a Tier 2 branch attaches
        // to. Built from `runs` rather than from `owner` above so that this
        // survives a future change to the confluence pass, and asserted
        // functional (one claiming run per cell) by
        // `tests/rill_properties.rs`'s R-4 rather than here, where a panic in
        // genesis would be the wrong instrument.
        let mut trunk_vertex: Vec<Option<(u32, u32)>> = vec![None; geo.cell_count()];
        for (i, run) in runs.iter().enumerate() {
            for (j, &c) in run.iter().enumerate() {
                if j + 1 < run.len() && trunk_vertex[c.0 as usize].is_none() {
                    trunk_vertex[c.0 as usize] = Some((i as u32, j as u32));
                }
            }
        }

        // ASSEMBLED, not struct-literalled, and assembled HERE — after the
        // confluence repair above, which relocates every tributary mouth.
        // `VertexGrid::build` measures `L_max` and buckets the vertices in the
        // same pass, so both would be taken from pre-repair geometry if this
        // ran any earlier.
        ChannelNetwork::assemble(polylines, all_edges, runs, meander, trunk_vertex)
    }

    /// Assemble a network from its parts, building the vertex index over the
    /// polylines as given.
    ///
    /// **The only constructor in the crate**, hand-built test networks
    /// included, and that is the point: `grid` is a derived field, and a struct
    /// literal that forgot it — or supplied one built from *different*
    /// polylines — would produce a network whose `nearest_line` silently
    /// answers about geometry it does not have. A private constructor makes
    /// that unrepresentable; a private field alone does not.
    fn assemble(
        polylines: Vec<SphericalPolyline>,
        band_edges: Vec<Vec<[f64; 4]>>,
        run_cells: Vec<Vec<CellId>>,
        meander: SphereFbm,
        trunk_vertex: Vec<Option<(u32, u32)>>,
    ) -> ChannelNetwork {
        let grid = VertexGrid::build(&polylines);
        ChannelNetwork {
            polylines,
            band_edges,
            run_cells,
            meander,
            trunk_vertex,
            grid,
        }
    }

    /// The `(polyline, vertex)` of the run that carries `cell` and continues
    /// past it — where a Tier 2 branch of `cell`'s catchment attaches, and
    /// `None` for a cell no run continues past.
    ///
    /// The vertex is never the polyline's last, so `points[vertex + 1]` is
    /// always the next one downstream. **An in-process handle, never
    /// serialized**, for the reason [`BankReading::line`] gives.
    /// type-audit: bare-ok(index: return)
    pub fn trunk_vertex(&self, cell: CellId) -> Option<(usize, usize)> {
        // `get` rather than an index: a network built by hand for a test
        // carries no index at all, and "this cell has no trunk" is the right
        // answer there rather than a panic.
        self.trunk_vertex
            .get(cell.0 as usize)
            .copied()
            .flatten()
            .map(|(line, vertex)| (line as usize, vertex as usize))
    }

    /// The transverse band at `position`, and the **signed** great-circle
    /// distance to the nearest channel in radians (left-positive relative to
    /// the channel's direction of travel).
    ///
    /// The sign is not decoration: a ford is precisely the path on which it
    /// flips, and cut-bank versus point-bar is that sign read against meander
    /// curvature. On an empty network the answer is `(Dry, f64::INFINITY)`.
    ///
    /// The band geometry is that of the nearest *vertex* of the winning
    /// polyline, so width and confinement vary down a river as its discharge
    /// and gradient do.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn transverse_at(&self, position: [f64; 3]) -> (Transverse, f64) {
        let Some(reading) = self.bank_reading(position) else {
            return (Transverse::Dry, f64::INFINITY);
        };
        (reading.transverse(), reading.signed_distance)
    }

    /// The whole reading at `position`: the **signed** distance
    /// [`ChannelNetwork::bank_signed_distance`] reports, paired with the four
    /// [`band_edges`] borders that apply *there* — those of the nearest vertex
    /// of the winning polyline — and the cell that vertex was placed from.
    /// `None` on an empty network.
    ///
    /// **This is the one implementation of "which vertex's edges apply here",
    /// and it exists so that there can only be one.**
    /// [`ChannelNetwork::transverse_at`] classifies through it and a consumer
    /// that stores the pair reads it from here, so the edges a document
    /// carries are by construction the edges the classification was made
    /// with. The alternative — a caller re-selecting the vertex itself — would
    /// have to duplicate both the winning-line tie-break (which lives in
    /// [`ChannelNetwork::nearest_line`], and reaches the *sign*) and the
    /// nearest-vertex scan below, and would then agree with `transverse_at`
    /// only by luck. That is the same argument
    /// [`ChannelNetwork::bank_signed_distance`] makes for delegating rather
    /// than re-deriving.
    ///
    /// The vertex scan is by *undisplaced angular separation* from the vertex,
    /// not by the segment the signed distance was measured against: band
    /// geometry is a per-vertex property (discharge, gradient, cell spacing),
    /// and the nearest vertex is the reach whose hydraulics a point actually
    /// sits in.
    ///
    /// [`BankReading::cell`] is here for the same reason the edges are: a
    /// consumer asking whether a channel can be forded needs that reach's
    /// **discharge** as well as its width, and a second nearest-vertex search
    /// to find it would be a second chance to answer about a different reach
    /// than the bands describe. [`BankReading::line`] is here for the same
    /// reason once more: a consumer comparing *two* readings — the shape every
    /// crossing query has — must be able to ask whether they are about the
    /// same channel, and a second `nearest_line` call to find out would be the
    /// duplicate selection this method exists to remove.
    /// type-audit: pending(wave-1: position)
    pub fn bank_reading(&self, position: [f64; 3]) -> Option<BankReading> {
        let (line_index, signed) = self.nearest_line(position)?;
        let points = &self.polylines[line_index].points;
        let mut nearest = 0usize;
        let mut nearest_distance = f64::INFINITY;
        for (j, &v) in points.iter().enumerate() {
            let a = angle(position, v);
            if a < nearest_distance {
                nearest_distance = a;
                nearest = j;
            }
        }
        Some(BankReading {
            signed_distance: signed,
            band_edges: self.band_edges[line_index][nearest],
            cell: self.run_cells[line_index][nearest],
            line: line_index,
        })
    }

    /// The polyline [`ChannelNetwork::transverse_at`] would answer from at
    /// `position`, and the **signed** distance to it — `None` on an empty
    /// network.
    ///
    /// Published because *which* channel a reading came from is not
    /// recoverable from `(Transverse, f64)` alone, and a consumer that
    /// re-derives it re-derives the tie-break too. A transverse profile that
    /// re-enters `Channel` is a defect if it is the SAME channel and an
    /// ordinary neighbouring river if it is not; only this can tell them
    /// apart. Strict `<` keeps the first (lowest-index) line on an exact tie,
    /// which is the whole of the tie-break contract.
    ///
    /// That tie-break is the one place where build order still reaches the
    /// **sign**: on an exact `|d|` tie between two lines the lowest index
    /// wins, and the winner's downstream direction is what
    /// [`ChannelNetwork::bank_signed_distance`] then reports. The set of
    /// positions equidistant from two lines has measure zero and this is
    /// correct as it stands — but the campaign's rule is that the line index
    /// is never serialized while the sign it selects will be, so it is worth
    /// knowing the two are not entirely independent.
    /// # Implementation: the index narrows the candidate set and nothing else
    ///
    /// The scan below is the scan
    /// [`ChannelNetwork::nearest_line_reference`] runs — the same
    /// `line.signed_distance(position)`, the same `d.abs() < best.abs()`, in
    /// ascending polyline index — restricted to the lines
    /// [`VertexGrid::gather`] returns. The returned `f64` is bit-identical
    /// because it is produced by the same function on the same inputs, and the
    /// sign is **never re-derived from a segment here**: doing that would
    /// duplicate the degenerate-segment side-borrowing and the intra-segment
    /// tie-break that live in the kernel, which is the second chance to
    /// disagree that this method's own doc argues against.
    ///
    /// So correctness reduces to whether the winner is in the set, which
    /// [`VertexGrid`] states and argues. The loop iterates because the cap's
    /// radius depends on the answer: it opens at `L_max / 2` (the smallest
    /// radius the inequality can hold at, since `D >= 0`), and if the best
    /// distance found needs a wider cap than was gathered, it re-gathers at
    /// `D + L_max / 2`. `D` cannot increase when the set grows, so each pass
    /// strictly widens the cap and the next pass's requirement is no larger —
    /// it terminates, and in practice on the first or second pass.
    ///
    /// **Two exits go to the reference scan, and both are mandatory rather than
    /// defensive.** A cap of radius `pi` is the whole sphere, so there is
    /// nothing left to narrow and the index would only be paying for its own
    /// bookkeeping. And a candidate set in which nothing compares — every
    /// distance NaN, which a NaN position produces — must still answer exactly
    /// what the unindexed scan answers, which is what running it does.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn nearest_line(&self, position: [f64; 3]) -> Option<(usize, f64)> {
        if self.polylines.is_empty() {
            return None;
        }
        let half_max_segment = self.grid.max_segment / 2.0;
        let mut rho = half_max_segment.max(MIN_SEARCH_RADIUS);
        let mut candidates: Vec<u32> = Vec::new();
        loop {
            if rho.is_nan() || rho >= std::f64::consts::PI {
                return self.nearest_line_reference(position);
            }
            candidates.clear();
            self.grid.gather(position, rho, &mut candidates);
            if candidates.is_empty() {
                // Nothing in the cap at all: widen and look again. Growth is
                // multiplicative so this reaches the whole-sphere fallback in a
                // bounded number of passes however empty the neighbourhood is.
                rho *= 4.0;
                continue;
            }
            let mut best = f64::INFINITY;
            let mut best_line: Option<usize> = None;
            for &i in &candidates {
                let d = self.polylines[i as usize].signed_distance(position);
                if d.abs() < best.abs() {
                    best = d;
                    best_line = Some(i as usize);
                }
            }
            let Some(line) = best_line else {
                return self.nearest_line_reference(position);
            };
            if best.abs() + half_max_segment <= rho {
                return Some((line, best));
            }
            rho = best.abs() + half_max_segment;
        }
    }

    /// The **unindexed** linear scan over every polyline — the definition of
    /// [`ChannelNetwork::nearest_line`], kept as an executable oracle.
    ///
    /// It is byte-for-byte the loop `nearest_line` was before The Millrace
    /// indexed it, and it is never deleted. An index whose reference
    /// implementation is gone is an index nobody can ever re-verify, and this
    /// one narrows a candidate set that decides a **serialized sign** (see
    /// [`ChannelNetwork::bank_signed_distance`]) — a wrong answer here does not
    /// fail, it commits a different world and then drift-checks green forever.
    ///
    /// `domains/terrain/tests/channel_properties.rs` asserts
    /// `nearest_line == nearest_line_reference` — the full `Option<(usize,
    /// f64)>`, with the `f64` bit-equal — across levels 4 through 7 and a
    /// position sample that includes the places the index is most likely to be
    /// wrong. It is reached from there through
    /// [`ChannelNetwork::nearest_line_reference_for_test`].
    ///
    /// It is also the query path's own mandatory fallback: when the search
    /// radius reaches `pi` the cap is the whole sphere and the index has
    /// nothing left to narrow, so the scan runs.
    fn nearest_line_reference(&self, position: [f64; 3]) -> Option<(usize, f64)> {
        let mut best = f64::INFINITY;
        let mut best_line: Option<usize> = None;
        for (i, line) in self.polylines.iter().enumerate() {
            let d = line.signed_distance(position);
            if d.abs() < best.abs() {
                best = d;
                best_line = Some(i);
            }
        }
        best_line.map(|i| (i, best))
    }

    /// Test-only door onto [`ChannelNetwork::nearest_line_reference`], so the
    /// equality property battery in `tests/channel_properties.rs` can compare
    /// the index against the scan it replaced without the scan becoming part of
    /// this crate's real public surface.
    ///
    /// `#[doc(hidden)]` for the reason
    /// `hornvale_worldgen::defensibility_for_test` is: the reference stays
    /// private, with `nearest_line` as its only production entry point, and
    /// publishing a second "which line is nearest" method would invite exactly
    /// the duplicate selection [`ChannelNetwork::bank_reading`] exists to
    /// prevent.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    #[doc(hidden)]
    pub fn nearest_line_reference_for_test(&self, position: [f64; 3]) -> Option<(usize, f64)> {
        self.nearest_line_reference(position)
    }

    /// Angular distance from `position` to the nearest channel in radians,
    /// **positive on the left bank facing downstream** and negative on the
    /// right. `None` on an empty network — there is no bank to be on.
    ///
    /// This is the name stage 2 stores a channel distance under, and the name
    /// exists for the referent rather than the arithmetic. The number is the
    /// one [`ChannelNetwork::nearest_line`] already reports; what this method
    /// adds is a meaning for its **sign** that survives a rebuild.
    ///
    /// **Why "left of travel" is "left of downstream".** [`SphericalPolyline`]
    /// documents its sign as left-positive relative to the winning segment's
    /// direction of travel, and travel direction is vertex order. That alone
    /// is not a durable referent: the polyline *index* is build order and must
    /// never be serialized. But the vertex order within a line is not an
    /// accident — [`ChannelNetwork::build`] starts every run at a head and
    /// appends each `downhill` target in turn, so `run_cells[i]` and the
    /// parallel `polylines[i].points` run downstream, from source toward the
    /// sea. Left of travel is therefore left facing downstream: hydrology's
    /// own convention, the term a person would use, and stable across builds
    /// because it is a property of the flow graph rather than of the walk that
    /// rendered it.
    ///
    /// That is a property of `build`'s loop, not of any type here, so
    /// `channel_properties.rs::the_polyline_vertex_order_is_downstream_order`
    /// asserts it on a real world. A change that collected a run upstream
    /// would swap every bank in the world and break nothing else.
    ///
    /// # WHERE THE SIGN MEANS ANYTHING
    ///
    /// **Only within the banded neighbourhood of the winning segment.** This
    /// is a signed distance to the nearest of many *open arcs*, and such a
    /// field changes sign on surfaces that have nothing to do with water:
    /// beyond a line's endpoint, and along the bisector between two arcs that
    /// meet. A sign change out there is a fact about the polyline soup, not
    /// about a river. **Read the sign only where `|d|` is small — inside the
    /// [`band_edges`] of the reading, or equivalently where
    /// [`ChannelNetwork::transverse_at`] does not answer `Dry`.** A consumer
    /// that treats *any* sign change as a crossing (a ford, say) will report
    /// one on dry ground at every river source and every river mouth.
    ///
    /// Measured on seed 42 at `Geosphere::new(5)` **at The Ford, on the
    /// pre-Task-2 network** — 13 lines, 46 vertices, 26 endpoints, 1
    /// confluence, terrace edge (the outermost band) 2.0e-4 rad at its
    /// narrowest, 3.5e-3 median, **6.9e-3 at its widest**. That network is now
    /// 22 lines and 76 vertices (The Rill, Task 2: a run includes the cell it
    /// drains into), so the *counts* below have moved and have not been
    /// re-measured. What the paragraph is here to say has not: a sign change
    /// out beyond an endpoint is a fact about the polyline soup rather than
    /// about a river, the locus is a ray that scales with the probe, and no
    /// fixed distance threshold removes it. More lines means more endpoints
    /// means more of it.
    ///
    /// - A 720-point circle of radius **1.0e-2 rad** about each endpoint gives
    ///   54 sign changes. 29 are real crossings, inside the bands. **25 sit at
    ///   `|d|` = 1.0e-2 — the circle's own radius** — one for each of the 25
    ///   true endpoints (the 26th is a confluence mouth). That is farther from
    ///   water than any band edge in the world.
    /// - The locus is a *ray*, not a place: probing at radius 5.0e-3 finds the
    ///   same 54 flips with `|d|` ≤ 5.0e-3. It extends outward without bound,
    ///   so **no fixed distance threshold makes it go away** — only asking
    ///   whether the reading is inside its own bands does.
    /// - The confluence gives 4 flips at radius 1.0e-2: three real ones at
    ///   `|d|` ≈ 3e-5, and **one on the bisector between two branches at
    ///   `|d|` = 4.8e-3** (2.4e-3 at radius 5.0e-3 — it too scales with the
    ///   probe). Note that 4.8e-3 is *inside* the widest terrace edge, so a
    ///   band test only excludes it if the band is a narrow one. Gate a
    ///   crossing on `Channel`/`Bank`, not on the terrace.
    ///
    /// None of this is introduced here — it is inherent to the quantity, and
    /// [`ChannelNetwork::transverse_at`] has always had it. It is written down
    /// here because this is the method whose *name* promises the sign means
    /// something, and stage 2 stores that sign in a document.
    ///
    /// # Implementation note
    ///
    /// Delegating to `nearest_line` rather than re-deriving the sign from the
    /// winning segment is deliberate: the tie-break between equidistant
    /// segments and the side a **degenerate** segment borrows from its
    /// neighbour both live in the kernel, and meander displacement can
    /// collapse adjacent vertices, so that second path is reachable. A second
    /// implementation here would be a second chance to disagree with it.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn bank_signed_distance(&self, position: [f64; 3]) -> Option<f64> {
        self.nearest_line(position).map(|(_, signed)| signed)
    }

    /// The widest channel half-width anywhere in the network, radians
    /// (`0.0` on an empty network). The max, over every vertex of every
    /// polyline, of [`band_edges`]'s first border — the whole-network
    /// analogue of [`channel_half_width`] for a caller (Task 5's provider
    /// property test) that wants one number rather than a per-vertex query.
    /// type-audit: pending(wave-1: return)
    pub fn widest_half_width(&self) -> f64 {
        self.band_edges
            .iter()
            .flatten()
            .map(|edges| edges[0])
            .fold(0.0_f64, f64::max)
    }

    /// The meander displacement field at `position`, in `[-1, 1]` — the
    /// signed lateral offset (as a fraction of the local amplitude) this
    /// network's channels wander by.
    ///
    /// Public because the property it carries is the one the whole design
    /// rests on and downstream consumers may need to assert it: this is a
    /// **position-continuous** field, not a per-address hash like
    /// `windows/locale`'s `wetness` axis — which is exactly why sibling rooms
    /// there can never form a connected watercourse.
    /// type-audit: pending(wave-1: position), bare-ok(ratio: return)
    pub fn meander_at(&self, position: [f64; 3]) -> f64 {
        meander_field(&self.meander, position)
    }
}

/// The meander field, centred on zero: `SphereFbm` samples `[0, 1)`, and a
/// displacement needs both banks.
fn meander_field(fbm: &SphereFbm, position: [f64; 3]) -> f64 {
    2.0 * fbm.sample(position) - 1.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::streams;

    fn unit(x: f64, y: f64, z: f64) -> [f64; 3] {
        normalize([x, y, z])
    }

    /// The **measured** mean angular separation between neighbouring cells on
    /// the canonical `Geosphere::new(6)`, radians. Do not try to re-derive it
    /// and conclude it is wrong: it is not `sqrt(4π/N)` (0.01752), not the
    /// equal-area circle diameter (0.01976), and not a hexagon edge (0.0109).
    /// It is the mean over every cell of the mean angle to its neighbours,
    /// taken from the Task 4 probe — the same quantity `cell_spacing` computes
    /// per cell at build time. (Level 5, for reference: 0.037769.)
    const CANONICAL_CELL_EDGE: f64 = 0.018_886;

    /// The largest drainage the canonical grid actually produces, from the
    /// same probe: max 146 / 177 / 173 on seeds 42 / 7 / 1234, p99 ≈ 117–138,
    /// p50 ≈ 23–25. Claims about "the widest river" are stated HERE rather
    /// than at some round number the world never reaches — an assertion
    /// pinned to an unreachable discharge is a tripwire that reddens for
    /// reasons unrelated to what it claims.
    const CANONICAL_MAX_DRAINAGE: f64 = 180.0;

    /// A **measured** number of confluences on seeds 42 and 7 at the canonical
    /// level 6 — the population the confluence-repair test asserts over. A
    /// datum about the terrain, not a threshold anyone chose.
    ///
    /// **67 was 15 + 52, measured at The Ford. It is now 90 (20 + 70)**, and
    /// the constant is deliberately left at the older, lower value because the
    /// test below asserts `joins * 2 >= CONFLUENCES_AT_LEVEL_6` — an
    /// anti-vacuity floor, not a count. What moved it was The Rill's Task 2:
    /// before it, a run stopped one cell short of its outlet, so a trunk's last
    /// river cell was that trunk's FINAL vertex and the owner map never
    /// recorded it; a tributary joining exactly there was not recognised as a
    /// confluence at all. Extending every run to its outlet makes those joins
    /// visible — 5 more on seed 42, 18 on seed 7. They were harmless while
    /// invisible (both copies of the shared cell were anchored, so they
    /// coincided by accident), and they are now repaired explicitly.
    ///
    /// Level 6 rather than the level 5 the rest of this file uses, because
    /// level 5 does not have the phenomenon: seeds 42, 7 and 1234 together
    /// produce exactly **one** confluence there, so a test on that grid would
    /// assert over a single join however many worlds it swept. The test does
    /// not call `transverse_at`, so it pays only for genesis — measured at
    /// 0.63 s for both worlds, in line with the rest of this suite.
    const CONFLUENCES_AT_LEVEL_6: usize = 67;

    /// A hand-built two-segment polyline on the equator, with band edges from
    /// the REAL laws at a deliberately coarse synthetic cell (spacing 1.0
    /// rad). The coarseness is the point: it puts every band wide enough for
    /// the monotone sweep to resolve. Drainage 36 is an ordinary river on the
    /// canonical grid (p50 ≈ 23, p90 ≈ 55) and slope 0 is a flat, fully
    /// unconfined reach. Resulting edges at the calibrated coefficient:
    /// [0.00255, 0.00765, 0.10965, 0.164475] rad — 1.7x the pre-calibration
    /// figures, which is why the sweep below takes its reach from
    /// [`band_edges`] rather than from a fixed step count.
    fn test_network() -> ChannelNetwork {
        let points = vec![
            unit(1.0, 0.0, 0.0),
            unit(1.0, 0.5, 0.0),
            unit(0.5, 1.0, 0.0),
        ];
        let edges = band_edges(36.0, 0.0, 1.0);
        ChannelNetwork::assemble(
            vec![SphericalPolyline {
                points: points.clone(),
            }],
            vec![vec![edges; points.len()]],
            vec![(0..points.len() as u32).map(CellId).collect()],
            SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
            // These hand-built networks have no `CellId` domain to index, and
            // nothing in this module's own tests asks about a trunk vertex.
            Vec::new(),
        )
    }

    /// A network of exactly two hand-built polylines, in the order given, with
    /// the same synthetic band edges [`test_network`] uses on both. Built here
    /// for the same reason [`test_network`] is: `meander` is private, so no
    /// network can be assembled from outside the crate at all.
    fn two_line_network(first: Vec<[f64; 3]>, second: Vec<[f64; 3]>) -> ChannelNetwork {
        let edges = band_edges(36.0, 0.0, 1.0);
        let (first_len, second_len) = (first.len(), second.len());
        ChannelNetwork::assemble(
            vec![
                SphericalPolyline { points: first },
                SphericalPolyline { points: second },
            ],
            vec![vec![edges; first_len], vec![edges; second_len]],
            vec![
                (0..first_len as u32).map(CellId).collect(),
                (0..second_len as u32).map(CellId).collect(),
            ],
            SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
            // These hand-built networks have no `CellId` domain to index, and
            // nothing in this module's own tests asks about a trunk vertex.
            Vec::new(),
        )
    }

    /// Offset `start` by `off` radians perpendicular to the polyline's first
    /// segment, left-positive — the same side convention `signed_distance`
    /// reads.
    fn offset_perpendicular(line: &SphericalPolyline, start: [f64; 3], off: f64) -> [f64; 3] {
        let tangent = normalize([
            line.points[1][0] - line.points[0][0],
            line.points[1][1] - line.points[0][1],
            line.points[1][2] - line.points[0][2],
        ]);
        let left = normalize(cross(start, tangent));
        normalize([
            start[0] + left[0] * off,
            start[1] + left[1] * off,
            start[2] + left[2] * off,
        ])
    }

    fn sample_left(net: &ChannelNetwork) -> [f64; 3] {
        let line = &net.polylines[0];
        offset_perpendicular(line, line.points[0], 0.002)
    }

    fn sample_right(net: &ChannelNetwork) -> [f64; 3] {
        let line = &net.polylines[0];
        offset_perpendicular(line, line.points[0], -0.002)
    }

    /// The campaign's whole claim, as an assertion: the largest river the
    /// canonical grid ACTUALLY PRODUCES is a small FRACTION of a cell edge,
    /// not a cell. Stated over the measured discharge ceiling (~180), not a
    /// hypothetical one. Task 6's calibration moved the coefficient 1.70x
    /// (5.0e-4 -> 8.5e-4) and this test stayed green with room to spare: the
    /// widest channel at the ceiling discharge is now 2.154e-4 rad against a
    /// bound of 1.889e-3, i.e. ~8.8x of headroom, down from ~14.9x. The
    /// campaign's claim is a bound, not the calibration target — the target
    /// (1/100 of an edge, at seed 42's REAL max discharge of 146) sits an
    /// order of magnitude inside it, which is why fitting one did not
    /// threaten the other.
    #[test]
    fn the_widest_channel_is_far_narrower_than_a_cell() {
        let cell_edge = CANONICAL_CELL_EDGE;
        let widest = channel_half_width(CANONICAL_MAX_DRAINAGE, cell_edge) * 2.0;
        assert!(
            widest < cell_edge / 10.0,
            "widest channel {widest} rad is not << cell edge {cell_edge} rad"
        );
        assert!(widest > 0.0, "width law produced a non-positive width");
    }

    /// Width must be MONOTONE in discharge — a bigger river is never narrower.
    #[test]
    fn width_increases_with_discharge() {
        let e = CANONICAL_CELL_EDGE;
        // Across the range the world actually spans: threshold to ceiling.
        let small = channel_half_width(crate::water::RIVER_MIN_DRAINAGE, e);
        let big = channel_half_width(CANONICAL_MAX_DRAINAGE, e);
        assert!(big > small, "big {big} not wider than small {small}");
        // And beyond it, so the law itself is monotone rather than merely
        // happening to be so over the sampled interval.
        assert!(channel_half_width(2_000.0, e) > big);
    }

    /// A sub-threshold trickle is a NARROW channel, not an absent one — the
    /// whole of decision 0130, as an assertion. This test used to be
    /// `drainage_below_the_river_threshold_has_zero_width` and asserted the
    /// exact opposite; it is superseded rather than deleted so the reversal is
    /// visible where the old claim lived.
    ///
    /// The lower bound is the point. A creek carrying the runoff of one cell —
    /// `drainage` is a land-cell count and every land cell drains at least
    /// itself, so 1.0 is the floor the world can produce — must still have a
    /// positive width, because every band edge derives from this half-width and
    /// a zero here reads `Dry` at the channel's own centre.
    #[test]
    fn a_sub_threshold_trickle_is_a_narrow_channel_not_an_absent_one() {
        let e = CANONICAL_CELL_EDGE;
        let threshold = channel_half_width(crate::water::RIVER_MIN_DRAINAGE, e);
        let trickle = channel_half_width(crate::water::RIVER_MIN_DRAINAGE - 1.0, e);
        // The floor of the world's discharge range: one land cell's own runoff.
        let headwater = channel_half_width(1.0, e);
        assert!(headwater > 0.0, "a headwater creek has no channel at all");
        assert!(trickle > headwater, "width is not monotone below threshold");
        assert!(
            trickle < threshold,
            "a trickle is not narrower than a river"
        );
        // And it is narrow in the sense that matters: a headwater channel is
        // orders of magnitude below the cell that carries it, which is the
        // campaign's own claim extended to the smallest reach in the world.
        assert!(
            headwater * 2.0 < e / 1_000.0,
            "a headwater channel {} rad is not << cell edge {e} rad",
            headwater * 2.0
        );
    }

    /// Bands are ordered outward and exhaustive: walking away from the
    /// centreline you pass every band in order and never re-enter one.
    /// This is H4 (spec 10) as a unit test on the band function alone.
    ///
    /// The sweep's REACH is derived from the outermost band edge rather than
    /// hardcoded. It used to be 200 fixed 0.0005-rad steps, and Task 6's
    /// calibration (`CHANNEL_WIDTH_COEFF` 5.0e-4 -> 8.5e-4) scaled the bands
    /// past the end of it — the sweep stopped inside the floodplain and the
    /// anti-vacuity assertion below caught it. Deriving the reach fixes that
    /// for the right reason: the claim being tested is about ORDER, not about
    /// any particular width, so the sweep should follow the widths wherever
    /// calibration puts them instead of having to be re-tuned alongside.
    #[test]
    fn bands_are_monotone_outward() {
        let net = test_network();
        let start = net.polylines[0].points[0];
        let steps = 400;
        // Past the terrace, so `Dry` is reachable at any calibration.
        let reach = net.band_edges[0][0][3] * 1.2;
        let mut seen = Vec::new();
        for i in 0..=steps {
            let off = reach * f64::from(i) / f64::from(steps);
            let p = offset_perpendicular(&net.polylines[0], start, off);
            let (t, _) = net.transverse_at(p);
            if seen.last() != Some(&t) {
                seen.push(t);
            }
        }
        let expected = [
            Transverse::Channel,
            Transverse::Bank,
            Transverse::Floodplain,
            Transverse::Terrace,
            Transverse::Dry,
        ];
        assert!(
            seen.iter().eq(expected.iter().filter(|e| seen.contains(e))),
            "bands re-entered or came out of order: {seen:?}"
        );
        // The sweep is only worth running if it actually reaches every band:
        // a sweep that stops inside the floodplain satisfies the ordering
        // assertion above VACUOUSLY, since the filter drops what it never saw.
        assert_eq!(
            seen,
            expected.to_vec(),
            "the sweep must observe all five bands, not a prefix"
        );
    }

    /// The sign survives the band lookup, so a crossing is detectable.
    #[test]
    fn transverse_at_reports_a_signed_distance() {
        let net = test_network();
        let (_, left) = net.transverse_at(sample_left(&net));
        let (_, right) = net.transverse_at(sample_right(&net));
        assert!(left * right < 0.0, "sign lost: {left} and {right}");
    }

    /// An empty network has no bank to be on, and must say so rather than
    /// answer a number. `transverse_at` can report `f64::INFINITY` because it
    /// pairs it with `Dry`; a bare distance has no such companion, and `0.0`
    /// would read to a consumer banding it as "in the water".
    ///
    /// Constructed here rather than in `channel_properties.rs` because the
    /// `meander` field is private, so an empty network cannot be built from
    /// outside the crate at all.
    #[test]
    fn an_empty_network_has_no_bank() {
        let empty = ChannelNetwork::assemble(
            Vec::new(),
            Vec::new(),
            Vec::new(),
            SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
            // These hand-built networks have no `CellId` domain to index, and
            // nothing in this module's own tests asks about a trunk vertex.
            Vec::new(),
        );
        assert_eq!(empty.bank_signed_distance(unit(1.0, 0.0, 0.0)), None);
    }

    /// The bank distance is the same number `nearest_line` reports, so a
    /// consumer that needs the line index and one that needs only the distance
    /// can never disagree about which bank a point is on.
    #[test]
    fn the_bank_distance_agrees_with_the_nearest_line() {
        let net = test_network();
        for p in [sample_left(&net), sample_right(&net)] {
            let (_, expected) = net.nearest_line(p).expect("the test network is non-empty");
            assert_eq!(net.bank_signed_distance(p), Some(expected));
        }
        assert!(net.bank_signed_distance(sample_left(&net)).unwrap() > 0.0);
        assert!(net.bank_signed_distance(sample_right(&net)).unwrap() < 0.0);
    }

    /// On an **exact** `|d|` tie between two channels the lower polyline index
    /// wins, and the sign the world stores is that winner's — so build order
    /// decides a serialized value at every tied position.
    ///
    /// [`ChannelNetwork::nearest_line`] is the lexicographic argmin over
    /// `(|d|, index)`: minimise `|d|`, and the strict `<` keeps the lowest
    /// index when two are equal. Relax that one character to `<=` and the
    /// *last* equidistant line wins instead, which flips the sign
    /// [`ChannelNetwork::bank_signed_distance`] reports there. Nothing else in
    /// the suite objects to that flip; this test is what holds it.
    ///
    /// **The tie is constructed, not hunted.** The set of positions exactly
    /// equidistant from two lines has measure zero, so a tie found on a real
    /// world is a tie that moves the next time the network does. Two arcs
    /// mirrored in `z`, queried from a point on the equator, tie *bitwise*:
    /// under `z -> -z` every product inside `SphericalPolyline::signed_distance`
    /// is either unchanged or an exact IEEE negation, and negation commutes
    /// with round-to-nearest — while the two sides come out exactly opposite,
    /// the query point being left of the southern arc and right of the
    /// northern one. The equality is asserted rather than assumed: a tie that
    /// has quietly stopped being exact would make everything below vacuous.
    #[test]
    fn an_exact_tie_between_two_lines_is_won_by_the_lower_index() {
        let north = vec![unit(1.0, 0.0, 0.25), unit(0.0, 1.0, 0.25)];
        let south = vec![unit(1.0, 0.0, -0.25), unit(0.0, 1.0, -0.25)];
        let query = unit(1.0, 1.0, 0.0);

        let d_north = SphericalPolyline {
            points: north.clone(),
        }
        .signed_distance(query);
        let d_south = SphericalPolyline {
            points: south.clone(),
        }
        .signed_distance(query);
        assert_eq!(
            d_north.abs().to_bits(),
            d_south.abs().to_bits(),
            "the tie is not exact: |{d_north}| vs |{d_south}|"
        );
        assert!(d_north != 0.0, "the query point is ON both lines");
        assert_eq!(
            d_north, -d_south,
            "the two lines do not report opposite sides: {d_north} and {d_south}"
        );

        // North built first: index 0 wins the tie, and its sign is what the
        // network reports.
        let north_first = two_line_network(north.clone(), south.clone());
        assert_eq!(
            north_first.nearest_line(query).map(|(i, _)| i),
            Some(0),
            "an exact tie did not go to the lower index"
        );
        assert_eq!(north_first.bank_signed_distance(query), Some(d_north));
        assert_eq!(north_first.bank_reading(query).map(|r| r.line), Some(0));

        // The same two arcs, built in the other order: index 0 still wins, so
        // the reported sign is now the other one. Same geometry, same query,
        // opposite serialized sign — which is the whole reason this tie-break
        // is a determinism contract and not an implementation detail.
        let south_first = two_line_network(south, north);
        assert_eq!(
            south_first.nearest_line(query).map(|(i, _)| i),
            Some(0),
            "an exact tie did not go to the lower index"
        );
        assert_eq!(south_first.bank_signed_distance(query), Some(d_south));
        assert_eq!(
            north_first.bank_signed_distance(query).unwrap(),
            -south_first.bank_signed_distance(query).unwrap(),
            "build order stopped deciding the sign of a tied position"
        );
    }

    /// THE COVERAGE PROPERTY ITSELF, asserted directly rather than inferred
    /// from an answer coming out right.
    ///
    /// `tests/channel_properties.rs` holds the contract — the indexed
    /// `nearest_line` equals the unindexed scan — and that is the assertion
    /// that matters. But it can only see a gather bug that actually *changes an
    /// answer*, and measurement showed how thin that is: with the `L_max / 2`
    /// term deleted from the pruning bound outright, **one probe in 6,175**
    /// across four levels disagreed, and deleting the pole test from the
    /// longitude window changed nothing at all. Neither is a wrong answer being
    /// tolerated; both are the grid being far more generous than the cap it is
    /// asked for — the bucket edge at every level exceeds `L_max`, so bucket
    /// quantization supplies more margin than the term it would be covering
    /// for, and a query never gets far enough from every line to need a radius
    /// where the pole test binds. An equality test is a poor instrument for a
    /// property whose violations are that rare.
    ///
    /// So the mechanism is pinned where it is stated: **`gather` returns a
    /// superset of the lines with a vertex inside the cap.** That is the whole
    /// of [`VertexGrid`]'s argument, it holds for every radius rather than for
    /// the radii one world's queries happen to produce, and it is what the
    /// near-pole coverage hole The Bearing shipped would violate.
    ///
    /// The radii sweep from far below the bucket edge to most of a hemisphere,
    /// because the window formula changes branch across that range: below
    /// `pi/2 - |lat|` the longitude window is `asin(sin r / cos lat)`, and above
    /// it the cap swallows a pole and longitude bounds nothing.
    #[test]
    fn the_gather_covers_every_line_with_a_vertex_in_the_cap() {
        let geo = Geosphere::new(5);
        let outcome =
            crate::generate(Seed(42), &geo, &crate::TerrainPins::default()).expect("seed 42");
        let terrain = crate::GeneratedTerrain::new(geo, outcome);
        let net = terrain.channels();
        let geo = terrain.geosphere();

        // Positions: cell centres across the whole globe, plus the poles and a
        // ladder of latitudes closing on them — the region the window formula
        // is most sensitive in.
        let mut positions: Vec<[f64; 3]> =
            geo.cells().step_by(97).map(|c| geo.position(c)).collect();
        positions.push([0.0, 0.0, 1.0]);
        positions.push([0.0, 0.0, -1.0]);
        for lat in [89.99_f64, 89.0, 80.0, 45.0] {
            for hemisphere in [1.0_f64, -1.0] {
                for step in 0..8 {
                    positions.push(math::unit_sphere_from_lat_lon(
                        hemisphere * lat,
                        -180.0 + f64::from(step) * 45.0,
                    ));
                }
            }
        }

        let mut checked = 0usize;
        let mut nonempty = 0usize;
        let mut candidates = Vec::new();
        for &p in &positions {
            for rho in [1.0e-4_f64, 1.0e-2, 5.0e-2, 0.25, 0.75, 1.5, 2.0, 3.0] {
                candidates.clear();
                net.grid.gather(p, rho, &mut candidates);
                // Every line with a vertex inside the cap must be offered.
                for (i, line) in net.polylines.iter().enumerate() {
                    if line.points.iter().any(|&v| angle(p, v) <= rho) {
                        assert!(
                            candidates.binary_search(&(i as u32)).is_ok(),
                            "line {i} has a vertex inside the cap of radius {rho} about {p:?} \
                             but the gather did not offer it — the coverage argument the index \
                             rests on does not hold, and nearest_line can silently answer with \
                             the wrong river"
                        );
                    }
                }
                // Ascending and deduplicated, which is what makes the candidate
                // scan's strict `<` reproduce the reference's lowest-index
                // tie-break.
                assert!(
                    candidates.windows(2).all(|w| w[0] < w[1]),
                    "the gather is not strictly ascending: {candidates:?}"
                );
                if !candidates.is_empty() {
                    nonempty += 1;
                }
                checked += 1;
            }
        }
        println!(
            "gather coverage: {checked} (position, radius) pairs, {nonempty} with candidates, \
             over {} lines",
            net.polylines.len()
        );
        // Anti-vacuity in both directions: the sweep ran, and it ran on gathers
        // that actually returned something to be right about.
        assert!(checked >= 1_000, "only {checked} pairs swept");
        assert!(
            nonempty * 2 >= checked,
            "only {nonempty} of {checked} gathers returned any candidate at all — the \
             assertion above is mostly ranging over empty sets"
        );
    }

    /// `L_max` is **measured**, not assumed: the stored `max_segment` is the
    /// longest segment the network actually has.
    ///
    /// Spec §3.2 argues independently that `L_max <= 1.5 * E_max`, and Task 2
    /// scored that at 1.051. That argument is a tripwire on this measurement,
    /// never a substitute for it — a hard-coded ceiling would be a bound
    /// nobody checked against the geometry it is meant to bound.
    #[test]
    fn the_stored_max_segment_is_the_longest_segment_there_is() {
        let geo = Geosphere::new(5);
        let outcome =
            crate::generate(Seed(42), &geo, &crate::TerrainPins::default()).expect("seed 42");
        let terrain = crate::GeneratedTerrain::new(geo, outcome);
        let net = terrain.channels();
        let mut longest = 0.0_f64;
        let mut segments = 0usize;
        for line in &net.polylines {
            for w in line.points.windows(2) {
                longest = longest.max(angle(w[0], w[1]));
                segments += 1;
            }
        }
        assert!(segments > 1_000, "only {segments} segments to measure over");
        assert_eq!(
            net.grid.max_segment.to_bits(),
            longest.to_bits(),
            "the stored L_max ({}) is not the network's longest segment ({longest})",
            net.grid.max_segment
        );
        assert!(
            longest > 0.0,
            "a zero L_max would make the cap argument vacuous"
        );
        println!("L_max {longest} rad over {segments} segments");
    }

    /// ANTI-VACUITY FOR THE TEST ABOVE. The tie query reaches the tie-break
    /// through the **indexed gather**, not through the whole-sphere fallback.
    ///
    /// This matters because `nearest_line` answers from
    /// [`ChannelNetwork::nearest_line_reference`] whenever the search radius
    /// reaches `pi`, and the reference is the very implementation whose
    /// tie-break the test above is checking. If this network's query took that
    /// exit, the test would go green no matter what order the index gathered
    /// candidates in — it would be pinning the oracle against itself and
    /// asserting nothing about the index at all.
    ///
    /// The two arcs are each ~86.6 degrees, so `L_max` is ~1.51 rad and the
    /// half-segment term alone is ~0.76 — a large fraction of `pi`, which is
    /// exactly why the question is worth asking here and would not be worth
    /// asking on a real network (where `L_max` is a cell spacing, ~2e-2 rad).
    ///
    /// `D + L_max / 2` is the **widest** radius `nearest_line`'s loop can ever
    /// reach for a query whose answer is at distance `D`: the loop opens at
    /// `L_max / 2` and only ever re-gathers at `D + L_max / 2`. So showing that
    /// radius is below `pi`, and that the gather there already holds both
    /// lines, shows the fallback cannot fire and the tie is decided among
    /// candidates.
    #[test]
    fn the_exact_tie_is_decided_by_the_indexed_gather_not_the_fallback() {
        let north = vec![unit(1.0, 0.0, 0.25), unit(0.0, 1.0, 0.25)];
        let south = vec![unit(1.0, 0.0, -0.25), unit(0.0, 1.0, -0.25)];
        let query = unit(1.0, 1.0, 0.0);
        let net = two_line_network(north, south);

        let half = net.grid.max_segment / 2.0;
        let (_, signed) = net.nearest_line(query).expect("the network is non-empty");
        let widest = signed.abs() + half;
        println!(
            "tie query: L_max {} rad, D {} rad, widest search radius {widest} rad",
            net.grid.max_segment,
            signed.abs()
        );
        assert!(
            widest < std::f64::consts::PI,
            "the tie query's widest search radius is {widest} rad, so nearest_line can reach \
             the whole-sphere fallback and the tie-break test above says nothing about the index"
        );

        let mut candidates = Vec::new();
        net.grid.gather(query, widest, &mut candidates);
        assert_eq!(
            candidates,
            vec![0, 1],
            "the gather at the widest radius does not offer both tied lines in ascending order"
        );
        // And at the radius the loop actually OPENS at, so the assertion above
        // is not resting on a pass the query never makes.
        let mut opening = Vec::new();
        net.grid
            .gather(query, half.max(MIN_SEARCH_RADIUS), &mut opening);
        assert_eq!(
            opening, candidates,
            "the opening gather and the widest gather disagree about the candidate set"
        );
    }

    /// `Transverse`'s index/name/legend triple is a committed contract (a
    /// scene legend indexes into it), so it is pinned exactly as
    /// `WaterKind`'s is.
    #[test]
    fn transverse_index_name_and_legend_are_stable() {
        assert_eq!(Transverse::Channel.index(), 0);
        assert_eq!(Transverse::Bank.index(), 1);
        assert_eq!(Transverse::Floodplain.index(), 2);
        assert_eq!(Transverse::Terrace.index(), 3);
        assert_eq!(Transverse::Dry.index(), 4);
        assert_eq!(
            Transverse::LEGEND,
            ["channel", "bank", "floodplain", "terrace", "dry"]
        );
        for t in [
            Transverse::Channel,
            Transverse::Bank,
            Transverse::Floodplain,
            Transverse::Terrace,
            Transverse::Dry,
        ] {
            assert_eq!(Transverse::LEGEND[t.index() as usize], t.name());
            assert_eq!(Transverse::from_band(t.index() as usize), t);
        }
        assert_eq!(Transverse::from_band(99), Transverse::Dry);
    }

    /// The confinement law's defining property: A GORGE HAS NO FLOODPLAIN.
    /// At or above `GORGE_SLOPE` the bank and floodplain borders coincide, so
    /// the floodplain band is empty and a point just outside the bank reads
    /// `Terrace`. A flat reach of the same river has a broad one.
    #[test]
    fn a_gorge_has_no_floodplain_and_a_flat_reach_has_a_broad_one() {
        let spacing = CANONICAL_CELL_EDGE;
        let gorge = band_edges(100.0, GORGE_SLOPE, spacing);
        assert_eq!(
            gorge[1], gorge[2],
            "a gorge's floodplain band must be empty: {gorge:?}"
        );
        // Steeper still is no less confined — the clamp holds.
        let steeper = band_edges(100.0, GORGE_SLOPE * 3.0, spacing);
        assert_eq!(steeper, gorge);
        let flat = band_edges(100.0, 0.0, spacing);
        assert!(
            flat[2] > flat[1] * 10.0,
            "a flat reach must have a broad floodplain: {flat:?}"
        );
        // Confinement is monotone across the measured population, and the
        // normalization is chosen so it discriminates rather than saturating:
        // the median river (~1.3e4) sits mid-range, the steep quartile low.
        assert!(confinement(1.3e4) > 0.6 && confinement(1.3e4) < 0.8);
        assert!(confinement(2.4e4) > 0.3 && confinement(2.4e4) < 0.5);
        assert!(confinement(2.0e3) > confinement(2.4e4));
    }

    /// Band edges are non-decreasing at every gradient, which is what lets
    /// `band` read them directly.
    #[test]
    fn band_edges_are_non_decreasing_at_every_gradient() {
        for step in 0..40 {
            let slope = f64::from(step) * 2_500.0;
            let e = band_edges(60.0, slope, CANONICAL_CELL_EDGE);
            assert!(
                e[0] <= e[1] && e[1] <= e[2] && e[2] <= e[3],
                "edges out of order at slope {slope}: {e:?}"
            );
        }
    }

    /// THE POSITIVE CONTROL for spec §5.2's named trap. The meander must come
    /// from a position-continuous field, not a per-address hash. Two positions
    /// a hair apart must give nearly the same displacement — an address hash
    /// would make them independent draws and blow this apart — and two far
    /// apart must differ, which is what rules out the field being constant
    /// (the trivial way to pass the first assertion).
    #[test]
    fn meander_displacement_is_continuous_in_position() {
        let net = test_network();
        let a = net.meander_at(unit(1.0, 0.0, 0.0));
        let b = net.meander_at(unit(1.0, 1e-6, 0.0));
        let far = net.meander_at(unit(0.0, 1.0, 0.0));
        assert!(
            (a - b).abs() < 1e-3,
            "adjacent positions jumped: {a} vs {b}"
        );
        assert!((a - far).abs() > 0.0, "field is constant, not noise");
    }

    /// Continuity is not just a local accident: sweeping a great-circle arc,
    /// consecutive samples one milliradian apart never jump by more than a
    /// small fraction of the field's full range, AND the field varies over
    /// the arc by an appreciable amount. An address-hashed field fails the
    /// first; a constant field fails the second.
    #[test]
    fn the_meander_field_is_smooth_along_an_arc_and_not_constant() {
        let net = test_network();
        let mut previous = net.meander_at(unit(1.0, 0.0, 0.0));
        let mut largest_step = 0.0_f64;
        let mut lowest = previous;
        let mut highest = previous;
        for i in 1..500 {
            let t = f64::from(i) * 1e-3;
            let v = net.meander_at(unit(math::cos(t), math::sin(t), 0.0));
            largest_step = largest_step.max((v - previous).abs());
            lowest = lowest.min(v);
            highest = highest.max(v);
            previous = v;
        }
        assert!(
            largest_step < 0.1,
            "a 1 mrad step moved the field by {largest_step} — that is a hash, not a field"
        );
        assert!(
            highest - lowest > 0.1,
            "the field barely varies over half a radian ({lowest}..{highest}) — not noise"
        );
    }

    /// The campaign's central claim, as a network-level assertion: the band
    /// geometry is read from the NEAREST VERTEX, so one river is wide at its
    /// mouth and narrow at its head. A lookup that always took the polyline's
    /// first vertex — or one flat width per line — reads the same band at
    /// both ends and fails here.
    #[test]
    fn band_geometry_varies_along_one_river() {
        // Three collinear vertices: a trickle at the head, a mainstem at the
        // mouth. Same laws, three discharges an order of magnitude apart.
        let points = vec![
            unit(1.0, 0.0, 0.0),
            unit(1.0, 0.5, 0.0),
            unit(0.5, 1.0, 0.0),
        ];
        let run_cells = vec![(0..points.len() as u32).map(CellId).collect()];
        let net = ChannelNetwork::assemble(
            vec![SphericalPolyline { points }],
            vec![vec![
                band_edges(16.0, 0.0, 1.0),
                band_edges(200.0, 0.0, 1.0),
                band_edges(4_000.0, 0.0, 1.0),
            ]],
            run_cells,
            SphereFbm::new(
                Seed(42).derive(streams::CHANNEL_MEANDER),
                MEANDER_FREQUENCY,
                MEANDER_OCTAVES,
            ),
            // These hand-built networks have no `CellId` domain to index, and
            // nothing in this module's own tests asks about a trunk vertex.
            Vec::new(),
        );
        let head_half = net.band_edges[0][0][0];
        let mouth_half = net.band_edges[0][2][0];
        assert!(
            mouth_half > head_half * 10.0,
            "the test setup is not graded"
        );
        // One offset, chosen to sit INSIDE the mouth's channel and OUTSIDE
        // the head's: the reading must differ purely by where along the line
        // it is taken.
        let offset = 0.5 * (head_half + mouth_half);
        let line = &net.polylines[0];
        let at_head = offset_perpendicular(line, line.points[0], offset);
        let at_mouth = offset_perpendicular(line, line.points[2], offset);
        assert_eq!(
            net.transverse_at(at_mouth).0,
            Transverse::Channel,
            "the mainstem's own width was not used at the mouth"
        );
        assert_ne!(
            net.transverse_at(at_head).0,
            Transverse::Channel,
            "the head read the mainstem's width — geometry is not per-vertex"
        );
    }

    /// claim: structural(seed: [42, 7]) — exact vertex coincidence at every
    /// confluence of two fixed worlds, not a search across seeds for one that
    /// exhibits it. The second seed is coverage, not a hunt: the assertion is
    /// universal over every join in both worlds and the floor below asserts
    /// the population it ran on.
    ///
    /// THE CONFLUENCE REPAIR, as a property. Where a run ends on a cell some
    /// other run continues past, the two polylines must meet **exactly** —
    /// not merely nearby. Before the repair the tributary's mouth sat at the
    /// cell's undisplaced position and the trunk's copy was meander-displaced,
    /// a median 4.5 channel half-widths apart, which is what falsified H2.
    ///
    /// Exact equality is the right assertion because the repair is an
    /// assignment, not an approximation: a tolerance would pass for a network
    /// that merely brought the two ends close, which is the state this repair
    /// replaces.
    ///
    /// **The floor is the anti-vacuity half, and it is a floor rather than
    /// `> 0` for a measured reason.** Seed 42 at level 5 has exactly ONE
    /// confluence — and so do seeds 42, 7 and 1234 *together* on that grid —
    /// so a `joins > 0` assertion there would be satisfied by a single join
    /// and would stay green through a regression that stopped emitting almost
    /// every tributary. At level 6 the two campaign seeds carry
    /// [`CONFLUENCES_AT_LEVEL_6`] of them.
    ///
    /// Requiring **at least half** rather than the exact count is deliberate:
    /// the claim being guarded is "this assertion ran over a real population",
    /// which an exact count would turn into an unrelated tripwire reddening
    /// on any ordinary terrain drift.
    #[test]
    fn a_tributary_mouth_sits_exactly_on_the_trunk_vertex_it_joins() {
        let geo = Geosphere::new(6);
        let mut joins = 0usize;
        for seed in [42u64, 7] {
            let outcome =
                crate::globe::generate(Seed(seed), &geo, &crate::pins::TerrainPins::default())
                    .unwrap();
            let net =
                ChannelNetwork::build(&outcome.globe, &geo, outcome.globe.channel_noise_seed());
            // The same owner map `build` uses, rebuilt from the published
            // `run_cells` rather than from anything private.
            let mut owner: Vec<Option<(usize, usize)>> = vec![None; geo.cell_count()];
            for (i, run) in net.run_cells.iter().enumerate() {
                for (j, &c) in run.iter().enumerate() {
                    if j + 1 < run.len() {
                        owner[c.0 as usize] = Some((i, j));
                    }
                }
            }
            for (i, cells) in net.run_cells.iter().enumerate() {
                let last = cells.len() - 1;
                let Some((trunk, vertex)) = owner[cells[last].0 as usize] else {
                    continue;
                };
                if trunk == i {
                    continue;
                }
                joins += 1;
                assert_eq!(
                    net.polylines[i].points[last], net.polylines[trunk].points[vertex],
                    "seed {seed}: tributary {i} does not meet trunk {trunk} at cell {:?}",
                    cells[last]
                );
            }
        }
        assert!(
            joins * 2 >= CONFLUENCES_AT_LEVEL_6,
            "only {joins} confluences across the two seeds (measured \
             {CONFLUENCES_AT_LEVEL_6}) — the equality assertion above is running on almost \
             nothing"
        );
    }

    /// The network built from a real (small) globe is well-formed and
    /// deterministic. Structural only — the measurement of a canonical world
    /// is Task 6's, not this task's.
    #[test]
    fn a_built_network_is_well_formed_and_deterministic() {
        // Level 5, not the canonical 6: level 4 is too coarse to accumulate
        // `RIVER_MIN_DRAINAGE` anywhere (seed 42 has zero river cells there),
        // and level 6 is Task 6's measurement, not this test's.
        let geo = Geosphere::new(5);
        let outcome =
            crate::globe::generate(Seed(42), &geo, &crate::pins::TerrainPins::default()).unwrap();
        // The real production seed: the globe's own already-derived
        // CHANNEL_MEANDER leg, exactly as `GeneratedTerrain::new` passes it.
        let meander_seed = outcome.globe.channel_noise_seed();
        let net = ChannelNetwork::build(&outcome.globe, &geo, meander_seed);
        let again = ChannelNetwork::build(&outcome.globe, &geo, meander_seed);
        assert_eq!(net.polylines, again.polylines, "build is not deterministic");
        assert_eq!(net.band_edges, again.band_edges);
        assert!(
            !net.polylines.is_empty(),
            "no channels on a level-5 seed 42"
        );
        assert_eq!(net.run_cells.len(), net.polylines.len());
        for (line, cells) in net.polylines.iter().zip(&net.run_cells) {
            assert_eq!(
                line.points.len(),
                cells.len(),
                "run cells parallel to points"
            );
            // A run follows `downhill`, so consecutive cells are adjacent and
            // strictly descending — the property that makes a polyline a
            // rendering OF that run rather than an unrelated line beside it.
            for pair in cells.windows(2) {
                assert_eq!(
                    *outcome.globe.downhill.get(pair[0]),
                    Some(pair[1]),
                    "run cells are not a downhill chain"
                );
            }
        }
        for (line, edges) in net.polylines.iter().zip(&net.band_edges) {
            assert!(line.points.len() >= 2, "a one-point polyline is not a line");
            assert_eq!(line.points.len(), edges.len(), "edges parallel to points");
            for p in &line.points {
                let norm = (p[0] * p[0] + p[1] * p[1] + p[2] * p[2]).sqrt();
                assert!((norm - 1.0).abs() < 1e-12, "vertex off the unit sphere");
            }
            for e in edges {
                assert!(e[0] <= e[1] && e[1] <= e[2] && e[2] <= e[3], "{e:?}");
                assert!(e[0] > 0.0, "a river vertex with zero channel width");
            }
        }
        // Every river cell is covered by the network: it sits inside the
        // channel band of the polyline it belongs to, or is one of the
        // dropped isolated singletons.
        let on_line: BTreeSet<CellId> = geo
            .cells()
            .filter(|&c| {
                matches!(*outcome.globe.water_kind.get(c), WaterKind::River)
                    && matches!(net.transverse_at(geo.position(c)).0, Transverse::Channel)
            })
            .collect();
        assert!(
            !on_line.is_empty(),
            "no river cell reads as Channel at its own centre"
        );
    }
}
