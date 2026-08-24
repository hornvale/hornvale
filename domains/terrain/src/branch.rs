//! Sub-vertex tributaries (The Rill, Tier 2): the branches that **attach** to
//! the trunks Tier 1 renders, and the area partition that says how much water
//! each of them carries.
//!
//! # The keystone: subdivide the scalar, never the direction
//!
//! Drainage is a **scalar** — a number on a node — and refining it means
//! partitioning it by area. That is canonical, conserves by construction, and
//! cannot disagree with the coarse answer because the parts sum to the whole.
//! Flow is a **direction** — an arrow on an edge — and moving it from the
//! grid's vertices onto anything finer needs a transfer operator, of which there
//! is no canonical one. So a branch's direction is never computed: **it is
//! inherited from what it is attached to.** A branch joins the line it drains
//! into at a point, and flows into it because it is joined to it.
//!
//! ## Why this module does not route flow, stated as the measurement that
//! settled it
//!
//! An earlier Tier 2 lifted the coarse flow graph onto the room mesh and
//! routed a direction out of every room. `Geosphere` vertices are the icosphere's
//! **vertices** and a [`Facet`] is a **face**; a `downhill` edge joins two
//! adjacent vertices and an edge is shared by exactly two faces, so a coarse
//! flow edge runs **along a room's boundary and never through it**. That lift
//! had to be invented, and what it cost was measured on three worlds: 26-31%
//! of land delivered to the sea where the coarse graph delivers 74-82%, the
//! basin count doubled, and 6.0-7.5% of basin-interior faces terminating in a
//! *different* coarse basin — while every one of its one-step invariants
//! passed. It also saturated: a direction out of every face gives every face a
//! channel, so ~74% of land rooms carried a reach and ~5-6% of all land sat
//! inside one against a preregistered ceiling of 0.5%. **Saturation is the
//! signature of refining the wrong quantity.**
//!
//! Nothing here reads [`crate::globe::TectonicGlobe::elevation`], recomputes
//! accumulation, or asks where a room's water goes. The only questions this
//! module answers are *how much of a vertex's own catchment is in this part of
//! it* and *which line does that part drain into*, and the second has a
//! one-word answer: its parent's.
//!
//! # What is partitioned, and against what
//!
//! Coarse `drainage` counts land vertices upstream of and including a vertex, so
//! `drainage(c) − Σ drainage(u)` over the vertices `u` that drain into `c` is
//! **exactly 1**: the vertex's own area, the one unit of catchment that reaches
//! the trunk at `c` without passing through another vertex. The trunk already
//! carries everything else. So the object partitioned here is that single
//! unit — [`vertex_catchment`] steradians of it — and it is partitioned by
//! repeated bisection.
//!
//! **The partitioned scalar is an INTEGER, and that is what makes conservation
//! structural rather than merely careful.** A part carries a
//! [`Rill::share`] of [`RILL_WHOLE`]; a bisection computes the first part's
//! share as an exact `u128` product and the second's as `share − first`, so
//! the two sum to their parent with no rounding at all, and the shares of the
//! parts a vertex finally divides into sum to `RILL_WHOLE` **exactly, in any
//! order**. The area in steradians is a *rendering* of that share, one
//! multiplication away, and it is the share the conservation claim is made
//! about (`tests/rill_properties.rs`, R-3).
//!
//! An f64 partition would have been structural too — a part defined as its
//! parent minus its sibling cannot drift — but not exact, and the difference
//! showed up the moment R-3 was written to ask for bits: `a·f + (a − a·f)`
//! misses `a` by an ulp whenever the subtraction is inexact. The integer is
//! what lets the guard demand equality rather than a tolerance, and a
//! tolerance is what would have absorbed a partition that leaked.
//!
//! # The one invention, named as one
//!
//! Where the partition cuts is not derivable from committed state, so it is
//! **drawn** ([`crate::streams::RILL_PARTITION`], hash-noise per vertex and per
//! path — no `Stream` is consumed, so no draw order and no save-format
//! contract beyond the label). The cut is bounded to
//! `[CUT_FLOOR, 1 − CUT_FLOOR]` so that no part is smaller than a quarter of
//! its parent: a geometric bound that keeps a catchment from degenerating into
//! a sliver, chosen and stated before anything was measured.
//!
//! **The branching is therefore driven by the partition and not by a fixed
//! ratio**, which is what keeps Horton's bifurcation ratio (R-5) a claim about
//! something rather than an arithmetic property of the rule. The falsification
//! is a first-class arm rather than a source edit: [`CatchmentCut::Even`]
//! holds the drawn freedom constant at one half, and the campaign's
//! instruction is to re-measure under it and report the two side by side.
//!
//! # Geometry: planar in the vertex's own tangent frame
//!
//! A catchment is a rectangle in the tangent plane at its vertex, the root being
//! a square of the vertex's own area with one axis along the trunk. A part's
//! branch runs from its own centre to the nearest point on its parent's
//! branch — the trunk stretch itself, for the two parts the vertex first divides
//! into. Everything is planar and lifted to the sphere only at the boundary.
//!
//! **The projection is gnomonic, and the reason is attachment.** A gnomonic
//! projection maps great circles to straight lines exactly, so a mouth placed
//! on a 2D segment lifts *onto* the arc it was placed on. An orthographic
//! projection does not, and the first version of this module used one: mouths
//! landed 5.2e-6 rad off the line they drain into — most of a headwater
//! half-width, which is the scale of The Ford's H2 defect, where a tributary
//! joined its trunk in the graph and missed it in space. The guard measured
//! it; the projection change took it to 1e-16. What gnomonic costs instead is
//! scale: distances are stretched by `sec²θ`, which over a vertex's own 0.019
//! rad is 4 parts in 10,000 — four orders of magnitude below the room this
//! network resolves to, and it distorts no topology.
//!
//! Two consequences worth stating rather than discovering:
//!
//! - every branch lies inside the convex hull of the vertex's own square and its
//!   trunk stretch, so **a branch cannot cross a coarse divide** — it is
//!   bounded by the catchment it is a share of, by construction;
//! - the square is a same-area proxy for the vertex's real region, so
//!   neighbouring vertices' squares overlap slightly at their corners. The
//!   partitioned **scalar** is exact; the geometry is an approximation, and
//!   `tests/rill_probe.rs` measures the spill.
//!
//! # Units
//!
//! [`Rill::catchment`] is an area in **steradians** — the scale-free form, and
//! deliberately not a count. A count is a statement about a grid: refine the
//! grid and it quadruples, which is exactly why
//! [`crate::water::RIVER_MIN_DRAINAGE`] could not be inherited below vertex
//! scale. [`RILL_MIN_CATCHMENT`], the resolution the network stops at, is an
//! area for the same reason.
//!
//! The width law is fed a **count in vertex units** (`catchment / vertex area`)
//! paired with the **vertex's own spacing**, so both of its arguments come from
//! the same level, which is the pairing
//! [`crate::channel::channel_half_width`]'s own doc names as the trap. The
//! level-free form of that pairing — the half-width divided by the square root
//! of the drained area is a constant of the law, whatever level expresses
//! it — is asserted absolutely in `tests/rill_properties.rs`.

use crate::channel::{ChannelNetwork, band_edges, local_slope, vertex_spacing};
use crate::globe::TectonicGlobe;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{Facet, Geosphere, NearestVertexIndex, Seed, Vertex, math};

/// The finest catchment the branch network resolves, in steradians: the mean
/// area of one walk-depth room, `4π / (20·4¹²)`.
///
/// **The resolution is chosen, absolutely, as "the finest thing that can ask a
/// question".** `windows/locale` places a walker in a depth-12 room, so a
/// partition finer than one room draws line no observer can be positioned to
/// distinguish. Stated as an area rather than as a depth or a count because
/// the network has no grid: an area is the same quantity at every level, which
/// a count is not.
///
/// This is the constant that replaces [`crate::water::RIVER_MIN_DRAINAGE`]
/// below vertex scale — not by inheriting it (it compares a count, so a trickle
/// that is no channel at one level is one at the next) but by asking a
/// different question: not *is this a channel* but *is this catchment still
/// worth dividing*.
/// type-audit: pending(wave-1)
pub const RILL_MIN_CATCHMENT: f64 = 4.0 * std::f64::consts::PI / 335_544_320.0;

/// A vertex's whole catchment as an integer share — the unit the partition
/// conserves. A power of two below `2^53`, so every share is exactly
/// representable as an `f64` and the area a share renders to is one rounding
/// from the share rather than an accumulation of them.
/// type-audit: bare-ok(count)
pub const RILL_WHOLE: u64 = 1 << 50;

/// The smallest share a cut may leave to either part. A geometric bound, not a
/// tuned one: without it a uniform cut draws slivers, and a catchment shaped
/// like a sliver is not a catchment. Fixed before any measurement.
const CUT_FLOOR: f64 = 0.25;

/// The cut's fixed-point precision, in bits: fine enough that the drawn
/// fraction is not visibly quantized, coarse enough that a `u64` share times a
/// cut fits a `u128` with room to spare.
const CUT_BITS: u32 = 32;

/// `2^CUT_BITS`, as the divisor the cut is expressed over.
const CUT_SCALE: u64 = 1 << CUT_BITS;

/// Hard bound on partition depth, so a defect in the stopping rule fails
/// visibly rather than hanging. Not reachable: a part is at most
/// `1 − CUT_FLOOR` of its parent, so `0.75^d · vertex area < RILL_MIN_CATCHMENT`
/// forces `d ≤ 32` at the canonical level, and the depth is asserted below the
/// bound in `tests/rill_properties.rs`.
const MAX_PARTITION_DEPTH: u32 = 40;

/// One tributary: a great-circle segment from its head to the point where it
/// enters the line it drains into, and the share of its vertex's catchment it
/// carries.
///
/// **Never serialized, and it carries no index that could be.** A branch's
/// identity is its vertex plus its position in the partition, and neither is a
/// save-format quantity; a consumer wanting the network at a position asks
/// [`rill_reading`], which resolves it from the seed.
/// type-audit: pending(wave-1: head), pending(wave-1: mouth), pending(wave-1: catchment), bare-ok(ratio: share), bare-ok(count: depth), bare-ok(index: parent)
#[derive(Clone, Debug, PartialEq)]
pub struct Rill {
    /// The upstream end: the centre of the catchment this branch drains.
    pub head: [f64; 3],
    /// The downstream end: the point on the parent line this branch enters.
    /// For the two branches a vertex first divides into, that line is the trunk
    /// stretch itself, so the mouth lies **on** the rendered polyline.
    pub mouth: [f64; 3],
    /// The area this branch drains, in steradians — its share of its vertex's
    /// own one unit of catchment, and the scale-free discharge the width law
    /// is fed. A rendering of `share`, and inexact in the last bit for that
    /// reason; the conserved quantity is `share`.
    pub catchment: f64,
    /// The same area as an exact fraction of [`RILL_WHOLE`] — **the quantity
    /// the partition conserves.** Two siblings' shares sum to their parent's
    /// with no rounding, and a vertex's leaves sum to `RILL_WHOLE` in any order,
    /// which is what makes R-3 an equality rather than a tolerance.
    pub share: u64,
    /// The coarse vertex whose catchment this is a share of. The branch is
    /// attached, directly or through its ancestors, to this vertex's trunk, so
    /// the coarse vertex its water ultimately reaches is the one this vertex's
    /// trunk chain reaches (R-4).
    pub vertex: Vertex,
    /// How many bisections deep in the partition this branch is; `0` for the
    /// two parts the vertex's own catchment first divides into.
    pub depth: u32,
    /// The branch this one drains into, as an index into the slice
    /// [`rills_of`] returned — `None` for the two parts the vertex first divides
    /// into, which drain into the trunk itself.
    ///
    /// **An index into one call's own output, and never serialized**, for the
    /// reason [`crate::channel::BankReading::line`] gives. It is published
    /// because a branch set without its edges is not a tree, and Horton's laws
    /// are a claim about the tree.
    pub parent: Option<usize>,
}

/// What [`rill_reading`] found at a position: the nearest sub-vertex branch, and
/// the transverse geometry that branch implies.
///
/// Unsigned, unlike [`crate::channel::BankReading::signed_distance`]. A left
/// bank facing downstream is meaningful for a rendered polyline a walker can
/// follow; for a rill it would be an invention on top of an invention, and
/// nothing downstream asks for it.
/// type-audit: pending(wave-1: distance), pending(wave-1: band_edges), pending(wave-1: catchment)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RillReading {
    /// Angular distance to the nearest branch, radians.
    pub distance: f64,
    /// That branch's four [`band_edges`] borders — channel/bank,
    /// bank/floodplain, floodplain/terrace, terrace/dry — computed from its
    /// catchment and its vertex's own gradient and spacing, exactly as a trunk
    /// vertex's are.
    pub band_edges: [f64; 4],
    /// The winning branch's drained area in steradians.
    pub catchment: f64,
    /// The coarse vertex whose catchment the winning branch is a share of. Not
    /// necessarily the vertex nearest the query position: a vertex's square region
    /// is a same-area proxy for its real one, so the two disagree near a
    /// shared corner.
    pub vertex: Vertex,
}

/// Where a catchment divides between its two parts.
///
/// Two arms, and the second exists so that R-5's falsification is a committed,
/// re-runnable measurement rather than a source edit somebody has to describe
/// afterwards.
#[derive(Clone, Copy, Debug)]
pub enum CatchmentCut {
    /// The shipped partition: the cut is drawn from
    /// [`crate::streams::RILL_PARTITION`], per vertex and per path through the
    /// partition, bounded to `[CUT_FLOOR, 1 − CUT_FLOOR]`. Hash-noise only —
    /// the seed is sub-derived per key and no `Stream` is consumed, so there
    /// is no draw order to preserve.
    Drawn(Seed),
    /// **The falsification arm.** Every catchment halves, so the partition
    /// tree is balanced and its branching statistics are whatever the rule
    /// alone implies. If Horton's ratios do not move between this and
    /// [`CatchmentCut::Drawn`], they are the rule's and not the partition's,
    /// and R-5 is untestable as posed — which is exactly what happened to the
    /// quadrisecting construction this design replaces.
    Even,
}

impl CatchmentCut {
    /// The share of a catchment that goes to the first part, in
    /// `2^-32` units — the cut, before it is applied to anything.
    ///
    /// `code` identifies the part being divided: a leading `1` followed by one
    /// bit per bisection, so it is the *path* through the partition rather
    /// than a serial number, and it is stable under any change to the order
    /// the partition is walked in.
    fn cut(&self, vertex: Vertex, code: u64) -> u64 {
        let fraction = match self {
            CatchmentCut::Even => 0.5,
            CatchmentCut::Drawn(seed) => {
                let key = format!("{}-{}", vertex.0, code);
                let drawn = seed.derive(StreamLabel::dynamic(&key)).stream().next_f64();
                CUT_FLOOR + drawn * (1.0 - 2.0 * CUT_FLOOR)
            }
        };
        (fraction * CUT_SCALE as f64) as u64
    }

    /// Divide `share` in two, exactly. The first part is an exact `u128`
    /// product shifted back down — the shift is by a power of two, so it is a
    /// truncation and not a rounding — and the second is the remainder, so
    /// nothing is created or lost at any depth.
    fn divide(&self, vertex: Vertex, code: u64, share: u64) -> (u64, u64) {
        let first = (((share as u128) * (self.cut(vertex, code) as u128)) >> CUT_BITS) as u64;
        // `share − first`, and never `share`: the second part is the REMAINDER.
        // Returning the parent's whole share here is the defect this comment
        // now guards — it made two siblings sum to more than their parent, so
        // the partition stopped conserving, and the second part never shrank,
        // so the recursion ran to MAX_PARTITION_DEPTH spawning a full subtree
        // at every level (269 million nodes, 23.7 GB, a kernel panic).
        (first, share - first)
    }
}

/// The mean area of one vertex of `geo`, in steradians — the one unit of
/// catchment a vertex contributes to its own trunk, and the denominator that
/// turns a branch's area into the count the width law takes.
///
/// The **mean** rather than the vertex's own polygon area, matching what coarse
/// `drainage` counts: an accumulation of unit vertices, not of measured ones.
/// Pairing it with the vertex's **local** spacing is the same pairing every
/// trunk vertex already makes.
/// type-audit: pending(wave-1: return)
pub fn vertex_catchment(geo: &Geosphere) -> f64 {
    4.0 * std::f64::consts::PI / geo.vertex_count() as f64
}

/// The room's own angular spacing, radians: the mean of the three arc lengths
/// of its own triangle at its own depth.
///
/// **Carried forward from the construction this module replaces, because the
/// obligation it serves is unchanged**: a room's corners are mesh *vertices*,
/// so at a `Geosphere`'s own level a room's three edges are three
/// vertex-to-vertex separations — the same quantity `channel.rs`'s `vertex_spacing`
/// averages, taken around a face instead of around a vertex. Below vertex scale
/// there is no `Geosphere` to ask (level 12 would be `10·4¹² + 2` vertices), so
/// this is how a room-scale area or spacing is obtained. `tests/` anchors it
/// absolutely against [`Geosphere::position`], because a parent/child ratio
/// cannot see a spacing derived one level off — the factor cancels.
/// type-audit: pending(wave-1: return)
pub fn room_spacing(addr: &Facet) -> f64 {
    let [a, b, c] = addr.corners();
    (angle(a, b) + angle(b, c) + angle(c, a)) / 3.0
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

/// The point halfway along the great circle between two unit vectors.
fn midpoint(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    normalize([a[0] + b[0], a[1] + b[1], a[2] + b[2]])
}

/// A vertex's local tangent frame: orthographic coordinates in radians, with
/// `e1` along the trunk's own direction of travel.
struct Frame {
    /// The vertex's position on the unit sphere.
    origin: [f64; 3],
    /// Downstream, in the tangent plane.
    e1: [f64; 3],
    /// Left of downstream, in the tangent plane.
    e2: [f64; 3],
}

impl Frame {
    /// A unit-sphere position in local coordinates. Gnomonic: the radial
    /// component divides out, which is what makes a great circle a straight
    /// line here. Points more than a right angle from the origin project
    /// behind the observer; nothing in a vertex's own catchment is remotely
    /// that far, and a caller that reached one would see a sign flip rather
    /// than a wrong answer quietly.
    fn project(&self, q: [f64; 3]) -> [f64; 2] {
        let radial = dot(q, self.origin);
        [dot(q, self.e1) / radial, dot(q, self.e2) / radial]
    }

    /// Local coordinates back onto the unit sphere.
    fn lift(&self, p: [f64; 2]) -> [f64; 3] {
        normalize([
            self.origin[0] + p[0] * self.e1[0] + p[1] * self.e2[0],
            self.origin[1] + p[0] * self.e1[1] + p[1] * self.e2[1],
            self.origin[2] + p[0] * self.e1[2] + p[1] * self.e2[2],
        ])
    }
}

/// A catchment's region: a rectangle in local coordinates.
#[derive(Clone, Copy)]
struct Patch {
    /// Lower bound along `e1`.
    x0: f64,
    /// Upper bound along `e1`.
    x1: f64,
    /// Lower bound along `e2`.
    y0: f64,
    /// Upper bound along `e2`.
    y1: f64,
}

impl Patch {
    /// The rectangle's centre — where the branch draining it starts.
    fn centre(&self) -> [f64; 2] {
        [0.5 * (self.x0 + self.x1), 0.5 * (self.y0 + self.y1)]
    }

    /// Divide the longer side at `f`, so the two parts' areas are in the same
    /// ratio as the scalar the caller splits alongside this.
    fn split(&self, f: f64) -> (Patch, Patch) {
        if self.x1 - self.x0 >= self.y1 - self.y0 {
            let cut = self.x0 + (self.x1 - self.x0) * f;
            (Patch { x1: cut, ..*self }, Patch { x0: cut, ..*self })
        } else {
            let cut = self.y0 + (self.y1 - self.y0) * f;
            (Patch { y1: cut, ..*self }, Patch { y0: cut, ..*self })
        }
    }

    /// Whether a local point is inside the rectangle.
    fn contains(&self, p: [f64; 2]) -> bool {
        p[0] >= self.x0 && p[0] <= self.x1 && p[1] >= self.y0 && p[1] <= self.y1
    }
}

/// One node of the partition: its region, its share of the catchment, its path
/// through the partition, and the line its own parts will attach to.
#[derive(Clone, Copy)]
struct Node {
    /// The region this node drains.
    patch: Patch,
    /// Its share of the vertex's catchment, in `RILL_WHOLE` units.
    share: u64,
    /// A leading `1` then one bit per bisection: the path, not a serial.
    code: u64,
    /// Bisections above this node.
    depth: u32,
    /// The line this node's own parts drain into — its branch, or for the root
    /// the vertex's own bent stretch of trunk.
    line: Chain,
}

/// A line a part may drain into: two segments, given as three points.
///
/// **Three and not two, because the trunk BENDS at the vertex it belongs to.** A
/// vertex's stretch of trunk runs from the midpoint of the vertex above to the
/// midpoint of the vertex below, and the rendered polyline turns at the vertex's
/// own vertex in between. Treating that stretch as one straight segment cuts
/// the corner, and the guard measured what it cost: mouths 5.2e-6 rad off the
/// line they drain into, most of a headwater half-width. A branch's own line
/// has no bend, so it repeats its last point.
type Chain = [[f64; 2]; 3];

/// The point of `chain` nearest `p`, in local coordinates.
fn nearest_on(chain: Chain, p: [f64; 2]) -> [f64; 2] {
    let mut best = chain[0];
    let mut best_gap = f64::INFINITY;
    for seg in [[chain[0], chain[1]], [chain[1], chain[2]]] {
        let d = [seg[1][0] - seg[0][0], seg[1][1] - seg[0][1]];
        let len2 = d[0] * d[0] + d[1] * d[1];
        let q = if len2 <= 0.0 {
            seg[0]
        } else {
            let t =
                (((p[0] - seg[0][0]) * d[0] + (p[1] - seg[0][1]) * d[1]) / len2).clamp(0.0, 1.0);
            [seg[0][0] + t * d[0], seg[0][1] + t * d[1]]
        };
        let (dx, dy) = (p[0] - q[0], p[1] - q[1]);
        let gap = dx * dx + dy * dy;
        if gap < best_gap {
            best_gap = gap;
            best = q;
        }
    }
    best
}

/// Distance from `p` to `chain`, in local coordinates (radians).
fn distance_to(chain: Chain, p: [f64; 2]) -> f64 {
    let q = nearest_on(chain, p);
    let (dx, dy) = (p[0] - q[0], p[1] - q[1]);
    (dx * dx + dy * dy).sqrt()
}

/// The two parts a node divides into, or `None` where the partition stops.
///
/// **The conservation is here, in two lines**: the second part's share is the
/// parent's *minus* the first's, so the parts sum to the parent bit-exactly
/// and no rounding accumulates down the partition. Each part's branch runs
/// from its own centre to the nearest point on its parent's line, which is
/// what "the direction is inherited" means concretely — nothing computes a
/// direction for a part; it flows into the thing it is joined to.
fn parts(node: &Node, vertex: Vertex, unit: f64, cut: &CatchmentCut) -> Option<[Node; 2]> {
    if area_of(node.share, unit) <= RILL_MIN_CATCHMENT || node.depth >= MAX_PARTITION_DEPTH {
        return None;
    }
    let (first, second) = cut.divide(vertex, node.code, node.share);
    // The GEOMETRY FOLLOWS THE SCALAR, not the other way round: the rectangle
    // is cut at the share the scalar was cut at, so a part's region is the
    // part's own area to the precision the projection allows.
    let (lo, hi) = node.patch.split(first as f64 / node.share as f64);
    let make = |patch: Patch, share: u64, bit: u64| {
        let head = patch.centre();
        let mouth = nearest_on(node.line, head);
        Node {
            patch,
            share,
            code: node.code * 2 + bit,
            depth: node.depth + 1,
            line: [head, mouth, mouth],
        }
    };
    Some([make(lo, first, 0), make(hi, second, 1)])
}

/// The area a share renders to, steradians.
fn area_of(share: u64, unit: f64) -> f64 {
    unit * (share as f64 / RILL_WHOLE as f64)
}

/// The stretch of trunk that belongs to `vertex`, and the local frame it sets —
/// `None` for a vertex no polyline runs through (an ocean vertex, a coastal or
/// salt-basin outlet, anything the coarse graph gives no outflow).
///
/// The stretch runs from the midpoint of the vertex before to the midpoint of
/// the vertex after, so consecutive vertices' stretches tile the polyline without
/// overlapping, and the whole of it is the part of the line this vertex's own
/// catchment can reach. It is read off the **rendered** polyline rather than
/// off `Geosphere::position`, so a branch's mouth lands on the meander-
/// displaced line a walker actually finds — the same lesson The Ford's
/// confluence repair paid for, where a shared vertex was mistaken for a shared
/// point.
fn trunk_stretch(vertex: Vertex, net: &ChannelNetwork, geo: &Geosphere) -> Option<(Frame, Chain)> {
    let (line, j) = net.trunk_vertex(vertex)?;
    let points = &net.polylines[line].points;
    let here = points[j];
    let below = points[j + 1];
    let start = if j > 0 {
        midpoint(points[j - 1], here)
    } else {
        here
    };
    let end = midpoint(here, below);
    let origin = geo.position(vertex);
    let travel = [end[0] - start[0], end[1] - start[1], end[2] - start[2]];
    let radial = dot(travel, origin);
    let tangent = normalize([
        travel[0] - radial * origin[0],
        travel[1] - radial * origin[1],
        travel[2] - radial * origin[2],
    ]);
    // A degenerate stretch (a zero-length trunk step) would leave `tangent`
    // unnormalizable; fall back to any tangent direction rather than emitting
    // a frame whose axes are not orthonormal.
    let e1 = if dot(tangent, tangent) > 0.5 {
        tangent
    } else {
        normalize(cross(origin, [0.0, 0.0, 1.0]))
    };
    let e2 = cross(origin, e1);
    let frame = Frame { origin, e1, e2 };
    let stretch = [
        frame.project(start),
        frame.project(here),
        frame.project(end),
    ];
    Some((frame, stretch))
}

/// The root of a vertex's partition: a square of the vertex's own catchment area,
/// centred on the vertex and squared to its trunk, whose parts drain into the
/// trunk stretch.
fn root(stretch: Chain, unit: f64) -> Node {
    let half = 0.5 * unit.sqrt();
    Node {
        patch: Patch {
            x0: -half,
            x1: half,
            y0: -half,
            y1: half,
        },
        share: RILL_WHOLE,
        code: 1,
        depth: 0,
        line: stretch,
    }
}

/// The stated ceiling on what [`rills_of`] materialises for one vertex at the
/// canonical level-6 grid, in branches.
///
/// **Derived, then measured, and the two are reconciled here rather than left
/// as two different numbers in two different prose comments.** A vertex's
/// catchment is `4π/40962 = 3.068e-4` sr and the partition stops at
/// [`RILL_MIN_CATCHMENT`] `= 4π/335544320 = 3.745e-8` sr, a ratio of
/// **8191.6**. Under [`CatchmentCut::Even`] every leaf is the same size, so
/// the depth is `⌈log₂ 8191.6⌉ = 13` and there are exactly `2¹³ = 8192`
/// leaves and `2·8192 − 2 = 16382` branches. Under [`CatchmentCut::Drawn`] the
/// leaves are unequal and there are *more* of them: a leaf overshoots the
/// threshold rather than landing on it. Picking a leaf size-biased by mass
/// makes the chosen fraction's density `4v` on `[0.25, 0.75]` — size-biasing
/// favours the larger part — so the mean log-step is `0.6504` and renewal
/// theory's stationary overshoot gives `E[e^R] = 1.5376`. That predicts
/// `1.5376 × 8191.6 = 12,596` leaves and `2·12,596 − 2 = 25,190` branches;
/// the probe measures 12,570–12,602 leaves and 25,138–25,202 branches, which
/// is agreement to better than a part in a thousand. This bound is `2¹⁵`,
/// about 30% above the drawn arm's measured worst.
///
/// Of the two numbers this replaces, `tests/rill_probe.rs`'s "~25,000" was
/// **right** and [`rills_of`]'s own "about 14,000" was not — it is not the
/// branch count (25,190) and not the leaf count either (12,596), so nothing
/// it could have meant was true. Held by `branch.rs`'s own bounded probe, in
/// nodes.
/// type-audit: bare-ok(count)
pub const RILLS_PER_VERTEX_MAX: usize = 1 << 15;

/// Every branch of `vertex`'s catchment, in partition order.
///
/// The whole partition, so this is `O(vertex catchment / RILL_MIN_CATCHMENT)` —
/// at most [`RILLS_PER_VERTEX_MAX`] branches per vertex at the canonical level,
/// and about 25,190 of them under the drawn cut (16,382 under the even one).
/// It exists for the property tests and the probes, which need the population
/// rather than an answer; a consumer asking about a *position* wants
/// [`rill_reading`], which descends the same partition in `O(depth)` and never
/// builds this.
///
/// **The bound is asserted, in node count, by the probe at the foot of this
/// file** — the discipline a materialised recursion carries, and the one this
/// function was shipped without: its second part was returned as the parent's
/// whole share, so the partition neither shrank nor conserved, and one call
/// reached 269 million nodes and 23.7 GB before the kernel killed the box.
pub fn rills_of(
    vertex: Vertex,
    net: &ChannelNetwork,
    geo: &Geosphere,
    cut: &CatchmentCut,
) -> Vec<Rill> {
    let Some((frame, stretch)) = trunk_stretch(vertex, net, geo) else {
        return Vec::new();
    };
    let unit = vertex_catchment(geo);
    let mut out = Vec::new();
    let mut stack = vec![(root(stretch, unit), None)];
    while let Some((node, index)) = stack.pop() {
        let Some(children) = parts(&node, vertex, unit, cut) else {
            continue;
        };
        for child in children {
            out.push(Rill {
                head: frame.lift(child.line[0]),
                mouth: frame.lift(child.line[1]),
                catchment: area_of(child.share, unit),
                share: child.share,
                vertex,
                depth: child.depth - 1,
                parent: index,
            });
            stack.push((child, Some(out.len() - 1)));
        }
    }
    out
}

/// The nearest branch to `target` (in local coordinates) found by **descending**
/// the partition from `start`, as `(distance, catchment)`.
///
/// At each bisection both parts' branches are measured and then the part whose
/// region holds the query point is entered, so the cost is `O(depth)` — about
/// thirty segment tests — with nothing stored. The winner is therefore the
/// nearest branch **on the descent**, which is the nearest branch outright
/// except where a sibling subtree reaches back across a region boundary.
///
/// **How often that matters is now MEASURED, and the answer has two halves.**
/// `tests::the_descent_finds_what_an_exhaustive_walk_finds` runs this against
/// an exhaustive walk of the same partition, on the same query points, in the
/// same frame: **inside** the catchment square the two disagree on 2.0% of
/// queries by at most 0.118 of a walk-depth room's edge — below the resolution
/// the network is drawn to; **outside** it they disagree on 26.5% by up to
/// 1.099 room edges. So the shortcut is exact where it has a rectangle to
/// stand on and degrades to about a room where it does not, which is why
/// [`rill_reading`] scans the neighbours rather than trusting one vertex.
///
/// This doc previously asserted that "the probe measures how often that
/// matters" while nothing measured it: `tests/rill_probe.rs` measures which
/// coarse vertex owns a branch HEAD, a different quantity in a different frame.
/// The descent is split out from [`nearest_rill`] precisely so the claim could
/// be made checkable rather than left as prose.
fn descend_to_nearest(
    vertex: Vertex,
    start: Node,
    unit: f64,
    cut: &CatchmentCut,
    target: [f64; 2],
) -> Option<(f64, f64)> {
    let mut node = start;
    let mut best: Option<(f64, f64)> = None;
    while let Some(children) = parts(&node, vertex, unit, cut) {
        let mut chosen = 0usize;
        let mut chosen_gap = f64::INFINITY;
        for (i, child) in children.iter().enumerate() {
            let distance = distance_to(child.line, target);
            if best.is_none_or(|(d, _)| distance < d) {
                best = Some((distance, area_of(child.share, unit)));
            }
            // Descend into the region that holds the point; where the point is
            // outside both (the square is a proxy for a real vertex region, so
            // this happens near a corner), descend toward the nearer centre.
            let centre = child.patch.centre();
            let gap = if child.patch.contains(target) {
                0.0
            } else {
                let (dx, dy) = (target[0] - centre[0], target[1] - centre[1]);
                (dx * dx + dy * dy).sqrt()
            };
            if gap < chosen_gap {
                chosen_gap = gap;
                chosen = i;
            }
        }
        node = children[chosen];
    }
    best
}

/// The nearest branch of `vertex`'s own partition to `position`, as
/// `(distance, catchment)` in local coordinates — `None` where the vertex has no
/// trunk to attach to.
///
/// The frame and the root; the search itself is [`descend_to_nearest`].
fn nearest_rill(
    vertex: Vertex,
    position: [f64; 3],
    net: &ChannelNetwork,
    geo: &Geosphere,
    cut: &CatchmentCut,
) -> Option<(f64, f64)> {
    let (frame, stretch) = trunk_stretch(vertex, net, geo)?;
    let unit = vertex_catchment(geo);
    descend_to_nearest(
        vertex,
        root(stretch, unit),
        unit,
        cut,
        frame.project(position),
    )
}

/// The vertices [`rill_reading`] measures, **in the order it offers them**:
/// `here` first, then `geo.neighbors(here)` in the order the geosphere yields
/// them.
///
/// Split out from [`rill_reading`] with [`nearest_offered`] so that the search
/// order half of that function's determinism contract is a thing a test can
/// hold rather than only a sentence in a doc comment; see
/// `tests::an_exact_tie_between_candidate_vertices_is_kept_by_the_first_offered`.
/// Allocation-free on purpose — the shipped call is one `once` and one slice
/// iterator, exactly the two loops this replaces.
fn rill_candidates(here: Vertex, geo: &Geosphere) -> impl Iterator<Item = Vertex> + '_ {
    core::iter::once(here).chain(geo.neighbors(here).iter().copied())
}

/// The first candidate whose measured distance is **strictly** less than every
/// earlier one's, as `(distance, catchment, vertex)` — `None` when `measure`
/// answers `None` for all of them.
///
/// The strictness is the contract: on an exact tie the candidate offered
/// *first* is kept, so [`rill_candidates`]'s order decides the answer.
/// Relaxing this one `<` to `<=` hands every tied position to the last
/// equidistant candidate instead, and the reading that results feeds
/// `grounded_wetness` and so `micro.wetness`, which is emitted.
fn nearest_offered(
    candidates: impl Iterator<Item = Vertex>,
    mut measure: impl FnMut(Vertex) -> Option<(f64, f64)>,
) -> Option<(f64, f64, Vertex)> {
    let mut best: Option<(f64, f64, Vertex)> = None;
    for candidate in candidates {
        if let Some((distance, catchment)) = measure(candidate)
            && best.is_none_or(|(d, _, _)| distance < d)
        {
            best = Some((distance, catchment, candidate));
        }
    }
    best
}

/// The sub-vertex network at a position: the nearest branch, and the transverse
/// geometry it implies.
///
/// Scans the nearest vertex and its neighbours, because a vertex's square region
/// is a same-area proxy for its real one and a position near a shared corner
/// can be inside a neighbour's. `None` where no vertex in that neighbourhood
/// carries a trunk — open ocean, and the coastal and salt-basin faces the
/// coarse graph gives no outflow, which is how the fine network ends at a
/// coast with no special case for one.
///
/// **This is the "more line" Tier 2 exists to produce**, queryable the way
/// [`ChannelNetwork::bank_reading`] is. It is deliberately *not* a per-room
/// flow direction: nothing downstream consumes a direction, and inventing one
/// is what the design this replaces was falsified for.
///
/// **The search order is a determinism contract, for the same reason
/// [`ChannelNetwork::nearest_line`]'s is.** Candidates are [`rill_candidates`]
/// — `here` first, then `geo.neighbors(here)` in the order the geosphere
/// yields them — and [`nearest_offered`] compares with a **strict** `<`, so an
/// exact tie keeps whichever vertex was offered first. Changing the enumeration
/// order, or giving `here` a different priority, or relaxing the comparison,
/// silently re-decides every tied position. Both halves are now held by
/// `tests::an_exact_tie_between_candidate_vertices_is_kept_by_the_first_offered`,
/// which is why they are two named functions rather than two loops inline: a
/// tie is unreachable on a real world (0 of 3,352 multi-candidate positions on
/// seed 42 at level 5, closest non-zero gap 1.5e-8), so the assertion has to
/// be able to supply the distances itself.
///
/// That this reaches a *serialized* value is one step longer than it looks,
/// and getting it wrong is easy: [`RillReading`]'s `distance` and
/// `band_edges` are indeed never serialized, but they are not the end of the
/// path. `rill_reading` feeds `grounded_wetness`, which feeds `micro.wetness`,
/// which **is** emitted — The Rill's own blast radius moved `micro/wetness` in
/// the gallery, in three vessel snapshots and in two game-core fixtures. So a
/// future spatial index over this search is a determinism-contract change
/// needing byte-identity evidence, not a refactor. See follow-up 1 in
/// `docs/retrospectives/the-rill.md`.
/// type-audit: pending(wave-1: position)
pub fn rill_reading(
    position: [f64; 3],
    net: &ChannelNetwork,
    globe: &TectonicGlobe,
    geo: &Geosphere,
    index: &NearestVertexIndex,
    cut: &CatchmentCut,
) -> Option<RillReading> {
    let here = index.nearest_to_position(geo, position);
    let unit = vertex_catchment(geo);
    let (distance, catchment, vertex) = nearest_offered(rill_candidates(here, geo), |c| {
        nearest_rill(c, position, net, geo, cut)
    })?;
    Some(RillReading {
        distance,
        band_edges: band_edges(
            catchment / unit,
            local_slope(globe, geo, vertex),
            vertex_spacing(geo, vertex),
        ),
        catchment,
        vertex,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// [`rill_reading`] offers `here` before any neighbour, offers neighbours
    /// in the geosphere's own yield order, and takes a candidate only on a
    /// **strictly** smaller distance — so two candidates that measure exactly
    /// equal are decided by that order alone.
    ///
    /// This reaches a serialized value: the reading feeds `grounded_wetness`
    /// and so `micro.wetness`, which is emitted into the gallery, the vessel
    /// snapshots and the game-core fixtures. A spatial index over this search
    /// that gathered its candidates in a different order, or relaxed the `<`
    /// to `<=`, would move every tied position and leave every drift check
    /// green afterwards.
    ///
    /// **The tie is supplied, not found, and it has to be.** A throwaway probe
    /// over seed 42 at level 5 measured the real distribution: of 10,242 query
    /// positions, 3,352 had two or more candidate vertices answering, and
    /// **exactly none** of them tied — the closest two distances anywhere in
    /// that sweep differed by 1.5e-8. Vertex centres are the most symmetric
    /// positions the grid has, so a tie is not merely rare there but absent,
    /// and a version of this test that hunted for one on a real world would
    /// have asserted over an empty population and passed forever. That is why
    /// [`nearest_offered`] takes its measurements as a closure: the only way
    /// to hold this contract is to hand it the tie.
    #[test]
    fn an_exact_tie_between_candidate_vertices_is_kept_by_the_first_offered() {
        let geo = Geosphere::new(1);
        // A pentagon (5 neighbours) and a hexagon (6), because the candidate
        // count is not fixed and neither should the assertion be.
        for here in [Vertex(0), Vertex(20)] {
            let offered: Vec<Vertex> = rill_candidates(here, &geo).collect();
            assert_eq!(offered[0], here, "`here` is no longer offered first");
            assert_eq!(
                &offered[1..],
                geo.neighbors(here),
                "the neighbours are no longer offered in the geosphere's yield order"
            );

            // Every candidate exactly equidistant: the order decides.
            let tied = nearest_offered(rill_candidates(here, &geo), |_| Some((0.25, 1.0)));
            assert_eq!(
                tied.map(|(_, _, c)| c),
                Some(here),
                "an exact tie was not kept by the first candidate offered"
            );

            // And the tie-break must not be a constant preference for `here`:
            // a strictly nearer candidate later in the order still wins.
            let last = *offered.last().expect("a vertex has neighbours");
            let strict = nearest_offered(rill_candidates(here, &geo), |c| {
                Some((if c == last { 0.1 } else { 0.25 }, 1.0))
            });
            assert_eq!(
                strict.map(|(_, _, c)| c),
                Some(last),
                "a strictly nearer later candidate did not win"
            );

            // A candidate that answers `None` is skipped rather than winning
            // with an absent distance — the `here`-has-no-trunk case.
            let skipped = nearest_offered(rill_candidates(here, &geo), |c| {
                (c != here).then_some((0.25, 1.0))
            });
            assert_eq!(
                skipped.map(|(_, _, c)| c),
                Some(offered[1]),
                "a candidate with no rills did not step aside"
            );
        }
    }

    /// The canonical grid the bound is stated for, matching
    /// `tests/rill_properties.rs`.
    const PROBE_LEVEL: u32 = 6;

    /// The hard ceiling the probe aborts at, in emitted nodes — generous
    /// against [`RILLS_PER_VERTEX_MAX`] (32,768, itself about 30% above the
    /// drawn arm's measured worst of 25,202) and small enough that a broken
    /// stopping rule reports a number instead of exhausting the box.
    ///
    /// This doc read "about 27,000" until the stale-figure sweep of fix round
    /// 1 reached it — a fourth number for a quantity that already had a
    /// derived one and a measured one, sitting 160 lines below the constant
    /// that reconciles them. Stated as a multiple of the real bound rather
    /// than as a fresh figure, so it cannot drift on its own.
    ///
    /// **This constant is the reason the probe is safe to run at all.** The
    /// defect it was written against reached 269 million nodes and 23.7 GB of
    /// resident memory before the kernel killed the process; a walk that
    /// cannot allocate past a ceiling turns that into a printed count.
    const PROBE_NODE_CAP: usize = 400_000;

    /// What one vertex's partition costs, measured by walking it under
    /// [`PROBE_NODE_CAP`].
    struct Partition {
        /// Emitted nodes — every node but the root, which is what
        /// [`rills_of`] returns one [`Rill`] for.
        nodes: usize,
        /// Nodes the partition declined to divide.
        leaves: usize,
        /// The deepest emitted node's [`Rill::depth`].
        max_depth: u32,
        /// The smallest leaf area, steradians.
        min_leaf_area: f64,
        /// The largest leaf area, steradians.
        max_leaf_area: f64,
        /// The leaves' shares summed in `u128`, so an overflow of the `u64`
        /// they are held in cannot hide a partition that creates catchment.
        leaf_share_total: u128,
        /// Whether the ceiling was hit, in which case every other field is a
        /// floor and not a measurement.
        capped: bool,
    }

    /// Walk one vertex's partition with a ceiling, counting rather than
    /// materialising.
    ///
    /// **This is the same recursion [`rills_of`] materialises**, driven by the
    /// same [`root`] and the same [`parts`]: the partition of a vertex's scalar
    /// depends only on the vertex, the cut and the unit area, never on the
    /// trunk geometry, which enters only as the line a branch's mouth is
    /// projected onto. So a synthetic stretch measures the real node count.
    fn walk(vertex: Vertex, unit: f64, cut: &CatchmentCut) -> Partition {
        let half = 0.5 * unit.sqrt();
        let stretch: Chain = [[-half, 0.0], [0.0, 0.0], [half, 0.0]];
        let mut out = Partition {
            nodes: 0,
            leaves: 0,
            max_depth: 0,
            min_leaf_area: f64::INFINITY,
            max_leaf_area: 0.0,
            leaf_share_total: 0,
            capped: false,
        };
        let mut stack = vec![root(stretch, unit)];
        while let Some(node) = stack.pop() {
            let Some(children) = parts(&node, vertex, unit, cut) else {
                out.leaves += 1;
                let area = area_of(node.share, unit);
                out.min_leaf_area = out.min_leaf_area.min(area);
                out.max_leaf_area = out.max_leaf_area.max(area);
                out.leaf_share_total += node.share as u128;
                continue;
            };
            for child in children {
                out.nodes += 1;
                out.max_depth = out.max_depth.max(child.depth - 1);
                stack.push(child);
            }
            if out.nodes > PROBE_NODE_CAP {
                out.capped = true;
                break;
            }
        }
        out
    }

    /// claim: bound(one vertex's partition, in nodes) — the recursion
    /// [`rills_of`] materialises terminates inside [`RILLS_PER_VERTEX_MAX`] on
    /// the canonical grid, under both cut arms.
    ///
    /// **The bound is asserted in node count, not stated in prose**, because
    /// prose is what failed: `rills_of`'s doc claimed "about 14,000 branches
    /// per vertex" while `tests/rill_probe.rs` said "~25,000", and a stopping
    /// rule that produced 269 million contradicted both without anything
    /// going red. A materialised recursion states its bound and is held to it.
    #[test]
    fn one_vertices_partition_is_bounded_in_nodes() {
        let geo = Geosphere::new(PROBE_LEVEL);
        let unit = vertex_catchment(&geo);
        let vertices = [Vertex(0), Vertex(1), Vertex(4_099), Vertex(20_481)];
        let mut worst = 0usize;
        for vertex in vertices {
            for (name, cut) in [
                ("even ", CatchmentCut::Even),
                ("drawn", CatchmentCut::Drawn(Seed(42))),
            ] {
                let p = walk(vertex, unit, &cut);
                assert!(
                    !p.capped,
                    "{name} {vertex:?}: the walk hit its {PROBE_NODE_CAP}-node ceiling. The \
                     stopping rule does not terminate, and the count below the ceiling is a \
                     floor rather than a measurement"
                );
                println!(
                    "{name} {vertex:?}: {} nodes, {} leaves, max depth {}, leaf area \
                     {:.4e}..{:.4e} sr ({:.3}..{:.3} of RILL_MIN_CATCHMENT)",
                    p.nodes,
                    p.leaves,
                    p.max_depth,
                    p.min_leaf_area,
                    p.max_leaf_area,
                    p.min_leaf_area / RILL_MIN_CATCHMENT,
                    p.max_leaf_area / RILL_MIN_CATCHMENT,
                );
                assert_eq!(
                    p.nodes,
                    2 * p.leaves - 2,
                    "{name} {vertex:?}: {} nodes for {} leaves — every internal node divides in \
                     two, so a full binary tree's emitted nodes are 2·leaves − 2 and anything \
                     else means a node was emitted without its sibling",
                    p.nodes,
                    p.leaves
                );
                assert!(
                    p.nodes <= RILLS_PER_VERTEX_MAX,
                    "{name} {vertex:?}: {} nodes against a stated bound of {RILLS_PER_VERTEX_MAX}. \
                     Either the bound in `rills_of`'s doc is wrong or the partition is",
                    p.nodes
                );
                assert!(
                    p.max_depth < MAX_PARTITION_DEPTH,
                    "{name} {vertex:?}: depth {} reached the {MAX_PARTITION_DEPTH} guard, which \
                     is supposed to be unreachable — the partition is being stopped by the \
                     backstop rather than by area",
                    p.max_depth
                );
                worst = worst.max(p.nodes);
            }
        }
        // The bound in BYTES as well as nodes, because the failure this test
        // exists against was a memory one: what a caller pays for one vertex is
        // the number every sweep multiplies by its vertex count.
        let rill = std::mem::size_of::<Rill>();
        println!(
            "worst over {} vertices x 2 arms: {worst} nodes = {} bytes at {rill} B/Rill; the \
             stated ceiling of {RILLS_PER_VERTEX_MAX} is {} bytes",
            vertices.len(),
            worst * rill,
            RILLS_PER_VERTEX_MAX * rill,
        );
    }

    /// The nearest branch to `target` found by walking the WHOLE partition —
    /// the control [`descend_to_nearest`]'s `O(depth)` shortcut is measured
    /// against.
    ///
    /// **Carries the same hard node cap the rest of this module's probes do**,
    /// and returns `None` if it trips, so a stopping rule that stopped
    /// stopping reports a refusal rather than eating the box. The DFS stack is
    /// bounded by depth, and nothing is materialised — the walk carries one
    /// running minimum.
    fn exhaustive_nearest(
        vertex: Vertex,
        start: Node,
        unit: f64,
        cut: &CatchmentCut,
        target: [f64; 2],
    ) -> Option<(f64, f64)> {
        let mut best: Option<(f64, f64)> = None;
        let mut seen = 0usize;
        let mut stack = vec![start];
        while let Some(node) = stack.pop() {
            let Some(children) = parts(&node, vertex, unit, cut) else {
                continue;
            };
            for child in children {
                seen += 1;
                if seen > PROBE_NODE_CAP {
                    return None;
                }
                let distance = distance_to(child.line, target);
                if best.is_none_or(|(d, _)| distance < d) {
                    best = Some((distance, area_of(child.share, unit)));
                }
                stack.push(child);
            }
        }
        best
    }

    /// claim: bound(the descent's answer against the exhaustive one, over a
    /// lattice of query points) — [`descend_to_nearest`] finds the nearest
    /// branch, or one no further out than a stated ceiling.
    ///
    /// **This is the measurement `nearest_rill`'s doc used to claim and nobody
    /// had built.** The shipped read path descends the partition choosing one
    /// child per level, so it cannot see a branch in the sibling subtree it
    /// declined to enter. Whether that matters is an empirical question about
    /// how far a branch reaches out of its own rectangle, and the honest way
    /// to answer it is to run the exhaustive walk the descent replaces and
    /// compare, on the same partition and in the same frame — which is the
    /// second thing the old claim got wrong, since `tests/rill_probe.rs`
    /// measures which coarse vertex owns a branch head, a different quantity
    /// entirely.
    ///
    /// The query lattice deliberately runs **past** the catchment square's own
    /// edges, because a position outside every child's rectangle is exactly
    /// the case the descent has to fall back on "nearer centre" for. Inside
    /// and outside are counted separately, because they are two different
    /// claims and a single mixed rate would hide which one moved.
    ///
    /// **WHAT IT MEASURED, and the split is the whole result.** Inside the
    /// catchment square the descent disagrees with the exhaustive walk on
    /// **3 of 150** query points (2.0%), by at most **0.118** of a walk-depth
    /// room's edge — under the resolution the network is drawn to, so no
    /// observer can be placed where it matters. Outside the square it
    /// disagrees on **89 of 336** (26.5%), by up to **1.099** room edges.
    ///
    /// That is a sharper statement than the old prose and a different one. The
    /// shortcut is not "the nearest branch outright, except rarely": it is
    /// **the nearest branch wherever the query is inside the catchment it
    /// subdivides**, and an approximation that degrades to about one room
    /// outside it. Which is the same boundary everything else in this module
    /// is honest about — the square is a same-area proxy, and it is at the
    /// corners that it stops being one.
    ///
    /// [`rill_reading`] is the shipped path and it takes the outside case
    /// seriously already: it runs this descent on the nearest vertex **and all
    /// of its neighbours** and takes the minimum, so a position outside one
    /// vertex's square is inside another's. That mitigation is not measured
    /// here — this isolates the shortcut itself, which is the quantity the doc
    /// claimed.
    ///
    /// The ceilings below are a **ratchet on the measured value, not a claim
    /// that the value is good**: they exist so a change that makes the
    /// shortcut worse goes red.
    #[test]
    fn the_descent_finds_what_an_exhaustive_walk_finds() {
        let geo = Geosphere::new(PROBE_LEVEL);
        let unit = vertex_catchment(&geo);
        let half = 0.5 * unit.sqrt();
        let stretch: Chain = [[-half, 0.0], [0.0, 0.0], [half, 0.0]];
        let room = RILL_MIN_CATCHMENT.sqrt();
        // (queries, disagreements, worst excess in room edges), inside the
        // catchment square and outside it.
        let mut arm = [(0usize, 0usize, 0.0_f64); 2];
        for vertex in [Vertex(0), Vertex(4_099), Vertex(20_481)] {
            for cut in [CatchmentCut::Even, CatchmentCut::Drawn(Seed(42))] {
                // A 9x9 lattice over 1.5x the square, so the outer ring sits
                // outside the catchment entirely.
                for i in 0..9 {
                    for j in 0..9 {
                        let target = [
                            (i as f64 / 4.0 - 1.0) * 1.5 * half,
                            (j as f64 / 4.0 - 1.0) * 1.5 * half,
                        ];
                        let start = root(stretch, unit);
                        // 1 when the target is OUTSIDE the patch, which is index 1 of the
                        // `["inside ", "outside"]` labels below.
                        let outside = usize::from(!start.patch.contains(target));
                        let descent = descend_to_nearest(vertex, start, unit, &cut, target)
                            .expect("the partition has parts");
                        let exhaustive = exhaustive_nearest(vertex, start, unit, &cut, target)
                            .expect("the exhaustive walk stayed inside its node cap");
                        arm[outside].0 += 1;
                        if descent.0 != exhaustive.0 {
                            arm[outside].1 += 1;
                            // Measured against the ROOM the network resolves
                            // to, not against the exhaustive distance: a query
                            // sitting almost on a branch has a near-zero
                            // denominator, and dividing by it would report an
                            // enormous relative error for an absolute
                            // difference no observer could be positioned to
                            // notice.
                            arm[outside].2 = arm[outside].2.max((descent.0 - exhaustive.0) / room);
                        }
                    }
                }
            }
        }
        for (name, &(queries, disagreed, worst)) in ["inside ", "outside"].iter().zip(arm.iter()) {
            println!(
                "the descent against the exhaustive walk, {name} the catchment square: \
                 {disagreed} of {queries} queries disagree ({:.1}%), worst excess {worst:.4} of \
                 a walk-depth room's edge",
                100.0 * disagreed as f64 / queries as f64
            );
        }
        assert!(
            arm[0].0 >= 100 && arm[1].0 >= 200,
            "the lattice no longer straddles the square: {} inside, {} outside — measured \
             150 and 336, and a comparison run entirely on one side of the boundary is not \
             the comparison this makes",
            arm[0].0,
            arm[1].0
        );
        // RECORDED, NOT DERIVED. Measured 0.1180 inside and 1.0990 outside;
        // these are the next round numbers up. A tighter bound would be a
        // claim about the construction that nothing here establishes, and a
        // looser one would stop noticing.
        assert!(
            arm[0].2 < 0.5 && arm[1].2 < 1.5,
            "the descent's excess over the true nearest is {:.4} room edges inside the \
             catchment square and {:.4} outside, against the 0.1180 / 1.0990 this was \
             recorded at. The O(depth) shortcut has got worse, and `rill_reading` is the \
             shipped path that takes it",
            arm[0].2,
            arm[1].2
        );
    }

    /// claim: invariant(the scalar partition conserves, exactly) — one vertex's
    /// leaves hold [`RILL_WHOLE`] between them, in integers, with no
    /// tolerance.
    ///
    /// **This is the keystone, and it is checkable only because the walk is
    /// bounded.** The defect that motivated the bound returned the parent's
    /// whole share as the second part, so two siblings summed to *more* than
    /// their parent and "subdivide the scalar" stopped conserving — but the
    /// test that would have said so died before its assertion. A cheap,
    /// geometry-free walk says it in milliseconds.
    #[test]
    fn one_vertices_leaves_hold_the_whole_exactly() {
        let geo = Geosphere::new(PROBE_LEVEL);
        let unit = vertex_catchment(&geo);
        for vertex in [Vertex(0), Vertex(4_099), Vertex(20_481)] {
            for cut in [CatchmentCut::Even, CatchmentCut::Drawn(Seed(42))] {
                let p = walk(vertex, unit, &cut);
                assert!(!p.capped, "{vertex:?}: the walk hit its ceiling");
                assert_eq!(
                    p.leaf_share_total, RILL_WHOLE as u128,
                    "{vertex:?}: the leaves hold {} of {RILL_WHOLE}. Summed in `u128` so a \
                     partition that CREATES catchment shows as a surplus rather than wrapping",
                    p.leaf_share_total
                );
                // A leaf is a part small enough to stop at, and no part is
                // under a quarter of its parent, so every leaf sits inside one
                // factor of CUT_FLOOR below the resolution.
                assert!(
                    p.max_leaf_area <= RILL_MIN_CATCHMENT
                        && p.min_leaf_area > CUT_FLOOR * RILL_MIN_CATCHMENT,
                    "{vertex:?}: leaf areas {:.4e}..{:.4e} fall outside \
                     ({CUT_FLOOR}·RILL_MIN_CATCHMENT, RILL_MIN_CATCHMENT]",
                    p.min_leaf_area,
                    p.max_leaf_area
                );
            }
        }
    }
}
