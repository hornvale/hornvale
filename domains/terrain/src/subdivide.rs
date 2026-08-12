//! Sub-cell drainage on the room mesh (The Rill, Tier 2): where a room's
//! water goes, and how much of it there is, **below the resolution of the
//! coarse flow graph** — resolved by descending a [`RoomAddr`], in `O(depth)`,
//! with nothing stored.
//!
//! # Why this is not `MAP-subcell-hydrology`
//!
//! The idea registry **rejects** refining elevation per cell and recomputing
//! flow below the cell floor, because a lazily-refined flow field carries no
//! guarantee of agreeing with the coarse answer it constitutionally may not
//! contradict. Nothing here recomputes flow. **This module never reads
//! [`crate::globe::TectonicGlobe::elevation`] at all** — not at cell scale and
//! certainly not below it. It reads only the coarse graph's own *answers*
//! (`downhill`, `drainage`, `water_kind`), and below the cell floor it reads
//! nothing but the room address: the coarse outflow is imposed as a boundary
//! condition on every level of the subdivision, so agreement with the coarse
//! graph is **structural** rather than hoped for.
//!
//! # What is inherited and what is invented, stated plainly
//!
//! The coarse flow graph is a flow on **cells**, and cells are the *vertices*
//! of the icosphere mesh while rooms are its *faces*. A `downhill` edge
//! `c -> t` joins two adjacent vertices, so it runs **along a room's boundary
//! and never through a room's interior**. There is therefore no canonical lift
//! of the coarse flow onto rooms: every room-level flow is an invention, and
//! saying otherwise would be the campaign's own face/cell duality trap wearing
//! a third coat.
//!
//! So the invention is confined to one place — [`floor_outflow`], at the cell
//! floor — and it is made from committed coarse state alone:
//!
//! - A **flow room** is a room at the globe level whose three corner cells are
//!   all *reaches* (not ocean, and with a `downhill` target). Anything else is
//!   terminal, which is how the fine network ends at a coast and at a salt
//!   basin without either being a special case.
//! - Its **potential** is the sum of its three corner cells' coarse
//!   `drainage`. Drainage is the coarse graph's own accumulation and increases
//!   strictly downstream, so "flow toward greater potential" *is* "flow the way
//!   the coarse graph says the water goes", expressed on faces.
//! - A room drains across the edge to the **strictly greater** such neighbour,
//!   greatest first, ties by lowest packed [`hornvale_kernel::RoomId`]. Strict
//!   increase makes the floor graph acyclic by construction. Where no
//!   neighbour is strictly greater the room is terminal — and that is the
//!   deliberate conservative choice: **where the coarse graph has no opinion,
//!   this module invents no direction.**
//!
//! Below the floor nothing is invented except one bit per room: which of the
//! **two** children on the parent's outflow edge carries the outlet
//! ([`crate::streams::SUBCELL_OUTLET`]). Everything else is forced by the mesh —
//! see [`flow_at`].
//!
//! # Everything here is angular, and the units are the whole point
//!
//! [`SubFlow::upstream`] is a count **in the querying room's own depth's
//! sub-triangle units**, and [`room_spacing`] is that same depth's angular
//! spacing. The two must be paired, and they must be paired *at the same
//! depth*: `channel_half_width(flow.upstream, room_spacing(addr))` is the
//! only correct call, and pairing a depth-`d` count with a depth-`d-1` spacing
//! is the trap [`crate::channel::channel_half_width`]'s own doc names, in the
//! direction Task 1's pure-function test structurally cannot see. That pairing
//! is asserted in `tests/rill_properties.rs`.

use crate::globe::TectonicGlobe;
use crate::water::WaterKind;
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{CellId, Geosphere, NearestCellIndex, RoomAddr, Seed, math};

/// Where a room's water goes below cell scale, and how much of it there is.
///
/// Both fields are stated in the **querying room's own depth**: `outlet` is a
/// room at that same depth, and `upstream` counts sub-triangles of that depth.
/// Neither is ever serialized — a subdivision path is not a save-format
/// quantity, and `RoomAddr` already carries the durable identity.
/// type-audit: bare-ok(count: upstream)
#[derive(Clone, Debug, PartialEq)]
pub struct SubFlow {
    /// The edge-adjacent room this room's water leaves into, at the same
    /// depth. Always one of `addr.neighbors()`.
    pub outlet: RoomAddr,
    /// Accumulated upstream area, counted in **this room's own depth's**
    /// sub-triangles and including the room itself, so it is never below 1.
    ///
    /// Multiply by the room's own solid angle to get a drained **area** in
    /// steradians, which is the scale-free form: a room's drained area is
    /// invariant under subdivision, while this count quadruples per level and
    /// [`room_spacing`] halves. That invariance is what makes
    /// [`crate::channel::channel_half_width`] give the same width at every
    /// depth, and it is why there is no count threshold anywhere in this
    /// module (see this module's own doc, and decision 0129).
    pub upstream: f64,
}

/// Dot product of two 3-vectors.
fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

/// Angular separation of two unit vectors, radians.
fn angle(a: [f64; 3], b: [f64; 3]) -> f64 {
    math::acos(dot(a, b).clamp(-1.0, 1.0))
}

/// The room's own angular spacing, radians: the mean of the three arc lengths
/// of its own triangle at its own depth.
///
/// **This is the spacing a [`SubFlow::upstream`] count must be paired with,
/// and the reason it is derived from [`RoomAddr::corners`] rather than from a
/// [`Geosphere`] is that below cell scale there is no `Geosphere` to ask** —
/// level 12 would be `10·4¹² + 2 = 167,772,162` cells. The correspondence is
/// exact rather than analogical: a room's corners are mesh *vertices*, so at
/// the globe's own level a room's three edges are three cell-to-cell
/// separations — the same quantity `channel.rs`'s `cell_spacing` averages,
/// taken around a face instead of around a cell.
/// type-audit: pending(wave-1: return)
pub fn room_spacing(addr: &RoomAddr) -> f64 {
    let [a, b, c] = addr.corners();
    (angle(a, b) + angle(b, c) + angle(c, a)) / 3.0
}

/// A **reach**: land with somewhere to send its water. The same predicate
/// `ChannelNetwork::build` walks on, restated here rather than shared so that
/// this module reads committed globe state directly and not the channel
/// network's opinion of it.
fn is_reach(globe: &TectonicGlobe, c: CellId) -> bool {
    !matches!(*globe.water_kind.get(c), WaterKind::Ocean) && globe.downhill.get(c).is_some()
}

/// A floor room's potential: the sum of its three corner cells' coarse
/// `drainage`. `None` unless the room is a **flow room** — every corner cell a
/// reach — which is what makes a coastal or salt-basin face terminal without a
/// special case.
///
/// `None` also when `addr` is coarser than the grid, because
/// [`RoomAddr::corner_weights`] is.
fn floor_potential(
    addr: &RoomAddr,
    globe: &TectonicGlobe,
    geo: &Geosphere,
    index: &NearestCellIndex,
) -> Option<f64> {
    let corners = addr.corner_weights(geo, index)?;
    let mut sum = 0.0;
    for (cell, _weight) in corners {
        if !is_reach(globe, cell) {
            return None;
        }
        sum += *globe.drainage.get(cell);
    }
    Some(sum)
}

/// The coarse boundary condition at the cell floor: which of a globe-level
/// room's three edges its water leaves by, and how much water that is in
/// floor-room units.
///
/// `None` when the room is not a flow room, or when no edge-neighbour is a
/// strictly-greater flow room — a terminal face, which is what a coast, a salt
/// basin, and a local maximum of the coarse accumulation all reduce to.
///
/// **The reference is the coarse graph and nothing else.** `drainage` and
/// `water_kind`/`downhill` are the coarse answer; no elevation is read, no
/// flow is recomputed, and no seed is consumed — the floor is entirely
/// determined by committed state, so the seeded freedom this module has lives
/// strictly *below* it.
///
/// The returned count converts cells to floor rooms by the mesh's own ratio,
/// `20·4^L` faces over `10·4^L + 2` cells (1.99990 at the canonical level 6) —
/// the face/cell duality stated as arithmetic rather than assumed away. The
/// per-room share is the **mean** of the three corners' accumulations: a room
/// lies between its three cells and carries no more of the catchment than they
/// average. Floored at one room, because a room always drains itself.
fn floor_outflow(
    addr: &RoomAddr,
    globe: &TectonicGlobe,
    geo: &Geosphere,
    index: &NearestCellIndex,
) -> Option<(usize, f64)> {
    let own = floor_potential(addr, globe, geo, index)?;
    let neighbors = addr.neighbors();
    let mut best: Option<(usize, f64, u64)> = None;
    for (i, neighbor) in neighbors.iter().enumerate() {
        let Some(potential) = floor_potential(neighbor, globe, geo, index) else {
            continue;
        };
        if potential.total_cmp(&own).is_le() {
            continue;
        }
        let id = neighbor.pack().ok()?.0;
        let better = match best {
            None => true,
            Some((_, best_potential, best_id)) => match potential.total_cmp(&best_potential) {
                std::cmp::Ordering::Greater => true,
                std::cmp::Ordering::Equal => id < best_id,
                std::cmp::Ordering::Less => false,
            },
        };
        if better {
            best = Some((i, potential, id));
        }
    }
    let (edge, _, _) = best?;
    let faces = (20u64 << (2 * geo.level())) as f64;
    let per_cell = faces / geo.cell_count() as f64;
    Some((edge, (own / 3.0 * per_cell).max(1.0)))
}

/// Which of the two children on a parent's outflow edge carries the outlet.
///
/// **The one degree of freedom in the whole subdivision**, and it exists
/// because the campaign spec was wrong about the geometry: the parent's
/// outflow edge is shared by the two corner children `(edge+1)%3` and
/// `(edge+2)%3` — verified in `tests/rill_properties.rs` — so the outlet is
/// constrained to a choice of two rather than forced to one.
///
/// Hash-noise only: [`crate::streams::SUBCELL_OUTLET`] is derived once per world and
/// sub-derived per room **address**, so no `Stream` is shared, no draw order
/// exists, and no save-format contract is created beyond the label itself.
/// Keyed on the address rather than on a position because this is a discrete
/// choice about a discrete object; the continuity argument that rules
/// address-hashing out for [`crate::streams::CHANNEL_MEANDER`] is about a
/// *displacement field*, which has to be continuous to make a connected
/// watercourse. Connectedness here is carried by the incidence, not by the
/// hash.
fn outlet_child(parent: &RoomAddr, edge: usize, seed: Seed) -> Option<u8> {
    let id = parent.pack().ok()?.0;
    let drawn = seed
        .derive(StreamLabel::dynamic(&id.to_string()))
        .stream()
        .next_u64();
    let offset = if drawn & 1 == 0 { 1 } else { 2 };
    Some(((edge + offset) % 3) as u8)
}

/// Where `addr`'s water goes below cell scale, and how much of it there is —
/// `None` where the coarse graph gives the containing face no outflow (an
/// ocean or coastal face, a salt basin, a local maximum of coarse
/// accumulation), and `None` for a room coarser than the grid.
///
/// `seed` is the **already-derived** [`crate::streams::SUBCELL_OUTLET`] leg
/// ([`TectonicGlobe::subcell_flow_seed`]), never the terrain root: handing a
/// root seed here would grant this path the ability to derive every other
/// terrain stream, the same leak `channel_seed`'s own doc warns against.
///
/// # The descent, and why the sub-network cannot contradict the coarse one
///
/// The floor supplies an **outflow edge** for the globe-level ancestor. Every
/// level below it is then forced by the mesh's incidence, which
/// `tests/rill_properties.rs` asserts from `RoomAddr` rather than assuming:
///
/// - The four children are a **star**: the central child is edge-adjacent to
///   all three corner children, and the corner children are pairwise
///   non-adjacent (they meet only at midpoints). So once the outlet child is
///   chosen there is no spanning tree left to draw — the two other corner
///   children can only reach the outlet through the centre.
/// - The central child touches no parent edge along a segment, so it can never
///   be the outlet. That is geometry, not a rule imposed here.
/// - The outlet child leaves across the parent's outflow edge through its own
///   local neighbour index **2** if it is `(edge+1)%3`, and index **1** if it
///   is `(edge+2)%3`.
/// - The central child reaches corner child `o` through its own local
///   neighbour index `(o+1)%3` — a rotation, because the central child's
///   corners are the parent's midpoints in the order `[m01, m12, m20]`. The
///   first draft of this module assumed the identity and was refuted by the
///   incidence test in the same commit.
/// - A non-outlet corner child reaches the centre through its own index **0**.
///
/// So **exactly one child drains out of the parent, across the parent's own
/// outflow edge**, and the other three drain inward. Applied at every level,
/// that is the guarantee: a room's water leaves its globe-level face across
/// the edge the coarse graph chose for that face, whatever depth it is asked
/// at. Nothing below the floor can move it.
///
/// # Accumulation
///
/// One level down, the parent's own area is four children and its inherited
/// inflow is `4·(upstream − 1)` in child units. The inflow enters at the two
/// non-outlet corner children, split evenly — the choice this module makes,
/// and the only accumulation choice it makes: they are the two children that
/// between them cover the whole of both non-outflow parent edges, which is
/// where inflow physically arrives. Then
///
/// ```text
///   inflow child  = 1 + I/2          (each of the two)
///   centre        = 1 + 2·(1 + I/2)  = 3 + I
///   outlet child  = 1 + (3 + I)      = 4 + I = 4 · upstream(parent)
/// ```
///
/// so the accumulation leaving the parent equals the parent's own, converted
/// to child units, exactly. That identity is R-3, and it is what makes the
/// width law scale-free through the subdivision: `4·count` against `spacing/2`
/// is precisely the invariance Task 1 pinned.
pub fn flow_at(
    addr: &RoomAddr,
    globe: &TectonicGlobe,
    geo: &Geosphere,
    index: &NearestCellIndex,
    seed: Seed,
) -> Option<SubFlow> {
    let level = geo.level();
    if addr.depth() < level {
        return None;
    }
    let floor = addr.ancestor(level)?;
    let (mut edge, mut upstream) = floor_outflow(&floor, globe, geo, index)?;
    for k in (level as usize)..addr.path.len() {
        let parent = addr.ancestor(k as u32)?;
        let outlet = outlet_child(&parent, edge, seed)?;
        let inflow = 4.0 * (upstream - 1.0);
        let digit = addr.path[k];
        if digit == outlet {
            edge = if usize::from(outlet) == (edge + 1) % 3 {
                2
            } else {
                1
            };
            upstream *= 4.0;
        } else if digit == 3 {
            edge = (usize::from(outlet) + 1) % 3;
            upstream = 3.0 + inflow;
        } else {
            edge = 0;
            upstream = 1.0 + inflow / 2.0;
        }
    }
    Some(SubFlow {
        outlet: addr.neighbors()[edge].clone(),
        upstream,
    })
}

/// The coarse boundary condition itself, published so a test can state R-4
/// against it without reaching into this module's private descent: the
/// globe-level face `addr` lies in, and the face its water leaves into.
/// `None` exactly where [`flow_at`] is `None`.
///
/// Published for that reason and no other — a consumer wanting sub-cell flow
/// wants [`flow_at`], which is the same answer resolved all the way down.
pub fn floor_flow(
    addr: &RoomAddr,
    globe: &TectonicGlobe,
    geo: &Geosphere,
    index: &NearestCellIndex,
) -> Option<(RoomAddr, RoomAddr)> {
    let floor = addr.ancestor(geo.level())?;
    let (edge, _) = floor_outflow(&floor, globe, geo, index)?;
    let outlet = floor.neighbors()[edge].clone();
    Some((floor, outlet))
}
