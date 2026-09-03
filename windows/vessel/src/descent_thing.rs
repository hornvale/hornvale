//! Identities for things the descent plan places (The Brattice, spec §3.7):
//! a key at a node, a door on an edge. Keyed on the PLAN POSITION —
//! `(vertex, level, grid cell[s])` — on the `thing@passage/<addr>/<kind>`
//! precedent ([`crate::passage::cave_mouth_role`]). Spec §5: from the first
//! saved custody fact naming one of these, the plan grammar is a save-format
//! contract.
//!
//! # What that costs, stated where the spelling is
//!
//! Every plan, gate and level is `FRAME`-tier under decision 0069 — derived
//! on entry, discarded on exit — and nothing here changes that. What changes
//! is that a descent key's *identity* is a function of a plan node, and
//! `take` posts a [`crate::thing::LOCATED_IN`] fact whose **subject** is that
//! identity. From the first world saved holding a descent key, a change to
//! `underworld/plan/v1`'s draws or to `underworld/gate/v1/pattern`'s
//! selection would leave that world naming a key that no longer exists. So
//! such a change is an **epoch** — `underworld/plan/v2` or
//! `underworld/gate/v2`, never a silent edit; the spec's §5 is the statement
//! and this campaign's decision block records it — exactly as a
//! change to [`crate::thing::thing_role`]'s or
//! [`crate::passage::cave_mouth_role`]'s spelling is.
//!
//! The three spellings below are themselves save-format contracts on those
//! same terms, and `roles_are_pure_functions_of_the_plan_position_and_order_
//! the_door_cells` writes each one out as a literal so a spelling change
//! cannot be rebaselined.
//!
//! # The namespaces cannot collide
//!
//! A room thing's role is `thing@<packed FacetId>/<kind>` — a decimal — and a
//! cave mouth's is `thing@passage/<addr key>/<kind>`. This one leads with
//! `descent`, which is neither a decimal nor `passage`, so no facet id and no
//! chamber address can ever spell it. That is worth stating because all three
//! feed ONE derivation ([`crate::thing::id_for_role`]) and a collision there
//! would be two things wearing one identity.

use hornvale_kernel::{EntityId, Ledger, Vertex, WorldTime};
use hornvale_worldgen::circuit::{GridCell, NodeId};

use crate::lattice::Cell;
use crate::underground::Underground;

/// The kind label a descent door wears — [`hornvale_thing::kinds::DOOR`]'s
/// own string, named once here so the role spelling and the `instance-of`
/// object cannot drift apart.
pub(crate) const DOOR: &str = hornvale_thing::kinds::DOOR.0;

/// The kind label a descent key wears.
pub(crate) const KEY: &str = hornvale_thing::kinds::KEY.0;

/// The REGION a plan node names, as save-format text: the descent's vertex,
/// the level index, and the node's own grid cell.
///
/// **The region, never the level cell, is the committed grain** (spec §3.7).
/// A thing dropped underground gets this string as its
/// [`crate::thing::LOCATED_IN`] object, so `look` can list it again; a level
/// cell would be a fine position of a thing derived from a `FRAME`-tier
/// level, and the region is the coarsest address that still answers "is it
/// where I left it".
/// type-audit: bare-ok(index: level), bare-ok(identifier-text: return)
pub(crate) fn region_key(vertex: Vertex, level: usize, cell: GridCell) -> String {
    format!("descent/{}/{}/{}.{}", vertex.0, level, cell.col, cell.row)
}

/// The role leg of the key lying at a plan node.
/// type-audit: bare-ok(index: level), bare-ok(identifier-text: return)
pub(crate) fn key_role(vertex: Vertex, level: usize, cell: GridCell) -> String {
    format!("thing@{}/key", region_key(vertex, level, cell))
}

/// The role leg of the door hung on the edge between two plan nodes.
///
/// **Lesser cell first, so a door has one name however you cross it.** The
/// edge is undirected — a `Needs(Key(_))` gate on a passage is stamped
/// symmetrically ([`Underground::has_door`]'s own doc) — so a role spelled
/// from the caller's arrival order would give the same door two identities
/// and two independent openness folds.
/// type-audit: bare-ok(index: level), bare-ok(identifier-text: return)
pub(crate) fn door_role(vertex: Vertex, level: usize, a: GridCell, b: GridCell) -> String {
    let (lo, hi) = if (a.col, a.row) <= (b.col, b.row) {
        (a, b)
    } else {
        (b, a)
    };
    format!(
        "thing@descent/{}/{}/{}.{}-{}.{}/door",
        vertex.0, level, lo.col, lo.row, hi.col, hi.row
    )
}

/// The entity the key at this plan node has, whether or not anything has ever
/// promoted it — a pure derivation, exactly like [`crate::thing::thing_id`].
///
/// Ordinal 0 unconditionally: a node holds at most one key by construction
/// (spec §3.2 step 3), so there is no sibling for an ordinal to distinguish.
/// type-audit: bare-ok(index: level)
pub(crate) fn key_id(vertex: Vertex, level: usize, cell: GridCell) -> EntityId {
    crate::thing::id_for_role(&key_role(vertex, level, cell), 0)
}

/// The entity the door on this plan edge has. Ordinal 0 for
/// [`key_id`]'s reason: an edge carries at most one gate.
/// type-audit: bare-ok(index: level)
pub(crate) fn door_id(vertex: Vertex, level: usize, a: GridCell, b: GridCell) -> EntityId {
    crate::thing::id_for_role(&door_role(vertex, level, a, b), 0)
}

/// The plan node whose region holds `cell` on the rung `ug` currently
/// occupies, if any.
///
/// **Asked of the PLAN's rectangles, not of the realized cells**, because
/// that is what a region key is a function of. A cell in the divider between
/// two regions belongs to whichever region's rectangle contains it — a
/// threshold cell may therefore answer with either of its two nodes, or with
/// neither, and no caller here depends on which: the key fold asks about the
/// standing cell, which the realizer only ever places inside a region.
pub(crate) fn node_here(ug: &Underground, cell: Cell) -> Option<NodeId> {
    (0..ug.plan.nodes.len()).find(|&n| {
        ug.plan.nodes[n].level as usize == ug.rung && {
            let r = ug.plan.region_of(n);
            cell.0 >= r.x && cell.0 < r.x + r.w && cell.1 >= r.y && cell.1 < r.y + r.h
        }
    })
}

/// The region key for the cell `ug`'s possession stands on, if it lies in a
/// region at all.
pub(crate) fn region_here(ug: &Underground) -> Option<String> {
    let n = node_here(ug, ug.cell)?;
    let node = ug.plan.nodes[n];
    Some(region_key(ug.vertex, node.level as usize, node.cell))
}

/// The door anchored at `cell`, with the plan node holding its key — `None`
/// for every cell that is not a gated threshold.
///
/// **The plan is the only source, and the cells cannot be one.** Every
/// passage's crossing is a `Threshold` whether the plan gated it or not, so
/// the door is a fact about the edge; [`Underground::has_door`] is the
/// boolean half of this same read and this is the identified half. The two
/// stay separate because the walk (`underground.rs`) holds no ledger and must
/// not learn about [`EntityId`]s to ask whether a door is there at all.
///
/// Reads `toward_a` alone, for [`Underground::has_door`]'s own reason: a
/// `Needs(Key(_))` gate on a passage is symmetric, so asking one way is
/// asking both.
pub(crate) fn door_at(ug: &Underground, cell: Cell) -> Option<(EntityId, NodeId)> {
    door_parts(ug, cell).map(|(level, a, b, holder)| (door_id(ug.vertex, level, a, b), holder))
}

/// [`door_at`]'s role leg — what [`crate::thing::set_openness_role`] and
/// [`crate::thing::set_lockedness_role`] need, which an [`EntityId`] cannot
/// be turned back into.
///
/// Both wrappers are one line over [`door_parts`] rather than two derivations
/// that agree: an id and the role it was derived from must not be able to
/// disagree about which door they name, which is the same seam
/// [`crate::thing::thing_lineage`]'s own doc records as having been live once.
pub(crate) fn door_role_at(ug: &Underground, cell: Cell) -> Option<String> {
    door_parts(ug, cell).map(|(level, a, b, _)| door_role(ug.vertex, level, a, b))
}

/// The door's PLAN POSITION — level and the two nodes' grid cells — plus the
/// node holding its key, or `None` where the plan hangs no door. The two
/// wrappers above spell an id and a role from it, both through
/// [`door_role`], so there is one spelling and not two.
fn door_parts(ug: &Underground, cell: Cell) -> Option<(usize, GridCell, GridCell, NodeId)> {
    let (a, b) = ug.threshold_edge(cell)?;
    let (_, gate) = ug.plan.gate_between(a, b)?;
    let hornvale_worldgen::brattice::Way::Needs(hornvale_worldgen::brattice::Requirement::Key(
        holder,
    )) = gate.toward_a
    else {
        return None;
    };
    let na = ug.plan.nodes[a];
    let nb = ug.plan.nodes[b];
    Some((na.level as usize, na.cell, nb.cell, holder))
}

/// The entity the key held by plan node `holder` has — what `open` compares
/// the body's custody against (spec §3.7: the binding is structural, never a
/// table and never a fact).
pub(crate) fn key_id_of_node(ug: &Underground, holder: NodeId) -> EntityId {
    let node = ug.plan.nodes[holder];
    key_id(ug.vertex, node.level as usize, node.cell)
}

/// The key the plan puts at the region underfoot, with its role leg — a
/// PURE PLAN READ, answering whether this node holds a key at all and saying
/// nothing about where that key has since got to.
///
/// The role travels beside the id because `take` must PROMOTE a key nothing
/// has touched, and [`crate::thing::promote_role`] needs the spelling an
/// [`EntityId`] cannot be turned back into.
pub(crate) fn key_here(ug: &Underground) -> Option<(EntityId, String)> {
    let n = node_here(ug, ug.cell)?;
    let node = ug.plan.nodes[n];
    node.key?;
    let level = node.level as usize;
    Some((
        key_id(ug.vertex, level, node.cell),
        key_role(ug.vertex, level, node.cell),
    ))
}

/// Every thing lying in the region the possession stands in, as of `day`:
/// the node's own LATENT key (the plan put it there and no fact has moved
/// it) plus everything a body has set down here.
///
/// **Two sources, one list, and the latent half is the one the ledger cannot
/// supply.** A key the plan places has no `located-in` fact at all until
/// somebody touches it — that absence IS its position, the same "no fact
/// means whatever the seed drew" rule [`crate::thing::is_open`] states for
/// openness — so a fold over postings alone would report an empty floor in
/// the one region that certainly holds something. [`crate::thing::
/// lying_at_place`] supplies the other half, and a key taken and set back
/// down arrives through it rather than through the latent arm, which is why
/// the result is deduped.
///
/// Ascending [`EntityId`] order, never ledger order, on
/// [`crate::thing::held_by`]'s own terms.
pub(crate) fn things_lying_here(
    ug: &Underground,
    ledger: &Ledger,
    day: WorldTime,
) -> Vec<EntityId> {
    let Some(place) = region_here(ug) else {
        return Vec::new();
    };
    let mut out: std::collections::BTreeSet<EntityId> =
        crate::thing::lying_at_place(ledger, &place, day)
            .into_iter()
            .collect();
    if let Some((key, _)) = key_here(ug)
        && crate::thing::location_of(ledger, key, day).is_none()
    {
        out.insert(key);
    }
    out.into_iter().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::affordance::ObjectProperty;

    /// The three spellings, written out as literals so a change to any of
    /// them cannot be rebaselined — the same discipline
    /// `the_thing_role_spelling_is_the_permanent_lineage_key`
    /// (`crate::thing`) and
    /// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key`
    /// (`tests/suite/passage.rs`) already hold their own namespaces to. Every
    /// other property a role could have — injectivity, agreement between two
    /// derivations — is satisfied by ANY injective spelling; only a literal
    /// can fail on a spelling change.
    ///
    /// The door's own half is the ordering claim: an edge is undirected, so
    /// the two crossing orders must produce one name.
    #[test]
    fn roles_are_pure_functions_of_the_plan_position_and_order_the_door_cells() {
        let v = Vertex(7);
        assert_eq!(
            region_key(v, 2, GridCell { col: 3, row: 1 }),
            "descent/7/2/3.1"
        );
        assert_eq!(
            key_role(v, 2, GridCell { col: 3, row: 1 }),
            "thing@descent/7/2/3.1/key"
        );
        let a = GridCell { col: 3, row: 1 };
        let b = GridCell { col: 4, row: 1 };
        assert_eq!(
            door_role(v, 2, a, b),
            door_role(v, 2, b, a),
            "a door has one name however you cross it"
        );
        assert_eq!(door_role(v, 2, a, b), "thing@descent/7/2/3.1-4.1/door");
    }

    /// The two derivations that feed [`crate::thing::id_for_role`] are
    /// distinct, and distinct from a room thing's — the collision claim the
    /// module doc makes, asserted rather than argued.
    #[test]
    fn a_key_a_door_and_a_room_thing_never_share_an_identity() {
        let v = Vertex(7);
        let a = GridCell { col: 3, row: 1 };
        let b = GridCell { col: 4, row: 1 };
        let key = key_id(v, 2, a);
        let door = door_id(v, 2, a, b);
        assert_ne!(key, door, "a key and a door on its own edge are two things");
        let room = crate::thing::thing_id(
            &hornvale_kernel::Facet {
                face: 0,
                path: vec![1],
            },
            "key",
            0,
        )
        .expect("a shallow facet packs");
        assert_ne!(key, room, "a descent key is not a room's key");
        // And the level matters: the same grid cell one rung down is a
        // different node and must be a different key.
        assert_ne!(key, key_id(v, 3, a), "level is part of the identity");
        assert_ne!(key, key_id(Vertex(8), 2, a), "vertex is part of it too");
    }

    /// The kind itself: `door` joins the roster carrying the cave mouth's
    /// properties plus the strongbox's lock (spec §3.7).
    #[test]
    fn the_door_kind_carries_passage_lid_and_lock() {
        let reg = crate::affordance::object_registry();
        let t = reg
            .get(&hornvale_kernel::KindId("door"))
            .expect("door is registered");
        for p in [
            ObjectProperty::AffordsPassage,
            ObjectProperty::Openable,
            ObjectProperty::Lockable,
        ] {
            assert!(t.properties.contains(&p), "door must carry {p:?}");
        }
        assert!(hornvale_thing::THING_KINDS.contains(&"door"));
        assert_eq!(DOOR, "door");
        assert_eq!(KEY, "key");
    }
}
