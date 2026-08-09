//! §7 rule 5 — at most one creature per cell, and never in the fabric — as a
//! TYPE rather than as a test over data that does not exist yet.
//!
//! Written when no creature stood in a cell at all, because a test asserting the
//! rule over lattices with no occupants would pass without examining anything, and
//! a vacuous test is worse than a missing one: it reads as coverage. So the rule is
//! enforced by the only structure that can hold an occupant — one keyed by cell,
//! whose placement REFUSES rather than overwrites.
//!
//! **The Sighting made it load-bearing.** [`crate::lattice::anchor_cells`] embeds a
//! chamber's anchors into cells and creatures resolve through it, so `Held` is now a
//! refusal that actually fires — it is one of the four legitimate reasons a
//! co-located creature has no cell, and the one that proves the type was the right
//! place for the rule. Absence from the map still never means "hidden"; see
//! `Session::sighting`.
//!
//! Refuses rather than overwrites deliberately: silently displacing whoever was
//! there is how two creatures come to believe they hold one cell.
//!
//! # The companion Task 4b's model earns
//!
//! Under the boundary model every cell was floor, so "a creature cannot be placed
//! in an impassable cell" was a sentence with no referent. A wall is a cell now, so
//! it is a real refusal — and it is enforced the same way, by the signature:
//! [`Occupancy::place`] takes the lattice, so there is no placement path that does
//! not consult passability. Making it a second, optional method would leave the
//! unchecked one available, and an invariant you can opt out of is a convention.
//!
//! It asks [`CellKind::passable`], never `== CellKind::Wall`, so the day `Rubble`
//! arrives a creature is refused from it without this file changing.

use super::{Cell, CellKind, Lattice, classify::kind_of};
use hornvale_kernel::EntityId;
use std::collections::BTreeMap;

/// Why a placement was refused.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Refusal {
    /// Somebody else already holds the cell. Carries who, so the caller can say
    /// so rather than guess.
    Held(EntityId),
    /// The cell is the building's fabric, not standing room. Carries the kind, so
    /// a diagnostic can name what is in the way.
    Impassable(CellKind),
    /// The cell is not part of this plan at all.
    Outside,
}

/// Who stands where. `FRAME`-tier like the lattice itself (decision 0069) —
/// derived on entry, discarded on exit, never serialized.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Occupancy(BTreeMap<Cell, EntityId>);

impl Occupancy {
    /// Place `who` at `cell` of `lattice`, or refuse and say why.
    ///
    /// Both halves of §7 rule 5 in one gate: the cell must be somewhere a mover
    /// could be at all, and it must not already be held by someone else. Placing
    /// a creature where it already stands is not a conflict.
    pub fn place(&mut self, lattice: &Lattice, cell: Cell, who: EntityId) -> Result<(), Refusal> {
        match kind_of(lattice, cell) {
            None => return Err(Refusal::Outside),
            Some(k) if !k.passable() => return Err(Refusal::Impassable(k)),
            Some(_) => {}
        }
        match self.0.get(&cell) {
            Some(&held) if held != who => Err(Refusal::Held(held)),
            _ => {
                self.0.insert(cell, who);
                Ok(())
            }
        }
    }

    /// Who stands at `cell`, if anyone.
    pub fn at(&self, cell: Cell) -> Option<EntityId> {
        self.0.get(&cell).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::brief::Brief;
    use crate::lattice::{embed_with, extent_for};
    use crate::structure::structure_at;
    use hornvale_kernel::{RoomAddr, Seed};

    const WALK: u32 = 12;

    /// `EntityId` wraps a `NonZeroU64` (`kernel/src/ledger.rs:14`), so an id is
    /// built rather than written as a literal.
    fn id(n: u64) -> EntityId {
        EntityId(std::num::NonZeroU64::new(n).expect("nonzero"))
    }

    fn plan() -> Lattice {
        let addr = RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        };
        let brief = Brief::from_parts(None, None, None, None, 0, true, true);
        let s = structure_at(&addr, &brief, Seed(42), WALK).expect("built");
        embed_with(&s, &brief, extent_for(&s), Seed(42))
    }

    /// A cell of the given kind in `l`, by predicate. Found rather than written as
    /// a coordinate, so the tests survive the extent moving.
    fn a_cell(l: &Lattice, want: fn(&CellKind) -> bool) -> Cell {
        *l.cells
            .iter()
            .find(|(_, k)| want(k))
            .expect("the plan holds a cell of that kind")
            .0
    }

    #[test]
    fn rule_5_a_cell_holds_at_most_one_creature() {
        let l = plan();
        let floor = a_cell(&l, |k| matches!(k, CellKind::Floor(_)));
        let mut o = Occupancy::default();
        let (a, b) = (id(1), id(2));
        assert!(o.place(&l, floor, a).is_ok());
        assert_eq!(
            o.place(&l, floor, b),
            Err(Refusal::Held(a)),
            "the second creature must be refused, and told who holds the cell"
        );
        assert_eq!(o.at(floor), Some(a), "the refusal must not displace");
        assert!(
            o.place(&l, floor, a).is_ok(),
            "placing the same creature where it already stands is not a conflict"
        );
    }

    #[test]
    fn rule_5_a_creature_cannot_be_placed_in_the_fabric() {
        // The companion the reification earns. A wall is a cell, so this refusal
        // has a referent for the first time.
        let l = plan();
        let wall = a_cell(&l, |k| *k == CellKind::Wall);
        let mut o = Occupancy::default();
        assert_eq!(
            o.place(&l, wall, id(1)),
            Err(Refusal::Impassable(CellKind::Wall)),
            "a creature standing inside a wall is not a position, it is a bug"
        );
        assert_eq!(o.at(wall), None, "a refused placement must record nothing");
    }

    #[test]
    fn rule_5_a_threshold_is_somewhere_a_creature_may_stand() {
        // The other direction, and it is not decoration: a threshold that refused
        // occupants would make a doorway a thing you pass through without ever
        // being in, which is what `passable()` exists to avoid asserting variant
        // by variant.
        let l = plan();
        let door = a_cell(&l, |k| matches!(k, CellKind::Threshold(_, _)));
        let mut o = Occupancy::default();
        assert!(o.place(&l, door, id(1)).is_ok(), "{door:?}");
    }

    #[test]
    fn rule_5_a_creature_cannot_be_placed_outside_the_plan() {
        let l = plan();
        let mut o = Occupancy::default();
        assert_eq!(
            o.place(&l, Cell(-1, -1), id(1)),
            Err(Refusal::Outside),
            "outside the extent is a distinct refusal from inside a wall — \
             `kind_of` returning None means exactly one thing now"
        );
    }
}
