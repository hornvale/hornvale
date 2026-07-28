//! §7 rule 5 — at most one creature per cell — as a TYPE rather than as a test
//! over data that does not exist yet.
//!
//! No creature stands in a cell until The Sighting, so a test asserting the rule
//! over today's lattices would pass without examining anything. A vacuous test is
//! worse than a missing one: it reads as coverage. So the rule is enforced by the
//! only structure that can hold an occupant — one keyed by cell, whose placement
//! REFUSES rather than overwrites.
//!
//! Refuses rather than overwrites deliberately: silently displacing whoever was
//! there is how two creatures come to believe they hold one cell.

use super::Cell;
use hornvale_kernel::EntityId;
use std::collections::BTreeMap;

/// Who stands where. `FRAME`-tier like the lattice itself (decision 0069) —
/// derived on entry, discarded on exit, never serialized.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Occupancy(BTreeMap<Cell, EntityId>);

impl Occupancy {
    /// Place `who` at `cell`, or refuse with whoever already holds it.
    pub fn place(&mut self, cell: Cell, who: EntityId) -> Result<(), EntityId> {
        match self.0.get(&cell) {
            Some(&held) if held != who => Err(held),
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

    /// `EntityId` wraps a `NonZeroU64` (`kernel/src/ledger.rs:14`), so an id is
    /// built rather than written as a literal.
    fn id(n: u64) -> EntityId {
        EntityId(std::num::NonZeroU64::new(n).expect("nonzero"))
    }

    #[test]
    fn rule_5_a_cell_holds_at_most_one_creature() {
        let mut o = Occupancy::default();
        let (a, b) = (id(1), id(2));
        assert!(o.place(Cell(3, 3), a).is_ok());
        assert_eq!(
            o.place(Cell(3, 3), b),
            Err(a),
            "the second creature must be refused, and told who holds the cell"
        );
        assert_eq!(o.at(Cell(3, 3)), Some(a), "the refusal must not displace");
        assert!(
            o.place(Cell(3, 3), a).is_ok(),
            "placing the same creature where it already stands is not a conflict"
        );
    }
}
