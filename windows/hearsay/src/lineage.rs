//! The community tree, read from `occ-founded-from`.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeMap;

/// The founding tree: who was founded from whom.
#[derive(Clone, Debug, Default)]
pub struct Lineage {
    parent: BTreeMap<EntityId, EntityId>,
    roots: Vec<EntityId>,
}

impl Lineage {
    /// The occupation this one was founded from, or `None` for a root.
    pub fn parent(&self, of: EntityId) -> Option<EntityId> {
        self.parent.get(&of).copied()
    }

    /// Occupations founded at a site rather than from another community,
    /// ascending. Each is an independent origin by construction.
    pub fn roots(&self) -> &[EntityId] {
        &self.roots
    }

    /// `of` first, then each ancestor, root last. Cycle-guarded: the ledger
    /// cannot express a founding cycle, but a hand-built one can, and an
    /// infinite loop in a census metric is a very expensive way to find out.
    pub fn ancestry(&self, of: EntityId) -> Vec<EntityId> {
        let mut out = vec![of];
        let mut seen = std::collections::BTreeSet::new();
        seen.insert(of);
        let mut cursor = of;
        while let Some(next) = self.parent(cursor) {
            if !seen.insert(next) {
                break;
            }
            out.push(next);
            cursor = next;
        }
        out
    }

    /// Every occupation descending from `of`, ascending. Excludes `of` itself.
    pub fn descendants_of(&self, of: EntityId) -> Vec<EntityId> {
        let mut out: Vec<EntityId> = self
            .parent
            .keys()
            .copied()
            .filter(|k| *k != of && self.ancestry(*k).contains(&of))
            .collect();
        out.sort();
        out
    }
}

/// Read the founding tree out of a ledger.
///
/// `occ-founded-from` is a sum type: `Value::Entity` is a parent link,
/// `Value::Number` is `Founding::Genesis(cell)` — a SITE id, and therefore a
/// root with no ancestor. Reading a Number as a parent is the mistake this
/// function exists to make impossible.
pub fn lineage_of(ledger: &Ledger) -> Lineage {
    let mut out = Lineage::default();
    for fact in ledger.find(hornvale_history::OCC_FOUNDED_FROM) {
        match &fact.object {
            Value::Entity(p) => {
                out.parent.insert(fact.subject, *p);
            }
            _ => out.roots.push(fact.subject),
        }
    }
    out.roots.sort();
    out.roots.dedup();
    out
}
