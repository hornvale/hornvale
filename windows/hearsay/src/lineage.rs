//! The community tree, read from `occ-founded-from`.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::{BTreeMap, BTreeSet};

/// The founding tree: who was founded from whom.
#[derive(Clone, Debug, Default)]
pub struct Lineage {
    parent: BTreeMap<EntityId, EntityId>,
    roots: Vec<EntityId>,
    /// Every node's ancestor set, memoised once at construction. Membership
    /// only — `EntityId` is not its own ancestor — never a replacement for
    /// `ancestry`'s ordered walk, which callers rely on for hop counts.
    ancestors: BTreeMap<EntityId, BTreeSet<EntityId>>,
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

    /// Every occupation this tree knows — roots and descendants alike —
    /// ascending and deduplicated. Includes roots, so callers must not chain
    /// `roots()` onto it.
    pub fn all(&self) -> Vec<EntityId> {
        let mut out: Vec<EntityId> = self
            .parent
            .keys()
            .copied()
            .chain(self.parent.values().copied())
            .chain(self.roots.iter().copied())
            .collect();
        out.sort();
        out.dedup();
        out
    }

    /// Whether `ancestor` is a strict ancestor of `descendant` — O(log n)
    /// against the memo built once in [`lineage_of`]. A node is never its
    /// own ancestor, so `is_ancestor(x, x)` is always `false`.
    /// type-audit: bare-ok(flag: return)
    pub fn is_ancestor(&self, ancestor: EntityId, descendant: EntityId) -> bool {
        self.ancestors
            .get(&descendant)
            .is_some_and(|anc| anc.contains(&ancestor))
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
    // Build the ancestor memo once, here, rather than per lookup: one
    // `ancestry` walk per node (self excluded) beats walking to the root on
    // every `is_ancestor` call. See the plan's node-visit measurement.
    for node in out.all() {
        let mut anc = out.ancestry(node);
        anc.retain(|a| *a != node);
        out.ancestors.insert(node, anc.into_iter().collect());
    }
    out
}
