//! The community tree, read from `occ-founded-from`.

use hornvale_kernel::ledger::{EntityId, Ledger, Value};
use std::collections::BTreeMap;

/// The founding tree: who was founded from whom.
///
/// `children` is the inverse of `parent`, derived once in `lineage_of` and
/// never mutated after. It exists so the downward reads below walk the tree
/// instead of re-deriving each node's ancestry, which made them quadratic.
#[derive(Clone, Debug, Default)]
pub struct Lineage {
    parent: BTreeMap<EntityId, EntityId>,
    children: BTreeMap<EntityId, Vec<EntityId>>,
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

    /// The occupations founded directly from `of`, ascending. Empty for a
    /// leaf. These are exactly the `descendants_of` at one hop.
    pub fn children_of(&self, of: EntityId) -> &[EntityId] {
        self.children.get(&of).map_or(&[], Vec::as_slice)
    }

    /// Every occupation descending from `of`, ascending, each paired with the
    /// number of founding steps from it up to `of`. Excludes `of` itself.
    ///
    /// WHY THIS IS THE SAME ANSWER `ancestry` GAVE, CYCLES INCLUDED. The
    /// obvious rewrite of the old quadratic form — a global `depth` map — is
    /// NOT safe: depth is undefined on a cycle, so it would silently diverge
    /// exactly where `ancestry`'s guard stops. A downward walk is safe, and
    /// the argument does not depend on the data being acyclic:
    ///
    /// - Every node has at most one parent, so the upward walk from `d` is a
    ///   single deterministic sequence. `ancestry` breaks on the first
    ///   REPEAT, so it enumerates every upward-reachable node exactly once —
    ///   which makes `ancestry(k).contains(&of)` equivalent to "`of` is
    ///   upward-reachable from `k`", and that is the reverse of "`k` is
    ///   downward-reachable from `of`". A visited-set walk computes that set
    ///   exactly, so the membership half is unconditional.
    /// - For the hop count: that same single-parent property makes the
    ///   downward path from `of` to a descendant UNIQUE, because reversing it
    ///   yields a prefix of `d`'s one deterministic upward walk. The only way
    ///   to reach `d` by a longer route is to go around a cycle through `of`
    ///   itself — which is exactly what seeding `seen` with `of` prevents. So
    ///   the emitted count equals `ancestry(d).position(of)`, the first
    ///   occurrence, which is what the old code read.
    ///
    /// TRAVERSAL ORDER IS THEREFORE NOT LOAD-BEARING, and this comment said
    /// the opposite until it was measured: a depth-first variant was run
    /// against this one over every shape in `tests/lineage.rs` and agreed on
    /// all of them (0 differing pairs), so "BFS reaches it by the shortest
    /// path" was a true sentence about an irrelevant property. The `of`
    /// pre-seed IS load-bearing — dropping it reddens all three equivalence
    /// tests, on the cyclic shapes specifically.
    ///
    /// `tests/lineage.rs` pins both halves against the old code as an oracle,
    /// on cyclic input included.
    /// type-audit: bare-ok(count: return)
    pub fn descendants_with_hops(&self, of: EntityId) -> Vec<(EntityId, u32)> {
        let mut out: Vec<(EntityId, u32)> = Vec::new();
        // `of` starts seen, so a cycle that returns to it neither re-emits it
        // (it is excluded by contract) nor revisits ground already covered.
        let mut seen = std::collections::BTreeSet::from([of]);
        let mut frontier = vec![of];
        let mut hops: u32 = 0;
        while !frontier.is_empty() {
            hops += 1;
            let mut next = Vec::new();
            for node in &frontier {
                for child in self.children_of(*node) {
                    if seen.insert(*child) {
                        out.push((*child, hops));
                        next.push(*child);
                    }
                }
            }
            frontier = next;
        }
        out.sort();
        out
    }

    /// Every occupation descending from `of`, ascending. Excludes `of` itself.
    pub fn descendants_of(&self, of: EntityId) -> Vec<EntityId> {
        self.descendants_with_hops(of)
            .into_iter()
            .map(|(k, _)| k)
            .collect()
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
    // Invert `parent` once. Built from the parent map rather than from the
    // ledger so the two cannot disagree, and ascending because `parent` is a
    // BTreeMap — which is what lets every downward read stay ordered without
    // sorting per call.
    for (child, parent) in &out.parent {
        out.children.entry(*parent).or_default().push(*child);
    }
    out
}
