//! In-memory triple-store indexes over the fact ledger — a derived view, never
//! serialized (spec §3). Three `BTreeMap` permutation indexes (SPO/PSO/OSP) key
//! on the full triple in each rotation's order; their values are ascending
//! fact-position postings. Predicates are interned to a `Symbol` so index keys
//! are compact and compare in O(1). Rebuilding from `&[Fact]` and inserting
//! incrementally produce byte-identical indexes (BTree order is insertion-
//! independent), which is why lazy-rebuild and incremental-maintenance agree.
use crate::ledger::{EntityId, Fact, Value};
use std::cmp::Ordering;
use std::collections::BTreeMap;

/// Interned predicate id. Ephemeral build-state; never serialized.
pub(crate) type Symbol = u32;

/// A fact object wrapped with a deterministic total order for index keys.
/// `Number` uses `total_cmp` (the workspace float-sort rule); the variant rank
/// orders across kinds. Never serialized, so this order is internal only.
#[derive(Clone, Debug, PartialEq)]
pub(crate) struct ObjKey(pub Value);

impl ObjKey {
    /// Lowest possible key, for `(prefix, .., MIN)` range starts.
    pub(crate) const MIN: ObjKey = ObjKey(Value::Entity(EntityId::MIN));
    /// Highest possible key, for `(prefix, .., MAX)` range ends.
    pub(crate) const MAX: ObjKey = ObjKey(Value::Flag(true));
}
impl Eq for ObjKey {}
impl PartialOrd for ObjKey {
    fn partial_cmp(&self, o: &Self) -> Option<Ordering> {
        Some(self.cmp(o))
    }
}
// `Ord` orders finite non-zero numbers by total_cmp, but canonicalizes signed
// zero (see `canon` below) so -0.0 and 0.0 share one index bucket — matching
// the naive path's IEEE `==` and keeping INDEX == SCAN. The derived `PartialEq`
// (IEEE, so -0.0 == 0.0) therefore agrees with `Ord` on zero. BTreeMap uses
// only `Ord::cmp`; non-finite objects are rejected at commit before indexing.
impl Ord for ObjKey {
    fn cmp(&self, other: &Self) -> Ordering {
        fn rank(v: &Value) -> u8 {
            match v {
                Value::Entity(_) => 0,
                Value::Text(_) => 1,
                Value::Number(_) => 2,
                Value::Flag(_) => 3,
            }
        }
        // Canonicalize signed zero: the ledger's naive scan compares objects
        // with IEEE equality (-0.0 == 0.0), so the index MUST bucket the two
        // zero spellings together or the two paths diverge (INDEX != SCAN).
        // total_cmp otherwise orders -0.0 < 0.0. All other finite values order
        // by total_cmp; non-finite objects are rejected at commit and never
        // reach the index.
        fn canon(x: f64) -> f64 {
            if x == 0.0 { 0.0 } else { x }
        }
        match (&self.0, &other.0) {
            (Value::Entity(a), Value::Entity(b)) => a.cmp(b),
            (Value::Text(a), Value::Text(b)) => a.cmp(b),
            (Value::Number(a), Value::Number(b)) => canon(*a).total_cmp(&canon(*b)),
            (Value::Flag(a), Value::Flag(b)) => a.cmp(b),
            (a, b) => rank(a).cmp(&rank(b)),
        }
    }
}

/// String <-> Symbol table. Interns on demand in first-seen order; the numeric
/// value is byte-irrelevant (never serialized), so first-seen vs registry order
/// makes no observable difference.
#[derive(Clone, Debug, Default)]
pub(crate) struct Interner {
    to_symbol: BTreeMap<String, Symbol>,
    labels: Vec<String>,
}
impl Interner {
    pub(crate) fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.to_symbol.get(s) {
            return sym;
        }
        let sym = self.labels.len() as Symbol;
        self.labels.push(s.to_string());
        self.to_symbol.insert(s.to_string(), sym);
        sym
    }
    pub(crate) fn get(&self, s: &str) -> Option<Symbol> {
        self.to_symbol.get(s).copied()
    }
    #[allow(dead_code)]
    pub(crate) fn resolve(&self, sym: Symbol) -> &str {
        &self.labels[sym as usize]
    }
}

type Postings = Vec<usize>;

/// The three permutation indexes over the ledger. Keys are full triples in each
/// rotation's order; values are ascending fact positions.
#[derive(Clone, Debug, Default)]
pub(crate) struct FactIndex {
    interner: Interner,
    spo: BTreeMap<(EntityId, Symbol, ObjKey), Postings>,
    pso: BTreeMap<(Symbol, EntityId, ObjKey), Postings>,
    osp: BTreeMap<(ObjKey, EntityId, Symbol), Postings>,
}

impl FactIndex {
    /// Rebuild from scratch over the whole fact slice (lazy-load path).
    pub(crate) fn rebuild(&mut self, facts: &[Fact]) {
        *self = FactIndex::default();
        for (pos, fact) in facts.iter().enumerate() {
            self.insert(pos, fact);
        }
    }

    /// Insert one fact's position into all three indexes (incremental path).
    /// `pos` is monotonic (append-only), so postings stay ascending.
    pub(crate) fn insert(&mut self, pos: usize, fact: &Fact) {
        let sym = self.interner.intern(&fact.predicate);
        let obj = ObjKey(fact.object.clone());
        self.spo
            .entry((fact.subject, sym, obj.clone()))
            .or_default()
            .push(pos);
        self.pso
            .entry((sym, fact.subject, obj.clone()))
            .or_default()
            .push(pos);
        self.osp
            .entry((obj, fact.subject, sym))
            .or_default()
            .push(pos);
    }

    /// Is an identical full fact already present? (idempotency dedup)
    pub(crate) fn contains_full(&self, fact: &Fact, facts: &[Fact]) -> bool {
        let Some(sym) = self.interner.get(&fact.predicate) else {
            return false;
        };
        let obj = ObjKey(fact.object.clone());
        self.spo
            .get(&(fact.subject, sym, obj))
            .is_some_and(|ps| ps.iter().any(|&p| &facts[p] == fact))
    }

    /// Does the subject already hold a *different* object for this predicate?
    pub(crate) fn has_conflicting_object(&self, fact: &Fact, facts: &[Fact]) -> bool {
        let Some(sym) = self.interner.get(&fact.predicate) else {
            return false;
        };
        self.spo
            .range((fact.subject, sym, ObjKey::MIN)..=(fact.subject, sym, ObjKey::MAX))
            .flat_map(|(_, ps)| ps.iter())
            .any(|&p| facts[p].object != fact.object)
    }

    /// Ascending positions of all facts about `subject`.
    pub(crate) fn positions_for_subject(&self, subject: EntityId) -> Vec<usize> {
        let mut v: Vec<usize> = self
            .spo
            .range((subject, Symbol::MIN, ObjKey::MIN)..=(subject, Symbol::MAX, ObjKey::MAX))
            .flat_map(|(_, ps)| ps.iter().copied())
            .collect();
        v.sort_unstable();
        v
    }

    /// Ascending positions of all facts with `pred`.
    pub(crate) fn positions_for_predicate(&self, pred: &str) -> Vec<usize> {
        let Some(sym) = self.interner.get(pred) else {
            return Vec::new();
        };
        let mut v: Vec<usize> = self
            .pso
            .range((sym, EntityId::MIN, ObjKey::MIN)..=(sym, EntityId::MAX, ObjKey::MAX))
            .flat_map(|(_, ps)| ps.iter().copied())
            .collect();
        v.sort_unstable();
        v
    }

    /// Ascending positions of all facts whose object equals `object`.
    pub(crate) fn positions_for_object(&self, object: &Value) -> Vec<usize> {
        let obj = ObjKey(object.clone());
        let mut v: Vec<usize> = self
            .osp
            .range((obj.clone(), EntityId::MIN, Symbol::MIN)..=(obj, EntityId::MAX, Symbol::MAX))
            .flat_map(|(_, ps)| ps.iter().copied())
            .collect();
        v.sort_unstable();
        v
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interner_round_trips() {
        let mut i = Interner::default();
        let a = i.intern("name");
        let b = i.intern("located-in");
        assert_eq!(i.intern("name"), a); // stable
        assert_ne!(a, b);
        assert_eq!(i.resolve(a), "name");
        assert_eq!(i.get("name"), Some(a));
        assert_eq!(i.get("absent"), None);
    }

    #[test]
    fn objkey_orders_numbers_by_total_cmp_and_ranks_variants() {
        assert!(ObjKey(Value::Number(-1.0)) < ObjKey(Value::Number(1.0)));
        assert!(ObjKey(Value::Entity(EntityId::new(9).unwrap())) < ObjKey(Value::Text("a".into())));
        assert!(ObjKey::MIN <= ObjKey(Value::Number(0.0)));
        assert!(ObjKey(Value::Number(0.0)) <= ObjKey::MAX);
    }
}
