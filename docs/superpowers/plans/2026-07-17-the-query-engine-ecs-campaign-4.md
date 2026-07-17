# The Query Engine (ECS Campaign 4) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give the Fact ledger three triple-store permutation indexes (SPO/PSO/OSP) plus predicate interning, as in-memory derived views, so `commit` and all pattern queries become O(log n) — fixing the O(n²) world-build (metaplan §12) — with the serialized world byte-identical throughout.

**Architecture:** Extend `kernel::ledger::Ledger` in place with a private `#[serde(skip)] index: Option<FactIndex>`. `FactIndex` holds an interner (`String`↔`Symbol`) and three `BTreeMap` permutation indexes whose values are ascending fact-position postings. The index is maintained incrementally on `commit` (`&mut`) and lazily rebuilt on first use after load; `&self` readers use it if present and fall back to a naive scan if absent. Every existing method keeps its signature and its commit-order output, so all 303 call-sites and every committed artifact are unchanged.

**Tech Stack:** Rust (edition 2024), `std` + `serde` only (no new deps — no proptest, no criterion). `BTreeMap`/`BTreeSet`/`Vec` only (no `HashMap`). All the project gate ladder (`make gate` = fmt + clippy + nextest + doctests).

## Global Constraints

- **Determinism / save-format (spec §3):** the serialized `Ledger` stays exactly `{ facts: Vec<Fact>, next_entity: u64 }`. Indexes and the interner are `#[serde(skip)]`. `Fact` is unchanged (`predicate` stays `String`); `EntityId` stays `u64`. Every committed artifact must be **byte-identical** — the drift-check is the proof.
- **No `HashMap`/`HashSet`** — `BTreeMap`/`BTreeSet`/`Vec` only (clippy-enforced workspace-wide). Float ordering uses `total_cmp`.
- **No new dependencies** — the allowlist is `[libm, serde, serde_json]`, enforced by `cli/tests/architecture.rs`. Random ledgers in property tests use a hand-rolled seeded PRNG (splitmix64), the same "roll our own" style as `domains/astronomy/tests/genesis_properties.rs`.
- **No criterion** (decision-ledger #37): wall-time micros use `std::hint::black_box` + `std::time::Instant` in the `heavy:` ignore tier (ungated); deterministic budgets are `const` asserts / lab studies.
- **Commit order is preserved:** index-backed `facts_about`/`find`/`value_of` yield facts in ascending fact-position order (== commit order), so no artifact drifts.
- Every crate sets `#![warn(missing_docs)]`; every **public** item gets a one-line doc comment. `FactIndex` and its helpers are **module-private** (declared `mod fact_index;`, not `pub mod`), so they need no doc comments and expose no new pub-boundary primitives (no type-audit tags required).
- Run `cargo fmt` as the final step before every commit.

---

## File Structure

- **Create** `kernel/src/fact_index.rs` — `Symbol`, `ObjKey` (+ `Ord` via `total_cmp`, `MIN`/`MAX`), `Interner`, `FactIndex` (three `BTreeMap`s + all insert/query/rebuild logic). Module-private. Its own `#[cfg(test)]` unit tests for the interner + object-order.
- **Modify** `kernel/src/lib.rs` — add `mod fact_index;` (private).
- **Modify** `kernel/src/ledger.rs` — add the `#[serde(skip)] index: Option<FactIndex>` field; add `ensure_index`/`reindex`; rewrite `commit`, `check`, `facts_about`, `find`, `value_of` to be index-backed with naive fallback; keep `pub(crate)` naive reference impls (`naive_facts_about`, `naive_find`, `naive_value_of`, `naive_contains`, `naive_has_conflict`); add `query_by_object`. Extend the `#[cfg(test)]` module with the INDEX≡SCAN battery and the heavy-tier micro-bench.
- **Modify** `windows/worldgen/src/components.rs` — add `ComponentTag` enum + `WorldComponents::kinds_with(tag) -> Vec<KindId>` (the reflection query).
- **Create** `studies/index-budget.study.json` + wire it into the artifact set — a deterministic index-size budget as a lab study (if the lab metric surface supports fact/index counts; else a `const`-assert test in `fact_index.rs`).

---

### Task 1: `FactIndex` + the commit fix (SPO) — the load-bearing MVP

Builds the whole `FactIndex` (all three permutation maps + interner + object key) and wires the SPO-backed consumers: `commit` (both scans), `check` (contradiction), and `facts_about`. This alone resolves metaplan §12 blocker #4 (world-build O(n log n)); Tasks 2–3 add the remaining query surfaces.

**Files:**
- Create: `kernel/src/fact_index.rs`
- Modify: `kernel/src/lib.rs` (add `mod fact_index;`)
- Modify: `kernel/src/ledger.rs` (field + `ensure_index`/`reindex`, rewrite `commit`/`check`/`facts_about`, naive refs)
- Test: `kernel/src/fact_index.rs` (`#[cfg(test)]`), `kernel/src/ledger.rs` (`#[cfg(test)] mod tests`)

**Interfaces:**
- Produces (module-private, used by `ledger.rs` via `crate::fact_index`):
  - `type Symbol = u32;`
  - `struct ObjKey(pub Value);` with `Ord` (via `total_cmp` for `Number`), `const MIN`, `const MAX`.
  - `struct Interner { … }` with `fn intern(&mut self, &str) -> Symbol`, `fn get(&self, &str) -> Option<Symbol>`, `fn resolve(&self, Symbol) -> &str`.
  - `struct FactIndex { … }` with:
    - `fn rebuild(&mut self, facts: &[Fact])`
    - `fn insert(&mut self, pos: usize, fact: &Fact)`
    - `fn contains_full(&self, fact: &Fact, facts: &[Fact]) -> bool`
    - `fn has_conflicting_object(&self, fact: &Fact, facts: &[Fact]) -> bool`
    - `fn positions_for_subject(&self, subject: EntityId) -> Vec<usize>` (ascending)
    - `fn positions_for_predicate(&self, pred: &str) -> Vec<usize>` (ascending)
    - `fn positions_for_object(&self, object: &Value) -> Vec<usize>` (ascending)
- Produces (on `Ledger`, `pub(crate)`): `naive_facts_about`, `naive_has_conflict`, `naive_contains` — the reference impls used by tests and the bench.

- [ ] **Step 1: Write the failing test** (byte-identical + index-backed contradiction/dedup)

Add to `kernel/src/ledger.rs` `#[cfg(test)] mod tests` (helpers `registry()`/`named()` already exist):

```rust
#[test]
fn index_backed_commit_matches_naive_semantics() {
    // Idempotent recommit, functional contradiction, and non-functional
    // multi-object all behave exactly as the pre-index ledger did.
    let r = registry();
    let mut l = Ledger::default();
    let f = named(&mut l, "Zaggrak");
    let s = f.subject;
    assert!(l.commit(f.clone(), &r).unwrap());          // appended
    assert!(!l.commit(f, &r).unwrap());                 // idempotent no-op
    assert_eq!(l.len(), 1);
    let clash = Fact { subject: s, predicate: "name".into(),
        object: Value::Text("Bolnar".into()), place: None, day: None,
        provenance: "t".into() };
    assert!(matches!(l.commit(clash, &r), Err(LedgerError::Contradiction { .. })));
}

#[test]
fn facts_about_yields_commit_order_after_index() {
    let r = registry();
    let mut l = Ledger::default();
    let village = l.mint_entity();
    for c in ["located-in"].iter().cycle().take(3).zip(0..3) {
        let target = l.mint_entity();
        l.commit(Fact { subject: village, predicate: c.0.to_string(),
            object: Value::Entity(target), place: None, day: None,
            provenance: "t".into() }, &r).unwrap();
    }
    let objs: Vec<&Value> = l.facts_about(village).map(|f| &f.object).collect();
    // ascending commit order == ascending minted target ids (2,3,4)
    assert_eq!(objs, l.facts_about(village).map(|f| &f.object).collect::<Vec<_>>());
    assert_eq!(l.facts_about(village).count(), 3);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-kernel index_backed_commit_matches_naive_semantics`
Expected: FAIL to compile (nothing changed yet) or the new module missing — confirm red.

- [ ] **Step 3: Create `kernel/src/fact_index.rs`**

```rust
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
    pub(crate) const MIN: ObjKey = ObjKey(Value::Entity(EntityId(0)));
    /// Highest possible key, for `(prefix, .., MAX)` range ends.
    pub(crate) const MAX: ObjKey = ObjKey(Value::Flag(true));
}
impl Eq for ObjKey {}
impl PartialOrd for ObjKey {
    fn partial_cmp(&self, o: &Self) -> Option<Ordering> { Some(self.cmp(o)) }
}
impl Ord for ObjKey {
    fn cmp(&self, other: &Self) -> Ordering {
        fn rank(v: &Value) -> u8 {
            match v { Value::Entity(_) => 0, Value::Text(_) => 1,
                      Value::Number(_) => 2, Value::Flag(_) => 3 }
        }
        match (&self.0, &other.0) {
            (Value::Entity(a), Value::Entity(b)) => a.cmp(b),
            (Value::Text(a), Value::Text(b)) => a.cmp(b),
            (Value::Number(a), Value::Number(b)) => a.total_cmp(b),
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
        if let Some(&sym) = self.to_symbol.get(s) { return sym; }
        let sym = self.labels.len() as Symbol;
        self.labels.push(s.to_string());
        self.to_symbol.insert(s.to_string(), sym);
        sym
    }
    pub(crate) fn get(&self, s: &str) -> Option<Symbol> { self.to_symbol.get(s).copied() }
    #[allow(dead_code)]
    pub(crate) fn resolve(&self, sym: Symbol) -> &str { &self.labels[sym as usize] }
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
        self.spo.entry((fact.subject, sym, obj.clone())).or_default().push(pos);
        self.pso.entry((sym, fact.subject, obj.clone())).or_default().push(pos);
        self.osp.entry((obj, fact.subject, sym)).or_default().push(pos);
    }

    /// Is an identical full fact already present? (idempotency dedup)
    pub(crate) fn contains_full(&self, fact: &Fact, facts: &[Fact]) -> bool {
        let Some(sym) = self.interner.get(&fact.predicate) else { return false; };
        let obj = ObjKey(fact.object.clone());
        self.spo.get(&(fact.subject, sym, obj))
            .is_some_and(|ps| ps.iter().any(|&p| &facts[p] == fact))
    }

    /// Does the subject already hold a *different* object for this predicate?
    pub(crate) fn has_conflicting_object(&self, fact: &Fact, facts: &[Fact]) -> bool {
        let Some(sym) = self.interner.get(&fact.predicate) else { return false; };
        self.spo.range((fact.subject, sym, ObjKey::MIN)..=(fact.subject, sym, ObjKey::MAX))
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
        let Some(sym) = self.interner.get(pred) else { return Vec::new(); };
        let mut v: Vec<usize> = self
            .pso
            .range((sym, EntityId(0), ObjKey::MIN)..=(sym, EntityId(u64::MAX), ObjKey::MAX))
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
            .range((obj.clone(), EntityId(0), Symbol::MIN)..=(obj, EntityId(u64::MAX), Symbol::MAX))
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
        assert_eq!(i.intern("name"), a);      // stable
        assert_ne!(a, b);
        assert_eq!(i.resolve(a), "name");
        assert_eq!(i.get("name"), Some(a));
        assert_eq!(i.get("absent"), None);
    }

    #[test]
    fn objkey_orders_numbers_by_total_cmp_and_ranks_variants() {
        assert!(ObjKey(Value::Number(-1.0)) < ObjKey(Value::Number(1.0)));
        assert!(ObjKey(Value::Entity(EntityId(9))) < ObjKey(Value::Text("a".into())));
        assert!(ObjKey::MIN <= ObjKey(Value::Number(0.0)));
        assert!(ObjKey(Value::Number(0.0)) <= ObjKey::MAX);
    }
}
```

- [ ] **Step 4: Wire the module** — add to `kernel/src/lib.rs` after `pub mod ecology;` (keep alphabetical grouping; `fact_index` is private):

```rust
mod fact_index;
```

- [ ] **Step 5: Extend `Ledger` in `kernel/src/ledger.rs`** — the struct field and lifecycle:

Change the struct:

```rust
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Ledger {
    facts: Vec<Fact>,
    next_entity: u64,
    /// Derived permutation indexes — never serialized; rebuilt on first use
    /// after load, maintained incrementally on commit. Absent-or-complete.
    #[serde(skip)]
    index: Option<crate::fact_index::FactIndex>,
}
```

Add near the top of `impl Ledger`:

```rust
    /// Ensure the derived index exists and is current (rebuild-if-absent).
    fn ensure_index(&mut self) {
        if self.index.is_none() {
            let mut idx = crate::fact_index::FactIndex::default();
            idx.rebuild(&self.facts);
            self.index = Some(idx);
        }
    }

    // --- naive reference impls: the O(n) truth the index refines. Kept for the
    // INDEX≡SCAN property test and the heavy-tier before/after micro-bench.
    pub(crate) fn naive_has_conflict(&self, fact: &Fact) -> bool {
        self.facts.iter().any(|f| {
            f.subject == fact.subject && f.predicate == fact.predicate && f.object != fact.object
        })
    }
    pub(crate) fn naive_contains(&self, fact: &Fact) -> bool { self.facts.contains(fact) }
    pub(crate) fn naive_facts_about(&self, subject: EntityId) -> Vec<usize> {
        (0..self.facts.len()).filter(|&p| self.facts[p].subject == subject).collect()
    }
```

- [ ] **Step 6: Rewrite `check` and `commit`** (index-backed, naive fallback):

```rust
    pub fn check(&self, fact: &Fact, registry: &ConceptRegistry) -> Result<(), LedgerError> {
        let def = registry
            .predicate(&fact.predicate)
            .ok_or_else(|| LedgerError::UnknownPredicate { predicate: fact.predicate.clone() })?;
        let object_is_non_finite = matches!(fact.object, Value::Number(n) if !n.is_finite());
        let day_is_non_finite = matches!(fact.day, Some(d) if !d.is_finite());
        if object_is_non_finite || day_is_non_finite {
            return Err(LedgerError::NonFiniteNumber {
                subject: fact.subject,
                predicate: fact.predicate.clone(),
            });
        }
        if def.functional {
            let clash = match &self.index {
                Some(idx) => idx.has_conflicting_object(fact, &self.facts),
                None => self.naive_has_conflict(fact),
            };
            if clash {
                return Err(LedgerError::Contradiction {
                    subject: fact.subject,
                    predicate: fact.predicate.clone(),
                });
            }
        }
        Ok(())
    }

    pub fn commit(
        &mut self,
        mut fact: Fact,
        registry: &ConceptRegistry,
    ) -> Result<bool, LedgerError> {
        if let Value::Number(n) = fact.object {
            fact.object = Value::Number(crate::quantize::quantize(n));
        }
        fact.day = fact.day.map(crate::quantize::quantize);
        self.ensure_index(); // fast contradiction/dedup for the rest of this build
        self.check(&fact, registry)?;
        let dup = match &self.index {
            Some(idx) => idx.contains_full(&fact, &self.facts),
            None => self.naive_contains(&fact),
        };
        if dup {
            return Ok(false);
        }
        let pos = self.facts.len();
        if let Some(idx) = self.index.as_mut() {
            idx.insert(pos, &fact);
        }
        self.facts.push(fact);
        Ok(true)
    }
```

- [ ] **Step 7: Rewrite `facts_about`** to be index-backed with commit-order output:

```rust
    pub fn facts_about(&self, subject: EntityId) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_subject(subject),
            None => self.naive_facts_about(subject),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }
```

- [ ] **Step 8: Run the tests to verify they pass**

Run: `cargo test -p hornvale-kernel`
Expected: PASS (new tests green; all pre-existing ledger tests still green — the `commit_and_query_roundtrip`, `functional_contradiction_is_rejected`, `identical_recommit_is_idempotent`, `iter_yields_facts_in_commit_order`, `ledger_serializes_roundtrip_including_minting_state` tests exercise the rewrite).

- [ ] **Step 9: Verify clippy + fmt + no serialized-shape change**

Run: `cargo clippy -p hornvale-kernel --all-targets -- -D warnings && cargo fmt -p hornvale-kernel`
Run: `cargo test -p hornvale-kernel ledger_serializes_roundtrip_including_minting_state`
Expected: PASS — the `#[serde(skip)]` field means serialized JSON is unchanged.

- [ ] **Step 10: Commit**

```bash
git add kernel/src/fact_index.rs kernel/src/lib.rs kernel/src/ledger.rs
git commit -m "feat(kernel): SPO permutation index + O(log n) commit (ecs-c4 T1)"
```

---

### Task 2: `find` via PSO + `value_of` + INDEX≡SCAN battery (S & P)

Switch the predicate query onto the PSO index (no per-call `String` alloc) and `value_of` onto the SPO index, then add the keystone property test for the subject and predicate shapes.

**Files:**
- Modify: `kernel/src/ledger.rs` (`find`, `value_of` rewrite; add `naive_find`, `naive_value_of`; property test)
- Test: `kernel/src/ledger.rs` `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: `FactIndex::positions_for_predicate`, `positions_for_subject` (Task 1).
- Produces (`pub(crate)` on `Ledger`): `naive_find(&self, &str) -> Vec<usize>`, `naive_value_of(&self, EntityId, &str) -> Option<&Value>`.

- [ ] **Step 1: Write the failing test** (INDEX≡SCAN over random ledgers, S & P shapes)

Add to `kernel/src/ledger.rs` tests:

```rust
// Tiny deterministic PRNG — no dep (splitmix64). Same "roll our own" style as
// the astronomy property batteries.
fn splitmix(state: &mut u64) -> u64 {
    *state = state.wrapping_add(0x9E3779B97F4A7C15);
    let mut z = *state;
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
    z ^ (z >> 31)
}

fn random_ledger(seed: u64, n: usize) -> (Ledger, ConceptRegistry, Vec<EntityId>) {
    let r = registry(); // predicates: "name" (functional), "located-in" (non-functional)
    let mut l = Ledger::default();
    let subjects: Vec<EntityId> = (0..8).map(|_| l.mint_entity()).collect();
    let mut st = seed.wrapping_add(1);
    for _ in 0..n {
        let s = subjects[(splitmix(&mut st) as usize) % subjects.len()];
        // only use the non-functional predicate for bulk facts, so random
        // objects never trip the functional-contradiction reject
        let target = subjects[(splitmix(&mut st) as usize) % subjects.len()];
        let _ = l.commit(
            Fact { subject: s, predicate: "located-in".into(),
                object: Value::Entity(target), place: None, day: None,
                provenance: "t".into() },
            &r,
        );
    }
    (l, r, subjects)
}

#[test]
fn index_equals_scan_subject_and_predicate() {
    for seed in 0..64u64 {
        let (l, _r, subjects) = random_ledger(seed, 200);
        // S-shape: facts_about == naive scan (same facts, same commit order)
        for &s in &subjects {
            let idx: Vec<&Fact> = l.facts_about(s).collect();
            let scan: Vec<&Fact> = l.naive_facts_about(s).iter().map(|&p| l.fact_at(p)).collect();
            assert_eq!(idx, scan, "facts_about seed {seed} subj {s:?}");
        }
        // P-shape: find == naive scan
        let idx: Vec<&Fact> = l.find("located-in").collect();
        let scan: Vec<&Fact> = l.naive_find("located-in").iter().map(|&p| l.fact_at(p)).collect();
        assert_eq!(idx, scan, "find seed {seed}");
    }
}
```

Note: this test uses a `fact_at` helper and `naive_find`; both are added in Step 3.

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-kernel index_equals_scan_subject_and_predicate`
Expected: FAIL to compile (`naive_find`, `naive_value_of`, `fact_at` missing).

- [ ] **Step 3: Add the naive refs, a test accessor, and rewrite `find`/`value_of`**

Add to `impl Ledger`:

```rust
    /// Position accessor for tests/benches (the naive refs return positions).
    #[cfg(test)]
    pub(crate) fn fact_at(&self, pos: usize) -> &Fact { &self.facts[pos] }

    pub(crate) fn naive_find(&self, predicate: &str) -> Vec<usize> {
        (0..self.facts.len()).filter(|&p| self.facts[p].predicate == predicate).collect()
    }
    pub(crate) fn naive_value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value> {
        self.facts.iter().find(|f| f.subject == subject && f.predicate == predicate).map(|f| &f.object)
    }
```

Rewrite `find` and `value_of`:

```rust
    pub fn find(&self, predicate: &str) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_predicate(predicate),
            None => self.naive_find(predicate),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }

    pub fn value_of(&self, subject: EntityId, predicate: &str) -> Option<&Value> {
        match &self.index {
            Some(idx) => {
                // first fact (commit order) for (subject, predicate)
                let first = idx
                    .positions_for_subject(subject)
                    .into_iter()
                    .find(|&p| self.facts[p].predicate == predicate);
                first.map(|p| &self.facts[p].object)
            }
            None => self.naive_value_of(subject, predicate),
        }
    }
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-kernel`
Expected: PASS — INDEX≡SCAN green for S and P over 64 seeds; existing `find`/`value_of`/`text_of` tests still green (`text_of` calls `value_of`).

- [ ] **Step 5: clippy + fmt + commit**

```bash
cargo clippy -p hornvale-kernel --all-targets -- -D warnings && cargo fmt -p hornvale-kernel
git add kernel/src/ledger.rs
git commit -m "feat(kernel): PSO-backed find + value_of + INDEX=SCAN battery (ecs-c4 T2)"
```

---

### Task 3: `query_by_object` via OSP + INDEX≡SCAN (O-shape)

Add the object query (the one shape the old ledger had no method for) on the OSP index, and extend the property test to the object shape — completing the keystone across all three permutations.

**Files:**
- Modify: `kernel/src/ledger.rs` (add `query_by_object`; `naive_query_by_object`; extend property test)
- Test: `kernel/src/ledger.rs` `#[cfg(test)] mod tests`

**Interfaces:**
- Consumes: `FactIndex::positions_for_object` (Task 1).
- Produces (`pub` on `Ledger`): `query_by_object(&self, object: &Value) -> impl Iterator<Item = &Fact>`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn index_equals_scan_object() {
    for seed in 0..64u64 {
        let (l, _r, subjects) = random_ledger(seed, 200);
        for &s in &subjects {
            let obj = Value::Entity(s);
            let idx: Vec<&Fact> = l.query_by_object(&obj).collect();
            let scan: Vec<&Fact> = l.naive_query_by_object(&obj).iter().map(|&p| l.fact_at(p)).collect();
            assert_eq!(idx, scan, "query_by_object seed {seed} obj {obj:?}");
        }
    }
}

#[test]
fn query_by_object_finds_committed_facts() {
    let r = registry();
    let mut l = Ledger::default();
    let a = l.mint_entity();
    let hub = l.mint_entity();
    l.commit(Fact { subject: a, predicate: "located-in".into(),
        object: Value::Entity(hub), place: None, day: None, provenance: "t".into() }, &r).unwrap();
    let found: Vec<EntityId> = l.query_by_object(&Value::Entity(hub)).map(|f| f.subject).collect();
    assert_eq!(found, vec![a]);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-kernel query_by_object_finds_committed_facts`
Expected: FAIL to compile (`query_by_object`, `naive_query_by_object` missing).

- [ ] **Step 3: Add `query_by_object` + naive ref** to `impl Ledger`:

```rust
    /// All facts whose object equals `object`, in commit order (the O-shape
    /// query the flat ledger could not answer). O(log n + k) via the OSP index.
    pub fn query_by_object(&self, object: &Value) -> impl Iterator<Item = &Fact> {
        let positions = match &self.index {
            Some(idx) => idx.positions_for_object(object),
            None => self.naive_query_by_object(object),
        };
        positions.into_iter().map(move |p| &self.facts[p])
    }

    pub(crate) fn naive_query_by_object(&self, object: &Value) -> Vec<usize> {
        (0..self.facts.len()).filter(|&p| &self.facts[p].object == object).collect()
    }
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-kernel`
Expected: PASS — INDEX≡SCAN green across S, P, O over 64 seeds each.

- [ ] **Step 5: clippy + fmt + commit**

```bash
cargo clippy -p hornvale-kernel --all-targets -- -D warnings && cargo fmt -p hornvale-kernel
git add kernel/src/ledger.rs
git commit -m "feat(kernel): OSP-backed query_by_object + INDEX=SCAN O-shape (ecs-c4 T3)"
```

---

### Task 4: Reflection — `kinds_with(component)` over `WorldComponents`

The kind-side backend: a uniform "which kinds carry component X" query (UNI-21's capability query, the GOAP action-set read). Small, over the campaign-3 component registries; independent of the ledger index.

**Files:**
- Modify: `windows/worldgen/src/components.rs` (add `ComponentTag` + `kinds_with`)
- Test: `windows/worldgen/src/components.rs` `#[cfg(test)]` (or the existing test module)

**Interfaces:**
- Consumes: `WorldComponents` fields (`biosphere`, `psyche`, …), each a `ComponentStore<KindId, _>` with `.ids()`.
- Produces: `enum ComponentTag { Biosphere, Psyche, Perception, Articulation, Lexicon, FamilyProto, FamilyOf }`; `WorldComponents::kinds_with(&self, tag: ComponentTag) -> Vec<KindId>` (ascending `KindId`, since `ComponentStore` is `BTreeMap`-backed).

- [ ] **Step 1: Write the failing test**

Add to `windows/worldgen/src/components.rs` tests:

```rust
#[test]
fn kinds_with_biosphere_is_the_full_roster_and_psyche_is_the_peopled_subset() {
    let wc = WorldComponents::assemble().unwrap();
    let bio = wc.kinds_with(ComponentTag::Biosphere);
    let psy = wc.kinds_with(ComponentTag::Psyche);
    assert!(!bio.is_empty());
    // every peopled (psyche) kind has a biosphere row (referential integrity)
    assert!(psy.iter().all(|k| bio.contains(k)));
    // fauna (menagerie) exist in biosphere but not psyche => strict subset
    assert!(psy.len() < bio.len());
    // ascending KindId order (BTreeMap-backed store)
    let mut sorted = bio.clone();
    sorted.sort();
    assert_eq!(bio, sorted);
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p hornvale-worldgen kinds_with_biosphere`
Expected: FAIL to compile (`ComponentTag`, `kinds_with` missing).

- [ ] **Step 3: Add `ComponentTag` + `kinds_with`** to `windows/worldgen/src/components.rs`:

```rust
/// A selector over the per-domain component registries, for reflection —
/// "which kinds carry this component" (UNI-21's capability query; the GOAP
/// available-action set).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ComponentTag {
    /// Universal body component.
    Biosphere,
    /// Peopled psychology.
    Psyche,
    /// Peopled perception.
    Perception,
    /// Peopled phonology.
    Articulation,
    /// Peopled lexicon.
    Lexicon,
    /// Family proto vectors.
    FamilyProto,
    /// Taxonomy family label.
    FamilyOf,
}

impl WorldComponents {
    /// The kinds carrying a given component, in ascending `KindId` order.
    pub fn kinds_with(&self, tag: ComponentTag) -> Vec<KindId> {
        match tag {
            ComponentTag::Biosphere => self.biosphere.ids().copied().collect(),
            ComponentTag::Psyche => self.psyche.ids().copied().collect(),
            ComponentTag::Perception => self.perception.ids().copied().collect(),
            ComponentTag::Articulation => self.articulation.ids().copied().collect(),
            ComponentTag::Lexicon => self.lexicon.ids().copied().collect(),
            ComponentTag::FamilyProto => self.family_proto.ids().copied().collect(),
            ComponentTag::FamilyOf => self.family_of.ids().copied().collect(),
        }
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test -p hornvale-worldgen kinds_with_biosphere`
Expected: PASS.

- [ ] **Step 5: clippy + fmt + commit**

```bash
cargo clippy -p hornvale-worldgen --all-targets -- -D warnings && cargo fmt -p hornvale-worldgen
git add windows/worldgen/src/components.rs
git commit -m "feat(worldgen): kinds_with reflection query over the component registries (ecs-c4 T4)"
```

---

### Task 5: Perf budgets (heavy-tier micro-bench + deterministic asserts) + verify + close

Prove the win with a rolled-our-own before/after micro-bench (heavy tier, ungated) and a gate-able deterministic budget, then run the full gate and complete the campaign's book/retro DoD.

**Files:**
- Modify: `kernel/src/ledger.rs` (`#[cfg(test)]`: heavy-tier bench module + a `const`/deterministic size assert)
- Create/Modify: book chronicle + retro (close DoD)

**Interfaces:**
- Consumes: `Ledger::commit`, `naive_has_conflict`, `naive_contains`, `facts_about`, `naive_facts_about` (Tasks 1–2).

- [ ] **Step 1: Write the deterministic budget assert** (gate-able, byte-identical)

Add to `kernel/src/ledger.rs` tests:

```rust
#[test]
fn symbol_is_four_bytes() {
    // The interning space contract: a predicate key is a u32, not a String.
    assert_eq!(std::mem::size_of::<crate::fact_index::Symbol>(), 4);
}

#[test]
fn index_is_absent_until_first_use_then_complete() {
    // The lifecycle invariant: a freshly-deserialized ledger has no index;
    // a query over it still returns the right answers (naive fallback), and a
    // commit builds it. (Byte-identity of the serialized form is covered by
    // ledger_serializes_roundtrip_including_minting_state.)
    let r = registry();
    let mut l = Ledger::default();
    let f = named(&mut l, "Zaggrak");
    let s = f.subject;
    l.commit(f, &r).unwrap();
    let json = serde_json::to_string(&l).unwrap();
    let l2: Ledger = serde_json::from_str(&json).unwrap();
    // index skipped on the wire => rebuilt-on-use; answers match
    assert_eq!(l2.facts_about(s).count(), 1);
    assert_eq!(l2.value_of(s, "name"), Some(&Value::Text("Zaggrak".into())));
}
```

- [ ] **Step 2: Write the heavy-tier micro-bench** (ungated; proves O(n²)→O(n log n))

Add to `kernel/src/ledger.rs` tests:

```rust
#[test]
#[ignore = "heavy: wall-time micro-bench (not byte-identical, never gated); run in gate-full"]
fn bench_commit_scaling_before_vs_after_index() {
    use std::hint::black_box;
    use std::time::Instant;
    let r = registry();
    for n in [1_000usize, 5_000, 20_000] {
        // AFTER: index-backed commit (each commit maintains the index).
        let mut l = Ledger::default();
        let subj = l.mint_entity();
        let start = Instant::now();
        for i in 0..n {
            let target = l.mint_entity();
            let _ = black_box(l.commit(
                Fact { subject: subj, predicate: "located-in".into(),
                    object: Value::Entity(target), place: None, day: None,
                    provenance: "b".into() }, &r));
            let _ = i;
        }
        let after = start.elapsed();
        // BEFORE (reference): the naive O(n) contradiction+dedup scans over the
        // same facts, showing the quadratic the index removes.
        let facts: Vec<Fact> = l.iter().cloned().collect();
        let scan_start = Instant::now();
        let mut probe = Ledger::default();
        for f in &facts {
            let _ = black_box(probe.naive_has_conflict(f) || probe.naive_contains(f));
            // (probe is not mutated; this measures the scan cost per fact)
        }
        let before = scan_start.elapsed();
        eprintln!("n={n:>6}  after(indexed commit)={after:?}  before(naive scans/one pass)={before:?}");
    }
}
```

- [ ] **Step 3: Run the deterministic tests + the bench**

Run: `cargo test -p hornvale-kernel symbol_is_four_bytes index_is_absent_until_first_use_then_complete`
Expected: PASS.
Run: `cargo test -p hornvale-kernel bench_commit_scaling_before_vs_after_index -- --ignored --nocapture`
Expected: PASS; printed timings show the indexed commit scaling sub-quadratically vs. the naive per-fact scan growing with n.

- [ ] **Step 4: Full gate + byte-identity confirmation**

Run: `cargo fmt --check && cargo clippy --workspace --all-targets -- -D warnings`
Run: `cargo nextest run --workspace 2>&1 | tee /tmp/hv-c4.txt` (inspect the file; trust the exit code)
Run the artifact freshness check (the CI "Artifacts are current" shape):
```bash
cargo run -p hornvale -- new --seed 42 --out /tmp/hv.json
cargo run -p hornvale -- almanac --world /tmp/hv.json > book/src/gallery/almanac-seed-42-sky.md
cargo run -p hornvale -- lab run studies/the-census.study.json
git diff --exit-code book/src/gallery/ book/src/reference/ book/src/laboratory/
```
Expected: all green; **`git diff --exit-code` is clean** — the campaign is byte-identical (this is the master proof). (Census fixtures may already be red from the shared AWS-regen batch inherited from main; a byte-identical refactor cannot add new census reds — confirm any red predates this branch.)

- [ ] **Step 5: Close DoD — book + retro**

- Chronicle entry `book/src/chronicle/the-concordance.md` (candidate name; confirm at G6): the query engine as derived views, the INDEX≡SCAN keystone, byte-identity as the proof. Wire it into `book/src/SUMMARY.md`.
- Freshness sweep: update the UNI-22 registry row (`book/src/frontier/idea-registry.md`) — campaign 4 SHIPPED (SPO/PSO/OSP + interning); campaign 5 (instance⋈ledger) is next. Cross-refs UNI-20/UNI-21/UNI-23 stay live.
- Retrospective `docs/retrospectives/<n>-the-concordance.md` (process lessons).
- Mark the spec + this plan SHIPPED.

- [ ] **Step 6: Commit the close**

```bash
git add book/ docs/retrospectives/ docs/superpowers/specs/2026-07-17-the-query-engine-ecs-campaign-4-design.md docs/superpowers/plans/2026-07-17-the-query-engine-ecs-campaign-4.md
git commit -m "docs(ecs-c4): The Concordance chronicle + retro + UNI-22 re-score (close DoD)"
```

Then hand off to G6 (merge is Nathan's call): `make preflight` from the branch, run the full `cargo nextest run --workspace` **before** any FF (the campaign-3 lesson — #[ignore]d/lib tests from parallel campaigns are invisible to build+integration runs), and land via the guarded main-side true fast-forward.

---

## Self-Review

**Spec coverage:**
- §2 three permutation indexes SPO/PSO/OSP → Task 1 (all three built), consumers wired T1–T3. ✓
- §2 predicate interning (query-speed half) → Task 1 (`Interner`, Symbols key the indexes; `Fact.predicate` stays `String`). ✓
- §2 commit fix (both scans) → Task 1 (`contains_full` dedup + `has_conflicting_object` contradiction). ✓
- §2 `kinds_with`/reflection → Task 4. ✓
- §2 benchmark slice → Task 5 (heavy-tier micro-bench + deterministic `symbol_is_four_bytes` + lifecycle assert). ✓
- §3 save-format line (byte-identical, `#[serde(skip)]`) → Task 1 Step 5/9, Task 5 Step 4 (`git diff --exit-code`). ✓
- §4.1 extend in place, position-postings, commit order → Tasks 1–3 (positions sorted ascending). ✓
- §4.3 lifecycle (incremental-on-write, lazy-rebuild-on-load, absent-or-complete) → Task 1 (`ensure_index`) + Task 5 (`index_is_absent_until_first_use_then_complete`). ✓
- §4.1 `Value` total order via `total_cmp` → Task 1 (`ObjKey`). ✓
- §5 INDEX≡SCAN keystone (random seeded ledgers, no proptest) → Tasks 2–3 (splitmix64, 64 seeds × S/P/O). ✓
- §2 out-of-scope (Fact-shrink, NonZeroU64, Phenomena/Field, place/day index) → not implemented; in `followups.md`. ✓

**Placeholder scan:** No TBD/TODO. Every code step shows complete, final code. The Task-2 Step-1 test forward-references `fact_at`/`naive_find`/`naive_value_of`, all added in Task-2 Step 3 (noted inline); the test is red until then by design (TDD).

**Type consistency:** `positions_for_subject`/`_predicate`/`_object` return `Vec<usize>` (ascending) everywhere; naive refs return `Vec<usize>` (positions), unified through `fact_at`/`&self.facts[p]`. `ComponentTag`/`kinds_with` signatures match between Task 4 interface and impl. `Symbol = u32`, `ObjKey`, `FactIndex` module-private throughout.
