//! One generic store for a derived value, parameterized per value SHAPE —
//! never a single heterogeneous store (spec §3). A heterogeneous version
//! would need `Box<dyn Any>` keyed by `TypeId` to hold different `V`s side
//! by side, and `TypeId`'s ordering is not build-stable (it is an opaque
//! hash that can differ between compilations of the same source), so a
//! `BTreeMap<TypeId, _>` would put an unstable iteration order under a
//! byte-identity guarantee — exactly the defect class the workspace's
//! `HashMap`/`HashSet` ban (`clippy.toml` `disallowed-types`) already exists
//! to close. `Derived<K, V>` is instead one `BTreeMap<K, V>` per
//! caller-declared shape; every map's iteration order is `K: Ord`'s order,
//! deterministic and build-stable across hosts and compilations.
//!
//! # Two validity classes, split at key-completeness
//!
//! [`Validity`] names the only two ways a derived value can stay correct
//! (spec §2.2): [`Validity::Pure`], a pure function of its key that never
//! invalidates, and [`Validity::Ledger`], a fold over a ledger prefix that
//! invalidates when a later fact touches one of its declared [`DepKey`]
//! dependencies. "World-derived" is not a third class — it is `Pure` with
//! the world's identity (a seed, a level, whatever the derivation actually
//! reads) folded into the key.
//!
//! # What this store proves, and what it cannot
//!
//! The store's real contribution (spec §2.3) is making **key-completeness**
//! a typed obligation rather than a doc comment: a shape declares its key
//! type, and that key must carry every parameter its derivation reads. What
//! the store actually GUARANTEES is only ordinary map correctness — the
//! value returned for a key is whatever was last [`Derived::insert`]ed
//! under that exact key. It does **not**, and cannot, prove the key is
//! COMPLETE — that the derivation reads nothing the key leaves out. An
//! incomplete key is a bug the type system cannot see: it type-checks,
//! inserts, and reads back cleanly, and only ever diverges from a fresh
//! recomputation once an entry survives across two different values of the
//! thing the key omitted. Pressuring exactly that bug is [`Derived::
//! evict_all`]'s job in the property battery ("chaos eviction": evict at
//! every legal opportunity and assert output is unchanged) — a derivation
//! that reads something outside its key recomputes a DIFFERENT answer the
//! moment its cache entry is dropped and refilled, which chaos eviction
//! forces to happen on every step rather than leaving it to chance.

use std::collections::BTreeMap;

/// One store of a derived value, keyed by every parameter its derivation
/// reads. See the module doc for why this is generic per shape rather than
/// one heterogeneous store, and for the key-completeness obligation the
/// store makes typed but cannot itself discharge.
#[derive(Debug, Clone)]
pub struct Derived<K: Ord + Clone, V: Clone> {
    /// The resident entries, keyed by `K`'s own `Ord` — never a `HashMap`
    /// (decision 0005): iteration order must stay deterministic even though
    /// nothing here currently iterates.
    entries: BTreeMap<K, V>,
    /// How many `get` calls, ever, found an already-cached entry. Never
    /// reset — the scaling property's own deterministic witness, never a
    /// wall-clock proxy (matching `RoomMeshMemo`'s counters, the-forebay
    /// Task 1).
    hits: u64,
    /// How many `get` calls, ever, found no entry. Never reset.
    misses: u64,
}

impl<K: Ord + Clone, V: Clone> Derived<K, V> {
    /// An empty store.
    pub fn new() -> Self {
        Self {
            entries: BTreeMap::new(),
            hits: 0,
            misses: 0,
        }
    }

    /// Consult the store for `key`, counting a hit or a miss. `None` on a
    /// miss — the caller recomputes and [`Self::insert`]s the fresh value;
    /// `Some` on a hit, with the previously inserted value.
    pub fn get(&mut self, key: &K) -> Option<&V> {
        if self.entries.contains_key(key) {
            self.hits += 1;
        } else {
            self.misses += 1;
        }
        self.entries.get(key)
    }

    /// Record `value` under `key`, overwriting any prior entry at that key.
    pub fn insert(&mut self, key: K, value: V) {
        self.entries.insert(key, value);
    }

    /// How many entries the store currently holds.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the store currently holds no entries.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// How many [`Self::get`] calls, ever, found an already-cached entry.
    /// Never reset.
    /// type-audit: bare-ok(count: return)
    pub fn hits(&self) -> u64 {
        self.hits
    }

    /// How many [`Self::get`] calls, ever, found no entry. Never reset.
    /// type-audit: bare-ok(count: return)
    pub fn misses(&self) -> u64 {
        self.misses
    }

    /// Drop every entry. The hit/miss counters are untouched — they answer
    /// "how much reuse happened", not "how many entries currently survive".
    /// This is the chaos-eviction battery's own instrument: calling it at
    /// every legal opportunity must leave every subsequent read-through
    /// byte-identical to a resident run (see the module doc).
    pub fn evict_all(&mut self) {
        self.entries.clear();
    }

    /// Drop the entry at `key`, if any.
    pub fn evict(&mut self, key: &K) {
        self.entries.remove(key);
    }
}

impl<K: Ord + Clone, V: Clone> Default for Derived<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// The (subject, predicate, place) triple a [`Validity::Ledger`] entry
/// watches — the smallest ledger query surface that names "the facts this
/// derivation read." Mirrors the corresponding three fields of
/// [`crate::ledger::Fact`]; `place` stays an `Option` for exactly the same
/// reason `Fact::place` is one — not every fact is location-bound.
/// type-audit: bare-ok(identifier-text: predicate)
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DepKey {
    /// The entity the watched facts are about.
    pub subject: crate::ledger::EntityId,
    /// The predicate name, resolved against the concept registry — the same
    /// string a `Fact::predicate` carries.
    pub predicate: String,
    /// The entity the watched facts were observed at, if location-bound.
    /// `None` matches a fact with no place, exactly as `Fact::place` does.
    pub place: Option<crate::ledger::EntityId>,
}

/// The invalidation discipline a [`Derived`] entry obtains (spec §2.2): two
/// classes, split at key-completeness rather than at "world-derived" vs.
/// "ledger-derived" — the line the metaplan originally drew and the one
/// this store's own constructors refuted (spec §2.1).
/// type-audit: bare-ok(count: Ledger.position)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Validity {
    /// A pure function of its key: never invalidated, regardless of ledger
    /// growth or which world produced it — PROVIDED the key is complete.
    /// That proviso is the obligation the store makes typed but cannot
    /// itself discharge (see the module doc); "world-derived" is `Pure`
    /// with the world's identity folded into the key, not a third class.
    Pure,
    /// A fold over a ledger prefix ending at `position` (the ledger's
    /// length when this entry was computed), watching `deps`: stale once a
    /// fact committed after `position` touches any watched dependency.
    Ledger {
        /// The ledger's length when this entry was computed.
        position: u64,
        /// Every [`DepKey`] this entry's derivation read.
        deps: Vec<DepKey>,
    },
}

impl Validity {
    /// Whether this entry is stale, given the ledger's current length
    /// (`current_position`) and the [`DepKey`] of every fact committed
    /// after the entry's own recorded position (`touched_since`).
    /// `Pure` is never stale — that is its entire contract, independent of
    /// both arguments. `Ledger` is stale exactly when the ledger has grown
    /// past its recorded `position` AND at least one fact committed since
    /// then touches a watched dependency; a ledger that has grown but
    /// touched none of `deps` is not stale.
    /// type-audit: bare-ok(count: current_position), bare-ok(flag: return)
    pub fn is_stale(&self, current_position: u64, touched_since: &[DepKey]) -> bool {
        match self {
            Validity::Pure => false,
            Validity::Ledger { position, deps } => {
                current_position > *position && touched_since.iter().any(|t| deps.contains(t))
            }
        }
    }
}
