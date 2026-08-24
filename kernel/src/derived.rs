//! One generic store for a derived value, parameterized per value SHAPE —
//! never a single heterogeneous store (spec §3). A heterogeneous version
//! would need `Box<dyn Any>` keyed by `TypeId` to hold different `V`s side
//! by side, and `TypeId`'s ordering is not build-stable (it is an opaque
//! hash that can differ between compilations of the same source), so a
//! `BTreeMap<TypeId, _>` would put an unstable iteration order under a
//! byte-identity guarantee — exactly the defect class the workspace's
//! `HashMap`/`HashSet` ban (`clippy.toml` `disallowed-types`) already exists
//! to close. `Derived<K, V>` is instead one `BTreeMap<K, _>` per
//! caller-declared shape; every map's iteration order is `K: Ord`'s order,
//! deterministic and build-stable across hosts and compilations.
//!
//! # Two validity classes, split at key-completeness, carried per entry
//!
//! [`Validity`] names the only two ways a derived value can stay correct
//! (spec §2.2): [`Validity::Pure`], a pure function of its key that never
//! invalidates, and [`Validity::Ledger`], a fold over a ledger prefix that
//! invalidates when a later fact touches one of its declared [`DepKey`]
//! dependencies. "World-derived" is not a third class — it is `Pure` with
//! the world's identity (a seed, a level, whatever the derivation actually
//! reads) folded into the key.
//!
//! Every entry carries its own [`Validity`] (metaplan §6.6: "a store whose
//! entries each carry their own dependency key … serves both, and serves
//! the ones nobody has thought of yet"). Two accessors judge it, because the
//! two classes need different information and the campaign's one live
//! tenant (Task 3's `RoomMeshMemo`, migrated next) is entirely `Pure`:
//!
//! - [`Derived::get`] is the `Pure`-only fast path: one `BTreeMap` lookup,
//!   no ledger cursor to carry around, no staleness arithmetic. It is the
//!   cheap, obvious call for a store with no `Ledger` tenant. Calling it
//!   against a key holding a `Validity::Ledger` entry is a caller bug —
//!   judging that entry needs the ledger context only [`Derived::get_at`]
//!   takes — so a debug build `debug_assert!`s loudly, and a release build
//!   takes the safe default anyway (counts a miss, returns `None`) rather
//!   than guessing.
//! - [`Derived::get_at`] is the validity-aware path: it takes the ledger's
//!   current length and the dependencies touched since, and judges BOTH
//!   classes correctly. A stale entry is evicted on read — never returned,
//!   and never left behind to be silently reused — and counted as a miss,
//!   exactly like a fresh cold miss.
//!
//! Bundling the ledger cursor into every `get` call was the other
//! defensible shape; a separate accessor was chosen so the only tenant that
//! exists today pays nothing for a class it never stores.
//!
//! # What this store proves, and what it cannot
//!
//! The store's real contribution (spec §2.3) is making **key-completeness**
//! a typed obligation rather than a doc comment: a shape declares its key
//! type, and that key must carry every parameter its derivation reads. What
//! the store actually GUARANTEES is ordinary map correctness plus validity
//! enforcement — the value returned for a key is whatever was last
//! inserted under that exact key, AND a `Ledger` entry whose watched
//! dependency has since been touched is never returned as a hit. It does
//! **not**, and cannot, prove the key is COMPLETE — that the derivation
//! reads nothing the key leaves out. An incomplete key is a bug neither the
//! type system nor the validity check can see: it type-checks, inserts, and
//! reads back cleanly, and only ever diverges from a fresh recomputation
//! once an entry survives across two different values of the thing the key
//! omitted. Pressuring exactly that bug is [`Derived::evict_all`]'s job in
//! the property battery ("chaos eviction": evict at every legal opportunity
//! and assert output is unchanged) — a derivation that reads something
//! outside its key recomputes a DIFFERENT answer the moment its cache entry
//! is dropped and refilled, which chaos eviction forces to happen on every
//! step rather than leaving it to chance.

use std::collections::BTreeMap;

/// One store of a derived value, keyed by every parameter its derivation
/// reads, with each entry carrying its own [`Validity`]. See the module doc
/// for why this is generic per shape rather than one heterogeneous store,
/// why validity has two accessors rather than one, and for the
/// key-completeness obligation the store makes typed but cannot itself
/// discharge.
#[derive(Debug, Clone)]
pub struct Derived<K: Ord + Clone, V: Clone> {
    /// The resident entries, keyed by `K`'s own `Ord` — never a `HashMap`
    /// (decision 0005): iteration order must stay deterministic even though
    /// nothing here currently iterates. Each entry pairs its value with the
    /// [`Validity`] it was inserted under.
    entries: BTreeMap<K, (V, Validity)>,
    /// How many `get`/`get_at` calls, ever, found an already-cached, still
    /// valid entry. Never reset — the scaling property's own deterministic
    /// witness, never a wall-clock proxy (matching `RoomMeshMemo`'s
    /// counters, the-forebay Task 1). A stale `Ledger` entry does NOT count
    /// here, even though it was found.
    hits: u64,
    /// How many `get`/`get_at` calls, ever, found no entry OR found a stale
    /// one. Never reset.
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

    /// The `Pure`-only read: consult the store for `key`, counting a hit
    /// only when the cached entry declares [`Validity::Pure`] — the only
    /// class that needs no ledger context to judge. `None` on a miss, on
    /// no entry, or (loudly, in debug builds) on a caller bug: the key
    /// holds a [`Validity::Ledger`] entry, which only [`Self::get_at`] can
    /// judge correctly. See the module doc.
    pub fn get(&mut self, key: &K) -> Option<&V> {
        let is_ledger = matches!(self.entries.get(key), Some((_, Validity::Ledger { .. })));
        if is_ledger {
            debug_assert!(
                false,
                "Derived::get called on a key holding a Validity::Ledger entry; use get_at"
            );
            self.misses += 1;
            return None;
        }
        match self.entries.get(key) {
            Some((value, Validity::Pure)) => {
                self.hits += 1;
                Some(value)
            }
            _ => {
                self.misses += 1;
                None
            }
        }
    }

    /// The validity-aware read: consult the store for `key` as of a ledger
    /// whose length is now `current_position`, where `touched_since` names
    /// the [`DepKey`] of every fact committed since the entry's own
    /// recorded ledger position. `Derived` never reads a `Ledger` itself
    /// (the module doc explains why); this is the caller's own ledger
    /// accounting, handed in. Handles both validity classes correctly:
    /// `Pure` is always fresh; `Ledger` is stale exactly per
    /// [`Validity::is_stale`]. A stale entry is evicted on read — never
    /// returned, never left behind to be silently reused later — and
    /// counted as a miss, exactly like a fresh cold miss.
    /// type-audit: bare-ok(count: current_position)
    pub fn get_at(
        &mut self,
        key: &K,
        current_position: u64,
        touched_since: &[DepKey],
    ) -> Option<&V> {
        let stale = match self.entries.get(key) {
            Some((_, validity)) => validity.is_stale(current_position, touched_since),
            None => {
                self.misses += 1;
                return None;
            }
        };
        if stale {
            self.entries.remove(key);
            self.misses += 1;
            return None;
        }
        self.hits += 1;
        self.entries.get(key).map(|(value, _)| value)
    }

    /// Record `value` under `key` with [`Validity::Pure`], overwriting any
    /// prior entry at that key. The common case: every tenant this store
    /// has today is `Pure` (Task 3's `RoomMeshMemo`). Use
    /// [`Self::insert_with_validity`] for a `Validity::Ledger` entry.
    pub fn insert(&mut self, key: K, value: V) {
        self.entries.insert(key, (value, Validity::Pure));
    }

    /// Record `value` under `key` with an explicit [`Validity`], overwriting
    /// any prior entry at that key. The path a `Validity::Ledger` tenant
    /// uses; a `Pure` tenant can use this too, but [`Self::insert`] says the
    /// same thing more plainly.
    pub fn insert_with_validity(&mut self, key: K, value: V, validity: Validity) {
        self.entries.insert(key, (value, validity));
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

    /// How many [`Self::get`]/[`Self::get_at`] calls, ever, found an
    /// already-cached, still-valid entry. Never reset.
    /// type-audit: bare-ok(count: return)
    pub fn hits(&self) -> u64 {
        self.hits
    }

    /// How many [`Self::get`]/[`Self::get_at`] calls, ever, found no entry
    /// or a stale one. Never reset.
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

/// The invalidation discipline a [`Derived`] entry carries (spec §2.2): two
/// classes, split at key-completeness rather than at "world-derived" vs.
/// "ledger-derived" — the line the metaplan originally drew and the one
/// this store's own constructors refuted (spec §2.1). Stored per entry
/// (`Derived`'s internal map is `BTreeMap<K, (V, Validity)>`) and judged by
/// [`Derived::get`]/[`Derived::get_at`], per the module doc.
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
