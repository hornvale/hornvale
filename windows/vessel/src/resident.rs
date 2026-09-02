//! The resident fold store: the session-owned, per-entity accumulation of
//! what the ledger already determines (The Pawl, spec §2). The decisions this
//! layer will be ratified under are named in that spec's §8 and are not cited
//! by number here: none is written yet, and a cite to an unratified record is
//! a dangling reference the docs-consistency gate refuses.
//!
//! **The currency invariant is that a reader never observes a fold behind the
//! ledger it is given** (spec §2.2). The seam is at READ, not at commit:
//! `Ledger::commit` gains no hook, and every read of a tenant first advances
//! that tenant to the end of the ledger it was handed. `Folded::advance_to` is
//! idempotent in position, so a second evaluation of the same walk, a re-read
//! within a turn, or a snapshot after a `wait` each cost O(facts committed
//! since the last read) and absorb nothing twice — which is what makes the
//! tick's "same walk, read twice" shape (`Session::wait`) a non-issue rather
//! than a design constraint on the commit path. **Nothing here is ever
//! serialized**: the store has no `Serialize`, is not reachable from `World`,
//! and holds only values the ledger re-determines, so discarding it at any
//! instant is unobservable and a save written mid-session contains none of it.

use crate::liveness::{AGENT_AT, room_from_text};
use hornvale_kernel::fold::{Folded, LedgerFold};
use hornvale_kernel::{EntityId, Facet, Fact, Ledger, Value, WorldTime};
use std::collections::BTreeMap;

/// One entity's committed `agent-at` trail, in the order `agent_sightings`
/// sorted it: by `(day, room)` ascending. Absorbed once per fact, never
/// rebuilt. A resident permutation index (spec §2.4) — not a
/// cached hub: its contents are a strict subset of the ledger's own, each
/// fact is absorbed exactly once, and no caller ever rebuilds a timeline per
/// call.
///
/// **The insert is at the sorted position, not an append**, and that is the
/// one line of this type worth being deliberate about. Spec §3 rule 2 asks
/// whether an entity's `agent-at` facts ever commit out of day order; the
/// witness lives in `windows/vessel/tests/suite/resident_folds.rs` and prints
/// its verdict. Inserting at the sorted position is correct under either
/// answer, and it is still O(log h) to find the position plus a memmove —
/// never a rebuild. Ties (the same `(day, room)` twice) are inserted AFTER
/// their equals, which is what a stable sort of the commit order does, so the
/// two agree even where the key does not discriminate.
#[derive(Debug, PartialEq, Default)]
pub struct Trail {
    /// Each entity's sightings, ascending by `(day, room)`.
    by_entity: BTreeMap<EntityId, Vec<(WorldTime, Facet)>>,
}

impl Trail {
    /// The entity's trail, ascending by `(day, room)`; empty if never seen.
    pub fn of(&self, entity: EntityId) -> &[(WorldTime, Facet)] {
        self.by_entity
            .get(&entity)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }

    /// Index of the first entry with day > `t` — equivalently, the length of
    /// the `day <= t` prefix of [`Self::of`]. The trail is sorted by day, so
    /// this is a binary search rather than a scan; it is the door every
    /// past-instant read goes through.
    /// type-audit: bare-ok(index: return)
    pub fn prefix_len(&self, entity: EntityId, t: WorldTime) -> usize {
        self.of(entity).partition_point(|(d, _)| *d <= t)
    }
}

impl LedgerFold for Trail {
    fn empty() -> Self {
        Trail::default()
    }

    fn absorb(&mut self, fact: &Fact) {
        if fact.predicate != AGENT_AT {
            return;
        }
        let Value::Text(s) = &fact.object else {
            return;
        };
        // An undated `agent-at` is not a sighting: `agent_sightings` filters
        // it out (`f.day?`) and so does this.
        let Some(day) = fact.day else {
            return;
        };
        let entry = (day, room_from_text(s));
        let trail = self.by_entity.entry(fact.subject).or_default();
        // Strictly-less keeps equal keys in commit order, matching the stable
        // `sort_by` the scan half uses.
        let at = trail.partition_point(|e| *e < entry);
        trail.insert(at, entry);
    }
}

/// A [`ResidentFolds`] behind interior mutability — what a caller OWNS and
/// threads into `DriveMovements`: the session, a bench, a fixture.
///
/// The store is advanced on READ (spec §2.2) and several of its readers hold
/// only `&self`, which is what rules out a plain `&mut` and what rules out the
/// alternative spec §2.2 refuses outright, a throwaway rebuild per read. This
/// alias exists so the standard library type behind that is spelled in exactly
/// one place in this crate; it is a transparent alias, so a caller may name
/// the underlying type directly wherever that reads better.
pub type OwnedFolds = std::cell::RefCell<ResidentFolds>; // lexicon: std::cell::RefCell is the standard library's interior-mutability type — not a place at all, neither a mesh vertex nor an area

/// The session-owned resident fold store: one [`Folded`] per tenant,
/// advance-on-read.
///
/// `Trail` is its first and, as of this task, only tenant; later tasks add the
/// sustenance integrals, the believed-water set, the latest-visit map and the
/// alarm rooms (spec §2.4). The store knows nothing about what a tenant's
/// state means — a future tenant is a new [`LedgerFold`] impl and a new field,
/// never a new store.
#[derive(Debug, Default)]
pub struct ResidentFolds {
    /// Every entity's `agent-at` trail.
    trail: Folded<Trail>,
}

impl ResidentFolds {
    /// An empty store, every tenant at position 0.
    pub fn new() -> Self {
        ResidentFolds::default()
    }

    /// Advance every tenant to `ledger`'s end, then hand back the trail — the
    /// currency invariant, enforced at the one place a reader can reach the
    /// state.
    pub fn trail(&mut self, ledger: &Ledger) -> &Trail {
        self.trail.advance_to(ledger);
        self.trail.state()
    }

    /// Every tenant's position — the number of facts absorbed. With one tenant
    /// this is that tenant's position; when a second is added they advance
    /// together on every read, so a single number stays the honest answer and
    /// a divergence would be a bug this accessor is the witness for.
    /// type-audit: bare-ok(count: return)
    pub fn position(&self) -> u64 {
        self.trail.position()
    }
}
