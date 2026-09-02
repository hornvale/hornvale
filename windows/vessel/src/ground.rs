//! The room memo (The Detent, spec §2.1): what the TERRAIN determines about a
//! room, held for the session. The world-derived half of the adaptive cache
//! whose ledger-derived half is [`crate::resident`] — a `Validity::Pure`
//! tenant of the kernel's `Derived` store (decision 0206), keyed by the room
//! alone.
//!
//! # One memo per `(LocaleContext, predator field)`
//!
//! `Terrain::hazards` reads the room, the locale's climate/regime/geosphere
//! and the session's predator field, none of which change after
//! `Session::start`; the key is the room and the REST is supplied by
//! ownership — the session that owns the context and the field owns this
//! memo, and hands it to exactly the terrains built over them. That is the
//! same rule `SustenanceMemo` states for the temperature field. A caller
//! with a second terrain over a different field builds a second memo; the
//! two-terrain test in `tests/suite/the_detent.rs` is what refuses sharing.
//!
//! # What could make it wrong, and why it cannot happen silently
//!
//! A hazard that depends on the day. `Terrain::hazards` takes no `day` by
//! contract ("a slow field"); a seasonal hazard would change that signature,
//! and this memo's key is the first thing the compiler refuses. A predator
//! field that moves per tick would need `Validity::Ledger` or a rebuild per
//! tick; today it is computed once per session (`Session.predator`).

use crate::liveness::Hazards;
use hornvale_kernel::Facet;
use hornvale_kernel::derived::Derived;

/// The per-room hazard memo. See the module doc.
#[derive(Debug, Default)]
pub struct GroundHazards {
    /// `room -> hazards(room)`, `Validity::Pure`, never invalidated.
    memo: Derived<Facet, Hazards>,
}

impl GroundHazards {
    /// An empty memo.
    pub fn new() -> Self {
        Self::default()
    }

    /// The hazards at `room`: the held value on a hit, else `compute()`,
    /// held from then on. The value returned on a miss IS the value held, so
    /// a later hit returns the identical `f64`s.
    pub fn hazards_or_insert_with(
        &mut self,
        room: &Facet,
        compute: impl FnOnce() -> Hazards,
    ) -> Hazards {
        if let Some(h) = self.memo.get(room) {
            return *h;
        }
        let h = compute();
        self.memo.insert(room.clone(), h);
        h
    }

    /// Rooms held.
    /// type-audit: bare-ok(count: return)
    pub fn len(&self) -> usize {
        self.memo.len()
    }
    /// Whether nothing is held.
    /// type-audit: bare-ok(flag: return)
    pub fn is_empty(&self) -> bool {
        self.memo.is_empty()
    }
    /// Reads served without computing, ever.
    /// type-audit: bare-ok(count: return)
    pub fn hits(&self) -> u64 {
        self.memo.hits()
    }
    /// Reads that computed, ever — the field samples taken.
    /// type-audit: bare-ok(count: return)
    pub fn misses(&self) -> u64 {
        self.memo.misses()
    }
    /// Drop every held room (chaos eviction; unobservable by construction).
    pub fn evict_all(&mut self) {
        self.memo.evict_all()
    }
}

/// The memo behind interior mutability, so `&self` terrain readers can fill
/// it — the `OwnedFolds` shape (The Pawl, spec §2.2).
pub type OwnedGround = std::cell::RefCell<GroundHazards>; // lexicon: std::cell::RefCell is the standard library's interior-mutability type, not a place

#[cfg(test)]
mod tests {
    use super::*;

    fn room(face: u8) -> Facet {
        Facet {
            face,
            path: Vec::new(),
        }
    }
    fn hz(v: f64) -> Hazards {
        Hazards {
            uncanny: v,
            heat: 0.0,
            cold: 0.0,
            predator: 0.0,
        }
    }

    #[test]
    fn a_miss_computes_and_a_hit_does_not() {
        let mut g = GroundHazards::new();
        let mut computed = 0;
        let a = g.hazards_or_insert_with(&room(0), || {
            computed += 1;
            hz(0.5)
        });
        let b = g.hazards_or_insert_with(&room(0), || {
            computed += 1;
            hz(0.9)
        });
        assert_eq!(computed, 1, "the second read must not recompute");
        assert_eq!(a, b, "the held value is the first one computed");
        assert_eq!((g.hits(), g.misses(), g.len()), (1, 1, 1));
    }

    #[test]
    fn evicting_everything_recomputes_the_same_value() {
        let mut g = GroundHazards::new();
        let a = g.hazards_or_insert_with(&room(3), || hz(0.25));
        g.evict_all();
        assert!(g.is_empty());
        let b = g.hazards_or_insert_with(&room(3), || hz(0.25));
        assert_eq!(a, b);
        assert_eq!(g.misses(), 2);
    }
}
