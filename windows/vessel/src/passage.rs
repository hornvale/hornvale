//! Restricted passage (The Latch, arc IV.b) — the first precondition in
//! Hornvale that reads committed world facts.
//!
//! State is DERIVED, never stored. [`hornvale_worldgen::barrier_of`] gives a
//! chamber address its seeded barrier; only the CHANGE is committed, as a
//! [`PASSAGE_CLEARED`] fact whose object carries the address as
//! `Value::Text`. This follows `agent_at_fact`'s own precedent — places are
//! values here, never entity subjects — so no entity is minted and nothing
//! new is serialized.
//!
//! **Monotone by ruling:** a latch, once thrown, stays thrown. Re-closing is
//! arc IV.c's, alongside containers.
//!
//! **Lifetime is the SESSION, not the world** (spec section 3.1): the session
//! ledger is a clone that is never written back, so this — like every other
//! fact live play commits — evaporates when the possession ends.

use hornvale_kernel::{EntityId, Fact, Seed, Value, WorldTime};
use hornvale_worldgen::chamber::ChamberAddr;
use hornvale_worldgen::{BarrierPins, BarrierState, barrier_of};

/// The predicate naming a passage a body has cleared. Registered PER-SESSION
/// (never at genesis), following `AGENT_AT`'s own discipline — see
/// `Session::start`.
/// type-audit: bare-ok(identifier-text)
pub const PASSAGE_CLEARED: &str = "passage-cleared";

/// A chamber address as a ledger-storable key. Injective across every field
/// of [`ChamberAddr`]: two distinct addresses never share a key, or clearing
/// one passage would silently clear another.
/// type-audit: bare-ok(identifier-text: return)
pub fn addr_key(addr: &ChamberAddr) -> String {
    format!(
        "{}/{:?}/{}/{}",
        addr.vertex.0, addr.band, addr.branch, addr.level
    )
}

/// The fact committed when `who` clears the passage at `addr` on `day`.
/// The address rides in the OBJECT, the clearing body is the SUBJECT — the
/// same shape `agent_at_fact` uses for position.
pub fn cleared_fact(who: EntityId, addr: &ChamberAddr, day: WorldTime) -> Fact {
    Fact {
        subject: who,
        predicate: PASSAGE_CLEARED.to_string(),
        object: Value::Text(addr_key(addr)),
        place: None,
        day: Some(day),
        provenance: "the-latch: a body cleared a barred passage".to_string(),
    }
}

/// The barrier state at `addr` as of `day`: [`BarrierState::Open`] if any
/// committed [`PASSAGE_CLEARED`] fact names this address at or before `day`,
/// otherwise the seeded state [`barrier_of`] draws.
///
/// **Monotone and time-correct.** The `<= day` filter is the same discipline
/// `last_fact_day_at_or_before` uses in the liveness walk: a fold over the
/// whole history would look chronologically PAST the instant being asked
/// about, which is precisely the failure mode that makes a mutable flag wrong
/// for a replayed past.
///
/// The subject is not consulted — any body's clearing fact opens the passage
/// for everyone, which is what makes this the 90% rung rather than a private
/// daybook entry.
pub fn effective_state(
    ledger: &hornvale_kernel::Ledger,
    seed: Seed,
    addr: &ChamberAddr,
    day: WorldTime,
    pins: &BarrierPins,
) -> BarrierState {
    let key = addr_key(addr);
    let cleared = ledger.find(PASSAGE_CLEARED).any(|f| {
        matches!(&f.object, Value::Text(t) if *t == key) && f.day.is_some_and(|d| d <= day)
    });
    if cleared {
        return BarrierState::Open;
    }
    barrier_of(seed, addr.vertex, addr.band, addr.branch, pins)
}
