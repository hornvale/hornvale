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

use hornvale_kernel::{EntityId, Fact, Value, WorldTime};
use hornvale_worldgen::chamber::ChamberAddr;

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
