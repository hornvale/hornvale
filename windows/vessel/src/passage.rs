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
//! **Lifetime is the SESSION BY DEFAULT, and a played world is a FORK**
//! (spec section 3.1, decision 0368). This paragraph used to say the fact
//! "evaporates when the possession ends… the session ledger is a clone that
//! is never written back," and that was wrong in the half that mattered.
//!
//! The session ledger is indeed a clone and the possessed world is never
//! mutated — `--world` is read-only. But `possess` takes a documented
//! `--out <PATH>`, and [`Session::into_played_world`] folds the evolved
//! ledger AND the per-session registry into a new `World` that `--out`
//! saves; decision 0171 rules that a player's acts are not filtered on the
//! way out. So without `--out` nothing survives, and with it these facts
//! reach a new world file that can be possessed again.
//!
//! **What is proved here is the session claim**
//! (`a_cleared_passage_stays_open_for_the_rest_of_the_session`). The save
//! round trip — clear, `--out`, re-possess, delve — is NOT tested, so it is
//! not claimed. The carrying mechanism demonstrably works for `agent-at`, a
//! sibling predicate committed through the same `Ledger::commit` call on the
//! same ledger, and nothing here differs from it; that is a strong inference
//! and still an inference.
//!
//! [`Session::into_played_world`]: crate::Session::into_played_world

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
