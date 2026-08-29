//! Restricted passage (The Latch, arc IV.b; rebuilt on the object model by
//! The Chattel, arc IV.c) — the first precondition in Hornvale that reads
//! committed world facts.
//!
//! State is DERIVED, never stored. [`hornvale_worldgen::barrier_of`] gives a
//! chamber address its seeded barrier; only the CHANGE is committed. What
//! carries the change is the thing this module's own subject has become: a
//! cave mouth is a [`crate::thing`] of kind [`CAVE_MOUTH`], with a derived
//! [`EntityId`] and an [`crate::thing::OPENNESS`] fact, exactly like a
//! strongbox. Decision 0396.
//!
//! **`passage-cleared` is RETIRED, and the retirement is the deliverable.**
//! The Latch shipped a private predicate whose object carried the address as
//! `Value::Text`; decision 0367 made it monotone and said why, in terms that
//! forecast this module's rewrite: *"a closing act belongs with doors, lids,
//! and containers, because they are the same mechanism seen from three
//! angles, and a `passage-closed` predicate shipped now would be designed
//! against one of the three."* IV.c is the campaign in which all three angles
//! are visible, so the passage joins the object model rather than keeping a
//! private predicate of its own.
//!
//! **The one predicate the passage now writes is one another kind already
//! reads**, which is what makes the merge worth doing: `openness` is
//! non-functional and append-only, so a cave mouth opens, closes and opens
//! again, and [`effective_state`] is an as-of-day fold rather than a latch.
//!
//! **Monotonicity is gone, deliberately.** 0367's own closing consequence —
//! *"a monotone latch cannot express a trap … the first thing anyone will
//! want"* — is now expressible: a `Thin` mouth that was opened and then closed
//! falls back to `Thin` and re-bars. Nothing in this module relies on
//! [`BarrierState`]'s derived `Ord` any more, and no comment here claims it
//! does.
//!
//! **What a saved world loses.** A `possess --out` world written before this
//! flip carries `passage-cleared` facts nothing reads now, so a passage it
//! recorded as cleared is barred again on reload. The blast radius was
//! MEASURED rather than estimated: `grep -rl 'passage-cleared'
//! --include='*.json' .` returns nothing, so no committed fixture carries the
//! predicate and the break reaches only hand-made saves written since The
//! Latch landed. Decision 0189 is the precedent for a deliberate break.
//!
//! **Lifetime is the SESSION BY DEFAULT, and a played world is a FORK**
//! (decision 0368). The session ledger is a clone and the possessed world is
//! never mutated — `--world` is read-only. But `possess` takes a documented
//! `--out <PATH>`, and [`Session::into_played_world`] folds the evolved
//! ledger AND the per-session registry into a new `World` that `--out` saves;
//! decision 0171 rules that a player's acts are not filtered on the way out.
//! So without `--out` nothing survives, and with it these facts reach a new
//! world file that can be possessed again.
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

use hornvale_kernel::{ConceptRegistry, EntityId, Ledger, Seed, WorldTime};
use hornvale_worldgen::chamber::ChamberAddr;
use hornvale_worldgen::{BarrierPins, BarrierState, barrier_of};

/// The thing-kind a cave mouth is — the label `hornvale_thing::THING_KINDS`
/// carries and `affordance::object_registry` gives `Openable` and
/// `AffordsPassage`. Spelled once here so the lineage role below and the
/// `instance-of` object [`crate::thing::promote_role`] commits cannot drift
/// apart from each other.
/// type-audit: bare-ok(identifier-text)
pub const CAVE_MOUTH: &str = "cave-mouth";

/// A chamber address as text. Injective across every field of
/// [`ChamberAddr`]: two distinct addresses never share a key, or opening one
/// cave mouth would silently open another.
///
/// **Injectivity is not the only contract this string owes**, and what it owes
/// CHANGED with The Chattel without getting weaker. Under The Latch this was
/// the `Value::Text` object of a `passage-cleared` fact. It is now the address
/// leg of [`cave_mouth_role`] — an input to a derived [`EntityId`] — so a
/// changed spelling no longer makes one fold miss a committed fact, it
/// RENUMBERS every cave mouth in every saved world, which is the same class of
/// contract `thing::thing_role` and `thing::room_key` carry and a strictly
/// wider blast radius than before.
///
/// The band still rides as `{:?}`, so a [`hornvale_kernel::Band`] variant
/// rename keeps every key distinct while changing what every key SAYS.
/// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key`
/// (`tests/suite/passage.rs`) pins the literal that contains this one, for
/// that reason; a rename is an epoch, not an edit.
/// type-audit: bare-ok(identifier-text: return)
pub fn addr_key(addr: &ChamberAddr) -> String {
    format!(
        "{}/{:?}/{}/{}",
        addr.vertex.0, addr.band, addr.branch, addr.level
    )
}

/// The role leg of a cave mouth's lineage — [`crate::thing::thing_role`]'s
/// counterpart for a thing whose address is a [`ChamberAddr`] rather than a
/// room [`hornvale_kernel::Facet`].
///
/// **The `passage/` segment is what keeps the two namespaces apart.** A room
/// thing's role is `thing@<packed FacetId>/<kind>`, a decimal followed by a
/// kind; this one is `thing@passage/<addr key>/<kind>`. `passage` is not a
/// decimal, so no facet id can ever spell it and no address can ever collide
/// with a room — a property worth stating because the two roles feed ONE
/// derivation ([`crate::thing::id_for_role`]) and a collision there would be
/// two things wearing one identity.
///
/// A save-format contract, on exactly the terms
/// `crate::thing::thing_role`'s is: changing this spelling renumbers every
/// cave mouth in every saved world. A change is an epoch, not an edit, and
/// `the_cave_mouth_role_spelling_is_the_permanent_lineage_key` writes the
/// literal out so it cannot be rebaselined.
/// type-audit: bare-ok(identifier-text: return)
pub fn cave_mouth_role(addr: &ChamberAddr) -> String {
    format!("thing@passage/{}/{}", addr_key(addr), CAVE_MOUTH)
}

/// The entity the cave mouth at `addr` has — whether or not anything has ever
/// touched it. A pure derivation, exactly like [`crate::thing::thing_id`]:
/// nothing is minted, no ledger is read, and two ledgers that have never met
/// agree.
///
/// Ordinal 0 unconditionally: one address names one mouth, so there is no
/// sibling for an ordinal to distinguish.
pub fn cave_mouth_id(addr: &ChamberAddr) -> EntityId {
    crate::thing::id_for_role(&cave_mouth_role(addr), 0)
}

/// Record that the cave mouth at `addr` was opened (`open == true`) or closed,
/// on `day` — promoting it to a ledger entity first, so the thing exists in
/// the ledger with its `instance-of` kind and not merely as an id some fold
/// happens to derive.
///
/// **The one writer, in both directions.** A separate `open`/`close` pair
/// would be two functions whose promotion halves could drift; a caller that
/// promoted and forgot to commit, or committed against an id it derived
/// itself, would produce a ledger that reads correctly to its own author and
/// to nothing else. `Session::clear_passage_at` is the production caller and
/// the tests use the same door.
///
/// **Idempotent WITHIN A DAY, and only within one** (fix round 1, m4).
/// [`crate::thing::promote`]'s idempotence is `Ledger::commit`'s dedup of an
/// IDENTICAL fact, and [`hornvale_kernel::Fact`] derives `PartialEq` over all
/// six fields including `day` — so two calls on the same day yield one entity
/// and one `instance-of` fact, and two calls on DIFFERENT days yield one
/// entity and TWO `instance-of` facts, saying the same thing twice. The
/// `openness` fact is deliberately not idempotent across days either, and
/// that half is the point: it is what makes [`effective_state`] an as-of-day
/// fold rather than a flag.
///
/// **Harmless here, load-bearing for the verbs that come next.**
/// `Session::clear_passage_at` is the only production caller and it writes
/// only for `Thin`, so a second `clear` sees `Open` and writes nothing at
/// all; the two-facts case is unreachable through the verb. A general
/// `open`/`close` pair calling this repeatedly across days would accumulate
/// one redundant `instance-of` per call. Pinned by
/// `promoting_a_cave_mouth_on_a_second_day_repeats_its_instance_of`
/// (`tests/suite/passage.rs`) so the caveat is a measured fact rather than a
/// warning that rots.
/// type-audit: bare-ok(flag: open)
pub fn set_openness(
    ledger: &mut Ledger,
    registry: &ConceptRegistry,
    addr: &ChamberAddr,
    open: bool,
    day: WorldTime,
) -> Result<EntityId, crate::thing::ThingError> {
    let id =
        crate::thing::promote_role(ledger, registry, &cave_mouth_role(addr), CAVE_MOUTH, 0, day)?;
    ledger.commit(crate::thing::openness_fact(id, open, day), registry)?;
    Ok(id)
}

/// The barrier state at `addr` as of `day`: [`BarrierState::Open`] if the
/// latest [`crate::thing::OPENNESS`] fact about this cave mouth at or before
/// `day` says open, otherwise the seeded state [`barrier_of`] draws.
///
/// The shape is the one spec §3.7 pins, unchanged from The Latch:
///
/// ```text
///   effective_state(thing, day) =
///       Open                     if the latest openness fact at-or-before day is true
///       barrier_of(seed, addr)   if it is false, or if there is none
/// ```
///
/// **NOT MONOTONE, and that is the deliverable rather than a regression.**
/// Decision 0367 short-circuited on *any* clearing fact ever committed, so no
/// path could lower a barrier and `BarrierState`'s derived `Ord` (`Sealed <
/// Warded < Thin < Open`) agreed with the rule by construction. Neither
/// sentence is true now: [`crate::thing::is_open`] takes the LATEST posting,
/// so a close after an open re-bars, and the `Ord` is just an ordering on a
/// four-valued enum. Decision 0396 supersedes 0367.
///
/// **Closing does not invent a barrier the seed never drew**, which is the
/// asymmetry the formula above encodes and the one thing about it worth
/// stating twice: `open == false` falls back to `barrier_of`, so closing a
/// mouth the seed drew as `Open` reads `Open` still. A trap needs a seeded
/// barrier to fall back TO; the thing this expresses is re-barring, not
/// barring from nothing. Pinned in both directions by
/// `a_closed_cave_mouth_falls_back_to_the_barrier_the_seed_drew`.
///
/// **Time-correct.** The `<= day` filter lives in
/// [`crate::thing::is_open`]'s own fold, the same discipline
/// `last_fact_day_at_or_before` uses in the liveness walk: a fold over the
/// whole history would look chronologically PAST the instant being asked
/// about, which is precisely the failure mode that makes a mutable flag wrong
/// for a replayed past.
///
/// No subject is consulted beyond the mouth itself — any body's opening act
/// opens the passage for everyone, which is what makes this the 90% rung
/// rather than a private daybook entry.
pub fn effective_state(
    ledger: &Ledger,
    seed: Seed,
    addr: &ChamberAddr,
    day: WorldTime,
    pins: &BarrierPins,
) -> BarrierState {
    if crate::thing::is_open(ledger, cave_mouth_id(addr), day) == Some(true) {
        return BarrierState::Open;
    }
    barrier_of(seed, addr.vertex, addr.band, addr.branch, pins)
}
