//! The Chattel, Task 6 — latency across TWO ENTRIES into one room, over the
//! public surface and against the REAL pattern grammar.
//!
//! The in-module tests in `windows/vessel/src/thing.rs` pin `is_latent`'s
//! arms one at a time against hand-built facets. This file asks the question
//! the campaign actually has, which none of them can: *does a room stop
//! offering the thing that was carried out of it?* It composes a real
//! `Interior` through `interior_of`, filters the composed anchors by
//! `is_latent`, and compares the offer list on entry 1 against entry 2.
//!
//! **The two conjuncts of spec §3.4's latency rule are both visible here, and
//! they are visible in two different places.** The grammar's half is
//! `interior_of` — what the room offers at all — and it belongs to the
//! CALLER, because `is_latent` holds no terrain, no seed and no world. The
//! ledger's half is `is_latent`. `offers_of` below is the four-line join, and
//! it is written out rather than imported because nothing in the tree
//! performs it yet: Task 12's verbs are what will, and this file is the
//! standing demonstration that the signature they inherit is sufficient.
//!
//! **The `AnchorKind` -> thing-kind spelling here is a STAND-IN.** Task 7 owns
//! the real, total mapping (spec §3.6, one property table keyed on `KindId`);
//! until it lands there is nothing to import, so `kind_name` derives a name
//! from the anchor's own `Debug`. Nothing in this file asserts on the
//! spelling — only that whatever spelling the caller uses on both entries
//! addresses the same thing.

use hornvale_kernel::{ConceptRegistry, Facet, Ledger, WorldTime};
use hornvale_vessel::interior::{AnchorId, AnchorKind, Interior, interior_of};
use hornvale_vessel::liveness::Terrain;
use hornvale_vessel::thing::{
    LOCATED_IN, LOCATED_IN_DOC, held_by, is_latent, located_in_holder_fact, located_in_room_fact,
    promote, thing_id,
};

/// A terrain that is built and cold everywhere — the same four-method
/// stand-in `chamber_prose.rs`'s own grammar test uses, and for the same
/// reason: `interior_of` reads exactly `is_built` and `is_cold`, and a real
/// world would cost a full genesis to answer two booleans.
struct ColdBuilt;

impl Terrain for ColdBuilt {
    fn elevation(&self, _room: &Facet) -> f64 {
        0.0
    }
    fn is_fresh_water(&self, _room: &Facet) -> bool {
        false
    }
    fn temperature(&self, _room: &Facet, _day: WorldTime) -> f64 {
        -20.0
    }
    fn is_built(&self, _room: &Facet) -> bool {
        true
    }
}

/// The stand-in thing-kind spelling for an anchor kind — see the module doc.
/// **Why this must not ship as the real mapping, which Task 7 owns.** It is
/// injective today, so a non-injective replacement would red this file rather
/// than pass quietly — but `thing_role` makes the kind string an input to a
/// derived `EntityId` and its own doc calls that a save-format contract,
/// while `#[derive(Debug)]` output is not one: a variant rename would
/// silently renumber every thing in every saved world. This stand-in also
/// exercises 7 of 14 `AnchorKind` variants (built + cold, locale band only),
/// so it cannot witness a mapping that is partial over the chamber-band
/// seven.
fn kind_name(kind: AnchorKind) -> String {
    format!("{kind:?}").to_lowercase()
}

/// **The join**: the grammar's offer list, minus everything the ledger says
/// has left. This is the shape spec §3.4 describes and the shape that makes
/// the cost one indexed lookup per SLOT — the interior is enumerated once and
/// each slot asks one question, so nothing here scales with world history.
fn offers_of(
    ledger: &Ledger,
    room: &Facet,
    interior: &Interior,
    day: WorldTime,
) -> Vec<(AnchorId, AnchorKind)> {
    interior
        .ids()
        .into_iter()
        .map(|id| (id, interior.anchor(id).kind))
        .filter(|(_, kind)| {
            is_latent(ledger, room, &kind_name(*kind), 0, day).expect("a shallow room facet packs")
        })
        .collect()
}

fn at(days: f64) -> WorldTime {
    WorldTime::from_std_days(days).expect("a small day count is in range")
}

/// A room offers every anchor its grammar composes until something carries
/// one out, and then it offers exactly the rest — the negative fold, asserted
/// across two entries into the SAME room.
///
/// The three assertions are one claim each and the middle one is the reason
/// the other two are not enough on their own:
///
/// 1. entry 1 offers the whole composed interior (nothing has happened yet);
/// 2. entry 2 offers exactly one fewer, and the missing one is the anchor
///    that was carried out — not merely *an* anchor;
/// 3. the room it was carried INTO does not lose its own water jar, because
///    that is a different entity (`thing_id` is keyed on the room).
///
/// MUTATION THIS FAILS AGAINST: `Some(Value::Text(room)) => room == here` to
/// `Some(Value::Text(room)) => room != here` in `is_latent` — the SENSE of
/// the one comparison the negative fold is made of. Under it the jar's
/// location (another room) reads as "still here", so entry 2 offers all
/// seven anchors and nothing has gone:
///
/// ```text
/// thread 'thing::a_room_stops_offering_what_was_carried_out_of_it' panicked
///   at windows/vessel/tests/suite/thing.rs:164:5:
/// assertion `left == right` failed: entry 2 must offer exactly the anchors
/// entry 1 did, minus the one that left
///   left: []
///  right: [Vessel]
/// ```
#[test]
fn a_room_stops_offering_what_was_carried_out_of_it() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(LOCATED_IN, false, LOCATED_IN_DOC)
        .expect("a fresh registry accepts the location predicate");
    let mut ledger = Ledger::default();

    let here = Facet {
        face: 0,
        path: vec![1],
    };
    let elsewhere = Facet {
        face: 0,
        path: vec![2],
    };
    let interior = interior_of(&here, &ColdBuilt);

    // The grammar's half, on its own: a built, cold locale composes a water
    // jar among its anchors. Asserted rather than assumed, because the whole
    // test is about that anchor and a grammar change must red HERE rather
    // than silently reduce the test to a tautology over an empty list.
    assert!(
        interior
            .ids()
            .into_iter()
            .any(|id| interior.anchor(id).kind == AnchorKind::Vessel),
        "a built, cold locale must compose `the-water-jar`; if the grammar \
         moved, this test needs a different anchor, not a rebaseline"
    );

    let entry_one = offers_of(&ledger, &here, &interior, at(1.0));
    assert_eq!(
        entry_one.len(),
        interior.ids().len(),
        "before anything is touched, every composed anchor is on offer"
    );

    // Something takes the water jar to the next room.
    let jar = thing_id(&here, &kind_name(AnchorKind::Vessel), 0).expect("a shallow facet packs");
    ledger
        .commit(
            located_in_room_fact(jar, &elsewhere, at(2.0)).expect("a shallow facet packs"),
            &registry,
        )
        .expect("the location predicate is registered");

    let entry_two = offers_of(&ledger, &here, &interior, at(3.0));
    let gone: Vec<AnchorKind> = entry_one
        .iter()
        .filter(|(id, _)| !entry_two.iter().any(|(kept, _)| kept == id))
        .map(|(_, kind)| *kind)
        .collect();
    assert_eq!(
        gone,
        vec![AnchorKind::Vessel],
        "entry 2 must offer exactly the anchors entry 1 did, minus the one \
         that left"
    );

    // And the room it went to still offers its OWN water jar: two rooms'
    // water jars are two entities, so carrying one in does not fill the
    // other's slot.
    let there = interior_of(&elsewhere, &ColdBuilt);
    assert_eq!(
        offers_of(&ledger, &elsewhere, &there, at(3.0)).len(),
        there.ids().len(),
        "a thing carried INTO a room does not consume that room's own latent \
         slot of the same kind"
    );
}

/// A thing carried out and BROUGHT BACK is offered again — the same room,
/// a third entry, and the case that separates the negative fold from "has
/// this thing ever been touched".
///
/// It is the two-entry test's necessary partner rather than a repeat of the
/// in-module `a_thing_put_back_is_still_here`: at this level the claim is
/// about the OFFER LIST a room composes, so an implementation that read any
/// `located-in` fact as departure would show a room permanently one anchor
/// short after a player picked something up and set it down again — a
/// visible, durable hole in the world that carries away no assertion in the
/// unit tests below the join.
///
/// MUTATION THIS FAILS AGAINST: `Some(Value::Text(room)) => room == here` to
/// `Some(Value::Text(_)) => false` in `is_latent`. It reds exactly two tests
/// in the vessel crate — this one and its unit-level twin
/// `thing::tests::a_thing_put_back_is_still_here` — while the two-entry test
/// above stays green, because the jar really did leave:
///
/// ```text
/// thread 'thing::a_room_offers_again_what_was_brought_back' panicked at
///   windows/vessel/tests/suite/thing.rs:249:5:
/// assertion `left == right` failed: a jar set back down is on offer again:
/// the fold is keyed on WHERE the thing is, never on whether anything has
/// ever happened to it
///   left: 6
///  right: 7
/// ```
#[test]
fn a_room_offers_again_what_was_brought_back() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(LOCATED_IN, false, LOCATED_IN_DOC)
        .expect("a fresh registry accepts the location predicate");
    let mut ledger = Ledger::default();

    let here = Facet {
        face: 0,
        path: vec![1],
    };
    let elsewhere = Facet {
        face: 0,
        path: vec![2],
    };
    let interior = interior_of(&here, &ColdBuilt);
    let jar = thing_id(&here, &kind_name(AnchorKind::Vessel), 0).expect("a shallow facet packs");

    ledger
        .commit(
            located_in_room_fact(jar, &elsewhere, at(2.0)).expect("a shallow facet packs"),
            &registry,
        )
        .expect("the location predicate is registered");
    ledger
        .commit(
            located_in_room_fact(jar, &here, at(4.0)).expect("a shallow facet packs"),
            &registry,
        )
        .expect("the location predicate is registered");

    assert_eq!(
        offers_of(&ledger, &here, &interior, at(3.0)).len(),
        interior.ids().len() - 1,
        "between the taking and the return the room really is one anchor \
         short — without this the assertion below could pass on a fold that \
         never fires"
    );
    assert_eq!(
        offers_of(&ledger, &here, &interior, at(5.0)).len(),
        interior.ids().len(),
        "a jar set back down is on offer again: the fold is keyed on WHERE \
         the thing is, never on whether anything has ever happened to it"
    );
}

// --- The Chattel, Task 11: the custody fold ---------------------------

/// What a holder holds, over the PUBLIC surface — the O-shaped question
/// `thing::held_by` answers and the one every other fold in that module
/// answers backwards.
///
/// **Three states, and the middle one is what separates a fold from a
/// search.** A thing put into a holder is held; a thing put somewhere else
/// afterwards is not, even though `Ledger::query_by_object` still finds the
/// putting-in fact and always will (the ledger is append-only); and a thing
/// nobody ever placed is not held by anyone. An implementation that trusted
/// the O-index alone passes the first and third and fails the second, which
/// is why the second exists.
///
/// It also pins the answer's ORDER: `EntityId` order, from the `BTreeSet` the
/// fold collects through, never ledger order — a caller that rendered "you
/// are carrying …" would otherwise print a list whose sequence depended on
/// how the history happened to be written.
///
/// MUTATION THIS MUST FAIL AGAINST: in `thing::held_by`, drop the
/// latest-posting re-ask — `.filter(|&thing| location_of(ledger, thing, day)
/// == Some(Value::Entity(holder)))` -> `.filter(|&thing| { let _ = thing;
/// true })`. Confirmed 2026-08-29, unfiltered over the whole crate (`847
/// tests run: 845 passed, 2 failed` — this test and
/// `session::tests::a_lockable_thing_opens_only_with_the_key_in_custody`,
/// which is the same property asserted one layer up):
///
/// ```text
/// assertion `left == right` failed: the chest holds what was LAST put in it:
/// `given_up`'s putting-in fact is still in the ledger and must not count,
/// and `untouched` (EntityId(621894274334195712)) was never placed at all
///   left: [EntityId(3811067634696519680), EntityId(3811067634696519681)]
///  right: [EntityId(3811067634696519680)]
/// ```
#[test]
fn a_holder_holds_what_was_last_put_in_it_and_nothing_it_has_given_up() {
    let mut registry = ConceptRegistry::default();
    registry
        .register_predicate(LOCATED_IN, false, LOCATED_IN_DOC)
        .expect("a fresh registry accepts located-in");
    registry
        .register_predicate(hornvale_kernel::INSTANCE_OF, false, "the kind of a thing")
        .expect("a fresh registry accepts instance-of");
    let mut ledger = Ledger::default();

    let room = Facet {
        face: 0,
        path: vec![1],
    };
    let chest = promote(&mut ledger, &registry, &room, "strongbox", 0, at(1.0))
        .expect("a shallow facet packs");
    let kept =
        promote(&mut ledger, &registry, &room, "key", 0, at(1.0)).expect("a shallow facet packs");
    let given_up =
        promote(&mut ledger, &registry, &room, "key", 1, at(1.0)).expect("a shallow facet packs");
    let untouched = thing_id(&room, "log", 0).expect("a shallow facet packs");

    for thing in [kept, given_up] {
        ledger
            .commit(located_in_holder_fact(thing, chest, at(2.0)), &registry)
            .expect("located-in is registered and non-functional");
    }
    // ...and one of them comes back out, into the room.
    ledger
        .commit(
            located_in_room_fact(given_up, &room, at(3.0)).expect("a shallow facet packs"),
            &registry,
        )
        .expect("located-in is registered and non-functional");

    assert_eq!(
        held_by(&ledger, chest, at(4.0)),
        vec![kept],
        "the chest holds what was LAST put in it: `given_up`'s putting-in \
         fact is still in the ledger and must not count, and `untouched` \
         ({untouched:?}) was never placed at all"
    );
    assert_eq!(
        held_by(&ledger, chest, at(2.5)),
        {
            let mut both = vec![kept, given_up];
            both.sort();
            both
        },
        "as of an instant BEFORE the thing came back out, it was still held \
         — the fold is evaluated at the day asked about, never at the end of \
         history"
    );
    assert!(
        held_by(&ledger, untouched, at(4.0)).is_empty(),
        "a thing nobody ever placed anything in holds nothing"
    );
}
