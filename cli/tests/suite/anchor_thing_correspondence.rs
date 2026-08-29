//! The Chattel: every `hornvale_vessel::interior::AnchorKind` has a
//! counterpart in `hornvale_thing::THING_KINDS`, and every key the vessel's
//! property table mints is a row that roster actually carries.
//!
//! **This file exists in `cli/` because it is the only crate that already
//! depends on both.** A domain may not depend on a window (`domains/thing`
//! cannot see `AnchorKind`), and `windows/vessel` carries no dependency on
//! `hornvale-thing` — it needs one for nothing else, and adding a production
//! dependency to hold a test-only invariant would move the workspace's
//! dependency graph (a drift-checked artifact) for no runtime reason.
//!
//! **Task 7 turned this from a duplicate into a check.** It used to carry
//! its own private `thing_label_for` — a hand-copy of a mapping that then
//! lived nowhere else — so it asserted that a table in this file agreed with
//! `THING_KINDS`, and said nothing about any mapping production used. Task 7
//! put the real mapping in `hornvale_vessel::affordance::thing_kind_of`
//! (total, exhaustive, no wildcard arm), and this file now reads THAT. A
//! duplicated rule whose cheapest repair deletes one side is the shape
//! decision 0261 warns about; deleting the copy was the repair.
//!
//! **What the re-key made this load-bearing for.** The Offer keyed the
//! property table on `AnchorKind`, a closed enum: a mis-spelled key could not
//! compile. Task 7 keyed it on `KindId`, a newtype over `&'static str`, where
//! `KindId("srongbox")` is a well-typed key that silently resolves to "this
//! kind carries no property". Nothing in `windows/vessel` can close that gap.
//! This file is what closes it.

use hornvale_vessel::affordance::{object_registry, thing_kind_of};
use hornvale_vessel::interior::AnchorKind;

/// Every `AnchorKind` variant. Hand-listed because `AnchorKind` has no
/// `all()`; the compile-time guard against an appended variant is
/// `thing_kind_of`'s own exhaustive match (and
/// `windows/vessel/tests/suite/affordance.rs`'s scan for a wildcard arm that
/// would defeat it), not this list.
const EVERY_ANCHOR_KIND: [AnchorKind; 14] = [
    AnchorKind::Hearth,
    AnchorKind::Threshold,
    AnchorKind::Bed,
    AnchorKind::Vessel,
    AnchorKind::Screen,
    AnchorKind::Pool,
    AnchorKind::Log,
    AnchorKind::Ground,
    AnchorKind::Alcove,
    AnchorKind::Strongbox,
    AnchorKind::HighSeat,
    AnchorKind::Loom,
    AnchorKind::Anvil,
    AnchorKind::Altar,
];

/// Every anchor kind's thing-kind — read from the production mapping — is a
/// row `hornvale_thing::THING_KINDS` actually carries.
///
/// MUTATION THIS MUST FAIL AGAINST: point `thing_kind_of`'s `HighSeat` arm at
/// `KindId("high_seat")` (an underscore where the roster spells a hyphen —
/// the single most likely way to mint an unbacked key, and one no `windows/
/// vessel` test can see, since a key absent from `object_registry` is a legal
/// property-less kind there). Red observed:
///
/// ```text
/// thread 'anchor_thing_correspondence::every_anchor_kind_has_a_thing_kind_counterpart'
/// panicked at cli/tests/suite/anchor_thing_correspondence.rs:
/// HighSeat maps to thing-kind KindId("high_seat"), which THING_KINDS does not carry
/// ```
#[test]
fn every_anchor_kind_has_a_thing_kind_counterpart() {
    for kind in EVERY_ANCHOR_KIND {
        let id = thing_kind_of(kind);
        assert!(
            hornvale_thing::THING_KINDS.contains(&id.0),
            "{kind:?} maps to thing-kind {id:?}, which THING_KINDS does not carry"
        );
    }
}

/// The other direction the re-key opened: every key `object_registry` mints
/// is a real thing-kind too. `key` and `cave-mouth` have no `AnchorKind`
/// behind them at all (that is the point of keying on thing-kind — a cave
/// mouth is a `Vertex`, a key is carried), so the test above cannot reach
/// them, and a typo in either row would produce a property nothing carries
/// while `each_property_is_carried_by_at_least_one_thing_kind` stayed green
/// on the misspelt row.
///
/// MUTATION THIS MUST FAIL AGAINST: spell `object_registry`'s key row
/// `KindId("cave_mouth")`. Red observed:
///
/// ```text
/// thread 'anchor_thing_correspondence::every_property_table_key_is_a_real_thing_kind'
/// panicked at cli/tests/suite/anchor_thing_correspondence.rs:
/// object_registry carries properties for KindId("cave_mouth"), which is not a
/// hornvale_thing::THING_KINDS row — the key space widened from a closed enum to
/// an arbitrary string when the table re-keyed, and this is what replaces the
/// compiler
/// ```
#[test]
fn every_property_table_key_is_a_real_thing_kind() {
    for id in object_registry().ids() {
        assert!(
            hornvale_thing::THING_KINDS.contains(&id.0),
            "object_registry carries properties for {id:?}, which is not a \
             hornvale_thing::THING_KINDS row — the key space widened from a \
             closed enum to an arbitrary string when the table re-keyed, and \
             this is what replaces the compiler"
        );
    }
}

/// Anti-vacuity for both tests above: they sweep a non-empty population, and
/// the roster they check against is not so permissive that any string passes.
/// Without this, deleting every row of `object_registry` — or replacing
/// `THING_KINDS` with a list that happened to contain everything — would read
/// as two green results.
#[test]
fn the_correspondence_checks_are_not_vacuous() {
    assert_eq!(EVERY_ANCHOR_KIND.len(), 14);
    assert!(
        object_registry().ids().count() >= 9,
        "object_registry lost rows; the key check above sweeps whatever is left"
    );
    assert!(
        !hornvale_thing::THING_KINDS.contains(&"high_seat"),
        "THING_KINDS must not carry a label the mutations above rely on being \
         absent, or those reds are not reproducible"
    );
}
