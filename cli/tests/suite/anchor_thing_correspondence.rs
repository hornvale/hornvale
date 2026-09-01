//! The Chattel: every key the vessel's property table mints is a row
//! `hornvale_thing::THING_KINDS` actually carries.
//!
//! **This file exists in `cli/` because it WAS the only crate that already
//! depended on both, and that reason lapsed with The Wicket's Task 2**, which
//! gave `windows/vessel` an ordinary `hornvale-thing` dependency (a window
//! may depend on a domain; vessel already depends on eight of them). Task 3
//! deletes this file and reopens its checks in
//! `windows/vessel/tests/suite/kind_totality.rs`, where their subject lives.
//! Nothing here is intended to survive that task.
//!
//! **What the re-key made this load-bearing for.** The Offer keyed the
//! property table on a closed enum: a mis-spelled key could not compile.
//! Task 7 keyed it on `KindId`, a newtype over `&'static str`, where
//! `KindId("srongbox")` is a well-typed key that silently resolves to "this
//! kind carries no property". That is the gap these checks close, and spec
//! §5.1's G-e is where the closing moves to.
//!
//! **One check left with the enum (The Wicket, Task 2).**
//! `every_anchor_kind_has_a_thing_kind_counterpart` swept every anchor-kind
//! variant through `affordance::thing_kind_of` and asserted the result was a
//! roster row. Both the enum and the mapping are gone: an anchor CARRIES a
//! `KindId` now, so the sweep would have been over
//! `hornvale_thing::kinds::EVERY_HANDLE` asserting each handle is a roster
//! row — which is exactly `domains/thing`'s own
//! `every_named_handle_is_a_roster_row` (G-d), already written by Task 1. Two
//! copies of one rule whose cheapest repair deletes one side is the shape
//! decision 0261 warns about, so the copy was not made.

use hornvale_vessel::affordance::object_registry;

/// The other direction the re-key opened: every key `object_registry` mints
/// is a real thing-kind too. `key` and `cave-mouth` had no anchor-kind
/// variant behind them at all (that was the point of keying on thing-kind — a
/// cave mouth is a `Vertex`, a key is carried), and a typo in either row would
/// produce a property nothing carries while
/// `each_property_is_carried_by_at_least_one_thing_kind` stayed green on the
/// misspelt row.
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

/// Anti-vacuity for the test above: it sweeps a non-empty population, and
/// the roster it checks against is not so permissive that any string passes.
/// Without this, deleting every row of `object_registry` — or replacing
/// `THING_KINDS` with a list that happened to contain everything — would read
/// as a green result.
#[test]
fn the_correspondence_checks_are_not_vacuous() {
    // The anchor-kind arm of this control went with its test (see the module
    // doc). What is left is the half that guards the check below: a registry
    // stripped of rows, and a roster permissive enough to accept anything.
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
/// **The fourth spelling of `cave-mouth`, and the agreement its own doc
/// claims (fix round 1, m3).** `hornvale_vessel::passage::CAVE_MOUTH` says it
/// is "the label `THING_KINDS` carries and `object_registry` gives `Openable`
/// and `AffordsPassage`" — three tables, three separate `"cave-mouth"`
/// literals, and until this test nothing held any two of them together. The
/// const's NARROW claim was guarded (the lineage role and the `instance-of`
/// object are one spelling because both read the const); the CROSS-TABLE
/// claim in the same sentence was not.
///
/// **Here rather than in `windows/vessel` for a reason that has lapsed**:
/// the assertion needs `hornvale_thing::THING_KINDS`, and `windows/vessel`
/// carried no `hornvale-thing` dependency until The Wicket's Task 2. Task 3
/// moves it.
/// A cave mouth is also the second row (with `key`) that no room's grammar
/// ever composes, so nothing reaches it from the anchor side either.
///
/// MUTATION THIS MUST FAIL AGAINST: rename ONE side of the agreement — spell
/// `passage::CAVE_MOUTH` `"cave_mouth"`, leaving `THING_KINDS` and
/// `object_registry` on the hyphen. Neither test above objects (both read the
/// registry's own key, which did not move). Red observed 2026-08-29:
///
/// ```text
/// thread 'anchor_thing_correspondence::the_cave_mouth_kind_is_one_label_in_every_table_that_names_it'
/// panicked at cli/tests/suite/anchor_thing_correspondence.rs:
/// passage::CAVE_MOUTH is "cave_mouth", which hornvale_thing::THING_KINDS does
/// not carry — the const's doc claims it is the label that roster carries
/// ```
#[test]
fn the_cave_mouth_kind_is_one_label_in_every_table_that_names_it() {
    use hornvale_kernel::KindId;
    use hornvale_vessel::affordance::ObjectProperty;
    use hornvale_vessel::passage::CAVE_MOUTH;

    assert!(
        hornvale_thing::THING_KINDS.contains(&CAVE_MOUTH),
        "passage::CAVE_MOUTH is {CAVE_MOUTH:?}, which hornvale_thing::THING_KINDS \
         does not carry — the const's doc claims it is the label that roster \
         carries"
    );

    let registry = object_registry();
    let traits = registry.get(&KindId(CAVE_MOUTH)).unwrap_or_else(|| {
        panic!(
            "object_registry carries no row for {CAVE_MOUTH:?} — the const's doc \
             claims that table gives it Openable and AffordsPassage"
        )
    });
    for wanted in [ObjectProperty::Openable, ObjectProperty::AffordsPassage] {
        assert!(
            traits.properties.contains(&wanted),
            "object_registry's {CAVE_MOUTH:?} row lacks {wanted:?}, which \
             passage::CAVE_MOUTH's doc says it gives"
        );
    }
}
