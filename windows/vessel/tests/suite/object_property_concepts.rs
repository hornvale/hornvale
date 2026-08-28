//! Keeps `ObjectProperty`'s five variants and `domains/language`'s
//! `object_property_pack` concepts in step across the kernel -> domains ->
//! windows layering (The Offer, Task 9), modelled directly on The
//! Confidant's epoch 12 (`felt_state_concepts.rs`) — the same problem, the
//! same constraint.
//!
//! `ObjectProperty` lives here, in a window (`windows/vessel/src/
//! affordance.rs`); `object_property_pack` lives in `domains/language`, a
//! domain. A domain depends on the kernel and nothing else
//! (`domains/CLAUDE.md`'s one rule), so `hornvale_language` cannot import
//! `ObjectProperty` to derive its concept roster from the enum, and that
//! import could never run the other way either without creating a
//! window -> domain -> window cycle. This window already depends on
//! `hornvale_language` (see `Cargo.toml`), so the check runs from here
//! instead: one test asserting the two rosters name the same five
//! properties, rather than an import running the wrong way across the
//! layering.
//!
//! Unlike `felt_state_concepts.rs`, `ObjectProperty` already carries its
//! own `all()` and `concept_name()` (`affordance.rs`'s exhaustive-match
//! tripwire), so this test reads those directly rather than hand-rolling a
//! parallel `concept_name` function and an `ALL_*` const the way the felt-
//! state precedent had to.

use hornvale_language::object_property_pack;
use hornvale_vessel::affordance::ObjectProperty;
use std::collections::BTreeSet;

#[test]
fn every_object_property_has_exactly_one_registered_concept() {
    let pack_names: BTreeSet<&str> = object_property_pack().iter().map(|(n, _)| *n).collect();
    assert_eq!(
        pack_names.len(),
        object_property_pack().len(),
        "object_property_pack lists a concept name twice"
    );

    let prop_names: BTreeSet<&str> = ObjectProperty::all()
        .into_iter()
        .map(|p| p.concept_name())
        .collect();
    assert_eq!(
        prop_names.len(),
        ObjectProperty::all().len(),
        "two ObjectProperty variants map to the same concept name"
    );

    assert_eq!(
        pack_names, prop_names,
        "domains/language::object_property_pack and windows/vessel::ObjectProperty \
         have fallen out of step: a variant with no registered concept, or a \
         registered concept with no ObjectProperty variant behind it"
    );
}
