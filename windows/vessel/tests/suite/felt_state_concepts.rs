//! Keeps `AffectLabel`'s six variants and `domains/language`'s
//! `felt_state_pack` concepts in step across the kernel -> domains ->
//! windows layering (The Confidant, Task 3).
//!
//! `AffectLabel` lives here, in a window; `felt_state_pack` lives in
//! `domains/language`, a domain. A domain depends on the kernel and nothing
//! else (`domains/CLAUDE.md`'s one rule), so `hornvale_language` cannot
//! import `AffectLabel` to derive its concept roster from the enum, and
//! that import could never run the other way either without creating a
//! window -> domain -> window cycle. This window already depends on
//! `hornvale_language` (see `Cargo.toml`), so the check runs from here
//! instead: one test asserting the two rosters name the same six felt
//! states, rather than an import running the wrong way across the layering.

use hornvale_language::felt_state_pack;
use hornvale_vessel::liveness::AffectLabel;
use std::collections::BTreeSet;

/// `AffectLabel`'s registry name -- the lowercase-word convention every
/// other concept in `EPOCH_COHORTS` already uses (`frustrated`, not
/// `Frustrated`). Exhaustive by construction: a future `AffectLabel`
/// variant fails this match at compile time until named here, which is the
/// half of "kept in step" a runtime check alone could not force.
fn concept_name(label: AffectLabel) -> &'static str {
    match label {
        AffectLabel::Content => "content",
        AffectLabel::Eager => "eager",
        AffectLabel::Searching => "searching",
        AffectLabel::Frustrated => "frustrated",
        AffectLabel::Lost => "lost",
        AffectLabel::Helpless => "helpless",
    }
}

/// Every `AffectLabel` variant, in declaration order -- the reverse-audit
/// roster this test's set comparison walks, the same shape epoch 7's
/// compass audit uses over `Compass::all()`.
const ALL_LABELS: [AffectLabel; 6] = [
    AffectLabel::Content,
    AffectLabel::Eager,
    AffectLabel::Searching,
    AffectLabel::Frustrated,
    AffectLabel::Lost,
    AffectLabel::Helpless,
];

#[test]
fn every_affect_label_has_exactly_one_registered_felt_state_concept() {
    let pack_names: BTreeSet<&str> = felt_state_pack().iter().map(|(name, _)| *name).collect();
    assert_eq!(
        pack_names.len(),
        felt_state_pack().len(),
        "felt_state_pack lists a concept name twice"
    );

    let label_names: BTreeSet<&str> = ALL_LABELS.iter().copied().map(concept_name).collect();
    assert_eq!(
        label_names.len(),
        ALL_LABELS.len(),
        "two AffectLabel variants map to the same concept name"
    );

    assert_eq!(
        pack_names, label_names,
        "domains/language::felt_state_pack and windows/vessel::AffectLabel \
         have fallen out of step: a variant with no registered concept, or a \
         registered concept with no AffectLabel variant behind it"
    );
}
