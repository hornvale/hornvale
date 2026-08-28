//! The Chattel, Task 2 review finding: `domains/thing`'s module doc claims
//! every `hornvale_vessel::interior::AnchorKind` variant has a counterpart in
//! `hornvale_thing::THING_KINDS`, so Task 7's `AnchorKind` → `KindId` mapping
//! can be total. Nothing held that claim — a domain may not depend on a
//! window (`domains/thing` cannot see `AnchorKind`, and `windows/vessel`
//! carries no dependency on `hornvale-thing` yet), so `cli/` is the only
//! crate in the workspace that already depends on both. Same shape as
//! `cave_kind_correspondence.rs`'s `CaveKind`/`Formation` guard: the mapping
//! below is exhaustive over `AnchorKind` with **no wildcard arm**, so an
//! appended variant (the enum's own doc calls appending routine) fails to
//! *compile* here rather than leaving Task 7 with an untargeted anchor kind
//! that nothing reddens.

use hornvale_vessel::interior::AnchorKind;

/// The thing-kind label Task 2 authored for each anchor kind (see
/// `domains/thing/src/lib.rs`'s `thing_registry`). Exhaustive by design: a
/// new `AnchorKind` variant must get a row here before this crate builds.
fn thing_label_for(kind: AnchorKind) -> &'static str {
    match kind {
        AnchorKind::Hearth => "hearth",
        AnchorKind::Threshold => "threshold",
        AnchorKind::Bed => "bed",
        AnchorKind::Vessel => "vessel",
        AnchorKind::Screen => "screen",
        AnchorKind::Pool => "pool",
        AnchorKind::Log => "log",
        AnchorKind::Ground => "ground",
        AnchorKind::Alcove => "alcove",
        AnchorKind::Strongbox => "strongbox",
        AnchorKind::HighSeat => "high-seat",
        AnchorKind::Loom => "loom",
        AnchorKind::Anvil => "anvil",
        AnchorKind::Altar => "altar",
    }
}

/// Every anchor kind's label (via [`thing_label_for`], the exhaustive
/// compile-time guard) is a row `hornvale_thing::THING_KINDS` actually
/// carries — the runtime half of the guard, for the label text itself.
#[test]
fn every_anchor_kind_has_a_thing_kind_counterpart() {
    for (kind, label) in [
        (AnchorKind::Hearth, thing_label_for(AnchorKind::Hearth)),
        (
            AnchorKind::Threshold,
            thing_label_for(AnchorKind::Threshold),
        ),
        (AnchorKind::Bed, thing_label_for(AnchorKind::Bed)),
        (AnchorKind::Vessel, thing_label_for(AnchorKind::Vessel)),
        (AnchorKind::Screen, thing_label_for(AnchorKind::Screen)),
        (AnchorKind::Pool, thing_label_for(AnchorKind::Pool)),
        (AnchorKind::Log, thing_label_for(AnchorKind::Log)),
        (AnchorKind::Ground, thing_label_for(AnchorKind::Ground)),
        (AnchorKind::Alcove, thing_label_for(AnchorKind::Alcove)),
        (
            AnchorKind::Strongbox,
            thing_label_for(AnchorKind::Strongbox),
        ),
        (AnchorKind::HighSeat, thing_label_for(AnchorKind::HighSeat)),
        (AnchorKind::Loom, thing_label_for(AnchorKind::Loom)),
        (AnchorKind::Anvil, thing_label_for(AnchorKind::Anvil)),
        (AnchorKind::Altar, thing_label_for(AnchorKind::Altar)),
    ] {
        assert!(
            hornvale_thing::THING_KINDS.contains(&label),
            "{kind:?} maps to thing-kind label {label:?}, which THING_KINDS does not carry"
        );
    }
}
