//! Seams — the join between the two position scales (The Threshold, spec §4.2).
//!
//! A `Threshold` anchor is one SPECIES of a wider thing: the seam between
//! scales, simultaneously a room-graph edge and an anchor. A doorway, a ford
//! and an open field edge are all seams; only the first is a threshold. Built
//! and natural rooms are mirror images — indoors the default is a chokepoint
//! and the wall is impassable; outdoors the whole border is passable and the
//! chokepoint is the exception:
//!
//! ```text
//!               NARROW (chokepoint)         BROAD (whole edge)
//!   BUILT       doorway, gate  <- common    colonnade  <- rare
//!   NATURAL     ford, col      <- exception open edge  <- THE COMMON CASE
//! ```
//!
//! A seam belongs to the room-graph EDGE, not to a room's interior — which is
//! why this module is separate from `anchor`. A broad seam lands at the
//! interior's hub because that is the only topologically available answer:
//! without coordinates there is no "nearest anchor to the north edge", and The
//! Hearth's §2.1 forbids reaching for one (outcomes read topology, never
//! metrics). The forced answer being the metric-free one is a good sign.

use super::anchor::{AnchorId, AnchorKind, Interior};

/// Whether passage between two rooms is a chokepoint or the whole shared edge.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SeamKind {
    /// A chokepoint — a doorway, a ford, a gap in a cliff.
    Narrow,
    /// The entire shared border is passable — meadow to meadow.
    Broad,
}

/// The seam kind for a room, derived from whether it is built. Built rooms
/// default to a chokepoint; unbuilt land defaults to an open edge.
/// type-audit: bare-ok(flag: built)
pub fn seam_kind(built: bool) -> SeamKind {
    if built {
        SeamKind::Narrow
    } else {
        SeamKind::Broad
    }
}

/// Which anchor an arriving creature stands at. A narrow seam lands at the
/// interior's `Threshold` if it has one; everything else lands at the **hub**.
///
/// The hub is the first `Ground` anchor, which is how `compose` defines it —
/// NOT the first anchor by index. Those coincide today only because `Ground`
/// happens to lead `INVENTORY` for both the built and wild filters, and
/// depending on that coincidence would be identity-by-position, the same bug
/// class this campaign has now found at two other scales (`AnchorId` as a
/// vector offset; a seeded pattern draw keyed by index). Falls back to the
/// first anchor for an interior with no `Ground` at all, and `None` only for
/// an empty one.
pub fn landing(interior: &Interior, kind: SeamKind) -> Option<AnchorId> {
    let ids = interior.ids();
    if kind == SeamKind::Narrow
        && let Some(&t) = ids
            .iter()
            .find(|&&a| interior.anchor(a).kind == AnchorKind::Threshold)
    {
        return Some(t);
    }
    ids.iter()
        .find(|&&a| interior.anchor(a).kind == AnchorKind::Ground)
        .copied()
        .or_else(|| ids.first().copied())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::anchor::{AnchorKind, Interior};

    /// A built interior: a threshold and a hearth, hub-composed.
    fn built() -> Interior {
        let mut i = Interior::new();
        let t = i.push(AnchorKind::Threshold, None);
        let h = i.push(AnchorKind::Hearth, None);
        i.connect(t, h);
        i
    }

    /// A wilderness interior, shaped as The Hearth's revised T4 composes it:
    /// `the-clearing` (a `Ground` hub) with `the-pool` beside it. No threshold
    /// anywhere, which is legitimate (spec §4.2) rather than a gap to be
    /// patched with a fake doorway.
    fn wild() -> Interior {
        let mut i = Interior::new();
        let g = i.push(AnchorKind::Ground, None);
        let p = i.push(AnchorKind::Pool, None);
        i.connect(g, p);
        i
    }

    /// An interior with no `Ground` at all — the fallback path.
    fn groundless() -> Interior {
        let mut i = Interior::new();
        let p = i.push(AnchorKind::Pool, None);
        let l = i.push(AnchorKind::Log, None);
        i.connect(p, l);
        i
    }

    #[test]
    fn a_narrow_seam_lands_at_the_threshold() {
        let i = built();
        let at = landing(&i, SeamKind::Narrow).expect("a built interior has a landing");
        assert_eq!(i.anchor(at).kind, AnchorKind::Threshold);
    }

    #[test]
    fn a_broad_seam_lands_at_the_ground_hub() {
        // Without coordinates there is no "nearest anchor to the north edge",
        // and spec §2.1 of The Hearth forbids reaching for one, so the hub is
        // the only available answer.
        let i = wild();
        let at = landing(&i, SeamKind::Broad).expect("a wilderness interior has a landing");
        assert_eq!(i.anchor(at).kind, AnchorKind::Ground);
    }

    #[test]
    fn the_hub_is_found_by_kind_not_by_index() {
        // Ground leads INVENTORY today, so hub and ids()[0] coincide. Build an
        // interior where they do NOT, and assert we followed the kind.
        let mut i = Interior::new();
        let p = i.push(AnchorKind::Pool, None);
        let g = i.push(AnchorKind::Ground, None);
        i.connect(p, g);
        assert_eq!(landing(&i, SeamKind::Broad), Some(g));
        assert_ne!(landing(&i, SeamKind::Broad), Some(i.ids()[0]));
    }

    #[test]
    fn an_interior_with_no_ground_falls_back_to_the_first_anchor() {
        let i = groundless();
        assert_eq!(landing(&i, SeamKind::Broad), Some(i.ids()[0]));
    }

    #[test]
    fn wilderness_has_no_threshold_and_that_is_fine() {
        // `wild()` composes only `Pool`/`Log` (see its own definition above) —
        // that it carries no `Threshold` is a property of the FIXTURE, not
        // something `landing` computes, so it is not asserted here. What this
        // test actually exercises is that `landing` still succeeds without one.
        let i = wild();
        assert!(
            landing(&i, SeamKind::Broad).is_some(),
            "it still has somewhere to arrive"
        );
    }

    #[test]
    fn a_narrow_seam_into_an_interior_with_no_threshold_falls_back_to_the_hub() {
        // Robustness: a built room whose selection happened to draw no
        // threshold must still be enterable.
        let i = wild();
        assert_eq!(landing(&i, SeamKind::Narrow), Some(i.ids()[0]));
    }

    #[test]
    fn an_empty_interior_has_no_landing() {
        assert_eq!(landing(&Interior::new(), SeamKind::Broad), None);
    }

    #[test]
    fn built_rooms_are_narrow_and_wilderness_is_broad() {
        assert_eq!(seam_kind(true), SeamKind::Narrow);
        assert_eq!(seam_kind(false), SeamKind::Broad);
    }
}
