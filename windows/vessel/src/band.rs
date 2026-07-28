//! The band notation (Rose Window metaplan §1b.3) in code. A place's BAND is a
//! function of its address depth: the walk band is the ~1.7 km locale a body
//! commits to, the chamber band is the ~3.3 m place inside a structure.
//!
//! An address below the walk band is IDENTITY, NOT SHAPE (§1b.3 law 3): its
//! triangle geometry means nothing, and connectivity comes from the structure's
//! own graph.
//!
//! What this module holds is therefore only ARITHMETIC on depths: where chambers
//! sit ([`chamber_depth`]) and how to get back up to the walk band
//! ([`truncate_to_walk`]) before reading any walk-band-keyed datum. It
//! deliberately holds no `Band` enum and no `band_of(addr)` classifier: the
//! question the session actually asks is "am I inside a structure?", which is
//! session state (`Session::inside`), not a property an address can answer. An
//! address at chamber depth is a chamber only because a structure put one there.

use hornvale_kernel::RoomAddr;

/// How many refinements below the walk band a chamber sits. Nine halvings of a
/// ~1.7 km locale edge is ≈3.3 m — a human-scale room. Declared as a constant
/// because it is a shape of the world, not a tuning knob: changing it changes
/// which addresses are chambers.
/// type-audit: bare-ok(count)
pub const CHAMBER_DEPTH_OFFSET: u32 = 9;

/// The address depth chambers live at, given the world's walk depth.
/// type-audit: bare-ok(count: walk_depth), bare-ok(count: return)
pub fn chamber_depth(walk_depth: u32) -> u32 {
    walk_depth + CHAMBER_DEPTH_OFFSET
}

/// The walk-band ancestor of `addr` — 0077's path truncation, used DOWNWARD.
///
/// Every walk-band-keyed datum (the settlement-territory set, the locale
/// describer, the climate read) must be consulted with this, never with a raw
/// chamber address. An address at or above the walk band is returned unchanged,
/// so callers may apply this unconditionally — which is the whole point, and the
/// only thing this adds over the kernel primitive it delegates to.
/// type-audit: bare-ok(count: walk_depth)
pub fn truncate_to_walk(addr: &RoomAddr, walk_depth: u32) -> RoomAddr {
    // `RoomAddr::ancestor` (kernel/src/room.rs) already does the bounds-checked
    // slice and returns `None` when `walk_depth` is deeper than the address.
    // Delegate: re-deriving the slice here would duplicate a save-format-
    // adjacent primitive, and a second copy is a second thing to get wrong.
    addr.ancestor(walk_depth).unwrap_or_else(|| addr.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::RoomAddr;

    /// The walk depth on the canonical globe (`GLOBE_LEVEL` 6 + 6).
    const WALK: u32 = 12;

    fn addr(depth: u32) -> RoomAddr {
        RoomAddr {
            face: 3,
            // a fixed, arbitrary child sequence: 0,1,2,3,0,1,2,3,...
            path: (0..depth).map(|i| (i % 4) as u8).collect(),
        }
    }

    #[test]
    fn truncation_is_the_identity_at_the_walk_band() {
        let a = addr(WALK);
        assert_eq!(truncate_to_walk(&a, WALK), a);
    }

    #[test]
    fn truncation_yields_the_walk_band_ancestor_of_a_chamber() {
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_eq!(walk.depth(), WALK);
        assert_eq!(walk.path[..], chamber.path[..WALK as usize]);
        assert_eq!(walk.face, chamber.face);
    }

    #[test]
    fn a_chamber_and_its_ancestor_pack_to_different_ids() {
        // The footgun Task 2 defends against: a chamber id is NOT its
        // locale's id, so any walk-band-keyed set must be consulted with
        // the TRUNCATED address.
        let chamber = addr(chamber_depth(WALK));
        let walk = truncate_to_walk(&chamber, WALK);
        assert_ne!(chamber.pack().unwrap().0, walk.pack().unwrap().0);
    }

    #[test]
    fn the_chamber_depth_fits_the_packing_cap() {
        assert!(chamber_depth(WALK) as usize <= hornvale_kernel::MAX_DEPTH);
        assert!(addr(chamber_depth(WALK)).pack().is_ok());
    }

    #[test]
    fn an_address_shallower_than_the_walk_band_truncates_to_itself() {
        // Coarser than the walk band: there is nothing to truncate, and this
        // must not panic on a slice out of range.
        let coarse = addr(8);
        assert_eq!(truncate_to_walk(&coarse, WALK), coarse);
    }
}
