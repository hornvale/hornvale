//! The band notation (Rose Window metaplan §1b.3) in code. A place's BAND is a
//! function of its address depth: the walk band is the ~1.7 km locale a body
//! commits to, the chamber band is the ~3.3 m place inside a structure.
//!
//! An address below the walk band is IDENTITY, NOT SHAPE (§1b.3 law 3): its
//! triangle geometry means nothing, and connectivity comes from the structure's
//! own graph. What the depth *is* used for is deciding which band's rules apply,
//! and truncating back to the walk band when a walk-band-keyed datum is read.

use hornvale_kernel::RoomAddr;

/// How many refinements below the walk band a chamber sits. Nine halvings of a
/// ~1.7 km locale edge is ≈3.3 m — a human-scale room. Declared as a constant
/// because it is a shape of the world, not a tuning knob: changing it changes
/// which addresses are chambers.
/// type-audit: bare-ok(count)
pub const CHAMBER_DEPTH_OFFSET: u32 = 9;

/// Which band an address belongs to. Deliberately only two variants: the
/// STRUCTURE band of metaplan §1b.3 has no code yet (The Precincts), and
/// inventing a variant nothing constructs would be a lie in the type.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Band {
    /// The ~1.7 km locale a body commits to — the walk band.
    Walk,
    /// A human-scale place inside a structure, ≈3.3 m.
    Chamber,
}

/// The address depth chambers live at, given the world's walk depth.
/// type-audit: bare-ok(count: walk_depth), bare-ok(count: return)
pub fn chamber_depth(walk_depth: u32) -> u32 {
    walk_depth + CHAMBER_DEPTH_OFFSET
}

/// Which band `addr` is in. Anything deeper than the walk band is a chamber;
/// anything at or above it is walk-band (this campaign ships no coarser band).
/// type-audit: bare-ok(count: walk_depth)
pub fn band_of(addr: &RoomAddr, walk_depth: u32) -> Band {
    if addr.depth() > walk_depth {
        Band::Chamber
    } else {
        Band::Walk
    }
}

/// The walk-band ancestor of `addr` — 0077's path truncation, used DOWNWARD.
///
/// Every walk-band-keyed datum (the settlement-territory set, the locale
/// describer, the climate read) must be consulted with this, never with a raw
/// chamber address. An address at or above the walk band is returned unchanged,
/// so callers may apply this unconditionally.
/// type-audit: bare-ok(count: walk_depth)
pub fn truncate_to_walk(addr: &RoomAddr, walk_depth: u32) -> RoomAddr {
    if addr.depth() <= walk_depth {
        return addr.clone();
    }
    RoomAddr {
        face: addr.face,
        path: addr.path[..walk_depth as usize].to_vec(),
    }
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
    fn a_walk_depth_address_is_the_walk_band() {
        assert_eq!(band_of(&addr(WALK), WALK), Band::Walk);
    }

    #[test]
    fn a_deeper_address_is_the_chamber_band() {
        assert_eq!(band_of(&addr(chamber_depth(WALK)), WALK), Band::Chamber);
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
