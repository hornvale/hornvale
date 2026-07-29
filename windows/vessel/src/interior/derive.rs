//! `interior_of` — a real room to a real `Interior` (The Threshold, spec §3).
//!
//! The Hearth builds an interior nobody can reach; this is the path from a
//! `RoomAddr` to a composed pattern set. It takes NO era parameter: the only
//! `Era` in this codebase is stratigraphic (rock bands), and a room's
//! furnishing must not flicker with the seasons anyway, so `cold` is read at a
//! canonical day and the result is a pure function of the room.
//!
//! Nothing here is serialized (decision 0069). The interior is derived per
//! room, bubble-scoped, and discarded with the bubble.

use super::anchor::Interior;
use super::pattern::{compose, selection};
use crate::liveness::Terrain;
use hornvale_kernel::RoomAddr;

/// The interior of `room`: which patterns it draws, composed into an anchor
/// graph. `built` is "is anyone's territory this" and `cold` is "does warmth
/// matter here" — both read from `terrain`, both stable.
pub fn interior_of(room: &RoomAddr, terrain: &dyn Terrain) -> Interior {
    let built = terrain.is_built(room);
    let cold = terrain.is_cold(room);
    // `selection` takes no seed: The Hearth's revised T4 dropped it, since v1's
    // draw is a pure admissibility filter. When a variation draw lands it must
    // key on pattern NAME, never on position.
    compose(&selection(built, cold))
}

/// The interior of a CHAMBER — a place below the walk band.
///
/// Two departures from [`interior_of`], and only one of them is new:
///
/// 1. Every terrain read is taken at the chamber's **walk-band ancestor**. That
///    is not a convenience: `LocaleTerrain` answers `is_built` from a
///    settlement-territory set keyed on walk-band room ids, so a raw chamber
///    address reads as unbuilt and a dwelling would furnish itself with
///    wilderness patterns.
/// 2. The draw is **role-gated** (The Blocking): a chamber is FOR something, so it
///    draws `selection_for` rather than `selection`. This is what stopped the
///    chambers of one structure being identical — under The Lintel every chamber
///    of a structure composed the same interior, and four doors onto one room was
///    a headline that was literally true and experientially thin.
///
/// `interior_of` is deliberately left untouched: its output for every walk-band
/// address is a committed-history input (a creature's thermal drive reads the
/// warmth it implies), so it must stay bit-for-bit what The Threshold shipped.
/// That is why the role layer lives HERE and why `selection` gained no parameter.
///
/// `brief` and `chamber_index` are what the role is derived from
/// ([`crate::interior::pattern::role_for`]) — the brief for what the place's
/// business is, the index for how far in the chamber is. Both are already in
/// every caller's hand.
/// type-audit: bare-ok(count: walk_depth), bare-ok(index: chamber_index)
pub fn chamber_interior_of(
    chamber: &RoomAddr,
    terrain: &dyn Terrain,
    walk_depth: u32,
    brief: &crate::brief::Brief,
    chamber_index: usize,
) -> Interior {
    let locale = crate::band::truncate_to_walk(chamber, walk_depth);
    let built = terrain.is_built(&locale);
    let cold = terrain.is_cold(&locale);
    // The brief's own flags are read at the walk band too (`brief_of` truncates
    // identically), so these must agree. Stated as a guard rather than trusted:
    // two independent readings of "is this built" is exactly the drift that made
    // the walk-band truncation necessary in the first place.
    debug_assert_eq!(brief.built, built, "the brief disagrees about `built`");
    debug_assert_eq!(brief.cold, cold, "the brief disagrees about `cold`");
    let role = super::pattern::role_for(chamber_index, brief);
    compose(&super::pattern::selection_for(
        role,
        built,
        cold,
        brief.is_populous(),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interior::anchor::AnchorKind;
    use hornvale_kernel::WorldTime;

    /// A `Terrain` that answers only what derivation reads.
    struct Stub {
        built: bool,
        cold: bool,
    }
    impl Terrain for Stub {
        fn elevation(&self, _r: &RoomAddr) -> f64 {
            0.0
        }
        fn is_fresh_water(&self, _r: &RoomAddr) -> bool {
            false
        }
        fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
            if self.cold { -20.0 } else { 25.0 }
        }
        fn is_built(&self, _r: &RoomAddr) -> bool {
            self.built
        }
    }

    /// An arbitrary room. `RoomAddr` carries no `Default` (both fields are
    /// public; the depth-0 base face is the natural stand-in) — derivation
    /// reads nothing about WHICH room beyond what `terrain` reports.
    fn room() -> RoomAddr {
        RoomAddr {
            face: 0,
            path: Vec::new(),
        }
    }

    #[test]
    fn a_built_cold_room_draws_a_hearth() {
        let i = interior_of(
            &room(),
            &Stub {
                built: true,
                cold: true,
            },
        );
        assert!(
            i.ids()
                .iter()
                .any(|&a| i.anchor(a).kind == AnchorKind::Hearth)
        );
    }

    #[test]
    fn a_wilderness_room_draws_no_built_anchors() {
        let i = interior_of(
            &room(),
            &Stub {
                built: false,
                cold: true,
            },
        );
        assert!(!i.ids().is_empty(), "wilderness gets an interior too");
        assert!(
            !i.ids()
                .iter()
                .any(|&a| i.anchor(a).kind == AnchorKind::Hearth)
        );
        assert!(
            !i.ids()
                .iter()
                .any(|&a| i.anchor(a).kind == AnchorKind::Threshold)
        );
    }

    #[test]
    fn every_derived_interior_is_well_formed() {
        for &built in &[true, false] {
            for &cold in &[true, false] {
                let i = interior_of(&room(), &Stub { built, cold });
                assert!(
                    crate::interior::permits(&i),
                    "derivation must never produce an interior the validator rejects"
                );
            }
        }
    }

    #[test]
    fn derivation_is_a_pure_function_of_the_room() {
        // Called twice, identical — nothing time-varying leaks in, so a
        // furnishing cannot flicker with the seasons.
        let a = interior_of(
            &room(),
            &Stub {
                built: true,
                cold: true,
            },
        );
        let b = interior_of(
            &room(),
            &Stub {
                built: true,
                cold: true,
            },
        );
        assert_eq!(a.ids().len(), b.ids().len());
        for (x, y) in a.ids().iter().zip(b.ids().iter()) {
            assert_eq!(a.anchor(*x).kind, b.anchor(*y).kind);
        }
    }

    #[test]
    fn every_derived_interior_has_a_landing() {
        use crate::interior::seam::{landing, seam_kind};
        for &built in &[true, false] {
            let i = interior_of(&room(), &Stub { built, cold: true });
            assert!(
                landing(&i, seam_kind(built)).is_some(),
                "a creature must always have somewhere to arrive"
            );
        }
    }

    /// A `Terrain` whose built-set is keyed at the WALK band, exactly as
    /// `LocaleTerrain` is (`liveness.rs`: built iff the packed room id is in
    /// the injected settlement-territory set).
    struct WalkKeyedTerrain {
        built_walk_ids: std::collections::BTreeSet<u64>,
    }
    impl Terrain for WalkKeyedTerrain {
        fn elevation(&self, _r: &RoomAddr) -> f64 {
            0.0
        }
        fn is_fresh_water(&self, _r: &RoomAddr) -> bool {
            false
        }
        fn temperature(&self, _r: &RoomAddr, _d: WorldTime) -> f64 {
            -20.0
        }
        fn is_built(&self, r: &RoomAddr) -> bool {
            r.pack()
                .ok()
                .is_some_and(|id| self.built_walk_ids.contains(&id.0))
        }
    }

    const WALK: u32 = 12;

    fn walk_addr() -> RoomAddr {
        RoomAddr {
            face: 3,
            path: (0..WALK).map(|i| (i % 4) as u8).collect(),
        }
    }

    fn chamber_addr() -> RoomAddr {
        let mut path: Vec<u8> = walk_addr().path;
        path.extend((0..crate::band::CHAMBER_DEPTH_OFFSET).map(|i| (i % 4) as u8));
        RoomAddr { face: 3, path }
    }

    /// A brief matching [`WalkKeyedTerrain`]'s reads (which are always cold), so
    /// `chamber_interior_of`'s debug assertions hold.
    fn brief(built: bool) -> crate::brief::Brief {
        crate::brief::Brief::from_parts(None, None, None, None, 0, built, true)
    }

    #[test]
    fn a_chamber_in_a_built_locale_draws_built_patterns() {
        // THE FOOTGUN: the built-set holds the LOCALE's id, never the
        // chamber's, so a raw read would furnish a dwelling as wilderness.
        //
        // Asserted at chamber INDEX 1 since Task 6, because the hearth is the
        // hearthroom's — index 0 is the threshold chamber and a screened doorway
        // is not evidence of a built read the way a fire is.
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        assert!(
            !terrain.is_built(&chamber_addr()),
            "precondition: a raw chamber read is UNBUILT — this is the footgun"
        );
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK, &brief(true), 1);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            kinds.contains(&AnchorKind::Hearth),
            "a built-cold hearthroom draws a hearth, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_in_an_unbuilt_locale_draws_wild_patterns() {
        let terrain = WalkKeyedTerrain {
            built_walk_ids: std::collections::BTreeSet::new(),
        };
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK, &brief(false), 1);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            !kinds.contains(&AnchorKind::Bed),
            "an unbuilt place has no bed, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_no_longer_composes_exactly_as_its_locale_does() {
        // THE TEST THAT WAS ALWAYS GOING TO CHANGE, changed. It used to assert
        // the two bands compose IDENTICALLY, and said in its own comment that a
        // campaign giving a band its own vocabulary is the one that must rewrite
        // it, deliberately. This is that campaign.
        //
        // What replaces it is the two halves of the claim:
        //
        //   1. the CHAMBER band composes something the locale band does not
        //      (differentiation happened at all), and
        //   2. the LOCALE band is byte-identical to what The Threshold shipped
        //      (the epoch was not taken by accident) — which is `selection`'s
        //      own frozen output, asserted in
        //      `pattern::tests::the_locale_band_draws_exactly_what_it_drew`.
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        let locale = interior_of(&walk_addr(), &terrain);
        let threshold = chamber_interior_of(&chamber_addr(), &terrain, WALK, &brief(true), 0);
        let hearthroom = chamber_interior_of(&chamber_addr(), &terrain, WALK, &brief(true), 1);
        assert_ne!(
            threshold, locale,
            "a chamber is FOR something and a locale is not; they must not compose alike"
        );
        assert_ne!(
            threshold, hearthroom,
            "two chambers of one structure still compose identically"
        );
    }

    #[test]
    fn every_role_at_every_chamber_index_is_well_formed_and_landable() {
        // The validator's rule and the seam's, swept over every index a
        // structure can have — because `chamber_interior_of` is the ONLY
        // composer the session calls, and a role whose composition the validator
        // rejects would strand a possession in an unwalkable room.
        use crate::interior::seam::{landing, seam_kind};
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        for index in 0..crate::structure::MAX_CHAMBERS {
            let i = chamber_interior_of(&chamber_addr(), &terrain, WALK, &brief(true), index);
            assert!(
                crate::interior::permits(&i),
                "chamber {index}'s role composes an interior the validator rejects"
            );
            assert!(
                landing(&i, seam_kind(true)).is_some(),
                "chamber {index}'s role leaves a possession nowhere to arrive"
            );
        }
    }
}
