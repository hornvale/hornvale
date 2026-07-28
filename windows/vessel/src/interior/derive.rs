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
/// Identical to [`interior_of`] except that every terrain read is taken at the
/// chamber's **walk-band ancestor**. That is not a convenience: `LocaleTerrain`
/// answers `is_built` from a settlement-territory set keyed on walk-band room
/// ids, so a raw chamber address reads as unbuilt and a dwelling would furnish
/// itself with wilderness patterns.
///
/// `interior_of` is deliberately left untouched: its output for every walk-band
/// address is a committed-history input (a creature's thermal drive reads the
/// warmth it implies), so it must stay bit-for-bit what The Threshold shipped.
/// type-audit: bare-ok(count: walk_depth)
pub fn chamber_interior_of(chamber: &RoomAddr, terrain: &dyn Terrain, walk_depth: u32) -> Interior {
    let locale = crate::band::truncate_to_walk(chamber, walk_depth);
    interior_of(&locale, terrain)
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

    #[test]
    fn a_chamber_in_a_built_locale_draws_built_patterns() {
        // THE FOOTGUN: the built-set holds the LOCALE's id, never the
        // chamber's, so a raw read would furnish a dwelling as wilderness.
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        assert!(
            !terrain.is_built(&chamber_addr()),
            "precondition: a raw chamber read is UNBUILT — this is the footgun"
        );
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            kinds.contains(&AnchorKind::Hearth),
            "a built-cold chamber draws a hearth, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_in_an_unbuilt_locale_draws_wild_patterns() {
        let terrain = WalkKeyedTerrain {
            built_walk_ids: std::collections::BTreeSet::new(),
        };
        let i = chamber_interior_of(&chamber_addr(), &terrain, WALK);
        let kinds: Vec<AnchorKind> = i.ids().iter().map(|&id| i.anchor(id).kind).collect();
        assert!(
            !kinds.contains(&AnchorKind::Bed),
            "an unbuilt place has no bed, got {kinds:?}"
        );
    }

    #[test]
    fn a_chamber_composes_exactly_as_its_locale_does() {
        // THIS TEST IS SPEC §3's ADMISSIBILITY TABLE, asserted. The table's
        // content in v1 is "every kind is admissible at both bands", so the
        // observable claim is exactly that the two bands compose identically.
        // When a later campaign gives a band its own vocabulary, this test is
        // the one that must change, deliberately and with an epoch argument.
        //
        // The composer is shared and FROZEN: the chamber's interior is the
        // same graph the locale would draw, so this campaign moves no
        // behaviour (spec §2).
        let terrain = WalkKeyedTerrain {
            built_walk_ids: [walk_addr().pack().unwrap().0].into_iter().collect(),
        };
        assert_eq!(
            chamber_interior_of(&chamber_addr(), &terrain, WALK),
            interior_of(&walk_addr(), &terrain),
        );
    }
}
