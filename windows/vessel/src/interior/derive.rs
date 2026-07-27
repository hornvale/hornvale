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
}
