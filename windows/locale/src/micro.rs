//! The sub-cell micro-field: a few grounded per-room continuous axes drawn
//! from the room's address noise, so a walk through homogeneous biome still
//! varies room-to-room (the "miles and miles of forest" answer).

use crate::regime::MicroField;
use crate::streams::LOCALE_MICRO;
use hornvale_kernel::{Seed, quantize};

/// Four axes in [-1, 1], each a distinct sub-stream of the room's micro label,
/// quantized at emit (platform-exact).
pub(crate) fn micro_field(room_seed: Seed) -> MicroField {
    let mut s = room_seed.derive(LOCALE_MICRO).stream();
    let mut axis = || quantize(s.next_f64() * 2.0 - 1.0);
    MicroField {
        relief: axis(),
        aspect: axis(),
        wetness: axis(),
        openness: axis(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{RoomAddr, Seed};

    fn seed_for(path: Vec<u8>) -> Seed {
        RoomAddr { face: 3, path }.seed(Seed(42))
    }

    #[test]
    fn micro_field_is_deterministic_and_in_range() {
        let s = seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3]);
        let a = micro_field(s);
        let b = micro_field(s);
        assert_eq!(a, b);
        for v in [a.relief, a.aspect, a.wetness, a.openness] {
            assert!((-1.0..=1.0).contains(&v));
        }
    }

    #[test]
    fn sibling_rooms_differ() {
        let a = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 0]));
        let b = micro_field(seed_for(vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 1]));
        assert_ne!(a, b);
    }
}
