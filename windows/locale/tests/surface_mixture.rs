//! The mixture-keeping refactor must not move a single colour.
//!
//! Task 2a splits `reflectance_at` into a mixture producer
//! (`reflectance_mixture_at`) plus an `integrate()` call, so a later caller
//! (Task 2b) can reach the un-integrated components. This is the positive
//! control for that split: integrating the kept mixture must equal the old
//! immediate integration, exactly — `Reflectance` derives `PartialEq`, so
//! the comparison below is exact, not approximate. If this test ever fails,
//! the refactor (not the cover model that comes after it) moved a colour.

use hornvale_kernel::{RoomAddr, Seed, World};
use hornvale_locale::LocaleContext;

/// A depth at which every constructed `RoomAddr` is guaranteed addressable:
/// `corner_weights` requires `path.len() >= geo.level()`, and the seed-42
/// world's canonical grid sits at level 6 (`GLOBE_LEVEL`), so 12 — the same
/// "six refinement levels below the canonical grid" convention
/// `windows/locale/src/lib.rs` and `windows/vessel/src/agent.rs` already use
/// for a walking depth — clears that floor with room to spare.
const DEPTH: usize = 12;

/// A spread of `n` distinct, addressable `RoomAddr`s across all 20
/// icosahedron faces, varying the leading two path digits and padding the
/// rest to `DEPTH` with zeros. 20 faces x 4 x 4 = 320 distinct addresses,
/// comfortably above the 200 the brief asks for.
fn spread(n: usize) -> Vec<RoomAddr> {
    let mut out = Vec::with_capacity(n);
    'outer: for face in 0..20u8 {
        for a in 0..4u8 {
            for b in 0..4u8 {
                let mut path = vec![a, b];
                path.resize(DEPTH, 0);
                out.push(RoomAddr { face, path });
                if out.len() >= n {
                    break 'outer;
                }
            }
        }
    }
    out
}

#[test]
fn integrating_the_kept_mixture_equals_integrating_immediately() {
    let world = World::new(Seed(42));
    let ctx = LocaleContext::build(&world).unwrap();
    let addrs = spread(200);
    assert!(
        addrs.len() >= 200,
        "spread must cover at least 200 addresses"
    );
    for addr in &addrs {
        let via_mixture = ctx.reflectance_mixture_at(addr).unwrap().integrate();
        let direct = ctx.reflectance_at(addr).unwrap();
        assert_eq!(via_mixture, direct, "addr {addr:?} moved");
    }
}
