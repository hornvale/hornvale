//! Task 2a's positive control: the mixture-keeping split of `reflectance_at`
//! stays wired correctly.
//!
//! `reflectance_at` is now defined as
//! `self.reflectance_mixture_at(addr)?.integrate()`, so
//! `assert_eq!(reflectance_mixture_at(addr).integrate(), reflectance_at(addr))`
//! holds for *any* implementation of `reflectance_mixture_at` — there is no
//! interior mutability anywhere in the call chain, so the two sides are
//! structurally the same expression evaluated twice. This test therefore
//! does **not** establish byte-identity against the pre-refactor
//! implementation; it is a wiring check. It is still worth keeping: a future
//! edit that accidentally de-links the delegation (has `reflectance_at`
//! integrate something other than what `reflectance_mixture_at` returns)
//! would fail it.
//!
//! **The byte-identity control is the empty `make rebaseline` diff**
//! against `docs/generated-paths.txt` (see the task report) — that is the
//! artifact comparison that actually shows no colour moved, because it
//! compares this commit's rendered output against the committed output of
//! the pre-refactor code, not two calls into the same commit.

use hornvale_kernel::{RoomAddr, Seed, World};
use hornvale_locale::LocaleContext;
use std::collections::BTreeSet;

/// A depth at which every constructed `RoomAddr` is guaranteed addressable:
/// `corner_weights` requires `path.len() >= geo.level()`, and the seed-42
/// world's canonical grid sits at level 6 (`GLOBE_LEVEL`), so 12 — the same
/// "six refinement levels below the canonical grid" convention
/// `windows/locale/src/lib.rs` and `windows/vessel/src/agent.rs` already use
/// for a walking depth — clears that floor with room to spare.
const DEPTH: usize = 12;

/// How many addresses to generate per icosahedron face.
const PER_FACE: u8 = 10;

/// A spread of exactly `20 * PER_FACE` (= 200) distinct, addressable
/// `RoomAddr`s, round-robin distributed 10 per face across all 20
/// icosahedron faces (never filled greedily from one face — the earlier
/// version of this test claimed "all 20 faces" while actually only
/// reaching 13 of them, because it broke out of the loop as soon as it had
/// 200 addresses). Each face's 10 addresses vary the leading two path
/// digits (`i % 4`, `i / 4` for `i` in `0..PER_FACE`, base-4 digits so all
/// ten are distinct), padded to `DEPTH` with zeros.
fn spread() -> Vec<RoomAddr> {
    let mut out = Vec::with_capacity(20 * PER_FACE as usize);
    for face in 0..20u8 {
        for i in 0..PER_FACE {
            let mut path = vec![i % 4, i / 4];
            path.resize(DEPTH, 0);
            out.push(RoomAddr { face, path });
        }
    }
    out
}

#[test]
fn integrating_the_kept_mixture_equals_integrating_immediately() {
    let world = World::new(Seed(42));
    let ctx = LocaleContext::build(&world).unwrap();
    let addrs = spread();
    assert_eq!(
        addrs.len(),
        200,
        "spread must produce exactly 200 addresses"
    );
    let faces: BTreeSet<u8> = addrs.iter().map(|a| a.face).collect();
    assert_eq!(
        faces.len(),
        20,
        "spread must cover all 20 icosahedron faces"
    );
    for addr in &addrs {
        let via_mixture = ctx.reflectance_mixture_at(addr).unwrap().integrate();
        let direct = ctx.reflectance_at(addr).unwrap();
        assert_eq!(via_mixture, direct, "addr {addr:?} moved");
    }
}
