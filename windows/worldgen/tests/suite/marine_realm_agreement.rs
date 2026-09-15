//! The Tidemark, Task 1, spec §3.5: a two-way agreement test pinning
//! `hornvale_species::HabitatRealm` against `hornvale_climate::facets::Realm`
//! as a **bijection**.
//!
//! **Why here, and nowhere else.** A domain crate may depend on the kernel
//! and nothing else — `domains/species` may not depend on `domains/climate`,
//! nor the reverse (root `CLAUDE.md`, "Architecture"). The two realm
//! vocabularies therefore cannot meet inside either domain; they meet only
//! at the composition root, `windows/worldgen`, which is the only crate
//! that legitimately depends on both. This file is that meeting.
//!
//! **Why a one-directional check would be blind to the hazard that matters
//! here.** `HabitatRealm` has three inhabited variants today
//! (`Surface`/`Subterranean`/`Marine`); `Realm` carries three sanctioned
//! consts (`OVERWORLD`/`UNDERDARK`/`WATERWORLD`), discriminated by
//! `Medium` (`AirOverRock`/`Water`/`Rock`). Checking only that every
//! `HabitatRealm` maps to SOME `Realm` (injectivity alone) would stay green
//! even if a future `Medium` variant (an elemental plane, say) had no
//! `HabitatRealm` counterpart to come back to — an OVER-ADMISSION on the
//! `Realm` side that a forward-only check cannot see, because it never asks
//! the reverse question at all. So this file asserts BOTH directions:
//!
//! - [`the_forward_map_is_total_and_recovers_its_input`][]: every
//!   `HabitatRealm` maps to a `Realm` whose medium maps straight back to the
//!   SAME `HabitatRealm` — enforces that `species` → `climate` loses nothing.
//! - [`the_backward_map_is_total_and_recovers_its_input`][]: every `Medium`
//!   (the discriminating half of `Realm`) maps to a `HabitatRealm` whose
//!   forward image has that SAME medium — enforces that `climate` → `species`
//!   admits nothing extra.
//!
//! **Why this is guaranteed to fail on an unpaired addition, not merely
//! likely to.** Both conversion functions below `match` on an enum with no
//! wildcard arm (`HabitatRealm` in [`to_climate_realm`], `Medium` in
//! [`from_climate_realm`]) — the same "the compiler enumerates the sites"
//! discipline `substrate_response` uses (spec §3.1). Adding a `HabitatRealm`
//! variant with no `climate` counterpart is a **compile error** in
//! [`to_climate_realm`], not a runtime miss; adding a `Medium` variant with
//! no `HabitatRealm` counterpart is a compile error in
//! [`from_climate_realm`]. The round-trip assertions below additionally
//! catch a MISMATCHED pairing (both sides added something, but not to each
//! other) — the case an exhaustive match alone cannot see, because it only
//! demands every arm return something, never that the something is correct.
//!
//! Verified by hand while authoring this file: adding a placeholder fourth
//! `HabitatRealm` variant and building the workspace fails closed at
//! `hornvale_species::substrate_response` first (`domains/species` compiles
//! before `windows/worldgen` in the dependency order, so its own
//! no-wildcard match is what the compiler reaches first — this is the exact
//! "compiler enumerates the sites" mechanism spec §3.1 describes, and this
//! task hit six real instances of it while adding `Marine`). The same
//! guarantee is what makes [`to_climate_realm`] and [`from_climate_realm`]
//! below fail identically the moment `domains/species` itself compiles
//! again: neither carries a wildcard arm, and Rust's exhaustiveness check is
//! a hard property of the match, not something that needs re-demonstrating
//! per call site — reverted before commit either way.

use hornvale_climate::facets::{Medium, Realm};
use hornvale_species::HabitatRealm;

/// `species` → `climate`: total by an exhaustive match with no wildcard arm.
/// A `HabitatRealm` variant added here without a corresponding arm fails to
/// compile, in this function, before any test runs.
fn to_climate_realm(realm: HabitatRealm) -> Realm {
    match realm {
        HabitatRealm::Surface => Realm::OVERWORLD,
        HabitatRealm::Subterranean => Realm::UNDERDARK,
        HabitatRealm::Marine => Realm::WATERWORLD,
    }
}

/// `climate` → `species`: total over [`Medium`], the discriminating half of
/// [`Realm`] (`access` varies per formation/site and carries no independent
/// realm identity — see [`Realm::strata`]'s own `match self.medium`, which
/// this mirrors). Also an exhaustive match with no wildcard arm.
fn from_climate_realm(medium: Medium) -> HabitatRealm {
    match medium {
        Medium::AirOverRock => HabitatRealm::Surface,
        Medium::Water => HabitatRealm::Marine,
        Medium::Rock => HabitatRealm::Subterranean,
    }
}

/// Every [`HabitatRealm`] variant, named explicitly rather than derived —
/// there is no `HabitatRealm::all()` and inventing one for a single test
/// would be a needless production addition. Adding a variant here is a
/// separate, deliberate edit from the compile-enforced totality above; this
/// array is what makes the ROUND TRIP observable, not what makes the
/// mapping total.
const ALL_HABITAT_REALMS: [HabitatRealm; 3] = [
    HabitatRealm::Surface,
    HabitatRealm::Subterranean,
    HabitatRealm::Marine,
];

/// Every [`Medium`] variant, named explicitly for the same reason.
const ALL_MEDIA: [Medium; 3] = [Medium::AirOverRock, Medium::Water, Medium::Rock];

/// Enforces the `species` → `climate` direction: every `HabitatRealm` maps
/// forward to a `Realm`, and that `Realm`'s medium maps straight back to the
/// SAME `HabitatRealm` it started from. A `HabitatRealm` that mapped forward
/// to a `Realm` sharing its medium with some OTHER `HabitatRealm` (an
/// over-admission on the `Realm` side reached through this direction) would
/// fail this round trip even though [`to_climate_realm`] compiled cleanly.
#[test]
fn the_forward_map_is_total_and_recovers_its_input() {
    for hr in ALL_HABITAT_REALMS {
        let realm = to_climate_realm(hr);
        let back = from_climate_realm(realm.medium);
        assert_eq!(
            back, hr,
            "HabitatRealm::{hr:?} -> Realm(medium={:?}) -> HabitatRealm::{back:?}: \
             round trip must recover the original variant",
            realm.medium
        );
    }
}

/// Enforces the `climate` → `species` direction: every `Medium` maps
/// backward to a `HabitatRealm`, and that `HabitatRealm`'s forward image
/// carries the SAME medium it started from. A `Medium` that mapped backward
/// to a `HabitatRealm` whose own forward image used a DIFFERENT medium (an
/// over-admission on the `HabitatRealm` side reached through this direction)
/// would fail this round trip even though [`from_climate_realm`] compiled
/// cleanly. This is the direction a one-directional check (the forward one
/// alone) cannot see at all.
#[test]
fn the_backward_map_is_total_and_recovers_its_input() {
    for medium in ALL_MEDIA {
        let hr = from_climate_realm(medium);
        let back = to_climate_realm(hr).medium;
        assert_eq!(
            back, medium,
            "Medium::{medium:?} -> HabitatRealm::{hr:?} -> Medium::{back:?}: \
             round trip must recover the original medium"
        );
    }
}

/// A sanity check the two tests above don't state directly: the mapping is
/// actually one-to-one over the three named variants, not merely
/// round-trip-stable (which a constant function would also satisfy). Three
/// distinct inputs must produce three distinct outputs on each side.
#[test]
fn the_map_is_injective_over_the_three_named_realms() {
    let images: Vec<Realm> = ALL_HABITAT_REALMS
        .iter()
        .map(|&hr| to_climate_realm(hr))
        .collect();
    assert_ne!(images[0], images[1]);
    assert_ne!(images[0], images[2]);
    assert_ne!(images[1], images[2]);

    let preimages: Vec<HabitatRealm> = ALL_MEDIA.iter().map(|&m| from_climate_realm(m)).collect();
    assert_ne!(preimages[0], preimages[1]);
    assert_ne!(preimages[0], preimages[2]);
    assert_ne!(preimages[1], preimages[2]);
}
