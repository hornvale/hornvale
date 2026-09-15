//! **Reach is not residence** — The Tidemark, Task 5; spec §3.8.
//!
//! Spec §3.8: "a kind's realm answers where it *lives*, never where it *can
//! go*. Any future question of the form 'but it can travel there' is a
//! locomotion and access question, not a realm one."
//!
//! `sea-elf` is the case that forces the distinction and the case this file
//! pins. It is authored two ways at once, deliberately:
//!
//! - `habitat_realm_registry` says `HabitatRealm::Surface`, and
//!   `biome_affinity_registry` confines it to the four shelf biome classes.
//!   That is RESIDENCE, and it is measured: `radiation_affinity::
//!   the_sea_elf_is_confined_to_the_shelf_band` pins it because authored to
//!   the whole ocean this kind would hold ~27,000 vertices against
//!   wood-elf's ~800. **That test is not touched by this campaign and must
//!   stay green unmodified** — if extending reach reddens it, reach has
//!   leaked into residence and the design is wrong, not the test.
//! - `locomotion_registry` says `SWIM`. That is REACH, and until The
//!   Tidemark nothing read it for the water column at all:
//!   `hornvale_climate::Access` had **zero production consumers** in the
//!   whole tree, and `Session::dive` gated only on whether there was water
//!   here, so every body alike took the whole column.
//!
//! # What this file asserts, and the one thing it must NOT
//!
//! It asserts the capability, both poles, at the seam
//! (`vantage::reaches`/`deepest_reachable_band`). It does **not** assert
//! anything about which vertices `sea-elf` holds — that is the other
//! registry, the other test, and the whole point of the separation. A
//! residence assertion here would be this file quietly re-conflating what it
//! exists to keep apart.

use hornvale_climate::{Access, Realm};
use hornvale_kernel::KindId;
use hornvale_species::{HabitatRealm, Locomotion, WALKER, locomotion_registry};
use hornvale_vessel::{deepest_reachable_band, reaches};

/// The roster's own row for `kind`, or the absent-row default.
fn locomotion_of(kind: &'static str) -> Locomotion {
    locomotion_registry()
        .get(&KindId(kind))
        .copied()
        .unwrap_or(WALKER)
}

/// The precondition every assertion below rests on: the two registries
/// really do disagree about `sea-elf`, which is what makes it the case that
/// forces the distinction. Asserted before anything is concluded from it.
#[test]
fn the_sea_elf_is_authored_as_a_surface_dweller_that_swims() {
    assert_eq!(
        hornvale_species::habitat_realm_registry()
            .get(&KindId("sea-elf"))
            .copied(),
        Some(HabitatRealm::Surface),
        "sea-elf's RESIDENCE row is Surface and this campaign does not move it"
    );
    assert!(
        locomotion_of("sea-elf").swim,
        "sea-elf's REACH row carries SWIM, which is what this campaign makes readable"
    );
}

/// The realm publishes how it is entered; the body publishes how it moves;
/// `reaches` is the join. This is `Access`'s first production reader, so the
/// row it reads is pinned here rather than assumed.
#[test]
fn the_water_realm_asks_a_body_to_swim_and_the_overworld_asks_nothing() {
    assert_eq!(
        Realm::WATERWORLD.access,
        Access::Dive,
        "the water column is entered by diving — `deepest_reachable_band` reads this row rather \
         than spelling a second copy of it"
    );
    assert!(reaches(Realm::WATERWORLD.access, locomotion_of("sea-elf")));
    assert!(!reaches(Realm::WATERWORLD.access, WALKER));

    assert_eq!(Realm::OVERWORLD.access, Access::Default);
    assert!(
        reaches(Realm::OVERWORLD.access, WALKER),
        "the overworld asks a body for nothing — a reach gate that refused a walker the land \
         would be the mechanism inverted"
    );
    assert!(
        !reaches(Access::Sealed, locomotion_of("sea-elf")),
        "a sealed void is unreachable by anything, however it moves"
    );
}

/// **The campaign's claim, at the seam.** A sea elf takes the whole pelagic
/// column; a body with no swim row wades the sunlit band and goes no
/// further. Both poles, because a one-sided floor would pass for any
/// authoring at all.
#[test]
fn a_sea_elf_takes_the_whole_column_and_a_walker_wades_the_top_of_it() {
    let depth = Realm::WATERWORLD.strata().len();
    assert_eq!(depth, 5, "the pelagic column is five bands");

    assert_eq!(
        deepest_reachable_band(depth, locomotion_of("sea-elf")),
        depth,
        "a sea elf dives the column and meets tritons, merfolk and abyssal elves where those \
         peoples live (spec §3.8)"
    );
    assert_eq!(
        deepest_reachable_band(depth, WALKER),
        1,
        "a walker `walks and wades`, so it holds the sunlit band and nothing below it — \
         `Locomotion::swim` is documented `crosses DEEP water`, not `enters water`"
    );
    assert_eq!(
        deepest_reachable_band(0, locomotion_of("sea-elf")),
        0,
        "dry land is no bands for anybody"
    );
    assert_eq!(
        deepest_reachable_band(1, WALKER),
        1,
        "a one-band column is entirely the band a walker can already wade"
    );
}

/// Every kind the roster gives a `SWIM` row to takes the whole column, and
/// the flying kinds do not — so the gate is reading the `swim` field rather
/// than the presence of any row at all.
#[test]
fn the_gate_reads_the_swim_field_and_not_the_mere_presence_of_a_row() {
    let depth = Realm::WATERWORLD.strata().len();
    let mut swimmers = 0;
    let mut fliers = 0;
    for (kind, locomotion) in locomotion_registry().iter() {
        if locomotion.swim {
            swimmers += 1;
            assert_eq!(
                deepest_reachable_band(depth, *locomotion),
                depth,
                "{} carries SWIM and must take the whole column",
                kind.0
            );
        } else {
            fliers += 1;
            assert_eq!(
                deepest_reachable_band(depth, *locomotion),
                1,
                "{} has a locomotion row but cannot swim, so a row alone must buy it nothing \
                 in the water",
                kind.0
            );
        }
    }
    assert!(
        swimmers > 0 && fliers > 0,
        "both arms above must be reached, or one of them is vacuous: {swimmers} swimmers, \
         {fliers} non-swimming rows"
    );
}
