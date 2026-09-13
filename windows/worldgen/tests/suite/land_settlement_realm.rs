//! `land_settlement` must select on the occupying people's
//! [`hornvale_species::HabitatRealm`], not on the settlement's own committed
//! `biome` fact — the fact this suite proves is that the two disagree for a
//! real settlement (The Tidemark, found by a board notice rather than
//! review).
//!
//! `Biome::is_marine` is an explicit match over **surface** marine variants
//! and cannot see realm at all: a subterranean settlement commits its
//! vertex's *surface* biome (a duergar hold under a forest commits
//! `"temperate forest"`), so `!is_marine()` reads it as land. The predicate
//! this test exercises is the one that distinguishes the two cases, and it
//! would go RED under the old `!hornvale_climate::Biome::is_marine()`
//! filter — verified by temporarily restoring that filter and confirming
//! this test fails (see the campaign ledger).

use hornvale_kernel::Seed;
use hornvale_species::HabitatRealm;

/// The habitat realm a settlement's occupying people resolves to, `None` if
/// the settlement carries no committed `peopled-by` fact.
fn realm_of(world: &hornvale_kernel::World, id: hornvale_kernel::EntityId) -> Option<HabitatRealm> {
    let species = hornvale_species::species_of(world, id)?;
    Some(
        hornvale_species::habitat_realm_registry()
            .get_by_label(&species)
            .copied()
            .unwrap_or(HabitatRealm::SURFACE),
    )
}

/// Seed 17 is a fact about the generator, not an assumption: measured while
/// writing this test (a sweep of seeds 1-59), it is the first seed at which
/// ledger order places a `Subterranean`-realm settlement (a drow/duergar
/// hold, non-marine biome) *before* every settlement of an earlier-sorting
/// Surface people — the exact ordering the old `!is_marine()` filter would
/// have walked into and returned. Nine of the fifty-nine seeds swept share
/// this shape (17, 18, 20, 22, 29, 39, 48, 54, 59); seed 17 is simply the
/// first. The settlement this test asserts on is found by SEARCHING that
/// world's committed settlements for one whose people resolves to
/// `Subterranean`, never by pinning a vertex — a pinned vertex would assert
/// today's placement rather than today's SELECTION RULE.
#[test]
fn land_settlement_rejects_a_subterranean_occupant_the_old_biome_filter_could_not_see() {
    let world = hornvale_worldgen::build_world(
        Seed(17),
        &Default::default(),
        &Default::default(),
        &Default::default(),
    )
    .expect("seed 17 builds");

    let subterranean = hornvale_settlement::all_settlements(&world)
        .into_iter()
        .find(|v| realm_of(&world, v.id) == Some(HabitatRealm::Subterranean))
        .expect(
            "seed 17 is expected to place at least one Subterranean \
             settlement (mountain-dwarf/duergar/kuo-toa/svirfneblin); if the \
             generator has changed and none exists any more, find another \
             seed that does rather than deleting this precondition",
        );
    // Sanity on the precondition itself: a settlement whose own committed
    // biome would have passed the OLD `!is_marine()` filter, which is what
    // makes it capable of exposing the defect at all. Every land biome is
    // non-marine, so this holds for any Subterranean settlement standing
    // under a dry vertex -- true of the whole roster here, since none of the
    // four subterranean peoples is marine.
    let biome = world
        .ledger
        .text_of(subterranean.id, hornvale_settlement::BIOME)
        .expect("a settlement carries a committed biome");
    assert!(
        !hornvale_climate::Biome::catalog()
            .iter()
            .find(|b| b.name() == biome)
            .expect("a committed biome name is a known biome")
            .is_marine(),
        "the found settlement's biome ({biome}) is marine, so it would not \
         have exposed the old filter's defect -- this precondition needs a \
         non-marine subterranean settlement specifically",
    );

    // The actual assertion: `land_settlement` must never return a settlement
    // whose people is not Surface-realm, even though this one would have
    // passed a biome-only filter.
    let land = hornvale_worldgen::land_settlement(&world)
        .expect("a world with a settlement roster has a land subject");
    let land_realm = realm_of(&world, land.id);
    assert_eq!(
        land_realm,
        Some(HabitatRealm::Surface),
        "land_settlement returned {} ({}), whose people resolves to {:?} \
         rather than Surface -- a subterranean settlement's own committed \
         biome is a SURFACE reading and cannot be trusted to reject it",
        land.name,
        land.id.0,
        land_realm
    );
    assert_ne!(
        land.id, subterranean.id,
        "land_settlement returned the very settlement this test found to be \
         Subterranean; the assertion above should already have caught this, \
         but a coincidental Surface realm would make it vacuous",
    );
}
