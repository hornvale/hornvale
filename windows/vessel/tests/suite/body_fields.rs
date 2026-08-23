//! The Hand, Task 1: a derived creature carries the two fields that used to
//! belong only to a possessed body.

/// Seed 42's world and locale context. Tasks 2-6 use this rather than
/// repeating the build; one place to change if the flagship seed ever moves.
///
/// The plan brief that seeded this helper named `hornvale_worldgen::
/// locale_context`, which does not exist — `LocaleContext::build` lives in
/// `hornvale_locale` (confirmed against `derive_npcs_are_distinct_and_placed`
/// in `windows/vessel/src/liveness.rs`, which builds a context the same way).
pub(crate) fn seed_42() -> (hornvale_kernel::World, hornvale_locale::LocaleContext) {
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .expect("seed 42 builds");
    let ctx = hornvale_locale::LocaleContext::build(&world).expect("a context builds");
    (world, ctx)
}

#[test]
fn a_derived_creature_carries_its_species_perception_and_its_village() {
    let (world, ctx) = seed_42();
    let mut ledger = world.ledger.clone();
    let home = hornvale_vessel::most_populous_settlement(&world)
        .expect("seed 42 has a settlement")
        .id;
    let npcs = hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut ledger, 4, home);

    assert!(!npcs.is_empty(), "precondition: seed 42 derives creatures");
    for npc in &npcs {
        assert!(
            npc.village.population > 0,
            "{}: a derived creature's village is real, not a placeholder",
            npc.label
        );
        // The perception vector is the species', so its activity cycle must
        // agree with the activity the creature was already derived with —
        // two reads of one authored fact, which is what makes this a
        // coherence check rather than a restatement.
        assert_eq!(
            npc.perception.activity, npc.activity,
            "{}: perception and activity are both the species' own",
            npc.label
        );
    }
}
