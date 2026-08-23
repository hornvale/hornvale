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
    let registry = hornvale_species::perception_registry();
    for npc in &npcs {
        let village = npc
            .village
            .as_ref()
            .expect("a settlement-derived creature carries Some(village)");
        assert!(
            village.population > 0,
            "{}: a derived creature's village is real, not a placeholder",
            npc.label
        );
        // Fix round 1, Finding 1: the original assertion here compared
        // `npc.perception.activity` to `npc.activity` — two fields written
        // by the SAME call site in `derive_npcs`, so a mutant that replaced
        // the whole perception resolution with `PerceptionVector::MANIKIN`
        // unconditionally still passed (seed 42's hobgoblins are `Diurnal`,
        // which coincides with the manikin's default), and `night_vision`/
        // `sky_attention` were unguarded entirely. Assert against the
        // authored source directly instead — the species' own registry
        // entry, on all three fields — so a constant cannot satisfy it: the
        // registry's `hobgoblin` row carries `night_vision: 0.6`, which
        // `MANIKIN` does not.
        let expected = registry.get_by_label(&npc.species).unwrap_or_else(|| {
            panic!(
                "{}: species '{}' has no authored perception entry",
                npc.label, npc.species
            )
        });
        assert_eq!(
            npc.perception, *expected,
            "{}: perception must be the species' own authored vector, not a constant",
            npc.label
        );
    }
}

/// The Hand's premise: possession mints a body at the most-populous
/// settlement, and `ordered_for_derivation` hoists that same settlement to
/// index 0 — so the two are one villager built twice. This test states that
/// as an equality; Task 3 deletes one of them on the strength of it.
///
/// The plan brief that seeded this test called `body_at(&world, &ctx,
/// &flagship)` — three arguments. Task 2's ambiguity ruling (the entity is
/// passed in, not minted by `body_at`) adds a fourth: an already-minted
/// `EntityId`. The equality below never compares `entity` (the brief did
/// not either), so any value works; `first.entity` is used because it is
/// the natural one on hand. The brief also compared `first.village.id` to
/// `minted.village.id` directly — stale against Task 1, which made
/// `Npc.village` an `Option<VillageInfo>`.
#[test]
fn the_flagship_body_and_the_first_derived_creature_are_the_same_body() {
    let (world, ctx) = seed_42();
    let mut ledger = world.ledger.clone();
    let flagship =
        hornvale_vessel::most_populous_settlement(&world).expect("seed 42 has a settlement");

    let derived = hornvale_vessel::liveness::derive_npcs(&world, &ctx, &mut ledger, 4, flagship.id);
    let first = derived
        .first()
        .expect("precondition: at least one creature");
    let minted = hornvale_vessel::liveness::body_at(&world, &ctx, &flagship, first.entity);

    assert_eq!(
        first
            .village
            .as_ref()
            .expect("a settlement-derived creature carries Some(village)")
            .id,
        minted
            .village
            .as_ref()
            .expect("body_at derives from a real settlement")
            .id,
        "same settlement"
    );
    assert_eq!(first.species, minted.species, "same species");
    assert_eq!(first.home, minted.home, "same home");
    assert_eq!(first.mass_kg, minted.mass_kg, "same body");
    assert_eq!(
        first.perception.activity, minted.perception.activity,
        "same senses"
    );
}
