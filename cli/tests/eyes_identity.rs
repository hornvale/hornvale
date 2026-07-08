//! The Eyes identity contract (spec §7): a goblin-pinned world reproduces
//! pre-Eyes main as a superset (almanac byte-identical; ledger identical
//! off the species predicates added since the pre-Eyes baseline — the three
//! perception predicates and, from The Tongues, the six articulation
//! predicates), and the default world's goblin pantheon — entities, facts,
//! and rendered section — is unmoved.

use hornvale_kernel::Seed;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// Species predicates added AFTER the pre-Eyes fixture was frozen: the three
/// perception predicates (The Eyes) and the six articulation predicates (The
/// Tongues). The fixture has neither, so the superset comparison excludes
/// them — extending them is the same superset rule the campaigns before it
/// applied.
const POST_BASELINE_SPECIES_PREDICATES: [&str; 9] = [
    hornvale_species::SPECIES_ACTIVITY_CYCLE,
    hornvale_species::SPECIES_NIGHT_VISION,
    hornvale_species::SPECIES_SKY_ATTENTION,
    hornvale_species::SPECIES_LABIALITY,
    hornvale_species::SPECIES_VOWEL_SPACE,
    hornvale_species::SPECIES_VOICING,
    hornvale_species::SPECIES_SIBILANCE,
    hornvale_species::SPECIES_VOICE_LOUDNESS,
    hornvale_species::SPECIES_EXOTIC_MANNER,
];

fn build(species: Option<&str>) -> hornvale_kernel::World {
    let world = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: species.map(str::to_string),
            ..SettlementPins::default()
        },
    )
    .unwrap();
    // Round-trip through the save format — same rationale as
    // species_identity.rs (serde_json's 1-ULP float quirk).
    hornvale_kernel::World::from_json(&world.to_json()).unwrap()
}

fn filtered(world: &hornvale_kernel::World) -> Vec<String> {
    world
        .ledger
        .iter()
        .filter(|f| !POST_BASELINE_SPECIES_PREDICATES.contains(&f.predicate.as_str()))
        .map(|f| format!("{f:?}"))
        .collect()
}

#[test]
#[ignore = "superseded by The Tongues structural invariants (tongues_identity.rs, Task 11); \
            byte-identity against pre-Eyes/pre-species fixtures does not survive belief \
            restructuring — spec §8"]
fn goblin_pinned_seed_42_is_a_superset_of_pre_eyes_main() {
    let fixture: hornvale_kernel::World =
        serde_json::from_str(include_str!("fixtures/pre-eyes-seed-42-goblin-world.json")).unwrap();
    let world = build(Some("goblin"));
    assert_eq!(
        filtered(&world),
        filtered(&fixture),
        "filtered ledgers diverge"
    );
    let ctx = hornvale_worldgen::almanac_context(&world).unwrap();
    assert_eq!(
        hornvale_almanac::render(&ctx),
        include_str!("fixtures/pre-eyes-seed-42-goblin-almanac.md"),
        "goblin-pinned almanac diverged from the pre-Eyes fixture"
    );
}

#[test]
#[ignore = "superseded by The Tongues structural invariants (tongues_identity.rs, Task 11); \
            byte-identity against pre-Eyes/pre-species fixtures does not survive belief \
            restructuring — spec §8"]
fn the_default_worlds_goblin_pantheon_is_unmoved() {
    let fixture: hornvale_kernel::World =
        serde_json::from_str(include_str!("fixtures/pre-eyes-seed-42-default-world.json")).unwrap();
    let world = build(None);
    let goblin_flagship = |w: &hornvale_kernel::World| {
        hornvale_worldgen::flagship_of(w, "goblin")
            .expect("goblin flagship")
            .id
    };
    let ours = hornvale_religion::beliefs_held_by(&world, goblin_flagship(&world));
    let theirs = hornvale_religion::beliefs_held_by(&fixture, goblin_flagship(&fixture));
    assert_eq!(
        ours, theirs,
        "goblin beliefs (ids, tenets, kinds, heads) moved"
    );
    assert!(
        !ours.is_empty(),
        "vacuous identity — no goblin pantheon at all"
    );

    // The kobold pantheon exists and is differently headed (spec §12.1).
    let kobold_flagship = hornvale_worldgen::flagship_of(&world, "kobold")
        .expect("seed 42 places a kobold flagship")
        .id;
    let kobold = hornvale_religion::beliefs_held_by(&world, kobold_flagship);
    assert!(!kobold.is_empty(), "the kobold pantheon exists");
    let seen = hornvale_worldgen::observed_phenomena_as(&world, "kobold").unwrap();
    assert_eq!(
        seen[0].venue,
        hornvale_kernel::Venue::NightSky,
        "the kobold head deity derives from the night sky"
    );
}
