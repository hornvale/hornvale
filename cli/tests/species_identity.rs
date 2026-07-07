//! The superset contract (spec §8): a goblin-pinned world reproduces
//! pre-species main — almanac byte-identical, ledger identical on pre-C1
//! predicates (species predicates and the pin fact set aside). Also asserts
//! pin isolation: pinning `--species goblin` must not shift the draws for
//! cells goblins already win in the unpinned world.

use hornvale_kernel::{Seed, Value};
use hornvale_settlement::CELL_ID;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// Predicates that did not exist before species genesis (Campaign Y2-1):
/// the psychology vector, the peopled-by link, and the settlement pin fact
/// that records the `--species` pin itself. Excluded from the pre-C1 ledger
/// comparison.
const NEW_PREDICATES: [&str; 9] = [
    hornvale_species::SPECIES_NAME,
    hornvale_species::THREAT_RESPONSE,
    hornvale_species::DELIBERATION_LATENCY,
    hornvale_species::IN_GROUP_RADIUS,
    hornvale_species::TIME_HORIZON,
    hornvale_species::SOCIALITY_MODE,
    hornvale_species::STATUS_BASIS,
    hornvale_species::PEOPLED_BY,
    hornvale_settlement::SETTLEMENT_PIN,
];

/// The ledger's facts, restricted to pre-species predicates, as debug
/// strings. Iteration is commit order on both sides (kernel `Ledger::iter`),
/// so this is a faithful ledger-identity comparison.
fn filtered(world: &hornvale_kernel::World) -> Vec<String> {
    world
        .ledger
        .iter()
        .filter(|f| !NEW_PREDICATES.contains(&f.predicate.as_str()))
        .map(|f| format!("{f:?}"))
        .collect()
}

#[test]
fn goblin_pinned_seed_42_is_a_superset_of_pre_species_main() {
    let fixture: hornvale_kernel::World =
        serde_json::from_str(include_str!("fixtures/pre-species-seed-42-world.json"))
            .expect("fixture parses");
    let world = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
            ..SettlementPins::default()
        },
    )
    .unwrap();
    // Round-trip through the JSON save format before comparing, exactly as
    // `hornvale new` writes world.json and `hornvale almanac`/`repl` load it
    // back (`cmd_almanac` in cli/src/main.rs always loads from disk — it
    // never renders a freshly-built, never-saved world). This is not
    // optional housekeeping: serde_json 1.0.150's float parser does not
    // always invert its own writer bit-for-bit (a handful of literals, this
    // world's obliquity-degrees among them, round-trip 1 ULP off). The
    // fixture was captured from a saved-and-reloaded world, so comparing it
    // against an in-memory-only world would flag that external, pre-existing
    // serialization quirk as a false divergence. Round-tripping both sides
    // through the identical save format compares what a CLI user actually
    // observes, and still catches any real regression (which would produce
    // far more than a 1-ULP difference).
    let world = hornvale_kernel::World::from_json(&world.to_json())
        .expect("world round-trips through JSON");

    // Ledger, restricted to pre-species predicates, is identical.
    assert_eq!(
        filtered(&world),
        filtered(&fixture),
        "filtered ledgers diverge"
    );

    // Almanac byte-identical. Goes through the same two calls
    // `cmd_almanac` in cli/src/main.rs makes: `almanac_context` then
    // `hornvale_almanac::render`.
    let ctx = hornvale_worldgen::almanac_context(&world).expect("almanac context builds");
    let rendered = hornvale_almanac::render(&ctx);
    let expected = include_str!("fixtures/pre-species-seed-42-almanac.md");
    assert_eq!(
        rendered, expected,
        "almanac diverged from the pre-species fixture"
    );
}

#[test]
fn species_pin_does_not_shift_draws_for_cells_goblins_hold_in_both_worlds() {
    let unpinned = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let pinned = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
            ..SettlementPins::default()
        },
    )
    .unwrap();

    // Every goblin settlement in the pinned world, in placement order (the
    // flagship is first). For each, find the settlement sitting on the same
    // cell in the unpinned world; if that cell went to another species there,
    // move on. The first cell goblins hold in both worlds is the one we
    // compare — this is robust even if some cells goblins win in the
    // goblin-only pinned world are kobold-held in the unpinned two-species
    // world.
    let cell_of = |world: &hornvale_kernel::World, id: hornvale_kernel::EntityId| -> u32 {
        match world.ledger.value_of(id, CELL_ID) {
            Some(Value::Number(n)) => *n as u32,
            other => panic!("settlement {id:?} has no numeric cell-id fact: {other:?}"),
        }
    };
    let settlement_on_cell = |world: &hornvale_kernel::World, cell: u32| {
        world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .map(|f| f.subject)
            .find(|id| cell_of(world, *id) == cell)
    };

    let pinned_goblins: Vec<hornvale_kernel::EntityId> = pinned
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .filter(|id| hornvale_species::species_of(&pinned, *id).as_deref() == Some("goblin"))
        .collect();
    assert!(
        !pinned_goblins.is_empty(),
        "the goblin-pinned world should place at least one goblin settlement"
    );

    let shared_cell = pinned_goblins.into_iter().find_map(|id| {
        let cell = cell_of(&pinned, id);
        let unpinned_id = settlement_on_cell(&unpinned, cell)?;
        if hornvale_species::species_of(&unpinned, unpinned_id).as_deref() == Some("goblin") {
            Some((id, unpinned_id))
        } else {
            None
        }
    });
    let (pinned_id, unpinned_id) =
        shared_cell.expect("seed 42 should hold at least one cell goblins win in both worlds");

    let name_of = |world: &hornvale_kernel::World, id: hornvale_kernel::EntityId| -> String {
        world
            .ledger
            .text_of(id, hornvale_kernel::NAME)
            .expect("a settlement has a name")
            .to_string()
    };
    let population_of = |world: &hornvale_kernel::World, id: hornvale_kernel::EntityId| -> u32 {
        match world.ledger.value_of(id, hornvale_settlement::POPULATION) {
            Some(Value::Number(n)) => *n as u32,
            other => panic!("settlement {id:?} has no numeric population fact: {other:?}"),
        }
    };

    assert_eq!(
        name_of(&pinned, pinned_id),
        name_of(&unpinned, unpinned_id),
        "pinning --species goblin shifted the settlement name on a cell goblins hold in both worlds"
    );
    assert_eq!(
        population_of(&pinned, pinned_id),
        population_of(&unpinned, unpinned_id),
        "pinning --species goblin shifted the population on a cell goblins hold in both worlds"
    );
}
