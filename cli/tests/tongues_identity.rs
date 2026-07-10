//! The Tongues structural-invariant keystone (spec §8): every proper noun
//! regenerates, so byte-identity is gone. Instead: entity structure and all
//! NON-linguistic facts are unchanged from pre-Tongues; names/tenets differ;
//! names are unique and phonotactically valid (well-formed by construction —
//! see Task 12 for the census-level calibration); determinism holds.

use hornvale_kernel::{EntityId, Seed, Value, World};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

/// Predicates whose SHAPE changed under The Tongues, or that are wholly new
/// this campaign or a later one: settlement/deity names are freshly drawn
/// text (never byte-identical to the fixture); belief content moved from a
/// single prose `tenet` fact to the structured deity/epithet/sentiment
/// facts `render_line` now voices (spec §6); the six articulation-vector
/// predicates did not exist before this campaign; the two orbital-forcing
/// predicates (mean eccentricity, obliquity oscillation amplitude) are new
/// in Firm Ground II and were never committed by pre-Tongues genesis; the
/// five paleoclimate predicates are new in Deep Time and were
/// likewise never committed by pre-Tongues genesis; `name-gloss` is wholly
/// new to The Words (Task 9), which glosses proper names but never touches
/// this file's own pre-Tongues fixture or comparison — it's listed here only
/// so this older keystone keeps passing against a world that now carries the
/// newer predicate too. `terrain-pin` is excluded for a different reason:
/// the pre-Tongues fixture predates the Crust epoch's `--globe-level` pin
/// entirely (it was built when level 5 was simply the unpinned default), so
/// this file's comparison world must pin `globe-level=5` to keep comparing
/// like-for-like terrain (see `entity_structure_and_non_linguistic_facts_
/// match_the_pre_tongues_fixture`) — that pin mints a `terrain-pin` ledger
/// fact the unpinned fixture never recorded, an artifact of how this test
/// reconstructs the old default, not a genuine divergence. Excluded from the
/// entity-structure / non-linguistic-fact comparison below — the same
/// "superset, minus what's genuinely new" pattern `eyes_identity.rs` and
/// `species_identity.rs` use.
const CHANGED_OR_NEW_PREDICATES: [&str; 22] = [
    hornvale_kernel::NAME,
    hornvale_worldgen::NAME_GLOSS,
    hornvale_religion::TENET,
    hornvale_religion::DEITY_NAME,
    hornvale_religion::DEITY_NAME_IPA,
    hornvale_religion::DEITY_EPITHET,
    hornvale_religion::DEITY_EPITHET_IPA,
    hornvale_religion::SENTIMENT,
    hornvale_species::SPECIES_LABIALITY,
    hornvale_species::SPECIES_VOWEL_SPACE,
    hornvale_species::SPECIES_VOICING,
    hornvale_species::SPECIES_SIBILANCE,
    hornvale_species::SPECIES_VOICE_LOUDNESS,
    hornvale_species::SPECIES_EXOTIC_MANNER,
    hornvale_astronomy::facts::ECCENTRICITY_MEAN,
    hornvale_astronomy::facts::OBLIQUITY_AMPLITUDE,
    hornvale_paleoclimate::facts::GLACIAL_MAXIMUM_ERA,
    hornvale_paleoclimate::facts::MAX_ICE_FRACTION,
    hornvale_paleoclimate::facts::FOSSIL_SHORELINE,
    hornvale_paleoclimate::facts::REFUGIUM,
    hornvale_paleoclimate::facts::FROST_RETREAT,
    hornvale_terrain::facts::TERRAIN_PIN,
];

/// The ledger's facts, restricted to predicates whose shape didn't move
/// under The Tongues, as debug strings. Iteration is commit order on both
/// sides (kernel `Ledger::iter`), so this is a faithful comparison.
fn filtered(world: &World) -> Vec<String> {
    world
        .ledger
        .iter()
        .filter(|f| !CHANGED_OR_NEW_PREDICATES.contains(&f.predicate.as_str()))
        .map(|f| format!("{f:?}"))
        .collect()
}

/// Every entity id bearing `predicate`, sorted — used below to compare the
/// settlement/belief entity *sets* without caring about fact order.
fn entity_ids_with(world: &World, predicate: &str) -> Vec<u64> {
    let mut ids: Vec<u64> = world.ledger.find(predicate).map(|f| f.subject.0).collect();
    ids.sort_unstable();
    ids
}

/// Every `name`-fact's text, in commit order — settlement names today (the
/// only entities carrying the bare `name` predicate).
fn name_texts(world: &World) -> Vec<String> {
    world
        .ledger
        .find(hornvale_kernel::NAME)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect()
}

fn default_generated_seed_42() -> World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// (1) Entity structure: the same settlement + belief entity ids, and every
/// non-name, non-belief-content fact byte-identical to the pre-Tongues
/// fixture (Task 2) — the language layer changes name TEXT and belief-fact
/// SHAPE, never the entity graph or anything else it touches. Also confirms
/// the thing that DOES change actually changed (names regenerated), so this
/// test cannot pass by accident if the seam were never wired.
#[test]
fn entity_structure_and_non_linguistic_facts_match_the_pre_tongues_fixture() {
    let fixture: World =
        serde_json::from_str(include_str!("fixtures/pre-tongues-seed-42-world.json"))
            .expect("fixture parses");

    // The fixture froze a level-5 world (Crust's `--globe-level` pin did not
    // exist yet); pin the same level here so this keystone keeps comparing
    // like-for-like terrain rather than confounding the naming-layer
    // invariant it checks with the unrelated Crust terrain epoch (spec §5,
    // §9 — a `GLOBE_LEVEL` bump is deliberately a different world).
    let world = build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins {
            globe_level: Some(5),
            ..Default::default()
        },
        &SettlementPins::default(),
    )
    .unwrap();
    // Round-trip through the save format, exactly as `hornvale new`/`almanac`
    // do and as `eyes_identity.rs`/`species_identity.rs` compare (serde_json's
    // float round-trip quirk — see their comments for the full rationale).
    let world = World::from_json(&world.to_json()).expect("world round-trips through JSON");

    assert_eq!(
        entity_ids_with(&world, hornvale_settlement::IS_SETTLEMENT),
        entity_ids_with(&fixture, hornvale_settlement::IS_SETTLEMENT),
        "settlement entity ids diverged from the pre-Tongues fixture"
    );
    assert_eq!(
        entity_ids_with(&world, hornvale_religion::IS_BELIEF),
        entity_ids_with(&fixture, hornvale_religion::IS_BELIEF),
        "belief entity ids diverged from the pre-Tongues fixture"
    );

    assert_eq!(
        filtered(&world),
        filtered(&fixture),
        "a non-linguistic fact diverged from the pre-Tongues fixture — only \
         name text and belief-fact shape may move under The Tongues"
    );

    assert_ne!(
        name_texts(&world),
        name_texts(&fixture),
        "settlement names must regenerate under The Tongues, not reproduce \
         the fixture's byte-identical text — the whole point of this campaign"
    );
}

/// (2) Determinism (constitutional, beneath every campaign): same seed, same
/// pins → byte-identical world JSON.
#[test]
fn seed_42_builds_byte_identically_across_two_runs() {
    let a = default_generated_seed_42().to_json();
    let b = default_generated_seed_42().to_json();
    assert_eq!(a, b, "same seed + pins must yield byte-identical worlds");
}

/// (3) De-facto uniqueness (the Task-9 owner decision, spec §8/§9): a name
/// is a pure, single deterministic draw per `(seed, species, kind, salt)`
/// with no re-draw and no shared "used" set, so world-wide uniqueness is
/// NOT guaranteed — only empirically likely, given the vast phonology name
/// space (a low collision rate is measured as a calibration in Task 12, not
/// asserted here as an invariant). This test asserts the empirical fact for
/// seed 42, to catch a gross regression (e.g. every name collapsing to one
/// string), not as a structural guarantee.
#[test]
fn seed_42_settlement_names_are_not_degenerate() {
    // Since The Words (Task 9), a name compounds over its own species'
    // small site-concept vocabulary (its biome, its people's presiding
    // belief) at the `/v2` glossed epoch, rather than drawing a free stem
    // from the vast phonology name space this test named for at The
    // Tongues — so world-wide uniqueness, never guaranteed even then, is
    // meaningfully less de-facto now ("glossed compounds shrink the name
    // space" is The Words' own documented tradeoff, spec §9, re-measured
    // honestly as a collision-rate calibration in that campaign's Task
    // 12). This only guards against total collapse (every settlement
    // sharing one name).
    let world = default_generated_seed_42();
    let names: Vec<String> = hornvale_settlement::all_settlements(&world)
        .iter()
        .map(|v| v.name.clone())
        .collect();
    assert!(!names.is_empty(), "seed 42 should place settlements");
    let unique: std::collections::BTreeSet<&String> = names.iter().collect();
    assert!(
        unique.len() > 1,
        "seed 42 must not collapse every settlement onto a single glossed name"
    );
}

/// The syllable pools The Tongues deleted (frozen copies — the source pools
/// themselves are gone): goblin's old settlement pool
/// (`domains/settlement`, pre-Tongues `SYLLABLES`) and kobold's old species
/// pool (`domains/species`, pre-Tongues `KOBOLD_SYLLABLES`).
const DELETED_GOBLIN_SYLLABLES: [&str; 10] = [
    "zag", "gru", "mok", "nar", "bol", "ish", "rak", "ug", "tor", "gna",
];
const DELETED_KOBOLD_SYLLABLES: [&str; 10] = [
    "zik", "thur", "kra", "ssk", "vex", "mir", "dak", "usz", "pli", "kek",
];

/// (4) Non-English: no generated proper noun — settlement name, deity name,
/// or epithet — reproduces a deleted syllable-pool word (case-insensitive
/// exact match). The new phonology engine draws from a wholly different
/// segment/phonotactic system than the retired ten-syllable pools, so this
/// also stands in for "the stopgap is really gone, not just unused code."
#[test]
fn no_generated_name_matches_a_deleted_syllable_pool_word() {
    let world = default_generated_seed_42();

    let text_facts = |predicate: &str| -> Vec<String> {
        world
            .ledger
            .find(predicate)
            .filter_map(|f| match &f.object {
                Value::Text(t) => Some(t.clone()),
                _ => None,
            })
            .collect()
    };

    let mut names: Vec<String> = hornvale_settlement::all_settlements(&world)
        .iter()
        .map(|v| v.name.clone())
        .collect();
    names.extend(text_facts(hornvale_religion::DEITY_NAME));
    names.extend(text_facts(hornvale_religion::DEITY_EPITHET));
    assert!(!names.is_empty(), "seed 42 should mint names and beliefs");

    let deleted: Vec<&str> = DELETED_GOBLIN_SYLLABLES
        .iter()
        .chain(DELETED_KOBOLD_SYLLABLES.iter())
        .copied()
        .collect();
    for name in &names {
        let lower = name.to_lowercase();
        assert!(
            !deleted.contains(&lower.as_str()),
            "generated name {name:?} exactly matches a deleted syllable-pool \
             word — the old stopgap must be gone, not merely unused"
        );
    }
}

/// (5) Pin-isolation: `--species goblin` produces the same goblin settlement
/// name AND population as the unpinned world for a cell goblins hold in
/// both — and here the name holds **by construction** (spec §8), not
/// incidentally: a name is a pure function of `(seed, species, kind, salt)`
/// where `salt` is the settlement's own cell id, with no re-draw and no
/// shared "used" set threaded through naming, so pinning which OTHER
/// species place can never perturb it. Population isolation is the
/// unchanged pre-Tongues guarantee (settlement's own draw, untouched by
/// this campaign). Ports `species_identity.rs`'s pin-isolation test
/// (retired — this is now its only home) and extends it to names.
#[test]
fn species_goblin_pin_reproduces_the_same_goblin_settlement_names() {
    let unpinned = default_generated_seed_42();
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

    let cell_of = |world: &World, id: EntityId| -> u32 {
        match world.ledger.value_of(id, hornvale_settlement::CELL_ID) {
            Some(Value::Number(n)) => *n as u32,
            other => panic!("settlement {id:?} has no numeric cell-id fact: {other:?}"),
        }
    };
    let settlement_on_cell = |world: &World, cell: u32| {
        world
            .ledger
            .find(hornvale_settlement::IS_SETTLEMENT)
            .map(|f| f.subject)
            .find(|id| cell_of(world, *id) == cell)
    };

    let pinned_goblins: Vec<EntityId> = pinned
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

    let name_of = |world: &World, id: EntityId| -> String {
        world
            .ledger
            .text_of(id, hornvale_kernel::NAME)
            .expect("a settlement has a name")
            .to_string()
    };
    assert_eq!(
        name_of(&pinned, pinned_id),
        name_of(&unpinned, unpinned_id),
        "pinning --species goblin shifted the settlement name on a cell \
         goblins hold in both worlds — names must be pin-isolated by \
         construction (spec §8)"
    );

    let population_of = |world: &World, id: EntityId| -> u32 {
        match world.ledger.value_of(id, hornvale_settlement::POPULATION) {
            Some(Value::Number(n)) => *n as u32,
            other => panic!("settlement {id:?} has no numeric population fact: {other:?}"),
        }
    };
    assert_eq!(
        population_of(&pinned, pinned_id),
        population_of(&unpinned, unpinned_id),
        "pinning --species goblin shifted the population on a cell goblins \
         hold in both worlds"
    );
}
