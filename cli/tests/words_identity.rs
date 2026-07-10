//! The Words structural-invariant keystone (spec §8/§9): the campaign that
//! made every generated proper noun a *true story* about the entity it
//! names — compounded over that entity's own site facts, via a real
//! lexicon with a real sound-change history — must not have disturbed
//! anything else. Seven invariants, each its own test:
//!
//! 1. `entity_graph_unchanged` — the entity graph and every non-linguistic
//!    fact match the pre-Words fixture (Task 2) exactly; only name text and
//!    the wholly-new `name-gloss` predicate may differ.
//! 2. `names_wellformed_and_glosses_true` — every committed `name-gloss` is
//!    a truthful composition of that entity's own re-derived site concepts.
//! 3. `every_concept_resolves_once` — every registered concept resolves to
//!    exactly one lexicon entry, for every species.
//! 4. `derivations_replay` — every `Root` entry's sound-change derivation
//!    replays byte-identically through `evolve`.
//! 5. `gaps_have_reasons` — every `Gap` carries a non-empty, recountable
//!    reason.
//! 6. `pin_isolation_holds` — `--species goblin` reproduces the same
//!    goblin settlement names (and glosses) as the unpinned world.
//! 7. `determinism_holds` — same seed, same pins, byte-identical worlds.

use hornvale_kernel::{EntityId, Phenomenon, Seed, Value, World};
use hornvale_language::{GapReason, LexEntry};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::collections::BTreeSet;

/// Predicates whose TEXT may differ from the pre-Words fixture (settlement
/// and deity proper nouns, freshly drawn at the `/v2` glossed epoch — Task
/// 9), plus the wholly new `name-gloss` predicate this campaign introduces.
/// Everything else — entity ids, every other fact — must be byte-identical
/// to the fixture: The Words changes what a name IS, never anything else
/// about the world it names. Same "superset, minus what's genuinely new"
/// pattern `tongues_identity.rs` (its own predecessor keystone) uses.
const CHANGED_OR_NEW_PREDICATES: [&str; 6] = [
    hornvale_kernel::NAME,
    hornvale_worldgen::NAME_GLOSS,
    hornvale_religion::DEITY_NAME,
    hornvale_religion::DEITY_NAME_IPA,
    hornvale_religion::DEITY_EPITHET,
    hornvale_religion::DEITY_EPITHET_IPA,
];

/// The ledger's facts, restricted to predicates whose shape didn't move
/// under The Words, as debug strings (which fold in the subject's entity
/// id, so this also catches an entity-id divergence on anything still
/// carrying a non-linguistic fact). Iteration is commit order on both
/// sides (kernel `Ledger::iter`), so this is a faithful comparison.
fn filtered(world: &World) -> Vec<String> {
    world
        .ledger
        .iter()
        .filter(|f| !CHANGED_OR_NEW_PREDICATES.contains(&f.predicate.as_str()))
        .map(|f| format!("{f:?}"))
        .collect()
}

/// Every entity id bearing `predicate`, sorted.
fn entity_ids_with(world: &World, predicate: &str) -> Vec<u64> {
    let mut ids: Vec<u64> = world.ledger.find(predicate).map(|f| f.subject.0).collect();
    ids.sort_unstable();
    ids
}

/// Every `name`-fact's text, in commit order.
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

/// (1) Entity structure: the same entity ids (via total minted count, plus
/// the settlement/belief subsets by name — the two kinds that carry a
/// generated proper noun) and every non-linguistic fact byte-identical to
/// the pre-Words fixture (Task 2). Also confirms the thing that DOES change
/// actually changed (names regenerated at the `/v2` epoch), so this test
/// cannot pass by accident if the seam were never wired.
#[test]
fn entity_graph_unchanged() {
    let fixture: World =
        serde_json::from_str(include_str!("fixtures/pre-words-seed-42-world.json"))
            .expect("fixture parses");

    let world = default_generated_seed_42();
    // Round-trip through the save format, exactly as `hornvale new`/
    // `almanac` do and as the predecessor keystone compares (serde_json's
    // float round-trip quirk).
    let world = World::from_json(&world.to_json()).expect("world round-trips through JSON");

    assert_eq!(
        world.ledger.max_entity_id(),
        fixture.ledger.max_entity_id(),
        "total minted entity count diverged from the pre-Words fixture"
    );
    assert_eq!(
        entity_ids_with(&world, hornvale_settlement::IS_SETTLEMENT),
        entity_ids_with(&fixture, hornvale_settlement::IS_SETTLEMENT),
        "settlement entity ids diverged from the pre-Words fixture"
    );
    assert_eq!(
        entity_ids_with(&world, hornvale_religion::IS_BELIEF),
        entity_ids_with(&fixture, hornvale_religion::IS_BELIEF),
        "belief entity ids diverged from the pre-Words fixture"
    );

    assert_eq!(
        filtered(&world),
        filtered(&fixture),
        "a non-linguistic fact diverged from the pre-Words fixture — only \
         name/deity-name/deity-epithet text and the new name-gloss \
         predicate may move under The Words"
    );

    assert_ne!(
        name_texts(&world),
        name_texts(&fixture),
        "settlement names must regenerate at the /v2 glossed epoch, not \
         reproduce the fixture's pre-Words text — the whole point of this \
         campaign"
    );
}

/// The concept a phenomenon kind glosses to — re-derived here the same way
/// worldgen's own (private) `phenomenon_concept` does, from the same public
/// phenomenon-kind constants it matches on. Duplicated rather than exposed
/// because this mapping is a composition-root judgment call (its own doc
/// comment says so), not a save-format contract; if worldgen's version ever
/// drifts from this one, this test starts failing on real seed-42 glosses,
/// which is exactly the point of a re-derivation keystone.
fn phenomenon_concept(phenomenon: &Phenomenon) -> Option<&'static str> {
    match phenomenon.kind.as_str() {
        hornvale_astronomy::CELESTIAL_BODY => {
            if phenomenon.description.contains("moon") {
                Some("moon")
            } else if phenomenon.description.contains("star") {
                Some("star")
            } else {
                Some("sun")
            }
        }
        hornvale_astronomy::SEASONAL_CYCLE => Some("day"),
        hornvale_astronomy::NIGHT_STAR => Some("star"),
        hornvale_climate::AMBIENT => Some("wind"),
        _ => None,
    }
}

/// A settlement's own re-derived site concepts: its committed biome fact
/// (which is already the concept id — `Biome::concept_name() ==
/// Biome::name()`, the same kebab-case string settlement genesis commits,
/// domains/climate/src/biome.rs) plus the presiding phenomenon concept its
/// species observes from THIS settlement's own vantage — the settlement's
/// committed coordinates cull the sky (SEQ-5), the same per-entity
/// observation `build_world_with_roster` makes at the settlement's cell
/// right before drawing its glossed name (windows/worldgen/src/lib.rs,
/// `site_concepts = vec![biome_concept]; site_concepts.extend(presiding)`).
/// Re-deriving from the fully-built world is observationally identical to
/// the in-progress placement pass's own read: the pass observes from the
/// same coordinate the committed entity now carries as its lat/lon facts.
fn settlement_site_concepts(world: &World, id: EntityId) -> Vec<String> {
    let biome = world
        .ledger
        .text_of(id, hornvale_settlement::BIOME)
        .expect("every settlement has a biome fact")
        .to_string();
    let species =
        hornvale_species::species_of(world, id).expect("every settlement has a species fact");
    let phenomena = hornvale_worldgen::observed_phenomena_as_at(
        world,
        &hornvale_worldgen::default_roster(),
        &species,
        id,
    )
    .expect("observed_phenomena_as_at must succeed for a placed species");
    let mut concepts = vec![biome];
    if let Some(concept) = phenomena.first().and_then(phenomenon_concept) {
        concepts.push(concept.to_string());
    }
    concepts
}

/// A deity belief's own re-derived site concepts, via the public
/// `deity_site_concepts` (Task 9 made it pub for exactly this purpose): the
/// belief's phenomenon is reconstructed by matching its position among its
/// community's beliefs (`beliefs_held_by`, commit order) against that
/// community's species' own observed phenomena in the same order —
/// `hornvale_religion::genesis`'s own contract guarantees `members ==
/// &phenomena[..take]`, minted in that order.
fn deity_site_concepts_of(world: &World, id: EntityId) -> Vec<String> {
    let community = match world.ledger.value_of(id, hornvale_religion::HELD_BY) {
        Some(Value::Entity(e)) => *e,
        other => panic!("belief {id:?} has no held-by community fact: {other:?}"),
    };
    let beliefs = hornvale_religion::beliefs_held_by(world, community);
    let idx = beliefs
        .iter()
        .position(|b| b.id == id)
        .expect("a belief must be among its own held-by community's beliefs");
    let species = hornvale_species::species_of(world, community)
        .expect("a belief's community has a species fact");
    // Deity naming observes from the world's first place, hemisphere-culled
    // (SEQ-4/SEQ-5) — the same slice religion's genesis consumed — so the
    // re-derivation must use exactly that vantage, not a position-free one.
    let phenomena = hornvale_worldgen::observed_phenomena_as(world, &species)
        .expect("observed_phenomena_as must succeed for a placed species");
    let phenomenon = phenomena
        .get(idx)
        .expect("a belief's phenomenon must exist at its own commit-order index");
    hornvale_worldgen::deity_site_concepts(phenomenon, beliefs[idx].sentiment)
        .into_iter()
        .map(str::to_string)
        .collect()
}

/// Every gloss `glossed_name` could truthfully produce from `concepts`
/// (itself, `chosen.join("-")` over 1 or 2 site concepts — Task 9): each
/// concept alone, plus every ordered pair joined with `"-"`. Compared as
/// whole composed strings, never split, because a component may itself
/// contain a hyphen (e.g. `"kobold-kind"`), which would make naive
/// splitting ambiguous.
fn candidate_glosses(concepts: &[String]) -> BTreeSet<String> {
    let mut set = BTreeSet::new();
    for c in concepts {
        set.insert(c.clone());
    }
    for i in 0..concepts.len() {
        for j in 0..concepts.len() {
            if i != j {
                set.insert(format!("{}-{}", concepts[i], concepts[j]));
            }
        }
    }
    set
}

/// (2) Gloss truthfulness (spec §8/§9): every committed `name-gloss` fact
/// is one of the candidate compositions of that SAME entity's own
/// re-derived site concepts — never a concept from anywhere else in the
/// world.
#[test]
fn names_wellformed_and_glosses_true() {
    assert_glosses_true(&default_generated_seed_42(), "seed 42");
}

/// The Firm-Ground-II merge regression probe: seed 8 is the seed where
/// hemisphere culling (SEQ-5) first diverged a settlement's own sky from
/// the position-free stand-in glossed naming originally observed, making a
/// committed gloss untruthful. Naming now observes settlements from their
/// own cells and deities from the species' position-free sky; this seed
/// guards that split.
#[test]
fn names_wellformed_and_glosses_true_on_seed_8() {
    let world = build_world(
        Seed(8),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .expect("seed 8 must build");
    assert_glosses_true(&world, "seed 8");
}

fn assert_glosses_true(world: &World, label: &str) {
    let mut checked_settlements = 0;
    for f in world.ledger.find(hornvale_settlement::IS_SETTLEMENT) {
        let id = f.subject;
        let Some(gloss) = world.ledger.text_of(id, hornvale_worldgen::NAME_GLOSS) else {
            continue;
        };
        checked_settlements += 1;
        let concepts = settlement_site_concepts(world, id);
        let candidates = candidate_glosses(&concepts);
        assert!(
            candidates.contains(gloss),
            "{label}: settlement {id:?} gloss {gloss:?} is not a truthful \
             composition of its own re-derived site concepts {concepts:?} \
             (candidates: {candidates:?})"
        );
    }
    assert!(
        checked_settlements > 0,
        "{label} should gloss at least one settlement"
    );

    let mut checked_deities = 0;
    for f in world.ledger.find(hornvale_religion::IS_BELIEF) {
        let id = f.subject;
        let Some(gloss) = world.ledger.text_of(id, hornvale_worldgen::NAME_GLOSS) else {
            continue;
        };
        checked_deities += 1;
        let concepts = deity_site_concepts_of(world, id);
        let candidates = candidate_glosses(&concepts);
        assert!(
            candidates.contains(gloss),
            "{label}: deity belief {id:?} gloss {gloss:?} is not a truthful \
             composition of its own re-derived site concepts {concepts:?} \
             (candidates: {candidates:?})"
        );
    }
    assert!(
        checked_deities > 0,
        "{label} should gloss at least one deity"
    );
}

/// (3) Coverage (spec §7): for every placed species, every concept the
/// world's registry knows about resolves to exactly one lexicon entry —
/// `build_lexicon`'s own invariant, re-checked from outside the crate.
#[test]
fn every_concept_resolves_once() {
    let world = default_generated_seed_42();
    let concept_count = world.registry.concepts().count();
    assert!(concept_count > 0, "the registry should hold concepts");

    for def in hornvale_worldgen::default_roster() {
        let lex = hornvale_worldgen::lexicon_of(&world, def.name)
            .unwrap_or_else(|e| panic!("lexicon_of({}) failed: {e:?}", def.name));
        for concept in world.registry.concepts() {
            assert!(
                lex.entry(&concept.name).is_some(),
                "species {} has no lexicon entry for registered concept {:?}",
                def.name,
                concept.name
            );
        }
        assert_eq!(
            lex.entries().count(),
            concept_count,
            "species {}'s lexicon fabricated or dropped entries relative to \
             the registry",
            def.name
        );
    }
}

/// (4) Replay (spec §9): every `Root` entry's recorded sound-change
/// derivation replays byte-identically through `evolve` given the same
/// proto-root and the species' own drawn cascade.
#[test]
fn derivations_replay() {
    let world = default_generated_seed_42();
    let mut checked = 0;

    for def in hornvale_worldgen::default_roster() {
        let species = def.name;
        let ph = hornvale_worldgen::language_of(&world, species);
        let cascade = hornvale_language::draw_cascade(&world.seed, species);
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));

        for (concept, entry) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = entry {
                let replayed = hornvale_language::evolve(&derivation.proto, &cascade, &ph);
                assert_eq!(
                    replayed.modern, derivation.modern,
                    "species {species} concept {concept:?} did not replay \
                     identically through evolve"
                );
                checked += 1;
            }
        }
    }
    assert!(
        checked > 0,
        "seed 42 should mint at least one Root lexicon entry across the roster"
    );
}

/// (5) Recountability (spec §7): every `Gap` entry carries a non-empty
/// reason that names a concrete fact (a settlement's missing biome/sea
/// exposure, a missing compound recipe or component) or a vector dimension
/// (a perception ladder rank/depth, night-vision) — never a placeholder.
#[test]
fn gaps_have_reasons() {
    let world = default_generated_seed_42();
    let mut checked = 0;
    // Substrings drawn directly from worldgen's `experiential_reason`/
    // `perceptual_reason` and lexicon's own composed compound-gap messages
    // — every reason `build_lexicon` can produce matches at least one.
    let markers = [
        "hue",
        "luminance",
        "night-vision",
        "no settlement",
        "has no exposure",
        "no compound recipe",
        "needs",
    ];

    for def in hornvale_worldgen::default_roster() {
        let lex = hornvale_worldgen::lexicon_of(&world, def.name)
            .unwrap_or_else(|e| panic!("lexicon_of({}) failed: {e:?}", def.name));
        for (concept, entry) in lex.entries() {
            if let LexEntry::Gap { reason } = entry {
                checked += 1;
                let text = match reason {
                    GapReason::Experiential(s) => s,
                    GapReason::Perceptual(s) => s,
                };
                assert!(
                    !text.is_empty(),
                    "species {} concept {concept:?} has an empty gap reason",
                    def.name
                );
                assert!(
                    markers.iter().any(|m| text.contains(m)),
                    "species {} concept {concept:?} gap reason {text:?} \
                     names neither a recognizable fact nor a vector dimension",
                    def.name
                );
            }
        }
    }
    assert!(
        checked > 0,
        "seed 42 should leave at least one concept a gap for some species"
    );
}

/// (6) Pin-isolation (spec §8): `--species goblin` reproduces the exact
/// same goblin settlement name — and, when both worlds glossed it, the
/// exact same gloss — as the unpinned world, on a cell goblins hold in
/// both. Names (and the lexicon/gloss draw behind them) are pure functions
/// of `(seed, species, kind, salt, site, lexicon)` with no shared "used"
/// set and no dependence on which OTHER species a world happens to place,
/// so pinning which other species place can never perturb them. Extends
/// the predecessor keystone's pin test (`tongues_identity.rs`) to the
/// glossed epoch.
#[test]
fn pin_isolation_holds() {
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
         goblins hold in both worlds — glossed names must stay pin-isolated \
         by construction under The Words too (spec §8)"
    );

    let gloss_of = |world: &World, id: EntityId| -> Option<String> {
        world
            .ledger
            .text_of(id, hornvale_worldgen::NAME_GLOSS)
            .map(str::to_string)
    };
    assert_eq!(
        gloss_of(&pinned, pinned_id),
        gloss_of(&unpinned, unpinned_id),
        "pinning --species goblin shifted the settlement gloss on a cell \
         goblins hold in both worlds"
    );
}

/// (7) Determinism (constitutional, beneath every campaign): same seed,
/// same pins → byte-identical world JSON.
#[test]
fn determinism_holds() {
    let a = default_generated_seed_42().to_json();
    let b = default_generated_seed_42().to_json();
    assert_eq!(a, b, "same seed + pins must yield byte-identical worlds");
}
