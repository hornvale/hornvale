//! `parent-of` and `kin-of`: kinship as a genesis fact (spec §4.3, decision
//! 0578).
//!
//! `domains/history::descent` has always computed `Kinship` between a
//! founder and their forebear; until this campaign nothing committed it.
//! `windows/worldgen::person_promote::promote` now resolves, for every
//! promoted founder, whether the community it descended from was ALSO
//! promoted — through entity identity (`records[i].founded_from`, an
//! `EntityId`), never through `founder_of`'s `RoleHandle`, which collides on
//! ~3.5% of seed 42's occupations (Task 1's ledger entry #6) and would
//! misattribute roughly 1 in 100 forebear edges if used to match promoted
//! founders.

use hornvale_astronomy::SkyPins;
use hornvale_history::record::Founding;
use hornvale_kernel::{EntityId, Seed, Value, World};
use hornvale_person::{KIN_OF, PARENT_OF};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::person_promote::select_founders;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, SkyChoice, WorldComponents, build_world_to, forebear_of,
    occupation_records,
};
use std::collections::BTreeMap;

fn seed42() -> World {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    build_world_to(
        Seed(42),
        &SkyPins::default(),
        SkyChoice::Generated,
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds")
}

/// The promoted person who founded `community`, by reading `person-founded`
/// back off the ledger — the same entity-identity route `promote` itself
/// uses, never a `RoleHandle`.
fn person_of_community(world: &World, community: EntityId) -> Option<EntityId> {
    world
        .ledger
        .find(hornvale_person::PERSON_FOUNDED)
        .find(|f| f.object == Value::Entity(community))
        .map(|f| f.subject)
}

#[test]
fn a_promoted_forebear_yields_a_parent_of_fact_naming_that_entity() {
    let w = seed42();
    let records = occupation_records(&w);
    let cast = select_founders(&records).remembered;
    let community_to_cast: BTreeMap<EntityId, usize> = cast
        .iter()
        .enumerate()
        .map(|(i, f)| (f.community, i))
        .collect();

    let mut found = false;
    for f in &cast {
        let Founding::From(mother) = records[f.occupation].founded_from else {
            continue;
        };
        if !community_to_cast.contains_key(&mother) {
            continue;
        }
        let Some((_, hornvale_history::descent::Kinship::Ancestor(_))) =
            forebear_of(&w, f.community)
        else {
            continue;
        };
        let person =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        let parent = person_of_community(&w, mother).expect("the mother founder is promoted too");
        assert_eq!(
            w.ledger.value_of(person, PARENT_OF),
            Some(&Value::Entity(parent)),
            "founder of {:?} must carry parent-of naming its promoted forebear",
            f.community
        );
        assert_eq!(
            w.ledger.value_of(person, KIN_OF),
            None,
            "an Ancestor edge must not also carry kin-of"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted-ancestor edge — spec §4.3's reference reads 93 at this seed"
    );
}

#[test]
fn an_unpromoted_forebear_yields_no_parent_of_fact() {
    let w = seed42();
    let records = occupation_records(&w);
    let cast = select_founders(&records).remembered;
    let community_to_cast: BTreeMap<EntityId, usize> = cast
        .iter()
        .enumerate()
        .map(|(i, f)| (f.community, i))
        .collect();

    let mut found = false;
    for f in &cast {
        let Founding::From(mother) = records[f.occupation].founded_from else {
            continue;
        };
        if community_to_cast.contains_key(&mother) {
            continue; // the forebear WAS promoted — not this case
        }
        let person =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        assert_eq!(
            w.ledger.value_of(person, PARENT_OF),
            None,
            "a founder whose forebear was never promoted must carry no parent-of — \
             the ledger says what is remembered (spec §4.3)"
        );
        assert_eq!(w.ledger.value_of(person, KIN_OF), None);
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted-founder-with-unpromoted-forebear — spec §4.3's \
         reference reads 76 at this seed"
    );
}

#[test]
fn a_root_founder_carries_neither_predicate() {
    let w = seed42();
    let records = occupation_records(&w);
    let cast = select_founders(&records).remembered;

    let mut found = false;
    for f in &cast {
        if !matches!(records[f.occupation].founded_from, Founding::Genesis(_)) {
            continue;
        }
        let person =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        assert_eq!(
            w.ledger.value_of(person, PARENT_OF),
            None,
            "a root founder (no occ-founded-from) must carry no parent-of"
        );
        assert_eq!(
            w.ledger.value_of(person, KIN_OF),
            None,
            "a root founder must carry no kin-of either"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted root founder — spec §4.3's reference reads 35 at this seed"
    );
}

#[test]
fn a_sibling_edge_renders_as_kin_of_never_as_descent() {
    let w = seed42();
    let records = occupation_records(&w);
    let cast = select_founders(&records).remembered;
    let community_to_cast: BTreeMap<EntityId, usize> = cast
        .iter()
        .enumerate()
        .map(|(i, f)| (f.community, i))
        .collect();

    let mut found = false;
    for f in &cast {
        let Founding::From(mother) = records[f.occupation].founded_from else {
            continue;
        };
        if !community_to_cast.contains_key(&mother) {
            continue;
        }
        let Some((_, hornvale_history::descent::Kinship::Sibling)) = forebear_of(&w, f.community)
        else {
            continue;
        };
        let person =
            person_of_community(&w, f.community).expect("a cast member is always promoted");
        let sibling = person_of_community(&w, mother).expect("the mother founder is promoted");
        assert_eq!(
            w.ledger.value_of(person, KIN_OF),
            Some(&Value::Entity(sibling)),
            "a Sibling edge must commit kin-of, naming the contemporary forebear"
        );
        assert_eq!(
            w.ledger.value_of(person, PARENT_OF),
            None,
            "a Sibling edge must NEVER render as descent (spec §4.3) — it must not \
             also carry parent-of"
        );
        found = true;
        break;
    }
    assert!(
        found,
        "seed 42 has no promoted-sibling edge — descent_graph.rs's own test expects \
         ~13% of edges to resolve as Sibling"
    );
}

/// Save-format contract (spec §4.3, step 2 item 5): resolving and committing
/// `parent-of`/`kin-of` consumes no `Stream` draw.
///
/// `promote()`'s only draw is `Namer::new(&world.seed, ...).name(...)`, once
/// per founder, in the FIRST pass — byte-for-byte unchanged by this
/// campaign. The kinship pass is a SECOND pass that runs strictly after
/// `hornvale_person::genesis` has already returned every id, and everything
/// it touches (`records`, already materialized before either pass starts;
/// `community_to_cast`, a plain map over in-memory `Founder` values;
/// `forebear_of`, a total function of already-committed founding years and
/// the species allometry table per its own doc) never reaches
/// `hornvale_kernel::Seed` or `Stream`. Demonstrated by determinism: two
/// independent `BuildDepth::Full` builds of the same seed — which exercises
/// the kinship pass in full — commit byte-identical ledgers. A stray or
/// reordered draw anywhere in `promote()` would perturb this exactly as
/// readily as a real defect in the resolution logic would.
#[test]
fn kinship_resolution_draws_no_stream() {
    let a = seed42();
    let b = seed42();
    assert_eq!(
        serde_json::to_string(&a.ledger).unwrap(),
        serde_json::to_string(&b.ledger).unwrap(),
        "seed 42 must build a byte-identical ledger twice, kinship facts included — \
         a Stream draw hidden anywhere in promote() would break this"
    );
}
