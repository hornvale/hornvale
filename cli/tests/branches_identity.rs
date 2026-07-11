//! THE structural guard for the post-Branches world (spec §6), superseding
//! `words_identity.rs`/`tongues_identity.rs`. Those keystones asserted
//! "campaign X changed only names, never structure" — an invariant true of
//! every campaign up to and including The Tongues, because each of those
//! campaigns changed how a fact was drawn, never who a settlement's people
//! were. The Branches is different by design: it adds two new peoples
//! (hobgoblin, bugbear — cognate members of the goblinoid family alongside
//! goblin, descending from a shared drawn proto-goblinoid vector) into the
//! same settlement-placement pass that already held goblin and kobold. A
//! bigger roster competing for the same cells shifts WHO wins WHICH cell
//! world-wide, so goblin's own placement — and therefore its own committed
//! names — legitimately changed too. Asserting "goblin structure/facts
//! unchanged" here would be FALSE and this file must never do it.
//!
//! What actually stayed true, and is asserted below instead:
//!
//! 1. `seed_42_is_deterministic_across_two_builds` — same seed, same pins,
//!    byte-identical world JSON.
//! 2. `kobold_lexicon_is_the_singleton_build_lexicon_call` — kobold (the
//!    one people outside the goblinoid family — the unrelated outgroup)
//!    still resolves through the direct, unmediated `build_lexicon` call
//!    with `family == species` and `proto_ph == ph`: the pre-family
//!    singleton mechanism, unperturbed by the family machinery The
//!    Branches introduced for its three goblinoid daughters.
//! 3. `kobold_phonology_is_a_pure_function_of_seed_and_envelope` —
//!    kobold's drawn phonology is a pure function of `(seed, species,
//!    envelope)` alone, exactly reproducible outside a built world and
//!    unperturbed by which OTHER species share the roster.
//! 4. `hobgoblin_and_bugbear_are_present` — the world actually contains
//!    hobgoblin and bugbear species entities: the new peoples this
//!    campaign exists to add are really there.
//! 5. `goblin_names_are_rebaselined_not_frozen` — goblin's committed
//!    settlement names are non-empty AND DIFFER from the pre-Branches
//!    fixture's (frozen at Task 2, back when only goblin and kobold
//!    competed for cells) — proof the re-derivation actually happened, not
//!    a no-op merge, and (since the founder floor, MAP-22 K=1) proof
//!    goblin still places real settlements in the shared four-people
//!    world rather than being boxed out to zero.
//! 6. `every_goblinoid_words_root_is_in_its_own_daughters_inventory` — the
//!    family-consistency guard (Task 6, mirrored here from
//!    `windows/worldgen/src/lib.rs`'s own
//!    `every_goblinoid_word_is_in_its_inventory`): every goblinoid
//!    daughter's every `Root` lexicon entry's evolved/nativized form draws
//!    only from that daughter's own phonology inventory, never a cousin's.
//! 7. `hobgoblin_and_bugbear_each_hold_at_least_one_committed_settlement_name`
//!    — the founder floor's world-level guarantee (settlement's
//!    founder-reservation pass, MAP-22 allocation-at-K=1): every placed
//!    people, however weak its suitability score against its
//!    competitors, wins at least one cell and therefore commits at least
//!    one settlement name. Pinned here as an invariant of the shipped
//!    world, not merely of the algorithm's unit tests.

use hornvale_kernel::{Seed, Value, World};
use hornvale_language::LexEntry;
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};

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

/// Every settlement `name`-fact's text among settlements peopled by
/// `species`, sorted — lets a comparison of "goblin's own committed
/// output" ignore placement/commit-order shuffling from the bigger
/// roster. Reuses `words_identity.rs`/`tongues_identity.rs`'s
/// entity-id-then-fact-lookup idiom.
fn settlement_names_of_species(world: &World, species: &str) -> Vec<String> {
    let mut names: Vec<String> = world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .filter(|id| hornvale_species::species_of(world, *id).as_deref() == Some(species))
        .filter_map(|id| world.ledger.text_of(id, hornvale_kernel::NAME))
        .map(str::to_string)
        .collect();
    names.sort();
    names
}

/// Every committed `species-name` fact's text, sorted — the world's
/// roster of actually-placed peoples.
fn species_names(world: &World) -> Vec<String> {
    let mut names: Vec<String> = world
        .ledger
        .find(hornvale_species::SPECIES_NAME)
        .filter_map(|f| match &f.object {
            Value::Text(t) => Some(t.clone()),
            _ => None,
        })
        .collect();
    names.sort();
    names
}

/// (1) Determinism (constitutional, beneath every campaign): same seed,
/// same pins → byte-identical world JSON.
#[test]
fn seed_42_is_deterministic_across_two_builds() {
    let a = default_generated_seed_42().to_json();
    let b = default_generated_seed_42().to_json();
    assert_eq!(a, b, "same seed + pins must yield byte-identical worlds");
}

/// (2) Singleton mechanism (kobold): `lexicon_of` for the one people
/// outside the goblinoid family must still resolve through the direct,
/// pre-family `build_lexicon` call — `family == species`, `proto_ph ==
/// ph` — end to end. Mirrors worldgen's own
/// `kobold_lexicon_mechanism_is_stable_given_fixed_exposures`
/// (`windows/worldgen/src/lib.rs`), re-checked here from outside the
/// crate as a structural guard.
#[test]
fn kobold_lexicon_is_the_singleton_build_lexicon_call() {
    let world = default_generated_seed_42();
    let kph = hornvale_worldgen::language_of(&world, "kobold");
    let kex = hornvale_worldgen::exposure_of(&world, "kobold")
        .expect("exposure_of(kobold) must succeed for a placed species");
    let kdaughters =
        hornvale_worldgen::family_daughters(&world, &hornvale_worldgen::default_roster(), "kobold");
    let direct = hornvale_language::build_lexicon(
        &world.seed,
        "kobold",
        "kobold",
        &kph,
        &kph,
        &kex,
        &kdaughters,
    );
    assert_eq!(
        hornvale_worldgen::lexicon_of(&world, "kobold").expect("lexicon_of(kobold) must succeed"),
        direct,
        "kobold — the one unrelated outgroup, a singleton family — must \
         still resolve through the direct build_lexicon call with \
         family == species and proto_ph == ph; the family machinery The \
         Branches introduced for the goblinoid daughters must never \
         perturb the one people outside it"
    );
}

/// (3) Kobold draw-path stability: `language_of` is a pure function of
/// `(seed, species, envelope)` alone — reproducible directly via
/// `draw_phonology`, unperturbed by which OTHER species now share the
/// roster (hobgoblin and bugbear joining changes nothing about how
/// kobold's own phonology is drawn).
#[test]
fn kobold_phonology_is_a_pure_function_of_seed_and_envelope() {
    let world = default_generated_seed_42();
    let registry = hornvale_species::registry();
    let kobold_articulation = &registry["kobold"].articulation;
    let envelope = hornvale_worldgen::envelope_of(kobold_articulation);
    let direct = hornvale_language::draw_phonology(&world.seed, "kobold", &envelope);
    assert_eq!(
        hornvale_worldgen::language_of(&world, "kobold"),
        direct,
        "kobold phonology must remain a pure function of (seed, species, \
         envelope) alone, unchanged by the roster growing to four peoples"
    );
}

/// (4) New peoples present: the world actually contains hobgoblin and
/// bugbear species entities — the two peoples this campaign exists to
/// add are really there, not just theorized.
#[test]
fn hobgoblin_and_bugbear_are_present() {
    let world = default_generated_seed_42();
    let names = species_names(&world);
    assert!(
        names.iter().any(|n| n == "hobgoblin"),
        "seed 42 must place a hobgoblin species entity; got {names:?}"
    );
    assert!(
        names.iter().any(|n| n == "bugbear"),
        "seed 42 must place a bugbear species entity; got {names:?}"
    );
}

/// (5) Goblin re-baselined: goblin's committed settlement names are
/// non-empty AND DIFFER from the pre-Branches fixture's (frozen at Task
/// 2, when only goblin and kobold competed for cells) — proving both
/// that the re-derivation actually happened world-wide (not a no-op
/// merge) and that, since the founder floor (settlement's
/// founder-reservation pass, MAP-22 K=1), goblin still places real
/// settlements in the shared four-people world rather than being boxed
/// out to zero by a stronger competitor. This is the one comparison this
/// keystone makes against the OLD, two-peoples world, and the inequality
/// is deliberate: "goblin structure unchanged" would be false.
#[test]
fn goblin_names_are_rebaselined_not_frozen() {
    let fixture: World =
        serde_json::from_str(include_str!("fixtures/pre-branches-seed-42-world.json"))
            .expect("fixture parses");
    let world = default_generated_seed_42();

    let fixture_names = settlement_names_of_species(&fixture, "goblin");
    let world_names = settlement_names_of_species(&world, "goblin");

    assert!(
        !fixture_names.is_empty(),
        "the pre-Branches fixture should place at least one goblin settlement"
    );
    assert!(
        !world_names.is_empty(),
        "goblin must still place at least one settlement in the current \
         four-people world — a stronger competitor's suitability score \
         must never box a placed people out to zero"
    );
    assert_ne!(
        world_names, fixture_names,
        "goblin's committed settlement names must differ from the \
         pre-Branches fixture — adding hobgoblin and bugbear to the same \
         placement pass shifts which cells goblin wins world-wide, so its \
         own names re-derive too; byte-identity here would mean The \
         Branches never actually touched placement"
    );
}

/// (7) Founder-floor world invariant: hobgoblin and bugbear — the two
/// peoples weakest under the current suitability-scoring formula — each
/// still hold at least one committed settlement name in the shared,
/// unpinned seed-42 world. This is the founder floor's guarantee
/// (settlement's founder-reservation pass, MAP-22 allocation-at-K=1)
/// pinned as a fact about the shipped world, not merely about the
/// algorithm's own unit tests (`domains/settlement/src/placement.rs`).
#[test]
fn hobgoblin_and_bugbear_each_hold_at_least_one_committed_settlement_name() {
    let world = default_generated_seed_42();
    for species in ["hobgoblin", "bugbear"] {
        let names = settlement_names_of_species(&world, species);
        assert!(
            !names.is_empty(),
            "{species} must hold at least one committed settlement name \
             under the founder floor; got none"
        );
    }
}

/// (6) Family inventory-closure: every goblinoid daughter's every `Root`
/// lexicon entry's evolved/nativized form draws only from that
/// daughter's own phonology inventory — never a cousin's, and never a
/// stray proto segment that failed to nativize. Mirrors the Task 6
/// worldgen test `every_goblinoid_word_is_in_its_inventory`
/// (`windows/worldgen/src/lib.rs`), re-checked here from outside the
/// crate as a structural guard on the shipped goblinoid family
/// (goblin, hobgoblin, bugbear).
#[test]
fn every_goblinoid_words_root_is_in_its_own_daughters_inventory() {
    let world = default_generated_seed_42();
    let mut checked = 0;
    for species in ["goblin", "hobgoblin", "bugbear"] {
        let ph = hornvale_worldgen::language_of(&world, species);
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));
        for (concept, entry) in lex.entries() {
            if let LexEntry::Root { derivation, .. } = entry {
                checked += 1;
                assert!(
                    derivation.modern.iter().all(|s| ph.inventory.contains(s)),
                    "{species} concept {concept:?}: a nativized root must \
                     draw only from its own daughter's inventory, got \
                     {:?} against inventory {:?}",
                    derivation.modern,
                    ph.inventory
                );
            }
        }
    }
    assert!(
        checked > 0,
        "seed 42 should mint at least one Root lexicon entry across the \
         goblinoid family"
    );
}
