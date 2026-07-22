//! Integration coverage recovered from the retired `words_identity.rs` /
//! `tongues_identity.rs` keystones (The Branches, final-review Finding 1).
//! When those files were superseded by `branches_identity.rs`, four
//! INTEGRATION-level guards were not re-asserted anywhere. This file
//! restores them, adapted to the four-people world (goblin, hobgoblin,
//! bugbear, kobold):
//!
//! 1. `species_pin_isolation_holds_for_*` — a `--species <X>` pin is a
//!    **deterministic restricted roster** (decision-ledger #49, reframed
//!    after the niche-differentiated-K cutover): building the same pin
//!    twice yields a byte-identical world. The stack is competitive, so
//!    pinning legitimately changes the pinned species' own density versus
//!    the unpinned world (its competitors are gone) — that population-
//!    equality contract is retired; determinism of the restricted roster
//!    is what remains and is asserted here. Covered for goblin AND a
//!    goblinoid daughter (hobgoblin), because the family machinery The
//!    Branches introduced must not have threaded any roster-dependent
//!    nondeterminism into a daughter's naming path.
//! 2. `derivations_replay` — for every roster species' every `Root`
//!    lexicon entry, `evolve(derivation.proto, cascade, ph).modern ==
//!    derivation.modern`: the recorded etymology replays byte-identically.
//! 3. `every_concept_resolves_once` — every registered concept resolves to
//!    exactly one lexicon entry (`Root` | `Compound` | `Gap`) per species;
//!    no concept missing, none double-resolved.
//! 4. `gaps_have_reasons` — every `Gap` entry carries a non-empty,
//!    recountable reason naming a concrete fact or a vector dimension.

use hornvale_kernel::{EntityId, Seed, Value, World};
use hornvale_language::{GapReason, LexEntry};
use hornvale_worldgen::{SettlementPins, SkyChoice, build_world};
use std::collections::BTreeMap;

fn seed_42_with(settlement_pins: &SettlementPins) -> World {
    build_world(
        Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        settlement_pins,
    )
    .unwrap()
}

fn default_generated_seed_42() -> World {
    seed_42_with(&SettlementPins::default())
}

/// `species`' settlements keyed by their committed cell id — the stable
/// coordinate for comparing a pinned against an unpinned world, since a
/// bigger or smaller roster shuffles entity ids and commit order but never
/// which cell a settlement stands on.
fn settlements_by_cell(world: &World, species: &str) -> BTreeMap<u32, EntityId> {
    world
        .ledger
        .find(hornvale_settlement::IS_SETTLEMENT)
        .map(|f| f.subject)
        .filter(|id| hornvale_species::species_of(world, *id).as_deref() == Some(species))
        .map(|id| {
            let cell = match world.ledger.value_of(id, hornvale_settlement::CELL_ID) {
                Some(Value::Number(n)) => *n as u32,
                other => panic!("settlement {id:?} has no numeric cell-id fact: {other:?}"),
            };
            (cell, id)
        })
        .collect()
}

/// (1) Pin-isolation for one species, reframed under the niche-K
/// coexistence stack (decision-ledger #49): a species pin now selects a
/// **deterministic restricted roster**, not a population-preserving mask.
/// Because settlement genesis packs species competitively against a
/// shared per-cell capacity (`niche_per_species_k`), pinning `--species X`
/// removes X's competitors and legitimately changes X's own density on
/// cells it still holds — the old "population unchanged vs. unpinned"
/// contract no longer holds by construction, and asserting it would be
/// asserting a falsehood about the coexistence stack.
///
/// What must still hold, and does: **pin determinism**. The same
/// `(seed, --species X)` pin, built twice, produces byte-identical
/// worlds — the stream-consumption-order / save-format contract (spec §8)
/// survives the cutover even though pin-vs-unpinned equality does not.
/// Ports `words_identity.rs`'s `pin_isolation_holds` / `tongues_identity.rs`'s
/// `species_goblin_pin_reproduces_the_same_goblin_settlement_names` to the
/// four-people world under the new contract.
fn assert_species_pin_isolated(species: &str) {
    let pinned_a = seed_42_with(&SettlementPins {
        species: Some(species.to_string()),
    });
    let pinned_b = seed_42_with(&SettlementPins {
        species: Some(species.to_string()),
    });

    let pinned_cells = settlements_by_cell(&pinned_a, species);
    assert!(
        !pinned_cells.is_empty(),
        "the {species}-pinned world should place at least one {species} settlement"
    );

    assert_eq!(
        pinned_a.to_json(),
        pinned_b.to_json(),
        "building the same --species {species} pin twice must yield a \
         byte-identical world (same seed + pin → identical bytes) — the \
         niche-K coexistence stack is competitive, so pin-vs-unpinned \
         population equality no longer holds (decision-ledger #49), but \
         determinism of the restricted roster must"
    );
}

/// Pin-isolation for goblin — the family's reference daughter, and the
/// species the retired keystones covered.
#[test]
fn species_pin_isolation_holds_for_goblin() {
    assert_species_pin_isolated("goblin");
}

/// Pin-isolation for hobgoblin — a goblinoid daughter new under The
/// Branches, exercising the family lexicon path (shared proto phonology +
/// per-daughter cascade) end to end under a pin.
#[test]
fn species_pin_isolation_holds_for_hobgoblin() {
    assert_species_pin_isolated("hobgoblin");
}

/// (2) Replay (spec §9): every `Root` entry's recorded sound-change
/// derivation replays byte-identically through `evolve` given the same
/// proto-root and the species' own drawn cascade — for every roster
/// species, so the goblinoid daughters' family path (proto drawn at the
/// family level, evolved through the daughter's own cascade into its own
/// phonology) is covered alongside kobold's singleton path.
#[test]
fn derivations_replay() {
    let world = default_generated_seed_42();
    let mut checked = 0;

    // Language/lexicon coverage is a speaker-only concern. Since The Eremite the
    // psyche registry is a SUPERSET of the speakers — the dragons carry a mind
    // but no articulation — so iterate the articulation registry, which holds
    // exactly the settling, speaking kinds (the same peopled boundary
    // `windows/worldgen`'s settlement-genesis pass applies).
    for kind in hornvale_language::articulation_registry().ids() {
        let species = kind.0;
        let ph = hornvale_worldgen::language_of(&world, species);
        // Route through the composition root's cascade_of (not the language
        // crate's default-regime draw_cascade): the articulation registry is
        // dragon-reachable (The Solitary Tongue), and a dragon's cascade must
        // be drawn at its frozen regime, consistent with the lexicon
        // lexicon_of below already builds at that same regime.
        let cascade = hornvale_worldgen::cascade_of(&world, species)
            .unwrap_or_else(|e| panic!("cascade_of({species}) failed: {e:?}"));
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

/// (3) Coverage (spec §7): for every roster species, every concept the
/// world's registry knows about resolves to exactly one lexicon entry —
/// `build_lexicon`'s own invariant, re-checked from outside the crate.
/// `entry` proves no concept is missing; the entry count against the
/// registry's proves none is fabricated or double-resolved.
#[test]
fn every_concept_resolves_once() {
    let world = default_generated_seed_42();
    let concept_count = world.registry.concepts().count();
    assert!(concept_count > 0, "the registry should hold concepts");

    // Language/lexicon coverage is a speaker-only concern. Since The Eremite the
    // psyche registry is a SUPERSET of the speakers — the dragons carry a mind
    // but no articulation — so iterate the articulation registry, which holds
    // exactly the settling, speaking kinds (the same peopled boundary
    // `windows/worldgen`'s settlement-genesis pass applies).
    for kind in hornvale_language::articulation_registry().ids() {
        let species = kind.0;
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));
        for concept in world.registry.concepts() {
            assert!(
                lex.entry(&concept.name).is_some(),
                "species {} has no lexicon entry for registered concept {:?}",
                species,
                concept.name
            );
        }
        assert_eq!(
            lex.entries().count(),
            concept_count,
            "species {}'s lexicon fabricated or dropped entries relative to \
             the registry",
            species
        );
    }
}

/// (4) Recountability (spec §7): every `Gap` entry carries a non-empty
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
        "night-vision",
        "no settlement",
        "has no exposure",
        "no compound recipe",
        "needs",
    ];

    // Language/lexicon coverage is a speaker-only concern. Since The Eremite the
    // psyche registry is a SUPERSET of the speakers — the dragons carry a mind
    // but no articulation — so iterate the articulation registry, which holds
    // exactly the settling, speaking kinds (the same peopled boundary
    // `windows/worldgen`'s settlement-genesis pass applies).
    for kind in hornvale_language::articulation_registry().ids() {
        let species = kind.0;
        let lex = hornvale_worldgen::lexicon_of(&world, species)
            .unwrap_or_else(|e| panic!("lexicon_of({species}) failed: {e:?}"));
        for (concept, entry) in lex.entries() {
            if let LexEntry::Gap { reason } = entry {
                checked += 1;
                let text = match reason {
                    GapReason::Experiential(s) => s,
                    GapReason::Perceptual(s) => s,
                };
                assert!(
                    !text.is_empty(),
                    "species {species} concept {concept:?} has an empty gap reason"
                );
                assert!(
                    markers.iter().any(|m| text.contains(m)),
                    "species {species} concept {concept:?} gap reason {text:?} \
                     names neither a recognizable fact nor a vector dimension"
                );
            }
        }
    }
    assert!(
        checked > 0,
        "seed 42 should leave at least one concept a gap for some species"
    );
}
