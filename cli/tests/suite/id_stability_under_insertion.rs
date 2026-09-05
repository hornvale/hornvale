//! The Signet's acceptance test: inserting a minting stage must not move any
//! id outside that stage's own lineage. Before this campaign every id was the
//! mint counter's value at the moment of minting, so one extra entity minted
//! anywhere upstream renumbered everything downstream of it — which is why six
//! committed fixtures had to be re-pinned every time a stage grew.
//!
//! **Why the insertion happens where it does.** `build_to` is a single
//! straight-line pipeline with no injection seam, and adding one to production
//! purely so a test could use it is exactly the "burn extra mints behind an env
//! var" shape The Salt rejected. So the insertion is made at the two seams a
//! test can reach *without* touching production code, on a live seed-42 world:
//!
//! 1. `emit_history` — the deep-history stage that mints one ROOT-lineage
//!    entity per occupation record. This is the genesis-side stage, the one
//!    whose ids the fixtures actually carry.
//! 2. `derive_npcs` — the vessel's post-genesis stage, which mints CHILD
//!    lineages (`parent: Some(village.id)`) into a session's ledger clone.
//!
//! Between them the two shapes of lineage — rooted and parented, `mint_entity`
//! and `reuse_or_mint_entity` — are both covered.
//!
//! **A set difference is the WEAK form of this claim, and the mutation proof
//! is what showed it.** The natural way to write "no id moved" is
//! `plain_ids.difference(&extra_ids)`, and under the pre-campaign counter that
//! difference is EMPTY: the counter hands the inserted entity id `n` and
//! shifts every later id up by one, so the plain arm's id set is a strict
//! SUBSET of the perturbed arm's and nothing reads as missing. The assertions
//! that actually bite are therefore the ELEMENTWISE comparisons of the ids a
//! stage minted, in derivation order — `plain[i] == extra[i]`, which a shift
//! breaks immediately. The set difference is kept as the campaign's stated
//! claim, below the comparison that can see a shift.
//!
//! **The anti-vacuity guards matter as much as the headline assertion.**
//! A difference over two id sets is also empty when the later stage minted
//! nothing at all and the two sets are copies of the same genesis facts. Each
//! test therefore asserts that the stage under test minted a substantial
//! number of ids the base ledger did not already hold, and that the inserted
//! entity really is present in the perturbed ledger and absent from the
//! unperturbed one. Without those, this file would pass on a stage that had
//! quietly stopped running.

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{EntityId, Fact, Ledger, Lineage, Seed, Value, World};
use hornvale_locale::LocaleContext;
use hornvale_terrain::TerrainPins;
use hornvale_vessel::liveness::derive_npcs;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, emit_history, history_for,
    occupation_records,
};
use std::collections::BTreeSet;

/// Seed 42 — the world every committed fixture and gallery page is cut from,
/// and therefore the world whose ids this campaign promises to hold still.
const WITNESS_SEED: u64 = 42;

/// The lineage of the entity the "inserted stage" mints. Nothing in the real
/// pipeline uses this role, so it collides with no genuine lineage — and the
/// collision assert in `Ledger::mint_entity` would say so loudly if it did.
const INSERTED_ROLE: &str = "the-signet-inserted-stage";

/// Every entity the ledger holds a fact about. A minted id with no facts is
/// invisible here, which is why the inserted entity below commits one.
fn subjects(ledger: &Ledger) -> BTreeSet<EntityId> {
    ledger.iter().map(|f| f.subject).collect()
}

/// Mint one root-lineage entity into `ledger` and give it a single NAME fact
/// so it is observable in [`subjects`]. This is the whole of the "inserted
/// stage": one extra mint, upstream of the stage under test.
fn insert_a_minting_stage(ledger: &mut Ledger, world: &World) -> EntityId {
    let id = ledger.mint_entity(Lineage {
        parent: None,
        role: INSERTED_ROLE,
        ordinal: 0,
    });
    ledger
        .commit(
            Fact {
                subject: id,
                predicate: hornvale_kernel::NAME.to_string(),
                object: Value::Text("the inserted stage".to_string()),
                place: None,
                day: None,
                provenance: "the-signet-acceptance-test".to_string(),
            },
            &world.registry,
        )
        .expect("a freshly minted entity's first NAME fact always commits");
    id
}

/// The shared claim, asserted the same way at both seams: `with_extra` must
/// hold every id `plain` holds, plus exactly the one inserted entity.
///
/// `stage` names the stage under test in the failure messages; `fresh_floor`
/// is how many ids that stage must mint for the comparison to mean anything.
fn assert_only_the_inserted_id_is_new(
    plain: &Ledger,
    with_extra: &Ledger,
    base: &Ledger,
    inserted: EntityId,
    stage: &str,
    fresh_floor: usize,
) {
    let base_ids = subjects(base);
    let plain_ids = subjects(plain);
    let extra_ids = subjects(with_extra);

    // Anti-vacuity: the stage under test must actually mint. If it minted
    // nothing, both sets below are just the base ledger's ids copied twice and
    // the difference is empty no matter how ids are derived.
    let fresh: BTreeSet<EntityId> = plain_ids.difference(&base_ids).copied().collect();
    assert!(
        fresh.len() >= fresh_floor,
        "the {stage} stage minted only {} ids the base ledger did not already \
         hold (floor {fresh_floor}) -- with nothing fresh downstream of the \
         insertion this test compares two copies of the same genesis facts and \
         proves nothing about id stability",
        fresh.len()
    );

    // Anti-vacuity: the insertion must have taken effect. A no-op insertion
    // makes the two ledgers identical by construction.
    assert!(
        !plain_ids.contains(&inserted),
        "the inserted entity {:#x} is present in the ledger that never \
         inserted it -- the two arms are not distinguishable",
        inserted.get()
    );
    assert!(
        extra_ids.contains(&inserted),
        "the inserted entity {:#x} is absent from the ledger that inserted it \
         -- the insertion did not happen, so the {stage} stage was never \
         perturbed",
        inserted.get()
    );

    // The campaign's claim in its stated form: inserting a stage moves nothing
    // outside its own lineage. The WEAK form — a uniform shift leaves the
    // unperturbed arm a subset of the perturbed one, so this alone cannot see
    // a counter. The elementwise comparison each caller makes first is what
    // catches that; this catches an id that VANISHED rather than moved.
    let missing: Vec<EntityId> = plain_ids.difference(&extra_ids).copied().collect();
    assert!(
        missing.is_empty(),
        "inserting a stage moved {} pre-existing ids across the {stage} stage \
         -- the positional identity this campaign removed has come back: {:?}",
        missing.len(),
        missing.iter().take(10).collect::<Vec<_>>()
    );

    // The other direction, which the difference above cannot see: the second
    // ledger must not have grown by anything BUT the inserted entity. A
    // derivation that moved ids while keeping the old ones around would pass
    // the assertion above and fail this one.
    assert_eq!(
        extra_ids.len(),
        plain_ids.len() + 1,
        "inserting one entity changed the {stage} stage's id population by {} \
         rather than by exactly the inserted id",
        extra_ids.len() as i64 - plain_ids.len() as i64
    );
}

/// The genesis-side seam: the deep-history stage mints one root-lineage entity
/// per occupation record. Inserting a mint immediately before it must leave
/// every occupation id exactly where it was.
#[test]
fn inserting_a_minting_stage_moves_no_unrelated_id() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    // Terrain depth: the last rung before anything is minted for a settlement
    // or an occupation, so the history stage below is genuinely still ahead of
    // us rather than already committed.
    let base = build_world_to(
        Seed(WITNESS_SEED),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds to terrain depth");
    let history = history_for(
        Seed(WITNESS_SEED),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
    )
    .expect("seed 42 bakes a deep history");
    assert!(
        history.records.len() >= 50,
        "seed 42 must bake a substantial history for this comparison to bite \
         (baked {})",
        history.records.len()
    );

    let mut plain = base.clone();
    emit_history(&mut plain, &history).expect("the baked history commits");

    let mut with_extra = base.clone();
    let inserted = insert_a_minting_stage(&mut with_extra.ledger, &base);
    emit_history(&mut with_extra, &history).expect("the baked history commits after an insertion");

    // The occupation ids themselves, in commit order — the comparison that can
    // see a SHIFT. The set difference below cannot: a counter that renumbers
    // every occupation upward by one still leaves the unperturbed arm's ids a
    // subset of the perturbed arm's.
    let plain_occupations: Vec<EntityId> = occupation_records(&plain)
        .into_iter()
        .map(|o| o.id)
        .collect();
    let extra_occupations: Vec<EntityId> = occupation_records(&with_extra)
        .into_iter()
        .map(|o| o.id)
        .collect();
    assert!(
        plain_occupations.len() >= 50,
        "the deep-history stage must commit a substantial occupation roster \
         for this comparison to bite (committed {})",
        plain_occupations.len()
    );
    assert_eq!(
        plain_occupations, extra_occupations,
        "an entity minted before the deep-history stage moved the occupations' \
         own ids"
    );

    assert_only_the_inserted_id_is_new(
        &plain.ledger,
        &with_extra.ledger,
        &base.ledger,
        inserted,
        "deep-history",
        50,
    );
}

/// The session-side seam, and the one that exercises CHILD lineages: the
/// vessel derives an NPC per settlement into its own ledger clone, each parented
/// to its settlement. Inserting a mint before that derivation must leave every
/// NPC id where it was — under the counter it shifted all of them by one, which
/// is what moved the committed session fixtures.
#[test]
fn inserting_a_stage_moves_no_id_a_later_session_stage_derives() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = build_world_to(
        Seed(WITNESS_SEED),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Full,
    )
    .expect("seed 42 builds in full");
    let ctx = LocaleContext::build(&world).expect("seed 42 has a locale");
    let settlements = hornvale_settlement::all_settlements(&world);
    assert!(
        settlements.len() >= 10,
        "seed 42 must carry settlements for the NPC stage to mint against \
         (found {})",
        settlements.len()
    );
    let home = settlements[0].id;
    let k = settlements.len();

    let mut plain = world.ledger.clone();
    let plain_npcs = derive_npcs(&world, &ctx, &mut plain, k, home);

    let mut with_extra = world.ledger.clone();
    let inserted = insert_a_minting_stage(&mut with_extra, &world);
    let extra_npcs = derive_npcs(&world, &ctx, &mut with_extra, k, home);

    // The NPC ids themselves, compared in derivation order rather than as a
    // set: the set comparison below would still pass if the stage produced the
    // same ids in a different order, and this campaign's claim is that an
    // NPC's id is a function of its settlement alone.
    let plain_entities: Vec<EntityId> = plain_npcs.iter().map(|n| n.entity).collect();
    let extra_entities: Vec<EntityId> = extra_npcs.iter().map(|n| n.entity).collect();
    assert!(
        plain_entities.len() >= 10,
        "the NPC stage must derive a substantial roster for this comparison to \
         bite (derived {})",
        plain_entities.len()
    );
    assert_eq!(
        plain_entities, extra_entities,
        "an entity minted before the NPC derivation moved the NPCs' own ids"
    );

    assert_only_the_inserted_id_is_new(&plain, &with_extra, &world.ledger, inserted, "npc", 10);
}
