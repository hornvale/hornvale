//! The Doctrine (C6 Task 2): the SOC-1 gate, the four preregistered
//! deltas, and the dial-roster separation — measured against live seeds
//! and pinned exact.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
//!
//! The SOC-1 gate's negative arm is a synthetic-society unit test plus a
//! single live smoke, not a seed hunt (decision 0093, "seed-hunting is not
//! a test mechanism"): a hand-built world supplies deterministic,
//! zero-build coverage of `doctrine_from`'s folk-gates-to-None behavior,
//! while one real generated seed is checked directly for liveness.
#![allow(clippy::disallowed_methods)]

use hornvale_kernel::{EntityId, Fact, Value};
use hornvale_language::schemas::{Manner, SchemaId};
use hornvale_language::{Disposition, LexemeId, LossReason};
use hornvale_worldgen::{SettlementPins, SkyChoice, doctrine_from, doctrines_from};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// neighboring worldgen integration test (`explanations.rs`,
/// `chorus_params.rs`) uses.
fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// A hand-built synthetic world (decision 0093, "seed-hunting is not a test
/// mechanism"): the minimum committed state for `flagship_of` +
/// `cult_form_held_by` + `doctrine_from`'s SOC-1 gate to be REAL — one
/// settlement peopled by `species` (`is-settlement` + `peopled-by`) and one
/// belief held by that settlement (`is-belief` + `held-by` + `cult-form`).
/// Predicates are registered through the world's own registry exactly as
/// genesis does (each domain's `register_concepts`), and every fact commits
/// through the normal `Ledger::commit` path, so contradiction-checking is
/// exercised the same as a generated world's. No sky, no terrain sculpt, no
/// generation of any kind — zero builds, deterministic by construction.
fn synthetic_flagship(species: &str, cult_form: &str) -> (hornvale_kernel::World, EntityId) {
    let mut w = hornvale_kernel::World::new(hornvale_kernel::Seed(1));
    hornvale_settlement::register_concepts(&mut w.registry)
        .expect("settlement predicates register");
    hornvale_species::register_concepts(&mut w.registry).expect("species predicates register");
    hornvale_religion::register_concepts(&mut w.registry).expect("religion predicates register");

    let provenance = || "synthetic negative arm (decision 0093)".to_string();
    let settlement = w.ledger.mint_entity();
    w.ledger
        .commit(
            Fact {
                subject: settlement,
                predicate: hornvale_settlement::IS_SETTLEMENT.to_string(),
                object: Value::Flag(true),
                place: None,
                day: Some(0.0),
                provenance: provenance(),
            },
            &w.registry,
        )
        .expect("commit is-settlement");
    w.ledger
        .commit(
            Fact {
                subject: settlement,
                predicate: hornvale_species::PEOPLED_BY.to_string(),
                object: Value::Text(species.to_string()),
                place: None,
                day: Some(0.0),
                provenance: provenance(),
            },
            &w.registry,
        )
        .expect("commit peopled-by");

    let belief = w.ledger.mint_entity();
    w.ledger
        .commit(
            Fact {
                subject: belief,
                predicate: hornvale_religion::IS_BELIEF.to_string(),
                object: Value::Flag(true),
                place: None,
                day: Some(0.0),
                provenance: provenance(),
            },
            &w.registry,
        )
        .expect("commit is-belief");
    w.ledger
        .commit(
            Fact {
                subject: belief,
                predicate: hornvale_religion::HELD_BY.to_string(),
                object: Value::Entity(settlement),
                place: None,
                day: Some(0.0),
                provenance: provenance(),
            },
            &w.registry,
        )
        .expect("commit held-by");
    w.ledger
        .commit(
            Fact {
                subject: belief,
                predicate: hornvale_religion::CULT_FORM.to_string(),
                object: Value::Text(cult_form.to_string()),
                place: None,
                day: Some(0.0),
                provenance: provenance(),
            },
            &w.registry,
        )
        .expect("commit cult-form");

    (w, settlement)
}

#[test]
fn synthetic_folk_flagship_gates_doctrine_to_none() {
    // Decision 0093: the SOC-1 gate's negative arm no longer sweeps seeds
    // 1..=60 looking for a folk flagship (a census question, answered
    // badly) — it hand-builds one directly (a synthetic question, answered
    // exactly). `doctrine_from`'s gate (`chorus.rs`) reads only
    // `flagship_of` + `cult_form_held_by` and returns `None` the moment
    // `cult_form != "organized"`, BEFORE it ever touches terrain/climate —
    // so a synthetic world needs no generation at all, and terrain/climate
    // are inert for this arm (still derived once, matching every other call
    // site's shape, so this test would also catch a gate reordering that
    // started reading them first).
    let (w, settlement) = synthetic_flagship("goblin", "folk");
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");

    let flagship = hornvale_worldgen::flagship_of(&w, "goblin").expect("synthetic flagship");
    assert_eq!(
        flagship.id, settlement,
        "flagship_of must find the synthetic settlement"
    );
    assert_eq!(
        hornvale_religion::cult_form_held_by(&w, settlement).as_deref(),
        Some("folk"),
        "the synthetic belief's cult-form must read back as folk"
    );
    assert!(
        doctrine_from(&w, "goblin", &terrain, &climate).is_none(),
        "a folk flagship must gate doctrine_from to None"
    );
}

#[test]
fn the_soc1_gate_is_the_flagship_cult_form() {
    // Positive arm, seed 1: the SOC-1 gate is exact — a placed culture's
    // flagship cult-form gates doctrine_from exactly (organized <=> Some, folk
    // <=> None). Post-Demesne (BIO-35 Stage 1 recalibration), seed-1's goblin
    // is organized and its hobgoblin flipped to folk (the same organized->folk
    // drift the book's seed-1 doctrine tests pin), so this seed exercises BOTH
    // arms of the gate directly, and doctrines_from covers exactly the organized
    // subset (no longer every placed culture).
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let placed = hornvale_worldgen::placed_peoples(&w);
    assert!(!placed.is_empty(), "seed 1 must place at least one culture");
    let mut organized_count = 0usize;
    let mut goblin_organized = false;
    for (kind, village) in &placed {
        let cult_form = hornvale_religion::cult_form_held_by(&w, village.id);
        let is_organized = cult_form.as_deref() == Some("organized");
        assert_eq!(
            doctrine_from(&w, kind, &terrain, &climate).is_some(),
            is_organized,
            "seed 1's {kind}: doctrine_from must be Some iff its flagship cult-form is organized \
             (cult_form={cult_form:?})"
        );
        if is_organized {
            organized_count += 1;
        }
        if *kind == "goblin" {
            goblin_organized = is_organized;
        }
    }
    assert!(
        goblin_organized,
        "seed 1's goblin flagship is organized (the seed-1 anchor; ledger #1)"
    );
    let doctrines = doctrines_from(&w, &terrain, &climate);
    assert_eq!(
        doctrines.len(),
        organized_count,
        "doctrines_from must cover exactly every organized placed culture"
    );

    // Negative arm (decision 0093, "seed-hunting is not a test mechanism"):
    // the 1..=60-seed hunt for a folk flagship is gone. The synthetic test
    // `synthetic_folk_flagship_gates_doctrine_to_none` above now supplies
    // deterministic, zero-build coverage of the gate's None branch by
    // construction rather than by search. What remains here is a single
    // live folk smoke — one real generated seed, checked directly, to prove
    // a genuinely-generated folk flagship (not just a hand-built one) gates
    // the same way. This is a liveness check, not a sweep: it does not
    // search for an instance, it asserts against one known one.
    //
    // FOLK_SMOKE_SEED/KIND are epoch-sensitive (a lexicon or genesis-bake
    // re-draw can shift which culture at which seed carries a folk
    // flagship, exactly as it already has once — see below). If this
    // assertion ever fails on an otherwise-unrelated change, re-find with a
    // single bounded scan (a handful of seeds around the old constant, NOT
    // the deleted 1..=60 sweep — decision 0093's "do not widen the sweep
    // back") and update both constants together.
    //
    // Re-found this merge: The Wearing's lexicon re-draw left seed 56 (the
    // prior constant) with an all-organized roster; a bounded 50..=60 scan
    // found seed 57's bugbear flagship still folk.
    const FOLK_SMOKE_SEED: u64 = 57;
    const FOLK_SMOKE_KIND: &str = "bugbear";
    let w = generated(FOLK_SMOKE_SEED);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let flagship = hornvale_worldgen::flagship_of(&w, FOLK_SMOKE_KIND).unwrap_or_else(|| {
        panic!("seed {FOLK_SMOKE_SEED} must place a {FOLK_SMOKE_KIND} flagship")
    });
    assert_eq!(
        hornvale_religion::cult_form_held_by(&w, flagship.id).as_deref(),
        Some("folk"),
        "seed {FOLK_SMOKE_SEED}'s {FOLK_SMOKE_KIND} flagship must still carry a folk cult-form \
         (epoch-sensitive constant — re-find with a bounded scan if this fails)"
    );
    assert!(
        doctrine_from(&w, FOLK_SMOKE_KIND, &terrain, &climate).is_none(),
        "seed {FOLK_SMOKE_SEED}'s {FOLK_SMOKE_KIND} carries a folk flagship — doctrine_from must \
         gate to None"
    );
}

#[test]
fn the_selection_bias_law_field_by_field() {
    // Measured, seed 1 goblin: folk sky_capability is 0.5; doctrine's is
    // exactly folk + 0.25 (capped at 1.0). Every other AccountParams field
    // is copied verbatim — no hidden divergence.
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let folk_params =
        hornvale_worldgen::account_params_from(&w, "goblin", &terrain, &climate).unwrap();
    let doctrine = doctrine_from(&w, "goblin", &terrain, &climate)
        .expect("goblin must be organized at seed 1");
    let doctrine_params = &doctrine.params;

    assert_eq!(folk_params.sky_capability, 0.5, "measured folk capability");
    assert_eq!(
        doctrine_params.sky_capability,
        (folk_params.sky_capability + 0.25).min(1.0)
    );
    assert_eq!(doctrine_params.sky_capability, 0.75);

    assert_eq!(doctrine_params.hold_all, folk_params.hold_all);
    assert_eq!(doctrine_params.holdings, folk_params.holdings);
    assert_eq!(doctrine_params.observability, folk_params.observability);
    assert_eq!(doctrine_params.order, folk_params.order);
    assert_eq!(doctrine_params.stances, folk_params.stances);
    assert_eq!(doctrine_params.world_carving, folk_params.world_carving);

    // Beta is not an AccountParams field: assert the function-level delta
    // directly.
    let registry = hornvale_species::society_registry();
    let society = registry.get_by_label("goblin").unwrap();
    assert_eq!(
        hornvale_worldgen::doctrine_beta_of(society),
        hornvale_worldgen::beta_of(society) + 0.5
    );
}

#[test]
fn doctrine_keeps_what_folk_lose() {
    // Seed 1 goblin (folk cap 0.5, doctrine 0.75 >= the moons' 0.6
    // threshold): moon-count is Lost in the folk account but effectively
    // Kept (however explain wraps it) in the doctrine account.
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let folk = hornvale_worldgen::accounts_from(&w, &terrain, &climate)
        .into_iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin folk voice at seed 1");
    let doctrine =
        doctrine_from(&w, "goblin", &terrain, &climate).expect("goblin doctrine voice at seed 1");

    let folk_moon = folk
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "moon-count")
        .expect("a moon-count ground fact must exist");
    let doctrine_moon = doctrine
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "moon-count")
        .expect("a moon-count ground fact must exist");

    assert!(
        matches!(folk_moon.disposition, Disposition::Lost(_)),
        "folk goblin (capability 0.5) must lose moon-count (threshold 0.6), got {:?}",
        folk_moon.disposition
    );
    assert!(
        effectively_kept(&doctrine_moon.disposition),
        "doctrine goblin (capability 0.75) must keep moon-count, got {:?}",
        doctrine_moon.disposition
    );

    // Measured exact shape: the doctrine keeps it AND explains it
    // (Agentive, bound to the slowest cyclic belief, manner Slow).
    assert_eq!(
        doctrine_moon.disposition,
        Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            // The Wearing (this merge): the deity's rendered name re-derived
            // Soevvae -> Wtoevvelqa. Task 3's 19 new concepts shift the
            // proto-root walk, so every lexicon-derived name re-draws;
            // underlying (Kept), schema, lexeme and manner are unchanged —
            // only the name moved.
            agent: Some("Wtoevvelqa".to_string()),
            lexeme: Some(LexemeId("strides")),
            manner: Manner::Slow,
        }
    );
}

/// Whether `d` reduces (through any number of `Explained` wrappers) to
/// `Disposition::Kept` — the same effective-disposition question
/// `hornvale_language::schemas::conflict_of` asks through its `effective`
/// seam, re-derived here locally since that helper is `pub(crate)` to the
/// language crate and this is an external integration test.
fn effectively_kept(d: &Disposition) -> bool {
    match d {
        Disposition::Kept => true,
        Disposition::Explained { underlying, .. } => effectively_kept(underlying),
        _ => false,
    }
}

#[test]
fn the_high_god_takes_the_day_where_compatible() {
    // Measured, seed 1 goblin: the pantheon is unranked, so it carries NO
    // high-god belief at all (society.strata never clears RANKED_STRATA)
    // — delta d's preference is inert for this culture, and the doctrine's
    // day binding falls straight through to folk's OWN period-match rule.
    // That rule finds the SAME belief (Voovoo, period 1.55 std days,
    // matching the world's committed day-length-std, 1.5507196, within the
    // 1% tolerance) folk's own cyclic_beliefs_from would also find — even
    // though folk's OWN measured schema draw for this fact (PathJourney)
    // is agentless and so never surfaces a deity at all. "Whichever branch
    // is true" (plan header): this is the no-high-god branch, pinned
    // exact.
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let flagship = hornvale_worldgen::flagship_of(&w, "goblin").expect("goblin flagship at seed 1");
    let beliefs = hornvale_religion::beliefs_held_by(&w, flagship.id);
    assert!(
        !beliefs.iter().any(|b| b.high_god),
        "seed 1 goblin's pantheon must carry no high-god belief (measured, unranked society)"
    );

    let doctrine =
        doctrine_from(&w, "goblin", &terrain, &climate).expect("goblin doctrine voice at seed 1");
    let doctrine_day = doctrine
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "day-length-std")
        .expect("a day-length-std ground fact must exist");
    assert_eq!(
        doctrine_day.disposition,
        Disposition::Explained {
            underlying: Box::new(Disposition::Lost(LossReason::BeyondCapability {
                domain: "sky"
            })),
            schema: SchemaId::Agentive,
            // The Wearing (this merge): Wowako -> Kaavoa, the same
            // lexicon re-draw. The rebase onto The Toponym's cohort
            // ordering re-draws it once more: Kaavoa -> Voovoo. At both
            // steps the belief, its period and the day-match are
            // unchanged, and `underlying`, `schema`, `lexeme` and
            // `manner` compare byte-identical — only `agent` moved.
            agent: Some("Voovoo".to_string()),
            lexeme: Some(LexemeId("strides")),
            manner: Manner::Brisk,
        }
    );

    // Cross-check against folk's own period-match rule, driven directly:
    // the same belief the doctrine bound is exactly the one folk's rule
    // would find, independent of which schema either account's draw fires.
    let cyclic = hornvale_worldgen::cyclic_beliefs_from(&w, "goblin", &climate);
    let day_value = 1.5507196;
    let folk_bound = cyclic
        .iter()
        .find(|(_, p)| (*p - day_value).abs() < 0.01 * day_value)
        .expect("a day-matched cyclic belief must exist at seed 1 goblin");
    assert_eq!(
        folk_bound.0.deity, "Voovoo",
        "doctrine's binding must be the SAME belief folk's own period-match rule finds"
    );
}

#[test]
fn doctrine_voices_never_enter_the_dial_roster() {
    // accounts_from(seed 1) returns exactly placed_peoples-many folk voices
    // — never a DoctrineVoice (a distinct type, so this could not even
    // compile the other way). Determinism check: re-deriving accounts_from
    // twice yields byte-identical Debug output, and the folk goblin's
    // sky_capability is 0.5 EXACTLY (no +0.25 leak from the doctrine
    // stack) — the dial-roster law (ledger #4), value-level.
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let placed = hornvale_worldgen::placed_peoples(&w);

    let voices_a = hornvale_worldgen::accounts_from(&w, &terrain, &climate);
    let voices_b = hornvale_worldgen::accounts_from(&w, &terrain, &climate);
    assert_eq!(voices_a.len(), placed.len());
    assert_eq!(
        format!("{voices_a:?}"),
        format!("{voices_b:?}"),
        "accounts_from must be a pure, deterministic function of the world"
    );

    let goblin = voices_a
        .iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin folk voice at seed 1");
    assert_eq!(
        goblin.params.sky_capability, 0.5,
        "the folk goblin's capability must stay exactly 0.5 — no doctrine leak into accounts_from"
    );
}

#[test]
fn doctrine_is_deterministic() {
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let a = format!("{:?}", doctrines_from(&w, &terrain, &climate));
    let b = format!("{:?}", doctrines_from(&w, &terrain, &climate));
    assert_eq!(
        a, b,
        "doctrines_from (post-doctrine-explain) must be a pure function of the world"
    );
}
