//! The Doctrine (C6 Task 2): the SOC-1 gate, the four preregistered
//! deltas, and the dial-roster separation — measured against live seeds
//! and pinned exact.

use hornvale_language::schemas::{Manner, SchemaId};
use hornvale_language::{Disposition, LexemeId, LossReason};
use hornvale_worldgen::{SettlementPins, SkyChoice, doctrine_of, doctrines_of};

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

#[test]
fn the_soc1_gate_is_the_flagship_cult_form() {
    // Positive arm, seed 1: the SOC-1 gate is exact — a placed culture's
    // flagship cult-form gates doctrine_of exactly (organized <=> Some, folk
    // <=> None). Post-Demesne (BIO-35 Stage 1 recalibration), seed-1's goblin
    // is organized and its hobgoblin flipped to folk (the same organized->folk
    // drift the book's seed-1 doctrine tests pin), so this seed exercises BOTH
    // arms of the gate directly, and doctrines_of covers exactly the organized
    // subset (no longer every placed culture).
    let w = generated(1);
    let placed = hornvale_worldgen::placed_peoples(&w);
    assert!(!placed.is_empty(), "seed 1 must place at least one culture");
    let mut organized_count = 0usize;
    let mut goblin_organized = false;
    for (kind, village) in &placed {
        let cult_form = hornvale_religion::cult_form_held_by(&w, village.id);
        let is_organized = cult_form.as_deref() == Some("organized");
        assert_eq!(
            doctrine_of(&w, kind).is_some(),
            is_organized,
            "seed 1's {kind}: doctrine_of must be Some iff its flagship cult-form is organized \
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
    let doctrines = doctrines_of(&w);
    assert_eq!(
        doctrines.len(),
        organized_count,
        "doctrines_of must cover exactly every organized placed culture"
    );

    // Negative arm: sweep seeds 1..=10 for any species whose flagship's
    // committed cult-form is "folk" -> doctrine_of must gate to None. If
    // the sweep finds no such culture at all, PANIC (fail loudly, the C5
    // F2 lesson) rather than silently passing a law whose negative arm was
    // never exercised.
    let mut found_folk = false;
    for seed in 1..=10u64 {
        let w = generated(seed);
        for (kind, village) in hornvale_worldgen::placed_peoples(&w) {
            if hornvale_religion::cult_form_held_by(&w, village.id).as_deref() == Some("folk") {
                found_folk = true;
                assert!(
                    doctrine_of(&w, kind).is_none(),
                    "seed {seed}'s {kind} carries a folk flagship — doctrine_of must gate to None"
                );
            }
        }
    }
    assert!(
        found_folk,
        "SOC-1's negative arm found NO folk-cult-form flagship across seeds 1..=10 — this must \
         not silently pass. Add a synthetic-society unit test driving doctrine_of directly \
         against a hand-built world whose flagship's committed cult-form is \"folk\" instead of \
         relying on this sweep."
    );
}

#[test]
fn the_selection_bias_law_field_by_field() {
    // Measured, seed 1 goblin: folk sky_capability is 0.5; doctrine's is
    // exactly folk + 0.25 (capped at 1.0). Every other AccountParams field
    // is copied verbatim — no hidden divergence.
    let w = generated(1);
    let folk_params = hornvale_worldgen::account_params_of(&w, "goblin").unwrap();
    let doctrine = doctrine_of(&w, "goblin").expect("goblin must be organized at seed 1");
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
    let registry = hornvale_species::psyche_registry();
    let psych = registry.get_by_label("goblin").unwrap();
    assert_eq!(
        hornvale_worldgen::doctrine_beta_of(psych),
        hornvale_worldgen::beta_of(psych) + 0.5
    );
}

#[test]
fn doctrine_keeps_what_folk_lose() {
    // Seed 1 goblin (folk cap 0.5, doctrine 0.75 >= the moons' 0.6
    // threshold): moon-count is Lost in the folk account but effectively
    // Kept (however explain wraps it) in the doctrine account.
    let w = generated(1);
    let folk = hornvale_worldgen::accounts_of(&w)
        .into_iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin folk voice at seed 1");
    let doctrine = doctrine_of(&w, "goblin").expect("goblin doctrine voice at seed 1");

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
            agent: Some("Soevvae".to_string()),
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
    // That rule finds the SAME belief (Wowako, period 1.55 std days,
    // matching the world's committed day-length-std, 1.5507196, within the
    // 1% tolerance) folk's own cyclic_beliefs_of would also find — even
    // though folk's OWN measured schema draw for this fact (PathJourney)
    // is agentless and so never surfaces a deity at all. "Whichever branch
    // is true" (plan header): this is the no-high-god branch, pinned
    // exact.
    let w = generated(1);
    let flagship = hornvale_worldgen::flagship_of(&w, "goblin").expect("goblin flagship at seed 1");
    let beliefs = hornvale_religion::beliefs_held_by(&w, flagship.id);
    assert!(
        !beliefs.iter().any(|b| b.high_god),
        "seed 1 goblin's pantheon must carry no high-god belief (measured, unranked society)"
    );

    let doctrine = doctrine_of(&w, "goblin").expect("goblin doctrine voice at seed 1");
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
            agent: Some("Wowako".to_string()),
            lexeme: Some(LexemeId("strides")),
            manner: Manner::Brisk,
        }
    );

    // Cross-check against folk's own period-match rule, driven directly:
    // the same belief the doctrine bound is exactly the one folk's rule
    // would find, independent of which schema either account's draw fires.
    let cyclic = hornvale_worldgen::cyclic_beliefs_of(&w, "goblin");
    let day_value = 1.5507196;
    let folk_bound = cyclic
        .iter()
        .find(|(_, p)| (*p - day_value).abs() < 0.01 * day_value)
        .expect("a day-matched cyclic belief must exist at seed 1 goblin");
    assert_eq!(
        folk_bound.0.deity, "Wowako",
        "doctrine's binding must be the SAME belief folk's own period-match rule finds"
    );
}

#[test]
fn doctrine_voices_never_enter_the_dial_roster() {
    // accounts_of(seed 1) returns exactly placed_peoples-many folk voices
    // — never a DoctrineVoice (a distinct type, so this could not even
    // compile the other way). Determinism check: re-deriving accounts_of
    // twice yields byte-identical Debug output, and the folk goblin's
    // sky_capability is 0.5 EXACTLY (no +0.25 leak from the doctrine
    // stack) — the dial-roster law (ledger #4), value-level.
    let w = generated(1);
    let placed = hornvale_worldgen::placed_peoples(&w);

    let voices_a = hornvale_worldgen::accounts_of(&w);
    let voices_b = hornvale_worldgen::accounts_of(&w);
    assert_eq!(voices_a.len(), placed.len());
    assert_eq!(
        format!("{voices_a:?}"),
        format!("{voices_b:?}"),
        "accounts_of must be a pure, deterministic function of the world"
    );

    let goblin = voices_a
        .iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin folk voice at seed 1");
    assert_eq!(
        goblin.params.sky_capability, 0.5,
        "the folk goblin's capability must stay exactly 0.5 — no doctrine leak into accounts_of"
    );
}

#[test]
fn doctrine_is_deterministic() {
    let w = generated(1);
    let a = format!("{:?}", doctrines_of(&w));
    let b = format!("{:?}", doctrines_of(&w));
    assert_eq!(
        a, b,
        "doctrines_of (post-doctrine-explain) must be a pure function of the world"
    );
}
