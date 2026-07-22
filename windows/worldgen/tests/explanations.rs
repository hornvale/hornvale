//! The Explanations (C5 Task 3): derivation, binding, and assembly —
//! `schema_prior`, `beta_of`, `cyclic_beliefs_of`, and `explain` (called
//! from `accounts_of`), measured against live seeds and pinned exact.

use hornvale_language::Disposition;
use hornvale_language::schemas::{Manner, SchemaId};
use hornvale_worldgen::{SettlementPins, SkyChoice, accounts_of, beta_of, cyclic_beliefs_of};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// neighboring worldgen integration test (`chorus_params.rs`,
/// `exposure.rs`) uses.
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
fn beta_matches_the_preregistered_roster() {
    // Plan header roster (ledger #6): goblin/hobgoblin 2.5, bugbear 2.0,
    // kobold 1.0 — status_basis (Knowledge -> 1.0, Rank -> 2.0) plus 0.5
    // for Hierarchic sociality.
    let psych = hornvale_species::psyche_registry();
    let goblin = psych.get_by_label("goblin").unwrap();
    let hobgoblin = psych.get_by_label("hobgoblin").unwrap();
    let kobold = psych.get_by_label("kobold").unwrap();
    let bugbear = psych.get_by_label("bugbear").unwrap();

    assert_eq!(beta_of(goblin), 2.5);
    assert_eq!(beta_of(hobgoblin), 2.5);
    assert_eq!(beta_of(bugbear), 2.0);
    assert_eq!(beta_of(kobold), 1.0);
}

#[test]
fn the_day_binds_by_period_match_never_identity() {
    // Measured (seed 4, bugbear): the day entry fires Agentive, bound to
    // the deity whose re-derived period matches the world's committed day
    // length within 1% relative tolerance — a cyclic belief in bugbear's
    // pantheon (manner Brisk). Binding is by PERIOD alone; this test
    // additionally verifies the bound agent name against the ledger's OWN
    // `deity-name` fact (not just the `Belief.deity` copy) so the pin is
    // truly identity-free: nothing here ever asks which phenomenon produced
    // the belief.
    //
    // Re-pointed under The Living Community epoch (this merge): the prior
    // anchor (seed 10 hobgoblin) no longer fires an Agentive day-schema —
    // the epoch re-placed every world, so its day now reads Balance
    // (deity-free). Seed 4 bugbear is the nearest surviving seed whose day
    // still fires the Agentive-bound-by-period case this test exercises.
    let w = generated(4);
    let voices = accounts_of(&w);
    let hobgoblin = voices
        .iter()
        .find(|v| v.kind == "bugbear")
        .expect("bugbear must place at seed 4");

    let day = hobgoblin
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "day-length-std")
        .expect("a day-length-std ground fact must exist at seed 4");

    let day_value = match &day.fact.object {
        hornvale_kernel::Value::Number(n) => *n,
        other => panic!("day-length-std must be a Number, got {other:?}"),
    };
    let cyclic = cyclic_beliefs_of(&w, "bugbear");
    let matched = cyclic
        .iter()
        .find(|(_, p)| (*p - day_value).abs() < 0.01 * day_value)
        .expect("a day-matched cyclic belief must exist at seed 4 bugbear");
    let deity_name_fact = w
        .ledger
        .text_of(matched.0.id, hornvale_religion::DEITY_NAME)
        .expect("the belief's own deity-name fact must be committed");

    assert_eq!(
        day.disposition,
        Disposition::Explained {
            underlying: Box::new(Disposition::Lost(
                hornvale_language::LossReason::BeyondCapability { domain: "sky" }
            )),
            schema: SchemaId::Agentive,
            agent: Some(deity_name_fact.to_string()),
            lexeme: Some(hornvale_language::LexemeId("strides")),
            manner: Manner::Brisk,
        }
    );
    assert_eq!(matched.0.deity, deity_name_fact);
}

#[test]
fn schema_competition_is_real_across_the_roster() {
    // Across seeds 1..=3, every placed culture's day-schema: MEASURED
    // (not forced) — the derived prior/β competition really does select
    // different schemas for different cultures at the floor (goblin at
    // seed 1 tells PathJourney; hobgoblin at seed 1 and every culture at
    // seed 2 tells CycleReturn; seed 3 splits Balance/PathJourney).
    let mut schemas: Vec<SchemaId> = Vec::new();
    for seed in 1..=3u64 {
        let w = generated(seed);
        for voice in accounts_of(&w) {
            let day = voice
                .account
                .entries
                .iter()
                .find(|e| e.fact.predicate == "day-length-std");
            if let Some(day) = day
                && let Disposition::Explained { schema, .. } = day.disposition
                && !schemas.contains(&schema)
            {
                schemas.push(schema);
            }
        }
    }
    assert!(
        schemas.len() >= 2,
        "schema competition must be real across the measured roster, got {schemas:?}"
    );
}

#[test]
fn explanations_are_deterministic() {
    let w = generated(1);
    let a = format!("{:?}", accounts_of(&w));
    let b = format!("{:?}", accounts_of(&w));
    assert_eq!(
        a, b,
        "accounts_of (post-explain) must be a pure function of the world"
    );
}

#[test]
fn no_deity_bearing_schema_ever_fires_agentless() {
    // Review carry-over (C5 T4): explain_day/explain_moons used to bind
    // agent/lexeme ONLY for SchemaId::Agentive, so a Kinship or
    // LinkSympathy draw (both admitted for FactShape::Count, the moons'
    // shape) would wrap the fact in Explained { agent: None, .. } — a
    // frame windows/book's renderer correctly refuses to emit, silently
    // vanishing two of the six closed frames. Sweep every placed
    // culture's day and moons entries across seeds 1..=10 and assert NONE
    // of the three deity-bearing schemas (Agentive, Kinship,
    // LinkSympathy) ever surfaces with agent == None — the vanishing
    // class is closed regardless of which schema the derived
    // prior/beta competition actually draws.
    let mut kinship_firings = 0usize;
    let mut link_sympathy_firings = 0usize;
    let mut agentive_firings = 0usize;

    for seed in 1..=10u64 {
        let w = generated(seed);
        for voice in accounts_of(&w) {
            for entry in &voice.account.entries {
                let Disposition::Explained { schema, agent, .. } = &entry.disposition else {
                    continue;
                };
                match schema {
                    SchemaId::Agentive => agentive_firings += 1,
                    SchemaId::Kinship => kinship_firings += 1,
                    SchemaId::LinkSympathy => link_sympathy_firings += 1,
                    _ => continue,
                }
                assert!(
                    agent.is_some(),
                    "seed {seed}, {}: {schema:?} fired with agent == None \
                     (predicate {:?}) — the vanishing-frame bug this test closes",
                    voice.kind,
                    entry.fact.predicate
                );
            }
        }
    }

    // The sweep's own finding, reported rather than assumed: at the floor
    // (seeds 1..=10, the shipped four-people roster), the schema
    // competition draws Agentive at least once, but Kinship/LinkSympathy
    // may or may not surface — either is a valid finding. What matters is
    // that IF they fire, they now carry an agent (asserted above); they
    // are no longer unreachable-by-BUG, only possibly unreachable-by-draw
    // at today's floor priors.
    println!(
        "no_deity_bearing_schema_ever_fires_agentless: seeds 1..=10 found \
         {agentive_firings} Agentive, {kinship_firings} Kinship, \
         {link_sympathy_firings} LinkSympathy firing(s)."
    );
}

#[test]
fn moons_explained_only_where_kept() {
    // Seed 2: kobold keeps the moons (moon-count Kept), goblin loses them
    // (BeyondCapability). Measured: kobold's moon-count fires Agentive,
    // bound to the SLOWEST cyclic belief ("Nggo", period 1174.61, the
    // pantheon's longest — manner Slow); goblin's stays plain Lost, never
    // Explained.
    //
    // Re-pinned again under The Living Community epoch (history is the sole
    // settlement placer, this merge): seed 2's kobold re-derived its
    // settlement demography once more, so its language re-mapped and the Slow
    // manner verb shifted "stalks" -> "strides". The agent ("Nggo"), schema
    // (Agentive), underlying (Kept), and manner (Slow) are all unchanged —
    // only the drawn lexeme moved, the expected signature of a language
    // re-derivation. (Prior drifts: strides -> drives -> stalks -> strides.)
    let w = generated(2);
    let voices = accounts_of(&w);
    let kobold = voices
        .iter()
        .find(|v| v.kind == "kobold")
        .expect("kobold must place at seed 2");
    let goblin = voices
        .iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin must place at seed 2");

    let kobold_moon = kobold
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "moon-count")
        .expect("a moon-count ground fact must exist");
    assert_eq!(
        kobold_moon.disposition,
        Disposition::Explained {
            underlying: Box::new(Disposition::Kept),
            schema: SchemaId::Agentive,
            agent: Some("Nggo".to_string()),
            lexeme: Some(hornvale_language::LexemeId("strides")),
            manner: Manner::Slow,
        }
    );

    let goblin_moon = goblin
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "moon-count")
        .expect("a moon-count ground fact must exist");
    assert!(
        !matches!(goblin_moon.disposition, Disposition::Explained { .. }),
        "goblin's lost moon-count must never gain an Explained wrapper, got {:?}",
        goblin_moon.disposition
    );
    assert_eq!(
        goblin_moon.disposition,
        Disposition::Lost(hornvale_language::LossReason::BeyondCapability { domain: "sky" })
    );
}
