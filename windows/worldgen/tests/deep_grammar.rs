//! The Deep Grammar (C7 Task 2): the worldgen readouts —
//! `tongue_morphology_of`, `day_schema_of` (the SAME draw `explain_day`
//! resolves — no drift between the two readers), and the derived
//! `noun_class_of` animacy coherence law — measured against live worlds and
//! pinned exact.

use hornvale_language::{Disposition, MorphDepth, NounClass, SchemaId};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, accounts_of, day_schema_of, noun_class_of, placed_peoples,
    tongue_morphology_of,
};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// neighboring worldgen integration test (`chorus_params.rs`,
/// `explanations.rs`) uses.
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
fn the_coherence_law() {
    // Every placed culture: noun_class_of(.., "sun") is Animate IFF
    // day_schema_of == Some(Agentive); same for "moon"/"earth"/"star";
    // "<kind>-kind" always Animate; a terrain concept (e.g. "forest")
    // always Inanimate. Measure which cultures are agentive (never force
    // it) — C5 measured PathJourney/CycleReturn/Balance at 1..=3; the DAY
    // schema draws Agentive at seed 4 bugbear
    // (`explanations.rs::the_day_binds_by_period_match_never_identity`), so
    // seed 4 joins the sweep specifically to reach it (each of
    // day_schema_of/noun_class_of independently re-derives a whole Account
    // per call — the re-derivation idiom every readout in this module
    // follows — so a full battery over ALL of 1..=10 costs meaningfully
    // more than this targeted set for no added coverage) — assert at least
    // one Animate-sky culture across the sweep, else panic demanding a
    // wider sweep still.
    // Re-pointed under The Living Community epoch (this merge): the prior
    // Agentive anchor (seed 10 hobgoblin) no longer fires Agentive after the
    // world re-placement, so the sweep now includes seed 4 (bugbear), the
    // nearest surviving Agentive-day culture.
    let mut any_animate_sky = false;
    for seed in [1, 2, 3, 4, 10] {
        let w = generated(seed);
        for (kind, _village) in placed_peoples(&w) {
            let schema = day_schema_of(&w, kind);
            let expect_animate = schema == Some(SchemaId::Agentive);
            if expect_animate {
                any_animate_sky = true;
                println!("the_coherence_law: Animate-sky override fires at seed {seed} {kind}");
            }
            let expected = if expect_animate {
                NounClass::Animate
            } else {
                NounClass::Inanimate
            };
            for concept in ["sun", "moon", "earth", "star"] {
                assert_eq!(
                    noun_class_of(&w, kind, concept),
                    expected,
                    "seed {seed} {kind} concept {concept:?}: day_schema_of == {schema:?}"
                );
            }

            let kind_concept = format!("{kind}-kind");
            assert_eq!(
                noun_class_of(&w, kind, &kind_concept),
                NounClass::Animate,
                "seed {seed} {kind}: '{kind_concept}' must always be Animate"
            );
            assert_eq!(
                noun_class_of(&w, kind, "forest"),
                NounClass::Inanimate,
                "seed {seed} {kind}: a terrain concept must always be Inanimate"
            );
        }
    }
    assert!(
        any_animate_sky,
        "seeds [1, 2, 3, 10] across the placed roster found NO Animate-sky (Agentive \
         day-schema) culture — the override arm went untested; widen the sweep"
    );
}

#[test]
fn day_schema_of_matches_the_explained_entry() {
    // For a culture whose day entry IS Explained (seed 1 goblin):
    // day_schema_of == Some(that entry's schema) — the accessor and the
    // explain pass resolve the SAME draw (no drift between the two readers
    // of one stream).
    let w = generated(1);
    let voices = accounts_of(&w);
    let goblin = voices
        .iter()
        .find(|v| v.kind == "goblin")
        .expect("goblin must place at seed 1");
    let day = goblin
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "day-length-std")
        .expect("a day-length-std ground fact must exist at seed 1");
    let Disposition::Explained { schema, .. } = day.disposition else {
        panic!(
            "seed 1 goblin's day-length-std entry must be Explained for this test to exercise \
             the matching path, got {:?}",
            day.disposition
        );
    };
    assert_eq!(
        day_schema_of(&w, "goblin"),
        Some(schema),
        "day_schema_of must resolve the SAME draw explain_day already resolved into this account"
    );
}

#[test]
fn morphology_is_deterministic() {
    let w = generated(1);
    let a = format!("{:?}", tongue_morphology_of(&w, "goblin").unwrap());
    let b = format!("{:?}", tongue_morphology_of(&w, "goblin").unwrap());
    assert_eq!(
        a, b,
        "tongue_morphology_of must be a pure function of (world, species)"
    );
}

#[test]
fn depth_landscape_measured() {
    // Sweep seeds 1..=3 x placed species: print + pin the drawn depth
    // triple per species (exact values — the blast-radius map for T3). At
    // least one species must draw a non-None depth somewhere in 1..=3, else
    // PANIC demanding the weights be revisited at G6 (the Book must SHOW
    // morphology somewhere in its three volumes).
    let mut any_non_none = false;
    let mut measured: Vec<(u64, &'static str, MorphDepth, MorphDepth)> = Vec::new();
    for seed in 1..=3u64 {
        let w = generated(seed);
        for (kind, _village) in placed_peoples(&w) {
            let morph = tongue_morphology_of(&w, kind)
                .unwrap_or_else(|e| panic!("seed {seed} {kind}: tongue_morphology_of failed: {e}"));
            if morph.evidential_depth != MorphDepth::None
                || morph.noun_class_depth != MorphDepth::None
            {
                any_non_none = true;
            }
            measured.push((seed, kind, morph.evidential_depth, morph.noun_class_depth));
        }
    }

    for (seed, kind, evidential, noun_class) in &measured {
        println!(
            "depth_landscape_measured: seed {seed} {kind}: evidential={evidential:?} noun_class={noun_class:?}"
        );
    }

    // Pinned exact (measured seeds 1..=3, the shipped four-people roster).
    // Re-pinned under The Living Community epoch (history is the sole
    // settlement placer, this merge): the deep-history bake's genesis seeds
    // ALL FOUR peoples on every world, so bugbear and kobold now place at
    // every seed 1..=3 (previously bugbear never placed and kobold only at
    // seed 2). The landscape is now the full 12-row (4 peoples x 3 seeds)
    // grid — re-measured and re-pinned exactly below.
    let expect: Vec<(u64, &str, MorphDepth, MorphDepth)> = vec![
        (1, "bugbear", MorphDepth::Affix, MorphDepth::None),
        (1, "goblin", MorphDepth::None, MorphDepth::Affix),
        (1, "hobgoblin", MorphDepth::Particle, MorphDepth::Particle),
        (1, "kobold", MorphDepth::None, MorphDepth::None),
        (2, "bugbear", MorphDepth::Affix, MorphDepth::Particle),
        (2, "goblin", MorphDepth::None, MorphDepth::None),
        (2, "hobgoblin", MorphDepth::Particle, MorphDepth::Affix),
        (2, "kobold", MorphDepth::None, MorphDepth::None),
        (3, "bugbear", MorphDepth::None, MorphDepth::Affix),
        (3, "goblin", MorphDepth::Particle, MorphDepth::None),
        (3, "hobgoblin", MorphDepth::None, MorphDepth::Affix),
        (3, "kobold", MorphDepth::None, MorphDepth::Affix),
    ];
    assert_eq!(
        measured, expect,
        "the measured depth landscape drifted from the pinned exact triples \
         (re-measure and re-pin deliberately if this is an intended change)"
    );

    assert!(
        any_non_none,
        "seeds 1..=3 across the placed roster drew MorphDepth::None on both axes for every \
         species — the deep-grammar weights never surface at this floor; revisit the weights \
         at G6 (the Book must SHOW morphology somewhere in its three volumes)"
    );
}
