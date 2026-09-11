//! The Deep Grammar (C7 Task 2): the worldgen readouts —
//! `tongue_morphology_of`, `day_schema_from` (the SAME draw `explain_day`
//! resolves — no drift between the two readers), and the derived
//! `noun_class_from` animacy coherence law — measured against live worlds and
//! pinned exact.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_language::{Disposition, MorphDepth, NounClass, SchemaId};
use hornvale_worldgen::{
    SettlementPins, accounts_from, day_schema_from, noun_class_from, placed_peoples,
    tongue_morphology_of, tongue_paradigm_of,
};

/// Build a world with the shipped four-people component set, generated
/// sky, default terrain/settlement pins — the shared pattern every
/// neighboring worldgen integration test (`chorus_params.rs`,
/// `explanations.rs`) uses.
fn generated(seed: u64) -> hornvale_kernel::World {
    hornvale_worldgen::build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// claim: invariant(census: none yet) — noun_class/day_schema biconditional
/// over a targeted seed set, with an embedded vacuity guard
/// (any_animate_sky) riding on the same sweep
#[test]
fn the_coherence_law() {
    // Every placed culture: noun_class_from(.., "sun") is Animate IFF
    // day_schema_from == Some(Agentive); same for "moon"/"earth"/"star";
    // "<kind>-kind" always Animate; a terrain concept (e.g. "forest")
    // always Inanimate. Measure which cultures are agentive (never force
    // it) — C5 measured PathJourney/CycleReturn/Balance at 1..=3; the DAY
    // schema draws Agentive at seed 4 bugbear
    // (`explanations.rs::the_day_binds_by_period_match_never_identity`), so
    // seed 4 joins the sweep specifically to reach it (each of
    // day_schema_from/noun_class_from independently re-derives a whole Account
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
        let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
        let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
        for (kind, _village) in placed_peoples(&w) {
            let schema = day_schema_from(&w, kind, &terrain, &climate);
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
                    noun_class_from(&w, kind, concept, &terrain, &climate),
                    expected,
                    "seed {seed} {kind} concept {concept:?}: day_schema_from == {schema:?}"
                );
            }

            let kind_concept = format!("{kind}-kind");
            assert_eq!(
                noun_class_from(&w, kind, &kind_concept, &terrain, &climate),
                NounClass::Animate,
                "seed {seed} {kind}: '{kind_concept}' must always be Animate"
            );
            assert_eq!(
                noun_class_from(&w, kind, "forest", &terrain, &climate),
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
fn day_schema_from_matches_the_explained_entry() {
    // For a culture whose day entry IS Explained (seed 1 goblin):
    // day_schema_from == Some(that entry's schema) — the accessor and the
    // explain pass resolve the SAME draw (no drift between the two readers
    // of one stream).
    let w = generated(1);
    let terrain = hornvale_worldgen::terrain_of(&w).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_from(&w, &terrain).expect("climate derives");
    let voices = accounts_from(&w, &terrain, &climate);
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
        day_schema_from(&w, "goblin", &terrain, &climate),
        Some(schema),
        "day_schema_from must resolve the SAME draw explain_day already resolved into this account"
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

/// The composition root fills the pronoun inventory, so a pronoun realizes
/// for a PRODUCTION caller and not only in `domains/language`'s own tests.
///
/// The Inquest's Task 7 drew `pronoun_forms`; without this assembler every
/// window would still hand `realize_tongue_deep` a bundle with an empty
/// inventory, the pronoun would gap exactly as it did before the campaign,
/// and the crate-local tests would pass anyway. That is the hazard this test
/// exists to close: it asserts the six rows arrive on a REAL world, for every
/// placed people.
/// claim: structural(seed: [1,2,3]) — the assembler fills every row for every
/// placed people; not a rate, so three seeds is the whole claim
#[test]
fn every_placed_people_draws_a_full_pronoun_inventory() {
    let expected: Vec<&str> = hornvale_language::Person::ALL
        .into_iter()
        .flat_map(|person| {
            [hornvale_language::Number::Sg, hornvale_language::Number::Pl]
                .into_iter()
                .map(move |number| person.paradigm_key(number))
        })
        .collect();
    let mut seen_any = false;
    for seed in 1..=3u64 {
        let w = generated(seed);
        for (kind, _village) in placed_peoples(&w) {
            let morph = tongue_morphology_of(&w, kind)
                .unwrap_or_else(|e| panic!("seed {seed} {kind}: {e}"));
            for key in &expected {
                let form = morph
                    .pronouns
                    .get(key)
                    .unwrap_or_else(|| panic!("seed {seed} {kind}: no {key} pronoun"));
                assert!(
                    !form.roman.is_empty(),
                    "seed {seed} {kind}: {key} drew an empty pronoun"
                );
            }
            assert_eq!(
                morph.pronouns.len(),
                expected.len(),
                "seed {seed} {kind}: the inventory carries exactly the drawn rows"
            );
            seen_any = true;
        }
    }
    assert!(
        seen_any,
        "at least one people must place across seeds 1..=3"
    );
}

/// The Inquest T8b: every placed people's assembled `TongueParadigm` carries
/// what `paradigm_depths` drew for that species, plus a real marker form on
/// each of the two axes `realize_tongue_deep` reads.
///
/// The assertion that makes this non-vacuous is the EQUALITY against
/// `paradigm_depths`' own draw: an assembler that returned a default or empty
/// bundle would still type-check, still be called by `windows/book`, and
/// still change no rendered byte (the book's clauses are all present-tense
/// and positive), so nothing else in the tree would object to it.
/// claim: structural(seed: [1,2,3]) — the assembler carries the drawn depths
/// and both marked markers for every placed people; not a rate, so three
/// seeds is the whole claim
#[test]
fn every_placed_people_assembles_its_drawn_paradigm() {
    let mut seen_any = false;
    for seed in 1..=3u64 {
        let w = generated(seed);
        for (kind, _village) in placed_peoples(&w) {
            let paradigm =
                tongue_paradigm_of(&w, kind).unwrap_or_else(|e| panic!("seed {seed} {kind}: {e}"));
            assert_eq!(
                paradigm.depths,
                hornvale_language::paradigm::paradigm_depths(&w.seed, kind),
                "seed {seed} {kind}: the bundle must carry the depths paradigm_depths drew"
            );
            let past = paradigm
                .tense
                .get("past")
                .unwrap_or_else(|| panic!("seed {seed} {kind}: no past marker"));
            assert!(
                !past.roman.is_empty() && !past.segments.is_empty(),
                "seed {seed} {kind}: the past marker is empty"
            );
            let negative = paradigm
                .polarity
                .get("negative")
                .unwrap_or_else(|| panic!("seed {seed} {kind}: no negative marker"));
            assert!(
                !negative.roman.is_empty() && !negative.segments.is_empty(),
                "seed {seed} {kind}: the negative marker is empty"
            );
            // Exactly the marked members: present and positive are the zero
            // members and must never acquire a form (spec §4.1).
            assert_eq!(
                (paradigm.tense.len(), paradigm.polarity.len()),
                (1, 1),
                "seed {seed} {kind}: only the marked member of each axis is drawn"
            );
            seen_any = true;
        }
    }
    assert!(
        seen_any,
        "at least one people must place across seeds 1..=3"
    );
}

/// claim: readout — prints the drawn depth triple per seed x species,
/// morphology depth landscape over seeds 1..=3
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

    // Pinned exact (measured seeds 1..=3, the shipped five-people roster).
    // Re-pinned under The Living Community epoch (history is the sole
    // settlement placer, this merge): the deep-history bake's genesis seeds
    // ALL FOUR (then-)peoples on every world, so bugbear and kobold now place
    // at every seed 1..=3 (previously bugbear never placed and kobold only at
    // seed 2). The Vacancy T9 adds a fifth people (the gnoll), which also
    // places at every seed 1..=3; the landscape is now the full 15-row (5
    // peoples x 3 seeds) grid — re-measured and re-pinned exactly below.
    //
    // The Generalist re-pin (2026-08-03): human is a sixth Settled people
    // and places at every seed 1..=3 too, growing the grid to the full
    // 18-row (6 peoples x 3 seeds) landscape — re-measured and re-pinned
    // exactly below.
    //
    // The Delvers re-pin (C2c, 2026-08-07): the three dwarves are Settled
    // peoples seven through nine and every one of them places at every
    // seed 1..=3, so the grid is the full 27-row (9 peoples x 3 seeds)
    // landscape. **The eighteen pre-existing rows are byte-identical** —
    // re-measured, not assumed: a people's morphological depth is drawn per
    // people and does not move when the roster around it grows, even though
    // settlement PLACEMENT moved everywhere (which is what redecided the
    // lexicons' exposure, a different quantity).
    //
    // **The roster cut to three (spec §11) moved NOTHING here.** This grid
    // was first pinned at 33 rows with five dwarves; withdrawing duergar and
    // mountain-dwarf deleted their six rows and left the other twenty-seven
    // byte-identical, which is the same per-people independence stated above,
    // now measured in the shrinking direction as well as the growing one.
    // THE RADIATION re-pin (C2d, 2026-08-10): 27 -> 45 rows, six elves placed
    // on each of seeds 1..=3. **Not one of the 27 pre-existing rows moved.**
    // That is worth stating because it is not what the neighbouring re-pins in
    // this suite did — the ladder table, the exposure sets and the toponymy
    // all shifted for existing peoples when the roster grew. A tongue's
    // morphology depth is drawn per SPECIES from its own labels, so it is
    // insensitive to who else is in the world; settlement placement is not.
    // The two behave differently under a roster change and this row-for-row
    // equality is the cleanest evidence of it in the suite.
    let expect: Vec<(u64, &str, MorphDepth, MorphDepth)> = vec![
        // THE TIDEMARK re-pin (Task 3, 2026-09-11): 45 -> 60 rows, five settling
        // marine peoples placed on each of seeds 1..=3 (merfolk is `Gregarious`
        // and places nothing, so it is absent here as it is everywhere placement
        // is the subject). **Not one of the 45 pre-existing rows moved**, which
        // is the fourth consecutive roster change at which that has held — and
        // it is worth restating rather than assuming, because the neighbouring
        // re-pins in this suite (the ladder table, the exposure sets, the
        // toponymy) ALL moved for existing peoples this time. A tongue's
        // morphology depth is drawn per SPECIES from its own labels; settlement
        // placement is not. That is the difference, measured again.
        (1, "abyssal-elf", MorphDepth::None, MorphDepth::None),
        (1, "bugbear", MorphDepth::Affix, MorphDepth::None),
        (1, "desert-dwarf", MorphDepth::None, MorphDepth::None),
        (1, "desert-elf", MorphDepth::Affix, MorphDepth::Affix),
        (1, "drow", MorphDepth::None, MorphDepth::None),
        (1, "gnoll", MorphDepth::None, MorphDepth::Affix),
        (1, "goblin", MorphDepth::None, MorphDepth::Affix),
        (1, "gully-dwarf", MorphDepth::None, MorphDepth::None),
        (1, "high-elf", MorphDepth::None, MorphDepth::Particle),
        (1, "hill-dwarf", MorphDepth::None, MorphDepth::None),
        (1, "hobgoblin", MorphDepth::Particle, MorphDepth::Particle),
        (1, "human", MorphDepth::Affix, MorphDepth::Affix),
        (1, "kelp-tender", MorphDepth::Particle, MorphDepth::Affix),
        (1, "kobold", MorphDepth::None, MorphDepth::None),
        (1, "reef-mason", MorphDepth::None, MorphDepth::Affix),
        (1, "sea-elf", MorphDepth::None, MorphDepth::None),
        (1, "snow-elf", MorphDepth::None, MorphDepth::None),
        (1, "triton", MorphDepth::Affix, MorphDepth::None),
        (
            1,
            "vent-commensal",
            MorphDepth::Particle,
            MorphDepth::Particle,
        ),
        (1, "wood-elf", MorphDepth::Particle, MorphDepth::None),
        (2, "abyssal-elf", MorphDepth::None, MorphDepth::None),
        (2, "bugbear", MorphDepth::Affix, MorphDepth::Particle),
        (2, "desert-dwarf", MorphDepth::Particle, MorphDepth::None),
        (2, "desert-elf", MorphDepth::None, MorphDepth::Affix),
        (2, "drow", MorphDepth::None, MorphDepth::Particle),
        (2, "gnoll", MorphDepth::None, MorphDepth::Particle),
        (2, "goblin", MorphDepth::None, MorphDepth::None),
        (2, "gully-dwarf", MorphDepth::None, MorphDepth::Particle),
        (2, "high-elf", MorphDepth::Affix, MorphDepth::None),
        (2, "hill-dwarf", MorphDepth::Particle, MorphDepth::Particle),
        (2, "hobgoblin", MorphDepth::Particle, MorphDepth::Affix),
        (2, "human", MorphDepth::None, MorphDepth::None),
        (2, "kelp-tender", MorphDepth::None, MorphDepth::Particle),
        (2, "kobold", MorphDepth::None, MorphDepth::None),
        (2, "reef-mason", MorphDepth::None, MorphDepth::Affix),
        (2, "sea-elf", MorphDepth::None, MorphDepth::Affix),
        (2, "snow-elf", MorphDepth::None, MorphDepth::None),
        (2, "triton", MorphDepth::Particle, MorphDepth::None),
        (2, "vent-commensal", MorphDepth::Particle, MorphDepth::None),
        (2, "wood-elf", MorphDepth::None, MorphDepth::Affix),
        (3, "abyssal-elf", MorphDepth::None, MorphDepth::Affix),
        (3, "bugbear", MorphDepth::None, MorphDepth::Affix),
        (3, "desert-dwarf", MorphDepth::Affix, MorphDepth::None),
        (3, "desert-elf", MorphDepth::Particle, MorphDepth::Particle),
        (3, "drow", MorphDepth::Particle, MorphDepth::None),
        (3, "gnoll", MorphDepth::None, MorphDepth::Affix),
        (3, "goblin", MorphDepth::Particle, MorphDepth::None),
        (3, "gully-dwarf", MorphDepth::None, MorphDepth::None),
        (3, "high-elf", MorphDepth::Particle, MorphDepth::Particle),
        (3, "hill-dwarf", MorphDepth::None, MorphDepth::None),
        (3, "hobgoblin", MorphDepth::None, MorphDepth::Affix),
        (3, "human", MorphDepth::Particle, MorphDepth::None),
        (3, "kelp-tender", MorphDepth::Particle, MorphDepth::None),
        (3, "kobold", MorphDepth::None, MorphDepth::Affix),
        (3, "reef-mason", MorphDepth::None, MorphDepth::Affix),
        (3, "sea-elf", MorphDepth::Affix, MorphDepth::Particle),
        (3, "snow-elf", MorphDepth::Particle, MorphDepth::None),
        (3, "triton", MorphDepth::Particle, MorphDepth::None),
        (
            3,
            "vent-commensal",
            MorphDepth::Particle,
            MorphDepth::Particle,
        ),
        (3, "wood-elf", MorphDepth::None, MorphDepth::None),
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
