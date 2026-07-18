//! Param derivation at the composition root (C4 Task 3): `AccountParams`
//! derived from existing authored/committed state — the `voice_params`
//! twin — plus the authored observability table and the ground fact list.
use hornvale_language::{Disposition, LossReason, OrderPolicy, Stance};
use hornvale_worldgen::{
    SettlementPins, SkyChoice, accounts_of, build_world, chorus_ground, pathological_params,
    sky_capability,
};

/// Build a world with the shipped four-people component set, generated sky,
/// default terrain/settlement pins — the shared pattern every neighboring
/// worldgen integration test (`exposure.rs`, `species_worlds.rs`) uses.
fn generated(seed: u64) -> hornvale_kernel::World {
    build_world(
        hornvale_kernel::Seed(seed),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

#[test]
fn sky_capability_matches_the_authored_roster() {
    let perception = hornvale_species::perception_registry();
    let goblin = perception.get_by_label("goblin").unwrap();
    let hobgoblin = perception.get_by_label("hobgoblin").unwrap();
    let kobold = perception.get_by_label("kobold").unwrap();
    let bugbear = perception.get_by_label("bugbear").unwrap();

    assert_eq!(sky_capability(goblin), 0.5);
    assert_eq!(sky_capability(hobgoblin), 0.55);
    assert_eq!(sky_capability(kobold), 1.0);
    assert_eq!(sky_capability(bugbear), 0.65);
}

#[test]
fn goblin_params_derive_from_existing_state_only() {
    // Seed 1: goblin and hobgoblin both place (the default world's shared
    // joint-greedy placement pass — see windows/worldgen/tests/exposure.rs).
    let w = generated(1);
    let params = hornvale_worldgen::account_params_of(&w, "goblin").unwrap();

    for concept in [
        "earth",
        "moon",
        "sun",
        "star",
        "goblin-kind",
        "hobgoblin-kind",
    ] {
        assert!(
            params.holdings.contains(concept),
            "goblin's holdings should contain {concept:?}, got {:?}",
            params.holdings
        );
    }
    assert_eq!(params.world_carving, Some("earth".to_string()));
    assert_eq!(params.order, OrderPolicy::Salience { sky_first: false });
    assert_eq!(
        params.stances.get("hobgoblin").copied(),
        Some(Stance::Neighbors)
    );
    assert_eq!(
        params.stances.get("goblin").copied(),
        Some(Stance::Ourselves)
    );
}

#[test]
fn hobgoblin_reads_rivals_where_goblin_reads_neighbors() {
    let w = generated(1);
    let goblin_params = hornvale_worldgen::account_params_of(&w, "goblin").unwrap();
    let hobgoblin_params = hornvale_worldgen::account_params_of(&w, "hobgoblin").unwrap();

    assert_eq!(
        goblin_params.stances.get("hobgoblin").copied(),
        Some(Stance::Neighbors)
    );
    assert_eq!(
        hobgoblin_params.stances.get("goblin").copied(),
        Some(Stance::Rivals)
    );
    assert_eq!(
        hobgoblin_params.stances.get("hobgoblin").copied(),
        Some(Stance::Ourselves)
    );
}

#[test]
fn kobold_keeps_the_moons_goblin_loses() {
    // Seed 2: kobold places (a different seed's attractor-dominance draw —
    // see the C4 decision ledger's seed-2 sky-calibration note).
    let w = generated(2);
    let voices = accounts_of(&w);
    let kobold_voice = voices
        .iter()
        .find(|v| v.kind == "kobold")
        .expect("kobold must place at seed 2");
    let goblin_voice = voices.iter().find(|v| v.kind == "goblin");

    let kobold_moon = kobold_voice
        .account
        .entries
        .iter()
        .find(|e| e.fact.predicate == "moon-count")
        .expect("a moon-count ground fact must exist");
    assert_eq!(kobold_moon.disposition, Disposition::Kept);

    if let Some(goblin_voice) = goblin_voice {
        let goblin_moon = goblin_voice
            .account
            .entries
            .iter()
            .find(|e| e.fact.predicate == "moon-count")
            .expect("a moon-count ground fact must exist");
        assert_eq!(
            goblin_moon.disposition,
            Disposition::Lost(LossReason::BeyondCapability { domain: "sky" })
        );
    }

    for voice in &voices {
        let star = voice
            .account
            .entries
            .iter()
            .find(|e| e.fact.predicate == "star-class");
        if let Some(star) = star {
            assert_eq!(
                star.disposition,
                Disposition::Lost(LossReason::BeyondCapability { domain: "sky" }),
                "{}'s star-class must be lost (Instrumental)",
                voice.kind
            );
        }
        let day = voice
            .account
            .entries
            .iter()
            .find(|e| e.fact.predicate == "day-length-std");
        if let Some(day) = day {
            assert_eq!(
                day.disposition,
                Disposition::Lost(LossReason::BeyondCapability { domain: "sky" }),
                "{}'s day-length-std must be lost (CrossReferential)",
                voice.kind
            );
        }
        let is_a = voice
            .account
            .entries
            .iter()
            .find(|e| e.fact.predicate == "is-a");
        if let Some(is_a) = is_a {
            match &is_a.disposition {
                Disposition::Substituted { truth, theirs } => {
                    assert_eq!(truth, "planet");
                    assert_eq!(theirs, "earth");
                }
                other => panic!("{}'s is-a should be Substituted, got {other:?}", voice.kind),
            }
        }
    }
}

#[test]
fn accounts_are_deterministic() {
    let w = generated(1);
    let a = format!("{:?}", accounts_of(&w));
    let b = format!("{:?}", accounts_of(&w));
    assert_eq!(a, b, "accounts_of must be a pure function of the world");
}

#[test]
fn pathological_pole_exercises_loss_and_collision() {
    let w = generated(2);
    let ground = chorus_ground(&w);
    let pathological = pathological_params();
    let account = hornvale_language::account_of(&ground, &pathological);

    assert!(
        account
            .entries
            .iter()
            .all(|e| !matches!(e.disposition, Disposition::Kept)),
        "every entry must be Lost or Substituted under the pathological pole"
    );

    let substituted_to_earth = account
        .entries
        .iter()
        .filter(|e| {
            matches!(&e.disposition, Disposition::Substituted { theirs, .. } if theirs == "earth")
        })
        .count();
    assert!(
        substituted_to_earth >= 2,
        "at least two entries must collide on the same substituted target 'earth', got {substituted_to_earth}"
    );
    assert_eq!(
        hornvale_language::recoverability(&account),
        0.0,
        "the injectivity collision must destroy recoverability entirely"
    );

    let distortion = hornvale_language::distortion(&account);
    assert!(distortion > 0.0);

    // The gibberish-pole contrast: measured (not narrated) against the
    // world's shipped voices. `distortion()` folds three components (loss,
    // order-distance, stance) into one scalar, and order-distance is driven
    // by each culture's OWN `OrderPolicy` (Task 3's `sky_first: sky_attention
    // >= 0.6`) rather than by how much truth it lost. At this seed, goblin
    // and hobgoblin's `sky_first: false` reorders three `instance-of` facts
    // ahead of four sky facts — a large permutation whose order-distance
    // component alone can outweigh the pathological pole's fully-lost-but-
    // ground-ordered account, so a raw `distortion(pathological) >
    // distortion(goblin)` comparison is NOT a world-invariant claim (verified
    // false at seeds 1-6 for the sky_first:false cultures; a real finding
    // about the metric's order/loss conflation, not a derivation bug — see
    // the C4 task-3 report). The invariant that DOES hold, checked directly
    // rather than through the conflated scalar: every shipped voice keeps at
    // least one fact (its own and its neighbors'/rivals' `instance-of`,
    // Manifest and Steeped by construction), while the pathological pole
    // keeps none — already established by the `all(... !Kept)` assertion
    // above. Made explicit here as the pole's defining contrast.
    let voices = accounts_of(&w);
    assert!(
        !voices.is_empty(),
        "seed 2 must place at least one people to contrast against"
    );
    for voice in &voices {
        assert!(
            voice
                .account
                .entries
                .iter()
                .any(|e| matches!(e.disposition, Disposition::Kept)),
            "{}'s shipped account must keep at least one fact, unlike the pathological pole",
            voice.kind
        );
        assert!(hornvale_language::distortion(&voice.account) > 0.0);
    }
}
