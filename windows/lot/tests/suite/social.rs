use hornvale_kernel::{Lineage, Seed, Value, WorldTime};
use hornvale_lot::context::assemble;
use hornvale_lot::draw::draw;
use hornvale_lot::json::life_json;
use hornvale_lot::narrate::narrate;
use hornvale_lot::slots::{Silence, SlotValue, tell};
use hornvale_lot::{LotIndex, Pick};
use std::collections::BTreeMap;

fn social_world() -> (hornvale_kernel::World, hornvale_kernel::EntityId) {
    let mut world = hornvale_worldgen::seed_42_world();
    let person = world.ledger.mint_entity(Lineage {
        parent: None,
        role: "lot-social-test-person",
        ordinal: 0,
    });
    for (predicate, object) in [
        (hornvale_person::IS_PERSON, Value::Flag(true)),
        (
            hornvale_person::SEX_TRAIT,
            Value::Text("variant-a".to_string()),
        ),
        (
            hornvale_person::REPRODUCTIVE_ROLE,
            Value::Text("development-supporter".to_string()),
        ),
        (
            hornvale_person::PERSON_SOCIAL_PROVENANCE,
            Value::Text("lot-social-test".to_string()),
        ),
    ] {
        world
            .ledger
            .commit(
                hornvale_kernel::Fact {
                    subject: person,
                    predicate: predicate.to_string(),
                    object,
                    place: None,
                    day: Some(WorldTime::GENESIS),
                    provenance: "lot-social-test".to_string(),
                },
                &world.registry,
            )
            .unwrap();
    }
    (world, person)
}

#[test]
fn social_axes_are_sourced_and_missing_identity_is_silent() {
    let (mut world, person) = social_world();
    let initial = assemble(&world).unwrap();
    let target = initial
        .occupations
        .iter()
        .find(|occupation| {
            !initial
                .social_people_by_occupation
                .contains_key(&occupation.record.id)
        })
        .expect("the synthetic identity has an unclaimed occupation")
        .record
        .clone();
    world
        .ledger
        .commit(
            hornvale_kernel::Fact {
                subject: person,
                predicate: hornvale_person::PERSON_FOUNDED.to_string(),
                object: Value::Entity(target.id),
                place: None,
                day: Some(WorldTime::GENESIS),
                provenance: "lot-social-test".to_string(),
            },
            &world.registry,
        )
        .unwrap();
    let ctx = assemble(&world).unwrap();
    assert!(
        !ctx.social_people.is_empty(),
        "social people were not indexed"
    );
    let life = draw(
        &ctx,
        LotIndex(0),
        &Pick {
            year: Some(target.core.founded + 0.01),
            site: Some(target.core.site),
        },
    )
    .unwrap();
    let story = tell(&world, &ctx, &life);

    assert!(matches!(
        story.slot("sex").unwrap().value,
        SlotValue::Filled(_)
    ));
    assert!(matches!(
        story.slot("reproductive-role").unwrap().value,
        SlotValue::Filled(_)
    ));
    assert!(matches!(
        story.slot("gender-identity").unwrap().value,
        SlotValue::Silent(Silence::NoFact(_))
    ));
    assert!(!story.slot("sex").unwrap().sources.is_empty());

    let payload: serde_json::Value = serde_json::from_str(&life_json(&ctx, &life, &story)).unwrap();
    let sex = payload["slots"]
        .as_array()
        .unwrap()
        .iter()
        .find(|slot| slot["key"] == "sex")
        .unwrap();
    assert!(sex["value"].is_string());
    assert!(!sex["sources"].as_array().unwrap().is_empty());
    let gender = payload["slots"]
        .as_array()
        .unwrap()
        .iter()
        .find(|slot| slot["key"] == "gender-identity")
        .unwrap();
    assert_eq!(gender["silence"]["kind"], "no-fact");
    assert!(
        narrate(&ctx, &life, &story).contains("Nothing in the record says how they identified")
    );
}

#[test]
fn ordinary_worlds_keep_social_silence_without_by_design_household_claims() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);

    assert!(matches!(
        story.slot("sex").unwrap().value,
        SlotValue::Silent(Silence::NoFact(_))
    ));
    assert!(matches!(
        story.slot("family").unwrap().value,
        SlotValue::Silent(Silence::NoFact(_))
    ));
}

#[test]
fn approved_worldgen_projection_reaches_lot_without_vacuous_social_slots() {
    let mut world = hornvale_worldgen::seed_42_world();
    let projection = hornvale_worldgen::approved_lot_probe_projection(Seed(42)).unwrap();
    assert!(
        projection
            .events()
            .iter()
            .flat_map(|event| event.facts())
            .any(|fact| fact.predicate == "residence")
    );
    hornvale_worldgen::emit_social_projection(&mut world, Some(&projection)).unwrap();
    let initial = assemble(&world).unwrap();
    let target = initial
        .occupations
        .iter()
        .find(|occupation| {
            !initial
                .social_people_by_occupation
                .contains_key(&occupation.record.id)
        })
        .expect("approved projection leaves an unclaimed occupation for the Lot identity")
        .record
        .clone();
    let pick = Pick {
        year: Some(target.core.founded + 0.01),
        site: Some(target.core.site),
    };
    let life = draw(&initial, LotIndex(0), &pick).unwrap();
    let mut residence_counts = BTreeMap::new();
    for fact in projection
        .events()
        .iter()
        .flat_map(|event| event.facts())
        .filter(|fact| fact.predicate == "residence")
    {
        *residence_counts.entry(fact.subject).or_insert(0usize) += 1;
    }
    let person = residence_counts
        .into_iter()
        .max_by_key(|(_, count)| *count)
        .unwrap()
        .0;
    world
        .ledger
        .commit(
            hornvale_kernel::Fact {
                subject: person,
                predicate: hornvale_person::PERSON_FOUNDED.to_string(),
                object: Value::Entity(life.occupation),
                place: None,
                day: Some(WorldTime::GENESIS),
                provenance: "lot-social-probe".to_string(),
            },
            &world.registry,
        )
        .unwrap();
    let ctx = assemble(&world).unwrap();
    let story = tell(&world, &ctx, &draw(&ctx, LotIndex(0), &pick).unwrap());
    assert!(matches!(
        story.slot("migration").unwrap().value,
        SlotValue::Filled(_)
    ));
    assert!(!story.slot("migration").unwrap().sources.is_empty());
}

#[test]
fn ended_axis_facts_are_not_read_for_a_life_boundary() {
    let (mut world, person) = social_world();
    let initial = assemble(&world).unwrap();
    let drawn = draw(&initial, LotIndex(0), &Pick::default()).unwrap();
    for fact in [
        hornvale_kernel::Fact {
            subject: person,
            predicate: hornvale_person::PERSON_FOUNDED.to_string(),
            object: Value::Entity(drawn.occupation),
            place: None,
            day: Some(WorldTime::GENESIS),
            provenance: "lot-social-test".to_string(),
        },
        hornvale_kernel::Fact {
            subject: person,
            predicate: hornvale_person::SEX_TRAIT_ENDED.to_string(),
            object: Value::Text("variant-a".to_string()),
            place: None,
            day: Some(WorldTime::from_std_days(0.01).unwrap()),
            provenance: "lot-social-test".to_string(),
        },
    ] {
        world.ledger.commit(fact, &world.registry).unwrap();
    }
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    assert!(matches!(
        story.slot("sex").unwrap().value,
        SlotValue::Silent(Silence::NoFact(_))
    ));
}
