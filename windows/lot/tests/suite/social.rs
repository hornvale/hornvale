use hornvale_kernel::{Lineage, Value, WorldTime};
use hornvale_lot::context::assemble;
use hornvale_lot::draw::draw;
use hornvale_lot::json::life_json;
use hornvale_lot::narrate::narrate;
use hornvale_lot::slots::{Silence, SlotValue, tell};
use hornvale_lot::{LotIndex, Pick};

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
    let (world, _) = social_world();
    let ctx = assemble(&world).unwrap();
    assert!(
        !ctx.social_people.is_empty(),
        "social people were not indexed"
    );
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
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
