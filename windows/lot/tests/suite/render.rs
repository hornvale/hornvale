//! The narrator and the four `lot/*/v1` payloads: four headed stages,
//! numbered `[n]` references that resolve, and byte-stable JSON that names
//! its schema.
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{curve, draw, odds, places};
use hornvale_lot::json::{curve_json, life_json, odds_json, places_json};
use hornvale_lot::narrate::{STAGES, narrate};
use hornvale_lot::slots::{SlotValue, tell};
use hornvale_lot::{LotIndex, Pick};

/// Every `[n]` in `text`, in first-appearance order, with repeats kept.
fn references(text: &str) -> Vec<usize> {
    let bytes: Vec<char> = text.chars().collect();
    let mut found = Vec::new();
    let mut at = 0;
    while at < bytes.len() {
        if bytes[at] == '[' {
            let mut end = at + 1;
            while end < bytes.len() && bytes[end].is_ascii_digit() {
                end += 1;
            }
            if end > at + 1 && end < bytes.len() && bytes[end] == ']' {
                let digits: String = bytes[at + 1..end].iter().collect();
                found.push(digits.parse::<usize>().expect("digits parse"));
                at = end + 1;
                continue;
            }
        }
        at += 1;
    }
    found
}

/// claim: structural(seed: 42) — one world.
#[test]
fn the_payload_is_byte_stable_and_names_its_schema() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    let first = life_json(&ctx, &life, &story);
    let again = life_json(&ctx, &life, &story);
    assert_eq!(
        first, again,
        "the same story must serialize to the same bytes"
    );
    let doc: serde_json::Value = serde_json::from_str(&first).unwrap();
    assert_eq!(doc["schema"], "lot/life/v1");
    assert_eq!(doc["seed"], 42);
    assert_eq!(doc["index"], 0);
    assert_eq!(doc["slots"].as_array().unwrap().len(), 26);
    assert_eq!(doc["silences"]["by_design"].as_u64().unwrap(), 4);
    let filled = doc["silences"]["filled"].as_u64().unwrap();
    let no_fact = doc["silences"]["no_fact"].as_u64().unwrap();
    assert_eq!(
        filled + no_fact + 4,
        26,
        "every slot is counted exactly once"
    );
    // A filled slot carries at least one source number, and every number in
    // it indexes the flat list.
    let sources = doc["sources"].as_array().unwrap();
    for slot in doc["slots"].as_array().unwrap() {
        let cited = slot["sources"].as_array().unwrap();
        if slot["value"].is_null() {
            assert!(slot["silence"].is_object(), "a null value states a silence");
        } else {
            assert!(
                !cited.is_empty(),
                "{} is filled with no source",
                slot["key"]
            );
        }
        for number in cited {
            let number = number.as_u64().unwrap() as usize;
            assert!(
                number >= 1 && number <= sources.len(),
                "source {number} is out of range"
            );
        }
    }
    for (position, source) in sources.iter().enumerate() {
        assert_eq!(source["number"].as_u64().unwrap() as usize, position + 1);
        if source["kind"] == "fact" {
            // An `EntityId` is a full-width draw, so it crosses the wire as
            // decimal text: a bare integer above 2^53 is rounded by
            // `JSON.parse`, and the exhibit reads this payload in a browser.
            assert!(source["entity"].is_string(), "a fact's entity is text");
        }
    }
    assert!(doc["occupation"].is_string(), "the occupation id is text");
}

/// claim: structural(seed: 42) — one world.
#[test]
fn the_prose_has_four_stages_and_a_sources_list_numbered_from_one() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    let text = narrate(&ctx, &life, &story);
    for heading in ["## When", "## Where", "## Life", "## Story", "### Sources"] {
        assert!(text.contains(heading), "missing {heading}");
    }
    assert!(text.contains("[1]"));
    assert!(text.contains("not a real person"));
    // The disclaimer opens the Story stage, before any other sentence there.
    let stage = text.split("## Story").nth(1).expect("a Story stage");
    let opening = stage.trim_start().lines().next().expect("an opening line");
    assert!(
        opening.contains("not a real person"),
        "the Story stage opens with {opening:?}"
    );
    // The four by-design silences are folded into exactly one closing line.
    assert_eq!(
        text.matches("those four silences are the world's").count(),
        1,
        "the by-design silences are said once"
    );
}

/// claim: structural(seed: 42) — one world.
#[test]
fn the_prose_numbers_every_source_once_in_first_citation_order() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    for index in 0..4 {
        let life = draw(&ctx, LotIndex(index), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        let text = narrate(&ctx, &life, &story);
        let (body, listing) = text.split_once("### Sources").expect("a Sources list");

        // The prose's own numbers, deduplicated in first-appearance order,
        // are exactly 1..=n — so no sentence cites a number the list does
        // not carry, and no listed source goes uncited.
        let mut seen: Vec<usize> = Vec::new();
        for number in references(body) {
            if !seen.contains(&number) {
                seen.push(number);
            }
        }
        let expected: Vec<usize> = (1..=seen.len()).collect();
        assert_eq!(seen, expected, "lot {index}: prose numbers out of order");

        // The listing carries the same count, one line each, in order.
        let listed = references(listing);
        assert_eq!(listed, expected, "lot {index}: the Sources list disagrees");

        // Every Filled slot's first source is cited by its own sentence.
        let doc: serde_json::Value = serde_json::from_str(&life_json(&ctx, &life, &story)).unwrap();
        for (slot, rendered) in story.slots.iter().zip(doc["slots"].as_array().unwrap()) {
            if let SlotValue::Filled(_) = slot.value {
                let first = rendered["sources"].as_array().unwrap()[0].as_u64().unwrap();
                assert!(
                    body.contains(&format!("[{first}]")),
                    "lot {index}: slot {} cites nothing in the prose",
                    slot.key
                );
            }
        }
    }
}

/// claim: structural(seed: 42) — one world.
#[test]
fn every_slot_is_told_in_exactly_one_stage() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    for slot in &story.slots {
        let homes = STAGES
            .iter()
            .filter(|(_, keys)| keys.contains(&slot.key))
            .count();
        assert_eq!(homes, 1, "{} is told in {homes} stages", slot.key);
    }
    // The stages are a partition of the story's slots into CONTIGUOUS runs,
    // so the prose's first-appearance order and `Story::citations`'
    // first-citation order are the same order. Re-grouping a slot is legal;
    // re-ordering one past its neighbours would number the prose one way and
    // the payload another, and fails here instead.
    let staged: Vec<&str> = STAGES
        .iter()
        .flat_map(|(_, keys)| keys.iter().copied())
        .collect();
    let asked: Vec<&str> = story.slots.iter().map(|slot| slot.key).collect();
    assert_eq!(staged, asked, "the stages are not the story's own order");
}

/// claim: structural(seed: 42) — one world.
#[test]
fn the_other_three_payloads_name_their_schemas_and_are_byte_stable() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();

    let drawn = curve(&ctx);
    let first = curve_json(&drawn);
    assert_eq!(first, curve_json(&drawn));
    let doc: serde_json::Value = serde_json::from_str(&first).unwrap();
    assert_eq!(doc["schema"], "lot/curve/v1");
    assert_eq!(doc["epoch_years"], 25.0);
    assert_eq!(
        doc["births_by_epoch"].as_array().unwrap().len(),
        drawn.births_by_epoch.len()
    );
    assert!(doc["souls_ever"].as_f64().unwrap() > 0.0);
    assert!(doc["births_by_people"].is_object());

    let standing = places(&ctx, ctx.present_year - 1.0);
    let first = places_json(&standing);
    assert_eq!(first, places_json(&standing));
    let doc: serde_json::Value = serde_json::from_str(&first).unwrap();
    assert_eq!(doc["schema"], "lot/places/v1");
    assert_eq!(doc["places"].as_array().unwrap().len(), standing.len());
    for place in doc["places"].as_array().unwrap() {
        assert!(place["entity"].is_string(), "a place's entity is text");
    }

    let chances = odds(&ctx, 0);
    let first = odds_json(&chances);
    assert_eq!(first, odds_json(&chances));
    let doc: serde_json::Value = serde_json::from_str(&first).unwrap();
    assert_eq!(doc["schema"], "lot/odds/v1");
    assert!(doc["e0"].as_f64().unwrap() > 0.0);

    // Quantize at emit: every float in every payload survives a round trip
    // through `quantize` unchanged, which is only true if it was quantized
    // on the way out.
    for payload in [
        curve_json(&drawn),
        places_json(&standing),
        odds_json(&chances),
    ] {
        let doc: serde_json::Value = serde_json::from_str(&payload).unwrap();
        assert_quantized(&doc);
    }
}

/// Every `f64` reachable in `doc` is its own quantization.
fn assert_quantized(doc: &serde_json::Value) {
    match doc {
        serde_json::Value::Number(number) => {
            if number.is_f64()
                && let Some(value) = number.as_f64()
            {
                assert_eq!(
                    value,
                    hornvale_kernel::quantize::quantize(value),
                    "{value} was emitted unquantized"
                );
            }
        }
        serde_json::Value::Array(items) => items.iter().for_each(assert_quantized),
        serde_json::Value::Object(fields) => fields.values().for_each(assert_quantized),
        _ => {}
    }
}
