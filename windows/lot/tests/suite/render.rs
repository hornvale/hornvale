//! The narrator and the four `lot/*/v1` payloads: four headed stages,
//! numbered `[n]` references that resolve, and byte-stable JSON that names
//! its schema.
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{Curve, curve, draw, odds, places};
use hornvale_lot::json::{curve_json, life_json, odds_json, places_json};
use hornvale_lot::narrate::{STAGES, curve_text, narrate};
use hornvale_lot::shape::EPOCH_YEARS;
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
    assert_eq!(doc["slots"].as_array().unwrap().len(), 40);
    assert_eq!(doc["silences"]["by_design"].as_u64().unwrap(), 2);
    let filled = doc["silences"]["filled"].as_u64().unwrap();
    let no_fact = doc["silences"]["no_fact"].as_u64().unwrap();
    assert_eq!(
        filled + no_fact + 2,
        40,
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
    // The two remaining by-design silences are folded into exactly one closing line.
    assert_eq!(
        text.matches("those two silences are the world's").count(),
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

    let year = ctx.present_year - 1.0;
    let standing = places(&ctx, year);
    let first = places_json(year, &standing);
    assert_eq!(first, places_json(year, &standing));
    let doc: serde_json::Value = serde_json::from_str(&first).unwrap();
    assert_eq!(doc["schema"], "lot/places/v1");
    // The caller's own argument, echoed: two cached payloads must be
    // distinguishable from their contents alone.
    assert_eq!(doc["year"].as_f64().unwrap(), year);
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
        places_json(year, &standing),
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

/// claim: structural(seed: 42) — one world.
#[test]
fn the_when_graph_names_its_total_and_bins_the_span_by_century() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let drawn = curve(&ctx);
    let text = curve_text(&ctx, &drawn);

    // The span the closing sentence's "five hundred years" is exact for. If
    // a future bake changes it, this reddens and the phrase gets derived
    // rather than rotting into a wrong number.
    assert_eq!(drawn.start_year, 0.0);
    assert_eq!(drawn.present_year, 2000.0);

    // The souls-ever line, and the number in it read back: three
    // significant figures is at worst a 0.05% rounding, so 0.5% is a real
    // check that the quoted total IS the curve's own.
    let opening = text.lines().next().expect("an opening line");
    assert!(
        opening.starts_with("About ")
            && opening.ends_with("lives have been lived in seed 42 between year 0 and year 2000."),
        "the souls-ever line reads {opening:?}"
    );
    let quoted: f64 = opening
        .trim_start_matches("About ")
        .split_whitespace()
        .next()
        .expect("a number")
        .replace(',', "")
        .parse()
        .expect("the souls-ever number parses");
    assert!(
        (quoted - drawn.souls_ever).abs() < 0.005 * drawn.souls_ever,
        "the line quotes {quoted}, the curve holds {}",
        drawn.souls_ever
    );

    // One row per century over the span, and the shares the reader can see
    // account for the whole curve.
    let rows: Vec<&str> = text
        .lines()
        .filter(|line| line.starts_with("| ") && !line.starts_with("| years"))
        .collect();
    assert_eq!(rows.len(), 20, "2,000 years is 20 centuries");
    let mut shares = 0.0;
    let mut births = 0.0;
    for row in &rows {
        let mut columns = row.rsplit('|');
        let share: f64 = columns
            .nth(1)
            .expect("a share column")
            .trim()
            .trim_end_matches('%')
            .parse()
            .unwrap_or_else(|e| panic!("share column of {row:?}: {e}"));
        let count: f64 = columns
            .next()
            .expect("a births column")
            .trim()
            .replace(',', "")
            .parse()
            .unwrap_or_else(|e| panic!("births column of {row:?}: {e}"));
        shares += share;
        births += count;
    }
    // TWO bounds, both derived from the table's own precision rather than
    // guessed. The fix brief said ±0.2 on the share sum; seed 42 renders
    // 99.8, which sits ON that bound and fails it by a float epsilon — a
    // tolerance that would have been green by luck. 20 rows each rounded to
    // one decimal can lose 20 × 0.05 = 1.0, and each births column rounded to
    // a whole number can lose 20 × 0.5 = 10.
    assert!(
        (shares - 100.0).abs() <= 20.0 * 0.05,
        "the century shares sum to {shares}, not 100"
    );
    assert!(
        (births - drawn.souls_ever).abs() <= 20.0 * 0.5,
        "the century rows hold {births} births, the curve holds {}",
        drawn.souls_ever
    );

    // The closing sentence is computed from THIS curve. Seed 42's last
    // quarter carries far more than a third of its births, so it takes the
    // "born recently" arm; the flat arm must not also appear.
    assert!(
        text.contains("Most of these lives were born recently: the last five hundred years hold "),
        "seed 42 takes the flat arm: {text}"
    );
    assert!(!text.contains("stopped growing"));
}

/// claim: structural — a hand-built flat curve, no world required beyond
/// the `LotContext` `curve_text` reads `seed` from (any assembled world
/// supplies that field; seed 42 is used only for convenience).
///
/// `the_when_graph_names_its_total_and_bins_the_span_by_century` above only
/// ever exercises the "born recently" arm, because seed 42's own curve is
/// growing. This test drives the OTHER arm directly: equal births in every
/// epoch means the last quarter holds exactly a quarter of the total, well
/// under `RECENT_SHARE`'s 33%, so `curve_text` must print the "stopped
/// growing" sentence and never the "born recently" one.
#[test]
fn curve_texts_flat_arm_fires_on_a_uniform_curve() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let epochs = 80; // 2000 years / 25-year epochs
    let per_epoch = 10.0;
    let flat = Curve {
        epoch_years: EPOCH_YEARS,
        start_year: 0.0,
        present_year: epochs as f64 * EPOCH_YEARS,
        births_by_epoch: vec![per_epoch; epochs],
        births_by_people_by_epoch: std::collections::BTreeMap::new(),
        souls_ever: per_epoch * epochs as f64,
    };
    let text = curve_text(&ctx, &flat);
    assert!(
        text.contains(
            "This world's population stopped growing: a birth is about as likely in any \
             century.\n"
        ),
        "a uniform curve must take the flat arm: {text}"
    );
    assert!(
        !text.contains("born recently"),
        "a uniform curve must not also claim recent growth: {text}"
    );
}
