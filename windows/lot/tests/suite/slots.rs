//! Every slot of spec §5 is asked for every life; a silence is a value, and
//! the by-design silences are exactly the spec's list.
use hornvale_lot::context::assemble;
use hornvale_lot::draw::draw;
use hornvale_lot::slots::{Silence, SlotValue, Source, tell};
use hornvale_lot::{LotIndex, Pick};

const KEYS: [&str; 26] = [
    "when",
    "where",
    "people",
    "name",
    "community-size",
    "founded-from",
    "founder-kinship",
    "community-fate",
    "tech",
    "function",
    "tongue",
    "belief",
    "held-true",
    "subsistence",
    "standing",
    "tribute",
    "dwelling",
    "mine",
    "climate",
    "sky",
    "ground",
    "diet",
    "sex",
    "family",
    "work",
    "literacy",
];

/// claim: structural(seed: 42) — one world, twenty lots.
#[test]
fn every_slot_is_asked_and_by_design_silences_are_declared() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        let keys: Vec<&str> = story.slots.iter().map(|slot| slot.key).collect();
        assert_eq!(keys, KEYS.to_vec(), "slot order is the story's order");
        for slot in &story.slots {
            match &slot.value {
                SlotValue::Filled(_) => assert!(
                    !slot.sources.is_empty(),
                    "{} filled with no source",
                    slot.key
                ),
                SlotValue::Silent(Silence::ByDesign(_)) => {
                    assert!(matches!(slot.key, "sex" | "family" | "work" | "literacy"))
                }
                SlotValue::Silent(Silence::NoFact(_)) => {}
            }
        }
        let when = &story.slots[0];
        assert!(matches!(when.value, SlotValue::Filled(_)));
    }
}

/// claim: structural(seed: 42) — one world.
///
/// Every `Source::Fact` a filled slot cites must name a predicate the
/// world's own registry knows: a citation to an unregistered predicate is a
/// fabricated provenance, which is the one failure mode this campaign's
/// whole instrument rests on not having.
#[test]
fn every_cited_fact_names_a_registered_predicate() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        for slot in &story.slots {
            if matches!(slot.value, SlotValue::Filled(_)) {
                assert!(
                    !slot.sources.is_empty(),
                    "{} filled with no source",
                    slot.key
                );
            }
            for source in &slot.sources {
                if let Source::Fact {
                    predicate, caption, ..
                } = source
                {
                    assert!(
                        world.registry.predicate(predicate).is_some(),
                        "slot {} cites unregistered predicate {predicate}",
                        slot.key
                    );
                    assert!(
                        !caption.is_empty(),
                        "slot {} cites {predicate} with no caption",
                        slot.key
                    );
                }
            }
        }
    }
}

/// claim: structural(seed: 42) — one world.
///
/// The four by-design silences of spec §4.4 are exactly `sex`, `family`,
/// `work` and `literacy`, and nothing else may claim that verdict: a
/// resolver that could not answer must say `NoFact`, which the coverage
/// readout counts as a silence the WORLD has.
#[test]
fn the_by_design_silences_are_exactly_the_specs_four() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let life = draw(&ctx, LotIndex(0), &Pick::default()).unwrap();
    let story = tell(&world, &ctx, &life);
    let by_design: Vec<&str> = story
        .slots
        .iter()
        .filter(|slot| matches!(slot.value, SlotValue::Silent(Silence::ByDesign(_))))
        .map(|slot| slot.key)
        .collect();
    assert_eq!(by_design, vec!["sex", "family", "work", "literacy"]);
}

/// claim: structural(seed: 42) — one world, two hundred lots.
///
/// A READOUT, not an assertion beyond the two floors below: the per-slot
/// fill count over lots 0–199 is H-P4/H-P5's first look (spec §8), and it
/// is printed rather than pinned because the campaign has not yet
/// preregistered a threshold for any slot but these two.
#[test]
fn the_per_slot_fill_counts_are_printed() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut filled: std::collections::BTreeMap<&str, usize> = std::collections::BTreeMap::new();
    let mut order: Vec<&str> = Vec::new();
    let lots = 200;
    for i in 0..lots {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        for slot in &story.slots {
            if order.len() < story.slots.len() && !order.contains(&slot.key) {
                order.push(slot.key);
            }
            let entry = filled.entry(slot.key).or_insert(0);
            if matches!(slot.value, SlotValue::Filled(_)) {
                *entry += 1;
            }
        }
    }
    println!("per-slot fill counts over lots 0..{lots} of seed 42:");
    for key in &order {
        println!("  {key:<16} {}/{lots}", filled[key]);
    }
    // The two floors that are not a readout: `when` and `community-fate`
    // are Filled for every life by construction (a drawn year always
    // exists; a fate is always either witnessed or "still standing").
    assert_eq!(filled["when"], lots as usize);
    assert_eq!(filled["community-fate"], lots as usize);
}
