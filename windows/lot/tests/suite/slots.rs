//! Every slot of spec §5 is asked for every life; a silence is a value, and
//! the by-design silences are exactly the spec's list.
use hornvale_lot::context::assemble;
use hornvale_lot::draw::{Ending, draw};
use hornvale_lot::slots::{Silence, SlotValue, Source, tell};
use hornvale_lot::{LotIndex, Pick};

const KEYS: [&str; 39] = [
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
    "reproductive-role",
    "gender-identity",
    "gender-recognition",
    "family",
    "associations",
    "children",
    "siblings",
    "descent",
    "adoption",
    "care",
    "group-membership",
    "migration",
    "parental-death",
    "inheritance",
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
                    assert!(matches!(slot.key, "work" | "literacy"))
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
/// The two remaining by-design silences of spec §4.4 are exactly `work` and
/// `literacy`, and nothing else may claim that verdict: a
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
    assert_eq!(by_design, vec!["work", "literacy"]);
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

/// claim: structural(seed: 42) — one world, twenty lots.
///
/// A citation must name a read that PRODUCED the value, and the two halves
/// of that are checked here. First: every `Source::Fact` a Filled slot
/// cites must resolve to a fact that actually stands in the ledger — a
/// citation whose `(entity, predicate)` pair holds nothing is a fabricated
/// provenance that a registry-membership check alone cannot catch, since
/// the predicate is registered whether or not this subject carries it.
/// Second, for `where`: when the birth settlement carries `latitude` and
/// `longitude` facts, the coordinates the sentence displays must be THOSE
/// values — the defect this test was written for displayed the derived
/// Geosphere position while citing the two facts on the strength of their
/// mere existence.
#[test]
fn every_cited_fact_stands_in_the_ledger_and_where_shows_the_facts_own_coordinates() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut coordinate_checks = 0;
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        for slot in &story.slots {
            for source in &slot.sources {
                if let Source::Fact {
                    entity, predicate, ..
                } = source
                {
                    let id = hornvale_kernel::EntityId::new(*entity)
                        .unwrap_or_else(|| panic!("slot {} cites entity 0", slot.key));
                    assert!(
                        world.ledger.value_of(id, predicate).is_some(),
                        "slot {} cites ({entity}, {predicate}), which stands nowhere in the ledger",
                        slot.key
                    );
                }
            }
        }

        // The birth settlement's own coordinates, if it committed any.
        let people = ctx.occupations[life.occ].record.core.people.0;
        let Some(settlement) = ctx
            .settlements_by_vertex
            .get(&life.site)
            .and_then(|here| {
                here.iter()
                    .find(|id| {
                        hornvale_species::species_of(&world, **id).as_deref() == Some(people)
                    })
                    .or_else(|| here.first())
            })
            .copied()
        else {
            continue;
        };
        let (
            Some(hornvale_kernel::Value::Number(latitude)),
            Some(hornvale_kernel::Value::Number(longitude)),
        ) = (
            world
                .ledger
                .value_of(settlement, hornvale_settlement::LATITUDE),
            world
                .ledger
                .value_of(settlement, hornvale_settlement::LONGITUDE),
        )
        else {
            continue;
        };
        let site = story.slot("where").expect("`where` is asked");
        let SlotValue::Filled(text) = &site.value else {
            panic!("`where` is Filled for every life");
        };
        // The primary site is the one rendered after ", at " — a daughter
        // site, when there is one, renders its own pair further along.
        //
        // MEASURED, AND SAY IT: this half does NOT discriminate against the
        // defect it was written for. The committed `latitude`/`longitude`
        // facts ARE the Geosphere position, quantized to 8 significant
        // digits at the emit boundary, so over these twenty lots the two
        // disagree by at most 4.83e-6° — three orders of magnitude under the
        // one decimal place the sentence displays. Reintroducing the defect
        // (display the derived pair, cite the facts) leaves this assertion
        // green. It is kept because it pins what a reader actually checks —
        // the number in the sentence is the number in the fact — and the
        // assertion below is the one with teeth.
        let shown = format!(", at {latitude:.1}°, {longitude:.1}°");
        assert!(
            text.contains(&shown),
            "lot {i}: `where` reads {text:?}, which does not show the settlement's own \
             committed coordinates {shown:?}"
        );
        // THE DISCRIMINATING HALF. A citation must name the read that
        // produced the value, so when the settlement's own coordinate facts
        // are what the sentence shows, the derivation that was NOT consulted
        // must not appear beside them. The defect cited all three at once.
        let cites_facts = site.sources.iter().any(|source| {
            matches!(source, Source::Fact { predicate, .. } if predicate == hornvale_settlement::LATITUDE)
        });
        let cites_derivation = site.sources.iter().any(|source| {
            matches!(source, Source::Derived { function, .. } if *function == "lot::context::LotContext::lat_lon")
        });
        assert!(
            cites_facts,
            "lot {i}: the settlement carries coordinate facts, but `where` cites none"
        );
        assert!(
            !cites_derivation,
            "lot {i}: `where` cites the settlement's coordinate facts AND the Geosphere \
             derivation — one of the two did not produce the displayed numbers"
        );
        coordinate_checks += 1;
    }
    // Guards the guard: the coordinate half is vacuous if no lot of the
    // twenty ever lands on a settlement carrying both facts.
    assert!(
        coordinate_checks > 0,
        "no lot exercised the committed-coordinate path — the check is vacuous"
    );
    println!("committed-coordinate checks exercised: {coordinate_checks}/20");
}

/// claim: structural(seed: 42) — one world, twenty lots.
///
/// A settlement's name reaches the rendered sentence at five slots, and
/// four of them once printed it while citing nothing — the `Filled`-implies-
/// non-empty-sources check could not see it, because each of those slots
/// already carried some OTHER citation. So this asserts the specific thing:
/// a slot that rendered a settlement's name must cite a `name` fact.
///
/// Each of these four slots renders at most one settlement name and falls
/// back to a recognisable unnamed phrasing, so "did this slot name a
/// settlement" is decidable from the text alone.
#[test]
fn a_slot_that_prints_a_settlements_name_cites_the_name_fact() {
    // (slot key, the markers that mean "no settlement was named here")
    const NAMED_SLOTS: [(&str, &[&str]); 4] = [
        ("where", &["nobody names now"]),
        (
            "founded-from",
            &["an unnamed community at site", "raised from nothing"],
        ),
        ("held-true", &["the community at site ", "entity "]),
        ("tribute", &["an unnamed community at site"]),
    ];
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut named_renders = 0;
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);
        for (key, unnamed_markers) in NAMED_SLOTS {
            let slot = story.slot(key).expect("every slot is asked");
            let SlotValue::Filled(text) = &slot.value else {
                continue;
            };
            if unnamed_markers.iter().any(|marker| text.contains(marker)) {
                continue;
            }
            assert!(
                slot.sources.iter().any(|source| matches!(
                    source,
                    Source::Fact { predicate, .. } if predicate == hornvale_kernel::NAME
                )),
                "lot {i}: slot {key} reads {text:?} — it names a settlement but cites no \
                 `name` fact"
            );
            named_renders += 1;
        }
    }
    // Guards the guard: vacuous if no lot ever rendered a name.
    assert!(
        named_renders > 0,
        "no lot rendered a settlement name — the check is vacuous"
    );
    println!("slots that rendered a settlement name: {named_renders}");
}

/// claim: structural(seed: 42) — one world, twenty lots.
///
/// The `when` sentence prints three integers — born, dead/present, aged —
/// and the review found they disagreed because each was rounded from an
/// independent float. `year()`/`rounded_span()` in `slots.rs` now derive
/// the third from the other two's own rounding, so parsing the sentence
/// back out and checking `D - B == A` is a direct test of that fix, not an
/// indirect one over the underlying floats.
#[test]
fn the_when_sentences_triple_is_internally_consistent() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut dead_checked = 0;
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        if life.ending == Ending::Alive {
            continue;
        }
        let story = tell(&world, &ctx, &life);
        let when = story.slot("when").expect("when is always asked");
        let SlotValue::Filled(text) = &when.value else {
            panic!("lot {i}: when is never silent");
        };
        // "born in year B, dead in year D, aged A"
        let numbers: Vec<i64> = text
            .split(|c: char| !c.is_ascii_digit() && c != '-')
            .filter(|s| !s.is_empty())
            .map(|s| s.parse().unwrap())
            .collect();
        assert_eq!(
            numbers.len(),
            3,
            "lot {i}: expected three integers in {text:?}, got {numbers:?}"
        );
        let (born, dead, aged) = (numbers[0], numbers[1], numbers[2]);
        assert_eq!(
            dead - born,
            aged,
            "lot {i}: {text:?} — dead ({dead}) - born ({born}) != aged ({aged})"
        );
        dead_checked += 1;
    }
    assert!(
        dead_checked > 0,
        "no dead lot among 0..20 on seed 42 — the check is vacuous"
    );
    println!("dead lots checked for when-triple consistency: {dead_checked}");
}

/// claim: structural(seed: 42) — one world, twenty lots.
///
/// The final review found two slots citing facts they never rendered:
/// `community-fate` cited `occ-cause`/`occ-ended-by` on every arm, even the
/// `Alive`/`Hazard` ones that never name a cause or an attacker; `climate`
/// cited a `longitude` fact it never displays. Both are now conditioned on
/// what the sentence actually says, and this test reads the sentence back
/// to check the citation agrees with it.
#[test]
fn community_fate_and_climate_cite_only_what_they_render() {
    let world = hornvale_worldgen::seed_42_world();
    let ctx = assemble(&world).unwrap();
    let mut cause_checked = 0;
    let mut climate_checked = 0;
    for i in 0..20 {
        let life = draw(&ctx, LotIndex(i), &Pick::default()).unwrap();
        let story = tell(&world, &ctx, &life);

        let fate = story.slot("community-fate").expect("always asked");
        let SlotValue::Filled(fate_text) = &fate.value else {
            panic!("lot {i}: community-fate is never silent");
        };
        let names_cause = matches!(life.ending, Ending::CommunityFate(_));
        let cites_cause = fate.sources.iter().any(
            |s| matches!(s, Source::Fact { predicate, .. } if predicate == hornvale_history::OCC_CAUSE),
        );
        assert_eq!(
            names_cause, cites_cause,
            "lot {i}: community-fate text {fate_text:?} names a cause = {names_cause}, but \
             cites occ-cause = {cites_cause}"
        );
        cause_checked += 1;

        let climate = story.slot("climate").expect("always asked");
        let SlotValue::Filled(climate_text) = &climate.value else {
            panic!("lot {i}: climate is never silent");
        };
        let cites_longitude = climate.sources.iter().any(
            |s| matches!(s, Source::Fact { predicate, .. } if predicate == hornvale_settlement::LONGITUDE),
        );
        assert!(
            !cites_longitude,
            "lot {i}: climate text {climate_text:?} cites a longitude fact it never displays"
        );
        climate_checked += 1;
    }
    assert!(cause_checked > 0 && climate_checked > 0, "vacuous check");
    println!("community-fate/climate citation checks: {cause_checked} lots");
}
