//! The Reticence, Task 1: what a people with no word for a rider reaches for
//! instead. The frontier essay puts `god` and `spirit` on the doctrine-LESS
//! side ("a people without one explains you with the words it has"), so these
//! are IMPROVISED names, never a doctrine.

use crate::body_fields::seed_42;
use hornvale_vessel::doctrine::{ImprovisedName, Openness, improvised_name, openness};

// Named construction site (decision 0092): sculpts/fits once to build the
// lexicon `improvised_name` reads, for this test's own readout — never a
// second, independent draw the sim depends on.
#[allow(clippy::disallowed_methods)]
#[test]
fn a_settled_peoples_improvised_name_follows_its_cult_form() {
    let (world, _ctx) = seed_42();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("seed 42 fits");
    let lexicon = hornvale_worldgen::lexicon_from(&world, "bugbear", &terrain, &climate)
        .expect("bugbear's lexicon builds");

    let name = improvised_name(&world, &lexicon, "bugbear");
    assert!(
        matches!(name, ImprovisedName::God | ImprovisedName::Spirit),
        "a SETTLED people has god/spirit steeped (worldgen:5938), so it must \
         improvise with one of them, got {name:?}"
    );
}

#[test]
fn the_least_equipped_people_is_the_most_open() {
    // The inversion this campaign turns on (spec 3.2): a people with apparatus
    // knows what to do about you; a people with no word at all has nothing to
    // invoke. Openness ORDER is the assertion, not any single value.
    assert!(openness(&ImprovisedName::God) < openness(&ImprovisedName::Spirit));
    assert!(
        openness(&ImprovisedName::Spirit)
            < openness(&ImprovisedName::Wordless {
                reason: hornvale_language::GapReason::Experiential("unsettled".into()),
            })
    );
    assert_eq!(openness(&ImprovisedName::God), Openness::Guarded);
}

#[test]
fn cult_form_is_uniform_within_every_people() {
    // The spec's section 3.2 takes the FIRST belief as decisive. That is only
    // sound because no people holds two cult-forms. Measured 0 of 15 on seed
    // 42, and 0 of 20 since THE TIDEMARK widened the settling roster; this
    // test is what keeps it true.
    use hornvale_kernel::Value;
    use hornvale_religion::{CULT_FORM, HELD_BY};
    let (world, _ctx) = seed_42();

    // site -> the species that peoples it
    let mut species_of_site = std::collections::BTreeMap::new();
    for f in world.ledger.find("occ-people") {
        if let Value::Text(t) = &f.object {
            species_of_site.insert(f.subject, t.clone());
        }
    }
    // species -> every cult-form held at any site it peoples
    let mut per_species: std::collections::BTreeMap<String, std::collections::BTreeSet<String>> =
        std::collections::BTreeMap::new();
    for f in world.ledger.find(HELD_BY) {
        let Value::Entity(site) = &f.object else {
            continue;
        };
        let (Some(sp), Some(form)) = (
            species_of_site.get(site),
            world.ledger.text_of(f.subject, CULT_FORM),
        ) else {
            continue;
        };
        per_species
            .entry(sp.clone())
            .or_default()
            .insert(form.to_string());
    }
    let mixed: Vec<_> = per_species.iter().filter(|(_, s)| s.len() > 1).collect();
    assert!(
        mixed.is_empty(),
        "cult-form must be uniform per SPECIES, mixed: {mixed:?}"
    );
    // Two campaigns add belief-holding peoples to seed 42 at once: the
    // Underworld Peoples' four and five of The Tidemark's six. MEASURED on
    // the merged world, not added from the two branches' separate pins.
    assert_eq!(
        per_species.len(),
        24,
        "seed 42 has 24 species holding beliefs"
    );
}
