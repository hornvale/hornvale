//! The Vacancy: the coverage table. Every declared state of the species model,
//! the rung it is intended to occupy, and its witnesses.
//!
//! A state's rung is a claim about how well the model is exercised, not about
//! how good it is:
//!
//! - `Declared`  — the variant or branch exists; nothing carries it.
//! - `Witnessed` — at least one kind carries it.
//!
//! The campaign's wider ladder has a third rung, `Pinned` ("a test fails if it
//! breaks"), which deliberately has no [`Rung`] variant: **appearing in this
//! table with a witness list IS the pin.** A witnessed state listed here cannot
//! silently lose its witness or gain an unintended one without failing one of
//! the tests below, so `Witnessed`-and-listed and `Pinned` are the same claim,
//! and a second variant would let them drift apart. A state promoted to
//! `Witnessed` here is therefore what the campaign's exit criteria mean by
//! reaching `Pinned`.
//!
//! This table deliberately does NOT assert that every declared variant has a
//! witness. Deliberately-empty cells are legitimate creature-design
//! predictions, and an assertion forbidding them would either be false or
//! would force junk kinds into the roster to satisfy it. What it DOES assert
//! is that the intended rung matches reality — so promoting a state, or
//! letting one rot, forces a deliberate edit here.

use hornvale_species::{
    ActivityCycle, HabitatRealm, LifeSchedule, MetabolicClass, SocialForm, StatusBasis,
    biosphere_registry, habitat_realm_registry, perception_registry, psyche_registry,
    society_registry,
};

/// How well a declared state is exercised by the shipped roster.
#[derive(Debug, PartialEq, Eq)]
enum Rung {
    /// The variant or branch exists; no kind carries it.
    Declared,
    /// At least one kind carries it.
    Witnessed,
}

/// The witnesses of each `MetabolicClass`, ascending by `KindId`.
fn metabolic_witnesses(class: MetabolicClass) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.metabolic_class == class)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `SocialForm`, ascending by `KindId`.
fn social_form_witnesses(form: SocialForm) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.social_form == form)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `ActivityCycle` in the perception registry.
fn activity_witnesses(cycle: ActivityCycle) -> Vec<&'static str> {
    perception_registry()
        .iter()
        .filter(|(_, p)| p.activity == cycle)
        .map(|(k, _)| k.0)
        .collect()
}

/// The witnesses of each `StatusBasis` in the society registry.
fn status_basis_witnesses(basis: StatusBasis) -> Vec<&'static str> {
    society_registry()
        .iter()
        .filter(|(_, s)| s.status_basis == basis)
        .map(|(k, _)| k.0)
        .collect()
}

#[test]
fn metabolic_class_coverage_matches_the_table() {
    let expected: &[(MetabolicClass, Rung, &[&str])] = &[
        (
            MetabolicClass::Endotherm,
            Rung::Witnessed,
            &[
                "black-dragon",
                "bugbear",
                "carrion-crawler",
                // C2c (The Delvers): three dwarves, all endotherms of human
                // mass class. The metabolic cell they actually move is
                // `LifeSchedule`, not this one — see the life-schedule table.
                "desert-dwarf",
                "dire-wolf",
                "giant-elk",
                "giant-goat",
                "giant-hyena",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "killer-whale",
                "otyugh",
                "owlbear",
                "red-dragon",
                "rhinoceros",
                "white-dragon",
                "woolly-mammoth",
            ],
        ),
        (
            MetabolicClass::Ectotherm,
            Rung::Witnessed,
            &[
                "giant-constrictor-snake",
                "giant-crocodile",
                "giant-octopus",
                "giant-scorpion",
                "giant-squid",
                "kobold",
                "reef-shark",
                "rust-monster",
            ],
        ),
        // WITNESSED but NOT exercised: allometry computes Autotroph exactly as
        // Endotherm despite the class doc's surface-limited claim. See BIO-autotroph-physics
        // and `autotroph_is_computed_as_an_endotherm_today` in this file.
        (
            MetabolicClass::Autotroph,
            Rung::Witnessed,
            &["shrieker", "treant", "twig-blight"],
        ),
        // The sole carrier of the `None` life-history branch.
        (MetabolicClass::Ametabolic, Rung::Witnessed, &["xorn"]),
    ];
    for (class, rung, witnesses) in expected {
        let actual = metabolic_witnesses(*class);
        assert_eq!(&actual, witnesses, "{class:?} witnesses");
        let actual_rung = if actual.is_empty() {
            Rung::Declared
        } else {
            Rung::Witnessed
        };
        assert_eq!(&actual_rung, rung, "{class:?} rung");
    }
}

#[test]
fn status_basis_coverage_matches_the_table() {
    let expected: &[(StatusBasis, Rung, &[&str])] = &[
        (
            StatusBasis::Rank,
            Rung::Witnessed,
            &["bugbear", "goblin", "hobgoblin"],
        ),
        // The Generalist (C2-0) gives Knowledge its SECOND witness. Human
        // standing rests on craft and lore rather than dominance, which is
        // also what distinguishes the sixth people from the Rank-heavy
        // goblinoids.
        //
        // C2c (The Delvers) adds the desert dwarf: the one dwarf authored to
        // survive on a climate niche rather than an elevation one, whose
        // standing rests on knowing where the water is.
        (
            StatusBasis::Knowledge,
            Rung::Witnessed,
            &["desert-dwarf", "human", "kobold"],
        ),
        // WITNESSED as of The Vacancy T9: the gnoll, the campaign's headline
        // promotion. Justified from the ecology (a scarce, high-variance
        // desert forage base rewards windfall-sharing), not from lore — see
        // `society_registry`'s doc comment on the gnoll's `SocietyVector`.
        //
        // C2c (The Delvers) gives the cell two more witnesses that reach it
        // from two further directions, which is what a three-witness cell is
        // worth. Read `society_registry`'s own rows rather than inferring a
        // shared story from the shared variant: gnoll shares a windfall too
        // large to keep, gully-dwarf a find too small to fight over, and
        // hill-dwarf a settled surplus the hall sets out. One authored basis,
        // three unrelated ecologies.
        (
            StatusBasis::Generosity,
            Rung::Witnessed,
            &["gnoll", "gully-dwarf", "hill-dwarf"],
        ),
    ];
    for (basis, rung, witnesses) in expected {
        let actual = status_basis_witnesses(*basis);
        assert_eq!(&actual, witnesses, "{basis:?} witnesses");
        let actual_rung = if actual.is_empty() {
            Rung::Declared
        } else {
            Rung::Witnessed
        };
        assert_eq!(&actual_rung, rung, "{basis:?} rung");
    }
}

#[test]
fn activity_cycle_coverage_matches_the_table() {
    let expected: &[(ActivityCycle, Rung, &[&str])] = &[
        (
            ActivityCycle::Diurnal,
            Rung::Witnessed,
            // C2c (The Delvers): hill-dwarf, a surface farmer keeping the
            // sun's hours. The family does NOT share a cycle — see
            // Crepuscular below.
            &["goblin", "hill-dwarf", "hobgoblin", "human", "red-dragon"],
        ),
        (
            ActivityCycle::Nocturnal,
            Rung::Witnessed,
            &["black-dragon", "bugbear", "kobold"],
        ),
        // Witnessed by a dragon since The Vigil; a mundane beast still cannot
        // carry an `ActivityCycle` at all (`perception_registry` is keyed to
        // minded SPEAKING kinds only — `speech ⊆ perception ⊆ mind`), but a
        // SPEAKING kind can: The Vacancy T9's gnoll is read off its own
        // authored low insolation optimum (a desert forager sheltering
        // through the day's peak heat), giving this cell its second witness
        // and its first non-dragon one.
        //
        // C2c (The Delvers) takes the cell from two witnesses to four, and
        // the two it adds arrive for unrelated reasons: desert-dwarf shelters
        // through the peak heat (gnoll's own argument, on the same climate
        // tile), gully-dwarf works the margins of the day because that is
        // when what it scavenges is least contested.
        (
            ActivityCycle::Crepuscular,
            Rung::Witnessed,
            &["desert-dwarf", "gnoll", "gully-dwarf", "white-dragon"],
        ),
    ];
    for (cycle, rung, witnesses) in expected {
        let actual = activity_witnesses(*cycle);
        assert_eq!(&actual, witnesses, "{cycle:?} witnesses");
        let actual_rung = if actual.is_empty() {
            Rung::Declared
        } else {
            Rung::Witnessed
        };
        assert_eq!(&actual_rung, rung, "{cycle:?} rung");
    }
}

#[test]
fn social_form_coverage_matches_the_table() {
    let expected: &[(SocialForm, Rung, &[&str])] = &[
        (
            SocialForm::Sessile,
            Rung::Witnessed,
            &["shrieker", "treant", "twig-blight"],
        ),
        (
            SocialForm::Solitary,
            Rung::Witnessed,
            &[
                "black-dragon",
                "carrion-crawler",
                "giant-constrictor-snake",
                "giant-crocodile",
                "giant-octopus",
                "giant-scorpion",
                "giant-squid",
                "otyugh",
                "owlbear",
                "red-dragon",
                "reef-shark",
                "rhinoceros",
                "rust-monster",
                "white-dragon",
                "xorn",
            ],
        ),
        (
            SocialForm::Gregarious,
            Rung::Witnessed,
            &[
                "dire-wolf",
                "giant-elk",
                "giant-goat",
                "giant-hyena",
                "killer-whale",
                "woolly-mammoth",
            ],
        ),
        // C2c (The Delvers): the settling roster goes six to NINE. This list
        // is the one the census population is drawn from, so its length is
        // the quantity the campaign's regen moves.
        (
            SocialForm::Settled,
            Rung::Witnessed,
            &[
                "bugbear",
                "desert-dwarf",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "kobold",
            ],
        ),
    ];
    for (form, rung, witnesses) in expected {
        let actual = social_form_witnesses(*form);
        assert_eq!(&actual, witnesses, "{form:?} witnesses");
        let actual_rung = if actual.is_empty() {
            Rung::Declared
        } else {
            Rung::Witnessed
        };
        assert_eq!(&actual_rung, rung, "{form:?} rung");
    }
}

/// The witnesses of each `LifeSchedule` variant, ascending by `KindId`.
/// `Paced` carries a factor, so kinds are classified by variant rather than
/// compared by value — two differently-paced kinds still witness one state.
fn life_schedule_witnesses(paced: bool) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| matches!(b.schedule, LifeSchedule::Paced { .. }) == paced)
        .map(|(k, _)| k.0)
        .collect()
}

#[test]
fn life_schedule_coverage_matches_the_table() {
    // THE LONG AGE shipped lifespan's authoring channel with NO occupant, so
    // `Paced` sat at `Declared` and nothing witnessed it. That empty cell was
    // that campaign's stated result rather than an oversight, and it named the
    // first campaign to author a long-lived kind as the one that would have to
    // make a deliberate edit here.
    //
    // C2c (THE DELVERS) IS THAT CAMPAIGN. `Paced` moves `Declared` ->
    // `Witnessed` with three witnesses, all at factor 4.0: long life is a
    // dwarf FAMILY trait, not a trait of any one dwarf's habitat.
    //
    // THE NON-OBVIOUS HALF, and why the `Allometric` row is spelled out. It
    // previously read `&every_kind` — the whole registry, computed from
    // `biosphere_registry()`. Once any kind is `Paced` that is simply wrong,
    // but the tempting repair (filter `every_kind` by "not paced") is WORSE
    // than wrong: it is the same computation `life_schedule_witnesses(false)`
    // already performs, so the assertion would compare a value to itself and
    // pass for any roster whatsoever. An explicit list is the only form of
    // this row that can fail. It costs one line per kind added and that cost
    // is the point — a kind silently acquiring a non-default schedule is
    // exactly what this table exists to catch.
    let expected: &[(&str, Rung, &[&str])] = &[
        (
            "Allometric",
            Rung::Witnessed,
            &[
                "black-dragon",
                "bugbear",
                "carrion-crawler",
                "dire-wolf",
                "giant-constrictor-snake",
                "giant-crocodile",
                "giant-elk",
                "giant-goat",
                "giant-hyena",
                "giant-octopus",
                "giant-scorpion",
                "giant-squid",
                "gnoll",
                "goblin",
                "hobgoblin",
                "human",
                "killer-whale",
                "kobold",
                "otyugh",
                "owlbear",
                "red-dragon",
                "reef-shark",
                "rhinoceros",
                "rust-monster",
                "shrieker",
                "treant",
                "twig-blight",
                "white-dragon",
                "woolly-mammoth",
                "xorn",
            ],
        ),
        (
            "Paced",
            Rung::Witnessed,
            &["desert-dwarf", "gully-dwarf", "hill-dwarf"],
        ),
    ];
    for (variant, rung, witnesses) in expected {
        let actual = life_schedule_witnesses(*variant == "Paced");
        assert_eq!(&actual, witnesses, "{variant} witnesses");
        let actual_rung = if actual.is_empty() {
            Rung::Declared
        } else {
            Rung::Witnessed
        };
        assert_eq!(&actual_rung, rung, "{variant} rung");
    }
}

#[test]
fn the_dark_trait_combinations_are_named() {
    // Combinations, not single variants — each is a cell the roster does not
    // occupy, recorded so the vacancy is a decision rather than an oversight.
    use hornvale_kernel::{ANIMAL_PREY, DETRITUS, MARINE_FORAGE};

    let bio = biosphere_registry();

    // `Gregarious x ANIMAL_PREY`: WITNESSED as of The Vacancy T7 — the giant
    // hyena (savanna) and the dire wolf (boreal) are the roster's first
    // pack-hunting predators; every herder before them was a pure forager.
    // The Vacancy T8 adds the killer whale, a MARINE witness of the same
    // combination: `MARINE_FORAGE` is "the sea's single trophic axis"
    // (kernel doc) — the marine analogue of `ANIMAL_PREY`'s land predation,
    // so a `Gregarious` kind weighting either axis is a pack-hunting
    // predator, on land or at sea. The filter below recognizes both.
    let gregarious_predators: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| {
            b.social_form == SocialForm::Gregarious
                && (b.niche.weight(ANIMAL_PREY) > 0.0 || b.niche.weight(MARINE_FORAGE) > 0.0)
        })
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        gregarious_predators,
        vec!["dire-wolf", "giant-hyena", "killer-whale"],
        "Gregarious x ANIMAL_PREY: WITNESSED by The Vacancy T7; killer-whale adds a marine witness (T8)"
    );

    // `Sessile x DETRITUS`: WITNESSED as of The Vacancy T7 — the shrieker, a
    // decomposer that cannot move. treant/twig-blight remain photosynthate
    // autotrophs; the shrieker is the roster's first Sessile detritivore.
    let sessile_detritivores: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Sessile && b.niche.weight(DETRITUS) > 0.0)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        sessile_detritivores,
        vec!["shrieker"],
        "Sessile x DETRITUS: WITNESSED by The Vacancy T7"
    );

    // A minded `Gregarious` kind — decision 0068's whole reason for existing,
    // shipped with zero instances. DELIBERATELY left dark by this campaign
    // (spec S6): the blocker is that settlement-free peoples are unaudited
    // downstream, which is its own campaign.
    let psy = psyche_registry();
    let minded_gregarious: Vec<&str> = bio
        .iter()
        .filter(|(k, b)| b.social_form == SocialForm::Gregarious && psy.contains(k))
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        minded_gregarious,
        Vec::<&str>::new(),
        "minded Gregarious stays DECLARED - deferred, not forgotten"
    );
}

#[test]
fn autotroph_is_computed_as_an_endotherm_today() {
    // A KNOWN DIVERGENCE, pinned deliberately so BIO-autotroph-physics's fix is a visible
    // diff rather than a silent change. `MetabolicClass::Autotroph`'s doc says
    // a phototroph's basal rate is surface/area-limited so Kleiber's 3/4 mass
    // exponent does not apply; `allometry.rs` nonetheless gives it
    // `B0_ENDOTHERM` and a pace multiplier of 1.0. This test asserts the
    // SHIPPED behaviour, not the correct one. When BIO-autotroph-physics lands, this test is
    // expected to fail, and its failure is the point.
    use hornvale_kernel::Mass;
    use hornvale_species::{LifeSchedule, basal_metabolic_rate_w, lifespan};

    let mass = Mass::new(1800.0).expect("positive mass");
    assert_eq!(
        basal_metabolic_rate_w(mass, MetabolicClass::Autotroph),
        basal_metabolic_rate_w(mass, MetabolicClass::Endotherm),
        "Autotroph BMR is identical to Endotherm today (BIO-autotroph-physics)"
    );
    assert_eq!(
        lifespan(mass, MetabolicClass::Autotroph, LifeSchedule::ALLOMETRIC),
        lifespan(mass, MetabolicClass::Endotherm, LifeSchedule::ALLOMETRIC),
        "Autotroph lifespan is identical to Endotherm today (BIO-autotroph-physics)"
    );
}

#[test]
fn every_kind_with_a_mind_carries_a_dispersion() {
    let disp = hornvale_species::dispersion_registry();
    for (k, _) in hornvale_species::psyche_registry().iter() {
        assert!(disp.contains(k), "minded kind {k:?} has no dispersion row");
    }
}

#[test]
fn dispersion_is_a_ratio_on_every_axis() {
    for (k, d) in hornvale_species::dispersion_registry().iter() {
        for (name, v) in [
            ("mind", d.mind),
            ("society", d.society),
            ("perception", d.perception),
        ] {
            assert!(
                (0.0..=1.0).contains(&v),
                "{k:?}'s {name} dispersion {v} is not a ratio"
            );
        }
    }
}

#[test]
fn only_the_dwarves_depart_from_pure_allometry() {
    // THE LONG AGE shipped this as `every_authored_kind_is_allometric_today`,
    // the auditable evidence that its channel had zero occupants, and named
    // C2c as the campaign that would have to widen it.
    //
    // RENAMED RATHER THAN DELETED, because the old name states a claim that
    // is now FALSE — three kinds do depart — while the check it performs is
    // still worth making. What it guards is not "nothing is paced" but
    // "nothing is paced BY ACCIDENT": the departure set is enumerated, so a
    // kind acquiring a non-default schedule without a coverage-table edit
    // fails here as well as there.
    //
    // Direction this enforces, stated because a set equality reads as total
    // and is not: it catches a kind added to the exception set and a kind
    // removed from it, in both directions.
    //
    // It also pins the FACTOR, which nothing else in the workspace did. The
    // schedule's *variant* was audited in three places and its *value* in
    // none, so the 4.0 that produces every dwarf lifespan could have been
    // retyped to any other number with a green suite. That is the shape The
    // Vigil named — a verified claim left unpinned by any failing test — and
    // it is cheapest to close here, where the departure set is already
    // enumerated.
    use hornvale_species::LifeSchedule;

    let reg = hornvale_species::biosphere_registry();
    let departures: Vec<(&str, LifeSchedule)> = reg
        .iter()
        .filter(|(_, b)| b.schedule != LifeSchedule::Allometric)
        .map(|(k, b)| (k.0, b.schedule))
        .collect();
    assert_eq!(
        departures,
        vec![
            ("desert-dwarf", LifeSchedule::Paced { factor: 4.0 }),
            ("gully-dwarf", LifeSchedule::Paced { factor: 4.0 }),
            ("hill-dwarf", LifeSchedule::Paced { factor: 4.0 }),
        ],
        "the dwarf family is the ONLY departure from pure allometry, and \
         shares ONE factor: long life is a family trait, not a habitat one"
    );
    assert_eq!(
        reg.len(),
        33,
        "30 before C2c, plus the dwarf family's three"
    );
}

#[test]
fn the_subterranean_roster_is_exactly_the_two_rehomed_kinds() {
    // THE WARREN: C2a re-authored these two for true darkness and
    // SUBTERRANEAN_MOISTURE and nothing scored them there. This store is the
    // consumer half. It ships with exactly these two, and adding a row is a
    // deliberate edit.
    //
    // C2c (THE DELVERS) WAS EXPECTED TO ADD TWO ROWS AND ADDED NONE. Its
    // mountain and duergar dwarves were cut mid-campaign (spec §11): both
    // were authored "deep" as a LOW elevation above sea level, and
    // depth-below-surface and height-above-sea-level are different
    // quantities — a deep chamber under a mountain is at high ASL, a shallow
    // cave in a marsh is at low ASL. The curve selected lowland marshes and
    // the toponymy reported lowland marshes, which was read as an emergent
    // finding until it was read as the authored value it was.
    //
    // So this store stays at two, and the reason it does is worth more than
    // the rows would have been: the realm gate places a kind at a cave MOUTH,
    // because settlements are cell-keyed and a Subterranean kind lives on the
    // surface of a cell that has a cave in it. The model has no vocabulary for
    // the inside of the world — the sea got depth-named biomes and the rock
    // got a graph. `BIO-kinds-declare-biomes` is the successor.
    let reg = habitat_realm_registry();
    let sub: Vec<&str> = reg
        .iter()
        .filter(|(_, r)| **r == HabitatRealm::Subterranean)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(sub, vec!["rust-monster", "xorn"], "ascending by KindId");
    assert_eq!(reg.len(), 2, "the store is sparse: absence means Surface");
}

#[test]
fn every_kind_in_the_realm_store_has_a_biosphere_row() {
    // Referential integrity, mirroring the peopled-cluster checks in
    // windows/worldgen/src/components.rs: a realm for a kind that does not
    // exist is a typo that would otherwise be silent.
    let bio = biosphere_registry();
    for (kind, _) in habitat_realm_registry().iter() {
        assert!(bio.get(kind).is_some(), "{} has no biosphere row", kind.0);
    }
}
