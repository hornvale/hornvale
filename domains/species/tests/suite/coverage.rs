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
//! witness. Deliberately-empty vertices are legitimate creature-design
//! predictions, and an assertion forbidding them would either be false or
//! would force junk kinds into the roster to satisfy it. What it DOES assert
//! is that the intended rung matches reality — so promoting a state, or
//! letting one rot, forces a deliberate edit here.

use hornvale_species::{
    ActivityCycle, HabitatRealm, LifeSchedule, SocialForm, StatusBasis, ThermalStrategy,
    biosphere_registry, habitat_realm_registry, perception_registry, psyche_registry,
    society_registry, substrate_response,
};

/// How well a declared state is exercised by the shipped roster.
#[derive(Debug, PartialEq, Eq)]
enum Rung {
    /// The variant or branch exists; no kind carries it.
    Declared,
    /// At least one kind carries it.
    Witnessed,
}

/// The witnesses of each `ThermalStrategy`, ascending by `KindId`.
fn thermal_witnesses(strategy: ThermalStrategy) -> Vec<&'static str> {
    biosphere_registry()
        .iter()
        .filter(|(_, b)| b.thermal_strategy == strategy)
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
    let expected: &[(ThermalStrategy, Rung, &[&str])] = &[
        (
            ThermalStrategy::Endothermic,
            Rung::Witnessed,
            &[
                "black-dragon",
                "bugbear",
                "carrion-crawler",
                // C2c (The Delvers): three dwarves, all endotherms of human
                // mass class. The metabolic vertex they actually move is
                // `LifeSchedule`, not this one — see the life-schedule table.
                "desert-dwarf",
                // C2d (The Radiation): six elves, all endotherms in the
                // 50-60 kg band. Like the dwarves, the metabolic vertex they
                // move is `LifeSchedule`, not this one.
                "desert-elf",
                "dire-wolf",
                "drow",
                "giant-elk",
                "giant-goat",
                "giant-hyena",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "killer-whale",
                "otyugh",
                "owlbear",
                "red-dragon",
                "rhinoceros",
                "sea-elf",
                "snow-elf",
                "white-dragon",
                "wood-elf",
                "woolly-mammoth",
            ],
        ),
        (
            ThermalStrategy::Ectothermic,
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
        // WITNESSED but NOT exercised: allometry computes the old `Autotroph`
        // exactly as an endotherm despite the class doc's surface-limited
        // claim. See BIO-autotroph-physics and
        // `autotroph_is_computed_as_an_endotherm_today` in this file.
        (
            ThermalStrategy::Unmodelled,
            Rung::Witnessed,
            &["shrieker", "treant", "twig-blight"],
        ),
        // The sole carrier of the `None` life-history branch.
        (ThermalStrategy::Absent, Rung::Witnessed, &["xorn"]),
    ];
    for (class, rung, witnesses) in expected {
        let actual = thermal_witnesses(*class);
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
            // C2d: drow is the family's only `Rank` reading.
            &["bugbear", "drow", "goblin", "hobgoblin"],
        ),
        // The Generalist (C2-0) gives Knowledge its SECOND witness. Human
        // standing rests on craft and lore rather than dominance, which is
        // also what distinguishes the sixth people from the Rank-heavy
        // goblinoids.
        //
        // C2c (The Delvers) adds the desert dwarf: the one dwarf authored to
        // survive on a climate niche rather than an elevation one, whose
        // standing rests on knowing where the water is.
        //
        // C2d (The Radiation) adds two more from two further directions:
        // desert-elf, on the same route-and-season argument desert-dwarf
        // reaches it by, and high-elf, whose lore is held by an institution
        // rather than by a person — which is the whole of what separates it
        // from wood-elf's `Generosity`.
        (
            StatusBasis::Knowledge,
            Rung::Witnessed,
            &["desert-dwarf", "desert-elf", "high-elf", "human", "kobold"],
        ),
        // WITNESSED as of The Vacancy T9: the gnoll, the campaign's headline
        // promotion. Justified from the ecology (a scarce, high-variance
        // desert forage base rewards windfall-sharing), not from lore — see
        // `society_registry`'s doc comment on the gnoll's `SocietyVector`.
        //
        // C2c (The Delvers) gives the vertex two more witnesses that reach it
        // from two further directions, which is what a three-witness vertex is
        // worth. Read `society_registry`'s own rows rather than inferring a
        // shared story from the shared variant: gnoll shares a windfall too
        // large to keep, gully-dwarf a find too small to fight over, and
        // hill-dwarf a settled surplus the hall sets out. One authored basis,
        // three unrelated ecologies.
        //
        // C2d adds three more, again unrelated: sea-elf provisions through a
        // lean season, snow-elf through a winter, wood-elf has no hall to
        // govern from and so names what is given rather than who governs.
        (
            StatusBasis::Generosity,
            Rung::Witnessed,
            &[
                "gnoll",
                "gully-dwarf",
                "hill-dwarf",
                "sea-elf",
                "snow-elf",
                "wood-elf",
            ],
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
            &[
                "goblin",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "red-dragon",
                "sea-elf",
            ],
        ),
        (
            ActivityCycle::Nocturnal,
            Rung::Witnessed,
            // C2d (The Radiation): drow, the roster's least ambiguous
            // nocturnal kind — there is no sun underground to keep hours by.
            &["black-dragon", "bugbear", "drow", "kobold"],
        ),
        // Witnessed by a dragon since The Vigil; a mundane beast still cannot
        // carry an `ActivityCycle` at all (`perception_registry` is keyed to
        // minded SPEAKING kinds only — `speech ⊆ perception ⊆ mind`), but a
        // SPEAKING kind can: The Vacancy T9's gnoll is read off its own
        // authored low insolation optimum (a desert forager sheltering
        // through the day's peak heat), giving this vertex its second witness
        // and its first non-dragon one.
        //
        // C2c (The Delvers) takes the vertex from two witnesses to four, and
        // the two it adds arrive for unrelated reasons: desert-dwarf shelters
        // through the peak heat (gnoll's own argument, on the same climate
        // tile), gully-dwarf works the margins of the day because that is
        // when what it scavenges is least contested.
        (
            ActivityCycle::Crepuscular,
            Rung::Witnessed,
            &[
                "desert-dwarf",
                "desert-elf",
                "gnoll",
                "gully-dwarf",
                "snow-elf",
                "white-dragon",
                "wood-elf",
            ],
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
        // C2c (The Delvers): the settling roster goes six to NINE. C2d (The
        // Radiation) takes it to FIFTEEN. This list is the one the census
        // population is drawn from, so its length is the quantity the
        // campaign's regen moves.
        (
            SocialForm::Settled,
            Rung::Witnessed,
            &[
                "bugbear",
                "desert-dwarf",
                "desert-elf",
                "drow",
                "gnoll",
                "goblin",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "hobgoblin",
                "human",
                "kobold",
                "sea-elf",
                "snow-elf",
                "wood-elf",
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
    // `Paced` sat at `Declared` and nothing witnessed it. That empty vertex was
    // that campaign's stated result rather than an oversight, and it named the
    // first campaign to author a long-lived kind as the one that would have to
    // make a deliberate edit here.
    //
    // C2c (THE DELVERS) IS THAT CAMPAIGN. `Paced` moves `Declared` ->
    // `Witnessed` with three witnesses, all at factor 4.0: long life is a
    // dwarf FAMILY trait, not a trait of any one dwarf's habitat.
    //
    // C2d (THE RADIATION) adds six more, all at factor 5.0, and the same
    // family reading holds: the elves are the roster's longest-lived people
    // and every one of them carries the family's factor rather than a
    // habitat-specific one. Note what the second family makes visible that
    // the first could not — the variant now has TWO distinct factors, so a
    // classifier that compared by value instead of by variant would split
    // this vertex in two. That is why `life_schedule_witnesses` classifies by
    // variant, and why `only_the_dwarves_depart_from_pure_allometry` (now
    // renamed) is the test that carries the factors.
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
            &[
                "desert-dwarf",
                "desert-elf",
                "drow",
                "gully-dwarf",
                "high-elf",
                "hill-dwarf",
                "sea-elf",
                "snow-elf",
                "wood-elf",
            ],
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
    // Combinations, not single variants — each is a vertex the roster does not
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
    // diff rather than a silent change. THE GOSSAN renamed the value; the
    // divergence it pins is unchanged. `ThermalStrategy::Unmodelled`'s doc says
    // a phototroph's basal rate is surface/area-limited so Kleiber's 3/4 mass
    // exponent does not apply; `allometry.rs` nonetheless gives it
    // `B0_ENDOTHERM` and a pace multiplier of 1.0. This test asserts the
    // SHIPPED behaviour, not the correct one. When BIO-autotroph-physics lands, this test is
    // expected to fail, and its failure is the point.
    use hornvale_kernel::Mass;
    use hornvale_species::{LifeSchedule, basal_metabolic_rate_w, lifespan};

    let mass = Mass::new(1800.0).expect("positive mass");
    assert_eq!(
        basal_metabolic_rate_w(mass, ThermalStrategy::Unmodelled),
        basal_metabolic_rate_w(mass, ThermalStrategy::Endothermic),
        "Autotroph BMR is identical to Endotherm today (BIO-autotroph-physics)"
    );
    assert_eq!(
        lifespan(mass, ThermalStrategy::Unmodelled, LifeSchedule::ALLOMETRIC),
        lifespan(mass, ThermalStrategy::Endothermic, LifeSchedule::ALLOMETRIC),
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

/// The Wicket, Task 9, fix round 1 (Important 1): `fatigue_rise_registry`
/// must be a TOTAL map over `biosphere_registry`'s roster, every kind
/// included — a missing row used to mean "does not sleep", and review found
/// that indistinguishable from "not yet authored" for exactly the reason
/// [`every_kind_with_a_mind_carries_a_dispersion`] above already guards
/// against for the mind/dispersion pair: a coverage GAP a caller-side
/// fallback quietly papers over is invisible to every reader except this
/// kind of ratchet. `xorn` (`ThermalStrategy::Absent`) is not exempt — it
/// carries an explicit `0.0` row instead of being left off the table.
#[test]
fn every_biosphere_kind_carries_a_fatigue_rise_row() {
    let fatigue = hornvale_species::fatigue_rise_registry();
    for (k, _) in hornvale_species::biosphere_registry().iter() {
        assert!(
            fatigue.contains(k),
            "biosphere kind {k:?} has no fatigue-rise row"
        );
    }
}

/// The Pallet, Task 4: [`hornvale_species::sleep_grade_registry`] must be a
/// TOTAL map over `biosphere_registry`'s roster, for exactly the reason
/// [`every_biosphere_kind_carries_a_fatigue_rise_row`] above states for its
/// own table — the consumer
/// \(`windows/vessel::liveness::sleep_grade_for`\) falls back to a documented
/// neutral on a miss, so a kind left off this table is invisible to every
/// reader except this ratchet, and reads as an authoring choice rather than
/// as the omission it is.
#[test]
fn every_biosphere_kind_carries_a_sleep_grade_row() {
    let grade = hornvale_species::sleep_grade_registry();
    for (k, _) in hornvale_species::biosphere_registry().iter() {
        assert!(
            grade.contains(k),
            "biosphere kind {k:?} has no sleep-grade row"
        );
    }
}

/// The sleep grade is a **preference**, so every row must be a bonus or
/// nothing — never a penalty. A value below `1.0` would make an afforded
/// room repay LESS than the open road, inverting the framing
/// `windows/vessel::liveness::SiteGrade`'s own doc records as rejected; a
/// value above the ceiling would break the calibration argument the old
/// `AFFORDED_REST_GAIN = 1.5` constant carried, which bounds how much a site
/// may ever be worth before a bed stops being a preference and becomes a
/// necessity.
#[test]
fn every_sleep_grade_is_a_bonus_bounded_by_the_authored_ceiling() {
    for (k, g) in hornvale_species::sleep_grade_registry().iter() {
        assert!(
            (1.0..=1.5).contains(g),
            "{k:?}'s sleep grade {g} is outside [1.0, 1.5]: below 1.0 an \
             afforded room would repay less than bare ground, and above 1.5 \
             the site stops being a preference"
        );
    }
}

/// THE TABLE MUST ACTUALLY DIFFERENTIATE — the assertion this campaign
/// exists to be held to.
///
/// The Wicket built the per-species mechanism for `fatigue_rise_registry`
/// and filled 39 rows with two distinct values, which the plumb audit
/// reported as an outstanding defect: a per-species *mechanism* carrying no
/// per-species *difference*. This test refuses that shape here. The bound is
/// deliberately well below the authored count \(seven\) rather than equal to
/// it, so re-authoring a single row is not a gate failure while collapsing
/// the ladder is.
///
/// The two endpoints are named because they are the two the ladder's own
/// argument turns on: `xorn` is ametabolic and can collect nothing, and
/// `human` is the body the ceiling was calibrated for.
///
/// MUTATION THIS MUST FAIL AGAINST — two, both run against
/// `domains/species/src/lib.rs`:
///
/// 1. **the ladder collapses**: `NO_GAIN`, `ALREADY_BUOYED`,
///    `TOO_LARGE_TO_FIT` and `CONTACT_ONLY` all set to `1.30`, leaving three
///    distinct values. Red: `the sleep-grade table carries only 3 distinct
///    value\(s\)`.
/// 2. **the floor is not a floor**: `const NO_GAIN: f64 = 1.50`. Red at the
///    `xorn` assertion: `left: 1.5  right: 1.0`.
#[test]
fn the_sleep_grade_table_carries_a_real_ladder_not_one_repeated_number() {
    use hornvale_kernel::KindId;
    let grade = hornvale_species::sleep_grade_registry();
    let mut seen: Vec<u64> = grade.iter().map(|(_, g)| g.to_bits()).collect();
    seen.sort_unstable();
    seen.dedup();
    assert!(
        seen.len() >= 5,
        "the sleep-grade table carries only {} distinct value(s) — a \
         per-species table with no per-species difference is the defect The \
         Wicket shipped for the fatigue-rise rate, not a success",
        seen.len()
    );
    let xorn = grade.get(&KindId("xorn")).copied().expect("xorn has a row");
    let human = grade
        .get(&KindId("human"))
        .copied()
        .expect("human has a row");
    assert_eq!(
        xorn, 1.0,
        "an ametabolic kind must sit at the floor — a bed gives a xorn nothing"
    );
    assert!(
        human > xorn,
        "a human must gain strictly more from a bed than a creature of stone: \
         human={human}, xorn={xorn}"
    );
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
fn only_the_dwarf_and_elf_families_depart_from_pure_allometry() {
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
    // C2d (THE RADIATION) RENAMED IT AGAIN, on the same principle: the six
    // elves are a second departing family, at factor 5.0, so "only the
    // dwarves" had become the false half of the name. Two families with two
    // factors is what the list below now records — and it is the only place
    // in the workspace where 4.0 and 5.0 are pinned as VALUES.
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
            ("desert-elf", LifeSchedule::Paced { factor: 5.0 }),
            ("drow", LifeSchedule::Paced { factor: 5.0 }),
            ("gully-dwarf", LifeSchedule::Paced { factor: 4.0 }),
            ("high-elf", LifeSchedule::Paced { factor: 5.0 }),
            ("hill-dwarf", LifeSchedule::Paced { factor: 4.0 }),
            ("sea-elf", LifeSchedule::Paced { factor: 5.0 }),
            ("snow-elf", LifeSchedule::Paced { factor: 5.0 }),
            ("wood-elf", LifeSchedule::Paced { factor: 5.0 }),
        ],
        "the dwarf and elf families are the ONLY departures from pure \
         allometry, and each shares ONE factor within itself (4.0, 5.0): long \
         life is a family trait, not a habitat one"
    );
    assert_eq!(
        reg.len(),
        39,
        "30 before C2c, plus the dwarf family's three and the elf family's six"
    );
}

#[test]
fn the_subterranean_roster_is_the_two_rehomed_kinds_and_the_drow() {
    // THE WARREN: C2a re-authored these two for true darkness and for what was
    // then a fixed `SUBTERRANEAN_MOISTURE` (retired by The Underworld, whose
    // chamber moisture is derived per vertex), and nothing scored them there.
    // This store is the consumer half. It ships with exactly these two, and adding a row is a
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
    // So this store stayed at two through C2c, and the reason it did is worth
    // more than the rows would have been: the realm gate places a kind at a
    // cave MOUTH, because settlements are vertex-keyed and a Subterranean kind
    // lives on the surface of a vertex that has a cave in it. The model has no
    // vocabulary for the inside of the world — the sea got depth-named biomes
    // and the rock got a graph. `BIO-kinds-declare-biomes` is the successor.
    //
    // C2d (THE RADIATION) ADDS EXACTLY ONE, and adds it under that constraint
    // rather than in spite of it: the drow is the store's first PEOPLED
    // occupant, and the realm gate is its ONLY authored separation from the
    // surface elves. Its elevation curve is wood-elf's byte for byte, which is
    // the withdrawn duergar lesson applied rather than restated —
    // `windows/worldgen/tests/radiation_admission.rs::drows_elevation_curve_is_woods_and_says_nothing_about_depth`
    // is the assertion that keeps it that way. One cave kind needs only to
    // differ from the surface; distinguishing two cave kinds by DEPTH is what
    // the model still cannot do.
    let reg = habitat_realm_registry();
    let sub: Vec<&str> = reg
        .iter()
        .filter(|(_, r)| **r == HabitatRealm::Subterranean)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        sub,
        vec!["drow", "rust-monster", "xorn"],
        "ascending by KindId"
    );
    // THE TIDEMARK, Task 1: the store gained two explicit `Surface` rows
    // (sea-elf, giant-crocodile) alongside the three `Subterranean` ones
    // above — absence still means `Surface` for everyone else, but these
    // two are stated rather than left to the default now that a third
    // realm exists for a reader to mistake them into (spec §3.6). The
    // count moves from 3 to 5 for that reason, not because the store
    // stopped being sparse.
    assert_eq!(
        reg.len(),
        5,
        "3 subterranean rows (drow, rust-monster, xorn) plus 2 explicit \
         Surface rows (sea-elf, giant-crocodile) — the store is still \
         sparse: every OTHER kind is absent and still means Surface"
    );
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

/// The Tenon: the substrate response must actually DISCRIMINATE, and in the
/// direction the habitat realm states. A curve that answered the same for
/// both realms would make the whole `(species, thing)` relation rank-1 --
/// separable, and therefore not an edge at all.
#[test]
fn the_two_realms_order_hardness_oppositely() {
    const SOFT: f64 = 0.1;
    const HARD: f64 = 0.95;
    const FLOOR: f64 = 0.0;

    let surface = substrate_response(HabitatRealm::Surface);
    let under = substrate_response(HabitatRealm::Subterranean);

    assert!(
        surface.eval(SOFT, FLOOR) > surface.eval(HARD, FLOOR),
        "a surface-dwelling kind must prefer the yielding surface"
    );
    assert!(
        under.eval(HARD, FLOOR) > under.eval(SOFT, FLOOR),
        "a subterranean kind must prefer the hard one -- this is the \
         reversal the campaign exists to make expressible"
    );
}

/// THE TIDEMARK, Task 1: pins the entire stated argument for
/// `substrate_response`'s marine curve, which is otherwise unasserted.
/// `MARINE_OPTIMUM`/`MARINE_WIDTH` are literal aliases of the subterranean
/// values (same specialist peak, same specialist band -- see the function's
/// own doc), so `devotion` is the ONLY independent number Task 1 authored
/// for `Marine`. Without this test, a later edit setting `MARINE_DEVOTION`
/// to `SUBTERRANEAN_DEVOTION` (erasing the one distinction the third curve
/// exists to make) would pass every other gate silently.
///
/// Asserts the RELATIONSHIP the doc argues -- devotion falls by the same
/// step twice, `Surface` (1.0) to `Subterranean` (0.8) to `Marine` (0.6) --
/// never the literal constants, so a deliberate future re-tuning of any one
/// value stays green as long as the ordering (and, more specifically, the
/// EQUAL step) survives it.
#[test]
fn marine_devotion_falls_below_subterranean_by_the_same_step_that_separated_it_from_surface() {
    let surface = substrate_response(HabitatRealm::Surface);
    let under = substrate_response(HabitatRealm::Subterranean);
    let marine = substrate_response(HabitatRealm::Marine);

    assert!(
        marine.devotion < under.devotion,
        "a marine kind's habitat is the water column, not the seabed beneath it -- its \
         devotion ({}) must sit below a subterranean kind's ({}), which is already below a \
         surface kind's whole ({})",
        marine.devotion,
        under.devotion,
        surface.devotion
    );
    assert!(
        under.devotion < surface.devotion,
        "the three-way ordering's other half: a subterranean kind's devotion ({}) must still \
         sit below a surface kind's whole ({})",
        under.devotion,
        surface.devotion
    );

    let surface_to_subterranean_step = surface.devotion - under.devotion;
    let subterranean_to_marine_step = under.devotion - marine.devotion;
    assert!(
        (surface_to_subterranean_step - subterranean_to_marine_step).abs() < 1e-12,
        "the doc's stated argument is that devotion falls again \"by the same 0.2 step\" -- \
         surface-to-subterranean is {surface_to_subterranean_step}, subterranean-to-marine is \
         {subterranean_to_marine_step}; these must be equal, not merely both positive"
    );

    // Marine's optimum and width are aliases of subterranean's by
    // construction (same specialist argument) -- pinned here so a change
    // to either is a deliberate, reviewed edit rather than a silent drift.
    assert_eq!(
        marine.optimum, under.optimum,
        "MARINE_OPTIMUM is authored as an alias of SUBTERRANEAN_OPTIMUM -- see substrate_response's doc"
    );
    assert_eq!(
        marine.width, under.width,
        "MARINE_WIDTH is authored as an alias of SUBTERRANEAN_WIDTH -- see substrate_response's doc"
    );
}

/// The reversal must survive the LOOKUP, not just the two curves: `drow` and
/// `human` must actually land in different realms. The curves are useless if
/// every species resolves to the same one, and nothing in the test above
/// would notice.
#[test]
fn drow_and_human_resolve_to_different_realms() {
    use hornvale_species::KindId;

    let realms = habitat_realm_registry();
    let realm_of = |k: &'static str| {
        realms
            .get(&KindId(k))
            .copied()
            .unwrap_or(HabitatRealm::SURFACE)
    };
    assert_eq!(realm_of("drow"), HabitatRealm::Subterranean);
    assert_eq!(realm_of("human"), HabitatRealm::Surface);
    assert_ne!(
        substrate_response(realm_of("drow")),
        substrate_response(realm_of("human")),
        "the campaign's motivating reversal needs these two to differ"
    );
}
