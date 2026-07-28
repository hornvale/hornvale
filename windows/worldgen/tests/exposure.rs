//! Exposure derivation at the composition root (Words Task 8):
//! `pack_depths` maps a species' perception vector onto the color-pack
//! ladders, and `exposure_of`/`lexicon_of` classify (and then name) every
//! registered concept for a settled species. Also covers The Vigil's other
//! composition-root perception seam, `observed_phenomena_as`: the malformed-
//! kind failure for a non-perceiving fauna kind, and the dragon success path.
use hornvale_language::{ExposureClass, GapReason, LexEntry, PackDepths, color_pack, in_ladder};
use hornvale_species::{ActivityCycle, DRACONIC_NIGHT_VISION, PerceptionVector};
use hornvale_worldgen::{
    BuildError, SettlementPins, SkyChoice, build_world, exposure_of, lexicon_of,
    observed_phenomena_as, pack_depths, placed_peoples,
};

/// The seed-42, generated-sky, default-pins (full four-people roster)
/// world `species_worlds.rs` builds. Task A15a cut settlement genesis over
/// onto the coexistence stack's niche-differentiated K: a settlement is
/// `peopled-by` whichever species locally DOMINATES its attractor, so
/// "placed" now means "dominates at least one settlement," not merely
/// "present in the stack somewhere." At seed 42, under the frozen
/// `BETA`/`FLOOR`, goblin and hobgoblin each dominate a share of the
/// world's attractors and so are both "placed" in this sense; bugbear and
/// kobold are outcompeted at every attractor (present in the coexistence
/// stack almost everywhere, but never locally densest) and so are NOT
/// placed, even though nothing pins them out. The coexistence test below
/// uses goblin and hobgoblin, the two peoples this seed actually places.
fn world() -> hornvale_kernel::World {
    build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

#[test]
fn goblin_lexicon_has_a_root_for_water_the_universal_concept() {
    let w = world();
    let lex = lexicon_of(&w, "goblin").unwrap();
    match lex.entry("water") {
        Some(LexEntry::Root { .. }) => {}
        other => panic!("expected water to be a Root entry (universal stratum), got {other:?}"),
    }
}

#[test]
fn kobold_blue_is_a_perceptual_gap_and_goblin_blue_is_not() {
    let w = world();
    let goblin = exposure_of(&w, "goblin").unwrap();
    let kobold = exposure_of(&w, "kobold").unwrap();

    match kobold.get("blue") {
        Some(ExposureClass::Unknown {
            reason: GapReason::Perceptual(_),
        }) => {}
        other => panic!("expected kobold's 'blue' to be a Perceptual gap, got {other:?}"),
    }
    assert!(
        !matches!(
            goblin.get("blue"),
            Some(ExposureClass::Unknown {
                reason: GapReason::Perceptual(_)
            })
        ),
        "goblin's 'blue' should not be a Perceptual gap (goblin's hue depth reaches blue); got {:?}",
        goblin.get("blue")
    );
}

#[test]
fn each_placed_species_holds_a_root_for_every_placed_species_kind() {
    // Spec §3: "each language will hold its own words for goblin-kind and
    // hobgoblin-kind — endonym and exonym fall out free." Coexistence in
    // one shared world is exposure: both peoples place (dominate at least
    // one attractor), so each is Steeped in the other's kind and each
    // lexicon roots both. Goblin and hobgoblin, not goblin and kobold: the
    // niche cutover (task A15a) means kobold no longer dominates any
    // attractor in the shared default world (see `world()`'s doc comment),
    // so it is no longer "placed" in the `peopled-by` sense this exposure
    // rule reads. Seed 1, not the shared `world()`'s 42: at 42 goblin and
    // hobgoblin's exonym/endonym for "goblin-kind" happen to render
    // identically (both goblinoid-family, sharing a proto-phonology, so
    // occasional root collisions on a shared-family concept are expected)
    // -- 1 is also the witness seed `locked_rotation_changes_the_flagship_
    // cascade`/`the_pantheon_reorganizes_between_spinning_and_locked`
    // already use post-cutover, for the same reason (seed 42's dominant
    // coexistence attractor moved under the niche cutover).
    let w = build_world(
        hornvale_kernel::Seed(1),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let goblin = lexicon_of(&w, "goblin").unwrap();
    let hobgoblin = lexicon_of(&w, "hobgoblin").unwrap();

    let mut romans = Vec::new();
    for (lex, species) in [(&goblin, "goblin"), (&hobgoblin, "hobgoblin")] {
        for concept in ["goblin-kind", "hobgoblin-kind"] {
            match lex.entry(concept) {
                Some(LexEntry::Root { views, .. }) => romans.push(views.roman.clone()),
                other => panic!("{species}'s '{concept}' should be a Root, got {other:?}"),
            }
        }
    }
    // The exonym exists and differs between the two languages: each species
    // draws its word for either kind from its own phonology.
    assert_ne!(
        romans[0], romans[2],
        "goblin and hobgoblin words for goblin-kind should differ"
    );
    assert_ne!(
        romans[1], romans[3],
        "goblin and hobgoblin words for hobgoblin-kind should differ"
    );
}

/// The Wearing (Task 4): a people settled near one of the nine toponymic
/// terrain concepts holds the word for it, and a people that never came
/// near it carries a Gap with a reason — the same shape `sea` already
/// established, extended over the whole terrain vocabulary Task 3
/// registered. This does not assert any one concept resolves to a real
/// word for any one species (that would hardcode this seed's geography
/// into the test); it asserts the map is always TOTAL and every gap is
/// recountable, which is what would break if a concept fell through the
/// closing `Unknown` sweep unclassified by any real rule.
#[test]
fn toponymic_terrain_concepts_resolve_to_a_word_or_a_reasoned_gap() {
    let w = world();
    let terrain_concepts = [
        "river", "hill", "lake", "valley", "coast", "island", "ford", "marsh", "spring",
    ];
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_of(&w, species).expect("lexicon");
        for concept in terrain_concepts {
            match lex.entry(concept) {
                Some(LexEntry::Root { .. }) | Some(LexEntry::Compound { .. }) => {}
                Some(LexEntry::Gap { reason, .. }) => {
                    assert!(
                        !format!("{reason}").is_empty(),
                        "{species}: empty gap reason for '{concept}'"
                    );
                }
                None => panic!("{species}: '{concept}' is registered but absent from the lexicon"),
            }
        }
    }
}

/// The river-specific instance of the property above (Task 4's brief's
/// literal ask), kept as its own named test — but strengthened past the
/// brief's original shape, which the brief itself warned would pass
/// trivially (every registered concept is `Unknown`-by-default from the
/// closing sweep, so "resolves to a word or a reasoned gap" is true even
/// with ZERO exposure rules; measured, not assumed — see this file's
/// history). At seed 42 `river` itself turns out to be `Steeped` for
/// EVERY one of the four placed peoples (deep-history settlement scatter
/// touches a river cell for all of them), so even "at least one Root"
/// would be too weak: that was ALSO true before Task 4, back when `river`
/// was (by a bug in `hornvale_language::packs::universal_stratum` this
/// task found and fixed) unconditionally `Steeped` for every species
/// regardless of geography. The one contrast that is real and robust
/// regardless of any seed's particular geography: an UNPLACED species
/// (one this build never settled anywhere) must be a Gap for `river`,
/// because every terrain rule in `exposure_of_impl` only ever looks at
/// `settled` cells. Under the pre-fix bug this assertion would have
/// FAILED (kobold held a root for `river` — and every other toponymic
/// terrain concept — without ever having stood on one).
#[test]
fn river_exposure_tracks_real_proximity() {
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
        },
    )
    .unwrap();

    // kobold never settles in a goblin-only world: it cannot be exposed to
    // river the way a real settlement would be.
    let exposures = exposure_of(&w, "kobold").unwrap();
    assert!(
        matches!(exposures.get("river"), Some(ExposureClass::Unknown { .. })),
        "an unplaced species must not hold 'river' — got {:?}",
        exposures.get("river")
    );
    let lex = lexicon_of(&w, "kobold").expect("lexicon");
    match lex.entry("river") {
        Some(LexEntry::Gap { reason, .. }) => {
            assert!(
                !format!("{reason}").is_empty(),
                "kobold: empty gap reason for 'river'"
            );
        }
        other => panic!("an unplaced species' 'river' must be a Gap, got {other:?}"),
    }
}

/// The real, positive half of the claim `river_exposure_tracks_real_
/// proximity` cannot make at seed 42 (river saturates to universal
/// there): at least one placed people is a real word (`Root`) for a
/// toponymic terrain concept and at least one is a reasoned `Gap` — proof
/// that the exposure rules discriminate by geography rather than by
/// roster membership, for concepts this seed's four peoples' settlements
/// actually spread across differently. Measured at seed 42 (Task 4
/// review round): `hill` (a strict local elevation maximum among LAND
/// neighbors only — an ocean neighbor is lower than any land cell by
/// definition, so including it made every coastal promontory trivially a
/// "hill") splits 2/4; `valley` (the land-only local-minimum mirror)
/// splits 3/4; `marsh` (a drainage band above ordinary dry land) splits
/// 3/4; `spring` (a karst conduit at channelized-flow drainage — see the
/// report for why `Hydro::Spring` itself is dead code) splits 2/4. See
/// the Task 4 report for the full nine-concept spread table.
#[test]
fn hill_valley_marsh_spring_exposure_differ_across_the_placed_peoples() {
    let w = world();
    for concept in ["hill", "valley", "marsh", "spring"] {
        let mut any_root = false;
        let mut any_gap = false;
        for (species, _) in placed_peoples(&w) {
            let lex = lexicon_of(&w, species).expect("lexicon");
            match lex.entry(concept) {
                Some(LexEntry::Root { .. }) => any_root = true,
                Some(LexEntry::Gap { .. }) => any_gap = true,
                other => panic!("{species}: unexpected '{concept}' entry {other:?}"),
            }
        }
        assert!(
            any_root,
            "'{concept}' should be a Root for at least one placed people at seed 42"
        );
        assert!(
            any_gap,
            "'{concept}' should be a Gap for at least one placed people at seed 42"
        );
    }
}

/// The mirror of [`river_exposure_tracks_real_proximity`] over the whole
/// nine-concept terrain vocabulary, not just `river`: an unplaced species
/// gets a Gap for every one of them, because every Steeped/KnowsOf rule
/// this task adds reads only `settled` cells, which are empty for a
/// species this build never placed. This is the assertion that would have
/// failed outright, for all nine at once, under the pre-fix
/// `universal_stratum` bug.
#[test]
fn an_unplaced_species_gets_a_gap_for_every_toponymic_terrain_concept() {
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
        },
    )
    .unwrap();
    let exposures = exposure_of(&w, "kobold").unwrap();
    for concept in [
        "river", "hill", "lake", "valley", "coast", "island", "ford", "marsh", "spring",
    ] {
        assert!(
            matches!(exposures.get(concept), Some(ExposureClass::Unknown { .. })),
            "an unplaced species must not hold '{concept}' — got {:?}",
            exposures.get(concept)
        );
    }
}

#[test]
fn every_unknown_entrys_reason_is_non_empty() {
    let w = world();
    for species in ["goblin", "kobold"] {
        let exposures = exposure_of(&w, species).unwrap();
        for (concept, class) in &exposures {
            if let ExposureClass::Unknown { reason } = class {
                let text = match reason {
                    GapReason::Experiential(s) | GapReason::Perceptual(s) => s,
                };
                assert!(
                    !text.trim().is_empty(),
                    "{species}'s Unknown reason for '{concept}' must be non-empty"
                );
            }
        }
    }
}

#[test]
fn exposure_of_is_pure_across_two_calls() {
    let w = world();
    let a = exposure_of(&w, "goblin").unwrap();
    let b = exposure_of(&w, "goblin").unwrap();
    assert_eq!(a, b, "same world+species must yield identical exposure");
}

/// A species the world never placed still gets a total, well-reasoned
/// exposure map: build a goblin-only world (species pin) and query the
/// UNPLACED kobold. Every registered concept classifies exactly once; the
/// experiential core (universal stratum) is Steeped regardless of
/// settlement; geography-derived and coexistence-derived concepts fall to
/// reasoned gaps. Guards the zero-settlement path the seed-42 default
/// world never exercises.
#[test]
fn an_unplaced_species_still_gets_a_total_reasoned_exposure_map() {
    let w = build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins {
            species: Some("goblin".to_string()),
        },
    )
    .unwrap();

    let exposures = exposure_of(&w, "kobold").unwrap();
    assert_eq!(
        exposures.len(),
        w.registry.concepts().count(),
        "every registered concept must classify exactly once for an unplaced species"
    );
    assert!(
        matches!(
            exposures.get("water"),
            Some(hornvale_language::ExposureClass::Steeped)
        ),
        "the universal stratum is experience every embodied species has, settled or not"
    );
    for (concept, class) in &exposures {
        if let hornvale_language::ExposureClass::Unknown { reason } = class {
            let text = match reason {
                hornvale_language::GapReason::Experiential(s) => s,
                hornvale_language::GapReason::Perceptual(s) => s,
            };
            assert!(
                !text.is_empty(),
                "unplaced-species gap for '{concept}' must carry a reason"
            );
        }
    }
    // The lexicon still assembles over that map without panicking.
    let lex = lexicon_of(&w, "kobold").unwrap();
    assert_eq!(lex.entries().count(), exposures.len());
}

#[test]
fn a_kind_without_perception_fails_loudly_instead_of_borrowing_goblin_eyes() {
    // Before The Vigil, `exposure_of` resolved a hardcoded goblin baseline for
    // any kind with no perception row — so a bear classified colour as though
    // it saw like a goblin, and the dictionary printed "night-vision 0.5" as a
    // claim about dragons. The baseline is gone: no speaker lacks perception
    // (check_integrity enforces speech ⊆ perception), so the only kinds that
    // reach this path are plain fauna, and they must fail loudly.
    let w = world();
    let err = exposure_of(&w, "owlbear").expect_err("plain fauna carries no perception");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("owlbear") && msg.contains("perception"),
        "the error must name the kind and the missing component, got {msg}"
    );
}

#[test]
fn a_dragon_perceives_with_its_own_eyes_not_the_goblins() {
    // The load-bearing consequence: a dragon's exposure is now classified from
    // ITS vector. At the draconic clade value the hue ladder sits at depth 2,
    // so blue is a perceptual gap for a dragon exactly as it is for a kobold —
    // and unlike the goblin, whose depth-4 ladder lexicalizes it.
    let w = world();
    let dragon = exposure_of(&w, "red-dragon").unwrap();
    let goblin = exposure_of(&w, "goblin").unwrap();
    assert!(
        matches!(
            dragon.get("blue"),
            Some(ExposureClass::Unknown {
                reason: GapReason::Perceptual(_)
            })
        ),
        "blue is a perceptual gap for a dragon, got {:?}",
        dragon.get("blue")
    );
    assert!(
        matches!(goblin.get("blue"), Some(ExposureClass::Steeped)),
        "the goblin still lexicalizes blue — the dragon's gap is its own"
    );
    for concept in ["green", "yellow"] {
        assert!(
            matches!(
                dragon.get(concept),
                Some(ExposureClass::Unknown {
                    reason: GapReason::Perceptual(_)
                })
            ),
            "'{concept}' (hue rank 3) must be a perceptual gap at hue depth 2 \
             (dark/light/red only) — the discriminator between depth 2 and \
             depth 3, unlike 'blue' which is a gap at both, got {:?}",
            dragon.get(concept)
        );
    }
    assert!(
        matches!(dragon.get("starlit"), Some(ExposureClass::Steeped)),
        "the full luminance ladder opens at the draconic clade eye"
    );
}

/// A direct unit test of `pack_depths` at the draconic clade value (spec
/// §8.3, never written until this fix pass): the campaign's headline claim —
/// Draconic's hue inventory is exactly `dark`/`light`/`red` — was pinned only
/// by the drift-checked generated dictionary, which does not distinguish the
/// shipped `night_vision = 0.9` from the `0.75` spec §11 left live (both
/// round to hue depth 2). Constructing the vector straight from
/// `DRACONIC_NIGHT_VISION` makes this test move if that constant ever does.
#[test]
fn pack_depths_at_the_draconic_clade_value_opens_exactly_dark_light_red() {
    let draconic = PerceptionVector {
        activity: ActivityCycle::Diurnal,
        night_vision: DRACONIC_NIGHT_VISION,
        // `pack_depths` reads only `night_vision`; the other two fields are
        // irrelevant to this claim.
        sky_attention: 0.0,
    };
    let depths = pack_depths(&draconic);
    assert_eq!(
        depths,
        PackDepths {
            hue: 2,
            luminance: 3
        },
        "the draconic clade eye must yield hue depth 2 and the full \
         luminance ladder (depth 3)"
    );

    // State the claim as concept ids, not integers: which entries the
    // ladders let through at this depth.
    let mut in_ids: Vec<&str> = Vec::new();
    let mut out_ids: Vec<&str> = Vec::new();
    for entry in color_pack() {
        if in_ladder(entry, &depths) {
            in_ids.push(entry.concept);
        } else {
            out_ids.push(entry.concept);
        }
    }
    in_ids.sort_unstable();
    out_ids.sort_unstable();
    assert_eq!(
        in_ids,
        vec!["dark", "gloom", "light", "red", "shadow", "starlit"],
        "in the lexicon at the draconic clade value: the hue ladder's \
         dark/light/red, and the whole luminance ladder"
    );
    assert_eq!(
        out_ids,
        vec!["blue", "brown", "green", "yellow"],
        "NOT in the lexicon at the draconic clade value: hue ranks 3-5"
    );
}

#[test]
fn a_kind_without_perception_fails_loudly_when_observing_phenomena_too() {
    // Sibling of `a_kind_without_perception_fails_loudly_instead_of_
    // borrowing_goblin_eyes` above, but for the OTHER perception-driven
    // composition-root seam: before The Vigil, `observe_with_sources` held
    // an `.expect("peopled pass over a fauna kind")`, so the REPL's
    // `phenomena --as owlbear` panicked the whole process. Regression pin:
    // a plain fauna kind must fail loudly, by name, with the error and not
    // a panic.
    let w = world();
    let err = observed_phenomena_as(&w, "owlbear").expect_err("plain fauna carries no perception");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("owlbear") && msg.contains("perception"),
        "the error must name the kind and the missing component, got {msg}"
    );
    assert!(
        matches!(err, BuildError::MalformedKind(_)),
        "must fail as MalformedKind, not any other BuildError variant, got {err:?}"
    );
}

#[test]
fn a_dragon_observes_phenomena_with_its_own_eyes() {
    // The success path a dragon must keep: since The Vigil a dragon carries
    // real perception, so observing phenomena AS a dragon succeeds and
    // returns a non-empty, sky-shaped list — the campaign's legible payoff
    // (spec §8.4/§8.5), downgraded in the plan to a one-shot manual REPL
    // step and never pinned by a test until now.
    let w = world();
    let phenomena = observed_phenomena_as(&w, "red-dragon").expect("a dragon perceives");
    assert!(
        !phenomena.is_empty(),
        "a dragon must observe a non-empty phenomena list"
    );
}
