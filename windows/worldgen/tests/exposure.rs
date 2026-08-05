//! Exposure derivation at the composition root (Words Task 8):
//! `pack_depths` maps a species' perception vector onto the color-pack
//! ladders, and `exposure_from`/`lexicon_from` classify (and then name) every
//! registered concept for a settled species. Also covers The Vigil's other
//! composition-root perception seam, `observed_phenomena_as`: the malformed-
//! kind failure for a non-perceiving fauna kind, and the dragon success path.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]
use hornvale_language::{ExposureClass, GapReason, LexEntry, PackDepths, color_pack, in_ladder};
use hornvale_species::{ActivityCycle, DRACONIC_NIGHT_VISION, PerceptionVector};
use hornvale_worldgen::{
    BuildError, SettlementPins, SkyChoice, build_world, exposure_from, lexicon_from,
    observed_phenomena_as, pack_depths, placed_peoples,
};

/// The seed-42, generated-sky, default-pins world `species_worlds.rs`
/// builds. Task A15a cut settlement genesis over onto the coexistence
/// stack's niche-differentiated K: a settlement is `peopled-by` whichever
/// species locally DOMINATES its attractor, so "placed" now means
/// "dominates at least one settlement," not merely "present in the stack
/// somewhere." At seed 42, under the frozen `BETA`/`FLOOR`, this used to
/// place only goblin and hobgoblin (bugbear and kobold were outcompeted at
/// every attractor). Main's absorb into The Wearing (77 commits, merge
/// `166d4ad9`: The Vacancy's fifth people `gnoll` plus new terrain and
/// settlement placement) moved the coexistence outcome under this seed —
/// re-measured after the absorb, ALL FIVE registered peoples (bugbear,
/// gnoll, goblin, hobgoblin, kobold) now dominate at least one attractor
/// and so are all "placed" at seed 42. The coexistence test below
/// (`each_placed_species_holds_a_root_for_every_placed_species_kind`)
/// hardcodes goblin/hobgoblin at seed 1, which this shift does not touch.
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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let lex = lexicon_from(&w, "goblin", &terrain, &climate).unwrap();
    match lex.entry("water") {
        Some(LexEntry::Root { .. }) => {}
        other => panic!("expected water to be a Root entry (universal stratum), got {other:?}"),
    }
}

#[test]
fn kobold_blue_is_a_perceptual_gap_and_goblin_blue_is_not() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let goblin = exposure_from(&w, "goblin", &terrain, &climate).unwrap();
    let kobold = exposure_from(&w, "kobold", &terrain, &climate).unwrap();

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
    // -- 1 was also the witness seed `locked_rotation_changes_the_flagship_
    // cascade`/`the_pantheon_reorganizes_between_spinning_and_locked`
    // already use post-cutover, for the same reason (seed 42's dominant
    // coexistence attractor moved under the niche cutover).
    //
    // Seed 3, re-searched: The Wearing's nucleus fix shortened every root,
    // and shorter roots collide more often between two languages that share
    // a proto-phonology. At seed 1 goblin and hobgoblin now BOTH render
    // "hobgoblin-kind" as `Koe`. Sweeping 0..16 post-fix, 5 of the 14 seeds
    // that root all four words hit such a collision (1, 7, 10, 12, 13) --
    // so this is the rate the paragraph above anticipated, measured, not a
    // regression in the exposure rule. Seed 3 rendered all four distinctly
    // (`Zhoze`/`Sasta` against `Zhozeg`/`Shashtak`), and the pair read as
    // the cognates two sibling languages should have.
    //
    // Seed 0, re-searched for F7 (The Witness, 2026-07-30): gating
    // `Tonogenesis` on a prior merger reseeded every species' cascade, and
    // seed 3 collided again ("Zgaeg" for both goblin and hobgoblin's
    // "hobgoblin-kind"). Sweeping 0..40 post-fix, seed 0 was the first that
    // rendered all four words distinctly.
    let w = build_world(
        hornvale_kernel::Seed(0),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let goblin = lexicon_from(&w, "goblin", &terrain, &climate).unwrap();
    let hobgoblin = lexicon_from(&w, "hobgoblin", &terrain, &climate).unwrap();

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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let terrain_concepts = [
        "river", "hill", "lake", "valley", "coast", "island", "ford", "marsh", "spring",
    ];
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_from(&w, species, &terrain, &climate).expect("lexicon");
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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();

    // kobold never settles in a goblin-only world: it cannot be exposed to
    // river the way a real settlement would be.
    let exposures = exposure_from(&w, "kobold", &terrain, &climate).unwrap();
    assert!(
        matches!(exposures.get("river"), Some(ExposureClass::Unknown { .. })),
        "an unplaced species must not hold 'river' — got {:?}",
        exposures.get("river")
    );
    let lex = lexicon_from(&w, "kobold", &terrain, &climate).expect("lexicon");
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
/// roster membership. Measured at seed 42 (Task 4 review round 3, the
/// clamp-to-sea-level/full-ring gate, four placed peoples): `hill` split
/// 2/4, `marsh` split 3/4, `spring` split 2/4 — all three genuinely
/// discriminated, so the test asserted all three together.
///
/// **Re-measured after The Wearing absorbed 77 commits from main (merge
/// `166d4ad9`)**: the absorb moved both the terrain and the settlement
/// placement, and (per `world()`'s doc comment) grew the placed-people
/// roster from four to five (bugbear, gnoll, goblin, hobgoblin, kobold —
/// The Vacancy's `gnoll` plus a coexistence-outcome shift that now also
/// places bugbear and kobold). Re-measuring all three concepts against
/// the new roster: `hill` is now 0/5 Root, 5/5 Gap — nobody's settlement
/// sits at a strict local elevation maximum any more (see the dedicated
/// `hill_is_a_gap_for_every_placed_people_at_seed_42_except_goblin_which_
/// roots_it` below, which records the shape as it stands after The Wearing's
/// close merge moved the population again — 1/5 Root, 4/5 Gap). `marsh` is now 5/5
/// Root, 0/5 Gap — the opposite drift, now saturated like `river`/`ford`
/// (see `marsh_is_a_root_for_every_placed_people_at_seed_42` below).
/// `spring` alone still genuinely discriminated at that point: 1/5 Root
/// (kobold), 4/5 Gap.
///
/// # The Contour re-pin (2026-07-30)
///
/// Position-aware conflict (`defensibility`-gated raid dominance, spec
/// section 2.3a/2.4, decision 0096 clause 1) redecided seed 42's
/// deep-history settlement survival again, and `spring` did not survive as
/// a discriminator: it is now 5/5 Root — saturated, the same shape `marsh`
/// and `river`/`ford` already have, for the same reason (deep-history
/// settlement scatter across five peoples makes hitting at least one
/// spring-adjacent cell near-certain). This is a genuine behavior change,
/// not a broken gate — the rule that classifies `spring` did not change,
/// and reachability for a genuine `spring` Gap is still proven on other
/// seeds by `every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_
/// sweep`. `hill` and `valley` are the two concepts that still
/// discriminate at seed 42 after this re-pin (each 1/5 Root, 4/5 Gap, and
/// each re-pinned alongside this test); `spring` joins `marsh`/`river`/
/// `ford` as saturated, which is why this test is renamed and rewritten
/// to match `marsh_is_a_root_for_every_placed_people_at_seed_42`'s shape
/// rather than asserting a "differs" claim that is no longer true.
///
/// # The Contour absorb (2026-08-02)
///
/// Re-measured on the merged tree, which additionally carries main's
/// cascade/v2 reseed (`The Witness`/`The Watershed`): `spring` did NOT stay
/// saturated. It discriminates again — 1/5 Root (kobold), 4/5 Gap — the
/// same 1/4 shape it had before The Wearing's absorb, though for a
/// different reason this time: every non-kobold species now reads an
/// `Experiential` Gap ("has no exposure to 'spring'") rather than the
/// toponymic-classification Gap the pre-absorb measurement recorded. Not a
/// combination of the two prior deltas — cascade/v2 and `defensibility`
/// interact on WHICH cells peoples settle near, and this seed's outcome
/// happens to land back on a discriminating shape. This is why the test is
/// renamed and rewritten again, to the same exact-partition idiom `hill`
/// and `valley` already use rather than the saturated shape this file
/// carried between the two absorbs.
///
/// The Generalist re-pin (2026-08-03): human joins the coexistence stack as
/// a sixth competitor, redeciding seed 42's settlement placement once more —
/// kobold's flagship no longer has exposure to a spring cell either.
/// `spring` is saturated again: a Gap for every placed people (0/6 Root,
/// 6/6 Gap), the shape `river`/`ford` already carry. Renamed to match, per
/// this test's own established policy of renaming to the shape rather than
/// asserting a "discriminates" claim that is no longer true.
#[test]
fn spring_is_a_gap_for_every_placed_people_at_seed_42() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let mut gapped: Vec<&str> = Vec::new();
    let mut rooted: Vec<(&str, String)> = Vec::new();
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_from(&w, species, &terrain, &climate).expect("lexicon");
        match lex.entry("spring") {
            Some(LexEntry::Gap { .. }) => gapped.push(species),
            Some(LexEntry::Root { views, .. }) => rooted.push((species, views.roman.clone())),
            other => panic!("{species}: unexpected 'spring' entry at seed 42: {other:?}"),
        }
    }
    gapped.sort_unstable();
    rooted.sort_unstable();
    assert_eq!(
        gapped,
        vec!["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"],
        "the set of peoples gapping 'spring' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        Vec::<(&str, String)>::new(),
        "at seed 42 no placed people roots 'spring'"
    );
}

/// `hill`'s honest post-Contour shape (see `spring_is_a_root_for_every_
/// placed_people_at_seed_42`'s doc comment for the fuller measurement
/// history): at seed 42, under the unchanged clamp-to-sea-level/full-ring
/// gate, `hill` splits 1/5 Root, 4/5 Gap — same shape as before The Contour,
/// but the ONE rooting people changed.
///
/// # The Contour re-pin (2026-07-30)
///
/// Wiring `defensibility` into the deep-history raid dominance checks
/// (spec section 2.3a/2.4, decision 0096 clause 1) redecided which route a
/// raid could clear, which redecided seed 42's settlement survival and
/// placement outright: bugbear's flagship no longer sits at hill's strict
/// local elevation maximum — it now sits at valley's local minimum instead
/// (see `valley_is_a_gap_for_every_placed_people_at_seed_42_except_bugbear_
/// which_roots_it` immediately below, which is bugbear and hill's mirror).
/// Goblin's flagship is the new occupant of hill's elevation maximum,
/// rooting it as `Nootea`. The partition is still asserted EXACTLY, in both
/// directions and by name, for the same reason F11 gave: the exception is
/// not noise to route around, it is exactly what the elevation-maximum gate
/// is FOR.
///
/// `valley_is_a_gap_..._except_bugbear_which_roots_it` and
/// `marsh_is_a_root_...` were re-measured alongside this one; only `hill`
/// and `valley` moved (and, as it happens, moved into each other's shape) —
/// see this file's other two re-pinned tests for the full account.
///
/// The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
/// lefford, 0063): the BAKE label bump reseats settlements once more, and
/// NOBODY'S flagship sits on hill's strict local elevation maximum any
/// longer — `hill` is back to a Gap for every placed people (0/5 Root, 5/5
/// Gap), the shape it had before The Contour's own re-pin. Renamed to
/// match; this is a real geographic fact about this derivation of seed 42,
/// re-measured rather than assumed.
///
/// The Generalist re-pin (2026-08-03): human joins the coexistence stack as
/// a sixth competitor, which redecides seed 42's settlement placement once
/// more — kobold's flagship now sits at hill's strict local elevation
/// maximum, rooting it as `Roxoro`. `hill` splits again (1/6 Root, 5/6 Gap,
/// human among the gappers), the same shape family this test's own history
/// already carries; renamed to name the new sole rooter.
#[test]
fn hill_is_a_gap_for_every_placed_people_at_seed_42_except_kobold_which_roots_it() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    // The exact partition, by name — not a count, and not "at least one of
    // each". A count would survive the roster changing under it; naming both
    // sides means any movement at all lands in a failure message that says
    // which people moved and which way.
    let mut gapped: Vec<&str> = Vec::new();
    let mut rooted: Vec<(&str, String)> = Vec::new();
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_from(&w, species, &terrain, &climate).expect("lexicon");
        match lex.entry("hill") {
            Some(LexEntry::Gap { .. }) => gapped.push(species),
            Some(LexEntry::Root { views, .. }) => rooted.push((species, views.roman.clone())),
            other => panic!("{species}: unexpected 'hill' entry at seed 42: {other:?}"),
        }
    }
    gapped.sort_unstable();
    rooted.sort_unstable();
    assert_eq!(
        gapped,
        vec!["bugbear", "gnoll", "goblin", "hobgoblin", "human"],
        "the set of peoples gapping 'hill' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![("kobold", "Roxoro".to_string())],
        "at seed 42 exactly one people roots 'hill' — kobold"
    );
}

/// The honest counterpart to the test above, and no longer a symmetric
/// "Gap for every placed people" claim as of The Contour: at seed 42,
/// under the corrected (clamp-to-sea-level, full-ring) gate, `valley` now
/// splits 1/5 Root, 4/5 Gap — the mirror image of `hill`'s shape (see
/// `hill_is_a_gap_for_every_placed_people_at_seed_42_except_goblin_which_
/// roots_it`'s doc comment for why: defensibility-gated raid dominance
/// redecided settlement survival, and bugbear's flagship moved from hill's
/// local elevation maximum to valley's local elevation minimum). Before The
/// Contour this was 0/5 Root, 5/5 Gap for every placed people (re-measured
/// after The Wearing's absorb of main, merge `166d4ad9`, unchanged in shape
/// from the pre-absorb 0/4) — that shape is why the test kept its name
/// through The Wearing's re-pin but not through this one.
///
/// The Contour absorb (2026-08-02): the partition is unchanged — bugbear
/// still alone roots `valley` — but main's cascade/v2 reseed moved the
/// flagship's generated name, `Kodoa` -> `Godoa`.
///
/// The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
/// lefford, 0063): the BAKE label bump reseats settlements once more, and
/// NOBODY'S flagship sits on valley's strict local elevation minimum any
/// longer — `valley` is back to a Gap for every placed people (0/5 Root,
/// 5/5 Gap), the shape it had before The Contour's own re-pin (the mirror
/// of `hill`'s move at this same regen). Renamed to match; this is a real
/// geographic fact about this derivation of seed 42, re-measured rather
/// than assumed.
///
/// The Generalist re-pin (2026-08-03): human joins the coexistence stack as
/// a sixth competitor; the partition shape is unchanged (still a Gap for
/// every placed people) but the roster gains "human" alongside the other
/// five.
#[test]
fn valley_is_a_gap_for_every_placed_people_at_seed_42() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let mut gapped: Vec<&str> = Vec::new();
    let mut rooted: Vec<(&str, String)> = Vec::new();
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_from(&w, species, &terrain, &climate).expect("lexicon");
        match lex.entry("valley") {
            Some(LexEntry::Gap { .. }) => gapped.push(species),
            Some(LexEntry::Root { views, .. }) => rooted.push((species, views.roman.clone())),
            other => panic!("{species}: unexpected 'valley' entry at seed 42: {other:?}"),
        }
    }
    gapped.sort_unstable();
    rooted.sort_unstable();
    assert_eq!(
        gapped,
        vec!["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"],
        "the set of peoples gapping 'valley' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        Vec::<(&str, String)>::new(),
        "at seed 42 no placed people roots 'valley'"
    );
}

/// `marsh`'s honest post-absorb shape (see `spring_is_a_root_for_every_
/// placed_people_at_seed_42`'s doc comment for the measurement history):
/// pre-absorb this split 3/4 (a real per-culture discrimination); after
/// The Wearing absorbed main's terrain/settlement drift it is now a
/// `Root` for EVERY placed people at seed 42 — the same saturated shape
/// `river`/`ford` already have (deep-history settlement scatter across
/// many cells makes hitting at least one damp-but-not-riverine cell
/// near-certain once the roster grows to five). This is a genuine
/// behavior change, not a broken gate: the rule is unchanged
/// (`water_kind_at(cell) == WaterKind::DryLand && drainage_at(cell) >=
/// 5.0`, see the Task 4 report), and it still produces a real Gap for at
/// least some species on other seeds (nothing in this campaign requires
/// `marsh` to discriminate on every seed, only that it is reachable —
/// which `every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_
/// sweep` already proves).
///
/// The Contour epoch v2 re-pin (2026-08-02, history/bake/v2 regen on
/// lefford, 0063): the BAKE label bump reseats settlements again, and
/// bugbear's flagship no longer has exposure to a marsh cell at seed 42.
/// `marsh` is no longer a Root for EVERY placed people — it splits 4/5
/// Root, 1/5 Gap (bugbear). Renamed to match; asserted as an exact
/// partition, by name, the same discipline `hill`/`valley`/`spring`
/// already use, for the same reason: the exception is not noise to route
/// around.
///
/// The Generalist re-pin (2026-08-03): human joins the coexistence stack as
/// a sixth competitor, redeciding seed 42's settlement placement once more —
/// bugbear's flagship now has exposure to a marsh cell after all (rooting it
/// as `Qadoo`), and human's flagship is the new sole gapper. `marsh` keeps
/// the same 5/6-Root, 1/6-Gap shape, just with a different exception;
/// renamed to name it.
///
/// The Tolerance re-pin (2026-08-04): the raid gate became a per-settlement
/// draw rather than a per-species constant, redeciding seed 42's settlement
/// placement once more — and human's flagship now sits beside a marsh cell
/// after all, rooting it as `Meashngeo`. `marsh` is back to a Root for EVERY
/// placed people (6/6), which is where this test started and why its name
/// returns to that form.
///
/// **The re-pin is case (2), verified rather than assumed.** Every one of the
/// five previously-rooting peoples kept a BYTE-IDENTICAL root (`Qadoo`,
/// `Gshoovzngaov`, `Taneo`, `Qaneo`, `Rorora`); the only change is a Gap
/// becoming a Root. So the phonology did not move — an entry appeared where
/// exposure appeared, which is exactly what an upstream placement change is
/// supposed to look like. Had one of those five romanizations changed, that
/// would have been a phonology bug and not a re-pin.
#[test]
fn marsh_is_a_root_for_every_placed_people_at_seed_42() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let mut gapped: Vec<&str> = Vec::new();
    let mut rooted: Vec<(&str, String)> = Vec::new();
    for (species, _) in placed_peoples(&w) {
        let lex = lexicon_from(&w, species, &terrain, &climate).expect("lexicon");
        match lex.entry("marsh") {
            Some(LexEntry::Gap { .. }) => gapped.push(species),
            Some(LexEntry::Root { views, .. }) => rooted.push((species, views.roman.clone())),
            other => panic!("{species}: unexpected 'marsh' entry at seed 42: {other:?}"),
        }
    }
    gapped.sort_unstable();
    rooted.sort_unstable();
    assert_eq!(
        gapped,
        Vec::<&str>::new(),
        "the set of peoples gapping 'marsh' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![
            ("bugbear", "Qadoo".to_string()),
            ("gnoll", "Gshoovzngaov".to_string()),
            ("goblin", "Taneo".to_string()),
            ("hobgoblin", "Qaneo".to_string()),
            ("human", "Meashngeo".to_string()),
            ("kobold", "Rorora".to_string()),
        ],
        "at seed 42 all six placed peoples root 'marsh'"
    );
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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let exposures = exposure_from(&w, "kobold", &terrain, &climate).unwrap();
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

/// The Task 4 review's Important 2 (round 2): `cli/tests/correspondence.rs`
/// only checks that a concept declaring `Lexicalization::Expected` is
/// listed as core (or has a compound recipe) — a purely STATIC, per-name
/// check, blind to whether the `Steeped` rule that list-membership claims
/// actually fires in any world. `TOPONYMIC_CORE`
/// (`domains/language/src/packs.rs`) is a hand-maintained list asserting
/// "this concept can win a Root"; the property it claims lives here, in
/// `exposure_from`, which `hornvale_language` cannot depend on and so cannot
/// enforce. That gap is exactly how `spring`'s Critical 1 shipped
/// undetected in round 1: `Hydro::Spring` was structurally unreachable on
/// EVERY seed, not just seed 42, and nothing caught it before review.
///
/// This is the guard-rail: sweep a small, fixed, deterministic set of
/// seeds and require every core terrain concept to be `Steeped` for at
/// least one placed species on at least one of them — existence across a
/// real search of the reachable space, not a single seed's accident. A
/// concept that is structurally dead (like `Hydro::Spring` actually was)
/// fails this on every seed, so no sweep size saves it; a concept that is
/// merely unlucky at one seed (like `island`, `valley` at seed 42) only
/// needs the sweep to be wide enough to find its lucky one.
///
/// **There is no margin, and saying otherwise would be the third comment on
/// this gate to claim more than it delivers.** Originally swept over seeds
/// 0-7 (loop range `0..5`, seeds 0-4 actually exercised) and recorded every
/// witness: `island` was witnessed at seed 2 ALONE, `valley` at seeds 2 and
/// 7 only. Since this campaign deliberately breaks byte-identity, a later
/// terrain or settlement change can redden this test through no fault of
/// any gate — when that happens the honest repair is to widen the window
/// and re-record the witnesses, never to drop a concept from the
/// requirement.
///
/// **That happened.** The Wearing absorbed 77 commits from main (merge
/// `166d4ad9`; new terrain, settlement placement, and a fifth placed
/// people, `gnoll` — see `world()`'s doc comment), which moved `valley`'s
/// earliest witness from seed 2 to seed 5 and reddened this test (the loop
/// range was still only `0..5`, i.e. seeds 0-4, which no longer reached
/// it). Re-swept seeds 0-11 on the merged tree and recorded every witness:
/// `ford`/`hill`/`island`/`marsh`/`river`/`spring` are all witnessed
/// starting at seed 0 (`island`'s witness widened from "seed 2 alone" to
/// "seeds 0 and 1" — more redundant post-absorb, not less); `valley` is
/// witnessed at seeds 5, 7, 10, and 11 — first at seed 5. The loop range
/// below is widened to `0..8` (seeds 0-7) to comfortably cover `valley`'s
/// new earliest witness with one seed of margin (seed 7 also witnesses it,
/// so losing seed 5 alone would not immediately redden this again); the
/// early-break below means a typical run still only builds seeds 0-5 (six
/// worlds) before every concept is found. Wall-clock cost of the widened
/// sweep, measured on this box: seeds 0-7 in isolation take ~53s to build
/// and classify (seeds with zero placed peoples, e.g. 6 and 9 elsewhere in
/// the swept range, are cheap — no coexistence winner means no
/// `exposure_from` calls); the early break keeps the actual per-run cost
/// close to ~43s (seeds 0-5), under the roughly-a-minute budget this test
/// already implicitly accepted pre-absorb.
///
/// The set is **derived** from the language crate's own `concept_domain`
/// rather than duplicated. An earlier version of this test hardcoded the
/// seven and justified it by claiming the accession/correspondence tests
/// would catch a drifted list "on their own terms." That was checked by
/// injection and is false in the direction that matters: adding a
/// `Steeped`-impossible concept (`mountain`) to `TOPONYMIC_CORE` left
/// `cli/tests/accession.rs` 5/5 green and `cli/tests/correspondence.rs`
/// 4/4 green, and this test blind — which is precisely the shape of the
/// `spring` defect it exists to prevent. Removal was caught; addition, the
/// dangerous direction, was not. `concept_domain` is `pub`
/// (`domains/language/src/packs.rs`), and `cli/tests/correspondence.rs`
/// already documents preferring exactly this derivation, so there was never
/// a reason to duplicate.
#[test]
fn every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_sweep() {
    // Derived, never duplicated: whatever `TOPONYMIC_CORE` holds today is
    // what this test requires a witness for, so ADDING an unreachable
    // concept to that list reds this test instead of slipping past it.
    let core_toponymic: Vec<String> = {
        let w = build_world(
            hornvale_kernel::Seed(0),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        )
        .expect("seed 0 builds");
        w.registry
            .concepts()
            .filter(|c| hornvale_language::packs::concept_domain(&c.name) == Some("toponymic"))
            .map(|c| c.name.clone())
            .collect()
    };
    assert!(
        !core_toponymic.is_empty(),
        "no concept reports domain \"toponymic\" — the derivation broke, and an \
         empty requirement would make this test vacuously green"
    );
    let mut witnessed: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    for seed in 0u64..8 {
        let w = match build_world(
            hornvale_kernel::Seed(seed),
            &hornvale_astronomy::SkyPins::default(),
            SkyChoice::Generated,
            &hornvale_terrain::TerrainPins::default(),
            &SettlementPins::default(),
        ) {
            Ok(w) => w,
            Err(_) => continue,
        };
        let Ok(terrain) = hornvale_worldgen::terrain_of(&w) else {
            continue;
        };
        let Ok(climate) = hornvale_worldgen::climate_from(&w, &terrain) else {
            continue;
        };
        for (species, _) in placed_peoples(&w) {
            let Ok(exposures) = exposure_from(&w, species, &terrain, &climate) else {
                continue;
            };
            for concept in &core_toponymic {
                if matches!(
                    exposures.get(concept.as_str()),
                    Some(ExposureClass::Steeped)
                ) {
                    witnessed.insert(concept.clone());
                }
            }
        }
        if witnessed.len() == core_toponymic.len() {
            break;
        }
    }
    let missing: Vec<&String> = core_toponymic
        .iter()
        .filter(|c| !witnessed.contains(*c))
        .collect();
    assert!(
        missing.is_empty(),
        "these TOPONYMIC_CORE concepts never won a Root across seeds 0-4 on any \
         placed species — a structurally dead gate (exactly spring's Critical 1 \
         shape) would fail here on every seed, not just one: {missing:?}"
    );
}

#[test]
fn every_unknown_entrys_reason_is_non_empty() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    for species in ["goblin", "kobold"] {
        let exposures = exposure_from(&w, species, &terrain, &climate).unwrap();
        for (concept, class) in &exposures {
            if let ExposureClass::Unknown { reason } = class {
                let text = match reason {
                    GapReason::Experiential(s)
                    | GapReason::Perceptual(s)
                    | GapReason::Unnameable(s) => s,
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
fn exposure_from_is_pure_across_two_calls() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let a = exposure_from(&w, "goblin", &terrain, &climate).unwrap();
    let b = exposure_from(&w, "goblin", &terrain, &climate).unwrap();
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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();

    let exposures = exposure_from(&w, "kobold", &terrain, &climate).unwrap();
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
                hornvale_language::GapReason::Unnameable(s) => s,
            };
            assert!(
                !text.is_empty(),
                "unplaced-species gap for '{concept}' must carry a reason"
            );
        }
    }
    // The lexicon still assembles over that map without panicking.
    let lex = lexicon_from(&w, "kobold", &terrain, &climate).unwrap();
    assert_eq!(lex.entries().count(), exposures.len());
}

#[test]
fn a_kind_without_perception_fails_loudly_instead_of_borrowing_goblin_eyes() {
    // Before The Vigil, `exposure_from` resolved a hardcoded goblin baseline for
    // any kind with no perception row — so a bear classified colour as though
    // it saw like a goblin, and the dictionary printed "night-vision 0.5" as a
    // claim about dragons. The baseline is gone: no speaker lacks perception
    // (check_integrity enforces speech ⊆ perception), so the only kinds that
    // reach this path are plain fauna, and they must fail loudly.
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let err = exposure_from(&w, "owlbear", &terrain, &climate)
        .expect_err("plain fauna carries no perception");
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
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let dragon = exposure_from(&w, "red-dragon", &terrain, &climate).unwrap();
    let goblin = exposure_from(&w, "goblin", &terrain, &climate).unwrap();
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

/// Every people that can name north and east can name north-east. The four
/// cardinals are Steeped by universal-stratum membership; the four
/// intercardinals sit outside the stratum on purpose — giving them roots would
/// mint an unanalysable eighth word — so they need their own unconditional
/// `KnowsOf` rule to resolve as compounds instead of falling through to a
/// gap. Without that rule every people reads `gap (experiential): X has no
/// exposure to 'north-east'`, which is false of anyone who can walk.
#[test]
fn every_people_compounds_the_intercardinals_and_roots_the_cardinals() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();

    let peoples = ["goblin", "hobgoblin", "bugbear", "kobold"];
    for people in peoples {
        let lex = lexicon_from(&w, people, &terrain, &climate).unwrap();
        for cardinal in ["north", "south", "east", "west"] {
            match lex.entry(cardinal) {
                Some(LexEntry::Root { .. }) => {}
                other => panic!("{people}: {cardinal} should be a Root, got {other:?}"),
            }
        }
        for inter in ["north-east", "south-east", "south-west", "north-west"] {
            match lex.entry(inter) {
                Some(LexEntry::Compound { .. }) => {}
                other => panic!("{people}: {inter} should be a Compound, got {other:?}"),
            }
        }
    }
}

/// Anti-vacuity for the test above: it would pass just as happily over an
/// empty roster of peoples, and the compound claim is only meaningful if the
/// bearing concepts are actually registered in this world.
#[test]
fn the_bearing_exposure_check_runs_over_a_real_roster() {
    let w = world();
    for bearing in hornvale_language::BEARINGS {
        assert!(
            w.registry.concept(bearing).is_some(),
            "{bearing} should be registered in a built world"
        );
    }
    assert_eq!(
        hornvale_language::BEARINGS.len(),
        8,
        "the bearing roster should be the full eight points"
    );
}

/// The Generalist (Task 4): human's poor night vision (`night_vision =
/// 0.15`, Task 3) buys the deepest hue ladder and the shallowest luminance
/// ladder `pack_depths` offers — the depth-5 hue witness this campaign's
/// human addition is meant to exercise.
#[test]
fn human_is_the_hue_ladders_deepest_witness() {
    let wc = hornvale_worldgen::components::WorldComponents::assemble().unwrap();
    let p = *wc
        .perception
        .get(&hornvale_kernel::KindId("human"))
        .unwrap();
    let d = pack_depths(&p);
    assert_eq!(
        d.hue, 5,
        "human's poor night vision buys the deepest hue ladder"
    );
    assert_eq!(d.luminance, 1, "and the shallowest luminance ladder");
}
