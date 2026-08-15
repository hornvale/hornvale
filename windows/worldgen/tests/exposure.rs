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
/// (see `marsh_is_a_root_for_every_placed_people_at_seed_42_except_goblin` below).
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
/// and reachability for a genuine `spring` Gap is still proven across the
/// census by `some_census_world_steeps_every_toponymic_concept`
/// (`windows/lab/tests/calibration.rs`, The Assay Task 9 — originally this
/// file's `every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_
/// sweep`, retired once the census carried the same coverage over 1,000
/// worlds). `hill` and `valley` are the two concepts that still
/// discriminate at seed 42 after this re-pin (each 1/5 Root, 4/5 Gap, and
/// each re-pinned alongside this test); `spring` joins `marsh`/`river`/
/// `ford` as saturated, which is why this test is renamed and rewritten
/// to match `marsh_is_a_root_for_every_placed_people_at_seed_42_except_goblin`'s shape
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
/// **The Tense (2026-08-05) — kobold traded `hill` for `valley`, exactly.**
/// All four toponymic partitions in this file moved together, and the symmetry
/// is the readable part:
///
/// ```text
///   hill    kobold rooted, 5 gapped   ->  NO rooter, all six gap
///   valley  no rooter, all six gap    ->  kobold roots it, 5 gap
///   spring  no rooter, all six gap    ->  goblin/hobgoblin/human root it
///   marsh   all six root it           ->  goblin gaps it
/// ```
///
/// Kobold is the authored HIGHLAND specialist and it has swapped the highland
/// concept for the lowland one. That is worth flagging rather than burying in
/// a re-pin: it is the same signal that made kobold's niche a live question
/// this campaign, and re-authoring the niche was measured and made things
/// strictly worse (see `domains/species`'s kobold doc and the campaign
/// retrospective). The cause is upstream of the authoring — era-varying
/// capacity punishes high-elevation niches, because elevation correlates with
/// cold and the era minimum binds hardest there.
///
/// Note also what did NOT happen, since "exposure shrank" was the expected
/// reading and is wrong: total gaps across these four concepts went 17 -> 15.
/// Slightly MORE exposure, not less, on 42% fewer settlements.
///
/// Every test below is renamed to state what it now measures. A name that
/// claims a partition the body no longer asserts is the failure mode decision
/// 0106 is about — a wrong label defends itself.
/// The Generalist re-pin (2026-08-03): human joins the coexistence stack as
/// a sixth competitor, redeciding seed 42's settlement placement once more —
/// kobold's flagship no longer has exposure to a spring cell either.
/// `spring` is saturated again: a Gap for every placed people (0/6 Root,
/// 6/6 Gap), the shape `river`/`ford` already carry. Renamed to match, per
/// this test's own established policy of renaming to the shape rather than
/// asserting a "discriminates" claim that is no longer true.
///
/// # The Delvers re-measure (C2c, 2026-08-07)
///
/// Three dwarves join the coexistence stack as Settled peoples seven through
/// nine, redeciding seed 42's settlement placement across the whole map. All
/// four toponymic partitions moved, and they moved THREE times inside one
/// campaign: on the first authoring (five dwarves, `MINERAL` diets), again
/// when the diets were corrected onto `DETRITUS`, and again when the roster
/// was cut from five to three (spec §11 — Mountain and Duergar withdrawn as
/// inexpressible depth kinds). All three readings are recorded, because the
/// differences between them are the clearest evidence in this file that these
/// partitions track TROPHIC placement and roster COMPOSITION, not merely
/// roster size:
///
/// ```text
///              five, MINERAL       five, DETRITUS        three, DETRITUS
///   hill     kobold roots, 10 gap  gnoll roots, 10 gap   kobold roots, 8 gap
///   valley   gnoll roots,  10 gap  bugbear+duergar,       gnoll+goblin+human
///                                  9 gap                  +kobold root, 5 gap
///   spring   NO rooter,    11 gap  duergar+kobold+       goblin roots,  8 gap
///                                  mountain-dwarf, 8 gap
///   marsh    5 root,        6 gap  6 root, 5 gap         5 root, 4 gap
/// ```
///
/// **The two withdrawn kinds' effects were not separable from the rest.** No
/// partition returned to the value it held before this campaign began: with
/// duergar and mountain-dwarf gone, `spring` is rooted by goblin (which had
/// it pre-campaign) but `hill` gains kobold (which nobody rooted
/// pre-campaign), `valley` goes from one rooter to four, and `marsh` loses
/// bugbear and human while gaining goblin and hill-dwarf. Removing two
/// competitors from a coexistence stack is not the inverse of adding them —
/// the three surviving dwarves still occupy attractors the pre-campaign
/// roster left to bugbear, goblin and human.
///
/// **What DID return exactly is the phonology.** Every people that roots a
/// concept both before this campaign and after the cut carries a
/// BYTE-IDENTICAL romanization: goblin's `spring` is `Nebao`, kobold's
/// `valley` is `Raxoroo`, and gnoll's / hobgoblin's / kobold's `marsh` are
/// `Gshoovzngaov` / `Qaneo` / `Rorora` — the same strings the pre-Delvers
/// six carried. That is the accession discipline working: cohort 9 is
/// strictly last, so shrinking it from five names to three displaces no
/// earlier concept's proto-root. Entries appear and disappear where exposure
/// does; words do not move.
///
/// Note also what did NOT happen: total gaps across these four concepts are
/// 25 on 9 peoples, i.e. 2.8 per people against 3.4 for the pre-Delvers six.
/// Proportionally MORE exposure, not less, on a half-again larger roster.
///
/// # THE RANGE re-measure (task 4, 2026-08-09)
///
/// Gnoll gains the campaign's first declared biome affinity (Desert), which
/// takes its seed-42 settlement count from 20 to 2 and — through the bake's
/// multi-era competition for ground — re-places every OTHER people too
/// (goblin 21 -> 12, kobold 34 -> 43, human 9 -> 17; measured in
/// `windows/worldgen/tests/range_readout.rs`). All four partitions moved
/// again:
///
/// ```text
///              three dwarves (pre-Range)      The Range
///   hill     kobold roots, 8 gap            NO rooter, 9 gap
///   valley   gnoll+goblin+human+kobold,     goblin+kobold root, 7 gap
///            5 gap
///   spring   goblin roots, 8 gap            NO rooter, 9 gap
///   marsh    5 root, 4 gap                  6 root, 3 gap
/// ```
///
/// `hill` and `spring` are saturated again — a Gap for every placed people —
/// so both tests are RENAMED to the shape they now measure rather than kept
/// under a name asserting a rooter that no longer exists (decision 0106: a
/// wrong label defends itself; this file's own established policy).
///
/// **The phonology returned byte-identical once more**, which is the reading
/// that matters most here: kobold's `marsh` is `Rorora` and its `valley` is
/// `Raxoroo`, hobgoblin's `marsh` is `Qaneo` — the same strings these peoples
/// carried before The Delvers and before this campaign. Entries appear and
/// disappear where exposure does; words do not move. Nothing about a biome
/// affinity touches the accession discipline, and this is the evidence.
///
/// Total gaps across the four concepts are 28 on 9 peoples (3.11 per people),
/// against 25 (2.8) before. Slightly LESS exposure this time — the opposite
/// direction from the last two re-measures, which is worth recording because
/// "suppressing a people shrinks the world's vocabulary" is the obvious story
/// and it has now gone both ways.
///
/// **THE RADIATION re-measure (C2d, 2026-08-10).** Six elves join the contest
/// and the four tables become:
///
/// ```text
///                  before (9 peoples)      after (15 peoples)
///   spring   0 root, 9 gap             0 root, 15 gap
///   hill     0 root, 9 gap             0 root, 15 gap
///   valley   2 root, 7 gap             2 root, 13 gap
///   marsh    6 root, 3 gap             6 root,  9 gap
/// ```
///
/// The rooter COUNTS are unchanged on all four concepts while the roster grew
/// by two thirds — six new peoples won exactly zero net new roots. The
/// membership turned over inside those counts (goblin lost `valley` and
/// `marsh`, bugbear lost `marsh`, gnoll regained `marsh`; sea-elf gained
/// `valley` and drow gained `marsh`) so this is redistribution, not
/// saturation. Total gaps are 52 on 15 peoples (3.47 per people) against 28 on
/// 9 (3.11): exposure per people fell slightly again.
///
/// **AND THE PHONOLOGY RETURNED BYTE-IDENTICAL FOR THE THIRD ROSTER EPOCH
/// RUNNING.** kobold's `valley` is `Raxoroo` and its `marsh` is `Rorora`;
/// hobgoblin's `marsh` is `Qaneo`, hill-dwarf's is `Tag`, human's is
/// `Meashngeo` — every string these peoples carried before The Delvers, before
/// The Range and before this campaign. Six kinds and one appended accession
/// cohort moved which entries EXIST; they moved no word that already existed.
/// That is the accession discipline's additivity claim, measured on the
/// rendered product rather than argued from the cohort rule.
///
/// **THE GLASSHOUSE re-measure (Stage B, decision 0134) — the first cause on
/// this list that is not a roster change.** The craton rescale delivers its
/// budget, so seed 42's coastline rose to the shelf break and mean land
/// elevation fell 2257 -> 1783 m. The roster is untouched at fifteen peoples;
/// the GROUND under them moved, and every one of these four concepts is a
/// toponymic exposure read off that ground. The four tables become:
///
/// ```text
///                  before (15 peoples)     after (15 peoples)
///   spring   0 root, 15 gap            8 root,  7 gap
///   hill     3 root, 12 gap            1 root, 14 gap
///   valley   2 root, 13 gap            6 root,  9 gap
///   marsh    6 root,  9 gap            6 root,  9 gap
/// ```
///
/// Total roots 11 -> 21 on an unchanged roster: total gaps fall 49 -> 39, 3.27
/// to 2.60 per people. Exposure per people rose for the first time in this
/// file's history, and the mechanism is not mysterious — a coastline at the
/// shelf break puts more peoples within reach of a spring and a valley, and
/// fewer within reach of a hill, because the hills got shorter.
///
/// **AND THE PHONOLOGY RETURNED BYTE-IDENTICAL FOR THE FOURTH EPOCH RUNNING,
/// THIS TIME UNDER A TERRAIN EPOCH RATHER THAN A ROSTER CHANGE.** Every people
/// that kept a root kept the same word, without exception: on `marsh`, drow
/// `Goo`, gnoll `Gshoovzngaov`, hobgoblin `Qaneo`, kobold `Rorora` and snow-elf
/// `Boosh` — five of five surviving entries; on `valley`, sea-elf `Nadbbeus`,
/// and kobold's newly-recovered `valley` is `Raxoroo`, the same string this
/// comment recorded for it three campaigns ago. Ten entries appeared and two
/// disappeared; **zero words moved**. That is a stronger reading of the
/// accession discipline's additivity than any of the roster re-measures above,
/// because a roster change at least leaves an incumbent people's own
/// circumstances alone, and this one did not: it moved the very ground the
/// exposure is read from, and the phonology still did not notice.
///
/// All four tests are RENAMED to the shape they now measure, per this file's
/// established policy (decision 0106 — a wrong label defends itself).
///
/// **THE GLASSHOUSE re-measure (Stage B Task 4).** The thermostat (a damped,
/// greenhouse-forced insolation baseline replacing the fixed 288 K blackbody
/// one) re-placed every settlement again — a second cause on this list that
/// is not a roster change. `spring` moves from 7 root/8 gap to 9 root/6 gap:
/// bugbear, desert-dwarf, high-elf and hill-dwarf newly root it (hill-dwarf
/// keeping its `Maqtog` from before); desert-elf and human, its two rooters
/// under the craton-rescale reading, gap it instead.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 5, the area-mean-zero latitude
/// profile).** `spring` moves from 9 root/6 gap to 5 root/10 gap: bugbear,
/// desert-dwarf, high-elf, kobold and snow-elf all gap it now, leaving
/// desert-elf, gnoll, gully-dwarf, hill-dwarf and hobgoblin — desert-elf's
/// `Zeuz` and hill-dwarf's `Maqtog` both BYTE-IDENTICAL to the words they
/// carried before this re-measure. Re-measured wholesale, not hand-edited.
///
/// **THE GLASSHOUSE re-measure (Stage B, `k` re-decided 0.4 → 0.3).**
/// `spring` holds at 5 root/10 gap — the ONLY one of this file's four
/// concepts whose count did not move under `k` — but the membership swapped
/// underneath it: desert-elf and gully-dwarf gap it, goblin (`Nebao`) and
/// snow-elf (`Booz`) gain it. hill-dwarf keeps `Maqtog`. The test keeps its
/// name, and that is precisely the hazard worth naming: a count-shaped name
/// is stable across a change that replaced two-fifths of the set, so **the
/// name is not the assertion** — the named partition below is. A reviewer
/// checking only that the name still reads true would have seen nothing
/// here.
#[test]
fn spring_is_a_root_at_seed_42_for_five_peoples() {
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
        vec![
            "bugbear",
            "desert-dwarf",
            "desert-elf",
            "drow",
            "gully-dwarf",
            "high-elf",
            "human",
            "kobold",
            "sea-elf",
            "wood-elf",
        ],
        "the set of peoples gapping 'spring' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![
            ("gnoll", "Dzhaap".to_string()),
            ("goblin", "Nebao".to_string()),
            ("hill-dwarf", "Maqtog".to_string()),
            ("hobgoblin", "Negao".to_string()),
            ("snow-elf", "Booz".to_string()),
        ],
        "the set of peoples rooting 'spring' at seed 42 moved"
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
/// (see `valley_is_a_gap_for_every_placed_people_at_seed_42_except_kobold_which_roots_it_except_bugbear_
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
///
/// The Delvers re-pin (C2c, 2026-08-07): the sole rooter is KOBOLD, as
/// `Roxoro`. This partition moved three times inside one campaign — kobold
/// under the first dwarf authoring, gnoll when the diets were corrected off
/// `MINERAL`, kobold again when the roster was cut to three — while holding
/// the 1-rooter shape throughout (now 1/9 Root, 8/9 Gap). It did NOT return
/// to its pre-campaign value: before The Delvers NO people rooted `hill` at
/// seed 42. See the file-level note on
/// `spring_is_a_gap_at_seed_42_except_for_goblin_which_roots_it` for why a
/// quantity that has moved four times under changes that never touched
/// kobold's own niche is a threshold being crossed rather than a trend.
///
/// The Radiation re-pin (C2d task 6, 2026-08-10), and it is a RENAME as well
/// as a re-pin: `hill` splits 3/15 Root, 12/15 Gap — gnoll (`Pzoav`), kobold
/// (`Roxoro`) and wood-elf (`Nguznguu`). The previous pin, taken earlier in
/// this same campaign, read 0/15 Root, and the test name asserted that shape
/// in words; three rooters contradict it outright, so the name moves with the
/// value rather than being left to say something false. **Kobold keeps the
/// BYTE-IDENTICAL `Roxoro`** it has carried since The Generalist, across
/// three roster epochs and four placement changes — the entry set moved, the
/// phonology did not, which is the same separation `valley` records for the
/// same word.
///
/// Read alongside the paragraph above: this concept has now moved SIX times
/// under changes that never touched kobold's own niche, and it has held every
/// shape from 0 rooters to 3. That is a threshold sitting near the middle of
/// the roster's elevation spread, not a trend in anybody's niche, and this
/// test asserts no mechanism for it.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 4).** `hill` splits 2/15 Root,
/// 13/15 Gap: hobgoblin keeps its byte-identical `Nootea`, and gully-dwarf
/// joins it as a second rooter (`Ngab`), the same climate epoch that moved
/// `spring`/`valley`/`marsh` below.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 5, the area-mean-zero latitude
/// profile).** `hill` splits 1/15 Root, 14/15 Gap: gully-dwarf gaps it
/// again, leaving hobgoblin alone — still carrying its byte-identical
/// `Nootea`. Re-measured wholesale, not hand-edited.
///
/// **THE GLASSHOUSE re-measure (Stage B, `k` re-decided 0.4 → 0.3).** `hill`
/// splits 2/15 Root, 13/15 Gap: drow joins hobgoblin (`Godgoo`), and
/// hobgoblin STILL carries the byte-identical `Nootea` — seven re-measures
/// now without that word moving, which is the discriminator this file cares
/// about. A surviving rooter whose word is unchanged means EXPOSURE moved,
/// not phonology. The test is renamed rather than left claiming "alone".
///
/// This one is worth reading beside its three siblings, because they moved
/// TOGETHER and in one direction: `hill` 1 → 2 rooters, `marsh` 5 → 6,
/// `valley` 3 → 5, `spring` 5 → 5 with two swaps. Warming the population
/// added settlement (826 occupations across 302 sites, against 620 across
/// 217 at `k = 0.4`), settlement is what puts a people beside a landform,
/// and a people beside a landform roots the word for it. That is the whole
/// causal chain this file exists to expose, running forwards for once
/// instead of being read backwards out of a drift.
#[test]
fn hill_is_a_root_at_seed_42_for_hobgoblin_and_drow() {
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
        vec![
            "bugbear",
            "desert-dwarf",
            "desert-elf",
            "gnoll",
            "goblin",
            "gully-dwarf",
            "high-elf",
            "hill-dwarf",
            "human",
            "kobold",
            "sea-elf",
            "snow-elf",
            "wood-elf",
        ],
        "the set of peoples gapping 'hill' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![
            ("drow", "Godgoo".to_string()),
            ("hobgoblin", "Nootea".to_string()),
        ],
        "the set of peoples rooting 'hill' at seed 42 moved"
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
///
/// The Delvers re-pin (C2c, 2026-08-07): FOUR peoples root `valley` — gnoll,
/// goblin, human and kobold — so the partition leaves the 1-rooter shape it
/// has carried through every prior re-pin and splits 4/9 Root, 5/9 Gap, the
/// widest spread this concept has ever shown. It moved three times inside the
/// campaign (gnoll alone; bugbear + duergar; these four), which is the same
/// threshold-crossing behaviour `hill` shows and is read the same way. Kobold
/// keeps the BYTE-IDENTICAL `Raxoroo` it carried before The Delvers — the
/// entry set moved, the phonology did not. See the file-level note on
/// `spring_is_a_gap_at_seed_42_except_for_goblin_which_roots_it`.
///
/// The Radiation re-pin (C2d task 6, 2026-08-10), a RENAME as well as a
/// re-pin: `valley` is back to the 1-rooter shape it has carried through most
/// of its history, and the sole rooter is SEA-ELF (`Nadbbeus`). Kobold loses
/// it — the mirror of `hill`, where kobold regains a root at the same regen,
/// and the two concepts have now swapped shapes in opposite directions twice.
/// The previous name said "for goblin and kobold" and the previous pin
/// already said neither of those alone; neither survives, so the name moves
/// with the value. Sea-elf's `Nadbbeus` is BYTE-IDENTICAL to the word it
/// carried at the previous pin.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 4).** `valley` moves from 6
/// root/9 gap to 2 root/13 gap: gnoll, goblin, hobgoblin and kobold gap it
/// now, leaving only drow (`Gadbvoo`) and sea-elf (`Nadbbeus`) rooting it —
/// both BYTE-IDENTICAL to their previous words.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 5, the area-mean-zero latitude
/// profile).** `valley` moves from 2 root/13 gap to 3 root/12 gap: drow gaps
/// it now; gnoll and hobgoblin regain it (`Dsavshmaov`/`Konoa`, both
/// BYTE-IDENTICAL to words this file has recorded for them before). Sea-elf
/// keeps its `Nadbbeus`, unchanged across every re-measure in this file's
/// history. Re-measured wholesale, not hand-edited.
///
/// **THE GLASSHOUSE re-measure (Stage B, `k` re-decided 0.4 → 0.3).**
/// `valley` moves 3 root/12 gap → 5 root/10 gap, its widest yet: gnoll gaps
/// it, and drow (`Gadbvoo`), high-elf (`Mazbveos`) and human (`Ngaatae`) all
/// gain it. Both survivors keep their words — hobgoblin's `Konoa` and
/// sea-elf's `Nadbbeus`, the latter still unmoved across the whole of this
/// file's history. Renamed from `..._for_three_peoples`. Re-measured
/// wholesale, not hand-edited.
#[test]
fn valley_is_a_root_at_seed_42_for_five_peoples() {
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
        vec![
            "bugbear",
            "desert-dwarf",
            "desert-elf",
            "gnoll",
            "goblin",
            "gully-dwarf",
            "hill-dwarf",
            "kobold",
            "snow-elf",
            "wood-elf",
        ],
        "the set of peoples gapping 'valley' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![
            ("drow", "Gadbvoo".to_string()),
            ("high-elf", "Mazbveos".to_string()),
            ("hobgoblin", "Konoa".to_string()),
            ("human", "Ngaatae".to_string()),
            ("sea-elf", "Nadbbeus".to_string()),
        ],
        "the set of peoples rooting 'valley' at seed 42 moved"
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
/// which `some_census_world_steeps_every_toponymic_concept`
/// (`windows/lab/tests/calibration.rs`, The Assay Task 9 — originally this
/// file's `every_core_toponymic_concept_wins_a_root_somewhere_in_a_seed_
/// sweep`) already proves).
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
///
/// The Delvers re-pin (C2c, 2026-08-07): `marsh` splits 5/9 Root, 4/9 Gap.
/// BUGBEAR AND HUMAN LOST IT (`Qadoo` and `Meashngeo` are gone); GOBLIN
/// regained it as `Taneo` and `hill-dwarf` gained it as `Tag`. Under the
/// five-kind authoring `hill-dwarf` held it as `Ngabsmab` and `duergar` as
/// `Snadsnad`; cutting the roster to three moved hill-dwarf's own word, which
/// is expected — a kind's romanization is drawn against its own language, and
/// hill-dwarf's language is drawn inside a cohort that shrank.
///
/// **Case (2) again, verified rather than assumed.** Every people OUTSIDE the
/// dwarf cohort that rooted `marsh` before this campaign and still does kept
/// a BYTE-IDENTICAL root across all three movements (`Gshoovzngaov`,
/// `Qaneo`, `Rorora`). The phonology of the standing roster did not move;
/// entries appeared and disappeared where exposure did.
///
/// THE RANGE re-pin (task 4, 2026-08-09): `marsh` splits 6/9 Root, 3/9 Gap —
/// the WIDEST it has been. Bugbear and human regain it (`Qadoo` and
/// `Meashngeo`, both byte-identical to the words they held two re-pins ago)
/// while GNOLL loses it, which is the one movement this campaign can claim
/// directly: gnoll is the kind whose affinity was declared, and its two
/// surviving seed-42 settlements no longer sit beside a marsh cell. The other
/// two are the competitive cascade.
///
/// **Case (2) a third time.** Every people that rooted `marsh` before this
/// campaign and still does kept a BYTE-IDENTICAL root (`Taneo`, `Tag`,
/// `Qaneo`, `Rorora`), and the two that regained it did so with the exact
/// strings they carried when they last held it. A romanization has still
/// never moved for a reason other than its own cohort changing.
///
/// The Radiation re-pin (C2d task 6, 2026-08-10): `marsh` splits 7/15 Root,
/// 8/15 Gap — wider still, and the name moves from "six peoples" to "seven".
/// It is STILL exactly one dwarf, but not the same one: hill-dwarf loses it
/// and DESERT-dwarf gains it as `Dag`. Human loses it; snow-elf (`Boosh`) and
/// wood-elf (`Gee`) gain it.
///
/// **Case (2) a fourth time, verified rather than assumed.** Every people that
/// rooted `marsh` at the previous pin and still does kept a BYTE-IDENTICAL
/// root — gnoll's `Gshoovzngaov`, hobgoblin's `Qaneo`, kobold's `Rorora`,
/// drow's `Goo`. A romanization has still never moved for a reason other than
/// its own cohort changing; entries appear and disappear where exposure does.
/// Note desert-dwarf's `Dag` against hill-dwarf's departed `Tag`: two words in
/// the same family's cohort, one letter apart and independently drawn — near
/// neighbours, not a renamed pin.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 4).** `marsh` moves from 6
/// root/9 gap to 7 root/8 gap: gnoll and desert-elf gap it now; bugbear,
/// goblin and wood-elf gain it. **No dwarf roots it, for the fifth
/// consecutive re-measure** (all three dwarf kinds — desert-dwarf,
/// gully-dwarf, hill-dwarf — gap it). drow's `Goo`, hobgoblin's `Qaneo`,
/// kobold's `Rorora` and snow-elf's `Boosh` are all BYTE-IDENTICAL to the
/// words they carried before.
///
/// **THE GLASSHOUSE re-measure (Stage B Task 5, the area-mean-zero latitude
/// profile).** `marsh` moves from 7 root/8 gap to 5 root/10 gap: bugbear,
/// snow-elf and wood-elf gap it now; gnoll regains it (`Gshoovzngaov`,
/// BYTE-IDENTICAL to the word it carried two re-measures ago). **No dwarf
/// roots it, for the sixth consecutive re-measure.** drow's `Goo`, goblin's
/// `Taneo`, hobgoblin's `Qaneo` and kobold's `Rorora` are all unchanged.
/// Re-measured wholesale, not hand-edited.
///
/// **THE GLASSHOUSE re-measure (Stage B, `k` re-decided 0.4 → 0.3).**
/// `marsh` moves 5 root/10 gap → 6 root/9 gap: goblin and kobold gap it,
/// while bugbear (`Qadoo`), desert-elf (`Geesh`) and snow-elf (`Boosh`) gain
/// it. Every survivor is byte-identical — drow's `Goo`, gnoll's
/// `Gshoovzngaov`, hobgoblin's `Qaneo`. **No dwarf roots it, for the seventh
/// consecutive re-measure**, which is now the longest-running invariant this
/// file holds and the reason the clause is in the test's name. Renamed from
/// `..._for_five_peoples_and_no_dwarf`. Re-measured wholesale, not
/// hand-edited.
#[test]
fn marsh_is_a_root_at_seed_42_for_six_peoples_and_no_dwarf() {
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
        vec![
            "desert-dwarf",
            "goblin",
            "gully-dwarf",
            "high-elf",
            "hill-dwarf",
            "human",
            "kobold",
            "sea-elf",
            "wood-elf",
        ],
        "the set of peoples gapping 'marsh' at seed 42 moved"
    );
    assert_eq!(
        rooted,
        vec![
            ("bugbear", "Qadoo".to_string()),
            ("desert-elf", "Geesh".to_string()),
            ("drow", "Goo".to_string()),
            ("gnoll", "Gshoovzngaov".to_string()),
            ("hobgoblin", "Qaneo".to_string()),
            ("snow-elf", "Boosh".to_string()),
        ],
        "the set of peoples rooting 'marsh' at seed 42 moved, and no dwarf \
         is among them"
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

/// THE DELVERS (F1): `lexicon_of_in_from` resolved the kind AFTER calling the
/// panicking `language_of_in`, so a species outside the component set killed
/// the calling thread instead of returning the `BuildError` this function's
/// own signature promises. That is how the campaign's census died: a Lab
/// worker asked for a synthetic roster's kind and got a panic, not an `Err`
/// its caller was already written to handle.
///
/// The resolution now happens first. `goblin-twin` is the exact species that
/// crashed — it is the Lab's null-control twin, deliberately absent from every
/// canonical registry.
#[test]
fn a_species_outside_the_component_set_is_an_error_not_a_panic() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let err = lexicon_from(&w, "goblin-twin", &terrain, &climate)
        .expect_err("the canonical roster has no goblin-twin");
    let msg = format!("{err:?}");
    assert!(
        msg.contains("goblin-twin") && msg.contains("unknown species"),
        "the error must name the unresolvable kind, got {msg}"
    );
}

/// The wc-threaded twin measures the roster it is handed. Threading the
/// CANONICAL set through `lexicon_from_in` must reproduce `lexicon_from`
/// byte-for-byte — the property that lets the Lab switch every lexicon read
/// onto `_in` without moving a single value on `the-census`' default roster.
#[test]
fn lexicon_from_in_over_the_canonical_set_equals_lexicon_from() {
    let w = world();
    let terrain = hornvale_worldgen::terrain_of(&w).unwrap();
    let climate = hornvale_worldgen::climate_from(&w, &terrain).unwrap();
    let wc = hornvale_worldgen::WorldComponents::assemble().expect("canonical registries");
    for species in ["goblin", "kobold", "hill-dwarf"] {
        let threaded = hornvale_worldgen::lexicon_from_in(&w, &wc, species, &terrain, &climate)
            .unwrap_or_else(|e| panic!("lexicon_from_in({species}): {e:?}"));
        let assembled = lexicon_from(&w, species, &terrain, &climate)
            .unwrap_or_else(|e| panic!("lexicon_from({species}): {e:?}"));
        let rendered = |lex: &hornvale_language::Lexicon| -> Vec<String> {
            lex.entries()
                .map(|(c, e)| format!("{c}={e:?}"))
                .collect::<Vec<_>>()
        };
        assert_eq!(
            rendered(&threaded),
            rendered(&assembled),
            "{species}: threading the canonical set must be a no-op"
        );
    }
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
