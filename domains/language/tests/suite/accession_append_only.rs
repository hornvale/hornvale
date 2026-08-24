//! Mechanical enforcement of `domains/language/src/accession.rs`'s own
//! absolute rule: "Never edit an existing cohort" (the module doc, and the
//! withdrawn-exception section beneath it). Nothing before this test
//! checked that mechanically — `no_concept_appears_in_two_cohorts` and the
//! parity tests in `accession_properties.rs` are set-difference checks
//! against the UNION of all cohorts, so they cannot see a concept that
//! moved from one existing cohort to another while the union stayed the
//! same, which is exactly the churn the epoch-first ordering exists to
//! prevent (a concept's accession epoch is what an assignment is sorted
//! by).
//!
//! So this test pins the exact, ordered contents of every cohort that
//! existed before Task 3 of The Confidant (2026-08-24) as a literal
//! snapshot (`FROZEN_HISTORICAL_COHORTS`, a verbatim copy of
//! `EPOCH_COHORTS`'s first 12 entries at that commit) and asserts the live
//! table still starts with exactly that prefix, in order. A campaign that
//! appends a new cohort at the end never has to touch this file; a
//! campaign that edits, reorders, or removes anything inside cohorts 0-11
//! makes it fail.

use hornvale_language::EPOCH_COHORTS;

/// Verbatim copy of `EPOCH_COHORTS[0..=11]` as committed by Task 2 of The
/// Confidant (the epoch-11 action-suite cohort), before this task appended
/// epoch 12. Never edit this array — it is the frozen half of the
/// invariant under test, not a roster to keep in sync with future growth.
const FROZEN_HISTORICAL_COHORTS: &[&[&str]] = &[
    // Epoch 0 — the baseline roster at The Accession (76 concepts).
    &[
        "abyssal",
        "alpine",
        "bathypelagic",
        "blood",
        "blue",
        "bone",
        "brown",
        "bugbear-kind",
        "child",
        "cold",
        "coral-reef",
        "dark",
        "day",
        "desert",
        "die",
        "earth",
        "eat",
        "eclipse",
        "epipelagic",
        "eye",
        "fire",
        "foot",
        "gloom",
        "goblin-kind",
        "god",
        "green",
        "hadal-trench",
        "hand",
        "hearth",
        "heat",
        "hobgoblin-kind",
        "home",
        "hydrothermal-vent",
        "ice",
        "kelp-forest",
        "kobold-kind",
        "light",
        "many",
        "mesopelagic",
        "moon",
        "mountain",
        "mouth",
        "name",
        "night",
        "one",
        "parent",
        "person",
        "rain",
        "red",
        "savanna",
        "sea",
        "sea-ice",
        "shadow",
        "shrubland",
        "sibling",
        "sleep",
        "snow",
        "spirit",
        "star",
        "starlit",
        "stone",
        "sun",
        "taiga",
        "temperate-forest",
        "temperate-grassland",
        "temperate-rainforest",
        "tide",
        "tree",
        "tropical-rainforest",
        "tropical-seasonal-forest",
        "tundra",
        "two",
        "upwelling",
        "water",
        "wind",
        "yellow",
    ],
    // Epoch 1 — The Actants (2026-07-27): the twelve creatures The Menagerie
    // left unnamed, and the three acts the GOAP roster performs that no
    // concept named. Appended, never merged into cohort 0, so every word
    // already spoken keeps its form.
    &[
        "black-dragon-kind",
        "drink",
        "giant-elk-kind",
        "giant-goat-kind",
        "move",
        "otyugh-kind",
        "owlbear-kind",
        "red-dragon-kind",
        "rest",
        "rust-monster-kind",
        "treant-kind",
        "twig-blight-kind",
        "white-dragon-kind",
        "woolly-mammoth-kind",
        "xorn-kind",
    ],
    // Epoch 2 — The Vacancy (2026-07-27): the fifth people and the twelve
    // fauna that filled the model's uninhabited declared states — three
    // unoccupied land climate regions, nine of the ten marine biomes, and the
    // dark trait combinations. Appended for the same reason epoch 1 was: every
    // word already spoken keeps its form.
    //
    // The two campaigns met here by accident and agreed. The Actants ruled
    // that every kind the biosphere registry holds owes a name, not only the
    // speaking peoples; The Vacancy was concurrently adding thirteen kinds to
    // that registry. This cohort is what that rule costs when the roster
    // grows, and the tripwire above is what made the cost visible on contact
    // instead of at a silent default to epoch 0.
    &[
        "carrion-crawler-kind",
        "dire-wolf-kind",
        "giant-constrictor-snake-kind",
        "giant-crocodile-kind",
        "giant-hyena-kind",
        "giant-octopus-kind",
        "giant-scorpion-kind",
        "giant-squid-kind",
        "gnoll-kind",
        "killer-whale-kind",
        "reef-shark-kind",
        "rhinoceros-kind",
        "shrieker-kind",
    ],
    // Epoch 3 — The Toponym: the named sub-types of a formation, the
    // vocabulary a settlement can be named for.
    &[
        "abyssal-plain",
        "bait-ball",
        "boreal-stand",
        "burn",
        "closed-canopy",
        "cold-upwelling",
        "coral-head",
        "crevasse-field",
        "damp-hollow",
        "erg",
        "felsenmeer",
        "fire-scrub",
        "fish-shoal",
        "forest-gap",
        "frost-heave",
        "gallery-forest",
        "grass-sward",
        "hamada",
        "holdfast-tangle",
        "ice-lead",
        "kelp-canopy",
        "liana-forest",
        "lightless-water",
        "marine-snow",
        "melt-pond",
        "mossy-deadfall",
        "muskeg",
        "nodule-field",
        "old-growth",
        "open-blue",
        "plankton-bloom",
        "playa",
        "pressure-ridge",
        "rafted-floe",
        "reef-rubble",
        "reg",
        "sargassum-drift",
        "scattering-layer",
        "sclerophyll-scrub",
        "scoured-ice",
        "smoker-field",
        "snowfield",
        "spur-and-groove",
        "staghorn-stand",
        "thorn-scrub",
        "trench-floor",
        "trench-wall",
        "tubeworm-thicket",
        "twilight-water",
        "urchin-barren",
        "vent-plume",
        "wind-scour",
        "wooded-grassland",
    ],
    // Epoch 4 — The Wearing (2026-07-27, re-seated 2026-07-29): the nineteen
    // words a place-name is built out of. Nine landforms a settled cell can BE
    // or sit beside (`hill`, `valley`, `river`, `ford`, `spring`, `marsh`,
    // `island`, `coast`, `lake`), gated on the real terrain query that put a
    // settlement there; and the ten relative/evaluative modifiers every
    // speaking people has unconditionally (`high`, `low`, `great`, `little`,
    // `new`, `old`, `under`, `over`, `north`, `south`), which live in
    // `packs::universal_stratum`.
    //
    // These were originally merged INTO cohort 0 under a re-founding — see the
    // withdrawn exception in this module's doc. They are appended here instead,
    // which is the ordinary and only legal growth. The cost is real and
    // accepted: at a later epoch they sort last, so they draw after every
    // earlier concept and take whatever the probe walk leaves them, forfeiting
    // the short-form priority `core_rank` would otherwise give the Swadesh
    // members among them.
    //
    // **The cost was MEASURED on the merged tree, not assumed**, because a
    // stated cost nobody counted is how this campaign got into the argument it
    // is climbing out of. Seeds 1..=250 contiguous, the real 176-concept
    // registry, real daughters, all four proto-root assignment units; two arms
    // over the identical universe/phonology/daughters differing only in where
    // these nineteen sit (epoch 4 as shipped, versus epoch 0 as the withdrawn
    // re-founding would have placed them). 4750 roots per arm per unit. Mean
    // root length, in syllables:
    //
    //     unit        epoch 4    epoch 0     delta
    //     goblinoid    1.9638     1.7918    +0.1720   (max 6 vs 5)
    //     draconic     1.9381     1.7714    +0.1667   (max 6 vs 6)
    //     gnoll        1.9375     1.7571    +0.1804   (max 4 vs 4)
    //     kobold       2.0861     1.8360    +0.2501   (max 5 vs 4)
    //
    // For goblinoid, 871 of 4750 roots are longer at epoch 4, 85 shorter, 3794
    // unchanged; kobold is worst at 1181 longer / 75 shorter. Of the four
    // high-frequency generics, `ford` moves most (+0.25 to +0.33 across the
    // units) and `river` least (+0.09 to +0.14), with `hill` +0.22 to +0.26 and
    // `coast` +0.20 to +0.28.
    //
    // A root of 3+ syllables can ONLY come from the probe walk
    // (`PROTO_ROOT_SYLLABLE_RANGE` is 1..=2, lengthened one syllable per
    // exhausted `PROBE_BUDGET`), so that share isolates this mechanism from
    // LANG-55's coda carve, which adds consonants and not nuclei. In the
    // shipped configuration it runs 0.94% / 1.92% / 0.89% / 1.55% at epochs
    // 0-3 and **8.67% at epoch 4** for goblinoid; 2.14% / 5.33% / 4.68% /
    // 7.79% and **16.23%** for kobold. Epochs 1-3 are all subject to the carve
    // and sit barely above epoch 0, so the carve is a small flat share and the
    // rest is arrival order against a saturating form space.
    //
    // Recorded as dropped, because it is plausible and wrong: 17 of these 19
    // are core (`packs::is_core_concept` — the 7 `TOPONYMIC_CORE` landforms
    // plus the 10 universal-stratum modifiers; only `coast` and `lake` are
    // periphery), and a core candidate must also clear a minimal-pair guard
    // against every core root already placed, so the cost looked like it should
    // concentrate on the core members. It does not: the delta is flat across
    // the split (goblinoid core +0.1694, periphery +0.1940). The guard makes
    // core roots longer in ABSOLUTE terms at both epochs; it is not what the
    // epoch-4 delta is made of.
    //
    // LANG-27's Zipf ordering therefore stays deferred for these nineteen —
    // the ordinary Accession trade (§3.3), paid at the size measured above and
    // not a new one.
    &[
        "coast", "ford", "great", "high", "hill", "island", "lake", "little", "low", "marsh",
        "new", "north", "old", "over", "river", "south", "spring", "under", "valley",
    ],
    // Epoch 5 — The Watershed: the staples. What grows here is a fact about a
    // place that does NOT follow from where the place is, which is why these
    // six move the descriptor space where nineteen site descriptors could not
    // (LANG-9's recorded limit).
    &["barley", "millet", "rice", "tuber", "vine", "wheat"],
    // Epoch 6 — The Vernacular, Part 2: the nine spectral classes, each
    // registered `lexeme: Absent(Void::Unnamed(...))` — a star's class is
    // real whether or not anyone here has invented spectroscopy, but no
    // culture has, so no word realizes it. These nine DO reach the exposures
    // map: `exposure_of_impl` closes with a loop over every registered
    // concept (`windows/worldgen/src/lib.rs`'s own doc: "the map's keys are
    // always exactly `world.registry.concepts()`'s names"), so nothing in
    // the registry is ever absent from it. What keeps a proto-root from
    // being drawn for them is `hornvale_language::lexicon::
    // proto_root_universe`'s `GapReason::Unnameable` filter, which excludes
    // exactly this classification from `build_lexicon`'s universe before
    // `assign_proto_roots` runs — a language-side exclusion, not a registry-
    // side absence. This cohort exists to satisfy the parity check above and
    // to keep the epoch-first sort giving these nine no influence on any
    // earlier-epoch assignment, per that filter and the ordering property
    // this module's own tests pin.
    &[
        "orange-dwarf",
        "yellow-dwarf",
        "yellow-white-dwarf",
        "red-dwarf",
        "sun-like-star",
        "white-dwarf",
        "orange-giant",
        "red-giant",
        "blue-giant",
    ],
    // Epoch 7 — the compass completed. `north` and `south` have been rooted
    // since epoch 4, where they arrived as *toponymic* elements (Northriver,
    // Southvalley) rather than as bearings; the exit graph has always been an
    // eight-point compass, so six of its eight directions could be travelled
    // and none of them named. These six close that gap, and a reverse audit
    // over `Compass::all()` now keeps it closed.
    //
    // The two cardinals get roots in the universal stratum. The four
    // intercardinals do NOT — they are compound-only concepts realized as
    // (cardinal, cardinal) through `packs::compound_recipe`, the same footing
    // as `sea` and `mountain`, because every attested language builds these by
    // composition rather than minting an unanalysable eighth word.
    //
    // Appended rather than folded into epoch 4 beside `north`/`south`, per
    // this module's absolute rule: re-sorting that cohort is exactly the churn
    // the epoch-first ordering exists to prevent, and the withdrawn exception
    // above is the record of what it costs to reason otherwise.
    &[
        "east",
        "west",
        "north-east",
        "south-east",
        "south-west",
        "north-west",
    ],
    // Epoch 8 — The Generalist (2026-08-03): the sixth people, human. A
    // single-concept cohort, appended per this module's absolute rule rather
    // than folded into any earlier peopled-kind cohort (0, 2).
    &["human-kind"],
    // Epoch 9 — The Delvers (2026-08-07): the dwarf family, three kinds.
    // ONE cohort, not three: the campaign is one arrival event and one epoch,
    // and three cohorts would assert three successive language epochs — a
    // stronger claim about the world's history than this campaign makes.
    // Appended rather than folded into an earlier peopled-kind cohort (0, 2,
    // 8), per this module's absolute rule.
    &["desert-dwarf-kind", "gully-dwarf-kind", "hill-dwarf-kind"],
    // Epoch 10 — The Radiation (C2d, 2026-08-09): the elf family, six kinds.
    // ONE cohort, not six: the campaign is one arrival event and one epoch,
    // and six cohorts would assert six successive language epochs — a far
    // stronger claim about the world's history than this campaign makes, and
    // one that would permanently separate the family's roots. Appended rather
    // than folded into an earlier peopled-kind cohort (0, 2, 8, 9), per this
    // module's absolute rule.
    //
    // The roster's largest family: six daughters against goblinoid's three and
    // dwarf's three. `domains/language/tests/accession_properties.rs`'s
    // `appending_the_elf_cohort_displaces_no_existing_proto_root` measures the
    // additivity this placement buys, and measures the mutant that would lose
    // it.
    &[
        "desert-elf-kind",
        "drow-kind",
        "high-elf-kind",
        "sea-elf-kind",
        "snow-elf-kind",
        "wood-elf-kind",
    ],
    // Epoch 11 — The Deed (2026-08-20), Task 2: the action suite's concept
    // roster. Seven in-character concepts
    // (`hornvale_language::action_suite_pack`), one pair per spec §3.2 verb
    // that named an unnamed act — written explicitly because a positional
    // "these eight verbs onto these seven concepts" reading is exactly the
    // kind of silent mispairing this table cannot afford: `map` -> `chart`,
    // `examine` -> `look`, `look` -> `look` (examine and look fold onto one
    // concept — a focused look at a named thing vs. a survey of
    // surroundings differ only by SCOPE, the same shape as `MoveTo`/
    // `MoveWithin` folding onto `move` by SCALE), `needs` -> `sense`,
    // `knows` -> `know`, `wait` -> `wait`, `write` -> `write`,
    // `consult` -> `read`. Every OTHER world-act verb in spec §3.2
    // (`go`/`back`/`enter`/`out`/`dive`/`surface`/`delve`/`climb`) is a kind
    // of *going* and reuses the existing `move` (epoch 1), minting nothing.
    // Seven out-of-character
    // concepts (`hornvale_language::extradiegetic_pack`) for the operator
    // instruments with no referent in the world —
    // `recount`/`survey`/`help`/`lens`/`identify`/`provoke`/`soothe` — each
    // classified `Unknown { reason: GapReason::Extradiegetic }`
    // unconditionally by `windows/worldgen`'s `exposure_of_impl`, which is
    // what keeps `hornvale_language::lexicon::proto_root_universe` from
    // ever drawing a proto-root for one (Task 1's filter, closed here).
    // Appended, never merged into an earlier cohort, per this module's
    // absolute rule.
    &[
        "chart", "help", "identify", "know", "lens", "look", "provoke", "read", "recount", "sense",
        "soothe", "survey", "wait", "write",
    ],
];

/// The live table must still carry every historical cohort, unchanged and
/// in order, as its own prefix. `EPOCH_COHORTS` is allowed to be LONGER
/// (a legitimate append), never different in its first 12 entries.
#[test]
fn existing_cohorts_are_never_edited() {
    assert!(
        EPOCH_COHORTS.len() >= FROZEN_HISTORICAL_COHORTS.len(),
        "EPOCH_COHORTS shrank below the frozen historical cohort count \
         ({}); a cohort was removed",
        FROZEN_HISTORICAL_COHORTS.len()
    );
    assert_eq!(
        &EPOCH_COHORTS[..FROZEN_HISTORICAL_COHORTS.len()],
        FROZEN_HISTORICAL_COHORTS,
        "an existing cohort's contents changed -- append a NEW cohort at \
         the end of EPOCH_COHORTS instead of editing one already there \
         (see accession.rs's module doc and its withdrawn-exception \
         section for what this costs)"
    );
}

/// Growth is legal: a new cohort appended after the frozen prefix does not
/// fail the check above. Anti-vacuity for `existing_cohorts_are_never_edited`
/// itself -- without this, an implementation that always compared
/// `EPOCH_COHORTS` to itself (e.g. by mistakenly slicing to its own length)
/// would pass trivially forever.
#[test]
fn the_live_table_has_grown_past_the_frozen_prefix() {
    assert!(
        EPOCH_COHORTS.len() > FROZEN_HISTORICAL_COHORTS.len(),
        "EPOCH_COHORTS has no cohort beyond the frozen historical prefix; \
         this test needs at least one appended cohort to be a meaningful \
         check of append-only growth rather than a check of exact equality"
    );
}
