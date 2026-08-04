//! The accession register: which *generation* each concept joined the
//! registry in.
//!
//! A concept's proto-root is assigned by a global ordered walk with
//! rejection-probing ([`crate::etymology::assign_proto_roots`]), so an
//! assignment depends on every concept sorted at or before it. Ordering by
//! name alone therefore makes registry growth *probabilistically* additive:
//! a concept inserted mid-alphabet can take a form some later concept would
//! have drawn, forcing it to probe, and every word derived from it moves.
//! (Measured before this module existed: of twelve species kinds added at
//! once, ten were free, `treant` moved 5 facts and `otyugh` 65.)
//!
//! Sorting by accession epoch first makes a new concept land **strictly
//! last**, the one position that provably displaces nothing — so growth
//! becomes additive by construction. See The Accession spec §3.
//!
//! # Appending a cohort
//!
//! Add a new `&[...]` to the end of [`EPOCH_COHORTS`]. **Never edit an
//! existing cohort**: a concept that changed epoch would re-sort, which is
//! exactly the churn this module exists to prevent. Retired concepts stay
//! listed — their slot is spent either way.
//!
//! # A withdrawn exception, kept here because the withdrawal is the lesson
//!
//! For two days (2026-07-27 → 2026-07-29) this doc carried an exception: a
//! campaign that bumps `ROOT_EPOCH` was said to be free to **re-found cohort
//! 0**, on the reasoning that a bump reseeds every root anyway so there is no
//! churn left to prevent. The Wearing exercised it, folding The Actants'
//! cohort back into a 76-concept baseline and adding its own nineteen
//! toponymic concepts there. It has been withdrawn, and the rule above is
//! absolute again. Three things went wrong, and each is worth more than the
//! exception was.
//!
//! **The fidelity argument that motivated it was false.** Ledger #9 held that
//! placing basic words like `hill` and `river` in a later cohort would mark
//! them as audibly borrowed, via LANG-55's coda carve. Measured
//! (`.superpowers/sdd/loanword-claim-measurement.md`, seeds 1..=250 × 4
//! proto-root units, 123 000 roots per epoch population): epoch-0 roots
//! already end closed 48.18 % of the time against epoch-1+'s 60.20 %, so a
//! closed-final word is ~4.5× likelier to be *old* than new; 99.99 % of
//! later-epoch roots have a CV skeleton epoch-0 roots also have; and the carve
//! is structurally inert in **74.6 %** of drawn languages, because
//! `draw_phonotactics` gives a language one or two coda templates of length
//! 0–1 and any single-template language is degenerate by construction. There
//! is no marking to avoid.
//!
//! **The epoch label does not cause a regeneration; it documents one.**
//! `ROOT_EPOCH` exists so that a deliberate change to the *assignment
//! algorithm* forces fresh draws rather than silently corrupting saves. The
//! Wearing never changed the algorithm — it changed the phonology the
//! algorithm draws from (Task 8's nucleus template set), which reseeds every
//! root whether or not any label moves. Reading "the epoch is bumped anyway,
//! so cohort placement is free" backwards into "a bump licenses a
//! re-founding" made a *documentation* suffix into a permission slip.
//!
//! **The exception could not be scoped.** Its own text had to spend twenty
//! lines forbidding the reading it invited ("the bump must be independently
//! necessitated — it is not a lever to pull for this"). A rule that needs a
//! second rule to stop it from swallowing the first is not a scoped exception;
//! it is the first rule repealed with extra steps. The Toponym appended its 53
//! concepts as an ordinary cohort on the same day, at no cost, which is the
//! demonstration that the exception bought nothing that appending does not
//! already give.
//!
//! See `.superpowers/sdd/decision-ledger.md` #9 and its 2026-07-29 amendment.
#![warn(missing_docs)]

/// Concepts grouped by accession epoch: cohort `i` is epoch `i`. Cohort 0 is
/// the registry as it stood when The Accession landed (2026-07-27); every
/// later campaign appends its own cohort rather than editing an earlier one.
///
/// `cli/tests/accession.rs` asserts this table and the concept registry agree
/// in both directions — a concept registered with no cohort entry would
/// silently default to epoch 0 and re-open the churn, which is the one
/// failure mode an authored table has.
/// type-audit: bare-ok(identifier-text)
pub const EPOCH_COHORTS: &[&[&str]] = &[
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
];

/// The accession epoch of `concept`: the index of the cohort listing it, or
/// `0` for a name no cohort mentions.
///
/// Defaulting to `0` is deliberate and fail-*safe*: an unlisted name keeps
/// today's ordering rather than jumping the queue, and the synthetic ids the
/// language unit tests use need no cohort entry. It is emphatically not
/// fail-*loud* — loudness is the parity test's job, because a panic here
/// would fire inside the world-generation draw path.
/// type-audit: bare-ok(identifier-text: concept), bare-ok(count: return)
pub fn concept_epoch(concept: &str) -> u32 {
    for (epoch, cohort) in EPOCH_COHORTS.iter().enumerate() {
        if cohort.contains(&concept) {
            return epoch as u32;
        }
    }
    0
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeSet;

    /// Cohort 0 is frozen forever: it is the roster whose assignments every
    /// later cohort is defined not to disturb, so its SIZE is the invariant,
    /// not the number of cohorts above it. (An earlier version of this test
    /// also pinned `EPOCH_COHORTS.len() == 1`, which was true the day The
    /// Accession landed and wrong the moment The Actants appended a cohort —
    /// the count is expected to grow, the baseline is not.)
    ///
    /// This pin was moved to 110 by The Wearing's cohort-0 re-founding and
    /// moved back here on 2026-07-29 when that re-founding was withdrawn (see
    /// this module's doc, and ledger #9's amendment). "Forever" in the sentence
    /// above is meant literally again: there is no bump, no campaign and no
    /// argument that re-opens this number.
    #[test]
    fn cohort_zero_stays_the_frozen_landing_roster() {
        assert_eq!(
            EPOCH_COHORTS[0].len(),
            76,
            "cohort 0 is the 76-concept roster frozen at The Accession; \
             growing it would re-sort concepts that already have assignments — \
             append a NEW cohort instead"
        );
    }

    /// Appending is the only legal growth, so later cohorts must be non-empty
    /// (an empty cohort is a placeholder nobody filled) and the table must
    /// only ever grow at the end.
    #[test]
    fn later_cohorts_are_non_empty() {
        for (epoch, cohort) in EPOCH_COHORTS.iter().enumerate().skip(1) {
            assert!(!cohort.is_empty(), "cohort {epoch} is empty");
        }
    }

    #[test]
    fn no_concept_appears_in_two_cohorts() {
        // A concept that changed epoch would re-sort and reshuffle every
        // assignment after it -- the churn this module prevents.
        let mut seen: BTreeSet<&str> = BTreeSet::new();
        for cohort in EPOCH_COHORTS {
            for name in *cohort {
                assert!(seen.insert(name), "{name} is listed in two cohorts");
            }
        }
    }

    #[test]
    fn a_listed_concept_reports_its_cohort_and_an_unlisted_one_reports_zero() {
        assert_eq!(concept_epoch("water"), 0);
        assert_eq!(concept_epoch("goblin-kind"), 0);
        // Synthetic ids used by the etymology unit tests are unlisted.
        assert_eq!(concept_epoch("zzz-late"), 0);
    }
}
