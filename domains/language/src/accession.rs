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
//! **The one exception, and its boundary.** That rule is scoped to the
//! interval *between* epoch bumps. A campaign that bumps `ROOT_EPOCH` is a
//! declared total regeneration — every root reseeds regardless, so there is no
//! churn left to prevent — and it may therefore re-found cohort 0 as the
//! roster at that bump. The Wearing (2026-07-27) did exactly this, merging the
//! 15-concept Actants cohort back into a 91-concept baseline before adding its
//! own ~20. Outside a bump the rule is absolute; do not read this exception as
//! permission to edit a cohort in an ordinary campaign.
//!
//! **The bump must be independently necessitated — it is not a lever to pull
//! for this.** The Wearing's bump was forced by its phonology rework
//! (`draw_candidate` → `draw_syllables` reads `Phonology::nuclei`, which that
//! campaign changed), and the re-founding rode along on a regeneration that
//! was happening anyway. Bumping `ROOT_EPOCH` *in order to* unlock a
//! re-founding would invert the argument and hollow out the rule above: read
//! literally, "a bump permits a re-founding" would let any campaign buy its
//! way past The Accession. The real deterrent is the price — a bump reseeds
//! every word in every world and owes a full census regeneration and
//! re-baseline — but the principle does not rest on the price alone. If the
//! campaign would not have bumped the epoch anyway, it may not re-found the
//! cohort.
#![warn(missing_docs)]

/// Concepts grouped by accession epoch: cohort `i` is epoch `i`. Cohort 0 was
/// the registry as it stood when The Accession landed (2026-07-27), and was
/// re-founded at The Wearing's v4 root-epoch bump (2026-07-27) to fold in the
/// Actants cohort; every later campaign appends its own cohort rather than
/// editing an earlier one.
///
/// `cli/tests/accession.rs` asserts this table and the concept registry agree
/// in both directions — a concept registered with no cohort entry would
/// silently default to epoch 0 and re-open the churn, which is the one
/// failure mode an authored table has.
/// type-audit: bare-ok(identifier-text)
pub const EPOCH_COHORTS: &[&[&str]] = &[
    // Epoch 0 — the baseline roster re-founded at The Wearing's v4 root-epoch
    // bump (2026-07-27): the original 76-concept landing roster from The
    // Accession, merged with the 15-concept Actants cohort (The Actants,
    // 2026-07-27) for 91, plus the 19 toponymic concepts The Wearing itself
    // adds (Task 3) for 110 — all sorted by core_rank on merit. Legal only at
    // a root-epoch bump — see ledger #9 and the module doc above.
    &[
        "abyssal",
        "alpine",
        "bathypelagic",
        "black-dragon-kind",
        "blood",
        "blue",
        "bone",
        "brown",
        "bugbear-kind",
        "child",
        "coast",
        "cold",
        "coral-reef",
        "dark",
        "day",
        "desert",
        "die",
        "drink",
        "earth",
        "eat",
        "eclipse",
        "epipelagic",
        "eye",
        "fire",
        "foot",
        "ford",
        "giant-elk-kind",
        "giant-goat-kind",
        "gloom",
        "goblin-kind",
        "god",
        "great",
        "green",
        "hadal-trench",
        "hand",
        "hearth",
        "heat",
        "high",
        "hill",
        "hobgoblin-kind",
        "home",
        "hydrothermal-vent",
        "ice",
        "island",
        "kelp-forest",
        "kobold-kind",
        "lake",
        "light",
        "little",
        "low",
        "many",
        "marsh",
        "mesopelagic",
        "moon",
        "mountain",
        "mouth",
        "move",
        "name",
        "new",
        "night",
        "north",
        "old",
        "one",
        "otyugh-kind",
        "over",
        "owlbear-kind",
        "parent",
        "person",
        "rain",
        "red",
        "red-dragon-kind",
        "rest",
        "river",
        "rust-monster-kind",
        "savanna",
        "sea",
        "sea-ice",
        "shadow",
        "shrubland",
        "sibling",
        "sleep",
        "snow",
        "south",
        "spirit",
        "spring",
        "star",
        "starlit",
        "stone",
        "sun",
        "taiga",
        "temperate-forest",
        "temperate-grassland",
        "temperate-rainforest",
        "tide",
        "treant-kind",
        "tree",
        "tropical-rainforest",
        "tropical-seasonal-forest",
        "tundra",
        "twig-blight-kind",
        "two",
        "under",
        "upwelling",
        "valley",
        "water",
        "white-dragon-kind",
        "wind",
        "woolly-mammoth-kind",
        "xorn-kind",
        "yellow",
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

    /// Cohort 0 is frozen between epoch bumps: it is the roster whose
    /// assignments every later cohort is defined not to disturb. It is
    /// re-founded only by a campaign that bumps `ROOT_EPOCH`, when every root
    /// reseeds anyway and there is no churn left to prevent (The Wearing,
    /// 2026-07-27: 76 + the 15-concept Actants cohort + the 19 toponymic
    /// concepts of Task 3 = 110).
    #[test]
    fn cohort_zero_stays_the_frozen_landing_roster() {
        assert_eq!(
            EPOCH_COHORTS[0].len(),
            110,
            "cohort 0 is the 110-concept roster re-founded at The Wearing's \
             v4 root-epoch bump; growing it OUTSIDE such a bump would re-sort \
             concepts that already have assignments — append a NEW cohort instead"
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
