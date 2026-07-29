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
