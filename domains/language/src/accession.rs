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

    #[test]
    fn epoch_zero_is_the_landing_roster() {
        assert_eq!(EPOCH_COHORTS.len(), 1, "one cohort at landing");
        assert_eq!(EPOCH_COHORTS[0].len(), 76, "76 concepts at The Accession");
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
