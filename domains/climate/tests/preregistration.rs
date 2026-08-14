//! The Axes — preregistered criteria, frozen before the fit (spec §6).
//!
//! **These bounds derive from CORPUS SIZE** — 74 names, being 21 `Formation`s
//! and 53 `Variant`s — which was known before any assignment existed. They do
//! not derive from the fit's outcome. That is what makes them auditable as
//! un-tuned, and it is the reason the freeze could be written first at all.
//!
//! The criterion the metaplan's §7 stated could not fail: "no two names collide
//! and no name resists" is satisfied *perfectly* by one axis with 21 values,
//! which is the enum. A floor with no ceiling. `p1_the_decomposition_is_
//! compressive` is the missing ceiling.
//!
//! **A red here is not automatically a bug.** See the plan's branch table:
//! coverage and compression proving jointly unsatisfiable is the campaign's
//! headline finding — the corpus is not compositional — and is explicitly not
//! grounds for loosening a bound after unblinding.

use hornvale_climate::axes::{AssignedName, assignment};
use hornvale_kernel::environment_v1_basis;
use std::collections::{BTreeMap, BTreeSet};

/// Distinct values actually used on each axis, **measured from the assignment
/// rather than declared**. A declared cardinality could be set low to pass;
/// this cannot.
fn realised_cardinalities() -> BTreeMap<u16, usize> {
    let mut seen: BTreeMap<u16, BTreeSet<u64>> = BTreeMap::new();
    for AssignedName { vector, .. } in assignment() {
        for axis in environment_v1_basis() {
            if let Some(v) = vector.get(*axis) {
                seen.entry(axis.id).or_default().insert(v.to_bits());
            }
        }
    }
    seen.into_iter().map(|(k, v)| (k, v.len())).collect()
}

#[test]
fn p1_the_decomposition_is_compressive() {
    let card = realised_cardinalities();
    let sum: usize = card.values().sum();
    let product: u128 = card.values().map(|c| *c as u128).product();
    // Printed because a passing assertion hides its own measurement, and these
    // are the numbers the campaign reports. `--nocapture` to read them.
    println!(
        "P-1: cardinalities {card:?} -> sum {sum} (bound <= 30), product {product} (bound >= 500)"
    );

    // COVERAGE: >= 74 is necessary to distinguish the corpus at all; 500 is
    // ~7x headroom, so the space is generative rather than exactly-enumerating.
    assert!(
        product >= 500,
        "coverage: axis cardinality product {product} < 500 (cardinalities {card:?})"
    );
    // COMPRESSION: the enum costs 74 symbols; 30 is under 41% of that. Without
    // this clause, one axis with 21 values passes the whole criterion.
    assert!(
        sum <= 30,
        "compression: axis cardinality sum {sum} > 30 (cardinalities {card:?})"
    );
}

#[test]
fn p1_no_two_names_share_a_vector() {
    let mut by_vector: BTreeMap<Vec<(u16, u64)>, Vec<&'static str>> = BTreeMap::new();
    for AssignedName { name, vector, .. } in assignment() {
        if vector.is_unassigned() {
            continue; // a resister; p2 owns those
        }
        let key: Vec<(u16, u64)> = environment_v1_basis()
            .iter()
            .filter_map(|a| vector.get(*a).map(|v| (a.id, v.to_bits())))
            .collect();
        by_vector.entry(key).or_default().push(name);
    }
    let collisions: Vec<&Vec<&'static str>> =
        by_vector.values().filter(|names| names.len() > 1).collect();
    assert!(
        collisions.is_empty(),
        "distinct names collided on one vector, so the axes are too coarse: {collisions:?}"
    );
}

/// The ten names predicted to resist, frozen **before** the fit (spec §6.2).
///
/// Six are post-event succession and four are seasonal phases of sea ice. The
/// model behind the prediction is one sentence: *a phase is not a point in a
/// state space*. `DISTURBANCE` is the basis's only `Rate` axis, and a
/// whole-community name cannot carry a value on a rate.
///
/// Operational definition of resistance, so the criterion is not a judgement
/// call by the author who wants it to succeed: a name resists iff assigning it
/// would require an axis value no other name uses — a cardinality increase that
/// buys exactly one name.
const PREDICTED_RESISTERS: &[&str] = &[
    "forest-gap",
    "mossy-deadfall",
    "burn",
    "fire-scrub",
    "reef-rubble",
    "urchin-barren",
    "pressure-ridge",
    "ice-lead",
    "rafted-floe",
    "melt-pond",
];

#[test]
fn p2_the_resisters_are_the_ten_phase_variants() {
    let actual: BTreeSet<&str> = assignment()
        .iter()
        .filter(|a| a.vector.is_unassigned())
        .map(|a| a.name)
        .collect();
    let predicted: BTreeSet<&str> = PREDICTED_RESISTERS.iter().copied().collect();
    assert_eq!(
        actual,
        predicted,
        "PREREGISTERED PREDICTION MOVED. predicted-but-assigned: {:?}; \
         resisted-but-unpredicted: {:?}. A falsification here is a FINDING: \
         record it in this doc comment and change the constant in the same \
         commit, never silently.",
        predicted.difference(&actual).collect::<Vec<_>>(),
        actual.difference(&predicted).collect::<Vec<_>>()
    );
}

/// Guards the corpus size the bounds in `p1_the_decomposition_is_compressive`
/// are derived from. If the corpus grows, those bounds were computed against a
/// different denominator and must be re-derived deliberately — not silently
/// inherited.
#[test]
fn the_corpus_is_the_74_names_the_bounds_assume() {
    assert_eq!(
        assignment().len(),
        74,
        "the compression bounds derive from a 74-name corpus (21 formations, \
         53 variants); this assignment covers a different number"
    );
}
