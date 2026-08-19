//! The delve ladder's roster is mirrored in two crates. This is the guard that
//! makes the mirror a rule rather than a hope (decision 0094).
//!
//! `hornvale_terrain::delve::DelveRung` owns the ladder and its derivation —
//! which ΔT lands in which rung. `hornvale_climate::underworld::DelveZone`
//! carries the *same roster* so that the underworld corpus can say at what
//! depth class a community occurs, without `hornvale-climate` importing
//! `hornvale-terrain` (which the layering forbids outright).
//!
//! Decision 0094 splits such a duplicate in two: **the roster is shared, the
//! derivation is not.** Nothing here calls a terrain function to compute
//! anything; it compares the two lists of rung names and their order. That is
//! the whole point — a shared derivation would make the mirror an echo, and an
//! unguarded roster falls behind silently, which 0094 records happening twice
//! in eleven days elsewhere in this repo.
//!
//! This test lives in `cli/` because `cli/` is the only layer that sees both
//! crates. It is the same placement reason the workspace's other cross-crate
//! enforcement tests have.

use hornvale_climate::underworld::DelveZone;
use hornvale_terrain::delve::{DelveRung, rungs};

/// The rung names climate mirrors, shallow to deep. Written out rather than
/// derived from either side, so that a rename on EITHER side reddens this
/// test instead of the two sides silently agreeing on a new name.
const HABITATION_RUNGS: [&str; 5] = ["Undercroft", "Shallows", "Deeps", "Underdeep", "Sunless"];

fn debug_names<T: std::fmt::Debug>(items: &[T]) -> Vec<String> {
    items.iter().map(|i| format!("{i:?}")).collect()
}

#[test]
fn terrain_owns_exactly_these_habitation_rungs_plus_surface() {
    let all = debug_names(rungs());
    assert_eq!(
        all.first().map(String::as_str),
        Some("Surface"),
        "the ladder leads with Surface (spec §4.6: the overworld is a rung of \
         the same ladder)"
    );
    assert_eq!(
        &all[1..],
        &HABITATION_RUNGS[..],
        "terrain's delve ladder moved. If that is deliberate, move \
         hornvale_climate::underworld::DelveZone in the same commit and update \
         this roster — the underworld corpus's depth readings are stated \
         against these names."
    );
}

#[test]
fn climate_mirrors_the_habitation_rungs_and_deliberately_omits_surface() {
    // SORTED BY `Ord`, which is derived from declaration order. Comparing a
    // hand-written array in the order this test wants would be vacuous against
    // exactly the mutation that matters — swapping two variants in the enum —
    // and it was, until the mutation was run and only `both_rosters_order_
    // shallow_to_deep` fired. Sorting first is what makes this test see it.
    let mut zones = [
        DelveZone::Sunless,
        DelveZone::Deeps,
        DelveZone::Undercroft,
        DelveZone::Underdeep,
        DelveZone::Shallows,
    ];
    zones.sort_unstable();
    assert_eq!(
        debug_names(&zones),
        HABITATION_RUNGS.to_vec(),
        "climate's DelveZone no longer spells the roster it mirrors, in the \
         shallow-to-deep order its `Ord` is supposed to carry"
    );

    // The one deliberate difference, asserted rather than assumed: no
    // underworld community is at the surface, so `DelveZone` has no `Surface`.
    // Stating it here is what keeps it a decision instead of an omission.
    assert!(
        !debug_names(&zones).iter().any(|n| n == "Surface"),
        "DelveZone gained a Surface variant; no underworld community can take \
         it, so a row could never be authored with it"
    );
    assert_eq!(
        rungs().len(),
        zones.len() + 1,
        "the two rosters differ by exactly one variant, and that variant is \
         Surface"
    );
}

/// The ordering is half the roster. A rung's *identity* is its name; its
/// *meaning* is its position, and the underworld corpus's energy-inversion
/// measurement is taken against that position.
#[test]
fn both_rosters_order_shallow_to_deep() {
    assert!(DelveRung::Surface < DelveRung::Undercroft);
    assert!(DelveRung::Undercroft < DelveRung::Shallows);
    assert!(DelveRung::Shallows < DelveRung::Deeps);
    assert!(DelveRung::Deeps < DelveRung::Underdeep);
    assert!(DelveRung::Underdeep < DelveRung::Sunless);

    assert!(DelveZone::Undercroft < DelveZone::Shallows);
    assert!(DelveZone::Shallows < DelveZone::Deeps);
    assert!(DelveZone::Deeps < DelveZone::Underdeep);
    assert!(DelveZone::Underdeep < DelveZone::Sunless);
}
