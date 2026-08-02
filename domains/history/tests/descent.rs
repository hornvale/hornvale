//! The pure descent arithmetic: turning a founding-gap into a number of
//! generations, and walking a lazy chain of implied ancestors.

use hornvale_history::descent::{Kinship, ancestor, kinship, remove};
use hornvale_history::flesh::RoleHandle;
use hornvale_kernel::Seed;

#[test]
fn a_gap_shorter_than_half_a_generation_is_the_same_generation() {
    // Seed 42 measured 13% of founded-from edges at a zero remove: a
    // daughter community founded within a generation of its mother, whose
    // founder is therefore a SIBLING, not a descendant.
    assert_eq!(remove(0.0, 21.7), 0);
    assert_eq!(remove(10.0, 21.7), 0);
}

#[test]
fn a_gap_of_one_generation_is_one_remove() {
    assert_eq!(remove(21.7, 21.7), 1);
    assert_eq!(remove(25.0, 21.7), 1);
}

#[test]
fn the_measured_median_gap_resolves_to_the_measured_median_remove() {
    // Spec 1.1: median gap 50 y; goblin generation length 21.7 y; median
    // remove 2. This pins the plan's headline arithmetic to a real number.
    assert_eq!(remove(50.0, 21.7), 2);
    // 50/21.7 = 2.30 floors to 2 under both round-to-nearest and plain
    // floor, so the assertion above alone does not pin the rounding rule.
    // 33/21.7 = 1.52: rounds to 2 but floors to 1, and does distinguish.
    assert_eq!(remove(33.0, 21.7), 2);
}

#[test]
fn the_measured_maximum_gap_stays_bounded() {
    // Spec 1.1: max gap 975 y, max remove 32. 975 / 30.9 (hobgoblin) = 31.6.
    assert_eq!(remove(975.0, 30.9), 32);
}

#[test]
fn kinship_reports_sibling_at_zero_and_ancestor_above() {
    assert_eq!(kinship(0.0, 21.7), Kinship::Sibling);
    assert_eq!(kinship(50.0, 21.7), Kinship::Ancestor(2));
}

#[test]
fn a_nonpositive_generation_length_yields_zero_rather_than_infinity() {
    // An Ametabolic kind has generation_length None; a caller that
    // substitutes 0.0 must not produce NaN, Infinity, or a panic.
    assert_eq!(remove(50.0, 0.0), 0);
    assert_eq!(remove(50.0, -3.0), 0);
}

#[test]
fn a_negative_gap_yields_zero_rather_than_underflowing_the_u32() {
    // The bake never emits a daughter founded before its mother (measured
    // 0/1759 on seed 42), but `remove` is pub and must be total.
    assert_eq!(remove(-100.0, 21.7), 0);
}

#[test]
fn ancestor_is_deterministic_and_walks_away_from_its_start() {
    let seed = Seed(7);
    let h = RoleHandle(1234);
    assert_eq!(ancestor(h, 3, seed), ancestor(h, 3, seed));
    assert_ne!(ancestor(h, 1, seed), ancestor(h, 2, seed));
    assert_ne!(ancestor(h, 1, seed), h);
}

#[test]
fn ancestor_of_zero_steps_is_the_figure_themself() {
    let seed = Seed(7);
    let h = RoleHandle(1234);
    assert_eq!(ancestor(h, 0, seed), h);
}

#[test]
fn the_deepest_measured_chain_walks_without_collision() {
    // Spec 1.1: max remove 32. Every ancestor along the deepest real chain
    // must be distinct, or two forebears would share a name.
    let seed = Seed(42);
    let h = RoleHandle(99);
    let chain: Vec<RoleHandle> = (0..=32).map(|k| ancestor(h, k, seed)).collect();
    let mut sorted: Vec<u64> = chain.iter().map(|r| r.0).collect();
    sorted.sort_unstable();
    sorted.dedup();
    assert_eq!(sorted.len(), 33, "ancestor walk collided within 32 steps");
}
