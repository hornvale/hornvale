//! **The coarse value is a boundary condition, not a suggestion.**
//!
//! That is the metaplan's keystone corollary, adopted verbatim from The Rill,
//! and nothing in this campaign tested it until now. It is testable here because
//! the corpus has two grains — formations and the variants beneath them — and
//! the assignment records which genus each variant belongs to.
//!
//! # What counts as a contradiction, and why valence decides it
//!
//! A variant may **refine** its genus on a `Scalar` or `Ordinal` axis: a damper
//! hollow inside a temperate forest is a legitimate finer reading of the same
//! quantity. It may not **contradict** its genus on a `Nominal` axis, where the
//! value names an unordered class rather than a magnitude — a forest on soil
//! does not contain a variant on ice; that is a different kind of place, not a
//! finer view of the same one.
//!
//! This is the first thing in the campaign that gives `AxisValence` work to do.
//! Without it there would be no principled line between refining and
//! contradicting, and the check would either forbid all variation or permit all
//! of it.
//!
//! # Two genera that disagree constrain nothing
//!
//! Several variants have two parents, straight out of `variant_pool`'s match
//! arms: `Tundra | Alpine`, `TemperateForest | TemperateRainforest`,
//! `Savanna | TemperateGrassland`, `TropicalRainforest |
//! TropicalSeasonalForest`. Those pairs do not always agree — tundra is organic
//! ground and alpine is rock — so a variant under both is bound by neither on
//! that axis. A boundary condition only binds where the boundary is single-
//! valued, which is the genus-declines rule one level up.

use hornvale_climate::axes::assignment;
use hornvale_kernel::{AxisValence, environment_v1_basis};

/// The value all of `genera` agree on for `axis`, if there is one. `None` when
/// any genus declines the axis, or when two genera disagree.
fn agreed(genera: &[&str], axis: hornvale_kernel::EnvironmentAxis) -> Option<f64> {
    let mut agreed: Option<f64> = None;
    for genus in genera {
        let entry = assignment().iter().find(|e| e.name == *genus)?;
        let value = entry.vector.get(axis)?;
        match agreed {
            None => agreed = Some(value),
            Some(seen) if seen.to_bits() == value.to_bits() => {}
            Some(_) => return None, // genera disagree: nothing is constrained
        }
    }
    agreed
}

#[test]
fn a_variant_never_contradicts_its_genus_on_a_nominal_axis() {
    let mut violations: Vec<String> = Vec::new();
    for entry in assignment().iter().filter(|e| !e.genera.is_empty()) {
        for axis in environment_v1_basis() {
            if axis.valence != AxisValence::Nominal {
                continue; // refining a magnitude is legitimate
            }
            let (Some(bound), Some(mine)) = (agreed(entry.genera, *axis), entry.vector.get(*axis))
            else {
                continue;
            };
            if bound.to_bits() != mine.to_bits() {
                violations.push(format!(
                    "{} has {}={} under genera {:?} which agree on {}",
                    entry.name, axis.label, mine, entry.genera, bound
                ));
            }
        }
    }
    violations.sort();
    assert!(
        violations.is_empty(),
        "the coarse value is a boundary condition, not a suggestion — {} \
         variant(s) contradict their genus on an unordered axis:\n  {}",
        violations.len(),
        violations.join("\n  ")
    );
}

/// Guards the check against becoming vacuous: every genus a variant names must
/// actually be in the assignment, or `agreed` silently returns `None` and the
/// test passes by finding nothing to check.
#[test]
fn every_named_genus_resolves_and_the_check_has_work_to_do() {
    let names: Vec<&str> = assignment().iter().map(|e| e.name).collect();
    let mut unknown: Vec<&str> = Vec::new();
    for entry in assignment() {
        for genus in entry.genera {
            if !names.contains(genus) && !unknown.contains(genus) {
                unknown.push(genus);
            }
        }
    }
    assert!(
        unknown.is_empty(),
        "VACUOUS: variants name genera that are not assigned: {unknown:?}"
    );

    let constrained = assignment()
        .iter()
        .filter(|e| !e.genera.is_empty())
        .filter(|e| {
            environment_v1_basis()
                .iter()
                .any(|a| a.valence == AxisValence::Nominal && agreed(e.genera, *a).is_some())
        })
        .count();
    assert!(
        constrained > 20,
        "VACUOUS: only {constrained} variants are actually bound on a nominal \
         axis, so the check above is nearly empty"
    );
}
