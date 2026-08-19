//! Held-out arm 2 (spec §6.5): the keystone — *"the same space at every grain
//! and in every realm"* — measured rather than asserted.
//!
//! # This arm is WEAKER than the spec intended, and the reason is recorded here
//!
//! The spec called for the axes to be fitted on land and the marine names then
//! assigned **without revising the axis list**. That is not what happened: the
//! implementer authored all 74 names in one pass, so if the sea had demanded a
//! sixth axis it might have been added without anyone noticing the hold-out was
//! being violated. The blinding was lost before the arm ran.
//!
//! What is salvageable is not the blinding but the **containment question**,
//! which is a property of the committed data and is indifferent to what the
//! author intended: *did the sea use any axis, or any axis VALUE, that no land
//! name uses?* If it did, the space was stretched to accommodate the sea — which
//! is the failure the arm exists to detect — whether or not the stretch was
//! deliberate.
//!
//! Checking values and not merely axis ids is the part that matters. Comparing
//! ids alone would have been satisfied by construction, because the assignment
//! module writes all five occupied axes for nearly every name; it would have
//! passed while proving nothing.
//!
//! **Stop rule: HALT.** Unlike arm 1 (the variants), a failure here is not
//! "refine and carry forward" — it falsifies the program's keystone.

use hornvale_climate::axes::assignment;
use hornvale_kernel::environment_v1_basis;
use std::collections::{BTreeMap, BTreeSet};

/// Every name belonging to the water realm: the six marine formations and the
/// variants whose pools hang off them.
const MARINE: &[&str] = &[
    "sea-ice",
    "reef",
    "kelp-forest",
    "vent",
    "upwelling",
    "open-water",
    "pressure-ridge",
    "ice-lead",
    "rafted-floe",
    "melt-pond",
    "coral-head",
    "spur-and-groove",
    "reef-rubble",
    "staghorn-stand",
    "kelp-canopy",
    "holdfast-tangle",
    "urchin-barren",
    "smoker-field",
    "tubeworm-thicket",
    "vent-plume",
    "plankton-bloom",
    "cold-upwelling",
    "bait-ball",
    "open-blue",
    "sargassum-drift",
    "fish-shoal",
    "twilight-water",
    "scattering-layer",
    "lightless-water",
    "marine-snow",
    "abyssal-plain",
    "nodule-field",
    "trench-wall",
    "trench-floor",
];

/// The three cave formations — the underworld realm, excluded from both sides.
/// Campaign 2 is what will populate it; it is not part of this arm.
const CAVE: &[&str] = &["karst-cave", "lava-tube", "fracture-cave"];

/// Axis id to the set of values used, over the names matching `pick`.
fn values_used(pick: impl Fn(&str) -> bool) -> BTreeMap<u16, BTreeSet<u64>> {
    let mut out: BTreeMap<u16, BTreeSet<u64>> = BTreeMap::new();
    for entry in assignment().iter().filter(|e| pick(e.name)) {
        for axis in environment_v1_basis() {
            if let Some(value) = entry.vector.get(*axis) {
                out.entry(axis.id).or_default().insert(value.to_bits());
            }
        }
    }
    out
}

fn label(id: u16) -> &'static str {
    environment_v1_basis()
        .iter()
        .find(|a| a.id == id)
        .map(|a| a.label)
        .unwrap_or("<unknown axis>")
}

#[test]
fn the_sea_needs_no_axis_the_land_does_not_use() {
    let land = values_used(|n| !MARINE.contains(&n) && !CAVE.contains(&n));
    let sea = values_used(|n| MARINE.contains(&n));

    let land_axes: BTreeSet<u16> = land.keys().copied().collect();
    let sea_axes: BTreeSet<u16> = sea.keys().copied().collect();
    let novel: Vec<&str> = sea_axes
        .difference(&land_axes)
        .map(|id| label(*id))
        .collect();

    assert!(
        novel.is_empty(),
        "KEYSTONE FALSIFIED: the sea occupies axes the land does not: {novel:?}"
    );
}

#[test]
fn the_sea_needs_no_axis_value_the_land_does_not_use() {
    let land = values_used(|n| !MARINE.contains(&n) && !CAVE.contains(&n));
    let sea = values_used(|n| MARINE.contains(&n));

    let mut stretched: Vec<String> = Vec::new();
    for (id, sea_values) in &sea {
        let empty = BTreeSet::new();
        let land_values = land.get(id).unwrap_or(&empty);
        for bits in sea_values.difference(land_values) {
            stretched.push(format!("{}={}", label(*id), f64::from_bits(*bits)));
        }
    }
    stretched.sort();

    assert!(
        stretched.is_empty(),
        "KEYSTONE AT RISK: the sea uses axis values no land name uses, so the \
         space was stretched to accommodate it: {stretched:?}"
    );
}

/// Guards the arm against becoming vacuous. If the marine roster stopped
/// matching the assignment's spellings, both tests above would compare an empty
/// set against a full one and pass perfectly.
#[test]
fn the_marine_roster_actually_matches_assigned_names() {
    let assigned: BTreeSet<&str> = assignment().iter().map(|e| e.name).collect();
    let missing: Vec<&&str> = MARINE.iter().filter(|n| !assigned.contains(**n)).collect();
    assert!(
        missing.is_empty(),
        "VACUOUS: these marine names are not in the assignment at all: {missing:?}"
    );
    let assigned_marine = assignment()
        .iter()
        .filter(|e| MARINE.contains(&e.name) && !e.vector.is_unassigned())
        .count();
    assert_eq!(
        assigned_marine, 28,
        "expected 28 assigned marine names: 34 in the roster less SIX resisters \
         — the four sea-ice phases (pressure-ridge, ice-lead, rafted-floe, \
         melt-pond) plus reef-rubble and urchin-barren, which are post-event \
         succession in the sea exactly as burn and forest-gap are on land"
    );
}
