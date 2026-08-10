//! THE RADIATION (C2d): the six elves' biome-affinity rows, checked for the
//! four SILENT failure modes before anything measures them.
//!
//! `range_readout.rs::every_authored_affinity_row_is_well_formed` already
//! enforces non-uniformity, key spelling, the `> 0.0` floor, the `<= 1.0`
//! ceiling and no duplicate keys, over EVERY row in the registry — so the six
//! elf rows inherit all of it. This file adds only what is specific to the
//! family: that all six are present, that Sea is confined to the shelf, and
//! that High's row is Wood's.

use hornvale_kernel::KindId;

/// The family, in the order the registry lists it.
const ELVES: [&str; 6] = [
    "desert-elf",
    "drow",
    "high-elf",
    "sea-elf",
    "snow-elf",
    "wood-elf",
];

/// **Sea takes the PRODUCTIVE SHALLOW BAND, not the whole ocean.**
///
/// The campaign's second premise-check and its correction. "The ocean is 2.7x
/// the land" (29,896 cells against 11,066 on seed 42) is a real number with the
/// wrong denominator: no elf gets all the land either. On the shelf band Sea
/// gets **1,425 cells** as a three-seed mean (42/7/1234) — larger than Desert's
/// 241, smaller than Snow's 3,849 — and the roster comes out balanced with no
/// outlier. The runaway exists ONLY if Sea is authored to the whole ocean.
///
/// This test is the thing that keeps it from being. The deep classes must not
/// be lifted above the row's default; the four shelf classes must be, and
/// strictly.
#[test]
fn the_sea_elf_is_confined_to_the_shelf_band() {
    const SHELF: [&str; 4] = ["coral-reef", "kelp-forest", "upwelling", "epipelagic"];
    const DEEP: [&str; 5] = [
        "mesopelagic",
        "bathypelagic",
        "abyssal",
        "hadal-trench",
        "sea-ice",
    ];
    let registry = hornvale_species::biome_affinity_registry();
    let sea = registry
        .get(&KindId("sea-elf"))
        .expect("sea-elf carries a biome affinity row");

    for b in SHELF {
        assert!(
            sea.factor(b) > sea.default,
            "sea-elf's {b} factor ({}) is not above its default ({}); the \
             shelf band must be a STRONGHOLD, and a factor at the default is \
             indistinguishable from silence",
            sea.factor(b),
            sea.default
        );
    }
    for b in DEEP {
        assert!(
            sea.factor(b) <= sea.default,
            "sea-elf's {b} factor ({}) is above its default ({}). A SETTLED \
             people needs shallow productive water — which is also why human \
             settlement is coastal — and `marine_forage_supply_field` already \
             grades the water that way (Upwelling 1.0, reef/kelp 0.85, \
             epipelagic 0.45, mesopelagic 0.15, bathypelagic 0.05). The \
             affinity SHARPENS that ranking; it must not contradict it, and it \
             must not open 25,640 cells of deep ocean.",
            sea.factor(b),
            sea.default
        );
    }
}

/// High's row is Wood's row, exactly. §3.6's contrast — Wood vs High isolates
/// MIND — is only single-variable if their environment is identical, and the
/// affinity is the loudest environmental channel either of them has.
#[test]
fn high_elfs_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let high = registry.get(&KindId("high-elf")).expect("high-elf row");
    assert_eq!(
        wood, high,
        "high-elf's affinity differs from wood-elf's. High diverges in \
         psyche, society and language ONLY (spec §3.6, §4); an environmental \
         divergence makes P3 a two-variable comparison and High stops being a \
         control."
    );
}

/// Drow's row is Wood's row too — its authored separation is the realm gate
/// and nothing else, so that P4's mutation (remove the realm row, watch the
/// separation vanish) isolates the gate rather than the gate plus a biome
/// difference.
#[test]
fn drows_affinity_is_wood_elfs() {
    let registry = hornvale_species::biome_affinity_registry();
    let wood = registry.get(&KindId("wood-elf")).expect("wood-elf row");
    let drow = registry.get(&KindId("drow")).expect("drow row");
    assert_eq!(
        wood, drow,
        "drow's affinity differs from wood-elf's. Its only authored \
         separation from the surface elves is the REALM GATE (spec §3.5); a \
         second difference would make P4's mutation uninterpretable."
    );
}

/// All six carry a row. Spec §3.7's parenthetical says "five of six" and is
/// treated as an erratum: P3(a)'s primary arm needs High to carry Wood's row
/// (bit-identical fields are impossible otherwise) and P4's wording — the
/// separation vanishes "up to Drow's own biome and curve authoring" —
/// presupposes Drow has biome authoring.
#[test]
fn all_six_elves_carry_an_affinity_row() {
    let registry = hornvale_species::biome_affinity_registry();
    for name in ELVES {
        assert!(
            registry.contains(&KindId(name)),
            "{name} has no biome-affinity row; all six are authored on the \
             affinity route (spec §3.1)"
        );
    }
}
