//! The Assay (Dragons program, campaign 1): `potency` is the creature's 5E MM
//! adult Challenge Rating over 30 (`CR/30`), nonzero only for the supernatural
//! set. These tests pin the RULE's outcome — each derived value and the might
//! ranking — so a regression in the convention reddens rather than passing
//! silently (measure, don't narrate the mechanism).

use hornvale_kernel::KindId;
use hornvale_species::biosphere_registry;

/// The supernatural set and its 5E adult Challenge Rating — the authoring
/// source of truth. Every other roster kind is mundane and carries `0.0`.
const SUPERNATURAL_CR: &[(&str, f64)] = &[
    ("red-dragon", 17.0),
    ("black-dragon", 14.0),
    ("white-dragon", 13.0),
    ("treant", 9.0),
    ("xorn", 5.0),
];

/// Every roster kind that is NOT in the supernatural set: the four peoples and
/// the mundane fauna. All carry `potency == 0.0` regardless of their own CR,
/// because their formidability is physical (carried by `mass`).
const MUNDANE: &[&str] = &[
    "goblin",
    "kobold",
    "hobgoblin",
    "bugbear",
    "twig-blight",
    "giant-elk",
    "woolly-mammoth",
    "giant-goat",
    "otyugh",
    "rust-monster",
    "owlbear",
];

fn potency_of(name: &'static str) -> f64 {
    biosphere_registry()
        .get(&KindId(name))
        .unwrap_or_else(|| panic!("{name} has a biosphere row"))
        .potency
}

#[test]
fn potency_is_challenge_rating_over_thirty() {
    for (name, cr) in SUPERNATURAL_CR {
        assert_eq!(
            potency_of(name),
            cr / 30.0,
            "{name}: potency must equal CR/30 = {cr}/30"
        );
    }
}

#[test]
fn mundane_kinds_carry_no_potency() {
    for name in MUNDANE {
        assert_eq!(
            potency_of(name),
            0.0,
            "{name} is mundane: potency must be 0.0"
        );
    }
}

#[test]
fn the_might_order_is_red_black_white_treant_xorn() {
    let red = potency_of("red-dragon");
    let black = potency_of("black-dragon");
    let white = potency_of("white-dragon");
    let treant = potency_of("treant");
    let xorn = potency_of("xorn");
    assert!(
        red > black && black > white && white > treant && treant > xorn && xorn > 0.0,
        "might order red>black>white>treant>xorn>0: got {red} {black} {white} {treant} {xorn}"
    );
}
