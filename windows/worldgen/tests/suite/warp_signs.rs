//! The Warp, Task 1: the sign functions — one implementation each, read by
//! the prose (windows/vessel, windows/locale) and by the instrument
//! (windows/lab). Spec §4.4.

use hornvale_terrain::lithology::RockClass;
use hornvale_worldgen::{
    MICRO_WORD_THRESHOLD, STEEP_HI, STEEP_LO, Steepness, Wetness, rock_word, steepness_sign,
    steepness_word, wetness_sign,
};

const EVERY_ROCK: [RockClass; 19] = [
    RockClass::Granite,
    RockClass::Gabbro,
    RockClass::Basalt,
    RockClass::Andesite,
    RockClass::Rhyolite,
    RockClass::Sandstone,
    RockClass::Shale,
    RockClass::Conglomerate,
    RockClass::Evaporite,
    RockClass::Chert,
    RockClass::Ironstone,
    RockClass::ReefLimestone,
    RockClass::Coal,
    RockClass::Slate,
    RockClass::Schist,
    RockClass::Gneiss,
    RockClass::Marble,
    RockClass::Quartzite,
    RockClass::Alluvium,
];

#[test]
fn every_rock_class_has_a_distinct_nonempty_word() {
    let mut seen = std::collections::BTreeSet::new();
    for class in EVERY_ROCK {
        let w = rock_word(class);
        assert!(!w.is_empty(), "{class:?} renders an empty word");
        assert!(
            w.chars().next().is_some_and(|c| c.is_lowercase()),
            "{class:?}: {w:?} must be lowercase (it is spliced mid-sentence)"
        );
        assert!(
            seen.insert(w),
            "{class:?} shares its word {w:?} with another class — a sign that cannot separate two rocks is not a sign"
        );
    }
}

#[test]
// Both operands are compile-time constants, so clippy reads
// `STEEP_LO < STEEP_HI` as an assertion on a constant — that IS the point:
// this line exists to fail the moment either threshold drifts out of order,
// not to exercise runtime behavior (precedent: windows/vessel/src/purview.rs).
#[allow(clippy::assertions_on_constants)]
fn steepness_is_monotone_in_slope_and_cuts_at_the_shared_thresholds() {
    // tanh(x) = 0.25 at x ≈ 0.2554; 0.60 at x ≈ 0.6931. Slope units: metres per radian.
    let gorge = hornvale_terrain::GORGE_SLOPE;
    assert_eq!(steepness_sign(0.0), Steepness::Level);
    assert_eq!(steepness_sign(0.20 * gorge), Steepness::Level);
    assert_eq!(steepness_sign(0.30 * gorge), Steepness::Sloping);
    assert_eq!(steepness_sign(0.65 * gorge), Steepness::Sloping);
    assert_eq!(steepness_sign(0.75 * gorge), Steepness::Steep);
    assert_eq!(
        steepness_sign(-0.75 * gorge),
        Steepness::Steep,
        "a downhill slope is as steep as an uphill one"
    );
    assert!(STEEP_LO < STEEP_HI);
    let mut last = Steepness::Level;
    for i in 0..400 {
        let s = steepness_sign(i as f64 * 0.01 * gorge);
        assert!(s >= last, "steepness went backwards at step {i}");
        last = s;
    }
}

#[test]
fn wetness_sign_cuts_at_the_micro_word_threshold() {
    assert_eq!(
        MICRO_WORD_THRESHOLD, 0.33,
        "grammar.rs's four axes cut here today; change both or neither"
    );
    assert_eq!(wetness_sign(0.34), Wetness::Damp);
    assert_eq!(
        wetness_sign(0.33),
        Wetness::Mid,
        "the cut is strict, matching grammar.rs's `> 0.33`"
    );
    assert_eq!(wetness_sign(0.0), Wetness::Mid);
    assert_eq!(wetness_sign(-0.33), Wetness::Mid);
    assert_eq!(wetness_sign(-0.34), Wetness::Dry);
}

#[test]
fn steepness_words_are_distinct_and_lowercase() {
    let words = [Steepness::Level, Steepness::Sloping, Steepness::Steep].map(steepness_word);
    assert!(
        words
            .iter()
            .all(|w| !w.is_empty() && w.chars().next().is_some_and(|c| c.is_lowercase()))
    );
    assert_ne!(words[0], words[1]);
    assert_ne!(words[1], words[2]);
    assert_ne!(words[0], words[2]);
}
