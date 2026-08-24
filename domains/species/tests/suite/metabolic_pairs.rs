//! Which (thermal, trophic) pairs the corpus may contain.
//!
//! THE GOSSAN split one four-variant enum into two four-variant ones, so the
//! type now admits **sixteen** pairs where four are meaningful — it can say
//! "no metabolism, eats other life" (spec §4.4). This is the declared list,
//! and adding to it is a deliberate edit, the same ratchet shape as
//! `tropes check` and the type-audit waivers.
//!
//! **It is also the READER that keeps `TrophicMode` honest** (spec §4.5).
//! Nothing else consumes that axis in this campaign, and an axis nobody reads
//! is exactly how `MetabolicClass` rotted: its own doc records `Autotroph`
//! being "witnessed by The Menagerie without the modelling decision ever being
//! made", and still called itself an "unused seam" three campaigns later.

use hornvale_species::{ThermalStrategy as T, TrophicMode as M, biosphere_registry};

/// Every pair a kind is allowed to carry. Rung 1 of the Underworld Larder
/// declares four; rung 2 adds a `Chemotrophic` row, and that edit is the
/// visible moment the underworld gains a productive base.
const SANCTIONED: &[(T, M)] = &[
    (T::Endothermic, M::Heterotrophic),
    (T::Ectothermic, M::Heterotrophic),
    (T::Unmodelled, M::Phototrophic),
    (T::Absent, M::Absent),
];

#[test]
fn every_kind_carries_a_sanctioned_pair() {
    let mut checked = 0;
    for (kind, bio) in biosphere_registry().iter() {
        let pair = (bio.thermal_strategy, bio.trophic_mode);
        assert!(
            SANCTIONED.contains(&pair),
            "{} carries {pair:?}, which is not a sanctioned pair. The split \
             admits 16 combinations and only these are meaningful; if this one \
             genuinely is, add it to SANCTIONED deliberately and say why.",
            kind.0
        );
        checked += 1;
    }
    assert!(
        checked >= 20,
        "only {checked} kinds checked — the registry is not being read and this \
         guard is vacuous"
    );
}

/// `Chemotrophic` is DECLARED and not WITNESSED — the variant exists, no kind
/// carries it. Rung 2's success condition is that this test has to change.
#[test]
fn chemotrophic_is_declared_and_unwitnessed() {
    let carriers: Vec<&str> = biosphere_registry()
        .iter()
        .filter(|(_, b)| b.trophic_mode == M::Chemotrophic)
        .map(|(k, _)| k.0)
        .collect();
    assert!(
        carriers.is_empty(),
        "{carriers:?} carry TrophicMode::Chemotrophic. THE GOSSAN ships that \
         variant declared-but-unwitnessed on purpose: authoring a chemotroph \
         needs an energy field to feed it, which is rung 2. If rung 2 has \
         landed, this assertion is what you came to delete."
    );
    assert!(
        !SANCTIONED.iter().any(|(_, m)| *m == M::Chemotrophic),
        "SANCTIONED already admits a Chemotrophic pair while no kind carries \
         one — the declaration and the roster have drifted apart"
    );
}
