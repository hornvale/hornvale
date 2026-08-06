//! The Deep Realm, Task 1, Step 5: `hornvale_terrain::CaveKind` and
//! `hornvale_climate::Formation`'s cave variants are a **deliberate
//! duplicate roster** (decision 0094) — climate may not import terrain, so
//! nothing keeps the two enums in correspondence except a reader. `cli/` is
//! the only crate that depends on both domains, so this is where the
//! duplicate is kept honest.
//!
//! [`cave_formation_for`] is exhaustive over `CaveKind` with **no wildcard**:
//! if terrain ever adds a fourth `CaveKind`, this crate fails to *compile*,
//! which is the enforcement — not a runtime assertion that could be skipped.

use hornvale_climate::Formation;
use hornvale_terrain::CaveKind;

/// The one-to-one mapping decision 0094 requires. Exhaustive over `CaveKind`
/// by design (see the module doc): a new terrain `CaveKind` variant reddens
/// this crate's build rather than silently leaving the underworld with no
/// rock for it.
fn cave_formation_for(kind: CaveKind) -> Formation {
    match kind {
        CaveKind::Karst => Formation::KarstCave,
        CaveKind::LavaTube => Formation::LavaTube,
        CaveKind::Fracture => Formation::FractureCave,
    }
}

#[test]
fn every_cave_kind_has_exactly_one_cave_formation() {
    let karst = cave_formation_for(CaveKind::Karst);
    let lava_tube = cave_formation_for(CaveKind::LavaTube);
    let fracture = cave_formation_for(CaveKind::Fracture);
    assert_ne!(
        karst, lava_tube,
        "distinct CaveKinds must map to distinct Formations"
    );
    assert_ne!(
        karst, fracture,
        "distinct CaveKinds must map to distinct Formations"
    );
    assert_ne!(
        lava_tube, fracture,
        "distinct CaveKinds must map to distinct Formations"
    );
}
