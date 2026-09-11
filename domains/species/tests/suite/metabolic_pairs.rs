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
use std::collections::BTreeSet;

/// Every pair a kind is allowed to carry. Rung 1 of the Underworld Larder
/// declared four; rung 2 adds `(Absent, Chemotrophic)`, witnessed by `xorn` —
/// the visible moment the underworld gains a productive base.
const SANCTIONED: &[(T, M)] = &[
    (T::Endothermic, M::Heterotrophic),
    (T::Ectothermic, M::Heterotrophic),
    // THE TIDEMARK: the vent commensal. A body that eats a chemical gradient
    // and does not pay to hold its own temperature is a coherent pair, and it
    // is the only new one this campaign needs — `xorn` reached chemotrophy
    // from `Absent` (ametabolic, eats mineral), which is a different claim
    // about a different kind of thing. Deliberately not widened further: the
    // other combinations `Chemotrophic` admits are still unmeaningful.
    (T::Ectothermic, M::Chemotrophic),
    (T::Unmodelled, M::Phototrophic),
    (T::Absent, M::Absent),
    (T::Absent, M::Chemotrophic),
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

/// `Chemotrophic` is WITNESSED, by exactly two kinds since THE TIDEMARK.
///
/// **THE DIRECTION THIS ENFORCES, STATED** (the discipline `is_ametabolic`'s
/// doc uses): before rung 2, this test asserted the variant was carried by
/// NO kind ("declared, not witnessed") — its own message said "if rung 2 has
/// landed, this assertion is what you came to delete." Rung 2 has landed:
/// `xorn` burrows through stone and eats only mineral, which is a
/// chemolithotroph, so `Absent`/`Absent` — the honest encoding available
/// before this variant existed — became `Absent`/`Chemotrophic`. The
/// assertion inverts rather than deletes, because the axis still needs a
/// witness-count guard going forward: exactly one kind today, and a second
/// carrier appearing (or `xorn` losing the variant) is exactly as much a
/// finding as zero carriers was before rung 2.
/// **THE TIDEMARK ADDS THE SECOND CARRIER, WHICH IS THE EVENT THE ASSERTION
/// BELOW PREDICTED.** The message this test carried said a second carrier
/// "is exactly as much a finding as zero carriers was before rung 2", and
/// asked for a deliberate change with a reason rather than a silent
/// widening. The reason: `vent-commensal` is the marine half of the
/// Underworld Larder's rung 4 (spec §7). `xorn` eats a chemical gradient in
/// rock and reads its supply from `chemosynthate_per_rung`; this kind eats
/// one in water and reads `MarineHabitat::chemosynthate`, whose seabed term
/// is a live vent's chemistry and therefore a function of vent PHASE. Two
/// carriers, two realms, one axis — and the underworld's own consumer-side
/// half of rung 4 is still open and still belongs to THE TENANT.
#[test]
fn chemotrophic_is_witnessed_by_the_two_realms_that_have_a_gradient() {
    let carriers: Vec<&str> = biosphere_registry()
        .iter()
        .filter(|(_, b)| b.trophic_mode == M::Chemotrophic)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        carriers,
        vec!["vent-commensal", "xorn"],
        "TrophicMode::Chemotrophic is carried by {carriers:?}; expected \
         exactly [\"vent-commensal\", \"xorn\"]. Rung 2 of the Underworld \
         Larder witnessed the variant through xorn alone — a thing that \
         burrows through stone and eats only mineral is a chemolithotroph — \
         and The Tidemark adds the marine consumer at a hydrothermal vent. A \
         THIRD kind appearing, or either of these two losing the variant, is \
         a deliberate change to say why; it is not something to silently \
         widen this assertion for."
    );
    assert!(
        SANCTIONED.iter().any(|(_, m)| *m == M::Chemotrophic),
        "no kind carries TrophicMode::Chemotrophic yet SANCTIONED admits a \
         Chemotrophic pair — the declaration and the roster have drifted \
         apart"
    );
}

/// **THE DIRECT PER-KIND PIN THAT REPLACED THE PAIRWISE-DISTINCTNESS
/// PROPERTY.**
///
/// Under its original name, this test asserted a STRUCTURAL property of
/// `SANCTIONED`: that no two rows shared a thermal value, which is what let
/// `every_kind_carries_a_sanctioned_pair` pin the trophic axis merely by
/// virtue of pinning the thermal one (a kind's thermal strategy determined at
/// most one admissible row, so it determined the row's trophic mode too).
/// Rung 2 of the Underworld Larder broke that property ON PURPOSE: adding
/// `(Absent, Chemotrophic)` beside the already-sanctioned `(Absent, Absent)`
/// duplicates the `Absent` thermal key, exactly as this test's old failure
/// message predicted it would. With that key duplicated, a kind carrying
/// `ThermalStrategy::Absent` could swap between `TrophicMode::Absent` and
/// `TrophicMode::Chemotrophic` and NOTHING else would notice:
/// `every_kind_carries_a_sanctioned_pair` only checks membership in
/// `SANCTIONED`, `tests/suite/coverage.rs`'s
/// `metabolic_class_coverage_matches_the_table` pins `ThermalStrategy` alone
/// (it has zero occurrences of `TrophicMode` in its own table), and the
/// life-history golden is driven by `is_ametabolic`, which also reads the
/// thermal axis only. That was demonstrated by mutation before this
/// replacement existed: adding `(Unmodelled, Chemotrophic)` and flipping
/// `treant` to `Chemotrophic` was caught only by
/// `chemotrophic_is_declared_and_unwitnessed` — the one test that predicted
/// rung 2's landing at all.
///
/// **THE DIRECTION THIS NOW ENFORCES, STATED** (the discipline
/// `is_ametabolic`'s doc uses): every kind's `trophic_mode` must equal
/// exactly what `PINNED` names for it — not merely "some sanctioned pair" —
/// because with the thermal key no longer unique, membership in `SANCTIONED`
/// alone under-constrains the trophic axis. This table is EXHAUSTIVE over
/// every kind in the registry, checked in both directions (a kind present in
/// the registry but missing from `PINNED`, or named in `PINNED` but absent
/// from the registry, fails the set-equality assertion below), so a kind
/// added later cannot slip past this guard unnamed. As of rung 2, this is the
/// only guard on the trophic axis anywhere in the workspace suite.
#[test]
fn sanctioned_thermal_keys_are_pairwise_distinct() {
    const PINNED: &[(&str, M)] = &[
        ("goblin", M::Heterotrophic),
        ("kobold", M::Heterotrophic),
        ("hobgoblin", M::Heterotrophic),
        ("bugbear", M::Heterotrophic),
        ("treant", M::Phototrophic),
        ("twig-blight", M::Phototrophic),
        ("giant-elk", M::Heterotrophic),
        ("woolly-mammoth", M::Heterotrophic),
        ("giant-goat", M::Heterotrophic),
        ("otyugh", M::Heterotrophic),
        ("xorn", M::Chemotrophic),
        // THE TIDEMARK (Task 3): the six obligate marine peoples.
        ("abyssal-elf", M::Heterotrophic),
        ("kelp-tender", M::Phototrophic),
        ("merfolk", M::Heterotrophic),
        ("reef-mason", M::Heterotrophic),
        ("triton", M::Heterotrophic),
        ("vent-commensal", M::Chemotrophic),
        ("rust-monster", M::Heterotrophic),
        ("white-dragon", M::Heterotrophic),
        ("red-dragon", M::Heterotrophic),
        ("black-dragon", M::Heterotrophic),
        ("owlbear", M::Heterotrophic),
        ("giant-scorpion", M::Heterotrophic),
        ("giant-hyena", M::Heterotrophic),
        ("dire-wolf", M::Heterotrophic),
        ("rhinoceros", M::Heterotrophic),
        ("giant-constrictor-snake", M::Heterotrophic),
        ("carrion-crawler", M::Heterotrophic),
        ("shrieker", M::Phototrophic),
        ("reef-shark", M::Heterotrophic),
        ("giant-octopus", M::Heterotrophic),
        ("killer-whale", M::Heterotrophic),
        ("giant-squid", M::Heterotrophic),
        ("giant-crocodile", M::Heterotrophic),
        ("gnoll", M::Heterotrophic),
        ("human", M::Heterotrophic),
        ("desert-dwarf", M::Heterotrophic),
        ("gully-dwarf", M::Heterotrophic),
        ("hill-dwarf", M::Heterotrophic),
        ("desert-elf", M::Heterotrophic),
        ("drow", M::Heterotrophic),
        ("high-elf", M::Heterotrophic),
        ("sea-elf", M::Heterotrophic),
        ("snow-elf", M::Heterotrophic),
        ("wood-elf", M::Heterotrophic),
    ];

    let registry = biosphere_registry();

    let pinned_names: BTreeSet<&str> = PINNED.iter().map(|(name, _)| *name).collect();
    assert_eq!(
        pinned_names.len(),
        PINNED.len(),
        "PINNED lists {} entries but only {} distinct kind names — a kind is \
         named twice in this table",
        PINNED.len(),
        pinned_names.len()
    );

    let registry_names: BTreeSet<&str> = registry.iter().map(|(k, _)| k.0).collect();
    assert_eq!(
        pinned_names, registry_names,
        "PINNED and the registry name different kinds (see the symmetric \
         difference above). This table must name every kind in the registry \
         exactly once — a kind present in one but not the other can carry an \
         unnoticed trophic_mode, which is precisely the gap this test \
         replaced the pairwise-distinctness property to close."
    );

    for (name, expected) in PINNED {
        let bio = registry
            .get_by_label(name)
            .unwrap_or_else(|| panic!("PINNED names \"{name}\", which the set check above should have caught as absent from the registry"));
        assert_eq!(
            bio.trophic_mode, *expected,
            "\"{name}\" carries {:?}, but PINNED pins {expected:?}. If this \
             kind's trophic_mode genuinely changed, update PINNED \
             deliberately and say why in the commit — this table is the only \
             guard on the trophic axis anywhere in the suite \
             (`tests/suite/coverage.rs` pins ThermalStrategy only).",
            bio.trophic_mode
        );
    }

    assert_eq!(
        SANCTIONED.len(),
        6,
        "rung 1 declared four sanctioned pairs, rung 2 added \
         (Absent, Chemotrophic), and The Tidemark adds \
         (Ectothermic, Chemotrophic) for the vent commensal; if this count \
         moved again, re-read this test's doc before adjusting the number"
    );
}
