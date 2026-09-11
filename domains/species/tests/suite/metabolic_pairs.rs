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
/// declared four; rung 2 added `(Absent, Chemotrophic)`, witnessed by `xorn`
/// — the visible moment the underworld gains a productive base. Decision
/// 0976 ("Ametabolic life is a category error", The Trencher) then closed
/// that pair: a living kind always has a metabolism, so `xorn` moved to
/// `Unmodelled` and this table sanctions `(Unmodelled, Chemotrophic)` in its
/// place. `(Absent, Absent)` is unaffected — it stays the reserved
/// ghost/construct/undead corner, not a creature's pair.
const SANCTIONED: &[(T, M)] = &[
    (T::Endothermic, M::Heterotrophic),
    (T::Ectothermic, M::Heterotrophic),
    (T::Unmodelled, M::Phototrophic),
    (T::Absent, M::Absent),
    (T::Unmodelled, M::Chemotrophic),
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

/// `Chemotrophic` is now WITNESSED, by exactly one kind.
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
///
/// **This is still true history, and it is about the TROPHIC half.**
/// `xorn`'s thermal half moved separately, later: decision 0976 ("Ametabolic
/// life is a category error", The Trencher) took `xorn`'s thermal strategy
/// off `Absent` (it is alive, so `Absent`/`Chemotrophic` was never a stable
/// resting point — a living kind with no metabolism at all is the category
/// error). `xorn` carries `Unmodelled`/`Chemotrophic` today; only the pair's
/// first element changed, and this test's own claim (exactly one
/// `Chemotrophic` carrier, named `xorn`) is unaffected.
#[test]
fn chemotrophic_is_declared_and_unwitnessed() {
    let carriers: Vec<&str> = biosphere_registry()
        .iter()
        .filter(|(_, b)| b.trophic_mode == M::Chemotrophic)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        carriers,
        vec!["xorn"],
        "TrophicMode::Chemotrophic is carried by {carriers:?}; expected \
         exactly [\"xorn\"]. Rung 2 of the Underworld Larder witnesses the \
         variant through xorn alone — a thing that burrows through stone and \
         eats only mineral is a chemolithotroph. If a second kind now carries \
         it, or xorn no longer does, that is a deliberate change to say why; \
         it is not something to silently widen this assertion for."
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
        5,
        "rung 1 declared four sanctioned pairs and rung 2 added a fifth \
         (originally (Absent, Chemotrophic), replaced by decision 0976 with \
         (Unmodelled, Chemotrophic)); if this count moved again, re-read \
         this test's doc before adjusting the number"
    );
}
