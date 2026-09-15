//! Which (energy source, electron donor, carbon source) triples the corpus
//! may contain.
//!
//! THE GOSSAN split one four-variant enum (`MetabolicClass`) into two
//! four-variant ones (`ThermalStrategy`, `TrophicMode`). THE TRENCHER
//! (decision 0976, design authority `BIO-trophic-trichotomy`) factors
//! `TrophicMode` again, into the three axes microbiology actually uses —
//! energy source × electron donor × carbon source — so the type now admits
//! **eight** triples where three are meaningful. This is the declared list,
//! and adding to it is a deliberate edit, the same ratchet shape as
//! `tropes check` and the type-audit waivers.
//!
//! **It is also the READER that keeps the metabolic triple honest** (spec
//! §4.5, carried forward from THE GOSSAN). Nothing else in this campaign
//! consumes all three axes together, and an axis nobody reads is exactly how
//! `MetabolicClass` rotted: its own doc records `Autotroph` being "witnessed
//! by The Menagerie without the modelling decision ever being made", and
//! still called itself an "unused seam" three campaigns later.
//!
//! **`ThermalStrategy` does not appear here, and that is new.** Under
//! `TrophicMode`, `SANCTIONED` paired `(ThermalStrategy, TrophicMode)`
//! because `TrophicMode::Absent` needed pairing with
//! `ThermalStrategy::Absent` to close the ghost/construct/undead corner.
//! Decision 0976 removed `Absent` from the trophic side entirely (see
//! `EnergySource`'s doc in `hornvale_species`): ontological absence now
//! lives solely on the thermal axis, and a kind carrying
//! `ThermalStrategy::Absent` simply carries no metabolic triple at all. So
//! the two axes are no longer coupled, and this table needs no thermal
//! column to be exhaustive.

use hornvale_species::{
    CarbonSource as C, ElectronDonor as D, EnergySource as E, biosphere_registry,
};
use std::collections::BTreeSet;

/// Every (energy source, electron donor, carbon source) triple a kind is
/// allowed to carry.
///
/// - **Chemoorganoheterotrophy** — an ordinary animal: chemical energy from
///   oxidizing organic matter, an organic donor, organic carbon. Every
///   kind whose old `TrophicMode` was `Heterotrophic`.
/// - **Photolithoautotrophy** — a plant-folk/fungal analogue: light energy,
///   an inorganic donor, fixed (inorganic) carbon. Every kind whose old
///   `TrophicMode` was `Phototrophic`.
/// - **Chemolithoautotrophy** — `xorn` and, since THE TIDEMARK,
///   `vent-commensal`: chemical energy from a mineral/redox gradient, an
///   inorganic donor, fixed carbon. Rung 2 of the Underworld Larder
///   witnessed this as `(Absent, Chemotrophic)` under the old two-axis
///   scheme; decision 0976 moved `xorn`'s thermal strategy to `Unmodelled`
///   (a separate, thermal-axis-only fact) without touching this triple at
///   all. THE TIDEMARK's `vent-commensal` (a body that eats a chemical
///   gradient at a hydrothermal vent and does not pay to hold its own
///   temperature — `ThermalStrategy::Ectothermic`) carries the SAME triple
///   as `xorn`, so no new entry is needed here; only the per-kind witness
///   count below moves.
const SANCTIONED: &[(E, D, C)] = &[
    (E::Chemotrophic, D::Organotrophic, C::Heterotrophic),
    (E::Phototrophic, D::Lithotrophic, C::Autotrophic),
    (E::Chemotrophic, D::Lithotrophic, C::Autotrophic),
];

#[test]
fn every_kind_carries_a_sanctioned_combination() {
    let bio = biosphere_registry();
    let mut offenders = Vec::new();
    let mut checked = 0;
    for (id, traits) in bio.iter() {
        let combo = (
            traits.energy_source,
            traits.electron_donor,
            traits.carbon_source,
        );
        if !SANCTIONED.contains(&combo) {
            offenders.push(format!("  {id:?}: {combo:?}"));
        }
        checked += 1;
    }
    assert!(
        offenders.is_empty(),
        "kinds carrying an unsanctioned metabolic combination:\n{}\n\n\
         Adding a combination to SANCTIONED is a deliberate act — it asserts \
         the combination is biologically meaningful, not merely typeable.",
        offenders.join("\n")
    );
    assert!(
        checked >= 20,
        "only {checked} kinds checked — the registry is not being read and this \
         guard is vacuous"
    );
}

/// The chemolithoautotroph triple is now WITNESSED, by exactly two kinds
/// since THE TIDEMARK (one before it).
///
/// **THE DIRECTION THIS ENFORCES, STATED** (the discipline `is_ametabolic`'s
/// doc uses): before rung 2 of the Underworld Larder, the trophic axis
/// asserted no kind carried a chemotrophic value at all ("declared, not
/// witnessed"). Rung 2 landed: `xorn` burrows through stone and eats only
/// mineral, which is a chemolithotroph — chemical energy, an inorganic
/// donor, fixed carbon. The assertion inverts rather than deletes, because
/// the triple still needs a witness-count guard going forward: exactly one
/// kind then, and a second carrier appearing (or `xorn` losing the triple)
/// is exactly as much a finding as zero carriers was before rung 2.
///
/// **This is still true history, and it is about the METABOLIC triple, not
/// the thermal axis.** `xorn`'s thermal strategy moved separately, later:
/// decision 0976 took it off `ThermalStrategy::Absent` (it is alive, so
/// pairing an ametabolic thermal value with a live metabolic triple was
/// never a stable resting point — a living kind with no metabolism at all is
/// the category error). `xorn` carries `ThermalStrategy::Unmodelled` and the
/// chemolithoautotroph triple today; only the thermal half changed.
///
/// **THE TIDEMARK ADDS THE SECOND CARRIER, WHICH IS THE EVENT THE ASSERTION
/// ABOVE PREDICTED.** The message this test carried said a second carrier
/// "is exactly as much a finding as zero carriers was before rung 2", and
/// asked for a deliberate change with a reason rather than a silent
/// widening. The reason: `vent-commensal` is the marine half of the
/// Underworld Larder's rung 4 (spec §7). `xorn` eats a chemical gradient in
/// rock and reads its supply from `chemical_per_rung`
/// (`energy::chemical_supply_field_per_rung`, The Trencher Task 4's
/// per-metabolite reading); this kind eats one in water and reads
/// `MarineHabitat::chemosynthate`, whose seabed term is a live vent's
/// chemistry and therefore a function of vent PHASE. Two carriers, two
/// realms, one triple — and the underworld's own consumer-side half of
/// rung 4 is still open and still belongs to THE TENANT.
#[test]
fn chemolithoautotrophy_is_witnessed_by_xorn_and_vent_commensal() {
    let carriers: Vec<&str> = biosphere_registry()
        .iter()
        .filter(|(_, b)| {
            (b.energy_source, b.electron_donor, b.carbon_source)
                == (E::Chemotrophic, D::Lithotrophic, C::Autotrophic)
        })
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        carriers,
        vec!["vent-commensal", "xorn"],
        "the chemolithoautotroph triple is carried by {carriers:?}; expected \
         exactly [\"vent-commensal\", \"xorn\"]. Rung 2 of the Underworld \
         Larder witnessed it through xorn alone — a thing that burrows \
         through stone and eats only mineral is a chemolithotroph — and The \
         Tidemark adds the marine consumer at a hydrothermal vent. A THIRD \
         kind appearing, or either of these two losing the triple, is a \
         deliberate change to say why; it is not something to silently widen \
         this assertion for."
    );
    assert!(
        SANCTIONED.contains(&(E::Chemotrophic, D::Lithotrophic, C::Autotrophic)),
        "no kind carries the chemolithoautotroph triple yet SANCTIONED admits \
         it — the declaration and the roster have drifted apart"
    );
}

/// **THE DIRECT PER-KIND PIN.**
///
/// Membership in `SANCTIONED` alone under-constrains any one kind: a kind
/// could swap between two sanctioned triples (say, from
/// chemoorganoheterotroph to chemolithoautotroph) and nothing would notice,
/// because both remain valid combinations for *some* kind in the registry.
/// This table is EXHAUSTIVE over every kind in the registry, checked in both
/// directions (a kind present in the registry but missing from `PINNED`, or
/// named in `PINNED` but absent from the registry, fails the set-equality
/// assertion below), so a kind added or changed later cannot slip past this
/// guard unnamed. As of this split, this is the only guard on the metabolic
/// triple anywhere in the workspace suite.
///
/// **THE COUNT ASSERTION IS THE GUARD THAT MATTERS MOST, CARRIED FORWARD IN
/// ITS THREE-AXIS FORM.** Under the old two-axis scheme, adding
/// `(Unmodelled, Chemotrophic)` to `SANCTIONED` and flipping `treant` to
/// carry it was caught only by the chemotroph witness-count test above —
/// nothing else in the suite pinned the trophic axis at all. `SANCTIONED`'s
/// own length assertion below is the same shape of guard, one level up: a
/// combination added to `SANCTIONED` without a genuine new kind to justify
/// it is a silent widening, and the count is what catches it even if no
/// per-kind pin happens to be watching that particular combination.
#[test]
fn every_kind_is_pinned_to_its_metabolic_triple() {
    const PINNED: &[(&str, E, D, C)] = &[
        (
            "goblin",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "kobold",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "hobgoblin",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "bugbear",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        ("treant", E::Phototrophic, D::Lithotrophic, C::Autotrophic),
        (
            "twig-blight",
            E::Phototrophic,
            D::Lithotrophic,
            C::Autotrophic,
        ),
        (
            "giant-elk",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "woolly-mammoth",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-goat",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "otyugh",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        ("xorn", E::Chemotrophic, D::Lithotrophic, C::Autotrophic),
        // THE TIDEMARK (Task 3): the six obligate marine peoples. Triples
        // assigned by the same TrophicMode->triple mapping this migration
        // applied everywhere else (see the `BiosphereTraits` literals in
        // `hornvale_species::biosphere_registry`): an ordinary heterotroph
        // maps to (Chemotrophic, Organotrophic, Heterotrophic); kelp-tender's
        // phototrophy to (Phototrophic, Lithotrophic, Autotrophic);
        // vent-commensal's chemolithoautotrophy to the SAME triple xorn
        // carries (Chemotrophic, Lithotrophic, Autotrophic) -- no new
        // SANCTIONED entry needed.
        (
            "abyssal-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "kelp-tender",
            E::Phototrophic,
            D::Lithotrophic,
            C::Autotrophic,
        ),
        (
            "merfolk",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "reef-mason",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "triton",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "vent-commensal",
            E::Chemotrophic,
            D::Lithotrophic,
            C::Autotrophic,
        ),
        (
            "rust-monster",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "white-dragon",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "red-dragon",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "black-dragon",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "owlbear",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-scorpion",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-hyena",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "dire-wolf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "rhinoceros",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-constrictor-snake",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "carrion-crawler",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        ("shrieker", E::Phototrophic, D::Lithotrophic, C::Autotrophic),
        (
            "reef-shark",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-octopus",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "killer-whale",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-squid",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "giant-crocodile",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        ("gnoll", E::Chemotrophic, D::Organotrophic, C::Heterotrophic),
        ("human", E::Chemotrophic, D::Organotrophic, C::Heterotrophic),
        (
            "kuo-toa",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "mountain-dwarf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "desert-dwarf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "gully-dwarf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "hill-dwarf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "desert-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        ("drow", E::Chemotrophic, D::Organotrophic, C::Heterotrophic),
        (
            "duergar",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "high-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "sea-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "snow-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "svirfneblin",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
        (
            "wood-elf",
            E::Chemotrophic,
            D::Organotrophic,
            C::Heterotrophic,
        ),
    ];

    let registry = biosphere_registry();

    let pinned_names: BTreeSet<&str> = PINNED.iter().map(|(name, ..)| *name).collect();
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
         unnoticed metabolic triple, which is precisely the gap this test \
         exists to close."
    );

    for (name, energy, donor, carbon) in PINNED {
        let bio = registry
            .get_by_label(name)
            .unwrap_or_else(|| panic!("PINNED names \"{name}\", which the set check above should have caught as absent from the registry"));
        let actual = (bio.energy_source, bio.electron_donor, bio.carbon_source);
        let expected = (*energy, *donor, *carbon);
        assert_eq!(
            actual, expected,
            "\"{name}\" carries {actual:?}, but PINNED pins {expected:?}. If \
             this kind's metabolic triple genuinely changed, update PINNED \
             deliberately and say why in the commit — this table is the only \
             guard on the triple anywhere in the suite.",
        );
    }

    assert_eq!(
        SANCTIONED.len(),
        3,
        "the split (THE TRENCHER) declared three sanctioned triples — \
         chemoorganoheterotrophy, photolithoautotrophy, and xorn's \
         chemolithoautotrophy; if this count moved, re-read this test's doc \
         before adjusting the number. THE TIDEMARK's vent-commensal (a \
         second chemolithoautotroph carrier) needed no fourth entry here — \
         it carries the same triple xorn does, so only the per-kind PINNED \
         table and the witness-count test above moved."
    );
}
