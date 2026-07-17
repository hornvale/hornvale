//! The Individuation's composed demonstrations: a kind reached BY TRANSITION
//! (owlbear -> awakened-owlbear, the UNI-7/BIO-11 arc), with per-instance
//! overrides surviving the change — over a from_stores test roster, never
//! canonical (spec §4.4: genesis/placement must not see these kinds).

use hornvale_kernel::{Seed, Value, World};
use hornvale_lab::roster::awakened_owlbear_components;
use hornvale_species::{SPECIES_MASS_KG, instance_biosphere};

fn world() -> World {
    let mut w = World::new(Seed(11));
    hornvale_species::register_concepts(&mut w.registry).unwrap();
    w
}

#[test]
fn the_roster_passes_integrity_and_stays_out_of_the_canon() {
    let wc = awakened_owlbear_components();
    // from_stores ran check_integrity: the awakened kind carries the FULL
    // peopled cluster (it speaks).
    assert!(wc.psyche.get_by_label("awakened-owlbear").is_some());
    assert!(wc.biosphere.get_by_label("owlbear").is_some());
    // The canonical world knows neither the awakened kind…
    let canon = hornvale_worldgen::WorldComponents::assemble().unwrap();
    assert!(canon.biosphere.get_by_label("awakened-owlbear").is_none());
    // …and the awakened kind is mighty (potency > 0) while the beast is not.
    let beast = wc.biosphere.get_by_label("owlbear").unwrap();
    let awakened = wc.biosphere.get_by_label("awakened-owlbear").unwrap();
    assert_eq!(beast.potency, 0.0);
    assert!(awakened.potency > 0.0);
}

#[test]
fn awakening_is_a_fact_and_the_large_owlbear_stays_large() {
    let wc = awakened_owlbear_components();
    let mut w = world();
    let e =
        hornvale_worldgen::mint_instance_of_kind(&mut w, &wc, "owlbear", Some(0.0), "lab").unwrap();
    // An unusually large individual: a per-instance override.
    w.ledger
        .commit(
            hornvale_kernel::Fact {
                subject: e,
                predicate: SPECIES_MASS_KG.to_string(),
                object: Value::Number(900.0),
                place: None,
                day: Some(0.0),
                provenance: "lab".to_string(),
            },
            &w.registry,
        )
        .unwrap();
    // The awakening: a day-stamped kind-change fact, never a struct edit.
    w.ledger
        .change_kind(
            e,
            "awakened-owlbear",
            Some(40.0),
            "lab: the awakening",
            &w.registry,
        )
        .unwrap();
    assert_eq!(w.ledger.kind_of(e), Some("awakened-owlbear"));
    let eff = instance_biosphere(&w.ledger, e, &wc.biosphere).unwrap();
    // Defaults now come from the NEW kind (mighty)…
    assert!(eff.potency > 0.0);
    // …and the instance override survives the transition (spec §4.3).
    assert_eq!(eff.mass.kilograms(), 900.0);
}
