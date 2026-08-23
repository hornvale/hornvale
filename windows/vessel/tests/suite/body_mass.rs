//! One derivation of a body's mass, shared by every body.

use hornvale_vessel::clock::{REFERENCE_MASS_KG, mass_for_species};

#[test]
fn an_absent_biosphere_falls_back_to_the_reference_mass() {
    // liveness.rs states this fallback in a comment -- "a defaulted creature
    // reads at exactly tempo 1.0" -- and nothing asserted it. Now something
    // does.
    assert_eq!(mass_for_species("anything", None), REFERENCE_MASS_KG);
}

#[test]
fn an_unknown_species_falls_back_to_the_reference_mass() {
    assert_eq!(
        mass_for_species("no-such-species-exists", None),
        REFERENCE_MASS_KG
    );
}

/// The equivalence that makes this an EXTRACTION rather than a redefinition:
/// for every species the biosphere actually carries, `mass_for_species`
/// returns bit-for-bit what `liveness.rs`'s inline lookup returned. The
/// inline form is reproduced verbatim here as the oracle, so this compares two
/// implementations rather than one implementation with itself.
///
/// The species are drawn from the roster AT RUNTIME and never named: a
/// hardcoded species that later leaves the roster would silently turn this
/// into a vacuous pass against the fallback, which is indistinguishable from
/// success. Two guards close the vacuity the other way — the roster must be
/// non-empty, and at least one species must resolve to something OTHER than
/// `REFERENCE_MASS_KG`, or "both paths agree" would be satisfied by both
/// paths failing to find anything.
#[test]
fn the_inline_creature_derivation_and_mass_for_species_agree() {
    let biosphere = hornvale_species::biosphere_registry();
    assert!(
        !biosphere.is_empty(),
        "the authored biosphere roster is empty — every lookup below would \
         take the fallback and this test would pass vacuously"
    );

    let mut any_authored = false;
    for (kind, traits) in biosphere.iter() {
        let species = kind.0;

        // The derivation `liveness.rs` used to hold inline, twice, verbatim.
        let inline = biosphere
            .get_by_label(species)
            .map(|t| t.mass.kilograms())
            .unwrap_or(REFERENCE_MASS_KG);
        let shared = mass_for_species(species, Some(&biosphere));

        // Bits, not `==`: an extraction that moved a mass by one ULP would
        // change every action cost that body pays and therefore its whole
        // trajectory, and `==` on `f64` is the weaker instrument for saying so.
        assert_eq!(
            inline.to_bits(),
            shared.to_bits(),
            "{species}: inline derivation gives {inline}, mass_for_species gives {shared}"
        );
        assert_eq!(
            shared.to_bits(),
            traits.mass.kilograms().to_bits(),
            "{species}: the shared derivation is not returning the authored trait"
        );
        if shared != REFERENCE_MASS_KG {
            any_authored = true;
        }
    }

    assert!(
        any_authored,
        "every species on the roster resolved to REFERENCE_MASS_KG — the \
         agreement above is agreement on the fallback, not on authored data"
    );
}

/// The absent-biosphere arm agrees with the present-but-missing arm, which is
/// the property the possessed body will lean on in Arc I.b: a body whose
/// species the biosphere does not carry costs exactly what a body with no
/// biosphere at all costs.
#[test]
fn the_two_fallback_arms_are_the_same_value() {
    let biosphere = hornvale_species::biosphere_registry();
    let absent = mass_for_species("no-such-species-exists", None);
    let present_but_missing = mass_for_species("no-such-species-exists", Some(&biosphere));
    assert_eq!(absent.to_bits(), present_but_missing.to_bits());
    assert_eq!(absent, REFERENCE_MASS_KG);
}
