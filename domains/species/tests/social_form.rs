//! The Eremite (Dragons program C2): SocialForm is a universal biosphere axis.
//! The determinism keystone lives here — {Settled} must equal the psyche
//! key-set, so every gate re-keyed off psyche onto Settled selects the same set.

use hornvale_kernel::KindId;
use hornvale_species::{SocialForm, biosphere_registry, psyche_registry};

fn social_form_of(name: &'static str) -> SocialForm {
    biosphere_registry()
        .get(&KindId(name))
        .unwrap_or_else(|| panic!("{name} has a biosphere row"))
        .social_form
}

#[test]
fn every_kind_has_the_authored_social_form() {
    let expected: &[(&str, SocialForm)] = &[
        ("goblin", SocialForm::Settled),
        ("kobold", SocialForm::Settled),
        ("hobgoblin", SocialForm::Settled),
        ("bugbear", SocialForm::Settled),
        ("treant", SocialForm::Sessile),
        ("twig-blight", SocialForm::Sessile),
        ("giant-elk", SocialForm::Gregarious),
        ("woolly-mammoth", SocialForm::Gregarious),
        ("giant-goat", SocialForm::Gregarious),
        ("otyugh", SocialForm::Solitary),
        ("xorn", SocialForm::Solitary),
        ("rust-monster", SocialForm::Solitary),
        ("owlbear", SocialForm::Solitary),
        ("white-dragon", SocialForm::Solitary),
        ("red-dragon", SocialForm::Solitary),
        ("black-dragon", SocialForm::Solitary),
    ];
    for (name, sf) in expected {
        assert_eq!(social_form_of(name), *sf, "{name}");
    }
}

#[test]
fn settled_kinds_are_exactly_the_four_peoples() {
    // The byte-identity keystone: the settlement roster (and every gate re-keyed
    // off the retired "has a psyche" proxy onto `Settled`) is exactly the four
    // peoples — the same set the pre-Eremite psyche key-set held. After The
    // Eremite the dragons carry a mind while staying Solitary, so psyche is a
    // SUPERSET of Settled (Settled ⊆ psyche), not equal — hence a named pin.
    let bio = biosphere_registry();
    let psy = psyche_registry();
    let settled: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        settled,
        ["bugbear", "goblin", "hobgoblin", "kobold"],
        "Settled is exactly the four peoples (ascending KindId)"
    );
    for &name in &settled {
        assert!(
            psy.contains(&KindId(name)),
            "{name} settles, so it carries a mind (Settled ⊆ psyche)"
        );
    }
}

#[test]
fn the_dragons_have_a_solitary_mind() {
    use hornvale_species::{Sociality, StatusBasis, psyche_registry};
    let psy = psyche_registry();
    for name in ["white-dragon", "red-dragon", "black-dragon"] {
        let m = psy.get(&KindId(name)).expect("dragon mind");
        assert_eq!(m.threat_response, 0.95, "{name} stands, never flees");
        assert_eq!(m.in_group_radius, 0.05, "{name} is utterly insular");
        assert_eq!(m.time_horizon, 0.90, "{name} is a patient hoarder");
        assert_eq!(m.sociality, Sociality::Hierarchic);
        assert_eq!(m.status_basis, StatusBasis::Rank);
    }
    // A dragon is Solitary — it carries a mind but must NOT be Settled, so it
    // never enters settlement genesis (Task 3's re-key).
    for name in ["white-dragon", "red-dragon", "black-dragon"] {
        assert_ne!(
            social_form_of(name),
            SocialForm::Settled,
            "{name} is Solitary"
        );
    }
}
