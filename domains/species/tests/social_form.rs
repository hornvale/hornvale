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
fn settled_set_equals_the_psyche_key_set() {
    // The byte-identity keystone: re-keying any peoplehood gate from
    // "has a psyche" onto "is Settled" is a no-op iff these two sets are equal.
    let bio = biosphere_registry();
    let psy = psyche_registry();
    let settled: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| k.0)
        .collect();
    let peopled: Vec<&str> = psy.ids().map(|k| k.0).collect();
    assert_eq!(
        settled, peopled,
        "{{Settled}} must equal the psyche key-set"
    );
}
