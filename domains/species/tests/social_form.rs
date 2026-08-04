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
        // The Vacancy (T7): seven terrestrial fauna.
        ("giant-scorpion", SocialForm::Solitary),
        ("giant-hyena", SocialForm::Gregarious),
        ("dire-wolf", SocialForm::Gregarious),
        ("rhinoceros", SocialForm::Solitary),
        ("giant-constrictor-snake", SocialForm::Solitary),
        ("carrion-crawler", SocialForm::Solitary),
        ("shrieker", SocialForm::Sessile),
        // The Vacancy (T8): four marine kinds and the amphibious giant
        // crocodile.
        ("reef-shark", SocialForm::Solitary),
        ("giant-octopus", SocialForm::Solitary),
        ("killer-whale", SocialForm::Gregarious),
        ("giant-squid", SocialForm::Solitary),
        ("giant-crocodile", SocialForm::Solitary),
        // The Vacancy (T9): the fifth people.
        ("gnoll", SocialForm::Settled),
        // The Generalist (C2-0): the sixth people.
        ("human", SocialForm::Settled),
    ];
    for (name, sf) in expected {
        assert_eq!(social_form_of(name), *sf, "{name}");
    }
}

#[test]
fn settled_kinds_are_exactly_the_six_peoples() {
    // The byte-identity keystone: the settlement roster (and every gate re-keyed
    // off the retired "has a psyche" proxy onto `Settled`) is exactly the
    // settling peoples — the same set the pre-Eremite psyche key-set held for
    // the original four. The Vacancy T9 adds the gnoll, a fifth, and The
    // Generalist (C2-0) adds the human, a sixth. After The Eremite the
    // dragons carry a mind while staying Solitary, so psyche is a SUPERSET of
    // Settled (Settled ⊆ psyche), not equal — hence a named pin.
    let bio = biosphere_registry();
    let psy = psyche_registry();
    let settled: Vec<&str> = bio
        .iter()
        .filter(|(_, b)| b.social_form == SocialForm::Settled)
        .map(|(k, _)| k.0)
        .collect();
    assert_eq!(
        settled,
        ["bugbear", "gnoll", "goblin", "hobgoblin", "human", "kobold"],
        "Settled is exactly the six peoples (ascending KindId)"
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
    use hornvale_species::{psyche_registry, society_registry};
    let psy = psyche_registry();
    for name in ["white-dragon", "red-dragon", "black-dragon"] {
        let m = psy.get(&KindId(name)).expect("dragon mind");
        assert_eq!(m.threat_response, 0.95, "{name} stands, never flees");
        assert_eq!(m.time_horizon, 0.90, "{name} is a patient hoarder");
        // The Cloister: a dragon is minded but Solitary — it carries no
        // society vector at all (sociality/status_basis/in_group_radius
        // moved to `SocietyVector`, carried only by `Settled` kinds).
        assert!(
            society_registry().get(&KindId(name)).is_none(),
            "{name} carries no society vector"
        );
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
