//! Canonical exemplar reflectances for the colour lexicon's hue ladder.
//!
//! Naming a colour compares a sample against remembered examples *under the
//! light you share with them* — so an exemplar is a reflectance, not a
//! finished colour. That is what makes naming work for an observer with any
//! channel count without re-authoring anything: the exemplar goes through
//! the same illuminant and the same eye as the sample before either is
//! compared.
//!
//! Only the **hue** ladder gets exemplars. `color_pack`'s luminance ladder
//! (gloom, shadow, starlit) describes ambient darkness rather than a
//! surface, so it is selected by the illuminant's level, not by comparing a
//! reflectance.

use hornvale_kernel::color::Reflectance;

/// The hue-ladder concept ids from [`crate::packs::color_pack`], in ladder
/// order. The luminance ladder is deliberately absent — see the module doc.
/// `the_hue_concepts_are_exactly_the_color_packs_hue_ladder` pins this list
/// against the pack so the two cannot drift.
/// type-audit: bare-ok(identifier-text)
pub const HUE_CONCEPTS: [&str; 7] = ["dark", "light", "red", "green", "yellow", "blue", "brown"];

/// The canonical reflectance for a hue concept, or `None` for anything not
/// on the hue ladder.
///
/// Declared approximations, chosen so the *relations* between terms hold:
/// red reflects long and absorbs short, blue the reverse, brown is red at
/// lower luminance, and dark and light bracket everything. A disagreement
/// with these curves is a disagreement about those relations.
/// type-audit: bare-ok(identifier-text)
pub fn hue_exemplar(concept: &str) -> Option<Reflectance> {
    let bands: [f64; hornvale_kernel::color::BANDS] = match concept {
        "dark" => [0.04, 0.04, 0.05, 0.05, 0.05, 0.05, 0.06, 0.06, 0.06, 0.06],
        "light" => [0.80, 0.83, 0.85, 0.86, 0.86, 0.86, 0.86, 0.85, 0.85, 0.84],
        "red" => [0.05, 0.05, 0.05, 0.05, 0.06, 0.10, 0.45, 0.70, 0.75, 0.78],
        "green" => [0.05, 0.07, 0.10, 0.18, 0.45, 0.35, 0.12, 0.09, 0.10, 0.12],
        "yellow" => [0.05, 0.06, 0.08, 0.15, 0.55, 0.75, 0.80, 0.82, 0.83, 0.84],
        "blue" => [0.20, 0.45, 0.55, 0.45, 0.20, 0.08, 0.05, 0.05, 0.06, 0.08],
        "brown" => [0.03, 0.04, 0.05, 0.06, 0.09, 0.14, 0.24, 0.32, 0.35, 0.36],
        _ => return None,
    };
    Some(Reflectance::new(bands).expect("authored exemplar is within [0, 1]"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_hue_concept_has_an_exemplar() {
        for concept in HUE_CONCEPTS {
            assert!(
                hue_exemplar(concept).is_some(),
                "'{concept}' is on the hue ladder with no exemplar"
            );
        }
    }

    #[test]
    fn the_luminance_ladder_has_no_exemplars() {
        for concept in ["gloom", "shadow", "starlit"] {
            assert!(
                hue_exemplar(concept).is_none(),
                "'{concept}' describes ambient darkness, not a surface"
            );
        }
    }

    #[test]
    fn the_hue_concepts_are_exactly_the_color_packs_hue_ladder() {
        // The two lists must never drift: a hue term added to color_pack
        // without an exemplar would be unnameable, and would fail silently.
        let luminance = ["gloom", "shadow", "starlit"];
        let mut from_pack: Vec<&str> = crate::packs::color_pack()
            .iter()
            .map(|e| e.concept)
            .filter(|c| !luminance.contains(c))
            .collect();
        from_pack.sort_unstable();
        let mut declared: Vec<&str> = HUE_CONCEPTS.to_vec();
        declared.sort_unstable();
        assert_eq!(from_pack, declared);
    }

    #[test]
    fn red_leans_long_and_blue_leans_short() {
        let red = hue_exemplar("red").unwrap();
        let blue = hue_exemplar("blue").unwrap();
        assert!(
            red.get()[8] > red.get()[2],
            "red must reflect more long than short"
        );
        assert!(
            blue.get()[2] > blue.get()[8],
            "blue must reflect more short than long"
        );
    }

    #[test]
    fn dark_is_darker_than_light_in_every_band() {
        let dark = hue_exemplar("dark").unwrap();
        let light = hue_exemplar("light").unwrap();
        for (band, (d, l)) in dark.get().iter().zip(light.get().iter()).enumerate() {
            assert!(d < l, "band {band}");
        }
    }

    #[test]
    fn brown_is_a_darker_red() {
        // Brown is the last term on the ladder because it is the hardest to
        // separate: it is red at low luminance. The exemplars must encode
        // that relationship or the ladder's ordering is a fiction.
        let red = hue_exemplar("red").unwrap();
        let brown = hue_exemplar("brown").unwrap();
        let red_total: f64 = red.get().iter().sum::<f64>();
        let brown_total: f64 = brown.get().iter().sum::<f64>();
        assert!(
            brown_total < red_total,
            "brown must be darker than red overall"
        );
        assert!(
            brown.get()[8] > brown.get()[2],
            "brown must still lean long"
        );
        // The two assertions above are the plan's, and neither can fail for
        // any plausible authored brown — every brown is darker than red and
        // leans long. The claim that actually carries weight is about
        // *shape*: strip the luminance difference out by normalizing each
        // exemplar to unit total, and brown's remaining spectral shape must
        // sit closer to red's than to any other term's. That is falsifiable
        // — a brown drawn toward the middle bands would land nearer yellow —
        // and it is the property a nearest-exemplar namer depends on.
        let normalized = |r: &hornvale_kernel::color::Reflectance| -> Vec<f64> {
            let total: f64 = r.get().iter().sum::<f64>();
            r.get().iter().map(|b| b / total).collect()
        };
        let brown_shape = normalized(&brown);
        let distance_to = |concept: &str| -> f64 {
            let other = normalized(&hue_exemplar(concept).unwrap());
            let mut sum = 0.0;
            for (a, b) in brown_shape.iter().zip(other.iter()) {
                let d = a - b;
                sum += d * d;
            }
            sum
        };
        let to_red = distance_to("red");
        for concept in HUE_CONCEPTS {
            if concept == "brown" || concept == "red" {
                continue;
            }
            let to_other = distance_to(concept);
            assert!(
                to_red < to_other,
                "brown's shape is nearer '{concept}' ({to_other}) than red ({to_red}); \
                 brown is supposed to be red at low luminance"
            );
        }
    }
}
