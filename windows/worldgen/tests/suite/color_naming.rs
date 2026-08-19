//! The Pigment's two preregistered claims, plus the naming function's own
//! properties.
//!
//! **These two tests are the campaign's deliverable.** A reviewer must
//! break each one deliberately and report the measured values — a claim
//! that cannot fail is a decoration, not a finding.

use hornvale_astronomy::illuminant::{at_elevation, daylight};
use hornvale_kernel::color::{Illuminant, Reflectance, standard_observer};
use hornvale_language::PackDepths;
use hornvale_worldgen::color_naming::name_color;

/// An ochre outcrop: iron-rich, so it reflects long and absorbs short.
fn ochre() -> Reflectance {
    Reflectance::new([0.04, 0.05, 0.06, 0.08, 0.14, 0.24, 0.44, 0.55, 0.58, 0.60]).unwrap()
}

/// CLAIM 1 — the same outcrop, the same light, two species, two words.
///
/// The difference must come from `pack_depths`, not from a branch on
/// species. Breaking `pack_depths` (e.g. returning a constant) must turn
/// this red.
#[test]
fn the_same_outcrop_is_named_differently_by_two_species() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();

    // Roster values, from `pack_depths`'s own model card: a goblin's
    // night_vision 0.5 gives hue 4 (blue, no brown); a kobold's 0.9 gives
    // hue 2 (dark, light, red only).
    //
    // Derived through the real roster and the real `pack_depths` rather
    // than written down as literals — the plan's snippet hardcoded
    // `PackDepths { hue: 4, .. }`, which would have let this claim pass
    // unchanged with `pack_depths` flattened to a constant. The claim is
    // that the *ladder* separates the species, so the ladder has to be on
    // the path.
    let perception = hornvale_species::perception_registry();
    let goblin = hornvale_worldgen::pack_depths(
        perception
            .get(&hornvale_kernel::KindId("goblin"))
            .expect("the roster has a goblin"),
    );
    let kobold = hornvale_worldgen::pack_depths(
        perception
            .get(&hornvale_kernel::KindId("kobold"))
            .expect("the roster has a kobold"),
    );

    let by_goblin = name_color(&ochre(), &light, &eye, &goblin);
    let by_kobold = name_color(&ochre(), &light, &eye, &kobold);

    assert_ne!(
        by_goblin, by_kobold,
        "both species named the outcrop '{by_goblin}' — the ladder did nothing"
    );
    // The kobold's word must be one it actually holds.
    assert!(
        ["dark", "light", "red"].contains(&by_kobold),
        "a kobold reached for '{by_kobold}', which is past its ladder depth"
    );
}

/// CLAIM 2 — **FALSIFIED.** The preregistered prediction was that the same
/// outcrop would be named differently at noon and at dusk. It is not. This
/// test pins the measured reality rather than the prediction.
///
/// Ochre is named `brown` at 85° and `brown` at 2°. The illuminant moves
/// everything — the chromaticity distances shift three- to sixfold between
/// the two elevations, and `red` and `yellow` even swap rank — but ochre
/// sits deep inside brown's basin and dusk pushes it *further in* rather
/// than out:
///
/// | | noon 85° | dusk 2° |
/// |---|---|---|
/// | → neutral | 0.180507 | 0.072151 |
/// | → brown | 0.021370 | **0.003337** |
/// | → red | 0.088854 | 0.054907 |
/// | → yellow | 0.066711 | 0.062417 |
///
/// **The mechanism the claim predicted is real; this sample simply does not
/// exhibit it.** A post-hoc sweep of 273 reflectances found 125 (46%) that
/// do change name between these two elevations, across 16 distinct
/// transitions (`blue→green`, `yellow→dark`, `brown→red`, …). That sweep is
/// exploratory and is *not* a confirmed claim — it was run after unblinding
/// and is reported as such. The confirmed, preregistered result is this
/// null. See `a_surface_that_does_move_between_noon_and_dusk` for a pinned
/// instance of the effect.
///
/// Nothing was retuned to rescue the prediction: not an exemplar, not a
/// scattering constant, not either naming threshold (decision 0016).
#[test]
fn the_outcrop_keeps_its_name_between_noon_and_dusk() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let base = daylight(&star);
    let eye = standard_observer();
    let speaker = PackDepths {
        hue: 5,
        luminance: 3,
    };

    let noon = name_color(&ochre(), &at_elevation(&base, 85.0), &eye, &speaker);
    let dusk = name_color(&ochre(), &at_elevation(&base, 2.0), &eye, &speaker);

    assert_eq!(noon, "brown", "the measured noon name moved");
    assert_eq!(dusk, "brown", "the measured dusk name moved");
}

/// The effect claim 2 predicted, on a surface that exhibits it.
///
/// **Post-hoc and labelled as such** — this sample was chosen after
/// unblinding, so it confirms nothing on its own. It exists so the 46%
/// sweep result is represented by a reproducible instance rather than a
/// number in a doc comment, and so a regression that flattened the
/// illuminant's effect entirely would be caught.
///
/// The `yellow` exemplar itself, sensed as a surface, crosses the
/// achromatic threshold as the sun drops: its chromaticity distance from
/// neutral falls 0.139107 → 0.011493, passing below `ACHROMATIC_THRESHOLD`
/// (0.04), so it stops being nameable as a hue at all and becomes `light`.
#[test]
fn a_surface_that_does_move_between_noon_and_dusk() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let base = daylight(&star);
    let eye = standard_observer();
    let speaker = PackDepths {
        hue: 5,
        luminance: 3,
    };
    let sample = hornvale_language::hue_exemplar("yellow").expect("yellow is a hue term");

    let noon = name_color(&sample, &at_elevation(&base, 85.0), &eye, &speaker);
    let dusk = name_color(&sample, &at_elevation(&base, 2.0), &eye, &speaker);

    assert_eq!(noon, "yellow");
    assert_eq!(dusk, "light");
    assert_ne!(noon, dusk, "the illuminant moved no name at all");
}

/// CLAIM 2b — **ILL-POSED, not null.** Retired, and replaced by the
/// property that made it ill-posed.
///
/// 2b was preregistered after Task 4 measured that `at_elevation` dims
/// about eightfold as well as reddening. The worry was that claim 2 could
/// be satisfied by the trivial "everything is dark at dusk", so 2b
/// renormalized both illuminants to equal peak radiance to isolate the hue
/// half.
///
/// **It could never have discriminated anything.** Naming compares the
/// sample against exemplars *sensed under the same light*, and both of its
/// axes self-calibrate: chromaticity normalizes to unit sum, and the
/// `dark`/`light` split is the midpoint between those two exemplars under
/// the current light. So scaling an illuminant by any `k > 0` scales every
/// signal by `k`, leaving chromaticity untouched and turning the luminance
/// test into `k·L < k·M ⟺ L < M`. Peak-normalization *is* such a scaling.
/// 2b and claim 2 are therefore the same experiment, always — which the
/// sweep confirmed independently: 125 of 273 surfaces moved under claim 2's
/// conditions and *the same* 125 under 2b's.
///
/// The confound 2b guarded against is not merely unobserved, it is
/// impossible in this design. The guard was unnecessary; it was added
/// between Task 4 and Task 6, before Task 6's two-axis correction existed
/// to rule the confound out.
///
/// What is left is worth keeping: this is a **colour-constancy** guarantee,
/// and it constrains what comes next. Campaign 2 cannot model a nocturnal
/// observer as "the same eye under dimmer light" — that would produce
/// byte-identical names. Night vision has to be a change in channel
/// *sensitivity*, not gain, and this test is the tripwire that says so.
#[test]
fn naming_is_invariant_under_uniform_illuminant_rescale() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let base = daylight(&star);
    let eye = standard_observer();
    let speaker = PackDepths {
        hue: 5,
        luminance: 3,
    };

    // Every surface that has a name, at two elevations, under the raw
    // illuminant and under its peak-normalized rescale.
    for elevation in [85.0, 30.0, 2.0] {
        let light = at_elevation(&base, elevation);
        let rescaled = peak_normalized(&light);
        for concept in hornvale_language::HUE_CONCEPTS {
            let sample = hornvale_language::hue_exemplar(concept).expect("hue term");
            assert_eq!(
                name_color(&sample, &light, &eye, &speaker),
                name_color(&sample, &rescaled, &eye, &speaker),
                "'{concept}' at {elevation}° named differently under a uniform \
                 rescale — colour constancy is broken, and campaign 2's \
                 night-vision model can no longer rely on it"
            );
        }
    }
}

#[test]
fn a_speaker_never_reaches_past_its_ladder_depth() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    // Depth 1: only dark and light are lexicalized.
    let shallow = PackDepths {
        hue: 1,
        luminance: 1,
    };
    for sample in [ochre(), Reflectance::new([0.9; 10]).unwrap()] {
        let word = name_color(&sample, &light, &eye, &shallow);
        assert!(
            ["dark", "light"].contains(&word),
            "reached for '{word}' at depth 1"
        );
    }
}

#[test]
fn naming_is_deterministic_across_repeated_calls() {
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    let depths = PackDepths {
        hue: 5,
        luminance: 3,
    };
    let a = name_color(&ochre(), &light, &eye, &depths);
    let b = name_color(&ochre(), &light, &eye, &depths);
    assert_eq!(a, b);
}

#[test]
fn every_hue_term_is_reachable_by_some_sample() {
    // Memory `modelled-authored-unreachable`: this repo repeatedly ships
    // types that are defined, prose-authored, and cannot occur. A term no
    // sample can ever elicit is exactly that.
    let star = hornvale_astronomy::star::generate_star(test_astronomy_seed());
    let light = daylight(&star);
    let eye = standard_observer();
    let deep = PackDepths {
        hue: 5,
        luminance: 3,
    };
    let mut seen: Vec<&str> = Vec::new();
    for concept in hornvale_language::HUE_CONCEPTS {
        let exemplar = hornvale_language::hue_exemplar(concept).unwrap();
        let word = name_color(&exemplar, &light, &eye, &deep);
        if !seen.contains(&word) {
            seen.push(word);
        }
    }
    assert_eq!(
        seen.len(),
        hornvale_language::HUE_CONCEPTS.len(),
        "only {} of {} hue terms were reachable: {seen:?}",
        seen.len(),
        hornvale_language::HUE_CONCEPTS.len()
    );
}

/// Renormalize an illuminant to peak 1.0, so two illuminants differ in
/// spectral SHAPE only and carry identical peak radiance.
fn peak_normalized(light: &Illuminant) -> Illuminant {
    let mut bands = *light.get();
    let peak = bands.iter().copied().fold(0.0f64, f64::max);
    assert!(
        peak > 0.0,
        "an illuminant with no peak cannot be normalized"
    );
    for b in bands.iter_mut() {
        *b /= peak;
    }
    Illuminant::new(bands).expect("rescaling a valid illuminant leaves it valid")
}

/// The astronomy seed, built the way `domains/astronomy/src/wanderers.rs:155`
/// already builds it in its own tests.
///
/// Note `Seed(42)`, a tuple constructor — **not** `Seed::new(42)`, which
/// does not exist. That exact mistake has ridden into three tasks in this
/// repo before (memory: `plan-authored-test-snippets-are-uncompiled`).
fn test_astronomy_seed() -> hornvale_kernel::Seed {
    hornvale_kernel::Seed(42).derive(hornvale_astronomy::streams::ROOT)
}
