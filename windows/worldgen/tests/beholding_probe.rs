//! The observer a species' perception implies — calibration tests promoted
//! from the spec-time probe (spec §3, "The measurement that reshaped this
//! spec"). `observer_for`/`observer_named`/`observer_roster` are the real
//! implementation now; the probe's `candidate_observer` helper and its
//! copied curves are superseded and removed.

use hornvale_kernel::color::{BANDS, Illuminant, Reflectance};
use hornvale_language::exemplars::{HUE_CONCEPTS, hue_exemplar};
use hornvale_species::perception_registry;
use hornvale_worldgen::observer::{observer_for, observer_named, observer_roster};

fn flat_light() -> Illuminant {
    Illuminant::new([1.0; BANDS]).unwrap()
}

/// Every hue exemplar's screen triple under one observer.
fn swatches(p: &hornvale_species::PerceptionVector) -> Vec<[u8; 3]> {
    let obs = observer_for(p);
    let light = flat_light();
    HUE_CONCEPTS
        .iter()
        .map(|c| {
            let r = hue_exemplar(c).expect("every hue concept has an exemplar");
            obs.to_srgb(&obs.sense(&r, &light))
                .expect("a derived observer always declares a projection")
        })
        .collect()
}

/// H2 — the human row is not privileged.
#[test]
fn the_human_row_derives_exactly_the_standard_observer() {
    let reg = perception_registry();
    let human = reg
        .get(&hornvale_species::KindId("human"))
        .expect("human is a speaking kind and must carry a perception row");
    let derived = observer_for(human);
    let standard = hornvale_kernel::color::standard_observer();
    let light = flat_light();
    for step in 0..=10 {
        let v = step as f64 / 10.0;
        let r = Reflectance::new([v; BANDS]).unwrap();
        assert_eq!(
            derived.to_srgb(&derived.sense(&r, &light)),
            standard.to_srgb(&standard.sense(&r, &light)),
            "the standard observer is a DERIVED row, not a privileged base \
             case; reflectance {v} disagreed"
        );
    }
    assert_eq!(derived.channels(), standard.channels());
    assert_eq!(derived.roles(), standard.roles());
}

/// H1 — the model resolves the axis it reads.
#[test]
fn species_with_distinct_night_vision_see_distinctly() {
    let reg = perception_registry();
    let mut rows: Vec<(String, f64, Vec<[u8; 3]>)> = reg
        .iter()
        .map(|(k, p)| (k.0.to_string(), p.night_vision, swatches(p)))
        .collect();
    rows.sort_by(|a, b| a.0.cmp(&b.0));

    let mut compared = 0usize;
    for (i, a) in rows.iter().enumerate() {
        for b in rows.iter().skip(i + 1) {
            if a.1 == b.1 {
                // The honest converse, asserted rather than left implicit.
                assert_eq!(
                    a.2, b.2,
                    "{} and {} share night_vision {}, and the eye model reads \
                     only that axis, so they must see identically",
                    a.0, b.0, a.1
                );
                continue;
            }
            compared += 1;
            assert_ne!(
                a.2, b.2,
                "{} (nv {}) and {} (nv {}) must not see identically — this is \
                 the collapse the spec's M1 measured",
                a.0, a.1, b.0, b.1
            );
        }
    }
    // The probe must discriminate: a roster where every night_vision is equal
    // would pass the loop above vacuously.
    assert!(
        compared >= 6,
        "only {compared} distinct-night_vision pairs were compared; this test \
         is not exercising what it claims"
    );
}

/// H3 — dichromacy is real once roles are declared. THE SPEC EXPECTS THIS
/// MAY FAIL; see the panic message.
#[test]
fn a_dichromat_separates_red_from_green_less_than_a_trichromat_does() {
    let reg = perception_registry();
    let human = reg.get(&hornvale_species::KindId("human")).unwrap();
    let bugbear = reg.get(&hornvale_species::KindId("bugbear")).unwrap();
    let light = flat_light();
    let red = hue_exemplar("red").unwrap();
    let green = hue_exemplar("green").unwrap();

    let sep = |p: &hornvale_species::PerceptionVector| {
        let o = observer_for(p);
        o.chromatic_distance(&o.sense(&red, &light), &o.sense(&green, &light))
    };
    let (h, b) = (sep(human), sep(bugbear));
    // Anti-vacuity: a metric that returns 0 for everyone would "pass" a
    // naive `b < h`.
    assert!(h > 0.0, "a trichromat must separate red from green at all");
    assert!(
        b < h,
        "H3 FALSIFIED — bugbear separates red/green by {b}, human by {h}. \
         Ship the null: the model produces species that see differently but \
         not species that are colour-blind. Do NOT retune the merge to \
         rescue this."
    );
}

#[test]
fn the_roster_names_resolve_and_an_unknown_one_does_not() {
    let roster = observer_roster();
    assert!(roster.contains(&"standard".to_string()));
    assert!(roster.contains(&"bugbear".to_string()));
    for name in &roster {
        assert!(
            observer_named(name).is_some(),
            "{name} is advertised but does not resolve"
        );
    }
    assert!(observer_named("wyvern").is_none());
}
