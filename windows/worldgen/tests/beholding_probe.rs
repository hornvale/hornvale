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
///
/// Four assertions, four different failure modes, and none subsumes
/// another:
///
/// 1. **The flat sweep.** Guards the normalization path across the
///    intensity range (clamping/scaling/rounding in `to_srgb`). It does
///    **not** guard curve *shape*: on a spectrally flat reflectance under a
///    flat illuminant, `signal[c] / norm[c]` reduces to the flat value
///    regardless of what shape the channel curve has, as long as the norm
///    is self-derived from that same curve (which every non-hue-5 arm's
///    `build()` always does). A hue-5 arm that silently substituted a
///    different, but still self-normalized, curve set would pass this loop
///    unnoticed — confirmed by mutation; see the fix-round note in
///    `task-2-report.md`.
/// 2. **The exemplar sweep.** Pushes the seven authored, spectrally
///    *structured* hue reflectances through both observers and compares
///    `to_srgb` output. A structured reflectance is what makes a curve-shape
///    difference observable at all — this is the assertion the flat sweep
///    structurally cannot be.
/// 3. **The channel-identity check.** Senses a per-band delta reflectance
///    (1.0 at exactly one band, 0.0 elsewhere) under a flat illuminant,
///    which recovers each channel's raw curve value at that band with *no*
///    normalization in the way — the one comparison self-normalization
///    cannot launder away — for every band and every channel.
/// 4. **The projection identity check.** The two observers' `norms()` must
///    be the same carried constants, not independently (re)computed ones —
///    the carried-vs-derived byte-identity landmine Task 1's own kernel
///    tests guard from the other side.
#[test]
fn the_human_row_derives_exactly_the_standard_observer() {
    let reg = perception_registry();
    let human = reg
        .get(&hornvale_species::KindId("human"))
        .expect("human is a speaking kind and must carry a perception row");
    let derived = observer_for(human);
    let standard = hornvale_kernel::color::standard_observer();
    let light = flat_light();

    // 1. The flat sweep.
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

    // 2. The exemplar sweep: spectrally structured reflectances, the thing
    // a flat reflectance cannot exercise.
    for concept in HUE_CONCEPTS {
        let r = hue_exemplar(concept).expect("every hue concept has an exemplar");
        assert_eq!(
            derived.to_srgb(&derived.sense(&r, &light)),
            standard.to_srgb(&standard.sense(&r, &light)),
            "hue exemplar '{concept}' disagreed between the derived and \
             standard observer"
        );
    }

    // 3. Channel identity: a per-band delta reflectance recovers each
    // channel's raw curve value at that band, unnormalized.
    for band in 0..BANDS {
        let mut bands = [0.0; BANDS];
        bands[band] = 1.0;
        let delta = Reflectance::new(bands).unwrap();
        let d_signal = derived.sense(&delta, &light);
        let s_signal = standard.sense(&delta, &light);
        assert_eq!(
            d_signal.get(),
            s_signal.get(),
            "band {band}'s per-channel curve values disagreed between the \
             derived and standard observer"
        );
    }

    // 4. Projection identity: the CARRIED normalizers, not recomputed ones.
    assert_eq!(
        derived.projection().map(|p| p.norms()),
        standard.projection().map(|p| p.norms()),
        "the projection's normalizers must be the SAME carried constants, \
         not independently (re)computed ones"
    );

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

/// F3 — each of the three arms `observer_for` can build (hue 5's `native`,
/// hue 4's `native-anomalous`, hue ≤ 3's `yellow-blue`) gets its own direct
/// coverage of the projection it declares, rather than relying on the
/// vessel `vessel/session/v1` goldens — an incidental guard that only
/// exercised seed 42's single flagship species (bugbear, the `yellow-blue`
/// arm) and is re-baselineable on any drift, colour or not. Two mutations
/// slipped past that golden entirely: swapping the `preserves` strings
/// between arms, and building `norms` in *channel* order rather than the
/// *output-slot* order `Projection::rgb` names (`build`'s own doc, spec
/// §4.2). Each assertion below is chosen so at least one of those two
/// mutations reddens it; `native-anomalous` in particular (goblin, hue 4)
/// had zero coverage anywhere before this — nothing in the vessel goldens
/// exercises a species at that hue tier.
#[test]
fn each_arm_declares_its_own_projection_name_preserves_and_slot_ordered_norms() {
    let reg = perception_registry();
    let white = Reflectance::new([1.0; BANDS]).unwrap();
    let flat = flat_light();

    // hue 5 — human, `native`. This arm clones `standard_observer()`
    // wholesale (H2), so its norms are the CARRIED literals, not anything
    // this test derives live — re-deriving them would just restate
    // `build`'s own computation and could not catch either mutation this
    // test exists for.
    let human = reg.get(&hornvale_species::KindId("human")).unwrap();
    let native = observer_for(human);
    let proj = native.projection().expect("hue 5 declares a projection");
    assert_eq!(proj.name(), "native");
    assert_eq!(
        proj.preserves(),
        "the observer's own channels, carried straight to the screen"
    );
    // rgb = [long, medium, short] = [2, 1, 0] (kernel `standard_observer`'s
    // own comment); norms in OUTPUT-SLOT order is [LONG_NORM, MEDIUM_NORM,
    // SHORT_NORM].
    assert_eq!(proj.norms(), &[3.95, 3.51, 1.98]);

    // hue 4 — goblin, `native-anomalous`. LIVE and previously uncovered.
    let goblin = reg.get(&hornvale_species::KindId("goblin")).unwrap();
    let anomalous = observer_for(goblin);
    let proj = anomalous.projection().expect("hue 4 declares a projection");
    assert_eq!(proj.name(), "native-anomalous");
    assert_eq!(
        proj.preserves(),
        "three chromatic channels; the red-green axis is narrowed, not removed"
    );
    // rgb = [2, 1, 0] again (spec §4.4): slot order reads channel 2 (L')
    // into R, channel 1 (M') into G, channel 0 (S) into B — the REVERSE of
    // channel order, which is exactly what makes this arm able to tell the
    // two orderings apart.
    let signal = anomalous.sense(&white, &flat);
    let slot_order = [signal.get()[2], signal.get()[1], signal.get()[0]];
    let channel_order = [signal.get()[0], signal.get()[1], signal.get()[2]];
    assert_ne!(
        slot_order, channel_order,
        "this arm's rgb permutation must differ from identity, or a slot-vs-\
         channel-order mutation would be invisible to the assertion below"
    );
    assert_eq!(proj.norms(), &slot_order);

    // hue ≤ 3 — bugbear, `yellow-blue`.
    let bugbear = reg.get(&hornvale_species::KindId("bugbear")).unwrap();
    let dichromat = observer_for(bugbear);
    let proj = dichromat
        .projection()
        .expect("hue <= 3 declares a projection");
    assert_eq!(proj.name(), "yellow-blue");
    assert_eq!(
        proj.preserves(),
        "the short-to-long opposition; the red-green axis is not carried"
    );
    // rgb = [1, 1, 0]: the merged channel drives BOTH red and green, blue
    // reads the short channel. Channel order (a bug) would instead read the
    // ACHROMATIC rod (channel 2) into the blue slot — a large, easily
    // distinguished difference.
    let signal = dichromat.sense(&white, &flat);
    let slot_order = [signal.get()[1], signal.get()[1], signal.get()[0]];
    let channel_order = [signal.get()[0], signal.get()[1], signal.get()[2]];
    assert_ne!(
        slot_order, channel_order,
        "this arm's rgb permutation must differ from identity, or a slot-vs-\
         channel-order mutation would be invisible to the assertion below"
    );
    assert_eq!(proj.norms(), &slot_order);
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
