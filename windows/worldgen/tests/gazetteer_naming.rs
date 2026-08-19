//! The Gazetteer's preregistered measurements (Task 7): H1 (the landscape is
//! individuated at a useful granularity) and H2 (two peoples give the same
//! landform genuinely different names). See
//! `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md` §8.
//!
//! No later task calls anything in this file — it exists to measure and
//! report, not to ship a consumed interface.

use hornvale_kernel::{Geosphere, Seed, World};
use hornvale_language::MorphOptions;
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::{Feature, FeatureClass};
use hornvale_worldgen::{feature_name, gazetteer_features, language_of};

/// The mesh level every test here builds at — the production level
/// (`hornvale_terrain::GLOBE_LEVEL`), the same canonical globe Task 1's probe
/// and this crate's own `gazetteer`/`volcano` test modules use, so seed 42's
/// feature population is the same population Task 1 measured.
const LEVEL: u32 = 6;

/// The Step 1 constructor: `hornvale_terrain::generate` under default pins at
/// the production level, seed 42 — the same idiom
/// `windows/worldgen/src/gazetteer.rs`'s and `volcano.rs`'s test modules use.
fn test_terrain() -> (Geosphere, GeneratedTerrain) {
    let geo = Geosphere::new(LEVEL);
    let outcome = hornvale_terrain::generate(Seed(42), &geo, &TerrainPins::default())
        .expect("default pins generate");
    let terrain = GeneratedTerrain::new(geo.clone(), outcome);
    (geo, terrain)
}

/// A neutral shape profile: `NameKind::Landform` draws a bare stem and never
/// reads these (see `volcano.rs`'s own `morph()` fixture, which this mirrors
/// — `MorphOptions` has no `Default`).
fn morph() -> MorphOptions {
    MorphOptions {
        honorifics: false,
        shape_weights: [1.0, 1.0, 1.0],
        shape_beta: 1.0,
    }
}

/// H1: the landscape is individuated at a useful granularity. The bands come
/// from Task 1's probe (`windows/worldgen/examples/gazetteer_probe.rs`),
/// quoted here so the provenance is auditable, NOT from the Watershed spec's
/// figures, which contradict its own chronicle (that is why Task 1 exists).
///
/// Task 1's reported counts at seed 42, production level 6, default pins:
/// landmass 10, sea 1, salt-lake 80, river 106, volcano 208. Each band is set
/// generously around its measured count so the test is a regression detector
/// against a future terrain change, not a re-assertion of today's exact
/// value: too narrow and an unrelated terrain tweak falsely reads as breaking
/// the gazetteer; too wide and it stops detecting the collapse the decision
/// rule below describes (one dominant feature, or hundreds of indistinguishable
/// slivers).
const H1_BANDS: &[(FeatureClass, usize, usize)] = &[
    (FeatureClass::Landmass, 6, 18),
    (FeatureClass::Sea, 1, 3),
    (FeatureClass::SaltLake, 40, 160),
    (FeatureClass::River, 70, 160),
    (FeatureClass::Volcano, 120, 320),
];

/// **Decision rule** (do not widen a band to make this pass): a class
/// yielding ONE feature covering nearly everything means the floor is too
/// low to discriminate; hundreds of near-identical ones means too high to
/// name. Either outcome falsifies H1 for that class, and the honest report is
/// to say so, not to retune the band. `Sea` is legitimately 1 on this world —
/// one connected ocean, measured and ruled on, not overlooked (Task 1's
/// coordinator follow-up).
#[test]
fn seed_42_names_a_useful_number_of_features() {
    let (geo, terrain) = test_terrain();
    let features = gazetteer_features(Seed(42), &geo, &terrain);

    println!("H1: seed 42, production level {LEVEL}, default pins");
    for &(class, lo, hi) in H1_BANDS {
        let n = features.iter().filter(|f| f.id.class == class).count();
        println!("  {class:?}: {n} features (band {lo}..={hi})");
        assert!(
            (lo..=hi).contains(&n),
            "{class:?}: {n} features, outside the preregistered band {lo}..={hi}"
        );
    }
}

/// The world's peoples: every `SocialForm::Settled` kind in the canonical
/// biosphere registry — the same roster resolution
/// `bake_history_from`/`build_world`'s unpinned settlement stage performs
/// (`windows/worldgen/src/lib.rs`). Ascending `KindId` order, for free, from
/// `ComponentStore`'s `BTreeMap` backing — deterministic without an explicit
/// sort.
fn worlds_peoples() -> Vec<&'static str> {
    let biosphere = hornvale_species::biosphere_registry();
    biosphere
        .iter()
        .filter(|(_, b)| b.social_form == hornvale_species::SocialForm::Settled)
        .map(|(k, _)| k.0)
        .collect()
}

/// H2 — the one at real risk. Preregistered prediction (spec §8, Nathan's,
/// attributed): the names diverge — two peoples give the same landform
/// genuinely different names.
///
/// Measured over seed 42's full feature set (every class `gazetteer_features`
/// returns, 405 features at this build), for every pair of the world's
/// peoples (the 15 `SocialForm::Settled` kinds, 105 unordered pairs), the
/// fraction of features whose two names differ.
///
/// **What this measurement can and cannot show.** Each people's `Phonology`
/// is drawn for real (`language_of`, the production per-species draw), so
/// this run uses the world's REAL, mutually distinct tongues — which means it
/// cannot separate "the species salt reaches the draw" from "the tongues
/// happen to differ": both would move this number the same direction. The
/// unit-scale control that isolates the first claim is
/// `windows/worldgen/src/gazetteer.rs`'s
/// `two_peoples_name_one_feature_differently`, which holds ONE `Phonology`
/// fixed across two species so only `species` can move the name. That
/// control is asserted separately (and re-run below is not required — it is
/// part of this crate's own unit-test suite, `cargo test -p hornvale-worldgen
/// gazetteer::tests::two_peoples_name_one_feature_differently`); this test
/// cites it rather than re-deriving it, per the brief.
///
/// **Measured result (seed 42): every one of the 105 pairs differs on every
/// one of the 405 features — overall divergence 42525/42525 = 1.0000.** The
/// control above also passes, so this is not read as an artefact of the
/// species salt failing to reach the draw; H2 is CONFIRMED, as far as seed 42
/// alone can confirm anything (see below). The floor asserted here (0.9) sits
/// comfortably under the measured 1.0, deliberately not raised to match it —
/// a floor of "the observed value" would fail the instant a phonology change
/// nudges even one name, which is a fragility bet, not a hypothesis test.
///
/// **A single-world reading is an anecdote.** This is seed 42 only, and it
/// says "seed 42 says H2 confirmed", not "H2 is true of Hornvale worlds in
/// general". The Watershed had a single-world result reverse at census scale
/// three times in one campaign; a multi-seed run (a handful of seeds through
/// the same pipeline) would cost roughly one terrain build plus one
/// full-roster naming pass per seed — cheap relative to a census, and the
/// natural follow-up if this number is ever load-bearing for a design
/// decision rather than a shippable headline.
#[test]
fn seed_42_peoples_diverge_on_landform_names() {
    let (geo, terrain) = test_terrain();
    let features: Vec<Feature> = gazetteer_features(Seed(42), &geo, &terrain);
    let peoples = worlds_peoples();
    let world = World::new(Seed(42));
    let morph = morph();

    let phonologies: Vec<(&str, hornvale_language::Phonology)> = peoples
        .iter()
        .map(|&species| (species, language_of(&world, species)))
        .collect();

    println!(
        "H2: seed 42, {} features, {} peoples ({} pairs)",
        features.len(),
        peoples.len(),
        peoples.len() * peoples.len().saturating_sub(1) / 2
    );
    println!("peoples: {peoples:?}");

    let mut overall_differ: usize = 0;
    let mut overall_total: usize = 0;

    for i in 0..phonologies.len() {
        for j in (i + 1)..phonologies.len() {
            let (species_a, ph_a) = &phonologies[i];
            let (species_b, ph_b) = &phonologies[j];

            let mut differ = 0usize;
            for f in &features {
                let name_a = feature_name(Seed(42), f.id, species_a, ph_a, &morph);
                let name_b = feature_name(Seed(42), f.id, species_b, ph_b, &morph);
                if name_a.roman != name_b.roman {
                    differ += 1;
                }
            }
            let total = features.len();
            let frac = differ as f64 / total as f64;
            println!("  {species_a} x {species_b}: {differ}/{total} = {frac:.4}");

            overall_differ += differ;
            overall_total += total;
        }
    }

    let overall_frac = overall_differ as f64 / overall_total as f64;
    println!("overall: {overall_differ}/{overall_total} = {overall_frac:.4}");

    // Decision rule (do NOT retune to rescue the prediction — see the
    // module/function doc comment and the brief). The floor is set
    // comfortably below the measured overall figure, printed above, so a
    // reviewer can see the margin rather than a bare pass/fail.
    assert!(
        overall_frac > 0.9,
        "H2 measured overall divergence {overall_frac:.4} — see the printed per-pair table \
         (--nocapture) for the full readout"
    );
}
