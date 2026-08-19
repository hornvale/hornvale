//! The Gazetteer's preregistered measurements (Task 7): H1 (the landscape is
//! individuated at a useful granularity) and H2 (two peoples give the same
//! landform genuinely different names). See
//! `docs/superpowers/specs/2026-08-18-the-gazetteer-design.md` §8.
//!
//! No later task calls anything in this file — it exists to measure and
//! report, not to ship a consumed interface.
//!
//! **Coordinator fix round 1 (2026-08-19): the cross-people figure below is
//! a tautology, not a confirmation.** `Namer::name`'s stream
//! (`domains/language/src/naming.rs`) derives as
//! `seed.derive(ROOT).derive(species).derive(NAME).derive(kind).derive(salt)`
//! — `species` is a LEG IN THE DERIVE PATH, so two distinct species strings
//! draw from two entirely different streams for the same feature and their
//! names differ **by construction**, independent of phonology or anything
//! about the world. H2 is confirmed by the unit-scale control
//! (`windows/worldgen/src/gazetteer.rs::gazetteer::tests::two_peoples_name_one_feature_differently`,
//! which holds ONE phonology fixed so `species` is the only variable) and by
//! that control alone; see `seed_42_peoples_diverge_on_landform_names`'s own
//! doc comment for the full accounting, and `seed_42_within_people_name_collisions`
//! for the measurement that is actually at risk here.

use hornvale_kernel::{Geosphere, Seed, World};
use hornvale_language::MorphOptions;
use hornvale_terrain::GeneratedTerrain;
use hornvale_terrain::TerrainPins;
use hornvale_terrain::landscape::{Feature, FeatureClass, FeatureId};
use hornvale_worldgen::{feature_name, gazetteer_features, language_of};
use std::collections::BTreeMap;

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
/// landmass 10, sea 1, salt-lake 80, river 106. Task 1's probe never measured
/// volcanoes — they live in `windows/worldgen`, outside a domain-layer probe
/// — so the volcano 208 figure comes from Task 6 instead. Each band is set
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

/// H2 cross-people check — **a tautology, not evidence, kept only as a
/// collision sanity check** (coordinator fix round 1, 2026-08-19; see this
/// file's module doc comment).
///
/// Preregistered prediction (spec §8, Nathan's, attributed): two peoples give
/// the same landform genuinely different names.
///
/// **Why the number below cannot test that.** `Namer::name`'s stream
/// (`domains/language/src/naming.rs:407-417`) derives as
/// `seed.derive(ROOT).derive(species).derive(NAME).derive(kind).derive(salt)`
/// — `species` is a LEG IN THE DERIVE PATH, not a modifier layered onto a
/// shared draw. Two distinct species strings therefore draw from two
/// entirely different streams for the same feature, so their names differ
/// **by construction**, regardless of phonology, regardless of salt,
/// regardless of anything about the world. The only way this measurement
/// could report less than total divergence is if two "peoples" shared one
/// species label — which would make them one people, not two. So an
/// exactly-1.0000 result over 42,525 cross-people (feature, pair) draws is
/// not a confirmation of divergence; it is the signature of a hash producing
/// different outputs for different inputs.
///
/// **What actually confirms H2** is
/// `windows/worldgen/src/gazetteer.rs`'s
/// `gazetteer::tests::two_peoples_name_one_feature_differently` — the
/// unit-scale control that holds ONE `Phonology` fixed across two species so
/// `species` is the only variable, and which WOULD fail if `feature_name`
/// ever dropped `species` from the draw. This test cannot make that
/// distinction (a `feature_name` that silently ignored `species` would still
/// pass THIS test, because each people's own `Phonology` argument still
/// differs) — only the held-phonology control isolates the claim. That
/// control was re-run for this fix and passes; H2 is confirmed by it, in
/// full, and this test adds nothing to that confirmation.
///
/// **What this test is worth keeping for**: a mild, genuine fact about the
/// namer's collision behaviour — it verifies zero accidental full-string
/// collisions across all 42,525 cross-people pairs at seed 42. That is a
/// statement about the derive chain's hashing, not about the world's
/// languages, and the assertion below is read that way, not as an H2 floor.
///
/// **A single-world reading regardless.** Seed 42 only.
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
        "H2 cross-people collision check (tautological by construction — see doc comment; \
         NOT evidence for H2): seed 42, {} features, {} peoples ({} pairs)",
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

    // Collision sanity check only (see doc comment) — NOT an H2 floor. The
    // derive chain makes < 1.0 here mean an accidental full-string collision
    // between two different peoples' names, which is what this test exists
    // to catch; it says nothing about whether H2 is true.
    assert!(
        overall_frac > 0.9,
        "H2 cross-people collision check: {overall_frac:.4} — see the printed per-pair table \
         (--nocapture) for the full readout"
    );
}

/// H2's actually-informative addendum (coordinator fix round 1, 2026-08-19):
/// **within-people** name collision. Cross-people divergence is guaranteed
/// by the derive chain (see `seed_42_peoples_diverge_on_landform_names`'s doc
/// comment); the real open risk this gazetteer carries is the opposite
/// direction — 405 features all drawing 2-3 syllable stems from ONE tongue
/// may collide heavily, and a gazetteer whose names do not distinguish its
/// own places is worth much less than one whose names merely differ from a
/// neighbouring language's.
///
/// For each of the world's 15 peoples, over all 405 seed-42 features: the
/// count of distinct `roman` names, the collision rate
/// (`1 - distinct/total`), and the largest collision cluster (the most
/// features sharing one rendered name, and which features those are).
/// Reported per people and pooled overall. **Descriptive only — no threshold
/// is asserted and nothing here is tuned**; whatever this prints is the
/// finding, per the brief.
///
/// **Not automatically a defect.** Decision 0024 already ratified that
/// committed names may collide and that uniqueness is a reference-time
/// property (settlements disambiguate at render time from site facts, not by
/// stuffing more entropy into the name), so a high within-people rate here is
/// expected context for the campaign, not evidence of a bug — but it is
/// something the chronicle should state plainly rather than let the H2
/// cross-people tautology paper over.
#[test]
fn seed_42_within_people_name_collisions() {
    let (geo, terrain) = test_terrain();
    let features: Vec<Feature> = gazetteer_features(Seed(42), &geo, &terrain);
    let peoples = worlds_peoples();
    let world = World::new(Seed(42));
    let morph = morph();

    println!(
        "H2 addendum: within-people name collisions, seed 42, {} features, {} peoples",
        features.len(),
        peoples.len()
    );

    let mut pooled_total = 0usize;
    let mut pooled_distinct = 0usize;
    let mut rates: Vec<f64> = Vec::with_capacity(peoples.len());

    for &species in &peoples {
        let ph = language_of(&world, species);

        let mut by_name: BTreeMap<String, Vec<FeatureId>> = BTreeMap::new();
        for f in &features {
            let name = feature_name(Seed(42), f.id, species, &ph, &morph);
            by_name.entry(name.roman).or_default().push(f.id);
        }

        let total = features.len();
        let distinct = by_name.len();
        // Sanity only (descriptive test, no threshold): the accounting must
        // be internally consistent, or a bug upstream silently dropped a
        // feature rather than the roster genuinely colliding.
        assert_eq!(
            by_name.values().map(Vec::len).sum::<usize>(),
            total,
            "{species}: every feature must land in exactly one name bucket"
        );
        assert!(
            distinct <= total,
            "{species}: distinct names cannot exceed features named"
        );

        let collision_rate = 1.0 - (distinct as f64 / total as f64);
        let (largest_name, largest_members) = by_name
            .iter()
            .max_by_key(|(_, members)| members.len())
            .expect("at least one name bucket when total > 0");

        println!(
            "  {species}: {distinct}/{total} distinct (collision rate {collision_rate:.4}); \
             largest cluster '{largest_name}' x{} = {:?}",
            largest_members.len(),
            largest_members
        );

        pooled_total += total;
        pooled_distinct += distinct;
        rates.push(collision_rate);
    }

    let pooled_rate = 1.0 - (pooled_distinct as f64 / pooled_total as f64);
    let mean_rate = rates.iter().sum::<f64>() / rates.len() as f64;
    let min_rate = rates.iter().cloned().fold(f64::INFINITY, f64::min);
    let max_rate = rates.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    println!(
        "overall: pooled collision rate {pooled_rate:.4} ({pooled_distinct}/{pooled_total} distinct); \
         per-people mean {mean_rate:.4}, min {min_rate:.4}, max {max_rate:.4}"
    );
}
