//! BIO-35 (The Demesne): per-axis resource-supply fields (Stage 1, task T1)
//! and their consumer, the rank-restored per-species K (Stage 2, task T2).
//!
//! T1 built `mineral_supply_field`/`forage_supply_field`/`DETRITUS_AMBIENT`
//! as pure builders nothing yet consumed. T2 wires them into
//! `per_species_suitability` via [`hornvale_worldgen::axis_supply`], the axis
//! dot product that replaces the old `base_carrying(vertex) × Σuptake` scalar
//! — a niche direction now SELECTS a spatial combination instead of merely
//! rescaling one shared field, so two species with different uptake
//! *directions* can peak in different *places* (the rank-restoration
//! keystone below), and a genuinely mineral- or detritus-driven species can
//! now track its own supply field spatially (the emergence keystone below).
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{KindId, Seed, Value, World};
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    SettlementPins, SkyChoice, WorldComponents, axis_supply, build_world, carrying_inputs_of,
    species_carrying_input, terrain_of,
};

#[test]
fn mineral_supply_tracks_prospectivity_spatially() {
    // On a real seed-42 world, the mineral field peaks where prospectivity
    // peaks and is 0 where prospectivity is 0 — a genuinely SPATIAL field,
    // not a constant.
    let world = hornvale_worldgen::build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        hornvale_worldgen::SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &hornvale_worldgen::SettlementPins::default(),
    )
    .unwrap();

    // Reach the terrain handle the way `per_species_suitability`'s callers do
    // (`terrain_of`), then its geosphere — the single construction site for
    // the terrain provider on a built world.
    let terrain = hornvale_worldgen::terrain_of(&world).unwrap();
    let geo = terrain.geosphere();

    let scale = 10.0;
    let field = hornvale_worldgen::mineral_supply_field(geo, &terrain, scale);

    // Genuinely spatial: at least two distinct values across vertices.
    let mut distinct: Vec<f64> = Vec::new();
    for c in geo.vertices() {
        let v = *field.get(c);
        if !distinct.iter().any(|d: &f64| (*d - v).abs() < 1e-12) {
            distinct.push(v);
        }
        if distinct.len() >= 2 {
            break;
        }
    }
    assert!(
        distinct.len() >= 2,
        "mineral supply field must vary across vertices, not be a constant"
    );

    // Monotone in prospectivity at two probe vertices: whichever vertex has
    // higher prospectivity must have a proportionally higher supply value
    // (field = prospectivity * scale, so equality up to float epsilon).
    // The probes must be LAND vertices: since The Tumult's land mask the field
    // is 0 at sea regardless of the seafloor's (honestly derived, but
    // unreachable) prospectivity — the first and last land vertex in ascending
    // `Vertex` order, a deterministic choice with no float ordering.
    let land: Vec<hornvale_kernel::Vertex> =
        geo.vertices().filter(|c| !terrain.is_ocean(*c)).collect();
    assert!(
        land.len() >= 2,
        "seed 42 must have at least two land vertices"
    );
    let probe_a = land[0];
    let probe_b = land[land.len() - 1];
    let prospectivity_a = terrain.prospectivity_at(probe_a);
    let prospectivity_b = terrain.prospectivity_at(probe_b);
    let field_a = *field.get(probe_a);
    let field_b = *field.get(probe_b);
    assert!((field_a - prospectivity_a * scale).abs() < 1e-9);
    assert!((field_b - prospectivity_b * scale).abs() < 1e-9);
    match prospectivity_a.total_cmp(&prospectivity_b) {
        std::cmp::Ordering::Less => assert!(field_a < field_b),
        std::cmp::Ordering::Greater => assert!(field_a > field_b),
        std::cmp::Ordering::Equal => assert!((field_a - field_b).abs() < 1e-9),
    }

    // Bounds: prospectivity is [0,1], so the field is [0, scale].
    for c in geo.vertices() {
        let v = *field.get(c);
        assert!(
            (0.0..=scale + 1e-9).contains(&v),
            "mineral supply out of range: {v}"
        );
    }
}

/// THE LAND MASK (The Tumult), extended by THE SEA MASK (The Vacancy T6/T8):
/// every v1 TERRESTRIAL resource-supply axis is 0 at sea and `MARINE_FORAGE`
/// is 0 on land — both masks are a property of the supply fields, not a
/// decree in the K assembly (see `DETRITUS_AMBIENT`'s terrestrial-supply
/// frame and `marine_forage_supply_field`'s mirror). Before The Vacancy T8
/// authored any kind onto `MARINE_FORAGE`, this meant EVERY kind's K was 0 at
/// sea; T8 deliberately broke that for exactly five kinds (four marine plus
/// the amphibious giant crocodile), so the test below now asserts the mask
/// held for those five TOO, in its now-correct (not simply "always zero")
/// form, alongside the original guard for everyone else.
///
/// This states explicitly what a bug used to do by accident. Before The
/// Tumult's elevation re-datum, `ConditionNiche.elevation` was scored against
/// the raw isostatic `ReferenceElevation`, which put an ocean vertex ~4 km from
/// every authored optimum and so zeroed the seafloor through the *condition*
/// term. Correcting the datum left ocean vertices only ~1100 m below sea level
/// and exposed two supply axes that never had a mask of their own: `MINERAL`
/// (a prospectivity read, defined on the seafloor) and `DETRITUS` (a global
/// constant). Measured at seed 42 with the datum corrected and no mask, the
/// submerged share of total K was **0.85** for the otyugh, **0.86** for the
/// rust monster and **0.74** for the xorn — a swamp detritivore, a cave
/// mineral-eater and a burrowing elemental, each mostly at sea.
///
/// MUTATION GUARD: dropping either land mask (`mineral_supply_field`'s or
/// `detritus_supply_field`'s) re-admits exactly those three kinds' seabed K
/// and this test fails on them by name; dropping the sea mask
/// (`marine_forage_supply_field`'s `is_ocean` guard) re-admits marine
/// carrying capacity on land for the five T8 kinds and fails on THEM.
///
/// THE RADIATION (C2d): sea-elf is a SIXTH occupant of the marine axis and the
/// first people on it, mixed rather than pure — so the amphibious arm below is
/// a set now, not a constant.
#[test]
fn no_species_draws_carrying_capacity_from_the_wrong_medium() {
    // The four PURELY marine T8 kinds: their niche weights only
    // `MARINE_FORAGE`, so every terrestrial supply axis contributes an exact
    // zero to their dot product regardless of that axis's land value — they
    // must be wholly submerged (dry == 0), the mirror of the land mask.
    let marine_only: std::collections::BTreeSet<&str> =
        ["giant-octopus", "giant-squid", "killer-whale", "reef-shark"]
            .into_iter()
            .collect();
    // The amphibious proof cases (spec §3.4): a kind weighting BOTH a
    // terrestrial axis and `MARINE_FORAGE`, so its K must be nonzero in BOTH
    // media — the observable signature of the sparse-uptake, no-special-case
    // design.
    //
    // THE RADIATION (C2d) makes this a SET rather than a constant. Sea-elf is
    // the second occupant and the first PEOPLE here: 0.75 `MARINE_FORAGE`
    // plus 0.25 across two terrestrial axes, because a settled shore people
    // does not live entirely in the water. It arrived at this branch as a
    // FAILURE of the `else` arm ("sea-elf draws 140.02 of its 147.84 total
    // carrying capacity from submerged vertices"), which is the guard working:
    // the test had no way to express a second mixed kind, and a `const` was
    // the reason.
    let amphibious: std::collections::BTreeSet<&str> =
        ["giant-crocodile", "sea-elf"].into_iter().collect();

    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world_42();
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let sky = hornvale_worldgen::sky_of(&world).expect("sky reconstructs");
    let system = sky.system().expect("seed 42 has a generated star system");
    let insolation = hornvale_astronomy::insolation_rel(&system.star, &system.anchor);
    let obliquity = system.anchor.obliquity.get();
    let regime = match system.anchor.rotation {
        hornvale_astronomy::Rotation::Spinning { day, .. } => {
            hornvale_climate::RotationRegime::Spinning { day_std: day.get() }
        }
        hornvale_astronomy::Rotation::Locked => hornvale_climate::RotationRegime::Locked,
    };

    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let bios: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    // Same `wc.biosphere` order as `bios`, so the realm slice stays
    // index-aligned — a kind absent from the sparse habitat-realm store
    // defaults to `Surface`.
    let realm: Vec<hornvale_species::HabitatRealm> = wc
        .biosphere
        .iter()
        .map(|(k, _)| {
            wc.habitat_realm
                .get(k)
                .copied()
                .unwrap_or(hornvale_species::HabitatRealm::SURFACE)
        })
        .collect();
    // The Range: the `biome_affinity` registry is NOT empty — `gnoll` and
    // `woolly-mammoth` carry rows since task 4, and both are in the
    // whole-biosphere roster scored here. So this is a deliberate CONTROL, not a
    // copy of the registry.
    //
    // Deliberate because this readout is about the SUBMERGED/land partition and
    // the marine roster, a question upstream of any per-biome preference: an
    // affinity re-weights a kind across biomes it can already reach, it does not
    // change which vertices the field reaches. All-`None` is bit-identical to the
    // pre-affinity physics this file's numbers were taken under (task 3's
    // `an_absent_affinity_is_bit_identical`).
    let affinity: Vec<Option<hornvale_species::BiomeAffinity>> = vec![None; bios.len()];
    let ks = hornvale_worldgen::per_species_suitability(
        geo, &terrain, &climate, obliquity, insolation, &regime, &bios, &realm, &affinity,
    );

    let submerged: Vec<hornvale_kernel::Vertex> =
        geo.vertices().filter(|c| terrain.is_ocean(*c)).collect();
    assert!(
        !submerged.is_empty(),
        "seed 42 must have ocean vertices for this test to mean anything"
    );

    let mut placed_on_land = 0u32;
    for (tag, k) in &ks {
        let kind = kinds[*tag as usize].0;
        let mut wet = 0.0_f64;
        let mut dry = 0.0_f64;
        for c in geo.vertices() {
            let v = *k.get(c);
            if terrain.is_ocean(c) {
                wet += v;
            } else {
                dry += v;
            }
        }
        let total = wet + dry;
        if marine_only.contains(kind) {
            assert_eq!(
                dry, 0.0,
                "{kind} draws {dry} of its {total} total carrying capacity from LAND vertices — \
                 a purely marine kind's terrestrial supply axes must be 0"
            );
        } else if amphibious.contains(kind) {
            assert!(
                wet > 0.0 && dry > 0.0,
                "{kind} is an amphibious proof case: it must draw nonzero K from BOTH \
                 media, got wet={wet} dry={dry}"
            );
        } else {
            assert_eq!(
                wet, 0.0,
                "{kind} draws {wet} of its {total} total carrying capacity from submerged \
                 vertices — the terrestrial supply axes must be 0 at sea"
            );
        }
        if total > 0.0 {
            placed_on_land += 1;
        }
    }
    // Not a vacuous pass: the mask must not have zeroed the whole roster.
    assert!(
        placed_on_land >= 8,
        "only {placed_on_land} kinds have any carrying capacity at all — the land mask \
         should zero the seafloor, not the world"
    );
}

#[test]
fn forage_supply_is_a_fraction_of_base_carrying_and_deterministic() {
    let geo = hornvale_kernel::Geosphere::new(3);
    let base = hornvale_kernel::VertexMap::from_fn(&geo, |c| (c.0 as f64) * 0.1);
    let a = hornvale_worldgen::forage_supply_field(&geo, &base);
    let b = hornvale_worldgen::forage_supply_field(&geo, &base);
    for c in geo.vertices() {
        assert_eq!(a.get(c), b.get(c));
        assert!(
            *a.get(c) <= *base.get(c),
            "forage is a fraction of primary production"
        );
    }
}

/// THE RANK-RESTORATION KEYSTONE (T2, `axis_supply`): two vertices — A
/// photosynthate-rich, B mineral-rich — and two niches (a plant-eater, a
/// rock-eater) with opposite axis weights. Each niche's supply must peak in
/// the vertex that supplies ITS axis, not in the same vertex for both.
///
/// MUTATION GUARD: the OLD scalar `supply = base(vertex) × Σuptake` gives
/// every niche the SAME vertex ranking (`base` is identical per vertex,
/// `Σuptake` is a per-niche CONSTANT that does not depend on the vertex), so
/// this pair of strict inequalities cannot both hold under the collapsed
/// model — only the per-axis dot product can differentiate WHERE two
/// differently-shaped niches peak.
#[test]
fn different_uptake_vectors_peak_in_different_vertices() {
    use hornvale_kernel::{MINERAL, PHOTOSYNTHATE, ResourceVector};
    let vertex_a = [(PHOTOSYNTHATE, 10.0), (MINERAL, 0.0)];
    let vertex_b = [(PHOTOSYNTHATE, 0.0), (MINERAL, 10.0)];
    let plant = ResourceVector::new(&[(PHOTOSYNTHATE, 1.0), (MINERAL, 0.0)]).unwrap();
    let rock = ResourceVector::new(&[(PHOTOSYNTHATE, 0.0), (MINERAL, 1.0)]).unwrap();
    // the plant-eater's supply is higher in A; the rock-eater's is higher in B.
    assert!(
        axis_supply(&plant, &vertex_a) > axis_supply(&plant, &vertex_b),
        "plant-eater peaks in A"
    );
    assert!(
        axis_supply(&rock, &vertex_b) > axis_supply(&rock, &vertex_a),
        "rock-eater peaks in B"
    );
}

/// Every `stack_settlement`'s `.dominant` tag, mapped back to its `KindId`
/// label via `wc.biosphere`'s ascending-`KindId` order — the SAME
/// build-local dense-index contract `per_species_suitability`'s doc comment
/// spells out (never identity, valid only within this one report call).
/// Counts settlements per dominant kind over the WHOLE roster (fauna
/// included) — [`hornvale_worldgen::demography_report_from`]'s stack, not the
/// peopled-only settlement-genesis pipeline (which never places a fauna
/// kind by construction; see `species_worlds.rs`'s module doc).
fn dominant_settlement_counts(
    world: &World,
    wc: &WorldComponents,
) -> std::collections::BTreeMap<&'static str, u32> {
    let terrain = terrain_of(world).unwrap();
    let climate = hornvale_worldgen::climate_from(world, &terrain).unwrap();
    let report = hornvale_worldgen::demography_report_from(world, wc, &terrain, &climate).unwrap();
    let kinds: Vec<KindId> = wc.biosphere.iter().map(|(k, _)| *k).collect();
    let mut counts: std::collections::BTreeMap<&'static str, u32> =
        std::collections::BTreeMap::new();
    for s in &report.stack_settlements {
        *counts.entry(kinds[s.dominant as usize].0).or_insert(0) += 1;
    }
    counts
}

/// The distinct `peopled-by` objects across every committed settlement (the
/// psyche-only settlement-genesis pipeline's actual placement outcome) —
/// mirrors `hornvale_worldgen`'s own private `placed_species` helper, reread
/// here because integration tests only see `pub` surface.
fn peopled_kinds(world: &World) -> std::collections::BTreeSet<String> {
    world
        .ledger
        .find(hornvale_species::PEOPLED_BY)
        .filter_map(|f| match &f.object {
            Value::Text(s) => Some(s.clone()),
            _ => None,
        })
        .collect()
}

/// The ruler against the Confluence campaign's denominator artifact (a kind
/// that dominates exactly one attractor is measurement noise, not
/// "placed"): a kind counts as a material full-roster dominant only if it
/// tops `.dominant` on at least this many settlements.
const MIN_SETTLEMENTS_FOR_DOMINANCE: u32 = 2;

/// BASELINE (measured 2026-07-19, PRE-repoint — the old `base_carrying(vertex)
/// × Σuptake` scalar supply — over the REAL production roster: 16 kinds
/// (four peopled goblinoid-family + kobold, plus twelve fauna: treant,
/// twig-blight, giant-elk, woolly-mammoth, giant-goat, otyugh, xorn,
/// rust-monster, three chromatic dragons, owlbear), not the 6-kind roster
/// this task's brief sketched from memory.
///
/// At seed 42, `dominant_settlement_counts` (whole roster, 696 total
/// settlements) read `[rust-monster: 113, twig-blight: 49, xorn: 1]` —
/// xorn's single settlement is exactly the denominator-artifact noise
/// `MIN_SETTLEMENTS_FOR_DOMINANCE` exists to exclude, so the material
/// dominant count is **2**. `peopled_kinds` (the psyche-only genesis
/// pipeline) read `{goblin, hobgoblin}` — **2** kinds; bugbear and kobold
/// dominate zero attractors. Union of the two sets: **4** distinct kinds.
const BASELINE_DOMINANT_KINDS_42: usize = 2;
/// BASELINE peopled-by kind count at seed 42. The Living Community epoch made
/// the deep-history bake the settlement provider: it seeds EVERY goblinoid
/// people its own proto-communities (they persist by lineage, not by winning
/// local coexistence dominance), so all four peoples were peopled-by kinds —
/// re-pinned 2 -> 4 (measured on the epoch; this is a placement-provider
/// change, orthogonal to T2's per-axis supply thesis). The Vacancy T9 adds a
/// fifth people (the gnoll), measured at seed 42 to also place a settlement —
/// re-pinned 4 -> 5. The Generalist adds a sixth people (human), measured at
/// seed 42 to also place a settlement — re-pinned 5 -> 6. The Delvers (C2c)
/// adds three dwarves, measured at seed 42 to place settlements for all three
/// — re-pinned 6 -> 9. (It briefly read 11 while the campaign carried five
/// dwarves; spec §11 withdrew Mountain and Duergar.) The Radiation (C2d) adds
/// six elves, measured at seed 42 to place settlements for all six — re-pinned
/// 9 -> 15. Every people the roster holds is a peopled-by kind at this seed,
/// which is a property of the deep-history bake (it seeds every Settled people
/// its own proto-communities, which persist by lineage rather than by winning
/// local dominance), not a claim that six new peoples all found good ground.
const BASELINE_PEOPLED_KINDS_42: usize = 15;
/// BASELINE union (dominant ∪ peopled-by) distinct kind count at seed 42.
const BASELINE_UNION_KINDS_42: usize = 4;

/// Frozen BEFORE the post-repoint readout (preregistration, the-confluence
/// lesson): a MATERIAL rise over [`BASELINE_UNION_KINDS_42`]'s 4. Chosen
/// from theory: `axis_supply` gives every MINERAL-pure niche (xorn,
/// rust-monster) and every PHOTOSYNTHATE-pure niche (treant, twig-blight) a
/// genuinely distinct spatial pattern from the old shared-NPP ranking, so
/// at least one more full-roster dominant should clear the ruler at seed 42
/// once mineral/forage supply stops being a uniform rescale of the same
/// field every species shared.
const PREREGISTERED_MIN_DOMINANTS: usize = 5;

fn world_42() -> World {
    build_world(
        hornvale_kernel::Seed(42),
        &hornvale_astronomy::SkyPins::default(),
        SkyChoice::Generated,
        &hornvale_terrain::TerrainPins::default(),
        &SettlementPins::default(),
    )
    .unwrap()
}

/// THE EMERGENCE KEYSTONE (T2, preregistered against [`BASELINE_UNION_KINDS_42`]):
/// post-repoint, seed 42's full-roster dominant-kind count (ruler-filtered)
/// plus its peopled-by kind count must rise materially, and the newly
/// spatially-differentiated mineral axis must place a genuinely NEW kind —
/// xorn, absent from the baseline's material dominants (its lone
/// pre-repoint settlement was denominator noise) — as a real, ruler-clearing
/// full-roster dominant.
///
/// **Deviation from the brief's sketch, measured not assumed:** the brief's
/// draft expected a NON-goblinoid PEOPLED kind (kobold) to also newly place
/// at seed 42. Measured post-repoint, it does not (`peopled_kinds` stays
/// `{goblin, hobgoblin}`, byte-identical to baseline) — and this is
/// structural, not a tuning shortfall: every one of the four peopled
/// species' authored niches (`domains/species/src/lib.rs`) is a pure
/// `PLANT_FORAGE`/`ANIMAL_PREY` blend with ZERO weight on
/// `PHOTOSYNTHATE`/`MINERAL`/`DETRITUS` — the three axes T2 gave their own
/// spatial fields. `ANIMAL_PREY` supply is Stage 2's placeholder zero, so
/// the peopled-only stack's competition is governed by `PLANT_FORAGE`
/// alone, which (via `forage_supply_field`) is still a uniform rescale of
/// `base_carrying` for every peopled species — `axis_supply` cannot
/// differentiate their spatial RANKING no matter how `FORAGE_FRACTION` is
/// tuned (empirically swept 0.05..5.0 during this task: seed 42's
/// peopled-by set never changed). Diversifying the peopled roster's own
/// placement needs a future stage's authoring (e.g. a mining kobold niche
/// weighted onto `MINERAL`) — out of T2's scope per the 0021 constraint
/// (never author a placement to force a specific test to pass). The fauna
/// half of the brief's ask (`xorn`) IS measured below.
///
/// **THE STRUCTURAL PREMISE ABOVE EXPIRED IN THE DELVERS (C2c, 2026-08-07).**
/// The paragraph's whole argument rests on "every peopled species' authored
/// niche is a pure `PLANT_FORAGE`/`ANIMAL_PREY` blend with ZERO weight on
/// `PHOTOSYNTHATE`/`MINERAL`/`DETRITUS`". That is now false, and deliberately
/// so: `gully-dwarf` weights `DETRITUS` at 0.50 and `desert-dwarf` carries a
/// three-way vector. The `MINERAL` half of the expiry was withdrawn with the
/// two subterranean kinds (spec §11) — no people weights `MINERAL` today —
/// but the premise stays broken, because a `DETRITUS`-weighted people is
/// exactly as much a counterexample as a `MINERAL`-weighted one. So the
/// peopled-by count is re-pinned 6 -> 9 as a measurement, and the assertion
/// message no longer claims the structural reason, because the structure
/// changed.
///
/// claim: structural(seed: 42)
#[test]
fn settlements_and_dominants_diversify_on_seed_42() {
    let world = world_42();
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");

    let dominant_counts = dominant_settlement_counts(&world, &wc);
    let material_dominants: std::collections::BTreeSet<&'static str> = dominant_counts
        .iter()
        .filter(|(_, count)| **count >= MIN_SETTLEMENTS_FOR_DOMINANCE)
        .map(|(kind, _)| *kind)
        .collect();
    let peopled = peopled_kinds(&world);

    assert_eq!(
        peopled.len(),
        BASELINE_PEOPLED_KINDS_42,
        "the peopled-by roster at seed 42 moved — got {peopled:?}"
    );
    assert!(
        material_dominants.len() > BASELINE_DOMINANT_KINDS_42,
        "T2's axis dot product should differentiate at least one more full-roster \
         dominant beyond the {BASELINE_DOMINANT_KINDS_42}-kind baseline; got {material_dominants:?}"
    );

    let union: std::collections::BTreeSet<String> = material_dominants
        .iter()
        .map(|s| s.to_string())
        .chain(peopled.iter().cloned())
        .collect();
    assert!(
        union.len() >= PREREGISTERED_MIN_DOMINANTS,
        "union of material full-roster dominants and peopled-by kinds must clear the \
         preregistered floor of {PREREGISTERED_MIN_DOMINANTS} (baseline was \
         {BASELINE_UNION_KINDS_42}); got {} — {union:?}",
        union.len()
    );

    // The fauna half of the brief's ask: the pure-MINERAL niche (xorn or
    // rust-monster — see the UPDATE note below) must now be a MATERIAL
    // full-roster dominant — it was baseline noise (a single
    // denominator-artifact settlement) under the old shared-NPP scalar.
    //
    // UPDATE (The Deep Realm, Task 6): xorn and rust-monster share this one
    // MINERAL resource niche, so which specific kind clears the dominance
    // ruler has always depended on their `condition_niche` curves, not the
    // resource axis this assertion is really about. Task 6 honestly
    // re-authored both curves against real subterranean conditions
    // (`domains/species/src/lib.rs`): rust-monster's preferences sharpened
    // into genuine peaks, while xorn's stayed flat/indifferent (unfaked,
    // not weakened). Scored against the surface substrate that still governs
    // placement (chambers are not wired in yet, spec §6), the sharper
    // competitor now sweeps every MINERAL stronghold at seed 42 and xorn
    // holds none — a measured consequence of the re-authoring, not a
    // regression in the per-axis supply field this test otherwise pins.
    // ---- AND FALSIFIED AGAIN AT THE MERGE (The Tense, 2026-08-06). ----
    //
    // The Deep Realm's repair above does not survive this campaign's
    // productivity model, and the margin is exactly one settlement.
    //
    // Measured on the merged tree: rust-monster holds **1** dominant vertex and
    // xorn holds none. `MIN_SETTLEMENTS_FOR_DOMINANCE` is 2 — a kind topping
    // exactly one attractor is the Confluence campaign's denominator artifact,
    // measurement noise rather than placement — so neither specialist clears
    // the ruler. On main, where rust-monster's sharpened curves face the old
    // symmetric-tent productivity field, it clears; here the Lieth Miami model
    // lifts every BIOMASS-fed kind (giant-squid 1160, twig-blight 577) and the
    // mineral niche loses the margin it had.
    //
    // Neither campaign could have seen this alone. The Deep Realm re-authored
    // the curves against main's physics; The Tense replaced the physics without
    // touching the curves. This is precisely the semantic collision under a
    // clean merge that the preflight says it cannot score, and it is recorded
    // rather than tuned away — moving either the threshold or a niche constant
    // to recover one settlement would be a post-unblinding rescue.
    //
    // The STRUCTURAL claims are unaffected and still asserted above: the
    // peopled roster is unchanged, T2's dot product still differentiates more
    // dominants than the baseline, and the union clears the preregistered
    // floor. What is withdrawn is the per-kind prediction, for the second time.
    //
    // ---- AND IT IS BACK (The Radiation, C2d, 2026-08-10). ----
    //
    // The assertion above ended with an instruction: "rust-monster was ONE
    // settlement short; if it is back, re-read the comment above and establish
    // which productivity model is in play before flipping this." It is back,
    // at exactly 2 dominant vertices — the ruler's floor — so the instruction is
    // discharged here rather than the assertion quietly bumped.
    //
    // WHICH PRODUCTIVITY MODEL: unchanged. This campaign's diff touches
    // `domains/species`, `domains/language` and roster-facing test pins; it
    // does not touch `npp_temperature`, `mineral_supply_field`,
    // `axis_supply` or any part of The Tense's model. So the margin did not
    // move because the physics moved.
    //
    // WHAT DID MOVE: the competition. Six elves entered the contest for
    // attractors, and one of them (sea-elf) is the roster's first marine
    // PEOPLE, holding 34 dominant vertices of its own. Rust-monster regained
    // its one missing settlement out of that re-contest. The honest reading
    // is that this quantity has now been 1 -> 2 across a roster change with
    // no mechanism change, which makes it a **margin-of-one witness**: the
    // Demesne prediction it was written for is not confirmed by its return
    // any more than it was refuted by its absence. A campaign that needs
    // this claim should widen the seed sweep rather than read one seed's
    // ruler.
    //
    // ---- AND IT IS GONE AGAIN, AND THE ASSERTION IS WITHDRAWN UNDER 0097
    // (The Radiation, C2d task 6, 2026-08-10). ----
    //
    // The instruction the paragraph above left — "read this test's comment
    // before flipping it a third time, and establish which productivity model
    // is in play" — is discharged here, both halves, and the conclusion is
    // that this per-kind assertion should never have been in the gate.
    //
    // WHICH PRODUCTIVITY MODEL: still The Tense's, unchanged, and this time
    // checked against the diff rather than recalled. Against this branch's
    // merge-base with main, `domains/demography/` is untouched entirely and
    // every hunk in `windows/worldgen/src/lib.rs` falls inside `mod tests`.
    // `npp_temperature`, `mineral_supply_field`, `axis_supply` and
    // `forage_supply_field` have not moved. The margin did not move because
    // the physics moved — for the third campaign running.
    //
    // WHAT THE SEED SWEEP SAYS, which is the thing nobody had measured. Over
    // seeds 0..=23 (`dominant_settlement_counts`, same ruler, same
    // `MIN_SETTLEMENTS_FOR_DOMINANCE` of 2):
    //
    //   rust-monster clears the ruler on 18 of 24 seeds (75%)
    //   counts range 0..16, median 4; seed 42's 1 sits in the bottom sixth
    //   xorn holds ZERO dominant vertices on 24 of 24 seeds
    //
    // So the claim "the pure-MINERAL specialist clears the dominance ruler" is
    // TRUE of the world and FALSE of seed 42 about a quarter of the time. The
    // three flips this comment records (1 -> 2 -> 1, across The Tense, C2d and
    // C2d again) were never evidence about the mechanism; they are one world's
    // draw wandering across a bar of 2 in a distribution whose median is 4.
    //
    // That is exactly the shape ratified decision 0097 names — an EXISTENCE
    // CLAIM NEAR ITS THRESHOLD, carrying "a value pin's noise profile with an
    // invariant's authority" — and 0097's rule is that such a claim does not
    // belong in the commit gate at all, but is measured as a rate with a
    // sampling bound. The per-kind assertion is therefore WITHDRAWN rather
    // than flipped a fourth time. It is not relaxed and no threshold is
    // moved: 18/24 is reported, not asserted, and the follow-up to measure it
    // properly at census n is filed as `BIO-mineral-dominance-rate`.
    //
    // The STRUCTURAL claims are unaffected and still asserted above (the
    // peopled roster, T2's dot product differentiating more dominants than
    // baseline, the preregistered union floor). What is withdrawn is the
    // per-kind prediction — for the third time, and this time with a stated
    // rule for why it should not come back.
    println!("rust-monster dominant vertices at seed 42: {dominant_counts:?}");

    // The xorn half STAYS asserted, and it is a different kind of claim: the
    // same sweep measures xorn at ZERO dominant vertices on 24 of 24 seeds, so it
    // is not near any threshold and 0097's rule does not reach it. It was
    // previously a seed-42 point claim with no measured basis; it now has one.
    assert!(
        !material_dominants.contains("xorn"),
        "xorn cleared the dominance ruler ({dominant_counts:?}) — it has held none since \
         The Deep Realm sharpened rust-monster's curves and left xorn's flat, and it holds \
         none on any of seeds 0..=23; if this fires, that re-authoring is what to re-read."
    );
}

/// T3: THE K-GROUNDING CHECK (the-gathering discipline). The census-pinned
/// `capacity-by-abs-latitude` calibration (`gathering_calibration.rs`,
/// design spec §5) and its live seed-42 re-check
/// (`confluence.rs`'s `k_biomass_gradient_grounding_holds_after_the_
/// freshwater_repoint`) are both computed from `carrying_inputs_of` +
/// `species_carrying_input` + `hornvale_demography::carrying_capacity` — a
/// psychology-scaled, PEOPLED-ONLY carrying-capacity path that predates The
/// Niche's per-species differentiation and has never been re-pointed onto
/// it (settlement genesis moved onto `per_species_suitability` at Task A15a,
/// but this grounding metric stayed on the older, simpler path — the two
/// coexist, per the `species_carrying_input` doc comment). The per-axis
/// vector supply this campaign built (`mineral_supply_field`/
/// `forage_supply_field`/`DETRITUS_AMBIENT`) is consumed ONLY by
/// `per_species_suitability` (via `axis_supply`), so it cannot touch this
/// gradient's inputs at all — confirmed here, live, rather than assumed:
/// the measured ratio matches `confluence.rs`'s pinned 31.2563 exactly (T3
/// changed nothing upstream of it), so no `MINERAL_SUPPLY_SCALE`/
/// `FORAGE_FRACTION`/`CONDENSATION_THRESHOLD` re-fit is needed for THIS
/// metric. (T3's actual settlement-COUNT investigation — a different K,
/// `per_species_suitability`, the one settlement genesis and the menagerie
/// strongholds test use — lives in `confluence.rs`'s settlement-count test
/// and this file's `settlements_and_dominants_diversify_on_seed_42`.)
#[test]
fn k_biomass_gradient_grounding_is_unaffected_by_the_vector_supply() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = world_42();
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = hornvale_worldgen::climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();
    let base_inputs = carrying_inputs_of(geo, &terrain, &climate);

    let (mut trop_sum, mut trop_n, mut pole_sum, mut pole_n) = (0.0_f64, 0u32, 0.0_f64, 0u32);
    // Peoples-only carrying capacity (the settling roster); skip the minded
    // solitaries (a dragon carries a psyche but never settles) so the metric is
    // byte-identical to before The Eremite.
    for (kind, psych) in wc.psyche.iter() {
        if wc.biosphere.get(kind).map(|b| b.social_form)
            != Some(hornvale_species::SocialForm::Settled)
        {
            continue;
        }
        let inputs = hornvale_kernel::VertexMap::from_fn(geo, |c| {
            species_carrying_input(*base_inputs.get(c), psych)
        });
        let k = hornvale_demography::carrying_capacity(geo, &inputs);
        for vertex in geo.vertices() {
            if terrain.is_ocean(vertex) {
                continue;
            }
            let lat = geo.coord(vertex).latitude.abs();
            let kv = k.at(vertex);
            if lat < 30.0 {
                trop_sum += kv;
                trop_n += 1;
            } else if lat > 60.0 {
                pole_sum += kv;
                pole_n += 1;
            }
        }
    }
    assert!(trop_n > 0, "seed 42 has no tropical land vertices");
    assert!(pole_n > 0, "seed 42 has no polar land vertices");
    const POLE_FLOOR: f64 = 0.01;
    let trop_mean = trop_sum / f64::from(trop_n);
    let pole_mean = (pole_sum / f64::from(pole_n)).max(POLE_FLOOR);
    let ratio = trop_mean / pole_mean;
    // The decomposition is PRINTED, not just the ratio. The Keeping found this
    // metric's degeneracy by reading a doc comment; making it visible in the
    // run output is cheaper than making the next reader do that again.
    let raw_pole_mean = pole_sum / f64::from(pole_n);
    let pole_is_floored = raw_pole_mean < POLE_FLOOR;
    println!(
        "seed 42 capacity-by-abs-latitude: ratio={ratio:.4} \
         (trop_mean={trop_mean:.6} over {trop_n} vertices, raw_pole_mean={raw_pole_mean:.6} \
         over {pole_n} vertices, pole floored at {POLE_FLOOR}: {pole_is_floored})"
    );
    assert!(
        ratio >= 3.0,
        "capacity-by-abs-latitude on seed 42 fell to {ratio:.4} (below the preregistered floor \
         of 3) — the K-grounding may have drifted despite the-demesne touching a different K"
    );
    // Pinned to the merged-tree live reading: proof of ZERO drift from the
    // vector supply, not merely "still above the floor" — the vector supply's
    // code path (`per_species_suitability`/`axis_supply`) is disjoint from this
    // one (`carrying_inputs_of`/`species_carrying_input`/`carrying_capacity`),
    // so this ratio is BY CONSTRUCTION the pure scalar-path reading and the
    // vector supply cannot move it. The absolute value tracks the climate
    // inputs: it was 31.2563 pre-Rains (confluence.rs) and 31.1236 after
    // absorbing The Rains' moisture epoch (a 0.4% shift in the tropical/polar
    // NPP balance). **This loop sums the scalar K across every `Settled`
    // psyche kind** (`for (kind, psych) in wc.psyche.iter()`, filtered to
    // `Settled`), so a genuinely new competing Settled people is, by the
    // test's own construction, a new term in `trop_sum`/`pole_sum` — not
    // vector-supply contamination. The Vacancy T9 adds the gnoll (a hot-arid
    // desert people, contributing more to the tropical sum than the polar
    // one), moving the ratio to 30.8158 — still far above the preregistered
    // floor of 3.
    //
    // The Generalist re-pin (2026-08-03): human is a sixth Settled kind
    // (a temperate/subtropical generalist, per its own condition niche),
    // and by this test's own construction is a new term in `trop_sum`/
    // `pole_sum` — moving the ratio to 31.0099.
    // The Keeping step B re-pin (2026-08-04): `CarryingInput.habitable`
    // decomposed to `is_land`, so the arid and very-hot bands the old conflated
    // flag excluded outright now carry (low) scalar K — 31.0099 -> 31.0649.
    // The DIRECTION is the check that this is the intended mechanism and not
    // contamination: hot-and-arid ground is tropical/subtropical, never polar
    // (the poles stay closed by `temp_response`, zero below 2 C), so opening it
    // must add more to `trop_sum` than to `pole_sum` and the ratio must RISE.
    // It rose, by 0.18%. The preregistered floor of 3 still clears tenfold.
    //
    // ---- The Tense re-pin (2026-08-05): 31.0649 -> 35.4171, and the RATIO IS
    // ---- NOT A GRADIENT. Read this before touching the number again.
    //
    // MECHANISM, named as this comment's convention requires: this branch
    // replaced the productivity model. `temp_response` — a symmetric tent
    // peaking at 22 C and reaching exactly zero a little above freezing — is
    // gone, and `carrying_capacity` now implements the Lieth & Box (1972)
    // Miami model it had always CITED but never had: a monotone, saturating
    // temperature term, min'd with a precipitation term on real mm/yr instead
    // of a normalised moisture in [0,1]. That is The Keeping's headline defect
    // being repaired, motivated by decision 0106.
    //
    // THE DIRECTION CHECK CANNOT BE RUN, and that is the finding. Measured
    // here: raw_pole_mean = 0.004508, still BELOW `POLE_FLOOR`. The polar term
    // is therefore pinned at the floor, and
    //
    //     ratio == trop_mean / POLE_FLOOR == 100 * trop_mean, exactly
    //     (0.354171 * 100 = 35.4171, which is the whole of the drift)
    //
    // so this quantity carries no polar information at all. It is the tropical
    // mean in different units. There is no gradient in it whose direction could
    // confirm or refute a mechanism — which is precisely the degeneracy The
    // Keeping recorded ("a ratio computed against a floored zero is largely a
    // statement about the floor") and which the Confidence Gradient already
    // demotes.
    //
    // WHAT THIS ASSERTION IS, THEREFORE. It is a drift TRIPWIRE on scalar-path
    // productivity — an internal-consistency check on a Hornvale-internal
    // number, which decision 0106 rules a VALID use of internal measurement.
    // It is NOT evidence for the biomass-by-latitude gradient; treating it as
    // evidence would be 0106's CIRCULAR vertex, which names
    // `capacity-by-abs-latitude` explicitly. The preregistered floor of 3
    // above is the real surviving claim, and it clears tenfold.
    //
    // WHY THE POLES ARE STILL ~ZERO, given the tent that zeroed them is gone.
    //
    // >>> THE PARAGRAPH THAT FOLLOWS IS WRONG. It is kept verbatim, not
    // >>> deleted, because two campaigns inherited it and elaborated on it
    // >>> instead of re-deriving it, and the next reader should be able to see
    // >>> how the error survived. The correction, with the measurements that
    // >>> establish it, is in THE RADIATION RE-PIN block below. <<<
    //
    // Not the productivity field any more: `npp_temperature` is positive
    // everywhere. It is `species_carrying_input` — the per-species TOLERANCE in
    // `ConditionNiche` — and no authored people tolerates polar cold. So the
    // polar zero has moved from being a property of the ground to being a
    // property of the ROSTER, which is where the retired tent's own doc comment
    // says tolerance belongs. Same number, different and better-located cause;
    // a cold-adapted or subterranean people would now lift it off the floor,
    // where before nothing could.
    // THE DELVERS RE-PIN (C2c, 2026-08-07): 35.4171 -> 35.8831. The MECHANISM
    // is the roster, exactly as the paragraph above says it must be — this
    // number is a mean over the SETTLED peoples' per-species carrying
    // capacity, and the settling roster went from six to nine, so three new
    // tolerance curves entered the average. Nothing latitudinal moved.
    //
    // It read 36.0986 while the campaign carried five dwarves; withdrawing
    // Mountain and Duergar (spec §11) moved it to 35.8831 rather than back to
    // 35.4171, which is what a mean over a CHANGED population does — the two
    // kinds' contribution was never separable from the other three's.
    //
    // The paragraph above ends with a prediction: "a cold-adapted or
    // subterranean people would now lift [the poles] off the floor, where
    // before nothing could." That prediction is once again UNTESTED: the two
    // subterranean peoples that would have tested it are withdrawn, and while
    // they were present it did NOT come true (raw_pole_mean 0.004508 ->
    // 0.004574, still an order of magnitude under POLE_FLOOR = 0.01).
    // Recorded, not rescued: living underground is not the same axis as
    // tolerating polar cold. The degeneracy this assertion documents is
    // unchanged, and the ratio is still exactly 100 * trop_mean.
    //
    // THE RADIATION RE-PIN (C2d, 2026-08-10): 35.8831 -> 36.3288. The
    // MECHANISM is again the roster and only the roster — the settling
    // population went from nine to fifteen, so six more terms entered the
    // mean. Nothing latitudinal moved. **They are PSYCHE terms, not tolerance
    // curves**, which is where the paragraphs above go wrong; see the
    // correction below.
    //
    // WHAT THIS LOOP ACTUALLY READS, since two campaigns have now got it
    // wrong. Per vertex it reads `carrying_inputs_of(geo, terrain, climate)` —
    // a species-BLIND per-vertex record of land/temperature/precipitation/
    // freshwater/coast/hostility. Per species it reads exactly one thing:
    // `species_carrying_input(CarryingInput, &MindVector)`, whose signature
    // (`windows/worldgen/src/lib.rs`) contains no `ConditionNiche`, and whose
    // body touches `time_horizon` and `threat_response` and nothing else.
    // `ConditionNiche` and `BiomeAffinity` reach K only through
    // `per_species_suitability`, which this loop never calls — the same
    // disjointness this test's own header asserts for the vector supply.
    // **This metric cannot evaluate `tolerance_liebig` at all.**
    //
    // THE MOVEMENT, ATTRIBUTED ARITHMETICALLY rather than narrated. The pole
    // is floored, so ratio == 100 * trop_mean exactly, and trop_mean is the
    // unweighted mean of the per-kind tropical means (every kind contributes
    // the same 4380 tropical land vertices). Measured per kind on seed 42:
    //
    //   bugbear   0.344060   desert-dwarf 0.369867   gnoll      0.338622
    //   goblin    0.353578   gully-dwarf  0.364961   hill-dwarf 0.369628
    //   hobgoblin 0.354906   human        0.364780   kobold     0.369080
    //     -> mean over these NINE = 0.358831 -> 35.8831, the previous pin
    //
    //   desert-elf 0.369521  drow     0.370663  high-elf 0.372182
    //   sea-elf    0.367904  snow-elf 0.369703  wood-elf 0.369867
    //     -> mean over all FIFTEEN = 0.363288 -> 36.3288, this pin
    //
    // The nine-kind subset reproduces the old pin to every digit, so the six
    // new terms account for the whole movement and nothing else moved. The
    // direction is `time_horizon`, through `freshwater_factor = 0.5 +
    // time_horizon`: the elves carry 0.85-0.95 against a nine-kind spread of
    // 0.20-0.90, and a mean pulled toward the high end rises.
    //
    // ---- THE CORRECTION: WHY THE POLES ARE ~ZERO. It is the GROUND, not
    // ---- the roster, and never was the roster.
    //
    // Measured on seed 42 (probe run 2026-08-10, reverted): polar land
    // averages **T = -42.65 C** and 757.1 mm/yr over its 1855 vertices, so
    // inside `carrying_capacity` the species-blind Liebig minimum reads
    //
    //     npp_temperature(-42.65) = 0.001674
    //     npp_precipitation(757.1) = 0.395099      min = 0.001674
    //
    // — the temperature term, smaller by 236x. Running `carrying_capacity`
    // over the BASE inputs with no psyche folded in at all gives trop_mean
    // 0.353578 and pole_mean 0.004473: **the ground alone is already an order
    // of magnitude under POLE_FLOOR before any species exists.** Folding
    // psyche in moves each kind's polar mean only into 0.004366 (gnoll) ..
    // 0.004696 (drow), a +-4% modulation. goblin reproduces the ground
    // reading to every digit (0.004473 / 0.353578) because its psyche is
    // 0.50/0.50 and both factors are exactly 1.0.
    //
    // So the retired tent's zero did NOT relocate from the ground to the
    // roster. It stayed in the ground and changed shape — Lieth's saturating
    // curve is merely very small at -42 C rather than exactly zero at +2 C.
    //
    // ---- AND THE PREDICTION IS NOT UNTESTED. IT IS UNTESTABLE HERE.
    //
    // This campaign ships a genuinely COLD-ADAPTED people (snow-elf, 0.0 C
    // optimum) and a SUBTERRANEAN one (drow) — the two kinds the Tense
    // paragraph says would lift the poles. raw_pole_mean moved 0.004574 ->
    // 0.004589 and `pole floored: true`. That is not a third failure of a
    // live prediction; no authoring could have succeeded, because the only
    // per-species channel this metric has is psyche and psyche does not know
    // what a temperature is. FALSIFIED BY EXECUTION rather than by reading
    // (mutations run 2026-08-10, all reverted):
    //
    //   snow-elf elevation devotion 0.30 -> 0.90, i.e. authored ABOVE its
    //     0.435955 floor — the exact remedy an earlier draft of this comment
    //     prescribed                            -> 36.3288, byte-identical
    //   snow-elf temperature optimum 0.0 -> -40.0 C, width 14 -> 3,
    //     devotion 0.35 -> 0.99                 -> 36.3288, byte-identical
    //   snow-elf psyche time_horizon 0.88 -> 0.10 -> 36.0686, RED
    //
    // The positive control is what makes the two nulls mean anything: the
    // measurement is live and snow-elf IS in the loop. It simply cannot see a
    // niche. The same fact read from the other end: drow (0.004696) and
    // high-elf (0.004694) carry the two HIGHEST polar means in the roster,
    // while snow-elf, the only cold-adapted kind, sits mid-pack at 0.004650.
    // That ordering is `time_horizon`'s. It is not cold tolerance's.
    //
    // **Do not author a cold people to test the polar prediction** — it was
    // tried here and moved nothing. Testing it means re-pointing this loop
    // onto `per_species_suitability`, the path that does read `ConditionNiche`
    // and `BiomeAffinity`; that is a campaign, not an authoring change. And
    // the sentence "a people can be cold-adapted in the registry and not
    // cold-adapted in the field" may well be true of this commit, but **this
    // line is not the measurement that says so** and must not be cited as
    // one.
    // THE GLASSHOUSE re-pin (Stage B, decision 0134): 36.3288 -> 36.2088, and
    // **the identity in the two paragraphs above is no longer true**. Read the
    // printed decomposition rather than the arithmetic those paragraphs assume:
    // `raw_pole_mean` is now 0.011362 and `pole floored: false`, against `true`
    // at EVERY prior reading in this comment's history. The polar term has come
    // off `POLE_FLOOR` for the first time, so `ratio` is a genuine
    // tropics/poles ratio (0.411404 / 0.011362 = 36.2088) and NOT
    // `100 * trop_mean` — that equality held only while the denominator was the
    // constant. Anyone re-deriving this number from `trop_mean` alone will get
    // 41.14 and conclude something is broken.
    //
    // The mechanism is the terrain epoch. The craton rescale delivers its
    // budget, so the coastline sits at the shelf break; the land mask grows and
    // mean land elevation falls 2257 -> 1783 m. Lower, warmer polar land is
    // what lifts `raw_pole_mean` 0.004589 -> 0.011362 — which is, incidentally,
    // the movement the withdrawn-paragraph above predicted a cold-adapted
    // PEOPLE would produce and which no roster change ever did. It came from
    // the ground, exactly where that paragraph said it no longer lived. Do not
    // read this as vindication of the roster mechanism; it is the opposite.
    //
    // The degeneracy this assertion documents is therefore REDUCED, not gone,
    // and it stays a drift tripwire on a Hornvale-internal number (0106's valid
    // use), not evidence for the biomass-by-latitude gradient.
    //
    // THE GLASSHOUSE re-pin (Stage B Task 4): 36.2088 -> 7.7803. The
    // thermostat (a damped, greenhouse-forced insolation baseline replacing
    // the fixed 288 K blackbody one) warms polar land far more than
    // tropical land moves: `raw_pole_mean` rises 0.011362 -> 0.061245 while
    // `trop_mean` moves only 0.411404 -> 0.476504, so the tropics/poles
    // ratio compresses sharply. The pole term is still off `POLE_FLOOR`
    // (`pole floored: false`), so this remains a genuine ratio, not the
    // constant-denominator degeneracy the paragraphs above retire. Measured
    // against the OLD +30/-30 latitude profile (Task 5 had not yet landed).
    //
    // THE GLASSHOUSE re-pin (Stage B Task 5): 7.7803 -> 12.2953. The
    // area-mean-zero latitude profile corrects the old profile's
    // unrealistically hot equator (+44 C at the Earth anchor -> +26 C, spec
    // §3.2) and its milder pole (-15 C -> -25 C at the anchor), cooling BOTH
    // ends but the pole harder in absolute terms on this land distribution:
    // `raw_pole_mean` falls 0.061245 -> 0.035406 while `trop_mean` falls
    // less, 0.476504 -> 0.435333, so the ratio widens again. Still off
    // `POLE_FLOOR` (`pole floored: false`), so still a genuine ratio.
    // Post-unblinding re-measure, declared per decision 0016.
    //
    // THE GLASSHOUSE re-pin (Stage B, `k` re-decided 0.4 -> 0.3): 12.2953 ->
    // 10.1472, and this is the first movement in this line's history where
    // the ratio NARROWS because both ends rose. A smaller residual fraction
    // compensates more of seed 42's insolation shortfall, so the whole world
    // warms — but the two ends do not warm equally in effect:
    //   trop_mean     0.435333 -> 0.450636   (+3.5%, 68565 vertices)
    //   raw_pole_mean 0.035406 -> 0.044410  (+25.4%,  7920 vertices)
    // Warming a near-unproductive pole buys far more proportional
    // productivity than warming an already-productive tropics, which is
    // ordinary saturation and not a latitudinal mechanism moving. The
    // assertion's own message tells its reader to check the decomposition
    // before assuming anything latitudinal moved; the decomposition is why
    // this re-pin is a narrowing rather than a defect. `pole floored: false`
    // still holds, so this remains a genuine tropics/poles ratio and not
    // `100 * trop_mean` in disguise — which is the failure mode this line
    // has to keep proving it is not. Post-unblinding re-measure, declared
    // per decision 0016.
    assert!(
        (ratio - 10.1472).abs() < 1e-3,
        "scalar-path productivity drifted: {ratio:.4} (expected ~10.1472). Check the \
         printed decomposition above before assuming anything latitudinal moved — and \
         note that since The Glasshouse the polar term is OFF its floor, so this is a \
         real tropics/poles ratio and no longer 100 * trop_mean."
    );
}

// T3: THE EPOCH SURFACE. Settlement placement is deterministic OVER K —
// `hornvale_demography` (condensation, the coexistence-stack packer) draws
// no `Seed`/`Stream` (grep confirms it; see `confluence.rs`'s byte-identity
// test doc, which established this same fact for The Confluence's
// freshwater re-point), and neither does anything T1/T2/T3 added here
// (`mineral_supply_field`/`forage_supply_field`/`axis_supply`/
// `per_species_suitability` are pure functions of terrain/climate/biosphere —
// no `Seed`, no `Stream`, no RNG). The per-axis vector supply changes WHICH
// vertices a species' K peaks in (a derived-FORMULA change), never adds or
// reorders a seed draw, so the settlement seed-derivation's
// stream-consumption order is unchanged. Confirmed directly (not just
// argued): the generated stream manifest (`cargo run -p hornvale --
// streams`) is byte-identical to the committed
// `book/src/reference/stream-manifest-generated.md` after this campaign's
// changes. This is a save-format-relevant DERIVED-FORMULA change (spec §6)
// — not a stream-label epoch; no `settlement/*` label gets an epoch
// suffix. `seed_42_is_byte_identical_across_two_builds_after_the_demesne`
// below is the direct determinism assertion.

/// T3: THE BYTE-IDENTITY CHECK. Same seed + pins must still produce a
/// byte-identical world under the-demesne's re-pointed resource-supply
/// term — mirrors `confluence.rs`'s
/// `seed_42_is_byte_identical_across_two_builds_after_the_confluence`,
/// scoped to the crate this campaign actually touched.
#[test]
fn seed_42_is_byte_identical_across_two_builds_after_the_demesne() {
    let build = || {
        build_world(
            Seed(42),
            &SkyPins::default(),
            SkyChoice::Generated,
            &TerrainPins::default(),
            &SettlementPins::default(),
        )
        .unwrap()
        .to_json()
    };
    let a = build();
    let b = build();
    assert_eq!(
        a, b,
        "same seed + pins must yield a byte-identical world under the-demesne's per-axis \
         vector resource supply"
    );
}
