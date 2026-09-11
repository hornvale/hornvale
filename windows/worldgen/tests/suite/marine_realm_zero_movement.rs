//! The Tidemark, Task 1, M4: introducing `HabitatRealm::Marine` and its two
//! explicit `Surface` rows (sea-elf, giant-crocodile) must move nothing.
//!
//! **A two-arm test in one run, not a before/after across the code change**
//! (the plan's own correction — `availability` is internal to
//! `per_species_suitability_masked` and is never returned, so it cannot be
//! observed directly; the observable is `per_species_suitability`'s returned
//! `Vec<(u32, VertexMap<f64>)>`). `species_realm` is a caller-supplied slice,
//! so one run scores sea-elf and giant-crocodile two ways against the SAME
//! built world: once under the realm vector `WorldComponents::habitat_realm`
//! actually yields (which now carries explicit `Surface` rows for both,
//! spec §3.6), and once under every kind forced to `Surface` regardless of
//! what the registry says — the pre-campaign behaviour these two kinds fell
//! into by absence. Follows `deep_realm_rehome.rs`'s `k_live` vs
//! `k_surface_forced` idiom (decision 0092's test-fixture posture: build a
//! world once, call the live entry point directly, copy rather than share).
//!
//! **Why equality here is a real test and not a tautology dressed up as
//! one.** Both rows already resolve to `Surface` today, so the two arms
//! start identical — but the EQUALITY is what stays true, not the
//! resolution. A regression that quietly reclassified sea-elf or
//! giant-crocodile as `Marine` (the exact hazard spec §3.6 names: "a reader
//! meeting a new `Marine` variant will reasonably assume a sea elf belongs
//! to it") would zero their live-arm suitability everywhere via `availability
//! = 0.0` (pre-flight ruling P1) while the forced arm stayed unchanged — so
//! this keeps working as a permanent guard long after this task closes, not
//! only as a one-off measurement of the moment the variant was introduced.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, once per test — the
//! sanctioned test-fixture posture, reused from `deep_realm_rehome.rs`.
#![allow(clippy::disallowed_methods)]

use hornvale_astronomy::SkyPins;
use hornvale_kernel::{Seed, VertexMap};
use hornvale_species::HabitatRealm;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WorldComponents, build_world_to, climate_of,
    per_species_suitability, terrain_of,
};

/// The `(names, realm slice)` construction `deep_realm_rehome.rs`'s
/// `realm_slice` performs — copied rather than shared (decision 0092's
/// test-fixture posture, the same one that file's header invokes).
fn realm_slice(wc: &WorldComponents) -> Vec<HabitatRealm> {
    wc.biosphere
        .iter()
        .map(|(kind, _)| {
            wc.habitat_realm
                .get(kind)
                .copied()
                .unwrap_or(HabitatRealm::SURFACE)
        })
        .collect()
}

/// Count vertices at which `map` reads strictly positive — the "how much of
/// the globe does this kind hold at all" reading M4 asks for.
fn count_nonzero(geo: &hornvale_kernel::Geosphere, map: &VertexMap<f64>) -> usize {
    geo.vertices().filter(|&v| *map.get(v) > 0.0).count()
}

#[test]
fn marine_realm_introduction_moves_sea_elf_and_giant_crocodile_nowhere() {
    let wc = WorldComponents::assemble().expect("canonical registries are well-formed");
    let world = build_world_to(
        Seed(42),
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("seed 42 builds");
    let terrain = terrain_of(&world).expect("terrain reconstructs");
    let climate = climate_of(&world).expect("climate reconstructs");
    let geo = terrain.geosphere();

    let bio: Vec<&hornvale_species::BiosphereTraits> =
        wc.biosphere.iter().map(|(_, b)| b).collect();
    let names: Vec<&'static str> = wc.biosphere.ids().map(|k| k.0).collect();

    // Arm 1: the realm vector the registry actually yields today — carries
    // sea-elf and giant-crocodile's new explicit `Surface` rows.
    let realm_live = realm_slice(&wc);
    // Arm 2: every kind forced to `Surface`, independent of what the
    // registry says — the control, and (for these two kinds specifically)
    // the pre-campaign behaviour absence used to produce.
    let realm_surface_forced: Vec<HabitatRealm> = vec![HabitatRealm::Surface; bio.len()];
    // The Range's control (see `deep_realm_rehome.rs`): an all-`None`
    // affinity slice isolates the realm question from the biome-affinity
    // one. Neither sea-elf nor giant-crocodile needs its own real affinity
    // row measured here — both arms hold it equal, so the comparison below
    // is unaffected either way.
    let none_affinity: Vec<Option<hornvale_species::BiomeAffinity>> = vec![None; bio.len()];

    let obliquity_deg = climate.obliquity_deg();
    let insolation_scalar = climate.insolation();
    let regime = climate.regime();

    let k_live = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm_live,
        &none_affinity,
    );
    let k_surface_forced = per_species_suitability(
        geo,
        &terrain,
        &climate,
        obliquity_deg,
        insolation_scalar,
        &regime,
        &bio,
        &realm_surface_forced,
        &none_affinity,
    );

    for label in ["sea-elf", "giant-crocodile"] {
        let tag = names
            .iter()
            .position(|n| *n == label)
            .unwrap_or_else(|| panic!("{label} missing from the biosphere roster"))
            as u32;
        let live_map = &k_live.iter().find(|(t, _)| *t == tag).unwrap().1;
        let forced_map = &k_surface_forced.iter().find(|(t, _)| *t == tag).unwrap().1;

        let live_count = count_nonzero(geo, live_map);
        let forced_count = count_nonzero(geo, forced_map);
        println!(
            "{label}: live_nonzero={live_count} surface_forced_nonzero={forced_count} \
             (of {} vertices)",
            geo.vertices().count()
        );

        assert_eq!(
            live_count, forced_count,
            "{label}'s non-zero-suitability vertex count must be identical between the live \
             registry realm and a realm forced to Surface — a difference means the Marine \
             variant's introduction silently reclassified a shipped kind, which spec §3.6 \
             requires be resolved before this task closes, not merely noted"
        );

        // Stronger than the count alone: the two arms must agree AT EVERY
        // VERTEX, bit for bit, not merely on how many are non-zero — "no
        // world moves" is a per-vertex claim, and a count match alone could
        // hide a permutation.
        for vertex in geo.vertices() {
            let live = *live_map.get(vertex);
            let forced = *forced_map.get(vertex);
            assert_eq!(
                live.to_bits(),
                forced.to_bits(),
                "{label} at {vertex:?}: live={live} forced={forced} — expected bit-identical \
                 suitability, since both arms score this kind against `Surface`"
            );
        }
    }
}
