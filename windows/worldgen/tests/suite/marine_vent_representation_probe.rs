//! The Tidemark, M1: do the world's two vent representations coincide?
//!
//! There are two today. `hornvale_climate::Biome::HydrothermalVent` is
//! derived — `classify_marine` reads it straight off
//! `SeafloorFeature::Ridge`, no draw — and it is the one the live suitability
//! path already consumes, through
//! `hornvale_worldgen::marine_chemosynthate_supply_field`. Worldgen's
//! `WaterVent` is a seeded admission (threshold 0.25) over seabed vertices
//! that carry an edifice or a plate boundary, and it is the only one of the
//! two carrying source identity, strength and a succession phase.
//!
//! M1 counts `|A|` (vertices whose biome is `HydrothermalVent`), `|B|`
//! (vertices hosting a `WaterVent`) and `|A ∩ B|` at seeds 42, 7 and 3, and
//! preregisters both poles: a small intersection means two different
//! phenomena and seating reads `WaterVent`; a large one means a redundant
//! duplication, which is a finding larger than this campaign.
//!
//! Kept as an `#[ignore]`d probe rather than a gate: it is a measurement
//! whose answer is recorded in the campaign ledger, and it builds three
//! worlds to terrain depth to take it. Its assertions are the
//! preregistration's own prediction, so running it by hand re-checks the
//! ruling rather than merely reprinting numbers.
//!
//! Test fixture (decision 0092): calls the sculpt/fit derivation entry
//! points directly to build its own world state, copied rather than shared.
#![allow(clippy::disallowed_methods)]

use std::collections::BTreeSet;

use hornvale_astronomy::SkyPins;
use hornvale_kernel::Seed;
use hornvale_terrain::TerrainPins;
use hornvale_worldgen::{
    BuildDepth, SettlementPins, WaterWorldConfig, WorldComponents, build_world_to_with_artifacts,
    climate_from, waterworld_from,
};

/// `(|A|, |B|, |A ∩ B|)` at one seed: biome-vent vertices, `WaterVent`
/// vertices, and the vertices carrying both.
fn counts_at(seed: Seed) -> (usize, usize, usize) {
    let wc = WorldComponents::assemble().expect("the shipped component roster assembles");
    let built = build_world_to_with_artifacts(
        seed,
        &SkyPins::default(),
        &TerrainPins::default(),
        &SettlementPins::default(),
        &wc,
        BuildDepth::Terrain,
    )
    .expect("the terrain-depth fixture builds");
    let terrain = built.terrain.expect("terrain depth returns terrain");
    let climate = climate_from(&built.world, &terrain).expect("climate derives from terrain");
    let geo = terrain.geosphere();

    let biome = climate.biome_map();
    let biome_vents: BTreeSet<hornvale_kernel::Vertex> = geo
        .vertices()
        .filter(|&v| *biome.get(v) == hornvale_climate::Biome::HydrothermalVent)
        .collect();

    let overlay = waterworld_from(
        &built.world,
        &terrain,
        &climate,
        WaterWorldConfig { enabled: true },
    );
    let water_vents: BTreeSet<hornvale_kernel::Vertex> =
        overlay.vents.iter().map(|vent| vent.vertex).collect();

    let intersection = biome_vents.intersection(&water_vents).count();
    (biome_vents.len(), water_vents.len(), intersection)
}

/// claim: readout(preregistered) — off-gate, three seeds (42, 7, 3), prints
/// `|A|`, `|B|` and `|A ∩ B|` and asserts only spec §8's M1 prediction, that
/// the intersection is under half the smaller set. The seed loop is a
/// measurement panel, not a quantifier over all worlds: a fourth seed that
/// disagreed would be a finding to report, not a bug to fix.
#[test]
#[ignore = "probe: M1, do the two vent representations coincide (three terrain-depth world builds); run by hand (The Tidemark, Task 2)"]
fn the_two_vent_representations_do_not_coincide() {
    for seed in [42_u64, 7, 3] {
        let (a, b, both) = counts_at(Seed(seed));
        println!(
            "seed {seed}: |HydrothermalVent|={a} |WaterVent|={b} |intersection|={both} \
             (smaller set = {})",
            a.min(b)
        );
        assert!(
            a > 0 && b > 0,
            "seed {seed}: both representations must be non-empty for the comparison to say \
             anything — |A|={a}, |B|={b}"
        );
        assert!(
            2 * both < a.min(b),
            "seed {seed}: M1 predicted the intersection to be under half the smaller set; got \
             |A|={a}, |B|={b}, |A∩B|={both}. A large overlap is the campaign's headline finding \
             (two redundant representations of one phenomenon), not a number to absorb"
        );
    }
}
