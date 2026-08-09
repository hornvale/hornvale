//! THE RANGE: the sparse biome-affinity store. Absence means unrestricted.

use hornvale_species::{BiomeAffinity, biome_affinity_registry};

#[test]
fn an_unlisted_biome_takes_the_default() {
    let a = BiomeAffinity {
        default: 0.25,
        by_biome: vec![("desert", 1.0)],
    };
    assert_eq!(a.factor("desert"), 1.0);
    assert_eq!(a.factor("temperate-forest"), 0.25);
}

/// A UNIFORM affinity is a no-op for placement: genesis and `best_home` rank
/// cells in the kind's OWN units, so a constant factor cannot reorder anything
/// (spec §3.1, task 0). Only the SHAPE across biomes matters. Asserted rather
/// than trusted, because an all-equal authored row is inert by construction and
/// would otherwise look like a working declaration.
#[test]
fn a_uniform_affinity_is_flat_across_every_biome() {
    let a = BiomeAffinity {
        default: 0.5,
        by_biome: vec![("desert", 0.5), ("tundra", 0.5)],
    };
    for b in ["desert", "tundra", "epipelagic", "alpine"] {
        assert_eq!(a.factor(b), 0.5, "uniform affinity must not vary at {b}");
    }
}

#[test]
fn the_registry_ships_empty_in_this_task() {
    assert_eq!(biome_affinity_registry().len(), 0);
}
