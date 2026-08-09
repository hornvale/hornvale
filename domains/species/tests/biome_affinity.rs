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
///
/// **Strengthened (Task 3, from Task 2's review):** as first shipped, every
/// fixture value here was `0.5`, so a `factor()` that ignored `by_biome`
/// entirely and always returned `self.default` would still have passed. The
/// property this test means to assert — "a uniform affinity is flat" — is
/// only meaningful next to proof that a NON-uniform one is demonstrably not
/// flat, so this now asserts both directions against the same biome set.
#[test]
fn a_uniform_affinity_is_flat_across_every_biome() {
    let uniform = BiomeAffinity {
        default: 0.5,
        by_biome: vec![("desert", 0.5), ("tundra", 0.5)],
    };
    for b in ["desert", "tundra", "epipelagic", "alpine"] {
        assert_eq!(
            uniform.factor(b),
            0.5,
            "uniform affinity must not vary at {b}"
        );
    }

    // The companion NON-uniform fixture: same biome names, `by_biome` values
    // that actually differ from `default`. If a `factor()` implementation
    // ignored `by_biome` and always returned `self.default`, this fixture's
    // values would collapse to one constant too — the uniform assertion
    // above would then be proving nothing about `factor()` itself.
    let non_uniform = BiomeAffinity {
        default: 0.5,
        by_biome: vec![("desert", 1.0), ("tundra", 0.1)],
    };
    assert_eq!(non_uniform.factor("desert"), 1.0, "listed override: desert");
    assert_eq!(non_uniform.factor("tundra"), 0.1, "listed override: tundra");
    assert_eq!(
        non_uniform.factor("epipelagic"),
        0.5,
        "an unlisted biome takes default"
    );
    let distinct_values: std::collections::BTreeSet<u64> =
        ["desert", "tundra", "epipelagic", "alpine"]
            .iter()
            .map(|b| non_uniform.factor(b).to_bits())
            .collect();
    assert!(
        distinct_values.len() > 1,
        "the non-uniform fixture must actually vary across biomes, or it cannot \
         discriminate a stub `factor()` that always returns `self.default` — \
         exactly what made the all-0.5 fixture weak"
    );
}

/// Task 2 shipped this as `the_registry_ships_empty_in_this_task`, asserting a
/// length of `0` — the store existed and resolved but nobody was in it, which
/// was the whole point of that task's byte-neutrality claim. **Task 4 is the
/// task that makes it false on purpose.**
///
/// It is rewritten rather than deleted, because "who is in this registry" is a
/// fact worth an assertion in either direction: an occupant appearing (or
/// vanishing) is a deliberate act with a measured world-diff behind it, and it
/// should not be possible to do silently. The admission test each occupant had
/// to pass is enforced separately, at
/// `windows/worldgen/tests/range_readout.rs::every_occupant_has_climate_curves_the_minimum_currently_discards`
/// — it needs the kernel's `sovereignty_floor` against the biosphere store, so
/// it cannot live in this domain-local file.
#[test]
fn the_registry_ships_exactly_the_declared_occupants() {
    let registry = biome_affinity_registry();
    let occupants: Vec<&str> = registry.ids().map(|k| k.0).collect();
    assert_eq!(
        occupants,
        vec!["gnoll", "woolly-mammoth"],
        "The Range task 4 declares exactly two occupants: gnoll (Desert) and \
         woolly-mammoth (Tundra/Ice). Adding or removing one moves every world, \
         so it belongs in a commit that says so."
    );
}
