//! The descriptor grammar (P3): ordered weighted-choice slots, conditioned on
//! biome / substrate / micro-field, assembled to prose. Pools are the
//! authoring-time artifact (decision 0009); this is the first complete set.

use crate::regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};
use crate::streams::{LOCALE_SUBSTRATE_DETAIL, LOCALE_VARIETY};
use hornvale_climate::{BiomeExpr, Formation, Stratum};
use hornvale_kernel::seed::StreamLabel;
use hornvale_kernel::{RoomAddr, Seed};

/// A weighted descriptor pool.
type Pool = &'static [(f64, &'static str)];

/// The derived overlay for a room (substrate-only negation; mundane energy/kingdom).
pub(crate) fn derived_regime(
    seed: Seed,
    addr: &RoomAddr,
    expr: BiomeExpr,
    substrate: Substrate,
    micro: MicroField,
) -> Regime {
    let negations = Negations {
        substrate,
        energy: EnergySource::Sunlit,
        kingdom: Kingdom::PlantAnimal,
        endemic: false,
    };
    let descriptor = render(negations, micro, expr, seed, addr);
    Regime {
        negations,
        micro,
        descriptor,
        strangeness: negations.strangeness(),
    }
}

/// Render a descriptor for any negation vector (used by both tiers).
pub(crate) fn render(
    negations: Negations,
    micro: MicroField,
    expr: BiomeExpr,
    seed: Seed,
    addr: &RoomAddr,
) -> String {
    let room = addr.seed(seed);
    let variety = draw(
        room,
        LOCALE_VARIETY,
        variety_pool(expr.formation, expr.stratum, negations.substrate),
    );
    let substrate_detail = draw(
        room,
        LOCALE_SUBSTRATE_DETAIL,
        substrate_pool(negations.substrate),
    );
    let habitat = micro_habitat(micro, expr);
    let exotic = exotic_clause(negations);
    // Assemble, dropping empty clauses.
    [variety, substrate_detail, habitat, exotic]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Draw one entry from a pool off a per-slot stream; "" if the pool is empty.
fn draw(room: Seed, label: StreamLabel<'_>, pool: Pool) -> String {
    if pool.is_empty() {
        return String::new();
    }
    let weights: Vec<f64> = pool.iter().map(|(w, _)| *w).collect();
    let i = room
        .derive(label)
        .stream()
        .weighted_index(&weights)
        .unwrap_or(0);
    pool[i].1.to_string()
}

/// The micro-habitat clause reads the MicroField deterministically (no draw).
fn micro_habitat(micro: MicroField, expr: BiomeExpr) -> String {
    let _ = expr;
    let relief = if micro.relief > 0.33 {
        "on a rise"
    } else if micro.relief < -0.33 {
        "in a hollow"
    } else {
        ""
    };
    let aspect = if micro.aspect > 0.33 {
        "sun-warmed"
    } else if micro.aspect < -0.33 {
        "shaded"
    } else {
        ""
    };
    let wet = if micro.wetness > 0.33 {
        "damp"
    } else if micro.wetness < -0.33 {
        "dry"
    } else {
        ""
    };
    [aspect, wet, relief]
        .into_iter()
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Exotic clause for placed regimes (empty for the derived tier).
pub(crate) fn exotic_clause(n: Negations) -> String {
    let mut parts = Vec::new();
    match n.energy {
        EnergySource::Chemosynthetic => parts.push("fed by cold seeps"),
        EnergySource::Geothermal => parts.push("warmed from below"),
        EnergySource::Sunlit => {}
    }
    match n.kingdom {
        Kingdom::Fungal => parts.push("under a fungal canopy"),
        Kingdom::Crystalline => parts.push("grown with mineral crystal"),
        Kingdom::Microbial => parts.push("crusted with microbial mats"),
        Kingdom::PlantAnimal => {}
    }
    if n.endemic {
        parts.push("with biota found nowhere else");
    }
    parts.join(", ")
}

/// Base-variety pool per biome (+ substrate for deserts). Real content drawn
/// from cycle-02 Appendix A; extend as authoring amplifies (decision 0009).
fn variety_pool(formation: Formation, stratum: Stratum, substrate: Substrate) -> Pool {
    let _ = stratum;
    match (formation, substrate) {
        (Formation::Desert, Substrate::Sand) => &[(3.0, "erg dunes"), (2.0, "a nabkha field")],
        (Formation::Desert, Substrate::Evaporite) => {
            &[(3.0, "a cracked playa"), (2.0, "a salt pan")]
        }
        (Formation::Desert, Substrate::Basaltic) => &[(3.0, "a hamada of bare rock")],
        (Formation::Desert, _) => &[
            (3.0, "a reg of wind-swept gravel"),
            (2.0, "a yardang field"),
        ],
        (Formation::TemperateForest | Formation::TemperateRainforest, _) => &[
            (3.0, "old-growth timber"),
            (3.0, "dense understory"),
            (2.0, "a mossy hollow"),
            (2.0, "a windthrow gap"),
            (2.0, "a fern-choked draw"),
            (2.0, "a lichen-hung grove"),
            (1.0, "a deadfall tangle"),
            (1.0, "a shaft of clear light"),
        ],
        (Formation::Taiga, _) => &[
            (3.0, "a boreal stand"),
            (2.0, "a peat hollow"),
            (1.0, "a burnt snag"),
        ],
        (Formation::Tundra | Formation::Alpine, _) => &[
            (3.0, "frost-heaved ground"),
            (2.0, "a boulder field"),
            (2.0, "wind scour"),
        ],
        (Formation::Savanna | Formation::TemperateGrassland, _) => {
            &[(3.0, "open sward"), (2.0, "a scattered copse")]
        }
        (Formation::TropicalRainforest | Formation::TropicalSeasonalForest, _) => &[
            (3.0, "buttressed canopy"),
            (2.0, "a liana tangle"),
            (2.0, "a stream gully"),
        ],
        _ => &[(2.0, "broken terrain"), (2.0, "unremarkable ground")],
    }
}

/// Substrate-detail clause pool.
fn substrate_pool(substrate: Substrate) -> Pool {
    match substrate {
        Substrate::Ordinary => &[],
        Substrate::Sand => &[(1.0, "of shifting sand")],
        Substrate::Evaporite => &[(1.0, "of salt-white crust")],
        Substrate::Basaltic => &[(1.0, "of black basalt")],
        Substrate::Ashen => &[(1.0, "of drifted ash")],
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_climate::Biome;
    use hornvale_climate::BiomeExpr;
    use hornvale_kernel::{RoomAddr, Seed};

    fn micro0() -> MicroField {
        MicroField {
            relief: 0.0,
            aspect: 0.0,
            wetness: 0.0,
            openness: 0.0,
        }
    }

    #[test]
    fn derived_is_deterministic() {
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let a = derived_regime(
            Seed(42),
            &addr,
            BiomeExpr::for_legacy(Biome::Desert),
            Substrate::Sand,
            micro0(),
        );
        let b = derived_regime(
            Seed(42),
            &addr,
            BiomeExpr::for_legacy(Biome::Desert),
            Substrate::Sand,
            micro0(),
        );
        assert_eq!(a, b);
    }

    #[test]
    fn derived_tier_is_never_exotic() {
        // The derived tier only negates substrate; energy/kingdom stay mundane.
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let r = derived_regime(
            Seed(42),
            &addr,
            BiomeExpr::for_legacy(Biome::Desert),
            Substrate::Sand,
            micro0(),
        );
        assert_eq!(r.negations.energy, EnergySource::Sunlit);
        assert_eq!(r.negations.kingdom, Kingdom::PlantAnimal);
        assert!(r.strangeness <= 15.0);
        assert!(!r.descriptor.is_empty());
    }

    #[test]
    fn adjacent_rooms_in_one_biome_differ() {
        // The "miles and miles of forest" guard: sibling forest rooms with the
        // same biome + substrate still produce distinguishable descriptors.
        let biome = Biome::TemperateForest;
        let mut seen = std::collections::BTreeSet::new();
        for last in 0..4u8 {
            let addr = RoomAddr {
                face: 3,
                path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, last],
            };
            let micro = crate::micro::micro_field(addr.seed(Seed(42)));
            let r = derived_regime(
                Seed(42),
                &addr,
                BiomeExpr::for_legacy(biome),
                Substrate::Ordinary,
                micro,
            );
            seen.insert(r.descriptor);
        }
        assert!(
            seen.len() >= 3,
            "adjacent forest rooms should mostly differ, got {seen:?}"
        );
    }

    /// The pre-campaign land descriptors, captured from the code as it stood
    /// before the re-key. The re-key must be invisible: matching on
    /// `Formation` instead of `Biome` may not change a single land draw.
    const LEGACY_LAND: &[(Biome, &str)] = &[
        (Biome::Tundra, "frost-heaved ground"),
        (Biome::Taiga, "a boreal stand"),
        (Biome::TemperateGrassland, "open sward"),
        (Biome::TemperateForest, "dense understory"),
        (Biome::TemperateRainforest, "dense understory"),
        (Biome::Desert, "a reg of wind-swept gravel"),
        (Biome::Savanna, "open sward"),
        (Biome::TropicalSeasonalForest, "buttressed canopy"),
        (Biome::TropicalRainforest, "buttressed canopy"),
        (Biome::Alpine, "frost-heaved ground"),
    ];

    #[test]
    fn re_keying_the_pool_leaves_every_land_descriptor_untouched() {
        let addr = RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        };
        let n = Negations {
            substrate: Substrate::Ordinary,
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::PlantAnimal,
            endemic: false,
        };
        for (biome, expected) in LEGACY_LAND {
            let got = render(n, micro0(), BiomeExpr::for_legacy(*biome), Seed(42), &addr);
            assert_eq!(&got, expected, "{biome:?} moved under the re-key");
        }
    }
}
