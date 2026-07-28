//! The descriptor grammar (P3): ordered weighted-choice slots, conditioned on
//! biome / substrate / micro-field, assembled to prose. Pools are the
//! authoring-time artifact (decision 0009); this is the first complete set.

use crate::regime::{EnergySource, Kingdom, MicroField, Negations, Regime, Substrate};
use crate::streams::{LOCALE_SUBSTRATE_DETAIL, LOCALE_VARIETY};
use hornvale_climate::{BiomeExpr, Formation, Medium, Stratum};
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
    match expr.realm.medium {
        Medium::AirOverRock => land_micro_habitat(micro),
        Medium::Water => water_micro_habitat(micro, expr.stratum),
    }
}

/// The overworld's habitat clause — the original body, unchanged.
fn land_micro_habitat(micro: MicroField) -> String {
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

/// The water column's habitat clause: the same micro-field, read as water
/// reads it.
///
/// Relief becomes the floor beneath rather than the ground underfoot; aspect
/// becomes light, and only where light arrives — below the sunlit water,
/// nothing is sun-warmed or shaded, it is simply dark; and wetness, which
/// means nothing in the sea, becomes the set of the current.
fn water_micro_habitat(micro: MicroField, stratum: Stratum) -> String {
    let relief = if micro.relief > 0.33 {
        "over a seamount"
    } else if micro.relief < -0.33 {
        "over a trough"
    } else {
        ""
    };
    let light = if matches!(stratum, Stratum::Epipelagic | Stratum::Surface) {
        if micro.aspect > 0.33 {
            "sunlit"
        } else if micro.aspect < -0.33 {
            "in blue shadow"
        } else {
            ""
        }
    } else {
        ""
    };
    let current = if micro.wetness > 0.33 {
        "swept by a current"
    } else if micro.wetness < -0.33 {
        "in slack water"
    } else {
        ""
    };
    [light, current, relief]
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
        (Formation::Ice, _) => &[
            (3.0, "a snowfield"),
            (2.0, "a crevasse field"),
            (2.0, "wind-carved sastrugi"),
            (1.0, "blue ice, swept bare"),
        ],
        (Formation::Shrubland, _) => &[
            (3.0, "thorn scrub"),
            (2.0, "a chaparral slope"),
            (2.0, "matorral, low and grey"),
            (1.0, "a burnt-over thicket"),
        ],
        (Formation::SeaIce, _) => &[
            (3.0, "a pressure ridge"),
            (2.0, "a lead of open water"),
            (2.0, "rafted floe"),
            (1.0, "a melt pond"),
        ],
        (Formation::Reef, _) => &[
            (3.0, "a coral head"),
            (2.0, "a spur-and-groove channel"),
            (2.0, "a rubble apron"),
            (2.0, "a stand of staghorn"),
            (1.0, "a bommie standing alone"),
        ],
        (Formation::KelpForest, _) => &[
            (3.0, "a kelp canopy"),
            (2.0, "a holdfast tangle"),
            (2.0, "a stipe forest"),
            (1.0, "an urchin barren, grazed bare"),
        ],
        (Formation::Vent, _) => &[
            (3.0, "a black smoker"),
            (2.0, "a chimney field"),
            (2.0, "a tubeworm thicket"),
            (1.0, "a shimmering haze of hot water"),
        ],
        (Formation::Upwelling, _) => &[
            (3.0, "a plankton bloom"),
            (2.0, "cold water rising"),
            (1.0, "a bait ball, turning"),
        ],
        // The arm the facets exist for: one formation, read by its depth.
        // The flat enum could not say this, because depth had already spent
        // the single slot the community needed.
        (Formation::OpenWater, _) => match stratum {
            Stratum::Epipelagic | Stratum::Surface => &[
                (3.0, "open blue water"),
                (2.0, "a drifting sargassum mat"),
                (1.0, "a shoal turning as one"),
            ],
            Stratum::Mesopelagic => &[
                (3.0, "the twilight water"),
                (2.0, "a scattering layer, rising"),
            ],
            Stratum::Bathypelagic => &[
                (3.0, "the lightless water"),
                (2.0, "marine snow, drifting down"),
            ],
            Stratum::Abyssal => &[
                (3.0, "the abyssal plain"),
                (2.0, "a field of manganese nodules"),
            ],
            Stratum::Hadal => &[(3.0, "the trench wall"), (2.0, "the trench floor")],
        },
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

    fn micro_high() -> MicroField {
        MicroField {
            relief: 0.9,
            aspect: 0.9,
            wetness: 0.9,
            openness: 0.5,
        }
    }

    fn mundane_negations() -> Negations {
        Negations {
            substrate: Substrate::Ordinary,
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::PlantAnimal,
            endemic: false,
        }
    }

    fn addr_() -> RoomAddr {
        RoomAddr {
            face: 3,
            path: vec![0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3],
        }
    }

    const ALL_FORMATIONS: &[Formation] = &[
        Formation::Ice,
        Formation::Tundra,
        Formation::Taiga,
        Formation::TemperateGrassland,
        Formation::Shrubland,
        Formation::TemperateForest,
        Formation::TemperateRainforest,
        Formation::Desert,
        Formation::Savanna,
        Formation::TropicalSeasonalForest,
        Formation::TropicalRainforest,
        Formation::Alpine,
        Formation::SeaIce,
        Formation::Reef,
        Formation::KelpForest,
        Formation::Vent,
        Formation::Upwelling,
        Formation::OpenWater,
    ];

    #[test]
    fn no_formation_falls_through_to_the_catch_all() {
        // The 79%: every formation must have prose of its own.
        for f in ALL_FORMATIONS {
            for stratum in [Stratum::Surface, Stratum::Epipelagic, Stratum::Hadal] {
                let pool = variety_pool(*f, stratum, Substrate::Ordinary);
                assert!(!pool.is_empty(), "{f:?} has no pool");
                for (_, word) in pool {
                    assert!(
                        *word != "broken terrain" && *word != "unremarkable ground",
                        "{f:?} still falls through to the catch-all"
                    );
                }
            }
        }
    }

    #[test]
    fn open_water_reads_differently_at_different_depths() {
        // The structural payoff: one formation, read by its depth.
        let at = |st| {
            render(
                mundane_negations(),
                micro0(),
                BiomeExpr {
                    realm: hornvale_climate::Realm::WATERWORLD,
                    formation: Formation::OpenWater,
                    stratum: st,
                },
                Seed(42),
                &addr_(),
            )
        };
        let shallow = at(Stratum::Epipelagic);
        let deep = at(Stratum::Bathypelagic);
        let trench = at(Stratum::Hadal);
        assert_ne!(shallow, deep);
        assert_ne!(deep, trench);
    }

    #[test]
    fn nothing_underwater_is_dry_or_sun_warmed() {
        for stratum in hornvale_climate::Realm::WATERWORLD.strata() {
            for micro in [micro_high(), micro0()] {
                let d = render(
                    mundane_negations(),
                    micro,
                    BiomeExpr {
                        realm: hornvale_climate::Realm::WATERWORLD,
                        formation: Formation::OpenWater,
                        stratum: *stratum,
                    },
                    Seed(42),
                    &addr_(),
                );
                for bad in ["dry", "damp", "sun-warmed", "on a rise", "in a hollow"] {
                    assert!(!d.contains(bad), "{stratum:?} rendered {bad:?}: {d}");
                }
            }
        }
    }

    #[test]
    fn only_the_sunlit_water_is_described_by_its_light() {
        let at = |st| {
            render(
                mundane_negations(),
                micro_high(),
                BiomeExpr {
                    realm: hornvale_climate::Realm::WATERWORLD,
                    formation: Formation::OpenWater,
                    stratum: st,
                },
                Seed(42),
                &addr_(),
            )
        };
        assert!(at(Stratum::Epipelagic).contains("sunlit"));
        assert!(!at(Stratum::Bathypelagic).contains("sunlit"));
        assert!(!at(Stratum::Abyssal).contains("sunlit"));
    }

    #[test]
    fn land_micro_clauses_are_unchanged() {
        // The guard on the guard: gating the water path must not disturb land.
        let d = render(
            mundane_negations(),
            micro_high(),
            BiomeExpr::for_legacy(Biome::Savanna),
            Seed(42),
            &addr_(),
        );
        assert!(d.contains("sun-warmed"), "{d}");
        assert!(d.contains("damp"), "{d}");
        assert!(d.contains("on a rise"), "{d}");
    }
}
