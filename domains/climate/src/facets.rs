//! The biome facets (The Stratum §3): a room's biome is a faceted expression
//! — `realm : formation : stratum` — not a single enum value. [`crate::Biome`]
//! remains as the *projection* of an expression, so every existing consumer is
//! unaffected.

use crate::Biome;

/// What fills a realm. A realm is `(medium, access, strata)`, never an
/// enumerated world, so a later sky realm — or an elemental plane — is a new
/// value rather than a new axis (The Stratum §3.4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Medium {
    /// Air over walkable ground — the overworld.
    AirOverRock,
    /// Salt or fresh water — the sea column.
    Water,
}

/// How a realm is reached. This — not materiality — is what separates the
/// world's own column (continuous movement with a medium change) from a plane
/// (transit). An elemental plane is material and still not part of the column,
/// which is what rules materiality out as the discriminator (The Stratum §3.4).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Access {
    /// Simply being there; the default band.
    Default,
    /// Entered by descending through water.
    Dive,
}

/// A realm: a medium, the way in, and the column of strata it holds.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Realm {
    /// What fills it.
    pub medium: Medium,
    /// How it is reached.
    pub access: Access,
}

impl Realm {
    /// The surface world.
    pub const OVERWORLD: Realm = Realm {
        medium: Medium::AirOverRock,
        access: Access::Default,
    };
    /// The sea column.
    pub const WATERWORLD: Realm = Realm {
        medium: Medium::Water,
        access: Access::Dive,
    };

    /// The strata this realm holds, shallowest first.
    pub fn strata(&self) -> &'static [Stratum] {
        match self.medium {
            Medium::AirOverRock => &[Stratum::Surface],
            Medium::Water => &[
                Stratum::Epipelagic,
                Stratum::Mesopelagic,
                Stratum::Bathypelagic,
                Stratum::Abyssal,
                Stratum::Hadal,
            ],
        }
    }
}

/// A position within a realm's column. Realm-relative by construction: the
/// pelagic zones and (later) the underworld's geological layers are the same
/// construct at different realms, rather than two parallel ones.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stratum {
    /// The overworld's only stratum.
    Surface,
    /// Sunlit water, above 200 m.
    Epipelagic,
    /// Twilight water, 200–1000 m.
    Mesopelagic,
    /// Lightless water, 1000–4000 m.
    Bathypelagic,
    /// The abyss, 4000–6000 m.
    Abyssal,
    /// Trench depths, below 6000 m.
    Hadal,
}

impl Stratum {
    /// The stratum at a depth below the surface, in metres. The bands are the
    /// ones `classify_marine` has always used; they move here unchanged.
    /// type-audit: bare-ok(diagnostic-value: depth_m)
    pub fn at_depth_m(depth_m: f64) -> Stratum {
        if depth_m < 200.0 {
            Stratum::Epipelagic
        } else if depth_m < 1000.0 {
            Stratum::Mesopelagic
        } else if depth_m < 4000.0 {
            Stratum::Bathypelagic
        } else if depth_m < 6000.0 {
            Stratum::Abyssal
        } else {
            Stratum::Hadal
        }
    }
}

/// A community type — what *lives* here, independent of how deep it is. This
/// is the half of the old [`crate::Biome`] enum that is genuinely a biome; the
/// other half was [`Stratum`] wearing the same coat.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Formation {
    /// Permanent land ice.
    Ice,
    /// Treeless cold ground.
    Tundra,
    /// Boreal coniferous forest.
    Taiga,
    /// Temperate grassland / steppe.
    TemperateGrassland,
    /// Dry temperate shrubland.
    Shrubland,
    /// Temperate broadleaf forest.
    TemperateForest,
    /// Wet temperate rainforest.
    TemperateRainforest,
    /// Hot desert.
    Desert,
    /// Tropical grassland with scattered trees.
    Savanna,
    /// Tropical forest with a dry season.
    TropicalSeasonalForest,
    /// Wet tropical rainforest.
    TropicalRainforest,
    /// High cold ground above the tree line.
    Alpine,
    /// Frozen sea surface.
    SeaIce,
    /// Warm shallow coral.
    Reef,
    /// Cold shallow kelp.
    KelpForest,
    /// A hydrothermal vent community.
    Vent,
    /// A nutrient-rich upwelling.
    Upwelling,
    /// Open sea with no distinguishing community — the marine default.
    OpenWater,
}

/// A room's biome as a faceted expression. This is the truth; [`crate::Biome`]
/// is its projection, kept so every existing consumer is unaffected.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BiomeExpr {
    /// Which world.
    pub realm: Realm,
    /// Which community.
    pub formation: Formation,
    /// Where in the realm's column.
    pub stratum: Stratum,
}

/// The [`Formation`] a land [`Biome`] names. Written once, used both by
/// [`BiomeExpr::for_legacy`] and by the land arm of `classify_expr`.
pub(crate) fn land_formation(b: Biome) -> Formation {
    match b {
        Biome::Ice => Formation::Ice,
        Biome::Tundra => Formation::Tundra,
        Biome::Taiga => Formation::Taiga,
        Biome::TemperateGrassland => Formation::TemperateGrassland,
        Biome::Shrubland => Formation::Shrubland,
        Biome::TemperateForest => Formation::TemperateForest,
        Biome::TemperateRainforest => Formation::TemperateRainforest,
        Biome::Desert => Formation::Desert,
        Biome::Savanna => Formation::Savanna,
        Biome::TropicalSeasonalForest => Formation::TropicalSeasonalForest,
        Biome::TropicalRainforest => Formation::TropicalRainforest,
        Biome::Alpine => Formation::Alpine,
        // Not land. `classify` never routes a submerged cell here, and the
        // marine arm of `for_legacy` handles these directly; answering
        // `OpenWater` keeps the function total rather than panicking.
        _ => Formation::OpenWater,
    }
}

impl BiomeExpr {
    /// The legacy [`crate::Biome`] this expression projects to.
    ///
    /// **This function is the campaign's byte-identity guarantee.** Every
    /// consumer still calls `biome_at()`, so as long as this reproduces what
    /// `classify` used to return, nothing downstream can tell the taxonomy was
    /// disentangled. `HadalTrench` is the tell: the legacy enum called it a
    /// biome, but it is open water at hadal depth, which is why it falls out
    /// of the `OpenWater` arm instead of having a formation of its own.
    pub fn biome(&self) -> Biome {
        match self.formation {
            Formation::Ice => Biome::Ice,
            Formation::Tundra => Biome::Tundra,
            Formation::Taiga => Biome::Taiga,
            Formation::TemperateGrassland => Biome::TemperateGrassland,
            Formation::Shrubland => Biome::Shrubland,
            Formation::TemperateForest => Biome::TemperateForest,
            Formation::TemperateRainforest => Biome::TemperateRainforest,
            Formation::Desert => Biome::Desert,
            Formation::Savanna => Biome::Savanna,
            Formation::TropicalSeasonalForest => Biome::TropicalSeasonalForest,
            Formation::TropicalRainforest => Biome::TropicalRainforest,
            Formation::Alpine => Biome::Alpine,
            Formation::SeaIce => Biome::SeaIce,
            Formation::Reef => Biome::CoralReef,
            Formation::KelpForest => Biome::KelpForest,
            Formation::Vent => Biome::HydrothermalVent,
            Formation::Upwelling => Biome::Upwelling,
            Formation::OpenWater => match self.stratum {
                Stratum::Hadal => Biome::HadalTrench,
                Stratum::Abyssal => Biome::Abyssal,
                Stratum::Bathypelagic => Biome::Bathypelagic,
                Stratum::Mesopelagic => Biome::Mesopelagic,
                // `Surface` is unreachable for open water in practice; treat
                // it as the shallowest water rather than inventing a Biome.
                Stratum::Epipelagic | Stratum::Surface => Biome::Epipelagic,
            },
        }
    }

    /// The canonical expression for a legacy [`crate::Biome`] — a right
    /// inverse of [`BiomeExpr::biome`], used to prove the projection is onto.
    /// Marine formations take the shallowest stratum that yields them.
    pub fn for_legacy(b: Biome) -> BiomeExpr {
        let (realm, formation, stratum) = match b {
            Biome::SeaIce => (Realm::WATERWORLD, Formation::SeaIce, Stratum::Epipelagic),
            Biome::CoralReef => (Realm::WATERWORLD, Formation::Reef, Stratum::Epipelagic),
            Biome::KelpForest => (
                Realm::WATERWORLD,
                Formation::KelpForest,
                Stratum::Epipelagic,
            ),
            Biome::HydrothermalVent => (Realm::WATERWORLD, Formation::Vent, Stratum::Abyssal),
            Biome::Upwelling => (Realm::WATERWORLD, Formation::Upwelling, Stratum::Epipelagic),
            Biome::Epipelagic => (Realm::WATERWORLD, Formation::OpenWater, Stratum::Epipelagic),
            Biome::Mesopelagic => (
                Realm::WATERWORLD,
                Formation::OpenWater,
                Stratum::Mesopelagic,
            ),
            Biome::Bathypelagic => (
                Realm::WATERWORLD,
                Formation::OpenWater,
                Stratum::Bathypelagic,
            ),
            Biome::Abyssal => (Realm::WATERWORLD, Formation::OpenWater, Stratum::Abyssal),
            Biome::HadalTrench => (Realm::WATERWORLD, Formation::OpenWater, Stratum::Hadal),
            land => (Realm::OVERWORLD, land_formation(land), Stratum::Surface),
        };
        BiomeExpr {
            realm,
            formation,
            stratum,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_realm_is_a_triple_not_a_flag() {
        // The Stratum §3.4: realms are values, so a later sky realm or plane
        // is a new value rather than a new axis. This test exists to fail if
        // someone collapses Realm back into a two-valued enum.
        assert_eq!(Realm::OVERWORLD.medium, Medium::AirOverRock);
        assert_eq!(Realm::OVERWORLD.access, Access::Default);
        assert_eq!(Realm::WATERWORLD.medium, Medium::Water);
        assert_eq!(Realm::WATERWORLD.access, Access::Dive);
        assert_ne!(Realm::OVERWORLD, Realm::WATERWORLD);
    }

    #[test]
    fn the_overworld_has_one_stratum_and_the_waterworld_a_column() {
        assert_eq!(Realm::OVERWORLD.strata(), &[Stratum::Surface]);
        assert_eq!(Realm::WATERWORLD.strata().len(), 5);
        assert_eq!(Realm::WATERWORLD.strata()[0], Stratum::Epipelagic);
        assert_eq!(Realm::WATERWORLD.strata()[4], Stratum::Hadal);
    }

    #[test]
    fn depth_maps_to_the_documented_stratum_bands() {
        assert_eq!(Stratum::at_depth_m(0.0), Stratum::Epipelagic);
        assert_eq!(Stratum::at_depth_m(199.9), Stratum::Epipelagic);
        assert_eq!(Stratum::at_depth_m(200.0), Stratum::Mesopelagic);
        assert_eq!(Stratum::at_depth_m(999.9), Stratum::Mesopelagic);
        assert_eq!(Stratum::at_depth_m(1000.0), Stratum::Bathypelagic);
        assert_eq!(Stratum::at_depth_m(3999.9), Stratum::Bathypelagic);
        assert_eq!(Stratum::at_depth_m(4000.0), Stratum::Abyssal);
        assert_eq!(Stratum::at_depth_m(5999.9), Stratum::Abyssal);
        assert_eq!(Stratum::at_depth_m(6000.0), Stratum::Hadal);
    }

    #[test]
    fn every_legacy_biome_is_the_projection_of_some_expression() {
        // The projection must be ONTO: a legacy Biome with no expression that
        // produces it would be a classification silently dropped.
        for b in Biome::catalog() {
            assert_eq!(
                BiomeExpr::for_legacy(*b).biome(),
                *b,
                "no expression projects back to {b:?}"
            );
        }
    }

    #[test]
    fn open_water_projects_by_its_stratum() {
        let ow = |s| BiomeExpr {
            realm: Realm::WATERWORLD,
            formation: Formation::OpenWater,
            stratum: s,
        };
        assert_eq!(ow(Stratum::Epipelagic).biome(), Biome::Epipelagic);
        assert_eq!(ow(Stratum::Mesopelagic).biome(), Biome::Mesopelagic);
        assert_eq!(ow(Stratum::Bathypelagic).biome(), Biome::Bathypelagic);
        assert_eq!(ow(Stratum::Abyssal).biome(), Biome::Abyssal);
        assert_eq!(ow(Stratum::Hadal).biome(), Biome::HadalTrench);
    }

    #[test]
    fn a_vent_keeps_its_identity_at_every_depth() {
        // The point of the split: a vent IS abyssal, rather than a community
        // that had to displace a stratum in order to exist.
        for s in Realm::WATERWORLD.strata() {
            let e = BiomeExpr {
                realm: Realm::WATERWORLD,
                formation: Formation::Vent,
                stratum: *s,
            };
            assert_eq!(e.biome(), Biome::HydrothermalVent);
        }
    }
}
