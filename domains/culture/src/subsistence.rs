//! Subsistence: a settlement's mode of making a living, an exact function of
//! its biome class and whether it reaches the coast. Farming in productive
//! land, herding in the arid interior, foraging on the cold/barren margins,
//! and fishing where a marginal hinterland meets the sea. The exactness is
//! the point — the Lab calibrates subsistence against biome (spec §10).

/// How a settlement makes its living.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Subsistence {
    /// Settled agriculture.
    Farming,
    /// Pastoral herding.
    Herding,
    /// Coastal fishing.
    Fishing,
    /// Hunting and gathering.
    Foraging,
}

/// A coarse biome category (culture-owned; the composition root maps every
/// `climate::Biome` into this so culture imports no domain).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BiomeClass {
    /// Forests, taiga, rainforests — productive, arable.
    Forest,
    /// Grasslands and savanna — arable/pastoral.
    Grassland,
    /// Deserts and shrubland — dry interior.
    Arid,
    /// Tundra and cold margins.
    Cold,
    /// Alpine, ice, or (defensively) marine — barren.
    Barren,
}

impl Subsistence {
    /// Kebab-case name (almanac, Lab, CSV).
    /// type-audit: bare-ok(identifier-text)
    pub fn name(self) -> &'static str {
        match self {
            Subsistence::Farming => "farming",
            Subsistence::Herding => "herding",
            Subsistence::Fishing => "fishing",
            Subsistence::Foraging => "foraging",
        }
    }
    /// The base worker role this subsistence implies.
    /// type-audit: bare-ok(identifier-text)
    pub fn worker(self) -> &'static str {
        match self {
            Subsistence::Farming => "farmer",
            Subsistence::Herding => "herder",
            Subsistence::Fishing => "fisher",
            Subsistence::Foraging => "forager",
        }
    }
}

/// Relative land fertility of a biome class, `[0, 1]`.
/// type-audit: bare-ok(ratio)
pub fn fertility(class: BiomeClass) -> f64 {
    match class {
        BiomeClass::Forest => 0.9,
        BiomeClass::Grassland => 0.7,
        BiomeClass::Arid => 0.3,
        BiomeClass::Cold => 0.2,
        BiomeClass::Barren => 0.1,
    }
}

/// The subsistence mode for a biome class and coastal access. Forest and
/// grassland farm regardless of coast; arid herds inland but fishes on the
/// coast; cold and barren forage inland but fish on the coast.
/// type-audit: bare-ok(flag)
pub fn subsistence(class: BiomeClass, coastal: bool) -> Subsistence {
    let inland = match class {
        BiomeClass::Forest | BiomeClass::Grassland => Subsistence::Farming,
        BiomeClass::Arid => Subsistence::Herding,
        BiomeClass::Cold | BiomeClass::Barren => Subsistence::Foraging,
    };
    if coastal && matches!(inland, Subsistence::Herding | Subsistence::Foraging) {
        Subsistence::Fishing
    } else {
        inland
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subsistence_is_an_exact_function_of_biome_and_coast() {
        assert_eq!(subsistence(BiomeClass::Forest, false), Subsistence::Farming);
        assert_eq!(subsistence(BiomeClass::Forest, true), Subsistence::Farming); // rich hinterland stays farming
        assert_eq!(
            subsistence(BiomeClass::Grassland, false),
            Subsistence::Farming
        );
        assert_eq!(subsistence(BiomeClass::Arid, false), Subsistence::Herding);
        assert_eq!(subsistence(BiomeClass::Arid, true), Subsistence::Fishing); // coast rescues the interior
        assert_eq!(subsistence(BiomeClass::Cold, false), Subsistence::Foraging);
        assert_eq!(subsistence(BiomeClass::Barren, true), Subsistence::Fishing);
        assert!(fertility(BiomeClass::Forest) > fertility(BiomeClass::Arid));
        assert_eq!(Subsistence::Fishing.worker(), "fisher");
    }
}
