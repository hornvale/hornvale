//! The strangeness regime — the natural-tier overlay on a room's inherited
//! biome (MAP-29). One value per exclusion slot, so composites are always
//! coherent; `strangeness` is the derived magnitude of that vector.

use hornvale_kernel::quantize;
use serde::Serialize;

/// This campaign's strangeness ceiling (rung "exotic").
const STRANGENESS_CEILING: f64 = 30.0;

/// The material a room's ground is made of (substrate slot; proxy-earned only).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Substrate {
    /// Rock and soil — the mundane default.
    Ordinary,
    /// Wind-worked sand.
    Sand,
    /// Evaporite salt/gypsum crust.
    Evaporite,
    /// Bare volcanic basalt.
    Basaltic,
    /// Volcanic ash drifts.
    Ashen,
}

/// What powers a room's ecology (energy slot).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum EnergySource {
    /// Sunlight — the mundane default.
    Sunlit,
    /// Chemosynthesis (cold seeps, vents).
    Chemosynthetic,
    /// Geothermal heat.
    Geothermal,
}

/// The dominant kingdom of life (kingdom slot).
/// type-audit: bare-ok(prose)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum Kingdom {
    /// Plants then animals — the mundane default.
    PlantAnimal,
    /// A fungal kingdom.
    Fungal,
    /// Mineral/crystalline "flora".
    Crystalline,
    /// Microbial mats.
    Microbial,
}

/// The negation vector: one draw per exclusion slot plus the endemic toggle.
/// type-audit: bare-ok(prose: substrate), bare-ok(prose: energy), bare-ok(prose: kingdom), bare-ok(flag: endemic)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Negations {
    /// Substrate slot.
    pub substrate: Substrate,
    /// Energy slot.
    pub energy: EnergySource,
    /// Kingdom slot.
    pub kingdom: Kingdom,
    /// Isolation modifier (endemic biota); gated on an isolation signal.
    pub endemic: bool,
}

impl Negations {
    /// The derived strangeness magnitude: the *maximum* slot departure (so a
    /// basaltic vent reads "exotic", not the summed rung-45 we defer), plus a
    /// small endemic bonus, clamped to this campaign's ceiling.
    /// type-audit: bare-ok(ratio)
    pub fn strangeness(&self) -> f64 {
        let substrate: f64 = if self.substrate == Substrate::Ordinary {
            0.0
        } else {
            15.0
        };
        let energy: f64 = if self.energy == EnergySource::Sunlit {
            0.0
        } else {
            30.0
        };
        let kingdom: f64 = if self.kingdom == Kingdom::PlantAnimal {
            0.0
        } else {
            30.0
        };
        let max = substrate.max(energy).max(kingdom);
        let bonus = if self.endemic { 5.0 } else { 0.0 };
        quantize((max + bonus).min(STRANGENESS_CEILING))
    }
}

/// A few grounded per-room continuous axes (from address noise); the descriptor
/// reads these so homogeneous biome still varies room-to-room.
/// type-audit: bare-ok(ratio: relief), bare-ok(ratio: aspect), bare-ok(ratio: wetness), bare-ok(ratio: openness)
#[derive(Debug, Clone, Copy, PartialEq, Serialize)]
pub struct MicroField {
    /// Micro-relief, hollow (-1) .. rise (+1).
    pub relief: f64,
    /// Slope aspect / insolation, shaded (-1) .. sunlit (+1).
    pub aspect: f64,
    /// Local wetness, dry (-1) .. wet (+1).
    pub wetness: f64,
    /// Canopy openness, closed (-1) .. open (+1).
    pub openness: f64,
}

/// The strangeness overlay for a room. The base biome stays on `Locale.biome`.
/// type-audit: bare-ok(prose: descriptor), bare-ok(ratio: strangeness)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Regime {
    /// The negation vector.
    pub negations: Negations,
    /// The sub-cell micro-field.
    pub micro: MicroField,
    /// The rendered descriptor prose.
    pub descriptor: String,
    /// The derived strangeness magnitude (0..=30, quantized).
    pub strangeness: f64,
}

#[allow(dead_code)] // consumers land in later tasks
#[cfg(test)]
mod tests {
    use super::*;

    fn mundane() -> Negations {
        Negations {
            substrate: Substrate::Ordinary,
            energy: EnergySource::Sunlit,
            kingdom: Kingdom::PlantAnimal,
            endemic: false,
        }
    }

    #[test]
    fn mundane_has_zero_strangeness() {
        assert_eq!(mundane().strangeness(), 0.0);
    }

    #[test]
    fn substrate_negation_is_extreme_rung() {
        let n = Negations {
            substrate: Substrate::Sand,
            ..mundane()
        };
        assert_eq!(n.strangeness(), 15.0);
    }

    #[test]
    fn energy_or_kingdom_negation_is_exotic_rung() {
        let e = Negations {
            energy: EnergySource::Chemosynthetic,
            ..mundane()
        };
        let k = Negations {
            kingdom: Kingdom::Fungal,
            ..mundane()
        };
        assert_eq!(e.strangeness(), 30.0);
        assert_eq!(k.strangeness(), 30.0);
    }

    #[test]
    fn magnitude_is_the_max_departure_not_the_sum() {
        // basaltic (15) + chemo (30) reads as exotic (30), never rung-45.
        let n = Negations {
            substrate: Substrate::Basaltic,
            energy: EnergySource::Chemosynthetic,
            ..mundane()
        };
        assert_eq!(n.strangeness(), 30.0);
    }

    #[test]
    fn endemic_adds_a_capped_bonus() {
        let n = Negations {
            endemic: true,
            ..mundane()
        };
        assert_eq!(n.strangeness(), 5.0);
        let capped = Negations {
            energy: EnergySource::Geothermal,
            endemic: true,
            ..mundane()
        };
        assert_eq!(capped.strangeness(), 30.0); // clamped to the campaign ceiling
    }
}
