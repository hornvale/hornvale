//! The strangeness regime — the natural-tier overlay on a room's inherited
//! biome (MAP-29). One value per exclusion slot, so composites are always
//! coherent; `strangeness` is the derived magnitude of that vector.

use hornvale_climate::GroundKind;
use hornvale_kernel::quantize;
use serde::Serialize;

/// This campaign's strangeness ceiling (rung "exotic").
pub(crate) const STRANGENESS_CEILING: f64 = 30.0;

/// What powers a room's ecology (energy slot).
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
/// type-audit: bare-ok(flag: endemic)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct Negations {
    /// Substrate slot.
    #[serde(serialize_with = "serialize_ground_kind")]
    pub substrate: GroundKind,
    /// Energy slot.
    pub energy: EnergySource,
    /// Kingdom slot.
    pub kingdom: Kingdom,
    /// Isolation modifier (endemic biota); gated on an isolation signal.
    pub endemic: bool,
}

/// Serializes `GroundKind` by variant name, matching exactly what
/// `#[derive(Serialize)]` on the enum itself would emit for `serde_json`
/// (`serialize_unit_variant` and `serialize_str` render identically there).
///
/// `GroundKind` cannot derive `Serialize` in its home crate,
/// `hornvale-climate`: decision 0002 holds every domain to `hornvale-kernel`
/// and nothing else in its normal deps — an *exact* match enforced by
/// `cli/tests/suite/architecture.rs::domains_depend_only_on_the_kernel`, not
/// merely the crate-external allowlist that admits `serde` for other layers.
/// `windows/locale` owns neither the type nor the trait, so it cannot supply
/// a foreign `impl Serialize for GroundKind` either (the orphan rule); this
/// field-level override reaches the same JSON shape without either.
fn serialize_ground_kind<S>(kind: &GroundKind, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let name = match kind {
        GroundKind::Ordinary => "Ordinary",
        GroundKind::Sand => "Sand",
        GroundKind::Evaporite => "Evaporite",
        GroundKind::Basaltic => "Basaltic",
        GroundKind::Ashen => "Ashen",
    };
    serializer.serialize_str(name)
}

impl Negations {
    /// The derived strangeness magnitude: the *maximum* slot departure (so a
    /// basaltic vent reads "exotic", not the summed rung-45 we defer), plus a
    /// small endemic bonus, clamped to this campaign's ceiling.
    /// type-audit: bare-ok(ratio)
    pub fn strangeness(&self) -> f64 {
        let substrate: f64 = if self.substrate == GroundKind::Ordinary {
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
/// type-audit: bare-ok(prose: descriptor), bare-ok(prose: descriptor_noun), bare-ok(ratio: strangeness)
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Regime {
    /// The negation vector.
    pub negations: Negations,
    /// The sub-vertex micro-field.
    pub micro: MicroField,
    /// The rendered descriptor prose.
    pub descriptor: String,
    /// The noun phrase within `descriptor` — the part a player would name.
    /// `descriptor` is the whole clause including qualifiers ("a stream gully,
    /// shaded, in a hollow"); this is "stream gully".
    pub descriptor_noun: String,
    /// The derived strangeness magnitude (0..=30, quantized).
    pub strangeness: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mundane() -> Negations {
        Negations {
            substrate: GroundKind::Ordinary,
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
            substrate: GroundKind::Sand,
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
            substrate: GroundKind::Basaltic,
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

    /// Pins the serialized spelling of the substrate slot across the
    /// Substrate -> GroundKind swap (The Hallmark): serde derives the
    /// variant name, and the two enums' variant names are identical, so
    /// this string must not move.
    #[test]
    fn substrate_slot_serializes_by_variant_name() {
        let n = mundane();
        let json = serde_json::to_string(&n).expect("Negations serializes");
        assert!(
            json.contains("\"substrate\":\"Ordinary\""),
            "substrate slot spelling moved: {json}"
        );
    }
}
