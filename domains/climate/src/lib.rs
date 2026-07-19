//! Climate, tier 0: mild and temperate everywhere, forever.
#![warn(missing_docs)]

pub mod biome;
pub mod circulation;
pub mod currents;
pub mod diurnal;
pub mod habitability;
pub mod moisture;
pub mod provider;
pub mod render;
pub mod substellar;
pub mod temperature;

pub use biome::{Biome, SeafloorFeature};
pub use circulation::{RotationRegime, band_count_for, prevailing_wind};
pub use currents::{ocean_current, ocean_current_field};
pub use diurnal::{diurnal_amplitude, diurnal_anomaly, diurnal_waveform};
pub use habitability::{habitable_fraction, is_habitable};
pub use provider::{ClimateInputs, ClimateSummary, GeneratedClimate, summarize};
pub use substellar::{
    SUBSTELLAR, locked_cell_temperature, substellar_at, substellar_cosine, substellar_cosine_dir,
};
pub use temperature::locked_temperature_at_position;

use hornvale_kernel::{
    ConceptDef, ConceptKind, ConceptRegistry, Correspondent, Manifest, ObserverContext,
    PhenomenaSource, Phenomenon, Position, RegistryError, Venue, Void,
};

/// Phenomenon kind for pervasive atmospheric conditions.
/// type-audit: bare-ok(identifier-text)
pub const AMBIENT: &str = "ambient";

/// Every seed-derivation label this crate uses (none yet).
/// type-audit: bare-ok(identifier-text)
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register climate's contribution to the concept registry.
///
/// Each concept is registered through its correspondence [`Manifest`], which
/// records — honestly, per edge — how the concept is (or is not yet) carried
/// across the lexicon, perception, and cognition ledgers:
/// - **lexeme**: no language pack names any of climate's concepts yet —
///   neither the everyday substances (snow/rain/ice) nor the biome classes —
///   so every lexeme edge is a `Gap`. (The Correspondence campaign's
///   reconciliation drift-check made the earlier over-optimistic `Expected`
///   on snow/rain/ice honest: an `Expected` a pack must actually cover.)
/// - **percept**: climate emits only the AMBIENT wind gloss, which is none of
///   these substances or biomes, so every edge is a `Gap` — not `AMBIENT`.
/// - **cognition**: the whole column voids to the future cognition wave.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(AMBIENT, "a pervasive atmospheric condition")?;

    // The everyday precipitation/frozen-water substances. No language pack
    // names them yet, so the lexeme edge is an honest Gap (not `Expected`).
    for (name, doc) in [
        ("snow", "frozen precipitation"),
        ("rain", "liquid precipitation"),
        ("ice", "frozen water"),
    ] {
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "climate".to_string(),
                kind: ConceptKind::Substance,
                doc: doc.to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names it yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }

    for b in biome::ALL {
        let name = b.concept_name();
        // Biome::Ice's kebab name is "ice", identical to the substance
        // concept above — the same word covers frozen water and the biome
        // it forms, so the substance registration (already Substance-kinded)
        // stands and the biome loop skips it rather than conflicting.
        if registry.concept(name).is_some() {
            continue;
        }
        registry.register_manifest(Manifest {
            concept: ConceptDef {
                name: name.to_string(),
                domain: "climate".to_string(),
                kind: ConceptKind::Terrain,
                doc: "a biome class".to_string(),
            },
            lexeme: Correspondent::Absent(Void::Gap("no language pack names biome classes yet")),
            percept: Correspondent::Absent(Void::Gap("not emitted as a phenomenon yet")),
            cognition: Correspondent::Absent(Void::Uncognized {
                pending_wave: "wave-cognition",
            }),
        })?;
    }
    Ok(())
}

/// Climate as a registrable unit for the composition-root roster.
/// type-audit: bare-ok(identifier-text: return)
pub struct Climate;

impl hornvale_kernel::Domain for Climate {
    fn crate_name(&self) -> &'static str {
        env!("CARGO_PKG_NAME")
    }
    fn register_concepts(
        &self,
        registry: &mut hornvale_kernel::ConceptRegistry,
    ) -> Result<(), hornvale_kernel::RegistryError> {
        crate::register_concepts(registry)
    }
    fn stream_labels(&self) -> Vec<(&'static str, &'static str)> {
        crate::stream_labels()
    }
}

/// Tier-0 climate: the same mild air everywhere.
pub struct UniformClimate;

/// Local climate conditions.
/// type-audit: pending(wave-2: temperature_c), bare-ok(prose: description)
#[derive(Debug, Clone, PartialEq)]
pub struct ClimateReport {
    /// Air temperature in degrees Celsius.
    pub temperature_c: f64,
    /// Human-readable description of the conditions.
    pub description: String,
}

impl UniformClimate {
    /// The climate at `_pos` — which, at tier 0, is the same everywhere.
    pub fn climate_at(&self, _pos: Position) -> ClimateReport {
        ClimateReport {
            temperature_c: 18.0,
            description: "Mild and temperate. The air is warm, still, and unchanging.".to_string(),
        }
    }
}

impl PhenomenaSource for UniformClimate {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: AMBIENT.to_string(),
            description: "warm, still, unchanging air".to_string(),
            period_days: None,
            salience: 0.15,
            venue: Venue::Ambient,
        }]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::{EntityId, WorldTime};

    #[test]
    fn climate_is_uniform_in_space() {
        let c = UniformClimate;
        let here = c.climate_at(Position { x: 0.0, y: 0.0 });
        let far = c.climate_at(Position { x: 1e6, y: -1e6 });
        assert_eq!(here.temperature_c, far.temperature_c);
        assert_eq!(here.description, far.description);
        assert_eq!(here.temperature_c, 18.0);
    }

    #[test]
    fn climate_contributes_a_low_salience_phenomenon() {
        let c = UniformClimate;
        let seen = c.phenomena(&ObserverContext::at(
            EntityId::new(1).unwrap(),
            WorldTime { day: 3.0 },
        ));
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, AMBIENT);
        assert!(seen[0].salience < 0.5);
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.phenomenon_kind(AMBIENT).is_some());
    }

    #[test]
    fn concepts_registered() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        for name in ["snow", "rain", "ice"] {
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing concept {name}"));
            assert_eq!(c.domain, "climate");
            assert_eq!(c.kind, ConceptKind::Substance);
        }
        for b in biome::ALL {
            let name = b.concept_name();
            let c = r
                .concept(name)
                .unwrap_or_else(|| panic!("missing biome concept {name}"));
            assert_eq!(c.domain, "climate");
            // "ice" the biome shares its word with "ice" the substance
            // (registered above as Substance); every other biome is Terrain.
            let expected_kind = if name == "ice" {
                ConceptKind::Substance
            } else {
                ConceptKind::Terrain
            };
            assert_eq!(c.kind, expected_kind);
        }
    }
}
