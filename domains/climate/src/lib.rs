//! Climate, tier 0: mild and temperate everywhere, forever.
#![warn(missing_docs)]

pub mod biome;
pub mod circulation;
pub mod habitability;
pub mod moisture;
pub mod temperature;

pub use biome::{Biome, SeafloorFeature};
pub use circulation::{RotationRegime, band_count_for};
pub use habitability::{habitable_fraction, is_habitable};

use hornvale_kernel::{
    ConceptRegistry, ObserverContext, PhenomenaSource, Phenomenon, Position, RegistryError,
};

/// Phenomenon kind for pervasive atmospheric conditions.
pub const AMBIENT: &str = "ambient";

/// Every seed-derivation label this crate uses (none yet).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register climate's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(AMBIENT, "a pervasive atmospheric condition")
}

/// Tier-0 climate: the same mild air everywhere.
pub struct UniformClimate;

/// Local climate conditions.
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
        let seen = c.phenomena(&ObserverContext {
            place: EntityId(1),
            time: WorldTime { day: 3.0 },
        });
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
}
