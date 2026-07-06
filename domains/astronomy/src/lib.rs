//! Astronomy, tier 0: a sun that never sets. All downstream systems see
//! astronomy only through phenomena — never this crate.
#![warn(missing_docs)]

use hornvale_kernel::{
    ConceptRegistry, ObserverContext, PhenomenaSource, Phenomenon, RegistryError, WorldTime,
};

/// Phenomenon kind for bodies visible in the sky.
pub const CELESTIAL_BODY: &str = "celestial-body";

/// Every seed-derivation label this crate uses (none yet).
pub fn stream_labels() -> Vec<(&'static str, &'static str)> {
    Vec::new()
}

/// Register astronomy's contribution to the concept registry.
pub fn register_concepts(registry: &mut ConceptRegistry) -> Result<(), RegistryError> {
    registry.register_phenomenon_kind(CELESTIAL_BODY, "a body visible in the sky")
}

/// Tier-0 astronomy: the sun is always up, fixed at zenith.
pub struct ConstantSun;

/// What the sky looks like at a given moment.
#[derive(Debug, Clone, PartialEq)]
pub struct SkyReport {
    /// Human-readable description of the sky.
    pub description: String,
    /// Names of the visible bodies.
    pub bodies: Vec<String>,
}

impl ConstantSun {
    /// The sky at `_time` — which, at tier 0, never changes.
    pub fn sky_at(&self, _time: WorldTime) -> SkyReport {
        SkyReport {
            description: "A golden sun hangs fixed at zenith. It has never been seen to move."
                .to_string(),
            bodies: vec!["the sun".to_string()],
        }
    }
}

impl PhenomenaSource for ConstantSun {
    fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
        vec![Phenomenon {
            kind: CELESTIAL_BODY.to_string(),
            description: "a golden sun fixed at zenith".to_string(),
            period_days: None,
            salience: 1.0,
        }]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use hornvale_kernel::EntityId;

    fn ctx(day: f64) -> ObserverContext {
        ObserverContext {
            place: EntityId(1),
            time: WorldTime { day },
        }
    }

    #[test]
    fn the_sky_never_changes() {
        let sun = ConstantSun;
        let a = sun.sky_at(WorldTime { day: 0.0 });
        let b = sun.sky_at(WorldTime { day: 9999.5 });
        assert_eq!(a.description, b.description);
        assert_eq!(a.bodies, b.bodies);
        assert!(a.description.contains("zenith"));
    }

    #[test]
    fn phenomena_are_constant_and_maximally_salient() {
        let sun = ConstantSun;
        let seen = sun.phenomena(&ctx(0.0));
        assert_eq!(seen.len(), 1);
        assert_eq!(seen[0].kind, CELESTIAL_BODY);
        assert_eq!(seen[0].period_days, None);
        assert_eq!(seen[0].salience, 1.0);
        assert_eq!(seen, sun.phenomena(&ctx(500.25)));
    }

    #[test]
    fn concepts_register_idempotently() {
        let mut r = ConceptRegistry::default();
        register_concepts(&mut r).unwrap();
        register_concepts(&mut r).unwrap();
        assert!(r.phenomenon_kind(CELESTIAL_BODY).is_some());
    }
}
