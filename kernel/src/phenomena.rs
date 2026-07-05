//! Phenomena: the universal read (spec §3.1.6). "What would an observer
//! at (place, time) notice?" Every meaning-making system consumes this
//! and only this; it never learns what produced a phenomenon.

use crate::field::WorldTime;
use crate::ledger::EntityId;
use serde::{Deserialize, Serialize};

/// Something an observer would notice. `kind` must be registered in the
/// concept registry by the producing domain. Consumers must not branch on
/// the producing system — only on kind, period, character, salience.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Phenomenon {
    /// Registered phenomenon kind (concept-registry key).
    pub kind: String,
    /// Human-readable character of the phenomenon.
    pub description: String,
    /// None = constant or aperiodic; Some(d) = recurs every d days.
    pub period_days: Option<f64>,
    /// How much this demands attention, in [0, 1].
    pub salience: f64,
}

/// Where and when the observation happens. Culture joins in a later
/// campaign; adding a field here must not break existing sources.
#[derive(Clone, Copy, Debug)]
pub struct ObserverContext {
    /// Entity id of the observer's location.
    pub place: EntityId,
    /// Simulated time of the observation.
    pub time: WorldTime,
}

/// Anything that contributes observable phenomena. Implementations must
/// be pure: same context → same phenomena.
pub trait PhenomenaSource {
    /// Phenomena this source contributes for the given observer.
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon>;
}

/// Aggregate all sources, sorted by salience descending. Ties break by
/// kind then description so output order never depends on source order
/// alone being stable — determinism is constitutional.
pub fn observe(sources: &[&dyn PhenomenaSource], ctx: &ObserverContext) -> Vec<Phenomenon> {
    let mut all: Vec<Phenomenon> = sources.iter().flat_map(|s| s.phenomena(ctx)).collect();
    all.sort_by(|a, b| {
        b.salience
            .total_cmp(&a.salience)
            .then_with(|| a.kind.cmp(&b.kind))
            .then_with(|| a.description.cmp(&b.description))
    });
    all
}

#[cfg(test)]
mod tests {
    use super::*;

    struct FixedSource(Vec<Phenomenon>);

    impl PhenomenaSource for FixedSource {
        fn phenomena(&self, _ctx: &ObserverContext) -> Vec<Phenomenon> {
            self.0.clone()
        }
    }

    fn ctx() -> ObserverContext {
        ObserverContext {
            place: EntityId(1),
            time: WorldTime { day: 0.0 },
        }
    }

    fn ph(kind: &str, salience: f64) -> Phenomenon {
        Phenomenon {
            kind: kind.to_string(),
            description: format!("the {kind}"),
            period_days: None,
            salience,
        }
    }

    #[test]
    fn observe_aggregates_all_sources() {
        let a = FixedSource(vec![ph("sun", 1.0)]);
        let b = FixedSource(vec![ph("breeze", 0.2), ph("river", 0.4)]);
        let out = observe(&[&a, &b], &ctx());
        assert_eq!(out.len(), 3);
    }

    #[test]
    fn observe_sorts_by_salience_descending() {
        let a = FixedSource(vec![ph("breeze", 0.2)]);
        let b = FixedSource(vec![ph("sun", 1.0), ph("river", 0.4)]);
        let kinds: Vec<String> = observe(&[&a, &b], &ctx())
            .into_iter()
            .map(|p| p.kind)
            .collect();
        assert_eq!(kinds, vec!["sun", "river", "breeze"]);
    }

    #[test]
    fn observe_breaks_salience_ties_deterministically() {
        // Equal salience: sorted by kind, then description.
        let a = FixedSource(vec![ph("zephyr", 0.5), ph("aurora", 0.5)]);
        let out = observe(&[&a], &ctx());
        assert_eq!(out[0].kind, "aurora");
        assert_eq!(out[1].kind, "zephyr");
    }

    #[test]
    fn observe_with_no_sources_is_empty() {
        assert!(observe(&[], &ctx()).is_empty());
    }
}
