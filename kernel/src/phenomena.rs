//! Phenomena: the universal read (spec §3.1.6). "What would an observer
//! at (place, time) notice?" Every meaning-making system consumes this
//! and only this; it never learns what produced a phenomenon.

use crate::field::WorldTime;
use crate::geosphere::GeoCoord;
use crate::ledger::EntityId;
use serde::{Deserialize, Serialize};

/// Where a phenomenon lives, as its producer honestly knows: the day sky,
/// the night sky, or the ambient world. Character, not cause — declaring a
/// venue reveals nothing about which system produced the phenomenon.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Venue {
    /// Seen in the daytime sky (the sun).
    DaySky,
    /// Seen in the night sky (moons, stars).
    NightSky,
    /// Felt through the world rather than watched (air, seasons).
    Ambient,
}

/// Something an observer would notice. `kind` must be registered in the
/// concept registry by the producing domain. Consumers must not branch on
/// the producing system — only on kind, period, character, salience.
/// type-audit: bare-ok(identifier-text: kind), bare-ok(prose: description), pending(wave-1: period_days), bare-ok(ratio: salience)
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
    /// Where this phenomenon lives (producer-declared character).
    pub venue: Venue,
}

/// Multiplicative per-venue salience weights: how much attention an
/// observer's eyes give each venue. The identity lens is a byte-level
/// no-op in `observe` — it triggers no arithmetic at all.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct PerceptionLens {
    /// Weight for `Venue::DaySky` phenomena.
    pub day_sky: f64,
    /// Weight for `Venue::NightSky` phenomena.
    pub night_sky: f64,
    /// Weight for `Venue::Ambient` phenomena.
    pub ambient: f64,
}

impl PerceptionLens {
    /// The identity lens: every venue weighted 1.0.
    pub fn identity() -> Self {
        PerceptionLens {
            day_sky: 1.0,
            night_sky: 1.0,
            ambient: 1.0,
        }
    }

    /// Whether this lens is exactly the identity (bitwise 1.0 weights).
    /// type-audit: bare-ok(flag)
    pub fn is_identity(&self) -> bool {
        self.day_sky == 1.0 && self.night_sky == 1.0 && self.ambient == 1.0
    }

    /// This lens seen through `other` — the component-wise product. Occlusion
    /// composes with a species' own perception rather than replacing it: a
    /// nocturnal observer under an overcast is subject to both.
    pub fn compose(&self, other: &PerceptionLens) -> PerceptionLens {
        PerceptionLens {
            day_sky: self.day_sky * other.day_sky,
            night_sky: self.night_sky * other.night_sky,
            ambient: self.ambient * other.ambient,
        }
    }

    fn weight(&self, venue: Venue) -> f64 {
        match venue {
            Venue::DaySky => self.day_sky,
            Venue::NightSky => self.night_sky,
            Venue::Ambient => self.ambient,
        }
    }
}

/// How much of the sky reaches the observer, in `[0, 1]`: `1.0` is an
/// unobstructed view, `0.0` a sky completely hidden. Deliberately abstract —
/// a producer decides what the ratio *means* for its own content, and never
/// learns what obstructed the view.
/// type-audit: bare-ok(ratio)
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Visibility(f64);

impl Visibility {
    /// A wholly unobstructed sky. The legacy path: every producer must render
    /// exactly its pre-occlusion content at this value.
    pub const CLEAR: Visibility = Visibility(1.0);

    /// A visibility ratio, or `None` if `v` is not a finite value in `[0, 1]`.
    /// type-audit: bare-ok(constructor-edge: v)
    pub fn new(v: f64) -> Option<Visibility> {
        (v.is_finite() && (0.0..=1.0).contains(&v)).then_some(Visibility(v))
    }

    /// The ratio itself.
    /// type-audit: bare-ok(ratio)
    pub fn get(&self) -> f64 {
        self.0
    }
}

/// Weighted salience at or above which a phenomenon still reaches the
/// observer. Below it the phenomenon is culled rather than merely demoted: a
/// star dimmed to a fiftieth is not a faint star, it is a star you cannot
/// see. Applied only when the lens is non-identity, so the legacy path is
/// untouched.
///
/// Measured against seed 42 at day 0 (a flat overcast, occlusion `v = 0.3`):
/// the two moons fall 0.64 → 0.19 and 0.47 → 0.14 and survive; the five
/// neighbour stars fall 0.10–0.11 → 0.03 and do not.
/// type-audit: bare-ok(ratio)
pub const VISIBILITY_FLOOR: f64 = 0.05;

/// Where and when the observation happens. Culture joins in a later
/// campaign; adding a field here must not break existing sources.
#[derive(Clone, Copy, Debug)]
pub struct ObserverContext {
    /// Entity id of the observer's location.
    pub place: EntityId,
    /// Simulated time of the observation.
    pub time: WorldTime,
    /// The observer's perception lens; `PerceptionLens::identity()` for an
    /// unlensed (instrument's-eye) observation.
    pub lens: PerceptionLens,
    /// The observer's position on the globe, if placed. `None` is a
    /// position-blind observation (nowhere in particular) — the sky is not
    /// culled by horizon. Placed by the composition root from the flagship
    /// cell (SEQ-4); consumed by providers to cull the visible sky (SEQ-5).
    pub position: Option<GeoCoord>,
}

impl ObserverContext {
    /// An unlensed, position-blind observation at a place and time (identity
    /// lens, no globe position — the sky is not culled by horizon).
    pub fn at(place: EntityId, time: WorldTime) -> Self {
        ObserverContext {
            place,
            time,
            lens: PerceptionLens::identity(),
            position: None,
        }
    }

    /// An unlensed observation from a real place on the globe (identity lens).
    /// Providers cull the visible sky to this position's hemisphere.
    pub fn at_position(place: EntityId, time: WorldTime, position: GeoCoord) -> Self {
        ObserverContext {
            place,
            time,
            lens: PerceptionLens::identity(),
            position: Some(position),
        }
    }
}

/// Anything that contributes observable phenomena. Implementations must
/// be pure: same context → same phenomena.
pub trait PhenomenaSource {
    /// Phenomena this source contributes for the given observer.
    fn phenomena(&self, ctx: &ObserverContext) -> Vec<Phenomenon>;
}

/// Aggregate all sources, sorted by salience descending. Ties break by
/// kind then description so output order never depends on source order
/// alone being stable — determinism is constitutional, and every sort
/// carries a deterministic tie-break (decision 0005).
pub fn observe(sources: &[&dyn PhenomenaSource], ctx: &ObserverContext) -> Vec<Phenomenon> {
    let mut all: Vec<Phenomenon> = sources.iter().flat_map(|s| s.phenomena(ctx)).collect();
    if !ctx.lens.is_identity() {
        for p in &mut all {
            let w = ctx.lens.weight(p.venue);
            p.salience = ((p.salience * w).clamp(0.0, 1.0) * 100.0).round() / 100.0;
        }
        all.retain(|p| p.salience >= VISIBILITY_FLOOR);
    }
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
        ObserverContext::at(EntityId::new(1).unwrap(), WorldTime { day: 0.0 })
    }

    fn ph(kind: &str, salience: f64) -> Phenomenon {
        Phenomenon {
            kind: kind.to_string(),
            description: format!("the {kind}"),
            period_days: None,
            salience,
            venue: Venue::Ambient,
        }
    }

    fn ph_venue(kind: &str, salience: f64, venue: Venue) -> Phenomenon {
        Phenomenon {
            venue,
            ..ph(kind, salience)
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

    #[test]
    fn identity_lens_is_a_byte_level_no_op() {
        // 0.3333 is chosen because round2(0.3333) = 0.33 != 0.3333: any
        // arithmetic leaking onto the identity path flips bits and fails
        // the assertion below. Identity must skip arithmetic entirely.
        let a = FixedSource(vec![ph("breeze", 0.3333), ph("sun", 1.0)]);
        let plain = observe(&[&a], &ctx());
        let via_identity = observe(
            &[&a],
            &ObserverContext {
                lens: PerceptionLens::identity(),
                ..ctx()
            },
        );
        assert_eq!(plain, via_identity);
        assert_eq!(plain[1].salience.to_bits(), 0.3333_f64.to_bits());
    }

    #[test]
    fn a_lens_reweights_by_venue_and_reranks() {
        let a = FixedSource(vec![
            ph_venue("sun", 1.0, Venue::DaySky),
            ph_venue("moon", 0.7, Venue::NightSky),
            ph_venue("air", 0.15, Venue::Ambient),
        ]);
        let lens = PerceptionLens {
            day_sky: 0.52,
            night_sky: 1.82,
            ambient: 0.70,
        };
        let out = observe(&[&a], &ObserverContext { lens, ..ctx() });
        // moon 0.7 × 1.82 = 1.274 → clamp 1.0; sun 1.0 × 0.52 = 0.52; air 0.15 × 0.7 = 0.11 (round2).
        assert_eq!(out[0].kind, "moon");
        assert_eq!(out[0].salience, 1.0);
        assert_eq!(out[1].kind, "sun");
        assert_eq!(out[1].salience, 0.52);
        assert_eq!(out[2].salience, 0.11);
    }

    #[test]
    fn lens_ties_break_by_kind_then_description() {
        // Two night phenomena both clamp to 1.0 under a strong lens.
        let a = FixedSource(vec![
            ph_venue("night-star", 0.6, Venue::NightSky),
            ph_venue("celestial-body", 0.7, Venue::NightSky),
        ]);
        let lens = PerceptionLens {
            day_sky: 1.0,
            night_sky: 1.82,
            ambient: 1.0,
        };
        let out = observe(&[&a], &ObserverContext { lens, ..ctx() });
        assert_eq!(out[0].kind, "celestial-body", "kind breaks the 1.0 tie");
    }

    #[test]
    fn at_is_position_blind_and_at_position_carries_a_coord() {
        let blind = ObserverContext::at(EntityId::new(1).unwrap(), WorldTime { day: 0.0 });
        assert!(blind.position.is_none());
        let placed = ObserverContext::at_position(
            EntityId::new(1).unwrap(),
            WorldTime { day: 0.0 },
            GeoCoord {
                latitude: 12.0,
                longitude: -30.0,
            },
        );
        assert_eq!(
            placed.position,
            Some(GeoCoord {
                latitude: 12.0,
                longitude: -30.0
            })
        );
    }

    #[test]
    fn observe_ignores_observer_position() {
        // Aggregation is position-agnostic; culling is the provider's job.
        let a = FixedSource(vec![ph("breeze", 0.3333), ph("sun", 1.0)]);
        let blind = observe(&[&a], &ctx());
        let placed = observe(
            &[&a],
            &ObserverContext {
                position: Some(GeoCoord {
                    latitude: 1.0,
                    longitude: 2.0,
                }),
                ..ctx()
            },
        );
        assert_eq!(blind, placed);
    }

    #[test]
    fn the_identity_lens_still_drops_nothing() {
        // The identity path performs no arithmetic, and the floor must not
        // retroactively cull a faint-but-real phenomenon on it.
        let faint = ph_venue("ember", 0.01, Venue::NightSky);
        let a = FixedSource(vec![faint.clone()]);
        assert_eq!(observe(&[&a], &ctx()), vec![faint]);
    }

    #[test]
    fn a_weighted_lens_culls_below_the_floor() {
        let a = FixedSource(vec![
            ph_venue("bright", 1.0, Venue::NightSky),
            ph_venue("faint", 0.1, Venue::NightSky),
        ]);
        let lens = PerceptionLens {
            day_sky: 1.0,
            night_sky: 0.2,
            ambient: 1.0,
        };
        let out = observe(&[&a], &ObserverContext { lens, ..ctx() });
        // bright: 1.0 × 0.2 = 0.20, survives. faint: 0.1 × 0.2 = 0.02, culled:
        // a star dimmed to a fiftieth is not a faint star, it is one you
        // cannot see.
        assert_eq!(out.len(), 1);
        assert_eq!(out[0].kind, "bright");
        assert_eq!(out[0].salience, 0.2);
    }

    #[test]
    fn lenses_compose_component_wise() {
        let a = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.4,
            ambient: 2.0,
        };
        let b = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.5,
            ambient: 0.5,
        };
        let c = a.compose(&b);
        assert_eq!(c.day_sky, 0.25);
        assert_eq!(c.night_sky, 0.2);
        assert_eq!(c.ambient, 1.0);
    }

    #[test]
    fn composing_with_identity_is_a_no_op() {
        let a = PerceptionLens {
            day_sky: 0.5,
            night_sky: 0.4,
            ambient: 2.0,
        };
        assert_eq!(a.compose(&PerceptionLens::identity()), a);
    }

    #[test]
    fn visibility_rejects_values_outside_the_unit_interval() {
        assert!(Visibility::new(-0.1).is_none());
        assert!(Visibility::new(1.1).is_none());
        assert!(Visibility::new(f64::NAN).is_none());
        assert_eq!(Visibility::new(0.5).map(|v| v.get()), Some(0.5));
        assert_eq!(Visibility::CLEAR.get(), 1.0);
    }
}
