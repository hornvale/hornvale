//! Fields: typed, lazily-evaluated functions over (space × time).
//! Fields are the "coarse" in coarse-constrains-fine (Constitution §2.2).

use crate::noise::fbm_2d;
use crate::seed::Seed;
use serde::{Deserialize, Serialize};

/// A location in world space. Units and topology are a terrain-domain
/// concern; the kernel only requires a metric-ish plane.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct Position {
    /// Horizontal coordinate.
    pub x: f64,
    /// Vertical coordinate.
    pub y: f64,
}

/// Simulated time in fractional days since world genesis. There is no
/// wall-clock time anywhere in Hornvale.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
pub struct WorldTime {
    /// Fractional days since world genesis.
    pub day: f64,
}

/// A typed field over (space × time). Implementations must be pure:
/// same (pos, time) → same value, always.
pub trait Field<T> {
    /// Sample the field's value at the given position and time.
    fn sample(&self, pos: Position, time: WorldTime) -> T;
}

/// The tier-0 field: the same value everywhere, forever.
#[derive(Clone, Debug)]
pub struct ConstantField<T: Clone>(
    /// The value returned for every position and time.
    pub T,
);

impl<T: Clone> Field<T> for ConstantField<T> {
    fn sample(&self, _pos: Position, _time: WorldTime) -> T {
        self.0.clone()
    }
}

/// A time-invariant fbm noise field in [0, 1). `scale` is the feature
/// wavelength in world units.
/// type-audit: bare-ok(count: octaves), pending(wave-1: scale)
#[derive(Clone, Copy, Debug)]
pub struct NoiseField {
    /// Seed driving the underlying noise stream.
    pub seed: Seed,
    /// Number of fbm octaves to accumulate.
    pub octaves: u32,
    /// Feature wavelength in world units.
    pub scale: f64,
}

impl Field<f64> for NoiseField {
    fn sample(&self, pos: Position, _time: WorldTime) -> f64 {
        fbm_2d(
            self.seed,
            pos.x / self.scale,
            pos.y / self.scale,
            self.octaves,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const NOON: WorldTime = WorldTime { day: 0.5 };

    #[test]
    fn constant_field_returns_its_value_everywhere() {
        let f = ConstantField(18.0_f64);
        assert_eq!(f.sample(Position { x: 0.0, y: 0.0 }, NOON), 18.0);
        assert_eq!(f.sample(Position { x: 1e6, y: -1e6 }, NOON), 18.0);
    }

    #[test]
    fn constant_field_works_for_non_numeric_types() {
        let f = ConstantField("temperate forest".to_string());
        assert_eq!(
            f.sample(Position { x: 3.0, y: 4.0 }, NOON),
            "temperate forest"
        );
    }

    #[test]
    fn noise_field_is_deterministic_and_bounded() {
        let f = NoiseField {
            seed: Seed(42),
            octaves: 3,
            scale: 10.0,
        };
        let p = Position { x: 12.5, y: -7.25 };
        let a = f.sample(p, NOON);
        assert_eq!(a, f.sample(p, NOON));
        assert!((0.0..1.0).contains(&a));
    }

    #[test]
    fn noise_field_scale_stretches_space() {
        // Two points one unit apart are nearly identical under a huge scale.
        let f = NoiseField {
            seed: Seed(42),
            octaves: 1,
            scale: 1000.0,
        };
        let a = f.sample(Position { x: 0.0, y: 0.0 }, NOON);
        let b = f.sample(Position { x: 1.0, y: 0.0 }, NOON);
        assert!((a - b).abs() < 0.01);
    }

    #[test]
    fn position_and_time_serialize_roundtrip() {
        let p = Position { x: 1.5, y: -2.5 };
        let t = WorldTime { day: 12.25 };
        let p2: Position = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();
        let t2: WorldTime = serde_json::from_str(&serde_json::to_string(&t).unwrap()).unwrap();
        assert_eq!((p2.x, p2.y, t2.day), (p.x, p.y, t.day));
    }
}
