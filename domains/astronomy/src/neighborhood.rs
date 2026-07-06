//! Notable neighbor stars: the objects that dominate a night sky.
//! Observational only (declared approximation): no physical effect on the
//! anchor. A full starfield and constellations are tier 3.

use crate::pins::{NeighborClass, SkyPins};
use crate::streams;
use crate::units::LightYears;
use hornvale_kernel::Seed;

/// A notable neighbor star, visible in the night sky.
#[derive(Debug, Clone, PartialEq)]
pub struct Neighbor {
    /// Spectral class (drawn or pinned).
    pub class: NeighborClass,
    /// Distance in light-years (drawn, 4–80).
    pub distance: LightYears,
    /// Apparent brightness, relative units (derived: L/d²).
    pub apparent_brightness: f64,
    /// Human-readable color character.
    pub color: String,
}

/// Luminosity of a spectral class in solar units (model card).
pub fn class_luminosity(class: NeighborClass) -> f64 {
    match class {
        NeighborClass::RedDwarf => 0.02,
        NeighborClass::SunLike => 1.0,
        NeighborClass::WhiteDwarf => 0.005,
        NeighborClass::OrangeGiant => 60.0,
        NeighborClass::RedGiant => 300.0,
        NeighborClass::BlueGiant => 10_000.0,
    }
}

fn class_color(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "dim red",
        NeighborClass::SunLike => "warm yellow",
        NeighborClass::WhiteDwarf => "pale white",
        NeighborClass::OrangeGiant => "deep orange",
        NeighborClass::RedGiant => "smoldering red",
        NeighborClass::BlueGiant => "hard blue-white",
    }
}

fn draw_class(roll: u32) -> NeighborClass {
    match roll {
        1..=40 => NeighborClass::RedDwarf,
        41..=65 => NeighborClass::SunLike,
        66..=75 => NeighborClass::WhiteDwarf,
        76..=85 => NeighborClass::OrangeGiant,
        86..=95 => NeighborClass::RedGiant,
        _ => NeighborClass::BlueGiant,
    }
}

/// Generate the notable neighbors, brightest first. The neighbor pin forces
/// the first star's class; the rest are drawn.
pub fn generate_neighbors(astronomy_seed: Seed, pins: &SkyPins) -> Vec<Neighbor> {
    let mut stream = astronomy_seed.derive(streams::NEIGHBORS).stream();
    let count = stream.range_u32(2, 5);
    let mut neighbors: Vec<Neighbor> = (0..count)
        .map(|index| {
            let roll = stream.range_u32(1, 100);
            let class = match (index, pins.neighbor) {
                (0, Some(pinned)) => pinned,
                _ => draw_class(roll),
            };
            let distance = LightYears(4.0 + stream.next_f64() * 76.0);
            Neighbor {
                class,
                distance,
                apparent_brightness: class_luminosity(class) / (distance.0 * distance.0),
                color: class_color(class).to_string(),
            }
        })
        .collect();
    neighbors.sort_by(|a, b| b.apparent_brightness.total_cmp(&a.apparent_brightness));
    neighbors
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn neighborhood_is_deterministic_and_sized() {
        let a = generate_neighbors(Seed(42), &SkyPins::default());
        assert_eq!(a, generate_neighbors(Seed(42), &SkyPins::default()));
        assert!((2..=5).contains(&a.len()));
    }

    #[test]
    fn brightness_is_derived_and_sorted_descending() {
        let neighbors = generate_neighbors(Seed(7), &SkyPins::default());
        for pair in neighbors.windows(2) {
            assert!(pair[0].apparent_brightness >= pair[1].apparent_brightness);
        }
        for n in &neighbors {
            let expected = class_luminosity(n.class) / (n.distance.get() * n.distance.get());
            assert!((n.apparent_brightness - expected).abs() < 1e-12);
            assert!((4.0..=80.0).contains(&n.distance.get()));
        }
    }

    #[test]
    fn neighbor_pin_leaves_the_rest_of_the_neighborhood_untouched() {
        let default = generate_neighbors(Seed(3), &SkyPins::default());
        let pins = SkyPins {
            neighbor: Some(NeighborClass::BlueGiant),
            ..SkyPins::default()
        };
        let pinned = generate_neighbors(Seed(3), &pins);

        assert_eq!(default.len(), pinned.len());

        let mut default_distances: Vec<f64> = default.iter().map(|n| n.distance.get()).collect();
        let mut pinned_distances: Vec<f64> = pinned.iter().map(|n| n.distance.get()).collect();
        default_distances.sort_by(f64::total_cmp);
        pinned_distances.sort_by(f64::total_cmp);
        assert_eq!(default_distances, pinned_distances);

        assert!(pinned.iter().any(|n| n.class == NeighborClass::BlueGiant));
    }

    #[test]
    fn neighbor_pin_forces_the_showpiece() {
        let pins = SkyPins {
            neighbor: Some(NeighborClass::BlueGiant),
            ..SkyPins::default()
        };
        let neighbors = generate_neighbors(Seed(3), &pins);
        assert!(
            neighbors
                .iter()
                .any(|n| n.class == NeighborClass::BlueGiant)
        );
        // A blue giant at 4–80 ly usually dominates; these seeds' draws make
        // it brightest here (asserted, not assumed).
        assert_eq!(neighbors[0].class, NeighborClass::BlueGiant);
        assert_eq!(neighbors[0].color, "hard blue-white");
    }
}
