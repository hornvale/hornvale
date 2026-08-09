//! Notable neighbor stars: the objects that dominate a night sky.
//! Observational only (declared approximation): no physical effect on the
//! anchor. A full starfield and constellations are tier 3.

use crate::pins::{NeighborClass, SkyPins};
use crate::streams;
use crate::units::LightYears;
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// A notable neighbor star, visible in the night sky.
/// type-audit: bare-ok(ratio: apparent_brightness), bare-ok(identifier-text: color), pending(wave-1: declination), pending(wave-1: right_ascension)
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
    /// Declination in degrees from the celestial equator (drawn, −90…90).
    /// The celestial equator is the anchor's rotational equator (spec §3).
    pub declination: f64,
    /// Right ascension in degrees (drawn, 0…360).
    pub right_ascension: f64,
}

impl Neighbor {
    /// The normative night-sky description of this star.
    /// type-audit: bare-ok(prose)
    pub fn night_description(&self) -> String {
        format!("a {} star that does not wander", self.color)
    }
}

/// Luminosity of a spectral class in solar units (model card).
/// type-audit: pending(wave-1)
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

/// The prose name of a spectral class, for chart legends.
/// type-audit: bare-ok(identifier-text)
pub fn class_name(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red dwarf",
        NeighborClass::SunLike => "sun-like star",
        NeighborClass::WhiteDwarf => "white dwarf",
        NeighborClass::OrangeGiant => "orange giant",
        NeighborClass::RedGiant => "red giant",
        NeighborClass::BlueGiant => "blue giant",
    }
}

/// The registered concept for a neighbour of this class. Total by
/// construction: a new variant fails to compile here, so no call site needs a
/// fallible lookup.
/// type-audit: bare-ok(identifier-text: return)
pub fn class_concept(class: NeighborClass) -> &'static str {
    match class {
        NeighborClass::RedDwarf => "red-dwarf",
        NeighborClass::SunLike => "sun-like-star",
        NeighborClass::WhiteDwarf => "white-dwarf",
        NeighborClass::OrangeGiant => "orange-giant",
        NeighborClass::RedGiant => "red-giant",
        NeighborClass::BlueGiant => "blue-giant",
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
    let mut positions = astronomy_seed.derive(streams::NEIGHBOR_POSITIONS).stream();
    let count = stream.range_u32(2, 5);
    let mut neighbors: Vec<Neighbor> = (0..count)
        .map(|index| {
            let roll = stream.range_u32(1, 100);
            let class = match (index, pins.neighbor) {
                (0, Some(pinned)) => pinned,
                _ => draw_class(roll),
            };
            let distance = LightYears(4.0 + stream.next_f64() * 76.0);
            let declination = math::asin(positions.next_f64() * 2.0 - 1.0).to_degrees();
            let right_ascension = positions.next_f64() * 360.0;
            Neighbor {
                class,
                distance,
                apparent_brightness: class_luminosity(class) / (distance.0 * distance.0),
                color: class_color(class).to_string(),
                declination,
                right_ascension,
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
    fn positions_are_on_the_sphere_and_deterministic() {
        let seed = Seed(7).derive(streams::ROOT);
        let a = generate_neighbors(seed, &SkyPins::default());
        let b = generate_neighbors(seed, &SkyPins::default());
        assert_eq!(a, b);
        for n in &a {
            assert!(
                (-90.0..=90.0).contains(&n.declination),
                "dec {}",
                n.declination
            );
            assert!(
                (0.0..360.0).contains(&n.right_ascension),
                "ra {}",
                n.right_ascension
            );
        }
    }

    #[test]
    fn night_description_names_the_color_and_the_stillness() {
        let neighbors = generate_neighbors(Seed(3), &SkyPins::default());
        let neighbor = &neighbors[0];
        assert_eq!(
            neighbor.night_description(),
            format!("a {} star that does not wander", neighbor.color)
        );
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

    /// Every neighbour class maps into the spectral table the ledger commits
    /// through. `facts.rs` `.expect()`s this lookup, so a drift between the two
    /// tables is a panic on whatever seed first draws the orphaned variant —
    /// seed 42 draws only five of the six.
    ///
    /// The `match` below has no wildcard arm, so adding a `NeighborClass`
    /// variant fails to compile right here, forcing an author to visit this
    /// test — the same shape of guard `class_name`'s own `match` already
    /// gives the display table, now extended to this test instead of
    /// stopping at `class_name`. That is real value, but it is narrower than
    /// it may look: the compile error is fixed by extending the or-pattern,
    /// which does not oblige anyone to also extend the array above. The
    /// array itself stays hand-kept in sync — proven by mutation: deleting
    /// `NeighborClass::BlueGiant` from the array (leaving the `match` arms
    /// untouched) still compiles and still passes, silently testing five of
    /// six variants. Read this test as "the compiler points you here," not
    /// as "the array cannot drift."
    #[test]
    fn every_neighbour_class_is_in_the_spectral_table() {
        fn assert_covered(class: NeighborClass) {
            let display = class_name(class);
            assert!(
                crate::star::class_concept(display).is_some(),
                "{class:?} mints {display:?}, which SPECTRAL_CLASSES does not carry"
            );
        }

        for class in [
            NeighborClass::RedDwarf,
            NeighborClass::SunLike,
            NeighborClass::WhiteDwarf,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::BlueGiant,
        ] {
            match class {
                NeighborClass::RedDwarf
                | NeighborClass::SunLike
                | NeighborClass::WhiteDwarf
                | NeighborClass::OrangeGiant
                | NeighborClass::RedGiant
                | NeighborClass::BlueGiant => assert_covered(class),
            }
        }
    }

    /// Every variant maps to a concept totally — no lookup, no Option, so no
    /// call site needs an `.expect()`.
    #[test]
    fn every_variant_derives_a_concept_agreeing_with_its_display() {
        for class in [
            NeighborClass::RedDwarf,
            NeighborClass::SunLike,
            NeighborClass::WhiteDwarf,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::BlueGiant,
        ] {
            assert_eq!(
                crate::star::tests::display_of(class_concept(class)),
                Some(class_name(class)),
                "{} derives a concept whose display disagrees with class_name",
                class_name(class)
            );
        }
    }
}
