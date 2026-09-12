//! Modeled neighboring stars and their compatibility observation view.
//! The bounded catalog has physical states but no effect on the inhabited
//! anchor. The legacy notable subset stays stable until its sky readers migrate.

use crate::pins::{NeighborClass, SkyPins};
use crate::streams;
use crate::units::LightYears;
use hornvale_kernel::Seed;
use hornvale_kernel::math;

/// Lower bound of the modeled catalog, including the legacy notable subset.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const MIN_CATALOG_STARS: u32 = 24;

/// Upper bound: retain tens of modeled objects, never the background field.
/// type-audit: bare-ok(count)
/// plumb: pending(wave-1)
pub const MAX_CATALOG_STARS: u32 = 40;

/// Stable catalog identity, independent of brightness rank or pin choice.
/// type-audit: bare-ok(constructor-edge)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct CatalogStarId(pub u64);

/// A physically modeled neighbor with a stable identity and sky location.
/// [`Neighbor`] is its source-compatible observational projection. Keeping
/// the legacy subset separate lets the figure pipeline migrate in Task 6.
/// type-audit: pending(wave-1: declination), pending(wave-1: right_ascension)
#[derive(Debug, Clone, PartialEq)]
pub struct CatalogStar {
    /// Seed-derived identity assigned before any brightness sort.
    pub id: CatalogStarId,
    /// Distance from the inhabited system.
    pub distance: LightYears,
    /// Genesis declination, degrees in the anchor's equatorial frame.
    pub declination: f64,
    /// Genesis right ascension, degrees in the anchor's equatorial frame.
    pub right_ascension: f64,
    /// Physical state at genesis, separate from the stable host.
    pub stellar: crate::star::NeighborStar,
    legacy: bool,
}

impl CatalogStar {
    /// Derived compatibility view, with the existing relative `L / d²`
    /// brightness convention (solar luminosities and light-years).
    pub fn neighbor(&self) -> Neighbor {
        let class = self.stellar.class();
        Neighbor {
            class,
            distance: self.distance,
            apparent_brightness: self.stellar.luminosity.get()
                / (self.distance.get() * self.distance.get()),
            color: class_color(class).to_string(),
            declination: self.declination,
            right_ascension: self.right_ascension,
        }
    }

    /// The old notable-neighbor roster only. Expanded catalog members await
    /// the stable-ID/brightness migration of the figure and night-sky readers.
    pub fn compatibility_neighbor(&self) -> Option<Neighbor> {
        self.legacy.then(|| self.neighbor())
    }
}

/// Build the bounded modeled catalog in stable identity-assignment order.
pub fn generate_catalog(astronomy_seed: Seed, pins: &SkyPins) -> Vec<CatalogStar> {
    let mut old = astronomy_seed.derive(streams::NEIGHBORS).stream();
    let mut old_positions = astronomy_seed.derive(streams::NEIGHBOR_POSITIONS).stream();
    let legacy_count = old.range_u32(2, 5);
    let count = astronomy_seed
        .derive(streams::CATALOG_COUNT)
        .stream()
        .range_u32(MIN_CATALOG_STARS, MAX_CATALOG_STARS);
    let mut identities = astronomy_seed.derive(streams::CATALOG_IDENTITIES).stream();
    let mut physics = astronomy_seed.derive(streams::CATALOG_PHYSICS).stream();
    let mut positions = astronomy_seed.derive(streams::CATALOG_POSITIONS).stream();
    (0..count)
        .map(|index| {
            // Fixed draws per identity, even when a pin or legacy constraint
            // overrides a physical value. Extra members never consume old draws.
            let mass_draw = physics.next_f64();
            let age_draw = physics.next_f64();
            let legacy = index < legacy_count;
            let (stellar, distance, declination, right_ascension) = if legacy {
                let roll = old.range_u32(1, 100);
                let class = match (index, pins.neighbor) {
                    (0, Some(pinned)) => pinned,
                    _ => draw_class(roll),
                };
                let distance = LightYears(4.0 + old.next_f64() * 76.0);
                let declination = math::asin(old_positions.next_f64() * 2.0 - 1.0).to_degrees();
                let right_ascension = old_positions.next_f64() * 360.0;
                (
                    crate::star::neighbor_with_legacy_class(class, age_draw),
                    distance,
                    declination,
                    right_ascension,
                )
            } else {
                let stellar = crate::star::draw_neighbor(mass_draw, age_draw);
                let distance = LightYears(4.0 + positions.next_f64() * 496.0);
                let declination = math::asin(positions.next_f64() * 2.0 - 1.0).to_degrees();
                let right_ascension = positions.next_f64() * 360.0;
                (stellar, distance, declination, right_ascension)
            };
            CatalogStar {
                id: CatalogStarId(identities.next_u64()),
                stellar,
                distance,
                declination,
                right_ascension,
                legacy,
            }
        })
        .collect()
}

/// Compatibility observation of a modeled catalog star. Kept as the original
/// struct shape for existing scene/provider callers and authored fixtures.
/// type-audit: bare-ok(ratio: apparent_brightness), bare-ok(identifier-text: color), pending(wave-1: declination), pending(wave-1: right_ascension)
#[derive(Debug, Clone, PartialEq)]
pub struct Neighbor {
    /// Coarse spectral class, derived from the catalog's physical state.
    pub class: NeighborClass,
    /// Distance in light-years (legacy subset: 4–80; expanded catalog: 4–500).
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
    /// The historical class description used in chart legends.
    /// type-audit: bare-ok(identifier-text)
    pub fn class_name(&self) -> &'static str {
        class_name(self.class)
    }

    /// The existing registered class concept, derived with the same taxonomy.
    /// type-audit: bare-ok(identifier-text)
    pub fn class_concept(&self) -> &'static str {
        class_concept(self.class)
    }

    /// The normative night-sky description of this star.
    /// type-audit: bare-ok(prose)
    pub fn night_description(&self) -> String {
        format!("a {} star that does not wander", self.color)
    }
}

/// Historical luminosity anchor in solar units (compatibility model card).
/// Expanded catalog callers read `CatalogStar::stellar.luminosity` instead:
/// physical luminosity varies within a class.
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
    compatibility_neighbors(&generate_catalog(astronomy_seed, pins))
}

pub(crate) fn compatibility_neighbors(catalog: &[CatalogStar]) -> Vec<Neighbor> {
    let mut neighbors: Vec<_> = catalog
        .iter()
        .filter_map(CatalogStar::compatibility_neighbor)
        .collect();
    neighbors.sort_by(|a, b| b.apparent_brightness.total_cmp(&a.apparent_brightness));
    neighbors
}

#[cfg(test)]
mod tests {
    use super::*;

    /// claim: invariant(catalog bounds, identity uniqueness, deterministic
    /// regeneration and compatibility projection over a bounded seed sample)
    #[test]
    fn catalog_is_bounded_stable_and_preserves_the_legacy_view() {
        for seed in 0..32 {
            let seed = Seed(seed);
            let catalog = generate_catalog(seed, &SkyPins::default());
            assert!((24..=40).contains(&catalog.len()));
            assert_eq!(catalog, generate_catalog(seed, &SkyPins::default()));
            let mut ids: Vec<_> = catalog.iter().map(|star| star.id).collect();
            ids.sort();
            ids.dedup();
            assert_eq!(ids.len(), catalog.len());
            let mut legacy: Vec<_> = catalog
                .iter()
                .filter_map(CatalogStar::compatibility_neighbor)
                .collect();
            legacy.sort_by(|a, b| b.apparent_brightness.total_cmp(&a.apparent_brightness));
            assert_eq!(legacy, generate_neighbors(seed, &SkyPins::default()));
            for star in &catalog {
                let view = star.neighbor();
                assert_eq!(view.class, star.stellar.class());
                assert_eq!(
                    view.apparent_brightness,
                    star.stellar.luminosity.get() / star.distance.get().powi(2)
                );
                assert!((0.0..360.0).contains(&star.right_ascension));
                assert!((-90.0..=90.0).contains(&star.declination));
                assert!(star.distance.get() > 0.0);
                assert!(view.apparent_brightness.is_finite() && view.apparent_brightness > 0.0);
                assert_eq!(
                    view.night_description(),
                    format!("a {} star that does not wander", view.color)
                );
            }
        }
    }

    #[test]
    fn catalog_pin_changes_only_the_first_identitys_physics() {
        let seed = Seed(42);
        let plain = generate_catalog(seed, &SkyPins::default());
        let pinned = generate_catalog(
            seed,
            &SkyPins {
                neighbor: Some(NeighborClass::BlueGiant),
                ..SkyPins::default()
            },
        );
        assert_eq!(plain.len(), pinned.len());
        assert_eq!(plain[0].id, pinned[0].id);
        assert_eq!(plain[0].distance, pinned[0].distance);
        assert_eq!(plain[0].right_ascension, pinned[0].right_ascension);
        assert_eq!(plain[0].declination, pinned[0].declination);
        assert_eq!(pinned[0].stellar.class(), NeighborClass::BlueGiant);
        assert_eq!(plain[1..], pinned[1..]);
        assert_ne!(
            plain[0].id,
            generate_catalog(Seed(43), &SkyPins::default())[0].id
        );
    }

    #[test]
    fn doubling_catalog_distance_quarters_brightness_without_changing_identity() {
        let a = generate_catalog(Seed(7), &SkyPins::default()).remove(0);
        let mut b = a.clone();
        b.distance = LightYears(a.distance.get() * 2.0);
        assert_eq!(a.id, b.id);
        assert_eq!(
            a.neighbor().apparent_brightness / 4.0,
            b.neighbor().apparent_brightness
        );
    }

    #[test]
    fn legacy_classes_are_physical_constraints_and_keep_registered_descriptions() {
        for class in [
            NeighborClass::RedDwarf,
            NeighborClass::SunLike,
            NeighborClass::WhiteDwarf,
            NeighborClass::OrangeGiant,
            NeighborClass::RedGiant,
            NeighborClass::BlueGiant,
        ] {
            let catalog = generate_catalog(
                Seed(42),
                &SkyPins {
                    neighbor: Some(class),
                    ..SkyPins::default()
                },
            );
            let star = &catalog[0];
            assert_eq!(star.stellar.class(), class);
            assert_eq!(star.stellar.luminosity.get(), class_luminosity(class));
            let physical = crate::star::NeighborStar::from_mass_age(
                star.stellar.initial_mass,
                star.stellar.age,
            )
            .unwrap();
            assert_eq!(physical.stage, star.stellar.stage);
            assert!(
                (physical.luminosity.get() / star.stellar.luminosity.get() - 1.0).abs() < 1e-12
            );
            let view = star.neighbor();
            assert_eq!(view.class_name(), class_name(class));
            assert_eq!(view.class_concept(), class_concept(class));
        }
    }

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
