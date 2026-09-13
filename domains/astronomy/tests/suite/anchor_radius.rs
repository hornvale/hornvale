use hornvale_astronomy::{EarthMasses, anchor_radius};

#[test]
fn radius_matches_a_published_interior_reference_point() {
    let radius = anchor_radius(EarthMasses::new(1.115).unwrap()).unwrap();
    assert!((radius.get() - 1.0309 * 6.371).abs() < 1e-12);
}

#[test]
fn unsupported_mass_is_not_silently_extrapolated() {
    assert!(anchor_radius(EarthMasses::new(0.49).unwrap()).is_err());
}

#[test]
fn supported_boundaries_are_interpolated_from_the_covering_curve() {
    let cases = [(0.5, 0.8178125516102395), (2.0, 1.2113332885906039)];
    for (mass, expected_earth_radii) in cases {
        let radius = anchor_radius(EarthMasses::new(mass).unwrap()).unwrap();
        assert!(
            (radius.get() - expected_earth_radii * 6.371).abs() < 1e-12,
            "mass {mass}"
        );
    }
}

#[test]
fn every_supported_tabulated_reference_is_exact() {
    let cases = [
        (0.5304, 0.8330),
        (0.6835, 0.8964),
        (0.8756, 0.9625),
        (1.1150, 1.0309),
        (1.4114, 1.1015),
        (1.7763, 1.1741),
    ];
    for (mass, expected_earth_radii) in cases {
        let radius = anchor_radius(EarthMasses::new(mass).unwrap()).unwrap();
        assert_eq!(radius.get(), expected_earth_radii * 6.371, "mass {mass}");
    }
}

#[test]
fn radius_is_positive_and_monotonic_across_supported_masses() {
    let mut previous = 0.0;
    for step in 0..=150 {
        let mass = 0.5 + f64::from(step) / 100.0;
        let radius = anchor_radius(EarthMasses::new(mass).unwrap())
            .unwrap()
            .get();
        assert!(radius > 0.0, "mass {mass}");
        assert!(radius > previous, "mass {mass}");
        previous = radius;
    }
}

#[test]
fn one_earth_mass_is_within_one_percent_of_earths_reference_radius() {
    let radius = anchor_radius(EarthMasses::new(1.0).unwrap()).unwrap();
    assert!((radius.get() / 6.371 - 1.0).abs() < 0.01);
}

#[test]
fn mass_above_the_supported_range_is_rejected_with_context() {
    let error = anchor_radius(EarthMasses::new(2.01).unwrap()).unwrap_err();
    assert_eq!(error.value, 2.01);
    assert!(error.reason.contains("0.5–2 Earth masses"));
}
