//! Golden values for seed 42's default system: captured before the typed-
//! quantities retrofit, asserted after. Exact f64 equality — determinism
//! is constitutional.

use hornvale_astronomy::{SkyPins, generate};
use hornvale_kernel::Seed;

#[test]
fn seed_42_default_system_matches_the_pre_retrofit_golden_values() {
    let s = generate(Seed(42), &SkyPins::default()).unwrap();

    assert_eq!(s.star.mass.get(), 0.9034065100014939);
    assert_eq!(s.star.luminosity.get(), 0.7007954206338779);
    assert_eq!(s.anchor.orbit.get(), 0.9716464747336115);
    assert_eq!(s.anchor.year.get(), 368.05357093462703);
    assert_eq!(s.anchor.obliquity.get(), 0.9593056670606165);

    let moons: Vec<(f64, f64)> = s
        .moons
        .iter()
        .map(|m| (m.distance.get(), m.period.get()))
        .collect();
    assert_eq!(
        moons,
        vec![
            (307.7443854122819, 15.993805342676167),
            (494.27357925778716, 32.55499951908771),
        ]
    );

    assert_eq!(s.neighbors.len(), 5);
    assert_eq!(s.neighbors[0].distance.get(), 68.2322807078267);
}
