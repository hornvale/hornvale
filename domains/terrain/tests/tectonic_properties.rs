//! The tectonic property battery (Campaign 3 spec §12): pin isolation here;
//! the N-seed invariant sweep joins in the final task.

use hornvale_kernel::{Geosphere, Seed};
use hornvale_terrain::{GenesisError, TerrainPins, generate, streams, summarize};

#[test]
fn pin_isolation_holds_at_the_globe_level() {
    let geo = Geosphere::new(4);
    let default = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();

    // Re-affirming the drawn plate count is byte-identical.
    let summary = summarize(&default.globe);
    let pins = TerrainPins {
        plates: Some(summary.plate_count),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);

    // Re-affirming the drawn ocean fraction (recovered by replaying its
    // labeled stream) is byte-identical.
    let drawn = 0.5
        + 0.25
            * Seed(42)
                .derive(streams::ROOT)
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let pins = TerrainPins {
        ocean_fraction: Some(drawn),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);

    // supercontinent=false re-affirms the drawn scattered layout.
    let pins = TerrainPins {
        supercontinent: Some(false),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);
}

#[test]
fn ocean_fraction_pin_perturbs_nothing_upstream() {
    let geo = Geosphere::new(4);
    let default = generate(Seed(7), &geo, &TerrainPins::default()).unwrap();
    let pinned = generate(
        Seed(7),
        &geo,
        &TerrainPins {
            ocean_fraction: Some(0.9),
            ..TerrainPins::default()
        },
    )
    .unwrap();
    assert_eq!(pinned.globe.elevation, default.globe.elevation);
    assert_eq!(pinned.globe.plate_of, default.globe.plate_of);
    assert_eq!(pinned.globe.unrest, default.globe.unrest);
    assert_ne!(pinned.globe.sea_level, default.globe.sea_level);
}

#[test]
fn out_of_range_pins_fail_loudly_with_the_reason() {
    let geo = Geosphere::new(2);
    let err = generate(
        Seed(1),
        &geo,
        &TerrainPins {
            plates: Some(200),
            ..TerrainPins::default()
        },
    )
    .unwrap_err();
    let GenesisError::InvalidPin { pin, reason } = err else {
        panic!("expected InvalidPin");
    };
    assert_eq!(pin, "plates");
    assert!(reason.contains("2-64"));
}
