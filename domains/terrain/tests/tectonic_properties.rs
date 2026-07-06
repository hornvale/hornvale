//! The tectonic property battery (Campaign 3 spec §12): pin isolation, plus
//! the N-seed invariant sweep (one-plate-per-cell, boundary agreement,
//! elevation envelope, ocean-fraction tolerance, determinism, and the
//! convergent-vs-interior elevation contrast).

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

#[test]
fn every_default_globe_satisfies_every_invariant() {
    let geo = Geosphere::new(4);
    for seed in 0..64u64 {
        let outcome = generate(Seed(seed), &geo, &TerrainPins::default())
            .unwrap_or_else(|e| panic!("seed {seed} failed default genesis: {e}"));
        let globe = &outcome.globe;
        let plate_count = globe.plates.len() as u32;
        assert!(
            (8..=40).contains(&plate_count),
            "seed {seed}: plate count {plate_count}"
        );
        assert_eq!(globe.plate_of.len(), geo.cell_count());
        for (cell, plate) in globe.plate_of.iter() {
            assert!(
                (*plate as usize) < globe.plates.len(),
                "seed {seed}: cell {} in nonexistent plate {plate}",
                cell.0
            );
        }
        for (cell, e) in globe.elevation.iter() {
            assert!(
                e.is_finite() && (-12000.0..=12000.0).contains(e),
                "seed {seed}: cell {} elevation {e} out of envelope",
                cell.0
            );
        }
        for (_, u) in globe.unrest.iter() {
            assert!((0.0..=1.0).contains(u), "seed {seed}: unrest {u}");
        }
        let summary = summarize(globe);
        assert!(
            (0.49..=0.76).contains(&summary.ocean_fraction),
            "seed {seed}: ocean fraction {} misses the drawn range",
            summary.ocean_fraction
        );
        assert!(summary.highest_elevation_m > summary.sea_level_m);
    }
}

#[test]
fn boundary_classification_agrees_from_both_sides_across_seeds() {
    use hornvale_terrain::boundaries::classify_contact;
    use hornvale_terrain::plates::{assign_plates, generate_plates};
    let geo = Geosphere::new(3);
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let plates = generate_plates(terrain_seed, &TerrainPins::default());
        let plate_of = assign_plates(&geo, &plates);
        for a in geo.cells() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(&geo, a, b, &plates[pa as usize], &plates[pb as usize]);
                let ba = classify_contact(&geo, b, a, &plates[pb as usize], &plates[pa as usize]);
                assert_eq!(ab.kind, ba.kind, "seed {seed}: {}-{}", a.0, b.0);
                assert_eq!(ab.magnitude, ba.magnitude, "seed {seed}: {}-{}", a.0, b.0);
            }
        }
    }
}

#[test]
fn genesis_is_deterministic_across_the_sweep() {
    let geo = Geosphere::new(4);
    for seed in [0u64, 17, 42, 63] {
        let a = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        assert_eq!(a, b, "seed {seed}");
    }
}

#[test]
fn convergent_boundaries_stand_above_continental_interiors_on_average() {
    use hornvale_terrain::BoundaryKind;
    use hornvale_terrain::boundaries::{boundary_distance, boundary_field};
    use hornvale_terrain::plates::{assign_plates, generate_plates};
    let geo = Geosphere::new(4);
    let mut uplifted = Vec::new();
    let mut interior = Vec::new();
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let pins = TerrainPins::default();
        let plates = generate_plates(terrain_seed, &pins);
        let plate_of = assign_plates(&geo, &plates);
        let boundaries = boundary_field(&geo, &plate_of, &plates);
        let distances = boundary_distance(&geo, &plate_of, &boundaries);
        let globe = generate(Seed(seed), &geo, &pins).unwrap().globe;
        for (cell, contact) in boundaries.iter() {
            let continental = plates[*plate_of.get(cell) as usize].continental;
            match contact {
                Some(c)
                    if continental
                        && matches!(
                            c.kind,
                            BoundaryKind::ContinentalCollision | BoundaryKind::CoastalRange
                        ) =>
                {
                    uplifted.push(*globe.elevation.get(cell));
                }
                None => {
                    if continental
                        && let Some((distance, _)) = distances.get(cell)
                        && *distance >= 6
                    {
                        interior.push(*globe.elevation.get(cell));
                    }
                }
                _ => {}
            }
        }
    }
    assert!(!uplifted.is_empty() && !interior.is_empty());
    let mean = |v: &[f64]| v.iter().sum::<f64>() / v.len() as f64;
    assert!(
        mean(&uplifted) > mean(&interior),
        "convergent mean {} not above interior mean {}",
        mean(&uplifted),
        mean(&interior)
    );
}
