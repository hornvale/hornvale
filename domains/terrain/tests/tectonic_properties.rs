//! The tectonic property battery (Campaign 3 spec §12): pin isolation, plus
//! the N-seed invariant sweep (one-plate-per-cell, boundary agreement,
//! elevation envelope, ocean-fraction tolerance, determinism, and the
//! convergent-vs-interior elevation contrast).

use hornvale_kernel::{CellMap, Geosphere, Seed};
use hornvale_terrain::{GenesisError, TerrainPins, generate, streams, summarize};

#[test]
fn pin_isolation_holds_at_the_globe_level() {
    let geo = Geosphere::new(4);
    let default = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();

    // Re-affirming the drawn plate count leaves the globe byte-identical
    // (the notes gain a metering entry — Task 7 — since the pin is `Some`
    // regardless of whether its value matches the drawn one; metering is
    // pin-only, not value-only, by design).
    let summary = summarize(&default.globe);
    let pins = TerrainPins {
        plates: Some(summary.plate_count),
        ..TerrainPins::default()
    };
    assert_eq!(
        generate(Seed(42), &geo, &pins).unwrap().globe,
        default.globe
    );

    // Re-affirming the drawn ocean fraction (recovered by replaying its
    // labeled stream) leaves the globe byte-identical, same caveat.
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
    assert_eq!(
        generate(Seed(42), &geo, &pins).unwrap().globe,
        default.globe
    );

    // supercontinent=false re-affirms the drawn scattered layout.
    let pins = TerrainPins {
        supercontinent: Some(false),
        ..TerrainPins::default()
    };
    assert_eq!(generate(Seed(42), &geo, &pins).unwrap(), default);

    // Re-affirming the drawn craton count (Crust epoch, Task 8) leaves the
    // globe byte-identical too — same pin-isolation caveat as plates and
    // ocean-fraction above.
    let pins = TerrainPins {
        continents: Some(default.globe.cratons.len() as u32),
        ..TerrainPins::default()
    };
    assert_eq!(
        generate(Seed(42), &geo, &pins).unwrap().globe,
        default.globe
    );

    // supercontinent on cratons (Crust epoch, Task 8): scattered vs
    // supercontinent=false is byte-identical (the same re-affirmation
    // guarantee as plates', now on the craton draw).
    let pins = TerrainPins {
        continents: Some(default.globe.cratons.len() as u32),
        supercontinent: Some(false),
        ..TerrainPins::default()
    };
    assert_eq!(
        generate(Seed(42), &geo, &pins).unwrap().globe,
        default.globe
    );
}

#[test]
fn ocean_fraction_pin_conditions_cratons_but_not_the_plate_skeleton() {
    // Task 9 iteration 3': the ocean-fraction target now feeds the
    // craton-area budget too (see `hornvale_terrain::crust::draw_cratons`'s
    // doc), so pinning it legitimately conditions craton radii and, in
    // turn, the crust/continental fields, elevation, and unrest — a
    // pinned target conditions downstream identically to a drawn one
    // (pin doctrine). What must stay untouched is the plate skeleton
    // (built from streams independent of ocean fraction) and each
    // shared-prefix craton's age (a raw stream draw the area-budget
    // rescale never touches).
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
    assert_eq!(
        pinned.globe.plates, default.globe.plates,
        "plate skeleton perturbed"
    );
    assert_eq!(
        pinned.globe.plate_of, default.globe.plate_of,
        "plate assignment perturbed"
    );
    for (a, b) in default.globe.cratons.iter().zip(&pinned.globe.cratons) {
        assert_eq!(a.id, b.id);
        assert_eq!(
            a.age, b.age,
            "shared-prefix craton {} age perturbed by the ocean-fraction pin",
            a.id
        );
    }
    assert_ne!(
        pinned.globe.crust, default.globe.crust,
        "ocean-fraction pin should move craton radii (and therefore crust)"
    );
    assert_ne!(
        pinned.globe.sea_level, default.globe.sea_level,
        "ocean-fraction pin should move sea level"
    );
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
            let e = e.get();
            assert!(
                e.is_finite() && (-12000.0..=12000.0).contains(&e),
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
    use hornvale_terrain::crust::{CrustField, draw_cratons};
    use hornvale_terrain::elevation::resolve_ocean_fraction;
    use hornvale_terrain::plates::{assign_plates, generate_plates};
    let geo = Geosphere::new(3);
    for seed in 0..16u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let plates = generate_plates(terrain_seed, &TerrainPins::default(), &mut Vec::new());
        let plate_of = assign_plates(&geo, terrain_seed, &plates);
        let ocean_target =
            resolve_ocean_fraction(terrain_seed, &TerrainPins::default(), &mut Vec::new());
        let cratons = draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        );
        let field = CrustField::new(terrain_seed, cratons);
        let continental = CellMap::from_fn(&geo, |c| field.continental_at(geo.position(c)));
        for a in geo.cells() {
            for &b in geo.neighbors(a) {
                let (pa, pb) = (*plate_of.get(a), *plate_of.get(b));
                if pa == pb {
                    continue;
                }
                let ab = classify_contact(
                    &geo,
                    a,
                    b,
                    &plates[pa as usize],
                    &plates[pb as usize],
                    *continental.get(a),
                    *continental.get(b),
                );
                let ba = classify_contact(
                    &geo,
                    b,
                    a,
                    &plates[pb as usize],
                    &plates[pa as usize],
                    *continental.get(b),
                    *continental.get(a),
                );
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
    use hornvale_terrain::boundaries::boundary_distance;
    use hornvale_terrain::crust::CONTINENTAL_THRESHOLD_KM;
    let geo = Geosphere::new(4);
    let mut uplifted = Vec::new();
    let mut interior = Vec::new();
    for seed in 0..16u64 {
        let pins = TerrainPins::default();
        let globe = generate(Seed(seed), &geo, &pins).unwrap().globe;
        let distances = boundary_distance(&geo, &globe.plate_of, &globe.boundary);
        for (cell, contact) in globe.boundary.iter() {
            let continental = *globe.crust.get(cell) >= CONTINENTAL_THRESHOLD_KM;
            match contact {
                Some(c)
                    if continental
                        && matches!(
                            c.kind,
                            BoundaryKind::ContinentalCollision | BoundaryKind::CoastalRange
                        ) =>
                {
                    uplifted.push(globe.elevation.get(cell).get());
                }
                None => {
                    if continental
                        && let Some((distance, _)) = distances.get(cell)
                        && *distance >= 6
                    {
                        interior.push(globe.elevation.get(cell).get());
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

#[test]
fn single_craton_worlds_have_shelves_and_bimodal_hypsometry_across_the_sweep() {
    use hornvale_terrain::shape::{hypsometric_bimodality, shelf_fraction, shelf_land_ratio};
    let geo = Geosphere::new(4);
    let pins = TerrainPins {
        continents: Some(1),
        ..TerrainPins::default()
    };
    for seed in 1..=40u64 {
        let outcome =
            generate(Seed(seed), &geo, &pins).unwrap_or_else(|e| panic!("seed {seed}: {e}"));
        // The fallback must actually engage — a vacuous pass through the
        // percentile path would mean the activation condition drifted.
        assert!(
            outcome.notes.iter().any(|n| n.contains("shelf break")),
            "seed {seed}: fallback never engaged: {:?}",
            outcome.notes
        );
        let globe = &outcome.globe;
        let d =
            hypsometric_bimodality(&globe.elevation, globe.sea_level).expect("has land and ocean");
        assert!(d > 1.5, "seed {seed}: hypsometry not bimodal: D = {d}");
        // Land-normalized floor + absolute ceiling (decision 0053): the
        // ceiling still guards the drowned-into-the-abyss failure mode.
        let shelf_land = shelf_land_ratio(&globe.elevation, globe.sea_level).expect("has land");
        assert!(
            shelf_land > 0.05,
            "seed {seed}: no shelf band relative to land: {shelf_land}"
        );
        let shelf = shelf_fraction(&globe.elevation, globe.sea_level);
        assert!(shelf < 0.5, "seed {seed}: everything is shelf: {shelf}");
    }
}

#[test]
fn default_worlds_never_trip_the_supply_fallback() {
    use hornvale_terrain::crust::{continental_supply, draw_cratons};
    use hornvale_terrain::elevation::{
        SUPPLY_SHORTFALL_FACTOR, effective_ocean_target, resolve_ocean_fraction,
    };
    // Craton-level (no genesis): cheap, and grid-free by construction —
    // this is the byte-identity proof that the fallback cannot rewrite
    // default worlds (whose frozen census fixtures and seed-42 artifacts
    // must not drift).
    for seed in 0..64u64 {
        let terrain_seed = Seed(seed).derive(streams::ROOT);
        let ocean_target =
            resolve_ocean_fraction(terrain_seed, &TerrainPins::default(), &mut Vec::new());
        let cratons = draw_cratons(
            terrain_seed,
            &TerrainPins::default(),
            ocean_target,
            &mut Vec::new(),
        );
        let supply = continental_supply(&cratons);
        let quota = 1.0 - ocean_target;
        assert!(
            supply >= SUPPLY_SHORTFALL_FACTOR * quota,
            "seed {seed}: default draw is supply-limited (supply {supply:.3} vs quota \
             {quota:.3}) — the fallback would rewrite default worlds and drift every \
             committed artifact"
        );
        let mut notes = Vec::new();
        assert_eq!(
            effective_ocean_target(ocean_target, supply, &mut notes),
            ocean_target,
            "seed {seed}: effective target diverged from the pinned-percentile path"
        );
        assert!(notes.is_empty(), "seed {seed}: {notes:?}");
    }
}
