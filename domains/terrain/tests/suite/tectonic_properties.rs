//! The tectonic property battery (Campaign 3 spec §12): pin isolation, plus
//! the N-seed invariant sweep (one-plate-per-cell, boundary agreement,
//! elevation envelope, ocean-fraction tolerance, determinism, and the
//! convergent-vs-interior elevation contrast).

use hornvale_kernel::{CellMap, Geosphere, Seed};
use hornvale_terrain::{GeneratedTerrain, GenesisError, TerrainPins, generate, streams, summarize};

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
fn pin_isolation_extends_to_new_streams() {
    // Sculpting Task 13: terranes (`terrain/terranes`) and microcontinents
    // (`terrain/microcontinents`) are new drawn sets with their own stream
    // labels (spec §6). Neither is itself pinnable (no CLI knob draws
    // them directly), but each is drawn from the SAME terrain-seed-derived
    // stream regardless of whether the four upstream pins that condition
    // the crust field (plates, ocean-fraction, supercontinent, continents)
    // are set — so re-affirming any of those four must leave terranes and
    // microcontinents byte-identical to the unpinned path, not merely the
    // whole globe (which `pin_isolation_holds_at_the_globe_level` above
    // already asserts implicitly): the house pin-isolation pattern,
    // mirrored exactly, but naming the two new sets explicitly so a future
    // terrane-specific pin cannot silently start reading a different
    // stream position without a test noticing.
    let geo = Geosphere::new(4);
    let default = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();

    let plates_pin = TerrainPins {
        plates: Some(default.globe.plates.len() as u32),
        ..TerrainPins::default()
    };
    let pinned = generate(Seed(42), &geo, &plates_pin).unwrap();
    assert_eq!(
        pinned.globe.terranes, default.globe.terranes,
        "plates pin perturbed terranes"
    );
    assert_eq!(
        pinned.globe.microcontinents, default.globe.microcontinents,
        "plates pin perturbed microcontinents"
    );

    let drawn_ocean = 0.5
        + 0.25
            * Seed(42)
                .derive(streams::ROOT)
                .derive(streams::OCEAN_FRACTION)
                .stream()
                .next_f64();
    let ocean_pin = TerrainPins {
        ocean_fraction: Some(drawn_ocean),
        ..TerrainPins::default()
    };
    let pinned = generate(Seed(42), &geo, &ocean_pin).unwrap();
    assert_eq!(
        pinned.globe.terranes, default.globe.terranes,
        "ocean-fraction pin perturbed terranes"
    );
    assert_eq!(
        pinned.globe.microcontinents, default.globe.microcontinents,
        "ocean-fraction pin perturbed microcontinents"
    );

    let super_pin = TerrainPins {
        supercontinent: Some(false),
        ..TerrainPins::default()
    };
    let pinned = generate(Seed(42), &geo, &super_pin).unwrap();
    assert_eq!(
        pinned.globe.terranes, default.globe.terranes,
        "supercontinent pin perturbed terranes"
    );
    assert_eq!(
        pinned.globe.microcontinents, default.globe.microcontinents,
        "supercontinent pin perturbed microcontinents"
    );

    let continents_pin = TerrainPins {
        continents: Some(default.globe.cratons.len() as u32),
        ..TerrainPins::default()
    };
    let pinned = generate(Seed(42), &geo, &continents_pin).unwrap();
    assert_eq!(
        pinned.globe.terranes, default.globe.terranes,
        "continents pin perturbed terranes"
    );
    assert_eq!(
        pinned.globe.microcontinents, default.globe.microcontinents,
        "continents pin perturbed microcontinents"
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

/// claim: invariant(census: none yet — migration candidate, default/unpinned)
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
        // L4 coarse-discretization envelope for the drawn ocean-fraction
        // range [0.5, 0.75). Epoch v4 (rift-and-fit): the seam clip trims
        // major caps to their conjugate margins, which lengthens the runs
        // of tied oceanic-floor elevations near the sea-level percentile, so
        // the strict-`<` achieved fraction misses the coarse target by a
        // hair more than before — the observed 64-seed spread is now
        // [0.489 (seed 54), 0.753], so the floor drops 0.49 -> 0.48. At the
        // canonical L6 the miss is far tighter; this band only guards
        // against gross failures (a runaway all-ocean or all-land world).
        assert!(
            (0.48..=0.76).contains(&summary.ocean_fraction),
            "seed {seed}: ocean fraction {} misses the drawn range",
            summary.ocean_fraction
        );
        assert!(summary.highest_elevation_m > summary.sea_level_m);
    }
}

/// claim: invariant(census: none yet — migration candidate, default/unpinned)
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

/// claim: structural(seed: [0,17,42,63]) — byte-identity, build twice, compare
/// (audit note: rubric's canonical shape is one fixed seed; this sweeps four,
/// which is more coverage, not a hunt — flagged, not narrowed, per the audit)
#[test]
fn genesis_is_deterministic_across_the_sweep() {
    let geo = Geosphere::new(4);
    for seed in [0u64, 17, 42, 63] {
        let a = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        let b = generate(Seed(seed), &geo, &TerrainPins::default()).unwrap();
        assert_eq!(a, b, "seed {seed}");
    }
}

/// claim: invariant(census: none yet — migration candidate, default/unpinned)
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

/// claim: sanctioned-sweep(pinned regime, continents: Some(1) — no census home;
/// the-census.study.json carries exactly one pin_set)
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

/// claim: sanctioned-sweep(pinned regime, continents: Some(1) — no census home)
#[test]
fn single_craton_genesis_survives_maximal_terrane_stacking() {
    // Terrane saturation guard (Sculpting Task 1 review): under
    // `--continents 1` every drawn terrane hosts the same craton (hosts
    // are drawn with replacement, bearings uniform), so up to
    // TERRANE_COUNT_MAX kernels can pile onto one rim — unclamped, a
    // young craton's 45 km peak plus 6 x 12 km of terrane would breach
    // CrustKm's validated [0, 100] ceiling as a raw panic mid-genesis,
    // not a GenesisError. The cap in `thickness_at` saturates instead;
    // this sweep asserts full genesis returns Ok under that maximal
    // stacking pressure. (The exact-value clamp unit test lives in
    // crust.rs: `stacked_terranes_saturate_at_the_crust_ceiling`.)
    let geo = Geosphere::new(4);
    let pins = TerrainPins {
        continents: Some(1),
        ..TerrainPins::default()
    };
    for seed in 1..=40u64 {
        let outcome = generate(Seed(seed), &geo, &pins);
        assert!(outcome.is_ok(), "seed {seed}: {:?}", outcome.err());
    }
}

/// claim: invariant(census: none yet) — craton-level-only, grid-free (no
/// Geosphere/generate); proves the supply fallback never fires across the
/// frozen 1000-seed census population, protecting byte-identity of every
/// committed default-world artifact (seedless sweep, audit §5)
#[test]
fn default_worlds_never_trip_the_supply_fallback() {
    use hornvale_terrain::crust::{continental_supply, draw_cratons};
    use hornvale_terrain::elevation::{
        SUPPLY_SHORTFALL_FACTOR, effective_ocean_target, resolve_ocean_fraction,
    };
    // Craton-level (no genesis): cheap, and grid-free by construction —
    // this is the byte-identity proof that the fallback cannot rewrite
    // default worlds (whose frozen census fixtures and seed-42 artifacts
    // must not drift). The sweep covers the frozen 1000-seed census
    // population (seeds 0..999, `studies/the-census.study.json`) — the
    // exact population whose byte-identity this guard proves.
    for seed in 0..1000u64 {
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

#[test]
fn rift_pin_isolation_supercontinent_consumes_identical_draws() {
    // The supercontinent pin (epoch v4, spec §4) is post-draw arithmetic — a
    // craton-center replacement at genesis, plus the pre-existing repulsion
    // skip. Neither draws from any stream, so every position-INDEPENDENT
    // drawn quantity, and the (unmetered, structural) notes, must be
    // byte-identical across Some(true), Some(false), and None: the pin never
    // changes consumption. Terrane and microcontinent CENTERS legitimately
    // differ under Some(true) — they place against the drawn craton rims,
    // which the repulsion skip moves — so this probes the drawn SCALARS, not
    // the placed geometry (which is where the pin's effect legitimately
    // shows). This is the draw-count-equality proof the wiring resolution
    // asks for.
    let geo = Geosphere::new(4);
    let none = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    let off = generate(
        Seed(42),
        &geo,
        &TerrainPins {
            supercontinent: Some(false),
            ..TerrainPins::default()
        },
    )
    .unwrap();
    let on = generate(
        Seed(42),
        &geo,
        &TerrainPins {
            supercontinent: Some(true),
            ..TerrainPins::default()
        },
    )
    .unwrap();

    for (label, outcome) in [("false", &off), ("true", &on)] {
        // Structural pin — never metered — so the notes are identical.
        assert_eq!(
            outcome.notes, none.notes,
            "supercontinent={label} perturbed the genesis notes"
        );
        // One spreading-rate draw off `terrain/rift`, position-independent.
        assert_eq!(
            outcome.globe.rift.spreading_rate, none.globe.rift.spreading_rate,
            "supercontinent={label} perturbed the rift spreading-rate draw"
        );
        // Craton radii and ages are pure draws (repulsion and the center
        // replacement touch only centers), so they match across all three.
        assert_eq!(outcome.globe.cratons.len(), none.globe.cratons.len());
        for (a, b) in none.globe.cratons.iter().zip(&outcome.globe.cratons) {
            assert_eq!(a.id, b.id);
            assert_eq!(
                a.radius_rad, b.radius_rad,
                "supercontinent={label}: craton {} radius consumed differently",
                a.id
            );
            assert_eq!(
                a.age, b.age,
                "supercontinent={label}: craton {} age consumed differently",
                a.id
            );
        }
        // Terranes: count and every position-independent drawn field match —
        // the terrane stream draws the same values regardless of the pin;
        // only the geometric placement (center/along) rides craton positions.
        assert_eq!(
            outcome.globe.terranes.len(),
            none.globe.terranes.len(),
            "supercontinent={label}: terrane count consumed differently"
        );
        for (a, b) in none.globe.terranes.iter().zip(&outcome.globe.terranes) {
            assert_eq!(a.half_len_rad, b.half_len_rad);
            assert_eq!(a.half_wid_rad, b.half_wid_rad);
            assert_eq!(a.age, b.age);
            assert_eq!(a.thickness_km, b.thickness_km);
        }
    }
}

/// The pinned-supercontinent assemblies over the sweep in which a craton
/// interpenetrates an earlier one because the placed set leaves it nowhere
/// clear to stand. **A geometric census, not a tolerance**: measured with an
/// independent 200,000-point sphere sample asking whether *any* position
/// clears every placed craton, 55 of the sweep's 400 non-anchor cratons have
/// none — and the placements those 55 force on their successors bring the
/// realised total to 67. See `pinned_supercontinent_is_sutured`.
const SUTURE_FORCED_OVERLAPS: usize = 67;

/// claim: invariant(forall-seed) — every major in every pinned-supercontinent
/// world of the sweep touches an earlier major at exactly the contact
/// separation, and interpenetration stays at its geometrically forced floor
///
/// **The sweep is half the assertion.** This ran on seed 42 alone until
/// decision 0134, and that single seed is precisely what hid a real defect:
/// `assemble_cratons` was leaving cratons floating clear of contact on 31 of
/// these 400 non-anchor cratons — overlapping on 9 more, by up to 0.89 rad —
/// while seed 42 happened to settle cleanly and the test stayed green through
/// every campaign that touched the assembly. The same single-sample blindness
/// hid the repulsion defect 0131 also records (seeds 0..8, all passing, over a
/// pass that degraded 35 of 200 worlds). One seed is not a property.
///
/// **Why contact is asserted strictly and overlap is ratcheted.** These are
/// not the same kind of claim, and measurement is what separates them:
///
/// - *Touching* is always achievable, and is now always achieved — every
///   craton is placed at exactly the contact separation from some earlier
///   one, because `settle_against_a_host` searches contact circles, on which
///   every point attains contact by construction. This half is absolute; a
///   single floating craton fails the test.
/// - *Not overlapping* is **provably impossible** for part of the population.
///   A contact separation reaches `CONTACT_FACTOR × 1.6 ≈ 1.36` rad, so one
///   craton's forbidden cap covers ~31% of the sphere; eight of them cannot
///   avoid covering all of it. An independent 200,000-point sphere sample
///   confirms it directly rather than by area argument: **55 of 400** cratons
///   here have no clear position anywhere on the sphere. This is not new with
///   the raised clamp — the same probe finds **6 of 400** on shipped `main`
///   (0.6 clamp, closed-form rescale) — it is newly *visible* because the
///   sweep looks at more than one seed.
///
/// So the floor is ratcheted rather than wished away. It cannot grow silently,
/// and lowering it means changing `CONTACT_FACTOR` or the clamp — a decision
/// 0057 question this campaign deliberately did not open.
#[test]
fn pinned_supercontinent_is_sutured() {
    use hornvale_kernel::math;
    use hornvale_terrain::crust::CONTACT_FACTOR;
    let geo = Geosphere::new(4);
    let sep_of = |a: [f64; 3], b: [f64; 3]| {
        math::acos((a[0] * b[0] + a[1] * b[1] + a[2] * b[2]).clamp(-1.0, 1.0))
    };
    let (mut multi_major_seeds, mut overlapping) = (0, 0);
    for seed in 0..40u64 {
        let on = generate(
            Seed(seed),
            &geo,
            &TerrainPins {
                supercontinent: Some(true),
                ..TerrainPins::default()
            },
        )
        .unwrap();
        let cratons = &on.globe.cratons;
        if cratons.len() > 1 {
            multi_major_seeds += 1;
        }
        // Under the pin every major's final center IS its `rift.assembly`
        // position, so each craton i>0 must touch at least one earlier craton
        // at the contact separation (within 1e-9 rad) — a sutured
        // supercontinent.
        for i in 1..cratons.len() {
            let mut touched = false;
            let mut overlaps = false;
            for j in 0..i {
                let sep = sep_of(cratons[i].center, cratons[j].center);
                let contact = CONTACT_FACTOR * (cratons[i].radius_rad + cratons[j].radius_rad);
                if sep < contact - 1e-9 {
                    overlaps = true;
                }
                if sep <= contact + 1e-9 {
                    touched = true;
                }
            }
            assert!(
                touched,
                "seed {seed} craton {i} floats free of the sutured supercontinent"
            );
            if overlaps {
                overlapping += 1;
            }
        }
    }
    assert!(
        multi_major_seeds >= 35,
        "the sweep must actually exercise multi-craton assemblies; only \
         {multi_major_seeds}/40 seeds drew more than one major"
    );
    assert_eq!(
        overlapping, SUTURE_FORCED_OVERLAPS,
        "interpenetrating majors moved off their measured geometric floor — \
         if this ROSE, the assembly regressed; if it FELL, the floor is stale \
         and this constant should be lowered in the same commit"
    );
}

#[test]
fn default_world_carries_a_rift_history() {
    let geo = Geosphere::new(4);
    let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
    assert!(
        !outcome.globe.rift.seams.is_empty(),
        "seed 42's default globe drew no rift seams"
    );
    assert_eq!(
        outcome.globe.rift.assembly.len(),
        outcome.globe.cratons.len(),
        "rift assembly must align index-for-index with the major cratons"
    );
}

#[test]
fn the_clip_reshapes_real_coastlines_on_seed_42() {
    // Real-world exercise of the wiring (spec §3): on seed 42's actual
    // generated globe, the seam clip must change the continental
    // classification of at least one cell versus the same craton/terrane set
    // built WITHOUT a rift. This is the honest, robust variant of the
    // "coastal pair straddles a seam curve" check — reconstructing the field
    // with and without the clip and diffing `continental_at` over every cell
    // directly exercises the clip on real drawn geometry, without the
    // brittleness of bisecting a seam curve and hoping a sampled forward
    // image happens to land in a differently-classified cell.
    use hornvale_terrain::crust::CrustField;
    let geo = Geosphere::new(5);
    let g = &generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    let terrain_seed = Seed(42).derive(streams::ROOT);
    let all = [g.cratons.clone(), g.microcontinents.clone()].concat();
    let clipped = CrustField::new_with_rift(
        terrain_seed,
        all.clone(),
        g.terranes.clone(),
        Some(g.rift.clone()),
        g.cratons.len(),
    );
    let plain = CrustField::new_with_terranes(terrain_seed, all, g.terranes.clone());
    let flips = geo
        .cells()
        .filter(|&c| {
            let p = geo.position(c);
            clipped.continental_at(p) != plain.continental_at(p)
        })
        .count();
    assert!(
        flips > 0,
        "the seam clip changed no cell's continental classification on seed 42 — \
         the rift is not shaping real coastlines"
    );
}

#[test]
fn the_column_is_deterministic() {
    let geo = Geosphere::new(4);
    let a = GeneratedTerrain::new(
        geo.clone(),
        generate(Seed(42), &geo, &TerrainPins::default()).unwrap(),
    );
    let b = GeneratedTerrain::new(
        geo.clone(),
        generate(Seed(42), &geo, &TerrainPins::default()).unwrap(),
    );
    for cell in geo.cells() {
        assert_eq!(a.column_at(cell), b.column_at(cell));
        assert_eq!(
            a.geothermal_gradient_at(cell),
            b.geothermal_gradient_at(cell)
        );
    }
}

#[test]
fn the_column_is_a_pure_projection_unperturbed_by_pins() {
    let geo = Geosphere::new(4);
    let base = GeneratedTerrain::new(
        geo.clone(),
        generate(Seed(42), &geo, &TerrainPins::default()).unwrap(),
    );
    // Re-affirm a drawn value via a pin; the derived column must not shift.
    let pins = TerrainPins {
        plates: Some(summarize(base.globe()).plate_count),
        ..TerrainPins::default()
    };
    let pinned = GeneratedTerrain::new(geo.clone(), generate(Seed(42), &geo, &pins).unwrap());
    for cell in geo.cells() {
        assert_eq!(base.column_at(cell), pinned.column_at(cell));
    }
}

#[test]
fn features_seed_is_derived_and_perturbs_no_existing_draw() {
    let geo = Geosphere::new(4);
    let a = generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    let b = generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    // deterministic
    assert_eq!(a.features_noise_seed(), b.features_noise_seed());
    // distinct from the lithology seed (a different label)
    assert_ne!(a.features_noise_seed(), a.lithology_noise_seed());
}

/// The Ford, Task 5 review finding 2: `channel_seed` must be the already-
/// derived `streams::CHANNEL_MEANDER` LEAF, not the terrain-root seed — a
/// leaf like `lithology_seed`/`features_seed`, never a value a caller could
/// derive any other terrain stream from. This is also the byte-identity
/// check for that fix: `ChannelNetwork::build` used to receive the raw
/// terrain-root seed and derive `CHANNEL_MEANDER` from it internally; now
/// `generate()` derives it once and `build` uses the result directly. Both
/// orderings must produce the exact same `Seed` value into
/// `SphereFbm::new`, which this asserts directly by re-deriving the leg
/// from the public root the same way the OLD internal call would have.
#[test]
fn channel_seed_is_the_derived_leaf_and_matches_the_old_internal_derivation() {
    let geo = Geosphere::new(4);
    let a = generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    let b = generate(Seed(42), &geo, &TerrainPins::default())
        .unwrap()
        .globe;
    // Deterministic.
    assert_eq!(a.channel_noise_seed(), b.channel_noise_seed());
    // Distinct from its siblings (different labels).
    assert_ne!(a.channel_noise_seed(), a.lithology_noise_seed());
    assert_ne!(a.channel_noise_seed(), a.features_noise_seed());
    // The byte-identity claim: re-deriving CHANNEL_MEANDER from the public
    // root the way `ChannelNetwork::build` used to do it INTERNALLY (before
    // this fix) yields exactly the value now stored on the globe and passed
    // to `build` directly.
    let terrain_root = Seed(42).derive(streams::ROOT);
    let old_style_derivation = terrain_root.derive(streams::CHANNEL_MEANDER);
    assert_eq!(
        a.channel_noise_seed(),
        old_style_derivation,
        "channel_seed must equal terrain_seed.derive(CHANNEL_MEANDER) — the \
         same value ChannelNetwork::build derived internally before this fix"
    );
}

/// KNOWN GAP, recorded where a reader of the two tests above will meet it:
/// `TectonicGlobe` carries five hash-noise leaf seeds — `lithology_seed`,
/// `features_seed`, `channel_seed`, `rill_seed` and `arc_gate_seed` — and
/// **`arc_gate_seed` has no leaf-seed property test of its own.**
///
/// What the two tests above pin, for the leaves they do cover, is the shape
/// worth copying: the leaf is *derived* (same seed in, same seed out), it is
/// *distinct* from its siblings, and deriving it *perturbs no existing draw*.
/// `arc_gate_seed` gets none of those. It is also the only one of the five
/// with no `*_noise_seed()` accessor, so `has_edifice` reads the field
/// directly; the two facts share a cause (The Repose, Task 4 added the leaf
/// and followed neither convention).
///
/// Why this is a gap and not a hole: the arc gate IS covered end-to-end, by
/// `provider.rs`'s `has_edifice_names_the_cells_the_shipped_elevation_raised`,
/// which re-runs `assemble_elevation` under a deliberately different gate
/// seed. That test would catch a gate seed that stopped being derived from
/// the terrain root. It would NOT catch the leaf colliding with a sibling
/// leaf, which is precisely what the two tests above exist to rule out.
#[test]
fn features_are_a_pure_pin_invariant_projection() {
    let geo = Geosphere::new(4);
    let base = GeneratedTerrain::new(
        geo.clone(),
        generate(Seed(42), &geo, &TerrainPins::default()).unwrap(),
    );
    // Re-affirm a drawn value via a pin; the derived caves/deposits must not shift.
    let pins = TerrainPins {
        plates: Some(summarize(base.globe()).plate_count),
        ..TerrainPins::default()
    };
    let pinned = GeneratedTerrain::new(geo.clone(), generate(Seed(42), &geo, &pins).unwrap());
    for cell in geo.cells() {
        assert_eq!(base.cave_at(cell), pinned.cave_at(cell));
        assert_eq!(base.deposit_at(cell), pinned.deposit_at(cell));
    }
}
