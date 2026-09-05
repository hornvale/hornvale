//! `field_pack_from` must materialize every field pack scalar over the whole
//! vertex grid, in range — The Weft, Task 4; extended for `land` (Task 7,
//! R1's eligibility fix), `slope` (Task 7, overhang/hollow) and
//! `temperature`/`moisture` (Task 7, thicket/brake).
//!
//! Test fixture (decision 0092): calls the sculpt derivation entry point
//! (`terrain_of`) directly to build its own world state, once per test — the
//! sanctioned test-fixture posture the weir's spec carves out.
#![allow(clippy::disallowed_methods)]

/// Every field covers every vertex and stays inside its documented range.
/// A field with a hole would make `blend_at` return `None` at a facet whose
/// corners straddle it, which reads downstream as "no macro state here".
#[test]
fn the_field_pack_is_total_over_the_grid_and_in_range() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(pack.carbonate.len(), n, "carbonate must cover every vertex");
    for v in 0..n {
        let c = *pack.carbonate.get(hornvale_kernel::Vertex(v as u32));
        assert!(
            (0.0..=1.0).contains(&c),
            "carbonate out of range at {v}: {c}"
        );
    }
}

/// `induration` covers every vertex and stays in `[0,1]` — the same total-
/// coverage/in-range guarantee as `carbonate`, checked on the second field so
/// a per-field materialization bug (e.g. accidentally reusing one field's
/// values for another) cannot hide behind a single-field test.
#[test]
fn induration_is_total_over_the_grid_and_in_range() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(
        pack.induration.len(),
        n,
        "induration must cover every vertex"
    );
    for v in 0..n {
        let i = *pack.induration.get(hornvale_kernel::Vertex(v as u32));
        assert!(
            (0.0..=1.0).contains(&i),
            "induration out of range at {v}: {i}"
        );
    }
}

/// `drainage` covers every vertex and agrees with `GeneratedTerrain::drainage_at`
/// pointwise — it is a count (`>= 0`), not `[0,1]`-scaled, so this checks
/// non-negativity and identity with the accessor it materializes rather than
/// reusing the `[0,1]` range check.
#[test]
fn drainage_is_total_over_the_grid_and_matches_the_accessor() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(pack.drainage.len(), n, "drainage must cover every vertex");
    for v in 0..n {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let d = *pack.drainage.get(vertex);
        assert!(d >= 0.0, "drainage must be non-negative at {v}: {d}");
        assert_eq!(
            d,
            terrain.drainage_at(vertex),
            "field pack drainage must match the accessor at {v}"
        );
    }
}

/// `slope` (Task 7) covers every vertex and agrees pointwise with the
/// promoted `hornvale_terrain::local_slope` it materializes — the same
/// identity-with-the-accessor shape `drainage` checks above, on the field
/// R4 of Task 7's dispatch required be a promotion rather than a
/// reimplementation.
#[test]
fn slope_is_total_over_the_grid_and_matches_local_slope() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();
    let geo = terrain.geosphere();
    let globe = terrain.globe();

    assert_eq!(pack.slope.len(), n, "slope must cover every vertex");
    for v in 0..n {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let s = *pack.slope.get(vertex);
        assert!(s.is_finite(), "slope must be finite at {v}: {s}");
        assert_eq!(
            s,
            hornvale_terrain::local_slope(globe, geo, vertex),
            "field pack slope must match the promoted accessor at {v}"
        );
    }
}

/// `temperature` (Task 7) covers every vertex and agrees pointwise with
/// `GeneratedClimate::mean_temperature_at` — the annual-mean accessor R3 of
/// Task 7's dispatch required, never the seasonal `temperature_at(v, at)`.
#[test]
fn temperature_is_total_over_the_grid_and_matches_mean_temperature_at() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(
        pack.temperature.len(),
        n,
        "temperature must cover every vertex"
    );
    for v in 0..n {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let t = *pack.temperature.get(vertex);
        assert!(t.is_finite(), "temperature must be finite at {v}: {t}");
        assert_eq!(
            t,
            climate.mean_temperature_at(vertex).get(),
            "field pack temperature must match mean_temperature_at at {v}"
        );
    }
}

/// `moisture` (Task 7) covers every vertex, stays in `[0,1]`, and agrees
/// pointwise with `GeneratedClimate::moisture_at`.
#[test]
fn moisture_is_total_over_the_grid_and_matches_the_accessor() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(pack.moisture.len(), n, "moisture must cover every vertex");
    for v in 0..n {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let m = *pack.moisture.get(vertex);
        assert!(
            (0.0..=1.0).contains(&m),
            "moisture out of range at {v}: {m}"
        );
        assert_eq!(
            m,
            climate.moisture_at(vertex),
            "field pack moisture must match moisture_at at {v}"
        );
    }
}

/// `land` (Task 7, R1) covers every vertex, is exactly `0.0` or `1.0`, and
/// agrees pointwise with `!GeneratedTerrain::is_ocean` — the field the
/// eligibility fix reads instead of exposing a typed elevation/sea-level
/// pair as bare `f64`s (see `crate::fieldpack`'s module doc).
#[test]
fn land_is_total_over_the_grid_and_matches_is_ocean() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let climate = hornvale_worldgen::climate_from(&world, &terrain).expect("climate reconstructs");
    let pack = hornvale_worldgen::field_pack_from(&terrain, &climate);
    let n = terrain.geosphere().vertex_count();

    assert_eq!(pack.land.len(), n, "land must cover every vertex");
    let mut land_count = 0;
    let mut ocean_count = 0;
    for v in 0..n {
        let vertex = hornvale_kernel::Vertex(v as u32);
        let l = *pack.land.get(vertex);
        assert!(
            l == 0.0 || l == 1.0,
            "land must be exactly 0.0 or 1.0 at {v}: {l}"
        );
        let expected = if terrain.is_ocean(vertex) { 0.0 } else { 1.0 };
        assert_eq!(l, expected, "field pack land must match !is_ocean at {v}");
        if l == 1.0 {
            land_count += 1;
        } else {
            ocean_count += 1;
        }
    }
    assert!(land_count > 0, "seed 42 must have some land vertices");
    assert!(ocean_count > 0, "seed 42 must have some ocean vertices");
}
