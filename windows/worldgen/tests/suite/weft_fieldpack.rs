//! `field_pack_from` must materialize every field pack scalar over the whole
//! vertex grid, in range — The Weft, Task 4; extended for `land` (Task 7,
//! R1's eligibility fix).
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
    let pack = hornvale_worldgen::field_pack_from(&terrain);
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
    let pack = hornvale_worldgen::field_pack_from(&terrain);
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
    let pack = hornvale_worldgen::field_pack_from(&terrain);
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

/// `land` (Task 7, R1) covers every vertex, is exactly `0.0` or `1.0`, and
/// agrees pointwise with `!GeneratedTerrain::is_ocean` — the field the
/// eligibility fix reads instead of exposing a typed elevation/sea-level
/// pair as bare `f64`s (see `crate::fieldpack`'s module doc).
#[test]
fn land_is_total_over_the_grid_and_matches_is_ocean() {
    let world = hornvale_worldgen::seed_42_world();
    let terrain = hornvale_worldgen::terrain_of(&world).expect("seed 42 sculpts");
    let pack = hornvale_worldgen::field_pack_from(&terrain);
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
