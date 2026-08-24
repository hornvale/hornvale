//! THE FATHOM: the column — every stratum present at a vertex, and the
//! community at each. Additive over the stored `biome_expr`.
//!
//! `hornvale_climate::provider::test_support` (the module the rest of this
//! crate's own tests build fixtures from) is `#[cfg(test)]`-gated, so it is
//! compiled only for the crate's own unit-test build and is invisible to an
//! integration test, which links a separately-compiled, non-`cfg(test)` copy
//! of the library. Reaching for it here fails to compile: rustc's E0432
//! reads "could not find `test_support` in `provider`". So this file builds
//! its own fixture —
//! the same shape `test_support::sample_world` uses (`ClimateInputs` and
//! `GeneratedClimate::generate` are both `pub`, as the module's own doc
//! anticipates) — behind one local helper every test in this file calls, so
//! the mesh a test iterates and the climate it queries always come from the
//! same construction.

use hornvale_climate::{
    ClimateInputs, Formation, GeneratedClimate, Realm, RotationRegime, Stratum,
};
use hornvale_kernel::{Geosphere, ReferenceElevation, Seed, VertexMap};

/// A small mixed land/ocean, spinning world — the same land/ocean split and
/// scalar inputs `hornvale_climate::provider::test_support::sample_world`
/// uses, rebuilt here because that module is unreachable from an integration
/// test (see the file doc comment). Land is `+300.0` m, ocean a uniform
/// `-1000.0` m, so every ocean vertex has the same depth and floor stratum.
fn sample_world() -> (Geosphere, GeneratedClimate) {
    let geo = Geosphere::new(4);
    let elevation = VertexMap::from_fn(&geo, |c| {
        let m = if geo.position(c)[2] > 0.0 {
            300.0
        } else {
            -1000.0
        };
        ReferenceElevation::new(m).unwrap()
    });
    let seafloor = VertexMap::from_fn(&geo, |_| hornvale_climate::SeafloorFeature::None);
    let climate = GeneratedClimate::generate(&ClimateInputs {
        geosphere: &geo,
        elevation: &elevation,
        sea_level: ReferenceElevation::new(0.0).unwrap(),
        seafloor: &seafloor,
        insolation: 1.0,
        obliquity_deg: 23.5,
        regime: RotationRegime::Spinning { day_std: 1.0 },
        year_length_std: 365.25,
        year_phase_offset: 0.0,
        seed: Seed(1),
        greenhouse_forcing_k: 0.0,
    });
    (geo, climate)
}

/// THE HEADLINE. The column must agree with the accessor that already
/// exists, at the vertex's own rung, at EVERY vertex. This is what makes the
/// column a re-reading of the stored expression rather than a second,
/// silently-diverging derivation of it.
#[test]
fn the_column_agrees_with_biome_expr_at_at_every_vertex() {
    let (geo, climate) = sample_world();
    for vertex in geo.vertices() {
        let e = climate.biome_expr_at(vertex);
        assert_eq!(
            climate.biome_expr_at_stratum(vertex, e.stratum),
            Some(e),
            "column disagrees with biome_expr_at at {vertex:?}"
        );
    }
}

/// A land vertex's column is exactly one rung.
#[test]
fn a_land_vertex_has_a_one_rung_column() {
    let (geo, climate) = sample_world();
    let land = geo
        .vertices()
        .find(|c| climate.biome_expr_at(*c).realm == Realm::OVERWORLD)
        .expect("this fixture has land");
    assert_eq!(climate.strata_at(land), vec![Stratum::Surface]);
}

/// A marine vertex's column runs from the surface down to its own floor, in
/// order, with no gaps and no repeats.
#[test]
fn a_marine_column_runs_from_the_surface_to_its_own_floor() {
    let (geo, climate) = sample_world();
    for vertex in geo.vertices() {
        let e = climate.biome_expr_at(vertex);
        if e.realm != Realm::WATERWORLD {
            continue;
        }
        let column = climate.strata_at(vertex);
        let ladder = e.realm.strata();
        assert_eq!(
            column.first(),
            Some(&ladder[0]),
            "column must start at the top"
        );
        assert_eq!(
            column.last(),
            Some(&e.stratum),
            "column must end at the floor"
        );
        let expected: Vec<Stratum> = ladder
            .iter()
            .copied()
            .take_while(|s| *s != e.stratum)
            .chain(std::iter::once(e.stratum))
            .collect();
        assert_eq!(
            column, expected,
            "column is the ladder prefix at {vertex:?}"
        );
    }
}

/// Below the floor is not a place. A stratum deeper than the vertex's own is
/// absent, not empty-but-present.
#[test]
fn nothing_exists_below_the_floor() {
    let (geo, climate) = sample_world();
    for vertex in geo.vertices() {
        let e = climate.biome_expr_at(vertex);
        let ladder = e.realm.strata();
        let floor = ladder
            .iter()
            .position(|s| *s == e.stratum)
            .expect("floor is on its own ladder");
        for deeper in &ladder[floor + 1..] {
            assert_eq!(
                climate.biome_expr_at_stratum(vertex, *deeper),
                None,
                "{deeper:?} is below the floor at {vertex:?} and must be absent"
            );
        }
    }
}

/// Water above a seafloor community is open water AT ITS OWN DEPTH — the
/// reading `classify_marine_expr`'s own doc argues for ("a vent is a
/// community AT a depth"). Asserted on a vertex deep enough to have water
/// above it. This fixture's ocean is uniformly 1000 m deep, so every ocean
/// vertex has floor stratum `Bathypelagic` and a three-rung column — there is
/// exactly one column height in this world, and a multi-rung vertex must
/// exist.
///
/// claim: structural(seed: 1) — false-positive seed-loop flag; `s` binds
/// a Stratum walked over one fixed synthetic world's column, not a seed.
#[test]
fn water_above_the_floor_is_open_water_at_its_own_depth() {
    let (geo, climate) = sample_world();
    let deep = geo.vertices().find(|c| climate.strata_at(*c).len() > 1);
    let Some(deep) = deep else {
        panic!(
            "the fixture's ocean is uniformly 1000 m deep, so every ocean \
             vertex must be multi-rung — found none, which means the column \
             is degenerate"
        );
    };
    let column = climate.strata_at(deep);
    for s in &column[..column.len() - 1] {
        let above = climate
            .biome_expr_at_stratum(deep, *s)
            .expect("a stratum in the column is present by construction");
        assert_eq!(above.formation, Formation::OpenWater, "at {s:?}");
        assert_eq!(above.stratum, *s, "an expression carries its own stratum");
    }
}

/// A rung from another realm's ladder is not a query this vertex can answer.
#[test]
fn a_rung_from_another_realms_ladder_is_absent() {
    let (geo, climate) = sample_world();
    let land = geo
        .vertices()
        .find(|c| climate.biome_expr_at(*c).realm == Realm::OVERWORLD)
        .expect("this fixture has land");
    assert_eq!(climate.biome_expr_at_stratum(land, Stratum::Abyssal), None);
    assert_eq!(climate.biome_expr_at_stratum(land, Stratum::Basement), None);
}
