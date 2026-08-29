//! Integration tests for `traversal_cost`: the per-vertex terrain
//! traversal-cost field the natural-land-route pathfinder
//! (`hornvale_topology::route`) plans over.
//!
//! The test substrate is a real `Geosphere` (its fields are private, so there
//! is no toy-grid constructor — see `domains/topology/tests/route.rs` and
//! `windows/worldgen/tests/history_bake.rs` for the same pattern) built at
//! `Geosphere::new(1)` (42 vertices, fully deterministic).

use hornvale_climate::Biome;
use hornvale_kernel::{Geosphere, ReferenceElevation, Vertex, VertexMap};
use hornvale_worldgen::traversal::{BASE_COST, traversal_cost, traversal_cost_at};

/// Test-only helper: a validated `ReferenceElevation`.
fn e(m: f64) -> ReferenceElevation {
    ReferenceElevation::new(m).unwrap()
}

/// A fixture over `Geosphere::new(1)` with three disjoint, non-adjacent
/// regions of interest:
///
/// - `peak`: a very high vertex surrounded by much lower neighbors (a large
///   elevation gap on every side) -- the steep case.
/// - `flat`: a vertex whose neighbors sit at the exact same elevation as it --
///   zero slope, the lowland case.
/// - `ocean`: a marine-biome vertex -- impassable to land travel regardless of
///   its elevation.
///
/// The one overlap that would matter (`peak` sitting inside `flat`'s
/// neighbor ring) is asserted against below; other neighbor-set overlaps are
/// harmless (see that assertion's comment).
fn fixture() -> (
    Geosphere,
    Vertex,
    Vertex,
    Vertex,
    VertexMap<ReferenceElevation>,
    VertexMap<Biome>,
) {
    let geo = Geosphere::new(1); // 42 vertices

    let peak = Vertex(0);
    let flat = Vertex(3);
    let ocean = Vertex(6);

    let peak_neighbors: Vec<Vertex> = geo.neighbors(peak).to_vec();
    let flat_neighbors: Vec<Vertex> = geo.neighbors(flat).to_vec();

    // Guard the fixture's own assumption that matters for a zero flat-side
    // gap: `peak` must not be one of `flat`'s neighbors (adjacency is
    // symmetric, so checking one direction is enough) -- otherwise the
    // elevation closure below would assign `flat`'s own neighbor ring the
    // peak's height instead of the flat height, and the flat vertex would no
    // longer read a zero-slope cost. `ocean`'s cost is driven entirely by its
    // biome (checked before elevation), so it needs no such isolation: it is
    // measured correctly regardless of any neighbor-set overlap.
    assert!(peak != flat && peak != ocean && flat != ocean);
    assert!(!peak_neighbors.contains(&flat) && !flat_neighbors.contains(&peak));

    let elevation = VertexMap::from_fn(&geo, |c| {
        if c == peak {
            e(5000.0)
        } else if c == flat || flat_neighbors.contains(&c) {
            e(50.0)
        } else if peak_neighbors.contains(&c) {
            e(0.0)
        } else {
            e(50.0)
        }
    });

    let biome = VertexMap::from_fn(&geo, |c| {
        if c == ocean {
            Biome::Epipelagic
        } else {
            Biome::TemperateGrassland
        }
    });

    (geo, peak, flat, ocean, elevation, biome)
}

#[test]
fn a_flat_lowland_vertex_reads_the_base_cost() {
    let (geo, _peak, flat, _ocean, elevation, biome) = fixture();
    let cost = traversal_cost(&geo, &elevation, &biome);
    // Zero slope: the field reduces to exactly the base cost.
    assert_eq!(*cost.get(flat), BASE_COST);
}

#[test]
fn a_steep_peak_reads_a_strictly_higher_cost_than_the_flat_vertex() {
    let (geo, peak, flat, _ocean, elevation, biome) = fixture();
    let cost = traversal_cost(&geo, &elevation, &biome);
    assert!(
        *cost.get(peak) > *cost.get(flat),
        "peak cost {} must exceed flat cost {}",
        *cost.get(peak),
        *cost.get(flat)
    );
}

#[test]
fn an_ocean_vertex_is_impassable() {
    let (geo, _peak, _flat, ocean, elevation, biome) = fixture();
    let cost = traversal_cost(&geo, &elevation, &biome);
    assert_eq!(*cost.get(ocean), u64::MAX);
}

#[test]
fn the_field_is_deterministic_across_rebuilds() {
    let (geo, _peak, _flat, _ocean, elevation, biome) = fixture();
    let a = traversal_cost(&geo, &elevation, &biome);
    let b = traversal_cost(&geo, &elevation, &biome);
    for vertex in geo.vertices() {
        assert_eq!(*a.get(vertex), *b.get(vertex));
    }
}

#[test]
fn a_shelf_vertex_is_ocean_at_present_but_a_bridge_at_glacial_low_stand() {
    let geo = Geosphere::new(1);
    let shelf = Vertex(5);
    // shelf sits at -50 m; everything else is upland at +100 m.
    let elevation = VertexMap::from_fn(&geo, |c| if c == shelf { e(-50.0) } else { e(100.0) });
    let present = traversal_cost_at(&geo, &elevation, e(0.0));
    let glacial = traversal_cost_at(&geo, &elevation, e(-120.0));
    assert_eq!(
        *present.get(shelf),
        u64::MAX,
        "shelf is ocean at present sea level"
    );
    assert!(
        *glacial.get(shelf) < u64::MAX,
        "shelf is passable land at -120 m (a bridge)"
    );
}
