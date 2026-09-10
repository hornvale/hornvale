//! Compact Waterworld projection over generated terrain and climate.

use hornvale_climate::{BiomeExpr, GeneratedClimate, Realm, Stratum};
use hornvale_kernel::{Vertex, World};
use hornvale_terrain::landscape::FeatureId;
use hornvale_terrain::{BoundaryKind, GeneratedTerrain, WaterKind};

pub use crate::waterworld_propagation::{WaterPropagation, WaterTrajectorySample};

/// Configuration for the compact Waterworld overlay.
/// type-audit: bare-ok(flag: enabled)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WaterWorldConfig {
    /// Whether the derived overlay is present.
    pub enabled: bool,
}

/// One sampled position in an existing marine column.
/// type-audit: bare-ok(flag: is_seabed), bare-ok(flag: has_edifice), bare-ok(diagnostic-value: depth_m)
#[derive(Clone, Debug, PartialEq)]
pub struct WaterSubstrate {
    /// Stable surface vertex owning this column.
    pub vertex: Vertex,
    /// Stable vertex used by rendering and later adjacency.
    pub render_vertex: Vertex,
    /// Whether this sample is the column's terminal seafloor expression.
    pub is_seabed: bool,
    /// Existing terrain water classification.
    pub water_kind: WaterKind,
    /// Sample depth below sea level, in metres.
    pub depth_m: f64,
    /// Existing marine depth band.
    pub depth_band: Stratum,
    /// Existing community expression at this band.
    pub biome_expr: BiomeExpr,
    /// Existing terrain boundary beneath this column, when present.
    pub seafloor_boundary: Option<BoundaryKind>,
    /// Whether terrain derives a volcanic edifice at this surface vertex.
    pub has_edifice: bool,
    /// Existing terrain features whose extent contains this vertex.
    pub terrain_features: Vec<FeatureId>,
}

/// Stage-2 ambient fields; deliberately empty until their sources are tested.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WaterFields;

/// Stage-2 vent source; deliberately empty until admission is tested.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WaterVent;

/// Stage-3 aggregate stocks; deliberately empty until derivation is tested.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct WaterStocks;

/// Read-only generated Waterworld state.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WaterWorld {
    /// Existing marine substrate projected in stable vertex/column order.
    pub substrate: Vec<WaterSubstrate>,
}

/// Composition-root entry point for the Waterworld overlay.
pub fn waterworld_from(
    _world: &World,
    terrain: &GeneratedTerrain,
    climate: &GeneratedClimate,
    config: WaterWorldConfig,
) -> WaterWorld {
    if !config.enabled {
        return WaterWorld::default();
    }
    assert_eq!(
        terrain.geosphere().vertex_count(),
        climate.geosphere().vertex_count(),
        "Waterworld terrain and climate must share one vertex space"
    );

    let mut features_at = vec![Vec::new(); terrain.geosphere().vertex_count()];
    for feature in terrain.features().all() {
        for &vertex in &feature.extent {
            features_at[vertex.0 as usize].push(feature.id);
        }
    }

    let mut substrate = Vec::new();
    for vertex in terrain.geosphere().vertices() {
        if terrain.water_kind_at(vertex) != WaterKind::Ocean {
            continue;
        }
        let floor_expr = climate.biome_expr_at(vertex);
        assert_eq!(
            floor_expr.realm,
            Realm::WATERWORLD,
            "ocean terrain must have a Waterworld climate column at {vertex:?}"
        );
        let column = climate.strata_at(vertex);
        let seabed_depth_m =
            (terrain.sea_level().get() - terrain.elevation_at(vertex).get()).max(0.0);
        let last = column.len() - 1;
        for (index, depth_band) in column.into_iter().enumerate() {
            let biome_expr = climate
                .biome_expr_at_stratum(vertex, depth_band)
                .expect("a stratum returned by strata_at is present");
            let is_seabed = index == last;
            substrate.push(WaterSubstrate {
                vertex,
                render_vertex: vertex,
                is_seabed,
                water_kind: WaterKind::Ocean,
                depth_m: if is_seabed {
                    seabed_depth_m
                } else {
                    band_entry_depth_m(depth_band)
                },
                depth_band,
                biome_expr,
                seafloor_boundary: terrain.boundary_at(vertex).map(|boundary| boundary.kind),
                has_edifice: terrain.has_edifice(vertex),
                terrain_features: features_at[vertex.0 as usize].clone(),
            });
        }
    }
    WaterWorld { substrate }
}

/// Shallow edge of each marine band, using the exact thresholds documented by
/// `hornvale_climate::Stratum::at_depth_m`. The terminal sample uses the
/// terrain-derived seabed depth instead; this proxy locates only open-column
/// samples where climate exposes a band but no inverse depth accessor.
fn band_entry_depth_m(stratum: Stratum) -> f64 {
    match stratum {
        Stratum::Epipelagic => 0.0,
        Stratum::Mesopelagic => 200.0,
        Stratum::Bathypelagic => 1_000.0,
        Stratum::Abyssal => 4_000.0,
        Stratum::Hadal => 6_000.0,
        Stratum::Surface | Stratum::Rock(_) => {
            unreachable!("a Waterworld column contains only marine strata")
        }
    }
}
