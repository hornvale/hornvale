//! Deterministic surface-plus-overlay lenses for the Skyworld.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write;

use hornvale_kernel::{NearestVertexIndex, VertexMap};
use hornvale_terrain::GeneratedTerrain;

use crate::skyworld::{SkyTerritory, SkyWorld};

/// Detail materialized by a Skyworld lens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SkyWorldDetail {
    /// Planet-wide coverage and sparse events.
    Planet,
    /// A region's physical, exchange, influence, and route footprints.
    Regional,
    /// One habitat's compact phenotype, lifecycle, and stocks.
    Habitat,
}

/// plumb: pending(wave-1)
const WIDTH: u32 = 256;
/// plumb: pending(wave-1)
const HEIGHT: u32 = 128;

fn mark(
    colors: &mut BTreeMap<hornvale_kernel::Vertex, [u8; 3]>,
    vertices: impl IntoIterator<Item = hornvale_kernel::Vertex>,
    color: [u8; 3],
) {
    for vertex in vertices {
        colors.insert(vertex, color);
    }
}

fn pixels(skyworld: &SkyWorld, terrain: &GeneratedTerrain, detail: SkyWorldDetail) -> Vec<u8> {
    #[cfg(test)]
    crate::skyworld::record_raster();
    let geo = terrain.geosphere();
    let base = VertexMap::from_fn(geo, |vertex| {
        if terrain.is_ocean(vertex) {
            [24, 72, 112]
        } else {
            [76, 112, 70]
        }
    });
    let mut overlays = BTreeMap::new();
    for territory in &skyworld.territories {
        match detail {
            SkyWorldDetail::Planet => {
                mark(
                    &mut overlays,
                    territory.physical.projected.iter().copied(),
                    [210, 190, 85],
                );
                mark(
                    &mut overlays,
                    territory
                        .influence
                        .corridors
                        .iter()
                        .flat_map(|c| c.projected.iter().copied()),
                    [225, 135, 70],
                );
                mark(
                    &mut overlays,
                    territory.influence.events.iter().map(|e| e.surface),
                    [245, 90, 80],
                );
            }
            SkyWorldDetail::Regional => {
                mark(
                    &mut overlays,
                    territory.influence.local.iter().copied(),
                    [175, 95, 205],
                );
                mark(
                    &mut overlays,
                    territory.exchange.projected.iter().copied(),
                    [105, 180, 215],
                );
                mark(
                    &mut overlays,
                    territory.physical.projected.iter().copied(),
                    [220, 190, 75],
                );
                mark(
                    &mut overlays,
                    territory
                        .influence
                        .corridors
                        .iter()
                        .flat_map(|c| c.projected.iter().copied()),
                    [230, 130, 65],
                );
                mark(
                    &mut overlays,
                    territory.influence.events.iter().map(|e| e.surface),
                    [250, 75, 75],
                );
            }
            SkyWorldDetail::Habitat => {
                mark(
                    &mut overlays,
                    territory.influence.local.iter().copied(),
                    [150, 90, 205],
                );
                mark(
                    &mut overlays,
                    territory.exchange.projected.iter().copied(),
                    [90, 175, 220],
                );
                mark(
                    &mut overlays,
                    territory.physical.projected.iter().copied(),
                    [240, 205, 70],
                );
                mark(
                    &mut overlays,
                    territory
                        .influence
                        .corridors
                        .iter()
                        .flat_map(|c| c.projected.iter().copied()),
                    [235, 120, 55],
                );
                mark(
                    &mut overlays,
                    territory.influence.events.iter().map(|e| e.surface),
                    [255, 65, 65],
                );
            }
        }
    }
    let colors = base.map_indexed(|vertex, base| overlays.get(&vertex).copied().unwrap_or(*base));
    let index = NearestVertexIndex::new(geo);
    let mut out = Vec::with_capacity((WIDTH * HEIGHT * 3) as usize);
    for py in 0..HEIGHT {
        let latitude = 90.0 - (f64::from(py) + 0.5) / f64::from(HEIGHT) * 180.0;
        for px in 0..WIDTH {
            let longitude = (f64::from(px) + 0.5) / f64::from(WIDTH) * 360.0 - 180.0;
            #[cfg(test)]
            crate::skyworld::record_raster_pixel();
            out.extend_from_slice(colors.get(index.nearest(geo, latitude, longitude)));
        }
    }
    // A sparse feature may be smaller than a nearest-vertex pixel bin at this
    // coarse planet raster. Stamp its own geographic pixel as well so moving
    // one route sample has a visible, bounded pixel delta.
    let occupied: BTreeSet<_> = skyworld
        .territories
        .iter()
        .flat_map(|t| {
            t.physical
                .projected
                .iter()
                .chain(t.exchange.projected.iter())
                .chain(t.influence.local.iter())
                .copied()
        })
        .collect();
    for (&vertex, &color) in &overlays {
        if detail != SkyWorldDetail::Planet && occupied.contains(&vertex) {
            continue;
        }
        let coord = geo.coord(vertex);
        let px = (((coord.longitude + 180.0) / 360.0) * f64::from(WIDTH))
            .floor()
            .clamp(0.0, f64::from(WIDTH - 1)) as usize;
        let py = (((90.0 - coord.latitude) / 180.0) * f64::from(HEIGHT))
            .floor()
            .clamp(0.0, f64::from(HEIGHT - 1)) as usize;
        let offset = (py * WIDTH as usize + px) * 3;
        out[offset..offset + 3].copy_from_slice(&color);
    }
    out
}

/// Render the fixed surface with the selected Skyworld layer over it.
/// type-audit: bare-ok(artifact: return)
pub fn render_skyworld_png(
    skyworld: &SkyWorld,
    terrain: &GeneratedTerrain,
    detail: SkyWorldDetail,
) -> Vec<u8> {
    hornvale_kernel::png::encode_rgb(WIDTH, HEIGHT, &pixels(skyworld, terrain, detail))
}

fn territory_line(territory: &SkyTerritory, detail: SkyWorldDetail) -> String {
    let physical = territory.physical.projected.len();
    let exchange = territory.exchange.projected.len();
    let local = territory.influence.local.len();
    let route = territory
        .influence
        .corridors
        .iter()
        .map(|c| c.projected.len())
        .sum::<usize>();
    let events = territory.influence.events.len();
    match detail {
        SkyWorldDetail::Planet => format!(
            "territory={} coverage={} centroid={:?} corridor={} event={}",
            territory.id, physical, territory.origin.surface, route, events
        ),
        SkyWorldDetail::Regional => format!(
            "territory={} physical={} projection={:?} influence={} route={} event={}",
            territory.id,
            physical,
            territory.origin.surface,
            local + exchange,
            route,
            events
        ),
        SkyWorldDetail::Habitat => format!(
            "territory={} phenotype={:?} lifecycle={:?} stocks={:?} physical={} projection={} influence={} route={}",
            territory.id,
            territory.phenotype,
            territory.lineage.current,
            territory.stocks,
            physical,
            exchange,
            local,
            route
        ),
    }
}

/// Render visible Skyworld consequences without exposing causal fields.
/// type-audit: bare-ok(artifact: return)
pub fn render_skyworld_readout(skyworld: &SkyWorld, detail: SkyWorldDetail) -> String {
    let mut out = String::new();
    let _ = writeln!(
        out,
        "skyworld detail={detail:?} territories={}",
        skyworld.territories.len()
    );
    for territory in &skyworld.territories {
        let _ = writeln!(out, "{}", territory_line(territory, detail));
        if !territory.physical.projected.is_empty() {
            out.push_str("shadow ");
        }
        if !territory.stocks.seed_spore_reserve.is_sign_negative()
            && territory.stocks.seed_spore_reserve > 0.0
        {
            out.push_str("spores ");
        }
        if territory.stocks.cloud_water > 0.0 {
            out.push_str("rain ");
        }
        if !territory.exchange.projected.is_empty() {
            out.push_str("cloud-contact ");
        }
        out.push('\n');
    }
    out
}

/// Render hidden atmospheric and resource causes for diagnostics only.
/// type-audit: bare-ok(prose: return)
pub fn render_skyworld_diagnostic_readout(skyworld: &SkyWorld, detail: SkyWorldDetail) -> String {
    format!(
        "detail={detail:?} pressure={} density={} radiation={} aether={} wind={:?} moisture={} stocks={:?} propagation={:?}",
        skyworld.fields.pressure,
        skyworld.fields.density,
        skyworld.fields.high_sky_radiation,
        skyworld.fields.aether,
        skyworld.fields.wind,
        skyworld.fields.moisture,
        skyworld
            .territories
            .iter()
            .map(|t| &t.stocks)
            .collect::<Vec<_>>(),
        skyworld
            .territories
            .iter()
            .map(|t| &t.influence)
            .collect::<Vec<_>>()
    )
}
