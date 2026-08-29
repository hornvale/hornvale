//! THE classifier. One definition of what a piece of ground IS, called by
//! the tile-scene builder and by the game client's plate, so the two can
//! never disagree.
//!
//! Why a function and not a wire field the plate reads back: `RegionScene`
//! is a CUBE-FACE quadtree that barycentrically resamples geosphere values
//! onto its nodes, and the plate addresses the geosphere mesh directly. A
//! plate fetching that scene would resample twice, violating decision 0287
//! (a tile IS a facet) and 0196 (never invent detail below the datum). See
//! The Legend spec §3.0.

/// The five elevation rungs, ascending. The index is [`elevation_band`]'s
/// return; the TUI binds these to ink, atlas binds them to colour.
/// type-audit: bare-ok(identifier-text)
pub const ELEVATION_LEGEND: [&str; 5] = ["abyssal", "lowland", "upland", "highland", "montane"];

/// Metres above sea level at which each band STARTS, ascending. Band 0 is
/// everything below `BAND_FLOORS_M[0]`.
const BAND_FLOORS_M: [f64; 4] = [0.0, 400.0, 1200.0, 2800.0];

/// Which [`ELEVATION_LEGEND`] rung `elevation_m` falls in, given this
/// world's own `sea_level_m`.
///
/// **Ordinal and total.** Monotone non-decreasing in `elevation_m`, so ink
/// density may carry it (decision NNNN). Sea level is the DATUM, never the
/// number zero — a world whose sea level is 1,200 m bands identically to one
/// at 0 m for the same height above sea.
/// type-audit: bare-ok(ratio: elevation_m), bare-ok(ratio: sea_level_m), bare-ok(index: return)
pub fn elevation_band(elevation_m: f64, sea_level_m: f64) -> u8 {
    let asl = elevation_m - sea_level_m;
    let mut band = 0u8;
    for floor in BAND_FLOORS_M {
        if asl >= floor {
            band += 1;
        }
    }
    band
}

/// The four water classes, in [`WATER_LEGEND`] order.
/// type-audit: bare-ok(identifier-text)
pub const WATER_LEGEND: [&str; 4] = ["ocean", "salt basin", "river", "dry"];

/// Which [`WATER_LEGEND`] class a [`hornvale_terrain::WaterKind`] is.
///
/// NOMINAL, not ordinal — "river" is not more or less than "ocean" — so
/// under decision NNNN this rides colour, and only the ocean/dry split
/// (a boundary the reader must trust) reaches a glyph.
/// type-audit: bare-ok(index: return)
pub fn water_class(kind: hornvale_terrain::WaterKind) -> u8 {
    match kind {
        hornvale_terrain::WaterKind::Ocean => 0,
        hornvale_terrain::WaterKind::SaltBasin => 1,
        hornvale_terrain::WaterKind::River => 2,
        hornvale_terrain::WaterKind::DryLand => 3,
    }
}
