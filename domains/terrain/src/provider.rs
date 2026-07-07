//! The tier-1 terrain provider: a queryable generated tectonic globe.

use crate::boundaries::CellBoundary;
use crate::globe::{GenesisOutcome, TectonicGlobe};
use crate::plates::dot;
use hornvale_kernel::{CellId, Geosphere};

/// A queryable tectonic terrain provider. Owns its Geosphere so queries and
/// the globe's CellMaps always agree on the cell space — a CellMap must
/// only ever be read with the mesh that built it.
#[derive(Debug, Clone)]
pub struct GeneratedTerrain {
    geosphere: Geosphere,
    globe: TectonicGlobe,
    notes: Vec<String>,
}

impl GeneratedTerrain {
    /// Wrap a genesis outcome with the Geosphere it was generated over.
    /// Panics (fail fast) if the mesh and the globe disagree on cell count —
    /// the caller must pass the same Geosphere it gave `generate`.
    pub fn new(geosphere: Geosphere, outcome: GenesisOutcome) -> GeneratedTerrain {
        assert_eq!(
            geosphere.cell_count(),
            outcome.globe.elevation.len(),
            "GeneratedTerrain: geosphere and globe disagree on cell count"
        );
        GeneratedTerrain {
            geosphere,
            globe: outcome.globe,
            notes: outcome.notes,
        }
    }

    /// The Geosphere the globe is computed over.
    pub fn geosphere(&self) -> &Geosphere {
        &self.geosphere
    }

    /// The full tectonic globe.
    pub fn globe(&self) -> &TectonicGlobe {
        &self.globe
    }

    /// Degradation notes recorded at genesis.
    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    /// Elevation at a cell, meters.
    pub fn elevation_at(&self, id: CellId) -> f64 {
        *self.globe.elevation.get(id)
    }

    /// Unrest at a cell, in [0, 1].
    pub fn unrest_at(&self, id: CellId) -> f64 {
        *self.globe.unrest.get(id)
    }

    /// The plate a cell belongs to.
    pub fn plate_of(&self, id: CellId) -> u32 {
        *self.globe.plate_of.get(id)
    }

    /// Sea level, meters.
    pub fn sea_level(&self) -> f64 {
        self.globe.sea_level
    }

    /// Whether a cell lies strictly below sea level.
    pub fn is_ocean(&self, id: CellId) -> bool {
        self.elevation_at(id) < self.globe.sea_level
    }

    /// The strongest cross-plate boundary contact at a cell, if any.
    pub fn boundary_at(&self, id: CellId) -> Option<CellBoundary> {
        *self.globe.boundary.get(id)
    }

    /// The cell nearest a geographic coordinate (degrees), by maximum dot
    /// product with the coordinate's unit vector; ties break to the lower
    /// cell id. Inverts the kernel's coord convention — latitude = asin(z),
    /// longitude = atan2(y, x) — so `nearest_cell(coord(c))` returns `c`.
    pub fn nearest_cell(&self, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
        let mut best = CellId(0);
        let mut best_dot = f64::NEG_INFINITY;
        for cell in self.geosphere.cells() {
            let d = dot(self.geosphere.position(cell), target);
            if d > best_dot {
                best_dot = d;
                best = cell;
            }
        }
        best
    }

    /// Flow-accumulation drainage at a cell (upstream land-cell count; 0 on ocean).
    pub fn drainage_at(&self, id: CellId) -> f64 {
        *self.globe.drainage.get(id)
    }

    /// Whether a cell is an endorheic (interior-draining) land cell.
    pub fn is_endorheic(&self, id: CellId) -> bool {
        *self.globe.endorheic.get(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{CellId, Geosphere, Seed};

    #[test]
    fn provider_answers_every_query_consistently() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
        let cell = CellId(0);
        assert_eq!(
            terrain.elevation_at(cell),
            *outcome.globe.elevation.get(cell)
        );
        assert_eq!(terrain.plate_of(cell), *outcome.globe.plate_of.get(cell));
        assert_eq!(terrain.unrest_at(cell), *outcome.globe.unrest.get(cell));
        assert_eq!(terrain.sea_level(), outcome.globe.sea_level);
        assert_eq!(
            terrain.is_ocean(cell),
            terrain.elevation_at(cell) < terrain.sea_level()
        );
        assert_eq!(terrain.geosphere().cell_count(), geo.cell_count());
        assert_eq!(terrain.notes(), outcome.notes.as_slice());
    }

    #[test]
    fn nearest_cell_round_trips_cell_coordinates() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in [CellId(0), CellId(100), CellId(641)] {
            let coord = geo.coord(cell);
            assert_eq!(terrain.nearest_cell(coord.latitude, coord.longitude), cell);
        }
    }

    #[test]
    #[should_panic(expected = "disagree on cell count")]
    fn mismatched_geosphere_fails_fast() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        GeneratedTerrain::new(Geosphere::new(2), outcome);
    }

    #[test]
    fn provider_exposes_boundary_classification() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
        for cell in geo.cells() {
            assert_eq!(terrain.boundary_at(cell), *outcome.globe.boundary.get(cell));
        }
        // At least one cell is a classified boundary on a real globe.
        assert!(geo.cells().any(|c| terrain.boundary_at(c).is_some()));
    }

    #[test]
    fn provider_exposes_drainage_and_endorheic() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
        for cell in geo.cells() {
            assert_eq!(terrain.drainage_at(cell), *outcome.globe.drainage.get(cell));
            assert_eq!(
                terrain.is_endorheic(cell),
                *outcome.globe.endorheic.get(cell)
            );
        }
        // Land cells accumulate at least themselves.
        let land = geo.cells().find(|c| !terrain.is_ocean(*c)).unwrap();
        assert!(terrain.drainage_at(land) >= 1.0);
    }
}
