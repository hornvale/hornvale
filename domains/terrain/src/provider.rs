//! The tier-1 terrain provider: a queryable generated tectonic globe.

use crate::boundaries::CellBoundary;
use crate::carve::Provenance;
use crate::globe::{GenesisOutcome, TectonicGlobe};
use crate::plates::dot;
use hornvale_kernel::{CellId, Geosphere, ReferenceElevation, math};

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
    /// type-audit: bare-ok(prose)
    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    /// Elevation at a cell, meters (relative to the isostatic reference
    /// datum — see `hornvale_kernel::ReferenceElevation`).
    pub fn elevation_at(&self, id: CellId) -> ReferenceElevation {
        *self.globe.elevation.get(id)
    }

    /// Unrest at a cell, in [0, 1].
    /// type-audit: bare-ok(ratio)
    pub fn unrest_at(&self, id: CellId) -> f64 {
        *self.globe.unrest.get(id)
    }

    /// The plate a cell belongs to.
    /// type-audit: bare-ok(index)
    pub fn plate_of(&self, id: CellId) -> u32 {
        *self.globe.plate_of.get(id)
    }

    /// Sea level, meters.
    pub fn sea_level(&self) -> ReferenceElevation {
        self.globe.sea_level
    }

    /// Whether a cell lies strictly below sea level.
    /// type-audit: bare-ok(flag)
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
    /// type-audit: pending(wave-2: latitude), pending(wave-2: longitude)
    pub fn nearest_cell(&self, latitude: f64, longitude: f64) -> CellId {
        let (lat, lon) = (latitude.to_radians(), longitude.to_radians());
        let target = [
            math::cos(lat) * math::cos(lon),
            math::cos(lat) * math::sin(lon),
            math::sin(lat),
        ];
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
    /// type-audit: bare-ok(count)
    pub fn drainage_at(&self, id: CellId) -> f64 {
        *self.globe.drainage.get(id)
    }

    /// Whether a cell is an endorheic (interior-draining) land cell.
    /// type-audit: bare-ok(flag)
    pub fn is_endorheic(&self, id: CellId) -> bool {
        *self.globe.endorheic.get(id)
    }

    /// The water classification (Ocean/SaltBasin/River/DryLand) at a cell.
    pub fn water_kind_at(&self, id: CellId) -> crate::water::WaterKind {
        *self.globe.water_kind.get(id)
    }

    /// Crust thickness at a cell, km.
    /// type-audit: bare-ok(ratio)
    pub fn crust_thickness_at(&self, id: CellId) -> f64 {
        *self.globe.crust.get(id)
    }

    /// Winning-craton age at a cell, `[0,1]` (0 on oceanic floor).
    /// type-audit: bare-ok(ratio)
    pub fn crust_age_at(&self, id: CellId) -> f64 {
        *self.globe.crust_age.get(id)
    }

    /// Whether a cell's crust clears the continental threshold.
    /// type-audit: bare-ok(flag)
    pub fn is_continental_at(&self, id: CellId) -> bool {
        self.crust_thickness_at(id) >= crate::crust::CONTINENTAL_THRESHOLD_KM
    }

    /// Graph hops to the nearest same-plate boundary cell (`None` = none reachable).
    /// type-audit: bare-ok(count)
    pub fn boundary_distance_at(&self, id: CellId) -> Option<u32> {
        self.globe.boundary_distance.get(id).map(|(hops, _)| hops)
    }

    /// Induration/hardness at a cell, `[0,1]` (the Sculpting/Ground seam,
    /// spec §4). Computed before elevation; agrees with `material_at`'s
    /// `induration` axis everywhere.
    /// type-audit: bare-ok(ratio)
    pub fn induration_at(&self, id: CellId) -> f64 {
        *self.globe.induration.get(id)
    }

    /// The material buffer at a cell (The Ground, spec §2).
    pub fn material_at(&self, id: CellId) -> crate::lithology::MaterialBuffer {
        *self.globe.lithology.get(id)
    }

    /// The rock class at a cell (The Ground, spec §4).
    pub fn rock_at(&self, id: CellId) -> crate::lithology::RockClass {
        crate::lithology::classify_rock(
            &self.material_at(id),
            self.drainage_at(id),
            self.is_endorheic(id),
            self.is_ocean(id),
            self.sediment_thickness_at(id),
        )
    }

    /// The carve's deposited sediment thickness at a cell, metres (≥ 0;
    /// Sculpting Task 10). Retained on the globe post-carve: repose's
    /// receiver-side gains, routing's floodplain/playa deposit, the marine
    /// wedge/delta fill, and atoll cap material, all summed.
    /// type-audit: bare-ok(ratio)
    pub fn sediment_thickness_at(&self, id: CellId) -> f64 {
        *self.globe.sediment_thickness.get(id)
    }

    /// The carve's net elevation delta at a cell, metres (± — Sculpting
    /// Task 10): incision subtracts, repose/deposition/wedge/delta/atoll
    /// all add. Already folded into `elevation_at`; retained separately so
    /// consumers can see how much of a cell's relief the carve moved.
    /// type-audit: bare-ok(ratio)
    pub fn carve_delta_at(&self, id: CellId) -> f64 {
        *self.globe.carve_delta_m.get(id)
    }

    /// The hydrogeologic class at a cell (The Ground, spec §3).
    pub fn hydro_at(&self, id: CellId) -> crate::lithology::Hydro {
        crate::lithology::hydrogeology(
            &self.material_at(id),
            self.drainage_at(id),
            self.is_ocean(id),
        )
    }

    /// Cave/karst void-proneness at a cell, `[0,1]` (The Ground, spec §3).
    /// type-audit: bare-ok(ratio)
    pub fn cave_proneness_at(&self, id: CellId) -> f64 {
        crate::lithology::cave_proneness(&self.material_at(id), self.drainage_at(id))
    }

    /// The cave at a cell, if the fluid-flow point process places one.
    pub fn cave_at(&self, id: CellId) -> Option<crate::features::Cave> {
        if self.is_ocean(id) {
            return None;
        }
        let belt = crate::features::belt_weight(self.boundary_distance_at(id));
        let pos = self.geosphere.position(id);
        let noise = crate::crust::sphere_fbm01(self.globe.features_noise_seed(), pos, 5.0, 4);
        let prob = crate::features::presence_prob(self.cave_proneness_at(id), belt);
        if noise >= prob {
            return None;
        }
        let buf = self.material_at(id);
        let near_fault = self.boundary_at(id).is_some();
        let kind = crate::features::cave_kind(&buf, near_fault);
        // Depth-reach grows with proneness (deeper karst in wetter, more soluble rock).
        let depth_reach_bands = 1 + (self.cave_proneness_at(id) * 3.0) as u32;
        Some(crate::features::Cave {
            kind,
            depth_reach_bands,
        })
    }

    /// The dominant ore deposit at a cell, if the point process places one.
    pub fn deposit_at(&self, id: CellId) -> Option<crate::features::Deposit> {
        if self.is_ocean(id) {
            return None;
        }
        let buf = self.material_at(id);
        let rock = self.rock_at(id);
        let boundary = self.boundary_at(id).map(|b| b.kind);
        let endorheic = self.is_endorheic(id);
        let (process, commodity) =
            crate::features::deposit_kind(rock, boundary, &buf, endorheic, self.crust_age_at(id))?;
        // Areal ores (rock IS the ore) are always present; point ores gate on prospectivity×belt×noise.
        let areal = matches!(process, crate::features::DepositProcess::ChemicalSediment)
            || matches!(process, crate::features::DepositProcess::Placer);
        let belt = crate::features::belt_weight(self.boundary_distance_at(id));
        let pos = self.geosphere.position(id);
        let noise = crate::crust::sphere_fbm01(self.globe.features_noise_seed(), pos, 7.0, 4);
        if !areal {
            let prob = crate::features::presence_prob(self.prospectivity_at(id), belt);
            if noise >= prob {
                return None;
            }
        }
        let (grade, tonnage) =
            crate::features::deposit_grade_tonnage(process, self.prospectivity_at(id), noise);
        Some(crate::features::Deposit {
            process,
            commodity,
            depth: crate::features::deposit_depth(process),
            grade,
            tonnage,
        })
    }

    /// The geothermal gradient at a cell (K/km) — the deep's energy base.
    pub fn geothermal_gradient_at(&self, id: CellId) -> crate::strata::GeothermalGradient {
        crate::strata::geothermal_gradient(
            self.crust_thickness_at(id),
            self.crust_age_at(id),
            self.is_continental_at(id),
        )
    }

    /// The cell's stratigraphic column — its vertical dimension and deep-time archive.
    pub fn column_at(&self, id: CellId) -> crate::strata::StratigraphicColumn {
        let buf = self.material_at(id);
        crate::strata::column(
            self.crust_thickness_at(id),
            self.crust_age_at(id),
            self.is_continental_at(id),
            self.sediment_thickness_at(id),
            buf.soil_depth.get(),
            self.rock_at(id),
            buf.basement,
        )
    }

    /// Depth to crystalline basement at a cell, metres.
    /// type-audit: bare-ok(diagnostic-value: return)
    pub fn depth_to_basement_at(&self, id: CellId) -> f64 {
        self.column_at(id).depth_to_basement_m
    }

    /// Whether the cell's column records a nonconformity (missing time).
    /// type-audit: bare-ok(flag: return)
    pub fn unconformity_at(&self, id: CellId) -> bool {
        self.column_at(id).unconformity
    }

    /// Walk-facing appearance vector at a cell (The Ground, spec §3).
    pub fn appearance_at(&self, id: CellId) -> crate::lithology::Appearance {
        crate::lithology::appearance(&self.material_at(id), self.rock_at(id))
    }

    /// Mineral prospectivity at a cell, `[0,1]` (The Ground, spec §3; the
    /// deferred deposits campaign's down-payment).
    /// type-audit: bare-ok(ratio)
    pub fn prospectivity_at(&self, id: CellId) -> f64 {
        crate::lithology::prospectivity(
            &self.material_at(id),
            self.boundary_at(id).map(|b| b.kind),
            self.unrest_at(id),
        )
    }

    /// Waterfall (knickpoint) sites the carve found (Sculpting Task 11, spec
    /// §5): land cells where a high-drainage watercourse crosses a sharp
    /// PRE-carve induration step. Sorted ascending `CellId`.
    pub fn waterfalls(&self) -> &[CellId] {
        &self.globe.waterfall_sites
    }

    /// Cells a river-mouth delta lobe raised above sea level (Sculpting Task
    /// 9/11, spec §5). Not independently sorted here beyond
    /// `deposit_wedge`'s own ascending-`CellId` dedup.
    pub fn deltas(&self) -> &[CellId] {
        &self.globe.delta_cells
    }

    /// Playas: endorheic interior sinks carrying real sediment fill
    /// (Sculpting Task 11, spec §5) — the salt-flat floors the carve's
    /// routing filled toward flat. Computed live rather than stored, since
    /// it is a plain filter over two fields the globe already retains;
    /// ascending `CellId` (cell iteration order).
    pub fn playas(&self) -> Vec<CellId> {
        self.geosphere
            .cells()
            .filter(|&c| self.is_endorheic(c) && self.sediment_thickness_at(c) > 0.0)
            .collect()
    }

    /// Provenance of every waterfall this provider exposes (spec §5): this
    /// campaign only builds the ordinary geologic process. A future gated
    /// mythic overlay would land an alternate per-class accessor rather
    /// than widen this one — phenomena never reveal their producing
    /// system, so every waterfall reads uniformly today.
    pub fn waterfall_provenance(&self) -> Provenance {
        Provenance::Process
    }

    /// Provenance of every delta this provider exposes (spec §5); see
    /// [`Self::waterfall_provenance`].
    pub fn delta_provenance(&self) -> Provenance {
        Provenance::Process
    }

    /// Provenance of every playa this provider exposes (spec §5); see
    /// [`Self::waterfall_provenance`].
    pub fn playa_provenance(&self) -> Provenance {
        Provenance::Process
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

    #[test]
    fn provider_exposes_crust_and_boundary_distance() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());
        for cell in geo.cells() {
            assert_eq!(
                terrain.crust_thickness_at(cell),
                *outcome.globe.crust.get(cell)
            );
            assert_eq!(
                terrain.is_continental_at(cell),
                *outcome.globe.crust.get(cell) >= crate::crust::CONTINENTAL_THRESHOLD_KM
            );
            // Age is 0 on oceanic floor, in [0,1] everywhere.
            let age = terrain.crust_age_at(cell);
            assert!((0.0..=1.0).contains(&age));
            assert_eq!(
                terrain.crust_age_at(cell),
                *outcome.globe.crust_age.get(cell)
            );
        }
        // Some cell is within finite graph distance of a boundary.
        assert!(
            geo.cells()
                .any(|c| terrain.boundary_distance_at(c).is_some())
        );
    }

    #[test]
    fn provider_exposes_point_observations_and_their_provenance() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome.clone());

        assert_eq!(
            terrain.waterfalls(),
            outcome.globe.waterfall_sites.as_slice()
        );
        assert_eq!(terrain.deltas(), outcome.globe.delta_cells.as_slice());

        // playas() is a live filter, not a stored field: check it against
        // the same filter applied directly.
        let expected_playas: Vec<CellId> = geo
            .cells()
            .filter(|&c| terrain.is_endorheic(c) && terrain.sediment_thickness_at(c) > 0.0)
            .collect();
        assert_eq!(terrain.playas(), expected_playas);

        // This campaign only ever draws the ordinary geologic process.
        assert_eq!(terrain.waterfall_provenance(), Provenance::Process);
        assert_eq!(terrain.delta_provenance(), Provenance::Process);
        assert_eq!(terrain.playa_provenance(), Provenance::Process);
    }
}
