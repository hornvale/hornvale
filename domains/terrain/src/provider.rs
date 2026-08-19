//! The tier-1 terrain provider: a queryable generated tectonic globe.

use crate::boundaries::CellBoundary;
use crate::carve::Provenance;
use crate::channel::{ChannelNetwork, Transverse};
use crate::globe::{GenesisOutcome, TectonicGlobe};
use crate::landscape::{self, Feature, FeatureClass, FeatureIndex};
use crate::plates::dot;
use crate::water::WaterKind;
use hornvale_kernel::{CellId, Geosphere, ReferenceElevation, math};

/// A queryable tectonic terrain provider. Owns its Geosphere so queries and
/// the globe's CellMaps always agree on the cell space — a CellMap must
/// only ever be read with the mesh that built it.
#[derive(Debug, Clone)]
pub struct GeneratedTerrain {
    geosphere: Geosphere,
    globe: TectonicGlobe,
    notes: Vec<String>,
    channels: ChannelNetwork,
    features: FeatureIndex,
}

/// Winning-craton age above which crust counts as ancient enough to have
/// witnessed the pre-human deep (`[0,1]`, `GeneratedTerrain::crust_age_at`'s
/// scale).
/// type-audit: bare-ok(ratio)
const ANCIENT_CRUST_AGE: f64 = 0.8;

/// Spatial frequency for the pre-human scar presence noise. Distinct from
/// The Lode's cave (5.0) and deposit (7.0) frequencies sampled off the same
/// seed, so the three point processes decorrelate on the sphere.
/// type-audit: bare-ok(ratio)
const PREHUMAN_SCAR_FREQ: f64 = 11.0;

/// fBm octaves for the pre-human scar presence noise (matches The Lode's
/// caves/deposits).
/// type-audit: bare-ok(count)
const PREHUMAN_SCAR_OCTAVES: u32 = 4;

/// Presence threshold for the pre-human scar noise test: `sphere_fbm01`
/// compresses variance toward 0.5 (see `crust.rs`), so this is not a small
/// absolute probability but is still sparse relative to the ancient-crust
/// population it gates within — at seed 42 it selects 1 of ~1900
/// ancient-crust cells.
/// type-audit: bare-ok(ratio)
const PREHUMAN_SCAR_THRESHOLD: f64 = 0.30;

/// Promote a pointwise `Aquifer` reading to `Spring`: `hydrogeology`
/// classifies one cell's rock, but a spring is not a property of a single
/// cell, it is a property of a *contact* — "where an aquifer meets the
/// surface with flow" (The Witness, Task 5b; decision 0085's precedent for
/// splitting durable pointwise petrophysics from derived geometry). Any
/// other pointwise reading passes through unchanged. Free of `Geosphere`/
/// `TectonicGlobe` so it is unit-testable on a hand-built neighbourhood,
/// mirroring the pattern `hydrogeology` itself already uses.
fn promote_to_spring(
    base: crate::lithology::Hydro,
    cell_elevation: ReferenceElevation,
    neighbors: impl Iterator<Item = (crate::lithology::Hydro, ReferenceElevation)>,
) -> crate::lithology::Hydro {
    if base != crate::lithology::Hydro::Aquifer {
        return base;
    }
    let descending_contact = neighbors.into_iter().any(|(nb_hydro, nb_elevation)| {
        nb_hydro != crate::lithology::Hydro::Aquifer
            && nb_elevation.total_cmp(cell_elevation) == std::cmp::Ordering::Less
    });
    if descending_contact {
        crate::lithology::Hydro::Spring
    } else {
        crate::lithology::Hydro::Aquifer
    }
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
        // Built once here, beside the other genesis-time derivations, not
        // lazily per call (The Ford, Task 5). A pure read over already-
        // committed state plus one hash-noise field — see `ChannelNetwork`'s
        // own doc comment for why this makes no seed draws.
        let channels = ChannelNetwork::build(
            &outcome.globe,
            &geosphere,
            outcome.globe.channel_noise_seed(),
        );

        // The Gazetteer, Task 5: the landscape feature index, also built
        // once here rather than lazily (Task 1 measured the traversal at
        // ~4.8 ms against a ~200 ms terrain build, 2.4%, under the 5%
        // rule). NO Volcano class: `volcano_at` lives in `windows/worldgen`,
        // which this crate may not depend on (`cli/tests/architecture.rs`),
        // so a volcano can never enter an index built here — Task 6
        // assembles the full set including volcanoes at the composition
        // root.
        //
        // Membership predicates read `outcome.globe` directly rather than
        // through `Self::water_kind_at`/`Self::elevation_at`: those accessors
        // need `self`, which does not exist until the struct literal below,
        // and `outcome.globe`'s `elevation`/`sea_level`/`water_kind` fields
        // are already fully computed at genesis (see their doc comments in
        // `globe.rs`) — nothing here forces a restructure. Landmass and Sea
        // use `elevation`/`sea_level` and `water_kind == WaterKind::Ocean`
        // respectively rather than `WaterKind` for land, for parity with
        // `crate::landscape::rivers`'s own land test (`elevation >=
        // sea_level`) and because it's exactly what `water::classify`'s
        // `Ocean` arm computes (`elevation < sea_level`, top precedence) —
        // so the two are equivalent, and this avoids a `classify()` call per
        // cell.
        let land = |c: CellId| *outcome.globe.elevation.get(c) >= outcome.globe.sea_level;
        let total_land = geosphere.cells().filter(|&c| land(c)).count();
        let total_ocean = geosphere
            .cells()
            .filter(|&c| *outcome.globe.water_kind.get(c) == WaterKind::Ocean)
            .count();
        // Floors: MEASURED (Task 1, controller-ruled) — see the doc comment
        // on each class arm below. Landmass/Sea scale with this world's own
        // land/ocean extent; SaltLake/River are fixed constants.
        let landmass_floor = (0.005 * total_land as f64) as usize;
        let sea_floor = (0.005 * total_ocean as f64) as usize;
        let landmass: Vec<Feature> =
            landscape::classify(&geosphere, FeatureClass::Landmass, land, landmass_floor);
        let sea: Vec<Feature> = landscape::classify(
            &geosphere,
            FeatureClass::Sea,
            |c| *outcome.globe.water_kind.get(c) == WaterKind::Ocean,
            sea_floor,
        );
        // SaltLake floor is 1, not the spec's 20 (which yields zero here):
        // `WaterKind::SaltBasin` is a classification (a terminal endorheic
        // sink), not a threshold on a continuous field, so a 1-cell salt
        // basin is a real feature rather than a quantization artifact.
        let salt_lake: Vec<Feature> = landscape::classify(
            &geosphere,
            FeatureClass::SaltLake,
            |c| *outcome.globe.water_kind.get(c) == WaterKind::SaltBasin,
            1,
        );
        // River floor is the spec's own catchment tier, retained.
        let river: Vec<Feature> = landscape::rivers(
            &geosphere,
            &outcome.globe.elevation,
            outcome.globe.sea_level,
            24,
        );
        let features = FeatureIndex::from_parts(vec![
            (FeatureClass::Landmass, landmass),
            (FeatureClass::Sea, sea),
            (FeatureClass::SaltLake, salt_lake),
            (FeatureClass::River, river),
        ]);

        GeneratedTerrain {
            geosphere,
            globe: outcome.globe,
            notes: outcome.notes,
            channels,
            features,
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
        let target = math::unit_sphere_from_lat_lon(latitude, longitude);
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

    /// The nearest reachable same-plate contact — how far, and what kind.
    ///
    /// [`boundary_at`](Self::boundary_at) answers only for a cell that *is* a
    /// contact; this answers for any cell, by reading the kind off the boundary
    /// cell the distance field already attributes as the source. A consumer
    /// that cares about the stress *regime* rather than mere proximity needs
    /// the kind — a rift and a collision sit the same distance away and do
    /// opposite things to a fracture. The distance field seeds every boundary
    /// cell with `(0, itself)`, so the kind is always present when the distance
    /// is.
    /// type-audit: bare-ok(count: return)
    pub fn nearest_boundary_at(&self, id: CellId) -> Option<(u32, crate::BoundaryKind)> {
        let (hops, cell) = (*self.globe.boundary_distance.get(id))?;
        Some((hops, self.boundary_at(cell)?.kind))
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

    /// The hydrogeologic class at a cell (The Ground, spec §3). `hydrogeology`
    /// itself is pointwise matrix petrophysics and never returns `Spring`
    /// (The Witness, Task 5b); this is the one place that promotes an
    /// `Aquifer` reading to `Spring` when the cell sits at a descending
    /// contact — see [`promote_to_spring`] and decision 0085 (pointwise
    /// petrophysics is the durable signal, geometric promotion is derived
    /// from it, computed here where the geosphere is in hand).
    pub fn hydro_at(&self, id: CellId) -> crate::lithology::Hydro {
        let base = crate::lithology::hydrogeology(&self.material_at(id), self.is_ocean(id));
        let cell_elevation = self.elevation_at(id);
        let neighbors = self.geosphere.neighbors(id).iter().map(|&nb| {
            let nb_hydro = crate::lithology::hydrogeology(&self.material_at(nb), self.is_ocean(nb));
            (nb_hydro, self.elevation_at(nb))
        });
        promote_to_spring(base, cell_elevation, neighbors)
    }

    /// Cave/karst void-proneness at a cell, `[0,1]` (The Ground, spec §3).
    /// type-audit: bare-ok(ratio)
    pub fn cave_proneness_at(&self, id: CellId) -> f64 {
        crate::lithology::cave_proneness(&self.material_at(id), self.drainage_at(id))
    }

    /// The cave at a cell, if the fluid-flow point process places one.
    ///
    /// Kind is selected BEFORE existence is tested (`features::cave_process`),
    /// existence is gated on that kind's own proneness against a uniformized
    /// noise sample, and depth reads the cell's stratigraphic column — the
    /// three repairs of The Hollow (spec §3).
    ///
    /// Since The Underworld (spec §4.0) depth is a budget in **metres**
    /// ([`crate::cave_depth::cave_depth_reach_m`]) rather than a band index,
    /// and it does not read `proneness` — the band is derived from the budget.
    /// `proneness` reaches the presence gate below and nothing else.
    pub fn cave_at(&self, id: CellId) -> Option<crate::features::Cave> {
        if self.is_ocean(id) {
            return None;
        }
        let (kind, proneness) = crate::features::cave_process(
            &self.material_at(id),
            self.drainage_at(id),
            self.crust_age_at(id),
            self.nearest_boundary_at(id),
        )?;
        let belt = crate::features::belt_weight(self.boundary_distance_at(id));
        let prob = crate::features::presence_prob(proneness, belt);
        let pos = self.geosphere.position(id);
        let noise = crate::features::uniformize(crate::crust::sphere_fbm01(
            self.globe.features_noise_seed(),
            pos,
            crate::features::CAVE_GATE_FREQ,
            crate::features::CAVE_GATE_OCTAVES,
        ));
        if noise >= prob {
            return None;
        }
        Some(crate::features::Cave::new(
            kind,
            &self.material_at(id),
            &self.column_at(id),
        ))
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

    /// Whether a cell carries a rare pre-human "gate scar": deep crust old
    /// enough to have witnessed the pre-human world, plus a hash-noise
    /// presence test firing. Pure and deterministic — no draws, no facts,
    /// no epoch — reuses The Lode's FEATURES noise seed exactly as
    /// [`cave_at`](Self::cave_at)/[`deposit_at`](Self::deposit_at) do, so no
    /// new stream label is introduced. Ocean cells never qualify (nothing
    /// pre-human is legible under open water in this model). This
    /// encapsulates the calibration coupling between the ancient-crust
    /// threshold and terrain's internal presence noise so a caller (e.g.
    /// `windows/worldgen`'s pre-human vestige gate) reads only the boolean,
    /// never terrain's noise field directly.
    /// type-audit: bare-ok(flag: return)
    pub fn prehuman_scar_at(&self, id: CellId) -> bool {
        if self.is_ocean(id) {
            return false;
        }
        if self.crust_age_at(id) <= ANCIENT_CRUST_AGE {
            return false;
        }
        let pos = self.geosphere.position(id);
        let noise = crate::crust::sphere_fbm01(
            self.globe.features_noise_seed(),
            pos,
            PREHUMAN_SCAR_FREQ,
            PREHUMAN_SCAR_OCTAVES,
        );
        noise < PREHUMAN_SCAR_THRESHOLD
    }

    /// Whether a cell carries a volcanic **edifice** — the gated island-arc
    /// cone the elevation raised there (The Repose, Task 4).
    ///
    /// A derived read, not a second model: it resamples the retained
    /// `streams::ARC_GATE` hash-noise field at the same **source** boundary
    /// cell `assemble_elevation` sampled (one value per contact, so a whole
    /// edifice shares it) and hands the result to
    /// [`elevation::edifice_present`](crate::elevation), the very predicate
    /// the island-arc profile arm applies. Pure and deterministic — the gate
    /// is hash-noise, never consumed as a `Stream`, so this costs no draw and
    /// touches no draw-order/save-format contract, exactly as
    /// [`cave_at`](Self::cave_at) and [`prehuman_scar_at`](Self::prehuman_scar_at)
    /// resample terrain's other noise fields.
    ///
    /// `false` everywhere but an island arc's overriding side: a coastal
    /// range's volcanic line shares the ungated collision-belt crest, so the
    /// shipped elevation holds nothing that separates a volcanic crest cell
    /// from a non-volcanic one there (see `elevation::edifice_present`).
    ///
    /// **Read that limit as being about the tectonic LABEL, not about the
    /// shape — a subaerial volcano on a continent-scale landmass is reachable
    /// today.** Measured on seed 42 at L6 (40,962 cells): of 360 edifice
    /// cells, **55 stand above sea level**, and they sit on land components
    /// of 1, 40, 104, 1277, 1842, 1976 and 1994 cells — the four largest of
    /// which are this world's four largest landmasses outright. So what the
    /// paragraph above rules out is `CoastalRange`-labelled volcanism, not a
    /// mountain that erupts over a continent; a campaign wanting the latter
    /// needs no terrain epoch (The Repose, Task 4 review).
    ///
    /// A pure restatement of [`edifice_source_at`](Self::edifice_source_at) —
    /// there is one derivation, and this is the question that only asks
    /// whether it answered.
    /// type-audit: bare-ok(flag: return)
    pub fn has_edifice(&self, id: CellId) -> bool {
        self.edifice_source_at(id).is_some()
    }

    /// The **source contact cell** of the edifice a cell belongs to, or
    /// `None` where there is no edifice (The Repose, Task 5).
    ///
    /// This is the identity of one edifice, and it exists because an
    /// edifice is wider than one cell: the gate is sampled once per
    /// **contact**, at the source, precisely so every cell attributed to
    /// that contact shares one value, and the elevation then decays that
    /// value out to `ARC_EDIFICE_DECAY_CELLS`. Every cell of one contact's
    /// edifice therefore answers with the *same* source, while the query
    /// cell it was asked about differs — so a consumer that keys identity
    /// (or a name) on the query cell mints several edifices for one
    /// landform. On the canonical seed-42 L6 globe that is 360 edifice
    /// cells over ~187 **contact cells** — read that as 187 identities, not
    /// 187 physically separate cones: adjacent contacts along one arc are
    /// common (173 of the 187 have another contact as a same-plate
    /// neighbor), so one continuous volcanic arc resolves to a *chain* of
    /// separately identified edifices rather than a single one. See
    /// `windows/worldgen/src/volcano.rs`'s module docs for what that means
    /// for naming.
    ///
    /// Same purity as [`has_edifice`](Self::has_edifice), which is defined in
    /// terms of this: the arc gate is hash-noise resampled at the source, so
    /// this consumes no draw and touches no draw-order/save-format contract.
    pub fn edifice_source_at(&self, id: CellId) -> Option<CellId> {
        let (distance, source) = (*self.globe.boundary_distance.get(id))?;
        let contact = (*self.globe.boundary.get(source))?;
        let plate = &self.globe.plates[*self.globe.plate_of.get(id) as usize];
        let arc_side = plate.id > contact.other_plate;
        let gate = crate::elevation::arc_gate_fbm(self.globe.arc_gate_seed)
            .sample(self.geosphere.position(source));
        crate::elevation::edifice_present(contact.kind, arc_side, distance, gate).then_some(source)
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

    /// The river channel network (The Ford, spec §5.2/§5.3): the world's
    /// rivers as polylines, with the discharge-derived band geometry each
    /// vertex implies. Built once at construction — see [`Self::new`].
    pub fn channels(&self) -> &ChannelNetwork {
        &self.channels
    }

    /// The genesis-time landscape feature index (The Gazetteer, spec §3):
    /// individuated landmasses, seas, salt lakes, and rivers, ordered within
    /// each class by magnitude descending then identity ascending. Built
    /// once at construction — see [`Self::new`]. Carries no `Volcano`
    /// features; `windows/worldgen` assembles those alongside this index's
    /// four classes.
    pub fn features(&self) -> &FeatureIndex {
        &self.features
    }

    /// The transverse band at `position`, and the signed great-circle
    /// distance to the nearest channel in radians (The Ford, spec §5.3) —
    /// see [`ChannelNetwork::transverse_at`], which this delegates to
    /// directly. `position` is O(total channel vertices) per call; a hot
    /// caller should batch queries rather than call this per-pixel.
    /// type-audit: pending(wave-1: position), pending(wave-1: return)
    pub fn transverse_at(&self, position: [f64; 3]) -> (Transverse, f64) {
        self.channels.transverse_at(position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::globe::generate;
    use crate::pins::TerrainPins;
    use hornvale_kernel::{CellId, CellMap, Geosphere, Seed};

    /// The edifice read must name the cells the SHIPPED elevation raised as
    /// edifices — one source of truth, never a second opinion.
    ///
    /// The proof is behavioural, and deliberately NOT a comparison against a
    /// re-implementation of the gate formula (which would only establish that
    /// two copies of one expression agree). It re-runs the shipped assembler
    /// over the globe's own inputs under TWO arc-gate seeds and
    /// cross-examines the accessor against where the elevation actually
    /// moved:
    ///
    /// - **anchor** — under the shipped gate seed the re-run reproduces the
    ///   shipped pre-carve surface (`elevation - carve_delta_m`), so the
    ///   inputs really are the ones that shipped;
    /// - **no phantom edifices** — a verdict that differs between the two
    ///   gate fields must move the elevation, and move it UP on the side that
    ///   calls it an edifice. A wrong seed, a wrong sample position or a
    ///   wrong side desynchronises the verdict from the elevation; an
    ///   inverted threshold flips on exactly the same cells but lowers them.
    /// - **no missed cones** — an elevation that moves where the verdict does
    ///   NOT differ must lie outside the edifice's own decay length. The read
    ///   may narrow the skirt (`edifice_present` says so); it may not miss a
    ///   cone.
    #[test]
    fn has_edifice_names_the_cells_the_shipped_elevation_raised() {
        let geo = Geosphere::new(5);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        let globe = terrain.globe().clone();
        // `assemble_elevation`'s inputs, all read back off the globe it
        // produced. `continental` is the crust-threshold flag the provider
        // publishes; the relief seed is the same leg `generate_elevation`
        // derives (it is held fixed across both runs, so relief cancels).
        let continental = CellMap::from_fn(&geo, |c| terrain.is_continental_at(c));
        let terrain_seed = Seed(42).derive(crate::streams::ROOT);
        let relief_seed = terrain_seed.derive(crate::streams::RELIEF);
        let assemble = |arc_gate: Seed| {
            crate::elevation::assemble_elevation(
                &geo,
                &globe.plates,
                &globe.plate_of,
                &globe.boundary,
                &globe.boundary_distance,
                &globe.trail_seamounts,
                &globe.crust,
                &continental,
                arc_gate,
                &globe.induration,
                relief_seed,
            )
        };
        let shipped_gate = globe.arc_gate_seed;
        // A plainly different gate field over identical everything else.
        let other_gate = Seed(shipped_gate.0 ^ 0xA5A5_A5A5_A5A5_A5A5);
        let elev_a = assemble(shipped_gate);
        let elev_b = assemble(other_gate);

        // Anchor. `elevation == elevation_pre + carve_delta_m` is a retained
        // identity, but the sum is re-associated on the way there, so this
        // compares at 1e-6 m — nine orders below the edifice signal.
        for cell in geo.cells() {
            let pre = terrain.elevation_at(cell).get() - terrain.carve_delta_at(cell);
            assert!(
                (elev_a.get(cell).get() - pre).abs() < 1e-6,
                "re-run under the shipped gate seed is not the shipped surface at {cell:?}: \
                 {} vs {pre}",
                elev_a.get(cell).get()
            );
        }

        // The accessor under the other gate field: the same code path, the
        // same globe, one seed changed.
        let mut terrain_b = terrain.clone();
        terrain_b.globe.arc_gate_seed = other_gate;

        let mut differing_verdicts = 0_u32;
        let mut moved_elevations = 0_u32;
        for cell in geo.cells() {
            let (a, b) = (elev_a.get(cell).get(), elev_b.get(cell).get());
            let (ed_a, ed_b) = (terrain.has_edifice(cell), terrain_b.has_edifice(cell));
            if ed_a != ed_b {
                differing_verdicts += 1;
                let (raised, lowered) = if ed_a { (a, b) } else { (b, a) };
                // STRICT `>`, and that couples this assertion to a data
                // property nothing states: an arc contact whose edifice
                // magnitude happened to be exactly 0 would raise nothing,
                // making `raised == lowered` and reddening a correct read.
                // It holds on seed 42 deterministically (this is a fixed
                // seed, so it is not a flake), and it is left strict on
                // purpose — `>=` would also pass for a gate that moved no
                // elevation at all, which is exactly the failure the
                // "no phantom edifices" arm exists to catch. If a future
                // seed or profile change trips this, the fix is to skip
                // zero-magnitude contacts explicitly, NOT to weaken the
                // comparison.
                assert!(
                    raised > lowered,
                    "the gate field that calls {cell:?} an edifice does not stand higher \
                     there: {raised} vs {lowered}"
                );
            }
            if a != b {
                moved_elevations += 1;
                if ed_a == ed_b {
                    let hops = terrain
                        .boundary_distance_at(cell)
                        .expect("a cell whose elevation the arc gate moved has a boundary");
                    assert!(
                        f64::from(hops) > crate::elevation::ARC_EDIFICE_DECAY_CELLS,
                        "the gate moved {cell:?} at {hops} hop(s) — inside the edifice's own \
                         decay length — and the read did not notice"
                    );
                }
            }
        }
        assert!(
            differing_verdicts > 0,
            "no cell changed its edifice verdict between two gate fields — the test is vacuous"
        );
        assert!(
            moved_elevations > differing_verdicts,
            "expected the gate to move more cells than it renames (the skirt beyond the cone): \
             {moved_elevations} moved, {differing_verdicts} renamed"
        );
    }

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

    #[test]
    fn prehuman_scar_never_fires_on_ocean() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in geo.cells() {
            if terrain.is_ocean(cell) {
                assert!(
                    !terrain.prehuman_scar_at(cell),
                    "ocean cells never carry a pre-human scar"
                );
            }
        }
    }

    /// The `Cave` derived-field invariant, on live worlds: `deepest_band` is
    /// always the band the cave's own metre budget reaches (spec §4.0). Both
    /// constructors preserve it by construction, so this asserts that the
    /// generator really does go through one — a hand-built literal anywhere in
    /// the production path would show up here.
    ///
    /// Non-vacuous by assertion: the sweep must actually see caves, and it must
    /// see the band vary, or a generator that returned one constant band for
    /// everything would satisfy the invariant trivially.
    /// claim: invariant(forall-seed, forall-cave) — `deepest_band` equals
    /// `band_at_depth(column, depth_reach_m)` for every cave the generator
    /// authors, over a small fixed seed set at a level-4 globe
    #[test]
    fn every_generated_cave_agrees_with_its_own_depth_budget() {
        let geo = Geosphere::new(4);
        let mut seen = 0usize;
        let mut bands: std::collections::BTreeSet<&'static str> = std::collections::BTreeSet::new();
        for raw in [1u64, 7, 42] {
            let outcome = generate(Seed(raw), &geo, &TerrainPins::default()).unwrap();
            let terrain = GeneratedTerrain::new(geo.clone(), outcome);
            for cell in geo.cells() {
                let Some(cave) = terrain.cave_at(cell) else {
                    continue;
                };
                let column = terrain.column_at(cell);
                assert!(
                    cave.band_agrees_with_reach(&column),
                    "seed {raw} cell {cell:?}: band {:?} against a {} m budget, \
                     whose column puts it in {:?}",
                    cave.deepest_band,
                    cave.depth_reach_m,
                    crate::features::band_at_depth(&column, cave.depth_reach_m)
                );
                seen += 1;
                bands.insert(match cave.deepest_band {
                    crate::strata::BandKind::Regolith => "Regolith",
                    crate::strata::BandKind::Cover => "Cover",
                    crate::strata::BandKind::Basement => "Basement",
                    crate::strata::BandKind::Roots => "Roots",
                    crate::strata::BandKind::Underneath => "Underneath",
                });
            }
        }
        assert!(seen > 0, "the sweep found no caves — it asserts nothing");
        assert!(
            bands.len() > 1,
            "every cave landed in the same band ({bands:?}) — the invariant \
             holds trivially and this test would not notice a constant"
        );
    }

    #[test]
    fn cave_at_agrees_with_the_kind_first_gate() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in geo.cells() {
            let expected = if terrain.is_ocean(cell) {
                None
            } else {
                crate::features::cave_process(
                    &terrain.material_at(cell),
                    terrain.drainage_at(cell),
                    terrain.crust_age_at(cell),
                    terrain.nearest_boundary_at(cell),
                )
                .and_then(|(kind, proneness)| {
                    let belt = crate::features::belt_weight(terrain.boundary_distance_at(cell));
                    let prob = crate::features::presence_prob(proneness, belt);
                    let noise = crate::features::uniformize(crate::crust::sphere_fbm01(
                        terrain.globe().features_noise_seed(),
                        geo.position(cell),
                        crate::features::CAVE_GATE_FREQ,
                        crate::features::CAVE_GATE_OCTAVES,
                    ));
                    (noise < prob).then(|| {
                        crate::features::Cave::new(
                            kind,
                            &terrain.material_at(cell),
                            &terrain.column_at(cell),
                        )
                    })
                })
            };
            assert_eq!(terrain.cave_at(cell), expected, "cell {cell:?} disagrees");
        }
    }

    #[test]
    fn prehuman_scar_at_matches_the_ancient_crust_and_noise_gate() {
        let geo = Geosphere::new(3);
        let outcome = generate(Seed(42), &geo, &TerrainPins::default()).unwrap();
        let terrain = GeneratedTerrain::new(geo.clone(), outcome);
        for cell in geo.cells() {
            let expected = !terrain.is_ocean(cell)
                && terrain.crust_age_at(cell) > ANCIENT_CRUST_AGE
                && crate::crust::sphere_fbm01(
                    terrain.globe().features_noise_seed(),
                    geo.position(cell),
                    PREHUMAN_SCAR_FREQ,
                    PREHUMAN_SCAR_OCTAVES,
                ) < PREHUMAN_SCAR_THRESHOLD;
            assert_eq!(
                terrain.prehuman_scar_at(cell),
                expected,
                "cell {cell:?} disagrees with the direct ancient-crust + noise gate"
            );
        }
    }

    #[test]
    fn promote_to_spring_only_touches_aquifer_with_a_lower_non_aquifer_neighbor() {
        use crate::lithology::Hydro;
        let hi = ReferenceElevation::new(100.0).unwrap();
        let lo = ReferenceElevation::new(50.0).unwrap();

        // Not an Aquifer to begin with -> passed through unchanged,
        // regardless of the neighborhood.
        assert_eq!(
            promote_to_spring(Hydro::Karst, hi, [(Hydro::Runoff, lo)].into_iter()),
            Hydro::Karst
        );

        // Aquifer with no neighbors at all -> stays Aquifer (still water).
        assert_eq!(
            promote_to_spring(Hydro::Aquifer, hi, std::iter::empty()),
            Hydro::Aquifer
        );

        // Aquifer surrounded only by higher or equal non-aquifer neighbors
        // -> no descending contact, stays Aquifer.
        assert_eq!(
            promote_to_spring(
                Hydro::Aquifer,
                hi,
                [(Hydro::Runoff, hi), (Hydro::Aquitard, hi)].into_iter()
            ),
            Hydro::Aquifer
        );

        // Aquifer with a LOWER Aquifer neighbor only -> not a contact
        // (both sides are the same rock type), stays Aquifer.
        assert_eq!(
            promote_to_spring(Hydro::Aquifer, hi, [(Hydro::Aquifer, lo)].into_iter()),
            Hydro::Aquifer
        );

        // Aquifer with a lower NON-aquifer neighbor -> the descending
        // contact a spring geologically is.
        assert_eq!(
            promote_to_spring(Hydro::Aquifer, hi, [(Hydro::Runoff, lo)].into_iter()),
            Hydro::Spring
        );
    }
}
