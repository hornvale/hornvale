//! Deterministic composition of addressed terrain and climate surface patches.

use std::fmt::{self, Write as _};

use hornvale_kernel::{Facet, NearestVertexIndex, Seed, Vertex, World};
use hornvale_terrain::{
    FacetAddress, FacetFieldSample, FeatureEndpoint, FeatureId, FeatureStripSampling,
    RealizedCurve, TerrainFacetInputs, WaterKind, canonical_corner_sample, canonical_edge_sample,
    realize_channel_curves, realize_channel_strips,
};

use crate::{GeneratedClimate, climate_from, sky_of, terrain_of};

/// Revision of the macro world and the surface-realization algorithm applied to it.
/// type-audit: bare-ok(identifier-text: source_revision), bare-ok(identifier-text: algorithm_version), bare-ok(artifact: configuration_hash)
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SurfaceRevision {
    /// Deterministic digest of the canonical source-world bytes.
    pub source_revision: String,
    /// Version of the realization algorithm and its channel meanings.
    pub algorithm_version: &'static str,
    /// Digest of the canonical algorithm/configuration record and source bytes.
    pub configuration_hash: [u8; 32],
}

/// One fully composed, addressed surface patch.
/// type-audit: bare-ok(index: triangles), bare-ok(index: transition_triangles)
#[derive(Clone, Debug, PartialEq)]
pub struct SurfacePatch {
    /// Macro-world and realization revision that produced the patch.
    pub revision: SurfaceRevision,
    /// Canonical Level-6 macro facet and any refinement below it.
    pub address: FacetAddress,
    /// Corners 0..4, center 4, then the four dyadic edge splits 5..9.
    /// The splits form a derived border ring used by mixed-LOD topology.
    pub samples: Vec<FacetFieldSample>,
    /// Stable inherited terrain curves clipped to this patch.
    pub curves: Vec<RealizedCurve>,
    /// Adaptive source-owned ribbons independent of the terrain sample grid.
    pub strips: Vec<SurfaceFeatureStrip>,
    /// Sample indices forming the patch mesh.
    pub triangles: Vec<[u32; 3]>,
    /// Source-computed replacement topology for one unequal-LOD boundary.
    /// Indices address this patch's samples; an empty list means no seam was
    /// requested for this realization.
    pub transition_triangles: Vec<[u32; 3]>,
}

/// One source-evaluated edge vertex of an adaptive feature ribbon.
/// type-audit: pending(wave-1)
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SurfaceFeatureStripVertex {
    /// Canonical unit-sphere ribbon-edge position.
    pub position: [f64; 3],
    /// Source-owned patch height carried onto the narrow ribbon.
    pub height_m: f64,
    /// Source-owned patch normal carried onto the narrow ribbon.
    pub normal: [f64; 3],
    /// `-1` on the right and `1` on the left, facing feature travel.
    pub side: i8,
    /// Signed angular distance from the feature centerline.
    pub signed_distance_rad: f64,
}

/// One canonical patch-local feature ribbon ready for derived protocols.
/// type-audit: pending(wave-1)
#[derive(Clone, Debug, PartialEq)]
pub struct SurfaceFeatureStrip {
    /// Stable feature identity, unchanged by patch refinement.
    pub feature: FeatureId,
    /// Adaptive unit-sphere centerline positions in feature travel order.
    pub centerline: Vec<[f64; 3]>,
    /// Full angular width at each centerline position.
    pub width_rad: Vec<f64>,
    /// Paired source-evaluated right/left vertices.
    pub vertices: Vec<SurfaceFeatureStripVertex>,
    /// Canonical triangle-list topology over `vertices`.
    pub triangles: Vec<[u32; 3]>,
    /// Source-owned terrain material-channel influence.
    pub semantic_mask: [f32; 8],
    /// Original terminal and canonical continuation metadata.
    pub endpoints: [FeatureEndpoint; 2],
}

/// Why a surface context or patch could not be constructed.
/// type-audit: bare-ok(prose: InvalidAddress.0), bare-ok(prose: RevisionMismatch.0), bare-ok(prose: MissingMacroContext.0), bare-ok(prose: Numeric.0)
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SurfaceBuildError {
    /// The supplied facet address is not a valid Level-6-rooted address.
    InvalidAddress(String),
    /// The context's public revision no longer names its retained macro state.
    RevisionMismatch(String),
    /// A required terrain, climate, or astronomy context could not be reconstructed.
    MissingMacroContext(String),
    /// A composed value was non-finite or otherwise numerically invalid.
    Numeric(String),
}

impl fmt::Display for SurfaceBuildError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidAddress(reason) => write!(formatter, "invalid surface address: {reason}"),
            Self::RevisionMismatch(reason) => {
                write!(formatter, "surface revision mismatch: {reason}")
            }
            Self::MissingMacroContext(reason) => {
                write!(formatter, "missing surface macro context: {reason}")
            }
            Self::Numeric(reason) => write!(formatter, "invalid surface numeric value: {reason}"),
        }
    }
}

impl std::error::Error for SurfaceBuildError {}

/// A retained, deterministic terrain/climate composition for surface requests.
pub struct SurfaceRealizationContext {
    /// Revision exposed to callers and copied into every realized patch.
    pub revision: SurfaceRevision,
    expected_revision: SurfaceRevision,
    terrain: hornvale_terrain::GeneratedTerrain,
    climate: GeneratedClimate,
    nearest: NearestVertexIndex,
    planet_radius_m: f64,
    relief_noise: hornvale_terrain::SphereFbm,
    configuration: SurfaceConfiguration,
    rill_candidates: RillCandidates,
}

/// Every authored calibration consumed by surface realization, held once so
/// the canonical revision record and the realization path cannot drift apart.
#[derive(Clone, Copy, Debug, PartialEq)]
struct SurfaceConfiguration {
    record_schema: &'static str,
    algorithm_version: &'static str,
    globe_level: u32,
    sample_topology: &'static str,
    material_channels: &'static str,
    flow_half_saturation: f64,
    slope_half_saturation_m_per_rad: f64,
    channel_width_multiplier: f64,
    relief_frequency: f64,
    relief_octaves: u32,
    relief_max_amplitude_m: f64,
    relief_slope_share: f64,
    relief_elevation_half_saturation_m: f64,
    relief_coast_taper_m: f64,
    channel_incision_m: f64,
    relief_elevation_base: f64,
    relief_coast_minimum: f64,
    bank_height_m: f64,
    floodplain_height_m: f64,
    hydrology_max_adjustment_m: f64,
    delta_height_m: f64,
    ridge_share: f64,
    ridge_smoothing_rad: f64,
    material_altitude_scale_m: f64,
    material_shore_scale_m: f64,
    material_sediment_half_saturation_m: f64,
    material_drainage_half_saturation: f64,
    material_warmth_floor_c: f64,
    material_warmth_span_c: f64,
    material_cold_ceiling_c: f64,
    material_cold_span_c: f64,
    material_fresh_drainage_share: f64,
    material_bedrock_base: f64,
    material_soil_base: f64,
    material_soil_sediment_suppression: f64,
    material_sediment_drainage_share: f64,
    material_sediment_shore_share: f64,
    material_snow_base: f64,
    material_snow_ocean_share: f64,
    strip_patch_divisions: u32,
    strip_width_step_multiplier: f64,
    strip_curvature_gain: f64,
    strip_max_subdivisions: u32,
}

impl SurfaceConfiguration {
    const fn current() -> Self {
        Self {
            record_schema: "hornvale/surface-configuration/v4",
            algorithm_version: "hornvale/surface-realization/v7",
            globe_level: hornvale_terrain::GLOBE_LEVEL,
            sample_topology: "quad-corners-center-dyadic-ring/world-space-fan-v2",
            material_channels: "bedrock,soil,sediment,wetland,fresh-water,salt-water,snow-ice,vegetation/v1",
            flow_half_saturation: 24.0,
            slope_half_saturation_m_per_rad: 80_000.0,
            channel_width_multiplier: 2.0,
            relief_frequency: 192.0,
            relief_octaves: 4,
            relief_max_amplitude_m: 160.0,
            relief_slope_share: 0.65,
            relief_elevation_half_saturation_m: 1_200.0,
            relief_coast_taper_m: 30_000.0,
            channel_incision_m: 12.0,
            relief_elevation_base: 0.35,
            relief_coast_minimum: 0.15,
            bank_height_m: 4.0,
            floodplain_height_m: 6.0,
            hydrology_max_adjustment_m: 2_000.0,
            delta_height_m: 3.0,
            ridge_share: 0.35,
            ridge_smoothing_rad: 0.008,
            material_altitude_scale_m: 4_000.0,
            material_shore_scale_m: 20_000.0,
            material_sediment_half_saturation_m: 8.0,
            material_drainage_half_saturation: 24.0,
            material_warmth_floor_c: -10.0,
            material_warmth_span_c: 35.0,
            material_cold_ceiling_c: 5.0,
            material_cold_span_c: 30.0,
            material_fresh_drainage_share: 0.35,
            material_bedrock_base: 0.2,
            material_soil_base: 0.2,
            material_soil_sediment_suppression: 0.5,
            material_sediment_drainage_share: 0.5,
            material_sediment_shore_share: 0.5,
            material_snow_base: 0.25,
            material_snow_ocean_share: 0.5,
            strip_patch_divisions: 8,
            strip_width_step_multiplier: 8.0,
            strip_curvature_gain: 1.0,
            strip_max_subdivisions: 32,
        }
    }
}

impl SurfaceRealizationContext {
    /// Reconstruct and retain the macro terrain and climate used by every patch request.
    // Named construction site (decision 0092): this context exists to build
    // and retain one terrain/climate pair for all subsequent patch reads.
    #[allow(clippy::disallowed_methods)]
    pub fn build(world: &World) -> Result<SurfaceRealizationContext, SurfaceBuildError> {
        let terrain = terrain_of(world).map_err(|error| {
            SurfaceBuildError::MissingMacroContext(format!(
                "terrain reconstruction failed: {error}"
            ))
        })?;
        let climate = climate_from(world, &terrain).map_err(|error| {
            SurfaceBuildError::MissingMacroContext(format!(
                "climate reconstruction failed: {error}"
            ))
        })?;
        Self::from_parts(world, terrain, climate)
    }

    /// Reconstruct a context whose revision uses the source's supplied
    /// binding revision rather than deriving a second identity from world
    /// bytes. This is the source boundary's entry point.
    /// type-audit: bare-ok(identifier-text: source_revision)
    #[allow(clippy::disallowed_methods)]
    pub fn build_with_source_revision(
        world: &World,
        source_revision: &str,
    ) -> Result<SurfaceRealizationContext, SurfaceBuildError> {
        let terrain = terrain_of(world).map_err(|error| {
            SurfaceBuildError::MissingMacroContext(format!(
                "terrain reconstruction failed: {error}"
            ))
        })?;
        let climate = climate_from(world, &terrain).map_err(|error| {
            SurfaceBuildError::MissingMacroContext(format!(
                "climate reconstruction failed: {error}"
            ))
        })?;
        Self::from_parts_with_source_revision(world, terrain, climate, source_revision)
    }

    /// Compose a context from macro terrain and climate already derived by a
    /// caller, avoiding a second terrain/climate derivation.
    pub fn from_parts(
        world: &World,
        terrain: hornvale_terrain::GeneratedTerrain,
        climate: GeneratedClimate,
    ) -> Result<SurfaceRealizationContext, SurfaceBuildError> {
        let source_revision = hexadecimal(&stable_digest(&world.to_json()));
        Self::from_parts_with_source_revision(world, terrain, climate, &source_revision)
    }

    /// Compose a context from caller-owned macro values and the source
    /// binding revision that names them.
    /// type-audit: bare-ok(identifier-text: source_revision)
    pub fn from_parts_with_source_revision(
        world: &World,
        terrain: hornvale_terrain::GeneratedTerrain,
        climate: GeneratedClimate,
        source_revision: &str,
    ) -> Result<SurfaceRealizationContext, SurfaceBuildError> {
        let configuration = SurfaceConfiguration::current();
        let world_bytes = world.to_json();
        let revision = surface_revision(&world_bytes, source_revision, configuration);

        if terrain.geosphere().depth() != configuration.globe_level {
            return Err(SurfaceBuildError::MissingMacroContext(format!(
                "terrain mesh depth {} is not the Level-{} surface authority",
                terrain.geosphere().depth(),
                configuration.globe_level
            )));
        }
        if climate.geosphere().depth() != terrain.geosphere().depth()
            || climate.geosphere().vertex_count() != terrain.geosphere().vertex_count()
        {
            return Err(SurfaceBuildError::MissingMacroContext(
                "terrain and climate do not share one macro vertex space".to_string(),
            ));
        }
        let nearest = NearestVertexIndex::new(terrain.geosphere());

        let sky = sky_of(world).map_err(|error| {
            SurfaceBuildError::MissingMacroContext(format!(
                "astronomy reconstruction failed: {error}"
            ))
        })?;
        let planet_radius_m = hornvale_astronomy::anchor_radius(sky.system().anchor.mass)
            .map_err(|error| SurfaceBuildError::Numeric(error.to_string()))?
            .get()
            * 1_000_000.0;
        if !planet_radius_m.is_finite() || planet_radius_m <= 0.0 {
            return Err(SurfaceBuildError::Numeric(format!(
                "planet radius must be finite and positive, got {planet_radius_m}"
            )));
        }

        let relief_noise = hornvale_terrain::SphereFbm::new(
            terrain.globe().lithology_noise_seed(),
            configuration.relief_frequency,
            configuration.relief_octaves,
        );

        let rill_candidates = rill_candidates(&terrain);
        Ok(Self {
            revision: revision.clone(),
            expected_revision: revision,
            terrain,
            climate,
            nearest,
            planet_radius_m,
            relief_noise,
            configuration,
            rill_candidates,
        })
    }

    /// Compute the active revision without deriving terrain, climate, or
    /// astronomy. Request boundaries use this to reject stale work before a
    /// full surface context is built.
    /// type-audit: bare-ok(identifier-text: source_revision)
    pub fn revision_for(world: &World, source_revision: &str) -> SurfaceRevision {
        surface_revision(
            &world.to_json(),
            source_revision,
            SurfaceConfiguration::current(),
        )
    }

    /// Realize one patch solely from retained macro state and its validated address.
    pub fn realize(&self, address: &FacetAddress) -> Result<SurfacePatch, SurfaceBuildError> {
        if self.revision != self.expected_revision {
            return Err(SurfaceBuildError::RevisionMismatch(format!(
                "context exposes {:?}, but retained macro state is {:?}",
                self.revision, self.expected_revision
            )));
        }
        FacetAddress::new(address.macro_face.clone(), address.child_path.clone()).map_err(
            |error| {
                SurfaceBuildError::InvalidAddress(format!(
                    "{:?}/{:?}: {error:?}",
                    address.macro_face, address.child_path
                ))
            },
        )?;

        let samples = patch_positions(address)
            .into_iter()
            .map(|position| self.sample_at(position))
            .collect::<Result<Vec<_>, _>>()?;

        let curves = realize_channel_curves(
            TerrainFacetInputs {
                globe: self.terrain.globe(),
                geo: self.terrain.geosphere(),
                channels: self.terrain.channels(),
            },
            address,
        );
        let strips = realize_channel_strips(
            TerrainFacetInputs {
                globe: self.terrain.globe(),
                geo: self.terrain.geosphere(),
                channels: self.terrain.channels(),
            },
            address,
            FeatureStripSampling {
                patch_divisions: self.configuration.strip_patch_divisions,
                width_step_multiplier: self.configuration.strip_width_step_multiplier,
                curvature_gain: self.configuration.strip_curvature_gain,
                max_subdivisions: self.configuration.strip_max_subdivisions,
            },
        )
        .into_iter()
        .map(|layout| {
            let vertices = layout
                .vertices
                .iter()
                .map(|vertex| {
                    let surface = self.sample_at(vertex.position)?;
                    Ok(SurfaceFeatureStripVertex {
                        position: vertex.position,
                        height_m: surface.height_m,
                        normal: surface.normal,
                        side: vertex.side,
                        signed_distance_rad: vertex.signed_distance_rad,
                    })
                })
                .collect::<Result<Vec<_>, SurfaceBuildError>>()?;
            Ok(SurfaceFeatureStrip {
                feature: layout.feature,
                centerline: layout.centerline,
                width_rad: layout.width_rad,
                vertices,
                triangles: layout.triangles,
                semantic_mask: layout.semantic_mask,
                endpoints: layout.endpoints,
            })
        })
        .collect::<Result<Vec<_>, SurfaceBuildError>>()?;

        Ok(SurfacePatch {
            revision: self.revision.clone(),
            address: address.clone(),
            samples,
            curves,
            strips,
            triangles: vec![[0, 1, 4], [1, 2, 4], [2, 3, 4], [3, 0, 4]],
            transition_triangles: Vec::new(),
        })
    }

    /// Choose interpolation inputs from the position alone, including at a
    /// seam or corner. Neither the requesting patch nor its LOD enters this
    /// calculation. The cube inverse includes the same warp as the forward
    /// projection; chord midpoints must not be treated as dyadic midpoints.
    fn sample_at(&self, position: [f64; 3]) -> Result<FacetFieldSample, SurfaceBuildError> {
        let macro_face = Facet::containing(position, self.configuration.globe_level);
        let lattice = macro_face.face_lattice();
        let (_, a, b) = hornvale_kernel::locate(position);
        let u = ((a + 1.0) * lattice.scale as f64 / 2.0 - lattice.x as f64).clamp(0.0, 1.0);
        let v = ((b + 1.0) * lattice.scale as f64 / 2.0 - lattice.y as f64).clamp(0.0, 1.0);
        let vertices = macro_face.corners().map(|position| {
            self.nearest
                .nearest_to_position(self.terrain.geosphere(), position)
        });
        self.compose_sample(
            position,
            vertices,
            [(1.0 - u) * (1.0 - v), u * (1.0 - v), u * v, (1.0 - u) * v],
        )
    }

    fn compose_sample(
        &self,
        position: [f64; 3],
        vertices: [Vertex; 4],
        weights: [f64; 4],
    ) -> Result<FacetFieldSample, SurfaceBuildError> {
        let macro_height_m = blend(vertices, weights, |vertex| {
            self.terrain.elevation_at(vertex).get()
        });
        let sea_level_m = self.terrain.sea_level().get();
        let macro_height_asl_m = macro_height_m - sea_level_m;
        let temperature_c = blend(vertices, weights, |vertex| {
            self.climate.mean_temperature_at(vertex).get()
        });
        let moisture = blend(vertices, weights, |vertex| self.climate.moisture_at(vertex));
        let slope_m_per_rad = blend(vertices, weights, |vertex| {
            hornvale_terrain::local_slope(self.terrain.globe(), self.terrain.geosphere(), vertex)
        })
        .max(0.0);
        let sediment_m = blend(vertices, weights, |vertex| {
            self.terrain.sediment_thickness_at(vertex)
        });
        let drainage = blend(vertices, weights, |vertex| self.terrain.drainage_at(vertex));
        let induration = blend(vertices, weights, |vertex| {
            self.terrain.induration_at(vertex)
        });
        let ocean = category_fraction(vertices, weights, |vertex| {
            self.terrain.water_kind_at(vertex) == WaterKind::Ocean
        });
        let salt = category_fraction(vertices, weights, |vertex| {
            self.terrain.water_kind_at(vertex) == WaterKind::SaltBasin
        });
        let river = category_fraction(vertices, weights, |vertex| {
            self.terrain.water_kind_at(vertex) == WaterKind::River
        });
        let delta = category_fraction(vertices, weights, |vertex| {
            self.terrain.globe().delta_vertices.contains(&vertex)
        });

        let flow_direction = normalized_or_zero(blend_vector(vertices, weights, |vertex| {
            downhill_direction(&self.terrain, vertex)
        }));
        let flow_strength = ratio(drainage, self.configuration.flow_half_saturation);
        let slope_strength = ratio(
            slope_m_per_rad,
            self.configuration.slope_half_saturation_m_per_rad,
        );
        let normal = normalized_or_zero([
            position[0] + flow_direction[0] * slope_m_per_rad / self.planet_radius_m,
            position[1] + flow_direction[1] * slope_m_per_rad / self.planet_radius_m,
            position[2] + flow_direction[2] * slope_m_per_rad / self.planet_radius_m,
        ]);
        let max_surface_distance = std::f64::consts::PI * self.planet_radius_m;
        let shoreline_distance_m = if slope_m_per_rad > f64::EPSILON {
            (macro_height_asl_m * self.planet_radius_m / slope_m_per_rad)
                .clamp(-max_surface_distance, max_surface_distance)
        } else if macro_height_asl_m < 0.0 {
            -max_surface_distance
        } else {
            max_surface_distance
        };
        let relief_amplitude_m = relief_amplitude_m(
            slope_strength,
            macro_height_asl_m,
            shoreline_distance_m,
            self.configuration,
        );
        let ridge_direction = normalized_or_zero(cross(position, flow_direction));
        let ridge_strength = slope_strength * (1.0 - flow_strength);
        // Average the same random-access field along the inherited ridge axis.
        // Reversing the axis is identical; variation across it is retained.
        let ridge_noise = ridge_noise(
            &self.relief_noise,
            position,
            ridge_direction,
            self.configuration.ridge_smoothing_rad,
        );
        let ridge_share = self.configuration.ridge_share * ridge_strength;
        let relief_m = relief_amplitude_m
            * ((1.0 - ridge_share) * (2.0 * self.relief_noise.sample(position) - 1.0)
                + ridge_share * ridge_noise);
        let channel_reading = self.terrain.channels().bank_reading(position);
        let channel_bed = channel_reading
            .map(|reading| channel_bed_height(&self.terrain, reading.line, position));
        let trunk = channel_reading
            .zip(channel_bed)
            .map(|(reading, bed)| (reading.signed_distance, bed));
        let channel =
            rill_bed_from_candidates(&self.terrain, &self.rill_candidates, position, trunk)
                .or(trunk);
        let ambient_height = macro_height_m + relief_m;
        let height_m = match channel {
            Some((distance, bed)) => {
                channel_height_m(ambient_height, distance.abs(), bed, self.configuration)
            }
            _ => ambient_height,
        };
        let height_asl_m = height_m - sea_level_m;
        let water_depth_m = (-height_asl_m).max(0.0);

        let (channel_distance_m, channel_width_m, bank_weight, floodplain_weight, terrace_weight) =
            match channel {
                Some((distance, bed)) => {
                    let distance = distance * self.planet_radius_m;
                    let edges = bed.edges.map(|edge| edge * self.planet_radius_m);
                    (
                        distance,
                        self.configuration.channel_width_multiplier * edges[0],
                        annulus_weight(distance.abs(), edges[0], edges[1]),
                        annulus_weight(distance.abs(), edges[1], edges[2]),
                        annulus_weight(distance.abs(), edges[2], edges[3]),
                    )
                }
                None => (max_surface_distance, 0.0, 0.0, 0.0, 0.0),
            };

        let material_weights = material_weights(
            MaterialInputs {
                temperature_c,
                moisture,
                height_asl_m,
                slope_strength,
                shoreline_distance_m,
                ocean,
                salt,
                river,
                sediment_m,
                drainage,
                induration,
            },
            self.configuration,
        );
        let sample = FacetFieldSample {
            position,
            height_m,
            normal,
            material_weights,
            shoreline_distance_m,
            water_depth_m,
            flow_direction,
            flow_strength,
            channel_distance_m,
            channel_width_m,
            floodplain_weight,
            bank_weight,
            terrace_weight,
            delta_weight: (delta * (1.0 - slope_strength)).max(channel_bed.map_or(0.0, |bed| {
                bed.terminal_progress * f64::from(floodplain_weight)
            })) as f32,
            ridge_direction,
            ridge_strength: ridge_strength as f32,
        };
        validate_sample(&sample)?;
        Ok(sample)
    }
}

/// Replace the coarse fan at a one-level-finer neighbor with a split fan.
///
/// Returned indices address `coarse.samples` only and REPLACE its triangle
/// list. Fine triangles remain unchanged. Either fine half returns the same
/// replacement; installing it before stitching another edge preserves earlier
/// splits. The border split is a child corner, not a chord midpoint. Curves,
/// feature IDs and sample values are never edited.
/// type-audit: bare-ok(index: return)
pub fn stitch_transition(
    coarse: &SurfacePatch,
    fine: &SurfacePatch,
) -> Result<Vec<[u32; 3]>, SurfaceBuildError> {
    if coarse.revision != fine.revision {
        return Err(SurfaceBuildError::RevisionMismatch(
            "transition patches name different surfaces".into(),
        ));
    }
    validate_patch(coarse)?;
    validate_patch(fine)?;
    let coarse_facet = resolved_facet(&coarse.address);
    let mut fine_parent = resolved_facet(&fine.address);
    if fine_parent.depth() != coarse_facet.depth() + 1 {
        return Err(SurfaceBuildError::InvalidAddress(
            "transition requires fine depth = coarse depth + 1".into(),
        ));
    }
    fine_parent.path.pop();
    if !coarse_facet.neighbors().contains(&fine_parent) {
        return Err(SurfaceBuildError::InvalidAddress(
            "fine patch is not in an adjacent facet".into(),
        ));
    }
    let edge = (0..4)
        .find(|&edge| {
            let midpoint = coarse.samples[5 + edge].position;
            let ends = [
                coarse.samples[edge].position,
                coarse.samples[(edge + 1) % 4].position,
            ];
            (0..4).any(|i| {
                let fine_ends = [fine.samples[i].position, fine.samples[(i + 1) % 4].position];
                fine_ends.contains(&midpoint) && ends.iter().any(|p| fine_ends.contains(p))
            })
        })
        .ok_or_else(|| {
            SurfaceBuildError::InvalidAddress("patches do not share a half edge".into())
        })?;
    let start = edge as u32;
    let end = ((edge + 1) % 4) as u32;
    let midpoint = (5 + edge) as u32;
    let replacements = [[start, midpoint, 4], [midpoint, end, 4]];
    let mut triangles = coarse.triangles.clone();
    if let Some(index) = triangles.iter().position(|t| *t == [start, end, 4]) {
        triangles.splice(index..=index, replacements);
    } else if !replacements.iter().all(|t| triangles.contains(t)) {
        return Err(SurfaceBuildError::InvalidAddress(
            "coarse edge is not a surface fan".into(),
        ));
    }
    Ok(triangles)
}

/// Restrict four immediate children to their parent's canonical sample sites.
///
/// Child samples coinciding at a parent site are averaged in address order,
/// including all material and directional fields. Parent topology and semantic
/// curves are retained because clipping is a sampling operation, not a new
/// feature. For realized children this preserves unit vectors to 1e-12,
/// metre-valued fields to 1e-7 m, and f32 weights to 1e-6.
///
/// # Panics
/// Requires four distinct immediate children from the parent's revision, with
/// valid complete sample rings. A partial/stale set cannot define restriction.
pub fn aggregate_children(parent: &SurfacePatch, children: &[SurfacePatch]) -> SurfacePatch {
    validate_patch(parent).expect("valid parent surface patch");
    let mut ordered: Vec<_> = children.iter().collect();
    ordered.sort_by(|a, b| a.address.cmp(&b.address));
    assert!(
        ordered.len() == 4
            && ordered.iter().enumerate().all(|(digit, child)| {
                child.address.macro_face == parent.address.macro_face
                    && child.address.child_path.len() == parent.address.child_path.len() + 1
                    && child
                        .address
                        .child_path
                        .starts_with(&parent.address.child_path)
                    && child.address.child_path.last() == Some(&(digit as u8))
            }),
        "aggregation requires four distinct immediate children"
    );
    for child in &ordered {
        assert_eq!(
            child.revision, parent.revision,
            "aggregation requires one surface revision"
        );
        validate_patch(child).expect("valid child surface patch");
    }
    let mut aggregate = parent.clone();
    for sample in &mut aggregate.samples {
        let matches: Vec<_> = ordered
            .iter()
            .flat_map(|p| &p.samples)
            .filter(|s| s.position == sample.position)
            .collect();
        assert!(
            !matches.is_empty(),
            "children must cover every parent sample"
        );
        *sample = average_samples(&matches, sample.position);
    }
    aggregate
}

fn average_samples(samples: &[&FacetFieldSample], position: [f64; 3]) -> FacetFieldSample {
    let mean = |field: fn(&FacetFieldSample) -> f64| {
        samples.iter().map(|s| field(s)).sum::<f64>() / samples.len() as f64
    };
    let vector = |field: fn(&FacetFieldSample) -> [f64; 3]| {
        std::array::from_fn(|axis| {
            samples.iter().map(|s| field(s)[axis]).sum::<f64>() / samples.len() as f64
        })
    };
    FacetFieldSample {
        position,
        height_m: mean(|s| s.height_m),
        normal: vector(|s| s.normal),
        material_weights: std::array::from_fn(|i| {
            (samples
                .iter()
                .map(|s| f64::from(s.material_weights[i]))
                .sum::<f64>()
                / samples.len() as f64) as f32
        }),
        shoreline_distance_m: mean(|s| s.shoreline_distance_m),
        water_depth_m: mean(|s| s.water_depth_m),
        flow_direction: vector(|s| s.flow_direction),
        flow_strength: mean(|s| s.flow_strength),
        channel_distance_m: mean(|s| s.channel_distance_m),
        channel_width_m: mean(|s| s.channel_width_m),
        floodplain_weight: mean(|s| f64::from(s.floodplain_weight)) as f32,
        bank_weight: mean(|s| f64::from(s.bank_weight)) as f32,
        terrace_weight: mean(|s| f64::from(s.terrace_weight)) as f32,
        delta_weight: mean(|s| f64::from(s.delta_weight)) as f32,
        ridge_direction: vector(|s| s.ridge_direction),
        ridge_strength: mean(|s| f64::from(s.ridge_strength)) as f32,
    }
}

fn patch_positions(address: &FacetAddress) -> [[f64; 3]; 9] {
    let resolved = resolved_facet(address);
    let lattice = resolved.face_lattice();
    let x = 2 * lattice.x;
    let y = 2 * lattice.y;
    let scale = 2 * lattice.scale;
    let splits = [(x + 1, y), (x + 2, y + 1), (x + 1, y + 2), (x, y + 1)];
    std::array::from_fn(|i| match i {
        0..4 => canonical_corner_sample(address, i as u8),
        4 => resolved.centroid(),
        _ => {
            let (u, v) = splits[i - 5];
            // This also works at the maximum address depth: the derived ring
            // needs a dyadic coordinate, not an address beyond the depth cap.
            hornvale_kernel::face_unit(
                resolved.face as usize,
                (2 * u - scale) as f64 / scale as f64,
                (2 * v - scale) as f64 / scale as f64,
            )
        }
    })
}

fn validate_patch(patch: &SurfacePatch) -> Result<(), SurfaceBuildError> {
    FacetAddress::new(
        patch.address.macro_face.clone(),
        patch.address.child_path.clone(),
    )
    .map_err(|error| SurfaceBuildError::InvalidAddress(format!("{error:?}")))?;
    let positions = patch_positions(&patch.address);
    if patch.samples.len() != positions.len()
        || patch
            .samples
            .iter()
            .zip(positions)
            .any(|(s, p)| s.position != p)
        || patch
            .triangles
            .iter()
            .flatten()
            .any(|&i| i as usize >= positions.len())
    {
        return Err(SurfaceBuildError::InvalidAddress(
            "patch lacks its canonical samples or valid triangle indices".into(),
        ));
    }
    for sample in &patch.samples {
        validate_sample(sample)?;
    }
    // Check the corner oracle against the canonical edge endpoint convention.
    // Interior split coordinates deliberately remain dyadic, not chord t=0.5.
    for edge in 0..4 {
        let endpoints = [
            canonical_edge_sample(&patch.address, edge, 0.0),
            canonical_edge_sample(&patch.address, edge, 1.0),
        ];
        if !endpoints.contains(&patch.samples[edge as usize].position) {
            return Err(SurfaceBuildError::InvalidAddress(
                "corner and edge oracles disagree".into(),
            ));
        }
    }
    Ok(())
}

fn surface_revision(
    world_bytes: &str,
    source_revision: &str,
    configuration: SurfaceConfiguration,
) -> SurfaceRevision {
    let configuration_record =
        canonical_configuration_record(world_bytes, source_revision, configuration);
    SurfaceRevision {
        source_revision: source_revision.into(),
        algorithm_version: configuration.algorithm_version,
        configuration_hash: stable_digest(&configuration_record),
    }
}

/// Serialize one complete realization configuration in a fixed field order.
/// Text fields are length-framed and floating-point values use their exact
/// IEEE-754 bits, so locale and display-format changes cannot move identity.
fn canonical_configuration_record(
    world_bytes: &str,
    source_revision: &str,
    configuration: SurfaceConfiguration,
) -> String {
    let mut record = String::new();
    push_text_field(&mut record, "record-schema", configuration.record_schema);
    push_text_field(
        &mut record,
        "algorithm-version",
        configuration.algorithm_version,
    );
    push_u32_field(&mut record, "globe-level", configuration.globe_level);
    push_text_field(
        &mut record,
        "sample-topology",
        configuration.sample_topology,
    );
    push_text_field(
        &mut record,
        "material-channels",
        configuration.material_channels,
    );
    push_f64_field(
        &mut record,
        "flow-half-saturation",
        configuration.flow_half_saturation,
    );
    push_f64_field(
        &mut record,
        "slope-half-saturation-m-per-rad",
        configuration.slope_half_saturation_m_per_rad,
    );
    push_f64_field(
        &mut record,
        "channel-width-multiplier",
        configuration.channel_width_multiplier,
    );
    push_f64_field(
        &mut record,
        "relief-frequency",
        configuration.relief_frequency,
    );
    push_u32_field(&mut record, "relief-octaves", configuration.relief_octaves);
    push_f64_field(
        &mut record,
        "relief-max-amplitude-m",
        configuration.relief_max_amplitude_m,
    );
    push_f64_field(
        &mut record,
        "relief-slope-share",
        configuration.relief_slope_share,
    );
    push_f64_field(
        &mut record,
        "relief-elevation-half-saturation-m",
        configuration.relief_elevation_half_saturation_m,
    );
    push_f64_field(
        &mut record,
        "relief-coast-taper-m",
        configuration.relief_coast_taper_m,
    );
    push_f64_field(
        &mut record,
        "channel-incision-m",
        configuration.channel_incision_m,
    );
    for (name, value) in [
        ("relief-elevation-base", configuration.relief_elevation_base),
        ("relief-coast-minimum", configuration.relief_coast_minimum),
        ("bank-height-m", configuration.bank_height_m),
        ("floodplain-height-m", configuration.floodplain_height_m),
        (
            "hydrology-max-adjustment-m",
            configuration.hydrology_max_adjustment_m,
        ),
        ("delta-height-m", configuration.delta_height_m),
        ("ridge-share", configuration.ridge_share),
        ("ridge-smoothing-rad", configuration.ridge_smoothing_rad),
    ] {
        push_f64_field(&mut record, name, value);
    }
    push_f64_field(
        &mut record,
        "material-altitude-scale-m",
        configuration.material_altitude_scale_m,
    );
    push_f64_field(
        &mut record,
        "material-shore-scale-m",
        configuration.material_shore_scale_m,
    );
    push_f64_field(
        &mut record,
        "material-sediment-half-saturation-m",
        configuration.material_sediment_half_saturation_m,
    );
    push_f64_field(
        &mut record,
        "material-drainage-half-saturation",
        configuration.material_drainage_half_saturation,
    );
    push_f64_field(
        &mut record,
        "material-warmth-floor-c",
        configuration.material_warmth_floor_c,
    );
    push_f64_field(
        &mut record,
        "material-warmth-span-c",
        configuration.material_warmth_span_c,
    );
    push_f64_field(
        &mut record,
        "material-cold-ceiling-c",
        configuration.material_cold_ceiling_c,
    );
    push_f64_field(
        &mut record,
        "material-cold-span-c",
        configuration.material_cold_span_c,
    );
    push_f64_field(
        &mut record,
        "material-fresh-drainage-share",
        configuration.material_fresh_drainage_share,
    );
    push_f64_field(
        &mut record,
        "material-bedrock-base",
        configuration.material_bedrock_base,
    );
    push_f64_field(
        &mut record,
        "material-soil-base",
        configuration.material_soil_base,
    );
    push_f64_field(
        &mut record,
        "material-soil-sediment-suppression",
        configuration.material_soil_sediment_suppression,
    );
    push_f64_field(
        &mut record,
        "material-sediment-drainage-share",
        configuration.material_sediment_drainage_share,
    );
    push_f64_field(
        &mut record,
        "material-sediment-shore-share",
        configuration.material_sediment_shore_share,
    );
    push_f64_field(
        &mut record,
        "material-snow-base",
        configuration.material_snow_base,
    );
    push_f64_field(
        &mut record,
        "material-snow-ocean-share",
        configuration.material_snow_ocean_share,
    );
    push_u32_field(
        &mut record,
        "strip-patch-divisions",
        configuration.strip_patch_divisions,
    );
    push_f64_field(
        &mut record,
        "strip-width-step-multiplier",
        configuration.strip_width_step_multiplier,
    );
    push_f64_field(
        &mut record,
        "strip-curvature-gain",
        configuration.strip_curvature_gain,
    );
    push_u32_field(
        &mut record,
        "strip-max-subdivisions",
        configuration.strip_max_subdivisions,
    );
    push_text_field(&mut record, "source-revision", source_revision);
    push_text_field(&mut record, "world-bytes", world_bytes);
    record
}

fn push_text_field(record: &mut String, name: &str, value: &str) {
    write!(record, "{}:{name}:text:{}:", name.len(), value.len())
        .expect("writing a canonical record to String cannot fail");
    record.push_str(value);
    record.push('\n');
}

fn push_u32_field(record: &mut String, name: &str, value: u32) {
    writeln!(record, "{}:{name}:u32:{value:08x}", name.len())
        .expect("writing a canonical record to String cannot fail");
}

fn push_f64_field(record: &mut String, name: &str, value: f64) {
    writeln!(record, "{}:{name}:f64:{:016x}", name.len(), value.to_bits())
        .expect("writing a canonical record to String cannot fail");
}

/// A stable 256-bit metadata digest built from four domain-separated uses of
/// the kernel's frozen seed mixer. This derives identifiers only: it creates
/// no stream and consumes no draw.
fn stable_digest(input: &str) -> [u8; 32] {
    let mut digest = [0; 32];
    let mut framed = String::with_capacity(input.len() + 32);
    for block in 0..4u64 {
        framed.clear();
        framed.push_str("surface-digest-block:");
        framed.push_str(&block.to_string());
        framed.push('\0');
        framed.push_str(input);
        let value = Seed(block)
            .derive(hornvale_kernel::seed::StreamLabel::dynamic(&framed))
            .0;
        digest[(block as usize) * 8..(block as usize + 1) * 8]
            .copy_from_slice(&value.to_le_bytes());
    }
    digest
}

fn hexadecimal(bytes: &[u8]) -> String {
    const DIGITS: &[u8; 16] = b"0123456789abcdef";
    let mut output = String::with_capacity(bytes.len() * 2);
    for &byte in bytes {
        output.push(DIGITS[(byte >> 4) as usize] as char);
        output.push(DIGITS[(byte & 0x0f) as usize] as char);
    }
    output
}

fn resolved_facet(address: &FacetAddress) -> Facet {
    let mut facet = address.macro_face.clone();
    facet.path.extend_from_slice(&address.child_path);
    facet
}

fn blend(vertices: [Vertex; 4], weights: [f64; 4], value: impl Fn(Vertex) -> f64) -> f64 {
    let denominator = weights.iter().sum::<f64>();
    vertices
        .into_iter()
        .zip(weights)
        .map(|(vertex, weight)| value(vertex) * weight)
        .sum::<f64>()
        / denominator
}

fn blend_vector(
    vertices: [Vertex; 4],
    weights: [f64; 4],
    value: impl Fn(Vertex) -> [f64; 3],
) -> [f64; 3] {
    std::array::from_fn(|axis| blend(vertices, weights, |vertex| value(vertex)[axis]))
}

fn category_fraction(
    vertices: [Vertex; 4],
    weights: [f64; 4],
    predicate: impl Fn(Vertex) -> bool,
) -> f64 {
    blend(vertices, weights, |vertex| f64::from(predicate(vertex)))
}

/// Interpolate the same inherited run elevations as terrain's bed profile.
/// Projection reads the repaired centerline; it never constructs drainage.
#[derive(Clone, Copy)]
struct ChannelBed {
    height_m: f64,
    grade_m_per_rad: f64,
    edges: [f64; 4],
    terminal_progress: f64,
}

/// Read uncut, source-owned rills before composing the surface. Candidate
/// selection depends only on position, never the requesting patch or LOD.
/// Trunk beds retain authority; outside them the nearer supported rill wins.
#[derive(Clone, Copy)]
struct RillCandidate {
    vertex: Vertex,
    origin: [f64; 3],
    radius: f64,
    slope: f64,
    spacing: f64,
}

struct RillCandidates {
    candidates: Vec<RillCandidate>,
    index: RillCandidateNode,
    // Dense candidate identity owns its expansion for the context's lifetime.
    // Lazy construction avoids expanding the whole globe for a local request;
    // OnceLock also preserves sharing when callers sample concurrently.
    expanded: Vec<std::sync::OnceLock<ExpandedRills>>,
}

struct ExpandedRills {
    rills: Vec<ExpandedRill>,
    index: RillCandidateNode,
}

struct ExpandedRill {
    curve: RealizedCurve,
    edges: [f64; 4],
    bed: hornvale_terrain::RillBedInputs,
}

struct RillBounds {
    lower: [f64; 3],
    upper: [f64; 3],
}

/// Cartesian bounds avoid pole/face seams. On a unit sphere chord distance
/// never exceeds angular distance, so origin ± angular radius encloses the
/// entire supported cap. Padding covers normalization and bound roundoff;
/// the original angular predicate remains the final authority.
struct RillCandidateNode {
    lower: [f64; 3],
    upper: [f64; 3],
    contents: RillCandidateContents,
}

enum RillCandidateContents {
    Leaf(Vec<usize>),
    Branch(Box<[RillCandidateNode; 2]>),
}

impl RillCandidateNode {
    fn build(bounds: &[RillBounds], mut indices: Vec<usize>) -> Self {
        let mut lower = [f64::INFINITY; 3];
        let mut upper = [f64::NEG_INFINITY; 3];
        for &index in &indices {
            for axis in 0..3 {
                lower[axis] = lower[axis].min(bounds[index].lower[axis]);
                upper[axis] = upper[axis].max(bounds[index].upper[axis]);
            }
        }
        let contents = if indices.len() <= 8 {
            RillCandidateContents::Leaf(indices)
        } else {
            let axis = (0..3)
                .max_by(|&a, &b| (upper[a] - lower[a]).total_cmp(&(upper[b] - lower[b])))
                .expect("three axes");
            indices.sort_unstable_by(|&a, &b| {
                (bounds[a].lower[axis] + bounds[a].upper[axis])
                    .total_cmp(&(bounds[b].lower[axis] + bounds[b].upper[axis]))
                    .then(a.cmp(&b))
            });
            let right = indices.split_off(indices.len() / 2);
            RillCandidateContents::Branch(Box::new([
                Self::build(bounds, indices),
                Self::build(bounds, right),
            ]))
        };
        Self {
            lower,
            upper,
            contents,
        }
    }

    fn lookup(&self, position: [f64; 3], indices: &mut Vec<usize>) {
        if (0..3).any(|axis| position[axis] < self.lower[axis] || position[axis] > self.upper[axis])
        {
            return;
        }
        match &self.contents {
            RillCandidateContents::Leaf(leaf) => indices.extend_from_slice(leaf),
            RillCandidateContents::Branch(children) => {
                for child in children.iter() {
                    child.lookup(position, indices);
                }
            }
        }
    }

    fn at(&self, position: [f64; 3]) -> Vec<usize> {
        let mut indices = Vec::new();
        self.lookup(position, &mut indices);
        // Both levels must preserve the original strict-nearer tie winner.
        indices.sort_unstable();
        indices
    }
}

impl RillCandidates {
    fn new(candidates: Vec<RillCandidate>) -> Self {
        let bounds = candidates
            .iter()
            .map(|candidate| {
                let radius = candidate.radius + 1e-12;
                RillBounds {
                    lower: candidate.origin.map(|v| v - radius),
                    upper: candidate.origin.map(|v| v + radius),
                }
            })
            .collect::<Vec<_>>();
        let index = RillCandidateNode::build(&bounds, (0..candidates.len()).collect());
        let expanded = (0..candidates.len())
            .map(|_| std::sync::OnceLock::new())
            .collect();
        Self {
            candidates,
            index,
            expanded,
        }
    }

    fn at(&self, position: [f64; 3]) -> Vec<usize> {
        self.index.at(position)
    }
}

fn rill_candidates(terrain: &hornvale_terrain::GeneratedTerrain) -> RillCandidates {
    let geo = terrain.geosphere();
    let channels = terrain.channels();
    let square_radius = (hornvale_terrain::vertex_catchment(geo) / 2.0).sqrt();
    RillCandidates::new(
        geo.vertices()
            .filter_map(|vertex| {
                let (line, index) = channels.trunk_vertex(vertex)?;
                let origin = geo.position(vertex);
                let points = &channels.polylines[line].points;
                let reach = points[index.saturating_sub(1)..=index + 1]
                    .iter()
                    .map(|&point| angular_distance(origin, point))
                    .fold(square_radius, f64::max);
                // A branch drains at most this vertex's unit catchment. Include its
                // entire terrace in the cap, not just the centerline's support.
                let neighbors = geo.neighbors(vertex);
                let spacing = neighbors
                    .iter()
                    .map(|&neighbor| {
                        hornvale_kernel::math::acos(
                            dot(origin, geo.position(neighbor)).clamp(-1.0, 1.0),
                        )
                    })
                    .sum::<f64>()
                    / neighbors.len() as f64;
                let slope = hornvale_terrain::local_slope(terrain.globe(), geo, vertex);
                let maximum_edges = hornvale_terrain::band_edges(1.0, slope, spacing);
                Some(RillCandidate {
                    vertex,
                    origin,
                    radius: reach + maximum_edges[3],
                    slope,
                    spacing,
                })
            })
            .collect(),
    )
}

fn rill_bed_from_candidates(
    terrain: &hornvale_terrain::GeneratedTerrain,
    candidates: &RillCandidates,
    position: [f64; 3],
    trunk: Option<(f64, ChannelBed)>,
) -> Option<(f64, ChannelBed)> {
    if trunk.is_some_and(|(distance, bed)| distance.abs() <= bed.edges[0]) {
        return None;
    }
    let mut nearest = trunk.map_or(f64::INFINITY, |(distance, _)| distance.abs());
    let mut winner = None;
    for index in candidates.at(position) {
        let RillCandidate {
            origin,
            radius,
            slope,
            ..
        } = candidates.candidates[index];
        #[cfg(test)]
        tests::RILL_CANDIDATE_VISITS.with(|visits| visits.set(visits.get() + 1));
        if angular_distance(origin, position) > radius {
            continue;
        }
        let rills = candidates.expanded[index]
            .get_or_init(|| expand_rill_candidate(terrain, candidates.candidates[index]));
        for index in rills.index.at(position) {
            let rill = &rills.rills[index];
            #[cfg(test)]
            tests::RILL_SEGMENT_VISITS.with(|visits| visits.set(visits.get() + 1));
            let (distance, _) = hornvale_terrain::feature_sample(&rill.curve, position);
            if distance.abs() < nearest && distance.abs() < rill.edges[3] {
                nearest = distance.abs();
                winner = Some((distance, rill, slope));
            }
        }
    }
    winner.map(|(distance, rill, slope)| {
        (
            distance,
            ChannelBed {
                height_m: rill.bed.sample_at(position),
                grade_m_per_rad: slope,
                edges: rill.edges,
                terminal_progress: 0.0,
            },
        )
    })
}

fn expand_rill_candidate(
    terrain: &hornvale_terrain::GeneratedTerrain,
    candidate: RillCandidate,
) -> ExpandedRills {
    use hornvale_terrain::{EndpointSide, FeatureEndpoint, FeatureId, FeatureKind, TerminalKind};

    let RillCandidate {
        vertex,
        slope,
        spacing,
        ..
    } = candidate;
    let geo = terrain.geosphere();
    let unit = hornvale_terrain::vertex_catchment(geo);
    let cut = hornvale_terrain::CatchmentCut::Drawn(terrain.globe().rill_partition_seed());
    let rills = hornvale_terrain::rills_of(vertex, terrain.channels(), geo, &cut);
    let beds = hornvale_terrain::RillBedInputs::for_rills(terrain, vertex, &rills);
    let rills: Vec<_> = rills
        .iter()
        .zip(beds)
        .enumerate()
        .map(|(index, (rill, bed))| {
            let edges = hornvale_terrain::band_edges(rill.catchment / unit, slope, spacing);
            let feature = FeatureId::new(
                FeatureKind::ChannelReach,
                vertex,
                u32::try_from(index + 1).expect("rill ordinal exceeds feature identity"),
            );
            let curve = RealizedCurve {
                feature,
                points: vec![rill.head, rill.mouth],
                width: vec![2.0 * edges[0]; 2],
                endpoints: [
                    FeatureEndpoint {
                        feature,
                        side: EndpointSide::Upstream,
                        boundary: None,
                        terminal: TerminalKind::Headwater,
                    },
                    FeatureEndpoint {
                        feature,
                        side: EndpointSide::Downstream,
                        boundary: None,
                        terminal: TerminalKind::Confluence,
                    },
                ],
            };
            ExpandedRill { curve, edges, bed }
        })
        .collect();
    let bounds = rills
        .iter()
        .map(|rill| {
            let [a, b] = [rill.curve.points[0], rill.curve.points[1]];
            let normal = cross(a, b);
            let length = hornvale_kernel::math::atan2(dot(normal, normal).sqrt(), dot(a, b));
            // The short great-circle arc lies within length²/8 of its chord.
            // Expand the endpoint box by that sagitta bound and the entire terrace.
            // acos(dot) can round tiny distances to zero; sqrt(epsilon) padding
            // retains those near-boundary winners for the unchanged exact sampler.
            let padding = length * length / 8.0 + rill.edges[3] + 4.0 * f64::EPSILON.sqrt();
            RillBounds {
                lower: std::array::from_fn(|axis| a[axis].min(b[axis]) - padding),
                upper: std::array::from_fn(|axis| a[axis].max(b[axis]) + padding),
            }
        })
        .collect::<Vec<_>>();
    let index = RillCandidateNode::build(&bounds, (0..rills.len()).collect());
    ExpandedRills { rills, index }
}

// Frozen pre-cache evaluator for the equivalence regression.
#[cfg(test)]
fn rill_bed_at(
    terrain: &hornvale_terrain::GeneratedTerrain,
    position: [f64; 3],
    trunk: Option<(f64, ChannelBed)>,
) -> Option<(f64, ChannelBed)> {
    use hornvale_terrain::{EndpointSide, FeatureEndpoint, FeatureId, FeatureKind, TerminalKind};

    if trunk.is_some_and(|(distance, bed)| distance.abs() <= bed.edges[0]) {
        return None;
    }
    let geo = terrain.geosphere();
    let channels = terrain.channels();
    let unit = hornvale_terrain::vertex_catchment(geo);
    let square_radius = (unit / 2.0).sqrt();
    let cut = hornvale_terrain::CatchmentCut::Drawn(terrain.globe().rill_partition_seed());
    let mut nearest = trunk.map_or(f64::INFINITY, |(distance, _)| distance.abs());
    let mut winner = None;
    for vertex in geo.vertices() {
        let Some((line, index)) = channels.trunk_vertex(vertex) else {
            continue;
        };
        let origin = geo.position(vertex);
        let points = &channels.polylines[line].points;
        let reach = points[index.saturating_sub(1)..=index + 1]
            .iter()
            .map(|&point| angular_distance(origin, point))
            .fold(square_radius, f64::max);
        // A branch drains at most this vertex's unit catchment. Include its
        // entire terrace in the cap, not just the centerline's support.
        let neighbors = geo.neighbors(vertex);
        let spacing = neighbors
            .iter()
            .map(|&neighbor| {
                hornvale_kernel::math::acos(dot(origin, geo.position(neighbor)).clamp(-1.0, 1.0))
            })
            .sum::<f64>()
            / neighbors.len() as f64;
        let slope = hornvale_terrain::local_slope(terrain.globe(), geo, vertex);
        let maximum_edges = hornvale_terrain::band_edges(1.0, slope, spacing);
        if angular_distance(origin, position) > reach + maximum_edges[3] {
            continue;
        }
        for (index, rill) in hornvale_terrain::rills_of(vertex, channels, geo, &cut)
            .iter()
            .enumerate()
        {
            let edges = hornvale_terrain::band_edges(rill.catchment / unit, slope, spacing);
            let feature = FeatureId::new(
                FeatureKind::ChannelReach,
                vertex,
                u32::try_from(index + 1).expect("rill ordinal exceeds feature identity"),
            );
            let curve = RealizedCurve {
                feature,
                points: vec![rill.head, rill.mouth],
                width: vec![2.0 * edges[0]; 2],
                endpoints: [
                    FeatureEndpoint {
                        feature,
                        side: EndpointSide::Upstream,
                        boundary: None,
                        terminal: TerminalKind::Headwater,
                    },
                    FeatureEndpoint {
                        feature,
                        side: EndpointSide::Downstream,
                        boundary: None,
                        terminal: TerminalKind::Confluence,
                    },
                ],
            };
            let (distance, _) = hornvale_terrain::feature_sample(&curve, position);
            if distance.abs() < nearest && distance.abs() < edges[3] {
                nearest = distance.abs();
                winner = Some((distance, curve, edges, slope));
            }
        }
    }
    winner.map(|(distance, mut curve, edges, slope)| {
        // The terrain bed evaluator reconstructs this feature's parent chain
        // and samples its settled bed at the query's projection onto the rill.
        curve.points = vec![position; 2];
        let height_m = hornvale_terrain::bed_height_profile(&curve, terrain)[0];
        (
            distance,
            ChannelBed {
                height_m,
                grade_m_per_rad: slope,
                edges,
                terminal_progress: 0.0,
            },
        )
    })
}

fn channel_bed_height(
    terrain: &hornvale_terrain::GeneratedTerrain,
    line: usize,
    position: [f64; 3],
) -> ChannelBed {
    let points = &terrain.channels().polylines[line].points;
    let mut best = (f64::INFINITY, 0, 0.0);
    for (j, pair) in points.windows(2).enumerate() {
        let axis = normalized_or_zero(cross(pair[0], pair[1]));
        let foot = normalized_or_zero(std::array::from_fn(|i| {
            position[i] - axis[i] * dot(position, axis)
        }));
        let length = angular_distance(pair[0], pair[1]);
        let t = if dot(cross(pair[0], foot), axis) < 0.0 {
            0.0
        } else if dot(cross(foot, pair[1]), axis) < 0.0 {
            1.0
        } else if length > 0.0 {
            angular_distance(pair[0], foot) / length
        } else {
            0.0
        };
        let projected = if t <= 0.0 {
            pair[0]
        } else if t >= 1.0 {
            pair[1]
        } else {
            foot
        };
        let distance = angular_distance(position, projected);
        if distance < best.0 {
            best = (distance, j, t.clamp(0.0, 1.0));
        }
    }
    let vertices = &terrain.channels().run_vertices[line];
    let a = terrain.elevation_at(vertices[best.1]).get();
    let b = terrain.elevation_at(vertices[best.1 + 1]).get();
    let length = angular_distance(points[best.1], points[best.1 + 1]);
    let edges = &terrain.channels().band_edges[line];
    let terminal = hornvale_terrain::channel_endpoint_kind(
        TerrainFacetInputs {
            globe: terrain.globe(),
            geo: terrain.geosphere(),
            channels: terrain.channels(),
        },
        *vertices.last().expect("run has a terminal"),
        true,
    );
    ChannelBed {
        height_m: a + best.2 * (b - a),
        grade_m_per_rad: if length > 0.0 {
            ((a - b) / length).max(0.0)
        } else {
            0.0
        },
        edges: std::array::from_fn(|i| {
            edges[best.1][i] + best.2 * (edges[best.1 + 1][i] - edges[best.1][i])
        }),
        terminal_progress: if best.1 + 2 == points.len()
            && matches!(
                terminal,
                hornvale_terrain::TerminalKind::Lake | hornvale_terrain::TerminalKind::Ocean
            ) {
            best.2
        } else {
            0.0
        },
    }
}

/// A continuous transverse profile over the source's four band borders.
/// The center bed excludes noise; banks climb to a graded floodplain, and
/// the terrace settles back to the ambient surface. A terminal apron adds
/// sediment outside the bed only, so it cannot reverse the downstream grade.
/// Total hydrology displacement is capped independently of the relief budget.
fn channel_height_m(
    ambient: f64,
    distance: f64,
    bed: ChannelBed,
    config: SurfaceConfiguration,
) -> f64 {
    let [channel, bank, floodplain, terrace] = bed.edges;
    if distance >= terrace {
        return ambient;
    }
    let mix = |a: f64, b: f64, t: f64| a + t.clamp(0.0, 1.0) * (b - a);
    let flatness = 1.0 - ratio(bed.grade_m_per_rad, config.slope_half_saturation_m_per_rad);
    let bank_height = bed.height_m + config.bank_height_m * flatness;
    let flood_height = bed.height_m + config.floodplain_height_m * flatness;
    let shaped = if distance <= channel {
        bed.height_m - config.channel_incision_m
    } else if distance < bank {
        mix(
            bed.height_m - config.channel_incision_m,
            bank_height,
            (distance - channel) / (bank - channel),
        )
    } else if distance < floodplain {
        mix(
            bank_height,
            flood_height,
            (distance - bank) / (floodplain - bank),
        )
    } else {
        mix(
            flood_height,
            ambient,
            (distance - floodplain) / (terrace - floodplain),
        )
    };
    let apron = config.delta_height_m
        * bed.terminal_progress
        * f64::from(annulus_weight(distance, bank, floodplain));
    ambient
        + (shaped + apron - ambient).clamp(
            -config.hydrology_max_adjustment_m,
            config.hydrology_max_adjustment_m,
        )
}

fn angular_distance(a: [f64; 3], b: [f64; 3]) -> f64 {
    let normal = cross(a, b);
    hornvale_kernel::math::atan2(dot(normal, normal).sqrt(), dot(a, b))
}

fn ridge_noise(
    noise: &hornvale_terrain::SphereFbm,
    position: [f64; 3],
    direction: [f64; 3],
    smoothing_rad: f64,
) -> f64 {
    let offset = |sign: f64| {
        normalized_or_zero(std::array::from_fn(|i| {
            position[i] + sign * smoothing_rad * direction[i]
        }))
    };
    // Sum the symmetric pair first so direction reversal is bit-identical.
    2.0 * ((noise.sample(offset(-1.0)) + noise.sample(offset(1.0))) + noise.sample(position)) / 3.0
        - 1.0
}

fn relief_amplitude_m(
    slope_strength: f64,
    height_asl_m: f64,
    shoreline_distance_m: f64,
    configuration: SurfaceConfiguration,
) -> f64 {
    let slope = (1.0 - configuration.relief_slope_share)
        + configuration.relief_slope_share * slope_strength.clamp(0.0, 1.0);
    let elevation = configuration.relief_elevation_base
        + (1.0 - configuration.relief_elevation_base)
            * ratio(
                height_asl_m.abs(),
                configuration.relief_elevation_half_saturation_m,
            );
    let coast = (shoreline_distance_m.abs() / configuration.relief_coast_taper_m)
        .clamp(configuration.relief_coast_minimum, 1.0);
    configuration.relief_max_amplitude_m * slope * elevation * coast
}

fn downhill_direction(terrain: &hornvale_terrain::GeneratedTerrain, vertex: Vertex) -> [f64; 3] {
    let Some(target) = *terrain.globe().downhill.get(vertex) else {
        return [0.0; 3];
    };
    let from = terrain.geosphere().position(vertex);
    let to = terrain.geosphere().position(target);
    let projection = dot(from, to);
    normalized_or_zero([
        to[0] - projection * from[0],
        to[1] - projection * from[1],
        to[2] - projection * from[2],
    ])
}

fn annulus_weight(distance: f64, inner: f64, outer: f64) -> f32 {
    if !distance.is_finite() || outer <= inner || distance < inner || distance > outer {
        return 0.0;
    }
    let midpoint = (inner + outer) / 2.0;
    let half_width = (outer - inner) / 2.0;
    (1.0 - (distance - midpoint).abs() / half_width).clamp(0.0, 1.0) as f32
}

struct MaterialInputs {
    temperature_c: f64,
    moisture: f64,
    height_asl_m: f64,
    slope_strength: f64,
    shoreline_distance_m: f64,
    ocean: f64,
    salt: f64,
    river: f64,
    sediment_m: f64,
    drainage: f64,
    induration: f64,
}

fn material_weights(input: MaterialInputs, configuration: SurfaceConfiguration) -> [f32; 8] {
    let moisture = input.moisture.clamp(0.0, 1.0);
    let slope = input.slope_strength.clamp(0.0, 1.0);
    let land = (1.0 - input.ocean - input.salt).clamp(0.0, 1.0);
    let altitude =
        (input.height_asl_m.max(0.0) / configuration.material_altitude_scale_m).clamp(0.0, 1.0);
    let shore = (1.0 - input.shoreline_distance_m.abs() / configuration.material_shore_scale_m)
        .clamp(0.0, 1.0);
    let sediment = ratio(
        input.sediment_m.max(0.0),
        configuration.material_sediment_half_saturation_m,
    );
    let drainage = ratio(
        input.drainage.max(0.0),
        configuration.material_drainage_half_saturation,
    );
    let warmth = ((input.temperature_c - configuration.material_warmth_floor_c)
        / configuration.material_warmth_span_c)
        .clamp(0.0, 1.0);
    let cold = ((configuration.material_cold_ceiling_c - input.temperature_c)
        / configuration.material_cold_span_c)
        .clamp(0.0, 1.0);
    let fresh_water = (input.river
        + configuration.material_fresh_drainage_share * drainage * shore)
        .clamp(0.0, 1.0);
    let salt_water = (input.ocean + input.salt).clamp(0.0, 1.0);
    let mut raw = [
        land * (configuration.material_bedrock_base
            + input.induration.clamp(0.0, 1.0)
            + slope
            + altitude),
        land * (1.0 - slope)
            * (configuration.material_soil_base + moisture)
            * (1.0 - sediment * configuration.material_soil_sediment_suppression),
        land * (sediment
            + configuration.material_sediment_drainage_share * drainage
            + configuration.material_sediment_shore_share * shore),
        land * moisture * (1.0 - slope) * (input.river + drainage),
        fresh_water,
        salt_water,
        cold * (configuration.material_snow_base
            + altitude
            + configuration.material_snow_ocean_share * input.ocean),
        land * warmth * moisture * (1.0 - slope),
    ];
    let total = raw.iter().sum::<f64>();
    if total <= f64::EPSILON || !total.is_finite() {
        raw = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0];
    } else {
        for weight in &mut raw {
            *weight /= total;
        }
    }
    raw.map(|weight| weight as f32)
}

fn validate_sample(sample: &FacetFieldSample) -> Result<(), SurfaceBuildError> {
    let finite = sample.position.into_iter().all(f64::is_finite)
        && sample.height_m.is_finite()
        && sample.normal.into_iter().all(f64::is_finite)
        && sample.material_weights.into_iter().all(f32::is_finite)
        && sample.shoreline_distance_m.is_finite()
        && sample.water_depth_m.is_finite()
        && sample.flow_direction.into_iter().all(f64::is_finite)
        && sample.flow_strength.is_finite()
        && sample.channel_distance_m.is_finite()
        && sample.channel_width_m.is_finite()
        && sample.floodplain_weight.is_finite()
        && sample.bank_weight.is_finite()
        && sample.terrace_weight.is_finite()
        && sample.delta_weight.is_finite()
        && sample.ridge_direction.into_iter().all(f64::is_finite)
        && sample.ridge_strength.is_finite();
    if finite {
        Ok(())
    } else {
        Err(SurfaceBuildError::Numeric(format!(
            "surface sample at {:?} contains a non-finite value",
            sample.position
        )))
    }
}

fn ratio(value: f64, half_saturation: f64) -> f64 {
    value / (value + half_saturation)
}

fn dot(a: [f64; 3], b: [f64; 3]) -> f64 {
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
}

fn cross(a: [f64; 3], b: [f64; 3]) -> [f64; 3] {
    [
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    ]
}

fn normalized_or_zero(vector: [f64; 3]) -> [f64; 3] {
    let magnitude = dot(vector, vector).sqrt();
    if magnitude <= f64::EPSILON {
        [0.0; 3]
    } else {
        vector.map(|component| component / magnitude)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    thread_local! {
        pub(super) static RILL_CANDIDATE_VISITS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }; // lexicon: std::cell::Cell is an interior-mutability counter, not a spatial area
        pub(super) static RILL_SEGMENT_VISITS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) }; // lexicon: std::cell::Cell is an interior-mutability counter, not a spatial area
    }

    #[test]
    fn expanded_rill_lookup_examines_only_nearby_segments() {
        let context = fixture_context();
        let cut =
            hornvale_terrain::CatchmentCut::Drawn(context.terrain.globe().rill_partition_seed());
        let mut maximum_visits = 0;
        for candidate in context.rill_candidates.candidates.iter().step_by(700) {
            let rills = hornvale_terrain::rills_of(
                candidate.vertex,
                context.terrain.channels(),
                context.terrain.geosphere(),
                &cut,
            );
            for rill in rills.iter().take(2) {
                let position =
                    normalized_or_zero(std::array::from_fn(|i| rill.head[i] + rill.mouth[i]));
                // Repeat a real supported position to exercise retained geometry.
                for _ in 0..2 {
                    RILL_SEGMENT_VISITS.with(|visits| visits.set(0));
                    rill_bed_from_candidates(
                        &context.terrain,
                        &context.rill_candidates,
                        position,
                        None,
                    );
                    let visits = RILL_SEGMENT_VISITS.with(|visits| visits.get());
                    maximum_visits = maximum_visits.max(visits);
                    assert!(
                        visits <= 256,
                        "one sample examined {visits} expanded segments (budget 256)"
                    );
                }
            }
        }
        assert!(maximum_visits > 0);
        eprintln!("expanded rill lookup: at most {maximum_visits} segments examined");
    }

    #[test]
    fn rill_lookup_does_not_scan_the_globe_per_sample() {
        let context = fixture_context();
        let mut maximum_visits = 0;
        for position in context
            .terrain
            .geosphere()
            .vertices()
            .step_by(641)
            .map(|vertex| context.terrain.geosphere().position(vertex))
        {
            RILL_CANDIDATE_VISITS.with(|visits| visits.set(0));
            // Exercise the lookup without trunk suppression hiding its cost.
            rill_bed_from_candidates(&context.terrain, &context.rill_candidates, position, None);
            let visits = RILL_CANDIDATE_VISITS.with(|visits| visits.get());
            maximum_visits = maximum_visits.max(visits);
            assert!(
                visits <= 256,
                "one position examined {visits} rill candidates (budget 256)"
            );
        }
        eprintln!(
            "rill lookup: at most {maximum_visits} of {} candidates examined",
            context.rill_candidates.candidates.len()
        );
    }

    #[test]
    fn rill_index_preserves_cap_boundaries_and_scan_order() {
        let mut points = vec![
            [1.0, 0.0, 0.0],
            [-1.0, 0.0, 0.0],
            [0.0, 1.0, 0.0],
            [0.0, -1.0, 0.0],
            [0.0, 0.0, 1.0],
            [0.0, 0.0, -1.0],
        ];
        for face in 0..6 {
            points.extend(
                Facet {
                    face,
                    path: vec![0; 6],
                }
                .corners(),
            );
        }
        let candidates = RillCandidates::new(
            points
                .iter()
                .flat_map(|&origin| {
                    // Duplicate origins and different support radii exercise ties and
                    // caps spanning split planes, poles and cube-face boundaries.
                    [0.0, 0.005, 0.04, 0.5, std::f64::consts::PI].map(|radius| RillCandidate {
                        vertex: Vertex(0),
                        origin,
                        radius,
                        slope: 0.0,
                        spacing: 0.0,
                    })
                })
                .collect(),
        );
        for candidate in &candidates.candidates {
            let axis = if candidate.origin[2].abs() < 0.9 {
                [0.0, 0.0, 1.0]
            } else {
                [1.0, 0.0, 0.0]
            };
            let tangent = normalized_or_zero(cross(candidate.origin, axis));
            for angle in [
                candidate.radius - 1e-13,
                candidate.radius,
                candidate.radius + 1e-13,
            ] {
                points.push(normalized_or_zero(std::array::from_fn(|i| {
                    candidate.origin[i] * hornvale_kernel::math::cos(angle)
                        + tangent[i] * hornvale_kernel::math::sin(angle)
                })));
            }
        }
        for position in points {
            let supports = |&i: &usize| {
                let candidate = candidates.candidates[i];
                angular_distance(candidate.origin, position) <= candidate.radius
            };
            let expected = (0..candidates.candidates.len())
                .filter(supports)
                .collect::<Vec<_>>();
            let actual = candidates
                .at(position)
                .into_iter()
                .filter(supports)
                .collect::<Vec<_>>();
            assert_eq!(
                actual, expected,
                "cap membership/order changed at {position:?}"
            );
        }
        assert!(
            RillCandidates::new(Vec::new())
                .at([1.0, 0.0, 0.0])
                .is_empty()
        );
    }

    #[test]
    fn indexed_rill_samples_are_bit_exact_with_full_scan() {
        let mut context = fixture_context();
        let cut =
            hornvale_terrain::CatchmentCut::Drawn(context.terrain.globe().rill_partition_seed());
        let mut points = Vec::new();
        for candidate in context.rill_candidates.candidates.iter().step_by(1400) {
            for rill in hornvale_terrain::rills_of(
                candidate.vertex,
                context.terrain.channels(),
                context.terrain.geosphere(),
                &cut,
            )
            .iter()
            .take(2)
            {
                points.extend([
                    rill.head,
                    rill.mouth,
                    normalized_or_zero(std::array::from_fn(|i| rill.head[i] + rill.mouth[i])),
                ]);
            }
        }
        assert!(!points.is_empty());
        let bits = |result: Option<(f64, ChannelBed)>| {
            result.map(|(distance, bed)| {
                (
                    distance.to_bits(),
                    bed.height_m.to_bits(),
                    bed.grade_m_per_rad.to_bits(),
                    bed.edges.map(f64::to_bits),
                    bed.terminal_progress.to_bits(),
                )
            })
        };
        let mut supported = 0;
        for position in points {
            let expected = rill_bed_at(&context.terrain, position, None);
            supported += usize::from(expected.is_some());
            assert_eq!(
                bits(rill_bed_from_candidates(
                    &context.terrain,
                    &context.rill_candidates,
                    position,
                    None
                )),
                bits(expected)
            );
            let sample = context.sample_at(position).unwrap();
            // Force the same production composition through a full scan to
            // compare every emitted field, including height/normal/materials.
            let index = std::mem::replace(
                &mut context.rill_candidates.index,
                RillCandidateNode {
                    lower: [f64::NEG_INFINITY; 3],
                    upper: [f64::INFINITY; 3],
                    contents: RillCandidateContents::Leaf(
                        (0..context.rill_candidates.candidates.len()).collect(),
                    ),
                },
            );
            let full_scan_sample = context.sample_at(position).unwrap();
            context.rill_candidates.index = index;
            assert_eq!(sample, full_scan_sample);
        }
        assert!(supported > 0, "oracle must exercise winning rills");
    }

    fn fixture_context() -> SurfaceRealizationContext {
        let world = crate::seed_42_world();
        SurfaceRealizationContext::build(&world).unwrap()
    }

    fn fine_address(position: [f64; 3]) -> FacetAddress {
        let fine = Facet::containing(position, 20);
        FacetAddress::new(
            Facet {
                face: fine.face,
                path: fine.path[..6].to_vec(),
            },
            fine.path[6..].to_vec(),
        )
        .unwrap()
    }

    #[test]
    fn strip_heights_and_normals_use_position_evaluation_at_every_lod() {
        let context = fixture_context();
        let point = context.terrain.channels().polylines[0].points[0];
        let macro_face = Facet::containing(point, 6);
        for path in [vec![], vec![0], vec![1], vec![2], vec![3]] {
            let patch = context
                .realize(&FacetAddress::new(macro_face.clone(), path).unwrap())
                .unwrap();
            for vertex in patch.strips.iter().flat_map(|strip| &strip.vertices) {
                let sample = context.sample_at(vertex.position).unwrap();
                assert_eq!(
                    vertex.height_m, sample.height_m,
                    "strip height must not depend on nearest patch sample"
                );
                assert_eq!(vertex.normal, sample.normal);
            }
        }
    }

    #[test]
    fn retained_rill_candidates_preserve_position_based_bed_evaluation() {
        let context = fixture_context();
        let candidates = rill_candidates(&context.terrain);
        for point in context.terrain.channels().polylines[0]
            .points
            .iter()
            .take(2)
        {
            let actual = rill_bed_from_candidates(&context.terrain, &candidates, *point, None);
            let expected = rill_bed_at(&context.terrain, *point, None);
            assert_eq!(
                actual.map(|(d, b)| (d, b.height_m, b.edges)),
                expected.map(|(d, b)| (d, b.height_m, b.edges))
            );
        }
    }

    #[test]
    fn strip_footprints_stay_inside_the_requested_patch() {
        let context = fixture_context();
        let point = context.terrain.channels().polylines[0].points[0];
        let macro_face = Facet::containing(point, 6);
        let mut checked = 0;
        for path in [vec![], vec![0], vec![1], vec![2], vec![3]] {
            let address = FacetAddress::new(macro_face.clone(), path).unwrap();
            let patch = context.realize(&address).unwrap();
            let corners = (0..4)
                .map(|i| canonical_corner_sample(&address, i))
                .collect::<Vec<_>>();
            for vertex in patch.strips.iter().flat_map(|strip| &strip.vertices) {
                checked += 1;
                for edge in 0..4 {
                    let a = corners[edge];
                    let b = corners[(edge + 1) % 4];
                    let normal = [
                        a[1] * b[2] - a[2] * b[1],
                        a[2] * b[0] - a[0] * b[2],
                        a[0] * b[1] - a[1] * b[0],
                    ];
                    let dot = |p: [f64; 3]| normal.iter().zip(p).map(|(n, p)| n * p).sum::<f64>();
                    assert!(
                        dot(vertex.position) * dot(patch.samples[4].position).signum() >= -1e-15,
                        "ribbon footprint escapes patch at edge {edge}: {:?}",
                        vertex.position
                    );
                }
            }
        }
        assert!(checked > 0);
    }

    #[test]
    fn emitted_rill_outside_trunk_terrace_uses_inherited_bed() {
        let mut context = fixture_context();
        let inputs = TerrainFacetInputs {
            globe: context.terrain.globe(),
            geo: context.terrain.geosphere(),
            channels: context.terrain.channels(),
        };
        let address = FacetAddress::new(
            Facet::containing(inputs.channels.polylines[0].points[0], 6),
            vec![],
        )
        .unwrap();
        let curves = realize_channel_curves(inputs, &address);
        for curve in curves.iter().filter(|curve| curve.feature.ordinal > 0) {
            let position = normalized_or_zero(std::array::from_fn(|i| {
                curve.points[0][i] + curve.points[1][i]
            }));
            let address = fine_address(position);
            let before = context.realize(&address).unwrap();
            let sample = before.samples[4];
            let reading = context
                .terrain
                .channels()
                .bank_reading(sample.position)
                .unwrap();
            let trunk_bed = channel_bed_height(&context.terrain, reading.line, sample.position);
            let (distance, width) = hornvale_terrain::feature_sample(curve, sample.position);
            if reading.signed_distance.abs() <= trunk_bed.edges[3] || distance.abs() >= width / 2.0
            {
                continue;
            }
            let mut at_sample = curve.clone();
            at_sample.points = vec![sample.position; 2];
            let inherited = hornvale_terrain::bed_height_profile(&at_sample, &context.terrain)[0];
            assert!(
                (sample.height_m - (inherited - context.configuration.channel_incision_m)).abs()
                    < 1e-6,
                "rill {:?} outside trunk terrace has height {}, inherited bed {inherited}",
                curve.feature,
                sample.height_m,
            );
            context.configuration.channel_incision_m += 1.0;
            let after = context.realize(&address).unwrap();
            assert!((sample.height_m - after.samples[4].height_m - 1.0).abs() < 1e-6);
            return;
        }
        panic!("seed-42 fixture must emit a rill outside the trunk terrace");
    }

    #[test]
    fn emitted_hydrology_obeys_and_reaches_configured_adjustment_bound() {
        let mut context = fixture_context();
        let address = fine_address(context.terrain.channels().polylines[0].points[0]);
        context.configuration.hydrology_max_adjustment_m = 0.0;
        let ambient = context.realize(&address).unwrap();
        for bound in [0.5, 7.0, 2_000.0] {
            context.configuration.hydrology_max_adjustment_m = bound;
            let shaped = context.realize(&address).unwrap();
            let mut maximum: f64 = 0.0;
            for (a, b) in shaped.samples.iter().zip(&ambient.samples) {
                let displacement = (a.height_m - b.height_m).abs();
                assert!(
                    displacement <= bound + 1e-9,
                    "hydrology exceeds {bound} m: {displacement}"
                );
                maximum = maximum.max(displacement);
            }
            assert!(maximum > 0.0, "hydrology must be active");
            if bound <= 7.0 {
                assert!(
                    (maximum - bound).abs() < 1e-9,
                    "fixture must saturate {bound} m, got {maximum}"
                );
            }
        }
    }

    #[test]
    fn emitted_beds_descend_through_actual_confluence_lake_and_ocean_terminals() {
        use hornvale_terrain::TerminalKind;
        let context = fixture_context();
        let inputs = TerrainFacetInputs {
            globe: context.terrain.globe(),
            geo: context.terrain.geosphere(),
            channels: context.terrain.channels(),
        };
        for kind in [
            TerminalKind::Confluence,
            TerminalKind::Lake,
            TerminalKind::Ocean,
        ] {
            let line = inputs
                .channels
                .run_vertices
                .iter()
                .position(|vertices| {
                    hornvale_terrain::channel_endpoint_kind(inputs, *vertices.last().unwrap(), true)
                        == kind
                })
                .expect("seed-42 fixture must include each actual terminal kind");
            let points = &inputs.channels.polylines[line].points;
            let mut previous = f64::INFINITY;
            for pair in points.windows(2) {
                for t in [0.0, 0.25, 0.5, 0.75, 1.0] {
                    let position = if t == 0.0 {
                        pair[0]
                    } else if t == 1.0 {
                        pair[1]
                    } else {
                        normalized_or_zero(std::array::from_fn(|i| {
                            pair[0][i] * (1.0 - t) + pair[1][i] * t
                        }))
                    };
                    let sample = context.sample_at(position).unwrap();
                    assert!(
                        sample.height_m <= previous + 1e-8,
                        "bed rises on run {line} to {kind:?}: {previous} -> {}",
                        sample.height_m
                    );
                    previous = sample.height_m;
                }
            }
            let terminal = *inputs.channels.run_vertices[line].last().unwrap();
            let expected = context.terrain.elevation_at(terminal).get()
                - context.configuration.channel_incision_m;
            assert!(
                (previous - expected).abs() < 1e-8,
                "run {line} did not settle onto actual {kind:?} bed: {previous} vs {expected}"
            );
        }
    }

    #[test]
    fn relief_calibrations_each_change_the_configuration_hash() {
        let baseline = SurfaceConfiguration::current();
        let hash = surface_revision("world", "source", baseline).configuration_hash;
        let changes: [fn(&mut SurfaceConfiguration); 10] = [
            |c| c.relief_elevation_base += 0.01,
            |c| c.relief_coast_minimum += 0.01,
            |c| c.channel_incision_m += 1.0,
            |c| c.bank_height_m += 1.0,
            |c| c.floodplain_height_m += 1.0,
            |c| c.hydrology_max_adjustment_m += 1.0,
            |c| c.delta_height_m += 1.0,
            |c| c.ridge_share += 0.01,
            |c| c.ridge_smoothing_rad += 0.001,
            |c| c.relief_max_amplitude_m += 1.0,
        ];
        for change in changes {
            let mut config = baseline;
            change(&mut config);
            assert_ne!(
                hash,
                surface_revision("world", "source", config).configuration_hash
            );
        }
    }

    #[test]
    fn emitted_channel_bands_and_terminal_apron_respond_to_calibration() {
        let mut context = fixture_context();
        let mut checked = [false; 4];
        for line in 0..context.terrain.channels().polylines.len() {
            let points = context.terrain.channels().polylines[line].points.clone();
            let j = points.len() - 2;
            let center =
                normalized_or_zero(std::array::from_fn(|i| points[j][i] + points[j + 1][i]));
            let side = normalized_or_zero(cross(points[j], points[j + 1]));
            let edges = context.terrain.channels().band_edges[line][j];
            let vertex = *context.terrain.channels().run_vertices[line]
                .last()
                .unwrap();
            let terminal = hornvale_terrain::channel_endpoint_kind(
                TerrainFacetInputs {
                    globe: context.terrain.globe(),
                    geo: context.terrain.geosphere(),
                    channels: context.terrain.channels(),
                },
                vertex,
                true,
            );
            for (band, checked_band) in checked.iter_mut().enumerate() {
                if *checked_band
                    || (band == 3
                        && !matches!(
                            terminal,
                            hornvale_terrain::TerminalKind::Ocean
                                | hornvale_terrain::TerminalKind::Lake
                        ))
                {
                    continue;
                }
                let borders = match band {
                    0 => [edges[0], edges[1]],
                    1 | 3 => [edges[1], edges[2]],
                    _ => [edges[2], edges[3]],
                };
                if borders[1] <= borders[0] {
                    continue;
                }
                let distance = (borders[0] + borders[1]) / 2.0;
                let position =
                    normalized_or_zero(std::array::from_fn(|i| center[i] + distance * side[i]));
                if context
                    .terrain
                    .channels()
                    .bank_reading(position)
                    .unwrap()
                    .line
                    != line
                {
                    continue;
                }
                let fine = Facet::containing(position, 20);
                let address = FacetAddress::new(
                    Facet {
                        face: fine.face,
                        path: fine.path[..6].to_vec(),
                    },
                    fine.path[6..].to_vec(),
                )
                .unwrap();
                let before = context.realize(&address).unwrap();
                let saved = context.configuration;
                match band {
                    0 => context.configuration.bank_height_m += 1.0,
                    1 | 2 => context.configuration.floodplain_height_m += 1.0,
                    _ => context.configuration.delta_height_m += 1.0,
                }
                let after = context.realize(&address).unwrap();
                context.configuration = saved;
                assert!(
                    before
                        .samples
                        .iter()
                        .zip(&after.samples)
                        .any(|(a, b)| (a.height_m - b.height_m).abs() > 1e-7),
                    "band {band} calibration never reaches emitted heights"
                );
                *checked_band = true;
            }
            if checked.iter().all(|v| *v) {
                return;
            }
        }
        panic!("fixture missed channel bands: {checked:?}");
    }

    #[test]
    fn emitted_ridge_relief_is_active_and_bounded_by_configuration() {
        let mut context = fixture_context();
        // Exercise the configured bound, including a non-default budget.
        context.configuration.relief_max_amplitude_m = 7.0;
        let mut active = 0;
        let mut directional = 0;
        for face in 0..6 {
            let address = FacetAddress::new(
                Facet {
                    face,
                    path: vec![0; 6],
                },
                vec![],
            )
            .unwrap();
            let with = context.realize(&address).unwrap();
            let share = context.configuration.ridge_share;
            context.configuration.ridge_share = 0.0;
            let without = context.realize(&address).unwrap();
            context.configuration.ridge_share = share;
            let bound = context.configuration.relief_max_amplitude_m;
            context.configuration.relief_max_amplitude_m = 0.0;
            let no_relief = context.realize(&address).unwrap();
            context.configuration.relief_max_amplitude_m = bound;
            for (sample, baseline) in with.samples.iter().zip(&no_relief.samples) {
                assert!(
                    (sample.height_m - baseline.height_m).abs() <= bound + 1e-10,
                    "emitted relief exceeds its configured {bound} m budget"
                );
                let ridge = ridge_noise(
                    &context.relief_noise,
                    sample.position,
                    sample.ridge_direction,
                    context.configuration.ridge_smoothing_rad,
                );
                let reversed = ridge_noise(
                    &context.relief_noise,
                    sample.position,
                    sample.ridge_direction.map(|v| -v),
                    context.configuration.ridge_smoothing_rad,
                );
                let transverse = ridge_noise(
                    &context.relief_noise,
                    sample.position,
                    normalized_or_zero(cross(sample.position, sample.ridge_direction)),
                    context.configuration.ridge_smoothing_rad,
                );
                assert_eq!(
                    ridge, reversed,
                    "an undirected ridge must survive axis reversal"
                );
                directional += usize::from((ridge - transverse).abs() > 1e-7);
            }
            for (a, b) in with.samples.iter().zip(&without.samples) {
                let difference = (a.height_m - b.height_m).abs();
                assert!(difference <= 2.0 * share * context.configuration.relief_max_amplitude_m);
                active += usize::from(difference > 1e-7 && a.ridge_strength > 0.0);
            }
        }
        assert!(
            active > 0,
            "ridge contribution never reaches emitted heights"
        );
        assert!(
            directional > 0,
            "ridge noise ignores the inherited direction"
        );
    }

    #[test]
    fn changing_consumed_calibration_changes_configuration_hash_and_revision() {
        let baseline = SurfaceConfiguration::current();
        let mut changed = baseline;
        changed.flow_half_saturation += 1.0;

        let baseline_revision =
            surface_revision("canonical-world-bytes", "source-revision", baseline);
        let changed_revision =
            surface_revision("canonical-world-bytes", "source-revision", changed);

        assert_ne!(
            baseline_revision.configuration_hash,
            changed_revision.configuration_hash
        );
        assert_ne!(baseline_revision, changed_revision);
    }
}
