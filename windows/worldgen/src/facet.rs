//! Deterministic composition of addressed terrain and climate surface patches.

use std::fmt::{self, Write as _};

use hornvale_kernel::{Facet, NearestVertexIndex, Seed, Vertex, World};
use hornvale_terrain::{
    FacetAddress, FacetFieldSample, RealizedCurve, TerrainFacetInputs, WaterKind,
    canonical_corner_sample, canonical_edge_sample, realize_channel_curves,
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
    /// Sample indices forming the patch mesh.
    pub triangles: Vec<[u32; 3]>,
    /// Source-computed replacement topology for one unequal-LOD boundary.
    /// Indices address this patch's samples; an empty list means no seam was
    /// requested for this realization.
    pub transition_triangles: Vec<[u32; 3]>,
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
}

impl SurfaceConfiguration {
    const fn current() -> Self {
        Self {
            record_schema: "hornvale/surface-configuration/v4",
            algorithm_version: "hornvale/surface-realization/v4",
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

        Ok(Self {
            revision: revision.clone(),
            expected_revision: revision,
            terrain,
            climate,
            nearest,
            planet_radius_m,
            relief_noise,
            configuration,
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

        Ok(SurfacePatch {
            revision: self.revision.clone(),
            address: address.clone(),
            samples,
            curves,
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
        let ambient_height = macro_height_m + relief_m;
        let height_m = match (channel_reading, channel_bed) {
            (Some(reading), Some(bed)) => channel_height_m(
                ambient_height,
                reading.signed_distance.abs(),
                bed,
                self.configuration,
            ),
            _ => ambient_height,
        };
        let height_asl_m = height_m - sea_level_m;
        let water_depth_m = (-height_asl_m).max(0.0);

        let (channel_distance_m, channel_width_m, bank_weight, floodplain_weight, terrace_weight) =
            match channel_reading {
                Some(reading) => {
                    let distance = reading.signed_distance * self.planet_radius_m;
                    let edges = channel_bed
                        .expect("reading has a bed")
                        .edges
                        .map(|edge| edge * self.planet_radius_m);
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

    fn fixture_context() -> SurfaceRealizationContext {
        let world = crate::seed_42_world();
        SurfaceRealizationContext::build(&world).unwrap()
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
