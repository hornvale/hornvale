//! Deterministic composition of addressed terrain and climate surface patches.

use std::fmt::{self, Write as _};

use hornvale_kernel::{Facet, NearestVertexIndex, Seed, Vertex, World};
use hornvale_terrain::{
    FacetAddress, FacetFieldSample, RealizedCurve, TerrainFacetInputs, WaterKind,
    realize_channel_curves,
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
/// type-audit: bare-ok(index: triangles)
#[derive(Clone, Debug, PartialEq)]
pub struct SurfacePatch {
    /// Macro-world and realization revision that produced the patch.
    pub revision: SurfaceRevision,
    /// Canonical Level-6 macro facet and any refinement below it.
    pub address: FacetAddress,
    /// Continuous terrain and material samples in vertex order.
    pub samples: Vec<FacetFieldSample>,
    /// Stable inherited terrain curves clipped to this patch.
    pub curves: Vec<RealizedCurve>,
    /// Sample indices forming the patch mesh.
    pub triangles: Vec<[u32; 3]>,
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
            record_schema: "hornvale/surface-configuration/v2",
            algorithm_version: "hornvale/surface-realization/v1",
            globe_level: hornvale_terrain::GLOBE_LEVEL,
            sample_topology: "quad-corners-then-centroid/fan-v1",
            material_channels: "bedrock,soil,sediment,wetland,fresh-water,salt-water,snow-ice,vegetation/v1",
            flow_half_saturation: 24.0,
            slope_half_saturation_m_per_rad: 80_000.0,
            channel_width_multiplier: 2.0,
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
        let configuration = SurfaceConfiguration::current();
        let world_bytes = world.to_json();
        let revision = surface_revision(&world_bytes, configuration);

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

        Ok(Self {
            revision: revision.clone(),
            expected_revision: revision,
            terrain,
            climate,
            nearest,
            planet_radius_m,
            configuration,
        })
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

        let resolved = resolved_facet(address);
        let mut positions = resolved.corners().to_vec();
        positions.push(resolved.centroid());
        let sample_weights = patch_sample_weights(address)?;
        let macro_vertices = self.macro_vertices(address);
        let mut samples = Vec::with_capacity(positions.len());
        for (position, weights) in positions.into_iter().zip(sample_weights) {
            samples.push(self.compose_sample(position, macro_vertices, weights)?);
        }

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
        })
    }

    fn macro_vertices(&self, address: &FacetAddress) -> [Vertex; 4] {
        address.macro_face.corners().map(|position| {
            self.nearest
                .nearest_to_position(self.terrain.geosphere(), position)
        })
    }

    fn compose_sample(
        &self,
        position: [f64; 3],
        vertices: [Vertex; 4],
        weights: [u64; 4],
    ) -> Result<FacetFieldSample, SurfaceBuildError> {
        let height_m = blend(vertices, weights, |vertex| {
            self.terrain.elevation_at(vertex).get()
        });
        let sea_level_m = self.terrain.sea_level().get();
        let height_asl_m = height_m - sea_level_m;
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
            (height_asl_m * self.planet_radius_m / slope_m_per_rad)
                .clamp(-max_surface_distance, max_surface_distance)
        } else if height_asl_m < 0.0 {
            -max_surface_distance
        } else {
            max_surface_distance
        };
        let water_depth_m = (-height_asl_m).max(0.0);

        let (channel_distance_m, channel_width_m, bank_weight, floodplain_weight, terrace_weight) =
            match self.terrain.channels().bank_reading(position) {
                Some(reading) => {
                    let distance = reading.signed_distance * self.planet_radius_m;
                    let edges = reading.band_edges.map(|edge| edge * self.planet_radius_m);
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
        let ridge_direction = normalized_or_zero(cross(position, flow_direction));
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
            delta_weight: (delta * (1.0 - slope_strength)) as f32,
            ridge_direction,
            ridge_strength: (slope_strength * (1.0 - flow_strength)) as f32,
        };
        validate_sample(&sample)?;
        Ok(sample)
    }
}

fn surface_revision(world_bytes: &str, configuration: SurfaceConfiguration) -> SurfaceRevision {
    let source_revision = hexadecimal(&stable_digest(world_bytes));
    let configuration_record =
        canonical_configuration_record(world_bytes, &source_revision, configuration);
    SurfaceRevision {
        source_revision,
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

fn patch_sample_weights(address: &FacetAddress) -> Result<[[u64; 4]; 5], SurfaceBuildError> {
    let relative = Facet {
        face: 0,
        path: address.child_path.clone(),
    }
    .face_lattice();
    let x = u64::try_from(relative.x)
        .map_err(|_| SurfaceBuildError::Numeric("negative child lattice x".to_string()))?;
    let y = u64::try_from(relative.y)
        .map_err(|_| SurfaceBuildError::Numeric("negative child lattice y".to_string()))?;
    let scale = u64::try_from(relative.scale)
        .map_err(|_| SurfaceBuildError::Numeric("negative child lattice scale".to_string()))?;
    Ok([
        bilinear_weights(x, y, scale),
        bilinear_weights(x + 1, y, scale),
        bilinear_weights(x + 1, y + 1, scale),
        bilinear_weights(x, y + 1, scale),
        bilinear_weights(2 * x + 1, 2 * y + 1, 2 * scale),
    ])
}

fn bilinear_weights(u: u64, v: u64, scale: u64) -> [u64; 4] {
    [
        (scale - u) * (scale - v),
        u * (scale - v),
        u * v,
        (scale - u) * v,
    ]
}

fn blend(vertices: [Vertex; 4], weights: [u64; 4], value: impl Fn(Vertex) -> f64) -> f64 {
    let denominator = weights.iter().sum::<u64>() as f64;
    vertices
        .into_iter()
        .zip(weights)
        .map(|(vertex, weight)| value(vertex) * weight as f64)
        .sum::<f64>()
        / denominator
}

fn blend_vector(
    vertices: [Vertex; 4],
    weights: [u64; 4],
    value: impl Fn(Vertex) -> [f64; 3],
) -> [f64; 3] {
    std::array::from_fn(|axis| blend(vertices, weights, |vertex| value(vertex)[axis]))
}

fn category_fraction(
    vertices: [Vertex; 4],
    weights: [u64; 4],
    predicate: impl Fn(Vertex) -> bool,
) -> f64 {
    blend(vertices, weights, |vertex| f64::from(predicate(vertex)))
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
    use super::{SurfaceConfiguration, surface_revision};

    #[test]
    fn changing_consumed_calibration_changes_configuration_hash_and_revision() {
        let baseline = SurfaceConfiguration::current();
        let mut changed = baseline;
        changed.flow_half_saturation += 1.0;

        let baseline_revision = surface_revision("canonical-world-bytes", baseline);
        let changed_revision = surface_revision("canonical-world-bytes", changed);

        assert_ne!(
            baseline_revision.configuration_hash,
            changed_revision.configuration_hash
        );
        assert_ne!(baseline_revision, changed_revision);
    }
}
