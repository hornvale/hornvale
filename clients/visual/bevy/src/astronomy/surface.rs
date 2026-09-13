//! Source-grid materials. Colors are presentation; elevation is never amplified.
// Clients are outside the simulation determinism boundary (clients/CLAUDE.md).
#![allow(
    clippy::disallowed_methods,
    reason = "client mesh and material math must not import the simulation kernel"
)]
use crate::documents::{Moon, SurfacePatchDocument, SurfacePatchFeature, SurfacePatchStrip, Tiles};
use bevy::{
    asset::RenderAssetUsages,
    mesh::{Indices, MeshVertexAttribute, PrimitiveTopology},
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat, VertexFormat},
};

/// Source-owned directional channels retained in the render mesh. These are
/// direct data attributes; the client does not reconstruct terrain semantics.
pub const ATTRIBUTE_FLOW_DIRECTION: MeshVertexAttribute =
    MeshVertexAttribute::new("Surface_FlowDirection", 8, VertexFormat::Float32x3);
pub const ATTRIBUTE_RIDGE_DIRECTION: MeshVertexAttribute =
    MeshVertexAttribute::new("Surface_RidgeDirection", 9, VertexFormat::Float32x3);
pub const ATTRIBUTE_FEATURE_SIDE: MeshVertexAttribute =
    MeshVertexAttribute::new("Surface_FeatureSide", 10, VertexFormat::Float32);
pub const ATTRIBUTE_FEATURE_DISTANCE: MeshVertexAttribute =
    MeshVertexAttribute::new("Surface_FeatureDistance", 11, VertexFormat::Float32);
fn image(width: u32, height: u32, bytes: Vec<u8>) -> Image {
    Image::new(
        Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        bytes,
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::all(),
    )
}
fn mix(a: [f32; 3], b: [f32; 3], t: f32) -> [f32; 3] {
    std::array::from_fn(|i| a[i] * (1. - t) + b[i] * t)
}
/// Bilinear reconstruction of continuous source fields at their pixel centers.
/// Longitude wraps; latitude clamps at the pole. No category IDs are averaged.
pub fn sample(t: &Tiles, field: &[f64], u: f64, v: f64) -> f64 {
    let x = u * t.width as f64 - 0.5;
    let y = (v * t.height as f64 - 0.5).clamp(0.0, t.height as f64 - 1.0);
    let x0 = x.floor() as i64;
    let y0 = y.floor() as usize;
    let ax = x - x.floor();
    let ay = y - y.floor();
    let at = |dx: i64, dy: usize| {
        field[(y0 + dy).min(t.height as usize - 1) * t.width as usize
            + (x0 + dx).rem_euclid(t.width as i64) as usize]
    };
    (at(0, 0) * (1.0 - ax) + at(1, 0) * ax) * (1.0 - ay)
        + (at(0, 1) * (1.0 - ax) + at(1, 1) * ax) * ay
}
/// A convex 3×3 reconstruction kernel suppresses nearest-vertex raster steps.
/// It smooths only continuous source fields; it creates no new extrema or detail.
fn filter_field(t: &Tiles, field: &[f64]) -> Vec<f64> {
    let mut result = Vec::with_capacity(field.len());
    let w = t.width as i64;
    let h = t.height as i64;
    for y in 0..h {
        for x in 0..w {
            let mut value = 0.0;
            for dy in -1_i64..=1 {
                for dx in -1_i64..=1 {
                    let weight = if dx == 0 { 2.0 } else { 1.0 } * if dy == 0 { 2.0 } else { 1.0 };
                    value += field
                        [((y + dy).clamp(0, h - 1) * w + (x + dx).rem_euclid(w)) as usize]
                        * weight
                        / 16.0;
                }
            }
            result.push(value);
        }
    }
    result
}
fn reconstructed(t: &Tiles) -> Tiles {
    let mut result = t.clone();
    result.elevation_m = filter_field(t, &t.elevation_m);
    result.moisture = filter_field(t, &t.moisture);
    result.t_mean_c = filter_field(t, &t.t_mean_c);
    result.snow_fraction = filter_field(t, &t.snow_fraction);
    result
}
pub fn anchor_texture(t: &Tiles) -> Image {
    let t = reconstructed(t);
    let t = &t;
    let sea_ice: Vec<f64> = t
        .biome
        .iter()
        .map(|b| {
            if t.biome_legend[*b] == "sea-ice" {
                1.0
            } else {
                0.0
            }
        })
        .collect();
    let ocean_mask: Vec<f64> = t.ocean.iter().map(|o| if *o { 1.0 } else { 0.0 }).collect();
    // Extend conditional sea-ice coverage using only nearby valid ocean samples.
    // Land's absent ice classification must not become dark water at a rebuilt coast.
    let sea_ice = filter_field(t, &sea_ice);
    let ocean_mask = filter_field(t, &ocean_mask);
    let (w, h) = (2048, 1024);
    let mut pixels = Vec::with_capacity(w * h * 4);
    for y in 0..h {
        for x in 0..w {
            let u = (x as f64 + 0.5) / w as f64;
            let v = (y as f64 + 0.5) / h as f64;
            let altitude = sample(t, &t.elevation_m, u, v) - t.sea_level_m;
            let moisture = sample(t, &t.moisture, u, v).clamp(0.0, 1.0) as f32;
            let snow = sample(t, &t.snow_fraction, u, v).clamp(0.0, 1.0) as f32;
            let temperature = sample(t, &t.t_mean_c, u, v) as f32;
            let land = mix([0.42, 0.32, 0.17], [0.12, 0.24, 0.105], moisture.sqrt());
            let land = mix(
                land,
                [0.36, 0.36, 0.31],
                ((altitude as f32 - 2500.0) / 4500.0).clamp(0.0, 0.7),
            );
            let land = mix(land, [0.73, 0.81, 0.84], snow);
            let depth = (-altitude).max(0.0) as f32;
            let ocean = mix(
                [0.018, 0.29, 0.31],
                [0.006, 0.035, 0.10],
                (depth / 1800.0).clamp(0.0, 1.0).sqrt(),
            );
            // Sea-ice remains the exported categorical coverage. Only its color is
            // reconstructed; no interpolation of the biome identifiers themselves.
            let denominator = sample(t, &ocean_mask, u, v);
            let ice = if denominator > 1e-8 {
                (sample(t, &sea_ice, u, v) / denominator).clamp(0.0, 1.0) as f32
            } else {
                0.0
            };
            let ice_color = mix(
                [0.28, 0.46, 0.51],
                [0.73, 0.81, 0.84],
                ((-temperature - 1.0) / 20.0).clamp(0.0, 1.0),
            );
            let ocean = mix(ocean, ice_color, ice);
            let coverage = ((altitude as f32 + 30.0) / 60.0).clamp(0.0, 1.0);
            let c = mix(ocean, land, coverage);
            // Stable cosmetic pigment grain, bounded to ±12%; never relief or a source fact.
            let lon = u * std::f64::consts::TAU;
            let lat = (0.5 - v) * std::f64::consts::PI;
            let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let grain = 0.55 * noise(p.map(|v| v * 95.), 42)
                + 0.3 * noise(p.map(|v| v * 240.), 43)
                + 0.15 * noise(p.map(|v| v * 510.), 44);
            let pigment = 1.0
                + (grain as f32 - 0.5)
                    * (2. * crate::camera::ViewSettings::default().pigment_variation)
                    * coverage;
            let c = c.map(|v| v * pigment);
            pixels.extend(c.map(|x| (x.clamp(0.0, 1.0) * 255.0).round() as u8));
            pixels.push(255);
        }
    }
    image(w as u32, h as u32, pixels)
}
/// Source-water coverage controls a presentation roughness map; metalness is zero.
pub fn anchor_roughness(t: &Tiles) -> Image {
    let t = reconstructed(t);
    let (w, h) = (2048, 1024);
    let mut pixels = Vec::with_capacity(w * h * 4);
    for y in 0..h {
        for x in 0..w {
            let u = (x as f64 + 0.5) / w as f64;
            let v = (y as f64 + 0.5) / h as f64;
            let elevation = sample(&t, &t.elevation_m, u, v) - t.sea_level_m;
            let snow = sample(&t, &t.snow_fraction, u, v).clamp(0.0, 1.0);
            let coverage = ((elevation + 30.0) / 60.0).clamp(0.0, 1.0);
            let roughness = (f64::from(crate::camera::ViewSettings::default().water_roughness)
                * (1.0 - coverage)
                + 0.86 * coverage)
                * (1.0 - snow)
                + 0.65 * snow;
            pixels.extend([255, (roughness * 255.0).round() as u8, 0, 255]);
        }
    }
    let mut result = image(w as u32, h as u32, pixels);
    result.texture_descriptor.format = TextureFormat::Rgba8Unorm;
    result
}
/// Own Z-north mesh: U=0 is longitude -180°, V=0 is north.
pub fn globe_mesh(t: &Tiles, radius_km: f64, km_per_unit: f64) -> Mesh {
    let t = reconstructed(t);
    let t = &t;
    let w = t.width as usize;
    let h = t.height as usize;
    let mut positions = Vec::new();
    let mut normals = Vec::new();
    let mut uv = Vec::new();
    let mut indices = Vec::new();
    for y in 0..=h {
        for x in 0..=w {
            let lon = (x as f64 / w as f64 * 2. - 1.) * std::f64::consts::PI;
            let lat = (0.5 - y as f64 / h as f64) * std::f64::consts::PI;
            let n = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let relief = (sample(t, &t.elevation_m, x as f64 / w as f64, y as f64 / h as f64)
                - t.sea_level_m)
                .max(0.0)
                / 1000.0;
            positions.push(n.map(|v| (v * (radius_km + relief) / km_per_unit) as f32));
            // Derivatives of the very same source relief, with no elevation multiplier.
            let u = x as f64 / w as f64;
            let v = y as f64 / h as f64;
            let du = 1. / w as f64;
            let dv = 1. / h as f64;
            let height = |u, v| ((sample(t, &t.elevation_m, u, v) - t.sea_level_m).max(0.)) / 1000.;
            let east = Vec3::new(-lon.sin() as f32, lon.cos() as f32, 0.);
            let north = Vec3::new(
                (-lat.sin() * lon.cos()) as f32,
                (-lat.sin() * lon.sin()) as f32,
                lat.cos() as f32,
            );
            let dx = (height(u + du, v) - height(u - du, v))
                / (2.
                    * du
                    * std::f64::consts::TAU
                    * (radius_km + relief)
                    * lat.cos().abs().max(1e-6));
            let dy = (height(u, (v - dv).max(0.)) - height(u, (v + dv).min(1.)))
                / (2. * dv * std::f64::consts::PI * (radius_km + relief));
            normals.push(
                (Vec3::from_array(n.map(|v| v as f32)) - east * dx as f32 - north * dy as f32)
                    .normalize()
                    .to_array(),
            );
            uv.push([x as f32 / w as f32, y as f32 / h as f32]);
        }
    }
    for y in 0..h {
        for x in 0..w {
            let a = (y * (w + 1) + x) as u32;
            let b = a + (w + 1) as u32;
            indices.extend([a, b, a + 1, a + 1, b, b + 1]);
        }
    }
    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::all())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uv)
        .with_inserted_indices(Indices::U32(indices))
}

/// Convert a source-owned facet document into a render mesh. Heights and
/// normals come from the document; the small unit scale keeps this helper
/// independent of the body's physical radius, which is applied by the caller.
pub fn surface_mesh(
    patch: &SurfacePatchDocument,
    transition: Option<&SurfacePatchDocument>,
) -> Mesh {
    let mut vertices = patch
        .vertices
        .iter()
        .map(|vertex| (vertex, patch.features.as_slice()))
        .collect::<Vec<_>>();
    let indices = if !patch.transition_triangles.is_empty() {
        patch
            .transition_triangles
            .iter()
            .flatten()
            .copied()
            .collect()
    } else if let Some(document) = transition {
        let candidate = if document.transition_triangles.is_empty() {
            &document.triangles
        } else {
            &document.transition_triangles
        };
        if candidate.is_empty() {
            patch.triangles.iter().flatten().copied().collect()
        } else {
            let mut remapped = Vec::with_capacity(candidate.len() * 3);
            for index in candidate.iter().flatten().copied() {
                let vertex = &document.vertices[index as usize];
                let mapped = vertices
                    .iter()
                    .position(|(candidate, _)| candidate.position == vertex.position)
                    .unwrap_or_else(|| {
                        vertices.push((vertex, document.features.as_slice()));
                        vertices.len() - 1
                    });
                remapped.push(mapped as u32);
            }
            remapped
        }
    } else {
        patch.triangles.iter().flatten().copied().collect()
    };
    let positions = vertices
        .iter()
        .map(|(vertex, _)| render_position(vertex))
        .collect::<Vec<_>>();
    let normals = vertices
        .iter()
        .map(|(vertex, _)| vertex.normal.map(|value| value as f32))
        .collect::<Vec<_>>();
    let uv = vertices
        .iter()
        .map(|(vertex, _)| render_uv(vertex))
        .collect::<Vec<_>>();
    let colors = vertices
        .iter()
        .map(|(vertex, features)| {
            let feature_mask = features
                .iter()
                .map(|feature| feature_mask(features, feature, vertex.position.map(|v| v as f32)))
                .fold(0.0, f32::max);
            material_color(source_material_weights(vertex, feature_mask))
        })
        .collect::<Vec<_>>();
    let flow_direction = vertices
        .iter()
        .map(|(vertex, _)| vertex.flow_direction.map(|value| value as f32))
        .collect::<Vec<_>>();
    let ridge_direction = vertices
        .iter()
        .map(|(vertex, _)| vertex.ridge_direction.map(|value| value as f32))
        .collect::<Vec<_>>();
    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::all())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uv)
        .with_inserted_attribute(Mesh::ATTRIBUTE_COLOR, colors)
        .with_inserted_attribute(ATTRIBUTE_FLOW_DIRECTION, flow_direction)
        .with_inserted_attribute(ATTRIBUTE_RIDGE_DIRECTION, ridge_direction)
        .with_inserted_indices(Indices::U32(indices))
}

/// Convert one validated source-owned strip into independent ribbon geometry.
pub fn feature_strip_mesh(strip: &SurfacePatchStrip) -> Mesh {
    let positions = strip
        .vertices
        .iter()
        .map(|vertex| {
            let direction = Vec3::from_array(vertex.position.map(|value| value as f32));
            (direction * (1.0 + vertex.height_m as f32 * 1e-6).max(0.001)).to_array()
        })
        .collect::<Vec<_>>();
    let normals = strip
        .vertices
        .iter()
        .map(|vertex| vertex.normal.map(|value| value as f32))
        .collect::<Vec<_>>();
    let sides = strip
        .vertices
        .iter()
        .map(|vertex| f32::from(vertex.side))
        .collect::<Vec<_>>();
    let distances = strip
        .vertices
        .iter()
        .map(|vertex| vertex.signed_distance_rad as f32)
        .collect::<Vec<_>>();
    let color = material_color(strip.semantic_mask);
    let colors = vec![color; strip.vertices.len()];
    Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::all())
        .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
        .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
        .with_inserted_attribute(Mesh::ATTRIBUTE_COLOR, colors)
        .with_inserted_attribute(ATTRIBUTE_FEATURE_SIDE, sides)
        .with_inserted_attribute(ATTRIBUTE_FEATURE_DISTANCE, distances)
        .with_inserted_indices(Indices::U32(
            strip.triangles.iter().flatten().copied().collect(),
        ))
}

/// Presentation material driven only by the strip's source semantic mask.
pub fn feature_strip_material(strip: &SurfacePatchStrip) -> StandardMaterial {
    let color = material_color(strip.semantic_mask);
    StandardMaterial {
        base_color: Color::linear_rgba(color[0], color[1], color[2], color[3]),
        perceptual_roughness: 0.45,
        reflectance: 0.04,
        ..default()
    }
}

/// Quantized presentation color used to identify pixels contributed by a
/// source-owned feature ribbon in rendered review evidence.
pub fn feature_strip_color(strip: &SurfacePatchStrip) -> [u8; 3] {
    let color = material_color(strip.semantic_mask);
    std::array::from_fn(|index| (color[index] * 255.0).round() as u8)
}

fn render_position(vertex: &crate::documents::SurfacePatchVertex) -> [f32; 3] {
    let position = Vec3::from_array(vertex.position.map(|value| value as f32));
    let height = (1.0 + vertex.height_m as f32 * 1e-6).max(0.001);
    (position * height).to_array()
}

fn render_uv(vertex: &crate::documents::SurfacePatchVertex) -> [f32; 2] {
    let p = vertex.position;
    [
        (p[1].atan2(p[0]) / std::f64::consts::TAU + 0.5) as f32,
        (0.5 - p[2].clamp(-1.0, 1.0).asin() / std::f64::consts::PI) as f32,
    ]
}

/// Consume the continuous source material weights as one presentation material.
/// No biome category or semantic feature is inferred in the client.
pub fn surface_material(patch: &SurfacePatchDocument) -> StandardMaterial {
    let mut weights = [0.0_f64; 8];
    let mut water = 0.0;
    let mut semantic_roughness = 0.0;
    for vertex in &patch.vertices {
        let feature_mask = patch
            .features
            .iter()
            .map(|feature| narrow_feature_mask(patch, feature, vertex.position.map(|v| v as f32)))
            .fold(0.0, f32::max) as f64;
        for (sum, value) in weights
            .iter_mut()
            .zip(source_material_weights(vertex, feature_mask as f32))
        {
            *sum += value;
        }
        water += vertex.water_depth_m.max(0.0);
        semantic_roughness += vertex.bank_weight
            + vertex.terrace_weight
            + vertex.ridge_strength
            + vertex.shoreline_distance_m.abs().min(1000.0) / 1000.0;
    }
    let count = patch.vertices.len().max(1) as f64;
    let color = material_color(weights.map(|value| value / count));
    StandardMaterial {
        base_color: Color::linear_rgba(color[0], color[1], color[2], color[3]),
        perceptual_roughness: (0.92 - (water / count / 500.0).clamp(0.0, 0.55)
            + (semantic_roughness / count * 0.01).clamp(0.0, 0.04))
        .clamp(0.0, 1.0) as f32,
        reflectance: 0.04,
        ..default()
    }
}

fn source_material_weights(
    vertex: &crate::documents::SurfacePatchVertex,
    feature_mask: f32,
) -> [f64; 8] {
    let mut weights = vertex.material_weights;
    let channel = if vertex.channel_width_m > 0.0 {
        (1.0 - vertex.channel_distance_m.abs() / vertex.channel_width_m).clamp(0.0, 1.0)
    } else {
        0.0
    };
    weights[2] += (vertex.floodplain_weight + vertex.delta_weight) * 0.25;
    weights[3] += vertex.terrace_weight * 0.15;
    weights[4] +=
        f64::from((feature_mask + channel as f32 * vertex.flow_strength as f32).clamp(0.0, 1.0));
    weights[5] += (vertex.water_depth_m / 500.0).clamp(0.0, 1.0);
    weights[6] += vertex.ridge_strength * 0.2;
    weights[7] += vertex.bank_weight * 0.1;
    weights
}

fn material_color(weights: [f64; 8]) -> [f32; 4] {
    let palette = [
        [0.36, 0.28, 0.16],
        [0.18, 0.38, 0.16],
        [0.50, 0.46, 0.25],
        [0.63, 0.58, 0.48],
        [0.03, 0.22, 0.38],
        [0.26, 0.56, 0.58],
        [0.58, 0.48, 0.28],
        [0.72, 0.74, 0.70],
    ];
    let total = weights.iter().copied().sum::<f64>().max(1e-12);
    let color: [f32; 3] = std::array::from_fn(|channel| {
        (weights
            .iter()
            .zip(palette)
            .map(|(weight, color)| weight.max(0.0) * color[channel])
            .sum::<f64>()
            / total) as f32
    });
    [color[0], color[1], color[2], 1.0]
}

/// Return a bounded source-owned curve footprint at a render position.
/// Distance is measured against every curve segment, so a feature remains
/// visible even when no sampled patch vertex lies on its centerline.
pub fn narrow_feature_mask(
    patch: &SurfacePatchDocument,
    feature: &SurfacePatchFeature,
    position: [f32; 3],
) -> f32 {
    feature_mask(&patch.features, feature, position)
}

fn feature_mask(
    features: &[SurfacePatchFeature],
    feature: &SurfacePatchFeature,
    position: [f32; 3],
) -> f32 {
    if !features.iter().any(|candidate| candidate == feature) {
        return 0.0;
    }
    let position = Vec3::from_array(position);
    let mut best = 0.0_f32;
    for (index, segment) in feature.points.windows(2).enumerate() {
        let start = Vec3::from_array(segment[0].map(|value| value as f32));
        let end = Vec3::from_array(segment[1].map(|value| value as f32));
        let delta = end - start;
        let denominator = delta.length_squared();
        let along = if denominator > f32::EPSILON {
            ((position - start).dot(delta) / denominator).clamp(0.0, 1.0)
        } else {
            0.0
        };
        let distance = position.distance(start + delta * along);
        let width = feature.width_rad[index].max(1e-6) as f32;
        best = best.max((1.0 - distance / width).clamp(0.0, 1.0));
    }
    best
}
/// Smooth, seeded presentation noise. Never used for physical positions or relief.
fn noise(p: [f64; 3], seed: u32) -> f64 {
    let base = p.map(|v| v.floor() as i32);
    let f = p.map(|v| {
        let t = v - v.floor();
        t * t * (3.0 - 2.0 * t)
    });
    let mut value = 0.0;
    for z in 0..2 {
        for y in 0..2 {
            for x in 0..2 {
                let mut hash = seed ^ ((base[0] + x) as u32).wrapping_mul(0x9e3779b9);
                hash ^= ((base[1] + y) as u32).wrapping_mul(0x85ebca6b);
                hash ^= ((base[2] + z) as u32).wrapping_mul(0xc2b2ae35);
                hash ^= hash >> 16;
                hash = hash.wrapping_mul(0x7feb352d);
                hash ^= hash >> 15;
                let weight = (if x == 0 { 1.0 - f[0] } else { f[0] })
                    * (if y == 0 { 1.0 - f[1] } else { f[1] })
                    * (if z == 0 { 1.0 - f[2] } else { f[2] });
                value += f64::from(hash & 0xffffff) / 16777215.0 * weight;
            }
        }
    }
    value
}
/// Cosmetic, body-fixed albedo variation only: no invented moon relief or spin.
pub fn moon_texture(m: &Moon) -> Image {
    let (w, h) = (1024, 512);
    let mut bytes = Vec::with_capacity(w * h * 4);
    for y in 0..h {
        for x in 0..w {
            let lon = x as f64 / w as f64 * std::f64::consts::TAU;
            let lat = (0.5 - y as f64 / h as f64) * std::f64::consts::PI;
            let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let mut detail = 0.0;
            let mut amplitude = 0.5;
            let mut frequency = 8.0;
            for octave in 0..6 {
                detail += amplitude * noise(p.map(|v| v * frequency), m.index + octave + 81);
                amplitude *= 0.5;
                frequency *= 2.0;
            }
            let maria = noise(p.map(|v| v * 3.5), m.index + 73);
            let coverage = ((maria - (1.0 - m.maria_fraction)) * 2.4 + 0.5).clamp(0.0, 1.0);
            let value =
                m.albedo * (1.2 - 0.45 * coverage) * (0.65 + detail * (0.5 + 0.25 * m.cratering));
            for tint in m.tint {
                let linear = (value * tint).clamp(0.0, 1.0);
                let srgb = if linear <= 0.0031308 {
                    linear * 12.92
                } else {
                    1.055 * linear.powf(1.0 / 2.4) - 0.055
                };
                bytes.push((srgb * 255.0).round() as u8);
            }
            bytes.push(255);
        }
    }
    image(w as u32, h as u32, bytes)
}

/// A static presentation layer conditioned by exported cloud coverage. The source
/// does not resolve cloud shapes or altitude: this seeded texture is cosmetic.
pub fn cloud_texture(t: &Tiles) -> Image {
    let (w, h) = (2048, 1024);
    let mut bytes = Vec::with_capacity(w * h * 4);
    for y in 0..h {
        for x in 0..w {
            let u = (x as f64 + 0.5) / w as f64;
            let v = (y as f64 + 0.5) / h as f64;
            let lon = u * std::f64::consts::TAU;
            let lat = (0.5 - v) * std::f64::consts::PI;
            let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            let warp = [
                noise(p.map(|v| v * 9.), 152),
                noise(p.map(|v| v * 9. + 7.), 153),
                noise(p.map(|v| v * 9. - 3.), 154),
            ];
            let q = std::array::from_fn::<_, 3, _>(|i| p[i] * 15. + warp[i] * 0.7);
            let detail = 0.55 * noise(q, 142)
                + 0.3 * noise(q.map(|v| v * 2.), 143)
                + 0.15 * noise(q.map(|v| v * 4.), 144);
            let coverage = sample(t, &t.cloud_fraction, u, v).clamp(0., 1.);
            let opacity = ((detail - (0.65 - coverage * 0.3)) * 3.).clamp(0., 1.)
                * coverage
                * f64::from(crate::camera::ViewSettings::default().cloud_opacity);
            bytes.extend([218, 234, 238, (opacity * 255.).round() as u8]);
        }
    }
    image(w as u32, h as u32, bytes)
}
/// Nonphysical presentation shell; no terrain is moved and no body dimension changes.
pub fn cloud_shell(t: &Tiles, radius_km: f64, km_per_unit: f64) -> Mesh {
    let mut flat = t.clone();
    flat.elevation_m.fill(t.sea_level_m);
    globe_mesh(
        &flat,
        radius_km + f64::from(crate::camera::ViewSettings::default().cloud_shell_km),
        km_per_unit,
    )
}

/// Stable, source-cratering-conditioned cosmetic normal map. Crater positions,
/// sizes and slopes are presentation marks, not measured relief or geometry.
/// Sphere UV tangents are generated at the material binding; no vertex moves.
pub fn moon_normal(m: &Moon) -> Image {
    let (w, h) = (1024_usize, 512_usize);
    let craters: Vec<_> = (0..420)
        .map(|i| {
            let z = 1. - 2. * noise([i as f64 * 17., 9., 3.], m.index + 801);
            let angle = std::f64::consts::TAU * noise([i as f64 * 19., 1., 7.], m.index + 803);
            let center = [
                (1. - z * z).sqrt() * angle.cos(),
                (1. - z * z).sqrt() * angle.sin(),
                z,
            ];
            let size = 0.012 + 0.13 * noise(center.map(|v| v * 35.), m.index + 901).powi(2);
            (center, size)
        })
        .collect();
    let mut heights = vec![0.; w * h];
    for y in 0..h {
        for x in 0..w {
            let lon = (x as f64 + 0.5) / w as f64 * std::f64::consts::TAU;
            let lat = (0.5 - (y as f64 + 0.5) / h as f64) * std::f64::consts::PI;
            let p = [lat.cos() * lon.cos(), lat.cos() * lon.sin(), lat.sin()];
            for (center, size) in &craters {
                let d2: f64 = (0..3).map(|i| (p[i] - center[i]).powi(2)).sum();
                if d2 < size * size * 2.56 {
                    let irregularity = noise(p.map(|v| v * 5. / size), m.index + 905);
                    let r = d2.sqrt() / size * (1. + 0.10 * (irregularity - 0.5));
                    let bowl = if r < 1. {
                        -0.08 * size * (1. - r * r).powi(2)
                    } else {
                        0.
                    };
                    let rim = 0.012 * size * (-((r - 1.) / 0.22).powi(2)).exp();
                    let maria = noise(p.map(|v| v * 3.5), m.index + 73);
                    let coverage = ((maria - (1.0 - m.maria_fraction)) * 2.4 + 0.5).clamp(0., 1.);
                    heights[y * w + x] +=
                        (bowl + rim) * m.cratering.clamp(0., 1.).sqrt() * (1. - 0.5 * coverage);
                }
            }
        }
    }
    let mut bytes = Vec::with_capacity(w * h * 4);
    for y in 0..h {
        for x in 0..w {
            let lat = (0.5 - (y as f64 + 0.5) / h as f64) * std::f64::consts::PI;
            let dx = (heights[y * w + (x + 1) % w] - heights[y * w + (x + w - 1) % w])
                / (2. * std::f64::consts::TAU / w as f64 * lat.cos().max(0.01));
            let dy = (heights[(y + 1).min(h - 1) * w + x] - heights[y.saturating_sub(1) * w + x])
                / (2. * std::f64::consts::PI / h as f64);
            let normal = cosmetic_normal(dx, dy);
            bytes.extend(
                normal
                    .to_array()
                    .map(|v| ((v * 0.5 + 0.5) * 255.).round() as u8),
            );
            bytes.push(255);
        }
    }
    let mut result = image(w as u32, h as u32, bytes);
    result.texture_descriptor.format = TextureFormat::Rgba8Unorm;
    result
}
fn cosmetic_normal(dx: f64, dy: f64) -> Vec3 {
    Vec3::new(-dx as f32, -dy as f32, 1.).normalize()
}
#[cfg(test)]
mod crater_tests {
    use super::*;
    #[test]
    fn cosmetic_normals_tilt_against_increasing_texture_height() {
        assert!(cosmetic_normal(0., 1.).y < 0.);
        assert!(cosmetic_normal(1., 0.).x < 0.);
    }
    #[test]
    fn cosmetic_normals_are_stable_and_conditioned_by_source_cratering() {
        let mut moon = Moon {
            index: 2,
            radius_km: 1000.,
            albedo: 0.1,
            cratering: 0.8,
            maria_fraction: 0.2,
            tint: [1.; 3],
        };
        let first = moon_normal(&moon);
        assert_eq!(first.data, moon_normal(&moon).data);
        moon.cratering = 0.;
        let flat = moon_normal(&moon);
        assert_ne!(first.data, flat.data);
        assert!(
            flat.data
                .unwrap()
                .chunks_exact(4)
                .all(|p| p == [128, 128, 255, 255])
        );
        assert_eq!(first.texture_descriptor.format, TextureFormat::Rgba8Unorm);
    }
}
