//! Source-grid materials. Colors are presentation; elevation is never amplified.
// Clients are outside the simulation determinism boundary (clients/CLAUDE.md).
#![allow(
    clippy::disallowed_methods,
    reason = "client mesh and material math must not import the simulation kernel"
)]
use crate::documents::{Moon, Tiles};
use bevy::{
    asset::RenderAssetUsages,
    mesh::{Indices, PrimitiveTopology},
    prelude::*,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
};
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
