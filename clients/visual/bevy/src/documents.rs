//! Strict typed edges around additive native scene documents.
use crate::{Binding, ViewError};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeSet;

#[derive(Clone, Debug, Deserialize)]
pub struct Initial {
    pub schema: String,
    pub binding: Binding,
    pub system: Value,
    pub moons: Moons,
    pub tiles: Tiles,
    pub ticks_per_std_day: i64,
}
#[derive(Clone, Debug, Deserialize)]
pub struct Moons {
    pub schema: String,
    pub seed: u64,
    pub moons: Vec<Moon>,
}
#[derive(Clone, Debug, Deserialize)]
pub struct Moon {
    pub index: u32,
    pub radius_km: f64,
    pub albedo: f64,
    pub cratering: f64,
    pub maria_fraction: f64,
    pub tint: [f64; 3],
}
#[derive(Clone, Debug, Deserialize)]
pub struct Tiles {
    pub schema: String,
    pub seed: u64,
    pub width: u32,
    pub height: u32,
    pub sea_level_m: f64,
    pub elevation_m: Vec<f64>,
    pub ocean: Vec<bool>,
    pub biome: Vec<usize>,
    pub biome_legend: Vec<String>,
    pub snow_fraction: Vec<f64>,
    pub cloud_fraction: Vec<f64>,
    pub moisture: Vec<f64>,
    pub t_mean_c: Vec<f64>,
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Body {
    pub id: String,
    pub kind: String,
    pub position_km: [f64; 3],
    pub radius_km: Option<f64>,
    pub body_to_frame: Option<[[f64; 3]; 3]>,
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Light {
    pub star_id: String,
    pub direction_from_anchor: [f64; 3],
    pub flux_rel: f64,
    pub luminosity_rel: f64,
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Astronomy {
    pub schema: String,
    pub seed: u64,
    pub ticks: i64,
    pub ticks_per_std_day: i64,
    pub frame: String,
    pub models: Vec<String>,
    pub bodies: Vec<Body>,
    pub lights: Vec<Light>,
}
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Reply {
    pub schema: String,
    pub binding: Binding,
    pub request_id: u64,
    pub ticks: i64,
    pub astronomy: Astronomy,
}
fn check(ok: bool, message: &str) -> Result<(), ViewError> {
    if ok {
        Ok(())
    } else {
        Err(ViewError::Document(message.into()))
    }
}
fn positive(v: f64) -> bool {
    v.is_finite() && v > 0.0
}
pub fn initial(json: &str) -> Result<Initial, ViewError> {
    let v: Initial = serde_json::from_str(json)?;
    check(v.schema == "visual/initial/v1", "unknown initial schema")?;
    v.binding.validate()?;
    check(v.ticks_per_std_day > 0, "invalid ticks_per_std_day")?;
    check(
        v.system["schema"] == "scene/system/v1"
            && v.system["world"].is_object()
            && v.system["stellar"].is_object(),
        "incomplete system",
    )?;
    check(
        matches!(
            v.system["stellar"]["topology"].as_str(),
            Some("single" | "wide-binary" | "close-binary")
        ),
        "unknown stellar topology",
    )?;
    check(v.moons.schema == "scene/moons/v1", "unknown moons schema")?;
    check(
        v.system["seed"].as_u64() == Some(v.tiles.seed) && v.tiles.seed == v.moons.seed,
        "initial document seed mismatch",
    )?;
    check(
        v.system["moons"].is_array()
            && v.system["wanderers"].is_array()
            && v.system["stellar"]["primary"].is_object(),
        "incomplete system inventory",
    )?;
    let mut ids = BTreeSet::new();
    for m in &v.moons.moons {
        check(
            ids.insert(m.index)
                && positive(m.radius_km)
                && [m.albedo, m.cratering, m.maria_fraction]
                    .iter()
                    .chain(m.tint.iter())
                    .all(|x| x.is_finite() && (0.0..=1.0).contains(x)),
            "invalid moon descriptor",
        )?;
    }
    let t = &v.tiles;
    check(
        t.schema == "scene/tiles/v1"
            && t.width >= 16
            && t.width <= 1024
            && t.height.checked_mul(2) == Some(t.width),
        "invalid tile schema or projection dimensions",
    )?;
    let n = t.width as usize * t.height as usize;
    check(
        [
            t.elevation_m.len(),
            t.ocean.len(),
            t.biome.len(),
            t.snow_fraction.len(),
            t.cloud_fraction.len(),
            t.moisture.len(),
            t.t_mean_c.len(),
        ]
        .iter()
        .all(|x| *x == n),
        "incomplete tile arrays",
    )?;
    check(
        t.sea_level_m.is_finite()
            && t.elevation_m.iter().all(|x| x.is_finite())
            && t.moisture.iter().chain(&t.t_mean_c).all(|x| x.is_finite())
            && t.biome.iter().all(|x| *x < t.biome_legend.len())
            && t.snow_fraction
                .iter()
                .chain(&t.cloud_fraction)
                .all(|x| x.is_finite() && (0.0..=1.0).contains(x)),
        "invalid tile values",
    )?;
    Ok(v)
}
pub fn reply(json: &str) -> Result<Reply, ViewError> {
    // Option alone treats missing as null; the wire contract requires explicit nulls.
    let raw: Value = serde_json::from_str(json)?;
    if let Some(bodies) = raw.pointer("/astronomy/bodies").and_then(Value::as_array) {
        for b in bodies {
            check(
                b.get("radius_km").is_some() && b.get("body_to_frame").is_some(),
                "missing explicit optional body fields",
            )?;
        }
    }
    let v: Reply = serde_json::from_value(raw)?;
    v.binding.validate()?;
    check(
        v.schema == "visual/reply/v1" && v.astronomy.schema == "scene/astronomy-at/v1",
        "unknown reply schema",
    )?;
    let a = &v.astronomy;
    check(
        a.frame == "anchor-centered-system-plane/km/right-handed",
        "unsupported coordinate frame or units",
    )?;
    check(
        a.ticks == v.ticks && a.ticks_per_std_day > 0 && !a.models.is_empty(),
        "inconsistent astronomy identity",
    )?;
    let mut ids = BTreeSet::new();
    for b in &a.bodies {
        check(ids.insert(&b.id), "duplicate body ID")?;
        check(
            b.position_km.iter().all(|x| x.is_finite()),
            "nonfinite position",
        )?;
        check(
            match b.kind.as_str() {
                "anchor" => {
                    b.id == "anchor"
                        && b.body_to_frame.is_some()
                        && b.radius_km.is_some()
                        && b.position_km == [0.0; 3]
                }
                "moon" => {
                    b.id.strip_prefix("moon:")
                        .is_some_and(|x| x.parse::<u32>().is_ok())
                        && b.radius_km.is_some()
                }
                "star" | "wanderer" => {
                    b.id.strip_prefix(&format!("{}:", b.kind))
                        .is_some_and(|x| x.parse::<u32>().is_ok())
                        && b.radius_km.is_none()
                }
                _ => false,
            },
            "invalid body kind, ID, or unavailable radius",
        )?;
        check(b.radius_km.is_none_or(positive), "invalid body radius")?;
        if let Some(cols) = b.body_to_frame {
            let [x, y, z] = cols;
            let determinant = x[0] * (y[1] * z[2] - y[2] * z[1])
                - y[0] * (x[1] * z[2] - x[2] * z[1])
                + z[0] * (x[1] * y[2] - x[2] * y[1]);
            check(
                (determinant - 1.0).abs() < 1e-5,
                "body basis is not right handed",
            )?;
            for (i, c) in cols.iter().enumerate() {
                check(
                    c.iter().all(|x| x.is_finite())
                        && (c.iter().map(|x| x * x).sum::<f64>() - 1.0).abs() < 1e-5,
                    "invalid body basis",
                )?;
                for d in &cols[..i] {
                    check(
                        c.iter().zip(d).map(|(x, y)| x * y).sum::<f64>().abs() < 1e-5,
                        "nonorthogonal body basis",
                    )?;
                }
            }
        }
    }
    check(ids.contains(&"anchor".to_string()), "missing anchor")?;
    let mut lights = BTreeSet::new();
    for l in &a.lights {
        check(
            lights.insert(&l.star_id)
                && a.bodies
                    .iter()
                    .any(|b| b.id == l.star_id && b.kind == "star")
                && positive(l.flux_rel)
                && positive(l.luminosity_rel)
                && l.direction_from_anchor.iter().all(|x| x.is_finite())
                && (l.direction_from_anchor.iter().map(|x| x * x).sum::<f64>() - 1.0).abs() < 1e-5,
            "invalid light",
        )?;
    }
    check(!a.lights.is_empty(), "missing source illumination")?;
    Ok(v)
}
