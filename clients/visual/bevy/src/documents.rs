//! Strict typed edges around additive native scene documents.
use crate::{Binding, ViewError, coordinates::render_radius};
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

/// The source-owned revision carried by one derived surface document.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchRevision {
    pub source_revision: String,
    pub algorithm_version: String,
    pub configuration_hash_hex: String,
}

/// Address of a rendered patch. This is a client cache address, not save data.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchAddress {
    pub macro_face: u32,
    pub child_path: Vec<u8>,
}

/// Stable identity of a source-owned semantic feature.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfaceFeatureId {
    pub kind: String,
    pub macro_anchor: u64,
    pub ordinal: u32,
}

/// Endpoint metadata is retained so the renderer never has to infer routing.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchEndpoint {
    pub feature: SurfaceFeatureId,
    pub side: String,
    pub boundary: Option<SurfacePatchBoundary>,
    pub terminal: String,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchBoundary {
    pub address: SurfacePatchAddress,
    pub edge: u8,
    pub t: f64,
}

/// One source-owned feature curve, including its narrow render footprint.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchFeature {
    pub feature: SurfaceFeatureId,
    pub points: Vec<[f64; 3]>,
    pub width_rad: Vec<f64>,
    pub endpoints: [SurfacePatchEndpoint; 2],
}

/// One source-owned surface sample. The renderer consumes these values as-is.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchVertex {
    pub position: [f64; 3],
    pub height_m: f64,
    pub normal: [f64; 3],
    pub material_weights: [f64; 8],
    pub shoreline_distance_m: f64,
    pub water_depth_m: f64,
    pub flow_direction: [f64; 3],
    pub flow_strength: f64,
    pub channel_distance_m: f64,
    pub channel_width_m: f64,
    pub floodplain_weight: f64,
    pub bank_weight: f64,
    pub terrace_weight: f64,
    pub delta_weight: f64,
    pub ridge_direction: [f64; 3],
    pub ridge_strength: f64,
}

/// Derived patch document used only at the renderer boundary.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchDocument {
    pub schema: String,
    pub revision: SurfacePatchRevision,
    pub address: SurfacePatchAddress,
    #[serde(rename = "samples", alias = "vertices")]
    pub vertices: Vec<SurfacePatchVertex>,
    #[serde(rename = "curves", alias = "features")]
    pub features: Vec<SurfacePatchFeature>,
    pub triangles: Vec<[u32; 3]>,
    /// Optional source-carried replacement topology for an unequal-LOD seam.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub transition_triangles: Vec<[u32; 3]>,
}

/// The complete source envelope for one surface observation.
#[derive(Clone, Debug, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfaceReplyDocument {
    pub schema: String,
    pub binding: Binding,
    pub request_id: u64,
    pub generation: u64,
    pub patch: SurfacePatchDocument,
}

#[derive(Clone, Debug, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct SurfaceRequestDocument {
    pub schema: String,
    pub binding: Binding,
    pub request_id: u64,
    pub generation: u64,
    pub address: SurfacePatchAddress,
    #[serde(default)]
    pub transition_address: Option<SurfacePatchAddress>,
    pub expected_revision: SurfacePatchRevision,
}

/// Full revision-qualified cache identity for a surface patch.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SurfacePatchCacheKey {
    pub revision: String,
    pub macro_face: u32,
    pub child_path: Vec<u8>,
}

fn valid_surface_number(value: f64) -> bool {
    value.is_finite()
}

const MAX_SURFACE_CHILD_DEPTH: usize = 23;
// The supported anchor-radius interval makes a planet's half-circumference
// smaller than this bound. Channel distance remains signed within that
// physical surface-distance envelope.
const MAX_CHANNEL_DISTANCE_M: f64 = 100_000_000.0;
const UNIT_TOLERANCE: f64 = 1.0e-5;
const WEIGHT_TOLERANCE: f64 = 1.0e-5;

fn valid_revision_component(value: &str, max_len: usize) -> bool {
    !value.is_empty()
        && value.len() <= max_len
        && value
            .bytes()
            .all(|byte| byte.is_ascii_graphic() && byte != b':')
}

fn valid_unit_vector(value: [f64; 3], allow_zero: bool) -> bool {
    let length = value
        .into_iter()
        .map(|component| component * component)
        .sum::<f64>()
        .sqrt();
    length.is_finite()
        && ((allow_zero && length <= UNIT_TOLERANCE) || (length - 1.0).abs() <= UNIT_TOLERANCE)
}

fn valid_weight(value: f64) -> bool {
    value.is_finite() && (0.0..=1.0).contains(&value)
}

fn valid_feature_kind(kind: &str) -> bool {
    matches!(
        kind,
        "channel_reach" | "confluence" | "shoreline" | "ridge" | "material_transition"
    )
}

fn valid_terminal(terminal: &str) -> bool {
    matches!(
        terminal,
        "headwater" | "confluence" | "lake" | "ocean" | "continuation"
    )
}

fn valid_endpoint(
    endpoint: &SurfacePatchEndpoint,
    feature: &SurfaceFeatureId,
    expected_side: &str,
) -> bool {
    endpoint.feature == *feature
        && endpoint.side == expected_side
        && valid_terminal(&endpoint.terminal)
        && endpoint.boundary.as_ref().is_none_or(|boundary| {
            valid_macro_face(boundary.address.macro_face)
                && boundary.edge < 4
                && (0.0..=1.0).contains(&boundary.t)
                && boundary.address.child_path.len() <= MAX_SURFACE_CHILD_DEPTH
                && boundary.address.child_path.iter().all(|digit| *digit <= 3)
        })
        && (endpoint.terminal == "continuation") == endpoint.boundary.is_some()
}

fn valid_macro_face(value: u32) -> bool {
    let face = value & 0x1f;
    let pathword = value >> 5;
    face < 6 && (0x1000..0x2000).contains(&pathword)
}

pub(crate) fn validate_surface_patch_revision(
    revision: &SurfacePatchRevision,
) -> Result<(), ViewError> {
    check(
        revision.source_revision.len() == 40
            && revision
                .source_revision
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
            && valid_revision_component(&revision.algorithm_version, 128)
            && revision.configuration_hash_hex.len() == 64
            && revision
                .configuration_hash_hex
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase()),
        "invalid surface patch revision",
    )
}

pub(crate) fn validate_surface_patch_address(
    address: &SurfacePatchAddress,
) -> Result<(), ViewError> {
    check(
        valid_macro_face(address.macro_face)
            && address.child_path.len() <= MAX_SURFACE_CHILD_DEPTH
            && address.child_path.iter().all(|digit| *digit <= 3),
        "invalid surface patch address",
    )
}

pub(crate) fn validate_surface_patch(document: &SurfacePatchDocument) -> Result<(), ViewError> {
    check(
        document.schema == "scene/surface/v1",
        "unknown surface patch schema",
    )?;
    check(
        !document.revision.source_revision.is_empty()
            && document.revision.source_revision.len() == 40
            && document
                .revision
                .source_revision
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase())
            && valid_revision_component(&document.revision.algorithm_version, 128)
            && document.revision.configuration_hash_hex.len() == 64
            && document
                .revision
                .configuration_hash_hex
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase()),
        "invalid surface patch revision",
    )?;
    check(
        document.address.child_path.len() <= MAX_SURFACE_CHILD_DEPTH
            && document.address.child_path.iter().all(|digit| *digit <= 3),
        "surface child path digit is outside 0..=3",
    )?;
    check(
        valid_macro_face(document.address.macro_face),
        "surface macro face is not a packed Level-6 facet",
    )?;
    check(
        !document.vertices.is_empty(),
        "surface patch has no samples",
    )?;
    check(
        document.vertices.iter().all(|vertex| {
            valid_unit_vector(vertex.position, false)
                && valid_unit_vector(vertex.normal, false)
                && vertex.material_weights.iter().copied().all(valid_weight)
                && (vertex.material_weights.iter().sum::<f64>() - 1.0).abs() <= WEIGHT_TOLERANCE
                && vertex.water_depth_m >= 0.0
                && vertex.channel_distance_m.abs() <= MAX_CHANNEL_DISTANCE_M
                && vertex.channel_width_m >= 0.0
                && vertex.flow_strength >= 0.0
                && vertex.flow_strength <= 1.0
                && valid_unit_vector(vertex.flow_direction, true)
                && valid_unit_vector(vertex.ridge_direction, true)
                && [
                    vertex.floodplain_weight,
                    vertex.bank_weight,
                    vertex.terrace_weight,
                    vertex.delta_weight,
                    vertex.ridge_strength,
                ]
                .into_iter()
                .all(valid_weight)
                && vertex.position.into_iter().all(valid_surface_number)
                && [
                    vertex.height_m,
                    vertex.shoreline_distance_m,
                    vertex.water_depth_m,
                    vertex.flow_strength,
                    vertex.channel_distance_m,
                    vertex.channel_width_m,
                    vertex.floodplain_weight,
                    vertex.bank_weight,
                    vertex.terrace_weight,
                    vertex.delta_weight,
                    vertex.ridge_strength,
                ]
                .into_iter()
                .all(valid_surface_number)
        }),
        "surface patch contains a non-finite sample",
    )?;
    check(
        document
            .triangles
            .iter()
            .chain(document.transition_triangles.iter())
            .all(|triangle| {
                triangle
                    .iter()
                    .all(|index| (*index as usize) < document.vertices.len())
                    && triangle[0] != triangle[1]
                    && triangle[1] != triangle[2]
                    && triangle[0] != triangle[2]
            }),
        "surface topology references an invalid or degenerate triangle",
    )?;
    check(
        document.features.iter().all(|feature| {
            valid_feature_kind(&feature.feature.kind)
                && feature.points.len() >= 2
                && feature.points.len() == feature.width_rad.len()
                && feature
                    .points
                    .iter()
                    .copied()
                    .all(|point| valid_unit_vector(point, false))
                && feature.width_rad.iter().all(|width| {
                    valid_surface_number(*width) && (0.0..=std::f64::consts::PI).contains(width)
                })
                && valid_endpoint(&feature.endpoints[0], &feature.feature, "upstream")
                && valid_endpoint(&feature.endpoints[1], &feature.feature, "downstream")
        }),
        "surface feature curve is incomplete or non-finite",
    )?;
    Ok(())
}

impl SurfacePatchDocument {
    pub fn cache_key(&self) -> SurfacePatchCacheKey {
        SurfacePatchCacheKey {
            revision: self.revision.cache_token(),
            macro_face: self.address.macro_face,
            child_path: self.address.child_path.clone(),
        }
    }
}

impl SurfacePatchRevision {
    /// A cache revision includes every source-owned revision component. The
    /// separator is outside the allowed hexadecimal/hash and revision fields,
    /// so distinct revisions cannot collapse to one cache entry.
    pub fn cache_token(&self) -> String {
        format!(
            "{}:{}{}:{}{}",
            self.source_revision.len(),
            self.source_revision,
            self.algorithm_version.len(),
            self.algorithm_version,
            self.configuration_hash_hex
        )
    }
}

/// Decode and validate one source-owned surface patch document.
pub fn surface_patch(json: &str) -> Result<SurfacePatchDocument, ViewError> {
    let document: SurfacePatchDocument = serde_json::from_str(json)?;
    validate_surface_patch(&document)?;
    Ok(document)
}

/// Decode and validate the complete source-owned surface reply envelope.
pub fn surface_reply(json: &str) -> Result<SurfaceReplyDocument, ViewError> {
    let reply: SurfaceReplyDocument = serde_json::from_str(json)?;
    check(
        reply.schema == "visual/surface-reply/v1",
        "unknown surface reply schema",
    )?;
    reply.binding.validate()?;
    check(
        reply.patch.revision.source_revision == reply.binding.source_revision,
        "surface reply patch revision does not match its binding",
    )?;
    validate_surface_patch(&reply.patch)?;
    Ok(reply)
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
    // Consumed catalog fields: topology, native ordered body inventory and moon
    // appearance. Orbital elements, stellar class/luminosity and world calendar
    // fields remain source provenance; this view never evaluates them.
    let secondary = v.system["stellar"].get("companion");
    check(
        if v.system["stellar"]["topology"] == "single" {
            secondary.is_none_or(Value::is_null)
        } else {
            secondary.is_some_and(|c| c["star"].is_object() && c["orbit"].is_object())
        },
        "stellar secondary disagrees with topology",
    )?;
    check(
        v.system["moons"]
            .as_array()
            .is_some_and(|a| a.len() == v.moons.moons.len() && a.iter().all(Value::is_object))
            && v.system["wanderers"]
                .as_array()
                .is_some_and(|a| a.iter().all(Value::is_object)),
        "inconsistent system body inventory",
    )?;
    check(
        v.moons
            .moons
            .iter()
            .enumerate()
            .all(|(index, m)| usize::try_from(m.index) == Ok(index)),
        "moon indices must match native catalog order",
    )?;
    let mut ids = BTreeSet::new();
    for m in &v.moons.moons {
        check(
            ids.insert(m.index)
                && render_radius(m.radius_km, 0.0, 1000.0).is_ok()
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
            && t.sea_level_m.abs() <= 1e9
            && t.elevation_m
                .iter()
                .all(|x| x.is_finite() && x.abs() <= 1e9)
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
        if let Some(radius) = b.radius_km {
            render_radius(radius, if b.kind == "anchor" { 80.0 } else { 0.0 }, 1000.0)?;
        }
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
    check(
        !a.lights.is_empty()
            && a.bodies
                .iter()
                .filter(|b| b.kind == "star")
                .all(|b| lights.contains(&b.id)),
        "incomplete stellar illumination inventory",
    )?;
    Ok(v)
}

/// Bound all globe geometry before reconstruction or creation of GPU assets.
/// The convex reconstruction cannot exceed the largest input elevation.
pub fn geometry(initial: &Initial, astronomy: &Astronomy, scale: f64) -> Result<(), ViewError> {
    let relief = initial.tiles.max_relief_km();
    for body in &astronomy.bodies {
        if let Some(radius) = body.radius_km {
            render_radius(
                radius,
                if body.id == "anchor" {
                    relief.max(80.0)
                } else {
                    0.0
                },
                scale,
            )?;
        }
    }
    Ok(())
}

impl Tiles {
    pub fn max_relief_km(&self) -> f64 {
        self.elevation_m
            .iter()
            .map(|e| (e - self.sea_level_m).max(0.0) / 1000.0)
            .fold(0.0_f64, f64::max)
    }
}
