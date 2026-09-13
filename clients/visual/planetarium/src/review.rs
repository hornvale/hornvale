#![allow(
    clippy::disallowed_types,
    reason = "Instant measures review render duration; exact frame ticks own source time"
)]
//! Disposable directed witnesses, deliberately distinct from a complete film package.
use crate::{
    bridge::Bridge,
    live::positions,
    shots::{FilmDefinition, sample_caption, sample_shot},
};
use hornvale_bevy_view::{
    ObservationMirror, Renderer,
    documents::{self, SurfacePatchCacheKey, SurfacePatchRevision},
    lifecycle,
};
use sha2::{Digest, Sha256};
use std::{
    collections::BTreeSet,
    error::Error,
    path::PathBuf,
    process::Command,
    sync::atomic::{AtomicU64, Ordering},
    time::Instant,
};

const PROOF_CASES: [&str; 5] = [
    "confluence",
    "terminal_basin",
    "coast_crossing",
    "face_corner",
    "unequal_lod",
];
static NEXT_PROOF_REQUEST_ID: AtomicU64 = AtomicU64::new(1 << 32);

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct FrameMetadata {
    camera: hornvale_bevy_view::CameraPose,
    source_revision: String,
    evidence: hornvale_bevy_view::lifecycle::SurfaceRenderEvidence,
}

/// Measurements from the small, repeatable coherent-ground proof slice.
///
/// These values are observations for review, not performance acceptance
/// thresholds. Generation measures source patch production, memory is the
/// sampled maximum process resident set across each proof operation, and frame
/// time covers the serialized-reply validation plus Bevy mesh/material
/// application path. The memory value is a bounded RSS proxy, not a GPU
/// allocation measurement.
#[derive(Debug, Clone, PartialEq)]
pub struct SurfaceProofMetrics {
    pub seed: u64,
    pub patch_count: usize,
    pub refinement_levels: Vec<u8>,
    pub generation_latency_ms: Vec<u64>,
    pub peak_memory_bytes: Vec<u64>,
    pub frame_time_ms: Vec<f64>,
}

/// The reviewable facts recorded for one before or after frame set.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct CapturedFrames {
    pub required_features: BTreeSet<String>,
    pub seams_coherent: bool,
    pub rivers_reach_declared_ends: bool,
    pub coast_is_continuous: bool,
    pub mountain_direction_reads: bool,
    pub biome_transitions_are_blended: bool,
}

/// Results of comparing the current Planetarium frame set with the revised
/// source-owned coherent-ground frame set.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SurfaceReview {
    pub required_features_visible: bool,
    pub seams_coherent: bool,
    pub rivers_reach_declared_ends: bool,
    pub coast_is_continuous: bool,
    pub mountain_direction_reads: bool,
    pub biome_transitions_are_blended: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RenderedSurfaceFrame {
    pub png_path: PathBuf,
    pub png_sha256: String,
    pub camera_sha256: String,
    pub source_revision: String,
    pub patch_entities: usize,
    pub narrow_feature_entities: usize,
    pub fallback_visible: bool,
    pub narrow_feature_pixels: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RenderedSurfaceProof {
    pub before: RenderedSurfaceFrame,
    pub after: RenderedSurfaceFrame,
    pub source_generation_ms: Vec<u64>,
    pub mesh_material_application_ms: f64,
    pub first_visible_frame_ms: f64,
    pub steady_state_frame_ms: f64,
    pub peak_rss_bytes: u64,
}

/// Compare visible before/after facts without deriving semantic terrain in the
/// renderer. Each result is a transition claim: the after capture must carry
/// the fact and the before capture must not already carry it.
pub fn compare_surface_review(before: &CapturedFrames, after: &CapturedFrames) -> SurfaceReview {
    SurfaceReview {
        required_features_visible: PROOF_CASES
            .iter()
            .all(|feature| after.required_features.contains(*feature))
            && PROOF_CASES
                .iter()
                .any(|feature| !before.required_features.contains(*feature)),
        seams_coherent: !before.seams_coherent && after.seams_coherent,
        rivers_reach_declared_ends: !before.rivers_reach_declared_ends
            && after.rivers_reach_declared_ends,
        coast_is_continuous: !before.coast_is_continuous && after.coast_is_continuous,
        mountain_direction_reads: !before.mountain_direction_reads
            && after.mountain_direction_reads,
        biome_transitions_are_blended: !before.biome_transitions_are_blended
            && after.biome_transitions_are_blended,
    }
}

/// Run the real seed-42 surface proof region through the source boundary.
///
/// The committed scene fixture names the five pathologies and their addresses.
/// Each address is queried from the live source, with one additional child
/// query proving the second local refinement level. The proof deliberately
/// records evidence only; it does not write a world or establish a numeric
/// performance target.
pub fn run_surface_proof(seed: u64) -> Result<SurfaceProofMetrics, String> {
    let world = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../cli/tests/fixtures/world-seed-42.json");
    let fixture: serde_json::Value = serde_json::from_str(include_str!(
        "../../../../windows/scene/tests/fixtures/surface-seed-42-proof.json"
    ))
    .map_err(|error| format!("surface proof fixture is invalid: {error}"))?;
    validate_proof_fixture(&fixture)?;
    let world_bytes = std::fs::read(&world)
        .map_err(|error| format!("read surface proof world {}: {error}", world.display()))?;
    let world_document: serde_json::Value = serde_json::from_slice(&world_bytes)
        .map_err(|error| format!("parse surface proof world: {error}"))?;
    let actual_seed = world_document["seed"]
        .as_u64()
        .ok_or("surface proof world has no numeric seed")?;
    if actual_seed != seed {
        return Err(format!(
            "surface proof fixture is for seed {actual_seed}, not seed {seed}"
        ));
    }

    let mut source = hornvale_visual_source::Source::open(
        &world,
        crate::provenance::BUILD_REVISION,
        "planetarium-surface-proof",
    )
    .map_err(|error| format!("open surface proof source: {error}"))?;
    let initial: serde_json::Value = serde_json::from_str(
        &source
            .initial_document(512)
            .map_err(|error| format!("load surface proof initial document: {error}"))?,
    )
    .map_err(|error| format!("parse surface proof initial document: {error}"))?;
    let binding = &initial["binding"];
    let revision = &initial["surface_revision"];
    let mut generation_latency_ms = Vec::new();
    let mut peak_memory_bytes = Vec::new();
    let mut frame_time_ms = Vec::new();
    let mut refinement_levels = BTreeSet::new();
    let mut request_id = NEXT_PROOF_REQUEST_ID.fetch_add(32, Ordering::Relaxed);

    for case_name in PROOF_CASES {
        let case = &fixture["cases"][case_name];
        let address = if case_name == "unequal_lod" {
            &case["coarse"]["address"]
        } else {
            &case["patch"]["address"]
        };
        let child_path = address["child_path"].clone();
        let (patch, generation, frame, memory) = observe_proof_patch(
            &mut source,
            binding,
            revision,
            address,
            &child_path,
            None,
            request_id,
        )?;
        request_id += 1;
        validate_observed_patch(case_name, &patch, address, &child_path, None)?;
        validate_live_pathology(case_name, &patch)?;
        refinement_levels.insert(child_path_level(&child_path)?);
        generation_latency_ms.push(generation);
        peak_memory_bytes.push(memory);
        frame_time_ms.push(frame);
    }

    // The confluence's immediate child is the second local proof level.
    let case = &fixture["cases"]["confluence"];
    let address = &case["patch"]["address"];
    let child_path = serde_json::json!([0]);
    let (patch, generation, frame, memory) = observe_proof_patch(
        &mut source,
        binding,
        revision,
        address,
        &child_path,
        None,
        request_id,
    )?;
    validate_observed_patch("confluence refinement", &patch, address, &child_path, None)?;
    refinement_levels.insert(child_path_level(&child_path)?);
    generation_latency_ms.push(generation);
    peak_memory_bytes.push(memory);
    frame_time_ms.push(frame);

    // The face-corner witness is a three-patch comparison. All three addresses
    // must cross the same cube-sphere corner in the live source.
    let case = &fixture["cases"]["face_corner"];
    let mut corner_patches = Vec::new();
    for (index, address) in std::iter::once(&case["patch"]["address"])
        .chain(case["neighbor_addresses"].as_array().into_iter().flatten())
        .enumerate()
    {
        let child_path = address["child_path"].clone();
        let (patch, generation, frame, memory) = observe_proof_patch(
            &mut source,
            binding,
            revision,
            address,
            &child_path,
            None,
            request_id,
        )?;
        request_id += 1;
        validate_observed_patch(
            &format!("face corner {index}"),
            &patch,
            address,
            &child_path,
            None,
        )?;
        corner_patches.push(patch);
        refinement_levels.insert(child_path_level(&child_path)?);
        generation_latency_ms.push(generation);
        peak_memory_bytes.push(memory);
        frame_time_ms.push(frame);
    }
    validate_face_corner(&corner_patches)?;

    // Query both fine addresses directly and also ask the coarse patch to
    // carry each source-owned transition. The fixture's two fine addresses are
    // therefore exercised by the live source, not merely counted as metadata.
    let case = &fixture["cases"]["unequal_lod"];
    let coarse_address = &case["coarse"]["address"];
    let fine_addresses = case["fine_addresses"]
        .as_array()
        .ok_or("unequal-LOD fixture has no fine addresses")?;
    for (index, fine_address) in fine_addresses.iter().enumerate() {
        let fine_child_path = fine_address["child_path"].clone();
        let (fine_patch, generation, frame, memory) = observe_proof_patch(
            &mut source,
            binding,
            revision,
            fine_address,
            &fine_child_path,
            None,
            request_id,
        )?;
        request_id += 1;
        validate_observed_patch(
            &format!("unequal-LOD fine {index}"),
            &fine_patch,
            fine_address,
            &fine_child_path,
            None,
        )?;
        refinement_levels.insert(child_path_level(&fine_child_path)?);
        generation_latency_ms.push(generation);
        peak_memory_bytes.push(memory);
        frame_time_ms.push(frame);

        let coarse_child_path = coarse_address["child_path"].clone();
        let (transition_patch, generation, frame, memory) = observe_proof_patch(
            &mut source,
            binding,
            revision,
            coarse_address,
            &coarse_child_path,
            Some(fine_address),
            request_id,
        )?;
        request_id += 1;
        validate_observed_patch(
            &format!("unequal-LOD transition {index}"),
            &transition_patch,
            coarse_address,
            &coarse_child_path,
            Some(fine_address),
        )?;
        validate_live_pathology("unequal_lod transition", &transition_patch)?;
        refinement_levels.insert(child_path_level(&coarse_child_path)?);
        generation_latency_ms.push(generation);
        peak_memory_bytes.push(memory);
        frame_time_ms.push(frame);
    }

    Ok(SurfaceProofMetrics {
        seed,
        patch_count: generation_latency_ms.len(),
        refinement_levels: refinement_levels.into_iter().collect(),
        generation_latency_ms,
        peak_memory_bytes,
        frame_time_ms,
    })
}

/// Render a fixed seed and camera before and after one real source patch is
/// applied. The PNGs and metadata are written to a unique temporary artifact
/// directory and are intentionally returned to callers for readback checks.
pub fn run_rendered_surface_proof(seed: u64) -> Result<RenderedSurfaceProof, String> {
    let world = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../../cli/tests/fixtures/world-seed-42.json");
    let world_document: serde_json::Value = serde_json::from_slice(
        &std::fs::read(&world).map_err(|error| format!("read proof world: {error}"))?,
    )
    .map_err(|error| format!("parse proof world: {error}"))?;
    if world_document["seed"].as_u64() != Some(seed) {
        return Err(format!("proof world is not seed {seed}"));
    }
    let mut source = hornvale_visual_source::Source::open(
        &world,
        crate::provenance::BUILD_REVISION,
        "planetarium-rendered-surface-proof",
    )
    .map_err(|error| format!("open proof source: {error}"))?;
    let initial_json = source
        .initial_document(512)
        .map_err(|error| format!("load proof initial document: {error}"))?;
    let initial: serde_json::Value = serde_json::from_str(&initial_json)
        .map_err(|error| format!("parse proof initial document: {error}"))?;
    let mut mirror = ObservationMirror::new(&initial_json)
        .map_err(|error| format!("create proof mirror: {error}"))?;
    let observation = mirror
        .request(0)
        .map_err(|error| format!("request proof observation: {error}"))?;
    mirror
        .accept(
            &source
                .observe(&observation)
                .map_err(|error| format!("observe proof observation: {error}"))?,
        )
        .map_err(|error| format!("accept proof observation: {error}"))?;
    let film: FilmDefinition = serde_json::from_str(include_str!("../films/pilot.json"))
        .map_err(|error| format!("parse proof film: {error}"))?;
    let camera = sample_shot(&film, 0, &positions(&mirror))
        .map_err(|error| format!("sample proof camera: {error}"))?;
    let camera_sha256 = hash(
        &serde_json::to_vec(&camera).map_err(|error| format!("serialize proof camera: {error}"))?,
    );
    let revision: SurfacePatchRevision =
        serde_json::from_value(initial["surface_revision"].clone())
            .map_err(|error| format!("parse proof revision: {error}"))?;
    let mut renderer = Renderer::new(&mirror, 1920, 1080)
        .map_err(|error| format!("create proof renderer: {error}"))?;
    let mut rss_peak = process_memory_bytes()?;
    renderer
        .set_caption_font(include_bytes!("../assets/LibreBaskerville-Regular.ttf").to_vec())
        .map_err(|error| format!("load proof caption font: {error}"))?;
    renderer
        .apply(&mirror, &camera)
        .map_err(|error| format!("apply proof before scene: {error}"))?;
    let output = std::env::temp_dir().join(format!(
        "hornvale-coherent-ground-proof-{}-{}-{}",
        std::process::id(),
        NEXT_PROOF_REQUEST_ID.fetch_add(1, Ordering::Relaxed),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|error| format!("read proof clock: {error}"))?
            .as_nanos()
    ));
    std::fs::create_dir_all(&output)
        .map_err(|error| format!("create proof artifact dir: {error}"))?;
    let before_path = output.join("before.png");
    renderer
        .capture(&before_path)
        .map_err(|error| format!("capture proof before frame: {error}"))?;
    rss_peak = rss_peak.max(process_memory_bytes()?);
    let before_metadata = output.join("before.json");
    write_frame_metadata(
        &before_metadata,
        &camera,
        &revision.source_revision,
        renderer.surface_render_evidence(),
    )?;
    let before = rendered_frame(&before_path, &before_metadata, None)?;
    let anchor = mirror
        .current()
        .and_then(|reply| {
            reply
                .astronomy
                .bodies
                .iter()
                .find(|body| body.id == "anchor")
        })
        .ok_or("proof observation has no anchor")?;
    let candidates = lifecycle::visible_surface_patches(
        &hornvale_bevy_view::camera::OrbitCamera::new(camera.clone()),
        anchor.position_km,
        anchor.radius_km.ok_or("proof anchor has no radius")?,
        &revision,
    )
    .map_err(|error| format!("select camera-visible proof patches: {error}"))?;
    let key = candidates
        .first()
        .cloned()
        .ok_or("camera-visible proof patch set is empty")?;
    let macro_face = key.macro_face;
    let child_path = key.child_path.clone();
    renderer
        .set_desired_surface_patches(&mirror, vec![key.clone()])
        .map_err(|error| format!("select proof patch: {error}"))?;
    let request_id = NEXT_PROOF_REQUEST_ID.fetch_add(1, Ordering::Relaxed);
    let request = serde_json::json!({
        "schema": "visual/surface-request/v1",
        "binding": initial["binding"],
        "request_id": request_id,
        "generation": mirror.generation(),
        "address": {"macro_face": macro_face, "child_path": child_path},
        "expected_revision": initial["surface_revision"],
    })
    .to_string();
    renderer
        .schedule_surface_patch(key, request.clone())
        .map_err(|error| format!("schedule proof patch: {error}"))?;
    let generation_start = Instant::now();
    let reply = source
        .observe_surface(&request)
        .map_err(|error| format!("generate proof patch: {error}"))?;
    rss_peak = rss_peak.max(process_memory_bytes()?);
    let source_generation_ms = vec![generation_start.elapsed().as_millis().max(1) as u64];
    let application_start = Instant::now();
    renderer
        .apply_surface_reply(&reply)
        .map_err(|error| format!("apply proof patch: {error}"))?;
    renderer
        .apply(&mirror, &camera)
        .map_err(|error| format!("apply proof after scene: {error}"))?;
    rss_peak = rss_peak.max(process_memory_bytes()?);
    let mesh_material_application_ms =
        (application_start.elapsed().as_secs_f64() * 1000.).max(f64::EPSILON);
    let after_path = output.join("after.png");
    let first_start = Instant::now();
    renderer
        .capture(&after_path)
        .map_err(|error| format!("capture proof after frame: {error}"))?;
    rss_peak = rss_peak.max(process_memory_bytes()?);
    let first_visible_frame_ms = (first_start.elapsed().as_secs_f64() * 1000.).max(f64::EPSILON);
    let after_metadata = output.join("after.json");
    write_frame_metadata(
        &after_metadata,
        &camera,
        &revision.source_revision,
        renderer.surface_render_evidence(),
    )?;
    let after = rendered_frame(&after_path, &after_metadata, Some(&before_path))?;
    let steady_start = Instant::now();
    renderer
        .capture(&output.join("steady.png"))
        .map_err(|error| format!("capture proof steady frame: {error}"))?;
    rss_peak = rss_peak.max(process_memory_bytes()?);
    let steady_state_frame_ms = (steady_start.elapsed().as_secs_f64() * 1000.).max(f64::EPSILON);
    Ok(RenderedSurfaceProof {
        before,
        after,
        source_generation_ms,
        mesh_material_application_ms,
        first_visible_frame_ms,
        steady_state_frame_ms,
        peak_rss_bytes: rss_peak,
    })
}

fn rendered_frame(
    path: &std::path::Path,
    metadata_path: &std::path::Path,
    before_path: Option<&std::path::Path>,
) -> Result<RenderedSurfaceFrame, String> {
    let bytes =
        std::fs::read(path).map_err(|error| format!("read rendered proof frame: {error}"))?;
    image::load_from_memory_with_format(&bytes, image::ImageFormat::Png)
        .map_err(|error| format!("decode rendered proof frame: {error}"))?;
    let metadata: FrameMetadata = serde_json::from_slice(
        &std::fs::read(metadata_path).map_err(|e| format!("read frame metadata: {e}"))?,
    )
    .map_err(|e| format!("parse frame metadata: {e}"))?;
    let narrow_feature_pixels =
        before_path.map_or(0, |before| feature_delta_pixels(before, path).unwrap_or(0));
    Ok(RenderedSurfaceFrame {
        png_path: path.to_path_buf(),
        png_sha256: hash(&bytes),
        camera_sha256: hash(&serde_json::to_vec(&metadata.camera).map_err(|e| e.to_string())?),
        source_revision: metadata.source_revision,
        patch_entities: metadata.evidence.patch_entities,
        narrow_feature_entities: metadata.evidence.narrow_feature_entities,
        fallback_visible: metadata.evidence.fallback_visible,
        narrow_feature_pixels,
    })
}

fn write_frame_metadata(
    path: &std::path::Path,
    camera: &hornvale_bevy_view::CameraPose,
    source_revision: &str,
    evidence: hornvale_bevy_view::lifecycle::SurfaceRenderEvidence,
) -> Result<(), String> {
    let bytes = serde_json::to_vec(&FrameMetadata {
        camera: camera.clone(),
        source_revision: source_revision.into(),
        evidence,
    })
    .map_err(|e| e.to_string())?;
    std::fs::write(path, bytes).map_err(|e| e.to_string())
}

fn feature_delta_pixels(
    before: &std::path::Path,
    after: &std::path::Path,
) -> Result<usize, String> {
    let before = image::open(before).map_err(|e| e.to_string())?.to_rgba8();
    let after = image::open(after).map_err(|e| e.to_string())?.to_rgba8();
    if before.dimensions() != after.dimensions() {
        return Err("frame dimensions differ".into());
    }
    Ok(before
        .pixels()
        .zip(after.pixels())
        .filter(|(old, new)| {
            old != new && new[2] > new[0].saturating_add(8) && new[2] > new[1].saturating_add(4)
        })
        .count())
}

#[cfg(test)]
mod review_tests {
    use super::*;

    #[test]
    fn feature_delta_requires_blue_feature_pixels_in_png() {
        let directory = tempfile_dir();
        let before = directory.join("before.png");
        let after = directory.join("after.png");
        image::RgbaImage::from_pixel(2, 1, image::Rgba([10, 10, 10, 255]))
            .save(&before)
            .unwrap();
        let mut pixels = image::RgbaImage::from_pixel(2, 1, image::Rgba([10, 10, 10, 255]));
        pixels.put_pixel(1, 0, image::Rgba([20, 40, 100, 255]));
        pixels.save(&after).unwrap();
        assert_eq!(feature_delta_pixels(&before, &after).unwrap(), 1);
    }

    fn tempfile_dir() -> PathBuf {
        let path =
            std::env::temp_dir().join(format!("hornvale-review-test-{}", std::process::id()));
        std::fs::create_dir_all(&path).unwrap();
        path
    }
}

fn observe_proof_patch(
    source: &mut hornvale_visual_source::Source,
    binding: &serde_json::Value,
    revision: &serde_json::Value,
    address: &serde_json::Value,
    child_path: &serde_json::Value,
    transition: Option<&serde_json::Value>,
    request_id: u64,
) -> Result<(serde_json::Value, u64, f64, u64), String> {
    let macro_face = packed_macro_face(address)?;
    let child_path: Vec<u8> = serde_json::from_value(child_path.clone())
        .map_err(|error| format!("surface proof child path is invalid: {error}"))?;
    let revision_document: documents::SurfacePatchRevision =
        serde_json::from_value(revision.clone())
            .map_err(|error| format!("surface proof revision is invalid: {error}"))?;
    let transition_address = transition
        .map(|value| {
            Ok::<_, String>(serde_json::json!({
                "macro_face": packed_macro_face(value)?,
                "child_path": value["child_path"].clone(),
            }))
        })
        .transpose()?;
    let request = serde_json::json!({
        "schema": "visual/surface-request/v1",
        "binding": binding,
        "request_id": request_id,
        "generation": lifecycle::surface_patch_generation(),
        "address": {"macro_face": macro_face, "child_path": child_path},
        "transition_address": transition_address,
        "expected_revision": revision,
    });
    lifecycle::schedule_surface_patch(
        SurfacePatchCacheKey {
            revision: revision_document.source_revision.clone(),
            macro_face,
            child_path: child_path.clone(),
        },
        request.to_string(),
    )
    .map_err(|error| format!("schedule surface proof request {request_id}: {error}"))?;
    let memory_before = process_memory_bytes()?;
    let generation_start = Instant::now();
    let reply_json = source
        .observe_surface(&request.to_string())
        .map_err(|error| format!("surface proof source query {request_id}: {error}"))?;
    let generation_latency_ms = generation_start.elapsed().as_millis().max(1) as u64;
    let frame_start = Instant::now();
    let reply = documents::surface_reply(&reply_json)
        .map_err(|error| format!("surface proof reply {request_id} is invalid: {error}"))?;
    let patch = serde_json::to_value(&reply.patch).map_err(|error| {
        format!("surface proof patch {request_id} is not serializable: {error}")
    })?;
    lifecycle::apply_surface_reply(&reply_json)
        .map_err(|error| format!("apply surface proof reply {request_id}: {error}"))?;
    let frame_time_ms = (frame_start.elapsed().as_secs_f64() * 1000.).max(f64::EPSILON);
    let memory_after = process_memory_bytes()?;
    Ok((
        patch,
        generation_latency_ms,
        frame_time_ms,
        memory_before.max(memory_after),
    ))
}

fn validate_proof_fixture(fixture: &serde_json::Value) -> Result<(), String> {
    let cases = fixture
        .get("cases")
        .and_then(serde_json::Value::as_object)
        .ok_or("surface proof fixture has no cases")?;
    for name in PROOF_CASES {
        if !cases.contains_key(name) {
            return Err(format!("surface proof fixture is missing {name}"));
        }
    }
    let confluence = &fixture["cases"]["confluence"];
    if confluence["tributary"]["terminals"]
        .as_array()
        .is_none_or(|terminals| !terminals.iter().any(|v| v == "confluence"))
    {
        return Err("confluence fixture lacks a confluence terminal".into());
    }
    if fixture["cases"]["terminal_basin"]["terminal"]["terminals"]
        .as_array()
        .is_none_or(|terminals| !terminals.iter().any(|v| v == "lake"))
    {
        return Err("terminal basin fixture lacks a lake terminal".into());
    }
    let coast_samples = fixture["cases"]["coast_crossing"]["patch"]["samples"]
        .as_array()
        .ok_or("coast crossing fixture has no samples")?;
    let has_land = coast_samples.iter().any(|sample| {
        sample["fields"]["water_depth_m"] == 0.0
            && sample["fields"]["shoreline_distance_m"]
                .as_f64()
                .is_some_and(|v| v > 0.)
    });
    let has_water = coast_samples.iter().any(|sample| {
        sample["fields"]["water_depth_m"]
            .as_f64()
            .is_some_and(|v| v > 0.)
            && sample["fields"]["shoreline_distance_m"]
                .as_f64()
                .is_some_and(|v| v < 0.)
    });
    if !has_land || !has_water {
        return Err("coast crossing fixture does not cross the shoreline".into());
    }
    if fixture["cases"]["face_corner"]["neighbor_addresses"]
        .as_array()
        .is_none_or(|neighbors| neighbors.len() != 2)
    {
        return Err("face corner fixture lacks its two cross-face neighbors".into());
    }
    if fixture["cases"]["unequal_lod"]["coarse_replacement_triangles"]
        .as_array()
        .is_none_or(Vec::is_empty)
        || fixture["cases"]["unequal_lod"]["fine_addresses"]
            .as_array()
            .is_none_or(|addresses| addresses.len() != 2)
    {
        return Err("unequal-LOD fixture lacks coarse replacement or fine neighbors".into());
    }
    Ok(())
}

fn packed_macro_face(address: &serde_json::Value) -> Result<u32, String> {
    let face = address["face"]
        .as_u64()
        .ok_or("surface proof address has no face")?;
    if face >= 6 {
        return Err(format!("surface proof address has invalid face {face}"));
    }
    let path = address["macro_path"]
        .as_array()
        .ok_or("surface proof address has no macro path")?;
    let mut pathword = 1u64;
    for digit in path {
        let digit = digit
            .as_u64()
            .ok_or("surface proof macro path contains a non-integer")?;
        if digit >= 4 {
            return Err(format!(
                "surface proof macro path has invalid digit {digit}"
            ));
        }
        pathword = (pathword << 2) | digit;
    }
    ((pathword << 5) | face)
        .try_into()
        .map_err(|_| "surface proof macro face does not fit the wire integer".into())
}

fn child_path_level(child_path: &serde_json::Value) -> Result<u8, String> {
    child_path
        .as_array()
        .ok_or("surface proof child path is not an array")?
        .len()
        .try_into()
        .map_err(|_| "surface proof child path is too deep".into())
}

fn validate_observed_patch(
    case_name: &str,
    patch: &serde_json::Value,
    address: &serde_json::Value,
    child_path: &serde_json::Value,
    transition: Option<&serde_json::Value>,
) -> Result<(), String> {
    if patch["schema"] != "scene/surface/v1" {
        return Err(format!("{case_name} proof patch has the wrong schema"));
    }
    if patch["address"]["macro_face"] != packed_macro_face(address)?
        || patch["address"]["child_path"] != *child_path
    {
        return Err(format!("{case_name} proof patch changed its address"));
    }
    if patch["samples"].as_array().is_none_or(Vec::is_empty)
        || patch["triangles"].as_array().is_none_or(Vec::is_empty)
    {
        return Err(format!("{case_name} proof patch has no renderable samples"));
    }
    for hook in ["weather", "cloud", "precip"] {
        if patch.to_string().contains(hook) {
            return Err(format!(
                "{case_name} proof patch contains deferred hook {hook}"
            ));
        }
    }
    if transition.is_some()
        && patch["transition_triangles"]
            .as_array()
            .is_none_or(Vec::is_empty)
    {
        return Err(format!(
            "{case_name} proof patch has no mixed-LOD transition"
        ));
    }
    Ok(())
}

fn validate_live_pathology(case_name: &str, patch: &serde_json::Value) -> Result<(), String> {
    let features = patch["curves"]
        .as_array()
        .ok_or_else(|| format!("{case_name} live patch has no curves"))?;
    let terminals = features
        .iter()
        .flat_map(|feature| feature["endpoints"].as_array().into_iter().flatten())
        .filter_map(|endpoint| endpoint["terminal"].as_str());
    match case_name {
        "confluence" => {
            if !terminals.clone().any(|terminal| terminal == "confluence") {
                return Err("live confluence patch has no confluence terminal".into());
            }
        }
        "terminal_basin" => {
            if !terminals.clone().any(|terminal| terminal == "lake") {
                return Err("live terminal basin patch has no lake terminal".into());
            }
        }
        "coast_crossing" => {
            let samples = patch["samples"]
                .as_array()
                .ok_or("live coast crossing patch has no samples")?;
            let has_land = samples.iter().any(|sample| {
                sample["water_depth_m"] == 0.0
                    && sample["shoreline_distance_m"]
                        .as_f64()
                        .is_some_and(|distance| distance > 0.)
            });
            let has_water = samples.iter().any(|sample| {
                sample["water_depth_m"]
                    .as_f64()
                    .is_some_and(|depth| depth > 0.)
                    && sample["shoreline_distance_m"]
                        .as_f64()
                        .is_some_and(|distance| distance < 0.)
            });
            if !has_land || !has_water {
                return Err("live coast crossing patch does not cross the shoreline".into());
            }
        }
        "unequal_lod transition"
            if patch["transition_triangles"]
                .as_array()
                .is_none_or(Vec::is_empty) =>
        {
            return Err("live unequal-LOD patch has no transition triangles".into());
        }
        _ => {}
    }
    Ok(())
}

fn validate_face_corner(patches: &[serde_json::Value]) -> Result<(), String> {
    if patches.len() != 3 {
        return Err(format!(
            "face corner proof observed {} patches instead of 3",
            patches.len()
        ));
    }
    let corner = [
        1.0 / 3.0_f64.sqrt(),
        -1.0 / 3.0_f64.sqrt(),
        -1.0 / 3.0_f64.sqrt(),
    ];
    for (index, patch) in patches.iter().enumerate() {
        let samples = patch["samples"]
            .as_array()
            .ok_or_else(|| format!("face corner patch {index} has no samples"))?;
        let has_corner = samples.iter().any(|sample| {
            sample["fields"]["position"]
                .as_array()
                .or_else(|| sample["position"].as_array())
                .is_some_and(|position| {
                    position.iter().zip(corner).all(|(actual, expected)| {
                        actual
                            .as_f64()
                            .is_some_and(|actual| (actual - expected).abs() < 1e-5)
                    })
                })
        });
        if !has_corner {
            return Err(format!(
                "face corner live patch {index} does not contain the shared corner"
            ));
        }
    }
    Ok(())
}

fn process_memory_bytes() -> Result<u64, String> {
    let pid = std::process::id().to_string();
    let output = Command::new("ps")
        .args(["-o", "rss=", "-p", &pid])
        .output()
        .map_err(|error| format!("read proof process memory: {error}"))?;
    if !output.status.success() {
        return Err(format!(
            "read proof process memory: ps exited with {}",
            output.status
        ));
    }
    let kib = String::from_utf8(output.stdout)
        .map_err(|error| format!("read proof process memory: {error}"))?
        .trim()
        .parse::<u64>()
        .map_err(|error| format!("parse proof process memory: {error}"))?;
    Ok(kib.saturating_mul(1024))
}
pub fn run(
    world: PathBuf,
    revision: String,
    film: FilmDefinition,
    output: PathBuf,
    stride: u32,
    width: u32,
) -> Result<(), Box<dyn Error>> {
    if stride == 0 || ![1920, 3840].contains(&width) {
        return Err("review needs positive stride and qualified 1920/3840 width".into());
    }
    std::fs::create_dir(&output)?;
    let status = std::process::Command::new("git")
        .args(["status", "--porcelain"])
        .output()?;
    let head = std::process::Command::new("git")
        .args(["rev-parse", "HEAD"])
        .output()?;
    let executable_sha256 = hash(&std::fs::read(std::env::current_exe()?)?);
    let (mut bridge, initial) = Bridge::open(world, revision)?;
    std::fs::write(output.join("initial.json"), &initial)?;
    let mut mirror = ObservationMirror::new(&initial)?;
    film.validate(&mirror.initial().binding)?;
    let mut renderer = None;
    let mut records = Vec::new();
    let start = std::time::Instant::now();
    for frame in 0..film.frames {
        let ticks = film.clock().tick_at(frame)?;
        let q = mirror.request(ticks)?;
        let reply = bridge.observe(q)?;
        if !mirror.accept(&reply)? {
            return Err("exact review reply rejected".into());
        }
        let camera = sample_shot(&film, frame, &positions(&mirror))?;
        std::fs::write(output.join(format!("observation-{frame:05}.json")), &reply)?;
        for b in crate::live::bounds(&mirror) {
            camera.validate_body(b.position_km, b.outer_radius_km)?;
        }
        let mut file = None;
        let mut sha = None;
        let mut record_camera = camera.clone();
        let mut record_revision = mirror.initial().binding.source_revision.clone();
        if frame % stride == 0 || frame == film.frames - 1 {
            if renderer.is_none() {
                let mut r = Renderer::new(&mirror, width, width * 9 / 16)?;
                r.set_caption_font(
                    include_bytes!("../assets/LibreBaskerville-Regular.ttf").to_vec(),
                )?;
                renderer = Some(r);
            }
            let r = renderer.as_mut().unwrap();
            r.set_caption(sample_caption(&film, frame)?);
            r.apply(&mirror, &camera)?;
            let filename = format!("frame-{frame:05}.png");
            let png_path = output.join(&filename);
            r.capture(&png_path)?;
            let metadata_path = output.join(format!("frame-{frame:05}.json"));
            write_frame_metadata(
                &metadata_path,
                &camera,
                &mirror.initial().binding.source_revision,
                r.surface_render_evidence(),
            )?;
            let metadata: FrameMetadata = serde_json::from_slice(&std::fs::read(metadata_path)?)?;
            record_camera = metadata.camera;
            record_revision = metadata.source_revision;
            sha = Some(hash(&std::fs::read(&png_path)?));
            file = Some(filename);
            println!(
                "review frame {frame} tick {ticks} elapsed {:.2}s",
                start.elapsed().as_secs_f64()
            );
        }
        records.push(serde_json::json!({"frame":frame,"ticks":ticks,"camera":record_camera,"source_revision":record_revision,"caption":sample_caption(&film,frame)?,"file":file,"sha256":sha,"observation_sha256":hash(reply.as_bytes())}));
    }
    // Endpoint is an avoidance probe, not a presentation frame.
    let q = mirror.request(film.end_ticks)?;
    std::fs::write(output.join("observation-endpoint.json"), bridge.observe(q)?)?;
    std::fs::write(
        output.join("draft.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"schema":"planetarium/task5-review/v1","capture_head":String::from_utf8_lossy(&head.stdout).trim(),"working_tree_status":String::from_utf8_lossy(&status.stdout),"executable_sha256":executable_sha256,"film":film,"render_dimensions":[width,width*9/16],"stride":stride,"font_sha256":hash(include_bytes!("../assets/LibreBaskerville-Regular.ttf")),"appearance":{"native_light":true,"physical_relief":"positive elevation minus sea level at 1:1; source derivative normals","pigment":"body-fixed seeded cosmetic albedo grain bounded +/-12 percent","atmosphere":"cosmetic Earth scattering 0.18 density and 80km shell","clouds":"source-fraction-conditioned static cosmetic pattern; 12km presentation shell, not physical cloud altitude; no shadows or weather motion","msaa":4,"focus":"Gaussian spatial, settings in film; km divided by shared 1000 km/unit","shadows":"disabled; no eclipse claim","history":hornvale_bevy_view::HISTORY_RESET_POLICY},"elapsed_seconds":start.elapsed().as_secs_f64(),"frames":records}),
        )?,
    )?;
    println!("REVIEW COMPLETE {}", output.display());
    Ok(())
}
fn hash(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}

/// Explicit GPU witness: exact representative set, then consecutive temporal
/// samples, twice with a fresh production renderer. Never a complete package.
pub fn qualify(
    world: PathBuf,
    revision: String,
    film: FilmDefinition,
    output: PathBuf,
) -> Result<(), Box<dyn Error>> {
    std::fs::create_dir(&output)?;
    let provenance = crate::provenance::source_state(&std::env::current_dir()?)?;
    std::fs::write(
        output.join("provenance.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"head":provenance.head,"status":provenance.status,"build_revision":crate::provenance::BUILD_REVISION,"build_tree_clean":crate::provenance::BUILD_CLEAN,"executable_sha256":hash(&std::fs::read(std::env::current_exe()?)?)}),
        )?,
    )?;
    let sequence: Vec<u32> = [0, 89, 90, 209, 210, 299]
        .into_iter()
        .chain(210..220)
        .collect();
    for pass in 0..2 {
        let directory = output.join(format!("pass-{pass}"));
        std::fs::create_dir(&directory)?;
        let (mut source, initial) = Bridge::open(world.clone(), revision.clone())?;
        std::fs::write(directory.join("initial.json"), &initial)?;
        let mut mirror = ObservationMirror::new(&initial)?;
        film.validate(&mirror.initial().binding)?;
        let first = mirror.request(film.clock().tick_at(0)?)?;
        mirror.accept(&source.observe(first)?)?;
        let mut renderer = Renderer::new(&mirror, 3840, 2160)?;
        renderer
            .set_caption_font(include_bytes!("../assets/LibreBaskerville-Regular.ttf").to_vec())?;
        let mut records = Vec::new();
        for (index, frame) in sequence.iter().copied().enumerate() {
            let query = mirror.request(film.clock().tick_at(frame)?)?;
            let reply = source.observe(query)?;
            if !mirror.accept(&reply)? {
                return Err("qualification exact reply rejected".into());
            }
            let camera = sample_shot(&film, frame, &positions(&mirror))?;
            renderer.set_caption(sample_caption(&film, frame)?);
            renderer.apply(&mirror, &camera)?;
            let file = format!("{index:02}-frame-{frame:06}.png");
            renderer.capture(&directory.join(&file))?;
            std::fs::write(
                directory.join(format!("{index:02}-observation.json")),
                &reply,
            )?;
            records.push(serde_json::json!({"index":index,"frame":frame,"file":file,"camera":camera,"camera_sha256":hash(&serde_json::to_vec(&camera)?),"observation_sha256":hash(reply.as_bytes()),"png_sha256":hash(&std::fs::read(directory.join(&file))?)}));
            println!("qualification pass={pass} frame={frame}");
        }
        std::fs::write(
            directory.join("records.json"),
            serde_json::to_vec_pretty(&records)?,
        )?;
    }
    std::fs::write(output.join("film.json"), serde_json::to_vec_pretty(&film)?)?;
    println!("QUALIFICATION COMPLETE {}", output.display());
    Ok(())
}
