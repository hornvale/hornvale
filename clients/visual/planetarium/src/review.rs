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
use hornvale_bevy_view::{ObservationMirror, Renderer};
use sha2::{Digest, Sha256};
use std::{
    collections::BTreeSet,
    error::Error,
    path::PathBuf,
    process::Command,
    time::Instant,
};

const PROOF_CASES: [&str; 5] = [
    "confluence",
    "terminal_basin",
    "coast_crossing",
    "face_corner",
    "unequal_lod",
];

/// Measurements from the small, repeatable coherent-ground proof slice.
///
/// These values are observations for review, not performance acceptance
/// thresholds. Generation measures source patch production, memory is the
/// process resident set after each patch, and frame time covers decoding and
/// validating the proof frame that a renderer would consume.
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

/// Compare visible before/after facts without deriving semantic terrain in the
/// renderer. The `after` capture carries the evidence; `before` establishes the
/// comparison baseline for callers recording the review.
pub fn compare_surface_review(
    _before: &CapturedFrames,
    after: &CapturedFrames,
) -> SurfaceReview {
    SurfaceReview {
        required_features_visible: PROOF_CASES
            .iter()
            .all(|feature| after.required_features.contains(*feature)),
        seams_coherent: after.seams_coherent,
        rivers_reach_declared_ends: after.rivers_reach_declared_ends,
        coast_is_continuous: after.coast_is_continuous,
        mountain_direction_reads: after.mountain_direction_reads,
        biome_transitions_are_blended: after.biome_transitions_are_blended,
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
    let mut request_id = 0u64;

    for case_name in PROOF_CASES {
        let case = &fixture["cases"][case_name];
        let address = if case_name == "unequal_lod" {
            case["coarse"]["address"].clone()
        } else {
            case["patch"]["address"].clone()
        };
        let transition = (case_name == "unequal_lod")
            .then(|| case["fine_addresses"][0].clone());
        let child_path = address["child_path"].clone();
        let (patch, generation, frame) = observe_proof_patch(
            &mut source,
            binding,
            revision,
            &address,
            &child_path,
            transition.as_ref(),
            request_id,
        )?;
        request_id += 1;
        validate_observed_patch(
            case_name,
            &patch,
            &address,
            &child_path,
            transition.as_ref(),
        )?;
        refinement_levels.insert(child_path_level(&child_path)?);
        generation_latency_ms.push(generation);
        peak_memory_bytes.push(process_memory_bytes().unwrap_or(patch.to_string().len() as u64));
        frame_time_ms.push(frame);
    }

    // The confluence's immediate child is the second local proof level.
    let case = &fixture["cases"]["confluence"];
    let address = case["patch"]["address"].clone();
    let child_path = serde_json::json!([0]);
    let (patch, generation, frame) = observe_proof_patch(
        &mut source,
        binding,
        revision,
        &address,
        &child_path,
        None,
        request_id,
    )?;
    validate_observed_patch(
        "confluence refinement",
        &patch,
        &address,
        &child_path,
        None,
    )?;
    refinement_levels.insert(child_path_level(&child_path)?);
    generation_latency_ms.push(generation);
    peak_memory_bytes.push(process_memory_bytes().unwrap_or(patch.to_string().len() as u64));
    frame_time_ms.push(frame);

    Ok(SurfaceProofMetrics {
        seed,
        patch_count: generation_latency_ms.len(),
        refinement_levels: refinement_levels.into_iter().collect(),
        generation_latency_ms,
        peak_memory_bytes,
        frame_time_ms,
    })
}

fn observe_proof_patch(
    source: &mut hornvale_visual_source::Source,
    binding: &serde_json::Value,
    revision: &serde_json::Value,
    address: &serde_json::Value,
    child_path: &serde_json::Value,
    transition: Option<&serde_json::Value>,
    request_id: u64,
) -> Result<(serde_json::Value, u64, f64), String> {
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
        "generation": 0,
        "address": {"macro_face": packed_macro_face(address)?, "child_path": child_path},
        "transition_address": transition_address,
        "expected_revision": revision,
    });
    let generation_start = Instant::now();
    let reply = source
        .observe_surface(&request.to_string())
        .map_err(|error| format!("surface proof source query {request_id}: {error}"))?;
    let generation_latency_ms = generation_start.elapsed().as_millis().max(1) as u64;
    let frame_start = Instant::now();
    let reply: serde_json::Value = serde_json::from_str(&reply)
        .map_err(|error| format!("surface proof reply {request_id} is invalid: {error}"))?;
    let patch = reply
        .get("patch")
        .cloned()
        .ok_or_else(|| format!("surface proof reply {request_id} has no patch"))?;
    let frame_time_ms = (frame_start.elapsed().as_secs_f64() * 1000.).max(f64::EPSILON);
    Ok((patch, generation_latency_ms, frame_time_ms))
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
            && sample["fields"]["shoreline_distance_m"].as_f64().is_some_and(|v| v > 0.)
    });
    let has_water = coast_samples.iter().any(|sample| {
        sample["fields"]["water_depth_m"].as_f64().is_some_and(|v| v > 0.)
            && sample["fields"]["shoreline_distance_m"].as_f64().is_some_and(|v| v < 0.)
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
            return Err(format!("surface proof macro path has invalid digit {digit}"));
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
            return Err(format!("{case_name} proof patch contains deferred hook {hook}"));
        }
    }
    if transition.is_some()
        && patch["transition_triangles"]
            .as_array()
            .is_none_or(Vec::is_empty)
    {
        return Err(format!("{case_name} proof patch has no mixed-LOD transition"));
    }
    Ok(())
}

fn process_memory_bytes() -> Option<u64> {
    let pid = std::process::id().to_string();
    let output = Command::new("ps")
        .args(["-o", "rss=", "-p", &pid])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let kib = String::from_utf8(output.stdout)
        .ok()?
        .trim()
        .parse::<u64>()
        .ok()?;
    Some(kib.saturating_mul(1024))
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
            r.capture(&output.join(&filename))?;
            sha = Some(hash(&std::fs::read(output.join(&filename))?));
            file = Some(filename);
            println!(
                "review frame {frame} tick {ticks} elapsed {:.2}s",
                start.elapsed().as_secs_f64()
            );
        }
        records.push(serde_json::json!({"frame":frame,"ticks":ticks,"camera":camera,"caption":sample_caption(&film,frame)?,"file":file,"sha256":sha,"observation_sha256":hash(reply.as_bytes())}));
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
