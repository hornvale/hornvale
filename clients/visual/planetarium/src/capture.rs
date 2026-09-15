#![allow(
    clippy::disallowed_types,
    reason = "Instant measures capture stage deadlines and performance, never source time"
)]
//! Fresh source observations, durable frame records and verified study publication.
use crate::{
    bridge::Bridge,
    live::positions,
    shots::{FilmDefinition, sample_caption, sample_shot},
};
use hornvale_bevy_view::{
    ObservationMirror, Renderer,
    capture::{CaptureMachine, CaptureSettings, CaptureState},
};
use sha2::{Digest, Sha256};
use std::{error::Error, io::Write, path::PathBuf, time::Instant};
fn hash(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}
fn write(path: impl AsRef<std::path::Path>, bytes: &[u8]) -> Result<(), Box<dyn Error>> {
    let mut file = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    file.write_all(bytes)?;
    file.flush()?;
    file.sync_all()?;
    Ok(())
}
fn json(
    path: impl AsRef<std::path::Path>,
    value: &impl serde::Serialize,
) -> Result<(), Box<dyn Error>> {
    write(path, &serde_json::to_vec_pretty(value)?)
}
pub fn run(
    world: PathBuf,
    revision: String,
    film: FilmDefinition,
    output: PathBuf,
    limit: Option<u32>,
) -> Result<(), Box<dyn Error>> {
    let frames = limit.unwrap_or(film.frames);
    if frames == 0 || frames > film.frames {
        return Err("capture limit outside film".into());
    }
    std::fs::create_dir(&output)?;
    let start = Instant::now();
    let result = capture(world, revision, film, &output, frames, &start);
    if let Err(error) = &result {
        write(output.join("FAILED"), error.to_string().as_bytes())?;
    }
    result
}
fn capture(
    world: PathBuf,
    revision: String,
    film: FilmDefinition,
    output: &std::path::Path,
    frames: u32,
    start: &Instant,
) -> Result<(), Box<dyn Error>> {
    let source = crate::provenance::source_state(&std::env::current_dir()?)?;
    write(
        output.join("development-source.patch"),
        source.patch.as_bytes(),
    )?;
    std::fs::create_dir(output.join("source"))?;
    write(output.join("source/world.json"), &std::fs::read(&world)?)?;
    json(output.join("film.json"), &film)?;
    let settings = CaptureSettings {
        width: film.width,
        height: film.height,
        frames,
        warmup_frames: 3,
        timeout_seconds: 120,
    };
    json(output.join("source-files.json"), &source.files)?;
    std::fs::create_dir(output.join("assets"))?;
    write(
        output.join("assets/LibreBaskerville-Regular.ttf"),
        include_bytes!("../assets/LibreBaskerville-Regular.ttf"),
    )?;
    let clean = crate::provenance::BUILD_CLEAN
        && source.status.is_empty()
        && source.head == crate::provenance::BUILD_REVISION
        && source.head == revision;
    let mut provenance = serde_json::json!({
        "schema":"planetarium/capture-provenance/v1",
        "purpose":if clean { "clean pinned study" } else { "dirty or unpinned development study" },
        "head":source.head, "working_tree_status":source.status,
        "rendering_source_tree_clean":clean, "build_revision":crate::provenance::BUILD_REVISION,
        "build_tree_clean":crate::provenance::BUILD_CLEAN, "rustc":crate::provenance::RUSTC,
        "os":format!("{} {}",std::env::consts::OS,std::env::consts::ARCH),
        "renderer":"Bevy 0.19.1; hornvale-bevy-view 0.1.0",
        "executable_sha256":hash(&std::fs::read(std::env::current_exe()?)?), "source_patch_sha256":hash(source.patch.as_bytes()),
        "source_revision":revision, "settings":settings, "full_film_frames":film.frames,
        "font_sha256":hash(include_bytes!("../assets/LibreBaskerville-Regular.ttf")),
        "presentation_seed":film.presentation_seed, "view_settings":film.settings, "history":hornvale_bevy_view::HISTORY_RESET_POLICY,
        "cosmetic_treatments":["seeded descriptor-conditioned pigment and maria; stable source-cratering-conditioned moon tangent normals (cosmetic marks, no physical relief)", "static cosmetic moon orientation where native spin is unavailable; no invented spin", "source-owned coherent ground: heights, normals, channels, coasts, ranges, and blended biome materials", "water roughness and reflectance", "cloud shell and opacity", "dynamic weather remains deferred", "atmospheric scattering", "ACES fitted tonemapping and exposure", "depth of field", "Libre Baskerville caption overlay"],
        "readback":"Bevy Screenshot::image; RGBA8 sRGB tightly packed top-to-bottom, PNG RGB8 lossless",
        "window":"none; independent image target"
    });
    let mut machine = CaptureMachine::new(settings)?;
    machine.check_timeout(start.elapsed().as_millis() as u64)?;
    let initial_budget =
        std::time::Duration::from_millis(machine.remaining_ms(start.elapsed().as_millis() as u64));
    let (mut bridge, initial) =
        Bridge::open_timeout(world, revision, initial_budget).map_err(|e| machine.fail(&e))?;
    write(output.join("source/initial.json"), initial.as_bytes())?;
    let mut mirror = ObservationMirror::new(&initial)?;
    film.validate(&mirror.initial().binding)?;
    std::fs::create_dir(output.join("source/observations"))?;
    std::fs::create_dir(output.join("frames"))?;
    let mut records = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(output.join("frames.jsonl"))?;
    let mut renderer = None;
    machine.check_timeout(start.elapsed().as_millis() as u64)?;
    machine.prepare(start.elapsed().as_millis() as u64)?;
    while let CaptureState::AwaitingObservation { frame } = *machine.state() {
        let frame_start = Instant::now();
        let ticks = film.clock().tick_at(frame)?;
        let request = mirror.request(ticks)?;
        machine.check_timeout(start.elapsed().as_millis() as u64)?;
        let query_budget = std::time::Duration::from_millis(
            machine.remaining_ms(start.elapsed().as_millis() as u64),
        );
        let reply = bridge
            .observe_timeout(request, query_budget)
            .map_err(|e| machine.fail(&e))?;
        if !mirror.accept(&reply)? {
            return Err(machine.fail("exact capture observation rejected").into());
        }
        machine.check_timeout(start.elapsed().as_millis() as u64)?;
        let camera = sample_shot(&film, frame, &positions(&mirror))?;
        for body in crate::live::bounds(&mirror) {
            camera.validate_body(body.position_km, body.outer_radius_km)?;
        }
        let caption = sample_caption(&film, frame)?;
        let observation_file = format!("source/observations/{frame:06}.json");
        write(output.join(&observation_file), reply.as_bytes())?;
        if renderer.is_none() {
            let mut r = Renderer::new(&mirror, film.width, film.height)?;
            r.set_caption_font(include_bytes!("../assets/LibreBaskerville-Regular.ttf").to_vec())?;
            let (gpu, backend) = r.adapter_identity();
            provenance["gpu"] = serde_json::json!(gpu);
            provenance["backend"] = serde_json::json!(backend);
            json(output.join("provenance.json"), &provenance)?;
            renderer = Some(r);
        }
        let r = renderer.as_mut().unwrap();
        r.set_caption(caption);
        r.apply(&mirror, &camera)?;
        let file = format!("frames/{frame:06}.png");
        let capture_start = Instant::now();
        r.capture_acknowledged(&mut machine, &output.join(&file), start)?;
        let capture_seconds = capture_start.elapsed().as_secs_f64();
        let record = serde_json::json!({"frame":frame,"presentation_time":{"numerator":frame,"denominator":film.fps},"request_id":mirror.current().unwrap().request_id,"ticks":ticks,"camera":camera,"caption":caption,"file":file,"png_sha256":hash(&std::fs::read(output.join(&file))?),"observation_file":observation_file,"observation_sha256":hash(reply.as_bytes()),"capture_seconds":capture_seconds,"frame_seconds":frame_start.elapsed().as_secs_f64(),"elapsed_seconds":start.elapsed().as_secs_f64()});
        serde_json::to_writer(&mut records, &record)?;
        records.write_all(b"\n")?;
        records.flush()?;
        records.sync_all()?;
        println!(
            "capture frame {frame:06} tick {ticks} capture {capture_seconds:.3}s elapsed {:.3}s",
            start.elapsed().as_secs_f64()
        );
    }
    if machine.state() != &CaptureState::Complete {
        return Err("capture did not reach all-files-written state".into());
    }
    json(
        output.join("capture.json"),
        &serde_json::json!({"schema":"planetarium/capture-result/v1","state":machine.state(),"frames_written":frames,"full_film":frames == film.frames,"elapsed_seconds":start.elapsed().as_secs_f64(),"package_complete":false}),
    )?;
    if frames == film.frames {
        if clean {
            let end = crate::provenance::source_state(&std::env::current_dir()?)?;
            if !end.status.is_empty() || end.head != crate::provenance::BUILD_REVISION {
                return Err(
                    "source tree changed during clean capture; refusing package completion".into(),
                );
            }
        }
        crate::package::finish_package(output)?;
        println!("COMPLETE {}", output.display());
    }
    println!(
        "CAPTURE FILES WRITTEN {} ({frames} frames; see COMPLETE for package status)",
        output.display()
    );
    Ok(())
}
