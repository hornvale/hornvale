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
use std::{error::Error, path::PathBuf};
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
            &serde_json::json!({"schema":"planetarium/task5-review/v1","capture_head":String::from_utf8_lossy(&head.stdout).trim(),"working_tree_status":String::from_utf8_lossy(&status.stdout),"executable_sha256":executable_sha256,"film":film,"render_dimensions":[width,width*9/16],"stride":stride,"font_sha256":hash(include_bytes!("../assets/LibreBaskerville-Regular.ttf")),"appearance":{"native_light":true,"physical_relief":"positive elevation minus sea level at 1:1; source derivative normals","pigment":"body-fixed seeded cosmetic albedo grain bounded +/-12 percent","atmosphere":"cosmetic Earth scattering 0.18 density and 80km shell","clouds":"not rendered; source climate static","msaa":4,"focus":"Gaussian spatial, settings in film; km divided by shared 1000 km/unit","shadows":"disabled; no eclipse claim","history":hornvale_bevy_view::HISTORY_RESET_POLICY},"elapsed_seconds":start.elapsed().as_secs_f64(),"frames":records}),
        )?,
    )?;
    println!("REVIEW COMPLETE {}", output.display());
    Ok(())
}
fn hash(bytes: &[u8]) -> String {
    format!("{:x}", Sha256::digest(bytes))
}
