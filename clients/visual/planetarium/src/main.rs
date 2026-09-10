#![allow(
    clippy::disallowed_types,
    reason = "SystemTime only creates collision-resistant draft directory names; observation time is exact source ticks"
)]
use hornvale_bevy_view::{ObservationMirror, Renderer};
use planetarium::{
    bridge::Bridge,
    pilot::{Pilot, pose},
};
use sha2::{Digest, Sha256};
use std::{error::Error, path::PathBuf};
fn main() {
    if let Err(e) = run() {
        eprintln!("planetarium: {e}");
        std::process::exit(1);
    }
}
fn run() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.get(1).map(String::as_str) != Some("inspect") {
        return Err("usage: planetarium inspect --world PATH --revision SHA --film PATH --output NEW_DIRECTORY".into());
    }
    let arg = |name: &str| -> Result<String, Box<dyn Error>> {
        let i = args
            .iter()
            .position(|a| a == name)
            .ok_or_else(|| format!("missing {name}"))?;
        args.get(i + 1)
            .cloned()
            .ok_or_else(|| format!("missing value for {name}").into())
    };
    let world = PathBuf::from(arg("--world")?);
    let revision = arg("--revision")?;
    let film = PathBuf::from(arg("--film")?);
    let output = if args.iter().any(|a| a == "--output") {
        PathBuf::from(arg("--output")?)
    } else {
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)?
            .as_nanos();
        PathBuf::from(format!("planetarium-draft-{stamp}"))
    };
    let pilot: Pilot = serde_json::from_slice(&std::fs::read(&film)?)?;
    if pilot.schema != "planetarium/draft/v1" || pilot.frames == 0 || pilot.frames > 300 {
        return Err("invalid draft film".into());
    }
    std::fs::create_dir(&output)?;
    let (mut bridge, initial) = Bridge::open(world.clone(), revision.clone())?;
    std::fs::write(output.join("initial.json"), &initial)?;
    let mut mirror = ObservationMirror::new(&initial)?;
    let request = mirror.request(pilot.start_ticks)?;
    let reply = bridge.observe(request)?;
    mirror.accept(&reply)?;
    hornvale_bevy_view::astronomy::surface::anchor_texture(&mirror.initial().tiles)
        .try_into_dynamic()?
        .save(output.join("surface-texture.png"))?;
    let mut renderer = Renderer::new(&mirror, pilot.width, pilot.height)?;
    if let Some(path) = &pilot.font_path {
        renderer.set_caption_font(std::fs::read(path)?)?;
    }
    if pilot.body_only {
        renderer.disable_atmosphere();
    }
    renderer.set_caption(if pilot.shot == "limb" {
        "A world turns."
    } else {
        "Its moons keep time."
    });
    let fixed_camera = pose(&mirror, &pilot.shot)?;
    let mut records = Vec::new();
    for frame in 0..pilot.frames {
        let ticks = pilot
            .start_ticks
            .checked_add(
                i64::from(frame)
                    .checked_mul(pilot.step_ticks)
                    .ok_or("draft time overflow")?,
            )
            .ok_or("draft time overflow")?;
        if frame != 0 {
            let request = mirror.request(ticks)?;
            let reply = bridge.observe(request)?;
            if !mirror.accept(&reply)? {
                return Err("draft reply unexpectedly obsolete".into());
            }
        }
        let camera = fixed_camera.clone();
        renderer.apply(&mirror, &camera)?;
        let file = format!("frame-{frame:05}.png");
        renderer.capture(&output.join(&file))?;
        let reply = mirror.current().expect("accepted");
        std::fs::write(
            output.join(format!("observation-{frame:05}.json")),
            serde_json::to_vec(reply)?,
        )?;
        records.push(serde_json::json!({"frame":frame,"ticks":ticks,"request_id":reply.request_id,"camera":camera,"file":file}));
        println!(
            "captured {frame}/{} tick={ticks}: {}",
            pilot.frames,
            output.join(file).display()
        );
    }
    let executable_sha256 = format!(
        "{:x}",
        Sha256::digest(std::fs::read(std::env::current_exe()?)?)
    );
    let font_sha256 = pilot
        .font_path
        .as_ref()
        .map(|p| std::fs::read(p).map(|bytes| format!("{:x}", Sha256::digest(bytes))))
        .transpose()?;
    let status = std::process::Command::new("git")
        .args(["status", "--porcelain"])
        .output()?;
    std::fs::write(
        output.join("draft.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"schema":"planetarium/draft-evidence/v1","world":world,"binding":mirror.initial().binding,"declared_source_revision":revision,"working_tree_status":String::from_utf8_lossy(&status.stdout),"film":pilot,"executable_sha256":executable_sha256,"font_sha256":font_sha256,"appearance":{"render_mode":"synchronous, pipelined rendering disabled; fixed exposure, no temporal history","history_reset_policy":hornvale_bevy_view::HISTORY_RESET_POLICY,"caption_height_fraction":0.06,"surface_reconstruction":"3x3 convex scalar filter + bilinear source sampling; normalized valid-ocean ice coverage; no new extrema","water_roughness":0.24,"land_roughness":0.86,"ice_roughness":0.65,"reflectance":0.35,"solar_photometric_reference_lux":127000,"point_light_range_render_units":1e9,"point_source_radius":0,"cloud_layer":"not rendered; source climate remains static","km_per_unit":1000,"sea_radius_reference":"bulk radius at sea surface; positive elevation-minus-sea relief 1:1","atmosphere":"cosmetic Earth scattering with 0.18 optical-density multiplier, 80km shell","exposure_ev100":13.3,"tonemapping":"AcesFitted","moon":"source albedo/tint with static cosmetic maria variation; no physical spin","shadows":"disabled, no finite stellar disc"},"frames":records}),
        )?,
    )?;
    Ok(())
}
