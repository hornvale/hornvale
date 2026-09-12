#![allow(
    clippy::disallowed_types,
    reason = "GPU diagnostic wall-clock deadlines only"
)]
//! Explicit GPU-only diagnostic. Normal CPU tests never initialize a device.
use hornvale_bevy_view::{
    ObservationMirror, Renderer,
    capture::{CaptureMachine, CaptureSettings},
};
use planetarium::{
    bridge::Bridge,
    live::positions,
    shots::{FilmDefinition, sample_shot},
};
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();
    let film: FilmDefinition = serde_json::from_slice(&std::fs::read(&args[2])?)?;
    let output = std::path::PathBuf::from(&args[3]);
    std::fs::create_dir(&output)?;
    let (mut bridge, initial) =
        Bridge::open((&args[1]).into(), film.binding.source_revision.clone())?;
    let mut mirror = ObservationMirror::new(&initial)?;
    let q = mirror.request(0)?;
    assert!(mirror.accept(&bridge.observe(q)?)?);
    let pose = sample_shot(&film, 0, &positions(&mirror))?;
    let mut renderer = Renderer::new(&mirror, 257, 129)?;
    let mut machine = CaptureMachine::new(CaptureSettings {
        width: 257,
        height: 129,
        frames: 3,
        warmup_frames: 3,
        timeout_seconds: 120,
    })?;
    let clock = std::time::Instant::now();
    machine.prepare(0)?;
    for frame in 0..3 {
        renderer.apply(&mirror, &pose)?;
        renderer.diagnostic_overlay(frame);
        renderer.capture_acknowledged(
            &mut machine,
            &output.join(format!("{frame:06}.png")),
            &clock,
        )?;
        println!(
            "diagnostic frame {frame} acknowledged at {:.3}s",
            clock.elapsed().as_secs_f64()
        );
    }
    println!("DIAGNOSTIC {:?}", machine.state());
    Ok(())
}
