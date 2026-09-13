use hornvale_bevy_view::capture::{CaptureMachine, CaptureSettings, CaptureState};
fn machine() -> CaptureMachine {
    let mut m = CaptureMachine::new(CaptureSettings {
        width: 3840,
        height: 2160,
        frames: 2,
        warmup_frames: 2,
        timeout_seconds: 3,
    })
    .unwrap();
    m.prepare(0).unwrap();
    m
}
fn ready(m: &mut CaptureMachine, frame: u32) {
    m.observation_ready(frame, 0).unwrap();
    m.warmed(frame, 0).unwrap();
    m.warmed(frame, 0).unwrap();
}
#[test]
fn wrong_duplicate_and_obsolete_readbacks_cannot_advance() {
    let mut m = machine();
    ready(&mut m, 0);
    assert!(!m.readback(1, Ok(()), 0).unwrap());
    assert!(m.readback(0, Ok(()), 0).unwrap());
    assert!(!m.readback(0, Ok(()), 0).unwrap());
    m.written(0, 0).unwrap();
    ready(&mut m, 1);
    assert!(!m.readback(0, Ok(()), 0).unwrap());
    assert_eq!(m.state(), &CaptureState::AwaitingReadback { frame: 1 });
    m.readback(1, Ok(()), 0).unwrap();
    m.written(1, 0).unwrap();
    assert_eq!(m.state(), &CaptureState::Complete);
}
#[test]
fn failed_readback_is_terminal() {
    let mut m = machine();
    ready(&mut m, 0);
    assert!(m.readback(0, Err("device lost".into()), 0).is_err());
    assert!(
        matches!(m.state(), CaptureState::Failed(s) if s.contains("frame: 0") && s.contains("device lost"))
    );
    assert!(m.written(0, 0).is_err());
}
#[test]
fn missing_readback_times_out_with_frame_and_stage() {
    let mut m = machine();
    ready(&mut m, 0);
    assert!(m.check_timeout(3000).is_err());
    assert!(
        matches!(m.state(), CaptureState::Failed(s) if s.contains("AwaitingReadback") && s.contains("frame: 0"))
    );
}
#[test]
fn asset_failure_never_reaches_writing_frame_zero() {
    let mut m = machine();
    use hornvale_bevy_view::{bevy::prelude::*, capture::validate_scene_assets};
    let mut world = World::new();
    world.init_resource::<Assets<Mesh>>();
    world.init_resource::<Assets<Image>>();
    let texture = world.resource_mut::<Assets<Image>>().add(Image::default());
    world.resource_mut::<Assets<Image>>().remove(texture.id());
    let error = validate_scene_assets(&world, &[], &[texture]).unwrap_err();
    assert!(error.to_string().contains("missing scene texture"));
    m.fail(&error.to_string());
    assert!(m.observation_ready(0, 0).is_err());
    assert!(!m.readback(0, Ok(()), 0).unwrap());
    assert!(m.written(0, 0).is_err());
}
#[test]
fn readiness_and_warmup_have_deadlines() {
    let mut m = machine();
    assert!(m.check_timeout(3000).is_err());
    let mut m = machine();
    m.observation_ready(0, 1000).unwrap();
    m.warmed(0, 2000).unwrap();
    assert!(m.check_timeout(4000).is_err());
}
#[test]
fn png_rejects_bad_readback_and_never_overwrites() {
    use hornvale_bevy_view::{
        bevy::{
            asset::RenderAssetUsages,
            prelude::*,
            render::render_resource::{Extent3d, TextureDimension, TextureFormat},
        },
        capture::write_png,
    };
    let image = Image::new_fill(
        Extent3d {
            width: 2,
            height: 2,
            depth_or_array_layers: 1,
        },
        TextureDimension::D2,
        &[255, 0, 0, 255],
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::MAIN_WORLD,
    );
    let directory =
        std::env::temp_dir().join(format!("planetarium-png-test-{}", std::process::id()));
    std::fs::create_dir(&directory).unwrap();
    let path = directory.join("000000.png");
    assert!(write_png(&image, &path, 3, 2).is_err());
    assert!(!path.exists());
    let mut bad = image.clone();
    bad.data.as_mut().unwrap().push(0);
    assert!(write_png(&bad, &path, 2, 2).is_err());
    assert!(!path.exists());
    write_png(&image, &path, 2, 2).unwrap();
    let bytes = std::fs::read(&path).unwrap();
    assert!(write_png(&image, &path, 2, 2).is_err());
    assert_eq!(std::fs::read(&path).unwrap(), bytes);
    std::fs::remove_dir_all(directory).unwrap();
}
