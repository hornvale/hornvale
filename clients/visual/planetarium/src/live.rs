#![allow(
    clippy::disallowed_types,
    reason = "Instant measures benchmark startup independently of simulation time"
)]
//! The visible app owns transport and controls; the shared catalog owns all scene assets.
use crate::{
    bridge::Bridge,
    controls::{DragOwner, Playback, PointerGesture},
    observation::ObservationState,
    shots::{FilmDefinition, sample_caption, sample_shot},
};
use hornvale_bevy_view::{
    ObservationMirror, PresentationTimeline, VisualPlugin,
    bevy::{
        self,
        input::{
            InputSystems,
            mouse::{AccumulatedMouseScroll, MouseScrollUnit},
        },
        prelude::*,
        render::RenderPlugin,
        window::{PrimaryWindow, WindowResolution},
    },
    camera::{BodyBound, OrbitCamera, PickTarget, camera_components},
    lifecycle::{SceneCatalog, SceneTarget},
};
use std::{collections::BTreeMap, path::PathBuf};
#[derive(Resource)]
struct Live {
    source: Bridge,
    observation: ObservationState,
    film: FilmDefinition,
    playback: Playback,
    camera: Entity,
    caption: Entity,
    caption_root: Entity,
    status: Entity,
    toolbar: Entity,
    scrub_fill: Entity,
    orbit: OrbitCamera,
    catalog: SceneCatalog,
    committed_frame: u32,
    inspection: bool,
    free_camera: bool,
    drag_distance: f32,
    pointer: PointerGesture,
    last_cursor: Option<Vec2>,
    dimensions: (u32, u32),
    elapsed: f64,
    render_frames: u64,
}
pub fn positions(m: &ObservationMirror) -> BTreeMap<String, [f64; 3]> {
    m.current()
        .expect("accepted observation")
        .astronomy
        .bodies
        .iter()
        .map(|b| (b.id.clone(), b.position_km))
        .collect()
}
pub fn bounds(m: &ObservationMirror) -> Vec<BodyBound> {
    m.current()
        .expect("accepted observation")
        .astronomy
        .bodies
        .iter()
        .filter_map(|b| {
            b.radius_km.map(|r| BodyBound {
                id: b.id.clone(),
                position_km: b.position_km,
                outer_radius_km: r + if b.id == "anchor" {
                    m.initial().tiles.max_relief_km()
                } else {
                    0.
                },
            })
        })
        .collect()
}
/// Only entities rendered as physical bodies or unresolved wanderer markers are
/// pickable; a light without a rendered stellar sphere is not a pick target.
pub fn pick_targets(m: &ObservationMirror) -> Vec<PickTarget> {
    m.current()
        .expect("accepted observation")
        .astronomy
        .bodies
        .iter()
        .filter(|b| b.radius_km.is_some() || b.kind == "wanderer")
        .map(|b| PickTarget {
            id: b.id.clone(),
            position_km: b.position_km,
            physical_radius_km: b.radius_km,
        })
        .collect()
}
fn text(world: &mut World, value: &str, size: f32) -> Entity {
    world
        .spawn((
            Text::new(value),
            TextFont {
                font_size: FontSize::Px(size),
                ..default()
            },
            TextColor(Color::srgb(0.84, 0.9, 0.92)),
        ))
        .id()
}
pub fn run(
    world_path: PathBuf,
    revision: String,
    film: FilmDefinition,
    recording: Option<PathBuf>,
    benchmark: Option<PathBuf>,
    started: std::time::Instant,
) -> Result<(), Box<dyn std::error::Error>> {
    if recording.is_some() && benchmark.is_some() {
        return Err("benchmark requires screenshot recording disabled".into());
    }
    let benchmark = benchmark
        .map(|p| crate::benchmark::Benchmark::new(p, started))
        .transpose()?;
    let (mut source, initial) = Bridge::open(world_path, revision)?;
    if benchmark.is_some() {
        source.enable_query_samples();
    }
    let mut mirror = ObservationMirror::new(&initial)?;
    film.validate(&mirror.initial().binding)?;
    let q = mirror.request(film.clock().tick_at(0)?)?;
    mirror.accept(&source.observe(q)?)?;
    let pose = sample_shot(&film, 0, &positions(&mirror))?;
    let mut app = App::new();
    app.add_plugins(
        DefaultPlugins
            .set(WindowPlugin {
                primary_window: Some(Window {
                    title: "Hornvale Planetarium".into(),
                    resolution: WindowResolution::new(1920, 1080),
                    ..default()
                }),
                ..default()
            })
            .set(RenderPlugin {
                synchronous_pipeline_compilation: true,
                ..default()
            })
            .disable::<bevy::render::pipelined_rendering::PipelinedRenderingPlugin>(),
    )
    .add_plugins(VisualPlugin)
    .insert_resource(ClearColor(Color::BLACK))
    .insert_resource(GlobalAmbientLight::NONE);
    let world = app.world_mut();
    if let Some(directory) = recording {
        world.insert_resource(crate::control_recording::ControlRecording::new(
            directory, &film,
        )?);
    }
    let camera = world.spawn(camera_components(&film.settings)).id();
    let mut catalog = SceneCatalog::default();
    catalog.apply(
        world,
        &mirror,
        &pose,
        SceneTarget {
            camera,
            width: 1920,
            height: 1080,
        },
    )?;
    catalog.select("anchor", world)?;
    let font = world.resource_mut::<Assets<Font>>().add(Font::from_bytes(
        include_bytes!("../assets/LibreBaskerville-Regular.ttf").to_vec(),
    ));
    let caption = world
        .spawn((
            Text::new(sample_caption(&film, 0)?),
            TextFont {
                font: FontSource::Handle(font),
                font_size: FontSize::Px(48.),
                ..default()
            },
            TextColor(Color::srgb(0.9, 0.89, 0.83)),
            TextLayout::justify(Justify::Center),
        ))
        .id();
    let caption_root = world
        .spawn((
            Node {
                position_type: PositionType::Absolute,
                bottom: percent(12.),
                width: percent(100.),
                justify_content: JustifyContent::Center,
                ..default()
            },
            UiTargetCamera(camera),
        ))
        .add_child(caption)
        .id();
    let status = text(world, "Loading scene...", 16.);
    world
        .spawn((
            Node {
                position_type: PositionType::Absolute,
                left: px(24.),
                top: px(20.),
                ..default()
            },
            UiTargetCamera(camera),
        ))
        .add_child(status);
    let toolbar = world
        .spawn((
            Node {
                position_type: PositionType::Absolute,
                left: px(0.),
                bottom: px(0.),
                width: percent(100.),
                height: px(92.),
                ..default()
            },
            BackgroundColor(Color::srgba(0., 0., 0., 0.88)),
            UiTargetCamera(camera),
        ))
        .id();
    let label = text(
        world,
        "Space  Play / pause     Left / Right  Step     - +  Rate     V  Reverse     Tab  Film view    F9 Record",
        16.,
    );
    world.entity_mut(label).insert(Node {
        position_type: PositionType::Absolute,
        left: px(24.),
        top: px(8.),
        ..default()
    });
    world.entity_mut(toolbar).add_child(label);
    let play = text(world, "Play", 23.);
    world.entity_mut(play).insert(Node {
        position_type: PositionType::Absolute,
        left: px(24.),
        bottom: px(20.),
        ..default()
    });
    world.entity_mut(toolbar).add_child(play);
    let track = world
        .spawn((
            Node {
                position_type: PositionType::Absolute,
                left: percent(12.),
                right: percent(4.),
                bottom: px(24.),
                height: px(18.),
                ..default()
            },
            BackgroundColor(Color::srgb(0.11, 0.20, 0.23)),
        ))
        .id();
    let scrub_fill = world
        .spawn((
            Node {
                height: percent(100.),
                width: percent(0.),
                ..default()
            },
            BackgroundColor(Color::srgb(0.48, 0.79, 0.83)),
        ))
        .id();
    world.entity_mut(track).add_child(scrub_fill);
    world.entity_mut(toolbar).add_child(track);
    let benchmark_meshes = world.resource::<Assets<Mesh>>().ids().collect::<Vec<_>>();
    let benchmark_textures = world.resource::<Assets<Image>>().ids().collect::<Vec<_>>();
    world.insert_resource(Live {
        source,
        observation: ObservationState {
            mirror,
            timeline: PresentationTimeline::new(film.clock())?,
            error: None,
        },
        film,
        playback: Playback::default(),
        camera,
        caption,
        caption_root,
        status,
        toolbar,
        scrub_fill,
        orbit: OrbitCamera::new(pose),
        catalog,
        committed_frame: 0,
        inspection: true,
        free_camera: false,
        drag_distance: 0.,
        pointer: PointerGesture::default(),
        last_cursor: None,
        dimensions: (0, 0),
        elapsed: 0.,
        render_frames: 0,
    });
    if let Some(benchmark) = benchmark {
        use bevy::render::{
            Render, RenderApp,
            render_asset::RenderAssets,
            render_resource::{CachedPipelineState, PipelineCache},
        };
        use bevy::{render::mesh::RenderMesh, render::texture::GpuImage};
        let ready = benchmark.ready.clone();
        app.insert_resource(benchmark);
        app.sub_app_mut(RenderApp).add_systems(
            Render,
            move |pipelines: Res<PipelineCache>,
                  meshes: Res<RenderAssets<RenderMesh>>,
                  images: Res<RenderAssets<GpuImage>>| {
                if pipelines.pipelines().next().is_some()
                    && pipelines
                        .pipelines()
                        .all(|p| matches!(p.state, CachedPipelineState::Ok(_)))
                    && benchmark_meshes.iter().all(|h| meshes.get(*h).is_some())
                    && benchmark_textures.iter().all(|h| images.get(*h).is_some())
                {
                    ready.store(true, std::sync::atomic::Ordering::Release);
                }
            },
        );
    }
    app.add_systems(PreUpdate, update.after(InputSystems));
    run_app(&mut app)
}

fn run_app(app: &mut App) -> Result<(), Box<dyn std::error::Error>> {
    match app.run() {
        bevy::app::AppExit::Success => Ok(()),
        bevy::app::AppExit::Error(code) => {
            Err(format!("live application exited with error code {code}").into())
        }
    }
}
fn finish_benchmark(
    benchmark: &crate::benchmark::Benchmark,
    film: &FilmDefinition,
    dimensions: (u32, u32),
    observation_error: &Option<String>,
) -> bevy::app::AppExit {
    if let Err(error) = benchmark.finish(film, dimensions, observation_error) {
        eprintln!("benchmark write failed: {error}");
        bevy::app::AppExit::error()
    } else {
        println!(
            "BENCHMARK COMPLETE {} samples={}",
            benchmark.output.display(),
            benchmark.frames.len()
        );
        bevy::app::AppExit::Success
    }
}

fn update(world: &mut World) {
    let Some(mut live) = world.remove_resource::<Live>() else {
        return;
    };
    if let Some(mut benchmark) = world.remove_resource::<crate::benchmark::Benchmark>() {
        let samples = live.source.take_query_samples();
        if let Some((elapsed, interval)) = benchmark.sample() {
            benchmark.queries.extend(samples);
            if elapsed >= 60. {
                world.write_message(finish_benchmark(
                    &benchmark,
                    &live.film,
                    live.dimensions,
                    &live.observation.error,
                ));
            } else {
                benchmark.frames.push(serde_json::json!({"elapsed_seconds":elapsed,"interval_seconds":interval,"frame":live.committed_frame,"pending":live.observation.pending()}));
                let block = (elapsed / 10.).floor() as u32;
                live.playback.paused = true;
                let frame = match block {
                    0 | 5 => ((elapsed % 10.) * 30.) as u32,
                    1 => 299 - ((elapsed % 10.) * 30.) as u32,
                    _ => 150,
                };
                live.playback.seek(frame, live.film.frames);
                let b = bounds(&live.observation.mirror);
                live.free_camera = (2..=4).contains(&block);
                let result = match block {
                    2 => live.orbit.orbit(interval * 0.08, 0., &b),
                    3 => live.orbit.pan(interval * 0.008, 0., &b),
                    4 => live.orbit.dolly(interval * 0.008, &b),
                    _ => Ok(()),
                };
                if let Err(error) = result {
                    live.observation.error = Some(error.to_string());
                }
            }
        } else {
            benchmark.startup_queries.extend(samples);
        }
        world.insert_resource(benchmark);
    }
    let result = update_inner(world, &mut live);
    if let Err(e) = result {
        live.observation.error = Some(e.clone());
        world.entity_mut(live.status).insert((Text::new(format!("Observation or camera error: {e}\nR resets the camera; time controls remain available.")),Visibility::Visible));
    }
    crate::control_recording::sample(
        world,
        live.elapsed,
        serde_json::json!({"frame":live.committed_frame,"ticks":live.observation.mirror.current_ticks(),"paused":live.playback.paused,"rate":live.playback.rate,"camera":live.orbit.pose,"inspection":live.inspection,"selected":live.catalog.selected(),"pending":live.observation.pending(),"error":live.observation.error}),
    );
    world.insert_resource(live);
}
fn update_inner(world: &mut World, l: &mut Live) -> Result<(), String> {
    let dt = world.resource::<Time<Real>>().delta_secs_f64();
    l.elapsed += dt;
    l.render_frames += 1;
    if l.observation.poll(&mut l.source).unwrap_or(false) {
        l.committed_frame = l
            .observation
            .timeline
            .sample()
            .map_err(|e| e.to_string())?
            .frame;
        if !l.free_camera {
            l.orbit.reset(
                sample_shot(
                    &l.film,
                    l.committed_frame,
                    &positions(&l.observation.mirror),
                )
                .map_err(|e| e.to_string())?,
            );
        }
    }
    let mut windows = world.query_filtered::<&mut Window, With<PrimaryWindow>>();
    let mut w = windows.single_mut(world).map_err(|e| e.to_string())?;
    // Winit discovers the native DPI after creation. Apply the requested physical
    // preview extent once against that scale, then respect subsequent user resizing.
    if l.dimensions == (0, 0) {
        w.resolution.set_physical_resolution(1920, 1080);
    }
    let (width, height, logical_width, logical_height, scale, cursor) = (
        w.physical_width(),
        w.physical_height(),
        w.width(),
        w.height(),
        w.scale_factor(),
        w.cursor_position(),
    );
    if (width, height) != l.dimensions {
        println!(
            "viewport physical={width}x{height} logical={logical_width}x{logical_height} scale={scale}"
        );
        l.dimensions = (width, height);
    }
    if width == 0 || height == 0 {
        return Ok(());
    }
    let keys = world.resource::<ButtonInput<KeyCode>>();
    if keys.just_pressed(KeyCode::Tab) {
        l.inspection = !l.inspection;
    }
    if keys.just_pressed(KeyCode::Space) {
        l.playback.paused = !l.playback.paused;
        println!("control pause={}", l.playback.paused);
    }
    if keys.just_pressed(KeyCode::KeyV) {
        l.playback.set_rate(-l.playback.rate);
        println!("control rate={}", l.playback.rate);
    }
    if keys.just_pressed(KeyCode::Equal) {
        l.playback.set_rate(l.playback.rate * 2.);
    }
    if keys.just_pressed(KeyCode::Minus) {
        l.playback.set_rate(l.playback.rate * 0.5);
    }
    if keys.just_pressed(KeyCode::ArrowLeft) {
        l.playback.paused = true;
        l.playback
            .seek(l.playback.frame().saturating_sub(1), l.film.frames);
    }
    if keys.just_pressed(KeyCode::ArrowRight) {
        l.playback.paused = true;
        l.playback.seek(l.playback.frame() + 1, l.film.frames);
    }
    let record_toggle = keys.just_pressed(KeyCode::F9);
    let reset = keys.just_pressed(KeyCode::KeyR);
    let focus = keys.just_pressed(KeyCode::KeyF);
    let shift = keys.pressed(KeyCode::ShiftLeft) || keys.pressed(KeyCode::ShiftRight);
    if record_toggle
        && let Some(mut recording) =
            world.get_resource_mut::<crate::control_recording::ControlRecording>()
    {
        recording.toggle();
    }
    let b = bounds(&l.observation.mirror);
    let targets = pick_targets(&l.observation.mirror);
    if reset {
        l.free_camera = false;
        l.orbit.reset(
            sample_shot(
                &l.film,
                l.committed_frame,
                &positions(&l.observation.mirror),
            )
            .map_err(|e| e.to_string())?,
        );
        println!("control reset frame={}", l.committed_frame);
    }
    if focus
        && let Some(body) = targets
            .iter()
            .find(|b| Some(b.id.as_str()) == l.catalog.selected())
    {
        l.orbit.focus_target(body, &b).map_err(|e| e.to_string())?;
        l.free_camera = true;
        println!("control focus={}", body.id);
    }
    let mouse = world.resource::<ButtonInput<MouseButton>>();
    let motion = cursor
        .zip(l.last_cursor)
        .map(|(now, old)| now - old)
        .unwrap_or(Vec2::ZERO);
    l.last_cursor = cursor;
    let wheel = world.resource::<AccumulatedMouseScroll>();
    let scroll = wheel.delta
        * match wheel.unit {
            MouseScrollUnit::Line => 1.,
            MouseScrollUnit::Pixel => 0.025,
        };
    let hover_ui = l.inspection && cursor.is_some_and(|p| p.y > logical_height - 92.);
    if mouse.just_pressed(MouseButton::Left) || mouse.just_pressed(MouseButton::Middle) {
        l.pointer.begin(hover_ui);
        l.drag_distance = 0.;
    }
    if mouse.pressed(MouseButton::Left) || mouse.pressed(MouseButton::Middle) {
        l.drag_distance += motion.length();
    }
    let press = mouse.just_pressed(MouseButton::Left);
    let release = mouse.just_released(MouseButton::Left);
    let middle_release = mouse.just_released(MouseButton::Middle);
    let held = mouse.pressed(MouseButton::Left);
    let middle = mouse.pressed(MouseButton::Middle);
    let over_ui = l.pointer.owner(hover_ui) == DragOwner::Ui;
    if over_ui {
        if let Some(p) = cursor {
            if press && p.x < logical_width * 0.1 && p.y > logical_height - 60. {
                l.playback.paused = !l.playback.paused;
            }
            if (held || press || release)
                && p.x >= logical_width * 0.12
                && p.x <= logical_width * 0.96
                && p.y > logical_height - 60.
            {
                let fraction = ((p.x / logical_width - 0.12) / 0.84).clamp(0., 1.);
                l.playback.paused = true;
                l.playback.seek(
                    (fraction * (l.film.frames - 1) as f32).round() as u32,
                    l.film.frames,
                );
            }
        }
    } else {
        if (held || middle) && motion.length_squared() > 0. {
            let result = if middle || shift {
                l.orbit.pan(
                    -f64::from(motion.x) / f64::from(logical_height),
                    f64::from(motion.y) / f64::from(logical_height),
                    &b,
                )
            } else {
                l.orbit.orbit(
                    -f64::from(motion.x) * 0.004,
                    -f64::from(motion.y) * 0.004,
                    &b,
                )
            };
            if result.is_ok() {
                l.free_camera = true;
            }
        }
        if scroll.y != 0. {
            l.orbit
                .dolly(f64::from(scroll.y) * 0.08, &b)
                .map_err(|e| e.to_string())?;
            l.free_camera = true;
            println!("control dolly eye_km={:?}", l.orbit.pose.eye_km);
        }
        if release
            && l.drag_distance < 4.
            && let Some(p) = cursor
            && let Some(id) = l.orbit.pick(
                [
                    f64::from(p.x / logical_width) * 2. - 1.,
                    1. - f64::from(p.y / logical_height) * 2.,
                ],
                f64::from(width) / f64::from(height),
                &targets,
                0.02,
            )
        {
            l.catalog.select(&id, world).map_err(|e| e.to_string())?;
            println!("control selected={id}");
        }
    }
    if (release || middle_release) && l.free_camera {
        println!(
            "control camera eye_km={:?} target_km={:?}",
            l.orbit.pose.eye_km, l.orbit.pose.target_km
        );
    }
    if release || middle_release {
        l.pointer.end();
    }
    l.playback.advance(dt, l.film.frames, l.film.fps);
    let desired = l.playback.frame();
    if (!l.observation.pending() || l.playback.paused)
        && desired
            != l.observation
                .timeline
                .sample()
                .map_err(|e| e.to_string())?
                .frame
    {
        l.observation.seek(&mut l.source, desired)?;
        if l.playback.paused {
            println!("control seek frame={desired}");
        }
    }
    l.catalog
        .apply(
            world,
            &l.observation.mirror,
            &l.orbit.pose,
            SceneTarget {
                camera: l.camera,
                width,
                height,
            },
        )
        .map_err(|e| e.to_string())?;
    let caption = sample_caption(&l.film, l.committed_frame).map_err(|e| e.to_string())?;
    world.entity_mut(l.caption).insert((
        Text::new(caption),
        if l.inspection {
            Visibility::Hidden
        } else {
            Visibility::Visible
        },
    ));
    if let Some(mut node) = world.get_mut::<Node>(l.caption_root) {
        node.bottom = percent(if l.inspection { 12. } else { 6. });
    }
    if let Some(mut font) = world.get_mut::<TextFont>(l.caption) {
        font.font_size = FontSize::Px(
            (logical_height * 0.06).min(logical_width / (caption.chars().count() as f32 * 0.65)),
        );
    }
    let info = format!(
        "{}  |  frame {}/{}  |  {:.2}x{}\nSelected: {}    F Focus    R Reset\nDrag Orbit    Shift-drag / middle Pan    Wheel Dolly\n{}x{} physical; scale {:.2}; {:.1} fps mean\nSource: 100,000 ticks = 1 standard day; distances in km\nCamera: >= 2 x (radius + positive relief) from each body center\nCamera-origin distance <= 2,000,000,000 km\nMoon crater normals: cosmetic, source-cratering conditioned\nCloud shapes / 12 km altitude and haze: static cosmetics\nF on unresolved marker: aim only; retain safe eye position\n{}",
        if l.playback.paused {
            "Paused"
        } else {
            "Playing"
        },
        l.committed_frame,
        l.film.frames - 1,
        l.playback.rate,
        if l.free_camera {
            "  |  Free camera"
        } else {
            ""
        },
        l.catalog.selected().unwrap_or("none"),
        width,
        height,
        scale,
        l.render_frames as f64 / l.elapsed.max(0.001),
        l.observation
            .error
            .as_deref()
            .map(str::to_owned)
            .unwrap_or_else(|| if l.observation.pending() {
                format!(
                    "Waiting: frame {desired}; showing tick {}",
                    l.observation.mirror.current_ticks().unwrap_or(0)
                )
            } else {
                format!(
                    "Tick {}; cosmetic materials; no eclipse shadows",
                    l.observation.mirror.current_ticks().unwrap_or(0)
                )
            })
    );
    world.entity_mut(l.status).insert((
        Text::new(info),
        if l.inspection {
            Visibility::Visible
        } else {
            Visibility::Hidden
        },
    ));
    world.entity_mut(l.toolbar).insert(if l.inspection {
        Visibility::Visible
    } else {
        Visibility::Hidden
    });
    if let Some(mut n) = world.get_mut::<Node>(l.scrub_fill) {
        n.width = percent(desired as f32 / (l.film.frames - 1) as f32 * 100.);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn benchmark_app(output: PathBuf, remove_output: bool) -> App {
        let benchmark =
            crate::benchmark::Benchmark::new(output.clone(), std::time::Instant::now()).unwrap();
        if remove_output {
            std::fs::remove_dir(&output).unwrap();
        }
        let film: FilmDefinition =
            serde_json::from_str(include_str!("../films/pilot.json")).unwrap();
        let mut app = App::new();
        app.set_runner(move |_| finish_benchmark(&benchmark, &film, (1920, 1080), &None));
        app
    }

    #[test]
    fn benchmark_output_failure_reaches_app_caller() {
        let output =
            std::env::temp_dir().join(format!("planetarium-exit-failure-{}", std::process::id()));
        let mut app = benchmark_app(output.clone(), true);
        let result = run_app(&mut app);
        assert!(!output.join("samples.json").exists());
        assert!(
            result.is_err(),
            "failed benchmark output must not return success"
        );
    }

    #[test]
    fn benchmark_output_success_reaches_app_caller() {
        let output =
            std::env::temp_dir().join(format!("planetarium-exit-success-{}", std::process::id()));
        let mut app = benchmark_app(output.clone(), false);
        let result = run_app(&mut app);
        let samples: serde_json::Value =
            serde_json::from_slice(&std::fs::read(output.join("samples.json")).unwrap()).unwrap();
        std::fs::remove_dir_all(output).unwrap();
        assert!(result.is_ok());
        assert_eq!(samples["schema"], "planetarium/interactive-benchmark/v1");
    }
}
