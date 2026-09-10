#![allow(
    clippy::disallowed_types,
    reason = "Instant measures GPU timeout deadlines only; source ticks alone determine simulation time"
)]
//! Persistent GPU scene and synchronous draft witness; source ticks advance only
//! after screenshot completion. Final film packaging belongs to the application.
use crate::capture::{CaptureMachine, CaptureSettings, CaptureState};
use crate::lifecycle::*;
use crate::{Binding, CameraPose, ObservationMirror, ViewError};
use bevy::{
    app::{AppLabel, SubApps},
    asset::RenderAssetUsages,
    camera::RenderTarget,
    light::Atmosphere,
    pbr::AtmosphereSettings,
    prelude::*,
    render::{
        RenderApp, RenderPlugin,
        mesh::RenderMesh,
        render_asset::RenderAssets,
        render_resource::{
            CachedPipelineState, Extent3d, PipelineCache, PollType, TextureDimension,
            TextureFormat, TextureUsages,
        },
        renderer::RenderDevice,
        texture::GpuImage,
        view::screenshot::{Screenshot, ScreenshotCaptured},
    },
    window::ExitCondition,
    winit::WinitPlugin,
};
use std::{
    path::Path,
    time::{Duration, Instant},
};
pub struct VisualPlugin;
impl Plugin for VisualPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<CaptureResult>()
            .init_resource::<PendingScene>()
            .init_resource::<AppliedObservation>()
            .add_systems(Update, apply_pending_scene);
    }
}
/// A draft offscreen renderer. It owns no source and accepts only a validated mirror.
pub struct Renderer {
    apps: SubApps,
    camera: Entity,
    atmosphere: Entity,
    atmosphere_enabled: bool,
    caption: Entity,
    target: Handle<Image>,
    diagnostic: Option<Entity>,
    catalog: SceneCatalog,
    binding: Binding,
    generation: u64,
    applied: Option<(u64, i64)>,
    pub width: u32,
    pub height: u32,
}
impl Renderer {
    /// Identity reported by the adapter actually selected by Bevy.
    pub fn adapter_identity(&self) -> (String, String) {
        let info = self
            .apps
            .main
            .world()
            .resource::<bevy::render::renderer::RenderAdapterInfo>();
        (info.name.clone(), format!("{:?}", info.backend))
    }

    pub fn new(mirror: &ObservationMirror, width: u32, height: u32) -> Result<Self, ViewError> {
        if width == 0 || height == 0 || width > 7680 || height > 4320 {
            return Err(ViewError::Range("unsupported render dimensions".into()));
        }
        let reply = mirror
            .current()
            .ok_or_else(|| ViewError::Document("render requires an accepted observation".into()))?;
        crate::documents::geometry(mirror.initial(), &reply.astronomy, KM_PER_UNIT)?;
        let mut app = App::new();
        app.add_plugins(
            DefaultPlugins
                .set(WindowPlugin {
                    primary_window: None,
                    exit_condition: ExitCondition::DontExit,
                    ..default()
                })
                .set(RenderPlugin {
                    synchronous_pipeline_compilation: true,
                    ..default()
                })
                .disable::<WinitPlugin>()
                .disable::<bevy::render::pipelined_rendering::PipelinedRenderingPlugin>(),
        )
        .add_plugins(VisualPlugin)
        .insert_resource(ClearColor(Color::BLACK))
        .insert_resource(GlobalAmbientLight::NONE);
        app.finish();
        app.cleanup();
        let world = app.world_mut();
        let mut target = Image::new_uninit(
            Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            TextureDimension::D2,
            TextureFormat::Rgba8UnormSrgb,
            RenderAssetUsages::RENDER_WORLD,
        );
        target.texture_descriptor.usage |= TextureUsages::RENDER_ATTACHMENT;
        let target = world.resource_mut::<Assets<Image>>().add(target);
        let camera = world
            .spawn((
                crate::camera::camera_components(&crate::camera::ViewSettings::default()),
                RenderTarget::Image(target.clone().into()),
            ))
            .id();
        let mut catalog = SceneCatalog::default();
        catalog.populate(world, mirror)?;
        let atmosphere = catalog.atmosphere.expect("populated");
        let caption = world
            .spawn((
                Text::new(""),
                TextFont {
                    font_size: FontSize::Px(height as f32 * 0.060),
                    ..default()
                },
                TextColor(Color::srgb(0.9, 0.89, 0.83)),
            ))
            .id();
        world
            .spawn(Node {
                position_type: PositionType::Absolute,
                bottom: percent(6.0),
                width: percent(100.0),
                justify_content: JustifyContent::Center,
                ..default()
            })
            .insert(UiTargetCamera(camera))
            .add_child(caption);
        Ok(Self {
            apps: std::mem::take(app.sub_apps_mut()),
            camera,
            atmosphere,
            atmosphere_enabled: true,
            caption,
            target,
            diagnostic: None,
            catalog,
            binding: reply.binding.clone(),
            generation: mirror.generation(),
            applied: None,
            width,
            height,
        })
    }
    /// Clear all source-owned assets and identities immediately, before a new reply.
    pub fn reset(&mut self, mirror: &ObservationMirror) -> Result<(), ViewError> {
        self.catalog.reset(self.apps.main.world_mut(), mirror)?;
        self.binding = mirror.initial().binding.clone();
        self.generation = mirror.generation();
        self.applied = None;
        self.set_caption("");
        Ok(())
    }
    /// Disable volume scattering for an explicit material comparison.
    pub fn disable_atmosphere(&mut self) {
        self.atmosphere_enabled = false;
        if let Ok(mut entity) = self.apps.main.world_mut().get_entity_mut(self.atmosphere) {
            entity.remove::<Atmosphere>();
        }
        self.apps
            .main
            .world_mut()
            .entity_mut(self.camera)
            .remove::<AtmosphereSettings>();
    }
    /// Separate calibration scene: opaque quadrant colors, numeric counter and
    /// a byte-valued center marker. Never enable this for study footage.
    pub fn diagnostic_overlay(&mut self, frame: u32) {
        let world = self.apps.main.world_mut();
        if let Some(old) = self.diagnostic.take() {
            world.despawn(old);
        }
        let root = world
            .spawn((
                Node {
                    position_type: PositionType::Absolute,
                    width: percent(100.),
                    height: percent(100.),
                    ..default()
                },
                UiTargetCamera(self.camera),
                GlobalZIndex(100),
            ))
            .id();
        for (left, top, color) in [
            (0., 0., Color::srgb_u8(255, 0, 0)),
            (50., 0., Color::srgb_u8(0, 255, 0)),
            (0., 50., Color::srgb_u8(0, 0, 255)),
            (50., 50., Color::WHITE),
        ] {
            let child = world
                .spawn((
                    Node {
                        position_type: PositionType::Absolute,
                        left: percent(left),
                        top: percent(top),
                        width: percent(50.),
                        height: percent(50.),
                        ..default()
                    },
                    BackgroundColor(color),
                ))
                .id();
            world.entity_mut(root).add_child(child);
        }
        let marker = world
            .spawn((
                Node {
                    position_type: PositionType::Absolute,
                    left: percent(40.),
                    top: percent(40.),
                    width: percent(20.),
                    height: percent(20.),
                    ..default()
                },
                BackgroundColor(Color::srgb_u8(frame as u8, 128, 64)),
            ))
            .id();
        world.entity_mut(root).add_child(marker);
        let counter = world
            .spawn((
                Text::new(format!("FRAME {frame:06}")),
                TextFont {
                    font_size: FontSize::Px(12.),
                    ..default()
                },
                TextColor(Color::BLACK),
                Node {
                    position_type: PositionType::Absolute,
                    left: percent(55.),
                    top: percent(75.),
                    ..default()
                },
            ))
            .id();
        world.entity_mut(root).add_child(counter);
        self.diagnostic = Some(root);
    }
    pub fn set_caption_font(&mut self, bytes: Vec<u8>) -> Result<(), ViewError> {
        if bytes.len() < 12 {
            return Err(ViewError::Document("caption font is truncated".into()));
        }
        let font = Font::from_bytes(bytes);
        let world = self.apps.main.world_mut();
        let font = world.resource_mut::<Assets<Font>>().add(font);
        world.entity_mut(self.caption).insert(TextFont {
            font: FontSource::Handle(font),
            font_size: FontSize::Px(self.height as f32 * 0.060),
            ..default()
        });
        Ok(())
    }
    pub fn set_caption(&mut self, caption: &str) {
        self.apps
            .main
            .world_mut()
            .entity_mut(self.caption)
            .insert(Text::new(caption));
    }
    pub fn apply(
        &mut self,
        mirror: &ObservationMirror,
        pose: &CameraPose,
    ) -> Result<(), ViewError> {
        let reply = mirror
            .current()
            .ok_or_else(|| ViewError::Document("no current observation".into()))?;
        if reply.binding != self.binding || mirror.generation() != self.generation {
            return Err(ViewError::Binding(
                "renderer must be rebuilt after binding reset".into(),
            ));
        }
        let identity = self.catalog.apply(
            self.apps.main.world_mut(),
            mirror,
            pose,
            SceneTarget {
                camera: self.camera,
                width: self.width,
                height: self.height,
            },
        )?;
        self.atmosphere = self.catalog.atmosphere.expect("populated");
        if !self.atmosphere_enabled {
            self.apps
                .main
                .world_mut()
                .entity_mut(self.atmosphere)
                .remove::<Atmosphere>();
        }
        self.applied = Some(identity);
        Ok(())
    }
    fn update(&mut self, timeout: Duration) -> Result<(), ViewError> {
        self.apps.update();
        self.apps
            .main
            .world()
            .resource::<RenderDevice>()
            .wgpu_device()
            .poll(PollType::Wait {
                submission_index: None,
                timeout: Some(timeout),
            })
            .map_err(|e| ViewError::Capture(format!("GPU poll: {e}")))?;
        Ok(())
    }
    pub fn capture(&mut self, path: &Path) -> Result<(), ViewError> {
        let mut machine = CaptureMachine::new(CaptureSettings {
            width: self.width,
            height: self.height,
            frames: 1,
            warmup_frames: 3,
            timeout_seconds: 120,
        })?;
        let start = Instant::now();
        machine.prepare(0)?;
        self.capture_acknowledged(&mut machine, path, &start)
    }
    /// The scene lock spans preparation, warmup, readback, validation and durable write.
    pub fn capture_acknowledged(
        &mut self,
        machine: &mut CaptureMachine,
        path: &Path,
        clock: &Instant,
    ) -> Result<(), ViewError> {
        let frame = match machine.state() {
            CaptureState::AwaitingObservation { frame } => *frame,
            _ => {
                return Err(ViewError::Capture(
                    "capture requires the next observation".into(),
                ));
            }
        };
        if (machine.settings().width, machine.settings().height) != (self.width, self.height) {
            return Err(machine.fail("capture target dimensions differ from settings"));
        }
        self.catalog.begin_capture(path, self.applied)?;
        let result = self
            .capture_frame(machine, frame, path, clock)
            .map_err(|error| machine.fail(&error.to_string()));
        self.catalog.finish_capture(&result);
        result
    }
    fn capture_update(
        &mut self,
        machine: &mut CaptureMachine,
        clock: &Instant,
    ) -> Result<(), ViewError> {
        let now = clock.elapsed().as_millis() as u64;
        machine.check_timeout(now)?;
        self.update(Duration::from_millis(machine.remaining_ms(now).min(1000)))?;
        machine.check_timeout(clock.elapsed().as_millis() as u64)
    }
    fn capture_frame(
        &mut self,
        machine: &mut CaptureMachine,
        frame: u32,
        path: &Path,
        clock: &Instant,
    ) -> Result<(), ViewError> {
        crate::capture::validate_scene_assets(
            self.apps.main.world(),
            &self.catalog.meshes,
            &self.catalog.textures,
        )?;
        let mut extracted = 0;
        // Extraction and asset preparation precede pipeline readiness; an empty cache
        // before the scene was extracted is not readiness.
        loop {
            self.capture_update(machine, clock)?;
            extracted += 1;
            let render = self
                .apps
                .sub_apps
                .get(&RenderApp.intern())
                .ok_or_else(|| ViewError::Capture("missing render app".into()))?
                .world();
            let text = self
                .apps
                .main
                .world()
                .get::<bevy::text::TextLayoutInfo>(self.caption);
            let caption_empty = self
                .apps
                .main
                .world()
                .get::<Text>(self.caption)
                .is_some_and(|t| t.0.is_empty());
            let assets = render.resource::<RenderAssets<RenderMesh>>();
            let images = render.resource::<RenderAssets<GpuImage>>();
            let pipelines = render.resource::<PipelineCache>();
            let text_ready = caption_empty
                || text.is_some_and(|t| {
                    !t.glyphs.is_empty()
                        && t.glyphs
                            .iter()
                            .all(|g| images.get(g.atlas_info.texture).is_some())
                });
            if self.apps.main.world().resource::<AppliedObservation>().0 == self.applied
                && text_ready
                && extracted >= 3
                && images.get(&self.target).is_some()
                && self.catalog.meshes.iter().all(|m| assets.get(m).is_some())
                && self
                    .catalog
                    .textures
                    .iter()
                    .all(|t| images.get(t).is_some())
                && pipelines.pipelines().next().is_some()
                && pipelines.waiting_pipelines().next().is_none()
            {
                if let Some(error) = pipelines.pipelines().find_map(|p| match &p.state {
                    CachedPipelineState::Err(e) => Some(e.to_string()),
                    _ => None,
                }) {
                    return Err(ViewError::Capture(format!("render pipeline: {error}")));
                }
                if pipelines
                    .pipelines()
                    .all(|p| matches!(p.state, CachedPipelineState::Ok(_)))
                {
                    break;
                }
            }
        }
        machine.observation_ready(frame, clock.elapsed().as_millis() as u64)?;
        while matches!(machine.state(), CaptureState::Warming { .. }) {
            self.capture_update(machine, clock)?;
            machine.warmed(frame, clock.elapsed().as_millis() as u64)?;
        }
        self.apps.main.world_mut().resource_mut::<CaptureResult>().0 = None;
        self.apps
            .main
            .world_mut()
            .spawn(Screenshot::image(self.target.clone()))
            .observe(
                move |event: On<ScreenshotCaptured>, mut result: ResMut<CaptureResult>| {
                    result.0 = Some((frame, Ok(event.image.clone())));
                },
            );
        loop {
            self.capture_update(machine, clock)?;
            if let Some((callback_frame, result)) = self
                .apps
                .main
                .world_mut()
                .resource_mut::<CaptureResult>()
                .0
                .take()
            {
                let status = result.as_ref().map(|_| ()).map_err(Clone::clone);
                if machine.readback(callback_frame, status, clock.elapsed().as_millis() as u64)? {
                    let image = result.map_err(ViewError::Capture)?;
                    crate::capture::write_png(&image, path, self.width, self.height)?;
                    machine.written(frame, clock.elapsed().as_millis() as u64)?;
                    return Ok(());
                }
            }
        }
    }
}
