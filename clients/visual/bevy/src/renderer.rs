#![allow(
    clippy::disallowed_types,
    reason = "Instant measures GPU timeout deadlines only; source ticks alone determine simulation time"
)]
//! Persistent GPU scene and synchronous draft witness; source ticks advance only
//! after screenshot completion. Final film packaging belongs to the application.
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
    catalog: SceneCatalog,
    binding: Binding,
    generation: u64,
    applied: Option<(u64, i64)>,
    pub width: u32,
    pub height: u32,
}
impl Renderer {
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
    fn update(&mut self) -> Result<(), ViewError> {
        self.apps.update();
        self.apps
            .main
            .world()
            .resource::<RenderDevice>()
            .wgpu_device()
            .poll(PollType::Wait {
                submission_index: None,
                timeout: Some(Duration::from_secs(60)),
            })
            .map_err(|e| ViewError::Capture(format!("GPU poll: {e}")))?;
        Ok(())
    }
    pub fn capture(&mut self, path: &Path) -> Result<(), ViewError> {
        self.catalog.begin_capture(path, self.applied)?;
        let result = self.capture_frame(path);
        self.catalog.finish_capture(&result);
        result
    }
    fn capture_frame(&mut self, path: &Path) -> Result<(), ViewError> {
        if self.applied.is_none() {
            return Err(ViewError::Capture("no complete observation applied".into()));
        }
        if path.exists() {
            return Err(ViewError::Capture("capture refuses existing output".into()));
        }
        let start = Instant::now();
        let mut extracted = 0;
        // Extraction and asset preparation precede pipeline readiness; an empty cache
        // before the scene was extracted is not readiness.
        loop {
            self.update()?;
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
            if start.elapsed() > Duration::from_secs(120) {
                return Err(ViewError::Capture(
                    "asset/pipeline readiness timed out".into(),
                ));
            }
        }
        self.apps.main.world_mut().resource_mut::<CaptureResult>().0 = None;
        let path = path.to_owned();
        self.apps
            .main
            .world_mut()
            .spawn(Screenshot::image(self.target.clone()))
            .observe(
                move |event: On<ScreenshotCaptured>, mut result: ResMut<CaptureResult>| {
                    result.0 = Some(
                        event
                            .image
                            .clone()
                            .try_into_dynamic()
                            .map_err(|e| e.to_string())
                            .and_then(|img| img.to_rgb8().save(&path).map_err(|e| e.to_string())),
                    );
                },
            );
        loop {
            self.update()?;
            if let Some(result) = self
                .apps
                .main
                .world_mut()
                .resource_mut::<CaptureResult>()
                .0
                .take()
            {
                return result.map_err(ViewError::Capture);
            }
            if start.elapsed() > Duration::from_secs(180) {
                return Err(ViewError::Capture("screenshot completion timed out".into()));
            }
        }
    }
}
