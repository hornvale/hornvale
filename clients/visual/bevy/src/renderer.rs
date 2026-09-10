#![allow(
    clippy::disallowed_types,
    reason = "Instant measures GPU timeout deadlines only; source ticks alone determine simulation time"
)]
//! Persistent GPU scene and synchronous draft witness; source ticks advance only
//! after screenshot completion. Final film packaging belongs to the application.
use crate::{
    Binding, CameraPose, ObservationMirror, ViewError,
    astronomy::{lighting, surface},
    coordinates::render_position,
};
use bevy::{
    app::{AppLabel, SubApps},
    asset::RenderAssetUsages,
    camera::{Exposure, RenderTarget, visibility::RenderLayers},
    core_pipeline::tonemapping::Tonemapping,
    light::{Atmosphere, atmosphere::ScatteringMedium},
    pbr::{AtmosphereMode, AtmosphereSettings},
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
const KM_PER_UNIT: f64 = 1000.;
#[derive(Component)]
struct BodyVisual {
    binding: Binding,
    id: String,
}
#[derive(Component)]
struct PointVisual {
    id: String,
}
#[derive(Component)]
struct StarLight {
    star_id: String,
}
#[derive(Component)]
struct StellarPoint {
    star_id: String,
}
#[derive(Resource, Default)]
struct CaptureResult(Option<Result<(), String>>);
#[derive(Resource, Default)]
struct PendingScene(Option<PreparedScene>);
#[derive(Resource, Default)]
struct AppliedObservation(Option<(u64, i64)>);
struct PreparedScene {
    identity: (u64, i64),
    transforms: std::collections::BTreeMap<String, Transform>,
    lights: std::collections::BTreeMap<String, (Transform, f32, f32)>,
    camera_transform: Transform,
    camera: Entity,
    atmosphere: Entity,
    fov: f32,
    width: u32,
    height: u32,
}
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
    caption: Entity,
    target: Handle<Image>,
    meshes: Vec<Handle<Mesh>>,
    textures: Vec<Handle<Image>>,
    binding: Binding,
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
                Camera3d::default(),
                bevy::camera::ShadowLodOrigin,
                RenderTarget::Image(target.clone().into()),
                Transform::IDENTITY,
                Exposure { ev100: 13.3 },
                Tonemapping::AcesFitted,
                Msaa::Off,
                AtmosphereSettings {
                    rendering_method: AtmosphereMode::Raymarched,
                    ..default()
                },
                RenderLayers::from_layers(&[0, 1]),
            ))
            .id();
        let radius = reply
            .astronomy
            .bodies
            .iter()
            .find(|b| b.id == "anchor")
            .and_then(|b| b.radius_km)
            .ok_or_else(|| ViewError::Document("missing anchor radius".into()))?;
        let medium = world
            .resource_mut::<Assets<ScatteringMedium>>()
            .add(ScatteringMedium::earth(256, 256).with_density_multiplier(0.18));
        let atmosphere = world
            .spawn((
                Atmosphere {
                    inner_radius: (radius * 1000.) as f32,
                    outer_radius: ((radius + 80.) * 1000.) as f32,
                    ground_albedo: Vec3::splat(0.24),
                    medium,
                },
                Transform::from_scale(Vec3::splat(1e-6)),
                GlobalTransform::from_scale(Vec3::splat(1e-6)),
                RenderLayers::layer(0),
            ))
            .id();
        let mut meshes = Vec::new();
        let mut textures = vec![target.clone()];
        for body in reply
            .astronomy
            .bodies
            .iter()
            .filter(|b| b.radius_km.is_some())
        {
            let radius = body.radius_km.expect("filtered physical bodies");
            let (mesh, texture, roughness) = if body.id == "anchor" {
                (
                    surface::globe_mesh(&mirror.initial().tiles, radius, KM_PER_UNIT),
                    surface::anchor_texture(&mirror.initial().tiles),
                    1.0,
                )
            } else {
                let moon = mirror
                    .initial()
                    .moons
                    .moons
                    .iter()
                    .find(|m| body.id == format!("moon:{}", m.index))
                    .ok_or_else(|| ViewError::Document("moon has no static descriptor".into()))?;
                (
                    Sphere::new((radius / KM_PER_UNIT) as f32)
                        .mesh()
                        .uv(192, 96),
                    surface::moon_texture(moon),
                    0.94,
                )
            };
            let mesh = world.resource_mut::<Assets<Mesh>>().add(mesh);
            meshes.push(mesh.clone());
            let texture = world.resource_mut::<Assets<Image>>().add(texture);
            textures.push(texture.clone());
            let roughness_map = if body.id == "anchor" {
                let map = world
                    .resource_mut::<Assets<Image>>()
                    .add(surface::anchor_roughness(&mirror.initial().tiles));
                textures.push(map.clone());
                Some(map)
            } else {
                None
            };
            let material = world
                .resource_mut::<Assets<StandardMaterial>>()
                .add(StandardMaterial {
                    base_color_texture: Some(texture),
                    perceptual_roughness: roughness,
                    metallic_roughness_texture: roughness_map,
                    reflectance: 0.35,
                    ..default()
                });
            world.spawn((
                Mesh3d(mesh),
                MeshMaterial3d(material),
                Transform::IDENTITY,
                BodyVisual {
                    binding: reply.binding.clone(),
                    id: body.id.clone(),
                },
                RenderLayers::layer(0),
            ));
        }
        for light in &reply.astronomy.lights {
            world.spawn((
                DirectionalLight {
                    illuminance: 0.0,
                    shadow_maps_enabled: false,
                    ..default()
                },
                bevy::light::SunDisk::OFF,
                Transform::IDENTITY,
                StarLight {
                    star_id: light.star_id.clone(),
                },
                RenderLayers::layer(1),
            ));
            world.spawn((
                PointLight {
                    intensity: 0.0,
                    range: 1e9,
                    radius: 0.0,
                    shadow_maps_enabled: false,
                    ..default()
                },
                Transform::IDENTITY,
                StellarPoint {
                    star_id: light.star_id.clone(),
                },
                RenderLayers::layer(0),
            ));
        }
        // Unresolved wanderers are two-pixel point markers, never sized bodies.
        for body in reply
            .astronomy
            .bodies
            .iter()
            .filter(|b| b.kind == "wanderer")
        {
            let mesh = world
                .resource_mut::<Assets<Mesh>>()
                .add(Rectangle::new(1.0, 1.0));
            meshes.push(mesh.clone());
            let material = world
                .resource_mut::<Assets<StandardMaterial>>()
                .add(StandardMaterial {
                    base_color: Color::srgb(0.62, 0.7, 0.76),
                    unlit: true,
                    ..default()
                });
            world.spawn((
                Mesh3d(mesh),
                MeshMaterial3d(material),
                Transform::IDENTITY,
                PointVisual {
                    id: body.id.clone(),
                },
                RenderLayers::layer(0),
            ));
        }
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
            caption,
            target,
            meshes,
            textures,
            binding: reply.binding.clone(),
            applied: None,
            width,
            height,
        })
    }
    /// Disable the cosmetic atmospheric treatment for an explicit body-only view.
    pub fn disable_atmosphere(&mut self) {
        self.apps
            .main
            .world_mut()
            .entity_mut(self.atmosphere)
            .remove::<Atmosphere>();
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
        if reply.binding != self.binding {
            return Err(ViewError::Binding(
                "renderer must be rebuilt after binding reset".into(),
            ));
        }
        // Validate every conversion before touching ECS: a bad snapshot never half-applies.
        let camera = pose.transform(KM_PER_UNIT)?;
        let mut transforms = std::collections::BTreeMap::new();
        for body in &reply.astronomy.bodies {
            let position =
                Vec3::from_array(render_position(body.position_km, pose.eye_km, KM_PER_UNIT)?);
            let rotation = body
                .body_to_frame
                .map(|c| {
                    Quat::from_mat3(&Mat3::from_cols_array_2d(
                        &c.map(|col| col.map(|x| x as f32)),
                    ))
                })
                .unwrap_or(Quat::IDENTITY);
            transforms.insert(
                body.id.clone(),
                Transform::from_translation(position).with_rotation(rotation),
            );
        }
        let mut lights = std::collections::BTreeMap::new();
        let anchor = reply
            .astronomy
            .bodies
            .iter()
            .find(|b| b.id == "anchor")
            .expect("validated anchor");
        for light in &reply.astronomy.lights {
            let star = reply
                .astronomy
                .bodies
                .iter()
                .find(|b| b.id == light.star_id)
                .expect("validated light star");
            let (direction, flux) = lighting::at_body(light, star, anchor)?;
            let direction = Vec3::from_array(direction);
            let up = if direction.dot(Vec3::Z).abs() < 0.99 {
                Vec3::Z
            } else {
                Vec3::Y
            };
            let intensity = lighting::point_intensity(light.luminosity_rel, KM_PER_UNIT)?;
            lights.insert(
                light.star_id.clone(),
                (
                    Transform::IDENTITY.looking_to(-direction, up),
                    127_000.0 * flux,
                    intensity,
                ),
            );
        }
        let world = self.apps.main.world_mut();
        if world
            .query::<&BodyVisual>()
            .iter(world)
            .any(|v| v.binding != reply.binding || !transforms.contains_key(&v.id))
        {
            return Err(ViewError::Binding(
                "stale entity binding or inventory".into(),
            ));
        }
        world.insert_resource(mirror.clone());
        world.resource_mut::<PendingScene>().0 = Some(PreparedScene {
            identity: (reply.request_id, reply.ticks),
            transforms,
            lights,
            camera_transform: camera,
            camera: self.camera,
            atmosphere: self.atmosphere,
            fov: pose.vertical_fov_radians as f32,
            width: self.width,
            height: self.height,
        });
        self.applied = Some((reply.request_id, reply.ticks));
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
                && self.meshes.iter().all(|m| assets.get(m).is_some())
                && self.textures.iter().all(|t| images.get(t).is_some())
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

/// Exclusive application makes one binding-qualified snapshot visible atomically;
/// transform propagation and render extraction only run after this system returns.
fn apply_pending_scene(world: &mut World) {
    let Some(scene) = world.resource_mut::<PendingScene>().0.take() else {
        return;
    };
    let transforms = &scene.transforms;
    let lights = &scene.lights;
    for (visual, mut t) in world
        .query::<(&BodyVisual, &mut Transform)>()
        .iter_mut(world)
    {
        *t = transforms[&visual.id];
    }
    for (key, mut t, mut light) in world
        .query::<(&StarLight, &mut Transform, &mut DirectionalLight)>()
        .iter_mut(world)
    {
        let (transform, illuminance, _) = lights[&key.star_id];
        *t = transform;
        light.illuminance = illuminance;
    }
    for (key, mut t, mut light) in world
        .query::<(&StellarPoint, &mut Transform, &mut PointLight)>()
        .iter_mut(world)
    {
        *t = transforms[&key.star_id];
        light.intensity = lights[&key.star_id].2;
    }
    for (point, mut t) in world
        .query::<(&PointVisual, &mut Transform)>()
        .iter_mut(world)
    {
        let position = transforms[&point.id].translation;
        let diameter =
            position.length() * 2.0 * (scene.fov / 2.0).tan() / scene.height as f32 * 2.0;
        *t = Transform::from_translation(position)
            .with_rotation(scene.camera_transform.rotation)
            .with_scale(Vec3::splat(diameter));
    }
    world.entity_mut(scene.camera).insert((
        scene.camera_transform,
        Projection::Perspective(PerspectiveProjection {
            fov: scene.fov,
            aspect_ratio: scene.width as f32 / scene.height as f32,
            near: 0.0001,
            far: 2e7,
            ..default()
        }),
    ));
    let center = transforms["anchor"].translation;
    world
        .entity_mut(scene.atmosphere)
        .insert(Transform::from_translation(center).with_scale(Vec3::splat(1e-6)));

    world.resource_mut::<AppliedObservation>().0 = Some(scene.identity);
}
