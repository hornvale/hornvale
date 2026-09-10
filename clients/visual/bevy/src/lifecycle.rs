//! GPU-independent scene ownership, asset lifetime and atomic ECS application.
use crate::{Binding, CameraPose, ObservationMirror, ViewError, astronomy::surface};
use bevy::{
    camera::visibility::RenderLayers,
    light::{Atmosphere, atmosphere::ScatteringMedium},
    prelude::*,
};
pub(crate) const KM_PER_UNIT: f64 = 1000.;
#[derive(Component)]
pub(crate) struct BodyVisual {
    pub(crate) binding: Binding,
    pub(crate) id: String,
}
#[derive(Component)]
struct PointVisual {
    pub(crate) id: String,
}
#[derive(Component)]
struct StarLight {
    pub(crate) star_id: String,
}
#[derive(Component)]
struct StellarPoint {
    pub(crate) star_id: String,
}
#[derive(Resource, Default)]
pub(crate) struct CaptureResult(pub(crate) Option<Result<(), String>>);
#[derive(Resource, Default)]
pub(crate) struct PendingScene(pub(crate) Option<PreparedScene>);
#[derive(Resource, Default)]
pub(crate) struct AppliedObservation(pub(crate) Option<(u64, i64)>);
pub(crate) struct PreparedScene {
    pub(crate) identity: (u64, i64),
    pub(crate) transforms: std::collections::BTreeMap<String, Transform>,
    pub(crate) lights: std::collections::BTreeMap<String, (Transform, f32, f32)>,
    pub(crate) camera_transform: Transform,
    pub(crate) camera: Entity,
    pub(crate) atmosphere: Entity,
    pub(crate) fov: f32,
    pub(crate) width: u32,
    pub(crate) height: u32,
}

#[derive(Resource, Default)]
pub struct SceneCatalog {
    pub(crate) meshes: Vec<Handle<Mesh>>,
    pub(crate) textures: Vec<Handle<Image>>,
    materials: Vec<Handle<StandardMaterial>>,
    medium: Option<Handle<ScatteringMedium>>,
    entities: Vec<Entity>,
    pub(crate) atmosphere: Option<Entity>,
    binding: Option<Binding>,
    generation: u64,
    selected: Option<String>,
    capture: bool,
}
/// Application camera/viewport; source geometry always comes from the mirror.
pub struct SceneTarget {
    pub camera: Entity,
    pub width: u32,
    pub height: u32,
}
impl SceneCatalog {
    /// Prepare the entire snapshot first, then queue one atomic ECS application.
    pub fn apply(
        &mut self,
        world: &mut World,
        mirror: &ObservationMirror,
        pose: &CameraPose,
        target: SceneTarget,
    ) -> Result<(u64, i64), ViewError> {
        if self.capture {
            return Err(ViewError::Capture(
                "scene changes require completed capture; rebuild after failure".into(),
            ));
        }
        if target.width == 0
            || target.height == 0
            || target.width > 7680
            || target.height > 4320
            || world.get_entity(target.camera).is_err()
        {
            return Err(ViewError::Range("invalid scene camera or viewport".into()));
        }
        let reply = mirror
            .current()
            .ok_or_else(|| ViewError::Document("no current observation".into()))?;
        if self
            .binding
            .as_ref()
            .is_some_and(|b| b != &reply.binding || self.generation != mirror.generation())
        {
            return Err(ViewError::Binding(
                "scene requires reset for this source generation".into(),
            ));
        }
        let crate::scene::Geometry {
            transforms,
            lights,
            camera_transform,
        } = crate::scene::prepare(mirror, pose)?;
        if world
            .query::<&BodyVisual>()
            .iter(world)
            .any(|v| v.binding != reply.binding || !transforms.contains_key(&v.id))
        {
            return Err(ViewError::Binding(
                "stale entity binding or inventory".into(),
            ));
        }
        if self.entity_count() == 0 {
            self.populate(world, mirror)?;
        }
        world.insert_resource(mirror.clone());
        world.resource_mut::<PendingScene>().0 = Some(PreparedScene {
            identity: (reply.request_id, reply.ticks),
            transforms,
            lights,
            camera_transform,
            camera: target.camera,
            atmosphere: self.atmosphere.expect("populated"),
            fov: pose.vertical_fov_radians as f32,
            width: target.width,
            height: target.height,
        });
        Ok((reply.request_id, reply.ticks))
    }

    pub fn entity_count(&self) -> usize {
        self.entities.len()
    }
    pub fn selected(&self) -> Option<&str> {
        self.selected.as_deref()
    }
    pub fn select(&mut self, id: &str, world: &mut World) -> Result<(), ViewError> {
        if !self.entities.iter().any(|entity| {
            world
                .get::<BodyVisual>(*entity)
                .is_some_and(|b| b.id == id && Some(&b.binding) == self.binding.as_ref())
                || world
                    .get::<PointVisual>(*entity)
                    .is_some_and(|b| b.id == id)
                || world
                    .get::<StarLight>(*entity)
                    .is_some_and(|b| b.star_id == id)
        }) {
            return Err(ViewError::Binding(
                "selection is absent from current catalog".into(),
            ));
        }
        self.selected = Some(id.into());
        Ok(())
    }
    pub fn begin_capture(
        &mut self,
        path: &std::path::Path,
        applied: Option<(u64, i64)>,
    ) -> Result<(), ViewError> {
        if path.exists() {
            return Err(ViewError::Capture("capture refuses existing output".into()));
        }
        if applied.is_none() {
            return Err(ViewError::Capture("no complete observation applied".into()));
        }
        if self.capture || self.entities.is_empty() {
            return Err(ViewError::Capture(
                "capture requires a populated idle scene".into(),
            ));
        }
        self.capture = true;
        Ok(())
    }
    /// A failed readback may still own a callback. Keep reset/capture blocked;
    /// rebuild the renderer until the capture path supports safe aborts.
    pub fn finish_capture(&mut self, result: &Result<(), ViewError>) {
        if result.is_ok() {
            self.end_capture();
        }
    }
    fn end_capture(&mut self) {
        self.capture = false;
    }
    pub fn reset(
        &mut self,
        world: &mut World,
        mirror: &ObservationMirror,
    ) -> Result<(), ViewError> {
        if self.capture {
            return Err(ViewError::Capture(
                "capture cannot span source reset".into(),
            ));
        }
        for entity in self.entities.drain(..) {
            world.despawn(entity);
        }
        if let Some(h) = self.medium.take() {
            world
                .resource_mut::<Assets<ScatteringMedium>>()
                .remove(h.id());
        }
        for h in self.materials.drain(..) {
            world
                .resource_mut::<Assets<StandardMaterial>>()
                .remove(h.id());
        }
        for h in self.meshes.drain(..) {
            world.resource_mut::<Assets<Mesh>>().remove(h.id());
        }
        for h in self.textures.drain(..) {
            world.resource_mut::<Assets<Image>>().remove(h.id());
        }
        self.selected = None;
        self.atmosphere = None;
        self.binding = Some(mirror.initial().binding.clone());
        self.generation = mirror.generation();
        world.insert_resource(CaptureResult::default());
        world.insert_resource(PendingScene::default());
        world.insert_resource(AppliedObservation::default());
        world.insert_resource(mirror.clone());
        Ok(())
    }
    pub fn populate(
        &mut self,
        world: &mut World,
        mirror: &ObservationMirror,
    ) -> Result<(), ViewError> {
        if !self.entities.is_empty() {
            return Err(ViewError::Binding("catalog already populated".into()));
        }
        let reply = mirror
            .current()
            .ok_or_else(|| ViewError::Document("missing accepted observation".into()))?;
        if self
            .binding
            .as_ref()
            .is_some_and(|b| b != &reply.binding || self.generation != mirror.generation())
        {
            return Err(ViewError::Binding(
                "catalog requires matching reset before population".into(),
            ));
        }
        crate::documents::geometry(mirror.initial(), &reply.astronomy, KM_PER_UNIT)?;
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
                    medium: medium.clone(),
                },
                Transform::from_scale(Vec3::splat(1e-6)),
                GlobalTransform::from_scale(Vec3::splat(1e-6)),
                RenderLayers::layer(0),
            ))
            .id();
        let mut meshes = Vec::new();
        let mut textures = Vec::new();
        let mut materials = Vec::new();
        let mut entities = vec![atmosphere];
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
            materials.push(material.clone());
            entities.push(
                world
                    .spawn((
                        Mesh3d(mesh),
                        MeshMaterial3d(material),
                        Transform::IDENTITY,
                        BodyVisual {
                            binding: reply.binding.clone(),
                            id: body.id.clone(),
                        },
                        RenderLayers::layer(0),
                    ))
                    .id(),
            );
        }
        for light in &reply.astronomy.lights {
            entities.push(
                world
                    .spawn((
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
                    ))
                    .id(),
            );
            entities.push(
                world
                    .spawn((
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
                    ))
                    .id(),
            );
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
            materials.push(material.clone());
            entities.push(
                world
                    .spawn((
                        Mesh3d(mesh),
                        MeshMaterial3d(material),
                        Transform::IDENTITY,
                        PointVisual {
                            id: body.id.clone(),
                        },
                        RenderLayers::layer(0),
                    ))
                    .id(),
            );
        }

        self.medium = Some(medium);
        self.meshes = meshes;
        self.textures = textures;
        self.materials = materials;
        self.entities = entities;
        self.atmosphere = Some(atmosphere);
        self.binding = Some(reply.binding.clone());
        self.generation = mirror.generation();
        Ok(())
    }
}
/// Exclusive application makes one binding-qualified snapshot visible atomically;
/// transform propagation and render extraction only run after this system returns.
pub(crate) fn apply_pending_scene(world: &mut World) {
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
#[cfg(test)]
#[path = "lifecycle_tests.rs"]
mod tests;
