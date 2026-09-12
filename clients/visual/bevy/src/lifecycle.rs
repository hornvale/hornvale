//! GPU-independent scene ownership, asset lifetime and atomic ECS application.
use crate::{
    Binding, CameraPose, ObservationMirror, ViewError,
    astronomy::surface,
    documents::{self, SurfacePatchCacheKey, SurfacePatchDocument},
};
use bevy::{
    camera::visibility::RenderLayers,
    light::{Atmosphere, atmosphere::ScatteringMedium},
    prelude::*,
};
use std::collections::BTreeMap;
use std::sync::{Mutex, OnceLock};
pub const KM_PER_UNIT: f64 = 1000.;
#[derive(Component)]
pub(crate) struct BodyVisual {
    pub(crate) binding: Binding,
    pub(crate) id: String,
}
#[derive(Component)]
struct CosmeticCloud;
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
pub(crate) struct CaptureResult(pub(crate) Option<(u32, Result<Image, String>)>);
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
    pub(crate) focus: f32,
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

/// Mesh and material data prepared from one revision-qualified patch.
pub struct SurfaceMeshHandles {
    pub key: SurfacePatchCacheKey,
    pub mesh: Mesh,
    pub material: StandardMaterial,
}

#[derive(Default)]
struct SurfacePatchState {
    generation: u64,
    pending: BTreeMap<(Binding, u64, u64), SurfacePatchCacheKey>,
}

static ACTIVE_SURFACE_PATCHES: OnceLock<Mutex<SurfacePatchState>> = OnceLock::new();

fn surface_patch_state() -> &'static Mutex<SurfacePatchState> {
    ACTIVE_SURFACE_PATCHES.get_or_init(|| Mutex::new(SurfacePatchState::default()))
}

/// Invalidate every outstanding surface request when the source mirror resets.
pub fn reset_surface_patches() {
    let mut state = surface_patch_state()
        .lock()
        .expect("surface schedule state is not poisoned");
    state.generation = state
        .generation
        .checked_add(1)
        .expect("surface generation overflow");
    state.pending.clear();
}

/// Generation to put on a new source request.
pub fn surface_patch_generation() -> u64 {
    surface_patch_state()
        .lock()
        .expect("surface schedule state is not poisoned")
        .generation
}

/// Register a source request as the active patch generation.
///
/// The request body is intentionally opaque here: source-side schema validation
/// remains the source's responsibility. The renderer records only the cache
/// identity needed to reject a late document from another observation.
pub fn schedule_surface_patch(
    mut key: SurfacePatchCacheKey,
    request: String,
) -> Result<(), ViewError> {
    let request: documents::SurfaceRequestDocument = serde_json::from_str(&request)
        .map_err(|error| ViewError::Document(format!("invalid surface request: {error}")))?;
    if request.schema != "visual/surface-request/v1" {
        return Err(ViewError::Document("unknown surface request schema".into()));
    }
    request.binding.validate()?;
    if request.expected_revision.source_revision != request.binding.source_revision {
        return Err(ViewError::Binding(
            "surface request revision does not match its binding".into(),
        ));
    }
    documents::validate_surface_patch_revision(&request.expected_revision)?;
    documents::validate_surface_patch_address(&request.address)?;
    if let Some(transition_address) = &request.transition_address {
        documents::validate_surface_patch_address(transition_address)?;
    }
    if key.revision == request.expected_revision.source_revision {
        key.revision = request.expected_revision.cache_token();
    }
    if key.revision != request.expected_revision.cache_token()
        || key.macro_face != request.address.macro_face
        || key.child_path != request.address.child_path
    {
        return Err(ViewError::Binding(
            "surface request does not match its cache key".into(),
        ));
    }
    let mut state = surface_patch_state()
        .lock()
        .expect("surface schedule state is not poisoned");
    if request.generation != state.generation {
        return Err(ViewError::Binding(
            "surface request belongs to a stale source generation".into(),
        ));
    }
    let identity = (
        request.binding.clone(),
        request.request_id,
        request.generation,
    );
    if state.pending.contains_key(&identity) {
        return Err(ViewError::Binding(
            "surface request ID is already scheduled for this binding".into(),
        ));
    }
    state.pending.insert(identity, key);
    Ok(())
}

/// Apply a source-owned patch only if it belongs to the currently scheduled
/// observation. Asset creation is pure until the caller inserts the returned
/// handles into Bevy's asset collections.
pub fn apply_surface_patch(
    document: &SurfacePatchDocument,
) -> Result<SurfaceMeshHandles, ViewError> {
    documents::validate_surface_patch(document)?;
    let key = document.cache_key();
    let mut state = surface_patch_state()
        .lock()
        .expect("surface schedule state is not poisoned");
    let matches: Vec<_> = state
        .pending
        .iter()
        .filter(|(_, scheduled)| *scheduled == &key)
        .map(|(identity, _)| identity.clone())
        .collect();
    let [identity] = matches.as_slice() else {
        return Err(ViewError::Binding(
            if matches.is_empty() {
                "surface patch has no active scheduled request"
            } else {
                "surface patch is ambiguous across concurrent requests"
            }
            .into(),
        ));
    };
    state.pending.remove(identity);
    Ok(SurfaceMeshHandles {
        key,
        mesh: surface::surface_mesh(document, None),
        material: surface::surface_material(document),
    })
}

/// Decode, identity-check and apply a complete source reply atomically.
pub fn apply_surface_reply(json: &str) -> Result<SurfaceMeshHandles, ViewError> {
    let reply = documents::surface_reply(json)?;
    let key = reply.patch.cache_key();
    let mut state = surface_patch_state()
        .lock()
        .expect("surface schedule state is not poisoned");
    let identity = (reply.binding.clone(), reply.request_id, reply.generation);
    if reply.generation != state.generation {
        return Err(ViewError::Binding(
            "surface reply belongs to a stale source generation".into(),
        ));
    }
    if state.pending.get(&identity) != Some(&key) {
        return Err(ViewError::Binding(
            "surface reply belongs to a stale binding, request, generation, or patch".into(),
        ));
    }
    state.pending.remove(&identity);
    drop(state);
    Ok(SurfaceMeshHandles {
        key,
        mesh: surface::surface_mesh(&reply.patch, None),
        material: surface::surface_material(&reply.patch),
    })
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
            focus: (pose.focus_distance_km / KM_PER_UNIT) as f32,
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
        reset_surface_patches();
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
            let mut mesh = mesh;
            let normal_map = if body.kind == "moon" {
                mesh.generate_tangents()
                    .map_err(|e| ViewError::Document(format!("moon tangents: {e}")))?;
                let moon = mirror
                    .initial()
                    .moons
                    .moons
                    .iter()
                    .find(|m| body.id == format!("moon:{}", m.index))
                    .unwrap();
                let normal = world
                    .resource_mut::<Assets<Image>>()
                    .add(surface::moon_normal(moon));
                textures.push(normal.clone());
                Some(normal)
            } else {
                None
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
                    normal_map_texture: normal_map,
                    perceptual_roughness: roughness,
                    metallic_roughness_texture: roughness_map,
                    reflectance: crate::camera::ViewSettings::default().reflectance,
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
        // A static, source-coverage-conditioned presentation layer. It never casts
        // an eclipse/cloud shadow and is not part of the physical body inventory.
        let cloud_mesh = world
            .resource_mut::<Assets<Mesh>>()
            .add(surface::cloud_shell(
                &mirror.initial().tiles,
                radius,
                KM_PER_UNIT,
            ));
        let cloud_texture = world
            .resource_mut::<Assets<Image>>()
            .add(surface::cloud_texture(&mirror.initial().tiles));
        let cloud_material =
            world
                .resource_mut::<Assets<StandardMaterial>>()
                .add(StandardMaterial {
                    base_color_texture: Some(cloud_texture.clone()),
                    alpha_mode: AlphaMode::Blend,
                    perceptual_roughness: 1.,
                    reflectance: 0.0,
                    ..default()
                });
        meshes.push(cloud_mesh.clone());
        textures.push(cloud_texture);
        materials.push(cloud_material.clone());
        entities.push(
            world
                .spawn((
                    Mesh3d(cloud_mesh),
                    MeshMaterial3d(cloud_material),
                    Transform::IDENTITY,
                    CosmeticCloud,
                    bevy::light::NotShadowCaster,
                    RenderLayers::layer(0),
                ))
                .id(),
        );
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
    for mut t in world
        .query_filtered::<&mut Transform, With<CosmeticCloud>>()
        .iter_mut(world)
    {
        *t = transforms["anchor"];
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
    if let Some(mut focus) = world.get_mut::<bevy::post_process::dof::DepthOfField>(scene.camera) {
        focus.focal_distance = scene.focus;
    }
    let center = transforms["anchor"].translation;
    world
        .entity_mut(scene.atmosphere)
        .insert(Transform::from_translation(center).with_scale(Vec3::splat(1e-6)));

    world.resource_mut::<AppliedObservation>().0 = Some(scene.identity);
}
#[cfg(test)]
#[path = "lifecycle_tests.rs"]
mod tests;
