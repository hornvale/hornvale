use bevy::prelude::*;

pub struct CameraPlugin;

#[derive(Component)]
struct Reticle;

/// The speed at which the reticle is moved by the player's input.
/// This has a proportional effect on the movement of the reticle.
const RETICLE_SPEED: f32 = 200.;

/// How quickly the camera should snap to the desired location.
/// This is expressed as a float relative to the delta time.
/// A higher number means a faster snap.
const CAMERA_DECAY_RATE: f32 = 1.5;

fn setup_scene(mut commands: Commands, mut meshes: ResMut<Assets<Mesh>>, mut materials: ResMut<Assets<ColorMaterial>>) {
  // World where we move the camera
  commands.spawn((
    Mesh2d(meshes.add(Rectangle::new(1000., 700.))),
    MeshMaterial2d(materials.add(Color::srgb(0.2, 0.2, 0.3))),
  ));

  // Camera
  commands.spawn((
    Reticle,
    Mesh2d(meshes.add(Circle::new(25.))),
    MeshMaterial2d(materials.add(Color::srgb(3.25, 6.4, 6.1))), // RGB values exceed 1 to achieve a bright color for the bloom effect
    Transform::from_xyz(0., 0., 2.),
  ));
}

fn setup_instructions(mut commands: Commands) {
  commands.spawn((
    Text::new("WASD: Move"),
    Node {
      position_type: PositionType::Absolute,
      bottom: Val::Px(12.0),
      left: Val::Px(12.0),
      ..default()
    },
  ));
}

fn setup_camera(mut commands: Commands) {
  commands.spawn((Camera2d, Camera { ..default() }));
}

/// Update the camera position by tracking the reticle.
fn update_camera(
  mut camera: Query<&mut Transform, (With<Camera2d>, Without<Reticle>)>,
  reticle: Query<&Transform, (With<Reticle>, Without<Camera2d>)>,
  time: Res<Time>,
) {
  let Ok(mut camera) = camera.get_single_mut() else {
    return;
  };

  let Ok(reticle) = reticle.get_single() else {
    return;
  };

  let Vec3 { x, y, .. } = reticle.translation;
  let direction = Vec3::new(x, y, camera.translation.z);

  // Applies a smooth effect to camera movement using stable interpolation
  // between the camera position and the reticle position on the x and y axes.
  camera
    .translation
    .smooth_nudge(&direction, CAMERA_DECAY_RATE, time.delta_secs());
}

fn move_reticle(
  mut reticle: Query<&mut Transform, With<Reticle>>,
  time: Res<Time>,
  kb_input: Res<ButtonInput<KeyCode>>,
) {
  let Ok(mut reticle) = reticle.get_single_mut() else {
    return;
  };

  let mut direction = Vec2::ZERO;

  if kb_input.pressed(KeyCode::KeyW) {
    direction.y += 1.;
  }

  if kb_input.pressed(KeyCode::KeyS) {
    direction.y -= 1.;
  }

  if kb_input.pressed(KeyCode::KeyA) {
    direction.x -= 1.;
  }

  if kb_input.pressed(KeyCode::KeyD) {
    direction.x += 1.;
  }

  // Progressively update the camera's position over time. Normalize the
  // direction vector to prevent it from exceeding a magnitude of 1 when
  // moving diagonally.
  let move_delta = direction.normalize_or_zero() * RETICLE_SPEED * time.delta_secs();
  reticle.translation += move_delta.extend(0.);
}

impl Plugin for CameraPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, (setup_scene, setup_instructions, setup_camera));
    app.add_systems(Update, (move_reticle, update_camera).chain());
  }
}
