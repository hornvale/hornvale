use bevy::prelude::*;
use bevy::window::WindowResized;

/// A dumb bouncing text plugin.
#[derive(Debug, Clone, Copy)]
pub struct BouncerPlugin;

/// A dumb bounce component.
#[derive(Debug, Clone, Copy, Component)]
pub struct Bounce;

/// The direction of the bounce.
#[derive(Debug, Clone, Copy, Component)]
pub struct Heading(pub Vec2);

/// The dimensions of the window.
#[derive(Debug, Clone, Copy, Resource)]
pub struct WindowDimensions(pub Vec2);

impl Plugin for BouncerPlugin {
  fn build(&self, app: &mut App) {
    app
      .insert_resource(WindowDimensions(Vec2::new(800.0, 600.0)))
      .add_systems(Startup, setup)
      .add_systems(Update, (bounce, on_resize_system));
  }
}

/// The startup system.
fn setup(mut commands: Commands) {
  for i in (-60..60_i32).step_by(1) {
    for j in (-60..60_i32).step_by(1) {
      commands.spawn((
        Text2dBundle {
          text: Text {
            sections: vec![TextSection {
              value: "T".to_string(),
              style: TextStyle {
                font_size: 20.0,
                color: Color::rgb(
                  i.abs() as f32 / 60.0,
                  j.abs() as f32 / 60.0,
                  (i + j).abs() as f32 / 180.0,
                ),
                ..Default::default()
              },
            }],
            ..Default::default()
          },
          transform: Transform {
            translation: Vec3::new(i as f32, j as f32, 0.0),
            ..Default::default()
          },
          ..Default::default()
        },
        Heading(Vec2::new(j as f32, i as f32).normalize()),
        Bounce,
      ));
    }
  }
}

/// The update system.
fn bounce(
  time: Res<Time>,
  window_dimensions: ResMut<WindowDimensions>,
  mut query: Query<(&mut Heading, &mut Transform), With<Bounce>>,
) {
  let min_x = -window_dimensions.0.x / 2.0;
  let max_x = window_dimensions.0.x / 2.0;
  let min_y = -window_dimensions.0.y / 2.0;
  let max_y = window_dimensions.0.y / 2.0;
  for (mut heading, mut transform) in query.iter_mut() {
    if (transform.translation.y > max_y && heading.0.y > 0.0) || (transform.translation.y < min_y && heading.0.y < 0.0)
    {
      heading.0.y *= -1.0;
    }
    if (transform.translation.x > max_x && heading.0.x > 0.0) || (transform.translation.x < min_x && heading.0.x < 0.0)
    {
      heading.0.x *= -1.0;
    }
    heading.0 = heading.0.normalize();
    let speed = ((transform.translation.x.abs() + transform.translation.y.abs()) % 5.0 + 3.0) * 20.0;
    transform.translation.x += time.delta_seconds() * speed * heading.0.x;
    transform.translation.y += time.delta_seconds() * speed * heading.0.y;
  }
}

/// This system shows how to respond to a window being resized.
fn on_resize_system(mut window_dimensions: ResMut<WindowDimensions>, mut resize_reader: EventReader<WindowResized>) {
  for e in resize_reader.read() {
    window_dimensions.0 = Vec2::new(e.width, e.height);
  }
}
