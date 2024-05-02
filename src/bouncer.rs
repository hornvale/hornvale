use bevy::prelude::*;

/// A dumb bouncing text plugin.
#[derive(Debug, Clone, Copy)]
pub struct BouncerPlugin;

/// A dumb bounce component.
#[derive(Debug, Clone, Copy, Component)]
pub struct Bounce;

/// The direction of the bounce.
#[derive(Debug, Clone, Copy, Component)]
pub struct Heading(pub Vec2);

impl Plugin for BouncerPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, setup).add_systems(Update, bounce);
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
fn bounce(time: Res<Time>, mut query: Query<(&mut Heading, &mut Transform), With<Bounce>>) {
  for (mut heading, mut transform) in query.iter_mut() {
    if (transform.translation.y > 290.0 && heading.0.y > 0.0) || (transform.translation.y < -290.0 && heading.0.y < 0.0)
    {
      heading.0.y *= -1.0;
    }
    if (transform.translation.x > 390.0 && heading.0.x > 0.0) || (transform.translation.x < -390.0 && heading.0.x < 0.0)
    {
      heading.0.x *= -1.0;
    }
    heading.0 = heading.0.normalize();
    let speed = ((transform.translation.x.abs() + transform.translation.y.abs()) % 5.0 + 3.0) * 20.0;
    transform.translation.x += time.delta_seconds() * speed * heading.0.x;
    transform.translation.y += time.delta_seconds() * speed * heading.0.y;
  }
}
