use bevy::input::{keyboard::KeyboardInput, ButtonState};
use bevy::prelude::*;

/// A player.
#[derive(Debug, Clone, Copy)]
pub struct PlayerPlugin;

/// Player component.
#[derive(Debug, Clone, Copy, Component)]
pub struct Player;

impl Plugin for PlayerPlugin {
  fn build(&self, app: &mut App) {
    app.add_systems(Startup, setup).add_systems(Update, control_player);
  }
}

/// The startup system.
fn setup(mut commands: Commands) {
  commands.spawn((
    Text2dBundle {
      text: Text {
        sections: vec![TextSection {
          value: "@".to_string(),
          style: TextStyle {
            font_size: 20.0,
            color: Color::WHITE,
            ..Default::default()
          },
        }],
        ..Default::default()
      },
      transform: Transform {
        translation: Vec3::new(0.0, 0.0, 0.0),
        ..Default::default()
      },
      ..Default::default()
    },
    Player,
  ));
}

/// Control the player.
fn control_player(
  mut keyboard_input_events: EventReader<KeyboardInput>,
  mut query: Query<&mut Transform, With<Player>>,
  time: Res<Time>,
) {
  let mut player_transform = query.single_mut();
  let mut direction = (0.0, 0.0);
  for event in keyboard_input_events.read() {
    if event.state == ButtonState::Pressed {
      match event.key_code {
        KeyCode::ArrowUp => direction.1 = 1.0,
        KeyCode::ArrowDown => direction.1 = -1.0,
        KeyCode::ArrowLeft => direction.0 = -1.0,
        KeyCode::ArrowRight => direction.0 = 1.0,
        _ => {},
      }
    }
    let new_position = (
      player_transform.translation.x + direction.0 * 1000.0 * time.delta_seconds(),
      player_transform.translation.y + direction.1 * 1000.0 * time.delta_seconds(),
    );
    player_transform.translation = Vec3::new(new_position.0, new_position.1, 0.0);
  }
}
