use bevy::prelude::*;
use hornvale::prelude::*;

fn main() {
  App::new().add_plugins(DefaultPlugins).add_plugins(CameraPlugin).run();
}
