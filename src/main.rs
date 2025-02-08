use bevy::prelude::*;
use hornvale::HelloPlugin;

fn main() {
  App::new().add_plugins(DefaultPlugins).add_plugins(HelloPlugin).run();
}
