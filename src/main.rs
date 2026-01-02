//! Hornvale main application.
//!
//! Creates a simple world with a room and a goat, then runs the REPL.

use hornvale::{StdIO, World};

fn main() {
    // Create the world
    let mut world = World::new();

    // Create a room
    let room = world.create_entity();
    world.set_component(room, "Name", "A small room");
    world.set_component(room, "Description", "A cozy room with stone walls.");

    // Create a goat
    let goat = world.create_entity();
    world.set_component(goat, "Name", "goat");
    world.set_component(goat, "Description", "A scruffy goat with knowing eyes.");

    // Run the REPL
    let mut io = StdIO::new();
    hornvale::repl::run_repl(&mut world, &mut io);
}
