//! Hornvale main application.
//!
//! Creates a simple world with a room and a goat, then runs the REPL.

use hornvale::rules::{Effect, Pattern, Rule, RuleSet, Trigger};
use hornvale::{Cardinality, RelationSchema, StdIO, World};

fn main() {
    // Create the world
    let mut world = World::new();

    // Register the Location relation (many entities can be in one location)
    world.register_relation(RelationSchema::new(
        "Location",
        Cardinality::Many,
        Cardinality::One,
    ));

    // Create a room
    let room = world.create_entity();
    world.set_component(room, "Name", "A small room");
    world.set_component(room, "Description", "A cozy room with stone walls.");

    // Create a goat in the room
    let goat = world.create_entity();
    world.set_component(goat, "Name", "goat");
    world.set_component(goat, "Description", "A scruffy goat with knowing eyes.");
    world.add_relation("Location", goat, room);

    // Create the rules
    // Rule: entities named "goat" that have a Location say "Baa!" every 10 ticks
    let mut rules = RuleSet::from_rules(vec![Rule::new(
        "goat-baas",
        Pattern::and(vec![
            Pattern::component_value("?e", "Name", "goat"),
            Pattern::in_relation("Location", "?e", "?_"),
        ]),
        Trigger::every(10),
        Effect::emit_message("The goat says: Baa!"),
    )]);

    // Run the REPL
    let mut io = StdIO::new();
    hornvale::repl::run_repl(&mut world, &mut rules, &mut io);
}
