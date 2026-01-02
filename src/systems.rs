//! Hardcoded systems for Phase 1.
//!
//! These will be replaced by data-driven rules in Phase 2.
//! For now, they demonstrate the tick loop concept.

use crate::core::World;
use crate::io::WorldIO;

/// The tick interval at which the goat baas.
const GOAT_BAA_INTERVAL: u64 = 10;

/// Run all systems for a single tick.
///
/// This is the main simulation step. Systems read world state,
/// perform their logic, and may produce output.
pub fn tick_world(world: &mut World, io: &mut dyn WorldIO) {
    // Advance the tick counter first
    world.advance_tick();

    // Run the goat-baa system
    goat_baa_system(world, io);
}

/// Run multiple ticks at once.
pub fn tick_world_n(world: &mut World, io: &mut dyn WorldIO, count: u64) {
    for _ in 0..count {
        tick_world(world, io);
    }
}

/// Hardcoded system: goats say "Baa!" periodically.
///
/// Finds all entities with Name = "goat" and prints a message
/// every GOAT_BAA_INTERVAL ticks.
fn goat_baa_system(world: &World, io: &mut dyn WorldIO) {
    let current_tick = world.tick();

    // Only trigger on the interval
    if current_tick % GOAT_BAA_INTERVAL != 0 {
        return;
    }

    // Find all goats
    for (entity, name_value) in world.entities_with("Name") {
        if let Some(name) = name_value.as_str() {
            if name == "goat" {
                io.println(&format!(
                    "[Tick {current_tick}] The goat (entity {entity}) says: Baa!"
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::TestIO;

    fn setup_world_with_goat() -> World {
        let mut world = World::new();

        // Create a room
        let room = world.create_entity();
        world.set_component(room, "Name", "A small room");

        // Create a goat
        let goat = world.create_entity();
        world.set_component(goat, "Name", "goat");

        world
    }

    #[test]
    fn test_goat_baas_at_interval() {
        let mut world = setup_world_with_goat();
        let mut io = TestIO::new(vec![]);

        // Tick up to just before the interval - no baa
        for _ in 0..(GOAT_BAA_INTERVAL - 1) {
            tick_world(&mut world, &mut io);
        }
        assert!(!io.output.contains("Baa!"));

        // One more tick should trigger the baa
        tick_world(&mut world, &mut io);
        assert!(io.output.contains("Baa!"));
        assert!(io.output.contains(&format!("Tick {GOAT_BAA_INTERVAL}")));
    }

    #[test]
    fn test_goat_baas_repeatedly() {
        let mut world = setup_world_with_goat();
        let mut io = TestIO::new(vec![]);

        // Run for multiple intervals
        tick_world_n(&mut world, &mut io, GOAT_BAA_INTERVAL * 3);

        // Should have 3 baas
        let baa_count = io.output.matches("Baa!").count();
        assert_eq!(baa_count, 3);
    }

    #[test]
    fn test_non_goat_doesnt_baa() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "sheep"); // Not a goat!

        let mut io = TestIO::new(vec![]);
        tick_world_n(&mut world, &mut io, GOAT_BAA_INTERVAL * 2);

        assert!(!io.output.contains("Baa!"));
    }
}
