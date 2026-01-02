//! Systems and tick loop for the simulation.
//!
//! Phase 2: Rules are now data-driven via RuleSet.

use crate::core::World;
use crate::io::WorldIO;
use crate::rules::RuleSet;

/// Run all systems for a single tick.
///
/// This is the main simulation step:
/// 1. Advance the tick counter
/// 2. Evaluate all rules against the current world state
pub fn tick_world(world: &mut World, rules: &mut RuleSet, io: &mut dyn WorldIO) {
    // Advance the tick counter first
    world.advance_tick();

    // Evaluate all rules
    rules.evaluate(world, io, world.tick());
}

/// Run multiple ticks at once.
pub fn tick_world_n(world: &mut World, rules: &mut RuleSet, io: &mut dyn WorldIO, count: u64) {
    for _ in 0..count {
        tick_world(world, rules, io);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::TestIO;
    use crate::rules::{Effect, Pattern, Rule, Trigger};

    /// The tick interval at which the goat baas (for tests).
    const GOAT_BAA_INTERVAL: u64 = 10;

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

    fn goat_baa_rules() -> RuleSet {
        RuleSet::from_rules(vec![Rule::new(
            "goat-baas",
            Pattern::component_value("?e", "Name", "goat"),
            Trigger::every(GOAT_BAA_INTERVAL),
            Effect::emit_message("The goat says: Baa!"),
        )])
    }

    #[test]
    fn test_goat_baas_at_interval() {
        let mut world = setup_world_with_goat();
        let mut rules = goat_baa_rules();
        let mut io = TestIO::new(vec![]);

        // Tick up to just before the interval - no baa
        for _ in 0..(GOAT_BAA_INTERVAL - 1) {
            tick_world(&mut world, &mut rules, &mut io);
        }
        assert!(!io.output.contains("Baa!"));

        // One more tick should trigger the baa
        tick_world(&mut world, &mut rules, &mut io);
        assert!(io.output.contains("Baa!"));
        assert!(io.output.contains(&format!("Tick {GOAT_BAA_INTERVAL}")));
    }

    #[test]
    fn test_goat_baas_repeatedly() {
        let mut world = setup_world_with_goat();
        let mut rules = goat_baa_rules();
        let mut io = TestIO::new(vec![]);

        // Run for multiple intervals
        tick_world_n(&mut world, &mut rules, &mut io, GOAT_BAA_INTERVAL * 3);

        // Should have 3 baas
        let baa_count = io.output.matches("Baa!").count();
        assert_eq!(baa_count, 3);
    }

    #[test]
    fn test_non_goat_doesnt_baa() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "sheep"); // Not a goat!

        let mut rules = goat_baa_rules();
        let mut io = TestIO::new(vec![]);
        tick_world_n(&mut world, &mut rules, &mut io, GOAT_BAA_INTERVAL * 2);

        assert!(!io.output.contains("Baa!"));
    }
}
