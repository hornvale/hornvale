//! Rule evaluation engine.
//!
//! The engine manages a set of rules and evaluates them against the world state.

use im::OrdMap;

use crate::core::World;
use crate::io::WorldIO;
use crate::symbol::Symbol;

use super::rule::{Rule, Trigger};

/// A collection of rules with evaluation state.
#[derive(Debug, Clone)]
pub struct RuleSet {
    /// The rules in this set.
    rules: Vec<Rule>,
    /// Last tick each rule fired (by rule name).
    last_fired: OrdMap<Symbol, u64>,
}

impl RuleSet {
    /// Create a new empty rule set.
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            last_fired: OrdMap::new(),
        }
    }

    /// Create a rule set from a list of rules.
    pub fn from_rules(rules: Vec<Rule>) -> Self {
        Self {
            rules,
            last_fired: OrdMap::new(),
        }
    }

    /// Add a rule to the set.
    pub fn add_rule(&mut self, rule: Rule) {
        self.rules.push(rule);
    }

    /// Get the number of rules.
    pub fn len(&self) -> usize {
        self.rules.len()
    }

    /// Check if the rule set is empty.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    /// Iterate over rules.
    pub fn rules(&self) -> impl Iterator<Item = &Rule> {
        self.rules.iter()
    }

    /// Get a rule by name.
    pub fn get_rule(&self, name: &str) -> Option<&Rule> {
        let name_sym = Symbol::new(name);
        self.rules.iter().find(|r| r.name == name_sym)
    }

    /// Evaluate all rules against the current world state.
    ///
    /// For periodic rules, checks if enough ticks have elapsed since last firing.
    /// For each matching entity, executes the rule's effect.
    pub fn evaluate(&mut self, world: &World, io: &mut dyn WorldIO, current_tick: u64) {
        for rule in &self.rules {
            if self.should_fire(rule, current_tick) {
                self.evaluate_rule(world, io, rule, current_tick);
                self.last_fired.insert(rule.name, current_tick);
            }
        }
    }

    /// Check if a rule should fire at the current tick.
    fn should_fire(&self, rule: &Rule, current_tick: u64) -> bool {
        match &rule.trigger {
            Trigger::Periodic { interval } => {
                // Fire on ticks that are multiples of the interval
                // (tick 10, 20, 30, etc. for interval=10)
                if current_tick == 0 || *interval == 0 {
                    return false;
                }
                current_tick % interval == 0
            }
        }
    }

    /// Evaluate a single rule against all entities.
    fn evaluate_rule(&self, world: &World, io: &mut dyn WorldIO, rule: &Rule, current_tick: u64) {
        // Find all entities that match the pattern
        for entity in world.all_entities() {
            if rule.pattern.matches(world, entity) {
                rule.effect.execute(world, io, entity, current_tick);
            }
        }
    }
}

impl Default for RuleSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::TestIO;
    use crate::rules::{Effect, Pattern};

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

    fn goat_baa_rule() -> Rule {
        Rule::new(
            "goat-baas",
            Pattern::component_value("?e", "Name", "goat"),
            Trigger::every(10),
            Effect::emit_message("The goat says: Baa!"),
        )
    }

    #[test]
    fn test_rule_fires_at_interval() {
        let world = setup_world_with_goat();
        let mut rules = RuleSet::from_rules(vec![goat_baa_rule()]);
        let mut io = TestIO::new(vec![]);

        // Ticks 1-9: no baa
        for tick in 1..10 {
            rules.evaluate(&world, &mut io, tick);
        }
        assert!(!io.output.contains("Baa!"));

        // Tick 10: baa!
        rules.evaluate(&world, &mut io, 10);
        assert!(io.output.contains("Baa!"));
        assert!(io.output.contains("Tick 10"));
    }

    #[test]
    fn test_rule_fires_repeatedly() {
        let world = setup_world_with_goat();
        let mut rules = RuleSet::from_rules(vec![goat_baa_rule()]);
        let mut io = TestIO::new(vec![]);

        // Ticks 1-30
        for tick in 1..=30 {
            rules.evaluate(&world, &mut io, tick);
        }

        // Should have 3 baas (at ticks 10, 20, 30)
        let baa_count = io.output.matches("Baa!").count();
        assert_eq!(baa_count, 3);
    }

    #[test]
    fn test_non_matching_entity_doesnt_trigger() {
        let mut world = World::new();
        world.create_entity(); // entity 0
        let sheep = world.create_entity();
        world.set_component(sheep, "Name", "sheep"); // Not a goat!

        let mut rules = RuleSet::from_rules(vec![goat_baa_rule()]);
        let mut io = TestIO::new(vec![]);

        for tick in 1..=20 {
            rules.evaluate(&world, &mut io, tick);
        }

        assert!(!io.output.contains("Baa!"));
    }

    #[test]
    fn test_multiple_matching_entities() {
        let mut world = World::new();
        let goat1 = world.create_entity();
        world.set_component(goat1, "Name", "goat");
        let goat2 = world.create_entity();
        world.set_component(goat2, "Name", "goat");

        let mut rules = RuleSet::from_rules(vec![goat_baa_rule()]);
        let mut io = TestIO::new(vec![]);

        rules.evaluate(&world, &mut io, 10);

        // Both goats should baa
        let baa_count = io.output.matches("Baa!").count();
        assert_eq!(baa_count, 2);
    }

    #[test]
    fn test_multiple_rules() {
        let mut world = World::new();
        let goat = world.create_entity();
        world.set_component(goat, "Name", "goat");
        let dog = world.create_entity();
        world.set_component(dog, "Name", "dog");

        let rules = RuleSet::from_rules(vec![
            Rule::new(
                "goat-baas",
                Pattern::component_value("?e", "Name", "goat"),
                Trigger::every(10),
                Effect::emit_message("Baa!"),
            ),
            Rule::new(
                "dog-barks",
                Pattern::component_value("?e", "Name", "dog"),
                Trigger::every(5),
                Effect::emit_message("Woof!"),
            ),
        ]);

        let mut rules = rules;
        let mut io = TestIO::new(vec![]);

        // At tick 10, both should fire
        for tick in 1..=10 {
            rules.evaluate(&world, &mut io, tick);
        }

        assert!(io.output.contains("Baa!"));
        assert!(io.output.contains("Woof!"));
    }

    #[test]
    fn test_get_rule_by_name() {
        let rules = RuleSet::from_rules(vec![goat_baa_rule()]);

        let rule = rules.get_rule("goat-baas");
        assert!(rule.is_some());
        assert_eq!(rule.unwrap().name.as_str(), "goat-baas");

        let not_found = rules.get_rule("nonexistent");
        assert!(not_found.is_none());
    }
}
