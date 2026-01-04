//! Rule evaluation engine.
//!
//! The engine manages a set of rules and evaluates them against the world state.
//!
//! ## Hook Rules
//!
//! Hook rules use Before/On/After triggers and are executed by the action system,
//! not during tick evaluation. Use `before_hooks`, `on_hooks`, and `after_hooks`
//! to query hook rules for a specific action.

use im::OrdMap;

use crate::core::{EntityId, World};
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

    // =========================================================================
    // Hook Rule Queries
    // =========================================================================

    /// Get all Before hook rules for an action.
    pub fn before_hooks(&self, action: &str) -> impl Iterator<Item = &Rule> {
        let action_sym = Symbol::new(action);
        self.rules
            .iter()
            .filter(move |r| matches!(&r.trigger, Trigger::Before(a) if *a == action_sym))
    }

    /// Get all On hook rules for an action.
    pub fn on_hooks(&self, action: &str) -> impl Iterator<Item = &Rule> {
        let action_sym = Symbol::new(action);
        self.rules
            .iter()
            .filter(move |r| matches!(&r.trigger, Trigger::On(a) if *a == action_sym))
    }

    /// Get all After hook rules for an action.
    pub fn after_hooks(&self, action: &str) -> impl Iterator<Item = &Rule> {
        let action_sym = Symbol::new(action);
        self.rules
            .iter()
            .filter(move |r| matches!(&r.trigger, Trigger::After(a) if *a == action_sym))
    }

    /// Get all hook rules (Before, On, After) for an action.
    pub fn hooks_for_action(&self, action: &str) -> impl Iterator<Item = &Rule> {
        let action_sym = Symbol::new(action);
        self.rules
            .iter()
            .filter(move |r| r.trigger.is_hook() && r.trigger.action() == Some(action_sym))
    }

    // =========================================================================
    // Derivation Rule Queries
    // =========================================================================

    /// Get all derivation rules for a property.
    pub fn derive_rules(&self, property: &str) -> impl Iterator<Item = &Rule> {
        let property_sym = Symbol::new(property);
        self.rules
            .iter()
            .filter(move |r| matches!(&r.trigger, Trigger::Derive(p) if *p == property_sym))
    }

    /// Get derivation rules that match a specific entity for a property.
    pub fn matching_derive_rules<'a>(
        &'a self,
        world: &'a World,
        entity: EntityId,
        property: &str,
    ) -> impl Iterator<Item = &'a Rule> + 'a {
        let property_sym = Symbol::new(property);
        self.rules.iter().filter(move |r| {
            matches!(&r.trigger, Trigger::Derive(p) if *p == property_sym)
                && r.pattern.matches(world, entity)
        })
    }

    /// Get hook rules that match a specific entity for an action phase.
    ///
    /// This filters hook rules to only those whose pattern matches the entity.
    pub fn matching_hooks<'a>(
        &'a self,
        world: &'a World,
        entity: EntityId,
        phase: &'static str,
        action: &str,
    ) -> impl Iterator<Item = &'a Rule> + 'a {
        let action_sym = Symbol::new(action);
        self.rules.iter().filter(move |r| {
            // Check trigger matches phase and action
            let trigger_matches = match phase {
                "Before" => matches!(&r.trigger, Trigger::Before(a) if *a == action_sym),
                "On" => matches!(&r.trigger, Trigger::On(a) if *a == action_sym),
                "After" => matches!(&r.trigger, Trigger::After(a) if *a == action_sym),
                _ => false,
            };

            // Check pattern matches entity
            trigger_matches && r.pattern.matches(world, entity)
        })
    }

    // =========================================================================
    // Inline Hook Conversion
    // =========================================================================

    /// Import hooks from entity components into rules.
    ///
    /// This scans all entities for hook components (e.g., "Before:take", "On:burn")
    /// and creates corresponding Rule objects. The pattern for each rule matches
    /// the specific entity the hook was defined on.
    ///
    /// This bridges the legacy inline hook syntax with the new rule-based system.
    pub fn import_hooks_from_world(&mut self, world: &World) {
        use super::effect::Effect;
        use super::pattern::Pattern;
        use crate::core::Value;

        for entity in world.all_entities() {
            // Check all components for hook patterns
            for (comp_name, value) in world.components_of(entity) {
                let comp_str = comp_name.0.as_str();

                // Parse hook component names like "Before:take", "On:burn", "After:open"
                let (phase, action) = match parse_hook_component_name_owned(&comp_str) {
                    Some(result) => result,
                    None => continue,
                };

                // Create a rule from this hook
                let trigger = match phase.as_str() {
                    "Before" => Trigger::before(action.as_str()),
                    "On" => Trigger::on(action.as_str()),
                    "After" => Trigger::after(action.as_str()),
                    _ => continue,
                };

                // Pattern matches this specific entity
                let pattern = Pattern::entity_equals("?target", entity);

                // Effect is the hook body (VM script)
                let effect = if let Value::List(_) = value {
                    Effect::script(value.clone())
                } else {
                    continue;
                };

                // Generate a unique rule name
                let rule_name =
                    format!("hook-{}-{}:{}", entity.raw(), phase.to_lowercase(), action);

                self.add_rule(Rule::new(rule_name, pattern, trigger, effect));
            }
        }
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
    ///
    /// Note: Hook triggers (Before/On/After) are fired by the action system,
    /// not during tick evaluation. This method returns false for them.
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
            // Hook triggers are fired by the action system, not tick evaluation
            Trigger::Before(_) | Trigger::On(_) | Trigger::After(_) => false,
            // Derive triggers are fired on-demand when property values are requested
            Trigger::Derive(_) => false,
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

/// Parse a hook component name like "Before:take" into (phase, action).
///
/// Returns `Some(("Before", "take"))` for "Before:take", or `None` if not a hook.
/// Returns owned Strings to avoid lifetime issues with Symbol::as_str().
fn parse_hook_component_name_owned(name: &str) -> Option<(String, String)> {
    // Check for standard hook prefixes
    for prefix in ["Before:", "On:", "After:"] {
        if let Some(action) = name.strip_prefix(prefix) {
            let phase = &prefix[..prefix.len() - 1]; // Remove trailing ":"
            return Some((phase.to_string(), action.to_string()));
        }
    }
    None
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

    // =========================================================================
    // Hook Rule Query Tests
    // =========================================================================

    fn setup_hook_rules() -> RuleSet {
        RuleSet::from_rules(vec![
            // Before:take hook on holy-book
            Rule::new(
                "holy-book-before-take",
                Pattern::entity("?e"),
                Trigger::before("take"),
                Effect::emit_message("A voice booms: Do not touch!"),
            ),
            // On:burn hook on holy-book
            Rule::new(
                "holy-book-on-burn",
                Pattern::entity("?e"),
                Trigger::on("burn"),
                Effect::emit_message("Lightning strikes!"),
            ),
            // After:open hook on chest
            Rule::new(
                "chest-after-open",
                Pattern::entity("?e"),
                Trigger::after("open"),
                Effect::emit_message("The chest creaks."),
            ),
            // Another On:burn hook
            Rule::new(
                "paper-on-burn",
                Pattern::entity("?e"),
                Trigger::on("burn"),
                Effect::emit_message("The paper burns away."),
            ),
            // Periodic rule (not a hook)
            goat_baa_rule(),
        ])
    }

    #[test]
    fn test_before_hooks_query() {
        let rules = setup_hook_rules();

        let before_take: Vec<_> = rules.before_hooks("take").collect();
        assert_eq!(before_take.len(), 1);
        assert_eq!(before_take[0].name.as_str(), "holy-book-before-take");

        let before_burn: Vec<_> = rules.before_hooks("burn").collect();
        assert_eq!(before_burn.len(), 0);
    }

    #[test]
    fn test_on_hooks_query() {
        let rules = setup_hook_rules();

        let on_burn: Vec<_> = rules.on_hooks("burn").collect();
        assert_eq!(on_burn.len(), 2);

        let on_take: Vec<_> = rules.on_hooks("take").collect();
        assert_eq!(on_take.len(), 0);
    }

    #[test]
    fn test_after_hooks_query() {
        let rules = setup_hook_rules();

        let after_open: Vec<_> = rules.after_hooks("open").collect();
        assert_eq!(after_open.len(), 1);
        assert_eq!(after_open[0].name.as_str(), "chest-after-open");
    }

    #[test]
    fn test_hooks_for_action() {
        let rules = setup_hook_rules();

        let burn_hooks: Vec<_> = rules.hooks_for_action("burn").collect();
        assert_eq!(burn_hooks.len(), 2);

        let take_hooks: Vec<_> = rules.hooks_for_action("take").collect();
        assert_eq!(take_hooks.len(), 1);

        let open_hooks: Vec<_> = rules.hooks_for_action("open").collect();
        assert_eq!(open_hooks.len(), 1);
    }

    #[test]
    fn test_matching_hooks() {
        let mut world = World::new();
        let holy_book = world.create_entity();
        world.set_component(holy_book, "Name", "holy book");
        world.set_component(holy_book, "HolyBook", true);

        let paper = world.create_entity();
        world.set_component(paper, "Name", "paper");

        // Create rules that match specific entities
        let rules = RuleSet::from_rules(vec![
            Rule::new(
                "holy-book-on-burn",
                Pattern::has_component("?e", "HolyBook"),
                Trigger::on("burn"),
                Effect::emit_message("Lightning strikes!"),
            ),
            Rule::new(
                "paper-on-burn",
                Pattern::entity("?e"), // Matches any entity
                Trigger::on("burn"),
                Effect::emit_message("It burns."),
            ),
        ]);

        // Holy book matches both rules (one specific, one general)
        let holy_book_hooks: Vec<_> = rules
            .matching_hooks(&world, holy_book, "On", "burn")
            .collect();
        assert_eq!(holy_book_hooks.len(), 2);

        // Paper only matches the general rule
        let paper_hooks: Vec<_> = rules.matching_hooks(&world, paper, "On", "burn").collect();
        assert_eq!(paper_hooks.len(), 1);
        assert_eq!(paper_hooks[0].name.as_str(), "paper-on-burn");
    }

    #[test]
    fn test_hook_rules_dont_fire_on_tick() {
        let rules = setup_hook_rules();
        let mut rules = rules;
        let world = World::new();
        let mut io = TestIO::new(vec![]);

        // Evaluate for many ticks - hook rules should never fire
        for tick in 1..=100 {
            rules.evaluate(&world, &mut io, tick);
        }

        // Only periodic rules should have fired (goat-baas at tick 10, 20, ...)
        // Hook rules should NOT fire
        assert!(!io.output.contains("Lightning"));
        assert!(!io.output.contains("voice booms"));
        assert!(!io.output.contains("creaks"));
    }

    // =========================================================================
    // Import Hooks Tests
    // =========================================================================

    #[test]
    fn test_parse_hook_component_name() {
        assert_eq!(
            parse_hook_component_name_owned("Before:take"),
            Some(("Before".to_string(), "take".to_string()))
        );
        assert_eq!(
            parse_hook_component_name_owned("On:burn"),
            Some(("On".to_string(), "burn".to_string()))
        );
        assert_eq!(
            parse_hook_component_name_owned("After:open"),
            Some(("After".to_string(), "open".to_string()))
        );
        assert_eq!(parse_hook_component_name_owned("Name"), None);
        assert_eq!(parse_hook_component_name_owned("Portable"), None);
    }

    #[test]
    fn test_import_hooks_from_world() {
        use crate::core::Value;
        use crate::symbol::Symbol;

        let mut world = World::new();

        // Create an entity with hooks
        let holy_book = world.create_entity();
        world.set_component(holy_book, "Name", "holy book");

        // Add Before:take hook
        world.set_component(
            holy_book,
            "Before:take",
            Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("A voice booms: Do not touch!"),
            ]),
        );

        // Add On:burn hook
        world.set_component(
            holy_book,
            "On:burn",
            Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("Lightning strikes!"),
            ]),
        );

        // Import hooks into rules
        let mut rules = RuleSet::new();
        rules.import_hooks_from_world(&world);

        // Should have 2 rules imported
        assert_eq!(rules.len(), 2);

        // Check Before hooks for "take"
        let before_take: Vec<_> = rules.before_hooks("take").collect();
        assert_eq!(before_take.len(), 1);
        assert!(before_take[0].name.as_str().contains("before:take"));

        // Check On hooks for "burn"
        let on_burn: Vec<_> = rules.on_hooks("burn").collect();
        assert_eq!(on_burn.len(), 1);
        assert!(on_burn[0].name.as_str().contains("on:burn"));

        // The rule's pattern should match only the holy_book entity
        let other_entity = world.create_entity();
        assert!(before_take[0].pattern.matches(&world, holy_book));
        assert!(!before_take[0].pattern.matches(&world, other_entity));
    }

    // =========================================================================
    // Derivation Rule Query Tests
    // =========================================================================

    #[test]
    fn test_derive_rules_query() {
        let rules = RuleSet::from_rules(vec![
            Rule::new(
                "fire-resist-base",
                Pattern::has_component("?e", "Ancestry"),
                Trigger::derive("FireResistance"),
                Effect::no_op(),
            ),
            Rule::new(
                "fire-resist-biome",
                Pattern::has_component("?e", "InRoom"),
                Trigger::derive("FireResistance"),
                Effect::no_op(),
            ),
            Rule::new(
                "cold-resist-base",
                Pattern::entity("?e"),
                Trigger::derive("ColdResistance"),
                Effect::no_op(),
            ),
            // Non-derive rule should not be returned
            Rule::new(
                "goat-baas",
                Pattern::entity("?e"),
                Trigger::every(10),
                Effect::emit_message("Baa!"),
            ),
        ]);

        let fire_rules: Vec<_> = rules.derive_rules("FireResistance").collect();
        assert_eq!(fire_rules.len(), 2);

        let cold_rules: Vec<_> = rules.derive_rules("ColdResistance").collect();
        assert_eq!(cold_rules.len(), 1);

        let poison_rules: Vec<_> = rules.derive_rules("PoisonResistance").collect();
        assert_eq!(poison_rules.len(), 0);
    }

    #[test]
    fn test_matching_derive_rules() {
        let mut world = World::new();

        // Create a creature with Ancestry component
        let goblin = world.create_entity();
        world.set_component(goblin, "Ancestry", "goblin");
        world.set_component(goblin, "InRoom", true);

        // Create a creature without Ancestry
        let slime = world.create_entity();
        world.set_component(slime, "InRoom", true);

        let rules = RuleSet::from_rules(vec![
            Rule::new(
                "fire-resist-ancestry",
                Pattern::has_component("?e", "Ancestry"),
                Trigger::derive("FireResistance"),
                Effect::no_op(),
            ),
            Rule::new(
                "fire-resist-biome",
                Pattern::has_component("?e", "InRoom"),
                Trigger::derive("FireResistance"),
                Effect::no_op(),
            ),
        ]);

        // Goblin matches both rules
        let goblin_rules: Vec<_> = rules
            .matching_derive_rules(&world, goblin, "FireResistance")
            .collect();
        assert_eq!(goblin_rules.len(), 2);

        // Slime only matches biome rule
        let slime_rules: Vec<_> = rules
            .matching_derive_rules(&world, slime, "FireResistance")
            .collect();
        assert_eq!(slime_rules.len(), 1);
        assert_eq!(slime_rules[0].name.as_str(), "fire-resist-biome");
    }

    #[test]
    fn test_derive_rules_dont_fire_on_tick() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Ancestry", "goblin");

        let mut rules = RuleSet::from_rules(vec![Rule::new(
            "fire-resist",
            Pattern::entity("?e"),
            Trigger::derive("FireResistance"),
            Effect::emit_message("Deriving fire resistance..."),
        )]);

        let mut io = TestIO::new(vec![]);

        // Evaluate for many ticks - derive rules should never fire
        for tick in 1..=100 {
            rules.evaluate(&world, &mut io, tick);
        }

        // Derive rules should NOT have fired
        assert!(!io.output.contains("Deriving"));
    }
}
