//! Simple REPL for Phase 2.
//!
//! Commands:
//! - tick [N]       : Advance simulation by 1 or N ticks
//! - inspect <id>   : Show all components for an entity
//! - list           : List all entities with their names
//! - rules          : List all rules
//! - rule <name>    : Show details of a specific rule
//! - relations      : List all relation types
//! - help           : Show available commands
//! - quit / exit    : Exit the REPL

use crate::core::{EntityId, World};
use crate::io::WorldIO;
use crate::rules::RuleSet;
use crate::systems::tick_world_n;

/// Result of executing a REPL command.
pub enum ReplResult {
    /// Continue the REPL loop
    Continue,
    /// Exit the REPL
    Exit,
}

/// Run the REPL loop.
pub fn run_repl(world: &mut World, rules: &mut RuleSet, io: &mut dyn WorldIO) {
    io.println("Hornvale Phase 2 - \"Goat in a Room\"");
    io.println("Type 'help' for available commands.\n");

    loop {
        let input = match io.prompt("> ") {
            Some(line) => line,
            None => {
                io.println("\nGoodbye!");
                break;
            }
        };

        let input = input.trim();
        if input.is_empty() {
            continue;
        }

        match execute_command(world, rules, io, input) {
            ReplResult::Continue => {}
            ReplResult::Exit => {
                io.println("Goodbye!");
                break;
            }
        }
    }
}

/// Parse and execute a single command.
pub fn execute_command(
    world: &mut World,
    rules: &mut RuleSet,
    io: &mut dyn WorldIO,
    input: &str,
) -> ReplResult {
    let parts: Vec<&str> = input.split_whitespace().collect();
    let command = parts.first().copied().unwrap_or("");
    let args = &parts[1..];

    match command {
        "tick" | "t" => cmd_tick(world, rules, io, args),
        "inspect" | "i" => cmd_inspect(world, io, args),
        "list" | "ls" | "l" => cmd_list(world, io),
        "rules" => cmd_rules(rules, io),
        "rule" | "r" => cmd_rule(rules, io, args),
        "relations" | "rels" => cmd_relations(world, io),
        "help" | "h" | "?" => cmd_help(io),
        "quit" | "exit" | "q" => return ReplResult::Exit,
        "" => {}
        _ => {
            io.println(&format!(
                "Unknown command: '{command}'. Type 'help' for commands."
            ));
        }
    }

    ReplResult::Continue
}

/// Advance simulation by N ticks (default 1).
fn cmd_tick(world: &mut World, rules: &mut RuleSet, io: &mut dyn WorldIO, args: &[&str]) {
    let count: u64 = args.first().and_then(|s| s.parse().ok()).unwrap_or(1);

    if count == 0 {
        io.println("Tick count must be positive.");
        return;
    }

    let start_tick = world.tick();
    tick_world_n(world, rules, io, count);
    let end_tick = world.tick();

    io.println(&format!(
        "Advanced {count} tick(s): {start_tick} -> {end_tick}"
    ));
}

/// Show all components for an entity.
fn cmd_inspect(world: &World, io: &mut dyn WorldIO, args: &[&str]) {
    let id_str = match args.first() {
        Some(s) => *s,
        None => {
            io.println("Usage: inspect <entity-id>");
            return;
        }
    };

    let id: u64 = match id_str.parse() {
        Ok(n) => n,
        Err(_) => {
            io.println(&format!("Invalid entity ID: '{id_str}'"));
            return;
        }
    };

    if id >= world.entity_count() {
        io.println(&format!("Entity {id} does not exist."));
        return;
    }

    let entity = EntityId::from_raw(id);
    let components: Vec<_> = world.components_of(entity).collect();

    if components.is_empty() {
        io.println(&format!("Entity {id} has no components."));
    } else {
        io.println(&format!("Entity {id}:"));
        for (comp_type, value) in components {
            io.println(&format!("  {comp_type}: {value}"));
        }
    }
}

/// List all entities with their names.
fn cmd_list(world: &World, io: &mut dyn WorldIO) {
    let count = world.entity_count();
    if count == 0 {
        io.println("No entities in the world.");
        return;
    }

    io.println(&format!("Entities ({count}):"));
    for entity in world.all_entities() {
        let name = world
            .get_component(entity, "Name")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| "(unnamed)".to_string());

        io.println(&format!("  {entity}: {name}"));
    }
}

/// List all rules.
fn cmd_rules(rules: &RuleSet, io: &mut dyn WorldIO) {
    if rules.is_empty() {
        io.println("No rules defined.");
        return;
    }

    io.println(&format!("Rules ({}):", rules.len()));
    for rule in rules.rules() {
        let interval = rule
            .trigger
            .interval()
            .map(|n| format!(" (every {n} ticks)"))
            .unwrap_or_default();
        io.println(&format!("  {}{}", rule.name.as_str(), interval));
    }
}

/// Show details of a specific rule.
fn cmd_rule(rules: &RuleSet, io: &mut dyn WorldIO, args: &[&str]) {
    let name = match args.first() {
        Some(s) => *s,
        None => {
            io.println("Usage: rule <name>");
            return;
        }
    };

    match rules.get_rule(name) {
        Some(rule) => {
            io.println(&rule.describe());
        }
        None => {
            io.println(&format!("Rule '{name}' not found."));
        }
    }
}

/// List all relation types.
fn cmd_relations(world: &World, io: &mut dyn WorldIO) {
    let types: Vec<_> = world.relation_types().collect();

    if types.is_empty() {
        io.println("No relation types defined.");
        return;
    }

    io.println(&format!("Relation types ({}):", types.len()));
    for rel_type in types {
        if let Some(schema) = world.relation_schema(rel_type) {
            let cardinality = format!(
                "{:?}-to-{:?}",
                schema.from_cardinality, schema.to_cardinality
            );
            let symmetric = if schema.symmetric { " (symmetric)" } else { "" };
            io.println(&format!("  {rel_type}: {cardinality}{symmetric}"));
        }
    }
}

/// Show help information.
fn cmd_help(io: &mut dyn WorldIO) {
    io.println("Available commands:");
    io.println("  tick [N]      - Advance simulation by 1 or N ticks (alias: t)");
    io.println("  inspect <id>  - Show all components for entity (alias: i)");
    io.println("  list          - List all entities (alias: ls, l)");
    io.println("  rules         - List all rules");
    io.println("  rule <name>   - Show details of a rule (alias: r)");
    io.println("  relations     - List relation types (alias: rels)");
    io.println("  help          - Show this help (alias: h, ?)");
    io.println("  quit          - Exit the REPL (alias: exit, q)");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::TestIO;

    fn setup_test_world() -> World {
        let mut world = World::new();

        let room = world.create_entity();
        world.set_component(room, "Name", "A small room");

        let goat = world.create_entity();
        world.set_component(goat, "Name", "goat");

        world
    }

    fn empty_rules() -> RuleSet {
        RuleSet::new()
    }

    #[test]
    fn test_cmd_list() {
        let world = setup_test_world();
        let mut io = TestIO::new(vec![]);

        cmd_list(&world, &mut io);

        assert!(io.output.contains("A small room"));
        assert!(io.output.contains("goat"));
    }

    #[test]
    fn test_cmd_inspect() {
        let world = setup_test_world();
        let mut io = TestIO::new(vec![]);

        cmd_inspect(&world, &mut io, &["1"]);

        assert!(io.output.contains("Entity 1:"));
        assert!(io.output.contains("Name"));
        assert!(io.output.contains("goat"));
    }

    #[test]
    fn test_cmd_tick() {
        let mut world = setup_test_world();
        let mut rules = empty_rules();
        let mut io = TestIO::new(vec![]);

        cmd_tick(&mut world, &mut rules, &mut io, &["5"]);

        assert_eq!(world.tick(), 5);
        assert!(io.output.contains("Advanced 5 tick(s)"));
    }

    #[test]
    fn test_cmd_help() {
        let mut io = TestIO::new(vec![]);
        cmd_help(&mut io);

        assert!(io.output.contains("tick"));
        assert!(io.output.contains("inspect"));
        assert!(io.output.contains("list"));
        assert!(io.output.contains("quit"));
    }

    #[test]
    fn test_unknown_command() {
        let mut world = setup_test_world();
        let mut rules = empty_rules();
        let mut io = TestIO::new(vec![]);

        execute_command(&mut world, &mut rules, &mut io, "foobar");

        assert!(io.output.contains("Unknown command"));
    }

    #[test]
    fn test_quit_command() {
        let mut world = setup_test_world();
        let mut rules = empty_rules();
        let mut io = TestIO::new(vec![]);

        let result = execute_command(&mut world, &mut rules, &mut io, "quit");
        assert!(matches!(result, ReplResult::Exit));
    }

    #[test]
    fn test_cmd_rules() {
        use crate::rules::{Effect, Pattern, Rule, Trigger};

        let rules = RuleSet::from_rules(vec![Rule::new(
            "test-rule",
            Pattern::entity("?e"),
            Trigger::every(5),
            Effect::emit_message("Hello"),
        )]);
        let mut io = TestIO::new(vec![]);

        cmd_rules(&rules, &mut io);

        assert!(io.output.contains("Rules (1)"));
        assert!(io.output.contains("test-rule"));
        assert!(io.output.contains("every 5 ticks"));
    }

    #[test]
    fn test_cmd_rule() {
        use crate::rules::{Effect, Pattern, Rule, Trigger};

        let rules = RuleSet::from_rules(vec![Rule::new(
            "goat-baas",
            Pattern::component_value("?e", "Name", "goat"),
            Trigger::every(10),
            Effect::emit_message("Baa!"),
        )]);
        let mut io = TestIO::new(vec![]);

        cmd_rule(&rules, &mut io, &["goat-baas"]);

        assert!(io.output.contains("goat-baas"));
        assert!(io.output.contains("Pattern"));
        assert!(io.output.contains("goat"));
    }

    #[test]
    fn test_cmd_relations() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        world.register_relation(RelationSchema::new(
            "Location",
            Cardinality::Many,
            Cardinality::One,
        ));
        let mut io = TestIO::new(vec![]);

        cmd_relations(&world, &mut io);

        assert!(io.output.contains("Relation types (1)"));
        assert!(io.output.contains("Location"));
        assert!(io.output.contains("Many-to-One"));
    }
}
