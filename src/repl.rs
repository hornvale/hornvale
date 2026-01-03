//! Enhanced REPL for Phase 4.
//!
//! Commands:
//! - tick [N]       : Advance simulation by 1 or N ticks
//! - inspect <id>   : Show all components for an entity
//! - list           : List all entities with their names
//! - rules          : List all rules
//! - rule <name>    : Show details of a specific rule
//! - relations      : List all relation types
//! - load <file>    : Load definitions from file
//! - eval <expr>    : Evaluate an expression
//! - define <def>   : Add a definition inline
//! - parse <expr>   : Show parsed AST
//! - help           : Show available commands
//! - quit / exit    : Exit the REPL
//!
//! DSL-defined commands can be added via:
//! ```lisp
//! (repl-command "name" :doc "description" :expands-to (expression))
//! ```

pub mod command;

use crate::compiler::Compiler;
use crate::core::{EntityId, World};
use crate::input::{self, Resolver};
use crate::io::WorldIO;
use crate::lang::{self, WorldLoader, is_complete};
use crate::rules::RuleSet;
use crate::systems::tick_world_n;
use crate::verbs;
use crate::vm::{StdLib, VM};
use std::path::Path;

pub use command::{ReplCommand, ReplCommandRegistry};

/// Result of executing a REPL command.
pub enum ReplResult {
    /// Continue the REPL loop
    Continue,
    /// Exit the REPL
    Exit,
}

/// Run the REPL loop.
pub fn run_repl(world: &mut World, rules: &mut RuleSet, io: &mut dyn WorldIO) {
    io.println("Hornvale Phase 4 - Language");
    io.println("Type 'help' for available commands.\n");

    let mut loader = WorldLoader::new();
    let mut buffer = String::new();

    loop {
        let prompt = if buffer.is_empty() { "> " } else { "... " };
        let input = match io.prompt(prompt) {
            Some(line) => line,
            None => {
                io.println("\nGoodbye!");
                break;
            }
        };

        // Accumulate input for multi-line expressions
        if !buffer.is_empty() {
            buffer.push('\n');
        }
        buffer.push_str(&input);

        // Check if expression is complete (balanced parens)
        if !is_complete(&buffer) {
            continue;
        }

        let input = buffer.trim();
        if input.is_empty() {
            buffer.clear();
            continue;
        }

        match execute_command(world, rules, &mut loader, io, input) {
            ReplResult::Continue => {}
            ReplResult::Exit => {
                io.println("Goodbye!");
                break;
            }
        }

        buffer.clear();
    }
}

/// Parse and execute a single command.
pub fn execute_command(
    world: &mut World,
    rules: &mut RuleSet,
    loader: &mut WorldLoader,
    io: &mut dyn WorldIO,
    input: &str,
) -> ReplResult {
    let parts: Vec<&str> = input.split_whitespace().collect();
    let command = parts.first().copied().unwrap_or("");
    let rest = input.strip_prefix(command).unwrap_or("").trim();
    let args = &parts[1..];

    // Check for DSL-defined commands first
    if let Some(dsl_cmd) = loader.repl_commands().get(command) {
        match dsl_cmd.expand(args) {
            Ok(expanded) => {
                cmd_eval(world, io, &expanded);
            }
            Err(e) => {
                io.println(&format!("Error: {e}"));
            }
        }
        return ReplResult::Continue;
    }

    match command {
        "tick" | "t" => cmd_tick(world, rules, io, args),
        "inspect" => cmd_inspect(world, io, args),
        "list" | "ls" => cmd_list(world, io),
        "rules" => cmd_rules(rules, io),
        "rule" | "r" => cmd_rule(rules, io, args),
        "relations" | "rels" => cmd_relations(world, io),
        "load" => cmd_load(world, rules, loader, io, args),
        "eval" => cmd_eval(world, io, rest),
        "define" | "def" => cmd_define(world, rules, loader, io, rest),
        "parse" | "p" => cmd_parse(io, rest),
        "help" | "h" | "?" => cmd_help(world, loader, io),
        "quit" | "exit" | "q" => return ReplResult::Exit,
        "" => {}

        // All other commands (including game commands) go through grammar/syntax matching
        _ => {
            // Create stdlib for hook execution
            let stdlib = StdLib::with_builtins();

            // Try grammar-based command matching first (new system)
            if let Some(player) = find_player(world) {
                let tokens: Vec<&str> = input.split_whitespace().collect();
                if let Some(grammar_match) = loader
                    .command_registry()
                    .match_input(world, player, &tokens)
                {
                    let result =
                        verbs::execute_grammar_action(world, player, &grammar_match, &stdlib);
                    io.println(result.output.as_ref());
                    return ReplResult::Continue;
                }
            }

            // Try syntax table matching (deprecated, for backward compatibility)
            if let Some(player) = find_player(world) {
                let tokens: Vec<&str> = input.split_whitespace().collect();
                if let Some(action_match) = loader.syntax_table().best_match(&tokens) {
                    let result = verbs::execute_action(world, player, &action_match);
                    io.println(result.output.as_ref());
                    return ReplResult::Continue;
                }
            }

            // Fall back to traditional game command parsing
            if let Some(player) = find_player(world) {
                let input_event = input::Input::new(input, world.tick()).with_source(player);
                if let Some(cmd) = input::parse_input(&input_event) {
                    let resolver = Resolver::new();
                    let resolved = resolver.resolve_command(world, player, &cmd);
                    let result = verbs::execute_command(world, player, &resolved);
                    io.println(result.output.as_ref());
                    return ReplResult::Continue;
                }
            }

            io.println(&format!(
                "Unknown command: '{command}'. Type 'help' for commands."
            ));
        }
    }

    ReplResult::Continue
}

/// Find the player entity in the world.
fn find_player(world: &World) -> Option<EntityId> {
    for entity in world.all_entities() {
        if let Some(val) = world.get_component(entity, "IsPlayer") {
            if val.as_bool() == Some(true) {
                return Some(entity);
            }
        }
    }
    None
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

/// Load definitions from a file.
fn cmd_load(
    world: &mut World,
    rules: &mut RuleSet,
    loader: &mut WorldLoader,
    io: &mut dyn WorldIO,
    args: &[&str],
) {
    let path = match args.first() {
        Some(s) => *s,
        None => {
            io.println("Usage: load <file>");
            return;
        }
    };

    match loader.load_file(world, rules, Path::new(path)) {
        Ok(()) => io.println(&format!("Loaded: {path}")),
        Err(e) => io.println(&format!("Error loading file: {e}")),
    }
}

/// Evaluate an expression.
fn cmd_eval(world: &World, io: &mut dyn WorldIO, expr: &str) {
    if expr.is_empty() {
        io.println("Usage: eval <expression>");
        return;
    }

    // Parse the expression
    let parsed = match lang::parse(expr) {
        Ok(p) => p,
        Err(e) => {
            io.println(&format!("Parse error: {e}"));
            return;
        }
    };

    // Compile to bytecode
    let chunk = match Compiler::compile(&parsed) {
        Ok(c) => c,
        Err(e) => {
            io.println(&format!("Compile error: {e}"));
            return;
        }
    };

    // Execute
    let stdlib = StdLib::with_builtins();
    let mut vm = VM::new(&chunk, world, &stdlib);

    match vm.run() {
        Ok(value) => io.println(&format!("{value}")),
        Err(e) => io.println(&format!("Runtime error: {e}")),
    }
}

/// Add a definition inline.
fn cmd_define(
    world: &mut World,
    rules: &mut RuleSet,
    loader: &mut WorldLoader,
    io: &mut dyn WorldIO,
    def: &str,
) {
    if def.is_empty() {
        io.println("Usage: define <definition>");
        io.println("  define (entity foo (Name \"Foo\"))");
        io.println("  define (relation Contains :from :one :to :many)");
        io.println("  define (rule my-rule :pattern (entity ?e) :effect (emit-message \"Hi\"))");
        return;
    }

    match loader.load_str(world, rules, def) {
        Ok(()) => io.println("Defined."),
        Err(e) => io.println(&format!("Error: {e}")),
    }
}

/// Show parsed AST for debugging.
fn cmd_parse(io: &mut dyn WorldIO, expr: &str) {
    if expr.is_empty() {
        io.println("Usage: parse <expression>");
        return;
    }

    match lang::parse_all(expr) {
        Ok(exprs) => {
            for (i, parsed) in exprs.iter().enumerate() {
                if exprs.len() > 1 {
                    io.println(&format!("[{i}] {parsed:#?}"));
                } else {
                    io.println(&format!("{parsed:#?}"));
                }
            }
        }
        Err(e) => io.println(&format!("Parse error: {e}")),
    }
}

/// Show help information.
fn cmd_help(_world: &World, loader: &WorldLoader, io: &mut dyn WorldIO) {
    io.println("REPL commands:");
    io.println("  tick [N]      - Advance simulation by 1 or N ticks (alias: t)");
    io.println("  inspect <id>  - Show all components for entity");
    io.println("  list          - List all entities (alias: ls)");
    io.println("  rules         - List all rules");
    io.println("  rule <name>   - Show details of a rule (alias: r)");
    io.println("  relations     - List relation types (alias: rels)");
    io.println("  load <file>   - Load definitions from file");
    io.println("  eval <expr>   - Evaluate an expression");
    io.println("  define <def>  - Add a definition inline (alias: def)");
    io.println("  parse <expr>  - Show parsed AST (alias: p)");
    io.println("  help          - Show this help (alias: h, ?)");
    io.println("  quit          - Exit the REPL (alias: exit, q)");

    // Show grammar-defined game commands
    let cmd_names = loader.command_registry().command_names();
    if !cmd_names.is_empty() {
        io.println("");
        io.println("Game commands (from grammar):");
        for name in cmd_names {
            io.println(&format!("  {}", name.as_str()));
        }
    }

    // Show DSL-defined REPL commands
    let dsl_cmds: Vec<_> = loader.repl_commands().commands().collect();
    if !dsl_cmds.is_empty() {
        io.println("");
        io.println("DSL-defined commands:");
        for cmd in dsl_cmds {
            io.println(&format!("  {}", cmd.help()));
        }
    }

    io.println("");
    io.println("Multi-line input: Incomplete expressions continue on next line.");
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
        let world = setup_test_world();
        let loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);
        cmd_help(&world, &loader, &mut io);

        assert!(io.output.contains("tick"));
        assert!(io.output.contains("inspect"));
        assert!(io.output.contains("list"));
        assert!(io.output.contains("quit"));
        assert!(io.output.contains("load"));
        assert!(io.output.contains("eval"));
        assert!(io.output.contains("define"));
        assert!(io.output.contains("parse"));
    }

    #[test]
    fn test_unknown_command() {
        let mut world = setup_test_world();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        execute_command(&mut world, &mut rules, &mut loader, &mut io, "foobar");

        assert!(io.output.contains("Unknown command"));
    }

    #[test]
    fn test_quit_command() {
        let mut world = setup_test_world();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        let result = execute_command(&mut world, &mut rules, &mut loader, &mut io, "quit");
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

    #[test]
    fn test_cmd_eval() {
        let world = setup_test_world();
        let mut io = TestIO::new(vec![]);

        cmd_eval(&world, &mut io, "(+ 1 2)");

        assert!(io.output.contains("3"));
    }

    #[test]
    fn test_cmd_eval_parse_error() {
        let world = setup_test_world();
        let mut io = TestIO::new(vec![]);

        cmd_eval(&world, &mut io, "(+ 1");

        assert!(io.output.contains("Parse error"));
    }

    #[test]
    fn test_cmd_define_entity() {
        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        cmd_define(
            &mut world,
            &mut rules,
            &mut loader,
            &mut io,
            "(entity foo (Name \"Foo\"))",
        );

        assert!(io.output.contains("Defined"));
        assert_eq!(world.entity_count(), 1);

        // Verify the entity was created
        let foo = loader.get_entity("foo").unwrap();
        assert_eq!(
            world.get_component(foo, "Name"),
            Some(&crate::Value::string("Foo"))
        );
    }

    #[test]
    fn test_cmd_parse() {
        let mut io = TestIO::new(vec![]);

        cmd_parse(&mut io, "(+ 1 2)");

        assert!(io.output.contains("List"));
    }

    #[test]
    fn test_is_complete() {
        assert!(is_complete("(+ 1 2)"));
        assert!(is_complete("foo"));
        assert!(!is_complete("(+ 1"));
        assert!(!is_complete("(+ (- 1 2)"));
        assert!(is_complete("(+ (- 1 2))"));
        // Strings don't affect balance
        assert!(is_complete("\"hello\""));
        assert!(!is_complete("\"hello"));
        // Escaped quote inside string
        assert!(is_complete("\"he\\\"llo\""));
    }

    #[test]
    fn test_dsl_command() {
        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Define a DSL command (expansion as string literal)
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"(repl-command "add" :args (a b) :expands-to "(+ $a $b)")"#,
            )
            .unwrap();

        // Execute the DSL command
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "add 3 4");

        assert!(io.output.contains("7"));
    }

    #[test]
    fn test_dsl_command_with_alias() {
        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Define a DSL command with alias (expansion as string literal)
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"(repl-command "double"
                     :aliases ("d")
                     :args (x)
                     :expands-to "(* $x 2)")"#,
            )
            .unwrap();

        // Execute via alias
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "d 5");

        assert!(io.output.contains("10"));
    }

    #[test]
    fn test_dsl_command_in_help() {
        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Define a DSL command
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"(repl-command "greet" :doc "Say hello" :expands-to "Hello!")"#,
            )
            .unwrap();

        cmd_help(&world, &loader, &mut io);

        assert!(io.output.contains("DSL-defined commands"));
        assert!(io.output.contains("greet"));
        assert!(io.output.contains("Say hello"));
    }

    #[test]
    fn test_game_look_command() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Register relations
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create room
        let room = world.create_entity();
        world.set_component(room, "Name", "Test Room");
        world.set_component(room, "IsRoom", true);
        world.set_component(room, "RoomDescription", "A test room for verbs.");

        // Create player
        let player = world.create_entity();
        world.set_component(player, "Name", "player");
        world.set_component(player, "IsPlayer", true);
        world.add_relation("InRoom", player, room);

        // Execute look command
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "look");

        assert!(io.output.contains("Test Room"));
        assert!(io.output.contains("test room for verbs"));
    }

    #[test]
    fn test_game_take_command() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Register relations
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create room
        let room = world.create_entity();
        world.set_component(room, "IsRoom", true);

        // Create player
        let player = world.create_entity();
        world.set_component(player, "IsPlayer", true);
        world.add_relation("InRoom", player, room);

        // Create object
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", "lamp");
        world.set_component(lamp, "Portable", true);
        world.add_relation("InRoom", lamp, room);

        // Take the lamp
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "take lamp");

        assert!(io.output.contains("Taken"));
    }

    #[test]
    fn test_syntax_table_integration() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Register relations
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create room
        let room = world.create_entity();
        world.set_component(room, "Name", "Test Room");
        world.set_component(room, "IsRoom", true);
        world.set_component(room, "RoomDescription", "A test room with stuff.");

        // Create player
        let player = world.create_entity();
        world.set_component(player, "Name", "player");
        world.set_component(player, "IsPlayer", true);
        world.add_relation("InRoom", player, room);

        // Create object
        let gem = world.create_entity();
        world.set_component(gem, "Name", "gem");
        world.set_component(gem, "Portable", true);
        world.add_relation("InRoom", gem, room);

        // Load syntax definitions via DSL
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                ; Define some custom syntax patterns
                (syntax "l" :to look)
                (syntax "peep" :to look)
                (syntax "grab" noun :to take)
                "#,
            )
            .unwrap();

        // Test custom "peep" command works via syntax table
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "peep");
        assert!(
            io.output.contains("Test Room"),
            "Custom 'peep' should trigger look"
        );

        // Test "grab" as synonym for take
        io.output.clear();
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "grab gem");
        assert!(
            io.output.contains("Taken"),
            "Custom 'grab gem' should take the gem"
        );
    }

    #[test]
    fn test_grammar_command_integration() {
        use crate::core::{Cardinality, RelationSchema};

        let mut world = World::new();
        let mut rules = empty_rules();
        let mut loader = WorldLoader::new();
        let mut io = TestIO::new(vec![]);

        // Register relations
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create room
        let room = world.create_entity();
        world.set_component(room, "Name", "Test Room");
        world.set_component(room, "IsRoom", true);
        world.set_component(room, "RoomDescription", "A test room.");

        // Create player
        let player = world.create_entity();
        world.set_component(player, "Name", "player");
        world.set_component(player, "IsPlayer", true);
        world.add_relation("InRoom", player, room);

        // Create object
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", "lamp");
        world.set_component(lamp, "Portable", true);
        world.add_relation("InRoom", lamp, room);

        // Load grammar command definitions via DSL
        // Note: Use aliases that aren't hardcoded in the REPL (peep, survey, snag)
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command survey
                  :aliases ("peep")
                  :forms
                    ((() -> look-around)
                     (("at" obj:noun) -> (examine obj))))

                (command snag
                  :forms
                    (((obj:noun) -> (take obj))))
                "#,
            )
            .unwrap();

        // Test "peep" alias works via grammar system
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "peep");
        assert!(
            io.output.contains("Test Room"),
            "Grammar 'peep' alias should trigger look"
        );

        // Test "snag lamp" via grammar system
        io.output.clear();
        execute_command(&mut world, &mut rules, &mut loader, &mut io, "snag lamp");
        assert!(
            io.output.contains("Taken"),
            "Grammar 'snag lamp' should take the lamp"
        );

        // Test "survey at lamp" via grammar system (examine)
        io.output.clear();
        execute_command(
            &mut world,
            &mut rules,
            &mut loader,
            &mut io,
            "survey at lamp",
        );
        assert!(
            io.output.contains("nothing special"),
            "Grammar 'survey at lamp' should examine the lamp, got: {}",
            io.output
        );
    }
}
