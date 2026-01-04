//! Interactive REPL for Hornvale.
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
use crate::grammar::MatchResult;
use crate::io::WorldIO;
use crate::lang::{self, WorldLoader, is_complete};
use crate::rules::RuleSet;
use crate::symbol::Symbol;
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
    io.println("Welcome to Hornvale.");
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

        // All other commands (including game commands) go through grammar matching
        _ => {
            // Create stdlib for hook execution
            let stdlib = StdLib::with_builtins();

            // Try grammar-based command matching with fallback support
            if let Some(player) = find_player(world) {
                let tokens: Vec<&str> = input.split_whitespace().collect();
                match loader
                    .command_registry()
                    .match_input_with_fallbacks(world, player, &tokens)
                {
                    MatchResult::Action(grammar_match) => {
                        // Use execute_grammar_action_full to enable DSL handlers
                        let result = verbs::execute_grammar_action_full(
                            world,
                            player,
                            &grammar_match,
                            &stdlib,
                            Some(loader.action_registry()),
                            Some(loader.precondition_registry()),
                            Some(loader.function_registry()),
                        );
                        io.println(result.output.as_ref());
                        return ReplResult::Continue;
                    }
                    MatchResult::Fallback(fallback_match) => {
                        // Execute the fallback response expression
                        let output = execute_fallback_response(
                            world,
                            player,
                            &fallback_match,
                            &stdlib,
                            loader.function_registry(),
                        );
                        io.println(&output);
                        return ReplResult::Continue;
                    }
                    MatchResult::GlobalFallback(response) => {
                        // Execute the global fallback response
                        let output = execute_response_expr(
                            world,
                            &response,
                            &stdlib,
                            loader.function_registry(),
                        );
                        io.println(&output);
                        return ReplResult::Continue;
                    }
                    MatchResult::NoMatch => {
                        // Fall through to default error message
                    }
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

/// Execute a fallback response expression.
///
/// This compiles and runs the response expression from a FallbackMatch.
/// Slot values are bound by wrapping the expression in a let block.
fn execute_fallback_response(
    world: &World,
    _actor: EntityId,
    fallback_match: &crate::grammar::FallbackMatch,
    stdlib: &StdLib,
    functions: &crate::lang::FunctionRegistry,
) -> String {
    use crate::lang::{Atom, SExpr, Span};

    // Build a let expression that binds the slot values
    // (let ((name1 value1) (name2 value2) ...) response)
    let span = Span::default();
    let mut let_bindings = Vec::new();

    for (name, slot_value) in &fallback_match.slots {
        let value_expr = match slot_value {
            crate::grammar::SlotValue::Entity(eid) => {
                // (entity <id>)
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), span),
                        SExpr::Atom(Atom::Int(eid.raw() as i64), span),
                    ],
                    span,
                )
            }
            crate::grammar::SlotValue::Direction(dir) => {
                // :direction-symbol
                SExpr::Atom(Atom::Keyword(*dir), span)
            }
            crate::grammar::SlotValue::Text(text) => {
                // "text"
                SExpr::Atom(Atom::String(text.to_string()), span)
            }
        };

        let_bindings.push(SExpr::List(
            vec![SExpr::Atom(Atom::Symbol(*name), span), value_expr],
            span,
        ));
    }

    // Wrap response in let if we have bindings
    let expr_to_compile = if let_bindings.is_empty() {
        fallback_match.response.clone()
    } else {
        SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("let")), span),
                SExpr::List(let_bindings, span),
                fallback_match.response.clone(),
            ],
            span,
        )
    };

    execute_response_expr(world, &expr_to_compile, stdlib, functions)
}

/// Execute a response expression (from a fallback or global fallback).
///
/// Compiles and runs the expression, capturing any output from `say`.
fn execute_response_expr(
    world: &World,
    response: &crate::lang::SExpr,
    stdlib: &StdLib,
    functions: &crate::lang::FunctionRegistry,
) -> String {
    // Compile the response expression with user functions
    let chunk = match Compiler::compile_with_functions(response, functions) {
        Ok(c) => c,
        Err(e) => return format!("[Fallback compile error: {e}]"),
    };

    // Execute in VM
    let mut vm = VM::new(&chunk, world, stdlib);

    match vm.run() {
        Ok(_) => {
            // Return any output from say
            let output = vm.take_output();
            if output.is_empty() {
                String::new()
            } else {
                output.join("")
            }
        }
        Err(e) => format!("[Fallback runtime error: {e}]"),
    }
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

        // Load grammar command and action with DSL handler for "look"
        // Note: We use simple say statements since the test creates a minimal environment
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command look :aliases ("l") :forms ((() -> look-around)))
                (action look-around
                  :handler
                    (do
                      (say "Test Room")
                      (say "A test room for verbs.")
                      :success))
                "#,
            )
            .unwrap();

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

        assert!(
            io.output.contains("Test Room"),
            "Expected 'Test Room' in output, got: {}",
            io.output
        );
        assert!(
            io.output.contains("test room for verbs"),
            "Expected 'test room for verbs' in output, got: {}",
            io.output
        );
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

        // Load grammar command and action with DSL handler for "take"
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command take :aliases ("get") :forms (((obj:noun) -> (take obj))))
                (action take
                  :handler
                    (do
                      (relate! :Contains (actor) (direct-object))
                      (say "Taken.")
                      :success))
                "#,
            )
            .unwrap();

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

        assert!(
            io.output.contains("Taken"),
            "Expected 'Taken' in output, got: {}",
            io.output
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

        // Load grammar command definitions and actions via DSL
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

                ; Action handlers - simplified for test environment
                (action look-around
                  :handler
                    (do
                      (say "Test Room")
                      (say "A test room.")
                      :success))

                (action examine
                  :handler
                    (do
                      (say "You see nothing special about it.")
                      :success))

                (action take
                  :handler
                    (do
                      (relate! :Contains (actor) (direct-object))
                      (say "Taken.")
                      :success))
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
