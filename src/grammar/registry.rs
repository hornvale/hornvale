//! Command registry for the grammar system.
//!
//! The command registry is the main entry point for matching player input
//! against the grammar. It combines the intent trie and form matching.

use crate::core::{EntityId, World};
use crate::lang::SExpr;
use crate::symbol::Symbol;
use im::OrdMap;

use super::command::{Command, GrammarMatch};
use super::trie::IntentTrie;
use super::types::{TypePredicate, TypeRegistry};

/// Registry of all commands and types.
///
/// The command registry provides:
/// - Type predicate registration and lookup
/// - Command registration with aliases
/// - Input matching via trie dispatch + form matching
#[derive(Debug, Clone, Default)]
pub struct CommandRegistry {
    /// Type predicates.
    types: TypeRegistry,
    /// Commands indexed by name.
    commands: OrdMap<Symbol, Command>,
    /// Intent trie for dispatch.
    trie: IntentTrie,
}

impl CommandRegistry {
    /// Create a new empty command registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the type registry.
    pub fn types(&self) -> &TypeRegistry {
        &self.types
    }

    /// Get the type registry mutably.
    pub fn types_mut(&mut self) -> &mut TypeRegistry {
        &mut self.types
    }

    /// Register a type predicate.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::grammar::CommandRegistry;
    /// use hornvale::lang::{Atom, SExpr, Span};
    /// use hornvale::symbol::Symbol;
    ///
    /// let mut registry = CommandRegistry::new();
    ///
    /// // (type portable (has? entity :Portable))
    /// let predicate = SExpr::List(vec![
    ///     SExpr::Atom(Atom::Symbol(Symbol::new("has?")), Span::default()),
    ///     SExpr::Atom(Atom::Symbol(Symbol::new("entity")), Span::default()),
    ///     SExpr::Atom(Atom::Keyword(Symbol::new("Portable")), Span::default()),
    /// ], Span::default());
    /// registry.register_type("portable", predicate);
    /// ```
    pub fn register_type(&mut self, name: impl Into<Symbol>, predicate: SExpr) {
        let name = name.into();
        self.types.register(TypePredicate::new(name, predicate));
    }

    /// Register a command with aliases.
    ///
    /// # Arguments
    /// * `command` - The command definition
    /// * `aliases` - List of alias token sequences (e.g., `[["l"], ["look"], ["pick", "up"]]`)
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::grammar::{CommandRegistry, Command, Form, FormElement, FormAction};
    /// use hornvale::symbol::Symbol;
    ///
    /// let mut registry = CommandRegistry::new();
    ///
    /// let mut cmd = Command::new("look");
    /// cmd.add_form(Form::new(vec![], FormAction::simple("look-around")));
    /// cmd.add_form(Form::new(
    ///     vec![FormElement::word("at"), FormElement::noun("obj")],
    ///     FormAction::with_slots("examine", vec![Symbol::new("obj")]),
    /// ));
    ///
    /// registry.register_command(cmd, vec![vec!["look".to_string()], vec!["l".to_string()]]);
    /// ```
    pub fn register_command(&mut self, command: Command, aliases: Vec<Vec<String>>) {
        let name = command.name;

        // Add the command name itself as an alias
        let name_str = command.name.as_str();
        self.trie.insert(&[&name_str], command.name);

        // Add explicit aliases
        for alias in &aliases {
            let alias_refs: Vec<&str> = alias.iter().map(|s| s.as_str()).collect();
            self.trie.insert(&alias_refs, name);
        }

        // Store the command
        self.commands.insert(name, command);
    }

    /// Get a command by name.
    pub fn get_command(&self, name: Symbol) -> Option<&Command> {
        self.commands.get(&name)
    }

    /// Check if a command exists.
    pub fn has_command(&self, name: Symbol) -> bool {
        self.commands.contains_key(&name)
    }

    /// Get all registered command names.
    pub fn command_names(&self) -> Vec<Symbol> {
        self.commands.keys().cloned().collect()
    }

    /// Match input tokens against the grammar.
    ///
    /// This is the main entry point for command parsing:
    /// 1. Use the trie to find the command (longest prefix match)
    /// 2. Use the command's forms to match remaining tokens
    ///
    /// # Arguments
    /// * `world` - The world state (for entity resolution)
    /// * `actor` - The actor performing the command
    /// * `tokens` - The input tokens (already split and lowercased)
    ///
    /// # Returns
    /// A `GrammarMatch` if successful, `None` if no match.
    pub fn match_input(
        &self,
        world: &World,
        actor: EntityId,
        tokens: &[&str],
    ) -> Option<GrammarMatch> {
        // Step 1: Find command via trie
        let (command_name, consumed) = self.trie.longest_match(tokens)?;

        // Step 2: Get the command
        let command = self.commands.get(&command_name)?;

        // Step 3: Match remaining tokens against forms
        let remaining = &tokens[consumed..];
        command.match_forms(world, actor, remaining, &self.types)
    }

    /// Match input and return detailed error information.
    ///
    /// Unlike `match_input`, this returns information about why matching failed.
    pub fn match_input_detailed(
        &self,
        world: &World,
        actor: EntityId,
        tokens: &[&str],
    ) -> MatchInputResult {
        // Step 1: Find command via trie
        match self.trie.longest_match(tokens) {
            None => MatchInputResult::NoCommand,
            Some((command_name, consumed)) => {
                // Step 2: Get the command
                match self.commands.get(&command_name) {
                    None => MatchInputResult::NoCommand,
                    Some(command) => {
                        // Step 3: Match remaining tokens against forms
                        let remaining = &tokens[consumed..];
                        match command.match_forms_detailed(world, actor, remaining, &self.types) {
                            Ok(m) => MatchInputResult::Matched(m),
                            Err(failures) => MatchInputResult::FormsFailed {
                                command: command_name,
                                failures,
                            },
                        }
                    }
                }
            }
        }
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }

    /// Number of registered commands.
    pub fn len(&self) -> usize {
        self.commands.len()
    }
}

/// Result of detailed input matching.
#[derive(Debug)]
pub enum MatchInputResult {
    /// Successfully matched.
    Matched(GrammarMatch),
    /// No command matched the input prefix.
    NoCommand,
    /// Command found but no forms matched.
    FormsFailed {
        command: Symbol,
        failures: Vec<super::matcher::MatchResult>,
    },
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::command::{Form, FormAction, FormElement};

    fn setup_registry() -> CommandRegistry {
        let mut registry = CommandRegistry::new();

        // Look command
        let mut look = Command::new("look");
        look.add_form(Form::new(vec![], FormAction::simple("look-around")));
        look.add_form(Form::new(
            vec![FormElement::word("at"), FormElement::noun("obj")],
            FormAction::with_slots("examine", vec![Symbol::new("obj")]),
        ));
        look.add_form(Form::new(
            vec![FormElement::direction("dir")],
            FormAction::with_slots("look-direction", vec![Symbol::new("dir")]),
        ));

        registry.register_command(look, vec![vec!["l".to_string()]]);

        // Take command with multi-word alias
        let mut take = Command::new("take");
        take.add_form(Form::new(
            vec![FormElement::noun("obj")],
            FormAction::with_slots("take", vec![Symbol::new("obj")]),
        ));

        registry.register_command(
            take,
            vec![
                vec!["get".to_string()],
                vec!["grab".to_string()],
                vec!["pick".to_string(), "up".to_string()],
            ],
        );

        // Go command
        let mut go = Command::new("go");
        go.add_form(Form::new(
            vec![FormElement::direction("dir")],
            FormAction::with_slots("go", vec![Symbol::new("dir")]),
        ));

        registry.register_command(go, vec![]);

        registry
    }

    fn setup_world() -> (World, EntityId, EntityId) {
        use crate::core::{Cardinality, ComponentTypeId, RelationSchema, RelationTypeId, Value};

        let mut world = World::new();

        // Register relation schemas (required for relation queries to work)
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

        let room = world.create_entity();
        world.set_component(room, ComponentTypeId::new("IsRoom"), Value::Bool(true));

        let actor = world.create_entity();
        world.add_relation(RelationTypeId::new("InRoom"), actor, room);

        let lamp = world.create_entity();
        world.set_component(
            lamp,
            ComponentTypeId::new("Name"),
            Value::String("lamp".into()),
        );
        world.add_relation(RelationTypeId::new("InRoom"), lamp, room);

        (world, actor, lamp)
    }

    #[test]
    fn test_registry_creation() {
        let registry = setup_registry();
        assert!(!registry.is_empty());
        assert_eq!(registry.len(), 3); // look, take, go

        assert!(registry.has_command(Symbol::new("look")));
        assert!(registry.has_command(Symbol::new("take")));
        assert!(registry.has_command(Symbol::new("go")));
        assert!(!registry.has_command(Symbol::new("nonexistent")));
    }

    #[test]
    fn test_simple_command_match() {
        let registry = setup_registry();
        let (world, actor, _) = setup_world();

        // "look" matches look command, empty form
        let result = registry.match_input(&world, actor, &["look"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.command, Symbol::new("look"));
        assert_eq!(m.action.action_name(), Symbol::new("look-around"));
    }

    #[test]
    fn test_alias_match() {
        let registry = setup_registry();
        let (world, actor, _) = setup_world();

        // "l" is alias for look
        let result = registry.match_input(&world, actor, &["l"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.command, Symbol::new("look"));
    }

    #[test]
    fn test_multi_word_alias() {
        let registry = setup_registry();
        let (world, actor, lamp) = setup_world();

        // "pick up lamp" uses multi-word alias
        let result = registry.match_input(&world, actor, &["pick", "up", "lamp"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.command, Symbol::new("take"));
        assert_eq!(m.get_entity("obj"), Some(lamp));
    }

    #[test]
    fn test_direction_slot() {
        let registry = setup_registry();
        let (world, actor, _) = setup_world();

        // "go north"
        let result = registry.match_input(&world, actor, &["go", "north"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.command, Symbol::new("go"));
        assert_eq!(m.get_direction("dir"), Some(Symbol::new("north")));

        // "look north" (look direction form)
        let result = registry.match_input(&world, actor, &["look", "north"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("look-direction"));
    }

    #[test]
    fn test_noun_slot_with_resolution() {
        let registry = setup_registry();
        let (world, actor, lamp) = setup_world();

        // "look at lamp"
        let result = registry.match_input(&world, actor, &["look", "at", "lamp"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("examine"));
        assert_eq!(m.get_entity("obj"), Some(lamp));
    }

    #[test]
    fn test_unknown_command() {
        let registry = setup_registry();
        let (world, actor, _) = setup_world();

        let result = registry.match_input(&world, actor, &["dance"]);
        assert!(result.is_none());
    }

    #[test]
    fn test_type_registration() {
        use crate::lang::Span;

        let mut registry = CommandRegistry::new();

        let span = Span::new(0, 0, 1, 1);
        let predicate = SExpr::Atom(crate::lang::Atom::Bool(true), span);
        registry.register_type("portable", predicate);

        assert!(registry.types().contains(Symbol::new("portable")));
    }

    #[test]
    fn test_command_names() {
        let registry = setup_registry();
        let names = registry.command_names();

        assert!(names.contains(&Symbol::new("look")));
        assert!(names.contains(&Symbol::new("take")));
        assert!(names.contains(&Symbol::new("go")));
    }
}
