//! World loader - loads DSL definitions into the World.

use crate::Value;
use crate::core::{Cardinality, EntityId, RelationSchema, World};
use crate::generator::{GenerationStub, GeneratorRegistry, StubStorage};
use crate::grammar::{Command as GrammarCommand, CommandRegistry, Form, FormAction, FormElement};
use crate::lang::{Atom, ParseError, SExpr, parse_all};
use crate::repl::command::{ReplCommandRegistry, parse_repl_command};
use crate::rules::{Effect, Pattern, Rule, RuleSet, Trigger};
use crate::symbol::Symbol;
use crate::syntax::{Action, SyntaxElement, SyntaxPattern, SyntaxTable};
use crate::template::{FieldSpec, Template, TemplateRegistry};
use im::OrdMap;
use std::path::Path;

/// Error loading definitions.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum LoadError {
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("invalid definition: {0}")]
    InvalidDefinition(String),

    #[error("unknown entity: {0}")]
    UnknownEntity(String),

    #[error("io error: {0}")]
    Io(String),
}

/// Loads DSL definitions into a World.
pub struct WorldLoader {
    /// Entity names to IDs.
    entity_names: OrdMap<Symbol, EntityId>,
    /// Pending relations (resolved after all entities created).
    pending_relations: Vec<(EntityId, Symbol, Symbol)>,
    /// Template registry.
    templates: TemplateRegistry,
    /// Generator registry.
    generators: GeneratorRegistry,
    /// Stub storage for lazy generation.
    stubs: StubStorage,
    /// DSL-defined REPL commands.
    repl_commands: ReplCommandRegistry,
    /// Syntax table for command parsing (deprecated, use command_registry).
    syntax_table: SyntaxTable,
    /// Command registry for grammar-based command parsing.
    command_registry: CommandRegistry,
}

impl WorldLoader {
    pub fn new() -> Self {
        Self {
            entity_names: OrdMap::new(),
            pending_relations: Vec::new(),
            templates: TemplateRegistry::new(),
            generators: GeneratorRegistry::new(),
            stubs: StubStorage::new(),
            repl_commands: ReplCommandRegistry::new(),
            syntax_table: SyntaxTable::new(),
            command_registry: CommandRegistry::new(),
        }
    }

    /// Get the template registry.
    pub fn templates(&self) -> &TemplateRegistry {
        &self.templates
    }

    /// Get the template registry mutably.
    pub fn templates_mut(&mut self) -> &mut TemplateRegistry {
        &mut self.templates
    }

    /// Get the generator registry.
    pub fn generators(&self) -> &GeneratorRegistry {
        &self.generators
    }

    /// Get the generator registry mutably.
    pub fn generators_mut(&mut self) -> &mut GeneratorRegistry {
        &mut self.generators
    }

    /// Get the stub storage.
    pub fn stubs(&self) -> &StubStorage {
        &self.stubs
    }

    /// Get the stub storage mutably.
    pub fn stubs_mut(&mut self) -> &mut StubStorage {
        &mut self.stubs
    }

    /// Get the REPL command registry.
    pub fn repl_commands(&self) -> &ReplCommandRegistry {
        &self.repl_commands
    }

    /// Get the REPL command registry mutably.
    pub fn repl_commands_mut(&mut self) -> &mut ReplCommandRegistry {
        &mut self.repl_commands
    }

    /// Get the syntax table (deprecated, use command_registry).
    pub fn syntax_table(&self) -> &SyntaxTable {
        &self.syntax_table
    }

    /// Get the syntax table mutably (deprecated, use command_registry).
    pub fn syntax_table_mut(&mut self) -> &mut SyntaxTable {
        &mut self.syntax_table
    }

    /// Get the command registry.
    pub fn command_registry(&self) -> &CommandRegistry {
        &self.command_registry
    }

    /// Get the command registry mutably.
    pub fn command_registry_mut(&mut self) -> &mut CommandRegistry {
        &mut self.command_registry
    }

    /// Load definitions from source string.
    pub fn load_str(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        source: &str,
    ) -> Result<(), LoadError> {
        let exprs = parse_all(source)?;
        self.load_exprs(world, rules, &exprs)
    }

    /// Load definitions from a file.
    pub fn load_file(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        path: &Path,
    ) -> Result<(), LoadError> {
        let source = std::fs::read_to_string(path).map_err(|e| LoadError::Io(e.to_string()))?;
        self.load_str(world, rules, &source)
    }

    /// Load parsed expressions.
    fn load_exprs(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        exprs: &[SExpr],
    ) -> Result<(), LoadError> {
        // First pass: create entities, register relations, load templates, and register generators
        for expr in exprs {
            if expr.is_call("entity") {
                self.load_entity(world, expr)?;
            } else if expr.is_call("relation") {
                self.load_relation(world, expr)?;
            } else if expr.is_call("template") {
                self.load_template(expr)?;
            } else if expr.is_call("generator") {
                self.load_generator(expr)?;
            } else if expr.is_call("repl-command") {
                self.load_repl_command(expr)?;
            } else if expr.is_call("syntax") {
                self.load_syntax(expr)?;
            } else if expr.is_call("type") {
                self.load_type(expr)?;
            } else if expr.is_call("command") {
                self.load_command(expr)?;
            }
        }

        // Resolve pending relations
        self.resolve_relations(world)?;

        // Second pass: load rules and stubs (stubs need entities to be resolved first)
        for expr in exprs {
            if expr.is_call("rule") {
                self.load_rule(rules, expr)?;
            } else if expr.is_call("stub") {
                self.load_stub(expr)?;
            }
        }

        Ok(())
    }

    /// Load a REPL command definition.
    fn load_repl_command(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let cmd = parse_repl_command(expr).map_err(LoadError::InvalidDefinition)?;
        self.repl_commands.register(cmd);
        Ok(())
    }

    /// Load a type predicate definition.
    ///
    /// Format: `(type name predicate-expr)`
    ///
    /// Example:
    /// ```lisp
    /// (type portable (has? entity :Portable))
    /// (type container (has? entity :Container))
    /// (type held (held-by? entity actor))
    /// ```
    fn load_type(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() != 3 {
            return Err(LoadError::InvalidDefinition(
                "type requires name and predicate expression".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("type name must be a symbol".to_string())
        })?;

        // The predicate expression is stored as-is
        let predicate = items[2].clone();
        self.command_registry.register_type(name, predicate);

        Ok(())
    }

    /// Load a command definition.
    ///
    /// Format:
    /// ```lisp
    /// (command name
    ///   :aliases ("alias1" "alias2" ("multi" "word"))
    ///   :forms
    ///     (()                        -> action-name)
    ///     (("word" slot:type)        -> (action-name slot)))
    /// ```
    ///
    /// Example:
    /// ```lisp
    /// (command look
    ///   :aliases ("l" "examine" "x")
    ///   :forms
    ///     (()                        -> look-around)
    ///     (("at" obj:noun)           -> (examine obj))
    ///     ((dir:direction)           -> (look-direction dir)))
    /// ```
    fn load_command(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "command requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("command name must be a symbol".to_string())
        })?;

        let mut command = GrammarCommand::new(name);
        let mut aliases: Vec<Vec<String>> = Vec::new();

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("aliases") && i + 1 < items.len() {
                aliases = self.parse_aliases(&items[i + 1])?;
                i += 2;
            } else if items[i].is_keyword("forms") && i + 1 < items.len() {
                self.parse_forms(&mut command, &items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        self.command_registry.register_command(command, aliases);
        Ok(())
    }

    /// Parse alias list.
    ///
    /// Aliases can be:
    /// - Single strings: "l"
    /// - Multi-word lists: ("pick" "up")
    fn parse_aliases(&self, expr: &SExpr) -> Result<Vec<Vec<String>>, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("aliases must be a list".to_string()))?;

        let mut aliases = Vec::new();
        for item in items {
            if let Some(s) = item.as_string() {
                // Single word alias
                aliases.push(vec![s.to_string()]);
            } else if let Some(list) = item.as_list() {
                // Multi-word alias
                let mut words = Vec::new();
                for word in list {
                    let w = word.as_string().ok_or_else(|| {
                        LoadError::InvalidDefinition("alias words must be strings".to_string())
                    })?;
                    words.push(w.to_string());
                }
                aliases.push(words);
            } else {
                return Err(LoadError::InvalidDefinition(
                    "alias must be a string or list of strings".to_string(),
                ));
            }
        }

        Ok(aliases)
    }

    /// Parse form definitions.
    ///
    /// Format: `((pattern...) -> action)`
    fn parse_forms(&self, command: &mut GrammarCommand, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("forms must be a list".to_string()))?;

        for item in items {
            let form = self.parse_form(item)?;
            command.add_form(form);
        }

        Ok(())
    }

    /// Parse a single form definition.
    ///
    /// Format: `((pattern...) -> action)` where pattern tuple can be empty
    ///
    /// Examples:
    /// - `(() -> look-around)` - empty pattern
    /// - `(("at" obj:noun) -> (examine obj))` - pattern with elements
    fn parse_form(&self, expr: &SExpr) -> Result<Form, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("form must be a list".to_string()))?;

        // Expected format: (pattern-tuple -> action)
        // items[0] = pattern tuple (a list, possibly empty)
        // items[1] = ->
        // items[2] = action
        if items.len() != 3 {
            return Err(LoadError::InvalidDefinition(
                "form must be (pattern -> action)".to_string(),
            ));
        }

        if !items[1].is_symbol("->") {
            return Err(LoadError::InvalidDefinition(
                "form requires -> between pattern and action".to_string(),
            ));
        }

        // Parse pattern elements from the pattern tuple
        let pattern_items = items[0]
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("pattern must be a list".to_string()))?;

        let mut elements = Vec::new();
        for item in pattern_items {
            let element = self.parse_form_element(item)?;
            elements.push(element);
        }

        // Parse action
        let action = self.parse_form_action(&items[2])?;

        Ok(Form::new(elements, action))
    }

    /// Parse a form pattern element.
    ///
    /// - Strings are literal words: "at", "in"
    /// - Symbols without `:` are literal words: at, in
    /// - Symbols with `:` are typed slots: obj:noun, dir:direction
    ///
    /// Examples:
    /// - `"at"` or `at` -> Word("at")
    /// - `obj:noun` -> Slot { name: obj, type: Noun }
    /// - `dir:direction` -> Slot { name: dir, type: Direction }
    /// - `c:container` -> Slot { name: c, type: Custom(container) }
    fn parse_form_element(&self, expr: &SExpr) -> Result<FormElement, LoadError> {
        match expr {
            // String literal -> word element
            SExpr::Atom(Atom::String(s), _) => Ok(FormElement::word(s.as_str())),

            // Symbol -> check for slot syntax (name:type) or plain word
            SExpr::Atom(Atom::Symbol(sym), _) => {
                let s = sym.as_str();
                if let Some((name, type_str)) = s.split_once(':') {
                    // Typed slot: name:type
                    let slot_type = match type_str {
                        "noun" => crate::grammar::SlotType::Noun,
                        "direction" => crate::grammar::SlotType::Direction,
                        _ => crate::grammar::SlotType::Custom(Symbol::new(type_str)),
                    };
                    Ok(FormElement::Slot {
                        name: Symbol::new(name),
                        slot_type,
                    })
                } else {
                    // Plain symbol -> word element
                    Ok(FormElement::word(&s))
                }
            }

            _ => Err(LoadError::InvalidDefinition(
                "form element must be a string or symbol".to_string(),
            )),
        }
    }

    /// Parse a form action.
    ///
    /// - Symbol: simple action with no slots -> `look-around`
    /// - List: action with slots -> `(examine obj)` or `(take-from obj c)`
    fn parse_form_action(&self, expr: &SExpr) -> Result<FormAction, LoadError> {
        match expr {
            // Simple action: just the action name
            SExpr::Atom(Atom::Symbol(name), _) => Ok(FormAction::simple(*name)),

            // Action with slots: (action-name slot1 slot2 ...)
            SExpr::List(items, _) => {
                if items.is_empty() {
                    return Err(LoadError::InvalidDefinition(
                        "action list cannot be empty".to_string(),
                    ));
                }

                let action_name = items[0].as_symbol().ok_or_else(|| {
                    LoadError::InvalidDefinition("action name must be a symbol".to_string())
                })?;

                let slots: Result<Vec<Symbol>, _> = items[1..]
                    .iter()
                    .map(|item| {
                        item.as_symbol().ok_or_else(|| {
                            LoadError::InvalidDefinition(
                                "slot reference must be a symbol".to_string(),
                            )
                        })
                    })
                    .collect();

                Ok(FormAction::with_slots(action_name, slots?))
            }

            _ => Err(LoadError::InvalidDefinition(
                "action must be a symbol or list".to_string(),
            )),
        }
    }

    /// Load a syntax definition.
    ///
    /// Format: `(syntax "word" "word" noun :to action-name)`
    ///
    /// Elements:
    /// - Strings are literal words
    /// - `noun` or `noun:name` is a noun slot
    /// - `direction` or `direction:name` is a direction slot
    /// - `any:name` is an any-token slot
    /// - `:to` separates pattern from action
    fn load_syntax(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 3 {
            return Err(LoadError::InvalidDefinition(
                "syntax requires pattern and action".to_string(),
            ));
        }

        // Find the :to keyword that separates pattern from action
        let mut to_idx = None;
        for (i, item) in items.iter().enumerate().skip(1) {
            if item.is_keyword("to") {
                to_idx = Some(i);
                break;
            }
        }

        let to_idx = to_idx.ok_or_else(|| {
            LoadError::InvalidDefinition("syntax requires :to before action name".to_string())
        })?;

        if to_idx + 1 >= items.len() {
            return Err(LoadError::InvalidDefinition(
                "syntax requires action name after :to".to_string(),
            ));
        }

        // Parse action name
        let action_name = items[to_idx + 1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("action name must be a symbol".to_string())
        })?;

        // Parse pattern elements (between "syntax" and :to)
        // Handle named slots where symbol is followed by a keyword (e.g., noun :item)
        let mut elements = Vec::new();
        let pattern_items = &items[1..to_idx];
        let mut i = 0;
        while i < pattern_items.len() {
            let item = &pattern_items[i];

            // Check if this is a slot type symbol followed by a keyword (named slot)
            if let SExpr::Atom(Atom::Symbol(sym), _) = item {
                let sym_str = sym.as_str();
                // Check if next item is a keyword (the slot name)
                if i + 1 < pattern_items.len() {
                    if let SExpr::Atom(Atom::Keyword(name_sym), _) = &pattern_items[i + 1] {
                        let name = name_sym.as_str();
                        let element = match sym_str.as_str() {
                            "noun" => SyntaxElement::noun_named(&name),
                            "direction" => SyntaxElement::direction_named(&name),
                            "any" => SyntaxElement::any(&name),
                            _ => {
                                // Not a slot type, treat the keyword separately
                                elements.push(Self::parse_syntax_element(item)?);
                                i += 1;
                                continue;
                            }
                        };
                        elements.push(element);
                        i += 2; // Skip both symbol and keyword
                        continue;
                    }
                }
            }

            // Regular element parsing
            let element = Self::parse_syntax_element(item)?;
            elements.push(element);
            i += 1;
        }

        if elements.is_empty() {
            return Err(LoadError::InvalidDefinition(
                "syntax pattern cannot be empty".to_string(),
            ));
        }

        let pattern = SyntaxPattern::new(elements);
        let action = Action::new(action_name);
        self.syntax_table.add(pattern, action);

        Ok(())
    }

    /// Parse a single syntax pattern element.
    fn parse_syntax_element(expr: &SExpr) -> Result<SyntaxElement, LoadError> {
        match expr {
            // String literal → Word element
            SExpr::Atom(Atom::String(s), _) => Ok(SyntaxElement::word(s.as_str())),

            // Symbol → special slot type
            SExpr::Atom(Atom::Symbol(sym), _) => {
                let s = sym.as_str();
                // Check for named slots (noun:name, direction:name, any:name)
                if let Some((kind, name)) = s.split_once(':') {
                    match kind {
                        "noun" => Ok(SyntaxElement::noun_named(name)),
                        "direction" => Ok(SyntaxElement::direction_named(name)),
                        "any" => Ok(SyntaxElement::any(name)),
                        _ => Err(LoadError::InvalidDefinition(format!(
                            "unknown slot type: {kind}"
                        ))),
                    }
                } else {
                    // Unnamed slots
                    match s.as_str() {
                        "noun" => Ok(SyntaxElement::noun()),
                        "direction" => Ok(SyntaxElement::direction()),
                        _ => {
                            // Treat as a literal word (for convenience)
                            Ok(SyntaxElement::word(&s))
                        }
                    }
                }
            }

            // List → optional sequence
            SExpr::List(items, _) => {
                let mut opt_elements = Vec::new();
                for item in items {
                    opt_elements.push(Self::parse_syntax_element(item)?);
                }
                Ok(SyntaxElement::optional(opt_elements))
            }

            _ => Err(LoadError::InvalidDefinition(
                "invalid syntax element".to_string(),
            )),
        }
    }

    /// Load an entity definition.
    fn load_entity(&mut self, world: &mut World, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "entity requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("entity name must be a symbol".to_string())
        })?;

        let entity = world.create_entity();
        self.entity_names.insert(name, entity);

        // Track hook components to detect duplicates
        let mut seen_hooks: std::collections::HashSet<String> = std::collections::HashSet::new();

        // Process components and relations
        for item in &items[2..] {
            let comp = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("entity component must be a list".to_string())
            })?;

            if comp.is_empty() {
                return Err(LoadError::InvalidDefinition(
                    "component cannot be empty".to_string(),
                ));
            }

            let comp_name = comp[0].as_symbol().ok_or_else(|| {
                LoadError::InvalidDefinition("component name must be a symbol".to_string())
            })?;

            let comp_name_str = comp_name.as_str();

            // Check if this is a hook component (Before:*, On:*, After:*)
            if Self::is_hook_component(&comp_name_str) {
                // Check for duplicate hooks
                if seen_hooks.contains(&comp_name_str) {
                    return Err(LoadError::InvalidDefinition(format!(
                        "duplicate hook '{}' on entity '{}'",
                        comp_name_str,
                        name.as_str()
                    )));
                }
                seen_hooks.insert(comp_name_str.clone());

                // Hook component: rest of list is the hook body
                // Convert body expressions to Value::List
                let body_values: Result<Vec<Value>, _> =
                    comp[1..].iter().map(|e| self.expr_to_value(e)).collect();
                let hook_body = Value::list(body_values?);

                world.set_component(entity, &*comp_name_str, hook_body);
            } else if comp.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "component must have name and value".to_string(),
                ));
            } else {
                // Regular component (name value)

                // Check if this is a relation (value is a symbol that could be an entity name)
                if let Some(target_name) = comp[1].as_symbol() {
                    // Could be a relation - defer resolution
                    self.pending_relations
                        .push((entity, comp_name, target_name));
                } else {
                    // Regular component
                    let value = self.expr_to_value(&comp[1])?;
                    world.set_component(entity, comp_name.as_str().as_str(), value);
                }
            }
        }

        Ok(())
    }

    /// Check if a component name is a hook component.
    fn is_hook_component(name: &str) -> bool {
        name.starts_with("Before:") || name.starts_with("On:") || name.starts_with("After:")
    }

    /// Load a relation schema.
    fn load_relation(&mut self, world: &mut World, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "relation requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("relation name must be a symbol".to_string())
        })?;

        let mut from_card = Cardinality::Many;
        let mut to_card = Cardinality::One;

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("from") && i + 1 < items.len() {
                from_card = self.parse_cardinality(&items[i + 1])?;
                i += 2;
            } else if items[i].is_keyword("to") && i + 1 < items.len() {
                to_card = self.parse_cardinality(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        world.register_relation(RelationSchema::new(
            name.as_str().as_str(),
            from_card,
            to_card,
        ));
        Ok(())
    }

    /// Parse cardinality from :one or :many.
    fn parse_cardinality(&self, expr: &SExpr) -> Result<Cardinality, LoadError> {
        if expr.is_keyword("one") {
            Ok(Cardinality::One)
        } else if expr.is_keyword("many") {
            Ok(Cardinality::Many)
        } else {
            Err(LoadError::InvalidDefinition(
                "cardinality must be :one or :many".to_string(),
            ))
        }
    }

    /// Resolve pending relations after all entities are created.
    fn resolve_relations(&mut self, world: &mut World) -> Result<(), LoadError> {
        for (from, rel_name, target_name) in std::mem::take(&mut self.pending_relations) {
            if let Some(&target) = self.entity_names.get(&target_name) {
                // Target is an entity - check if this is a registered relation
                let rel_name_str = rel_name.as_str();
                if world.relation_schema(&*rel_name_str).is_some() {
                    // It's a registered relation - add as relation
                    world.add_relation(&*rel_name_str, from, target);
                } else {
                    // Not a registered relation - store as component with entity reference
                    world.set_component(from, &*rel_name_str, Value::EntityRef(target));
                }
            } else {
                // Not an entity - treat as a symbol component value
                world.set_component(from, rel_name.as_str().as_str(), Value::Symbol(target_name));
            }
        }
        Ok(())
    }

    /// Load a rule definition.
    fn load_rule(&self, rules: &mut RuleSet, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "rule requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("rule name must be a symbol".to_string())
        })?;

        let mut pattern: Option<Pattern> = None;
        let mut trigger: Option<Trigger> = None;
        let mut effect: Option<Effect> = None;

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("pattern") && i + 1 < items.len() {
                pattern = Some(self.parse_pattern(&items[i + 1])?);
                i += 2;
            } else if items[i].is_keyword("every") && i + 1 < items.len() {
                let interval = items[i + 1].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition(":every requires an integer".to_string())
                })?;
                trigger = Some(Trigger::every(interval as u64));
                i += 2;
            } else if items[i].is_keyword("effect") && i + 1 < items.len() {
                effect = Some(self.parse_effect(&items[i + 1])?);
                i += 2;
            } else {
                i += 1;
            }
        }

        let pattern = pattern.unwrap_or_else(|| Pattern::entity("?e"));
        let trigger = trigger.unwrap_or_else(|| Trigger::every(1));
        let effect = effect
            .ok_or_else(|| LoadError::InvalidDefinition("rule requires an effect".to_string()))?;

        rules.add_rule(Rule::new(name.as_str().as_str(), pattern, trigger, effect));
        Ok(())
    }

    /// Load a template definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (template goblin
    ///   :doc "A small green creature"
    ///   :tags (:creature :hostile)
    ///   :fields
    ///     ((Name "Goblin")
    ///      (HP (range 8 15))
    ///      (Speed (range-f64 0.8 1.2))
    ///      (Element (choice :fire :ice :lightning))
    ///      (HasPoison (chance 0.3))))
    /// ```
    fn load_template(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "template requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("template name must be a symbol".to_string())
        })?;

        let mut template = Template::new(name);

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("doc") && i + 1 < items.len() {
                if let Some(doc) = items[i + 1].as_string() {
                    template.set_doc(doc);
                }
                i += 2;
            } else if items[i].is_keyword("tags") && i + 1 < items.len() {
                self.parse_template_tags(&mut template, &items[i + 1])?;
                i += 2;
            } else if items[i].is_keyword("fields") && i + 1 < items.len() {
                self.parse_template_fields(&mut template, &items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        self.templates.register(template);
        Ok(())
    }

    /// Load a generator definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (generator treasure-gen
    ///   :doc "Generates treasure for containers"
    ///   :produces (:Contents :Value))
    /// ```
    ///
    /// Note: Generator bodies must be registered in Rust code.
    /// The DSL defines generator metadata/schema only.
    fn load_generator(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "generator requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("generator name must be a symbol".to_string())
        })?;

        let mut doc: Option<String> = None;
        let mut produces: Vec<Symbol> = Vec::new();

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("doc") && i + 1 < items.len() {
                if let Some(d) = items[i + 1].as_string() {
                    doc = Some(d.to_string());
                }
                i += 2;
            } else if items[i].is_keyword("produces") && i + 1 < items.len() {
                produces = self.parse_symbol_list(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        // Register a placeholder generator that will be overwritten by Rust code.
        // This allows the DSL to declare generator interfaces.
        self.generators.register_with_doc(
            name.as_str(),
            doc,
            produces.iter().map(|s| s.as_str().to_string()).collect(),
            |_world, _entity, _rng, _params| {
                // Placeholder: real implementation must be provided in Rust
                crate::Value::Bool(false)
            },
        );

        Ok(())
    }

    /// Load a stub definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (stub entity-name :component ComponentName :generator gen-name :seed 12345
    ///   :params ((key value) ...))
    /// ```
    fn load_stub(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "stub requires an entity name".to_string(),
            ));
        }

        let entity_name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("stub entity must be a symbol".to_string())
        })?;

        let entity = self
            .entity_names
            .get(&entity_name)
            .copied()
            .ok_or_else(|| LoadError::UnknownEntity(entity_name.as_str().to_string()))?;

        let mut component: Option<Symbol> = None;
        let mut generator: Option<Symbol> = None;
        let mut seed: u64 = 0;
        let mut params: OrdMap<String, Value> = OrdMap::new();

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("component") && i + 1 < items.len() {
                component = items[i + 1]
                    .as_symbol()
                    .or_else(|| items[i + 1].as_keyword());
                i += 2;
            } else if items[i].is_keyword("generator") && i + 1 < items.len() {
                generator = items[i + 1].as_symbol();
                i += 2;
            } else if items[i].is_keyword("seed") && i + 1 < items.len() {
                seed = items[i + 1].as_int().unwrap_or(0) as u64;
                i += 2;
            } else if items[i].is_keyword("params") && i + 1 < items.len() {
                params = self.parse_params(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        let component = component
            .ok_or_else(|| LoadError::InvalidDefinition("stub requires :component".to_string()))?;
        let generator = generator
            .ok_or_else(|| LoadError::InvalidDefinition("stub requires :generator".to_string()))?;

        let stub = GenerationStub::new(generator, seed).with_params(params);
        let component_str = component.as_str();
        self.stubs.add(entity, &*component_str, stub);

        Ok(())
    }

    /// Parse a list of symbols/keywords.
    fn parse_symbol_list(&self, expr: &SExpr) -> Result<Vec<Symbol>, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("expected a list".to_string()))?;

        let mut result = Vec::new();
        for item in items {
            if let Some(s) = item.as_symbol() {
                result.push(s);
            } else if let Some(k) = item.as_keyword() {
                result.push(k);
            }
        }
        Ok(result)
    }

    /// Parse params from a list of (key value) pairs.
    fn parse_params(&self, expr: &SExpr) -> Result<OrdMap<String, Value>, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("params must be a list".to_string()))?;

        let mut result = OrdMap::new();
        for item in items {
            let pair = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("param must be a (key value) pair".to_string())
            })?;

            if pair.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "param must have key and value".to_string(),
                ));
            }

            let key = pair[0]
                .as_symbol()
                .or_else(|| pair[0].as_keyword())
                .ok_or_else(|| {
                    LoadError::InvalidDefinition("param key must be a symbol".to_string())
                })?;

            let value = self.expr_to_value(&pair[1])?;
            result.insert(key.as_str().to_string(), value);
        }

        Ok(result)
    }

    /// Parse template tags from a list of keywords.
    fn parse_template_tags(&self, template: &mut Template, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("tags must be a list".to_string()))?;

        for item in items {
            if let Some(tag) = item.as_keyword() {
                template.add_tag(tag);
            } else if let Some(tag) = item.as_symbol() {
                template.add_tag(tag);
            }
        }

        Ok(())
    }

    /// Parse template fields from a list of field definitions.
    fn parse_template_fields(
        &self,
        template: &mut Template,
        expr: &SExpr,
    ) -> Result<(), LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("fields must be a list".to_string()))?;

        for item in items {
            let field_def = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("field definition must be a list".to_string())
            })?;

            if field_def.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "field definition must have name and spec".to_string(),
                ));
            }

            let field_name = field_def[0].as_symbol().ok_or_else(|| {
                LoadError::InvalidDefinition("field name must be a symbol".to_string())
            })?;

            let field_spec = self.parse_field_spec(&field_def[1])?;
            template.set_field(field_name, field_spec);
        }

        Ok(())
    }

    /// Parse a field specification.
    ///
    /// Supports:
    /// - Literal values (42, "hello", :keyword)
    /// - (range min max) for integer ranges
    /// - (range-f64 min max) for float ranges
    /// - (choice v1 v2 ...) for random choice
    /// - (chance probability) for boolean chance
    fn parse_field_spec(&self, expr: &SExpr) -> Result<FieldSpec, LoadError> {
        // Check for special forms
        if let Some(items) = expr.as_list() {
            if items.is_empty() {
                return Err(LoadError::InvalidDefinition("empty field spec".to_string()));
            }

            // (range min max)
            if items[0].is_symbol("range") {
                if items.len() != 3 {
                    return Err(LoadError::InvalidDefinition(
                        "range requires min and max".to_string(),
                    ));
                }
                let min = items[1].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition("range min must be an integer".to_string())
                })?;
                let max = items[2].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition("range max must be an integer".to_string())
                })?;
                return Ok(FieldSpec::range(min, max));
            }

            // (range-f64 min max)
            if items[0].is_symbol("range-f64") {
                if items.len() != 3 {
                    return Err(LoadError::InvalidDefinition(
                        "range-f64 requires min and max".to_string(),
                    ));
                }
                let min = self.parse_float(&items[1])?;
                let max = self.parse_float(&items[2])?;
                return Ok(FieldSpec::range_f64(min, max));
            }

            // (choice v1 v2 ...)
            if items[0].is_symbol("choice") {
                let values: Result<Vec<Value>, _> =
                    items[1..].iter().map(|e| self.expr_to_value(e)).collect();
                return Ok(FieldSpec::choice(values?));
            }

            // (chance probability)
            if items[0].is_symbol("chance") {
                if items.len() != 2 {
                    return Err(LoadError::InvalidDefinition(
                        "chance requires a probability".to_string(),
                    ));
                }
                let prob = self.parse_float(&items[1])?;
                return Ok(FieldSpec::chance(prob));
            }
        }

        // Otherwise, it's a fixed value
        let value = self.expr_to_value(expr)?;
        Ok(FieldSpec::fixed(value))
    }

    /// Parse a float from an expression (int or float).
    fn parse_float(&self, expr: &SExpr) -> Result<f64, LoadError> {
        if let Some(f) = expr.as_float() {
            Ok(f)
        } else if let Some(i) = expr.as_int() {
            Ok(i as f64)
        } else {
            Err(LoadError::InvalidDefinition(
                "expected a number".to_string(),
            ))
        }
    }

    /// Parse a pattern expression.
    fn parse_pattern(&self, expr: &SExpr) -> Result<Pattern, LoadError> {
        // Simple pattern: (= (get ?e Name) "goat")
        if expr.is_call("=") || expr.is_call("==") {
            let items = expr.as_list().unwrap();
            if items.len() != 3 {
                return Err(LoadError::InvalidDefinition(
                    "= requires 2 arguments".to_string(),
                ));
            }

            // Check for (get ?e Component) form
            if items[1].is_call("get") || items[1].is_call("get-component") {
                let get_items = items[1].as_list().unwrap();
                if get_items.len() == 3 {
                    let var = self.parse_var(&get_items[1])?;
                    let comp = self.parse_component_name(&get_items[2])?;
                    let value = self.expr_to_value(&items[2])?;
                    return Ok(Pattern::component_value(
                        var.as_str().as_str(),
                        comp.as_str().as_str(),
                        value,
                    ));
                }
            }
        }

        // Pattern: (has ?e :Component)
        if expr.is_call("has") || expr.is_call("has-component") {
            let items = expr.as_list().unwrap();
            if items.len() == 3 {
                let var = self.parse_var(&items[1])?;
                let comp = self.parse_component_name(&items[2])?;
                return Ok(Pattern::has_component(
                    var.as_str().as_str(),
                    comp.as_str().as_str(),
                ));
            }
        }

        // Pattern: (entity ?e)
        if expr.is_call("entity") {
            let items = expr.as_list().unwrap();
            if items.len() == 2 {
                let var = self.parse_var(&items[1])?;
                return Ok(Pattern::entity(var.as_str().as_str()));
            }
        }

        // Default: match any entity
        Ok(Pattern::entity("?e"))
    }

    /// Parse a variable like ?e.
    fn parse_var(&self, expr: &SExpr) -> Result<Symbol, LoadError> {
        expr.as_symbol()
            .ok_or_else(|| LoadError::InvalidDefinition("expected variable symbol".to_string()))
    }

    /// Parse a component name (symbol or keyword).
    fn parse_component_name(&self, expr: &SExpr) -> Result<Symbol, LoadError> {
        expr.as_symbol()
            .or_else(|| expr.as_keyword())
            .ok_or_else(|| LoadError::InvalidDefinition("expected component name".to_string()))
    }

    /// Parse an effect expression.
    fn parse_effect(&self, expr: &SExpr) -> Result<Effect, LoadError> {
        if expr.is_call("emit-message") || expr.is_call("emit") {
            let items = expr.as_list().unwrap();
            if items.len() >= 2 {
                let msg = items[1].as_string().ok_or_else(|| {
                    LoadError::InvalidDefinition("emit-message requires a string".to_string())
                })?;
                return Ok(Effect::emit_message(msg));
            }
        }

        Err(LoadError::InvalidDefinition(format!(
            "unknown effect: {expr}"
        )))
    }

    /// Convert an expression to a Value.
    #[allow(clippy::only_used_in_recursion)]
    fn expr_to_value(&self, expr: &SExpr) -> Result<Value, LoadError> {
        match expr {
            SExpr::Atom(atom, _) => match atom {
                Atom::Int(n) => Ok(Value::Int(*n)),
                Atom::Float(n) => Ok(Value::Float(*n)),
                Atom::Bool(b) => Ok(Value::Bool(*b)),
                Atom::String(s) => Ok(Value::string(s.clone())),
                Atom::Symbol(s) => Ok(Value::Symbol(*s)),
                Atom::Keyword(k) => Ok(Value::Symbol(*k)),
            },
            SExpr::List(items, _) => {
                // Convert list to Value::List
                let values: Result<Vec<Value>, LoadError> =
                    items.iter().map(|item| self.expr_to_value(item)).collect();
                Ok(Value::list(values?))
            }
        }
    }

    /// Get an entity by name.
    pub fn get_entity(&self, name: &str) -> Option<EntityId> {
        self.entity_names.get(&Symbol::new(name)).copied()
    }
}

impl Default for WorldLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_entity() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"(entity goat (Name "goat") (HP 10))"#,
            )
            .unwrap();

        let goat = loader.get_entity("goat").unwrap();
        assert_eq!(
            world.get_component(goat, "Name"),
            Some(&Value::string("goat"))
        );
        assert_eq!(world.get_component(goat, "HP"), Some(&Value::Int(10)));
    }

    #[test]
    fn test_load_relation() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (relation Location :from :many :to :one)
                (entity room (Name "A room"))
                (entity goat (Name "goat") (Location room))
                "#,
            )
            .unwrap();

        let room = loader.get_entity("room").unwrap();
        let goat = loader.get_entity("goat").unwrap();

        assert!(world.has_relation("Location", goat, room));
    }

    #[test]
    fn test_load_rule() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (rule goat-baas
                  :pattern (= (get ?e Name) "goat")
                  :every 10
                  :effect (emit-message "Baa!"))
                "#,
            )
            .unwrap();

        assert_eq!(rules.len(), 1);
    }

    #[test]
    fn test_full_example() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                ; Relation schema
                (relation Location :from :many :to :one)

                ; Entities
                (entity room (Name "A small room"))
                (entity goat (Name "goat") (HP 10) (Location room))

                ; Rule
                (rule goat-baas
                  :pattern (= (get ?e Name) "goat")
                  :every 10
                  :effect (emit-message "The goat says: Baa!"))
                "#,
            )
            .unwrap();

        assert_eq!(world.entity_count(), 2);
        assert_eq!(rules.len(), 1);

        let goat = loader.get_entity("goat").unwrap();
        assert_eq!(
            world.get_component(goat, "Name"),
            Some(&Value::string("goat"))
        );
    }

    #[test]
    fn test_load_template_basic() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :doc "A small green creature"
                  :fields
                    ((Name "Goblin")
                     (HP (range 8 15))))
                "#,
            )
            .unwrap();

        assert!(loader.templates().contains("goblin"));
        let template = loader.templates().get("goblin").unwrap();
        assert_eq!(template.doc(), Some("A small green creature"));
    }

    #[test]
    fn test_load_template_with_tags() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :tags (:creature :hostile)
                  :fields ((Name "Goblin")))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("goblin").unwrap();
        assert!(template.has_tag("creature"));
        assert!(template.has_tag("hostile"));
    }

    #[test]
    fn test_load_template_all_field_specs() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template test-creature
                  :fields
                    ((Name "Test")
                     (HP (range 10 20))
                     (Speed (range-f64 0.5 1.5))
                     (Element (choice :fire :ice :lightning))
                     (HasPoison (chance 0.5))))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("test-creature").unwrap();

        // Verify field specs exist
        assert!(template.get_field("Name").is_some());
        assert!(template.get_field("HP").is_some());
        assert!(template.get_field("Speed").is_some());
        assert!(template.get_field("Element").is_some());
        assert!(template.get_field("HasPoison").is_some());
    }

    #[test]
    fn test_load_template_instantiate() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :fields
                    ((Name "Goblin")
                     (HP (range 8 15))
                     (Strength (range 6 12))))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("goblin").unwrap();
        let entity = template.instantiate_with_seed(&mut world, 12345);

        // Check the entity was created with expected components
        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("Goblin"))
        );

        // HP should be in range
        if let Some(Value::Int(hp)) = world.get_component(entity, "HP") {
            assert!((8..=15).contains(hp), "HP {hp} not in range 8-15");
        } else {
            panic!("Expected HP to be Int");
        }
    }

    #[test]
    fn test_load_generator() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator treasure-gen
                  :doc "Generates treasure for containers"
                  :produces (:Contents :Value))
                "#,
            )
            .unwrap();

        assert!(loader.generators().contains("treasure-gen"));
        let generator = loader.generators().get("treasure-gen").unwrap();
        assert_eq!(generator.doc(), Some("Generates treasure for containers"));
    }

    #[test]
    fn test_load_stub() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator item-gen :doc "Generates items")
                (entity chest (Name "Treasure Chest"))
                (stub chest :component Contents :generator item-gen :seed 12345)
                "#,
            )
            .unwrap();

        let chest = loader.get_entity("chest").unwrap();

        // Check that the stub was registered
        assert!(loader.stubs().has(chest, "Contents"));

        let stub = loader.stubs().get(chest, "Contents").unwrap();
        assert_eq!(stub.generator().as_str(), "item-gen");
        assert_eq!(stub.seed(), 12345);
    }

    #[test]
    fn test_load_stub_with_params() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator item-gen :doc "Generates items")
                (entity chest (Name "Treasure Chest"))
                (stub chest :component Contents :generator item-gen :seed 42
                  :params ((min-value 100) (max-value 500)))
                "#,
            )
            .unwrap();

        let chest = loader.get_entity("chest").unwrap();
        let stub = loader.stubs().get(chest, "Contents").unwrap();

        assert_eq!(stub.get_param("min-value"), Some(&Value::Int(100)));
        assert_eq!(stub.get_param("max-value"), Some(&Value::Int(500)));
    }

    #[test]
    fn test_stub_unknown_entity_error() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        let result = loader.load_str(
            &mut world,
            &mut rules,
            r#"
            (generator item-gen)
            (stub nonexistent :component Contents :generator item-gen :seed 1)
            "#,
        );

        assert!(matches!(result, Err(LoadError::UnknownEntity(_))));
    }

    #[test]
    fn test_load_syntax_basic() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "look" :to look-around)
                (syntax "l" :to look-around)
                "#,
            )
            .unwrap();

        // Test that both patterns match
        let result = loader.syntax_table().best_match(&["look"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.name(), "look-around");

        let result2 = loader.syntax_table().best_match(&["l"]);
        assert!(result2.is_some());
        assert_eq!(result2.unwrap().action.name(), "look-around");
    }

    #[test]
    fn test_load_syntax_with_noun() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "examine" noun :to examine)
                (syntax "x" noun :to examine)
                "#,
            )
            .unwrap();

        let result = loader.syntax_table().best_match(&["examine", "sword"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.name(), "examine");
        assert_eq!(m.slots.get("noun"), Some(&"sword".to_string()));
    }

    #[test]
    fn test_load_syntax_with_direction() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "go" direction :to go)
                (syntax direction :to go)
                "#,
            )
            .unwrap();

        let result = loader.syntax_table().best_match(&["go", "north"]);
        assert!(result.is_some());
        assert_eq!(result.unwrap().action.name(), "go");

        // Bare direction should also work
        let result2 = loader.syntax_table().best_match(&["north"]);
        assert!(result2.is_some());
        assert_eq!(result2.unwrap().action.name(), "go");
    }

    #[test]
    fn test_load_syntax_with_to_keyword() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        // Test :to keyword
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "take" noun :to take)
                "#,
            )
            .unwrap();

        let result = loader.syntax_table().best_match(&["take", "key"]);
        assert!(result.is_some());
        assert_eq!(result.unwrap().action.name(), "take");
    }

    #[test]
    fn test_load_syntax_complex_pattern() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        // Use two noun slots with default names
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "put" noun "in" noun :to put-in)
                "#,
            )
            .unwrap();

        let result = loader
            .syntax_table()
            .best_match(&["put", "sword", "in", "chest"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.name(), "put-in");
        // With unnamed slots, the second noun slot overwrites the first in the HashMap
        assert_eq!(m.slots.get("noun"), Some(&"chest".to_string()));
    }

    #[test]
    fn test_load_syntax_named_slots() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        // Test named slots using colon notation in a quoted string
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (syntax "give" noun:item "to" noun:recipient :to give)
                "#,
            )
            .unwrap();

        let result = loader
            .syntax_table()
            .best_match(&["give", "key", "to", "guard"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.name(), "give");
        assert_eq!(m.slots.get("item"), Some(&"key".to_string()));
        assert_eq!(m.slots.get("recipient"), Some(&"guard".to_string()));
    }

    #[test]
    fn test_load_type_predicate() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (type portable (has? entity :Portable))
                (type container (has? entity :Container))
                "#,
            )
            .unwrap();

        assert!(
            loader
                .command_registry()
                .types()
                .contains(Symbol::new("portable"))
        );
        assert!(
            loader
                .command_registry()
                .types()
                .contains(Symbol::new("container"))
        );
    }

    #[test]
    fn test_load_command_basic() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command look
                  :aliases ("l" "examine")
                  :forms
                    ((() -> look-around)
                     (("at" obj:noun) -> (examine obj))))
                "#,
            )
            .unwrap();

        assert!(loader.command_registry().has_command(Symbol::new("look")));
    }

    #[test]
    fn test_load_command_with_multi_word_alias() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command take
                  :aliases ("get" "grab" ("pick" "up"))
                  :forms
                    (((obj:noun) -> (take obj))))
                "#,
            )
            .unwrap();

        assert!(loader.command_registry().has_command(Symbol::new("take")));
    }

    #[test]
    fn test_load_command_with_direction() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command go
                  :forms
                    (((dir:direction) -> (go dir))))
                "#,
            )
            .unwrap();

        assert!(loader.command_registry().has_command(Symbol::new("go")));
    }

    #[test]
    fn test_load_command_with_custom_type() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (type portable (has? entity :Portable))
                (command take
                  :forms
                    (((obj:portable) -> (take obj))))
                "#,
            )
            .unwrap();

        assert!(loader.command_registry().has_command(Symbol::new("take")));
        assert!(
            loader
                .command_registry()
                .types()
                .contains(Symbol::new("portable"))
        );
    }

    #[test]
    fn test_command_registry_match() {
        use crate::core::{Cardinality, ComponentTypeId, RelationSchema, RelationTypeId};

        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        // Register relations needed for entity resolution
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

        // Create entities
        let room = world.create_entity();
        world.set_component(room, ComponentTypeId::new("IsRoom"), Value::Bool(true));

        let player = world.create_entity();
        world.add_relation(RelationTypeId::new("InRoom"), player, room);

        let lamp = world.create_entity();
        world.set_component(
            lamp,
            ComponentTypeId::new("Name"),
            Value::String("lamp".into()),
        );
        world.add_relation(RelationTypeId::new("InRoom"), lamp, room);

        // Load command definitions
        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (command look
                  :aliases ("l")
                  :forms
                    ((() -> look-around)
                     (("at" obj:noun) -> (examine obj))))
                "#,
            )
            .unwrap();

        // Test matching
        let registry = loader.command_registry();

        // "look" should match the empty form
        let result = registry.match_input(&world, player, &["look"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("look-around"));

        // "l" alias should also work
        let result = registry.match_input(&world, player, &["l"]);
        assert!(result.is_some());
        assert_eq!(
            result.unwrap().action.action_name(),
            Symbol::new("look-around")
        );

        // "look at lamp" should match the second form
        let result = registry.match_input(&world, player, &["look", "at", "lamp"]);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("examine"));
        assert_eq!(m.get_entity("obj"), Some(lamp));
    }
}
