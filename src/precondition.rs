//! Precondition system for action validation.
//!
//! Preconditions are predicates that must be satisfied before an action can be executed.
//! They provide declarative validation with automatic failure message generation.
//!
//! # Example
//!
//! ```ignore
//! // In DSL:
//! (precondition reachable?
//!   :params (actor target)
//!   :check (in-scope? target actor)
//!   :failure "You can't reach ~(name target).")
//!
//! (action take
//!   :preconditions [(reachable? actor direct-object)
//!                   (portable? direct-object)]
//!   :handler take-handler)
//! ```

use crate::compiler::{CompileError, Compiler};
use crate::core::{EntityId, Value, World};
use crate::lang::SExpr;
use crate::symbol::Symbol;
use crate::vm::{ActionContext, StdLib, VM, VMError};
use im::OrdMap;
use std::sync::Arc;

/// A precondition definition.
///
/// Preconditions are named predicates with:
/// - Parameters (e.g., actor, target)
/// - A check expression that evaluates to a boolean
/// - A failure message template with interpolation
#[derive(Debug, Clone)]
pub struct Precondition {
    /// The name of the precondition (e.g., "reachable?").
    name: Symbol,
    /// Parameter names.
    params: Vec<Symbol>,
    /// The check expression (as AST).
    check: SExpr,
    /// The failure message template.
    failure_template: Arc<str>,
}

impl Precondition {
    /// Create a new precondition.
    pub fn new(
        name: Symbol,
        params: Vec<Symbol>,
        check: SExpr,
        failure_template: impl Into<Arc<str>>,
    ) -> Self {
        Self {
            name,
            params,
            check,
            failure_template: failure_template.into(),
        }
    }

    /// Get the precondition name.
    pub fn name(&self) -> Symbol {
        self.name
    }

    /// Get the parameter names.
    pub fn params(&self) -> &[Symbol] {
        &self.params
    }

    /// Get the check expression.
    pub fn check(&self) -> &SExpr {
        &self.check
    }

    /// Get the failure message template.
    pub fn failure_template(&self) -> &str {
        &self.failure_template
    }
}

/// A precondition invocation (precondition name + arguments).
#[derive(Debug, Clone)]
pub struct PreconditionCall {
    /// The precondition name.
    pub name: Symbol,
    /// Arguments to the precondition.
    pub args: Vec<PreconditionArg>,
}

impl PreconditionCall {
    /// Create a new precondition call.
    pub fn new(name: Symbol, args: Vec<PreconditionArg>) -> Self {
        Self { name, args }
    }
}

/// An argument to a precondition call.
#[derive(Debug, Clone)]
pub enum PreconditionArg {
    /// The actor entity.
    Actor,
    /// The direct object entity.
    DirectObject,
    /// The indirect object entity.
    IndirectObject,
    /// A specific entity by ID.
    Entity(EntityId),
    /// A symbol reference (resolved at runtime).
    Symbol(Symbol),
}

/// Error during precondition evaluation.
#[derive(Debug, Clone, thiserror::Error)]
pub enum PreconditionError {
    #[error("unknown precondition: {0}")]
    UnknownPrecondition(String),

    #[error("compile error: {0}")]
    Compile(#[from] CompileError),

    #[error("VM error: {0}")]
    VM(#[from] VMError),

    #[error("precondition {0} expects {1} arguments, got {2}")]
    WrongArity(String, usize, usize),

    #[error("missing required entity for argument: {0}")]
    MissingEntity(String),
}

/// Result of checking a precondition.
#[derive(Debug, Clone)]
pub enum PreconditionResult {
    /// Precondition passed.
    Passed,
    /// Precondition failed with a message.
    Failed(String),
}

impl PreconditionResult {
    /// Check if the precondition passed.
    pub fn passed(&self) -> bool {
        matches!(self, PreconditionResult::Passed)
    }

    /// Check if the precondition failed.
    pub fn failed(&self) -> bool {
        matches!(self, PreconditionResult::Failed(_))
    }

    /// Get the failure message, if any.
    pub fn message(&self) -> Option<&str> {
        match self {
            PreconditionResult::Passed => None,
            PreconditionResult::Failed(msg) => Some(msg),
        }
    }
}

/// Registry of precondition definitions.
#[derive(Debug, Clone, Default)]
pub struct PreconditionRegistry {
    preconditions: OrdMap<Symbol, Precondition>,
}

impl PreconditionRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            preconditions: OrdMap::new(),
        }
    }

    /// Create a registry with built-in preconditions.
    pub fn with_builtins() -> Self {
        let mut registry = Self::new();
        registry.register_builtins();
        registry
    }

    /// Register the built-in preconditions.
    fn register_builtins(&mut self) {
        // These are placeholder definitions - actual checking is done via special-cased
        // evaluation in check_precondition. The SExpr here is just for documentation.
        // In practice, built-in preconditions are handled specially.
    }

    /// Register a precondition.
    pub fn register(&mut self, precondition: Precondition) {
        self.preconditions.insert(precondition.name, precondition);
    }

    /// Get a precondition by name.
    pub fn get(&self, name: Symbol) -> Option<&Precondition> {
        self.preconditions.get(&name)
    }

    /// Check if a precondition exists.
    pub fn contains(&self, name: Symbol) -> bool {
        self.preconditions.contains_key(&name)
    }

    /// Get all precondition names.
    pub fn names(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.preconditions.keys().copied()
    }

    /// Check a precondition call against the world state.
    pub fn check(
        &self,
        world: &World,
        call: &PreconditionCall,
        context: &ActionContext,
        stdlib: &StdLib,
    ) -> Result<PreconditionResult, PreconditionError> {
        // First try built-in preconditions
        if let Some(result) = self.check_builtin(world, call, context)? {
            return Ok(result);
        }

        // Look up custom precondition
        let precondition = self.get(call.name).ok_or_else(|| {
            PreconditionError::UnknownPrecondition(call.name.as_str().to_string())
        })?;

        // Check arity
        if call.args.len() != precondition.params.len() {
            return Err(PreconditionError::WrongArity(
                call.name.as_str().to_string(),
                precondition.params.len(),
                call.args.len(),
            ));
        }

        // Compile and execute the check expression
        let chunk = Compiler::compile(precondition.check())?;
        let mut vm = VM::new(&chunk, world, stdlib).with_action_context(context.clone());

        // TODO: bind parameters to their values before execution
        // For now, we rely on the check expression using (actor), (direct-object), etc.

        let result = vm.run()?;

        // Evaluate result as boolean
        let passed = match result {
            Value::Bool(b) => b,
            _ => false, // Non-boolean treated as false
        };

        if passed {
            Ok(PreconditionResult::Passed)
        } else {
            let message = self.format_failure_message(world, precondition, call, context);
            Ok(PreconditionResult::Failed(message))
        }
    }

    /// Check a built-in precondition.
    fn check_builtin(
        &self,
        world: &World,
        call: &PreconditionCall,
        context: &ActionContext,
    ) -> Result<Option<PreconditionResult>, PreconditionError> {
        let name = call.name.as_str();

        match name.as_str() {
            "reachable?" => {
                // reachable?(actor, target) - target is in scope
                let target = self.resolve_arg(world, call.args.get(1), context)?;
                if let Some(target) = target {
                    let passed = world.is_in_scope(context.actor, target);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "You can't reach the {name}."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Failed(
                        "You can't reach that.".to_string(),
                    )))
                }
            }

            "visible?" => {
                // visible?(actor, target) - same as reachable for now
                let target = self.resolve_arg(world, call.args.get(1), context)?;
                if let Some(target) = target {
                    let passed = world.is_in_scope(context.actor, target);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "You can't see the {name}."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Failed(
                        "You can't see that.".to_string(),
                    )))
                }
            }

            "held?" => {
                // held?(obj) - obj is held by the current actor
                let target = self.resolve_arg(world, call.args.first(), context)?;
                if let Some(target) = target {
                    let passed = world.is_held_by(target, context.actor);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "You're not holding the {name}."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Failed(
                        "You're not holding that.".to_string(),
                    )))
                }
            }

            "held-by?" => {
                // held-by?(obj, holder) - obj is held by the specified holder
                let target = self.resolve_arg(world, call.args.first(), context)?;
                let holder = self.resolve_arg(world, call.args.get(1), context)?;
                if let (Some(target), Some(holder)) = (target, holder) {
                    let passed = world.is_held_by(target, holder);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        let holder_name = entity_name(world, holder);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "The {holder_name} isn't holding the {name}."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Failed(
                        "That isn't being held.".to_string(),
                    )))
                }
            }

            "portable?" => {
                // portable?(obj) - obj has Portable component or lacks Fixed
                let target = self.resolve_arg(world, call.args.first(), context)?;
                if let Some(target) = target {
                    let passed = world.is_portable(target);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "The {name} is fixed in place."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Failed(
                        "That's fixed in place.".to_string(),
                    )))
                }
            }

            "not-held?" => {
                // not-held?(obj) - obj is NOT held by the current actor
                let target = self.resolve_arg(world, call.args.first(), context)?;
                if let Some(target) = target {
                    let passed = !world.is_held_by(target, context.actor);
                    if passed {
                        Ok(Some(PreconditionResult::Passed))
                    } else {
                        let name = entity_name(world, target);
                        Ok(Some(PreconditionResult::Failed(format!(
                            "You're already holding the {name}."
                        ))))
                    }
                } else {
                    Ok(Some(PreconditionResult::Passed))
                }
            }

            _ => Ok(None), // Not a built-in
        }
    }

    /// Resolve a precondition argument to an entity.
    fn resolve_arg(
        &self,
        _world: &World,
        arg: Option<&PreconditionArg>,
        context: &ActionContext,
    ) -> Result<Option<EntityId>, PreconditionError> {
        match arg {
            None => Ok(None),
            Some(PreconditionArg::Actor) => Ok(Some(context.actor)),
            Some(PreconditionArg::DirectObject) => Ok(context.direct_object),
            Some(PreconditionArg::IndirectObject) => Ok(context.indirect_object),
            Some(PreconditionArg::Entity(id)) => Ok(Some(*id)),
            Some(PreconditionArg::Symbol(sym)) => {
                // Try to resolve symbol to a context value
                let s = sym.as_str();
                match s.as_str() {
                    "actor" => Ok(Some(context.actor)),
                    "direct-object" => Ok(context.direct_object),
                    "indirect-object" => Ok(context.indirect_object),
                    _ => Ok(None),
                }
            }
        }
    }

    /// Format a failure message with entity name interpolation.
    fn format_failure_message(
        &self,
        world: &World,
        precondition: &Precondition,
        call: &PreconditionCall,
        context: &ActionContext,
    ) -> String {
        let template = precondition.failure_template();

        // Simple interpolation: replace ~(name target) with entity name
        // For now, just return the template as-is or do basic replacement
        let mut message = template.to_string();

        // Replace ~(name ...) patterns
        // This is a simplified implementation
        for (i, param) in precondition.params.iter().enumerate() {
            let placeholder = format!("~(name {})", param.as_str());
            if message.contains(&placeholder) {
                if let Some(entity) = self
                    .resolve_arg(world, call.args.get(i), context)
                    .ok()
                    .flatten()
                {
                    let name = entity_name(world, entity);
                    message = message.replace(&placeholder, &name);
                }
            }

            // Also try ~(Name ...) for capitalized
            let cap_placeholder = format!("~(Name {})", param.as_str());
            if message.contains(&cap_placeholder) {
                if let Some(entity) = self
                    .resolve_arg(world, call.args.get(i), context)
                    .ok()
                    .flatten()
                {
                    let name = entity_name(world, entity);
                    let capitalized = capitalize_first(&name);
                    message = message.replace(&cap_placeholder, &capitalized);
                }
            }
        }

        message
    }
}

// ============================================================================
// Helper functions for scope checking
// ============================================================================

/// Get the name of an entity.
fn entity_name(world: &World, entity: EntityId) -> String {
    world
        .get_component(entity, "Name")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "something".to_string())
}

/// Capitalize the first character of a string.
fn capitalize_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(c) => c.to_uppercase().chain(chars).collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema};

    fn setup_world() -> (World, EntityId, EntityId, EntityId) {
        let mut world = World::new();

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
        world.set_component(room, "IsRoom", Value::Bool(true));
        world.set_component(room, "Name", Value::string("Test Room"));

        // Create player
        let player = world.create_entity();
        world.set_component(player, "Name", Value::string("player"));
        world.add_relation("InRoom", player, room);

        // Create object
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", Value::string("lamp"));
        world.set_component(lamp, "Portable", Value::Bool(true));
        world.add_relation("InRoom", lamp, room);

        (world, player, room, lamp)
    }

    #[test]
    fn test_is_in_scope_same_room() {
        let (world, player, _, lamp) = setup_world();
        assert!(world.is_in_scope(player, lamp));
    }

    #[test]
    fn test_is_in_scope_carried() {
        let (mut world, player, _, lamp) = setup_world();

        // Pick up the lamp
        world.remove_relation(
            "InRoom",
            lamp,
            world.query_relation_forward("InRoom", lamp)[0],
        );
        world.add_relation("Contains", player, lamp);

        assert!(world.is_in_scope(player, lamp));
    }

    #[test]
    fn test_is_in_scope_different_room() {
        let (mut world, player, _, lamp) = setup_world();

        // Create another room and move lamp there
        let room2 = world.create_entity();
        world.set_component(room2, "IsRoom", Value::Bool(true));
        world.remove_relation(
            "InRoom",
            lamp,
            world.query_relation_forward("InRoom", lamp)[0],
        );
        world.add_relation("InRoom", lamp, room2);

        assert!(!world.is_in_scope(player, lamp));
    }

    #[test]
    fn test_is_held_by() {
        let (mut world, player, _, lamp) = setup_world();

        // Not held initially
        assert!(!world.is_held_by(lamp, player));

        // Pick up
        world.add_relation("Contains", player, lamp);
        assert!(world.is_held_by(lamp, player));
    }

    #[test]
    fn test_is_portable() {
        let (mut world, _, _, lamp) = setup_world();

        // Lamp is portable
        assert!(world.is_portable(lamp));

        // Create fixed object
        let statue = world.create_entity();
        world.set_component(statue, "Fixed", Value::Bool(true));
        assert!(!world.is_portable(statue));
    }

    #[test]
    fn test_builtin_reachable() {
        let (world, player, _, lamp) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        let context = ActionContext::new(player).with_direct_object(lamp);

        let call = PreconditionCall::new(
            Symbol::new("reachable?"),
            vec![PreconditionArg::Actor, PreconditionArg::DirectObject],
        );

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.passed());
    }

    #[test]
    fn test_builtin_reachable_fails() {
        let (mut world, player, _, lamp) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        // Move lamp to another room
        let room2 = world.create_entity();
        world.set_component(room2, "IsRoom", Value::Bool(true));
        world.remove_relation(
            "InRoom",
            lamp,
            world.query_relation_forward("InRoom", lamp)[0],
        );
        world.add_relation("InRoom", lamp, room2);

        let context = ActionContext::new(player).with_direct_object(lamp);

        let call = PreconditionCall::new(
            Symbol::new("reachable?"),
            vec![PreconditionArg::Actor, PreconditionArg::DirectObject],
        );

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.failed());
        assert!(result.message().unwrap().contains("can't reach"));
    }

    #[test]
    fn test_builtin_held() {
        let (mut world, player, _, lamp) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        // Pick up lamp
        world.add_relation("Contains", player, lamp);

        let context = ActionContext::new(player).with_direct_object(lamp);

        let call = PreconditionCall::new(Symbol::new("held?"), vec![PreconditionArg::DirectObject]);

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.passed());
    }

    #[test]
    fn test_builtin_not_held() {
        let (world, player, _, lamp) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        // Lamp is not held
        let context = ActionContext::new(player).with_direct_object(lamp);

        let call = PreconditionCall::new(
            Symbol::new("not-held?"),
            vec![PreconditionArg::DirectObject],
        );

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.passed());
    }

    #[test]
    fn test_builtin_portable() {
        let (world, player, _, lamp) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        let context = ActionContext::new(player).with_direct_object(lamp);

        let call = PreconditionCall::new(
            Symbol::new("portable?"),
            vec![PreconditionArg::DirectObject],
        );

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.passed());
    }

    #[test]
    fn test_builtin_portable_fails() {
        let (mut world, player, room, _) = setup_world();
        let registry = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        // Create fixed object
        let statue = world.create_entity();
        world.set_component(statue, "Name", Value::string("statue"));
        world.set_component(statue, "Fixed", Value::Bool(true));
        world.add_relation("InRoom", statue, room);

        let context = ActionContext::new(player).with_direct_object(statue);

        let call = PreconditionCall::new(
            Symbol::new("portable?"),
            vec![PreconditionArg::DirectObject],
        );

        let result = registry.check(&world, &call, &context, &stdlib).unwrap();
        assert!(result.failed());
        assert!(result.message().unwrap().contains("fixed in place"));
    }

    #[test]
    fn test_precondition_registry_register() {
        use crate::lang::{Atom, SExpr, Span};

        let mut registry = PreconditionRegistry::new();

        let check = SExpr::Atom(Atom::Bool(true), Span::new(0, 0, 0, 0));
        let precondition = Precondition::new(
            Symbol::new("test?"),
            vec![Symbol::new("obj")],
            check,
            "Test failed.",
        );

        registry.register(precondition);

        assert!(registry.contains(Symbol::new("test?")));
        assert!(!registry.contains(Symbol::new("unknown?")));
    }

    #[test]
    fn test_entity_name() {
        let (world, _, _, lamp) = setup_world();
        assert_eq!(entity_name(&world, lamp), "lamp");
    }

    #[test]
    fn test_capitalize_first() {
        assert_eq!(capitalize_first("lamp"), "Lamp");
        assert_eq!(capitalize_first(""), "");
        assert_eq!(capitalize_first("a"), "A");
    }
}
