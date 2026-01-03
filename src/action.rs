//! Action system for semantic verb handling.
//!
//! Actions represent attempts to do something in the game world. They are the semantic
//! layer above commands (which are the syntactic/input layer).
//!
//! An action has:
//! - A name (e.g., "take", "examine", "unlock")
//! - A list of preconditions that must be satisfied
//! - A handler that executes the action
//!
//! # Example
//!
//! ```ignore
//! // In DSL:
//! (action take
//!   :preconditions [(reachable? actor direct-object)
//!                   (visible? actor direct-object)
//!                   (portable? direct-object)
//!                   (not-held? direct-object)]
//!   :handler take-handler)
//! ```

use crate::core::{EntityId, World};
use crate::precondition::{PreconditionCall, PreconditionRegistry, PreconditionResult};
use crate::symbol::Symbol;
use crate::verbs::VerbResult;
use crate::vm::{HookContext, StdLib};
use im::OrdMap;
use std::sync::Arc;

/// A handler function type for actions.
pub type HandlerFn = Arc<
    dyn Fn(&mut World, EntityId, Option<EntityId>, Option<EntityId>) -> VerbResult + Send + Sync,
>;

/// An action definition.
#[derive(Clone)]
pub struct Action {
    /// The action name.
    name: Symbol,
    /// Preconditions that must be satisfied.
    preconditions: Vec<PreconditionCall>,
    /// The handler name (for lookup in handler registry).
    handler: Symbol,
}

impl Action {
    /// Create a new action.
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            preconditions: Vec::new(),
            handler: name, // Default handler name matches action name
        }
    }

    /// Create an action with a specific handler.
    pub fn with_handler(name: Symbol, handler: Symbol) -> Self {
        Self {
            name,
            preconditions: Vec::new(),
            handler,
        }
    }

    /// Get the action name.
    pub fn name(&self) -> Symbol {
        self.name
    }

    /// Get the handler name.
    pub fn handler(&self) -> Symbol {
        self.handler
    }

    /// Get the preconditions.
    pub fn preconditions(&self) -> &[PreconditionCall] {
        &self.preconditions
    }

    /// Set the handler.
    pub fn set_handler(&mut self, handler: Symbol) {
        self.handler = handler;
    }

    /// Add a precondition.
    pub fn add_precondition(&mut self, precondition: PreconditionCall) {
        self.preconditions.push(precondition);
    }

    /// Set all preconditions.
    pub fn set_preconditions(&mut self, preconditions: Vec<PreconditionCall>) {
        self.preconditions = preconditions;
    }
}

impl std::fmt::Debug for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Action")
            .field("name", &self.name)
            .field("preconditions", &self.preconditions)
            .field("handler", &self.handler)
            .finish()
    }
}

/// Registry of action definitions.
#[derive(Clone, Default)]
pub struct ActionRegistry {
    actions: OrdMap<Symbol, Action>,
}

impl ActionRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            actions: OrdMap::new(),
        }
    }

    /// Register an action.
    pub fn register(&mut self, action: Action) {
        self.actions.insert(action.name, action);
    }

    /// Get an action by name.
    pub fn get(&self, name: Symbol) -> Option<&Action> {
        self.actions.get(&name)
    }

    /// Check if an action exists.
    pub fn contains(&self, name: Symbol) -> bool {
        self.actions.contains_key(&name)
    }

    /// Get all action names.
    pub fn names(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.actions.keys().copied()
    }

    /// Get number of registered actions.
    pub fn len(&self) -> usize {
        self.actions.len()
    }

    /// Check if registry is empty.
    pub fn is_empty(&self) -> bool {
        self.actions.is_empty()
    }
}

impl std::fmt::Debug for ActionRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActionRegistry")
            .field("actions", &self.actions.keys().collect::<Vec<_>>())
            .finish()
    }
}

/// Registry of action handlers (Rust functions).
#[derive(Clone, Default)]
pub struct HandlerRegistry {
    handlers: OrdMap<Symbol, HandlerFn>,
}

impl HandlerRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self {
            handlers: OrdMap::new(),
        }
    }

    /// Create a registry with built-in handlers.
    pub fn with_builtins() -> Self {
        let mut registry = Self::new();
        registry.register_builtins();
        registry
    }

    /// Register the built-in handlers.
    fn register_builtins(&mut self) {
        use crate::verbs::{
            handle_drop, handle_examine, handle_inventory, handle_look, handle_take,
        };

        // Look
        self.register(
            "look",
            Arc::new(|world, actor, _, _| handle_look(world, actor)),
        );
        self.register(
            "look-around",
            Arc::new(|world, actor, _, _| handle_look(world, actor)),
        );

        // Inventory
        self.register(
            "inventory",
            Arc::new(|world, actor, _, _| handle_inventory(world, actor)),
        );

        // Examine
        self.register(
            "examine",
            Arc::new(|world, _, direct, _| {
                if let Some(target) = direct {
                    handle_examine(world, target)
                } else {
                    VerbResult::fail("Examine what?")
                }
            }),
        );

        // Take
        self.register(
            "take",
            Arc::new(|world, actor, direct, _| {
                if let Some(target) = direct {
                    handle_take(world, actor, target)
                } else {
                    VerbResult::fail("Take what?")
                }
            }),
        );
        self.register(
            "take-handler",
            Arc::new(|world, actor, direct, _| {
                if let Some(target) = direct {
                    handle_take(world, actor, target)
                } else {
                    VerbResult::fail("Take what?")
                }
            }),
        );

        // Drop
        self.register(
            "drop",
            Arc::new(|world, actor, direct, _| {
                if let Some(target) = direct {
                    handle_drop(world, actor, target)
                } else {
                    VerbResult::fail("Drop what?")
                }
            }),
        );

        // Go
        self.register(
            "go",
            Arc::new(|_world, _actor, _, _| {
                // Go requires direction, which isn't an entity
                // This handler is a fallback; direction-based go is handled specially
                VerbResult::fail("Go where?")
            }),
        );
    }

    /// Register a handler.
    pub fn register(&mut self, name: impl AsRef<str>, handler: HandlerFn) {
        self.handlers.insert(Symbol::new(name.as_ref()), handler);
    }

    /// Get a handler by name.
    pub fn get(&self, name: Symbol) -> Option<&HandlerFn> {
        self.handlers.get(&name)
    }

    /// Check if a handler exists.
    pub fn contains(&self, name: Symbol) -> bool {
        self.handlers.contains_key(&name)
    }

    /// Execute a handler.
    pub fn execute(
        &self,
        name: Symbol,
        world: &mut World,
        actor: EntityId,
        direct_object: Option<EntityId>,
        indirect_object: Option<EntityId>,
    ) -> Option<VerbResult> {
        self.handlers
            .get(&name)
            .map(|handler| handler(world, actor, direct_object, indirect_object))
    }
}

impl std::fmt::Debug for HandlerRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HandlerRegistry")
            .field("handlers", &self.handlers.keys().collect::<Vec<_>>())
            .finish()
    }
}

/// Result of checking all preconditions for an action.
#[derive(Debug, Clone)]
pub enum ActionCheckResult {
    /// All preconditions passed.
    Passed,
    /// A precondition failed with a message.
    Failed(String),
}

impl ActionCheckResult {
    /// Check if all preconditions passed.
    pub fn passed(&self) -> bool {
        matches!(self, ActionCheckResult::Passed)
    }

    /// Check if a precondition failed.
    pub fn failed(&self) -> bool {
        matches!(self, ActionCheckResult::Failed(_))
    }

    /// Get the failure message, if any.
    pub fn message(&self) -> Option<&str> {
        match self {
            ActionCheckResult::Passed => None,
            ActionCheckResult::Failed(msg) => Some(msg),
        }
    }
}

/// Check all preconditions for an action.
pub fn check_action_preconditions(
    action: &Action,
    world: &World,
    context: &HookContext,
    preconditions: &PreconditionRegistry,
    stdlib: &StdLib,
) -> ActionCheckResult {
    for call in action.preconditions() {
        match preconditions.check(world, call, context, stdlib) {
            Ok(PreconditionResult::Passed) => continue,
            Ok(PreconditionResult::Failed(msg)) => return ActionCheckResult::Failed(msg),
            Err(e) => {
                // Treat errors as failures
                return ActionCheckResult::Failed(format!("Precondition error: {e}"));
            }
        }
    }

    ActionCheckResult::Passed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::{Cardinality, RelationSchema, Value};
    use crate::precondition::PreconditionArg;

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
    fn test_action_creation() {
        let action = Action::new(Symbol::new("take"));
        assert_eq!(action.name(), Symbol::new("take"));
        assert_eq!(action.handler(), Symbol::new("take"));
        assert!(action.preconditions().is_empty());
    }

    #[test]
    fn test_action_with_handler() {
        let action = Action::with_handler(Symbol::new("take"), Symbol::new("take-handler"));
        assert_eq!(action.name(), Symbol::new("take"));
        assert_eq!(action.handler(), Symbol::new("take-handler"));
    }

    #[test]
    fn test_action_with_preconditions() {
        let mut action = Action::new(Symbol::new("take"));

        action.add_precondition(PreconditionCall::new(
            Symbol::new("reachable?"),
            vec![PreconditionArg::Actor, PreconditionArg::DirectObject],
        ));
        action.add_precondition(PreconditionCall::new(
            Symbol::new("portable?"),
            vec![PreconditionArg::DirectObject],
        ));

        assert_eq!(action.preconditions().len(), 2);
    }

    #[test]
    fn test_action_registry() {
        let mut registry = ActionRegistry::new();

        let action = Action::new(Symbol::new("take"));
        registry.register(action);

        assert!(registry.contains(Symbol::new("take")));
        assert!(!registry.contains(Symbol::new("unknown")));
        assert_eq!(registry.len(), 1);
    }

    #[test]
    fn test_handler_registry() {
        let mut registry = HandlerRegistry::new();

        registry.register("test", Arc::new(|_, _, _, _| VerbResult::success("Test!")));

        assert!(registry.contains(Symbol::new("test")));
        assert!(!registry.contains(Symbol::new("unknown")));
    }

    #[test]
    fn test_handler_execute() {
        let (mut world, player, _, _) = setup_world();
        let mut registry = HandlerRegistry::new();

        registry.register(
            "test",
            Arc::new(|_, _, _, _| VerbResult::success("Test executed!")),
        );

        let result = registry
            .execute(Symbol::new("test"), &mut world, player, None, None)
            .unwrap();
        assert!(result.success);
        assert!(result.output.contains("Test executed!"));
    }

    #[test]
    fn test_builtin_handlers() {
        let registry = HandlerRegistry::with_builtins();

        assert!(registry.contains(Symbol::new("look")));
        assert!(registry.contains(Symbol::new("inventory")));
        assert!(registry.contains(Symbol::new("examine")));
        assert!(registry.contains(Symbol::new("take")));
        assert!(registry.contains(Symbol::new("drop")));
    }

    #[test]
    fn test_check_action_preconditions_pass() {
        let (world, player, _, lamp) = setup_world();
        let preconditions = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        let mut action = Action::new(Symbol::new("take"));
        action.add_precondition(PreconditionCall::new(
            Symbol::new("reachable?"),
            vec![PreconditionArg::Actor, PreconditionArg::DirectObject],
        ));
        action.add_precondition(PreconditionCall::new(
            Symbol::new("portable?"),
            vec![PreconditionArg::DirectObject],
        ));
        action.add_precondition(PreconditionCall::new(
            Symbol::new("not-held?"),
            vec![PreconditionArg::DirectObject],
        ));

        let context = HookContext::new(player).with_direct_object(lamp);

        let result = check_action_preconditions(&action, &world, &context, &preconditions, &stdlib);
        assert!(result.passed());
    }

    #[test]
    fn test_check_action_preconditions_fail() {
        let (mut world, player, room, _) = setup_world();
        let preconditions = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        // Create a fixed object
        let statue = world.create_entity();
        world.set_component(statue, "Name", Value::string("statue"));
        world.set_component(statue, "Fixed", Value::Bool(true));
        world.add_relation("InRoom", statue, room);

        let mut action = Action::new(Symbol::new("take"));
        action.add_precondition(PreconditionCall::new(
            Symbol::new("portable?"),
            vec![PreconditionArg::DirectObject],
        ));

        let context = HookContext::new(player).with_direct_object(statue);

        let result = check_action_preconditions(&action, &world, &context, &preconditions, &stdlib);
        assert!(result.failed());
        assert!(result.message().unwrap().contains("fixed in place"));
    }

    #[test]
    fn test_check_action_no_preconditions() {
        let (world, player, _, _) = setup_world();
        let preconditions = PreconditionRegistry::with_builtins();
        let stdlib = StdLib::with_builtins();

        let action = Action::new(Symbol::new("look"));
        let context = HookContext::new(player);

        let result = check_action_preconditions(&action, &world, &context, &preconditions, &stdlib);
        assert!(result.passed());
    }
}
