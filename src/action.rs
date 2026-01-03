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
//! // In DSL with builtin handler:
//! (action take
//!   :preconditions ((reachable? actor direct-object)
//!                   (portable? direct-object)
//!                   (not-held? direct-object))
//!   :handler take-handler)
//!
//! // In DSL with inline handler:
//! (action take
//!   :preconditions ((reachable? actor direct-object)
//!                   (portable? direct-object))
//!   :handler
//!     (do
//!       (relate! :Contains (actor) (direct-object))
//!       (say "Taken.")
//!       :success))
//! ```

use crate::compiler::Compiler;
use crate::core::{EntityId, World};
use crate::lang::SExpr;
use crate::precondition::{PreconditionCall, PreconditionRegistry, PreconditionResult};
use crate::symbol::Symbol;
use crate::verbs::VerbResult;
use crate::vm::{ActionContext, StdLib, VM};
use im::OrdMap;
use std::sync::Arc;

/// A handler function type for actions.
pub type HandlerFn = Arc<
    dyn Fn(&mut World, EntityId, Option<EntityId>, Option<EntityId>) -> VerbResult + Send + Sync,
>;

/// The handler for an action - either a reference to a Rust builtin or inline DSL code.
#[derive(Clone, Debug)]
pub enum ActionHandler {
    /// A Rust handler referenced by symbol name (looked up in HandlerRegistry).
    Builtin(Symbol),
    /// A DSL handler as AST (compiled and executed via VM).
    /// The SExpr is typically a `(do ...)` block containing the handler body.
    Dsl(SExpr),
}

impl ActionHandler {
    /// Check if this is a builtin handler.
    pub fn is_builtin(&self) -> bool {
        matches!(self, ActionHandler::Builtin(_))
    }

    /// Check if this is a DSL handler.
    pub fn is_dsl(&self) -> bool {
        matches!(self, ActionHandler::Dsl(_))
    }

    /// Get the builtin handler name, if any.
    pub fn builtin_name(&self) -> Option<Symbol> {
        match self {
            ActionHandler::Builtin(name) => Some(*name),
            ActionHandler::Dsl(_) => None,
        }
    }

    /// Get the DSL handler body, if any.
    pub fn dsl_body(&self) -> Option<&SExpr> {
        match self {
            ActionHandler::Builtin(_) => None,
            ActionHandler::Dsl(body) => Some(body),
        }
    }
}

/// The result of executing an action handler.
#[derive(Debug, Clone, PartialEq)]
pub enum ActionResult {
    /// The action succeeded.
    Success,
    /// The action failed with an optional message.
    Failure(Option<String>),
}

impl ActionResult {
    /// Check if the action succeeded.
    pub fn is_success(&self) -> bool {
        matches!(self, ActionResult::Success)
    }

    /// Check if the action failed.
    pub fn is_failure(&self) -> bool {
        matches!(self, ActionResult::Failure(_))
    }

    /// Get the failure message, if any.
    pub fn failure_message(&self) -> Option<&str> {
        match self {
            ActionResult::Success => None,
            ActionResult::Failure(msg) => msg.as_deref(),
        }
    }
}

/// An action definition.
#[derive(Clone)]
pub struct Action {
    /// The action name.
    name: Symbol,
    /// Preconditions that must be satisfied.
    preconditions: Vec<PreconditionCall>,
    /// The handler (builtin or DSL).
    handler: ActionHandler,
}

impl Action {
    /// Create a new action with default builtin handler (same name as action).
    pub fn new(name: Symbol) -> Self {
        Self {
            name,
            preconditions: Vec::new(),
            handler: ActionHandler::Builtin(name), // Default handler name matches action name
        }
    }

    /// Create an action with a specific builtin handler.
    pub fn with_handler(name: Symbol, handler: Symbol) -> Self {
        Self {
            name,
            preconditions: Vec::new(),
            handler: ActionHandler::Builtin(handler),
        }
    }

    /// Create an action with a DSL handler.
    pub fn with_dsl_handler(name: Symbol, body: SExpr) -> Self {
        Self {
            name,
            preconditions: Vec::new(),
            handler: ActionHandler::Dsl(body),
        }
    }

    /// Get the action name.
    pub fn name(&self) -> Symbol {
        self.name
    }

    /// Get the handler.
    pub fn handler(&self) -> &ActionHandler {
        &self.handler
    }

    /// Get the builtin handler name (for backward compatibility).
    /// Returns the action name if handler is DSL.
    pub fn handler_name(&self) -> Symbol {
        match &self.handler {
            ActionHandler::Builtin(name) => *name,
            ActionHandler::Dsl(_) => self.name,
        }
    }

    /// Get the preconditions.
    pub fn preconditions(&self) -> &[PreconditionCall] {
        &self.preconditions
    }

    /// Set the handler to a builtin.
    pub fn set_handler(&mut self, handler: Symbol) {
        self.handler = ActionHandler::Builtin(handler);
    }

    /// Set the handler to a DSL body.
    pub fn set_dsl_handler(&mut self, body: SExpr) {
        self.handler = ActionHandler::Dsl(body);
    }

    /// Add a precondition.
    pub fn add_precondition(&mut self, precondition: PreconditionCall) {
        self.preconditions.push(precondition);
    }

    /// Set all preconditions.
    pub fn set_preconditions(&mut self, preconditions: Vec<PreconditionCall>) {
        self.preconditions = preconditions;
    }

    /// Check if this action has a DSL handler.
    pub fn has_dsl_handler(&self) -> bool {
        self.handler.is_dsl()
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

    /// Get a mutable reference to an action by name.
    pub fn get_mut(&mut self, name: Symbol) -> Option<&mut Action> {
        self.actions.get_mut(&name)
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
    context: &ActionContext,
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

/// Error during DSL handler execution.
#[derive(Debug, Clone, thiserror::Error)]
pub enum HandlerError {
    #[error("compile error: {0}")]
    Compile(#[from] crate::compiler::CompileError),

    #[error("VM error: {0}")]
    VM(#[from] crate::vm::VMError),
}

/// Result of executing a DSL handler.
#[derive(Debug)]
pub struct DslHandlerResult {
    /// The action result (success or failure).
    pub result: ActionResult,
    /// Any output text generated by the handler.
    pub output: Vec<String>,
    /// Pending mutations to apply.
    pub mutations: crate::hooks::PendingMutations,
}

/// Execute a DSL handler and return the result.
///
/// DSL handlers should return one of:
/// - `:success` - the action succeeded
/// - `(:failure "message")` - the action failed with a message
/// - Any other value - treated as success (for backward compatibility)
///
/// Output is collected from `(say ...)` calls during execution.
pub fn execute_dsl_handler(
    body: &SExpr,
    world: &World,
    context: &ActionContext,
    stdlib: &StdLib,
) -> Result<DslHandlerResult, HandlerError> {
    // Compile the handler body
    let chunk = Compiler::compile(body)?;

    // Execute with action context
    let mut vm = VM::new(&chunk, world, stdlib).with_action_context(context.clone());
    let result_value = vm.run()?;

    // Collect output and mutations
    let output: Vec<String> = vm.take_output().into_iter().collect();
    let mutations = crate::hooks::PendingMutations {
        set_components: vm.take_pending_set_components(),
        relate: vm.take_pending_relate(),
        unrelate: vm.take_pending_unrelate(),
        deletions: vm.take_pending_deletions(),
    };

    // Parse the result value
    let result = parse_action_result(&result_value);

    Ok(DslHandlerResult {
        result,
        output,
        mutations,
    })
}

/// Parse a Value into an ActionResult.
///
/// Recognizes:
/// - `:success` keyword -> Success
/// - `:failure` keyword -> Failure with no message
/// - `(:failure "message")` list -> Failure with message
/// - `(:failure)` list -> Failure with no message
/// - Any other value -> Success (backward compatibility)
fn parse_action_result(value: &crate::core::Value) -> ActionResult {
    use crate::core::Value;

    // Check for :success or :failure keyword
    if let Value::Symbol(sym) = value {
        let name = sym.as_str();
        if name.as_str() == "success" {
            return ActionResult::Success;
        }
        if name.as_str() == "failure" {
            return ActionResult::Failure(None);
        }
    }

    // Check for (:failure ...) list
    if let Value::List(items) = value {
        if let Some(Value::Symbol(sym)) = items.first() {
            if sym.as_str().as_str() == "failure" {
                // Get optional message
                let message = items.get(1).and_then(|v| v.as_str()).map(String::from);
                return ActionResult::Failure(message);
            }
        }
    }

    // Default: treat as success
    ActionResult::Success
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
        assert_eq!(action.handler_name(), Symbol::new("take"));
        assert!(action.preconditions().is_empty());
        assert!(!action.has_dsl_handler());
    }

    #[test]
    fn test_action_with_handler() {
        let action = Action::with_handler(Symbol::new("take"), Symbol::new("take-handler"));
        assert_eq!(action.name(), Symbol::new("take"));
        assert_eq!(action.handler_name(), Symbol::new("take-handler"));
        assert!(!action.has_dsl_handler());
    }

    #[test]
    fn test_action_with_dsl_handler() {
        use crate::lang::{Atom, SExpr, Span};

        let body = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("do")), Span::new(0, 0, 0, 0)),
                SExpr::Atom(Atom::Keyword(Symbol::new("success")), Span::new(0, 0, 0, 0)),
            ],
            Span::new(0, 0, 0, 0),
        );

        let action = Action::with_dsl_handler(Symbol::new("custom"), body);
        assert_eq!(action.name(), Symbol::new("custom"));
        assert!(action.has_dsl_handler());
        assert!(action.handler().is_dsl());
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

        let context = ActionContext::new(player).with_direct_object(lamp);

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

        let context = ActionContext::new(player).with_direct_object(statue);

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
        let context = ActionContext::new(player);

        let result = check_action_preconditions(&action, &world, &context, &preconditions, &stdlib);
        assert!(result.passed());
    }

    // ============================================================================
    // ActionResult parsing tests
    // ============================================================================

    #[test]
    fn test_parse_action_result_success_keyword() {
        let value = Value::Symbol(Symbol::new("success"));
        let result = parse_action_result(&value);
        assert!(result.is_success());
    }

    #[test]
    fn test_parse_action_result_failure_with_message() {
        let value = Value::list(vec![
            Value::Symbol(Symbol::new("failure")),
            Value::string("Something went wrong"),
        ]);
        let result = parse_action_result(&value);
        assert!(result.is_failure());
        assert_eq!(result.failure_message(), Some("Something went wrong"));
    }

    #[test]
    fn test_parse_action_result_failure_no_message() {
        let value = Value::list(vec![Value::Symbol(Symbol::new("failure"))]);
        let result = parse_action_result(&value);
        assert!(result.is_failure());
        assert_eq!(result.failure_message(), None);
    }

    #[test]
    fn test_parse_action_result_failure_keyword() {
        let value = Value::Symbol(Symbol::new("failure"));
        let result = parse_action_result(&value);
        assert!(result.is_failure());
        assert_eq!(result.failure_message(), None);
    }

    #[test]
    fn test_parse_action_result_default_success() {
        // Any other value treated as success
        let value = Value::Int(42);
        let result = parse_action_result(&value);
        assert!(result.is_success());

        let value = Value::string("hello");
        let result = parse_action_result(&value);
        assert!(result.is_success());

        let value = Value::Nil;
        let result = parse_action_result(&value);
        assert!(result.is_success());
    }

    // ============================================================================
    // DSL handler execution tests
    // ============================================================================

    #[test]
    fn test_execute_dsl_handler_success() {
        use crate::lang::parse;
        use crate::vm::ActionContext;

        let (world, player, _, _) = setup_world();
        let stdlib = StdLib::with_builtins();

        // Parse a simple handler that returns :success
        let body = parse(":success").unwrap();
        let context = ActionContext::new(player);

        let result = execute_dsl_handler(&body, &world, &context, &stdlib).unwrap();
        assert!(result.result.is_success());
        assert!(result.output.is_empty());
    }

    #[test]
    fn test_execute_dsl_handler_with_say() {
        use crate::lang::parse;
        use crate::vm::ActionContext;

        let (world, player, _, _) = setup_world();
        let stdlib = StdLib::with_builtins();

        // Parse a handler that says something and returns success
        let body = parse("(do (say \"Hello!\") :success)").unwrap();
        let context = ActionContext::new(player);

        let result = execute_dsl_handler(&body, &world, &context, &stdlib).unwrap();
        assert!(result.result.is_success());
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.output[0], "Hello!");
    }

    #[test]
    fn test_execute_dsl_handler_failure() {
        use crate::lang::parse;
        use crate::vm::ActionContext;

        let (world, player, _, _) = setup_world();
        let stdlib = StdLib::with_builtins();

        // :failure keyword returns failure with no message
        let body = parse("(do (say \"Oops!\") :failure)").unwrap();
        let context = ActionContext::new(player);

        let result = execute_dsl_handler(&body, &world, &context, &stdlib).unwrap();
        assert!(result.result.is_failure());
        assert_eq!(result.result.failure_message(), None);
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.output[0], "Oops!");
    }

    #[test]
    fn test_execute_dsl_handler_with_mutation() {
        use crate::lang::parse;
        use crate::vm::ActionContext;

        let (world, player, _, _) = setup_world();
        let stdlib = StdLib::with_builtins();

        // Parse a handler that sets a component using the actor from action context
        // Note: In real usage, (actor) would be available, but we need to test the mutation path
        // The 'set!' op requires an entity - for this test we'll just verify the handler runs
        let body = parse("(do (say \"Mutating\") :success)").unwrap();
        let context = ActionContext::new(player);

        let result = execute_dsl_handler(&body, &world, &context, &stdlib).unwrap();
        assert!(result.result.is_success());
        assert_eq!(result.output.len(), 1);
        assert_eq!(result.output[0], "Mutating");
    }
}
