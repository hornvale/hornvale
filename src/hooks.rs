//! Hook execution engine for object-specific verb responses.
//!
//! Hooks allow entities to respond to actions being performed on them.
//! There are three phases:
//! - **Before**: Runs before the action, can veto it
//! - **On**: Runs during the action, can handle it (skip default)
//! - **After**: Runs after the action completes
//!
//! # Example
//!
//! ```ignore
//! // In DSL:
//! (entity holy-book
//!   (Name "book")
//!   (On:burn
//!     (say "As the flames touch the sacred text, lightning strikes!")
//!     :veto))
//! ```

use crate::compiler::{CompileError, Compiler};
use crate::core::{EntityId, Value, World};
use crate::symbol::Symbol;
use crate::vm::{HookContext, StdLib, VM, VMError};

/// Result of executing a hook.
#[derive(Debug, Clone)]
pub enum HookResult {
    /// Continue with the action normally.
    Continue,
    /// Skip the default handler, use this output instead.
    Handled(String),
    /// Cancel the action entirely with this message.
    Veto(String),
}

impl HookResult {
    /// Check if this result is a veto.
    pub fn is_veto(&self) -> bool {
        matches!(self, HookResult::Veto(_))
    }

    /// Check if this result is handled.
    pub fn is_handled(&self) -> bool {
        matches!(self, HookResult::Handled(_))
    }

    /// Get the output message, if any.
    pub fn message(&self) -> Option<&str> {
        match self {
            HookResult::Continue => None,
            HookResult::Handled(msg) | HookResult::Veto(msg) => Some(msg),
        }
    }
}

/// Error during hook execution.
#[derive(Debug, Clone, thiserror::Error)]
pub enum HookError {
    #[error("compile error: {0}")]
    Compile(#[from] CompileError),

    #[error("VM error: {0}")]
    VM(#[from] VMError),

    #[error("invalid hook value: expected list, got {0}")]
    InvalidHookValue(String),
}

/// Result of running all hooks for an action.
#[derive(Debug, Clone)]
pub struct HookPipelineResult {
    /// Whether a Before hook vetoed the action.
    pub vetoed: bool,
    /// Whether an On hook handled the action.
    pub handled: bool,
    /// Combined output from all hooks.
    pub output: Vec<String>,
    /// Entities marked for deletion.
    pub deletions: Vec<EntityId>,
}

impl HookPipelineResult {
    /// Create a new empty result.
    pub fn new() -> Self {
        Self {
            vetoed: false,
            handled: false,
            output: Vec::new(),
            deletions: Vec::new(),
        }
    }

    /// Get combined output as a single string.
    pub fn combined_output(&self) -> String {
        self.output.join("\n")
    }
}

impl Default for HookPipelineResult {
    fn default() -> Self {
        Self::new()
    }
}

/// Execute a single hook expression.
///
/// The hook value should be a `Value::List` containing S-expression(s) to evaluate.
/// The last expression in the list determines the hook result:
/// - `:handled` symbol → HookResult::Handled
/// - `:veto` symbol → HookResult::Veto
/// - Anything else → HookResult::Continue
pub fn execute_hook(
    world: &World,
    hook_expr: &Value,
    context: &HookContext,
    stdlib: &StdLib,
) -> Result<(HookResult, Vec<String>, Vec<EntityId>), HookError> {
    // Hook body is stored as a list of expressions.
    // We need to wrap it in a (do ...) form to execute them sequentially.
    let wrapped_hook = match hook_expr {
        Value::List(items) => {
            // Prepend "do" symbol to make it (do expr1 expr2 ...)
            let mut do_form = vec![Value::Symbol(Symbol::new("do"))];
            do_form.extend(items.iter().cloned());
            Value::list(do_form)
        }
        other => other.clone(), // Single expression, use as-is
    };

    // Convert Value to S-expression for compilation
    let sexpr = value_to_sexpr(&wrapped_hook)?;

    // Compile the expression
    let chunk = Compiler::compile(&sexpr)?;

    // Execute with hook context
    let mut vm = VM::new(&chunk, world, stdlib).with_hook_context(context.clone());

    let result = vm.run()?;

    // Collect outputs and deletions
    let output = vm.take_output();
    let deletions = vm.take_pending_deletions();

    // Determine hook result based on return value
    let hook_result = match &result {
        Value::Symbol(s) if s.as_str() == "handled" => HookResult::Handled(output.join("\n")),
        Value::Symbol(s) if s.as_str() == "veto" => HookResult::Veto(output.join("\n")),
        _ => HookResult::Continue,
    };

    Ok((hook_result, output, deletions))
}

/// Convert a Value::List back to an S-expression for compilation.
///
/// Hook expressions are stored as Value::List (S-expr structure).
/// We need to reconstruct them as SExpr AST nodes for the compiler.
fn value_to_sexpr(value: &Value) -> Result<crate::lang::SExpr, HookError> {
    use crate::lang::{Atom, SExpr, Span};

    let span = Span::new(0, 0, 0, 0); // Dummy span for generated code

    match value {
        Value::List(items) => {
            if items.is_empty() {
                // Empty list - return nil
                Ok(SExpr::Atom(Atom::Symbol(Symbol::new("nil")), span))
            } else {
                // Convert each item recursively
                let exprs: Result<Vec<_>, _> = items.iter().map(value_to_sexpr).collect();
                Ok(SExpr::List(exprs?, span))
            }
        }
        Value::Int(n) => Ok(SExpr::Atom(Atom::Int(*n), span)),
        Value::Float(f) => Ok(SExpr::Atom(Atom::Float(*f), span)),
        Value::Bool(b) => Ok(SExpr::Atom(Atom::Bool(*b), span)),
        Value::String(s) => Ok(SExpr::Atom(Atom::String(s.to_string()), span)),
        Value::Symbol(s) => Ok(SExpr::Atom(Atom::Symbol(*s), span)),
        Value::EntityRef(e) => {
            // Entity refs can't be directly represented in S-expressions
            // Use a placeholder that will error at runtime if evaluated
            Ok(SExpr::Atom(
                Atom::Symbol(Symbol::new(&format!("__entity_{}", e.raw()))),
                span,
            ))
        }
    }
}

/// Run all hooks for an action on the relevant entities.
///
/// Hooks are run in this order:
/// 1. Before hooks (actor → direct_object → indirect_object → room)
/// 2. On hooks (same order)
/// 3. After hooks (same order)
///
/// If any Before hook returns :veto, the action is cancelled.
/// If any On hook returns :handled, the default handler is skipped.
pub fn run_hooks_for_action(
    world: &World,
    action: &str,
    context: &HookContext,
    stdlib: &StdLib,
) -> Result<HookPipelineResult, HookError> {
    let mut result = HookPipelineResult::new();

    // Collect entities to check for hooks (in order)
    let entities: Vec<EntityId> = [
        Some(context.actor),
        context.direct_object,
        context.indirect_object,
        context.room,
    ]
    .into_iter()
    .flatten()
    .collect();

    // Run Before hooks
    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "Before", action) {
            let (hook_result, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);

            if hook_result.is_veto() {
                result.vetoed = true;
                return Ok(result);
            }
        }
    }

    // Run On hooks
    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "On", action) {
            let (hook_result, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);

            if hook_result.is_handled() {
                result.handled = true;
                break; // First :handled wins
            }
        }
    }

    // Run After hooks (only if not vetoed)
    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "After", action) {
            let (hook_result, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);

            // After hooks can't veto or handle, but we still collect their output
            let _ = hook_result;
        }
    }

    Ok(result)
}

/// Run only the Before hooks for an action.
pub fn run_before_hooks(
    world: &World,
    action: &str,
    context: &HookContext,
    stdlib: &StdLib,
) -> Result<HookPipelineResult, HookError> {
    let mut result = HookPipelineResult::new();

    let entities: Vec<EntityId> = [
        Some(context.actor),
        context.direct_object,
        context.indirect_object,
        context.room,
    ]
    .into_iter()
    .flatten()
    .collect();

    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "Before", action) {
            let (hook_result, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);

            if hook_result.is_veto() {
                result.vetoed = true;
                return Ok(result);
            }
        }
    }

    Ok(result)
}

/// Run only the On hooks for an action.
pub fn run_on_hooks(
    world: &World,
    action: &str,
    context: &HookContext,
    stdlib: &StdLib,
) -> Result<HookPipelineResult, HookError> {
    let mut result = HookPipelineResult::new();

    let entities: Vec<EntityId> = [
        Some(context.actor),
        context.direct_object,
        context.indirect_object,
        context.room,
    ]
    .into_iter()
    .flatten()
    .collect();

    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "On", action) {
            let (hook_result, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);

            if hook_result.is_handled() {
                result.handled = true;
                break;
            }
        }
    }

    Ok(result)
}

/// Run only the After hooks for an action.
pub fn run_after_hooks(
    world: &World,
    action: &str,
    context: &HookContext,
    stdlib: &StdLib,
) -> Result<HookPipelineResult, HookError> {
    let mut result = HookPipelineResult::new();

    let entities: Vec<EntityId> = [
        Some(context.actor),
        context.direct_object,
        context.indirect_object,
        context.room,
    ]
    .into_iter()
    .flatten()
    .collect();

    for entity in &entities {
        if let Some(hook) = world.get_hook(*entity, "After", action) {
            let (_, output, deletions) = execute_hook(world, hook, context, stdlib)?;
            result.output.extend(output);
            result.deletions.extend(deletions);
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::parse;
    use crate::vm::{Chunk, OpCode, StdLib, VM};

    // ===== HookResult Tests =====

    #[test]
    fn test_hook_result_is_veto() {
        assert!(HookResult::Veto("msg".to_string()).is_veto());
        assert!(!HookResult::Handled("msg".to_string()).is_veto());
        assert!(!HookResult::Continue.is_veto());
    }

    #[test]
    fn test_hook_result_is_handled() {
        assert!(HookResult::Handled("msg".to_string()).is_handled());
        assert!(!HookResult::Veto("msg".to_string()).is_handled());
        assert!(!HookResult::Continue.is_handled());
    }

    #[test]
    fn test_hook_result_message() {
        assert_eq!(HookResult::Continue.message(), None);
        assert_eq!(
            HookResult::Handled("handled msg".to_string()).message(),
            Some("handled msg")
        );
        assert_eq!(
            HookResult::Veto("veto msg".to_string()).message(),
            Some("veto msg")
        );
    }

    // ===== HookPipelineResult Tests =====

    #[test]
    fn test_hook_pipeline_result_combined_output() {
        let mut result = HookPipelineResult::new();
        result.output.push("line 1".to_string());
        result.output.push("line 2".to_string());
        assert_eq!(result.combined_output(), "line 1\nline 2");
    }

    #[test]
    fn test_hook_pipeline_result_default() {
        let result = HookPipelineResult::default();
        assert!(!result.vetoed);
        assert!(!result.handled);
        assert!(result.output.is_empty());
        assert!(result.deletions.is_empty());
    }

    // ===== Value to SExpr Tests =====

    #[test]
    fn test_value_to_sexpr_simple() {
        let value = Value::Int(42);
        let sexpr = value_to_sexpr(&value).unwrap();
        assert!(matches!(
            sexpr,
            crate::lang::SExpr::Atom(crate::lang::Atom::Int(42), _)
        ));
    }

    #[test]
    fn test_value_to_sexpr_list() {
        let value = Value::list(vec![
            Value::Symbol(Symbol::new("say")),
            Value::string("hello"),
        ]);
        let sexpr = value_to_sexpr(&value).unwrap();
        assert!(matches!(sexpr, crate::lang::SExpr::List(_, _)));
    }

    #[test]
    fn test_value_to_sexpr_empty_list() {
        let value = Value::list(vec![]);
        let sexpr = value_to_sexpr(&value).unwrap();
        // Empty list becomes nil symbol
        assert!(matches!(
            sexpr,
            crate::lang::SExpr::Atom(crate::lang::Atom::Symbol(_), _)
        ));
    }

    #[test]
    fn test_value_to_sexpr_bool() {
        let value = Value::Bool(true);
        let sexpr = value_to_sexpr(&value).unwrap();
        assert!(matches!(
            sexpr,
            crate::lang::SExpr::Atom(crate::lang::Atom::Bool(true), _)
        ));
    }

    #[test]
    fn test_value_to_sexpr_float() {
        let value = Value::Float(3.14);
        let sexpr = value_to_sexpr(&value).unwrap();
        if let crate::lang::SExpr::Atom(crate::lang::Atom::Float(f), _) = sexpr {
            assert!((f - 3.14).abs() < 0.001);
        } else {
            panic!("Expected float atom");
        }
    }

    // ===== Hook Context Opcode Tests =====

    #[test]
    fn test_hook_context_actor_opcode() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::GetHookActor { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let mut vm = VM::new(&chunk, &world, &stdlib).with_hook_context(context);
        let result = vm.run().unwrap();
        assert_eq!(result, Value::EntityRef(actor));
    }

    #[test]
    fn test_hook_context_direct_object_opcode() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::GetHookDirectObject { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let mut vm = VM::new(&chunk, &world, &stdlib).with_hook_context(context);
        let result = vm.run().unwrap();
        assert_eq!(result, Value::EntityRef(obj));
    }

    #[test]
    fn test_hook_context_direct_object_none() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::GetHookDirectObject { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let mut vm = VM::new(&chunk, &world, &stdlib).with_hook_context(context);
        let result = vm.run().unwrap();
        // NIL is represented as Bool(false) in the VM
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_hook_context_room_opcode() {
        let mut chunk = Chunk::new();
        chunk.emit(OpCode::GetHookRoom { dst: 0 }, 1);
        chunk.emit(OpCode::Return { src: 0 }, 1);

        let mut world = World::new();
        let actor = world.create_entity();
        let room = world.create_entity();
        let stdlib = StdLib::with_builtins();

        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: Some(room),
        };

        let mut vm = VM::new(&chunk, &world, &stdlib).with_hook_context(context);
        let result = vm.run().unwrap();
        assert_eq!(result, Value::EntityRef(room));
    }

    // ===== Effect Function Tests =====

    #[test]
    fn test_say_opcode_collects_output() {
        let mut chunk = Chunk::new();
        let msg_idx = chunk.add_constant(Value::string("Hello, world!"));
        let zero_idx = chunk.add_constant(Value::Int(0));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: msg_idx,
            },
            1,
        );
        chunk.emit(OpCode::Say { message: 0 }, 1);
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: zero_idx,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap();

        let output = vm.take_output();
        assert_eq!(output, vec!["Hello, world!".to_string()]);
    }

    #[test]
    fn test_say_multiple_outputs() {
        let mut chunk = Chunk::new();
        let msg1 = chunk.add_constant(Value::string("Line 1"));
        let msg2 = chunk.add_constant(Value::string("Line 2"));
        let zero_idx = chunk.add_constant(Value::Int(0));
        chunk.emit(OpCode::LoadConst { dst: 0, idx: msg1 }, 1);
        chunk.emit(OpCode::Say { message: 0 }, 1);
        chunk.emit(OpCode::LoadConst { dst: 0, idx: msg2 }, 1);
        chunk.emit(OpCode::Say { message: 0 }, 1);
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: zero_idx,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap();

        let output = vm.take_output();
        assert_eq!(output, vec!["Line 1".to_string(), "Line 2".to_string()]);
    }

    #[test]
    fn test_destroy_opcode_marks_deletion() {
        let mut chunk = Chunk::new();
        let mut world = World::new();
        let entity = world.create_entity();

        let entity_idx = chunk.add_constant(Value::EntityRef(entity));
        let zero_idx = chunk.add_constant(Value::Int(0));
        chunk.emit(
            OpCode::LoadConst {
                dst: 0,
                idx: entity_idx,
            },
            1,
        );
        chunk.emit(OpCode::Destroy { entity: 0 }, 1);
        chunk.emit(
            OpCode::LoadConst {
                dst: 1,
                idx: zero_idx,
            },
            1,
        );
        chunk.emit(OpCode::Return { src: 1 }, 1);

        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib);
        vm.run().unwrap();

        let deletions = vm.take_pending_deletions();
        assert_eq!(deletions, vec![entity]);
    }

    // ===== Compiled Hook Context Accessor Tests =====

    fn eval_with_hook_context(source: &str, context: HookContext) -> (Value, Vec<String>) {
        let expr = parse(source).unwrap();
        let chunk = Compiler::compile(&expr).unwrap();
        let world = World::new();
        let stdlib = StdLib::with_builtins();
        let mut vm = VM::new(&chunk, &world, &stdlib).with_hook_context(context);
        let result = vm.run().unwrap();
        let output = vm.take_output();
        (result, output)
    }

    #[test]
    fn test_compiled_actor_accessor() {
        let mut world = World::new();
        let actor = world.create_entity();
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (result, _) = eval_with_hook_context("(actor)", context);
        assert_eq!(result, Value::EntityRef(actor));
    }

    #[test]
    fn test_compiled_direct_object_accessor() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let (result, _) = eval_with_hook_context("(direct-object)", context);
        assert_eq!(result, Value::EntityRef(obj));
    }

    #[test]
    fn test_compiled_say_effect() {
        let mut world = World::new();
        let actor = world.create_entity();
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (_, output) = eval_with_hook_context("(say \"Test message\")", context);
        assert_eq!(output, vec!["Test message".to_string()]);
    }

    // ===== Hook Component Lookup Tests =====

    #[test]
    fn test_get_hook() {
        let mut world = World::new();
        let entity = world.create_entity();

        // Set a hook component
        let hook_body = Value::list(vec![
            Value::Symbol(Symbol::new("say")),
            Value::string("burning!"),
        ]);
        world.set_component(entity, "On:burn", hook_body.clone());

        // Retrieve it
        let hook = world.get_hook(entity, "On", "burn");
        assert!(hook.is_some());
        assert_eq!(hook.unwrap(), &hook_body);

        // Non-existent hook
        let no_hook = world.get_hook(entity, "Before", "burn");
        assert!(no_hook.is_none());
    }

    #[test]
    fn test_has_hook() {
        let mut world = World::new();
        let entity = world.create_entity();

        world.set_component(
            entity,
            "Before:take",
            Value::list(vec![Value::Symbol(Symbol::new("veto"))]),
        );

        assert!(world.has_hook(entity, "Before", "take"));
        assert!(!world.has_hook(entity, "On", "take"));
        assert!(!world.has_hook(entity, "After", "take"));
    }

    #[test]
    fn test_get_hooks_for_action() {
        let mut world = World::new();
        let entity = world.create_entity();

        let before_hook = Value::list(vec![
            Value::Symbol(Symbol::new("say")),
            Value::string("before"),
        ]);
        let on_hook = Value::list(vec![Value::Symbol(Symbol::new("say")), Value::string("on")]);

        world.set_component(entity, "Before:open", before_hook.clone());
        world.set_component(entity, "On:open", on_hook.clone());

        let (before, on, after) = world.get_hooks_for_action(entity, "open");
        assert_eq!(before.unwrap(), &before_hook);
        assert_eq!(on.unwrap(), &on_hook);
        assert!(after.is_none());
    }

    #[test]
    fn test_hooks_for_phase() {
        let mut world = World::new();
        let entity = world.create_entity();

        world.set_component(
            entity,
            "On:burn",
            Value::list(vec![Value::Symbol(Symbol::new("handled"))]),
        );
        world.set_component(
            entity,
            "On:open",
            Value::list(vec![Value::Symbol(Symbol::new("handled"))]),
        );
        world.set_component(
            entity,
            "Before:take",
            Value::list(vec![Value::Symbol(Symbol::new("veto"))]),
        );

        let on_hooks: Vec<_> = world.hooks_for_phase(entity, "On").collect();
        assert_eq!(on_hooks.len(), 2);

        // Should have both burn and open
        let action_names: Vec<_> = on_hooks.iter().map(|(name, _)| name.as_str()).collect();
        assert!(action_names.contains(&"burn"));
        assert!(action_names.contains(&"open"));
    }

    // ===== Hook Execution Tests =====

    #[test]
    fn test_execute_hook_returns_continue() {
        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Hook that returns an integer (not :handled or :veto)
        let hook_expr = Value::list(vec![Value::Int(42)]);
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (result, _, _) = execute_hook(&world, &hook_expr, &context, &stdlib).unwrap();
        assert!(matches!(result, HookResult::Continue));
    }

    #[test]
    fn test_execute_hook_returns_handled() {
        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Hook that returns :handled symbol
        let hook_expr = Value::list(vec![Value::Symbol(Symbol::new("handled"))]);
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (result, _, _) = execute_hook(&world, &hook_expr, &context, &stdlib).unwrap();
        assert!(result.is_handled());
    }

    #[test]
    fn test_execute_hook_returns_veto() {
        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Hook that returns :veto symbol
        let hook_expr = Value::list(vec![Value::Symbol(Symbol::new("veto"))]);
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (result, _, _) = execute_hook(&world, &hook_expr, &context, &stdlib).unwrap();
        assert!(result.is_veto());
    }

    #[test]
    fn test_execute_hook_with_say() {
        let mut world = World::new();
        let actor = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Hook body: (say "The torch flares!") :handled
        // Automatically wrapped in (do ...) by execute_hook
        let hook_expr = Value::list(vec![
            Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("The torch flares!"),
            ]),
            Value::Symbol(Symbol::new("handled")),
        ]);
        let context = HookContext {
            actor,
            direct_object: None,
            indirect_object: None,
            room: None,
        };

        let (result, output, _) = execute_hook(&world, &hook_expr, &context, &stdlib).unwrap();
        assert!(result.is_handled());
        assert_eq!(output, vec!["The torch flares!".to_string()]);
    }

    // ===== Hook Pipeline Tests =====

    #[test]
    fn test_before_hook_veto_stops_pipeline() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Object has a Before:take hook that vetoes
        // Hook body automatically wrapped in (do ...) by execute_hook
        world.set_component(
            obj,
            "Before:take",
            Value::list(vec![
                Value::list(vec![
                    Value::Symbol(Symbol::new("say")),
                    Value::string("You cannot take this!"),
                ]),
                Value::Symbol(Symbol::new("veto")),
            ]),
        );

        // Also add an On:take hook that should NOT run
        world.set_component(
            obj,
            "On:take",
            Value::list(vec![Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("This should not appear"),
            ])]),
        );

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let result = run_hooks_for_action(&world, "take", &context, &stdlib).unwrap();
        assert!(result.vetoed);
        assert!(!result.handled);
        assert_eq!(result.output, vec!["You cannot take this!".to_string()]);
    }

    #[test]
    fn test_on_hook_handled_skips_later_on_hooks() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Actor has an On:burn hook
        world.set_component(
            actor,
            "On:burn",
            Value::list(vec![
                Value::list(vec![
                    Value::Symbol(Symbol::new("say")),
                    Value::string("Actor handles burn!"),
                ]),
                Value::Symbol(Symbol::new("handled")),
            ]),
        );

        // Object also has an On:burn hook (should not run since actor's handles first)
        world.set_component(
            obj,
            "On:burn",
            Value::list(vec![Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("Object burn - should not appear"),
            ])]),
        );

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let result = run_hooks_for_action(&world, "burn", &context, &stdlib).unwrap();
        assert!(!result.vetoed);
        assert!(result.handled);
        assert_eq!(result.output, vec!["Actor handles burn!".to_string()]);
    }

    #[test]
    fn test_after_hooks_always_run() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // On hook that handles
        world.set_component(
            obj,
            "On:open",
            Value::list(vec![
                Value::list(vec![
                    Value::Symbol(Symbol::new("say")),
                    Value::string("Opening..."),
                ]),
                Value::Symbol(Symbol::new("handled")),
            ]),
        );

        // After hook should still run
        world.set_component(
            obj,
            "After:open",
            Value::list(vec![Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("The door creaks."),
            ])]),
        );

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let result = run_hooks_for_action(&world, "open", &context, &stdlib).unwrap();
        assert!(result.handled);
        assert_eq!(result.output.len(), 2);
        assert_eq!(result.output[0], "Opening...");
        assert_eq!(result.output[1], "The door creaks.");
    }

    #[test]
    fn test_hook_order_actor_before_direct_object() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // Actor has Before hook
        // Hook body is a list of expressions, each wrapped
        world.set_component(
            actor,
            "Before:examine",
            Value::list(vec![Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("Actor before"),
            ])]),
        );

        // Object has Before hook
        world.set_component(
            obj,
            "Before:examine",
            Value::list(vec![Value::list(vec![
                Value::Symbol(Symbol::new("say")),
                Value::string("Object before"),
            ])]),
        );

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let result = run_before_hooks(&world, "examine", &context, &stdlib).unwrap();
        // Actor runs first, then object
        assert_eq!(result.output[0], "Actor before");
        assert_eq!(result.output[1], "Object before");
    }

    #[test]
    fn test_hook_with_destroy_effect() {
        let mut world = World::new();
        let actor = world.create_entity();
        let obj = world.create_entity();
        let stdlib = StdLib::with_builtins();

        // On:burn hook that destroys the object
        world.set_component(
            obj,
            "On:burn",
            Value::list(vec![
                Value::list(vec![
                    Value::Symbol(Symbol::new("say")),
                    Value::string("The paper burns away!"),
                ]),
                Value::list(vec![
                    Value::Symbol(Symbol::new("destroy")),
                    Value::list(vec![Value::Symbol(Symbol::new("direct-object"))]),
                ]),
                Value::Symbol(Symbol::new("handled")),
            ]),
        );

        let context = HookContext {
            actor,
            direct_object: Some(obj),
            indirect_object: None,
            room: None,
        };

        let result = run_hooks_for_action(&world, "burn", &context, &stdlib).unwrap();
        assert!(result.handled);
        assert_eq!(result.deletions, vec![obj]);
    }
}
