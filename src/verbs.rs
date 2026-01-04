//! Verb execution system for interactive fiction.
//!
//! This module provides the action execution pipeline that processes
//! grammar-matched commands through hooks and DSL handlers.
//!
//! ## Architecture
//!
//! All verb handlers are now defined in DSL via `libs/std/_lib.hvl`.
//! This module provides:
//! - `VerbResult` - Result type for action execution
//! - `execute_grammar_action` - Simple action execution
//! - `execute_grammar_action_full` - Full action execution with preconditions
//! - Component and relation type helpers
//!
//! ## Usage
//!
//! Load the standard library to get all standard verbs:
//! ```lisp
//! (load "libs/std/_lib.hvl")
//! ```

use crate::core::{ComponentTypeId, EntityId, RelationTypeId, World};
use std::sync::Arc;

/// Well-known component types for game objects.
pub mod components {
    use super::*;

    /// Output text to display to player.
    pub fn output() -> ComponentTypeId {
        ComponentTypeId::new("Output")
    }

    /// Primary name of an entity.
    pub fn name() -> ComponentTypeId {
        ComponentTypeId::new("Name")
    }

    /// Brief description (for lists).
    pub fn brief() -> ComponentTypeId {
        ComponentTypeId::new("Brief")
    }

    /// Detailed description (for examine).
    pub fn description() -> ComponentTypeId {
        ComponentTypeId::new("Description")
    }

    /// Room description (shown on look).
    pub fn room_description() -> ComponentTypeId {
        ComponentTypeId::new("RoomDescription")
    }

    /// Whether entity is a room.
    pub fn is_room() -> ComponentTypeId {
        ComponentTypeId::new("IsRoom")
    }

    /// Whether entity is portable.
    pub fn portable() -> ComponentTypeId {
        ComponentTypeId::new("Portable")
    }

    /// Whether entity is fixed (cannot be taken).
    pub fn fixed() -> ComponentTypeId {
        ComponentTypeId::new("Fixed")
    }

    /// Whether entity is a player.
    pub fn is_player() -> ComponentTypeId {
        ComponentTypeId::new("IsPlayer")
    }

    // --- Article-related components ---

    /// Whether name is a proper noun (no article, capitalize).
    /// e.g., "Bob" not "the Bob"
    pub fn proper_noun() -> ComponentTypeId {
        ComponentTypeId::new("ProperNoun")
    }

    /// Whether name starts with a vowel sound (use "an" not "a").
    /// e.g., "an apple" not "a apple"
    pub fn vowel_sound() -> ComponentTypeId {
        ComponentTypeId::new("VowelSound")
    }

    /// Whether to omit articles entirely.
    /// e.g., "you" not "a you" or "the you"
    pub fn no_article() -> ComponentTypeId {
        ComponentTypeId::new("NoArticle")
    }

    /// Whether name is plural.
    /// e.g., "pants" (affects verb agreement)
    pub fn plural_noun() -> ComponentTypeId {
        ComponentTypeId::new("PluralNoun")
    }

    /// Description shown before object is tampered with.
    /// e.g., "A brass lantern is on the trophy case."
    pub fn initial_description() -> ComponentTypeId {
        ComponentTypeId::new("InitialDescription")
    }

    /// Description shown in room listings after tampering.
    /// e.g., "There is a brass lantern here."
    pub fn ground_description() -> ComponentTypeId {
        ComponentTypeId::new("GroundDescription")
    }

    /// Whether object has been tampered with (moved, taken, etc.).
    pub fn tampered() -> ComponentTypeId {
        ComponentTypeId::new("Tampered")
    }
}

/// Well-known relation types.
pub mod relations {
    use super::*;

    /// Entity is located in a room.
    pub fn in_room() -> RelationTypeId {
        RelationTypeId::new("InRoom")
    }

    /// Entity contains another entity (inventory).
    pub fn contains() -> RelationTypeId {
        RelationTypeId::new("Contains")
    }

    /// Exit relation: (room, direction) -> destination room.
    pub fn exit() -> RelationTypeId {
        RelationTypeId::new("Exit")
    }
}

/// Result of executing a verb.
#[derive(Debug, Clone)]
pub struct VerbResult {
    /// Output text to display.
    pub output: Arc<str>,
    /// Whether the verb succeeded.
    pub success: bool,
}

impl VerbResult {
    /// Create a successful result.
    pub fn success(output: impl Into<Arc<str>>) -> Self {
        Self {
            output: output.into(),
            success: true,
        }
    }

    /// Create a failed result.
    pub fn fail(output: impl Into<Arc<str>>) -> Self {
        Self {
            output: output.into(),
            success: false,
        }
    }
}

/// Execute an action based on a grammar match.
///
/// This function dispatches to verb handlers based on the action name
/// from a `GrammarMatch`, using pre-resolved entity slots.
///
/// Hooks are executed in order:
/// 1. Before hooks (can veto)
/// 2. On hooks (can handle)
/// 3. DSL handler (if defined)
/// 4. After hooks
pub fn execute_grammar_action(
    world: &mut World,
    actor: EntityId,
    grammar_match: &crate::GrammarMatch,
    stdlib: &crate::vm::StdLib,
) -> VerbResult {
    // Call the full version with no registries
    execute_grammar_action_full(world, actor, grammar_match, stdlib, None, None, None)
}

/// Execute an action based on a grammar match with full precondition support.
///
/// This function dispatches to verb handlers based on the action name
/// from a `GrammarMatch`, using pre-resolved entity slots.
///
/// Execution order (within a transaction):
/// 1. Check preconditions (can fail with message)
/// 2. Before hooks (can veto) - mutations applied immediately
/// 3. On hooks (can handle) - mutations applied immediately
/// 4. DSL handler (if defined and not handled by On hook)
/// 5. After hooks - mutations applied immediately
/// 6. Commit transaction on success, rollback on veto/failure
///
/// Each phase sees mutations from previous phases (read-your-writes).
pub fn execute_grammar_action_full(
    world: &mut World,
    actor: EntityId,
    grammar_match: &crate::GrammarMatch,
    stdlib: &crate::vm::StdLib,
    action_registry: Option<&crate::action::ActionRegistry>,
    precondition_registry: Option<&crate::precondition::PreconditionRegistry>,
    function_registry: Option<&crate::lang::FunctionRegistry>,
) -> VerbResult {
    use crate::action::check_action_preconditions;
    use crate::hooks::{run_after_hooks, run_before_hooks, run_on_hooks};
    use crate::vm::ActionContext;

    let action_name = grammar_match.action.action_name();
    let action_str = action_name.as_str();

    // Build action context
    let direct_object = grammar_match.get_entity("obj");
    let indirect_object = grammar_match.get_entity("indirect");
    let room = world.get_entity_room(actor);

    let mut action_context = ActionContext::new(actor);
    if let Some(obj) = direct_object {
        action_context = action_context.with_direct_object(obj);
    }
    if let Some(obj) = indirect_object {
        action_context = action_context.with_indirect_object(obj);
    }
    if let Some(r) = room {
        action_context = action_context.with_room(r);
    }

    // Check preconditions (if registries are provided) BEFORE starting transaction
    // Preconditions are pure checks, they don't mutate
    if let (Some(action_reg), Some(precond_reg)) = (action_registry, precondition_registry) {
        if let Some(action_def) = action_reg.get(action_name) {
            let check_result =
                check_action_preconditions(action_def, world, &action_context, precond_reg, stdlib);

            if check_result.failed() {
                return VerbResult::fail(check_result.message().unwrap_or("You can't do that."));
            }
        }
    }

    // Begin transaction for the action execution
    world.begin_transaction();

    let mut output_parts: Vec<String> = Vec::new();

    // Run Before hooks
    match run_before_hooks(world, &action_str, &action_context, stdlib) {
        Ok(before_result) => {
            output_parts.extend(before_result.output);

            // Apply mutations so subsequent phases see them
            before_result.mutations.apply_to(world);

            if before_result.vetoed {
                // Rollback all changes on veto
                let _ = world.rollback_transaction();
                let msg = if output_parts.is_empty() {
                    "You can't do that.".to_string()
                } else {
                    output_parts.join("\n")
                };
                return VerbResult::fail(msg);
            }
        }
        Err(e) => {
            // Hook execution error - rollback and report
            let _ = world.rollback_transaction();
            return VerbResult::fail(format!("Hook error: {e}"));
        }
    }

    // Run On hooks
    let handled = match run_on_hooks(world, &action_str, &action_context, stdlib) {
        Ok(on_result) => {
            output_parts.extend(on_result.output);

            // Apply mutations so subsequent phases see them
            on_result.mutations.apply_to(world);

            on_result.handled
        }
        Err(e) => {
            // Hook execution error - rollback and report
            let _ = world.rollback_transaction();
            return VerbResult::fail(format!("Hook error: {e}"));
        }
    };

    // Run DSL handler if not handled by On hook
    if !handled {
        let action_def = action_registry.and_then(|reg| reg.get(action_name));

        if let Some(action) = action_def {
            if action.has_dsl_handler() {
                // Execute DSL handler with user-defined functions
                if let Some(body) = action.handler().dsl_body() {
                    match crate::action::execute_dsl_handler_with_functions(
                        body,
                        world,
                        &action_context,
                        stdlib,
                        function_registry,
                    ) {
                        Ok(dsl_result) => {
                            output_parts.extend(dsl_result.output);
                            dsl_result.mutations.apply_to(world);

                            // Check for failure
                            if dsl_result.result.is_failure() {
                                let _ = world.rollback_transaction();
                                let msg = dsl_result
                                    .result
                                    .failure_message()
                                    .map(String::from)
                                    .unwrap_or_else(|| output_parts.join("\n"));
                                return VerbResult::fail(msg);
                            }
                        }
                        Err(e) => {
                            let _ = world.rollback_transaction();
                            return VerbResult::fail(format!("Handler error: {e}"));
                        }
                    }
                }
            }
            // No else clause - if action exists but has no DSL handler, it's a no-op
            // (the action definition may only have preconditions, with hooks doing the work)
        }
        // No action definition means the action is unimplemented - that's okay,
        // the grammar matched but there's nothing to do (yet)
    }

    // Run After hooks
    match run_after_hooks(world, &action_str, &action_context, stdlib) {
        Ok(after_result) => {
            output_parts.extend(after_result.output);

            // Apply mutations
            after_result.mutations.apply_to(world);
        }
        Err(e) => {
            // After hook error - rollback and report
            let _ = world.rollback_transaction();
            return VerbResult::fail(format!("Hook error: {e}"));
        }
    }

    // Commit transaction on success
    let _ = world.commit_transaction();

    // Combine output
    let final_output = output_parts.join("\n");
    VerbResult::success(final_output)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verb_result_success() {
        let result = VerbResult::success("Done.");
        assert!(result.success);
        assert_eq!(result.output.as_ref(), "Done.");
    }

    #[test]
    fn test_verb_result_fail() {
        let result = VerbResult::fail("Can't do that.");
        assert!(!result.success);
        assert_eq!(result.output.as_ref(), "Can't do that.");
    }

    #[test]
    fn test_component_helpers() {
        // Just verify these return the expected component types
        assert_eq!(components::name().0.as_str(), "Name");
        assert_eq!(components::description().0.as_str(), "Description");
        assert_eq!(components::is_room().0.as_str(), "IsRoom");
        assert_eq!(components::portable().0.as_str(), "Portable");
        assert_eq!(components::tampered().0.as_str(), "Tampered");
    }

    #[test]
    fn test_relation_helpers() {
        assert_eq!(relations::in_room().0.as_str(), "InRoom");
        assert_eq!(relations::contains().0.as_str(), "Contains");
        assert_eq!(relations::exit().0.as_str(), "Exit");
    }
}
