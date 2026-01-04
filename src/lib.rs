//! Hornvale: A world programming environment.
//!
//! Hornvale is a minimal system for defining entities, rules, and derivations
//! that can express anything from a goat in a room to a procedurally generated galaxy.
//!
//! ## Phase 1: "Goat in a Room"
//!
//! The simplest possible world that does something:
//! - Entities with components
//! - A tick-based simulation loop
//! - Hardcoded rules (goat says "Baa!" every 10 ticks)
//! - Interactive REPL for inspection

pub mod action;
pub mod compiler;
pub mod core;
pub mod derive;
pub mod direction;
pub mod execution;
pub mod generator;
pub mod grammar;
pub mod hooks;
pub mod input;
pub mod io;
pub mod lang;
pub mod precondition;
pub mod repl;
pub mod rng;
pub mod rules;
pub mod symbol;
pub mod systems;
pub mod template;
pub mod verbs;
pub mod vm;

// Re-export commonly used types at the crate root
pub use core::{
    Cardinality, ComponentTypeId, EntityId, EpochSnapshot, Layer, LayerError, Phase,
    RelationSchema, RelationTypeId, Value, World,
};
pub use derive::{ComposeMode, DerivationEngine, DerivationError, DerivationRule, DerivedProperty};
pub use generator::{
    GenerationResult, GenerationStub, Generator, GeneratorRegistry, StubStorage, get_or_generate,
    has_or_stub,
};
pub use grammar::{
    Command as GrammarCommand, CommandRegistry, Form, FormAction, FormElement, GrammarMatch,
    IntentTrie, SlotType, SlotValue, TypePredicate, TypeRegistry,
};
pub use hooks::{
    HookError, HookPipelineResult, HookResult, PendingMutations, execute_hook, run_after_hooks,
    run_before_hooks, run_hooks_for_action, run_on_hooks,
};
pub use input::{
    Command, EntityCandidate, Input, ObjectRef, ResolutionResult, ResolvedCommand, Resolver,
    ScopeProvider, Token, parse_input,
};
pub use io::{StdIO, WorldIO};
pub use rng::SeededRng;
pub use symbol::Symbol;
pub use template::{FieldSpec, Template, TemplateRegistry};
pub use verbs::{VerbResult, execute_grammar_action_full};

// Action and precondition system
pub use action::{
    Action as ActionDef, ActionCheckResult, ActionRegistry, HandlerRegistry,
    check_action_preconditions,
};
pub use direction::{DirectionDef, DirectionRegistry};
pub use precondition::{
    Precondition, PreconditionArg, PreconditionCall, PreconditionError, PreconditionRegistry,
    PreconditionResult,
};

// Execution layer
pub use execution::{
    TraceSpan, begin_trace_span, create_action_attempt, create_command_entity, create_input,
    create_parse_result, create_result, enable_tracing, end_trace_span, query_execution_chain,
    register_execution_relations,
};
