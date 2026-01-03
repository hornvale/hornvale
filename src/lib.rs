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
pub mod syntax;
pub mod systems;
pub mod template;
pub mod verbs;
pub mod vm;

// Re-export commonly used types at the crate root
pub use core::{
    Cardinality, ComponentTypeId, EntityId, RelationSchema, RelationTypeId, Value, World,
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
    HookError, HookPipelineResult, HookResult, execute_hook, run_after_hooks, run_before_hooks,
    run_hooks_for_action, run_on_hooks,
};
pub use input::{
    Command, EntityCandidate, Input, ObjectRef, ResolutionResult, ResolvedCommand, Resolver,
    ScopeProvider, Token, parse_input,
};
pub use io::{StdIO, WorldIO};
pub use rng::SeededRng;
pub use symbol::Symbol;
pub use syntax::{Action, PatternBuilder, SyntaxElement, SyntaxMatch, SyntaxPattern, SyntaxTable};
pub use template::{FieldSpec, Template, TemplateRegistry};
pub use verbs::{VerbResult, execute_action, execute_command, execute_grammar_action_full};

// Action and precondition system
pub use action::{
    Action as ActionDef, ActionCheckResult, ActionRegistry, HandlerRegistry,
    check_action_preconditions,
};
pub use precondition::{
    Precondition, PreconditionArg, PreconditionCall, PreconditionError, PreconditionRegistry,
    PreconditionResult,
};
