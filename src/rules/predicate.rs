//! Compiled predicate patterns for VM-based filtering.
//!
//! PredicatePatterns are boolean expressions compiled to bytecode. They answer
//! the question "does entity E match?" efficiently via the VM.
//!
//! ## When to Use
//!
//! Use PredicatePattern for:
//! - Precondition checks (entity already resolved)
//! - Hook guards (direct-object already known)
//! - Type validation (candidate entity identified)
//! - Derivation filters (evaluating one entity at a time)
//!
//! Use Rust MatchPlan (the existing Pattern enum) for:
//! - "Find all creatures with HP > 0" → Rust query + optional VM filter
//! - "Find entities in room" → Rust relation query
//! - Periodic rule matching → Rust index lookup, then VM filter
//!
//! ## Example
//!
//! ```ignore
//! // Create a predicate from an S-expression
//! let pred = PredicatePattern::compile(
//!     "(and (has? ?e :Portable) (not (has? ?e :Fixed)))"
//! )?;
//!
//! // Evaluate against a specific entity
//! let matches = pred.evaluate(&world, lamp)?;
//! ```

use crate::compiler::{CompileError, Compiler};
use crate::core::{EntityId, Value, World};
use crate::lang::{Atom, SExpr, Span};
use crate::symbol::Symbol;
use crate::vm::{StdLib, VM, VMError};
use std::sync::Arc;

/// A compiled predicate pattern.
///
/// PredicatePatterns store the source expression and recompile with bindings
/// at evaluation time. This is necessary because the entity binding must be
/// baked into the bytecode at compile time.
#[derive(Debug, Clone)]
pub struct PredicatePattern {
    /// The original S-expression source (for debugging/inspection).
    source: Arc<str>,

    /// The parsed S-expression (for recompilation with bindings).
    sexpr: SExpr,

    /// The bound variable name (usually "?e" or "entity").
    bound_var: Symbol,
}

/// Error from predicate compilation or evaluation.
#[derive(Debug, Clone)]
pub enum PredicateError {
    /// Compilation failed.
    Compile(String),
    /// VM execution failed.
    Runtime(String),
}

impl std::fmt::Display for PredicateError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PredicateError::Compile(msg) => write!(f, "predicate compile error: {msg}"),
            PredicateError::Runtime(msg) => write!(f, "predicate runtime error: {msg}"),
        }
    }
}

impl std::error::Error for PredicateError {}

impl From<CompileError> for PredicateError {
    fn from(e: CompileError) -> Self {
        PredicateError::Compile(e.to_string())
    }
}

impl From<VMError> for PredicateError {
    fn from(e: VMError) -> Self {
        PredicateError::Runtime(e.to_string())
    }
}

impl PredicatePattern {
    /// Compile a predicate from an S-expression string.
    ///
    /// The predicate should reference `?e` (or `entity`) as the bound variable.
    pub fn compile(source: &str, bound_var: &str) -> Result<Self, PredicateError> {
        let sexpr =
            crate::lang::parse(source).map_err(|e| PredicateError::Compile(e.to_string()))?;

        Self::compile_sexpr(&sexpr, bound_var, source)
    }

    /// Compile a predicate from an SExpr AST.
    pub fn compile_sexpr(
        sexpr: &SExpr,
        bound_var: &str,
        source: &str,
    ) -> Result<Self, PredicateError> {
        // Validate: do a test compile to catch syntax errors early
        let _ = Compiler::compile(sexpr)?;

        Ok(Self {
            source: source.into(),
            sexpr: sexpr.clone(),
            bound_var: Symbol::new(bound_var),
        })
    }

    /// Compile a predicate from a Value (S-expression as data).
    pub fn compile_value(value: &Value, bound_var: &str) -> Result<Self, PredicateError> {
        let source = format!("{value}");
        let sexpr = value_to_sexpr(value)?;
        Self::compile_sexpr(&sexpr, bound_var, &source)
    }

    /// Compile a predicate from an SExpr for use with custom bindings.
    ///
    /// This is useful for type predicates that use `entity` and `actor` bindings.
    /// The predicate won't have a default bound variable - use `evaluate_with_bindings`.
    pub fn from_sexpr(sexpr: SExpr) -> Result<Self, PredicateError> {
        // Validate: do a test compile to catch syntax errors early
        let _ = Compiler::compile(&sexpr)?;

        let source = format!("{sexpr}");
        Ok(Self {
            source: source.into(),
            sexpr,
            bound_var: Symbol::new("entity"), // Default for backwards compatibility
        })
    }

    /// Evaluate this predicate against an entity.
    ///
    /// Returns true if the entity matches the predicate.
    pub fn evaluate(&self, world: &World, entity: EntityId) -> Result<bool, PredicateError> {
        self.evaluate_with_stdlib(world, entity, &StdLib::with_builtins())
    }

    /// Evaluate with a custom stdlib.
    pub fn evaluate_with_stdlib(
        &self,
        world: &World,
        entity: EntityId,
        stdlib: &StdLib,
    ) -> Result<bool, PredicateError> {
        // Compile with entity binding baked in
        let chunk = Compiler::compile_with_bindings(
            &self.sexpr,
            &[(self.bound_var, Value::EntityRef(entity))],
        )?;

        let mut vm = VM::new(&chunk, world, stdlib);
        let result = vm.run()?;
        Ok(result.is_truthy())
    }

    /// Evaluate with custom bindings.
    ///
    /// This allows passing arbitrary bindings like `entity` and `actor` for type predicates.
    pub fn evaluate_with_bindings(
        &self,
        world: &World,
        bindings: &[(Symbol, Value)],
    ) -> Result<bool, PredicateError> {
        self.evaluate_with_bindings_and_stdlib(world, bindings, &StdLib::with_builtins())
    }

    /// Evaluate with custom bindings and stdlib.
    pub fn evaluate_with_bindings_and_stdlib(
        &self,
        world: &World,
        bindings: &[(Symbol, Value)],
        stdlib: &StdLib,
    ) -> Result<bool, PredicateError> {
        let chunk = Compiler::compile_with_bindings(&self.sexpr, bindings)?;
        let mut vm = VM::new(&chunk, world, stdlib);
        let result = vm.run()?;
        Ok(result.is_truthy())
    }

    /// Get the source expression.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the bound variable name.
    pub fn bound_var(&self) -> Symbol {
        self.bound_var
    }

    /// Get a reference to the parsed S-expression.
    pub fn sexpr(&self) -> &SExpr {
        &self.sexpr
    }
}

/// Convert a Value to SExpr for compilation.
fn value_to_sexpr(value: &Value) -> Result<SExpr, PredicateError> {
    let span = Span::new(0, 0, 0, 0);

    match value {
        Value::Nil => Ok(SExpr::Atom(Atom::Nil, span)),
        Value::List(items) => {
            if items.is_empty() {
                Ok(SExpr::Atom(Atom::Nil, span))
            } else {
                let exprs: Result<Vec<_>, _> = items.iter().map(value_to_sexpr).collect();
                Ok(SExpr::List(exprs?, span))
            }
        }
        Value::Int(n) => Ok(SExpr::Atom(Atom::Int(*n), span)),
        Value::Float(f) => Ok(SExpr::Atom(Atom::Float(*f), span)),
        Value::Bool(b) => Ok(SExpr::Atom(Atom::Bool(*b), span)),
        Value::String(s) => Ok(SExpr::Atom(Atom::String(s.to_string()), span)),
        Value::Symbol(s) => Ok(SExpr::Atom(Atom::Symbol(*s), span)),
        Value::EntityRef(e) => Ok(SExpr::Atom(
            Atom::Symbol(Symbol::new(&format!("__entity_{}", e.raw()))),
            span,
        )),
    }
}

// =============================================================================
// Predicate builders for common patterns
// =============================================================================

/// Build common predicate expressions.
pub mod builders {
    use super::*;

    /// Create a "has component" predicate: `(has? ?e :Component)`
    pub fn has_component(component: &str) -> Value {
        Value::list(vec![
            Value::Symbol(Symbol::new("has?")),
            Value::Symbol(Symbol::new("?e")),
            Value::Symbol(Symbol::new(component)),
        ])
    }

    /// Create a "component equals" predicate: `(= (get ?e :Component) value)`
    pub fn component_equals(component: &str, value: Value) -> Value {
        Value::list(vec![
            Value::Symbol(Symbol::new("=")),
            Value::list(vec![
                Value::Symbol(Symbol::new("get")),
                Value::Symbol(Symbol::new("?e")),
                Value::Symbol(Symbol::new(component)),
            ]),
            value,
        ])
    }

    /// Create an "and" predicate combining multiple conditions.
    pub fn and(predicates: Vec<Value>) -> Value {
        let mut items = vec![Value::Symbol(Symbol::new("and"))];
        items.extend(predicates);
        Value::list(items)
    }

    /// Create an "or" predicate combining multiple conditions.
    pub fn or(predicates: Vec<Value>) -> Value {
        let mut items = vec![Value::Symbol(Symbol::new("or"))];
        items.extend(predicates);
        Value::list(items)
    }

    /// Create a "not" predicate.
    pub fn not(predicate: Value) -> Value {
        Value::list(vec![Value::Symbol(Symbol::new("not")), predicate])
    }

    /// Create "portable" predicate: has Portable and not Fixed.
    pub fn is_portable() -> Value {
        and(vec![has_component("Portable"), not(has_component("Fixed"))])
    }

    /// Create "creature" predicate: has HP component.
    pub fn is_creature() -> Value {
        has_component("HP")
    }

    /// Create "container" predicate: has Container component.
    pub fn is_container() -> Value {
        has_component("Container")
    }
}

// =============================================================================
// Integration with existing Pattern enum
// =============================================================================

/// Extension trait to add predicate-based filtering to Pattern results.
pub trait PatternFilter {
    /// Filter entities using a compiled predicate.
    fn filter_with_predicate<'a>(
        &'a self,
        world: &'a World,
        predicate: &'a PredicatePattern,
    ) -> impl Iterator<Item = EntityId> + 'a;
}

impl PatternFilter for Vec<EntityId> {
    fn filter_with_predicate<'a>(
        &'a self,
        world: &'a World,
        predicate: &'a PredicatePattern,
    ) -> impl Iterator<Item = EntityId> + 'a {
        self.iter()
            .copied()
            .filter(move |&entity| predicate.evaluate(world, entity).unwrap_or(false))
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Cardinality;
    use crate::core::RelationSchema;

    fn setup_world() -> World {
        let mut world = World::new();

        // Register relations
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create entities
        let lamp = world.create_entity();
        world.set_component(lamp, "Name", "brass lamp");
        world.set_component(lamp, "Portable", true);

        let table = world.create_entity();
        world.set_component(table, "Name", "heavy table");
        world.set_component(table, "Fixed", true);

        let goblin = world.create_entity();
        world.set_component(goblin, "Name", "goblin");
        world.set_component(goblin, "HP", 10_i64);

        world
    }

    #[test]
    fn test_has_component_predicate() {
        let world = setup_world();

        // Predicate: (has? ?e :Portable)
        let pred =
            PredicatePattern::compile_value(&builders::has_component("Portable"), "?e").unwrap();

        let lamp = EntityId::from_raw(0);
        let table = EntityId::from_raw(1);
        let goblin = EntityId::from_raw(2);

        assert!(pred.evaluate(&world, lamp).unwrap());
        assert!(!pred.evaluate(&world, table).unwrap());
        assert!(!pred.evaluate(&world, goblin).unwrap());
    }

    #[test]
    fn test_component_equals_predicate() {
        let world = setup_world();

        // Predicate: (= (get ?e :Name) "goblin")
        let pred = PredicatePattern::compile_value(
            &builders::component_equals("Name", Value::string("goblin")),
            "?e",
        )
        .unwrap();

        let lamp = EntityId::from_raw(0);
        let goblin = EntityId::from_raw(2);

        assert!(!pred.evaluate(&world, lamp).unwrap());
        assert!(pred.evaluate(&world, goblin).unwrap());
    }

    #[test]
    fn test_and_predicate() {
        let world = setup_world();

        // Add Portable to goblin
        let goblin = EntityId::from_raw(2);
        let mut world = world;
        world.set_component(goblin, "Portable", true);

        // Predicate: (and (has? ?e :Portable) (has? ?e :HP))
        let pred = PredicatePattern::compile_value(
            &builders::and(vec![
                builders::has_component("Portable"),
                builders::has_component("HP"),
            ]),
            "?e",
        )
        .unwrap();

        let lamp = EntityId::from_raw(0);

        // Lamp has Portable but no HP
        assert!(!pred.evaluate(&world, lamp).unwrap());
        // Goblin has both Portable and HP
        assert!(pred.evaluate(&world, goblin).unwrap());
    }

    #[test]
    fn test_not_predicate() {
        let world = setup_world();

        // Predicate: (not (has? ?e :Fixed))
        let pred =
            PredicatePattern::compile_value(&builders::not(builders::has_component("Fixed")), "?e")
                .unwrap();

        let lamp = EntityId::from_raw(0);
        let table = EntityId::from_raw(1);

        assert!(pred.evaluate(&world, lamp).unwrap());
        assert!(!pred.evaluate(&world, table).unwrap());
    }

    #[test]
    fn test_portable_predicate() {
        let world = setup_world();

        // Predicate: (and (has? ?e :Portable) (not (has? ?e :Fixed)))
        let pred = PredicatePattern::compile_value(&builders::is_portable(), "?e").unwrap();

        let lamp = EntityId::from_raw(0);
        let table = EntityId::from_raw(1);

        assert!(pred.evaluate(&world, lamp).unwrap());
        assert!(!pred.evaluate(&world, table).unwrap());
    }

    #[test]
    fn test_filter_with_predicate() {
        let world = setup_world();

        // Get all entities
        let all_entities: Vec<EntityId> = world.all_entities().collect();

        // Filter using HP predicate (creatures)
        let pred = PredicatePattern::compile_value(&builders::is_creature(), "?e").unwrap();

        let creatures: Vec<EntityId> = all_entities.filter_with_predicate(&world, &pred).collect();

        assert_eq!(creatures.len(), 1);
        assert_eq!(creatures[0], EntityId::from_raw(2)); // goblin
    }

    #[test]
    fn test_predicate_source() {
        let pred = PredicatePattern::compile_value(&builders::has_component("Name"), "?e").unwrap();

        assert!(pred.source().contains("has?"));
        assert!(pred.source().contains("Name"));
    }

    #[test]
    fn test_or_predicate() {
        let world = setup_world();

        // Predicate: (or (has? ?e :Portable) (has? ?e :HP))
        let pred = PredicatePattern::compile_value(
            &builders::or(vec![
                builders::has_component("Portable"),
                builders::has_component("HP"),
            ]),
            "?e",
        )
        .unwrap();

        let lamp = EntityId::from_raw(0);
        let table = EntityId::from_raw(1);
        let goblin = EntityId::from_raw(2);

        assert!(pred.evaluate(&world, lamp).unwrap()); // has Portable
        assert!(!pred.evaluate(&world, table).unwrap()); // has neither
        assert!(pred.evaluate(&world, goblin).unwrap()); // has HP
    }

    #[test]
    fn test_compile_from_string() {
        let world = setup_world();

        // Direct string compilation
        let pred = PredicatePattern::compile("(has? ?e :Portable)", "?e").unwrap();

        let lamp = EntityId::from_raw(0);
        assert!(pred.evaluate(&world, lamp).unwrap());
    }
}
