//! User-defined functions for the DSL.
//!
//! Functions are defined with `(defun name (params...) body...)` and called
//! like any other expression. Currently implemented via inline expansion
//! (compile-time substitution) rather than runtime call frames.
//!
//! # Example
//!
//! ```lisp
//! (defun greet (name)
//!   (str "Hello, " name "!"))
//!
//! (greet "World")  ; => "Hello, World!"
//! ```
//!
//! # Limitations
//!
//! - No recursion (would cause infinite expansion)
//! - No mutual recursion
//! - Functions must be defined before use

use crate::lang::SExpr;
use crate::symbol::Symbol;
use im::OrdMap;

/// A user-defined function.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    /// Function name.
    pub name: Symbol,
    /// Parameter names.
    pub params: Vec<Symbol>,
    /// Function body (AST, not yet compiled).
    pub body: SExpr,
}

impl FunctionDef {
    /// Create a new function definition.
    pub fn new(name: Symbol, params: Vec<Symbol>, body: SExpr) -> Self {
        Self { name, params, body }
    }

    /// Get the function's arity (number of parameters).
    pub fn arity(&self) -> usize {
        self.params.len()
    }
}

/// Registry of user-defined functions.
#[derive(Debug, Clone, Default)]
pub struct FunctionRegistry {
    functions: OrdMap<Symbol, FunctionDef>,
}

impl FunctionRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self {
            functions: OrdMap::new(),
        }
    }

    /// Define a function.
    pub fn define(&mut self, func: FunctionDef) {
        self.functions.insert(func.name, func);
    }

    /// Look up a function by name.
    pub fn get(&self, name: Symbol) -> Option<&FunctionDef> {
        self.functions.get(&name)
    }

    /// Check if a function is defined.
    pub fn contains(&self, name: Symbol) -> bool {
        self.functions.contains_key(&name)
    }

    /// Get the number of defined functions.
    pub fn len(&self) -> usize {
        self.functions.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }

    /// Iterate over all functions.
    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &FunctionDef)> {
        self.functions.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::{Atom, Span};

    fn make_span() -> Span {
        Span::new(0, 1, 1, 1)
    }

    #[test]
    fn test_function_def_basic() {
        let name = Symbol::new("greet");
        let params = vec![Symbol::new("x")];
        let body = SExpr::Atom(Atom::Symbol(Symbol::new("x")), make_span());

        let func = FunctionDef::new(name, params, body);

        assert_eq!(func.name, Symbol::new("greet"));
        assert_eq!(func.arity(), 1);
    }

    #[test]
    fn test_function_registry() {
        let mut registry = FunctionRegistry::new();

        let func = FunctionDef::new(
            Symbol::new("add1"),
            vec![Symbol::new("x")],
            SExpr::Atom(Atom::Int(1), make_span()),
        );

        assert!(!registry.contains(Symbol::new("add1")));
        registry.define(func);
        assert!(registry.contains(Symbol::new("add1")));
        assert_eq!(registry.len(), 1);

        let retrieved = registry.get(Symbol::new("add1")).unwrap();
        assert_eq!(retrieved.arity(), 1);
    }
}
