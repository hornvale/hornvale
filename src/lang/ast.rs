//! Abstract Syntax Tree for S-expressions.

use super::error::Span;
use crate::symbol::Symbol;
use std::fmt;

/// An atomic value in an S-expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    /// Nil literal (absence of value).
    Nil,
    /// Integer literal.
    Int(i64),
    /// Floating-point literal.
    Float(f64),
    /// Boolean literal (parsed from `true`/`false` symbols).
    Bool(bool),
    /// String literal.
    String(String),
    /// Symbol (identifier).
    Symbol(Symbol),
    /// Keyword (colon-prefixed symbol).
    Keyword(Symbol),
}

impl Atom {
    /// Get the type name of this atom.
    pub fn type_name(&self) -> &'static str {
        match self {
            Atom::Nil => "nil",
            Atom::Int(_) => "int",
            Atom::Float(_) => "float",
            Atom::Bool(_) => "bool",
            Atom::String(_) => "string",
            Atom::Symbol(_) => "symbol",
            Atom::Keyword(_) => "keyword",
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Nil => write!(f, "nil"),
            Atom::Int(n) => write!(f, "{n}"),
            Atom::Float(n) => write!(f, "{n}"),
            Atom::Bool(b) => write!(f, "{b}"),
            Atom::String(s) => write!(f, "\"{s}\""),
            Atom::Symbol(s) => write!(f, "{}", s.as_str()),
            Atom::Keyword(k) => write!(f, ":{}", k.as_str()),
        }
    }
}

/// An S-expression.
#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    /// Atomic value.
    Atom(Atom, Span),
    /// List of expressions: `(a b c)`
    List(Vec<SExpr>, Span),
}

impl SExpr {
    // --- Constructors ---

    /// Create an integer atom.
    pub fn int(n: i64, span: Span) -> Self {
        SExpr::Atom(Atom::Int(n), span)
    }

    /// Create a float atom.
    pub fn float(n: f64, span: Span) -> Self {
        SExpr::Atom(Atom::Float(n), span)
    }

    /// Create a boolean atom.
    pub fn bool(b: bool, span: Span) -> Self {
        SExpr::Atom(Atom::Bool(b), span)
    }

    /// Create a string atom.
    pub fn string(s: impl Into<String>, span: Span) -> Self {
        SExpr::Atom(Atom::String(s.into()), span)
    }

    /// Create a symbol atom.
    pub fn symbol(s: impl AsRef<str>, span: Span) -> Self {
        SExpr::Atom(Atom::Symbol(Symbol::new(s.as_ref())), span)
    }

    /// Create a keyword atom.
    pub fn keyword(s: impl AsRef<str>, span: Span) -> Self {
        SExpr::Atom(Atom::Keyword(Symbol::new(s.as_ref())), span)
    }

    /// Create a list.
    pub fn list(items: Vec<SExpr>, span: Span) -> Self {
        SExpr::List(items, span)
    }

    // --- Accessors ---

    /// Get the span of this expression.
    pub fn span(&self) -> Span {
        match self {
            SExpr::Atom(_, span) => *span,
            SExpr::List(_, span) => *span,
        }
    }

    /// Try to get as an atom.
    pub fn as_atom(&self) -> Option<&Atom> {
        match self {
            SExpr::Atom(a, _) => Some(a),
            SExpr::List(_, _) => None,
        }
    }

    /// Try to get as a list.
    pub fn as_list(&self) -> Option<&[SExpr]> {
        match self {
            SExpr::Atom(_, _) => None,
            SExpr::List(items, _) => Some(items),
        }
    }

    /// Try to get as a mutable list.
    pub fn as_list_mut(&mut self) -> Option<&mut Vec<SExpr>> {
        match self {
            SExpr::Atom(_, _) => None,
            SExpr::List(items, _) => Some(items),
        }
    }

    /// Try to get as an integer.
    pub fn as_int(&self) -> Option<i64> {
        match self {
            SExpr::Atom(Atom::Int(n), _) => Some(*n),
            _ => None,
        }
    }

    /// Try to get as a float.
    pub fn as_float(&self) -> Option<f64> {
        match self {
            SExpr::Atom(Atom::Float(n), _) => Some(*n),
            _ => None,
        }
    }

    /// Try to get as a number (int or float).
    pub fn as_number(&self) -> Option<f64> {
        match self {
            SExpr::Atom(Atom::Int(n), _) => Some(*n as f64),
            SExpr::Atom(Atom::Float(n), _) => Some(*n),
            _ => None,
        }
    }

    /// Try to get as a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            SExpr::Atom(Atom::Bool(b), _) => Some(*b),
            _ => None,
        }
    }

    /// Try to get as a string.
    pub fn as_string(&self) -> Option<&str> {
        match self {
            SExpr::Atom(Atom::String(s), _) => Some(s),
            _ => None,
        }
    }

    /// Try to get as a symbol.
    pub fn as_symbol(&self) -> Option<Symbol> {
        match self {
            SExpr::Atom(Atom::Symbol(s), _) => Some(*s),
            _ => None,
        }
    }

    /// Try to get as a keyword.
    pub fn as_keyword(&self) -> Option<Symbol> {
        match self {
            SExpr::Atom(Atom::Keyword(k), _) => Some(*k),
            _ => None,
        }
    }

    /// Check if this is a symbol with a specific name.
    pub fn is_symbol(&self, name: &str) -> bool {
        self.as_symbol().is_some_and(|s| s.as_str() == name)
    }

    /// Check if this is a keyword with a specific name.
    pub fn is_keyword(&self, name: &str) -> bool {
        self.as_keyword().is_some_and(|k| k.as_str() == name)
    }

    /// Check if this is an empty list.
    pub fn is_empty_list(&self) -> bool {
        matches!(self, SExpr::List(items, _) if items.is_empty())
    }

    /// Check if this is a list starting with a specific symbol.
    pub fn is_call(&self, name: &str) -> bool {
        match self {
            SExpr::List(items, _) if !items.is_empty() => items[0].is_symbol(name),
            _ => false,
        }
    }

    /// Get the type name for error messages.
    pub fn type_name(&self) -> &'static str {
        match self {
            SExpr::Atom(a, _) => a.type_name(),
            SExpr::List(_, _) => "list",
        }
    }
}

impl fmt::Display for SExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SExpr::Atom(a, _) => write!(f, "{a}"),
            SExpr::List(items, _) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::default()
    }

    #[test]
    fn test_atom_display() {
        assert_eq!(SExpr::int(42, span()).to_string(), "42");
        assert_eq!(SExpr::float(3.14, span()).to_string(), "3.14");
        assert_eq!(SExpr::bool(true, span()).to_string(), "true");
        assert_eq!(SExpr::string("hello", span()).to_string(), "\"hello\"");
        assert_eq!(SExpr::symbol("foo", span()).to_string(), "foo");
        assert_eq!(SExpr::keyword("bar", span()).to_string(), ":bar");
    }

    #[test]
    fn test_list_display() {
        let list = SExpr::list(
            vec![
                SExpr::symbol("+", span()),
                SExpr::int(1, span()),
                SExpr::int(2, span()),
            ],
            span(),
        );
        assert_eq!(list.to_string(), "(+ 1 2)");
    }

    #[test]
    fn test_nested_list_display() {
        let inner = SExpr::list(
            vec![SExpr::symbol("*", span()), SExpr::int(2, span())],
            span(),
        );
        let outer = SExpr::list(
            vec![SExpr::symbol("+", span()), SExpr::int(1, span()), inner],
            span(),
        );
        assert_eq!(outer.to_string(), "(+ 1 (* 2))");
    }

    #[test]
    fn test_as_int() {
        assert_eq!(SExpr::int(42, span()).as_int(), Some(42));
        assert_eq!(SExpr::float(3.14, span()).as_int(), None);
        assert_eq!(SExpr::list(vec![], span()).as_int(), None);
    }

    #[test]
    fn test_as_number() {
        assert_eq!(SExpr::int(42, span()).as_number(), Some(42.0));
        assert_eq!(SExpr::float(3.14, span()).as_number(), Some(3.14));
        assert_eq!(SExpr::bool(true, span()).as_number(), None);
    }

    #[test]
    fn test_as_list() {
        let list = SExpr::list(vec![SExpr::int(1, span())], span());
        assert!(list.as_list().is_some());
        assert_eq!(list.as_list().unwrap().len(), 1);

        assert!(SExpr::int(42, span()).as_list().is_none());
    }

    #[test]
    fn test_is_symbol() {
        assert!(SExpr::symbol("foo", span()).is_symbol("foo"));
        assert!(!SExpr::symbol("foo", span()).is_symbol("bar"));
        assert!(!SExpr::int(42, span()).is_symbol("foo"));
    }

    #[test]
    fn test_is_keyword() {
        assert!(SExpr::keyword("foo", span()).is_keyword("foo"));
        assert!(!SExpr::keyword("foo", span()).is_keyword("bar"));
        assert!(!SExpr::symbol("foo", span()).is_keyword("foo"));
    }

    #[test]
    fn test_is_call() {
        let call = SExpr::list(
            vec![SExpr::symbol("+", span()), SExpr::int(1, span())],
            span(),
        );
        assert!(call.is_call("+"));
        assert!(!call.is_call("-"));

        assert!(!SExpr::list(vec![], span()).is_call("+"));
        assert!(!SExpr::int(42, span()).is_call("+"));
    }

    #[test]
    fn test_type_name() {
        assert_eq!(SExpr::int(42, span()).type_name(), "int");
        assert_eq!(SExpr::float(3.14, span()).type_name(), "float");
        assert_eq!(SExpr::bool(true, span()).type_name(), "bool");
        assert_eq!(SExpr::string("hi", span()).type_name(), "string");
        assert_eq!(SExpr::symbol("x", span()).type_name(), "symbol");
        assert_eq!(SExpr::keyword("k", span()).type_name(), "keyword");
        assert_eq!(SExpr::list(vec![], span()).type_name(), "list");
    }
}
