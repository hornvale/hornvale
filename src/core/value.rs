//! Runtime values for the world VM.
//!
//! The `Value` type represents all data that can be stored in components
//! or passed around in the system.

use std::cmp::Ordering;
use std::sync::Arc;

use crate::core::EntityId;
use crate::symbol::Symbol;

/// A runtime value in the world VM.
///
/// Values are immutable and cheap to clone (using Arc for strings).
/// For Phase 1, we support basic types only; collections come later.
#[derive(Debug, Clone)]
pub enum Value {
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit floating point (NaN is treated as equal to itself for consistency)
    Float(f64),
    /// Boolean
    Bool(bool),
    /// Immutable string
    String(Arc<str>),
    /// Interned symbol
    Symbol(Symbol),
    /// Reference to an entity
    EntityRef(EntityId),
}

impl Value {
    /// Create a string value.
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Value::String(s.into())
    }

    /// Try to get this value as a string reference.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Value::String(s) => Some(s),
            _ => None,
        }
    }

    /// Try to get this value as an integer.
    pub fn as_int(&self) -> Option<i64> {
        match self {
            Value::Int(n) => Some(*n),
            _ => None,
        }
    }

    /// Try to get this value as a boolean.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Try to get this value as an entity reference.
    pub fn as_entity_ref(&self) -> Option<EntityId> {
        match self {
            Value::EntityRef(id) => Some(*id),
            _ => None,
        }
    }

    /// Get a human-readable type name.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Symbol(_) => "Symbol",
            Value::EntityRef(_) => "EntityRef",
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, ":{s}"),
            Value::EntityRef(id) => write!(f, "@{id}"),
        }
    }
}

// Manual PartialEq to handle float comparison
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => {
                // Treat NaN as equal to NaN for consistency
                if a.is_nan() && b.is_nan() {
                    true
                } else {
                    a == b
                }
            }
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::EntityRef(a), Value::EntityRef(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

// Manual Ord implementation for deterministic ordering
impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        // Order by type first, then by value
        fn type_order(v: &Value) -> u8 {
            match v {
                Value::Bool(_) => 0,
                Value::Int(_) => 1,
                Value::Float(_) => 2,
                Value::String(_) => 3,
                Value::Symbol(_) => 4,
                Value::EntityRef(_) => 5,
            }
        }

        match type_order(self).cmp(&type_order(other)) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // Same type, compare values
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a.cmp(b),
            (Value::Int(a), Value::Int(b)) => a.cmp(b),
            (Value::Float(a), Value::Float(b)) => {
                // Total ordering for floats: NaN sorts last
                match (a.is_nan(), b.is_nan()) {
                    (true, true) => Ordering::Equal,
                    (true, false) => Ordering::Greater,
                    (false, true) => Ordering::Less,
                    (false, false) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
                }
            }
            (Value::String(a), Value::String(b)) => a.cmp(b),
            (Value::Symbol(a), Value::Symbol(b)) => a.cmp(b),
            (Value::EntityRef(a), Value::EntityRef(b)) => a.cmp(b),
            _ => unreachable!("type_order should have handled mismatched types"),
        }
    }
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Int(n) => n.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::Bool(b) => b.hash(state),
            Value::String(s) => s.hash(state),
            Value::Symbol(s) => s.hash(state),
            Value::EntityRef(id) => id.hash(state),
        }
    }
}

// Convenient From implementations
impl From<i64> for Value {
    fn from(n: i64) -> Self {
        Value::Int(n)
    }
}

impl From<i32> for Value {
    fn from(n: i32) -> Self {
        Value::Int(n as i64)
    }
}

impl From<f64> for Value {
    fn from(n: f64) -> Self {
        Value::Float(n)
    }
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        Value::Bool(b)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::String(s.into())
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::String(s.into())
    }
}

impl From<Symbol> for Value {
    fn from(s: Symbol) -> Self {
        Value::Symbol(s)
    }
}

impl From<EntityId> for Value {
    fn from(id: EntityId) -> Self {
        Value::EntityRef(id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_display() {
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::string("hello").to_string(), "\"hello\"");
    }

    #[test]
    fn test_value_equality() {
        assert_eq!(Value::Int(42), Value::Int(42));
        assert_ne!(Value::Int(42), Value::Int(43));
        assert_ne!(Value::Int(42), Value::Bool(true));
    }

    #[test]
    fn test_float_nan_equality() {
        let nan1 = Value::Float(f64::NAN);
        let nan2 = Value::Float(f64::NAN);
        assert_eq!(nan1, nan2);
    }

    #[test]
    fn test_value_ordering() {
        // Different types: Bool < Int < Float < String < Symbol < EntityRef
        assert!(Value::Bool(true) < Value::Int(0));
        assert!(Value::Int(100) < Value::Float(0.0));
        assert!(Value::Float(100.0) < Value::string("a"));

        // Same type
        assert!(Value::Int(1) < Value::Int(2));
        assert!(Value::string("a") < Value::string("b"));
    }

    #[test]
    fn test_value_accessors() {
        let v = Value::string("test");
        assert_eq!(v.as_str(), Some("test"));
        assert_eq!(v.as_int(), None);

        let v = Value::Int(42);
        assert_eq!(v.as_int(), Some(42));
        assert_eq!(v.as_str(), None);
    }
}
