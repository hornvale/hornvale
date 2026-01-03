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
/// Values are immutable and cheap to clone (using Arc for strings and lists).
#[derive(Debug, Clone)]
pub enum Value {
    /// The absence of a value (distinct from Bool(false))
    Nil,
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
    /// List of values (immutable, reference-counted for cheap cloning)
    List(Arc<Vec<Value>>),
}

impl Value {
    /// Check if this value is nil.
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    /// Check if this value is truthy.
    /// Nil and Bool(false) are falsy, everything else is truthy.
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    /// Create a string value.
    pub fn string(s: impl Into<Arc<str>>) -> Self {
        Value::String(s.into())
    }

    /// Create a list value.
    pub fn list(items: Vec<Value>) -> Self {
        Value::List(Arc::new(items))
    }

    /// Create an empty list value.
    pub fn empty_list() -> Self {
        Value::List(Arc::new(Vec::new()))
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

    /// Try to get this value as a list reference.
    pub fn as_list(&self) -> Option<&[Value]> {
        match self {
            Value::List(items) => Some(items),
            _ => None,
        }
    }

    /// Get a human-readable type name.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Nil => "Nil",
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Symbol(_) => "Symbol",
            Value::EntityRef(_) => "EntityRef",
            Value::List(_) => "List",
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Int(n) => write!(f, "{n}"),
            Value::Float(n) => write!(f, "{n}"),
            Value::Bool(b) => write!(f, "{b}"),
            Value::String(s) => write!(f, "\"{s}\""),
            Value::Symbol(s) => write!(f, ":{s}"),
            Value::EntityRef(id) => write!(f, "@{id}"),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
        }
    }
}

// Manual PartialEq to handle float comparison
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
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
            (Value::List(a), Value::List(b)) => a == b,
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
        // Nil sorts first (smallest)
        fn type_order(v: &Value) -> u8 {
            match v {
                Value::Nil => 0,
                Value::Bool(_) => 1,
                Value::Int(_) => 2,
                Value::Float(_) => 3,
                Value::String(_) => 4,
                Value::Symbol(_) => 5,
                Value::EntityRef(_) => 6,
                Value::List(_) => 7,
            }
        }

        match type_order(self).cmp(&type_order(other)) {
            Ordering::Equal => {}
            ord => return ord,
        }

        // Same type, compare values
        match (self, other) {
            (Value::Nil, Value::Nil) => Ordering::Equal,
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
            (Value::List(a), Value::List(b)) => a.cmp(b),
            _ => unreachable!("type_order should have handled mismatched types"),
        }
    }
}

impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        match self {
            Value::Nil => {} // discriminant is enough
            Value::Int(n) => n.hash(state),
            Value::Float(f) => f.to_bits().hash(state),
            Value::Bool(b) => b.hash(state),
            Value::String(s) => s.hash(state),
            Value::Symbol(s) => s.hash(state),
            Value::EntityRef(id) => id.hash(state),
            Value::List(items) => items.hash(state),
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

impl From<Vec<Value>> for Value {
    fn from(items: Vec<Value>) -> Self {
        Value::List(Arc::new(items))
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
        // Different types: Nil < Bool < Int < Float < String < Symbol < EntityRef < List
        assert!(Value::Nil < Value::Bool(false));
        assert!(Value::Bool(true) < Value::Int(0));
        assert!(Value::Int(100) < Value::Float(0.0));
        assert!(Value::Float(100.0) < Value::string("a"));

        // Same type
        assert!(Value::Int(1) < Value::Int(2));
        assert!(Value::string("a") < Value::string("b"));
    }

    #[test]
    fn test_nil() {
        // Nil is equal to itself
        assert_eq!(Value::Nil, Value::Nil);

        // Nil is distinct from Bool(false)
        assert_ne!(Value::Nil, Value::Bool(false));

        // Nil display
        assert_eq!(Value::Nil.to_string(), "nil");

        // Nil type name
        assert_eq!(Value::Nil.type_name(), "Nil");

        // is_nil
        assert!(Value::Nil.is_nil());
        assert!(!Value::Bool(false).is_nil());
    }

    #[test]
    fn test_is_truthy() {
        // Nil and Bool(false) are falsy
        assert!(!Value::Nil.is_truthy());
        assert!(!Value::Bool(false).is_truthy());

        // Everything else is truthy
        assert!(Value::Bool(true).is_truthy());
        assert!(Value::Int(0).is_truthy()); // Int(0) is truthy!
        assert!(Value::Int(42).is_truthy());
        assert!(Value::string("").is_truthy()); // Empty string is truthy
        assert!(Value::empty_list().is_truthy()); // Empty list is truthy
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

    #[test]
    fn test_list_display() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(list.to_string(), "[1 2 3]");

        let empty = Value::empty_list();
        assert_eq!(empty.to_string(), "[]");
    }

    #[test]
    fn test_list_equality() {
        let list1 = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let list2 = Value::list(vec![Value::Int(1), Value::Int(2)]);
        let list3 = Value::list(vec![Value::Int(1), Value::Int(3)]);

        assert_eq!(list1, list2);
        assert_ne!(list1, list3);
    }

    #[test]
    fn test_list_accessor() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        assert!(list.as_list().is_some());
        assert_eq!(list.as_list().unwrap().len(), 2);

        let int_val = Value::Int(42);
        assert!(int_val.as_list().is_none());
    }

    #[test]
    fn test_list_ordering() {
        // Lists sort after EntityRef
        assert!(Value::EntityRef(EntityId::from_raw(0)) < Value::empty_list());

        // Lists compare element-wise
        let list1 = Value::list(vec![Value::Int(1)]);
        let list2 = Value::list(vec![Value::Int(2)]);
        assert!(list1 < list2);
    }
}
