//! Standard library functions for the VM.

use crate::core::Value;
use crate::symbol::Symbol;
use im::OrdMap;

/// Error from a stdlib function.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum StdLibError {
    #[error("type error: expected {expected}, got {got}")]
    TypeError {
        expected: &'static str,
        got: &'static str,
    },

    #[error("arity error: expected {expected} arguments, got {got}")]
    ArityError { expected: usize, got: usize },

    #[error("value error: {0}")]
    ValueError(String),
}

/// Signature for a standard library function.
pub type StdFn = fn(&[Value]) -> Result<Value, StdLibError>;

/// Standard library registry.
#[derive(Clone)]
pub struct StdLib {
    functions: OrdMap<Symbol, (StdFn, &'static str)>,
}

impl StdLib {
    /// Create an empty standard library.
    pub fn new() -> Self {
        Self {
            functions: OrdMap::new(),
        }
    }

    /// Create a standard library with all built-in functions.
    pub fn with_builtins() -> Self {
        let mut lib = Self::new();

        // Math functions
        lib.register("abs", "abs(x) -> number", stdlib_abs);
        lib.register("sqrt", "sqrt(x) -> float", stdlib_sqrt);
        lib.register("floor", "floor(x) -> int", stdlib_floor);
        lib.register("ceil", "ceil(x) -> int", stdlib_ceil);
        lib.register("round", "round(x) -> int", stdlib_round);
        lib.register("min", "min(a, b) -> number", stdlib_min);
        lib.register("max", "max(a, b) -> number", stdlib_max);

        // String functions
        lib.register(
            "string-length",
            "string-length(s) -> int",
            stdlib_string_length,
        );
        lib.register(
            "string-concat",
            "string-concat(a, b) -> string",
            stdlib_string_concat,
        );
        lib.register(
            "string-upper",
            "string-upper(s) -> string",
            stdlib_string_upper,
        );
        lib.register(
            "string-lower",
            "string-lower(s) -> string",
            stdlib_string_lower,
        );

        // Type functions
        lib.register("type", "type(x) -> string", stdlib_type);
        lib.register("int", "int(x) -> int", stdlib_int);
        lib.register("float", "float(x) -> float", stdlib_float);
        lib.register("string", "string(x) -> string", stdlib_string);

        // List functions
        lib.register("length", "length(list) -> int", stdlib_length);
        lib.register("first", "first(list) -> value", stdlib_first);
        lib.register("rest", "rest(list) -> list", stdlib_rest);
        lib.register("nth", "nth(list, index) -> value", stdlib_nth);
        lib.register("empty?", "empty?(list) -> bool", stdlib_empty);
        lib.register("cons", "cons(value, list) -> list", stdlib_cons);

        lib
    }

    /// Register a function.
    pub fn register(&mut self, name: &str, doc: &'static str, func: StdFn) {
        self.functions.insert(Symbol::new(name), (func, doc));
    }

    /// Get a function by name.
    pub fn get(&self, name: Symbol) -> Option<StdFn> {
        self.functions.get(&name).map(|(f, _)| *f)
    }

    /// Get a function by string name.
    pub fn get_by_name(&self, name: &str) -> Option<StdFn> {
        self.get(Symbol::new(name))
    }

    /// Get the documentation for a function.
    pub fn get_doc(&self, name: Symbol) -> Option<&'static str> {
        self.functions.get(&name).map(|(_, doc)| *doc)
    }

    /// List all function names.
    pub fn list(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.functions.keys().copied()
    }

    /// Number of registered functions.
    pub fn len(&self) -> usize {
        self.functions.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.functions.is_empty()
    }
}

impl Default for StdLib {
    fn default() -> Self {
        Self::with_builtins()
    }
}

impl std::fmt::Debug for StdLib {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StdLib")
            .field("functions", &self.functions.len())
            .finish()
    }
}

// === Math functions ===

fn stdlib_abs(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(n) => Ok(Value::Float(n.abs())),
        other => Err(StdLibError::TypeError {
            expected: "number",
            got: other.type_name(),
        }),
    }
}

fn stdlib_sqrt(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let n = as_float(&args[0])?;
    if n < 0.0 {
        Err(StdLibError::ValueError(
            "sqrt of negative number".to_string(),
        ))
    } else {
        Ok(Value::Float(n.sqrt()))
    }
}

fn stdlib_floor(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let n = as_float(&args[0])?;
    Ok(Value::Int(n.floor() as i64))
}

fn stdlib_ceil(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let n = as_float(&args[0])?;
    Ok(Value::Int(n.ceil() as i64))
}

fn stdlib_round(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let n = as_float(&args[0])?;
    Ok(Value::Int(n.round() as i64))
}

fn stdlib_min(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 2)?;
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
        _ => Err(StdLibError::TypeError {
            expected: "numbers",
            got: "non-numeric types",
        }),
    }
}

fn stdlib_max(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 2)?;
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
        _ => Err(StdLibError::TypeError {
            expected: "numbers",
            got: "non-numeric types",
        }),
    }
}

// === String functions ===

fn stdlib_string_length(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let s = as_string(&args[0])?;
    Ok(Value::Int(s.len() as i64))
}

fn stdlib_string_concat(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 2)?;
    let a = as_string(&args[0])?;
    let b = as_string(&args[1])?;
    Ok(Value::string(format!("{a}{b}")))
}

fn stdlib_string_upper(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let s = as_string(&args[0])?;
    Ok(Value::string(s.to_uppercase()))
}

fn stdlib_string_lower(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let s = as_string(&args[0])?;
    Ok(Value::string(s.to_lowercase()))
}

// === Type functions ===

fn stdlib_type(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    Ok(Value::string(args[0].type_name()))
}

fn stdlib_int(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)),
        Value::Float(n) => Ok(Value::Int(*n as i64)),
        Value::Bool(b) => Ok(Value::Int(if *b { 1 } else { 0 })),
        Value::String(s) => s
            .parse::<i64>()
            .map(Value::Int)
            .map_err(|_| StdLibError::ValueError(format!("cannot convert '{s}' to int"))),
        other => Err(StdLibError::TypeError {
            expected: "convertible to int",
            got: other.type_name(),
        }),
    }
}

fn stdlib_float(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(n) => Ok(Value::Float(*n)),
        Value::Bool(b) => Ok(Value::Float(if *b { 1.0 } else { 0.0 })),
        Value::String(s) => s
            .parse::<f64>()
            .map(Value::Float)
            .map_err(|_| StdLibError::ValueError(format!("cannot convert '{s}' to float"))),
        other => Err(StdLibError::TypeError {
            expected: "convertible to float",
            got: other.type_name(),
        }),
    }
}

fn stdlib_string(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.clone())),
        other => Ok(Value::string(format!("{other}"))),
    }
}

// === List functions ===

fn stdlib_length(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::List(items) => Ok(Value::Int(items.len() as i64)),
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        other => Err(StdLibError::TypeError {
            expected: "list or string",
            got: other.type_name(),
        }),
    }
}

fn stdlib_first(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let items = as_list(&args[0])?;
    items
        .first()
        .cloned()
        .ok_or_else(|| StdLibError::ValueError("empty list".to_string()))
}

fn stdlib_rest(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    let items = as_list(&args[0])?;
    if items.is_empty() {
        Ok(Value::empty_list())
    } else {
        Ok(Value::list(items[1..].to_vec()))
    }
}

fn stdlib_nth(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 2)?;
    let items = as_list(&args[0])?;
    let index = as_int(&args[1])? as usize;
    items.get(index).cloned().ok_or_else(|| {
        StdLibError::ValueError(format!(
            "index {index} out of bounds (length {})",
            items.len()
        ))
    })
}

fn stdlib_empty(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 1)?;
    match &args[0] {
        Value::List(items) => Ok(Value::Bool(items.is_empty())),
        Value::String(s) => Ok(Value::Bool(s.is_empty())),
        other => Err(StdLibError::TypeError {
            expected: "list or string",
            got: other.type_name(),
        }),
    }
}

fn stdlib_cons(args: &[Value]) -> Result<Value, StdLibError> {
    check_arity(args, 2)?;
    let item = args[0].clone();
    let items = as_list(&args[1])?;
    let mut new_items = vec![item];
    new_items.extend(items.iter().cloned());
    Ok(Value::list(new_items))
}

// === Helpers ===

fn check_arity(args: &[Value], expected: usize) -> Result<(), StdLibError> {
    if args.len() != expected {
        Err(StdLibError::ArityError {
            expected,
            got: args.len(),
        })
    } else {
        Ok(())
    }
}

fn as_float(value: &Value) -> Result<f64, StdLibError> {
    match value {
        Value::Int(n) => Ok(*n as f64),
        Value::Float(n) => Ok(*n),
        other => Err(StdLibError::TypeError {
            expected: "number",
            got: other.type_name(),
        }),
    }
}

fn as_string(value: &Value) -> Result<&str, StdLibError> {
    match value {
        Value::String(s) => Ok(s),
        other => Err(StdLibError::TypeError {
            expected: "string",
            got: other.type_name(),
        }),
    }
}

fn as_list(value: &Value) -> Result<&[Value], StdLibError> {
    match value {
        Value::List(items) => Ok(items),
        other => Err(StdLibError::TypeError {
            expected: "list",
            got: other.type_name(),
        }),
    }
}

fn as_int(value: &Value) -> Result<i64, StdLibError> {
    match value {
        Value::Int(n) => Ok(*n),
        other => Err(StdLibError::TypeError {
            expected: "int",
            got: other.type_name(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stdlib_has_builtins() {
        let lib = StdLib::with_builtins();
        assert!(lib.get_by_name("abs").is_some());
        assert!(lib.get_by_name("sqrt").is_some());
        assert!(lib.get_by_name("min").is_some());
        assert!(lib.get_by_name("max").is_some());
        assert!(lib.get_by_name("string-length").is_some());
    }

    #[test]
    fn test_abs() {
        let abs = stdlib_abs;
        assert_eq!(abs(&[Value::Int(-5)]).unwrap(), Value::Int(5));
        assert_eq!(abs(&[Value::Int(5)]).unwrap(), Value::Int(5));
        assert_eq!(abs(&[Value::Float(-3.5)]).unwrap(), Value::Float(3.5));
    }

    #[test]
    fn test_sqrt() {
        let sqrt = stdlib_sqrt;
        assert_eq!(sqrt(&[Value::Int(4)]).unwrap(), Value::Float(2.0));
        assert_eq!(sqrt(&[Value::Float(9.0)]).unwrap(), Value::Float(3.0));
        assert!(sqrt(&[Value::Int(-1)]).is_err());
    }

    #[test]
    fn test_floor_ceil_round() {
        assert_eq!(stdlib_floor(&[Value::Float(3.7)]).unwrap(), Value::Int(3));
        assert_eq!(stdlib_ceil(&[Value::Float(3.2)]).unwrap(), Value::Int(4));
        assert_eq!(stdlib_round(&[Value::Float(3.5)]).unwrap(), Value::Int(4));
        assert_eq!(stdlib_round(&[Value::Float(3.4)]).unwrap(), Value::Int(3));
    }

    #[test]
    fn test_min_max() {
        assert_eq!(
            stdlib_min(&[Value::Int(3), Value::Int(5)]).unwrap(),
            Value::Int(3)
        );
        assert_eq!(
            stdlib_max(&[Value::Int(3), Value::Int(5)]).unwrap(),
            Value::Int(5)
        );
        assert_eq!(
            stdlib_min(&[Value::Float(3.5), Value::Int(4)]).unwrap(),
            Value::Float(3.5)
        );
    }

    #[test]
    fn test_string_length() {
        assert_eq!(
            stdlib_string_length(&[Value::string("hello")]).unwrap(),
            Value::Int(5)
        );
        assert_eq!(
            stdlib_string_length(&[Value::string("")]).unwrap(),
            Value::Int(0)
        );
    }

    #[test]
    fn test_string_concat() {
        assert_eq!(
            stdlib_string_concat(&[Value::string("hello"), Value::string(" world")]).unwrap(),
            Value::string("hello world")
        );
    }

    #[test]
    fn test_string_case() {
        assert_eq!(
            stdlib_string_upper(&[Value::string("hello")]).unwrap(),
            Value::string("HELLO")
        );
        assert_eq!(
            stdlib_string_lower(&[Value::string("HELLO")]).unwrap(),
            Value::string("hello")
        );
    }

    #[test]
    fn test_type() {
        assert_eq!(
            stdlib_type(&[Value::Int(42)]).unwrap(),
            Value::string("Int")
        );
        assert_eq!(
            stdlib_type(&[Value::string("hi")]).unwrap(),
            Value::string("String")
        );
    }

    #[test]
    fn test_int_conversion() {
        assert_eq!(stdlib_int(&[Value::Float(3.7)]).unwrap(), Value::Int(3));
        assert_eq!(stdlib_int(&[Value::Bool(true)]).unwrap(), Value::Int(1));
        assert_eq!(stdlib_int(&[Value::string("42")]).unwrap(), Value::Int(42));
    }

    #[test]
    fn test_float_conversion() {
        assert_eq!(stdlib_float(&[Value::Int(3)]).unwrap(), Value::Float(3.0));
        assert_eq!(
            stdlib_float(&[Value::string("3.14")]).unwrap(),
            Value::Float(3.14)
        );
    }

    #[test]
    fn test_arity_error() {
        let result = stdlib_abs(&[]);
        assert!(matches!(result, Err(StdLibError::ArityError { .. })));
    }

    #[test]
    fn test_type_error() {
        let result = stdlib_abs(&[Value::string("not a number")]);
        assert!(matches!(result, Err(StdLibError::TypeError { .. })));
    }

    #[test]
    fn test_list_length() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert_eq!(stdlib_length(&[list]).unwrap(), Value::Int(3));
        assert_eq!(
            stdlib_length(&[Value::empty_list()]).unwrap(),
            Value::Int(0)
        );
    }

    #[test]
    fn test_list_first() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2)]);
        assert_eq!(stdlib_first(&[list]).unwrap(), Value::Int(1));
        assert!(stdlib_first(&[Value::empty_list()]).is_err());
    }

    #[test]
    fn test_list_rest() {
        let list = Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        let rest = stdlib_rest(&[list]).unwrap();
        assert_eq!(rest, Value::list(vec![Value::Int(2), Value::Int(3)]));

        let single = Value::list(vec![Value::Int(1)]);
        assert_eq!(stdlib_rest(&[single]).unwrap(), Value::empty_list());
    }

    #[test]
    fn test_list_nth() {
        let list = Value::list(vec![Value::Int(10), Value::Int(20), Value::Int(30)]);
        assert_eq!(
            stdlib_nth(&[list.clone(), Value::Int(0)]).unwrap(),
            Value::Int(10)
        );
        assert_eq!(
            stdlib_nth(&[list.clone(), Value::Int(2)]).unwrap(),
            Value::Int(30)
        );
        assert!(stdlib_nth(&[list, Value::Int(5)]).is_err());
    }

    #[test]
    fn test_list_empty() {
        assert_eq!(
            stdlib_empty(&[Value::empty_list()]).unwrap(),
            Value::Bool(true)
        );
        let list = Value::list(vec![Value::Int(1)]);
        assert_eq!(stdlib_empty(&[list]).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_list_cons() {
        let list = Value::list(vec![Value::Int(2), Value::Int(3)]);
        let result = stdlib_cons(&[Value::Int(1), list]).unwrap();
        assert_eq!(
            result,
            Value::list(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }
}
