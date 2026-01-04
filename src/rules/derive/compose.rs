//! Value composition for derivation rules.
//!
//! When multiple rules derive the same property, their contributions
//! are combined according to composition modes.

use crate::core::Value;

/// How multiple rules contributing to the same property are combined.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ComposeMode {
    /// Sum contributions (for numeric values).
    #[default]
    Add,
    /// Multiply contributions (for scaling factors).
    Multiply,
    /// Take the maximum value.
    Max,
    /// Take the minimum value.
    Min,
    /// Highest priority wins (ties go to first rule).
    Override,
    /// Override only if the new value is higher.
    OverrideIfHigher,
    /// Override only if the new value is lower.
    OverrideIfLower,
}

/// A single contribution to a derived value.
#[derive(Debug, Clone)]
pub struct Contribution {
    pub value: Value,
    pub mode: ComposeMode,
    pub priority: i32,
}

impl Contribution {
    /// Create a new contribution.
    pub fn new(value: impl Into<Value>, mode: ComposeMode, priority: i32) -> Self {
        Contribution {
            value: value.into(),
            mode,
            priority,
        }
    }
}

/// Compose multiple contributions into a final value.
///
/// Returns `None` if there are no contributions or if composition fails
/// due to type mismatches.
///
/// Composition order:
/// 1. Override modes are evaluated first (highest priority wins)
/// 2. Additive modes (Add) are summed
/// 3. Multiplicative modes (Multiply) are applied
/// 4. Max/Min are applied to the result
pub fn compose_values(contributions: Vec<Contribution>) -> Option<Value> {
    if contributions.is_empty() {
        return None;
    }

    // Single contribution: return as-is
    if contributions.len() == 1 {
        return Some(contributions.into_iter().next().unwrap().value);
    }

    // Separate by mode
    let mut overrides: Vec<Contribution> = Vec::new();
    let mut override_if_higher: Vec<Contribution> = Vec::new();
    let mut override_if_lower: Vec<Contribution> = Vec::new();
    let mut adds: Vec<Value> = Vec::new();
    let mut multiplies: Vec<Value> = Vec::new();
    let mut maxes: Vec<Value> = Vec::new();
    let mut mins: Vec<Value> = Vec::new();

    for c in contributions {
        match c.mode {
            ComposeMode::Override => overrides.push(c),
            ComposeMode::OverrideIfHigher => override_if_higher.push(c),
            ComposeMode::OverrideIfLower => override_if_lower.push(c),
            ComposeMode::Add => adds.push(c.value),
            ComposeMode::Multiply => multiplies.push(c.value),
            ComposeMode::Max => maxes.push(c.value),
            ComposeMode::Min => mins.push(c.value),
        }
    }

    // If there are overrides, highest priority wins
    if !overrides.is_empty() {
        overrides.sort_by(|a, b| b.priority.cmp(&a.priority));
        return Some(overrides.into_iter().next().unwrap().value);
    }

    // Start with base value from adds (or 0 if none)
    let mut result = if adds.is_empty() {
        Value::Float(0.0)
    } else {
        sum_values(&adds)?
    };

    // Apply multipliers
    for mult in multiplies {
        result = multiply_values(&result, &mult)?;
    }

    // Apply max constraints
    for max_val in maxes {
        result = max_value(&result, &max_val)?;
    }

    // Apply min constraints
    for min_val in mins {
        result = min_value(&result, &min_val)?;
    }

    // Apply conditional overrides
    for ovr in override_if_higher {
        if compare_values(&ovr.value, &result)? == std::cmp::Ordering::Greater {
            result = ovr.value;
        }
    }
    for ovr in override_if_lower {
        if compare_values(&ovr.value, &result)? == std::cmp::Ordering::Less {
            result = ovr.value;
        }
    }

    Some(result)
}

/// Sum numeric values.
fn sum_values(values: &[Value]) -> Option<Value> {
    let mut int_sum: i64 = 0;
    let mut float_sum: f64 = 0.0;
    let mut has_float = false;

    for v in values {
        match v {
            Value::Int(n) => int_sum += n,
            Value::Float(n) => {
                has_float = true;
                float_sum += n;
            }
            _ => return None, // Can't sum non-numeric values
        }
    }

    if has_float {
        Some(Value::Float(float_sum + int_sum as f64))
    } else {
        Some(Value::Int(int_sum))
    }
}

/// Multiply two numeric values.
fn multiply_values(a: &Value, b: &Value) -> Option<Value> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Some(Value::Int(x * y)),
        (Value::Int(x), Value::Float(y)) | (Value::Float(y), Value::Int(x)) => {
            Some(Value::Float(*x as f64 * y))
        }
        (Value::Float(x), Value::Float(y)) => Some(Value::Float(x * y)),
        _ => None,
    }
}

/// Get the maximum of two numeric values.
fn max_value(a: &Value, b: &Value) -> Option<Value> {
    match compare_values(a, b)? {
        std::cmp::Ordering::Greater | std::cmp::Ordering::Equal => Some(a.clone()),
        std::cmp::Ordering::Less => Some(b.clone()),
    }
}

/// Get the minimum of two numeric values.
fn min_value(a: &Value, b: &Value) -> Option<Value> {
    match compare_values(a, b)? {
        std::cmp::Ordering::Less | std::cmp::Ordering::Equal => Some(a.clone()),
        std::cmp::Ordering::Greater => Some(b.clone()),
    }
}

/// Compare two numeric values.
fn compare_values(a: &Value, b: &Value) -> Option<std::cmp::Ordering> {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Some(x.cmp(y)),
        (Value::Int(x), Value::Float(y)) => Some((*x as f64).total_cmp(y)),
        (Value::Float(x), Value::Int(y)) => Some(x.total_cmp(&(*y as f64))),
        (Value::Float(x), Value::Float(y)) => Some(x.total_cmp(y)),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_contributions() {
        assert_eq!(compose_values(vec![]), None);
    }

    #[test]
    fn test_single_contribution() {
        let result = compose_values(vec![Contribution::new(42_i64, ComposeMode::Add, 0)]);
        assert_eq!(result, Some(Value::Int(42)));
    }

    #[test]
    fn test_add_integers() {
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(20_i64, ComposeMode::Add, 0),
            Contribution::new(12_i64, ComposeMode::Add, 0),
        ]);
        assert_eq!(result, Some(Value::Int(42)));
    }

    #[test]
    fn test_add_floats() {
        let result = compose_values(vec![
            Contribution::new(0.3_f64, ComposeMode::Add, 0),
            Contribution::new(0.5_f64, ComposeMode::Add, 0),
        ]);
        assert_eq!(result, Some(Value::Float(0.8)));
    }

    #[test]
    fn test_add_mixed_numeric() {
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(0.5_f64, ComposeMode::Add, 0),
        ]);
        assert_eq!(result, Some(Value::Float(10.5)));
    }

    #[test]
    fn test_multiply() {
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(2_i64, ComposeMode::Multiply, 0),
        ]);
        assert_eq!(result, Some(Value::Int(20)));
    }

    #[test]
    fn test_multiply_float() {
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(1.5_f64, ComposeMode::Multiply, 0),
        ]);
        assert_eq!(result, Some(Value::Float(15.0)));
    }

    #[test]
    fn test_max() {
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(100_i64, ComposeMode::Max, 0),
        ]);
        // 10 vs max of 100 = 100
        assert_eq!(result, Some(Value::Int(100)));
    }

    #[test]
    fn test_min() {
        let result = compose_values(vec![
            Contribution::new(100_i64, ComposeMode::Add, 0),
            Contribution::new(10_i64, ComposeMode::Min, 0),
        ]);
        // 100 vs min of 10 = 10
        assert_eq!(result, Some(Value::Int(10)));
    }

    #[test]
    fn test_override_highest_priority() {
        let result = compose_values(vec![
            Contribution::new(100_i64, ComposeMode::Override, 1),
            Contribution::new(200_i64, ComposeMode::Override, 10),
            Contribution::new(50_i64, ComposeMode::Override, 5),
        ]);
        // Priority 10 wins
        assert_eq!(result, Some(Value::Int(200)));
    }

    #[test]
    fn test_override_if_higher() {
        // Base of 50, override if higher with 100
        let result = compose_values(vec![
            Contribution::new(50_i64, ComposeMode::Add, 0),
            Contribution::new(100_i64, ComposeMode::OverrideIfHigher, 0),
        ]);
        assert_eq!(result, Some(Value::Int(100)));

        // Base of 150, override if higher with 100 (should not override)
        let result = compose_values(vec![
            Contribution::new(150_i64, ComposeMode::Add, 0),
            Contribution::new(100_i64, ComposeMode::OverrideIfHigher, 0),
        ]);
        assert_eq!(result, Some(Value::Int(150)));
    }

    #[test]
    fn test_override_if_lower() {
        // Base of 100, override if lower with 50
        let result = compose_values(vec![
            Contribution::new(100_i64, ComposeMode::Add, 0),
            Contribution::new(50_i64, ComposeMode::OverrideIfLower, 0),
        ]);
        assert_eq!(result, Some(Value::Int(50)));

        // Base of 30, override if lower with 50 (should not override)
        let result = compose_values(vec![
            Contribution::new(30_i64, ComposeMode::Add, 0),
            Contribution::new(50_i64, ComposeMode::OverrideIfLower, 0),
        ]);
        assert_eq!(result, Some(Value::Int(30)));
    }

    #[test]
    fn test_complex_composition() {
        // 10 + 20 = 30, then * 2 = 60, max with 100 = 100
        let result = compose_values(vec![
            Contribution::new(10_i64, ComposeMode::Add, 0),
            Contribution::new(20_i64, ComposeMode::Add, 0),
            Contribution::new(2_i64, ComposeMode::Multiply, 0),
            Contribution::new(100_i64, ComposeMode::Max, 0),
        ]);
        assert_eq!(result, Some(Value::Int(100)));
    }

    #[test]
    fn test_non_numeric_fails() {
        let result = compose_values(vec![
            Contribution::new("hello", ComposeMode::Add, 0),
            Contribution::new("world", ComposeMode::Add, 0),
        ]);
        assert_eq!(result, None);
    }
}
