//! Generation context for tracking derivation chains.
//!
//! The GenerationContext tracks how derived values were computed,
//! enabling debugging ("why is this value X?") and reproducibility.
//!
//! When a seed is present, the context can create deterministic RNGs
//! for procedural generation.

use im::OrdMap;

use crate::core::{EntityId, Value};
use crate::rng::SeededRng;
use crate::symbol::Symbol;

/// A step in the derivation chain.
#[derive(Debug, Clone, PartialEq)]
pub struct GenerationStep {
    /// Name of the rule that produced this step.
    pub rule_name: Symbol,
    /// The entity being derived.
    pub entity: EntityId,
    /// Input values used in this derivation.
    pub inputs: Vec<Value>,
    /// The output value produced.
    pub output: Value,
}

impl GenerationStep {
    /// Create a new generation step.
    pub fn new(rule_name: impl Into<Symbol>, entity: EntityId, output: impl Into<Value>) -> Self {
        GenerationStep {
            rule_name: rule_name.into(),
            entity,
            inputs: Vec::new(),
            output: output.into(),
        }
    }

    /// Add an input value to this step.
    pub fn with_input(mut self, input: impl Into<Value>) -> Self {
        self.inputs.push(input.into());
        self
    }

    /// Add multiple input values to this step.
    pub fn with_inputs(mut self, inputs: impl IntoIterator<Item = Value>) -> Self {
        self.inputs.extend(inputs);
        self
    }

    /// Get a human-readable description of this step.
    pub fn describe(&self) -> String {
        if self.inputs.is_empty() {
            format!(
                "Rule '{}' on entity {} -> {}",
                self.rule_name, self.entity, self.output
            )
        } else {
            let inputs: Vec<_> = self.inputs.iter().map(|v| v.to_string()).collect();
            format!(
                "Rule '{}' on entity {} with [{}] -> {}",
                self.rule_name,
                self.entity,
                inputs.join(", "),
                self.output
            )
        }
    }
}

/// Context for tracking how a derived value was computed.
///
/// This enables debugging derivation chains and reproducing
/// the exact sequence of computations.
#[derive(Debug, Clone, Default)]
pub struct GenerationContext {
    /// Optional seed used for generation.
    pub seed: Option<u64>,
    /// The derivation path (sequence of rules applied).
    pub path: Vec<GenerationStep>,
    /// Accumulated parameters during generation.
    pub parameters: OrdMap<Symbol, Value>,
}

impl GenerationContext {
    /// Create a new empty context.
    pub fn new() -> Self {
        GenerationContext {
            seed: None,
            path: Vec::new(),
            parameters: OrdMap::new(),
        }
    }

    /// Create a context with a seed.
    pub fn with_seed(seed: u64) -> Self {
        GenerationContext {
            seed: Some(seed),
            path: Vec::new(),
            parameters: OrdMap::new(),
        }
    }

    /// Add a step to the derivation path.
    pub fn push_step(&mut self, step: GenerationStep) {
        self.path.push(step);
    }

    /// Remove the last step from the derivation path.
    pub fn pop_step(&mut self) -> Option<GenerationStep> {
        self.path.pop()
    }

    /// Set a parameter value.
    pub fn set_parameter(&mut self, name: impl Into<Symbol>, value: impl Into<Value>) {
        self.parameters.insert(name.into(), value.into());
    }

    /// Get a parameter value.
    pub fn get_parameter(&self, name: impl Into<Symbol>) -> Option<&Value> {
        self.parameters.get(&name.into())
    }

    /// Get the number of steps in the derivation path.
    pub fn depth(&self) -> usize {
        self.path.len()
    }

    /// Check if the context has any derivation steps.
    pub fn is_empty(&self) -> bool {
        self.path.is_empty()
    }

    /// Get the last step in the derivation path.
    pub fn last_step(&self) -> Option<&GenerationStep> {
        self.path.last()
    }

    /// Get a human-readable trace of the derivation path.
    pub fn trace(&self) -> String {
        let mut lines = Vec::new();

        if let Some(seed) = self.seed {
            lines.push(format!("Seed: {seed}"));
        }

        if !self.parameters.is_empty() {
            lines.push("Parameters:".to_string());
            for (name, value) in &self.parameters {
                lines.push(format!("  {name}: {value}"));
            }
        }

        if !self.path.is_empty() {
            lines.push("Derivation path:".to_string());
            for (i, step) in self.path.iter().enumerate() {
                lines.push(format!("  {}. {}", i + 1, step.describe()));
            }
        }

        lines.join("\n")
    }

    /// Clear the context.
    pub fn clear(&mut self) {
        self.seed = None;
        self.path.clear();
        self.parameters.clear();
    }

    /// Create a SeededRng from this context's seed.
    ///
    /// # Panics
    ///
    /// Panics if the context has no seed.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::derive::GenerationContext;
    ///
    /// let ctx = GenerationContext::with_seed(12345);
    /// let mut rng = ctx.rng();
    /// let value = rng.next_u64();
    /// ```
    pub fn rng(&self) -> SeededRng {
        SeededRng::new(self.seed.expect("GenerationContext::rng() requires a seed"))
    }

    /// Create a SeededRng if the context has a seed, or None otherwise.
    pub fn try_rng(&self) -> Option<SeededRng> {
        self.seed.map(SeededRng::new)
    }

    /// Create a child context with a derived seed.
    ///
    /// The child context inherits parameters but starts with an empty path.
    /// If this context has a seed, the child gets a deterministically derived seed.
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::derive::GenerationContext;
    ///
    /// let mut parent = GenerationContext::with_seed(12345);
    /// parent.set_parameter("biome", "forest");
    ///
    /// let child = parent.fork();
    /// assert!(child.seed.is_some());
    /// assert_ne!(child.seed, parent.seed); // Different seed
    /// ```
    pub fn fork(&mut self) -> Self {
        let child_seed = self.seed.map(|s| {
            let mut rng = SeededRng::new(s);
            // Advance parent's conceptual state by consuming one value
            // to derive the child seed
            rng.next_u64()
        });

        GenerationContext {
            seed: child_seed,
            path: Vec::new(),
            parameters: self.parameters.clone(),
        }
    }

    /// Create a child context with a seed derived from a key.
    ///
    /// Useful for generating deterministic sub-contexts based on an identifier
    /// (like an entity ID or location hash).
    ///
    /// # Example
    ///
    /// ```
    /// use hornvale::derive::GenerationContext;
    ///
    /// let ctx = GenerationContext::with_seed(12345);
    /// let room_ctx = ctx.fork_with_key(42); // Room ID 42
    /// ```
    pub fn fork_with_key(&self, key: u64) -> Self {
        let child_seed = self.seed.map(|s| {
            // Mix key with seed deterministically
            s.wrapping_mul(0x9e3779b97f4a7c15) ^ key
        });

        GenerationContext {
            seed: child_seed,
            path: Vec::new(),
            parameters: self.parameters.clone(),
        }
    }

    /// Check if this context has a seed for RNG operations.
    pub fn has_seed(&self) -> bool {
        self.seed.is_some()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_new() {
        let ctx = GenerationContext::new();
        assert!(ctx.seed.is_none());
        assert!(ctx.path.is_empty());
        assert!(ctx.parameters.is_empty());
    }

    #[test]
    fn test_context_with_seed() {
        let ctx = GenerationContext::with_seed(12345);
        assert_eq!(ctx.seed, Some(12345));
    }

    #[test]
    fn test_generation_step() {
        let entity = EntityId::from_raw(1);
        let step = GenerationStep::new("fire-resistance", entity, Value::Float(0.3))
            .with_input(Value::Int(100))
            .with_input(Value::string("volcanic"));

        assert_eq!(step.rule_name.as_str(), "fire-resistance");
        assert_eq!(step.entity, entity);
        assert_eq!(step.inputs.len(), 2);
        assert_eq!(step.output, Value::Float(0.3));
    }

    #[test]
    fn test_context_push_pop() {
        let mut ctx = GenerationContext::new();
        let entity = EntityId::from_raw(1);

        ctx.push_step(GenerationStep::new("rule-1", entity, Value::Int(1)));
        ctx.push_step(GenerationStep::new("rule-2", entity, Value::Int(2)));

        assert_eq!(ctx.depth(), 2);

        let step = ctx.pop_step().unwrap();
        assert_eq!(step.rule_name.as_str(), "rule-2");
        assert_eq!(ctx.depth(), 1);
    }

    #[test]
    fn test_context_parameters() {
        let mut ctx = GenerationContext::new();

        ctx.set_parameter("biome", "volcanic");
        ctx.set_parameter("level", 5_i64);

        assert_eq!(ctx.get_parameter("biome"), Some(&Value::string("volcanic")));
        assert_eq!(ctx.get_parameter("level"), Some(&Value::Int(5)));
        assert_eq!(ctx.get_parameter("unknown"), None);
    }

    #[test]
    fn test_context_trace() {
        let mut ctx = GenerationContext::with_seed(42);
        let entity = EntityId::from_raw(1);

        ctx.set_parameter("biome", "volcanic");
        ctx.push_step(
            GenerationStep::new("base-resistance", entity, Value::Float(0.1))
                .with_input(Value::string("creature")),
        );
        ctx.push_step(GenerationStep::new(
            "biome-bonus",
            entity,
            Value::Float(0.3),
        ));

        let trace = ctx.trace();
        assert!(trace.contains("Seed: 42"));
        assert!(trace.contains("biome: \"volcanic\""));
        assert!(trace.contains("base-resistance"));
        assert!(trace.contains("biome-bonus"));
    }

    #[test]
    fn test_step_describe() {
        let entity = EntityId::from_raw(5);

        let step1 = GenerationStep::new("simple-rule", entity, Value::Int(42));
        assert!(step1.describe().contains("simple-rule"));
        assert!(step1.describe().contains("42"));

        let step2 = GenerationStep::new("complex-rule", entity, Value::Float(0.5))
            .with_input(Value::Int(10))
            .with_input(Value::string("test"));
        let desc = step2.describe();
        assert!(desc.contains("10"));
        assert!(desc.contains("\"test\""));
    }

    #[test]
    fn test_context_clear() {
        let mut ctx = GenerationContext::with_seed(42);
        ctx.set_parameter("test", "value");
        ctx.push_step(GenerationStep::new(
            "rule",
            EntityId::from_raw(0),
            Value::Int(1),
        ));

        ctx.clear();

        assert!(ctx.seed.is_none());
        assert!(ctx.is_empty());
        assert!(ctx.parameters.is_empty());
    }

    #[test]
    fn test_last_step() {
        let mut ctx = GenerationContext::new();
        let entity = EntityId::from_raw(1);

        assert!(ctx.last_step().is_none());

        ctx.push_step(GenerationStep::new("rule-1", entity, Value::Int(1)));
        ctx.push_step(GenerationStep::new("rule-2", entity, Value::Int(2)));

        let last = ctx.last_step().unwrap();
        assert_eq!(last.rule_name.as_str(), "rule-2");
    }

    #[test]
    fn test_rng_from_context() {
        let ctx = GenerationContext::with_seed(12345);
        let mut rng1 = ctx.rng();
        let mut rng2 = ctx.rng();

        // Same seed produces same sequence
        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_try_rng() {
        let ctx_with_seed = GenerationContext::with_seed(42);
        let ctx_without_seed = GenerationContext::new();

        assert!(ctx_with_seed.try_rng().is_some());
        assert!(ctx_without_seed.try_rng().is_none());
    }

    #[test]
    #[should_panic(expected = "requires a seed")]
    fn test_rng_without_seed_panics() {
        let ctx = GenerationContext::new();
        ctx.rng();
    }

    #[test]
    fn test_has_seed() {
        let ctx_with = GenerationContext::with_seed(42);
        let ctx_without = GenerationContext::new();

        assert!(ctx_with.has_seed());
        assert!(!ctx_without.has_seed());
    }

    #[test]
    fn test_fork_context() {
        let mut parent = GenerationContext::with_seed(12345);
        parent.set_parameter("biome", "forest");

        let child = parent.fork();

        // Child has a different seed
        assert!(child.seed.is_some());
        assert_ne!(child.seed, parent.seed);

        // Child inherits parameters
        assert_eq!(child.get_parameter("biome"), Some(&Value::string("forest")));

        // Child has empty path
        assert!(child.is_empty());
    }

    #[test]
    fn test_fork_determinism() {
        let mut parent1 = GenerationContext::with_seed(12345);
        let mut parent2 = GenerationContext::with_seed(12345);

        let child1 = parent1.fork();
        let child2 = parent2.fork();

        // Same parent seed produces same child seed
        assert_eq!(child1.seed, child2.seed);
    }

    #[test]
    fn test_fork_without_seed() {
        let mut parent = GenerationContext::new();
        parent.set_parameter("test", "value");

        let child = parent.fork();

        assert!(child.seed.is_none());
        assert_eq!(child.get_parameter("test"), Some(&Value::string("value")));
    }

    #[test]
    fn test_fork_with_key() {
        let ctx = GenerationContext::with_seed(12345);

        let child_a = ctx.fork_with_key(100);
        let child_b = ctx.fork_with_key(200);

        // Different keys produce different seeds
        assert!(child_a.seed.is_some());
        assert!(child_b.seed.is_some());
        assert_ne!(child_a.seed, child_b.seed);
    }

    #[test]
    fn test_fork_with_key_determinism() {
        let ctx1 = GenerationContext::with_seed(12345);
        let ctx2 = GenerationContext::with_seed(12345);

        let child1 = ctx1.fork_with_key(42);
        let child2 = ctx2.fork_with_key(42);

        // Same parent seed + same key = same child seed
        assert_eq!(child1.seed, child2.seed);
    }
}
