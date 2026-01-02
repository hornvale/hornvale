//! Lazy generation system for on-demand content creation.
//!
//! This module provides infrastructure for deferring content generation until
//! first access, while maintaining determinism through seeded RNG.
//!
//! # Overview
//!
//! - **GenerationStub**: A placeholder marking that a component should be generated
//! - **Generator**: A function that produces values given context and RNG
//! - **GeneratorRegistry**: Storage for named generators
//!
//! # Example
//!
//! ```
//! use hornvale::generator::{Generator, GeneratorRegistry, GenerationStub};
//! use hornvale::rng::SeededRng;
//! use hornvale::core::Value;
//! use std::sync::Arc;
//!
//! // Define a generator
//! let mut registry = GeneratorRegistry::new();
//! registry.register(Generator::new(
//!     "random-treasure",
//!     Arc::new(|_world, _entity, rng, _params| {
//!         let gold = rng.range(10, 100);
//!         Value::Int(gold)
//!     }),
//! ));
//!
//! // Create a stub (deferred generation)
//! let stub = GenerationStub::new("random-treasure", 12345);
//!
//! // Generate when needed
//! let mut rng = SeededRng::new(stub.seed());
//! let generator = registry.get("random-treasure").unwrap();
//! // let value = generator.generate(&world, entity, &mut rng, stub.parameters());
//! ```

use im::OrdMap;
use std::sync::Arc;

use crate::core::{ComponentTypeId, EntityId, Value, World};
use crate::rng::SeededRng;
use crate::symbol::Symbol;

/// A placeholder for content that should be generated on first access.
///
/// Stubs store the information needed to generate content deterministically:
/// - Which generator to use
/// - A seed for the RNG
/// - Optional parameters for customization
#[derive(Debug, Clone)]
pub struct GenerationStub {
    /// Name of the generator to use.
    generator: Symbol,
    /// Seed for deterministic RNG.
    seed: u64,
    /// Target component to generate (if applicable).
    target: Option<ComponentTypeId>,
    /// Additional parameters for generation.
    parameters: OrdMap<Symbol, Value>,
}

impl GenerationStub {
    /// Create a new generation stub.
    pub fn new(generator: impl Into<Symbol>, seed: u64) -> Self {
        Self {
            generator: generator.into(),
            seed,
            target: None,
            parameters: OrdMap::new(),
        }
    }

    /// Set the target component.
    pub fn with_target(mut self, target: impl Into<ComponentTypeId>) -> Self {
        self.target = Some(target.into());
        self
    }

    /// Add a parameter.
    pub fn with_param(mut self, key: impl Into<Symbol>, value: impl Into<Value>) -> Self {
        self.parameters.insert(key.into(), value.into());
        self
    }

    /// Set all parameters from a map (keys are strings, converted to Symbols).
    pub fn with_params(mut self, params: OrdMap<String, Value>) -> Self {
        for (key, value) in params {
            self.parameters.insert(Symbol::new(&key), value);
        }
        self
    }

    /// Get the generator name.
    pub fn generator(&self) -> Symbol {
        self.generator
    }

    /// Get the seed.
    pub fn seed(&self) -> u64 {
        self.seed
    }

    /// Get the target component.
    pub fn target(&self) -> Option<ComponentTypeId> {
        self.target
    }

    /// Get the parameters.
    pub fn parameters(&self) -> &OrdMap<Symbol, Value> {
        &self.parameters
    }

    /// Get a specific parameter.
    pub fn get_param(&self, key: impl Into<Symbol>) -> Option<&Value> {
        self.parameters.get(&key.into())
    }

    /// Create an RNG from this stub's seed.
    pub fn rng(&self) -> SeededRng {
        SeededRng::new(self.seed)
    }
}

/// Type alias for generator functions.
///
/// A generator takes:
/// - `&World` - Read access to the world state
/// - `EntityId` - The entity being generated for
/// - `&mut SeededRng` - RNG for deterministic randomness
/// - `&OrdMap<Symbol, Value>` - Parameters from the stub
///
/// And returns a `Value` to be stored as the component.
pub type GeneratorFn =
    Arc<dyn Fn(&World, EntityId, &mut SeededRng, &OrdMap<Symbol, Value>) -> Value + Send + Sync>;

/// A named generator function.
#[derive(Clone)]
pub struct Generator {
    /// Generator name.
    name: Symbol,
    /// The generation function.
    generate: GeneratorFn,
    /// Documentation.
    doc: Option<String>,
}

impl Generator {
    /// Create a new generator.
    pub fn new(name: impl Into<Symbol>, generate: GeneratorFn) -> Self {
        Self {
            name: name.into(),
            generate,
            doc: None,
        }
    }

    /// Add documentation.
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }

    /// Get the generator name.
    pub fn name(&self) -> Symbol {
        self.name
    }

    /// Get the documentation.
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }

    /// Run the generator.
    pub fn generate(
        &self,
        world: &World,
        entity: EntityId,
        rng: &mut SeededRng,
        params: &OrdMap<Symbol, Value>,
    ) -> Value {
        (self.generate)(world, entity, rng, params)
    }
}

impl std::fmt::Debug for Generator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Generator")
            .field("name", &self.name)
            .field("doc", &self.doc)
            .finish()
    }
}

/// Registry of available generators.
#[derive(Clone, Default)]
pub struct GeneratorRegistry {
    generators: OrdMap<Symbol, Generator>,
}

impl GeneratorRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self {
            generators: OrdMap::new(),
        }
    }

    /// Register a generator.
    pub fn register(&mut self, generator: Generator) {
        self.generators.insert(generator.name(), generator);
    }

    /// Register a generator from a closure with optional documentation and produces list.
    ///
    /// This is a convenience method for creating and registering a generator in one step.
    pub fn register_with_doc<F>(
        &mut self,
        name: impl Into<Symbol>,
        doc: Option<String>,
        _produces: Vec<String>, // Reserved for future schema validation
        f: F,
    ) where
        F: Fn(&World, EntityId, &mut SeededRng, &OrdMap<Symbol, Value>) -> Value
            + Send
            + Sync
            + 'static,
    {
        let mut generator = Generator::new(name, Arc::new(f));
        if let Some(d) = doc {
            generator = generator.with_doc(d);
        }
        self.register(generator);
    }

    /// Get a generator by name.
    pub fn get(&self, name: impl Into<Symbol>) -> Option<&Generator> {
        self.generators.get(&name.into())
    }

    /// Check if a generator exists.
    pub fn contains(&self, name: impl Into<Symbol>) -> bool {
        self.generators.contains_key(&name.into())
    }

    /// List all generator names.
    pub fn names(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.generators.keys().copied()
    }

    /// Number of registered generators.
    pub fn len(&self) -> usize {
        self.generators.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.generators.is_empty()
    }
}

impl std::fmt::Debug for GeneratorRegistry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GeneratorRegistry")
            .field("count", &self.generators.len())
            .finish()
    }
}

/// Storage for pending generation stubs.
///
/// Maps (entity, component) pairs to their generation stubs.
#[derive(Debug, Clone, Default)]
pub struct StubStorage {
    stubs: OrdMap<(EntityId, ComponentTypeId), GenerationStub>,
}

impl StubStorage {
    /// Create empty storage.
    pub fn new() -> Self {
        Self {
            stubs: OrdMap::new(),
        }
    }

    /// Add a stub for a specific entity/component.
    pub fn add(
        &mut self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
        stub: GenerationStub,
    ) {
        self.stubs.insert((entity, component.into()), stub);
    }

    /// Get a stub for a specific entity/component.
    pub fn get(
        &self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> Option<&GenerationStub> {
        self.stubs.get(&(entity, component.into()))
    }

    /// Remove and return a stub.
    pub fn take(
        &mut self,
        entity: EntityId,
        component: impl Into<ComponentTypeId>,
    ) -> Option<GenerationStub> {
        let key = (entity, component.into());
        let stub = self.stubs.get(&key).cloned();
        if stub.is_some() {
            self.stubs.remove(&key);
        }
        stub
    }

    /// Check if a stub exists.
    pub fn has(&self, entity: EntityId, component: impl Into<ComponentTypeId>) -> bool {
        self.stubs.contains_key(&(entity, component.into()))
    }

    /// Remove all stubs for an entity.
    pub fn remove_entity(&mut self, entity: EntityId) {
        // OrdMap doesn't have retain, so we filter and rebuild
        self.stubs = self
            .stubs
            .iter()
            .filter(|((e, _), _)| *e != entity)
            .map(|(k, v)| (*k, v.clone()))
            .collect();
    }

    /// Number of pending stubs.
    pub fn len(&self) -> usize {
        self.stubs.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.stubs.is_empty()
    }
}

/// Result of a generation attempt.
#[derive(Debug, Clone, PartialEq)]
pub enum GenerationResult {
    /// Value already existed (no generation needed).
    Existing(Value),
    /// Value was generated from a stub.
    Generated(Value),
    /// No value and no stub exists.
    NotFound,
    /// Stub exists but generator not found.
    GeneratorNotFound(Symbol),
}

impl GenerationResult {
    /// Get the value if present.
    pub fn value(&self) -> Option<&Value> {
        match self {
            GenerationResult::Existing(v) | GenerationResult::Generated(v) => Some(v),
            _ => None,
        }
    }

    /// Check if generation occurred.
    pub fn was_generated(&self) -> bool {
        matches!(self, GenerationResult::Generated(_))
    }
}

/// Get a component value, generating it from a stub if necessary.
///
/// This is the main entry point for lazy generation:
/// 1. If the component already exists, return it
/// 2. If a stub exists, run the generator and store the result
/// 3. Otherwise, return NotFound
///
/// # Example
///
/// ```
/// use hornvale::generator::{
///     get_or_generate, Generator, GeneratorRegistry, GenerationStub, StubStorage,
/// };
/// use hornvale::core::{World, Value};
/// use std::sync::Arc;
///
/// let mut world = World::new();
/// let entity = world.create_entity();
///
/// // Set up a generator
/// let mut generators = GeneratorRegistry::new();
/// generators.register(Generator::new(
///     "random-gold",
///     Arc::new(|_, _, rng, _| Value::Int(rng.range(10, 100))),
/// ));
///
/// // Add a stub for deferred generation
/// let mut stubs = StubStorage::new();
/// stubs.add(entity, "Gold", GenerationStub::new("random-gold", 12345));
///
/// // First access triggers generation
/// let result = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");
/// assert!(result.was_generated());
///
/// // Second access returns existing value
/// let result2 = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");
/// assert!(!result2.was_generated());
/// ```
pub fn get_or_generate(
    world: &mut World,
    stubs: &mut StubStorage,
    generators: &GeneratorRegistry,
    entity: EntityId,
    component: impl Into<ComponentTypeId> + Copy,
) -> GenerationResult {
    let component = component.into();

    // Check if component already exists
    if let Some(value) = world.get_component(entity, component) {
        return GenerationResult::Existing(value.clone());
    }

    // Check if there's a stub
    if let Some(stub) = stubs.take(entity, component) {
        // Get the generator
        let generator_name = stub.generator();
        if let Some(generator) = generators.get(generator_name) {
            // Generate the value
            let mut rng = stub.rng();
            let value = generator.generate(world, entity, &mut rng, stub.parameters());

            // Store the generated value
            let component_str = component.0.as_str();
            world.set_component(entity, &*component_str, value.clone());

            return GenerationResult::Generated(value);
        } else {
            // Generator not found - put stub back and return error
            stubs.add(entity, component, stub);
            return GenerationResult::GeneratorNotFound(generator_name);
        }
    }

    GenerationResult::NotFound
}

/// Check if a component exists or has a pending stub.
pub fn has_or_stub(
    world: &World,
    stubs: &StubStorage,
    entity: EntityId,
    component: impl Into<ComponentTypeId> + Copy,
) -> bool {
    let component = component.into();
    world.has_component(entity, component) || stubs.has(entity, component)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generation_stub() {
        let stub = GenerationStub::new("test-gen", 12345)
            .with_param("min", 10_i64)
            .with_param("max", 20_i64);

        assert_eq!(stub.generator().as_str(), "test-gen");
        assert_eq!(stub.seed(), 12345);
        assert_eq!(stub.get_param("min"), Some(&Value::Int(10)));
        assert_eq!(stub.get_param("max"), Some(&Value::Int(20)));
    }

    #[test]
    fn test_stub_rng_determinism() {
        let stub = GenerationStub::new("test", 42);

        let mut rng1 = stub.rng();
        let mut rng2 = stub.rng();

        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_generator_basic() {
        let generator = Generator::new(
            "double",
            Arc::new(|_world, _entity, _rng, params| {
                let n = params
                    .get(&Symbol::new("value"))
                    .and_then(|v| v.as_int())
                    .unwrap_or(0);
                Value::Int(n * 2)
            }),
        );

        let world = World::new();
        let entity = EntityId::from_raw(0);
        let mut rng = SeededRng::new(42);
        let mut params = OrdMap::new();
        params.insert(Symbol::new("value"), Value::Int(21));

        let result = generator.generate(&world, entity, &mut rng, &params);
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_generator_with_rng() {
        let generator = Generator::new(
            "random-value",
            Arc::new(|_world, _entity, rng, _params| Value::Int(rng.range(1, 100))),
        );

        let world = World::new();
        let entity = EntityId::from_raw(0);
        let params = OrdMap::new();

        // Same seed = same result
        let mut rng1 = SeededRng::new(12345);
        let mut rng2 = SeededRng::new(12345);

        let result1 = generator.generate(&world, entity, &mut rng1, &params);
        let result2 = generator.generate(&world, entity, &mut rng2, &params);

        assert_eq!(result1, result2);
    }

    #[test]
    fn test_generator_registry() {
        let mut registry = GeneratorRegistry::new();

        registry.register(Generator::new(
            "gen-a",
            Arc::new(|_, _, _, _| Value::Int(1)),
        ));
        registry.register(Generator::new(
            "gen-b",
            Arc::new(|_, _, _, _| Value::Int(2)),
        ));

        assert!(registry.contains("gen-a"));
        assert!(registry.contains("gen-b"));
        assert!(!registry.contains("gen-c"));
        assert_eq!(registry.len(), 2);
    }

    #[test]
    fn test_stub_storage() {
        let mut storage = StubStorage::new();
        let entity = EntityId::from_raw(1);

        storage.add(entity, "HP", GenerationStub::new("random-hp", 42));
        storage.add(entity, "Gold", GenerationStub::new("random-gold", 43));

        assert!(storage.has(entity, "HP"));
        assert!(storage.has(entity, "Gold"));
        assert!(!storage.has(entity, "Name"));

        let stub = storage.take(entity, "HP").unwrap();
        assert_eq!(stub.generator().as_str(), "random-hp");
        assert!(!storage.has(entity, "HP"));
    }

    #[test]
    fn test_stub_storage_remove_entity() {
        let mut storage = StubStorage::new();
        let e1 = EntityId::from_raw(1);
        let e2 = EntityId::from_raw(2);

        storage.add(e1, "A", GenerationStub::new("gen", 1));
        storage.add(e1, "B", GenerationStub::new("gen", 2));
        storage.add(e2, "A", GenerationStub::new("gen", 3));

        assert_eq!(storage.len(), 3);

        storage.remove_entity(e1);

        assert_eq!(storage.len(), 1);
        assert!(!storage.has(e1, "A"));
        assert!(!storage.has(e1, "B"));
        assert!(storage.has(e2, "A"));
    }

    #[test]
    fn test_generation_result() {
        let existing = GenerationResult::Existing(Value::Int(42));
        let generated = GenerationResult::Generated(Value::Int(42));
        let not_found = GenerationResult::NotFound;

        assert_eq!(existing.value(), Some(&Value::Int(42)));
        assert_eq!(generated.value(), Some(&Value::Int(42)));
        assert_eq!(not_found.value(), None);

        assert!(!existing.was_generated());
        assert!(generated.was_generated());
    }

    #[test]
    fn test_get_or_generate_existing() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Gold", 100_i64);

        let mut stubs = StubStorage::new();
        let generators = GeneratorRegistry::new();

        let result = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");

        assert!(matches!(result, GenerationResult::Existing(_)));
        assert_eq!(result.value(), Some(&Value::Int(100)));
        assert!(!result.was_generated());
    }

    #[test]
    fn test_get_or_generate_from_stub() {
        let mut world = World::new();
        let entity = world.create_entity();

        let mut generators = GeneratorRegistry::new();
        generators.register(Generator::new(
            "random-gold",
            Arc::new(|_, _, rng, _| Value::Int(rng.range(50, 100))),
        ));

        let mut stubs = StubStorage::new();
        stubs.add(entity, "Gold", GenerationStub::new("random-gold", 12345));

        // First access triggers generation
        let result = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");

        assert!(result.was_generated());
        assert!(matches!(result, GenerationResult::Generated(_)));

        // Value should be in range
        if let Some(Value::Int(gold)) = result.value() {
            assert!((50..=100).contains(gold), "Gold {gold} not in range");
        } else {
            panic!("Expected Int value");
        }

        // Stub should be consumed
        assert!(!stubs.has(entity, "Gold"));

        // World should have the component now
        assert!(world.has_component(entity, "Gold"));
    }

    #[test]
    fn test_get_or_generate_determinism() {
        // Same stub + seed = same result
        let mut generators = GeneratorRegistry::new();
        generators.register(Generator::new(
            "random-value",
            Arc::new(|_, _, rng, _| Value::Int(rng.range(1, 1000000))),
        ));

        // First world
        let mut world1 = World::new();
        let entity1 = world1.create_entity();
        let mut stubs1 = StubStorage::new();
        stubs1.add(entity1, "Value", GenerationStub::new("random-value", 42));
        let result1 = get_or_generate(&mut world1, &mut stubs1, &generators, entity1, "Value");

        // Second world with same setup
        let mut world2 = World::new();
        let entity2 = world2.create_entity();
        let mut stubs2 = StubStorage::new();
        stubs2.add(entity2, "Value", GenerationStub::new("random-value", 42));
        let result2 = get_or_generate(&mut world2, &mut stubs2, &generators, entity2, "Value");

        assert_eq!(result1.value(), result2.value());
    }

    #[test]
    fn test_get_or_generate_not_found() {
        let mut world = World::new();
        let entity = world.create_entity();

        let mut stubs = StubStorage::new();
        let generators = GeneratorRegistry::new();

        let result = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");

        assert!(matches!(result, GenerationResult::NotFound));
        assert_eq!(result.value(), None);
    }

    #[test]
    fn test_get_or_generate_generator_not_found() {
        let mut world = World::new();
        let entity = world.create_entity();

        let mut stubs = StubStorage::new();
        stubs.add(entity, "Gold", GenerationStub::new("nonexistent", 42));

        let generators = GeneratorRegistry::new(); // Empty!

        let result = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");

        assert!(matches!(result, GenerationResult::GeneratorNotFound(_)));

        // Stub should be restored
        assert!(stubs.has(entity, "Gold"));
    }

    #[test]
    fn test_get_or_generate_second_access() {
        let mut world = World::new();
        let entity = world.create_entity();

        let mut generators = GeneratorRegistry::new();
        generators.register(Generator::new(
            "gold-gen",
            Arc::new(|_, _, rng, _| Value::Int(rng.range(1, 100))),
        ));

        let mut stubs = StubStorage::new();
        stubs.add(entity, "Gold", GenerationStub::new("gold-gen", 12345));

        // First access
        let result1 = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");
        assert!(result1.was_generated());

        // Second access - should return existing, not regenerate
        let result2 = get_or_generate(&mut world, &mut stubs, &generators, entity, "Gold");
        assert!(!result2.was_generated());
        assert_eq!(result1.value(), result2.value());
    }

    #[test]
    fn test_has_or_stub() {
        let mut world = World::new();
        let entity = world.create_entity();
        world.set_component(entity, "Name", "Test");

        let mut stubs = StubStorage::new();
        stubs.add(entity, "Gold", GenerationStub::new("gen", 42));

        // Has component
        assert!(has_or_stub(&world, &stubs, entity, "Name"));

        // Has stub
        assert!(has_or_stub(&world, &stubs, entity, "Gold"));

        // Has neither
        assert!(!has_or_stub(&world, &stubs, entity, "HP"));
    }
}
