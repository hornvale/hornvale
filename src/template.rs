//! Templates for entity archetypes with variation.
//!
//! Templates define entity archetypes that can be instantiated with seeded
//! variation. This enables procedural generation where the same template + seed
//! always produces the same entity.
//!
//! # Example
//!
//! ```
//! use hornvale::template::{Template, FieldSpec};
//! use hornvale::rng::SeededRng;
//! use hornvale::World;
//!
//! // Define a goblin template
//! let mut template = Template::new("goblin");
//! template.set_field("Name", FieldSpec::fixed("Goblin"));
//! template.set_field("HP", FieldSpec::range(8, 15));
//! template.set_field("Strength", FieldSpec::range(6, 12));
//!
//! // Instantiate with a seed
//! let mut world = World::new();
//! let mut rng = SeededRng::new(12345);
//! let entity = template.instantiate(&mut world, &mut rng);
//! ```

use im::OrdMap;

use crate::core::{EntityId, Value, World};
use crate::rng::SeededRng;
use crate::symbol::Symbol;

/// A field specification describing how to generate a field value.
#[derive(Debug, Clone, PartialEq)]
pub enum FieldSpec {
    /// A fixed value.
    Fixed(Value),

    /// A random integer in an inclusive range.
    IntRange { min: i64, max: i64 },

    /// A random float in a half-open range [min, max).
    FloatRange { min: f64, max: f64 },

    /// A random choice from a list of values.
    Choice(Vec<Value>),

    /// A boolean with a given probability of being true.
    Chance(f64),
}

impl FieldSpec {
    /// Create a fixed value field.
    pub fn fixed(value: impl Into<Value>) -> Self {
        FieldSpec::Fixed(value.into())
    }

    /// Create an integer range field.
    pub fn range(min: i64, max: i64) -> Self {
        FieldSpec::IntRange { min, max }
    }

    /// Create a float range field.
    pub fn range_f64(min: f64, max: f64) -> Self {
        FieldSpec::FloatRange { min, max }
    }

    /// Create a choice field from values.
    pub fn choice(values: impl IntoIterator<Item = Value>) -> Self {
        FieldSpec::Choice(values.into_iter().collect())
    }

    /// Create a boolean chance field.
    pub fn chance(probability: f64) -> Self {
        FieldSpec::Chance(probability)
    }

    /// Generate a value from this field spec using the given RNG.
    pub fn generate(&self, rng: &mut SeededRng) -> Value {
        match self {
            FieldSpec::Fixed(v) => v.clone(),
            FieldSpec::IntRange { min, max } => Value::Int(rng.range(*min, *max)),
            FieldSpec::FloatRange { min, max } => Value::Float(rng.range_f64(*min, *max)),
            FieldSpec::Choice(values) => {
                if values.is_empty() {
                    Value::Nil
                } else {
                    rng.choice(values).clone()
                }
            }
            FieldSpec::Chance(p) => Value::Bool(rng.chance(*p)),
        }
    }
}

/// A template for creating entities with variation.
///
/// Templates define the structure of an entity archetype. When instantiated
/// with a seed, they produce deterministic entities with varied attributes.
#[derive(Debug, Clone)]
pub struct Template {
    /// Template name (e.g., "goblin", "treasure-chest").
    name: Symbol,

    /// Field specifications.
    fields: OrdMap<Symbol, FieldSpec>,

    /// Optional documentation.
    doc: Option<String>,

    /// Tags for categorization (e.g., "creature", "item").
    tags: Vec<Symbol>,
}

impl Template {
    /// Create a new template with the given name.
    pub fn new(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            fields: OrdMap::new(),
            doc: None,
            tags: Vec::new(),
        }
    }

    /// Get the template name.
    pub fn name(&self) -> Symbol {
        self.name
    }

    /// Set a field specification.
    pub fn set_field(&mut self, name: impl Into<Symbol>, spec: FieldSpec) {
        self.fields.insert(name.into(), spec);
    }

    /// Get a field specification.
    pub fn get_field(&self, name: impl Into<Symbol>) -> Option<&FieldSpec> {
        self.fields.get(&name.into())
    }

    /// Set the documentation.
    pub fn set_doc(&mut self, doc: impl Into<String>) {
        self.doc = Some(doc.into());
    }

    /// Get the documentation.
    pub fn doc(&self) -> Option<&str> {
        self.doc.as_deref()
    }

    /// Add a tag.
    pub fn add_tag(&mut self, tag: impl Into<Symbol>) {
        let tag = tag.into();
        if !self.tags.contains(&tag) {
            self.tags.push(tag);
        }
    }

    /// Check if the template has a tag.
    pub fn has_tag(&self, tag: impl Into<Symbol>) -> bool {
        self.tags.contains(&tag.into())
    }

    /// Get all tags.
    pub fn tags(&self) -> &[Symbol] {
        &self.tags
    }

    /// Iterate over field names.
    pub fn field_names(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.fields.keys().copied()
    }

    /// Instantiate this template, creating a new entity in the world.
    ///
    /// The RNG determines variation in field values. Same template + same RNG
    /// state = same entity.
    pub fn instantiate(&self, world: &mut World, rng: &mut SeededRng) -> EntityId {
        let entity = world.create_entity();

        // Set the template name as a component
        world.set_component(entity, "Template", Value::Symbol(self.name));

        // Generate and set each field
        for (field_name, spec) in &self.fields {
            let value = spec.generate(rng);
            let field_name_str = field_name.as_str();
            world.set_component(entity, &*field_name_str, value);
        }

        entity
    }

    /// Instantiate with a specific seed (convenience method).
    pub fn instantiate_with_seed(&self, world: &mut World, seed: u64) -> EntityId {
        let mut rng = SeededRng::new(seed);
        self.instantiate(world, &mut rng)
    }
}

/// Registry of templates.
#[derive(Debug, Clone, Default)]
pub struct TemplateRegistry {
    templates: OrdMap<Symbol, Template>,
}

impl TemplateRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self {
            templates: OrdMap::new(),
        }
    }

    /// Register a template.
    pub fn register(&mut self, template: Template) {
        self.templates.insert(template.name(), template);
    }

    /// Get a template by name.
    pub fn get(&self, name: impl Into<Symbol>) -> Option<&Template> {
        self.templates.get(&name.into())
    }

    /// Check if a template exists.
    pub fn contains(&self, name: impl Into<Symbol>) -> bool {
        self.templates.contains_key(&name.into())
    }

    /// List all template names.
    pub fn names(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.templates.keys().copied()
    }

    /// Number of registered templates.
    pub fn len(&self) -> usize {
        self.templates.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.templates.is_empty()
    }

    /// Find templates with a given tag.
    pub fn with_tag(&self, tag: impl Into<Symbol>) -> Vec<&Template> {
        let tag = tag.into();
        self.templates.values().filter(|t| t.has_tag(tag)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_field_spec_fixed() {
        let spec = FieldSpec::fixed(42_i64);
        let mut rng = SeededRng::new(12345);
        assert_eq!(spec.generate(&mut rng), Value::Int(42));
    }

    #[test]
    fn test_field_spec_range() {
        let spec = FieldSpec::range(1, 10);
        let mut rng = SeededRng::new(12345);

        for _ in 0..100 {
            let value = spec.generate(&mut rng);
            match value {
                Value::Int(n) => assert!((1..=10).contains(&n)),
                _ => panic!("Expected Int"),
            }
        }
    }

    #[test]
    fn test_field_spec_float_range() {
        let spec = FieldSpec::range_f64(0.0, 1.0);
        let mut rng = SeededRng::new(12345);

        for _ in 0..100 {
            let value = spec.generate(&mut rng);
            match value {
                Value::Float(f) => assert!((0.0..1.0).contains(&f)),
                _ => panic!("Expected Float"),
            }
        }
    }

    #[test]
    fn test_field_spec_choice() {
        let spec = FieldSpec::choice([
            Value::Symbol(Symbol::new("fire")),
            Value::Symbol(Symbol::new("ice")),
            Value::Symbol(Symbol::new("lightning")),
        ]);
        let mut rng = SeededRng::new(12345);

        for _ in 0..100 {
            let value = spec.generate(&mut rng);
            match value {
                Value::Symbol(s) => {
                    let name = s.as_str();
                    assert!(
                        name == "fire" || name == "ice" || name == "lightning",
                        "Unexpected symbol: {name}"
                    );
                }
                _ => panic!("Expected Symbol"),
            }
        }
    }

    #[test]
    fn test_field_spec_chance() {
        let spec = FieldSpec::chance(0.5);
        let mut rng = SeededRng::new(12345);

        let mut true_count = 0;
        for _ in 0..1000 {
            if spec.generate(&mut rng) == Value::Bool(true) {
                true_count += 1;
            }
        }

        // Should be roughly 50%
        assert!(
            (400..600).contains(&true_count),
            "Expected ~500 true, got {true_count}"
        );
    }

    #[test]
    fn test_template_instantiate() {
        let mut template = Template::new("goblin");
        template.set_field("Name", FieldSpec::fixed("Goblin"));
        template.set_field("HP", FieldSpec::range(8, 15));
        template.set_field("Strength", FieldSpec::range(6, 12));

        let mut world = World::new();
        let entity = template.instantiate_with_seed(&mut world, 12345);

        // Check template component
        assert_eq!(
            world.get_component(entity, "Template"),
            Some(&Value::Symbol(Symbol::new("goblin")))
        );

        // Check fixed field
        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("Goblin"))
        );

        // Check ranged fields exist and are in range
        if let Some(Value::Int(hp)) = world.get_component(entity, "HP") {
            assert!((8..=15).contains(hp));
        } else {
            panic!("Expected HP to be Int");
        }

        if let Some(Value::Int(str)) = world.get_component(entity, "Strength") {
            assert!((6..=12).contains(str));
        } else {
            panic!("Expected Strength to be Int");
        }
    }

    #[test]
    fn test_template_determinism() {
        let mut template = Template::new("test");
        template.set_field("Value", FieldSpec::range(1, 1000));

        let mut world1 = World::new();
        let mut world2 = World::new();

        let entity1 = template.instantiate_with_seed(&mut world1, 42);
        let entity2 = template.instantiate_with_seed(&mut world2, 42);

        assert_eq!(
            world1.get_component(entity1, "Value"),
            world2.get_component(entity2, "Value")
        );
    }

    #[test]
    fn test_template_different_seeds() {
        let mut template = Template::new("test");
        template.set_field("Value", FieldSpec::range(1, 1000000));

        let mut world1 = World::new();
        let mut world2 = World::new();

        let entity1 = template.instantiate_with_seed(&mut world1, 12345);
        let entity2 = template.instantiate_with_seed(&mut world2, 54321);

        // Very unlikely to be equal with different seeds
        assert_ne!(
            world1.get_component(entity1, "Value"),
            world2.get_component(entity2, "Value")
        );
    }

    #[test]
    fn test_template_tags() {
        let mut template = Template::new("goblin");
        template.add_tag("creature");
        template.add_tag("hostile");

        assert!(template.has_tag("creature"));
        assert!(template.has_tag("hostile"));
        assert!(!template.has_tag("friendly"));
    }

    #[test]
    fn test_template_registry() {
        let mut registry = TemplateRegistry::new();

        let mut goblin = Template::new("goblin");
        goblin.add_tag("creature");

        let mut treasure = Template::new("treasure-chest");
        treasure.add_tag("container");

        registry.register(goblin);
        registry.register(treasure);

        assert!(registry.contains("goblin"));
        assert!(registry.contains("treasure-chest"));
        assert!(!registry.contains("dragon"));

        let creatures = registry.with_tag("creature");
        assert_eq!(creatures.len(), 1);
        assert_eq!(creatures[0].name().as_str(), "goblin");
    }

    #[test]
    fn test_template_doc() {
        let mut template = Template::new("goblin");
        template.set_doc("A small, green creature");

        assert_eq!(template.doc(), Some("A small, green creature"));
    }

    #[test]
    fn test_choice_empty() {
        let spec = FieldSpec::choice(std::iter::empty::<Value>());
        let mut rng = SeededRng::new(12345);

        // Empty choice returns nil
        assert_eq!(spec.generate(&mut rng), Value::Nil);
    }
}
