//! World loader - loads DSL definitions into the World.

use crate::Value;
use crate::core::{Cardinality, EntityId, RelationSchema, World};
use crate::generator::{GenerationStub, GeneratorRegistry, StubStorage};
use crate::lang::{Atom, ParseError, SExpr, parse_all};
use crate::rules::{Effect, Pattern, Rule, RuleSet, Trigger};
use crate::symbol::Symbol;
use crate::template::{FieldSpec, Template, TemplateRegistry};
use im::OrdMap;
use std::path::Path;

/// Error loading definitions.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum LoadError {
    #[error("parse error: {0}")]
    Parse(#[from] ParseError),

    #[error("invalid definition: {0}")]
    InvalidDefinition(String),

    #[error("unknown entity: {0}")]
    UnknownEntity(String),

    #[error("io error: {0}")]
    Io(String),
}

/// Loads DSL definitions into a World.
pub struct WorldLoader {
    /// Entity names to IDs.
    entity_names: OrdMap<Symbol, EntityId>,
    /// Pending relations (resolved after all entities created).
    pending_relations: Vec<(EntityId, Symbol, Symbol)>,
    /// Template registry.
    templates: TemplateRegistry,
    /// Generator registry.
    generators: GeneratorRegistry,
    /// Stub storage for lazy generation.
    stubs: StubStorage,
}

impl WorldLoader {
    pub fn new() -> Self {
        Self {
            entity_names: OrdMap::new(),
            pending_relations: Vec::new(),
            templates: TemplateRegistry::new(),
            generators: GeneratorRegistry::new(),
            stubs: StubStorage::new(),
        }
    }

    /// Get the template registry.
    pub fn templates(&self) -> &TemplateRegistry {
        &self.templates
    }

    /// Get the template registry mutably.
    pub fn templates_mut(&mut self) -> &mut TemplateRegistry {
        &mut self.templates
    }

    /// Get the generator registry.
    pub fn generators(&self) -> &GeneratorRegistry {
        &self.generators
    }

    /// Get the generator registry mutably.
    pub fn generators_mut(&mut self) -> &mut GeneratorRegistry {
        &mut self.generators
    }

    /// Get the stub storage.
    pub fn stubs(&self) -> &StubStorage {
        &self.stubs
    }

    /// Get the stub storage mutably.
    pub fn stubs_mut(&mut self) -> &mut StubStorage {
        &mut self.stubs
    }

    /// Load definitions from source string.
    pub fn load_str(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        source: &str,
    ) -> Result<(), LoadError> {
        let exprs = parse_all(source)?;
        self.load_exprs(world, rules, &exprs)
    }

    /// Load definitions from a file.
    pub fn load_file(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        path: &Path,
    ) -> Result<(), LoadError> {
        let source = std::fs::read_to_string(path).map_err(|e| LoadError::Io(e.to_string()))?;
        self.load_str(world, rules, &source)
    }

    /// Load parsed expressions.
    fn load_exprs(
        &mut self,
        world: &mut World,
        rules: &mut RuleSet,
        exprs: &[SExpr],
    ) -> Result<(), LoadError> {
        // First pass: create entities, register relations, load templates, and register generators
        for expr in exprs {
            if expr.is_call("entity") {
                self.load_entity(world, expr)?;
            } else if expr.is_call("relation") {
                self.load_relation(world, expr)?;
            } else if expr.is_call("template") {
                self.load_template(expr)?;
            } else if expr.is_call("generator") {
                self.load_generator(expr)?;
            }
        }

        // Resolve pending relations
        self.resolve_relations(world)?;

        // Second pass: load rules and stubs (stubs need entities to be resolved first)
        for expr in exprs {
            if expr.is_call("rule") {
                self.load_rule(rules, expr)?;
            } else if expr.is_call("stub") {
                self.load_stub(expr)?;
            }
        }

        Ok(())
    }

    /// Load an entity definition.
    fn load_entity(&mut self, world: &mut World, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "entity requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("entity name must be a symbol".to_string())
        })?;

        let entity = world.create_entity();
        self.entity_names.insert(name, entity);

        // Process components and relations
        for item in &items[2..] {
            let comp = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("entity component must be a list".to_string())
            })?;

            if comp.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "component must have name and value".to_string(),
                ));
            }

            let comp_name = comp[0].as_symbol().ok_or_else(|| {
                LoadError::InvalidDefinition("component name must be a symbol".to_string())
            })?;

            // Check if this is a relation (value is a symbol that could be an entity name)
            if let Some(target_name) = comp[1].as_symbol() {
                // Could be a relation - defer resolution
                self.pending_relations
                    .push((entity, comp_name, target_name));
            } else {
                // Regular component
                let value = self.expr_to_value(&comp[1])?;
                world.set_component(entity, comp_name.as_str().as_str(), value);
            }
        }

        Ok(())
    }

    /// Load a relation schema.
    fn load_relation(&mut self, world: &mut World, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "relation requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("relation name must be a symbol".to_string())
        })?;

        let mut from_card = Cardinality::Many;
        let mut to_card = Cardinality::One;

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("from") && i + 1 < items.len() {
                from_card = self.parse_cardinality(&items[i + 1])?;
                i += 2;
            } else if items[i].is_keyword("to") && i + 1 < items.len() {
                to_card = self.parse_cardinality(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        world.register_relation(RelationSchema::new(
            name.as_str().as_str(),
            from_card,
            to_card,
        ));
        Ok(())
    }

    /// Parse cardinality from :one or :many.
    fn parse_cardinality(&self, expr: &SExpr) -> Result<Cardinality, LoadError> {
        if expr.is_keyword("one") {
            Ok(Cardinality::One)
        } else if expr.is_keyword("many") {
            Ok(Cardinality::Many)
        } else {
            Err(LoadError::InvalidDefinition(
                "cardinality must be :one or :many".to_string(),
            ))
        }
    }

    /// Resolve pending relations after all entities are created.
    fn resolve_relations(&mut self, world: &mut World) -> Result<(), LoadError> {
        for (from, rel_name, target_name) in std::mem::take(&mut self.pending_relations) {
            if let Some(&target) = self.entity_names.get(&target_name) {
                // It's a relation to another entity
                world.add_relation(rel_name.as_str().as_str(), from, target);
            } else {
                // Not an entity - treat as a symbol component value
                world.set_component(from, rel_name.as_str().as_str(), Value::Symbol(target_name));
            }
        }
        Ok(())
    }

    /// Load a rule definition.
    fn load_rule(&self, rules: &mut RuleSet, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "rule requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("rule name must be a symbol".to_string())
        })?;

        let mut pattern: Option<Pattern> = None;
        let mut trigger: Option<Trigger> = None;
        let mut effect: Option<Effect> = None;

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("pattern") && i + 1 < items.len() {
                pattern = Some(self.parse_pattern(&items[i + 1])?);
                i += 2;
            } else if items[i].is_keyword("every") && i + 1 < items.len() {
                let interval = items[i + 1].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition(":every requires an integer".to_string())
                })?;
                trigger = Some(Trigger::every(interval as u64));
                i += 2;
            } else if items[i].is_keyword("effect") && i + 1 < items.len() {
                effect = Some(self.parse_effect(&items[i + 1])?);
                i += 2;
            } else {
                i += 1;
            }
        }

        let pattern = pattern.unwrap_or_else(|| Pattern::entity("?e"));
        let trigger = trigger.unwrap_or_else(|| Trigger::every(1));
        let effect = effect
            .ok_or_else(|| LoadError::InvalidDefinition("rule requires an effect".to_string()))?;

        rules.add_rule(Rule::new(name.as_str().as_str(), pattern, trigger, effect));
        Ok(())
    }

    /// Load a template definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (template goblin
    ///   :doc "A small green creature"
    ///   :tags (:creature :hostile)
    ///   :fields
    ///     ((Name "Goblin")
    ///      (HP (range 8 15))
    ///      (Speed (range-f64 0.8 1.2))
    ///      (Element (choice :fire :ice :lightning))
    ///      (HasPoison (chance 0.3))))
    /// ```
    fn load_template(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "template requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("template name must be a symbol".to_string())
        })?;

        let mut template = Template::new(name);

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("doc") && i + 1 < items.len() {
                if let Some(doc) = items[i + 1].as_string() {
                    template.set_doc(doc);
                }
                i += 2;
            } else if items[i].is_keyword("tags") && i + 1 < items.len() {
                self.parse_template_tags(&mut template, &items[i + 1])?;
                i += 2;
            } else if items[i].is_keyword("fields") && i + 1 < items.len() {
                self.parse_template_fields(&mut template, &items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        self.templates.register(template);
        Ok(())
    }

    /// Load a generator definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (generator treasure-gen
    ///   :doc "Generates treasure for containers"
    ///   :produces (:Contents :Value))
    /// ```
    ///
    /// Note: Generator bodies must be registered in Rust code.
    /// The DSL defines generator metadata/schema only.
    fn load_generator(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "generator requires a name".to_string(),
            ));
        }

        let name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("generator name must be a symbol".to_string())
        })?;

        let mut doc: Option<String> = None;
        let mut produces: Vec<Symbol> = Vec::new();

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("doc") && i + 1 < items.len() {
                if let Some(d) = items[i + 1].as_string() {
                    doc = Some(d.to_string());
                }
                i += 2;
            } else if items[i].is_keyword("produces") && i + 1 < items.len() {
                produces = self.parse_symbol_list(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        // Register a placeholder generator that will be overwritten by Rust code.
        // This allows the DSL to declare generator interfaces.
        self.generators.register_with_doc(
            name.as_str(),
            doc,
            produces.iter().map(|s| s.as_str().to_string()).collect(),
            |_world, _entity, _rng, _params| {
                // Placeholder: real implementation must be provided in Rust
                crate::Value::Bool(false)
            },
        );

        Ok(())
    }

    /// Load a stub definition.
    ///
    /// Syntax:
    /// ```lisp
    /// (stub entity-name :component ComponentName :generator gen-name :seed 12345
    ///   :params ((key value) ...))
    /// ```
    fn load_stub(&mut self, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr.as_list().unwrap();
        if items.len() < 2 {
            return Err(LoadError::InvalidDefinition(
                "stub requires an entity name".to_string(),
            ));
        }

        let entity_name = items[1].as_symbol().ok_or_else(|| {
            LoadError::InvalidDefinition("stub entity must be a symbol".to_string())
        })?;

        let entity = self
            .entity_names
            .get(&entity_name)
            .copied()
            .ok_or_else(|| LoadError::UnknownEntity(entity_name.as_str().to_string()))?;

        let mut component: Option<Symbol> = None;
        let mut generator: Option<Symbol> = None;
        let mut seed: u64 = 0;
        let mut params: OrdMap<String, Value> = OrdMap::new();

        // Parse keyword arguments
        let mut i = 2;
        while i < items.len() {
            if items[i].is_keyword("component") && i + 1 < items.len() {
                component = items[i + 1]
                    .as_symbol()
                    .or_else(|| items[i + 1].as_keyword());
                i += 2;
            } else if items[i].is_keyword("generator") && i + 1 < items.len() {
                generator = items[i + 1].as_symbol();
                i += 2;
            } else if items[i].is_keyword("seed") && i + 1 < items.len() {
                seed = items[i + 1].as_int().unwrap_or(0) as u64;
                i += 2;
            } else if items[i].is_keyword("params") && i + 1 < items.len() {
                params = self.parse_params(&items[i + 1])?;
                i += 2;
            } else {
                i += 1;
            }
        }

        let component = component
            .ok_or_else(|| LoadError::InvalidDefinition("stub requires :component".to_string()))?;
        let generator = generator
            .ok_or_else(|| LoadError::InvalidDefinition("stub requires :generator".to_string()))?;

        let stub = GenerationStub::new(generator, seed).with_params(params);
        let component_str = component.as_str();
        self.stubs.add(entity, &*component_str, stub);

        Ok(())
    }

    /// Parse a list of symbols/keywords.
    fn parse_symbol_list(&self, expr: &SExpr) -> Result<Vec<Symbol>, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("expected a list".to_string()))?;

        let mut result = Vec::new();
        for item in items {
            if let Some(s) = item.as_symbol() {
                result.push(s);
            } else if let Some(k) = item.as_keyword() {
                result.push(k);
            }
        }
        Ok(result)
    }

    /// Parse params from a list of (key value) pairs.
    fn parse_params(&self, expr: &SExpr) -> Result<OrdMap<String, Value>, LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("params must be a list".to_string()))?;

        let mut result = OrdMap::new();
        for item in items {
            let pair = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("param must be a (key value) pair".to_string())
            })?;

            if pair.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "param must have key and value".to_string(),
                ));
            }

            let key = pair[0]
                .as_symbol()
                .or_else(|| pair[0].as_keyword())
                .ok_or_else(|| {
                    LoadError::InvalidDefinition("param key must be a symbol".to_string())
                })?;

            let value = self.expr_to_value(&pair[1])?;
            result.insert(key.as_str().to_string(), value);
        }

        Ok(result)
    }

    /// Parse template tags from a list of keywords.
    fn parse_template_tags(&self, template: &mut Template, expr: &SExpr) -> Result<(), LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("tags must be a list".to_string()))?;

        for item in items {
            if let Some(tag) = item.as_keyword() {
                template.add_tag(tag);
            } else if let Some(tag) = item.as_symbol() {
                template.add_tag(tag);
            }
        }

        Ok(())
    }

    /// Parse template fields from a list of field definitions.
    fn parse_template_fields(
        &self,
        template: &mut Template,
        expr: &SExpr,
    ) -> Result<(), LoadError> {
        let items = expr
            .as_list()
            .ok_or_else(|| LoadError::InvalidDefinition("fields must be a list".to_string()))?;

        for item in items {
            let field_def = item.as_list().ok_or_else(|| {
                LoadError::InvalidDefinition("field definition must be a list".to_string())
            })?;

            if field_def.len() != 2 {
                return Err(LoadError::InvalidDefinition(
                    "field definition must have name and spec".to_string(),
                ));
            }

            let field_name = field_def[0].as_symbol().ok_or_else(|| {
                LoadError::InvalidDefinition("field name must be a symbol".to_string())
            })?;

            let field_spec = self.parse_field_spec(&field_def[1])?;
            template.set_field(field_name, field_spec);
        }

        Ok(())
    }

    /// Parse a field specification.
    ///
    /// Supports:
    /// - Literal values (42, "hello", :keyword)
    /// - (range min max) for integer ranges
    /// - (range-f64 min max) for float ranges
    /// - (choice v1 v2 ...) for random choice
    /// - (chance probability) for boolean chance
    fn parse_field_spec(&self, expr: &SExpr) -> Result<FieldSpec, LoadError> {
        // Check for special forms
        if let Some(items) = expr.as_list() {
            if items.is_empty() {
                return Err(LoadError::InvalidDefinition("empty field spec".to_string()));
            }

            // (range min max)
            if items[0].is_symbol("range") {
                if items.len() != 3 {
                    return Err(LoadError::InvalidDefinition(
                        "range requires min and max".to_string(),
                    ));
                }
                let min = items[1].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition("range min must be an integer".to_string())
                })?;
                let max = items[2].as_int().ok_or_else(|| {
                    LoadError::InvalidDefinition("range max must be an integer".to_string())
                })?;
                return Ok(FieldSpec::range(min, max));
            }

            // (range-f64 min max)
            if items[0].is_symbol("range-f64") {
                if items.len() != 3 {
                    return Err(LoadError::InvalidDefinition(
                        "range-f64 requires min and max".to_string(),
                    ));
                }
                let min = self.parse_float(&items[1])?;
                let max = self.parse_float(&items[2])?;
                return Ok(FieldSpec::range_f64(min, max));
            }

            // (choice v1 v2 ...)
            if items[0].is_symbol("choice") {
                let values: Result<Vec<Value>, _> =
                    items[1..].iter().map(|e| self.expr_to_value(e)).collect();
                return Ok(FieldSpec::choice(values?));
            }

            // (chance probability)
            if items[0].is_symbol("chance") {
                if items.len() != 2 {
                    return Err(LoadError::InvalidDefinition(
                        "chance requires a probability".to_string(),
                    ));
                }
                let prob = self.parse_float(&items[1])?;
                return Ok(FieldSpec::chance(prob));
            }
        }

        // Otherwise, it's a fixed value
        let value = self.expr_to_value(expr)?;
        Ok(FieldSpec::fixed(value))
    }

    /// Parse a float from an expression (int or float).
    fn parse_float(&self, expr: &SExpr) -> Result<f64, LoadError> {
        if let Some(f) = expr.as_float() {
            Ok(f)
        } else if let Some(i) = expr.as_int() {
            Ok(i as f64)
        } else {
            Err(LoadError::InvalidDefinition(
                "expected a number".to_string(),
            ))
        }
    }

    /// Parse a pattern expression.
    fn parse_pattern(&self, expr: &SExpr) -> Result<Pattern, LoadError> {
        // Simple pattern: (= (get ?e Name) "goat")
        if expr.is_call("=") || expr.is_call("==") {
            let items = expr.as_list().unwrap();
            if items.len() != 3 {
                return Err(LoadError::InvalidDefinition(
                    "= requires 2 arguments".to_string(),
                ));
            }

            // Check for (get ?e Component) form
            if items[1].is_call("get") || items[1].is_call("get-component") {
                let get_items = items[1].as_list().unwrap();
                if get_items.len() == 3 {
                    let var = self.parse_var(&get_items[1])?;
                    let comp = self.parse_component_name(&get_items[2])?;
                    let value = self.expr_to_value(&items[2])?;
                    return Ok(Pattern::component_value(
                        var.as_str().as_str(),
                        comp.as_str().as_str(),
                        value,
                    ));
                }
            }
        }

        // Pattern: (has ?e :Component)
        if expr.is_call("has") || expr.is_call("has-component") {
            let items = expr.as_list().unwrap();
            if items.len() == 3 {
                let var = self.parse_var(&items[1])?;
                let comp = self.parse_component_name(&items[2])?;
                return Ok(Pattern::has_component(
                    var.as_str().as_str(),
                    comp.as_str().as_str(),
                ));
            }
        }

        // Pattern: (entity ?e)
        if expr.is_call("entity") {
            let items = expr.as_list().unwrap();
            if items.len() == 2 {
                let var = self.parse_var(&items[1])?;
                return Ok(Pattern::entity(var.as_str().as_str()));
            }
        }

        // Default: match any entity
        Ok(Pattern::entity("?e"))
    }

    /// Parse a variable like ?e.
    fn parse_var(&self, expr: &SExpr) -> Result<Symbol, LoadError> {
        expr.as_symbol()
            .ok_or_else(|| LoadError::InvalidDefinition("expected variable symbol".to_string()))
    }

    /// Parse a component name (symbol or keyword).
    fn parse_component_name(&self, expr: &SExpr) -> Result<Symbol, LoadError> {
        expr.as_symbol()
            .or_else(|| expr.as_keyword())
            .ok_or_else(|| LoadError::InvalidDefinition("expected component name".to_string()))
    }

    /// Parse an effect expression.
    fn parse_effect(&self, expr: &SExpr) -> Result<Effect, LoadError> {
        if expr.is_call("emit-message") || expr.is_call("emit") {
            let items = expr.as_list().unwrap();
            if items.len() >= 2 {
                let msg = items[1].as_string().ok_or_else(|| {
                    LoadError::InvalidDefinition("emit-message requires a string".to_string())
                })?;
                return Ok(Effect::emit_message(msg));
            }
        }

        Err(LoadError::InvalidDefinition(format!(
            "unknown effect: {expr}"
        )))
    }

    /// Convert an expression to a Value.
    fn expr_to_value(&self, expr: &SExpr) -> Result<Value, LoadError> {
        match expr {
            SExpr::Atom(atom, _) => match atom {
                Atom::Int(n) => Ok(Value::Int(*n)),
                Atom::Float(n) => Ok(Value::Float(*n)),
                Atom::Bool(b) => Ok(Value::Bool(*b)),
                Atom::String(s) => Ok(Value::string(s.clone())),
                Atom::Symbol(s) => Ok(Value::Symbol(*s)),
                Atom::Keyword(k) => Ok(Value::Symbol(*k)),
            },
            SExpr::List(_, _) => Err(LoadError::InvalidDefinition(
                "list values not supported yet".to_string(),
            )),
        }
    }

    /// Get an entity by name.
    pub fn get_entity(&self, name: &str) -> Option<EntityId> {
        self.entity_names.get(&Symbol::new(name)).copied()
    }
}

impl Default for WorldLoader {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_entity() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"(entity goat (Name "goat") (HP 10))"#,
            )
            .unwrap();

        let goat = loader.get_entity("goat").unwrap();
        assert_eq!(
            world.get_component(goat, "Name"),
            Some(&Value::string("goat"))
        );
        assert_eq!(world.get_component(goat, "HP"), Some(&Value::Int(10)));
    }

    #[test]
    fn test_load_relation() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (relation Location :from :many :to :one)
                (entity room (Name "A room"))
                (entity goat (Name "goat") (Location room))
                "#,
            )
            .unwrap();

        let room = loader.get_entity("room").unwrap();
        let goat = loader.get_entity("goat").unwrap();

        assert!(world.has_relation("Location", goat, room));
    }

    #[test]
    fn test_load_rule() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (rule goat-baas
                  :pattern (= (get ?e Name) "goat")
                  :every 10
                  :effect (emit-message "Baa!"))
                "#,
            )
            .unwrap();

        assert_eq!(rules.len(), 1);
    }

    #[test]
    fn test_full_example() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                ; Relation schema
                (relation Location :from :many :to :one)

                ; Entities
                (entity room (Name "A small room"))
                (entity goat (Name "goat") (HP 10) (Location room))

                ; Rule
                (rule goat-baas
                  :pattern (= (get ?e Name) "goat")
                  :every 10
                  :effect (emit-message "The goat says: Baa!"))
                "#,
            )
            .unwrap();

        assert_eq!(world.entity_count(), 2);
        assert_eq!(rules.len(), 1);

        let goat = loader.get_entity("goat").unwrap();
        assert_eq!(
            world.get_component(goat, "Name"),
            Some(&Value::string("goat"))
        );
    }

    #[test]
    fn test_load_template_basic() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :doc "A small green creature"
                  :fields
                    ((Name "Goblin")
                     (HP (range 8 15))))
                "#,
            )
            .unwrap();

        assert!(loader.templates().contains("goblin"));
        let template = loader.templates().get("goblin").unwrap();
        assert_eq!(template.doc(), Some("A small green creature"));
    }

    #[test]
    fn test_load_template_with_tags() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :tags (:creature :hostile)
                  :fields ((Name "Goblin")))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("goblin").unwrap();
        assert!(template.has_tag("creature"));
        assert!(template.has_tag("hostile"));
    }

    #[test]
    fn test_load_template_all_field_specs() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template test-creature
                  :fields
                    ((Name "Test")
                     (HP (range 10 20))
                     (Speed (range-f64 0.5 1.5))
                     (Element (choice :fire :ice :lightning))
                     (HasPoison (chance 0.5))))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("test-creature").unwrap();

        // Verify field specs exist
        assert!(template.get_field("Name").is_some());
        assert!(template.get_field("HP").is_some());
        assert!(template.get_field("Speed").is_some());
        assert!(template.get_field("Element").is_some());
        assert!(template.get_field("HasPoison").is_some());
    }

    #[test]
    fn test_load_template_instantiate() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (template goblin
                  :fields
                    ((Name "Goblin")
                     (HP (range 8 15))
                     (Strength (range 6 12))))
                "#,
            )
            .unwrap();

        let template = loader.templates().get("goblin").unwrap();
        let entity = template.instantiate_with_seed(&mut world, 12345);

        // Check the entity was created with expected components
        assert_eq!(
            world.get_component(entity, "Name"),
            Some(&Value::string("Goblin"))
        );

        // HP should be in range
        if let Some(Value::Int(hp)) = world.get_component(entity, "HP") {
            assert!((8..=15).contains(hp), "HP {hp} not in range 8-15");
        } else {
            panic!("Expected HP to be Int");
        }
    }

    #[test]
    fn test_load_generator() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator treasure-gen
                  :doc "Generates treasure for containers"
                  :produces (:Contents :Value))
                "#,
            )
            .unwrap();

        assert!(loader.generators().contains("treasure-gen"));
        let generator = loader.generators().get("treasure-gen").unwrap();
        assert_eq!(generator.doc(), Some("Generates treasure for containers"));
    }

    #[test]
    fn test_load_stub() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator item-gen :doc "Generates items")
                (entity chest (Name "Treasure Chest"))
                (stub chest :component Contents :generator item-gen :seed 12345)
                "#,
            )
            .unwrap();

        let chest = loader.get_entity("chest").unwrap();

        // Check that the stub was registered
        assert!(loader.stubs().has(chest, "Contents"));

        let stub = loader.stubs().get(chest, "Contents").unwrap();
        assert_eq!(stub.generator().as_str(), "item-gen");
        assert_eq!(stub.seed(), 12345);
    }

    #[test]
    fn test_load_stub_with_params() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        loader
            .load_str(
                &mut world,
                &mut rules,
                r#"
                (generator item-gen :doc "Generates items")
                (entity chest (Name "Treasure Chest"))
                (stub chest :component Contents :generator item-gen :seed 42
                  :params ((min-value 100) (max-value 500)))
                "#,
            )
            .unwrap();

        let chest = loader.get_entity("chest").unwrap();
        let stub = loader.stubs().get(chest, "Contents").unwrap();

        assert_eq!(stub.get_param("min-value"), Some(&Value::Int(100)));
        assert_eq!(stub.get_param("max-value"), Some(&Value::Int(500)));
    }

    #[test]
    fn test_stub_unknown_entity_error() {
        let mut world = World::new();
        let mut rules = RuleSet::new();
        let mut loader = WorldLoader::new();

        let result = loader.load_str(
            &mut world,
            &mut rules,
            r#"
            (generator item-gen)
            (stub nonexistent :component Contents :generator item-gen :seed 1)
            "#,
        );

        assert!(matches!(result, Err(LoadError::UnknownEntity(_))));
    }
}
