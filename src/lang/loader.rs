//! World loader - loads DSL definitions into the World.

use crate::Value;
use crate::core::{Cardinality, EntityId, RelationSchema, World};
use crate::lang::{Atom, ParseError, SExpr, parse_all};
use crate::rules::{Effect, Pattern, Rule, RuleSet, Trigger};
use crate::symbol::Symbol;
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
}

impl WorldLoader {
    pub fn new() -> Self {
        Self {
            entity_names: OrdMap::new(),
            pending_relations: Vec::new(),
        }
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
        // First pass: create entities and register relations
        for expr in exprs {
            if expr.is_call("entity") {
                self.load_entity(world, expr)?;
            } else if expr.is_call("relation") {
                self.load_relation(world, expr)?;
            }
        }

        // Resolve pending relations
        self.resolve_relations(world)?;

        // Second pass: load rules
        for expr in exprs {
            if expr.is_call("rule") {
                self.load_rule(rules, expr)?;
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
}
