//! Type predicate evaluation for the grammar system.
//!
//! This module evaluates type predicates against entities to determine
//! if they satisfy semantic constraints like "portable" or "container".

use crate::core::{ComponentTypeId, EntityId, RelationTypeId, World};
use crate::lang::{Atom, SExpr};
use crate::symbol::Symbol;

use super::types::TypeRegistry;

/// Evaluates type predicates against entities.
#[derive(Debug)]
pub struct PredicateEvaluator<'a> {
    types: &'a TypeRegistry,
}

impl<'a> PredicateEvaluator<'a> {
    /// Create a new predicate evaluator.
    pub fn new(types: &'a TypeRegistry) -> Self {
        Self { types }
    }

    /// Check if an entity satisfies a type predicate.
    ///
    /// # Arguments
    /// * `world` - The world state
    /// * `actor` - The actor performing the action (for context)
    /// * `type_name` - Name of the type to check
    /// * `entity` - The entity to check
    ///
    /// # Returns
    /// `true` if the entity satisfies the type, `false` otherwise.
    /// Returns `true` if the type is not registered (permissive by default).
    pub fn check_type(
        &self,
        world: &World,
        actor: EntityId,
        type_name: Symbol,
        entity: EntityId,
    ) -> bool {
        match self.types.get(type_name) {
            Some(type_pred) => self.eval_predicate(world, actor, entity, &type_pred.predicate),
            None => {
                // Unknown type - be permissive for now
                // In the future, this could be an error
                true
            }
        }
    }

    /// Evaluate a predicate expression.
    ///
    /// The expression can reference:
    /// - `entity` - The candidate entity
    /// - `actor` - The actor performing the action
    fn eval_predicate(
        &self,
        world: &World,
        actor: EntityId,
        entity: EntityId,
        expr: &SExpr,
    ) -> bool {
        match expr {
            // Literal boolean
            SExpr::Atom(Atom::Bool(b), _) => *b,

            // Function call
            SExpr::List(items, _) if !items.is_empty() => {
                if let SExpr::Atom(Atom::Symbol(func), _) = &items[0] {
                    let func_name = func.as_str();
                    self.eval_builtin(world, actor, entity, &func_name, &items[1..])
                } else {
                    false
                }
            }

            // Anything else is false
            _ => false,
        }
    }

    /// Evaluate a built-in predicate function.
    fn eval_builtin(
        &self,
        world: &World,
        actor: EntityId,
        entity: EntityId,
        func: &str,
        args: &[SExpr],
    ) -> bool {
        match func {
            // (has? entity :Component) - Check if entity has component
            "has?" => {
                if args.len() >= 2 {
                    let target = self.resolve_entity_arg(actor, entity, &args[0]);
                    if let Some(component) = self.get_keyword_arg(&args[1]) {
                        let comp_id = ComponentTypeId::new(&component);
                        return world.get_component(target, comp_id).is_some();
                    }
                }
                false
            }

            // (held-by? entity actor) - Check if entity is held by actor
            "held-by?" => {
                if args.len() >= 2 {
                    let target = self.resolve_entity_arg(actor, entity, &args[0]);
                    let holder = self.resolve_entity_arg(actor, entity, &args[1]);
                    let contains_rel = RelationTypeId::new("Contains");
                    let contained = world.query_relation_forward(contains_rel, holder);
                    return contained.contains(&target);
                }
                false
            }

            // (in-room? entity room) - Check if entity is in room
            "in-room?" => {
                if args.len() >= 2 {
                    let target = self.resolve_entity_arg(actor, entity, &args[0]);
                    let room = self.resolve_entity_arg(actor, entity, &args[1]);
                    let in_room_rel = RelationTypeId::new("InRoom");
                    let rooms = world.query_relation_forward(in_room_rel, target);
                    return rooms.contains(&room);
                }
                false
            }

            // (= expr1 expr2) - Equality check
            "=" | "eq?" => {
                if args.len() >= 2 {
                    let val1 = self.eval_value(world, actor, entity, &args[0]);
                    let val2 = self.eval_value(world, actor, entity, &args[1]);
                    return val1 == val2;
                }
                false
            }

            // (and expr...) - Logical and
            "and" => args
                .iter()
                .all(|arg| self.eval_predicate(world, actor, entity, arg)),

            // (or expr...) - Logical or
            "or" => args
                .iter()
                .any(|arg| self.eval_predicate(world, actor, entity, arg)),

            // (not expr) - Logical not
            "not" => {
                if !args.is_empty() {
                    !self.eval_predicate(world, actor, entity, &args[0])
                } else {
                    false
                }
            }

            // (get entity :Component) - Get component value (for comparisons)
            "get" => {
                // This should be used in equality checks, not as a predicate
                false
            }

            _ => false,
        }
    }

    /// Resolve an entity argument.
    ///
    /// Handles:
    /// - `entity` symbol -> the candidate entity
    /// - `actor` symbol -> the actor
    fn resolve_entity_arg(&self, actor: EntityId, entity: EntityId, arg: &SExpr) -> EntityId {
        match arg {
            SExpr::Atom(Atom::Symbol(s), _) => {
                let s_str = s.as_str();
                if s_str == "entity" {
                    entity
                } else if s_str == "actor" {
                    actor
                } else {
                    entity // Default to entity
                }
            }
            _ => entity,
        }
    }

    /// Get a keyword argument as a string.
    ///
    /// Keywords look like `:Portable` in the DSL.
    fn get_keyword_arg(&self, arg: &SExpr) -> Option<String> {
        match arg {
            SExpr::Atom(Atom::Keyword(k), _) => Some(k.as_str().to_string()),
            SExpr::Atom(Atom::Symbol(s), _) => {
                // Allow symbols that start with colon as keywords
                let s_str = s.as_str();
                if let Some(stripped) = s_str.strip_prefix(':') {
                    Some(stripped.to_string())
                } else {
                    Some(s_str.to_string())
                }
            }
            _ => None,
        }
    }

    /// Evaluate an expression to a value for comparison.
    fn eval_value(
        &self,
        world: &World,
        actor: EntityId,
        entity: EntityId,
        expr: &SExpr,
    ) -> Option<crate::Value> {
        match expr {
            SExpr::Atom(Atom::Int(i), _) => Some(crate::Value::Int(*i)),
            SExpr::Atom(Atom::Float(f), _) => Some(crate::Value::Float(*f)),
            SExpr::Atom(Atom::Bool(b), _) => Some(crate::Value::Bool(*b)),
            SExpr::Atom(Atom::String(s), _) => Some(crate::Value::String(s.clone().into())),
            SExpr::Atom(Atom::Keyword(k), _) => {
                Some(crate::Value::Symbol(Symbol::new(&k.as_str())))
            }
            SExpr::Atom(Atom::Symbol(s), _) => {
                // Check for entity/actor references
                let s_str = s.as_str();
                if s_str == "entity" {
                    Some(crate::Value::EntityRef(entity))
                } else if s_str == "actor" {
                    Some(crate::Value::EntityRef(actor))
                } else {
                    Some(crate::Value::Symbol(*s))
                }
            }
            SExpr::List(items, _) if !items.is_empty() => {
                // Handle (get entity :Component) calls
                if let SExpr::Atom(Atom::Symbol(func), _) = &items[0] {
                    if func.as_str() == "get" && items.len() >= 3 {
                        let target = self.resolve_entity_arg(actor, entity, &items[1]);
                        if let Some(component) = self.get_keyword_arg(&items[2]) {
                            let comp_id = ComponentTypeId::new(&component);
                            return world.get_component(target, comp_id).cloned();
                        }
                    }
                }
                None
            }
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::types::TypePredicate;
    use crate::lang::Span;

    fn dummy_span() -> Span {
        Span {
            start: 0,
            end: 0,
            line: 1,
            column: 1,
        }
    }

    fn setup_world() -> (World, EntityId, EntityId, EntityId) {
        use crate::core::{Cardinality, ComponentTypeId, RelationSchema, RelationTypeId, Value};

        let mut world = World::new();

        // Register relation schemas (required for relation queries to work)
        world.register_relation(RelationSchema::new(
            "InRoom",
            Cardinality::Many,
            Cardinality::One,
        ));
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        // Create a room
        let room = world.create_entity();
        world.set_component(room, ComponentTypeId::new("IsRoom"), Value::Bool(true));

        // Create an actor
        let actor = world.create_entity();
        world.add_relation(RelationTypeId::new("InRoom"), actor, room);

        // Create a portable lamp
        let lamp = world.create_entity();
        world.set_component(lamp, ComponentTypeId::new("Portable"), Value::Bool(true));
        world.set_component(
            lamp,
            ComponentTypeId::new("Name"),
            Value::String("lamp".into()),
        );
        world.add_relation(RelationTypeId::new("InRoom"), lamp, room);

        (world, room, actor, lamp)
    }

    #[test]
    fn test_has_predicate() {
        let (world, _, actor, lamp) = setup_world();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        // (has? entity :Portable)
        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                SExpr::Atom(Atom::Keyword(Symbol::new("Portable")), dummy_span()),
            ],
            dummy_span(),
        );

        assert!(evaluator.eval_predicate(&world, actor, lamp, &expr));

        // (has? entity :Container) - lamp doesn't have Container
        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                SExpr::Atom(Atom::Keyword(Symbol::new("Container")), dummy_span()),
            ],
            dummy_span(),
        );

        assert!(!evaluator.eval_predicate(&world, actor, lamp, &expr));
    }

    #[test]
    fn test_type_registry_lookup() {
        let (world, _, actor, lamp) = setup_world();

        let mut types = TypeRegistry::new();

        // Register "portable" type
        let portable_pred = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                SExpr::Atom(Atom::Keyword(Symbol::new("Portable")), dummy_span()),
            ],
            dummy_span(),
        );
        types.register(TypePredicate::new("portable", portable_pred));

        let evaluator = PredicateEvaluator::new(&types);

        // Check type
        assert!(evaluator.check_type(&world, actor, Symbol::new("portable"), lamp));

        // Unknown type is permissive
        assert!(evaluator.check_type(&world, actor, Symbol::new("unknown"), lamp));
    }

    #[test]
    fn test_held_by_predicate() {
        use crate::core::{Cardinality, RelationSchema, RelationTypeId};

        let mut world = World::new();

        // Register relation schemas (required for relation queries to work)
        world.register_relation(RelationSchema::new(
            "Contains",
            Cardinality::One,
            Cardinality::Many,
        ));

        let actor = world.create_entity();
        let item = world.create_entity();

        // Item is not held initially
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("held-by?")), dummy_span()),
                SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                SExpr::Atom(Atom::Symbol(Symbol::new("actor")), dummy_span()),
            ],
            dummy_span(),
        );

        assert!(!evaluator.eval_predicate(&world, actor, item, &expr));

        // Now actor holds item
        world.add_relation(RelationTypeId::new("Contains"), actor, item);
        assert!(evaluator.eval_predicate(&world, actor, item, &expr));
    }

    #[test]
    fn test_and_predicate() {
        let (world, _, actor, lamp) = setup_world();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        // (and (has? entity :Portable) (has? entity :Name))
        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("and")), dummy_span()),
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                        SExpr::Atom(Atom::Keyword(Symbol::new("Portable")), dummy_span()),
                    ],
                    dummy_span(),
                ),
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                        SExpr::Atom(Atom::Keyword(Symbol::new("Name")), dummy_span()),
                    ],
                    dummy_span(),
                ),
            ],
            dummy_span(),
        );

        assert!(evaluator.eval_predicate(&world, actor, lamp, &expr));

        // (and (has? entity :Portable) (has? entity :Container)) - fails
        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("and")), dummy_span()),
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                        SExpr::Atom(Atom::Keyword(Symbol::new("Portable")), dummy_span()),
                    ],
                    dummy_span(),
                ),
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                        SExpr::Atom(Atom::Keyword(Symbol::new("Container")), dummy_span()),
                    ],
                    dummy_span(),
                ),
            ],
            dummy_span(),
        );

        assert!(!evaluator.eval_predicate(&world, actor, lamp, &expr));
    }

    #[test]
    fn test_not_predicate() {
        let (world, _, actor, lamp) = setup_world();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        // (not (has? entity :Container))
        let expr = SExpr::List(
            vec![
                SExpr::Atom(Atom::Symbol(Symbol::new("not")), dummy_span()),
                SExpr::List(
                    vec![
                        SExpr::Atom(Atom::Symbol(Symbol::new("has?")), dummy_span()),
                        SExpr::Atom(Atom::Symbol(Symbol::new("entity")), dummy_span()),
                        SExpr::Atom(Atom::Keyword(Symbol::new("Container")), dummy_span()),
                    ],
                    dummy_span(),
                ),
            ],
            dummy_span(),
        );

        assert!(evaluator.eval_predicate(&world, actor, lamp, &expr));
    }

    #[test]
    fn test_literal_bool() {
        let world = World::new();
        let actor = EntityId::from_raw(1);
        let entity = EntityId::from_raw(2);
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        assert!(evaluator.eval_predicate(
            &world,
            actor,
            entity,
            &SExpr::Atom(Atom::Bool(true), dummy_span())
        ));
        assert!(!evaluator.eval_predicate(
            &world,
            actor,
            entity,
            &SExpr::Atom(Atom::Bool(false), dummy_span())
        ));
    }
}
