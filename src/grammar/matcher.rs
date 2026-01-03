//! Form matching for the grammar system.
//!
//! This module matches input tokens against form patterns, resolving
//! entity references and validating type constraints.

use crate::core::{EntityId, World};
use crate::input::{ResolutionResult, Resolver};
use crate::symbol::Symbol;
use im::OrdMap;
use std::sync::Arc;

use super::command::{Command, Form, FormElement, GrammarMatch, SlotValue};
use super::predicate::PredicateEvaluator;
use super::types::{SlotType, TypeRegistry};

/// Known directions for direction slot matching.
const DIRECTIONS: &[&str] = &[
    "north",
    "south",
    "east",
    "west",
    "up",
    "down",
    "northeast",
    "northwest",
    "southeast",
    "southwest",
    "n",
    "s",
    "e",
    "w",
    "u",
    "d",
    "ne",
    "nw",
    "se",
    "sw",
    "in",
    "out",
];

/// Known prepositions that terminate noun phrases.
const PREPOSITIONS: &[&str] = &[
    "in", "into", "on", "onto", "at", "to", "from", "with", "under", "behind", "through", "about",
    "for",
];

/// Result of attempting to match a form.
#[derive(Debug)]
pub enum MatchResult {
    /// Form matched successfully.
    Matched(OrdMap<Symbol, SlotValue>),
    /// Form didn't match (wrong structure).
    NoMatch,
    /// Form structure matched but type check failed.
    TypeCheckFailed {
        slot: Symbol,
        expected_type: SlotType,
        entity: EntityId,
    },
    /// Entity resolution failed.
    ResolutionFailed { slot: Symbol, phrase: Arc<str> },
    /// Ambiguous entity reference.
    Ambiguous {
        slot: Symbol,
        phrase: Arc<str>,
        candidates: Vec<(EntityId, Arc<str>)>,
    },
}

impl Command {
    /// Match remaining tokens against this command's forms.
    ///
    /// Forms are tried in priority order (higher priority first).
    /// Returns the first successful match.
    pub fn match_forms(
        &self,
        world: &World,
        actor: EntityId,
        tokens: &[&str],
        types: &TypeRegistry,
    ) -> Option<GrammarMatch> {
        let resolver = Resolver::new();
        let evaluator = PredicateEvaluator::new(types);

        for form in &self.forms {
            match form.try_match(world, actor, tokens, &resolver, &evaluator) {
                MatchResult::Matched(slots) => {
                    return Some(GrammarMatch::new(
                        form.action.clone(),
                        slots,
                        self.name,
                        form.priority,
                    ));
                }
                MatchResult::NoMatch
                | MatchResult::TypeCheckFailed { .. }
                | MatchResult::ResolutionFailed { .. }
                | MatchResult::Ambiguous { .. } => {
                    // Try next form
                    continue;
                }
            }
        }

        None
    }

    /// Match forms and return detailed error information.
    ///
    /// Unlike `match_forms`, this returns information about why matching failed,
    /// which can be used for better error messages.
    pub fn match_forms_detailed(
        &self,
        world: &World,
        actor: EntityId,
        tokens: &[&str],
        types: &TypeRegistry,
    ) -> Result<GrammarMatch, Vec<MatchResult>> {
        let resolver = Resolver::new();
        let evaluator = PredicateEvaluator::new(types);
        let mut failures = Vec::new();

        for form in &self.forms {
            match form.try_match(world, actor, tokens, &resolver, &evaluator) {
                MatchResult::Matched(slots) => {
                    return Ok(GrammarMatch::new(
                        form.action.clone(),
                        slots,
                        self.name,
                        form.priority,
                    ));
                }
                failure => {
                    failures.push(failure);
                }
            }
        }

        Err(failures)
    }
}

impl Form {
    /// Try to match this form against tokens.
    pub fn try_match(
        &self,
        world: &World,
        actor: EntityId,
        tokens: &[&str],
        resolver: &Resolver,
        evaluator: &PredicateEvaluator,
    ) -> MatchResult {
        // Empty pattern matches only if no tokens remain
        if self.pattern.is_empty() {
            return if tokens.is_empty() {
                MatchResult::Matched(OrdMap::new())
            } else {
                MatchResult::NoMatch
            };
        }

        let mut slots = OrdMap::new();
        let mut token_idx = 0;
        let mut elem_idx = 0;

        while elem_idx < self.pattern.len() {
            let elem = &self.pattern[elem_idx];

            match elem {
                FormElement::Word(word) => {
                    // Must have a token to match
                    if token_idx >= tokens.len() {
                        return MatchResult::NoMatch;
                    }
                    // Case-insensitive match
                    if tokens[token_idx].to_lowercase() != *word {
                        return MatchResult::NoMatch;
                    }
                    token_idx += 1;
                    elem_idx += 1;
                }
                FormElement::Slot { name, slot_type } => {
                    match slot_type {
                        SlotType::Direction => {
                            // Direction is a single token
                            if token_idx >= tokens.len() {
                                return MatchResult::NoMatch;
                            }
                            let dir_token = tokens[token_idx].to_lowercase();
                            if !DIRECTIONS.contains(&dir_token.as_str()) {
                                return MatchResult::NoMatch;
                            }
                            // Normalize direction
                            let normalized = normalize_direction(&dir_token);
                            slots.insert(*name, SlotValue::Direction(Symbol::new(normalized)));
                            token_idx += 1;
                            elem_idx += 1;
                        }
                        SlotType::Noun | SlotType::Custom(_) => {
                            // Noun phrase: consume tokens until next word element or preposition
                            if token_idx >= tokens.len() {
                                return MatchResult::NoMatch;
                            }

                            // Find where to stop consuming tokens
                            let stop_at = self.find_noun_end(elem_idx, tokens, token_idx);
                            if stop_at <= token_idx {
                                return MatchResult::NoMatch;
                            }

                            // Collect noun phrase
                            let phrase: Vec<&str> = tokens[token_idx..stop_at].to_vec();
                            let phrase_str: Arc<str> = phrase.join(" ").into();

                            // Resolve to entity
                            match resolver.resolve_noun_phrase(world, actor, &phrase_str) {
                                ResolutionResult::Resolved(entity) => {
                                    // Type check if custom type
                                    if let SlotType::Custom(type_name) = slot_type {
                                        if !evaluator.check_type(world, actor, *type_name, entity) {
                                            return MatchResult::TypeCheckFailed {
                                                slot: *name,
                                                expected_type: slot_type.clone(),
                                                entity,
                                            };
                                        }
                                    }
                                    slots.insert(*name, SlotValue::Entity(entity));
                                }
                                ResolutionResult::NotFound(_) => {
                                    return MatchResult::ResolutionFailed {
                                        slot: *name,
                                        phrase: phrase_str,
                                    };
                                }
                                ResolutionResult::Ambiguous(candidates) => {
                                    let candidate_info: Vec<(EntityId, Arc<str>)> = candidates
                                        .into_iter()
                                        .map(|c| (c.entity, c.name))
                                        .collect();
                                    return MatchResult::Ambiguous {
                                        slot: *name,
                                        phrase: phrase_str,
                                        candidates: candidate_info,
                                    };
                                }
                                ResolutionResult::NotApplicable => {
                                    // Treat as text
                                    slots.insert(*name, SlotValue::Text(phrase_str));
                                }
                            }

                            token_idx = stop_at;
                            elem_idx += 1;
                        }
                    }
                }
            }
        }

        // All pattern elements consumed; all tokens must also be consumed
        if token_idx == tokens.len() {
            MatchResult::Matched(slots)
        } else {
            MatchResult::NoMatch
        }
    }

    /// Find where a noun phrase ends.
    ///
    /// Noun phrases end when we hit:
    /// - The next Word element in the pattern
    /// - A preposition (if no more Word elements)
    /// - End of tokens
    fn find_noun_end(&self, current_elem: usize, tokens: &[&str], start_token: usize) -> usize {
        // Look ahead to find the next Word element
        let next_word = self.pattern[current_elem + 1..]
            .iter()
            .find_map(|e| match e {
                FormElement::Word(w) => Some(w.as_str()),
                _ => None,
            });

        let mut end = tokens.len();

        for (i, token) in tokens.iter().enumerate().skip(start_token) {
            let tok_lower = token.to_lowercase();

            // Stop at the next pattern word
            if let Some(word) = next_word {
                if tok_lower == word {
                    end = i;
                    break;
                }
            }

            // Stop at prepositions (unless it's the first token)
            if i > start_token && PREPOSITIONS.contains(&tok_lower.as_str()) {
                // Only stop if this preposition could be part of a following pattern
                if next_word.is_some() || self.pattern.len() > current_elem + 1 {
                    end = i;
                    break;
                }
            }
        }

        end
    }
}

/// Normalize direction abbreviations to full form.
fn normalize_direction(dir: &str) -> &'static str {
    match dir {
        "n" => "north",
        "s" => "south",
        "e" => "east",
        "w" => "west",
        "u" => "up",
        "d" => "down",
        "ne" => "northeast",
        "nw" => "northwest",
        "se" => "southeast",
        "sw" => "southwest",
        "north" => "north",
        "south" => "south",
        "east" => "east",
        "west" => "west",
        "up" => "up",
        "down" => "down",
        "northeast" => "northeast",
        "northwest" => "northwest",
        "southeast" => "southeast",
        "southwest" => "southwest",
        "in" => "in",
        "out" => "out",
        _ => "unknown", // This shouldn't happen if DIRECTIONS check passed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::World;
    use crate::grammar::command::FormAction;

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
        world.set_component(
            room,
            ComponentTypeId::new("Name"),
            Value::String("Test Room".into()),
        );

        // Create an actor (player)
        let actor = world.create_entity();
        world.set_component(
            actor,
            ComponentTypeId::new("Name"),
            Value::String("Player".into()),
        );
        world.add_relation(RelationTypeId::new("InRoom"), actor, room);

        // Create a portable object
        let lamp = world.create_entity();
        world.set_component(
            lamp,
            ComponentTypeId::new("Name"),
            Value::String("lamp".into()),
        );
        world.set_component(lamp, ComponentTypeId::new("Portable"), Value::Bool(true));
        world.add_relation(RelationTypeId::new("InRoom"), lamp, room);

        (world, room, actor, lamp)
    }

    #[test]
    fn test_empty_form_matches_empty_tokens() {
        let form = Form::new(vec![], FormAction::simple("look"));

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &[], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::Matched(_)));
    }

    #[test]
    fn test_empty_form_fails_with_tokens() {
        let form = Form::new(vec![], FormAction::simple("look"));

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &["extra"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::NoMatch));
    }

    #[test]
    fn test_word_element_matches() {
        let form = Form::new(vec![FormElement::word("at")], FormAction::simple("look-at"));

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &["at"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::Matched(_)));

        // Case insensitive
        let result = form.try_match(&world, actor, &["AT"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::Matched(_)));

        // Wrong word
        let result = form.try_match(&world, actor, &["in"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::NoMatch));
    }

    #[test]
    fn test_direction_slot() {
        let form = Form::new(
            vec![FormElement::direction("dir")],
            FormAction::with_slots("go", vec![Symbol::new("dir")]),
        );

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        // Full direction
        let result = form.try_match(&world, actor, &["north"], &resolver, &evaluator);
        if let MatchResult::Matched(slots) = result {
            assert_eq!(
                slots.get(&Symbol::new("dir")),
                Some(&SlotValue::Direction(Symbol::new("north")))
            );
        } else {
            panic!("Expected match");
        }

        // Abbreviation gets normalized
        let result = form.try_match(&world, actor, &["n"], &resolver, &evaluator);
        if let MatchResult::Matched(slots) = result {
            assert_eq!(
                slots.get(&Symbol::new("dir")),
                Some(&SlotValue::Direction(Symbol::new("north")))
            );
        } else {
            panic!("Expected match");
        }

        // Non-direction fails
        let result = form.try_match(&world, actor, &["lamp"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::NoMatch));
    }

    #[test]
    fn test_noun_slot_resolution() {
        let form = Form::new(
            vec![FormElement::noun("obj")],
            FormAction::with_slots("take", vec![Symbol::new("obj")]),
        );

        let (world, _, actor, lamp) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &["lamp"], &resolver, &evaluator);
        if let MatchResult::Matched(slots) = result {
            assert_eq!(
                slots.get(&Symbol::new("obj")),
                Some(&SlotValue::Entity(lamp))
            );
        } else {
            panic!("Expected match, got {result:?}");
        }
    }

    #[test]
    fn test_word_and_noun() {
        let form = Form::new(
            vec![FormElement::word("at"), FormElement::noun("obj")],
            FormAction::with_slots("examine", vec![Symbol::new("obj")]),
        );

        let (world, _, actor, lamp) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &["at", "lamp"], &resolver, &evaluator);
        if let MatchResult::Matched(slots) = result {
            assert_eq!(
                slots.get(&Symbol::new("obj")),
                Some(&SlotValue::Entity(lamp))
            );
        } else {
            panic!("Expected match");
        }
    }

    #[test]
    fn test_command_form_matching() {
        let mut cmd = Command::new("look");
        cmd.add_form(Form::new(vec![], FormAction::simple("look-around")));
        cmd.add_form(Form::new(
            vec![FormElement::word("at"), FormElement::noun("obj")],
            FormAction::with_slots("examine", vec![Symbol::new("obj")]),
        ));

        let (world, _, actor, lamp) = setup_world();
        let types = TypeRegistry::new();

        // Empty matches first form (after priority sort, but empty has lowest priority)
        // Actually, "at" + noun has higher priority, so empty form is tried last
        let result = cmd.match_forms(&world, actor, &[], &types);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("look-around"));

        // "at lamp" matches second form
        let result = cmd.match_forms(&world, actor, &["at", "lamp"], &types);
        assert!(result.is_some());
        let m = result.unwrap();
        assert_eq!(m.action.action_name(), Symbol::new("examine"));
        assert_eq!(m.get_entity("obj"), Some(lamp));
    }

    #[test]
    fn test_noun_not_found() {
        let form = Form::new(
            vec![FormElement::noun("obj")],
            FormAction::with_slots("take", vec![Symbol::new("obj")]),
        );

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        let result = form.try_match(&world, actor, &["nonexistent"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::ResolutionFailed { .. }));
    }

    #[test]
    fn test_tokens_must_be_fully_consumed() {
        let form = Form::new(vec![FormElement::word("at")], FormAction::simple("look-at"));

        let (world, _, actor, _) = setup_world();
        let resolver = Resolver::new();
        let types = TypeRegistry::new();
        let evaluator = PredicateEvaluator::new(&types);

        // "at lamp" should fail because "lamp" is not consumed
        let result = form.try_match(&world, actor, &["at", "lamp"], &resolver, &evaluator);
        assert!(matches!(result, MatchResult::NoMatch));
    }
}
