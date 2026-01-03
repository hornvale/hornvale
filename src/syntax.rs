//! Syntax tables for command parsing.
//!
//! This module provides a data-driven approach to mapping player input to
//! semantic actions. Instead of hardcoding verb parsing, syntax patterns
//! are defined as data that can be loaded from DSL files.
//!
//! ## Architecture
//!
//! - `Action`: A semantic operation (examine, take, go, etc.)
//! - `SyntaxPattern`: A sequence of tokens/slots that can match input
//! - `SyntaxEntry`: Maps a pattern to an action
//! - `SyntaxTable`: Registry of all syntax entries, handles matching
//!
//! ## Example
//!
//! ```
//! use hornvale::syntax::{Action, SyntaxPattern, SyntaxElement, SyntaxTable};
//!
//! let mut table = SyntaxTable::new();
//!
//! // "look" → look-around
//! table.add(
//!     SyntaxPattern::new(vec![SyntaxElement::word("look")]),
//!     Action::new("look-around"),
//! );
//!
//! // "look at <noun>" → examine
//! table.add(
//!     SyntaxPattern::new(vec![
//!         SyntaxElement::word("look"),
//!         SyntaxElement::word("at"),
//!         SyntaxElement::noun(),
//!     ]),
//!     Action::new("examine"),
//! );
//!
//! // "x <noun>" → examine (abbreviation)
//! table.add(
//!     SyntaxPattern::new(vec![
//!         SyntaxElement::word("x"),
//!         SyntaxElement::noun(),
//!     ]),
//!     Action::new("examine"),
//! );
//!
//! // Match against input
//! let matches = table.match_tokens(&["look", "at", "lamp"]);
//! assert_eq!(matches.len(), 1);
//! assert_eq!(matches[0].action.name(), "examine");
//! assert_eq!(matches[0].slots.get("noun"), Some(&"lamp".to_string()));
//! ```

use std::collections::HashMap;
use std::sync::Arc;

use crate::symbol::Symbol;

/// A semantic action that can be performed.
///
/// Actions are the "verbs" of the game logic, independent of their
/// surface syntax. Multiple syntax patterns can map to the same action.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Action {
    /// The action's identifier (e.g., "examine", "take", "go")
    name: Symbol,
}

impl Action {
    /// Create a new action with the given name.
    pub fn new(name: impl Into<Symbol>) -> Self {
        Self { name: name.into() }
    }

    /// Get the action's name as a string.
    pub fn name(&self) -> String {
        self.name.as_str()
    }

    /// Get the action's name as a symbol.
    pub fn name_symbol(&self) -> Symbol {
        self.name
    }
}

/// An element in a syntax pattern.
///
/// Patterns are sequences of elements that can be literal words,
/// slots for nouns, or slots for directions.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxElement {
    /// A literal word that must match exactly (case-insensitive).
    Word(String),

    /// A slot for a noun phrase. Captures one or more tokens.
    /// The string is the slot name (default: "noun").
    Noun(String),

    /// A slot for a direction. Captures one token.
    /// The string is the slot name (default: "direction").
    Direction(String),

    /// A slot that captures any single token.
    /// The string is the slot name.
    Any(String),

    /// An optional sequence of elements.
    /// If present, all must match; if absent, matching continues.
    Optional(Vec<SyntaxElement>),
}

impl SyntaxElement {
    /// Create a literal word element.
    pub fn word(w: impl Into<String>) -> Self {
        Self::Word(w.into().to_lowercase())
    }

    /// Create a noun slot with default name "noun".
    pub fn noun() -> Self {
        Self::Noun("noun".to_string())
    }

    /// Create a noun slot with a custom name.
    pub fn noun_named(name: impl Into<String>) -> Self {
        Self::Noun(name.into())
    }

    /// Create a direction slot with default name "direction".
    pub fn direction() -> Self {
        Self::Direction("direction".to_string())
    }

    /// Create a direction slot with a custom name.
    pub fn direction_named(name: impl Into<String>) -> Self {
        Self::Direction(name.into())
    }

    /// Create an any-token slot.
    pub fn any(name: impl Into<String>) -> Self {
        Self::Any(name.into())
    }

    /// Create an optional sequence.
    pub fn optional(elements: Vec<SyntaxElement>) -> Self {
        Self::Optional(elements)
    }

    /// Check if this element is a slot (captures input).
    pub fn is_slot(&self) -> bool {
        matches!(
            self,
            Self::Noun(_) | Self::Direction(_) | Self::Any(_) | Self::Optional(_)
        )
    }

    /// Get the slot name, if this is a slot.
    pub fn slot_name(&self) -> Option<&str> {
        match self {
            Self::Noun(name) | Self::Direction(name) | Self::Any(name) => Some(name),
            Self::Optional(_) | Self::Word(_) => None,
        }
    }
}

/// A syntax pattern that can match player input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxPattern {
    /// The sequence of elements in this pattern.
    elements: Vec<SyntaxElement>,
}

impl SyntaxPattern {
    /// Create a new pattern from a sequence of elements.
    pub fn new(elements: Vec<SyntaxElement>) -> Self {
        Self { elements }
    }

    /// Get the elements in this pattern.
    pub fn elements(&self) -> &[SyntaxElement] {
        &self.elements
    }

    /// Get the number of required tokens (minimum match length).
    pub fn min_tokens(&self) -> usize {
        self.elements
            .iter()
            .filter(|e| !matches!(e, SyntaxElement::Optional(_)))
            .count()
    }

    /// Get the maximum number of tokens this pattern could match.
    pub fn max_tokens(&self) -> usize {
        self.elements.len()
    }

    /// Attempt to match this pattern against input tokens.
    ///
    /// Returns `Some(slots)` if the pattern matches, where slots is a map
    /// of slot names to captured values. Returns `None` if no match.
    pub fn match_tokens(&self, tokens: &[&str]) -> Option<HashMap<String, String>> {
        let mut slots = HashMap::new();
        let mut token_idx = 0;
        let mut elem_idx = 0;

        while elem_idx < self.elements.len() {
            let element = &self.elements[elem_idx];

            match element {
                SyntaxElement::Word(expected) => {
                    if token_idx >= tokens.len() {
                        return None;
                    }
                    if tokens[token_idx].to_lowercase() != *expected {
                        return None;
                    }
                    token_idx += 1;
                }

                SyntaxElement::Noun(name) => {
                    if token_idx >= tokens.len() {
                        return None;
                    }
                    // For now, nouns consume a single token.
                    // Multi-word noun phrases would need lookahead.
                    // We can enhance this later with adjective handling.
                    let mut noun_tokens = vec![tokens[token_idx].to_string()];
                    token_idx += 1;

                    // Consume additional tokens until we hit a keyword or end
                    while token_idx < tokens.len() {
                        // Check if the next token would match the next required element
                        if elem_idx + 1 < self.elements.len() {
                            if let SyntaxElement::Word(next_word) = &self.elements[elem_idx + 1] {
                                if tokens[token_idx].to_lowercase() == *next_word {
                                    break;
                                }
                            }
                        }
                        // Check if it looks like a preposition
                        let lower = tokens[token_idx].to_lowercase();
                        if is_preposition(&lower) {
                            break;
                        }
                        noun_tokens.push(tokens[token_idx].to_string());
                        token_idx += 1;
                    }

                    slots.insert(name.clone(), noun_tokens.join(" "));
                }

                SyntaxElement::Direction(name) => {
                    if token_idx >= tokens.len() {
                        return None;
                    }
                    // Directions are single tokens
                    slots.insert(name.clone(), tokens[token_idx].to_string());
                    token_idx += 1;
                }

                SyntaxElement::Any(name) => {
                    if token_idx >= tokens.len() {
                        return None;
                    }
                    slots.insert(name.clone(), tokens[token_idx].to_string());
                    token_idx += 1;
                }

                SyntaxElement::Optional(opt_elements) => {
                    // Try to match the optional sequence
                    let remaining = &tokens[token_idx..];
                    if let Some((opt_slots, consumed)) = match_optional_sequence(
                        opt_elements,
                        remaining,
                        &self.elements[elem_idx + 1..],
                    ) {
                        for (k, v) in opt_slots {
                            slots.insert(k, v);
                        }
                        token_idx += consumed;
                    }
                    // If optional doesn't match, we just continue
                }
            }

            elem_idx += 1;
        }

        // All tokens must be consumed
        if token_idx == tokens.len() {
            Some(slots)
        } else {
            None
        }
    }
}

/// Check if a word is a preposition (used for noun phrase boundaries).
fn is_preposition(word: &str) -> bool {
    matches!(
        word,
        "in" | "into"
            | "on"
            | "onto"
            | "at"
            | "to"
            | "from"
            | "with"
            | "under"
            | "over"
            | "through"
            | "behind"
            | "inside"
            | "outside"
    )
}

/// Try to match an optional sequence, returning slots and tokens consumed.
fn match_optional_sequence(
    elements: &[SyntaxElement],
    tokens: &[&str],
    _following: &[SyntaxElement],
) -> Option<(HashMap<String, String>, usize)> {
    let mut slots = HashMap::new();
    let mut token_idx = 0;

    for element in elements {
        match element {
            SyntaxElement::Word(expected) => {
                if token_idx >= tokens.len() {
                    return None;
                }
                if tokens[token_idx].to_lowercase() != *expected {
                    return None;
                }
                token_idx += 1;
            }
            SyntaxElement::Noun(name)
            | SyntaxElement::Direction(name)
            | SyntaxElement::Any(name) => {
                if token_idx >= tokens.len() {
                    return None;
                }
                slots.insert(name.clone(), tokens[token_idx].to_string());
                token_idx += 1;
            }
            SyntaxElement::Optional(_) => {
                // Nested optionals not supported for now
            }
        }
    }

    Some((slots, token_idx))
}

/// The result of a successful syntax match.
#[derive(Debug, Clone)]
pub struct SyntaxMatch {
    /// The action this syntax maps to.
    pub action: Action,
    /// Captured slot values (noun phrases, directions, etc.).
    pub slots: HashMap<String, String>,
    /// The pattern that matched (for debugging/disambiguation).
    pub pattern: Arc<SyntaxPattern>,
    /// Priority of this match (higher = more specific).
    pub priority: i32,
}

/// A syntax table mapping patterns to actions.
#[derive(Debug, Clone, Default)]
pub struct SyntaxTable {
    /// All registered syntax entries, sorted by priority.
    entries: Vec<SyntaxEntry>,
}

/// An entry in the syntax table.
#[derive(Debug, Clone)]
struct SyntaxEntry {
    pattern: Arc<SyntaxPattern>,
    action: Action,
    priority: i32,
}

impl SyntaxTable {
    /// Create a new empty syntax table.
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
        }
    }

    /// Add a syntax pattern that maps to an action.
    ///
    /// Priority is computed automatically based on pattern specificity:
    /// - More elements = higher priority
    /// - More literal words = higher priority
    pub fn add(&mut self, pattern: SyntaxPattern, action: Action) {
        let priority = compute_priority(&pattern);
        self.entries.push(SyntaxEntry {
            pattern: Arc::new(pattern),
            action,
            priority,
        });
        // Keep sorted by priority (highest first)
        self.entries.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Add a syntax pattern with explicit priority.
    pub fn add_with_priority(&mut self, pattern: SyntaxPattern, action: Action, priority: i32) {
        self.entries.push(SyntaxEntry {
            pattern: Arc::new(pattern),
            action,
            priority,
        });
        self.entries.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Match input tokens against all patterns.
    ///
    /// Returns all matching patterns, sorted by priority (best first).
    pub fn match_tokens(&self, tokens: &[&str]) -> Vec<SyntaxMatch> {
        let mut matches = Vec::new();

        for entry in &self.entries {
            if let Some(slots) = entry.pattern.match_tokens(tokens) {
                matches.push(SyntaxMatch {
                    action: entry.action.clone(),
                    slots,
                    pattern: entry.pattern.clone(),
                    priority: entry.priority,
                });
            }
        }

        matches
    }

    /// Get the best (highest priority) match, if any.
    pub fn best_match(&self, tokens: &[&str]) -> Option<SyntaxMatch> {
        self.match_tokens(tokens).into_iter().next()
    }

    /// Get all registered actions.
    pub fn actions(&self) -> Vec<&Action> {
        let mut seen = std::collections::HashSet::new();
        self.entries
            .iter()
            .filter_map(|e| {
                let name = e.action.name();
                if seen.insert(name) {
                    Some(&e.action)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Get all patterns for a given action.
    pub fn patterns_for(&self, action_name: &str) -> Vec<&SyntaxPattern> {
        self.entries
            .iter()
            .filter(|e| e.action.name() == action_name)
            .map(|e| e.pattern.as_ref())
            .collect()
    }

    /// Get the number of entries in the table.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Check if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Compute priority for a pattern based on specificity.
fn compute_priority(pattern: &SyntaxPattern) -> i32 {
    let mut priority = 0;

    for element in &pattern.elements {
        match element {
            // Literal words are most specific
            SyntaxElement::Word(_) => priority += 10,
            // Named slots are moderately specific
            SyntaxElement::Noun(_) => priority += 5,
            SyntaxElement::Direction(_) => priority += 5,
            // Any slots are least specific
            SyntaxElement::Any(_) => priority += 1,
            // Optional elements add a small bonus
            SyntaxElement::Optional(inner) => {
                priority += 2;
                for e in inner {
                    if matches!(e, SyntaxElement::Word(_)) {
                        priority += 3;
                    }
                }
            }
        }
    }

    priority
}

/// Builder for creating syntax patterns from a DSL-like string.
///
/// Format: `"word" "word" noun direction [optional "word" noun]`
pub struct PatternBuilder {
    elements: Vec<SyntaxElement>,
}

impl PatternBuilder {
    /// Create a new pattern builder.
    pub fn new() -> Self {
        Self {
            elements: Vec::new(),
        }
    }

    /// Add a literal word.
    pub fn word(mut self, w: impl Into<String>) -> Self {
        self.elements.push(SyntaxElement::word(w));
        self
    }

    /// Add a noun slot with default name.
    pub fn noun(mut self) -> Self {
        self.elements.push(SyntaxElement::noun());
        self
    }

    /// Add a noun slot with custom name.
    pub fn noun_named(mut self, name: impl Into<String>) -> Self {
        self.elements.push(SyntaxElement::noun_named(name));
        self
    }

    /// Add a direction slot with default name.
    pub fn direction(mut self) -> Self {
        self.elements.push(SyntaxElement::direction());
        self
    }

    /// Add a direction slot with custom name.
    pub fn direction_named(mut self, name: impl Into<String>) -> Self {
        self.elements.push(SyntaxElement::direction_named(name));
        self
    }

    /// Add an any-token slot.
    pub fn any(mut self, name: impl Into<String>) -> Self {
        self.elements.push(SyntaxElement::any(name));
        self
    }

    /// Add an optional sequence.
    pub fn optional(mut self, builder: PatternBuilder) -> Self {
        self.elements
            .push(SyntaxElement::Optional(builder.elements));
        self
    }

    /// Build the pattern.
    pub fn build(self) -> SyntaxPattern {
        SyntaxPattern::new(self.elements)
    }
}

impl Default for PatternBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_action_creation() {
        let action = Action::new("examine");
        assert_eq!(action.name(), "examine");
    }

    #[test]
    fn test_pattern_single_word() {
        let pattern = SyntaxPattern::new(vec![SyntaxElement::word("look")]);

        assert!(pattern.match_tokens(&["look"]).is_some());
        assert!(pattern.match_tokens(&["Look"]).is_some()); // case insensitive
        assert!(pattern.match_tokens(&["LOOK"]).is_some());
        assert!(pattern.match_tokens(&["examine"]).is_none());
        assert!(pattern.match_tokens(&["look", "around"]).is_none()); // extra tokens
    }

    #[test]
    fn test_pattern_multiple_words() {
        let pattern =
            SyntaxPattern::new(vec![SyntaxElement::word("pick"), SyntaxElement::word("up")]);

        assert!(pattern.match_tokens(&["pick", "up"]).is_some());
        assert!(pattern.match_tokens(&["pick"]).is_none());
        assert!(pattern.match_tokens(&["pick", "up", "lamp"]).is_none());
    }

    #[test]
    fn test_pattern_with_noun() {
        let pattern = SyntaxPattern::new(vec![SyntaxElement::word("take"), SyntaxElement::noun()]);

        let result = pattern.match_tokens(&["take", "lamp"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("noun"), Some(&"lamp".to_string()));

        // Multi-word noun
        let result = pattern.match_tokens(&["take", "brass", "lamp"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("noun"), Some(&"brass lamp".to_string()));
    }

    #[test]
    fn test_pattern_noun_with_preposition() {
        let pattern = SyntaxPattern::new(vec![
            SyntaxElement::word("put"),
            SyntaxElement::noun_named("object"),
            SyntaxElement::word("in"),
            SyntaxElement::noun_named("container"),
        ]);

        let result = pattern.match_tokens(&["put", "lamp", "in", "box"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("object"), Some(&"lamp".to_string()));
        assert_eq!(slots.get("container"), Some(&"box".to_string()));

        // Multi-word nouns
        let result = pattern.match_tokens(&["put", "brass", "lamp", "in", "wooden", "box"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("object"), Some(&"brass lamp".to_string()));
        assert_eq!(slots.get("container"), Some(&"wooden box".to_string()));
    }

    #[test]
    fn test_pattern_with_direction() {
        let pattern =
            SyntaxPattern::new(vec![SyntaxElement::word("go"), SyntaxElement::direction()]);

        let result = pattern.match_tokens(&["go", "north"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("direction"), Some(&"north".to_string()));
    }

    #[test]
    fn test_syntax_table_basic() {
        let mut table = SyntaxTable::new();

        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("look")]),
            Action::new("look-around"),
        );
        table.add(
            SyntaxPattern::new(vec![
                SyntaxElement::word("look"),
                SyntaxElement::word("at"),
                SyntaxElement::noun(),
            ]),
            Action::new("examine"),
        );

        // "look" matches look-around
        let matches = table.match_tokens(&["look"]);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].action.name(), "look-around");

        // "look at lamp" matches examine
        let matches = table.match_tokens(&["look", "at", "lamp"]);
        assert_eq!(matches.len(), 1);
        assert_eq!(matches[0].action.name(), "examine");
        assert_eq!(matches[0].slots.get("noun"), Some(&"lamp".to_string()));
    }

    #[test]
    fn test_syntax_table_priority() {
        let mut table = SyntaxTable::new();

        // Less specific: "look" <any>
        table.add(
            SyntaxPattern::new(vec![
                SyntaxElement::word("look"),
                SyntaxElement::any("thing"),
            ]),
            Action::new("look-at"),
        );

        // More specific: "look" "at" <noun>
        table.add(
            SyntaxPattern::new(vec![
                SyntaxElement::word("look"),
                SyntaxElement::word("at"),
                SyntaxElement::noun(),
            ]),
            Action::new("examine"),
        );

        // "look at lamp" should match examine (more specific)
        let best = table.best_match(&["look", "at", "lamp"]);
        assert!(best.is_some());
        assert_eq!(best.unwrap().action.name(), "examine");
    }

    #[test]
    fn test_syntax_table_multiple_actions() {
        let mut table = SyntaxTable::new();

        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("x"), SyntaxElement::noun()]),
            Action::new("examine"),
        );
        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("examine"), SyntaxElement::noun()]),
            Action::new("examine"),
        );
        table.add(
            SyntaxPattern::new(vec![
                SyntaxElement::word("look"),
                SyntaxElement::word("at"),
                SyntaxElement::noun(),
            ]),
            Action::new("examine"),
        );

        // All three should route to examine
        assert_eq!(
            table.best_match(&["x", "lamp"]).unwrap().action.name(),
            "examine"
        );
        assert_eq!(
            table
                .best_match(&["examine", "lamp"])
                .unwrap()
                .action
                .name(),
            "examine"
        );
        assert_eq!(
            table
                .best_match(&["look", "at", "lamp"])
                .unwrap()
                .action
                .name(),
            "examine"
        );
    }

    #[test]
    fn test_pattern_builder() {
        let pattern = PatternBuilder::new()
            .word("put")
            .noun_named("object")
            .word("in")
            .noun_named("container")
            .build();

        let result = pattern.match_tokens(&["put", "key", "in", "box"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("object"), Some(&"key".to_string()));
        assert_eq!(slots.get("container"), Some(&"box".to_string()));
    }

    #[test]
    fn test_syntax_table_no_match() {
        let mut table = SyntaxTable::new();
        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("look")]),
            Action::new("look-around"),
        );

        assert!(table.best_match(&["jump"]).is_none());
        assert!(table.best_match(&[]).is_none());
    }

    #[test]
    fn test_syntax_table_actions_and_patterns() {
        let mut table = SyntaxTable::new();

        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("l")]),
            Action::new("look-around"),
        );
        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("look")]),
            Action::new("look-around"),
        );
        table.add(
            SyntaxPattern::new(vec![SyntaxElement::word("x"), SyntaxElement::noun()]),
            Action::new("examine"),
        );

        let actions = table.actions();
        assert_eq!(actions.len(), 2); // look-around and examine

        let look_patterns = table.patterns_for("look-around");
        assert_eq!(look_patterns.len(), 2); // "l" and "look"
    }

    #[test]
    fn test_optional_elements() {
        let pattern = SyntaxPattern::new(vec![
            SyntaxElement::word("attack"),
            SyntaxElement::noun_named("target"),
            SyntaxElement::Optional(vec![
                SyntaxElement::word("with"),
                SyntaxElement::noun_named("weapon"),
            ]),
        ]);

        // Without optional
        let result = pattern.match_tokens(&["attack", "goblin"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("target"), Some(&"goblin".to_string()));
        assert!(slots.get("weapon").is_none());

        // With optional
        let result = pattern.match_tokens(&["attack", "goblin", "with", "sword"]);
        assert!(result.is_some());
        let slots = result.unwrap();
        assert_eq!(slots.get("target"), Some(&"goblin".to_string()));
        assert_eq!(slots.get("weapon"), Some(&"sword".to_string()));
    }
}
