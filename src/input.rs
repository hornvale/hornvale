//! Input pipeline for interactive text processing.
//!
//! This module handles the conversion of raw player text into structured
//! commands that can be processed by the world systems.
//!
//! ## Pipeline
//!
//! 1. **Input**: Raw text from player, injected into world as component
//! 2. **Tokens**: Input split into words
//! 3. **Command**: Parsed verb + targets + arguments
//! 4. **ResolvedCommand**: Command with entity references resolved
//!
//! ## Usage
//!
//! ```
//! use hornvale::input::{Input, Command, parse_input};
//!
//! let input = Input::new("go north", 0);
//! let tokens = input.tokenize();
//! let command = parse_input(&input);
//! ```

mod resolve;

pub use resolve::{
    EntityCandidate, ResolutionResult, ResolvedCommand, ResolvedObject, Resolver, ScopeProvider,
};

use crate::core::{ComponentTypeId, EntityId, Value};
use crate::symbol::Symbol;
use std::sync::Arc;

/// Well-known component types for input processing.
pub mod components {
    use super::*;

    /// Raw input text component.
    pub fn input() -> ComponentTypeId {
        ComponentTypeId::new("Input")
    }

    /// Tokenized input component.
    pub fn tokens() -> ComponentTypeId {
        ComponentTypeId::new("Tokens")
    }

    /// Parsed command component.
    pub fn command() -> ComponentTypeId {
        ComponentTypeId::new("Command")
    }

    /// Input source entity (e.g., player).
    pub fn input_source() -> ComponentTypeId {
        ComponentTypeId::new("InputSource")
    }

    /// Input tick (when input was received).
    pub fn input_tick() -> ComponentTypeId {
        ComponentTypeId::new("InputTick")
    }
}

/// Raw input from a player or other source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Input {
    /// The raw text.
    pub text: Arc<str>,
    /// The tick when this input was received.
    pub tick: u64,
    /// The source entity (e.g., player).
    pub source: Option<EntityId>,
}

impl Input {
    /// Create a new input.
    pub fn new(text: impl Into<Arc<str>>, tick: u64) -> Self {
        Self {
            text: text.into(),
            tick,
            source: None,
        }
    }

    /// Create input with a source entity.
    pub fn with_source(mut self, source: EntityId) -> Self {
        self.source = Some(source);
        self
    }

    /// Tokenize the input into words.
    ///
    /// This performs basic word splitting:
    /// - Splits on whitespace
    /// - Lowercases all tokens
    /// - Removes empty tokens
    pub fn tokenize(&self) -> Vec<Token> {
        self.text
            .split_whitespace()
            .map(|word| Token {
                text: word.to_lowercase().into(),
                original: word.into(),
            })
            .collect()
    }
}

/// A single token from input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    /// Normalized (lowercase) text.
    pub text: Arc<str>,
    /// Original text before normalization.
    pub original: Arc<str>,
}

impl Token {
    /// Create a new token.
    pub fn new(text: impl Into<Arc<str>>) -> Self {
        let text: Arc<str> = text.into();
        Self {
            text: text.clone(),
            original: text,
        }
    }

    /// Check if this token matches a word (case-insensitive).
    pub fn matches(&self, word: &str) -> bool {
        self.text.as_ref() == word.to_lowercase()
    }
}

/// A parsed command.
#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    /// The verb (action) symbol.
    pub verb: Symbol,
    /// Direct object reference (if any).
    pub direct_object: Option<ObjectRef>,
    /// Indirect object reference (if any).
    pub indirect_object: Option<ObjectRef>,
    /// Additional arguments (preposition-value pairs).
    pub arguments: Vec<(Symbol, ObjectRef)>,
}

impl Command {
    /// Create a new command with just a verb.
    pub fn verb(verb: impl Into<Symbol>) -> Self {
        Self {
            verb: verb.into(),
            direct_object: None,
            indirect_object: None,
            arguments: Vec::new(),
        }
    }

    /// Add a direct object.
    pub fn with_direct(mut self, obj: ObjectRef) -> Self {
        self.direct_object = Some(obj);
        self
    }

    /// Add an indirect object.
    pub fn with_indirect(mut self, obj: ObjectRef) -> Self {
        self.indirect_object = Some(obj);
        self
    }

    /// Add an argument.
    pub fn with_arg(mut self, prep: impl Into<Symbol>, obj: ObjectRef) -> Self {
        self.arguments.push((prep.into(), obj));
        self
    }

    /// Convert to a Value for storage in components.
    pub fn to_value(&self) -> Value {
        // Store as a map-like structure
        let mut items = vec![Value::Symbol(Symbol::new("verb")), Value::Symbol(self.verb)];

        if let Some(ref obj) = self.direct_object {
            items.push(Value::Symbol(Symbol::new("direct")));
            items.push(obj.to_value());
        }

        if let Some(ref obj) = self.indirect_object {
            items.push(Value::Symbol(Symbol::new("indirect")));
            items.push(obj.to_value());
        }

        for (prep, obj) in &self.arguments {
            items.push(Value::Symbol(*prep));
            items.push(obj.to_value());
        }

        Value::list(items)
    }
}

/// Reference to an object in a command.
///
/// Before resolution, this contains the noun phrase text.
/// After resolution, it contains an entity reference.
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectRef {
    /// Unresolved noun phrase (e.g., "brass lamp").
    Unresolved(Arc<str>),
    /// Resolved entity reference.
    Entity(EntityId),
    /// Direction (north, south, etc.).
    Direction(Symbol),
    /// Special pronoun (it, them, etc.).
    Pronoun(Symbol),
}

impl ObjectRef {
    /// Create an unresolved reference.
    pub fn unresolved(text: impl Into<Arc<str>>) -> Self {
        ObjectRef::Unresolved(text.into())
    }

    /// Create a direction reference.
    pub fn direction(dir: impl Into<Symbol>) -> Self {
        ObjectRef::Direction(dir.into())
    }

    /// Create a pronoun reference.
    pub fn pronoun(pronoun: impl Into<Symbol>) -> Self {
        ObjectRef::Pronoun(pronoun.into())
    }

    /// Convert to a Value.
    pub fn to_value(&self) -> Value {
        match self {
            ObjectRef::Unresolved(text) => Value::string(text.as_ref()),
            ObjectRef::Entity(id) => Value::EntityRef(*id),
            ObjectRef::Direction(sym) => Value::Symbol(*sym),
            ObjectRef::Pronoun(sym) => Value::Symbol(*sym),
        }
    }
}

/// Known directions.
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

/// Known pronouns.
const PRONOUNS: &[&str] = &[
    "it",
    "them",
    "him",
    "her",
    "that",
    "this",
    "all",
    "everything",
];

/// Known prepositions.
const PREPOSITIONS: &[&str] = &[
    "in", "into", "on", "onto", "with", "at", "to", "from", "under", "behind", "through",
];

/// Parse raw input into a command.
///
/// This is a simple verb-noun parser that handles:
/// - Single verb: "look", "inventory"
/// - Verb + direction: "go north"
/// - Verb + object: "take lamp"
/// - Verb + object + preposition + object: "put lamp in chest"
pub fn parse_input(input: &Input) -> Option<Command> {
    let tokens = input.tokenize();
    if tokens.is_empty() {
        return None;
    }

    parse_tokens(&tokens)
}

/// Parse a list of tokens into a command.
pub fn parse_tokens(tokens: &[Token]) -> Option<Command> {
    if tokens.is_empty() {
        return None;
    }

    let verb = Symbol::new(tokens[0].text.as_ref());
    let mut cmd = Command::verb(verb);

    if tokens.len() == 1 {
        return Some(cmd);
    }

    // Find preposition index if any
    let prep_idx = tokens[1..]
        .iter()
        .position(|t| PREPOSITIONS.contains(&t.text.as_ref()))
        .map(|i| i + 1);

    // Parse direct object (between verb and preposition, or to end)
    let direct_end = prep_idx.unwrap_or(tokens.len());
    if direct_end > 1 {
        let direct_tokens: Vec<_> = tokens[1..direct_end].iter().map(|t| &t.text).collect();
        let direct_text = direct_tokens
            .iter()
            .map(|s| s.as_ref())
            .collect::<Vec<_>>()
            .join(" ");

        let obj = classify_object(&direct_text);
        cmd = cmd.with_direct(obj);
    }

    // Parse preposition + indirect object
    if let Some(prep_i) = prep_idx {
        if prep_i < tokens.len() {
            let prep = Symbol::new(tokens[prep_i].text.as_ref());

            if prep_i + 1 < tokens.len() {
                let indirect_tokens: Vec<_> =
                    tokens[prep_i + 1..].iter().map(|t| &t.text).collect();
                let indirect_text = indirect_tokens
                    .iter()
                    .map(|s| s.as_ref())
                    .collect::<Vec<_>>()
                    .join(" ");

                let obj = classify_object(&indirect_text);
                cmd = cmd.with_arg(prep, obj);
            }
        }
    }

    Some(cmd)
}

/// Classify an object reference from text.
fn classify_object(text: &str) -> ObjectRef {
    let normalized = text.to_lowercase();

    // Check if it's a direction
    if DIRECTIONS.contains(&normalized.as_str()) {
        return ObjectRef::direction(expand_direction(&normalized));
    }

    // Check if it's a pronoun
    if PRONOUNS.contains(&normalized.as_str()) {
        return ObjectRef::pronoun(normalized.as_str());
    }

    // Otherwise it's an unresolved noun phrase
    ObjectRef::unresolved(text)
}

/// Expand direction abbreviations.
fn expand_direction(dir: &str) -> &str {
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
        _ => dir,
    }
}

/// Canonical direction symbols.
pub mod directions {
    use super::*;

    pub fn north() -> Symbol {
        Symbol::new("north")
    }
    pub fn south() -> Symbol {
        Symbol::new("south")
    }
    pub fn east() -> Symbol {
        Symbol::new("east")
    }
    pub fn west() -> Symbol {
        Symbol::new("west")
    }
    pub fn up() -> Symbol {
        Symbol::new("up")
    }
    pub fn down() -> Symbol {
        Symbol::new("down")
    }
    pub fn northeast() -> Symbol {
        Symbol::new("northeast")
    }
    pub fn northwest() -> Symbol {
        Symbol::new("northwest")
    }
    pub fn southeast() -> Symbol {
        Symbol::new("southeast")
    }
    pub fn southwest() -> Symbol {
        Symbol::new("southwest")
    }
    pub fn r#in() -> Symbol {
        Symbol::new("in")
    }
    pub fn out() -> Symbol {
        Symbol::new("out")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_tokenize() {
        let input = Input::new("GO North", 0);
        let tokens = input.tokenize();

        assert_eq!(tokens.len(), 2);
        assert_eq!(tokens[0].text.as_ref(), "go");
        assert_eq!(tokens[0].original.as_ref(), "GO");
        assert_eq!(tokens[1].text.as_ref(), "north");
    }

    #[test]
    fn test_input_tokenize_multiple_spaces() {
        let input = Input::new("  take   the   lamp  ", 0);
        let tokens = input.tokenize();

        assert_eq!(tokens.len(), 3);
        assert_eq!(tokens[0].text.as_ref(), "take");
        assert_eq!(tokens[1].text.as_ref(), "the");
        assert_eq!(tokens[2].text.as_ref(), "lamp");
    }

    #[test]
    fn test_token_matches() {
        let token = Token::new("north");
        assert!(token.matches("north"));
        assert!(token.matches("NORTH"));
        assert!(token.matches("North"));
        assert!(!token.matches("south"));
    }

    #[test]
    fn test_parse_single_verb() {
        let input = Input::new("look", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "look");
        assert!(cmd.direct_object.is_none());
    }

    #[test]
    fn test_parse_verb_direction() {
        let input = Input::new("go north", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "go");
        assert_eq!(cmd.direct_object, Some(ObjectRef::direction("north")));
    }

    #[test]
    fn test_parse_verb_direction_abbrev() {
        let input = Input::new("go n", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "go");
        assert_eq!(cmd.direct_object, Some(ObjectRef::direction("north")));
    }

    #[test]
    fn test_parse_verb_object() {
        let input = Input::new("take lamp", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "take");
        assert_eq!(cmd.direct_object, Some(ObjectRef::unresolved("lamp")));
    }

    #[test]
    fn test_parse_verb_adjective_object() {
        let input = Input::new("take brass lamp", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "take");
        assert_eq!(cmd.direct_object, Some(ObjectRef::unresolved("brass lamp")));
    }

    #[test]
    fn test_parse_verb_object_prep_object() {
        let input = Input::new("put lamp in chest", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "put");
        assert_eq!(cmd.direct_object, Some(ObjectRef::unresolved("lamp")));
        assert_eq!(cmd.arguments.len(), 1);
        assert_eq!(cmd.arguments[0].0.as_str(), "in");
        assert_eq!(cmd.arguments[0].1, ObjectRef::unresolved("chest"));
    }

    #[test]
    fn test_parse_pronoun() {
        let input = Input::new("take it", 0);
        let cmd = parse_input(&input).unwrap();

        assert_eq!(cmd.verb.as_str(), "take");
        assert_eq!(cmd.direct_object, Some(ObjectRef::pronoun("it")));
    }

    #[test]
    fn test_parse_empty() {
        let input = Input::new("", 0);
        assert!(parse_input(&input).is_none());
    }

    #[test]
    fn test_command_to_value() {
        let cmd = Command::verb("go").with_direct(ObjectRef::direction("north"));

        let value = cmd.to_value();
        let list = value.as_list().unwrap();

        assert!(list.len() >= 4); // verb, go, direct, north
    }

    #[test]
    fn test_input_with_source() {
        let source = EntityId::from_raw(42);
        let input = Input::new("look", 5).with_source(source);

        assert_eq!(input.source, Some(source));
        assert_eq!(input.tick, 5);
    }

    #[test]
    fn test_object_ref_to_value() {
        let unresolved = ObjectRef::unresolved("lamp");
        assert_eq!(unresolved.to_value(), Value::string("lamp"));

        let direction = ObjectRef::direction("north");
        assert_eq!(direction.to_value(), Value::Symbol(Symbol::new("north")));

        let entity = ObjectRef::Entity(EntityId::from_raw(1));
        assert_eq!(entity.to_value(), Value::EntityRef(EntityId::from_raw(1)));
    }
}
