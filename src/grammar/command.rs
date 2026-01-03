//! Command definitions and grammar matching results.

use crate::core::EntityId;
use crate::symbol::Symbol;
use im::OrdMap;
use std::sync::Arc;

use super::types::SlotType;

/// An element in a form pattern.
///
/// Form patterns describe the structure of command arguments after the
/// initial command/alias has been matched by the intent trie.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormElement {
    /// A literal word that must match exactly (case-insensitive).
    Word(String),
    /// A typed slot that captures a value.
    Slot {
        /// The name used to reference this slot in the action.
        name: Symbol,
        /// The type constraint for this slot.
        slot_type: SlotType,
    },
}

impl FormElement {
    /// Create a literal word element.
    pub fn word(w: impl Into<String>) -> Self {
        Self::Word(w.into().to_lowercase())
    }

    /// Create a noun slot with the given name.
    pub fn noun(name: impl Into<Symbol>) -> Self {
        Self::Slot {
            name: name.into(),
            slot_type: SlotType::Noun,
        }
    }

    /// Create a direction slot with the given name.
    pub fn direction(name: impl Into<Symbol>) -> Self {
        Self::Slot {
            name: name.into(),
            slot_type: SlotType::Direction,
        }
    }

    /// Create a slot with a custom type.
    pub fn typed(name: impl Into<Symbol>, type_name: impl Into<Symbol>) -> Self {
        Self::Slot {
            name: name.into(),
            slot_type: SlotType::Custom(type_name.into()),
        }
    }

    /// Check if this element is a word.
    pub fn is_word(&self) -> bool {
        matches!(self, FormElement::Word(_))
    }

    /// Check if this element is a slot.
    pub fn is_slot(&self) -> bool {
        matches!(self, FormElement::Slot { .. })
    }

    /// Get the slot name if this is a slot.
    pub fn slot_name(&self) -> Option<Symbol> {
        match self {
            FormElement::Slot { name, .. } => Some(*name),
            _ => None,
        }
    }

    /// Get the slot type if this is a slot.
    pub fn slot_type(&self) -> Option<&SlotType> {
        match self {
            FormElement::Slot { slot_type, .. } => Some(slot_type),
            _ => None,
        }
    }
}

/// The action a form produces when matched.
///
/// Actions can be simple (just a name) or include slot references
/// that will be populated from the matched input.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormAction {
    /// Simple action name (e.g., `look-around`).
    Simple(Symbol),
    /// Action with slot references (e.g., `(examine obj)`).
    WithSlots {
        /// The action name.
        action: Symbol,
        /// Slot names to pass as arguments.
        slots: Vec<Symbol>,
    },
}

impl FormAction {
    /// Create a simple action.
    pub fn simple(name: impl Into<Symbol>) -> Self {
        Self::Simple(name.into())
    }

    /// Create an action with slot references.
    pub fn with_slots(action: impl Into<Symbol>, slots: Vec<Symbol>) -> Self {
        Self::WithSlots {
            action: action.into(),
            slots,
        }
    }

    /// Get the action name.
    pub fn action_name(&self) -> Symbol {
        match self {
            FormAction::Simple(name) => *name,
            FormAction::WithSlots { action, .. } => *action,
        }
    }

    /// Get the slot references, if any.
    pub fn slot_refs(&self) -> &[Symbol] {
        match self {
            FormAction::Simple(_) => &[],
            FormAction::WithSlots { slots, .. } => slots,
        }
    }
}

/// A form pattern with its action.
///
/// Forms define how arguments after the command name are parsed.
/// Multiple forms can exist for a single command, tried in priority order.
#[derive(Debug, Clone)]
pub struct Form {
    /// Pattern elements to match.
    pub pattern: Vec<FormElement>,
    /// Action to invoke on match.
    pub action: FormAction,
    /// Explicit priority (higher = tried first). Defaults to pattern length.
    pub priority: i32,
}

impl Form {
    /// Create a new form.
    pub fn new(pattern: Vec<FormElement>, action: FormAction) -> Self {
        let priority = Self::compute_priority(&pattern);
        Self {
            pattern,
            action,
            priority,
        }
    }

    /// Create a new form with explicit priority.
    pub fn with_priority(pattern: Vec<FormElement>, action: FormAction, priority: i32) -> Self {
        Self {
            pattern,
            action,
            priority,
        }
    }

    /// Compute default priority from pattern.
    /// Longer patterns and more literal words = higher priority.
    fn compute_priority(pattern: &[FormElement]) -> i32 {
        let mut priority = 0;
        for elem in pattern {
            match elem {
                FormElement::Word(_) => priority += 10,
                FormElement::Slot { slot_type, .. } => match slot_type {
                    SlotType::Direction => priority += 5,
                    SlotType::Noun | SlotType::Custom(_) => priority += 3,
                },
            }
        }
        priority
    }

    /// Check if this is an empty pattern (matches bare command).
    pub fn is_empty(&self) -> bool {
        self.pattern.is_empty()
    }

    /// Get the minimum number of tokens this pattern can match.
    pub fn min_tokens(&self) -> usize {
        self.pattern
            .iter()
            .filter(|e| !matches!(e, FormElement::Word(_)))
            .count()
            + self
                .pattern
                .iter()
                .filter(|e| matches!(e, FormElement::Word(_)))
                .count()
    }
}

/// A command definition with aliases and forms.
///
/// Commands are the semantic operations that players can perform.
/// Each command has a canonical name, optional aliases, and one or
/// more forms that define its argument patterns.
#[derive(Debug, Clone)]
pub struct Command {
    /// Canonical command name.
    pub name: Symbol,
    /// All forms for this command, in priority order.
    pub forms: Vec<Form>,
}

impl Command {
    /// Create a new command with no forms.
    pub fn new(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            forms: Vec::new(),
        }
    }

    /// Add a form to this command.
    pub fn add_form(&mut self, form: Form) {
        self.forms.push(form);
        // Keep forms sorted by priority (highest first)
        self.forms.sort_by(|a, b| b.priority.cmp(&a.priority));
    }

    /// Builder method to add a form.
    pub fn with_form(mut self, form: Form) -> Self {
        self.add_form(form);
        self
    }
}

/// A resolved slot value.
///
/// After matching, slots contain resolved values that can be
/// entities, directions, or raw text.
#[derive(Debug, Clone, PartialEq)]
pub enum SlotValue {
    /// Resolved to an entity.
    Entity(EntityId),
    /// A direction symbol.
    Direction(Symbol),
    /// Raw text (for unresolved or text-only slots).
    Text(Arc<str>),
}

impl SlotValue {
    /// Create an entity slot value.
    pub fn entity(id: EntityId) -> Self {
        Self::Entity(id)
    }

    /// Create a direction slot value.
    pub fn direction(dir: impl Into<Symbol>) -> Self {
        Self::Direction(dir.into())
    }

    /// Create a text slot value.
    pub fn text(s: impl Into<Arc<str>>) -> Self {
        Self::Text(s.into())
    }

    /// Get the entity ID if this is an entity value.
    pub fn as_entity(&self) -> Option<EntityId> {
        match self {
            SlotValue::Entity(id) => Some(*id),
            _ => None,
        }
    }

    /// Get the direction symbol if this is a direction value.
    pub fn as_direction(&self) -> Option<Symbol> {
        match self {
            SlotValue::Direction(dir) => Some(*dir),
            _ => None,
        }
    }

    /// Get the text if this is a text value.
    pub fn as_text(&self) -> Option<&str> {
        match self {
            SlotValue::Text(s) => Some(s),
            _ => None,
        }
    }
}

/// Result of matching input against the grammar.
///
/// Contains the matched action, captured slot values, and metadata
/// about which command and form matched.
#[derive(Debug, Clone)]
pub struct GrammarMatch {
    /// The action to execute.
    pub action: FormAction,
    /// Captured slot values (slot name -> resolved value).
    pub slots: OrdMap<Symbol, SlotValue>,
    /// The command that matched.
    pub command: Symbol,
    /// Priority of the matching form.
    pub priority: i32,
}

impl GrammarMatch {
    /// Create a new grammar match.
    pub fn new(
        action: FormAction,
        slots: OrdMap<Symbol, SlotValue>,
        command: Symbol,
        priority: i32,
    ) -> Self {
        Self {
            action,
            slots,
            command,
            priority,
        }
    }

    /// Get a slot value by name.
    pub fn get_slot(&self, name: &str) -> Option<&SlotValue> {
        self.slots.get(&Symbol::new(name))
    }

    /// Get an entity from a slot.
    pub fn get_entity(&self, name: &str) -> Option<EntityId> {
        self.get_slot(name).and_then(|v| v.as_entity())
    }

    /// Get a direction from a slot.
    pub fn get_direction(&self, name: &str) -> Option<Symbol> {
        self.get_slot(name).and_then(|v| v.as_direction())
    }

    /// Get text from a slot.
    pub fn get_text(&self, name: &str) -> Option<&str> {
        self.get_slot(name).and_then(|v| v.as_text())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_form_element_word() {
        let elem = FormElement::word("look");
        assert!(elem.is_word());
        assert!(!elem.is_slot());
        assert_eq!(elem.slot_name(), None);
    }

    #[test]
    fn test_form_element_noun_slot() {
        let elem = FormElement::noun("obj");
        assert!(!elem.is_word());
        assert!(elem.is_slot());
        assert_eq!(elem.slot_name(), Some(Symbol::new("obj")));
        assert_eq!(elem.slot_type(), Some(&SlotType::Noun));
    }

    #[test]
    fn test_form_element_direction_slot() {
        let elem = FormElement::direction("dir");
        assert!(elem.is_slot());
        assert_eq!(elem.slot_name(), Some(Symbol::new("dir")));
        assert_eq!(elem.slot_type(), Some(&SlotType::Direction));
    }

    #[test]
    fn test_form_element_typed_slot() {
        let elem = FormElement::typed("obj", "portable");
        assert!(elem.is_slot());
        assert_eq!(elem.slot_name(), Some(Symbol::new("obj")));
        assert_eq!(
            elem.slot_type(),
            Some(&SlotType::Custom(Symbol::new("portable")))
        );
    }

    #[test]
    fn test_form_action_simple() {
        let action = FormAction::simple("look-around");
        assert_eq!(action.action_name(), Symbol::new("look-around"));
        assert!(action.slot_refs().is_empty());
    }

    #[test]
    fn test_form_action_with_slots() {
        let action = FormAction::with_slots("examine", vec![Symbol::new("obj")]);
        assert_eq!(action.action_name(), Symbol::new("examine"));
        assert_eq!(action.slot_refs(), &[Symbol::new("obj")]);
    }

    #[test]
    fn test_form_priority() {
        // Empty form
        let empty = Form::new(vec![], FormAction::simple("look"));
        assert_eq!(empty.priority, 0);

        // Single word
        let word = Form::new(vec![FormElement::word("at")], FormAction::simple("look"));
        assert_eq!(word.priority, 10);

        // Word + noun slot
        let with_noun = Form::new(
            vec![FormElement::word("at"), FormElement::noun("obj")],
            FormAction::with_slots("examine", vec![Symbol::new("obj")]),
        );
        assert_eq!(with_noun.priority, 13); // 10 for word + 3 for noun

        // Direction slot
        let direction = Form::new(
            vec![FormElement::direction("dir")],
            FormAction::with_slots("go", vec![Symbol::new("dir")]),
        );
        assert_eq!(direction.priority, 5);
    }

    #[test]
    fn test_command_form_ordering() {
        let mut cmd = Command::new("look");

        // Add forms out of order
        cmd.add_form(Form::new(vec![], FormAction::simple("look-around")));
        cmd.add_form(Form::new(
            vec![FormElement::word("at"), FormElement::noun("obj")],
            FormAction::with_slots("examine", vec![Symbol::new("obj")]),
        ));
        cmd.add_form(Form::new(
            vec![FormElement::direction("dir")],
            FormAction::with_slots("look-direction", vec![Symbol::new("dir")]),
        ));

        // Forms should be sorted by priority (highest first)
        assert_eq!(cmd.forms[0].priority, 13); // "at" + noun
        assert_eq!(cmd.forms[1].priority, 5); // direction
        assert_eq!(cmd.forms[2].priority, 0); // empty
    }

    #[test]
    fn test_slot_value_variants() {
        let entity = SlotValue::entity(EntityId::from_raw(42));
        assert_eq!(entity.as_entity(), Some(EntityId::from_raw(42)));
        assert_eq!(entity.as_direction(), None);
        assert_eq!(entity.as_text(), None);

        let dir = SlotValue::direction("north");
        assert_eq!(dir.as_entity(), None);
        assert_eq!(dir.as_direction(), Some(Symbol::new("north")));
        assert_eq!(dir.as_text(), None);

        let text = SlotValue::text("brass lamp");
        assert_eq!(text.as_entity(), None);
        assert_eq!(text.as_direction(), None);
        assert_eq!(text.as_text(), Some("brass lamp"));
    }

    #[test]
    fn test_grammar_match_accessors() {
        let mut slots = OrdMap::new();
        slots.insert(Symbol::new("obj"), SlotValue::entity(EntityId::from_raw(1)));
        slots.insert(Symbol::new("dir"), SlotValue::direction("north"));
        slots.insert(Symbol::new("text"), SlotValue::text("hello"));

        let gmatch = GrammarMatch::new(
            FormAction::simple("test"),
            slots,
            Symbol::new("test-cmd"),
            10,
        );

        assert_eq!(gmatch.get_entity("obj"), Some(EntityId::from_raw(1)));
        assert_eq!(gmatch.get_direction("dir"), Some(Symbol::new("north")));
        assert_eq!(gmatch.get_text("text"), Some("hello"));
        assert_eq!(gmatch.get_slot("nonexistent"), None);
    }
}
