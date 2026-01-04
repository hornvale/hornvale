//! Direction system for configurable compass directions.
//!
//! This module provides direction definitions and a registry that can be
//! populated from DSL code, allowing games to define custom directions
//! without modifying Rust code.
//!
//! ## Usage
//!
//! ```lisp
//! (directions
//!   (north :abbrev "n" :opposite south :display "to the north")
//!   (south :abbrev "s" :opposite north :display "to the south")
//!   (upstream :opposite downstream :display "upstream"))
//! ```

use crate::symbol::Symbol;
use im::OrdMap;

/// Definition of a single direction.
#[derive(Debug, Clone, PartialEq)]
pub struct DirectionDef {
    /// Canonical name (e.g., "north").
    pub name: Symbol,
    /// Short abbreviation (e.g., "n").
    pub abbrev: Option<String>,
    /// Opposite direction (e.g., "south").
    pub opposite: Option<Symbol>,
    /// Display text (e.g., "to the north").
    pub display: Option<String>,
}

impl DirectionDef {
    /// Create a new direction definition with just a name.
    pub fn new(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            abbrev: None,
            opposite: None,
            display: None,
        }
    }

    /// Set the abbreviation.
    pub fn with_abbrev(mut self, abbrev: impl Into<String>) -> Self {
        self.abbrev = Some(abbrev.into());
        self
    }

    /// Set the opposite direction.
    pub fn with_opposite(mut self, opposite: impl Into<Symbol>) -> Self {
        self.opposite = Some(opposite.into());
        self
    }

    /// Set the display text.
    pub fn with_display(mut self, display: impl Into<String>) -> Self {
        self.display = Some(display.into());
        self
    }
}

/// Registry of all directions.
///
/// The registry maintains a mapping from direction names and abbreviations
/// to their canonical forms, enabling validation and normalization of
/// direction tokens during grammar matching and input parsing.
#[derive(Debug, Clone, Default)]
pub struct DirectionRegistry {
    /// Directions by canonical name.
    directions: OrdMap<Symbol, DirectionDef>,
    /// Token (name or abbreviation, lowercase) to canonical name mapping.
    token_map: OrdMap<String, Symbol>,
}

impl DirectionRegistry {
    /// Create an empty direction registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a registry preloaded with standard compass directions.
    pub fn with_standard_directions() -> Self {
        let mut registry = Self::new();
        registry.register_standard_directions();
        registry
    }

    /// Register the standard compass directions.
    pub fn register_standard_directions(&mut self) {
        // Cardinal directions
        self.register(
            DirectionDef::new("north")
                .with_abbrev("n")
                .with_opposite("south")
                .with_display("to the north"),
        );
        self.register(
            DirectionDef::new("south")
                .with_abbrev("s")
                .with_opposite("north")
                .with_display("to the south"),
        );
        self.register(
            DirectionDef::new("east")
                .with_abbrev("e")
                .with_opposite("west")
                .with_display("to the east"),
        );
        self.register(
            DirectionDef::new("west")
                .with_abbrev("w")
                .with_opposite("east")
                .with_display("to the west"),
        );

        // Vertical
        self.register(
            DirectionDef::new("up")
                .with_abbrev("u")
                .with_opposite("down")
                .with_display("above"),
        );
        self.register(
            DirectionDef::new("down")
                .with_abbrev("d")
                .with_opposite("up")
                .with_display("below"),
        );

        // Diagonals
        self.register(
            DirectionDef::new("northeast")
                .with_abbrev("ne")
                .with_opposite("southwest")
                .with_display("to the northeast"),
        );
        self.register(
            DirectionDef::new("northwest")
                .with_abbrev("nw")
                .with_opposite("southeast")
                .with_display("to the northwest"),
        );
        self.register(
            DirectionDef::new("southeast")
                .with_abbrev("se")
                .with_opposite("northwest")
                .with_display("to the southeast"),
        );
        self.register(
            DirectionDef::new("southwest")
                .with_abbrev("sw")
                .with_opposite("northeast")
                .with_display("to the southwest"),
        );

        // Special directions
        self.register(
            DirectionDef::new("in")
                .with_opposite("out")
                .with_display("inside"),
        );
        self.register(
            DirectionDef::new("out")
                .with_opposite("in")
                .with_display("outside"),
        );
    }

    /// Register a direction definition.
    pub fn register(&mut self, def: DirectionDef) {
        let name = def.name;

        // Map the full name to itself (lowercase)
        self.token_map.insert(name.as_str().to_lowercase(), name);

        // Map the abbreviation if present
        if let Some(ref abbrev) = def.abbrev {
            self.token_map.insert(abbrev.to_lowercase(), name);
        }

        self.directions.insert(name, def);
    }

    /// Clear all registered directions.
    pub fn clear(&mut self) {
        self.directions.clear();
        self.token_map.clear();
    }

    /// Check if a token is a valid direction (name or abbreviation).
    pub fn is_direction(&self, token: &str) -> bool {
        self.token_map.contains_key(&token.to_lowercase())
    }

    /// Normalize a direction token to its canonical name.
    ///
    /// Handles both full names and abbreviations, case-insensitively.
    /// Returns `None` if the token is not a recognized direction.
    pub fn normalize(&self, token: &str) -> Option<Symbol> {
        self.token_map.get(&token.to_lowercase()).copied()
    }

    /// Get a direction definition by its canonical name.
    pub fn get(&self, name: Symbol) -> Option<&DirectionDef> {
        self.directions.get(&name)
    }

    /// Get the opposite direction for a given direction.
    pub fn opposite(&self, name: Symbol) -> Option<Symbol> {
        self.directions.get(&name).and_then(|def| def.opposite)
    }

    /// Get the display text for a direction.
    pub fn display(&self, name: Symbol) -> Option<&str> {
        self.directions
            .get(&name)
            .and_then(|def| def.display.as_deref())
    }

    /// Iterate over all registered directions.
    pub fn iter(&self) -> impl Iterator<Item = (&Symbol, &DirectionDef)> {
        self.directions.iter()
    }

    /// Get the number of registered directions.
    pub fn len(&self) -> usize {
        self.directions.len()
    }

    /// Check if the registry is empty.
    pub fn is_empty(&self) -> bool {
        self.directions.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_direction_def_new() {
        let def = DirectionDef::new("north");
        assert_eq!(def.name, Symbol::new("north"));
        assert!(def.abbrev.is_none());
        assert!(def.opposite.is_none());
        assert!(def.display.is_none());
    }

    #[test]
    fn test_direction_def_builder() {
        let def = DirectionDef::new("north")
            .with_abbrev("n")
            .with_opposite("south")
            .with_display("to the north");

        assert_eq!(def.name, Symbol::new("north"));
        assert_eq!(def.abbrev, Some("n".to_string()));
        assert_eq!(def.opposite, Some(Symbol::new("south")));
        assert_eq!(def.display, Some("to the north".to_string()));
    }

    #[test]
    fn test_registry_empty() {
        let registry = DirectionRegistry::new();
        assert!(registry.is_empty());
        assert_eq!(registry.len(), 0);
        assert!(!registry.is_direction("north"));
    }

    #[test]
    fn test_registry_with_standard_directions() {
        let registry = DirectionRegistry::with_standard_directions();

        // Check count (12 standard directions)
        assert_eq!(registry.len(), 12);

        // Check cardinal directions
        assert!(registry.is_direction("north"));
        assert!(registry.is_direction("south"));
        assert!(registry.is_direction("east"));
        assert!(registry.is_direction("west"));

        // Check abbreviations
        assert!(registry.is_direction("n"));
        assert!(registry.is_direction("s"));
        assert!(registry.is_direction("e"));
        assert!(registry.is_direction("w"));

        // Check diagonals
        assert!(registry.is_direction("northeast"));
        assert!(registry.is_direction("ne"));

        // Check special
        assert!(registry.is_direction("in"));
        assert!(registry.is_direction("out"));
    }

    #[test]
    fn test_registry_normalize() {
        let registry = DirectionRegistry::with_standard_directions();

        // Full names normalize to themselves
        assert_eq!(registry.normalize("north"), Some(Symbol::new("north")));
        assert_eq!(registry.normalize("south"), Some(Symbol::new("south")));

        // Abbreviations normalize to full names
        assert_eq!(registry.normalize("n"), Some(Symbol::new("north")));
        assert_eq!(registry.normalize("s"), Some(Symbol::new("south")));
        assert_eq!(registry.normalize("ne"), Some(Symbol::new("northeast")));

        // Unknown directions return None
        assert_eq!(registry.normalize("foo"), None);
        assert_eq!(registry.normalize("sideways"), None);
    }

    #[test]
    fn test_registry_normalize_case_insensitive() {
        let registry = DirectionRegistry::with_standard_directions();

        assert_eq!(registry.normalize("NORTH"), Some(Symbol::new("north")));
        assert_eq!(registry.normalize("North"), Some(Symbol::new("north")));
        assert_eq!(registry.normalize("N"), Some(Symbol::new("north")));
        assert_eq!(registry.normalize("NE"), Some(Symbol::new("northeast")));
    }

    #[test]
    fn test_registry_get_definition() {
        let registry = DirectionRegistry::with_standard_directions();

        let north = registry.get(Symbol::new("north")).unwrap();
        assert_eq!(north.name, Symbol::new("north"));
        assert_eq!(north.abbrev, Some("n".to_string()));
        assert_eq!(north.opposite, Some(Symbol::new("south")));
        assert_eq!(north.display, Some("to the north".to_string()));
    }

    #[test]
    fn test_registry_opposite() {
        let registry = DirectionRegistry::with_standard_directions();

        assert_eq!(
            registry.opposite(Symbol::new("north")),
            Some(Symbol::new("south"))
        );
        assert_eq!(
            registry.opposite(Symbol::new("south")),
            Some(Symbol::new("north"))
        );
        assert_eq!(
            registry.opposite(Symbol::new("in")),
            Some(Symbol::new("out"))
        );
    }

    #[test]
    fn test_registry_display() {
        let registry = DirectionRegistry::with_standard_directions();

        assert_eq!(registry.display(Symbol::new("north")), Some("to the north"));
        assert_eq!(registry.display(Symbol::new("up")), Some("above"));
        assert_eq!(registry.display(Symbol::new("in")), Some("inside"));
    }

    #[test]
    fn test_custom_direction() {
        let mut registry = DirectionRegistry::new();

        registry.register(
            DirectionDef::new("upstream")
                .with_opposite("downstream")
                .with_display("upstream"),
        );
        registry.register(
            DirectionDef::new("downstream")
                .with_opposite("upstream")
                .with_display("downstream"),
        );

        assert!(registry.is_direction("upstream"));
        assert!(registry.is_direction("downstream"));
        assert_eq!(
            registry.normalize("upstream"),
            Some(Symbol::new("upstream"))
        );
        assert_eq!(
            registry.opposite(Symbol::new("upstream")),
            Some(Symbol::new("downstream"))
        );
    }

    #[test]
    fn test_registry_clear() {
        let mut registry = DirectionRegistry::with_standard_directions();
        assert!(!registry.is_empty());

        registry.clear();
        assert!(registry.is_empty());
        assert!(!registry.is_direction("north"));
    }

    #[test]
    fn test_registry_iter() {
        let registry = DirectionRegistry::with_standard_directions();

        let names: Vec<String> = registry.iter().map(|(name, _)| name.as_str()).collect();
        assert!(names.iter().any(|n| n == "north"));
        assert!(names.iter().any(|n| n == "south"));
        assert!(names.iter().any(|n| n == "up"));
        assert!(names.iter().any(|n| n == "in"));
    }
}
