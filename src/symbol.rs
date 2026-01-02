//! Symbol interning for efficient string comparison and storage.
//!
//! Symbols are interned strings represented as integer IDs. This provides:
//! - O(1) equality comparison
//! - Compact storage (u32 instead of String)
//! - Copy semantics

use std::collections::HashMap;
use std::sync::{LazyLock, Mutex};

/// Global intern table mapping strings to symbol IDs and back.
struct InternTable {
    /// String to ID mapping
    to_id: HashMap<Box<str>, u32>,
    /// ID to string mapping (index = ID)
    to_str: Vec<Box<str>>,
}

impl InternTable {
    fn new() -> Self {
        Self {
            to_id: HashMap::new(),
            to_str: Vec::new(),
        }
    }

    fn intern(&mut self, s: &str) -> u32 {
        if let Some(&id) = self.to_id.get(s) {
            return id;
        }

        let id = self.to_str.len() as u32;
        let boxed: Box<str> = s.into();
        self.to_str.push(boxed.clone());
        self.to_id.insert(boxed, id);
        id
    }

    fn resolve(&self, id: u32) -> Option<&str> {
        self.to_str.get(id as usize).map(|s| s.as_ref())
    }
}

static INTERN_TABLE: LazyLock<Mutex<InternTable>> =
    LazyLock::new(|| Mutex::new(InternTable::new()));

/// An interned string symbol.
///
/// Symbols are cheap to copy and compare (just integer comparison).
/// The actual string content is stored in a global intern table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(u32);

impl Symbol {
    /// Create or retrieve a symbol for the given string.
    pub fn new(s: &str) -> Self {
        let id = INTERN_TABLE.lock().unwrap().intern(s);
        Symbol(id)
    }

    /// Get the string content of this symbol.
    pub fn as_str(&self) -> String {
        INTERN_TABLE
            .lock()
            .unwrap()
            .resolve(self.0)
            .expect("Symbol ID should always be valid")
            .to_string()
    }

    /// Get the raw ID (for debugging/serialization).
    pub fn id(&self) -> u32 {
        self.0
    }
}

impl std::fmt::Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Symbol({:?})", self.as_str())
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        Symbol::new(s)
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        Symbol::new(&s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_roundtrip() {
        let sym = Symbol::new("hello");
        assert_eq!(sym.as_str(), "hello");
    }

    #[test]
    fn test_symbol_dedup() {
        let sym1 = Symbol::new("world");
        let sym2 = Symbol::new("world");
        assert_eq!(sym1, sym2);
        assert_eq!(sym1.id(), sym2.id());
    }

    #[test]
    fn test_symbol_different() {
        let sym1 = Symbol::new("foo");
        let sym2 = Symbol::new("bar");
        assert_ne!(sym1, sym2);
    }

    #[test]
    fn test_symbol_ordering() {
        // Ordering is by ID (insertion order), not alphabetical
        let sym1 = Symbol::new("zebra_test");
        let sym2 = Symbol::new("apple_test");
        // sym1 was interned first, so it has a lower ID
        assert!(sym1 < sym2);
    }
}
