//! Intent trie for command dispatch.
//!
//! The intent trie maps token prefixes to commands using longest-prefix matching.
//! This allows aliases like "pick up" to route to the "take" command while
//! "pick" alone might route to a different command (or not match at all).

use crate::symbol::Symbol;
use im::OrdMap;

/// A trie node for intent dispatch.
#[derive(Debug, Clone, Default)]
pub struct TrieNode {
    /// Command at this node (if this is a valid endpoint).
    pub command: Option<Symbol>,
    /// Children keyed by token (lowercase).
    pub children: OrdMap<String, TrieNode>,
}

impl TrieNode {
    /// Create a new empty trie node.
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a node with a command.
    #[allow(dead_code)]
    pub fn with_command(command: Symbol) -> Self {
        Self {
            command: Some(command),
            children: OrdMap::new(),
        }
    }
}

/// Intent trie for longest-prefix command matching.
///
/// The trie supports:
/// - Single-word aliases: `"look"` -> look command
/// - Multi-word aliases: `["pick", "up"]` -> take command
/// - Abbreviations: `"l"` -> look command
///
/// Matching uses longest-prefix: if both "look" and "look at" are registered,
/// input "look at lamp" will match "look at" (consuming 2 tokens) rather than
/// just "look" (consuming 1 token).
#[derive(Debug, Clone, Default)]
pub struct IntentTrie {
    root: TrieNode,
}

impl IntentTrie {
    /// Create a new empty intent trie.
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert an alias path that routes to a command.
    ///
    /// # Arguments
    /// * `tokens` - The alias tokens (e.g., `["pick", "up"]` for multi-word)
    /// * `command` - The command name to route to
    ///
    /// # Examples
    ///
    /// ```
    /// use hornvale::grammar::IntentTrie;
    /// use hornvale::symbol::Symbol;
    ///
    /// let mut trie = IntentTrie::new();
    /// trie.insert(&["look"], Symbol::new("look"));
    /// trie.insert(&["l"], Symbol::new("look"));
    /// trie.insert(&["pick", "up"], Symbol::new("take"));
    /// ```
    pub fn insert(&mut self, tokens: &[&str], command: Symbol) {
        if tokens.is_empty() {
            return;
        }

        let mut node = &mut self.root;
        for token in tokens {
            let key = token.to_lowercase();
            node = node.children.entry(key).or_default();
        }
        node.command = Some(command);
    }

    /// Find the longest matching prefix.
    ///
    /// Returns `(command, tokens_consumed)` if a match is found.
    /// The remaining tokens (after `tokens_consumed`) should be passed
    /// to the command's form matcher.
    ///
    /// # Examples
    ///
    /// ```
    /// use hornvale::grammar::IntentTrie;
    /// use hornvale::symbol::Symbol;
    ///
    /// let mut trie = IntentTrie::new();
    /// trie.insert(&["look"], Symbol::new("look"));
    /// trie.insert(&["look", "at"], Symbol::new("look")); // same command, different form
    /// trie.insert(&["pick", "up"], Symbol::new("take"));
    ///
    /// // "look at lamp" matches "look at" (2 tokens)
    /// let result = trie.longest_match(&["look", "at", "lamp"]);
    /// assert_eq!(result, Some((Symbol::new("look"), 2)));
    ///
    /// // "pick up key" matches "pick up" (2 tokens)
    /// let result = trie.longest_match(&["pick", "up", "key"]);
    /// assert_eq!(result, Some((Symbol::new("take"), 2)));
    ///
    /// // "pick the lock" only matches if "pick" is registered
    /// let result = trie.longest_match(&["pick", "the", "lock"]);
    /// assert_eq!(result, None); // "pick" alone isn't registered
    /// ```
    pub fn longest_match(&self, tokens: &[&str]) -> Option<(Symbol, usize)> {
        if tokens.is_empty() {
            return None;
        }

        let mut node = &self.root;
        let mut last_match: Option<(Symbol, usize)> = None;

        for (i, token) in tokens.iter().enumerate() {
            let key = token.to_lowercase();
            match node.children.get(&key) {
                Some(child) => {
                    node = child;
                    // If this node has a command, record it as a potential match
                    if let Some(cmd) = node.command {
                        last_match = Some((cmd, i + 1));
                    }
                }
                None => break,
            }
        }

        last_match
    }

    /// Check if the trie is empty.
    pub fn is_empty(&self) -> bool {
        self.root.children.is_empty() && self.root.command.is_none()
    }

    /// Get all commands reachable from the root (for help/listing).
    pub fn commands(&self) -> Vec<Symbol> {
        let mut result = Vec::new();
        Self::collect_commands(&self.root, &mut result);
        result.sort();
        result.dedup();
        result
    }

    fn collect_commands(node: &TrieNode, result: &mut Vec<Symbol>) {
        if let Some(cmd) = node.command {
            result.push(cmd);
        }
        for child in node.children.values() {
            Self::collect_commands(child, result);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_trie() {
        let trie = IntentTrie::new();
        assert!(trie.is_empty());
        assert_eq!(trie.longest_match(&["look"]), None);
        assert_eq!(trie.longest_match(&[]), None);
    }

    #[test]
    fn test_single_word_insert() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));

        assert!(!trie.is_empty());
        assert_eq!(
            trie.longest_match(&["look"]),
            Some((Symbol::new("look"), 1))
        );
        assert_eq!(
            trie.longest_match(&["look", "at", "lamp"]),
            Some((Symbol::new("look"), 1))
        );
        assert_eq!(trie.longest_match(&["take"]), None);
    }

    #[test]
    fn test_abbreviation() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));
        trie.insert(&["l"], Symbol::new("look"));

        assert_eq!(trie.longest_match(&["l"]), Some((Symbol::new("look"), 1)));
        assert_eq!(
            trie.longest_match(&["look"]),
            Some((Symbol::new("look"), 1))
        );
    }

    #[test]
    fn test_multi_word_alias() {
        let mut trie = IntentTrie::new();
        trie.insert(&["pick", "up"], Symbol::new("take"));

        // "pick" alone doesn't match
        assert_eq!(trie.longest_match(&["pick"]), None);

        // "pick up" matches
        assert_eq!(
            trie.longest_match(&["pick", "up"]),
            Some((Symbol::new("take"), 2))
        );

        // "pick up key" matches, consuming 2 tokens
        assert_eq!(
            trie.longest_match(&["pick", "up", "key"]),
            Some((Symbol::new("take"), 2))
        );
    }

    #[test]
    fn test_longest_match_priority() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));
        trie.insert(&["look", "at"], Symbol::new("look")); // Different entry point, same command

        // "look" matches 1 token
        assert_eq!(
            trie.longest_match(&["look"]),
            Some((Symbol::new("look"), 1))
        );

        // "look at lamp" matches 2 tokens (longest)
        assert_eq!(
            trie.longest_match(&["look", "at", "lamp"]),
            Some((Symbol::new("look"), 2))
        );

        // "look around" matches 1 token (no "look around" registered)
        assert_eq!(
            trie.longest_match(&["look", "around"]),
            Some((Symbol::new("look"), 1))
        );
    }

    #[test]
    fn test_case_insensitive() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));

        assert_eq!(
            trie.longest_match(&["LOOK"]),
            Some((Symbol::new("look"), 1))
        );
        assert_eq!(
            trie.longest_match(&["Look"]),
            Some((Symbol::new("look"), 1))
        );
    }

    #[test]
    fn test_multiple_commands() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));
        trie.insert(&["take"], Symbol::new("take"));
        trie.insert(&["get"], Symbol::new("take"));
        trie.insert(&["drop"], Symbol::new("drop"));

        assert_eq!(
            trie.longest_match(&["look"]),
            Some((Symbol::new("look"), 1))
        );
        assert_eq!(
            trie.longest_match(&["take"]),
            Some((Symbol::new("take"), 1))
        );
        assert_eq!(trie.longest_match(&["get"]), Some((Symbol::new("take"), 1)));
        assert_eq!(
            trie.longest_match(&["drop"]),
            Some((Symbol::new("drop"), 1))
        );
    }

    #[test]
    fn test_commands_list() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));
        trie.insert(&["l"], Symbol::new("look"));
        trie.insert(&["take"], Symbol::new("take"));
        trie.insert(&["get"], Symbol::new("take"));

        let commands = trie.commands();
        assert_eq!(commands.len(), 2); // look and take (deduplicated)
        assert!(commands.contains(&Symbol::new("look")));
        assert!(commands.contains(&Symbol::new("take")));
    }

    #[test]
    fn test_empty_tokens() {
        let mut trie = IntentTrie::new();
        trie.insert(&["look"], Symbol::new("look"));

        assert_eq!(trie.longest_match(&[]), None);

        // Empty insert is a no-op
        trie.insert(&[], Symbol::new("empty"));
        assert_eq!(trie.longest_match(&[]), None);
    }

    #[test]
    fn test_overlapping_paths() {
        let mut trie = IntentTrie::new();
        trie.insert(&["put"], Symbol::new("put"));
        trie.insert(&["put", "down"], Symbol::new("drop"));

        // "put lamp" matches "put" (1 token)
        assert_eq!(
            trie.longest_match(&["put", "lamp"]),
            Some((Symbol::new("put"), 1))
        );

        // "put down lamp" matches "put down" (2 tokens)
        assert_eq!(
            trie.longest_match(&["put", "down", "lamp"]),
            Some((Symbol::new("drop"), 2))
        );
    }
}
