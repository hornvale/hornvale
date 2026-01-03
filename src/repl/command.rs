//! DSL-defined REPL commands.
//!
//! Allows REPL commands to be defined in the DSL:
//!
//! ```lisp
//! (repl-command "l"
//!   :aliases ("ls" "list")
//!   :doc "List all entities"
//!   :expands-to "(list-entities)")
//!
//! (repl-command "i"
//!   :aliases ("inspect")
//!   :doc "Inspect an entity"
//!   :args (id)
//!   :expands-to "(inspect-entity $id)")
//! ```

use crate::lang::SExpr;
use crate::symbol::Symbol;
use im::OrdMap;
use std::sync::Arc;

/// A DSL-defined REPL command.
#[derive(Debug, Clone)]
pub struct ReplCommand {
    /// Primary command name.
    pub name: Symbol,
    /// Alternative names (aliases).
    pub aliases: Vec<Symbol>,
    /// Documentation string.
    pub doc: Option<String>,
    /// Argument names (optional args prefixed with ?).
    pub args: Vec<ArgSpec>,
    /// Expression template to expand to.
    pub expansion: Arc<str>,
}

/// Specification for a command argument.
#[derive(Debug, Clone)]
pub struct ArgSpec {
    /// Argument name (without $ prefix).
    pub name: Symbol,
    /// Whether this argument is optional.
    pub optional: bool,
}

impl ReplCommand {
    /// Create a new REPL command.
    pub fn new(name: impl Into<Symbol>, expansion: impl Into<Arc<str>>) -> Self {
        Self {
            name: name.into(),
            aliases: Vec::new(),
            doc: None,
            args: Vec::new(),
            expansion: expansion.into(),
        }
    }

    /// Add documentation.
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }

    /// Add aliases.
    pub fn with_aliases(mut self, aliases: Vec<Symbol>) -> Self {
        self.aliases = aliases;
        self
    }

    /// Add arguments.
    pub fn with_args(mut self, args: Vec<ArgSpec>) -> Self {
        self.args = args;
        self
    }

    /// Get the minimum required argument count.
    pub fn min_args(&self) -> usize {
        self.args.iter().filter(|a| !a.optional).count()
    }

    /// Get the maximum argument count.
    pub fn max_args(&self) -> usize {
        self.args.len()
    }

    /// Expand this command with the given arguments.
    ///
    /// Replaces `$name` placeholders with argument values.
    pub fn expand(&self, args: &[&str]) -> Result<String, String> {
        // Check argument count
        if args.len() < self.min_args() {
            return Err(format!(
                "command '{}' requires at least {} argument(s), got {}",
                self.name.as_str(),
                self.min_args(),
                args.len()
            ));
        }
        if args.len() > self.max_args() {
            return Err(format!(
                "command '{}' accepts at most {} argument(s), got {}",
                self.name.as_str(),
                self.max_args(),
                args.len()
            ));
        }

        let mut result = self.expansion.to_string();

        // Replace named arguments
        for (i, arg_spec) in self.args.iter().enumerate() {
            let placeholder = format!("${}", arg_spec.name.as_str());
            let value = args.get(i).copied().unwrap_or("nil");
            result = result.replace(&placeholder, value);
        }

        // Replace positional arguments ($1, $2, etc.)
        for (i, &arg) in args.iter().enumerate() {
            let placeholder = format!("${}", i + 1);
            result = result.replace(&placeholder, arg);
        }

        Ok(result)
    }

    /// Get help text for this command.
    pub fn help(&self) -> String {
        let mut help = self.name.as_str().to_string();

        // Add arguments
        for arg in &self.args {
            if arg.optional {
                help.push_str(&format!(" [{}]", arg.name.as_str()));
            } else {
                help.push_str(&format!(" <{}>", arg.name.as_str()));
            }
        }

        // Add aliases
        if !self.aliases.is_empty() {
            let alias_str: Vec<_> = self.aliases.iter().map(|a| a.as_str()).collect();
            help.push_str(&format!(" (alias: {})", alias_str.join(", ")));
        }

        // Add doc
        if let Some(doc) = &self.doc {
            help.push_str(&format!(" - {doc}"));
        }

        help
    }
}

impl ArgSpec {
    /// Create a required argument.
    pub fn required(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            optional: false,
        }
    }

    /// Create an optional argument.
    pub fn optional(name: impl Into<Symbol>) -> Self {
        Self {
            name: name.into(),
            optional: true,
        }
    }
}

/// Registry of DSL-defined REPL commands.
#[derive(Debug, Clone, Default)]
pub struct ReplCommandRegistry {
    /// Commands by name.
    commands: OrdMap<Symbol, ReplCommand>,
    /// Alias to primary name mapping.
    aliases: OrdMap<Symbol, Symbol>,
}

impl ReplCommandRegistry {
    /// Create an empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a command.
    pub fn register(&mut self, cmd: ReplCommand) {
        // Register aliases
        for alias in &cmd.aliases {
            self.aliases.insert(*alias, cmd.name);
        }
        // Register primary name
        self.commands.insert(cmd.name, cmd);
    }

    /// Get a command by name or alias.
    pub fn get(&self, name: &str) -> Option<&ReplCommand> {
        let sym = Symbol::new(name);
        // Check if it's an alias first
        if let Some(&primary) = self.aliases.get(&sym) {
            self.commands.get(&primary)
        } else {
            self.commands.get(&sym)
        }
    }

    /// Check if a command exists.
    pub fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    /// List all commands.
    pub fn commands(&self) -> impl Iterator<Item = &ReplCommand> {
        self.commands.values()
    }

    /// Number of registered commands.
    pub fn len(&self) -> usize {
        self.commands.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.commands.is_empty()
    }
}

/// Parse a repl-command definition from an S-expression.
///
/// Syntax:
/// ```lisp
/// (repl-command "name"
///   :aliases ("a1" "a2")
///   :doc "description"
///   :args (arg1 ?opt-arg)
///   :expands-to "(expression $arg1 $opt-arg)")
/// ```
pub fn parse_repl_command(expr: &SExpr) -> Result<ReplCommand, String> {
    let items = expr
        .as_list()
        .ok_or_else(|| "repl-command must be a list".to_string())?;

    if items.len() < 2 {
        return Err("repl-command requires a name".to_string());
    }

    // Get the command name (can be string or symbol)
    let name = items[1]
        .as_string()
        .map(Symbol::new)
        .or_else(|| items[1].as_symbol())
        .ok_or_else(|| "command name must be a string or symbol".to_string())?;

    let mut cmd = ReplCommand {
        name,
        aliases: Vec::new(),
        doc: None,
        args: Vec::new(),
        expansion: "nil".into(),
    };

    // Parse keyword arguments
    let mut i = 2;
    while i < items.len() {
        if items[i].is_keyword("aliases") && i + 1 < items.len() {
            cmd.aliases = parse_aliases(&items[i + 1])?;
            i += 2;
        } else if items[i].is_keyword("doc") && i + 1 < items.len() {
            cmd.doc = items[i + 1].as_string().map(|s| s.to_string());
            i += 2;
        } else if items[i].is_keyword("args") && i + 1 < items.len() {
            cmd.args = parse_args(&items[i + 1])?;
            i += 2;
        } else if items[i].is_keyword("expands-to") && i + 1 < items.len() {
            // Accept either a string literal or a parsed expression
            if let Some(s) = items[i + 1].as_string() {
                cmd.expansion = s.into();
            } else {
                cmd.expansion = format!("{}", items[i + 1]).into();
            }
            i += 2;
        } else {
            i += 1;
        }
    }

    Ok(cmd)
}

/// Parse aliases from a list of strings/symbols.
fn parse_aliases(expr: &SExpr) -> Result<Vec<Symbol>, String> {
    let items = expr
        .as_list()
        .ok_or_else(|| "aliases must be a list".to_string())?;

    let mut aliases = Vec::new();
    for item in items {
        if let Some(s) = item.as_string() {
            aliases.push(Symbol::new(s));
        } else if let Some(s) = item.as_symbol() {
            aliases.push(s);
        }
    }
    Ok(aliases)
}

/// Parse argument specs from a list.
fn parse_args(expr: &SExpr) -> Result<Vec<ArgSpec>, String> {
    let items = expr
        .as_list()
        .ok_or_else(|| "args must be a list".to_string())?;

    let mut args = Vec::new();
    for item in items {
        let sym = item
            .as_symbol()
            .ok_or_else(|| "arg must be a symbol".to_string())?;

        let name_str = sym.as_str();
        if let Some(stripped) = name_str.strip_prefix('?') {
            // Optional argument
            args.push(ArgSpec::optional(stripped));
        } else {
            // Required argument
            args.push(ArgSpec::required(name_str));
        }
    }
    Ok(args)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::parse;

    #[test]
    fn test_repl_command_basic() {
        let cmd = ReplCommand::new("test", "(+ 1 2)");
        assert_eq!(cmd.name.as_str(), "test");
        assert_eq!(cmd.expand(&[]).unwrap(), "(+ 1 2)");
    }

    #[test]
    fn test_repl_command_with_args() {
        let cmd = ReplCommand::new("add", "(+ $a $b)")
            .with_args(vec![ArgSpec::required("a"), ArgSpec::required("b")]);

        assert_eq!(cmd.expand(&["1", "2"]).unwrap(), "(+ 1 2)");
    }

    #[test]
    fn test_repl_command_with_optional_args() {
        let cmd = ReplCommand::new("tick", "(advance-ticks (or $n 1))")
            .with_args(vec![ArgSpec::optional("n")]);

        assert_eq!(cmd.min_args(), 0);
        assert_eq!(cmd.max_args(), 1);
        assert_eq!(cmd.expand(&[]).unwrap(), "(advance-ticks (or nil 1))");
        assert_eq!(cmd.expand(&["5"]).unwrap(), "(advance-ticks (or 5 1))");
    }

    #[test]
    fn test_repl_command_arity_error() {
        let cmd = ReplCommand::new("add", "(+ $a $b)")
            .with_args(vec![ArgSpec::required("a"), ArgSpec::required("b")]);

        assert!(cmd.expand(&["1"]).is_err());
        assert!(cmd.expand(&["1", "2", "3"]).is_err());
    }

    #[test]
    fn test_registry_basic() {
        let mut registry = ReplCommandRegistry::new();
        registry.register(ReplCommand::new("test", "(+ 1 2)").with_aliases(vec![Symbol::new("t")]));

        assert!(registry.contains("test"));
        assert!(registry.contains("t"));
        assert!(!registry.contains("foo"));
    }

    #[test]
    fn test_parse_repl_command() {
        let expr = parse(
            r#"(repl-command "l"
                 :aliases ("ls" "list")
                 :doc "List entities"
                 :expands-to (list-entities))"#,
        )
        .unwrap();

        let cmd = parse_repl_command(&expr).unwrap();
        assert_eq!(cmd.name.as_str(), "l");
        assert_eq!(cmd.aliases.len(), 2);
        assert_eq!(cmd.doc, Some("List entities".to_string()));
    }

    #[test]
    fn test_parse_repl_command_with_args() {
        let expr = parse(
            r#"(repl-command "i"
                 :doc "Inspect entity"
                 :args (id)
                 :expands-to "(inspect-entity $id)")"#,
        )
        .unwrap();

        let cmd = parse_repl_command(&expr).unwrap();
        assert_eq!(cmd.args.len(), 1);
        assert!(!cmd.args[0].optional);
        assert_eq!(cmd.args[0].name.as_str(), "id");
        assert!(cmd.expansion.contains("$id"));
    }

    #[test]
    fn test_parse_repl_command_optional_args() {
        let expr = parse(
            r#"(repl-command "t"
                 :args (?n)
                 :expands-to "(advance-ticks (or $n 1))")"#,
        )
        .unwrap();

        let cmd = parse_repl_command(&expr).unwrap();
        assert_eq!(cmd.args.len(), 1);
        assert!(cmd.args[0].optional);
        assert_eq!(cmd.args[0].name.as_str(), "n");
    }

    #[test]
    fn test_command_help() {
        let cmd = ReplCommand::new("inspect", "(inspect-entity $id)")
            .with_doc("Show entity details")
            .with_aliases(vec![Symbol::new("i")])
            .with_args(vec![ArgSpec::required("id")]);

        let help = cmd.help();
        assert!(help.contains("inspect"));
        assert!(help.contains("<id>"));
        assert!(help.contains("alias: i"));
        assert!(help.contains("Show entity details"));
    }
}
