//! Type-audit: a syn-based checker that requires every primitive crossing a
//! public API boundary to carry a `type-audit:` verdict in its doc comment.
#![warn(missing_docs)]

pub mod args;
pub mod primitives;

use args::{Command, parse_args};

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = success, non-zero = usage error or audit failure).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { .. }) => {
            // Wired to the real walk + audit in Task 7.
            0
        }
        Ok(Command::Report) => {
            // Wired to the report generator in Task 8.
            0
        }
        Err(msg) => {
            eprintln!("{msg}");
            2
        }
    }
}
