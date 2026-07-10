//! Type-audit: a syn-based checker that requires every primitive crossing a
//! public API boundary to carry a `type-audit:` verdict in its doc comment.
#![warn(missing_docs)]

pub mod args;
pub mod audit;
pub mod extract;
pub mod primitives;
pub mod tag;
pub mod walk;

use args::{Command, parse_args};

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = success, non-zero = usage error or audit failure).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { paths }) => match walk::scan(&paths) {
            Ok(crates) => {
                let mut lines: Vec<String> = Vec::new();
                for c in &crates {
                    for f in audit::audit_items(&c.items) {
                        lines.push(format!(
                            "{}:{}: {} ({})",
                            c.crate_name, f.line, f.message, f.item
                        ));
                    }
                }
                lines.sort();
                for l in &lines {
                    println!("{l}");
                }
                if lines.is_empty() {
                    0
                } else {
                    eprintln!("{} untagged/stale/malformed primitive(s)", lines.len());
                    1
                }
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
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
