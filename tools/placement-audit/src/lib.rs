//! Placement-audit: a syn-based checker that detects "shape twins" — pub
//! enums/structs with identical member-name sets duplicated across two or
//! more crates in `kernel/` and `domains/`.
#![warn(missing_docs)]

pub mod args;
pub mod detect;
pub mod extract;
pub mod walk;

use args::{Command, parse_args};

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = no twins found, 1 = twin group(s) found, 2 = usage or scan
/// error).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { paths }) => match walk::scan(&paths) {
            Ok(crates) => {
                let twins = detect::twins(&crates);
                for line in render_twin_lines(&twins) {
                    println!("{line}");
                }
                if twins.is_empty() {
                    0
                } else {
                    eprintln!("{} shape-twin group(s) found", twins.len());
                    1
                }
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
        Ok(Command::Report) => match walk::scan(&[]) {
            Ok(crates) => {
                print!("{}", render_report(&detect::twins(&crates)));
                0
            }
            Err(e) => {
                eprintln!("scan error: {e}");
                2
            }
        },
        Err(msg) => {
            eprintln!("{msg}");
            2
        }
    }
}

/// One diagnostic line per member of every twin group, in the groups'
/// already-deterministic order.
fn render_twin_lines(twins: &[detect::TwinGroup]) -> Vec<String> {
    let mut lines = Vec::new();
    for group in twins {
        let names: Vec<String> = group
            .members
            .iter()
            .map(|t| format!("{}::{}", t.crate_name, t.name))
            .collect();
        lines.push(format!("twin: {}", names.join(" == ")));
    }
    lines
}

/// Render a minimal Markdown report of every twin group found. Verdict tags
/// (Task 10) and gate wiring (Task 11) are not implemented yet — this is a
/// findings dump, not the drift-checked committed artifact.
fn render_report(twins: &[detect::TwinGroup]) -> String {
    let mut out = String::from("# Placement audit report\n\n");
    if twins.is_empty() {
        out.push_str("No shape twins found.\n");
        return out;
    }
    for group in twins {
        let names: Vec<String> = group
            .members
            .iter()
            .map(|t| format!("`{}::{}`", t.crate_name, t.name))
            .collect();
        out.push_str(&format!("- {}\n", names.join(" == ")));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_command_exits_two() {
        assert_eq!(run(&["frobnicate".to_string()]), 2);
    }

    #[test]
    fn no_args_exits_two() {
        assert_eq!(run(&[]), 2);
    }
}
