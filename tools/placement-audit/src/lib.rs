//! Placement-audit: a syn-based checker that detects "shape twins" — pub
//! enums/structs with identical member-name sets duplicated across two or
//! more crates in `kernel/` and `domains/`.
#![warn(missing_docs)]

pub mod args;
pub mod detect;
pub mod extract;
pub mod fingerprint;
pub mod report;
pub mod tag;
pub mod verdict;
pub mod walk;

use args::{Command, parse_args};

/// Run the tool with `argv` (without the program name); returns the process
/// exit code (0 = no findings, 1 = untagged/stale/malformed twin(s) found,
/// 2 = usage or scan error).
pub fn run(args: &[String]) -> i32 {
    match parse_args(args) {
        Ok(Command::Check { paths }) => match walk::scan(&paths) {
            Ok(crates) => {
                let twins = detect::twins(&crates);
                let findings = verdict::judge(&twins);
                for f in &findings {
                    println!(
                        "{}:{}: {} ({})",
                        f.crate_name, f.line, f.message, f.type_name
                    );
                }
                if findings.is_empty() {
                    0
                } else {
                    eprintln!("{} untagged/stale/malformed twin(s) found", findings.len());
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
                print!("{}", report::render_report(&detect::twins(&crates)));
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
