//! Command-line argument parsing for the plumb tool.

use std::path::PathBuf;

/// The subcommand selected on the command line.
#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    /// List every constant that carries no usable rung. **Report-only today**
    /// — see [`crate::run`].
    Check {
        /// Directory or file roots to scan; empty means
        /// [`crate::walk::AUDITED_ROOTS`].
        paths: Vec<PathBuf>,
    },
    /// Render the coverage report on stdout.
    Report {
        /// Directory or file roots to scan; empty means
        /// [`crate::walk::AUDITED_ROOTS`].
        paths: Vec<PathBuf>,
    },
}

/// Parse `argv` (without the program name) into a [`Command`].
pub fn parse_args(args: &[String]) -> Result<Command, String> {
    match args.split_first() {
        Some((cmd, rest)) if cmd == "check" => Ok(Command::Check {
            paths: rest.iter().map(PathBuf::from).collect(),
        }),
        Some((cmd, rest)) if cmd == "report" => Ok(Command::Report {
            paths: rest.iter().map(PathBuf::from).collect(),
        }),
        Some((cmd, _)) => Err(format!("unknown command: {cmd}")),
        None => Err("usage: plumb <check|report> [paths…]".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_check_and_report_and_rejects_unknown() {
        assert_eq!(
            parse_args(&["check".to_string()]),
            Ok(Command::Check { paths: vec![] })
        );
        assert_eq!(
            parse_args(&["report".to_string(), "kernel".to_string()]),
            Ok(Command::Report {
                paths: vec![PathBuf::from("kernel")]
            })
        );
        assert!(parse_args(&["frobnicate".to_string()]).is_err());
        assert!(parse_args(&[]).is_err());
    }

    /// `report` takes roots here where `type-audit`'s refuses them, because the
    /// default scope excludes `kernel/` and `cli/` and a reader must be able to
    /// widen it without editing the tool.
    #[test]
    fn report_accepts_explicit_roots() {
        assert_eq!(
            parse_args(&[
                "report".to_string(),
                "kernel".to_string(),
                "cli".to_string()
            ]),
            Ok(Command::Report {
                paths: vec![PathBuf::from("kernel"), PathBuf::from("cli")]
            })
        );
    }
}
