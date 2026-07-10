//! Command-line argument parsing for the type-audit tool.

use std::path::PathBuf;

/// The subcommand selected on the command line.
#[derive(Debug, PartialEq, Eq)]
pub enum Command {
    /// Default-deny check; optional path roots scope the scan.
    Check {
        /// Directory roots to scan; empty means every workspace crate.
        paths: Vec<PathBuf>,
    },
    /// Regenerate the audit report on stdout.
    Report,
}

/// Parse `argv` (without the program name) into a [`Command`].
pub fn parse_args(args: &[String]) -> Result<Command, String> {
    match args.split_first() {
        Some((cmd, rest)) if cmd == "check" => Ok(Command::Check {
            paths: rest.iter().map(PathBuf::from).collect(),
        }),
        Some((cmd, rest)) if cmd == "report" => {
            if rest.is_empty() {
                Ok(Command::Report)
            } else {
                Err("report takes no arguments".to_string())
            }
        }
        Some((cmd, _)) => Err(format!("unknown command: {cmd}")),
        None => Err("usage: type-audit <check [paths…]|report>".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_check_report_and_rejects_unknown() {
        assert_eq!(
            parse_args(&["check".to_string()]),
            Ok(Command::Check { paths: vec![] })
        );
        assert_eq!(
            parse_args(&["check".to_string(), "kernel".to_string()]),
            Ok(Command::Check {
                paths: vec![PathBuf::from("kernel")]
            })
        );
        assert_eq!(parse_args(&["report".to_string()]), Ok(Command::Report));
        assert!(parse_args(&["frobnicate".to_string()]).is_err());
        assert!(parse_args(&[]).is_err());
    }
}
