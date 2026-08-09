//! The Cairn — a git-native, append-only medium for cross-session agent
//! coordination. See `docs/superpowers/specs/2026-08-09-the-cairn-design.md`.
#![warn(missing_docs)]

pub mod git;
pub mod post;

/// Everything that can go wrong, always carrying the physical reason.
#[derive(Debug)]
pub enum BoardError {
    /// A `git` invocation exited non-zero.
    Git {
        /// The argv, joined, for the message.
        cmd: String,
        /// The exit code, if the process exited normally.
        code: Option<i32>,
        /// Whatever git said on stderr.
        stderr: String,
    },
    /// A post did not parse, or would not serialize.
    Json(String),
    /// Filesystem trouble.
    Io(String),
}

impl std::fmt::Display for BoardError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Git { cmd, code, stderr } => {
                write!(f, "git {cmd} failed (code {code:?}): {stderr}")
            }
            Self::Json(m) => write!(f, "post json: {m}"),
            Self::Io(m) => write!(f, "io: {m}"),
        }
    }
}

impl std::error::Error for BoardError {}
