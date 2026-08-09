//! The only place this crate invokes `git`. Every operation takes an explicit
//! repo root; nothing depends on the process working directory, which is what
//! makes the tests hermetic.

use crate::BoardError;
use std::path::{Path, PathBuf};
use std::process::Command;

/// A handle to a git repository, identified by its root directory.
#[derive(Debug, Clone)]
pub struct Repo {
    root: PathBuf,
}

impl Repo {
    /// Wrap a repository root.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// The repository root.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Run git, returning trimmed stdout, or the physical reason it failed.
    pub fn git(&self, args: &[&str]) -> Result<String, BoardError> {
        let out = self
            .command()
            .args(args)
            .output()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
        Self::finish(args, out)
    }

    /// Run git with `GIT_INDEX_FILE` pointed at `index`, returning trimmed
    /// stdout. This is how a caller builds a tree without touching the
    /// repository's real index (`git mktree` cannot place a file under a
    /// path containing a slash, so a throwaway index plus `read-tree` /
    /// `update-index` / `write-tree` is the working recipe — see
    /// `store::Board::tree_with`). `index` must be an absolute path: git
    /// resolves a relative `GIT_INDEX_FILE` against its own `-C` root, but
    /// any Rust-side cleanup of the same path resolves against the
    /// *process* cwd, so a relative path here is a latent leak.
    pub fn git_with_index(&self, index: &Path, args: &[&str]) -> Result<String, BoardError> {
        let out = self
            .command()
            .env("GIT_INDEX_FILE", index)
            .args(args)
            .output()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
        Self::finish(args, out)
    }

    /// The `git -C <root>` invocation common to every command this crate runs.
    fn command(&self) -> Command {
        let mut cmd = Command::new("git");
        cmd.arg("-C").arg(&self.root);
        cmd
    }

    /// Shared success/failure handling for a finished `git` invocation.
    fn finish(args: &[&str], out: std::process::Output) -> Result<String, BoardError> {
        if !out.status.success() {
            return Err(BoardError::Git {
                cmd: args.join(" "),
                code: out.status.code(),
                stderr: String::from_utf8_lossy(&out.stderr).trim().to_string(),
            });
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim_end().to_string())
    }

    /// Run git with bytes on stdin, returning trimmed stdout.
    pub fn git_stdin(&self, args: &[&str], input: &[u8]) -> Result<String, BoardError> {
        use std::io::Write;
        use std::process::Stdio;
        let mut child = Command::new("git")
            .arg("-C")
            .arg(&self.root)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
        child
            .stdin
            .as_mut()
            .ok_or_else(|| BoardError::Io("git stdin".into()))?
            .write_all(input)
            .map_err(|e| BoardError::Io(format!("writing git stdin: {e}")))?;
        let out = child
            .wait_with_output()
            .map_err(|e| BoardError::Io(format!("waiting on git: {e}")))?;
        if !out.status.success() {
            return Err(BoardError::Git {
                cmd: args.join(" "),
                code: out.status.code(),
                stderr: String::from_utf8_lossy(&out.stderr).trim().to_string(),
            });
        }
        Ok(String::from_utf8_lossy(&out.stdout).trim_end().to_string())
    }

    /// Resolve a ref, reporting absence as `None` rather than an error.
    pub fn rev_parse_verify(&self, refname: &str) -> Result<Option<String>, BoardError> {
        match self.git(&["rev-parse", "--verify", "--quiet", refname]) {
            Ok(sha) if !sha.is_empty() => Ok(Some(sha)),
            Ok(_) => Ok(None),
            // `--quiet` makes a missing ref exit 1 with empty stderr; that is
            // absence, not failure.
            Err(BoardError::Git { stderr, .. }) if stderr.is_empty() => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// Write `bytes` as a blob and return its object id.
    pub fn hash_object(&self, bytes: &[u8]) -> Result<String, BoardError> {
        self.git_stdin(&["hash-object", "-w", "--stdin"], bytes)
    }

    /// Resolve a per-worktree private path (untracked, dies with the
    /// worktree). Always absolute: `--path-format=absolute` forces this even
    /// in a plain (non-worktree) repository, where `--git-path` alone would
    /// print a path relative to the repo root. Callers that pass this to
    /// `std::fs` directly need the absolute form — a relative one resolves
    /// against the *process* cwd, not the repo root, and silently no-ops on
    /// cleanup.
    pub fn git_path(&self, name: &str) -> Result<PathBuf, BoardError> {
        Ok(PathBuf::from(self.git(&[
            "rev-parse",
            "--path-format=absolute",
            "--git-path",
            name,
        ])?))
    }
}

/// Hermetic test scaffolding: a throwaway repository per test.
#[cfg(test)]
pub mod test_support {
    use super::Repo;
    use std::path::PathBuf;
    use std::sync::atomic::{AtomicU32, Ordering};

    static N: AtomicU32 = AtomicU32::new(0);

    /// A fresh `git init` repo in a temp dir, with identity configured so
    /// commit-tree works in CI. Returns the dir (kept alive by the caller) and
    /// a handle to it.
    pub fn temp_repo() -> (PathBuf, Repo) {
        let n = N.fetch_add(1, Ordering::SeqCst);
        let dir = std::env::temp_dir().join(format!("hv-board-test-{}-{}", std::process::id(), n));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).expect("temp dir");
        let repo = Repo::new(&dir);
        repo.git(&["init", "--quiet", "--initial-branch=main"])
            .expect("git init");
        repo.git(&["config", "user.email", "board@test"])
            .expect("email");
        repo.git(&["config", "user.name", "board test"])
            .expect("name");
        (dir, repo)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hash_object_is_content_addressed_and_writes_the_object() {
        let (_dir, repo) = test_support::temp_repo();
        let a = repo.hash_object(b"hello\n").expect("hash");
        let b = repo.hash_object(b"hello\n").expect("hash again");
        assert_eq!(a, b, "same bytes must hash to the same object id");
        let back = repo.git(&["cat-file", "-p", &a]).expect("cat-file");
        assert_eq!(back, "hello", "the object must be readable back");
    }

    #[test]
    fn rev_parse_verify_reports_absence_as_none_not_an_error() {
        let (_dir, repo) = test_support::temp_repo();
        assert_eq!(
            repo.rev_parse_verify("refs/hornvale/board")
                .expect("verify"),
            None
        );
    }

    #[test]
    fn a_failing_git_command_carries_the_physical_reason() {
        let (_dir, repo) = test_support::temp_repo();
        let err = repo
            .git(&["cat-file", "-p", "0000000000000000000000000000000000000000"])
            .expect_err("must fail");
        let msg = format!("{err}");
        assert!(msg.contains("cat-file"), "error names the command: {msg}");
        assert!(!msg.is_empty());
    }
}
