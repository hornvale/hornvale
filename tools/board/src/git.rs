//! The only place this crate invokes `git`. Every operation takes an explicit
//! repo root, and every invocation is built by [`Repo::command`], which is
//! what makes the tests hermetic.
//!
//! **`-C <root>` is not enough, and believing it was cost this project a
//! corrupted checkout.** `-C` sets git's working directory; it does *not*
//! scope which repository git acts on. `GIT_DIR` in the environment
//! outranks it entirely, and git *exports an absolute `GIT_DIR`* to every
//! hook it runs from a linked worktree:
//!
//! ```text
//! GIT_DIR=/…/hornvale/.git/worktrees/the-beacon
//! GIT_INDEX_FILE=/…/hornvale/.git/worktrees/the-beacon/index
//! ```
//!
//! `scripts/hooks/pre-commit` runs this crate's suite on a board-only commit
//! (B13, decision 0128), so every `git -C <tempdir>` in every test inherited
//! that and operated on the developer's real repository instead: `git init`
//! re-initialised it (and, because a worktree gitdir does not end in
//! `/.git`, guessed *bare* and set `core.bare = true`), `git config` rewrote
//! its identity to the test's, `git commit`/`merge` landed `board test`
//! commits on `main`, and `update-ref` left dangling refs that broke
//! `git fetch` repository-wide.
//!
//! Hence [`GIT_LOCATION_VARS`]: every invocation starts from a scrubbed
//! environment, so the repository is decided by `-C <root>` and nothing else.
//! `tools/board/tests/hermeticity.rs` is the guard.

use crate::BoardError;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Every environment variable that can redirect `git` at a repository, index,
/// or object store other than the one `-C <root>` names.
///
/// Scrubbed from every invocation this crate makes ([`Repo::command`]).
/// `GIT_DIR` is the one that did the damage, but each of these can move the
/// target: `GIT_WORK_TREE`/`GIT_COMMON_DIR` relocate the tree and the shared
/// dir, `GIT_INDEX_FILE` the staging area, `GIT_OBJECT_DIRECTORY` and
/// `GIT_ALTERNATE_OBJECT_DIRECTORIES` where objects are written and read,
/// `GIT_NAMESPACE` which refs are visible, and the two discovery knobs how
/// far git walks up looking for a repository.
pub const GIT_LOCATION_VARS: &[&str] = &[
    "GIT_DIR",
    "GIT_WORK_TREE",
    "GIT_COMMON_DIR",
    "GIT_INDEX_FILE",
    "GIT_OBJECT_DIRECTORY",
    "GIT_ALTERNATE_OBJECT_DIRECTORIES",
    "GIT_NAMESPACE",
    "GIT_CEILING_DIRECTORIES",
    "GIT_DISCOVERY_ACROSS_FILESYSTEM",
    "GIT_PREFIX",
];

/// Every environment variable that can decide *who authored* a commit this
/// crate writes.
///
/// Also scrubbed, for a smaller reason than [`GIT_LOCATION_VARS`]: these
/// cannot corrupt another repository, but git exports `GIT_AUTHOR_NAME`,
/// `GIT_AUTHOR_EMAIL`, and `GIT_AUTHOR_DATE` to a hook, so a `board post`
/// made from inside one would silently be stamped with the *outer* commit's
/// author and timestamp rather than the identity `git config` names. Scrubbing
/// them makes a board commit read the same whether or not a hook is in the
/// call stack.
pub const GIT_IDENTITY_VARS: &[&str] = &[
    "GIT_AUTHOR_NAME",
    "GIT_AUTHOR_EMAIL",
    "GIT_AUTHOR_DATE",
    "GIT_COMMITTER_NAME",
    "GIT_COMMITTER_EMAIL",
    "GIT_COMMITTER_DATE",
];

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
    ///
    /// The `.env` here lands *after* [`command`](Self::command)'s scrub, which
    /// removes `GIT_INDEX_FILE` along with the rest of
    /// [`GIT_LOCATION_VARS`] — so this sets the throwaway index deliberately
    /// on top of a clean environment rather than inheriting a hook's.
    pub fn git_with_index(&self, index: &Path, args: &[&str]) -> Result<String, BoardError> {
        let out = self
            .command()
            .env("GIT_INDEX_FILE", index)
            .args(args)
            .output()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
        Self::finish(args, out)
    }

    /// The `git -C <root>` invocation common to every command this crate runs,
    /// built from an environment scrubbed of everything that could point git
    /// somewhere else.
    ///
    /// This is the **only** `Command::new("git")` in the crate, and
    /// `tools/board/tests/hermeticity.rs` asserts that it stays the only one —
    /// a second spawn site would be a second chance to inherit a hook's
    /// `GIT_DIR`. See the module docs for what that cost once.
    fn command(&self) -> Command {
        let mut cmd = Command::new("git");
        for var in GIT_LOCATION_VARS.iter().chain(GIT_IDENTITY_VARS) {
            cmd.env_remove(var);
        }
        cmd.arg("-C").arg(&self.root);
        cmd
    }

    /// Run git and hand back the raw [`Output`](std::process::Output),
    /// **without** treating a non-zero exit as an error.
    ///
    /// [`git`](Self::git) discards stdout on failure, which is exactly where
    /// git puts diagnostics like `CONFLICT (content)`. A caller that needs to
    /// assert *why* a command failed needs those bytes. Only a failure to
    /// spawn is an error here.
    pub fn git_output(&self, args: &[&str]) -> Result<std::process::Output, BoardError> {
        self.command()
            .args(args)
            .output()
            .map_err(|e| BoardError::Io(format!("spawning git: {e}")))
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
        let out = self.git_stdin_bytes(args, input)?;
        Ok(String::from_utf8_lossy(&out).trim_end().to_string())
    }

    /// Run `git` with `input` on stdin, returning stdout as **bytes**.
    ///
    /// [`git_stdin`](Self::git_stdin) decodes lossily and trims trailing
    /// whitespace, both of which corrupt a size-framed stream: lossy decoding
    /// changes byte lengths (U+FFFD is three bytes) and trimming eats the last
    /// record's terminator. `cat-file --batch` is size-framed, so it needs
    /// this.
    pub fn git_stdin_bytes(&self, args: &[&str], input: &[u8]) -> Result<Vec<u8>, BoardError> {
        use std::io::Write;
        use std::process::Stdio;
        // Through `command()`, not a second `Command::new("git")`: this path
        // must inherit the same environment scrub as every other.
        let mut child = self
            .command()
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
        Ok(out.stdout)
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

    /// Resolve a path under the repository's **common** dir — shared by
    /// every worktree, unlike [`git_path`](Self::git_path)'s per-worktree
    /// answer. A `git fetch` serves every worktree at once (there is one
    /// remote-tracking state, not one per worktree), so anything that
    /// records "when did we last sync" belongs here: verified empirically
    /// that from inside a linked worktree, `--git-path` returns
    /// `/…/.git/worktrees/<name>` while `--git-common-dir` returns the
    /// shared `/…/.git` — recording sync times under the former would report
    /// nine different ages for one fetch, one per worktree.
    ///
    /// Always absolute, for the same reason `git_path` is: a relative path
    /// resolves against the *process* cwd on the `std::fs` calls a caller
    /// makes with it, not the repo root.
    pub fn git_common_path(&self, name: &str) -> Result<PathBuf, BoardError> {
        Ok(
            PathBuf::from(self.git(&[
                "rev-parse",
                "--path-format=absolute",
                "--git-common-dir",
            ])?)
            .join(name),
        )
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
