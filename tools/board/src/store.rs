//! The board's write and read-tip paths.
//!
//! D11 — one immutable, content-addressed file per post; every operation is an
//! append. D13 — no code path here can reroot the ref.

use crate::BoardError;
use crate::git::Repo;
use crate::post::Post;

/// The board's ref. Deliberately outside `refs/heads/`, so it is invisible to
/// branch listings and never participates in a merge with `main`.
pub const BOARD_REF: &str = "refs/hornvale/board";

/// How many times a contended write retries before failing loudly.
const MAX_ATTEMPTS: u32 = 24;

/// Discriminates concurrent throwaway-index paths within one process.
///
/// The process id alone is not enough: `Board::append` is safe to call from
/// multiple threads of the same process (the concurrency property this crate
/// exists to hold), and several threads sharing one `GIT_INDEX_FILE` path
/// race on git's own `.lock` file, which fails hard rather than losing a
/// graceful compare-and-swap. Mixing the pid with a per-call counter keeps
/// paths unique both across processes and across threads within one.
static INDEX_DISCRIMINANT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

/// One board, on one ref, in one repository.
#[derive(Debug, Clone)]
pub struct Board {
    repo: Repo,
    refname: String,
}

impl Board {
    /// The board on the canonical ref.
    pub fn new(repo: Repo) -> Self {
        Self::with_ref(repo, BOARD_REF)
    }

    /// The board on an arbitrary ref — used by tests to simulate clones.
    pub fn with_ref(repo: Repo, refname: &str) -> Self {
        Self {
            repo,
            refname: refname.to_string(),
        }
    }

    /// The repository this board lives in.
    pub fn repo(&self) -> &Repo {
        &self.repo
    }

    /// This board's ref name.
    pub fn refname(&self) -> &str {
        &self.refname
    }

    /// The current tip commit, or `None` if the board does not exist yet.
    pub fn tip(&self) -> Result<Option<String>, BoardError> {
        self.repo.rev_parse_verify(&self.refname)
    }

    /// Append a post. Returns its id — the object id of its own bytes, which is
    /// also its filename, so appending identical content twice is idempotent.
    pub fn append(&self, post: &Post) -> Result<String, BoardError> {
        let bytes = post.canonical_bytes()?;
        let id = self.repo.hash_object(&bytes)?;
        let path = format!("posts/{id}.json");

        for attempt in 0..MAX_ATTEMPTS {
            let old = self.tip()?;
            let tree = self.tree_with(old.as_deref(), &id, &path)?;
            let mut args: Vec<String> = vec![
                "commit-tree".into(),
                tree.clone(),
                "-m".into(),
                format!("board: {} by {}", post.kind, post.by),
            ];
            if let Some(parent) = &old {
                args.push("-p".into());
                args.push(parent.clone());
            }
            let refs: Vec<&str> = args.iter().map(String::as_str).collect();
            let new = self.repo.git(&refs)?;

            if self.cas(&new, old.as_deref())? {
                return Ok(id);
            }
            // Contended: another writer moved the ref. Back off with a jitter
            // derived from our pid, so N concurrent writers do not retry in
            // lockstep. No `rand` dependency by design.
            let jitter = 3 + (std::process::id() % 11) as u64;
            std::thread::sleep(std::time::Duration::from_millis(
                jitter * u64::from(attempt + 1),
            ));
        }
        Err(BoardError::Io(format!(
            "board write to {} lost {MAX_ATTEMPTS} compare-and-swap races; the ref is under \
             sustained contention and this post was NOT recorded",
            self.refname
        )))
    }

    /// Post ids present in the tip tree, sorted.
    pub fn post_ids_at_tip(&self) -> Result<Vec<String>, BoardError> {
        let Some(tip) = self.tip()? else {
            return Ok(Vec::new());
        };
        let listed = self.repo.git(&["ls-tree", "-r", "--name-only", &tip])?;
        let mut ids: Vec<String> = listed
            .lines()
            .filter_map(|l| l.strip_prefix("posts/"))
            .filter_map(|l| l.strip_suffix(".json"))
            .map(str::to_string)
            .collect();
        ids.sort();
        Ok(ids)
    }

    /// Build a tree equal to `base`'s tree plus one post file.
    ///
    /// `git mktree` cannot do this: it rejects any path containing a slash. The
    /// working recipe is a throwaway index.
    fn tree_with(&self, base: Option<&str>, blob: &str, path: &str) -> Result<String, BoardError> {
        let discriminant = INDEX_DISCRIMINANT.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        let index = self.repo.git_path(&format!(
            "hv-board-index-{}-{discriminant}",
            std::process::id()
        ))?;
        let _ = std::fs::remove_file(&index);
        let index_str = index.to_string_lossy().to_string();

        let run = |args: &[&str]| -> Result<String, BoardError> {
            use std::process::Command;
            let out = Command::new("git")
                .arg("-C")
                .arg(self.repo.root())
                .env("GIT_INDEX_FILE", &index_str)
                .args(args)
                .output()
                .map_err(|e| BoardError::Io(format!("spawning git: {e}")))?;
            if !out.status.success() {
                return Err(BoardError::Git {
                    cmd: args.join(" "),
                    code: out.status.code(),
                    stderr: String::from_utf8_lossy(&out.stderr).trim().to_string(),
                });
            }
            Ok(String::from_utf8_lossy(&out.stdout).trim_end().to_string())
        };

        if let Some(base) = base {
            run(&["read-tree", base])?;
        }
        run(&[
            "update-index",
            "--add",
            "--cacheinfo",
            &format!("100644,{blob},{path}"),
        ])?;
        let tree = run(&["write-tree"])?;
        let _ = std::fs::remove_file(&index);
        Ok(tree)
    }

    /// Compare-and-swap the ref. `Ok(false)` means we lost the race.
    fn cas(&self, new: &str, old: Option<&str>) -> Result<bool, BoardError> {
        let result = match old {
            Some(old) => self.repo.git(&["update-ref", &self.refname, new, old]),
            None => self.repo.git_stdin(
                &["update-ref", "--stdin"],
                format!("create {} {new}\n", self.refname).as_bytes(),
            ),
        };
        match result {
            Ok(_) => Ok(true),
            // A lost race is expected control flow, not a failure. Anything
            // else is real.
            Err(BoardError::Git { ref stderr, .. })
                if stderr.contains("cannot lock ref") || stderr.contains("already exists") =>
            {
                Ok(false)
            }
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::test_support::temp_repo;

    #[test]
    fn append_creates_the_ref_and_touches_no_working_tree() {
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        assert_eq!(
            board.tip().expect("tip"),
            None,
            "no ref before the first post"
        );
        let id = board
            .append(&Post::new("claim", "campaign/x"))
            .expect("append");
        assert!(board.tip().expect("tip").is_some(), "ref exists after");
        assert_eq!(board.post_ids_at_tip().expect("ids"), vec![id]);
        let dirty = repo.git(&["status", "--porcelain"]).expect("status");
        assert!(
            dirty.is_empty(),
            "a board write must not dirty the checkout: {dirty:?}"
        );
    }

    #[test]
    fn the_same_post_appended_twice_is_one_file() {
        // D11: content addressing makes a double-record idempotent.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo);
        let p = Post::new("claim", "campaign/x");
        let a = board.append(&p).expect("first");
        let b = board.append(&p).expect("second");
        assert_eq!(a, b, "same content, same id");
        assert_eq!(board.post_ids_at_tip().expect("ids").len(), 1);
    }

    #[test]
    fn appends_accumulate_and_never_reroot() {
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let first_tip = {
            board.append(&Post::new("claim", "a")).expect("a");
            board.tip().expect("tip").expect("some")
        };
        board.append(&Post::new("claim", "b")).expect("b");
        let tip = board.tip().expect("tip").expect("some");
        assert_eq!(board.post_ids_at_tip().expect("ids").len(), 2);
        let ancestor = repo.git(&["merge-base", "--is-ancestor", &first_tip, &tip]);
        assert!(
            ancestor.is_ok(),
            "D13: the tip must descend from every earlier tip"
        );
    }
}
