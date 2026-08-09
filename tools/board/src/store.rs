//! The board's write and read-tip paths.
//!
//! D11 — one immutable, content-addressed file per post; every operation is an
//! append. D13 — no code path here can reroot the ref.

use crate::BoardError;
use crate::git::Repo;
use crate::post::Post;
use std::sync::atomic::{AtomicU64, Ordering};

/// The board's ref. Deliberately outside `refs/heads/`, so it is invisible to
/// branch listings and never participates in a merge with `main`.
pub const BOARD_REF: &str = "refs/hornvale/board";

/// How many times a contended write retries before failing loudly.
const MAX_ATTEMPTS: u32 = 24;

/// Discriminates concurrent throwaway-index paths (and, mixed into the retry
/// jitter, concurrent backoffs) within one process.
///
/// The process id alone is not enough for either job: `Board::append` is safe
/// to call from multiple threads of the same process (the concurrency
/// property this crate exists to hold), and a pid is shared by every thread
/// in that process. Two threads naming the same `GIT_INDEX_FILE` race on
/// git's own `.lock` file (a hard failure, not a graceful compare-and-swap
/// loss); two threads computing the same jitter retry in lockstep instead of
/// spreading out. Mixing a per-call counter into both keeps them unique
/// across threads within one process, on top of the pid keeping them unique
/// across processes.
static CALL_DISCRIMINANT: AtomicU64 = AtomicU64::new(0);

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
        self.append_with_attempts(post, MAX_ATTEMPTS)
    }

    /// `append`'s retry loop, with the attempt budget as a parameter so a
    /// test can drive the exhaustion arm deterministically without waiting on
    /// real contention. Private: this is not a wider public surface, just a
    /// seam `append` and its tests both go through.
    fn append_with_attempts(&self, post: &Post, max_attempts: u32) -> Result<String, BoardError> {
        let bytes = post.canonical_bytes()?;
        let id = self.repo.hash_object(&bytes)?;
        let path = format!("posts/{id}.json");
        let mut last_reason: Option<String> = None;

        for attempt in 0..max_attempts {
            let call_id = CALL_DISCRIMINANT.fetch_add(1, Ordering::SeqCst);
            let old = self.tip()?;
            let tree = self.tree_with(call_id, old.as_deref(), &id, &path)?;

            if let Some(parent) = &old {
                let base_tree =
                    self.repo
                        .git(&["rev-parse", "--verify", &format!("{parent}^{{tree}}")])?;
                if tree == base_tree {
                    // Content addressing means the only way this tree can come
                    // back byte-identical to the parent's is that `path`
                    // already held exactly `blob` — this exact post is already
                    // recorded. D11's idempotence should hold at the commit
                    // level too, not only the file level: nothing to commit.
                    return Ok(id);
                }
            }

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

            match self.cas(&new, old.as_deref())? {
                None => return Ok(id),
                Some(stderr) => last_reason = Some(stderr),
            }
            // Contended: another writer moved the ref. Back off with jitter
            // that mixes the pid (decorrelates processes) with `call_id`
            // (decorrelates threads within one process — a pid alone is
            // identical for every thread, so it cannot spread them out). No
            // `rand` dependency by design.
            let jitter = 3 + ((u64::from(std::process::id()) ^ call_id) % 11);
            std::thread::sleep(std::time::Duration::from_millis(
                jitter * u64::from(attempt + 1),
            ));
        }
        Err(BoardError::Git {
            cmd: format!("append to {}", self.refname),
            code: None,
            stderr: format!(
                "lost {max_attempts} compare-and-swap races; the ref is under sustained \
                 contention and this post was NOT recorded. Last reason: {}",
                last_reason.as_deref().unwrap_or("unknown")
            ),
        })
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
    /// working recipe is a throwaway index, named with `call_id` so concurrent
    /// callers in the same process (which share a pid) never share a path.
    fn tree_with(
        &self,
        call_id: u64,
        base: Option<&str>,
        blob: &str,
        path: &str,
    ) -> Result<String, BoardError> {
        let raw = self
            .repo
            .git_path(&format!("hv-board-index-{}-{call_id}", std::process::id()))?;
        // `Repo::git_path` returns an absolute path. Guard defensively
        // anyway: a relative path here would resolve against the *process*
        // cwd on the `std::fs` calls below, not the repo root, which is
        // exactly the leak this function used to have.
        let index = if raw.is_absolute() {
            raw
        } else {
            self.repo.root().join(raw)
        };

        // Pre-clean: a leaked index from an earlier crashed or interrupted
        // run must not contaminate this write (stale entries would survive
        // into `write-tree` on the `base == None` path, which has no
        // `read-tree` to overwrite them). Anything other than "already
        // absent" is a real problem worth surfacing, not swallowing — that
        // silence is what let the leak go unnoticed.
        match std::fs::remove_file(&index) {
            Ok(()) => {}
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(BoardError::Io(format!(
                    "removing stale throwaway index {}: {e}",
                    index.display()
                )));
            }
        }

        let result = (|| -> Result<String, BoardError> {
            if let Some(base) = base {
                self.repo.git_with_index(&index, &["read-tree", base])?;
            }
            self.repo.git_with_index(
                &index,
                &[
                    "update-index",
                    "--add",
                    "--cacheinfo",
                    &format!("100644,{blob},{path}"),
                ],
            )?;
            self.repo.git_with_index(&index, &["write-tree"])
        })();

        // Post-clean on every exit path, success or failure — not only after
        // success, which is what let this leak unboundedly. Best-effort: by
        // this point the write has already happened or definitely failed, so
        // a cleanup problem here is clutter, not corruption.
        let _ = std::fs::remove_file(&index);
        result
    }

    /// Compare-and-swap the ref.
    ///
    /// `Ok(None)` means we won. `Ok(Some(stderr))` means we lost a *genuine*
    /// race — classified semantically, not by matching git's stderr text:
    /// after `update-ref` fails, re-read the tip. If it no longer equals
    /// `old`, something really did move the ref out from under us, so this
    /// is expected control flow and the caller should retry; git's stderr is
    /// returned so a caller that exhausts its attempts can report the last
    /// real reason. If the tip is still `old` (or we cannot even read it),
    /// nothing moved — the failure is permanent (a name collision, a
    /// permissions problem, …) and must not be retried away, so it comes back
    /// as `Err` with git's own diagnosis intact.
    fn cas(&self, new: &str, old: Option<&str>) -> Result<Option<String>, BoardError> {
        let result = match old {
            Some(old) => self.repo.git(&["update-ref", &self.refname, new, old]),
            None => self.repo.git_stdin(
                &["update-ref", "--stdin"],
                format!("create {} {new}\n", self.refname).as_bytes(),
            ),
        };
        let err = match result {
            Ok(_) => return Ok(None),
            Err(e) => e,
        };
        let moved = matches!(self.tip(), Ok(current) if current.as_deref() != old);
        if moved {
            let stderr = match &err {
                BoardError::Git { stderr, .. } => stderr.clone(),
                other => other.to_string(),
            };
            Ok(Some(stderr))
        } else {
            Err(err)
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
    fn a_duplicate_append_does_not_grow_the_commit_chain() {
        // Q7: idempotence should hold at the commit level too, not just the
        // file level — a re-record of an already-present post must not add
        // an empty commit on top.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let p = Post::new("claim", "campaign/x");
        board.append(&p).expect("first");
        let tip_after_first = board.tip().expect("tip").expect("some");
        board.append(&p).expect("second");
        let tip_after_second = board.tip().expect("tip").expect("some");
        assert_eq!(
            tip_after_first, tip_after_second,
            "a duplicate append must not move the tip at all"
        );
        let chain_len = repo
            .git(&["rev-list", "--count", &tip_after_second])
            .expect("rev-list");
        assert_eq!(chain_len, "1", "still exactly one commit, not two");
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

    #[test]
    fn cas_reports_a_lost_race_when_the_ref_moved_away_from_the_expected_old_value() {
        // Q1/I1: a deterministic stand-in for a real race. `update-ref`'s
        // expected-value check fails exactly the same way whether the ref
        // moved because of a concurrent writer or, as here, because we hand
        // it a stale `old` on purpose — and `cas` must classify both as a
        // lost race, not a permanent failure.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board.append(&Post::new("claim", "a")).expect("a");
        let a_tip = board.tip().expect("tip").expect("some");
        board.append(&Post::new("claim", "b")).expect("b");
        let b_tip = board.tip().expect("tip").expect("some");

        let lost = board
            .cas(&b_tip, Some(&a_tip))
            .expect("cas must not error on a genuine race");
        assert!(
            lost.is_some(),
            "the ref moved out from under `old`, so this must be reported as a lost race"
        );
        assert_eq!(
            board.tip().expect("tip").expect("some"),
            b_tip,
            "a lost race must not have moved the ref again"
        );
    }

    #[test]
    fn cas_reports_a_real_error_when_nothing_moved() {
        // Q1/I1: the reviewer's D/F-conflict setup. A ref already exists
        // *under* the board's own ref path, so creating a leaf ref there
        // fails every single time — permanently, not by chance — and the
        // board's own ref never budges. `cas` must not classify this as a
        // lost race.
        let (_d, repo) = temp_repo();
        let scratch = Board::with_ref(repo.clone(), "refs/test/scratch");
        scratch
            .append(&Post::new("claim", "x"))
            .expect("scratch append");
        let commit = scratch.tip().expect("tip").expect("some");
        repo.git(&["update-ref", &format!("{BOARD_REF}/child"), &commit])
            .expect("child ref");

        let board = Board::new(repo.clone());
        let err = board
            .cas("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef", None)
            .expect_err("nothing moved, so this must propagate as a real error, not a lost race");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains(BOARD_REF),
            "should carry git's own diagnosis: {stderr}"
        );
    }

    #[test]
    fn append_propagates_a_permanent_failure_without_exhausting_attempts() {
        // The user-facing half of the previous test: `append` itself must
        // surface a permanent D/F conflict immediately, not spin through its
        // attempt budget treating it as contention. Checked by content, not
        // by timing (a wall-clock threshold here is at the mercy of however
        // busy the machine running the suite happens to be): give it a
        // budget of exactly 1. If the conflict were misclassified as a lost
        // race, that single attempt would be consumed and the failure would
        // come back as the generic "lost N compare-and-swap races"
        // exhaustion message instead of git's own diagnosis -- so the two
        // failure modes are textually distinguishable regardless of speed.
        let (_d, repo) = temp_repo();
        let scratch = Board::with_ref(repo.clone(), "refs/test/scratch");
        scratch
            .append(&Post::new("claim", "x"))
            .expect("scratch append");
        let commit = scratch.tip().expect("tip").expect("some");
        repo.git(&["update-ref", &format!("{BOARD_REF}/child"), &commit])
            .expect("child ref");

        let board = Board::new(repo.clone());
        let err = board
            .append_with_attempts(&Post::new("claim", "y"), 1)
            .expect_err("a permanent D/F conflict must not be retried away as a race");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            !stderr.contains("compare-and-swap"),
            "a permanent failure must propagate git's own diagnosis, not the generic exhaustion message: {stderr}"
        );
        assert!(
            stderr.contains(BOARD_REF),
            "the propagated error should carry git's own diagnosis: {stderr}"
        );
    }

    #[test]
    fn exhaustion_is_loud_and_names_the_ref_and_the_post() {
        // I3: make the exhaustion arm reachable without waiting on chance.
        // Attackers use raw plumbing (reuse one tree, `commit-tree` +
        // `update-ref`; no `tree_with`) so each of their iterations is
        // several times cheaper than the victim's real `append_with_attempts`
        // attempt (which builds a throwaway index, diffs the tree, and
        // commits). That speed asymmetry, not luck, is what makes a 2-attempt
        // budget against a dozen of them exhaust reliably -- verified with
        // repeated reruns (see the report).
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("claim", "genesis"))
            .expect("genesis");
        let genesis_tip = board.tip().expect("tip").expect("some");
        let tree = repo
            .git(&["rev-parse", "--verify", &format!("{genesis_tip}^{{tree}}")])
            .expect("tree");

        let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let attackers: Vec<_> = (0..12)
            .map(|_| {
                let attacker_repo = repo.clone();
                let attacker_stop = stop.clone();
                let attacker_tree = tree.clone();
                std::thread::spawn(move || {
                    while !attacker_stop.load(Ordering::Relaxed) {
                        let Ok(Some(old)) = attacker_repo.rev_parse_verify(BOARD_REF) else {
                            continue;
                        };
                        let Ok(new) = attacker_repo.git(&[
                            "commit-tree",
                            &attacker_tree,
                            "-p",
                            &old,
                            "-m",
                            "attack",
                        ]) else {
                            continue;
                        };
                        let _ = attacker_repo.git(&["update-ref", BOARD_REF, &new, &old]);
                    }
                })
            })
            .collect();

        let result = board.append_with_attempts(&Post::new("claim", "victim"), 2);
        stop.store(true, Ordering::Relaxed);
        for attacker in attackers {
            attacker.join().expect("attacker thread");
        }

        let err = result
            .expect_err("a tiny attempt budget against sustained contention must fail loudly");
        let BoardError::Git { stderr, .. } = err else {
            panic!("expected BoardError::Git, got a different variant");
        };
        assert!(
            stderr.contains(&board.refname),
            "the error should name the ref: {stderr}"
        );
        assert!(
            stderr.contains("NOT recorded"),
            "the error should say the post was not recorded: {stderr}"
        );
    }
}
