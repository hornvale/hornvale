//! Routing by topic, and per-worktree read state. Both are read-time concerns
//! (D10): nothing here is stored on the board.

use crate::BoardError;
use crate::git::Repo;
use crate::post::Post;
use crate::store::Board;
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The reading worktree's own changed files, relative to `main`.
pub fn changed_paths(repo: &Repo) -> Result<Vec<String>, BoardError> {
    // Qualified as `refs/heads/main...HEAD`, not the bare `main...HEAD`: a
    // same-named tag would otherwise win git's ref-disambiguation order
    // (`refs/<name>`, then `refs/tags/<name>`, then `refs/heads/<name>`) and
    // silently change what this range means (the same hazard `live.rs`'s
    // `resolve_branch_ref` closes for `by`). `unwrap_or_default()` is
    // deliberate, not sloppy: a fresh branch sitting at `main`'s tip
    // legitimately has no changed paths, and a reader with no changed paths
    // should still receive broadcasts.
    let out = repo
        .git(&["diff", "--name-only", "refs/heads/main...HEAD"])
        .unwrap_or_default();
    Ok(out.lines().map(str::to_string).collect())
}

/// Does this post concern work the reader is doing?
///
/// A post naming no paths is a broadcast and reaches everyone; anything else
/// matches when one of its paths prefixes one of the reader's changed files.
pub fn is_relevant(post: &Post, changed: &[String]) -> bool {
    let paths = post.paths();
    if paths.is_empty() {
        return true;
    }
    paths
        .iter()
        .any(|p| changed.iter().any(|c| c.starts_with(p.as_str())))
}

/// A worktree's private record of the last board tip it rendered.
#[derive(Debug, Clone)]
pub struct Cursor {
    path: PathBuf,
}

impl Cursor {
    /// Resolve this worktree's cursor file. Untracked, private, and it dies
    /// with the worktree — so there is no shared read state to contend on.
    pub fn open(repo: &Repo) -> Result<Self, BoardError> {
        Ok(Self {
            path: repo.git_path("hv-board-cursor")?,
        })
    }

    /// Where the cursor lives.
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// The last board tip this worktree rendered, if any.
    pub fn last_seen(&self) -> Option<String> {
        std::fs::read_to_string(&self.path)
            .ok()
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty())
    }

    /// Record a tip as seen.
    pub fn record(&self, tip: &str) -> Result<(), BoardError> {
        std::fs::write(&self.path, format!("{tip}\n"))
            .map_err(|e| BoardError::Io(format!("writing cursor {:?}: {e}", self.path)))
    }
}

/// Post ids added to the board since the cursor was last recorded.
///
/// **An empty return means genuinely nothing is new — never "could not
/// tell".** That distinction is the whole point: the caller filters a render
/// down to exactly this set and then advances the cursor past it, so an
/// empty set that actually meant "the read failed" would render nothing and
/// then mark every pending post seen, permanently. Three cases, kept
/// distinct on purpose:
///
/// 1. The board has no tip yet — legitimately nothing unseen: `Ok(empty)`.
/// 2. The cursor names a commit git cannot resolve (a plausible if rare
///    corruption). Treated as "this worktree has seen nothing" and reported
///    against the no-cursor fallback range, with a one-line stderr warning —
///    fail OPEN, because re-rendering a post is cosmetic and hiding one
///    forever is the harm this board exists to prevent.
/// 3. Any other git failure propagates as `Err`, so the caller can decline to
///    advance the cursor rather than have the failure silently reported as
///    "nothing new".
///
/// **Caller contract, for whoever wires this to a render:** advance the
/// cursor based on the ids returned *here*, not based on which of them
/// [`is_relevant`] chose to display. Filtering an id out for relevance is a
/// per-render, per-reader display decision with no persistent memory of its
/// own; treating a relevance-filtered id as "seen" would let a transient
/// [`changed_paths`] failure (which makes every path-scoped post look
/// irrelevant for that one pass) combine with cursor advancement to
/// reproduce this exact hazard one layer up, permanently hiding a post this
/// worktree never actually saw.
pub fn unseen(board: &Board, cursor: &Cursor) -> Result<BTreeSet<String>, BoardError> {
    let Some(tip) = board.tip()? else {
        return Ok(BTreeSet::new());
    };
    let range = match cursor.last_seen() {
        // `^{commit}` forces git to confirm the object actually exists and is
        // a commit, not merely that the string looks like one: a bare full
        // hex sha passes `rev-parse --verify` as syntactically valid even
        // when no such object is in the store, which would defeat this check
        // entirely.
        Some(seen)
            if board
                .repo()
                .rev_parse_verify(&format!("{seen}^{{commit}}"))?
                .is_some() =>
        {
            format!("{seen}..{tip}")
        }
        Some(seen) => {
            eprintln!(
                "board: cursor at {:?} names commit {seen}, which git cannot resolve; \
                 ignoring it and treating this worktree as having seen nothing, rather than \
                 risk reporting no unseen posts",
                cursor.path()
            );
            tip.clone()
        }
        None => tip.clone(),
    };
    let log = board
        .repo()
        .git(&["log", "--format=", "--diff-filter=A", "--name-only", &range])?;
    Ok(log
        .lines()
        .filter_map(|l| l.strip_prefix("posts/"))
        .filter_map(|l| l.strip_suffix(".json"))
        .map(str::to_string)
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::test_support::temp_repo;
    use crate::post::Post;
    use crate::store::Board;
    use serde_json::json;

    #[test]
    fn a_notice_with_no_paths_is_relevant_to_everyone() {
        // A broadcast is a legitimate post; absence of `paths` must not mean
        // "relevant to nobody", which would silently swallow it.
        assert!(is_relevant(
            &Post::new("notice", "b"),
            &["kernel/src/lib.rs".to_string()]
        ));
    }

    #[test]
    fn a_notice_is_relevant_when_its_path_prefixes_a_changed_file() {
        let p = Post::new("notice", "b").with("paths", json!(["domains/terrain/"]));
        assert!(is_relevant(
            &p,
            &["domains/terrain/src/carve.rs".to_string()]
        ));
    }

    #[test]
    fn a_notice_whose_paths_miss_everything_is_not_relevant() {
        let p = Post::new("notice", "b").with("paths", json!(["domains/terrain/"]));
        assert!(!is_relevant(&p, &["kernel/src/seed.rs".to_string()]));
    }

    #[test]
    fn the_cursor_lives_in_per_worktree_private_state() {
        let (_d, repo) = temp_repo();
        let cursor = Cursor::open(&repo).expect("cursor");
        assert!(
            cursor.path().to_string_lossy().contains(".git"),
            "the cursor must live under .git so it dies with the worktree: {:?}",
            cursor.path()
        );
        assert_eq!(
            cursor.last_seen(),
            None,
            "a fresh worktree has seen nothing"
        );
    }

    #[test]
    fn unseen_reports_only_posts_added_since_the_cursor_was_recorded() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cursor");
        let cursor = Cursor::open(&repo).expect("cursor");

        let first = board.append(&Post::new("notice", "b")).expect("first");
        assert!(unseen(&board, &cursor).expect("unseen").contains(&first));

        cursor
            .record(&board.tip().expect("tip").expect("some"))
            .expect("record");
        assert!(
            unseen(&board, &cursor).expect("unseen").is_empty(),
            "nothing new after recording"
        );

        let second = board.append(&Post::new("notice", "c")).expect("second");
        let now = unseen(&board, &cursor).expect("unseen");
        assert!(now.contains(&second), "the new post is unseen");
        assert!(!now.contains(&first), "the old post is not");
    }

    #[test]
    fn unseen_falls_back_to_everything_when_the_cursor_names_an_unresolvable_commit() {
        // A syntactically valid 40-hex-char sha that no object in the repo
        // matches. This must fail OPEN: a repeat render is cosmetic, a
        // silently swallowed post is the harm this board exists to prevent.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cursor-bad-sha");
        let cursor = Cursor::open(&repo).expect("cursor");

        let id = board.append(&Post::new("notice", "b")).expect("append");
        cursor
            .record(&"0".repeat(40))
            .expect("record an unresolvable cursor");

        let ids = unseen(&board, &cursor).expect("unseen");
        assert!(
            ids.contains(&id),
            "an unresolvable cursor must not silently hide a pending post: {ids:?}"
        );
    }

    #[test]
    fn unseen_propagates_a_genuine_git_failure_instead_of_reporting_empty() {
        // Corrupts the board's own tree object (deletes its loose object
        // file) so `git log` fails for a reason that has nothing to do with
        // the cursor. This must surface as `Err`, not silently collapse to
        // an empty set -- an empty set here would look identical to "nothing
        // new" to the caller, which then advances the cursor and loses
        // whatever was actually pending.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cursor-corrupt");
        let cursor = Cursor::open(&repo).expect("cursor");

        board.append(&Post::new("notice", "b")).expect("append");
        let tip = board.tip().expect("tip").expect("some");
        let tree = repo
            .git(&["rev-parse", "--verify", &format!("{tip}^{{tree}}")])
            .expect("tree");
        let object_path = repo
            .root()
            .join(".git/objects")
            .join(&tree[0..2])
            .join(&tree[2..]);
        std::fs::remove_file(&object_path).expect("corrupt the tree object");

        let err = unseen(&board, &cursor)
            .expect_err("a genuine git failure must surface as Err, not an empty set");
        assert!(
            matches!(err, BoardError::Git { .. }),
            "expected a git error, got {err:?}"
        );
    }
}
