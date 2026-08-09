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

/// A worktree's private record of which post ids it has actually been shown.
///
/// Deliberately **not** a board tip. A tip cannot express "which posts have I
/// actually been shown" — those differ exactly when a render's relevance
/// filter drops a post that was present at that tip. A `hold-off` notice
/// naming `domains/terrain/`, posted while this worktree is editing
/// `kernel/`, is unseen but irrelevant, so it does not render; if the cursor
/// held a tip, recording that tip would mark the notice seen anyway, and it
/// would never render even after the worktree later starts touching
/// `domains/terrain/` — silently defeating the exact collision this board
/// exists to warn about, by construction rather than on any error path.
/// Holding the set of ids actually shown closes this: an id that was skipped
/// for relevance is never recorded, so it stays unseen until it is actually
/// displayed.
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

    /// Post ids this worktree has already been shown.
    ///
    /// A read failure (most commonly: the file does not exist yet, for a
    /// fresh worktree) reads as "nothing shown yet" — the safe direction,
    /// since it can only cause a repeat render, never a swallowed one.
    pub fn seen(&self) -> BTreeSet<String> {
        std::fs::read_to_string(&self.path)
            .map(|s| {
                s.lines()
                    .filter(|l| !l.is_empty())
                    .map(str::to_string)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Record `shown` as seen, unioned into whatever this worktree had
    /// already recorded.
    ///
    /// **Caller contract:** `shown` must be exactly the ids actually
    /// displayed to the reader — post-relevance-filtering, not the full
    /// [`unseen`] set. Recording an id that was unseen but filtered out for
    /// relevance is precisely the bug this representation exists to prevent:
    /// it would mark the id seen before it was ever shown, so it could never
    /// render later even after it became relevant.
    ///
    /// Also prunes: any previously-recorded id no longer present at
    /// `board`'s tip is dropped. A reaped post can never render again, so
    /// keeping its id here forever would grow this file without bound — it
    /// stays sized to the live post count instead.
    pub fn record(&self, board: &Board, shown: &BTreeSet<String>) -> Result<(), BoardError> {
        let live: BTreeSet<String> = board.post_ids_at_tip()?.into_iter().collect();
        let mut all = self.seen();
        all.extend(shown.iter().cloned());
        all.retain(|id| live.contains(id));
        let text: String = all.iter().map(|id| format!("{id}\n")).collect();
        std::fs::write(&self.path, text)
            .map_err(|e| BoardError::Io(format!("writing cursor {:?}: {e}", self.path)))
    }
}

/// Post ids at the board's tip that this worktree has not yet been shown.
///
/// A plain set difference against [`Cursor::seen`] — no git log, no commit
/// range, no cursor-resolution fallback: the representation change that
/// replaced a board tip with a set of shown ids retired that whole class of
/// problem structurally, not just the failure path. An empty return means
/// genuinely nothing new, never "could not tell": the only way to end up
/// with a failure here is [`Board::post_ids_at_tip`] itself failing, which
/// surfaces as `Err`, never as an empty set standing in for it.
pub fn unseen(board: &Board, cursor: &Cursor) -> Result<BTreeSet<String>, BoardError> {
    let at_tip: BTreeSet<String> = board.post_ids_at_tip()?.into_iter().collect();
    Ok(at_tip.difference(&cursor.seen()).cloned().collect())
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
        assert!(
            cursor.seen().is_empty(),
            "a fresh worktree has been shown nothing"
        );
    }

    #[test]
    fn unseen_reports_only_ids_not_yet_recorded_as_shown() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cursor");
        let cursor = Cursor::open(&repo).expect("cursor");

        let first = board.append(&Post::new("notice", "b")).expect("first");
        assert!(unseen(&board, &cursor).expect("unseen").contains(&first));

        cursor
            .record(&board, &BTreeSet::from([first.clone()]))
            .expect("record");
        assert!(
            unseen(&board, &cursor).expect("unseen").is_empty(),
            "nothing new after recording what was shown"
        );

        let second = board.append(&Post::new("notice", "c")).expect("second");
        let now = unseen(&board, &cursor).expect("unseen");
        assert!(now.contains(&second), "the new post is unseen");
        assert!(!now.contains(&first), "the old post is not");
    }

    #[test]
    fn recording_only_the_shown_ids_leaves_a_relevance_filtered_post_unseen() {
        // The regression test for the flaw this representation replaced: a
        // post that is unseen but dropped by the relevance filter must stay
        // unseen. If `record` were ever called with the full unseen set
        // instead of only what was actually displayed, the filtered-out post
        // would be marked seen before anyone had shown it to this worktree —
        // exactly the "hold-off notice for domains/terrain/ silently expires
        // while this worktree is still editing kernel/, then never renders
        // even once it starts touching domains/terrain/" scenario the
        // coordinator described.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/relevance-regression");
        let cursor = Cursor::open(&repo).expect("cursor");

        let relevant = board.append(&Post::new("notice", "b")).expect("relevant");
        let filtered_out = board
            .append(&Post::new("notice", "c").with("paths", json!(["domains/terrain/"])))
            .expect("filtered out by relevance this render");

        // Simulate a render: both are unseen, but only `relevant` survives
        // this reader's relevance filter and is actually shown.
        let both_unseen = unseen(&board, &cursor).expect("unseen");
        assert!(both_unseen.contains(&relevant));
        assert!(both_unseen.contains(&filtered_out));
        let shown = BTreeSet::from([relevant.clone()]);
        cursor.record(&board, &shown).expect("record only shown");

        let still_unseen = unseen(&board, &cursor).expect("unseen after render");
        assert!(
            !still_unseen.contains(&relevant),
            "the displayed post is now seen"
        );
        assert!(
            still_unseen.contains(&filtered_out),
            "a post that was unseen but filtered out by relevance must remain unseen, \
             not get marked seen just because it was in the unseen set at render time: \
             {still_unseen:?}"
        );
    }

    #[test]
    fn recording_is_cumulative_across_calls() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cumulative");
        let cursor = Cursor::open(&repo).expect("cursor");

        let a = board.append(&Post::new("notice", "b")).expect("a");
        let b = board.append(&Post::new("notice", "c")).expect("b");

        cursor
            .record(&board, &BTreeSet::from([a.clone()]))
            .expect("record a");
        cursor
            .record(&board, &BTreeSet::from([b.clone()]))
            .expect("record b");

        let seen = cursor.seen();
        assert!(
            seen.contains(&a) && seen.contains(&b),
            "two separate record calls must union, not replace: {seen:?}"
        );
        assert!(unseen(&board, &cursor).expect("unseen").is_empty());
    }

    #[test]
    fn recording_prunes_ids_no_longer_present_at_the_tip() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/prune");
        let cursor = Cursor::open(&repo).expect("cursor");

        let a = board.append(&Post::new("notice", "b")).expect("a");
        cursor
            .record(&board, &BTreeSet::from([a.clone()]))
            .expect("record a");
        assert!(cursor.seen().contains(&a));

        // Simulate a future reap (Task 5 does not implement one itself):
        // force the ref onto a tip built from the well-known empty-tree
        // object id, so `a`'s file is no longer present at the tip.
        let empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904";
        let old_tip = board.tip().expect("tip").expect("some");
        let reaped = repo
            .git(&["commit-tree", empty_tree, "-p", &old_tip, "-m", "reap"])
            .expect("reap commit");
        repo.git(&["update-ref", board.refname(), &reaped])
            .expect("force the ref past a's post");
        assert!(
            board.post_ids_at_tip().expect("ids").is_empty(),
            "the simulated reap must have removed a from the tip"
        );

        cursor
            .record(&board, &BTreeSet::new())
            .expect("record after reap");
        assert!(
            !cursor.seen().contains(&a),
            "an id no longer at the tip must be pruned from the persisted set: {:?}",
            cursor.seen()
        );
    }
}
