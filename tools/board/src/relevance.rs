//! Routing by topic, and per-worktree read state. Both are read-time concerns
//! (D10): nothing here is stored on the board.

use crate::BoardError;
use crate::git::Repo;
use crate::post::Post;
use crate::store::{Board, StoredPost};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The reading worktree's own changed files, relative to `main`.
pub fn changed_paths(repo: &Repo) -> Result<Vec<String>, BoardError> {
    // Qualified as `refs/heads/main...HEAD`, not the bare `main...HEAD`: a
    // same-named tag would otherwise win git's ref-disambiguation order
    // (`refs/<name>`, then `refs/tags/<name>`, then `refs/heads/<name>`) and
    // silently change what this range means (the same hazard `live.rs`'s
    // `resolve_branch_ref` closes for `by`).
    //
    // `unwrap_or_default()` is safe here, but NOT for the reason the ledger
    // originally recorded ("fewer path filters ⇒ more posts reach the
    // reader"). That direction is backwards: `changed` is the reader's TOPIC
    // SET, not a filter list, so fewer entries mean fewer matches, and an
    // empty one used to mean "broadcasts only". What makes the fallback safe
    // is [`is_relevant`]'s treatment of the empty case — an empty `changed`
    // means "no routing information about this session", and the fail-open
    // reading of not-knowing is to show the post. The direction of a fallback
    // cannot be judged at this function's boundary; it is decided at the call
    // site that consumes the value.
    let out = repo
        .git(&["diff", "--name-only", "refs/heads/main...HEAD"])
        .unwrap_or_default();
    Ok(out.lines().map(str::to_string).collect())
}

/// Does this post concern work the reader is doing?
///
/// Three cases, and the third is the one that decides whether this board
/// works at the moment it fires:
///
/// - A post naming **no paths** is a broadcast and reaches everyone.
/// - A post naming paths matches when one of them prefixes one of the
///   reader's changed files.
/// - A reader with an **empty `changed` set** receives everything. An empty
///   set means "I do not know what this session is working on", never "this
///   session is working on nothing" — and the ambient render fires at
///   `SessionStart`, which is exactly when a fresh campaign branch sitting at
///   `main`'s tip has changed nothing yet, and when a session mid-edit-
///   before-commit still shows nothing in `main...HEAD`. Treating that as
///   "broadcasts only" filtered every path-scoped `hold-off` out of the one
///   render that had a chance to arrive in time: the session about to start
///   editing `domains/terrain/` was precisely the session not shown the
///   `domains/terrain/` hold-off. Fail open on not-knowing, as the rest of
///   this crate does.
pub fn is_relevant(post: &Post, changed: &[String]) -> bool {
    let paths = post.paths();
    if paths.is_empty() {
        return true;
    }
    if changed.is_empty() {
        return true;
    }
    paths
        .iter()
        .any(|p| changed.iter().any(|c| c.starts_with(p.as_str())))
}

/// The subset of unseen posts a render actually shows this reader: unseen
/// *and* relevant.
///
/// This type exists so that [`Cursor::record`] cannot be handed the wrong
/// set by accident. Its only public constructor, [`Displayed::filter`],
/// performs the unseen-and-relevant filter itself — there is no way to build
/// a `Displayed` from an arbitrary `BTreeSet<String>` (in particular, from
/// [`unseen`]'s own raw output) without going through that filter. That
/// makes it a *compile* error to record the wrong set, closing structurally
/// — not merely by doc comment — the bug where recording an
/// unseen-but-irrelevant post as "seen" permanently hides it, even after it
/// becomes relevant later.
#[derive(Debug, Clone)]
pub struct Displayed(Vec<StoredPost>);

impl Displayed {
    /// Filter `posts` down to exactly what one render displays to this
    /// reader: a member of `unseen`, and relevant to `changed` per
    /// [`is_relevant`]. This is the only way to construct a `Displayed`.
    pub fn filter(posts: &[StoredPost], unseen: &BTreeSet<String>, changed: &[String]) -> Self {
        Self(
            posts
                .iter()
                .filter(|s| unseen.contains(&s.id) && is_relevant(&s.post, changed))
                .cloned()
                .collect(),
        )
    }

    /// The posts actually displayed, in the order `posts` supplied them —
    /// what a render draws its text from.
    pub fn posts(&self) -> &[StoredPost] {
        &self.0
    }

    /// Their ids, derived from the already-filtered posts (not a second
    /// filter pass) — what [`Cursor::record`] consumes.
    pub fn ids(&self) -> BTreeSet<String> {
        self.0.iter().map(|s| s.id.clone()).collect()
    }

    /// Shrink to at most `max_posts` by dropping the **oldest**, reporting
    /// how many were dropped.
    ///
    /// `filter` preserves the oldest-first order
    /// [`Board::posts_at_tip`](crate::store::Board::posts_at_tip) supplies,
    /// so keeping the newest means keeping the *tail*. Truncating the tail
    /// instead — the obvious reading of "cap" — deferred the freshest posts
    /// and showed the twelve stalest, which is backwards for a channel whose
    /// entire value is timeliness: the newest post is the one most likely to
    /// be a still-live `hold-off`. Nothing is lost either way (an elided post
    /// stays unseen), but the priority was inverted.
    ///
    /// This does **not** weaken `Displayed`'s guarantee: `cap` can only ever
    /// shrink a `Displayed` that [`filter`](Self::filter) already built, so
    /// there is still no way to construct one holding a post that was never
    /// selected by the unseen-and-relevant filter — it can only hold fewer
    /// of them. It exists so a render's own line budget and
    /// [`Cursor::record`]'s notion of "shown" can never disagree: the value
    /// this returns is the exact value a render draws its text from, so
    /// recording it (rather than the pre-cap `Displayed`) is what keeps a
    /// post that a line budget elided from ever being marked seen. That
    /// property is unaffected by *which* end is dropped — both the text and
    /// the recorded ids come from this one value.
    ///
    /// Relative order among the survivors is preserved, so the render still
    /// reads chronologically, oldest of the survivors first.
    pub fn cap(mut self, max_posts: usize) -> (Self, usize) {
        if self.0.len() <= max_posts {
            return (self, 0);
        }
        let elided = self.0.len() - max_posts;
        self.0.drain(..elided);
        (self, elided)
    }
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
/// displayed. [`Displayed`] closes the remaining hole in that story — that
/// "actually shown" was, until now, only a documented convention on
/// [`Cursor::record`]'s parameter, and a `BTreeSet<String>` cannot enforce
/// which set it holds.
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
    /// `shown`'s type is the guarantee here, not a comment: a [`Displayed`]
    /// can only have been built by [`Displayed::filter`], so there is no
    /// unfiltered `BTreeSet<String>` — in particular, no raw [`unseen`]
    /// result — that type-checks as an argument here. Recording something
    /// that was never filtered for relevance is a compile error, not a
    /// silent recreation of the swallowed-notice bug this representation
    /// exists to prevent.
    ///
    /// Also prunes: any previously-recorded id no longer present at
    /// `board`'s tip is dropped. A reaped post can never render again, so
    /// keeping its id here forever would grow this file without bound — it
    /// stays sized to the live post count instead.
    pub fn record(&self, board: &Board, shown: &Displayed) -> Result<(), BoardError> {
        let live: BTreeSet<String> = board.post_ids_at_tip()?.into_iter().collect();
        let mut all = self.seen();
        all.extend(shown.ids());
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
///
/// This raw `BTreeSet<String>` is deliberately *not* what a render passes to
/// [`Cursor::record`] — feed it through [`Displayed::filter`] first.
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
    fn a_path_scoped_post_is_relevant_when_the_reader_has_no_changed_paths() {
        // I3. An empty `changed` set means "no routing information about this
        // session", never "this session is working on nothing" -- and the
        // ambient render fires at SessionStart, which is exactly when a fresh
        // campaign branch sitting at main's tip has changed nothing. Treating
        // that as "broadcasts only" filtered every path-scoped hold-off out
        // of the one render that could have arrived in time. There was no
        // test pinning this in EITHER direction before, which is how the
        // ledger came to record the direction backwards.
        let p = Post::new("notice", "b")
            .with("paths", json!(["domains/terrain/"]))
            .with("polarity", json!("hold-off"));
        assert!(
            is_relevant(&p, &[]),
            "with nothing known about the reader, fail OPEN and show the post"
        );
        // And the fallback `changed_paths` actually takes -- an empty Vec --
        // is the same value, so the two agree.
        let empty: Vec<String> = Vec::new();
        assert!(is_relevant(&p, &empty));
    }

    #[test]
    fn a_displayed_shows_a_path_scoped_hold_off_to_a_reader_with_no_changed_paths() {
        // I3 at the layer that decides what a session actually sees: the
        // fresh-branch case, end to end through `Displayed::filter`.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/empty-changed");
        let cursor = Cursor::open(&repo).expect("cursor");
        let id = board
            .append(
                &Post::new("notice", "b")
                    .with("paths", json!(["domains/terrain/"]))
                    .with("polarity", json!("hold-off")),
            )
            .expect("scoped hold-off");

        let posts = board.posts_at_tip().expect("posts");
        let unseen_ids = unseen(&board, &cursor).expect("unseen");
        let displayed = Displayed::filter(&posts, &unseen_ids, &[]);
        assert!(
            displayed.ids().contains(&id),
            "a fresh branch with no changed paths must still be shown a \
             path-scoped hold-off: {:?}",
            displayed.ids()
        );
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

        let posts = board.posts_at_tip().expect("posts");
        let displayed = Displayed::filter(&posts, &BTreeSet::from([first.clone()]), &[]);
        cursor.record(&board, &displayed).expect("record");
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
    fn recording_only_the_displayed_posts_leaves_a_relevance_filtered_post_unseen() {
        // The regression test for the flaw this representation replaced: a
        // post that is unseen but dropped by the relevance filter must stay
        // unseen. If `Displayed::filter` ever included the full unseen set
        // instead of only what actually passes `is_relevant`, the
        // filtered-out post would be marked seen before anyone had shown it
        // to this worktree — exactly the "hold-off notice for
        // domains/terrain/ silently expires while this worktree is still
        // editing kernel/, then never renders even once it starts touching
        // domains/terrain/" scenario the coordinator described.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/relevance-regression");
        let cursor = Cursor::open(&repo).expect("cursor");

        let relevant = board.append(&Post::new("notice", "b")).expect("relevant");
        let filtered_out = board
            .append(&Post::new("notice", "c").with("paths", json!(["domains/terrain/"])))
            .expect("filtered out by relevance this render");

        // Simulate a render: both are unseen, but this reader's changed
        // paths (`kernel/...`) do not touch `domains/terrain/`, so only
        // `relevant` survives the relevance filter and is actually shown.
        let posts = board.posts_at_tip().expect("posts");
        let both_unseen = unseen(&board, &cursor).expect("unseen");
        assert!(both_unseen.contains(&relevant));
        assert!(both_unseen.contains(&filtered_out));
        let changed = vec!["kernel/src/lib.rs".to_string()];
        let displayed = Displayed::filter(&posts, &both_unseen, &changed);
        assert_eq!(
            displayed.ids(),
            BTreeSet::from([relevant.clone()]),
            "only the relevant post should be displayed"
        );

        cursor
            .record(&board, &displayed)
            .expect("record only shown");

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
    fn a_displayed_with_no_relevant_posts_is_empty_and_records_nothing() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/none-relevant");
        let cursor = Cursor::open(&repo).expect("cursor");

        let id = board
            .append(&Post::new("notice", "b").with("paths", json!(["domains/terrain/"])))
            .expect("scoped post");

        let posts = board.posts_at_tip().expect("posts");
        let unseen_ids = unseen(&board, &cursor).expect("unseen");
        let changed = vec!["kernel/src/lib.rs".to_string()]; // never touches domains/terrain/
        let displayed = Displayed::filter(&posts, &unseen_ids, &changed);
        assert!(
            displayed.posts().is_empty(),
            "no post should pass the filter"
        );
        assert!(displayed.ids().is_empty());

        cursor
            .record(&board, &displayed)
            .expect("record an empty Displayed");
        assert!(
            cursor.seen().is_empty(),
            "recording an empty Displayed must mark nothing seen: {:?}",
            cursor.seen()
        );
        assert!(
            unseen(&board, &cursor).expect("unseen").contains(&id),
            "the post must still be unseen"
        );
    }

    #[test]
    fn a_displayed_includes_a_broadcast_post_with_no_paths_when_unseen() {
        // A post with no `paths` reaches everyone (`is_relevant`'s broadcast
        // rule) and so must always be displayed when unseen -- even against
        // the weakest possible relevance context, an empty changed set. If
        // `Displayed::filter` ever treated "no paths" as "not relevant",
        // every broadcast on the board would silently vanish, which is this
        // layer's worst failure mode.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/broadcast");
        let cursor = Cursor::open(&repo).expect("cursor");

        let id = board.append(&Post::new("notice", "b")).expect("broadcast");

        let posts = board.posts_at_tip().expect("posts");
        let unseen_ids = unseen(&board, &cursor).expect("unseen");
        let displayed = Displayed::filter(&posts, &unseen_ids, &[]);
        assert!(
            displayed.ids().contains(&id),
            "a broadcast must be displayed even with no changed paths: {:?}",
            displayed.ids()
        );
    }

    #[test]
    fn recording_is_cumulative_across_calls() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cumulative");
        let cursor = Cursor::open(&repo).expect("cursor");

        let a = board.append(&Post::new("notice", "b")).expect("a");
        let b = board.append(&Post::new("notice", "c")).expect("b");
        let posts = board.posts_at_tip().expect("posts");

        cursor
            .record(
                &board,
                &Displayed::filter(&posts, &BTreeSet::from([a.clone()]), &[]),
            )
            .expect("record a");
        cursor
            .record(
                &board,
                &Displayed::filter(&posts, &BTreeSet::from([b.clone()]), &[]),
            )
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
        let posts = board.posts_at_tip().expect("posts");
        cursor
            .record(
                &board,
                &Displayed::filter(&posts, &BTreeSet::from([a.clone()]), &[]),
            )
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
            .record(&board, &Displayed::filter(&[], &BTreeSet::new(), &[]))
            .expect("record after reap");
        assert!(
            !cursor.seen().contains(&a),
            "an id no longer at the tip must be pruned from the persisted set: {:?}",
            cursor.seen()
        );
    }

    #[test]
    fn cap_at_exactly_the_budget_elides_nothing() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cap-boundary-exact");
        for i in 0..3 {
            board
                .append(&Post::new("notice", "b").with("i", json!(i)))
                .expect("append");
        }
        let posts = board.posts_at_tip().expect("posts");
        let all: BTreeSet<String> = posts.iter().map(|sp| sp.id.clone()).collect();
        let displayed = Displayed::filter(&posts, &all, &[]);
        let (capped, elided) = displayed.cap(3);
        assert_eq!(elided, 0, "exactly at budget must elide nothing");
        assert_eq!(capped.posts().len(), 3);
    }

    #[test]
    fn cap_one_over_the_budget_elides_exactly_one() {
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cap-boundary-over");
        for i in 0..4 {
            board
                .append(&Post::new("notice", "b").with("i", json!(i)))
                .expect("append");
        }
        let posts = board.posts_at_tip().expect("posts");
        let all: BTreeSet<String> = posts.iter().map(|sp| sp.id.clone()).collect();
        let displayed = Displayed::filter(&posts, &all, &[]);
        let (capped, elided) = displayed.cap(3);
        assert_eq!(
            elided, 1,
            "one over budget must elide exactly one, not off by one"
        );
        assert_eq!(capped.posts().len(), 3);
    }

    #[test]
    fn cap_keeps_the_newest_posts_and_elides_the_oldest() {
        // I1. `posts_at_tip` sorts oldest-first, so a `truncate` kept the
        // twelve STALEST posts and deferred the freshest -- backwards for a
        // channel whose whole value is timeliness, since the newest post is
        // the one most likely to be a still-live hold-off. Order among the
        // survivors must still read chronologically.
        //
        // Built by hand rather than through a board: six real appends land in
        // the same wall-clock second, and `posts_at_tip`'s sort key is
        // `(committed_at, id)`, so append order is NOT recoverable from them.
        // Distinct `committed_at` values are what make "oldest" and "newest"
        // mean anything here.
        let posts: Vec<StoredPost> = (0..6)
            .map(|i| StoredPost {
                id: format!("id{i}"),
                post: Post::new("notice", "b").with("i", json!(i)),
                committed_at: 1_000 + i,
            })
            .collect();
        let all: BTreeSet<String> = posts.iter().map(|sp| sp.id.clone()).collect();
        let (shown, elided) = Displayed::filter(&posts, &all, &[]).cap(2);
        assert_eq!(elided, 4);
        let kept: Vec<&str> = shown.posts().iter().map(|sp| sp.id.as_str()).collect();
        assert_eq!(
            kept,
            vec!["id4", "id5"],
            "the two NEWEST must survive, in chronological order: {kept:?}"
        );
    }

    #[test]
    fn what_the_cap_keeps_is_exactly_what_gets_recorded_as_shown() {
        // The property that has broken twice, re-checked against I1's change
        // of which end `cap` drops: `render` draws its text from the capped
        // value and `Cursor::record` is handed the same capped value, so
        // "recorded" and "rendered" are the same set by construction, and the
        // elided OLDEST posts stay unseen rather than being marked seen.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cap-newest-record");
        let cursor = Cursor::open(&repo).expect("cursor");
        let ids: Vec<String> = (0..5)
            .map(|i| {
                board
                    .append(&Post::new("notice", "b").with("note", json!(format!("n{i}"))))
                    .expect("append")
            })
            .collect();

        let posts = board.posts_at_tip().expect("posts");
        let unseen_ids = unseen(&board, &cursor).expect("unseen");
        let (shown, elided) = Displayed::filter(&posts, &unseen_ids, &[]).cap(2);
        assert_eq!(elided, 3);
        let out = crate::render::render(
            shown.posts(),
            elided,
            &crate::render::RenderOptions::session_start(),
        );
        cursor.record(&board, &shown).expect("record");

        // The survivors are the tail of the order `filter` was given -- which
        // is `posts_at_tip`'s `(committed_at, id)` order, not append order,
        // since five appends land in the same second.
        let expected: BTreeSet<String> = posts[posts.len() - 2..]
            .iter()
            .map(|sp| sp.id.clone())
            .collect();
        assert_eq!(shown.ids(), expected, "the cap keeps the tail");

        let still_unseen = unseen(&board, &cursor).expect("unseen after");
        for id in &ids {
            let note = posts
                .iter()
                .find(|sp| &sp.id == id)
                .and_then(|sp| sp.post.str_field("note").map(str::to_string))
                .expect("note");
            let rendered = out.contains(&format!("note={note}"));
            assert_eq!(
                rendered,
                !still_unseen.contains(id),
                "post {note}: rendered={rendered} but seen={} -- recorded and \
                 rendered must be the same set:\n{out}",
                !still_unseen.contains(id)
            );
        }
        assert_eq!(
            still_unseen.len(),
            3,
            "the three elided (oldest) posts must remain unseen: {still_unseen:?}"
        );
    }

    #[test]
    fn cap_never_shrinks_below_what_filter_selected_when_under_budget() {
        // A `Displayed` smaller than the budget must pass through untouched
        // -- `cap` only ever removes, it never pads or reorders.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cap-under-budget");
        let id = board.append(&Post::new("notice", "b")).expect("append");
        let posts = board.posts_at_tip().expect("posts");
        let displayed = Displayed::filter(&posts, &BTreeSet::from([id.clone()]), &[]);
        let (capped, elided) = displayed.cap(50);
        assert_eq!(elided, 0);
        assert_eq!(capped.ids(), BTreeSet::from([id]));
    }

    #[test]
    fn capping_before_recording_leaves_elided_posts_unseen_not_permanently_hidden() {
        // The regression test for the Critical finding (C1): render()'s own
        // line-budget cap must never disagree with what `Cursor::record` is
        // told was shown. This mirrors main.rs's ambient path exactly --
        // filter, then cap to the render budget, then render, then record
        // the CAPPED `Displayed` -- and confirms that the posts the cap
        // elided remain unseen (so they can still render once the board
        // catches up), while the posts actually shown do not.
        let (_d, repo) = temp_repo();
        let board = Board::with_ref(repo.clone(), "refs/test/cap-regression");
        let cursor = Cursor::open(&repo).expect("cursor");

        let budget = crate::render::RenderOptions::session_start().post_budget();
        let total = budget + 5;
        let ids: Vec<String> = (0..total)
            .map(|i| {
                board
                    .append(&Post::new("notice", "b").with("note", json!(format!("n{i}"))))
                    .unwrap_or_else(|e| panic!("append {i}: {e}"))
            })
            .collect();

        let posts = board.posts_at_tip().expect("posts");
        let unseen_ids = unseen(&board, &cursor).expect("unseen");
        assert_eq!(
            unseen_ids.len(),
            total,
            "every post should start out unseen"
        );

        let displayed = Displayed::filter(&posts, &unseen_ids, &[]);
        let (shown, elided) = displayed.cap(budget);
        assert_eq!(
            elided, 5,
            "exactly the overflow past the render budget must be reported as elided"
        );
        assert_eq!(shown.posts().len(), budget);

        let out = crate::render::render(
            shown.posts(),
            elided,
            &crate::render::RenderOptions::session_start(),
        );
        assert!(
            out.contains("more"),
            "the elision notice must appear: {out}"
        );

        // Record the CAPPED value -- the fix. Recording the pre-cap
        // `displayed` instead is exactly the C1 bug: see the mutation check
        // in the task report (it reliably turns this test red).
        cursor
            .record(&board, &shown)
            .expect("record only what was actually rendered");

        let still_unseen = unseen(&board, &cursor).expect("unseen after render");
        let shown_ids = shown.ids();
        for id in &ids {
            if shown_ids.contains(id) {
                assert!(
                    !still_unseen.contains(id),
                    "a post that was actually rendered must now be seen: {id}"
                );
            } else {
                assert!(
                    still_unseen.contains(id),
                    "a post the cap elided -- never rendered -- must remain unseen, \
                     not be permanently hidden just because it was in the pre-cap \
                     Displayed at render time: {id}"
                );
            }
        }
        assert_eq!(
            still_unseen.len(),
            5,
            "exactly the 5 elided posts should remain unseen"
        );
    }
}
