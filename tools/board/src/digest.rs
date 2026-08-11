//! The human's view (D14). The reported failure mode of a cross-agent board is
//! not that it exists but that nobody looks, so this render exists for Nathan
//! and reads the ref's HISTORY rather than its tip.

use crate::BoardError;
use crate::post::Post;
use crate::store::{Board, StoredPost};
use std::collections::{BTreeMap, BTreeSet};

/// Every post appended in the last `since_days`, oldest first, from history.
///
/// Deduplicated by id: a post's id is the object id of its own bytes (D11),
/// so a reap-then-repost of byte-identical content is the SAME post
/// reappearing in the `--diff-filter=A` walk, not a new one. The walk is
/// oldest-first, and the cutoff for `since_days` is applied BEFORE this
/// dedupe bookkeeping — so the `committed_at` recorded here is only the
/// earliest occurrence *within the queried window*, not necessarily the
/// post's true first appearance. If the true first appearance falls
/// outside the window and only a later repost falls inside it, this
/// reports the repost's later time. Fine for a windowed view; do not read
/// it as "when the project first learned the thing."
pub fn history(
    board: &Board,
    since_days: u64,
    now_unix: u64,
) -> Result<Vec<StoredPost>, BoardError> {
    let Some(tip) = board.tip()? else {
        return Ok(Vec::new());
    };
    let cutoff = now_unix.saturating_sub(since_days.saturating_mul(86_400));
    let log = board.repo().git(&[
        "log",
        "--format=@%ct",
        "--diff-filter=A",
        "--name-only",
        "--reverse",
        &tip,
    ])?;
    // First pass: the same oldest-first walk as before, but it only collects
    // in-window ids and their attributed commit time -- no `git cat-file` yet.
    // `seen_ids`/`order` dedupe together (oldest wins, load-bearing: a
    // reap-then-repost of byte-identical content is the SAME post
    // reappearing in this walk, not a new one), so the ids handed to the
    // batch read below are already exactly the set the old per-post loop
    // would have read, once each.
    let mut order: Vec<String> = Vec::new();
    let mut when_by_id: BTreeMap<String, u64> = BTreeMap::new();
    let mut seen_ids: BTreeSet<String> = BTreeSet::new();
    let mut when = 0u64;
    for l in log.lines() {
        if let Some(ts) = l.strip_prefix('@') {
            when = ts.parse().unwrap_or(0);
        } else if let Some(id) = l
            .strip_prefix("posts/")
            .and_then(|l| l.strip_suffix(".json"))
        {
            if when < cutoff {
                continue;
            }
            if !seen_ids.insert(id.to_string()) {
                // Same content, already recorded from an earlier (or equal)
                // commit in this oldest-first walk -- not a second post.
                continue;
            }
            order.push(id.to_string());
            when_by_id.insert(id.to_string(), when);
        }
    }

    // Second pass: one batched read over every in-window id, same as
    // `posts_in`'s. Read the blob by id — it survives compaction because the
    // id IS the object id (D11).
    let blobs = board.cat_file_batch(&order)?;
    let mut out = Vec::new();
    for id in order {
        // Absent means `cat_file_batch` already warned (unreadable or, in
        // principle, malformed); do not warn twice.
        let Some(bytes) = blobs.get(&id) else {
            continue;
        };
        let text = String::from_utf8_lossy(bytes);
        match Post::from_json(&text) {
            Ok(post) => {
                let committed_at = when_by_id.get(&id).copied().unwrap_or(0);
                out.push(StoredPost {
                    id,
                    post,
                    committed_at,
                });
            }
            Err(e) => eprintln!("board: skipping malformed post {id}: {e}"),
        }
    }
    Ok(out)
}

/// A human-readable summary: what was posted, by whom, and every technique in
/// full, because technique is the half with compounding value.
pub fn digest(posts: &[StoredPost]) -> String {
    if posts.is_empty() {
        return "The board is empty — no session has posted yet.\n".to_string();
    }
    let mut by_kind: BTreeMap<&str, usize> = BTreeMap::new();
    let mut by_author: BTreeMap<&str, usize> = BTreeMap::new();
    for s in posts {
        *by_kind.entry(s.post.kind.as_str()).or_default() += 1;
        *by_author.entry(s.post.by.as_str()).or_default() += 1;
    }

    let mut out = String::from("== The board\n\n");
    out.push_str(&format!("{} posts\n\n", posts.len()));
    out.push_str("by kind:\n");
    for (kind, n) in &by_kind {
        out.push_str(&format!("  {kind:<12} {n}\n"));
    }
    out.push_str("\nby author:\n");
    for (author, n) in &by_author {
        out.push_str(&format!("  {author:<28} {n}\n"));
    }

    let techniques: Vec<&StoredPost> = posts
        .iter()
        .filter(|s| s.post.kind == "technique")
        .collect();
    if !techniques.is_empty() {
        out.push_str("\ntechnique published (the compounding half):\n");
        for s in techniques {
            out.push_str(&format!(
                "  [{}] {}\n",
                s.post.by,
                s.post.str_field("note").unwrap_or("(no note)")
            ));
            if s.post.extra.contains_key("evidence") {
                out.push_str("      (carries evidence)\n");
            } else {
                out.push_str("      (no evidence — weaker claim)\n");
            }
        }
    }

    let unanswered: Vec<&StoredPost> = posts
        .iter()
        .filter(|s| s.post.kind == "ask")
        .filter(|a| {
            // Answered-ness is judged PER ASK, not per thread value: two
            // asks can share a `thread`, and a single reply must not mark
            // both answered — a later question the reply never saw is still
            // open. A reply only counts if it was committed at or after
            // this specific ask, so a reply that predates the ask (and thus
            // cannot be responding to it) does not count either.
            let Some(thread) = a.post.str_field("thread") else {
                // No thread named: nothing could ever be correlated to this
                // ask, so it can never show as answered.
                return true;
            };
            !posts.iter().any(|r| {
                r.post.kind == "reply"
                    && r.post.str_field("thread") == Some(thread)
                    && r.committed_at >= a.committed_at
            })
        })
        .collect();
    if !unanswered.is_empty() {
        out.push_str("\nasks with no reply:\n");
        for s in unanswered {
            out.push_str(&format!(
                "  [{}] {}\n",
                s.post.by,
                s.post.str_field("note").unwrap_or("")
            ));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::git::test_support::temp_repo;
    use crate::post::Post;
    use crate::store::Board;
    use serde_json::json;

    #[test]
    fn the_digest_reads_history_so_a_reaped_post_still_appears() {
        // D14: this is the instrument for "what did sessions tell each other",
        // which is worthless if compaction hides the answer.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("notice", "campaign/never-existed").with("note", json!("ephemeral")))
            .expect("post");
        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let mut plan = crate::store::ReapPlan::probe(&repo, &snapshot).expect("probe");
        // A notice whose branch does not resolve at all is ambiguous
        // (live.rs's `is_reapable`: it could be a transient race), so it is
        // only reaped once it has sat unresolved past
        // `NOTICE_GRACE_PERIOD_S` -- the exact technique store.rs's own
        // grace-period tests use. Without this, this reap is a no-op and the
        // property below is never exercised.
        plan.advance_clock(crate::live::NOTICE_GRACE_PERIOD_S + 1);
        board.reap(&plan).expect("reap");
        assert!(
            board.post_ids_at_tip().expect("ids").is_empty(),
            "gone from the tip"
        );

        // A window measured from `ctx.now_unix` (real wall clock plus the
        // grace period), not an arbitrary far-future sentinel: `history`'s
        // cutoff is `now - since_days`, so a "now" set decades ahead of the
        // post's real commit time would push the cutoff past it and exclude
        // it even though it is well within any sane retention window.
        let seen = history(&board, 3_650, plan.context().now_unix).expect("history");
        assert_eq!(seen.len(), 1, "history still has it");
        assert_eq!(seen[0].post.str_field("note"), Some("ephemeral"));
    }

    #[test]
    fn one_unparseable_post_in_history_does_not_hide_the_digest() {
        // Spec test-plan item 9, for the digest's own copy of the
        // skip-and-warn arm. `history` reads blobs by object id, so a corrupt
        // post that arrived from a clone or a hand write reaches this parse
        // exactly as it reaches `posts_in`'s -- and the digest is the human's
        // only read seam, so one bad post silencing it is the worst outcome
        // available. Replacing this arm with `?` keeps every other test green
        // (see the mutation check in the fix-wave report).
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        board
            .append(&Post::new("technique", "campaign/x").with("note", json!("keep me")))
            .expect("good post");

        // Splice raw bytes into the tip tree, bypassing `append` --
        // `canonical_bytes` cannot emit something `from_json` rejects, which
        // is why this arm can only be reached from outside the tool.
        let blob = repo.hash_object(b"} not json {\n").expect("hash-object");
        let old = board.tip().expect("tip").expect("some");
        let index = repo
            .git_path(&format!("hv-digest-splice-{}", std::process::id()))
            .expect("index path");
        let _ = std::fs::remove_file(&index);
        repo.git_with_index(&index, &["read-tree", &old])
            .expect("read-tree");
        repo.git_with_index(
            &index,
            &[
                "update-index",
                "--add",
                "--cacheinfo",
                &format!("100644,{blob},posts/{blob}.json"),
            ],
        )
        .expect("update-index");
        let tree = repo
            .git_with_index(&index, &["write-tree"])
            .expect("write-tree");
        let _ = std::fs::remove_file(&index);
        let new = repo
            .git(&["commit-tree", &tree, "-p", &old, "-m", "a corrupt post"])
            .expect("commit-tree");
        repo.git(&["update-ref", board.refname(), &new, &old])
            .expect("update-ref");

        let now = repo
            .git(&["log", "-1", "--format=%ct", &new])
            .expect("commit time")
            .parse::<u64>()
            .expect("timestamp");
        let seen = history(&board, 3_650, now)
            .expect("a corrupt post must be SKIPPED, never turned into an Err");
        assert_eq!(seen.len(), 1, "only the good post survives: {seen:?}");
        assert_eq!(seen[0].post.str_field("note"), Some("keep me"));
        let out = digest(&seen);
        assert!(
            out.contains("keep me"),
            "the digest must still render the good post's body: {out}"
        );
    }

    #[test]
    fn the_digest_groups_by_kind_and_counts_authors() {
        let posts = vec![
            StoredPost {
                id: "a".into(),
                post: Post::new("technique", "campaign/x").with("note", json!("t1")),
                committed_at: 10,
            },
            StoredPost {
                id: "b".into(),
                post: Post::new("technique", "campaign/y").with("note", json!("t2")),
                committed_at: 20,
            },
            StoredPost {
                id: "c".into(),
                post: Post::new("claim", "campaign/x"),
                committed_at: 30,
            },
        ];
        let out = digest(&posts);
        assert!(out.contains("technique"), "names the kinds: {out}");
        assert!(out.contains('2'), "counts them: {out}");
        assert!(out.contains("campaign/x"), "names authors: {out}");
        assert!(
            out.contains("t1"),
            "shows technique bodies, which are the point: {out}"
        );
    }

    #[test]
    fn an_empty_board_digests_to_a_plain_statement_not_an_error() {
        let out = digest(&[]);
        assert!(
            !out.is_empty(),
            "say 'nothing yet' rather than printing nothing"
        );
    }

    #[test]
    fn a_single_ask_with_no_reply_is_unanswered() {
        let posts = vec![StoredPost {
            id: "a".into(),
            post: Post::new("ask", "campaign/x")
                .with("thread", json!("t1"))
                .with("note", json!("is anyone else seeing this")),
            committed_at: 10,
        }];
        let out = digest(&posts);
        assert!(
            out.contains("is anyone else seeing this"),
            "an ask with no reply at all must be flagged: {out}"
        );
    }

    #[test]
    fn a_single_ask_with_a_later_reply_in_its_thread_is_answered() {
        let posts = vec![
            StoredPost {
                id: "a".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("is anyone else seeing this")),
                committed_at: 10,
            },
            StoredPost {
                id: "b".into(),
                post: Post::new("reply", "campaign/y").with("thread", json!("t1")),
                committed_at: 20,
            },
        ];
        let out = digest(&posts);
        assert!(
            !out.contains("is anyone else seeing this"),
            "a reply committed after the ask, in the same thread, must clear it: {out}"
        );
    }

    #[test]
    fn an_ask_with_no_thread_field_is_always_unanswered() {
        // Nothing can correlate a reply to a threadless ask, so it must
        // never be able to show as answered.
        let posts = vec![StoredPost {
            id: "a".into(),
            post: Post::new("ask", "campaign/x").with("note", json!("untethered question")),
            committed_at: 10,
        }];
        let out = digest(&posts);
        assert!(
            out.contains("untethered question"),
            "a threadless ask must always be flagged: {out}"
        );
    }

    #[test]
    fn an_ask_is_answered_per_ask_not_per_thread_so_a_later_question_still_shows_unanswered() {
        // I1: two asks share one thread; a single reply that predates the
        // second question must not silently mark it answered too -- that
        // false negative is exactly the failure mode the unanswered-ask
        // list exists to catch.
        let posts = vec![
            StoredPost {
                id: "ask1".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("first question")),
                committed_at: 10,
            },
            StoredPost {
                id: "reply1".into(),
                post: Post::new("reply", "campaign/y").with("thread", json!("t1")),
                committed_at: 20,
            },
            StoredPost {
                id: "ask2".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("second question")),
                committed_at: 30,
            },
        ];
        let out = digest(&posts);
        assert!(
            out.contains("second question"),
            "the later, genuinely unanswered question must be flagged: {out}"
        );
        assert!(
            !out.contains("first question"),
            "the earlier question was answered by the reply and must not be flagged: {out}"
        );
    }

    #[test]
    fn a_reply_older_than_its_ask_does_not_answer_it() {
        // I1's other half: a reply that predates the ask cannot possibly be
        // responding to it, so it must not count as an answer.
        let posts = vec![
            StoredPost {
                id: "reply1".into(),
                post: Post::new("reply", "campaign/y").with("thread", json!("t1")),
                committed_at: 50,
            },
            StoredPost {
                id: "ask1".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("asked after the only reply")),
                committed_at: 100,
            },
        ];
        let out = digest(&posts);
        assert!(
            out.contains("asked after the only reply"),
            "a reply older than the ask it shares a thread with must not clear it: {out}"
        );
    }

    #[test]
    fn history_reads_every_post_in_the_window_in_one_batch() {
        // Behavioural, not a spawn count: the guarantee is that batching changed
        // nothing observable. Ten posts, all inside the window, all present, in
        // committed order.
        //
        // The brief's literal `now_unix` (2_000_000_000, i.e. 2033-05-18) is a
        // fixed future sentinel; with `since_days = 14` that opens a 14-day
        // window ending in 2033, which excludes every real commit made before
        // then -- including these, made today. Use the tip's own real commit
        // time instead, exactly as
        // `one_unparseable_post_in_history_does_not_hide_the_digest` already
        // does below: `SystemTime::now()` would need a fresh
        // `#[allow(clippy::disallowed_types)]`, and this crate already has
        // exactly two sanctioned wall-clock call sites (`main.rs`, `live.rs`'s
        // probe) -- a test can read the clock through git instead of minting
        // a third.
        let (_dir, repo) = crate::git::test_support::temp_repo();
        let board = Board::new(repo.clone());
        for i in 0..10 {
            board
                .append(&Post::new("technique", "main").with("note", json!(format!("note {i}"))))
                .expect("append");
        }
        let tip = board.tip().expect("tip").expect("some");
        let now_unix: u64 = repo
            .git(&["log", "-1", "--format=%ct", &tip])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let posts = history(&board, 14, now_unix).expect("history");
        assert_eq!(posts.len(), 10);
        for i in 0..10 {
            assert!(
                posts
                    .iter()
                    .any(|p| p.post.str_field("note") == Some(&format!("note {i}")[..])),
                "post {i} missing from the digest window"
            );
        }
    }

    #[test]
    fn history_dedupes_a_reap_then_repost_of_identical_content_keeping_the_earliest_committed_at() {
        // I3: a post's id is the hash of its own bytes, so a reap-then-
        // repost of byte-identical content is the SAME post reappearing in
        // the `--diff-filter=A` walk, not a new one -- double-counting it
        // would be exactly the "did the board double-count" blind spot the
        // review named.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let post = Post::new("technique", "campaign/x").with("note", json!("reuse a temp index"));
        let id = board.append(&post).expect("first append");
        let first_committed_at: u64 = repo
            .git(&[
                "log",
                "-1",
                "--format=%ct",
                &board.tip().expect("tip").expect("some"),
            ])
            .expect("first commit time")
            .parse()
            .expect("timestamp");

        // A bare technique never decays on its own (`is_reapable` never
        // drops one unretracted); retract it explicitly so it can be
        // legitimately reaped, then reappend byte-identical content -- the
        // repost the review describes.
        board
            .append(&Post::new("retract", "campaign/x").with("post", json!(id)))
            .expect("retract");
        let snapshot = board.snapshot().expect("snapshot").expect("some");
        let plan = crate::store::ReapPlan::probe(&repo, &snapshot).expect("probe");
        let dropped = board.reap(&plan).expect("reap");
        assert_eq!(
            dropped, 1,
            "the retracted technique should be the only thing reaped"
        );

        board.append(&post).expect("repost identical content");

        let seen = history(&board, 3_650, plan.context().now_unix + 1).expect("history");
        let techniques: Vec<&StoredPost> = seen
            .iter()
            .filter(|sp| sp.post.kind == "technique")
            .collect();
        assert_eq!(
            techniques.len(),
            1,
            "identical content must count once, not twice: {seen:?}"
        );
        assert_eq!(
            techniques[0].committed_at, first_committed_at,
            "the earliest commit's timestamp must win, not the repost's"
        );
    }
}
