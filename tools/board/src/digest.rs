//! The human's view (D14). The reported failure mode of a cross-agent board is
//! not that it exists but that nobody looks, so this render exists for Nathan
//! and reads the ref's HISTORY rather than its tip.

use crate::BoardError;
use crate::post::Post;
use crate::store::{Board, StoredPost};
use std::collections::BTreeMap;

/// Every post appended in the last `since_days`, oldest first, from history.
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
    let mut out = Vec::new();
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
            // Read the blob by id — it survives compaction because the id IS
            // the object id (D11).
            let Ok(text) = board.repo().git(&["cat-file", "-p", id]) else {
                continue;
            };
            match Post::from_json(&text) {
                Ok(post) => out.push(StoredPost {
                    id: id.to_string(),
                    post,
                    committed_at: when,
                }),
                Err(e) => eprintln!("board: skipping malformed post {id}: {e}"),
            }
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
            let thread = a.post.str_field("thread");
            thread.is_none()
                || !posts
                    .iter()
                    .any(|r| r.post.kind == "reply" && r.post.str_field("thread") == thread)
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
        let posts = board.posts_at_tip().expect("posts");
        let mut ctx = crate::live::LiveContext::probe(&repo, &posts).expect("probe");
        // A notice whose branch does not resolve at all is ambiguous
        // (live.rs's `is_reapable`: it could be a transient race), so it is
        // only reaped once it has sat unresolved past
        // `NOTICE_GRACE_PERIOD_S` -- the exact technique store.rs's own
        // grace-period tests use. Without this, this reap is a no-op and the
        // property below is never exercised.
        ctx.now_unix += crate::live::NOTICE_GRACE_PERIOD_S + 1;
        board.reap(&ctx).expect("reap");
        assert!(
            board.post_ids_at_tip().expect("ids").is_empty(),
            "gone from the tip"
        );

        // A window measured from `ctx.now_unix` (real wall clock plus the
        // grace period), not an arbitrary far-future sentinel: `history`'s
        // cutoff is `now - since_days`, so a "now" set decades ahead of the
        // post's real commit time would push the cutoff past it and exclude
        // it even though it is well within any sane retention window.
        let seen = history(&board, 3_650, ctx.now_unix).expect("history");
        assert_eq!(seen.len(), 1, "history still has it");
        assert_eq!(seen[0].post.str_field("note"), Some("ephemeral"));
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
}
