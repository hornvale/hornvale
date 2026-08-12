//! The human's view (D14). The reported failure mode of a cross-agent board is
//! not that it exists but that nobody looks, so this render exists for Nathan
//! and reads the ref's HISTORY rather than its tip.

use crate::BoardError;
use crate::live::redacted_ids;
use crate::post::Post;
use crate::store::{Board, Origin, StoredPost};
use std::collections::{BTreeMap, BTreeSet};

/// Every post appended in the last `since_days`, oldest first, from history —
/// across **every ref a read draws from** (B1), not just this host's log.
///
/// The union is not symmetry for its own sake. This is the HUMAN seam (D14),
/// and `make board-digest` is the command Nathan actually reads the board
/// through; leaving it single-ref would have made the human view show only
/// this host's history while the ambient render showed the union — a silent
/// divergence between the two seams, in the direction that matters least.
/// Same refs, same dedupe rule ([`crate::store::merge_by_id`]) as
/// [`Board::posts_at_tip`], so the two cannot drift.
///
/// Deduplicated by id: a post's id is the object id of its own bytes (D11),
/// so a reap-then-repost of byte-identical content is the SAME post
/// reappearing in the `--diff-filter=A` walk, not a new one — and so is the
/// same post appearing in two hosts' logs. The walk is oldest-first, and the
/// cutoff for `since_days` is applied BEFORE this dedupe bookkeeping — so the
/// `committed_at` recorded here is only the earliest occurrence *within the
/// queried window*, not necessarily the post's true first appearance. If the
/// true first appearance falls outside the window and only a later repost
/// falls inside it, this reports the repost's later time. Fine for a windowed
/// view; do not read it as "when the project first learned the thing."
pub fn history(
    board: &Board,
    since_days: u64,
    now_unix: u64,
) -> Result<Vec<StoredPost>, BoardError> {
    let cutoff = now_unix.saturating_sub(since_days.saturating_mul(86_400));
    let mut collected: Vec<StoredPost> = Vec::new();
    for (refname, origin, tip) in board.resolved_read_refs()? {
        match history_in(board, &tip, cutoff, &origin) {
            Ok(posts) => collected.extend(posts),
            // Same classification as the tip read: a peer mirror that cannot
            // be walked is one warning, never a blank digest (D7); this
            // host's own log is still fatal.
            Err(e) => crate::store::tolerate_unreadable_peer(&origin, &refname, e)?,
        }
    }
    Ok(crate::store::merge_by_id(collected))
}

/// [`history`]'s walk over ONE ref, against a tip the caller already
/// resolved. Split out so the union above is a loop rather than a second
/// copy of the walk.
fn history_in(
    board: &Board,
    tip: &str,
    cutoff: u64,
    origin: &Origin,
) -> Result<Vec<StoredPost>, BoardError> {
    let log = board.repo().git(&[
        "log",
        "--format=@%ct",
        "--diff-filter=A",
        "--name-only",
        "--reverse",
        tip,
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
                    // The origin of the ref this walk is over, exactly as the
                    // tip read tags its posts — `history`'s caller unions
                    // several of these.
                    origin: origin.clone(),
                });
            }
            Err(e) => eprintln!("board: skipping malformed post {id}: {e}"),
        }
    }
    Ok(out)
}

/// A human-readable summary: what was posted, by whom, and every technique in
/// full, because technique is the half with compounding value.
///
/// **Redaction (B8, D10) is enforced HERE, not upstream.** `history` never
/// drops a redacted post — D13 says the ref's root, and everything reachable
/// from it, is never rewritten — so the target is still IN `posts` exactly
/// as any other post is. What this function must not do is print its body:
/// every `redact` post in the window names a target id
/// ([`crate::live::redacted_ids`], the same computation the ambient render's
/// [`LiveContext`](crate::live::LiveContext) is built from — two copies could
/// only ever disagree in the direction where one seam prints what the other
/// suppresses), and every body-bearing section below consults that set before
/// printing anything the target carries. The act itself is still
/// reported, in its own section, by id and author — reporting the act
/// without the body is the whole point of a read-time redaction instead of a
/// (prohibited, and measured not to work) history rewrite.
pub fn digest(posts: &[StoredPost]) -> String {
    if posts.is_empty() {
        return "The board is empty — no session has posted yet.\n".to_string();
    }

    let redacted_ids = redacted_ids(posts);

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

    // B10's tally: corroboration, not consensus. `confirm` and `stale` are
    // additive evidence naming a target post's id in a `post` field -- never
    // an up/down vote, and never capable of suppressing anything (that
    // property is the whole point: a `hold-off` is exactly the post most
    // likely to attract a `stale`, and this tally must never read as a
    // reason to hide it). Counted separately per target, so "confirmed 2,
    // stale 1" says something a single combined number would flatten.
    let mut confirms: BTreeMap<&str, usize> = BTreeMap::new();
    let mut stales: BTreeMap<&str, usize> = BTreeMap::new();
    for s in posts {
        let Some(target) = s.post.str_field("post") else {
            continue;
        };
        match s.post.kind.as_str() {
            "confirm" => *confirms.entry(target).or_default() += 1,
            "stale" => *stales.entry(target).or_default() += 1,
            _ => {}
        }
    }

    let techniques: Vec<&StoredPost> = posts
        .iter()
        .filter(|s| s.post.kind == "technique")
        .filter(|s| !redacted_ids.contains(s.id.as_str()))
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
            let confirmed = confirms.get(s.id.as_str()).copied().unwrap_or(0);
            let stale = stales.get(s.id.as_str()).copied().unwrap_or(0);
            if confirmed > 0 || stale > 0 {
                out.push_str(&format!(
                    "      corroboration: confirmed {confirmed}, stale {stale}\n"
                ));
            }
        }
    }

    // B12: digest-only. `suggest` never renders ambiently (see
    // `crate::render::live_posts`'s `DIGEST_ONLY_KINDS`) -- it is the
    // lowest-effort post kind and so the likeliest flood source, and it is
    // not actionable by the session that would read it ambiently anyway.
    // This section is the one seam it does reach, and its promotion path
    // out of "merely suggested" is named right where the only reader who
    // can act on it looks.
    let suggestions: Vec<&StoredPost> = posts
        .iter()
        .filter(|s| s.post.kind == "suggest")
        .filter(|s| !redacted_ids.contains(s.id.as_str()))
        .collect();
    if !suggestions.is_empty() {
        out.push_str("\nsuggestions (promote via a PROC-* registry row):\n");
        for s in suggestions {
            out.push_str(&format!(
                "  [{}] {}\n",
                s.post.by,
                s.post.str_field("note").unwrap_or("(no note)")
            ));
        }
    }

    let unanswered: Vec<&StoredPost> = posts
        .iter()
        .filter(|s| s.post.kind == "ask")
        .filter(|s| !redacted_ids.contains(s.id.as_str()))
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

    // Reported, never suppressed: a redaction is an EVENT the ledger owes its
    // readers, which is the whole difference between this and a (prohibited,
    // and measured not to work) history rewrite. This is also where redaction
    // and retraction visibly stay distinct kinds of act — retraction gets no
    // section here, because "I withdraw this claim" is not a thing done TO
    // the board that a later reader must be told about.
    let redactions: Vec<&StoredPost> = posts.iter().filter(|s| s.post.kind == "redact").collect();
    if !redactions.is_empty() {
        out.push_str("\nredacted (body suppressed -- see Board::redact, D10):\n");
        for s in &redactions {
            out.push_str(&format!(
                "  [{}] redacted {}\n",
                s.post.by,
                s.post.str_field("post").unwrap_or("(unnamed target)")
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
    use crate::store::{Board, Origin};
    use serde_json::json;

    /// Point `refname` at a TREE rather than a commit -- the same
    /// reproduction `tests/resilience.rs`'s CLI-level twin uses. `git
    /// rev-parse --verify --quiet <ref>^{commit}` still writes to stderr and
    /// returns a genuine `Err` for this shape ("expected commit type, but
    /// the object dereferences to tree type"), not mere absence, which is
    /// what makes it a fail-loud reproduction rather than an empty-board one.
    fn corrupt_the_ref(repo: &crate::git::Repo, refname: &str) {
        let tree = repo.git(&["write-tree"]).expect("write-tree");
        repo.git(&["update-ref", refname, &tree])
            .expect("point the ref at a tree, not a commit");
    }

    #[test]
    fn the_digest_tallies_corroboration_per_technique() {
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let t = board
            .append(&Post::new("technique", "main").with("note", json!("mktree rejects slashes")))
            .expect("t");
        board
            .append(&Post::new("confirm", "campaign/x").with("post", json!(t.clone())))
            .expect("c1");
        board
            .append(&Post::new("confirm", "campaign/y").with("post", json!(t.clone())))
            .expect("c2");

        // Clock: read the tip's REAL commit time rather than a hardcoded
        // constant. `2_000_000_000` is 2033-05-18, so a 14-day window opens
        // seven years AFTER these posts are committed and `history`
        // correctly returns nothing -- Task 4 hit exactly that.
        // `SystemTime::now()` is not an option either: clippy's
        // `disallowed_types` fires. This mirrors the sibling tests already
        // in this module.
        let tip = board.tip().expect("tip").expect("some");
        let now_unix: u64 = repo
            .git(&["log", "-1", "--format=%ct", &tip])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let text = digest(&history(&board, 14, now_unix).expect("history"));
        assert!(
            text.contains('2'),
            "the corroboration count is the measurement; got {text}"
        );
        // Pinned against the corroboration line itself, not merely the
        // digest as a whole: the `by kind` table already prints a bare "2"
        // for two `confirm` posts, which would satisfy the assertion above
        // even with no corroboration tally implemented at all.
        assert!(
            text.contains("corroboration: confirmed 2, stale 0"),
            "the tally must attach to the technique it corroborates: {text}"
        );
    }

    #[test]
    fn the_digest_fails_loud_where_the_ambient_render_stays_quiet() {
        // The asymmetry IS the property (B12): the digest is the instrument
        // that would report the board being broken, so a quiet digest makes
        // board defects invisible by construction. Asserted in one test so
        // the next change to error handling cannot flatten them separately.
        let (_dir, repo) = temp_repo();
        let board = Board::new(repo.clone());
        corrupt_the_ref(&repo, board.refname()); // point the ref at a non-commit
        assert!(history(&board, 14, 0).is_err(), "the digest must fail loud");
        // and the ambient path must not lie about it either:
        assert!(
            Board::new(repo.clone()).posts_at_tip().is_err(),
            "the read reports the error"
        );
        // main.rs's render arm swallows this Err and returns quietly rather
        // than propagating it -- both library functions here still fail
        // loud, which is what the CLI's ambient path relies on being able
        // to *choose* to swallow, rather than never seeing the error at all.
    }

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
    fn the_digest_reports_that_a_redaction_happened_without_the_body() {
        // B8's third property: the act is reported, the body is not. History
        // keeps the post (D13), so `history` still returns it -- suppression
        // is `digest`'s job, not `history`'s.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let id = board
            .append(
                &Post::new("technique", "campaign/x").with("note", json!("SENSITIVE-BODY-TEXT")),
            )
            .expect("id");
        board.redact("main", &id).expect("redact");

        // Clock: read the tip's REAL commit time rather than a hardcoded
        // constant. `2_000_000_000` is 2033-05-18, so a 14-day window opens
        // seven years AFTER these posts are committed and `history`
        // correctly returns nothing -- Task 4 hit exactly that.
        // `SystemTime::now()` is not an option either: clippy's
        // `disallowed_types` fires (decision 0001's no-wall-clock ban
        // reaching this crate).
        let tip = board.tip().expect("tip").expect("some");
        let now_unix: u64 = repo
            .git(&["log", "-1", "--format=%ct", &tip])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let text = digest(&history(&board, 14, now_unix).expect("history"));
        assert!(
            !text.contains("SENSITIVE-BODY-TEXT"),
            "the body survived the redaction: {text}"
        );
        assert!(
            text.contains("redacted"),
            "the act must still be recorded; got {text}"
        );
        // Against the REDACTION LINE, not merely against the whole text.
        // `text.contains("main")` was already satisfied by the `by author:`
        // block, so it stayed green with the redaction line's author
        // replaced by a literal -- an assertion that cannot fail is not
        // testing the property it names.
        assert!(
            text.contains(&format!("[main] redacted {id}")),
            "the redaction line itself must name WHO redacted WHAT: {text}"
        );
    }

    #[test]
    fn a_redacted_ask_keeps_its_act_reported_but_loses_its_body() {
        // The `ask` section has its own suppression filter, and deleting it
        // left the whole suite green -- every other redaction test targets a
        // `technique`. One test per body-bearing section, because
        // `redacted_ids` being shared does not make a section that forgets
        // to call it fail anywhere else.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        // No `thread` field: an ask that can never correlate a reply is
        // always unanswered, so both of these are guaranteed to reach the
        // section under test.
        let secret = board
            .append(&Post::new("ask", "campaign/x").with("note", json!("SENSITIVE-QUESTION")))
            .expect("secret ask");
        board
            .append(&Post::new("ask", "campaign/y").with("note", json!("ORDINARY-QUESTION")))
            .expect("ordinary ask");
        board.redact("main", &secret).expect("redact");

        let tip = board.tip().expect("tip").expect("some");
        let now_unix: u64 = repo
            .git(&["log", "-1", "--format=%ct", &tip])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let text = digest(&history(&board, 14, now_unix).expect("history"));

        assert!(
            text.contains("asks with no reply"),
            "the section must actually render, or the assertion below is vacuous: {text}"
        );
        assert!(
            text.contains("ORDINARY-QUESTION"),
            "an unredacted ask still shows its body: {text}"
        );
        assert!(
            !text.contains("SENSITIVE-QUESTION"),
            "a redacted ask's body must not survive into the unanswered section: {text}"
        );
        assert!(
            text.contains(&format!("[main] redacted {secret}")),
            "and the act is still reported: {text}"
        );
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
                origin: Origin::Local,
            },
            StoredPost {
                id: "b".into(),
                post: Post::new("technique", "campaign/y").with("note", json!("t2")),
                committed_at: 20,
                origin: Origin::Local,
            },
            StoredPost {
                id: "c".into(),
                post: Post::new("claim", "campaign/x"),
                committed_at: 30,
                origin: Origin::Local,
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
            origin: Origin::Local,
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
                origin: Origin::Local,
            },
            StoredPost {
                id: "b".into(),
                post: Post::new("reply", "campaign/y").with("thread", json!("t1")),
                committed_at: 20,
                origin: Origin::Local,
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
            origin: Origin::Local,
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
                origin: Origin::Local,
            },
            StoredPost {
                id: "reply1".into(),
                post: Post::new("reply", "campaign/y").with("thread", json!("t1")),
                committed_at: 20,
                origin: Origin::Local,
            },
            StoredPost {
                id: "ask2".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("second question")),
                committed_at: 30,
                origin: Origin::Local,
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
                origin: Origin::Local,
            },
            StoredPost {
                id: "ask1".into(),
                post: Post::new("ask", "campaign/x")
                    .with("thread", json!("t1"))
                    .with("note", json!("asked after the only reply")),
                committed_at: 100,
                origin: Origin::Local,
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
    fn history_unions_peer_refs_so_the_human_view_does_not_lag_the_ambient_one() {
        // D14 is the reason this matters more than symmetry: `make
        // board-digest` is the seam Nathan reads the board through. Left
        // single-ref, the human view would show only this host's history
        // while `board read` showed the union -- and a reader would have no
        // way to tell that a whole machine's posts were missing.
        //
        // Peer name derived from the real host, never a literal: this suite
        // runs on lefford too, where a hardcoded "lefford" names THIS host's
        // own mirror and is correctly skipped.
        let (_d, repo) = temp_repo();
        let board = Board::new(repo.clone());
        let mine = board
            .append(&Post::new("technique", "main").with("note", json!("mine, locally")))
            .expect("mine");
        let peer = Board::with_ref(
            repo.clone(),
            &format!(
                "{}{}-peer",
                Board::PEERS_PREFIX,
                crate::live::current_host()
            ),
        );
        let theirs = peer
            .append(&Post::new("technique", "main").with("note", json!("theirs, on the peer")))
            .expect("theirs");
        // The same technique published independently on both hosts: it must
        // appear ONCE, or the digest's counts and its technique list would
        // double-count every shared post.
        let shared = Post::new("technique", "main").with("note", json!("published on both"));
        let shared_id = board.append(&shared).expect("shared here");
        assert_eq!(
            peer.append(&shared).expect("shared there"),
            shared_id,
            "content addressing must make the independent copies one id"
        );

        let now_unix: u64 = repo
            .git(&[
                "log",
                "-1",
                "--format=%ct",
                &board.tip().expect("tip").expect("some"),
            ])
            .expect("commit time")
            .parse()
            .expect("timestamp");
        let posts = history(&board, 14, now_unix).expect("history");

        let ids: Vec<&str> = posts.iter().map(|p| p.id.as_str()).collect();
        assert!(ids.contains(&mine.as_str()), "local post missing: {ids:?}");
        assert!(ids.contains(&theirs.as_str()), "peer post missing: {ids:?}");
        assert_eq!(
            ids.iter().filter(|i| **i == shared_id).count(),
            1,
            "the shared post must be counted once, not per host: {ids:?}"
        );
        assert_eq!(
            ids.len(),
            3,
            "three distinct posts across two logs: {ids:?}"
        );
        assert_eq!(
            posts
                .iter()
                .find(|p| p.id == theirs)
                .map(|p| p.origin.clone()),
            Some(Origin::Peer(format!(
                "{}-peer",
                crate::live::current_host()
            ))),
            "and the digest must know which host a post came from"
        );

        let text = digest(&posts);
        assert!(
            text.contains("theirs, on the peer"),
            "the human view must render the peer's technique in full: {text}"
        );
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
