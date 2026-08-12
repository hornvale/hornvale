//! Redaction at the AMBIENT seam — the render every session sees at start.
//!
//! B8/D10 splits redaction into two halves with different scopes, and this
//! file exists because the wider half was the one nothing tested. Digest
//! suppression is board-wide (the `redact` control post propagates, and
//! `redacted_ids` is computed over the union history). Tip eviction is
//! **per-log**: `Board::redact` can only rewrite the tree of the ref it
//! holds. Wherever eviction cannot reach, `render`/`read` had no redaction
//! filter of their own at all — so the control post rendered as content,
//! next to the body it was meant to suppress, naming it by id. A signpost
//! pointing at the secret is strictly worse than not redacting.
//!
//! Two cases put a post beyond this host's eviction, and both are exercised
//! below against the exact composition `main.rs`'s `render` arm uses
//! (`posts_at_tip` → `LiveContext::probe` → `live_posts` → `render`):
//!
//!   1. the post exists ONLY in a peer's log; and
//!   2. two hosts independently authored identical bytes, so one id sits in
//!      two logs (D11's content addressing) and evicting the local copy
//!      leaves the peer's.
//!
//! Each case renders an unrelated third post as well, so a passing assertion
//! means "the render worked and suppressed these two" rather than the
//! vacuous "the render produced nothing".

use board::git::Repo;
use board::live::LiveContext;
use board::post::Post;
use board::render::{RenderOptions, live_posts, render};
use board::store::Board;
use serde_json::json;

fn temp_repo(tag: &str) -> (std::path::PathBuf, Repo) {
    let dir = std::env::temp_dir().join(format!("hv-board-redact-{}-{}", std::process::id(), tag));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("dir");
    let repo = Repo::new(&dir);
    repo.git(&["init", "--quiet", "--initial-branch=main"])
        .expect("init");
    repo.git(&["config", "user.email", "board@test"])
        .expect("email");
    repo.git(&["config", "user.name", "board test"])
        .expect("name");
    (dir, repo)
}

/// A host name that is never this one, however either machine is renamed —
/// the same derivation `store.rs`'s union tests use, and for the same reason:
/// a hardcoded literal names THIS host's own mirror when the suite runs on
/// that host, where the union correctly skips it.
fn foreign_host() -> String {
    format!("{}-peer", board::live::current_host())
}

fn peer_board(repo: &Repo, host: &str) -> Board {
    Board::with_ref(repo.clone(), &format!("{}{host}", Board::PEERS_PREFIX))
}

/// The ambient render, composed exactly as `main.rs`'s `render` arm composes
/// it, minus the cursor (which decides what is UNSEEN, not what is
/// suppressed — an orthogonal filter this file is not about).
fn ambient(repo: &Repo, board: &Board) -> String {
    let posts = board.posts_at_tip().expect("read");
    let ctx = LiveContext::probe(repo, &posts).expect("probe");
    let live = live_posts(&posts, &ctx);
    render(&live, 0, &RenderOptions::session_start())
}

#[test]
fn a_post_only_a_peer_holds_is_suppressed_at_the_ambient_seam() {
    let (_d, repo) = temp_repo("peer-only");
    let board = Board::new(repo.clone());
    let peer = peer_board(&repo, &foreign_host());

    // Beyond this host's eviction by construction: `Board::redact` rewrites
    // the tree of ITS ref, and this post is not in that ref at all.
    let secret = peer
        .append(&Post::new("technique", "campaign/b").with("note", json!("SECRET-BRAVO")))
        .expect("peer post");
    board
        .append(&Post::new("technique", "campaign/a").with("note", json!("VISIBLE-CONTROL")))
        .expect("control post");

    board.redact("campaign/a", &secret).expect("redact");

    let out = ambient(&repo, &board);
    assert!(
        out.contains("VISIBLE-CONTROL"),
        "the render must actually be rendering, or the assertions below are vacuous: {out:?}"
    );
    assert!(
        !out.contains("SECRET-BRAVO"),
        "a redacted body must not survive at the ambient seam merely because eviction \
         could not reach the peer's log: {out}"
    );
    assert!(
        !out.contains(&secret),
        "the `redact` control post must not render as content -- it names the target by \
         id, so rendering it signposts exactly what it was meant to suppress: {out}"
    );
    assert!(
        !out.contains("[redact]"),
        "a control post is not content, however it renders: {out}"
    );
}

/// claim: structural(git-backed board plumbing test; no world seed loop —
/// the scanner's single-letter `s` closure-param heuristic false-fires)
#[test]
fn a_content_addressed_duplicate_in_two_logs_is_suppressed_at_the_ambient_seam() {
    let (_d, repo) = temp_repo("dup");
    let board = Board::new(repo.clone());
    let peer = peer_board(&repo, &foreign_host());

    // D11: the id IS the content hash, so two hosts that independently
    // authored the same bytes hold ONE id in TWO logs. Eviction reaches only
    // this host's copy; the peer's keeps the post in the union read.
    let same = Post::new("technique", "campaign/b").with("note", json!("SECRET-CHARLIE"));
    let mine = board.append(&same).expect("local");
    let theirs = peer.append(&same).expect("peer");
    assert_eq!(mine, theirs, "content addressing should make these one id");
    board
        .append(&Post::new("technique", "campaign/a").with("note", json!("VISIBLE-CONTROL")))
        .expect("control post");

    board.redact("campaign/a", &mine).expect("redact");
    assert!(
        !board
            .snapshot()
            .expect("snapshot")
            .expect("some")
            .posts()
            .iter()
            .any(|s| s.id == mine),
        "eviction should have removed OUR copy -- otherwise this test is not exercising \
         the duplicate case at all"
    );
    assert!(
        board.post_ids_at_tip().expect("ids").contains(&mine),
        "and the peer's copy should still be in the union -- the whole point of this case"
    );

    let out = ambient(&repo, &board);
    assert!(
        out.contains("VISIBLE-CONTROL"),
        "the render must actually be rendering, or the assertions below are vacuous: {out:?}"
    );
    assert!(
        !out.contains("SECRET-CHARLIE"),
        "a redacted body must not survive at the ambient seam merely because a peer \
         independently authored the same bytes: {out}"
    );
    assert!(
        !out.contains(&mine),
        "the `redact` control post must not render as content: {out}"
    );
    assert!(
        !out.contains("[redact]"),
        "a control post is not content, however it renders: {out}"
    );
}
