//! Spec test-plan item 9, at the seam that matters: **one corrupt post must
//! never silence a session's whole render** (D7).
//!
//! The unit tests in `store.rs` and `digest.rs` cover the skip-and-warn arms
//! themselves. These drive the actual binary, because D7's promise is about a
//! *session*, and three of its four properties are only observable from
//! outside the library: the process exits zero, the good post still reaches
//! stdout, and the corruption is announced on stderr rather than swallowed.
//!
//! A corrupt post cannot be produced by this tool — `Post::canonical_bytes`
//! gates every write — so each test splices raw bytes into the tip tree the
//! way a clone, an older version of the tool, or a hand write would.

use board::git::Repo;
use board::post::Post;
use board::store::Board;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::{AtomicU32, Ordering};

static N: AtomicU32 = AtomicU32::new(0);

/// A throwaway repo with a commit on `main`, so `changed_paths`' diff against
/// `refs/heads/main` resolves the way it does in a real worktree.
fn temp_repo(tag: &str) -> (PathBuf, Repo) {
    let n = N.fetch_add(1, Ordering::SeqCst);
    let dir = std::env::temp_dir().join(format!(
        "hv-board-resilience-{}-{tag}-{n}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("dir");
    let repo = Repo::new(&dir);
    repo.git(&["init", "--quiet", "--initial-branch=main"])
        .expect("init");
    repo.git(&["config", "user.email", "board@test"])
        .expect("email");
    repo.git(&["config", "user.name", "board test"])
        .expect("name");
    std::fs::write(dir.join("root.txt"), "root").expect("write");
    repo.git(&["add", "root.txt"]).expect("add");
    repo.git(&["commit", "-q", "-m", "root"]).expect("commit");
    (dir, repo)
}

/// Splice `bytes` into the board's tip tree as a post file, forward-only,
/// bypassing `append`.
fn splice_raw_post(board: &Board, repo: &Repo, bytes: &[u8]) -> String {
    let blob = repo.hash_object(bytes).expect("hash-object");
    let old = board.tip().expect("tip").expect("some");
    let n = N.fetch_add(1, Ordering::SeqCst);
    let index = repo
        .git_path(&format!("hv-splice-{}-{n}", std::process::id()))
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
    blob
}

/// Run the board binary in `repo`, returning (exit code, stdout, stderr).
fn run(repo: &Repo, args: &[&str]) -> (Option<i32>, String, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_board"))
        .current_dir(repo.root())
        .args(args)
        .output()
        .expect("spawn board");
    (
        out.status.code(),
        String::from_utf8_lossy(&out.stdout).to_string(),
        String::from_utf8_lossy(&out.stderr).to_string(),
    )
}

#[test]
fn a_corrupt_post_does_not_stop_read_from_rendering_the_rest() {
    let (_d, repo) = temp_repo("read");
    let board = Board::new(repo.clone());
    board
        .append(&Post::new("technique", "campaign/x").with("note", serde_json::json!("survivor")))
        .expect("good post");
    splice_raw_post(&board, &repo, b"absolutely not json\n");

    let (code, stdout, stderr) = run(&repo, &["read"]);
    assert_eq!(
        code,
        Some(0),
        "a corrupt post must not fail the read: {stderr}"
    );
    assert!(
        stdout.contains("survivor"),
        "the good post must still render: {stdout:?}"
    );
    assert!(
        stderr.contains("skipping malformed post"),
        "and the corruption must be ANNOUNCED, not swallowed: {stderr:?}"
    );
}

#[test]
fn a_corrupt_post_does_not_stop_the_ambient_render_or_advance_past_the_good_one() {
    let (_d, repo) = temp_repo("render");
    let board = Board::new(repo.clone());
    board
        .append(
            &Post::new("notice", "campaign/never-existed")
                .with("polarity", serde_json::json!("hold-off"))
                .with("note", serde_json::json!("survivor")),
        )
        .expect("good post");
    // A `technique` is durable regardless of branch liveness, so the ambient
    // render has something to show in a repo with no campaign branches.
    board
        .append(&Post::new("technique", "campaign/x").with("note", serde_json::json!("durable")))
        .expect("technique");
    splice_raw_post(&board, &repo, b"[not, a, post]\n");

    let (code, stdout, stderr) = run(&repo, &["render"]);
    assert_eq!(
        code,
        Some(0),
        "the ambient render may never break a session: {stderr}"
    );
    assert!(
        stdout.contains("durable"),
        "the good post must still reach the session: {stdout:?}"
    );
    assert!(
        stderr.contains("skipping malformed post"),
        "and the corruption must be announced: {stderr:?}"
    );
}

#[test]
fn a_corrupt_post_does_not_stop_the_digest() {
    let (_d, repo) = temp_repo("digest");
    let board = Board::new(repo.clone());
    board
        .append(&Post::new("technique", "campaign/x").with("note", serde_json::json!("survivor")))
        .expect("good post");
    splice_raw_post(&board, &repo, b"\x00\x01 binary garbage\n");

    let (code, stdout, stderr) = run(&repo, &["digest", "3650"]);
    assert_eq!(
        code,
        Some(0),
        "the human read seam must not break: {stderr}"
    );
    assert!(
        stdout.contains("survivor"),
        "the digest must still show the good technique: {stdout:?}"
    );
    assert!(
        stderr.contains("skipping malformed post"),
        "and say what it skipped: {stderr:?}"
    );
}

#[test]
fn a_non_numeric_ttl_is_warned_about_at_post_time() {
    // I8: `ttl_s=900s` is a legal post (D12) that will never decay. The typo
    // must not be silent at the one moment it is cheap to fix.
    let (_d, repo) = temp_repo("ttl");
    let (code, stdout, stderr) = run(
        &repo,
        &["post", "claim", "campaign/x", "ttl_s=900s", "pid=1"],
    );
    assert_eq!(code, Some(0), "the post is still accepted: {stderr}");
    assert!(!stdout.trim().is_empty(), "it prints the post id");
    assert!(
        stderr.contains("ttl_s") && stderr.contains("not a number"),
        "a non-numeric ttl_s must be warned about: {stderr:?}"
    );
    assert!(
        !stderr.contains("`pid` is not a number"),
        "a well-formed pid must not be warned about: {stderr:?}"
    );
}

#[test]
fn reap_refuses_to_run_rather_than_reap_against_an_unreadable_board() {
    // C2's second half. `posts_at_tip().unwrap_or_default()` used to turn a
    // read failure into an empty post set, which probes as "nothing is live"
    // -- the maximally destructive reading, in the only operation whose
    // consequences are permanent. Reap must refuse instead.
    let dir = std::env::temp_dir().join(format!(
        "hv-board-not-a-repo-{}-{}",
        std::process::id(),
        N.fetch_add(1, Ordering::SeqCst)
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("dir");
    let out = Command::new(env!("CARGO_BIN_EXE_board"))
        .current_dir(&dir)
        .arg("reap")
        .output()
        .expect("spawn board");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert_eq!(
        out.status.code(),
        Some(1),
        "a reap that cannot read the board must fail, not report success: {stderr}"
    );
    assert!(
        stderr.contains("refusing to reap"),
        "and say so in those terms: {stderr:?}"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
