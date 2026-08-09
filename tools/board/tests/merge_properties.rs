//! The merge properties from the spec's §7. These are the reason posts are
//! one-file-per-post rather than lines in a shared register (D8, D11).

use board::git::Repo;
use board::post::Post;
use board::store::Board;

fn temp_repo(tag: &str) -> (std::path::PathBuf, Repo) {
    let dir = std::env::temp_dir().join(format!("hv-board-merge-{}-{}", std::process::id(), tag));
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

/// Two divergent boards that recorded DIFFERENT posts merge with no conflict.
#[test]
fn divergent_boards_union_without_conflict() {
    let (_d, repo) = temp_repo("union");
    let base = Board::with_ref(repo.clone(), "refs/test/base");
    base.append(&Post::new("claim", "shared"))
        .expect("base post");
    let base_tip = base.tip().expect("tip").expect("some");

    repo.git(&["update-ref", "refs/test/a", &base_tip])
        .expect("branch a");
    repo.git(&["update-ref", "refs/test/b", &base_tip])
        .expect("branch b");
    Board::with_ref(repo.clone(), "refs/test/a")
        .append(&Post::new("notice", "campaign/a"))
        .expect("a post");
    Board::with_ref(repo.clone(), "refs/test/b")
        .append(&Post::new("notice", "campaign/b"))
        .expect("b post");

    let tree = repo
        .git(&["merge-tree", "--write-tree", "refs/test/a", "refs/test/b"])
        .expect("merge-tree must succeed with no conflict");
    let listed = repo
        .git(&[
            "ls-tree",
            "-r",
            "--name-only",
            tree.lines().next().expect("tree"),
        ])
        .expect("ls-tree");
    assert_eq!(
        listed.lines().filter(|l| l.starts_with("posts/")).count(),
        3,
        "all three posts survive the merge:\n{listed}"
    );
}

/// Two boards that independently recorded the SAME post converge.
#[test]
fn identical_posts_recorded_independently_are_idempotent() {
    let (_d, repo) = temp_repo("idem");
    let base = Board::with_ref(repo.clone(), "refs/test/ibase");
    base.append(&Post::new("claim", "shared")).expect("base");
    let base_tip = base.tip().expect("tip").expect("some");
    repo.git(&["update-ref", "refs/test/ia", &base_tip])
        .expect("ia");
    repo.git(&["update-ref", "refs/test/ib", &base_tip])
        .expect("ib");

    let same = Post::new("technique", "campaign/x");
    Board::with_ref(repo.clone(), "refs/test/ia")
        .append(&same)
        .expect("ia post");
    Board::with_ref(repo.clone(), "refs/test/ib")
        .append(&same)
        .expect("ib post");

    let tree = repo
        .git(&["merge-tree", "--write-tree", "refs/test/ia", "refs/test/ib"])
        .expect("merge-tree must succeed");
    let listed = repo
        .git(&[
            "ls-tree",
            "-r",
            "--name-only",
            tree.lines().next().expect("tree"),
        ])
        .expect("ls-tree");
    assert_eq!(
        listed.lines().filter(|l| l.starts_with("posts/")).count(),
        2,
        "the duplicate collapses to one file:\n{listed}"
    );
}

/// The control: the rejected shape. Two clones appending to one shared
/// line-oriented file CONFLICT. If this test ever passes, the premise behind
/// D11 has changed and the decision should be revisited.
#[test]
fn a_shared_append_only_file_conflicts_which_is_why_we_do_not_use_one() {
    let (_d, repo) = temp_repo("control");
    let write = |content: &str| repo.hash_object(content.as_bytes()).expect("blob");
    let tree_of = |blob: &str| {
        repo.git_stdin(
            &["mktree"],
            format!("100644 blob {blob}\tregister.jsonl\n").as_bytes(),
        )
        .expect("mktree")
    };
    let base = repo
        .git(&["commit-tree", &tree_of(&write("post-0\n")), "-m", "base"])
        .expect("base");
    let a = repo
        .git(&[
            "commit-tree",
            &tree_of(&write("post-0\npost-A\n")),
            "-p",
            &base,
            "-m",
            "a",
        ])
        .expect("a");
    let b = repo
        .git(&[
            "commit-tree",
            &tree_of(&write("post-0\npost-B\n")),
            "-p",
            &base,
            "-m",
            "b",
        ])
        .expect("b");
    assert!(
        repo.git(&["merge-tree", "--write-tree", &a, &b]).is_err(),
        "the shared-file shape must still conflict; if it does not, revisit D11"
    );
}

/// Eight concurrent writers must lose nothing. This is the ported form of the
/// probe recorded in the spec's §7 fact 4.
#[test]
fn eight_concurrent_writers_lose_no_posts() {
    let (_d, repo) = temp_repo("concurrency");
    let board = Board::with_ref(repo.clone(), "refs/test/concurrent");
    board
        .append(&Post::new("claim", "genesis"))
        .expect("genesis");

    let handles: Vec<_> = (0..8)
        .map(|i| {
            let b = Board::with_ref(repo.clone(), "refs/test/concurrent");
            std::thread::spawn(move || {
                b.append(&Post::new("claim", &format!("writer-{i}")))
                    .expect("append")
            })
        })
        .collect();
    let ids: Vec<String> = handles
        .into_iter()
        .map(|h| h.join().expect("thread"))
        .collect();

    let present = board.post_ids_at_tip().expect("ids");
    for id in &ids {
        assert!(present.contains(id), "post {id} was lost under contention");
    }
    assert_eq!(present.len(), 9, "genesis plus eight writers");
}
