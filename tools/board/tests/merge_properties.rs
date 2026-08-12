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
    let ia = Board::with_ref(repo.clone(), "refs/test/ia");
    let ib = Board::with_ref(repo.clone(), "refs/test/ib");
    let id_a = ia.append(&same).expect("ia post");
    let id_b = ib.append(&same).expect("ib post");
    // Q9: establish the property on each side before merging, not just on
    // the merged result — a no-op append on either side would also leave
    // the merge at 2 entries, so the merge count alone does not prove both
    // appends actually landed.
    assert_eq!(
        id_a, id_b,
        "identical content must produce identical ids on both sides"
    );
    assert_eq!(
        ia.post_ids_at_tip().expect("ia ids").len(),
        2,
        "ia should hold the shared base post plus this one"
    );
    assert_eq!(
        ib.post_ids_at_tip().expect("ib ids").len(),
        2,
        "ib should hold the shared base post plus this one"
    );

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

/// The read-side counterpart of the two arms above (B1). They establish that
/// two divergent logs *could* be merged into one tree without conflict; this
/// asserts that the board never needs to, because a read unions the refs
/// instead — each host keeps writing only its own log, and what a session
/// sees is the union of the local log with every peer mirror it has fetched.
///
/// Deliberately not `merge-tree`: merging is exactly what B1 declines to do.
/// One writer per ref is what keeps the compare-and-swap correct across
/// machines and what makes a reap terminal for the log that made it — under a
/// merge design, the host that reaps a post has it resurrected by the next
/// fetch from the host that did not, forever.
#[test]
fn a_union_read_sees_both_clones_posts_once_each_is_fetched_into_its_own_peer_ref() {
    let (_d, repo) = temp_repo("union-read");
    // Two hosts' logs, built exactly as a fetch of each host's own
    // `refs/hornvale/board` would land them: one ref per host, no merge.
    let peers_prefix = Board::PEERS_PREFIX;
    // Peer names derived from the real host, never literals: this suite runs
    // on lefford too, where a hardcoded "lefford" would name THIS host's own
    // mirror, be correctly skipped by the union, and fail this test for a
    // reason unrelated to what it checks.
    let host = board::live::current_host();
    let (peer_a, peer_b) = (format!("{host}-peer-a"), format!("{host}-peer-b"));
    let local = Board::new(repo.clone());
    let mine = local
        .append(&Post::new("technique", "campaign/here"))
        .expect("local post");
    let shared = Post::new("technique", "campaign/both");
    let mine_shared = local.append(&shared).expect("shared, locally");

    let first_peer = Board::with_ref(repo.clone(), &format!("{peers_prefix}{peer_a}"));
    let theirs = first_peer
        .append(&Post::new("notice", "campaign/there"))
        .expect("peer post");
    // The same technique, published independently on both hosts.
    let theirs_shared = first_peer.append(&shared).expect("shared, on the peer");
    assert_eq!(
        mine_shared, theirs_shared,
        "content addressing must make the independently-posted duplicate one id"
    );

    let second_peer = Board::with_ref(repo.clone(), &format!("{peers_prefix}{peer_b}"));
    let third = second_peer
        .append(&Post::new("ask", "campaign/elsewhere"))
        .expect("third host post");

    let ids: Vec<String> = local
        .posts_at_tip()
        .expect("union read")
        .into_iter()
        .map(|s| s.id)
        .collect();
    for (label, id) in [
        ("this host's own post", &mine),
        ("the peer's post", &theirs),
        ("a second peer's post", &third),
        ("the post both hosts published", &mine_shared),
    ] {
        assert!(
            ids.contains(id),
            "{label} is missing from the union: {ids:?}"
        );
    }
    assert_eq!(
        ids.iter().filter(|i| **i == mine_shared).count(),
        1,
        "the duplicate must collapse to one entry: {ids:?}"
    );
    assert_eq!(
        ids.len(),
        4,
        "four distinct posts across three logs: {ids:?}"
    );

    // And the union is a READ: neither log was rewritten to produce it.
    assert_eq!(
        local.post_ids_at_tip().expect("id union").len(),
        4,
        "the id read must union identically -- the cursor prunes against it"
    );
    let tip_of = |b: &Board| b.tip().expect("tip").expect("some");
    let listed = |b: &Board| {
        repo.git(&["ls-tree", "-r", "--name-only", &tip_of(b)])
            .expect("ls-tree")
            .lines()
            .count()
    };
    assert_eq!(listed(&local), 2, "the local log holds only its own posts");
    assert_eq!(listed(&first_peer), 2, "and the peer's, only its own");
    assert_eq!(
        listed(&second_peer),
        1,
        "and the third host's, only its own"
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
    // I4: `Repo::git` discards stdout on failure, and that is exactly where
    // git puts the one diagnostic (`CONFLICT (content)`) that tells us THIS
    // failed for the reason the control exists to demonstrate, rather than
    // for some unrelated reason (a bad object id, a dropped flag, a renamed
    // subcommand) that would also exit non-zero and leave the control
    // silently no longer watching anything.
    //
    // `git_output` rather than a bare `Command::new("git")`: this used to
    // spawn git directly, which meant it was the one call in the suite that
    // did NOT get `Repo::command`'s environment scrub, and so would still
    // have run against a hook's inherited `GIT_DIR` (see `git.rs`'s module
    // docs). Asserting on git's own behaviour does not require bypassing the
    // one constructor that makes the target unambiguous.
    let out = repo
        .git_output(&["merge-tree", "--write-tree", &a, &b])
        .expect("spawn git");
    assert!(
        !out.status.success(),
        "the shared-file shape must still conflict; if it does not, revisit D11"
    );
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(
        stdout.contains("CONFLICT (content)"),
        "must fail because of a content conflict, not some other reason: {stdout}"
    );
    assert!(
        stdout.contains("register.jsonl"),
        "the conflict must be on the shared register file: {stdout}"
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

    // Q11/spec §7 test-plan item 1: "all N posts present, chain length N+1".
    // D6 correction: this does NOT establish that a retry happened -- nine
    // appends produce a chain of nine commits whether or not any of them
    // contended (each successful append adds exactly one commit either way).
    // What it *does* establish, on top of the `present.len()` check above:
    // linearity (no stray merge or branch), and that no append silently
    // produced an empty commit (Q7's regression would still show 9 distinct
    // posts but a chain longer than 9). Direct evidence that the retry path
    // itself runs lives in `store.rs`'s `cas_reports_a_lost_race_...` and
    // `exhaustion_is_loud_...` unit tests, not here.
    let tip = board.tip().expect("tip").expect("some");
    let chain_len = repo.git(&["rev-list", "--count", &tip]).expect("rev-list");
    assert_eq!(
        chain_len, "9",
        "genesis plus eight appends must form one linear chain of length 9"
    );
}
