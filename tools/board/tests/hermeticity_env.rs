//! THE GUARD (The Beacon, Task 11b). This crate's own test suite once wrote
//! into the developer's real repository, and this file is what makes that
//! impossible to reintroduce silently.
//!
//! # What happened
//!
//! `scripts/hooks/pre-commit` runs `cargo test --manifest-path
//! tools/board/Cargo.toml` when every staged path is under `tools/board/`
//! (B13, decision 0129). Git runs a hook with `GIT_DIR` and `GIT_INDEX_FILE`
//! **exported**, and from a linked worktree — which is where all campaign
//! work happens — they are *absolute* paths into the real repository:
//!
//! ```text
//! GIT_DIR=/…/hornvale/.git/worktrees/the-beacon
//! GIT_INDEX_FILE=/…/hornvale/.git/worktrees/the-beacon/index
//! ```
//!
//! `GIT_DIR` outranks `git -C <dir>`. `-C` only sets the working directory;
//! it does not decide which repository git acts on. So every `git -C
//! <tempdir>` this suite makes operated on the real repository: `git init`
//! re-initialised it and guessed **bare** (a worktree gitdir does not end in
//! `/.git`), `git config` overwrote `user.name` with `board test`, the merge
//! helper landed `root`/`work`/`merge` commits on `main` and created
//! `campaign/*` branches, and `update-ref` left a dangling
//! `refs/hornvale/peers/dangling` that broke `git fetch` repository-wide.
//!
//! # What this guard asserts, and how it refuses to be vacuous
//!
//! It builds a **sentinel** repository standing in for the developer's own,
//! points the exact hook environment at it, runs this crate's write paths at
//! a *different* temp repository, and requires the sentinel to come out
//! byte-identical.
//!
//! Four separate assertions stop it passing for the wrong reason:
//!
//! 1. **The fingerprint must be substantive** before anything runs — it has
//!    to name the sentinel's own committer and refs, so an empty or
//!    all-`<error>` fingerprint cannot compare equal to itself and pass.
//! 2. **The poison must be live.** A deliberately *unscrubbed* `git` is run
//!    in a directory that is not a repository at all, and is required to
//!    succeed and report the sentinel — proving the environment really would
//!    have redirected a leaky call. If a later edit breaks the setup so
//!    `GIT_DIR` is not actually exported, this fails rather than quietly
//!    guarding nothing.
//! 3. **The scrub must bite**, asserted directly: the same probe through
//!    `Repo` must *fail*, because `-C <not-a-repo>` with a clean environment
//!    has no repository to find.
//! 4. **The writes must have landed somewhere.** Every exercised path is
//!    positively asserted against the work repository. A path that silently
//!    errored out would write nothing anywhere, leave the sentinel pristine,
//!    and otherwise sail through — so "the sentinel is unchanged" is never
//!    accepted on its own.
//!
//! This is the only `#[test]` in this file on purpose: it mutates the process
//! environment, and cargo runs the tests within one binary on parallel
//! threads. A second test here would race it. Each integration test file is
//! its own binary, so nothing else in the suite is affected.

use board::git::Repo;
use board::post::Post;
use board::store::{Board, ReapPlan};
use std::path::{Path, PathBuf};

/// Everything about a repository the incident actually moved: refs and what
/// they point at, HEAD, the whole commit graph with committers, `core.bare`,
/// the configured identity, and the working tree's cleanliness.
///
/// Read through `Repo`, whose scrub means `-C dir` genuinely reads `dir`. That
/// is not circular here: the poison points at the sentinel, so a *broken*
/// scrub would also read the sentinel. Either way this reports the sentinel's
/// true state.
fn fingerprint(dir: &Path) -> String {
    let repo = Repo::new(dir);
    let probes: [(&str, &[&str]); 6] = [
        (
            "refs",
            &[
                "for-each-ref",
                "--format=%(refname) %(objectname) %(objecttype)",
            ],
        ),
        ("head", &["rev-parse", "HEAD"]),
        ("graph", &["log", "--all", "--format=%H %cn <%ce> %s"]),
        ("bare", &["config", "--get", "core.bare"]),
        ("identity", &["config", "--get", "user.name"]),
        ("worktree", &["status", "--porcelain"]),
    ];
    let mut out = String::new();
    for (label, args) in probes {
        let value = repo
            .git(args)
            .unwrap_or_else(|e| format!("<error: {e}>"))
            .replace('\n', "\n        ");
        out.push_str(&format!("{label}: {value}\n"));
    }
    out
}

/// A repository with an identity and a history that are unmistakably not this
/// suite's, so any board write landing in it is obvious in the diff.
fn sentinel_repo(dir: &Path) -> Repo {
    std::fs::create_dir_all(dir).expect("sentinel dir");
    let repo = Repo::new(dir);
    repo.git(&["init", "--quiet", "--initial-branch=main"])
        .expect("sentinel init");
    repo.git(&["config", "user.email", "dev@example.invalid"])
        .expect("sentinel email");
    repo.git(&["config", "user.name", "Real Developer"])
        .expect("sentinel name");
    std::fs::write(dir.join("the-developers-work.txt"), "do not touch\n").expect("sentinel file");
    repo.git(&["add", "the-developers-work.txt"])
        .expect("sentinel add");
    repo.git(&["commit", "-q", "-m", "the developer's own commit"])
        .expect("sentinel commit");
    repo
}

fn unique_dir(tag: &str) -> PathBuf {
    let dir =
        std::env::temp_dir().join(format!("hv-board-hermeticity-{}-{tag}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("temp dir");
    dir
}

#[test]
fn a_hooks_inherited_git_dir_cannot_redirect_this_crate_at_another_repository() {
    let base = unique_dir("sentinel");
    let sentinel_dir = base.join("sentinel");
    let work_dir = base.join("work");
    let nowhere = base.join("nowhere");
    std::fs::create_dir_all(&work_dir).expect("work dir");
    std::fs::create_dir_all(&nowhere).expect("nowhere dir");

    // The sentinel, plus a linked worktree — the shape that made the incident
    // maximally destructive, because a worktree gitdir does not end in `/.git`
    // and so `git init` guesses `core.bare = true` for it.
    let sentinel = sentinel_repo(&sentinel_dir);
    let sentinel_wt = base.join("sentinel-wt");
    sentinel
        .git(&[
            "worktree",
            "add",
            "-q",
            sentinel_wt.to_str().expect("utf8"),
            "-b",
            "campaign/the-developers-branch",
        ])
        .expect("sentinel worktree");
    let poisoned_git_dir = sentinel_dir
        .join(".git")
        .join("worktrees")
        .join("sentinel-wt");
    assert!(
        poisoned_git_dir.is_dir(),
        "the worktree gitdir must exist for this to be the real shape: {poisoned_git_dir:?}"
    );

    // (1) The fingerprint must actually say something.
    let before = fingerprint(&sentinel_dir);
    for expected in [
        "Real Developer",
        "refs/heads/main",
        "refs/heads/campaign/the-developers-branch",
        "the developer's own commit",
    ] {
        assert!(
            before.contains(expected),
            "the sentinel fingerprint must be substantive — missing {expected:?}:\n{before}"
        );
    }

    // Exactly — and only — what git 2.50.1 exports to a pre-commit hook run
    // from a linked worktree, verified empirically. Fidelity matters here in
    // both directions. Adding `GIT_WORK_TREE`, which a hook does NOT export,
    // makes the leak fail *earlier* and more obviously (`git add` cannot find
    // its pathspec in the wrong tree), which would hide the far worse real
    // behaviour: with `GIT_DIR` alone, git takes the cwd as the work tree, so
    // every command **succeeds** and quietly commits into the other
    // repository. The variables a hook does not export are covered instead by
    // `hermeticity.rs`'s scrub-list test.
    //
    // `GIT_CONFIG_PARAMETERS` is included because it IS hook-exported whenever
    // the outer command used `-c` (`git -c user.name=… commit`), and it
    // outranks repo-local config. It cannot redirect the repository, so it does
    // not change what the fingerprint below proves — it is here so the
    // identity assertion further down is made against a genuinely poisoned
    // process, which is something `hermeticity.rs` cannot do (`.env` on a child
    // poisons only that child).
    //
    // SAFETY: this is the only `#[test]` in this test binary (see the module
    // docs), so no other thread is reading the environment concurrently.
    unsafe {
        std::env::set_var("GIT_DIR", &poisoned_git_dir);
        std::env::set_var("GIT_INDEX_FILE", poisoned_git_dir.join("index"));
        std::env::set_var("GIT_PREFIX", "");
        std::env::set_var(
            "GIT_CONFIG_PARAMETERS",
            "'user.name'='Injected Identity' 'user.email'='injected@evil'",
        );
        std::env::set_var("GIT_AUTHOR_NAME", "Real Developer");
        std::env::set_var("GIT_AUTHOR_EMAIL", "dev@example.invalid");
    }

    // (2) THE POISON MUST BE LIVE. Deliberately unscrubbed — the one place in
    // this crate that spawns git without going through `Repo::command`, and it
    // exists precisely to demonstrate that the environment this test builds
    // really would redirect a call that forgot to scrub. If this stops
    // succeeding, the guard is no longer guarding anything and must fail.
    let unscrubbed = std::process::Command::new("git")
        .arg("-C")
        .arg(&nowhere)
        .args(["rev-parse", "--absolute-git-dir"])
        .output()
        .expect("spawn git");
    let seen = String::from_utf8_lossy(&unscrubbed.stdout)
        .trim()
        .to_string();
    assert!(
        unscrubbed.status.success(),
        "the control must succeed: an inherited GIT_DIR is what lets git run \
         outside a repository at all. If this fails, the poison is not live \
         and the rest of this test proves nothing. stderr: {}",
        String::from_utf8_lossy(&unscrubbed.stderr)
    );
    assert_eq!(
        Path::new(&seen).canonicalize().ok(),
        poisoned_git_dir.canonicalize().ok(),
        "the control must land on the SENTINEL, not on {nowhere:?} — that is \
         what makes this environment hostile"
    );

    // (3) THE SCRUB MUST BITE. The same probe through `Repo` has no
    // repository to find, because `-C` names a plain directory and the
    // environment that would have rescued it is gone.
    let scrubbed = Repo::new(&nowhere).git(&["rev-parse", "--absolute-git-dir"]);
    assert!(
        scrubbed.is_err(),
        "an inherited GIT_DIR must not survive Repo::command's scrub — it \
         resolved to {scrubbed:?} instead of failing"
    );

    // Now run the write paths, under the poison, at the work repository. Note
    // that `init` itself is one of them: re-initialising the real repository
    // is what set `core.bare = true` during the incident.
    let work = Repo::new(&work_dir);
    work.git(&["init", "--quiet", "--initial-branch=main"])
        .expect("work init");
    work.git(&["config", "user.email", "board@test"])
        .expect("work email");
    work.git(&["config", "user.name", "board test"])
        .expect("work name");

    // The exact `store.rs::merge_branch_into_main` shape that authored the
    // three stray commits: commit on main, branch, commit, check out main,
    // merge --no-ff.
    std::fs::write(work_dir.join("root.txt"), "root").expect("write root");
    work.git(&["add", "root.txt"]).expect("add root");
    work.git(&["commit", "-q", "-m", "root"])
        .expect("commit root");
    work.git(&["checkout", "-q", "-b", "campaign/merged"])
        .expect("branch");
    std::fs::write(work_dir.join("work.txt"), "work").expect("write work");
    work.git(&["add", "work.txt"]).expect("add work");
    work.git(&["commit", "-q", "-m", "work"])
        .expect("commit work");
    work.git(&["checkout", "-q", "main"]).expect("back to main");
    work.git(&["merge", "--no-ff", "-m", "merge", "campaign/merged"])
        .expect("merge");

    // The board's own write paths: append, reap, redact, and a raw ref write
    // of the shape that left the dangling ref behind.
    let board = Board::new(work.clone());
    let kept = board
        .append(&Post::new("technique", "campaign/hermeticity"))
        .expect("append kept");
    let doomed = board
        .append(&Post::new("notice", "campaign/merged"))
        .expect("append doomed");
    let snapshot = board.snapshot().expect("snapshot").expect("some");
    let plan = ReapPlan::probe(&work, &snapshot).expect("probe");
    let reaped = board.reap(&plan).expect("reap");
    let (_redact_id, _outcome) = board.redact("campaign/hermeticity", &kept).expect("redact");
    // The dangling-ref shape, and a mechanism of its own: `git_path` asks git
    // where the ref store is and the caller then writes there with `std::fs`.
    // A leak here does not need a leaky *git* call — one bad answer from
    // `rev-parse --git-path` is enough to aim a plain file write at another
    // repository's ref store, which is how
    // `refs/hornvale/peers/dangling -> deadbeef…` came to break `git fetch`
    // repo-wide.
    let dangling_ref = format!("{}dangling", Board::PEERS_PREFIX);
    let dangling_path = work.git_path(&dangling_ref).expect("loose ref path");
    std::fs::create_dir_all(dangling_path.parent().expect("parent")).expect("ref dir");
    std::fs::write(&dangling_path, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef\n")
        .expect("write the loose ref");
    // Canonicalised on both sides: on macOS `std::env::temp_dir()` yields
    // `/var/folders/…` while git answers with the resolved `/private/var/…`,
    // and a raw `starts_with` would read that as a leak.
    let canon_work = work_dir.canonicalize().expect("canonical work dir");
    assert!(
        dangling_path
            .canonicalize()
            .expect("canonical ref path")
            .starts_with(&canon_work),
        "git_path must resolve inside the WORK repo ({canon_work:?}), not \
         wherever GIT_DIR points: {dangling_path:?}"
    );

    // Drop the poison before reading anything back.
    //
    // SAFETY: as above — single test, single thread.
    unsafe {
        for var in [
            "GIT_DIR",
            "GIT_INDEX_FILE",
            "GIT_PREFIX",
            "GIT_CONFIG_PARAMETERS",
            "GIT_AUTHOR_NAME",
            "GIT_AUTHOR_EMAIL",
        ] {
            std::env::remove_var(var);
        }
    }

    // THE WHOLE POINT, asserted first so that its message — the most
    // informative one in this file — cannot be preempted by some secondary
    // read failing on the wreckage.
    let after = fingerprint(&sentinel_dir);
    assert_eq!(
        before, after,
        "THE BOARD SUITE WROTE INTO ANOTHER REPOSITORY.\n\
         An inherited git environment reached past `git -C <root>`. Every git \
         invocation must be built by `Repo::command`, which scrubs \
         `board::git::GIT_LOCATION_VARS`.\n\n\
         before:\n{before}\nafter:\n{after}"
    );

    // (4) AND THE WRITES MUST HAVE LANDED — in the work repository, positively
    // asserted, so a path that quietly did nothing cannot masquerade as a path
    // that was correctly contained. The `soft` reads below use `unwrap_or_else`
    // rather than `expect` for the same reason the ordering above matters: a
    // leak destroys the work repo, and a panic from that wreckage would be a
    // less legible failure than the assertion actually describing it. Not every
    // read here is soft — `post_ids_at_tip()` below still panics on failure,
    // which is acceptable because the fingerprint has already been compared by
    // then, so the informative assertion cannot be preempted.
    //
    // `--branches`, not `--all`: the dangling peer ref planted above makes
    // `--all` fail outright ("fatal: bad object …"), which is precisely the
    // repository-wide breakage the incident caused — reproduced here, safely,
    // in a temp repo.
    let soft = |args: &[&str]| {
        work.git(args)
            .unwrap_or_else(|e| format!("<the work repo could not answer: {e}>"))
    };
    let work_graph = soft(&["log", "--branches", "--format=%s"]);
    for expected in ["merge", "work", "root"] {
        assert!(
            work_graph.lines().any(|l| l == expected),
            "the {expected:?} commit must exist in the WORK repo — if it exists \
             nowhere, this test exercised no write at all:\n{work_graph}"
        );
    }
    assert_eq!(
        soft(&["config", "--get", "user.name"]),
        "board test",
        "the identity write must have landed in the work repo"
    );
    // And the identity must be the one the repo's own config names, not the one
    // the poisoned `GIT_CONFIG_PARAMETERS` tried to inject. Those three commits
    // were made while the process carried
    // `'user.name'='Injected Identity'`, which outranks repo-local config
    // unless it is scrubbed — so before `GIT_CONFIG_VARS` existed this read
    // `Injected Identity <injected@evil>`. This is the mislabeling half of the
    // review's finding; the "cannot move the repository" half is pinned in
    // `hermeticity.rs`.
    let committers = soft(&["log", "--branches", "--format=%cn <%ce>"]);
    assert!(
        !committers.is_empty() && committers.lines().all(|l| l == "board test <board@test>"),
        "every work-repo commit must carry the repo's CONFIGURED identity, not \
         an environment-injected one:\n{committers}"
    );
    assert_eq!(
        reaped, 1,
        "the reap must actually have dropped the merged-branch notice \
         (id {doomed}) — a reap that dropped nothing would exercise no write"
    );
    let tip_ids = board.post_ids_at_tip().expect("ids");
    assert!(
        !tip_ids.contains(&kept),
        "the redaction must actually have evicted {kept} from the work repo's tip"
    );
    let listed = soft(&["for-each-ref", "--format=%(refname)", Board::PEERS_PREFIX]);
    assert!(
        listed.contains(&dangling_ref),
        "the dangling ref must have landed in the WORK repo's ref store: {listed:?}"
    );

    let _ = std::fs::remove_dir_all(&base);
}
