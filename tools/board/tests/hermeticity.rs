//! The structural half of the hermeticity guard. `hermeticity_env.rs` proves
//! that *today's* git invocations resist an inherited `GIT_DIR`; this file
//! stops a *new* one from being added that never gets the scrub in the first
//! place, and checks the compiled binary at the same seam.
//!
//! The runtime guard cannot cover a spawn site it never reaches, and the
//! incident's blast radius came from breadth: `git init`, `git config`,
//! `git commit`, `git merge`, `git update-ref` — every one of them a separate
//! call, all leaking for one shared reason. Funnelling them through a single
//! constructor is what made one fix sufficient, so the count is the invariant
//! worth pinning.

use std::path::{Path, PathBuf};

/// The only two places in the crate allowed to spawn `git` directly, and why.
///
/// `src/git.rs` holds `Repo::command`, the scrubbing constructor every other
/// call routes through. `tests/hermeticity_env.rs` holds the deliberately
/// *unscrubbed* control that proves the guard's poisoned environment is
/// actually hostile — a guard whose control also scrubbed would assert
/// nothing.
const SANCTIONED_GIT_SPAWNS: &[(&str, usize)] =
    &[("src/git.rs", 1), ("tests/hermeticity_env.rs", 1)];

/// The literal a raw spawn is written as. Matching on source text rather than
/// on behaviour is crude, but it is the only check that can see a call site
/// that does not exist yet.
const RAW_SPAWN: &str = r#"Command::new("git")"#;

/// This file is left out of its own scan. It has to name `RAW_SPAWN` to search
/// for it, and several comments here quote it while explaining the rule, so
/// scanning itself would report a permanent, meaningless hit.
const THE_SCANNER: &str = "tests/hermeticity.rs";

/// Drop comment lines before counting.
///
/// The rule is about call sites, and the reason for the rule is written down
/// in prose right next to several of them — `git.rs`'s module docs quote the
/// spawn literal three times explaining why there must only be one of it.
/// Counting prose would make the guard fail on its own documentation, and the
/// obvious "fix" for that is to stop documenting it.
fn code_only(body: &str) -> String {
    body.lines()
        .filter(|line| {
            let t = line.trim_start();
            !(t.starts_with("//") || t.starts_with("/*") || t.starts_with('*'))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn crate_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Every `.rs` file under `src/` and `tests/`, relative to the crate root.
fn rust_sources() -> Vec<(String, String)> {
    let root = crate_root();
    let mut found = Vec::new();
    for sub in ["src", "tests"] {
        let mut stack = vec![root.join(sub)];
        while let Some(dir) = stack.pop() {
            let entries = std::fs::read_dir(&dir).unwrap_or_else(|e| panic!("read {dir:?}: {e}"));
            for entry in entries {
                let path = entry.expect("dir entry").path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().is_some_and(|e| e == "rs") {
                    let rel = path
                        .strip_prefix(&root)
                        .expect("under root")
                        .to_string_lossy()
                        .replace('\\', "/");
                    let body = std::fs::read_to_string(&path)
                        .unwrap_or_else(|e| panic!("read {path:?}: {e}"));
                    found.push((rel, body));
                }
            }
        }
    }
    found.sort();
    found
}

#[test]
fn every_git_spawn_in_the_crate_routes_through_the_scrubbing_constructor() {
    let sources = rust_sources();

    // Not vacuous on an empty scan: if the walk finds nothing (a moved
    // directory, a bad `CARGO_MANIFEST_DIR`), there is no set of files in
    // which the count could be wrong, so require a plausible corpus first.
    assert!(
        sources.len() >= 8,
        "the source scan found only {} files — it is not looking where the \
         crate lives, so its verdict is meaningless: {:?}",
        sources.len(),
        sources.iter().map(|(p, _)| p).collect::<Vec<_>>()
    );
    for required in ["src/git.rs", "tests/hermeticity_env.rs"] {
        assert!(
            sources.iter().any(|(p, _)| p == required),
            "the scan must see {required}, or it cannot check its count"
        );
    }

    // Comment stripping must not have eaten the code it is meant to leave
    // behind — otherwise every count is zero and every file looks clean.
    let git_rs = code_only(
        &sources
            .iter()
            .find(|(p, _)| p == "src/git.rs")
            .expect("git.rs")
            .1,
    );
    assert!(
        git_rs.contains("fn command(&self)"),
        "comment stripping removed the code it was supposed to keep, so every \
         count below would be zero and every file would look clean"
    );
    // And the scrub itself must still be at that call site. This is the static
    // counterpart to `hermeticity_env.rs`'s runtime check: deleting the
    // `env_remove` loop is the single edit that reopens the whole hole, so it
    // is worth catching in the cheap test as well as the thorough one.
    assert!(
        git_rs.contains("env_remove"),
        "src/git.rs no longer scrubs the environment. `Repo::command` must \
         `env_remove` every name in GIT_LOCATION_VARS and GIT_IDENTITY_VARS, \
         or an inherited GIT_DIR outranks `git -C <root>` again"
    );

    let mut actual: Vec<(String, usize)> = Vec::new();
    for (path, body) in &sources {
        if path == THE_SCANNER {
            continue;
        }
        let n = code_only(body).matches(RAW_SPAWN).count();
        if n > 0 {
            actual.push((path.clone(), n));
        }
    }

    let expected: Vec<(String, usize)> = SANCTIONED_GIT_SPAWNS
        .iter()
        .map(|(p, n)| ((*p).to_string(), *n))
        .collect();

    assert_eq!(
        actual, expected,
        "a raw `{RAW_SPAWN}` bypasses `Repo::command`'s environment scrub, and \
         so would run against a hook's inherited GIT_DIR — the exact leak that \
         wrote `board test` commits into the real repository. Route it through \
         `Repo::git`, `Repo::git_output`, `Repo::git_with_index`, or \
         `Repo::git_stdin_bytes` instead. If a new raw spawn is genuinely \
         necessary, say why in SANCTIONED_GIT_SPAWNS."
    );
}

#[test]
fn the_scrub_list_covers_the_variables_git_actually_exports_to_a_hook() {
    // Measured against git 2.50.1: a pre-commit hook run from a linked
    // worktree receives GIT_DIR, GIT_INDEX_FILE, GIT_PREFIX and the
    // GIT_AUTHOR_* trio. Every one of them must be on a scrub list, or the
    // fix is narrower than the thing it is fixing.
    let scrubbed: Vec<&str> = board::git::GIT_LOCATION_VARS
        .iter()
        .chain(board::git::GIT_IDENTITY_VARS)
        .copied()
        .collect();
    for exported in [
        "GIT_DIR",
        "GIT_INDEX_FILE",
        "GIT_PREFIX",
        "GIT_AUTHOR_NAME",
        "GIT_AUTHOR_EMAIL",
        "GIT_AUTHOR_DATE",
        // Not exported by a hook, but each redirects git just as effectively,
        // and a partial list is the failure mode worth pinning.
        "GIT_WORK_TREE",
        "GIT_COMMON_DIR",
        "GIT_OBJECT_DIRECTORY",
        "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        "GIT_NAMESPACE",
    ] {
        assert!(
            scrubbed.contains(&exported),
            "{exported} can point git at another repository but is on neither \
             scrub list: {scrubbed:?}"
        );
    }
}

/// A throwaway repository with a board post whose text identifies it.
fn repo_with_post(dir: &Path, marker: &str) -> board::git::Repo {
    std::fs::create_dir_all(dir).expect("dir");
    let repo = board::git::Repo::new(dir);
    repo.git(&["init", "--quiet", "--initial-branch=main"])
        .expect("init");
    repo.git(&["config", "user.email", "board@test"])
        .expect("email");
    repo.git(&["config", "user.name", "board test"])
        .expect("name");
    std::fs::write(dir.join("seed.txt"), marker).expect("write");
    repo.git(&["add", "seed.txt"]).expect("add");
    repo.git(&["commit", "-q", "-m", "seed"]).expect("commit");
    board::store::Board::new(repo.clone())
        .append(
            &board::post::Post::new("technique", "campaign/hermeticity")
                .with("note", serde_json::json!(marker)),
        )
        .expect("append");
    repo
}

#[test]
fn the_binary_reads_the_repository_it_stands_in_not_the_one_git_dir_names() {
    // `main.rs` builds its `Repo` from `current_dir()`, which is correct — and
    // was silently overridden by an inherited GIT_DIR, so `board` invoked
    // anywhere under a hook read (and `reap` would have written) the hook's
    // repository instead. Two repositories, two distinguishable posts, and the
    // binary must report the one it is standing in.
    let base =
        std::env::temp_dir().join(format!("hv-board-hermeticity-bin-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&base);
    let here = base.join("here");
    let elsewhere = base.join("elsewhere");
    repo_with_post(&here, "THE-REPO-I-AM-STANDING-IN");
    repo_with_post(&elsewhere, "THE-REPO-GIT-DIR-NAMES");

    let out = std::process::Command::new(env!("CARGO_BIN_EXE_board"))
        .current_dir(&here)
        .env("GIT_DIR", elsewhere.join(".git"))
        .env("GIT_INDEX_FILE", elsewhere.join(".git").join("index"))
        .arg("render")
        .output()
        .expect("spawn board");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);

    // The positive half first: without it, a `render` that printed nothing at
    // all (a crash, a usage error) would satisfy the negative assertion.
    assert!(
        stdout.contains("THE-REPO-I-AM-STANDING-IN"),
        "render must report the cwd's own board.\nstdout: {stdout}\nstderr: {stderr}"
    );
    assert!(
        !stdout.contains("THE-REPO-GIT-DIR-NAMES"),
        "render must NOT reach the repository GIT_DIR names.\nstdout: {stdout}"
    );

    let _ = std::fs::remove_dir_all(&base);
}
