//! `tree_is_clean_in` must block on TRACKED modifications and ignore untracked
//! files.
//!
//! DIRECTION THESE TESTS ENFORCE: both of them. The permissive direction
//! (untracked is ignored) is the behaviour change; the restrictive direction
//! (a tracked edit still refuses) is the guarantee that must survive it, and
//! testing only the first would let the whole check be neutered to `true`
//! without a single test objecting.

use seam_guard::tree_is_clean_in;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Run git against `dir`, with the ambient git environment SCRUBBED.
///
/// `-C <dir>` does NOT override `GIT_DIR`: if the caller has it set, every
/// command here retargets the repository it names and `-C` only changes the
/// working directory. That is not theoretical — writing this test cost a real
/// incident on 2026-08-23. Running it once with `GIT_DIR` pointed at the
/// Hornvale checkout (to prove the outboard runner's `env -u` was load-bearing
/// rather than cargo-culted) sent this helper's `init`/`config`/`add`/`commit`
/// into that repository instead of the temp one: it added `tracked.txt` to the
/// index, landed a junk commit "root" on the checked-out branch, and
/// OVERWROTE `user.email`/`user.name` to the fixture's `t@t`, so every
/// subsequent commit would have carried the wrong author.
///
/// Scrubbing at the RUNNER (scripts/lane-outboard.sh) is not sufficient,
/// because it protects only that one invocation. A test that shells out to git
/// has to be safe when someone runs it by hand, which is precisely when nobody
/// is thinking about the environment.
fn git(dir: &Path, args: &[&str]) {
    let ok = Command::new("git")
        .env_remove("GIT_DIR")
        .env_remove("GIT_INDEX_FILE")
        .env_remove("GIT_WORK_TREE")
        .env_remove("GIT_OBJECT_DIRECTORY")
        .arg("-C")
        .arg(dir)
        .args(args)
        .output()
        .expect("git must be runnable")
        .status
        .success();
    assert!(ok, "git {args:?} failed in {}", dir.display());
}

/// A throwaway repository with one committed file, under the OS temp dir.
/// Never this repository — the check shells out to git, and a test that
/// mutated the real tree would be indistinguishable from the bug.
fn scratch_repo(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("seam-guard-tic-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("temp dir");
    git(&dir, &["init", "-q", "-b", "main", "."]);
    git(&dir, &["config", "user.email", "t@t"]);
    git(&dir, &["config", "user.name", "t"]);
    std::fs::write(dir.join("tracked.txt"), "original\n").expect("write");
    git(&dir, &["add", "tracked.txt"]);
    git(&dir, &["commit", "-qm", "root"]);
    dir
}

#[test]
fn a_pristine_tree_is_clean() {
    let dir = scratch_repo("pristine");
    assert!(
        tree_is_clean_in(&dir),
        "a freshly committed tree must be clean, or the tool can never start"
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn an_untracked_file_does_not_make_the_tree_dirty() {
    let dir = scratch_repo("untracked");
    std::fs::write(dir.join("stray.txt"), "another tool's state\n").expect("write");
    std::fs::create_dir_all(dir.join(".pi")).expect("mkdir");
    std::fs::write(dir.join(".pi/settings.json"), "{}\n").expect("write");
    assert!(
        tree_is_clean_in(&dir),
        "untracked files are not dirt for this check: they play no part in the \
         `git checkout -- <file>` restore path it exists to protect. A stray \
         untracked directory refusing the whole run is what prompted this."
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_tracked_modification_still_makes_the_tree_dirty() {
    let dir = scratch_repo("tracked");
    std::fs::write(dir.join("tracked.txt"), "edited\n").expect("write");
    assert!(
        !tree_is_clean_in(&dir),
        "a modification to a TRACKED file must still refuse — this is the \
         guarantee the untracked relaxation must not cost. seam-guard rewrites \
         sources in place, and restoring them assumes they started committed."
    );
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn a_staged_but_uncommitted_change_still_makes_the_tree_dirty() {
    let dir = scratch_repo("staged");
    std::fs::write(dir.join("new.txt"), "staged\n").expect("write");
    git(&dir, &["add", "new.txt"]);
    assert!(
        !tree_is_clean_in(&dir),
        "`--untracked-files=no` must not be read as `--ignore-the-index`: a \
         staged addition is a tracked change and still refuses"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
