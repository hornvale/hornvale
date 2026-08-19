//! Debug/release byte-identity cross-check — the ratification evidence for
//! running lab censuses under `--release`.
//!
//! Determinism across optimization levels is a save-format-class claim: if
//! the optimizer ever changed a drawn value (fast-math, float contraction, a
//! profile-gated `cfg`), release-built censuses would silently measure a
//! different world population than the debug gate verifies. This test builds
//! the seed-42 world through both profiles and asserts the JSON is
//! byte-identical.
//!
//! It is `#[ignore]`d in the local gate because it compiles the entire
//! workspace in release; CI runs it explicitly (see the "Artifacts are
//! current" step) before any release-mode `lab run`.

use std::path::PathBuf;
use std::process::Command;

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .to_path_buf()
}

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("hornvale-xcheck-{tag}-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

fn make_world(bin: &mut Command, path: &std::path::Path) -> Vec<u8> {
    let out = bin
        .args(["new", "--seed", "42", "--out", path.to_str().unwrap()])
        .output()
        .unwrap();
    assert!(out.status.success(), "new failed: {out:?}");
    std::fs::read(path).unwrap()
}

/// The seed-42 world serialized through the debug and release profiles must
/// be byte-identical. Release censuses are trustworthy exactly as long as
/// this holds.
#[test]
#[ignore = "compiles the workspace in release; CI runs it with -- --ignored"]
fn seed_42_world_is_byte_identical_across_opt_levels() {
    let root = workspace_root();
    let build = Command::new(env!("CARGO"))
        .args(["build", "--release", "-p", "hornvale"])
        .current_dir(&root)
        .output()
        .unwrap();
    assert!(
        build.status.success(),
        "release build failed: {}",
        String::from_utf8_lossy(&build.stderr)
    );

    let dir = temp_dir("opt");
    let debug_json = make_world(
        &mut Command::new(env!("CARGO_BIN_EXE_hornvale")),
        &dir.join("world-debug.json"),
    );
    let release_json = make_world(
        &mut Command::new(root.join("target/release/hornvale")),
        &dir.join("world-release.json"),
    );
    std::fs::remove_dir_all(&dir).unwrap();

    assert!(
        debug_json == release_json,
        "seed-42 world differs between debug and release builds: \
         determinism across opt levels is broken — do NOT ship release censuses"
    );
}
