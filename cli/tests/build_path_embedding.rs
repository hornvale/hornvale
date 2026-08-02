//! Production code must not embed new absolute build paths (decision 0090,
//! amendment 2).
//!
//! Every `env!("CARGO_MANIFEST_DIR")` in non-test source expands, at compile
//! time, to the absolute directory the crate was built in, and that string
//! ships inside the binary. Two builds of the same commit in two directories
//! therefore produce two different binaries — measured on the Mac during The
//! Pyx, where `/tmp/pyx-mech` and `/tmp/pyx-mech2` hashed `5bae5217…` and
//! `2b8a4f65…` respectively, each carrying its own path, with `debug = false`
//! and no debug information involved.
//!
//! That is what bounds decision 0090's cross-host oracle. Binary identity
//! qualifies a candidate host **only when both hosts build at the same
//! absolute path** — the condition a container satisfies for free by fixing
//! the build directory in the image. Each new path-embedding site is another
//! way for that condition to be violated invisibly: nothing in any output
//! changes, no test goes red, and two machines simply stop agreeing on a hash
//! for a reason unrelated to determinism.
//!
//! So the list is frozen. It may shrink freely; growing it is a deliberate
//! act that should say what it does to the oracle.
//!
//! **Scope, stated rather than implied.** This scans `*/src/` — production
//! source. Test code (`*/tests/`) is exempt and should be: a test binary's
//! embedded path never ships, and `CARGO_MANIFEST_DIR` is the correct way for
//! a test to find its fixtures. A `#[cfg(test)]` module *inside* `src/` would
//! be counted here, conservatively; there are none today, and counting one
//! would be a false positive worth taking over a missed real use.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

/// The compile-time env var whose expansion embeds a build path.
const EMBEDS_BUILD_PATH: &str = "CARGO_MANIFEST_DIR";

/// The workspace root — the parent of `cli/`, where this test crate lives.
fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ always has a parent")
        .to_path_buf()
}

/// The frozen list, as `path -> count`.
fn frozen() -> BTreeMap<String, usize> {
    include_str!("fixtures/manifest-dir-uses.txt")
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| {
            let (path, count) = l
                .rsplit_once(char::is_whitespace)
                .unwrap_or_else(|| panic!("malformed fixture row: {l:?} (want `<path> <count>`)"));
            (
                path.trim().to_string(),
                count
                    .trim()
                    .parse()
                    .unwrap_or_else(|e| panic!("malformed count in fixture row {l:?}: {e}")),
            )
        })
        .collect()
}

/// Recursively collect `.rs` files under `dir`.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path.is_dir() {
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// Every production `src/` tree in the workspace: the kernel, each domain,
/// each window, and the CLI. Discovered rather than listed, so a new domain
/// or window is covered the day it is added.
fn production_src_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = vec![root.join("kernel/src"), root.join("cli/src")];
    for layer in ["domains", "windows"] {
        let Ok(entries) = std::fs::read_dir(root.join(layer)) else {
            continue;
        };
        let mut crates: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
        crates.sort();
        for c in crates {
            let src = c.join("src");
            if src.is_dir() {
                dirs.push(src);
            }
        }
    }
    dirs
}

/// `path -> number of build-path-embedding expansions`, over production source.
fn live_uses() -> BTreeMap<String, usize> {
    let root = workspace_root();
    let mut found = BTreeMap::new();
    for dir in production_src_dirs(&root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            let text = std::fs::read_to_string(&file).unwrap_or_default();
            let count = text.matches(EMBEDS_BUILD_PATH).count();
            if count > 0 {
                let rel = file
                    .strip_prefix(&root)
                    .unwrap_or(&file)
                    .to_string_lossy()
                    .into_owned();
                found.insert(rel, count);
            }
        }
    }
    found
}

#[test]
fn production_code_embeds_no_new_build_paths() {
    let live = live_uses();
    let frozen = frozen();

    let mut offenders = Vec::new();
    for (path, count) in &live {
        match frozen.get(path) {
            None => offenders.push(format!("  {path}: {count} use(s), not on the frozen list")),
            Some(allowed) if count > allowed => offenders.push(format!(
                "  {path}: {count} use(s), frozen list allows {allowed}"
            )),
            Some(_) => {}
        }
    }

    assert!(
        offenders.is_empty(),
        "production code gained a build-path embedding:\n{}\n\n\
         `env!(\"{EMBEDS_BUILD_PATH}\")` expands at compile time to the \
         absolute directory the crate was built in, and that string ships in \
         the binary. Decision 0090's cross-host oracle — qualify a candidate \
         host by comparing one sha256sum instead of running a census — holds \
         only while both hosts build at the same absolute path, and every new \
         site here is another way to violate that invisibly: no output \
         changes, nothing goes red, and two machines just stop agreeing.\n\n\
         If a test needs its fixtures, use {EMBEDS_BUILD_PATH} in `tests/` \
         instead — that binary never ships and is not scanned. If production \
         really needs it, add the row to \
         cli/tests/fixtures/manifest-dir-uses.txt and say in the commit what \
         it costs the oracle.",
        offenders.join("\n")
    );
}

#[test]
fn the_frozen_list_only_shrinks() {
    let live = live_uses();
    let frozen = frozen();
    let stale: Vec<&String> = frozen.keys().filter(|p| !live.contains_key(*p)).collect();
    assert!(
        stale.is_empty(),
        "the frozen list names files that no longer embed a build path: {stale:?}\n\n\
         This is good news — the binary got more portable. Delete those rows \
         from cli/tests/fixtures/manifest-dir-uses.txt so the list keeps \
         meaning what it says."
    );
}

#[test]
fn the_scan_actually_resolves_production_trees() {
    // Anti-vacuity: both assertions above pass trivially if the scan finds
    // nothing at all, so pin that it finds the two known sites and that it
    // looks in real directories.
    let root = workspace_root();
    let dirs = production_src_dirs(&root);
    assert!(
        dirs.iter().all(|d| d.is_dir()),
        "every discovered production src dir must exist: {dirs:?}"
    );
    assert!(
        dirs.len() >= 10,
        "expected the kernel, cli, and every domain/window src tree; got {}",
        dirs.len()
    );

    let live = live_uses();
    assert!(
        live.contains_key("cli/src/main.rs"),
        "the scan must see the known ci-record use in cli/src/main.rs; got {live:?}"
    );
    assert!(
        live.contains_key("windows/lab/src/blackbox.rs"),
        "the scan must see the known blackbox use; got {live:?}"
    );
}
