//! `gate-fast` must never miss a dependent crate.
//!
//! The script's own header promises "Overapproximation is fine; missing a
//! dependent crate is not", and for a long time it broke that promise: its
//! dependents were a hand-maintained list that `hornvale-vessel` was never
//! added to. So a change under `windows/locale/` scoped to `hornvale-locale`
//! alone, while vessel, scene and lab all depend on locale — and a stale
//! vessel golden rode through a green `gate-fast` (The Handle, 2026-08-06).
//!
//! This asserts the promise directly, against the workspace's real dependency
//! graph read from the Cargo.toml files. The script is *asked* for its answer
//! (`--closure`) rather than having its rule copied here, because a copied
//! rule is what goes stale — the same trap `golden-pins.sql` fell into twice.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::Command;

/// The workspace root — this test's manifest dir is `cli/`.
fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Every workspace crate directory, as the script enumerates them.
fn crate_dirs() -> Vec<PathBuf> {
    let r = root();
    let mut dirs = vec![r.join("kernel"), r.join("cli")];
    for parent in ["domains", "windows"] {
        let Ok(entries) = std::fs::read_dir(r.join(parent)) else {
            continue;
        };
        // BTreeSet, not the raw read_dir order: directory order is not stable
        // across filesystems and this test must not vary by machine.
        let mut found: BTreeSet<PathBuf> = BTreeSet::new();
        for e in entries.flatten() {
            if e.path().join("Cargo.toml").is_file() {
                found.insert(e.path());
            }
        }
        dirs.extend(found);
    }
    dirs
}

/// `name` from a crate's Cargo.toml.
fn package_name(dir: &Path) -> String {
    let toml = std::fs::read_to_string(dir.join("Cargo.toml")).expect("a crate has a Cargo.toml");
    for line in toml.lines() {
        if let Some(rest) = line.strip_prefix("name = \"")
            && let Some(name) = rest.strip_suffix('"')
        {
            return name.to_string();
        }
    }
    panic!("{} declares no package name", dir.display());
}

/// package -> the packages that directly depend on it.
fn dependents() -> BTreeMap<String, BTreeSet<String>> {
    let mut map: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let dirs = crate_dirs();
    let names: Vec<(PathBuf, String)> = dirs.iter().map(|d| (d.clone(), package_name(d))).collect();
    for (dir, consumer) in &names {
        let toml = std::fs::read_to_string(dir.join("Cargo.toml")).expect("readable");
        for (_, dep) in &names {
            if dep == consumer {
                continue;
            }
            // Internal deps are always `hornvale-foo = { path = … }` at line
            // start; anchoring avoids matching a path fragment or a comment.
            if toml.lines().any(|l| l.starts_with(&format!("{dep} = "))) {
                map.entry(dep.clone()).or_default().insert(consumer.clone());
            }
        }
    }
    map
}

/// What `gate-fast --closure <pkg>` selects.
fn script_closure(pkg: &str) -> BTreeSet<String> {
    let out = Command::new("bash")
        .arg(root().join("scripts/gate-fast.sh"))
        .arg("--closure")
        .arg(pkg)
        .current_dir(root())
        .output()
        .expect("gate-fast.sh runs");
    assert!(
        out.status.success(),
        "gate-fast --closure {pkg} failed: {}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout)
        .lines()
        .map(|l| l.trim().to_string())
        .filter(|l| !l.is_empty())
        .collect()
}

#[test]
fn gate_fast_selects_every_transitive_dependent_of_every_crate() {
    let deps = dependents();
    let all: Vec<String> = crate_dirs().iter().map(|d| package_name(d)).collect();

    for pkg in &all {
        let selected = script_closure(pkg);
        assert!(
            selected.contains(pkg),
            "gate-fast --closure {pkg} does not include {pkg} itself"
        );

        // Transitive closure of `dependents`, computed here as ground truth.
        let mut want: BTreeSet<String> = BTreeSet::new();
        let mut frontier = vec![pkg.clone()];
        while let Some(cur) = frontier.pop() {
            for d in deps.get(&cur).into_iter().flatten() {
                if want.insert(d.clone()) {
                    frontier.push(d.clone());
                }
            }
        }

        let missing: Vec<&String> = want.iter().filter(|w| !selected.contains(*w)).collect();
        assert!(
            missing.is_empty(),
            "a change to {pkg} would skip {missing:?} — gate-fast promises \
             overapproximation, and missing a dependent is how a stale golden \
             rides through green. selected: {selected:?}"
        );
    }
}

#[test]
fn the_locale_to_vessel_edge_is_covered() {
    // The specific miss that motivated this test, named so a future reader
    // sees the concrete failure rather than only the general rule.
    let selected = script_closure("hornvale-locale");
    for want in ["hornvale-vessel", "hornvale-scene", "hornvale-lab"] {
        assert!(
            selected.contains(want),
            "a locale change must run {want}: {selected:?}"
        );
    }
}
