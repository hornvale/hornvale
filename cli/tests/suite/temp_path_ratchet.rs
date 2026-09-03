//! The fixed-temp-path ratchet: a test writing to a FIXED name under
//! `std::env::temp_dir()` shares that path with every concurrent run of the
//! same test on the box.
//!
//! **Observed, 2026-09-02** (The Reservoir, parked finding P1):
//! `scene_surrounds_colour_cli.rs` asserted `new` succeeded — so the CLI had
//! written the file — then panicked at `World::load` with
//! `Os { code: 2, kind: NotFound }`. Under the default fail-fast profile that
//! took the whole run down at test 274 of 4869. Two further sites
//! (`type-audit`, `placement-audit`) `remove_dir_all`ed a fixed directory
//! before creating it, so concurrent runs destroyed each other's fixtures.
//!
//! The Nettle fixed 14 sites. **Nothing stopped a fifteenth**, which is what
//! this ratchet is for — the same three-valued shape `tropes check`, the
//! timings baseline and type-audit's `waiver(...)` use. A guard that failed on
//! the mere existence of an accepted site would be red on day one and
//! trained-away by day two.
//!
//! **Direction this check enforces:** every `temp_dir()` site whose joined
//! name is a literal without a uniquifier must be declared. It is a
//! *syntactic* check over the joined expression, so it is blind to a site
//! that hides the name behind a function call — `repertory_corpus.rs:242` is
//! exactly that, and is declared for that reason rather than because it is
//! unsafe.
//!
//! **To accept a site**, append `<path>::<joined expression>` TAB `<reason>` to
//! `cli/tests/fixtures/fixed-temp-paths.txt` in the same commit. **To remove
//! one**, uniquify the site and delete its line — the check runs both
//! directions, so a stale entry fails too.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

/// The frozen roster. Append-with-reason; never edit in bulk.
const FROZEN: &str = include_str!("../fixtures/fixed-temp-paths.txt");

/// Directories scanned for `temp_dir()` sites, relative to the repo root.
///
/// Enumerated explicitly rather than walked from the root, which is what
/// keeps `.claude/worktrees/` out: a blind walk would report every finding
/// once per live worktree. `tools/` and `clients/` are included even though
/// they sit outside the cargo workspace — two of the fixed sites lived in
/// `tools/`, and `clients/` has four files using `temp_dir()` (all currently
/// uniquified, so including it costs nothing today and covers a future one).
fn scanned_dirs(root: &Path) -> Vec<PathBuf> {
    let mut out = vec![root.join("kernel"), root.join("cli")];
    for parent in ["domains", "windows", "tools", "clients"] {
        let Ok(entries) = std::fs::read_dir(root.join(parent)) else {
            continue;
        };
        let mut kids: Vec<PathBuf> = entries
            .filter_map(Result::ok)
            .map(|e| e.path())
            .filter(|p| p.is_dir())
            .collect();
        kids.sort();
        out.extend(kids);
    }
    out
}

/// Does this joined expression carry a uniquifier?
///
/// Mirrors the conventions actually in the tree: a `std::process::id()`, a
/// per-test tag or counter interpolation, or a nanosecond clock.
fn is_uniquified(expr: &str) -> bool {
    expr.contains("process::id")
        || expr.contains("{n}")
        || expr.contains("{tag}")
        || expr.contains("{i}")
        || expr.contains("COUNTER")
        || expr.contains("nanos")
        || expr.contains("SystemTime")
}

/// Every `.rs` file under `dir`, recursively, skipping `target/`.
fn rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    let mut kids: Vec<PathBuf> = entries.filter_map(Result::ok).map(|e| e.path()).collect();
    kids.sort();
    for path in kids {
        if path.is_dir() {
            if path.file_name().is_some_and(|n| n == "target") {
                continue;
            }
            rs_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "rs") {
            out.push(path);
        }
    }
}

/// A stable identity for a site: the file, then the expression joined onto
/// `temp_dir()`, whitespace-collapsed.
///
/// **Deliberately not the line number.** A roster keyed by line drifts the
/// moment anyone edits above the site, so every unrelated edit would raise a
/// stale-entry failure — which is precisely how a three-valued ratchet gets
/// trained away. The joined expression is what the acceptance is actually
/// *about*, and it moves only when the site itself does.
fn site_key(rel: &str, window: &str) -> String {
    let snippet = match window.find(".join(") {
        Some(n) => {
            let rest = &window[n + ".join(".len()..];
            let mut depth = 1usize;
            let mut end = rest.len();
            for (i, c) in rest.char_indices() {
                match c {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = i;
                            break;
                        }
                    }
                    _ => {}
                }
            }
            rest[..end].to_string()
        }
        // No `.join(` in the window — the call's result is bound and used
        // later. Fall back to the line itself, which still identifies it.
        None => window.lines().next().unwrap_or("").trim().to_string(),
    };
    let collapsed = snippet.split_whitespace().collect::<Vec<_>>().join(" ");
    format!("{rel}::{}", collapsed.chars().take(70).collect::<String>())
}

/// `<path>::<joined expression>` for every site whose joined name has no
/// uniquifier.
fn fixed_sites(root: &Path) -> BTreeSet<String> {
    let mut found = BTreeSet::new();
    for dir in scanned_dirs(root) {
        let mut files = Vec::new();
        rs_files(&dir, &mut files);
        for file in files {
            // Skip this checker's own source. It necessarily contains the
            // literal text `temp_dir()` in code (the substring search just
            // below) to implement the scan at all, which would otherwise
            // self-match as a "site" with no real `std::env::temp_dir()`
            // call behind it -- the same reason `test_binary_ratchet.rs`
            // excludes its own aggregator, `suite.rs`, from what it scans.
            if file
                .file_name()
                .is_some_and(|n| n == "temp_path_ratchet.rs")
            {
                continue;
            }
            let Ok(src) = std::fs::read_to_string(&file) else {
                continue;
            };
            let lines: Vec<&str> = src.lines().collect();
            for (i, line) in lines.iter().enumerate() {
                if !line.contains("temp_dir()") || line.trim_start().starts_with("//") {
                    continue;
                }
                // The joined expression can wrap across lines; take this line
                // plus the next few, cut at the statement's `;`.
                let window = lines[i..(i + 6).min(lines.len())].join("\n");
                let expr = match window.find(';') {
                    Some(n) => &window[..n],
                    None => &window[..],
                };
                if is_uniquified(expr) {
                    continue;
                }
                let rel = file
                    .strip_prefix(root)
                    .expect("scanned path is under the repo root");
                let rel = rel.to_string_lossy().replace('\\', "/");
                found.insert(site_key(&rel, expr));
            }
        }
    }
    found
}

/// The roster's declared sites, as `<path>::<expr>` with the reason dropped.
fn declared() -> BTreeSet<String> {
    FROZEN
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|l| l.split('\t').next().unwrap_or(l).trim().to_string())
        .collect()
}

#[test]
fn a_reasonless_roster_entry_is_a_parse_error() {
    let offenders: Vec<&str> = FROZEN
        .lines()
        .map(str::trim)
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .filter(|l| l.split('\t').nth(1).is_none_or(|r| r.trim().is_empty()))
        .collect();
    assert!(
        offenders.is_empty(),
        "every roster entry needs `<path>::<expr>` TAB `<reason>`; a reasonless \
         acceptance can only ever be satisfied, so it rots:\n{offenders:?}"
    );
}

#[test]
fn no_new_fixed_temp_path_appears() {
    let root = Path::new(concat!(env!("CARGO_MANIFEST_DIR"), "/..")); // cli/ -> repo root
    let found = fixed_sites(root);
    let frozen = declared();

    let added: Vec<&String> = found.difference(&frozen).collect();
    assert!(
        added.is_empty(),
        "new fixed temp path(s) — a fixed name under std::env::temp_dir() is \
         shared by every concurrent run of the same test on the box, and one \
         took a fail-fast run down at test 274 of 4869:\n{}\n\nUniquify it the \
         way its own file already does — a std::process::id() in the name, \
         which is the convention nearly every site in the tree uses. If the \
         site genuinely cannot collide — it \
         never creates the path, or the name is built by a helper this \
         syntactic check cannot see through — append `<path>::<expr>` TAB \
         `<reason>` to cli/tests/fixtures/fixed-temp-paths.txt in the same \
         commit. The key is the joined expression, not a line number, so it \
         survives edits above the site.",
        added
            .iter()
            .map(|entry| format!("  {entry}"))
            .collect::<Vec<_>>()
            .join("\n")
    );

    let stale: Vec<&String> = frozen.difference(&found).collect();
    assert!(
        stale.is_empty(),
        "roster entr(y/ies) that no longer match a fixed temp path — the site \
         was uniquified, moved, or its joined expression changed. Delete the \
         line, or correct it to the new expression:\n{}",
        stale
            .iter()
            .map(|entry| format!("  {entry}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
}
