//! The ignore-reason conventions. An `#[ignore]` is a promise deferred, and a
//! promise nobody can find again is a promise broken — so every class of
//! deferral here carries a TOKEN in its reason string and is held to that
//! string verbatim by a test, which is what keeps the class greppable rather
//! than tribal.
//!
//! Two classes so far:
//!
//! - `heavy:` (fast-gate-tiers spec) — a live-worldgen battery deferred from
//!   `make gate` to `make gate-full`, so the two stay in sync.
//! - `stale-census:` (The Wearing, task 11e) — a calibration row that cannot
//!   be honestly pinned until a deferred census regen is run.

use std::fs;
use std::path::{Path, PathBuf};

/// The one reason string every heavy-tier test must use verbatim.
const CANONICAL: &str =
    "heavy: live-worldgen battery (minutes); deferred from the commit gate to make gate-full";

/// The workspace root: the parent of this crate's manifest dir (`cli/`).
/// Filesystem-based, not git-based — the remote gate runs the suite in an
/// rsync'd tree that is not a git repository.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
}

/// Recursively collect every `.rs` file under `dir`, skipping `target/` and
/// dot-directories (the same source set `git grep -- '*.rs'` covered).
fn collect_rs(dir: &Path, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(dir).expect("directory is readable") {
        let entry = entry.expect("directory entry is readable");
        let path = entry.path();
        let name = entry.file_name().to_string_lossy().into_owned();
        if path.is_dir() {
            if name == "target" || name.starts_with('.') {
                continue;
            }
            collect_rs(&path, out);
        } else if name.ends_with(".rs") {
            out.push(path);
        }
    }
}

/// All `#[ignore = "..."]` reason strings in the repo's Rust sources
/// (std-only filesystem scan; reason strings never contain quotes).
fn ignore_reasons() -> Vec<String> {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    sources.sort();
    let mut reasons = Vec::new();
    for path in sources {
        let text = fs::read_to_string(&path).expect("source file is utf8");
        for line in text.lines() {
            if let Some((_, rest)) = line.split_once("#[ignore = \"")
                && let Some((reason, _)) = rest.split_once("\"]")
            {
                reasons.push(reason.to_string());
            }
        }
    }
    reasons
}

#[test]
fn heavy_tier_reason_strings_are_canonical() {
    let reasons = ignore_reasons();
    let heavy: Vec<&String> = reasons.iter().filter(|r| r.contains("heavy:")).collect();
    assert!(
        !heavy.is_empty(),
        "expected at least one heavy-tier #[ignore] test; found none"
    );
    for r in &heavy {
        assert_eq!(
            *r, CANONICAL,
            "heavy-tier ignore reason must be verbatim canonical; found: {r:?}"
        );
    }
}

/// The two canonical `stale-census:` reason strings (The Wearing, task 11e).
///
/// The campaign landed its code but DEFERRED its census regen, which leaves
/// calibration rows that cannot honestly be pinned yet. Deferred debt that is
/// merely commented gets lost, so it is spelled with a token that `git grep
/// stale-census:` finds in one shot, and held to a verbatim string here for
/// the same reason the heavy tier is: a reason string that drifts stops being
/// greppable, and the debt goes quiet again.
///
/// Two variants, because the two classes discharge differently — the first
/// needs a fresh census, the second only a re-measurement against the merged
/// tree. Keep them distinguishable; do not collapse them into one string.
const STALE_CENSUS: [&str; 2] = [
    "stale-census: The Wearing deferred its census regen; this row reads a census \
     predating the campaign's metrics. Re-derive per .superpowers/sdd/followups.md",
    "stale-census: The Wearing deferred its census regen; this live seed pin moved \
     when main's placement changed. Re-derive per .superpowers/sdd/followups.md",
];

#[test]
fn stale_census_reason_strings_are_canonical() {
    let reasons = ignore_reasons();
    let stale: Vec<&String> = reasons
        .iter()
        .filter(|r| r.contains("stale-census:"))
        .collect();
    assert!(
        !stale.is_empty(),
        "expected at least one stale-census #[ignore] test; found none. If the \
         deferred regen has been discharged and every row re-derived, delete \
         this test and its constant along with the last ignore."
    );
    for r in &stale {
        assert!(
            STALE_CENSUS.contains(&r.as_str()),
            "stale-census ignore reason must be one of the two canonical \
             strings verbatim; found: {r:?}"
        );
    }
}
