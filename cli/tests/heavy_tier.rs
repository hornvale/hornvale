//! The heavy-tier convention (fast-gate-tiers spec): every #[ignore]d test
//! that defers a live-worldgen battery carries the exact canonical reason
//! string, so `make gate` (which skips them) and `make gate-full` (which
//! runs them) stay in sync and the tier is greppable, not tribal.

use std::process::Command;

/// The one reason string every heavy-tier test must use verbatim.
const CANONICAL: &str = "heavy: live-worldgen battery; runs in make gate-full / cloud nightly";

/// The workspace root. `cargo test` runs with CWD set to the package dir
/// (`cli/`), so a bare `git grep` would only see `cli/**`; search from the
/// toplevel instead so the whole workspace is covered.
fn repo_root() -> String {
    let out = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .output()
        .expect("git rev-parse should run");
    assert!(out.status.success(), "git rev-parse failed");
    String::from_utf8(out.stdout)
        .expect("git output is utf8")
        .trim()
        .to_string()
}

/// All `#[ignore = "..."]` reason strings in the workspace's Rust sources,
/// found via `git grep` (std-only; the pattern architecture.rs already uses).
fn ignore_reasons() -> Vec<String> {
    let out = Command::new("git")
        .current_dir(repo_root())
        .args(["grep", "-hoE", r#"#\[ignore = "[^"]*"\]"#, "--", "*.rs"])
        .output()
        .expect("git grep should run");
    assert!(out.status.success(), "git grep failed");
    String::from_utf8(out.stdout)
        .expect("git output is utf8")
        .lines()
        .filter_map(|l| {
            l.split_once("= \"")
                .and_then(|(_, r)| r.strip_suffix("\"]"))
        })
        .map(str::to_string)
        .collect()
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
