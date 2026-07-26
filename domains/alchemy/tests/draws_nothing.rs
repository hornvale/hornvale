//! Enforces the campaign's headline claim (spec §5): "The Assay draws
//! nothing." A hidden draw in this crate would be an unregistered
//! save-format contract -- exactly the failure mode `derivation_is_pure` in
//! `production_properties.rs` cannot detect, since a tautology
//! (`assert_eq!(f(x), f(x))`) holds for any deterministic function and
//! can't see a later `Seed` parameter being added to that function's
//! signature.
//!
//! This test reads every `.rs` file under `domains/alchemy/src/` and asserts
//! none of them, OUTSIDE COMMENTS, mentions `Seed`, `StreamLabel`, or
//! `Stream`, or declares a `streams` module. The crate's own doc comment
//! (`lib.rs`) mentions these words in prose -- "There is no `streams.rs`, no
//! `StreamLabel`, and no `Seed` parameter anywhere in it" -- so comment
//! lines are excluded from the scan; only live code is checked.

use std::fs;
use std::path::{Path, PathBuf};

/// The alchemy crate's `src/` directory: this test's own manifest dir is
/// `<root>/domains/alchemy`, so `src` sits directly under it.
fn src_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("src")
}

/// Every `.rs` file directly or indirectly under `dir`.
fn rust_files(dir: &Path) -> Vec<PathBuf> {
    let mut out = Vec::new();
    let entries = fs::read_dir(dir).unwrap_or_else(|e| panic!("reading {}: {e}", dir.display()));
    for entry in entries {
        let entry = entry.unwrap_or_else(|e| panic!("reading entry in {}: {e}", dir.display()));
        let path = entry.path();
        if path.is_dir() {
            out.extend(rust_files(&path));
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
    out
}

/// Strip a line down to the code that precedes any `//` comment marker.
/// Doc comments (`///`, `//!`) and ordinary comments (`//`) all start with
/// `//`, so this single check excludes all three -- and this crate's source
/// contains no string literal with `//` in it, so the simplification is
/// exact here even though it would not be in general.
fn code_part(line: &str) -> &str {
    match line.find("//") {
        Some(idx) => &line[..idx],
        None => line,
    }
}

/// The Assay draws nothing: no source file under `domains/alchemy/src/`
/// mentions `Seed`, `StreamLabel`, or `Stream` in live code, and none
/// declares a `streams` module. A later change that threaded a `Seed`
/// through this crate -- an unregistered save-format contract, per the
/// spec -- must fail this test, not just violate a comment.
#[test]
fn domain_draws_nothing() {
    let forbidden = ["Seed", "StreamLabel", "Stream", "mod streams"];
    for path in rust_files(&src_dir()) {
        let content =
            fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
        for (lineno, line) in content.lines().enumerate() {
            let code = code_part(line);
            for needle in forbidden {
                assert!(
                    !code.contains(needle),
                    "{}:{}: found forbidden identifier `{needle}` in live code -- \
                     The Assay draws nothing (spec §5); a draw here is an \
                     unregistered save-format contract:\n    {line}",
                    path.display(),
                    lineno + 1,
                );
            }
        }
    }
}
