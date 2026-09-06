//! Spec §7: the lot window draws nothing new — every random choice it makes
//! is pure hash expansion of `(seed, index, label)` (`draw::uniform`) over
//! facts the ledger already committed, never a fresh `Stream` draw. Several
//! doc comments in `windows/lot/src` already say so in prose ("Nothing here
//! draws a `Stream`", `slots.rs`/`draw.rs`); this test is the mechanical
//! check behind that claim, in the same shape
//! `domains/alchemy/tests/suite/draws_nothing.rs` uses for The Reagent's own
//! "draws nothing" headline: it reads every `.rs` file under
//! `windows/lot/src/` and fails if the word `Stream` appears anywhere
//! outside a `//` comment.
//!
//! **Word-boundary, not substring.** A crate this size can plausibly grow an
//! identifier like `upstream` or `streaming` in prose or a variable name
//! without drawing anything; a bare substring check would false-positive on
//! those. The check here only fires when `Stream` stands as its own word —
//! flanked by anything that is not an ASCII letter, digit, or underscore.

use std::fs;
use std::path::{Path, PathBuf};

/// The lot crate's `src/` directory: this test's own manifest dir is
/// `<root>/windows/lot`, so `src` sits directly under it.
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
/// `//`, so this single check excludes all three, and the crate's source
/// contains no string literal with `//` in it, so the simplification is
/// exact here even though it would not be in general (same discipline as
/// the alchemy precedent this test is modelled on).
fn code_part(line: &str) -> &str {
    match line.find("//") {
        Some(idx) => &line[..idx],
        None => line,
    }
}

/// True if `needle` appears in `haystack` as its own word — not preceded or
/// followed by an ASCII letter, digit, or underscore.
fn contains_word(haystack: &str, needle: &str) -> bool {
    let bytes = haystack.as_bytes();
    let nlen = needle.len();
    let is_word_byte = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    let mut start = 0;
    while let Some(rel) = haystack[start..].find(needle) {
        let at = start + rel;
        let before_ok = at == 0 || !is_word_byte(bytes[at - 1]);
        let after = at + nlen;
        let after_ok = after >= bytes.len() || !is_word_byte(bytes[after]);
        if before_ok && after_ok {
            return true;
        }
        start = at + 1;
    }
    false
}

/// The lot window draws nothing: no source file under `windows/lot/src/`
/// mentions the word `Stream` in live code. A later change that threaded a
/// fresh draw through this window — an unregistered save-format contract —
/// must fail this test, not just contradict the module docs' prose.
#[test]
fn the_lot_window_draws_nothing() {
    for path in rust_files(&src_dir()) {
        let content =
            fs::read_to_string(&path).unwrap_or_else(|e| panic!("reading {}: {e}", path.display()));
        for (lineno, line) in content.lines().enumerate() {
            let code = code_part(line);
            assert!(
                !contains_word(code, "Stream"),
                "{}:{}: found the word `Stream` in live code — the lot window draws \
                 nothing new (spec §7); a draw here is an unregistered save-format \
                 contract:\n    {line}",
                path.display(),
                lineno + 1,
            );
        }
    }
}

#[cfg(test)]
mod word_boundary_self_test {
    use super::contains_word;

    #[test]
    fn matches_a_bare_word_but_not_a_substring_of_a_longer_identifier() {
        assert!(contains_word("let s = Stream::new();", "Stream"));
        assert!(contains_word("(Stream)", "Stream"));
        assert!(!contains_word("upstream flows", "Stream"));
        assert!(!contains_word("a streaming api", "Stream"));
        assert!(!contains_word("StreamLabel is a different word", "Stream"));
    }
}
