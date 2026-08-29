//! The workspace-side half of Task 9's band-coverage enforcement (The
//! Gallery; review round 1, Important 1).
//!
//! `clients/game/core/tests/schema.rs::the_client_can_parse_every_band_the_
//! sim_emits` is a real regression check over the client's own corpus, but
//! it structurally CANNOT detect a new `SpatialChannel` variant: that crate
//! has no dependency on `hornvale-vessel` at all (the repo boundary is the
//! determinism boundary, decision 0055), so nothing there is wired to the
//! type that would actually grow. This file is where `SpatialChannel` is
//! visible, so this is where the automatic trip has to live.
//!
//! [`wire_tag_of`] is an EXHAUSTIVE match over the real
//! `hornvale_vessel::SpatialChannel` — never constructed, never called; its
//! only job is to exist as compiled code, so the match's own exhaustiveness
//! check runs on every build of this crate's test binary. The day a fourth
//! variant is added to `SpatialChannel`, THIS FILE fails to compile, which
//! fails the whole `hornvale` test binary, which is the trip nothing in
//! `clients/game` can give.
//!
//! [`KNOWN_BAND_WIRE_TAGS`] is not derived from that match mechanically —
//! doing so would mean constructing a real `SurroundsScene` / `SessionPlan`
//! / `SessionLevel` for each variant just to read its wire tag back off a
//! serialization, which is the "new fixture pipeline" this check is not
//! worth building just to avoid a three-item list kept four lines away from
//! the arms it names. The coupling is a reviewer's, not the compiler's:
//! whoever fixes the compile error [`wire_tag_of`] throws on a new variant
//! is looking straight at this list and is asked, in the comment beside it,
//! to extend it too. What the second test below verifies mechanically is
//! narrower and real: that every tag CURRENTLY in the list is actually
//! exercised somewhere in the client's own test corpus, so the list itself
//! cannot silently drift ahead of what the client can prove it reads.
//!
//! Precedent for a `cli`-crate test reading files elsewhere in the repo as
//! plain text: `cli/tests/suite/docs_consistency.rs` and
//! `cli/tests/suite/lexicon_guard.rs`.

use std::fs;
use std::path::{Path, PathBuf};

/// The repository root: `cli/tests/` lives in the `cli` crate, whose
/// manifest dir is `<root>/cli`, so the root is its parent.
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli crate should sit under the repo root")
        .to_path_buf()
}

/// Every wire tag `SpatialChannel` (`windows/vessel/src/snapshot.rs`) can
/// emit, one per arm. See the module doc: this function is never called —
/// only compiled — so its exhaustiveness check is the enforcement.
#[allow(dead_code)]
fn wire_tag_of(v: &hornvale_vessel::SpatialChannel) -> &'static str {
    match v {
        hornvale_vessel::SpatialChannel::Walk { .. } => "walk",
        hornvale_vessel::SpatialChannel::Chamber { .. } => "chamber",
        hornvale_vessel::SpatialChannel::Underground { .. } => "underground",
    }
}

/// The tag set [`wire_tag_of`]'s arms name. See the module doc for why this
/// is a hand-kept list beside the match rather than one mechanically
/// derived from it.
const KNOWN_BAND_WIRE_TAGS: &[&str] = &["walk", "chamber", "underground"];

/// Recursively collect the text of every `.json` and `.rs` file under `dir`
/// into one haystack, skipping build output. Deliberately whole-directory
/// rather than a fixed file list: a future real underground fixture (rather
/// than the synthetic literal `tests/schema.rs` carries today) is picked up
/// with no edit needed here.
fn collect_text(dir: &Path, out: &mut String) {
    let Ok(entries) = fs::read_dir(dir) else {
        return;
    };
    let mut paths: Vec<PathBuf> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    paths.sort();
    for path in paths {
        if path
            .file_name()
            .is_some_and(|n| n == "target" || n == ".git")
        {
            continue;
        }
        if path.is_dir() {
            collect_text(&path, out);
        } else if path.extension().is_some_and(|e| e == "json" || e == "rs")
            && let Ok(text) = fs::read_to_string(&path)
        {
            out.push_str(&text);
            out.push('\n');
        }
    }
}

/// Whether `haystack` carries a `"band": "<tag>"` document tag, in either
/// spelling this repo's own JSON actually uses: compact (the committed
/// fixtures) or spaced (the pretty hand-authored literal in
/// `clients/game/core/tests/schema.rs`). No JSON parse is attempted — the
/// corpus mixes whole documents with a Rust source file that merely
/// CONTAINS one as a string literal, and a textual check is what precedent
/// (`docs_consistency.rs`, `lexicon_guard.rs`) already uses for exactly
/// this kind of cross-file check.
fn corpus_carries_band_tag(haystack: &str, tag: &str) -> bool {
    haystack.contains(&format!("\"band\":\"{tag}\""))
        || haystack.contains(&format!("\"band\": \"{tag}\""))
}

/// A malformed-list guard: every declared tag is non-empty. Cheap, and
/// keeps [`KNOWN_BAND_WIRE_TAGS`] itself under a named test rather than
/// only ever being read by the coverage test below.
#[test]
fn the_known_tag_list_is_well_formed() {
    assert!(
        !KNOWN_BAND_WIRE_TAGS.is_empty(),
        "KNOWN_BAND_WIRE_TAGS must name at least one band"
    );
    for tag in KNOWN_BAND_WIRE_TAGS {
        assert!(!tag.is_empty(), "KNOWN_BAND_WIRE_TAGS carries an empty tag");
    }
}

/// Every tag [`KNOWN_BAND_WIRE_TAGS`] currently names is actually exercised
/// somewhere in the client's own test corpus — the mechanical half. If a
/// human adds a `SpatialChannel` variant, fixes the compile break in
/// [`wire_tag_of`], extends this list, and stops there, this test is what
/// still catches the missing client-side coverage.
#[test]
fn every_known_band_wire_tag_is_covered_by_the_clients_own_corpus() {
    let root = repo_root();
    let corpus_dir = root.join("clients/game/core/tests");
    assert!(
        corpus_dir.is_dir(),
        "VACUOUS TEST GUARD: {} must exist, or this test checks nothing",
        corpus_dir.display()
    );

    let mut haystack = String::new();
    collect_text(&corpus_dir, &mut haystack);
    assert!(
        !haystack.is_empty(),
        "VACUOUS TEST GUARD: the client test corpus read as empty"
    );

    for tag in KNOWN_BAND_WIRE_TAGS {
        assert!(
            corpus_carries_band_tag(&haystack, tag),
            "no file under {} carries a band=\"{tag}\" document tag (compact \
             or spaced) — the client's own test corpus does not actually \
             exercise a document tagged this way",
            corpus_dir.display()
        );
    }
}
