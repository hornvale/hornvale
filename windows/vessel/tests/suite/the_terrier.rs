//! The Terrier (spec §3.3): no session path re-surveys the world's
//! occupation register.
//!
//! `brief::brief_of` used to call `hornvale_worldgen::occupations_by_vertex`
//! — a reconstruction of EVERY committed occupation from the ledger — on
//! every call, and a chamber turn called it two to five times, at 8.7-28.8
//! ms each. That was the whole of what The Rack priced as "one shadowcast".
//! The map now lives on `WorldContext`, built once.
//!
//! A structural source scan, because the property is about what the code
//! does NOT contain, which no runtime assertion can witness
//! (`affordance.rs::no_verb_by_object_table_exists` and
//! `underground.rs::the_reach_seam_is_the_only_source_of_the_radius` are
//! this repo's precedents for the shape). A `TurnWork` counter was
//! considered and rejected: after the hoist no turn path reconstructs an
//! occupation, so the counter would have no writer — the permanently-green
//! zero The Rack argued against (decision 0598).
//!
//! **Direction this check enforces:** it forbids the PRESENCE of a
//! whole-world occupation read anywhere under `windows/vessel/src` except
//! inside `WorldContext::build`'s body. It does not prove the hoisted map is
//! complete or current — `session.rs`'s in-module
//! `the_hoisted_brief_is_the_fresh_brief_at_every_visited_locale` does that.
//!
//! Not in the commit gate until the next green chamber job rewrites
//! `docs/timings/subfloor-roster.tsv`; runs in the stage gate from its first
//! commit.

use std::path::{Path, PathBuf};

/// The three ways `windows/worldgen` hands back the world's occupations.
/// All three rescan `world.ledger` in full (`history_emit.rs:514-535, 573-584`).
const WHOLE_WORLD_READERS: [&str; 3] = [
    "occupations_by_vertex(",
    "occupations_at(",
    "occupation_records(",
];

/// Every `.rs` file under `windows/vessel/src`, recursively, sorted so a
/// failure names the same file on every box.
fn production_sources() -> Vec<PathBuf> {
    fn walk(dir: &Path, out: &mut Vec<PathBuf>) {
        let mut entries: Vec<_> = std::fs::read_dir(dir)
            .unwrap_or_else(|e| panic!("read_dir {}: {e}", dir.display()))
            .map(|e| e.expect("dir entry").path())
            .collect();
        entries.sort();
        for path in entries {
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|x| x == "rs") {
                out.push(path);
            }
        }
    }
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("src");
    let mut out = Vec::new();
    walk(&root, &mut out);
    assert!(
        out.len() > 20,
        "positive control on the walk: only {} files under {} — the scan is \
         not looking at the crate",
        out.len(),
        root.display()
    );
    out
}

/// The file's production half with every comment line removed: everything
/// before the first `#[cfg(test)]`, minus lines whose trimmed form starts
/// with `//`. Comments are dropped so that a doc comment may NAME the
/// forbidden functions (brief.rs's rewritten cost note does) without the
/// scan reading prose as a call.
fn production_code(src: &str) -> String {
    src.split("#[cfg(test)]")
        .next()
        .expect("split always yields one piece")
        .lines()
        .filter(|l| !l.trim_start().starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// The brace-balanced body that follows `needle` in `src`: from the first
/// `{` after the needle to its matching `}`, inclusive. `None` if the needle
/// is absent. Naive about braces inside string literals, which is fine for
/// `WorldContext::build` (its only literals are error messages without
/// braces) and asserted by the controls in the test below.
fn block_body_after<'a>(src: &'a str, needle: &str) -> Option<&'a str> {
    let start = src.find(needle)? + needle.len();
    let open = start + src[start..].find('{')?;
    let mut depth = 0usize;
    for (i, ch) in src[open..].char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(&src[open..open + i + 1]);
                }
            }
            _ => {}
        }
    }
    None
}

/// The forbidden names appear in production code ONLY inside
/// `WorldContext::build`.
///
/// MUTATION THIS MUST FAIL AGAINST: restore the per-call read —
/// in `brief.rs`, replace `occupations.get(&vertex)` with
/// `hornvale_worldgen::occupations_by_vertex(world).get(&vertex)` (adding a
/// `world: &World` parameter). Observed red, against the pre-hoist tree at
/// `aeabc4549`:
/// ```text
/// running 3 tests
/// test the_terrier::the_block_extractor_matches_nested_braces ... ok
/// test the_terrier::the_register_scanner_catches_a_per_call_read ... ok
/// test the_terrier::no_session_path_re_surveys_the_occupation_register ... FAILED
///
/// failures:
///
/// ---- the_terrier::no_session_path_re_surveys_the_occupation_register stdout ----
///
/// thread 'the_terrier::no_session_path_re_surveys_the_occupation_register' (190835536) panicked at windows/vessel/tests/suite/the_terrier.rs:157:5:
/// positive control: WorldContext::build must build the register with occupations_by_vertex
/// note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
///
///
/// failures:
///     the_terrier::no_session_path_re_surveys_the_occupation_register
///
/// test result: FAILED. 2 passed; 1 failed; 0 ignored; 0 measured; 412 filtered out; finished in 0.00s
/// ```
#[test]
fn no_session_path_re_surveys_the_occupation_register() {
    let session =
        std::fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("src/session.rs"))
            .expect("session.rs");
    let session_code = production_code(&session);
    let build = block_body_after(&session_code, "pub fn build(world: &'w World)")
        .expect("session.rs must define WorldContext::build(world: &'w World)");
    // Controls on the extraction: the body we cut is the whole of `build`
    // and nothing past it.
    assert!(
        build.contains("Ok(WorldContext {"),
        "the extracted build body does not reach its own Ok(WorldContext {{ … }})"
    );
    assert!(
        !build.contains("fn context("),
        "the extracted build body overran into the next method"
    );
    // Positive control: the one permitted site does name a reader, so an
    // emptied scan cannot read as green.
    assert!(
        build.contains("occupations_by_vertex("),
        "positive control: WorldContext::build must build the register with \
         occupations_by_vertex"
    );

    let mut offenders = Vec::new();
    for path in production_sources() {
        let src = std::fs::read_to_string(&path).expect("source file");
        let mut code = production_code(&src);
        if path.ends_with("session.rs") {
            // Blank out the permitted block, keeping the rest of the file.
            code = code.replacen(build, "", 1);
        }
        for (n, line) in code.lines().enumerate() {
            for reader in WHOLE_WORLD_READERS {
                if line.contains(reader) {
                    offenders.push(format!(
                        "{}:{}: {}",
                        path.strip_prefix(env!("CARGO_MANIFEST_DIR"))
                            .unwrap_or(&path)
                            .display(),
                        n + 1,
                        line.trim()
                    ));
                }
            }
        }
    }
    assert!(
        offenders.is_empty(),
        "a session path re-surveys the world's occupation register; the map \
         is built once on WorldContext and read from there (The Terrier, \
         spec §3.1):\n{}",
        offenders.join("\n")
    );
}

/// Positive control for the scanner: a line-scan that never matched would
/// pass the test above for the wrong reason.
#[test]
fn the_register_scanner_catches_a_per_call_read() {
    let offending = "let alive = hornvale_worldgen::occupations_by_vertex(world).remove(&v);";
    assert!(
        WHOLE_WORLD_READERS.iter().any(|r| offending.contains(r)),
        "positive control: the reader list must match a real call"
    );
    let prose_only = "// hoisted; occupations_by_vertex( is built once in WorldContext::build";
    assert!(
        production_code(prose_only).is_empty(),
        "a comment line must be dropped before the scan, or the rewritten \
         cost note would trip it"
    );
}

/// Control on the body extractor over a shape with nested braces.
#[test]
fn the_block_extractor_matches_nested_braces() {
    let src = "fn a() { let x = S { y: 1 }; if x.y == 1 { 2 } else { 3 } }\nfn b() { 0 }";
    assert_eq!(
        block_body_after(src, "fn a()"),
        Some("{ let x = S { y: 1 }; if x.y == 1 { 2 } else { 3 } }")
    );
    assert_eq!(block_body_after(src, "fn zzz()"), None);
}
