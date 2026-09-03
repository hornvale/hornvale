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

/// The file's production half, up to its test MODULE: every comment line
/// (trimmed form starting with `//`) is BLANKED — replaced with an empty
/// line, not removed — so every remaining line keeps its real 1-based line
/// number in the file. Comments are blanked, not deleted, so that (a) a doc
/// comment may NAME the forbidden functions (`brief.rs`'s rewritten cost
/// note does) without the scan reading prose as a call, and (b) an offender
/// found in the truncated text is reported at the SAME line number a reader
/// sees by opening the file.
///
/// The split point is the real test MODULE boundary — a `#[cfg(test)]`
/// (optionally followed by more attributes, e.g. `#[path = "…"]`)
/// immediately followed by a `mod …` line — never the first `#[cfg(test)]`
/// attribute of any kind. A `#[cfg(test)]` on a `fn`/`const`/`impl` item does
/// NOT end the span; that is the CONSERVATIVE direction, because a test-only
/// helper that itself called a whole-world reader would then still be
/// scanned and flagged, which is the right failure mode for a check whose
/// entire job is not missing a call. If no such module boundary exists, the
/// whole file is production.
///
/// **Why this matters, measured against this crate's own files (fix round
/// 2):** an earlier draft split at the FIRST `#[cfg(test)]` of any kind, and
/// that silently discarded real production code in three of this crate's own
/// files:
/// - `liveness.rs`: the first `#[cfg(test)]` gates `fn alarm_at` at line
///   4353, while the real `mod tests` sits at line 8490 — everything between,
///   ~4,100 lines, including `species_activity` (line 8295), went unscanned.
/// - `roster.rs`: the first gates `pub(crate) fn driven_body_mut` at line
///   237, while `mod tests` sits at line 446 — `on_roll_others` (line 437)
///   went unscanned.
/// - `session.rs`: the first occurrence of the literal token `#[cfg(test)]`
///   is inside a DOC COMMENT at line 10439 (`session.rs`'s own note about
///   `COMPASS_SQUARE`). That one was harmless only by luck — the real module
///   follows at line 10594 with nothing whole-world-reading between them —
///   and only becomes safe in general once comments are blanked before the
///   module search runs, rather than the attribute being matched as a raw
///   substring of the file.
fn production_code(src: &str) -> String {
    let lines: Vec<&str> = src.lines().collect();
    let mut end = lines.len();
    for i in 0..lines.len() {
        if lines[i].trim() != "#[cfg(test)]" {
            continue;
        }
        let mut j = i + 1;
        while j < lines.len() && lines[j].trim_start().starts_with("#[") {
            j += 1;
        }
        if j < lines.len() && lines[j].trim_start().starts_with("mod ") {
            end = i;
            break;
        }
    }
    lines[..end]
        .iter()
        .map(|l| {
            if l.trim_start().starts_with("//") {
                ""
            } else {
                *l
            }
        })
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
/// MUTATION THIS MUST FAIL AGAINST: restore a per-call re-survey on a
/// session path — in `Session::brief_here` (`session.rs`), add as its FIRST
/// statement `let _re_survey = hornvale_worldgen::occupations_by_vertex(self.world);`.
/// This is a one-line, compiling, EXTRA re-survey rather than a reversion of
/// `brief_of`'s signature, and it is the more honest mutation for THIS test:
/// this test's own job is the offender scan below, not the positive control
/// above it (a narrower earlier draft named a `brief.rs` signature reversion
/// instead — reverting the caller's argument type — but that mutation
/// exercises the positive control's assertion (`build.contains("occupations_by_vertex(")`), not the
/// offender-scan assertion this doc sits beside; see the fix round that
/// caught the mismatch). Observed red, against the FINISHED (post-hoist)
/// tree with this mutation applied — and re-taken in fix round 2 after
/// `production_code` stopped stripping comments (which shifted line
/// numbers) and started blanking them (which preserves them): the cited
/// `src/session.rs:7232` below is now the REAL file line of the inserted
/// statement, confirmed independently with `grep -n _re_survey
/// windows/vessel/src/session.rs`:
/// ```text
/// running 4 tests
/// test the_terrier::the_block_extractor_matches_nested_braces ... ok
/// test the_terrier::the_production_span_reaches_the_test_module_not_the_first_attribute ... ok
/// test the_terrier::the_register_scanner_catches_a_per_call_read ... ok
/// test the_terrier::no_session_path_re_surveys_the_occupation_register ... FAILED
///
/// failures:
///
/// ---- the_terrier::no_session_path_re_surveys_the_occupation_register stdout ----
///
/// thread 'the_terrier::no_session_path_re_surveys_the_occupation_register' (190997382) panicked at windows/vessel/tests/suite/the_terrier.rs:272:5:
/// a session path re-surveys the world's occupation register; the map is built once on WorldContext and read from there (The Terrier, spec §3.1):
/// src/session.rs:7232: let _re_survey = hornvale_worldgen::occupations_by_vertex(self.world);
/// note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
///
///
/// failures:
///     the_terrier::no_session_path_re_surveys_the_occupation_register
///
/// test result: FAILED. 3 passed; 1 failed; 0 ignored; 0 measured; 412 filtered out; finished in 0.02s
/// ```
/// Also observed against the pre-hoist tree `aeabc4549`, before `build` named
/// the register (a different question — whether the positive control itself
/// can fail, not whether the offender scan can): `thread
/// 'the_terrier::no_session_path_re_surveys_the_occupation_register' panicked
/// at windows/vessel/tests/suite/the_terrier.rs:157:5: positive control:
/// WorldContext::build must build the register with occupations_by_vertex`
/// (that earlier line-165-era cite is from before fix round 2's line
/// renumbering too, and is quoted here for the message text only, not as a
/// current line pointer).
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
        // Coverage controls (fix round 2): a truncated production span must
        // not read green just because it found no offenders — it must
        // actually have reached a known-late function in each of the three
        // files an earlier, first-attribute-split draft under-scanned.
        if path.ends_with("session.rs") {
            assert!(
                code.contains("fn brief_here("),
                "positive control: the production span of session.rs is \
                 truncated before fn brief_here; the scan is not looking at \
                 the whole file"
            );
        }
        if path.ends_with("liveness.rs") {
            assert!(
                code.contains("fn species_activity("),
                "positive control: the production span of liveness.rs is \
                 truncated before fn species_activity; the scan is not \
                 looking at the whole file"
            );
        }
        if path.ends_with("roster.rs") {
            assert!(
                code.contains("fn on_roll_others"),
                "positive control: the production span of roster.rs is \
                 truncated before fn on_roll_others; the scan is not \
                 looking at the whole file"
            );
        }
        if path.ends_with("session.rs") {
            // Blank out the permitted block, keeping the rest of the file —
            // with the SAME number of newlines it contained, so every line
            // after it keeps its real file line number.
            let newlines = "\n".repeat(build.matches('\n').count());
            code = code.replacen(build, &newlines, 1);
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

/// The production span extends PAST a `#[cfg(test)]`-gated helper item (a
/// `fn`, not a module) to whatever production code follows it, and stops
/// only at the real test module — and blanking a comment line does not shift
/// the line numbers of what comes after it (fix round 2).
#[test]
fn the_production_span_reaches_the_test_module_not_the_first_attribute() {
    let synthetic = "// hoisted; occupations_by_vertex( is built once in WorldContext::build\n\
                      #[cfg(test)]\n\
                      fn helper() {}\n\
                      \n\
                      fn late() { let _ = occupations_at(x); }\n\
                      \n\
                      #[cfg(test)]\n\
                      #[path = \"x.rs\"]\n\
                      mod tests { occupations_by_vertex( }\n";
    let code = production_code(synthetic);
    assert!(
        code.contains("occupations_at("),
        "the production span must extend past a #[cfg(test)]-gated helper \
         fn to the production code that follows it — instead it stopped at \
         the first #[cfg(test)] attribute of any kind, which is the fix \
         round 2 defect (measured against liveness.rs and roster.rs)"
    );
    assert!(
        !code.contains("occupations_by_vertex("),
        "the production span must stop at the real test module and exclude \
         its body — a #[cfg(test)] immediately (allowing intervening \
         attributes) followed by `mod …`"
    );
    // Numbering is preserved: blanking the leading comment line must not
    // shift the line index of what follows it.
    let expected_line = synthetic
        .lines()
        .position(|l| l.contains("occupations_at("))
        .expect("synthetic source contains occupations_at(");
    let actual_line = code
        .lines()
        .position(|l| l.contains("occupations_at("))
        .expect("production text contains occupations_at(");
    assert_eq!(
        actual_line, expected_line,
        "blanking a comment line must not shift the line numbers of the \
         lines that follow it"
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
