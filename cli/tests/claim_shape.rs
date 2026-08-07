//! Default-deny claim-shape tags on seed-looping tests (The Assay).
//!
//! A test that iterates seeds is making a claim with a quantifier, and the
//! quantifier decides which instrument should hold it (spec §5). Decision 0093
//! settled that a test sweeping seeds to FIND an instance is doing the
//! census's job badly and the synthetic's job expensively; this lint keeps
//! that settlement findable instead of tribal, by requiring every seed loop to
//! say which shape it is.
//!
//! It cannot check that a declared shape is the RIGHT one — that is a review
//! question. It checks that the author answered it. That is the same limit
//! `cli/tests/heavy_tier.rs`'s module doc names for token guards: canonical is
//! not current.
//!
//! ## What this scan covers, and what it cannot see
//!
//! The scan is a std-only, source-level walk of every `.rs` file in the repo
//! (the same `collect_rs` shape `cli/tests/heavy_tier.rs` uses): for every
//! `#[test]` function, it looks at the function body for three signals —
//! a `for <seed-shaped ident> in ...` loop, a closure (`.map`/`.for_each`/
//! `.flat_map`/`.filter_map`/`.any`/`.all`/`.filter`) whose parameter is
//! seed-shaped, a `SEEDS`-like ALL-CAPS constant, or a call through
//! `map_seeds` — and, if any fire, requires a `claim:` tag in the doc-comment
//! block directly above the function.
//!
//! **Known blind spot: a world built by subprocess is invisible to what this
//! scan can VERIFY, even on the rare occasion it flags the loop.**
//! `cli/tests/sky_exit_criterion.rs::graded_pins_never_fail_above_min` loops
//! seeds `1..=20` and builds a world per seed by *spawning the CLI as a
//! subprocess* (Task 1's audit: no `build_world|generate(|BuildDepth::`
//! world-building regex can see that as a world build at all). This scan
//! happens to flag it anyway, because its loop variable is spelled `seed` —
//! but that is a coincidence of naming, not evidence the scan understands
//! what the loop does. A test that built worlds the same way through a
//! non-seed-named binding (`for n in 1..=20`, or a hardcoded per-call
//! argument with no loop at all) would be genuinely invisible, and nothing
//! here rules that shape out elsewhere in the tree. Once a subprocess is
//! involved, no source-level scan — regex, token, or this one — can see
//! across the process boundary to confirm the tag it demands describes what
//! the spawned binary actually did. This is the same class of admission
//! `heavy_tier.rs` makes for its own token guards: canonical is not current,
//! and a scan that is silent about its blind spot is worse than one that
//! names it.

use std::fs;
use std::path::{Path, PathBuf};

/// The sanctioned claim shapes (spec §5). Adding one is a review event.
const SHAPES: [&str; 7] = [
    "reachability",
    "rate",
    "invariant",
    "behavior",
    "readout",
    "structural",
    "sanctioned-sweep",
];

/// Does this line open a `claim:` tag with a sanctioned shape?
fn claim_shape(line: &str) -> Option<&'static str> {
    let rest = line
        .trim()
        .strip_prefix("//")?
        .trim_start_matches('/')
        .trim();
    let rest = rest.strip_prefix("claim:")?.trim();
    SHAPES.iter().copied().find(|shape| rest.starts_with(shape))
}

/// A seed-looping test with no sanctioned `claim:` tag.
struct Untagged {
    file: String,
    test: String,
}

/// Is `ident` a seed-shaped binding name? Covers the idioms seen across the
/// tree: `seed`, `seeds`, the single-letter `s`, and any identifier that
/// contains `seed` as a substring (`raw_seed`, `seed_val`, …).
fn seed_shaped(ident: &str) -> bool {
    let lower = ident.to_lowercase();
    lower == "s" || lower.contains("seed")
}

/// Is `tok` a `SEEDS`-like ALL-CAPS constant name? Plural-shaped
/// (`SEEDS`, `MEASURED_SEEDS`, `DIVERGENCE_SEEDS`) rather than a single named
/// seed (`CRISIS_SEED`), which is not itself a loop signal.
fn looks_like_seeds_const(tok: &str) -> bool {
    tok.len() >= 4
        && tok
            .chars()
            .all(|c| c.is_ascii_uppercase() || c == '_' || c.is_ascii_digit())
        && tok.contains("SEED")
        && tok.ends_with('S')
}

/// Split `body` into identifier tokens (runs of ASCII alphanumerics and `_`),
/// discarding everything else. Byte-indexed slicing is safe here because a
/// slice boundary is only ever taken immediately before or after a run of
/// single-byte ASCII characters, which are always valid UTF-8 boundaries.
fn tokenize(body: &str) -> Vec<&str> {
    let mut tokens = Vec::new();
    let bytes = body.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' {
            let start = i;
            while i < bytes.len() && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_') {
                i += 1;
            }
            tokens.push(&body[start..i]);
        } else {
            i += 1;
        }
    }
    tokens
}

/// Does `body` contain a closure (`.map`/`.flat_map`/`.for_each`/
/// `.filter_map`/`.any`/`.all`/`.filter`/`.find_map`) whose first parameter
/// is seed-shaped? A raw substring scan, since `|` is not an identifier
/// character and so is invisible to [`tokenize`].
fn has_seed_closure(body: &str) -> bool {
    const METHODS: [&str; 8] = [
        ".map(",
        ".flat_map(",
        ".for_each(",
        ".filter_map(",
        ".find_map(",
        ".any(",
        ".all(",
        ".filter(",
    ];
    for method in METHODS {
        let mut cursor = 0;
        while let Some(rel) = body[cursor..].find(method) {
            let after = cursor + rel + method.len();
            let rest = body[after..].trim_start();
            if let Some(params_and_more) = rest.strip_prefix('|')
                && let Some(end) = params_and_more.find('|')
            {
                let params = &params_and_more[..end];
                for part in params.split(',') {
                    let ident = part.trim().trim_start_matches('&').trim();
                    let ident = ident.strip_prefix("mut ").unwrap_or(ident).trim();
                    if seed_shaped(ident) {
                        return true;
                    }
                }
            }
            cursor = after;
        }
    }
    false
}

/// Does `body` contain a `for <seed-shaped ident> in ...` loop?
fn has_seed_for_loop(tokens: &[&str]) -> bool {
    let mut k = 0;
    while k < tokens.len() {
        if tokens[k] == "for" {
            let mut idx = k + 1;
            if idx < tokens.len() && tokens[idx] == "mut" {
                idx += 1;
            }
            if idx + 1 < tokens.len() && tokens[idx + 1] == "in" && seed_shaped(tokens[idx]) {
                return true;
            }
        }
        k += 1;
    }
    false
}

/// Is a function body seed-looping? Three independent signals (module doc):
/// a `for` loop over a seed-shaped binding, a closure with a seed-shaped
/// parameter, a `SEEDS`-like constant, or a call through `map_seeds`.
fn is_seed_looping(body: &str) -> bool {
    let tokens = tokenize(body);
    if tokens.contains(&"map_seeds") {
        return true;
    }
    if tokens.iter().any(|&t| looks_like_seeds_const(t)) {
        return true;
    }
    if has_seed_for_loop(&tokens) {
        return true;
    }
    has_seed_closure(body)
}

/// Does `line`, trimmed, open a doc comment or an attribute? Both are part of
/// the contiguous block immediately above a function that this scan reads
/// for a `#[test]` marker and a `claim:` tag.
fn is_block_line(line: &str) -> bool {
    let t = line.trim();
    t.starts_with("#[") || t.starts_with("//")
}

/// If `line` (trimmed of leading whitespace and any `pub`/`pub(crate)`/
/// `async`/`const`/`unsafe` modifiers) opens a function, return its name.
/// Deliberately indentation-agnostic — this scan reads whole-repo sources,
/// including `#[cfg(test)] mod tests { … }` blocks nested inside `src/`,
/// where a top-level-by-column heuristic would miss every indented test.
fn fn_name(line: &str) -> Option<&str> {
    let mut rest = line.trim_start();
    loop {
        rest = if let Some(r) = rest.strip_prefix("pub(crate) ") {
            r
        } else if let Some(r) = rest.strip_prefix("pub ") {
            r
        } else if let Some(r) = rest.strip_prefix("async ") {
            r
        } else if let Some(r) = rest.strip_prefix("const ") {
            r
        } else if let Some(r) = rest.strip_prefix("unsafe ") {
            r
        } else {
            break;
        };
    }
    let rest = rest.strip_prefix("fn ")?;
    let end = rest
        .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
        .unwrap_or(rest.len());
    if end == 0 { None } else { Some(&rest[..end]) }
}

/// The text of the function opening at `lines[start]`, from its first `{` to
/// the matching `}` (brace-depth counting; does not special-case string or
/// comment literals — acceptable for this scan's purpose, since a false
/// balance would need an unbalanced literal brace inside the very function
/// being scanned, which no test file in this tree does).
fn function_body(lines: &[&str], start: usize) -> String {
    let mut depth: i32 = 0;
    let mut started = false;
    let mut body = String::new();
    for line in &lines[start..] {
        body.push_str(line);
        body.push('\n');
        for ch in line.chars() {
            match ch {
                '{' => {
                    depth += 1;
                    started = true;
                }
                '}' => depth -= 1,
                _ => {}
            }
        }
        if started && depth <= 0 {
            break;
        }
    }
    body
}

/// Scan one source file's text. A test is seed-looping if its body contains a
/// `for` over a numeric range, a `SEEDS`-like constant, or `map_seeds`.
fn untagged_in(path: &str, text: &str) -> Vec<Untagged> {
    let lines: Vec<&str> = text.lines().collect();
    let mut out = Vec::new();
    for (i, &line) in lines.iter().enumerate() {
        let Some(name) = fn_name(line) else { continue };

        // Walk upward collecting the contiguous attribute/doc-comment block.
        let mut block: Vec<&str> = Vec::new();
        let mut j = i;
        while j > 0 && is_block_line(lines[j - 1]) {
            block.push(lines[j - 1]);
            j -= 1;
        }

        let is_test = block
            .iter()
            .any(|l| l.trim().replace(' ', "").starts_with("#[test]"));
        if !is_test {
            continue;
        }

        let tagged = block.iter().any(|l| claim_shape(l).is_some());
        if tagged {
            continue;
        }

        let body = function_body(&lines, i);
        if is_seed_looping(&body) {
            out.push(Untagged {
                file: path.to_string(),
                test: name.to_string(),
            });
        }
    }
    out
}

/// Recursively collect every `.rs` file under `dir`, skipping `target/` and
/// dot-directories — the same walker shape as `cli/tests/heavy_tier.rs`'s
/// `collect_rs` (`cli/tests/heavy_tier.rs:79`).
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

#[test]
fn the_tag_parser_accepts_every_shape_and_rejects_near_misses() {
    assert_eq!(
        claim_shape("/// claim: rate(census: crisis-fires, [0.01, 0.9])"),
        Some("rate")
    );
    assert_eq!(
        claim_shape("// claim: invariant(forall-seed)"),
        Some("invariant")
    );
    assert_eq!(
        claim_shape("    /// claim: structural(seed: 42)"),
        Some("structural")
    );
    assert_eq!(
        claim_shape("/// claim: reachability(census: hydro-variant-coverage)"),
        Some("reachability")
    );
    // Near misses must all be refusals, not silent passes.
    assert_eq!(claim_shape("/// claim: whatever"), None);
    assert_eq!(claim_shape("/// claims: rate(...)"), None);
    assert_eq!(claim_shape("/// this test claims a rate"), None);
    assert_eq!(claim_shape("let claim = \"rate\";"), None);
    assert_eq!(claim_shape(""), None);
}

/// claim: structural(scanner self-test) — this test's body carries the fixture
/// string `"for seed in 0..8u64 { ... }"` as literal text, not code, but the
/// scanner in this file reads whole-file bytes and does not distinguish a
/// string literal from a real loop (module doc, "does not special-case
/// string or comment literals"). That makes this test self-referentially
/// seed-looping by the scanner's own signal, though it builds zero worlds and
/// samples nothing — it drives a pure function over three fixed fixture
/// strings, once each.
#[test]
fn the_scan_flags_an_untagged_seed_loop_and_passes_a_tagged_one() {
    let untagged = untagged_in(
        "sample.rs",
        "#[test]\nfn sweeps() {\n    for seed in 0..8u64 { let _ = seed; }\n}\n",
    );
    assert_eq!(untagged.len(), 1, "an untagged seed loop must be flagged");
    assert_eq!(untagged[0].test, "sweeps");

    let tagged = untagged_in(
        "sample.rs",
        "/// claim: invariant(forall-seed)\n#[test]\nfn sweeps() {\n    \
         for seed in 0..8u64 { let _ = seed; }\n}\n",
    );
    assert!(tagged.is_empty(), "a tagged seed loop must pass");

    let no_loop = untagged_in(
        "sample.rs",
        "#[test]\nfn plain() {\n    assert!(true);\n}\n",
    );
    assert!(no_loop.is_empty(), "a test with no seed loop needs no tag");
}

#[test]
fn every_seed_looping_test_in_the_repo_declares_its_claim_shape() {
    let mut sources = Vec::new();
    collect_rs(std::path::Path::new(".."), &mut sources);
    let mut untagged = Vec::new();
    for path in &sources {
        let text = std::fs::read_to_string(path).expect("source is readable");
        untagged.extend(untagged_in(&path.to_string_lossy(), &text));
    }
    untagged.sort_by(|a, b| (&a.file, &a.test).cmp(&(&b.file, &b.test)));
    let listed: Vec<String> = untagged
        .iter()
        .map(|u| format!("  {}::{}", u.file, u.test))
        .collect();
    assert!(
        untagged.is_empty(),
        "these tests iterate seeds without declaring a claim shape. A seed loop is a \
         quantified claim, and the quantifier decides the instrument (decision 0093). \
         Add a doc-comment line `/// claim: <shape>(...)` with one of {SHAPES:?} — see \
         docs/audits/the-assay-build-volume-audit.md for each test's assigned shape.\n{}",
        listed.join("\n")
    );
}
