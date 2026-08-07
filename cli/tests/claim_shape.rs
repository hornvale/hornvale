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
//! `#[test]` function, it looks at the function body for five signals — a
//! `for <pattern> in ...` loop whose pattern binds a seed-shaped identifier
//! ANYWHERE in it (so a tuple pattern like `for (seed, expected) in [...]`
//! counts the same as `for seed in ...`), a `for` loop whose binding is
//! later passed to `Seed(...)` even when the binding itself isn't
//! seed-shaped by name (`for i in 0..N { … Seed(i) … }`), a closure
//! (`.map`/`.for_each`/`.flat_map`/`.filter_map`/`.any`/`.all`/`.filter`)
//! whose parameter is seed-shaped, a `SEEDS`-like ALL-CAPS constant, or a
//! call through `map_seeds` — and, if any fire, requires a `claim:` tag in
//! the doc-comment block directly above the function.
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
//!
//! ## Residual detection gaps, consolidated here so a reader finds them all
//! in one place instead of scattered across function docs
//!
//! Each of these is verified (traced, grepped, or measured against the real
//! scanner), not merely asserted — see the named function's own doc for the
//! full trace.
//!
//! 1. **A `for`-pattern binding 9 or more identifiers is invisible.**
//!    [`for_binding_idents`]'s `MAX_PATTERN_TOKENS = 8` window returns `None`
//!    for a 9-element tuple pattern even when one of the 9 is spelled
//!    `seed` — read as "not a for-loop at all," the same as `impl Trait for
//!    Type`. No live pattern in this tree binds that many idents at this
//!    commit.
//! 2. **A nested `/* ... */` block comment miscounts depth.**
//!    `BraceState::in_block_comment` does not handle nesting, so
//!    `/* a /* b */ } */` closes it at the wrong `*/` and reads the `}`
//!    between the two closers as live code. This corrupts only the
//!    CURRENT function's own extracted body (dropping or extending it,
//!    never hiding a later function's own detection — see that field's
//!    doc for the distinction and how it was verified). No live nested
//!    block comment exists in a `#[test]` body in this tree at this
//!    commit.
//! 3. **A raw string with an odd embedded-quote count, and a brace inside
//!    it, leaks past its own end.** [`count_braces`] does not recognize
//!    `r"..."`/`r#"..."#` as a string boundary; an embedded `"` toggles its
//!    string-tracking regardless. An EVEN embedded-quote count resettles
//!    correctly by the literal's true end regardless of any internal
//!    miscount; an ODD count does not, and over-extends past it — but only
//!    if a brace also falls inside the mis-toggled window. A workspace-wide
//!    scan (149 `r#"..."#` literals at this commit) found none matching
//!    that shape.
//! 4. **A test that iterates a SEEDED HELPER's return value, without itself
//!    writing a seed-shaped binding or a local `Seed(...)` call, is
//!    invisible — demonstrated live, not hypothetical.**
//!    `windows/vessel/src/lattice/classify.rs`'s and `render.rs`'s
//!    `corpus()` functions each loop `for s in SEEDS { … Seed(s) … }`
//!    internally, so every test that consumes `corpus()`'s output makes a
//!    real seed-sweep claim — but a caller that destructures the result
//!    with `_` instead of `s` (or via `.find()`/`.into_iter().find()`
//!    rather than a `for` loop) carries no seed-shaped token and no local
//!    `Seed(...)` call of its own, so nothing here fires. Nine confirmed,
//!    untagged, at this commit:
//!    `classify.rs:385,413,467,656` and `render.rs:221,254,302,336,441`.
//!    **Deliberately not fixed**: detecting that a *called* function
//!    sweeps seeds is interprocedural analysis, out of scope for a
//!    source-level, per-function scan like this one.
//! 5. **The detected-count floor (`DETECTED_SEED_LOOPING_FLOOR`) is a
//!    floor, not an exact count**, so its SLACK against the live total
//!    grows every time a seed-looping test is added without a matching
//!    floor bump — see that constant's own doc for why `>=` is the right
//!    check here anyway, and what a large slack would mean.

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

/// Collect the identifier tokens bound by the `for` pattern at
/// `tokens[for_idx]` (which must equal `"for"`), up to but excluding the
/// matching `in` token. `tokenize` drops parens/commas/`&`, so a tuple
/// pattern like `for (seed, expected) in [...]` arrives as the flat run
/// `["seed", "expected"]` — exactly the shape a single-binding check missed
/// (Fix round 1, Critical, Class 1: a destructured `seed` used to be
/// invisible even though it is spelled exactly `seed`, because the old check
/// looked only at the one token immediately after `for`/`mut`). `mut` is
/// filtered out rather than treated as a binding.
///
/// Returns `None` if no `in` turns up within a short window — almost
/// certainly not a real `for` loop (the leading case being `impl Trait for
/// Type`, where "for" is a keyword with no `in` anywhere nearby) — so this
/// can't misattribute a much later, unrelated `in` to a for-loop that never
/// had one.
///
/// **Residual silent miss, named in the module doc's "Residual detection
/// gaps" list (item 1):** `MAX_PATTERN_TOKENS` bounds that window, and a
/// pattern binding strictly more idents than the window allows returns
/// `None` — a real for-loop, not a false `impl … for …`, read as "not a
/// for-loop at all." Measured directly against this function: a 9-element
/// tuple pattern like `for (a, b, c, d, e, f, g, h, i) in …` is NOT
/// detected even when one of those 9 is spelled `seed`, while an
/// 8-element one is. No live pattern in this tree binds that many idents
/// at this commit.
fn for_binding_idents<'a>(tokens: &[&'a str], for_idx: usize) -> Option<Vec<&'a str>> {
    const MAX_PATTERN_TOKENS: usize = 8;
    let mut idents = Vec::new();
    let mut i = for_idx + 1;
    while i < tokens.len() && idents.len() <= MAX_PATTERN_TOKENS {
        if tokens[i] == "in" {
            return Some(idents);
        }
        if tokens[i] != "mut" {
            idents.push(tokens[i]);
        }
        i += 1;
    }
    None
}

/// Does `tokens` contain a `for` loop whose pattern binds a seed-shaped
/// identifier ANYWHERE in the pattern — not only as its sole binding, so a
/// tuple pattern like `for (seed, expected) in [...]` is caught the same way
/// `for seed in ...` is.
fn has_seed_for_loop(tokens: &[&str]) -> bool {
    let mut k = 0;
    while k < tokens.len() {
        if tokens[k] == "for"
            && let Some(idents) = for_binding_idents(tokens, k)
            && idents.iter().any(|&ident| seed_shaped(ident))
        {
            return true;
        }
        k += 1;
    }
    false
}

/// Every identifier token that appears ANYWHERE inside a `Seed(...)` call's
/// argument span — the whole parenthesized expression (paren-depth
/// matched, so nested calls like `Seed(0x51ED ^ u64::from(octaves))` are
/// walked past correctly), not merely the first token.
///
/// Fix round 2: the original version here (`tokens.windows(2)`) only ever
/// looked at the token immediately after `Seed(`, so
/// `Seed(0x51ED ^ u64::from(octaves))` — a real, live pattern in
/// `kernel/src/noise.rs` — never surfaced `octaves` as a seed-correlated
/// identifier. This scans the raw `body` text rather than the token stream,
/// because [`tokenize`] already dropped the parens that mark the argument
/// span's boundary.
fn seed_construction_args(body: &str) -> Vec<&str> {
    let mut args = Vec::new();
    let bytes = body.as_bytes();
    let mut cursor = 0;
    while let Some(rel) = body[cursor..].find("Seed(") {
        let start = cursor + rel + "Seed(".len();
        let mut depth: i32 = 1;
        let mut i = start;
        while i < bytes.len() && depth > 0 {
            match bytes[i] {
                b'(' => depth += 1,
                b')' => depth -= 1,
                _ => {}
            }
            i += 1;
        }
        // `i` sits just past the matching `)` (ASCII, so `i - 1` is always a
        // valid UTF-8 boundary) when balanced, or at the end of `body` when
        // not — either way a safe slice bound.
        let end = if depth == 0 { i - 1 } else { bytes.len() };
        args.extend(tokenize(&body[start..end]));
        cursor = start;
    }
    args
}

/// Does `tokens` contain a `for` loop whose pattern binds an identifier that
/// is later passed to `Seed(...)` — catching `for i in 0..N { … Seed(i) … }`,
/// where the loop binding is not seed-shaped by name but is demonstrably
/// USED as a seed downstream. Decision 0093's literal shape (a `for` loop
/// feeding a world build), just spelled with a loop variable this scan's
/// naming heuristic alone would miss (Fix round 1, Critical, Class 2; the
/// argument-span widening is Fix round 2).
///
/// Correlation is scoped to "anywhere in this function's tokens" rather than
/// to the loop's own braces (this scan does not track block boundaries) —
/// additive-only risk: at worst it flags a test whose `for` loop and
/// `Seed(...)` call happen to share a variable name but are otherwise
/// unrelated, which was not observed anywhere in this tree when this was
/// added.
fn has_seed_via_construction(tokens: &[&str], body: &str) -> bool {
    let seed_args = seed_construction_args(body);
    if seed_args.is_empty() {
        return false;
    }
    let mut k = 0;
    while k < tokens.len() {
        if tokens[k] == "for"
            && let Some(idents) = for_binding_idents(tokens, k)
            && idents.iter().any(|ident| seed_args.contains(ident))
        {
            return true;
        }
        k += 1;
    }
    false
}

/// Is a function body seed-looping? A `for` loop over a seed-shaped binding
/// (including a tuple pattern that binds one), a `for` loop whose binding is
/// later passed to `Seed(...)`, a closure with a seed-shaped parameter, a
/// `SEEDS`-like constant, or a call through `map_seeds` (module doc).
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
    if has_seed_via_construction(&tokens, body) {
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

/// Brace-counting state that must persist ACROSS lines: a `"..."` string
/// literal can continue onto the next physical line (a trailing `\` line
/// continuation, as `windows/lab/src/timings.rs`'s test fixtures use for
/// multi-line JSON literals), and a `/* ... */` block comment can span many
/// lines outright — so `in_string`/`escaped`/`in_block_comment` are fields
/// here rather than locals re-initialized per line. Re-initializing per line
/// was a real bug once (resetting to "not in a string" at the start of every
/// line silently missed a continuation and miscounted the NEXT line's
/// braces); the same class of bug would recur for block comments if they
/// were not tracked the same way.
#[derive(Default)]
struct BraceState {
    /// Whether the scan is currently inside a `"..."` string literal.
    in_string: bool,
    /// Whether the previous character was an unconsumed `\` escape inside
    /// a string literal.
    escaped: bool,
    /// Whether the scan is currently inside a `/* ... */` block comment.
    /// **Does not handle nesting, and nesting is a real silent-miss risk,
    /// not a theoretical one:** Rust's block comments CAN nest, and a
    /// nested one — `/* a /* b */ } */` — closes `in_block_comment` at the
    /// FIRST `*/` (the inner one), so the `}` between the inner and outer
    /// closer is read as live code and counted, miscounting depth.
    ///
    /// **What that miscount actually does — verified directly, not
    /// asserted: it silently drops the rest of THAT function's own body
    /// from the scan (or, symmetrically, absorbs trailing lines into it);
    /// it does NOT hide a following function.** A synthetic
    /// `fn a() { /* outer /* inner */ } */ }` immediately followed by a
    /// real seed-looping `fn b()` still reports `b` — confirmed by running
    /// [`seed_looping_tests_in`] against exactly that text and reading its
    /// output — because `fn_name` is checked on every line independently,
    /// with no dependency on where a previous function's body was judged to
    /// end. See `count_braces`'s doc for the general form of this
    /// correction (an earlier version of THIS doc made the same
    /// "hides a following function" overstatement the raw-string doc did).
    /// No live nested block comment was found in a `#[test]` body in this
    /// tree at this commit (`grep -rn '/\*.*/\*'`), but that is, again, a
    /// fact about this commit's sources, not a structural guarantee.
    in_block_comment: bool,
}

/// Count `{`/`}` on one line toward `depth`/`started`, skipping characters
/// inside a `//` line comment, a `/* ... */` block comment, a `'x'`/`'\n'`
/// char literal, or a `"..."` string literal (escape-aware, and all four
/// carried across lines via `state`) so a brace spelled out in any of them —
/// `.split("... mod tests {\n")`, which this very file's `the_readout_law`
/// test once tripped this exact way — cannot desynchronize the depth count.
///
/// **Known gap, and it is NOT inert today.** A raw string (`r"..."`/
/// `r#"..."#`) is not recognized as a string boundary — an embedded `"`
/// inside one (legal there, e.g. `r#"contains "quotes" fine"#`) is read as
/// an ordinary closing quote, potentially miscounting whatever brace text
/// follows on the same conceptual literal. This is a SILENT MISS, not a
/// visibly wrong body length.
///
/// **What actually gets corrupted when this fires — precisely, because
/// "hides a function" is too strong a claim and was wrong once already
/// (see `BraceState.in_block_comment`'s doc): `function_body` is called
/// fresh, with a fresh `BraceState`, once per detected `#[test]` — a
/// desync inside ONE call can only ever mis-extract THAT call's own
/// reported body. It CANNOT hide a later function's own detection, because
/// [`seed_looping_tests_in`]'s outer loop calls [`fn_name`] on every line
/// independently, with no dependency on where any earlier function's body
/// was judged to end.** Concretely, a desync either (a) truncates — depth
/// falsely reaches `<= 0` before the function's true end, silently
/// dropping the rest of THAT function's own body from the scan (a possible
/// false NEGATIVE on it, if the missing tail held a seed signal), or (b)
/// over-extends — depth never resettles at the true end and absorbs
/// trailing lines (possibly containing later `#[test]` fns' own source)
/// into THIS function's reported body (a possible false POSITIVE on it, if
/// the absorbed text holds a seed signal — exactly what happened to
/// `the_readout_law` before this file's own brace counter was fixed).
/// Either way, every function keeps getting found and scanned; what an
/// individual desync can corrupt is only ever the CURRENT function's own
/// classification.
///
/// The precise mechanism for a raw string specifically: each embedded `"`
/// wrongly toggles `in_string`, so text strictly BETWEEN an odd-indexed and
/// an even-indexed embedded quote is (wrongly) read as ordinary code, and a
/// brace there IS counted when it should not be — while a brace outside any
/// such window still gets (rightly, by the accidental combination of two
/// wrongs) skipped, because the surrounding text is still inside the
/// wrongly-toggled "string." An EVEN total of embedded quotes returns
/// `in_string` to `true` by the time the real closing delimiter is reached,
/// so THIS function's own extraction resettles correctly there (case (a)/(b)
/// above do not occur for it) even though a brace inside one of the
/// internal windows was still miscounted along the way; an ODD total does
/// not resettle, which is case (b) — over-extension past this function's
/// true end.
///
/// `tools/type-audit/src/stream_label.rs`'s
/// `raw_scan_sees_a_literal_inside_cfg_test_but_the_outside_tests_scan_does_not`
/// and
/// `a_literal_in_production_code_is_still_flagged_alongside_an_exempt_test_module`
/// were checked directly (both carry a `r#"..."#` fixture holding `"` and
/// `{`/`}`) by instrumenting this exact function against the real file and
/// printing the extracted body: **both terminate at their true end,
/// unaffected.** Fixture 1 carries ONE embedded quote pair
/// (`"astronomy"`); fixture 2 carries TWO (`"production/literal"` and
/// `"astronomy"`) — both counts are even, and neither fixture has a brace
/// inside either pair's bracketed text, so the internal miscounting window
/// this mechanism opens happens to contain nothing that would be
/// miscounted, in both fixtures. A workspace-wide scan for a raw string
/// with an ODD embedded-quote count AND at least one brace inside it (case
/// (b), over-extension past ITS OWN end) found none at this commit — 149
/// `r#"..."#` raw strings in the tree (re-counted for this fix round; no
/// `r"..."`/multi-`#` form exists here), 0 matching that shape. That is a
/// fact about this commit's raw strings, not a proof the mechanism above
/// is unreachable — an odd count only needs one write to exist, and this
/// scan does not run itself as a pre-commit check.
fn count_braces(line: &str, depth: &mut i32, started: &mut bool, state: &mut BraceState) {
    let chars: Vec<char> = line.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];
        if state.in_block_comment {
            if ch == '*' && chars.get(i + 1) == Some(&'/') {
                state.in_block_comment = false;
                i += 2;
            } else {
                i += 1;
            }
            continue;
        }
        if state.in_string {
            if state.escaped {
                state.escaped = false;
            } else if ch == '\\' {
                state.escaped = true;
            } else if ch == '"' {
                state.in_string = false;
            }
            i += 1;
            continue;
        }
        if ch == '"' {
            state.in_string = true;
            i += 1;
            continue;
        }
        if ch == '/' && chars.get(i + 1) == Some(&'/') {
            break; // rest of the line is a line comment
        }
        if ch == '/' && chars.get(i + 1) == Some(&'*') {
            state.in_block_comment = true;
            i += 2;
            continue;
        }
        if ch == '\'' {
            // A char literal closes with another `'` within at most 2
            // characters (one plain char, or a `\` escape pair); a lifetime
            // (`'a`, `'static`) never closes at all. Skip the whole literal
            // so an escaped brace (`'{'`, `'}'`) or quote (`'\''`) inside it
            // is never inspected below; a bare lifetime marker falls through
            // to the `i += 1` at the bottom, which has no brace effect.
            if chars.get(i + 2) == Some(&'\'') {
                i += 3;
                continue;
            }
            if chars.get(i + 1) == Some(&'\\') && chars.get(i + 3) == Some(&'\'') {
                i += 4;
                continue;
            }
        }
        match ch {
            '{' => {
                *depth += 1;
                *started = true;
            }
            '}' => *depth -= 1,
            _ => {}
        }
        i += 1;
    }
}

/// The text of the function opening at `lines[start]`, from its first `{` to
/// the matching `}` (brace-depth counting via [`count_braces`], which skips
/// string and line-comment content and carries string state across lines).
fn function_body(lines: &[&str], start: usize) -> String {
    let mut depth: i32 = 0;
    let mut started = false;
    let mut state = BraceState::default();
    let mut body = String::new();
    for line in &lines[start..] {
        body.push_str(line);
        body.push('\n');
        count_braces(line, &mut depth, &mut started, &mut state);
        if started && depth <= 0 {
            break;
        }
    }
    body
}

/// For every `#[test]` function in `text`, whether its body is seed-looping
/// and (if so) whether it already carries a sanctioned `claim:` tag. Shared
/// by [`untagged_in`] (which reports only the untagged half) and the
/// non-vacuity ratchet test below (which needs the WHOLE detected
/// population, tagged or not — see that test's doc for why the two differ).
fn seed_looping_tests_in(text: &str) -> Vec<(&str, bool)> {
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
        let body = function_body(&lines, i);
        if is_seed_looping(&body) {
            out.push((name, tagged));
        }
    }
    out
}

/// Scan one source file's text. A test is seed-looping if its body contains a
/// `for` over a numeric range, a `SEEDS`-like constant, or `map_seeds`.
fn untagged_in(path: &str, text: &str) -> Vec<Untagged> {
    seed_looping_tests_in(text)
        .into_iter()
        .filter(|(_, tagged)| !tagged)
        .map(|(name, _)| Untagged {
            file: path.to_string(),
            test: name.to_string(),
        })
        .collect()
}

/// The workspace root: the parent of this crate's manifest dir (`cli/`).
/// Filesystem-based, not git-based — the remote gate runs the suite in an
/// rsync'd tree that is not a git repository. Verbatim from
/// `cli/tests/heavy_tier.rs`'s `repo_root` (Fix round 1, Important: the
/// brief's own skeleton wrote `Path::new("..")`, a CWD-dependent regression
/// against exactly the precedent this mirrors).
fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("cli/ has a parent")
        .to_path_buf()
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

/// claim: structural(scanner self-test) — this test's body carries the
/// fixture string `"for seed in 0..8u64 { ... }"` as literal text, not code.
/// `tokenize`/`is_seed_looping` (unlike `count_braces`, which IS
/// string-aware for brace-matching purposes) work over the WHOLE extracted
/// body as plain text and do not know a token came from inside a string
/// literal rather than real code. That makes this test self-referentially
/// seed-looping by the scanner's own signal, though it builds zero worlds
/// and samples nothing — it drives a pure function over three fixed fixture
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
    collect_rs(&repo_root(), &mut sources);
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

/// The floor below which the repo-wide DETECTED seed-looping population
/// (tagged or not — see [`seed_looping_tests_in`]) must never drop without a
/// deliberate, reviewed reason. Set to 284 at Fix round 2, after the
/// tuple-pattern, `Seed(...)`-correlation, and `Seed(...)`-argument-span
/// detection fixes (`has_seed_for_loop`/`has_seed_via_construction`/
/// `seed_construction_args`); re-recorded to 286 at Fix round 3 after
/// absorbing `main` mid-task added two new seed-looping tests
/// (`windows/vessel/src/purview.rs`), an ordinary instance of the "campaign
/// work adds to this continuously" growth this constant's own doc already
/// names below — re-recording it is the routine case, not an exception.
/// Lower this number ONLY in the same commit that deliberately removes or
/// merges a real seed-looping test — never as an unexamined side effect of
/// a detection regression elsewhere in this file.
///
/// **This is a floor, not the frozen-corpus exact-equality this tree uses
/// elsewhere (a tropes corpus's situation count, a census's row count) —
/// deliberately**, because the population this counts is not a frozen
/// corpus: ordinary campaign work adds new seed-looping tests continuously,
/// and an exact-equality check would demand a floor bump on every such PR
/// for a reason unrelated to THIS lint. `>=` absorbs additions for free and
/// still catches the one failure mode this exists for — a silent drop. The
/// cost is real, not merely theoretical: the floor's SLACK (the gap between
/// this number and the live total) grows every time a seed-looping test is
/// added without a matching floor bump, and a large slack is exactly the
/// blind spot `heavy_tier.rs`'s own token-guard retrospective warns about —
/// a guard that is not wrong yet is not the same as a guard that is still
/// checking anything. Re-record this number whenever it is convenient to
/// (there is no requirement to do it on every PR), and treat a slack that
/// has grown by dozens as a sign the ratchet has gone quiet rather than as
/// evidence nothing moved.
const DETECTED_SEED_LOOPING_FLOOR: usize = 286;

/// The non-vacuity ratchet `cli/tests/heavy_tier.rs:117-120`'s
/// `!heavy.is_empty()` models, for this scan. `every_seed_looping_test_in_
/// the_repo_declares_its_claim_shape` above proves the UNTAGGED population
/// is empty — but that is exactly as true, and exactly as worthless, if
/// detection itself silently stopped seeing anything: an empty untagged
/// list is what "no test in the tree loops a seed" and "the scanner is
/// dead" both look like from the outside. This test instead asserts the
/// WHOLE detected population (tagged or not) is at least the recorded
/// floor, so a scanner regression that quietly stops matching real seed
/// loops shows up as a falling count rather than as continued, meaningless
/// green.
#[test]
fn the_detected_seed_looping_count_has_not_silently_collapsed() {
    let mut sources = Vec::new();
    collect_rs(&repo_root(), &mut sources);
    let mut total = 0usize;
    for path in &sources {
        let text = std::fs::read_to_string(path).expect("source is readable");
        total += seed_looping_tests_in(&text).len();
    }
    assert!(
        total >= DETECTED_SEED_LOOPING_FLOOR,
        "detected only {total} seed-looping tests, below the recorded floor of \
         {DETECTED_SEED_LOOPING_FLOOR}. Either a real seed loop was deliberately \
         removed or merged (lower the floor in that same commit, with a reason) or \
         detection itself regressed (fix the scanner — do not lower the floor to \
         paper over it)"
    );
}
